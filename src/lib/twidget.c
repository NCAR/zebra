/*
 * Time widget code.
 */

/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include <sys/types.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>

# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/SmeLine.h>

# include <ui.h>
# include <ui_date.h>
# include "defs.h"
# include "message.h"
# include "timer.h"
# include "bitmaps.h"
# include "twidget.h"

MAKE_RCSID ("$Id: twidget.c,v 2.14 1995-04-15 00:32:08 granger Exp $")


# define LABELWIDTH	60

# define TW_NAME	"time"

/*
 * Date/time button action codes.
 */
# define MONTHUP	1
# define MONTHDOWN	2
# define DAYUP		3
# define DAYDOWN	4
# define YEARUP		5
# define YEARDOWN	6
# define HOURUP		7
# define HOURDOWN	8
# define MINUP		9
# define MINDOWN	10
# define SECUP		11
# define SECDOWN	12

static HotTime HTimes[MAX_HOT_TIMES];
static int NTimes = 0;

static void (*HT_AddCallback)(/* ZebTime *zt, char *label */) = NULL;
static void (*HT_DeleteCallback)(/* ZebTime *zt */) = NULL;

#define HT_MenuName "hot-times-menu"
static Widget HT_Menu = NULL;
static Widget HT_Text = NULL;

/*
 * Global stuff
 */
static Widget Htext = NULL;
static Widget Stext = NULL, ControlButton = NULL;
static Widget RTToggle = NULL, HistoryToggle = NULL;

/*
 * Histdate is the history date that appears in the window.  Maxdate is
 * the highest date we've ever seen, used to keep the date from being
 * moved into the future.  RTMode tells us what mode we have most
 * recently been put in.
 */
static ZebTime Histdate;
static int RTMode = TRUE;
static char Ahistdate[80];
static char Title[200];
static int Tslot = -1;	/* Timeout slot		*/
static int SkipMin = 5;
static char TSString[10];
static int ControlAll = TRUE;

static int (*Tw_Callback) (/* enum pmode, ZebTime *, int control_all */) = 0;
static void (*Tw_HelpCallback) (/* void */) = 0;
static void (*Tw_PopupCallback) (/* void */) = 0;

static Widget tw_WCreate ();
static int tw_SyncTime FP((void));
static void tw_WCallback ();
static void enter_time FP ((Widget w, XEvent *, String *, Cardinal *));
static void finish_arrow (), datebutton ();
static void ChangeMonth (), ChangeDay (), ChangeYear (); 
static void ChangeHour (), ChangeMin ();
static void TimeSkip ();
static void ChangeControl ();
static void CallForHelp ();
static void QuitCallback ();
static void do_adjust FP ((int which));
static void set_dt FP ((void));
extern Widget LeftRightButtons ();

static void tw_HTAddCurrent FP ((Widget w, XtPointer client, XtPointer call));
static void tw_HTDeleteCurrent FP ((Widget w, XtPointer client, XtPointer ));
static void tw_HTEntryCallback FP ((Widget w, XtPointer client, XtPointer ));
static void tw_HTSyncEntries FP ((Widget menu, int entry));
static void tw_HTLabel FP ((char *label));
static Widget tw_HTCreateMenu FP ((Widget parent, char *menuname));



void
tw_AddHelpCallback (callback)
void (*callback) ();
{
	Tw_HelpCallback = callback;
}


void
tw_AddPopupCallback (callback)
void (*callback) ();
{
	Tw_PopupCallback = callback;
}


void
tw_DefTimeWidget (callback, title)
int	(*callback) ();
char	*title;
/*
 * Hook the time widget into the UI.
 */
{
	int year, month, day, hour, minute, second;
/*
 * Get the current time, then clear out the seconds portion of it.
 */
	tl_Time (&Histdate);
	RTMode = TRUE;
	/* Histdate.ds_hhmmss -= Histdate.ds_hhmmss % 100; */
	TC_ZtSplit (&Histdate, &year, &month, &day, &hour, &minute, &second,0);
	TC_ZtAssemble (&Histdate, year, month, day, hour, minute, 0, 0);

	strcpy (Title, title);
	Tw_Callback = callback;
	uw_def_widget (TW_NAME, "Time control", tw_WCreate, 0, 0);
	uw_NoHeader (TW_NAME);
}



void
tw_SetTime (zt)
ZebTime *zt;
/*
 * For setting the time from the outside.  If zt == NULL, the
 * system time is used (as is the default at definition).
 */
{
	int year, month, day, hour, minute, second;
	Arg true, false;

	XtSetArg (true, XtNstate, True);
	XtSetArg (false, XtNstate, False);

	if (zt)
	{
		Histdate = *zt;
		RTMode = FALSE;
	}
	else
	{
		tl_Time (&Histdate);
		RTMode = TRUE;
	}
	TC_ZtSplit (&Histdate, &year, &month, &day, &hour, &minute, &second,0);
	TC_ZtAssemble (&Histdate, year, month, day, hour, minute, 0, 0);
/*
 * If no zt we've gone into real time mode, else we've gone to history.
 * Reflect the change in the mode toggles and the time text, after checking
 * that the toggle widgets exist.
 */
	if (zt && RTToggle && HistoryToggle)
	{
		XtSetValues (RTToggle, &false, (Cardinal)1);
		XtSetValues (HistoryToggle, &true, (Cardinal)1);
		tw_HTLabel ("History Mode");
	}
	else if (RTToggle && HistoryToggle)
	{
		XtSetValues (HistoryToggle, &false, (Cardinal)1);
		XtSetValues (RTToggle, &true, (Cardinal)1);
		tw_HTLabel ("Real Time Mode");
	}
	set_dt ();
	uw_sync ();
}



static Widget
tw_WCreate (junk, parent, appc)
int	junk;
Widget	parent;
XtAppContext appc;
{
	Arg args[20];
	Widget form, title, left, cform, sform;
	Widget dateform;
	Widget button_bar;
	Widget mbutton, dbutton, ybutton, hbutton, minbutton;
	Widget skipbutton, helpbutton, quitbutton;
	Widget menubutton;
	Widget shell;
	int n;
	static char *ttrans = "<Btn1Down>,<Btn1Up>: 	set()notify()";
	static XtActionsRec actions[] = {
			{"finishadj", finish_arrow},
			{"enter-time", enter_time}
	};
	static char *atrans = "<Btn1Down>:	set()notify() \n\
	           	       <Btn1Up>: 	finishadj()unset()";
	XtTranslations ttable, atable;
/*
 * Translations.
 */
	ttable = XtParseTranslationTable (ttrans);
	XtAppAddActions (appc, actions, XtNumber(actions));
	atable = XtParseTranslationTable (atrans);
/*
 * Make the bitmaps for the left and right arrow buttons and the menu icon.
 */
	bm_BuildBitmaps (parent);
/*
 * Find our ancestor popup shell and set our window gravity to static
 */
	shell = parent;
	while (shell && !XtIsSubclass (shell, wmShellWidgetClass))
		shell = XtParent(shell);
	n = 0;
	XtSetArg (args[n], XtNwinGravity, StaticGravity); n++;
	XtSetValues (shell, args, n);
/*
 * Put a form inside it.
 */
	XtSetArg (args[0], XtNdefaultDistance, 5);
	form = XtCreateManagedWidget ("form", formWidgetClass, parent,args, 1);
/*
 * ----------------------------------------------------------------
 * Put the title label in the top left corner of the form.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, Title);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	title = XtCreateManagedWidget ("title", labelWidgetClass, form,
				       args, n);
/*
 * Put a text window for the current hot label next to it
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, title);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 180);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	XtSetArg (args[n], XtNlength, 80);		n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, False);	n++;
	XtSetArg (args[n], XtNstring, "");		n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	HT_Text = XtCreateManagedWidget ("hotlabel", asciiTextWidgetClass,
					 form, args, n);
/*
 * --------------------------------------------------------------------
 * On the next row put the most-used buttons in a button bar:
 * real-time mode, history mode, history times menu, help, and dismiss.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, title);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	button_bar = XtCreateManagedWidget ("buttonbar", formWidgetClass, 
					    form, args, n);
/*
 * Real time mode button is the leftmost
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Real Time");	++n;
	XtSetArg (args[n], XtNfromHoriz, NULL);		++n;
	XtSetArg (args[n], XtNfromVert, NULL);		++n;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	XtSetArg (args[n], XtNstate, (RTMode)?(True):(False));	++n;
	RTToggle = XtCreateManagedWidget ("rt", toggleWidgetClass, 
					  button_bar, args, n);
	left = RTToggle;
	XtOverrideTranslations (left, ttable);
	XtAddCallback (left, XtNcallback, tw_WCallback, (XtPointer) RealTime);
/*
 * Set history time button
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Set Time");	++n;
	XtSetArg (args[n], XtNfromHoriz, left);		++n;
	XtSetArg (args[n], XtNfromVert, NULL);		++n;
	XtSetArg (args[n], XtNradioGroup, left);	++n;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	XtSetArg (args[n], XtNstate, (RTMode)?(False):(True));	++n;
	HistoryToggle = XtCreateManagedWidget ("history", toggleWidgetClass, 
					       button_bar, args, n);
	left = HistoryToggle;
	XtOverrideTranslations (left, ttable);
	XtAddCallback (left, XtNcallback, tw_WCallback, (XtPointer) History);
/*
 * Menu button for the history times menu
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Deja Vu");	++n;
	XtSetArg (args[n], XtNfromHoriz, left);		++n;
	XtSetArg (args[n], XtNfromVert, NULL);		++n;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	XtSetArg (args[n], XtNmenuName, HT_MenuName);	++n;
	XtSetArg (args[n], XtNleftBitmap, MenuIcon);	++n;
	menubutton = XtCreateManagedWidget ("menubutton", 
					    menuButtonWidgetClass,  
					    button_bar, args, n);
	HT_Menu = tw_HTCreateMenu (menubutton, HT_MenuName);
/*
 * Add a history time
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Remember");	n++;
	XtSetArg (args[n], XtNfromHoriz, menubutton);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	left = XtCreateManagedWidget ("remember", commandWidgetClass, 
				      button_bar, args,n);
	XtAddCallback (left, XtNcallback, (XtCallbackProc) tw_HTAddCurrent, 
		       (XtPointer) 0);
/*
 * Delete a history time
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Forget");		n++;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	left = XtCreateManagedWidget ("forget", commandWidgetClass, 
				      button_bar, args,n);
	XtAddCallback (left, XtNcallback, (XtCallbackProc) tw_HTDeleteCurrent,
		       (XtPointer) 0);
/*
 * Help button
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Help");		n++;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	helpbutton = XtCreateManagedWidget ("helpbutton", commandWidgetClass, 
					    button_bar, args,n);
	XtAddCallback (helpbutton, XtNcallback, CallForHelp, NULL);
/*
 * Quit button
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Dismiss");	n++;
	XtSetArg (args[n], XtNfromHoriz, helpbutton);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	quitbutton = XtCreateManagedWidget ("quitbutton", 
					    commandWidgetClass, 
					    button_bar, args,n);
	XtAddCallback (quitbutton, XtNcallback, QuitCallback, NULL);
/*
 * End of button bar
 * ================================================================
 * Create a form for the history date below the button bar.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, button_bar);	n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	dateform = XtCreateManagedWidget ("dateform", formWidgetClass, form, 
					  args, n);
/*
 * Left/right buttons for month, day, year.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Day" );		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, LABELWIDTH);	n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("dlabel", labelWidgetClass, dateform,
				      args, n);

	dbutton = LeftRightButtons (dateform, ChangeDay, atable);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetValues (dbutton, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Month");		n++;
	XtSetArg (args[n], XtNfromHoriz, dbutton);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, LABELWIDTH);	n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("mlabel", labelWidgetClass, dateform,
				      args, n);

	mbutton = LeftRightButtons (dateform, ChangeMonth, atable);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetValues (mbutton, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Year");		n++;
	XtSetArg (args[n], XtNfromHoriz, mbutton);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, LABELWIDTH);	n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("ylabel", labelWidgetClass, dateform,
		args, n);

	ybutton = LeftRightButtons (dateform, ChangeYear, atable);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetValues (ybutton, args, n);
/*
 * Left/right buttons for hour, minute.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Hours");		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, dbutton);	n++;
	XtSetArg (args[n], XtNwidth, LABELWIDTH);	n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("hlabel", labelWidgetClass, dateform,
		args, n);

	hbutton = LeftRightButtons (dateform, ChangeHour, atable);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, dbutton);	n++;
	XtSetValues (hbutton, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Minutes" );	n++;
	XtSetArg (args[n], XtNfromHoriz, hbutton);	n++;
	XtSetArg (args[n], XtNfromVert, dbutton);	n++;
	XtSetArg (args[n], XtNwidth, LABELWIDTH);	n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("minlabel", labelWidgetClass, dateform,
		args, n);

	minbutton = LeftRightButtons (dateform, ChangeMin, atable);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, dbutton);	n++;
	XtSetValues (minbutton, args, n);
/*
 * Text widget for the history date.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, minbutton);	n++;
	XtSetArg (args[n], XtNfromVert, dbutton);	n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 140);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	XtSetArg (args[n], XtNlength, 80);		n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);	n++;
	XtSetArg (args[n], XtNstring, "history date");	n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	Htext = XtCreateManagedWidget ("entertime", asciiTextWidgetClass,
				       dateform, args, n);
/*
 * ----------------------------------------------------------------
 * Control one/all windows selection in lower left corner.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, dateform);	n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	cform = XtCreateManagedWidget ("cform", formWidgetClass, 
				       form, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Windows:");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("control", labelWidgetClass, 
		cform, args,n);

	n = 0;
	XtSetArg (args[n], XtNlabel, ControlAll ? "All" : "One");	n++;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, 30);		n++;
	ControlButton = XtCreateManagedWidget ("contbutton", 
		commandWidgetClass, cform, args,n);
	XtAddCallback (ControlButton, XtNcallback, ChangeControl, NULL);
/*
 * ----------------------------------------------------------------
 * Time skipping stuff in lower right corner.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, cform);	n++;
	XtSetArg (args[n], XtNfromVert, dateform);	n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	sform = XtCreateManagedWidget ("sform", formWidgetClass, form, 
				       args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Skip");		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("skip", labelWidgetClass, sform, args,n);

	sprintf (TSString, "%d", SkipMin);
	
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 35);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	XtSetArg (args[n], XtNlength, 80);		n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);	n++;
	XtSetArg (args[n], XtNstring, TSString);	n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	Stext = XtCreateManagedWidget ("stext", asciiTextWidgetClass,
				       sform, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "min.");		n++;
	XtSetArg (args[n], XtNfromHoriz, Stext);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("minutes", labelWidgetClass, sform, 
				      args, n);

	skipbutton = LeftRightButtons (sform, TimeSkip, NULL);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetValues (skipbutton, args, n);
/*
 * See what we get.
 */
	set_dt ();
	if (Tw_PopupCallback)
		(*Tw_PopupCallback)();
	return (form);
}



static void
CallForHelp ()
/*
 * Calls the help callback if it exists.
 */
{
	if (Tw_HelpCallback)
		(*Tw_HelpCallback) ();
}



static void
QuitCallback ()
/*
 * Popdown the time widget
 */
{
	uw_popdown (TW_NAME);
}



static void
ChangeControl ()
/*
 * Change window control between one window and all windows.
 */
{

	Arg arg;

	ControlAll = ! ControlAll;
	XtSetArg (arg, XtNlabel, ControlAll ? "All" : "One");
	XtSetValues (ControlButton, &arg, 1);
}


static void
TimeSkip (w, change, junk)
Widget w;
XtPointer change, junk;
{
	long systime;

	SkipMin = atoi (TSString);
	systime = TC_ZtToSys (&Histdate);
	TC_SysToZt (((int) change == 1) ? systime + SkipMin*60 :
			systime - SkipMin*60, &Histdate);
	set_dt ();
	(*Tw_Callback) (History, &Histdate, ControlAll);
}


static void
ChangeMonth (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((int) change == 1)
		datebutton (MONTHUP);
	else datebutton (MONTHDOWN);
}


static void
ChangeDay (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((int) change == 1)
		datebutton (DAYUP);
	else datebutton (DAYDOWN);
}


static void
ChangeYear (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((int) change == 1)
		datebutton (YEARUP);
	else datebutton (YEARDOWN);
}


static void
ChangeHour (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((int) change == 1)
		datebutton (HOURUP);
	else datebutton (HOURDOWN);
}


static void
ChangeMin (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((int) change == 1)
		datebutton (MINUP);
	else datebutton (MINDOWN);
}



static int
tw_SyncTime ()
{
	ZebTime past;

	/*
	 * Parse the time in the text window to get the current time
	 */
	if (! TC_DecodeTime (Ahistdate, &past))
	{
		tw_HTLabel ("Time not recognized; try again");
		XBell (XtDisplay(Htext), 0);
		return (False);
	}
	Histdate = past;
	return (True);
}



static void
tw_WCallback (w, cdata, calldata)
Widget w;
XtPointer cdata;
XtPointer calldata;
/*
 * The toggle callback.  Only point of entry for changing to real-time,
 * and only point of entry for changing to the time typed in the text
 * window.
 */
{
	enum pmode mode = (enum pmode) cdata;
	int activate = (int) calldata;

	if (! activate)
		return;
	if (mode == RealTime)
		tw_HTLabel ("Real Time Mode");
	else if (! tw_SyncTime())
		return;
	else
		tw_HTLabel ("History Mode");
	if (Tw_Callback)
		(*Tw_Callback) (mode, &Histdate, ControlAll);
}



static void
datebutton (code)
int code;
{
	void arrow_timeout ();
	bool norep = FALSE;
/*
 * Cancel if necessary.
 */
	if (Tslot >= 0)
	{
		finish_arrow ();
		norep = TRUE;
	}
/*
 * Do one adjustment immediately.
 */
	do_adjust (code);
/*
 * Add the timeout to start repeating.
 */
	if (! norep)
		Tslot = tl_RelativeReq (arrow_timeout, (char *) code, 5, 1);
	uw_sync ();
}




void
arrow_timeout (t, code)
ZebTime *t;
int code;
/*
 * Deal with repeating arrows.
 */
{
	do_adjust (code);
	uw_sync ();
}



/*ARGSUSED*/
static void
enter_time (w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
/*
 * Go to the history time contained in the time text window
 */
{
	tw_WCallback (w, (XtPointer) History, (XtPointer) True);
}



static void
finish_arrow ()
/*
 * Deal with the end of an arrow situation.
 */
{
	if (Tslot >= 0)
		tl_Cancel (Tslot);
	Tslot = -1;
}



static void
do_adjust (which)
int which;
/*
 * Perform a clock adjustment.
 */
{
	/* date incr, temp; */
	int y, m, d, hour, min, sec;
	long systime = TC_ZtToSys (&Histdate);;

/*
	y = Histdate.ds_yymmdd / 10000;
	m = (Histdate.ds_yymmdd % 10000) / 100;
	d = Histdate.ds_yymmdd % 100;
*/
	TC_ZtSplit (&Histdate, &y, &m, &d, &hour, &min, &sec, 0);
/*
 * See what they want to do.
 */
	switch (which)
	{
	   case MONTHUP:
		m += 1;
		if (m > 12)
		{
			m = 1;
			y++;
		}
		TC_ZtAssemble (&Histdate, y, m, d, hour, min, sec, 0);
		break;

	   case MONTHDOWN:
		m -= 1;
		if (m < 0) 
		{
			m = 12;
			y--;
		}
		TC_ZtAssemble (&Histdate, y, m, d, hour, min, sec, 0);
		break;

	   case DAYUP:
		TC_SysToZt (systime + 24*60*60, &Histdate);
		break;
	   case DAYDOWN:
		TC_SysToZt (systime - 24*60*60, &Histdate);
		break;

	   case YEARUP:
		y += 1;
		TC_ZtAssemble (&Histdate, y, m, d, hour, min, sec, 0);
		break;
	   case YEARDOWN:
		y -= 1;
		TC_ZtAssemble (&Histdate, y, m, d, hour, min, sec, 0);
		break;

	   case HOURUP:
		TC_SysToZt (systime + 60*60, &Histdate);
		break;
	   case HOURDOWN:
		TC_SysToZt (systime - 60*60, &Histdate);
		break;

	   case MINUP:
		TC_SysToZt (systime + 60, &Histdate);
		break;
	   case MINDOWN:
		TC_SysToZt (systime - 60, &Histdate);
		break;

	   case SECUP:
		TC_SysToZt (systime + 1, &Histdate);
		break;
	   case SECDOWN:
		TC_SysToZt (systime - 1, &Histdate);
		break;
	}
	set_dt ();
}



static void
set_dt ()
/*
 * Set the value of the date and time widgets.
 */
{
	Arg args[2];
/*
 * Make sure we have widgets first
 */
	if (! Htext)
		return;
/*
 * Format, then split, the date.
 */
	TC_EncodeTime (&Histdate, TC_Full, Ahistdate);
/*
 * Text too.
 */
	XtSetArg (args[0], XtNstring, Ahistdate);
	XtSetValues (Htext, args, 1);
}



/* --------------------------------------------------------------------
 * Hot times support
 */
void
tw_AddHTAddCallback (func)
void (*func)();
{
	HT_AddCallback = func;
}


void
tw_AddHTDeleteCallback (func)
void (*func)();
{
	HT_DeleteCallback = func;
}


void
tw_AddHotTime (zt, label)
ZebTime *zt;
char *label;
/*
 * Add this hot time to the list.  The label is truncated if necessary
 * to fit into the hot time structure.  Keep the list sorted in
 * reverse chronological order (most recent times first).
 * If the time already exists, then only the label is changed.
 */
{
	int i;
	int insert;

	for (i = 0; i < NTimes; ++i)
		if (TC_LessEq (HTimes[i].ht_zt, *zt))
			break;
	insert = i;
	if (insert < NTimes)
	{
		if (! TC_Eq (HTimes[insert].ht_zt, *zt))
		{
			/*
			 * So we need to insert a new time at 'insert' and
			 * move the rest down.  If our array is filled, the
			 * last time gets dropped. 
			 */
			if (NTimes < MAX_HOT_TIMES)
				++NTimes;
			for (i = NTimes - 1; i > insert; --i)
			{
				HTimes[i] = HTimes[i - 1];
			}
		}
		/*
		 * If the times are equal we overwrite this spot
		 */
	}
	else if (insert >= MAX_HOT_TIMES)
		/*
		 * Overwriting the last available slot in the array
		 */
		insert = MAX_HOT_TIMES - 1;
	else
		++NTimes;	/* Adding to the end of the array */

	HTimes[insert].ht_zt = *zt;
	strncpy (HTimes[insert].ht_label, label, HT_LABEL_LEN);
	HTimes[insert].ht_label[HT_LABEL_LEN - 1] = '\0';
	/*
	 * Sync the menu labels and call the add callback
	 */
	tw_HTSyncEntries (HT_Menu, insert);
	if (HT_AddCallback)
		(*HT_AddCallback) (&HTimes[insert].ht_zt, 
				   HTimes[insert].ht_label);
}



void
tw_DeleteHotTime (zt)
ZebTime *zt;
/*
 * Remove this hot time from the list.  Just find it and move everything
 * up a slot on top of it.
 */
{
	int i;

	for (i = 0; i < NTimes; ++i)
		if (TC_Eq (HTimes[i].ht_zt, *zt))
			break;
	if (i < NTimes)
	{
		/*
		 * So we need to move everything up one.
		 */
		--NTimes;
		for ( ; i < NTimes; ++i)
		{
			HTimes[i] = HTimes[i + 1];
		}
		/*
		 * Sync the menu labels and call the delete callback
		 */
		tw_HTSyncEntries (HT_Menu, -1);
		if (HT_DeleteCallback)
			(*HT_DeleteCallback) (zt);
	}
	/*
	 * If we didn't find it we don't do anything
	 */
}



const HotTime *
tw_ListHotTimes (ntime)
int *ntime;
{
	if (ntime)
		*ntime = NTimes;
	return ((const HotTime *) HTimes);
}



/* ARGSUSED */
static void
tw_HTAddCurrent (w, client, call)
Widget w;
XtPointer client;
XtPointer call;
/*
 * Add the current time and contents of the HT_Text widget to the hot list
 */
{
	String label;
	Arg arg;

	if (! tw_SyncTime ())
		return;
	XtSetArg (arg, XtNstring, &label);
	XtGetValues (HT_Text, &arg, 1);
	tw_AddHotTime (&Histdate, label);
}



/* ARGSUSED */
static void
tw_HTDeleteCurrent (w, client, call)
Widget w;
XtPointer client;
XtPointer call;
/*
 * Delete the current time from the hot list.
 */
{
	Arg args[5];
	Cardinal narg;

	narg = 0;
	XtSetArg (args[narg], XtNstring, "");  ++narg;
	XtSetValues (HT_Text, args, narg);
	tw_DeleteHotTime (&Histdate);
}




/* ARGSUSED */
static void
tw_HTEntryCallback (w, client, call)
Widget w;
XtPointer client;
XtPointer call;
/*
 * A hot list entry callback.  Change to the history time of this entry,
 * put its label into the hot label, and display the time in the
 * time text widget.  Essentially like the plot mode toggle callback
 * tw_WCallback().
 */
{
	int which = (int) client;
	HotTime *ht = HTimes + which;

	tw_SetTime (&ht->ht_zt);
	tw_HTLabel (ht->ht_label);
	if (Tw_Callback)
		(*Tw_Callback) (History, &ht->ht_zt, ControlAll);
}



static void
tw_HTLabel (label)
char *label;
{
	Arg args[10];
	Cardinal narg;

	narg = 0;
	XtSetArg (args[narg], XtNstring, label);  ++narg;
	XtSetValues (HT_Text, args, narg);
}



static void
tw_HTSyncEntries (menu, entry)
Widget menu;
int entry;
/*
 * Sync the menu entries with the list of hot times.  If non-negative, entry
 * is the index of the hot time which has been added or changed.  It needs
 * to be unmanaged and re-managed so that the menu recalculates its
 * geometry.
 */
{
	Arg args[10];
	Cardinal narg;
	Cardinal nchildren;
	WidgetList wlist;
	char label[64 + HT_LABEL_LEN];
	int i;

	narg = 0;
	XtSetArg (args[narg], XtNnumChildren, &nchildren);  	++narg;
	XtSetArg (args[narg], XtNchildren, &wlist);		++narg;
	XtGetValues (menu, args, narg);
	/*
	 * The first five entries are the label, line, add, delete, and line.
	 * We just act as if they don't exist.
	 */
	wlist += 5;
	nchildren -= 5;
	/*
	 * For each of the times in the list, give the entry widget
	 * the correct label and make sure it is managed.
	 */
	if (entry >= 0)
		XtUnmanageChild (wlist[entry]);	/* it gets re-managed below */
	for (i = 0; (i < NTimes) && (i < nchildren); ++i)
	{
		Widget w = wlist[i];
		sprintf (label, "%-20s%s", 
			 TC_AscTime (&HTimes[i].ht_zt, TC_Full),
			 HTimes[i].ht_label);
		XtSetArg (args[0], XtNlabel, label);
		XtSetValues (w, args, (Cardinal)1);
	}
	if (i)
		XtManageChildren (wlist, (Cardinal) i);
	/*
	 * If an entry beyond the active ones is still managed, then one
	 * or more children need to be unmanaged.
	 */
	if ((i < nchildren) && XtIsManaged (wlist[i]))
		XtUnmanageChildren (wlist + i, nchildren - i);
}



static Widget
tw_HTCreateMenu (parent, name)
Widget parent;
char *name;
/*
 * Create a menu for hot times and return the widget
 */
{
	Arg args[10];
	Cardinal narg;
	Widget entry;
	Widget menu;
	int i;

	narg = 0;
	XtSetArg (args[narg], XtNlabel, "History Times");	++narg;
	menu = XtCreatePopupShell (name, simpleMenuWidgetClass,
				   parent, args, narg);
/*
 * First line object
 */
	XtCreateManagedWidget ("line", smeLineObjectClass, menu, NULL, 0);
/*
 * Add and delete commands
 */
	narg = 1;
	XtSetArg (args[0], XtNlabel, "Add Current Time");
	entry = XtCreateManagedWidget ("addentry", smeBSBObjectClass, menu,
				       args, narg);
	XtAddCallback (entry, XtNcallback, (XtCallbackProc) tw_HTAddCurrent, 
		       (XtPointer) 0);
	XtSetArg (args[0], XtNlabel, "Delete Current Time");
	entry = XtCreateManagedWidget ("delentry", smeBSBObjectClass, menu,
				       args, narg);
	XtAddCallback (entry, XtNcallback, (XtCallbackProc) 
		       tw_HTDeleteCurrent, (XtPointer) 0);
/*
 * Last line object
 */
	XtCreateManagedWidget ("line", smeLineObjectClass, menu, NULL, 0);
/*
 * Create all of the entries, but don't manage them now.
 */
	narg = 0;
	XtSetArg (args[narg], XtNlabel, "[none]");	narg++;
	for (i = 0; i < MAX_HOT_TIMES; i++)
	{
		entry = XtCreateWidget ("HTentry", smeBSBObjectClass,
					menu, args, narg);
		XtAddCallback (entry, XtNcallback, (XtCallbackProc) 
			       tw_HTEntryCallback, (XtPointer) i);
	}
	return (menu);
}
