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
# include <string.h>
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

RCSID ("$Id: twidget.c,v 2.22 2001-01-19 04:51:39 granger Exp $")


# define LABELWIDTH	65

# define TW_NAME	"time"
# define ALL_WINDOWS	"All Windows"

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
#define HT_ForgetMenuName "forget-times-menu"
static Widget HT_Menu = NULL;
static Widget HT_ForgetMenu = NULL;
static Widget HT_Text = NULL;

static Widget TW_WindowMenu = NULL;

# define MAX_VISITED	5

static Widget Htext = NULL;
static Widget Stext = NULL;
static Widget RTToggle = NULL, HistoryToggle = NULL;

/*
 * Histdate is the history date that appears in the window.  RTMode tells
 * us what mode we have most recently been put in.
 */
static ZebTime Histdate;
static int RTMode = TRUE;
static int AutoAdvance = FALSE;	/* advance time window in real-time mode */
static char Ahistdate[80];
static char Title[200];
static int Tslot = -1;	/* Timeout slot		*/

struct SkipUnit { 
	int su_scale;
	char *su_string;
	char *su_label;
} TSUnits[] =
{
	{ 0, "none", "Invalid" },
	{ 1, "secs", "Seconds" },
	{ 60, "mins", "Minutes" },
	{ 3600, "hrs", "Hours" },
	{ 3600*24, "days", "Days" }
};
static int SkipSecs = 300; /* 5 minute default */
static int TSScale = 60; /* Minutes by default */
static char TSString[10];

static char *ControlWindow = NULL;

static int (*Tw_Callback) (/* enum pmode, ZebTime *, 
			      int control_all, char *control_window */) = 0;
static void (*Tw_HelpCallback) (/* void */) = 0;
static void (*Tw_PopupCallback) (/* void */) = 0;

/*
 * Private prototypes
 */
static Widget tw_WCreate ();
static int tw_SyncTime FP ((void));
static void tw_NewTime FP ((ZebTime *zt, char *label));
static void tw_ChangeTime FP ((ZebTime *zt, char *label));
static void tw_WCallback ();
static void enter_time FP ((Widget w, XEvent *, String *, Cardinal *));
static void finish_arrow (), datebutton ();
static void ChangeMonth (), ChangeDay (), ChangeYear (); 
static void ChangeHour (), ChangeMin ();
static void TimeSkip ();
static void CallForHelp ();
static void QuitCallback ();
static void do_adjust FP ((int which));
static void set_dt FP ((void));
extern Widget LeftRightButtons ();

static void tw_HTAddCurrent FP ((Widget w, XtPointer client, XtPointer call));
static void tw_HTEntryCallback FP ((Widget w, XtPointer client, XtPointer ));
static void tw_HTDeleteCallback FP ((Widget w, XtPointer client, 
				     XtPointer call));
static void tw_HTSyncEntries FP ((Widget menu));
static void tw_HTSyncMenus FP ((void));
static void tw_HTLabel FP ((char *label));
static int tw_InsertHotTime FP ((ZebTime *zt, char *label));
static int tw_RemoveHotTime FP ((ZebTime *zt));
static Widget tw_HTCreateMenu FP ((Widget parent, char *title,
				   char *menuname, void (*callback)()));
static Widget tw_CreateWindowMenu FP ((Widget parent, char *title,
				       char *name, Widget button));
static void tw_WindowCallback FP ((Widget w, XtPointer client, XtPointer));
static Widget tw_SkipMenu FP ((Widget parent, char *title, char *name,
			       void (*callback)()));
static void tw_SkipMenuCallback FP ((Widget w, XtPointer client, XtPointer));
static void tw_Visited FP ((ZebTime *zt));


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
tw_AutoAdvance (on_off)
int on_off;
{
	AutoAdvance = on_off;
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
 * The entry point for setting the time from outside.  If zt == NULL, the
 * system time is used (as is the default at definition).
 */
{
	tw_NewTime (zt, NULL);
}



static void
tw_NewTime (zt, label)
ZebTime *zt;
char *label;
/*
 * Set the mode and time, using 'label' in the message window.  If zt is
 * NULL, go to real time.  If label is NULL, a default is supplied
 * according to the new mode.  This is the sole access routine for changing
 * the time and mode of the widget and updating the toggle buttons and text
 * window.  All changes must go through here.
 */
{
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
	if (! label)
		label = (zt) ? "History Mode" : "Real Time Mode";
	Histdate.zt_MicroSec = 0;
/*
 * If no zt we've gone into real time mode, else we've gone to history.
 * Reflect the change in the mode toggles and the time text, after checking
 * that the toggle widgets exist.
 */
	if (zt && RTToggle && HistoryToggle)
	{
		XtSetValues (RTToggle, &false, (Cardinal)1);
		XtSetValues (HistoryToggle, &true, (Cardinal)1);
	}
	else if (RTToggle && HistoryToggle)
	{
		XtSetValues (HistoryToggle, &false, (Cardinal)1);
		XtSetValues (RTToggle, &true, (Cardinal)1);
	}
	tw_HTLabel (label);
	tw_Visited (&Histdate);
/*
 * Only update the time window to real-time when AutoAdvance enabled
 */
	if (! RTMode || AutoAdvance)
		set_dt ();
	uw_sync ();
}



static void
tw_ChangeTime (zt, label)
ZebTime *zt;
char *label;
/*
 * Changing the time and/or mode from within, so we sync buttons and
 * the text window and call the callback.
 */
{
	tw_NewTime (zt, label);
	if (Tw_Callback)
		(*Tw_Callback) (RTMode ? RealTime : History,
				&Histdate, (ControlWindow == NULL),
				ControlWindow);
}



static Widget
tw_WCreate (junk, parent, appc)
int	junk;
Widget	parent;
XtAppContext appc;
/*
 * Create the complicated time widget interface.  The current notion is
 * that the menus and buttons follow the order the user would ordinarily
 * follow to set and change things, from top to bottom.  So the upper right
 * is the menu for choosing which windows will be controlled.  Below that
 * are the arrow buttons and entry widget for setting the time manually.
 * Finally are the buttons one might press once a time is entered, such as
 * "real time", "set time", and "skip".  The bottom line is the hot times
 * interface, which allows the user to choose from a set of pre-determined
 * times and add a label for a time which has just been chosen.  The help
 * and quit buttons really should be near the top, but they fit better in
 * the bottom corner.  Also, the hot times description really should be a
 * dialog which pops up when "remember" is pressed, but in the interest of
 * time and expedience that has not been done yet.  Volunteers are welcome
 * to take it up. 
 */
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
			{"enter-time", enter_time},
	};
	static char *atrans = "<Btn1Down>:	set()notify() \n\
	           	       <Btn1Up>: 	finishadj()unset()";
#ifdef notdef
	static char *htrans = "#override \n\
 			       <KeyPress>Return:	set() \n\
			       <KeyRelease>Return:	notify()unset()";
	XtTranslations htable;
#endif
	XtTranslations ttable, atable;
/*
 * Translations.
 */
	XtAppAddActions (appc, actions, XtNumber(actions));
	ttable = XtParseTranslationTable (ttrans);
	atable = XtParseTranslationTable (atrans);
#ifdef notdef
	htable = XtParseAcceleratorTable (htrans);
#endif
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
 * ----------------------------------------------------------------
 * Control one/all windows selection in upper right corner.  Uses a menu
 * of the window names in the current configuration.  
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, title);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	cform = XtCreateManagedWidget ("cform", formWidgetClass, 
				       form, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Choose target:");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("control", labelWidgetClass, 
		cform, args,n);

	n = 0;
	XtSetArg (args[n], XtNlabel, ALL_WINDOWS);	++n;
	XtSetArg (args[n], XtNfromHoriz, left);		++n;
	XtSetArg (args[n], XtNfromVert, NULL);		++n;
	XtSetArg (args[n], XtNwidth, 140);		++n;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	XtSetArg (args[n], XtNmenuName, "window-menu"); ++n;
	XtSetArg (args[n], XtNleftBitmap, MenuIcon);	++n;
	menubutton = XtCreateManagedWidget ("window-button", 
					    menuButtonWidgetClass,  
					    cform, args, n);
	TW_WindowMenu = tw_CreateWindowMenu (menubutton, 
		     "Select Window to Control", "window-menu", menubutton);
/*
 * ================================================================
 * Create a form for the history date below the title.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, cform);		n++;
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
	XtSetArg (args[n], XtNlength, sizeof(Ahistdate)-1);	n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);	n++;
	strcpy (Ahistdate, "history date");
	XtSetArg (args[n], XtNstring, Ahistdate);	n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	Htext = XtCreateManagedWidget ("entertime", asciiTextWidgetClass,
				       dateform, args, n);

/*
 * ====================================================================
 * The main button bar.
 * --------------------------------------------------------------------
 * The real-time and history buttons are in a radio group so put them
 * in their own separate box.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, dateform);	n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	button_bar = XtCreateManagedWidget ("radiobar", formWidgetClass, 
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
 * ------------------------------------------------------------------
 * Time skipping stuff immediately right of the other action buttons.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, button_bar);	n++;
	XtSetArg (args[n], XtNfromVert, dateform);	n++;
	XtSetArg (args[n], XtNdefaultDistance, 2);	n++;
	sform = XtCreateManagedWidget ("sform", formWidgetClass, form, 
				       args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Skip");		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("skip", labelWidgetClass, sform, args,n);

	sprintf (TSString, "%d", SkipSecs/TSScale);
	
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 35);		n++;
	XtSetArg (args[n], XtNlength, 80);		n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);	n++;
	XtSetArg (args[n], XtNstring, TSString);	n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	Stext = XtCreateManagedWidget ("stext", asciiTextWidgetClass,
				       sform, args, n);
/*
 * The menu for the skip interval scale (i.e., units)
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, TSUnits[2].su_string /* mins */); n++;
	XtSetArg (args[n], XtNfromHoriz, Stext);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, 50);		n++;
	XtSetArg (args[n], XtNvertDistance, 5);		n++;
	XtSetArg (args[n], XtNborderWidth, 1);		n++;
	XtSetArg (args[n], XtNmenuName, "skipmenu");	++n;
	XtSetArg (args[n], XtNleftBitmap, MenuIcon);	++n;
	menubutton = XtCreateManagedWidget ("skipscale",
			    menuButtonWidgetClass, sform, args, n);
	tw_SkipMenu (menubutton, "Skip Interval Units", "skipmenu", 
		     tw_SkipMenuCallback);
	left = menubutton;

	skipbutton = LeftRightButtons (sform, TimeSkip, NULL);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetValues (skipbutton, args, n);
/*
 * ---------------------------------------------------------------------
 * Help and quit, being more administrative type actions, get their own
 * bounding box.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, sform);	n++;
	XtSetArg (args[n], XtNfromVert, dateform);	n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	button_bar = XtCreateManagedWidget ("helpbar", formWidgetClass, 
					    form, args, n);
/*
 * Help button
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Help");		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
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
 * =====================================================================
 * The hot-times interface consists of the description label, select button,
 * remember button, and forget button, all in their own bounding box on
 * the row beneath the title row.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, button_bar);	n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	button_bar = XtCreateManagedWidget ("hotbar", formWidgetClass, 
					    form, args, n);
/*
 * Label the succeeding text window as a description of the current time
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Enter label:");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("description", labelWidgetClass, 
				      button_bar, args,n);
/*
 * Add a text window for the current hot label next
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 150);		n++;
	XtSetArg (args[n], XtNlength, 80);		n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, False);	n++;
	XtSetArg (args[n], XtNstring, "");		n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	HT_Text = XtCreateManagedWidget ("hotlabel", asciiTextWidgetClass,
					 button_bar, args, n);
	left = HT_Text;
/*
 * Menu button for the history times menu
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Select");		++n;
	XtSetArg (args[n], XtNfromHoriz, left);		++n;
	XtSetArg (args[n], XtNfromVert, NULL);		++n;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	XtSetArg (args[n], XtNmenuName, HT_MenuName);	++n;
	XtSetArg (args[n], XtNleftBitmap, MenuIcon);	++n;
	menubutton = XtCreateManagedWidget ("timemenu", menuButtonWidgetClass, 
					    button_bar, args, n);
	HT_Menu = tw_HTCreateMenu (menubutton, "Select History Time", 
				   HT_MenuName, tw_HTEntryCallback);
/*
 * Add a history time
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Remember"); n++;
	XtSetArg (args[n], XtNfromHoriz, menubutton);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
#ifdef notdef
	XtSetArg (args[n], XtNaccelerators, htable);	++n;
#endif
	left = XtCreateManagedWidget ("remember", commandWidgetClass, 
				      button_bar, args,n);
	XtAddCallback (left, XtNcallback, (XtCallbackProc) tw_HTAddCurrent, 
		       (XtPointer) 0);
	XtInstallAllAccelerators (button_bar, button_bar);
	XtInstallAccelerators (HT_Text, left);
/*
 * Delete a history time
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Forget");		++n;
	XtSetArg (args[n], XtNfromHoriz, left);		++n;
	XtSetArg (args[n], XtNfromVert, NULL);		++n;
	XtSetArg (args[n], XtNborderWidth, 1);		++n;
	XtSetArg (args[n], XtNmenuName, HT_ForgetMenuName); ++n;
	XtSetArg (args[n], XtNleftBitmap, MenuIcon);	++n;
	menubutton = XtCreateManagedWidget ("forgetbutton", 
					    menuButtonWidgetClass,  
					    button_bar, args, n);
	HT_ForgetMenu = tw_HTCreateMenu (menubutton, "Forget History Time", 
				 HT_ForgetMenuName, tw_HTDeleteCallback);
	tw_HTSyncMenus ();
/*
 * End of hot-times bar
 * ======================================================================
 */
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
TimeSkip (w, change, junk)
Widget w;
XtPointer change, junk;
/*
 * Inc/dec the current time by the current skip interval.  Automatically
 * enters history mode, as would be expected, I think...
 */
{
	long systime;

	SkipSecs = atoi (TSString) * TSScale;
	systime = TC_ZtToSys (&Histdate);
	TC_SysToZt (((long) change == 1) ? systime + SkipSecs :
			systime - SkipSecs, &Histdate);
	tw_ChangeTime (&Histdate, NULL);
}



static void
ChangeMonth (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((long) change == 1)
		datebutton (MONTHUP);
	else datebutton (MONTHDOWN);
}



static void
ChangeDay (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((long) change == 1)
		datebutton (DAYUP);
	else datebutton (DAYDOWN);
}


static void
ChangeYear (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((long) change == 1)
		datebutton (YEARUP);
	else datebutton (YEARDOWN);
}


static void
ChangeHour (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((long) change == 1)
		datebutton (HOURUP);
	else datebutton (HOURDOWN);
}


static void
ChangeMin (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((long) change == 1)
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
	long activate = (long) calldata;

	if (! activate)
		return;
	if (mode == RealTime)
		tw_ChangeTime (NULL, NULL);
	else if (tw_SyncTime())
		tw_ChangeTime (&Histdate, NULL);
}



static void
datebutton (code)
int code;
{
	void arrow_timeout ();
	zbool norep = FALSE;
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
		Tslot = tl_RelativeReq (arrow_timeout, (void*)(long)code, 
					5, 1);
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
{
	tw_InsertHotTime (zt, label);
	/*
	 * Sync the menu labels and call the add callback
	 */
	tw_HTSyncMenus ();
	if (HT_AddCallback)
		(*HT_AddCallback) (zt, label);
}



static int
tw_InsertHotTime (zt, label)
ZebTime *zt;
char *label;
/*
 * Insert this hot time into the list.  The label is truncated if necessary
 * to fit into the hot time structure.  Keep the list sorted in
 * reverse chronological order (most recent times first).  Return the
 * slot in the hot times array to which the entry was added.
 */
{
	int i;
	int insert, last;

	for (i = 0; i < NTimes; ++i)
		if (TC_LessEq (HTimes[i].ht_zt, *zt))
			break;
	insert = i;
	last = 0;
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
		 * If the times are equal we overwrite this spot, but
		 * we carry over the visited order.
		 */
		else
		{
			last = HTimes[insert].ht_visited;
		}
	}
	else if (insert >= MAX_HOT_TIMES)
		/*
		 * Overwriting the last available slot in the array
		 */
		insert = MAX_HOT_TIMES - 1;
	else
		++NTimes;	/* Adding to the end of the array */

	HTimes[insert].ht_zt = *zt;
	HTimes[insert].ht_visited = last;
	strncpy (HTimes[insert].ht_label, label, HT_LABEL_LEN);
	HTimes[insert].ht_label[HT_LABEL_LEN - 1] = '\0';
	return (insert);
}



void
tw_DeleteHotTime (zt)
ZebTime *zt;
/*
 * Remove the hot time from the list, and if there sync the menus.
 */
{
	if (tw_RemoveHotTime (zt) >= 0)
	{
		/*
		 * Sync the menu labels and call the delete callback
		 */
		tw_HTSyncMenus ();
		if (HT_DeleteCallback)
			(*HT_DeleteCallback) (zt);
	}
}



static int
tw_RemoveHotTime (zt)
ZebTime *zt;
/*
 * Remove this hot time from the list.  Just find it and move everything
 * up a slot on top of it.  Don't bother sync'ing the menus just yet.
 * If the entry had a visited count, we'll have to move up all the counts
 * below it.
 */
{
	int i, v, last;

	for (v = 0; v < NTimes; ++v)
		if (TC_Eq (HTimes[v].ht_zt, *zt))
			break;
	if (v >= NTimes)
		return (-1);
	last = HTimes[v].ht_visited;
	if (last > 0 && last < MAX_VISITED)
	{
		for (i = 0; i < NTimes; ++i)
		{
			if (HTimes[i].ht_visited > last)
				--HTimes[i].ht_visited;
		}
	}
	/*
	 * Now we need to move everything up one slot.
	 */
	--NTimes;
	for (i = v; i < NTimes; ++i)
	{
		HTimes[i] = HTimes[i + 1];
	}
	return (v);
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
	long which = (long) client;
	HotTime *ht = HTimes + which;

	tw_ChangeTime (&ht->ht_zt, ht->ht_label);
}



/* ARGSUSED */
static void
tw_HTDeleteCallback (w, client, call)
Widget w;
XtPointer client;
XtPointer call;
/*
 * A hot list entry forget callback.  
 */
{
	long which = (long) client;
	HotTime *ht = HTimes + which;

	tw_DeleteHotTime (&ht->ht_zt);
}



static void
tw_HTLabel (label)
char *label;
{
	Arg args[10];
	Cardinal narg;

	if (HT_Text)
	{
		narg = 0;
		XtSetArg (args[narg], XtNstring, label);  ++narg;
		XtSetValues (HT_Text, args, narg);
	}
}



static void
tw_HTSyncMenus ()
{
	tw_HTSyncEntries (HT_Menu);
	tw_HTSyncEntries (HT_ForgetMenu);
}



static void
tw_HTSyncEntries (menu)
Widget menu;
/*
 * Sync the menu entries with the list of hot times.
 */
{
	Arg args[10];
	Cardinal narg;
	Cardinal nchildren;
	WidgetList wlist;
	char label[64 + HT_LABEL_LEN];
	int i;

	if (! menu)	/* nothing to sync yet */
		return;
	narg = 0;
	XtSetArg (args[narg], XtNnumChildren, &nchildren);  	++narg;
	XtSetArg (args[narg], XtNchildren, &wlist);		++narg;
	XtGetValues (menu, args, narg);
	/*
	 * The first two entries are the label and line.
	 * We just act as if they don't exist.
	 */
	wlist += 2;
	nchildren -= 2;
	/*
	 * For each of the times in the list, give the entry widget the
	 * correct label.  Unmanage all of the children first so that the
	 * new geometry includes changes in any currently managed entries.
	 * Brutal but effective.
	 */
	XtUnmanageChildren (wlist, nchildren);
	for (i = 0; (i < NTimes) && (i < nchildren); ++i)
	{
		Widget w = wlist[i];
		if (HTimes[i].ht_visited > 0)
			sprintf (label, "%19s %2i %s", 
				 TC_AscTime (&HTimes[i].ht_zt, TC_Full),
				 HTimes[i].ht_visited, HTimes[i].ht_label);
		else
			sprintf (label, "%19s    %s", 
				 TC_AscTime (&HTimes[i].ht_zt, TC_Full),
				 HTimes[i].ht_label);
		XtSetArg (args[0], XtNlabel, label);
		XtSetValues (w, args, (Cardinal)1);
	}
	if (i)
		XtManageChildren (wlist, (Cardinal) i);
}



static Widget
tw_HTCreateMenu (parent, title, name, callback)
Widget parent;
char *title;
char *name;
void (*callback)();
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
	XtSetArg (args[narg], XtNlabel, title);	++narg;
	XtSetArg (args[narg], XtNallowShellResize, True); ++narg;
	menu = XtCreatePopupShell (name, simpleMenuWidgetClass,
				   parent, args, narg);
/*
 * First line object
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
			       callback, (XtPointer)(long)i);
	}
	return (menu);
}



static Widget
tw_SkipMenu (parent, title, name, callback)
Widget parent;
char *title;
char *name;
void (*callback)();
/*
 * Create a menu for skip interval units.  We REQUIRE the parent to be
 * the menu button whose label will change with the units.
 */
{
	Arg args[10];
	Cardinal narg;
	Widget entry;
	Widget menu;
	int i;

	narg = 0;
	XtSetArg (args[narg], XtNlabel, title);	++narg;
	menu = XtCreatePopupShell (name, simpleMenuWidgetClass,
				   parent, args, narg);
/*
 * Line object to separate choices from title
 */
	XtCreateManagedWidget ("line", smeLineObjectClass, menu, NULL, 0);
/*
 * Create one entry for each unit.
 */
	for (i = 1; i < XtNumber(TSUnits); i++)
	{
		XtSetArg (args[0], XtNlabel, TSUnits[i].su_label);
		narg = 1;
		entry = XtCreateManagedWidget ("skipentry", smeBSBObjectClass,
					       menu, args, narg);
		XtAddCallback (entry, XtNcallback, (XtCallbackProc) 
			       callback, (XtPointer)(long)i);
	}
	return (menu);
}



/* ARGSUSED */
static void
tw_SkipMenuCallback (w, client, call)
Widget w;
XtPointer client;
XtPointer call;
/*
 * Change the units (scale) of the skip interval.
 */
{
	long which = (long) client;
	Widget button = XtParent(XtParent(w));
	Arg arg;

	TSScale = TSUnits[which].su_scale;
	XtSetArg (arg, XtNlabel, TSUnits[which].su_string);
	XtSetValues (button, &arg, (Cardinal) 1);
}



static Widget
tw_CreateWindowMenu (parent, title, name, button)
Widget parent;
char *title;
char *name;
Widget button;		/* the menu button passed to the callbacks */
/*
 * Create a menu of window names and return the widget
 */
{
	Arg args[10];
	Cardinal narg;
	Widget entry;
	Widget menu;
	int i;

	narg = 0;
	XtSetArg (args[narg], XtNlabel, title);	++narg;
	menu = XtCreatePopupShell (name, simpleMenuWidgetClass,
				   parent, args, narg);
/*
 * Line between title and first entry.
 */
	XtCreateManagedWidget ("line", smeLineObjectClass, menu, NULL, 0);
/*
 * The obligatory 'All' target, managed here, now, and forever after.
 */
	narg = 0;
	XtSetArg (args[narg], XtNlabel, ALL_WINDOWS);	narg++;
	entry = XtCreateManagedWidget ("all", smeBSBObjectClass,
				       menu, args, narg);
	XtAddCallback (entry, XtNcallback, (XtCallbackProc) 
		       tw_WindowCallback, (XtPointer) button);
/*
 * Create entries for the other window names, but don't manage them now.
 */
	narg = 0;
	XtSetArg (args[narg], XtNlabel, "[none]");	narg++;
	for (i = 0; i < MAX_WINDOWS; i++)
	{
		entry = XtCreateWidget ("wname", smeBSBObjectClass,
					menu, args, narg);
		XtAddCallback (entry, XtNcallback, (XtCallbackProc) 
			       tw_WindowCallback, (XtPointer) button);
	}
	return (menu);
}



/* ARGSUSED */
static void
tw_WindowCallback (w, client, call)
Widget w;
XtPointer client;
XtPointer call;
/*
 * Change the name of the window we're controlling.
 */
{
	Arg arg;
	Widget button = (Widget) client;
	char *label;

	XtSetArg (arg, XtNlabel, &label);
	XtGetValues (w, &arg, (Cardinal) 1);
	if (strcmp (label, ALL_WINDOWS) == 0)
		ControlWindow = NULL;
	else
		ControlWindow = label;
	XtSetArg (arg, XtNlabel, label);
	XtSetValues (button, &arg, (Cardinal) 1);
}



void
tw_SetWindowNames (nwin, names)
int nwin;
char **names;
/*
 * Set the window menu (if it exists) to this list of names.
 */
{
	Arg args[10];
	Cardinal narg;
	Cardinal nchildren;
	WidgetList wlist;
	Widget menu;
	int i;

	menu = TW_WindowMenu;
	if (! menu)
		return;
	narg = 0;
	XtSetArg (args[narg], XtNnumChildren, &nchildren);  	++narg;
	XtSetArg (args[narg], XtNchildren, &wlist);		++narg;
	XtGetValues (menu, args, narg);
	/*
	 * The first three entries are the label, line, and "All".
	 * We just act as if they don't exist.  "All" is managed when
	 * created and never changes.
	 */
	wlist += 3;
	nchildren -= 3;
	/*
	 * For each of the names in the list, give the entry widget
	 * the correct label and make sure it is managed.
	 */
	for (i = 0; (i < nwin) && (i < nchildren); ++i)
	{
		Widget w = wlist[i];
		XtSetArg (args[0], XtNlabel, names[i]);
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



static void
tw_Visited (zt)
ZebTime *zt;
/*
 * Add this time to the top of the most-recently-visited entries
 */
{
	tw_AddVisited (zt, 1, NULL);
}



void
tw_AddVisited (zt, visited, label)
ZebTime *zt;
int visited;
char *label;
/*
 * Set the visited count of this time and shift the other counts to make
 * room for it.  If the time is new, use the given label.  Otherwise the
 * label has no effect.
 */
{
	static char *generic = "";
	int last;
	HotTime *ht, *remove = NULL;
	int insert;
	int shift;
/*
 * Enforce some limits
 */
	if (visited < 0 || visited > MAX_VISITED)
		visited = 0;
/*
 * See if this time is already in the menu, and see if another time already
 * has this visited count.
 */
	shift = 0;
	insert = -1;
	for (ht = HTimes; ht - HTimes < NTimes; ++ht)
	{
		if (TC_Eq (ht->ht_zt, *zt))
			insert = ht - HTimes;
		if (ht->ht_visited == visited)
			shift = 1;
	}
	last = MAX_VISITED + 1;
	if (insert < 0)
	{
		insert = tw_InsertHotTime (zt, (label) ? label : generic);
	}
	else if (HTimes[insert].ht_visited)
	{
		last = HTimes[insert].ht_visited;
	}
/*
 * Now go back through the hot times and shift the visited counts
 * to make room for the new one, unless the new one's count is zero or
 * there was no time with that count and we don't need to shift.
 */
	for (ht = HTimes; ht - HTimes < NTimes; ++ht)
	{
		if (TC_Eq (ht->ht_zt, *zt))
		{
			ht->ht_visited = visited;
		}
		else if (visited > 0 && shift)
		{
			if ((ht->ht_visited >= visited) && 
			    (ht->ht_visited < last))
			{
				++ht->ht_visited;
				if (ht->ht_visited > MAX_VISITED &&
				    ht->ht_label[0] == '\0')
					remove = ht;
				ht->ht_visited %= (MAX_VISITED + 1);
			}			
		}
	}	
	/*
	 * Even though the time may not be new, the visit count changed.
	 */
	if (HT_AddCallback)
		(*HT_AddCallback) (zt, HTimes[insert].ht_label);
	/*
	 * Remove a generic visited time which dropped off the end.
	 */
	if (remove)
	{
		ZebTime rm;

		rm = remove->ht_zt;
		tw_RemoveHotTime (&rm);
		if (HT_DeleteCallback)
			(*HT_DeleteCallback) (&rm);
	}
	/*
	 * Sync the menus on our way out.
	 */
	tw_HTSyncMenus ();
}


