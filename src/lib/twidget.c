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

# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Cardinals.h>

# include <ui.h>
# include <ui_date.h>
# include "defs.h"
# include "message.h"
# include "timer.h"

MAKE_RCSID ("$Id: twidget.c,v 2.11 1993-03-12 23:48:55 granger Exp $")


# define LABELWIDTH	60
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

/*
 * Global stuff
 */
static Widget Form, Htext, Stext, ControlButton;

/*
 * Histdate is the history date that appears in the window.  Maxdate is
 * the highest date we've ever seen, used to keep the date from being
 * moved into the future.
 */
static ZebTime Histdate, Maxdate;
static char Ahistdate[80];
static char Title[200];
static int Tslot = -1;	/* Timeout slot		*/
static int SkipMin = 5;
static char TSString[10];
static int ControlAll = TRUE;
static int (*Tw_Callback) () = 0;

static Widget tw_WCreate ();
static void tw_WCallback ();
static void finish_arrow (), datebutton ();
static void ChangeMonth (), ChangeDay (), ChangeYear (); 
static void ChangeHour (), ChangeMin ();
static void TimeSkip ();
static void ChangeControl ();
static void CallForHelp ();
extern Widget LeftRightButtons ();


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
	/* Histdate.ds_hhmmss -= Histdate.ds_hhmmss % 100; */
	TC_ZtSplit (&Histdate, &year, &month, &day, &hour, &minute, &second,0);
	TC_ZtAssemble (&Histdate, year, month, day, hour, minute, 0, 0);

	strcpy (Title, title);
	Tw_Callback = callback;
	uw_def_widget ("time", "Time control", tw_WCreate, 0, 0);
}


static Widget
tw_WCreate (junk, parent, appc)
int	junk;
Widget	parent;
XtAppContext appc;
{
	Arg args[20];
	Widget form, title, f, left, dayleft, dform, sform, cform, hform;
	Widget mbutton, dbutton, ybutton, hbutton, minbutton, sbutton;
	Widget skipbutton, helpbutton;
	Widget MonText, DayText, YearText, HMSText;
	int n;
	static char *ttrans = "<Btn1Down>,<Btn1Up>: 	set()notify()";
	static XtActionsRec actions[] = {
			{"finishadj", finish_arrow}
	};
	static char *atrans = "<Btn1Down>:	set()notify() \n\
	           	       <Btn1Up>: 	finishadj()unset()";
	XtTranslations ttable, atable;
/*
 * Translations.
 */
	ttable = XtParseTranslationTable (ttrans);
	XtAppAddActions (appc, actions, 1);
	atable = XtParseTranslationTable (atrans);
/*
 * Make the bitmaps for the left and right arrow buttons.
 */
	bm_BuildBitmaps (parent);
/*
 * Put a form inside it.
 */
	XtSetArg (args[0], XtNdefaultDistance, 5);
	form = XtCreateManagedWidget ("form", formWidgetClass, parent,args, 1);
/*
 * Put the top label on the form.
 */
	XtSetArg (args[0], XtNlabel, Title);
	XtSetArg (args[1], XtNfromHoriz, form);
	XtSetArg (args[2], XtNfromVert, NULL);
	title = XtCreateManagedWidget ("title", labelWidgetClass, form,
		args, 3);
/*
 * Establish a help button
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, title);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	hform = XtCreateManagedWidget ("hform", formWidgetClass, 
				       form, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Help");		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	helpbutton = XtCreateManagedWidget ("helpbutton", 
					    commandWidgetClass, 
					    hform, args,n);
	XtAddCallback (helpbutton, XtNcallback, CallForHelp, NULL);
/*
 * Control one/all windows selection.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, hform);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
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
 * Set history date form.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, cform);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	dform = XtCreateManagedWidget ("dform", formWidgetClass, form, args, 4);
/*
 * Left/right buttons for month, day, year.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Day" );		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, LABELWIDTH);	n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("dlabel", labelWidgetClass, dform,
		args, n);

	dbutton = LeftRightButtons (dform, ChangeDay, atable);

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
	left = XtCreateManagedWidget ("mlabel", labelWidgetClass, dform,
		args, n);

	mbutton = LeftRightButtons (dform, ChangeMonth, atable);

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
	left = XtCreateManagedWidget ("ylabel", labelWidgetClass, dform,
		args, n);

	ybutton = LeftRightButtons (dform, ChangeYear, atable);

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
	left = XtCreateManagedWidget ("hlabel", labelWidgetClass, dform,
		args, n);

	hbutton = LeftRightButtons (dform, ChangeHour, atable);

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
	left = XtCreateManagedWidget ("minlabel", labelWidgetClass, dform,
		args, n);

	minbutton = LeftRightButtons (dform, ChangeMin, atable);

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
	Htext = XtCreateManagedWidget ("htext", asciiTextWidgetClass,
		dform, args, n);
/*
 * Throw in the plot mode toggle.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, dform);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	f = XtCreateManagedWidget ("pmform", formWidgetClass, form, args, n);

	XtSetArg (args[0], XtNlabel, "Plot mode:");
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNborderWidth, 1);
	left = XtCreateManagedWidget ("plotmode", labelWidgetClass, f, args,4);

	XtSetArg (args[0], XtNlabel, "Real Time");
	XtSetArg (args[1], XtNfromHoriz, left);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNborderWidth, 1);
	XtSetArg (args[4], XtNstate, True);
	left = XtCreateManagedWidget ("rt", toggleWidgetClass, f, args, 5);
	XtOverrideTranslations (left, ttable);
	XtAddCallback (left, XtNcallback, tw_WCallback, (XtPointer) RealTime);

	XtSetArg (args[0], XtNlabel, "History");
	XtSetArg (args[1], XtNfromHoriz, left);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNradioGroup, left);
	XtSetArg (args[4], XtNborderWidth, 0);
	left = XtCreateManagedWidget ("history", toggleWidgetClass, f, args,5);
	XtOverrideTranslations (left, ttable);
	XtAddCallback (left, XtNcallback, tw_WCallback, (XtPointer) History);
/*
 * Time skipping stuff.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, f);		n++;
	XtSetArg (args[n], XtNfromVert, dform);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);		n++;
	sform = XtCreateManagedWidget ("sform", formWidgetClass, form, args, n);

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
	return (form);
}



static void
CallForHelp ()
/*
 * Performs a 'help' command through UI.  Note that technically we
 * are not guaranteed such a command has been defined, but since
 * this "widget" is used exclusively by the Zeb display manager... XXX
 */
{
	ui_perform ("help historytime");
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
	int min;
	long systime;

	SkipMin = atoi (TSString);
# ifdef notdef
	min = (SkipMin / 60) * 10000 + (SkipMin % 60) * 100;
	if ((int) change == 1)
		pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, min); 
	else
		pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, min); 
# endif
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


static void
tw_WCallback (w, mode, value)
Widget w;
enum pmode mode;
int value;
/*
 * The toggle callback.
 */
{
	if (value)
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



set_dt ()
/*
 * Set the value of the date and time widgets.
 */
{
	Arg args[2];
	char dbuf[40], *timestr, *strchr ();
/*
 * Format, then split, the date.
 */
	TC_EncodeTime (&Histdate, TC_Full, dbuf);
	strcpy (Ahistdate, dbuf);
	timestr = strchr (dbuf, ',');
	*timestr++ = '\0';
/*
 * Text too.
 */
	XtSetArg (args[0], XtNstring, Ahistdate);
	XtSetValues (Htext, args, 1);
}



