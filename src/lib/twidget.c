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
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"


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
static Widget Form, Htext, Stext;

/*
 * Histdate is the history date that appears in the window.  Maxdate is
 * the highest date we've ever seen, used to keep the date from being
 * moved into the future.
 */
static date Histdate, Maxdate;
static char Ahistdate[80];
static char Title[200];
static int Tslot = -1;	/* Timeout slot		*/
static int SkipMin = 5;
static char TSString[10];
static int (*Tw_Callback) () = 0;


static Widget tw_WCreate ();
static int tw_WCallback ();
static void finish_arrow ();
static void ChangeMonth (), ChangeDay (), ChangeYear (); 
static void ChangeHour (), ChangeMin (), ChangeSec ();
static void TimeSkip ();
extern Widget LeftRightButtons ();


void
tw_DefTimeWidget (callback, title)
int	(*callback) ();
char	*title;
/*
 * Hook the time widget into the UI.
 */
{
	tl_GetTime (&Histdate);
	Histdate.ds_hhmmss -= Histdate.ds_hhmmss % 100;
	strcpy (Title, title);
	Tw_Callback = callback;
	uw_def_widget ("time", "Time control widget", tw_WCreate, 0, 0);
}


static Widget
tw_WCreate (junk, parent, appc)
int	junk;
Widget	parent;
XtAppContext appc;
{
	Arg args[10];
	Widget form, title, f, left, dform, sform;
	Widget mbutton, dbutton, ybutton, hbutton, minbutton, sbutton;
	Widget skipbutton;
	Widget MonText, DayText, YearText, HMSText;
	int n;
	static char *ttrans = "<Btn1Down>,<Btn1Up>: 	set()notify()";
	XtTranslations ttable;
/*
 * Translations.
 */
	ttable = XtParseTranslationTable (ttrans);
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
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, NULL);
	title = XtCreateManagedWidget ("title", labelWidgetClass, form,
		args, 3);
/*
 * Set history date form.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, title);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	dform = XtCreateManagedWidget ("dform", formWidgetClass, form, args, 4);
/*
 * Left/right buttons for month, day, year.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Day" );		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, 60);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("dlabel", labelWidgetClass, dform,
		args, n);

	dbutton = LeftRightButtons (dform, ChangeDay);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetValues (dbutton, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Month");		n++;
	XtSetArg (args[n], XtNfromHoriz, dbutton);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, 60);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("mlabel", labelWidgetClass, dform,
		args, n);

	mbutton = LeftRightButtons (dform, ChangeMonth);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetValues (mbutton, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Year");		n++;
	XtSetArg (args[n], XtNfromHoriz, mbutton);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, 60);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("ylabel", labelWidgetClass, dform,
		args, n);

	ybutton = LeftRightButtons (dform, ChangeYear);

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
	XtSetArg (args[n], XtNwidth, 60);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("hlabel", labelWidgetClass, dform,
		args, n);

	hbutton = LeftRightButtons (dform, ChangeHour);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, dbutton);	n++;
	XtSetValues (hbutton, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Minutes" );	n++;
	XtSetArg (args[n], XtNfromHoriz, hbutton);	n++;
	XtSetArg (args[n], XtNfromVert, dbutton);	n++;
	XtSetArg (args[n], XtNwidth, 60);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	left = XtCreateManagedWidget ("minlabel", labelWidgetClass, dform,
		args, n);

	minbutton = LeftRightButtons (dform, ChangeMin);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, dbutton);	n++;
	XtSetValues (minbutton, args, n);
/*
 * Text widget for the history date.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, hbutton);	n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 250);		n++;
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
	XtSetArg (args[0], XtNborderWidth, 2);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, dform);
	XtSetArg (args[3], XtNdefaultDistance, 5);
	f = XtCreateManagedWidget ("pmform", formWidgetClass, form, args, 4);

	XtSetArg (args[0], XtNlabel, "Plot mode:");
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNborderWidth, 0);
	left = XtCreateManagedWidget ("plotmode", labelWidgetClass, f, args,4);

	XtSetArg (args[0], XtNlabel, "Real Time");
	XtSetArg (args[1], XtNfromHoriz, left);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNborderWidth, 0);
	XtSetArg (args[4], XtNstate, True);
	left = XtCreateManagedWidget ("rt", toggleWidgetClass, f, args, 5);
	XtOverrideTranslations (left, ttable);
	XtAddCallback (left, XtNcallback, tw_WCallback, RealTime);

	XtSetArg (args[0], XtNlabel, "History");
	XtSetArg (args[1], XtNfromHoriz, left);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNradioGroup, left);
	XtSetArg (args[4], XtNborderWidth, 0);
	left = XtCreateManagedWidget ("history", toggleWidgetClass, f, args,5);
	XtOverrideTranslations (left, ttable);
	XtAddCallback (left, XtNcallback, tw_WCallback, History);
/*
 * Time skipping stuff.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, f);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);		n++;
	sform = XtCreateManagedWidget ("sform", formWidgetClass, form, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "Skip:");		n++;
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
	XtSetArg (args[n], XtNwidth, 50);		n++;
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
	XtSetArg (args[n], XtNlabel, "minutes.");	n++;
	XtSetArg (args[n], XtNfromHoriz, Stext);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("minutes", labelWidgetClass, sform, 
		args, n);

	skipbutton = LeftRightButtons (sform, TimeSkip);

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
TimeSkip (w, change, junk)
Widget w;
XtPointer change, junk;
{
	int min;

	SkipMin = atoi (TSString);

	min = (SkipMin / 60) * 10000 + (SkipMin % 60) * 100;
	if ((int) change == 1)
		pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, min); 
	else
		pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, min); 
	set_dt ();
	(*Tw_Callback) (History, &Histdate);
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
ChangeSec (w, change, junk)
Widget w;
XtPointer change, junk;
{
	if ((int) change == 1)
		datebutton (SECUP);
	else datebutton (SECDOWN);
}



static int
tw_WCallback (w, mode, value)
Widget w;
enum pmode mode;
int value;
/*
 * The toggle callback.
 */
{
	if (value)
		(*Tw_Callback) (mode, &Histdate);
}



int
datebutton (code)
int code;
{
	void arrow_timeout ();
	bool norep = TRUE;
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
	{
		Tslot = tl_AddRelativeEvent(arrow_timeout, (char *) code, 5,1);
	}
	uw_sync ();
}




void
arrow_timeout (t, code)
time *t;
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
	date incr, temp;
	int y, m, d;

	y = Histdate.ds_yymmdd / 10000;
	m = (Histdate.ds_yymmdd % 10000) / 100;
	d = Histdate.ds_yymmdd % 100;

	switch (which)
	{
	   case MONTHUP:
		m = m % 12 + 1;
		if (m == 1) y++;
		Histdate.ds_yymmdd = (y * 10000) + (m * 100) + d;
		break;
	   case MONTHDOWN:
		m -= 1;
		if (m <= 0) 
		{
			m = 12;
			y--;
		}
		Histdate.ds_yymmdd = (y * 10000) + (m * 100) + d;
		break;
	   case DAYUP:
	   	pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 240000);
		break;
	   case DAYDOWN:
	   	pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 240000);
		break;
	   case YEARUP:
		y += 1;
		Histdate.ds_yymmdd = (y * 10000) + (m * 100) + d;
		break;
	   case YEARDOWN:
		y -= 1;
		Histdate.ds_yymmdd = (y * 10000) + (m * 100) + d;
		break;
	   case HOURUP:
	   	pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 10000);
		break;
	   case HOURDOWN:
	   	pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 10000);
		break;
	   case MINUP:
	   	pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 100);
		break;
	   case MINDOWN:
	   	pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 100);
		break;
	   case SECUP:
	   	pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 1);
		break;
	   case SECDOWN:
	   	pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 1);
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
	char dbuf[40], *time, *strchr ();
/*
 * Format, then split, the date.
 */
	ud_format_date (dbuf, &Histdate, UDF_FULL);
	strcpy (Ahistdate, dbuf);
	time = strchr (dbuf, ',');
	*time++ = '\0';
/*
 * Text too.
 */
	XtSetArg (args[0], XtNstring, Ahistdate);
	XtSetValues (Htext, args, 1);
}



