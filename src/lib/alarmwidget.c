/*
 * The alarm widget code.
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
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Cardinals.h>

# include <ui.h>
# include <ui_date.h>
# include <defs.h>
# include <message.h>
# include <timer.h>

MAKE_RCSID ("$Id: alarmwidget.c,v 1.1 1992-04-06 17:59:13 burghart Exp $")

# define STRLEN 50
# define STATUSSTR 200

/*
 * Global stuff
 */
static Widget Form, AlarmText, RepeatText, StatusText;
static Widget RepeatButton, SetButton, CancelButton;
static char AlarmTime[STRLEN], RepeatMin[STRLEN];
static int Repeat = TRUE;
static int Tslot;

Widget aw_CreateWidget ();
static void ChangeRepeat (), SetAlarm (), CancelAlarm ();
static void SetStatus (), SoundAlarm ();



void
aw_DefAlarmWidget ()
/*
 * Hook the alarm widget into the UI.
 */
{
	uw_def_widget ("alarm", "Alarm Widget", aw_CreateWidget, 0, 0);
}



Widget
aw_CreateWidget (junk, parent, appc)
int	junk;
Widget	parent;
XtAppContext appc;
/*
 * Create the alarm widget.
 */
{
	Arg args[20];
	Widget form, left, above;
	int n;
/*
 * Put a form inside it.
 */
	XtSetArg (args[0], XtNdefaultDistance, 5);
	form = XtCreateManagedWidget ("form", formWidgetClass, parent, args, 1);
/*
 * Sound the alarm at?
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Sound alarm at");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("alarm_text", labelWidgetClass, 
		form, args, n);

	strcpy (AlarmTime, "22:30:00");

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 70);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);	n++;
	XtSetArg (args[n], XtNstring, AlarmTime);	n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	left = AlarmText = XtCreateManagedWidget ("alarm_at", 
		asciiTextWidgetClass, form, args, n);
/*
 * Repeat?
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, Repeat ? "Repeat" : "No Repeat");	n++;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, 70);		n++;
	left = RepeatButton = XtCreateManagedWidget ("repeatbutton", 
		commandWidgetClass, form, args, n);
	XtAddCallback (RepeatButton, XtNcallback, ChangeRepeat, NULL);

	n = 0;
	XtSetArg (args[n], XtNlabel, "every");		n++;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("every", labelWidgetClass, 
		form, args, n);

	strcpy (RepeatMin, "30");

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 30);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);	n++;
	XtSetArg (args[n], XtNstring, RepeatMin);	n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	left = RepeatText= XtCreateManagedWidget ("repeat_min", 
		asciiTextWidgetClass, form, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "minutes.");	n++;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	above = XtCreateManagedWidget ("minutes", labelWidgetClass, 
		form, args, n);
/*
 * Set the alarm.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Set Alarm");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNwidth, 70);		n++;
	left = SetButton = XtCreateManagedWidget ("setbutton", 
		commandWidgetClass, form, args, n);
	XtAddCallback (SetButton, XtNcallback, SetAlarm, NULL);
/*
 * Cancel the alarm.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Cancel Alarm");	n++;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNwidth, 100);		n++;
	above = CancelButton = XtCreateManagedWidget ("cancelbutton", 
		commandWidgetClass, form, args, n);
	XtAddCallback (CancelButton, XtNcallback, CancelAlarm, NULL);
/*
 * A status line.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNwidth, 350);		n++;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	above = XtCreateManagedWidget ("statusform", formWidgetClass, 
		form, args, n);

	n = 0;
	XtSetArg (args[n], XtNlabel, "");		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, 350);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	StatusText = XtCreateManagedWidget ("statustext", labelWidgetClass, 
		above, args, n);

	return (form);
}


static void
ChangeRepeat ()
/*
 * Toggle repeating.
 */
{
	Arg arg;

	Repeat = ! Repeat;
	XtSetArg (arg, XtNlabel, Repeat ? "Repeat" : "No Repeat");
	XtSetValues (RepeatButton, &arg, 1);
}



static void
SetStatus (string)
/*
 * Set the status line to 'string'.
 */
{
	Arg arg;

	XtSetArg (arg, XtNlabel, string);
	XtSetValues (StatusText, &arg, 1);
}



static void
SetAlarm ()
/*
 * Set the alarm.
 */
{
	ZebTime t;
	date uit;
	int hours, minutes, seconds;
	int current;
	int delay, incr;
	char string[STATUSSTR];
/*
 * Get the current time.
 */
	tl_Time (&t);
	TC_ZtToUI (&t, &uit);
/*
 * How different is the current time from the alarm time?
 */
	sscanf (AlarmTime, "%d:%d:%d", &hours, &minutes, &seconds);
	seconds += hours * 3600 + minutes * 60;
	current = uit.ds_hhmmss / 10000 * 3600 
		+ uit.ds_hhmmss % 10000 / 100 * 60 
		+ uit.ds_hhmmss % 100; 
	delay = seconds - current;
	if (delay < 0)
	{
		SetStatus ("Can't set alarm in the past.");
		return;
	}
/*
 * How often should the alarm be repeated?
 */
	if (Repeat)
	{
		sscanf (RepeatMin, "%d", &incr);
		incr *= 60;
	}
	else
		incr = 0;
/*
 * Add a timer event to sound the alarm.
 */
	tl_Cancel (Tslot);
	Tslot = tl_AddRelativeEvent (SoundAlarm, NULL, delay*INCFRAC, 
		incr*INCFRAC);
/*
 * Set the status line.
 */
	if (Repeat)
		sprintf (string, "Alarm set for %s; repeat every %s minutes.", 
			AlarmTime, RepeatMin);
	else
		sprintf (string, "Alarm set for %s.", AlarmTime);
	SetStatus (string);
}



static void
SoundAlarm ()
/*
 * Sound the alarm.
 */
{
	msg_ELog (EF_INFO, "Sounding alarm.");
	DoSound ("cuckoo");
}



static void
CancelAlarm ()
/*
 * Cancel the alarm.
 */
{

	tl_Cancel (Tslot);
	SetStatus ("Alarm cancelled.");
}
