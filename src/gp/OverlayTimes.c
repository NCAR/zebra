/*
 * Overlay times widget.  This is where plots can write per-component data
 * details like exact data time, altitude, etc.
 */
/*		Copyright (C) 1994 by UCAR
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

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/AsciiText.h>

# include <defs.h>
# include <pd.h>
# include <dm.h>
# include "GraphProc.h"

RCSID("$Id: OverlayTimes.c,v 2.7 2001-01-16 22:27:36 granger Exp $")

static Widget	OTWidget = NULL;
static char	OTString[1024];

/*
 * Prototypes
 */
static Widget	ot_Create FP ((char *junk, Widget parent, XtAppContext appc));
static void	ot_Update FP ((void));




void
ot_Init ()
/*
 * Declare the overlay times widget to UI, so that it can create it when
 * requested.
 */
{
	char	title[40];

	sprintf (title, "Data times for %s", dm_WindowName());
	uw_def_widget ("overlay", title, ot_Create, 0, 0);
	uw_NoHeader ("overlay");
}



static void
ot_Dismiss ()
/*
 * Popdown the overlay widget through UI
 */
{
	uw_popdown ("overlay");
}



static Widget
ot_Create (junk, parent, appc)
char *junk;
Widget parent;
XtAppContext appc;
/*
 * Actually create the overlay status widget.
 */
{
	Arg	args[15];
	int	n;
	char	title[40];
	Widget	form, w, above;
/*
 * We know that UI will create us with a Form widget parent and a TopLevelShell
 * as a grandparent.  We *need* a resizable shell, so we force it..
 */
	n = 0;
	XtSetArg (args[n], XtNallowShellResize, True); n++;
	XtSetValues (XtParent(parent), args, n);

	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 5); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = parent;
	XtSetValues (form, args, n);
/*
 * The label which holds our title.
 */
	sprintf (title, "Data times for %s", dm_WindowName());
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, title); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNbottom, XtChainTop); n++;
	XtSetArg (args[n], XtNtop, XtChainTop); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	w = XtCreateManagedWidget ("overlayLabel", labelWidgetClass,
		form, args, n);
	above = w;
/*
 * Help button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNbottom, XtChainTop); n++;
	XtSetArg (args[n], XtNtop, XtChainTop); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	XtSetArg (args[n], XtNlabel, "Help"); n++;
	w = XtCreateManagedWidget ("overlayHelp", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, HelpCallback, 
		       (XtPointer)GP_HELP_OVERLAYS);
/*
 * Dismiss button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Dismiss"); n++;
	XtSetArg (args[n], XtNbottom, XtChainTop); n++;
	XtSetArg (args[n], XtNtop, XtChainTop); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	w = XtCreateManagedWidget ("overlayDismiss", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) ot_Dismiss, 0);
/*
 * Create an AsciiText widget to hold our info.  The "resizable" resource
 * is a constraint resource available to us because our parent is a Form
 * widget.  We set it to let the form know we may be resizing.
 */
	n = 0;
	XtSetArg (args[n], XtNstring, "nothing"); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeBoth); n++;
	XtSetArg (args[n], XtNresizable, True); n++;
	XtSetArg (args[n], XtNbottom, XtChainBottom); n++;
	XtSetArg (args[n], XtNtop, XtChainTop); n++;
	XtSetArg (args[n], XtNright, XtChainRight); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNdisplayCaret, False); n++;
	OTWidget = XtCreateManagedWidget ("OverlayTimes", asciiTextWidgetClass,
					  form, args, n);
/*
 * Put in the current string
 */
	ot_Update ();

	return (form);
}




void
ot_SetString (text)
char *text;
/*
 * Set a new string for the widget.
 */
{
	strcpy (OTString, text);
	ot_Update ();
}




void 
ot_Append (text)
char *text;
/*
 * Append this string to our status.
 */
{
	strcat (OTString, text);
	ot_Update ();
}




void
ot_AddStatusLine (comp, plat, fname, t)
char	*comp, *plat, *fname;
ZebTime *t;
/*
 * Add a "standard" status line to the overlay times widget with component, 
 * platform, field, and time.
 */
{
	sprintf (OTString + strlen (OTString), "%-14s ", comp);
	sprintf (OTString + strlen (OTString), "%-10s ", plat);
	sprintf (OTString + strlen (OTString), "%-10s ", fname);

	TC_EncodeTime (t, TC_Full, OTString + strlen (OTString));
	strcat (OTString, "\n");

	ot_Update ();
}




char *
ot_GetString ()
/*
 * Return the current status string.
 */
{
	return (OTString);
}




static void
ot_Update ()
/*
 * Display the updated text
 */
{
	Arg	arg;
	char	*current;
	XawTextBlock	xtext;

	if (! OTWidget)
		return;
/*
 * Using XawTextReplace() seems to be the only way to get the resize of
 * the AsciiTextWidget to work properly.
 */
	XtSetArg (arg, XtNstring, &current);
	XtGetValues (OTWidget, &arg, 1);

	xtext.firstPos = 0;
	xtext.length = strlen (OTString);
	xtext.ptr = OTString;
	xtext.format = FMT8BIT;

	XawTextReplace (OTWidget, 0, strlen (current), &xtext);
	XawTextInvalidate (OTWidget, 0, strlen (OTString) + 1);
}
