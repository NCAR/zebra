//
// Implementation of the status window.
//
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

# include <stdio.h>
# include <iostream>
# include <unistd.h>
extern "C" 
{
#	include <X11/Intrinsic.h>
#	include <X11/Shell.h>
#	include <X11/StringDefs.h>
#	include <X11/Xaw/Command.h>
#	include <X11/Xaw/Form.h>
#	include <X11/Xaw/Label.h>
#	include <X11/Xaw/Scrollbar.h>
#	include <X11/Xaw/Toggle.h>
}
# include "dsmanage.h"
# include "dsmWindows.h"
# include "StatusWin.h"

static char *rcsid = "$Id: StatusWin.cc,v 1.7 2002-12-18 00:24:13 granger Exp $";


//
// Forwards.
//
void AbortButton (Widget, XtPointer, XtPointer);




StatusWindow::StatusWindow (const char *source, int nf, int bytes) :
	dsPopupWindow (*Disp, "Data load status window", 50)
//
// Make a status window.
//
{
	Arg args[15];
	int n;
	Widget above = corner, left = NULL;
	char title[100];
	const int width = 300;
//
// Redo the title.
//
	strcpy (title, "Loading from: ");
	strcat (title, source);
	SetTitle (title);
	sw_nf = nf;
	sw_bytes = bytes;
	sw_abort = 0;
//
// The textual info line.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, " ");			n++;
	XtSetArg (args[n], XtNwidth, width);			n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	AddConstraints (args, &n);
	sw_textline = above = XtCreateManagedWidget ("textline",
			labelWidgetClass, dw_form, args, n);
//
// Underneath it is the scrollbar to show graphically.
//
	n = 0;
	XtSetArg (args[n], XtNwidth, width);			n++;
	XtSetArg (args[n], XtNorientation, XtorientHorizontal);	n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	AddConstraints (args, &n);
	sw_scroll = above = XtCreateManagedWidget ("scroll",
		scrollbarWidgetClass, dw_form, args, n);
//
// An abort button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Abort");			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("abort", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, AbortButton, this);
# ifdef notdef
//
// A hold button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Hold");			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("hold", toggleWidgetClass, dw_form,
			args, n);
# endif
//
// Tweak the status stuff and we are done.
//
	(void) status (0, 0);
}



StatusWindow::~StatusWindow ()
{ }				     





int
StatusWindow::status (int nf, int bytes)
//
// Set the completion status, and return the abort status.
//
{
	Arg args[2];
	float len;
	char text[120];
//
// Do the text line first.
//
	sprintf (text, "%d of %d files (%.3f/%.3f MB) loaded", nf, sw_nf,
		bytes/1048576.0, sw_bytes/1048576.0);
	XtSetArg (args[0], XtNlabel, text);
	XtSetValues (sw_textline, args, 1);
//
// Scrollbar too.
//
	len = (float) bytes / sw_bytes;
	XawScrollbarSetThumb (sw_scroll, 0.0, len);
	return (sw_abort);
}





void
AbortButton (Widget w, XtPointer sw, XtPointer junk)
//
// The abort button has been hit.
//
{
	((StatusWindow *) sw)->setAbort (True);
}
