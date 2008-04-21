//
// Simple dialog widgets.
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

# include "dsmanage.h"

extern "C" 
{
#	include <X11/Intrinsic.h>
#	include <X11/Shell.h>
#	include <X11/StringDefs.h>
#	include <X11/Xaw/AsciiText.h>
#	include <X11/Xaw/Command.h>
#	include <X11/Xaw/Form.h>
#	include <X11/Xaw/Label.h>
#	include <X11/Xaw/Toggle.h>
#	include <X11/Xaw/Viewport.h>
}

# include "dsmWindows.h"
# include "Dialog.h"

static char *rcsid = "$Id: Dialog.cc,v 1.7 1999-03-01 02:03:51 burghart Exp $";

dsDialog::dsDialog (char *title, char *prompt, void (*goproc) (const char *),
			void (*cancelproc) ()) :
	dsPopupWindow (*Disp, title, 100)
//
// Create the dialog.
//
{
	Arg args[10];
	int n;
	Widget above = corner, left = NULL;
//
// Add the prompt.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, prompt);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	AddConstraints (args, &n);
	above = XtCreateManagedWidget ("prompt", labelWidgetClass, dw_form,
			args, n);
//
// Then the dialog box.
//
	n = 0;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);		n++;
	AddConstraints (args, &n);
	text = above = XtCreateManagedWidget ("dtext", asciiTextWidgetClass,
			dw_form, args, n);
//
// Go button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "OK");			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("go", commandWidgetClass, dw_form,
		args, n);
	XtAddCallback (left, XtNcallback, DialogCb, (XtPointer) this);
//
// Cancel button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Cancel");			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	wcancel = left = XtCreateManagedWidget ("cancel", commandWidgetClass,
		dw_form, args, n);
	XtAddCallback (left, XtNcallback, DialogCb, (XtPointer) this);
//
// Remember the callbacks and we're done.
//
	gocb = goproc;
	cancelcb = cancelproc;
}



dsDialog::~dsDialog ()
{ /* SGI gcc bug food */ }




void
DialogCb (Widget w, XtPointer xdialog, XtPointer junk)
//
// One of the buttons has been pushed.
//
{
	dsDialog *dialog = (dsDialog *) xdialog;
//
// See if it was a cancel or not.
//
	if (w == dialog->wcancel)
		dialog->cancel ();
	else
		dialog->ok ();
}





void
dsDialog::cancel ()
//
// The cancel button has been hit.
//
{
	if (cancelcb)
		(*cancelcb) ();
	popdown ();
}





void
dsDialog::ok ()
//
// The OK button has been hit.
//
{
	Arg args[2];
	char *string;
//
// Find out what is in the text widget.
//
	XtSetArg (args[0], XtNstring, &string);
	XtGetValues (text, args, 1);
//
// Pass it off to the OK callback and we are done.
//
	(*gocb) (string);
	popdown ();
}

