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




dsDialog::dsDialog (char *title, char *prompt, void (*goproc) (char *),
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

