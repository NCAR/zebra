//
// Basic window classes.
//
# include <stdio.h>
# include <stream.h>
# include <memory.h>
# include <unistd.h>
# include "dsmanage.h"
# include "container.h"
# include "DataDir.h"


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
//
// Forwards.
//

extern "C" void exit (int);





dsDisplay::dsDisplay (int *argc, char **argv)
//
// Open up the display.
//
{
	dd_Top = XtAppInitialize (&dd_Appc, "dsmanage", NULL, 0, argc, argv,
			NULL, NULL, 0);
}





dsWindow::dsWindow (char *name, const dsDisplay &disp, int top = 0)
//
// Create a base window.
//
{
//
// Create the shell, and go ahead and throw a form in as well.
//
	if (top)
		dw_shell = disp.dd_Top;
	else
		dw_shell = XtCreatePopupShell (name, topLevelShellWidgetClass,
				disp.dd_Top, NULL, 0);
	dw_form = XtCreateManagedWidget ("form", formWidgetClass, dw_shell,
			NULL, 0);
}



dsWindow::~dsWindow ()
//
// Destroy it.
//
{
	XtDestroyWidget (dw_shell);
}



void
dsWindow::popup ()
//
// Put this window on the screen; 
//
{
	XtPopup (dw_shell, XtGrabNone);
}



void
dsWindow::popdown ()
//
// Take it back off.
//
{
	XtPopdown (dw_shell);
}

static void ZapPopup (Widget, XtPointer, XtPointer);

//
// PopupWindow methods.
//


dsPopupWindow::dsPopupWindow (const dsDisplay &disp, char *title,
				int zapspace = 50) :
	dsWindow (title, disp, 0)
//
// Create a popup window.
//
{
	Arg args[10];
	int n;
	Widget zap;
//
// Add our label to the top.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, title);	n++;
	AddConstraints (args, &n);
	corner = XtCreateManagedWidget ("title", labelWidgetClass, 
			dw_form, args, n);
//
// The zap button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Zap");		n++;
	XtSetArg (args[n], XtNfromHoriz, corner);	n++;
	XtSetArg (args[n], XtNhorizDistance, zapspace);	n++;
	AddConstraints (args, &n);
	zap = XtCreateManagedWidget ("quit", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (zap, XtNcallback, ZapPopup, (XtPointer) this);
}




void 
dsPopupWindow::SetTitle (const char *title)
//
// Change the title.
//
{
	Arg args[1];

	XtSetArg (args[0], XtNlabel, title);
	XtSetValues (corner, args, 1);
}






static void
ZapPopup (Widget w, XtPointer win, XtPointer junk)
//
// Try to zap the examiner this way.
//
{
	dsPopupWindow *pwin = (dsPopupWindow *) win;

	pwin->popdown ();
}



//
// Now that we have all of our classes defined, here are some routines
// to use them.
//

//
// First a few globals.
//
dsDisplay *Disp;	// The display structure.
dsMainWindow *Main;	// The main window



void
DisplaySetup (int *argc, char **argv)
//
// Get the display started.
//
{
//
// Get hooked into the server.
//
	Disp = new dsDisplay (argc, argv);
//
// Make the main window.
//
	Main = new dsMainWindow (*Disp);
}





void
Run ()
//
// Actually make things go.
//
{
	Disp->run ();
}





void
Quit (Widget w, XtPointer junk1, XtPointer junk2)
//
// Bail out.
//
{
	exit (0);
}







