
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
# include <stdio.h>
# include <stream.h>
# include "dsmanage.h"
# include "dsmWindows.h"
# include "Dialog.h"

//
// Externs.
//
extern void PExamine (Widget, XtPointer, XtPointer);
extern void LoadData (Widget, XtPointer, XtPointer);
extern void Quit (Widget, XtPointer, XtPointer);
void DoIndex (Widget, XtPointer, XtPointer);
void ReallyDoIndex (const char *);



dsMainWindow::dsMainWindow (const dsDisplay &disp) :
	dsWindow ("Zeb Disk Manager", disp, 1)
//
// Make the window.
//
{
	Arg args[10];
	int n;
	Widget left, above;
//
// Add our label to the top.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "The zeb disk manager");	n++;
	above = XtCreateManagedWidget ("title", labelWidgetClass, dw_form,
		args, n);
//
// The free space line.
//
	char label[200];
	const char *dir;
	float space;
	DDInfo (&dir, &space);
	sprintf (label, "%.2f MB free in %s.", space, dir);
	n = 0;
	XtSetArg (args[n], XtNlabel, label);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	spaceLabel = XtCreateManagedWidget ("spaceLabel", labelWidgetClass,
		dw_form, args, n);
//
// New line
//
	left = NULL;
	above = spaceLabel;
//
// Look at platforms and files.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Platform and file maint."); n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("pandfile", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, PExamine, 0);
//
// Load data.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Load data");		n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("load", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, LoadData, 0);
//
// Do indexing.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Generate file index");	n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("index", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, DoIndex, 0);
//
// A quit button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Quit");			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("quit", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, Quit, 0);
//
// Put it up.
//
	XtRealizeWidget (dw_shell);
}









//-----------------------------------------------------------
//
// Generate an index.
//
void DoIndex (Widget w, XtPointer junk1, XtPointer junk2)
//
// Create an index.
//
{
	dsDialog *dialog = new dsDialog ("Index generation",
			"Enter index file name:", ReallyDoIndex, NULL);
	dialog->popup ();
}




void ReallyDoIndex (const char *file)
//
// Create the index in the given file.
//
{
	cout << "Make index in " << file << ".\n";
	MakeLocalIndex (file);
}
