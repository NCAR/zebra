//
// The main program window.
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

# ifdef hpux
# include <sys/sigevent.h>
# endif
# include <stdio.h>
# include <iostream>
# include <stdlib.h>
# include <DataStore.h>

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
# include "dsmanage.h"
# include "dsmWindows.h"
# include "Dialog.h"

static char *rcsid = "$Id: MainWindow.cc,v 1.12 2002-12-18 00:24:13 granger Exp $";
//
// Externs.
//
extern void DoTBDelete (Widget, XtPointer, XtPointer);
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
	XtSetArg (args[n], XtNlabel, "Zebra Data Store Manager");	n++;
	above = XtCreateManagedWidget ("title", labelWidgetClass, dw_form,
		args, n);
//
// The free space line.
//
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	spaceLabel = XtCreateManagedWidget ("spaceLabel", labelWidgetClass,
		dw_form, args, n);

	UpdateSpace();
//
// New line
//
	left = NULL;
	above = spaceLabel;
//
// Look at platforms and files.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Disk space cleanup"); n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("pandfile", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, DoTBDelete, 0);
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
	if (getenv ("DSMANAGE_GURU"))
	{
		n = 0;
		XtSetArg (args[n], XtNlabel, "Generate file index");	n++;
		XtSetArg (args[n], XtNfromVert, above);			n++;
		XtSetArg (args[n], XtNfromHoriz, left);			n++;
		AddConstraints (args, &n);
		left = XtCreateManagedWidget ("index", commandWidgetClass,
				dw_form, args, n);
		XtAddCallback (left, XtNcallback, DoIndex, 0);
	}
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



dsMainWindow::~dsMainWindow () 
{ /* yawn */ }



void
dsMainWindow::UpdateSpace ()
//
// Tweak the "space available" line.
//
{
	char label[200];
	float space;
	SourceInfo dsi;
	Arg args[1];
	extern unsigned int FreeSpace (const char *dirname);

	ds_GetSourceInfo (SrcId, &dsi);

	space = FreeSpace (dsi.src_Name) / (float)(1024 * 1024);
	sprintf (label, "Source '%s': %.2f MB free in %s.", dsi.src_Name, 
		 space, dsi.src_Dir);
	
	XtSetArg (args[0], XtNlabel, label);
	XtSetValues (spaceLabel, args, 1);
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
	std::cout << "Make index in " << file << ".\n";
	MakeLocalIndex (file);
}
