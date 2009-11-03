//
// The platform and file examiner widgets.
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
	extern void ds_ForceRescan (int, int);
}
//# include "container.h"
# include "dsPlatform.h"
# include "dsFile.h"
# include "dsmWindows.h"
# include "plcontainer.h"

static char *rcsid = "$Id: Examiner.cc,v 1.5 1999-03-01 02:03:52 burghart Exp $";

//
// Local forwards.
//
void PBCallback (Widget, XtPointer, XtPointer);
void FileDelToggle (Widget, XtPointer, XtPointer);
void NewFileExaminer (dsPlatform &);
void dsFEMakeButtons (Widget, dsPlatform &, char *);
void FEExecuteZap (Widget, XtPointer, XtPointer);
void GetRidOfFile (dsPlatform &, dsFile &, int);
void TimeBasedMode (Widget, XtPointer, XtPointer);

extern void PEMakePLabel (char *, const dsPlatform&);
extern void FEMakeFLabel (char *, const dsFile &);
//
// XXX hook into platform list
//
//extern IContainer<dsPlatform> *PList;

extern plContainer *PList;

//---------------------------------------------
//
// The platform examiner window.
//

class dsPEWindow : public dsPopupWindow
{
	Widget *pbuttons;		// Platform buttons
public:
	dsPEWindow (const dsDisplay &);
	void fixPlat (const dsPlatform &);	// Update platform.
};

static dsPEWindow *Examiner = 0;




dsPEWindow::dsPEWindow (const dsDisplay &disp) :
	dsPopupWindow (disp, "Platform and file maintenance", 250)
//
// Create a platform examiner window.
//
{
	Arg args[10];
	int n, plat;
	Widget left, above = corner, vp, vpform;
//
// Provide the time-based option.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Time-based file cleanup"); n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	above = XtCreateManagedWidget ("tbase", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (above, XtNcallback, TimeBasedMode, (XtPointer) this);
//
// Make the title label for the platform list.
//
	n = 0;
	XtSetArg (args[n], XtNlabel,
	"Plat. name     Begin time            End time           #files  MB");
	      							n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNhorizDistance, 25);		n++;
	above = XtCreateManagedWidget ("ptitle", labelWidgetClass, dw_form,
			args, n);
//
// Now we create the viewport to hold the list of platforms.
//
	n = 0;
	XtSetArg (args[n], XtNallowVert, True);			n++;
	XtSetArg (args[n], XtNheight, 200);			n++;
	XtSetArg (args[n], XtNwidth, 520);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	vp = XtCreateManagedWidget ("pvp", viewportWidgetClass, dw_form,
			args, n);
	vpform = XtCreateManagedWidget ("pform", formWidgetClass, vp, 0, 0);
//
// Add a bunch of platform buttons.
//
	above = NULL;
	pbuttons = new Widget[PList->ncontained ()];
	for (plat = 0; plat < PList->ncontained (); plat++)
	{
		char lbuf[120];
		dsPlatform& p = PList->nth (plat);
		PEMakePLabel (lbuf, p);
		n = 0;
		XtSetArg (args[n], XtNlabel, lbuf);		n++;
		XtSetArg (args[n], XtNfromVert, above);		n++;
		XtSetArg (args[n], XtNwidth, 490);		n++;
		XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
		XtSetArg (args[n], XtNborderWidth, 0);		n++;
		above = pbuttons[plat] = XtCreateManagedWidget (p.name(), 
				commandWidgetClass, vpform, args, n);
		XtAddCallback (above, XtNcallback, PBCallback,
				(XtPointer) plat);
	}
//
// Put it up on the screen.
//
	XtRealizeWidget (dw_shell);
}



void
dsPEWindow::fixPlat (const dsPlatform &p)
//
// Update the label for this window.
//
{
	char lbuf[120];
	Arg args[2];
	int position = PList->pos (p.index);

	PEMakePLabel (lbuf, p);
	XtSetArg (args[0], XtNlabel, lbuf);
	XtSetValues (pbuttons[position], args, 1);
}




void
PBCallback (Widget w, XtPointer xplat, XtPointer junk)
//
// They have clicked on a platform button.
//
{
	int plat = (int) xplat;

	NewFileExaminer (PList->nth (plat));
}




void
PExamine (Widget w, XtPointer junk1, XtPointer junk2)
//
// Put up a platform examiner window.
//
{
//
// If we have no window yet, make it.
//
	if (! Examiner)
		Examiner = new dsPEWindow (*Disp);
//
// Put it up on the screen.
//
	Examiner->popup ();
}




//------------------------------------------
//
// The file examiner window.
//
// There can be several of these going at once.
//

class dsFileExaminer : public dsPopupWindow
{
	friend FEExecuteZap ();
	Widget topLabel;	// The top label
	Widget vp, vpform;	// The viewport and its form.
	dsPlatform *plat;	// The platform of interest.
	char *delflag;		// Per-file delete flags
public:
	dsFileExaminer (const dsDisplay &, dsPlatform &);
	void popdown ();
	void DoZap ();		// Execute a deletion
};





dsFileExaminer::dsFileExaminer (const dsDisplay &disp, dsPlatform &p) :
	dsPopupWindow (disp, "File maintenance", 430)
//
// Create the file examiner window.
//
{
	Arg args[10];
	int n;
	Widget left, above = corner;
	char cbuf[80];
//
// Fill in some fields.
//
	plat = &p;
	delflag = new char[p.files.ncontained ()];
	memset (delflag, 0, p.files.ncontained ());
//
// The top label.
//
	sprintf (cbuf, "Platform %s uses %6.2f MB in %d files", p.name (),
		p.space (), p.files.ncontained ());
	n = 0;
	XtSetArg (args[n], XtNlabel, cbuf);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	above = topLabel = XtCreateManagedWidget ("toplabel", 
			labelWidgetClass, dw_form, args, n);
//
// The header to the file list.
//
	n = 0;
	XtSetArg (args[n], XtNlabel,
	"File name             Begin time            End time           MB");
	      							n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNhorizDistance, 150);		n++;
	above = XtCreateManagedWidget ("ftitle", labelWidgetClass, dw_form,
			args, n);
//
// Now we create the viewport to hold the list of files.
//
	n = 0;
	XtSetArg (args[n], XtNallowVert, True);			n++;
	XtSetArg (args[n], XtNheight, 200);			n++;
	XtSetArg (args[n], XtNwidth, 630);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	vp = XtCreateManagedWidget ("pvp", viewportWidgetClass, dw_form,
			args, n);
	vpform = XtCreateManagedWidget ("pform", formWidgetClass, vp, 0, 0);
//
// Do the file buttons.
//
	dsFEMakeButtons (vpform, p, delflag);
//
// Underneath all that, we do the execute button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Execute deletions (!)");	n++;
	XtSetArg (args[n], XtNfromVert, vp);			n++;
	above = XtCreateManagedWidget ("execute", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (above, XtNcallback, FEExecuteZap, (XtPointer) this);
}





void
dsFEMakeButtons (Widget form, dsPlatform &p, char *delflag)
//
// Make a set of file buttons in this form.
//
{
	Arg args[10];
	int n, nfile;
	Widget above = NULL, left;
//
// Go through and add them.
//
	for (nfile = 0; nfile < p.files.ncontained (); nfile++)
	{
		char lbuf[120];
	//
	// Pull out the file entry, and make the label for it.
	//
		const dsFile& f = p.files.nth (nfile);
		FEMakeFLabel (lbuf, f);
	//
	// Make the delete button.
	//
		n = 0;
		XtSetArg (args[n], XtNlabel, "Delete");		n++;
		XtSetArg (args[n], XtNfromVert, above);		n++;
		AddConstraints (args, &n);
		left = XtCreateManagedWidget ("delete", toggleWidgetClass,
			form, args, n);
		XtAddCallback (left, XtNcallback, FileDelToggle,
				delflag + nfile);
	//
	// Add the file label to the right.
	//
		n = 0;
		XtSetArg (args[n], XtNlabel, lbuf);		n++;
		XtSetArg (args[n], XtNfromVert, above);		n++;
		XtSetArg (args[n], XtNfromHoriz, left);		n++;
		XtSetArg (args[n], XtNwidth, 570);		n++;
		XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
		XtSetArg (args[n], XtNborderWidth, 0);		n++;
		AddConstraints (args, &n);
		(void) XtCreateManagedWidget ("flabel", labelWidgetClass,
				form, args, n);
		above = left;
	}
}





void
FileDelToggle (Widget w, XtPointer xdel, XtPointer junk)
//
// Toggle the delete flag.
//
{
	char *del = (char *) xdel;

	*del = ! *del;
}






void
dsFileExaminer::popdown ()
//
// Take it off the screen by demolishing it.
//
{
	delete[] this->delflag;
	delete this;
}





void
NewFileExaminer (dsPlatform &p)
//
// Make a new file examiner.
//
{
	dsFileExaminer *examiner = new dsFileExaminer (*Disp, p);
	
	examiner->popup ();
}





void FEExecuteZap (Widget w, XtPointer xfe, XtPointer junk)
//
// Get rid of the files they don't want.
//
{
	dsFileExaminer *fe = (dsFileExaminer *) xfe;

	fe->DoZap ();
}



void
dsFileExaminer::DoZap ()
//
// Clean up marked files.
//
{
	int file, nzapped = 0, nb, button;
	char cbuf[80];
	Arg args[2];
	WidgetList buttons;
//
// Go through the delete flags and see which ones should be gotten
// rid of.  We go from top to bottom because otherwise the index
// will change and we zap things we shouldn't ought to zap.
//
	for (file = plat->files.ncontained () - 1; file >= 0; file--)
		if (delflag[file])
		{
			dsFile &df = plat->files.nth (file);
			nzapped++;
			GetRidOfFile (*plat, df, file);
			cout << "Delete file " << df.name() << ".\n";
		}
//
// If there was nothing to delete, no more work to do now.
//
	if (nzapped == 0)
		return;
//
// Get the daemon updating, and do the main window too.
//
	ds_ForceRescan (plat->index, FALSE);
	Main->UpdateSpace ();
//
// Otherwise we need to fix up the examiner to match the new state
// of reality.  Start by making a new space label.
//
	sprintf (cbuf, "Platform %s uses %6.2f MB in %d files", plat->name (),
		plat->space (), plat->files.ncontained ());
	XtSetArg (args[0], XtNlabel, cbuf);
	XtSetValues (topLabel, args, 1);
//
// Clear out the buttons and start over.
//
	XtSetArg (args[0], XtNnumChildren, &nb);
	XtSetArg (args[1], XtNchildren, &buttons);
	XtGetValues (vpform, args, 2);
	for (button = 0; button < nb; button++)
		XtDestroyWidget (buttons[button]);
	memset (delflag, 0, plat->files.ncontained ());
	dsFEMakeButtons (vpform, *plat, delflag);
//
// Get the platform widget updated.
//
	Examiner->fixPlat (*plat);
}





void
GetRidOfFile (dsPlatform &plat, dsFile &file, int ind)
//
// Make this file go away and deal with the consequences thereof.
//
{
//
// Physically get rid of the file
//
	if (unlink (file.name ()))
		perror (file.name ());
//
// Clean this file out of the platform structure and get rid of it.
//
	plat.files.zap (ind);
}







void
TimeBasedMode (Widget w, XtPointer xfe, XtPointer x2)
//
// Go into the time based mode of deletion.
//
{
	dsPEWindow *ex = (dsPEWindow *) xfe;
	extern void DoTBDelete ();

	cout << "Go into TB mode\n";
	ex->popdown ();
	
	DoTBDelete ();
}
