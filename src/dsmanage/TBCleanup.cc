//
// Clean up data in a time-based mode.
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
# ifdef sgi
# define _BSD_TYPES
# include <sys/types.h>
# undef _BSD_TYPES
# endif

# include <stdio.h>
# include <iostream>
# include <unistd.h>
# include "dsmanage.h"
# include <defs.h>
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
	extern int uit_parse_date (const char *, SValue *, int); // XXX XXX
	extern int atoi (const char *);
	extern void ds_ForceRescan (int, int);
}
//# include "container.h"
# include "dsPlatform.h"
# include "STable.h"
# include "dsmWindows.h"
# include "Index.h"
# include "ZTime.h"
# include "plcontainer.h"
MAKE_RCSID ("$Id: TBCleanup.cc,v 1.12 2002-12-18 00:24:13 granger Exp $")

using std::cerr;
using std::cout;
using std::endl;

class DelSelect;

static void PerformZap (Widget, XtPointer, XtPointer);
static void MarkTimeFiles (DelSelect *ls, PlatformIndex *select,
		const ZebTime &tbegin, const ZebTime &tend, int skip = 0);
static void SetToggles (Widget, XtPointer, XtPointer);
static void ClearToggles (Widget, XtPointer, XtPointer);
static void ExecTimeSelect (Widget, XtPointer, XtPointer);
static void PlatformSel (Widget, XtPointer, XtPointer);
static void MakeFileLabel (const IndexFile *, char *);
static void GetVictimChooser (Widget, XtPointer, XtPointer);
static void SelFile (Widget, XtPointer, XtPointer);
static void MarkAllFiles (Widget, XtPointer, XtPointer);
static void UnmarkAllFiles (Widget, XtPointer, XtPointer);
static void ClearAll (Widget, XtPointer, XtPointer);
static void SyncChoosers ();
static void ZapChoosers ();




//
// XXX hook into platform list
//
//extern IContainer<dsPlatform> *PList;
extern plContainer *PList;






//
// The DelSelect widget -- presents a list of what can be zapped
//

class DelSelect : public dsPopupWindow
{
	Widget startTime;		// Begin time
	Widget endTime;
	PlatformIndex *index;		// Index we are working from
	Widget	*toggles;		// All our toggles.
	int ntoggle;			// How many there are.
	Widget gripe, sgripe;		// Complaint windows.
	const char *getstring (Widget) const;// Extract val from string widget.
	int nfile, nbyte;		// Amount selected.
	Widget fsummary;		// File select summary
	void addButtons (Widget, PlatformIndex *);
	void UpdFSummary ();
public:
	DelSelect (PlatformIndex *);
	~DelSelect ();
//
// Widget tweaking methods.
//
	void popdown () { delete this; };
	void complain (const char *, int = 0);	// Put in a complaint.
//
// Extract user-specified stuff
//
	const char *begin () const { return (getstring (startTime)); }
	const char *end () const { return (getstring (endTime)); }
	int skip () const;
	PlatformIndex *selected () const;	// Which plats selected
	PlatformIndex *pindex () const { return index; }
//	const char *tfile () const { return dir; }
	int file_sel () const { return nfile; }
	int byte_sel () const { return nbyte; }
//
// File accounting.
//
	void MarkFile (IndexFile *file, const int state);
	void MarkPlat (const char *plat, const int state);
};





DelSelect::DelSelect (PlatformIndex *ind) :
	dsPopupWindow (*Disp, "Select files for deletion", 545)
//
// Create the "delete select" dialog.
//
{
	Arg args[15];
	int n;
	const int labelwidth = 150;
	Widget above = corner, left = NULL, vp, vpform, selform;
//
// Save our info first.
//
	index = ind;
	nfile = nbyte = 0;
//
// The time-based select goes inside its own form, so we get a nice border.
//
	n = 0;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	AddConstraints (args, &n);
	selform = XtCreateManagedWidget ("selform", formWidgetClass, dw_form,
			args, n);
//
// Title of sorts.
//
	above = NULL;
	n = 0;
	XtSetArg (args[n], XtNlabel,
		"File selection by time: (format: dd-mmm-yy,hh:mm:ss)"); n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	above = XtCreateManagedWidget ("subtitle", labelWidgetClass, selform,
			args, n);
//
// A simple label for starters.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Select files between");	n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNhorizDistance, 20);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNwidth, labelwidth);		n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	XtSetArg (args[n], XtNjustify, XtJustifyRight);		n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("prompt", labelWidgetClass, selform,
			args, n);
//
// Blank for the begin time (default?)
//
	n = 0;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);		n++;
	XtSetArg (args[n], XtNwidth, 150);			n++;
	AddConstraints (args, &n);
	startTime = left = XtCreateManagedWidget ("btime",asciiTextWidgetClass,
			selform, args, n);
//
// more label
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "and");			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("prompt", labelWidgetClass, selform,
			args, n);
//
// End time.
//
	n = 0;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);		n++;
	XtSetArg (args[n], XtNwidth, 150);			n++;
	AddConstraints (args, &n);
	endTime = above = XtCreateManagedWidget("etime", asciiTextWidgetClass,
			selform, args, n);
//
// The execute button.
//
	n = 0;
	left = NULL;
	above = startTime;
	XtSetArg (args[n], XtNlabel, "Apply to selected platforms");	n++;
	AddConstraints (args, &n);
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNhorizDistance, 20);		n++;
	left = XtCreateManagedWidget ("selexec", commandWidgetClass, selform,
			args, n);
	XtAddCallback (left, XtNcallback, ExecTimeSelect, (XtPointer) this);
//
// And a gripe window.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, " ");			n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNwidth, 300);			n++;
	AddConstraints (args, &n);
	sgripe = XtCreateManagedWidget ("gripe", labelWidgetClass, selform,
			args, n);
//
// Put the summary window to the right of the time selection.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "100 files selected\n(300.000 MB)"); n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNfromHoriz, selform);		n++;
	XtSetArg (args[n], XtNfromVert, corner);		n++;
	XtSetArg (args[n], XtNhorizDistance, 10);		n++;
	XtSetArg (args[n], XtNwidth, 200);			n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	fsummary = XtCreateManagedWidget ("fsummary", labelWidgetClass,
			dw_form, args, n);
//
// The "clear all selections" button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Clear all file selections");	n++;
	XtSetArg (args[n], XtNfromHoriz, selform);			n++;
	XtSetArg (args[n], XtNfromVert, fsummary);			n++;
	XtSetArg (args[n], XtNhorizDistance, 20);			n++;
	XtSetArg (args[n], XtNvertDistance, 15);			n++;
	AddConstraints (args, &n);
	above = XtCreateManagedWidget ("clearall", commandWidgetClass,
			dw_form, args, n);
	XtAddCallback (above, XtNcallback, ClearAll, this);
//
// A title for the platform list.
//
	n = 0;
	above = selform;
	XtSetArg (args[n], XtNlabel,
"Platform                   Data present                 # Files   MB"); n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNhorizDistance, 80);		n++;
	XtSetArg (args[n], XtNvertDistance, 20);		n++;
	AddConstraints (args, &n);
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	above = XtCreateManagedWidget ("ltitle", labelWidgetClass, dw_form,
			args, n);
//
// Now we create the viewport to hold the list of platforms.
//
	n = 0;
	XtSetArg (args[n], XtNallowVert, True);			n++;
	XtSetArg (args[n], XtNheight, 200);			n++;
	XtSetArg (args[n], XtNwidth, 760);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNleft, XtChainLeft);		n++;
	XtSetArg (args[n], XtNright, XtChainRight);		n++;
	XtSetArg (args[n], XtNtop, XtChainTop);			n++;
	XtSetArg (args[n], XtNbottom, XtChainBottom);		n++;
	vp = XtCreateManagedWidget ("lvp", viewportWidgetClass, dw_form,
			args, n);
	vpform = XtCreateManagedWidget ("lform", formWidgetClass, vp, 0, 0);
//
// Time to add a whole bunch of buttons.
//
	addButtons (vpform, index);
//
// Select all.
//
	above = vp;
	left = NULL;
	n = 0;
	XtSetArg (args[n], XtNlabel, "Select all");		n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("selall", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, ::SetToggles, (XtPointer) this);
//
// Unselect all.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Unselect all");		n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("unselall", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, ClearToggles, (XtPointer) this);
//
// Even a "go" button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Perform deletion");	n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNhorizDistance, 75);		n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("delete", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, PerformZap, (XtPointer) this);
//
// And, of course, a gripe window.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, " ");			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNwidth, 400);			n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	AddConstraints (args, &n);
	gripe = XtCreateManagedWidget ("gripe", labelWidgetClass, dw_form,
			args, n);
//
// Just about done, at last.
//
	UpdFSummary ();
}


DelSelect::~DelSelect ()
{
	ZapChoosers (); 
};





void
DelSelect::UpdFSummary ()
//
// Update the file use summary.
//
{
	char line[120];
	Arg args[2];

	sprintf (line, "%d files selected\n(%.3f MB)", nfile, nbyte/1048576.0);
	XtSetArg (args[0], XtNlabel, line);
	XtSetValues (fsummary, args, 1);
}






void
DelSelect::addButtons (Widget form, PlatformIndex *index)
//
// Add all the platform buttons.
//
{
	char ebuf[180], *ep;
	Widget toggle, above = NULL, label, cw;
	int plat, n;
	Arg args[15];
	extern void PEMakePLabel (char *, const dsPlatform &);
//
// Allocate space to hold the toggles.
//
	ntoggle = PList->ncontained ();	// One per platform
	toggles = new Widget[ntoggle];
//
// Go through the list.
//
	for (plat = 0; plat < PList->ncontained (); plat++)
	{
		ZebTime bt, et;
		const dsPlatform &p = PList->nth (plat);
	//
	// Add a toggle for this platform.
	//
		n = 0;
		XtSetArg (args[n], XtNlabel, "Select");			n++;
		XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
		XtSetArg (args[n], XtNfromVert, above);			n++;
		XtSetArg (args[n], XtNstate, True);			n++;
		XtSetArg (args[n], XtNradioData, p.name ());		n++;
		AddConstraints (args, &n);
		toggle = XtCreateManagedWidget ("select", toggleWidgetClass,
				form, args, n);
		XtAddCallback (toggle, XtNcallback, PlatformSel, this);
		index->isMarked (p.name ()) = True;
	//
	// Put together the text for the label.
	//
# ifdef notdef
		sprintf (ebuf, "%-12s", p.name ());
		ep = ebuf + 12;
	//
	// Local data.  We should properly be doing this from the index,
	// 	        but for now I am trying to make this thing work
	//		in a hurry, and I ain't gonna do it.
	//
		if (p.files.ncontained () == 0)
		{
			strcpy (ep,
			  "               (none)                       ");
			ep += strlen (ep);
		}
		else
		{
			int dfi = p.files.nth (p.files.ncontained() - 1).index;
			TC_EncodeTime (&DFTable[dfi].df_begin, TC_Full, ep);
			strcat (ep, "   ");
			ep += 18;
			*ep = '\0';
			strcat (ep, " -> ");
			ep += strlen (ep);
			dfi = p.files.nth (0).index;
			TC_EncodeTime (&DFTable[dfi].df_end, TC_Full, ep);
			strcat (ep, "     ");
			ep += 22;
		}
# endif
		PEMakePLabel (ebuf, p);
	//
	// Now add the label with all this info.  KLUDGE: this thing is
	// done as a toggle since they consent to store radioData for us,
	// the the resources are set such that it looks like a command
	// widget.
	//
		n = 0;
		XtSetArg (args[n], XtNlabel, ebuf);			n++;
		XtSetArg (args[n], XtNfromHoriz, toggle);		n++;
		XtSetArg (args[n], XtNfromVert, above);			n++;
		AddConstraints (args, &n);
		XtSetArg (args[n], XtNborderWidth, 0);			n++;
		XtSetArg (args[n], XtNwidth, 690);			n++;
		XtSetArg (args[n], XtNjustify, XtJustifyLeft);		n++;
		XtSetArg (args[n], XtNresize, False);			n++;
		XtSetArg (args[n], XtNradioData, p.name ());		n++;
		cw = XtCreateManagedWidget ("FCToggle", toggleWidgetClass,
				form, args, n);
		XtAddCallback (cw, XtNcallback, GetVictimChooser, this);
		toggles[plat] = above = toggle;
	}
}







void DelSelect::complain (const char *complaint, int self)
//
// Register a complaint.
//
{
	Arg args[2];

	XtSetArg (args[0], XtNlabel, complaint);
	XtSetValues (self ? sgripe : gripe, args, 1);
}




const char *
DelSelect::getstring (Widget w) const
//
// Get a string out of a widget.
//
{
	Arg args[2];
	const char *ret;

	XtSetArg (args[0], XtNstring, &ret);
	XtGetValues (w, args, 1);
	return (ret);
}







void
DelSelect::MarkPlat (const char *platform, const int state)
//
// Change the marking on this platform to the given state.
//
{
	int toggle;
//
// If we already have this platform in this state, there
// is nothing to do.
//
	if (index->isMarked (platform) == state && index->files (platform))
		return;
	index->isMarked (platform) = state;
//
// OK, it has changed.  We need to find the toggle which corresponds
// to this platform.
//
	for (toggle = 0; toggle < PList->ncontained (); toggle++)
		if (! strcmp (platform, PList->nth (toggle).name ()))
			break;
	if (toggle >= PList->ncontained ())    // "Should never happen"
	{
		cerr << "Platform " << platform << " disappeared!\n";
		return;
	}
//
// set the toggle.  We do this because platform states can be set
// outside of the toggles.
//
	Arg args[1];
	XtSetArg (args[0], XtNstate, state);
	XtSetValues (toggles[toggle], args, 1);
//
// Compute the resources used by marked files, and change the total 
// accordingly.
//
	int pnf = 0, pbytes = 0;
	for (IndexFile *file = index->files (platform); file;
							file = file->next ())
	{
		if (! file->isMarked ())
			continue;
	//
	// If any duplicate entries for this file are already selected, 
	// then we don't need to adjust the accounting.  Otherwise, this is
	// the only file in the chain of entries whose selection state 
	// changes, so the accounting needs to change.	
	//
		IndexFile *chain = file->same();
		for ( ; chain && (chain != file); chain = chain->same())
		{
			if (index->isMarked (chain->plat()) &&
			    chain->isMarked())
				break;
		}
		if (! chain || (chain == file))
		{
			pnf++;
			pbytes += file->size ();
		}
	}
	nfile += (state ? pnf : -pnf);
	nbyte += (state ? pbytes : -pbytes);
	UpdFSummary ();
}






void
DelSelect::MarkFile (IndexFile *file, const int state)
//
// Set the marking on this file.
//
{
//
// Make sure the state has changed.  If it has not, nothing to do.
//
	if (state == file->isMarked ())
		return;
//
// Actually mark the file and all other entries for the same file.
// Note whether any files were already selected (i.e., in the count)
//
	int selected = file->isMarked() && index->isMarked(file->plat());
	file->isMarked () = state;
	int newmark = file->isMarked() && index->isMarked(file->plat());
	IndexFile *chain = file->same();
	for ( ; chain && (chain != file); chain = chain->same())
	{
		selected |= (index->isMarked(chain->plat()) &&
			     chain->isMarked());
		chain->isMarked () = state;
		newmark |= (index->isMarked(chain->plat()) &&
			    chain->isMarked());
	}

	//
	// If selecting any file where none were selected before, 
	// add to the count.  Else if unselecting where at least one
	// (including this file) was selected before, subtract.
	//
	if (state && newmark && !selected)
	{
		nfile++;
		nbyte += file->size ();
		UpdFSummary ();
	}
	else if (!state && selected)
	{
		nfile--;
		nbyte -= file->size ();
		UpdFSummary ();
	}
	// Now the caller should update any file chooser widgets!
}




PlatformIndex *
DelSelect::selected () const
//
// Return the index showing which platforms are selected.
//
{
	Arg args[2];
	int toggle, nplat = 0;
	Boolean state;
//
// What we do is we go through and query every toggle for its state.
//
	XtSetArg (args[0], XtNstate, &state);
	for (toggle = 0; toggle < ntoggle; toggle++)
	{
		XtGetValues (toggles[toggle], args, 1);
		if (state)
			nplat++;
		index->isMarked (PList->nth(toggle).name()) = state;
	}
//
// Check for no platforms selected.
//
	if (nplat == 0)
		return (NULL);
	return (index);
}





static void
ClearAll (Widget w, XtPointer xls, XtPointer junk)
//
// Clear all file selections.
//
{
	DelSelect *ls = (DelSelect *) xls;
	PlatformIndex *index = ls->pindex ();
	int plat;

	for (plat = 0; plat < PList->ncontained (); plat++)
	{
		dsPlatform &p = PList->nth (plat);

		IndexFile *file = index->files (p.name ());
		for (; file; file = file->next ())
			ls->MarkFile (file, False);
	}
	SyncChoosers ();
}





static void
PlatformSel (Widget w, XtPointer xls, XtPointer junk)
//
// They hit a platform select button.
//
{
	Arg args[2];
	Boolean state;
	DelSelect *ls = (DelSelect *) xls;
	char *plat;

	XtSetArg (args[0], XtNstate, &state);
	XtSetArg (args[1], XtNradioData, &plat);
	XtGetValues (w, args, 2);
	ls->MarkPlat (plat, state);
	SyncChoosers ();
}










static
void SetToggles (Widget w, XtPointer xls, XtPointer junk)
//
// Set all of the toggles.
//
{
	int plat;

	for (plat = 0; plat < PList->ncontained (); plat++)
		((DelSelect *) xls)->MarkPlat (PList->nth(plat).name(), True);
	SyncChoosers ();
}

static
void ClearToggles (Widget w, XtPointer xls, XtPointer junk)
//
// Clear all of the toggles.
//
{
	int plat;

	for (plat = 0; plat < PList->ncontained (); plat++)
		((DelSelect *) xls)->MarkPlat (PList->nth(plat).name(),False);
	SyncChoosers ();
}




static
void PerformZap (Widget w, XtPointer xls, XtPointer junk)
//
// Do the actual zap.
//
{
	DelSelect *ls = (DelSelect *) xls;
	PlatformIndex *index;
	int fsel = ls->file_sel ();
	int bsel = ls->byte_sel (), plat;
//
// Get the list of selected platforms.
//
	if ((index = ls->selected ()) == NULL)
	{
		ls->complain ("No platforms selected");
		return;
	}
//
// Make sure some files have been selected.
//
	if (fsel <= 0)
	{
		ls->complain ("You have not selected any files!");
		return;
	}
//
// Get rid of the select widget.
//
	ls->popdown ();
//
// Now we need to start zapping stuff.
//
	for (plat = 0; plat < PList->ncontained (); plat++)
	{
		const dsPlatform &p = PList->nth (plat);
		int ndel = 0;
	//
	// If this platform is not selected, we blow it off.
	//
		if (! index->isMarked (p.name ()))
			continue;
	//
	// Pass through the file list.
	//
		IndexFile *file;
		for (file = index->files (p.name ()); file;
						file = file->next ())
		{
		//
		// Only if it's marked.
		//
			if (! file->isMarked ())
				continue;
		//
		// Only if the same file has not alreay been removed.
		//
			IndexFile *chain = file->same();
			for ( ; chain && (chain != file); 
			     chain = chain->same())
			{
			//
			// Check for this platform among those already done
			//
				int done;
				for (done = 0; done < plat; ++done)
					if (strcmp ((PList->nth (done)).name(),
						    chain->plat()) == 0)
						break;
			//
			// If the file was removed here, don't remove it again
			//
				if (done < plat && chain->isMarked() && 
				    index->isMarked (chain->plat()))
				{
#ifdef DEBUG
					cout << p.name() << ": file " <<
						file->name() << " already "
						"removed\n";
#endif
					break;
				}
			}
			if (chain && (chain != file))
			{
				ndel++;	// we still want this plat scanned
				continue;
			}
		//
		// OK, nail it.
		//
		//	cout << "ZORCH " << file->name () << "\n";
			if (unlink (file->name ()))
				perror (file->name ());
			Main->UpdateSpace ();
			Disp->sync ();
			ndel++;
		}
		if (ndel)
			ds_ForceRescan (p.index, FALSE);
	}
//
// Wait a little bit for the daemon to finish rescanning, then 
// rescan ourselves.
//
	sleep (5);
	MakePlatformList ();
}











static
void ExecTimeSelect (Widget w, XtPointer xls, XtPointer junk)
//
// They have asked to select files based on time.
//
{
	DelSelect *ls = (DelSelect *) xls;
	const char *begin = ls->begin ();
	const char *end = ls->end ();
	ZebTime tbegin, tend;
	PlatformIndex *select;
	SValue v;

	ls->complain (" ", True);
//
// Try to comprehend the times.
//
	if (! uit_parse_date (begin, &v, 1))
	{
		ls->complain ("Unable to parse begin time", True);
		return;
	}
	TC_UIToZt (&v.us_v_date, &tbegin);
	if (! uit_parse_date (end, &v, 1))
	{
		ls->complain ("Unable to parse end time", True);
		return;
	}
	TC_UIToZt (&v.us_v_date, &tend);
//
// basic sanity check.
//
	if (tbegin >= tend)
	{
		ls->complain ("Begin time must be before end time!", True);
		return;
	}
//
// Get the index so we know what is selected now.
//
	if ((select = ls->selected ()) == NULL)
	{
		ls->complain ("No platforms selected!", True);
		return;
	}
//
// Go through and mark all of the files which meet the selection criteria.
//
	MarkTimeFiles (ls, select, tbegin, tend);
//
// We also have to make sure that any file selection widgets get updated
// properly.
//
	SyncChoosers ();
}





static void
MarkTimeFiles (DelSelect *ls, PlatformIndex *select,
		const ZebTime &tbegin, const ZebTime &tend, int skip)
//
// Mark up files which should be selected according to the criteria.
//
{
	int plat;
	IndexFile *file;
	ZebTime skiptime;

	for (plat = 0; plat < PList->ncontained (); plat++)
	{
		const dsPlatform &p = PList->nth (plat);
	//
	// If the entire platform has been disabled, we don't bother
	// with the files.
	//
		if (! select->isMarked (p.name ()))
			continue;
		skiptime = tbegin;
	//
	// Now we need to go through the file chain and check dates.
	//
		for (file = select->files (p.name ()); file; 
						file = file->next ())
			if (skiptime < file->end () && tend > file->begin ())
			{
				ls->MarkFile (file, True);
				skiptime.zt_Sec += skip;
				while (skip && skiptime < file->begin ())
					skiptime.zt_Sec += skip;
			}
	}
}





//-----------------------------------------------------------------
//
// The file chooser class.
//

class VictimChooser : public dsPopupWindow
{
	friend void SelFile (Widget, XtPointer, XtPointer);
	friend void SyncChoosers ();
	friend void ZapChoosers ();
	DelSelect *fc_ls;		// The load select widget
	char *fc_plat;			// The name of our platform
	IndexFile **fc_files;		// Index files
	Widget *fc_toggles;		// Toggles that go with them
	int fc_nfile;
//
// The following is used to keep track of existing choosers,
// mostly so that they can be found and updated or zapped.
//
	static VictimChooser *vc_choosers;
	VictimChooser *fc_next;		// Next in chain
	void SyncFiles ();			// Synch with reality
public:
	int index;			// not used -- for container
	VictimChooser (DelSelect *, const char *);
	~VictimChooser ();
	void MarkAll (const int state);
	void popdown () { delete this; };
};


VictimChooser *VictimChooser::vc_choosers = 0;




VictimChooser::VictimChooser (DelSelect *ls, const char *plat) :
	dsPopupWindow (*Disp, "Victim chooser", 470)
//
// Make a file chooser.
//
{
	Arg args[15];
	int n, line;
	const int labelwidth = 150;
	Widget above = corner, left = NULL, vp, vpform;
	IndexFile *file;
	char header[200];
//
// Save some stuff.
//
	fc_ls = ls;
	fc_plat = new char[strlen (plat) + 1];
	strcpy (fc_plat, plat);
	file = ls->pindex ()->files (plat);
	for (fc_nfile = 0; file; file = file->next ())
		fc_nfile++;	// just counting
	fc_files = new IndexFile *[fc_nfile];
	fc_toggles = new Widget[fc_nfile];
//
// Put us on the list of existing choosers.
//
	fc_next = vc_choosers;
	vc_choosers = this;
//
// Slip the platform name in next to the title.
//
	sprintf (header, "(%s)", plat);
	n = 0;
	XtSetArg (args[n], XtNlabel, header);			n++;
	XtSetArg (args[n], XtNfromVert, NULL);			n++;
	XtSetArg (args[n], XtNfromHoriz, corner);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	AddConstraints (args, &n);
	(void) XtCreateManagedWidget ("title", labelWidgetClass, dw_form,
		args, n);
//
// Throw in a title line over the file list.
//
	strcpy (header,
	  "File name                Begin time           End time         MB");
	n = 0;
	XtSetArg (args[n], XtNlabel, header);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNhorizDistance, 150);		n++;
	AddConstraints (args, &n);
	above = XtCreateManagedWidget ("header", labelWidgetClass, dw_form,
		args, n);
//
// We need a viewport to display the file list.
//
	n = 0;
	XtSetArg (args[n], XtNallowVert, True);			n++;
	XtSetArg (args[n], XtNheight, 200);			n++;
	XtSetArg (args[n], XtNwidth, 610);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNleft, XtChainLeft);		n++;
	XtSetArg (args[n], XtNright, XtChainRight);		n++;
	XtSetArg (args[n], XtNtop, XtChainTop);			n++;
	XtSetArg (args[n], XtNbottom, XtChainBottom);		n++;
	XtSetArg (args[n], XtNforceBars, True);			n++;
	vp = XtCreateManagedWidget ("lvp", viewportWidgetClass, dw_form,
			args, n);
	vpform = XtCreateManagedWidget ("lform", formWidgetClass, vp, 0, 0);
//
// Time to go through and add all of the file buttons.
//
	above = NULL;
	file = ls->pindex ()->files (plat);
	for (line = 0; line < fc_nfile; line++)
	{
		left = NULL;
		fc_files[line] = file;
	//
	// Start with the "select" toggle.
	//
		n = 0;
		XtSetArg (args[n], XtNlabel, "delete");		n++;
		XtSetArg (args[n], XtNfromVert, above);		n++;
		XtSetArg (args[n], XtNfromHoriz, left);		n++;
		XtSetArg (args[n], XtNstate, file->isMarked ()); n++;
		AddConstraints (args, &n);
		left = fc_toggles[line] = XtCreateManagedWidget ("delete",
			toggleWidgetClass, vpform, args, n);
		XtAddCallback (fc_toggles[line], XtNcallback, SelFile, this);
	//
	// Then the info line thereafter.
	//
		char label[200];
		MakeFileLabel (file, label);
		n = 0;
		XtSetArg (args[n], XtNlabel, label);		n++;
		XtSetArg (args[n], XtNfromVert, above);		n++;
		XtSetArg (args[n], XtNfromHoriz, left);		n++;
		XtSetArg (args[n], XtNborderWidth, 0);		n++;
		XtSetArg (args[n], XtNvertDistance, 6);		n++;
		AddConstraints (args, &n);
		(void) XtCreateManagedWidget ("flabel", labelWidgetClass,
				vpform, args, n);
	//
	// On to the next one.
	//
		above = fc_toggles[line];
		file = file->next ();
	}
//
// A "select all" button.
//
	above = vp;
	left = NULL;
	n = 0;
	XtSetArg (args[n], XtNlabel, "Select all files");	n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddBotConstraints (args, &n);
	left = XtCreateManagedWidget ("selall", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, MarkAllFiles, this);
//
// Unselect all
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Unselect all");		n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddBotConstraints (args, &n);
	left = XtCreateManagedWidget ("unselall", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, UnmarkAllFiles, this);
}






void
VictimChooser::MarkAll (const int state)
//
// Set all of the files to a given state.
//
{
	int line;
//
// Get all the files and toggles to the given state.
//
	for (line = 0; line < fc_nfile; line++)
		if (fc_files[line]->isMarked () != state)
		{
			Arg args[1];
			XtSetArg (args[0], XtNstate, state);
			XtSetValues (fc_toggles[line], args, 1);
			fc_ls->MarkFile (fc_files[line], state);
		}
//
// If we have selected files, insure that the platform is selected too.
//
	if (state)
		fc_ls->MarkPlat (fc_plat, True);
}





void
VictimChooser::SyncFiles ()
//
// Make all the file toggles match reality.
//
{
	int line;
	Arg args[1];

	for (line = 0; line < fc_nfile; line++)
	{
		XtSetArg (args[0], XtNstate, fc_files[line]->isMarked ());
		XtSetValues (fc_toggles[line], args, 1);
	}
}






static void
SyncChoosers ()
//
// Get all existing file choosers to synchronize themselves.
//
{
	VictimChooser *fc;

	for (fc = VictimChooser::vc_choosers; fc; fc = fc->fc_next)
		fc->SyncFiles ();
}




static void
ZapChoosers ()
//
// Get rid of all choosers.
//
{
	while (VictimChooser::vc_choosers)
		delete VictimChooser::vc_choosers;
}





static void
MarkAllFiles (Widget w, XtPointer xfc, XtPointer junk)
{
	((VictimChooser *) xfc)->MarkAll (True);
	SyncChoosers();
}




static void
UnmarkAllFiles (Widget w, XtPointer xfc, XtPointer junk)
{
	((VictimChooser *) xfc)->MarkAll (False);
	SyncChoosers();
}






static void
SelFile (Widget w, XtPointer xfc, XtPointer junk)
//
// (un)Select a file.
//
{
	int line;
	VictimChooser *fc = (VictimChooser *) xfc;
	Arg args[1];
	Boolean state;
//
// Go through and figure out which line they are on.
//
	for (line = 0; line < fc->fc_nfile; line++)
		if (w == fc->fc_toggles[line])
			break;
	if (line >= fc->fc_nfile)	// Should never happen!!
	{
		cerr << "SelFile on unknown toggle!\n";
		return;
	}
//
// Find out the current state of the toggle and set the file accordingly.
//
	XtSetArg (args[0], XtNstate, &state);
	XtGetValues (w, args, 1);
	fc->fc_ls->MarkFile (fc->fc_files[line], state);
//
// Make sure the platform is marked.
//
	fc->fc_ls->MarkPlat (fc->fc_plat, True);
	SyncChoosers();
}






static void
MakeFileLabel (const IndexFile *file, char *label)
//
// Make the file label.
//
{
	const char *slash;
//
// Find the base file name.
//
	if (slash = strrchr (file->name (), '/'))
		slash++;
	else 
		slash = file->name ();
	sprintf (label, "%-29s", slash);
	label += 29;
	label[-1] = ' ';
//
// Begin and end time.
//
	TC_EncodeTime (&file->begin (), TC_Full, label);
	strcat (label, "     ");
	label += 20;
	TC_EncodeTime (&file->end (), TC_Full, label);
	strcat (label, "     ");
	label += 20;
//
// Throw the size onto the end and we are done.
//
	sprintf (label, "%.3f", file->size ()/1048576.0);
}




VictimChooser::~VictimChooser ()
//
// Destruction time.
//
{
	VictimChooser *link;
//
// Get rid of dynamic stuff.
//
	delete[] fc_plat;
	delete[] fc_files;
	delete[] fc_toggles;
//
// Remove us from the file chooser chain.
//
	if (vc_choosers == this)
		vc_choosers = fc_next;
	else
	{
		for (link = vc_choosers; link; link = link->fc_next)
			if (link->fc_next == this)
				break;
		if (! link)
			cerr << "What!  File chooser gone from list!\n";
		else
			link->fc_next = this->fc_next;
	}
}






static void
GetVictimChooser (Widget w, XtPointer xls, XtPointer junk)
//
// Put up a file chooser.
//
{
	Arg args[1];
	char *plat;
//
// Extract the platform from the widget.
//
	XtSetArg (args[0], XtNradioData, &plat);
	XtGetValues (w, args, 1);
//
// Now we can create and put up the file chooser.
//
	VictimChooser *vc = new VictimChooser ((DelSelect *) xls, plat);
	vc->popup ();
}




//-------------------
//
// Externally callable stuff to make all this happen.
//
extern PlatformIndex *MakeDSIndex ();

/* ARGSUSED */
void
DoTBDelete (Widget w, XtPointer junk1, XtPointer junk2)
//
// Actually perform a time-based deletion.
//
{
	PlatformIndex *ourindex = MakeDSIndex ();
	DelSelect *ds = new DelSelect (ourindex);

	ds->popup ();
}
