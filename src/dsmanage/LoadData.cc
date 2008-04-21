//
// Loading data from whereever.
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
}
//# include "container.h"
# include "dsPlatform.h"
# include "STable.h"
# include "dsmWindows.h"
# include "Index.h"
# include "ZTime.h"
# include "plcontainer.h"
MAKE_RCSID ("$Id: LoadData.cc,v 1.18 2002-12-18 00:24:12 granger Exp $")

using std::cerr;

class LoadSelect;

void dsSourceSelect (Widget, XtPointer, XtPointer);
void dsLoadContinue (Widget, XtPointer, XtPointer);

void SetToggles (Widget, XtPointer, XtPointer);
void ClearToggles (Widget, XtPointer, XtPointer);
void PerformLoad (Widget, XtPointer, XtPointer);
void SkipCB (Widget, XtPointer, XtPointer);
void ExecTimeSelect (Widget, XtPointer, XtPointer);
static void PlatformSel (Widget, XtPointer, XtPointer);
static void MarkTimeFiles (LoadSelect *ls, PlatformIndex *select,
		const ZebTime &tbegin, const ZebTime &tend, int skip);
static void MakeFileLabel (const IndexFile *, char *);
static void GetFileChooser (Widget, XtPointer, XtPointer);
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


//--------------------------------------------------------------
// The load source widget.
//

class dsLoadSource : public dsPopupWindow
{
	friend void dsSourceSelect (Widget, XtPointer, XtPointer);
	int	tape;		// Loading from tape?
	Widget	file;		// Tape/directory
	Widget	twidget;	// Tape widget
	Widget	gripe;		// Where we put complaints.
	Widget	devprompt;	// Device/directory prompt
	Widget	indprompt;	// Index file prompt
	Widget	indtext;	// Associated text
public:
	dsLoadSource ();
	~dsLoadSource ();
	const char *GetIndFile ();
	const char *GetDev ();
	int isTape () { return tape; }	// Going to tape?
	void complain (const char *);	// Put out a complaint
};

static dsLoadSource *dsLdSrcWidget = 0;


dsLoadSource::dsLoadSource () :
		dsPopupWindow (*Disp, "Load data -- source", 140)
//
// Create the "load source" dialog.
//
{
	Arg args[15];
	int n;
	const int labelwidth = 120;
	Widget above = corner, left = NULL;
	char *defcd = getenv ("ZEB_CDROM");
//
// A simple label for starters.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Load data from:");	n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNwidth, labelwidth);		n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("prompt", labelWidgetClass, dw_form,
			args, n);
//
// Choose a default mode.  Go to CD-ROM mode iff ZEB_CDROM defined.
//
	tape = (defcd != NULL) ? False : True;
//
// The "tape" button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "Tape");			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNstate, (tape) ? True : False);		n++;
	AddConstraints (args, &n);
	twidget = left = XtCreateManagedWidget ("srcToggle", toggleWidgetClass,
			dw_form, args, n);
	XtAddCallback (twidget, XtNcallback, dsSourceSelect, (XtPointer) 1);
//
// The CDROM button.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, "CDROM");			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNstate, (tape) ? False : True);	n++;
	XtSetArg (args[n], XtNradioGroup, left);		n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("srcToggle", toggleWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, dsSourceSelect, (XtPointer) 0);
//
// File prompt.
//
	n = 0;
	left = NULL;
	above = twidget;
	XtSetArg (args[n], XtNlabel, 
		  tape ? "Tape device:" : "CD Directory:");	n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNwidth, labelwidth);		n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	XtSetArg (args[n], XtNjustify, XtJustifyRight);		n++;
	AddConstraints (args, &n);
	devprompt = left = XtCreateManagedWidget ("dev", labelWidgetClass,
			dw_form, args, n);
//
// The blank for the source.
//
	n = 0;
	XtSetArg (args[n], XtNstring, 
		  tape ? "/dev/rst8" : defcd);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);		n++;
	XtSetArg (args[n], XtNwidth, 225);			n++;
	AddConstraints (args, &n);
	file = above = XtCreateManagedWidget ("dtext", asciiTextWidgetClass,
			dw_form, args, n);
//
// Index file prompt.
//
	n = 0;
	left = NULL;
	above = file;
	XtSetArg (args[n], XtNlabel, "Tape index file:");	n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNwidth, labelwidth);		n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	XtSetArg (args[n], XtNjustify, XtJustifyRight);		n++;
	XtSetArg (args[n], XtNsensitive, tape ? True : False);	n++;
	AddConstraints (args, &n);
	indprompt = left = XtCreateManagedWidget ("indfile", labelWidgetClass,
			dw_form, args, n);
//
// The blank for the tape index file.
//
	n = 0;
	XtSetArg (args[n], XtNstring, "tape.index");		n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);		n++;
	XtSetArg (args[n], XtNwidth, 225);			n++;
	XtSetArg (args[n], XtNsensitive, tape ? True : False);	n++;
	AddConstraints (args, &n);
	indtext = above = XtCreateManagedWidget("indfile",asciiTextWidgetClass,
			dw_form, args, n);
//
// The "continue" button
//
	n = 0;
	left = NULL;
	XtSetArg (args[n], XtNlabel, "Continue...");		n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("continue", commandWidgetClass, 
			dw_form, args, n);
	XtAddCallback (left, XtNcallback, dsLoadContinue, (XtPointer) this);
//
// The gripe area -- hopefully stays blank.
//
	n = 0;
	XtSetArg (args[n], XtNlabel, " ");			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNwidth, 200);			n++;
	XtSetArg (args[n], XtNresize, False);			n++;
	AddConstraints (args, &n);
	gripe = XtCreateManagedWidget ("gripe", labelWidgetClass, dw_form,
			args, n);
}




dsLoadSource::~dsLoadSource ()
{ }





void
LoadData (Widget w, XtPointer junk1, XtPointer junk2)
//
// Put up the data loader.
//
{
//
// Make sure we have a widget created, then put it on the screen.
//
	if (! dsLdSrcWidget)
		dsLdSrcWidget = new dsLoadSource;
	dsLdSrcWidget->complain (" ");
	dsLdSrcWidget->popup ();
}




void
dsSourceSelect (Widget w, XtPointer tbutton, XtPointer junk)
//
// The hit a source select button.
//
{
	Arg args[2];
	const char *defcd = getenv ("ZEB_CDROM") ? 
	    getenv ("ZEB_CDROM") : "/cd";
//
// Set the tape flag.  If it hasn't changed, we can quit.
//
	if ((tbutton && dsLdSrcWidget->tape) ||
				(! tbutton && ! dsLdSrcWidget->tape))
		return;
	dsLdSrcWidget->tape = (tbutton != 0);
//
// Put in a default source.
//
	XtSetArg (args[0], XtNlabel, dsLdSrcWidget->tape ?
			"Tape device:" : "CD Directory:");
	XtSetValues (dsLdSrcWidget->devprompt, args, 1);
	XtSetArg (args[0], XtNstring,
			dsLdSrcWidget->tape ? "/dev/rst8" : defcd);
	XtSetValues (dsLdSrcWidget->file, args, 1);
//
// Index sensitivity
//
	XtSetArg (args[0], XtNsensitive, tbutton);
	XtSetValues (dsLdSrcWidget->indprompt, args, 1);
	XtSetValues (dsLdSrcWidget->indtext, args, 1);
}





void
dsLoadSource::complain (const char *complaint)
//
// Put something in the complain area.
//
{
	Arg args[2];

	XtSetArg (args[0], XtNlabel, complaint);
	XtSetValues (gripe, args, 1);
}






const char *
dsLoadSource::GetIndFile ()
//
// Return the name of the selected index file.
//
{
	Arg args[2];
	char *ret;
	
	XtSetArg (args[0], XtNstring, &ret);
	XtGetValues (indtext, args, 1);
	return (ret);
}


const char *
dsLoadSource::GetDev ()
//
// Return the name of the selected device/directory.
//
{
	Arg args[2];
	char *ret;
	
	XtSetArg (args[0], XtNstring, &ret);
	XtGetValues (file, args, 1);
	return (ret);
}





//
// The LoadSelect widget -- presents a list of what can be snarfed.
//

class LoadSelect : public dsPopupWindow
{
	friend void SkipCB (Widget, XtPointer, XtPointer);
	Widget startTime;		// Begin time
	Widget endTime;
	PlatformIndex *index;		// Index we are working from
	int tape;
	char *dir;
	Widget	*toggles;		// All our toggles.
	int ntoggle;			// How many there are.
	Widget gripe, sgripe;		// Complaint windows.
	Widget minutes;			// Minute skip
	Widget skipt;			// Skip toggle
	const char *getstring (Widget) const;// Extract val from string widget.
	int nfile, nbyte;		// Amount selected.
	Widget fsummary;		// File select summary
	void addButtons (Widget, PlatformIndex *);
	void UpdFSummary ();
public:
	LoadSelect (PlatformIndex *, const char *, int);
	~LoadSelect ();
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
	const char *tfile () const { return dir; }
	int istape () const { return tape; }
	int file_sel () const { return nfile; }
	int byte_sel () const { return nbyte; }
//
// File accounting.
//
	void MarkFile (IndexFile *file, const int state);
	void MarkPlat (const char *plat, const int state);
};





LoadSelect::LoadSelect (PlatformIndex *ind, const char *directory,
				int fromtape) :
	dsPopupWindow (*Disp, "Load data -- selection", 545)
//
// Create the "load select" dialog.
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
	tape = fromtape;
	dir = new char[strlen (directory) + 1];
	strcpy (dir, directory);
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
// Optional skip
//
	n = 0;
	left = NULL;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNhorizDistance, 175);		n++;
	XtSetArg (args[n], XtNlabel, "only select every");	n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNvertDistance, 6);			n++;
	AddConstraints (args, &n);
	skipt = left = XtCreateManagedWidget ("skipt", toggleWidgetClass,
			selform, args, n);
	XtAddCallback (skipt, XtNcallback, SkipCB, (XtPointer) this);
//
// Minutes.
//
	n = 0;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);		n++;
	XtSetArg (args[n], XtNwidth, 50);			n++;
	XtSetArg (args[n], XtNsensitive, False);		n++;
	XtSetArg (args[n], XtNstring, "30");			n++;
	AddConstraints (args, &n);
	minutes = left = XtCreateManagedWidget("skip", asciiTextWidgetClass,
			selform, args, n);
//
// trailing "minutes"
//
	n = 0;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNlabel, "minutes.");		n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNvertDistance, 6);			n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("skipm", labelWidgetClass, selform,
			args, n);
//
// The execute button.
//
	n = 0;
	left = NULL;
	above = minutes;
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
"Platform                   Data present                            Data available"); n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNhorizDistance, 75);		n++;
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
	XtSetArg (args[n], XtNlabel, "Load the data");		n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	XtSetArg (args[n], XtNfromHoriz, left);			n++;
	XtSetArg (args[n], XtNhorizDistance, 75);		n++;
	AddConstraints (args, &n);
	left = XtCreateManagedWidget ("doit", commandWidgetClass, dw_form,
			args, n);
	XtAddCallback (left, XtNcallback, PerformLoad, (XtPointer) this);
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



LoadSelect::~LoadSelect () 
{
	delete[] dir;
	ZapChoosers (); 
};



void
LoadSelect::UpdFSummary ()
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
LoadSelect::addButtons (Widget form, PlatformIndex *index)
//
// Add all the platform buttons.
//
{
    char ebuf[180], *ep;
    Widget toggle, above = NULL, label, cw;
    PlatformId pid;
    int n;
    Arg args[15];
//
// Allocate space to hold the toggles.
//
    ntoggle = PList->ncontained ();	// One per platform
    toggles = new Widget[ntoggle];
//
// Go through the list.
//
    for (pid = 0; pid < PList->ncontained (); pid++)
    {
	ZebTime bt, et;
	const dsPlatform &p = PList->nth (pid);
	int nfiles = p.files.ncontained();
    //
    // Add a toggle for this platform.
    //
	n = 0;
	XtSetArg (args[n], XtNlabel, "Select");			n++;
	XtSetArg (args[n], XtNfromHoriz, 0);			n++;
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
	sprintf (ebuf, "%-12s", p.name ());
	ep = ebuf + 12;
    //
    // Local data.
    //
	if (nfiles == 0)
	{
	    strcpy (ep, "               (none)                       ");
	    ep += strlen (ep);
	}
	else
	{
	    TC_EncodeTime (p.files.nth(0).begin(), TC_Full, ep);
	    strcat (ep, "   ");
	    ep += 18;
	    *ep = '\0';
	    strcat (ep, " -> ");
	    ep += strlen (ep);
	    TC_EncodeTime (p.files.nth(nfiles - 1).end(), TC_Full, ep);
	    strcat (ep, "        ");
	    ep += 22;
	}
    //
    // Available data.
    //
	if (! index->coverage (p.name (), bt, et))
	    strcpy (ep, "    (none available)");
	else
	{
	    TC_EncodeTime (&bt, TC_Full, ep);
	    strcat (ep, " -> ");
	    ep += strlen (ep);
	    TC_EncodeTime (&et, TC_Full, ep);
	    strcat (ep, "        ");
	    ep += 22;
	}
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
	XtAddCallback (cw, XtNcallback, GetFileChooser, this);
	toggles[pid] = above = toggle;
    }
}







void LoadSelect::complain (const char *complaint, int self)
//
// Register a complaint.
//
{
	Arg args[2];

	XtSetArg (args[0], XtNlabel, complaint);
	XtSetValues (self ? sgripe : gripe, args, 1);
}




const char *
LoadSelect::getstring (Widget w) const
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
LoadSelect::MarkPlat (const char *platform, const int state)
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
// Set the toggle.  We do this because platform states can be set
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
LoadSelect::MarkFile (IndexFile *file, const int state)
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
LoadSelect::selected () const
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
	LoadSelect *ls = (LoadSelect *) xls;
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
	LoadSelect *ls = (LoadSelect *) xls;
	char *plat;

	XtSetArg (args[0], XtNstate, &state);
	XtSetArg (args[1], XtNradioData, &plat);
	XtGetValues (w, args, 2);
	ls->MarkPlat (plat, state);
}





void
dsLoadContinue (Widget w, XtPointer xdialog, XtPointer junk)
//
// Continue with the load process.
//
{
	dsLoadSource *dialog = (dsLoadSource *) xdialog;
	const char *ifile = dialog->GetIndFile ();
	char ifname[200];
//
// Quick check to insure that the index is a real file.
//
	if (dialog->isTape () && access (ifile, R_OK))
	{
		dialog->complain ("Can't access index file");
		return;
	}
//
// Put together the index file name for CD's.
//
	if (! dialog->isTape ())
	{
		strcpy (ifname, dialog->GetDev ());
		strcat (ifname, "/");
		strcat (ifname, ".cd_index");
	}
//
// Load the index in either case.
//
	PlatformIndex *index =
		new PlatformIndex (dialog->isTape () ? ifile : ifname);
//
// Throw a select widget up on the screen, get this one off, and we
// are done.
//
	LoadSelect *ls = new LoadSelect (index, dialog->GetDev (),
			dialog->isTape ());
	dialog->popdown ();
	ls->popup ();
}






void SetToggles (Widget w, XtPointer xls, XtPointer junk)
//
// Set all of the toggles.
//
{
	int plat;

	for (plat = 0; plat < PList->ncontained (); plat++)
		((LoadSelect *) xls)->MarkPlat (PList->nth(plat).name(), True);
}


void ClearToggles (Widget w, XtPointer xls, XtPointer junk)
//
// Clear all of the toggles.
//
{
	int plat;

	for (plat = 0; plat < PList->ncontained (); plat++)
		((LoadSelect *) xls)->MarkPlat (PList->nth(plat).name(),False);
}





void PerformLoad (Widget w, XtPointer xls, XtPointer junk)
//
// Actually load the data onto the disk.
//
{
	LoadSelect *ls = (LoadSelect *) xls;
	PlatformIndex *select;
	int tape = ls->istape (), fsel = ls->file_sel ();
	int bsel = ls->byte_sel ();
//
// Get the list of selected platforms.
//
	if ((select = ls->selected ()) == NULL)
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
// Pass it off to be executed.
//
	char tdev[200];
	strcpy (tdev, ls->tfile ());
	ls->popdown ();
	DLoad (select, tape, tdev, fsel, bsel);
}






void SkipCB (Widget w, XtPointer xls, XtPointer junk)
//
// Ths skip button has been hit.
//
{
	LoadSelect *ls = (LoadSelect *) xls;
	Arg args[2];
	Boolean state;
//
// Find out the toggle state now, then make the sensitivity of the
// text widget match it.
//
	XtSetArg (args[0], XtNstate, &state);
	XtGetValues (ls->skipt, args, 1);
	XtSetArg (args[0], XtNsensitive, state);
	XtSetValues (ls->minutes, args, 1);
}




int
LoadSelect::skip () const
//
// Return the skip period.
//
{
	Arg args[1];
	Boolean state;
	const char *str;
//
// See if the skip is enabled.  If not, just return zero.
//
	XtSetArg (args[0], XtNstate, &state);
	XtGetValues (skipt, args, 1);
	if (! state)
		return (0);
//
// OK, we need to extract the time and send it back.
//
	str = getstring (minutes);
	return (atoi (str) * 60);
}





void ExecTimeSelect (Widget w, XtPointer xls, XtPointer junk)
//
// They have asked to select files based on time.
//
{
	LoadSelect *ls = (LoadSelect *) xls;
	const char *begin = ls->begin ();
	const char *end = ls->end ();
	ZebTime tbegin, tend;
	PlatformIndex *select;
	SValue v;
	int skip = ls->skip ();

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
	MarkTimeFiles (ls, select, tbegin, tend, skip);
//
// We also have to make sure that any file selection widgets get updated
// properly.
//
	SyncChoosers ();
}





static void
MarkTimeFiles (LoadSelect *ls, PlatformIndex *select,
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

class FileChooser : public dsPopupWindow
{
	friend void SelFile (Widget, XtPointer, XtPointer);
	friend void SyncChoosers ();
	friend void ZapChoosers ();
	LoadSelect *fc_ls;		// The load select widget
	char *fc_plat;			// The name of our platform
	IndexFile **fc_files;		// Index files
	Widget *fc_toggles;		// Toggles that go with them
	int fc_nfile;
//
// The following is used to keep track of existing choosers,
// mostly so that they can be found and updated or zapped.
//
	static FileChooser *fc_choosers;
	FileChooser *fc_next;		// Next in chain
	void SyncFiles ();			// Synch with reality
public:
	int index;			// not used -- for container
	FileChooser (LoadSelect *, const char *);
	~FileChooser ();
	void MarkAll (const int state);
	void popdown () { delete this; };
};


FileChooser *FileChooser::fc_choosers = 0;




FileChooser::FileChooser (LoadSelect *ls, const char *plat) :
	dsPopupWindow (*Disp, "File chooser", 470)
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
	fc_next = fc_choosers;
	fc_choosers = this;
//
// Slip the platform name in next to the title.
//
	sprintf (header, "(%s)", plat);
	n = 0;
	XtSetArg (args[n], XtNlabel, header);			n++;
	XtSetArg (args[n], XtNfromVert, 0);			n++;
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
	XtSetArg (args[n], XtNfromHoriz, 0);			n++;
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
		XtSetArg (args[n], XtNlabel, "select");		n++;
		XtSetArg (args[n], XtNfromVert, above);		n++;
		XtSetArg (args[n], XtNfromHoriz, left);		n++;
		XtSetArg (args[n], XtNstate, file->isMarked ()); n++;
		AddConstraints (args, &n);
		left = fc_toggles[line] = XtCreateManagedWidget ("select",
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
FileChooser::MarkAll (const int state)
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
FileChooser::SyncFiles ()
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
	FileChooser *fc;

	for (fc = FileChooser::fc_choosers; fc; fc = fc->fc_next)
		fc->SyncFiles ();
}




static void
ZapChoosers ()
//
// Get rid of all choosers.
//
{
	while (FileChooser::fc_choosers)
		delete FileChooser::fc_choosers;
}





static void
MarkAllFiles (Widget w, XtPointer xfc, XtPointer junk)
{
	((FileChooser *) xfc)->MarkAll (True);
	SyncChoosers ();
}




static void
UnmarkAllFiles (Widget w, XtPointer xfc, XtPointer junk)
{
	((FileChooser *) xfc)->MarkAll (False);
	SyncChoosers ();
}






static void
SelFile (Widget w, XtPointer xfc, XtPointer junk)
//
// (un)Select a file.
//
{
	int line;
	FileChooser *fc = (FileChooser *) xfc;
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
	SyncChoosers ();
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
	label += strlen (label);
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




FileChooser::~FileChooser ()
//
// Destruction time.
//
{
	FileChooser *link;
//
// Get rid of dynamic stuff.
//
	delete[] fc_plat;
	delete[] fc_files;
	delete[] fc_toggles;
//
// Remove us from the file chooser chain.
//
	if (fc_choosers == this)
		fc_choosers = fc_next;
	else
	{
		for (link = fc_choosers; link; link = link->fc_next)
			if (link->fc_next == this)
				break;
		if (! link)
			cerr << "What!  File chooser gone from list!\n";
		else
			link->fc_next = this->fc_next;
	}
}






static void
GetFileChooser (Widget w, XtPointer xls, XtPointer junk)
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
	FileChooser *fc = new FileChooser ((LoadSelect *) xls, plat);
	fc->popup ();
}
