/*
 * Widget for changing radar parameters
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
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
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/AsciiText.h>

# include <ui.h>
# include <defs.h>

# include "prototypes.h"
# include "radar.h"

RCSID ("$Id: RadarWidget.c,v 1.8 1997-04-29 03:51:15 granger Exp $")

/*
 * Which radar are we dealing with?
 */
static int	UseRad = 0;
static Radar	Copy;

/*
 * global widgets
 */
static Widget	WChangeRad, WRadName, WExecute, WDiscard, WScanType, WStatus;
static Widget	WHits, WStep, WPrf, WMinRng, WMinElev;

Widget	LeftRightButtons ();	/* extern function */

/*
 * Private prototypes
 */
static Widget	rw_ParamWidget FP ((Widget));
static Widget	rw_EnableWidget FP ((Widget));
static void	rw_ShowParams FP ((void));
static void	rw_ChangeRad FP ((Widget, XtPointer, XtPointer));
static void	rw_ChangeType FP ((Widget, XtPointer, XtPointer));
static void	rw_ChangeStatus FP ((Widget, XtPointer, XtPointer));
static void	rw_ChangeHits FP ((Widget, XtPointer, XtPointer));
static void	rw_ChangeStep FP ((Widget, XtPointer, XtPointer));
static void	rw_TextChange FP ((Widget, XtPointer, XtPointer));
static void	rw_ExecChanges FP ((Widget, XtPointer, XtPointer));
static void	rw_DiscardChanges FP ((Widget, XtPointer, XtPointer));
static void	rw_AllowRadChange FP ((int));
static void	rw_ToggleEnable FP ((Widget, XtPointer, XtPointer));




Widget
rw_RWidget (parent)
Widget	parent;
/*
 * Create the radar widget
 */
{
	Widget	form, pwidget, ewidget;
	int	n;
	Arg	args[20];
/*
 * Create a form widget to hold everything
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = XtCreateManagedWidget ("radarForm", formWidgetClass, parent,
		args, n);
/*
 * Make the param widget and the enable/disable widget and set their
 * positions 
 */
	pwidget = rw_ParamWidget (form);
	ewidget = rw_EnableWidget (form);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetValues (pwidget, args, n);
	
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, pwidget); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetValues (ewidget, args, n);
/*
 * Done 
 */
	return (form);
}




static Widget
rw_ParamWidget (parent)
Widget	parent;
/*
 * Create the radar parameter adjustment widget
 */
{
	Widget	form, w, w_above, msrlabel;
	int	n, nldefs;
	Arg	args[20], ldefaults[10];
	XtCallbackRec	cblist[2];
/*
 * Make a copy of the radar we're using
 */
	Copy = Rad[UseRad];
/*
 * Create a form widget to hold everything
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 3); n++;
	form = XtCreateManagedWidget ("paramForm", formWidgetClass, parent,
		args, n);
/*
 * Next and prev radar buttons
 */
	w = WChangeRad = LeftRightButtons (form, rw_ChangeRad, NULL);
	
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetValues (w, args, n);
/*
 * Create a set of default args for our Label widgets
 */
	nldefs = 0;
	XtSetArg (ldefaults[nldefs], XtNborderWidth, 0); nldefs++;
	XtSetArg (ldefaults[nldefs], XtNjustify, XtJustifyLeft); nldefs++;
	XtSetArg (ldefaults[nldefs], XtNleft, XtChainLeft); nldefs++;
	XtSetArg (ldefaults[nldefs], XtNresizable, True); nldefs++;
/*
 * Label to show which radar we're dealing with
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	w = WRadName = XtCreateManagedWidget ("radarLabel", labelWidgetClass,
		form, args, n);
	XtSetValues (WRadName, ldefaults, nldefs);
/*
 * "Execute Changes" and "Discard Changes" buttons
 */
	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 2); n++;
	XtSetArg (args[n], XtNlabel, "Execute Changes"); n++;
	XtSetArg (args[n], XtNsensitive, False); n++;
	w = WExecute = XtCreateManagedWidget ("execute", commandWidgetClass, 
		form, args, n);
	XtAddCallback (WExecute, XtNcallback, rw_ExecChanges, 0);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 2); n++;
	XtSetArg (args[n], XtNlabel, "Discard Changes"); n++;
	XtSetArg (args[n], XtNsensitive, False); n++;
	w = WDiscard = XtCreateManagedWidget ("discard", commandWidgetClass, 
		form, args, n);
	XtAddCallback (WDiscard, XtNcallback, rw_DiscardChanges, 0);
/*
 * Buttons to change scan type
 */
	w_above = w;

	w = LeftRightButtons (form, rw_ChangeType, NULL);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetValues (w, args, n);
/*
 * Label for current scan type
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	w = WScanType = XtCreateManagedWidget ("stypeLabel", labelWidgetClass,
		form, args, n);
	XtSetValues (WScanType, ldefaults, nldefs);
/*
 * Buttons to change status
 */
	w_above = w;

	w = LeftRightButtons (form, rw_ChangeStatus, NULL);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetValues (w, args, n);
/*
 * Label for current status
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	w = WStatus = XtCreateManagedWidget ("statusLabel", labelWidgetClass,
		form, args, n);
	XtSetValues (WStatus, ldefaults, nldefs);
/*
 * Buttons to change hits
 */
	w_above = w;

	w = LeftRightButtons (form, rw_ChangeHits, NULL);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetValues (w, args, n);
/*
 * Label for current hits
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	w = WHits = XtCreateManagedWidget ("hitsLabel", labelWidgetClass,
		form, args, n);
	XtSetValues (WHits, ldefaults, nldefs);
/*
 * Buttons for fixed/variable steps
 */
	w_above = w;

	w = LeftRightButtons (form, rw_ChangeStep, NULL);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetValues (w, args, n);
/*
 * Label for current step choice
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	w = WStep = XtCreateManagedWidget ("stepLabel", labelWidgetClass,
		form, args, n);
	XtSetValues (WStep, ldefaults, nldefs);
/*
 * Build the callback list for use with the AsciiText widgets. 
 * (We can't just use XtAddCallback for AsciiText widgets, since the callback 
 * list is actually associated with the widget's AsciiSrc subwidget)
 */
	cblist[0].callback = (XtCallbackProc) rw_TextChange;
	cblist[0].closure = NULL;

	cblist[1].callback = NULL;
	cblist[1].closure = NULL;
/*
 * PRF
 */
	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNlabel, "PRF: "); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);
	XtSetValues (w, ldefaults, nldefs);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WPrf = XtCreateManagedWidget ("PRF", asciiTextWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "Hz"); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);
/*
 * Minimum scan range
 */
	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNlabel, "Minimum Scan Range: "); n++;
	w = msrlabel = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);
	XtSetValues (w, ldefaults, nldefs);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WMinRng = XtCreateManagedWidget ("minRange", asciiTextWidgetClass, 
		form, args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "km"); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);
/*
 * Minimum scan range
 */
	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNlabel, "Minimum Elevation: "); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);
	XtSetValues (w, ldefaults, nldefs);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, msrlabel); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WMinElev = XtCreateManagedWidget ("minElev", asciiTextWidgetClass, 
		form, args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "deg."); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);
/*
 * Put current values into the labels
 */
	rw_ShowParams ();
/*
 * Done 
 */
	return (form);
}




Widget
rw_EnableWidget (parent)
Widget	parent;
/*
 * Create the widget for enabling/disabling the radars
 */
{
	Widget	form, w, w_above;
	int	r, n;
	Arg	args[10];
	char	name[10], label[20];
/*
 * Create a form widget to hold everything
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 3); n++;
	form = XtCreateManagedWidget ("radarEnableForm", formWidgetClass, 
		parent, args, n);
/*
 * Title
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "Available Radars"); n++;
	w = XtCreateManagedWidget ("title", labelWidgetClass, form, args, n);
/*
 * A command widget for each radar
 */
	w_above = w;

	for (r = 0; r < Nradars; r++)
	{
		sprintf (name, "toggle%d", r);
		sprintf (label, "%s %s", Rad[r].name, 
			Rad[r].enabled ? "" : "(Disabled)");

		n = 0;
		XtSetArg (args[n], XtNfromVert, w_above); n++;
		XtSetArg (args[n], XtNresizable, True); n++;
		XtSetArg (args[n], XtNresize, True); n++;
		XtSetArg (args[n], XtNlabel, label); n++;

		w = XtCreateManagedWidget (name, commandWidgetClass, 
			form, args, n);

		XtAddCallback (w, XtNcallback, rw_ToggleEnable, (XtPointer) r);

		w_above = w;
	}
/*
 * Done 
 */
	return (form);
}




static void
rw_ShowParams ()
/*
 * Put in the current values for UseRad
 */
{
	Arg	args[10];
	int	n;
	char	label[50];
/*
 * Radar name
 */
	sprintf (label, "Radar: %s", Copy.name);

	n = 0;
	XtSetArg (args[n], XtNlabel, label); n++;
	XtSetArg (args[n], XtNheight, 32); n++;
	XtSetValues (WRadName, args, n);
/*
 * Scan type
 */
	switch (Copy.scantype)
	{
	    case PPI:
		strcpy (label, "PPI");
		break;
	    case RHI:
		strcpy (label, "RHI");
		break;
	    case SUR:
		strcpy (label, "SUR");
		break;
	    default:
		ui_error ("BUG: Unknown scan type %d in rw_ShowParams");
	}

	n = 0;
	XtSetArg (args[n], XtNlabel, label); n++;
	XtSetArg (args[n], XtNheight, 32); n++;
	XtSetValues (WScanType, args, n);
/*
 * Radar status
 */
	switch (Copy.status)
	{
	    case MatchBoth:
		strcpy (label, "Match time and spatial");
		break;
	    case MatchTime:
		strcpy (label, "Match time");
		break;
	    case MatchSpatial:
		strcpy (label, "Match spatial");
		break;
	    default:
		ui_error ("BUG: Unknown scan type %d in rw_ShowParams");
	}

	n = 0;
	XtSetArg (args[n], XtNlabel, label); n++;
	XtSetArg (args[n], XtNheight, 32); n++;
	XtSetValues (WStatus, args, n);
/*
 * Hits
 */
	if (Copy.fix_hits)
		sprintf (label, "Hits: Fixed (%d)", Copy.hits);
	else
		sprintf (label, "Hits: Variable");

	n = 0;
	XtSetArg (args[n], XtNlabel, label); n++;
	XtSetArg (args[n], XtNheight, 32); n++;
	XtSetValues (WHits, args, n);
/*
 * Step choice
 */
	if (Copy.fix_step)
		sprintf (label, "Steps: Constant");
	else
		sprintf (label, "Steps: Variable");

	n = 0;
	XtSetArg (args[n], XtNlabel, label); n++;
	XtSetArg (args[n], XtNheight, 32); n++;
	XtSetValues (WStep, args, n);
/*
 * Only update the AsciiText widgets if the user hasn't changed anything yet
 * (i.e., if the "Execute Changes" button is not sensitive)
 */
	if (! XtIsSensitive (WExecute))
	{
	/*
	 * PRF
	 */
		sprintf (label, "%d", Copy.prf);

		n = 0;
		XtSetArg (args[n], XtNstring, label); n++;
		XtSetValues (WPrf, args, n);
	/*
	 * Minimum range
	 */
		sprintf (label, "%.1f", Copy.min_range);

		n = 0;
		XtSetArg (args[n], XtNstring, label); n++;
		XtSetValues (WMinRng, args, n);
	/*
	 * Minimum elevation
	 */
		sprintf (label, "%.1f", Copy.min_elev);

		n = 0;
		XtSetArg (args[n], XtNstring, label); n++;
		XtSetValues (WMinElev, args, n);
	}
}




static void
rw_ChangeRad (w, change, junk)
Widget		w;
XtPointer	change, junk;
/*
 * Callback to change the radar we're dealing with
 */
{
	int	delta = (int) change;
/*
 * Change UseRad
 */
	UseRad = (UseRad + delta) % Nradars;
	if (UseRad < 0)
		UseRad += Nradars;
/*
 * Get a copy of the new radar
 */
	Copy = Rad[UseRad];
/*
 * Make the widget reflect the radar change
 */
	rw_ShowParams ();
}




static void
rw_ChangeType (w, change, junk)
Widget		w;
XtPointer	change, junk;
/*
 * Callback to change the scan type
 */
{
	int	delta = (int) change, type, ntypes = (int) N_STYPES;
/*
 * We're changing something, so disable the radar buttons
 */
	rw_AllowRadChange (False);
/*
 * Get the new scan type
 */
	type = (int) Copy.scantype;
	type = (type + delta) % ntypes;
	if (type < 0)
		type += ntypes;
/*
 * Assign it
 */
	Copy.scantype = (s_type) type;

	rw_ShowParams ();
}




static void
rw_ChangeStatus (w, change, junk)
Widget		w;
XtPointer	change, junk;
/*
 * Callback to change the radar's status
 */
{
	int	delta = (int) change, status, nstat = (int) N_STATUS;
/*
 * We're changing something, so disable the radar buttons
 */
	rw_AllowRadChange (False);
/*
 * Get the new scan type
 */
	status = (int) Copy.status;
	status = (status + delta) % nstat;
	if (status < 0)
		status += nstat;
/*
 * Assign it
 */
	Copy.status = (rstatus) status;

	rw_ShowParams ();
}




static void
rw_ChangeHits (w, change, junk)
Widget		w;
XtPointer	change, junk;
/*
 * Callback to change the number of hits
 */
{
	int	delta = (int) change;
	float	factor;
/*
 * We're changing something, so disable the radar buttons
 */
	rw_AllowRadChange (False);
/*
 * If it's currently variable, switch to fixed hits
 */
	if (! Copy.fix_hits)
	{
		Copy.fix_hits = TRUE;
		Copy.hits = (delta > 0) ? Copy.min_hits : Copy.max_hits;
	}
/*
 * For fixed hits, double (or halve) the current hits.  If it's outside
 * the legal range, switch to variable hits.
 */
	else
	{
		factor = (delta > 0) ? 2 : 0.5;

		Copy.hits *= factor;
		if (Copy.hits > Copy.max_hits || Copy.hits < Copy.min_hits)
		{
			Copy.hits = 0;
			Copy.fix_hits = FALSE;
		}
	}

	rw_ShowParams ();
}




static void
rw_ChangeStep (w, change, junk)
Widget		w;
XtPointer	change, junk;
/*
 * Callback to change step choice
 */
{
/*
 * We're changing something, so disable the radar buttons
 */
	rw_AllowRadChange (False);
/*
 * This is easy
 */
	Copy.fix_step = ! Copy.fix_step;

	rw_ShowParams ();
}




static void
rw_TextChange (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Callback to signal a text change
 */
{
/*
 * We're changing something, so disable the radar buttons
 */
	rw_AllowRadChange (False);
}




static void
rw_AllowRadChange (s)
bool	s;
/*
 * Set the sensitive state of the radar buttons to 's' and set the 
 * sensitive state of the "Execute" and "Discard" buttons to '!s'.
 */
{
	s ? XtMapWidget (WChangeRad) : XtUnmapWidget (WChangeRad);

	XtSetSensitive (WExecute, !s);
	XtSetSensitive (WDiscard, !s);
}




static void
rw_ExecChanges (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Make the changes as currently shown in the widget
 */
{
	Arg	arg;
	int	prf;
	float	min_range, min_elev;
	char	*string;
/*
 * Get the current PRF
 */
	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WPrf, &arg, 1);

	if (sscanf (string, "%d", &prf) == 1)
		Copy.prf = prf;
/*
 * Get the current minimum range
 */
	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WMinRng, &arg, 1);

	if (sscanf (string, "%f", &min_range) == 1)
		Copy.min_range = min_range;
/*
 * Get the current minimum elevation
 */
	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WMinElev, &arg, 1);

	if (sscanf (string, "%f", &min_elev) == 1)
		Copy.min_elev = min_elev;
/*
 * Copy the changed stuff into the real radar (we can't just do 
 * "Rad[UseRad] = Copy", because if the volume boundary has been changed, the
 * new limits are not reflected in Copy)
 */
	Rad[UseRad].status = Copy.status;
	Rad[UseRad].scantype = Copy.scantype;
	Rad[UseRad].prf = Copy.prf;
	Rad[UseRad].hits = Copy.hits;
	Rad[UseRad].fix_hits = Copy.fix_hits;
	Rad[UseRad].fix_step = Copy.fix_step;
	Rad[UseRad].min_range = Copy.min_range;
	Rad[UseRad].min_elev = Copy.min_elev;
/*
 * Enable the radar change buttons
 */
	rw_AllowRadChange (True);
/*
 * Update the display
 */
	rw_ShowParams ();
	ScanOptions ();
}




static void
rw_DiscardChanges (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Discard the changes reflected in the widget
 */
{
	rw_AllowRadChange (True);
	Copy = Rad[UseRad];
	rw_ShowParams ();
}




static void
rw_ToggleEnable (w, val, junk)
Widget		w;
XtPointer	val, junk;
/*
 * Toggle the enabled/disabled state of radar 'val'
 */
{
	int	r = (int) val;
	char	label[20];
	Arg	arg;
/*
 * Toggle the enabled/disabled state
 */
	Rad[r].enabled = ! Rad[r].enabled;
/*
 * Update the widget label
 */
	sprintf (label, "%s %s", Rad[r].name, 
		Rad[r].enabled ? "" : "(Disabled)");
	XtSetArg (arg, XtNlabel, label);
	XtSetValues (w, &arg, 1);
/*
 * Recalculate the scan options
 */
	ScanOptions ();
}
