/*
 * "Command" widget module
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

static char *rcsid = "$Id: CommandWidget.c,v 1.5 1997-04-29 03:51:12 granger Exp $";

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/AsciiText.h>

# include "prototypes.h"
# include "globals.h"
# include "radar.h"
# include "bitmaps.h"

/*
 * global widgets
 */
static Widget	WExecute, WDiscard, WHres, WVres, WHsep, WVsep;
static Widget	WBot, WTop, WTime, WAsap;

/*
 * Private prototypes
 */
static void	cw_ShowParams FP ((void));
static void	cw_Exit FP ((Widget, XtPointer, XtPointer));
static void	cw_TextChange FP ((Widget, XtPointer, XtPointer));
static void	cw_ExecChanges FP ((Widget, XtPointer, XtPointer));
static void	cw_DiscardChanges FP ((Widget, XtPointer, XtPointer));
static void	cw_TimeASAP FP ((Widget, XtPointer, XtPointer));
static void	cw_EnableButtons FP ((int));




Widget
cw_CWidget (parent)
Widget	parent;
/*
 * Create the "command" widget, through which non-radar-specific parameters
 * are controlled.
 */
{
	Widget	form, w, w_above, w_left, w_column1;
	int	n;
	Arg	args[20];
	XtCallbackRec	cblist[2];
/*
 * Build the callback list for use with the AsciiText widgets. 
 * (We can't just use XtAddCallback for AsciiText widgets, since the callback 
 * list is actually associated with the widget's AsciiSrc subwidget)
 */
	cblist[0].callback = (XtCallbackProc) cw_TextChange;
	cblist[0].closure = NULL;

	cblist[1].callback = NULL;
	cblist[1].closure = NULL;
/*
 * Make the enclosing form widget
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 3); n++;
	form = XtCreateManagedWidget ("commandForm", formWidgetClass, parent,
		args, n);
/*
 * "Execute Changes" and "Discard Changes" buttons
 */
	w_above = NULL;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 2); n++;
	XtSetArg (args[n], XtNlabel, "Exit Program"); n++;
	w = XtCreateManagedWidget ("exit", commandWidgetClass, 
		form, args, n);
	XtAddCallback (w, XtNcallback, cw_Exit, 0);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNhorizDistance, 50); n++;
	XtSetArg (args[n], XtNborderWidth, 2); n++;
	XtSetArg (args[n], XtNlabel, "Execute Changes"); n++;
	XtSetArg (args[n], XtNsensitive, False); n++;
	w = WExecute = XtCreateManagedWidget ("execute", commandWidgetClass, 
		form, args, n);
	XtAddCallback (WExecute, XtNcallback, cw_ExecChanges, 0);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 2); n++;
	XtSetArg (args[n], XtNlabel, "Discard Changes"); n++;
	XtSetArg (args[n], XtNsensitive, False); n++;
	w = WDiscard = XtCreateManagedWidget ("discard", commandWidgetClass, 
		form, args, n);
	XtAddCallback (WDiscard, XtNcallback, cw_DiscardChanges, 0);
/*
 * Spatial resolution
 */
	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "Desired Spatial Resolution"); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);

	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "            Horizontal: "); n++;
	w = w_left = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_left); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WHres = XtCreateManagedWidget ("hRes", asciiTextWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "km"); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);

	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "            Vertical: "); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_left); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WVres = XtCreateManagedWidget ("vRes", asciiTextWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "km"); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);
/*
 * Minimum beam separation
 */
	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "Minimum Beam Separation"); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);

	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "            Horizontal: "); n++;
	w = w_left = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_left); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WHsep = XtCreateManagedWidget ("hSep", asciiTextWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "deg."); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);

	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "            Vertical: "); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_left); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WVsep = XtCreateManagedWidget ("vSep", asciiTextWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "deg."); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);
/*
 * Volume height bounds
 */
	w_above = WExecute;
	w_column1 = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_column1); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "Volume Height Bounds"); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, args, n);

	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_column1); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "            Bottom:  "); n++;
	w = w_left = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_left); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WBot = XtCreateManagedWidget ("volBot", asciiTextWidgetClass, form,
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "km"); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);

	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_column1); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "            Top: "); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_left); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WTop = XtCreateManagedWidget ("volTop", asciiTextWidgetClass, form,
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "km"); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);
/*
 * Volume height bounds
 */
	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_column1); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, "Scan Time:"); n++;
	w = XtCreateManagedWidget ("label", labelWidgetClass, form, 
		args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_left); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
        XtSetArg (args[n], XtNwidth, 40); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	XtSetArg (args[n], XtNcallback, cblist); n++;
	w = WTime = XtCreateManagedWidget ("volTime", asciiTextWidgetClass, 
		form, args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNlabel, "s"); n++;
	w = XtCreateManagedWidget ("units", labelWidgetClass, form, args, n);

	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w_left); n++;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNinternalWidth, 2); n++;
	XtSetArg (args[n], XtNhorizDistance, 0); n++;
	XtSetArg (args[n], XtNbitmap, ASAPButton); n++;
	w = WAsap = XtCreateManagedWidget ("asap", commandWidgetClass, 
		form, args, n);

	XtAddCallback (WAsap, XtNcallback, cw_TimeASAP, 0);
/*
 * Put current values into the labels
 */
	cw_ShowParams ();

	return (form);
}




static void
cw_ShowParams ()
/*
 * Put in the current values
 */
{
	Arg	labelarg;
	char	label[50];
/*
 * Put the address of label into an X argument
 */
	XtSetArg (labelarg, XtNstring, label);
/*
 * Only update the AsciiText widgets if the user hasn't changed anything yet
 * (i.e., if the "Execute Changes" button is not sensitive)
 */
	if (! XtIsSensitive (WExecute))
	{
	/*
	 * Horizontal and vertical resolution
	 */
		sprintf (label, "%.1f", Hres);
		XtSetValues (WHres, &labelarg, 1);

		sprintf (label, "%.1f", Vres);
		XtSetValues (WVres, &labelarg, 1);
	/*
	 * Minimum horizontal and vertical beam separation
	 */
		sprintf (label, "%.1f", Hsep_min);
		XtSetValues (WHsep, &labelarg, 1);

		sprintf (label, "%.1f", Vsep_min);
		XtSetValues (WVsep, &labelarg, 1);
	/*
	 * Volume bottom and top
	 */
		sprintf (label, "%.1f", Vol_bot);
		XtSetValues (WBot, &labelarg, 1);

		sprintf (label, "%.1f", Vol_top);
		XtSetValues (WTop, &labelarg, 1);
	/*
	 * Volume scan time
	 */
		if (Vol_time == TIME_ASAP)
			sprintf (label, "Var.");
		else
			sprintf (label, "%d", (int)(Vol_time + 0.5));

		XtSetValues (WTime, &labelarg, 1);
	}
}




static void
cw_Exit (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Callback to exit the program
 */
{
	opt_Finish ();
}




static void
cw_TextChange (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Callback to signal a text change
 */
{
/*
 * Something's changed, so enable the "Execute" and "Discard" buttons
 */
	cw_EnableButtons (True);
}




static void
cw_EnableButtons (s)
bool	s;
/*
 * Enable or disable the "Execute" and "Discard" buttons.
 */
{
	XtSetSensitive (WExecute, s);
	XtSetSensitive (WDiscard, s);
}




static void
cw_ExecChanges (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Make the changes as currently shown in the widget
 */
{
	Arg	arg;
	float	val;
	char	*string;
	int	r;
/*
 * Get the current horizontal and vertical resolution values
 */
	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WHres, &arg, 1);

	if (sscanf (string, "%f", &val) == 1)
		Hres = val;

	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WVres, &arg, 1);

	if (sscanf (string, "%f", &val) == 1)
		Vres = val;
/*
 * Get the minimum horizontal and vertical beam separations
 */
	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WHsep, &arg, 1);

	if (sscanf (string, "%f", &val) == 1)
		Hsep_min = val;

	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WVsep, &arg, 1);
	if (sscanf (string, "%f", &val) == 1)
		Vsep_min = val;
/*
 * Get the volume bottom and top
 */
	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WBot, &arg, 1);
	if (sscanf (string, "%f", &val) == 1)
		Vol_bot = val;

	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WTop, &arg, 1);
	if (sscanf (string, "%f", &val) == 1)
		Vol_top = val;
/*
 * Modify the bottom and top elevations for the radars based on
 * the new volume bottom and top
 */
	for (r = 0; r < Nradars; r++)
	{
		Rad[r].el_bottom = ATAND (Vol_bot / Rad[r].rng_back);
		Rad[r].el_top = ATAND (Vol_top / Rad[r].rng_front);
	}
/*
 * Get the volume scan time
 */
	XtSetArg (arg, XtNstring, &string);
	XtGetValues (WTime, &arg, 1);

	if (string[0] == 'v' || string[0] == 'V')
		Vol_time = TIME_ASAP;
	else if (sscanf (string, "%f", &val) == 1)
		Vol_time = val;
/*
 * Disable the buttons
 */
	cw_EnableButtons (False);
/*
 * Update the display
 */
	cw_ShowParams ();
	ScanOptions ();
}




static void
cw_DiscardChanges (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Discard the changes reflected in the widget
 */
{
	cw_EnableButtons (False);
	cw_ShowParams ();
}




static void
cw_TimeASAP (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Set volume scan time to TIME_ASAP
 */
{
	Arg	arg;
/*
 * Enable the "execute" and "discard" buttons, since something's changing
 */
	cw_EnableButtons (True);
/*
 * Write ASAP in the volume time widget
 */
	XtSetArg (arg, XtNstring, "Var.");
	XtSetValues (WTime, &arg, 1);
}
