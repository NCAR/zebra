/*
 * Scan option generation
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

static char *rcsid = "$Id: ScanOptions.c,v 1.6 1993-10-21 17:59:16 burghart Exp $";

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>

# include <ui.h>
# include <ui_error.h>
# include "globals.h"
# include "prototypes.h"

/*
 * Scan option widget stuff
 */
static Widget	WScanOpts, WOpt[7];

/*
 * Display widget stuff
 */
Widget	WDisplay = 0;		/* the widget itself			*/
Widget	WDispOption;		/* which option are we displaying?	*/
Widget	WDispRadar;		/* radar name label			*/
Widget	WDispText;		/* text portion of the display widget	*/
bool	DisplayUp = False;	/* is the widget popped up?		*/
# define STRSIZE 2048		/* size of the display string		*/
char	DisplayStr[STRSIZE] = "";	/* display string		*/
int	DispRadar = 0;		/* which radar are we showing?		*/

/*
 * Header string for the option list
 */
static char	OptionHeader[] = "\
    SLOW    SCAN  HOR. RES.  VER. RES.\n\
    RADAR   TIME  deg.  km   deg.  km  SWEEPS\n\
    -----   ----  ---------  --------- ------";

/*
 * Private prototypes
 */
# ifdef __STDC__
	static Widget	so_CreateDisplayWidget (void);
	static void	so_DisplayCallback (Widget, XtPointer, XtPointer);
	static void	so_DisplayRadar (int);
	static void	so_ChangeRadar (Widget, XtPointer, XtPointer);
	static void	so_RemoveDisplay (Widget, XtPointer, XtPointer);
# else
	static Widget	so_CreateDisplayWidget ();
	static void	so_DisplayCallback ();
	static void	so_DisplayRadar ();
	static void	so_ChangeRadar ();
	static void	so_RemoveDisplay ();
# endif




void
ScanOptions ()
/*
 * Generate a few scan options for the user and return the number of the
 * limiting radar
 */
{
	int	r, itime, opt, slow;
	float	slowtime, voltime, hres, vres, dis;
	Radar	temp;
	char	string[80], empty[] = "";
	Arg	stringarg, arg, emptyarg;
	Pixel	color;
/*
 * Sanity check
 */
	if (Nradars == 0)
	{
		msg_ELog (EF_PROBLEM, "No radars to optimize!");
		ui_bailout (NULL);
	}
/*
 * Put the address of 'string' into an X Toolkit argument
 */
	XtSetArg (stringarg, XtNlabel, string);
	XtSetArg (emptyarg, XtNlabel, empty);
/*
 * Unhighlight the old option
 */
	if (Opt != NO_OPT)
	{
		XtSetArg (arg, XtNbackground, &color);
		XtGetValues (WOpt[Opt+3], &arg, 1);
		XtSetArg (arg, XtNborderColor, color);
		XtSetValues (WOpt[Opt+3], &arg, 1);
	}
/*
 * Initialize option limits
 */
	Opt = NO_OPT;
	MinOpt = 999;
	MaxOpt = -999;
/*
 * Empty the display widget
 */
	if (WDisplay)
	{
		Arg	arg;

		XtSetArg (arg, XtNlabel, "  ");
		XtSetValues (WDispOption, &arg, 1);

		DisplayStr[0] = '\0';
		XtSetArg (arg, XtNstring, DisplayStr);
		XtSetValues (WDispText, &arg, 1);
	}
/*
 * Loop through the options, presenting time/space resolution trade-offs
 */
	for (opt = -3; opt < 4; opt++)
	{
	/*
	 * Clear out the widget label
	 */
		sprintf (string, "%2d  No Scan ", opt);
		XtSetValues (WOpt[opt+3], &stringarg, 1);
	/*
	 * Resolution to use for this option
	 */
		hres = Hres + RES_STEP * opt;
		vres = Vres + RES_STEP * opt;
	/*
	 * Resolution sanity check
	 */
		if (hres <= 0.0 || vres <= 0.0)
			continue;
	/*
	 * Initialize the slow radar and slow scan time
	 */
		slow = 0;
		slowtime = 0.0;
	/*
	 * Error catch so we can skip to the next option if any radar
	 * can't cut it for this one
	 */
		ERRORCATCH
		/*
		 * Find the slow (limiting) radar with status MatchBoth, 
		 * or use zero if no radar has MatchBoth status
		 */
			for (r = 0; r < Nradars; r++)
			{
				if (! Rad[r].enabled)
					continue;
			/*
			 * Generate the optimal scan and check its time
			 */
				GenScan (Rad + r, TIME_ASAP, hres, vres, TRUE);

				if (Rad[r].scantime > slowtime && 
					Rad[r].status == MatchBoth)
				{
					slowtime = Rad[r].scantime;
					slow = r;
				}
			}
		ON_ERROR
			ui_epop ();
			continue;
		ENDCATCH

		Slowtime[opt+3] = slowtime;
	/*
	 * If we're using a fixed time, make sure the slow radar can 
	 * actually do it
	 */
		if (Vol_time != TIME_ASAP && slowtime > Vol_time &&
			Rad[slow].status != MatchTime)
		{
			msg_ELog (EF_INFO, 
				"%s cannot scan in %d s for option %d",
				Rad[slow].name, (int)(Vol_time + 0.5), opt);
			continue;
		}
	/*
	 * For the fixed time case, we can generate the real scans now
	 */
		if (Vol_time != TIME_ASAP)
		{
			for (r = 0; r < Nradars; r++)
			{
				if (! Rad[r].enabled)
					continue;

				GenScan (Rad + r, Vol_time, hres, vres, TRUE);
			}
			Slowtime[opt+3] = slowtime = Vol_time;
		}
	/*
	 * Keep track of the limits of our valid options
	 */
		MinOpt = (opt < MinOpt) ? opt : MinOpt;
		MaxOpt = (opt > MaxOpt) ? opt : MaxOpt;
	/*
	 * Display this scan option
	 */
		itime = (int)(slowtime + 0.5);
		hres = Rad[slow].res_horiz;
		vres = Rad[slow].res_vert;
		dis = Rad[slow].rng_back;

		sprintf (string, 
			"%2d %6s  %2d:%02d  %4.2f %4.2f  %4.2f %4.2f  %4d  ",
			opt, Rad[slow].name, itime / 60, itime % 60, 
			ATAND (hres / dis), hres, ATAND (vres / dis), vres, 
			Rad[slow].nsweeps);
	/*
	 * Fool the widget by giving it an empty label and then the new one
	 */
		XtSetValues (WOpt[opt+3], &emptyarg, 1);
		XtSetValues (WOpt[opt+3], &stringarg, 1);
	}
/*
 * Make 0 the current option if possible.  Otherwise, use the lowest
 * legal option.
 */
	Opt = MAX (0, MinOpt);
	if (MinOpt > MaxOpt)
		Opt = NO_OPT;
/*
 * Highlight the current option
 */
	if (Opt != NO_OPT)
	{
		XtSetArg (arg, XtNforeground, &color);
		XtGetValues (WOpt[Opt+3], &arg, 1);
		XtSetArg (arg, XtNborderColor, color);
		XtSetValues (WOpt[Opt+3], &arg, 1);
	}
/*
 * Kluge to force a redisplay.  This doesn't work and shouldn't be necessary
 * anyway.
 */
	XSync (XtDisplay (WScanOpts), False);
}




void
so_Display (opt)
int	opt;
/*
 * Display full information for the chosen scan option
 */
{
	int	r;
	char	ostring[16];
	Arg	arg;
	Pixel	color;
/*
 * Make sure this is an acceptable option number
 */
	if (opt < MinOpt || opt > MaxOpt)
	{
		msg_ELog (EF_PROBLEM, "Legal scan options are %d to %d", 
			MinOpt, MaxOpt);
		ui_bailout (NULL);
	}
/*
 * Unhighlight the old option
 */
	if (Opt != NO_OPT)
	{
		XtSetArg (arg, XtNbackground, &color);
		XtGetValues (WOpt[Opt+3], &arg, 1);
		XtSetArg (arg, XtNborderColor, color);
		XtSetValues (WOpt[Opt+3], &arg, 1);
	}
/*
 * Set the global option number and highlight the new option
 */
	Opt = opt;

	XtSetArg (arg, XtNforeground, &color);
	XtGetValues (WOpt[Opt+3], &arg, 1);
	XtSetArg (arg, XtNborderColor, color);
	XtSetValues (WOpt[Opt+3], &arg, 1);
/*
 * Show the current display radar
 */
	if (! WDisplay)
		so_CreateDisplayWidget ();

	sprintf (ostring, "Option %d", Opt);
	XtSetArg (arg, XtNlabel, ostring);
	XtSetValues (WDispOption, &arg, 1);

	so_DisplayRadar (DispRadar);
	XtSetArg (arg, XtNstring, DisplayStr);
	XtSetValues (WDispText, &arg, 1);

	if (! DisplayUp)
	{
		XtPopup (WDisplay, XtGrabNone);
		DisplayUp = True;
	}
	else
	/*
	 * Make sure the display widget is on top.
	 * Isn't there an Xt function for this?
	 */
		XRaiseWindow (XtDisplay (WDisplay), XtWindow (WDisplay));
}




static void
so_DisplayRadar (r)
int	r;
/*
 * Display the selected scan for radar r
 */
{
	float	hres, vres;
	int	minutes, seconds, i;
	char	*s;
/*
 * If the radar is disabled, get out now
 */
	if (! Rad[r].enabled)
	{
		sprintf (DisplayStr, "** DISABLED **");
		return;
	}
/*
 * Generate the scan for this radar, using the time from the slow radar
 * and resolution based on the option number
 */
	hres = Hres + Opt * RES_STEP;
	vres = Vres + Opt * RES_STEP;
	ERRORCATCH
		GenScan (Rad + r, Slowtime[Opt+3], hres, vres, TRUE);
	ON_ERROR
		msg_ELog (EF_PROBLEM, "BUG: Scan should be possible!");
	ENDCATCH
/*
 * Write the info into DisplayStr
 */
	minutes = (int)(Rad[r].scantime + 0.5) / 60;
	seconds = (int)(Rad[r].scantime + 0.5) % 60;

	sprintf (DisplayStr, "Nsweeps: %d  Scan Rate: %.1f  Hits: %d\n", 
		Rad[r].nsweeps, Rad[r].scanrate, Rad[r].hits);

	s = DisplayStr + strlen (DisplayStr);
	sprintf (s, "H Res.: %.2f   V Res.: %.2f\n", Rad[r].res_horiz, 
		Rad[r].res_vert);

	s = DisplayStr + strlen (DisplayStr);
	sprintf (s, "Scan Time: %d:%02d\n", minutes, seconds);

	s = DisplayStr + strlen (DisplayStr);
	sprintf (s, "Left Azimuth: %.1f   Right Azimuth: %.1f\n",
		Rad[r].az_left, Rad[r].az_right);

	s = DisplayStr + strlen (DisplayStr);
	sprintf (s, "Bottom Elevation: %.1f   Top Elevation: %.1f\n",
		Rad[r].el_bottom, Rad[r].el_top);
/*
 * Angle list
 */
	s = DisplayStr + strlen (DisplayStr);
	if (Rad[r].fix_step)
	{
	/*
	 * Just print the limits and step for a constant step list
	 */
		sprintf (s, "Angle List: %.1f to %.1f every %.1f", 
			Rad[r].anglist[0], 
			Rad[r].anglist[Rad[r].nsweeps - 1],
			Rad[r].anglist[1] - Rad[r].anglist[0]);
	}
	else
	{
		sprintf (s, "Angle List: ");

		for (i = 0; i < Rad[r].nsweeps; i++)
		{
			s = DisplayStr + strlen (DisplayStr);

			if (! (i % 8))
				sprintf (s++, "\n");

			sprintf (s, "%.1f ", Rad[r].anglist[i]);
		}
	}

	s = DisplayStr + strlen (DisplayStr);
	sprintf (s, "\n");
/*
 * Radio readable parameters, all in one place and in the right order: 
 * left azimuth, right azimuth, scan rate, bottom elevation, top elevation, 
 * step, and number of hits
 */
	if (Rad[r].fix_step)
	{
		float	left, right, bottom, top, step;

		s = DisplayStr + strlen (DisplayStr);
		sprintf (s, "\n\nRadio order scan parameters\n");

		if (Rad[r].scantype == RHI)
		{
			left = Rad[r].anglist[0];
			right = Rad[r].anglist[Rad[r].nsweeps - 1];
			bottom = MAX (Rad[r].min_elev, Rad[r].el_bottom);
			top = Rad[r].el_top;
		}
		else
		{
			left = Rad[r].az_left;
			right = Rad[r].az_right;
			bottom = Rad[r].anglist[0];
			top = Rad[r].anglist[Rad[r].nsweeps - 1];
		}


		s = DisplayStr + strlen (DisplayStr);
		sprintf (s, "L:%.1f  R:%.1f  V:%.1f  B:%.1f  T:%.1f  ",
			left, right, Rad[r].scanrate, bottom, top);

		step = Rad[r].anglist[1] - Rad[r].anglist[0];

		s = DisplayStr + strlen (DisplayStr);
		sprintf (s, "S:%.1f  H:%d\n", step, Rad[r].hits);
	}

}




Widget
so_SOWidget (parent)
Widget	parent;
/*
 * Build a widget for showing the scan options
 */
{
	int	i, n;
	Arg	args[10];
	Widget	form, w, w_above;
	char	name[20];
	Pixel	bg;
/*
 * A frame to hold the whole thing
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 3); n++;
	XtSetArg (args[n], XtNresizable, True); n++;
	WScanOpts = form = XtCreateManagedWidget ("optionForm", 
		formWidgetClass, parent, args, n);
/*
 * Label widget for header stuff
 */
	n = 0;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, OptionHeader); n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft); n++;
	w = XtCreateManagedWidget ("header", labelWidgetClass, form, args, n);
/*
 * Find our background color
 */
	XtSetArg (args[0], XtNbackground, &bg);
	XtGetValues (form, args, 1);
/*
 * A command widget for each option line
 */
	w_above = w;

	for (i = -3; i < 4; i++)
	{
		sprintf (name, "option%d", i);

		n = 0;
		XtSetArg (args[n], XtNborderWidth, 2); n++;
		XtSetArg (args[n], XtNborderColor, bg); n++;
		XtSetArg (args[n], XtNhighlightThickness, 1); n++;
		XtSetArg (args[n], XtNfromVert, w_above); n++;
		XtSetArg (args[n], XtNresize, True); n++;
		XtSetArg (args[n], XtNresizable, True); n++;
		w = WOpt[i+3] = XtCreateManagedWidget (name, 
			commandWidgetClass, form, args, n);

		XtAddCallback (w, XtNcallback, so_DisplayCallback, 
			       (XtPointer) i);

		w_above = w;
	}
/*
 * Return the widget
 */
	return (form);
}




static Widget
so_CreateDisplayWidget ()
/*
 * Build a widget for displaying the selected scan
 */
{
	int	i, n;
	Arg	args[15];
	Widget	form, w, wbuttons;
/*
 * Create the popup widget shell and the form widget within it that holds
 * everything
 */
	n = 0;
	XtSetArg (args[n], XtNresize, True); n++;
	WDisplay = XtCreatePopupShell ("OptimizerDisplay", 
		topLevelShellWidgetClass, WScanOpts, args, n);

	n = 0;
	XtSetArg (args[n], XtNborderWidth, 3); n++;
	form = XtCreateManagedWidget ("displayForm", formWidgetClass, WDisplay,
		args, n);
/*
 * Buttons to change radar displayed and the radar label
 */
	w = wbuttons = LeftRightButtons (form, so_ChangeRadar, NULL);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNwidth, 120); n++;
	XtSetArg (args[n], XtNresize, False); n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft); n++;
	XtSetArg (args[n], XtNlabel, Rad[DispRadar].name); n++;
	w = WDispRadar = XtCreateManagedWidget ("radar", labelWidgetClass, 
		form, args, n);
/*
 * Label to show which option is being displayed
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNwidth, 120); n++;
	XtSetArg (args[n], XtNresize, False); n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft); n++;
	w = WDispOption = XtCreateManagedWidget ("option", labelWidgetClass, 
		form, args, n);
/*
 * Button to remove widget
 */	
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNborderWidth, 2); n++;
	XtSetArg (args[n], XtNlabel, "Remove"); n++;
	w = XtCreateManagedWidget ("remove", commandWidgetClass, form, 
		args, n);
	XtAddCallback (w, XtNcallback, so_RemoveDisplay, (XtPointer) 0);
/*
 * AsciiText widget to hold the scan info
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, wbuttons); n++;
	XtSetArg (args[n], XtNwidth, 375); n++;
	XtSetArg (args[n], XtNheight, 150); n++;
	XtSetArg (args[n], XtNdisplayCaret, False); n++;
	XtSetArg (args[n], XtNscrollHorizontal, XawtextScrollWhenNeeded); n++;
	XtSetArg (args[n], XtNscrollVertical, XawtextScrollWhenNeeded); n++;
	WDispText = XtCreateManagedWidget ("display", asciiTextWidgetClass, 
		form, args, n);
}




static void
so_DisplayCallback (w, val, junk)
Widget		w;
XtPointer	val, junk;
/*
 * Display a scan option chosen from the widget
 */
{
	int	option = (int) val;

	if (option < MinOpt || option > MaxOpt)
		return;
	else
		so_Display (option);
}




static void
so_ChangeRadar (w, val, junk)
Widget		w;
XtPointer	val, junk;
/*
 * Change the radar we're displaying
 */
{
	int	change = (int) val;
	int	n;
	Arg	arg;
/*
 * Get the number of the new radar to display
 */
	DispRadar += change;
	if (DispRadar >= Nradars)
		DispRadar = 0;
	else if (DispRadar < 0)
		DispRadar = Nradars - 1;
/*
 * Change the label by the buttons
 */
	XtSetArg (arg, XtNlabel, Rad[DispRadar].name);
	XtSetValues (WDispRadar, &arg, 1);
/*
 * Generate the new display if we currently have one
 */
	if (DisplayStr[0] != '\0')
	{
		so_DisplayRadar (DispRadar);
		XtSetArg (arg, XtNstring, DisplayStr);
		XtSetValues (WDispText, &arg, 1);
	}
}




static void
so_RemoveDisplay (w, junk1, junk2)
Widget		w;
XtPointer	junk1, junk2;
/*
 * Remove (pop down) the display widget
 */
{
	if (DisplayUp)
	{
		XtPopdown (WDisplay);
		DisplayUp = False;
	}
}
