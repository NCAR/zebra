/*
 * Scan option generation
 * $Id: ScanOptions.c,v 1.1 1991-06-16 17:02:25 burghart Exp $
 */
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
 * The slow (limiting) radar
 */
static int	Slowrad;

/*
 * Limits for acceptable options
 */
# define NO_OPT	-999
static int	Opt = NO_OPT, MinOpt, MaxOpt;

/*
 * Scan option widget stuff
 */
static bool	UseWidget = FALSE;
static Widget	WScanOpts, WOpt[7], WLimitRadar;

/*
 * Display widget stuff
 */
Widget	WDisplay = 0;		/* the widget itself			*/
Widget	WDispOption;		/* which option are we displaying?	*/
Widget	WDispRadar;		/* radar name label			*/
Widget	WDispText;		/* text portion of the display widget	*/
bool	DisplayUp = False;	/* is the widget popped up?		*/
# define STRSIZE 2048		/* size of the display string		*/
char	DisplayString[STRSIZE] = "";	/* display string		*/
int	DispRadar = 0;		/* which radar are we showing?		*/

/*
 * Header string for the option list
 */
static char	OptionHeader[] = "\n\
         Volume    Hor. Res.  Ver. Res.\n\
Option  Scan Time  deg.  km   deg.  km  Sweeps\n\
------  ---------  ---------  --------- ------";

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
	int	r, scantime, opt;
	float	slowtime, voltime, hres, vres, dis;
	Radar	slow, temp;
	char	string[80];
	Arg	stringarg;
/*
 * Sanity check
 */
	if (Nradars == 0)
		ui_error ("No radars to optimize!");
/*
 * Put the address of 'string' into an X Toolkit argument
 */
	XtSetArg (stringarg, XtNlabel, string);
/*
 * Initialize option limits and clear out the widget labels
 */
	Opt = NO_OPT;
	MinOpt = 999;
	MaxOpt = -999;

	if (UseWidget)
	{
		for (opt = -3; opt < 4; opt++)
		{
			sprintf (string, "%3d  No Scan ", opt);
			XtSetValues (WOpt[opt+3], &stringarg, 1);
		}
	}
/*
 * Empty the display widget
 */
	if (WDisplay)
	{
		Arg	arg;

		XtSetArg (arg, XtNlabel, "  ");
		XtSetValues (WDispOption, &arg, 1);

		DisplayString[0] = '\0';
		XtSetArg (arg, XtNstring, DisplayString);
		XtSetValues (WDispText, &arg, 1);
	}
/*
 * Find the slow (limiting) radar with status MatchBoth, or use zero if no 
 * radar has MatchBoth status
 */
	Slowrad = 0;
	slowtime = 0.0;

	for (r = 0; r < Nradars; r++)
	{
		if (! Rad[r].enabled)
			continue;
	/*
	 * Generate the fastest optimal scan and check its time
	 */
		GenScan (Rad + r, TIME_ASAP, Hres, Vres, TRUE);

		if (Rad[r].scantime > slowtime && Rad[r].status == MatchBoth)
		{
			slowtime = Rad[r].scantime;
			Slowrad = r;
		}
	}
/*
 * If we're using a fixed time, make sure the slow radar can actually do it
 */
	if (Vol_time != TIME_ASAP && Rad[Slowrad].scantime > Vol_time &&
		Rad[Slowrad].status != MatchTime)
		ui_bailout ("Radar %s cannot scan the volume in %d seconds",
			Rad[Slowrad].name, (int)(Vol_time + 0.5));
/*
 * Tell which is the slow radar
 */
	if (UseWidget)
	{
		sprintf (string, "The limiting radar is %s\n", 
			Rad[Slowrad].name);
		XtSetValues (WLimitRadar, &stringarg, 1);
	}
	else
	{
		ui_printf ("The limiting radar is %s\n\n", Rad[Slowrad].name);
		ui_printf ("%s\n", OptionHeader);
	}
/*
 * Generate up to seven scan options
 */
	for (opt = -3; opt < 4; opt++)
	{
		hres = Hres + RES_STEP * opt;
		vres = Vres + RES_STEP * opt;
	/*
	 * Resolution sanity check
	 */
		if (hres <= 0.0 || vres <= 0.0)
			continue;
	/*
	 * Make sure all radars can live up to this option.  If any can't,
	 * just move on to the next option.
	 */
		ERRORCATCH
			slow = Rad[Slowrad];
			GenScan (&slow, Vol_time, hres, vres, FALSE);

			for (r = 0; r < Nradars; r++)
			{
				if (r == Slowrad || ! Rad[r].enabled)
					continue;

				temp = Rad[r];
				GenScan (&temp, slow.scantime, hres, vres, 
					FALSE);
			}
		ON_ERROR
			ui_epop ();
			continue;
		ENDCATCH
	/*
	 * Keep track of the limits of our valid options
	 */
		MinOpt = (opt < MinOpt) ? opt : MinOpt;
		MaxOpt = (opt > MaxOpt) ? opt : MaxOpt;
	/*
	 * Display this scan option
	 */
		scantime = (int)(slow.scantime + 0.5);
		hres = slow.res_horiz;
		vres = slow.res_vert;
		dis = slow.rng_back;

		sprintf (string, "%3d    %5d:%02d  %6.2f%5.2f%6.2f%5.2f %4d  ",
			opt, scantime / 60, scantime % 60, ATAND (hres / dis), 
			hres, ATAND (vres / dis), vres, slow.nsweeps);

		if (UseWidget)
			XtSetValues (WOpt[opt+3], &stringarg, 1);
		else
			ui_printf ("%s\n", string);
	}

	if (! UseWidget)
		ui_printf ("\n");
/*
 * This is needed, but I don't know why
 */
	XSync (XtDisplay (WScanOpts), False);
}




void
so_Display (cmds)
struct ui_command	*cmds;
/*
 * Display full information for the chosen scan option
 */
{
	int	r, opt = UINT (cmds[0]);
	int	minutes, seconds;
	float	scantime, hres, vres;
	char	ostring[16];
/*
 * Make sure this is an acceptable option number
 */
	if (opt < MinOpt || opt > MaxOpt)
		ui_error ("Legal scan options are %d to %d", MinOpt, MaxOpt);
/*
 * Set the global option number
 */
	Opt = opt;
/*
 * Generate the scan for the slow radar first
 */
	hres = Hres + Opt * RES_STEP;
	vres = Vres + Opt * RES_STEP;
	GenScan (Rad + Slowrad, Vol_time, hres, vres, TRUE);
/*
 * If we have a display widget, just show the current display radar, otherwise
 * spit all the radars to the screen
 */
	if (UseWidget)
	{
		Arg	arg;

		if (! WDisplay)
			so_CreateDisplayWidget ();

		sprintf (ostring, "Option %d", Opt);
		XtSetArg (arg, XtNlabel, ostring);
		XtSetValues (WDispOption, &arg, 1);

		so_DisplayRadar (DispRadar);
		XtSetArg (arg, XtNstring, DisplayString);
		XtSetValues (WDispText, &arg, 1);

		if (! DisplayUp)
		{
			XtPopup (WDisplay, XtGrabNone);
			DisplayUp = True;
		}
	}
	else
	{
		for (r = 0; r < Nradars; r++)
		{
			ui_printf ("\n%s\n", Rad[r].name);
			so_DisplayRadar (r);
			ui_printf (DisplayString);
		}
	}
}




static void
so_DisplayRadar (r)
int	r;
/*
 * Display the selected scan for radar r
 */
{
	float	scantime, hres, vres;
	int	minutes, seconds, i;
	char	*s;
/*
 * If the radar is disabled, get out now
 */
	if (! Rad[r].enabled)
	{
		sprintf (DisplayString, "** DISABLED **");
		return;
	}
/*
 * Generate the scan for this radar, using the time from the slow radar
 * and resolution based on the option number
 */
	if (r != Slowrad)
	{
		scantime = Rad[Slowrad].scantime;
		hres = Hres + Opt * RES_STEP;
		vres = Vres + Opt * RES_STEP;
		GenScan (Rad + r, scantime, hres, vres, TRUE);
	}
/*
 * Write the info into DisplayString
 */
	minutes = (int)(Rad[r].scantime + 0.5) / 60;
	seconds = (int)(Rad[r].scantime + 0.5) % 60;

	sprintf (DisplayString, "Nsweeps: %d  Scan Rate: %.1f  Hits: %d\n", 
		Rad[r].nsweeps, Rad[r].scanrate, Rad[r].hits);

	s = DisplayString + strlen (DisplayString);
	sprintf (s, "H Res.: %.2f   V Res.: %.2f\n", Rad[r].res_horiz, 
		Rad[r].res_vert);

	s = DisplayString + strlen (DisplayString);
	sprintf (s, "Scan Time: %d:%02d\n", minutes, seconds);

	s = DisplayString + strlen (DisplayString);
	sprintf (s, "Left Azimuth: %.1f   Right Azimuth: %.1f\n",
		Rad[r].az_left, Rad[r].az_right);

	s = DisplayString + strlen (DisplayString);
	sprintf (s, "Bottom Elevation: %.1f   Top Elevation: %.1f\n",
		Rad[r].el_bottom, Rad[r].el_top);

	s = DisplayString + strlen (DisplayString);
	sprintf (s, "Angle List: ");

	for (i = 0; i < Rad[r].nsweeps; i++)
	{
		s = DisplayString + strlen (DisplayString);

		if (! (i % 8))
			sprintf (s++, "\n");

		sprintf (s, "%.1f ", Rad[r].anglist[i]);
	}

	s = DisplayString + strlen (DisplayString);
	sprintf (s, "\n");
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
/*
 * A frame to hold the whole thing
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 3); n++;
	XtSetArg (args[n], XtNresizable, True); n++;
	WScanOpts = form = XtCreateManagedWidget ("optionForm", 
		formWidgetClass, parent, args, n);
/*
 * Label widget for limiting radar
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNresizable, True); n++;
	XtSetArg (args[n], XtNresize, True); n++;
	w = WLimitRadar = XtCreateManagedWidget ("limRadar", labelWidgetClass,
		form, args, n);
/*
 * Label widget for header stuff
 */
	w_above = w;

	n = 0;
	XtSetArg (args[n], XtNfromVert, w_above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNlabel, OptionHeader); n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft); n++;
	w = XtCreateManagedWidget ("header", labelWidgetClass, form, args, n);
/*
 * A command widget for each option line
 */
	w_above = w;

	for (i = -3; i < 4; i++)
	{
		sprintf (name, "option%d", i);

		n = 0;
		XtSetArg (args[n], XtNborderWidth, 0); n++;
		XtSetArg (args[n], XtNfromVert, w_above); n++;
		XtSetArg (args[n], XtNresizable, True); n++;
		XtSetArg (args[n], XtNresize, True); n++;
		w = WOpt[i+3] = XtCreateManagedWidget (name, 
			commandWidgetClass, form, args, n);

		XtAddCallback (w, XtNcallback, so_DisplayCallback, i);

		w_above = w;
	}
/*
 * Acknowledge that we have a widget now and return it
 */
	UseWidget = TRUE;

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
	w = wbuttons = LeftRightButtons (form, so_ChangeRadar);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNwidth, 100); n++;
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
	XtSetArg (args[n], XtNwidth, 100); n++;
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
	XtAddCallback (w, XtNcallback, so_RemoveDisplay, 0);
/*
 * AsciiText widget to hold the scan info
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, wbuttons); n++;
	XtSetArg (args[n], XtNwidth, 350); n++;
	XtSetArg (args[n], XtNheight, 150); n++;
	XtSetArg (args[n], XtNdisplayCaret, False); n++;
	XtSetArg (args[n], XtNscrollHorizontal, XawtextScrollWhenNeeded); n++;
	XtSetArg (args[n], XtNscrollVertical, XawtextScrollWhenNeeded); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, STRSIZE); n++;
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
	struct ui_command	cmd;

	cmd.uc_ctype = UTT_VALUE;
	cmd.uc_vptype = SYMT_INT;
	cmd.uc_v.us_v_int = option;

	if (option < MinOpt || option > MaxOpt)
		ui_printf ("\007");
	else
		so_Display (&cmd);
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
	if (DisplayString[0] != '\0')
	{
		so_DisplayRadar (DispRadar);
		XtSetArg (arg, XtNstring, DisplayString);
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
