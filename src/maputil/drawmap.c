/*
 * Use the graphics tablet to digitize a map for use in robot et al.
 */

# include <math.h>
# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <GraphicsW.h>
# include <ui.h>
# include <ui_error.h>

# define PI	3.141592654
# define BADPOINT_NOISE	"cat /rdss/audio/misc/drip.au > /dev/audio"

# define GHEIGHT	559
# define GWIDTH		864

/*
 * point structure
 */
typedef struct
{
	float	x, y;
} point;

/*
 * Other declarations
 */
static float	SouthLat, NorthLat, WestLon, EastLon, Angle;
static float	Xref, Yref, BaseAng, BaseWidth;
static float	LatScale, LonScale;
static float	LonAdjust;
static FILE	*Mfile;

/*
 * prototypes
 */
void	DoLine (int, int);
void	ScaleInfo (point, point, point, point);
void	xy_to_latlon (point, float *, float *);

/*
 * Our application context and graphics context
 */
XtAppContext	Appc;
GC		Gcontext, Erasegc;
Widget		Graphics;

/*
 * Default resources
 */
static String Resources[] = {
	" *foreground:	white",
	" *font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	0,
};



main (argc, argv)
int	argc;
char	**argv;
{
	char	reply[40], fname[40], device[40];
	bool	done;
	int	button, x, y, nargs, x_tab, y_tab;
	point	ll, lr, ul, ur;
	Widget	top;
	Arg	args[10];
	SValue	value;


ERRORCATCH
/*
 * Initialize UI
 */
	ui_init (NULL, TRUE, TRUE);
/*
 * Disable pagination by ui_printf
 */
	value.us_v_int = FALSE;
	usy_s_symbol (usy_g_stbl ("ui$variable_table"), "ui$pager_mode", 
		SYMT_BOOL, &value);
/*
 * Get the map file name
 */
	ui_string_prompt ("Enter the map file name", NULL, reply, NULL);

	fixdir_t ("", ".", reply, fname, ".map");

	if (access (fname, 0) == 0)
		ui_warning ("Appending to existing file '%s'", fname);

	Mfile = fopen (fname, "a+");
/*
 * Initialize the tablet
 */
	if (! tab_init ())
		ui_error ("Unable to initialize tablet!");
/*
 * Initialize the graphics
 */
	top = XtAppInitialize (&Appc, "DrawMap", NULL, 0, &argc, argv, 
		Resources, NULL, 0);

	nargs = 0;
	XtSetArg (args[nargs], XtNheight, GHEIGHT);	nargs++;
	XtSetArg (args[nargs], XtNwidth, GWIDTH); nargs++;
	XtSetArg (args[nargs], XtNframeCount, 1); nargs++;
	Graphics = XtCreateManagedWidget ("map", graphicsWidgetClass, top, 
		args, nargs);

	XtRealizeWidget (top);
	XSync (XtDisplay (top), False);

	Gcontext = XCreateGC (XtDisplay (top), XtWindow (top), 0, NULL);
	XSetForeground (XtDisplay (top), Gcontext, 
		WhitePixelOfScreen (XtScreen (top)));

	Erasegc = XCreateGC (XtDisplay (top), XtWindow (top), 0, NULL);
	XSetForeground (XtDisplay (top), Erasegc, 
		BlackPixelOfScreen (XtScreen (top)));
/*
 * Get the points of the trapezoid
 */
	ui_printf ("\nEnter the corner coordinates of the\n");
	ui_printf ("reference trapezoid\n");
	ui_printf ("\nNOTE: West longitudes should be negative\n\n");

	SouthLat = ui_float_prompt ("South latitude", NULL, -90.0, 90.0, 0.0);
	NorthLat = ui_float_prompt ("North latitude", NULL, -90.0, 90.0, 0.0);

	WestLon = ui_float_prompt ("West longitude", NULL, -180.0, 180.0, 0.0);
	EastLon = ui_float_prompt ("East longitude", NULL, -180.0, 180.0, 0.0);

	ui_printf ("	Click tablet pointer on lower left reference point:");
	tab_point (&button, &x_tab, &y_tab);
	ui_printf ("\n");

	ll.x = (float) x_tab;	ll.y = (float) y_tab;


	ui_printf ("	                        lower right reference point:");
	tab_point (&button, &x_tab, &y_tab);
	ui_printf ("\n");

	lr.x = (float) x_tab;	lr.y = (float) y_tab;

	ui_printf ("	                        upper right reference point:");
	tab_point (&button, &x_tab, &y_tab);
	ui_printf ("\n");

	ur.x = (float) x_tab;	ur.y = (float) y_tab;

	ui_printf ("	                        upper left reference point:");
	tab_point (&button, &x_tab, &y_tab);
	ui_printf ("\n");

	ul.x = (float) x_tab;	ul.y = (float) y_tab;
/*
 * Calculate the scaling info
 */
	ScaleInfo (ll, lr, ur, ul);
/*
 * Loop to get user input
 */
	done = FALSE;

	while (! done)
	{
		ui_printf ("\n");
		ui_printf ("Click the yellow button to start a line.\n");
		ui_printf ("Click the blue button to exit.\n");

		tab_point (&button, &x, &y);

		switch (button)
		{
		    case 1:
			DoLine (x, y);
			break;
		    case 4:
			done = TRUE;
			break;
		    default:
			system (BADPOINT_NOISE);
			ui_warning ("Bad point! Try again");
		}

	}

	ui_finish ();
ON_ERROR
	ui_finish ();
ENDCATCH
}




void
DoLine (x0, y0)
int	x0, y0;
/*
 * Draw a line starting from (x0,y0)
 */
{
	float	minlat, maxlat, minlon, maxlon;
	int	npts, i, x, y, button;
	float	lat[500], lon[500];
	point	pts[500];
	bool	done;
/*
 * Sanity check on position
 */
	if (x0 < 0 || x0 > 4317 || y0 < 0 || y0 > 2793)
	{
		system (BADPOINT_NOISE);
		ui_warning ("Bad point! Try again.");
		return;
	}
/*
 * Instructions
 */
	ui_printf ("\n LINE DRAWING:\n");
	ui_printf ("\tClick the yellow button to continue the line.\n");
	ui_printf ("\tClick the white button to delete a point.\n");
	ui_printf ("\tClick the blue button to finish the line.\n");
/*
 * Store the first point
 */
startline:
	pts[0].x = (float) x0;
	pts[0].y = (float) y0;
	xy_to_latlon (pts[0], &lat[0], &lon[0]);
	npts = 1;

	minlon = maxlon = lon[0];
	minlat = maxlat = lat[0];
/*
 * Loop to get the rest of the points
 */
	done = FALSE;

	while (! done)
	{
		tab_point (&button, &x, &y);
	/*
	 * Switch on the button number
	 */
		switch (button)
		{
		/*
		 * Yellow button (1) and blue button (4)
		 */
		    case 1:
		    case 4:
		    /*
		     * Sanity check on position
		     */
			if (x < 0 || x > 4317 || y < 0 || y > 2793)
			{
				system (BADPOINT_NOISE);
				ui_warning ("Bad point! Try again.");
				continue;
			}
		    /*
		     * Draw the line segment
		     */
			XDrawLine (XtDisplay (Graphics), GWFrame (Graphics),
				Gcontext, (int) pts[npts-1].x / 5, 
				GHEIGHT - (int) pts[npts-1].y / 5, 
				x / 5, GHEIGHT - y / 5);
			GWDisplayFrame (Graphics, 0);
			XSync (XtDisplay (Graphics), False);
		    /*
		     * Add it to our point list and to our lat and lon lists
		     */
			pts[npts].x = (float) x;
			pts[npts].y = (float) y;

			xy_to_latlon (pts[npts], &lat[npts], &lon[npts]);
		    /*
		     * Increment the point count
		     */
			npts++;
		    /*
		     * Write the line if we're done or we've filled the lists
		     */
			done = (button == 4);

			if (done || npts == 500)
			{
			/*
			 * Get the lat/lon limits
			 */
				for (i = 0; i < npts; i++)
				{
					if (lat[i] > maxlat) maxlat = lat[i];
					if (lat[i] < minlat) minlat = lat[i];
					if (lon[i] > maxlon) maxlon = lon[i];
					if (lon[i] < minlon) minlon = lon[i];
				}
			/*
			 * Dump the line to the file
			 */		
				fprintf (Mfile," %3d %9.6f %9.6f %9.6f %9.6f",
					 2 * npts, maxlat, minlat, maxlon, 
					minlon);

				for (i = 0; i < npts; i++)
				{
					if (! (i % 3))
						fprintf (Mfile, "\n");
					fprintf (Mfile, " %12.6f %12.6f", 
						lat[i], lon[i]);
				}

				fprintf (Mfile, "\n");
			/*
			 * Continue the line if we're not done
			 */
				if (! done)
				{
					x0 = x;
					y0 = y;
					goto startline;
				}
			}

			break;
		/*
		 * White button (8)
		 */
		    case 8:
		    /*
		     * Delete a point
		     */
			npts--;
		    /*
		     * Special case if we're deleting the first point
		     */
			if (npts == 0)
				return;
		    /*
		     * Undraw the line segment
		     */
			XDrawLine (XtDisplay (Graphics), GWFrame (Graphics),
				Erasegc, (int) pts[npts-1].x / 5, 
				GHEIGHT - (int) pts[npts-1].y / 5, 
				(int) pts[npts].x / 5, 
				GHEIGHT - (int) pts[npts].y / 5);
			GWDisplayFrame (Graphics, 0);
			XSync (XtDisplay (Graphics), False);
			break;
		/*
		 * The other button or transmission glitch
		 */
		    default:
		    /*
		     * Bad button or tablet glitch
		     */
			system (BADPOINT_NOISE);
			ui_warning ("Bad point! Try again.");
		}
	}
}




void
ScaleInfo (ll, lr, ur, ul)
point	ll, lr, ur, ul;
/*
 * Calculate scaling information based on the points of the trapezoid
 */
{
	float	r, ang, height, topwidth;
/*
 * Set the reference point and find the angle of the base of the trapezoid
 */
	Xref = ll.x;
	Yref = ll.y;

	BaseAng = atan2 (lr.y - ll.y, lr.x - ll.x);
/*
 * Convert the points to trapezoid space, with the lower left corner of the
 * trapezoid being the origin, and the base of the trapezoid defining the 
 * x-axis
 */
	ll.x = ll.y = 0.0;

	r = hypot (lr.y - Yref, lr.x - Xref);
	lr.x = r;
	lr.y = 0.0;

	r = hypot (ur.y - Yref, ur.x - Xref);
	ang = atan2 (ur.y - Yref, ur.x - Xref) - BaseAng;
	ur.x = r * cos (ang);
	ur.y = r * sin (ang);

	r = hypot (ul.y - Yref, ul.x - Xref);
	ang = atan2 (ul.y - Yref, ul.x - Xref) - BaseAng;
	ul.x = r * cos (ang);
	ul.y = r * sin (ang);
/*
 * Trapezoid height and base width
 */
	height = ul.y;
	BaseWidth = lr.x;
/*
 * LatScale and LonScale hold the number of degrees per pixel in the
 * lat- and lon-directions on the map (for longitude, it will be degrees
 * per pixel along the base of the trapezoid)
 */
	LatScale = (NorthLat - SouthLat) / height;
	LonScale = (EastLon - WestLon) / BaseWidth;
/*
 * LonAdjust is the factor to use for longitude adjustment as we move
 * away from the base of the trapezoid
 */
	topwidth = ur.x - ul.x;
	LonAdjust = ((BaseWidth / topwidth) - 1.0) / height;
}




void
xy_to_latlon (pt, lat, lon)
point	pt;
float	*lat, *lon;
/*
 * Return the lat and lon of the given tablet point
 */
{
	float	r, ang, x, y;
/*
 * Convert the point into trapezoid space coordinates
 */
	r = hypot (pt.y - Yref, pt.x - Xref);
	ang = atan2 (pt.y - Yref, pt.x - Xref) - BaseAng;

	x = r * cos (ang);
	y = r * sin (ang);
/*
 * Latitude
 */
	*lat = SouthLat + y * LatScale;
/*
 * Longitude
 */
	*lon = 0.5 * (WestLon + EastLon) + 
		LonScale * (x - 0.5 * BaseWidth) * (1.0 + y * LonAdjust);
}
