/*
 * Plot execution module
 */
static char *rcsid = "$Id: PlotExec.c,v 1.5 1990-11-19 13:44:16 burghart Exp $";

# include <X11/Intrinsic.h>
# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <pd.h>
# include <fields.h>
# include <mda.h>
# include <ui_date.h>
# include "../include/message.h"
# include "GraphProc.h"
# include "DrawText.h"
# include "PixelCoord.h"
# include "rg_status.h"
# include "EventQueue.h"

/*
 * Macro for a pointer to x cast into a char *
 */
# define CPTR(x)	(char *)(&(x))

/*
 * Structure for building a name to number conversion table
 */
typedef struct
{
	char	*name;
	int	number;
} name_to_num;

/*
 * Plot types and a string <--> plot type table
 */
# define PT_CAP		0
# define PT_SKEWT	1
# define PT_XSECT	2
# define N_PTYPES	3	/* Increase this as plot types are added */

name_to_num Pt_table[] =
{
	{"CAP",		PT_CAP		},
	{"skewt", 	PT_SKEWT	},
	{"xsect",	PT_XSECT	},
	{NULL,		0		}
};

/*
 * Plot types and a string <--> representation type table
 */
# define RT_FCONTOUR	0
# define RT_CONTOUR	1
# define RT_VECTOR	2
# define RT_RASTER	3
# define RT_TRACK	4
# define RT_OVERLAY	5
# define RT_SKEWT	6
# define N_RTYPES	7	/* Increase this as rep. types are added */

name_to_num Rt_table[] = 
{
	{"filled-contour",	RT_FCONTOUR	},
	{"contour",		RT_CONTOUR	},
	{"line-contour",	RT_CONTOUR	},
	{"vector",		RT_VECTOR	},
	{"raster",		RT_RASTER	},
	{"track",		RT_TRACK	},
	{"overlay",		RT_OVERLAY	},
	{"skewt",		RT_SKEWT	},
	{NULL,			0		}
};

/*
 * Two dimensional table (plot type vs. plot representation)
 * of plotting routines and a boolean to record if it's been initialized
 */
static void	(*Plot_routines[N_PTYPES][N_RTYPES])();
static Boolean	Table_built = FALSE;

/*
 * Contour plot types
 */
typedef enum {LineContour, FilledContour} contour_type;

/*
 * Forward declarations
 */
# ifdef __STDC__
	float	*px_GetGrid (time *, char *, char *, int *, int *, float *, 
		float *, float *, float *);
	int	px_NameToNumber (char *, name_to_num *);
	void	px_Init ();
	void	px_AddComponent (char *, int);
	void	px_CAPFContour (char *, int);
	void	px_CAPVector (char *, int);
	void	px_CAPRaster (char *, int);
	void	px_CAPLineContour (char *, int);
	void	px_CAPContour (char *, contour_type, char *, float *, float *);
	void	px_AdjustCoords (float *, float *, float *, float *);
	void	px_FixPlotTime ();
# else
	float	*px_GetGrid ();
	int	px_NameToNumber ();
	void	px_Init (), px_AddComponent (), px_CAPFContour ();
	void	px_CAPVector (), px_CAPRaster (), px_CAPLineContour ();
	void	px_CAPContour (), px_AdjustCoords ();
	void	px_FixPlotTime ();
# endif

/*
 * Other routines.
 */
extern void	tr_CAPTrack (), ov_CAPOverlay (), sk_Skewt (), xs_XSect ();

# ifdef titan
#	define do_rgrid DO_RGRID
# else
#	define do_rgrid do_rgrid_
# endif
extern int	do_rgrid ();

/*
 * How many plot components in our plot description and which
 * component are we currently dealing with?
 */
static int	Ncomps;
static int	Comp_index;

/*
 * Color stuff
 */
static XColor	*Colors;
static int	Ncolors;
Pixel	White;

/*
 * Stuff for px_GetGrid (the phony data store)
 * Macro to reference the grid two-dimensionally
 */
/* # define GRID(i,j)	grid[(i)* (*ydim) + (j)] */
# define GRID(j,i)	grid[(i)* (*ydim) + (j)]
# define BADVAL	-32768.0
static float Melev = 0.0;	/* Mean pam elevation		*/




void
px_PlotExec (component)
char	*component;
/*
 * Execute the given component of the plot description
 */
{
	char	**comps, datestring[40], plt[30], rep[30];
	float	lat, lon;
	int	i, expand, orig_alt;
	Boolean	ok, cvt_Origin (), global;
/*
 * Check now for an abort condition
 */
	if (Abort)
	{
		msg_ELog (EF_INFO, "%d %06d %s plot aborted!",
			PlotTime.ds_yymmdd, PlotTime.ds_hhmmss, component);
		return;
	}
/*
 * Initialize the table of plot functions if necessary
 */
	if (! Table_built)
		px_Init ();
/*
 * Get the white pixel value
 */
	White = WhitePixelOfScreen (XtScreen (Graphics));
/*
 * Set the busy cursor so that people know something is going on.
 */
	XDefineCursor (XtDisplay (Top), XtWindow (Graphics), BusyCursor);
	eq_sync ();
/*
 * Get the plot type
 */
	if (!pda_ReqSearch (Pd, "global", "plot-type", NULL, plt, SYMT_STRING))
		return;

	PlotType = px_NameToNumber (plt, Pt_table);
/*
 * Get the origin and plot limits
 */
	ok = pda_ReqSearch (Pd, "global", "origin-lat", NULL, CPTR (lat), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "origin-lon", NULL, CPTR (lon), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "x-min", NULL, CPTR (Xlo), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "x-max", NULL, CPTR (Xhi), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "y-min", NULL, CPTR (Ylo), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "y-max", NULL, CPTR (Yhi), 
		SYMT_FLOAT);
	if (! ok)
		return;
/*
 * Get the altitude too.  Default it to ground level if all else fails.
 */
	if (! pda_Search (Pd, "global", "altitude", NULL, CPTR (Alt),SYMT_INT))
		Alt = 0;
	orig_alt = Alt;
/*
 * Unless told otherwise, readjust the coordinates so that x == y.
 */
	if (! pda_Search (Pd, "global", "expand", NULL, (char *) &expand,
			SYMT_BOOL) || expand == FALSE)
		px_AdjustCoords (&Xlo, &Ylo, &Xhi, &Yhi);
/*
 * Save the origin
 */
	if (! cvt_Origin (lat, lon))
		return;
/*
 * Global or update plot?
 */
	if ((global = strcmp (component, "global") == 0) &&
		PlotMode == RealTime)
	/*
	 * Semi-kludge: roll back the plot time to the last trigger incr.
	 */
		px_FixPlotTime ();
/*
 * Three possibilities:
 *
 * (1) Global plot but already in the cache.
 */
	if (global && (DisplayFrame = fc_LookupFrame (&PlotTime)) >= 0)
	{
	/*
	 * Set up to draw in this frame -- we may be returning to RT mode
	 * after a movie, and partial updates will need to go here.
	 */
		DrawFrame = DisplayFrame;
		GWDrawInFrame (Graphics, DrawFrame);
	}
/*
 * (2) Global plot not cached.
 */
	else if (global)
	{
	/*
	 * Choose the drawing frame and clear it out
	 */
		DrawFrame = fc_GetFrame ();
		GWDrawInFrame (Graphics, DrawFrame);
		GWClearFrame (Graphics, DrawFrame);
		DisplayFrame = DrawFrame;
	/*
	 * Get the component list
	 */
		comps = pd_CompList (Pd);
	/*
	 * Count the components
	 * (7/90 jc) Don't count overlay components.  The use of this count
	 * 	     currently is only to parcel out side annotation space,
	 *	     and the overlays don't want it.
	 */
	 	Ncomps = 0;
		for (i = 1; comps[i]; i++)
			if (! pd_Retrieve (Pd, comps[i], "representation",
				  rep, SYMT_STRING) || strcmp (rep, "overlay"))
				Ncomps++;
	/*
	 * Annotate with the date and time
	 */
		An_ResetAnnot (Ncomps);
		ud_format_date (datestring, (date *)(&PlotTime), UDF_FULL);
		strcat (datestring, "  ");
		An_TopAnnot (datestring, White);
	/*
	 * Run through the plot components (start at 1 to skip the
	 * global component)
	 */
		for (i = 1; comps[i]; i++)
		{
			Comp_index = i - 1;
			px_AddComponent (comps[i], False);
		}
	/*
	 * Annotate the altitude we eventually got.
	 */
		sprintf (datestring, "Alt: %dm", Alt);
		DrawText (Graphics, GWFrame (Graphics), White,
			GWWidth (Graphics) - 10, GWHeight (Graphics) - 10, 
			datestring, 0.0, TOPANNOTHEIGHT, JustifyRight,
			JustifyBottom);
	/*
	 * If the altitude has changed, stash it.
	 */
		if (Alt != orig_alt)
		{
			pd_Store (Pd, "global", "altitude", CPTR (Alt),
				SYMT_INT);
			Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
		}
	/*
	 * Add this one to the cache.
	 */
		fc_AddFrame (&PlotTime, DisplayFrame);
	}
/*
 * (3) Update plot.
 */
	else
	{
	/*
	 * Update plot and synchronize
	 */
		px_AddComponent (component, True);
	}
/*
 * Display the frame
 */
/*	DisplayFrame = DrawFrame; */
	GWDisplayFrame (Graphics, DisplayFrame);
/*
 * Put the cursor back.
 */
	XDefineCursor (XtDisplay (Top), XtWindow (Graphics), NormalCursor);
	XSync (XtDisplay (Top), False);
}




void
px_FixPlotTime ()
/*
 * Roll back the plot time to the nearest multiple of the trigger time.
 */
{
	char trigger[40];
	int itrigger, seconds;
/*
 * Find our trigger time.
 */
	if (! pda_Search (Pd, "global", "trigger", 0, trigger, SYMT_STRING))
		return;
	if (! (itrigger = pc_TimeTrigger (trigger)))
		return;	/* Will complain elsewhere */
/*
 * Now fix up the plot time.
 */
	seconds = (PlotTime.ds_hhmmss/10000)*60*60 +
		  ((PlotTime.ds_hhmmss/100) % 100)*60 +
		  (PlotTime.ds_hhmmss % 100);
	seconds -= seconds % itrigger;
	PlotTime.ds_hhmmss = (seconds/3600)*10000 + ((seconds/60) % 60)*100 +
				seconds % 60;
}





void
px_AddComponent (c, update)
char	*c;
Boolean	update;
/*
 * Add the given component to the current plot.
 * Update is true if we're appending more data to a previous plot.
 */
{
	int	rtype, disable = FALSE;
	char	rep[30];
/*
 * If this overlay is disabled, ignore it.
 */
	if (pda_Search (Pd, c, "disable", c, (char *) &disable, SYMT_BOOL) &&
		disable)
		return;
/*
 * Get the representation type
 */
	if (! pda_ReqSearch (Pd, c, "representation", NULL, rep, SYMT_STRING))
		return;

	rtype = px_NameToNumber (rep, Rt_table);
/*
 * Execute the appropriate plot table entry
 */
	if (Plot_routines[PlotType][rtype] != NULL)
		(*Plot_routines[PlotType][rtype]) (c, update);
	else
		msg_ELog (EF_PROBLEM, "Cannot make a '%s' plot of type '%s'",
			rep, PlotType);
}




void
px_Init ()
/*
 * Initialize the table of plotting routines
 */
{
	int	pt, rt;
/*
 * Fill the table with NULLs
 */
	for (pt = 0; pt < N_PTYPES; pt++)
		for (rt = 0; rt < N_RTYPES; rt++)
			Plot_routines[pt][rt] = NULL;
/*
 * Put in the entries that exist
 */
	Plot_routines[PT_CAP][RT_FCONTOUR] = px_CAPFContour;
	Plot_routines[PT_CAP][RT_CONTOUR] = px_CAPLineContour;
	Plot_routines[PT_CAP][RT_VECTOR] = px_CAPVector;
	Plot_routines[PT_CAP][RT_RASTER] = px_CAPRaster;
	Plot_routines[PT_CAP][RT_TRACK] = tr_CAPTrack;
	Plot_routines[PT_CAP][RT_OVERLAY] = ov_CAPOverlay;

	Plot_routines[PT_SKEWT][RT_SKEWT] = sk_Skewt;

	Plot_routines[PT_XSECT][RT_CONTOUR] = xs_XSect;
/*
 * Done
 */
	Table_built = TRUE;
}





void
px_AdjustCoords (x0, y0, x1, y1)
float *x0, *y0, *x1, *y1;
/*
 * Adjust these coordinates so that (1) the entire given range fits on the
 * screen, and (2) the scaling is the same in both directions.
 */
{
	int width = (F_X1 - F_X0) * GWWidth (Graphics);
	int height = (F_Y1 - F_Y0) * GWHeight (Graphics);
	float hkmp, vkmp;	/* horiz km/pixel, vert too */
/*
 * Figure out the current scales in both directions.
 */
	hkmp = (*x1 - *x0)/width;
	vkmp = (*y1 - *y0)/height;
/*
 * Find which one is greater, and adjust the other to match.
 */
	if (hkmp > vkmp)
	{
		float newv = hkmp*height;
		float incr = newv - (*y1 - *y0);
		*y0 -= incr/2;
		*y1 += incr/2;
	}
	else
	{
		float newh = vkmp*width;
		float incr = newh - (*x1 - *x0);
		*x0 -= incr/2;
		*x1 += incr/2;
	}
}




void
px_CAPFContour (c, update)
char	*c;
Boolean	update;
/*
 * Filled contour CAP plot for the given component
 */
{
	float	center, step, bar_height, cval;
	int	i, left, right, top, bottom;
	char	string[10], fname[20];
	GC	gc;
	XGCValues	gcvals;
/*
 * Use the common CAP contouring routine to do a filled contour plot
 */
	px_CAPContour (c, FilledContour, fname, &center, &step);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	An_TopAnnot ("Filled contour plot of ", White);
	An_TopAnnot (px_FldDesc (c, fname), White);
	An_TopAnnot (".  ", White);
/*
 * Side annotation (color bar)
 */
	An_AnnotLimits (&top, &bottom, &left, &right);
	left += 5;
	top += 5;
	bottom -= 5;

	bar_height = (float)(bottom - top) / (float) Ncolors;

	for (i = 0; i <= Ncolors; i++)
	{
	/*
	 * Draw a color rectangle
	 */
		if (i < Ncolors)
		{
			gcvals.foreground = Colors[i].pixel;
			gc = XtGetGC (Graphics, GCForeground, &gcvals);
			XFillRectangle (XtDisplay (Graphics), 
				GWFrame (Graphics), gc, left, 
				(int)(top + i * bar_height), 10, 
				(int)(bar_height + 1.5));
		}
	/*
	 * Numeric label
	 */
		cval = center + (i - Ncolors / 2) * step;
		sprintf (string, "%.1f", cval);
		DrawText (Graphics, GWFrame (Graphics), White, left + 15, 
			(int)(top + i * bar_height), string, 0.0, 0.02, 
			JustifyLeft, JustifyCenter);
	}
}




void
px_CAPLineContour (c, update)
char	*c;
Boolean	update;
/*
 * Line contour CAP plot for the given component
 */
{
	float	center, step, cval;
	char	fname[20], string[10];
	int	top, bottom, left, right, wheight, i;
/* 
 * Use the common CAP contouring routine to do a color line contour plot
 */
	px_CAPContour (c, LineContour, fname, &center, &step);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	An_TopAnnot ("Contour plot of ", White);
	An_TopAnnot (px_FldDesc (c, fname), White);
	An_TopAnnot (".  ", White);
/*
 * Side annotation
 */
	An_AnnotLimits (&top, &bottom, &left, &right);
	left += 10;
	top += 5;

	wheight = GWHeight (Graphics);

	for (i = 0; i <= Ncolors; i++)
	{
	/*
	 * Numeric label
	 */
		cval = center + (i - Ncolors / 2) * step;
		sprintf (string, "%.1f", cval);
		DrawText (Graphics, GWFrame (Graphics), Colors[i].pixel, 
			left, top, string, 0.0, 0.02, JustifyLeft, JustifyTop);
		top += (int)(1.2 * 0.02 * wheight);
	}
}




void
px_CAPContour (c, type, fname, center, step)
char	*c, *fname;
contour_type	type;
float	*center, *step;
/*
 * Execute a CAP contour plot, based on the given plot
 * description, specified component, and contour type.
 * Return the field name, contour center, and step from the plot
 * description.
 */
{
	field	fnum;
	char	ctname[40], platform[40];
	int	xdim, ydim;
	float	*rgrid, *grid, x0, x1, y0, y1;
	int	pix_x0, pix_x1, pix_y0, pix_y1, dolabels, linewidth;
	Boolean	ok;
	XColor	black;
	XRectangle	clip;
/*
 * Get necessary parameters from the plot description
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "contour-center", fname, (char *) center, 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "contour-step", fname, (char *) step, 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "color-table", "contour", ctname, 
		SYMT_STRING);

	if (! ok)
		return;
/*
 * Special stuff for line contours
 */
	if (! pda_Search (Pd, c, "do-labels", "contour", (char *) &dolabels,
		SYMT_BOOL))
		dolabels = TRUE;

	if (! pda_Search (Pd, c, "line-width", "contour", (char *) &linewidth,
		SYMT_INT))
		linewidth = 0;
/*
 * Get the field number and grab the color table
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);
# ifdef notdef
	fnum = fld_number (fname);
/*
 * Allocate the data array
 */
	xdim = 16;
	ydim = 16;

	grid = (float *) malloc (xdim * ydim * sizeof (float));
# endif
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	rgrid = px_GetGrid (&PlotTime, platform, fname, &xdim, &ydim, &x0, &y0,
			&x1, &y1);
	if (! rgrid)
	{
		msg_ELog (EF_PROBLEM, "Unable to get grid");
		return;
	}
/*
 * Kludge: rotate the grid into the right ordering.
 */
	grid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, grid, xdim, ydim);
	free (rgrid);
/*
 * Convert the grid limits to pixel values
 */
	pix_x0 = XPIX (x0);	pix_x1 = XPIX (x1);
	pix_y0 = YPIX (y0);	pix_y1 = YPIX (y1);
/*
 * Clip rectangle
 */
	clip.x = F_X0 * GWWidth (Graphics);
	clip.y = (1.0 - F_Y1) * GWHeight (Graphics);
	clip.width = (F_X1 - F_X0) * GWWidth (Graphics);
	clip.height = (F_Y1 - F_Y0) * GWHeight (Graphics);
/*
 * Draw the contours
 */
	ct_GetColorByName ("black", &black);

	switch (type)
	{
	    case FilledContour:
		FC_Init (Colors, Ncolors, Ncolors / 2, black, clip, TRUE, 
			BADVAL);
		FillContour (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1, *center, *step);
		break;
	    case LineContour:
		CO_Init (Colors, Ncolors, Ncolors / 2, black, clip, TRUE, 
			BADVAL);
		Contour (Graphics, GWFrame (Graphics), grid, xdim, ydim,
			pix_x0, pix_y0, pix_x1, pix_y1, *center, *step, 
			dolabels, linewidth);
		break;
	    default:
		msg_ELog (EF_PROBLEM, "BUG: bad contour plot type %d", type);
	}
/*
 * Free the data array
 */
	free (grid);
}




void
px_CAPVector (c, update)
char	*c;
Boolean	update;
/*
 * Execute a CAP vector plot, based on the given plot
 * description, specified component, and plot time
 */
{
	char	uname[20], vname[20], cname[30], platform[40], annot[120];
	int	xdim, ydim;
	float	*rgrid, *ugrid, *vgrid;
	float	vscale, x0, x1, y0, y1;
	int	pix_x0, pix_x1, pix_y0, pix_y1;
	int	top, bottom, left, right, xannot, yannot;
	Boolean	ok;
	XColor	color;
/*
 * Get necessary parameters from the plot description
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "u-field", NULL, uname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "v-field", NULL, vname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "arrow-scale", NULL, CPTR (vscale), 
		SYMT_FLOAT);
	if (! ok)
		return;
/*
 * Figure out an arrow color.
 */
	if (! pda_Search (Pd, c, "arrow-color", platform, cname, SYMT_STRING)
		&& ! pda_Search (Pd, c, "color", platform, cname, SYMT_STRING))
		strcpy (cname, "white");
/*
 * Allocate the chosen arrow color
 */
	if (! ct_GetColorByName (cname, &color))
	{
		msg_ELog (EF_PROBLEM, "Can't get arrow color '%s'!", cname);
		return;
	}
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	rgrid = px_GetGrid (&PlotTime, platform, uname, &xdim, &ydim, &x0, &y0,
		&x1, &y1);
	if (! rgrid)
	{
		msg_ELog (EF_PROBLEM, "Unable to get U grid");
		return;
	}
	ugrid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, ugrid, xdim, ydim);
	free (rgrid);

	rgrid = px_GetGrid (&PlotTime, platform, vname, &xdim, &ydim, &x0, &y0,
		&x1, &y1);
	if (! rgrid)
	{
		msg_ELog (EF_PROBLEM, "Unable to get V grid");
		return;
	}
	vgrid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, vgrid, xdim, ydim);
	free (rgrid);
/*
 * Convert the grid limits to pixel values
 */
	pix_x0 = XPIX (x0);	pix_x1 = XPIX (x1);
	pix_y0 = YPIX (y0);	pix_y1 = YPIX (y1);
/*
 * Draw the vectors
 */
	VectorGrid (Graphics, GWFrame (Graphics), ugrid, vgrid, xdim, ydim, 
		pix_x0, pix_y0, pix_x1, pix_y1, vscale, BADVAL, color);
/*
 * Free the data arrays
 */
	free (ugrid);
	free (vgrid);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	sprintf (annot, "Vector winds plot (%s).", platform);
	An_TopAnnot (annot, White);
/*
 * Side annotation (scale vectors)
 */
	An_AnnotLimits (&top, &bottom, &left, &right);

	xannot = (left + right) / 2;
	yannot = top + 0.04 * GWHeight (Graphics);
	DrawText (Graphics, GWFrame (Graphics), White, xannot, yannot, 
		"VECTOR", 0.0, 0.02, JustifyCenter, JustifyBottom);

	yannot += 0.022 * GWHeight (Graphics);
	DrawText (Graphics, GWFrame (Graphics), White, xannot, yannot, 
		"SCALE", 0.0, 0.02, JustifyCenter, JustifyBottom);

	xannot = left + 0.25 * (right - left);
	yannot += 0.03 * GWHeight (Graphics);
	DrawText (Graphics, GWFrame (Graphics), White, xannot, yannot,
		"5.0", 0.0, 0.02, JustifyCenter, JustifyBottom);
	VG_AnnotVector (xannot, yannot + 4, 0.0, -5.0, White);

	xannot = left + 0.75 * (right - left);
	DrawText (Graphics, GWFrame (Graphics), White, xannot, yannot,
		"10.0", 0.0, 0.02, JustifyCenter, JustifyBottom);
	VG_AnnotVector (xannot, yannot + 4, 0.0, -10.0, White);
}




void
px_CAPRaster (c, update)
char	*c;
Boolean	update;
/*
 * Execute a CAP raster plot, based on the given plot
 * description, specified conent, and plot time
 */
{
	char	name[20], string[10], ctname[40], platform[40];
	int	xdim, ydim;
	int	top, bottom, left, right, i, newrp, fastloop;
	Boolean	ok;
	float	*grid, x0, x1, y0, y1;
	float	min, max, bar_height, val, frac;
	int	pix_x0, pix_x1, pix_y0, pix_y1;
	XRectangle	clip;
	XColor	black;
	GC	gc;
	XGCValues	gcvals;
/*
 * Get necessary parameters from the plot description
 */
	strcpy (name, "none");
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, name, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "minval", name, CPTR (min), SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "maxval", name, CPTR (max), SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "color-table", "raster", ctname, 
		SYMT_STRING);

	if (! ok)
		return;
/*
 * Rasterization control.
 */
	if (! pda_Search (Pd, c, "new-raster", NULL, (char *) &newrp,
		SYMT_BOOL))
		newrp = TRUE;
	if (! pda_Search (Pd, c, "fast-raster", NULL, (char *) &fastloop,
		SYMT_BOOL))
		fastloop = FALSE;
/*
 * Field number and color table
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	grid = px_GetGrid (&PlotTime, platform, name, &xdim, &ydim, &x0, &y0,
		&x1, &y1);
	if (! grid)
	{
		msg_ELog (EF_INFO, "Unable to get grid for %s at %d %d",
			platform, PlotTime.ds_yymmdd, PlotTime.ds_hhmmss);
		return;
	}
/*
 * Convert the grid limits to pixel coordinates
 */
	pix_x0 = XPIX (x0);	pix_x1 = XPIX (x1);
	pix_y0 = YPIX (y0);	pix_y1 = YPIX (y1);
/*
 * Clip rectangle
 */
	clip.x = F_X0 * GWWidth (Graphics);
	clip.y = (1.0 - F_Y1) * GWHeight (Graphics);
	clip.width = (F_X1 - F_X0) * GWWidth (Graphics);
	clip.height = (F_Y1 - F_Y0) * GWHeight (Graphics);
/*
 * Draw the raster plot
 */
	ct_GetColorByName ("black", &black);
	RP_Init (Colors, Ncolors, black, clip, min, max);
	if (! newrp)
		RasterPlot (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1);
	else
		RasterXIPlot (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1, fastloop);
/*
 * Free the data array
 */
	free (grid);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	An_TopAnnot ("Raster plot of ", White);
	An_TopAnnot (px_FldDesc (c, name), White);
	An_TopAnnot (".  ", White);
/*
 * Side annotation (color bar)
 */
	An_AnnotLimits (&top, &bottom, &left, &right);

	bottom -= 5;
	top += 5;

	bar_height = (bottom - top) / (float) Ncolors;

	for (i = 0; i < Ncolors; i++)
	{
	/*
	 * Draw a color rectangle
	 */
		gcvals.foreground = Colors[i].pixel;
		gc = XtGetGC (Graphics, GCForeground, &gcvals);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			gc, left, (int)(top + i * bar_height + 1), 10, 
			(int) bar_height);
	}

	for (i = 0; i < 9; i++)
	{
	/*
	 * Numeric label
	 */
		frac = (float) i / 8.0;
		val = min + frac * (max - min);
		sprintf (string, "%.1f", val);

		DrawText (Graphics, GWFrame (Graphics), White, left + 15, 
			(int)(top + frac * (bottom - top)), string, 0.0, 0.02, 
			JustifyLeft, JustifyCenter);
	}
}




float *
px_GetGrid (plot_time, platform, fname, xdim, ydim, x0, y0, x1, y1)
time	*plot_time;
char 	*platform, *fname;
int	*xdim, *ydim;
float	*x0, *y0, *x1, *y1;
{
/*
 * The static variables below keep the information gathered when
 * we initialize.  We need to keep the stuff around between calls.
 */
	static int	initialized = FALSE;
	static int	nsta;
	static float	*rawdata;
	static struct dstream	*ds;
	static int	slist[80];
	static float	width, height;
	static float	*xsta, *ysta;
	static float	xmin, xmax, ymin, ymax;
	float		*grid;
	float		val, lat, lon, badflag;
	float		*sval, *spos, *scratch;
	int		elev, ok, ngood, fnum, status, balt = Alt;
	short		i, j, ix, iy, bigdim;
	int		RGRID ();
	float		spline_eval ();
	void		spline ();

/*
 * If this is not a request for mesonet data, go off and look for a 
 * MUDRAS file.
 */
	if (strcmp (platform, "mesonet"))
	{
		float * ret = ga_MudrasGrid(plot_time, platform, fname, &balt, 
				xdim, ydim, x0, y0, x1, y1);
		if (Comp_index == 0)
			Alt = balt;
		return (ret);
	}
		

ERRORCATCH	/* For MDA stuff */
/*
 * Initialize if necessary
 */
	if (! initialized)
	{
	/*
	 * Declare the cinde database
	 */
		mda_declare_file ("/data/ppf", MDA_TYPE_DATABASE, MDA_F_PACKET,
			"ppf", "cinde");
	/*
	 * Get a list of all stations
	 */
		mda_do_init (plot_time->ds_yymmdd, plot_time->ds_hhmmss);
		sta_g_slist (slist, &nsta);
	/*
	 * Find the station positions and the (lat,lon) bounding box
	 */
		xmin = 99999.0;	xmax = -99999.0;
		ymin = 99999.0;	ymax = -99999.0;
		Melev = 0.0;
		xsta = (float *) malloc (nsta * sizeof (float));
		ysta = (float *) malloc (nsta * sizeof (float));

		for (i = 0; i < nsta; i++)
		{
			sta_g_position (slist[i], &lat, &lon, &elev);
			lon *= -1.0;
			cvt_ToXY (lat, lon, &xsta[i], &ysta[i]);

			if (xsta[i] < xmin)
				xmin = xsta[i];
			if (xsta[i] > xmax)
				xmax = xsta[i];
			if (ysta[i] < ymin)
				ymin = ysta[i];
			if (ysta[i] > ymax)
				ymax = ysta[i];
			Melev += elev;
		}

		width = xmax - xmin;
		height = ymax - ymin;
		Melev /= nsta;
# ifdef notdef /* don't do this when using rgrid */
	/*
	 * Change the station positions to be relative to the lower
	 * left corner
	 */
		for (i = 0; i < nsta; i++)
		{
			xsta[i] -= xmin;
			ysta[i] -= ymin;
		}
# endif
	/*
	 * Build the dstream structures
	 */
		rawdata = (float *) malloc (nsta * sizeof (float));
		ds = (struct dstream *) 
			malloc (nsta * sizeof (struct dstream));

		for (i = 0; i < nsta; i++)
		{
			ds[i].ds_plat = slist[i];
			ds[i].ds_stride = 1;
			ds[i].ds_data = &(rawdata[i]);
		}
	/*
	 * Done with initialization
	 */
		initialized = TRUE;
	}
/*
 * Allocate the grid for the return data.
 */
	*xdim = *ydim = 16;
	grid = (float *) malloc ((*xdim)*(*ydim)*sizeof (float));
/*
 * Put the grid limits into the return variables
 */
	*x0 = xmin;	*x1 = xmax;
	*y0 = ymin;	*y1 = ymax;
	if (Comp_index == 0)
		Alt = Melev;
/*
 * Put the chosen field into the dstream structures
 */
	if (! (fnum = fld_number (fname)))
	{
		msg_ELog (EF_PROBLEM, "Unrecognized PAM field: %s", fname);
		return (0);
	}
	for (i = 0; i < nsta; i++)
		ds[i].ds_field = fnum | FLD_F_DEGLITCH;
/*
 * Get the data using MDA
 */
	plot_time->ds_hhmmss -= plot_time->ds_hhmmss % 100;	/* XXX */
	mda_fetch (nsta, ds, plot_time, plot_time, BADVAL, 0);
ON_ERROR
/*
 * For UI error conditions, just return an array full of bad
 * value flags
 */
	msg_ELog (EF_PROBLEM, "px_GetGrid quitting on error");

	for (i = 0; i < *xdim; i++)
		for (j = 0; j < *ydim; j++)
			GRID (i,j) = BADVAL;

	return (grid);
ENDCATCH
/*
 * Fill the grid with bad value flags
 */
	for (i = 0; i < *xdim; i++)
		for (j = 0; j < *ydim; j++)
			GRID (i,j) = BADVAL;
/*
 * Use RGRID to generate gridded data
 */
	badflag = BADVAL;
	scratch = (float *) malloc ((*xdim) * (*ydim) * sizeof (float));
	status = do_rgrid (grid, xdim, ydim, &nsta, rawdata, &badflag, 
		xsta, ysta, &xmin, &ymin, &xmax, &ymax, scratch);
	free (scratch);

	switch (status)
	{
	    case RG_OK:
		break;
	    case RG_NOTENUFPTS:
		msg_ELog (EF_PROBLEM, 
			"Not enough good points to generate a grid");
		break;
	    case RG_COLLINEAR:
		msg_ELog (EF_PROBLEM,
			"Points are collinear, unable to generate a grid");
		break;
	    default:
		msg_ELog (EF_PROBLEM,
			"Unknown status %d returned by RGRID", status);
	}

	return (grid);
}




int
px_NameToNumber (name, table)
char		*name;
name_to_num	*table;
/*
 * Return the number associated with string 'name' in 'table'
 */
{
	name_to_num	entry;

	while (TRUE)
	{
		entry = *table++;

		if (entry.name == NULL)
			return (-1);

		if (strcmp (entry.name, name) == 0)
			return (entry.number);
	}
}




char *
px_FldDesc (comp, fld)
char *comp, *fld;
/*
 * Return the description of this field.
 */
{
	static char ret[80];

	if (pda_Search (Pd, comp, "desc", fld, ret, SYMT_STRING))
		return (ret);
	return (fld);
}
                                                                                 