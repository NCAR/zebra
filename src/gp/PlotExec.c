/*
 * Plot execution module
 */
static char *rcsid = "$Id: PlotExec.c,v 1.1 1990-05-07 16:08:08 corbet Exp $";

# include <X11/Intrinsic.h>
# include <ui.h>
# include <defs.h>
# include <pd.h>
# include "GraphProc.h"
# include "/rdss/include/fields.h"
# include "/rdss/include/mda.h"
# include "/rdss/include/ui_date.h"
# include "../graphproc/DrawText.h"
# include "../graphproc/PixelCoord.h"

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
# define N_PTYPES	1	/* Increase this as plot types are added */

name_to_num Pt_table[] =
{
	{"CAP",	PT_CAP	},
	{NULL,	0	}
};

/*
 * Plot types and a string <--> representation type table
 */
# define RT_FCONTOUR	0
# define RT_VECTOR	1
# define RT_RASTER	2
# define N_RTYPES	3	/* Increase this as rep. types are added */

name_to_num Rt_table[] = 
{
	{"filled-contour",	RT_FCONTOUR	},
	{"vector",		RT_VECTOR	},
	{"raster",		RT_RASTER	},
	{NULL,			0		}
};

/*
 * Two dimensional table (plot type vs. plot representation)
 * of plotting routines and a boolean to record if it's been initialized
 */
static void	(*Plot_routines[N_PTYPES][N_RTYPES])();
static bool	Table_built = FALSE;

/*
 * Forward declarations
 */
int	px_NameToNumber ();
void	px_TopAnnot (), px_ResetAnnot (), px_AnnotLimits (), px_Init ();
void	px_AddComponent (), px_UpdateComponent ();
void	px_CAPFContour (), px_CAPVector (), px_CAPRaster ();


/*
 * Top annotation stuff
 */
static int	Annot_xpos, Annot_ypos;
static int	Annot_height;
static int	Annot_lmargin = 10, Annot_rmargin;

/*
 * How many plot components in our plot description and which
 * component are we currently dealing with?
 */
static int	Ncomps;
static int	Comp_index;

/*
 * What's white?
 */
static Pixel	White;

/*
 * Stuff for px_GetGrid (the phony data store)
 * Macro to reference the grid two-dimensionally
 */
# define GRID(i,j)	grid[(i)*ydim + (j)]
# define BADVAL	-999.0



void
px_PlotExec (component)
char	*component;
/*
 * Execute the given component of the plot description
 */
{
	char	**comps, datestring[40];
	int	i;
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
 * Global or update plot?
 */
	if (strcmp (component, "global") == 0)
	{
	/*
	 * Global plot, get the component list
	 */
		comps = pd_CompList (Pd);
	/*
	 * Annotate with the date and time
	 */
		px_ResetAnnot ();
		ud_format_date (datestring, (date *)(&PlotTime), UDF_FULL);
		strcat (datestring, "  ");
		px_TopAnnot (datestring, White);
	/*
	 * Count the components (it looks like the count comes out low
	 * by one, but it's right because we don't want to count the
	 * global component)
	 */
		for (Ncomps = 0; comps[Ncomps]; Ncomps++)
			/* do nothing */;
	/*
	 * Run through the plot components (start at 1 to skip the
	 * global component)
	 */
		for (i = 1; comps[i]; i++)
		{
			Comp_index = i - 1;
			px_AddComponent (comps[i]);
		}
	}
	else
	{
	/*
	 * Update plot
	 */
		px_UpdateComponent (comps[i]);
	}
}




void
px_AddComponent (c, index)
char	*c;
int	index;
/*
 * Add the given component to the current plot.  
 * Index should be this component's position in the plot description
 * (The first component after "global" is index 0)
 */
{
	int	ptype, rtype, i;
	char	plt[30], rep[30];
/*
 * Get the plot type from the plot description
 */
	if (! pd_Retrieve (Pd, "global", "plot-type", plt, SYMT_STRING))
		msg_log ("Missing 'plot-type' in plot description");

	ptype = px_NameToNumber (plt, Pt_table);
/*
 * Get the representation type
 */
	if (! pd_Retrieve (Pd, c, "representation", rep, SYMT_STRING))
		msg_log ("Missing 'representation' in component %s", c);
	rtype = px_NameToNumber (rep, Rt_table);
/*
 * Execute the appropriate plot table entry
 */
	if (Plot_routines[ptype][rtype] != NULL)
	{
		msg_log ("Drawing %s plot of type %s", rep, plt);
		(*Plot_routines[ptype][rtype])(c, FALSE);
	}
	else
		msg_log ("Cannot make a %s plot of type %s", rep, plt);
}




void
px_UpdateComponent (c)
char	*c;
/*
 * Update the given component
 */
{
	int	ptype, rtype, i;
	char	plt[30], rep[30];
/*
 * Get the plot type from the plot description
 */
	if (! pd_Retrieve (Pd, "global", "plot-type", plt, SYMT_STRING))
		msg_log ("Missing 'plot-type' in plot description");

	ptype = px_NameToNumber (plt, Pt_table);
/*
 * Get the representation type
 */
	if (! pd_Retrieve (Pd, c, "representation", rep, SYMT_STRING))
		msg_log ("Missing 'representation' in component %s", c);
	rtype = px_NameToNumber (rep, Rt_table);
/*
 * Execute the appropriate plot table entry, with update set to TRUE
 */
	if (Plot_routines[ptype][rtype] != NULL)
		(*Plot_routines[ptype][rtype])(c, TRUE);
	else
		msg_log ("Cannot update a %s plot of type %s", rep, plt);
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
	Plot_routines[PT_CAP][RT_VECTOR] = px_CAPVector;
	Plot_routines[PT_CAP][RT_RASTER] = px_CAPRaster;
}




void
px_ResetAnnot ()
/*
 * Reset the annotation position
 */
{
	XGCValues	gcvals;
/*
 * Get the GC, if necessary
 */
	if (Agc == NULL)
	{
		gcvals.font = Afontstruct->fid;
		Agc = XCreateGC (XtDisplay (Graphics), XtWindow (Graphics), 
			GCFont, &gcvals);
	}
/*
 * Save the text height
 */
	Annot_height = (int)(1.2 * 
		(Afontstruct->ascent + Afontstruct->descent));
/*
 * Set the right margin
 */
	Annot_rmargin = GWWidth (Graphics) - Annot_lmargin;
/*
 * Set the initial text position
 */
	Annot_xpos = Annot_lmargin;
	Annot_ypos = Afontstruct->ascent;
}




void
px_TopAnnot (string, color)
char	*string;
Pixel	color;
/*
 * Add the string to the top annotation using the given color
 */
{
	int		i, brk, slen, wlen, swidth;
	char		*temp;

	slen = strlen (string);
/*
 * If we're at the left margin, strip leading spaces
 */
	while ((Annot_xpos == Annot_lmargin) && (string[0] == ' '))
	{
		string++;
		slen--;
	}
/*
 * Just return for zero length string
 */
	if (slen == 0)
		return;
/*
 * Handle newlines in the text
 */
	for (i = 0; i < slen; i++)
		if (string[i] == '\n')
		{
		/*
		 * Copy the string up to the newline into a temporary
		 * string, and TopAnnot it
		 */
			temp = (char *) malloc ((i + 1) * sizeof (char));
			strncpy (temp, string, i);
			temp[i] = '\0';
			px_TopAnnot (temp, color);
			free (temp);
		/*
		 * Move the annotation location to start a new line
		 * and call px_TopAnnot for the remainder of the string
		 */
			Annot_ypos += Annot_height;
			Annot_xpos = Annot_lmargin;
			px_TopAnnot (string + i + 1, color);
		/*
		 * We're done
		 */
			return;
		}
/*
 * Make sure the string will fit on the current line.  Break at
 * a space if necessary
 */
	swidth = XTextWidth (Afontstruct, string, slen);
	brk = slen;

	while (swidth > Annot_rmargin - Annot_xpos)
	{
		for (brk-- ;string[brk] != ' ' && brk > 0; brk--)
			/* backing up to a space */;

		swidth = XTextWidth (Afontstruct, string, brk);
	}
/*
 * If we're at the left margin and the break position is at zero, 
 * we have nowhere to break this string.  Just print what we can and
 * log an error.
 */
	if (brk == 0 && Annot_xpos == Annot_lmargin)
	{
		brk = slen;
		msg_log ("px_TopAnnot could not break annotation '%s'", 
			string);
	}
/*
 * Draw the string up to the break, if any
 */
	XSetForeground (XtDisplay (Graphics), Agc, color);
	XDrawString (XtDisplay (Graphics), GWFrame (Graphics), Agc, 
		Annot_xpos, Annot_ypos, string, brk);
	Annot_xpos += swidth;
/*
 * If we have to break, move down to the next line and call TopAnnot with
 * the remainder of the string
 */
	if (brk < slen - 1)
	{
		Annot_ypos += Annot_height;
		Annot_xpos = Annot_lmargin;
		px_TopAnnot (string + brk, color);
	}
}




void
px_AnnotLimits (top, bottom, left, right)
int	*top, *bottom, *left, *right;
/*
 * Return the pixel limits for the right side annotation area for
 * the current plot component
 */
{
	int	height = GWHeight (Graphics);
	int	width = GWWidth (Graphics);

	*top = 0.15 * height + 
		0.8 * height * (float) Comp_index / (float) Ncomps;
	*bottom = *top + 0.8 * height / (float) Ncomps;

	*left = 0.85 * width;
	*right = width;
}




void
px_CAPFContour (comp, update)
char	*comp;
bool	update;
/*
 * Execute a CAP filled contour plot, based on the given plot
 * description, specified component, and plot time
 */
{
	field	fnum;
	char	name[20], string[10];
	int	xdim, ydim;
	float	*grid;
	float	center, step;
	Pixel	color;
	int	center_index = 5, ncolors = 9;
	float	bar_height, cval;
	int	top, bottom, left, right, i;
	GC	gc;
	XGCValues	gcvals;
/*
 * Get the field, center value, and step value
 */
	if (! pd_Retrieve (Pd, comp, "field", name, SYMT_STRING))
	{
		msg_log ("Missing field entry in px_CAPFContour!");
		return;
	}

	fnum = fld_number (name);

	if (! pd_Retrieve (Pd, comp, "centerval", (char *)(&center), 
		SYMT_FLOAT))
	{
		msg_log ("Missing centerval entry in px_CAPFContour!");
		return;
	}

	if (! pd_Retrieve (Pd, comp, "stepval", (char *)(&step), SYMT_FLOAT))
	{
		msg_log ("Missing stepval entry in px_CAPFContour!");
		return;
	}
/*
 * Allocate the data array
 */
	xdim = 16;
	ydim = 16;

	grid = (float *) malloc (xdim * ydim * sizeof (float));
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	px_GetGrid (&PlotTime, fnum, grid, xdim, ydim);
/*
 * Draw the contours
 */
	FC_Init (center_index, ncolors, 0, Colors, TRUE, BADVAL);
	FillContour (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
		0.05, 0.05, 0.85, 0.85, center, step);
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
	px_TopAnnot ("Filled contour plot of ", White);
	px_TopAnnot (name, White);
	px_TopAnnot (".  ", White);
/*
 * Side annotation (color bar)
 */
	px_AnnotLimits (&top, &bottom, &left, &right);

	bar_height = (bottom - top - 10) / (float) ncolors;

	for (i = 0; i <= ncolors; i++)
	{
	/*
	 * Draw a color rectangle
	 */
		if (i < ncolors)
		{
			color = Colors[center_index - ncolors / 2 + i].pixel;
			XSetForeground (XtDisplay (Graphics), Agc, color);
			XFillRectangle (XtDisplay (Graphics), 
				GWFrame (Graphics), Agc, left, 
				(int)(top + i * bar_height), 10, 
				(int) bar_height);
		}
	/*
	 * Numeric label
	 */
		cval = center + (i - ncolors / 2) * step;
		sprintf (string, "%.1f", cval);
		XSetForeground (XtDisplay (Graphics), Agc, White);
		DrawText (Graphics, GWFrame (Graphics), Agc, left + 15, 
			(int)(top + i * bar_height), string, 0.0, 0.02, 
			JustifyLeft, JustifyCenter);
	}
}




void
px_CAPVector (comp, update)
char	*comp;
bool	update;
/*
 * Execute a CAP vector plot, based on the given plot
 * description, specified component, and plot time
 */
{
	field	ufld, vfld;
	char	uname[20], vname[20];
	int	xdim, ydim;
	float	*ugrid, *vgrid;
	float	vscale;
	int	top, bottom, left, right;
/*
 * Get the u field
 */
	if (! pd_Retrieve (Pd, comp, "u-field", uname, SYMT_STRING))
	{
		msg_log ("Missing u-field entry in px_CAPVector!");
		return;
	}

	ufld = fld_number (uname);
/*
 * Get the v field
 */
	if (! pd_Retrieve (Pd, comp, "v-field", vname, SYMT_STRING))
	{
		msg_log ("Missing v-field entry in px_CAPVector!");
		return;
	}

	vfld = fld_number (vname);
/*
 * Get the scale value
 */
	if (! pd_Retrieve (Pd, comp, "vectorscale", (char *)(&vscale), 
		SYMT_FLOAT))
	{
		msg_log ("Missing vectorscale entry in px_CAPVector!");
		return;
	}
/*
 * Allocate the data arrays
 */
	xdim = 16;
	ydim = 16;

	ugrid = (float *) malloc (xdim * ydim * sizeof (float));
	vgrid = (float *) malloc (xdim * ydim * sizeof (float));
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	px_GetGrid (&PlotTime, ufld, ugrid, xdim, ydim);
	px_GetGrid (&PlotTime, vfld, vgrid, xdim, ydim);
/*
 * Draw the vectors
 */
	VectorGrid (Graphics, GWFrame (Graphics), ugrid, vgrid, xdim, ydim, 
		0.05, 0.05, 0.85, 0.85, vscale, BADVAL, Colors[0]);
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
	px_TopAnnot ("Vector plot of ", White);
	px_TopAnnot (uname, White);
	px_TopAnnot (" vs. ", White);
	px_TopAnnot (vname, White);
	px_TopAnnot (".  ", White);
/*
 * Side annotation (scale vector)
 */
	px_AnnotLimits (&top, &bottom, &left, &right);

	VG_AnnotVector (left + 5, top + 15, 5.0, 0.0, White);

	XSetForeground (XtDisplay (Graphics), Agc, White);
	DrawText (Graphics, GWFrame (Graphics), Agc, left + 5, top + 20,
		"5.0", 0.0, 0.02, JustifyLeft, JustifyTop);
}




void
px_CAPRaster (comp, update)
char	*comp;
bool	update;
/*
 * Execute a CAP raster plot, based on the given plot
 * description, specified component, and plot time
 */
{
	field	fnum;
	char	name[20], string[10];
	int	xdim, ydim;
	int	top, bottom, left, right, i;
	int	ncolors = 9;
	float	*grid;
	float	min, max, bar_height, val, frac;
	Pixel	color;
/*
 * Get the field and the min and max values to display
 */
	if (! pd_Retrieve (Pd, comp, "field", name, SYMT_STRING))
	{
		msg_log ("Missing field entry in px_CAPRaster!");
		return;
	}

	fnum = fld_number (name);

	if (! pd_Retrieve (Pd, comp, "minval", (char *)(&min), SYMT_FLOAT))
	{
		msg_log ("Missing minval entry in px_CAPRaster!");
		return;
	}

	if (! pd_Retrieve (Pd, comp, "maxval", (char *)(&max), SYMT_FLOAT))
	{
		msg_log ("Missing maxval entry in px_CAPRaster!");
		return;
	}
/*
 * Allocate the data array
 */
	xdim = 16;
	ydim = 16;

	grid = (float *) malloc (xdim * ydim * sizeof (float));
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	px_GetGrid (&PlotTime, fnum, grid, xdim, ydim);
/*
 * Draw the raster plot
 */
	RP_Init (Colors + 1, ncolors, -1, min, max);
	RasterPlot (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
		0.05, 0.05, 0.85, 0.85);
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
	px_TopAnnot ("Raster plot of ", White);
	px_TopAnnot (name, White);
	px_TopAnnot (".  ", White);
/*
 * Side annotation (color bar)
 */
	px_AnnotLimits (&top, &bottom, &left, &right);

	bottom -= 5;
	top += 5;

	bar_height = (bottom - top) / (float) ncolors;

	for (i = 0; i < ncolors; i++)
	{
	/*
	 * Draw a color rectangle
	 */
		color = Colors[i+1].pixel;
		XSetForeground (XtDisplay (Graphics), Agc, color);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			Agc, left, (int)(top + i * bar_height), 10, 
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
		XSetForeground (XtDisplay (Graphics), Agc, White);
		DrawText (Graphics, GWFrame (Graphics), Agc, left + 15, 
			(int)(top + frac * (bottom - top)), string, 0.0, 0.02, 
			JustifyLeft, JustifyCenter);
	}
}




px_GetGrid (plot_time, fnum, grid, xdim, ydim)
time	*plot_time;
field	fnum;
float	*grid;
int	xdim, ydim;
{
	static int	initialized = FALSE;
	static int	nsta;
	static float	xpos[50], ypos[50], rawdata[50];
	static struct dstream	ds[50];
	static int	slist[50];
	float		lonmin = 0.0, lonmax = -180.0;
	float		latmin = 90.0, latmax = 0.0;
	float		width, height, val;
	float		*lat, *lon;
	float		*sval, *spos;
	int		elev, ok, ngood;
	short		i, j, ix, iy, bigdim;
	float		spline_eval ();
	void		spline ();
/*
 * Initialize if necessary
 */
	if (! initialized)
	{
	/*
	 * Declare the wisp database
	 */
		mda_declare_file ("/data/ppf", MDA_TYPE_DATABASE, MDA_F_PACKET,
			"ppf", "wisp");
	/*
	 * Get a list of all stations
	 */
		mda_do_init (plot_time->ds_yymmdd, plot_time->ds_hhmmss);
		sta_g_slist (slist, &nsta);
	/*
	 * Find the station positions and the (lat,lon) bounding box
	 */
		lat = (float *) malloc (nsta * sizeof (float));
		lon = (float *) malloc (nsta * sizeof (float));

		for (i = 0; i < nsta; i++)
		{
			sta_g_position (slist[i], &lat[i], &lon[i], &elev);

			lon[i] *= -1.0;

			if (lon[i] < lonmin)
				lonmin = lon[i];
			if (lon[i] > lonmax)
				lonmax = lon[i];
			if (lat[i] < latmin)
				latmin = lat[i];
			if (lat[i] > latmax)
				latmax = lat[i];
		}
	/*
	 * Convert the (lat,lon) positions to 0.0-1.0 positions 
	 * within the bounding box
	 */
		width = lonmax - lonmin;
		height = latmax - latmin;

		for (i = 0; i < nsta; i++)
		{
			xpos[i] = (lon[i] - lonmin) / width;
			ypos[i] = (lat[i] - latmin) / height;
		}

		free (lat);
		free (lon);
	/*
	 * Build the dstream structures
	 */
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
 * Put the chosen field into the dstream structures
 */
	for (i = 0; i < nsta; i++)
		ds[i].ds_field = fnum;
/*
 * Get the data using MDA
 */
	mda_fetch (nsta, ds, plot_time, plot_time, BADVAL, 0);
/*
 * Fill the grid with bad value flags
 */
	for (i = 0; i < xdim; i++)
		for (j = 0; j < ydim; j++)
			GRID (i,j) = BADVAL;
/*
 * Assign each mesonet datum to the closest point in the grid
 */
	for (i = 0; i < nsta; i++)
	{
		ix = (short)(xpos[i] * (xdim - 1) + 0.5);
		iy = (short)(ypos[i] * (ydim - 1) + 0.5);

		GRID (ix,iy) = rawdata[i];
	}
/*
 * We have the "raw" data in the array, now apply splines horizontally
 * to fill in missing data areas
 */
	bigdim = (xdim > ydim) ? xdim : ydim;

	sval = (float *) malloc (bigdim * sizeof (float));
	spos = (float *) malloc (bigdim * sizeof (float));

	for (j = 0; j < ydim; j++)
	{
		ngood = 0;
	/*
	 * Build vectors of good data points and their positions in this row
	 */
		for (i = 0; i < xdim; i++)
		{
			if (GRID (i,j) != BADVAL)
			{
				sval[ngood] = GRID (i,j);
				spos[ngood] = (float) i;
				ngood++;
			}
		}
	/*
	 * Don't do the spline if we don't have enough points
	 */
		if (ngood < 2)
			continue;
	/*
	 * Do the cubic spline fit for this row
	 */
		spline (spos, sval, ngood);
	/*
	 * Evaluate the spline at the bad value points in this row
	 */
		for (i = 0; i < xdim; i++)
		{
			if (GRID (i,j) == BADVAL)
			{
				val = spline_eval ((float) i, &ok);
				if (ok)
					GRID (i,j) = val;
			}
		}
	}
/*
 * Repeat the above steps to interpolate vertically
 */
	for (i = 0; i < xdim; i++)
	{
		ngood = 0;
	/*
	 * Build vectors of good data points and their positions in this column
	 */
		for (j = 0; j < ydim; j++)
		{
			if (GRID (i,j) != BADVAL)
			{
				sval[ngood] = GRID (i,j);
				spos[ngood] = (float) j;
				ngood++;
			}
		}
	/*
	 * Don't do the spline if we don't have enough points
	 */
		if (ngood < 2)
			continue;
	/*
	 * Do the cubic spline fit for this column
	 */
		spline (spos, sval, ngood);
	/*
	 * Evaluate the spline at the bad value points in this row
	 */
		for (j = 0; j < ydim; j++)
		{
			if (GRID (i,j) == BADVAL)
			{
				val = spline_eval ((float) j, &ok);
				if (ok)
					GRID (i,j) = val;
			}
		}
	}
/*
 * Horizontally again
 */
	for (j = 0; j < ydim; j++)
	{
		ngood = 0;
	/*
	 * Build vectors of good data points and their positions in this row
	 */
		for (i = 0; i < xdim; i++)
		{
			if (GRID (i,j) != BADVAL)
			{
				sval[ngood] = GRID (i,j);
				spos[ngood] = (float) i;
				ngood++;
			}
		}
	/*
	 * Don't do the spline if we don't have enough points
	 */
		if (ngood < 2)
			continue;
	/*
	 * Do the cubic spline fit for this row
	 */
		spline (spos, sval, ngood);
	/*
	 * Evaluate the spline at the bad value points in this row
	 */
		for (i = 0; i < xdim; i++)
		{
			if (GRID (i,j) == BADVAL)
			{
				val = spline_eval ((float) i, &ok);
				if (ok)
					GRID (i,j) = val;
			}
		}
	}
/*
 * Vertically again
 */
	for (i = 0; i < xdim; i++)
	{
		ngood = 0;
	/*
	 * Build vectors of good data points and their positions in this column
	 */
		for (j = 0; j < ydim; j++)
		{
			if (GRID (i,j) != BADVAL)
			{
				sval[ngood] = GRID (i,j);
				spos[ngood] = (float) j;
				ngood++;
			}
		}
	/*
	 * Don't do the spline if we don't have enough points
	 */
		if (ngood < 2)
			continue;
	/*
	 * Do the cubic spline fit for this column
	 */
		spline (spos, sval, ngood);
	/*
	 * Evaluate the spline at the bad value points in this row
	 */
		for (j = 0; j < ydim; j++)
		{
			if (GRID (i,j) == BADVAL)
			{
				val = spline_eval ((float) j, &ok);
				if (ok)
					GRID (i,j) = val;
			}
		}
	}
/*
 * Free allocated space we don't need to keep
 */
	free (sval);
	free (spos);
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
                                                                                 