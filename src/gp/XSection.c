/*
 * Vertical cross-sectioning
 */
static char *rcsid = "$Id: XSection.c,v 2.9 1993-07-13 19:59:31 burghart Exp $";
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

# include <config.h>

# if C_PT_XSECT

# include <math.h>
# include <ctype.h>
# include <X11/Intrinsic.h>
# include <ui.h>
# include <ui_date.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"

/*
 * General definitions
 */
# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define RAD_TO_DEG(x)	((x) * 57.29577951)
# define MAXPLAT	 20
# define BADVAL	-999.0
# define BUFLEN	1024

/*
 * Scratch string for building annotations, etc.
 */
char	Scratch[128];

/*
 * Action to take when we hit a bad sounding
 */
# define BAD_SOUNDING	{ \
				sprintf (Scratch, " (bad)"); \
				An_TopAnnot (Scratch, White.pixel); \
				continue; \
			}

/*
 * The cross-section plane array, its length, height, and dimensions
 */
float	*Plane, *P_wgt, P_len, P_hgt, P_bot;
int	Hdim, Vdim;

/*
 * Cross-section endpoints
 */
static float	X0 = 0.0, X1 = 0.0, Y0 = 0.0, Y1 = 0.0;
static ZebTime	T0;

/*
 * Pixel limits for the plot
 */
static int	Pix_left, Pix_right, Pix_bottom, Pix_top;

/*
 * Altitude or pressure on the vertical scale?
 * Field for vertical scale
 * Time-height plot?
 * Use filled contours?
 * "Zig-zag" cross-section?
 */
static bool	Use_alt = TRUE;
static char	Zfld[20];
static bool	Time_height;
static bool	Fill_contour;
static bool	Zig_zag;

/*
 * Contour info
 */
static float	Contour_center, Contour_step;
static int	Line_width;
static bool	Do_labels;

/*
 * Maximum acceptable time difference between sounding time and plot time
 */
static int	Maxdiff = -1;

/*
 * Points for a sounding trace
 */
static XPoint	Trace[BUFLEN];
static int	Tracelen = 0;

/*
 * Color array and indices
 */
static XColor	*Colors;
static int	Ncolors;
static XColor	White, Black;

/*
 * Clip and unclip rectangles
 */
static XRectangle	Clip, Unclip;

/*
 * Forward declarations
 */
void	xs_LineContour FP ((char *, int));
void	xs_FilledContour FP ((char *, int));
static void	xs_Contour FP ((char *, int)); 
static void	xs_Planar FP ((char *, char *, char *)); 
static void	xs_ZigZag FP ((char *, char *, char *)); 
static void	xs_ZigZagFillIn FP ((int, float *));
static void	xs_Background FP ((void));
static void	xs_TimeHeight FP ((void)); 
static void	xs_ExtendTrace FP ((double, double)); 
static void	xs_HDWeighting FP ((char *, char *));
static void	xs_Bilinear FP ((char *, char *));
static void	xs_DrawTrace FP ((char *)); 
static void	xs_AddToLevel FP ((int, double, double, double)); 
static void	xs_BuildLimits FP ((float *, float *, double, double, double));
static int	xs_Pos FP ((char *, ZebTime *, float **, float **)); 
static int	xs_TimePos FP ((char *, ZebTime *, float **)); 
static int	xs_ZIndex FP ((double));
static void	xs_ColorScale FP ((void));
static DataChunk *xs_GetObsDC FP ((char *, char *, ZebTime *));
static DataChunk *xs_GetGridDC FP ((char *, char *, ZebTime *));




void
xs_LineContour (c, update)
char	*c;
bool	update;
{
	Fill_contour = FALSE;

	Do_labels = TRUE;
	pda_Search (Pd, c, "do-labels", "xsect", (char *) &Do_labels, 
		    SYMT_BOOL);
	
	Line_width = 0;
	pda_Search (Pd, c, "line-width", "xsect", (char *) &Line_width,
		    SYMT_INT);

	xs_Contour (c, update);
}




void
xs_FilledContour (c, update)
char	*c;
bool	update;
{
	Fill_contour = TRUE;
	xs_Contour (c, update);
}




static void
xs_Contour (c, update)
char	*c;
bool	update;
/*
 * Draw a cross-section based on the given PD component.
 */
{
	bool	ok;
	float	zmax;
	char	platforms[120], fldname[20], ctname[20];
	char	param[50];
/*
 * Get the platform(s), field, contour limits, and color table
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok = pda_ReqSearch (Pd, c, "field", NULL, fldname, SYMT_STRING);
	sprintf (param, "%s-center", fldname);
	ok &= pda_ReqSearch (Pd, c, param, "contour", (char *) &Contour_center,
		SYMT_FLOAT);
	sprintf (param, "%s-step", fldname);
	ok &= pda_ReqSearch (Pd, c, param, "contour", (char *) &Contour_step, 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "color-table", "xsect", ctname, 
		SYMT_STRING);

	if (! ok)
		return;
/*
 * User-specified maximum time difference
 */
	if (! pda_Search (Pd, c, "max-time-diff", "xsect", (char *) &Maxdiff,
		SYMT_INT))
		Maxdiff = -1;
	else
		Maxdiff *= 60;	/* Convert to seconds */
/*
 * Load the color table + explicitly get black and white
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);

	ct_GetColorByName ("black", &Black);
	ct_GetColorByName ("white", &White);
/*
 * Altitude or pressure z scaling?
 */
	Use_alt = TRUE;
	pda_Search (Pd, c, "by-altitude", "xsect", (char *) &Use_alt, 
		SYMT_BOOL);

	if (Use_alt)
		strcpy (Zfld, "alt");
	else
		strcpy (Zfld, "pres");
/*
 * Altitude or pressure limits.  Take the user values if they exist, otherwise
 * default to:  altitude - 0 to 12 km, pressure - 1050 to 200 mb.
 */
	P_bot = Use_alt ? 0.0 : 1050.0;
	P_hgt = Use_alt ? 12.0 : -850.0;

	pda_Search (Pd, "global", "z-min", NULL, (char *) &P_bot, SYMT_FLOAT);
	if (pda_Search (Pd, "global", "z-max", NULL, (char *) &P_hgt, 
			SYMT_FLOAT))
		P_hgt = zmax - P_bot;
/*
 * Pixel plot limits
 */
	Xlo = Ylo = 0.0;
	Xhi = Yhi = 1.0;

	Pix_left = XPIX (Xlo);
	Pix_right = XPIX (Xhi);
	Pix_bottom = YPIX (Ylo);
	Pix_top = YPIX (Yhi);
/*
 * Clip and unclip rectangles
 */
	Clip.x = Pix_left + 1;
	Clip.y = Pix_top + 1;
	Clip.width = Pix_right - Pix_left;
	Clip.height = Pix_bottom - Pix_top;

	Unclip.x = Unclip.y = 0;
	Unclip.width = GWWidth (Graphics);
	Unclip.height = GWHeight (Graphics);
/*
 * Zig-zag cross section?
 * Time-height plot?
 */
	Zig_zag = FALSE;
	pda_Search (Pd, c, "zig-zag", "xsect", (char *) &Zig_zag,
		SYMT_BOOL);

	Time_height = FALSE;
	pda_Search (Pd, c, "time-height", "xsect", (char *) &Time_height,
		SYMT_BOOL);
/*
 * Actually draw the cross-section based on type
 */
	if (Time_height)	
		/* xs_TimeHeight (); */
		msg_ELog (EF_INFO, "Time-height plots not yet implemented");
	else if (Zig_zag)
		xs_ZigZag (c, platforms, fldname);
	else
		xs_Planar (c, platforms, fldname);
/*
 * Draw the background and color scale
 */
	xs_Background ();
	xs_ColorScale ();
}




static void
xs_ZigZag (c, platforms, fldname)
char	*c, *platforms, *fldname;
/*
 * Draw a zig-zag "pseudo-planar" cross-section
 */
{
	int	p, nplat, pt, npts, ngood = 0, zndx, zndx_prev, diff;
	int	row, col;
	char	*pnames[MAXPLAT];
	float	badvalue, fdata, val, val_prev, frac, dist[MAXPLAT];
	float	z, zpos, zstep, z_prev;
	float	loc_x[MAXPLAT], loc_y[MAXPLAT], bot[MAXPLAT], top[MAXPLAT];
	ZebTime	dtime;
	DataChunk	*dc;
	Location	loc;
/*
 * Title
 */
	sprintf (Scratch, "Zig-zag cross-section of %s using: ", fldname);
	An_TopAnnot (Scratch, White.pixel);
/*
 * Pull apart the platforms (and test for how many, albeit a little late
 * since we'll write past the end of the pnames array if there really are
 * too many platforms.)
 */
	nplat = CommaParse (platforms, pnames);
	if (nplat > MAXPLAT)
	{
		msg_ELog (EF_PROBLEM, "Too many platforms!");
		return;
	}
/*
 * Arbitrarily use 50 vertical levels (for now)
 */
	Vdim = 50;
/*
 * Get the data plane and fill it with bad values
 */
	Plane = (float *) alloca (nplat * Vdim * sizeof (float));

	for (pt = 0; pt < nplat * Vdim; pt++)
		Plane[pt] = BADVAL;
/*
 * Vertical grid spacing
 */
	zstep = P_hgt / (float)(Vdim - 1);
/*
 * Loop through the platforms
 */
	for (p = 0; p < nplat; p++)
	{
	/*
	 * Add this platform to the top annotation
	 */
		if (p != 0)
			An_TopAnnot (", ", White.pixel);

		An_TopAnnot (pnames[p], White.pixel);
	/*
	 * Get the data
	 */
		if (! (dc = xs_GetObsDC (pnames[p], fldname, &dtime)))
			BAD_SOUNDING;
	/*
	 * Move the platform name to the appropriate spot in the name array,
	 * increment the good platform count and set the index for the
	 * column we'll be building in Plane
	 */
		pnames[ngood] = pnames[p];
		col = ngood++;
	/*
	 * Store the platform location
	 */
		dc_GetLoc (dc, 0, &loc);
		cvt_ToXY (loc.l_lat, loc.l_lon, &loc_x[col], &loc_y[col]);
	/*
	 * Initialize bottom and top
	 */
		bot[col] = 99e6;
		top[col] = -99e6;
	/*
 	 * Get some info from the data chunk
	 */
		npts = dc_GetNSample (dc);
		badvalue = dc_GetBadval (dc);
	/*
	 * Loop through the points
	 */
		val_prev = badvalue;

		for (pt = 0; pt < npts; pt++)
		{
		/*
		 * Bag this point if the datum or position is bad
		 */
			fdata = dc_GetScalar (dc, pt, F_Lookup (fldname));
			zpos = dc_GetScalar (dc, pt, F_Lookup (Zfld));

			if (fdata == badvalue || zpos == badvalue)
				continue;
		/*
		 * Special treatment for the first good point
		 */
			if (val_prev == badvalue)
			{
			/*
			 * Assign the previous point values
			 */
				val_prev = fdata;
				z_prev = zpos;
				zndx_prev = xs_ZIndex (zpos);
			/*
			 * Go on to the next point
			 */
				continue;
			}
		/*
		 * Adjust bottom or top if necessary
		 */
			bot[p] = zpos < bot[p] ? zpos : bot[p];
			top[p] = zpos > top[p] ? zpos : top[p];
		/*
		 * Quit when we get above the grid
		 */
			if (zndx_prev >= Vdim)
				break;
		/*
		 * Find the index of the next grid height at or above zpos[pt]
		 */
			zndx = xs_ZIndex (zpos);
		/*
		 * Assign values in Plane between this and the previous point
		 */
			for (row = zndx_prev; row < zndx && row < Vdim; row++)
			{
			/*
			 * Don't assign anything below the first grid level
			 */
				if (row < 0)
					continue;
			/*
			 * Find the height of this grid index and interpolate
			 * the data and position to this height
			 */
				z = row * zstep + P_bot;
				frac = (z - z_prev) / (zpos - z_prev);

				val = val_prev + frac * (fdata - val_prev);
			/*
			 * Insert this point into the Plane
			 */
				Plane[col * Vdim + row] = val;
			}
		/*
		 * Make this the previous point
		 */
			val_prev = fdata;
			z_prev = zpos;
			zndx_prev = zndx;
		}
	/*
	 * Annotate the sounding with a time or date and time if the
	 * difference from the plot time is > 12 hours.  Mark the sounding
	 * as bad if we had no good points
	 */
		if (val_prev == badvalue)
		{
			TC_EncodeTime (&dtime, TC_Full, Scratch);
			msg_ELog (EF_PROBLEM, 
				"%s or %s data missing for %s at %s",
				fldname, Zfld, pnames[p], Scratch);
			BAD_SOUNDING;
		}
		else
		{
			sprintf (Scratch, " (");

			diff = PlotTime.zt_Sec - dtime.zt_Sec;

			if (diff > 43200)
				TC_EncodeTime (&dtime, TC_Full, Scratch + 2);
			else
				TC_EncodeTime (&dtime, TC_TimeOnly, 
					Scratch + 2);

			strcat (Scratch + strlen (Scratch) - 3, ")");

			An_TopAnnot (Scratch, White.pixel);
		}
	}
/*
 * Sanity
 */
	if (ngood < 2)
	{
		msg_ELog (EF_INFO, "Nothing drawn (< 2 good soundings)");
		return;
	}
/*
 * Find the position of each sounding
 */
	dist[0] = 0.0;
	for (p = 1; p < ngood; p++)
		dist[p] = dist[p-1] + hypot ((loc_x[p] - loc_x[p-1]), 
			(loc_y[p] - loc_y[p-1]));
/*
 * Interpolate data where possible
 */
	xs_ZigZagFillIn (ngood, dist);
/*
 * Set user coordinates for the graphics (remember that Xlo, Ylo, Xhi, and Yhi
 * are in screen space, not in data space)
 */
	Xlo = 0.0;
	Xhi = P_len = dist[ngood - 1];
	Ylo = P_bot;
	Yhi = P_bot + P_hgt;
/*
 * Draw the contour in sections
 */
	for (p = 0; p < ngood; p++)
	{
	/*
	 * Contour between this sounding and the next one
	 */
		if (p < ngood - 1)
		{
			if (Fill_contour)
			{
				FC_Init (Colors, Ncolors, Ncolors / 2, Black, 
					Clip, TRUE, BADVAL);
				FillContour (Graphics, GWFrame (Graphics), 
					Plane + p * Vdim, 2, Vdim, 
					XPIX (dist[p]), Pix_bottom, 
					XPIX (dist[p+1]), Pix_top, 
					Contour_center, Contour_step);
			}
			else
			{
				CO_Init (Colors, Ncolors, Ncolors / 2, Black, 
					Clip, TRUE, BADVAL);
				Contour (Graphics, GWFrame (Graphics), 
					Plane + p * Vdim, 2, Vdim, 
					XPIX (dist[p]), Pix_bottom, 
					XPIX (dist[p+1]), Pix_top, 
					Contour_center, Contour_step, 
					Do_labels, Line_width);
			}
		}
	/*
	 * Site label
	 */
		strcpyUC (Scratch, pnames[p]);
		XSetForeground (XtDisplay (Graphics), Gcontext, White.pixel);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (dist[p]), Pix_bottom + 3, Scratch, 0.0, 0.02, 
			JustifyCenter, JustifyTop);
	/*
	 * Trace of altitude span
	 */
		xs_ExtendTrace (dist[p], bot[p]);
		xs_ExtendTrace (dist[p], top[p]);
		xs_DrawTrace ("");
	}
/*
 * Done
 */
	return;
}




static void
xs_ZigZagFillIn (ncols, dist)
int	ncols;
float	*dist;
/*
 * Interpolate data horizontally in the Plane array consisting of ncols 
 * columns by Vdim rows.  The dist array holds the relative positions of
 * the columns.
 */
{
	int	col, row, prev, next;
/*
 * We can potentially interpolate to any of the interior columns (all but the
 * first and last)
 */
	for (col = 1; col < ncols - 1; col++)
	{
		for (row = 0; row < Vdim; row++)
		{
		/*
		 * Move on if we have data here
		 */
			if (Plane[col * Vdim + row] != BADVAL)
				continue;
		/*
		 * We have a hole, so try to interpolate
		 *
		 * Find the closest columns on either side with good data in
		 * this row.  If we don't have good data on both sides, we
		 * can't interpolate.
		 */
			for (prev = col - 1; prev >= 0; prev--)
				if (Plane[prev * Vdim + row] != BADVAL)
					break;

			if (prev < 0)
				continue;


			for (next = col + 1; next < ncols; next++)
				if (Plane[next * Vdim + row] != BADVAL)
					break;

			if (next >= ncols)
				continue;
		/*
		 * We have good data on both sides, so do the interpolation
		 */
			Plane[col * Vdim + row] = ((dist[next] - dist[col]) * 
				Plane[prev * Vdim + row] + (dist[col] - 
				dist[prev]) * Plane[next * Vdim + row]) /
				(dist[next] - dist[prev]);
		}
	}
}

			




static void
xs_Planar (c, platforms, fldname)
char	*c, *platforms, *fldname;
/*
 * Draw a true planar cross-section
 */
{
	char	*vals[2], method[20];
/*
 * Title
 */
	sprintf (Scratch, "Cross-section of %s using: ", fldname);
	An_TopAnnot (Scratch, White.pixel);
/*
 * Grab the left endpoint from the PD (should be in the global component, since
 * we don't want different endpoints for various components of the plot)
 */
	if (! pda_ReqSearch (Pd, "global", "left-endpoint", NULL, Scratch, 
		SYMT_STRING))
		return;

	if (CommaParse (Scratch, vals) != 2)
	{
		msg_ELog (EF_PROBLEM, "Endpoints must be in x,y format");
		return;
	}


	if (! sscanf (vals[0], "%f", &X0) || ! sscanf (vals[1], "%f", &Y0))
	{
		msg_ELog (EF_PROBLEM, "Bad left endpoint (%s,%s)", 
			vals[0], vals[1]);
		return;
	}
/*
 * Grab the right endpoint from the PD (should be in the global component, 
 * since we don't want different endpoints for various components of the plot)
 */
	if (! pda_ReqSearch (Pd, "global", "right-endpoint", NULL, Scratch, 
		SYMT_STRING))
		return;

	if (CommaParse (Scratch, vals) != 2)
	{
		msg_ELog (EF_PROBLEM, "Endpoints must be in x,y format");
		return;
	}


	if (! sscanf (vals[0], "%f", &X1) || ! sscanf (vals[1], "%f", &Y1))
	{
		msg_ELog (EF_PROBLEM, "Bad right endpoint (%s,%s)",
			vals[0], vals[1]);
		return;
	}
/*
 * Find the plane length
 */
	P_len = hypot (X1 - X0, Y1 - Y0);
/*
 * Create and fill the data plane
 */
	strcpy (method, "default");
	pda_Search (Pd, c, "grid-method", "xsect", method, SYMT_STRING);

	if (! strcmp (method, "bilinear"))
		xs_Bilinear (platforms, fldname);
	else
		xs_HDWeighting (platforms, fldname);
/*
 * Draw the contours
 */
	if (Fill_contour)
	{
		FC_Init (Colors, Ncolors, Ncolors / 2, Black, Clip, 
			TRUE, BADVAL);
		FillContour (Graphics, GWFrame (Graphics), Plane, Hdim, Vdim, 
			Pix_left, Pix_bottom, Pix_right, Pix_top, 
			Contour_center, Contour_step);
	}
	else
	{
		CO_Init (Colors, Ncolors, Ncolors / 2, Black, Clip, 
			TRUE, BADVAL);
		Contour (Graphics, GWFrame (Graphics), Plane, Hdim, Vdim, 
			Pix_left, Pix_bottom, Pix_right, Pix_top, 
			Contour_center, Contour_step, Do_labels, Line_width);
	}
/*
 * Clean up
 */
	free (Plane);
	return;
}




static void
xs_HDWeighting (platforms, fldname)
char	*platforms, *fldname;
/*
 * Fill the cross-section array with data from the chosen soundings, using
 * a horizontal distance weighting scheme.  Plane is malloc'ed here and 
 * should be free'd by the caller.
 */
{
	int	plat, nplat, pt, npts, iz, zndx, zndx_prev, ih, iv;
	int	i, j, diff, offset;
	float	val, x, y, z, t, val_prev, x_prev, y_prev, z_prev, t_prev;
	float	zstep, hlen, frac, badvalue;
	float	fdata, *xpos = NULL, *ypos = NULL, zpos, *tpos = NULL;
	float	xhighest, yhighest, zhighest;
	float	*floor, *ceiling, *f_wgt, *c_wgt;
	char	*pnames[20];
	ZebTime	dtime;
	DataChunk	*dc;
/*
 * Parse out platform names
 */
	nplat = CommaParse (platforms, pnames);
/*
 * Arbitrarily choose 50x50 for our grid size.
 */
	Hdim = Vdim = 50;
/*
 * Set user coordinates for pixel conversions
 */
	Xlo = 0.0;
	Xhi = P_len;

	Ylo = P_bot;
	Yhi = P_bot + P_hgt;
/*
 * Allocate space for the plane and weight arrays.
 */
	Plane = (float *) malloc (Hdim * Vdim * sizeof (float));
/*
 * Get the plane weight array plus the floor and ceiling arrays and 
 * their associated weight arrays.  These arrays are all only needed
 * for the duration of this routine, so we can use alloca.
 */
	P_wgt = (float *) alloca (Hdim * Vdim * sizeof (float));

	floor = (float *) alloca (Hdim * sizeof (float));
	f_wgt = (float *) alloca (Hdim * sizeof (float));
	ceiling = (float *) alloca (Hdim * sizeof (float));
	c_wgt = (float *) alloca (Hdim * sizeof (float));
/*
 * Fill the plane with BADVALs and set the weights to zero.
 * Initialize the floor and ceiling arrays also.
 */
	for (i = 0; i < Hdim; i++)
	{
		for (j = 0; j < Vdim; j++)
		{
			offset = (i * Vdim) + j;

			Plane[offset] = BADVAL;
			P_wgt[offset] = 0.0;
		}

		floor[i] = P_bot;
		f_wgt[i] = 0.0;
		ceiling[i] = P_bot + P_hgt;
		c_wgt[i] = 0.0;
	}
/*
 * Vertical grid spacing
 */
	zstep = P_hgt / (float)(Vdim - 1);
/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Add this platform to the top annotation
	 */
		if (plat != 0)
			An_TopAnnot (", ", White.pixel);

		An_TopAnnot (pnames[plat], White.pixel);
	/*
	 * Initialize for keeping track of the highest point
	 */
		if (Use_alt)
			zhighest = -9999.0;
		else
			zhighest = 9999.0;
	/*
	 * Get the data
	 */
		if (! (dc = xs_GetObsDC (pnames[plat], fldname, &dtime)))
			BAD_SOUNDING;
	/*
 	 * Get some info from the data chunk
	 */
		npts = dc_GetNSample (dc);
		badvalue = dc_GetBadval (dc);
	/*
	 * Put together the x,y position data
	 */
		if (Time_height && ! xs_TimePos (pnames[plat], &dtime, &tpos))
		{
			TC_EncodeTime (&dtime, TC_Full, Scratch);
			msg_ELog (EF_PROBLEM, "No '%s' time data at %s",
				pnames[plat], Scratch);
			BAD_SOUNDING;
		}
		else if (! Time_height && ! xs_Pos (pnames[plat], &dtime, 
			&xpos, &ypos))
		{
			TC_EncodeTime (&dtime, TC_Full, Scratch);
			msg_ELog (EF_PROBLEM, "No '%s' position data at %s", 
				pnames[plat], Scratch);
			BAD_SOUNDING;
		}
	/*
	 * Loop through the points
	 */
		val_prev = badvalue;

		for (pt = 0; pt < npts; pt++)
		{
		/*
		 * Bag this point if the datum or position is bad
		 */
			if (Time_height)
			{
				if (tpos[pt] == badvalue)
					continue;
			}
			else
			{
				if (xpos[pt] == badvalue 
				    || ypos[pt] == badvalue)
					continue;
			}
	
			fdata = dc_GetScalar (dc, pt, F_Lookup (fldname));
			zpos = dc_GetScalar (dc, pt, F_Lookup (Zfld));

			if (fdata == badvalue || zpos == badvalue)
				continue;
		/*
		 * Update the ceiling if this point is higher
		 */
			if ((Use_alt && zpos > zhighest) ||
				(!Use_alt && zpos < zhighest))
			{
				xhighest = xpos[pt];
				yhighest = ypos[pt];
				zhighest = zpos;
			}
		/*
		 * Special treatment for the first good point
		 */
			if (val_prev == badvalue)
			{
			/*
			 * Assign the previous point values
			 */
				val_prev = fdata;
				z_prev = zpos;
				zndx_prev = xs_ZIndex (zpos);

				if (Time_height)
					t_prev = tpos[pt];
				else
				{
					x_prev = xpos[pt];
					y_prev = ypos[pt];
				}
			/*
			 * Use the point to help build the floor array
			 */
				xs_BuildLimits (floor, f_wgt, xpos[pt], 
					ypos[pt], zpos);
			/*
			 * Go on to the next point
			 */
				continue;
			}
		/*
		 * Quit when we get above the grid
		 */
			if (zndx_prev >= Vdim)
				break;
		/*
		 * Find the index of the next grid height at or above zpos[pt]
		 */
			zndx = xs_ZIndex (zpos);
		/*
		 * Assign values at grid levels between this point and the
		 * previous one
		 */
			for (iz = zndx_prev; iz < zndx && iz < Vdim; iz++)
			{
			/*
			 * Don't assign anything below the first grid level
			 */
				if (iz < 0)
					continue;
			/*
			 * Find the height of this grid index and interpolate
			 * the data and position to this height
			 */
				z = iz * zstep + P_bot;
				frac = (z - z_prev) / (zpos - z_prev);

				val = val_prev + frac * (fdata - val_prev);

				if (Time_height)
					t = (t_prev + frac * 
						(tpos[pt] - t_prev)) / 3600.0;
				else
				{
					x = x_prev + frac * (xpos[pt]-x_prev);
					y = y_prev + frac * (ypos[pt]-y_prev);
				}
			/*
			 * Add this datum in at the current height index
			 */
				if (Time_height)
					xs_AddToLevel (iz, t, 0.0, val);
				else
					xs_AddToLevel (iz, x, y, val);
			}
		/*
		 * Project this point onto the plane, and add a point to 
		 * the trace for this sounding
		 */
			if (Time_height)
				hlen = tpos[pt] / 3600.0;	/* to hours */
			else
				hlen = hypot (xpos[pt] - X0, ypos[pt] - Y0) *
					cos (atan2 (ypos[pt]-Y0, xpos[pt]-X0) -
					atan2 (Y1-Y0, X1-X0));

			xs_ExtendTrace (hlen, zpos);
		/*
		 * Make this the previous point
		 */
			val_prev = fdata;
			z_prev = zpos;
			zndx_prev = zndx;
			if (Time_height)
				t_prev = tpos[pt];
			else
			{
				x_prev = xpos[pt];
				y_prev = ypos[pt];
			}
		}
	/*
	 * Free the position arrays returned by xs_Pos () or xs_TimePos ()
	 */
		if (xpos)
			free (xpos);

		if (ypos)
			free (ypos);

		if (tpos)
			free (tpos);
	/*
	 * Only go on if we had at least one good data value
	 */
		if (val_prev == badvalue)
		{
			TC_EncodeTime (&dtime, TC_Full, Scratch);
			msg_ELog (EF_PROBLEM, 
				"%s or %s data missing for %s at %s",
				fldname, Zfld, pnames[plat], Scratch);
			BAD_SOUNDING;
		}
	/*
	 * Use the highest point of this sounding to help build the 
	 * ceiling array
	 */
		xs_BuildLimits (ceiling, c_wgt, xhighest, yhighest, zhighest);
	/*
	 * Draw the trace for this sounding
	 */
		xs_DrawTrace (pnames[plat]);
	/*
	 * Annotate the sounding with a time or date and time if the
	 * difference from the plot time is > 12 hours.  
	 */
		sprintf (Scratch, " (");

		diff = PlotTime.zt_Sec - dtime.zt_Sec;

		if (diff > 43200)
			TC_EncodeTime (&dtime, TC_Full, Scratch + 2);
		else
			TC_EncodeTime (&dtime, TC_TimeOnly, Scratch + 2);

		strcat (Scratch + strlen (Scratch) - 3, ")");

		An_TopAnnot (Scratch, White.pixel);
	/*
	 * The plane array is populated, free the allocated memory and return
	 */
		dc_DestroyDC (dc);
	}
/*
 * Finish the top annotation with a period
 */
	An_TopAnnot (".  ", White.pixel);
/*
 * Remove data below the floor and above the ceiling
 */
	for (ih = 0; ih < Hdim; ih++)
	{
		for (iv = 0; iv < xs_ZIndex (floor[ih]); iv++)
			Plane[ih * Vdim + iv] = BADVAL;

		for (iv = xs_ZIndex (ceiling[ih]); iv < Vdim; iv++)
			Plane[ih * Vdim + iv] = BADVAL;
	}

	return;
}



static void
xs_Bilinear (platform, fldname)
char	*platform, *fldname;
/*
 * Fill the cross-section array with data from a cartesian grid, using
 * bilinear interpolation.  It is assumed that the data source will be a
 * 3d cartesian grid.  Plane is malloc'ed here and should be free'd by the
 * caller.
 */
{
	int	nplat, len, h, v, i, j, k;
	float	*sourcegrid, *sgp, sgbad, *pp;
	float	f_i0, f_istep, f_j0, f_jstep, f_k0, f_kstep, f_i, f_j;
	float	grid_x0, grid_y0, grid_z0, di, dj, val0, val1, val2, val3;
	char	*pnames[20];
	RGrid	rg;
	ZebTime dtime;
	Location	loc;
	DataChunk	*dc;
/*
 * Can't use pressure for the vertical scale for this type of plot
 */
	if (! Use_alt)
	{
		msg_ELog (EF_PROBLEM, 
			  "Can't do bilinear plots with pressure vert. scale");
		return;
	}
/*
 * Parse out platform names
 */
	nplat = CommaParse (platform, pnames);
	if (nplat > 1)
		msg_ELog (EF_INFO, 
		    "Only one platform used for bilinear method x-sections");
/*
 * Add this platform to the top annotation
 */
	An_TopAnnot (platform, White.pixel);
	An_TopAnnot (".  ", White.pixel);
/*
 * Get the data
 */
	if (! (dc = xs_GetGridDC (platform, fldname, &dtime)))
		return;
/*
 * Get the info we need from the data chunk
 */
	sourcegrid = dc_RGGetGrid (dc, 0, F_Lookup (fldname), &loc, &rg, &len);
	cvt_ToXY (loc.l_lat, loc.l_lon, &grid_x0, &grid_y0);
	grid_z0 = loc.l_alt;

	sgbad = dc_GetBadval (dc);
/*
 * More or less arbitrarily base the horizontal spacing for our plane
 * on the x spacing of the source grid.  The vertical spacing is set to
 * the z spacing of the source grid.
 */
	Hdim = (P_len / rg.rg_Xspacing) + 1;
	Vdim = (P_hgt / rg.rg_Zspacing) + 1;

	Plane = (float *) malloc (Hdim * Vdim * sizeof (float));
/*
 * Set user coordinates for pixel conversions (Note that "x" and "y" here refer
 * to user coordinates on the screen, which correspond to the direction of the
 * plane cut and altitude in the source grid.)
 */
	Xlo = 0.0;
	Xhi = P_len;

	Ylo = P_bot;
	Yhi = P_bot + P_hgt;
/*
 * Come up with numbers so we can easily translate horizontal steps in 
 * the plane into indices in the source grid. (Now we're using x and y
 * as defined in the source grid)
 */
	f_i0 = (X0 - grid_x0) / rg.rg_Xspacing;
	f_j0 = (Y0 - grid_y0) / rg.rg_Yspacing;
	f_k0 = (P_bot - grid_z0) / rg.rg_Zspacing;

	if (Hdim > 1)
	{
		f_istep = (X1 - X0) / (rg.rg_Xspacing * (Hdim - 1));
		f_jstep = (Y1 - Y0) / (rg.rg_Yspacing * (Hdim - 1));
	}
	else
		f_istep = f_jstep = 0.0;

	f_kstep = 1.0;
/*
 * h is the horizontal index and v is the vertical index into the vertical 
 * plane we're building.
 * i and j are the horizontal indices and k is the vertical index into the 
 * source grid.
 */
	for (v = 0; v < Vdim; v++)
	{
		k = nint (f_k0 + v * f_kstep);

		for (h = 0; h < Hdim; h++)
		{
			pp = Plane + h * Vdim + v;

			f_i = f_i0 + h * f_istep;
			f_j = f_j0 + h * f_jstep;

			i = nint (f_i);
			j = nint (f_j);
		/*
		 * Simple if we're outside the source grid
		 */
			if (i < 0 || j < 0 || k < 0 ||
			    i > rg.rg_nX-2 || j > rg.rg_nY-2 || k > rg.rg_nZ-1)
			{
				*pp = BADVAL;
				continue;
			}
		/*
		 * Do a bilinear interpolation using the four source grid
		 * points (.) surrounding the plane point (+).  Point 0 is
		 * at grid position (i,j) and di and dj are fractions of 
		 * the x and y grid spacing to the plane point, respectively.
		 *
		 *     2	 3
		 *	.	.
		 *	     +    -
		 *		   |
		 *		   | dj
		 *	.	. -
		 *     0	 1
		 *
		 *	|____|
		 *        di
		 *
		 *
		 *	  val =	(1-di)(1-dj) val0 + (di)(1-dj) val1 +
		 *		(1-di)(dj) val2 + (di)(dj) val3
		 */
			di = f_i - i;
			dj = f_j - j;

			sgp = sourcegrid + k * (rg.rg_nX * rg.rg_nY) + 
				j * rg.rg_nX + i;
			val0 = *sgp;
			val1 = *(sgp + 1);
			val2 = *(sgp + rg.rg_nX);
			val3 = *(sgp + rg.rg_nX + 1);
		/*
		 * If any of the surrounding grid points are bad, assign
		 * a bad value, otherwise do the interpolation
		 */
			if (val0 == sgbad || val1 == sgbad || 
				val2 == sgbad || val3 == sgbad)	
				*pp = BADVAL;
			else
				*pp = 	(1 - di) * (1 - dj) * val0 +
					di * (1 - dj) * val1 + 
					(1 - di) * dj * val2 + 
					di * dj * val3;
		}
	}
/*
 * The plane array is populated, free the data chunk and return
 */
	dc_DestroyDC (dc);
	return;
}




static int
xs_ZIndex (z)
float	z;
/*
 * Return the index of the first grid level at or above z
 */
{
	float	fndx = (z - P_bot) / P_hgt * (Vdim - 1);

	if ((float)((int) fndx) == fndx)
		return ((int) fndx);	
	else
		return ((int) fndx + 1);
}




static void
xs_AddToLevel (iv, xdat, ydat, vdat)
int	iv;
float	vdat, xdat, ydat;
/*
 * Apply the point with value vdat located at (xdat,ydat) to the plane
 * at vertical index iv.  (For time-height plots, xdat should be the time 
 * position and ydat should be zero)
 */
{
	int	ih, offset;
	float	x, y, d, xstep, ystep, tstep, wgt;
/*
 * Sanity check
 */
	if (iv < 0 || iv >= Vdim)
		ui_error ("*BUG* Bad vertical index in xs_AddToLevel");
/*
 * Step through the grid horizontally at vertical index iv and use a distance
 * weighting scheme to apply the given point
 */
	if (Time_height)
		tstep = P_len / (Hdim - 1);
	else
	{
		xstep = (X1 - X0) / (Hdim - 1);
		ystep = (Y1 - Y0) / (Hdim - 1);
	}


	for (ih = 0; ih < Hdim; ih++)
	{
		if (Time_height)
		{
			x = ih * tstep;
			y = 0.0;
		}
		else
		{
			x = X0 + ih * xstep;
			y = Y0 + ih * ystep;
		}

		d = sqrt ((xdat - x) * (xdat - x) + (ydat - y) * (ydat - y));
	/*
	 * Use a 1/d^2 weighting scheme, truncated at a weight of 100
	 */
		if (d < 0.1)
			wgt = 100;
		else
			wgt = 1.0 / (d * d);
	/*
	 * Apply the point
	 */
		offset = (ih * Vdim) + iv;
		Plane[offset] = (Plane[offset] * P_wgt[offset] + vdat * wgt) /
			(P_wgt[offset] + wgt);
		P_wgt[offset] = P_wgt[offset] + wgt;
	}
}




static void
xs_BuildLimits (array, weight, xdat, ydat, zdat)
float	*array, *weight;
float	xdat, ydat, zdat;
/*
 * Use the given point to help build either the given ceiling or floor
 * array.  For time-height plots, xdat should be the time position and 
 * ydat should be zero.
 */
{
	int	ih;
	float	pos, d, hstep, wgt;
/*
 * Horizontal step length in the plane
 */
	hstep = P_len / (Hdim - 1);
/*
 * Find the distance from the left endpoint of the plane to the projection
 * of the given point onto the plane.
 */
	if (Time_height)
		pos = xdat;
	else
		pos = hypot (xdat - X0, ydat - Y0) * 
			cos (atan2 (ydat-Y0, xdat-X0) - atan2 (Y1-Y0, X1-X0));
/*
 * Step through the array and use a distance weighting scheme to apply 
 * the given point.  The point is projected onto the plane before distances
 * are calculated. 
 */
	for (ih = 0; ih < Hdim; ih++)
	{
	/*
	 * Find the distance from this array point to the projection of
	 * the input point
	 */
		d = fabs (ih * hstep - pos);
	/*
	 * Use a 1/d weighting scheme, truncated at a weight of 100
	 */
		if (d < 0.1)
			wgt = 100;
		else
			wgt = 1.0 / (d * d);
	/*
	 * Apply the point
	 */
		array[ih] = (array[ih] * weight[ih] + zdat * wgt) /
			(weight[ih] + wgt);
		weight[ih] = weight[ih] + wgt;
	}
}




static void
xs_Background ()
/*
 * Draw the background for this cross-section
 */
{
	float	tick, tickinc, lolim, hilim;
	int	dolabel;
	XPoint	pts[5];
/*
 * Draw a box
 */
	pts[0].x = Pix_left;	pts[0].y = Pix_bottom;
	pts[1].x = Pix_right;	pts[1].y = Pix_bottom;
	pts[2].x = Pix_right;	pts[2].y = Pix_top;
	pts[3].x = Pix_left;	pts[3].y = Pix_top;
	pts[4].x = Pix_left;	pts[4].y = Pix_bottom;

	XSetForeground (XtDisplay (Graphics), Gcontext, White.pixel);
	XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, pts, 
		5, CoordModeOrigin);
/*
 * Get the lower and upper limits of the vertical axes
 */
	if (P_hgt > 0.0)
	{
		lolim = P_bot;
		hilim = P_bot + P_hgt;
	}
	else
	{
		lolim = P_bot + P_hgt;
		hilim = P_bot;
	}
/*
 * Figure out where to put ticks on the vertical axes
 */
	tickinc = pow (10.0, floor (log10 (fabs (P_hgt))));

	if (fabs (P_hgt / tickinc) < 1.5)
		tickinc *= 0.1;
	else if (fabs (P_hgt / tickinc) < 3.0)
		tickinc *= 0.2;
	else if (fabs (P_hgt / tickinc) < 8.0)
		tickinc *= 0.5;
/*
 * Adjust the lower limit to a reasonable label value
 */
	lolim = tickinc * ceil (lolim / tickinc);
/*
 * Loop to put tick marks and labels on vertical axes
 */
	dolabel = ((int)(lolim / tickinc) % 2) == 0;

	for (tick = lolim; tick <= hilim; tick += tickinc)
	{
	/*
	 * Draw the tick
	 */
		pts[0].y = pts[1].y = YPIX (tick);
		pts[0].x = Pix_left;
		pts[1].x = XPIX (0.025 * P_len);
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);

		pts[0].x = XPIX (0.975 * P_len);
		pts[1].x = Pix_right;
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{
			if (tickinc < 1.0)
				sprintf (Scratch, "%.1f", tick);
			else
				sprintf (Scratch, "%d", (int) tick);

			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				XPIX (-0.005 * P_len), YPIX (tick), 
				Scratch, 0.0, 0.02, JustifyRight, 
				JustifyCenter);
		}
		dolabel = ! dolabel;
	}

	if (Use_alt)
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (-0.04 * P_len), YPIX (P_bot + 0.5 * P_hgt), 
			"Altitude (km MSL)", 90.0, 0.02, JustifyCenter, 
			JustifyBottom);
	else
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (-0.04 * P_len), YPIX (P_bot + 0.5 * P_hgt), 
			"Pressure (mb)", 90.0, 0.02, JustifyCenter, 
			JustifyBottom);
/*
 * No horizontal axis labels for zig-zag plots
 */
	if (Zig_zag)
		return;
/*
 * Figure out where to put ticks on horizontal axes
 */
	if (Time_height)
	{
		tickinc = pow (10.0, floor (log10 (P_len)));

		if ((P_len / tickinc) < 1.5)
			tickinc *= 0.1;
		else if ((P_len / tickinc) < 3.0)
			tickinc *= 0.2;
		else if ((P_len / tickinc) < 8.0)
			tickinc *= 0.5;
	/*
	 * Don't make tick increment finer than 1/2 hour
	 */
		if (Time_height && tickinc < 0.5)
			tickinc = 0.5;
	}
	else
		tickinc = P_len / 8.0;
/*
 * Label the horizontal axes
 */
	dolabel = TRUE;

	for (tick = 0.0; tick <= 1.005 * P_len; tick += tickinc)
	{
	/*
	 * Draw the tick
	 */
		pts[0].x = pts[1].x = XPIX (tick);
		pts[0].y = Pix_bottom;
		pts[1].y = YPIX (P_bot + 0.025 * P_hgt);
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);

		pts[0].y = YPIX (P_bot + 0.975 * P_hgt);
		pts[1].y = Pix_top;
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{
			if (Time_height)
				sprintf (Scratch, "%d", (int) tick);
			else
				sprintf (Scratch, "(%.1f,%.1f)", 
					X0 + tick / P_len * (X1 - X0),
					Y0 + tick / P_len * (Y1 - Y0));

			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				XPIX (tick), YPIX (P_bot - 0.005 * P_hgt), 
				Scratch, 0.0, 0.02, JustifyCenter, JustifyTop);
		}
		dolabel = ! dolabel;
	}

	if (Time_height)
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (0.5 * P_len), YPIX (P_bot - 0.04 * P_hgt), 
			"Time from start (hours)", 0.0, 0.02, 
			JustifyCenter, JustifyTop);
	else
		DrawText (Graphics, GWFrame (Graphics), Gcontext,
			XPIX (0.5 * P_len), YPIX (P_bot - 0.04 * P_hgt), 
			"Position (km)", 0.0, 0.02, JustifyCenter, 
			JustifyTop);
}




static void
xs_TimeHeight ()
/*
 * Initialize for a time-height plot
 */
{
# ifdef notdef
	int	snd, sec_delta, pt, npts;
	float	*tdata;
	date	stime, diff, latest, earliest;
	date	snd_time ();
/*
 * Find the earliest and latest times from all of the soundings used
 */
	earliest.ds_yymmdd = 99991231;
	earliest.ds_hhmmss = 235959;
	latest.ds_yymmdd = 0;
	latest.ds_hhmmss = 0;

	for (snd = 0; snd < Nsnd; snd++)
	{
		stime = snd_time (S_id[snd]);
	/*
	 * See if this is the latest sounding by start time.
	 * If so, put the length of the sounding into sec_delta
	 */
		if (stime.ds_yymmdd > latest.ds_yymmdd || 
			(stime.ds_yymmdd == latest.ds_yymmdd && 
			 stime.ds_hhmmss > latest.ds_hhmmss))
		{
			latest = stime;

			if (snd_has_field (S_id[snd], f_time))
			{
				tdata = (float *) 
					malloc (BUFLEN * sizeof (float));
				npts = snd_get_data (S_id[snd], tdata, BUFLEN, 
					f_time, BADVAL);
				for (pt = npts-1; pt >= 0; pt--)
					if (tdata[pt] != BADVAL)
					{
						sec_delta = 
							(int)(tdata[pt] + 0.5);
						break;
					}

				free (tdata);
			}
			else
				sec_delta = 0;
		}
					
	/*
	 * See if this is the earliest sounding
	 */
		if (stime.ds_yymmdd < earliest.ds_yymmdd ||
			(stime.ds_yymmdd == earliest.ds_yymmdd &&
			 stime.ds_hhmmss < earliest.ds_hhmmss))
			earliest = stime;
	}
/*
 * Make sure we have a time range
 */
	if (earliest.ds_yymmdd == latest.ds_yymmdd && 
		earliest.ds_hhmmss == latest.ds_hhmmss)
		ui_error ("All soundings used are at the same time!");
/*
 * Set the plot start time
 */
	T0 = earliest;
/*
 * Find the length of the plane (the total time range in hours)
 */
	ud_sub_date (&latest, &earliest, &diff);
	P_len = (diff.ds_yymmdd % 100) * 86400 + 	/* days    */
		(diff.ds_hhmmss / 10000) * 3600 + 	/* hours   */
		((diff.ds_hhmmss / 100) % 100) * 60 + 	/* minutes */
		(diff.ds_hhmmss % 100) + sec_delta;	/* seconds */

	P_len /= 3600.0;
/*
 * Set the time-height flag
 */
	Time_height = TRUE;
# endif
}

	



static int
xs_Pos (plat, ptime, xpp, ypp)
char	*plat;
ZebTime	*ptime;
float	**xpp, **ypp;
/*
 * Return the x and y positions of the data for platform 'pid'
 * The xpp and ypp arrays are malloc'ed by xs_Pos () and must
 * be freed by the caller.
 */
{
	float	*xpos, *ypos, lat, lon, *wspd, *wdir, *wtime, *dummy;
	float	ws, wd, t, dt, site_x, site_y, badvalue;
	int	i, pt, npts, navail;
	bool	have_lat, have_lon;
	float	snd_s_lat (), snd_s_lon ();
	FieldId	fieldlist[2], available[50], f_lat, f_lon;
	PlatformId	pid;
	DataChunk	*dc;
	Location	loc;
/*
 * Get the ID of this platform
 */
	pid = ds_LookupPlatform (plat);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", plat);
		return (FALSE);
	}
/*
 * Get a field list so we can see if lat and lon are available
 */
	ds_GetFields (pid, ptime, &navail, available);

	f_lat = F_Lookup ("lat");
	f_lon = F_Lookup ("lon");

	have_lat = have_lon = FALSE;

	for (i = 0; i < navail; i++)
	{
		have_lat |= (available[i] == f_lat);
		have_lon |= (available[i] == f_lon);
	}
/*
 * Derive the (x,y) positions from (lat,lon) if possible
 */
	if (have_lat && have_lon)
	{
	/*
	 * Lat and lon are available (can be converted directly to x,y)
	 */
		fieldlist[0] = f_lat;
		fieldlist[1] = f_lon;
	/*
	 * Get the data chunk
	 */
		if (! (dc = ds_FetchObs (pid, DCC_Scalar, ptime, fieldlist, 2,
			NULL, 0)))
			return (FALSE);

		npts = dc_GetNSample (dc); 
		dc_GetLoc (dc, 0, &loc);
	/*
	 * Get the (x,y) site location
	 */
		cvt_ToXY (loc.l_lat, loc.l_lon, &site_x, &site_y);
	/*
	 * Allocate the x and y position arrays
	 */
		*xpp = xpos = (float *) malloc (npts * sizeof (float));
		*ypp = ypos = (float *) malloc (npts * sizeof (float));
	/*
	 * Convert the (lat,lon) points to (x,y) and put them in the arrays
	 */
		badvalue = dc_GetBadval (dc);

		for (pt = 0; pt < npts; pt++)
		{
			lat = dc_GetScalar (dc, pt, F_Lookup ("lat"));
			lon = dc_GetScalar (dc, pt, F_Lookup ("lon"));
			if (lat == badvalue || lon == badvalue)
				xpos[pt] = ypos[pt] = badvalue;
			else
				cvt_ToXY (lat, lon, &(xpos[pt]), &(ypos[pt]));
		}
	/*
	 * Free the data chunk
	 */
		dc_DestroyDC (dc);
	}
# ifdef notdef
	else if (snd_has_field (sid, f_wspd) && snd_has_field (sid, f_wdir) && 
		snd_has_field (sid, f_time))
	{
	/*
	 * Derive the (x,y) positions from wind speed, wind direction, 
	 * and time
	 */
		wspd = (float *) malloc (BUFLEN * sizeof (float));
		wdir = (float *) malloc (BUFLEN * sizeof (float));
		wtime = (float *) malloc (BUFLEN * sizeof (float));

		npts = snd_get_data (sid, wspd, BUFLEN, f_wspd, BADVAL);
		snd_get_data (sid, wdir, BUFLEN, f_wdir, BADVAL);
		snd_get_data (sid, wtime, BUFLEN, f_time, BADVAL);

		xpos[0] = site_x;
		ypos[0] = site_y;

		ws = wd = t = dt = 0.0;

		for (pt = 0; pt < npts - 1; pt++)
		{
			if (wspd[pt] != BADVAL && wdir[pt] != BADVAL)
			{
				ws = wspd[pt] * 0.001;	/* km/s */
				wd = wdir[pt];
			}

			if (wtime[pt] != BADVAL)
			{
				dt = wtime[pt] - t;
				t = wtime[pt];
			}
			else
				dt = 0.0;

			xpos[pt+1] = xpos[pt] + dt * ws * 
				cos (DEG_TO_RAD (-90.0 - wd));
			ypos[pt+1] = ypos[pt] + dt * ws *
				sin (DEG_TO_RAD (-90.0 - wd));
		}
	/*
	 * Free our data arrays
	 */
		free (wspd);
		free (wdir);
		free (wtime);
	}
# endif
	else
	{
	/*
	 * We have no position data, just assume the sounding
	 * goes straight up
	 *
	 * KLUGE: Ask for a data chunk of some field just to find out how
	 * many data points we need to do and the site location.
	 */
		fieldlist[0] = f_lat;
		if (! (dc = ds_FetchObs (pid, DCC_Scalar, ptime, fieldlist, 1,
			NULL, 0)))
			return (FALSE);
	/*
	 * Grab the location and the number of points
	 */
		dc_GetLoc (dc, 0, &loc);
		cvt_ToXY (loc.l_lat, loc.l_lon, &site_x, &site_y);

		npts = dc_GetNSample (dc);
	/*
	 * Allocate the x and y position arrays
	 */
		*xpp = xpos = (float *) malloc (npts * sizeof (float));
		*ypp = ypos = (float *) malloc (npts * sizeof (float));
	/*
	 * Just put the site location in each point
	 */
		for (pt = 0; pt < npts; pt++)
		{
			xpos[pt] = site_x;
			ypos[pt] = site_y;
		}
	}

	return (TRUE);
}




static int
xs_TimePos (plat, ptime, tpos)
char	*plat;
ZebTime	*ptime;
float	**tpos;
/*
 * Return the time positions of the data points for 'pid' relative to T0
 */
{
# ifdef notdef
	float	*time;
	int	npts, pt, start_sec;
	date	stime, diff, snd_time ();
/*
 * Get the sounding start time and the difference from the start time
 * of the plot
 */
	stime = snd_time (sid);
	ud_sub_date (&stime, &T0, &diff);
/*
 * Convert the time difference into seconds
 */
	start_sec = (diff.ds_yymmdd % 100) * 86400 +
		(diff.ds_hhmmss / 10000) * 3600 +
		((diff.ds_hhmmss / 100) % 100) * 60 + 
		(diff.ds_hhmmss % 100);
/*
 * If there is no time data for this sounding, fill the array with the
 * start time and return
 */
	if (! snd_has_field (sid, f_time))
	{
		for (pt = 0; pt < BUFLEN; pt++)
			tpos[pt] = start_sec;

		return;
	}
/*
 * Get the time data for the sounding
 */
	time = (float *) malloc (BUFLEN * sizeof (float));	

	npts = snd_get_data (sid, time, BUFLEN, f_time, BADVAL);

	for (pt = 0; pt < npts; pt++)
		if (time[pt] != BADVAL)
			tpos[pt] = start_sec + time[pt];
		else
			tpos[pt] = BADVAL;

	free (time);

	return;
# endif
}




static void
xs_ExtendTrace (x, y)
float	x, y;
/*
 * Add this (x,y) point to the trace for the current sounding
 */
{
	if (Tracelen >= BUFLEN)
		ui_error ("BUG! Too many points in x-section sounding trace!");

	Trace[Tracelen].x = XPIX (x);
	Trace[Tracelen].y = YPIX (y);
	Tracelen++;
}




static void
xs_DrawTrace (name)
char	*name;
/*
 * Draw the current platform's trace
 */
{
	char	dash[2];
	int	i, label_x, label_y;
/*
 * Use a white, dotted line
 */
	XSetForeground (XtDisplay (Graphics), Gcontext, White.pixel);

	dash[0] = 2;
	dash[1] = 4;

	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, LineOnOffDash, 
		CapButt, JoinMiter);
	XSetDashes (XtDisplay (Graphics), Gcontext, 0, dash, 2);
/*
 * Turn on clipping
 */
	XSetClipRectangles (XtDisplay (Graphics), Gcontext, 0, 0, &Clip, 1, 
		Unsorted);
/*
 * Draw the line
 */
	XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, Trace, 
		Tracelen, CoordModeOrigin);
/*
 * Make the GC use LineSolid again and turn off clipping
 */
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, LineSolid, 
		CapButt, JoinMiter);
	XSetClipRectangles (XtDisplay (Graphics), Gcontext, 0, 0, &Unclip, 1, 
		Unsorted);
/*
 * Find the first point of the trace which lies in the plot region
 */
	for (i = 0; i < Tracelen; i++)
		if (Trace[i].x >= Pix_left && Trace[i].x <= Pix_right &&
			Trace[i].y <= Pix_bottom && Trace[i].y >= Pix_top)
			break;
/*
 * Put the label at this point (or return if there isn't a point
 * in the plot region
 */
	if (i == Tracelen)
	{
		Tracelen = 0;
		return;
	}
	else
	{
		label_x = Trace[i].x;
		label_y = Trace[i].y;
	}
/*
 * Draw the label
 */
	Scratch[0] = ' ';
	strcpyUC (Scratch + 1, name);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, label_x, label_y, 
		Scratch, -90.0, 0.015, JustifyRight, JustifyCenter);
/*
 * Set up to start a new trace
 */
	Tracelen = 0;
}



static void
xs_ColorScale ()
/*
 * Draw a color scale on the side of the plot
 */
{
	int	top, bottom, left, right, i;
	float	bar_height, cval;
/*
 * Side annotation (color scales)
 */
	An_AnnotLimits (&top, &bottom, &left, &right);

	if (Fill_contour)
	{
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
				XSetForeground (XtDisplay (Graphics), Gcontext,
					Colors[i].pixel);
				XFillRectangle (XtDisplay (Graphics), 
					GWFrame (Graphics), Gcontext, left, 
					(int)(top + i * bar_height), 10, 
					(int)(bar_height + 1.5));
			}
		/*
		 * Numeric label
		 */
			cval = Contour_center + 
				(i - Ncolors / 2) * Contour_step;
			sprintf (Scratch, "%.1f", cval);

			XSetForeground (XtDisplay (Graphics), Gcontext, 
				White.pixel);
			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				left + 15, (int)(top + i * bar_height), 
				Scratch, 0.0, 0.02, JustifyLeft, 
				JustifyCenter);
		}
	}
	else
	{
		left += 10;
		top += 5;

		for (i = 0; i < Ncolors; i++)
		{
		/*
		 * Numeric label
		 */
			cval = Contour_center + 
				(i - Ncolors / 2) * Contour_step;
			sprintf (Scratch, "%.1f", cval);

			XSetForeground (XtDisplay (Graphics), Gcontext, 
				Colors[i].pixel);
			DrawText (Graphics, GWFrame (Graphics), Gcontext,
				left, top, Scratch, 0.0, 0.02, JustifyLeft, 
				JustifyTop);
			top += (int)(1.2 * 0.02 *  GWHeight (Graphics));
		}
	}
}



static DataChunk *
xs_GetObsDC (plat, fldname, dtime)
char	*plat, *fldname;
ZebTime *dtime;
/*
 * Find an observation from the given platform before the current plot time
 * and within the user-specified maximum time difference.  Return
 * a good data chunk if this is possible, otherwise return NULL.
 * Return the data time if a real data chunk is returned.
 */
{
	int	diff;
	FieldId	fieldlist[2];
	PlatformId	pid;
	DataChunk	*dc;
/*
 * Get the ID of this platform
 */
	pid = ds_LookupPlatform (plat);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", plat);
		return (NULL);
	}
/*
 * Find the closest data time before PlotTime
 */
	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, dtime))
	{
		TC_EncodeTime (&PlotTime, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "No data for '%s' at %s", plat, Scratch);
		return (NULL);
	}
/*
 * Make sure the data start time is within the user-specified
 * limit (if any)
 */
	diff = PlotTime.zt_Sec - dtime->zt_Sec;
	if (Maxdiff > 0 && diff > Maxdiff)
	{
		msg_ELog (EF_INFO, "No data recent enough from '%s'", plat);
		return (NULL);
	}
/*
 * Get the vertical position (altitude or pressure) and field data
 */
	fieldlist[0] = F_Lookup (Zfld);
	fieldlist[1] = F_Lookup (fldname);

	if (! (dc = ds_FetchObs (pid, DCC_Scalar, dtime, fieldlist, 2,
		NULL, 0)))
	{
		TC_EncodeTime (&PlotTime, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "'%s' or '%s' missing for '%s' at %s", 
			fldname, Zfld, plat, Scratch);
		return (NULL);
	}
	else
		return (dc);
}




static DataChunk *
xs_GetGridDC (plat, fldname, dtime)
char	*plat, *fldname;
ZebTime *dtime;
/*
 * Find the 3d grid for the given platform before the current plot time
 * and within the user-specified maximum time difference.  Return
 * a good data chunk if this is possible, otherwise return NULL.
 * Return the data time if a real data chunk is returned.
 */
{
	int	diff;
	FieldId	fld;
	PlatformId	pid;
	DataChunk	*dc;
/*
 * Get the ID of this platform
 */
	pid = ds_LookupPlatform (plat);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", plat);
		return (NULL);
	}
/*
 * Find the closest data time before PlotTime
 */
	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, dtime))
	{
		TC_EncodeTime (&PlotTime, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "No data for '%s' at %s", plat, Scratch);
		return (NULL);
	}
/*
 * Make sure the data start time is within the user-specified
 * limit (if any)
 */
	diff = PlotTime.zt_Sec - dtime->zt_Sec;
	if (Maxdiff > 0 && diff > Maxdiff)
	{
		msg_ELog (EF_INFO, "No data recent enough from '%s'", plat);
		return (NULL);
	}
/*
 * Get the data
 */
	fld = F_Lookup (fldname);

	if (! (dc = ds_Fetch (pid, DCC_RGrid, dtime, dtime, &fld, 1, NULL, 0)))
	{
		TC_EncodeTime (&PlotTime, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "'%s' missing for '%s' at %s", 
			fldname, plat, Scratch);
		return (NULL);
	}
	else
		return (dc);
}

# endif  /* C_PT_XSECT */
