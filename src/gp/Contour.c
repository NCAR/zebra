/*
 * Contour a rectangular array
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

# include <errno.h>
# include <math.h>
# include <X11/Intrinsic.h>

# include <defs.h>
# include <config.h>
# include <message.h>
# include <pd.h>
# include <GraphicsW.h>
# include "GraphProc.h"
# include "Contour.h"
# include "ContourP.h"
# include "DrawText.h"
# include "PixelCoord.h"


RCSID("$Id: Contour.c,v 2.24 2001-12-13 08:59:49 granger Exp $")

typedef short	cbool;

# ifndef PI
# define PI		3.141592654
# endif
# define RAD_TO_DEG(x)	((x)*57.29577951)

# define IXYPAK(i,j)	(((i) << 16) + (j))
# define FRAC(p1,p2)	(((p1) - Cval) / ((p1) - (p2)))

/*
 * Just in case it wasn't defined in config.h.  It also avoids editing
 * a bunch of existing config.h files which don't have a default yet.
 */
#ifndef CFG_GP_MAX_CONTOURS
#define CFG_GP_MAX_CONTOURS 50
#endif

/*
 * x and y offsets for surrounding points of the array, starting
 * with the point to the left and moving clockwise
 */
static int	Inx[] = { -1, -1,  0,  1,  1,  1,  0, -1};
static int	Iny[] = {  0,  1,  1,  1,  0, -1, -1, -1};

/*
 * Global stuff
 */
/* # define MAXPTS	2000 */
# define MAXPTS 8192
static cbool	Closed;		/* Closed contour?		*/
static int	Done[MAXPTS];	/* List of points already done	*/
static int	Ndone;		/* Number of points in the done list	*/
static float	Cval;		/* current contour value	*/
static int	DoLabels;	/* put labels on contours?	*/
static int      Nplotted;	/* Total number of points in contour lines. */

/*
 * Projection-related kludgery.
 */
static int Projecting = FALSE;	/* Fancy projection in use?	*/
static float LatSpacing, LonSpacing;
static Location Origin;
/*
 * The polyline
 */
static int	Nplpts;
static XPoint	Pl[MAXPTS];

/*
 * Label string
 */
static char	Label[16];

/*
 * Toggle so labels on adjacent contours are slightly offset from each
 * other
 */
static int	Ltoggle = FALSE;

/*
 * Other globals used here but not shared by FillContour.c
 */
static float	*Xpos, *Ypos;
static XColor	Color_mono;
static int	Monoflag;

/*
 * Forward declarations
 */
static void	CO_MinMax FP ((float *, float *));
static void	CO_DoContours FP ((void));
static void	CO_DrawContour FP ((void));
static void	CO_FollowContour FP ((int, int, int));
#ifdef notdef
static void	CO_FirstPoint FP ((float, float));
static void	CO_AddPoint FP ((float, float));
#endif
static void	CO_FindXYLoc FP ((int, int, int, int, float *, float *));

/*
 * The size of our drawable
 */
static unsigned int	Dwidth, Dheight;

/*
 * Our current contour color
 */
Pixel	Pix;



void
Contour (w, d, array, xdim, ydim, xlo, ylo, xhi, yhi, ccenter, cstep, 
	dolabels, linewidth)
Widget		w;
Drawable 	d;
float	*array;
int	xdim, ydim;
int	xlo, ylo, yhi, xhi;
double	ccenter, cstep;
int	dolabels, linewidth;
/*
 * Draw contours of the rectangular (xdim x ydim) array into drawable d
 * which must be associated with widget w.  The coordinates (xlo,ylo) and 
 * (xhi,yhi) specify the spatial extent of the array with respect to
 * the widget (pixel coordinates).  The values ccenter and cstep are 
 * the center contour value and the contour spacing, respectively.
 * Put labels on the contours if dolabels is true.  The width in pixels
 * to use for line drawing is linewidth (0 or 1 will give a one pixel
 * wide line).
 */
{
	int		cndx, cndx_min, cndx_max, dummy, i;
	float		min, max, min_ndx_f, max_ndx_f;
	unsigned int	udummy;
	Window		win;
/*
 * Initialize
 */
	W = w;
	D = d;
	Nx = xdim;
	Ny = ydim;
	Z = array;
	DoLabels = dolabels;
	Nplotted = 0;
/*
 * Get the width and height of our drawable (they are used in
 * determining how far apart to space contour labels)
 */
	XGetGeometry (XtDisplay (W), D, &win, &dummy, &dummy, &Dwidth, 
		&Dheight, &udummy, &udummy);
/*
 * Traverse the array to find the min and max
 */
	CO_MinMax (&min, &max);
	if (max == min)
	{
		msg_ELog (EF_INFO,
			"Constant surface in contour, nothing drawn");
		return;
	}
/*
 * Floating point index limits
 */
	min_ndx_f = (cstep > 0) ? 
	    (min - ccenter) / cstep : (max - ccenter) / cstep;
	max_ndx_f = (cstep > 0) ? 
	    (max - ccenter) / cstep : (min - ccenter) / cstep;
/*
 * Integer max and min contour indices
 * Limit ourselves to six out-of-range contours on either side of 
 * the color limits.
 */
	cndx_min = (int) ceil (min_ndx_f);
	cndx_max = (int) floor (max_ndx_f);

	if(! Monoflag)
	{
		if (cndx_min < -(Ncolor / 2 + 6))
			cndx_min = -(Ncolor / 2 + 6);

		if (cndx_max > (Ncolor / 2 + 6))
			cndx_max = (Ncolor / 2 + 6);
	}
/*
 * Graphics context stuff.
 */
	if (! ContourGC)
		ContourGC = XCreateGC (XtDisplay (W), XtWindow (W), 0, NULL);

	if (linewidth == 1)
		linewidth = 0;
	XSetLineAttributes (XtDisplay (W), ContourGC, linewidth, LineSolid, 
			    CapButt, JoinMiter);
	XSetClipRectangles (XtDisplay (W), ContourGC, 0, 0, &Clip, 1,Unsorted);
/*
 * Sanity test
 */
	if ((cndx_max - cndx_min + 1) > CFG_GP_MAX_CONTOURS)
	{
		msg_ELog (EF_PROBLEM, 
			  "Contour: %s (%d). Limit is %d. Nothing drawn.",
			  "Too many contours", cndx_max - cndx_min + 1, 
			  CFG_GP_MAX_CONTOURS);
		return;
	}
/*
 * Build arrays for index -> location lookups
 */
	if (! Projecting)
	{
		Xinc = (float)(xhi - xlo) / (Nx - 1);
		Yinc = (float)(yhi - ylo) / (Ny - 1);
		
		Xpos = (float *) malloc (Nx * sizeof (float));
		Ypos = (float *) malloc (Ny * sizeof (float));

		for (i = 0; i < Nx; i++)
			Xpos[i] = xlo + Xinc * i;
		for (i = 0; i < Ny; i++)
			Ypos[i] = ylo + Yinc * i;
	}
/*
 * Loop through the contour values
 */
	for (cndx = cndx_min; cndx <= cndx_max; cndx++)
	{
		Cval = ccenter + cndx * cstep;
	/*
	 * Assign the color and grab a graphics context with this
	 * color in the foreground and with the user's clip rectangle
	 */
		if (Monoflag)
			Pix = Color_mono.pixel;
		else if ((Color_center + cndx) >= 0 &&
			 (Color_center + cndx) < Ncolor)
			Pix = Colors[Color_center + cndx].pixel;
		else
		{
			if (! Do_outrange)
				continue;
			
			Pix = Color_outrange.pixel;
		}

		XSetForeground (XtDisplay (W), ContourGC, Pix);
	/*
	 * Labeling stuff
	 */
		if (DoLabels)
		{
		    LabelStep (Label, cstep, Cval);
		/*
		 * Alternate the toggle so that adjacent contours have slightly
		 * offset labels
		 */
		    Ltoggle = ! Ltoggle;
		}
	/*
	 * Find the contours for this contour value
	 */
		CO_DoContours ();
	}
/*
 * Check whether anything was plotted.
 */
	if (Nplotted < 10)
	{
	    msg_ELog (EF_PROBLEM, "few (%d) contour segments: "
		      "check interpolation grid resolution", Nplotted);
	}
/*
 * Release the allocated space
 */
	if (! Projecting)
	{
		free (Xpos);
		free (Ypos);
	}
}




void
CO_Init (colors, count, center, c_outrange, clip, flagged, flagval)
int	center, count, flagged;
XColor	*colors, *c_outrange;
XRectangle	clip;
double	flagval;
/*
 * Initialize colors and data flagging
 *
 * CENTER	center color index
 * COUNT	number of colors in range to use for contouring
 * C_OUTRANGE	color index for contours which fall outside of the specified
 *		range of colors (pass NULL to suppress drawing
 *		contours outside the range represented by the color map)
 * CLIP		XRectangle structure describing the rectangle to use
 *		for clipping the plot
 * FLAGGED	Boolean value telling whether the bad values in the data
 * 		will be flagged
 * FLAGVAL	the bad value used to identify flagged data
 */
{
	Projecting = FALSE;	/* One can always hope */
/*
 * Center color index and number of colors.
 */
	Monoflag = FALSE;
	Color_center = center;
	Ncolor = count;
	Colors = colors;
/*
 * Color index for out-of-range contours
 */
	if (c_outrange)
	{
		Do_outrange = TRUE;
		Color_outrange = *c_outrange;
	}
	else
		Do_outrange = FALSE;
/*
 * Clip rectangle
 */
	Clip = clip;
/*
 * Handle data flagging, if any
 */
	Use_flag = flagged;
	Badflag = flagval;
}


void
CO_InitMono (color, clip, flagged, flagval)
int	flagged;
XColor	color;
XRectangle	clip;
double	flagval;
/*
 * Initialize monochrome color and data flagging
 *
 * CLIP		XRectangle structure describing the rectangle to use
 *		for clipping the plot
 * FLAGGED	Boolean value telling whether the bad values in the data
 * 		will be flagged
 * FLAGVAL	the bad value used to identify flagged data
 */
{
	Projecting = FALSE;	/* One can always hope */
/*
 * Set the monochrome flag to true and save the color.
 */
	Monoflag = TRUE;
	Color_mono = color;
/*
 * Clip rectangle
 */
	Clip = clip;
/*
 * Handle data flagging, if any
 */
	Use_flag = flagged;
	Badflag = flagval;
}




void
CO_ProjSetup (origin, lats, lons)
Location *origin;
float lats, lons;
/*
 * We're doing projection, so get set up to do it.  This guy needs to
 * be called after one of the CO_*Init functions.
 */
{
	Projecting = TRUE;
	Origin = *origin;
	LatSpacing = lats;
	LonSpacing = lons;
}





static void
CO_MinMax (min, max)
float	*min, *max;
/*
 * Find the min and max values in the data array
 */
{
	int	i;

	*min = *max = Badflag;
/*
 * Start with the first good value
 */
	for (i = 0; i < Nx * Ny; i++)
	{
		if (Use_flag && Z[i] == Badflag)
			continue;
		else if (! FINITE(Z[i]))
			continue;
		else
		{
			*max = Z[i];
			*min = Z[i];
			break;
		}
	}

	if (i == Nx * Ny)
	{
		msg_ELog (EF_INFO, "No good values in array to be contoured!");
		return;
	}
/*
 * Find the max and min
 */
	for (; i < Nx * Ny; i++)
	{
		if (Use_flag && Z[i] == Badflag)
			continue;
		if (! FINITE(Z[i]))
			continue;
		if (Z[i] > *max)
			*max = Z[i];
		if (Z[i] < *min)
			*min = Z[i];
	}

	return;
}




static void
CO_DoContours ()
/*
 * Contour the array.
 * The logic here is taken from the CONREC module of the NCAR Plot Package.
 * Specifically, this routine corresponds to STLINE.
 */
{
	int	ip1, i, jp1, j, ixy, k;
	cbool	already;
/*
 * Initialize
 */
	Ndone = 0;
/*
 * Look for the beginning of open contours that cross the top or bottom
 */
	Closed = FALSE;

	for (ip1 = 1; ip1 < Nx; ip1++)
	{
		i = ip1 - 1;
	/*
	 * If a contour crosses at either top or bottom at this i, follow 
	 * it to completion
	 */
		if (ZVAL (i, 0) < Cval && ZVAL (ip1, 0) >= Cval)
			CO_FollowContour (ip1, 0, 0);

		if (ZVAL (ip1, Ny - 1) < Cval && ZVAL (i, Ny - 1) >= Cval)
			CO_FollowContour (i, Ny - 1, 4);
	}
/*
 * Look for the beginning of open contours that cross the left or right side
 */
	for (jp1 = 1; jp1 < Ny; jp1++)
	{
		j = jp1 - 1;
	/*
	 * If a contour crosses either side at this j, follow it to
	 * completion
	 */
		if (ZVAL (Nx - 1, j) < Cval && ZVAL (Nx - 1, jp1) >= Cval)
			CO_FollowContour (Nx - 1, jp1, 6);

		if (ZVAL (0, jp1) < Cval && ZVAL (0, j) >= Cval)
			CO_FollowContour (0, j, 2);
	}
/*
 * Look for the beginning of closed contours
 */
	Closed = TRUE;
	for (jp1 = 2; jp1 < Ny; jp1++)
	{
		j = jp1 - 1;
		for (ip1 = 1; ip1 < Nx; ip1++)
		{
			i = ip1 - 1;
		/*
		 * Try to start a contour if it crosses here
		 */
			if (ZVAL (i, j) < Cval && ZVAL (ip1, j) >= Cval)
			{
				ixy = IXYPAK (ip1, j);
			/*
			 * Make sure we haven't done this one already
			 */
				already = FALSE;
				for (k = 0; k < Ndone; k++)
					if (Done[k] == ixy)
					{
						already = TRUE;
						break;
					}
				if (already)
					continue;
			/*
			 * Mark this location as done
			 */
				if (Ndone == MAXPTS)
				{
					msg_ELog (EF_PROBLEM, 
						"Too many contour points!");
					return;
				}

				Done[Ndone++] = ixy;
			/*
			 * Follow the contour to completion
			 */
				CO_FollowContour (ip1, j, 0);
			}
		}
	}
/*
 * Free up allocated space and return
 */
	return;
}




static void
CO_FirstPoint (x, y)
float	x, y;
/*
 * Start a new polyline
 */
{
	Nplpts = 1;
	Pl[0].x = (int)(x + 0.5);
	Pl[0].y = (int)(y + 0.5);
}



static void
CO_AddPoint (x, y)
float	x, y;
/*
 * Add a point to the polyline
 */
{
	XPoint	lastpt;

	if (Nplpts == MAXPTS)
	{
		lastpt = Pl[Nplpts-1];
		CO_DrawContour ();
		Pl[0] = lastpt;
		Nplpts = 1;
	}

	Pl[Nplpts].x = (int)(x + 0.5);
	Pl[Nplpts].y = (int)(y + 0.5);
	Nplpts++;
}




static void
CO_FollowContour (ix, iy, iseg)
int	ix, iy, iseg;
/*
 * Trace a contour line when given the beginning by CO_DoContours.
 * This routine uses the logic of DRLINE in the CONREC module of the NCAR
 * plot package.
 */
{
	int	ix0 = ix, iy0 = iy, iseg0 = iseg, idx, idy;
	int	ix2, iy2, ix3, iy3, ix4, iy4, isbig;
	float	x, y, xprev = 0.0, yprev = 0.0;
	cbool	draw, drawprev, first;
/*
 * Get the other endpoint of this segment
 */
	idx = Inx[iseg];
	idy = Iny[iseg];

	ix2 = ix + idx;
	iy2 = iy + idy;
/*
 * Check for bad values
 */
	draw = (! Use_flag) || 
		(ZVAL (ix, iy) != Badflag && ZVAL (ix2, iy2) != Badflag);
	draw = draw && FINITE(ZVAL (ix, iy)) && FINITE(ZVAL (ix2, iy2));
	drawprev = draw;
/*
 * Get the first point
 */
	CO_FindXYLoc (ix, iy, idx, idy, &x, &y);
	CO_FirstPoint (x, y);
	first = TRUE;
/*
 * Find all the points for the contour
 */
	while (first || ix != ix0 || iy != iy0 || iseg != iseg0)
	{
	/*
	 * Look for the next point to use
	 */
		while (TRUE)
		{
		/*
		 * Find the endpoint for the next segment to test
		 */
			iseg++;
			iseg %= 8;

			idx = Inx[iseg];
			idy = Iny[iseg];
			ix2 = ix + idx;
			iy2 = iy + idy;
		/*
		 * For open contours, we can stop when we hit an edge
		 */
			if (! Closed && 
			    (ix2 >= Nx || iy2 >= Ny || ix2 < 0 || iy2 < 0))
			{
				CO_DrawContour ();
				return;
			}
		/*
		 * Test this segment.  We've found the next point if the 
		 * contour value is greater than the endpoint value and 
		 * 'iseg' is even.
		 */
			if (Cval <= ZVAL (ix2, iy2))
			{
				iseg += 4;
				ix = ix2;
				iy = iy2;
			}
			else if ((iseg % 2) == 0)
				break;
		}
	/*
	 * We have the segment that contains the next point; deal
	 * with it.
	 */
		isbig = (iseg < 2) ? iseg + 8 : iseg;
		ix3 = ix + Inx[isbig - 1];
		iy3 = iy + Iny[isbig - 1];
		ix4 = ix + Inx[isbig - 2];
		iy4 = iy + Iny[isbig - 2];

		drawprev = draw;
	/*
	 * We can stop an open contour when we hit an edge
	 */
		if (! Closed)
		{
			if (ix3 >= Nx || iy3 >= Ny || ix3 < 0 || iy3 < 0 ||
			    ix4 >= Nx || iy4 >= Ny || ix4 < 0 || iy4 < 0)
			{
				CO_DrawContour ();
				return;
			}
		}
	/*
	 * Bad value test
	 */
		draw = FINITE(ZVAL (ix, iy)) && FINITE(ZVAL (ix2, iy2)) &&
			FINITE(ZVAL (ix3, iy3)) && FINITE(ZVAL (ix4, iy4));
		if (Use_flag)
			draw = draw && ZVAL(ix, iy) != Badflag && 
				ZVAL(ix2, iy2) != Badflag &&
				ZVAL(ix3, iy3) != Badflag && 
				ZVAL(ix4, iy4) != Badflag;
	/*
	 * Find the (x,y) location of the point
	 */
		CO_FindXYLoc (ix, iy, idx, idy, &x, &y);
	/*
	 * Dump out an old contour if need be, then save this new point.
	 */
		if (draw)
		{
			if (! drawprev)
			{
				CO_DrawContour ();
				CO_FirstPoint (xprev, yprev);
			}
			CO_AddPoint (x, y);
		}

		first = FALSE;
		xprev = x;
		yprev = y;

		if (iseg == 0)
		{
			if (Ndone == MAXPTS)
			{
				msg_ELog (EF_PROBLEM, 
					"Too many contour points!");
				return;
			}
			Done[Ndone++] = IXYPAK (ix, iy);
		}
	}
/*
 * Finish up
 */
	CO_DrawContour ();
	return;
}





static void
CO_FindXYLoc (ix, iy, idx, idy, x, y)
int ix, iy, idx, idy;
float *x, *y;
/*
 * Turn this set of indices and offsets into a pixel location.
 */
{
/*
 * This calculation seems a little strange, in that it would appear to
 * ignore the diagonal cases when both of id[xy] are nonzero.  Only the
 * "even" cases (with one of them zero) are actually used to draw
 * contours, however.
 *
 * If projection is in use, we have to do this the slow way.
 */
	if (Projecting)
	{
		float lat = Origin.l_lat + iy*LatSpacing;
		float lon = Origin.l_lon + ix*LonSpacing;
		if (idx != 0)
			prj_Project (lat, lon + idx*LonSpacing*
				 FRAC(ZVAL(ix, iy), ZVAL(ix+idx, iy)), x, y);
		else
			prj_Project (lat + idy*LatSpacing*
				 FRAC(ZVAL(ix, iy), ZVAL(ix, iy+idy)), lon,
				 x, y);
		*x = XPIX (*x);
		*y = YPIX (*y);
	}
/*
 * The non-projecting case uses the constant, predefined positions.
 */
	else
	{
		if (idx != 0)
		{
			*y = Ypos[iy];
			*x = Xpos[ix] + 
				 Xinc * idx * 
				 FRAC (ZVAL (ix, iy), ZVAL (ix + idx, iy));
		}
		else
		{
			*x = Xpos[ix];
			*y = Ypos[iy] +
				 Yinc * idy * 
				 FRAC (ZVAL (ix, iy), ZVAL (ix, iy + idy));
		}
	}
}




static void
CO_DrawContour ()
/*
 * Draw the contour polyline which has been built
 */
{
	int	prev, start, i, label_now, count, hjust;
	float	del_x, del_y;
	float	angle, charsize, dist;
	int	x0, y0, x1, y1;
	float	test1, test2, fudge;
/*
 * Check for non-existent lines
 */
	if (Nplpts <= 1)
	{
		Nplpts = 0;
		return;
	}
	Nplotted += Nplpts - 1;
/*
 * It's easy if we're not doing labels
 */
	if (! DoLabels)
	{
		XDrawLines (XtDisplay (W), D, ContourGC, Pl, Nplpts, 
			CoordModeOrigin);
		Nplpts = 0;
		return;
	}
/*
 * Set the character size
 */
	charsize = 0.020;
/*
 * Set things up as if we've gone some distance through a contour, so 
 * we get the first label quickly
 */
	dist = Ltoggle ? 0.445 : 0.495;
/*
 * Run through the points of the contour, drawing polylines and inserting
 * labels where appropriate 
 */
	start = 0;
	count = 1;
	label_now = FALSE;

	for (i = 1; i < Nplpts; i++)
	{
		count++;

		if (! label_now)
		{
		/*
		 * This point will be in the polyline.  See if it takes
		 * us far enough to do the next label
		 */
			prev = i - 1;
			del_x = Pl[i].x - Pl[prev].x;
			del_y = Pl[i].y - Pl[prev].y;

			dist += hypot (del_x / Dwidth, del_y / Dheight);

			if (dist > 0.5)
			{
			/*
			 * We're far enough for the next label, draw 
			 * the line up to here
			 */
				label_now = TRUE;
				XDrawLines (XtDisplay (W), D, ContourGC,
					Pl + start, count, CoordModeOrigin);

				dist = 0.0;
				start = i;
				count = 1;
			}
		}
		else
		{
		/*
		 * See if we can fit the label between 'start' and the current 
		 * point
		 */
			del_x = Pl[i].x - Pl[start].x;
			del_y = Pl[i].y - Pl[start].y;

			if (del_x == 0.0 && del_y == 0.0)
				continue;

			angle = ATAN2 (-del_y, del_x);

			if (angle > PI/2)
			{
				hjust = JustifyRight;
				angle -= PI;
			}
			else if (angle < -PI/2)
			{
				hjust = JustifyRight;
				angle += PI;
			}
			else
				hjust = JustifyLeft;
		/*
		 * Get the text box that would result if we put the label here
		 */
			DT_TextBox (W, D, Pl[start].x, Pl[start].y, Label, 
				RAD_TO_DEG (angle), charsize, hjust,
				JustifyCenter, &x0, &y0, &x1, &y1);
		/*
		 * Go on to the next point if the current point would be 
		 * inside the text box
		 *
		 * We use the fudge factor in the test because text_box returns
		 * values which are rounded to the nearest pixel location
		 */
			test1 = ATAN2 ((double)(y0 - Pl[i].y), 
				(double)(Pl[i].x - x0)) - angle;
			while (test1 < 0.0)
				test1 += 2 * PI;

			test2 = ATAN2 ((double)(y1 - Pl[i].y), 
				(double)(Pl[i].x - x1)) - angle - PI;
			while (test2 < 0.0)
				test2 += 2 * PI;

			fudge = 0.4;	/* angle fudge factor */
			if (test1 < (PI/2 + fudge) && test2 < (PI/2 + fudge))
				continue;
		/*
		 * This point is good, write in the label and continue the
		 * line from just after the label
		 */
			DT_StrokeText (W, D, ContourGC, Pl[start].x, 
				Pl[start].y, Label, RAD_TO_DEG (angle), 
				charsize, hjust, JustifyCenter);

			if (hjust == JustifyLeft)
			{
				Pl[i-1].x = x1 + 0.5 * charsize * Dheight * 
					sin (angle);
				Pl[i-1].y = y1 + 0.5 * charsize * Dheight * 
					cos (angle);
			}
			else
			{
				Pl[i-1].x = x0 - 0.5 * charsize * Dheight *
					sin (angle);
				Pl[i-1].y = y0 - 0.5 * charsize * Dheight *
					cos (angle);
			}


			label_now = FALSE;
			start = i - 1;
			count = 2;
		}
	}
/*
 * Draw the remainder of the line
 */
	if (count > 1)
	{
		XDrawLines (XtDisplay (W), D, ContourGC, Pl + start, 
			count, CoordModeOrigin);
	}
/*
 * Reset
 */
	Nplpts = 0;
}
