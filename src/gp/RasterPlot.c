/*
 * Raster display a rectangular array
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

/*
 * This is some of the gnarlier code in gp, in that it tries to do raster
 * plots of lots of data, perhaps involving map projections, in a
 * reasonably quick manner.  Even given that, it could use some work.
 * Someday.
 *
 * There are three different public entry points for rasterization,
 * depending on what needs to be done.  They be:
 *
 * 	RasterPlot
 *		Plots from floating-point data using XFillPolygon
 *		requests (one per grid point).  It's fastest for
 *		small grids, dead slow for large ones.  It understands
 *		projections, though, and is the only option when
 *		projection is being done.
 *
 *	RasterXIPlot
 *		A faster version of RasterPlot, using (possibly shared-
 *		memory) XImages.  It should really use shared memory
 *		pixmaps, but it predates their introduction.  It's
 *		fairly fast for large floating-point grids, but can not
 *		do projections.  The internal routine RP_IRasterize is
 *		called to do the actual dirty work.
 *
 *	RasterImagePlot
 *		The routine to use when image data are to be plotted.
 *		If projections are in use, a kludgy heuristic is used to
 *		decide whether to defer to RP_SlowImagePlot; for "small"
 *		grids projection is ignored.  RP_ImageRasterize is used
 *		to do the dirty work without projection.
 *
 * RP_SlowImagePlot performs projections by reverse-mapping each pixel
 * back into the data space, then grabbing the data value.  It's almost
 * as slow as you would expect it to be.
 *
 * RasterXIPlot and RasterImagePlot should really share almost all of
 * their code, but don't currently.
 *
 * These routines now handle 8, 16, and 32-bit displays.  RP_IRasterize
 * currently does it using XPutPixel, which is not the fastest way of
 * going about things.
 */

# include <errno.h>
# include <math.h>
# include <X11/Intrinsic.h>

# include <config.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include "GraphProc.h"
# include "PixelCoord.h"

RCSID ("$Id: RasterPlot.c,v 2.33 1998-03-23 23:14:20 granger Exp $")

# ifdef TIMING
# include <sys/time.h>
# include <sys/resource.h>
# endif
# include <byteorder.h>
/*
 * Shared memory ximages, if we can.
 */
# ifdef SHM
# 	include <sys/ipc.h>
# 	include <sys/shm.h>
# 	include <X11/extensions/XShm.h>
# endif

# ifdef SHM
static char *shmopt[2] = { "$XShm: Compiled $", (char *)shmopt };
# else
static char *shmopt[2] = { "$XShm: NOT Compiled $", (char *)shmopt };
# endif /* SHM */

/*
 * Color stuff
 */
static int	Ncolor;
static XColor	*Colors, Color_outrange;

/*
 * Highlight color stuff.
 */
static Boolean	Highlight;
static XColor	HColor;
static float	HValue, HRange;

/*
 * Data range for the color scale
 */
static float	Datamin, Datamax, Datarange;

/*
 * Clipping rectangle
 */
static XRectangle	Clip;

/*
 * Useful macros.
 */
# define RPMIN(x, y) (((x) < (y)) ? (x) : (y))
# define RPMAX(x, y) (((x) > (y)) ? (x) : (y))
# define WRAPLON(l) (((l) > 180.0) ? ((l) - 360) : (l))
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */

/*
 * Kludgery of sorts: some of the raster code uses "fake floats" -- 32-bit
 * ints where the least-significant two bytes contain the fractional part
 * of a floating point number.  The point of all this is to allow floating
 * point precision (sort of) precision while making the integer part really
 * fast to get at.
 *
 * Here we give the offset (in shorts) to the integer part of such a number.
 */
# define FF_OFFSET (LittleEndian() ? 1 : 0)


/*--------------------------------------------------
 * Public prototypes
 */



/*--------------------------------------------------
 * Private forwards
 */
static void RP_FPRasterize FP((
	unsigned char *ximp,
	int width, int height, 
	unsigned int *colgrid, 
	double row, double icol, 
	double rowinc, double colinc,
	int xdim, int ydim,
	int pad));

static void RP_IRasterize FP((
	XImage *image,
	unsigned char *ximp,
	int width, int height, 
	unsigned int *colgrid, 
	double row, double icol, 
	double rowinc, double colinc,
	int xdim, int ydim,
	int pad));

static XImage *RP_GetXImage FP((Widget, int, int));

static void RP_ImageRasterize FP ((unsigned char *ximp,
				   int width, int height,
				   unsigned char *grid,
				   Pixel *cmap,
				   double row, double icol, 
				   double rowinc, double colinc,
				   int xdim, int ydim, int pad,
				   int bdepth));

static void RP_MakeColorMap (double, double, Pixel *);
static void RP_FixCMap FP ((Pixel *, int, int));


# ifdef MAP_PROJECTIONS
static void RP_SlowImagePlot FP ((Widget, int, unsigned char *, double, double,
		Location *, RGrid *, Pixel *));
# ifdef notdef
static void RP_FindLimits FP ((Location *, float, int, float, int, int*,
		int *, int *, int *));
# endif
# endif /* MAP_PROJECTIONS */

static int RasterLimits FP ((int xdim, int ydim, int *xlo, int *ylo, int *xhi, 
			     int *yhi, int *width_out, int *height_out,
			     float *colinc_out, float *rowinc_out,
			     float *leftcol_out, float *toprow_out));

# ifdef SHM
static XImage *RP_GetSharedXImage FP ((Widget w, int width, int height));
# endif

/*--------------------------------------------------end prototypes--*/



static void
RP_LLToXPoint (lat, lon, xp)
float lat, lon;
XPoint *xp;
/*
 * Convert a lat/lon into an xpoint structure.
 */
{
	float ux, uy;

	prj_Project (lat, lon, &ux, &uy);
	xp->x = XPIX (ux);
	xp->y = YPIX (uy);
}





static bool
RP_OffScreen (points, np)
XPoint *points;
int np;
/*
 * Return TRUE iff this polygon is completely off screen.  "Off screen"
 * meaning that all of its component points are off screen; we don't try
 * to catch more subtle cases.
 *
 * This is inefficient for it's only current use: RasterPlot.  We check
 * each point twice (actually, up to four times).  Someday...
 */
{
	int p, w = GWWidth (Graphics), h = GWHeight (Graphics);
	for (p = 0; p < np; p++)
		if (points[p].x < 0 || points[p].x >= w ||
				points[p].y < 0 || points[p].y >= h)
			return (TRUE);
	return (FALSE);
}





void
RasterPlot (dc, origin, array, xdim, ydim)
DataChunk *dc;
Location *origin;
float	*array;
int	xdim, ydim;
/*
 * Draw contours of the rectangular (xdim x ydim) array into widget w.
 *
 * 7/95 jc:	This routine has been completely rethrashed to do projection,
 *		meaning that it is even slower than before.  Since nobody
 *		used it before, that is OK, I suppose.  Eventually we will
 *		be able to deal with rotated grids here too.
 */
{
	Drawable d = GWFrame (Graphics);
	int		r_color, i, j;
	float		y0, cscale, lats, lons, xpos, ypos, badval;
	XPoint 	points[4];

# ifdef TIMING
	int msec;
	struct rusage	ru;

	getrusage (RUSAGE_SELF, &ru);
	msec = - ((ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
			(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000);
# endif
/*
 * setup stuff.
 */
	badval = dc_GetBadval (dc);
	GetLLSpacings (dc, &lats, &lons);
	SetClip (FALSE);
/*
 * Figure some quantities to take stuff out of the loop.
 *
 * 3/98 jc: this is wrong (and xpos too), since it can yield points in
 * 	    space for whole-globe grids.
 */
	y0 = origin->l_lat - 0.5*lats;
	cscale = Ncolor/Datarange;
/*
 * Loop through the array points
 */
	for (i = 0; i < xdim; i++)
	{
		float ux, uy;
	/*
	 * Get the position of the left side of the column, and project the
	 * bottom two points.
	 */
		xpos = origin->l_lon + (i - 0.5)*lons;
		RP_LLToXPoint (y0, xpos, points + 2);
		RP_LLToXPoint (y0, xpos + lons, points + 3);
	/*
	 * Now we move up the column making polygons.
	 */
		ypos = y0;
		for (j = 0; j < ydim; j++)
		{
		/*	float dval = array[i*ydim + j]; */
			float dval = array[i + j*xdim];
		/*
		 * Shift down the two lower corners, and figure the two
		 * new upper corners.  Swap them so that we describe the
		 * right path around the polygon.
		 */
			ypos += lats;
			points[1] = points[2];
			points[0] = points[3];
			RP_LLToXPoint (ypos, xpos, points + 2);
			RP_LLToXPoint (ypos, xpos + lons, points + 3);
		/*
		 * If this is a bad point just drop it.  Also if it's
		 * complete off screen.
		 */
			if (dval == badval || RP_OffScreen (points, 4))
				continue;
		/*
		 * Find the correct color and get a graphics context
		 * with the color in the foreground
		 */
			if (Highlight && (dval <= (HValue + HRange/2.0))
				      && (dval >= (HValue - HRange/2.0)))
				FixForeground (HColor.pixel);
			else
			{
				r_color = (int) (cscale * (dval - Datamin));
				if (r_color >= 0 && r_color < Ncolor)
					FixForeground (Colors[r_color].pixel);
				else
					FixForeground (Color_outrange.pixel);
			}
		/*
		 * Draw a polygon for this point
		 */
			XFillPolygon (Disp, d, Gcontext, points, 4, Convex,
					CoordModeOrigin);
		}
	}
# ifdef TIMING
	getrusage (RUSAGE_SELF, &ru);
	msec += (ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
		(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000;
	msg_ELog (EF_INFO, "Rect rastor time = %.3f sec", (float) msec/1000.0);
# endif
}




void
RP_Init (colors, count, c_outrange, clip, dmin, dmax, highlight, hvalue, 
	hcolor, hrange)
XColor	*colors;
int	count;
XColor	c_outrange; 
XRectangle clip;
float	dmin, dmax;
Boolean	highlight;
float	hvalue;
XColor	hcolor;
float	hrange;
/*
 * Initialize colors and data flagging
 *
 * CENTER	center color index
 * COUNT	number of colors in range to use for contouring
 * C_OUTRANGE	color index for contours which fall outside of the specified
 *		range of colors
 * CLIP		XRectangle to use for clipping
 * DMIN		minimum data value for the color scale
 * DMAX		maximum data value for the color scale
 */
{
	Ncolor = count;
	Colors = colors;
	Color_outrange = c_outrange;
	Clip = clip;

	Datamin = dmin;
	Datamax = dmax;
	Datarange = Datamax - Datamin;

	Highlight = highlight;
	if (highlight)
	{
		HValue = hvalue;
		HColor = hcolor;
		HRange = hrange;
	}
}



static int
RasterLimits (xdim, ydim, xlo, ylo, xhi, yhi, 
	      width_out, height_out, colinc_out, rowinc_out,
	      leftcol_out, toprow_out)
int xdim, ydim;
int *xlo, *ylo, *xhi, *yhi;
int *width_out, *height_out;
float *colinc_out, *rowinc_out;
float *leftcol_out, *toprow_out;
/*
 * Adjust pixel limits and floating point grid coordinates.  Return
 * non-zero if the region falls inside the plot, else zero indicates
 * that the raster should not be drawn.  (xlo, ylo) is the lower left
 * corner for grids, so (ylo > yhi) and rowinc will be negative.
 */
{
	float leftcol, toprow, rightcol, bottomrow;
	float rowinc, colinc;
	int width, height;
/*
 * Columns per pixel and rows per pixel.  We use (xdim - 1) and (ydim - 1)
 * here since our pixel limits refer to the *centers* of the edge grid 
 * elements.  These are just estimates so we can figure the pixel coords
 * of the grid edges and clip the grid coordinate limits to the clipping
 * region.
 */
	colinc = ((float) xdim - 1) / ((float) (*xhi - *xlo + 1));
	rowinc = -((float) ydim - 1) / ((float) (*ylo - *yhi + 1));
#ifdef DEBUG
	msg_ELog (EF_INFO, "xdim %d; ydim %d", xdim, ydim);
	msg_ELog (EF_INFO, "estimate: columns/pixel %f; rows/pixel %f", 
		  colinc, rowinc);
#endif
	if (fabs(rowinc) < 0.00001 || fabs(colinc) < 0.00001)
		return (0);
/*
 * Add half a grid width to our dimensions, so that we plot full squares
 * on the edges.  Again, this is because our current limits refer to the
 * centers of the edge grid elements.
 */
	*xlo -= 0.5 / colinc;
	*xhi += 0.5 / colinc;
	*ylo -= 0.5 / rowinc;
	*yhi += 0.5 / rowinc;
/*
 * Align the (floating) row and col coordinates to edges rather than centers.
 * So the left grid column falls in the range [0.0,1.0), and the rightmost
 * column falls in [xdim - 1, xdim).  Likewise the rows are in the ranges
 * [0.0,1.0), [1.0, 2.0), ..., [ydim - 1, ydim).  The idea is that 
 * (int)col and (int)row will return the row and col coordinate in the grid.
 */
	leftcol = 0.0;		/* column (floating) corresponding to xlo */
	rightcol = xdim;
	bottomrow = 0.0;	/* bottom, corresponding to ylo */
	toprow = ydim;
/*
 * Clip to the window, if appropriate.
 */
	if (*xlo < Clip.x)
	{
		leftcol += (Clip.x - *xlo) * colinc;
		*xlo = Clip.x;
	}
	if (*xhi > (int)(Clip.x + Clip.width))
	{
		rightcol -= (*xhi - (int)(Clip.x + Clip.width)) * colinc;
		*xhi = Clip.x + Clip.width;
	}
	if (*yhi < Clip.y)
	{
		toprow += (Clip.y - *yhi) * rowinc;
		*yhi = Clip.y;
	}
	if (*ylo > (int)(Clip.y + Clip.height))
	{
		bottomrow -= (*ylo - (int)(Clip.y + Clip.height)) * rowinc;
		*ylo = Clip.y + Clip.height;
	}
/*
 * Now we can figure the width and height in pixels
 */
	width = *xhi - *xlo + 1;
	height = *ylo - *yhi + 1;
/*
 * If width or height is <= 0, then this raster lies outside the plot
 */
	if (width <= 0 || height <= 0)
	{		
		msg_ELog (EF_DEBUG, "Raster outside the window not plotted");
		return (0);
	}
/*
 * Recalculate grids per pixels with the final official width and height
 * and the clipped grid limits.  Integer truncation when adjusting and
 * clipping the pixel limits makes this necessary.  Otherwise slight
 * inaccuracies in colinc and rowinc can cause grid coordinates to step
 * out of range.
 */
	colinc = (rightcol - leftcol) / width;	/* > 0 */
	rowinc = (bottomrow - toprow) / height;	/* < 0 for grids */
	if (fabs(rowinc) < 0.00001 || fabs(colinc) < 0.00001)
		return (0);
/*
 * Adjust the top grid row coordinate to the bottom of the pixel row (where
 * the row coordinate would end up if we started incrementing by rowinc
 * from zero, as we do for columns).  Since columns start at zero, which is
 * already the left side of the pixel column, we don't need to adjust
 * leftcol.
 */
	toprow += rowinc;
#ifdef DEBUG
	msg_ELog (EF_INFO, "recalc: columns/pixel %f; rows/pixel %f", 
		  colinc, rowinc);
	msg_ELog (EF_INFO, "width: %d; height %d; toprow %f",
		  width, height, toprow);
	/*
	 * Check some assertions
	 */
	if ((int)leftcol < 0 || (int)leftcol >= xdim)
		 msg_ELog (EF_PROBLEM, "leftcol %f out of bounds!", leftcol);
	if ((int)toprow < 0 || (int)toprow >= ydim)
		 msg_ELog (EF_PROBLEM, "toprow %f out of bounds!", toprow);
	if ((int)(leftcol + (width - 1)*colinc) >= xdim)
		msg_ELog (EF_PROBLEM, "rightmost column %f out of bounds",
			  (leftcol + (width - 1)*colinc));
	if ((toprow + (height - 1)*rowinc) < 0.0)
		msg_ELog (EF_PROBLEM, "bottom row %f out of bounds; (int) %d",
			  (toprow + (height - 1)*rowinc),
			  (int)(toprow + (height - 1)*rowinc));
#endif
/*
 * We're done.  Return the parameters.
 */
	*colinc_out = colinc;
	*rowinc_out = rowinc;
	*width_out = width;
	*height_out = height;
	*leftcol_out = leftcol;
	*toprow_out = toprow;
	return (1);
}




void
RasterXIPlot (w, d, array, xdim, ydim, xlo, ylo, xhi, yhi, fast)
Widget	w;
Drawable d;
float	*array;
int	xdim, ydim;
int	xlo, ylo, xhi, yhi;
bool	fast; /* not used any more */
/*
 * Draw a raster image of rectangular (xdim x ydim) array into widget w.
 * The coordinates (xlo,ylo) and (xhi,yhi) specify the centers of the
 * corner elements, in pixel space.  Since they are the centers, we will 
 * actually plot half a grid width outside of these bounds on each side 
 * (neglecting clipping, which may reduce this).
 *
 * This stuff uses shared memory XImages, which made sense at the time,
 * but no longer does.  We should really go straight into the SHM pixmaps,
 * and cut out that copy.  Someday.
 */
{
	int		r_color, i, width, height;
	unsigned int	*colgrid, *cgp;
	float		cscale, *gp;
	float		leftcol, toprow;
	float		rowinc, colinc;
	int		gridelem;
	int		outrange = Color_outrange.pixel;
	unsigned char	*ximp;
	XImage		*image;
#ifdef SHM
	bool		using_shared;
#endif
	GC		gcontext;
	XGCValues	gcvals;
	Display		*disp = XtDisplay (w);

# ifdef TIMING
	int msec;
	struct rusage	ru;

	getrusage (RUSAGE_SELF, &ru);
	msec = - ((ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
			(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000);
# endif
	if (! RasterLimits (xdim, ydim, &xlo, &ylo, &xhi, &yhi, &width, 
			    &height, &colinc, &rowinc, &leftcol, &toprow))
	{
		return;
	}
/*
 * Get a graphics context
 */
	gcontext = XCreateGC (disp, XtWindow (w), 0, &gcvals);
	XSetClipRectangles (disp, gcontext, 0, 0, &Clip, 1, Unsorted);
/*
 * Find the size of the grid, and allocate temporary space to store the
 * computed colors.
 */
	gridelem = xdim*ydim;
	colgrid = (unsigned int *) malloc (gridelem * sizeof (int));
/*
 * Go through and color code everything now.
 */
	cscale = Ncolor/Datarange;
	for (i = 0, gp = array, cgp = colgrid; i < gridelem; i++)
	{
		if (Highlight 
		   && (*gp <= (HValue + HRange/2.0)) 
		   && (*gp >= (HValue - HRange/2.0)))
		{
			++gp;
			*cgp++ = HColor.pixel;
		}
		else
		{
			r_color = (int) (cscale * (*gp++ - Datamin));
			*cgp++ = (r_color >= 0 && r_color < Ncolor) ? 
				Colors[r_color].pixel : outrange;
		}
	}
/*
 * Get our ximage and do the rasterization.
 */
# ifdef SHM
	using_shared = FALSE;
	image = NULL;
	if (GWShmPossible (Graphics))
		image = RP_GetSharedXImage (w, width, height);
	if (image)
		using_shared = TRUE;
	else
# endif
		image = RP_GetXImage (w, width, height);

	ximp = (unsigned char *) image->data;
	RP_IRasterize (image, ximp, width, height, colgrid, toprow, leftcol,
		   rowinc, colinc, xdim, ydim, image->bytes_per_line - width);
/*
 * Now we ship over the image, and deallocate everything.
 */
# ifdef SHM
	if (using_shared)
		XShmPutImage (disp, d, gcontext, image, 0, 0,
			      xlo, yhi, width, height, False);
	else
# endif
	   XPutImage (disp, d, gcontext, image, 0, 0, xlo, yhi, width, height);

	XFreeGC (disp, gcontext);
	free (colgrid);

# ifdef TIMING
	getrusage (RUSAGE_SELF, &ru);
	msec += (ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
		(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000;
	msg_ELog (EF_INFO, "XI raster time = %.3f sec", (float) msec/1000.0);
# endif
}






# ifdef SHM

/*
 * This is our Ximage in shared memory.
 */
static XImage *image = 0;
static int XIwidth = -1, XIheight = -1;
static XShmSegmentInfo shminfo;
static bool shm_failed = FALSE;	/* If we fail once, don't try it again */


static XImage *
RP_GetSharedXImage (w, width, height)
Widget w;
int width, height;
/*
 * Return a shared-memory XImage with this geometry.
 */
{
	Display *disp = XtDisplay (w);
	int depth = GWDepth (Graphics);

	if (shm_failed)
		return (NULL);
/*
 * If the geometry matches, we can just return what we got last time.  This
 * will be the case most of the time -- only when the window changes will
 * this change.
 */
	if (width == XIwidth && height == XIheight)
		return (image);
/*
 * If there is an existing image, get rid of it.
 */
	if (image)
	{
		XShmDetach (disp, &shminfo);
		XDestroyImage (image);
		shmdt (shminfo.shmaddr);
		shmctl (shminfo.shmid, IPC_RMID, 0);
	}
/*
 * Now get a new one.
 */
	image = XShmCreateImage (disp, 0, depth, ZPixmap, 0, &shminfo, width,
			height);
/*
 * All of the shmucking around.
 */
	shminfo.shmid = shmget (IPC_PRIVATE, image->bytes_per_line*height,
		IPC_CREAT | 0777);
/*
 * Check for failures.  More than likely the failures will be from lack of
 * shared memory facilities or high enough shared memory limits in the kernel.
 * Therefore, once we fail once, it behooves us not to try again.
 */
	if (shminfo.shmid < 0)
	{
		shm_failed = TRUE;
		msg_ELog (EF_EMERGENCY, "rp SHM get failure (%d)!", errno);
	}
	else if ((shminfo.shmaddr = (char *) shmat (shminfo.shmid, 0, 0))
		 == (char *) -1)
	{
		shm_failed = TRUE;
		msg_ELog (EF_EMERGENCY, "rp SHM attach failure (%d)!", 
			  errno);
	}
/*
 * Deal with our failures
 */
	if (shm_failed)
	{
		XDestroyImage (image);
		image = NULL;
		msg_ELog (EF_INFO, "Raster: Abandoning XShmExtension attempt");
		return (NULL);
	}
/*
 * Hook everything together.
 */
	image->data = shminfo.shmaddr;
	shminfo.readOnly = False;
	XShmAttach (disp, &shminfo);
/*
 * Save info and return the image.
 */
	XIwidth = width;
	XIheight = height;
	return (image);
}

# endif /* SHM */





void
RP_ZapShmImage (w)
Widget w;
/*
 * If there is a shared memory image, get rid of it.
 */
{
# ifdef SHM
	Display *disp = XtDisplay (w);

	if (image)
	{
		XShmDetach (disp, &shminfo);
		XDestroyImage (image);
		shmdt (shminfo.shmaddr);
		shmctl (shminfo.shmid, IPC_RMID, 0);
		image = 0;
	}
# endif
}





static XImage *
RP_GetXImage (w, width, height)
Widget w;
int width, height;
/*
 * Return an XImage with this geometry.
 */
{
	static int XIwidth = -1, XIheight = -1;
	static XImage *image = 0;
	char *xim;
	int depth = GWDepth (Graphics);
	int bpad = GWBDepth (Graphics)*8;
/*
 * If the geometry matches, we can just return what we got last time.  This
 * will be the case most of the time -- only when the window changes will
 * this change.
 */
	if (width == XIwidth && height == XIheight)
		return (image);
/*
 * If there is an existing image, get rid of it.
 */
	if (image)
		XDestroyImage (image);
/*
 * Now get a new one.
 */
	xim = malloc (width*height*(bpad/8));
	image = XCreateImage (XtDisplay (w),
		DefaultVisual (XtDisplay (w),
			XScreenNumberOfScreen (XtScreen (w))),
		depth, ZPixmap, 0, xim, width, height, bpad, 0);
/*
 * Save info and return the image.
 */
	XIwidth = width;
	XIheight = height;
	return (image);
}



static void
RP_IRasterize (image, ximp, width, height, colgrid, row, icol, rowinc, colinc,
	xdim, ydim, pad)
XImage *image;
unsigned char 	*ximp;
int 		width, height;
unsigned int 	*colgrid;
float 		row, icol, rowinc, colinc;
int		xdim, ydim, pad;
/*
 * Do rasterization using the new integer-based (Sun-fast) method.
 */
{
	int i, j, icolinc;
# ifdef __STDC__
	static int col;
	static short *s_col;
# else
	int col;
	short *s_col;
# endif
	s_col = FF_OFFSET + (short *) &col;
/*
 * Set up our integer values, which are simply the FP values scaled by 
 * 64K, so that the integer part is in the upper two bytes.  We then kludge
 * our way in with the short pointer to pull out the int part directly.
 */
	icolinc = (int) (colinc * 65536);
/*
 * Step through the data.
 */
	for (i = 0; i < height; i++)
	{
		unsigned int *cp = colgrid + ((int) row) * xdim;

		col = (int) (icol * 65536);
		for (j = 0; j < width; j++)
		{
# ifdef NOTTHISWAY
			*ximp++ = cp[*s_col];
# endif
			XPutPixel (image, j, i, cp[*s_col]);
			col += icolinc;
		}
		row += rowinc;
		ximp += pad;	/* End of raster line padding.	*/
	}
}




static void
RP_MakeColorMap (double scale, double bias, Pixel *cmap)
/*
 * Create the data->color map.
 */
{
	int c, rcolor;
	float cscale = Ncolor/Datarange;

	for (c = 0; c <= 254; c++)
	{
		if (Highlight 
		   && ((c*scale + bias) <= (HValue + HRange/2.0)) 
		   && ((c*scale + bias) >= (HValue - HRange/2.0)))
			cmap[c] = HColor.pixel;
		else
		{
			rcolor = (int) (cscale*(c*scale + bias - Datamin));
			cmap[c] = (rcolor >= 0 && rcolor < Ncolor) ? 
				Colors[rcolor].pixel : Color_outrange.pixel;
		}
	}
	cmap[255] = Color_outrange.pixel;
}




/*-----------------------------------------------------------------
 * The following routines do raster plots of data that already
 * have an Image organization.  If shared memory is supported and
 * possible, it is used.
 */


void
RasterImagePlot (w, frame, grid, xd, yd, xlo, ylo, xhi, yhi, scale, bias,
		loc, rg)
Widget		w;
unsigned char 	*grid;
int 		frame, xd, yd, xlo, ylo, xhi, yhi;
float 		scale, bias;
Location *loc;
RGrid *rg;
/*
 * Plot the  (xd x yd) raster image into widget w.
 * The coordinates (xlo,ylo) and (xhi,yhi) specify the centers of the
 * corner elements, in pixel space.  Since they are the centers, we will 
 * actually plot half a grid width outside of these bounds on each side 
 * (neglecting clipping, which may reduce this).
 */
{
	unsigned char *destimg;
	Pixel cmap[256];
	int width, height, bdepth = GWBDepth (Graphics);
	float toprow, leftcol, rowinc, colinc, bottomrow, rightcol;
	GC gcontext;
	XGCValues gcvals;
	Display *disp = XtDisplay(w);
	XImage *image;
/*
 * Go through and make the color map.
 */
	RP_MakeColorMap (scale, bias, cmap);

# ifdef MAP_PROJECTIONS
/*
 * Kludgery.  If (1) we are using a fancy map projection, and (2) this is
 * 	      a "big" image, go off and do it the slow way.
 */
	if (prj_FancyProjection () && rg && (rg->rg_Xspacing*rg->rg_nX) > 1000)
	{
		RP_SlowImagePlot (w, frame, grid, scale, bias, loc, rg, cmap);
		return;
	}
# endif
/*
 * Columns per pixel and rows per pixel.  We use (xd - 1) and (yd - 1)
 * here since our pixel limits refer to the *centers* of the edge grid 
 * elements.
 */
	colinc = ((float) xd - 1) / ((float) (xhi - xlo + 1));
	rowinc = ((float) yd - 1) / ((float) (ylo - yhi + 1));
	if (fabs(rowinc) < 0.00001 || fabs(colinc) < 0.00001)
		return;
/*
 * Add half a grid width to our dimensions, so that we plot full squares
 * on the edges.  Again, this is because our current limits refer to the
 * centers of the edge grid elements.
 */
	xlo -= 0.5 / colinc;
	xhi += 0.5 / colinc;
	leftcol = 0.0;	/* column (floating) corresponding to xlo */
	rightcol = (float) xd;

	ylo += 0.5 / rowinc;
	yhi -= 0.5 / rowinc;
	toprow = 0.0; /* row (floating) corresponding to yhi */
	bottomrow = (float) yd;
/*
 * Clip to the window, if appropriate.
 */
	if (xlo < Clip.x)
	{
		leftcol += (Clip.x - xlo) * colinc;
		xlo = Clip.x;
	}
	if (xhi > (int)(Clip.x + Clip.width))
	{
		rightcol -= (xhi - (int)(Clip.x + Clip.width)) * colinc;
		xhi = Clip.x + Clip.width;
	}

	if (yhi < Clip.y)
	{
		toprow += (Clip.y - yhi) * rowinc;
		yhi = Clip.y;
	}
	
	if (ylo > (int)(Clip.y + Clip.height))
	{
		bottomrow -= (ylo - (int)(Clip.y + Clip.height)) * rowinc;
		ylo = Clip.y + Clip.height;
	}
/*
 * Now we can figure the width and height
 */
	width = xhi - xlo + 1;
	height = ylo - yhi + 1;
/*
 * If width or height is <= 0, then this raster lies outside the plot
 */
	if (width <= 0 || height <= 0)
	{		
		msg_ELog (EF_DEBUG, "Image outside the window not plotted");
		return;
	}
/*
 * Recalculate cells per pixels with the final official width and height.
 * Integer truncation when adjusting and clipping the pixel limits makes
 * this necessary.  Otherwise slight inaccuracies in colinc and rowinc can
 * cause image cell coordinates to step out of range.
 */
	colinc = (rightcol - leftcol) / width;
	rowinc = (bottomrow - toprow) / height;
	if (fabs(rowinc) < 0.00001 || fabs(colinc) < 0.00001)
		return;
/*
 * Find the image space and start rasterizing.  Also force a sync with 
 * the server; things could happen in the wrong order otherwise.
 */
	eq_sync ();
# ifdef SHM
	/*
	 * The raster image will be written directly to the 
	 * server's memory through the Graphics Widget's shared memory,
	 * unless shared memory is not possible... in which case we
	 * must create a local image, process our data, and then
	 * send the image to the server.
	 */
	if (GWFrameShared (Graphics, frame))
	{
		destimg = (unsigned char *) GWGetFrameAddr (w, frame);
		destimg += yhi * GWGetBPL(w, frame) + xlo*bdepth;
		RP_FixCMap (cmap, GWGetByteOrder (w, frame), bdepth);
		RP_ImageRasterize (destimg, width, height, grid, cmap, toprow, 
				   leftcol, rowinc, colinc, xd, yd,
				   GWGetBPL (w, frame) - width*bdepth, bdepth);
	}
	else
# endif
	{
	/*
	 * Get a graphics context
	 */
		gcontext = XCreateGC( disp, XtWindow(w), 0, &gcvals);
		XSetClipRectangles( disp, gcontext, 0, 0, &Clip, 1, Unsorted);
	/*
	 * Get an XImage, fill it, and stuff it in.
	 */
		image = RP_GetXImage(w, width, height);
		RP_FixCMap (cmap, image->byte_order, bdepth);
		RP_ImageRasterize ((unsigned char *)(image->data), 
				   width, height, grid, cmap, toprow, leftcol,
				   rowinc, colinc, xd, yd,
				   image->bytes_per_line - width*bdepth,
				   bdepth);
	/*
	 * Now send our local XImage to the server
	 */
		XPutImage(disp, GWFrame(w), gcontext, image, 0, 0,
			  xlo, yhi, width, height);
		XFreeGC(disp, gcontext);
	}
/*
 * Done!
 */
}





static void
RP_ImageRasterize (ximp, width, height, grid, cmap, row, icol, rowinc, colinc,
		   xdim, ydim, pad, bdepth)
unsigned char 	*ximp;
int 		width, height;
unsigned char 	*grid;
Pixel		*cmap;
float 		row, icol, rowinc, colinc;
int		xdim, ydim, pad, bdepth;
/*
 * Do rasterization using the new integer-based (Sun-fast) method.
 */
{
	int i, j, icolinc;
	unsigned short *simp;
	unsigned long *limp;
# ifdef __STDC__
	static int col;
	static short *s_col;
# else
	int col;
	short *s_col;
# endif
	s_col = FF_OFFSET + (short *) &col;
/*
 * Set up our integer values, which are simply the FP values scaled by 
 * 64K, so that the integer part is in the upper two bytes.  We then kludge
 * our way in with the short pointer to pull out the int part directly.
 */
	icolinc = (int) (colinc * 65536);
/*
 * Step through the data.
 */
	for (i = 0; i < height; i++)
	{
		unsigned char *cp = grid + ((int) row) * xdim;
#ifdef DEBUG
		if (row < 0 || (int)row >= ydim)
			msg_ELog (EF_PROBLEM, "Image: row %f out of bounds",
				  row);
#endif
		col = (int) (icol * 65536);
		switch (bdepth)
		{
		    case 1: /* Eight-bit video */
			for (j = 0; j < width; j++)
			{
				*ximp++ = cmap[cp[*s_col]];
				col += icolinc;
			}
			break;
		    case 2:
			simp = (unsigned short *) ximp;
			for (j = 0; j < width; j++)
			{
				*simp++ = cmap[cp[*s_col]];
				col += icolinc;
			}
			ximp = (unsigned char *) simp;
			break;
		    case 4:
			limp = (unsigned long *) ximp;
			for (j = 0; j < width; j++)
			{
				*limp++ = cmap[cp[*s_col]];
				col += icolinc;
			}
			ximp = (unsigned char *) limp;
			break;
			
		    default:
			msg_ELog (EF_PROBLEM, "Bad bdepth %d", bdepth);
			return;
		}
	/* *ximp++ = cmap[cp[*s_col]]; */
		row += rowinc;
		ximp += pad;	/* End of raster line padding.	*/
	}
#ifdef DEBUG
	if (height && *s_col >= xdim)
		msg_ELog (EF_PROBLEM, "Image: col %f out of bounds",*s_col);
#endif
}



# ifdef MAP_PROJECTIONS


static void
RP_FindLimits (loc, lonstep, nx, latstep, ny, x0, y0, x1, y1)
Location *loc;
float lonstep, latstep;
int nx, ny, *x0, *y0, *x1, *y1;
/*
 * Figure out the area of the window covered by this grid.  This does not
 * yet work right.  For the moment, traverse the border in intervals
 * to find a reasonable bounding box.
 */
{
	int x, y;
	float xk, yk;
	int i;
	float latint, lonint;
#	define NLIMITSTEPS 20

	latint = nx*latstep/NLIMITSTEPS;
	lonint = ny*lonstep/NLIMITSTEPS;
	for (i = 0; i <= NLIMITSTEPS; ++i)
	{
		/* left edge */
		prj_Project (loc->l_lat + i*latint, loc->l_lon,
			     &xk, &yk);
		x = XPIX (xk); y = YPIX (yk);
		*x0 = RPMIN (*x0, x);	*x1 = RPMAX (*x1, x);
		*y0 = RPMIN (*y0, y);	*y1 = RPMAX (*y1, y);
		/* right edge */
		prj_Project (loc->l_lat + i*latint, 
			     WRAPLON (loc->l_lon + nx*lonstep), &xk, &yk);
		x = XPIX (xk); y = YPIX (yk);
		*x0 = RPMIN (*x0, x);	*x1 = RPMAX (*x1, x);
		*y0 = RPMIN (*y0, y);	*y1 = RPMAX (*y1, y);
		/* top edge */
		prj_Project (loc->l_lat + ny*latstep, 
			     WRAPLON (loc->l_lon + i*lonint), &xk, &yk);
		x = XPIX (xk); y = YPIX (yk);
		*x0 = RPMIN (*x0, x);	*x1 = RPMAX (*x1, x);
		*y0 = RPMIN (*y0, y);	*y1 = RPMAX (*y1, y);
		/* bottom edge */
		prj_Project (loc->l_lat, WRAPLON (loc->l_lon + i*lonint),
			     &xk, &yk);
		x = XPIX (xk); y = YPIX (yk);
		*x0 = RPMIN (*x0, x);	*x1 = RPMAX (*x1, x);
		*y0 = RPMIN (*y0, y);	*y1 = RPMAX (*y1, y);
	}
}



static void
RP_SlowImagePlot (w, frame, grid, scale, bias, loc, rg, cmap)
Widget w;
int frame;
unsigned char *grid;
Pixel *cmap;
double scale, bias;
Location *loc;
RGrid *rg;
/*
 * Deal with image plotting the slow way -- needed when map projections
 * are in use.
 */
{
	float olat, olon, latstep, lonstep, lat, lon;
	int shm, nx = rg->rg_nX, ny = rg->rg_nY, yoff, xoff, xp, yp;
	int x0 = 9999, y0 = 9999, x1 = -9999, y1 = -9999, x, y, lwidth;
	GC gcontext;
	unsigned char *destimg;
	Display *disp = XtDisplay(w);
	Location gloc;
	XImage *image;
	XGCValues gcvals;
	const int bdepth = GWBDepth (Graphics);
/*
 * Get our origin, and try to work back to the original lat/lon spacing.
 */
	cvt_GetOrigin (&olat, &olon);
	lonstep = KM_TO_DEG (rg->rg_Xspacing/cos (DEG_TO_RAD (olat)));
	latstep = KM_TO_DEG (rg->rg_Yspacing);
	msg_ELog (EF_DEBUG, "Slowplot, spacings %.2f %.2f", latstep, lonstep);
/*
 * Approximate a bounding box for the image, since the projected points
 * can end up in surprising places.
 */
	RP_FindLimits (loc, lonstep, rg->rg_nX, latstep, rg->rg_nY, &x0, &y0,
		       &x1, &y1);
/*
 * If the image is bigger than our clipping region, don't bother placing
 * points which are outside of that region.
 */
	if (x0 < Clip.x)
		x0 = Clip.x;
	if (x1 > (int)(Clip.x + Clip.width))
		x1 = Clip.x + Clip.width;
	if (y0 < Clip.y)
		y0 = Clip.y;
	if (y1 > (int)(Clip.y + Clip.height))
		y1 = Clip.y + Clip.height;
/*
 * Figure out where we're drawing.
 */
	eq_sync ();
# ifdef SHM
	/*
	 * The raster image will be written directly to the 
	 * server's memory through the Graphics Widget's shared memory,
	 * unless shared memory is not possible... in which case we
	 * must create a local image, process our data, and then
	 * send the image to the server.
	 */
	if (GWFrameShared (Graphics, frame))
	{
		destimg = (unsigned char *) GWGetFrameAddr (w, frame);
		lwidth = GWGetBPL (w, frame);
		shm = TRUE;
		yoff = 0;  /* x and y correspond to graphics frame */
		xoff = x0;
		RP_FixCMap (cmap, GWGetByteOrder (w, frame), bdepth);
	}
	else
# endif
	{
		gcontext = XCreateGC (disp, XtWindow(w), 0, &gcvals);
		XSetClipRectangles (disp, gcontext, 0, 0, &Clip, 1, Unsorted);
		image = RP_GetXImage(w, x1 - x0 + 1, y1 - y0 + 1);
		destimg = (unsigned char *) image->data;
		lwidth = image->bytes_per_line;
		shm = FALSE;
		yoff = y0;  /* x and y must be translated to image coords */
		xoff = 0;
		RP_FixCMap (cmap, image->byte_order, bdepth);
	}
/*
 * Now we plow through the pixels, reverse map each one, and assign the
 * proper data point.  Ugly.
 */
	for (y = y0; y <= y1; y++)
	{
		unsigned char *dest = destimg + (y-yoff)*lwidth + xoff*bdepth;
		unsigned int pval;
		for (x = x0; x <= x1; x++)
		{
			prj_Reverse (XUSER (x), YUSER (y), &lat, &lon);
			if (loc->l_lon > 0 && lon < 0)
				lon += 360;
			xp = (lon - loc->l_lon)/lonstep + 0.5;
			yp = (lat - loc->l_lat)/latstep + 0.5;
		/*
		 * Hate to have this switch so deep, but, compared to
		 * prj_reverse, it should be cheap...
		 */
			pval = (xp >= 0 && xp < nx && yp >= 0 && yp < ny) ?
				 cmap[grid[(ny - yp - 1)*nx + xp]] : cmap[255];
			switch (bdepth)
			{
			    case 1:
				*dest = pval;
				break;
			    case 2:
				* ((unsigned short *) dest) = pval;
				break;
			    case 4:
				* ((unsigned long *) dest) = pval;
			        break;
			    default:
				msg_ELog (EF_PROBLEM, "Bad byte depth");
				return;
			}
			dest += bdepth;
		} /* x loop */
	} /* Y loop */
/*
 * If we doing things with an XImage we need to ship it out.
 */
	if (! shm)
	{
		XPutImage(disp, GWFrame(w), gcontext, image, 0, 0,
			  x0, y0, x1 - x0 + 1, y1 - y0 + 1);
		XFreeGC(disp, gcontext);
	}
}
# endif /* MAP_PROJECTIONS */


static void
RP_FixCMap (cmap, serverorder, bdepth)
Pixel *cmap;
int serverorder, bdepth;
/*
 * Fix up this color map to match the servers byte ordering, if need be.
 */
{
	int cmentry;
	int ourorder = BigEndian () ? MSBFirst : LSBFirst;
	short *scmap;
/*
 * Maybe we don't need to do anything?
 */
	msg_ELog (EF_DEBUG, "FixCMap, us %d server %d depth %d", ourorder,
			serverorder, bdepth);
	if (bdepth == 1 || ourorder == serverorder)
		return;
	msg_ELog (EF_DEBUG, "Flipping...");
/*
 * Oh well.  Flip this one around.
 */
	switch (bdepth)
	{
	    case 2:
		scmap = (short *) cmap;
		if (ourorder == MSBFirst)
			scmap++;	/* To LSB part of word */
		for (cmentry = 0; cmentry < 256; cmentry++)
		{
			swap2 (scmap);
			scmap += 2;
		}
		break;
	    case 4:
		for (cmentry = 0; cmentry < 256; cmentry++)
			swap4 (cmap + cmentry);
		break;
	    default:
		msg_ELog (EF_PROBLEM, "Bad bdepth: %d", bdepth);
		break;
	}
}




# ifdef notdef
/*
 * No real need for this, for now.  Someday maybe we'll want it again
 * so I'll stash it here.
 */

static void
RP_FPRasterize (ximp, width, height, colgrid, row, icol, rowinc, colinc,
		xdim, ydim, pad)
unsigned char	*ximp;
int 		width, height;
unsigned int 	*colgrid;
float 		row, icol, rowinc, colinc;
int 		xdim, ydim, pad;
/*
 * Do rasterization using the old floating point (Ardent vectorizable) 
 * method.
 */
{
	int i, j;
	float col;

#ifdef DEBUG
	msg_ELog (EF_INFO, "entering FPRasterize");
#endif
	for (i = 0; i < height; i++)
	{
		unsigned int *cp = colgrid + ((int) row) * xdim;
#ifdef DEBUG
		/*
		 * It is very possible that the last (bottom) row will be
		 * slightly less than zero due to precision errors.  But
		 * fortunately such coords will still trunc to zero, so it's
		 * not really a problem. 
		 */
		if (row < 0 || (int)row >= ydim)
			msg_ELog (EF_PROBLEM, "FPRaster: row %f out of bounds",
				  row);
#endif
		col = icol;
		for (j = 0; j < width - 1; j++)
		{
			*ximp++ = cp[(int)col];
			col += colinc;
		}
		*ximp++ = cp[(int)col];
		row += rowinc;
		ximp += pad;	/* End of raster line padding.	*/
	}
#ifdef DEBUG
	if (height && (int)col >= xdim)
		msg_ELog (EF_PROBLEM, "FPRaster: col %f out of bounds", col);
#endif
}


# endif



