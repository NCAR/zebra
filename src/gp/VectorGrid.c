/*
 * Display two rectangular arrays (u and v) as wind vectors
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
# include <math.h>
# include <X11/Intrinsic.h>
# include <defs.h>
# include <draw.h>
# include <message.h>
# include <pd.h>
# include <GraphicsW.h>
# include "GraphProc.h"
# include "PixelCoord.h"

RCSID ("$Id: VectorGrid.c,v 2.10 1995-09-23 02:33:11 granger Exp $")

/*
 * Angle in radians between vector and lines for arrow head
 */
# define ARROWANG	.2618	/* PI / 12 */

/*
 * Is x between a and b (inclusive)?
 */
# define BETWEEN(x,a,b)	((((x)-(a))*((x)-(b))) <= 0)



void
WindGrid (w, d, gc, u_array, v_array, xdim, ydim, xlo, ylo, xhi, yhi, 
	  vlen, bad, color, degrade, vector)
Widget	w;
Drawable 	d;
GC	gc;
float	*u_array, *v_array;
int	xdim, ydim, xlo, ylo, xhi, yhi;
double	vlen, bad;
XColor	color;
int	degrade, vector;
/*
 * Draw winds using the rectangular (xdim x ydim) arrays u_array and
 * v_array into widget w.  The coordinates (xlo,ylo) and (xhi,yhi) specify
 * the spatial extent of the array with respect to the widget (pixel
 * coordinates). Vlen is the length of a unit vector (for vectors) or the
 * shaft length (for barbs) with respect to the height of the widget.
 * Color is the color to use for the vectors.  If 'vector' is true, we draw
 * vectors, otherwise barbs.
 */
{
	int		dummy, xpos, ypos, i, j;
	int		pa_left, pa_right, pa_bottom, pa_top, shaftlen;
	Window		win;
	float		xstep, ystep, u, v;
	float		unitlen;
/*
 * Find the unit length or shaft length in pixels
 */
	unitlen = GWHeight (Graphics)*vlen;
	shaftlen = (int)(GWHeight (Graphics)*vlen);
/*
 * Pixels per grid step in each direction
 */
	xstep = (xdim > 1) ? (float)(xhi - xlo) / (float)(xdim - 1) : 0.0;
	ystep = (ydim > 1) ? (float)(yhi - ylo) / (float)(ydim - 1) : 0.0;
/*
 * Pixel limits of the allowed plotting area of the screen
 */
	pa_left = XPIX (Xlo);
	pa_right = XPIX (Xhi);
	pa_bottom = YPIX (Ylo);
	pa_top = YPIX (Yhi);
/*
 * Loop through the array points
 */
	XSetForeground (XtDisplay (w), gc, color.pixel);

	for (i = 0; i < xdim; i++)
	{
		xpos = (int)(xlo + i * xstep + 0.5);
	/*
	 * Move on if this column of vectors begins outside the plotting area
	 */
		if (! BETWEEN (xpos, pa_left, pa_right))
			continue;

		for (j = 0; j < ydim; j++)
		{
			ypos = (int)(ylo + j * ystep + 0.5);

			if (degrade > 1 && ((i % degrade) || (j % degrade)))
				continue;
		/*
		 * Don't plot vectors which start outside the plotting area
		 */
			if (! BETWEEN (ypos, pa_bottom, pa_top))
				continue;
		/*
		 * Get our u and v and check for bad values
		 */
			u = u_array[i * ydim + j];
			v = v_array[i * ydim + j];

			if (u == bad || v == bad)
				continue;
		/*
		 * Draw the vector or barb
		 */
			if (vector)
				draw_vector (XtDisplay (w), d, gc, xpos, 
					     ypos, u, v, unitlen);
			else
				draw_barb (XtDisplay (w), d, gc, xpos, 
					   ypos, ATAN2 (-v, -u), hypot (u, v),
					   shaftlen, FALSE);
		}
	}
}




void
WindProjGrid (u_array, v_array, xdim, ydim, origin, lats, lons, vscale,
		badvalue, color, degrade, vector)
float *u_array, *v_array;
int xdim, ydim, degrade, vector;
Location *origin;
float lats, lons, vscale, badvalue;
Pixel color;
/*
 * Handle vector grid plotting in situations where projection is in
 * use.  This routine will work for all (CAP) grids regardless of projection,
 * but is slower.  XSections need the above (pixel-based) version.
 *
 * This function differs from WindGrid in a number of ways, in the hopes
 * of doing things better.  In particular, no GC is passed in, and the
 * array stays in row-major order.
 *
 * Entry:
 *	u_array, v_array are the winds arrays.
 *	xdim, ydim are the dimensions of those arrays
 *	origin is the grid origin
 *	lats, lons are the original latitude/longitude spacing of the grids
 *	vscale is the vector scale to use in plotting
 *	badvalue is the bad value flag
 *	color is the PIXEL value (not XColor as above) to use
 *	degrade is the thinning to use, if any
 *	vector is TRUE iff the data are to be plotted as vectors (otherwise
 *		they get barbs)
 * Exit:
 *	A heroic effort has been made to plot the data.
 */
{
	int x, y, shaftlen, pa_left, pa_right, pa_top, pa_bottom;
	float unitlen, ux, uy;
	Drawable d = GWFrame (Graphics);
/*
 * Find the unit length or shaft length in pixels
 */
	unitlen = GWHeight (Graphics)*vscale;
	shaftlen = (int)(GWHeight (Graphics)*vscale);
/*
 * Pixel limits of the allowed plotting area of the screen.  We do the
 * clipping ourselves because we want to clip the entire vector/barb or not
 * depending on whether its base falls within the clip area.
 */
	pa_left = XPIX (Xlo);
	pa_right = XPIX (Xhi);
	pa_bottom = YPIX (Ylo);
	pa_top = YPIX (Yhi);
/*
 * Get the color right.
 */
	XSetForeground (Disp, Gcontext, color);
/*
 * Time to start plotting.
 */
	for (y = 0; y < ydim; y++)
	{
		float lat = origin->l_lat + y*lats;
		for (x = 0; x < xdim; x++)
		{
			int px, py;
			float u = *u_array++, v = *v_array++;
		/*
		 * So where the hell are we?
		 */
			prj_Project (lat, origin->l_lon + x*lons, &ux, &uy);
			px = XPIX (ux);
			py = YPIX (uy);
		/*
		 * If we're out of bounds, bail.  Also bail for bad flags.
		 */
			if (! BETWEEN (px, pa_left, pa_right) ||
					! BETWEEN (py, pa_bottom, pa_top) ||
					u == badvalue || v == badvalue)
				continue;
		/*
		 * Plot this one.
		 */
			if (vector)
				draw_vector (Disp, d, Gcontext, px, py, u, v,
						unitlen);
			else
				draw_barb (Disp, d, Gcontext, px, py,
						ATAN2 (-u, -v), hypot (u, v),
						shaftlen, FALSE);
		}
	}
}







# ifdef notdef
/*
 * As far as I can tell, nobody calls these dudes.....
 */
void
VectorGrid (w, d, gc, u_array, v_array, xdim, ydim, xlo, ylo, xhi, yhi, 
	    vlen, bad, color, degrade)
Widget	w;
Drawable 	d;
GC	gc;
float	*u_array, *v_array;
int	xdim, ydim, xlo, ylo, xhi, yhi;
float	vlen, bad;
XColor	color;
int	degrade;
{
/*
 * Just call WindGrid, telling it we want vectors
 */
	WindGrid (w, d, gc, u_array, v_array, xdim, ydim, xlo, ylo, xhi,
		  yhi, vlen, bad, color, degrade, TRUE);
}




void
BarbGrid (w, d, gc, u_array, v_array, xdim, ydim, xlo, ylo, xhi, yhi, 
	  vlen, bad, color, degrade)
Widget	w;
Drawable 	d;
GC	gc;
float	*u_array, *v_array;
int	xdim, ydim, xlo, ylo, xhi, yhi;
float	vlen, bad;
XColor	color;
int	degrade;
{
/*
 * Just call WindGrid, telling it we want barbs
 */
	WindGrid (w, d, gc, u_array, v_array, xdim, ydim, xlo, ylo, xhi,
		  yhi, vlen, bad, color, degrade, FALSE);
}
# endif /* notdef */
