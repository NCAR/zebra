/*
 * Display two rectangular arrays (u and v) as wind vectors
 */
static char *rcsid = "$Id: VectorGrid.c,v 2.6 1994-04-15 21:26:38 burghart Exp $";
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
# include <message.h>
# include <pd.h>
# include "GraphProc.h"
# include "PixelCoord.h"

/*
 * Angle in radians between vector and lines for arrow head
 */
# define ARROWANG	.2618	/* PI / 12 */

/*
 * Is x between a and b (inclusive)?
 */
# define BETWEEN(x,a,b)	((((x)-(a))*((x)-(b))) <= 0)



void
WindGrid (w, d, Gcontext, u_array, v_array, xdim, ydim, xlo, ylo, xhi, yhi, 
	  vlen, bad, color, degrade, vector)
Widget	w;
Drawable 	d;
GC	Gcontext;
float	*u_array, *v_array;
int	xdim, ydim, xlo, ylo, xhi, yhi;
float	vlen, bad;
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
	unsigned int	dwidth, dheight, udummy;
	Window		win;
	float		xbottom, ybottom, xstep, ystep, u, v;
	float		unitlen;
	XGCValues	gcvals;
/*
 * Find the size of the drawable so we can scale the arrows properly
 */
	XGetGeometry (XtDisplay (w), d, &win, &dummy, &dummy, &dwidth, 
		      &dheight, &udummy, &udummy);
/*
 * Find the unit length or shaft length in pixels
 */
	unitlen = dheight * vlen;
	shaftlen = (int)(dheight * vlen);
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
	if (Gcontext == NULL)
	{
		gcvals.foreground = color.pixel;
		Gcontext = XCreateGC (XtDisplay (w), XtWindow (w), 
				      GCForeground, &gcvals);
	}
	else
		XSetForeground (XtDisplay (w), Gcontext, color.pixel);
	

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
				draw_vector (XtDisplay (w), d, Gcontext, xpos, 
					     ypos, u, v, unitlen);
			else
				draw_barb (XtDisplay (w), d, Gcontext, xpos, 
					   ypos, atan2 (-v, -u), hypot (u, v),
					   shaftlen, FALSE);
		}
	}
}



void
VectorGrid (w, d, Gcontext, u_array, v_array, xdim, ydim, xlo, ylo, xhi, yhi, 
	    vlen, bad, color, degrade)
Widget	w;
Drawable 	d;
GC	Gcontext;
float	*u_array, *v_array;
int	xdim, ydim, xlo, ylo, xhi, yhi;
float	vlen, bad;
XColor	color;
int	degrade;
{
/*
 * Just call WindGrid, telling it we want vectors
 */
	WindGrid (w, d, Gcontext, u_array, v_array, xdim, ydim, xlo, ylo, xhi,
		  yhi, vlen, bad, color, degrade, TRUE);
}




void
BarbGrid (w, d, Gcontext, u_array, v_array, xdim, ydim, xlo, ylo, xhi, yhi, 
	  vlen, bad, color, degrade)
Widget	w;
Drawable 	d;
GC	Gcontext;
float	*u_array, *v_array;
int	xdim, ydim, xlo, ylo, xhi, yhi;
float	vlen, bad;
XColor	color;
int	degrade;
{
/*
 * Just call WindGrid, telling it we want barbs
 */
	WindGrid (w, d, Gcontext, u_array, v_array, xdim, ydim, xlo, ylo, xhi,
		  yhi, vlen, bad, color, degrade, FALSE);
}

