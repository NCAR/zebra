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
 * Contour a rectangular array using color filled areas
 *
 * The array is contoured by breaking it into triangles so that the
 * contours are uniquely determined.  The nx by ny array is divided 
 * into (nx-1)*(ny-1) triangles thus:
 *
 *	ny-1 --------------------- . . . . -----------
 *	     |0,ny-2,1/|1,ny-2,1/|         |   <--------- nx-2,ny-2,1
 *	     |     _/  |     _/  |         |     _/  |
 *	     |   _/    |   _/    |         |   _/    |
 *	     | _/      | _/      |         | _/    <----- nx-2,ny-2,0
 *	     |/0,ny-2,0|/1,ny-2,0|         |/        |
 *	ny-2 --------------------- . . . . -----------
 *           .         .         .         .         .
 *           .         .         .         .         .
 *           .         .         .         .         .
 *           .         .         .         .         .
 *           .         .         .         .         .
 *	   2 --------------------- . . . . -----------
 *	     | 0,1,1 _/| 1,1,1 _/|         |nx-2,1,1/|
 *	     |     _/  |     _/  |         |     _/  |
 *	     |   _/    |   _/    |         |   _/    |
 *	     | _/      | _/      |         | _/      |
 *	     |/  0,1,0 |/  1,1,0 |         |/nx-2,1,0|
 *	   1 --------------------- . . . . -----------
 *	     | 0,0,1 _/| 1,0,1 _/|         |nx-2,0,1/|
 *	     |     _/  |     _/  |         |     _/  |
 *	     |   _/    |   _/    |         |   _/    |
 *	     | _/      | _/      |         | _/      |
 *	     |/  0,0,0 |/  1,0,0 |         |/nx-2,0,0|
 *	   0 --------------------- . . . . -----------
 *	     0         1         2        nx-2      nx-1
 *
 * Each triangle is designated by the x,y coordinates of its lower left corner
 * and an index (either zero or one) to tell whether it is the lower right
 * or the upper left triangle in the rectangle.  The designation for each 
 * triangle is shown.
 */

# include <errno.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include "Contour.h"

/*
 * Triangle and vertex structures
 */
typedef struct
{
	int	i, j, k;
} triangle;

typedef struct
{
	int	i, j;
} vertex;

/*
 * Is a triangle within the bounds of the array?
 */
# define LEGAL_TRI(tri)	((tri.i)>=0&&(tri.i)<=Nx-2&&(tri.j)>=0&&(tri.j)<=Ny-2)

/*
 * XPoint array to hold a polygon
 */
# define MAXPTS	5
static XPoint	Points[MAXPTS];
static int	Npt;

/*
 * Pixel location of the lower left corner
 */
int	X0, Y0;

/*
 * Forward declarations
 */
void	FC_Init (), FC_MinMax (), FC_DoContour (), FC_AddPoint ();




FillContour (w, d, array, xdim, ydim, xlo, ylo, xhi, yhi, ccenter, cstep)
Widget	w;
Drawable 	d;
int	xlo, ylo, xhi, yhi;
float	*array, ccenter, cstep;
int	xdim, ydim;
/*
 * Draw filled contours of the rectangular (xdim x ydim) array into 
 * widget w.  The coordinates (xlo,ylo) and (xhi,yhi) specify the 
 * spatial extent of the array with respect to the widget (pixel 
 * coordinates).  The values ccenter and cstep are the center contour 
 * value and the contour spacing, respectively.
 */
{
	int	cndx, cndx_min, cndx_max;
	Pixel	foreground;
	float	min, max, cval;
	triangle	tri;
/*
 * Initialize
 */
	W = w;
	D = d;
	Z = array;
	Nx = xdim;
	Ny = ydim;
	X0 = xlo;
	Y0 = ylo;
/*
 * Build arrays of pixel locations for each array point
 */
	Xinc = (float)(xhi - xlo) / (Nx - 1);
	Yinc = (float)(yhi - ylo) / (Ny - 1);
/*
 * Traverse the array to find the min and max
 */
	FC_MinMax (&min, &max);
	if (max == min)
	{
		msg_log ("Constant surface in contour, nothing drawn");
		return;
	}
/*
 * Make sure we have a GC
 */
	if (! Gcontext)
		Gcontext = XCreateGC (XtDisplay (W), XtWindow (W), 0, NULL);
/*
 * Loop through the contour values
 */
	cndx_min = (int) floor ((min - ccenter) / cstep);
	cndx_max = (int) floor ((max - ccenter) / cstep);

	for (cndx = cndx_min; cndx <= cndx_max; cndx++)
	{
		cval = ccenter + cndx * cstep;
	/*
	 * Assign the color and grab a graphics context with this 
	 * color in the foreground and with the user's clip area
	 */
		if (ABS (cndx) > (Ncolor - 1) / 2)
			foreground = Color_outrange.pixel;
		else
			foreground = Colors[Color_center + cndx].pixel;

		XSetForeground (XtDisplay (W), Gcontext, foreground);
		XSetClipRectangles (XtDisplay (W), Gcontext, 0, 0, &Clip, 1, 
			Unsorted);
	/*
	 * Loop through the triangles
	 */
		for (tri.i = 0; tri.i < Nx - 1; tri.i++)
		    for (tri.j = 0; tri.j < Ny - 1; tri.j++)
			for (tri.k = 0; tri.k < 2; tri.k++)
				FC_DoContour (tri, cval, cstep);
	}
}




void
FC_Init (colors, count, center, c_outrange, clip, flagged, flagval)
int	center, count, flagged;
XColor	*colors, c_outrange;
XRectangle	clip;
float	flagval;
/*
 * Initialize colors and data flagging
 *
 * CENTER	center color index
 * COUNT	number of colors in range to use for contouring
 * C_OUTRANGE	color for contours which fall outside of the specified
 *		range of colors
 * CLIP		XRectangle structure describing the rectangle to use
 *		for clipping the plot
 * FLAGGED	Boolean value telling whether the bad values in the data
 * 		will be flagged
 * FLAGVAL	the bad value used to identify flagged data
 */
{
/*
 * Center color index and number of colors.
 * Make sure Ncolor is odd, so the Color_center is actually at the center
 */
	Color_center = center;
	Ncolor = (count % 2) ? count : count - 1;
	Colors = colors;
/*
 * Color index for out-of-range contours
 */
	Color_outrange = c_outrange;
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
FC_MinMax (min, max)
float	*min, *max;
/*
 * Find the min and max values in the data array
 */
{
	int	i;
/*
 * Start with the first good value
 */
	for (i = 0; i < Nx * Ny; i++)
	{
		if (Use_flag && Z[i] == Badflag)
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
		*min = *max = 0.0;
		msg_log ("No good values in array to be contoured!");
		return;
	}
/*
 * Find the max and min
 */
	for (; i < Nx * Ny; i++)
	{
		if (Use_flag && Z[i] == Badflag)
			continue;
		if (Z[i] > *max)
			*max = Z[i];
		if (Z[i] < *min)
			*min = Z[i];
	}

	return;
}




void
FC_DoContour (tri, cval, cstep)
triangle	tri;
float	cval, cstep;
/*
 * Do a contour fill in this triangle for the given contour value
 */
{
	int	lower_right;
	int	side, ep0, ep1;
	float	v_val[3], val0, val1, frac, ctop, x, y;
	vertex	v[3];
/*
 * Return if the triangle is outside the defined boundaries
 */
	if (! LEGAL_TRI (tri))
		return;
/*
 * Get the vertices of the triangle.
 * The points are defined such that sides are consistent for both
 * lower right and upper left triangles.
 *
 *	Vertices	Side defined
 *	----------------------------------------
 *	 2 and 0	horizontal side (side 0)
 *	 0 and 1	diagonal side	(side 1)
 *	 1 and 2	vertical side	(side 2)
 */
	lower_right = (tri.k == 0);

	if (lower_right)	/* lower right triangle */
	{
		v[0].i = tri.i;		v[0].j = tri.j;
		v[1].i = tri.i + 1;	v[1].j = tri.j + 1;
		v[2].i = tri.i + 1;	v[2].j = tri.j;
	}
	else			/* upper left triangle */
	{
		v[0].i = tri.i + 1;	v[0].j = tri.j + 1;
		v[1].i = tri.i;		v[1].j = tri.j;
		v[2].i = tri.i;		v[2].j = tri.j + 1;
	}

	v_val[0] = ZVAL (v[0].i, v[0].j);
	v_val[1] = ZVAL (v[1].i, v[1].j);
	v_val[2] = ZVAL (v[2].i, v[2].j);
/*
 * Bail out if any vertex is bad
 */
	if (Use_flag &&
		(v_val[0]==Badflag || v_val[1]==Badflag || v_val[2]==Badflag))
		return;
/*
 * Bail out if we don't need to draw anything in this triangle
 */
	ctop = cval + cstep;

	if (v_val[0] < cval && v_val[1] < cval && v_val[2] < cval)
		return;
	else if (v_val[0] > ctop && v_val[1] > ctop && v_val[2] > ctop)
		return;
/*
 * Run through the sides of the triangle to see if the contour
 * crosses them.  If so, add a point to our polyline list
 */
	Npt = 0;

	for (side = 0; side < 3; side++)
	{
	/*
	 * Get the data values at either endpoint of the side.
	 */
		ep1 = side;
		ep0 = (side == 0) ? 2 : side - 1;

		val0 = v_val[ep0];
		val1 = v_val[ep1];
	/*
	 * If endpoint 0 has a value greater than cval, add it to the 
	 * path
	 */
		if (val0 > cval)
		{
			x = v[ep0].i;
			y = v[ep0].j;
			FC_AddPoint (x, y);
		}
	/*
	 * If the contour doesn't cross this side, go on to the next side
	 */
		if ((cval < val0 && cval < val1)||(cval > val0 && cval > val1))
			continue;	
	/*
	 * Fractional distance along side where contour crosses
	 */
		if (val1 != val0)
			frac = (cval - val0) / (val1 - val0);
		else
			frac = 0.0;
	/*
	 * Add a point to the list for this contour
	 */
		x = v[ep0].i + (v[ep1].i - v[ep0].i) * frac;
		y = v[ep0].j + (v[ep1].j - v[ep0].j) * frac;
		FC_AddPoint (x, y);
	}
/*
 * Do the fill
 */
	XFillPolygon (XtDisplay (W), D, Gcontext, Points, Npt, Convex, 
		CoordModeOrigin);
}




void
FC_AddPoint (x, y)
float	x, y;
/*
 * Add a point to the polygon vertex list
 */
{
	Points[Npt].x = (short)(X0 + x * Xinc + 0.5);
	Points[Npt].y = (short)(Y0 + y * Yinc + 0.5);
	Npt++;
}
