/*
 * Display two rectangular arrays (u and v) as wind vectors
 */
static char *rcsid = "$Id: VectorGrid.c,v 2.2 1991-11-14 17:50:55 kris Exp $";
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
# include "../include/defs.h"
# include "../include/message.h"

/*
 * Angle in radians between vector and lines for arrow head
 */
# define ARROWANG	.2618	/* PI / 12 */


/*
 * Macros to reference the data arrays two dimensionally
 */
# define UDATA(i,j)	u_array[(i) * ydim + (j)]
# define VDATA(i,j)	v_array[(i) * ydim + (j)]



VectorGrid (w, d, Gcontext, u_array, v_array, xdim, ydim, xlo, ylo, xhi, yhi, 
	vlen, bad, color)
Widget	w;
Drawable 	d;
GC	Gcontext;
float	*u_array, *v_array, bad;
int	xlo, ylo, xhi, yhi;
int	xdim, ydim;
XColor	color;
float	vlen;
/*
 * Draw wind vectors using the rectangular (xdim x ydim) arrays u_array
 * and v_array into widget w.  The coordinates (xlo,ylo) and (xhi,yhi) 
 * specify the spatial extent of the array with respect to the widget
 * (pixel coordinates). Vlen is the length of a unit vector with respect 
 * to the height of the widget.  Color is the color to use for the vectors.
 */
{
	int		dummy, xpos, ypos, i, j;
	unsigned int	dwidth, dheight, udummy;
	Window		win;
	float		xbottom, ybottom, xstep, ystep;
	float		unitlen;
	XGCValues	gcvals;
/*
 * Find the size of the drawable so we can scale the arrows properly
 */
	XGetGeometry (XtDisplay (w), d, &win, &dummy, &dummy, &dwidth, 
		&dheight, &udummy, &udummy);
/*
 * Find the unit length in pixels
 */
	unitlen = dheight * vlen;
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
		xpos = (int)(xlo + (float) i / (float)(xdim - 1) * 
			(xhi - xlo) + 0.5);

		for (j = 0; j < ydim; j++)
		{
			ypos = (int)(ylo + (float) j / (float)(ydim - 1) *
				(yhi - ylo) + 0.5);
		/*
		 * Check for bad values
		 */
			if ((int) UDATA(i,j) != bad && (int) VDATA(i,j) != bad)
				draw_vector (XtDisplay (w), d, Gcontext, 
					xpos, ypos, UDATA (i, j), VDATA (i, j),
					 unitlen);
		}
	}
}



# ifdef notdef

/*
 * Unit wind length
 */
static float	Unitlen;

/*
 * Bad value flag
 */
static int	Badval;

/*
 * Graphics context
 */
static GC	Gcontext = NULL;

/*
 * The widget and drawable we're using
 */
static Widget	W;
static Drawable	D;

/*
 * Forward declarations
 */
void	VG_DrawVector (), VG_AnnotVector ();


void
VG_DrawVector (x, y, u, v)
int	x, y;
float	u, v;
/*
 * Draw a vector from pixel location (x,y) using the specified floating
 * u and v components.  Scaling of u and v is done here.
 */
{
	float	dx, dy;
	int	xend, yend;
	float	veclen, vecang, ang;
/*
 * Check for bad values
 */
	if ((int) u == Badval || (int) v == Badval)
		return;
/*
 * Draw the shaft of the vector
 */
	dx = u * Unitlen;
	dy = -v * Unitlen;

	xend = (int)(x + dx + 0.5);
	yend = (int)(y + dy + 0.5);

	XDrawLine (XtDisplay (W), D, Gcontext, x, y, xend, yend);
/*
 * If the vector has any length, put on the arrow head
 */
	if (dx != 0 || dy != 0)
	{
		vecang = atan2 (v, u);
		veclen = hypot (u, v);

		ang = vecang + ARROWANG;
		dx = 0.4 * veclen * Unitlen * cos (ang);
		dy = -0.4 * veclen * Unitlen * sin (ang);

		XDrawLine (XtDisplay (W), D, Gcontext, xend, yend, 
			(int)(xend - dx), (int)(yend - dy));


		ang = vecang - ARROWANG;
		dx = 0.4 * veclen * Unitlen * cos (ang);
		dy = -0.4 * veclen * Unitlen * sin (ang);

		XDrawLine (XtDisplay (W), D, Gcontext, xend, yend, 
			(int)(xend - dx), (int)(yend - dy));
	}
}




void
VG_AnnotVector (x, y, u, v, color)
int	x, y;
float	u, v;
Pixel	color;
/*
 * Draw a vector starting at pixel location (x,y) with
 * the given u and v values and in the specified color.
 * (This routine is provided so that routines that call
 * VectorPlot can draw an annotation vector, it
 * can only be called after VectorPlot)
 */
{
	XSetForeground (XtDisplay (W), Gcontext, color);
	VG_DrawVector (x, y, u, v);
}

# endif
