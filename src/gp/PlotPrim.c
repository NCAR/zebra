/*
 * Graphic Plotting Primitive routines.
 * These are intended to be generic and make use of the layout configuration
 * details and transformations out-lined in "LayoutControl"  for the data
 * region to hide details of the X coordinate system from individual
 * Plotting routines.
 */
static char *rcsid = "$Id: PlotPrim.c,v 1.1 1991-10-30 19:26:00 barrett Exp $";
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


# include <math.h>
# include <X11/Intrinsic.h>
# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include "derive.h"
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "DrawText.h"

/*
 * General definitions
 */
# define BADVAL		-999.0
# define AUTO_XMAX	(1<<0)
# define AUTO_YMAX	(1<<1)
# define AUTO_XMIN	(1<<2)
# define AUTO_YMIN	(1<<3)
# define ROUNDIT(x)	((int)(x + 0.5))

void	gp_Pline (),  gp_Clip () ;
extern int TransConfig;

/*
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

/*
 * Color array and indices
 */
extern XColor	*Colors;
extern int	Ncolors;
extern XColor 	Tadefclr;

# define C_BLACK	0
# define C_WHITE	1
# define C_BG1		2
# define C_BG2		3
# define C_BG3		4
# define C_BG4		5
# define C_DATA(i)	(6 + (i))

void
gp_Clip (xlo, ylo, xhi, yhi)
float	xlo, ylo, xhi, yhi;
/*
 * Set the clipping window
 */
{
	XRectangle	r;
	int		saveConfig;
/*
 * Build the clip rectangle (XRectangle (x,y) is its upper left corner)
 */
	saveConfig = TransConfig;
	TransConfig = 0;
	r.x = devX (xlo);
	r.y = devY (yhi);

	r.width = devX (xhi) - r.x + 1;
	r.height = devY (ylo) - r.y + 1;
/*
 * Put the clip rectangle into the GC
 */
	XSetClipRectangles (XtDisplay (Graphics), Gcontext, 0, 0, &r, 1, 
		Unsorted);
	TransConfig = saveConfig;
}




void
gp_Pline (x, y, npts, style, color_ndx)
float		*x, *y;
int		npts;
LineStyle	style;
XColor		color_ndx;
/*
 * ENTRY:
 *	x,y	polyline coordinates in user-coordinate system.
 *	npts	the number of points
 *	style	line type (solid, dashed, or dotted)
 *	color_ndx	index of the color to use
 * EXIT:
 *	The polyline has been drawn
 */
{
	int	i, line_style;
	XPoint	*pts;
	char	dash[2];
	Pixel	color;

	if (npts == 0)
		return;
/*
 * Allocate the XPoint array
 */
	pts = (XPoint*) malloc (npts * sizeof (XPoint));
/*
 * Fill the pixel location arrays
 */
	for (i = 0; i < npts; i++)
	{
		pts[i].x = devX (x[i]);
		pts[i].y = devY (y[i]);
	}
/*
 * Set up the correct foreground color and line style
 */
	color = color_ndx.pixel;
	XSetForeground (XtDisplay (Graphics), Gcontext, color);

	switch (style)
	{
	    case L_solid:
		line_style = LineSolid;
		break;
	    case L_dashed:
		line_style = LineOnOffDash;
		dash[0] = 6;
		dash[1] = 6;
		break;
	    case L_dotted:
		line_style = LineOnOffDash;
		dash[0] = 2;
		dash[1] = 4;
		break;
	    default:
		msg_ELog (EF_PROBLEM, "Unknown line style %d in gp_Pline\n",
			style);
		line_style = LineSolid;
	}

	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, line_style, 
		CapButt, JoinMiter);
	if (line_style != LineSolid)
		XSetDashes (XtDisplay (Graphics), Gcontext, 0, dash, 2);
/*
 * Draw the line
 */
	XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, pts, 
		npts, CoordModeOrigin);
/*
 * Make the GC use LineSolid again
 */
	if (line_style != LineSolid)
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, 
			LineSolid, CapButt, JoinMiter);
/*
 * Free the allocated points
 */
	free (pts);
}

