/*
 * Graphic Plotting Primitive routines.
 * These are intended to be generic and make use of the layout configuration
 * details and transformations out-lined in "LayoutControl"  for the data
 * region to hide details of the X coordinate system from individual
 * Plotting routines.
 */
static char *rcsid = "$Id: PlotPrim.c,v 1.3 1992-01-10 19:20:14 barrett Exp $";
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
gp_Clip (xlo, ylo, xhi, yhi,xscalemode, yscalemode)
DataValPtr	xlo, ylo, xhi, yhi;
unsigned short	xscalemode, yscalemode;
/*
 * Set the clipping window
 */
{
	XRectangle	r;
	int		saveConfig;
/*
 * Build the clip rectangle (XRectangle (x,y) is its upper left corner)
 */
	if ( xscalemode & INVERT )
	{
	    r.x = devX (xhi,xscalemode);
	    r.width = devX (xlo,xscalemode) - r.x + 1;
	}
	else
	{
	    r.x = devX (xlo,xscalemode);
	    r.width = devX (xhi,xscalemode) - r.x + 1;
	}
	if ( yscalemode & INVERT )
	{
	    r.y = devY (ylo,yscalemode);
	    r.height = devY (yhi,yscalemode) - r.y + 1;
	}
	else
	{
	    r.y = devY (yhi,yscalemode);
	    r.height = devY (ylo,yscalemode) - r.y + 1;
	}
/*
 * Put the clip rectangle into the GC
 */
	XSetClipRectangles (XtDisplay (Graphics), Gcontext, 0, 0, &r, 1, 
		Unsorted);
}

void
gp_Pline (x, y, npts, style, color_ndx,xscalemode, yscalemode)
DataValPtr	x, y;
int		npts;
LineStyle	style;
XColor		color_ndx;
unsigned short	xscalemode, yscalemode;
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
	int	x0,x1,y0,y1;

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
		pts[i].x = devX (&(x[i]),xscalemode);
		pts[i].y = devY (&(y[i]),yscalemode);
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

void
gp_WindVector (x, y, u, v, npts, isangle,scale, style, colors, ncolor, cstep,
		xscalemode,yscalemode)
DataValPtr	x, y,u,v;
int		npts;
int		isangle;
double		scale;
LineStyle	style;
XColor		*colors;
int		ncolor;
float		cstep;
unsigned short	xscalemode, yscalemode;
{
    double	radians;
    double	radius;
    int		i,line_style;
    double	upt,vpt;
    char	dash[2];
    Pixel	color;
    float	tangle;
    int		level;

    if ( npts == 0 ) return;


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
    for (i = 0; i < npts; i++)
    {
	if ( isangle )
	{
	    tangle = 270.0 - u[i].val.f;
	    radius = (double)v[i].val.f;
	    radians = (double)tangle/57.2958; /* convert to radians */
	    upt = radius * cos(radians);
	    vpt = radius* sin(radians);
	}
	else
	{
	    upt = (double)u[i].val.f;
	    vpt = (double)v[i].val.f;
	    radius = sqrt(upt*upt + vpt*vpt);
	}
	level = (int)(radius/cstep);
	color = colors[level % ncolor].pixel;
	XSetForeground (XtDisplay (Graphics), Gcontext, color);
	draw_vector ( XtDisplay(Graphics), GWFrame(Graphics),Gcontext,
		devX(&(x[i]),xscalemode),devY(&(y[i]),yscalemode), upt, vpt, scale);
    }
	if (line_style != LineSolid)
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, 
			LineSolid, CapButt, JoinMiter);
}

void
gp_WindBarb (x, y, u, v, npts, isangle,shaftlen, style, colors, ncolor, cstep,
		xscalemode,yscalemode)
DataValPtr	x, y,u,v;
int		npts;
int		isangle;
int		shaftlen;
LineStyle	style;
XColor		*colors;
int		ncolor;
float		cstep;
unsigned short	xscalemode, yscalemode;
{
    double	radians;
    double	radius;
    int		i,line_style;
    double	upt,vpt;
    char	dash[2];
    Pixel	color;
    float	tangle;
    int		level;

    if ( npts == 0 ) return;


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
    for (i = 0; i < npts; i++)
    {
	if ( isangle )
	{
	    tangle = (270.0 - u[i].val.f);
	    radians = (double)tangle/57.2958;
	    radius = (double)v[i].val.f;
	}
	else
	{
	    upt = (double)u[i].val.f;
	    vpt = (double)v[i].val.f;
	    radius = sqrt(upt*upt + vpt*vpt);
	    radians = atan2 ( vpt, upt ) ;
	}
/*
	fprintf ( stdout, "\rradius = %f radians = %f\n",(float)radius,
		(float)radians);
*/
	level = (int)(radius/cstep);
	color = colors[level % ncolor].pixel;
	XSetForeground (XtDisplay (Graphics), Gcontext, color);
	radians = radians + 3.1415926; /* reverse direction */
	draw_barb ( XtDisplay(Graphics), GWFrame(Graphics),Gcontext,
		devX(&(x[i]),xscalemode),devY(&(y[i]),yscalemode), radians, radius, shaftlen);
    }
	if (line_style != LineSolid)
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, 
			LineSolid, CapButt, JoinMiter);
}
void
gp_Points (x, y, npts, color_ndx,xscalemode, yscalemode)
DataValPtr	x, y;
int		npts;
XColor		color_ndx;
unsigned short	xscalemode, yscalemode;
/*
 * ENTRY:
 *	x,y	polyline coordinates in user-coordinate system.
 *	npts	the number of points
 *	color_ndx	index of the color to use
 * EXIT:
 *	The polyline has been drawn
 */
{
	int	i;
	XPoint	*pts;
	char	dash[2];
	Pixel	color;
	int	x0,x1,y0,y1;

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
		pts[i].x = devX (&(x[i]),xscalemode);
		pts[i].y = devY (&(y[i]),yscalemode);
	}
/*
 * Set up the correct foreground color and line style
 */
	color = color_ndx.pixel;
	XSetForeground (XtDisplay (Graphics), Gcontext, color);

/*
 * Draw the points
 */
	XDrawPoints (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, pts, 
		npts, CoordModeOrigin);
/*
 * Free the allocated points
 */
	free (pts);
}
