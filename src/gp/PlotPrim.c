/*
 * Graphic Plotting Primitive routines.
 * These are intended to be generic and make use of the layout configuration
 * details and transformations out-lined in "LayoutControl"  for the data
 * region to hide details of the X coordinate system from individual
 * Plotting routines.
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
# include <config.h>


# include <math.h>
# include <X11/Intrinsic.h>

# include <defs.h>
# include <draw.h>
# include <pd.h>
# include <GraphicsW.h>
# include <message.h>
# include <DataStore.h>
# include <met_formulas.h>
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "DrawText.h"
# include "PlotPrim.h"

RCSID("$Id: PlotPrim.c,v 1.14 1997-10-16 20:41:37 burghart Exp $")


static int PP_LineWidth = 0;


void
pp_Clip (xlo, ylo, xhi, yhi, fudgebot)
DataValPtr	xlo, ylo, xhi, yhi;
int	fudgebot;
/*
 * Set the clipping window
 */
{
	int		dx0 = devX (xlo), dx1 = devX (xhi);
	int		dy0 = devY (ylo), dy1 = devY (yhi);
	XRectangle	r;
/*
 * Build the rectangle
 */
	r.x = (dx0 < dx1) ? dx0 : dx1;
	r.width = abs (dx1 - dx0);

	r.y = (dy0 < dy1) ? dy0 : dy1;
	r.height = abs (dy1 - dy0);
/*
 * jc -- Move the bottom clip down if desired.  This is here to make
 *	 wind profiles more useful by allowing the bottommost barbs
 *	 to be seen even if they go below the grid.
 */
	if (fudgebot)
		r.height += FUDGEAMOUNT;
/*
 * Put the clip rectangle into the GC
 */
	XSetClipRectangles (Disp, Gcontext, 0, 0, &r, 1, Unsorted);
}


void
pp_UnClip ()
/*
 * Clear clipping from the graphics context
 */
{
	XSetClipMask (Disp, Gcontext, None);
}	



int
pp_SetLWidth (comp, param, qual, def)
char *comp, *param, *qual;
int def;
/*
 * Set the line width in Gcontext;
 */
{
	int lwidth;

	if (! pda_Search (Pd, comp, param, qual, (char *) &lwidth, SYMT_INT))
		lwidth = def;
	/* FixLWidth (lwidth); */
	PP_LineWidth = lwidth;
	return (lwidth);
}




void
pp_Pline (x, y, npts, style, color_ndx)
DataValPtr	x, y;
int		npts;
LineStyle	style;
Pixel		color_ndx;
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
		pts[i].x = devX (x + i);
		pts[i].y = devY (y + i);
	}
/*
 * Set up the correct foreground color and line style
 */
	XSetForeground (Disp, Gcontext, color_ndx);

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
		msg_ELog (EF_PROBLEM, "Unknown line style %d in pp_Pline\n",
			style);
		line_style = LineSolid;
	}

	XSetLineAttributes (Disp, Gcontext, PP_LineWidth, line_style, 
			    CapButt, JoinMiter);
	if (line_style != LineSolid)
		XSetDashes (Disp, Gcontext, 0, dash, 2);
/*
 * Draw the line
 */
	XDrawLines (Disp, GWFrame (Graphics), Gcontext, pts, npts, 
		    CoordModeOrigin);
/*
 * Make the GC use LineSolid and default width again
 */
	if (line_style != LineSolid || PP_LineWidth != 0)
		XSetLineAttributes (Disp, Gcontext, 0, LineSolid, 
				    CapButt, JoinMiter);
/*
 * Free the allocated points
 */
	free (pts);
}




void
pp_WindVector (x, y, u, v, npts, isangle, scale, style, colors, ncolor, cstep)
DataValPtr	x, y,u,v;
int		npts;
int		isangle;
double		scale;
LineStyle	style;
XColor		*colors;
int		ncolor;
float		cstep;
{
	double	radians;
	double	radius;
	int	i, line_style, level;
	double	upt, vpt;
	char	dash[2];
	float	tangle;

	if (npts == 0) 
		return;


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
		msg_ELog (EF_PROBLEM, "Unknown line style %d in pp_Pline\n",
			style);
		line_style = LineSolid;
	}

	XSetLineAttributes (Disp, Gcontext, 0, line_style, 
		CapButt, JoinMiter);

	if (line_style != LineSolid)
		XSetDashes (Disp, Gcontext, 0, dash, 2);

	for (i = 0; i < npts; i++)
	{
		if ( isangle )
		{
			tangle = 270.0 - u[i].val.f;
			radius = (double)v[i].val.f;
			radians = (double)tangle/57.2958;
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
		if (level >= ncolor)
			level = ncolor - 1;
		XSetForeground (Disp, Gcontext, colors[level].pixel);
		draw_vector (Disp, GWFrame(Graphics), Gcontext, devX (x + i),
			     devY (y + i), upt, vpt, scale);
	}

	if (line_style != LineSolid)
		XSetLineAttributes (Disp, Gcontext, 0, LineSolid, CapButt, 
				    JoinMiter);
}




void
pp_WindBarb (x, y, u, v, npts, isangle,shaftlen, style, colors, ncolor, cstep,
	     doKnot)
DataValPtr	x, y,u,v;
int		npts;
int		isangle;
int		shaftlen;
LineStyle	style;
XColor		*colors;
int		ncolor;
float		cstep;
int		doKnot;
{
    double	radians;
    double	radius;
    int		i,line_style;
    double	upt,vpt;
    char	dash[2];
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
		msg_ELog (EF_PROBLEM, "Unknown line style %d in pp_Pline\n",
			style);
		line_style = LineSolid;
	}

	XSetLineAttributes (Disp, Gcontext, 0, line_style, 
		CapButt, JoinMiter);
	if (line_style != LineSolid)
		XSetDashes (Disp, Gcontext, 0, dash, 2);
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
	    radians = ATAN2 ( vpt, upt ) ;
	}

	level = doKnot ? (int)((radius/.5148)/cstep): (int)(radius/cstep);
	if (level >= ncolor)
		level = ncolor - 1;
	XSetForeground (Disp, Gcontext, colors[level].pixel);
	radians = radians + 3.1415926; /* reverse direction */
	draw_barb ( Disp, GWFrame(Graphics), Gcontext, devX (x + i), 
		   devY (y + i), radians, radius, shaftlen, doKnot);
    }

    if (line_style != LineSolid)
	XSetLineAttributes (Disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
}




void
pp_RGBtoHLS (r,g,b,h,l,s)
double	r, g, b; /* range [0,1] */
float	*h, *l, *s; /* hue = [0,360], lightness & saturation = [0,1] 
		     except if s == 0, h = undefined */
{
    float	max,min;
    float	undefined = 0.0;
    float	rc,bc,gc;
    max = r > g ? r : g;
    max = max > b ? max : b;
    min = r < g ? r : g;
    min = min < b ? min : b;
    *l = (max+min)/2.0;		/*lightness*/

    /*
     * Calculate saturation and hue
     */
    if ( max == min ) /* achromatic r==g==b */
    {
	*s = 0.0;
	*h = undefined;
    }
    else	      /* chromatic */
    {
	if ( *l <= 0.5 )
	    *s = (max-min)/(max+min);
	else
	    *s = (max-min)/(2.0-max-min);

	rc = (max-r)/(max-min);
	gc = (max-g)/(max-min);
	bc = (max-b)/(max-min);
	if ( r == max ) 
	    *h = bc-gc;
	else if ( g == max ) 
	    *h = 2 + rc - bc;
	else if ( b == max )
	    *h = 4 + gc -rc;
	*h = *h * 60.0;  /* convert to degrees */
	if ( *h < 0.0 ) *h = *h + 360.0;
    }
}




float
additiveColor ( n1,n2, hue )
float	n1,n2,hue;
{
    if ( hue > 360.0 ) hue = hue - 360;
    if ( hue < 0.0 ) hue = hue + 360;
    if ( hue < 60.0 ) 
	return ( n1 + (n2-n1)*hue/60.0 );
    else if ( hue < 180.0 ) 
	return ( n2 );
    else if ( hue < 240.0 ) 
	return ( n1 + (n2-n1)*(240.0-hue)/60.0 );
    else
	return ( n1 );
    
}




void
pp_HLStoRGB(r,g,b,h,l,s)
float	*r,*g,*b;
double	h,l,s;
{
    float	m1,m2;
    float	undefined = 0.0;
    if ( l <= 0.5 ) 
	m2 = l *(1.0+s);
    else
	m2 = l + s - l*s;
    m1 = 2*l -m2;
    if ( s == 0.0 )
    {
	if ( h == undefined )
	    *r = *g = *b = l;
	else
	    fprintf ( stderr, "\rbad h,l,s values\n");
    }
    else
    {
	*r = additiveColor( m1,m2, h + 120);
	*g = additiveColor( m1,m2, h );
	*b = additiveColor( m1,m2, h - 120);
    }
}




void
pp_Icons (x, y, npts, icon, color_ndx, comp, platform)
DataValPtr	x, y;
int		npts;
Pixel		color_ndx;
char		*icon, *comp, *platform;
/*
 * ENTRY:
 *	x,y	polyline coordinates in user-coordinate system.
 *	npts	the number of points
 *	color_ndx	index of the color to use
 *	icon		name of the icon to use for the points
 *	comp		name of the current PD component
 *	platform	name of the platform
 * EXIT:
 *	The polyline has been drawn
 */
{
	bool	makeActive;
	int	i, xhot, yhot, w, h;
	int	xPix,yPix;
	Pixmap	pmap;
	XGCValues	vals;
/*
 * Make sure we have a good icon name
 */
	if (! (pmap = I_GetPMap (icon, &xhot, &yhot, &w, &h)))
		return;
/*
 * Do we want active icons?
 */
	makeActive = FALSE;
	pda_Search (Pd, comp, "active-icon", platform, &makeActive, SYMT_BOOL);
/*
 * Set some GC stuff
 */
	vals.foreground = color_ndx;
	vals.fill_style = FillStippled;
	vals.stipple = pmap;
	XChangeGC (Disp, Gcontext, GCForeground|GCFillStyle|GCStipple, &vals); 
/*
 * Draw the symbol for each point, making it active if necessary
 */
	for (i = 0; i < npts; i++)
	{
		xPix = devX (x + i) - xhot;
		yPix = devY (y + i) - yhot;

		XSetTSOrigin (Disp, Gcontext, xPix, yPix);
		XFillRectangle (Disp, GWFrame (Graphics), Gcontext, xPix, 
				yPix, w, h);

		if (makeActive)
			I_ActivateArea (xPix, yPix, w, h, "pp_icons", comp, 
					platform, 0);
	}
/*
 * Make the GC use FillSolid again before we leave.
 */
	vals.fill_style = FillSolid;
	XChangeGC (Disp, Gcontext, GCFillStyle, &vals); 
}
