/*
 * The grid overlay code, moved out of Overlay.c
 */
/*		Copyright (C) 1995 by UCAR
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

# include <string.h>
# include <math.h>
# include <X11/Intrinsic.h>

# include <config.h>
# include <defs.h>
# include <pd.h>
# include <GraphicsW.h>
# include <message.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"

RCSID ("$Id: Ov_Grid.c,v 2.6 1996-03-12 17:41:32 granger Exp $")

# define BETWEEN(x,a,b)    ((((x)-(a))*((x)-(b))) <= 0)

/*
 * Some macros.  I'm not sure they are all used here.
 */
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */
# define TOLERANCE  (0.0001)
#define WidthPixPerInch(screen) \
	WidthOfScreen(screen)/(WidthMMOfScreen(screen)/25.4)
#define HeightPixPerInch(screen) \
	(HeightOfScreen(screen)/(HeightMMOfScreen(screen)/25.4))

/*
 * The maximum number of segments we want to draw in a curve.
 */
# define MAXSEG 100



static void	ov_CGFixLL FP ((float *, float *, float *, float *, float *,
			float *, double, double));
static void	ov_SGSetup FP ((char *, float *, float *, float *, bool *,
			int *, bool *, float *, float *));
static void 	ov_SolidGrid FP ((int, int, int, int, double, double,
			double, double, double, double));
static void 	ov_LLSolidGrid FP ((int, int, int, int, double, double,
				    double, double));
static int	ov_DoSegAndLabel FP ((double, double, double, double, int, int,
				      int, int, char *, double));
static void	ov_TicGrid FP ((int, int, int, int, double, double,
			double, double, int, double, double));
static void	ov_LLTicGrid FP ((int, int, int, int, double, double,
				double, double, int));

/*
 * A couple of useful number crunchers.
 */
static inline int
Round (x)
float x;
{
	return ((x<0) ? -(int)(-x+0.5) : (int)(x+0.5));
}


static inline int 
DegreesInterval (deg, iv)
float deg;
float iv;
{
/*
 * Convert and round to seconds and compare as ints
 */
	return ((Round (deg * 3600) % Round (iv * 3600)) == 0);
}


/*
 * Return the next greater float which is a multiple of the given interval
 */
#ifdef notdef
#define FloatTrunc(v, interval) (((v) < 0) ? \
				 (-(interval) * (int)((-(v))/(interval))) : \
				 ((interval) * ((int)((v)/(interval) + 1))))
#endif

	

static inline float
FloatTrunc (v, interval)
float v, interval;
{
	float ret;

	if (v < 0)
	{
		ret = interval * (int) ((-v)/interval);
		return (-ret);
	}
	else
	{
		ret = interval * (int) (v/interval);
		return ( (ret >= interval) ? (ret) : (ret + interval) );
	}
}





/*ARGSUSED*/
void
ov_Grid (comp, update)
char *comp;
int update;
/*
 * Draw a grid overlay, either solid or tic'ed, in either lat/lon or km coords.
 * Annotate on multiples of a multiple of the interval, taking care not to
 * crowd them.
 */
{
	float xs, ys, theight, xoff, yoff;
	int top, bottom, left, right, tic;
	bool solid, ll;
	float xskm, uxs;
	float aint;
/*
 * Dig out our info.
 */
	ov_SGSetup (comp, &xs, &ys, &theight, &solid, &tic, &ll, &xoff, &yoff);
/*
 * Set the pixel limits of the grid according to the current user km limits
 */
#ifdef notdef
	bottom = YPIX (Yhi) - 10;
	top = YPIX (Ylo) - 12;
	left = XPIX (Xlo);
	right = XPIX (Xhi);
#endif
#ifdef notdef
	bottom = YPIX (Yhi) - AXIS_SPACE(SideTop)/4;
	top = YPIX (Ylo) + AXIS_SPACE(SideBottom)/4;
	left = XPIX (Xlo) - AXIS_SPACE(SideLeft)/4;
	right = XPIX (Xhi) + AXIS_SPACE(SideRight)/4;
#endif
	bottom = YPIX (Yhi) - 5;
	top = YPIX (Ylo) + 5;
	left = XPIX (Xlo) - 5;
	right = XPIX (Xhi) + 5;

/* 
 * If this is a lat/lon grid, the x/y-spacing parameters are interpreted as
 * minutes.  If this is a km grid, they are in kilometers.  The calculation
 * of aint is different for each type of grid.  We want no more than one
 * label per inch.  If x/y-spacing is less than one per inch, label every
 * x/y-spacing interval.
 */
	if (ll)			/* set xskm to xs in km rather than minutes */

		xskm = DEG_TO_KM(xs/60.0);
	else
		xskm = xs;
/* 
 * xskm is in km, so things not too difficult now.  Find number of km in 
 * 0.8 inches of screen, and use this to space the annotation on a multiple
 * of the x-spacing (in km).
 */
	uxs = 0.8*WidthPixPerInch(XtScreen(Graphics));
	uxs = XUSER( uxs + XPIX(0.0) );

	if (xskm > uxs)	/* x-spacing large enough to not crowd text */
		aint = xskm;
	else		/* take first multiple of xskm greater than uxs */
		aint = FloatTrunc (uxs, xskm);
/*
 * Now convert aint to degrees for lat/lon case
 */
	if (ll)
		aint = KM_TO_DEG(aint);
/*
 * Draw the grid.  Each routine should write annotation when the x or y
 * coordinate, in either km or degrees, is a multiple of aint.
 */
	if (solid)
	{
		if (ll)
			ov_LLSolidGrid (left, right, top, bottom, xs, ys,
					theight, aint);
		else
			ov_SolidGrid (left, right, top, bottom, xs, ys, 
				      theight, aint, xoff, yoff);
	}
	else
	{
		if (ll)
			ov_LLTicGrid (left, right, top, bottom, xs, ys,
				theight, aint, tic);
		else
			ov_TicGrid (left, right, top, bottom, xs, ys, theight,
				aint, tic, xoff, yoff);
	}
}





static void
ov_TicGrid (left, right, top, bottom, xs, ys, theight, aint, ticwidth, 
		xoff, yoff)
int left, right, top, bottom, ticwidth;
float xs, ys, theight, xoff, yoff;
float aint;
/*
 * Draw a tic-style cartesian grid.
 */
{
	float xpos, ypos;	/* the current km coords, including offset */
	int xp, yp;		/* the pixel locations of a tic */
	int nx = 0;
	char label[30];
	Drawable frame = GWFrame (Graphics);
/*
 * Pass along the rows.  Start as far left as possible on a multiple of xs,
 * but greater than (Xlo + xoff)
 */
 	for (xpos = FloatTrunc (Xlo + xoff, xs); xpos <= Xhi+xoff; xpos += xs)
	{
		if ((xp = XPIX (xpos - xoff)) < left)
			continue;
	/*
	 * And down the columns.
	 */
	 	for (ypos = FloatTrunc (Ylo + yoff, ys); ypos <= Yhi + yoff;
				ypos += ys)
		{
			if ((yp = YPIX (ypos - yoff)) > top)
				continue;
		/*
		 * Draw the tic marks.
		 */
			XDrawLine (Disp, frame, Gcontext, xp - ticwidth, yp,
					xp + ticwidth, yp);
			XDrawLine (Disp, frame, Gcontext, xp, yp - ticwidth,
					xp, yp + ticwidth);
		/*
		 * Annotate if this is the first time through and the coord
		 * falls on a multiple of aint
		 */
			if ((nx == 0) && (fabs(fmod(ypos,aint)) < TOLERANCE))
			{
				sprintf (label, "%.1f", ypos);
				DrawText (Graphics, frame, Gcontext, left - 1,
					yp, label, 0.0, theight, JustifyRight,
					JustifyCenter);
			}
		}
	/*
	 * Bottom annotation.
	 */
		if (fabs(fmod(xpos,aint)) < TOLERANCE)
		{
			sprintf (label, "%.1f", xpos);
			DrawText (Graphics, frame, Gcontext, xp, top + 1,
				label, 0.0, theight,JustifyCenter, JustifyTop);
		}
	}
}




static void
ov_SolidGrid (left, right, top, bottom, xs, ys, theight, aint, xoff, yoff)
int left, right, top, bottom;
float xs, ys, theight, xoff, yoff;
float aint;
/*
 * Draw a solid grid.
 */
{
	char label[30];
	float pos;
	Drawable frame = GWFrame (Graphics);
/*
 * Draw the vertical lines.
 */
	for (pos = FloatTrunc (Xlo + xoff, xs); pos <= Xhi + xoff; pos += xs)
	{
		if (XPIX (pos - xoff) < left)
			continue;
		XDrawLine (Disp, frame, Gcontext, XPIX (pos - xoff), top,
				XPIX (pos - xoff), bottom);
		if (fabs(fmod(pos,aint)) < TOLERANCE)
		{
			sprintf (label, "%.1f", pos);
			DrawText (Graphics, frame, Gcontext, XPIX (pos - xoff),
				top + 1, label, 0.0, theight,
				JustifyCenter, JustifyTop);
		}
	}
/*
 * And horizontal.
 */
	for (pos = FloatTrunc (Ylo + yoff, ys); pos <= Yhi + yoff; pos += ys)
	{
		if (YPIX (pos - yoff) > top)
			continue;
		XDrawLine (Disp, frame, Gcontext, left, YPIX (pos - yoff),
				right, YPIX (pos - yoff));
		if (fabs(fmod(pos,aint)) < TOLERANCE)
		{
			sprintf (label, "%.1f", pos);
			DrawText (Graphics, frame, Gcontext, left - 1,
				YPIX (pos - yoff), label, 0.0, theight,
				JustifyRight, JustifyCenter);
		}
	}
}





static void
ov_CGFixLL (minlat, minlon, maxlat, maxlon, blat, blon, xs, ys)
float *minlat, *minlon, *maxlat, *maxlon, *blat, *blon;
double xs, ys;
/*
 * Fix these values up to nice increments.
 */
{
	int iblat, iblon, ixs, iys;
	float olat, olon;
/*
 * Try to project the corners of our window into ll space.
 */
	prj_Reverse (Xlo, Ylo, minlat, minlon);
	prj_Reverse (Xhi, Yhi, maxlat, maxlon);
/*
 * See if we can expand our view a bit, since, in a projection environment,
 * it can be hard to get all the lines on the screen.
 */
	if (prj_FancyProjection ())
	{
		*minlat -= ys/60;
		*maxlat += ys/60;
		*minlon -= xs/60;
		*maxlon += xs/60;
	}
/*
 * It's possible that we're completely out of the globe for some projections.
 * try to deal with that situation.
 */
	prj_GetOrigin (&olat, &olon);
	if (*minlat < -90.0 || *minlat > 90.0)
		*minlat = -90.0;
	if (*maxlat < -90.0 || *maxlat > 90.0)
		*maxlat = 90.0;
	if (*minlon < -180.0 || *minlon > 180.0)
		if ((*minlon = olon - 180.0) < -180.0)
			*minlon += 360.0;
	if (*maxlon < -180.0 || *maxlon > 180.0)
		if ((*maxlon = olon + 180.0) > 180.0)
			*maxlon -= 360.0;
	*blat = *minlat;
	*blon = *minlon;
/*
 * Put everything into easy integer increments.
 */
	iblat = Round (*blat * 3600.0);
	iblon = Round (*blon * 3600.0);
	ixs = Round (xs * 60.0);
	iys = Round (ys * 60.0);
/*
 * Now truncate things accordingly.  Longitude set to 1st multiple of ixs
 * greater than iblon, since labels are on left.
 */
	if (iblat < 0)
		iblat += (-iblat) % iys;
	else
	{
		iblat -= iblat % iys;
		iblat += iys;
	}
	if (iblon < 0)
		iblon += (-iblon) % ixs;
	else
	{
		iblon -= iblon % ixs;
		iblon += ixs;
	}
	*blat = ((float) iblat)/3600.0;
	*blon = ((float) iblon)/3600.0;
}




static void
ov_LLTicGrid (left, right, top, bottom, xs, ys, theight, aint, ticwidth)
int left, right, top, bottom, ticwidth;
float xs, ys, theight;
float aint;
/*
 * Draw a tic-style lat/lon cartesian grid.
 */
{
	float xpos, ypos, blat, blon, maxlat, maxlon, minlat, minlon;
	int xp, yp, nx;
	char label[30];
	int approx;
	Drawable frame = GWFrame (Graphics);
/*
 * Figure out where we are.
 */
	ov_CGFixLL (&minlat, &minlon, &maxlat, &maxlon, &blat, &blon, xs, ys);
	prj_Reverse (Xhi, Yhi, &maxlat, &maxlon);
	approx = DT_ApproxHeight (Graphics, theight, 1);
/*
 * NOTE: maxlon will be more than 90 degrees LESS THAN blon if maxlon is
 * actually across the Intl Date Line.  We must take this into account and
 * make maxlon positive by adding a full circle to it, and then subtracting
 * the circle when it's time to determine the actual longitude label.  
 * Requiring a 90 degree difference avoids situations where maxlon < blon
 * because of precision errors.
 */
	if (maxlon + 90.0 < blon)
		maxlon += 360.0;
	xs /= 60.0;			/* convert minutes to degrees */
	ys /= 60.0;
/*
 * Pass along the rows.
 */
	nx = 0;
	xp = 0;
	for (xpos = blon; xpos <= maxlon; xpos += xs)
	{
	/*
	 * And down the columns.
	 */
		for (ypos = blat; ypos <= maxlat; ypos += ys)
		{
			float xkm, ykm;
			prj_Project (ypos, xpos, &xkm, &ykm);
			xp = XPIX (xkm);
			yp = YPIX (ykm);
			if (yp > top)
				continue;
		/*
		 * Draw the tic marks.
		 */
			XDrawLine (Disp, frame, Gcontext, xp - ticwidth, yp,
					xp + ticwidth, yp);
			XDrawLine (Disp, frame, Gcontext, xp, yp - ticwidth,
					xp, yp + ticwidth);
		/*
		 * Annotate if this is the first time through.
		 */
			if (!nx && (DegreesInterval (ypos,aint)))
			{
				sprintf (label, "%d  ", Round(ypos*60)/60);
				DrawText (Graphics, frame, Gcontext, left - 1,
					  yp, label, 0.0, theight, 
					  JustifyRight, JustifyBottom);
			/*
			 * Do minutes and seconds if the interval is not
			 * in integral degrees.
			 */
				if (fmod (aint, 1.0) != 0.0)
				{
					sprintf (label, "%d' %d\"", 
						 Round(fabs(ypos)*60)%60,
						 Round(fabs(ypos)*3600)%60);
					DrawText (Graphics, frame, Gcontext, 
						  left - 1, yp, label, 0.0, 
						  theight, JustifyRight, 
						  JustifyTop);
				}
			}
		}
	/*
	 * Bottom annotation.
	 */
		if (DegreesInterval (xpos,aint))
		{
			sprintf (label, "%d", 
				 Round(((xpos>180)?(xpos-360):(xpos))*60)/60);
			DrawText (Graphics, frame, Gcontext, xp, top + 1,
				  label,0.0,theight,JustifyCenter,JustifyTop);
		/*
		 * Do minutes and seconds if the interval is not
		 * in integral degrees.
		 */
			if (fmod (aint, 1.0) != 0.0)
			{
				sprintf (label, "%d' %d\"", 
					 Round(fabs(xpos)*60)%60,
					 Round(fabs(xpos)*3600)%60);
				DrawText (Graphics, frame, Gcontext, xp, 
					  top+approx+1, label, 0.0, theight,
					  JustifyCenter, JustifyTop);
			}
		}
		nx++;
	}
}



static void
ov_LLSolidGrid (left, right, top, bottom, xs, ys, theight, aint)
int left, right, top, bottom;
float xs, ys, theight;
float aint;
/*
 * Draw a solid-line lat/lon cartesian grid.
 */
{
	float	ctr_lat, ctr_lon, prevlat, prevlon, lat, lon;
	float	startlat, startlon, latstep, lonstep;
	int	dir, npts, did_draw, did_label, ideg, imin;
	char	string[24], *label;
/*
 * Find the lat/lon of the center of the display area
 */
	prj_Reverse (YUSER ((top + bottom)/2),  XUSER ((left + right)/2),
		     &ctr_lat, &ctr_lon);
/*
 * Change our spacings from minutes into degrees
 */
	xs /= 60.0;
	ys /= 60.0;
/*
 * Make clipping work for us, and tell DrawText to blank out under our labels
 */
	SetClip (FALSE);
	dt_SetBlankLabel (TRUE);
/*
 * Move north from the center, drawing lines of latitude, until we hit one
 * that doesn't intersect the display area, or until we hit 90 degrees N.
 * Then repeat the same, moving south.
 */
	for (dir = 0; dir < 2; dir++)
	{
		if (dir == 0)	/* north */
		{
			startlat = ys * ceil (ctr_lat / ys);
			latstep = ys;
		}			
		else		/* south */
		{
			startlat = ys * floor (ctr_lat / ys);
			latstep = -ys;
		}

		for (lat = startlat; lat >= -90.0 && lat <= 90.0; 
		     lat += latstep)
		{
			ideg = (int) lat;
			imin = (int)(fabs (lat - ideg) * 60.0);
			
			if (imin)
				sprintf (string, " %d %d' ", ideg, imin);
			else
				sprintf (string, " %d ", ideg);
		/*
		 * Run through the range of longitudes, stepping by
		 * half the distance between our longitude lines.
		 */
			did_draw = FALSE;
			did_label = FALSE;

			for (lon = ctr_lon - 180.0; 
			     lon <= ctr_lon + 180.0; 
			     lon += 0.5 * xs)
			{
				label = did_label ? NULL : string;
				npts = ov_DoSegAndLabel (lat, lon, lat, 
							 lon + 0.5 * xs, 
							 left, right,
							 top, bottom, label,
							 theight);
				did_draw |= npts;
				did_label |= (npts == 2);
			}
		/*
		 * Break out if no portion of this latitude line showed up
		 * on the plot
		 */
			if (! did_draw)
			{
				msg_ELog (EF_DEBUG, 
					  "Stopping lat lines @ %.1f", lat);
				break;
			}
		}
	}
/*
 * Now do the same for longitude lines, going east from the center, and
 * then west.
 */
	for (dir = 0; dir < 2; dir++)
	{
		if (dir == 0)	/* east */
		{
			startlon = xs * ceil (ctr_lon / xs);
			lonstep = xs;
		}			
		else		/* west */
		{
			startlon = xs * floor (ctr_lon / xs);
			lonstep = -xs;
		}

		for (lon = startlon; 
		     lon <= startlon + 180.0 && lon >= startlon - 180.0; 
		     lon += lonstep)
		{
			ideg = (int) lon;
			imin = (int)(fabs (lon - ideg) * 60.0);

			if (ideg < -180)
				ideg += 360;
			else if (ideg > 180)
				ideg -= 360;
			
			if (imin)
				sprintf (string, " %d %d' ", ideg, imin);
			else
				sprintf (string, " %d ", ideg);
		/*
		 * Run through the range of latitudes, stepping by
		 * half the distance between our latitude lines.
		 */
			did_draw = FALSE;
			did_label = FALSE;

			for (lat = -90.0; lat < 90.0; lat += 0.5 * ys)
			{
				label = did_label ? NULL : string;
				npts = ov_DoSegAndLabel (lat, lon, 
							 lat + 0.5 * ys,
							 lon, left, right, 
							 top, bottom, label,
							 theight);
				did_draw |= npts;
				did_label |= (npts == 2);
			}
			
		/*
		 * Break out if no portion of this longitude line showed up
		 * on the plot
		 */
			if (! did_draw)
			{
				msg_ELog (EF_DEBUG, 
					  "Stopping lon lines @ %.1f", lon);
				break;
			}
		}
	}
/*
 * And unclip again
 */
	SetClip (TRUE);
}



static int
ov_DoSegAndLabel (lat0, lon0, lat1, lon1, left, right, top, bottom, label,
		  theight)
double	lat0, lon0, lat1, lon1;
int	left, right, top, bottom;
char	*label;
double	theight;
/*
 * Draw whatever portion of the given segment intersects our drawing
 * area.  Return 0 if neither endpoint lies within the drawing area,
 * 1 if one of the points is in the drawing area, and 2 if both points
 * lie within the area.  If the given label is non-null and both points are
 * in the drawing area, the label will be drawn.
 */
{
	int	px0, px1, py0, py1, ninside;
	float	x0, y0, x1, y1;
/*
 * Project the lat/lon pairs.  If either one fails to yield a good point, 
 * drop this segment.
 */
	if (! prj_Project (lat0, lon0, &x0, &y0) ||
	    ! prj_Project (lat1, lon1, &x1, &y1))
		return (0);
/*
 * Get coordinates in pixel space and draw the segment
 */
	px0 = IXPIX (x0);
	px1 = IXPIX (x1);
	py0 = IYPIX (y0);
	py1 = IYPIX (y1);

	XDrawLine (Disp, GWFrame (Graphics), Gcontext, px0, py0, px1, py1);
/*
 * Are there 0, 1, or 2 points inside the plot region?
 */
	ninside = (BETWEEN (px0, left, right) && BETWEEN (py0, bottom, top)) + 
		(BETWEEN (px1, left, right) && BETWEEN (py1, bottom, top));
/*
 * If both points are inside the plot region and we were given a label, print 
 * it.
 */
	if (ninside == 2 && label)
	{
		int j;
		double textrot;
	/*
	 * Find the text rotation and twiddle it if necessary.  If we
	 * twiddle it, we also have to switch whether we justify to the left
	 * or right.
	 */
		textrot = RAD_TO_DEG (atan2 ((double)(py0 - py1), 
					     (double)(px1 - px0)));

		if (textrot > 90.0 || textrot < -90.0)
		{
			textrot += 180.0;
			j = JustifyLeft;
		}
		else
			j = JustifyRight;
	/*
	 * Unclip, draw the text, and turn clipping on again.
	 */
		SetClip (TRUE);
		DT_StrokeText (Graphics, GWFrame (Graphics), Gcontext, 
			       px1, py1, label, textrot, theight, j,
			       JustifyCenter);
		SetClip (FALSE);
	}

	return (ninside);
}



# ifdef notdef /* Old ov_LLSolidGrid() */


static void
ov_LLCurve (lat1, lon1, lat2, lon2, nseg, ypmax, fx, fy)
float lat1, lat2, lon1, lon2;
int nseg, ypmax, *fx, *fy;
/*
 * Draw a curved lat/lon line.
 */
{
# ifdef MAP_PROJECTIONS
	float latinc, loninc;
	XPoint points[MAXSEG];
	float xk, yk;
	int seg, npt = 0, first = TRUE;
/*
 * Figure segments and increments.
 */
	if (nseg > MAXSEG)
		nseg = MAXSEG;
	latinc = (lat2 - lat1)/(nseg - 1);
	loninc = (lon2 - lon1)/(nseg - 1); /* works cuz of maxlon correction */
/*
 * Calculate some end points.
 */
# ifdef notdef
	prj_Project (lat1, lon1, &xk, &yk);
	points[0].x = XPIX (xk);
	points[0].y = YPIX (yk);
# endif
	for (seg = 0; seg < nseg; seg++)
	{
		int prjok = prj_Project (lat1, lon1, &xk, &yk);
	/*
	 * Clip things on the bottom, even though regular clipping will be
	 * in effect.  One reason to do this is to remember where we drew
	 * the first real point.
	 */
		if (! prjok || YPIX (yk) > ypmax)
		{
			if (npt > 1)
				XDrawLines (Disp, GWFrame (Graphics), Gcontext,
						points, npt, CoordModeOrigin);
			npt = 0;
		}
	/*
	 * OK we are drawing this one.  Remember the first point if
	 * applicable.
	 */
		else 
		{
			if (first)
			{
				*fx = XPIX (xk);
				*fy = YPIX (yk);
				first = FALSE;
			}
			points[npt].x = XPIX (xk);
			points[npt++].y = YPIX (yk);
		}
	/*
	 * Move on.
	 */
		lat1 += latinc;
		if ((lon1 += loninc) > 180.0)
			lon1 -= 360.0;
	}
/*
 * Draw and we're done.
 */
	SetClip (FALSE);
	XDrawLines (Disp, GWFrame (Graphics), Gcontext, points, npt,
			CoordModeOrigin);
	SetClip (TRUE);
# endif /* MAP_PROJECTIONS */
}


static void
ov_LLSolidGrid (left, right, top, bottom, xs, ys, theight, aint)
int left, right, top, bottom;
float xs, ys, theight;
float aint;
/*
 * Draw a solid-line lat/lon cartesian grid.
 */
{
	float xpos, ypos, blat, blon;
	float maxlat, maxlon;
	float minlat, minlon;
	int xp, yp, fx, fy, min_yannot = (1.0 - F_Y1)*GWHeight (Graphics);
	int max_xannot = F_X1*GWWidth (Graphics);
	int xe, ye, proj = prj_FancyProjection ();
	char label[30];
	int approx;
	Drawable frame = GWFrame (Graphics);
/*
 * Make nice increments.
 */
	ov_CGFixLL (&minlat, &minlon, &maxlat, &maxlon, &blat, &blon, xs, ys);

	if (maxlon + 90.0 < blon)		/* see note in LLTicGrid */
		maxlon += 360.0;
	xs /= 60.0;
	ys /= 60.0;
	approx = DT_ApproxHeight (Graphics, theight, 1);
/*
 * Draw all of the horizontal lines and the left annotation
 */
	SetClip (FALSE);
	xpos = minlon;
	for (ypos = blat; ypos <= maxlat; ypos += ys)
	{
		float xkm, ykm;
	/*
	 * Figure out where this line goes.
	 */
		prj_Project (ypos, xpos, &xkm, &ykm);
		xp = XPIX (xkm);
		yp = YPIX (ykm);
		if (yp > top - 1)
			continue;
		prj_Project (ypos, maxlon, &xkm, &ykm);
		xe = XPIX (xkm);
		ye = YPIX (ykm);
	/*
	 * Draw the line(s).
	 */
		if (proj)
			ov_LLCurve (ypos, xpos, ypos, maxlon, 80, top,
					&fx, &yp);
		else
			XDrawLine (Disp, frame, Gcontext, xp, yp, xe, ye);
	/*
	 * Finally, annotate if appropriate.
	 */
		if (DegreesInterval (ypos, aint) && yp > min_yannot)
		{
			SetClip (TRUE);
			sprintf (label, "%d  ", Round(ypos*60)/60);
			DrawText (Graphics, frame, Gcontext, left - 1,
				  yp, label, 0.0, theight/1.2,
				  JustifyRight, JustifyBottom);
		/*
		 * Do minutes and seconds if the interval is not
		 * in integral degrees.
		 */
			if (fmod (aint, 1.0) != 0.0)
			{
				sprintf (label, "%d' %d\"", 
					 Round(fabs(ypos)*60)%60,
					 Round(fabs(ypos)*3600)%60);
				DrawText (Graphics, frame, Gcontext, left - 1,
					  yp, label, 0.0, theight, 
					  JustifyRight, JustifyTop);
			}
			SetClip (FALSE);
		}
	}

	/* ypos = minlat;*/
	for (xpos = blon; xpos <= maxlon; xpos += xs)
	{
	/*
	 * Vertical lines
	 */
		float xkm, ykm, flon = (xpos > 180) ? xpos - 360.0 : xpos;
		
		prj_Project (minlat, flon, &xkm, &ykm);
		xp = XPIX (xkm);
		yp = YPIX (ykm);
		prj_Project (maxlat, flon, &xkm, &ykm);
		xe = XPIX (xkm);
		ye = YPIX (ykm);
	/*
	 * Draw the lines first.  In the projection case, that will
	 * tell us where the annotation should really go.
	 */
		if (proj)
			ov_LLCurve (minlat, flon, maxlat, flon, 80, top,
					&xp, &fy);
		else
			XDrawLine (Disp, frame, Gcontext, xp, yp, xe, ye);
	/*
	 * Bottom annotation.  Adjust end point to allow text to fit.
	 */
		if (DegreesInterval (xpos,aint) && xp < max_xannot && xp >= 0)
		{
			SetClip (TRUE);
			sprintf (label, "%d", 
				 Round(((xpos>180)?(xpos-360):(xpos))*60)/60);
			DrawText (Graphics, frame, Gcontext, xp, top + 1,
				  label, 0.0, theight,JustifyCenter, 
				  JustifyTop);
		/*
		 * Do minutes and seconds if the interval is not
		 * in integral degrees.
		 */
			if (fmod (aint, 1.0) != 0.0)
			{
				sprintf (label, "%d' %d\"", 
					 Round(fabs(xpos)*60)%60,
					 Round(fabs(xpos)*3600)%60);
				DrawText (Graphics, frame, Gcontext, xp, 
					  top + 1 + approx, label, 0.0, 
					  theight, JustifyCenter, JustifyTop);
			}
			yp = top - 1;
			SetClip (FALSE);
		}
	}
}
# endif /* Old ov_LLSolidGrid() */




static void
ov_SGSetup (comp, xs, ys, theight, solid, ticwidth, ll, xoff, yoff)
char *comp;
float *xs, *ys;
float *theight, *xoff, *yoff;
int *ticwidth;
bool *solid, *ll;
/*
 * Get everything set up to draw a grid.
 */
{
	int lwidth;
	Location loc;
	char origin[80];
/*
 * Find our spacings.
 */
	if (! pda_Search (Pd, comp, "x-spacing", "grid", (char *) xs,
			SYMT_FLOAT))
		*xs = 10.0;
	if (! pda_Search (Pd, comp, "y-spacing", "grid", (char *) ys,
			SYMT_FLOAT))
		*ys = 10.0;
/*
 * Go ahead and set the line drawing parameters.
 */
	SetColor (comp, "color", "grid", "gray60");
	if (! pda_Search (Pd, comp, "line-width", "grid", (char *) &lwidth,
			SYMT_INT))
		lwidth = 0;
	XSetLineAttributes (Disp, Gcontext, lwidth, LineSolid, CapButt,
			JoinMiter);
/*
 * Text height.
 */
	if (! pda_Search (Pd, comp, "annot-height", "grid", (char *) theight,
			SYMT_FLOAT))
		*theight = 0.02;
/*
 * Solidness.
 */
	if (! pda_Search(Pd, comp, "solid", "grid", (char *) solid, SYMT_BOOL))
		*solid = FALSE;
	if (! pda_Search (Pd, comp, "tic-width", "grid", (char *) ticwidth,
			SYMT_INT))
		*ticwidth = 8;
/*
 * Do we do this in lat/lon?
 */
	*ll = 0;
	if (! pda_Search (Pd, comp, "lat-lon", "grid", (char *) ll, SYMT_BOOL))
		*ll = FALSE;
/*
 * See if we should displace the origin.
 */
	*xoff = *yoff = 0.0;
	if (pda_Search (Pd, comp, "origin", "grid", origin, SYMT_STRING))
	{
		if (GetLocation (origin, &PlotTime, &loc))
		{
			prj_Project (loc.l_lat, loc.l_lon, xoff, yoff);
			*xoff = - *xoff;
			*yoff = - *yoff;
		}
		else
			msg_ELog (EF_PROBLEM, "Bad origin '%s'", origin);
	}
}
