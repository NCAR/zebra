/*
 * Miscellaneous utility functions.
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

# include <X11/Intrinsic.h>
# include <math.h>
# include <defs.h>
# include <message.h>
# include <pd.h>
# include <DataStore.h>
# include <time.h>
# include "GraphProc.h"
# include "PixelCoord.h"
MAKE_RCSID ("$Id: Utilities.c,v 2.7 1992-11-03 20:58:23 burghart Exp $")


static void ApplyConstOffset FP ((Location *, double, double));
static void ApplyAdvection FP ((Location *, double, double, ZebTime *,
		ZebTime *));

# define CPTR(x)     (char *)(&(x))



int
GetLocation (platform, t, loc)
char *platform;
ZebTime *t;
Location *loc;
/*
 * Find out where this platform is at this time.
 */
{

	char sloc[80];
/*
 * The data store routine for finding a location isn't there yet.  We'll
 * instead use the fallback -- which will be here anyway -- of finding the
 * location in the defaults table.
 */
	if (! pda_ReqSearch (Pd, "global", "location", platform, sloc,
			SYMT_STRING))
		return (FALSE);
	if (sscanf (sloc, "%f %f %f", &loc->l_lat, &loc->l_lon, &loc->l_alt)
				!= 3)
	{
		msg_ELog (EF_PROBLEM, "Bad location string: '%s'", sloc);
		return (FALSE);
	}
/*
 * Range checking?
 */
	return (TRUE);
}




void
SetClip (full)
int full;
/*
 * Set the clip region in Gcontext.  If FULL is set, it is clipped to the
 * full window; otherwise just the graphics region is allowed.
 */
{
	XRectangle clip;
/*
 * If they said "full", simply turn off clipping altogether.
 */
	if (full)
		XSetClipMask (Disp, Gcontext, None);
/*
 * Otherwise limit it to the area outside of annotation.
 */
	else
	{
		clip.x = F_X0 * GWWidth (Graphics);
		clip.y = (1.0 - F_Y1) * GWHeight (Graphics);
		clip.width = (F_X1 - F_X0) * GWWidth (Graphics);
		clip.height = (F_Y1 - F_Y0) * GWHeight (Graphics);
		XSetClipRectangles (Disp, Gcontext, 0, 0, &clip, 1, Unsorted);
	}
}









void
ResetGC ()
/*
 * Restore the graphics context to a "clean" state.
 */
{
	XGCValues vals;

	vals.function = GXcopy;
	vals.line_width = 0;
	vals.line_style = LineSolid;
	vals.cap_style = CapButt;
	vals.join_style = JoinMiter;
	vals.fill_style = FillSolid;
	XChangeGC (Disp, Gcontext, GCFunction | GCLineWidth | GCLineStyle |
		GCCapStyle | GCJoinStyle | GCFillStyle, &vals);
}






void
SetColor (comp, param, qual, def)
char *comp, *param, *qual, *def;
/*
 * Set the color in Gcontext;
 */
{
	char color[40];
	XColor xc;

	if (! pda_Search (Pd, comp, param, qual, color, SYMT_STRING))
		strcpy (color, def);
	if (! ct_GetColorByName (color, &xc))
	{
		msg_ELog (EF_PROBLEM, "Unknown color: %s", color);
		ct_GetColorByName ("white", &xc);
	}
	XSetForeground (Disp, Gcontext, xc.pixel);
}




void
FixForeground (pix)
long pix;
/*
 * Set the GC forground to this pixel value.
 */
{
	XSetForeground (Disp, Gcontext, pix);
}




int
SetLWidth (comp, param, qual, def)
char *comp, *param, *qual;
int def;
/*
 * Set the line width in Gcontext;
 */
{
	int lwidth;
	XColor xc;

	if (! pda_Search (Pd, comp, param, qual, (char *) &lwidth, SYMT_INT))
		lwidth = def;
	FixLWidth (lwidth);
	return (lwidth);
}




void
FixLWidth (width)
int width;
/*
 * Set the line width to this value.
 */
{
	XSetLineAttributes (Disp, Gcontext, width, LineSolid, CapButt,
		JoinMiter);
}




int
AgeCheck (comp, platform, t)
char	*comp, *platform;
ZebTime	*t;
/*
 * If this component has an age limit, enforce it.  Return FALSE if the
 * given time is too old, relative to the plot time.
 */
{
	char	limit[100];
	int	seconds, psec, dsec;
/*
 * Look for the limit.  If none exists, return TRUE.
 */
	if (! pda_Search (Pd, comp, "age-limit", platform, limit, SYMT_STRING))
		return (TRUE);
/*
 * Turn this thing into a useful number.
 */
	if (! (seconds = pc_TimeTrigger (limit)))
	{
		msg_ELog (EF_PROBLEM, "Funky age limit: '%s'", limit);
		return (TRUE);
	}
/*
 * Now see how close we are.
 */
	return ((PlotTime.zt_Sec - t->zt_Sec) <= seconds);
}

long
GetSec (t)
time    t;
/*
 * Get the seconds in a number represented by hhmmss.
 * (Note: type long == type time_t, see <sys/stdtypes.h>)
 */
{
        struct tm       syst;

        syst.tm_year = t.ds_yymmdd/10000;
        syst.tm_mon = (t.ds_yymmdd/100) % 100 - 1;
        syst.tm_mday = t.ds_yymmdd % 100;
        syst.tm_hour = t.ds_hhmmss/10000;
        syst.tm_min = t.ds_hhmmss/100 % 100;
        syst.tm_sec = t.ds_hhmmss % 100;
        syst.tm_zone = (char *) 0;
        syst.tm_wday = syst.tm_isdst = syst.tm_yday = 0;
        return (timegm (&syst));
}







int
ApplySpatialOffset (dc, comp, ptime)
DataChunk *dc;
char *comp;
ZebTime *ptime;
/*
 * Apply a spatial offset, if any, to this data chunk.  Return value is
 * TRUE iff locations were actually changed.
 *
 * PROBLEM: this routine does not currently deal with the complications
 * 	    involved with irregular grid data chunks, which have a set
 *	    of per-platform locations, stored separately, with no time
 *	    dimension.
 */
{
	int advect, constant, sample, ns;
	bool enable = FALSE;
	float xoffset = 0, yoffset = 0, xpos, ypos, advdir, advspeed;
	char *pname = ds_PlatformName (dc->dc_Platform);
	Location loc;
	ZebTime t;
/*
 * If none of this is allowed, bail now.
 */
	if (! pda_Search (Pd, "global", "enable-spatial-shift", NULL,
			CPTR (enable), SYMT_BOOL) || ! enable)
		return (FALSE);
/*
 * Look for constant offsets.
 */
	pda_Search (Pd, comp, "x-shift", pname, CPTR (xoffset), SYMT_FLOAT);
	pda_Search (Pd, comp, "y-shift", pname, CPTR (yoffset), SYMT_FLOAT);
	constant = (xoffset != 0.0) || (yoffset != 0.0);
/*
 * And advection parameters.
 */
	advect = pda_Search (Pd, comp, "enable-advection", pname, CPTR(enable),
			SYMT_BOOL) && enable &&
		 pda_Search (Pd, comp, "advection-speed", NULL, 
		 	CPTR (advspeed), SYMT_FLOAT) &&
		 pda_Search (Pd, comp, "advection-direction", NULL,
		 	CPTR (advdir), SYMT_FLOAT);
	if (advspeed == 0.0)
		advect = FALSE;
/*
 * If there is no work to do here, quit.
 */
	if (! constant && ! advect)
		return (FALSE);
	msg_ELog (EF_INFO, "Offsetting comp %s", comp);
/*
 * Make a special case for immobile platforms where we can retain the
 * static location and not incur some extra overhead.
 */
	ns = dc_GetNSample (dc);
# ifdef notdef	/* Causes problems with rgrids */
	if (! ds_IsMobile (dc->dc_Platform) && ns == 1)
	{
		dc_GetLoc (dc, 0, &loc);
		if (constant)
			ApplyConstOffset (&loc, xoffset, yoffset);
		if (advect)
		{
			dc_GetTime (dc, 0, &t);
			ApplyAdvection (&loc, advdir, advspeed, ptime, &t);
		}
		dc_SetStaticLoc (dc, &loc);
		return (TRUE);
	}
# endif
/*
 * Otherwise we need to go through each sample and shift it.
 */
	for (sample = 0; sample < ns; sample++)
	{
		dc_GetLoc (dc, sample, &loc);
		if (constant)
			ApplyConstOffset (&loc, xoffset, yoffset);
		if (advect)
		{
			dc_GetTime (dc, sample, &t);
			ApplyAdvection (&loc, advdir, advspeed, ptime, &t);
		}
		dc_SetLoc (dc, sample, &loc);
	}
	return (TRUE);
}




static void
ApplyConstOffset (loc, x, y)
Location *loc;
double x, y;
/*
 * Apply a constant offset to this location.
 */
{
	float xpos, ypos;

	cvt_ToXY (loc->l_lat, loc->l_lon, &xpos, &ypos);
	cvt_ToLatLon (xpos + x, ypos + y, &loc->l_lat, &loc->l_lon);
}




static void
ApplyAdvection (loc, dir, speed, plottime, datatime)
Location *loc;
double dir, speed;
ZebTime *plottime, *datatime;
/*
 * Apply an advective offset.
 */
{
	double distance, xoff, yoff, rdir = dir*M_PI/180.0;
	int tdiff;
/*
 * Figure the time difference, and, from that, the distance covered.
 */
	tdiff = plottime->zt_Sec - datatime->zt_Sec;
	distance = (speed*tdiff)/1000.0;	/* In km */
	xoff = distance*sin (rdir);
	yoff = distance*cos (rdir);
/*
 * Now apply as in a constant offset.
 */
	ApplyConstOffset (loc, xoff, yoff);
}
