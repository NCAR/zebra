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
# include <X11/StringDefs.h>
# include <math.h>
# include <defs.h>
# include <message.h>
# include <pd.h>
# include <DataStore.h>
# include <time.h>
# include "GraphProc.h"
# include "PixelCoord.h"
MAKE_RCSID ("$Id: Utilities.c,v 2.24 1994-06-07 20:09:27 corbet Exp $")

/*
 * Rules for image dumping.  Indexed by keyword number in GraphProc.state
 */
static char *ImgRules[] =
{
	"cat",					/* xwd */
	"xwdtopnm -quiet | ppmtogif -quiet",	/* gif */
	"xwd2ps",				/* pscolor */
	"xwd2ps -I -m",				/* psmono */
	"xwd2ps -I",				/* psrev */
};


# define DEG_TO_RAD(deg) ((deg)*0.017453292) /* Deg to Rad conv. */


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
	PlatformId pid;
	ZebTime dtime;
	DataChunk *dc;
/*
 * Try first to find a static location in the plot description.
 */
	if (pda_Search (Pd, "global", "location", platform, sloc, SYMT_STRING))
	{
		if (sscanf (sloc, "%f %f %f", &loc->l_lat, &loc->l_lon,
				&loc->l_alt) != 3)
		{
			msg_ELog (EF_PROBLEM, "Bad location string: '%s'",
					sloc);
			return (FALSE);
		}
		return (TRUE);
	}
/*
 * Well, let's see what the data store can do for us.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform %s for loc", platform);
		return (FALSE);
	}
	if (! ds_DataTimes (pid, t, 1, DsBefore, &dtime))
	{
		msg_ELog (EF_INFO, "No position info for %s", platform);
		return (FALSE);
	}
/*
 * Fetch the location and we are done.
 */
	if (! (dc = ds_Fetch (pid, DCC_Location, &dtime, &dtime, 0, 0, 0, 0)))
	{
		msg_ELog (EF_PROBLEM, "Unable to fetch %s location", platform);
		return (FALSE);
	}
	dc_GetLoc (dc, 0, loc);
	dc_DestroyDC (dc);
	return (TRUE);
}






int
FancyGetLocation (c, platform, when, actual, loc)
char *c, *platform;
ZebTime *when, *actual;
Location *loc;
/*
 * Find out where this platform is at this time.
 * This is the fancier version, here so that I can avoid the extra hassle
 * of changing the interface to GetLocation.
 * Entry:
 *	C	is the component of interest.
 *	PLATFORM is the platform for which the location is to be found.
 * 	WHEN	is the time of interest
 * Exit:
 *	If a location is found, then:
 *		ACTUAL	is the real time associated with it
 *		LOC	is the location
 *		the return value is true.
 *	else
 *		return value is false.
 *
 */
{

	char sloc[80];
	PlatformId pid;
	DataChunk *dc;
/*
 * Try first to find a static location in the plot description.
 */
	if (pda_Search (Pd, "global", "location", platform, sloc, SYMT_STRING))
	{
		if (sscanf (sloc, "%f %f %f", &loc->l_lat, &loc->l_lon,
				&loc->l_alt) != 3)
		{
			msg_ELog (EF_PROBLEM, "Bad location string: '%s'",
					sloc);
			return (FALSE);
		}
		*actual = *when;
		return (TRUE);
	}
/*
 * Well, let's see what the data store can do for us.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform %s for loc", platform);
		return (FALSE);
	}
	if (! ds_DataTimes (pid, when, 1, DsBefore, actual))
	{
		msg_ELog (EF_DEBUG, "No position info for %s", platform);
		return (FALSE);
	}
/*
 * Perform an age check.
 */
	if (! AgeCheck (c, platform, actual))
		return (FALSE);
/*
 * Fetch the location and we are done.
 */
	if (! (dc = ds_Fetch (pid, DCC_Location, actual, actual, 0, 0, 0, 0)))
	{
		msg_ELog (EF_PROBLEM, "Unable to fetch %s location", platform);
		return (FALSE);
	}
	dc_GetLoc (dc, 0, loc);
	dc_DestroyDC (dc);
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
	vals.subwindow_mode = IncludeInferiors;
	XChangeGC (Disp, Gcontext, GCFunction | GCLineWidth | GCLineStyle |
		GCCapStyle | GCJoinStyle | GCFillStyle | GCSubwindowMode,
		&vals);
/*
 * Don't forget to unclip...
 */
	XSetClipMask (Disp, Gcontext, None);
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




void
CalcCenterStep (bottom, top, nstep, center, step)
float top, bottom, *center, *step;
int nstep;
/*
 * Find appropriate center and step values for the display of data that
 * ranges between TOP and BOTTOM, where NSTEP steps are desired.
 */
{
	double range = fabs (top - bottom), nominal, ordermag = 1.0;
/*
 * If we have no range, just set things up so we end up with a step of 1
 */
	if (range == 0.0)
		range = nstep;
/*
 * Find the nominal step, then go through some gymnastics to turn it into
 * a "nice" value.  This could be done more elegantly with logarithms, but,
 * given the ranges of data we see, this should really be faster.
 */
	nominal = range/nstep;
	while (nominal > 10.0)
	{
		nominal /= 10.0;
		ordermag *= 10.0;
	}
	while (nominal < 1.0)
	{
		nominal *= 10.0;
		ordermag /= 10.0;
	}
	*step = ((float) ((int) (nominal + 0.9)))*ordermag;
/*
 * Now find a nice center that's a multiple of step
 */
	*center = 0.5 * (top + bottom);
	*center = nint (*center/(*step)) * (*step);
}





void
FindCenterStep (dc, field, nstep, center, step)
DataChunk *dc;
FieldId field;
int nstep;
float *center, *step;
/*
 * Find appropriate center and step values for this DC.
 */
{
	int samp, nsamp = dc_GetNSample (dc), freedata = FALSE, np;
	float max = -99999.9, min = 99999.9, badval = dc_GetBadval (dc);
	float *data;
	RGrid rg;
/*
 * Pull out the data.
 * We are organization-dependent here...
 */
	switch (dc->dc_Class)
	{
	   case DCC_Scalar:
		data = (float *) malloc (nsamp * sizeof (float));
		np = nsamp;
		freedata = TRUE;
		for (samp = 0; samp < nsamp; samp++)
			data[samp] = dc_GetScalar (dc, samp, field);
		break;

	   case DCC_IRGrid:
		data = dc_IRGetGrid (dc, 0 /* kludge */, field);
		np = dc_IRGetNPlatform (dc);
		break;

	   case DCC_RGrid:
		data = dc_RGGetGrid (dc, 0, field, 0, &rg, 0);
		np = rg.rg_nX * rg.rg_nY * rg.rg_nZ;
		break;

	   default:
		msg_ELog (EF_PROBLEM, "FindCenterStep on unsupported org");
		*center = 0;
		*step = 1;
		return;
	}
/*
 * Get the range of the data, then calculate the values.
 */
	GetRange (data, np, badval, &min, &max);
	CalcCenterStep (min, max, nstep, center, step);
	if (freedata)
		free (data);
}




void
GetRange (data, np, badval, min, max)
float *data, badval, *min, *max;
int np;
/*
 * Find the data range covered by this data.
 */
{
	*min = 99999.9;
	*max = -99999.9;
/*
 * Just pass through and figure it out.  Someday we may want to do something
 * fancier, like skipping outliers or some such.
 */
	for (; np > 0; np--)
	{
		float dv = *data++;
		if (dv == badval)
			continue;
		if (! FINITE(dv))
			continue;
		if (dv < *min)
			*min = dv;
		if (dv > *max)
			*max = dv;
	}
}
	



# if defined(hpux) || defined(SVR4)
int
nint (x)
double x;
/*
 * nearest int to x
 */
{
	if (x > 0)
		return ((int) (x + 0.5));
	else
		return ((int) (x - 0.5));
}

# endif




void
ImageDump (format, file)
int format;
char *file;
/*
 * Dump out an image of our screen.
 */
{
	char cmd[200], efile[120];
	date uid;
/*
 * Work up the file name.
 */
	TC_ZtToUI (&PlotTime, &uid);
	sprintf (efile, file, uid.ds_yymmdd, uid.ds_hhmmss);
/*
 * Do it.
 */
	sprintf (cmd, "xwd -id 0x%x | %s > %s", XtWindow (Graphics),
			ImgRules[format], efile);
	system (cmd);
}






void
FindWindsFields (plat, zt, ufield, vfield, fids)
PlatformId plat;
ZebTime *zt;
char *ufield, *vfield;
FieldId *fids;
/*
 * Figure out what fields to snarf for winds.
 */
{
	FieldId afids[128];
	int nfid = 128, i;
/*
 * Look up the u component and see if we can really get it.
 */
	fids[0] = F_Lookup (ufield);
	ds_GetFields (plat, zt, &nfid, afids);
	for (i = 0; i < nfid; i++)
		if (fids[0] == afids[i])
		{
			msg_ELog (EF_INFO, "Found U");
			fids[1] = F_Lookup (vfield);
			return;
		}
/*
 * Nope.  Instead, we need to ask for speed and direction and hope it
 * is available.
 */
	msg_ELog (EF_INFO, "Going for speed/direction");
	fids[0] = F_Lookup ("wspd");	/* XXX should parameterize */
	fids[1] = F_Lookup ("wdir");
}





void
GetWindData (fids, u, v, badval)
FieldId *fids;
float *u, *v, badval;
/*
 * Return the actual wind data.
 */
{
	int wsfid = F_Lookup ("wspd");
	float wspd, wdir;
/*
 * If we already have u/v, just return it.
 */
	if (fids[0] != wsfid)
		return;
/*
 * Nope gotta calculate it.
 */
	if (*u == badval || *v == badval)
	{
		*u = *v = badval;	/* get both */
		return;
	}
	wspd = *u;
	wdir = *v;
	*u = -wspd * sin (DEG_TO_RAD (wdir));
	*v = -wspd * cos (DEG_TO_RAD (wdir));
}




void
ChangeCursor (w, cursor)
Widget	w;
Cursor	cursor;
/*
 * Change to the selected cursor in the widget and its children.
 */
{
	static int	rlevel = 0;
	int	n, i, nchildren;
	Arg	args[4];
	WidgetList	children;

	if (w && XtWindow (w))
		XDefineCursor (XtDisplay (w), XtWindow (w), cursor);
/*
 * Recurse through children, if any
 */
	nchildren = 0;

	n = 0;
	XtSetArg (args[n], XtNnumChildren, &nchildren); n++;
	XtSetArg (args[n], XtNchildren, &children); n++;
	XtGetValues (w, args, n);

	rlevel++;	/* recursion level */
	for (i = 0; i < nchildren; i++)
		ChangeCursor (children[i], cursor);
	rlevel--;
/*
 * Sync if we're the top level of recursion
 */
	if (rlevel == 0)
		eq_sync ();
}

