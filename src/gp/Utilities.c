/*
 * Miscellaneous utility functions.
 */
/*		Copyright (C) 1987-1998 by UCAR
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
# include <X11/XWDFile.h>	/* for ImageDump() */
# include <unistd.h>            /* for unlink() */
# include <math.h>
# include <string.h>
# include <time.h>

# include <defs.h>
# include <message.h>
# include <pd.h>
# include <GraphicsW.h>
# include <DataStore.h>
# include <byteorder.h>
# include "GraphProc.h"
# include "PixelCoord.h"

/*
 * A bug in XWDFile.h in OpenWindows omits the typedef for the XWDColor
 * struct, so we have to define our own equivalent type in order to
 * declare XWDColor structures.
 */
typedef struct {
        CARD32  pixel B32;
        CARD16  red B16;
        CARD16  green B16;
        CARD16  blue B16;
        CARD8   flags;
        CARD8   pad;
} U_XWDColor;

RCSID ("$Id: Utilities.c,v 2.62 2006-02-27 22:01:08 granger Exp $")

/*
 * Rules for image dumping.  Indexed by keyword number in GraphProc.state
 */
static char *ImgRules[] =
{
	"cat",			/* xwd */
	"convert xwd:- gif:-",	/* gif */
	"xwd2ps",		/* pscolor */
	"xwd2ps -I -m",		/* psmono */
	"xwd2ps -I",		/* psrev */
};


# define DEG_TO_RAD(deg) ((deg)*0.017453292) /* Deg to Rad conv. */

# ifndef M_PI
# define M_PI 3.14159265358979323846
# endif

# define lowbit(x) ((x) & (~(x) + 1))

static Pixel MakePixel (int entry, Visual *vis);
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
const char *c, *platform;
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

	char sloc[80], ltime[80];
	PlatformId pid;
	DataChunk *dc;
/*
 * Try first to find a static location in the plot description.  If we succeed,
 * it overrides trying to get a location from the data.
 */
	if (pda_Search (Pd, "global", "location", platform, sloc, SYMT_STRING))
	{
		if (sscanf (sloc, "%f %f %f", &loc->l_lat, &loc->l_lon,
				&loc->l_alt) != 3)
		{
			msg_ELog (EF_PROBLEM, "Bad %s-location string: '%s'",
				  platform, sloc);
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
	    msg_ELog (EF_DEBUG, "Unknown platform %s", platform);
	    return (FALSE);
	}
/*
 * Some platforms want the location closest to the display time (i.e. aircraft,
 * boats); others need the observation beginning (soundings).
 */
	if (pda_Search (Pd, c, "location-time", platform, ltime, SYMT_STRING)
			&& ! strcmp (ltime, "observation"))
	{
		if (! ds_GetObsTimes (pid, when, actual, 1, 0))
		{
			msg_ELog (EF_DEBUG, "No position for %s", platform);
			return (FALSE);
		}
	}
	else if (! ds_DataTimes (pid, when, 1, DsBefore, actual))
	{
		msg_ELog (EF_DEBUG, "No position info for %s", platform);
		return (FALSE);
	}
/*
 * Perform an age check.  Then if we're already happy with regard to the
 * location, just quit.
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



int
Intersects (x0, y0, x1, y1)
double x0, y0, x1, y1;
/*
 * Return non-zero if the line (x0, y0) <-> (x1, y1) intersects the plot's
 * grid in the kilometer domain.
 */
{
	float m;
	int set;
	int intersects;
#	define XOR(a,b) ((!(a) && (b)) || ((a) && !(b)))
#	define INSIDE(x,y) (Xlo<=(x)&&(x)<=Xhi&&Ylo<=(y)&&(y)<=Yhi)

	/*
	 * If neither point is in the region, then don't count the line between
	 * them as intersecting.
	 */
	if (!INSIDE(x0,y0) && !INSIDE(x1,y1))
		return (0);
	if (fabs (x1 - x0) < 0.0001)
	{
		return ((Xlo <= x0) && (x0 <= Xhi));
	}
	m = (y1 - y0) / (x1 - x0);
	/*
	 * The line intersects the region if at least one of the corners
	 * is in the set { (x,y): y < m * (x - x0) + y0 } 
	 * while one other is not.
	 */
	set = (Ylo <= (m * (Xlo - x0) + y0));
	intersects = XOR((Ylo <= (m * (Xhi - x0) + y0)), set);
	intersects |= XOR((Yhi <= (m * (Xlo - x0) + y0)), set);
	intersects |= XOR((Yhi <= (m * (Xhi - x0) + y0)), set);
	return (intersects);
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
AgeCheck (comp, plat, t)
const char	*comp, *plat;
ZebTime	*t;
/*
 * If this component has an age limit, enforce it.  Return FALSE if age
 * limits are enabled and the given time is too old relative to the plot 
 * time.
 */
{
	zbool	limit_data_age;
	int	maxdiff, diff;
	char	s[128];
/*
 * Data age check enabled?
 */
	limit_data_age = FALSE;	/* disabled by default */
	pda_Search (Pd, comp, "limit-data-age", plat, (char *) &limit_data_age,
		    SYMT_BOOL);
	if (! limit_data_age)
		return (TRUE);
/*
 * Get the age limit
 */	
	if (pda_Search (Pd, comp, "data-age-limit", plat, s, SYMT_STRING))
	    maxdiff = abs(pc_TimeTrigger (s));
	else
	    return (TRUE);
/*
 * Finally, test to see if the data are recent enough.  Treat a data
 * age limit of zero as inifinite (i.e., display data of any age).
 */	
	diff = PlotTime.zt_Sec - t->zt_Sec;
	if (maxdiff && (diff > maxdiff))
	{
	    msg_ELog (EF_INFO, 
		      "%s: data for %s are too old (%ds > %ds), not displayed",
		      comp, plat, diff, maxdiff);
	    return (FALSE);
	}
	else
	    return (TRUE);
}




int
DataAgeOK (const char* comp, int pid)
/*
 * If this component has an age limit, enforce it.  Return FALSE if age
 * limits are enabled and data for the given platform are too old relative 
 * to the plot time.
 */
{
    ZebTime zt;
    const char *platname = ds_PlatformName (pid);
/*
 * Nearest data time
 */
    if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, &zt))
    {
	msg_ELog(EF_DEBUG, "DataAgeOK: no data before plot time for '%s'", 
		 platname);
	return (FALSE);
    }
    return (AgeCheck(comp, platname, &zt));
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
	zbool enable = FALSE;
	float xoffset = 0, yoffset = 0, advdir, advspeed;
	const char *pname = ds_PlatformName (dc->dc_Platform);
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

	prj_Project (loc->l_lat, loc->l_lon, &xpos, &ypos);
	prj_Reverse (xpos + x, ypos + y, &loc->l_lat, &loc->l_lon);
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

#ifdef __osf__
	*center = (int) nint (*center/(*step)) * (*step);
#else
	*center = nint (*center/(*step)) * (*step);
#endif
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
	float *data = NULL;
	unsigned char *cdata = NULL;
	RGrid rg;
	ScaleInfo scale;
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
		np = rg.rg_nX * rg.rg_nY * ((rg.rg_nZ > 0) ? rg.rg_nZ : 1);
		break;

	   case DCC_Image:
		cdata = dc_ImgGetImage (dc, 0, field, 0, &rg, 0, &scale);
		np = rg.rg_nX * rg.rg_nY;
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
	if (data)
	{
		GetRange (data, np, badval, &min, &max);
	}
	else	/* calculate byte range and scale to data range */
	{
		GetByteRange  (cdata, np, &min, &max);
		min = min * scale.s_Scale + scale.s_Offset;
		max = max * scale.s_Scale + scale.s_Offset;
		if (min > max)	/* scale is negative */
		{
			float swap = min;
			min = max; 
			max = swap;
		}
	}
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
	if (*min > *max)	/* all bad values */
		*min = *max = badval;
}
	



void
GetByteRange (cdata, np, min, max)
unsigned char *cdata;
int np;
float *min, *max;
/*
 * Find the data range covered by this byte data.
 */
{
	*min = 256;
	*max = -1;
/*
 * Just pass through and figure it out.  Someday we may want to do something
 * fancier, like skipping outliers or some such.
 */
	for (; np > 0; np--)
	{
		float dv = (float) *cdata++;
		if (dv == 255)
			continue;
		if (dv < *min)
			*min = dv;
		if (dv > *max)
			*max = dv;
	}
	if (*min > *max)	/* all out-of-range bytes */
		*min = *max = 255;
}
	



# if defined(hpux) || defined(SVR4) || defined (linux) || defined(AIXV3)
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
    char cmd[512], efile[512], xwdname[128], *wname = dm_MessageName ();
    date uid;
    FILE *xwdfile;
    int imgfmt, i, ncolors;
    XImage *image;
    XWindowAttributes win_info;
    Visual *vis;
    XWDFileHeader header;
/*
 * Open the temporary xwd format file
 */
    sprintf (xwdname, "/tmp/zebra_%s.xwd", wname);
    if (! (xwdfile = fopen (xwdname, "w+")))
    {
	msg_ELog (EF_PROBLEM, "Unable to open xwd file %s\n", xwdname);
	return;
    }
/*
 * Convert our pixmap to an XImage
 */
    imgfmt = ZPixmap;
    image = XGetImage (Disp, GWGetFrame (Graphics, DisplayFrame), 0, 0, 
		       GWWidth (Graphics), GWHeight (Graphics), AllPlanes, 
		       imgfmt);
/*
 * Get the window attributes
 */
    XGetWindowAttributes (Disp, XtWindow (Graphics), &win_info);
    vis = win_info.visual;
    ncolors = vis->map_entries;
/*
 * Build the xwd header.  All pieces of the header are of type CARD32, which
 * is a 4-byte big-endian unsigned int.
 */
    ToBigUI4 (sz_XWDheader + strlen (wname) + 1, (void*)&(header.header_size));
    ToBigUI4 (XWD_FILE_VERSION, (void*)&(header.file_version));
    ToBigUI4 (imgfmt, (void*)&(header.pixmap_format));
    ToBigUI4 (image->depth, (void*)&(header.pixmap_depth));
    ToBigUI4 (image->width, (void*)&(header.pixmap_width));
    ToBigUI4 (image->height, (void*)&(header.pixmap_height));
    ToBigUI4 (image->xoffset, (void*)&(header.xoffset));
    ToBigUI4 (image->byte_order, (void*)&(header.byte_order));
    ToBigUI4 (image->bitmap_unit, (void*)&(header.bitmap_unit));
    ToBigUI4 (image->bitmap_bit_order, (void*)&(header.bitmap_bit_order));
    ToBigUI4 (image->bitmap_pad, (void*)&(header.bitmap_pad));
    ToBigUI4 (image->bits_per_pixel, (void*)&(header.bits_per_pixel));
    ToBigUI4 (image->bytes_per_line, (void*)&(header.bytes_per_line));
    ToBigUI4 (vis->class, (void*)&(header.visual_class));
    ToBigUI4 (vis->red_mask, (void*)&(header.red_mask));
    ToBigUI4 (vis->green_mask, (void*)&(header.green_mask));
    ToBigUI4 (vis->blue_mask, (void*)&(header.blue_mask));
    ToBigUI4 (vis->bits_per_rgb, (void*)&(header.bits_per_rgb));
    ToBigUI4 (vis->map_entries, (void*)&(header.colormap_entries));
    ToBigUI4 (ncolors, (void*)&(header.ncolors));
    ToBigUI4 (GWWidth (Graphics), (void*)&(header.window_width));
    ToBigUI4 (GWHeight (Graphics), (void*)&(header.window_height));
    ToBigUI4 (0, (void*)&(header.window_x));
    ToBigUI4 (0, (void*)&(header.window_y));
    ToBigUI4 (0, (void*)&(header.window_bdrwidth));
/*
 * Write out the header
 */
    fwrite ((char *)&header, sz_XWDheader, 1, xwdfile);
    fwrite (wname, strlen (wname) + 1, 1, xwdfile);
/*
 * Write the color map
 */
    for (i = 0; i < ncolors; i++)
    {
	XColor	xc;
	U_XWDColor xwdc;
    /*
     * Generate the pixel, then get the associated XColor
     */
	xc.pixel = MakePixel (i, vis);
	XQueryColor (Disp, win_info.colormap, &xc);
    /*
     * Convert it to a XWDColor
     */
	ToBigUI4 (xc.pixel, (void*)&(xwdc.pixel));
	ToBigUI2 (xc.red, (void*)&(xwdc.red));
	ToBigUI2 (xc.green, (void*)&(xwdc.green));
	ToBigUI2 (xc.blue, (void*)&(xwdc.blue));
	xwdc.flags = xc.flags;
	xwdc.pad = 0;

	fwrite ((char *) &xwdc, sz_XWDColor, 1, xwdfile);
    }
/*
 * Write the image and close the file.  
 * Image size here assumes ZPixmap format.
 */  
    fwrite (image->data, image->bytes_per_line * image->height, 1, xwdfile);
    fclose (xwdfile);
    XDestroyImage (image);
/*
 * Write in the plot time/date to generate the final file name
 */
    TC_ZtToUI (&PlotTime, &uid);
    sprintf (efile, file, uid.ds_yymmdd, uid.ds_hhmmss);
/*
 * Finally use the required system utilities to convert to the requested
 * format.
 */
    sprintf (cmd, "cat %s | %s > %s", xwdname, ImgRules[format], efile);
    cmd[sizeof(cmd)-1] = '\0';
    system (cmd);
    unlink (xwdname);
}



static Pixel 
MakePixel (int entry, Visual *vis)
/*
 * Turn the colormap entry number into a good pixel value.  For PseudoColor
 * this is trivial, but for DirectColor and TrueColor we have to set red, 
 * green, and blue bits separately.
 */
{
    if (vis->class == DirectColor || vis->class == TrueColor) 
    {
        Pixel red, green, blue, red1, green1, blue1;

        red1 = lowbit (vis->red_mask);
        red = (entry * red1) & vis->red_mask;

        green1 = lowbit (vis->green_mask);
	green = (entry * green1) & vis->green_mask;

        blue1 = lowbit (vis->blue_mask);
	blue = (entry * blue1) & vis->blue_mask;

	return (red | green | blue);
    }
    else 
	return (entry);
}



static int
FindWindComps (pid, nfid, fids, name1, name2, field1, field2)
PlatformId pid;
int nfid;
FieldId *fids;
char *name1;
char *name2;
FieldId *field1;
FieldId *field2;
{
	*field1 = F_Lookup (name1);
	*field2 = F_Lookup (name2);
	if (ds_IsDerivable (pid, *field1, fids, nfid) &&
	    ds_IsDerivable (pid, *field2, fids, nfid))
	{
	    return (1);
	}
	else
	{
	    msg_ELog (EF_DEBUG, "FindWindComps: Plat %s can't yield %s and %s",
		      ds_PlatformName (pid), name1, name2);
	    return (0);
	}
}



void
FindWindsFields (comp, plat, zt, fids, wi)
char *comp;		/* plot component name */
PlatformId plat;
ZebTime *zt;
FieldId *fids;
WindInfo *wi;
/*
 * Figure out what fields to snarf for winds.  Initialize *info according
 * to what we find.  Set the fids array to the fields which need to be
 * fetched.
 */
{
	char uname[128];
	char vname[128];
	const char *p;
	char wspd[128];
	char wdir[128];
	FieldId fields[MAXFIELD];
	int nfield = MAXFIELD;
	int found;
/*
 * Initialize info structure
 */
	wi->wi_polar = 0;
	wi->wi_wspd = BadField;
	wi->wi_wdir = BadField;
	wi->wi_uwind = BadField;
	wi->wi_vwind = BadField;

	ds_GetFields (plat, zt, &nfield, fields);
/*
 * Explicit parameters take precedence, then we start looking for default
 * field names.  Look for "x-field" and "y-field" as well since those
 * names are accepted for track plots.
 */
	p = ds_PlatformName (plat);
	found = 0;
	if (pda_Search (Pd, comp, "u-field", p, uname, SYMT_STRING) &&
	    pda_Search (Pd, comp, "v-field", p, vname, SYMT_STRING))
	{
		found = FindWindComps (plat, nfield, fields, uname, vname,
				       &wi->wi_uwind, &wi->wi_vwind);
	}

	if (! found &&
	    pda_Search (Pd, comp, "wspd-field", p, wspd, SYMT_STRING) &&
	    pda_Search (Pd, comp, "wdir-field", p, wdir, SYMT_STRING))
	{
		wi->wi_polar = 1;
		found = FindWindComps (plat, nfield, fields, wspd, wdir,
				       &wi->wi_wspd, &wi->wi_wdir);
	}

	/*
	 * Leave this to last since these are valid parms in a xywind plot
	 * but not used for wind field names.
	 */
	if (! found &&
	    pda_Search (Pd, comp, "x-field", p, uname, SYMT_STRING) &&
	    pda_Search (Pd, comp, "y-field", p, vname, SYMT_STRING))
	{
		found = FindWindComps (plat, nfield, fields, uname, vname,
				       &wi->wi_uwind, &wi->wi_vwind);
	}

	if (! found)
	{
		wi->wi_polar = 0;
		found = FindWindComps (plat, nfield, fields, "u_wind", 
				       "v_wind", &wi->wi_uwind, &wi->wi_vwind);
	}
	if (! found)
	{
		wi->wi_polar = 1;
		found = FindWindComps (plat, nfield, fields, "wspd", "wdir",
				       &wi->wi_wspd, &wi->wi_wdir);
	}
/*
 * Let someone know if we didn't succeed
 */
	if (! found)
	{
		msg_ELog (EF_PROBLEM, "no wind vector fields for %s:%s",
			  comp, p);
	}
/*
 * Now return the fields which need to be fetched.  If no wind components
 * were found in the platform, the id's will at least be valid and will be
 * filled in with bad values when fetched.
 */
	if (wi->wi_polar)
	{
		fids[0] = wi->wi_wspd;
		fids[1] = wi->wi_wdir;
	}
	else
	{
		fids[0] = wi->wi_uwind;
		fids[1] = wi->wi_vwind;
	}
}




void
GetWindData (wi, u, v, badval)
WindInfo *wi;
float *u, *v, badval;
/*
 * Return the actual wind data.
 */
{
	float wspd, wdir;
/*
 * If we already have u/v, just return it.
 */
	if (! wi->wi_polar)
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



zbool
ImageDataTime (c, pid, alt, dtime)
char	*c;
PlatformId	pid;
double		alt;
ZebTime		*dtime;
/*
 * Get the "real" time for image data, applying the current altitude 
 * selection if appropriate.
 */
{
	ZebTime wanted_time;
	int	ntime;
	zbool	rspace = FALSE;
/*
 * Of course radar is a special case.
 */
	rspace = r_RadarSpace (c);
	if (rspace)
	{
		return (r_ImageTime (c, pid, alt, dtime));
	}
/*
 * Find out when we can really get data.
 */
	wanted_time = *dtime;
	if (! (ntime = ds_DataTimes (pid, &wanted_time, 1, DsBefore, dtime)))
	{
		msg_ELog (EF_INFO, "ImageDataTime: No data for '%s'",
			  ds_PlatformName (pid));
		return (0);
	}

	return (1);
}





int
GetLLSpacings (dc, latspacing, lonspacing)
DataChunk *dc;
float *latspacing, *lonspacing;
/*
 * Attempt to get the spacings from this grid DC.
 */
{
	float *spacings, lat1, lon1, x0, y0;
	FieldId *fids;
	int nv;
	DC_ElemType type;
	Location origin;
	RGrid rg;
/*
 * If it's not an rgrid, we don't even try.
 */
	if (! dc_IsSubClassOf (dc_ClassId (dc), DCC_RGrid))
	{
		msg_ELog (EF_PROBLEM, "GetLLSpacings on non-rgrid DC");
		return (FALSE);
	}
/*
 * Look for our special attribute.
 */
	spacings = (float *) dc_GetGlobalAttrArray (dc, ATTR_LATLON, &type,
			&nv);
	if (spacings)
	{
		*latspacing = spacings[0];
		*lonspacing = spacings[1];
		return (TRUE);
	}
/*
 * Nope, no such luck.  Just calculate our best guess and return that.  Start
 * by extracting the required info from the data chunk.
 */
	fids = dc_GetFields (dc, NULL);
	(void) dc_RGGetGrid (dc, 0, fids[0], &origin, &rg, NULL);
/*
 * Project the origin and one cell away in each direction, and take the
 * difference as the spacing.
 */
	prj_Project (origin.l_lat, origin.l_lon, &x0, &y0);
	prj_Reverse (x0 + rg.rg_Xspacing, y0 + rg.rg_Yspacing, &lat1, &lon1);
	*latspacing = lat1 - origin.l_lat;
	*lonspacing = lon1 - origin.l_lon;
	return (TRUE);
}




zbool
ClosestRHI (c, pid, wanted_azim, dtime, azim)
char    *c;
PlatformId      pid;
double          wanted_azim;    /* in degrees clockwise from north */
ZebTime         *dtime;
float           *azim;
/*
 * Find an RHI radar sweep for the given platform, from the volume at or
 * before dtime.  If the "every-sweep" parameter is not in the plot
 * component or is set explicitly to false, we choose the sweep with the
 * smallest angular difference from wanted_azim.  If "every-sweep" is present
 * AND true, then we simply return the sweep closest to dtime.  We return
 * true if a scan is found, false otherwise. 
 * 
 * On successful exit:
 *      dtime           - holds the time of the sweep found
 *      azim            - the azimuth of the sweep found, in degrees clockwise
 *                        from north
 */
{
	ZebTime stimes[60], obstime;
	int     nsample, samp, ntime;
	float   diff, mindiff;
	zbool    matchazim, everysweep;
	Location        slocs[60];
/*
 * Find out when we can really get data.
 */
	if (! (ntime = ds_GetObsTimes (pid, dtime, &obstime, 1, "rhi")))
	{
		msg_ELog (EF_INFO, "ClosestRHI: No RHI data for %s before %s",
			  ds_PlatformName (pid), TC_AscTime (dtime, TC_Full));
		return (0);
	}
	
	*dtime = obstime;
/*
 * Unless they have specified that they want every sweep, we find the one
 * with the closest azimuth match.
 */
	everysweep = FALSE;
	pda_Search (Pd, c, "every-sweep", NULL, (char *) &everysweep, 
		    SYMT_BOOL);
	
	matchazim = !everysweep;
	
	if (matchazim)
	{
	/*
	 * Get the sweeps from the volume and figure out which is most nearly
	 * parallel to azimuth.
	 */
		if (! (nsample = ds_GetObsSamples (pid, &obstime, stimes, 
						   slocs, 60)))
		{
			msg_ELog (EF_PROBLEM, "ClosestRHI: No observations!");
			return (0);
		}

		mindiff = 999.0;

		for (samp = 0; samp < nsample; samp++)
		{
		/*
		 * OK, a reasonably big kluge here.  For RHI sweeps in a raster
		 * file, the "altitude" is actually the azimuth of the sweep...
		 */
			diff = slocs[samp].l_alt - wanted_azim;
			while (diff > 90.0)
				diff -= 180.0;
			while (diff < -90.0)
				diff += 180.0;

                        if (ABS (diff) < mindiff)
			{
				mindiff = ABS (diff);
				*azim = slocs[samp].l_alt;
				*dtime = stimes[samp];
			}
		}
	}
	
	return (1);
}




int
ParseFieldList (char *string, char **substrings)
/*
 * Parse comma-separated names from 'string' by putting NULLs in place of
 * the commas, and putting pointers to the beginning of each name into the
 * 'substrings' array.  Return the number of substrings in the string.
 *
 * We ignore commas enclosed in brackets.
 */
{
    int       i = 0, nsubs = 0, bracketlevel = 0;

    while (1)
    {
	int foundcomma;
    /*
     * Skip past leading white space
     */
	i += strspn (string + i, " \t");

	if (string[i] == '\0')
	    break;
    /*                                          
     * We're at the beginning of a substring
     */
	substrings[nsubs++] = string + i;
    /*
     * Skip characters until we hit a comma not enclosed in brackets
     */
	foundcomma = 0;
	while (! foundcomma)
	{
	/*
	 * Move to the next character that isn't a comma or bracket,
	 * quitting if that brings us to the end of the string.
	 */
	    i += strcspn (string + i, ",[]");
	    if (string[i] == '\0')
		break;
	/*
	 * Switch based on the character that stopped us
	 */
	    switch (string[i])
	    {
	    /*
	     * If we found a comma not enclosed in brackets, replace it
	     * with a NULL, and move on to the next field.
	     */
	      case ',':
		if (bracketlevel == 0)
		{
		    string[i] = '\0';
		    foundcomma = 1;
		}
		break;
	    /*
	     * Open bracket.  Increment the bracket level.
	     */
	      case '[':
		bracketlevel++;
		break;
	    /*
	     * Close bracket.  Decrement the bracket level.
	     */
	      case ']':
		bracketlevel--;
		break;
	    }

	    i++;
	}
    }

    return (nsubs);
}



/*
 * Print a numeric label trying to narrow its printed precision without
 * making it indistinguishable from neighboring values a 'step' apart and
 * without resorting to exponential format until the magnitude is several
 * places from the decimal point in either direction.  The precision is
 * chosen according to the magnitude of the step rather than cval.
 * Essentially we try to print two significant digits in cval on the same
 * order as the step on the thinking that should differentiate values
 * differing by 'step'.
 */
void
LabelStep (char *lbl, double step, double cval)
{
    int mag = 1;    /* Steps of zero will default to a precision of one. */

    if (step != 0.0)
    {
	double lg = log10(fabs(step));
	mag = (lg < 0) ? - (int) ceil ( -lg ) : (int) ceil (lg);
    }
    if (mag <= -6)
    {
	sprintf (lbl, "%.2g", cval);
    }
    else if (mag <= 0)
    {
	int prec = 1 - mag;
	sprintf (lbl, "%.*f", prec, cval);
    }
    else if (mag < 6)
    {
	int prec = 2 - mag;
	prec = (prec < 0) ? 0 : prec;
	sprintf (lbl, "%.*f", prec, cval);
    }
    else
    {
	sprintf (lbl, "%.2g", cval);
    }
}
