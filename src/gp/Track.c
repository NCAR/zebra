/*
 * Track drawing routines.
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

# include "config.h"
# if C_CAP_TRACKS		/* Skip everything if Tracks not desired */

# include <X11/X.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/AsciiText.h>
# include <string.h>
# include <signal.h>
# include <math.h>
# include "defs.h"
# include "pd.h"
# include "message.h"
# include "DataStore.h"
# include "GraphicsW.h"
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"

# ifndef lint
MAKE_RCSID ("$Id: Track.c,v 2.29 1994-04-15 21:26:33 burghart Exp $")
# endif

# define ARROWANG .2618 /* PI/12 */

/*
 * Forwards.
 */
static bool tr_CCSetup FP((char *, char *, char *, char *, XColor **,
		int *, float *, float *, XColor *, float *, float *, bool *));
static void tr_GetArrowParams FP((char *, char *, float *, int *, bool *,
		int *, char *, XColor *, char *, char *, char *));
static bool tr_CTSetup FP((char *, char *, PlatformId *, int *, int *,
		char *, bool *, char *, bool *, char *));
static void tr_AnnotTrack FP((char *, char *, char *, int, char *, char *,
		char *, double, double, double, char *, bool));
static void tr_AnnotTime FP((char *, char *, DataChunk *, Drawable));
static void tr_DoTimeAnnot FP((Drawable, int, int, char *, char *, double, 
		XColor, ZebTime, double rot, int justify));
static float tr_FigureRot FP ((double, double, double, double, int *));
static int tr_LocateAnnot FP ((DataChunk *dc, int sample,
			       ZebTime *a_time, int *x, int *y, 
			       float *rot, int *justify));


static float
tr_GetBadval (dc)
DataChunk *dc;
{
	if (dc_IsSubClassOf (dc->dc_Class, DCC_MetData))
		return (dc_GetBadval (dc));
	else
	{
		char *abad;
		float badvalue;

		abad = dc_GetGlobalAttr (dc, "bad_value_flag");
		if (!abad)	/* trouble */
			return (-99999.0);
		sscanf (abad,"%f",&badvalue);
		return (badvalue);
	}
}



void
tr_CAPTrack (comp, update)
char *comp;
bool update;
{
	char platform[30], ccfield[30], positionicon[40], param[40];
	char mtcolor[20], ctable[30], a_color[30];
	char a_xfield[30], a_yfield[30], a_type[30];
	int period, nc, lwidth, pid, index;
	int dskip = 0, i, a_int, numfields = 0, afield;
	int x0, y0, x1, y1;
	int samp0;		/* sample at which (x0,y0) last set  */
	long vectime;	/* the time, multiple of the arrow interval */
		        /* a_int, for which last vector arrow drawn */
	int npt;   	/* number pts read so far, for data-skipping */
	int a_lwidth, nsamp;
	bool arrow, showposition, annot_time;
	bool mono, shifted, a_invert, autoscale;
	ZebTime begin, zt;
	float *data, base, incr, a_scale;
	float unitlen, center, step;
	Drawable d;
	XColor xc, *colors, outrange, a_clr;
	DataChunk *dc;
	FieldId fields[5];
	float badvalue;
/*
 * Pull in our parameters.
 */
	if (! tr_CTSetup (comp, platform, &pid, &period, &dskip, mtcolor,
			&mono, ccfield, &showposition, positionicon))
		return;
	if (update)
		period = period/4;
/*
 * Color code field.
 */
	if (! mono)
		mono = ! tr_CCSetup (comp, platform, ccfield, ctable, &colors,
				     &nc, &base, &incr, &outrange, &center,
				     &step, &autoscale);
/*
 * Put together the field list.
 */
	if (! mono)
		fields[numfields++] = F_Lookup (ccfield);
/*
 * Read arrow parameters if necessary.
 */
	arrow = FALSE;
	tr_GetParam (comp, "arrow", platform, (char *) &arrow, SYMT_BOOL);
	if (arrow)
	{
		tr_GetArrowParams (comp, platform, &a_scale, &a_lwidth,
			&a_invert, &a_int, a_color, &a_clr, a_type, 
			a_xfield, a_yfield);
		fields[numfields] = F_Lookup (a_xfield);
		fields[numfields + 1] = F_Lookup (a_yfield);
		afield = numfields;
		numfields += 2;
	} 
/*
 * Figure the begin time and fetch data.
 */
	if (period)
	{
	  /* time-period type plot */

		begin = PlotTime;
		begin.zt_Sec -= period;
		dc = ds_Fetch (pid, numfields ? DCC_Scalar : DCC_Location,
			       &begin, &PlotTime, fields, numfields, 0, 0);
	}
	else
	{
	    /* observation type plot */
		if (update)
		{
			/* update, get data since last plot */

			if (!pda_Search ( Pd, comp, "data-end-time", NULL, 
					 (char*) &begin, SYMT_DATE))
				begin = PlotTime;

			msg_ELog(EF_DEBUG,"update in obs mode from %d to %d",
				 begin.zt_Sec, PlotTime.zt_Sec);
			dc = ds_Fetch (pid, numfields ? DCC_Scalar : DCC_Location,
				       &begin, &PlotTime, fields, numfields,
				       0, 0);
		}
		else
		{
	      	      /* global, get whole observation */
			begin = PlotTime;
	      
			if (!ds_GetObsTimes(pid, &PlotTime, &begin, 1, NULL)) {
				dc = NULL;
			} else {
				msg_ELog(EF_DEBUG,"global, FetchObs for %d", begin.zt_Sec);
				dc = ds_FetchObs(pid,
					numfields ? DCC_Scalar : DCC_Location,
					 &begin, fields, numfields, 0, 0);
	      }
	    }
        }
/*
 * Well, if we can't get any data, there is no point in going any further
 * with this whole thing.
 */
	if (! dc)
	{
		msg_ELog (EF_INFO, "No %s data available", platform);
		return;
	}
/*
 * Stash away the data end time for use with updates later.
 */
	nsamp = dc_GetNSample (dc);
	msg_ELog(EF_DEBUG,
		 "%d points returned in observation mode", nsamp);
	dc_GetTime (dc, nsamp-1, &zt);
	      
	/* remember end of this data span */
	msg_ELog(EF_DEBUG, "storing zt as data-end-time of %d", zt.zt_Sec);
	pd_Store (Pd, comp, "data-end-time", (char*) &zt, SYMT_DATE);

/*
 * Do some initial loooking over the data.
 */
	shifted = ApplySpatialOffset (dc, comp, &PlotTime);
	nsamp = dc_GetNSample (dc);
	badvalue = tr_GetBadval (dc);
/*
 * If they want autoscaling figure out how it goes now.
 */
	if (autoscale && ! mono)
	{
		FindCenterStep (dc, fields[0], nc, &center, &step);
		sprintf (param, "%s-center", ccfield);
		pd_Store (Pd, comp, param, (char *) &center, SYMT_FLOAT);
		sprintf (param, "%s-step", ccfield);
		pd_Store (Pd, comp, param, (char *) &step, SYMT_FLOAT);
	}

/*
 * Fix up the parameters to make coding a little easier.
 */
	if ((nc & 0x1) == 0)
		nc--;
	base = center - (nc/2)*step - step/2;
	incr = step;
/*
 * Fix up some graphics info.
 */
	if (mono)
		ct_GetColorByName (mtcolor, &xc);
	d = GWFrame (Graphics);
/*
 * How wide do they like their lines?
 */
	lwidth = SetLWidth (comp, "line-width", platform, 0);
	unitlen = GWHeight (Graphics) * a_scale;
/*
 * Now work through the data.
 */
	vectime = 0;
	npt = 0;
	samp0 = -1;
	for (i = 0; i < nsamp; i++)
	{
		float u, v;		/* vector component values	     */
		long timenow;		/* time of the current sample	     */
		float fx, fy;	 /* x,y Cartesian coords of lat/lon location */
		Location loc;	 	/* lat/lon locn extracted from sample*/
	/*
	 * Do skipping if requested.
	 */
		if ((dskip) && ((npt++ % dskip) != 0))
			continue;
	/*
	 * Locate this point.  Skip it if either coordinate == badvalue.
	 */
		dc_GetLoc (dc, i, &loc);
		if ((loc.l_lat == badvalue) || (loc.l_lon == badvalue))
			continue;
		cvt_ToXY (loc.l_lat, loc.l_lon, &fx, &fy);
		x1 = XPIX (fx); y1 = YPIX (fy);
	/*
	 * Draw arrows if necessary.  Get the time of the sample, and see
	 * if its time for an arrow: either the time falls on a multiple of
	 * the interval, or the interval of time has passed since the last
	 * vector was drawn.
	 */
		if (arrow)
		{
			dc_GetTime (dc, i, &zt);
			timenow = TC_ZtToSys (&zt);
			if(((timenow % a_int) == 0) || 
			   ((vectime + a_int) < timenow))
			{
				u = dc_GetScalar (dc, i, fields[afield]);
				v = dc_GetScalar(dc, i, fields[afield+1]);
				if (u != badvalue && v != badvalue) {
				  vectime = timenow - timenow % a_int;
				  FixForeground (a_clr.pixel);
				  FixLWidth (a_lwidth);
				  draw_vector (Disp, d, Gcontext, 
					       x1, y1, u, v, unitlen);
				  FixLWidth (lwidth);
				}
			}
		}
	/*
	 * Draw the line, if (x0,y0) valid, color coding if requested.
	 */
		if (samp0 >= 0)
		{
			if (mono)
				FixForeground (xc.pixel);
			else
			{
				index = (dc_GetScalar (dc, i, fields[0]) - 
					 base)/incr;
				FixForeground ((index >= 0 && index < nc) ?
					       colors[index].pixel : 
					       outrange.pixel);
			}
			XDrawLine (Disp, d, Gcontext, x0, y0, x1, y1); 
		}
		x0 = x1; y0 = y1;
		samp0 = i;
	}
/*
 * If this isn't an update, indicate which end of the track is the front.
 */
	if ((! update) && showposition && (samp0 >= 0))
		I_PositionIcon (comp, platform, &zt, positionicon, x0, y0,
				mono ? xc.pixel : (index >= 0 && index < nc) ? 
				colors[index].pixel : outrange.pixel);
/*
 * If this isn't an update, see about annotating the track with times.
 */
	annot_time = FALSE;
	if ((! update) && pda_Search (Pd, comp, "annot-time", "track", 
		(char *) &annot_time, SYMT_BOOL)) 
	{
		if (annot_time)
			tr_AnnotTime (comp, platform, dc, d);
	}
	ResetGC ();
/*
 * Put in the status line before we lose the data object, then get rid of it.
 * (Only do it if this isn't an update.  It won't get printed anyway, and
 * it's likely to overflow the overlay widget's text space.)
 */
	if (! update)
	{
		dc_GetTime (dc, nsamp - 1, &zt);
		ot_AddStatusLine (comp, platform, mono ? "" : ccfield, &zt);
	}
	dc_DestroyDC (dc);
/*
 * Annotate if necessary.
 */
	if (! update)
		tr_AnnotTrack (comp, platform, mono ? NULL : ccfield, arrow,
			a_type, mtcolor, ctable, center, step, unitlen,
			mono ? mtcolor : a_color, shifted);
}





static void
tr_AnnotTime (comp, platform, dc, d)
char	*comp, *platform;
DataChunk	*dc;
Drawable	d;
/*
 * Annotate the track with times.
 */
{
	char	interval[20], label[40], icon[40], color[20];
	int	i, interval_sec, nsamp;
	int	x, y;
	float	label_scale, rot;
	XColor	x_color;
	ZebTime	when, t;
	int	justify;
/*
 * Get the time interval. 
 */
	if (! pda_Search (Pd, comp, "annot-time-interval", "track", 
		interval, SYMT_STRING))
	{
	/*
	 * Default to one hour intervals.
	 */
		interval_sec = 3600;
	}
	else
	{
		interval_sec = pc_TimeTrigger (interval);
	}
/*
 * Make sure the user hasn't given us a bad interval
 */
	if (!interval_sec)
	{
		msg_ELog (EF_PROBLEM,
		  "bad annotation interval for aircraft track %s, using 1h",
		  comp);
		interval_sec = 3600;
	}
/*
 * Label to use.  May be either "none", "time", or a string.
 */
	if (! pda_Search (Pd, comp, "annot-time-label", "track", 
			label, SYMT_STRING))
		strcpy (label, "time");
/*
 * Icon used to mark the track.
 */
	if (! pda_Search (Pd, comp, "annot-time-icon", "track", 
			icon, SYMT_STRING))
		strcpy (icon, "littlecircle");
/*
 * Color for time annotations.
 */
	if (! pda_Search (Pd, comp, "annot-time-color", "track", 
			color, SYMT_STRING))
		strcpy (color, "white");
	if (! ct_GetColorByName (color, &x_color))
	{
		strcpy (color, "white");
		ct_GetColorByName (color, &x_color);
	}
/*
 * Scale of the labels.
 */
	if (! pda_Search (Pd, comp, "annot-time-scale", "track", 
			(char *) &label_scale, SYMT_FLOAT))
		label_scale = 0.01;
/*
 * Set label-blanking to false.
 */
	dt_SetBlankLabel (FALSE);
/*
 * Figure the first time that we want to annotate (most recent time on
 * the annot-time-interval boundary).
 */
	nsamp = dc_GetNSample (dc);
	dc_GetTime (dc, nsamp - 1, &when);
	t.zt_Sec = when.zt_Sec - (when.zt_Sec % interval_sec);
/*
 * Loop through the data.
 */
	for (i = nsamp - 2; i >= 0; i--)
	{
	/*
	 * Get the time of this sample.
	 */
		dc_GetTime (dc, i, &when);
		if (when.zt_Sec <= t.zt_Sec)
		{
		/*
		 * Figure out where the annotation is supposed to go, and
		 * do the annotation if possible.
		 */
			if (tr_LocateAnnot (dc, i, &t, &x, &y, &rot, &justify))
				tr_DoTimeAnnot (d, x, y, icon, label, 
						label_scale, x_color, t, 
						rot, justify);
		/*
		 * Decrement the time to the next interval multiple.
		 */
			t.zt_Sec -= interval_sec;
		} 
	}

}




static float
tr_FigureRot (x0, y0, x1, y1, justify)
float	x0, y0, x1, y1;
int *justify;
/*
 * Figure a rotation factor (in degrees) which is perpendicular to the 
 * line defined by (x0, y0) and (x1, y1).
 */
{
	double	theta;
/*
 * Find the angle, -pi to pi, defined by the right-hand perpendicular of
 * the line (x0,y0) - (x1,y1).
 */
	if ((x0 - x1 == 0) && (y1 - y0 == 0))	/* to avoid DOMAIN warnings */
		theta = 0.0;
	else
		theta = atan2 ( (double)(x0 - x1), (double)(y1 - y0) );
/*
 * Always label the "right" side of line, depending upon direction, putting
 * labels perpendicular to line but right-side-up, and justifying according
 * to the side.
 */
	if ((theta > M_PI/2.0) || (theta < -(M_PI)/2.0)) /* in Quads II,III */
	{
		*justify = JustifyRight;
		theta += M_PI;
	}
	else
	{
		*justify = JustifyLeft;
	}
	if (theta < 0)
		theta += 2*M_PI;
	return ((float)(theta * 180.0 / M_PI));
}







static void
tr_DoTimeAnnot (d, x, y, icon, label, label_scale, x_color, t, rot, justify)
Drawable	d;
int		x, y;
char		*icon, *label;
float		label_scale, rot;
XColor		x_color;
ZebTime		t;
int		justify;
/*
 * Draw the icon and place the text for a time annotation.
 */
{
	char	label_str[40];
/*
 * Place the icon.
 */
	ov_PositionIcon (icon, x, y, x_color.pixel);
/*
 * Label it.
 */
	if (strcmp (label, "time") == 0)
	{
	/*
	 * Insert some space before the time
	 */
		strcpy (label_str, "  ");
		TC_EncodeTime (&t, TC_TimeOnly, label_str + strlen(label_str));
	/*
	 * Get rid of seconds, and add a space for right-justified times.
	 */
		label_str[strlen (label_str) - 3] = '\0';
		strcat (label_str, "  ");
	}
	else if (strcmp (label, "none") == 0)
		return;
	else 
	{
		strncpy (label_str, label, sizeof(label_str));
		label_str[sizeof(label_str) - 1] = '\0';
	}
	DrawText (Graphics, d, Gcontext, x, y, label_str, rot, label_scale, 
		  justify, JustifyCenter);
}




static void
tr_AnnotTrack (comp, platform, ccfield, arrow, a_type, mtcolor, ctable,
	center, step, unitlen, a_color, shifted)
char *comp, *platform, *ccfield, *a_type, *mtcolor, *ctable, *a_color;
int arrow;
float center, step, unitlen;
bool shifted;
/*
 * Annotate the track we have just done.
 */
{
	char tadefcolor[30], datastr[100];
	XColor tadefclr, taclr, xc;
	bool tacmatch = FALSE;
	float sascale;
/*
 * Read in annotation information.
 */
	if(! tr_GetParam ("global", "ta-color", NULL, tadefcolor, 
			SYMT_STRING))
		strcpy (tadefcolor, "white");
	if(! ct_GetColorByName (tadefcolor, &tadefclr))
	{
		msg_ELog (EF_PROBLEM,"Can't get default color: '%s'.", 
			tadefcolor);
		strcpy (tadefcolor,"white");
		ct_GetColorByName (tadefcolor,&tadefclr);
	}
/*
 * Do we do color matching?
 */
	tr_GetParam("global", "ta-color-match", NULL, (char *) &tacmatch,
		SYMT_BOOL);
	if(tacmatch)
		ct_GetColorByName (a_color, &taclr);
	else
		taclr = tadefclr;
/*
 * And scaling for side annotation.
 */
	if(! tr_GetParam(comp, "sa-scale", platform, (char *) &sascale,
		SYMT_FLOAT))
		sascale = 0.02;
/*
 * Annotate along the top.
 */
	An_TopAnnot(" ", tadefclr.pixel);
	An_TopAnnot (platform, tadefclr.pixel);
	if (ccfield)
	{
		An_TopAnnot(" ", tadefclr.pixel);
		An_TopAnnot (px_FldDesc (ccfield), tadefclr.pixel);
	}
	An_TopAnnot (" track", tadefclr.pixel);
	if (shifted)
		An_TopAnnot (" (SHIFTED)", tadefclr.pixel);
/*
 * Annotate arrows if necessary.
 */
	if (arrow)
	{
		An_TopAnnot (" with ",tadefclr.pixel);
		An_TopAnnot (a_type,taclr.pixel);
		An_TopAnnot (" vectors",taclr.pixel);
	}
	An_TopAnnot (".  ", tadefclr.pixel);
/*
 * Down the side too.
 */
	if (! ccfield)
	{
		ct_GetColorByName (mtcolor, &xc);
		sprintf (datastr, "%s %d", platform, xc.pixel);
		An_AddAnnotProc (An_ColorString, comp, datastr,
			strlen (datastr), 25, FALSE, FALSE);
	}
	else
	{
		sprintf (datastr, "%s %s %f %f", ccfield, ctable,center, step);
		An_AddAnnotProc (An_ColorBar, comp, datastr,
			strlen (datastr), 75, TRUE, FALSE);
		if (arrow)
		{
			sprintf (datastr, "%s %d %f %f %f", "10m/sec", 
				taclr.pixel, 10.0, 0.0, unitlen);
			An_AddAnnotProc (An_ColorVector, comp, datastr,
				strlen (datastr), 25, FALSE, FALSE);
		}
	}
}



static void
tr_GetLocations (dc, sample, loc0, samp0, loc1, samp1)
DataChunk *dc;
int sample;
Location *loc0;
int *samp0;
Location *loc1;
int *samp1;
/*
 * Given a sample, find the sample in the datachunk nearest to it,
 * including the sample itself, with a valid location; and find the first
 * sample after the given sample with a valid location.  samp0 and samp1
 * return as -1 if a good location could not be found meeting the
 * requirements.
 *
 * As an added bonus, we'll look a few seconds into the future (or to the
 * very end, whichever comes first) in an attempt to smooth the slope
 * determined by loc0 and loc1.
 */
{
	float badval;
	Location loc;
	ZebTime t0, t1;
	int i;
	int nsample;

	badval = tr_GetBadval (dc);
	i = sample + 1;
	*samp0 = -1;
	while (--i >= 0)
	{
		dc_GetLoc (dc, i, &loc);
		if (loc.l_lat == badval || loc.l_lon == badval)
			continue;
		else
		{
			*samp0 = i;
			*loc0 = loc;
			dc_GetTime (dc, i, &t0);
			break;
		}
	}

	nsample = dc_GetNSample (dc);
	i = sample;
	*samp1 = -1;
	while (++i < nsample)
	{
		dc_GetLoc (dc, i, &loc);
		if (loc.l_lat == badval || loc.l_lon == badval)
			continue;
		else
		{
		/*
		 * Try to get closest to 5 seconds into the future without
		 * going past.  Otherwise we take the first that's valid.
		 */
			dc_GetTime (dc, i, &t1);
			if ((*samp1 >= 0) && (t1.zt_Sec - t0.zt_Sec > 5))
				break;
			*samp1 = i;
			*loc1 = loc;
		}
	}
}




static int
tr_LocateAnnot (dc, sample, a_time, x, y, rot, justify)
DataChunk *dc;
int sample;		/* Current sample about where annot needs plotting */
ZebTime *a_time;	/* Time desired for annotation */
int *x, *y;		/* Pixel location to put annotation */
float *rot;		/* Rotation angle to use for annotation */
int *justify;		/* Justification to use for annotation */
/*
 * Given the dc and the sample, find a location, interpolating over bad
 * values if necessary, for the annotation and the rotation to use for the
 * text.  Return 1 on success, 0 on failure.
 */
{
	ZebTime t0, t1;
	float fx, fy, fx0, fy0, fx1, fy1;
	Location loc0, loc1;
	int samp0, samp1;
/*
 * Try to find valid locations surrounding this particular sample
 */
	tr_GetLocations (dc, sample, &loc0, &samp0, &loc1, &samp1);
/*
 * If both locations valid, get the times for each and interpolate between
 * them to find a location for the desired annotation time.
 */
	if (samp0 >= 0)
	{
		dc_GetTime (dc, samp0, &t0);
		cvt_ToXY (loc0.l_lat, loc0.l_lon, &fx0, &fy0);
		if (samp1 >= 0)
			cvt_ToXY (loc1.l_lat, loc1.l_lon, &fx1, &fy1);
	/*
	 * If the time of the given sample equals the desired annot time,
	 * we use the sample's location
	 */
		if ( t0.zt_Sec == a_time->zt_Sec )
		{
			fx = fx0;
			fy = fy0;
		}
	/*
	 * Otherwise we have to interpolate between the locations
	 */
		else if (samp1 >= 0)
		{
			dc_GetTime (dc, samp1, &t1);
			fx = fx0 + (fx1 - fx0)*(a_time->zt_Sec - t0.zt_Sec) / 
				(t1.zt_Sec - t0.zt_Sec);
			fy = fy0 + (fy1 - fy0)*(a_time->zt_Sec - t0.zt_Sec) / 
				(t1.zt_Sec - t0.zt_Sec);
		}
		*x = XPIX (fx); *y = YPIX (fy);
	}
	else	/* we're screwed, abandon our efforts */
		return (0);
/*
 * Now we've got to determine the rotation.  If we don't have a samp1 to
 * determine the slope, just supply a default.
 */
	if (samp1 >= 0)
		*rot = tr_FigureRot (fx, fy, fx1, fy1, justify);
	else
	{
		*rot = 0;
		*justify = JustifyLeft;
	}
	return (1);
}




static bool
tr_CTSetup (comp, platform, pid, period, dskip, mtcolor, mono, ccfield,
	showposition, positionicon)
char *comp, *platform, *mtcolor, *ccfield, *positionicon;
PlatformId *pid;
int *period, *dskip;
bool *mono, *showposition;
/*
 * Do the basic setup to plot aircraft tracks.
 */
{
	char tmp[80];
/*
 * Get our platform first, since that's what is of interest to us.
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return (FALSE);
	if ((*pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform '%s'", platform);
		return (FALSE);
	}
/*
 * Make sure that we'll get the right sort of stuff.
 */
	if (! ds_IsMobile (*pid))
	{
		msg_ELog (EF_PROBLEM, "Track attempted on static platform %s",
			platform);
		return (FALSE);
	}
/*
 * Pull out other parameters of interest.
 */
	if (! tr_GetParam (comp, "time-period", platform, tmp, SYMT_STRING))
		*period = 300;
	else if ((!strcmp(tmp, "observation"))) {
	  /* if time-period == "observation", set period == 0 as a flag */
	  *period = 0;
	  } else if ((*period = pc_TimeTrigger (tmp)) == 0)
	    {
	      msg_ELog (EF_PROBLEM, "Unparsable time-period: '%s'", tmp);
	      *period = 300;
	    }
/*
 * Do they want us to pare things down?
 */
	if (! tr_GetParam (comp, "data-skip", platform, (char *) dskip,
			SYMT_INT))
		*dskip = 0;
/*
 * Color info.
 */
	if (! tr_GetParam (comp, "color", platform, mtcolor,SYMT_STRING))
		strcpy (mtcolor, "white");
/*
 * Color code field.
 */
	*mono = ! (tr_GetParam (comp, "field", platform, ccfield, SYMT_STRING)
			|| tr_GetParam (comp, "color-code-field", platform,
				ccfield, SYMT_STRING));
/*
 * Show the location?
 */
	if (! tr_GetParam (comp, "show-position", platform, 
				(char *) showposition, SYMT_BOOL))
		*showposition = FALSE;
	if (*showposition && (! tr_GetParam (comp, "position-icon", platform, 
			positionicon, SYMT_STRING)))
	{
		msg_ELog (EF_PROBLEM, "Show position, but no icon.");
		*showposition = FALSE;
	}
	return (TRUE);
}





static bool
tr_CCSetup (comp, platform, ccfield, ctable, colors, nc, base, incr, outrange,
	center, step, autoscale)
char *comp, *platform, *ccfield, *ctable;
XColor **colors, *outrange;
int *nc;
float *base, *incr, *center, *step;
bool *autoscale;
/*
 * Get everything set up to color-code a track.
 */
{
	char orc[20], param1[50], param2[50], mode[32];
/*
 * Get the color table.
 */
	if (! tr_GetParam (comp, "color-table", platform, ctable, SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM,
			"No color table specified in component %s", comp);
		return (FALSE);
	}
	if (! ct_LoadTable (ctable, colors, nc))
	{
		msg_ELog (EF_PROBLEM, "Unable to load color table %s", ctable);
		return (FALSE);
	}
/*
 * Autoscaling?
 */
	if (pda_Search (Pd, comp, "scale-mode", "track", mode, SYMT_STRING))
		*autoscale = strcmp (mode, "manual");
	else
		*autoscale = FALSE;
/*
 * Get our color coding parameters.
 */
	if (! *autoscale)
	{
		sprintf (param1, "%s-center", ccfield);
		sprintf (param2, "%s-step", ccfield);
		if (! pda_ReqSearch (Pd, comp, param1, "track", (char *)center,
				     SYMT_FLOAT) ||
		    ! pda_ReqSearch (Pd, comp, param2, "track", (char *) step,
				     SYMT_FLOAT))
			return (FALSE);
	}
/*
 * Something for completely funky colors too.  Red is probably a bad
 * choice but that's what we have for now.
 */
	if (! tr_GetParam (comp, "out-of-range-color", ccfield, orc,
		SYMT_STRING))
		strcpy (orc, "red");
	if (! ct_GetColorByName (orc, outrange))
	{
		msg_ELog (EF_PROBLEM, "Bad out of range color: %s", orc);
		ct_GetColorByName ("red", outrange); /* assume this works */
	}

	return (TRUE);
}



static void
tr_GetArrowParams (comp, platform, a_scale, a_lwidth, a_invert, a_int, 
		a_color, a_clr, a_type, a_xfield, a_yfield)
char *comp, *platform, *a_type, *a_xfield, *a_yfield, *a_color;
float *a_scale;
int *a_lwidth, *a_int;
bool *a_invert;
XColor *a_clr;
/*
 * Get the parameters that control track arrows.
 */
{
	char a_interval[30];
/*
 * Misc params.
 */
	if(! tr_GetParam(comp, "arrow-scale", platform, (char *) a_scale,
			SYMT_FLOAT))
		*a_scale = 0.007;
	if(! tr_GetParam(comp, "arrow-line-width", platform, (char *) a_lwidth,
			SYMT_INT))
		*a_lwidth = 1;
	if(! tr_GetParam(comp, "arrow-invert", platform, (char *) a_invert,
			SYMT_BOOL))
		*a_invert = FALSE;
/*
 * Get and parse the arrow interval.
 */
	if(! tr_GetParam(comp, "arrow-interval", platform, a_interval,
			SYMT_STRING))
		*a_int = 10;
	else if((*a_int = pc_TimeTrigger (a_interval)) == 0)
	{
		msg_ELog(EF_PROBLEM,"Unparsable arrow interval: '%s'.",
			a_interval);
		*a_int = 10;
	}
/*
 * Color information.
 */
	if(! tr_GetParam (comp, "arrow-color", platform, a_color, SYMT_STRING))
		strcpy (a_color, "white");
	if(! ct_GetColorByName (a_color, a_clr))
	{
		msg_ELog (EF_PROBLEM, "Can't get arrow color: '%s'.",a_color);
		strcpy (a_color, "white");
		ct_GetColorByName (a_color, a_clr);
	}
/*
 * And what are we actually plotting?
 */
	if(! tr_GetParam (comp, "arrow-type", platform, a_type, SYMT_STRING))
		strcpy (a_type,"wind");
	if(! tr_GetParam (comp, "x-field", platform, a_xfield, SYMT_STRING))
		strcpy (a_xfield, "u_wind");
	if(! tr_GetParam (comp, "y-field", platform, a_yfield, SYMT_STRING))
		strcpy (a_yfield, "v_wind");
}




tr_GetParam (comp, param, qual, target, type)
char *comp, *param, *qual, *target;
int type;
/*
 * Get a PD parameter.
 */
{
	return (pda_Search (Pd, comp, param, qual, target, type) ||
		pda_Search (Pd, comp, param, "track", target, type));
}


# endif /* C_CAP_TRACKS */
