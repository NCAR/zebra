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
# include <defs.h>
# include <draw.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include <GraphicsW.h>

# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"

RCSID ("$Id: Track.c,v 2.50 2001-04-20 05:04:55 granger Exp $")

# define ARROWANG .2618 /* PI/12 */
# ifndef M_PI
# define M_PI 3.14159265358979323846
# endif

/*
 * It's amazing how something as conceptually simple as tracks can turn into
 * some of the gnarliest stuff in the whole system.  In an attempt to
 * simplify the interface to the various routines, and to keep all of
 * the control parameters together, the following structure is now used to
 * hold them all.
 */
typedef struct _TrackParams
{
/*
 * General parameters.
 */
	int	tp_Period;			/* Time period to plot	*/
	int	tp_LWidth;			/* Line width		*/
	int	tp_DSkip;			/* Data skip		*/
	int	tp_NField;			/* How many fields	*/
	FieldId	tp_Fields[5];			/* The fields		*/
	zbool	tp_AnnotTime;			/* Time annotations?	*/
/*
 * Color coding parameters.
 */
	zbool	tp_Mono;			/* Plotting mono?	*/
	zbool	tp_ShowPosition;		/* Add position icon?	*/
	zbool	tp_PositionArrow;		/* Arrow at end of track*/
	zbool	tp_AutoScale;			/* Scale automatically? */
	char	tp_CCField[CFG_FIELD_NAME_LEN];	/* color-code field 	*/
	char	tp_MTColor[CFG_FILENAME_LEN];	/* Color for mono tracks*/
	char	tp_CTable[CFG_FILENAME_LEN];	/* Color table for coded*/
	int	tp_NColor;			/* Number of colors	*/
	char	tp_PosIcon[CFG_FILENAME_LEN];	/* Position icon 	*/
	XColor	*tp_Colors;			/* Color cell values	*/
	XColor	tp_Outrange;			/* Out-of-range color	*/
	float	tp_Center, tp_Step;		/* Range values		*/
/*
 * Arrow parameters.
 */
	zbool	tp_DoArrows;			/* Do they want them?	*/
	zbool	tp_AInvert;			/* Flip 180 degrees?	*/
	zbool	tp_Barb;			/* Really do barbs	*/
	char	tp_AColor[CFG_FILENAME_LEN];	/* Arrow color		*/
	char	tp_AType[CFG_PDPARAM_LEN];	/* Type (for annot)	*/
	int	tp_AInterval;			/* Arrow interval	*/
	float	tp_ArrowScale;			/* Scale		*/
	int	tp_ALWidth;			/* Line width for arrows*/
	XColor	tp_ArrowColor;			/* Which color to use	*/
	float	tp_UnitLen;			/* Arrow length/unit	*/
	int	tp_AField;			/* Index to arrow data	*/
	int	tp_ShaftLen;			/* Barb shaft length	*/
/*
 * Results and informational stuff.
 */
	int	tp_Shifted;			/* Spatial shifting done */
	int	tp_NPlats;			/* Total platforms 	*/
	long	tp_Vectime;			/* When last arrow drawn */
	float	tp_BadValue;			/* Bad value flag	*/
} TrackParams;


/*
 * Forwards.
 */
static zbool tr_CCSetup FP((char *, char *, TrackParams *));
static void tr_GetArrowParams FP((char *, char *, TrackParams *));
static zbool tr_CTSetup FP((char *, char **, PlatformId *,TrackParams*));
static void tr_AnnotTrack FP((char *, char *, TrackParams *));
static void tr_AnnotTime FP((char *, char *, DataChunk *, Drawable,
		TrackParams *));
static void tr_DoTimeAnnot FP((Drawable, int, int, char *, char *, double, 
		XColor, ZebTime, double rot, int justify));
static float tr_FigureRot FP ((double, double, double, double, int *));
static int tr_LocateAnnot FP ((DataChunk *dc, int sample,
			       ZebTime *a_time, int interval_sec, 
			       int *x, int *y, float *rot, int *justify));
static int tr_GetParam FP ((char *comp, char *param, char *qual, 
			    char *target, int type));
static DataChunk *tr_DoNSpace FP ((PlatformId pid, FieldId *fields, 
				   int numfields, dsDetail *details, 
				   int ndetail));
static DataChunk *tr_TrajNSpace FP ((PlatformId pid, FieldId *fields, 
				     int numfields, dsDetail *details, 
				     int ndetail));
static DataChunk *tr_GetData FP ((char *, int, PlatformId, TrackParams *,
		dsDetail *, int, ZebTime *));
static void tr_Barb FP ((Drawable, int, int, double, double, int));
static float tr_GetBadval FP ((DataChunk *));
static void tr_PlotPlatform FP ((char *, int, char *, PlatformId,
		TrackParams *));
static void tr_DrawArrow FP ((Drawable, DataChunk *, int, WindInfo, int, int,
		TrackParams *));


/*
 * Time for some real code.
 */





void
tr_CAPTrack (comp, update)
char *comp;
zbool update;
/*
 * Add a track to a CAP plot.
 */
{
	char *pnames[MaxPlatforms], annot[PlatformListLen];
	int p;
	TrackParams tparams;
	PlatformId pids[MaxPlatforms];
/*
 * Pull in our parameters.
 */
	if (! tr_CTSetup (comp, pnames, pids, &tparams))
		return;
/*
 * Go through and plot the track for each platform.
 */
	for (p = 0; p < tparams.tp_NPlats; p++)
		tr_PlotPlatform (comp, update, pnames[p], pids[p], &tparams);
/*
 * Finish up the annotation if need be.
 */
	if (! update)
	{
		annot[0] = '\0';
		for (p = 0; p < tparams.tp_NPlats; p++)
		{
			if (p > 0)
				strcat (annot, " ");
			strcat (annot, pnames[p]);
		}
		tr_AnnotTrack (comp, annot, &tparams);
	}
}






static void
tr_PlotPlatform (comp, update, pname, pid, tparams)
char *comp, *pname;
zbool update;
PlatformId pid;
TrackParams *tparams;
/*
 * Do all the work to get a single track on the screen.
 */
{
	char param[CFG_PDPARAM_LEN], ctime[64];
	int index, i, x0, y0, x1, y1, x00, y00;
	int npt, nsamp, ndetail, samp0;
	ZebTime begin, zt;
	float base, incr, fx0, fy0;
	Drawable d;
	XColor xc;
	DataChunk *dc;
	WindInfo wi;
	dsDetail details[5];
/*
 * Color code field.
 */
	if (! tparams->tp_Mono)
	{
		tr_CCSetup (comp, pname, tparams);
		if (! tparams->tp_Mono && tparams->tp_AutoScale &&
				(tparams->tp_NPlats > 1))
		{
			msg_ELog (EF_PROBLEM, 
				"Can't autoscale tracks w/multiple platforms");
			return;
		}
	}
/*
 * Start the field list.
 */
	tparams->tp_NField = 0;
	if (! tparams->tp_Mono)
		tparams->tp_Fields[tparams->tp_NField++] =
			F_Lookup (tparams->tp_CCField);
/*
 * Read arrow parameters if necessary.
 */
	tparams->tp_DoArrows = FALSE;
	tr_GetParam (comp, "arrow", pname, (char *) &tparams->tp_DoArrows, 
		     SYMT_BOOL);
	if (tparams->tp_DoArrows)
	{
		tr_GetArrowParams (comp, pname, tparams);
		FindWindsFields (comp, pid, &PlotTime, 
				tparams->tp_Fields + tparams->tp_NField, &wi);
		tparams->tp_AField = tparams->tp_NField;
		tparams->tp_NField += 2;
	} 
/*
 * Check for a particular bad value to use for this platform.
 * Useful for platforms with bad locations since locations don't
 * get processed by file access routines like regular fields. 
 */
	ndetail = 0;
	if (pda_Search (Pd, comp, "bad-value", pname, 
			(char *)&tparams->tp_BadValue, SYMT_FLOAT))
	{
		details[ndetail].dd_V.us_v_float = tparams->tp_BadValue;
		details[ndetail++].dd_Name = DD_FETCH_BADVAL;
	}
/*
 * Time to get some data!
 */
	dc = tr_GetData (comp, update, pid, tparams, details, ndetail, &begin);
/*
 * Well, if we can't get any data, there is no point in going any
 * further with this whole thing. 
 */
	if (! dc)
	{
		msg_ELog (EF_INFO, "No %s data available", pname);
		return;
	}
/*
 * Set the badval here anyway, in case bad values are not supported
 * (ARM) but we still got a value to use from the pd.  If bad value
 * fetching is supported, then this doesn't hurt anything. 
 */
	if (ndetail)
		dc_SetBadval (dc, tparams->tp_BadValue);
/*
 * Remember the begin time in case we don't plot any points here
 * because of data skips. 
 */
	zt = begin;
	nsamp = dc_GetNSample (dc);
	msg_ELog (EF_DEBUG, "track %s: %d points to plot in %s mode", 
			comp, nsamp, update ? "update" :
			(tparams->tp_Period ? "period" : "obs"));
/*
 * Do some initial looking over the data.
 */
	tparams->tp_Shifted = ApplySpatialOffset (dc, comp, &PlotTime);
	nsamp = dc_GetNSample (dc);
	tparams->tp_BadValue = tr_GetBadval (dc);
/*
 * If they want autoscaling figure out how it goes now.
 */
	if (tparams->tp_AutoScale && ! tparams->tp_Mono)
	{
		FieldId fid = F_Lookup (tparams->tp_CCField);
		char *justname = F_GetName (fid);

		FindCenterStep (dc, tparams->tp_Fields[0], tparams->tp_NColor,
				&tparams->tp_Center, &tparams->tp_Step);

		sprintf (param, "%s-center", justname);
		pd_Store (Pd, comp, param, (char *) &tparams->tp_Center,
				SYMT_FLOAT);

		sprintf (param, "%s-step", justname);
		pd_Store (Pd, comp, param, (char *) &tparams->tp_Step,
				SYMT_FLOAT);
	}
/*
 * Fix up the parameters to make coding a little easier.
 */
	if ((tparams->tp_NColor & 0x1) == 0)
		tparams->tp_NColor--;
	base = tparams->tp_Center -
		(tparams->tp_NColor/2)*tparams->tp_Step - tparams->tp_Step/2;
	incr = tparams->tp_Step;
/*
 * Fix up some graphics info.
 */
	if (tparams->tp_Mono)
		ct_GetColorByName (tparams->tp_MTColor, &xc);
	d = GWFrame (Graphics);
/*
 * How wide do they like their lines?
 */
	tparams->tp_LWidth = SetLWidth (comp, "line-width", pname, 0);
	tparams->tp_UnitLen = GWHeight (Graphics)*tparams->tp_ArrowScale;
/*
 * Now work through the data.
 */
	tparams->tp_Vectime = 0;
	npt = 0;
	x00 = y00 = -1;
	samp0 = -1;
	for (i = 0; i < nsamp; i++)
	{
		float u, v; /* vector component values	     */
		long timenow; /* time of the current sample	     */
		float fx, fy; /* x,y Cartesian coords of lat/lon */
		Location loc; /* lat/lon locn extracted from sample*/
	/*
	 * Do skipping if requested.
	 */
		if ((tparams->tp_DSkip) && ((npt++ % tparams->tp_DSkip) != 0))
			continue;
	/*
	 * Locate this point.  Skip it if either coordinate ==
	 * badvalue. 
	 */
		dc_GetLoc (dc, i, &loc);
		dc_GetTime (dc, i, &zt);
		if ((loc.l_lat == tparams->tp_BadValue) ||
				(loc.l_lon == tparams->tp_BadValue))
			continue;
		prj_Project (loc.l_lat, loc.l_lon, &fx, &fy);
		x1 = XPIX (fx); y1 = YPIX (fy);
	/*
	 * Draw arrows if necessary.
	 */
		if (tparams->tp_DoArrows)
			tr_DrawArrow (d, dc, i, wi, x1, y1, tparams);
	/*
	 * Draw the line, if (x0,y0) valid, color coding if requested.
	 * Don't draw the line if it doesn't intersect the plot region.
	 */
		if (samp0 >= 0 && Intersects (fx0, fy0, fx, fy))
		{
			if (tparams->tp_Mono)
				FixForeground (xc.pixel);
			else
			{
				index = (dc_GetScalar (dc, i,
						tparams->tp_Fields[0]) - 
						base)/incr;
				FixForeground ((index >= 0 &&
						index < tparams->tp_NColor) ?
						tparams->tp_Colors[index].pixel
						: tparams->tp_Outrange.pixel);
			}
			XDrawLine (Disp, d, Gcontext, x0, y0, x1, y1); 
		}
		x00 = x0; x0 = x1; y00 = y0; y0 = y1;
		fx0 = fx; fy0 = fy;
		samp0 = i;
	}
/*
 * remember the last point plotted
 */
	TC_EncodeTime (&zt, TC_Full, ctime);
	msg_ELog (EF_DEBUG, "storing data-end-time: %s", ctime);
	pd_Store (Pd, comp, "data-end-time", (char*) &zt, SYMT_DATE);
/*
 * If this isn't an update, indicate which end of the track is the
 * front. 
 */
	if ((! update) && tparams->tp_ShowPosition && (samp0 >= 0))
	{
		if (tparams->tp_PositionArrow)
		{
			double theta;

			if (x00 < 0 && y00 < 0)
			{
				x00 = 0;
				y00 = y0 - 1;
			}

			theta = ATAN2 ((double)(y00 - y0), 
					(double)(x00 - x0));

			XDrawLine (Disp, d, Gcontext, x0, y0, 
					x0 + 15 * cos (theta - 0.26),
					y0 + 15 * sin (theta - 0.26));
			XDrawLine (Disp, d, Gcontext, x0, y0,
					x0 + 15 * cos (theta + 0.26),
					y0 + 15 * sin (theta + 0.26));
		}
	/*
	 * I_PositionIcon disregards clipping, so check the location.
	 */
		else if (fx0 >= Xlo && fx0 <= Xhi && fy0 >= Ylo && 
				fy0 <= Yhi)
		{
			I_PositionIcon (comp, pname, &zt, 
					tparams->tp_PosIcon, 
					x0, y0, tparams->tp_Mono ? xc.pixel : 
					(index >= 0 && index < tparams->tp_NColor) ? 
					tparams->tp_Colors[index].pixel:
					tparams->tp_Outrange.pixel);
		}
	}
/*
 * See about annotating the track with times.
 */
	if (tparams->tp_AnnotTime)
		tr_AnnotTime (comp, pname, dc, d, tparams);
	ResetGC ();
/*
 * Put in the status line before we lose the data object, then get
 * rid of it.  (Only do it if this isn't an update.  It won't get
 * printed anyway, and it's likely to overflow the overlay widget's
 * text space.) 
 */
	if (! update)
	{
		dc_GetTime (dc, nsamp - 1, &zt);
		ot_AddStatusLine (comp, pname, 
			tparams->tp_Mono ? "" : tparams->tp_CCField, &zt);
	}
	dc_DestroyDC (dc);
}






static void
tr_DrawArrow (d, dc, sample, wi, x, y, tparams)
Drawable d;
DataChunk *dc;
WindInfo wi;
int x, y;
TrackParams *tparams;
/*
 * Add arrows to a track.
 */
{
	long timenow;
	float u, v;
	ZebTime zt;
/*
 * What is the time of this sample?
 */
	dc_GetTime (dc, sample, &zt);
	timenow = TC_ZtToSys (&zt);
/*
 * Look to see if it's time to do another arrow.
 */
	if (((timenow % tparams->tp_AInterval) == 0) || 
			((tparams->tp_Vectime + tparams->tp_AInterval)
					< timenow))
	{
	/*
	 * Yup, it is.  Get the wind values.
	 */
		u = dc_GetScalar (dc, sample, 
				tparams->tp_Fields[tparams->tp_AField]);
		v = dc_GetScalar (dc, sample, 
				tparams->tp_Fields[tparams->tp_AField + 1]);
		GetWindData (&wi, &u, &v, tparams->tp_BadValue);
	/*
	 * If they are good, plot them.
	 */
		if (u != tparams->tp_BadValue && v != tparams->tp_BadValue) 
		{
			tparams->tp_Vectime = timenow - 
				timenow % tparams->tp_AInterval;
			FixForeground (tparams->tp_ArrowColor.pixel);
			FixLWidth (tparams->tp_ALWidth);
			if (tparams->tp_Barb)
				tr_Barb (d, x, y, u, v, tparams->tp_ShaftLen);
			else
				draw_vector (Disp, d, Gcontext, x, y, u, v, 
						tparams->tp_UnitLen);
			FixLWidth (tparams->tp_LWidth);
		}
	}
}





static void
tr_AnnotTime (comp, platform, dc, d, tparams)
char	*comp, *platform;
DataChunk	*dc;
Drawable	d;
TrackParams *tparams;
/*
 * Annotate the track with times.
 */
{
	char	interval[20], label[40], icon[40], color[20], pformat[20];
	int	i, interval_sec, nsamp;
	int	x, y;
	float	label_scale, rot, fv;
	XColor	x_color;
	ZebTime	when, t;
	int	justify;
	enum { L_Time, L_Data, L_Constant, L_None } labelopt;
/*
 * Get the time interval. 
 */
	if (! pda_Search (Pd, comp, "annot-time-interval", "track", 
		interval, SYMT_STRING))
		interval_sec = 3600;
	else
		interval_sec = pc_TimeTrigger (interval);
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
 * Label to use.  May be either "none", "time", "data", or a string.
 */
	if (! pda_Search (Pd, comp, "annot-time-label", "track", 
			label, SYMT_STRING) || ! strcmp (label, "time"))
		labelopt = L_Time;
	else if (! strcmp (label, "data"))
	{
		if (tparams->tp_Mono)
		{
			msg_ELog (EF_PROBLEM,
				"Can't do data labels on mono tracks");
			labelopt = L_None;
		}
		else
		{
			if (! pda_Search (Pd, comp, "annot-time-print-format",
					"track", pformat, SYMT_STRING))
				strcpy (pformat, "%.1f");
			labelopt = L_Data;
		}
	}
	else if (! strcmp (label, "none"))
		labelopt = L_None;
	else
		labelopt = L_Constant;
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
		 * Create the label to be used with the annotation.
		 */
			char alabel[80];
			switch (labelopt)
			{
			    case L_Constant:
				strcpy (alabel, label);
				break;
			    case L_Time:
				TC_EncodeTime (&t, TC_TimeOnly, alabel);
				alabel[strlen (alabel) - 3] = '\0';
				break;
			    case L_Data:
				fv = dc_GetScalar (dc,i,tparams->tp_Fields[0]);
				if (fv != tparams->tp_BadValue)
					sprintf (alabel, pformat, fv);
				else
					strcpy (alabel, "-");
				break;
			    case L_None:
				alabel[0] = '\0';
				break;
			}
		/*
		 * Figure out where the annotation is supposed to go, and
		 * do the annotation if possible.
		 */
			if (tr_LocateAnnot (dc, i, &t, interval_sec, &x, &y, 
					    &rot, &justify))
				tr_DoTimeAnnot (d, x, y, icon, alabel, 
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
#ifdef notdef
	if ((x0 - x1 == 0) && (y1 - y0 == 0))	/* to avoid DOMAIN warnings */
		theta = 0.0;
	else
#endif
		theta = ATAN2 ( (double)(x0 - x1), (double)(y1 - y0) );
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
	sprintf (label_str, " %s ", label);
	DrawText (Graphics, d, Gcontext, x, y, label_str, rot, label_scale, 
		  justify, JustifyCenter);
}




static void
tr_AnnotTrack (comp, platform, tparams)
char *comp, *platform;
TrackParams *tparams;
/*
 * Annotate the track we have just done.
 */
{
	char tadefcolor[30], datastr[100];
	XColor tadefclr, taclr, xc;
	zbool tacmatch = FALSE;
/*
 * Read in annotation information.
 */
	if(! tr_GetParam ("global", "ta-color", NULL, tadefcolor, 
			SYMT_STRING))
		strcpy (tadefcolor, "white");
	if(! ct_GetColorByName (tadefcolor, &tadefclr))
	{
		msg_ELog (EF_PROBLEM, "Can't get default color: '%s'.", 
			tadefcolor);
		strcpy (tadefcolor, "white");
		ct_GetColorByName (tadefcolor, &tadefclr);
	}
/*
 * Do we do color matching?
 */
	tr_GetParam ("global", "ta-color-match", NULL, (char *) &tacmatch,
		SYMT_BOOL);
	if (tacmatch)
		taclr = tparams->tp_ArrowColor;
	else
		taclr = tadefclr;
/*
 * Annotate along the top.
 */
	An_TopAnnot (" ", tadefclr.pixel);
	An_TopAnnot (platform, tadefclr.pixel);
	if (! tparams->tp_Mono)
	{
		An_TopAnnot (" ", tadefclr.pixel);
		An_TopAnnot (px_FldDesc (tparams->tp_CCField), tadefclr.pixel);
	}
	An_TopAnnot (" track", tadefclr.pixel);
	if (tparams->tp_Shifted)
		An_TopAnnot (" (SHIFTED)", tadefclr.pixel);
/*
 * Annotate arrows if necessary.
 */
	if (tparams->tp_DoArrows)
	{
		An_TopAnnot (" with ",tadefclr.pixel);
		An_TopAnnot (tparams->tp_AType, taclr.pixel);
		An_TopAnnot (tparams->tp_Barb ? " barbs" : " vectors",
				taclr.pixel);
	}
	An_TopAnnot (".  ", tadefclr.pixel);
/*
 * Down the side too.
 */
	if (tparams->tp_Mono)
	{
		ct_GetColorByName (tparams->tp_MTColor, &xc);
		sprintf (datastr, "%s|%li", platform, xc.pixel);
		An_AddAnnotProc (An_ColorString, comp, datastr,
			strlen (datastr), 25, FALSE, FALSE);
	}
	else
	{
		sprintf (datastr, "%s|%s|%f|%f", tparams->tp_CCField,
				tparams->tp_CTable, tparams->tp_Center, 
				tparams->tp_Step);
		An_AddAnnotProc (An_ColorBar, comp, datastr,
				 strlen (datastr), 75, TRUE, FALSE);
	}
/*
 * Arrows/barbs.  Pulled out of the color-code branch above (jc); I don't
 * know why it was there.  Mono tracks can have arrows too.
 */
	if (tparams->tp_DoArrows)
	{
		if (tparams->tp_Barb)
		{
			sprintf (datastr, "m/s|%li|%d", taclr.pixel,
					tparams->tp_ShaftLen);
			An_AddAnnotProc (An_BarbLegend, comp, datastr,
					strlen (datastr), 100, FALSE, FALSE);
		}
		else
		{
			sprintf (datastr, "%s|%li|%f|%f|%f", "m/s", 
				taclr.pixel, 10.0, 0.0, tparams->tp_UnitLen);
			An_AddAnnotProc (An_ColorVector, comp, datastr,
				strlen (datastr), 30, FALSE, FALSE);
		}
	}
}



static void
tr_GetLocations (dc, sample, interval_sec, loc0, samp0, loc1, samp1)
DataChunk *dc;
int sample, interval_sec;
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
	ZebTime t0, t1, t;
	int i;
	int nsample;
	int timewindow = interval_sec / 2;	/* our leeway time */

	badval = tr_GetBadval (dc);
	dc_GetTime (dc, sample, &t);
	i = sample + 1;
	*samp0 = -1;
	*samp1 = -1;
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
	/*
	 * If nothing good at or close, forget it.
	 */
	if (*samp0 < 0)
		return;
	if (t.zt_Sec - t0.zt_Sec > timewindow)
	{
		*samp0 = -1;
		return;
	}

	nsample = dc_GetNSample (dc);
	i = sample;
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
	/*
	 * Verify that the second sample is not too far into the future.
	 */
	if (*samp0 >= 0 && (t1.zt_Sec - t.zt_Sec > timewindow))
		*samp0 = -1;
}




static int
tr_LocateAnnot (dc, sample, a_time, interval_sec, x, y, rot, justify)
DataChunk *dc;
int sample;		/* Current sample about where annot needs plotting */
ZebTime *a_time;	/* Time desired for annotation */
int interval_sec;	/* Time interval between annotations */
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
	tr_GetLocations (dc, sample, interval_sec, &loc0, &samp0, &loc1, 
			 &samp1);
/*
 * If both locations valid, get the times for each and interpolate between
 * them to find a location for the desired annotation time.
 */
	if (samp0 >= 0)
	{
		dc_GetTime (dc, samp0, &t0);
		prj_Project (loc0.l_lat, loc0.l_lon, &fx0, &fy0);
		if (samp1 >= 0)
			prj_Project (loc1.l_lat, loc1.l_lon, &fx1, &fy1);
	/*
	 * If the time of the given sample equals the desired annot time,
	 * we use the sample's location
	 */
		if (TC_Eq (t0, *a_time))
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
	/*
	 * If we don't have a location at the desired time nor two points to
	 * interpolate, we have to bail.
	 */
		else
		{
			return (0);
		}
		*x = XPIX (fx); *y = YPIX (fy);
	}
	else	/* we're screwed, abandon our efforts */
		return (0);
/*
 * Now we've got to determine the rotation.  If we don't have a samp1 to
 * determine the slope, return failure.
 */
	if (samp1 >= 0)
		*rot = tr_FigureRot (fx, fy, fx1, fy1, justify);
	else
	{
		return (0);
#ifdef notdef
		/* Supply a default rotation of zero */
		*rot = 0;
		*justify = JustifyLeft;
#endif
	}
	return (1);
}




static zbool
tr_CTSetup (comp, pnames, pids, tparams)
char *comp, **pnames;
PlatformId *pids;
TrackParams *tparams;
/*
 * Do the basic setup to plot aircraft tracks.
 */
{
	static char platform[PlatformListLen];
	int p;
	char tmp[80];
/*
 * Get our platform first, since that's what is of interest to us.
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return (FALSE);
	tparams->tp_NPlats = CommaParse (platform, pnames);
/*
 * Look up all the platforms
 */
	for (p = 0; p < tparams->tp_NPlats; p++)
	{
	/*
	 * Get the id
	 */
		if ((pids[p] = ds_LookupPlatform (pnames[p])) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Unknown platform '%s'", 
				  pnames[p]);
			return (FALSE);  /* XXX should just skip this one */
		}
	}
/*
 * Pull out other parameters of interest.
 */
	if (! tr_GetParam (comp, "time-period", platform, tmp, SYMT_STRING))
		tparams->tp_Period = 300;
	else if ((! strcmp (tmp, "observation")))
	  /* if time-period == "observation", set period == 0 as a flag */
		tparams->tp_Period = 0;
	else if ((tparams->tp_Period = pc_TimeTrigger (tmp)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Unparsable time-period: '%s'", tmp);
		tparams->tp_Period = 300;
	}
/*
 * Do they want us to pare things down?
 */
	if (! tr_GetParam (comp, "data-skip", platform,
			(char *) &tparams->tp_DSkip, SYMT_INT))
		tparams->tp_DSkip = 0;
/*
 * Time annotations?
 */
	if (! pda_Search (Pd, comp, "annot-time", "track", 
			  (char *) &tparams->tp_AnnotTime, SYMT_BOOL))
		tparams->tp_AnnotTime = FALSE;
/*
 * Color info.
 */
	if (! tr_GetParam (comp, "color", platform, tparams->tp_MTColor,
			SYMT_STRING))
		strcpy (tparams->tp_MTColor, "white");
/*
 * Color code field.
 */
	tparams->tp_Mono = ! (tr_GetParam (comp, "field", platform,
			tparams->tp_CCField, SYMT_STRING)
			|| tr_GetParam (comp, "color-code-field", platform,
				tparams->tp_CCField, SYMT_STRING));
/*
 * Show the location?  As arrow or icon?
 */
	tparams->tp_ShowPosition = FALSE;
	tparams->tp_PositionArrow = FALSE;
	if (tr_GetParam (comp, "show-position", platform, 
			 (char*) &tparams->tp_ShowPosition, SYMT_BOOL) &&
	    tparams->tp_ShowPosition)
	{
	/*
	 * do-position-arrow takes precedence over position-icon; if
	 * position arrow enabled, positionicon is undefined.
	 */
		if (tr_GetParam (comp, "do-position-arrow", platform, 
				 (char*) &tparams->tp_PositionArrow, 
				 SYMT_BOOL) &&
		    tparams->tp_PositionArrow)
			; /* hunky-dory */
	/*
	 * If we need an icon to show, get it from position-icon if it
	 * exists, else default to plain old icon.  The thinking is that
	 * the icon parameter often makes a dandy and convenient default for 
	 * position-icon, without so much redundancy in plot descriptions.
	 */
		else if (! tr_GetParam (comp, "position-icon", platform, 
				tparams->tp_PosIcon, SYMT_STRING) &&
			 ! tr_GetParam (comp, "icon", platform, 
				tparams->tp_PosIcon, SYMT_STRING))
		{
			tparams->tp_ShowPosition = FALSE;
			msg_ELog (EF_PROBLEM, "%s: show-position true, %s.",
					comp, "but no icon or arrow");
		}
	}
/*
 * A few should be intialized, especially for annotation use later on
 * (i.e. shifted), even if no data are plotted.
 */
	tparams->tp_LWidth = 1;
	tparams->tp_UnitLen = 0;
	tparams->tp_Vectime = 0;
	tparams->tp_BadValue = 0;
	tparams->tp_Shifted = FALSE;
}





static zbool
tr_CCSetup (comp, platform, tparams)
char *comp, *platform;
TrackParams *tparams;
/*
 * Get everything set up to color-code a track.
 */
{
	char orc[20], param1[50], param2[50], mode[32];
/*
 * Assume the worst.  We're cynical folks.
 */
	tparams->tp_Mono = TRUE;
/*
 * Get the color table.
 */
	if (! tr_GetParam (comp, "color-table", platform, tparams->tp_CTable,
			SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM,
			"No color table specified in component %s", comp);
		return (FALSE);
	}
	if (! ct_LoadTable (tparams->tp_CTable, &tparams->tp_Colors,
			&tparams->tp_NColor))
	{
		msg_ELog (EF_PROBLEM, "Unable to load color table %s",
				tparams->tp_CTable);
		return (FALSE);
	}
/*
 * Autoscaling?
 */
	if (pda_Search (Pd, comp, "scale-mode", "track", mode, SYMT_STRING))
		tparams->tp_AutoScale = strcmp (mode, "manual");
	else
		tparams->tp_AutoScale = FALSE;
/*
 * Get our color coding parameters.
 */
	if (! tparams->tp_AutoScale)
	{
		FieldId fid = F_Lookup (tparams->tp_CCField);
		char *justname = F_GetName (fid);

		sprintf (param1, "%s-center", justname);
		sprintf (param2, "%s-step", justname);
		if (! pda_ReqSearch (Pd, comp, param1, "track",
				(char *) &tparams->tp_Center, SYMT_FLOAT) ||
		    ! pda_ReqSearch (Pd, comp, param2, "track",
				    (char *) &tparams->tp_Step, SYMT_FLOAT))
			return (FALSE);
	}
/*
 * Something for completely funky colors too.  Red is probably a bad
 * choice but that's what we have for now.
 */
	if (! tr_GetParam (comp, "out-of-range-color", tparams->tp_CCField,
			orc, SYMT_STRING))
		strcpy (orc, "red");
	if (! ct_GetColorByName (orc, &tparams->tp_Outrange))
	{
		msg_ELog (EF_PROBLEM, "Bad out of range color: %s", orc);
		ct_GetColorByName ("red", &tparams->tp_Outrange);
	}

	tparams->tp_Mono = FALSE;
	return (TRUE);
}



static void
tr_GetArrowParams (comp, platform, tparams)
char *comp;
char *platform;
TrackParams *tparams;
/*
 * Get the parameters that control track arrows.
 */
{
	char param[CFG_PDPARAM_LEN];
/*
 * Misc params.
 */
	if (! tr_GetParam (comp, "arrow-scale", platform,
			(char *) &tparams->tp_ArrowScale, SYMT_FLOAT))
		tparams->tp_ArrowScale = 0.007;
	if (! tr_GetParam (comp, "arrow-line-width", platform,
			(char *) &tparams->tp_ALWidth, SYMT_INT))
		tparams->tp_ALWidth = 1;
	if (! tr_GetParam(comp, "arrow-invert", platform,
			  (char *) &tparams->tp_AInvert, SYMT_BOOL))
		tparams->tp_AInvert = FALSE;
/*
 * Maybe they really want to plot barbs?
 */
	if (tr_GetParam (comp, "arrow-style", platform, param, SYMT_STRING))
		tparams->tp_Barb = ! strcmp (param, "barb");
	else
		tparams->tp_Barb = FALSE;
	if (tparams->tp_Barb)
	{
		if (! tr_GetParam (comp, "shaft-length", platform,
				(char *) &tparams->tp_ShaftLen, SYMT_INT))
			tparams->tp_ShaftLen = 20;
	}
/*
 * Get and parse the arrow interval.
 */
	if (! tr_GetParam (comp, "arrow-interval", platform, param,
			SYMT_STRING))
		tparams->tp_AInterval = 10;
	else if ((tparams->tp_AInterval = pc_TimeTrigger (param)) == 0)
	{
		msg_ELog(EF_PROBLEM,"Unparsable arrow interval: '%s'.",
			param);
		tparams->tp_AInterval = 10;
	}
/*
 * Color information.
 */
	if (! tr_GetParam (comp, "arrow-color", platform, tparams->tp_AColor,
			SYMT_STRING))
		strcpy (tparams->tp_AColor, "white");
	if (! ct_GetColorByName (tparams->tp_AColor, &tparams->tp_ArrowColor))
	{
		msg_ELog (EF_PROBLEM, "Can't get arrow color: '%s'.",
				tparams->tp_AColor);
		strcpy (tparams->tp_AColor, "white");
		ct_GetColorByName (tparams->tp_AColor,&tparams->tp_ArrowColor);
	}
/*
 * And what are we actually plotting?
 */
	if (! tr_GetParam (comp, "arrow-type", platform, tparams->tp_AType,
			SYMT_STRING))
		strcpy (tparams->tp_AType, "wind");
}





static int
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



static DataChunk *
tr_DoNSpace (pid, fields, numfields, details, ndetail)
PlatformId	pid;
int		numfields;
FieldId		*fields;
dsDetail	*details;
int		ndetail;
/*
 * Get data from an NSpace platform, and return it in the form of a
 * DCC_Location data chunk (if numfields == 0) or a DCC_Scalar data
 * chunk (if numfields > 0).
 */
{
/*
 * Only one NSpace handler at this point
 */
	if (1)
	    return (tr_TrajNSpace (pid, fields, numfields, details, ndetail));
	else
	    return (0);
}




static DataChunk *
tr_TrajNSpace (pid, fields, numfields, details, ndetail)
PlatformId	pid;
int		numfields;
FieldId		*fields;
dsDetail	*details;
int		ndetail;
/*
 * This is a *very* crude NSpace handler for 1D trajectory data from
 * ACE1.
 */
{
	int	n_ourfields, ndims, fld, samp, ntimes;
	char	*dnames[DC_MaxDimension];
	float	*lat, *lon, *pres, *foffsets, *data;
	unsigned long	dsizes[DC_MaxDimension];
	Location	loc;
	DataChunk	*dc_ns, *dc;
	DataClass	class;
	ZebTime		dtime, samptime, *times;
	FieldId		*ourfields, latfld, lonfld, presfld, fofld;
/*
 * Find the closest time for which we have data
 */
	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, &dtime))
	{
		char tstring[32];
		TC_EncodeTime (&PlotTime, TC_Full, tstring);
		msg_ELog (EF_PROBLEM, "tr_DoNSpace: no data for '%s' at %s", 
			  ds_PlatformName (pid), tstring);
		return (NULL);
	}
/*
 * Build our field list
 */
	n_ourfields = numfields + 4;
	ourfields = (FieldId *) malloc (n_ourfields * sizeof (FieldId));
	memcpy ((void *) ourfields, (void *) fields, 
		numfields * sizeof (FieldId));
	latfld = ourfields[numfields] = F_Lookup ("f_lat");
	lonfld = ourfields[numfields+1] = F_Lookup ("f_lon");
	presfld = ourfields[numfields+2] = F_Lookup ("f_pres");
	fofld = ourfields[numfields+3] = F_Lookup ("forecast_offset");
/*
 * Get the data, which we assume comes back in 1D arrays...
 */
	dc_ns = ds_Fetch (pid, DCC_NSpace, &dtime, &dtime, ourfields, 
			  n_ourfields, details, ndetail);
/*
 * Make sure we got 1D data and that the dimension is the
 * forecast_offset NSpace coordinate variable.  If it's right, get
 * the dimension size (i.e., the length of the data arrays).
 */
	dc_NSGetField (dc_ns, ourfields[0], &ndims, dnames, dsizes, NULL);
	if (ndims != 1 || F_Declared (dnames[0]) != fofld)
	{
		msg_ELog (EF_PROBLEM, "tr_TrajNSpace: Dimension problem");
		dc_DestroyDC (dc_ns);
		free (ourfields);
		return (NULL);
	}
/*
 * Build an array of times by adding our forecast offset times
 * to the sample time from the NSpace data chunk (the initial time
 * for the trajectory)
 */
	ntimes = dsizes[0];	
	times = (ZebTime *) malloc (ntimes * sizeof (ZebTime));
	foffsets = (float *) dc_NSGetSample (dc_ns, 0, fofld, NULL);

	for (samp = 0; samp < ntimes; samp++)
	{
		times[samp] = dtime;
		times[samp].zt_Sec += (int)(foffsets[samp]) * 3600;
	}
/*
 * Create the desired class of data chunk: DCC_Location if
 * numfields == 0 or DCC_Scalar if numfields > 0.
 */
	class = (numfields == 0) ? DCC_Location : DCC_Scalar;
	
	dc = dc_CreateDC (class);
	dc->dc_Platform = pid;
	dc_SetLocAltUnits (dc, AU_mb);
	if (class == DCC_Scalar)
	{
		dc_SetScalarFields (dc, numfields, fields);
		dc_SetBadval (dc, dc_GetBadval (dc_ns));
	}
/*
 * Move in the user-requested fields
 */
	for (fld = 0; fld < numfields; fld++)
	{
		data = (float *) dc_NSGetSample (dc_ns, 0, fields[fld], NULL);
		dc_AddMultScalar (dc, times, 0, ntimes, fields[fld], 
				  (void *) data);
	}
/*
 * And the locations...
 */
	lat = (float *) dc_NSGetSample (dc_ns, 0, latfld, NULL);
	lon = (float *) dc_NSGetSample (dc_ns, 0, lonfld, NULL);
	pres = (float *) dc_NSGetSample (dc_ns, 0, presfld, NULL);

	for (samp = 0; samp < ntimes; samp++)
	{
		loc.l_lat = lat[samp];
		loc.l_lon = lon[samp];
		loc.l_alt = pres[samp];

		if (class == DCC_Location)
			dc_LocAdd (dc, times + samp, &loc);
		else
			dc_SetLoc (dc, samp, &loc);
	}

	dc_DestroyDC (dc_ns);
	free (ourfields);

	return (dc);
}    




static DataChunk *
tr_GetData (comp, update, pid, tparams, details, ndetail, begin)
char *comp;
PlatformId pid;
int ndetail, update;
dsDetail *details;
ZebTime *begin;
TrackParams *tparams;
{
	DataChunk *dc;
/*
 * Figure the begin time and fetch data.  If updating, only fetch and
 * plot data since the last plot of this track; otherwise, fetch the
 * period or the whole observation depending upon the mode.
 */
	if (update && pda_Search (Pd, comp, "data-end-time", NULL, 
			(char*) begin, SYMT_DATE))
	{
	/*
	 * Update: get data since last plot regardless of
	 * obesrvation or period mode.  If we don't know when
	 * we last plotted data, skip this and fetch a full
	 * swath of data.
	 */
		msg_ELog (EF_DEBUG, "update in %s mode from %d to %d",
				(tparams->tp_Period) ? "period" : "obs",
				begin->zt_Sec, PlotTime.zt_Sec);
		dc = ds_Fetch (pid, tparams->tp_NField ? DCC_Scalar :
				DCC_Location, begin, &PlotTime,
				tparams->tp_Fields, tparams->tp_NField,
				details, ndetail);
	}
/*
 * If this platform delivers NSpace data, then do what we can
 * to get it in DCC_Scalar or DCC_Location form
 */
	else if (ds_PlatformDataOrg (pid) == OrgNSpace)
		dc = tr_DoNSpace (pid, tparams->tp_Fields, tparams->tp_NField,
				details, ndetail);
/*
 * Barring updates we fetch data based on period or observation
 */
	else if (tparams->tp_Period) /* time-period type plot */
	{
		*begin = PlotTime;
		begin->zt_Sec -= tparams->tp_Period;
		dc = ds_Fetch (pid, tparams->tp_NField ? DCC_Scalar :
				DCC_Location, begin, &PlotTime,
				tparams->tp_Fields, tparams->tp_NField,
				details, ndetail);
	}
/*
 * OK, looks like they want an observation plot.  See when we might have
 * an observation for them.
 */
	else if (ds_GetObsTimes (pid, &PlotTime, begin, 1, NULL))
	{
		msg_ELog (EF_DEBUG, "global, FetchObs for %d", 	begin->zt_Sec);
		dc = ds_FetchObs (pid,	tparams->tp_NField ?
				DCC_Scalar : DCC_Location, begin,
				tparams->tp_Fields, tparams->tp_NField,
				details, ndetail);
	}
/*
 * Bummer.
 */
	else
		dc = NULL;
	return (dc);
}





static void
tr_Barb (d, x1, y1, u, v, shaftlen)
Drawable d;
int x1, y1, shaftlen;
float u, v;
/*
 * Draw a barb on the screen.
 */
{
	draw_barb (Disp, d, Gcontext, x1, y1, atan2 (-v, -u), hypot (u, v),
			shaftlen, 0);
}




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


# endif /* C_CAP_TRACKS */


