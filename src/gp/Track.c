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
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include <DataStore.h>
# include "../include/GraphicsW.h"
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"
MAKE_RCSID ("$Id: Track.c,v 2.19 1993-01-19 22:48:03 kris Exp $")

# define ARROWANG .2618 /* PI/12 */


/*
 * Forwards.
 */
static bool tr_CCSetup FP((char *, char *, char *, char *, XColor **,
		int *, float *, float *, XColor *, float *, float *));
static void tr_GetArrowParams FP((char *, char *, float *, int *, bool *,
		int *, char *, XColor *, char *, char *, char *));
static bool tr_CTSetup FP((char *, char *, PlatformId *, int *, int *,
		char *, bool *, char *, bool *, char *));
static void tr_AnnotTrack FP((char *, char *, char *, int, char *, char *,
		char *, double, double, double, char *, bool));
static void tr_AnnotTime FP((char *, char *, DataChunk *, Drawable));
static void tr_DoTimeAnnot FP((Drawable, int, int, char *, char *, double, 
		XColor, ZebTime, double));
static float tr_FigureRot FP ((double, double, double, double));

# define BADVAL -32768


void
tr_CAPTrack (comp, update)
char *comp;
bool update;
{
	char platform[30], ccfield[30], positionicon[40];
	char mtcolor[20], ctable[30], a_color[30];
	char a_xfield[30], a_yfield[30], a_type[30];
	int period, x0, y0, x1, y1, nc, lwidth, pid, index;
	int dskip = 0, npt = 0, i, a_int, numfields = 0, afield;
	int a_lwidth, nfld, nsamp;
	bool arrow, showposition, annot_time;
	long timenow, vectime = 0;
	bool mono, shifted, a_invert;
	ZebTime begin, zt;
	float *data, fx, fy, base, incr, a_scale, *a_xdata, *a_ydata;
	float unitlen, center, step;
	Drawable d;
	XColor xc, *colors, outrange, a_clr;
	DataChunk *dc;
	Location loc;
	FieldId fields[5];
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
				&nc, &base, &incr, &outrange, &center, &step);
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
	if(arrow)
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
 * Figure the begin time.
 */
	begin = PlotTime;
	begin.zt_Sec -= period;
/*
 * Get the data.
 */
	dc = ds_Fetch (pid, numfields ? DCC_Scalar : DCC_Location, &begin,
			&PlotTime, fields, numfields, 0, 0);
	if (! dc)
	{
		msg_ELog (EF_INFO, "No %s data available", platform);
		return;
	}
	shifted = ApplySpatialOffset (dc, comp, &PlotTime);
	nsamp = dc_GetNSample (dc);
/*
 * Convert the first points.
 */
	dc_GetLoc (dc, 0, &loc);
	cvt_ToXY (loc.l_lat, loc.l_lon, &fx, &fy);
	x0 = XPIX (fx); y0 = YPIX (fy);
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
	for (i = 1; i < nsamp; i++)
	{
	/*
	 * Do skipping if requested.
	 */
		if (dskip && (npt++ % dskip) != 0)
			continue;
	/*
	 * Locate this point.
	 */
		dc_GetLoc (dc, i, &loc);
		cvt_ToXY (loc.l_lat, loc.l_lon, &fx, &fy);
		x1 = XPIX (fx); y1 = YPIX (fy);
	/*
	 * Draw arrows if necessary.
	 */
		if(arrow)
		{
			dc_GetTime (dc, i, &zt);
			timenow = TC_ZtToSys (&zt);
			if(((timenow % a_int) == 0) || 
			   ((vectime + a_int) < timenow))
			{
				vectime = timenow - timenow % a_int;
				FixForeground (a_clr.pixel);
				FixLWidth (a_lwidth);
				draw_vector (Disp, d, Gcontext, x0, y0, 
					dc_GetScalar (dc, i-1, fields[afield]),
					dc_GetScalar(dc, i-1,fields[afield+1]),
					unitlen);
				FixLWidth (lwidth);
			}
		}
	/*
	 * Color code if necessary.
	 */
	 	if (mono)
			FixForeground (xc.pixel);
		else
		{
			index = (dc_GetScalar (dc, i, fields[0]) - base)/incr;
			FixForeground ((index >= 0 && index < nc) ?
				colors[index].pixel : outrange.pixel);
		}
	/*
	 * Finally draw the line.
	 */
		XDrawLine (Disp, d, Gcontext, x0, y0, x1, y1); 
		x0 = x1; y0 = y1;
	}
/*
 * If this isn't an update, indicate which end of the track is the front.
 */
	if ((! update) && showposition)
		if (mono) ov_PositionIcon (positionicon, x0, y0, xc.pixel);
		else ov_PositionIcon (positionicon, x0, y0, (index >= 0 &&
			index < nc) ? colors[index].pixel : outrange.pixel);
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
		lw_TimeStatus (comp, platform, &zt);
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
	float	fx, fy, fx0, fy0, fx1, fy1;
	XColor	x_color;
	ZebTime	when, t, t0, t1;
	Location	loc;
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
		if (when.zt_Sec == t.zt_Sec)
		{
		/*
		 * Figure out where the annotation is supposed to go. 
		 */
			dc_GetLoc (dc, i, &loc);
			cvt_ToXY (loc.l_lat, loc.l_lon, &fx, &fy);
			x = XPIX (fx); y = YPIX (fy);
		/*
		 * Figure a rotation for the text.
		 */
			dc_GetLoc (dc, i + 1, &loc);
			cvt_ToXY (loc.l_lat, loc.l_lon, &fx1, &fy1);
			rot = tr_FigureRot (fx, fy, fx1, fy1);
		/*
		 * Do the annotation.
		 */
			tr_DoTimeAnnot (d, x, y, icon, label, label_scale,
				x_color, t, rot);
		/*
		 * Increment the time.
		 */
			t.zt_Sec -= interval_sec;
		} 
		else if (when.zt_Sec < t.zt_Sec)
		{
		/*
		 * Figure out where to put the annotation.
		 */
			dc_GetLoc (dc, i, &loc);
			cvt_ToXY (loc.l_lat, loc.l_lon, &fx0, &fy0);
			t0.zt_Sec = when.zt_Sec;

			dc_GetLoc (dc, i + 1, &loc);
			cvt_ToXY (loc.l_lat, loc.l_lon, &fx1, &fy1);
			dc_GetTime (dc, i + 1, &t1);

			fx = fx0 + (fx1 - fx0) * (t.zt_Sec - t0.zt_Sec) / 
				(t1.zt_Sec - t0.zt_Sec);

			fy = fy0 + (fy1 - fy0) * (t.zt_Sec - t0.zt_Sec) / 
				(t1.zt_Sec - t0.zt_Sec);

			x = XPIX (fx); y = YPIX (fy);
		/*
		 * Firgure a rotation for the text.
		 */
			rot = tr_FigureRot (fx, fy, fx1, fy1);
		/*
		 * Do the annotation.
		 */
			tr_DoTimeAnnot (d, x, y, icon, label, label_scale,
				x_color, t, rot);
		/*
		 * Increment the time.
		 */
			t.zt_Sec -= interval_sec;
		}
	}

}



static float
tr_FigureRot (x0, y0, x1, y1)
float	x0, y0, x1, y1;
/*
 * Figure a rotation factor (in degrees) which is perpendicular to the 
 * line defined by (x0, y0) and (x1, y1).
 */
{
	float	sub_x = (x1 - x0), sub_y = (y1 - y0);
	float	degrees;
	double	radians, temp;

	temp = (double) (sub_x * sub_x + sub_y * sub_y);
	if (temp == 0.0) temp = 1.0;
	radians = asin ((double) sub_y / sqrt (temp));
	degrees = (float) radians * 180.0 / M_PI;
	if (degrees >= 0.0) degrees -= 90.0;
	else degrees += 90.0;
	if (degrees < 0.0) degrees += 360.0;
	return (degrees); 
}



static void
tr_DoTimeAnnot (d, x, y, icon, label, label_scale, x_color, t, rot)
Drawable	d;
int		x, y;
char		*icon, *label;
float		label_scale, rot;
XColor		x_color;
ZebTime		t;
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
		TC_EncodeTime (&t, TC_TimeOnly, label_str);
	/*
	 * Get rid of seconds.
	 */
		label_str[strlen (label_str) - 3] = '\0';
	}
	else if (strcmp (label, "none") == 0)
		return;
	else 
		strcpy (label_str, label);
	DrawText (Graphics, d, Gcontext, x, y, label_str, rot, label_scale, 
		JustifyLeft, JustifyCenter);
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
	XColor tadefclr, taclr;
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
		An_TopAnnot (px_FldDesc (comp, ccfield), 
			tadefclr.pixel);
	}
	An_TopAnnot (" track", tadefclr.pixel);
	if (shifted)
		An_TopAnnot (" (SHIFTED)", tadefclr.pixel);
/*
 * Annotate arrows if necessary.
 */
	if(arrow)
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
		sprintf (datastr, "%s %s", platform, mtcolor);
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
			sprintf (datastr, "%s %s %f %f %f", "10m/sec", 
				a_color, 10.0, 0.0, unitlen);
			An_AddAnnotProc (An_ColorVector, comp, datastr,
				strlen (datastr), 25, FALSE, FALSE);
		}
	}
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
	else if ((*period = pc_TimeTrigger (tmp)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Unparsable time-period: '%s'", tmp);
		*period = 300;
	}
/*
 * Do they want us to pare things down?
 */
	if (! tr_GetParam (comp, "data-skip", platform, (char *) &dskip,
			SYMT_INT))
		dskip = 0;
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
	center, step)
char *comp, *platform, *ccfield, *ctable;
XColor **colors, *outrange;
int *nc;
float *base, *incr, *center, *step;
/*
 * Get everything set up to color-code a track.
 */
{
	char orc[20], param1[50], param2[50];
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
 * Get our color coding parameters.
 */
	sprintf (param1, "%s-center", ccfield);
	sprintf (param2, "%s-step", ccfield);
	if (! pda_ReqSearch (Pd, comp, param1, "track", (char *) center, 
			SYMT_FLOAT) ||
	    ! pda_ReqSearch (Pd, comp, param2, "track", (char *) step,
	    	SYMT_FLOAT))
		return (FALSE);
	if (! tr_GetParam (comp, "out-of-range-color", ccfield, orc,
		SYMT_STRING))
		strcpy (orc, "red");
	if (! ct_GetColorByName (orc, outrange))
	{
		msg_ELog (EF_PROBLEM, "Bad out of range color: %s", orc);
		ct_GetColorByName ("red", outrange); /* assume this works */
	}
/*
 * Fix up the parameters to make coding a little easier.
 */
	if ((*nc & 0x1) == 0)
		(*nc)--;
	*base = *center - (*nc/2)*(*step) - *step/2;
	*incr = *step;

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
		*a_int = 30;
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



# ifdef notdef
/*
 * This routine has hopefully been replaced by the generic
 * draw_vector ().
 */
tr_DrawVector(x, y, u, v, unit, W, D, Gcontext)
int x, y;
float u, v, unit;
Display *W;
Drawable D;
GC Gcontext;
/*
 *  Draw a vector along a track.
 */
{
	float dx, dy;
	int xend, yend;
	float veclen, vecang, ang;

/*
 *  Draw the shaft of the vector.
 */
	dx = u * unit;
	dy = -v * unit;

	xend = (int)(x + dx + 0.5);
	yend = (int)(y + dy + 0.5);

	XDrawLine(W, D, Gcontext, x, y, xend, yend);
/*
 *  If the vector has any length, put on the arrow head.
 */
	if(dx != 0 || dy !=0)
	{
		vecang = atan2(v,u);
		veclen = hypot(u,v);

		ang = vecang + ARROWANG;
		dx = 0.4 * veclen * unit * cos(ang);
		dy = -0.4 * veclen * unit * sin(ang);

		XDrawLine(W, D, Gcontext, xend, yend,
			(int)(xend - dx), (int)(yend - dy));

		ang = vecang - ARROWANG;
		dx = 0.4 * veclen * unit * cos(ang);
		dy = -0.4 * veclen * unit * sin(ang);

		XDrawLine(W, D, Gcontext, xend, yend,
			(int)(xend - dx), (int)(yend - dy));
	}
}
# endif
