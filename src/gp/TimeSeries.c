/*
 * Time Series Plotting
 */
static char *rcsid = "$Id: TimeSeries.c,v 2.6 1992-05-27 16:34:36 kris Exp $";
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
# if C_PT_TSERIES

# include <math.h>
# include <ctype.h>
# include <time.h>
# include <X11/Intrinsic.h>
# include <ui.h> 
# include <ui_date.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include <DataChunk.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"
# include "EventQueue.h"

/*
 * General definitions
 */
# define MAXFLDS	2		/*  Maximum number of fields	*/
# define STRLEN		80		/*  Generic string length	*/
# define MAXPLTS	20		/*  Maximum number of platforms	*/
# define PARAMLEN	30		/*  Parameter string length	*/

/*
 *  Convert values between field coordinates and plot window coordinates
 */
# define CONVERT(i, min, max)	(float)((float)((i)-(min))/(float)((max)-(min)))
# define UNCONVERT(i, min, max)	(float)(((i) * ((max) - (min))) + (min))

/*
 * Plot window coordinates.
 */
static float	X0 = 0.0, X1 = 1.0, Y0 = 0.0, Y1 = 1.0;

/*
 * Pixel limits for the plot
 */
static int	Pix_left, Pix_right, Pix_bottom, Pix_top;

/*
 * Minimum and Maximum data values and time values.
 */
static float	Minval[MAXFLDS], Maxval[MAXFLDS];
static long	TMinval, TMaxval;

/*
 * Begin and End times, and the fixed PlotTime.
 */
static ZebTime	Begin, End, FixPT;

/*
 * Save the last data points plotted, used during updates.
 */
static long	Save_x[MAXPLTS * MAXFLDS];
static float	Save_y[MAXPLTS * MAXFLDS];

/*
 * Should time axis be flipped.
 */
static int	FlipTime;

/*
 * Save the sa-scale value.
 */
static float	TSScale = 0.02;

/*
 * Color array and indices
 */
static XColor	*Colors;
static int	Ncolors;
static XColor	Black;
static XColor	BackColor;

/*
 * Clip and unclip rectangles
 */
static XRectangle	Clip, Unclip;

/*
 * Forward declarations
 */
void		ts_Plot FP ((char *, int)); 
static void	ts_Annotate FP ((char *, char **, int, char **, int)); 
static void	ts_Background FP ((char *, char **, int, float *, float *)); 
static void	ts_PutData FP ((int, char *, char **, int, char **, int));
static void	ts_AnnotTime FP ((int)); 
static void	ts_SlideData FP (()); 
static void	ts_DrawLines FP ((int, int));


void
ts_Plot (c, update)
char	*c;
bool	update;
/*
 * Draw a time series plot based on the given PD component.
 */
{
	bool	ok;
	char	string[STRLEN], platforms[MAXPLTS*PARAMLEN]; 
	char	fields[MAXFLDS*PARAMLEN], ctname[PARAMLEN];
	char	*pnames[MAXPLTS*PARAMLEN], *fnames[MAXFLDS*PARAMLEN];
	char	interval[PARAMLEN], color[PARAMLEN], trigger[PARAMLEN];
	int	period, nplat, nfld, nstep, i;
	float	center[MAXFLDS], step[MAXFLDS];
/*
 * Get the platform and field from the plot description.
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fields, SYMT_STRING);
/*
 * Get the platforms and fields.
 */	
	nplat = CommaParse (platforms, pnames);
	nfld = CommaParse (fields, fnames);
	if (nfld > MAXFLDS) nfld = MAXFLDS;
/*
 * Get the plot description parameters.
 */
	ok &= pda_ReqSearch (Pd, c, "color-table", "tseries", ctname, 
		SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "time-interval", "tseries", interval, 
		SYMT_STRING);
	for (i = 0; i < nfld; i ++)
	{
		sprintf (string, "%s-center", fnames[i]);
		ok &= pda_ReqSearch (Pd, c, string, "tseries", 
			(char *) &center[i], SYMT_FLOAT);
		sprintf (string, "%s-step", fnames[i]);
		ok &= pda_ReqSearch (Pd, c, string, "tseries", 
			(char *) &step[i], SYMT_FLOAT);
	}
	ok &= pda_ReqSearch (Pd, c, "nstep", "tseries", (char *) &nstep, 
		SYMT_INT);
	if (! ok)
		return;
/*
 * Get other plot description parameters.
 */
	FlipTime = FALSE;
	pda_Search (Pd, c, "flip-time", "tseries", (char *) &FlipTime, 
		SYMT_BOOL);
	if (! pda_Search (Pd, c, "axis-color", "tseries", color, SYMT_STRING))
		strcpy(color, "gray60");
	if (! ct_GetColorByName (color, &BackColor))
		ct_GetColorByName ("gray", &BackColor);
	if (! pda_Search (Pd, c, "trigger", "tseries", trigger, SYMT_STRING))
		strcpy(color, "gray");
/*
 * Get the color table.
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);
	ct_GetColorByName ("black", &Black);
/*
 * Set pixel limits.
 */
	Xlo = X0;
	Xhi = X1;
	Ylo = -0.04;
	Yhi = Y1;
	
	Pix_left = XPIX (X0);
	Pix_right = XPIX (X1);
	Pix_bottom = YPIX (Y0);
	Pix_top = YPIX (Y1);
/*
 * Clip and unclip rectangles
 */
	Clip.x = Pix_left + 1;
	Clip.y = Pix_top + 1;
	Clip.width = Pix_right - Pix_left - 1;
	Clip.height = Pix_bottom - Pix_top - 1 ;

	Unclip.x = 0;
	Unclip.y = 0;
	Unclip.width = GWWidth (Graphics);
	Unclip.height = GWHeight (Graphics);
/*
 * Figure the begin time.
 */
	if ((PlotMode == RealTime) && (! update))
		px_FixPlotTime (&FixPT);
	else
		FixPT = PlotTime;
	Begin = FixPT;
	if((period = pc_TimeTrigger (interval)) == 0)	
		period = 300;
	Begin.zt_Sec -= period;
/*
 * If we're updating then slide the old data over to make room for the new.
 */
	if (update) 
		ts_SlideData ();
/*
 * Get Minval and Maxval, TMinval and TMaxval.
 */
	if (! update)
	{
		if ((nstep & 0x1) == 0)
			nstep--;
		for (i = 0; i < nfld; i++)
		{
			Minval[i] = center[i] - ((nstep - 1) / 2) * step[i];
			Maxval[i] = center[i] + ((nstep - 1) / 2) * step[i];
		}
	}
	if (! FlipTime)
	{
		TMinval = Begin.zt_Sec;
		TMaxval = FixPT.zt_Sec;
	}
	else
	{
		TMinval = FixPT.zt_Sec;
		TMaxval = Begin.zt_Sec;
	}
/*
 * Annotate the time on the horizontal axis of the background.
 */
	ts_AnnotTime (update);
/*
 * If we're updating reset the begin time.
 */
	if (update) 
		Begin = End;
/*
 * Draw lines on the background.
 */
	ts_DrawLines (update, nstep);
/*
 * Annotate
 */
	if (! update) 
		ts_Annotate(c, pnames, nplat, fnames, nfld);
/*
 * Draw the background
 */
	if (! update)
		ts_Background (c, fnames, nfld, center, step);
/*
 * Draw the data 
 */
	ts_PutData (update, c, pnames, nplat, fnames, nfld);
/*
 * Set the end time.
 */
	End = FixPT;
}


static void
ts_Annotate (comp, platforms, nplat, fields, nfld)
char	*comp, **platforms, **fields;
int	nplat, nfld;
/*
 *  Put the annotation in the window.
 */
{
	char	string[STRLEN], tacolor[PARAMLEN]; 
	bool	tacmatch; 
	int	i, j, top, bottom, left, right;
	float	sascale;
	XColor	taclr;
/*
 *  Get annotation plot description parameters.
 */
	if(! pd_Retrieve (Pd, "global", "ta-color", tacolor, SYMT_STRING))
		strcpy (tacolor, "white");
	if(! ct_GetColorByName (tacolor, &taclr))
		ct_GetColorByName ("white", &taclr);
	tacmatch = FALSE;
	pd_Retrieve (Pd, "global", "ta-color-match", (char *) &tacmatch, 
		SYMT_BOOL);
	if(! pda_Search (Pd, comp, "sa-scale", "tseries", (char *) &sascale, 
		SYMT_FLOAT))
		sascale = 0.02;
/*
 *  Top Annotation
 */
	An_TopAnnot ("Time series plot of ", taclr.pixel);
	for (i = 0; i < nfld; i++)
	{
		An_TopAnnot (px_FldDesc(comp, fields[i]), taclr.pixel);
		if (i < (nfld - 1)) An_TopAnnot (", ", taclr.pixel);
	}
	An_TopAnnot (" for platforms: ", taclr.pixel);
	An_AnnotLimits (&top, &bottom, &left, &right);
	left += 25;
	for (i = 0; i < nplat; i++)
	{
		An_TopAnnot (platforms[i], taclr.pixel);
		if (i < (nplat - 1)) An_TopAnnot (", ", taclr.pixel);
	}
	An_TopAnnot (".", taclr.pixel);
/*
 *  Side Annotation
 */
	for (i = 0; i < nfld; i++)
	{
		for (j = 0; j < nplat; j++)
		{
			if (ds_LookupPlatform (platforms[j]) != BadPlatform)
			{
				XSetForeground (XtDisplay (Graphics), Gcontext, 
					Colors[i * nplat + j].pixel);
				sprintf (string, "%s-%s", fields[i], 
					platforms[j]);
				DrawText (Graphics, GWFrame (Graphics), 
					Gcontext, left, top, string, 0.0, 
					sascale, JustifyLeft, JustifyTop);
				top += sascale * GWHeight (Graphics);
			}
		}
	}
/*
 * Save sa-scale in a global.
 */
	TSScale = sascale;
}


static void
ts_AnnotTime (update)
bool	update;
/*
 *  Annotate the begin and end time on the horizontal axis of the plot.
 */
{
	Display		*disp = XtDisplay (Graphics);
	Drawable	d = GWFrame (Graphics);
	char		string[STRLEN];
/*
 * If this is an update blank out the old times.
 */
	if (update)
	{
		XSetForeground (disp, Gcontext, Black.pixel);
		XFillRectangle (disp, d, Gcontext, Pix_left, Pix_bottom + 1, 
			Pix_right - Pix_left, 
			(int) (TSScale * (float) GWHeight (Graphics)));
	}
	XSetForeground (disp, Gcontext, BackColor.pixel);
/*
 * Put time on the left side.
 */
	if (! FlipTime) 
		TC_EncodeTime (&Begin, TC_TimeOnly, string);
	else
		TC_EncodeTime (&FixPT, TC_TimeOnly, string);
	DrawText (Graphics, d, Gcontext, XPIX (X0), Pix_bottom, string, 
		0.0, TSScale, JustifyLeft, JustifyTop);
/*
 * Put time on the right side.
 */
	if (! FlipTime) 
		TC_EncodeTime (&FixPT, TC_TimeOnly, string);
	else
		TC_EncodeTime (&Begin, TC_TimeOnly, string);
	DrawText (Graphics, d, Gcontext, XPIX (X1), Pix_bottom, string, 
		0.0, TSScale, JustifyRight, JustifyTop);
}


static void
ts_Background (comp, fields, nfld, center, step)
char	*comp, **fields;
int	nfld;
float 	*center, *step;
/*
 * Draw the background.
 */
{
	float		tick, tickinc;
	char		string[STRLEN];	
	int		dolabel;
	XPoint		pts[5];
	Display		*disp = XtDisplay (Graphics);
	Drawable	d = GWFrame(Graphics);
	
	XSetForeground (disp, Gcontext, BackColor.pixel);
/*
 * Draw a box.
 */
	pts[0].x = Pix_left;	pts[0].y = Pix_bottom;
	pts[1].x = Pix_right;	pts[1].y = Pix_bottom;
	pts[2].x = Pix_right;	pts[2].y = Pix_top;
	pts[3].x = Pix_left;	pts[3].y = Pix_top;
	pts[4].x = Pix_left;	pts[4].y = Pix_bottom;

	XDrawLines (disp, d, Gcontext, pts, 5, CoordModeOrigin);
/*
 * Label the horizontal axis.
 */
	DrawText (Graphics, d, Gcontext, XPIX (0.5), YPIX (-0.04), "Time", 
		0.0, TSScale, JustifyCenter, JustifyTop);
/*
 * Label the vertical axis (left). 
 */
	DrawText (Graphics, d, Gcontext, XPIX (-0.04), YPIX (0.5), 
		px_FldDesc(comp, fields[0]), 90.0, TSScale, JustifyCenter, 
		JustifyBottom);
/*
 * Draw ticks on the vertical axis (left).
 */
	tickinc = CONVERT (step[0], 0.0, Maxval[0] - Minval[0]);
	dolabel = TRUE;

	for (tick = Y0; tick <= Y1; tick += tickinc)
	{
	/*
	 * Draw the tick
	 */
		pts[0].y = pts[1].y = YPIX (tick);
		pts[0].x = Pix_left;
		pts[1].x = XPIX (-0.025);
		XDrawLines (disp, d, Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{
			sprintf (string, "%d", (int) UNCONVERT(tick, Minval[0],
				Maxval[0]));
			DrawText (Graphics, d, Gcontext, XPIX (-0.005), 
				YPIX (tick), string, 0.0, TSScale, 
				JustifyRight, JustifyBottom);
		}
		dolabel = ! dolabel;
	}
/*
 * If there is only one field then don't label the right side.
 */
	if (nfld <= 1) return;
/*
 * Label the vertical axis (right). 
 */
	DrawText (Graphics, d, Gcontext, XPIX (1.04), YPIX (0.5), 
		px_FldDesc(comp, fields[1]), -90.0, TSScale, 
		JustifyCenter, JustifyBottom);
/*
 * Draw ticks on the vertical axis (right).
 */
	tickinc = CONVERT (step[1], 0.0, Maxval[1] - Minval[1]);
	dolabel = TRUE;

	for (tick = Y0; tick <= Y1; tick += tickinc)
	{
	/*
	 * Draw the tick
	 */
		pts[0].y = pts[1].y = YPIX (tick);
		pts[0].x = XPIX (1.025);
		pts[1].x = Pix_right;
		XDrawLines (disp, d, Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{
			sprintf (string, "%d", (int) UNCONVERT(tick, Minval[1],
				Maxval[1]));
			DrawText (Graphics, d, Gcontext, XPIX (1.005), 
				YPIX (tick), string, 0.0, TSScale, JustifyLeft, 
				JustifyBottom);
		}
		dolabel = ! dolabel;
	}
}


static void
ts_PutData (update, comp, platforms, nplat, fields, nfld)
bool	update;
char	*comp, **platforms, **fields;
int	nplat, nfld;
/*
 *  Plot the data.
 */
{
	int		x0, y0, x1, y1, plat, i, j, linewidth, dskip, npt = 0;
	int		numpts;
	long		t;
	float		fx, fy, badvalue, v;
	Display		*disp = XtDisplay (Graphics);
	Drawable	d = GWFrame (Graphics);
	PlatformId	pid;
	DataChunk	*dc;
	ZebTime		when;
	FieldId		fieldlist[MAXFLDS];
/*
 * Get the plot description parameters for plotting data.
 */	
	if (! pda_Search (Pd, comp, "line-width", "tseries", 
			(char *) &linewidth, SYMT_INT))
		linewidth = 0;
	if (! pda_ReqSearch (Pd, comp, "data-skip", "tseries", (char *) &dskip, 
			SYMT_INT))
		dskip = 0;
/*
 * Turn on clipping.
 */
	XSetClipRectangles (disp, Gcontext, 0, 0, &Clip, 1, Unsorted);
/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Get the ID of this platform
	 */
		if ((pid = ds_LookupPlatform (platforms[plat])) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Bad platform '%s'", 
				platforms[plat]);
			continue;
		}
	/*
	 * Get field id's.
	 */
		for (i = 0; i < nfld; i++)
			fieldlist[i] = F_Lookup (fields[i]);
	/*
	 * Get the data.
	 */
		if (! (dc = ds_Fetch (pid, DCC_Scalar, &Begin, &FixPT,
			fieldlist, nfld, NULL, 0)))
		{
			msg_ELog (EF_PROBLEM, "No %s data available", 
				platforms[plat]);
			continue;
		}
	/*
	 * Now work through the data.
	 */
		for (i = 0; i < nfld; i++)
		{
			XSetForeground (disp, Gcontext, 
				Colors[i * nplat + plat].pixel);
			if (update)
			{
				fx = CONVERT (Save_x[plat * nfld + i],
					TMinval, TMaxval);
				fy = CONVERT (Save_y[plat * nfld + i],
					Minval[i], Maxval[i]);
			}
			else
			{
				fx = CONVERT (Begin.zt_Sec, TMinval, 
					TMaxval);
				fy = CONVERT (dc_GetScalar (dc, 0, 
					fieldlist[i]), Minval[i], 
					Maxval[i]); 
			}
			x0 = XPIX (fx); 
			y0 = YPIX (fy);
			npt = 0;
		/* 
		 * Draw the data.
		 */
			numpts = dc_GetNSample (dc);
			badvalue = dc_GetBadval (dc);
			for (j = 1; j < numpts; j++)
			{
			/*
			 * Get the value of the current data point.
			 */
				v = dc_GetScalar (dc, j, fieldlist[i]);
			/*
			 * If its time to skip or the data is bad, continue.
			 */
				if (dskip && (npt++ % dskip) != 0)
					continue;
				if (v == badvalue)
					continue;
			/*
			 * Save the data in case this is the last point.
			 */
				dc_GetTime (dc, j, &when);
				t = TC_ZtToSys (&when);
				Save_x[plat * nfld + i] = t;
				Save_y[plat * nfld + i] = v;
			/*
			 * Get the point to draw and draw them.
			 */
				fx = CONVERT (t, TMinval, TMaxval);
				fy = CONVERT (v, Minval[i], Maxval[i]); 
				x1 = XPIX (fx); y1 = YPIX (fy);
				XDrawLine (disp, d, Gcontext, x0, y0, x1, y1); 
				x0 = x1; y0 = y1;
			}
		}
	/*
	 * Free the data chunk.
	 */
		dc_DestroyDC (dc);
	}
/*
 * Turn off clipping.
 */
	XSetClipRectangles (disp, Gcontext, 0, 0, &Unclip, 1, Unsorted);
}


static void
ts_SlideData()
/*
 * On an update slide the old data down to make room for the new data.
 */
{
	long	beginsec;
	Pixmap	temp;
	Display	*disp = XtDisplay (Graphics);
/*
 * Create temporary pixmap.
 */
	temp = XCreatePixmap (disp, XtWindow (Graphics), GWWidth (Graphics),
		GWHeight (Graphics), GWDepth (Graphics));
/*
 * Turn on clipping.
 */
	XSetClipRectangles (disp, Gcontext, 0, 0, &Clip, 1, Unsorted);
/*
 * Copy the data.
 */
	XCopyArea (disp, GWFrame (Graphics), temp, Gcontext, Pix_left, 
		Pix_top, Pix_right - Pix_left, Pix_bottom - Pix_top, 
		Pix_left, Pix_top);
/*
 * Clear the area.
 */
	XSetForeground (disp, Gcontext, Black.pixel);
	XFillRectangle (disp, GWFrame (Graphics), Gcontext, Pix_left, Pix_top, 
		Pix_right - Pix_left, Pix_bottom - Pix_top);
/*
 * Figure the new data position.
 */
	beginsec = Begin.zt_Sec;	
/*
 * Put the data back in its new position.
 */
	if (! FlipTime)
		XCopyArea (disp, temp, GWFrame (Graphics), Gcontext, Pix_left, 
			Pix_top, Pix_right - Pix_left, Pix_bottom - Pix_top, 
			2 * Pix_left - XPIX (CONVERT (beginsec, TMinval, 
			TMaxval)), Pix_top);
	else
		XCopyArea (disp, temp, GWFrame (Graphics), Gcontext, Pix_left 
			+ 1, Pix_top, Pix_right - Pix_left - 1, 
			Pix_bottom - Pix_top, 
			Pix_left + Pix_right - XPIX(CONVERT(beginsec, TMinval, 
			TMaxval)), Pix_top);
/*
 * Turn off clipping.
 */
	XSetClipRectangles (disp, Gcontext, 0, 0, &Unclip, 1, Unsorted);
	XFreePixmap (disp, temp);
}



static void
ts_DrawLines (update, nstep)
bool	update;
int	nstep;
/* 
 * Draw the lines in the background of the plot.
 */
{
	Display	*disp = XtDisplay (Graphics);
	Drawable	d = GWFrame (Graphics);
	float	begin_x, tick, tickinc, ll;
	long	llinc;
	XPoint	pts[2];
	
	static 	ZebTime	lastline;

	XSetForeground (disp, Gcontext, BackColor.pixel);
	XSetLineAttributes (disp, Gcontext, 1, LineOnOffDash, CapButt, 
		JoinMiter);
	if (nstep <= 1) 
		nstep = 2;
	tickinc = (Y1 - Y0) / (nstep - 1);
	begin_x = CONVERT (Begin.zt_Sec, TMinval, TMaxval);
/*
 * Horizontal lines.
 */
	pts[0].x = XPIX (begin_x);
	if (! FlipTime)
		pts[1].x = Pix_right;
	else
		pts[1].x = Pix_left;
	for (tick = Y0; tick < Y1; tick += tickinc)
	{
		pts[0].y = pts[1].y = YPIX (tick);
		XDrawLines (disp, d, Gcontext, pts, 2, CoordModeOrigin);
	}
/* 
 * Vertical lines.
 */
	if (! update)
		lastline = Begin; 
	ll = CONVERT (lastline.zt_Sec, TMinval, TMaxval);
	pts[0].y = Pix_bottom;
	pts[1].y = Pix_top;
	if (! FlipTime)
	{
		for (tick = begin_x; tick <= X1; tick += tickinc)
			if ((tick >= (ll + tickinc - 0.00001)) &&
				(ll + tickinc < X1))
			{
				ll += tickinc;
				pts[0].x = pts[1].x = XPIX (ll);
				XDrawLines (disp, d, Gcontext, pts, 2, 
					CoordModeOrigin);
				llinc = (long) UNCONVERT (tickinc, 0.0, 
					TMaxval - TMinval);
				lastline.zt_Sec += llinc;
			}
	}
	else
	{
		for (tick = begin_x; tick >= X0; tick -= tickinc)
			if ((tick <= (ll - tickinc + 0.00001)) &&
				(ll - tickinc > X0))
			{
				ll -= tickinc;
				pts[0].x = pts[1].x = XPIX (ll);
				XDrawLines (disp, d, Gcontext, pts, 2, 
					CoordModeOrigin);
				llinc = (long) UNCONVERT (tickinc, 0.0, 
					TMinval - TMaxval);
				lastline.zt_Sec+= llinc;
			}
	}
	XSetLineAttributes (disp, Gcontext, 1, LineSolid, CapButt, 
		JoinMiter);
}




# endif  /* C_PT_TSERIES */
