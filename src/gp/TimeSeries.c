/*
 * Time Series Plotting
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
# if C_PT_TSERIES

# include <math.h>
# include <ctype.h>
# include <time.h>
# include <unistd.h>

# include <X11/Intrinsic.h>

# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include <GraphicsW.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"
# include "EventQueue.h"

RCSID("$Id: TimeSeries.c,v 2.25 2001-04-20 08:26:27 granger Exp $")

/*
 * General definitions
 */
# define MAXFLDS	2		/*  Maximum number of fields	*/
# define STRLEN		80		/*  Generic string length	*/
# define MAXPLTS	MaxPlatforms	/*  Maximum number of platforms	*/
# define PARAMLEN	30		/*  Parameter string length	*/

/*
 * Pixel limits for the plot
 */
static int	Pix_left, Pix_right, Pix_bottom, Pix_top;

/*
 * Minimum and Maximum data values and time values.
 */
static float	Minval[MAXFLDS], Maxval[MAXFLDS];

/*
 * Begin and End times of the whole plot, and the time from which we're
 * updating
 */
static ZebTime	Begin, End, Update_begin;

/*
 * Save the last data points plotted, used during updates.
 */
static long	Save_t[MAXPLTS * MAXFLDS];
static float	Save_v[MAXPLTS * MAXFLDS];

/*
 * Should time axis be flipped.
 */
static zbool	FlipTime;

/*
 * Update plot?
 */
static zbool	Update = FALSE;

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
static void	ts_PutData FP ((char *, char **, int, char **, int));
static void	ts_AnnotTime FP ((void)); 
static void	ts_SlideData FP (()); 
static void	ts_DrawLines FP ((int));
static void	ts_TimeTick FP ((ZebTime *, ZebTime *, ZebTime *, long *));


void
ts_Plot (c, update)
char	*c;
zbool	update;
/*
 * Draw a time series plot based on the given PD component.
 */
{
	zbool	ok;
	char	string[STRLEN], platforms[PlatformListLen]; 
	char	fields[MAXFLDS*PARAMLEN], ctname[PARAMLEN];
	char	*pnames[MaxPlatforms], *fnames[MAXFLDS];
	char	interval[PARAMLEN], color[PARAMLEN], trigger[PARAMLEN];
	int	period, nplat, nfld, nstep, i;
	float	center[MAXFLDS], step[MAXFLDS];
	ZebTime	prev_begin, prev_end;
/*
 * Set the update status
 */
	Update = update;
/*
 * Get the platform and field from the plot description.
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fields, SYMT_STRING);
/*
 * Get the platforms and fields.
 */
	if ((nplat = CommaParse (platforms, pnames)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Empty 'platform' parameter");
		ok = FALSE;
	}

	if ((nfld = ParseFieldList (fields, fnames)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Empty 'field' parameter");
		ok = FALSE;
	}
	else if (nfld > MAXFLDS)
		nfld = MAXFLDS;
/*
 * Get the plot description parameters.
 */
	ok &= pda_ReqSearch (Pd, c, "color-table", "tseries", ctname, 
		SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "time-interval", "tseries", interval, 
		SYMT_STRING);
	for (i = 0; i < nfld; i ++)
	{
		FieldId fid = F_Lookup (fnames[i]);
		char *justname = F_GetName (fid);

		sprintf (string, "%s-center", justname);
		ok &= pda_ReqSearch (Pd, c, string, "tseries", 
			(char *) &center[i], SYMT_FLOAT);
		sprintf (string, "%s-step", justname);
		ok &= pda_ReqSearch (Pd, c, string, "tseries", 
			(char *) &step[i], SYMT_FLOAT);
	}
	ok &= pda_ReqSearch (Pd, c, "nstep", "tseries", (char *) &nstep, 
		SYMT_INT);
	if (! ok)
		return;
/*
 * Make sure we use an odd number of steps
 */
	if ((nstep & 0x1) == 0)
		nstep--;
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
 * Save the begin and end of the previous plot
 */
	prev_begin = Begin;
	prev_end = End;
/*
 * Figure the begin time.
 */
	End = PlotTime;
	if ((PlotMode == RealTime) && (! Update))
		px_FixPlotTime (&End);

	Begin = End;
	if((period = pc_TimeTrigger (interval)) == 0)	
		period = 300;
	Begin.zt_Sec -= period;
/*
 * Build the Minval and Maxval arrays
 */

	if (! Update)
	{
		for (i = 0; i < nfld; i++)
		{
			Minval[i] = center[i] - ((nstep - 1) / 2) * step[i];
			Maxval[i] = center[i] + ((nstep - 1) / 2) * step[i];
		}
	}
/*
 * Set pixel limits.
 */
	Xlo = FlipTime ? (float)(End.zt_Sec - Begin.zt_Sec) : 0.0;
	Xhi = FlipTime ? 0.0 : (float)(End.zt_Sec - Begin.zt_Sec);
	Pix_left = XPIX (Xlo);
	Pix_right = XPIX (Xhi);

	Ylo = 0.0;
	Yhi = 1.0;
	Pix_bottom = YPIX (Ylo);
	Pix_top = YPIX (Yhi);
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
 * If we're updating then slide the old data over and draw the new
 * stuff starting from the old end
 */
	if (Update)
	{
		ts_SlideData (&prev_begin, &prev_end);
		Update_begin = prev_end;
	}
	else
		Update_begin = Begin;
/*
 * Draw lines on the background.
 */
	ts_DrawLines (nstep);
/*
 * Annotate
 */
	if (! Update) 
		ts_Annotate(c, pnames, nplat, fnames, nfld);

	ts_AnnotTime ();
/*
 * Draw the background
 */
	if (! Update)
		ts_Background (c, fnames, nfld, center, step);
/*
 * Draw the data 
 */
	ts_PutData (c, pnames, nplat, fnames, nfld);
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
	int	i, j, top, bottom, left, right;
	float	sascale;
/*
 *  Get annotation plot description parameters.
 */
	if(! pda_Search (Pd, comp, "sa-scale", "tseries", (char *) &sascale, 
		SYMT_FLOAT))
		sascale = 0.03;
/*
 *  Top Annotation
 */
	An_TopAnnot ("Time series plot of ");
	for (i = 0; i < nfld; i++)
	{
		An_TopAnnot (px_FldDesc (fields[i]));
		if (i < (nfld - 1)) An_TopAnnot (", ");
	}
	An_TopAnnot (" for platforms: ");
	An_AnnotLimits (&top, &bottom, &left, &right);
	left += 25;
	for (i = 0; i < nplat; i++)
	{
		An_TopAnnot (platforms[i]);
		if (i < (nplat - 1)) An_TopAnnot (", ");
	}
	An_TopAnnot (".");
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
ts_AnnotTime ()
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
	if (Update)
	{
		XSetForeground (disp, Gcontext, Black.pixel);
		XFillRectangle (disp, d, Gcontext, 0, Pix_bottom + 1, 
			GWWidth (Graphics) - 1,
			(int) (TSScale * (float) GWHeight (Graphics)));
	}
	XSetForeground (disp, Gcontext, BackColor.pixel);
/*
 * "Time" label
 */
	DrawText (Graphics, d, Gcontext, (int) (0.5 * GWWidth (Graphics)), 
		Pix_bottom + 1, "Time", 0.0, TSScale, JustifyCenter, 
		JustifyTop);
/*
 * Put time on the left side.
 */
	if (! FlipTime) 
		TC_EncodeTime (&Begin, TC_Full, string);
	else
		TC_EncodeTime (&End, TC_Full, string);
	DrawText (Graphics, d, Gcontext, 
                (int) (Pix_left - 0.04 * GWWidth (Graphics)),
		Pix_bottom + 1, string, 0.0, TSScale, JustifyLeft, JustifyTop);
/*
 * Put time on the right side.
 */
	if (! FlipTime) 
		TC_EncodeTime (&End, TC_Full, string);
	else
		TC_EncodeTime (&Begin, TC_Full, string);
	DrawText (Graphics, d, Gcontext, 
                (int) (Pix_right + 0.04 * GWWidth (Graphics)),
		Pix_bottom + 1, string, 0.0, TSScale, JustifyRight, 
		JustifyTop);
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
	float	tick;
	char	string[STRLEN];	
	int	dolabel;
	int	gwidth = GWWidth (Graphics), gheight = GWHeight (Graphics);
	XPoint	pts[5];
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
 * Label the vertical axis (left). 
 */
	DrawText (Graphics, d, Gcontext, (int) (Pix_left - 0.035 * gwidth),
		(int) (0.5 * gheight), px_FldDesc (fields[0]), 90.0, TSScale, 
		JustifyCenter, JustifyBottom);
/*
 * Draw ticks on the vertical axis (left).
 */
	dolabel = TRUE;

	Ylo = Minval[0];
	Yhi = Maxval[0];

	for (tick = Minval[0]; tick <= Maxval[0]; tick += step[0])
	{
	/*
	 * Draw the tick
	 */
		pts[0].y = pts[1].y = YPIX (tick);
		pts[0].x = Pix_left;
		pts[1].x = Pix_left - 0.01 * gwidth;
		XDrawLines (disp, d, Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{
			sprintf (string, "%d", nint (tick));
			DrawText (Graphics, d, Gcontext, 
				(int) (Pix_left - 0.005 * gwidth), 
                                YPIX (tick), string,
				0.0, TSScale, JustifyRight, JustifyBottom);
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
	DrawText (Graphics, d, Gcontext, (int) (Pix_right + 0.035 * gwidth), 
		(int) (0.5 * gheight), px_FldDesc (fields[1]), -90.0, TSScale, 
		JustifyCenter, JustifyBottom);
/*
 * Draw ticks on the vertical axis (right).
 */
	dolabel = TRUE;

	Ylo = Minval[1];
	Yhi = Maxval[1];

	for (tick = Minval[1]; tick <= Maxval[1]; tick += step[1])
	{
	/*
	 * Draw the tick
	 */
		pts[0].y = pts[1].y = YPIX (tick);
		pts[0].x = Pix_right + 0.01 * gwidth;
		pts[1].x = Pix_right;
		XDrawLines (disp, d, Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{

			sprintf (string, "%d", nint (tick));
			DrawText (Graphics, d, Gcontext, 
				(int) (Pix_right + 0.005 * gwidth), 
				YPIX (tick), 
				string, 0.0, TSScale, JustifyLeft, 
				JustifyBottom);
		}
		dolabel = ! dolabel;
	}
}




static void
ts_PutData (comp, platforms, nplat, fields, nfld)
char	*comp, **platforms, **fields;
int	nplat, nfld;
/*
 *  Plot the data.
 */
{
	int		plat, i, j, linewidth, dskip, npt = 0;
	int		numpts;
	long		t, tprev;
	float		badvalue, v, vprev;
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
		if (! (dc = ds_Fetch (pid, DCC_Scalar, &Update_begin, &End, 
			fieldlist, nfld, NULL, 0)))
		{
			msg_ELog (EF_PROBLEM, "No %s data available", 
				platforms[plat]);
			continue;
		}

		numpts = dc_GetNSample (dc);
		badvalue = dc_GetBadval (dc);
	/*
	 * Now work through the data.
	 */
		for (i = 0; i < nfld; i++)
		{
			XSetForeground (disp, Gcontext, 
				Colors[i * nplat + plat].pixel);

			if (Update)
			{
				tprev = Save_t[plat * nfld + i];
				vprev = Save_v[plat * nfld + i];
			}
			else
				vprev = badvalue;

			npt = 0;
		/*
		 * Set our user coords
		 */
			Ylo = Minval[i];
			Yhi = Maxval[i];
		/* 
		 * Draw the data.
		 */
			for (j = 0; j < numpts; j++)
			{
			/*
			 * Get the value and time of the current data point.
			 */
				v = dc_GetScalar (dc, j, fieldlist[i]);

				dc_GetTime (dc, j, &when);
				t = TC_ZtToSys (&when);
			/*
			 * If its time to skip or the data is bad, continue.
			 */
				if (dskip && (npt++ % dskip) != 0)
					continue;
			/*
			 * Draw the segment if both endpoints are good
			 */
				if (v != badvalue && vprev != badvalue)
					XDrawLine (disp, d, Gcontext, 
						XPIX (tprev - Begin.zt_Sec), 
						YPIX (vprev), 
						XPIX (t - Begin.zt_Sec), 
						YPIX (v)); 
			/*
			 * Set the prev points
			 */
				tprev = t; vprev = v;
			}
		/*
		 * Save the data from the last point.
		 */
			Save_t[plat * nfld + i] = t;
			Save_v[plat * nfld + i] = v;
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
ts_SlideData (prev_begin, prev_end)
ZebTime	*prev_begin, *prev_end;
/*
 * On an update slide the old data down to make room for the new data.
 */
{
	int	beginpix;
	float	save_xlo = Xlo, save_xhi = Xhi;
	Pixmap	temp;
	Display	*disp = XtDisplay (Graphics);
/*
 * Go back to the coordinates of the previous plot
 */
	Xlo = FlipTime ? prev_end->zt_Sec - prev_begin->zt_Sec : 0.0;
	Xhi = FlipTime ? 0.0 : prev_end->zt_Sec - prev_begin->zt_Sec;
/*
 * Create temporary pixmap.
 */
	temp = XCreatePixmap (disp, XtWindow (Graphics), Pix_right - Pix_left,
		Pix_bottom - Pix_top, GWDepth (Graphics));
/*
 * Copy the data.
 */
	XCopyArea (disp, GWFrame (Graphics), temp, Gcontext, Pix_left, 
		Pix_top, Pix_right - Pix_left, Pix_bottom - Pix_top, 0, 0);
/*
 * Clear the area.
 */
	XSetForeground (disp, Gcontext, Black.pixel);
	XFillRectangle (disp, GWFrame (Graphics), Gcontext, Pix_left + 1, 
		Pix_top + 1, Pix_right - Pix_left - 1, 
		Pix_bottom - Pix_top - 1);
/*
 * Find the position in the old plot of our current begin time
 */
	beginpix = XPIX (Begin.zt_Sec - prev_begin->zt_Sec);
/*
 * Turn on clipping.
 */
	XSetClipRectangles (disp, Gcontext, 0, 0, &Clip, 1, Unsorted);
/*
 * Put the data back in its new position.
 */
	if (! FlipTime)
		XCopyArea (disp, temp, GWFrame (Graphics), Gcontext, 0, 0,
			Pix_right - Pix_left, Pix_bottom - Pix_top, 
			2 * Pix_left - beginpix, Pix_top);
	else
		XCopyArea (disp, temp, GWFrame (Graphics), Gcontext, 1, 0,
			Pix_right - Pix_left, Pix_bottom - Pix_top, 
			Pix_left + Pix_right - beginpix + 1, Pix_top);
/*
 * Turn off clipping.
 */
	XSetClipRectangles (disp, Gcontext, 0, 0, &Unclip, 1, Unsorted);
	XFreePixmap (disp, temp);
/*
 * Return to the current time coordinates
 */
	Xlo = save_xlo;
	Xhi = save_xhi;
}



static void
ts_DrawLines (nstep)
int	nstep;
/* 
 * Draw the lines in the background of the plot.
 */
{
	Display	*disp = XtDisplay (Graphics);
	float	tick, tickinc;
	long	sec, secinc;
	ZebTime	first;
	XPoint	pts[2];
	Drawable	d = GWFrame (Graphics);

	XSetForeground (disp, Gcontext, BackColor.pixel);
	XSetLineAttributes (disp, Gcontext, 1, LineOnOffDash, CapButt, 
		JoinMiter);

	if (nstep <= 1) 
		nstep = 2;
/*
 * Horizontal lines.
 */
	pts[0].x = XPIX (Update_begin.zt_Sec - Begin.zt_Sec);
	pts[1].x = XPIX (End.zt_Sec - Begin.zt_Sec);

	tickinc = (Yhi - Ylo) / (nstep - 1);
	for (tick = Ylo; tick < Yhi; tick += tickinc)
	{
		pts[0].y = pts[1].y = YPIX (tick);
		XDrawLines (disp, d, Gcontext, pts, 2, CoordModeOrigin);
	}
/*
 * Find a reasonable time tick interval
 */
	ts_TimeTick (&Begin, &End, &first, &secinc);
/* 
 * Vertical lines.
 */
	pts[0].y = Pix_bottom;
	pts[1].y = Pix_top;

	for (sec = first.zt_Sec; sec < End.zt_Sec; sec += secinc)
	{
		if (sec < Update_begin.zt_Sec)
			continue;

		pts[0].x = pts[1].x = XPIX (sec - Begin.zt_Sec);
		XDrawLines (disp, d, Gcontext, pts, 2, CoordModeOrigin);
	}

	XSetLineAttributes (disp, Gcontext, 1, LineSolid, CapButt, 
		JoinMiter);
}




/*
 * Array of good time tick increments and the minimum plot spans to which
 * they apply
 */
static struct _goodtick
{
	int	inc, minspan;
} Goodtick[] = 
{
	{ 1, 0 },		/* 1 s 	*/
	{ 2, 6 },		/* 2 s for interval >= 6 s	*/
	{ 5, 15 },		/* 5 s for >= 15 s	*/
	{ 10, 30 },		/* 10 s for >= 30 s	*/
	{ 15, 45 },		/* 15 s for >= 45 s	*/
	{ 30, 90 },		/* 30 s for >= 90 s	*/
	{ 60, 60*3 },		/* 1 m for >= 3 m	*/
	{ 60*2, 60*6 },		/* 2 m for >= 6 m	*/
	{ 60*5, 60*15 },	/* 5 m for >= 15 m	*/
	{ 60*10, 60*30 },	/* 10 m for >= 30 m	*/
	{ 60*15, 60*45 },	/* 15 m for >= 45 m	*/
	{ 60*30, 60*90 },	/* 30 m for >= 90 m	*/
	{ 60*60*1, 60*60*3 },	/* 1 h for >= 3 h	*/
	{ 60*60*3, 60*60*12 },	/* 3 h for >= 12 h	*/
	{ 60*60*6, 60*60*24 },	/* 6 h for >= 24 h	*/
	{ 60*60*12, 60*60*48 },	/* 12 h for >= 48 h	*/
	{ 60*60*24*1, 60*60*24*4 },	/* 1 d for >= 4 d	*/
	{ 60*60*24*2, 60*60*24*8 },	/* 2 d for >= 8 d	*/
	{ 0, 0 },		/* no more		*/
};




static void
ts_TimeTick (begin, end, first, step)
ZebTime	*begin, *end, *first;
long	*step;
/*
 * Find a good starting time and increment for time ticks.
 *  Input:
 *	begin:	starting time of plot
 *	end:	end time of plot
 *  Return:
 *	first:	first tick time > begin that's on an even step boundary
 *	step:	tick step
 */
{
	long	span;
	short	i, nstep;
	struct tm	*tm;
#if defined(SVR4) || defined(SYSV) || defined (__osf__)
	char tz[20];
	struct tm zt;
#endif
/*
 * Find the appropriate step in the table
 */
	span = end->zt_Sec - begin->zt_Sec;
	for (i = 0; Goodtick[i].inc && (Goodtick[i].minspan <= span); i++)
		*step = Goodtick[i].inc;
/*
 * Find a good starting time that's an even multiple of our step time
 */
	*first = *begin;

	tm = gmtime ((time_t *)&(first->zt_Sec));
	tm->tm_hour = tm->tm_min = tm->tm_sec = 0;
#if defined(SVR4) || defined(SYSV) || defined (__osf__)
        strcpy (tz, "TZ=GMT");
        putenv (tz);
#ifdef notdef
        timezone = 0;
        daylight = 0;
#endif
        zt.tm_wday = zt.tm_yday = 0;
        zt.tm_isdst = -1;

	first->zt_Sec = mktime (tm);
#else
	first->zt_Sec = timegm (tm);
#endif

	nstep = (begin->zt_Sec - first->zt_Sec) / *step;
	first->zt_Sec += nstep * (*step);
	if (first->zt_Sec < begin->zt_Sec)
		first->zt_Sec += *step;
}
	



# endif  /* C_PT_TSERIES */
