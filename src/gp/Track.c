/*
 * Track drawing routines.
 */
static char *rcsid = "$Id: Track.c,v 2.5 1991-10-31 20:29:56 kris Exp $";
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
# include "../include/DataStore.h"
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"

# define ARROWANG .2618 /* PI/12 */

extern Pixel White;	/* XXX */


/*
 * Forwards.
 */
# ifdef __STDC__
	static bool tr_CCSetup (char *, char *, char *, char *, XColor **,
		int *, float *, float *, XColor *, float *, float *);
	static void tr_GetArrowParams (char *, char *, float *, int *, int *,
			int *, char *, XColor *, char *, char *, char *);
# else
	static bool tr_CCSetup ();
	static void tr_GetArrowParams ();
# endif

# define BADVAL -32768


void
tr_CAPTrack (comp, update)
char *comp;
bool update;
{
	char platform[30], tp[30], ccfield[30], datastr[100];
	char *fields[5], mtcolor[20], string[40], ctable[30], a_color[30];
	char a_xfield[30], a_yfield[30];
	char a_type[30], tadefcolor[30];
	int period, dsperiod, x0, y0, x1, y1, nc, lwidth, pid;
	int dskip = 0, npt = 0, i, top, bottom, left, right, wheight, mid;
	int arrow, a_invert, a_int, numfields = 1, dummy, xannot, yannot;
	int a_lwidth, tacmatch, ctlimit;
	long timenow, vectime = 0;
	unsigned int udummy, dwidth, dheight;
	bool mono; 
	time begin;
	float *data, fx, fy, base, incr, cval, a_scale, *a_xdata, *a_ydata;
	float a_x, a_y, unitlen, sascale, center, step;
	Drawable d;
	Window win;
	Display *disp = XtDisplay (Graphics);
	XColor xc, *colors, outrange, a_clr, taclr, tadefclr;
	DataObject *dobj;

/*
 * Get our platform first, since that's what is of interest to us.
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return;
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform '%s'", platform);
		return;
	}
/*
 * Make sure that we'll get the right sort of stuff.
 */
	if (! ds_IsMobile (pid))
	{
		msg_ELog (EF_PROBLEM, "Track attempted on static platform %s",
			platform);
		return;
	}
/*
 * Pull out other parameters of interest.
 */
# ifdef notdef
	if (! tr_GetParam (comp, update ? "trigger" : "time-period", platform,
			tp, SYMT_STRING))
# endif
	if (! tr_GetParam (comp, "time-period", platform, tp, SYMT_STRING))
		period = 300;
	else if ((period = pc_TimeTrigger (tp)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Unparsable time-period: '%s'", tp);
		period = 300;
	}
	if (update)
		period = period/4;
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
	mono = ! (tr_GetParam (comp, "field", platform, ccfield, SYMT_STRING)
			|| tr_GetParam (comp, "color-code-field", platform,
				ccfield, SYMT_STRING));
	if (! mono)
		mono = ! tr_CCSetup (comp, platform, ccfield, ctable, &colors,
				&nc, &base, &incr, &outrange, &center, &step);
/*
 * Put together the field list.
 */
	if (! mono)
	{
		fields[0] = ccfield;
		numfields = 1;
	}
	else
	{
		fields[0] = NULL;
		numfields = 0;
	}
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
		fields[1] = a_xfield;
		fields[2] = a_yfield;
		numfields += 2;
	} 
/*
 * Read in annotation information.
 */
	if(! tr_GetParam("global", "ta-color", NULL, tadefcolor, 
		SYMT_STRING))
		strcpy(tadefcolor, "white");
	if(! ct_GetColorByName(tadefcolor, &tadefclr))
	{
		msg_ELog(EF_PROBLEM,"Can't get default color:
			'%s'.",tadefcolor);
		strcpy(tadefcolor,"white");
		ct_GetColorByName(tadefcolor,&tadefclr);
	}
	tacmatch = FALSE;
	tr_GetParam("global", "ta-color-match", NULL, (char *) &tacmatch,
		SYMT_BOOL);
	if(tacmatch)
		taclr = a_clr;
	else taclr = tadefclr;
	if(! tr_GetParam(comp, "sa-scale", platform, (char *) &sascale,
		SYMT_FLOAT))
		sascale = 0.02;
	if(! tr_GetParam(comp, "ct-limit", platform, (char *) &ctlimit,
		SYMT_INT))
		ctlimit = 1;
	if(ctlimit < 1) ctlimit = 1;
/*
 * Figure the begin time.
 */
	begin = PlotTime;
	dsperiod = (period/3600)*10000 + ((period/60) % 60)*100 + period % 60;
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, dsperiod);
/*
 * Get the data.
 */
	dobj = ds_GetData(pid, fields, numfields, &begin, &PlotTime, OrgScalar,
		0.0, BADVAL);
	if (! dobj)
	{
		msg_ELog (EF_INFO, "No %s data available", platform);
		return;
	}
/*
 * Convert the first points.
 */
	cvt_ToXY (dobj->do_aloc[0].l_lat, dobj->do_aloc[0].l_lon, &fx, &fy);
	x0 = XPIX (fx); y0 = YPIX (fy);
	data = dobj->do_data[0];
	if(arrow)
	{
		a_xdata = dobj->do_data[1];
		a_ydata = dobj->do_data[2];
	}
/*
 * We need a graphics context.
 */
	if (mono)
	{
		ct_GetColorByName (mtcolor, &xc);
		XSetForeground (disp, Gcontext, xc.pixel);
	}
	d = GWFrame (Graphics);
/*
 * How wide do they like their lines?
 */
	if (! tr_GetParam (comp, "line-width", platform, (char *) &lwidth,
			SYMT_INT))
		lwidth = 0;
	XSetLineAttributes (disp, Gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Now work through the data.
 */
	for (i = 1; i < dobj->do_npoint; i++)
	{
		Location *loc = ds_Where (dobj, i);
	/*
	 * Do skipping if requested.
	 */
		if (dskip && (npt++ % dskip) != 0)
			continue;
	/*
	 * Locate this point.
	 */
		cvt_ToXY (loc->l_lat, loc->l_lon, &fx, &fy);
		x1 = XPIX (fx); y1 = YPIX (fy);
	/*
	 * Color code if necessary.
	 */
	 	if (! mono)
		{
			int index = (data[i] - base)/incr;
			XSetForeground (disp, Gcontext,
				(index >= 0 && index < nc) ?
				colors[index].pixel : outrange.pixel);
		}
	/*
	 * Finally draw the line.
	 */
		XDrawLine (disp, d, Gcontext, x0, y0, x1, y1); 
		x0 = x1; y0 = y1;
	/*
	 * Draw arrows if necessary.
	 */
		if(arrow)
		{
			timenow = TC_FccToSys (dobj->do_times + i);
			if(((timenow % a_int) == 0) || 
			   ((vectime + a_int) < timenow))
			{
				vectime = timenow - timenow % a_int;
				XGetGeometry(disp, d, &win, &dummy, &dummy,
					&dwidth, &dheight, &udummy, 
					&udummy);
				unitlen = dheight * a_scale;
				XSetForeground(disp, Gcontext, a_clr.pixel);
				a_x = *(a_xdata + i - 1);
				XSetLineAttributes (disp, Gcontext, a_lwidth, 
					LineSolid, CapButt, JoinMiter);
				a_y = *(a_ydata + i - 1);
				draw_vector (disp, d, Gcontext, x0, y0, 
					a_x, a_y, unitlen);
				XSetLineAttributes (disp, Gcontext, lwidth, 
					LineSolid, CapButt, JoinMiter);
			}
		}
	}
	XSetLineAttributes (disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
/*
 * Put in the status line before we lose the data object, then get rid of it.
 * (Only do it if this isn't an update.  It won't get printed anyway, and
 * it's likely to overflow the overlay widget's text space.)
 */
	if (! update)
		lw_TimeStatus (comp, &dobj->do_end);
	ds_FreeDataObject (dobj);
/*
 * Annotate if necessary.
 */
 	if (! update)
	{
	/*
	 * On the top.
	 */
		An_TopAnnot(" ", tadefclr.pixel);
		An_TopAnnot (platform, tadefclr.pixel);
		if (! mono)
		{
			An_TopAnnot(" ", tadefclr.pixel);
			An_TopAnnot (px_FldDesc (comp, ccfield), 
				tadefclr.pixel);
		}
		An_TopAnnot (" track", tadefclr.pixel);
	/*
	 * Annotate arrows if necessary.
	 */
		if(arrow)
		{
			An_TopAnnot(" with ",tadefclr.pixel);
			An_TopAnnot(a_type,taclr.pixel);
			An_TopAnnot(" vectors",taclr.pixel);
		}
		An_TopAnnot (".  ", tadefclr.pixel);
	/*
	 * Down the side too.
	 */
		if (mono)
		{
			sprintf (datastr, "%s %s", platform, mtcolor);
			An_AddAnnotProc (An_ColorString, comp, datastr,
				strlen (datastr), 25, FALSE, FALSE);
		}
		else
		{
			sprintf (datastr, "%s %s %f %f", ccfield, ctable,
				center, step);
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
	char orc[20];
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
	if (! pda_ReqSearch (Pd, comp, "track-center", ccfield, (char *)
			center, SYMT_FLOAT) ||
	    ! pda_ReqSearch (Pd, comp, "track-step", ccfield, (char *) step,
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
int *a_lwidth, *a_invert, *a_int;
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
		msg_ELog(EF_PROBLEM,"Unparsable arrow interval:
			'%s'.",a_interval);
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
