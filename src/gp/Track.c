/*
 * Track drawing routines.
 */
static char *rcsid = "$Id: Track.c,v 1.10 1991-01-14 18:20:08 kris Exp $";


# include <X11/Intrinsic.h>
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
	static bool tr_CCSetup (char *, char *, char *, XColor **,
		int *, float *, float *, XColor *);
# else
	static bool tr_CCSetup ();
# endif

# define BADVAL -32768


void
tr_CAPTrack (comp, update)
char *comp;
bool update;
{
	char platform[30], tp[30], ccfield[30], ctable[30];
	char *fields[5], mtcolor[20], string[40], format[30];
	char a_interval[30], a_color[30], a_xfield[30], a_yfield[30];
	char a_type[30];
	int period, dsperiod, x0, y0, x1, y1, nc, lwidth, pid;
	int dskip = 0, npt = 0, i, top, bottom, left, right, wheight, mid;
	int arrow, a_invert, a_int, numfields = 1, dummy, xannot, yannot;
	int timenow, vectime = 0, a_lwidth;
	unsigned int udummy, dwidth, dheight;
	bool mono; 
	time begin;
	float *data, fx, fy, base, incr, cval, a_scale, *a_xdata, *a_ydata;
	float a_x, a_y, unitlen;
	Drawable d;
	Window win;
	Display *disp = XtDisplay (Graphics);
	XColor xc, *colors, outrange, a_clr;
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
	if (! tr_GetParam (comp, update ? "trigger" : "time-period", platform,
			tp, SYMT_STRING))
		period = 300;
	else if ((period = pc_TimeTrigger (tp)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Unparsable time-period: '%s'", tp);
		period = 300;
	}
	if (update)
		period = (5*period)/4;
	if (PlotTime.ds_yymmdd == 0)
	{
		msg_ELog (EF_PROBLEM, "ZERO PLOT TIME?????");
		return;
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
	if (! tr_GetParam (comp, "track-color", platform, mtcolor,SYMT_STRING))
		strcpy (mtcolor, "white");
/*
 * Color code field.
 */
	mono = ! (tr_GetParam (comp, "field", platform, ccfield, SYMT_STRING)
			|| tr_GetParam (comp, "color-code-field", platform,
				ccfield, SYMT_STRING));
	if (! mono)
		mono = ! tr_CCSetup (comp, platform, ccfield, &colors,
				&nc, &base, &incr, &outrange);
/*
 * Put together the field list.
 */
	if (! mono)
		fields[0] = ccfield;
	else
		fields[0] = "temperature";	/* XXX */
/*
 * Read arrow parameters if necessary.
 */
	arrow = FALSE;
	tr_GetParam(comp, "arrow", platform, (char *) &arrow, SYMT_BOOL);
	if(arrow)
	{
		if(! tr_GetParam(comp, "arrow-scale", platform, 
				(char *) &a_scale,
				SYMT_FLOAT))
			a_scale = 0.007;
		if(! tr_GetParam(comp, "arrow-line-width", platform, 
				(char *) &a_lwidth,
				SYMT_INT))
			a_lwidth = 1;
		if(! tr_GetParam(comp, "arrow-invert", platform, 
				(char *) &a_invert, SYMT_BOOL))
			a_invert = FALSE;
		if(! tr_GetParam(comp, "arrow-interval", platform, a_interval,
				SYMT_STRING))
			a_int = 30;
		else if((a_int = pc_TimeTrigger(a_interval))  == 0)
		{
			msg_ELog(EF_PROBLEM,"Unparsable arrow interval:
				'%s'.",a_interval);
			a_int = 30;
		}
		if(! tr_GetParam(comp, "arrow-color", platform, a_color,
				SYMT_STRING))
			strcpy(a_color,"white");
		if(! ct_GetColorByName(a_color,&a_clr))
		{
			msg_ELog(EF_PROBLEM,"Can't get arrow color:
				'%s'.",a_color);
			strcpy(a_color,"white");
			ct_GetColorByName(a_color,&a_clr);
		}
		if(! tr_GetParam(comp, "arrow-type", platform, a_type,
				SYMT_STRING))
			strcpy(a_type,"wind");
		if(! tr_GetParam(comp, "x-field", platform, a_xfield,
				SYMT_STRING))
			strcpy(a_xfield,"u_wind");
		if(! tr_GetParam(comp, "y-field", platform, a_yfield,
				SYMT_STRING))
			strcpy(a_yfield,"v_wind");
		fields[1] = a_xfield;
		fields[2] = a_yfield;
		numfields = 3;
	} 

/*
 * Figure the begin time.
 */
	begin = PlotTime;
	dsperiod = (period/3600)*10000 + ((period/60) % 60)*100 + period % 60;
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, dsperiod);
/*
 * Get the data.
 */
	dobj = ds_GetData (pid, fields, numfields, &begin, &PlotTime, OrgScalar,
		0.0, BADVAL);
	if (! dobj)
	{
		msg_ELog (EF_INFO, "No %s data available", platform);
		return;
	}
	msg_ELog (EF_DEBUG, "Got track data, %d pt", dobj->do_npoint);
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
			timenow = tr_GetSec(((dobj->do_times) + i - 1)
				->ds_hhmmss);
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
				tr_DrawVector(x0, y0, a_x, a_y, unitlen,
					disp,d, Gcontext);
				XSetLineAttributes (disp, Gcontext, lwidth, 
					LineSolid, CapButt, JoinMiter);
			}
		}
	}

	XSetLineAttributes (disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
	ds_FreeDataObject (dobj);
/*
 * Annotate if necessary.
 */
 	if (! update)
	{
	/*
	 * On the top.
	 */
		An_TopAnnot (platform, White);
		An_TopAnnot (" track", White);
		if (! mono)
		{
			An_TopAnnot (" color coded by ", White);
			An_TopAnnot (px_FldDesc (comp, ccfield), White);
		}
	/*
	 * Annotate arrows if necessary.
	 */
		if(arrow)
		{
			An_TopAnnot(" with ",White);
			An_TopAnnot(a_type,White);
			An_TopAnnot(" vectors",White);
		}
		An_TopAnnot (".", White);
	/*
	 * Down the side too.
	 */
		An_AnnotLimits (&top, &bottom, &left, &right);
		XSetForeground (disp, Gcontext, White);
		wheight = GWHeight (Graphics);
		
		left += 10;
		top += 5;
		mid = (left + right)/2;
	/*
	 * Some text.
	 */
		sprintf (string, "%s:", ccfield);

	 	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, top,
			string, 0.0, 0.02, JustifyLeft, JustifyTop);

		top += (int)(1.2 * 0.02 * wheight);
	/*
	 * See if there is a special printf format for this field.
	 */
		if (! pda_Search (Pd, comp, "annotation-format", ccfield,
					format, SYMT_STRING))
			strcpy (format, "%.1f");
	/*
	 * Then put in the annotation.
	 */
		cval = base + incr/2.0;
		for (i = 0; i <= nc; i++)
		{
		/*
		 * Numeric label
		 */
			cval += incr;
			sprintf (string, format, cval);

			XSetForeground (disp, Gcontext, colors[i].pixel);
			DrawText (Graphics, GWFrame (Graphics),
				Gcontext, EVEN(i) ? left : mid, top, string, 
				0.0, 0.02, JustifyLeft, JustifyTop);
			if (ODD(i))
				top += (int)(1.2 * 0.02 * wheight);
		}
	/*
	 * Annotate arrows if necessary on side
	 */
		if(arrow)
		{
			top += 5;
			xannot = (left + right)/2;
			yannot = top + 0.04 * wheight;
			DrawText(Graphics, GWFrame(Graphics), Gcontext,
				xannot, yannot, "VECTOR", 0.0, 0.02,
				JustifyCenter, JustifyBottom); 
			yannot += 0.022 * wheight;
			DrawText(Graphics, GWFrame(Graphics), Gcontext,
				xannot, yannot, "SCALE", 0.0, 0.02,
				JustifyCenter, JustifyBottom); 
			xannot = left + 0.25 * (right - left);
			yannot += 0.03 * wheight;
			DrawText(Graphics, GWFrame(Graphics), Gcontext,
				xannot, yannot, "5.0", 0.0, 0.02,
				JustifyCenter, JustifyBottom); 
			tr_DrawVector(xannot, yannot + 4, 0.0, -5.0, unitlen,
				disp, d, Gcontext);
			xannot = left + 0.75 * (right - left);
			DrawText(Graphics, GWFrame(Graphics), Gcontext,
				xannot, yannot, "10.0", 0.0, 0.02,
				JustifyCenter, JustifyBottom); 
			tr_DrawVector(xannot, yannot + 4, 0.0, -10.0, unitlen,
				disp, d, Gcontext);
		}
	
	}
}






static bool
tr_CCSetup (comp, platform, ccfield, colors, nc, base, incr, outrange)
char *comp, *platform, *ccfield;
XColor **colors, *outrange;
int *nc;
float *base, *incr;
/*
 * Get everything set up to color-code a track.
 */
{
	float center, step;
	char orc[20], ctable[20];
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
			&center, SYMT_FLOAT) ||
	    ! pda_ReqSearch (Pd, comp, "track-step", ccfield, (char *) &step,
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
	*base = center - (*nc/2)*step - step/2;
	*incr = step;

	return (TRUE);
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



tr_GetSec(hhmmss)
int hhmmss;
{
	int sec, sub;

	sec = hhmmss % 100;
	hhmmss -= sec;
	sub = hhmmss % 10000;
	sec += sub / 100 * 60;
	hhmmss -= sub;
	sec += hhmmss / 10000 * 3600;
	return(sec);
}	
