/*
 * Track drawing routines.
 */
static char *rcsid = "$Id: Track.c,v 1.7 1990-12-04 15:12:30 corbet Exp $";


# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "../include/DataStore.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"


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
	char *fields[5], mtcolor[20], string[40];
	int period, dsperiod, x0, y0, x1, y1, nc, lwidth, pid;
	int dskip = 0, npt = 0, i, top, bottom, left, right, wheight, mid;
	bool mono;
	time begin;
	float *data, fx, fy, base, incr, cval;
	Drawable d;
	GC Gcontext;
	Display *disp = XtDisplay (Graphics);
	XColor xc, *colors, outrange;
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
 * Figure the begin time.
 */
	begin = PlotTime;
	dsperiod = (period/3600)*10000 + ((period/60) % 60)*100 + period % 60;
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, dsperiod);
/*
 * Get the data.
 */
	dobj = ds_GetData (pid, fields, 1, &begin, &PlotTime, OrgScalar,
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
/*
 * We need a graphics context.
 */
	Gcontext = XCreateGC (disp, XtWindow (Graphics), 0, NULL);
	if (mono)
	{
		ct_GetColorByName (mtcolor, &xc);
		XSetForeground (disp, Gcontext, xc.pixel);
	}
	d = GWFrame (Graphics);
/*
 * How wide do they like their lines?
 */
	if (tr_GetParam (comp, "line-width", platform, (char *) &lwidth,
		SYMT_INT))
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
	}
	XFreeGC (disp, Gcontext);
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
		An_TopAnnot (".", White);
	/*
	 * Down the side too.
	 */
		An_AnnotLimits (&top, &bottom, &left, &right);
		left += 10;
		top += 20;
		wheight = GWHeight (Graphics);
		mid = (left + right)/2;
	/*
	 * Some text.
	 */
		sprintf (string, "%s:", ccfield);
	 	DrawText (Graphics, GWFrame (Graphics), White, left, top,
			string, 0.0, 0.02, JustifyLeft, JustifyTop);
		top += (int)(1.2 * 0.02 * wheight);
	/*
	 * Numbers too.
	 */
		cval = base + incr/2.0;
		for (i = 0; i <= nc; i++)
		{
		/*
		 * Numeric label
		 */
			cval += incr;
			sprintf (string, "%.1f", cval);
			DrawText (Graphics, GWFrame (Graphics),
				colors[i].pixel, EVEN(i) ? left : mid, top,
				string, 0.0, 0.02, JustifyLeft, JustifyTop);
			if (ODD(i))
				top += (int)(1.2 * 0.02 * wheight);
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

