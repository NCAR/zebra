/*
 * Track drawing routines.
 */
static char *rcsid = "$Id: Track.c,v 1.4 1990-11-14 10:51:25 burghart Exp $";


# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"


extern Pixel White;	/* XXX */

/*
 * Source name to file mapping.
 */
static struct amap
{
	char	am_name[20];		/* Aircraft name		*/
	char	*am_file;		/* File ptr			*/
} Amap[20];
static int Namap = 0;



/*
 * Forwards.
 */
# ifdef __STDC__
	static char *tr_GetFile (char *);
	static bool tr_CCSetup (char *, char *, char *, char *, XColor **,
		int *, float *, float *, XColor *);
# else
	static char *tr_GetFile ();
	static bool tr_CCSetup ();
# endif



void
tr_CAPTrack (comp, update)
char *comp;
bool update;
{
	char platform[30], *file, tp[30], ccfield[30], ctable[30];
	char *fields[5], field_data[60], *afctx, mtcolor[20], string[40];
	int period, nfield, dsperiod, when, x0, y0, x1, y1, nc, lwidth;
	int dskip = 0, npt = 0, i, top, bottom, left, right, wheight, mid;
	bool mono;
	time begin;
	float data[5], fx, fy, base, incr, cval;
	Drawable d;
	GC Gcontext;
	Display *disp = XtDisplay (Graphics);
	XColor xc, *colors, outrange;
/*
 * Get our platform first, since that's what is of interest to us.
 */
	if (! pda_ReqSearch (comp, "platform", NULL, platform, SYMT_STRING))
		return;
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
 * Find our file.
 */
	if (! (file = tr_GetFile (platform)))
		return;
/*
 * Color code field.
 */
	mono = ! (tr_GetParam (comp, "field", platform, ccfield, SYMT_STRING)
			|| tr_GetParam (comp, "color-code-field", platform,
				ccfield, SYMT_STRING));
	if (! mono)
		mono = ! tr_CCSetup (file, comp, platform, ccfield, &colors,
				&nc, &base, &incr, &outrange);
/*
 * Put together the field list.
 */
	fields[0] = "alat";
	fields[1] = "alon";
	nfield = 2;
	if (! mono)
		fields[nfield++] = ccfield;
/*
 * Figure the begin time.
 */
	begin = PlotTime;
	dsperiod = (period/3600)*10000 + ((period/60) % 60)*100 + period % 60;
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, dsperiod);
/*
 * Position to get the data.
 */
	if (! (afctx = af_Setup (file, &begin, nfield, fields)))
	{
		msg_ELog (EF_PROBLEM, "AF Setup failed");
		return;
	}
/*
 * Get the initial point.
 */
	if (! af_NextSample (afctx, &when, data))
	{
		msg_ELog (EF_DEBUG, "No aircraft data for initial point");
		af_ReleaseCtx (afctx);
		return;
	}
	cvt_ToXY (data[0], data[1], &fx, &fy);
	x0 = XPIX (fx); y0 = YPIX (fy);
/*
 * We need a graphics context.
 */
	Gcontext = XCreateGC (disp, XtWindow (Graphics), 0, NULL);
	ct_GetColorByName (mtcolor, &xc);
	XSetForeground (disp, Gcontext, xc.pixel);
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
	for (;;)
	{
	/*
	 * Get another sample, and make sure we're not past our time.
	 */
		if (! af_NextSample (afctx, &when, data) ||
				when > PlotTime.ds_hhmmss)
			break;
	/*
	 * Do skipping if requested.
	 */
		if (dskip && (npt++ % dskip) != 0)
			continue;
	/*
	 * Locate this point.
	 */
		cvt_ToXY (data[0], data[1], &fx, &fy);
		x1 = XPIX (fx); y1 = YPIX (fy);
	/*
	 * Color code if necessary.
	 */
	 	if (! mono)
		{
			int index = (data[2] - base)/incr;
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
	af_ReleaseCtx (afctx);
	XFreeGC (disp, Gcontext);
/*
 * Annotate if necessary.
 */
 	if (! update)
	{
	/*
	 * On the top.
	 */
		px_TopAnnot (platform, White);
		px_TopAnnot (" track", White);
		if (! mono)
		{
			px_TopAnnot (" color coded by ", White);
			px_TopAnnot (px_FldDesc (comp, ccfield), White);
		}
		px_TopAnnot (".", White);
	/*
	 * Down the side too.
	 */
		px_AnnotLimits (&top, &bottom, &left, &right);
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
tr_CCSetup (file, comp, platform, ccfield, colors, nc, base, incr, outrange)
char *file, *comp, *platform, *ccfield;
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
 * Check and make sure they've given us a real field.
 */
	if (! af_FieldOK (file, ccfield))
	{
		msg_ELog (EF_PROBLEM, "Color code field '%s' unknown",
			ccfield);
		return (FALSE);
	}
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
	if (! pda_ReqSearch (comp, "track-center", ccfield, (char *) &center,
		SYMT_FLOAT) ||
	    ! pda_ReqSearch (comp, "track-step", ccfield, (char *) &step,
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






static char *
tr_GetFile (platform)
char *platform;
/*
 * Open up the aircraft file associated with this platform.
 */
{
	int i;
/*
 * See if it's already open.
 */
	for (i = 0; i < Namap; i++)
		if (! strcmp (platform, Amap[i].am_name))
			return (Amap[i].am_file);
/*
 * Open it ourselves.
 */
	if (! (Amap[Namap].am_file = af_OpenFile (platform)))
		return (0);
	strcpy (Amap[Namap++].am_name, platform);
	return (Amap[Namap-1].am_file);
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

