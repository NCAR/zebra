/*
 * Lightning display routine.
 */
static char *rcsid = "$Id: Lightning.c,v 2.2 1991-09-12 19:29:08 corbet Exp $";
# include <config.h>
# if C_CAP_LIGHTNING

# include <X11/Intrinsic.h>
# include <math.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include <ui_date.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"
# define BADVAL -32768

extern Pixel	White;

void CAPLight ();

void
li_CAPLight (comp, update)
char *comp;
bool update;
{
	char	platform[30], step[30], ctable[30];
	char	**fields, string[40],	tadefcolor[30];
	int	period, dsperiod, x, y, numcolor, pid, istep;
	int	i, top, bottom, left, right, numfields, index;
	time	begin, t, temp;
	float	fx, fy, cval, sascale, bar_height;
	Display	*disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
	XColor	*colors, tadefclr;
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
		msg_ELog (EF_PROBLEM, 
			"Lightning display on static platform %s", platform);
		return;
	}
/*
 * Pull out other parameters of interest.
 */
	if (! pda_Search (Pd, comp, "time-step", platform, step, SYMT_STRING))
		period = 300;
	else if ((period = pc_TimeTrigger (step)) == 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Unparsable time-step: '%s'", step);
		period = 300;
	}
	if (PlotTime.ds_yymmdd == 0)
	{
		msg_ELog (EF_PROBLEM, "ZERO PLOT TIME?????");
		return;
	}
/*
 * Get the color table.
 */
	if (! pda_ReqSearch (Pd, comp, "color-table", platform, ctable, 
		SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, 
			"No color table specified in component %s", comp);
		return;
	}
	if (! ct_LoadTable (ctable, &colors, &numcolor))
	{
		msg_ELog (EF_PROBLEM, "Unable to load color table %s", ctable);
		return;
	}
/*
 * Read in annotation information.
 */
	if(! pd_Retrieve (Pd, "global", "ta-color", tadefcolor, SYMT_STRING))
		strcpy (tadefcolor, "white");
	if(! ct_GetColorByName (tadefcolor, &tadefclr))
	{
		msg_ELog (EF_PROBLEM, 
			"Can't get default color: '%s'.", tadefcolor);
		strcpy (tadefcolor, "white");
		ct_GetColorByName (tadefcolor, &tadefclr);
	}
	if (! pda_Search (Pd, comp, "sa-scale", platform, (char *) &sascale,
		SYMT_FLOAT))
		sascale = 0.02;
/*
 * Figure the begin time.
 */
	begin = PlotTime;
	istep = period / 60;
	period *= numcolor;
	dsperiod = update ? 15 :
		(period/3600)*10000 + ((period/60) % 60)*100 + period % 60;
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, dsperiod);
/*
 * Set up the field list.
 */
	numfields = 0;
	fields = NULL;
/*
 * Get the data.
 */
	dobj = ds_GetData (pid, fields, numfields, &begin, &PlotTime, 
		OrgScalar, 0.0, BADVAL);
	if (! dobj)
	{
		msg_ELog (EF_INFO, "No %s data available", platform);
		return;
	}
/*
 * Work through the data.
 */
	for (i = 0; i < dobj->do_npoint; i++)
	{
		Location *loc = ds_Where (dobj, i);
	/*
	 * Locate this point.
	 */
		cvt_ToXY (loc->l_lat, loc->l_lon, &fx, &fy);
		x = XPIX (fx); 
		y = YPIX (fy);
	/*
	 * Place the icon. 
	 */
		t = dobj->do_times[i];
		index = (int) (((float) TC_FccToSys (&PlotTime) - 
			(float) TC_FccToSys (&t)) / (float) period * 
			(float) (numcolor - 1));
		if ((index < 0) || (index >= numcolor))
			index = 0;
		ov_PositionIcon ("light", x, y, colors[index].pixel);
	}
/*
 * Annotate if necessary.
 */
 	if (! update)
	{
	/*
	 * On the top.
	 */
		An_TopAnnot ("Lightning location. ", tadefclr.pixel);
	/*
	 * Down the side too.
	 */
		An_AnnotLimits (&top, &bottom, &left, &right);
		
		left += 5;
		top += 5;
		bar_height = (float) (bottom - top) / (float) numcolor;
		temp.ds_yymmdd = 0;
		for (i = 0; i < numcolor; i++)
		{
			XSetForeground(disp, Gcontext, colors[i].pixel);
			XFillRectangle (disp, d, Gcontext, left,
				(int) (top + i * bar_height), 10,
				(int) (bar_height + 1));
			sprintf (string, "%d - %d", i*(istep),(i+1)*istep); 
			XSetForeground (disp, Gcontext, White);
			DrawText (Graphics, d, Gcontext, left + 15, 
				(int) (top + i * bar_height), string, 
				0.0, sascale, JustifyLeft, JustifyTop);
		}
		sprintf (string, "Total = %d", dobj->do_npoint); 
		DrawText (Graphics, d, Gcontext, left, 
			(int) (top + numcolor * bar_height), string, 0.0, 
			sascale, JustifyLeft, JustifyTop);
		An_SAUsed ((int) (top + (numcolor + 1) * bar_height +  8));
	}
/*
 * Put in the status line before we lose the data object, then get rid of it.
 */
	lw_TimeStatus (comp, &dobj->do_end);
	ds_FreeDataObject (dobj);
}


# endif   /* C_CAP_LIGHTNING */
