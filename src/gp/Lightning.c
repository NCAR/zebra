/*
 * Lightning display routine.
 */
static char *rcsid = "$Id: Lightning.c,v 2.4 1991-10-31 20:29:23 kris Exp $";
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

void li_CAPLight ();
void li_SideAnnot ();

void
li_CAPLight (comp, update)
char *comp;
bool update;
{
	char	platform[30], step[30], ctable[30];
	char	**fields, string[40], tadefcolor[30], data[100];
	int	period, dsperiod, x, y, numcolor, pid, istep;
	int	i, top, bottom, left, right, numfields, index;
	time	begin, t;
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
        	sprintf (data, "%s %d %d", ctable, dobj->do_npoint, istep);
		An_AddAnnotProc (li_SideAnnot, comp, data, strlen (data),
			75, TRUE, FALSE);
	}
/*
 * Put in the status line before we lose the data object, then get rid of it.
 */
	lw_TimeStatus (comp, &dobj->do_end);
	ds_FreeDataObject (dobj);
}



void
li_SideAnnot (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * Do side annotation for a lightning plot.
 */
{
        char string[40], ctable[40];
        float max, min, frac, val, used, scale;
        int i, left, limit, ncolors, bar_height, match;
	int npoint, istep;
        XColor *colors, xc;
/*
 * Get the data.
 */
	An_GetSideParams (comp, &scale, &limit);
	An_GetTopParams (&xc, &match);
        sscanf (data, "%s %d %d", ctable, &npoint, &istep);
        ct_LoadTable (ctable, &colors, &ncolors);
/*
 * Put in the colored bars.
 */
	left = An_GetLeft ();
	bar_height = (float) space / (float) (ncolors + 1);
	for (i = 0; i < ncolors; i++)
	{
		XSetForeground(XtDisplay (Graphics), Gcontext, 
			colors[i].pixel);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, left, begin, 10, bar_height);
		sprintf (string, "%d - %d", i*(istep),(i+1)*istep); 
		XSetForeground (XtDisplay (Graphics), Gcontext, xc.pixel);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left + 15, 
			begin, string, 0.0, scale, JustifyLeft, JustifyTop);
		begin += bar_height;	
		space -= bar_height;
	}
	sprintf (string, "Total = %d", npoint); 
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, 
		begin, string, 0.0, scale, JustifyLeft, JustifyTop);
}


# endif   /* C_CAP_LIGHTNING */
