/*
 * XY-Wind plotting module
 */
static char *rcsid = "$Id: XYWind.c,v 1.20 1993-12-01 17:32:30 burghart Exp $";
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

# if C_PT_XYGRAPH


# include <math.h>
# include <X11/Intrinsic.h>
# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <pd.h>
# include <time.h>
# include <message.h>
# include <DataStore.h>
# include "derive.h"
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "XYCommon.h"
# include "AxisControl.h"
# include "PlotPrim.h"

/*
 * General definitions
 */
void	xy_Wind FP ((char *, int));




void
xy_Wind(c, update)
char	*c;
bool	update;
/*
 * Draw an xy-graph on the given component
 */
{
	bool	ok, xauto, yauto, xinvert, yinvert, angle, sideAnnot, doKnot;
	int	npts[MAX_PLAT], plat, nplat;
	int	nxfield, nyfield, ncolors, skip, dmode;
	char	platforms[MAX_PLAT_LEN], *pnames[MAX_PLAT];
	char	xflds[MAX_PLAT_LEN], yflds[MAX_PLAT_LEN];
	char	*xfnames[MAX_PLAT], *yfnames[MAX_PLAT];
	char	windfld1[32], windfld2[32], ctname[32], style[32];
	char	xtype, ytype, csystem[16], annotcontrol[80], barbtype[16];
	float   cstep, scaleSpeed, vecScale = 0.01;
	XColor	*colors;
	ZebTime	bTimeTarget, eTimeTarget, eTimeReq, bTimeReq;
	ZebTime	eTimeOld,bTimeOld;
	DataValPtr	*xdata, *ydata, *w1data, *w2data;
	DataValRec	xmin, xmax, ymin, ymax;
	DataValRec	xleft, xright, ybottom, ytop;
	xyDataVector	dv[4];
	Pixel	taColor;
/*
 * Get X-Y Winds Required parameters:
 * "platform","x-field", "y-field", "coords", "color-table", "org"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "x-field", NULL, xflds, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "y-field", NULL, yflds, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "coords", "xy-wind", csystem, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "color-table", "xy-wind", ctname,
			     SYMT_STRING);

	if (! ok)
		return;
/*
 * Winds coordinate system
 */
	if (! strcmp (csystem, "compass"))
	{
		strcpy (windfld1, "wdir");
		pda_Search (Pd, c, "wdir-field", "xy-wind", windfld1, 
			    SYMT_STRING);

		strcpy (windfld2, "wspd");
		pda_Search (Pd, c, "wspd-field", "xy-wind", windfld2, 
			    SYMT_STRING);

		angle = TRUE;
	}
	else 
	{
		strcpy (windfld1, "u_wind");
		pda_Search (Pd, c, "u-field", "xy-wind", windfld1, 
			    SYMT_STRING);

		strcpy (windfld2, "v_wind");
		pda_Search (Pd, c, "v-field", "xy-wind", windfld2, 
			    SYMT_STRING);

		angle = FALSE;
	}
/*
 * Parse platform and field name information. 
 */
	if ((nplat = CommaParse (platforms, pnames)) > MAX_PLAT)
	{
		msg_ELog (EF_PROBLEM, "XYWind: %s: too many platforms", c);
		return;
	}

	nxfield = CommaParse (xflds, xfnames);
	nyfield = CommaParse (yflds, yfnames);

	if ((nxfield != nplat && nxfield != 1) || 
	    (nyfield != nplat && nyfield != 1))
	{
		msg_ELog (EF_PROBLEM, 
			  "XYWind: %s: bad number of fields.", c);
		return;
	}
/*
 * Get X-Y Winds optional parameters:
 * "vec-scale" - the number of pixels long to make the barb, or the vector
 *		scaling factor
 * "data-skip" - skip every "n" data points.
 * "style" - "barb" or "vector"
 * "step" - float, the size of the color table intervale
 * "barb-type" - "m/s" or "knots"
 */
	sideAnnot = TRUE;
	pda_Search (Pd, c, "do-side-annotation", "xy-wind", 
		    (char *) &sideAnnot, SYMT_BOOL);

	skip = 5;
	pda_Search (Pd, c, "data-skip", "xy-wind", (char *) &skip, SYMT_INT);

	strcpy (style, "vector");
	pda_Search (Pd, c, "representation-style", "xy-wind", style, 
		    SYMT_STRING);

	if (strcmp (style, "vector") == 0)
	{
		vecScale = 5.0;
		pda_Search (Pd, c, "vec-scale", "xy-wind", (char *) &vecScale, 
			    SYMT_FLOAT);
	}
	else 
	{
		vecScale = 25.0;
		pda_Search (Pd, c, "barb-scale", "xy-wind", (char *) &vecScale,
			    SYMT_FLOAT);

		strcpy (barbtype, "m/s");
		pda_Search (Pd, c, "barb-type", "xy-wind", barbtype, 
			    SYMT_STRING);

		doKnot = strcmp (barbtype, "knots") == 0;
	}

	cstep = 5.0;
	pda_Search (Pd, c, "step", "xy-wind", (char *) &cstep, SYMT_FLOAT);

	scaleSpeed = 25.0;
	pda_Search (Pd, c, "scale-speed", "xy-wind", (char *) &scaleSpeed, 
		    SYMT_FLOAT);
/*
 * data types
 */
	xtype = (strcmp (xfnames[0], "time") == 0) ? 't' : 'f';
	ytype = (strcmp (yfnames[0], "time") == 0) ? 't' : 'f';
/*
 * Get our time bounds
 */
	xy_GetTimes (c, &bTimeTarget,&eTimeTarget, &bTimeOld, &eTimeOld, 
		     &dmode);

	if (! update)
	{
		bTimeOld.zt_Sec = bTimeOld.zt_MicroSec = 0;
		eTimeOld.zt_Sec = eTimeOld.zt_MicroSec = 0;
	}
/*
 * Top annotation color
 */
	xy_GetPlotColors (c, nplat, NULL, &taColor);
/*
 * Attempt to load color table 
 */
	ct_LoadTable (ctname, &colors, &ncolors);
	if (ncolors < 1)
	{
		msg_ELog (EF_PROBLEM, "XY-Contour: color table too small");
		return;
	}
/*
 * Allocate space for pointers to the data arrays and for observation info
 */
	xdata = (DataValPtr*) malloc (nplat * sizeof(DataValPtr));
	ydata = (DataValPtr*) malloc (nplat * sizeof(DataValPtr));
	w1data = (DataValPtr*) malloc (nplat * sizeof(DataValPtr));
	w2data = (DataValPtr*) malloc (nplat * sizeof(DataValPtr));
/*
 * Initialize data min/max values.
 */
	xmin.type = xmax.type = xtype;
	if (xtype == 'f')
	{
		xmin.val.f = 9.99e9;
		xmax.val.f = -9.99e9;
	}
	else
	{
		xmin.val.t = bTimeTarget;
		xmax.val.t = eTimeTarget;
	}

	ymin.type = ymax.type = ytype;
	if (ytype == 'f')
	{
		ymin.val.f = 9.99e9;
		ymax.val.f = -9.99e9;
	}
	else
	{
		ymin.val.t = bTimeTarget;
		ymax.val.t = eTimeTarget;
	}
/*
 * Loop through the platforms and get the data vectors
 */
	for (plat = 0; plat < nplat; plat++)
	{
		bool	single_obs;
		PlatformId	pid = ds_LookupPlatform (pnames[plat]);

		npts[plat] = 0;
	/*
	 * Check the platform ID
	 */
		if (pid == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, 
				  "XYGraph: %s: Bad platform '%s'", c, 
				  pnames[plat]);
			continue;
		}
	/*
	 * Figure out good data request begin and end times
	 */
		if (! xy_AvailableData (pid, &bTimeTarget, &eTimeTarget, 
					&eTimeOld, &bTimeReq, &eTimeReq))
			continue;
	/*
	 * Do we want a single "observation" worth of data?
	 */
		single_obs = (dmode == DATA_SNAPSHOT && ! update);
	/*
	 * Set the field names in the data vectors, then get the vectors
	 * filled in.
	 */
		dv[0].fname = xfnames[nxfield > 1 ? plat : 0];
		dv[1].fname = yfnames[nyfield > 1 ? plat : 0];
		dv[2].fname = windfld1;
		dv[3].fname = windfld2;

		npts[plat] = xy_GetDataVectors (pid, &bTimeReq, &eTimeReq, 
						single_obs, skip, dv, 4, NULL);
	/*
	 * Keep the pointers to the data and apply the min and max values
	 */
		xdata[plat] = dv[0].data;
		if ((xtype == 'f') && (dv[0].min.val.f < xmin.val.f))
			xmin = dv[0].min;
		if ((xtype == 'f') && (dv[0].max.val.f > xmax.val.f))
			xmax = dv[0].max;

		ydata[plat] = dv[1].data;
		if ((ytype == 'f') && (dv[1].min.val.f < ymin.val.f))
			ymin = dv[1].min;
		if ((ytype == 'f') && (dv[1].max.val.f > ymax.val.f))
			ymax = dv[1].max;

		w1data[plat] = dv[2].data;
		w2data[plat] = dv[3].data;
	}
/*
 * Get the info on whether we're using autoscaling or inverted scales
 */
	xy_GetScaleModes (c, &xauto, &xinvert, &yauto, &yinvert);
/*
 * Determine our final plot bounds.  
 */
	TriggerGlobal |= xy_DetermineBounds (c, 'x', &xmin, &xmax, xauto, 
					     xfnames[0], update);

	TriggerGlobal |= xy_DetermineBounds (c, 'y', &ymin, &ymax, yauto, 
					     yfnames[0], update);
/*
 * Plot the data.
 */
        if (! TriggerGlobal)
        {
	/*
	 * Save our bounds for posterity
	 */
	    xy_SetPrivateScale (c, &xmin, &xmax, &ymin, &ymax);
	    xy_SaveDataTimes (c, &bTimeTarget, &eTimeTarget);
	/*
	 * Make the bounds official for coordinate conversion
	 */
	    xleft = (xinvert) ? xmax : xmin;
	    xright = (xinvert) ? xmin : xmax;
	    ybottom = (yinvert) ? ymax : ymin;
	    ytop = (yinvert) ? ymin : ymax;

	    lc_SetBaseUserCoord (&xleft, &xright, &ybottom, &ytop);
	/*
	 * Axes and top annotation
	 */
	    if (! update)
	    {
		    ac_PlotAxes (c);

		    An_TopAnnot ("XYWind:", taColor);
		    An_TopAnnot (c, taColor);
	    }
	/*
	 * Get our coordinates back, with zooming applied if necessary, then
	 * set up clipping
	 */
            lc_GetUserCoord (&xleft, &xright, &ybottom, &ytop);
	    pp_Clip (&xleft, &ybottom, &xright, &ytop, FALSE);
	/*
	 * For each platform...
	 */
            for (plat = 0; plat < nplat; plat++)
            {
		if (npts[plat] == 0)
			continue;
	    /*
	     * Draw the data
	     */
	        if (strcmp(style, "barb" ) == 0)
		    pp_WindBarb (xdata[plat], ydata[plat], w1data[plat],
				 w2data[plat], npts[plat], angle, 
				 (int) vecScale, L_solid, colors, ncolors, 
				 cstep, doKnot);
	        else
	            pp_WindVector (xdata[plat], ydata[plat], w1data[plat],
				   w2data[plat], npts[plat], angle, 
				   (double) vecScale, L_solid, colors, 
				   ncolors, cstep);
	    /*
	     * Update the overlay times widget and set up for side annotation
	     */
		if (! update)
		{
		    lw_TimeStatus (c, pnames[plat], &eTimeReq);

		    if (sideAnnot)
		    {
			if (strcmp (style, "vector") == 0)
			{
			    sprintf (annotcontrol, "%5.1f%s %d %f %f %f", 
				     scaleSpeed, "m/sec", taColor, scaleSpeed, 
				     0.0, vecScale);
			    An_AddAnnotProc (An_ColorVector, c, annotcontrol,
					     strlen (annotcontrol) + 1, 30, 
					     FALSE, FALSE);
			    sprintf (annotcontrol, "%s %s %f %f", 
				     "wind-speed:m/sec", ctname, 
				     ncolors % 2 ? 
				     (ncolors - 1) * cstep * 0.5 :
				     ncolors * cstep * 0.5, cstep);
			}

			if (strcmp (style, "barb") == 0)
			{	
			    sprintf (annotcontrol, "%s %d %d", barbtype,
				     taColor,  (int) vecScale);
			    An_AddAnnotProc (An_BarbLegend, c, annotcontrol,
					     strlen (annotcontrol) + 1, 100, 
					     FALSE, FALSE);
			    sprintf (annotcontrol, "%s%s %s %f %f ",
				     "wind-speed:", barbtype, ctname,
				     ncolors % 2 ? 
				     (ncolors - 1) * cstep * 0.5 :
				     ncolors * cstep * 0.5, cstep);
			}

			An_AddAnnotProc (An_ColorBar, c, annotcontrol, 
					 strlen (annotcontrol) + 1, 75, TRUE, 
					 FALSE);

			sprintf (annotcontrol, "%s %d", pnames[plat], taColor);
			An_AddAnnotProc (An_ColorString, c, annotcontrol, 
					 strlen (annotcontrol), 25, FALSE, 
					 FALSE);
		    }
		}
            }
	/*
	 * Add a period to the top annotation
	 */
	    An_TopAnnot (".  ", taColor);

	    pp_UnClip ();
        }
/*
 * Free local memory
 */
	for (plat = 0; plat < nplat; plat++)
	{
	    if (xdata[plat])
		    free (xdata[plat]);

	    if (ydata[plat])
		    free (ydata[plat]);

	    if (w1data[plat])
		    free (w1data[plat]);

	    if (w2data[plat])
		    free (w2data[plat]);
	}

	free (xdata);
	free (ydata);
	free (w1data);
	free (w2data);
}
# endif /* C_PT_XYGRAPH */
