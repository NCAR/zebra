/*
 * XY-Wind plotting module
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

# if C_PT_XYGRAPH

# include <math.h>
# include <X11/Intrinsic.h>

# include <defs.h>
# include <pd.h>
# include <time.h>
# include <message.h>
# include <DataStore.h>
# include <met_formulas.h>
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "XYCommon.h"
# include "AxisControl.h"
# include "PlotPrim.h"

RCSID ("$Id: XYWind.c,v 1.40 2001-04-20 08:26:29 granger Exp $")

/*
 * General definitions
 */
void	xy_Wind FP ((char *, int));
static void xyw_PlotColors FP ((char *, zbool *, XColor **, int *, char *));



void
xy_Wind(c, update)
char	*c;
zbool	update;
/*
 * Draw an xy-graph on the given component
 */
{
	zbool	ok, xauto, yauto, xinvert, yinvert, angle, doKnot;
	zbool	mono;
	int	npts[MAX_PLAT], plat, nplat, alen;
	int	nxfield, nyfield, ncolors, skip, dmode;
	char	platforms[PlatformListLen], *pnames[MaxPlatforms];
	char	xflds[FieldListLen], yflds[FieldListLen];
	char	*xfnames[MaxFields], *yfnames[MaxFields];
	char	windfld1[256], windfld2[256], ctname[32], style[32];
	char	xtype, ytype, annotcontrol[80], barbtype[16];
#ifdef notdef
	char	csystem[16];
#endif
	float   cstep, scaleSpeed, vecScale = 0.01;
	XColor	*colors;
	ZebTime	bTimeTarget, eTimeTarget, eTimeReq, bTimeReq;
	ZebTime	eTimeOld,bTimeOld;
	DataValPtr	*xdata, *ydata, *w1data, *w2data;
	DataValRec	xmin, xmax, ymin, ymax;
	DataValRec	xleft, xright, ybottom, ytop;
	xyDataVector	dv[4];
	XColor	taColor;
	FieldId windfids[2];
	WindInfo windinfo;
/*
 * Get X-Y Winds Required parameters:
 * "platform","x-field", "y-field", "coords", "color-table", "org"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "x-field", NULL, xflds, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "y-field", NULL, yflds, SYMT_STRING);
#ifdef notdef
	ok &= pda_ReqSearch (Pd, c, "coords", "xy-wind", csystem, SYMT_STRING);
#endif
/*
 * If something vital is missing bail.
 */
	if (! ok)
		return;
/*
 * Figure out color stuff.
 */
	xyw_PlotColors (c, &mono, &colors, &ncolors, ctname);
/*
 * See FindWindsFields below for wind field derivations.  All wind coords
 * end up cartesian.
 */
	angle = 0;
#ifdef notdef
/*
 * Winds coordinate system.  This should be automatic.
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
#endif
/*
 * Parse platform and field name information. 
 */
	if ((nplat = CommaParse (platforms, pnames)) > MAX_PLAT)
	{
		msg_ELog (EF_PROBLEM, "XYWind: %s: too many platforms", c);
		return;
	}

	nxfield = ParseFieldList (xflds, xfnames);
	nyfield = ParseFieldList (yflds, yfnames);

	if (((nxfield != 1) && (nplat != 1) && (nplat != nxfield)) ||
	    ((nyfield != 1) && (nplat != 1) && (nplat != nyfield)) ||
	    (nxfield != nyfield))
	{
		msg_ELog (EF_PROBLEM, "XYWind: %s: bad number of fields", c);
		return;
	}
	else if ((nplat == 1) && (nxfield > MAX_PLAT))
	{
		msg_ELog (EF_PROBLEM, "XYWind: %s: too many fields", c);
		return;
	}
/*
 * So if we have one platform and many fields, just copy the platform name
 * into pnames as if it had come that way from the plot description
 */
	if ((nplat == 1) && (nxfield > 1))
	{
		for (plat = 1; plat < nxfield; ++plat)
			pnames[plat] = pnames[0];
		nplat = nxfield;
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
	skip = 5;
	pda_Search (Pd, c, "data-skip", "xy-wind", (char *) &skip, SYMT_INT);

	strcpy (style, "vector");
	pda_Search (Pd, c, "representation-style", "xy-wind", style, 
		    SYMT_STRING);

	/* The default is vector if no setting or setting is incorrect */
	if (strcmp (style, "barb") != 0)
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
	An_GetTopParams (&taColor, 0);
	xy_GetPlotColors (c, nplat, NULL);
/*
 * Allocate space for pointers to the data arrays and for observation info.
 * Also zero them out or else we get undesirable free() calls below.  I speak
 * from experience.
 */
	alen = nplat*sizeof (DataValPtr);
	xdata = (DataValPtr*) malloc (alen);  memset ((void*)xdata, 0, alen);
	ydata = (DataValPtr*) malloc (alen);  memset ((void*)ydata, 0, alen);
	w1data = (DataValPtr*) malloc (alen); memset ((void*)w1data, 0, alen);
	w2data = (DataValPtr*) malloc (alen); memset ((void*)w2data, 0, alen);
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
		zbool	single_obs;
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
	 * Look for explicit or derivable winds fields.
	 */
		FindWindsFields (c, pid, &eTimeReq, windfids, &windinfo);
		strcpy (windfld1, F_GetFullName (windfids[0]));
		strcpy (windfld2, F_GetFullName (windfids[1]));
	/*
	 * Set the field names in the data vectors, then get the vectors
	 * filled in.
	 */
		dv[0].fname = xfnames[nxfield > 1 ? plat : 0];
		dv[1].fname = yfnames[nyfield > 1 ? plat : 0];
		dv[2].fname = windfld1;
		dv[3].fname = windfld2;

		npts[plat] = xy_GetDataVectors (pid, &bTimeReq, &eTimeReq, 
					single_obs, skip, dv, 4, NULL, c);
	    /*
	     * Update the overlay times widget and set up for side annotation
	     * (Do it here since eTimeReq may change from platform to platform)
	     */
		if (npts[plat] > 0 && ! update)
		{
		    ot_AddStatusLine (c, pnames[plat], "(winds)", &eTimeReq);

		    if (An_SaShow (c, "xy-wind"))
		    {
			if (strcmp (style, "vector") == 0)
			{
			    sprintf (annotcontrol, "%s|%li|%f|%f|%f", "m/s", 
				     taColor.pixel, scaleSpeed, 0.0, vecScale);
			    An_AddAnnotProc (An_ColorVector, c, annotcontrol,
					     strlen (annotcontrol) + 1, 30, 
					     FALSE, FALSE);
			    if (! mono)
				    sprintf (annotcontrol, "%s|%s|%f|%f", 
					     "wind-speed: m/s", ctname, 
					     cstep * ncolors / 2.0, cstep);
			}
			else if (strcmp (style, "barb") == 0)
			{	
			    sprintf (annotcontrol, "%s|%li|%d", barbtype,
				     taColor.pixel,  (int) vecScale);
			    An_AddAnnotProc (An_BarbLegend, c, annotcontrol,
					     strlen (annotcontrol) + 1, 100, 
					     FALSE, FALSE);
			    if (! mono)
				    sprintf (annotcontrol, "%s%s|%s|%f|%f ",
					     "wind-speed: ", barbtype, ctname,
					     cstep * ncolors / 2.0, cstep);
			}

			if (! mono)
				An_AddAnnotProc (An_ColorBar, c, annotcontrol, 
					 strlen (annotcontrol) + 1, 75, TRUE, 
					 FALSE);

			sprintf(annotcontrol, "%s|%li", pnames[plat], 
				taColor.pixel);
			An_AddAnnotProc (An_ColorString, c, annotcontrol, 
					 strlen (annotcontrol), 25, FALSE, 
					 FALSE);
		    }
		}
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
	/*
	 * Do any necessary wind field conversion.  We shouldn't have
	 * any bad values by now, so pass an arbitrary bad value.  All
	 * platforms should end up with cartesian winds.
	 */
		if (windinfo.wi_polar)
		{
		    int wi;
		    msg_ELog (EF_DEBUG, "xywind: polar winds"
			      "(%s,%s) for platform %s being converted",
			      windfld1, windfld2, pnames[plat]);
		    for (wi = 0; wi < npts[plat]; ++wi)
		    {
			GetWindData (&windinfo, &(dv[2].data[wi].val.f),
				     &(dv[3].data[wi].val.f), -99999.0);
		    }
		}
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

		    An_TopAnnot ("XYWind:");
		    An_TopAnnot (c);
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
            }
	/*
	 * Add a period to the top annotation
	 */
	    An_TopAnnot (".  ");

	    pp_UnClip ();
        }
	ResetGC ();
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




static void
xyw_PlotColors (c, mono, colors, ncolors, ctname)
char *c;
zbool *mono;
XColor **colors;
int *ncolors;
char *ctname;
/*
 * Figure out how we'll be coloring the arrows.
 */
{
	char color[32];
	static XColor monocolor;	/* XXX */
/*
 * See if they want mono or not.
 */
	if (! pda_Search (Pd, c, "color-mono", "xy-wind", (char*) mono, 
			  SYMT_BOOL))
		*mono = FALSE;
/*
 * If they don't want mono, look for a color table.  If we encounter
 * trouble with that, they get mono anyway.
 */
	if (! *mono)
	{
		if (! pda_ReqSearch (Pd, c, "color-table", "xy-wind", ctname,
				     SYMT_STRING))
		{
			msg_ELog (EF_PROBLEM, 
				  "xywind: color-table parameter missing");
			*mono = TRUE;
		}
		else if (! ct_LoadTable (ctname, colors, ncolors))
		{
			msg_ELog (EF_PROBLEM, 
				  "xywind: unloadable color table %s",
				  ctname);
			*mono = TRUE;
		}
	}
/*
 * OK, see what our color situation is now.
 */
	if (*mono)
	{
		strcpy (color, "white");
		pda_Search (Pd, c, "color", "xy-wind", color, SYMT_STRING);
		if (! ct_GetColorByName (color, &monocolor))
			ct_GetColorByName ("white", &monocolor);
		*colors = &monocolor; /* XXX */
		*ncolors = 1;
	}
}










# endif /* C_PT_XYGRAPH */
