/*
 * XY-Observation plotting module
 */
static char *rcsid = "$Id: XYObservation.c,v 1.8 1993-12-01 17:32:27 burghart Exp $";
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
# include "ui_date.h"
# include "DrawText.h"
# include "PlotPrim.h"

/*
 * Enum to tell how we anchor z values
 */
typedef enum
{
	AnchorBase, AnchorMean, AnchorTop
} ZAnchorType;


/*
 * Our routines.
 */
void xy_Observation FP ((char *, int));




void
xy_Observation(c, update)
char	*c;
bool	update;
/*
 * Draw an xy-graph on the given component
 */
{
	bool	ok, doLine, annotLoc, sideAnnot;
	bool	xauto, yauto, xinvert, yinvert;
	int	plat, nplat, npts[MAX_PLAT], dmode, ob, nobs;
	int	nxfield, nyfield, nzfield, obsStart, obsEnd, obsLen;
	char	platforms[MAX_PLAT_LEN], *pnames[MAX_PLAT];
	char	xflds[MAX_PLAT_LEN], yflds[MAX_PLAT_LEN], zflds[MAX_PLAT_LEN];
	char	*xfnames[MAX_PLAT], *yfnames[MAX_PLAT], *zfnames[MAX_PLAT];
	char	label[40], style[20], zJustify[10], annotcontrol[80];
	char	xtype, ytype, ztype;
	ZebTime	eTimeTarget, bTimeTarget, bTimeOld, eTimeOld;
	ZebTime	eTimeReq, bTimeReq;
	float	zstart, xfrac, zScale, fscale;
	Pixel	taColor, *lcolor;
	ZAnchorType	zanchor;
	DataValPtr	*xdata, *ydata, *zdata;
	DataValRec	xmin, xmax, ymin, ymax, zmin[MAX_PLAT], zmax[MAX_PLAT];
	DataValRec	xleft, xright, ybottom, ytop, zleft, zright;
	DataValRec	xpos[2], ypos[2];
	xyDataVector	dv[3];
	xyObsInfo	*dvObsInfo;
/*
 * Get X-Y Observation Required parameters:
 * "platform","x-field", "y-field", "z-field"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "x-field", NULL, xflds, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "y-field", NULL, yflds, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "z-field", NULL, zflds, SYMT_STRING);

	if (! ok)
		return;
/*
 * Parse platform and field name information. 
 */
	if ((nplat = CommaParse (platforms, pnames)) > MAX_PLAT)
	{
		msg_ELog (EF_PROBLEM, "XYObs: %s: too many platforms", c);
		return;
	}

	nxfield = CommaParse (xflds, xfnames);
	nyfield = CommaParse (yflds, yfnames);
	nzfield = CommaParse (zflds, zfnames);

	if ((nxfield != nplat && nxfield != 1) || 
	    (nyfield != nplat && nyfield != 1) || 
	    (nzfield != nplat && nzfield != 1))
	{
		msg_ELog (EF_PROBLEM, "XYObs: %s: bad number of fields.", c);
		return;
	}
/*
 *  Get optional "obs" parameters.
 *  "style" - "point" , "line", "cross" or "xmark"
 */
	strcpy (style, "line");
	pda_Search (Pd, c, "representation-style", "xy-obs", style, 
		    SYMT_STRING);

	sideAnnot = TRUE;
	pda_Search (Pd, c, "do-side-annotation", "xy-obs", (char *) &sideAnnot,
		    SYMT_BOOL);

	zScale = 0.001;
	pda_Search (Pd, c, "z-scale", "xy-obs", (char *) &zScale, SYMT_FLOAT);

	strcpy (zJustify, "mid");
	pda_Search (Pd, c, "z-justify", "xy-obs", zJustify, SYMT_STRING);

	annotLoc = TRUE;
	pda_Search (Pd, c, "annotate-loc", "xy-obs", (char *) &annotLoc, 
		    SYMT_BOOL);
/*
 * Convert the z-justify string into one of our anchor types
 */
	if (! strcmp (zJustify, "mid"))
		zanchor = AnchorMean;
	else if (! strcmp (zJustify, "base"))
		zanchor = AnchorBase;
	else if (! strcmp (zJustify, "top"))
		zanchor = AnchorTop;
	else
	{
		msg_ELog (EF_PROBLEM, 
			  "XYObs: %s: bad z-justify '%s', using 'mid'", c,
			  zJustify);
		zanchor = AnchorMean;
	}
/*
 * Get the icon to use for non-line representations.  The old special cases
 * of "point", "cross", and "xmark" just use an icon of the same name.
 * Otherwise, we look for the "point-icon" parameter in the plot description
 * (defaulting to "cross").
 */
	doLine = ! strcmp (style, "line");

	if (! doLine && strcmp (style, "point") != 0 && 
	    strcmp (style, "cross") != 0 && strcmp (style, "xmark") != 0)
	{
		if (strcmp (style, "icon") != 0)
			msg_ELog (EF_INFO, 
				  "XYGraph: %s: bad style '%s', using 'icon'",
				  c, style);

		strcpy (style, "cross");
		pda_Search (Pd, c, "point-icon", "xy-obs", style, SYMT_STRING);
	}
/*
 * data types
 */
	xtype = (strcmp (xfnames[0], "time") == 0) ? 't' : 'f';
	ytype = (strcmp (yfnames[0], "time") == 0) ? 't' : 'f';
	ztype = (strcmp (zfnames[0], "time") == 0) ? 't' : 'f';
	if (ztype == 't')
	{
		msg_ELog ("XYObs: %s: Cannot use time for z-field", c);
		return;
	}
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
 * Allocate space for pointers to the data arrays and for observation info
 */
	xdata = (DataValPtr *) calloc (nplat, sizeof (DataValPtr));
	ydata = (DataValPtr *) calloc (nplat, sizeof (DataValPtr));
	zdata = (DataValPtr *) calloc (nplat, sizeof (DataValPtr));

	dvObsInfo = (xyObsInfo *) malloc (nplat * sizeof (xyObsInfo));
/*
 * Colors
 */
	lcolor = (Pixel *) malloc (nplat * sizeof (Pixel));
	xy_GetPlotColors (c, nplat, lcolor, &taColor);
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
		dv[2].fname = zfnames[nzfield > 1 ? plat : 0];

		npts[plat] = xy_GetDataVectors (pid, &bTimeReq, &eTimeReq, 
						single_obs, 1, dv, 3, 
						dvObsInfo + plat);
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

		zdata[plat] = dv[2].data;
		zmin[plat] = dv[2].min;
		zmax[plat] = dv[2].max;
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

		    An_TopAnnot ("XYObservation:", taColor);
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
	     * Update the overlay times widget and set up for side annotation
	     */
		if (! update)
		{
		    lw_TimeStatus (c, pnames[plat], &eTimeReq);

		    if (sideAnnot)
		    {
			sprintf (annotcontrol, "%s-%s:%s %d", pnames[plat], 
				 xfnames[nxfield > 1 ? plat : 0],
				 yfnames[nyfield > 1 ? plat : 0], 
				 lcolor[plat]);
			An_AddAnnotProc (An_ColorString, c, annotcontrol,
					 strlen (annotcontrol) + 1, 25, FALSE,
					 FALSE);

			TC_EncodeTime (&eTimeReq, TC_Full, label);
			sprintf (annotcontrol, "   %s %d", label, 
				 lcolor[plat]);
			An_AddAnnotProc (An_ColorString, c, annotcontrol,
					 strlen (annotcontrol) + 1, 25, FALSE,
					 FALSE);

			sprintf (annotcontrol, "%f %d %d %s", zScale, 
				 F_PIX_WIDTH, lcolor[plat], 
				 zfnames[nzfield > 1 ? plat : 0]);
			An_AddAnnotProc (An_ColorScale, c,  annotcontrol, 
					 strlen (annotcontrol) + 1, 35, FALSE,
					 FALSE);
		    }
		}
	    /*
	     * Loop through the observations
	     */
		nobs = dvObsInfo[plat].nobs;

		for (ob = 0; ob < nobs; ob++)
		{
			obsStart = dvObsInfo[plat].obsndx[ob];
			obsEnd = (ob < nobs - 1) ? 
				dvObsInfo[plat].obsndx[ob + 1] : npts[plat];

			if ((obsLen = obsEnd - obsStart) <= 0)
				continue;
		/*
		 * starting x location as a fraction of the data space
		 */
			switch (xtype)
			{
			    case 't':
				xfrac = (float)
				   (xdata[plat][obsStart].val.t.zt_Sec -
				    xleft.val.t.zt_Sec) /
				   (xright.val.t.zt_Sec - xleft.val.t.zt_Sec);
			    break;
			    case 'f':
				xfrac = (xdata[plat][obsStart].val.f - 
					  xleft.val.f) /
					 (xright.val.f - xleft.val.f);
			    break;
			}
		/*
		 * Find the z value that should map to 'xfrac'
		 */
			if (zanchor == AnchorMean)
			    zstart = zmin[plat].val.f + 
				(zmax[plat].val.f - zmin[plat].val.f) * 0.5;
			else if (zanchor == AnchorBase)
			    zstart = zdata[plat][obsStart].val.f;
			else
			    zstart = zdata[plat][obsEnd - 1].val.f;
		/*
		 * Come up with the z data limits for the entire data space
		 */
			zleft.type = zright.type = 'f';
			zleft.val.f = zstart - (xfrac / zScale);
			zright.val.f = zleft.val.f + 1.0 / zScale;
	        /*
		 * Location annotation
		 */
			if (annotLoc && 
			    zdata[plat][obsStart].val.f > zleft.val.f)
			{
			    lc_SetBaseUserCoord (&xleft, &xright, &ybottom,
						 &ytop);
			    lc_GetUserCoord (&xleft, &xright, &ybottom, &ytop);
			    pp_Clip (&xleft, &ybottom, &xright, &ytop, FALSE);
			/*
			 * Top to bottom line
			 */
			    xpos[0] = xdata[plat][obsStart];
			    xpos[1] = xdata[plat][obsStart];
			    ypos[0] = ybottom;
			    ypos[1] = ytop;
			    pp_Pline (xpos, ypos, 2, L_dashed, taColor);

			    pp_UnClip ();
			/*
			 * Numeric label
			 */
			    XSetForeground (Disp, Gcontext, taColor);
			    sprintf (label, "%0.2f", 
				     zdata[plat][obsStart].val.f);

			    fscale = 0.025;
			    pda_Search (Pd, c, "axis-t-font-scale", "xy", 
					(char *)&(fscale), SYMT_FLOAT);

			    DrawText (Graphics, GWFrame(Graphics), Gcontext, 
				      devX (xpos), devY (&ytop), label, 30.0, 
				      fscale, JustifyLeft, JustifyBottom);
			}
		/*
		 * Now draw the data
		 */
			lc_SetBaseUserCoord (&zleft, &zright, &ybottom, &ytop);
			pp_Clip (&zleft, &ybottom, &zright, &ytop, FALSE);

			if (doLine)
				pp_Pline (zdata[plat] + obsStart,
					  ydata[plat] + obsStart,
					  obsLen, L_solid, lcolor[plat]);
			else
				pp_Icons (zdata[plat] + obsStart,
					  ydata[plat] + obsStart, 
					  obsLen, style, lcolor[plat], c,
					  pnames[plat]);
		}
	    }

	    pp_UnClip ();
	/*
	 * Add a period to the top annotation
	 */
	    if (! update)
		    An_TopAnnot (".  ", taColor);
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

		if (zdata[plat])
			free (zdata[plat]);
	}

	free (xdata);
	free (ydata);
	free (zdata);
	free (lcolor);
	free (dvObsInfo);
}
# endif /* C_PT_XYGRAPH */
