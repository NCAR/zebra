/*
 * XY-Graph plotting module
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
# include "DrawText.h"

RCSID ("$Id: XYGraph.c,v 1.41 2001-04-20 08:26:28 granger Exp $")

/*
 * Prototypes
 */
void 		xy_Graph FP ((char *, int));
static void	XYG_DoSideAnnotation FP ((char*, char*, char*, Pixel, char*,
					  char*, ZebTime*));
static int xy_CheckFieldPlat FP ((int, int, int *, char **));
static void xy_GetGraphParams FP ((char *, char *, int *));


void
xy_Graph (c, update)
char	*c;
zbool	update;
/*
 * Draw an xy graph based on the given component
 */
{
	zbool	ok;
	int	doLine;
	int	plat, nplat, npts[MAX_PLAT];
	int	nxfield, nyfield, dmode;
	char	platforms[PlatformListLen], *pnames[MaxPlatforms];
	char	xflds[FieldListLen], yflds[FieldListLen];
	char	*xfnames[MaxFields], *yfnames[MaxFields];
	char	style[24], xtype, ytype;
	Pixel	*lcolor;
	ZebTime	eTimeTarget, bTimeTarget, bTimeOld, eTimeOld;
	ZebTime	eTimeReq, bTimeReq;
	zbool	xauto, yauto, xinvert, yinvert;
	PlatformId	pid;
	xyDataVector	dv[2];
	DataValPtr	*xdata, *ydata;
	DataValRec	xmin, xmax, ymin, ymax;
	DataValRec	xleft, xright, ybottom, ytop;
/*
 * Get X-Y Graph Required parameters:
 * "platform", "x-field", "y-field"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "x-field", NULL, xflds, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "y-field", NULL, yflds, SYMT_STRING);

	if (! ok)
		return;
/*
 * Parse platform and field name information. 
 */
	if ((nplat = CommaParse (platforms, pnames)) > MAX_PLAT)
	{
		msg_ELog (EF_PROBLEM, "XYGraph: %s: too many platforms", c);
		return;
	}

	nxfield = ParseFieldList (xflds, xfnames);
	nyfield = ParseFieldList (yflds, yfnames);
/*
 * Check out field/plat combinations.
 */
	if (! xy_CheckFieldPlat (nxfield, nyfield, &nplat, pnames))
		return;
/*
 * Get basic graphics parameters.
 */
	xy_GetGraphParams (c, style, &doLine);
/*
 * data types ('t'ime or 'f'loat)
 */
	xtype = (strcmp (xfnames[0], "time") == 0) ? 't' : 'f';
	ytype = (strcmp (yfnames[0], "time") == 0) ? 't' : 'f';
/*
 * Get our time bounds, scratching the old times if we're doing a global plot
 */
	xy_GetTimes (c, &bTimeTarget, &eTimeTarget, &bTimeOld, &eTimeOld, 
		     &dmode);

	if (! update)
	{
		bTimeOld.zt_Sec = bTimeOld.zt_MicroSec = 0;
		eTimeOld.zt_Sec = eTimeOld.zt_MicroSec = 0;
	}
/*
 * Allocate space for pointers to the data arrays
 */
	xdata = (DataValPtr*) calloc (nplat, sizeof (DataValPtr));
	ydata = (DataValPtr*) calloc (nplat, sizeof (DataValPtr));
/*
 * Colors
 */
	lcolor = (Pixel *) malloc (nplat * sizeof (Pixel));
	xy_GetPlotColors (c, nplat, lcolor);
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

		pid = ds_LookupPlatform (pnames[plat]);

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

		npts[plat] = xy_GetDataVectors (pid, &bTimeReq, &eTimeReq, 
						single_obs, 0, dv, 2, NULL, c);
	/*
	 * Update the overlay times widget and set up for side annotation
	 * (Do it here since eTimeReq may change from platform to platform)
	 */
		if (npts[plat] > 0 && ! update)
		{
			char	fstring[40];
			char	dimns[40];

			sprintf (fstring, "%s/%s", 
				 xfnames[nxfield > 1 ? plat : 0],
				 yfnames[nyfield > 1 ? plat : 0]);
			if (pda_Search(Pd, c, "dimensions", NULL, dimns, 
				       SYMT_STRING))
			    sprintf (fstring+strlen(fstring), "(%s)", dimns);

			ot_AddStatusLine (c, pnames[plat], fstring, &eTimeReq);

			if (An_SaShow (c, "xy-simple"))
			{
				XYG_DoSideAnnotation (c, style, pnames[plat],
					      lcolor[plat],
					      xfnames[nxfield > 1 ? plat : 0],
					      yfnames[nyfield > 1 ? plat : 0],
					      &eTimeReq);
			}
		}
	/*
	 * If we got some data, tweak min and max.
	 */
		if (npts[plat] > 0)
		{
			if ((xtype == 'f') && (dv[0].min.val.f < xmin.val.f))
				xmin = dv[0].min;
			if ((xtype == 'f') && (dv[0].max.val.f > xmax.val.f))
				xmax = dv[0].max;
			if ((ytype == 'f') && (dv[1].min.val.f < ymin.val.f))
				ymin = dv[1].min;
			if ((ytype == 'f') && (dv[1].max.val.f > ymax.val.f))
				ymax = dv[1].max;
		}
	/*
	 * Keep the pointers to the data, even if there aren't any...
	 */
		xdata[plat] = dv[0].data;
		ydata[plat] = dv[1].data;
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
	    xy_SaveDataTimes (c, &bTimeReq, &eTimeReq);
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

		    An_TopAnnot ("XY Graph:");
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
		pp_SetLWidth (c, "line-width", pnames[plat], 0);
		if (doLine)
			pp_Pline (xdata[plat], ydata[plat], npts[plat], 
				  L_solid, lcolor[plat]);
		else
			pp_Icons (xdata[plat], ydata[plat], npts[plat], 
				  style, lcolor[plat], c, pnames[plat]);
	    }
	/*
	 * Unclip and add a period to the top annotation.
	 */
	    pp_UnClip ();

	    if (! update)
		    An_TopAnnot (".  ");
	}
	ResetGC();
/*
 * Free local memory
 */
	for (plat = 0; plat < nplat; plat++)
	{
	    if (xdata[plat])
		    free (xdata[plat]); 

	    if (ydata[plat])
		    free (ydata[plat]);
	}
	free (lcolor);
	free (xdata);
	free (ydata); 
}




static void
XYG_DoSideAnnotation (c, style, plat, color, xfield, yfield, time)
char *c, *style, *plat, *xfield, *yfield;
Pixel color;
ZebTime *time;
{
	int dotime = 0, nline = 2;
	char label[200], timelabel[32];
	char dimns[200];
	float scale;
/*
 * Throw together the basic info.
 */
	An_GetSideParams (c, &scale, NULL);
	sprintf(label, "%s|%li|%s|%s|%s", style, color, plat, xfield, yfield);
	if (pda_Search (Pd, c, "dimensions", NULL, dimns, SYMT_STRING))
		sprintf (label+strlen(label), "(%s)", dimns);
/*
 * If they want the time put that in too.
 */
	if (! pda_Search (Pd, c, "time-annot", "xy", (char *) &dotime,
			  SYMT_BOOL))
		dotime = TRUE;
	if (dotime)
	{
		strcat (label, "|");
		TC_EncodeTime (time, TC_DateOnly, timelabel);
		strcat (label, timelabel);
		strcat (label, "| ");
		TC_EncodeTime (time, TC_TimeOnly, timelabel);
		strcat (label, timelabel);
		nline += 2;
	}
/*
 * Store up the annotation request and we're set.
 */
	An_AddAnnotProc (An_XYGString, c, label, strlen (label) + 1,
			 DT_ApproxHeight (Graphics, scale, nline),
			 FALSE, FALSE);
}




static int
xy_CheckFieldPlat (nxfield, nyfield, np, pnames)
int nxfield, nyfield, *np;
char **pnames;
/*
 * Check out the platform/field combinations and see that we can
 * handle them.
 */
{
	int plat, nplat = *np;
/*
 * Accept a few possible combinations:
 * 	One field and many platforms plots that field for each platform
 *	Many fields and one platforms plots each field for that platform
 *	Many fields and many platforms implies number of each must be equal
 */
	if (((nxfield != 1) && (nplat != 1) && (nplat != nxfield)) ||
	    ((nyfield != 1) && (nplat != 1) && (nplat != nyfield)))
	{
		msg_ELog (EF_PROBLEM, "XYGraph: bad number of fields");
		return (FALSE);
	}
	else if (nxfield != nyfield)
	{
		msg_ELog (EF_PROBLEM, 
			  "XYGraph: number of x and y fields %s",
			  "must be equal");
		return (FALSE);
	}
	else if ((nplat == 1) && (nxfield > MAX_PLAT))
	{
		msg_ELog (EF_PROBLEM, "XYGraph: too many fields");
		return (FALSE);
	}
/*
 * So if we have one platform and many fields, just copy the platform name
 * into pnames as if it had come that way from the plot description
 */
	if ((nplat == 1) && (nxfield > 1))
	{
		for (plat = 1; plat < nxfield; ++plat)
			pnames[plat] = pnames[0];
		*np = nxfield;
	}
	return (TRUE);
}





static void
xy_GetGraphParams (c, style, doLine)
char *c, *style;
int *doLine;
/*
 * Get basic graphics parameters.
 */
{
/*
 *  Get optional "simple" parameters.
 *	representation-style:	"point", "line", "cross", "xmark", or "icon"
 */
	strcpy (style, "line");
	pda_Search (Pd, c, "representation-style", "xy-simple", style, 
		    SYMT_STRING);
/*
 * Get the icon to use for non-line representations.  The old special cases
 * of "point", "cross", and "xmark" just use an icon of the same name.
 * Otherwise, we look for the "point-icon" parameter in the plot description
 * (defaulting to "cross").
 */
	*doLine = ! strcmp (style, "line");

	if (! *doLine && strcmp (style, "point") != 0 && 
	    strcmp (style, "cross") != 0 && strcmp (style, "xmark") != 0)
	{
		if (strcmp (style, "icon") != 0)
			msg_ELog (EF_INFO, 
				  "XYGraph: %s: bad style '%s', using 'icon'",
				  c, style);

		strcpy (style, "cross");
		pda_Search (Pd, c, "point-icon", "xy-simple", style, 
			    SYMT_STRING);
	}
}
# endif /* C_PT_XYGRAPH */
