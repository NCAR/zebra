/*
 * XY-Contour plotting module
 */
/*		Copyright (C) 1993 by UCAR
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
# include <math.h>
# include <X11/Intrinsic.h>

# include <defs.h>
# include <pd.h>
# include <GraphicsW.h>
# include <time.h>
# include <message.h>
# include <DataStore.h>
# include <met_formulas.h>
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "XYCommon.h"
# include "AxisControl.h"
# include "rg_status.h"
# include "Contour.h"

RCSID("$Id: XYContour.c,v 1.45 2001-06-19 23:48:31 granger Exp $")

# define GRID(g,i,j,ydim)   (g[((i) * (ydim)) + (j)])

/*
 * General definitions
 */




typedef struct GridInfo {
    float	*grid;
    float	*scratch;
    int		xdim, ydim;
    char	component[80];
    struct GridInfo	*next;
} GridInfoRec, *GridInfoPtr;

static GridInfoPtr GridInfoList = NULL;

/*
 * Prototypes
 */
static int xyc_PlatsAndFields FP ((char *, char *, int *, char **, int *,
		char **, int *, char **, int *, char **));
static void xyc_DoColors FP ((char *, int, char *, XColor **, int *));

float	*xy_RGridit FP ((char *, DataValPtr, DataValPtr, int, DataValPtr, 
			 DataValPtr, int, DataValPtr, DataValPtr, DataValPtr, 
			 int, double));
static void gridRandomData FP ((GridInfoPtr, DataValPtr, DataValPtr,
		DataValPtr, int, DataValPtr, DataValPtr, DataValPtr,
		DataValPtr, double));
float	*xy_InterpolateLinearOnY FP ((GridInfoPtr, double));
GridInfoPtr 	getGrid FP ((char *, int, int, double));
static void     xyc_FixYRes FP ((int *, DataValPtr, int, double, double));
static float    gridStep FP ((int, DataValPtr, DataValPtr));



void
xy_Contour (c, update)
char	*c;
zbool	update;
/*
 * Draw an xy-graph on the given component
 */
{
	zbool	fill;
	int	npts[MAX_PLAT], plat, nplat, ncolors, dmode;
	int	dolabel, labelblank, linewidth = 0, nxfield, nyfield, nzfield;
	int	xgridres, ygridres = 0;
	int	oneplot;
	float	zstep, ccenter, *datagrid = NULL, widen = 0.0;
	char	platforms[PlatformListLen], *pnames[MaxPlatforms];
	char	*xfnames[MaxFields], *yfnames[MaxFields], *zfnames[MaxFields];
	char	gridtype[20], ctname[24], style[20], annotcontrol[80];
	char	xtype, ytype, ztype;
	zbool	xauto, yauto, xinvert, yinvert, zauto, zinvert;
	XColor	*colors, white;
	ZebTime	bTimeTarget, eTimeTarget, bTimeReq, eTimeReq;
	ZebTime	eTimeOld, bTimeOld;
	xyDataVector	dv[3];
	DataValPtr	*xdata, *ydata, *zdata;
	DataValRec	xmin, xmax, ymin, ymax, zmin[MAX_PLAT], zmax[MAX_PLAT];
	DataValRec	xleft, xright, ybottom, ytop;
	XRectangle	clip;
	GridInfoPtr	ginfo;
/*
 * An update contour plot?  Yeah, right.
 */
	if (update)
	{
	    TriggerGlobal = TRUE;
	    return;
	}
/*
 * Figure out what they want to look at.
 */
	if (! xyc_PlatsAndFields (c, platforms, &nplat, pnames, &nxfield,
			xfnames, &nyfield, yfnames, &nzfield, zfnames))
		return;
/*
 * Get X-Y Contour optional parameters:
 * "style" - "line" or "filled"
 * "x-grid", "y-grid" - the number of grid cells to use in the X and Y 
 *			dimensions
 * "contour-grid-type" - method of interpolating gridded data
 */
/*
 * New scheme for contour styles: take the old "representation-style"
 * if it's there (backwards compatibility); otherwise look at the
 * representation directly, as is done in CAPland.
 */
	if (pda_Search (Pd, c, "representation-style", "xy-contour",
		    (char *) style, SYMT_STRING))
		fill = ! strcmp (style, "filled");
	else
	{
		pd_Retrieve (Pd, c, "representation", style, SYMT_STRING);
		fill = ! strcmp (style, "filled-contour");
	}
/*
 * Figure out our colors.
 */
	xyc_DoColors (c, fill, ctname, &colors, &ncolors);
/*
 * Gridding (for now)
 */
	xgridres = 25;
	pda_Search (Pd, c, "x-grid", "xy-contour", (char *) &xgridres, 
		    SYMT_INT);

	ygridres = 25;
	pda_Search (Pd, c, "y-grid", "xy-contour", (char *) &ygridres, 
		    SYMT_INT);

	strcpy (gridtype, "raw");
	pda_Search (Pd, c, "grid-method", "xy-contour", gridtype, SYMT_STRING);
	pda_Search (Pd, c, "line-width", "xy-contour", (char *) &linewidth,
			SYMT_INT);
/*
 * Contour labels and whether we blank the area beneath them
 */
	dolabel = TRUE;
	pda_Search (Pd, c, "do-labels", "contour", (char *) &dolabel, 
		    SYMT_BOOL);

	labelblank = TRUE;
	pda_Search (Pd, c, "label-blanking", "contour", (char *) &labelblank,
		    SYMT_BOOL);
	dt_SetBlankLabel (labelblank);
/*
 * Data types
 */
	xtype = (strcmp (xfnames[0], "time")) ? 'f' : 't';
	ytype = (strcmp (yfnames[0], "time")) ? 'f' : 't';
	ztype = (strcmp (zfnames[0], "time")) ? 'f' : 't';

	if (ztype == 't')
	{
		msg_ELog (EF_PROBLEM, 
			  "XYContour: %s: Can't use time for z-field", c);
		return;
	}
/*
 * Get our time bounds
 */
        xy_GetTimes (c, &bTimeTarget, &eTimeTarget, &bTimeOld, &eTimeOld, 
		     &dmode);

	bTimeOld.zt_Sec = bTimeOld.zt_MicroSec = 0;
	eTimeOld.zt_Sec = eTimeOld.zt_MicroSec = 0;
/*
 * Allocate memory for pointers to the data arrays
 */
	xdata = (DataValPtr *) calloc (nplat, sizeof (DataValPtr));
	ydata = (DataValPtr *) calloc (nplat, sizeof (DataValPtr));
	zdata = (DataValPtr *) calloc (nplat, sizeof (DataValPtr));
/*
 * Get top annotation color
 */
	xy_GetPlotColors (c, nplat, NULL);
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
 * Think about maybe widening the time period for the purposes of fetching
 * the data.
 */
	if (pda_Search (Pd, c, "widen-time", "xy-contour", (char *) &widen,
			SYMT_FLOAT) && widen > 0)
	{
		int tp = eTimeTarget.zt_Sec - bTimeTarget.zt_Sec;
		int adjust = (int) (tp * widen/100.0);
		bTimeTarget.zt_Sec -= adjust;
		eTimeTarget.zt_Sec += adjust;
		msg_ELog (EF_INFO, "Widening by %d", adjust);
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
	 * Get and check the platform ID
	 */
		if (pid == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, 
				  "XYContour: %s: Bad platform '%s'", c,
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
						single_obs, 0, dv, 3, NULL, c);
	/*
	 * Update the overlay times widget 
	 */
		if (npts[plat] > 0)
			ot_AddStatusLine (c, pnames[plat], 
				  (nzfield > 1) ? zfnames[plat] : zfnames[0],
				  &eTimeReq);
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
 * Do it all as one plot if either:
 *	o  we have just one platform
 *		OR
 *	o  there are multiple platforms, but exactly one each of 
 *	   x-field, y-field, and z-field
 */
	oneplot = (nplat == 1) || 
	    (nxfield == 1 && nyfield == 1 && nzfield == 1);
/*
 * Gather the data together if we're doing this as one plot.  For each
 * of the platforms after the first, we append the data to those from the
 * first platform, i.e., everything ends up in xdata[0], ydata[0], and 
 * zdata[0].
 */
	if (oneplot && nplat > 1)
	{
	    int	allpts = 0;

	    for (plat = 0; plat < nplat; plat++)
		allpts += npts[plat];
	    
	    xdata[0] = (DataValPtr) realloc (xdata[0], 
					     allpts * sizeof (DataValRec));
	    ydata[0] = (DataValPtr) realloc (ydata[0], 
					     allpts * sizeof (DataValRec));
	    zdata[0] = (DataValPtr) realloc (zdata[0], 
					     allpts * sizeof (DataValRec));

	    for (plat = 1; plat < nplat; plat++)
	    {
		memcpy ((char*)(xdata[0] + npts[0]), (char*)(xdata[plat]),
			npts[plat] * sizeof (DataValRec));
		memcpy ((char*)(ydata[0] + npts[0]), (char*)(ydata[plat]),
			npts[plat] * sizeof (DataValRec));
		memcpy ((char*)(zdata[0] + npts[0]), (char*)(zdata[plat]),
			npts[plat] * sizeof (DataValRec));

		npts[0] += npts[plat];
	    }
	}
/*
 * Get the info on whether we're using autoscaling or inverted scales
 */
	xy_GetScaleModes (c, &xauto, &xinvert, &yauto, &yinvert);
	xy_GetZModes (c, &zauto, &zinvert);
/*
 * Determine our final plot bounds.  
 */
	xy_DetermineBounds (c, 'x', &xmin, &xmax, xauto, xfnames[0], update);
	xy_DetermineBounds (c, 'y', &ymin, &ymax, yauto, yfnames[0], update);
/*
 * Save current bounds for posterity
 */
	xy_SetPrivateScale (c, &xmin, &xmax, &ymin, &ymax);
	xy_SaveDataTimes (c, &bTimeTarget, &eTimeTarget);
/*
 * Make the bounds official for coordinate conversion, and get the
 * (possibly zoomed) bounds back
 */
	xleft = (xinvert) ? xmax : xmin;
	xright = (xinvert) ? xmin : xmax;
	ybottom = (yinvert) ? ymax : ymin;
	ytop = (yinvert) ? ymin : ymax;

	lc_SetBaseUserCoord (&xleft, &xright, &ybottom, &ytop);
	lc_GetUserCoord (&xleft, &xright, &ybottom, &ytop);
/*
 * Top annotation
 */
	An_TopAnnot ("Contour plot of ");
	An_TopAnnot (platforms);
/*
 * Do each platform
 */
	for (plat = 0; plat < (oneplot ? 1 : nplat); plat++)
	{
		FieldId fid = F_Lookup (zfnames[0]);
		char z0justname[64];

		strcpy (z0justname, SimpleFieldName (fid));
	/*
	 * If z is manual get the info now.
	 */
		if (! zauto)
		{
			zstep = 20.0;
			pda_Search (Pd, c, "scale-z-step", z0justname,
					(char *) &zstep, SYMT_FLOAT);
			ccenter = 0.0; /* XXX */
			pda_Search (Pd, c, "scale-z-center", z0justname,
					(char *) &ccenter, SYMT_FLOAT);
		}
	/*
	 * If we are autoscaling, figure out the bounds.
	 */
		if (zauto)
		{
			char param[80];
			CalcCenterStep (zmin[plat].val.f, zmax[plat].val.f,
					(ncolors == 1) ? 10 : ncolors,
					&ccenter, &zstep);
			sprintf (param, "%s-scale-z-center", z0justname);
			pd_Store (Pd, "global", param, (char *) &ccenter,
					SYMT_FLOAT);
			sprintf (param, "%s-scale-z-step", z0justname);
			pd_Store (Pd, "global", param, (char *) &zstep,
					SYMT_FLOAT);
		}
	/*
	 * NOW that we have a center value, we can reasonably set up
	 * for the color bar.
	 */
		if (An_SaShow (c, "xy-contour") && ncolors > 1)
		{	
			sprintf (annotcontrol, "%s|%s|%f|%f", zfnames[plat],
				 ctname, ccenter, zstep);
			An_AddAnnotProc (An_ColorBar, c, annotcontrol,
					strlen (annotcontrol) + 1, 75,
					TRUE, FALSE);
		}
	/*
	 * Grid the data as requested
	 */
		if (strcmp (gridtype, "rgrid") == 0)
		{
			datagrid = xy_RGridit (c, &xleft, &xright, xgridres, 
					       &ybottom, &ytop, ygridres, 
					       xdata[plat], ydata[plat], 
					       zdata[plat], npts[plat], 
					       BADVAL);
		}
		else if (strcmp (gridtype, "raw") == 0)
		{
			ginfo = getGrid (c, xgridres, ygridres, BADVAL);
			gridRandomData (ginfo, xdata[plat], ydata[plat], 
					zdata[plat], npts[plat], &xleft, 
					&xright, &ybottom, &ytop, BADVAL);
			datagrid = ginfo->grid;
		}
		else if (strcmp (gridtype, "profile-linear") == 0)
		{
			xyc_FixYRes (&ygridres, ydata[plat], npts[plat],
				(double)ybottom.val.f, (double)ytop.val.f);
			ginfo = getGrid (c, xgridres, ygridres, BADVAL);
			gridRandomData (ginfo, xdata[plat], ydata[plat], 
					zdata[plat], npts[plat], &xleft, 
					&xright, &ybottom, &ytop, BADVAL);
			datagrid = xy_InterpolateLinearOnY (ginfo, BADVAL);
		}
	/*
	 * Clip rectangle for contouring module
	 */
		clip.x = FX0 * GWWidth (Graphics);
		clip.width = (FX1 - FX0) * GWWidth (Graphics);
		clip.y = (1.0 - FY1) * GWHeight (Graphics);
		clip.height = (FY1 - FY0) * GWHeight (Graphics);
	/*
	 * Do the contours
	 */
		ct_GetColorByName ("white", &white);

	        if (fill)
		{
			FC_Init (colors, ncolors, ncolors/2, &white, clip, 
				 TRUE, BADVAL);
			FillContour (Graphics, GWFrame (Graphics), datagrid,
				     xgridres, ygridres, devX (&xleft), 
				     devY (&ytop), devX (&xright), 
				     devY (&ybottom), ccenter, zstep);
		}
		else
	        {
			if (ncolors > 1)
				CO_Init (colors, ncolors, ncolors/2, &white,
						clip, TRUE, BADVAL);
			else
				CO_InitMono (*colors, clip, TRUE, BADVAL);
			Contour (Graphics, GWFrame (Graphics), datagrid,
				 xgridres, ygridres, devX (&xleft), 
				 devY (&ytop), devX (&xright), devY (&ybottom),
				 ccenter, zstep, dolabel, linewidth);
	        }
		SetClip (TRUE);
	}
/*
 * Add a period to the top annotation and draw axes
 */
	An_TopAnnot (".  ");
	ResetGC ();
	ac_PlotAxes (c);
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
}




GridInfoPtr
getGrid (c, xdim, ydim, badval)
char		*c;
int		xdim, ydim;
float		badval;
{
    GridInfoPtr	gridInfo = NULL; 
    int		newData = 0;
    int		i, j;
    /*
     * Search for old gridInfo for this component.
     */
    for (gridInfo = GridInfoList; gridInfo; gridInfo = gridInfo->next)
	if (strcmp (c, gridInfo->component) == 0)
		break;
    /*
     * Not found so need to allocate new gridInfo
     */
    if (!gridInfo)
    {
	gridInfo = (GridInfoPtr) calloc (1, sizeof (GridInfoRec));
	gridInfo->xdim = gridInfo->ydim = 0;
	gridInfo->grid = gridInfo->scratch = NULL;
    /*
     * Put it at the head of GridInfoList
     */
	gridInfo->next = GridInfoList;
	GridInfoList = gridInfo;
    }
    /*
     * Allocate the grid.
     */
    if (xdim != gridInfo->xdim || ydim != gridInfo->ydim)
    {
	if (gridInfo->grid)
		free (gridInfo->grid);
	if (gridInfo->scratch)
		free (gridInfo->scratch);

        gridInfo->grid = (float*) malloc (ydim * xdim * sizeof (float));
        gridInfo->scratch = (float*) malloc (ydim * xdim * sizeof (float));
	gridInfo->xdim = xdim;
	gridInfo->ydim = ydim;
	newData = 1;
    }

    /*
     * Initialize the grid.
     */
    for (i = 0; i < xdim; i++)
    {
            for (j = 0; j < ydim; j++)
            {
		    GRID (gridInfo->grid, i, j, ydim) = badval;
		    GRID (gridInfo->scratch, i, j, ydim) = 0.0;
            }
    }
    return (gridInfo);
}




float *
xy_RGridit (c, x0, x1, xdim, y0, y1, ydim, xdata, ydata, zdata, npts, 
	    badval)
char		*c;
DataValPtr	x0, x1;
int		xdim;
DataValPtr	y0, y1;
int		ydim;
DataValPtr	xdata, ydata, zdata;
int		npts;
float		badval;
{
    int	lxdim, lydim, lnpts;
    float lbad;
    int 	k;
    int		status;
    GridInfoPtr pinfo = NULL;
    float	*xpos, *ypos, *data;
    float	fx0, fx1, fy0, fy1;

    pinfo = getGrid (c, xdim, ydim, badval);
    xpos = (float*) calloc (npts, sizeof (float));
    ypos = (float*) calloc (npts, sizeof (float));
    data = (float*) calloc (npts, sizeof (float));
    /*
     * Add the data points to the grid.
     */
    for (k = 0; k < npts; k++)
    {
	switch (ydata[k].type)
	{
	    case 't':
		    ypos[k]= (float) 
			    (ydata[k].val.t.zt_Sec - y0->val.t.zt_Sec);
		    break;
	    case 'f':
		    ypos[k] = ydata[k].val.f;
		    break;
	}
	switch (xdata[k].type)
	{
	    case 't':
		    xpos[k]= (float) 
			    (xdata[k].val.t.zt_Sec - x0->val.t.zt_Sec);
		    break;
	    case 'f':
		    xpos[k] = xdata[k].val.f;
		    break;
	}
	data[k] = zdata[k].val.f;
    }
    
    switch (x0->type)
    {
	case 't':
	    fx0 = 0.0;
	    fx1 = (float) (x1->val.t.zt_Sec - x0->val.t.zt_Sec);
	    break;
	case 'f':
	    fx0 = x0->val.f;
	    fx1 = x1->val.f;
	    break;
    }
    switch (y0->type)
    {
	case 't':
	    fy0 = 0.0;
	    fy1 = (float) (y1->val.t.zt_Sec - y0->val.t.zt_Sec);
	    break;
	case 'f':
	    fy0 = y0->val.f;
	    fy1 = y1->val.f;
	    break;
    }
    lxdim = xdim;
    lydim = ydim;
    lnpts = npts;
    lbad = badval;

    status = dorgrid_ (pinfo->grid, &lxdim, &lydim, &lnpts, data, &lbad,
		       xpos, ypos, &fx0, &fy0, &fx1, &fy1, pinfo->scratch);
    switch (status)
    {
	case RG_NOTENUFPTS:
	    msg_ELog (EF_PROBLEM, "XYContour: Too few points for RGRID.");
	    break;
	case RG_COLLINEAR:
	    msg_ELog (EF_PROBLEM, 
		      "XYContour: Points passed to RGRID are collinear.");
	    break;
    }

    free (xpos);
    free (ypos);
    free (data);

    return (pinfo->grid);
}




int
yGridIndex (ydata, ydim, ystep, y0)
DataValPtr	ydata;
int		ydim;
float		ystep;
DataValPtr	y0;
{
    int index;
    switch (ydata->type)
    {
	case 't':
	    index = (ydim - 1) - (int) ((float) (ydata->val.t.zt_Sec
					    /* + ystep * 0.5 */  
						- y0->val.t.zt_Sec) / ystep);

	    break;
	case 'f':
	    index = (ydim - 1) - (int) ((ydata->val.f /* + ystep * 0.5 */ - 
					 y0->val.f) / ystep);

	    break;
    }

    if (index >= 0 && index < ydim) 
	    return (index);
    else
	    return (-1);
}




int
xGridIndex (xdata, xdim, xstep, x0)
DataValPtr	xdata;
int		xdim;
float		xstep;
DataValPtr	x0;
{
    int	index = -1;
    switch (xdata->type)
    {
	case 't':
	    index = (int) ((float) (xdata->val.t.zt_Sec /* + xstep * 0.5 */ - 
				    x0->val.t.zt_Sec) / xstep);

	    break;
	case 'f':
	    index = (int) ((xdata->val.f + /* xstep * 0.5 */
			    - x0->val.f) / xstep);

	    break;
    }

    if (index < xdim && index >= 0)
	    return (index);
    else 
	    return (-1);
}



static float
gridStep (ddim, data0, data1)
int	ddim;
DataValPtr	data0, data1;
{
    float 	dstep = 0.0;
    switch (data0->type)
    {
	case 't':
	    dstep = (float) (data1->val.t.zt_Sec - data0->val.t.zt_Sec) /
			 ((float) ddim /* - 1 */);
	    break;
	case 'f':
	    dstep = (data1->val.f - data0->val.f) / ((float) ddim /* -1 */);
	    break;
    }
    return (dstep);
}



static void
gridRandomData (ginfo, xdata, ydata, zdata, npts, x0, x1, y0, y1, 
		badval)
GridInfoPtr	ginfo;
DataValPtr	xdata, ydata, zdata;
int		npts;
DataValPtr	x0, x1;
DataValPtr	y0, y1;
float		badval;
{
    float	xstep, ystep;
    int		ix, iy, k;
    xstep = gridStep (ginfo->xdim, x0, x1);
    ystep = gridStep (ginfo->ydim, y0, y1);
    /*
     * Add each data point to the correct grid cell. Keep track of how
     * many points are contributing to the cell in "scratch"
     */
    for (k = 0; k < npts; k++)
    {
	ix = xGridIndex (& (xdata[k]), ginfo->xdim, xstep, x0);
	iy = yGridIndex (& (ydata[k]), ginfo->ydim, ystep, y0);
	if (ix >= 0 && iy >= 0 && ix < ginfo->xdim && iy < ginfo->ydim)
	{
	    if (GRID (ginfo->grid, ix, iy, ginfo->ydim) == badval)
	    {
                GRID (ginfo->grid, ix, iy, ginfo->ydim) = zdata[k].val.f;
                GRID (ginfo->scratch, ix, iy, ginfo->ydim) = 1.0;
	    }
	    else
	    {
                GRID (ginfo->grid, ix, iy, ginfo->ydim) += zdata[k].val.f;
                GRID (ginfo->scratch, ix, iy, ginfo->ydim) += 1.0;
	    }
	}
    }
    /*
     * Now average the points at each grid cell.
     */
    for (iy = 0; iy < ginfo->ydim; iy++)
    {
        for (ix = 0; ix < ginfo->xdim; ix++)
        {
	    if (GRID (ginfo->grid, ix, iy, ginfo->ydim) != badval)
	    {
                GRID (ginfo->grid, ix, iy, ginfo->ydim) /= 
        	    GRID (ginfo->scratch, ix, iy, ginfo->ydim);
                GRID (ginfo->scratch, ix, iy, ginfo->ydim) = -1.0;
	    }
        }
    }
}

float *
xy_InterpolateLinearOnY (ginfo, badval)
GridInfoPtr	ginfo;
float		badval;
{
    int 	iy, ix;
    int		nx;
    float	zstep;
    int		obsid;
    float	dz;
    int		*dataLoc;

    dataLoc = (int*) calloc (ginfo->xdim, sizeof (int));

    for (iy = 0; iy < ginfo->ydim; iy++)
    {
	nx = 0;
	/*
	 * First find all the observed values at this y-level.
	 */
        for (ix = 0; ix < ginfo->xdim; ix++)
        {
	    if (GRID (ginfo->grid, ix, iy, ginfo->ydim) != badval)
	    {
		dataLoc[nx] = ix;
		nx++;
	    }
        }
	/*
	 * Now interpolate missing values at this y-level.
	 */
        for (obsid = 0; obsid < nx-1; obsid++)
        {
	    dz = GRID (ginfo->grid, dataLoc[obsid+1], iy, ginfo->ydim) -
		     GRID (ginfo->grid, dataLoc[obsid], iy, ginfo->ydim);
	    zstep = dz / (dataLoc[obsid+1] - dataLoc[obsid]);

	    for (ix = dataLoc[obsid] + 1; ix < dataLoc[obsid+1]; ix++)
	    {
		GRID (ginfo->grid, ix, iy, ginfo->ydim) = 
			GRID (ginfo->grid, dataLoc[obsid], iy, ginfo->ydim) +
				(zstep* (ix - dataLoc[obsid]));
	    }
	}
    }
    free (dataLoc);
    return (ginfo->grid);
}





static int
xyc_PlatsAndFields (c, platforms, nplat, pnames, nxfield, xfnames, nyfield,
		yfnames, nzfield, zfnames)
int *nplat, *nxfield, *nyfield, *nzfield;
char *c, *platforms, **pnames, **xfnames, **yfnames, **zfnames;
{
	static char xflds[MAX_PLAT_LEN], yflds[MAX_PLAT_LEN];
	static char zflds[MAX_PLAT_LEN]; /* XXX */
	zbool ok;
	int plat;
/*
 * Look up the parameters.
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "x-field", NULL, xflds, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "y-field", NULL, yflds, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "z-field", NULL, zflds, SYMT_STRING);
/*
 * Parse platform and field name information. 
 */
	if ((*nplat = CommaParse (platforms, pnames)) > MAX_PLAT)
	{
		msg_ELog (EF_PROBLEM, "XYContour: %s: too many platforms", c);
		return (FALSE);
	}
	*nxfield = ParseFieldList (xflds, xfnames);
	*nyfield = ParseFieldList (yflds, yfnames);
	*nzfield = ParseFieldList (zflds, zfnames);
/*
 * Test for acceptable combinations of numbers of fields and platforms.
 * Number of fields must be equal, and the number must be equal to 
 * the number of platforms if greater than one.
 */
	if ((*nxfield != *nplat && *nxfield != 1 && *nplat != 1) ||
	    (*nyfield != *nplat && *nyfield != 1 && *nplat != 1) ||
	    (*nzfield != *nplat && *nzfield != 1 && *nplat != 1) ||
	    (*nxfield != *nyfield) || (*nyfield != *nzfield))
	{
		msg_ELog (EF_PROBLEM, "XYContour: %s: bad number of fields", 
			  c);
		return (FALSE);
	}
/*
 * If one platform and many fields, replicate the platform name as if we
 * had that many platforms to begin with
 */
	if ((*nplat == 1) && (*nxfield > 1))
		for (plat = 1; plat < *nxfield; ++plat)
			pnames[plat] = pnames[0];
	return (TRUE);
}






static void
xyc_DoColors (c, fill, ctname, colors, ncolors)
char *c;
int fill;
char *ctname;
XColor **colors;
int *ncolors;
/*
 * Figure out what's happening with colors.
 */
{
	zbool mono;
	static XColor monocolor;
/*
 * If we are not doing filled contours, give them the opportunity to
 * ask for things in monochrome.
 */
	if (! fill && pda_Search (Pd, c, "color-mono", "xy-contour",
				  (char*) &mono, SYMT_BOOL) && mono)
	{
		if (! pda_Search (Pd, c, "color", "xy-contour", ctname,
				SYMT_STRING))
			strcpy (ctname, "white");
		if (! ct_GetColorByName (ctname, &monocolor))
			ct_GetColorByName ("white", &monocolor);
		*colors = &monocolor;
		*ncolors = 1;
		return;
	}
/*
 * OK, it's a polychromatic world.
 */
	if (! pda_Search (Pd, c, "color-table", "xy-contour", ctname,
			     SYMT_STRING))
	{
		msg_ELog (EF_INFO, "No color table given, using Contour");
		strcpy (ctname, "Contour");
	}
/*
 * Attempt to load color table 
 */
	if (!ct_LoadTable (ctname, colors, ncolors))
	{
		msg_ELog (EF_PROBLEM,"Unable to load table %s, trying Contour",
				ctname);
		ct_LoadTable ("Contour", colors, ncolors);
		strcpy (ctname, "Contour");
	}
}



static void
xyc_FixYRes (ygridres, ydata, npts, y0, y1)
int *ygridres;
DataValPtr ydata;
int npts;
double y0, y1;
/*
 * Go through and adjust the Y grid resolution if need be.
 */
{
	int nvisible = 0, i, lastnv = 0;
	float lastyv;
/*
 * This is a heroic kludge.  You see, all the Y data has been mashed
 * together at this point.  We really want to try to figure the number of
 * levels that are visible, not the number of points.  So we try to catch
 * when it drops back down and start over, taking the maximum.
 */
	for (i = 0; i < npts; i++)
	{
		float yv = ydata[i].val.f;
		if (yv >= y0 && yv <= y1)
			nvisible++;
		if (i > 0 && yv < lastyv)
		{
			if (nvisible > lastnv)
				lastnv = nvisible;
			nvisible = 0;
		}
		lastyv = yv;
	}
/*
 * OK, we have an answer of sorts.  IF the data resolution is below the
 * grid resolution (but we have data), then tweak down the grid resolution
 * to match.
 */
	if (lastnv > nvisible)
		nvisible = lastnv;
	msg_ELog (EF_DEBUG, "FIXY, res %d, vis %d", *ygridres, nvisible);
	if (*ygridres > nvisible && nvisible > 3)
		*ygridres = nvisible - 1;
}
