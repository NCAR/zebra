/*
 * XY-Contour plotting module
 */
static char *rcsid = "$Id: XYContour.c,v 1.14 1993-10-22 21:25:46 corbet Exp $";
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
# include "rg_status.h"
# define GRID(g,i,j,ydim)    (g[((i)*(ydim))+(j)])

/*
 * General definitions
 */

void	xy_Contour FP ((char *, int));

/*
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

typedef struct GridInfo {
    float	*grid;
    float	*scratch;
    int		xdim,ydim;
    char	component[80];
    struct GridInfo	*next;
}GridInfoRec,*GridInfoPtr;
static GridInfoPtr GridInfoList = NULL;

/*
 * Color array and indices
 */
static XColor 	Tadefclr;

/*
 * Forwards
 */
GridInfoPtr getGrid FP((char *, int, int, int, double));

float *xy_RGridit FP((char *, int, 
		      DataValPtr, DataValPtr,
		      int, int,
		      DataValPtr, DataValPtr, 
		      int, int,
		      DataValPtr, DataValPtr, DataValPtr,
		      int, double));

void gridRandomData FP((
	GridInfoPtr	ginfo,
	DataValPtr	xdata,
	DataValPtr	ydata,
	DataValPtr	zdata,
	int		npts,
	DataValPtr	xmin,
	DataValPtr	xmax,
	int	xscalemode,
	DataValPtr	ymin,
	DataValPtr	ymax,
	int	yscalemode,
	double		badval));

float *xy_InterpolateLinearOnY FP((
	GridInfoPtr	ginfo,
	DataValPtr	xmin,
	DataValPtr	xmax,
	DataValPtr	ymin,
	DataValPtr	ymax,
	double		badval));


void
xy_Contour(c, update)
char	*c;
bool	update;
/*
 * Draw an xy-graph on the given component
 */
{
	bool	ok;
	int	i, npts, plat, nplat, ii, jj, ns ;
	int	nxfield,nyfield;
	int	count;
	int 	nPlotted=0;
	char	platforms[MAX_PLAT_LEN], tadefcolor[30];
	char	ctname[20];
	char	dataNames[4][MAX_PLAT_LEN];
	ZebTime	bTimeTarget,eTimeTarget;
	ZebTime	eTimeReq,bTimeReq;
	ZebTime	eTimeOld,bTimeOld;
	long	autoTime;
	char	*pnames[MAX_PLAT];
	char	*fnames[4][MAX_PLAT];
	PlatformId	pid;
	DataChunk	*dc = NULL;
	DataClass	xyClass;
	Location	origin;
	char		stime1[80],stime2[80];
	int		n, m, len;
	RGrid		rg;
	float		badvalue, *data[MAX_PLAT], *tempdata;
	FieldId		fids[4];
	DataOrganization	xyOrg;
	DataValPtr	*xdata,*ydata;
	DataValPtr	*zdata;
	DataValRec	xmin,xmax,ymin,ymax;
	DataValRec	oldxmin,oldxmax,oldymin,oldymax;
	int		xrescale=0,yrescale=0;
	DataValRec	zmin,zmax;
	char		xtype = 'f',ytype = 'f';
	unsigned short	xscalemode,yscalemode;
	int	fcount;
        int	xdim,ydim;
	int	saveConfig;
	int	angle = 0;
	int	zdim;
	int	nzfield ;
	float	vecScale = 0.01;
	char	style[80];
	float	zstep;
	int		xgridres, ygridres=0;
	char	csystem[32];
	int	dmode;
	char	datalabel[MAX_PLAT_LEN];
        float	ccenter;
	char	gridtype[80];
	XColor	*colors;
	int	ncolors;
	bool	sideAnnot;
/*
 * Contour forces global update.
 */
	if ( update )
	{
	    TriggerGlobal=1;
	    return;
	}
/*
 * Get X-Y Contour Required parameters:
 * "platform","x-field", "y-field", "wind-coords", "color-table", "org"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"x-field",NULL, (dataNames[0]), SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"y-field",NULL, (dataNames[1]), SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"z-field",NULL, (dataNames[2]), SYMT_STRING);

	ok &= pda_ReqSearch (Pd, c, "color-table", "xy-contour",ctname,SYMT_STRING);

	if (! ok) return;
/*
 * Parse platform and field name information. 
 */
	nplat = CommaParse (platforms, pnames);
	nxfield = CommaParse (dataNames[0], fnames[0]);
	nyfield = CommaParse (dataNames[1], fnames[1]);
	nzfield = CommaParse (dataNames[2], fnames[2]);
	if ( nxfield != nplat && nyfield != nplat && nzfield != nplat )
	{
	    msg_ELog ( EF_PROBLEM, 
		"X/Y Contour: number of fields of doesn't correspond to number of platforms.");
	    return;
	}
/* for now, only allow max MAX_PLAT platforms */
	if (nplat > MAX_PLAT)
		nplat = MAX_PLAT;
/*
 * Get X-Y Contour optional parameters:
 * "style" - "line" or "filled"
 * "x-grid","y-grid" - the number of grid cells to use in the X and Y dimensions
 * "z-step" - the contour interval.
 * "contour-grid-type" - method of interpolating gridded data
 */
        if ( !pda_Search (Pd,c,"do-side-annotation", "xy-contour",
                (char *) &sideAnnot, SYMT_BOOL))
        {
            sideAnnot = True;
        }

	if ( !pda_Search (Pd,c,"representation-style", "xy-contour",
			  (char *)style, SYMT_STRING))
	{
	    strcpy(style, "line" );
	}
	if ( !pda_Search (Pd,c,"x-grid", "xy-contour",(char *) &xgridres, 
			  SYMT_INT))
	{
		xgridres = 25;
	}
	if ( !pda_Search (Pd,c,"y-grid", "xy-contour",(char *) &ygridres, 
			  SYMT_INT))
	{
		ygridres = 25;
	}
	if ( !pda_Search (Pd,c,"z-step", "xy-contour",(char *) &zstep, 
			  SYMT_FLOAT))
	{
		zstep = 20.0;
	}
	if ( !pda_Search(Pd,c,"grid-method", "xy-contour",(char *)gridtype, 
			 SYMT_STRING))
	{
		strcpy( gridtype, "raw");
	}
/*
 * Allocate memory for data and attibutes
 */
	xdata = (DataValPtr*)malloc (nplat* sizeof(DataValPtr));
	ydata = (DataValPtr*)malloc (nplat* sizeof(DataValPtr));
	zdata = (DataValPtr*)malloc (nplat* sizeof(DataValPtr));
/*
 * Get Generic plot description information.
 */
	if ( strcmp( fnames[0][0],"time" ) == 0 )
	    xtype = 't';
	if ( strcmp( fnames[1][0],"time" ) == 0 )
	    ytype = 't';
	if ( strcmp( fnames[2][0],"time" ) == 0 )
	{
	    msg_ELog ( EF_PROBLEM, "X/Y Contour: 'time' is an invalid z-field");
	    return;
	}
        xy_GetScaleInfo(Pd,c,'x',&xscalemode);
        xy_GetScaleInfo(Pd,c,'y',&yscalemode);
        xy_GetCurrentScaleBounds(Pd,c,'x',xtype,&oldxmin,&oldxmax,fnames[0][0]);
        xy_GetCurrentScaleBounds(Pd,c,'y',ytype,&oldymin,&oldymax,fnames[1][0]);
	xmin = oldxmin;
	xmax = oldxmax;
	ymin = oldymin;
	ymax = oldymax;
	/* Make sure coordinate system is initialized */
        lc_SetUserCoord ( &xmin,&xmax,&ymin,&ymax);
        xy_GetDataDescriptors(Pd, c, update,
                              &bTimeTarget,&eTimeTarget,
                              &bTimeOld,&eTimeOld,
                              &dmode,&nPlotted );

   /*
    * Check to see if the current Plot-Time is already beyond
    * the existing time-scale.
    */
        if ( (xscalemode & AUTO) && xtype == 't' )
        {
            if ( update )
            {
		TC_EncodeTime ( &eTimeTarget, TC_Full, stime1 );
		TC_EncodeTime ( &(xmax.val.t), TC_Full, stime2 );
                msg_ELog ( EF_DEBUG,
                   "%s new Axis EndTime = %s old Axis EndTime = %s",
                        c, stime1, stime2 );
                if ( eTimeTarget.zt_Sec > xmax.val.t.zt_Sec )
                {
                    TriggerGlobal = 1;
                }
            }
        }
        if ( (yscalemode & AUTO) && ytype == 't' )
        {
            if ( update )
            {
                if ( eTimeTarget.zt_Sec > ymax.val.t.zt_Sec )
                {
                    TriggerGlobal = 1;
                }
            }
        }

	xy_GetPlotColors(Pd,c,nplat,NULL,tadefcolor);

/*
 **********************************************************
 * X-dependent set-up
 **********************************************************
 */
/*
 * Get X-pixel for colors...
 */
	if (! ct_GetColorByName(tadefcolor, &Tadefclr))
	{
	    msg_ELog(EF_PROBLEM, "Can't get X annotation color: '%s'.",
			tadefcolor);
	    strcpy(tadefcolor, "white");
	    ct_GetColorByName(tadefcolor, &Tadefclr);
	}
/*
 * Attempt to load color table 
 */
	ct_LoadTable (ctname, &colors, &ncolors);
	if (ncolors < 1)
	{
		msg_ELog(EF_PROBLEM, "XY-Contourcolor table too small");
		return;
	}
/*
 **********************************************************
 **********************************************************
 */

/*
 * Do the plot.
 */

/*
 * Plot the annotation.
 */
	if (! update)
	{
	    An_TopAnnot ("X/Y Contour:", Tadefclr.pixel);
	    An_TopAnnot (c,Tadefclr.pixel);
	}
/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Get the data and determine the coordinate min and max's
	 */
	    pid = ds_LookupPlatform (pnames[plat]);
	    if (pid == BadPlatform)
	    {
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", pnames[plat]);
		continue;
	    }
            xyOrg = ds_PlatformDataOrg(pid);
	    switch (xyOrg)
	    {
		case OrgScalar:
		case OrgFixedScalar:
			xyClass = DCC_Scalar;
			break;
		case Org1dGrid:
			xyClass = DCC_RGrid;
			break;
		default:
			msg_ELog (EF_PROBLEM, "Bad organization.");
			continue;
	    }

	/*
	 * Set up the field-names for the data retrieval request.
	 */
	    fcount = 0;
	    if (xtype != 't')
	    {
		xdim = fcount;
	        fids[fcount] = F_Lookup (fnames[0][plat]); fcount++;
	    }
	    if (ytype != 't')
	    {
		ydim = fcount;
	        fids[fcount] = F_Lookup (fnames[1][plat]); fcount++;
	    }
	    zdim = fcount;
	    fids[fcount] = F_Lookup (fnames[2][plat]); fcount++;
	/*
	 *  Determine times of data to request.
	 */
            if (! xy_AvailableData(pid,&bTimeTarget,&eTimeTarget,&eTimeOld,
				   &bTimeReq, &eTimeReq))
	    {
		    TC_EncodeTime (&bTimeTarget, TC_Full, stime1);
		    TC_EncodeTime (&eTimeTarget, TC_Full, stime2);
		    msg_ELog (EF_INFO, "No data for '%s' between %s and %s",
			      pnames[plat], stime1, stime2);
		    npts = 0;
		    xdata[plat] = NULL;
		    ydata[plat] = NULL;
		    zdata[plat] = NULL;
		    continue;
	    }

	    TC_EncodeTime (&bTimeTarget, TC_Full, stime1);
	    TC_EncodeTime (&eTimeTarget, TC_Full, stime2);
	    msg_ELog (EF_DEBUG, "%s data request times begin: %s end: %s",
		      c, stime1, stime2);

	    dc = NULL;
	    dc = ds_Fetch (pid, xyClass, &bTimeReq, &eTimeReq, fids, fcount,
			   NULL, 0);

	    if (! dc)
	    {
		    msg_ELog (EF_INFO, 
			      "No requested data for '%s' between %s and %s",
			      pnames[plat], stime1, stime2);
		    npts = 0;
		    xdata[plat] = NULL;
		    ydata[plat] = NULL;
		    zdata[plat] = NULL;
		    continue;
	    }
	/*
	 * Now that we have a data chunk, update the overlay times widget
	 */
	    if (!update) lw_TimeStatus (c, pnames[plat], &eTimeReq);
	/*
	 * Extract the data from the data chunk
	 */
	    badvalue = dc_GetBadval (dc);
	    if ( xyOrg == OrgScalar )
	    {
		    npts = dc_GetNSample (dc);
		    for (n = 0; n < fcount; n++)
		    {
			    data[n] = (float *) malloc (npts * sizeof (float));
			    for (m = 0; m < npts; m++)
				    data[n][m] = dc_GetScalar (dc, m, fids[n]);
		    }
	    }
	    else if ( xyOrg == Org1dGrid )
	    {
		    ns = dc_GetNSample (dc);
		    for (n = 0; n < fcount; n++)
			    for (m = 0; m < ns; m++)
			    {
				    tempdata = dc_RGGetGrid (dc, m, fids[n],
							     &origin, &rg, 
							     &len);
				    if ( m == 0 )
				    {
					    npts = rg.rg_nX * ns;
					    data[n] = (float *) 
						malloc (npts * sizeof (float));
				    }
				    memcpy (data[n] + (m*rg.rg_nX), tempdata, 
					    len);
			    }
	    }
	    xdata[plat] = (DataValPtr)malloc( npts * sizeof(DataValRec));
	    ydata[plat] = (DataValPtr)malloc( npts * sizeof(DataValRec));
	    zdata[plat] = (DataValPtr)malloc( npts * sizeof(DataValRec));

	    count = 0;
	/*
	 * Extract field data arrays.
	 */
            msg_ELog ( EF_DEBUG,
                   "X-Y Graph found %d data points for component %s.",npts,c);
	    for ( ii = 0; ii < npts; ii++ )
	    {
		/* skip bad data points */
		do {
		    for ( jj = 0; jj < fcount ; jj++)
		    {
			if(data[jj][ii] == badvalue)
			{
			    ii++; break;
			}
		    }
		} while ( jj < fcount && ii < npts);
		if ( ii < npts )
		{
		  if ( xtype == 't' )
		  {
		    dc_GetTime (dc, xyOrg == OrgScalar ? ii : 
			(int) (ii/rg.rg_nX), &(xdata[plat][count].val.t));
		    xdata[plat][count].type = 't';
		  }
		  else
		  {
		    xdata[plat][count].val.f = data[xdim][ii];
		    xdata[plat][count].type = 'f';
		  }
		  if ( ytype == 't' )
		  {
		    dc_GetTime (dc, xyOrg == OrgScalar ? ii : 
			(int) (ii/rg.rg_nX), &(ydata[plat][count].val.t));
		    ydata[plat][count].type = 't';
		  }
		  else
		  {
		    ydata[plat][count].val.f = data[ydim][ii];
		    ydata[plat][count].type = 'f';
		  }
		  zdata[plat][count].val.f = data[zdim][ii];
		  zdata[plat][count].type = 'f';
		  count++;
		}
	    }
	    npts = count;
	    /*
	     * Calculate autoscaled mins/maxs if necessary.
	     */
            if ( (xscalemode & AUTO) && xtype != 't' )
            {
                xy_GetDataMinMax(update, &xmin, &xmax, xdata[plat], npts);
            }
            if ( (yscalemode & AUTO) && ytype != 't' )
            {
                xy_GetDataMinMax(update, &ymin, &ymax, ydata[plat], npts);
            }
	    /*
	     * Free memory.
	     */
	    dc_DestroyDC (dc);
	    for (n = 0; n < fcount; n++)
		if (data[n])
			free (data[n]);
	}
/*
 * Now set the current scale bounds.
 */
        autoTime = eTimeTarget.zt_Sec +
                (long)((eTimeTarget.zt_Sec-bTimeTarget.zt_Sec)*0.025);
        if ( !update )
        {
            if ( xscalemode & AUTO)
            {
                switch ( xtype )
                {
                    case 't':
			xmax.val.t.zt_Sec = autoTime;
			xmax.val.t.zt_MicroSec = 0;
                        xmin.val.t = bTimeTarget;
                    break;
                    case 'f':
                        xmin.val.f -= 5.0;
                        xmax.val.f += 5.0;
                    break;
                }
            }
            if ( yscalemode & AUTO)
            {
                switch ( ytype )
                {
                    case 't':
			ymax.val.t.zt_Sec = autoTime;
			ymax.val.t.zt_MicroSec = 0;
                        ymin.val.t = bTimeTarget;
                    break;
                    case 'f':
                        ymin.val.f -= 5.0;
                        ymax.val.f += 5.0;
                    break;
                }
            }
        }
        if ( xscalemode & AUTO )xy_SetScaleBounds(Pd,c,'x',xtype,&xmin,&xmax);
        if ( yscalemode & AUTO )xy_SetScaleBounds(Pd,c,'y',ytype,&ymin,&ymax);

        /*
         * Now check if scale-bounds are different from the axis-bounds
         * If so, and this is only an update, then trigger a global
         * redraw so shift is reflected for all components
         */
        if ( ((lc_CompareData(&ymin,&oldymin) != 0 ) ||
              (lc_CompareData(&ymax,&oldymax) != 0 ) )) yrescale = 1;
        if ( ((lc_CompareData(&xmin,&oldxmin) != 0 ) ||
              (lc_CompareData(&xmax,&oldxmax) != 0 ) )) xrescale = 1;
        if ( update && (xrescale || yrescale )) TriggerGlobal = 1;
        xy_AdjustAxes( Pd,c,xtype,update ? 0 : 1,ytype,update ? 0 : 1);

/*
 * Plot the data.
 */
        if ( !TriggerGlobal )
        {
	    XRectangle	clip;
	    XColor	Black;
	    int		dolabel = 1;
	    int		linewidth = 1;
	    float	*datagrid = NULL;
	    GridInfoPtr	ginfo;
	    int		ii,jj;

	    ct_GetColorByName("white",&Black);
            lc_SetUserCoord ( &xmin,&xmax,&ymin,&ymax);
            lc_GetUserCoord ( &xmin,&xmax,NULL,NULL, xscalemode);
            lc_GetUserCoord ( NULL,NULL,&ymin,&ymax, yscalemode);
	    if ( xscalemode & INVERT )
	    {
	        clip.x = devX(&xmax,xscalemode);
	        clip.width = devX(&xmin,xscalemode) - clip.x;
	    }
	    else 
	    {
	        clip.x = devX(&xmin,xscalemode);
	        clip.width = devX(&xmax,xscalemode) - clip.x;
	    }
	    if ( yscalemode & INVERT )
	    {
	        clip.y = devY(&ymin,yscalemode);
	        clip.height = devY(&ymax,yscalemode) - clip.y;
	    }
	    else
	    {
	        clip.y = devY(&ymax,yscalemode);
	        clip.height = devY(&ymin,yscalemode) - clip.y;
	    }
            gp_Clip( &xmin, &ymin,&xmax,&ymax,xscalemode,yscalemode );
            for (plat = 0; plat < nplat; plat++)
            {
                xy_GetDataMinMax(update, &zmin, &zmax, zdata[plat], npts);
		ccenter = (zmin.val.f + (zmax.val.f - zmin.val.f)*0.5)/zstep;
		ccenter = ccenter - (int)ccenter > 0.5 ? 
		    zstep*(float)(int)(ccenter+1.0):
		    zstep*(float)(int)(ccenter);
	/*
	 * Add this platform to the annotation
	 */
	    if ( sideAnnot && !update)
	    {

                sprintf (datalabel, "%s%s %s %f %f", 
			"contour-", fnames[2][plat],ctname, ccenter, zstep);
                An_AddAnnotProc (An_ColorBar, c, datalabel,
                                strlen(datalabel)+1, 75, TRUE, FALSE);
	    }
		/*
		 * Call "rgrid" on the data
		 */
		if ( strcmp(gridtype,"rgrid") == 0)
		{
	            datagrid = (float*)xy_RGridit(c,update, 
			 &xmin, &xmax, xscalemode,xgridres,
			 &ymin, &ymax, yscalemode,ygridres,
			  xdata[plat], ydata[plat], zdata[plat], npts, 
			  BADVAL );
		}
		else if ( strcmp(gridtype,"raw")==0)
		{
    	    	    ginfo = (GridInfoPtr)getGrid(c,update,xgridres, 
				ygridres,BADVAL);
	    	    gridRandomData(ginfo,
			xdata[plat],ydata[plat],zdata[plat],npts,
			&xmin,&xmax,xscalemode,
			&ymin,&ymax,yscalemode,BADVAL);
		    datagrid = ginfo->grid;
		}
		else if ( strcmp(gridtype,"profile-linear")==0)
		{
    	    	    ginfo = (GridInfoPtr)getGrid(c,update,xgridres, 
				ygridres,BADVAL);
	    	    gridRandomData(ginfo,
			xdata[plat],ydata[plat],zdata[plat],npts,
			&xmin,&xmax,xscalemode,
			&ymin,&ymax,yscalemode,BADVAL);
		    datagrid = (float*)xy_InterpolateLinearOnY( ginfo, 
			  &xmin, &xmax, &ymin, &ymax, BADVAL );
/*
		    for ( jj= 0; jj < ygridres; jj++)
		    {
		       fprintf ( stdout, "\r\n");
		       for ( ii= 0; ii < xgridres; ii++)
		       {
			if ( GRID(ginfo->scratch,ii,jj,ygridres) < 0.0 )
			    fprintf ( stdout, "*");
	        	fprintf ( stdout, "%6.2f ",
			   GRID(datagrid,ii,jj,ygridres) );
		       }
		    }
*/
		}
	        if ( strcmp(style, "line" ) == 0 )
	        {
		    CO_Init( colors, ncolors, ncolors/2, Black, clip,
				TRUE, BADVAL );
		    if ( xscalemode & INVERT )
		    {
		      Contour ( Graphics, GWFrame(Graphics), datagrid,
			xgridres, ygridres, 
			devX(&xmax,xscalemode),devY(&ymax,yscalemode),
			devX(&xmin,xscalemode),devY(&ymin,yscalemode),
			ccenter, zstep, dolabel ,linewidth );
		    }
		    else
		    {
		      Contour ( Graphics, GWFrame(Graphics), datagrid,
			xgridres, ygridres, 
			devX(&xmin,xscalemode),devY(&ymax,yscalemode),
			devX(&xmax,xscalemode),devY(&ymin,yscalemode),
			ccenter, zstep, dolabel ,linewidth );
		    }
	        }
		else
		{
		    FC_Init( colors, ncolors, ncolors/2, Black, clip,
				TRUE, BADVAL );
		    FillContour ( Graphics, GWFrame(Graphics), datagrid,
			xgridres, ygridres, 
			devX(&xmin,xscalemode),devY(&ymax,yscalemode),
			devX(&xmax,xscalemode),devY(&ymin,yscalemode),
			ccenter, zstep );
		}
            }
            XSetClipMask(XtDisplay(Graphics), Gcontext, None);
        }

/*
 * Add a period to the top annotation
 */
	if ( !update )
	    An_TopAnnot (".  ", Tadefclr.pixel);

        /*
         * Store the updated data begin and end times.
         */
        if (update)
            xy_SetPrivateDD(Pd,c,NULL, &eTimeReq, &nPlotted);
        else
            xy_SetPrivateDD(Pd,c,&bTimeReq, &eTimeReq, &nPlotted);

/*
 * Free local memory
 */
	for ( i = 0; i < nplat; i++)
	{
	    if (xdata[i]) free(xdata[i]);
	    if (ydata[i]) free(ydata[i]);
	    if (zdata[i]) free(zdata[i]);
	}
	free(xdata); free(ydata); free(zdata);
}


GridInfoPtr
getGrid(c,update,xdim, ydim,badval)
char		*c;
bool		update;
int		xdim,ydim;
float		badval;
{
    GridInfoPtr	gridInfo = NULL; 
    int			newData = 0;
    int			i,j;
    /*
     * Search for old gridInfo for this component.
     */
    while ( GridInfoList )
    {
	if ( strcmp( c, GridInfoList->component ) == 0 )
	{
	    gridInfo = GridInfoList;
	    break;
	}
	GridInfoList = GridInfoList->next;
    }

    /*
     * Not found so need to allocate new gridInfo
     */
    if ( !gridInfo ) 
    {
	gridInfo = (GridInfoPtr)calloc( 1, sizeof(GridInfoRec));
	gridInfo->next = GridInfoList;
	gridInfo->xdim = gridInfo->ydim = 0;
	gridInfo->grid = gridInfo->scratch = NULL;
    }
    /*
     * Allocate the grid.
     */
    if ( xdim != gridInfo->xdim || ydim != gridInfo->ydim )
    {
	if ( gridInfo->grid ) free(gridInfo->grid);
	if ( gridInfo->scratch ) free(gridInfo->scratch);
        gridInfo->grid = (float*)malloc(ydim * xdim * sizeof(float));
        gridInfo->scratch = (float*)malloc(ydim * xdim * sizeof(float));
	gridInfo->xdim = xdim;
	gridInfo->ydim = ydim;
	newData = 1;
    }

    /*
     * Initialize the grid.
     */
    if ( !update || newData )
    {
        for ( i = 0; i < xdim; i++)
        {
            for ( j = 0; j < ydim; j++)
            {
	        GRID(gridInfo->grid,i,j,ydim) = badval;
	        GRID(gridInfo->scratch,i,j,ydim) = 0.0;
            }
        }
    }
    return ( gridInfo );
}
void
xy_AddToLevel (plane, pwgt,xdim, ydim, xstep, xmin, xscalemode,iy, xdat, zdat)
float	*plane;
float	*pwgt;
int	xdim,ydim;
float	xstep;
DataValPtr	xmin;
unsigned short	xscalemode;
int     iy;
DataValPtr  xdat;
DataValPtr  zdat;
/*
 * Apply the point with value zdat located at (xdat,ydat) to the grid
 * at height index iy.  (For time-height plots, xdat should be the time
 * position and ydat should be zero)
 */
{
        int     ix,ii,jj;
        float   x, d, wgt,xflt;
/*
 * Step through the plane in the x direction at position index iy and use a 
 * weighting scheme to apply the given data point
 */
	if ( zdat->type != 'f' ) return;
        for (ix = 0; ix < xdim; ix++)
        {
		switch ( xdat->type )
		{
		    case 't':
			xflt = 
			 (float)((xdat->val.t.zt_Sec+xstep*0.5) - 
				 xmin->val.t.zt_Sec);
		    break;
		    case 'f':
			xflt = (xdat->val.f+xstep*0.5) - xmin->val.f;
		    break;
		}
		if ( xflt < 0.0 || xflt > xdim * xstep ) continue;

		if ( xscalemode & INVERT )
		{
                    x = (xdim-1) - (ix * xstep);
		}
		else
		{
                    x = ix * xstep;
		}
                d = fabs(xflt - x);
        /*
         * Use a 1/d^2 weighting scheme, truncated at a weight of 100
         */
                if (d < 0.1)
                        wgt = 100;
                else
                        wgt = 1.0 / (d * d);
        /*
         * Apply the point
         */
                GRID(plane,ix,iy,ydim) = (GRID(plane,ix,iy,ydim) * GRID(pwgt,ix,iy,ydim) + 
		    zdat->val.f * wgt) / (GRID(pwgt,ix,iy,ydim) + wgt);
                GRID(pwgt,ix,iy,ydim) = GRID(pwgt,ix,iy,ydim) + wgt;
        }
}

float *
xy_RGridit( c,update,
			  xmin, xmax, xscalemode,xdim,
			  ymin, ymax,yscalemode,ydim,
			  xdata, ydata, zdata, npts, badval )
char		*c;
bool		update;
DataValPtr	xmin,xmax;
unsigned short  xscalemode;
int		xdim;
DataValPtr	ymin,ymax;
unsigned short yscalemode;
int		ydim;
DataValPtr	xdata,ydata,zdata;
int		npts;
float		badval;
{
    int	lxdim,lydim,lnpts;
    float lbad;
    int 	i,j,k;
    int		status;
    GridInfoPtr pinfo = NULL;
    float	*xpos,*ypos,*data;
    float	fxmin,fxmax, fymin,fymax;
    pinfo = (GridInfoPtr)getGrid(c,update,xdim, ydim,badval);
    xpos = (float*)calloc( npts, sizeof(float));
    ypos = (float*)calloc( npts, sizeof(float));
    data = (float*)calloc( npts, sizeof(float));
    /*
     * Add the data points to the grid.
     */
    for ( k = 0; k < npts; k++)
    {
	switch ( ydata[k].type )
	{
	    case 't':
		    ypos[k]=(float)(ydata[k].val.t.zt_Sec - ymin->val.t.zt_Sec);
	    break;
	    case 'f':
		    ypos[k] = ydata[k].val.f;
	    break;
	}
	switch ( xdata[k].type )
	{
	    case 't':
		    xpos[k]=(float)(xdata[k].val.t.zt_Sec - xmin->val.t.zt_Sec);
	    break;
	    case 'f':
		    xpos[k] = xdata[k].val.f;
	    break;
	}
	data[k] = zdata[k].val.f;
    }
    
    switch ( xmin->type )
    {
	case 't':
	    fxmin = 0.0;
	    fxmax = (float)(xmax->val.t.zt_Sec-xmin->val.t.zt_Sec);
	break;
	case 'f':
	    fxmin = xmin->val.f;
	    fxmax = xmax->val.f;
	break;
    }
    switch ( ymin->type )
    {
	case 't':
	    fymin = 0.0;
	    fymax = (float)(ymax->val.t.zt_Sec-ymin->val.t.zt_Sec);
	break;
	case 'f':
	    fymin = ymin->val.f;
	    fymax = ymax->val.f;
	break;
    }
    lxdim = xdim;
    lydim = ydim;
    lnpts = npts;
    lbad = badval;
    status =
# ifdef hpux
	    do_rgrid
# else
	    do_rgrid_
# endif
		    (pinfo->grid, &lxdim, &lydim, &lnpts,
		     data, &lbad, xpos, ypos, &fxmin, &fymin,
		     &fxmax, &fymax, pinfo->scratch);
    switch ( status )
    {
	case RG_NOTENUFPTS:
	    msg_ELog ( EF_PROBLEM, 
	"X/Y Contour: Not enough data points to perform interpolation.");
	break;
	case RG_COLLINEAR:
	    msg_ELog ( EF_PROBLEM, 
	"X/Y Contour: Data points are collinear can't perform interpolation.");
	break;
    }
    free ( xpos ); free (ypos); free(data);

    return ( pinfo->grid );
}

int
yGridIndex ( ydata, ydim, ystep, ymin, yscalemode )
DataValPtr	ydata;
int		ydim;
float		ystep;
DataValPtr	ymin;
unsigned short	yscalemode;
{
    int index;
    switch ( ydata->type )
    {
	case 't':
	if ( yscalemode & INVERT )
	{
	    index = (int)
	         ((float)((ydata->val.t.zt_Sec+ystep*0.5) - ymin->val.t.zt_Sec)/
			 ystep);
	}
	else
	{
	    index = (ydim-1)-(int)
	          ((float)((ydata->val.t.zt_Sec+ystep*0.5) - ymin->val.t.zt_Sec)/
			 ystep);
	}
	break;
	case 'f':
	if ( yscalemode & INVERT )
	{
	    index = (int)(((ydata->val.f+ystep*0.5) - ymin->val.f)/ ystep);
	}
	else
	{
	    index = (ydim-1)-(int)(((ydata->val.f+ystep*0.5) - ymin->val.f)/ ystep);
	}
	break;
    }
    if ( index < ydim ) 
	return ( index );
    else
	return ( -1 );
}
int
xGridIndex ( xdata, xdim, xstep, xmin, xscalemode )
DataValPtr	xdata;
int		xdim;
float		xstep;
DataValPtr	xmin;
unsigned short	xscalemode;
{
    int	index = -1;
    switch ( xdata->type )
    {
	case 't':
	    if ( xscalemode & INVERT )
	    {
		index = (xdim-1)-(int)
	          ((float)((xdata->val.t.zt_Sec+xstep*0.5) - xmin->val.t.zt_Sec)/
			 xstep);
	    }
	    else
	    {
		index = (int)
	          ((float)((xdata->val.t.zt_Sec+xstep*0.5) - xmin->val.t.zt_Sec)/
			 xstep);
	    }
	break;
	case 'f':
	    if ( xscalemode & INVERT )
	    {
		index = (xdim-1)-(int)(((xdata->val.f+xstep*0.5) - xmin->val.f)/ xstep);
	    }
	    else
	    {
		index = (int)(((xdata->val.f+xstep*0.5) - xmin->val.f)/ xstep);
	    }
	break;
    }
    if ( index < xdim ) 
	return ( index );
    else
	return ( -1 );
}
float
gridStep ( ddim, dataMin, dataMax )
int	ddim;
DataValPtr	dataMin,dataMax;
{
    float 	dstep = 0.0;
    switch ( dataMin->type )
    {
	case 't':
	    dstep = 
		(float)(dataMax->val.t.zt_Sec - dataMin->val.t.zt_Sec)/
			 ((float)ddim-1);
	break;
	case 'f':
	    dstep = (dataMax->val.f - dataMin->val.f)/ ((float)ddim-1);
	break;
    }
    return ( dstep );
}

void
gridRandomData(ginfo,xdata,ydata,zdata,npts,xmin,xmax,xscalemode,
		ymin,ymax,yscalemode,badval)
GridInfoPtr	ginfo;
DataValPtr	xdata,ydata,zdata;
int		npts;
DataValPtr	xmin,xmax;
unsigned short	xscalemode;
DataValPtr	ymin,ymax;
unsigned short	yscalemode;
float		badval;
{
    float	xstep,ystep;
    int		ix,iy,k;
    xstep = gridStep ( ginfo->xdim, xmin, xmax );
    ystep = gridStep ( ginfo->ydim, ymin, ymax );
    /*
     * Add each data point to the correct grid cell. Keep track of how
     * many points are contributing to the cell in "scratch"
     */
    for ( k = 0; k < npts; k++)
    {
	ix = xGridIndex ( &(xdata[k]), ginfo->xdim, xstep, xmin, xscalemode );
	iy = yGridIndex ( &(ydata[k]), ginfo->ydim, ystep, ymin, yscalemode );
	if ( ix >= 0 && iy >= 0 )
	{
	    if ( GRID(ginfo->grid,ix,iy,ginfo->ydim) == badval )
	    {
                GRID(ginfo->grid,ix,iy,ginfo->ydim) = zdata[k].val.f;
                GRID(ginfo->scratch,ix,iy,ginfo->ydim) = 1.0;
	    }
	    else
	    {
                GRID(ginfo->grid,ix,iy,ginfo->ydim) += zdata[k].val.f;
                GRID(ginfo->scratch,ix,iy,ginfo->ydim) += 1.0;
	    }
	}
    }
    /*
     * Now average the points at each grid cell.
     */
    for ( iy = 0; iy < ginfo->ydim; iy++)
    {
        for ( ix = 0; ix < ginfo->xdim; ix++)
        {
	    if ( GRID(ginfo->grid,ix,iy,ginfo->ydim) != badval )
	    {
                GRID(ginfo->grid,ix,iy,ginfo->ydim) /= 
        	    GRID(ginfo->scratch,ix,iy,ginfo->ydim);
                GRID(ginfo->scratch,ix,iy,ginfo->ydim) = -1.0;
	    }
        }
    }
}

float *
xy_InterpolateLinearOnY( ginfo, xmin, xmax, ymin, ymax,badval )
GridInfoPtr	ginfo;
DataValPtr	xmin,xmax;
DataValPtr	ymin,ymax;
float		badval;
{
    int 	iy,ix;
    int		nx;
    float	zstep;
    int		obsid;
    float	dz;
    int		*dataLoc;

    dataLoc = (int*)calloc( ginfo->xdim, sizeof(int));

    for ( iy = 0; iy < ginfo->ydim; iy++)
    {
	nx = 0;
	/*
	 * First find all the observed values at this y-level.
	 */
        for ( ix = 0; ix < ginfo->xdim; ix++)
        {
	    if ( GRID(ginfo->grid,ix,iy,ginfo->ydim) != badval )
	    {
		dataLoc[nx] = ix;
		nx++;
	    }
        }
	/*
	 * Now interpolate missing values at this y-level.
	 */
        for ( obsid = 0; obsid < nx-1; obsid++)
        {
	    dz = GRID(ginfo->grid,dataLoc[obsid+1],iy,ginfo->ydim)-
		     GRID(ginfo->grid,dataLoc[obsid],iy,ginfo->ydim);
	    zstep = dz/(dataLoc[obsid+1]-dataLoc[obsid]);

	    for ( ix = dataLoc[obsid]+1; ix < dataLoc[obsid+1]; ix++)
	    {
		GRID(ginfo->grid,ix,iy,ginfo->ydim) = 
		     GRID(ginfo->grid,dataLoc[obsid],iy,ginfo->ydim) +
		   (zstep*(ix - dataLoc[obsid]));
	    }
	}
    }
    free(dataLoc);
    return ( ginfo->grid );
}
