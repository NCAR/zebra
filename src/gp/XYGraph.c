/*
 * XY-Graph plotting module
 */
static char *rcsid = "$Id: XYGraph.c,v 1.6 1992-02-19 23:54:17 barrett Exp $";
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

/*
 * General definitions
 */
# define CROSS		1
# define XMARK		2

extern void xy_Graph();

/*
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

/*
 * Color array and indices
 */
extern XColor 	Tadefclr;


void
xy_Graph(c, update)
char	*c;
bool	update;
/*
 * Draw an xy-graph on the given component
 */
{
	bool	ok;
	int	i,  plat, nplat,ii,jj;
	int	xrescale = 0, yrescale = 0;
	int	npts[MAX_PLAT];
	int	nxfield,nyfield;
	int	count;
	int 	nPlotted;
	char	platforms[80], tadefcolor[30];
	char	ctname[20];
	char	dataNames[2][80];
	char	*flist[2];
	time    eTimeTarget,bTimeTarget,bTimeOld,eTimeOld;
	time    eTimeReq,bTimeReq;
	int	change;
	long	autoTime;
	char	*pnames[MAX_PLAT];
	char	*fnames[2][MAX_PLAT];
	PlatformId	pid;
	DataObject	*dobj = NULL;
	XColor	*lcolor;
	char	*linecolor[MAX_PLAT];
	DataOrganization	xyOrg;
	DataValPtr	*xdata,*ydata;
	DataValRec	xmin,xmax,ymin,ymax;
	DataValRec	oldxmin,oldxmax,oldymin,oldymax;
	unsigned short	xscalemode,yscalemode;
	char	xtype = 'f', ytype = 'f';
	int	fcount;
        int	xdim,ydim;
	int	saveConfig;
	int	dmode ;
	char	style[80];
	char	datalabel[80];
	char	timelabel[80];
	int	sideAnnot;
/*
 * Get X-Y Graph Required parameters:
 * "platform","x-field", "y-field", "wind-coords", "org"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"x-field",NULL, (char*)&(dataNames[0]), SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"y-field",NULL, (char*)&(dataNames[1]), SYMT_STRING);

	if (! ok) return;
/*
 * Parse platform and field name information. 
 */
	nplat = CommaParse (platforms, pnames);
	nxfield = CommaParse (dataNames[0], fnames[0]);
	nyfield = CommaParse (dataNames[1], fnames[1]);
	if ( nxfield != nplat && nyfield != nplat  )
	{
	    msg_ELog ( EF_PROBLEM, 
		"X/Y Graph: number of fields of doesn't correspond to number of platforms.");
	    return;
	}
/* for now, only allow max MAX_PLAT platforms */
	if (nplat > MAX_PLAT)
		nplat = MAX_PLAT;
/*
 *  Get optional "simple" parameters.
 *  "style" - "point" , "line", "cross" or "xmark"
 */
        if ( !pda_Search (Pd,c,"representation-style", "xy-simple",
		(char *) &style, SYMT_STRING))
        {
            strcpy(style, "line" );
        }
        if ( !pda_Search (Pd,c,"do-side-annotation", "xy-simple",
		(char *) &sideAnnot, SYMT_BOOL))
        {
            sideAnnot = True;
        }

/*
 * Allocate memory for data and attibutes
 */
	lcolor = (XColor*)calloc ( nplat, sizeof(XColor));
	xdata = (DataValPtr*)calloc (nplat, sizeof(DataValPtr));
	ydata = (DataValPtr*)calloc (nplat, sizeof(DataValPtr));
/*
 * Initialize local environment.
 */
/* lc_SetUserCoord ( 0.0,1.0,0.0,1.0);*/
/*
 * Get Generic plot description information.
 */
	if ( strcmp( fnames[0][0],"time" ) == 0 )
	    xtype = 't';
	if ( strcmp( fnames[1][0],"time" ) == 0 )
	    ytype = 't';
	xy_GetScaleInfo(Pd,c,'x',&xscalemode);
	xy_GetScaleInfo(Pd,c,'y',&yscalemode);
	xy_GetCurrentScaleBounds(Pd,c,'x',xtype,&oldxmin,&oldxmax,fnames[0][0]);
	xy_GetCurrentScaleBounds(Pd,c,'y',ytype,&oldymin,&oldymax,fnames[1][0]);
	xmin = oldxmin;
	xmax = oldxmax;
	ymin = oldymin;
	ymax = oldymax;
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
		msg_ELog ( EF_DEBUG, 
		   "%s new Axis EndTime = %d %d old Axis EndTime = %d %d",
			c,
			eTimeTarget.ds_yymmdd,eTimeTarget.ds_hhmmss,
			xmax.val.t.ds_yymmdd,xmax.val.t.ds_hhmmss);
		if ( GetSec(eTimeTarget) > GetSec((xmax.val.t)) )
		{
		    TriggerGlobal = 1;
		}
	    }
	}
	if ( (yscalemode & AUTO) && ytype == 't' )
	{
	    if ( update )
	    {
		if ( GetSec(eTimeTarget) > GetSec((ymax.val.t)) )
		{
		    TriggerGlobal = 1;
		}
	    }
	}
	for ( i = 0; i < nplat; i++)
	    linecolor[i] = (char*)calloc(64,sizeof(char));
	xy_GetPlotColors(Pd,c,nplat,linecolor,tadefcolor);
/*
 **********************************************************
 * X-dependent set-up
 **********************************************************
 */
/*
 * Get X-pixel for colors...
 */
	for ( i =0 ; i < nplat; i++)
	{
	    if(! ct_GetColorByName(linecolor[i], &(lcolor[i])))
	    {
		msg_ELog(EF_PROBLEM, "Can't get X line color: '%s'.",
				linecolor[i]);
		ct_GetColorByName("white", &(lcolor[i]));
	    }
	}
	if (! ct_GetColorByName(tadefcolor, &Tadefclr))
	{
	    msg_ELog(EF_PROBLEM, "Can't get X annotation color: '%s'.",
			tadefcolor);
	    strcpy(tadefcolor, "white");
	    ct_GetColorByName(tadefcolor, &Tadefclr);
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
	    An_TopAnnot ("X/Y Graph:", Tadefclr.pixel);
	    An_TopAnnot (c, Tadefclr.pixel);
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

	    /*
	     * Set up the field-names for the data retrieval request.
	     */
	    fcount = 0;
	    if ( xtype != 't' )
	    {
		xdim = fcount;
	        flist[fcount] = fnames[0][plat]; fcount++;
	    }
	    if ( ytype != 't' )
	    {
		ydim = fcount;
	        flist[fcount] = fnames[1][plat]; fcount++;
	    }
	    /*
 	     * Determine times of data to request.
 	     */
	    if (xy_AvailableData(pid,bTimeTarget,eTimeTarget,eTimeOld,
				&bTimeReq, &eTimeReq))
	    {
		msg_ELog ( EF_DEBUG, 
		   "%s Data request times begin %d %d end = %d %d",
			c,
			bTimeTarget.ds_yymmdd,bTimeTarget.ds_hhmmss,
			eTimeTarget.ds_yymmdd,eTimeTarget.ds_hhmmss);
		if ( dmode == DATA_SNAPSHOT )
		{
                    dobj =ds_GetObservation (pid, flist, fcount, &eTimeReq, 
			xyOrg, 0.0, BADVAL);
		}
		else
		{
	            dobj = ds_GetData (pid, flist, fcount,&bTimeReq,&eTimeReq, 
			xyOrg, 0.0, BADVAL);
		}
	    }

	    if (! dobj)
	    {
		msg_ELog (EF_PROBLEM, 
				"Unable to get field data for '%s' at %d %06d", 
				pnames[plat], eTimeReq.ds_yymmdd, 
			eTimeReq.ds_hhmmss);
		npts[plat] = 0;
	        xdata[plat] = NULL;
	        ydata[plat] = NULL;
		continue;
	    }
	    else
            {
	        if ( xyOrg == OrgScalar )
		    npts[plat] = dobj->do_npoint;
	        else if ( xyOrg == Org1dGrid )
		    npts[plat] = dobj->do_desc.d_rgrid.rg_nX * dobj->do_npoint;

	        xdata[plat] = (DataValPtr)calloc(npts[plat],sizeof(DataValRec));
	        ydata[plat] = (DataValPtr)calloc(npts[plat],sizeof(DataValRec));
	    }
	    count = 0;
	    /*
	     * Extract field data arrays.
	     */
	    msg_ELog ( EF_DEBUG, 
		   "X-Y Graph found %d data points for component %s.",npts[plat],c);
	    for ( ii = 0; ii < npts[plat]; ii++ )
	    {
		/* search for next good data point. */
		do {
		    for ( jj = 0; jj < fcount ; jj++)
		    {
			if(dobj->do_data[jj][ii] == BADVAL)
			{
			    ii++; break;
			}
		    }
		} while ( jj < fcount && ii < npts[plat]);
		if ( ii < npts[plat] )
		{
		  if ( xtype == 't' )
		  {
		    xdata[plat][count].val.t = dobj->do_times[xyOrg==OrgScalar?
			       ii : (int)(ii/dobj->do_desc.d_rgrid.rg_nX)];
		    xdata[plat][count].type = 't';
		  }
		  else
		  {
		    xdata[plat][count].val.f = dobj->do_data[xdim][ii];
		    xdata[plat][count].type = 'f';
		  }
		  if ( ytype == 't' )
		  {
		    ydata[plat][count].val.t = dobj->do_times[xyOrg==OrgScalar?
			       ii : (int)(ii/dobj->do_desc.d_rgrid.rg_nX)];
		    ydata[plat][count].type = 't';
		  }
		  else
		  {
		    ydata[plat][count].val.f = dobj->do_data[ydim][ii];
		    ydata[plat][count].type = 'f';
		  }
		  count += 1;
		}
	    }
	    npts[plat] = count;
	    /*
	     * Calculate autoscaled mins/maxs if necessary.
	     */
	    if ( (xscalemode & AUTO) && xtype != 't' )
	    {
	        xy_GetDataMinMax(update, &xmin, &xmax, xdata[plat], npts[plat]);
            }
	    if ( (yscalemode & AUTO) && ytype != 't' )
	    {
	        xy_GetDataMinMax(update, &ymin, &ymax, ydata[plat], npts[plat]);
            }
	    ds_FreeDataObject (dobj);
	    /*
	     * Do the side annotation for this data
	     */
	    if ( sideAnnot && npts[plat] > 0 && !update )
	    {
		sprintf(datalabel, "%s-%s:%s %s", 
			pnames[plat], fnames[0][plat],
			fnames[1][plat], linecolor[plat]);
		An_AddAnnotProc ( An_ColorString, c, datalabel,
		strlen(datalabel)+1,25, FALSE,FALSE);
		    ud_format_date ( timelabel, &eTimeReq, UDF_FULL );
		    sprintf(datalabel, "   %s %s", timelabel, linecolor[plat]);
		    An_AddAnnotProc ( An_ColorString, c, datalabel,
		    strlen(datalabel)+1,25, FALSE,FALSE);
	    }
	}
/*
 * Now set the current scale bounds.
 */
	/*
	 *  If this is a global update, and there's autoscaling going on,
	 *  set a fudge factor to bound the min and max of the data.
	 */
	autoTime = GetSec(eTimeTarget) + 
		(long)((GetSec(eTimeTarget)-GetSec(bTimeTarget))*0.025);
	if ( !update )
	{
	    if ( xscalemode & AUTO)
	    {
		switch ( xtype )
		{
		    case 't':
		        lc_GetTime( &(xmax.val.t), autoTime);
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
		        lc_GetTime( &(ymax.val.t), autoTime);
		        ymin.val.t = bTimeTarget;
		    break;
		    case 'f':
			ymin.val.f -= 5.0;
			ymax.val.f += 5.0;
		    break;
		}
	    }
	}
	if ( xscalemode & AUTO ) xy_SetScaleBounds(Pd,c,'x',xtype,&xmin,&xmax);
	if ( yscalemode & AUTO ) xy_SetScaleBounds(Pd,c,'y',ytype,&ymin,&ymax);
	/* 
	 * Trigger a global redraw to update the axis if they will have
	 * changed.
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
	    lc_SetUserCoord ( &xmin,&xmax,&ymin,&ymax);
	    gp_Clip( &xmin, &ymin,&xmax,&ymax, xscalemode, yscalemode );
	    for (plat = 0; plat < nplat; plat++)
	    {
	    msg_ELog ( EF_DEBUG, 
	       "X-Y Graph plotting %d data points for component %s.",npts[plat],c);
                if ( npts[plat] > 0 )
		{
		    if ( strcmp(style,"point")==0)
		    {
		        gp_Points( xdata[plat],ydata[plat],npts[plat],
                          lcolor[plat], xscalemode, yscalemode);
		    }
		    else if ( strcmp(style,"cross")==0)
		    {
		        gp_Symbol( xdata[plat],ydata[plat],npts[plat],
                          lcolor[plat], xscalemode, yscalemode,CROSS);
		    }
		    else if ( strcmp(style,"xmark")==0)
		    {
                    if ( npts[plat] > 0 )
		        gp_Symbol( xdata[plat],ydata[plat],npts[plat],
                          lcolor[plat], xscalemode, yscalemode,XMARK);
		    }
		    else if ( (strcmp(style,"line")==0) && (npts[plat] > 1) )
		    {
		        gp_Pline( xdata[plat],ydata[plat],npts[plat],L_solid,
                          lcolor[plat], xscalemode, yscalemode);
		    }
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
	    if ( xdata[i] ) free(xdata[i]); 
	    if ( ydata[i] ) free(ydata[i]);
	    free(linecolor[i]);
	}
	free(lcolor); free(xdata); free(ydata); 
}
# endif /* C_PT_XYGRAPH */
