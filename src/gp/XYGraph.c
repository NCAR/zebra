/*
 * XY-Graph plotting module
 */
static char *rcsid = "$Id: XYGraph.c,v 1.4 1992-01-03 00:28:10 barrett Exp $";
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
# include "DrawText.h"

/*
 * General definitions
 */
# define BADVAL		-999.0
# define MAX_PLAT	10

extern void xy_Graph();

/*
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

/*
 * Color array and indices
 */
extern XColor	*Colors;
extern int	Ncolors;
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
	int	status, i,  plat, nplat,ii,jj;
	int	npts = 0;
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
	int	fudge;
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
	DataValRec	oldmin,oldmax;
	unsigned short	xscalemode,yscalemode;
	char	xtype = 'f', ytype = 'f';
	int	fcount;
        int	xdim,ydim;
	int	plotAxis[4];
	int	saveConfig;
	int	dmode ;
	char	style[80];
	char	datalabel[80];
/*
 * Get X-Y Graph Required parameters:
 * "platform","x-field", "y-field", "wind-coords", "color-table", "org"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"x-field",NULL, &(dataNames[0]), SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"y-field",NULL, &(dataNames[1]), SYMT_STRING);
	ok = pda_ReqSearch (Pd, c, "color-table", NULL,ctname,SYMT_STRING);

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
 *  "style" - "point" or "line"
 */
        if ( !pda_Search (Pd,c,"representation-style", NULL,
		(char *) &style, SYMT_STRING))
        {
            strcpy(style, "line" );
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
	xy_GetScaleMode(Pd,c,'x',&xscalemode);
	xy_GetScaleMode(Pd,c,'y',&yscalemode);
	xy_GetCurrentScaleBounds(Pd,c,'x',xtype,&xmin,&xmax);
	xy_GetCurrentScaleBounds(Pd,c,'y',ytype,&ymin,&ymax);
	xy_GetComponentAxes(Pd,c,plotAxis);
        xy_GetDataDescriptors(Pd, c, update, 
			      &bTimeTarget,&eTimeTarget, 
			      &bTimeOld,&eTimeOld,
			      &dmode );

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
		if ( ts_GetSec(eTimeTarget) > ts_GetSec((xmax.val.t)) )
		{
		    TriggerGlobal = 1;
		}
	    }
	}
	if ( (yscalemode & AUTO) && ytype == 't' )
	{
	    if ( update )
	    {
		if ( ts_GetSec(eTimeTarget) > ts_GetSec((ymax.val.t)) )
		{
		    TriggerGlobal = 1;
		}
	    }
	}
	for ( i = 0; i < nplat; i++)
	    linecolor[i] = (char*)calloc(64,sizeof(char));
	xy_GetPlotAttr(Pd,c,nplat,linecolor,tadefcolor);
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
 * Attempt to load color table 
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);
	if (Ncolors < 1)
	{
		msg_ELog(EF_PROBLEM, "XY-Graph color table too small");
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
	    An_TopAnnot ("X/Y Graph:", Tadefclr.pixel);
	    An_TopAnnot (c, Tadefclr.pixel);
	    lw_OvInit ("PLATFORM    TIME\n");
	}
/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Add this platform to the annotation
	 */
	    if (! update)
	    {
		sprintf(datalabel, "%s-%s:%s %s", 
			pnames[plat], fnames[0][plat],
			fnames[1][plat], linecolor[plat]);
		An_AddAnnotProc ( An_ColorString, c, datalabel,
		strlen(datalabel)+1,25, FALSE,FALSE);
	    }
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
		npts = 0;
	        xdata[plat] = NULL;
	        ydata[plat] = NULL;
		continue;
	    }
	    else
            {
	        if ( xyOrg == OrgScalar )
		    npts = dobj->do_npoint;
	        else if ( xyOrg == Org1dGrid )
		    npts = dobj->do_desc.d_rgrid.rg_nX * dobj->do_npoint;

	        xdata[plat] = (DataValPtr)calloc( npts , sizeof(DataValRec));
	        ydata[plat] = (DataValPtr)calloc( npts , sizeof(DataValRec));
	    }
	    count = 0;
	    /*
	     * Extract field data arrays.
	     */
	    msg_ELog ( EF_DEBUG, 
		   "X-Y Graph found %d data points for component %s.",npts,c);
	    for ( ii = 0; ii < npts; ii++ )
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
		} while ( jj < fcount && ii < npts);
		if ( ii < npts )
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
	    ds_FreeDataObject (dobj);
	}
/*
 * Now set the current scale bounds.
 */
	fudge = 300;
	autoTime = ts_GetSec(eTimeTarget) + 
		(long)((ts_GetSec(eTimeTarget)-ts_GetSec(bTimeTarget))*0.025);
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
	xy_SetScaleBounds(Pd,c,'x',xtype,&xmin,&xmax);
	xy_SetScaleBounds(Pd,c,'y',ytype,&ymin,&ymax);

	/*
	 * Now check if scale-bounds are different from the axis-bounds
	 */
	if ( plotAxis[AXIS_TOP] )
	{
	    status = ac_AxisState( Pd,c,'t',xtype,&oldmin, &oldmax);
	    if ( !status ||
		 lc_CompareData(&oldmin,&xmin) != 0 ||
	         lc_CompareData(&oldmax,&xmax) != 0 )
	    {
		ac_UpdateAxisState(Pd,c,'t',xtype,&xmin,&xmax);
	        if ( update ) TriggerGlobal = 1;
	    }
	}
	if ( plotAxis[AXIS_BOTTOM] )
	{
	    status = ac_AxisState( Pd,c,'b',xtype,&oldmin, &oldmax);
	    if ( !status ||
		 lc_CompareData(&oldmin,&xmin) != 0 ||
	         lc_CompareData(&oldmax,&xmax) != 0 )
	    {
		ac_UpdateAxisState(Pd,c,'b',xtype,&xmin,&xmax);
	        if ( update ) TriggerGlobal = 1;
	    }
	}
	if ( plotAxis[AXIS_LEFT] )
	{
	    status = ac_AxisState( Pd,c,'l',ytype,&oldmin, &oldmax);
	    if ( !status ||
		 lc_CompareData(&oldmin,&ymin) != 0 ||
	         lc_CompareData(&oldmax,&ymax) != 0 )
	    {
		ac_UpdateAxisState(Pd,c,'l',ytype,&ymin,&ymax);
	        if ( update ) TriggerGlobal = 1;
	    }
	}
	if ( plotAxis[AXIS_RIGHT] )
	{
	    status = ac_AxisState( Pd,c,'r',ytype,&oldmin, &oldmax);
	    if ( !status ||
		 lc_CompareData(&oldmin,&ymin) != 0 ||
	         lc_CompareData(&oldmax,&ymax) != 0 )
	    {
		ac_UpdateAxisState(Pd,c,'r',ytype,&ymin,&ymax);
	        if ( update ) TriggerGlobal = 1;
	    }
	}

/*
 * Plot the data.
 */
	if ( !TriggerGlobal )
	{
	    lc_SetUserCoord ( &xmin,&xmax,&ymin,&ymax);
	    gp_Clip( &xmin, &ymin,&xmax,&ymax, xscalemode, yscalemode );
	    msg_ELog ( EF_DEBUG, 
	       "X-Y Graph plotting %d data points for component %s.",npts,c);
	    for (plat = 0; plat < nplat; plat++)
	    {
		if ( strcmp(style,"point")==0)
		{
                    if ( npts > 0 )
		        gp_Points( xdata[plat],ydata[plat],npts,
                          lcolor[plat], xscalemode, yscalemode);
	            else
		    {
	    	        msg_ELog ( EF_INFO, 
       				"X-Y Graph: zero plottable points for %s.",c);
		    }
		}
		else
		{
                    if ( npts > 1 )
		        gp_Pline( xdata[plat],ydata[plat],npts,L_solid,
                          lcolor[plat], xscalemode, yscalemode);
	            else
		    {
	    	        msg_ELog ( EF_INFO, 
       "X-Y Graph less than 2 data points for component %s, not plotted.",c);
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
