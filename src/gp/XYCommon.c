/*
 * Routines common to XY-Type plots
 */
static char *rcsid = "$Id: XYCommon.c,v 1.4 1992-01-29 22:26:51 barrett Exp $";
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
# include "DrawText.h"
# include "XYCommon.h"

void
xy_GetScaleInfo(pd,c,dim,info)
plot_description	pd;
char			*c;
char			dim;
short			*info; /* return */
/*
 * Retrieve the scaling information from the plot-description.
 * pd - the plot-description to search
 * c - the component to search in the plot-description
 * dim - requested dimension. 'x' or 'y'
 * info - the scaling information defined for this dimension including:
 * pd-param: (xy)-scale-<dim>-mode
 *	  mode == AUTO || MANUAL    (default==AUTO)
 * pd-param: (xy)-scale-<dim>-style
 *	  style == INVERT || REGULAR (default==REGULAR)
 *
 */
{
    char	keyword[20];
    char	string[80];
    strcpy(keyword, "scale-");
    keyword[6] = dim;
    keyword[7] = '\0';
    strcat(keyword, "-mode");
    *info = 0;
    if ( pda_Search (pd, c, keyword, "xy", string, SYMT_STRING))
    {
	if ( strcmp(string,"autoscale")==0)
	    *info = *info | AUTO;
	else if ( strcmp(string,"manual")==0)
	    *info = *info | MANUAL;
	else	
	{
    	    msg_ELog (EF_PROBLEM, 
		"Unknown '%c' scaling mode: %s. Using autoscaling",
		dim,string);
	    *info = *info | AUTO;
	}
    }
    else
    {
	*info = *info | AUTO;
    }
    strcpy(keyword, "scale-");
    keyword[6] = dim;
    keyword[7] = '\0';
    strcat(keyword, "-style");
    if ( pda_Search (pd, c, keyword, "xy", string, SYMT_STRING))
    {
	if ( strcmp(string,"invert")==0)
	    *info = *info | INVERT;
	else	
    	    msg_ELog (EF_PROBLEM, 
		"Unknown '%c' scaling style: %s.", dim,string);
    }
}

void
xy_SetScaleBounds(pd,c,dim,dimtype,min,max)
plot_description	pd;
char			*c;
char			dim;
char			dimtype;
DataValPtr		min,max;
/*
 * For a given dimension, record the current scale min and max bounds
 * in the plot-description.
 * pd - plot-description
 * c - component in the plot-description to modify
 * dim - dimension for which bounds are being recorded, ('x' or 'y')
 * dimtype - data type of 'min' and 'max' ( 'f' or 't')
 * min,max - value of minimum scale bound and maximum scale bound
 *           respectively
 */
{
    char	minkey[32];
    char	maxkey[32];
    short	info;

    xy_GetScaleInfo(pd,c,dim,&info);
    if ( info & AUTO )
    {
        strcpy(minkey, "auto-scale-");
        minkey[11] = dim;
        minkey[12] = '\0';
        strcat(minkey, "-min");
        strcpy(maxkey, "auto-scale-");
        maxkey[11] = dim;
        maxkey[12] = '\0';
        strcat(maxkey, "-max");
    }
    else
    {
        strcpy(minkey, "scale-");
        minkey[6] = dim;
        minkey[7] = '\0';
        strcat(minkey, "-min");
        strcpy(maxkey, "scale-");
        maxkey[6] = dim;
        maxkey[7] = '\0';
        strcat(maxkey, "-max");
    }
    switch ( dimtype )
    {
	case 'f':
            pd_Store(pd,c,maxkey,(char *)&(max->val.f),SYMT_FLOAT);
            pd_Store(pd,c,minkey,(char *)&(min->val.f),SYMT_FLOAT);
	break;
	case 't':
            pd_Store(pd,c,maxkey,(char *)&(max->val.t),SYMT_DATE);
            pd_Store(pd,c,minkey,(char *)&(min->val.t),SYMT_DATE);
	break;
    }

}
void
xy_GetCurrentScaleBounds(pd,c,dim,dimtype,min,max)
plot_description	pd;
char			*c;
char			dim;
char			dimtype;
DataValPtr		min,max;
/*
 * Find the scale bounds that apply to this component.
 * pd - the plot-description to search
 * c - the component to search
 * dim - the dimension to find ( 'x' or 'y' )
 * dimtype - the data type of the dimension data ( 'f' or 't' )
 * min,max - the (returned) scale min and max bounds respectively.
 *
 * "xy_GetCurrentScaleBounds" will first search the given component, 
 * then if the parameter scale-<dim>-min or scale-<dim>-max is not
 * found, the global component and defaults will be searched respectively
 * with the qualifier "xy" prepended to the parameter name.
 */
{
    char	minkey[32];
    char	maxkey[32];
    short	info;
    xy_GetScaleInfo(pd,c,dim,&info);
    if ( info & AUTO )
    {
        strcpy(minkey, "auto-scale-");
        minkey[11] = dim;
        minkey[12] = '\0';
        strcat(minkey, "-min");
        strcpy(maxkey, "auto-scale-");
        maxkey[11] = dim;
        maxkey[12] = '\0';
        strcat(maxkey, "-max");
    }
    else
    {
        strcpy(minkey, "scale-");
        minkey[6] = dim;
        minkey[7] = '\0';
        strcat(minkey, "-min");
        strcpy(maxkey, "scale-");
        maxkey[6] = dim;
        maxkey[7] = '\0';
        strcat(maxkey, "-max");
    }

    min->type = dimtype;
    max->type = dimtype;
    switch ( dimtype )
    {
	case 'f':
            if (!pda_Search(pd,c,maxkey,"xy",(char *)&(max->val.f),SYMT_FLOAT)||
                !pda_Search(pd,c,minkey,"xy",(char *)&(min->val.f),SYMT_FLOAT)
	       )
	    {
		min->val.f =  9999.0;
		max->val.f = -9999.0;
	    }
	break;
	case 't':
            if (!pda_Search(pd,c,maxkey,"xy",(char *)&(max->val.t),SYMT_DATE) ||
                !pda_Search(pd,c,minkey,"xy",(char *)&(min->val.t),SYMT_DATE)
	       )
	    {
		min->val.t.ds_yymmdd = 0;
		min->val.t.ds_hhmmss = 0;
		max->val.t.ds_yymmdd = 0;
		max->val.t.ds_hhmmss = 0;
	    }
	break;
	default:
	    msg_ELog ( EF_PROBLEM, "Invalid data type: '%c'",dimtype);
	break;
    }
}

void
xy_SetPrivateDD ( pd, c, btime,etime,npnts)
plot_description pd;
char	*c;
time	*btime,*etime;	
int	*npnts;
/*
 * Store the private data descriptors "data-end-time", "data-begin-time"
 * and "data-ndatapoints".  These are meant to record the current state
 * of the amount and span of data currently displayed on the plot
 * for a given component.  These are "private" in the sense that they
 * are changed regularly by the plot routine and should not be tampered
 * with by "manual" means.
 */
{
    if ( btime ) pd_Store (pd, c, "data-begin-time",(char*)btime,SYMT_DATE);
    if ( etime ) pd_Store (pd, c, "data-end-time",(char*)etime,SYMT_DATE);
    if ( npnts ) pd_Store (pd, c, "data-ndatapoints",(char*)npnts,SYMT_INT);
}

void
xy_GetDataDescriptors( pd,c,update,btime,etime,bold,eold,dmode,ndat)
plot_description pd;
char	*c;
bool	update;
time	*btime,*etime;	
time	*bold,*eold;
int	*dmode;
int	*ndat;
/*
 * Get data descriptors.  "xy_GetDataDescriptors" returns the maximum
 * range of data times that are requested by the plot-description.
 * pd - the plot-description
 * c - the component name
 * update - true if this is an update plot
 * btime,etime - (return) The target data begin and end times needed to
 *	to make the plot completely upto date.
 * bold,eold - (return) The begin and end times of data that already
 *	exists on the plot.  if (!update) bold and eold will be set
 * 	to zero.
 */
{
    char	span[80];
    char	mode[80];
    int		spanSec;
    unsigned long spanTime;

    /* 
     * If snap-shot mode, then use the current PLOT-TIME for begin and
     * end times.  If series mode, then compute the target begin time at
     * the appropriate span-offset from PLOT-TIME.
     */
    *etime = PlotTime;
    *btime = PlotTime;
    *dmode = DATA_SNAPSHOT;
    if(pda_Search (pd, c, "data-mode","xy", (char*)mode,SYMT_STRING))
    {
	if ( strcmp(mode,"series")==0) 
	{
	    *dmode = DATA_SERIES;
            if(pda_Search (pd, "global", "series-span","xy", 
			(char*)span,SYMT_STRING))
    	    {
		if ( (spanSec = pc_TimeTrigger(span)) == 0 )
		{
	    	    msg_ELog (EF_PROBLEM, "Unparseable series span: %s",span);
		}
		spanTime = GetSec(*etime) - spanSec;
		lc_GetTime ( btime, spanTime );
    	    }
	}
    }
    /*
     * Now check for the private data descriptors
     */
    if ( update )
    {
        pda_Search ( pd, c, "data-begin-time", NULL, (char*)bold, SYMT_DATE); 
        pda_Search ( pd, c, "data-end-time", NULL, (char*)eold, SYMT_DATE); 
        pda_Search ( pd, c, "data-ndatapoints", NULL, (char*)ndat, SYMT_INT); 
    }
    else
    {
	bold->ds_yymmdd = 0;
	bold->ds_hhmmss = 0;
	eold->ds_yymmdd = 0;
	eold->ds_hhmmss = 0;
	*ndat = 0;
    }
}

void
xy_GetPlotColors(pd,c,nplat,datacolor,topAnnColor)
plot_description pd;
char	*c;
int	nplat;
char	*datacolor[MAX_PLAT];
char	*topAnnColor;
/*
 * Get the color names for "ta-color" and "platform-color"
 * from the plot description.
 * pd - the plot-description to search
 * c - the component to search
 * nplat - the number of colors expected for "platform-color"
 * datacolor - the (returned) list of platform colors.
 * topAnnColor - the (returned) top annotation color
 * 
 * If datacolor or topAnnColor is NULL, then that value will not
 * be searched for.
 */
{
	char	colors[80];
	char	*colorlist[MAX_PLAT];
 	int 	ncol;
	int	i;
/*
 * Get color for data from each platform
 */
	if ( datacolor )
	{
	    if ( pda_Search (pd, c, "platform-color", NULL,colors, SYMT_STRING))
	    {
	        ncol = CommaParse( colors, colorlist);
	        if ( ncol < nplat )
	        {
		    msg_ELog ( EF_INFO, 
    "%s Not enough platform-colors specified, filling in with white.",c);
	        }
	        for ( i = 0; i < nplat; i++)
	        {
	            if (GWDepth(Graphics) == 1 || i >= ncol )
		         strcpy(datacolor[i], "white");
		    else
		         strcpy(datacolor[i], colorlist[i]);
	        }
	    }
	    else
	    {
		msg_ELog ( EF_INFO,
			"%s: No platform-color(s) specified, using white.",c);
		for ( i = 0; i < nplat; i++ )
		{
		    strcpy(datacolor[i], "white");
		}
	    }
	}
/*
 * Get color for top annotation
 */
	if ( topAnnColor )
	{
	    if (! pda_Search (Pd,"global","ta-color", NULL,
			topAnnColor,SYMT_STRING))
		strcpy(topAnnColor, "white");
	}
}

int
xy_AvailableData(pid,bTimeTarget,eTimeTarget,eTimeOld,bTimeReq, eTimeReq)
PlatformId      pid;
time            bTimeTarget,eTimeTarget,eTimeOld;
time            *bTimeReq,*eTimeReq;
/*
 * Find out the maximum begin and end times of actual data available
 * within a requested range.
 * pid - the platform id.
 * bTimeTarget,eTimeTarget - the begin and end times of the requested
 * 	range of data.
 * eTimeOld - the ending time of data already retrieved.
 *	If no data previously exists, the time should be 000000 000000.
 * bTimeReq,eTimeReq - the begin and end times of data available to
 *	fill the target range.
 * If data is actually available, then return 1 else 0
 */
{
    int available = 1;
    *eTimeReq = eTimeOld;
    *bTimeReq = bTimeTarget;
    if (! ds_DataTimes (pid, &eTimeTarget, 1, DsBefore, eTimeReq))
    {
        msg_ELog (EF_INFO, "No data before %d %d", eTimeTarget.ds_yymmdd,
                                eTimeTarget.ds_hhmmss);
        available = 0;
    }
    /*
     * If no data previously exists, then get the time of the oldest
     * available data within the target range.
     */
    if ( eTimeOld.ds_yymmdd == 0 )
    {
        if (! ds_DataTimes (pid, &bTimeTarget, 1, DsBefore, bTimeReq))
        {
            msg_ELog (EF_INFO, "No data before %d %d", bTimeTarget.ds_yymmdd,
                                bTimeTarget.ds_hhmmss);
            if (! ds_DataTimes (pid, &bTimeTarget, 1, DsAfter, bTimeReq))
            {
                msg_ELog (EF_INFO, "No data after %d %d", bTimeTarget.ds_yymmdd,
                                bTimeTarget.ds_hhmmss);
                available = 0;
            }
        }
    }
    else
        *bTimeReq = eTimeOld;

    if ( available && GetSec(*eTimeReq) < GetSec(*bTimeReq) )
    {
        available = 0;
        msg_ELog (EF_INFO, "No new data for at %d %d",eTimeTarget.ds_yymmdd,
                                eTimeTarget.ds_hhmmss);
    }
    return (available);
}

void
xy_GetDataMinMax(update, min, max, data, npts)
bool    update;
DataValPtr      min,max;
DataValPtr      data;
int             npts;
/*
 * Get the min and max data value for an array of data.
 * If update is false, min and max will be reset and their returned
 * values will be the true min and max of the data set.
 * If update is true, min and max will be left as their current (passed in)
 * values and will only be changed if the values in the current data
 * set exceed them.
 */
{
   int  i;
   if ( !update && data )
   {
        *min = data[0];
        *max = data[0];
   }
   for ( i = 0; i < npts; i++)
   {
        if ( lc_CompareData( &(data[i]), max ) > 0 )
                        *max = data[i];
        if ( lc_CompareData( &(data[i]), min ) < 0 )
                        *min = data[i];
   }
}

void
xy_AdjustAxes(pd,c,xtype,xrescale,ytype,yrescale )
plot_description	pd;
char			*c;
char			xtype;
int			xrescale;
char			ytype;
int			yrescale;
/*
 * Adjust the plot-description parameters controlling the limits of plotted
 * axes associated with a given component. If for any horizontal axis
 * plotted, xmin or xmax is different from the current axis min and max
 * the axis plot-description parameters will be adjusted to reflect
 * the new values. Similarly, if ymin and ymax are different from the existing
 * left or right axis values, they will also be changed.
 * If any one axis is adjusted, the plot-description will be changed
 * and "xy_AdjustAxes" will return 1, indicating the need for the component
 * axes to be redrawn to reflect the change. If no axes are adjusted,
 * then 0 is returned.
 */
{
    char	datatype;
    int		computed;
    
    if ( ac_PlotAxis ( pd,c,'t') )
    {
        computed = ac_QueryAxisState(pd,c,'t',&datatype);
	computed = 0;
	if ( datatype == 'n' ) 
	{
	    ac_UpdateAxisState ( pd,c,'t',&xtype,&computed);
	}
	else if ( xrescale )
	{
	    ac_UpdateAxisState ( pd, c, 't',NULL, &computed );
	}
    }
    if ( ac_PlotAxis ( pd,c,'b') )
    {
        computed = ac_QueryAxisState(pd,c,'b',&datatype);
	computed = 0;
	if ( datatype == 'n' ) 
	{
	    ac_UpdateAxisState ( pd,c,'b',&xtype,&computed);
	}
	else if ( xrescale )
	{
	    ac_UpdateAxisState ( pd, c, 'b',NULL, &computed );
	}
    }
    if ( ac_PlotAxis ( pd,c,'l') )
    {
        computed = ac_QueryAxisState(pd,c,'l',&datatype);
	computed = 0;
	if ( datatype == 'n' ) 
	{
	    ac_UpdateAxisState ( pd,c,'l',&ytype,&computed);
	}
	else if ( yrescale )
	{
	    ac_UpdateAxisState ( pd, c, 'l',NULL, &computed );
	}
    }
    if ( ac_PlotAxis ( pd,c,'r') )
    {
        computed = ac_QueryAxisState(pd,c,'r',&datatype);
	computed = 0;
	if ( datatype == 'n' ) 
	{
	    ac_UpdateAxisState ( pd,c,'r',&ytype,&computed);
	}
	else if ( yrescale )
	{
	    ac_UpdateAxisState ( pd, c, 'r',NULL, &computed );
	}
    }
}
