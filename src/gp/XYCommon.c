/*
 * Routines common to XY-Type plots
 */
static char *rcsid = "$Id: XYCommon.c,v 1.14 1993-10-15 16:31:31 corbet Exp $";
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
# include "xdr_infinity.h"	/* needed for infinity kluge below */
# include "derive.h"
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "DrawText.h"
# include "XYCommon.h"

/*
 * Our routines.
 */
void	xy_Init FP ((UItime *));
void	xy_GetScaleInfo FP ((plot_description, char *, int, short *));
void	xy_SetScaleBounds FP ((plot_description, char *, int, int, DataValPtr,
		DataValPtr));
void	xy_TicInterval FP ((DataValPtr, DataValPtr, DataValPtr));
void	xy_GetCurrentScaleBounds FP ((plot_description, char *, int, int,
		DataValPtr, DataValPtr, char *));

void
xy_Init (t)
UItime *t;
/*
 * CAP Plot initialization.
 */
{
	lw_OvInit ("COMPONENT      PLATFORM   FIELD       TIME\n");
}


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
    char	minkey[32], maxkey[32], key1[32], key2[32], prefix[16];
    bool	autoscale;
    short	info;
    DataValRec	interval;

    xy_GetScaleInfo(pd,c,dim,&info);
    autoscale = (info & AUTO);

    strcpy (prefix, autoscale ? "auto-scale" : "scale");
    sprintf (minkey, "%s-%c-min", prefix, dim);
    sprintf (maxkey, "%s-%c-max", prefix, dim);

    switch ( dimtype )
    {
	case 'f':
            pd_Store(pd,c,maxkey,(char *)&(max->val.f),SYMT_FLOAT);
            pd_Store(pd,c,minkey,(char *)&(min->val.f),SYMT_FLOAT);
	break;
	case 't':
            pd_Store(pd,c,maxkey,(char *) &(max->val.t),SYMT_DATE);
            pd_Store(pd,c,minkey,(char *) &(min->val.t),SYMT_DATE);
	break;
    }
/*
 * If we're not autoscaling, we're done
 */
    if (! autoscale)
	return;
/*
 * Generate and store good tic intervals
 */
    xy_TicInterval (min, max, &interval);

    if (dim == 'x')
    {
	strcpy (key1, "axis-b-tic-interval");
	strcpy (key2, "axis-t-tic-interval");
    }
    else
    {
	strcpy (key1, "axis-l-tic-interval");
	strcpy (key2, "axis-r-tic-interval");
    }

    switch (dimtype)
    {
	char	sval[16];

	case 'f':
	    pd_Store (pd, c, key1, (char *)&(interval.val.f), SYMT_FLOAT);
	    pd_Store (pd, c, key2, (char *)&(interval.val.f), SYMT_FLOAT);
	break;
	case 't':
	    sprintf (sval, "%ds", interval.val.t.zt_Sec);
	    pd_Store (pd, c, key1, sval, SYMT_STRING);
	    pd_Store (pd, c, key2, sval, SYMT_STRING);
	break;
    }
}
void
xy_TicInterval (min, max, interval)
DataValPtr	min, max, interval;
/*
 * Find a good tic interval, given the min and max
 */
{
	float	span, tic_inc;
	int	i, tspan;
/*
 * Array of good time steps and the associated minimum span for each
 */
	struct
	{
		int	step, minspan;
	} timeSteps[] =
	{
		{86400,	345600},	/* 1d, 4d */
		{43200,	172800},	/* 12h, 2d */
		{21600,	86400},		/* 6h, 1d */
		{7200,	43200},		/* 2h, 12h */
		{3600,	21600},		/* 1h, 6h */
		{1800,	10800},		/* 30m, 3h */
		{900,	3600},		/* 15m, 1h */
		{300,	1800},		/* 5m, 30m */
		{120,	600},		/* 2m, 10m */
		{60,	300},		/* 1m, 5m */
		{30,	180},		/* 30s, 3m */
		{15,	60},		/* 15s, 1m */
		{5,	30},		/* 5s, 30s */
		{2,	10},		/* 2s, 10s */
		{1,	0},		/* 1s for anything smaller than 10s */
	};
/*
 * Handle float and time limits differently
 */
	switch (min->type)
	{
	/*
	 * Float values
	 */
	    case 'f':
	    /*
	     * Find a good interval based on the span
	     */
		span = max->val.f - min->val.f;

		tic_inc = pow (10.0, floor (log10 (fabs (span))));

		if (fabs (span / tic_inc) < 1.5)
			tic_inc *= 0.1;
		else if (fabs (span / tic_inc) < 3.0)
			tic_inc *= 0.2;
		else if (fabs (span / tic_inc) < 8.0)
			tic_inc *= 0.5;
	    /*
	     * Store the interval we found
	     */
		interval->type = 'f';
		interval->val.f = tic_inc;

		break;
	/*
	 * Time values
	 */
	    case 't':
	    /*
	     * Find the appropriate time interval from the table, based on
	     * the span between min and max
	     */
		tspan = max->val.t.zt_Sec - min->val.t.zt_Sec;
		for (i = 0; tspan < timeSteps[i].minspan; i++)
			/* nothing */;
	    /*
	     * Store the interval we found
	     */
		interval->type = 't';
		interval->val.t.zt_Sec = timeSteps[i].step;
		interval->val.t.zt_MicroSec = 0;

		break;
	/*
	 * Uh-oh.  Can't handle any others
	 */
	    default:
		msg_ELog (EF_PROBLEM, "xy_TicInterval can't handle type '%c'",
			min->type);
	}
}
void
xy_GetCurrentScaleBounds(pd,c,dim,dimtype,min,max,qual)
plot_description	pd;
char			*c;
char			dim;
char			dimtype;
DataValPtr		min,max;
char			*qual;
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
 * with the qualifier "qual" prepended to the parameter name. "qual" should
 * be the field-name.
 */
{
    char	minkey[32];
    char	maxkey[32];
    char	prefix[16];
    short	info;

    xy_GetScaleInfo(pd,c,dim,&info);

    strcpy (prefix, (info & AUTO) ? "auto-scale" : "scale");
    sprintf (minkey, "%s-%c-min", prefix, dim);
    sprintf (maxkey, "%s-%c-max", prefix, dim);

    min->type = dimtype;
    max->type = dimtype;
    switch ( dimtype )
    {
	case 'f':
            if (!pda_Search(pd,c,maxkey,qual,(char *)&(max->val.f),SYMT_FLOAT)||
                !pda_Search(pd,c,minkey,qual,(char *)&(min->val.f),SYMT_FLOAT)
	       )
	    {
		min->val.f =  9999.0;
		max->val.f = -9999.0;
	    }
	break;
	case 't':
            if (!pda_Search(pd,c,maxkey,qual, (char *) &(max->val.t), SYMT_DATE) ||
                !pda_Search(pd,c,minkey,qual, (char *) &(min->val.t), SYMT_DATE)
	       )
	    {
		min->val.t.zt_Sec = 0;
		min->val.t.zt_MicroSec = 0;
		max->val.t.zt_Sec = 0;
		max->val.t.zt_MicroSec = 0;
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
ZebTime	*btime,*etime;	
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
    if ( btime ) 
    {
	pd_Store (pd, c, "data-begin-time", (char*) btime, SYMT_DATE);
    }
    if ( etime ) 
    {
	pd_Store (pd, c, "data-end-time", (char*) etime, SYMT_DATE);
    }
    if ( npnts ) pd_Store (pd, c, "data-ndatapoints",(char*)npnts,SYMT_INT);
}

void
xy_GetDataDescriptors( pd,c,update,btime,etime,bold,eold,dmode,ndat)
plot_description pd;
char	*c;
bool	update;
ZebTime	*btime,*etime;	
ZebTime	*bold,*eold;
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
		btime->zt_Sec = etime->zt_Sec - spanSec;
    	    }
	}
    }
    /*
     * Now check for the private data descriptors
     */
    if ( update )
    {
        pda_Search ( pd, c, "data-begin-time", NULL, (char*) bold, SYMT_DATE); 
        pda_Search ( pd, c, "data-end-time", NULL, (char*) eold, SYMT_DATE); 
        pda_Search ( pd, c, "data-ndatapoints", NULL, (char*) ndat, SYMT_INT); 
    }
    else
    {
	bold->zt_Sec = 0;
	bold->zt_MicroSec = 0;
	eold->zt_Sec = 0;
	eold->zt_MicroSec = 0;
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
 * Get the color names for "ta-color" and "field-color"
 * from the plot description.
 * pd - the plot-description to search
 * c - the component to search
 * nplat - the number of colors expected for "field-color"
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
	    if ( pda_Search (pd, c, "field-color", NULL,colors, SYMT_STRING))
	    {
	        ncol = CommaParse( colors, colorlist);
	        if ( ncol < nplat )
	        {
		    msg_ELog ( EF_INFO, 
    "%s Not enough field-colors specified, filling in with white.",c);
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
			"%s: No field-color(s) specified, using white.",c);
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
ZebTime            *bTimeTarget,*eTimeTarget,*eTimeOld;
ZebTime            *bTimeReq,*eTimeReq;
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
    char	stime[80];

    *eTimeReq = *eTimeOld;
    *bTimeReq = *bTimeTarget;
    if (! ds_DataTimes (pid, eTimeTarget, 1, DsBefore, eTimeReq))
    {
	TC_EncodeTime ( eTimeTarget, TC_Full, stime );
        msg_ELog (EF_INFO, "No data before %s ", stime );
        available = 0;
    }
    /*
     * If no data previously exists, then get the time of the oldest
     * available data within the target range.
     */
    if ( eTimeOld->zt_Sec == 0  && eTimeOld->zt_MicroSec == 0 )
    {
        if (! ds_DataTimes (pid, bTimeTarget, 1, DsBefore, bTimeReq))
        {
	    TC_EncodeTime ( bTimeTarget, TC_Full, stime );
            msg_ELog (EF_DEBUG, "No data before %s", stime );
            if (! ds_DataTimes (pid, bTimeTarget, 1, DsAfter, bTimeReq))
            {
	        TC_EncodeTime ( bTimeTarget, TC_Full, stime );
                msg_ELog (EF_INFO, "No data after %s", stime );
                available = 0;
            }
        }
    }
    else
        *bTimeReq = *eTimeOld;

    if ( available && ( eTimeReq->zt_Sec == bTimeReq->zt_Sec ? 
    			eTimeReq->zt_MicroSec < bTimeReq->zt_MicroSec :
    			eTimeReq->zt_Sec < bTimeReq->zt_Sec ) )
    {
        available = 0;
        TC_EncodeTime ( eTimeTarget, TC_Full, stime );
        msg_ELog (EF_INFO, "No new data for at %s",stime );
    }
    return (available);
}

void
xy_GetDataMinMax(haveminmax, min, max, data, npts)
bool   haveminmax;
DataValPtr      min,max;
DataValPtr      data;
int             npts;
/*
 * Get the min and max data value for an array of data.
 * If haveminmax is false, min and max will be reset and their returned
 * values will be the true min and max of the data set.
 * If haveminmax is true, min and max will be left as their current (passed in)
 * values and will only be changed if the values in the current data
 * set exceed them.
 */
{
   int  i;

   for (i = 0; i < npts; i++)
   {
   /*
    * Kluge test to bypass infinities in floating data.  We have some
    * files that contain them and they tend to screw up autoscaling...
    * (XDR_F_INFINITY comes from netcdf.h)
    */
	if (data[i].type == 'f' && data[i].val.f == XDR_F_INFINITY)
	{
		msg_ELog (EF_DEBUG, "Ignoring infinity while autoscaling");
		continue;
	}
   /*
    * Initialize the min and max if they don't exist yet
    */
	if (! haveminmax)
	{
		*min = data[i];
		*max = data[i];
		haveminmax = TRUE;
	}
   /*
    * Update the min and max based on this point
    */
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
