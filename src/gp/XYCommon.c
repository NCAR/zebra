/*
 * Routines common to XY-Type plots
 */
static char *rcsid = "$Id: XYCommon.c,v 1.1 1992-01-02 17:07:29 barrett Exp $";
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

/*
 * General definitions
 */
# define MAX_PLAT	10

/*
# ifdef __STDC__
    extern void xy_GetScaleMode(plot_description,char*,char,unsigned short);
    extern void xy_SetScaleBounds(plot_description,char*,char,char,DataValPtr,
		DataValPtr);
    extern void xy_GetCurrentScaleBounds(plot_description,char*,char,char,
		DataValPtr,DataValPtr);
    extern void xy_GetComponentAxes(plot_description, char*, int[4]);
    extern void xy_GetAxisDescriptors( plot_description, char*, char, char,
		       int*, int*,int*, int*,
		       int*, float*,DataValPtr,float*, char*, label*);
    extern void xy_SetPrivateAxisDescriptors( plot_description, char*, char, 
		char, int*, DataValPtr,int*,int*,int*, float*);
    extern void xy_SetPrivateDD ( plot_description, char*, time*,time*,int*);
    extern void xy_GetDataDescriptors( plot_description,char*,bool,time*,time*,
		time*,time*,int*);
    extern void xy_GetPlotAttr(plot_description,char*,int,char*[],char*);
    extern void xy_GetDataMinMax(bool, DataValPtr, DataValPtr, DataValPtr, int);
    extern int xy_AvailableData(PlatformId, time,time,time, time*,time*)
# else
    extern void xy_GetScaleMode();
    extern void xy_SetScaleBounds();
    extern void xy_GetCurrentScaleBounds();
    extern void xy_GetComponentAxes();
    extern void xy_GetAxisDescriptors();
    extern void xy_SetPrivateAxisDescriptors();
    extern void xy_SetPrivateDD ();
    extern void xy_GetDataDescriptors();
    extern void xy_GetPlotAttr();
    extern void xy_GetDataMinMax();
    extern int xy_AvailableData()
# endif
*/

void
xy_GetScaleMode(pd,c,dim,mode)
plot_description	pd;
char			*c;
char			dim;
unsigned short		*mode; /* return */
{
    char	keyword[20];
    char	string[80];
    strcpy(keyword, "scale-");
    keyword[6] = dim;
    keyword[7] = '\0';
    strcat(keyword, "-mode");
    *mode = 0;
    if ( pda_Search (pd, c, keyword, "xy", string, SYMT_STRING))
    {
	if ( strcmp(string,"autoscale")==0)
	    *mode = *mode | AUTO;
	else if ( strcmp(string,"manual")==0)
	    *mode = *mode | MANUAL;
	else	
	{
    	    msg_ELog (EF_PROBLEM, 
		"Unknown '%c' scaling mode: %s. Using autoscaling",
		dim,string);
	    *mode = *mode | AUTO;
	}
    }
    else
    {
	*mode = *mode | AUTO;
    }
    strcpy(keyword, "scale-");
    keyword[6] = dim;
    keyword[7] = '\0';
    strcat(keyword, "-style");
    if ( pda_Search (pd, c, keyword, "xy", string, SYMT_STRING))
    {
	if ( strcmp(string,"invert")==0)
	    *mode = *mode | INVERT;
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
{
    char	minkey[20];
    char	maxkey[20];
    strcpy(minkey, "scale-");
    minkey[6] = dim;
    minkey[7] = '\0';
    strcat(minkey, "-min");
    strcpy(maxkey, "scale-");
    maxkey[6] = dim;
    maxkey[7] = '\0';
    strcat(maxkey, "-max");
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
{
    char	minkey[20];
    char	maxkey[20];
    strcpy(minkey, "scale-");
    minkey[6] = dim;
    minkey[7] = '\0';
    strcat(minkey, "-min");
    strcpy(maxkey, "scale-");
    maxkey[6] = dim;
    maxkey[7] = '\0';
    strcat(maxkey, "-max");

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
xy_GetComponentAxes(pd, c, plotAxis)
plot_description pd; /* input */
char	*c;	     /* input */
int	plotAxis[4];
{
    if ( !pda_Search (pd, c, "axis-bottom", "xy", 
		(char *) &(plotAxis[AXIS_BOTTOM]), SYMT_INT))
    {
	plotAxis[AXIS_BOTTOM] = 1;
    }

    if ( !pda_Search (pd, c, "axis-top", "xy", 
		(char *) &(plotAxis[AXIS_TOP]), SYMT_INT))
    {
	plotAxis[AXIS_TOP] = 0;
    }

    if ( !pda_Search (pd, c, "axis-left", "xy", 
		(char *) &(plotAxis[AXIS_LEFT]), SYMT_INT))
    {
	plotAxis[AXIS_LEFT] = 1;
    }

    if ( !pda_Search (pd, c, "axis-right", "xy", 
		(char *) &(plotAxis[AXIS_RIGHT]), SYMT_INT))
    {
	plotAxis[AXIS_RIGHT] = 0;
    }
}

void
xy_GetAxisDescriptors( pd, c, side, datatype,
		       offset, tlabelWidth,tlabelHeight, nticLabel,
		       ticlen, ticInterval,baseTic,fontScale, color, label)
plot_description pd; /* input */
char	*c;	     /* input */
char	side;	     /* input */
char	datatype;    /* input */
int	*offset;
int	*tlabelWidth,*tlabelHeight;
int	*nticLabel;
int	*ticlen;
float	*ticInterval;
DataValPtr	baseTic;
float	*fontScale;
char	*color;
char	*label;
{
    char	keyword[80];
    char	string[80];
    /*
     * Get the user-settable axis descriptors
     */
    strcpy(keyword, "axis-");
    keyword[5] = side;
    keyword[6] = '\0';
    strcat(keyword, "-color");
    if (! pda_Search (pd, c, keyword, "xy", (char *)color, SYMT_STRING))
    {
	strcpy(color,"white");
    }
    strcpy(keyword, "axis-");
    keyword[5] = side;
    keyword[6] = '\0';
    strcat(keyword, "-label");
    if (! pda_Search (pd, c, keyword, "xy", (char *)label, SYMT_STRING))
    {
	strcpy(label,"label");
    }

    strcpy(keyword, "axis-");
    keyword[5] = side;
    keyword[6] = '\0';
    strcat(keyword, "-tic-len");
    if (! pda_Search (pd, c, keyword, "xy", (char *)ticlen, SYMT_INT))
    {
	*ticlen = 5;
    }
    strcpy(keyword, "axis-");
    keyword[5] = side;
    keyword[6] = '\0';
    strcat(keyword, "-tic-interval");
    /* hardwired for now */
    switch( datatype )
    {
	case 't':
    	    *ticInterval = 600.0;
            if(pda_Search (pd, c, keyword,"xy", (char*)string,SYMT_STRING))
    	    {
		if ( (*ticInterval = (float)pc_TimeTrigger(string)) == 0.0 )
		{
	    	    msg_ELog (EF_PROBLEM,"Unparseable tic interval: %s",string);
		}
    	    }
	break;
	case 'f':
            if(!pda_Search (pd, c, keyword,"xy", (char*)ticInterval,SYMT_FLOAT))
    	    {
    	        *ticInterval = 1.0;
	    }
	break;
    }
    strcpy(keyword, "axis-");
    keyword[5] = side;
    keyword[6] = '\0';
    strcat(keyword, "-font-scale");
    if (! pda_Search (pd, c, keyword, "xy", (char *)fontScale, SYMT_FLOAT))
    {
	*fontScale = 0.1;
    }

    /*
     * Get the axis descriptors that are private (calculated)
     */
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-offset");
    if (! pda_Search (pd, c, keyword, NULL, (char *)offset, SYMT_INT))
    {
	*offset = 0;
    }
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-base-tic");
    baseTic->type = datatype;
    switch( datatype )
    {
	case 't':
    	    if (! pda_Search (pd, c, keyword, NULL, 
		(char *)&(baseTic->val.t), SYMT_DATE))
            {
	        baseTic->val.t.ds_yymmdd = 0;
	        baseTic->val.t.ds_hhmmss = 0;
            }
	break;
	case 'f':
    	    if (! pda_Search (pd, c, keyword, NULL, 
		(char *)&(baseTic->val.f), SYMT_FLOAT))
            {
	        baseTic->val.f = 0.0;
            }
	break;
    }
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-tic-label-height");
    if (! pda_Search (pd, c, keyword, NULL, (char *)tlabelHeight, SYMT_INT))
    {
	*tlabelHeight = 0;
    }
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-n-tic-label");
    if (! pda_Search (pd, c, keyword, NULL, (char *)nticLabel, SYMT_INT))
    {
	*nticLabel = 0;
    }
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-tic-label-width");
    if (! pda_Search (pd, c, keyword, NULL, (char *)tlabelWidth, SYMT_INT))
    {
	*tlabelWidth = 0;
    }

}
void
xy_SetPrivateAxisDescriptors( pd, c, side, datatype,
		       offset, baseTic,tlabelHeight,tlabelWidth,nticLabel,
		       ticInterval)
plot_description pd; /* input */
char	*c;	     /* input */
char	side;	     /* input */
char	datatype;    /* input */
int	*offset;
DataValPtr	baseTic;
int	*tlabelHeight,*tlabelWidth;
int	*nticLabel;
float	*ticInterval;
{
    char	mode[80];
    char	keyword[80];

    if ( offset )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-offset");
        pd_Store (pd, c, keyword, (char *)offset, SYMT_INT);
    }

    if ( baseTic)
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-base-tic");
        switch( datatype )
        {
	    case 't':
    	        pd_Store (pd, c, keyword, (char *)&(baseTic->val.t), SYMT_DATE);
	    break;
	    case 'f':
    	        pd_Store (pd, c, keyword,(char *)&(baseTic->val.f), SYMT_FLOAT);
	    break;
        }
    }

    if ( tlabelHeight )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-tic-label-height");
        pd_Store (pd, c, keyword, (char *)tlabelHeight, SYMT_INT);
    }

    if ( nticLabel )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-n-tic-label");
        pd_Store (pd, c, keyword, (char *)nticLabel, SYMT_INT);
    }

    if ( tlabelWidth )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-tic-label-width");
        pd_Store (pd, c, keyword,  (char *)tlabelWidth, SYMT_INT);
    }
}

void
xy_SetPrivateDD ( pd, c, btime,etime,npnts)
plot_description pd;
char	*c;
time	*btime,*etime;	
int	*npnts;
{
    if ( btime ) pd_Store (pd, c, "data-begin-time",(char*)btime,SYMT_DATE);
    if ( etime ) pd_Store (pd, c, "data-end-time",(char*)etime,SYMT_DATE);
    if ( npnts ) pd_Store (pd, c, "data-ndatapoints",(char*)npnts,SYMT_INT);
}

void
xy_GetDataDescriptors( pd,c,update,btime,etime,bold,eold,dmode)
plot_description pd;
char	*c;
bool	update;
time	*btime,*etime;	/* Computed current target begin and end times */
time	*bold,*eold;	/* old begin and end times of data */
int	*dmode;
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
		spanTime = ts_GetSec(*etime) - spanSec;
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
    }
    else
    {
	bold->ds_yymmdd = 0;
	bold->ds_hhmmss = 0;
	eold->ds_yymmdd = 0;
	eold->ds_hhmmss = 0;
    }
}

void
xy_GetPlotAttr(pd,c,nplat,linecolor,topAnnColor)
plot_description pd;
char	*c;
int	nplat;
char	*linecolor[MAX_PLAT];
char	*topAnnColor;
{
	char	attrs[80];
	char	*attrlist[MAX_PLAT];
 	int 	nattr;
	int	i;
/*
 * Get line color for data from each platform
 */
	if ( pda_Search (pd, c, "line-color", NULL, attrs, SYMT_STRING))
	{
	    nattr = CommaParse( attrs, attrlist);
	    if ( nattr < nplat )
	    {
		msg_ELog ( EF_INFO, 
		    "Not enough line-colors specified, filling in with white.");
	    }
	    for ( i = 0; i < nplat; i++)
	    {
	        if (GWDepth(Graphics) == 1 || i >= nattr )
		     strcpy(linecolor[i], "white");
		else
		     strcpy(linecolor[i], attrlist[i]);
	    }
	}
	if (! pda_Search (Pd,"global","ta-color", NULL,topAnnColor,SYMT_STRING))
		strcpy(topAnnColor, "white");
}

int
xy_AvailableData(pid,bTimeTarget,eTimeTarget,eTimeOld,bTimeReq, eTimeReq)
PlatformId      pid;
time            bTimeTarget,eTimeTarget,eTimeOld;
time            *bTimeReq,*eTimeReq;
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

    if ( available && ts_GetSec(*eTimeReq) < ts_GetSec(*bTimeReq) )
    {
        available = 0;
        msg_ELog (EF_INFO, "No new data for at %d %d",eTimeTarget.ds_yymmdd,
                                eTimeTarget.ds_hhmmss);
    }
    return (available);
}
xy_GetDataMinMax(update, min, max, data, npts)
bool    update;
DataValPtr      min,max;
DataValPtr      data;
int             npts;
{
   int  i;
   if ( !update )
   {
        *min = data[0];
        *max = data[0];
   }
   for ( i = 0; i < npts; i++)
   {
        if ( lc_CompareData( &(data[i]), &max ) > 0 )
                        *max = data[i];
        if ( lc_CompareData( &(data[i]), &min ) < 0 )
                        *min = data[i];
   }
}
