/*
 * XY-Wind plotting module
 */
static char *rcsid = "$Id: XYWind.c,v 1.3 1992-01-10 19:18:40 barrett Exp $";
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

# if C_PT_XYWIND


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

void	xy_Wind();

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
xy_Wind(c, update)
char	*c;
bool	update;
/*
 * Draw an xy-graph on the given component
 */
{
	bool	ok;
	int	status, i, npts, plat, nplat,ii,jj;
	int	nxfield,nyfield;
	int	count;
	int 	nPlotted=0;
	char	platforms[80], tadefcolor[30];
	char	ctname[20];
	float   cstep;               /* interval for each step of color tables */
	float	scaleSpeed;
	char	dataNames[4][80];
	char	*flist[4];
	time	ptime;
	time	bTimeTarget,eTimeTarget;
	time	eTimeReq,bTimeReq;
	time	eTimeOld,bTimeOld;
	long	autoTime;
	char	*pnames[MAX_PLAT];
	char	*fnames[4][MAX_PLAT];
	PlatformId	pid;
	DataObject	*dobj = NULL;
	DataOrganization	xyOrg;
	DataValPtr	*xdata,*ydata;
	DataValPtr	*udata,*vdata;
	DataValRec	xmin,xmax,ymin,ymax;
	char		xtype = 'f',ytype = 'f';
	DataValRec	oldmin, oldmax;
	unsigned short	xscalemode,yscalemode;
	int	fcount;
        int	xdim,ydim;
	int	plotAxis[4];
	int	saveConfig;
	int	angle = 0;
	int	udim,vdim;
	int	nufield, nvfield;
	float	vecScale = 0.01;
	int	sample;
	char	style[80];
	char	csystem[32];
	int	dmode;
	char	datalabel[80];
/*
 * Get X-Y Winds Required parameters:
 * "platform","x-field", "y-field", "wind-coords", "color-table", "org"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"x-field",NULL, &(dataNames[0]), SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"y-field",NULL, &(dataNames[1]), SYMT_STRING);

	/* The winds coordinate system */
	ok = pda_ReqSearch (Pd,c,"wind-coords",NULL, &csystem, SYMT_STRING);
	if ( strcmp ( csystem , "compass" ) == 0 )
	{
	    if( !pda_Search (Pd,c,"wdir-field",NULL,&(dataNames[2]),
			     SYMT_STRING) )
	    {
		strcpy ( dataNames[2], "wdir");
	    }
	    if( !pda_Search (Pd,c,"wspd-field",NULL,&(dataNames[3]),
			     SYMT_STRING) )
	    {
		strcpy ( dataNames[3], "wspd");
	    }
	    angle = 1;
	}
	else 
	{
	    if( !pda_Search (Pd,c,"u-field",NULL,&(dataNames[2]),
			     SYMT_STRING) )
	    {
		strcpy ( dataNames[2], "u_wind");
	    }
	    if( !pda_Search (Pd,c,"v-field",NULL,&(dataNames[3]),
			     SYMT_STRING) )
	    {
		strcpy ( dataNames[3], "v_wind");
	    }
	    angle = 0;
	}

	ok &= pda_ReqSearch (Pd, c, "color-table", NULL,ctname,SYMT_STRING);

	if (! ok) return;
/*
 * Parse platform and field name information. 
 */
	nplat = CommaParse (platforms, pnames);
	nxfield = CommaParse (dataNames[0], fnames[0]);
	nyfield = CommaParse (dataNames[1], fnames[1]);
	nufield = CommaParse (dataNames[2], fnames[2]);
	nvfield = CommaParse (dataNames[3], fnames[3]);
	if ( nxfield != nplat && nyfield != nplat && 
	     nufield != nplat && nvfield != nplat)
	{
	    msg_ELog ( EF_PROBLEM, 
		"X/Y Wind: number of fields of doesn't correspond to number of platforms.");
	    return;
	}
/* for now, only allow max MAX_PLAT platforms */
	if (nplat > MAX_PLAT)
		nplat = MAX_PLAT;
/*
 * Get X-Y Winds optional parameters:
 * "vec-scale" - the number of pixels long to make the barb, or the vector
 *		scaling factor
 * "sample-n" - sample every "n" data points.
 * "style" - "barb" or "vector"
 * "step" - float, the size of the color table intervale
 */
	if( !pda_Search (Pd,c,"vec-scale", NULL,(char *) &vecScale, SYMT_FLOAT))
	{
	    vecScale = 25.0;
	}
	if ( !pda_Search (Pd,c,"sample-n", NULL,(char *) &sample, SYMT_INT))
	{
	    sample = 5;
	}
	if ( !pda_Search (Pd,c,"representation-style", NULL,(char *) &style, SYMT_STRING))
	{
	    strcpy(style, "vector" );
	}
	if( !pda_Search (Pd,c,"step", NULL,(char *) &cstep, SYMT_FLOAT))
	{
	  cstep = 5.0;
	}
	if( !pda_Search (Pd,c,"scale-speed", NULL,(char *) &scaleSpeed, SYMT_FLOAT))
	{
	  scaleSpeed = 25.0;
	}
/*
 * Allocate memory for data and attibutes
 */
	xdata = (DataValPtr*)malloc (nplat* sizeof(DataValPtr));
	ydata = (DataValPtr*)malloc (nplat* sizeof(DataValPtr));
	udata = (DataValPtr*)malloc (nplat* sizeof(DataValPtr));
	vdata = (DataValPtr*)malloc (nplat* sizeof(DataValPtr));
/*
 * Initialize local environment.
 */
	/*lc_SetUserCoord ( 0.0,1.0,0.0,1.0);*/
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

	xy_GetPlotAttr(Pd,c,nplat,NULL,tadefcolor);

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
	ct_LoadTable (ctname, &Colors, &Ncolors);
	if (Ncolors < 1)
	{
		msg_ELog(EF_PROBLEM, "XY-Windcolor table too small");
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
	    An_TopAnnot ("X/Y Wind:", Tadefclr.pixel);
	    An_TopAnnot (c,Tadefclr.pixel);
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

	        if ( strcmp(style, "vector" ) == 0 )
		{
                    sprintf (datalabel, "%f5.1%s %s %f %f %f", scaleSpeed,
			"m/sec", tadefcolor, scaleSpeed, 0.0, vecScale);
                    An_AddAnnotProc (An_ColorVector, c, datalabel,
                                strlen(datalabel)+1, 30, FALSE, FALSE);
		}
	        if ( strcmp(style, "barb" ) == 0 )
		{
                    sprintf (datalabel, "%s %s %d", "m/sec",
				tadefcolor,  (int)vecScale);
                    An_AddAnnotProc (An_BarbLegend, c, datalabel,
                                strlen(datalabel)+1, 100, FALSE, FALSE);
		}
                sprintf (datalabel, "%s %s %f %f", "wind-speed:m/sec", ctname,
                    Ncolors%2 ?(Ncolors*cstep*0.5)-cstep*0.5 :Ncolors*cstep*0.5, 
		    cstep);
                An_AddAnnotProc (An_ColorBar, c, datalabel,
                                strlen(datalabel)+1, 75, TRUE, FALSE);
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
	    if (xtype != 't')
	    {
		xdim = fcount;
	        flist[fcount] = fnames[0][plat]; fcount++;
	    }
	    if (ytype != 't')
	    {
		ydim = fcount;
	        flist[fcount] = fnames[1][plat]; fcount++;
	    }
	    udim = fcount;
	    flist[fcount] = fnames[2][plat]; fcount++;
	    vdim = fcount;
	    flist[fcount] = fnames[3][plat]; fcount++;

	    /*
	     *  Determine times of data to request.
	     */
            if (xy_AvailableData(pid,bTimeTarget,eTimeTarget,eTimeOld,
                                &bTimeReq, &eTimeReq))
            {


                msg_ELog ( EF_DEBUG,
                   "%s Data request times begin %d %d end = %d %d",
                        c,
                        bTimeTarget.ds_yymmdd,bTimeTarget.ds_hhmmss,
                        eTimeTarget.ds_yymmdd,eTimeTarget.ds_hhmmss);
                dobj = ds_GetData (pid, flist, fcount,&bTimeReq,&eTimeReq,
                        xyOrg, 0.0, BADVAL);
	    }

	    if (! dobj)
	    {
		msg_ELog (EF_INFO, 
			"Unable to get field data for '%s' at %d %06d", 
			pnames[plat], eTimeReq.ds_yymmdd, 
			eTimeReq.ds_hhmmss);
		npts = 0;
	        xdata[plat] = NULL;
	        ydata[plat] = NULL;
	        udata[plat] = NULL;
	        vdata[plat] = NULL;
		continue;
	    }
	    else
	    {

	        if ( xyOrg == OrgScalar )
		    npts = dobj->do_npoint;
	        else if ( xyOrg == Org1dGrid )
		    npts = dobj->do_desc.d_rgrid.rg_nX * dobj->do_npoint;

	        xdata[plat] = (DataValPtr)malloc( npts * sizeof(DataValRec));
	        ydata[plat] = (DataValPtr)malloc( npts * sizeof(DataValRec));
	        udata[plat] = (DataValPtr)malloc( npts * sizeof(DataValRec));
	        vdata[plat] = (DataValPtr)malloc( npts * sizeof(DataValRec));
	    }
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
		  udata[plat][count].val.f = dobj->do_data[udim][ii];
		  vdata[plat][count].val.f = dobj->do_data[vdim][ii];
		  udata[plat][count].type = 'f';
		  vdata[plat][count].type = 'f';
		  count += nPlotted % sample == 0 ? 1 : 0;
		  nPlotted++;
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
            gp_Clip( &xmin, &ymin,&xmax,&ymax,xscalemode,yscalemode );
            msg_ELog ( EF_DEBUG,
               "X-Y Graph plotting %d data points for component %s.",npts,c);
            for (plat = 0; plat < nplat; plat++)
            {
	        if ( strcmp(style, "barb" ) == 0 )
	        {
	            gp_WindBarb( xdata[plat],ydata[plat],
			udata[plat],vdata[plat],
			npts, angle, (int)vecScale, L_solid, 
			Colors,Ncolors,cstep,xscalemode,yscalemode);
	        }
	        else
	        {
	            gp_WindVector( xdata[plat],ydata[plat],
			udata[plat],vdata[plat],
			npts, angle, (double)vecScale, L_solid, 
			Colors,Ncolors,cstep,xscalemode,yscalemode);
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
	    if (udata[i]) free(udata[i]);
	    if (vdata[i]) free(vdata[i]);
	}
	free(xdata); free(ydata); free(udata); free(vdata);
	ac_DisplayAxes();
}
# endif /* C_PT_XYWIND */
