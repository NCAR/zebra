/*
 * XY-Wind plotting module
 */
static char *rcsid = "$Id: XYWind.c,v 1.17 1993-06-29 15:37:16 barrett Exp $";
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

/*
 * General definitions
 */
void	xy_Wind FP ((char *, int));

/*
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

/*
 * Color array and indices
 */
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
	int	i, npts, plat, nplat, ii, jj, ns ;
	int	nxfield,nyfield;
	int	count;
	int 	nPlotted=0;
	char	platforms[80], tadefcolor[30];
	char	ctname[20];
	float   cstep;           /* interval for each step of color tables */
	float	scaleSpeed;
	XColor	*colors;
	int	ncolors;
	char	dataNames[4][80];
	FieldId	fids[4];
	ZebTime	bTimeTarget,eTimeTarget;
	ZebTime	eTimeReq,bTimeReq;
	ZebTime	eTimeOld,bTimeOld;
	char	stime1[80],stime2[80];
	long	autoTime;
	char	*pnames[MAX_PLAT];
	char	*fnames[4][MAX_PLAT];
	PlatformId	pid;
	DataChunk	*dc = NULL;
	DataClass	xyClass;
	Location	origin;
	int		n, m, len;
	float		badvalue, *data[MAX_PLAT], *tempdata;
	RGrid		rg;
	DataOrganization	xyOrg;
	DataValPtr	*xdata,*ydata;
	DataValPtr	*udata,*vdata;
	DataValRec	xmin,xmax,ymin,ymax;
	DataValRec	oldxmin,oldxmax,oldymin,oldymax;
	int		xrescale=0,yrescale =0;
	char		xtype = 'f',ytype = 'f';
	DataValRec	oldmin, oldmax;
	unsigned short	xscalemode,yscalemode;
	int	fcount;
        int	xdim,ydim;
	int	saveConfig;
	int	angle = 0;
	int	udim,vdim;
	int	nufield, nvfield;
	float	vecScale = 0.01;
	int	skip;
	char	style[80];
	char	csystem[32];
	int	dmode;
	char	datalabel[80];
	bool	sideAnnot;
        char    barbtype[8];
        int     doKnot = 0;
/*
 * Get X-Y Winds Required parameters:
 * "platform","x-field", "y-field", "coords", "color-table", "org"
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"x-field",NULL, (char*)(dataNames[0]), SYMT_STRING);
	ok = pda_ReqSearch (Pd,c,"y-field",NULL, (char*)(dataNames[1]), SYMT_STRING);

	/* The winds coordinate system */
	ok = pda_ReqSearch (Pd,c,"coords","xy-wind", csystem, SYMT_STRING);
	if ( strcmp ( csystem , "compass" ) == 0 )
	{
	    if( !pda_Search (Pd,c,"wdir-field","xy-wind",(dataNames[2]),
			     SYMT_STRING) )
	    {
		strcpy ( dataNames[2], "wdir");
	    }
	    if( !pda_Search (Pd,c,"wspd-field","xy-wind",(dataNames[3]),
			     SYMT_STRING) )
	    {
		strcpy ( dataNames[3], "wspd");
	    }
	    angle = 1;
	}
	else 
	{
	    if( !pda_Search (Pd,c,"u-field","xy-wind",(dataNames[2]),
			     SYMT_STRING) )
	    {
		strcpy ( dataNames[2], "u_wind");
	    }
	    if( !pda_Search (Pd,c,"v-field","xy-wind",(dataNames[3]),
			     SYMT_STRING) )
	    {
		strcpy ( dataNames[3], "v_wind");
	    }
	    angle = 0;
	}

	ok &= pda_ReqSearch (Pd, c, "color-table", "xy-wind",ctname,SYMT_STRING);

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
 * "data-skip" - skip every "n" data points.
 * "style" - "barb" or "vector"
 * "step" - float, the size of the color table intervale
 * "barb-type" - "m/s" or "knots"
 */
        if ( !pda_Search (Pd,c,"do-side-annotation", "xy-wind",
                (char *) &sideAnnot, SYMT_BOOL))
        {
            sideAnnot = True;
        }

	if ( !pda_Search (Pd,c,"data-skip", "xy-wind",(char *) &skip, SYMT_INT))
	{
	    skip = 5;
	}
	if ( !pda_Search (Pd,c,"representation-style", "xy-wind",(char *)style, SYMT_STRING))
	{
	    strcpy(style, "vector" );
	}
	if ( strcmp(style, "vector" )==0)
	{
	    if( !pda_Search (Pd,c,"vec-scale", "xy-wind",(char *) &vecScale, 
			SYMT_FLOAT))
	    {
	        vecScale = 5.0;
	    }
	}
	else 
	{
	    if( !pda_Search (Pd,c,"barb-scale", "xy-wind",(char *) &vecScale, 
			SYMT_FLOAT))
	    {
	        vecScale = 25.0;
	    }
            if( !pda_Search (Pd,c,"barb-type", "xy-wind",(char *) barbtype,
                        SYMT_STRING))
            {
                strcpy ( barbtype, "m/s");
            }
	}
	if( !pda_Search (Pd,c,"step", "xy-wind",(char *) &cstep, SYMT_FLOAT))
	{
	  cstep = 5.0;
	}
	if( !pda_Search (Pd,c,"scale-speed", "xy-wind",(char *) &scaleSpeed, SYMT_FLOAT))
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
 * Plot the top annotation.
 */
	if (! update)
	{
	    An_TopAnnot ("X/Y Wind:", Tadefclr.pixel);
	    An_TopAnnot (c,Tadefclr.pixel);
	}
/*
 * Side annotation.
 */
	if (sideAnnot && !update)
	{
	    if (strcmp (style, "vector") == 0 )
	    {
                sprintf (datalabel, "%5.1f%s %s %f %f %f", scaleSpeed,
			"m/sec", tadefcolor, scaleSpeed, 0.0, vecScale);
                An_AddAnnotProc (An_ColorVector, c, datalabel,
                        strlen (datalabel) + 1, 30, FALSE, FALSE);
		sprintf (datalabel, "%s %s %f %f", "wind-speed:m/sec",
                        ctname, 
		ncolors%2 ?(ncolors*cstep*0.5)-cstep*0.5 :ncolors*cstep*0.5,
                        cstep);
	    }
	    if (strcmp (style, "barb") == 0 )
	    {	
                doKnot = strcmp( barbtype, "knots" ) == 0 ? 1 :0;
                sprintf (datalabel, "%s %s %d", barbtype,
                                tadefcolor,  (int)vecScale);
                An_AddAnnotProc (An_BarbLegend, c, datalabel,
                                strlen(datalabel)+1, 100, FALSE, FALSE);
                sprintf (datalabel, "%s%s %s %f %f ",
                        "wind-speed:",barbtype ,ctname,
                    ncolors%2 ?(ncolors*cstep*0.5)-cstep*0.5 :ncolors*cstep*0.5,
                        cstep);
	    }
            An_AddAnnotProc (An_ColorBar, c, datalabel, strlen (datalabel) + 1,
		 75, TRUE, FALSE);
	}

/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Add this platform to the annotation
	 */
	    if (sideAnnot && !update)
	    {
		sprintf (datalabel, "%s %s", pnames[plat], tadefcolor);
		An_AddAnnotProc (An_ColorString, c, datalabel, 
			strlen (datalabel), 25, FALSE, FALSE);
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
			msg_ELog (EF_PROBLEM, "Bad orgainization.");
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
	    udim = fcount;
	    fids[fcount] = F_Lookup (fnames[2][plat]); fcount++;
	    vdim = fcount;
	    fids[fcount] = F_Lookup (fnames[3][plat]); fcount++;
	/*
	 *  Determine times of data to request.
	 */
            if (! xy_AvailableData (pid, &bTimeTarget, &eTimeTarget, &eTimeOld,
				   &bTimeReq, &eTimeReq))
	    {
		    TC_EncodeTime (&bTimeTarget, TC_Full, stime1);
		    TC_EncodeTime (&eTimeTarget, TC_Full, stime2);
		    msg_ELog (EF_INFO, "No data for '%s' between %s and %s",
			      pnames[plat], stime1, stime2);
		    npts = 0;
		    xdata[plat] = NULL;
		    ydata[plat] = NULL;
		    udata[plat] = NULL;
		    vdata[plat] = NULL;
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
		    udata[plat] = NULL;
		    vdata[plat] = NULL;
		    continue;
	    }
	/*
	 * Now that we have a data chunk, update the overlay times widget
	 */
	    if ( !update ) lw_TimeStatus (c, pnames[plat], &eTimeReq);
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
		    {
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
	    }
	    
	    xdata[plat] = (DataValPtr) malloc (npts * sizeof(DataValRec));
	    ydata[plat] = (DataValPtr) malloc (npts * sizeof(DataValRec));
	    udata[plat] = (DataValPtr) malloc (npts * sizeof(DataValRec));
	    vdata[plat] = (DataValPtr) malloc (npts * sizeof(DataValRec));

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
			if (data[jj][ii] == badvalue)
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
		  udata[plat][count].val.f = data[udim][ii];
		  vdata[plat][count].val.f = data[vdim][ii];
		  udata[plat][count].type = 'f';
		  vdata[plat][count].type = 'f';
		  count += nPlotted % (skip+1) == 0 ? 1 : 0;
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
        if ( xscalemode & AUTO ) xy_SetScaleBounds(Pd,c,'x',xtype,&xmin,&xmax);
        if ( yscalemode & AUTO ) xy_SetScaleBounds(Pd,c,'y',ytype,&ymin,&ymax);

        /*
         * Now check if scale-bounds are different from the axis-bounds
         * If so, and this is only an update, then trigger a global
         * redraw so shift is reflected for all components
         */
        if ( ((lc_CompareData(&ymin,&ymax) >= 0 ) ||
              (lc_CompareData(&xmin,&xmax) >= 0 ) )) 
        {
	    msg_ELog ( EF_PROBLEM, 
	     "Scale max's must be greater than scale min's");
	    goto errorExit;
        }

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
            lc_GetUserCoord ( &xmin,&xmax,NULL,NULL,xscalemode);
            lc_GetUserCoord ( NULL,NULL,&ymin,&ymax,yscalemode);
            gp_Clip( &xmin, &ymin, &xmax, &ymax, xscalemode,
	    		yscalemode | FUDGEBOT);
            msg_ELog ( EF_DEBUG,
               "X-Y Graph plotting %d data points for component %s.",npts,c);
            for (plat = 0; plat < nplat; plat++)
            {
	        if ( strcmp(style, "barb" ) == 0 )
	        {
	            gp_WindBarb( xdata[plat],ydata[plat],
			udata[plat],vdata[plat],
			npts, angle, (int)vecScale, L_solid, 
			colors,ncolors,cstep,xscalemode,yscalemode,doKnot);
	        }
	        else
	        {
	            gp_WindVector( xdata[plat],ydata[plat],
			udata[plat],vdata[plat],
			npts, angle, (double)vecScale, L_solid, 
			colors,ncolors,cstep,xscalemode,yscalemode);
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
errorExit:
	for ( i = 0; i < nplat; i++)
	{
	    if (xdata[i]) free(xdata[i]);
	    if (ydata[i]) free(ydata[i]);
	    if (udata[i]) free(udata[i]);
	    if (vdata[i]) free(vdata[i]);
	}
	free(xdata); free(ydata); free(udata); free(vdata);
}
# endif /* C_PT_XYGRAPH */
