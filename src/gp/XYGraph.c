/*
 * XY-Graph plotting module
 */
static char *rcsid = "$Id: XYGraph.c,v 1.2 1991-10-30 21:35:47 barrett Exp $";
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
# define AUTO_XMAX	(1<<0)
# define AUTO_YMAX	(1<<1)
# define AUTO_XMIN	(1<<2)
# define AUTO_YMIN	(1<<3)
# define ROUNDIT(x)	((int)(x + 0.5))

extern int TransConfig;
void	xy_Graph();

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
xy_Graph (c, update)
char	*c;
bool	update;
/*
 * Draw an xy-graph on the given component
 */
{
	bool	ok;
	int	axid;
	int	status, i, npts, plat, nplat,ii;
	int	nxfield,nyfield;
	int	count;
	char	platforms[80], annot[80], tadefcolor[30];
	char	ctname[20];
	char	dataNames[2][80];
	char	*flist[2];
	time	ptime;
	char	*pnames[10];
	char	*fnames[2][10];
	PlatformId	pid;
	DataObject	*dobj = NULL;
	XColor	*lcolor;
	char	xlabel[80],ylabel[80];
	DataOrganization	xyOrg;
	float	xmin,xmax,ymin,ymax;
	char	attrs[80];
	char	*attrlist[10];
	float	**xdata,**ydata;
 	int 	nattr;
	unsigned short	autoscale;
	float	xbox[5],ybox[5];
	int	axisTic, axisLoc;
	int	axisNticks;
	char	axisColor[20];
	int	plotAxis[4];
	float	axisFontScale;
	XRectangle	r;
	int	transConfig;
	int	saveConfig;
/*
 * Get the platform and color table
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "x-field", NULL, &(dataNames[0]), SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "y-field", NULL, &(dataNames[1]), SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "org", NULL, (char*)&xyOrg, SYMT_INT);
/* kludge for now to determine data organization type */
	if ( xyOrg == 3 ) 
	   xyOrg = OrgScalar;
	else if ( xyOrg == 1 ) 
	   xyOrg = Org1dGrid;
	ok &= pda_ReqSearch (Pd, c, "color-table", NULL,ctname,SYMT_STRING);

	if (! ok) return;
/*
 * Initialize user-coordinate system
 */
	lc_SetUserCoord ( 0.0,1.0,0.0,1.0);
/*
 * Break apart the platform names.
 */
	nplat = CommaParse (platforms, pnames);
/*
 * Break apart the field names.
 */
	nxfield = CommaParse (dataNames[0], fnames[0]);
	nyfield = CommaParse (dataNames[1], fnames[1]);
	if ( nxfield != nplat && nyfield != nplat )
	{
	    msg_ELog ( EF_PROBLEM, 
		"X/Y Graph: x/y fields of plot-description don't correspond to number of platforms.");
	    return;
	}
/* for now, only allow max 5 platforms */
	if (nplat > 5)
		nplat = 5;
/*
 * Allocate space for data and attributes associated with each platform
 */
	lcolor = (XColor*)malloc ( nplat* sizeof(XColor));
	xdata = (float**)malloc (nplat* sizeof(float*));
	ydata = (float**)malloc (nplat* sizeof(float*));
/*
 * Look for transformation directives
 */
	saveConfig = TransConfig;
	TransConfig = 0;
	if ( pda_Search (Pd, c, "log-x", "trans", 
		(char *) &transConfig, SYMT_INT))
	{
	    TransConfig = TransConfig | ( transConfig ? INVERT_X : 0 );
	}
	if ( pda_Search (Pd, c, "log-y", "trans", 
		(char *) &transConfig, SYMT_INT))
	{
	    TransConfig = TransConfig | ( transConfig ? INVERT_Y : 0 );
	}
	if ( pda_Search (Pd, c, "invert-x", "trans", 
		(char *) &transConfig, SYMT_INT))
	{
	    TransConfig = TransConfig | ( transConfig ? INVERT_X : 0 );
	}
	if ( pda_Search (Pd, c, "invert-y", "trans", 
		(char *) &transConfig, SYMT_INT))
	{
	    TransConfig = TransConfig | ( transConfig ? INVERT_Y : 0 );
	}

/*
 * Look for axis plotting directives.
 */
	if ( !pda_Search (Pd, c, "axis-bottom", NULL, 
		(char *) &(plotAxis[AXIS_BOTTOM]), SYMT_INT))
	{
	    plotAxis[AXIS_BOTTOM] = 1;
	}

	if ( !pda_Search (Pd, c, "axis-top", NULL, 
		(char *) &(plotAxis[AXIS_TOP]), SYMT_INT))
	{
	    plotAxis[AXIS_TOP] = 0;
	}

	if ( !pda_Search (Pd, c, "axis-left", NULL, 
		(char *) &(plotAxis[AXIS_LEFT]), SYMT_INT))
	{
	    plotAxis[AXIS_LEFT] = 1;
	}

	if ( !pda_Search (Pd, c, "axis-right", NULL, 
		(char *) &(plotAxis[AXIS_RIGHT]), SYMT_INT))
	{
	    plotAxis[AXIS_RIGHT] = 0;
	}

/*
 * Look for axis attributes
 */
	
	if (! pda_Search (Pd, c, "axis-line-pos", "xygraph", (char *) &axisLoc, 
		SYMT_INT))
	{
	    axisLoc = 0;
	}
	if (! pda_Search (Pd, c, "axis-tic-len", "xygraph", (char *) &axisTic, 
		SYMT_INT))
	{
	    axisTic = 5;
	}
	if (! pda_Search (Pd, c, "axis-color", "xygraph", axisColor, 
		SYMT_STRING) || GWDepth(Graphics) == 1 )
	{
	    strcpy (axisColor, "white");
	}
	if (! pda_Search (Pd, c, "axis-nticks", "xygraph", (char *) &axisNticks,
		SYMT_INT))
	{
	    axisNticks = 10;
	}
	if (! pda_Search (Pd, c, "axis-font-scale", "xygraph", 
		(char *) &axisFontScale, SYMT_FLOAT))
	{
	    axisFontScale = 0.025;
	}
	if (! pda_Search (Pd, c, "x-label", "xygraph", xlabel, 
		SYMT_STRING))
	{
	    strcpy ( xlabel, fnames[0][0] );
	    for ( i = 1; i < nplat; i++)
	        strcat ( xlabel, fnames[0][i] );
	}
	if (! pda_Search (Pd, c, "y-label", "xygraph", ylabel, 
		SYMT_STRING))
	{
	    strcpy ( ylabel, fnames[1][0] );
	    for ( i = 1; i < nplat; i++)
	        strcat ( ylabel, fnames[1][i] );
	}

/*
 * Look for data limits; otherwise set-up for auto scaling
 */
	autoscale = 0;
	if (! pda_Search (Pd, c, "x-max", NULL, (char *) &xmax, 
		SYMT_FLOAT))
		autoscale = autoscale | AUTO_XMAX;
	if (! pda_Search (Pd, c, "x-min", NULL, (char *) &xmin, 
		SYMT_FLOAT))
		autoscale = autoscale | AUTO_XMIN;
	if (! pda_Search (Pd, c, "y-max", NULL, (char *) &ymax, 
		SYMT_FLOAT))
	{
		autoscale = autoscale | AUTO_YMAX;
	}
	if (! pda_Search (Pd, c, "y-min", NULL, (char *) &ymin, 
		SYMT_FLOAT))
	{
		autoscale = autoscale | AUTO_YMIN;
	}

/*
 * Get line color for data from each platform
 */
	if ( pda_Search (Pd, c, "line-color", NULL, attrs, 
		SYMT_STRING))
	{
	    nattr = CommaParse( attrs, attrlist);
	    if ( nattr < nplat )
	    {
		msg_ELog ( EF_INFO, 
		    "Not enough line-colors specified, using white.");
	    }
	    for ( i = 0; i < nplat && i < nattr; i++)
	    {
		if ( GWDepth(Graphics) == 1 )
		{
			ct_GetColorByName("white", &(lcolor[i]));
		}
		else if (! ct_GetColorByName(attrlist[i], &(lcolor[i])))
		{
			msg_ELog(EF_PROBLEM, "Can't get default color: '%s'.",
				attrlist[i]);
			ct_GetColorByName("white", &(lcolor[i]));
	        }
		
	    }
	    for ( i = nattr; i < nplat; i++)
	    {
		ct_GetColorByName("white", &(lcolor[i]));
	    }
	}

	if (! pda_Search (Pd, "global", "ta-color", NULL, tadefcolor, 
		SYMT_STRING))
		strcpy(tadefcolor, "white");
	if (! ct_GetColorByName(tadefcolor, &Tadefclr))
	{
		msg_ELog(EF_PROBLEM, "Can't get default color: '%s'.",
			tadefcolor);
		strcpy(tadefcolor, "white");
		ct_GetColorByName(tadefcolor, &Tadefclr);
	}
/*
 * Attempt to load color table only if Pseudo-color device
 */
	if ( GWDepth(Graphics) == 8 )
	{
	    ct_LoadTable (ctname, &Colors, &Ncolors);
	    if (Ncolors < 8)
	    {
		msg_ELog(EF_PROBLEM, "XY-Graph color table too small");
		return;
	    }
	}

/*
 * Plot the annotation.
 */
	if (! update)
	{
	    An_TopAnnot ("X/Y Graph for ", Tadefclr.pixel);
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
		if (plat > 0)
		    An_TopAnnot (", ", Tadefclr.pixel);

		An_TopAnnot (pnames[plat], lcolor[plat].pixel);
		An_TopAnnot (":", lcolor[plat].pixel);
		An_TopAnnot (fnames[0][plat], lcolor[plat].pixel);
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

	    ptime = PlotTime;
	    if (! ds_DataTimes (pid, &ptime, 1, DsBefore, &ptime))
	    {
		msg_ELog (EF_INFO, "No data for '%s' at %d %d",
				pnames[plat], ptime.ds_yymmdd, 
				ptime.ds_hhmmss);
		continue;
	    }
	    flist[0] = fnames[0][plat];
	    flist[1] = fnames[1][plat];
	    dobj =ds_GetObservation (pid, flist, 2, &ptime, xyOrg,
			0.0, BADVAL);
	    if (! dobj)
	    {
			msg_ELog (EF_PROBLEM, 
				"Unable to get x-field for '%s' at %d %06d", 
				pnames[plat], PlotTime.ds_yymmdd, 
				PlotTime.ds_hhmmss);
			continue;
	    }

	    if ( xyOrg == OrgScalar )
		    npts = dobj->do_npoint;
	    else if ( xyOrg == Org1dGrid )
		    npts = dobj->do_desc.d_rgrid.rg_nX;

	    xdata[plat] = (float*)malloc( npts* sizeof(float));
	    ydata[plat] = (float*)malloc( npts* sizeof(float));
	    count = 0;
	    for ( ii = 0; ii < npts; ii++ )
	    {
		if ( dobj->do_data[0][ii] != BADVAL &&
		         dobj->do_data[1][ii] != BADVAL )
		{
		    xdata[plat][count] = dobj->do_data[0][ii];
		    ydata[plat][count] = dobj->do_data[1][ii];
		    count++;
		}
	    }
	    npts = count;
	    if ( (autoscale & AUTO_XMAX) && plat == 0)
		    	    xmax = xdata[plat][0];
	    if ( (autoscale & AUTO_YMAX) && plat == 0)
		    	    ymax = ydata[plat][0];
	    if ( (autoscale & AUTO_XMIN) && plat == 0)
		    	    xmin = xdata[plat][0];
	    if ( (autoscale & AUTO_YMIN) && plat == 0)
		    	    ymin = ydata[plat][0];

	    for ( ii = 0; ii < npts && autoscale; ii++ )
	    {
		    if ( autoscale & AUTO_XMAX )
		        xmax = xdata[plat][ii] > xmax ? xdata[plat][ii]: xmax;
		    if ( autoscale & AUTO_YMAX )
		        ymax = ydata[plat][ii] > ymax ? ydata[plat][ii]: ymax;
		    if ( autoscale & AUTO_XMIN )
		        xmin = xdata[plat][ii] < xmin ? xdata[plat][ii]: xmin;
		    if ( autoscale & AUTO_YMIN )
		        ymin = ydata[plat][ii] < ymin ? ydata[plat][ii]: ymin;
	    }
	    ds_FreeDataObject (dobj);
	}
/*
 * Now set the coordinate system and plot data and axis.
 */
	if ( plotAxis[AXIS_BOTTOM] )
	{
	    if ( (axid = ac_AxisId ( AXIS_BOTTOM,c)) < 0 )
	    {
	        ac_AddAxis ( AXIS_BOTTOM,c, axisTic,axisNticks, xmin,xmax,
			xlabel, axisFontScale, TransConfig & INVERT_X);
	    }
	    else if ( autoscale & AUTO_XMAX )
	    {
	        ac_ChangeAxisMax( AXIS_BOTTOM, axid, xmax );
	    }
	    else if ( autoscale & AUTO_XMIN )
	    {
	        ac_ChangeAxisMin( AXIS_BOTTOM, axid, xmin );
	    }
	    if ( !update ) ac_TouchAxis ( AXIS_BOTTOM,axid);
	}
	if ( plotAxis[AXIS_LEFT] )
	{
	    if ( (axid = ac_AxisId ( AXIS_LEFT,c)) < 0 )
	    {
	        ac_AddAxis ( AXIS_LEFT,c, axisTic,axisNticks, ymin,ymax,
			ylabel, axisFontScale, TransConfig & INVERT_Y);
	    }
	    else if ( autoscale & AUTO_YMAX )
	    {
	        ac_ChangeAxisMax( AXIS_LEFT, axid, ymax );
	    }
	    else if ( autoscale & AUTO_YMIN )
	    {
	        ac_ChangeAxisMin( AXIS_LEFT, axid, ymin );
	    }
	    if ( !update ) ac_TouchAxis ( AXIS_LEFT,axid);
	}
	if ( plotAxis[AXIS_TOP] )
	{
	    if ( (axid = ac_AxisId ( AXIS_TOP,c)) < 0 )
	    {
	        ac_AddAxis ( AXIS_TOP,c, axisTic,axisNticks, xmin,xmax,
			xlabel, axisFontScale, TransConfig & INVERT_X);
	    }
	    else if ( autoscale & AUTO_XMAX )
	    {
	        ac_ChangeAxisMax( AXIS_TOP, axid, xmax );
	    }
	    else if ( autoscale & AUTO_XMIN )
	    {
	        ac_ChangeAxisMin( AXIS_TOP, axid, xmin );
	    }
	    if ( !update ) ac_TouchAxis ( AXIS_TOP,axid);
	}

	if ( plotAxis[AXIS_RIGHT] )
	{
	    if ( (axid = ac_AxisId ( AXIS_RIGHT,c)) < 0 )
	    {
	        ac_AddAxis ( AXIS_RIGHT,c, axisTic,axisNticks, ymin,ymax,
			ylabel, axisFontScale, TransConfig & INVERT_Y);
	    }
	    else if ( autoscale & AUTO_YMAX )
	    {
	        ac_ChangeAxisMax( AXIS_RIGHT, axid, ymax );
	    }
	    else if ( autoscale & AUTO_YMIN )
	    {
	        ac_ChangeAxisMin( AXIS_RIGHT, axid, ymin );
	    }
	    if ( !update ) ac_TouchAxis ( AXIS_RIGHT,axid);
	}

	lc_SetUserCoord ( xmin,xmax,ymin,ymax);
/*
 * Plot the data.
 */
	gp_Clip( xmin, ymin,xmax,ymax );
	for (plat = 0; plat < nplat; plat++)
	{
	    gp_Pline( xdata[plat],ydata[plat],npts,L_solid, 
			  lcolor[plat].pixel);

	}
	XSetClipMask(XtDisplay(Graphics), Gcontext, None);
/*
 * Add a period to the top annotation
 */
	if ( !update )
	    An_TopAnnot (".  ", Tadefclr.pixel);
/*
 * Free local memory
	free(lcolor); free(xdata); free(ydata);
 */
	ac_DisplayAxes();
	TransConfig = saveConfig;
}

# endif /* C_PT_XYGRAPH */
