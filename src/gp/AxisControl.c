/*
 * Axis control. 
 */
static char *rcsid = "$Id: AxisControl.c,v 1.15 1993-11-15 22:42:16 burghart Exp $";
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
# include <message.h>
# include <DataStore.h>
# include "derive.h"
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "DrawText.h"
# include "ui_date.h"
# include "AxisControl.h"


/*
 *  Static functions -- function prototypes
 */
static void ac_GetAxisDescriptors FP((plot_description, char*, int, int,
                       int*, int*,int*, int*, int*, float*, DataValPtr, 
			float*, char*, char*, float*));
static void ac_SetPrivateAxisDescriptors FP((plot_description, char*, int,
                int,  int*, DataValPtr, int*, int*, int*, float*));
static void ac_FormatLabel FP((DataValPtr, char*[], int*, int ));
static void ac_ComputeAxisDescriptors FP((plot_description, char*, int, int));
static int ac_DrawAxis FP((plot_description, char*, int, int, int));
static void ac_GetLabel FP ((plot_description, char *, int, char *));



int 
ac_DisplayAxes ()
/*
 * Display all axes of plot, fitting into allocated space on each side.
 */
{
    int 		ic,i;
    char		side;
    int 	        fitAxes = 0;
    int			nextoffset = 0;
    unsigned short	scalemode = 0;
    static int		oldHeight = 0;
    static int		oldncomps = 0;
    static int          zlev = 0;
    int		        ncomps = 0;
    int			currentHeight = GWHeight(Graphics);
    char		**comps;
    int			computed;
    char		datatype;
    char		dim;
    /*
     * Get list of components and count them.
     */
    comps = pd_CompList(Pd);
    for ( ic = 1; comps[ic]; ic++);
    ncomps = ic-1;
    /*
     *  Compute the axes: i.e. determine font-size and tic-base.
     *
     *  If the Graphics widget has been resized, then the axes need to
     *  re-computed and re-fitted as the fontscale is dependent upon 
     *  the size of the Graphics widget.
     *  Also, if there have been components added or deleted, the
     *  axes need to be fitted.
     */
    if ( currentHeight != oldHeight || ncomps != oldncomps) fitAxes = 1;
    for ( i = 0; i < 4; i++)
    {
	side = i == AXIS_BOTTOM ? 'b' :
		i == AXIS_LEFT ? 'l' :
		i == AXIS_RIGHT ? 'r' :
		i == AXIS_TOP ? 't' : 'n' ;
	nextoffset = 0;
        for ( ic = 1; comps[ic]; ic++ )
        {
            if ( ac_PlotAxis( Pd, comps[ic], side ) )
            {
	        computed = ac_QueryAxisState(Pd,comps[ic],side,&datatype);
	        if ( !computed || oldHeight != currentHeight || zlev != Zlevel )
	        {
		    ac_ComputeAxisDescriptors(Pd, comps[ic], side, datatype);
		    computed = 1;
		    ac_UpdateAxisState(Pd,comps[ic],side, NULL, &computed );
	        }
		dim = side == 'r' || side == 'l' ? 'y' : 'x' ;
	        xy_GetScaleInfo(Pd,comps[ic],dim, &scalemode);
		if ( fitAxes )
		{
		    ac_SetPrivateAxisDescriptors(Pd, comps[ic],side, datatype,
			 &nextoffset,NULL,NULL,NULL, NULL,NULL);
		}
	    	nextoffset = ac_DrawAxis(Pd,comps[ic],side, datatype,scalemode);
            }
        }
    }
    oldHeight = GWHeight(Graphics);
    oldncomps = ncomps;
    zlev = Zlevel;
}


static void
ac_FormatLabel( d1, string, nlab, dim )
DataValPtr	d1;
char		*string[];
int		*nlab;
int		dim;
{
    char	*lab[5];
    char	tstring[80];
    int		i;
    *nlab = 1;
    switch (d1->type)
    {
	case 'f':
	    string[0] = (char*)calloc(80,sizeof(char));
	    sprintf ( string[0], "%.2f",d1->val.f);
	break;
	case 'd':
	    string[0] = (char*)calloc(80,sizeof(char));
	    sprintf ( string[0], "%.2f",(double)(d1->val.d));
	break;
	case 'i':
	    string[0] = (char*)calloc(80,sizeof(char));
	    sprintf ( string[0], "%d",d1->val.i);
	break;
	case 't':
	    TC_EncodeTime ( &(d1->val.t), TC_Full, tstring );
	    *nlab = CommaParse( tstring, lab);
	    for ( i = 0; i < *nlab; i++)
	    {
	        string[i] = (char*)calloc(80,sizeof(char));
		strcpy( string[i], lab[i] );
	    }
	break;
    }
}

static void
ac_ComputeAxisDescriptors (pd, c, side, datatype )
plot_description	pd;
char			*c;
char			side;
char			datatype;
{
    DataValRec	min,max;
    int		offset,maxWidth,maxHeight,nticLabel;
    int		ticlen; 
    float	ticInterval;
    DataValRec	baseTic,ticLoc;
    char	color[80],label[80];
    float	fscale;
    char	*ticLabel[2];
    int		nlab;
    int		maxDim = 80;
    int		kk,x1,y1,x2,y2;
    long	iVal,iBase;
    float	drawGrid;
    int		nint;
    char	*flist[10];
    char	fnames[80];
    int		n;
    unsigned short	scalemode = 0;

    /*
     *  Get parameters to use while computing the scale.
     */
    if ( side == 't' || side == 'b' )
    {
	if (pda_ReqSearch( pd, c, "x-field", NULL, fnames, SYMT_STRING))
	    n = CommaParse ( fnames,flist );
        xy_GetCurrentScaleBounds(pd,c,'x',datatype,&min,&max,
				 n > 0 ? flist[0] : "");
	xy_GetScaleInfo(pd,c,'x', &scalemode);
	lc_ComputeZoom( &min, &max, 'x',scalemode);
    }
    else if ( side == 'r' || side == 'l' )
    {
	if (pda_ReqSearch( pd, c, "y-field", NULL, fnames, SYMT_STRING))
	    n = CommaParse ( fnames,flist );
        xy_GetCurrentScaleBounds(pd,c,'y',datatype,&min,&max,
				 n > 0 ? flist[0] : "");
	xy_GetScaleInfo(pd,c,'y', &scalemode);
	lc_ComputeZoom( &min, &max, 'y',scalemode);
    }

    ac_GetAxisDescriptors( pd, c, side, datatype, &offset,
		&maxWidth, &maxHeight, &nticLabel, &ticlen, &ticInterval,
		&baseTic,&fscale, color, label, &drawGrid );
    /*
     * Now re-compute the private axis-descriptors;
     */
    baseTic.type = min.type;
    switch ( min.type )
    {
	case 't':
	{
	    iVal = min.val.t.zt_Sec;
	    nint = iVal/((long)ticInterval);
	    baseTic.val.t.zt_Sec =  nint*((long)ticInterval) < iVal?
		((long)(nint+1)) * (long)ticInterval :
		((long)nint) * (long)ticInterval ;
	    baseTic.val.t.zt_MicroSec = 0;
	}
	break;
	case 'f':
	{
	    nint = (int)(min.val.f/ticInterval);
	    baseTic.val.f = ((float)nint)*ticInterval < min.val.f ?
		((float)(nint+1)) * ticInterval :
		((float)nint) * ticInterval ;
	}
	break;
    }
    
    /*
     *  Loop on tic-labels to determine required space.
     */
    ticLoc = baseTic;
    maxWidth = 0;
    maxHeight = 0;
    nticLabel = 0;
    while ( lc_CompareData(&ticLoc,&max) <= 0 )
    {
	ac_FormatLabel ( &ticLoc, ticLabel, &nlab,maxDim );
	for ( kk = 0; kk < nlab; kk++)
	{
            DT_TextBox ( Graphics, GWFrame(Graphics), 0,0,
				ticLabel[kk], 0.0, fscale, 
				JustifyLeft, JustifyTop, &x1,&y1,&x2,&y2);
	    maxWidth = abs(x2-x1) > maxWidth ? abs(x2-x1) : maxWidth;
	    maxHeight = abs(y2-y1) > maxHeight ? abs(y2-y1) : maxHeight;
	    free(ticLabel[kk]);
	}
	nticLabel = nlab > nticLabel ? nlab : nticLabel;
	lc_IncrData( &ticLoc, (double)ticInterval );
    }
    ac_SetPrivateAxisDescriptors(pd,c,side,datatype,
		NULL,&baseTic,&maxHeight,&maxWidth, &nticLabel,&ticInterval);
}

static int
ac_DrawAxis(pd,c,side,datatype,mode)
plot_description	pd;
char			*c;
char			side, datatype;
unsigned short		mode;
{
    DataValRec	min,max;
    int		offset,maxWidth,maxHeight,maxLabel;
    int		ticlen; 
    float	ticInterval;
    DataValRec	baseTic;
    char	color[80],label[80];
    float	fscale;
    char	*ticLabel[2];
    int		nlab, even;
    int		maxDim = 80;
    int		labelExtent;
    int		kk;
    DataValRec	ticLoc;
    int		fit = 1;
    int		xloc,yloc;
    DataValRec	savemin, savemax;
    int		axisSpaceHeight;
    float	center;
    int		axisSpaceWidth;
    DataValRec	yOrig,xOrig;
    DataValRec	umin,umax;
    int		direction;
    int		nextoffset = 0;
    unsigned short xmode = 0,ymode = 0;
    float	drawGrid;
    XColor	mainPix, gridPix;
    float	red,green,blue,hue,lightness,saturation;
    int		x1,x2,y1,y2,n;
    char	*flist[10];
    char	fnames[80];
    char	smin[80],smax[80];

    if ( side == 't' || side == 'b' )
    {
	if (pda_ReqSearch( pd, c, "x-field", NULL, fnames, SYMT_STRING))
	    n = CommaParse ( fnames,flist );
        xy_GetCurrentScaleBounds(pd,c,'x',datatype,&min,&max,
				 n > 0 ? flist[0] : "");
    }
    else if ( side == 'r' || side == 'l' )
    {
	if (pda_ReqSearch( pd, c, "y-field", NULL, fnames, SYMT_STRING))
	    n = CommaParse ( fnames,flist );
        xy_GetCurrentScaleBounds(pd,c,'y',datatype,&min,&max,
				 n > 0 ? flist[0] : "");
    }

    ac_GetAxisDescriptors( pd, c, side, datatype, &offset,
		&maxWidth, &maxHeight, &maxLabel, &ticlen, &ticInterval,
		&baseTic,&fscale, color, label,&drawGrid );
    XSetLineAttributes ( XtDisplay(Graphics), Gcontext, 0, LineSolid,
		CapButt, JoinMiter);
/*    SetColor(c,"axis-color",NULL,color);*/
    if ( !ct_GetColorByName( color, &mainPix ) )
    {
	msg_ELog ( EF_PROBLEM, "Unknown axis color: %s", color );
	ct_GetColorByName ( "white", &mainPix );
    }
    gridPix = mainPix;
    
    /*
     * Compute the RGB values for the grid color intensity
     */
    if ( drawGrid > 0.0 && drawGrid < 1.0 )
    {
	gp_RGBtoHLS ((float)((double)mainPix.red /(double)65535), 
		     (float)((double)mainPix.green /(double)65535),
		     (float)((double)mainPix.blue /(double)65535), 
		     &hue, &lightness, &saturation );
	lightness = lightness * drawGrid;
	gp_HLStoRGB ( &red, &green, &blue, hue, lightness, saturation );
	gridPix.red = (short)((double)red * (double)65535);
	gridPix.green = (short)((double)green * (double)65535);
	gridPix.blue = (short)((double)blue * (double)65535);
	ct_GetColorByRGB( &gridPix );
    }
    XSetForeground ( Disp, Gcontext, mainPix.pixel );
	
    msg_ELog ( EF_DEBUG,"Draw Axis: component = %s side = %c datatype = %c",
		c,side,datatype);
    switch (side)
    {
	case 't':
	    direction = -1;
	    axisSpaceHeight = abs((int)(GWHeight(Graphics)*AxisY1[AXIS_TOP]) -
			(int)(GWHeight(Graphics)*AxisY0[AXIS_TOP]));
	    lc_GetUserCoord (NULL,NULL,NULL,&yOrig,ymode);
	    goto horizontal;
	case 'b':
	    direction = 1;
	    axisSpaceHeight = (int)(GWHeight(Graphics)*AxisY1[AXIS_BOTTOM]) -
			(int)(GWHeight(Graphics)*AxisY0[AXIS_BOTTOM]);
	    lc_GetUserCoord (NULL,NULL,&yOrig,NULL,ymode);
horizontal:
	    xmode = mode;
	   /*
            * Set-up coordinate system.
            */
            lc_GetOrigCoord ( &savemin, &savemax, NULL, NULL);
	    lc_SetUserCoord ( &min,&max, NULL,NULL);
            lc_GetUserCoord ( &min,&max, NULL,NULL, xmode);
            switch ( datatype )
            {
	        case 'f':
                    msg_ELog ( EF_DEBUG,
			"Draw Axis: component = %s x-min = %f x-max = %f",
			c, min.val.f, max.val.f);
	        break;
	        case 't':
	            TC_EncodeTime ( &(min.val.t), TC_Full, smin );
	            TC_EncodeTime ( &(max.val.t), TC_Full, smax );
                    msg_ELog ( EF_DEBUG,
			"Draw Axis: component = %s x-min = %s x-max = %s",
			c, smin, smax );
	        break;
            }

	   /*
            * Draw axis-line.
            */
	    XDrawLine( XtDisplay(Graphics), GWFrame(Graphics), Gcontext, 
		devX(&min,xmode), devY(&yOrig,ymode) + direction*(offset), 
		devX(&max,xmode), devY(&yOrig,ymode) + direction*(offset));

	    labelExtent = xmode & INVERT ?
		devX(&min,xmode) + abs((int)(GWWidth(Graphics)*AxisX1[AXIS_RIGHT]) -
			(int)(GWWidth(Graphics)*AxisX0[AXIS_RIGHT])):
		devX(&min,xmode) - abs((int)(GWWidth(Graphics)*AxisX1[AXIS_LEFT]) -
			(int)(GWWidth(Graphics)*AxisX0[AXIS_LEFT]));
	    /* 
	     * Draw tic-marks and tic-labels
	     */
	    ticLoc = baseTic;
	    while ( lc_CompareData(&ticLoc,&max) <= 0 )
	    {
		if ( ticlen > axisSpaceHeight )
		{
		    fit = 0;
		    break;
		}
		else
		{

		  /* 
		   * Check for tic label to fit in given space 
		   */
		  if ( (maxHeight+2)* maxLabel > axisSpaceHeight - ticlen )
		  {
		    fit = 0;
		    break;
		  }
		  else
		  {
		    xloc = devX(&ticLoc,xmode);
		    /*
		     *  Make sure tic - labels don't over-lap
		     */
		    if ( xmode & INVERT ? xloc + (maxWidth/2) < labelExtent :
				xloc - (maxWidth/2) > labelExtent )
		    {
		        ac_FormatLabel ( &ticLoc, ticLabel, &nlab,maxDim );
		        yloc = devY(&yOrig,ymode) + direction*(offset + ticlen + 2);
		        kk = direction > 0 ? 0 : nlab-1; 
		        fit = 1;
		        while( direction > 0 ? kk < nlab: kk >= 0 )
		        {
		    	    XDrawLine( XtDisplay(Graphics), 
			    	GWFrame(Graphics), Gcontext, 
			    	devX(&ticLoc,xmode), 
				devY(&yOrig,ymode) + direction*(offset), 
			    	devX(&ticLoc,xmode), 
			    	devY(&yOrig,ymode) + direction*(offset + ticlen));
			    if ( drawGrid > 0.0 )
			    {
				lc_GetUserCoord(NULL, NULL, &umin,&umax, ymode);
    				XSetForeground ( Disp, Gcontext, gridPix.pixel);
		    	        XDrawLine( XtDisplay(Graphics), 
			    	    GWFrame(Graphics), Gcontext, 
			    	    devX(&ticLoc,xmode), 
				    devY(&umax,ymode) , 
			    	    devX(&ticLoc,xmode), 
			    	    devY(&umin,ymode) );
    				XSetForeground ( Disp, Gcontext, mainPix.pixel);
			    }
		            DrawText ( Graphics, GWFrame(Graphics), 
				    Gcontext, xloc, yloc,
				    ticLabel[kk], 0.0,fscale, 
				    JustifyCenter, 
				    direction > 0 ? JustifyTop : JustifyBottom);
			    free(ticLabel[kk]);
			    kk += direction;
			    yloc += direction*(maxHeight + 2);
		        }
			labelExtent = xmode & INVERT ? xloc - maxWidth/2 :
				xloc + maxWidth/2 ;
		    }
		    else
		    {
		        XDrawLine( XtDisplay(Graphics), 
			    GWFrame(Graphics), Gcontext, 
			    devX(&ticLoc,xmode), devY(&yOrig,ymode) + direction*(offset), 
			    devX(&ticLoc,xmode), 
			    devY(&yOrig,ymode) + direction*(offset + ticlen/2));
		    }
		  }
		}
		    
		lc_IncrData( &ticLoc, (double)(ticInterval) );
	    }
	    nextoffset = offset + ticlen + (maxHeight+2)*maxLabel + 2;
	    /*
	     * Now draw the axis label
	     */
	    xloc = xmode & INVERT ? devX(&max,xmode) + 
			(abs(devX(&min,xmode) - devX(&max,xmode)))/2:
	    	devX(&min,xmode) + 
			(abs(devX(&min,xmode) - devX(&max,xmode)))/2;
	    yloc = devY(&yOrig,ymode) + direction*(nextoffset); 
            DrawText ( Graphics, GWFrame(Graphics), 
		    Gcontext, xloc, yloc, label, 0.0,fscale, 
		    JustifyCenter, direction > 0 ? JustifyTop : JustifyBottom);
	    nextoffset = nextoffset + maxHeight + 2;
	    lc_SetUserCoord ( &savemin,&savemax, NULL,NULL);
	break;
	case 'r':
	    direction = 1;
	    axisSpaceWidth = abs((int)(GWWidth(Graphics)*AxisX1[AXIS_RIGHT]) -
			(int)(GWWidth(Graphics)*AxisX0[AXIS_RIGHT]));
	    lc_GetUserCoord (NULL,&xOrig,NULL,NULL,xmode);
	    goto vertical;
	case 'l':
	    direction = -1;
	    axisSpaceWidth = abs((int)(GWWidth(Graphics)*AxisX1[AXIS_LEFT]) -
			(int)(GWWidth(Graphics)*AxisX0[AXIS_LEFT]));
	    lc_GetUserCoord (&xOrig,NULL,NULL,NULL,xmode);
vertical:
	    ymode = mode;
	   /*
            * Set-up coordinate system.
            */
            lc_GetOrigCoord ( NULL,NULL, &savemin, &savemax );
	    lc_SetUserCoord ( NULL,NULL,&min,&max );
	    lc_GetUserCoord ( NULL,NULL,&min,&max, ymode );
            switch ( datatype )
            {
	        case 'f':
                    msg_ELog ( EF_DEBUG,
			"Draw Axis: component = %s y-min = %f y-max = %f",
			c, min.val.f, max.val.f);
	        break;
	        case 't':
	            TC_EncodeTime ( &(min.val.t), TC_Full, smin );
	            TC_EncodeTime ( &(max.val.t), TC_Full, smax );
                    msg_ELog ( EF_DEBUG,
			"Draw Axis: component = %s y-min = %s y-max = %s",
			c, smin, smax );
	        break;
            }

	   /*
            * Draw axis-line.
            */
	    XDrawLine( XtDisplay(Graphics), GWFrame(Graphics), Gcontext, 
			devX(&xOrig,xmode)+direction*(offset), devY(&min,ymode), 
			devX(&xOrig,xmode)+direction*(offset), devY(&max,ymode));

	    labelExtent = devY(&min,ymode);
	    labelExtent = ymode & INVERT ?
		devY(&min,ymode) - 
		  abs((int)(GWHeight(Graphics)*AxisY1[AXIS_TOP]) -
			(int)(GWHeight(Graphics)*AxisY0[AXIS_TOP])):
		devY(&min,ymode) + 
		  abs((int)(GWHeight(Graphics)*AxisY1[AXIS_BOTTOM]) -
			(int)(GWHeight(Graphics)*AxisY0[AXIS_BOTTOM]));
	    /* 
	     * Draw tic-marks and tic-labels
	     */
	    ticLoc = baseTic;
	    while ( lc_CompareData(&ticLoc,&max) <= 0 )
	    {
		if ( ticlen > axisSpaceWidth )
		{
		    fit = 0;
		    break;
		}
		else
		{
		  /* 
		   * Check for tic label to fit in given space 
		   */
		  if ( maxWidth > axisSpaceWidth - ticlen )
		  {
		    fit = 0;
		    break;
		  }
		  else
		  {
		    ac_FormatLabel ( &ticLoc, ticLabel, &nlab,maxDim );
		    xloc = devX(&xOrig,xmode) + direction*(offset + ticlen + 2);
		    xloc = xloc + direction*(maxWidth/2);
                    center = (float)(nlab-1)/2.0;
                    even = nlab%2 ? 0 : 1;
		    for ( kk = nlab-1; kk >= 0; kk--)
		    {
			yloc = devY(&ticLoc,ymode) -
                            (center-(float)(kk))*(maxHeight+1);

		        if ( ((yloc+maxHeight/2) < labelExtent && 
					!(ymode & INVERT)) ||
			       ((yloc-maxHeight/2) > labelExtent && 
					 (ymode &INVERT)) )
		        {
		    	    XDrawLine( XtDisplay(Graphics), 
			    	GWFrame(Graphics), Gcontext, 
			    	devX(&xOrig,xmode) + direction*(offset), 
			    	devY(&ticLoc,ymode), 
			    	devX(&xOrig,xmode) + direction*(offset+ticlen), 
			    	devY(&ticLoc,ymode));
			    if ( drawGrid > 0.0 )
			    {
				lc_GetUserCoord(&umin,&umax, NULL, NULL, xmode);
    				XSetForeground ( Disp, Gcontext, gridPix.pixel);
		    	        XDrawLine( XtDisplay(Graphics), 
			    	    GWFrame(Graphics), Gcontext, 
			    	    devX(&umin,xmode) , 
			    	    devY(&ticLoc,ymode), 
			    	    devX(&umax,xmode) , 
			    	    devY(&ticLoc,ymode));
    				XSetForeground ( Disp, Gcontext, mainPix.pixel);
			    }
		            DrawText ( Graphics, GWFrame(Graphics), 
				    Gcontext, xloc, yloc,
				    ticLabel[kk], 0.0,fscale, 
				    JustifyCenter, JustifyCenter);
			        if(direction > 0 ? kk==nlab-1 : kk==0)
				    labelExtent = ymode & INVERT ? 
					yloc+maxHeight/2 : 
					yloc-maxHeight/2;
		        }
			else
			{
		    	    XDrawLine( XtDisplay(Graphics), 
			    	GWFrame(Graphics), Gcontext, 
			    	devX(&xOrig,xmode) + direction*(offset), 
			    	devY(&ticLoc,ymode), 
			    	devX(&xOrig,xmode) + direction*(offset+ticlen/2), 
			    	devY(&ticLoc,ymode));
			}
			free(ticLabel[kk]);
			yloc += direction*(maxHeight + 2);
		    }
		  }
		}
		    
		lc_IncrData( &ticLoc, (double)(ticInterval) );
	    }
	    nextoffset = offset + ticlen + maxWidth + 2;
	    /*
	     * Now draw the axis label
	     */
	    yloc = ymode & INVERT ? devY(&min,ymode) + 
			(abs(devY(&min,ymode) - devY(&max,ymode)))/2:
	    	devY(&max,ymode) + 
			(abs(devY(&min,ymode) - devY(&max,ymode)))/2;
	    xloc = devX(&xOrig,xmode) + direction*(nextoffset); 
            DrawText ( Graphics, GWFrame(Graphics), 
		    Gcontext, xloc, yloc, label, direction > 0 ? -90.0 :90.0,
		    fscale, JustifyCenter, JustifyBottom);

	    /*
	     * Because the (rotated)title text will be stroked, it's size might
	     * differ from the label text so test to see how tall it really is
	     * to compute next offset
	     */
            DT_TextBox ( Graphics, GWFrame(Graphics), 0, 0, 
		    label, direction > 0 ? -90.0 :90.0,
		    fscale, JustifyCenter, JustifyBottom,&x1,&y1,&x2,&y2);
	    nextoffset = nextoffset + abs(x1-x2) + 2;
	    lc_SetUserCoord ( NULL,NULL,&savemin,&savemax);
	break;

    }
    if ( !fit )
    {
        msg_ELog (EF_PROBLEM,"Not enough space for axis: %s on side: %c",
	flist[0],side);
    }
    ResetGC();
    return(nextoffset);
}

int
ac_QueryAxisState (pd,c,side,datatype)
plot_description	pd; /* input */
char			*c; /* input */
char			side; /* input */
char			*datatype; /*return*/
{
    char	keyword[80];
    int		computed = 0;
    char	dtype[3];

    *datatype = 'n';
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-computed");
    if (! pda_Search (pd, c, keyword, NULL, (char *)&computed, SYMT_INT))
    {
	computed = 0;
    }

    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-datatype");
    if (  pda_Search (pd, c, keyword, NULL, dtype, SYMT_STRING) )
    {
	*datatype = dtype[0];
    }

    return ( computed );
}

void
ac_UpdateAxisState(pd,c,side, datatype, computed )
plot_description	pd;
char	*c;
char	side;
char	*datatype;
int	*computed;
{
    char	keyword[80];
    char	dtype[3];

    if ( computed )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-computed");
        pd_Store (pd,c,keyword,(char *)computed, SYMT_INT);
    }

    if ( datatype )
    {
        dtype[0] = datatype[0];
        dtype[1] = '\0';
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-datatype");
        pd_Store (pd,c,keyword,dtype, SYMT_STRING);
    }
}
int
ac_PlotAxis(pd, c, side)
plot_description pd; /* input */
char    *c;          /* input */
char	side;
{
    bool plot=FALSE;
    char ptype[32];
    if ( !pda_Search (pd, "global", "plot-type", NULL, ptype,SYMT_STRING)||
	 strcmp(ptype,"xygraph") != 0)
    {
	return(0);
    }
    switch ( side )
    {
	case 'b':
    	    if ( !pda_Search (pd, c, "axis-bottom", "xy",
                (char *)&plot, SYMT_BOOL))
    	    {
        	plot = 1;
    	    }
	break;
	case 't':
    	    if ( !pda_Search (pd, c, "axis-top", "xy",
                (char *)&plot, SYMT_BOOL))
    	    {
        	plot = 0;
    	    }
	break;
	case 'r':
    	    if ( !pda_Search (pd, c, "axis-right", "xy",
                (char *)&plot, SYMT_BOOL))
    	    {
        	plot = 0;
    	    }
	break;
	case 'l':
    	    if ( !pda_Search (pd, c, "axis-left", "xy",
                (char *)&plot, SYMT_BOOL))
    	    {
        	plot = 1;
    	    }
	break;
    }
    return (plot);
}

static void
ac_GetAxisDescriptors( pd, c, side, datatype,
                       offset, tlabelWidth,tlabelHeight, nticLabel,
                       ticlen, ticInterval,baseTic,fontScale, color, label,
                       drawGrid)
plot_description pd; /* input */
char    *c;          /* input */
char    side;        /* input */
char    datatype;    /* input */
int     *offset;
int     *tlabelWidth,*tlabelHeight;
int     *nticLabel;
int     *ticlen;
float   *ticInterval;
DataValPtr      baseTic;
float   *fontScale;
char    *color;
char    *label;
float   *drawGrid;
{
    char        keyword[80];
    char        string[80];
    /*
     * Get the user-settable axis descriptors
     */
    if ( color )
    {
        strcpy(keyword, "axis-");
        keyword[5] = side;
        keyword[6] = '\0';
        strcat(keyword, "-color");
        if (! pda_Search (pd, c, keyword, "xy", (char *)color, SYMT_STRING))
        {
            strcpy(color,"white");
        }
    }
    if (label)
	    ac_GetLabel (pd, c, side, label);

    if ( ticlen )
    {
        strcpy(keyword, "axis-");
        keyword[5] = side;
        keyword[6] = '\0';
        strcat(keyword, "-tic-len");
        if (! pda_Search (pd, c, keyword, "xy", (char *)ticlen, SYMT_INT))
        {
            *ticlen = 5;
        }
    }
    if ( ticInterval )
    {
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
    }
    if ( fontScale )
    {
        strcpy(keyword, "axis-");
        keyword[5] = side;
        keyword[6] = '\0';
        strcat(keyword, "-font-scale");
        if (! pda_Search (pd, c, keyword, "xy", (char *)fontScale, SYMT_FLOAT))
        {
            *fontScale = 0.025;
        }
    }
    if ( drawGrid )
    {
        strcpy(keyword, "axis-");
        keyword[5] = side;
        keyword[6] = '\0';
        strcat(keyword, "-grid-intensity");
        if (! pda_Search (pd, c, keyword, "xy", (char *)drawGrid, SYMT_FLOAT))
        {
            *drawGrid = 0.75;
        }
    }

    
    /*
     * Get the axis descriptors that are private (calculated)
     */
    if ( offset )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-offset");
        if (! pda_Search (pd, c, keyword, NULL, (char *)offset, SYMT_INT))
        {
            *offset = 0;
        }
    }
    if ( baseTic )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-base-tic");
        baseTic->type = datatype;
        switch( datatype )
        {
            case 't':
                if (! pda_Search (pd, c, keyword, NULL,
                    (char *) &(baseTic->val.t), SYMT_DATE))
                {
                    baseTic->val.t.zt_Sec = 0;
                    baseTic->val.t.zt_MicroSec = 0;
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
    }
    if ( tlabelHeight )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-tic-label-height");
        if (! pda_Search (pd, c, keyword, NULL, (char *)tlabelHeight, SYMT_INT))
        {
            *tlabelHeight = 0;
        }
    }
    if ( nticLabel )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-n-tic-label");
        if (! pda_Search (pd, c, keyword, NULL, (char *)nticLabel, SYMT_INT))
        {
            *nticLabel = 0;
        }
    }
    if ( tlabelWidth )
    {
        strcpy(keyword, "private-axis-");
        keyword[13] = side;
        keyword[14] = '\0';
        strcat(keyword, "-tic-label-width");
        if (! pda_Search (pd, c, keyword, NULL, (char *)tlabelWidth, SYMT_INT))
        {
            *tlabelWidth = 0;
        }
    }
}
static void
ac_SetPrivateAxisDescriptors( pd, c, side, datatype, 
                       offset, baseTic,tlabelHeight,tlabelWidth,nticLabel,
                       ticInterval)
plot_description pd; /* input */
char    *c;          /* input */
char    side;        /* input */
char    datatype;    /* input */
int     *offset;
DataValPtr      baseTic;
int     *tlabelHeight,*tlabelWidth;
int     *nticLabel;
float   *ticInterval;
{
    char        keyword[80];

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
                pd_Store(pd, c, keyword, (char *) &(baseTic->val.t), SYMT_DATE);
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





static void
ac_GetLabel (pd, c, side, label)
plot_description pd;
char *c, side, *label;
/*
 * Figure out a label for this side.
 */
{
	char keyword[120], field[120];
	FieldId fid;
/*
 * Try to find an explicit label first.
 */
        strcpy(keyword, "axis-");
        keyword[5] = side;
        keyword[6] = '\0';
        strcat(keyword, "-label");
        if (pda_Search (pd, c, keyword, "xy", label, SYMT_STRING))
		return;
/*
 * Figure out what our display field is.
 */
	if (! pda_Search (pd, c, (side == 'b' || side == 't') ? "x-field" :
			  "y-field", "xy", field, SYMT_STRING))
	{
		strcpy (label, "Who knows?");
		return;
	}
/*
 * If we can successfully look up the field, then set the label to be its
 * description.  Otherwise just use the literal field text.  The lookup will
 * fail, of course, with comma-separated field lists; but I don't know how
 * we would annotate those anyway.
 */
	if ((fid = F_Declared (field)) != BadField)
		strcpy (label, F_GetDesc (fid));
	else
		strcpy (label, field);
}
