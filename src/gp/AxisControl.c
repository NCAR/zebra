/*
 * Skew-t plotting module
 */
static char *rcsid = "$Id: AxisControl.c,v 1.5 1992-01-10 18:54:07 barrett Exp $";
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

/*
 * General definitions
 */

# define BADVAL		-999.0
# define AUTO_XMAX	(1<<0)
# define AUTO_YMAX	(1<<1)
# define AUTO_XMIN	(1<<2)
# define AUTO_YMIN	(1<<3)
# define ROUNDIT(x)	((long)(x + 0.5))

extern XColor   Tadefclr;
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;


# define MAX_AXIS  5

typedef struct _AxisInfo
{
    char	*component;
    int		computed;
    char	datatype;
} AxisInfoRec;
static int FitAxes = 0;

static AxisInfoRec AxisInfoList[4][5];
# ifdef __STDC__
    extern int ac_DisplayAxes( );
/*
    static int ac_AxisId( char, char* );
    static int ac_NewAxisId( char, char* );
    static int ac_AxisSide( char );
    static int ac_AddAxis( char, char*, char );
    static void ac_FormatLabel( DataValPtr, char*[], int*, int );
    static void ac_ComputeAxisDescriptors( plot_description, char*, char, char );
    static int ac_DrawAxis(plot_description,char*,char,char,unsigned short);
    extern int ac_AxisState(plot_description,char*,char,char,DataValPtr,DataValPtr );
    extern void ac_UpdateAxisState(plot_description,char*,char, char,DataValPtr, DataValPtr );
*/
# else
    extern int ac_DisplayAxes();
    static int ac_AxisId();
    static int ac_NewAxisId();
    static int ac_AxisSide();
    static int ac_AddAxis();
    static void ac_FormatLabel();
    static void ac_ComputeAxisDescriptors();
    static int ac_DrawAxis();
    extern int ac_AxisState();
    extern void ac_UpdateAxisState();
# endif

int ac_DisplayAxes ()
{
    int i,h;
    char		side;
    int			nextoffset = 0;
    unsigned short	scalemode = 0;
    static int		oldHeight = 0;
    int			currentHeight = GWHeight(Graphics);

    /*
     *  If the Graphics widget has been resized, that the axes need to
     *  re-computed and re-fitted as the fontscale is dependent upon 
     *  the size of the Graphics widget.
     */
    if ( currentHeight != oldHeight ) FitAxes = 1;
    for ( i = 0; i < MAX_AXIS; i++ )
    {
	for ( h=0; h < 4;h++)
	{
	    if ( AxisInfoList[h][i].component && 
		(!(AxisInfoList[h][i].computed)||
		oldHeight != currentHeight))
	    {
			side = h == AXIS_BOTTOM ? 'b' :
			       h == AXIS_LEFT ? 'l' :
			       h == AXIS_TOP ? 't' :
			       h == AXIS_RIGHT ? 'r':'n',
		ac_ComputeAxisDescriptors(Pd, AxisInfoList[h][i].component,
			side,
			AxisInfoList[h][i].datatype);
		AxisInfoList[h][i].computed = 1;
	    }
	}
    }
    for ( h=0; h < 4;h++)
    {
	nextoffset = 0;
        for ( i = 0; i < MAX_AXIS; i++ )
	{
	    if ( AxisInfoList[h][i].component )
	    {
		xy_GetScaleMode(Pd,AxisInfoList[h][i].component,
				h == AXIS_BOTTOM || h == AXIS_TOP ? 'x' :
				h == AXIS_LEFT || h == AXIS_RIGHT ? 'y' : 'n',
				&scalemode);
		if ( FitAxes )
		{
		    xy_SetPrivateAxisDescriptors(Pd,
			AxisInfoList[h][i].component,
			side = h == AXIS_BOTTOM ? 'b' :
			       h == AXIS_LEFT ? 'l' :
			       h == AXIS_TOP ? 't' :
			       h == AXIS_RIGHT ? 'r':'n',
			AxisInfoList[h][i].datatype,
			&nextoffset,NULL,NULL,NULL, NULL,NULL);
		}
	    	nextoffset = ac_DrawAxis(Pd,AxisInfoList[h][i].component,
			side = h == AXIS_BOTTOM ? 'b' :
			       h == AXIS_LEFT ? 'l' :
			       h == AXIS_TOP ? 't' :
			       h == AXIS_RIGHT ? 'r':'n',
		AxisInfoList[h][i].datatype,scalemode);
	    }
	}
    }
    FitAxes = 0;
    oldHeight = GWHeight(Graphics);
}
static int
ac_AxisId( side, cName )
char    side;
char	*cName;
{
    int id = -1;
    int i;
    if ( cName )
    {
	for ( i = 0; i < MAX_AXIS; i++)
	{
	    if ( AxisInfoList[ac_AxisSide(side)][i].component &&
	(strcmp(AxisInfoList[ac_AxisSide(side)][i].component, cName) == 0))
		break;
	}
	if ( i < MAX_AXIS )
	    id = i;
    }
    return ( id );
}
static int
ac_NewAxisId ( side, cName )
char	side;
char	*cName;
{
    int id = -1;
    int i;
    static init = 1;
    if ( init )
    {
	init = 0;
	for ( i = 0; i < MAX_AXIS; i++)
	{
	    AxisInfoList[AXIS_BOTTOM][i].component = NULL;
	    AxisInfoList[AXIS_LEFT][i].component = NULL;
	    AxisInfoList[AXIS_RIGHT][i].component = NULL;
	    AxisInfoList[AXIS_TOP][i].component = NULL;
	    AxisInfoList[AXIS_BOTTOM][i].computed = 0;
	    AxisInfoList[AXIS_LEFT][i].computed = 0;
	    AxisInfoList[AXIS_RIGHT][i].computed = 0;
	    AxisInfoList[AXIS_TOP][i].computed = 0;
	}
    }
    if ( cName )
    {
	for ( i = 0; i < MAX_AXIS; i++)
	{
	    if ( AxisInfoList[ac_AxisSide(side)][i].component &&
	 (strcmp(AxisInfoList[ac_AxisSide(side)][i].component, cName) == 0))
		break;
	}
	if ( i < MAX_AXIS )
	{
	    msg_ELog( EF_PROBLEM, 
	    "Axis already exists for component %s", cName);
	}
	else
	{
	    for ( i = 0; i < MAX_AXIS && AxisInfoList[ac_AxisSide(side)][i].component; i++);
	    if ( i < MAX_AXIS )
		id = i;
	}
    }
    return ( id );
}

static int
ac_AxisSide(side)
char	side;
{
    int iside;
    switch ( side )
    {
	case 'b':
	    iside = AXIS_BOTTOM;
	break;
	case 't':
	    iside = AXIS_TOP;
	break;
	case 'l':
	    iside = AXIS_LEFT;
	break;
	case 'r':
	    iside = AXIS_RIGHT;
	break;
	default:
	    iside = -1;
	break;
    }
    return (iside);
}


static int
ac_AddAxis ( side, cName, dtype )
char	side;
char	*cName;
char	dtype;
{
	int id;
/*
 * Make sure the id is less than the maximum number allowed
 */
    id = ac_NewAxisId ( side, cName );
    if ( id < 0 )
    {
	msg_ELog( EF_PROBLEM, 
	    "Invalid component name for Axis Control.");
	return(0);
    }
    AxisInfoList[ac_AxisSide(side)][id].component = 
		(char*)malloc ( (strlen(cName)+1)*sizeof(char));
    strcpy ( AxisInfoList[ac_AxisSide(side)][id].component, cName );
    AxisInfoList[ac_AxisSide(side)][id].computed = 0;
    AxisInfoList[ac_AxisSide(side)][id].datatype = dtype;
    FitAxes = 1;
    return(id);
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
	    ud_format_date ( tstring, &(d1->val.t), UDF_FULL);
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
ac_ComputeAxisDescriptors(pd,c, side, datatype )
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

    /*
     *  Get parameters to use while computing the scale.
     */
    (void)ac_AxisState(pd,c,side,datatype,&min,&max );
    xy_GetAxisDescriptors( pd, c, side, datatype, &offset,
		&maxWidth, &maxHeight, &nticLabel, &ticlen, &ticInterval,
		&baseTic,&fscale, color, label );
    /*
     * Now re-compute the private axis-descriptors;
     */
    baseTic.type = min.type;
    switch ( min.type )
    {
	case 't':
	{
	    iVal = ts_GetSec(min.val.t);
	    iBase = 
     ((float)iVal)/ticInterval - (float)(int)(((float)iVal)/ticInterval) > 0.0 ?
		((long)(((float)iVal)/ticInterval) + 1) * (long)ticInterval:
		(long)(((float)iVal)/ticInterval) * (long)ticInterval;
	    lc_GetTime( &(baseTic.val.t), iBase );
	}
	break;
	case 'f':
	{
	    baseTic.val.f = 
	min.val.f/ticInterval - (float)(int)(min.val.f/ticInterval) > 0.0 ?
		(float)((int)(min.val.f/ticInterval)+1) * ticInterval :
		(float)((int)(min.val.f/ticInterval)) * ticInterval ;
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
    xy_SetPrivateAxisDescriptors(pd,c,side,datatype,
		NULL,&baseTic,&maxHeight,&maxWidth, &nticLabel,&ticInterval);
}

static int
ac_DrawAxis(pd,c,side,datatype,mode)
plot_description	pd;
char			*c;
char			side,datatype;
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
    int		nlab;
    int		maxDim = 80;
    int		labelExtent;
    int		kk;
    DataValRec	ticLoc;
    int		fit = 1;
    int		xloc,yloc;
    DataValRec	savemin, savemax;
    unsigned int	saveConfig;
    int		axisSpaceHeight;
    float	center;
    int		even;
    int		axisSpaceWidth;
    DataValRec	yOrig,xOrig;
    int		direction;
    int		nextoffset = 0;
    unsigned short xmode = 0,ymode = 0;

    (void)ac_AxisState(pd,c,side,datatype,&min,&max );
    xy_GetAxisDescriptors( pd, c, side, datatype, &offset,
		&maxWidth, &maxHeight, &maxLabel, &ticlen, &ticInterval,
		&baseTic,&fscale, color, label );
    XSetLineAttributes ( XtDisplay(Graphics), Gcontext, 0, LineSolid,
		CapButt, JoinMiter);
    SetColor(c,"axis-color",NULL,color);
	
    msg_ELog ( EF_DEBUG,"Draw Axis: component = %s side = %c datatype = %c",
		c,side,datatype);
    switch ( datatype )
    {
	case 'f':
            msg_ELog ( EF_DEBUG,"Draw Axis: component = %s min = %f max = %f",
		c,
		min.val.f, max.val.f);
	break;
	case 't':
        msg_ELog ( EF_DEBUG,"Draw Axis: component = %s min = %d %d max = %d %d",
		c,
		min.val.t.ds_yymmdd, min.val.t.ds_hhmmss,
		max.val.t.ds_yymmdd, max.val.t.ds_hhmmss);
	break;
    }
    switch (side)
    {
	case 't':
	    direction = -1;
	    axisSpaceHeight = abs((int)(GWHeight(Graphics)*AxisY1[AXIS_TOP]) -
			(int)(GWHeight(Graphics)*AxisY0[AXIS_TOP]));
	    yOrig = UY1;
	    goto horizontal;
	case 'b':
	    direction = 1;
	    axisSpaceHeight = (int)(GWHeight(Graphics)*AxisY1[AXIS_BOTTOM]) -
			(int)(GWHeight(Graphics)*AxisY0[AXIS_BOTTOM]);
	    yOrig = UY0;
horizontal:
	    xmode = mode;
	   /*
            * Set-up coordinate system.
            */
	    savemin = UX0;
	    savemax = UX1;
	    lc_SetUserCoord ( &min,&max, &UY0,&UY1);

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
	    lc_SetUserCoord ( &savemin,&savemax, &UY0,&UY1);
	break;
	case 'r':
	    direction = 1;
	    axisSpaceWidth = abs((int)(GWWidth(Graphics)*AxisX1[AXIS_RIGHT]) -
			(int)(GWWidth(Graphics)*AxisX0[AXIS_RIGHT]));
	    xOrig = UX1;
	    goto vertical;
	case 'l':
	    direction = -1;
	    axisSpaceWidth = abs((int)(GWWidth(Graphics)*AxisX1[AXIS_LEFT]) -
			(int)(GWWidth(Graphics)*AxisX0[AXIS_LEFT]));
	    xOrig = UX0;
vertical:
	    ymode = mode;
	   /*
            * Set-up coordinate system.
            */
	    savemin = UY0;
	    savemax = UY1;
	    lc_SetUserCoord ( &UX0,&UX1,&min,&max );

	   /*
            * Draw axis-line.
            */
	    XDrawLine( XtDisplay(Graphics), GWFrame(Graphics), Gcontext, 
			devX(&xOrig,xmode)+direction*(offset), devY(&min,ymode), 
			devX(&xOrig,xmode)+direction*(offset), devY(&max,ymode));

	    labelExtent = devY(&min,ymode);
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
	    nextoffset = nextoffset + maxHeight + 2;
	    lc_SetUserCoord ( &UX0,&UX1,&savemin,&savemax);
	break;

    }
    return(nextoffset);
}

int
ac_AxisState(pd,c,side,datatype,min,max )
plot_description	pd; /* input */
char			*c; /* input */
char			side; /* input */
char			datatype; /*input*/
DataValPtr		min,max;
{
    char	keyword[80];
    int		axid;
    min->type = datatype;
    max->type = datatype;
    axid = ac_AxisId (side, c);
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-min");
    switch( datatype )
    {
        case 't':
            if (axid < 0 || ! pda_Search (pd, c, keyword, NULL,
                (char *)&(min->val.t), SYMT_DATE))
            {
                min->val.t.ds_yymmdd = 0;
                min->val.t.ds_hhmmss = 0;
            }
        break;
        case 'f':
            if (axid < 0 || ! pda_Search (pd, c, keyword, NULL,
                (char *)&(min->val.f), SYMT_FLOAT))
            {
                min->val.f = 0.0;
            }
        break;
    }
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-max");
    switch( datatype )
    {
        case 't':
            if (axid < 0 || ! pda_Search (pd, c, keyword, NULL,
                (char *)&(max->val.t), SYMT_DATE))
            {
                max->val.t.ds_yymmdd = 0;
                max->val.t.ds_hhmmss = 0;
            }
        break;
        case 'f':
            if (axid < 0 || ! pda_Search (pd, c, keyword, NULL,
                (char *)&(max->val.f), SYMT_FLOAT))
            {
                max->val.f = 0.0;
            }
        break;
    }
    if ( axid < 0 )
    {
	return ( 0 );
    }
    else
    {
        return ( AxisInfoList[ac_AxisSide(side)][axid].computed );
    }
}

void
ac_UpdateAxisState(pd,c,side, datatype,scalemin, scalemax )
plot_description	pd;
char	*c;
char	side;
char	datatype;
DataValPtr	scalemin;
DataValPtr	scalemax;
{
    int axid = 0;
    char	keyword[20];
    /* 
     * If axis doesn't exist, add it to the axis-list.
     */
    if ( (axid = ac_AxisId ( side, c)) < 0 )
    {
	axid = ac_AddAxis ( side,c,datatype);
    }
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-max");
    switch( datatype )
    {
        case 't':
            pd_Store (pd,c,keyword,(char *)&(scalemax->val.f), SYMT_DATE);
        break;
        case 'f':
            pd_Store (pd,c,keyword,(char *)&(scalemax->val.f), SYMT_FLOAT);
        break;
    }
    strcpy(keyword, "private-axis-");
    keyword[13] = side;
    keyword[14] = '\0';
    strcat(keyword, "-min");
    switch( datatype )
    {
        case 't':
            pd_Store (pd,c,keyword,(char *)&(scalemin->val.f), SYMT_DATE);
        break;
        case 'f':
            pd_Store (pd,c,keyword,(char *)&(scalemin->val.f), SYMT_FLOAT);
        break;
    }
    AxisInfoList[ac_AxisSide(side)][axid].computed = 0;
}
