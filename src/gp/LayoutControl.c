/*
 * Layout Control and Coordinate Transformations
 */
static char *rcsid = "$Id: LayoutControl.c,v 1.1 1991-10-30 19:23:42 barrett Exp $";
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
# include <X11/Intrinsic.h>
# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include "derive.h"
# include "GraphProc.h"
# include "LayoutControl.h"

/*
 * This set of routines maintains the division of the (NDC) coordinate
 * space in order to maximize the size of the data region represented
 * by the user coordinates.
 *
 * The current component layout regions that are managed are as follows:
 * Annotation Region: This region occupies the specified width and
 *	height at the top of the graphics window.
 * Legend Region: This region occpies the specified width and height
 *	at the far right of the graphics window
 * Icon Region: This region occupies the specified width and height at
 *	the bottom of the graphics window
 * Axis Region(s): The axis region(s) buffer the data region and any
 * 	other regions that may be defined.
 *
 * As each region is defined space is added or subtracted from the
 * central data region in order to maximize the space occupied by the
 * data region while still honoring the requested sized for the other
 * components.  Size for the various components is specified in pixels.
 * Specifying 0 pixels removes a component from the layout and returns
 * space to the data region.
 *
 *       ----------------------------------------
 * 	 |					|
 *	 |	annotate region			|
 *	 |					|
 *       ----------------------------------------
 *	 |    |	    top axis	   |	|	|
 *	 |    |			   |	|	|
 *       --------------------------------	|
 *	 |    |			   | r	|   l	|
 *	 | l  |			   | i	|   e	|
 *	 | e  |			   | g	|   g	|
 *	 | f  |			   | h	|   e	|
 *	 | p  |			   | t  |   n	|
 *	 |    |      data region   |  	|   d	|
 *	 | a  |			   | a	|	|
 *	 | x  |			   | x	|   r	|
 *	 | i  |			   | i	|   e	|
 *	 | s  |			   | s	|   g	|
 *	 |    |			   |	|   i	|
 *	 |------------------------------|   o	|
 *	 |    |	    bottom axis		|   n	|
 *	 |    |				|	|
 *       ----------------------------------------
 *	 |					|
 *	 |		icon region		|
 *	 |					|
 *       ----------------------------------------
 *
 * The transformations CL_XPIX,  CL_YPIX, CL_XUSER and CL_YUSER should
 * be used to reference the data region.  Utilities that plot in
 * each of the various component regions need to provide their own
 * boundary checking to ensure that plotting doesn't overflow the
 * space limits defined by their boundarys.
 *
 */

/*
 * Variables defining the layout of the various components.
 * and user coordinate-transformation.
 */
float	UX0 = 0.0, UX1 = 1.0, UY0 = 0.0, UY1 = 1.0;
float	FX0 = 0.0, FY0 = 0.0, FX1 = 1.0, FY1 = 1.0;
float	AxisX0[4],AxisX1[4],AxisY0[4],AxisY1[4];
float	IconX0,IconX1,IconY0,IconY1;
float	AnnotateX0,AnnotateX1,AnnotateY0,AnnotateY1;
float	LegendX0,LegendX1,LegendY0,LegendY1;
int	AxisSet[] = {0,0,0,0}, LegendSet = 0, IconSet = 0, AnnotateSet = 0;
TransRegion CurrentTrans = DataTrans;
int	TransConfig = 0;

void
lc_ClearTrans()
{
    FX0 = 0.0;
    FX1 = 1.0;
    FY0 = 0.0;
    FY1 = 1.0;
    lc_SetUserCoord(0.0,1.0,0.0,1.0);
    lc_SetAxisDim ( AXIS_BOTTOM, 0 );
    lc_SetAxisDim ( AXIS_TOP, 0 );
    lc_SetAxisDim ( AXIS_LEFT, 0 );
    lc_SetAxisDim ( AXIS_RIGHT, 0 );
    lc_SetAnnotateDim ( 0, 0);
    lc_SetIconDim ( 0, 0);
    lc_SetLegendDim ( 0, 0);
}

void
lc_SetAxisDim(axis,pixSize)
int	axis;
int	pixSize;
{
    if ( pixSize < 0 )
    {	
	msg_ELog (EF_PROBLEM, 
		"Pixel size (%d) for axis space is negative",pixSize);
	return;
    }
    switch (axis)
    {
        case AXIS_BOTTOM:
	    if ( AxisSet[axis] )
	    {
		FY0 = AxisY0[axis];
	    }
	    AxisX0[axis] = FX0;
	    AxisY0[axis] = FY0;
	    AxisX1[axis] = FX1;
	    AxisY1[axis] = FY0 + 
			(float)pixSize/(float)GWHeight(Graphics);
	    FY0 = AxisY1[axis];
        break;
        case AXIS_LEFT:
	    if ( AxisSet[axis] )
	    {
		FX0 = AxisX0[axis];
	    }
	    AxisX0[axis] = FX0;
	    AxisY0[axis] = FY0;
	    AxisX1[axis] = FX0 + 
			(float)pixSize/(float)GWWidth(Graphics);
	    AxisY1[axis] = FY1;
	    FX0 = AxisX1[axis];
        break;
        case AXIS_TOP:
	    if ( AxisSet[axis] )
	    {
		FY1 = AxisY1[axis];
	    }
	    AxisX0[axis] = FX0;
	    AxisY0[axis] = FY1 - 
			(float)pixSize/(float)GWHeight(Graphics);
	    AxisX1[axis] = FX1;
	    AxisY1[axis] = FY1;
	    FY1 = AxisY0[axis];
        break;
        case AXIS_RIGHT:
	    if ( AxisSet[axis] )
	    {
		FX1 = AxisX1[axis];
	    }
	    AxisX0[axis] = FX1 - 
			(float)pixSize/(float)GWWidth(Graphics);
	    AxisY0[axis] = FY0;
	    AxisX1[axis] = FX1;
	    AxisY1[axis] = FY1;
	    FX1 = AxisX0[axis];
        break;
    }
    if ( pixSize > 0 )
        AxisSet[axis] = 1;
    else
	AxisSet[axis] = 0;
}
void
lc_SetIconDim(pixWidth,pixHeight)
int	pixWidth,pixHeight;
{
    if ( pixWidth < 0 || pixHeight < 0 )
    {	
	msg_ELog (EF_PROBLEM, 
		"Pixel width (%d) or height (%d) for icon space is negative",
		pixWidth, pixHeight);
	return;
    }

    if ( IconSet && AxisSet[AXIS_BOTTOM])
    {
	AxisY1[AXIS_BOTTOM] = 0.0 + (AxisY1[AXIS_BOTTOM]-AxisY0[AXIS_BOTTOM]);
	AxisY0[AXIS_BOTTOM] = 0.0;
	FY0 = AxisY1[AXIS_BOTTOM];
    }
    else if ( IconSet );
    {
	FY0 = 0.0;
    }

    IconX0 = 0.0;
    IconY0 = 0.0;
    IconX1 = (float)pixWidth/(float)GWWidth(Graphics);
    IconY1 = (float)pixHeight/(float)GWHeight(Graphics);
    if ( AxisSet[AXIS_BOTTOM] )
    {
	AxisY1[AXIS_BOTTOM] = IconY1+(AxisY1[AXIS_BOTTOM]-AxisY0[AXIS_BOTTOM]);
	AxisY0[AXIS_BOTTOM] = IconY1;
	FY0 = AxisY1[AXIS_BOTTOM];
    }
    else
        FY0 = IconY1;

    if ( LegendSet && IconX1 > LegendX0 )
    {
	LegendY0 = IconY1;
    }
    if ( pixWidth > 0 && pixHeight > 0 )
	IconSet = 1;
    else
	IconSet = 0;
}

void
lc_SetLegendDim(pixWidth,pixHeight)
int	pixWidth;
int	pixHeight;
{
    if ( pixWidth < 0 || pixHeight < 0 )
    {	
	msg_ELog (EF_PROBLEM, 
		"Pixel width (%d) or height (%d) for legend space is negative",
		pixWidth, pixHeight);
	return;
    }

    if ( LegendSet )
    {
	FX1 = LegendX1;
    }
    LegendX0 = 1.0 - (float)pixWidth/(float)GWWidth(Graphics);
    if ( IconSet )
	LegendY0 = IconY1;
    else
	LegendY0 = 0.0;
    LegendX1 = 1.0;
    if ( AnnotateSet )
	LegendY1 = AnnotateY0;
    else
	LegendY1 = 1.0;
    FX1 = LegendX0;

    if ( pixWidth > 0 && pixHeight > 0 )
	LegendSet = 1;
    else
	LegendSet = 0;
}

void
lc_SetAnnotateDim(pixWidth,pixHeight)
int	pixWidth,pixHeight;
{
    if ( pixWidth < 0 || pixHeight < 0 )
    {	
	msg_ELog (EF_PROBLEM, 
	    "Pixel width (%d) or height (%d) for annotatione space is negative",
		pixWidth, pixHeight);
	return;
    }

    if ( AnnotateSet && AxisSet[AXIS_TOP])
    {
	AxisY0[AXIS_TOP] = 1.0 - (AxisY1[AXIS_TOP]-AxisY0[AXIS_TOP]);
	AxisY1[AXIS_TOP] = 1.0;
	FY1 = AxisY0[AXIS_TOP];
    }
    else if ( AnnotateSet )
    {
	FY1 = 1.0;
    }

    AnnotateX0 = 0.0;
    AnnotateY0 = 1.0 - (float)pixHeight/(float)GWHeight(Graphics);
    AnnotateX1 = (float)pixWidth/(float)GWWidth(Graphics);
    AnnotateY1 = 1.0;
    if ( LegendSet && AnnotateX1 > LegendX0)
    {
	LegendY1 = AnnotateY0;
    }
    if ( AxisSet[AXIS_TOP] )
    {
	AxisY0[AXIS_TOP] = AnnotateY0 - (AxisY1[AXIS_TOP]-AxisY0[AXIS_TOP]);
	AxisY1[AXIS_TOP] = AnnotateY0;
	FY1 = AxisY0[AXIS_TOP];
    }
    else
    {
	FY1 =  AnnotateY0;
    }
    if ( pixWidth > 0 || pixHeight > 0 )
	AnnotateSet = 1;
    else
	AnnotateSet = 0;
}


void
lc_SetUserCoord( xmin, xmax, ymin,ymax)
float xmin,xmax,ymin,ymax;
{
	UX0 = xmin;
	UX1 = xmax;
	UY0 = ymin;
	UY1 = ymax;
}
int 
devY ( user_y )
float user_y;
{
    int dev_y = 0;
    switch ( CurrentTrans )
    {
	case DataTrans:
	    if ( TransConfig & INVERT_Y )
	    {
		dev_y = LC_FYPIX(user_y);
	    }
	    else
	    {
		dev_y = LC_YPIX(user_y);
	    }
	break;
	case DeviceTrans:
	    if ( TransConfig & INVERT_Y )
	    {
	        dev_y = GWWidth(Graphics) - user_y;
	    }
	    else
	    {
	        dev_y = user_y;
	    }
	break;
    }
    return ( dev_y );
}

int 
devX ( user_x )
float user_x;
{
    int dev_x = 0;
    switch ( CurrentTrans )
    {
	case DataTrans:
	    if ( TransConfig & INVERT_X )
	    {
		dev_x = LC_FXPIX(user_x);
	    }
	    else
	    {
		dev_x = LC_XPIX(user_x);
	    }
	break;
	case DeviceTrans:
	    if ( TransConfig & INVERT_X )
	    {
	        dev_x = GWWidth(Graphics) - user_x;
	    }
	    else
	    {
	        dev_x = user_x;
	    }
	break;
    }
    return (dev_x);
}

float 
userX ( dev_x )
int dev_x;
{
    float user_x = 0;
    switch ( CurrentTrans )
    {
	case DataTrans:
	    if ( TransConfig & INVERT_X )
	    {
		user_x = LC_FXUSER(dev_x);
	    }
	    else
	    {
		user_x = LC_XUSER(dev_x);
	    }
	break;
	case DeviceTrans:
	    if ( TransConfig & INVERT_X )
	    {
	        user_x = GWWidth(Graphics) - dev_x;
	    }
	    else
	    {
	        user_x = dev_x;
	    }
	break;
    }
    return (user_x);
}
float 
userY ( dev_y )
int dev_y;
{
    float user_y = 0;
    switch ( CurrentTrans )
    {
	case DataTrans:
	    if ( TransConfig & INVERT_Y )
	    {
		user_y = LC_FYUSER(dev_y);
	    }
	    else
	    {
		user_y = LC_YUSER(dev_y);
	    }
	break;
	case DeviceTrans:
	    if ( TransConfig & INVERT_Y )
	    {
	        user_y = GWWidth(Graphics) - dev_y;
	    }
	    else
	    {
	        user_y = dev_y;
	    }
	break;
    }
    return (user_y);
}
