/*
 * Layout Control and Coordinate Transformations
 */
static char *rcsid = "$Id: LayoutControl.c,v 1.4 1992-01-29 22:28:26 barrett Exp $";
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
# include <time.h>
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
DataValRec	UX0, UX1, UY0, UY1;
float	FX0 = 0.0, FY0 = 0.0, FX1 = 1.0, FY1 = 1.0;
float	AxisX0[4],AxisX1[4],AxisY0[4],AxisY1[4];
float	IconX0,IconX1,IconY0,IconY1;
float	AnnotateX0,AnnotateX1,AnnotateY0,AnnotateY1;
float	LegendX0,LegendX1,LegendY0,LegendY1;
int	AxisSet[] = {0,0,0,0}, LegendSet = 0, IconSet = 0, AnnotateSet = 0;
TransRegion CurrentTrans = DataTrans;

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
DataValPtr xmin,xmax,ymin,ymax;
{
	UX0 = *xmin;
	UX1 = *xmax;
	UY0 = *ymin;
	UY1 = *ymax;
}

void
lc_GetTime( t, tsec )
time	*t;
time_t	tsec;
{
    struct tm 	*tstruct;
    tstruct = gmtime(&tsec);
    t->ds_yymmdd = tstruct->tm_year*10000 + 
		(tstruct->tm_mon+1)*100 + tstruct->tm_mday;
    t->ds_hhmmss = tstruct->tm_hour*10000 + 
		tstruct->tm_min*100 + tstruct->tm_sec;
}

void
lc_DecrData( d1,incr )
DataValPtr	d1;
double		incr;
{
    time_t	timeSec;
    switch ( d1->type )
    {
	case 't':
	    timeSec = GetSec(d1->val.t) - (long)incr;
	    lc_GetTime ( &(d1->val.t), timeSec);
	break;
	case 'i':
	    d1->val.i -= (int)incr;
	break;
	case 'f':
	    d1->val.f -= (float)incr;
	break;
	case 'd':
	    d1->val.d -= incr;
	break;
    }
}
void
lc_IncrData( d1,incr )
DataValPtr	d1;
double		incr;
{
    time_t	timeSec;
    switch ( d1->type )
    {
	case 't':
	    timeSec = GetSec(d1->val.t) + (long)incr;
	    lc_GetTime ( &(d1->val.t), timeSec);
	break;
	case 'i':
	    d1->val.i += (int)incr;
	break;
	case 'f':
	    d1->val.f += (float)incr;
	break;
	case 'd':
	    d1->val.d += incr;
	break;
    }
}

int
lc_CompareData( d1,d2 )
DataValPtr	d1,d2;
{
    int	val =0;
    switch ( d1->type )
    {
	case 't':
	    val = GetSec(d1->val.t) - GetSec(d2->val.t);
	break;
	case 'i':
	    val = d1->val.i-d2->val.i;
	break;
	case 'f':
	    val = d1->val.f > d2->val.f ? 1 : 
	          d1->val.f < d2->val.f ? -1 : 0;
	break;
	case 'd':
	    val = d1->val.d > d2->val.d ? 1 : 
	          d1->val.d < d2->val.d ? -1 : 0;
	break;
    }
    return (val);
}

int 
devY ( user_y , mode)
DataValPtr user_y;
unsigned short mode;
{
    int dev_y = 0;
    float	uy0,uy1,uy;
    switch ( user_y->type )
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    uy0 = 0.0;
	    uy1 = (float)(GetSec(UY1.val.t) - GetSec(UY0.val.t));
	    uy = (float)(GetSec(user_y->val.t) - GetSec(UY0.val.t));
	break;
	case 'd':
	    uy0 = (float)UY0.val.d;
	    uy1 = (float)UY1.val.d;
	    uy = (float)user_y->val.d;
	break;
	case 'i':
	    uy0 = (float)UY0.val.i;
	    uy1 = (float)UY1.val.i;
	    uy = (float)user_y->val.i;
	break;
	case 'f':
	    uy0 = (float)UY0.val.f;
	    uy1 = (float)UY1.val.f;
	    uy = (float)user_y->val.f;
	break;
	default:
	    fprintf ( stdout, "\rdevY: bad coordinate type\n");
    }
    switch ( CurrentTrans )
    {
	case DataTrans:
	    if ( mode & INVERT )
	    {
		dev_y = LC_FYPIX(uy,uy0,uy1);
	    }
	    else
	    {
		dev_y = LC_YPIX(uy,uy0,uy1);
	    }
	break;
	case DeviceTrans:
	    if ( mode & INVERT )
	    {
	        dev_y = GWWidth(Graphics) - (int)uy;
	    }
	    else
	    {
	        dev_y = (int)uy;
	    }
	break;
    }
    return ( dev_y );
}

int 
devX ( user_x,mode )
DataValPtr user_x;
unsigned short mode;
{
    int dev_x = 0;
    float ux,ux0,ux1;
    switch ( user_x->type )
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    ux0 = 0.0;
	    ux1 = (float)(GetSec(UX1.val.t) - GetSec(UX0.val.t));
	    ux = (float)(GetSec(user_x->val.t) - GetSec(UX0.val.t));
	break;
	case 'd':
	    ux0 = (float)UX0.val.d;
	    ux1 = (float)UX1.val.d;
	    ux = (float)user_x->val.d;
	break;
	case 'i':
	    ux0 = (float)UX0.val.i;
	    ux1 = (float)UX1.val.i;
	    ux = (float)user_x->val.i;
	break;
	case 'f':
	    ux0 = UX0.val.f;
	    ux1 = UX1.val.f;
	    ux = user_x->val.f;
	break;
	default:
	    fprintf ( stdout, "\rdevX: bad coordinate type\n");
    }
    switch ( CurrentTrans )
    {
	case DataTrans:
	    if ( mode & INVERT )
	    {
		dev_x = LC_FXPIX(ux,ux0,ux1);
	    }
	    else
	    {
		dev_x = LC_XPIX(ux,ux0,ux1);
	    }
	break;
	case DeviceTrans:
	    if ( mode & INVERT )
	    {
	        dev_x = GWWidth(Graphics) - (int)ux;
	    }
	    else
	    {
	        dev_x = (int)ux;
	    }
	break;
    }
    return (dev_x);
}

DataValRec 
userX ( dev_x,mode )
int dev_x;
unsigned short mode;
{
    DataValRec user_x;
    float	ux0,ux1,ux;
    struct tm	*t;
    time_t	timeSec;

/*
    user_x = (DataValPtr)calloc(1,sizeof(DataValRec));
*/
    switch ( UX0.type )
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    ux0 = 0.0;
	    ux1 = (float)(GetSec(UX1.val.t) - GetSec(UX0.val.t));
	break;
	case 'f':
	    ux0 = (float)UX0.val.f;
	    ux1 = (float)UX1.val.f;
	break;
	case 'i':
	    ux0 = (float)UX0.val.i;
	    ux1 = (float)UX1.val.i;
	break;
	case 'd':
	    ux0 = (float)UX0.val.d;
	    ux1 = (float)UX1.val.d;
	break;
    }
    switch ( CurrentTrans )
    {
	case DataTrans:
	    if ( mode & INVERT )
	    {
		ux = LC_FXUSER(dev_x,ux0,ux1);
	    }
	    else
	    {
		ux = LC_XUSER(dev_x,ux0,ux1);
	    }
	break;
	case DeviceTrans:
	    if ( mode & INVERT )
	    {
	        ux = (float)(GWWidth(Graphics) - dev_x);
	    }
	    else
	    {
	        ux = (float)dev_x;
	    }
	break;
    }
    switch (user_x.type = UX0.type)
    {
	case 't': 
	    timeSec = GetSec(UX0.val.t) + (long)ux;
	    lc_GetTime ( &(user_x.val.t), timeSec);
	break;
	case 'f':
	    user_x.val.f = ux;
	break;
	case 'i':
	    user_x.val.i = (int)ux;
	break;
	case 'd':
	    user_x.val.d = (double)ux;
	break;
    }
    return (user_x);
}
DataValRec 
userY ( dev_y ,mode)
int dev_y;
unsigned short mode;
{
    DataValRec user_y;
    float	uy0,uy1,uy;
    struct tm	*t;
    time_t	timeSec;

/*
    user_y = (DataValPtr)calloc(1,sizeof(DataValRec));*/
    switch ( UY0.type )
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    uy0 = 0.0;
	    uy1 = (float)(GetSec(UY1.val.t) - GetSec(UY0.val.t));
	break;
	case 'f':
	    uy0 = (float)UY0.val.f;
	    uy1 = (float)UY1.val.f;
	break;
	case 'i':
	    uy0 = (float)UY0.val.i;
	    uy1 = (float)UY1.val.i;
	break;
	case 'd':
	    uy0 = (float)UY0.val.d;
	    uy1 = (float)UY1.val.d;
	break;
    }
    switch ( CurrentTrans )
    {
	case DataTrans:
	    if ( mode & INVERT )
	    {
		uy = LC_FYUSER(dev_y,uy0,uy1);
	    }
	    else
	    {
		uy = LC_YUSER(dev_y,uy0,uy1);
	    }
	break;
	case DeviceTrans:
	    if ( mode & INVERT )
	    {
	        uy = (float)(GWWidth(Graphics) - dev_y);
	    }
	    else
	    {
	        uy = (float)dev_y;
	    }
	break;
    }
    switch (user_y.type = UY0.type)
    {
	case 't': 
	    timeSec = GetSec(UY0.val.t) + (time_t)uy;
	    lc_GetTime ( &(user_y.val.t), timeSec);
	break;
	case 'f':
	    user_y.val.f = uy;
	break;
	case 'i':
	    user_y.val.i = (int)uy;
	break;
	case 'd':
	    user_y.val.d = (double)uy;
	break;
    }
    return (user_y);
}
