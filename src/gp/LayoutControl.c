/*
 * Layout Control and Coordinate Transformations
 */
static char *rcsid = "$Id: LayoutControl.c,v 1.8 1993-06-24 20:36:13 barrett Exp $";
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
DataValRec	UX0, UX1, UY0, UY1; /* Actual coordinates of use */
DataValRec	IUX0, IUX1, IUY0, IUY1; /* Inverted coordinates */
DataValRec	OUX0, OUX1, OUY0, OUY1; /* The ORIGINAL coordinates
				           before applying zoom transform */
/*
 *  Variables containing boundary of the the various graphics regions
 *  in the normalized coordinate system (0.0, 1.0)
 */

float	FX0 = 0.0, FY0 = 0.0, FX1 = 1.0, FY1 = 1.0;
float	AxisX0[4],AxisX1[4],AxisY0[4],AxisY1[4];
float	IconX0,IconX1,IconY0,IconY1;
int	IconSpace;
float	AnnotateX0,AnnotateX1,AnnotateY0,AnnotateY1;
float	LegendX0,LegendX1,LegendY0,LegendY1;
int	AxisSet[] = {0,0,0,0}, LegendSet = 0, IconSet = 0, AnnotateSet = 0;
TransRegion CurrentTrans = DataTrans;

typedef struct ScaleRec_ {
    float scale;  /* store scaling factore */
    struct scaleRec_ *next, *prev;
} ScaleStack;

ScaleStack *ZoomStack[4] = { NULL, NULL, NULL, NULL};
ScaleStack *ZoomBottom[4] = { NULL, NULL, NULL, NULL};
int	Zlevel = 0;

void lc_LoadZoom ( )
{
   float xmin, xmax, ymin,ymax;
   int zlev;
   if ( pd_Retrieve (Pd, "global", "zoom-level", (char *) &zlev, SYMT_INT) &&
	zlev > 0 ) {
       pd_Retrieve (Pd, "global", "zoom-xmin", (char *) &xmin, SYMT_FLOAT);
       pd_Retrieve (Pd, "global", "zoom-xmax", (char *) &xmax, SYMT_FLOAT);
       pd_Retrieve (Pd, "global", "zoom-ymin", (char *) &ymin, SYMT_FLOAT);
       pd_Retrieve (Pd, "global", "zoom-ymax", (char *) &ymax, SYMT_FLOAT);
       lc_Zoom ( xmin, xmax, ymin, ymax );
   }
}

void lc_Zoom ( xmin, xmax, ymin, ymax )
float xmin,xmax,ymin,ymax;   /* normalized coordinates of area to be "zoomed" */
{
   ScaleStack *zs = NULL;
   int		i;
   float      box[4] = { xmin, xmax, ymin, ymax };
   /*
    * Only allow 13 levels of zoom...
    */
   for ( i =0; i < 4; i++) {
      zs = (ScaleStack*)malloc (sizeof (ScaleStack));
      if ( zs ) {
         zs->next = (struct scaleRec_ *)ZoomStack[i];
         zs->prev = NULL;
         zs->scale = box[i];
         if ( ZoomStack[i] ) {
            ZoomStack[i]->prev = (struct scaleRec_ *)zs;
         } else {
            ZoomBottom[i] = zs;
         }
         ZoomStack[i] = zs;
      } else {
	   msg_ELog (EF_PROBLEM, "Couldn't allocate zoom record.");
      }
   }
   /*
    * Mark the zoom-level
    */
   Zlevel++;
   pd_Store (Pd, "global", "zoom-level", (char *) &Zlevel, SYMT_INT);
   pd_Store (Pd, "global", "zoom-xmin", (char *) &box[0], SYMT_FLOAT);
   pd_Store (Pd, "global", "zoom-xmax", (char *) &box[1], SYMT_FLOAT);
   pd_Store (Pd, "global", "zoom-ymin", (char *) &box[2], SYMT_FLOAT);
   pd_Store (Pd, "global", "zoom-ymax", (char *) &box[3], SYMT_FLOAT);
}

void lc_UnZoom (nlevel)
int nlevel;
{
   ScaleStack *zs;
   int		i;
   do {
      for ( i= 0; i < 4; i++ ) {
        if ( ZoomStack[i] ) {
          zs = ZoomStack[i];
          ZoomStack[i] = (ScaleStack*)(zs->next);
          if (ZoomStack[i]) 
             ZoomStack[i]->prev = NULL;
          else 
	     ZoomBottom[i] = NULL;
          free (zs);
        }
      }
      /*
       * Don't decrement below zero.
       */
      if (Zlevel) { 
         Zlevel--;
      } 
      nlevel--;
   }
   while ( nlevel > 0);
   
   /*
    * Mark the zoom-level info in the plot description.
    */
   pd_Store (Pd, "global", "zoom-level", (char *) &Zlevel, SYMT_INT);
   if ( Zlevel > 0 ) {
       pd_Store (Pd, "global", "zoom-xmin", 
		(char *) &(ZoomStack[0]->scale), SYMT_FLOAT);
       pd_Store (Pd, "global", "zoom-xmax", 
		(char *) &(ZoomStack[1]->scale), SYMT_FLOAT);
       pd_Store (Pd, "global", "zoom-ymin", 
		(char *) &(ZoomStack[2]->scale), SYMT_FLOAT);
       pd_Store (Pd, "global", "zoom-ymax", 
		(char *) &(ZoomStack[3]->scale), SYMT_FLOAT);
   }
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
lc_GetOrigCoord( xmin, xmax, ymin, ymax )
DataValPtr xmin,xmax,ymin,ymax;
/*
 * Return the current UNZOOMED coordinate system
 */
{
    if (xmin) *xmin = OUX0;
    if (xmax) *xmax = OUX1;
    if (ymin) *ymin = OUY0;
    if (ymax) *ymax = OUY1;
}
void 
lc_GetUserCoord( xmin, xmax, ymin, ymax, mode )
DataValPtr xmin,xmax,ymin,ymax;
/*
 * Return the current ZOOMED  coordinate system
 */
{
    if (mode & INVERT) {
        if (xmin) *xmin = IUX0;
        if (xmax) *xmax = IUX1;
        if (ymin) *ymin = IUY0;
        if (ymax) *ymax = IUY1;
    } else {
        if (xmin) *xmin = UX0;
        if (xmax) *xmax = UX1;
        if (ymin) *ymin = UY0;
        if (ymax) *ymax = UY1;
    }
}
lc_ComputeZoom( min, max, dim, mode ) 
DataValPtr min,max;
char dim;
unsigned short mode;
{
   float	fmax, fmin;
   DataValRec   umax, umin;
   float	adjmin, adjmax;
   ScaleStack   *zsMin, *zsMax;
   char		a[8];
   if ( min->type != max->type ) {
      msg_ELog( EF_PROBLEM, "ComputeZoom: min and max are not same type");
      return;
   }
   /*
    * Set up the parameters
    */
   switch ( dim ) {
      case 'x':
	fmin = FX0;
	fmax = FX1;
        zsMin = ZoomBottom[0];
        zsMax = ZoomBottom[1];
      break;
      case 'y':
	fmin = FY0;
	fmax = FY1;
        zsMin = ZoomBottom[2];
        zsMax = ZoomBottom[3];
      break;
      default:
        msg_ELog( EF_PROBLEM, "ComputeZoom: unknown dimension %c",dim);
	return;
      break;
   }
   if (mode == INVERT) 
      strcpy( a, "(invt)");
   else
      strcpy( a, "(norm)");
       
   while ( zsMin && zsMax ) {
      switch ( min->type ) {
	    case 't':
		adjmin = ((zsMin->scale-fmin) / (fmax-fmin)) *
		    (float)( max->val.t.zt_Sec - min->val.t.zt_Sec );
		adjmax = ((zsMax->scale-fmax) / (fmax-fmin)) *
		    (float)( max->val.t.zt_Sec - min->val.t.zt_Sec );
                if (mode == INVERT) {
		   max->val.t.zt_Sec -= (long)adjmin;
		   min->val.t.zt_Sec -= (long)adjmax;
                } else {
		   min->val.t.zt_Sec += (long)adjmin;
		   max->val.t.zt_Sec += (long)adjmax;
                }
	        msg_ELog (EF_DEBUG, "%szoomed-%c: min(%f) %u max(%f) %u",a,dim,
			zsMin->scale,min->val.t.zt_Sec,
			zsMax->scale,max->val.t.zt_Sec);
	    break;
	    case 'i':
		adjmin = ((zsMin->scale-fmin) / (fmax-fmin)) *
		    (float)( max->val.i - min->val.i );
		adjmax = ((zsMax->scale-fmax) / (fmax-fmin)) *
		    (float)( max->val.i - min->val.i );
                if (mode == INVERT) {
		   max->val.i -= (int)adjmin;
		   min->val.i -= (int)adjmax;
                } else {
		   min->val.i += (int)adjmin;
		   max->val.i += (int)adjmax;
                }
	        msg_ELog (EF_DEBUG, "%szoomed-%c: min(%f) %d max(%f) %d",a,dim,
			zsMin->scale,min->val.i, zsMax->scale,max->val.i);
            break;
	    case 'f':
		adjmin = ((zsMin->scale-fmin) / (fmax-fmin)) *
		    (float)( max->val.f - min->val.f );
		adjmax = ((zsMax->scale-fmax) / (fmax-fmin)) *
		    (float)( max->val.f - min->val.f );
                if ( mode == INVERT ) {
		   max->val.f -= adjmin;
		   min->val.f -= adjmax;
                } else {
		   min->val.f += adjmin;
		   max->val.f += adjmax;
                }
	        msg_ELog (EF_DEBUG, "%szoomed-%c: min(%f) %f max(%f) %f",a,dim,
			zsMin->scale,min->val.f, zsMax->scale,max->val.f);
            break;
	    case 'd':
		adjmin = ((zsMin->scale-fmin) / (fmax-fmin)) *
		    (float)( max->val.d - min->val.d );
		adjmax = ((zsMax->scale-fmax) / (fmax-fmin)) *
		    (float)( max->val.d - min->val.d );
                if ( mode == INVERT ) {
		   max->val.d -= (double)adjmin;
		   min->val.d -= (double)adjmax;
                } else {
		   min->val.d += (double)adjmin;
		   max->val.d += (double)adjmax;
                }
	        msg_ELog (EF_DEBUG, "%szoomed-%c: min(%f) %f max(%f) %f",a,dim,
			zsMin->scale,min->val.d, zsMax->scale,max->val.d);
	    break;
      }
      zsMin = (ScaleStack *)(zsMin->prev);
      zsMax = (ScaleStack *)(zsMax->prev);
  }
}

void
lc_SetUserCoord( xmin, xmax, ymin,ymax )
DataValPtr xmin,xmax,ymin,ymax;
/*
 * Set the user coordinates from the original coordinates
 * input: ORIGINAL coordinates actual min and max values
 */
{
    if ( xmin && xmax ) {
      if ( xmin->type == xmax->type ) {
        switch ( xmin->type )
        {
	    case 't':
	        msg_ELog (EF_DEBUG, "xmin %u xmax %u", 
			xmin->val.t.zt_Sec, xmax->val.t.zt_Sec);
	    break;
	    case 'i':
	        msg_ELog (EF_DEBUG, "xmin %d xmax %d", 
			xmin->val.i, xmax->val.i);
	    break;
	    case 'f':
	        msg_ELog (EF_DEBUG, "xmin %f xmax %f", 
			xmin->val.f, xmax->val.f);
	    break;
	    case 'd':
	        msg_ELog (EF_DEBUG, "xmin %f xmax %f", 
			xmin->val.d, xmax->val.d);
	    break;
	    default:
	        msg_ELog (EF_PROBLEM, 
			"Unknown type for x user coordinate system");
        	return;
	    break;
        }
	OUX0 = *xmin; OUX1 = *xmax;
	UX0 = *xmin; UX1 = *xmax;
	IUX0 = *xmin; IUX1 = *xmax;
        lc_ComputeZoom( &UX0, &UX1, 'x', 0 );
        lc_ComputeZoom( &IUX0, &IUX1, 'x', INVERT);
      } else {
	msg_ELog (EF_PROBLEM, "xmin and xmax are not the same type.");
        return;
      }
    }
    if ( ymin && ymax ) {
      if ( ymin->type == ymax->type ) {
        switch ( ymin->type )
        {
	    case 't':
	        msg_ELog (EF_DEBUG, "ymin %u ymax %u", 
			ymin->val.t.zt_Sec, ymax->val.t.zt_Sec);
	    break;
	    case 'i':
	        msg_ELog (EF_DEBUG, "ymin %d ymax %d", 
			ymin->val.i, ymax->val.i);
	    break;
	    case 'f':
	        msg_ELog (EF_DEBUG, "ymin %f ymax %f", 
			ymin->val.f, ymax->val.f);
	    break;
	    case 'd':
	        msg_ELog (EF_DEBUG, "ymin %f ymax %f", 
			ymin->val.d, ymax->val.d);
	    break;
	    default:
	        msg_ELog (EF_PROBLEM, 
			"Unknown type for y user coordinate system");
        	return;
	    break;
        }
	OUY0 = *ymin; OUY1 = *ymax;
	UY0 = *ymin; UY1 = *ymax;
	IUY0 = *ymin; IUY1 = *ymax;
        lc_ComputeZoom( &UY0, &UY1, 'y',0 );
        lc_ComputeZoom( &IUY0, &IUY1, 'y',INVERT ); 
      } else {
	msg_ELog (EF_PROBLEM, "ymin and ymax are not the same type.");
        return;
      }
    }

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
	    d1->val.t.zt_Sec -= (long)incr;
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
	    d1->val.t.zt_Sec += (long)incr;
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
	    val = d1->val.t.zt_Sec - d2->val.t.zt_Sec;
	    if ( !val ) val = d1->val.t.zt_MicroSec - d2->val.t.zt_MicroSec;
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
    DataValRec umin, umax;
    lc_GetUserCoord( NULL, NULL, &umin, &umax,  mode );
    switch ( user_y->type )
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    uy0 = 0.0;
	    uy1 = (float)(umax.val.t.zt_Sec - umin.val.t.zt_Sec);
	    uy = (float)(user_y->val.t.zt_Sec - umin.val.t.zt_Sec);
	break;
	case 'd':
	    uy0 = (float)umin.val.d;
	    uy1 = (float)umax.val.d;
	    uy = (float)user_y->val.d;
	break;
	case 'i':
	    uy0 = (float)umin.val.i;
	    uy1 = (float)umax.val.i;
	    uy = (float)user_y->val.i;
	break;
	case 'f':
	    uy0 = (float)umin.val.f;
	    uy1 = (float)umax.val.f;
	    uy = (float)user_y->val.f;
	break;
	default:
	    fprintf ( stderr, "\rdevY: bad coordinate type: #%c#\n", user_y->type );
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
    DataValRec umin, umax;
    lc_GetUserCoord( &umin,&umax, NULL, NULL, mode );
    switch ( user_x->type )
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    ux0 = 0.0;
	    ux1 = (float)(umax.val.t.zt_Sec - umin.val.t.zt_Sec);
	    ux = (float)(user_x->val.t.zt_Sec - umin.val.t.zt_Sec);
	break;
	case 'd':
	    ux0 = (float)umin.val.d;
	    ux1 = (float)umax.val.d;
	    ux = (float)user_x->val.d;
	break;
	case 'i':
	    ux0 = (float)umin.val.i;
	    ux1 = (float)umax.val.i;
	    ux = (float)user_x->val.i;
	break;
	case 'f':
	    ux0 = umin.val.f;
	    ux1 = umax.val.f;
	    ux = user_x->val.f;
	break;
	default:
	    fprintf ( stderr, "\rdevX: bad coordinate type: #%c#\n", user_x->type );
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
    DataValRec umin, umax;
    lc_GetUserCoord( &umin,&umax, NULL, NULL, mode );

/*
    user_x = (DataValPtr)calloc(1,sizeof(DataValRec));
*/
    switch ( umin.type )
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    ux0 = 0.0;
	    ux1 = (float)(umax.val.t.zt_Sec - umin.val.t.zt_Sec);
	break;
	case 'f':
	    ux0 = (float)umin.val.f;
	    ux1 = (float)umax.val.f;
	break;
	case 'i':
	    ux0 = (float)umin.val.i;
	    ux1 = (float)umax.val.i;
	break;
	case 'd':
	    ux0 = (float)umin.val.d;
	    ux1 = (float)umax.val.d;
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
    switch (user_x.type = umin.type)
    {
	case 't': 
	    user_x.val.t.zt_Sec = umin.val.t.zt_Sec + (long)ux;
	    user_x.val.t.zt_MicroSec = umin.val.t.zt_MicroSec;
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
    DataValRec umin, umax;
    lc_GetUserCoord( NULL, NULL, &umin, &umax,  mode );

/*
    user_y = (DataValPtr)calloc(1,sizeof(DataValRec));*/
    switch ( umin.type )
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    uy0 = 0.0;
	    uy1 = (float)(umax.val.t.zt_Sec - umin.val.t.zt_Sec);
	break;
	case 'f':
	    uy0 = (float)umin.val.f;
	    uy1 = (float)umax.val.f;
	break;
	case 'i':
	    uy0 = (float)umin.val.i;
	    uy1 = (float)umax.val.i;
	break;
	case 'd':
	    uy0 = (float)umin.val.d;
	    uy1 = (float)umax.val.d;
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
    switch (user_y.type = umin.type)
    {
	case 't': 
	    user_y.val.t.zt_Sec = umin.val.t.zt_Sec + (long)uy;
	    user_y.val.t.zt_MicroSec = umin.val.t.zt_MicroSec;
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
