/*
 * Layout Control and Coordinate Transformations
 */
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
# include <X11/Intrinsic.h>

# include <config.h>
# include <defs.h>
# include <pd.h>
# include <GraphicsW.h>
# include <message.h>
# include <DataStore.h>
# include <time.h>
# include "derive.h"
# include "GraphProc.h"
# include "LayoutControl.h"

RCSID("$Id: LayoutControl.c,v 1.14 1996-11-19 07:28:52 granger Exp $")

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
 * Axis Region (s) : The axis region (s) buffer the data region and any
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
 *	 | t  |			   | t  |   n	|
 *	 |    |      data region   |  	|   d	|
 *	 | a  |			   | a	|	|
 *	 | x  |			   | x	|   r	|
 *	 | i  |			   | i	|   e	|
 *	 | s  |			   | s	|   g	|
 *	 |    |			   |	|   i	|
 *	 |------------------------------|   o	|
 *	 |    |	    bottom axis	   |	|   n	|
 *	 |    |			   |	|	|
 *       ----------------------------------------
 *	 |					|
 *	 |		icon region		|
 *	 |					|
 *       ----------------------------------------
 *
 * The transformations LC_XPIX,  LC_YPIX, LC_XUSER and LC_YUSER should
 * be used to reference the data region.  Utilities that plot in
 * each of the various component regions need to provide their own
 * boundary checking to ensure that plotting doesn't overflow the
 * space limits defined by their boundaries.
 *
 */

/*
 * Variables defining the layout of the various components.
 * and user coordinate-transformation.
 */
DataValRec	UX0, UX1, UY0, UY1; /* Actual coordinates of use */
DataValRec	OUX0, OUX1, OUY0, OUY1; /* The base (original) coordinates
				           before applying zoom transform */
/*
 *  Variables containing boundary of the various graphics regions
 *  in the normalized coordinate system (0.0, 1.0)
 */

float	FX0 = 0.0, FY0 = 0.0, FX1 = 1.0, FY1 = 1.0;
float	AxisX0[4], AxisX1[4], AxisY0[4], AxisY1[4];
float	IconX0, IconX1, IconY0, IconY1;
#ifdef notdef
int	IconSpace;
#endif
float	AnnotateX0, AnnotateX1, AnnotateY0, AnnotateY1;
float	LegendX0, LegendX1, LegendY0, LegendY1;
TransRegion CurrentTrans = DataTrans;

typedef struct ScaleRec_ {
    float scale;  /* store scaling factor */
    struct scaleRec_ *next, *prev;
} ScaleStack;

ScaleStack *ZoomStack[4] = {NULL, NULL, NULL, NULL};
ScaleStack *ZoomTail[4] = {NULL, NULL, NULL, NULL};
int	Zlevel = 0;


static void lc_ComputeZoom FP ((DataValPtr bottom, DataValPtr top, int dim));


void
lc_Init ()
/*
 * Initialize the layout variables
 */
{
/*
 * The data region takes up all the space by default
 */
	FX0 = 0.0; FY0 = 0.0; FX1 = 1.0; FY1 = 1.0;
/*
 * (No) space for each axis
 */
	AxisX0[SideLeft] = AxisX1[SideLeft] = 0.0;
	AxisY0[SideLeft] = 0.0;
	AxisY1[SideLeft] = 1.0;

	AxisX0[SideRight] = AxisX1[SideRight] = 1.0;
	AxisY0[SideRight] = 0.0;
	AxisY1[SideRight] = 1.0;

	AxisY0[SideBottom] = AxisY1[SideBottom] = 0.0;
	AxisX0[SideBottom] = 0.0;
	AxisX1[SideBottom] = 1.0;

	AxisY0[SideTop] = AxisY1[SideTop] = 1.0;
	AxisX0[SideTop] = 0.0;
	AxisX1[SideTop] = 1.0;
/*
 * (No) space icons
 */
	IconY0 = IconY1 = 0.0;
	IconX0 = 0.0;
	IconX1 = 1.0;
/*
 * (No) space for top annotation
 */
	AnnotateY0 = AnnotateY1 = 1.0;
	AnnotateX0 = 0.0;
	AnnotateX1 = 1.0;
/*
 * (No) space for right side legends
 */
	LegendX0 = LegendX1 = 1.0;
	LegendY0 = 0.0;
	LegendY1 = 1.0;
}

	

void
lc_LoadZoom ()
{
   float xmin, xmax, ymin,ymax;
   int zlev;

   if (pd_Retrieve (Pd, "global", "zoom-level", (char *) &zlev, SYMT_INT) &&
       zlev > 0) 
   {
	   pd_Retrieve (Pd, "global", "zoom-xmin", (char *) &xmin, SYMT_FLOAT);
	   pd_Retrieve (Pd, "global", "zoom-xmax", (char *) &xmax, SYMT_FLOAT);
	   pd_Retrieve (Pd, "global", "zoom-ymin", (char *) &ymin, SYMT_FLOAT);
	   pd_Retrieve (Pd, "global", "zoom-ymax", (char *) &ymax, SYMT_FLOAT);
	   lc_Zoom (xmin, xmax, ymin, ymax);
   }
}




void
lc_Zoom (xmin, xmax, ymin, ymax)
float xmin, xmax, ymin, ymax;	/* normalized coords of area to be "zoomed" */
{
   ScaleStack *zs = NULL;
   int		i;
   float      box[4];

   /*
    * Initialize the box manually since certain compilers gripe otherwise.
    */
        box[0] = xmin;
        box[1] = xmax;
        box[2] = ymin;
        box[3] = ymax;
   /*
    * Only allow 13 levels of zoom...
    */
   for (i =0; i < 4; i++) 
   {
      zs = (ScaleStack*) malloc (sizeof (ScaleStack));
      if (zs) 
      {
         zs->next = (struct scaleRec_ *) ZoomStack[i];
         zs->prev = NULL;
         zs->scale = box[i];
         if (ZoomStack[i])
            ZoomStack[i]->prev = (struct scaleRec_ *) zs;
	 else
            ZoomTail[i] = zs;

         ZoomStack[i] = zs;
      } 
      else
	   msg_ELog (EF_PROBLEM, "Couldn't allocate zoom record.");
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




int
lc_UnZoom (nlevel)
int nlevel;
{
   ScaleStack *zs;
   int		i;
/*
 * Return false if we can't unzoom any
 */
   if (Zlevel == 0)
	   return (FALSE);
/*
 * Pop the requested number of levels from the zoom stack
 */
   do 
   {
	   for (i = 0; i < 4; i++) 
	   {
		   if (ZoomStack[i]) 
		   {
			   zs = ZoomStack[i];
			   ZoomStack[i] = (ScaleStack*) (zs->next);
			   if (ZoomStack[i]) 
				   ZoomStack[i]->prev = NULL;
			   else 
				   ZoomTail[i] = NULL;
			   free (zs);
		   }
	   }
   /*
    * Don't decrement below zero.
    */
	   if (Zlevel)
		   Zlevel--;

	   nlevel--;
   } while (nlevel > 0);
   
/*
 * Mark the zoom-level info in the plot description.
 */
   pd_Store (Pd, "global", "zoom-level", (char *) &Zlevel, SYMT_INT);
   if (Zlevel > 0) 
   {
	   pd_Store (Pd, "global", "zoom-xmin", 
		     (char *) & (ZoomStack[0]->scale), SYMT_FLOAT);
	   pd_Store (Pd, "global", "zoom-xmax", 
		     (char *) & (ZoomStack[1]->scale), SYMT_FLOAT);
	   pd_Store (Pd, "global", "zoom-ymin", 
		     (char *) & (ZoomStack[2]->scale), SYMT_FLOAT);
	   pd_Store (Pd, "global", "zoom-ymax", 
		     (char *) & (ZoomStack[3]->scale), SYMT_FLOAT);
   }

   return (TRUE);
}




void
lc_SetAxisSpace (axis, pixSize)
AxisSide	axis;
int	pixSize;
/*
 * Change the space allocated for the named axis.
 */
{
/*
 * Sanity
 */
	if (pixSize < 0)
	{	
		msg_ELog (EF_PROBLEM, 
			  "Negative pixel size (%d) for %s axis space", 
			  pixSize, SIDE_NAME (axis));
		return;
	}
/*
 * Adjust the box for the appropriate axis.  The data region 
 * also gets adjusted.
 */
	switch (axis)
	{
	    case SideBottom:
		FY0 = AxisY1[axis] = AxisY0[axis] + 
			(float) pixSize / (float) GWHeight (Graphics);
		break;
	    case SideLeft:
		FX0 = AxisX1[axis] = AxisX0[axis] + 
			(float) pixSize / (float) GWWidth (Graphics);
		break;
	    case SideTop:
		FY1 = AxisY0[axis] = AxisY1[axis] - 
			(float) pixSize / (float) GWHeight (Graphics);
		break;
	    case SideRight:
		FX1 = AxisX0[axis] = AxisX1[axis] - 
			(float) pixSize / (float) GWWidth (Graphics);
		break;
	   default:
		msg_ELog (EF_PROBLEM, "axis type trashed; invalid side");
		break;
	}
}




void
lc_SetIconSpace (pixHeight)
int	pixHeight;
/*
 * Change the amount of space reserved for icons.
 */
{
	float	axisdepth;
/*
 * Sanity
 */
	if (pixHeight < 0)
	{	
		msg_ELog (EF_PROBLEM, 
			  "Negative pixel height (%d) for icon space",
			  pixHeight);
		return;
	}
/*
 * Set the depth for the icon space.
 */
	IconY1 = (float) pixHeight / (float) GWHeight (Graphics);
/*
 * Shift the bottom axis box, the bottom of the data region, and the
 * bottom of the legend space accordingly.
 */
	axisdepth = AxisY1[SideBottom] - AxisY0[SideBottom];
	AxisY0[SideBottom] = IconY1;
	AxisY1[SideBottom] = AxisY0[SideBottom] + axisdepth;

	FY0 = AxisY1[SideBottom];

	LegendY0 = IconY1;
}




void
lc_SetLegendSpace (pixWidth)
int	pixWidth;
/*
 * Set the width of the legend space on the right side of the window
 */
{
	float	axiswidth;
/*
 * Sanity
 */
	if (pixWidth < 0)
	{	
		msg_ELog (EF_PROBLEM, 
			  "Negative pixel width (%d) for legend space", 
			  pixWidth);
		return;
	}
/*
 * Adjust the left side of the legend space, which then affects the
 * right axis space and the data region.
 */
	LegendX0 = LegendX1 - (float) pixWidth / (float) GWWidth (Graphics);

	axiswidth = AxisX1[SideRight] - AxisX0[SideRight];
	AxisX1[SideRight] = LegendX0;
	AxisX0[SideRight] = AxisX1[SideRight] - axiswidth;

	FX1 = AxisX0[SideRight];
}

void
lc_SetAnnotateSpace (pixHeight)
int	pixHeight;
/*
 * Change the space allocated for top annotation.
 */
{
	float	axisdepth;
/*
 * Sanity
 */
	if (pixHeight < 0)
	{	
		msg_ELog (EF_PROBLEM, 
			  "Negative pixel height (%d) for annotatione space",
			  pixHeight);
		return;
	}
/*
 * Adjust the bottom of the annotation space, which then affects the
 * top axis space, the data region, and the legend region.
 */
	AnnotateY0 = AnnotateY1 - 
		(float) pixHeight / (float) GWWidth (Graphics);

	axisdepth = AxisY1[SideTop] - AxisY0[SideTop];
	AxisY1[SideTop] = AnnotateY0;
	AxisY0[SideTop] = AxisY1[SideTop] - axisdepth;
	
	FY1 = AxisY0[SideTop];
	LegendY1 = AnnotateY0;
}




void 
lc_GetBaseUserCoord (xleft, xright, ybottom, ytop)
DataValPtr xleft, xright, ybottom, ytop;
/*
 * Return the current UNZOOMED coordinate system
 */
{
    if (xleft) *xleft = OUX0;
    if (xright) *xright = OUX1;
    if (ybottom) *ybottom = OUY0;
    if (ytop) *ytop = OUY1;
}




void 
lc_GetUserCoord (xleft, xright, ybottom, ytop)
DataValPtr xleft, xright, ybottom, ytop;
/*
 * Return the current ZOOMED coordinate system
 */
{
        if (xleft) *xleft = UX0;
        if (xright) *xright = UX1;
        if (ybottom) *ybottom = UY0;
        if (ytop) *ytop = UY1;
}




static void
lc_ComputeZoom (bottom, top, dim)
DataValPtr	bottom, top;
int dim;
{
   float	ftop, fbottom;
   float	adjbottom, adjtop;
   ScaleStack   *zsBottom, *zsTop;

   if (bottom->type != top->type) 
   {
      msg_ELog (EF_PROBLEM, "ComputeZoom: bottom and top are not same type");
      return;
   }
   /*
    * Set up the parameters
    */
   switch (dim) {
      case 'x':
	fbottom = FX0;
	ftop = FX1;
        zsBottom = ZoomTail[0];
        zsTop = ZoomTail[1];
      break;
      case 'y':
	fbottom = FY0;
	ftop = FY1;
        zsBottom = ZoomTail[2];
        zsTop = ZoomTail[3];
      break;
      default:
        msg_ELog (EF_PROBLEM, "ComputeZoom: unknown dimension '%c'", dim);
	return;
   }

   while (zsBottom && zsTop) {
      switch (bottom->type) {
	    case 't':
		adjbottom = ((zsBottom->scale-fbottom) / (ftop-fbottom)) *
		    (float) (top->val.t.zt_Sec - bottom->val.t.zt_Sec);
		adjtop = ((zsTop->scale-ftop) / (ftop-fbottom)) *
		    (float) (top->val.t.zt_Sec - bottom->val.t.zt_Sec);

		bottom->val.t.zt_Sec += (long) adjbottom;
		top->val.t.zt_Sec += (long) adjtop;

	        msg_ELog (EF_DEBUG, "zoomed-%c: bottom (%f) %u top (%f) %u", 
			  dim, zsBottom->scale, bottom->val.t.zt_Sec, 
			  zsTop->scale, top->val.t.zt_Sec);
	    break;
	    case 'i':
		adjbottom = ((zsBottom->scale-fbottom) / (ftop-fbottom)) *
		    (float) (top->val.i - bottom->val.i);
		adjtop = ((zsTop->scale-ftop) / (ftop-fbottom)) *
		    (float) (top->val.i - bottom->val.i);

		bottom->val.i += (int) adjbottom;
		top->val.i += (int) adjtop;

	        msg_ELog (EF_DEBUG, "zoomed-%c: bottom (%f) %d top (%f) %d", 
			  dim, zsBottom->scale, bottom->val.i, 
			  zsTop->scale, top->val.i);
            break;
	    case 'f':
		adjbottom = ((zsBottom->scale-fbottom) / (ftop-fbottom)) *
		    (float) (top->val.f - bottom->val.f);
		adjtop = ((zsTop->scale-ftop) / (ftop-fbottom)) *
		    (float) (top->val.f - bottom->val.f);

		bottom->val.f += adjbottom;
		top->val.f += adjtop;

	        msg_ELog (EF_DEBUG, "zoomed-%c: bottom (%f) %f top (%f) %f", 
			  dim, zsBottom->scale, bottom->val.f, zsTop->scale, 
			  top->val.f);
            break;
	    case 'd':
		adjbottom = ((zsBottom->scale-fbottom) / (ftop-fbottom)) *
		    (float) (top->val.d - bottom->val.d);
		adjtop = ((zsTop->scale-ftop) / (ftop-fbottom)) *
		    (float) (top->val.d - bottom->val.d);

		bottom->val.d += (double) adjbottom;
		top->val.d += (double) adjtop;

	        msg_ELog (EF_DEBUG, "zoomed-%c: bottom (%f) %f top (%f) %f", 
			  dim, zsBottom->scale, bottom->val.d, zsTop->scale, 
			  top->val.d);
	    break;
      }
      zsBottom = (ScaleStack *) (zsBottom->prev);
      zsTop = (ScaleStack *) (zsTop->prev);
  }
}




void
lc_SetBaseUserCoord (xleft, xright, ybottom, ytop)
DataValPtr xleft, xright, ybottom, ytop;
/*
 * Set the base (unzoomed) user coordinates for the data region
 */
{
    if (xleft && xright) 
    {
      if (xleft->type == xright->type) 
      {
        switch (xleft->type)
        {
	    case 't':
	        msg_ELog (EF_DEBUG, "xleft %u xright %u", 
			xleft->val.t.zt_Sec, xright->val.t.zt_Sec);
	    break;
	    case 'i':
	        msg_ELog (EF_DEBUG, "xleft %d xright %d", 
			xleft->val.i, xright->val.i);
	    break;
	    case 'f':
	        msg_ELog (EF_DEBUG, "xleft %f xright %f", 
			xleft->val.f, xright->val.f);
	    break;
	    case 'd':
	        msg_ELog (EF_DEBUG, "xleft %f xright %f", 
			xleft->val.d, xright->val.d);
	    break;
	    default:
	        msg_ELog (EF_PROBLEM, 
			"Unknown type for x user coordinate system");
        	return;
        }
	OUX0 = *xleft; OUX1 = *xright;
	UX0 = *xleft; UX1 = *xright;
        lc_ComputeZoom (&UX0, &UX1, 'x');
      } 
      else 
      {
	msg_ELog (EF_PROBLEM, "xleft and xright are not the same type.");
        return;
      }
    }
    if (ybottom && ytop)
    {
      if (ybottom->type == ytop->type) 
      {
        switch (ybottom->type)
        {
	    case 't':
	        msg_ELog (EF_DEBUG, "ybottom %u ytop %u", 
			ybottom->val.t.zt_Sec, ytop->val.t.zt_Sec);
	    break;
	    case 'i':
	        msg_ELog (EF_DEBUG, "ybottom %d ytop %d", 
			ybottom->val.i, ytop->val.i);
	    break;
	    case 'f':
	        msg_ELog (EF_DEBUG, "ybottom %f ytop %f", 
			ybottom->val.f, ytop->val.f);
	    break;
	    case 'd':
	        msg_ELog (EF_DEBUG, "ybottom %f ytop %f", 
			ybottom->val.d, ytop->val.d);
	    break;
	    default:
	        msg_ELog (EF_PROBLEM, 
			"Unknown type for y user coordinate system");
        	return;
        }
	OUY0 = *ybottom; OUY1 = *ytop;
	UY0 = *ybottom; UY1 = *ytop;
        lc_ComputeZoom (&UY0, &UY1, 'y');
      } 
      else 
      {
	msg_ELog (EF_PROBLEM, "ybottom and ytop are not the same type.");
        return;
      }
    }

}




void
lc_DecrData (d1,incr)
DataValPtr	d1;
double		incr;
{
    switch (d1->type)
    {
	case 't':
	    d1->val.t.zt_Sec -= (long) incr;
	break;
	case 'i':
	    d1->val.i -= (int) incr;
	break;
	case 'f':
	    d1->val.f -= (float) incr;
	break;
	case 'd':
	    d1->val.d -= incr;
	break;
    }
}




void
lc_IncrData (d1, incr)
DataValPtr	d1;
double		incr;
{
    switch (d1->type)
    {
	case 't':
	    d1->val.t.zt_Sec += (long) incr;
	break;
	case 'i':
	    d1->val.i += (int) incr;
	break;
	case 'f':
	    d1->val.f += (float) incr;
	break;
	case 'd':
	    d1->val.d += incr;
	break;
    }
}




int
lc_CompareData (d1, d2)
DataValPtr	d1, d2;
{
    int	val = 0;
    switch (d1->type)
    {
	case 't':
	    val = d1->val.t.zt_Sec - d2->val.t.zt_Sec;
	    if (!val) val = d1->val.t.zt_MicroSec - d2->val.t.zt_MicroSec;
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
devY (user_y)
DataValPtr user_y;
{
    int		dev_y = 0;
    float	y0, y1, y;

    switch (user_y->type)
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    y0 = 0.0;
	    y1 = (float) (UY1.val.t.zt_Sec - UY0.val.t.zt_Sec);
	    y = (float) (user_y->val.t.zt_Sec - UY0.val.t.zt_Sec);
	break;
	case 'd':
	    y0 = (float) UY0.val.d;
	    y1 = (float) UY1.val.d;
	    y = (float) user_y->val.d;
	break;
	case 'i':
	    y0 = (float) UY0.val.i;
	    y1 = (float) UY1.val.i;
	    y = (float) user_y->val.i;
	break;
	case 'f':
	    y0 = (float) UY0.val.f;
	    y1 = (float) UY1.val.f;
	    y = (float) user_y->val.f;
	break;
	default:
	    msg_ELog (EF_PROBLEM, "devY: bad coordinate type: '%c'", 
		      user_y->type);
    }

    switch (CurrentTrans)
    {
	case DataTrans:
	    dev_y = LC_YPIX (y, y0, y1);
	break;
	case DeviceTrans:
	    dev_y = (int) y;
	break;
       default:
	    break;
    }

    return (dev_y);
}




int 
devX (user_x)
DataValPtr user_x;
{
    int		dev_x = 0;
    float	x, x0, x1;

    switch (user_x->type)
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    x0 = 0.0;
	    x1 = (float) (UX1.val.t.zt_Sec - UX0.val.t.zt_Sec);
	    x = (float) (user_x->val.t.zt_Sec - UX0.val.t.zt_Sec);
	break;
	case 'd':
	    x0 = (float) UX0.val.d;
	    x1 = (float) UX1.val.d;
	    x = (float) user_x->val.d;
	break;
	case 'i':
	    x0 = (float) UX0.val.i;
	    x1 = (float) UX1.val.i;
	    x = (float) user_x->val.i;
	break;
	case 'f':
	    x0 = UX0.val.f;
	    x1 = UX1.val.f;
	    x = user_x->val.f;
	break;
	default:
	    msg_ELog (EF_PROBLEM, "devX: bad coordinate type: '%c'", 
		      user_x->type);
    }

    switch (CurrentTrans)
    {
	case DataTrans:
	    dev_x = LC_XPIX (x, x0, x1);
	break;
	case DeviceTrans:
	    dev_x = (int) x;
	break;
       default:
	    break;
    }

    return (dev_x);
}




DataValRec 
userX (dev_x)
int dev_x;
{
    DataValRec	user_x;
    float	x0, x1, x;

    switch (UX0.type)
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    x0 = 0.0;
	    x1 = (float) (UX1.val.t.zt_Sec - UX0.val.t.zt_Sec);
	break;
	case 'f':
	    x0 = (float) UX0.val.f;
	    x1 = (float) UX1.val.f;
	break;
	case 'i':
	    x0 = (float) UX0.val.i;
	    x1 = (float) UX1.val.i;
	break;
	case 'd':
	    x0 = (float) UX0.val.d;
	    x1 = (float) UX1.val.d;
	break;
    }

    switch (CurrentTrans)
    {
	case DataTrans:
	    x = LC_XUSER (dev_x, x0, x1);
	break;
	case DeviceTrans:
	        x = (float) dev_x;
	break;
       default:
	    break;
    }

    switch (user_x.type = UX0.type)
    {
	case 't': 
	    user_x.val.t.zt_Sec = UX0.val.t.zt_Sec + (long) x;
	    user_x.val.t.zt_MicroSec = UX0.val.t.zt_MicroSec;
	break;
	case 'f':
	    user_x.val.f = x;
	break;
	case 'i':
	    user_x.val.i = (int) x;
	break;
	case 'd':
	    user_x.val.d = (double) x;
	break;
    }

    return (user_x);
}




DataValRec 
userY (dev_y)
int dev_y;
{
    DataValRec	user_y;
    float	uy0, uy1, uy;

    switch (UY0.type)
    {
	case 't': /* scale the time so as to minimize loss of acuracy */
	    uy0 = 0.0;
	    uy1 = (float) (UY1.val.t.zt_Sec - UY0.val.t.zt_Sec);
	break;
	case 'f':
	    uy0 = (float) UY0.val.f;
	    uy1 = (float) UY1.val.f;
	break;
	case 'i':
	    uy0 = (float) UY0.val.i;
	    uy1 = (float) UY1.val.i;
	break;
	case 'd':
	    uy0 = (float) UY0.val.d;
	    uy1 = (float) UY1.val.d;
	break;
    }

    switch (CurrentTrans)
    {
	case DataTrans:
	    uy = LC_YUSER (dev_y, uy0, uy1);
	break;
	case DeviceTrans:
	    uy = (float) dev_y;
	break;
       default:
	    break;
    }

    switch (user_y.type = UY0.type)
    {
	case 't': 
	    user_y.val.t.zt_Sec = UY0.val.t.zt_Sec + (long) uy;
	    user_y.val.t.zt_MicroSec = UY0.val.t.zt_MicroSec;
	break;
	case 'f':
	    user_y.val.f = uy;
	break;
	case 'i':
	    user_y.val.i = (int) uy;
	break;
	case 'd':
	    user_y.val.d = (double) uy;
	break;
    }

    return (user_y);
}
