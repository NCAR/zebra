/*
 * Axis control. 
 */
/*		Copyright (C) 1993 by UCAR
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
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include <DataStore.h>
# include <met_formulas.h>
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "DrawText.h"
# include "AxisControl.h"
# include "PlotPrim.h"

RCSID("$Id: AxisControl.c,v 1.32 2006-10-16 20:47:24 granger Exp $")

/*
 * Convenient scratch string
 */
static char	Scratch[200];

/*
 * How much space (in pixels) have we used on each side so far
 */
static int	SpaceUsed[NumSides];

/*
 *  Static functions -- function prototypes
 */
static void	ac_GetAxisDescriptors FP ((char*, AxisSide, int*, float*, 
					   float*, char*, char*, float*, 
					   int*));
static void	ac_FormatLabel FP ((DataValPtr, char*, char*, int tzoffset));
static void	ac_DrawAxis FP ((char*, AxisSide));
static void	ac_LabelInfo FP ((DataValPtr, DataValPtr, double, double,
				  DataValPtr, int*, int*, int tzoffset));
static double	ac_AutoTicInterval FP ((DataValPtr, DataValPtr));
static void	ac_GetLabel FP ((char *, int, char *));




void
ac_ResetAxes ()
/*
 * Reset for drawing new axes
 */
{
	AxisSide	side;
	
	for (side = (AxisSide) 0; side < NumSides; side++)
		SpaceUsed[side] = 0;
}




void
ac_PlotAxes (c)
char	*c;
/*
 * Actually plot the axes for all sides
 */
{
	AxisSide	side;
	/*
	 * Loop through the sides
	 */
	for (side = (AxisSide) 0; side < NumSides; side++)
	{
		if (ac_AxisEnabled (c, side))
			ac_DrawAxis (c, side);
	}
}

	


static void
ac_FormatLabel (v, string1, string2, tzoffset)
DataValPtr	v;
char		*string1, *string2;
int tzoffset;
/*
 * Encode the value from v into string1.  For time values, put the date into
 * string1 and the time into string2.
 */
{
    DataValRec v2 = *v;
    switch (v->type)
    {
	case 'f':
	    sprintf (string1, "%.2f", v->val.f);
	    break;
	case 'd':
	    sprintf (string1, "%.2f", (double) (v->val.d));
	    break;
	case 'i':
	    sprintf (string1, "%d", v->val.i);
	    break;
	case 't':
	    v2.val.t.zt_Sec += tzoffset*60;
	    TC_EncodeTime (&(v2.val.t), TC_DateOnly, string1);
	    TC_EncodeTime (&(v2.val.t), TC_TimeOnly, string2);
	    break;
    }
}




static void
ac_DrawAxis (c, side)
char		*c;
AxisSide	side;
/*
 * Draw the axis for the given component and side.
 */
{
    int		ticlen, maxHeight, maxWidth, edge, fit = TRUE, inverted;
    int		xloc, yloc, axisSpaceHeight, axisSpaceWidth, emult;
    int		yOrig, xOrig, direction, totalHeight;
    char	color[32], label[512], ticLabel[20], ticLabel2[20];
    float	ticInterval, fscale, drawGrid, span;
    float	red, green, blue, hue, lightness, sat;
    DataValRec	ticLoc, min, max, val0, val1;
    XColor	mainPix, gridPix;
    int tzoffset = 0;
/*
 * Get the axis drawing details from the plot description
 */
    ac_GetAxisDescriptors (c, side, &ticlen, &ticInterval, &fscale, color, 
			   label, &drawGrid, &tzoffset);

    if (!ct_GetColorByName (color, &mainPix))
    {
	msg_ELog (EF_PROBLEM, "Unknown axis color '%s', using white", color);
	ct_GetColorByName ("white", &mainPix);
    }

    ResetGC();
    XSetForeground (Disp, Gcontext, mainPix.pixel);
/*
 * Compute the RGB values for the grid color (which is just the axis color
 * modified to have a different intensity)
 */
    gridPix = mainPix;

    if (drawGrid > 0.0 && drawGrid < 1.0)
    {
	pp_RGBtoHLS ((float) mainPix.red / 65535.0, 
		     (float) mainPix.green / 65535.0,
		     (float) mainPix.blue / 65535.0, 
		     &hue, &lightness, &sat);

	pp_HLStoRGB (&red, &green, &blue, hue, lightness * drawGrid, sat);

	gridPix.red = (short) (red * 65535);
	gridPix.green = (short) (green * 65535);
	gridPix.blue = (short) (blue * 65535);

	ct_GetColorByRGB (&gridPix);
    }
/*
 * Starting pixel location and user coordinate bounds
 */	
    switch (side)
    {
	case SideTop:
	    direction = -1;
            lc_GetUserCoord (&val0, &val1, NULL, NULL);
	    yOrig = (1.0 - AxisY0[SideTop]) * GWHeight (Graphics) - 
		    SpaceUsed[SideTop];
	    break;
	case SideBottom:
	    direction = 1;
            lc_GetUserCoord (&val0, &val1, NULL, NULL);
	    yOrig = (1.0 - AxisY1[SideBottom]) * GWHeight (Graphics) + 
		    SpaceUsed[SideBottom];
	    break;
	case SideRight:
	    direction = 1;
            lc_GetUserCoord (NULL, NULL, &val0, &val1);
	    xOrig = AxisX0[SideRight] * GWWidth (Graphics) + 
		    SpaceUsed[SideRight];
	    break;
	case SideLeft:
	    direction = -1;
            lc_GetUserCoord (NULL, NULL, &val0, &val1);
	    xOrig = AxisX1[SideLeft] * GWWidth (Graphics) -
		    SpaceUsed[SideLeft];
	    break;
       default:
	    msg_ELog (EF_PROBLEM, "DrawAxis: axis side corrupted");
	    break;
    }
/*
 * Find the min and max ends of the bounds
 */
    inverted = (lc_CompareData (&val0, &val1) > 0);
    emult = (inverted) ? -1 : 1;
    
    max = inverted ? val0 : val1;
    min = inverted ? val1 : val0;
/*
 * Generate auto tic interval if necessary
 */
    if (ticInterval == 0.0)
	    ticInterval = (float) ac_AutoTicInterval (&min, &max);
/*
 * Make sure we don't try to draw tics forever...
 */
    switch (max.type)
    {
	case 't':
	    span = (float)(max.val.t.zt_Sec - min.val.t.zt_Sec);
	break;
	case 'i':
	    span = (float)(max.val.i - min.val.i);
	break;
	case 'f':
	    span = (float)(max.val.f - min.val.f);
	break;
	case 'd':
	    span = (float)(max.val.d - min.val.d);
	break;
    }

    if (fabs (span / ticInterval) > 150)
    {
	    msg_ELog (EF_INFO, 
		      "ac_DrawAxis: Requested %s axis tic interval too small",
		      SIDE_NAME (side));
	    ticInterval = (float) ac_AutoTicInterval (&min, &max);
    }
/*
 * Get max label sizes and the location for the first tic
 */
    ac_LabelInfo (&min, &max, ticInterval, fscale, &ticLoc, &maxHeight, 
		  &maxWidth, tzoffset);
/*
 * For time axes, we break tic labels onto two lines, so
 * the total height is actually bigger than maxHeight (the
 * maximum height of a single label)
 */
    totalHeight = (ticLoc.type == 't') ? maxHeight * 2 + 2 : maxHeight;
/*
 * Now do the work, based on the side
 */    
    switch (side)
    {
    /*
     * Bottom or top axis
     */
	case SideBottom:
	case SideTop:
	    /*
	     * How much space for the axis?	
	     */
	    axisSpaceHeight = (int) (GWHeight (Graphics) * 
				     (AxisY1[side] - AxisY0[side]));
	    /*
	     * Draw axis line
	     */
	    XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), Gcontext,
		       devX (&min), yOrig, devX (&max), yOrig);
	    /*
	     * Set the edge pixel that the next tic label shouldn't cross
	     */
	    edge = inverted ? AxisY1[SideRight] * GWWidth (Graphics) : 0;
	    /* 
	     * Draw tic marks and tic labels
	     */
	    while (lc_CompareData (&ticLoc, &max) <= 0)
	    {
		/*
		 * Will this stuff be too tall?
		 */
		if (totalHeight + ticlen + 2 > axisSpaceHeight)
		{
		    fit = FALSE;
		    break;
		}
		/*
		 * Put a full tic and a label here if we're far enough from 
		 * the previous one
		 */
		xloc = devX (&ticLoc);

		if ((emult * (xloc - edge)) >= 0)
		{
		    /*
		     * Draw the tic
		     */
		    XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), 
			       Gcontext, xloc, yOrig, xloc, 
			       yOrig + direction * ticlen);
		    /*
		     * Draw a grid line if we're doing a grid
		     */
		    if (drawGrid > 0.0)
		    {
			XSetForeground (Disp, Gcontext, gridPix.pixel);
			XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), 
				   Gcontext, xloc,
				   (int) ((1.0 - FY0) * GWHeight (Graphics)), 
				   xloc,
				   (int) ((1.0 - FY1) * GWHeight (Graphics)));
			XSetForeground (Disp, Gcontext, mainPix.pixel);
		    }
		    /*
		     * Draw the tic label
		     */
		    ac_FormatLabel (&ticLoc, ticLabel, ticLabel2, tzoffset);

		    yloc = yOrig + direction * (ticlen + 2);

		    DrawText (Graphics, GWFrame (Graphics), Gcontext, xloc, 
			      yloc, ticLabel, 0.0, fscale, JustifyCenter, 
			      direction > 0 ? JustifyTop : JustifyBottom);
		    /*
		     * Draw the second tic label if it's a time axis
		     */
		    if (ticLoc.type == 't')
		    {
			yloc += direction * (maxHeight + 2);

			DrawText (Graphics, GWFrame (Graphics), Gcontext, xloc,
				  yloc, ticLabel2, 0.0, fscale, JustifyCenter, 
				  direction > 0 ? JustifyTop : JustifyBottom);
		    }
		    /*
		     * Move the edge defining where we allow the next tic label
		     */
		    edge = (xloc < edge) ? 
			    xloc - 5 - maxWidth : xloc + 5 + maxWidth;
	    	}
		/*
		 * Otherwise, just put in a half tic
		 */
		else
		    XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), 
			       Gcontext, xloc, yOrig, xloc, 
			       yOrig + direction * (ticlen / 2));
		/*
		 * Next tic location
		 */
		lc_IncrData (&ticLoc, (double) (ticInterval));
	    }
	    /*
	     * Keep track of how much space we've used in the axis area
	     */
	    if (fit)
		    SpaceUsed[side] += ticlen + totalHeight + 2;
	    /*
	     * Now draw the axis label, if any
	     */
	    if (fit && label[0] != '\0')
	    {
		    xloc = 0.5 * (FX1 + FX0) * GWWidth (Graphics);
		    yloc += direction * (maxHeight + 2);
		    DrawText (Graphics, GWFrame (Graphics), Gcontext, xloc, 
			      yloc, label, 0.0, fscale, JustifyCenter,
			      direction > 0 ? JustifyTop : JustifyBottom);

		    SpaceUsed[side] += maxHeight + 2;
	    }
	    /*
	     * Done with bottom or top axis
	     */
	    break;
	/*
	 * Left or right axis
	 */
	case SideLeft:
	case SideRight:
	    /*
	     * How much space for the axis?	
	     */
	    axisSpaceWidth = (int) (GWWidth (Graphics) * 
				    (AxisX1[side] - AxisX0[side]));
	    /*
	     * Draw axis line
	     */
	    XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, 
		       xOrig, devY (&min), xOrig, devY (&max));
	    /*
	     * Set the edge pixel that the next tic label shouldn't cross
	     */
	    edge = inverted ? (1.0 - AxisY1[SideTop]) * GWHeight (Graphics) :
		    (1.0 - AxisY0[SideBottom]) * GWHeight (Graphics);
	    /* 
	     * Draw tic marks and tic labels
	     */
	    while (lc_CompareData (&ticLoc, &max) <= 0)
	    {
		/*
		 * Too wide?
		 */
		if ((ticlen + maxWidth + 2) > axisSpaceWidth)
		{
		    fit = FALSE;
		    break;
		}
		/*
		 * Put a full tic and a label here if we're far enough from 
		 * the previous one
		 */
		yloc = devY (&ticLoc);

		if (abs (yloc - edge) >= totalHeight / 2)
		{
		    /*
		     * Draw the tic
		     */
		    XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), 
			       Gcontext, xOrig, yloc, 
			       xOrig + direction * ticlen, yloc);
		    /*
		     * Draw a grid line if we're doing a grid
		     */
		    if (drawGrid > 0.0)
		    {
			XSetForeground (Disp, Gcontext, gridPix.pixel);
			XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), 
				   Gcontext, (int) (FX0 * GWWidth (Graphics)),
				   yloc, (int) (FX1 * GWWidth (Graphics)),
				   yloc);
			XSetForeground (Disp, Gcontext, mainPix.pixel);
		    }
		    /*
		     * Adjust the label location a bit for times, since
		     * we'll be writing two labels
		     */
		    if (ticLoc.type == 't')
			    yloc -= maxHeight / 2 + 1;
		    /*
		     * Draw the tic label
		     */
		    ac_FormatLabel (&ticLoc, ticLabel, ticLabel2, tzoffset);

		    xloc = xOrig + direction * (ticlen + 2 + maxWidth / 2);

		    DrawText (Graphics, GWFrame (Graphics), Gcontext,
			      xloc, yloc, ticLabel, 0.0, fscale, 
			      JustifyCenter, JustifyCenter);
		    /*
		     * Draw the second tic label if it's a time axis
		     */
		    if (ticLoc.type == 't')
		    {
			yloc += maxHeight + 2;

			DrawText (Graphics, GWFrame (Graphics), Gcontext,
				  xloc, yloc, ticLabel2, 0.0, fscale,
				  JustifyCenter, JustifyCenter);
		    }
		    /*
		     * Move the edge defining where we allow the next tic label
		     */
		    edge = (yloc < edge) ? yloc - 5 - totalHeight / 2 : 
			    yloc + 5 + totalHeight / 2;
	    	}
		/*
		 * Otherwise, just put in a half tic
		 */
		else
		    XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), 
			       Gcontext, xOrig, yloc, 
			       xOrig + direction * ticlen / 2, yloc);
		/*
		 * Next tic location
		 */
		lc_IncrData (&ticLoc, (double) (ticInterval));
	    }
	    /*
	     * Keep track of how much space we've used in the axis area
	     */
	    if (fit)
		    SpaceUsed[side] += ticlen + maxWidth + 4;
	    /*
	     * Now draw the axis label, if any
	     */
	    if (fit && label[0] != '\0')
	    {
		yloc = (1.0 - 0.5 * (FY0 + FY1)) * GWHeight (Graphics);
		xloc = xOrig + direction * (ticlen + maxWidth + 2); 
		DrawText (Graphics, GWFrame (Graphics), Gcontext, xloc, yloc,
			  label, direction > 0 ? -90.0 : 90.0, fscale,
			  JustifyCenter, JustifyBottom);

		SpaceUsed[side] += maxHeight + 2;
	    }
	    /*
	     * Done with bottom or top axis
	     */
	    break;
       default:
	    break;
    }

    if (! fit)
        msg_ELog (EF_PROBLEM, "Can't fit %s axis for component %s",
		  SIDE_NAME (side), c);

    ResetGC ();
    return;
}




static void
ac_LabelInfo (min, max, step, fontscale, firstTic, maxHeight, maxWidth,
	      tzoffset)
DataValPtr	min, max, firstTic;
float		step, fontscale;
int		*maxHeight, *maxWidth;
int tzoffset;
/*
 * Given the min, max, step, and font scale, return the first tic 
 * location and the maximum width and height of the resulting tic labels.
 */
{
	DataValRec	lastTic;
	float		diff;
	int		idiff, width, height, x1, x2, y1, y2, i;
	char		label1[16], label2[16];
	/*
	 * Start by finding the location of the first and last tics
	 */
	*firstTic = *min;
	lastTic = *max;

	switch (firstTic->type)
	{
	    case 't':
		if ((idiff = firstTic->val.t.zt_Sec % (int) step) != 0)
			firstTic->val.t.zt_Sec += (int)(step - idiff);

		lastTic.val.t.zt_Sec -= lastTic.val.t.zt_Sec % (int) step;

		break;
	    case 'f':
		diff = fmod (firstTic->val.f, step);
		if (diff > 0)
			firstTic->val.f += step - diff;
		else if (diff < 0)
			firstTic->val.f -= diff;

		diff = fmod (lastTic.val.f, step);
		if (diff > 0)
			lastTic.val.f -= diff;
		else if (diff < 0)
			lastTic.val.f -= (step + diff);
		
		break;
	}
	/*
	 * Now find the maximum label sizes, making the assumption that the
	 * biggest labels will occur either at the first or the last tic.
	 */
	*maxHeight = *maxWidth = 0;

	for (i = 0; i < 2; i++)
	{
		/*
		 * Get the label(s) for the appropriate tic
		 */
		if (i == 0)
			ac_FormatLabel (firstTic, label1, label2, tzoffset);
		else
			ac_FormatLabel (&lastTic, label1, label2, tzoffset);
		/*
		 * Test on the first label
		 */
		DT_TextBox (Graphics, GWFrame (Graphics), 0, 0, label1, 0.0,
			    fontscale, JustifyLeft, JustifyTop, &x1, &y1, &x2, 
			    &y2);

		width = abs (x2 - x1);
		height = abs (y2 - y1);
		
		*maxWidth = width > *maxWidth ? width : *maxWidth;
		*maxHeight = height > *maxHeight ? height : *maxHeight;
		/*
		 * We have a second label to test for time variables
		 */
		if (firstTic->type == 't')
		{
			DT_TextBox (Graphics, GWFrame (Graphics), 0, 0, 
				    label1, 0.0, fontscale, JustifyLeft, 
				    JustifyTop, &x1, &y1, &x2, &y2);

			width = abs (x2 - x1);
			height = abs (y2 - y1);
		
			*maxWidth = width > *maxWidth ? width : *maxWidth;
			*maxHeight = height > *maxHeight ? height : *maxHeight;
		}
	}
}




int
ac_AxisEnabled (c, side)
char    *c;
AxisSide	side;
{
    zbool plot;
/*
 * By default, we put an axis on the bottom and on the left
 */
    plot = (side == SideBottom || side == SideLeft);
/*
 * Of course, we get the last word from the plot description
 */
    sprintf (Scratch, "axis-%s", SIDE_NAME (side));
    pda_Search (Pd, c, Scratch, "xy", (char *) &plot, SYMT_BOOL);

    return (plot);
}




static void
ac_GetAxisDescriptors (c, side, ticLen, ticInterval, fontScale, color, label, 
		       drawGrid, tzoffset)
char    *c;
AxisSide	side;
int     *ticLen;
float   *ticInterval, *fontScale;
char    *color, *label;
float   *drawGrid;
int *tzoffset;
/*
 * Return axis info from the plot description for the given component and side.
 */
{
	char	tstep[20], sideletter, type;
	DataValRec	val;
/*
 * Get the side identification letter for our PD parameter lookups
 */
	sideletter = SIDE_LETTER (side);
/*
 * axis color
 */
	strcpy (color, "white");
 	sprintf (Scratch, "axis-%c-color", sideletter);
	pda_Search (Pd, c, Scratch, "xy", (char *) color, SYMT_STRING);
/*
 * axis label
 */
	ac_GetLabel (c, side, label);
/*
 * tic length
 */
	*ticLen = 5;
	sprintf (Scratch, "axis-%c-tic-len", sideletter);
	pda_Search (Pd, c, Scratch, "xy", (char *) ticLen, SYMT_INT);

	if (tzoffset)
	{
	  *tzoffset = 0;
	  sprintf (Scratch, "axis-%c-timezone-offset", sideletter);
	  pda_Search (Pd, c, Scratch, "xy", (char *) tzoffset, SYMT_INT);
	}
/*
 * Find the scale type (which we need for tic interval below)
 */
	if (side == SideLeft || side == SideRight)
		lc_GetUserCoord (NULL, NULL, &val, NULL);
	else
		lc_GetUserCoord (&val, NULL, NULL, NULL);

	type = val.type;
/*
 * tic interval
 */
	sprintf (Scratch, "axis-%c-tic-interval", sideletter);

	*ticInterval = 0.0;
	if (type == 't')
	{
                if (pda_Search (Pd, c, Scratch, "xy", tstep, SYMT_STRING))
		{
		    /* pc_TimeTrigger will complain for us if the tic step
		     * is bad, and the interval will be set to zero to
		     * trigger auto intervals.
		     */
		    *ticInterval = (float) pc_TimeTrigger (tstep);
		}
	}
	else
	{
		pda_Search (Pd, c, Scratch, "xy", (char*) ticInterval,
			    SYMT_FLOAT);
	}
/*
 * font scale
 */
	*fontScale = 0.025;
	sprintf (Scratch, "axis-%c-font-scale", sideletter);
	pda_Search (Pd, c, Scratch, "xy", (char *) fontScale, SYMT_FLOAT);
/*
 * grid intensity
 */
	*drawGrid = 0.75;
	sprintf (Scratch, "axis-%c-grid-intensity", sideletter);
	pda_Search (Pd, c, Scratch, "xy", (char *) drawGrid, SYMT_FLOAT);
}




static double
ac_AutoTicInterval (min, max)
DataValPtr	min, max;
/*
 * Find a good tic interval, given the min and max
 */
{
	float	span, interval;
	int	i, tspan;
/*
 * Array of good time steps and the associated minimum span for each
 */
	static struct
	{
		int	step, minspan;
	} timeSteps[] =
	{
		{86400,	345600}, 	/* 1d, 4d */
		{43200,	172800}, 	/* 12h, 2d */
		{21600,	86400},		/* 6h, 1d */
		{7200, 	43200},		/* 2h, 12h */
		{3600, 	21600},		/* 1h, 6h */
		{1800, 	10800},		/* 30m, 3h */
		{900, 	3600}, 		/* 15m, 1h */
		{300, 	1800}, 		/* 5m, 30m */
		{120, 	600}, 		/* 2m, 10m */
		{60, 	300}, 		/* 1m, 5m */
		{30, 	180}, 		/* 30s, 3m */
		{15, 	60}, 		/* 15s, 1m */
		{5, 	30}, 		/* 5s, 30s */
		{2, 	10}, 		/* 2s, 10s */
		{1, 	0}, 		/* 1s for anything smaller than 10s */
	};
/*
 * Handle float and time limits differently
 */
	switch (min->type)
	{
	/*
	 * Float values
	 */
	    case 'f':
	    /*
	     * Find a good interval based on the span
	     */
		span = max->val.f - min->val.f;

		interval = pow (10.0, floor (log10 (fabs (span))));

		if (fabs (span / interval) < 1.5)
			interval *= 0.1;
		else if (fabs (span / interval) < 3.0)
			interval *= 0.2;
		else if (fabs (span / interval) < 8.0)
			interval *= 0.5;

		break;
	/*
	 * Time values
	 */
	    case 't':
	    /*
	     * Find the appropriate time interval from the table, based on
	     * the span between min and max
	     */
		tspan = max->val.t.zt_Sec - min->val.t.zt_Sec;
		for (i = 0; tspan < timeSteps[i].minspan; i++)
			/* nothing */;
	    /*
	     * Store the interval we found
	     */
		interval = (float) timeSteps[i].step;

		break;
	/*
	 * Uh-oh.  Can't handle any others
	 */
	    default:
		msg_ELog (EF_PROBLEM, 
			  "ac_AutoTicInterval can't handle type '%c'", 
			  min->type);
	}

	return ((double) interval);
}





static void
ac_GetLabel (c, side, label)
char *c, side, *label;
/*
 * Figure out a label for this side.
 */
{
	char keyword[16];
	FieldId fid;
/*
 * Try to find an explicit label first.
 */
	sprintf (keyword, "axis-%c-label", SIDE_LETTER (side));
        if (pda_Search (Pd, c, keyword, "xy", label, SYMT_STRING))
		return;
/*
 * Figure out what our display field is.
 */
	if (! pda_Search (Pd, c, 
			  (side == SideBottom || side == SideTop) ? "x-field" :
			  "y-field", "xy", Scratch, SYMT_STRING))
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
	if ((fid = F_Declared (Scratch)) != BadField)
	{
		/*
		 * If a units string is not already contained in the description,
		 * then append one.
		 */
		char *units = F_GetUnits (fid);
		char *desc = F_GetDesc (fid);
		if (strstr (desc, units))
			strcpy (label, desc);
		else
			sprintf (label, "%s (%s)", desc, units);
	}
	else
		strcpy (label, Scratch);
}
