/*
 * Routines common to XY-Type plots
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
# include <time.h>
# include <message.h>
# include <DataStore.h>
# include <met_formulas.h>
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "DrawText.h"
# include "XYCommon.h"

RCSID("$Id: XYCommon.c,v 1.38 2001-06-19 23:48:31 granger Exp $")

/* 
 * One somewhat reasonable definition for infinity in XDR, lifted from 
 * NetCDF code.  This may not be right on some machines, but it's here
 * as a kluge anyway, and it won't stop things from compiling anywhere.
 */
long XDR_F_infinity = 0x7f800000;
# define XDR_F_INFINITY (*((float *)&XDR_F_infinity))

/*
 * Convenient scratch string
 */
char	Scratch[200];

/*
 * Our routines.
 */
void	xy_Init FP ((ZebTime *));

static plot_description XYPD = 0;

/*
 * Forwards.
 */
static void xy_NSpaceVector FP((DataChunk *dc, FieldId *fids, int fcount,
				int *dcveclen));
static void xy_NSpaceDimensions FP((char *comp, dsDetail *, int *ndetail));
static zbool xy_BadPoint FP((float *val, int fcount, float *badvals, int nbad));
static void xy_GetBadPoints FP((char *c, DataChunk *, float *bval, int *nbad));
static void xy_FillScalarObs FP((PlatformId pid, xyObsInfo *obsinfo, 
				 ZebTime *btime, ZebTime *etime, 
				 ZebTime *dvtimes, int npts));
static void xy_FillVectorObs FP((PlatformId pid, xyObsInfo *obsinfo, 
				 ZebTime *btime, ZebTime *etime, 
				 ZebTime *dvtimes, int npts));
static void xy_SetDataPoint FP((xyDataVector *dvectors, char *dtype, 
				int ndvec, ZebTime *when, int point,
				float *val, int *fndx));
static void xy_InitVectorData FP((xyDataVector *dvectors, char *dtype,
				  int ndvec, int npoints));
static zbool xy_DataVectorClass FP((PlatformId pid, char *c, 
				   DataOrganization *org, DataClass *class, 
				   dsDetail *details, int *ndetail));
static int xy_SetVectorTypes FP((xyDataVector *dvectors, char *dtype, 
				 int ndvec, FieldId *fids, int *fndx));



void
xy_Init (t)
ZebTime *t;
/*
 * XY Plot initialization.
 */
{
	ot_SetString ("COMPONENT      PLATFORM   FIELD       TIME\n");
}


static plot_description
xy_GetPrivatePD (char *comp)
/*
 * Setup the private pd as a copy of the global one to get all the right
 * components.  Then make sure it has the expected component in case the
 * global pd has changed since initialization.  Yes, over the lifetime of a
 * graphics process a few extra components may accumulate, but I'm not
 * worried about it.
 */
{
	if (! XYPD)
	{
	    XYPD = pd_CopyPD (Pd);
	}
	if (! pd_CompExists (XYPD, comp))
	{
	    pd_MergeComp (XYPD, comp, Pd, comp);
	}
	return XYPD;
}


void
xy_GetScaleModes (c, xauto, xinvert, yauto, yinvert)
char	*c;
zbool	*xauto, *xinvert, *yauto, *yinvert;
/*
 * Retrieve the scaling mode information from the plot-description.
 * ENTRY:
 * 	c:	the component to search in the plot-description
 * EXIT:
 *	xauto:	autoscale the x axis?
 *	xinvert:	invert the x axis?
 *	yauto:	autoscale the y axis?
 *	yinvert:	invert the y axis?
 *
 * The default is autoscaling and normal (not inverted) direction
 */
{
/*
 * Establish defaults
 */
    *xauto = *yauto = TRUE;
    *xinvert = *yinvert = FALSE;
/*
 * Look for scale mode ("manual" or "autoscale")
 */
    if (pda_Search (Pd, c, "scale-x-mode", "xy", Scratch, SYMT_STRING))
    {
	if (strcmp (Scratch, "manual") == 0)
	    *xauto = FALSE;
	else if (strcmp (Scratch, "autoscale") && strcmp (Scratch, "auto"))
    	    msg_ELog (EF_PROBLEM, 
		"Unknown x scaling mode '%s'. Using autoscaling.", Scratch);
    }

    if (pda_Search (Pd, c, "scale-y-mode", "xy", Scratch, SYMT_STRING))
    {
	if (strcmp (Scratch, "manual") == 0)
	    *yauto = FALSE;
	else if (strcmp (Scratch, "autoscale") && strcmp (Scratch, "auto"))
    	    msg_ELog (EF_PROBLEM, 
		"Unknown y scaling mode '%s'. Using autoscaling.", Scratch);
    }
/*
 * Scale style
 */
    if (pda_Search (Pd, c, "scale-x-style", "xy", Scratch, SYMT_STRING))
    {
	if (strcmp (Scratch, "invert") == 0)
	    *xinvert = TRUE;
	else
    	    msg_ELog (EF_PROBLEM, 
		"Unknown x scaling style '%s'.", Scratch);
    }

    if (pda_Search (Pd, c, "scale-y-style", "xy", Scratch, SYMT_STRING))
    {
	if (strcmp (Scratch, "invert") == 0)
	    *yinvert = TRUE;
	else
    	    msg_ELog (EF_PROBLEM, 
		"Unknown y scaling style '%s'.", Scratch);
    }
}



void
xy_GetZModes (c, zauto, invert)
char *c;
zbool *zauto, *invert;
/*
 * Figure out scaling modes for the Z axis.
 */
{
/*
 * Autoscale?
 */
	*zauto = TRUE;
	if (pda_Search (Pd, c, "scale-z-mode", "xy", Scratch, SYMT_STRING))
	{
		if (strcmp (Scratch, "manual") == 0)
			*zauto = FALSE;
		else if (strcmp (Scratch, "autoscale") != 0)
			msg_ELog (EF_PROBLEM, 
			       "Unknown z scaling mode '%s'.", Scratch);
	}
/*
 * Flip the axis?
 */
	*invert = FALSE;
	if (pda_Search (Pd, c, "scale-z-style", "xy", Scratch, SYMT_STRING))
	{
		if (strcmp (Scratch, "invert") == 0)
			*invert = TRUE;
	}
}






void
xy_SetPrivateScale (c, xmin, xmax, ymin, ymax)
char		*c;
DataValPtr	xmin, xmax, ymin, ymax;
/*
 * For a given dimension, record the current scale min and max bounds
 * for this component.
 * c - component in the plot-description to modify
 * xmin, xmax, ymin, ymax - current bounds
 */
{
    plot_description ppd;
    char	type[2];
    /*
     * x bounds
     */
    sprintf (type, "%c", xmin->type);
    ppd = xy_GetPrivatePD (c);
    pd_Store (ppd, c, "XYPrivate-x-type", type, SYMT_STRING);

    switch (xmin->type)
    {
	case 'f':
            pd_Store (ppd, c, "XYPrivate-x-max", 
		      (char *) &(xmax->val.f), SYMT_FLOAT);
            pd_Store (ppd, c, "XYPrivate-x-min",
		      (char *) &(xmin->val.f), SYMT_FLOAT);
	break;
	case 't':
            pd_Store (ppd, c, "XYPrivate-x-max",
		      (char *) &(xmax->val.t), SYMT_DATE);
            pd_Store (ppd, c, "XYPrivate-x-min",
		      (char *) &(xmin->val.t), SYMT_DATE);
	break;
    }
    /*
     * y bounds
     */
    sprintf (type, "%c", ymin->type);
    pd_Store (ppd, c, "XYPrivate-y-type", type, SYMT_STRING);

    switch (ymin->type)
    {
	case 'f':
            pd_Store (ppd, c, "XYPrivate-y-max",
		      (char *) &(ymax->val.f), SYMT_FLOAT);
            pd_Store (ppd, c, "XYPrivate-y-min",
		      (char *) &(ymin->val.f), SYMT_FLOAT);
	break;
	case 't':
            pd_Store (ppd, c, "XYPrivate-y-max",
		      (char *) &(ymax->val.t), SYMT_DATE);
            pd_Store (ppd, c, "XYPrivate-y-min",
		      (char *) &(ymin->val.t), SYMT_DATE);
	break;
    }
}




int
xy_ManualScale (c, dim, fldname, min, max)
char		*c;
char		dim;
char		*fldname;
DataValPtr	min, max;
/*
 * Get the bounds from the plot description for the given component and
 * dimension (x or y).  The field name 'fldname' is used as a parameter
 * qualifier if necessary.
 */
{
	char	keyword[20];
	char	justname[64];
	int	ok = TRUE, time;
	FieldId fid = F_Lookup (fldname);
	char *dv;
	int dt;
/*
 * Get just the name part of the field string
 */
	strcpy (justname, SimpleFieldName (fid));
/*
 * Are we dealing with time?
 */
	time = (strcmp (justname, "time") == 0);

	if (time)
		min->type = max->type = 't';
	else
		min->type = max->type = 'f';
/*
 * minimum
 */
	dt = time ? SYMT_DATE : SYMT_FLOAT;
	dv = time ? (char *) &(min->val.t) : (char *) &(min->val.f);
	sprintf (keyword, "xy-scale-%c-min", dim);
	if (pda_Search (Pd, "global", keyword, 0, dv, dt))
	{
	    msg_ELog (EF_DEBUG, "xygraph global %s overrides all components",
		      keyword);
	}
	else
	{
	    sprintf (keyword, "scale-%c-min", dim);
	    ok &= pda_ReqSearch (Pd, c, keyword, justname, dv, dt);
	}
/*
 * maximum
 */
	dv = time ? (char *) &(max->val.t) : (char *) &(max->val.f);
	sprintf (keyword, "xy-scale-%c-max", dim);
	if (pda_Search (Pd, "global", keyword, 0, dv, dt))
	{
	    msg_ELog (EF_DEBUG, "xygraph global %s overrides all components",
		      keyword);
	}
	else
	{
	    sprintf (keyword, "scale-%c-max", dim);
	    ok &= pda_ReqSearch (Pd, c, keyword, justname, dv, dt);
	}

	return (ok);
}




void
xy_GetPrivateScale (c, xmin, xmax, ymin, ymax)
char			*c;
DataValPtr		xmin, xmax, ymin, ymax;
/*
 * Find the scale bounds that were used when this component was last drawn.
 * c - the component to search
 * xmin, xmax, ymin, ymax - previous bounds
 */
{
    plot_description ppd;
    char	type[2];
    /*
     * x bounds
     */
    ppd = xy_GetPrivatePD (c);
    pda_Search (ppd, c, "XYPrivate-x-type", NULL, type, SYMT_STRING);
    xmin->type = xmax->type = type[0];

    switch (type[0])
    {
	case 'f':
	    pda_Search (ppd, c, "XYPrivate-x-max", NULL, 
			(char *) &(xmax->val.f), SYMT_FLOAT);
	    pda_Search (ppd, c, "XYPrivate-x-min", NULL, 
			(char *) &(xmin->val.f), SYMT_FLOAT);

	    break;
	case 't':
            pda_Search (ppd, c, "XYPrivate-x-max", NULL, 
			(char *) &(xmax->val.t), SYMT_DATE);
	    pda_Search (ppd, c, "XYPrivate-x-min", NULL, 
			(char *) &(xmin->val.t), SYMT_DATE);

	    break;
    }
    /*
     * y bounds
     */
    pda_Search (ppd, c, "XYPrivate-y-type", NULL, type, SYMT_STRING);
    ymin->type = ymax->type = type[0];

    switch (type[0])
    {
	case 'f':
	    pda_Search (ppd, c, "XYPrivate-y-max", NULL, 
			(char *) &(ymax->val.f), SYMT_FLOAT);
	    pda_Search (ppd, c, "XYPrivate-y-min", NULL, 
			(char *) &(ymin->val.f), SYMT_FLOAT);

	    break;
	case 't':
            pda_Search (ppd, c, "XYPrivate-y-max", NULL, 
			(char *) &(ymax->val.t), SYMT_DATE);
	    pda_Search (ppd, c, "XYPrivate-y-min", NULL, 
			(char *) &(ymin->val.t), SYMT_DATE);

	    break;
    }
}




void
xy_SaveDataTimes (c, btime, etime)
char	*c;
ZebTime	*btime, *etime;	
/*
 * Stash the current begin and end data times
 */
{
    	plot_description ppd = xy_GetPrivatePD (c);

	if (btime) 
		pd_Store (ppd, c, "XYPrivate-data-begin", (char*) btime, 
			  SYMT_DATE);

	if (etime) 
		pd_Store (ppd, c, "XYPrivate-data-end", (char*) etime, 
			  SYMT_DATE);
}




void
xy_GetTimes (c, btime, etime, bold, eold, dmode)
char	*c;
ZebTime	*btime, *etime;	
ZebTime	*bold, *eold;
int	*dmode;
/*
 * Get time information.
 * c - the component name
 * update - true if this is an update plot
 * btime, etime - (return) The target data begin and end times needed to
 *	to make the plot completely upto date.
 * bold, eold - (return) The begin and end times of data that already
 *	exists on the plot.  if (!update) bold and eold will be set
 * 	to zero.
 */
{
    int		spanSec;
/* 
 * If snap-shot mode, then use the current PLOT-TIME for begin and
 * end times.  If series mode, then compute the target begin time at
 * the appropriate span-offset from PLOT-TIME.
 */
    *etime = PlotTime;
    *btime = PlotTime;
    *dmode = DATA_SNAPSHOT;
    if (pda_Search (Pd, c, "data-mode", "xy", Scratch, SYMT_STRING))
    {
	if (strcmp (Scratch, "series") == 0)
	{
	    *dmode = DATA_SERIES;
            if (pda_Search (Pd, "global", "series-span", "xy", Scratch, 
			    SYMT_STRING))
    	    {
		if ((spanSec = pc_TimeTrigger (Scratch)) == 0)
	    	    msg_ELog (EF_PROBLEM, "Unparseable series span: %s", 
			      Scratch);

		btime->zt_Sec = etime->zt_Sec - spanSec;
    	    }
	}
    }
/*
 * Now check for the private data descriptors with the old times
 */
    bold->zt_Sec = 0;
    bold->zt_MicroSec = 0;
    eold->zt_Sec = 0;
    eold->zt_MicroSec = 0;

    {
    	plot_description ppd = xy_GetPrivatePD (c);
	pda_Search (ppd, c, "XYPrivate-data-begin", 
		    NULL, (char*) bold, SYMT_DATE);
	pda_Search (ppd, c, "XYPrivate-data-end", 
		    NULL, (char*) eold, SYMT_DATE);
    }
}




void
xy_GetPlotColors (char	*c, int	nplat, Pixel *datacolor)
/*
 * Get the color names for "ta-color" and "field-color"
 * from the plot description and return the pixel values.
 *	c:	the component to search
 *	nplat:	the number of colors expected for "field-color"
 *	datacolor:	the (returned) list of platform colors.
 * 
 * If datacolor is NULL, then that value will not be searched for.
 */
{
	char	*colorlist[MAX_PLAT];
	Pixel	white = WhitePixelOfScreen (XtScreen (Graphics));
	XColor	xc;
 	int 	ncol;
	int	i;
/*
 * Get color for data from each platform
 */
	if (datacolor)
	{
		for (i = 0; i < nplat; i++)
			datacolor[i] = white;

		if (pda_Search (Pd, c, "field-color", NULL, Scratch, 
				SYMT_STRING))
		{
			ncol = CommaParse (Scratch, colorlist);
			if (ncol < nplat)
			{
				msg_ELog (EF_INFO, 
					  "%s: Too few field-colors.", c);
			}
			else if (ncol > nplat)
				ncol = nplat;

			for (i = 0; i < ncol; i++)
			{
				if (ct_GetColorByName (colorlist[i], &xc))
					datacolor[i] = xc.pixel;
				else
					msg_ELog (EF_PROBLEM, 
						  "Can't get line color '%s'",
						  colorlist[i]);
			}
		}
		else
			msg_ELog (EF_INFO,
				  "%s: No field-color(s) specified", c);
	}
}




int
xy_AvailableData (pid, bTimeTarget, eTimeTarget, eTimeOld, bTimeReq, eTimeReq)
PlatformId	pid;
ZebTime		*bTimeTarget, *eTimeTarget, *eTimeOld;
ZebTime		*bTimeReq, *eTimeReq;
/*
 * Find out the maximum begin and end times of actual data available
 * within a requested range.
 * pid - the platform id.
 * bTimeTarget, eTimeTarget - the begin and end times of the requested
 * 	range of data.
 * eTimeOld - the ending time of data already retrieved.
 *	If no data previously exists, the time should be 000000 000000.
 * bTimeReq, eTimeReq - the begin and end times of data available to
 *	fill the target range.
 * If data is actually available, then return 1 else 0
 */
{
    int available = 1;

    *eTimeReq = *eTimeOld;
    *bTimeReq = *bTimeTarget;

    if (! ds_DataTimes (pid, eTimeTarget, 1, DsBefore, eTimeReq))
    {
	TC_EncodeTime (eTimeTarget, TC_Full, Scratch);
        msg_ELog (EF_DEBUG, "No data before %s ", Scratch);
        available = 0;
    }
    /*
     * If no data previously exists, then get the time of the oldest
     * available data within the target range.
     */
    if (eTimeOld->zt_Sec == 0)
    {
        if (! ds_DataTimes (pid, bTimeTarget, 1, DsBefore, bTimeReq) ||
	    (TC_Less (*bTimeReq, *bTimeTarget) && !TC_Eq (*bTimeTarget,
			    *eTimeTarget)))
        {
	    TC_EncodeTime (bTimeTarget, TC_Full, Scratch);
            msg_ELog (EF_DEBUG, "No data for %s at %s, checking after...", 
		      ds_PlatformName (pid), Scratch);
            if (! ds_DataTimes (pid, bTimeTarget, 1, DsAfter, bTimeReq))
            {
	        TC_EncodeTime (bTimeTarget, TC_Full, Scratch);
                msg_ELog (EF_DEBUG, "No data for %s after %s", 
			  ds_PlatformName (pid), Scratch);
                available = 0;
            }
        }
    }
    else
        *bTimeReq = *eTimeOld;

    if (available && TC_Less (*eTimeReq, *bTimeReq))
    {
        available = 0;
        TC_EncodeTime (eTimeTarget, TC_Full, Scratch);
        msg_ELog (EF_DEBUG, "No new data for %s at %s", ds_PlatformName (pid), 
		  Scratch);
    }
    return (available);
}




void
xy_GetDataMinMax (haveminmax, min, max, data, npts)
zbool   haveminmax;
DataValPtr      min, max;
DataValPtr      data;
int             npts;
/*
 * Get the min and max data value for an array of data.
 * If haveminmax is false, min and max will be reset and their returned
 * values will be the true min and max of the data set.
 * If haveminmax is true, min and max will be left as their current (passed in)
 * values and will only be changed if the values in the current data
 * set exceed them.
 */
{
   int  i;

   for (i = 0; i < npts; i++)
   {
   /*
    * Kluge test to bypass infinities in floating data.  We have some
    * files that contain them and they tend to screw up autoscaling...
    */
	if (data[i].type == 'f' && data[i].val.f == XDR_F_INFINITY)
	{
		msg_ELog (EF_DEBUG, "Ignoring infinity while autoscaling");
		continue;
	}
   /*
    * Initialize the min and max if they don't exist yet
    */
	if (! haveminmax)
	{
		*min = data[i];
		*max = data[i];
		haveminmax = TRUE;
	}
   /*
    * Update the min and max based on this point
    */
        if (lc_CompareData (data + i, max) > 0)
                        *max = data[i];
        if (lc_CompareData (data + i, min) < 0)
                        *min = data[i];
   }
}




int
xy_DetermineBounds (c, dim, min, max, autoscale, fldname, update)
char	*c;
char	dim;
DataValPtr	min, max;
zbool	autoscale;
char	*fldname;
zbool	update;
/*
 * Come up with plot bounds for the 'dim' (x or y) axis of the given
 * component.  On entry, we are given the actual data min and max for the
 * axis (or target begin and end for a time axis).  On exit, the correct
 * bounds are returned.  The return value of the function is TRUE if this
 * is an update plot and the new bounds necessitate a global replot, FALSE
 * otherwise.
 */
{
	float	span;
	char	datatype = min->type;
	int	triggerglobal;
/*
 * First try for manual bounds if they were requested
 */
	if (! autoscale && ! xy_ManualScale (c, dim, fldname, min, max))
	{
		autoscale = TRUE;
		msg_ELog (EF_INFO, "XYGraph: %s: using auto-scaling for %c", 
			  c, dim);
	}
/*
 * Autoscale bounds if requested, or if we couldn't get manual bounds.  
 */
	if (autoscale && datatype == 'f')
	{
	/*
	 * Give ourselves a 5% buffer on either side for floating
	 * point bounds.
	 */
		span = max->val.f - min->val.f;
		if (! span)
			span = 1.0;

		min->val.f -= 0.05 * span;
		max->val.f += 0.05 * span;
	}

	if (autoscale && datatype == 't' && ! update)
	{
	/*
	 * Add a 2.5% buffer to the end time on global plots to allow
	 * some space for update plots.
	 */
		max->val.t.zt_Sec += 
			(max->val.t.zt_Sec - min->val.t.zt_Sec) / 40;

		max->val.t.zt_MicroSec = 0;
	}
/* 
 * Return the previous plot bounds if this is an update plot.  Trigger a
 * global redraw if the new bounds determined above don't lie within the
 * old bounds.
 */
	if (update)
	{
		DataValRec	oldmin, oldmax, junk;

		if (dim == 'x')
			xy_GetPrivateScale (c, &oldmin, &oldmax, &junk, &junk);
		else
			xy_GetPrivateScale (c, &junk, &junk, &oldmin, &oldmax);
		
		triggerglobal = (lc_CompareData (min, &oldmin) < 0) ||
			(lc_CompareData (max, &oldmax) > 0);

		*min = oldmin;
		*max = oldmax;
	}
	else
		triggerglobal = FALSE;

	return (triggerglobal);
}



xyObsInfo *
xy_ObsAlloc (int n)
{
    xyObsInfo *obsinfo;
    int i;

    obsinfo = (xyObsInfo *) malloc (n * sizeof (xyObsInfo));
    for (i = 0; i < n; ++i)
    {
	obsinfo[i].obsndx = obsinfo[i].obsndx_static;
	obsinfo[i].nalloc = MAX_DV_OBS;
    }
    return obsinfo;
}


void
xy_ObsFree (xyObsInfo *obsinfo, int n)
{
    int i;
    for (i = 0; i < n; ++i)
    {
	if (obsinfo[i].obsndx != 0  && 
	    obsinfo[i].obsndx != obsinfo[i].obsndx_static)
	{
	    free (obsinfo[i].obsndx);
	}
    }
    free (obsinfo);
}



/*
 * Make sure the obs index of this obsinfo structure is large enough.
 */
static void
xy_ObsIndexAlloc (xyObsInfo *obsinfo, int newnobs)
{
    if (obsinfo->nalloc < newnobs)
    {
	obsinfo->nalloc += MAX_DV_OBS;
	if (obsinfo->obsndx == obsinfo->obsndx_static)
	{
	    obsinfo->obsndx = 
		(short *) malloc (obsinfo->nalloc * sizeof(short));
	    memcpy (obsinfo->obsndx, obsinfo->obsndx_static, 
		    obsinfo->nobs * sizeof(short));
	}
	else
	{
	    obsinfo->obsndx = (short *) 
		realloc (obsinfo->obsndx, obsinfo->nalloc * sizeof (short));
	}
    }
}




int
xy_GetDataVectors (pid, btime, etime, single_obs, nskip, dvectors, ndvec, 
		   obsinfo, c)
PlatformId	pid;
ZebTime		*btime, *etime;
int		single_obs, nskip;
xyDataVector	*dvectors;
int		ndvec;
xyObsInfo	*obsinfo;
char 		*c;		/* Plot description component */
/*
 * Fill in the given array of data vector structures, given the platform
 * name, begin time, end time, whether we should get data in the form of a
 * single "observation", and the length of the array.  The field name must be
 * already set in each data vector in the array.  Space for holding the
 * data in the data vectors will be malloc'ed here if necessary, and must
 * be free'd by the caller.  Only every (nskip+1)th good point will be put
 * into the resultant data vectors.
 *
 * If obsinfo is non-NULL, then an xyObsInfo structure describing the 
 * observation breakdown will be built.
 *
 * The number of points in the resultant data vectors will be returned.
 */
{
	int	i, pt, samp, f, npts, ngood, fcount, *fndx;
	int	dcpts, dcveclen, sample, ncopied, totalpts;
	float	*val, **dcvector = NULL;
	char	*dtype, stime1[32], stime2[32];
	FieldId	*fids;
	ZebTime	time, *dvtimes;
	RGrid	rg;
	DataChunk	*dc;
	DataClass	xyClass;
	DataOrganization	xyOrg;
	dsDetail details[ 2 * DC_MaxDimension ];
	int ndetail, gridpts;
	float 	badvals[20];
	int	nbadval;
	zbool	ok;
/*
 * Info message
 */
	TC_EncodeTime (btime, TC_Full, stime1);
	TC_EncodeTime (etime, TC_Full, stime2);
	msg_ELog (EF_DEBUG, "XY data vector request: '%s' from %s to %s", 
		  ds_PlatformName (pid), stime1, stime2);
/*
 * Initialize the data vectors' data pointers to NULL
 */
	for (i = 0; i < ndvec; i++)
		dvectors[i].data = NULL;
/*
 * Data types, field id's, and indices into the field id array for
 * each data vector
 */
	dtype = (char *) malloc (ndvec * sizeof (char));
	fids = (FieldId *) malloc (ndvec * sizeof (FieldId));
	fndx = (int *) malloc (ndvec * sizeof (int));

	fcount = xy_SetVectorTypes (dvectors, dtype, ndvec, fids, fndx);

	ok = FALSE;
	if (fcount == 0)
		msg_ELog (EF_PROBLEM, "XY: Only time was requested!");
/*
 * Determine the data chunk class we want based on the data 
 * organization for the platform
 */
	else if (! xy_DataVectorClass (pid, c, &xyOrg, &xyClass, 
				       details, &ndetail))
		msg_ELog (EF_PROBLEM, "XY: Can't use platform %s's data org.", 
			  ds_PlatformName (pid));
	else
	{
/*
 * Get the data chunk
 */
		dc = NULL;
		if (single_obs)
			dc = ds_FetchObs (pid, xyClass, etime, fids, fcount, 
					  details, ndetail);
		else
			dc = ds_Fetch (pid, xyClass, btime, etime, fids, 
				       fcount, details, ndetail);
		if (dc == NULL)
			msg_ELog (EF_INFO, 
			  "XY: No requested fields for '%s' between %s and %s",
			  ds_PlatformName (pid), stime1, stime2);
		else
			ok = TRUE;
	}
/*
 * Abort if any of the above failed
 */
	if (! ok)
	{
		free (dtype);
		free (fids);
		free (fndx);
		return (0);
	}

	xy_GetBadPoints (c, dc, badvals, &nbadval);

	totalpts = dcpts = dc_GetNSample (dc);
	if (xyOrg == Org1dGrid)
	{
	/*
	 * Stuff for dealing with the 1d grids (vectors) from the data chunk
	 * Figure the total length by looking at every vector.
	 */
		totalpts = 0;
		for (sample = 0; sample < dcpts; sample++)
		{
			dc_RGGeometry (dc, sample, NULL, &rg);
			/* dcveclen = rg.rg_nX; */
			totalpts += rg.rg_nX;
		}
		dcvector = (float **) malloc (fcount * sizeof (float *));
		dcpts = totalpts;
	}
	else if (xyOrg == OrgNSpace)
	{
	/*
	 * N-Space vectors
	 *
	 * FIX THIS ONE TOO:
	 * Not broken since the size of nspace dimensions cannot change
	 * between samples of the same datachunk.
	 */
		xy_NSpaceVector (dc, fids, fcount, &dcveclen);
		dcpts *= dcveclen;
		dcvector = (float **) malloc (fcount * sizeof (float *));
	}
/*
 * Allocate an array to hold a few values from the data chunk and another
 * to hold all the times that go into the data vectors
 */
	val = (float *) malloc (fcount * sizeof (float));
	dvtimes = (ZebTime *) malloc (dcpts * sizeof (ZebTime));
/*
 * Create the data arrays for each data vector structure and set data types
 */
	xy_InitVectorData (dvectors, dtype, ndvec, dcpts);
/*
 * Extract the data from the data chunk into the DataValRec arrays
 */
	sample = gridpts = -1;
	npts = ngood = ncopied = 0;

	for (pt = 0; pt < dcpts; pt++)
	{
	/*
	 * 1d grid specific stuff
	 */
		if (xyOrg == Org1dGrid)
		{
		/*
		 * Move to the next data chunk vector if necessary
		 */
			if (ncopied >= gridpts)
			{
				sample++;
				dc_GetTime (dc, sample, &time);
				dc_RGGeometry (dc, sample, NULL, &rg);
				gridpts = rg.rg_nX;
				ncopied = 0;

				for (f = 0; f < fcount; f++)
					dcvector[f] = dc_RGGetGrid (dc, sample,
							fids[f], NULL, NULL,
							NULL);
			}
		/*
		 * Grab the values for this point from the data chunk vectors
		 */
			for (f = 0; f < fcount; f++)
				val[f] = dcvector[f][ncopied];
			ncopied++;
		}
	/*
	 * n-space specific stuff
	 */
		else if (xyOrg == OrgNSpace)
		{
		/*
		 * Move to the next data chunk vector if necessary
		 */
			if (! (pt % dcveclen))
			{
				samp = pt / dcveclen;
				dc_GetTime (dc, samp, &time);

				for (f = 0; f < fcount; f++)
					dcvector[f] = dc_NSGetSample (dc,
						      samp, fids[f], NULL);
			}
		/*
		 * Grab the values for this point from the data chunk vectors
		 */
			for (f = 0; f < fcount; f++)
				val[f] = dcvector[f][pt % dcveclen];
		}
	/*
	 * scalar specifics
	 */
		else
		{
			dc_GetTime (dc, pt, &time);

			for (f = 0; f < fcount; f++)
				val[f] = dc_GetScalar (dc, pt, fids[f]);
		}
	/*
	 * Move on if any data for this point are bad or being skipped
	 */
		if (xy_BadPoint (val, fcount, badvals, nbadval))
			continue;
	/*
	 * We have good data, but make sure we only take every (nskip+1)th 
	 * good data point
	 */
		if ((ngood++ % (nskip + 1)) != 0)
			continue;
	/*
	 * OK, we finally have something we really want to put into the
	 * caller's data vectors.  Put the points into the DataValRec
	 * arrays and increment the point count.  Also keep track of max
	 * and min for non-time data.
	 */
		dvtimes[npts] = time;

		xy_SetDataPoint (dvectors,dtype,ndvec,&time,npts,val,fndx);
	/*
	 * Increment the point count
	 */
		npts++;
	}
/*
 * Free our local stuff
 */
	dc_DestroyDC (dc);

	free (dtype);
	free (fids);
	free (fndx);
	free (val);

	if (xyOrg == Org1dGrid || xyOrg == OrgNSpace)
		free (dcvector);
/*
 * Fill in observation info if needed
 */
	if (obsinfo)
	{
	/*
	 * If they asked for a single observation, it's still pretty simple
	 */
		if (single_obs)
		{
			/*
			 * One observation starting at index 0
			 */
			obsinfo->nobs = 1;
			obsinfo->obsndx[0] = 0;
		}
	/*
	 * Otherwise split out based on organization
	 */
		else if (xyOrg == Org1dGrid || xyOrg == OrgNSpace)
			xy_FillVectorObs (pid, obsinfo, btime, etime, 
					  dvtimes, npts);
		else	/* scalar org */
			xy_FillScalarObs (pid, obsinfo, btime, etime, 
					  dvtimes, npts);
	}

	free (dvtimes);
	return (npts);
}



static int
xy_SetVectorTypes (dvectors, dtype, ndvec, fids, fndx)
xyDataVector *dvectors;	/* Data vectors, each containing a field name	*/
char *dtype;		/* Array to hold type of each vector 		*/
int ndvec;		/* Number of data vectors			*/
FieldId *fids;		/* Field ids of non-time fiels			*/
int *fndx;	/* Array to hold mapping from vector to non-time fids	*/
/*
 * Determine the type of each data vector, setting the type in dtype[], and
 * create a mapping into non-time fields from vector index
 */
{
	int i;
	int fcount;

	fcount = 0;
	for (i = 0; i < ndvec; i++)
	{
		if (! strcmp (dvectors[i].fname, "time"))
			dtype[i] = 't';
		else
		{
			dtype[i] = 'f';
			fndx[i] = fcount;
			fids[fcount++] = F_Lookup (dvectors[i].fname);
		}
	}
	return (fcount);
}



static zbool
xy_DataVectorClass (pid, c, org, class, details, ndetail)
PlatformId pid;
char *c;
DataClass *class;
DataOrganization *org;
dsDetail *details;
int *ndetail;
/*
 * Determine the organization, class, and details for fetching data.
 * Return true on success.
 */
{
	*org = ds_PlatformDataOrg (pid);
	*ndetail = 0;
	*class = DCC_None;

	switch (*org)
	{
	   case OrgScalar:
	   case OrgFixedScalar:
		*class = DCC_Scalar;
		break;
	   case OrgNSpace:
		*class = DCC_NSpace;
	/*
	 * For n-space, some dimensions may need fixed indices.
	 */
		xy_NSpaceDimensions (c, details, ndetail);
		break;
	   case Org1dGrid:
		*class = DCC_RGrid;
		break;
	   default:
		msg_ELog (EF_PROBLEM, "xygraph: no class for org %d", *org);
		break;
	}
	return ((*class == DCC_None) ? FALSE : TRUE);
}



static void
xy_InitVectorData (dvectors, dtype, ndvec, npoints)
xyDataVector *dvectors;
char *dtype;
int ndvec;
int npoints;	/* Number of points to create space for */
{
	int i, pt;

	for (i = 0; i < ndvec; i++)
	{
		dvectors[i].data = (DataValPtr) 
			malloc (npoints * sizeof (DataValRec));

		dvectors[i].min.type = dvectors[i].max.type = dtype[i];

		for (pt = 0; pt < npoints; pt++)
			dvectors[i].data[pt].type = dtype[i];
	}
}



static void
xy_SetDataPoint (dvectors, dtype, ndvec, when, point, val, fndx)
xyDataVector	*dvectors;	/* Data vectors			*/
char		*dtype;		/* Type of each data vector	*/
int		ndvec;		/* Number of data vectors	*/
ZebTime 	*when;		/* Time of this data point	*/
int		point;		/* Point to be filled		*/
float		*val;		/* Values to store in point	*/
int		*fndx;		/* Mapping from data vector index to index
				   of field in 'val' array	*/
/*
 * Store the array of values into a point of the data vectors,
 * according to each vector's type.
 */
{
	int i;

	for (i = 0; i < ndvec; i++)
	{
	/*
	 * Floating point data
	 */
		if (dtype[i] == 'f')
		{
		/*
		 * Put the point into the data vector
		 */
			dvectors[i].data[point].val.f = val[fndx[i]];
		/*
		 * Keep track of min and max
		 */
			if (point == 0)
			{
				dvectors[i].min.val.f = val[fndx[i]];
				dvectors[i].max.val.f = val[fndx[i]];
			}
			else if (val[fndx[i]] < dvectors[i].min.val.f)
				dvectors[i].min.val.f = val[fndx[i]];
			else if (val[fndx[i]] > dvectors[i].max.val.f)
				dvectors[i].max.val.f = val[fndx[i]];
		}
	/*
	 * or time, which is simpler
	 */
		else
			dvectors[i].data[point].val.t = *when;
	}
}




static void
xy_FillVectorObs (pid, obsinfo, btime, etime, dvtimes, npts)
PlatformId 	pid;
xyObsInfo	*obsinfo;
ZebTime 	*btime;
ZebTime		*etime;
ZebTime		*dvtimes;
int		npts;
/*
 * For 1dgrid and nspace, we can assume that an "observation" is the vector
 * which spans the 1dgrid or the one-dimensional nspace variable.  This
 * means that every time change between points in the vector represents a
 * new observation.
 */
{
	int i;
	ZebTime last;

	obsinfo->obsndx[0] = 0;
	obsinfo->nobs = 1;
	last = dvtimes[0];
	for (i = 1; i < npts; ++i)
	{
		if (! TC_LessEq(dvtimes[i], last))
		{
		        xy_ObsIndexAlloc (obsinfo, obsinfo->nobs+1);
			obsinfo->obsndx[obsinfo->nobs++] = i;
			last = dvtimes[i];
		}
	}
}



static void
xy_FillScalarObs (pid, obsinfo, btime, etime, dvtimes, npts)
PlatformId 	pid;
xyObsInfo	*obsinfo;
ZebTime 	*btime;
ZebTime		*etime;
ZebTime		*dvtimes;
int		npts;
/*
 * For scalar orgs, we must rely on the file times to distinguish between
 * "observations".  
 */
{
	int ntimes;
	ZebTime obstimes[MAX_DV_OBS];
	int nobs, pt, i;

	ntimes = ds_GetObsTimes (pid, etime, obstimes, MAX_DV_OBS, NULL);

	for (nobs = 0; nobs < ntimes; nobs++)
		if (TC_LessEq (obstimes[nobs], *btime))
			break;
	nobs++;
	obsinfo->nobs = nobs;
/*
 * Now find the index for the start of each observation in the data arrays.
 * Ugly.
 */
	pt = npts - 1;

	for (i = 0; i < nobs; i++)
	{
		obsinfo->obsndx[nobs - i - 1] = pt;

		for (; pt >= 0; pt--)
		{
			if (TC_Less (dvtimes[pt], obstimes[i]))
				break;
			else
				obsinfo->obsndx[nobs - i - 1] = pt;
		}
	}
}



static void
xy_NSpaceVector (dc, fids, fcount, dcveclen)
DataChunk *dc;		/* N-Space datachunk */
FieldId *fids;
int fcount;
int *dcveclen;		/* Return the length of the n-space vectors */
/*
 * Verify that the fields wanted all have zero dimensions (simple scalar
 * case) or that they all vary along the same single dimension (all other
 * dimensions have size one).
 * Usually one of the fields is the coordinate variable for the dimension,
 * but that is not necessary.  Then return the length of the vectors, which
 * is the size of the common dimension.
 */
{
	int veclen;
	int i, j;
	char *dnames[ DC_MaxDimension ];
	unsigned long dsizes[ DC_MaxDimension ];
	int ndims;
	char *reqdim;
	int is_static;

	/*
	 * If reqdim remains NULL, it means that all of the fields should
	 * be size one---none of them may vary over any dimension.
	 */
	reqdim = NULL;
	*dcveclen = 1;
	for (i = 0; i < fcount; ++i)
	{
		dc_NSGetField(dc, fids[i], &ndims, dnames, dsizes, &is_static);
		for (j = 0; j < ndims; ++j)
		{
			if (dsizes[j] == 1)
			        continue;
			if (i == 0)
			{
				reqdim = dnames[j];
				veclen = dsizes[j];
			}
			else if (reqdim && strcmp(reqdim, dnames[j]))
			{
				msg_ELog (EF_PROBLEM, 
				  "%s: field '%s' %s '%s' %s '%s'",
				  "xy_NSpaceVector", F_GetName (fids[i]), 
				  "varies along dimension", dnames[j],
				  "instead of", reqdim);
				return ;
			}
			else if (!reqdim)
			{
				msg_ELog (EF_PROBLEM, 
				  "%s: field '%s' %s '%s' %s",
				  "xy_NSpaceVector", F_GetName(fids[i]),
				  "varies along", dnames[j], 
				  "while other fields do not");
				return ;
			}
		}
		/*
		 * If no required dimension was set above, then the
		 * rest of the fields must have either no dimensions or all
		 * dimensions of size one.
		 */
	}
	if (reqdim)
		*dcveclen = veclen;
}



static void
xy_NSpaceDimensions (c, details, ndetail)
char *c;		/* component to check for 'dimensions' parameter */
dsDetail *details;	/* array of details to fill			 */
int *ndetail;		/* return new number of details			 */
{
	if (pda_Search(Pd, c, "dimensions", NULL, Scratch, SYMT_STRING))
	{
	/*
	 * Pass on the rest of the work
	 */
		dc_NSFixedDetails (Scratch, details, ndetail);
	}
}



static zbool
xy_BadPoint (val, fcount, badvals, nbadval)
float *val;
int fcount;
float *badvals;
int nbadval;
/*
 * Return non-zero if any of the field values at this point match any
 * of the bad values
 */
{
	int f, i;

	for (f = 0; f < fcount; f++)
	{
		for (i = 0; i < nbadval; ++i)
		{
			if (val[f] == badvals[i])
				return (TRUE);
		}
	}
	return (FALSE);
}



static void
xy_GetBadPoints (c, dc, badvals, nbadval)
char *c;	/* plot component */
DataChunk *dc;
float *badvals;
int *nbadval;
/*
 * Thanks to ARM, there may be more than one bad data value, and none of them
 * may be specified in the data file.  So we have to provide plot parameters.
 */
{
	int nbad, i;

	badvals[0] = dc_GetBadval (dc);
	nbad = 1;
	if (pda_Search (Pd, c, "skip-data-points", NULL, Scratch, SYMT_STRING))
	{
		char *bv, *comma;
		bv = Scratch;
		while (bv[0] && (comma = (char *)strchr(bv, ',')) != NULL)
		{
			*comma = '\0';
			badvals[nbad++] = atof(bv);
			bv = comma + 1;
		}
		if (bv[0])
			badvals[nbad++] = atof(bv);
	}
	sprintf (Scratch, "xy vectors skipping data points:");
	for (i = 0; i < nbad; ++i)
		sprintf (Scratch+strlen(Scratch), " %f", badvals[i]);
	msg_ELog (EF_DEBUG, "%s", Scratch);
	*nbadval = nbad;
}
