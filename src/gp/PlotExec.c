/*
 * Plot execution module
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

# include <ui.h>
# include <ui_date.h>

# include <config.h>
# include <defs.h>
# include <GraphicsW.h>
# include <message.h>
# include <pd.h>
# include <DataStore.h>

# include "GC.h"
# include "GraphProc.h"
# include "DrawText.h"
# include "PixelCoord.h"
# include "EventQueue.h"
# include "LayoutControl.h"
# include "AxisControl.h"
# include "ActiveArea.h"

MAKE_RCSID ("$Id: PlotExec.c,v 2.59 2001-04-20 08:26:27 granger Exp $")

/*
 * Macro for a pointer to x cast into a char *
 */
# define CPTR(x) 	 (char *) (& (x))

/*
 * Structure for building a name to number conversion table
 */
typedef struct
{
	char	*name;
	int	number;
} name_to_num;

/*
 * Plot types and a string <--> plot type table
 */
# define PT_CAP		0
# define PT_SKEWT	1
# define PT_XSECT	2
# define PT_TSERIES	3
# define PT_XYGRAPH	4
# define PT_HISTOGRAM	5
# define PT_THETAPLOT	6
# define N_PTYPES	7	/* Increase this as plot types are added */

int TriggerGlobal = 0;		/* Update on component may trigger a global */

name_to_num Pt_table[] =
{
	{"CAP",		PT_CAP		},
	{"skewt", 	PT_SKEWT	},
	{"xsect",	PT_XSECT	},
	{"tseries",	PT_TSERIES	},
	{"xygraph",	PT_XYGRAPH	},
	{"histogram",	PT_HISTOGRAM    },
	{"thetaplot",	PT_THETAPLOT 	},
	{NULL,		0		}
};

/*
 * Plot types and a string <--> representation type table
 */
# define RT_INIT	0	/* "Initialize" representation	*/
# define RT_CONTOUR	1
# define RT_WIND	2
# define RT_RASTER	3
# define RT_TRACK	4
# define RT_OVERLAY	5
# define RT_SKEWT	6
# define RT_FCONTOUR	7
# define RT_TSERIES	8
# define RT_LIGHTNING	9
# define RT_SIMPLE	10
# define RT_OBS		11
# define RT_STATION	12	/* Station plot (not vector any more) 	*/
# define RT_BARCHART	13	/* For histograms	*/
# define RT_THETAE	14	/* Theta-e plot */
# define RT_THETAW	15	/* Theta-w plot */
# define RT_POLAR	16
# define N_RTYPES	17	/* Increase this as rep. types are added */

name_to_num Rt_table[] = 
{
	{"filled-contour",	RT_FCONTOUR	},
	{"contour",		RT_CONTOUR	},
	{"line-contour",	RT_CONTOUR	},
	{"wind",		RT_WIND		},
	{"vector",		RT_WIND		},	/* historical */
	{"station",		RT_STATION	},
	{"raster",		RT_RASTER	},
	{"track",		RT_TRACK	},
	{"overlay",		RT_OVERLAY	},
	{"lightning",		RT_LIGHTNING	},
	{"skewt",		RT_SKEWT	},
	{"tseries",		RT_TSERIES	},
	{"simple",		RT_SIMPLE	},
	{"obs",			RT_OBS		},
	{"barchart",		RT_BARCHART     },
	{"bar",			RT_BARCHART     },
	{"thetae",		RT_THETAE 	},
	{"thetaw",		RT_THETAW 	},
	{"polar",		RT_POLAR 	},
	{NULL,			0		}
};

/*
 * Two dimensional table (plot type vs. plot representation)
 * of plotting routines and a boolean to record if it's been initialized
 */
static void	 (*Plot_routines[N_PTYPES][N_RTYPES]) ();

/*
 * Our plot type
 */
static int	PlotType = -1;

/*
 * The end of plot handler, if any.
 */
static void	 (*EOPHandler) () = 0;

/*
 * Forward declarations
 */
int	px_NameToNumber FP ((char *, name_to_num *));
char *	px_NumberToName FP ((int, name_to_num *));
void	px_Init FP ((void));
void	px_AddComponent FP ((char *, int));
void	px_AdjustCoords FP ((float *, float *, float *, float *));
static int px_GetCoords FP ((void));

/*
 * To distinguish between missing capability and uncompiled capability in the
 * plot function table
 */
static void _UncompiledFunction () { }
# define UNCOMPILED_FUNCTION	 (_UncompiledFunction)

/*
 * External plot table routines
 */
# if C_PT_CAP
	void	CAP_FContour FP ((char *, int));
	void	CAP_Station FP ((char *, int));
	void	CAP_LineContour FP ((char *, int));
	void	CAP_Init FP ((ZebTime *));
#    if C_CAP_POLAR
	void	CAP_Polar FP ((char *, int));
#    endif
#    if C_CAP_VECTOR
	void	CAP_Vector FP ((char *, int));
#    endif
#    if C_CAP_RASTER
	void	CAP_Raster FP ((char *, int));
#    endif
#    if C_CAP_TRACKS
	extern void	tr_CAPTrack ();
#    endif
#    if C_CAP_OVERLAY
        extern void	ov_CAPOverlay ();
#    endif
#    if C_CAP_LIGHTNING
        extern void	li_CAPLight ();
#    endif
# endif
# if C_PT_SKEWT
	extern void	sk_Skewt ();
# endif
# if C_PT_XSECT
	extern void	xs_Init FP ((ZebTime *));
	extern void	xs_LineContour FP ((char *, int));
	extern void	xs_FilledContour FP ((char *, int));
	extern void	xs_Vector FP ((char *, int));
	extern void	xs_Raster FP ((char *, int));
	extern void	xs_Track FP ((char *, int));
# endif
# if C_PT_TSERIES
	extern void	ts_Plot ();
# endif
# if C_PT_XYGRAPH
	extern void	xy_Init ();
	extern void	xy_Graph ();
	extern void	xy_Wind ();
	extern void	xy_Contour ();
	extern void	xy_Observation ();
# endif

# if C_PT_HISTOGRAM
	extern void	HG_Init ();
	extern void	HG_CountBarChart ();
# endif

# if C_PT_THETAPLOT
	extern void	TP_ThetaPlot ();
# endif
/*
 * How many plot components in our plot description and which
 * component are we currently dealing with?
 */
static int	Ncomps;
int	Comp_index;


/*
 * Annotation stuff
 */
Pixel	White;

# ifdef notdef
/*
 * Stuff for px_GetGrid (the phony data store)
 * Macro to reference the grid two-dimensionally
 */
/* # define GRID(i,j) 	grid[ (i) * (*ydim) + (j) ] */
# define GRID(j,i) 	grid[ (i) * (*ydim) + (j) ]
static float Melev = 0.0;	/* Mean pam elevation		*/

# endif



void
px_PlotExec (component)
char	*component;
/*
 * Execute the given component of the plot description
 */
{
	char	plt[30], *info;
	Boolean	global;
	ZebTime	cachetime;
/*
 * Check now for an abort condition
 */
	if (Abort)
	{
		msg_ELog (EF_INFO, "%s %s plot aborted!",
			  TC_AscTime (&PlotTime, TC_Full), component);
		return;
	}
/*
 * Get the white pixel value
 */
	White = WhitePixelOfScreen (XtScreen (Graphics));
/*
 * Get the plot type
 */
	if (!pda_ReqSearch (Pd, "global", "plot-type", NULL, plt, SYMT_STRING))
		return;
	PlotType = px_NameToNumber (plt, Pt_table);
	if (PlotType < 0)
	{
		msg_ELog (EF_PROBLEM, "Invalid plot-type parameter: %s", plt);
		return;
	}
/*
 * Global or update plot?
 */
	cachetime = PlotTime;
	global = (strcmp (component, "global") == 0);
/*
 * Semi-kludge: roll back the plot time to the last trigger incr.
 */
	if (global && (PlotMode == RealTime))
		px_FixPlotTime (&cachetime);
/*
 * Geographical coords and layout stuff will only change for global plots.
 */
	if (global && ! px_GetCoords ())
		return;
/*
 * Set the busy cursor so that people know something is going on.
 */
	ChangeCursor (Graphics, BusyCursor);
	SetClip (TRUE);		/* Disable clipping for starters	*/
/*
 * Three possibilities:
 *
 * (1) Global plot but already in the cache.
 */
	if (global && (DisplayFrame = fc_LookupFrame (&cachetime, &info)) >= 0)
	{
	/*
	 * Set up to draw in this frame -- we may be returning to RT mode
	 * after a movie, and partial updates will need to go here.
	 */
		DrawFrame = DisplayFrame;
		GWDrawInFrame (Graphics, DrawFrame);
	/*
	 * Update the overlay times widget
	 */
		ot_SetString (info);
	}
/*
 * (2) Global plot not cached.  If you change something here, be aware that
 *     there is some awfully similar code in mc_GenNFrame that probably will
 *     have to change too.  I speak from experience.
 */
	else if (global)
	{
		ac_ResetAxes ();
		I_ClearPosIcons ();
		aa_ResetAreas ();
		Ue_ResetHighlight ();
		px_GlobalPlot (&cachetime);
		An_DoSideAnnot ();
		fc_AddFrame (&cachetime, DisplayFrame);
	}
/*
 * (3) Update plot.
 */
	else
	{
		px_AddComponent (component, True);
	/*
	 * Some updates require us to redo the whole plot.
	 */
		if (TriggerGlobal)
		{
			TriggerGlobal = FALSE;
			pc_TriggerGlobal ();
		}
	}
/*
 * Display the frame
 */
	GWDisplayFrame (Graphics, DisplayFrame);
	Ue_UnHighlight ();
/*
 * Call the end of plot handler, if there is one.
 */
	if (EOPHandler)
		 (*EOPHandler) ();
/*
 * Put the cursor back to normal.
 */
	ChangeCursor (Graphics, None);
}






void
px_GlobalPlot (cachetime)
ZebTime *cachetime;
/*
 * Perform a global update.
 */
{
	char **comps, datestring[80], rep[30];
	int i, showsteps = FALSE;
	Pixel timecolor;
	UItime temptime;
	XColor xc;
	zbool active;
/*
 * Choose the drawing frame and clear it out
 */
	DrawFrame = fc_GetFrame ();
	GWDrawInFrame (Graphics, DrawFrame);
	GWClearFrame (Graphics, DrawFrame);
	DisplayFrame = DrawFrame;
/*
 * Get the component list
 */
	comps = pd_CompList (Pd);
/*
 * Count the components
 * (7/90 jc) Don't count overlay components.  The use of this count
 * 	     currently is only to parcel out side annotation space,
 *	     and the overlays don't want it.
 */
	Ncomps = 0;
	for (i = 1; comps[i]; i++)
		if (! pd_Retrieve (Pd, comps[i], "representation",
			  rep, SYMT_STRING) || strcmp (rep, "overlay"))
			Ncomps++;
/*
 * See if they want to see each overlay as it is drawn.
 */
	if (! pda_Search (Pd, "global", "show-steps", NULL,
			 (char *) &showsteps, SYMT_BOOL))
		showsteps = FALSE;
/*
 * Annotate with the date and time
 */
	An_ResetAnnot (Ncomps);
	TC_ZtToUI (&PlotTime, &temptime);
	ud_format_date (datestring, (date *) (&temptime), UDF_FULL);
/*
 * Valid time/delta time info for model data
 */
	if (ForecastOffset)
		sprintf (datestring + strlen (datestring), " (%s)", 
			 px_ModelTimeLabel ());

	An_GetTopParams (&xc, 0);
	timecolor = xc.pixel;
	if (PlotMode == History)
	{
	/*
	 * Use "history-color" for time annotation on history mode plots
	 * (default to yellow)
	 */
		char	hcolor[30];

		if (pda_Search (Pd, "global", "history-color", NULL, hcolor, 
			SYMT_STRING))
		{
			if (! ct_GetColorByName (hcolor, &xc))
			{
				msg_ELog (EF_PROBLEM, 
					"Can't get history color '%s'", 
					hcolor);
				ct_GetColorByName ("yellow", &xc);
			}
		}
		else
			ct_GetColorByName ("yellow", &xc);

		timecolor = xc.pixel;
	}

	active = FALSE;
	pda_Search (Pd, "global", "time-annot-active", NULL,
		    (char *) &active, SYMT_BOOL);
	An_DoTopAnnot (datestring, timecolor, active ? "global" : 0,
		       active ? "time" : 0);
	An_TopAnnot ("  ");
/*
 * If they want to see the window name in the top annotation, add it now.
 */
	if (pda_Search (Pd, "global", "ta-show-name", NULL, (char *)&active,
			SYMT_BOOL) && active)
	{
		sprintf (datestring, "(%s) ", dm_WindowName());
		An_TopAnnot (datestring);
	}
/*
 * If there is an initialization routine, call it now.
 */
	if (Plot_routines[PlotType][RT_INIT])
		 (*Plot_routines[PlotType][RT_INIT]) (&PlotTime);
/*
 * Run through the plot components (start at 1 to skip the
 * global component)
 */
	for (i = 1; comps[i]; i++)
	{
		Comp_index = i;
		px_AddComponent (comps[i], False);
		if (showsteps && comps[i + 1])
			GWDisplayFrame (Graphics, DisplayFrame);
	}

# ifdef notdef	/* altitude *shouldn't* change during the plot any more */
# if C_PT_CAP
/*
 * On CAPs, annotate the altitude we eventually got.
 */
	if (PlotType == PT_CAP)
		CAP_Finish (Alt);
# endif
/*
 * If the altitude has changed, stash it.
 *
 * Only change if necessary.  This is certain to break something.
 */
	if (orig_alt < 0 && Alt != orig_alt)
	{
		pd_Store (Pd, "global", "altitude", CPTR (Alt),
			SYMT_FLOAT);
		Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
	}
# endif
}



static int
px_PixelSpace (param, space, pixels)
char *param;
float space;
int pixels;
/*
 * Retrieve this layout parameter and convert to pixels as needed.
 */
{
	pda_Search (Pd, "global", param, Pt_table[PlotType].name,
		    CPTR (space), SYMT_FLOAT);
	if (space > 1.0)
		return ((int) space);
	else
		return ((int) (space * pixels));
}



static int
px_GetCoords ()
{
	zbool expand;
	AxisSide side;
	char param[32];
	int pixel_space;
/*
 * Get CAP-oriented limits if need be.
 */
	if ((PlotType == PT_CAP || PlotType == PT_XSECT) && ! prj_Setup ())
		return (FALSE);
/*
 * Initialize plot altitude
 */
	alt_Initialize ();
/*
 * Reset window layout
 */
	lc_Init ();
# ifdef notdef
/*
 * Save the origin
 */
	if (! cvt_Origin (lat, lon))
		return (FALSE);
# endif
/*
 * Get layout parameters for use in conjunction with "LayoutControl"
 * routines
 */
	lc_SetIconSpace (px_PixelSpace ("icon-space", (float) ICONSPACE, 
					GWHeight(Graphics)));
	lc_SetLegendSpace (px_PixelSpace ("sa-space", (float) LEGENDSPACE, 
					  GWWidth(Graphics)));
	lc_SetAnnotateSpace (px_PixelSpace ("ta-space", (float) ANNOTATESPACE,
					    GWHeight(Graphics)));
	for (side = (AxisSide) 0; side < NumSides; side++)
	{
		sprintf (param, "axis-%s-space", SIDE_NAME (side));
		pixel_space = px_PixelSpace (param, 0.0, 
			     (side == SideTop || side == SideBottom) ? 
			     GWHeight(Graphics) : GWWidth(Graphics));
		lc_SetAxisSpace (side, pixel_space);
	}
/*
 * Unless told otherwise, readjust the coordinates so that x == y.
 */
	if (! pda_Search (Pd, "global", "expand", NULL, (char *) &expand,
			  SYMT_BOOL) || expand == FALSE)
		px_AdjustCoords (&Xlo, &Ylo, &Xhi, &Yhi);

	return (TRUE);
}




void
px_FixPlotTime (t)
ZebTime	*t;
/*
 * Roll back the plot time to the nearest multiple of the trigger time.
 */
{
	char	list[PlatformListLen], **comps;
	int	plats;
	char	platform[PlatformListLen];
	char 	*triggers[2*MaxPlatforms];
	int	n, i;
	int	itrigger, seconds;
	int	c;
	ZebTime	latest, avail;
	PlatformId pid;
	PlatformId fid = BadPlatform;
	int found;
/*
 * First try to roll back to the most recent global trigger time.
 */
	if (! pda_Search (Pd, "global", "trigger", 0, list, SYMT_STRING))
		return;
	n = CommaParse (list, triggers);
	seconds = 0;
	for (i = 0; i < n; ++i)
	{
		if ((itrigger = pc_TimeTrigger (triggers[i])))
		{
			if (!seconds || (itrigger < seconds))
				seconds = itrigger;
		}
	}
	if (seconds)
	{
		t->zt_Sec -= t->zt_Sec % seconds;
		msg_ELog (EF_DEBUG, "%s: %d sec @ %s",
			  "fixing plot time to time trigger",
			  seconds, TC_AscTime (t, TC_TimeOnly));
		return;
	}
/*
 * Otherwise we need to look at platform triggers.
 */
	comps = pd_CompList (Pd);
	found = 0;
	plats = 0;
	for (c = 0; comps[c]; c++)
	{
	/*
	 * Get the trigger platform.
	 */
		if (! pd_Retrieve(Pd, comps[c], "trigger", list, SYMT_STRING))
			continue;
		n = CommaParse (list, triggers);
		i = -1;
		while (++i < n)
		{
			if (! strcmp (triggers[i], "platform"))
			{
				if (!(plats++) &&
				    pda_ReqSearch (Pd, comps[c], "platform",
					   NULL, platform, SYMT_STRING))
				{
					n += CommaParse (platform, triggers+n);
				}
				continue;
			}
			pid = ds_LookupPlatform(triggers[i]);
			if (pid == BadPlatform)
				continue;
		/*
		 * Find the most recent time.
		 */
			if (!ds_DataTimes(pid, &PlotTime, 1, DsBefore, &avail))
				continue;
			if (!(found++) || TC_Less (latest, avail))
			{
				latest = avail;
				fid = pid;
			}
		}
	}
/*
 * If we found something, we go with it; otherwise keep the
 * plot time as it was. 
 */
	if (found)
	{
		*t = latest;
		msg_ELog (EF_DEBUG, "%s: %s @ %s",
			  "rolling plot time back to platform trigger",
			  ds_PlatformName(fid), TC_AscTime (t, TC_TimeOnly));
	}
}




void
px_AddComponent (c, update)
char	*c;
Boolean	update;
/*
 * Add the given component to the current plot.
 * Update is true if we're appending more data to a previous plot.
 */
{
	int	rtype;
	zbool	disable = FALSE;
	char	rep[30];
/*
 * If this overlay is disabled, ignore it.
 */
	if (pda_Search (Pd, c, "disable", c, (char *) &disable, SYMT_BOOL) &&
	    disable)
		return;
/*
 * Get the representation type
 */
	if (! pda_ReqSearch (Pd, c, "representation", NULL, rep, SYMT_STRING))
		return;
/*
 * Execute the appropriate plot table entry
 */
	rtype = px_NameToNumber (rep, Rt_table);
	if (rtype < 0)
		msg_ELog (EF_PROBLEM, 
			  "Comp '%s': unknown representation, '%s'", c, rep);
	else if (Plot_routines[PlotType][rtype] == NULL)
		msg_ELog (EF_PROBLEM, 
			  "Comp '%s': no representation '%s' for type '%s'",
			  c, rep, px_NumberToName (PlotType, Pt_table));
	else if (Plot_routines[PlotType][rtype] == UNCOMPILED_FUNCTION)
		msg_ELog (EF_PROBLEM, 
			  "Comp '%s': '%s' plot, type '%s' was not compiled",
			  c, rep, px_NumberToName (PlotType, Pt_table));
	else
		 (*Plot_routines[PlotType][rtype]) (c, update);
}




void
px_Init ()
/*
 * Initialize the table of plotting routines
 */
{
	int	pt, rt;
/*
 * Fill the table with NULLs
 */
	for (pt = 0; pt < N_PTYPES; pt++)
		for (rt = 0; rt < N_RTYPES; rt++)
			Plot_routines[pt][rt] = NULL;
/*
 * Put in the entries that exist and were chosen in configuration.
 * Entries that do not exist are left null, those that were not chosen
 * are set to UNCOMPILED_FUNCTION.
 */
# if C_PT_CAP
	Plot_routines[PT_CAP][RT_INIT] = CAP_Init;
	Plot_routines[PT_CAP][RT_FCONTOUR] = CAP_FContour;
	Plot_routines[PT_CAP][RT_CONTOUR] = CAP_LineContour;
	Plot_routines[PT_CAP][RT_STATION] = CAP_Station;
# if C_CAP_POLAR
	Plot_routines[PT_CAP][RT_POLAR] = CAP_Polar;
# endif
# else
	Plot_routines[PT_CAP][RT_INIT] = NULL;
	Plot_routines[PT_CAP][RT_FCONTOUR] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_CAP][RT_CONTOUR] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_CAP][RT_STATION] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_CAP][RT_POLAR] = UNCOMPILED_FUNCTION;
# endif
# if C_CAP_VECTOR
	Plot_routines[PT_CAP][RT_WIND] = CAP_Vector;
# else
	Plot_routines[PT_CAP][RT_WIND] = UNCOMPILED_FUNCTION;
# endif
# if C_CAP_RASTER
	Plot_routines[PT_CAP][RT_RASTER] = CAP_Raster;
# else
	Plot_routines[PT_CAP][RT_RASTER] = UNCOMPILED_FUNCTION;
# endif
# if C_CAP_TRACKS
	Plot_routines[PT_CAP][RT_TRACK] = tr_CAPTrack;
# else
	Plot_routines[PT_CAP][RT_TRACK] = UNCOMPILED_FUNCTION;
# endif
# if C_CAP_OVERLAY
	Plot_routines[PT_CAP][RT_OVERLAY] = ov_CAPOverlay;
# else
	Plot_routines[PT_CAP][RT_OVERLAY] = UNCOMPILED_FUNCTION;
# endif
# if C_CAP_LIGHTNING
	Plot_routines[PT_CAP][RT_LIGHTNING] = li_CAPLight;
# else
	Plot_routines[PT_CAP][RT_LIGHTNING] = UNCOMPILED_FUNCTION;
# endif

# if C_PT_SKEWT
	Plot_routines[PT_SKEWT][RT_SKEWT] = sk_Skewt;
# else
	Plot_routines[PT_SKEWT][RT_SKEWT] = UNCOMPILED_FUNCTION;
# endif

# if C_PT_XSECT
	Plot_routines[PT_XSECT][RT_INIT] = xs_Init;
	Plot_routines[PT_XSECT][RT_CONTOUR] = xs_LineContour;
	Plot_routines[PT_XSECT][RT_FCONTOUR] = xs_FilledContour;
	Plot_routines[PT_XSECT][RT_WIND] = xs_Vector;
	Plot_routines[PT_XSECT][RT_RASTER] = xs_Raster;
	Plot_routines[PT_XSECT][RT_TRACK] = xs_Track;
# else
	Plot_routines[PT_XSECT][RT_INIT] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XSECT][RT_CONTOUR] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XSECT][RT_FCONTOUR] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XSECT][RT_WIND] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XSECT][RT_RASTER] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XSECT][RT_TRACK] = UNCOMPILED_FUNCTION;
# endif
# if C_PT_TSERIES
	Plot_routines[PT_TSERIES][RT_TSERIES] = ts_Plot;	
# else
	Plot_routines[PT_TSERIES][RT_TSERIES] = UNCOMPILED_FUNCTION;
# endif
# if C_PT_XYGRAPH
	Plot_routines[PT_XYGRAPH][RT_INIT] = xy_Init;
	Plot_routines[PT_XYGRAPH][RT_SIMPLE] = xy_Graph;	
	Plot_routines[PT_XYGRAPH][RT_WIND] = xy_Wind;	
	Plot_routines[PT_XYGRAPH][RT_CONTOUR] = xy_Contour;	
	Plot_routines[PT_XYGRAPH][RT_FCONTOUR] = xy_Contour;	
	Plot_routines[PT_XYGRAPH][RT_OBS] = xy_Observation;	
# else
	Plot_routines[PT_XYGRAPH][RT_INIT] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XYGRAPH][RT_SIMPLE] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XYGRAPH][RT_WIND] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XYGRAPH][RT_CONTOUR] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_XYGRAPH][RT_OBS] = UNCOMPILED_FUNCTION;
# endif
# if C_PT_HISTOGRAM
	Plot_routines[PT_HISTOGRAM][RT_INIT] = HG_Init;
	Plot_routines[PT_HISTOGRAM][RT_BARCHART] = HG_CountBarChart;
# else
	Plot_routines[PT_HISTOGRAM][RT_INIT] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_HISTOGRAM][RT_BARCHART] = UNCOMPILED_FUNCTION;
# endif

# if C_PT_THETAPLOT
	Plot_routines[PT_THETAPLOT][RT_THETAE] = TP_ThetaPlot;
	Plot_routines[PT_THETAPLOT][RT_THETAW] = TP_ThetaPlot;
# else
	Plot_routines[PT_THETAPLOT][RT_THETAE] = UNCOMPILED_FUNCTION;
	Plot_routines[PT_THETAPLOT][RT_THETAW] = UNCOMPILED_FUNCTION;
# endif
}





void
px_AdjustCoords (x0, y0, x1, y1)
float *x0, *y0, *x1, *y1;
/*
 * Adjust these coordinates so that (1) the entire given range fits on the
 * screen, and (2) the scaling is the same in both directions.
 */
{
	int width = (F_X1 - F_X0) * GWWidth (Graphics);
	int height = (F_Y1 - F_Y0) * USABLE_HEIGHT;
	float hkmp, vkmp;	/* horiz km/pixel, vert too */
/*
 * Figure out the current scales in both directions.
 */
	hkmp = (*x1 - *x0) / width;
	vkmp = (*y1 - *y0) / height;
/*
 * Find which one is greater, and adjust the other to match.
 */
	if (hkmp > vkmp)
	{
		float newv = hkmp * height;
		float incr = newv - (*y1 - *y0);
		*y0 -= incr / 2;
		*y1 += incr / 2;
	}
	else
	{
		float newh = vkmp * width;
		float incr = newh - (*x1 - *x0);
		*x0 -= incr / 2;
		*x1 += incr / 2;
	}
}








int
px_NameToNumber (name, table)
char		*name;
name_to_num	*table;
/*
 * Return the number associated with string 'name' in 'table'
 */
{
	name_to_num	entry;

	while (TRUE)
	{
		entry = *table++;

		if (entry.name == NULL)
			return (-1);

		if (strcmp (entry.name, name) == 0)
			return (entry.number);
	}
}




char *
px_NumberToName (num, table)
int		num;
name_to_num	*table;
/*
 * Return the name associated with 'num' in 'table'
 */
{
	name_to_num	entry;
	static char	nullstring[] = "";

	while (TRUE)
	{
		entry = *table++;

		if (entry.name == NULL)
			return (nullstring);

		if (entry.number == num)
			return (entry.name);
	}
}




char *
px_FldDesc (fld)
char *fld;
/*
 * Return the description of this field.
 */
{
	char *ret = F_GetDesc (F_Lookup (fld));
	
	return ((ret && ret[0]) ? ret : fld);
}





void
px_SetEOPHandler (handler)
void (*handler) ();
/*
 * Set a routine to be called after a plot has been done.  This stuff is
 * kludge that should be replaced with a more general action mechanism.
 */
{
	if (EOPHandler)
		msg_ELog (EF_PROBLEM, "Overriding existing EOP handler");
	EOPHandler = handler;
}



void
px_ClearEOPHandler ()
{
	EOPHandler = 0;
}



		
char *
px_ModelTimeLabel ()
/*
 * Return a string showing the valid/delta time for model data.  The string
 * is valid until the next call to this function and should not be modified.
 */
{
	static char	label[40];
	int	year, month, day, hour, minute, second, usecond;
	ZebTime	valid;
/*
 * Forecast delta
 */
	if (ForecastOffset % 3600)
	{
	/*
	 * We need minutes and seconds in the forecast time
	 */
		sprintf (label, "%li:%02li:%02li forecast", 
			 ForecastOffset / 3600,
			 (ForecastOffset / 60) % 60, ForecastOffset % 60);
	}
	else
	{
	/*
	 * Just need hours
	 */
		sprintf (label, "%li h forecast", ForecastOffset / 3600);
	}
/*
 * Valid time
 */
	valid = PlotTime;
	if (! ValidationMode)
		valid.zt_Sec += ForecastOffset;

	TC_ZtSplit (&valid, &year, &month, &day, &hour, &minute, &second, 
		    &usecond);

	sprintf (label + strlen (label), ", valid %d/%d %d:%02d", month, day,
		 hour, minute);

	return (label);
}
