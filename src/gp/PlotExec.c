/*
 * Plot execution module
 */
static char *rcsid = "$Id: PlotExec.c,v 1.22 1991-04-19 17:35:57 kris Exp $";

# include <X11/Intrinsic.h>
# include <ui.h>
# include <defs.h>
# include <message.h>
# include <pd.h>
# include <DataStore.h>
# include <ui_date.h>
# include "GC.h"
# include "GraphProc.h"
# include "DrawText.h"
# include "PixelCoord.h"
# include "EventQueue.h"

/*
 * Macro for a pointer to x cast into a char *
 */
# define CPTR(x)	(char *)(&(x))

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
# define N_PTYPES	3	/* Increase this as plot types are added */

name_to_num Pt_table[] =
{
	{"CAP",		PT_CAP		},
	{"skewt", 	PT_SKEWT	},
	{"xsect",	PT_XSECT	},
	{NULL,		0		}
};

/*
 * Plot types and a string <--> representation type table
 */
# define RT_INIT	0	/* "Initialize" representation	*/
# define RT_CONTOUR	1
# define RT_VECTOR	2
# define RT_RASTER	3
# define RT_TRACK	4
# define RT_OVERLAY	5
# define RT_SKEWT	6
# define RT_FCONTOUR	7
# define N_RTYPES	8	/* Increase this as rep. types are added */

name_to_num Rt_table[] = 
{
	{"filled-contour",	RT_FCONTOUR	},
	{"contour",		RT_CONTOUR	},
	{"line-contour",	RT_CONTOUR	},
	{"vector",		RT_VECTOR	},
	{"raster",		RT_RASTER	},
	{"track",		RT_TRACK	},
	{"overlay",		RT_OVERLAY	},
	{"skewt",		RT_SKEWT	},
	{NULL,			0		}
};

/*
 * Two dimensional table (plot type vs. plot representation)
 * of plotting routines and a boolean to record if it's been initialized
 */
static void	(*Plot_routines[N_PTYPES][N_RTYPES])();
static Boolean	Table_built = FALSE;

/*
 * Our plot type
 */
static int	PlotType;

/*
 * The end of plot handler, if any.
 */
static void	(*EOPHandler) () = 0;

/*
 * Forward declarations
 */
# ifdef __STDC__
	int	px_NameToNumber (char *, name_to_num *);
	void	px_Init ();
	void	px_AddComponent (char *, int);
	void	CAP_FContour (char *, int);
	void	CAP_Vector (char *, int);
	void	CAP_Raster (char *, int);
	void	CAP_LineContour (char *, int);
	void	CAP_Init (time *);
	void	px_AdjustCoords (float *, float *, float *, float *);
	static bool px_GetCoords (void);
# else
	int	px_NameToNumber ();
	void	px_Init (), px_AddComponent (), CAP_FContour ();
	void	CAP_Vector (), CAP_Raster (), CAP_LineContour ();
	void	CAP_Init ();
	void	px_AdjustCoords ();
	static bool px_GetCoords ();
# endif

/*
 * Other routines.
 */
extern void	tr_CAPTrack (), ov_CAPOverlay (), sk_Skewt ();
extern void	xs_LineContour (), xs_FilledContour ();

/*
 * How many plot components in our plot description and which
 * component are we currently dealing with?
 */
static int	Ncomps;
int	Comp_index;


/*
 * Annotation stuff
 */
XColor Tadefclr;
Pixel	White;

# ifdef notdef
/*
 * Stuff for px_GetGrid (the phony data store)
 * Macro to reference the grid two-dimensionally
 */
/* # define GRID(i,j)	grid[(i)* (*ydim) + (j)] */
# define GRID(j,i)	grid[(i)* (*ydim) + (j)]
static float Melev = 0.0;	/* Mean pam elevation		*/

# endif



void
px_PlotExec (component)
char	*component;
/*
 * Execute the given component of the plot description
 */
{
	char	plt[30];
	int	i;
	Boolean	global;
	time 	cachetime;
/*
 * Check now for an abort condition
 */
	if (Abort)
	{
		msg_ELog (EF_INFO, "%d %06d %s plot aborted!",
			PlotTime.ds_yymmdd, PlotTime.ds_hhmmss, component);
		return;
	}
/*
 * Initialize the table of plot functions if necessary
 */
	if (! Table_built)
		px_Init ();
/*
 * Get the white pixel value
 */
	White = WhitePixelOfScreen (XtScreen (Graphics));
/*
 * Set the busy cursor so that people know something is going on.
 */
	XDefineCursor (XtDisplay (Top), XtWindow (Graphics), BusyCursor);
	eq_sync ();
/*
 * Get the plot type
 */
	if (!pda_ReqSearch (Pd, "global", "plot-type", NULL, plt, SYMT_STRING))
		return;
	PlotType = px_NameToNumber (plt, Pt_table);
/*
 * Figure out coords.
 */
	if (! px_GetCoords ())
		return;
/*
 * Global or update plot?
 */
	if ((global = strcmp (component, "global") == 0) &&
		(PlotMode == RealTime))
	/*
	 * Semi-kludge: roll back the plot time to the last trigger incr.
	 */
		px_FixPlotTime (&cachetime);
	else cachetime = PlotTime;
/*
 * Three possibilities:
 *
 * (1) Global plot but already in the cache.
 */
	if (global && (DisplayFrame = fc_LookupFrame (&cachetime)) >= 0)
	{
	/*
	 * Set up to draw in this frame -- we may be returning to RT mode
	 * after a movie, and partial updates will need to go here.
	 */
		DrawFrame = DisplayFrame;
		GWDrawInFrame (Graphics, DrawFrame);
	}
/*
 * (2) Global plot not cached.
 */
	else if (global)
		px_GlobalPlot (&cachetime);
/*
 * (3) Update plot.
 */
	else
		px_AddComponent (component, True);
/*
 * Display the frame
 */
/*	DisplayFrame = DrawFrame; */
	GWDisplayFrame (Graphics, DisplayFrame);
/*
 * Call the end of plot handler, if there is one.
 */
	if (EOPHandler)
		(*EOPHandler) ();
/*
 * Put the cursor back.
 */
	XDefineCursor (XtDisplay (Top), XtWindow (Graphics), NormalCursor);
	XSync (XtDisplay (Top), False);
}






void
px_GlobalPlot (cachetime)
time *cachetime;
/*
 * Perform a global update.
 */
{
	float orig_alt,tascale;
	char **comps, datestring[40], rep[30], tadefcolor[30];
	int i;
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
 * Get annotation information
 */
	if(! pd_Retrieve (Pd, "global", "ta-color", tadefcolor, SYMT_STRING))
		strcpy(tadefcolor, "white");
	if(! ct_GetColorByName(tadefcolor, &Tadefclr))
	{
		msg_ELog(EF_PROBLEM, "Can't get default color: '%s'.",
			tadefcolor);
		strcpy(tadefcolor, "white");
		ct_GetColorByName(tadefcolor, &Tadefclr);
	}
	if(pd_Retrieve(Pd, "global", "ta-scale", (char *) &tascale,
		SYMT_FLOAT))
		An_SetScale(tascale);
	
/*
 * Annotate with the date and time
 */
	An_ResetAnnot (Ncomps);
	ud_format_date (datestring, (date *)(&PlotTime), UDF_FULL);
	strcat (datestring, "  ");
	An_TopAnnot (datestring, Tadefclr.pixel);
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
		Comp_index = i - 1;
		px_AddComponent (comps[i], False);
	}
/*
 * On CAPs, annotate the altitude we eventually got.
 */
	if (PlotType == PT_CAP)
		CAP_Finish (Alt);
/*
 * If the altitude has changed, stash it.
 */
	if (Alt != orig_alt)
	{
		pd_Store (Pd, "global", "altitude", CPTR (Alt),
			SYMT_FLOAT);
		Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
	}
/*
 * Add this one to the cache.
 */
	fc_AddFrame (cachetime, DisplayFrame);
	lw_LoadStatus ();
}




static bool
px_GetCoords ()
{
	bool ok, cvt_Origin ();
	int expand, rs = 0;
	float lat, lon;
/*
 * Get the origin and plot limits
 */
	ok = pda_ReqSearch (Pd, "global", "origin-lat", NULL, CPTR (lat), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "origin-lon", NULL, CPTR (lon), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "x-min", NULL, CPTR (Xlo), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "x-max", NULL, CPTR (Xhi), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "y-min", NULL, CPTR (Ylo), 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "y-max", NULL, CPTR (Yhi), 
		SYMT_FLOAT);
	if (! ok)
		return (FALSE);
/*
 * Get the altitude too.  Default it to ground level if all else fails.
 */
	if (! pd_Retrieve (Pd, "global", "radar-space", CPTR (rs), SYMT_BOOL))
		rs = FALSE;
	if (! pda_Search (Pd, "global", "altitude", NULL, CPTR (Alt),
				SYMT_FLOAT))
		Alt = 0;
/*
 * Assume that we are not yet dealing with space-based platforms.  If we're
 * not in radar-space kludge mode, and the altitude is stratospheric, assume
 * that they give it in meters.
 */
	if (Alt > 10 && ! rs)
		Alt /= 1000.0;
/*
 * Unless told otherwise, readjust the coordinates so that x == y.
 */
	if (! pda_Search (Pd, "global", "expand", NULL, (char *) &expand,
			SYMT_BOOL) || expand == FALSE)
		px_AdjustCoords (&Xlo, &Ylo, &Xhi, &Yhi);
/*
 * Save the origin
 */
	if (! cvt_Origin (lat, lon))
		return (FALSE);
	return (TRUE);
}




void
px_FixPlotTime (t)
time *t;
/*
 * Roll back the plot time to the nearest multiple of the trigger time.
 */
{
	char trigger[40], **comps;
	int itrigger, seconds, i;
	time latest, avail;
	PlatformId pid;
/*
 * If the global trigger is time based, we just roll back to that time.
 */
	if (! pda_Search (Pd, "global", "trigger", 0, trigger, SYMT_STRING))
		return;
	if (itrigger = pc_TimeTrigger (trigger))
	{
		seconds = (PlotTime.ds_hhmmss/10000)*60*60 +
			  ((PlotTime.ds_hhmmss/100) % 100)*60 +
			  (PlotTime.ds_hhmmss % 100);
		seconds -= seconds % itrigger;
		t->ds_hhmmss = (seconds/3600)*10000 + ((seconds/60) % 60)*100 +
					seconds % 60;
		t->ds_yymmdd = PlotTime.ds_yymmdd;
		return;
	}
/*
 * Otherwise we need to look at platform triggers.
 */
	comps = pd_CompList (Pd);
	latest.ds_yymmdd = 800101;	/* pretty early */
	for (i = 0; comps[i]; i++)
	{
	/*
	 * Get the trigger platform.
	 */
	 	if (! pd_Retrieve (Pd, comps[i], "trigger", trigger,
				SYMT_STRING))
			continue;
		if ((pid = ds_LookupPlatform (trigger)) == BadPlatform)
			continue;
	/*
	 * Find the most recent time.
	 */
	 	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, &avail))
			continue;
		if (DLE (latest, avail))
			latest = avail;
	}
/*
 * If we found something, we go with it; otherwise keep the plot time
 * as it was.
 */
	if (latest.ds_yymmdd > 800101)	/* still pretty early */
		*t = latest;
	else
		*t = PlotTime;
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
	int	rtype, disable = FALSE;
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

	rtype = px_NameToNumber (rep, Rt_table);
/*
 * Execute the appropriate plot table entry
 */
	if (Plot_routines[PlotType][rtype] != NULL)
		(*Plot_routines[PlotType][rtype]) (c, update);
	else
		msg_ELog (EF_PROBLEM, "Cannot make a '%s' plot of type '%s'",
			rep, PlotType);
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
 * Put in the entries that exist
 */
	Plot_routines[PT_CAP][RT_INIT] = CAP_Init;

	Plot_routines[PT_CAP][RT_FCONTOUR] = CAP_FContour;
	Plot_routines[PT_CAP][RT_CONTOUR] = CAP_LineContour;
	Plot_routines[PT_CAP][RT_VECTOR] = CAP_Vector;
	Plot_routines[PT_CAP][RT_RASTER] = CAP_Raster;
	Plot_routines[PT_CAP][RT_TRACK] = tr_CAPTrack;
	Plot_routines[PT_CAP][RT_OVERLAY] = ov_CAPOverlay;

	Plot_routines[PT_SKEWT][RT_SKEWT] = sk_Skewt;

	Plot_routines[PT_XSECT][RT_CONTOUR] = xs_LineContour;
	Plot_routines[PT_XSECT][RT_FCONTOUR] = xs_FilledContour;
/*
 * Done
 */
	Table_built = TRUE;
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
	hkmp = (*x1 - *x0)/width;
	vkmp = (*y1 - *y0)/height;
/*
 * Find which one is greater, and adjust the other to match.
 */
	if (hkmp > vkmp)
	{
		float newv = hkmp*height;
		float incr = newv - (*y1 - *y0);
		*y0 -= incr/2;
		*y1 += incr/2;
	}
	else
	{
		float newh = vkmp*width;
		float incr = newh - (*x1 - *x0);
		*x0 -= incr/2;
		*x1 += incr/2;
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
px_FldDesc (comp, fld)
char *comp, *fld;
/*
 * Return the description of this field.
 */
{
	static char ret[80];

	if (pda_Search (Pd, comp, "desc", fld, ret, SYMT_STRING))
		return (ret);
	return (fld);
}






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



px_ClearEOPHandler ()
{
	EOPHandler = 0;
}
                                                                                 