/*
 * Window plot control routines.
 */
static char *rcsid = "$Id: PlotControl.c,v 1.4 1991-03-05 23:13:38 kris Exp $";

# include <ctype.h>
# include <X11/Intrinsic.h>

# include "../include/GraphicsW.h"
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include "../include/pd.h"
# include "../include/DataStore.h"
# include "GraphProc.h"
# include "EventQueue.h"




/*
 * Local stuff.
 */
static int	MovieEvent = -1;

# ifdef __STDC__
	int pc_TimeTrigger (char *);
	static void pc_SetTimeTrigger (int, char *);
	static void pc_PlotAlarm (time *, char *);
	static void pc_FrameAlarm ();
	static void pc_Plot (char *);
	static void pc_NextFrame ();
	static void pc_Notification (PlatformId, int, time *);
# else
	int pc_TimeTrigger ();
	static void pc_SetTimeTrigger ();
	static void pc_PlotAlarm ();
	static void pc_FrameAlarm ();
	static void pc_Plot ();
	static void pc_NextFrame ();
	static void pc_Notification ();
# endif



/*
 * The coordinate stack.
 */
typedef struct coordstack
{
	float cs_x0, cs_y0, cs_x1, cs_y1;
	struct coordstack *cs_next;
} CoordStack;

CoordStack *O_coords = 0;


/*
 * The following table is a rudimentary attempt to rule-ize the special
 * treatment of PD parameter changes.  Someday it should be smarter.
 */

/*
 * The default action when a parameter change comes in is to (1) invalidate
 * the frame cache, (2) (not) wipe out the icon bar, and (3) do a complete 
 * redraw.  These flags can change that.
 */
# define F_NOINVALIDATE		0x0001	/* Preserve frame cache		*/
# define F_NOREDRAW		0x0002	/* Do not replot the screen	*/
# define F_MOVIEUPD		0x0004	/* Update movie parameters	*/
# define F_ICONS		0x0008	/* Redo the icons		*/

/*
 * The structure describing PD parameters.  We should probably read this
 * from a file, eventually.  If necessary, an action procedure could be
 * added which would allow the calling of an outside procedure for truly
 * special cases.
 */
struct ParamAction
{
	char	*pa_param;		/* The parameter of interest	*/
	int	pa_flags;		/* Flags saying what to do	*/
};

/*
 * Here is the table.
 */
static struct ParamAction
PActions[] =
{
	{ "altitude",		F_NOINVALIDATE				},
	{ "disable",		F_ICONS					},
	{ "field",		F_NOINVALIDATE				},
	{ "frame-rate",		F_NOREDRAW | F_NOINVALIDATE | F_MOVIEUPD },
	{ "frame-skip",		F_NOREDRAW | F_NOINVALIDATE | F_MOVIEUPD },
	{ "icon",		F_NOREDRAW | F_NOINVALIDATE | F_ICONS	},
	{ "icon-background",	F_NOREDRAW | F_NOINVALIDATE | F_ICONS	},
	{ "icon-color",		F_NOREDRAW | F_NOINVALIDATE | F_ICONS	},
	{ "platform",		F_ICONS					},
	{ "plot-mode",		F_ICONS					},
	{ "movie-end-time",	F_NOREDRAW | F_NOINVALIDATE | F_MOVIEUPD },
	{ "movie-minutes",	F_NOREDRAW | F_NOINVALIDATE | F_MOVIEUPD },
	{ "trigger",		F_NOINVALIDATE				},
	{ "xorvalue",		F_NOREDRAW | F_NOINVALIDATE },
	{ 0, 0 }
};



void
pc_PlotHandler ()
/*
 * Deal with plotting based on plot mode
 */
{
	char	pmstring[80];
	time	ptime;
	Arg	arg;

/*
 * Cancel all existing timer requests.
 */
 	tl_AllCancel ();
	ds_CancelNotify ();
/*
 * Get the plot mode
 */
	PlotMode = RealTime;

	if (! pda_Search (Pd, "global", "plot-mode", 0, pmstring, SYMT_STRING))
		msg_log ("No plot mode given -- real-time used");
	else if (! strcmp (pmstring, "real-time"))
		PlotMode = RealTime;
	else if (! strcmp (pmstring, "history"))
		PlotMode = History;
	else
		msg_log ("Unknown plot mode '%s' -- real-time used", pmstring);
/*
 * Movie mode?
 */
	MovieMode = FALSE;
	pda_Search (Pd, "global", "movie-mode", 0, &MovieMode, SYMT_BOOL);
/*
 * Get the path to the FrameFile from the plot description.
 */
	if(! pda_Search (Pd, "global", "file-path", NULL, FrameFilePath,
		SYMT_STRING))
		strcpy(FrameFilePath, "/dt/tmp");
	FrameFileFlag = TRUE;
	fc_CreateFrameFile();
/*
 * Figure out how many frames we need the frame cache to have and the
 * maximum number of pixmaps the graphics widget may have.
 */
	FrameCount = 1;
	pda_Search (Pd, "global", "time-frames", 0, (char *)(&FrameCount), 
		SYMT_INT);
	/* 
	 * Add stuff here to get more frames for spatial depth
	 * and multiple base fields. . .
	 */
/*
 * Tell the graphics widget and frame cache how many frames we need
 */
	fc_SetNumFrames(FrameCount);
	XtSetArg (arg, XtNframeCount, FrameCount);
	XtSetValues (Graphics, &arg, 1);
/*
 * If we are running in real time mode, and the window is visible,
 * get our triggers set up.
 */
	if (PlotMode == RealTime && WindowState == UP)
		pc_SetUpTriggers ();
/*
 * If we're in history mode, find the plot time and do a global plot now
 */
	if (PlotMode == History)
	{
		if (! pda_Search (Pd, "global", "plot-time", 0, 
			(char *) &PlotTime, SYMT_DATE))
		{
			msg_log ("No plot time in plot description");
			return;
		}
	}
	else
		tl_GetTime (&PlotTime);
/*
 * Force a replot if the window is visible.
 */
	if (WindowState == UP)
		Eq_AddEvent (PDisplay, pc_Plot, "global", 7, Override);
/*
 * If we're in movie mode, set up the movie trigger
 */
	if (MovieMode && WindowState == UP)
		pc_DoMovie ();
	
}




void
pc_ParamChange (param)
char	*param;
/*
 * Deal with a parameter change in the PD
 */
{
	bool invalidate = TRUE;
	struct ParamAction *pa = PActions;
/*
 * Try to find this parameter in our table.
 */
	while (pa->pa_param && strcmp (pa->pa_param, param))
		pa++;
/*
 * If we did't find it, take the default action.
 */
	if (! pa->pa_param)
	{
		msg_ELog (EF_DEBUG, "Change '%s' -- no entry", param);
		ct_FreeColors ();	/* Release all colors	*/
		fc_InvalidateCache ();  /* Invalidate cache	*/
		pc_PlotHandler ();	/* Schedule an update	*/
		return;			/* That's enough	*/
	}
/*
 * Otherwise do what it says.
 */
	msg_ELog (EF_DEBUG, "Change '%s' -- flags 0x%x", param, pa->pa_flags);
	if ((pa->pa_flags & F_NOINVALIDATE) == 0)
	{
		ct_FreeColors ();
		fc_InvalidateCache ();
	}
	if (pa->pa_flags & F_MOVIEUPD)
		mc_LoadParams ();
	if (pa->pa_flags & F_ICONS)
		I_DoIcons ();
	if ((pa->pa_flags & F_NOREDRAW) == 0)
		pc_PlotHandler ();
}




pc_SetUpTriggers ()
/*
 * Figure out what our trigger condition will be.
 */
{
	char trigger[200], **comps;
	int i;
/*
 * Find the global trigger first.
 */
	if (! pda_Search (Pd, "global", "trigger", 0, trigger, SYMT_STRING))
		msg_ELog (EF_INFO, "No global trigger specified!");
	else
		pc_DoTrigger (trigger, "global");
/*
 * Now go through and find the minor updates for each component.
 */
	comps = pd_CompList (Pd);
	for (i = 1; comps[i]; i++)
		if (pd_Retrieve (Pd, comps[i], "trigger", trigger,SYMT_STRING))
			pc_DoTrigger (trigger, comps[i]);
}





pc_DoTrigger (trigger, comp)
char *trigger, *comp;
/*
 * Cope with a trigger condition.
 */
{
	int seconds;
	PlatformId pid;
/*
 * Try to interpret the trigger as a time.
 */
	if (seconds = pc_TimeTrigger (trigger))
		pc_SetTimeTrigger (seconds, comp);
/*
 * Then as a platform name.
 */
	else if ((pid = ds_LookupPlatform (trigger)) != BadPlatform)
		ds_RequestNotify (pid, 0, pc_Notification);
	else
		msg_log ("Funky trigger time '%s' in component %s", trigger, 
			comp);
}




int pc_TimeTrigger (trigger)
char *trigger;
/*
 * Try to interpret this trigger condition as a time.
 */
{
	int seconds = 0;
/*
 * Interpret as much as possible as a number.
 */
	while (isdigit (*trigger))
		seconds = seconds*10 + *trigger++ - '0';
/*
 * Insist on, at most, a following "m" or "s".
 */
	if (! *trigger || (*trigger == 's' && trigger[1] == '\0'))
		return (seconds);
	else if (trigger[0] == 'm' && trigger[1] == '\0')
		return (seconds*60);
	else
		return (0);
}





static void
pc_SetTimeTrigger (seconds, param)
int seconds;
char *param;
/*
 * Actually arrange for a time trigger to happen.
 */
{
	int ns;
	time t;
/*
 * Get the current time, and pull out the seconds portion.
 */
	tl_GetTime (&t);
	ns = t.ds_hhmmss % 100;
/*
 * If the trigger time is less than a second, just do it
 */
	if (seconds < 1)
		/* nothing */;
/*
 * If our trigger time is less than one minute, arrange things to line
 * up with the minute boundary.
 */
	else if (seconds < 60)
		pmu_dadd (&t.ds_yymmdd, &t.ds_hhmmss, (60 - ns) % seconds);
/*
 * Otherwise zap the seconds to zero, and aim for the hour boundary.
 */
	else
	{
		int min;
		t.ds_hhmmss -= t.ds_hhmmss % 100;
		min = (t.ds_hhmmss/100) % 100;
		pmu_dadd (&t.ds_yymmdd, &t.ds_hhmmss, 
			100*((60 - min) % (seconds/60)));
	}
/*
 * Send off the alarm request.
 */
	tl_AddAbsoluteEvent (pc_PlotAlarm, param, &t, seconds * INCFRAC);
}




static void
pc_PlotAlarm (t, comp)
time *t;
char *comp;
/*
 * Deal with a timer alarm.
 */
{
	PlotTime = *t;
	msg_ELog (EF_DEBUG, "Plot alarm at %d %d", t->ds_yymmdd, t->ds_hhmmss);
	Eq_AddEvent (PDisplay, pc_Plot, comp, 1 + strlen (comp),
		(strcmp (comp, "global") ? Bounce : Override));
}





static void
pc_Notification (pid, global, t)
PlatformId pid;
int global;
time *t;
/*
 * A data available notification has arrived.  Only do global updates for
 * the moment, until we decide how we're passing the component through
 * the PARAM field....
 */
{
	PlotTime = *t;
	msg_ELog (EF_DEBUG, "Data available on %s at %d %d", 
		ds_PlatformName (pid), t->ds_yymmdd, t->ds_hhmmss);
	Eq_AddEvent (PDisplay, pc_Plot, "global", 7, Override);
}





static void
pc_Plot (comp)
char	*comp;
/*
 * Actually execute the plot of the given component
 */
{
	 msg_ELog (EF_DEBUG, "pc_Plot (%s)", comp); 
/*
 * Stop movie mode until the plot is done
 */
	if (MovieEvent >= 0)
	{
		tl_Cancel (MovieEvent);
		MovieEvent = -1;
	}
/*
 * Plot
 */
	px_PlotExec (comp);
/*
 * Turn on the movie again if necessary
 */
	if (MovieMode)
		pc_DoMovie ();
}





void
pc_CancelPlot ()
/*
 * Cancel any ongoing plotting activity.
 */
{
/*
 * Set the abort flag for anything that might be running at the time.
 */
	Abort = TRUE;
	Eq_AddEvent (PDisplay, eq_ResetAbort, 0, 0, Bounce);
/*
 * Cancel any timer requests.
 */
	tl_AllCancel ();
	ds_CancelNotify ();
}




pc_DoMovie ()
/*
 * Get a movie going
 */
{
	float	fr;	/* Movie rate in frames/second */
	int	millisecs;
/*
 * Deal with a one frame movie
 */
	if (FrameCount == 1)
	{
		msg_log ("One frame movie!");
		return;
	}
/*
 * Find the frame rate
 */
	fr = 2.0;	/* default to 2 frames/second */
	pda_Search (Pd, "global", "frame-rate", 0, (char *)(&fr), SYMT_FLOAT);
/*
 * Set up the timer
 */
	if (MovieEvent >= 0)
		tl_Cancel (MovieEvent);

	millisecs = (int)(1000 / fr);
	MovieEvent = tl_AddRelativeEvent (pc_FrameAlarm, NULL, 0, 
		(millisecs*INCFRAC)/1000);
}




static void
pc_FrameAlarm ()
/*
 * Queue an event to display the next frame
 */
{
	Eq_AddEvent (PDisplay, pc_NextFrame, NULL, 0, Bounce);
}




static void
pc_NextFrame ()
/*
 * Display the next frame
 */
{
	int index;

	DisplayFrame++;
	DisplayFrame %= FrameCount;
	GWDisplayFrame (Graphics, DisplayFrame);
	XSync (XtDisplay (Top), False);
}





static void
pc_SetCoords (x0, y0, x1, y1)
float x0, y0, x1, y1;
/*
 * Actually store these coords.
 */
{
	extern void eq_ReturnPD ();
/*
 * Put the new stuff in the PD.
 */
	pd_Store (Pd, "global", "x-min", (char *) &x0, SYMT_FLOAT);
	pd_Store (Pd, "global", "y-min", (char *) &y0, SYMT_FLOAT);
	pd_Store (Pd, "global", "x-max", (char *) &x1, SYMT_FLOAT);
	pd_Store (Pd, "global", "y-max", (char *) &y1, SYMT_FLOAT);
/*
 * Now get a replot done, and ship the PD back to the display manager.
 */
	fc_InvalidateCache ();  /* Invalidate cache	*/
	Eq_AddEvent (PDisplay, pc_PlotHandler, NULL, 0, Override);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
}




pc_PushCoords (cmds)
struct ui_command *cmds;
/*
 * Push a new set of coords onto the stack.
 */
{
	SValue v;
	int type;
	float x0, y0, x1, y1;
	CoordStack *cs;
/*
 * Get a new coord stack entry, and fill it in from the current values.
 */
	cs = ALLOC (CoordStack);
	cs->cs_x0 = Xlo;
	cs->cs_y0 = Ylo;
	cs->cs_x1 = Xhi;
	cs->cs_y1 = Yhi;
	cs->cs_next = O_coords;
	O_coords = cs;
/*
 * If the coords are given explicitly, use them.
 */
	if (cmds->uc_ctype != UTT_END)
	{
		x0 = UFLOAT (cmds[0]);
		y0 = UFLOAT (cmds[1]);
		x1 = UFLOAT (cmds[2]);
		y1 = UFLOAT (cmds[3]);
	}
/*
 * Otherwise assume that a drawbox has been run, and use those values.
 */
	else
	{
		usy_g_symbol (Vtable, "boxx0", &type, &v); x0 = v.us_v_float;
		usy_g_symbol (Vtable, "boxy0", &type, &v); y0 = v.us_v_float;
		usy_g_symbol (Vtable, "boxx1", &type, &v); x1 = v.us_v_float;
		usy_g_symbol (Vtable, "boxy1", &type, &v); y1 = v.us_v_float;
	}
/*
 * Store these values.
 */
	msg_ELog (EF_DEBUG, "Set coords to (%.2f %.2f) (%.2f %.2f)", x0, y0,
		x1, y1);
	pc_SetCoords (x0, y0, x1, y1);
}




pc_PopCoords ()
/*
 * Pop one level of coords off the stack.
 */
{
	CoordStack *cs = O_coords;
/*
 * Make sure they know what they are doing.
 */
	if (! O_coords)
	{
		msg_ELog (EF_PROBLEM, "Popcoords on empty stack");
		return;
	}
/*
 * Set up for the new coords.
 */
	pc_SetCoords (cs->cs_x0, cs->cs_y0, cs->cs_x1, cs->cs_y1);
	O_coords = cs->cs_next;
	free (cs);
}
