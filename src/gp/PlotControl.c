/*
 * Window plot control routines.
 */
static char *rcsid = "$Id: PlotControl.c,v 2.26 1993-12-01 17:10:58 burghart Exp $";
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

# include <ctype.h>
# include <X11/Intrinsic.h>
# include "GraphicsW.h"
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <pd.h>
# include <DataStore.h>
# include "GraphProc.h"
# include "EventQueue.h"
# include "LayoutControl.h"
# include "PixelCoord.h"


int		pc_TimeTrigger FP ((char *));
void		pc_TriggerGlobal FP (());
static void	pc_SetTimeTrigger FP ((int, char *));
static void	pc_PlotAlarm FP ((UItime *, char *));
static void	pc_Plot FP ((char *));
static void	pc_NextFrame FP (());
static void	pc_Notification FP ((PlatformId, int, UItime *));
static void	pc_DoTrigger FP ((char *, char *, int));


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
 *
 * The default action when a parameter change comes in is to (1) invalidate
 * the frame cache, (2) (not) wipe out the icon bar, and (3) do a complete 
 * redraw.  These flags can change that.
 */
# define F_NOINVALIDATE		0x0001	/* Preserve frame cache		*/
# define F_NOREDRAW		0x0002	/* Do not replot the screen	*/
# define F_MOVIEUPD		0x0004	/* Update movie parameters	*/
# define F_ICONS		0x0008	/* Redo the icons		*/


/*
 * The structure describing PD parameters. 
 */
struct ParamAction
{
	char	*pa_param;		/* The parameter of interest	*/
	int	pa_flags;		/* Flags saying what to do	*/
};


/*
 * Here is the table.  We should probably read this from a file, eventually.  
 * If necessary, an action procedure could be added which would allow the 
 * calling of an outside procedure for truly special cases.
 */
static struct ParamAction PActions[] =
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
	{ "plot-hold",		F_NOINVALIDATE				},
	{ "plot-mode",		F_ICONS					},
	{ "movie-end-time",	F_NOREDRAW | F_NOINVALIDATE | F_MOVIEUPD },
	{ "movie-minutes",	F_NOREDRAW | F_NOINVALIDATE | F_MOVIEUPD },
	{ "require",		F_NOREDRAW | F_NOINVALIDATE		},
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
	Arg	arg;
	static bool First = TRUE;
	bool hold = FALSE;
/*
 * Cancel all existing timer requests.
 */
 	tl_AllCancel ();
	ds_CancelNotify ();
/*
 * See if they have put us on hold.
 */
	if (pd_Retrieve (Pd, "global", "plot-hold", (char *) &hold, SYMT_BOOL)
	    && hold)
		return;
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
	msg_ELog(EF_DEBUG, "PlotMode: %s", (PlotMode == History) ? "History" :
		"RealTime");
/*
 * Movie mode?
 */
	MovieMode = FALSE;
	pda_Search (Pd, "global", "movie-mode", NULL, (char *) &MovieMode, 
		SYMT_BOOL);
	msg_ELog(EF_DEBUG, "Movie mode: %s", MovieMode ? "TRUE" : "FALSE");
/*
 * Post processing mode?
 */
	PostProcMode = FALSE;
	pda_Search (Pd, "global", "post-proc-mode", NULL, 
		(char *) &PostProcMode, SYMT_BOOL);
	msg_ELog(EF_DEBUG, "Post processing mode: %s", PostProcMode ? "TRUE" : 
		"FALSE");
/*
 * If a FrameFile doesn't already exist...
 * Get the path to the FrameFile from the plot description and
 * create it.
 */
	if (! FrameFileFlag)
	{
		if (pda_Search (Pd, "global", "file-path", NULL, 
			FrameFilePath, SYMT_STRING))
		{
			FrameFileFlag = TRUE;
			fc_CreateFrameFile();
		}
		else msg_ELog (EF_DEBUG, "No FrameFile.");
	}
/*
 * Figure out how many pixmaps we need the graphics widget to have.
 */
	FrameCount = 1;
	pda_Search (Pd, "global", "time-frames", 0, (char *)(&FrameCount), 
		SYMT_INT);
	pda_Search (Pd, "global", "max-frames", 0, (char *)(&MaxFrames), 
		SYMT_INT);
	if ((MaxFrames <= 0) || (MaxFrames > NCACHE))
		MaxFrames = NCACHE;
	msg_ELog (EF_DEBUG, "Setting MaxFrames to %d.", MaxFrames);
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
 *  If a movie is running, keep it running, so the changes can be incorporated
 *  into the movie.
 */		
	if((PlotMode == History) && MovieMode)
	{
		mc_PDChange();
		return;
	}
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
		tl_Time (&PlotTime);
/*
 * If we're in post processing mode assign PostProcTime to something.
 */
	if ((PostProcMode && (PlotMode == RealTime)) || First)
	{
		PostProcTime = PlotTime;
		First = FALSE;
	}
/*
 * Force a replot if the window is visible.
 */
	if (WindowState == UP)
		Eq_AddEvent (PDisplay, pc_Plot, "global", 7, Override);
}





void
pc_ParamChange (param)
char	*param;
/*
 * Deal with a parameter change in the PD
 */
{
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
		ct_FreeColors ();		/* Release all colors	*/
		fc_InvalidateCache ();  	/* Invalidate cache	*/
		if(! MovieMode)
		{
			pc_PlotHandler ();	/* Schedule an update	*/
			aa_ResetAreas ();
		}
		else
			mc_ParamChange();
		return;
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
	if (((pa->pa_flags & F_NOREDRAW) == 0) && ! MovieMode)
	{
		pc_PlotHandler ();
		return;
	}
	if(MovieMode)
		mc_ParamChange();
/*
 * If it's a require, set up to do the load now.
 */
	if (! strcmp (pa->pa_param, "require"))
		Eq_AddEvent (PWhenever, DoRequires, 0, 0, Bounce);
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
		pc_DoTrigger (trigger, "global", 0);
/*
 * Now go through and find the minor updates for each component.
 */
	comps = pd_CompList (Pd);
	for (i = 1; comps[i]; i++)
		if (pd_Retrieve (Pd, comps[i], "trigger", trigger,SYMT_STRING))
			pc_DoTrigger (trigger, comps[i], i);
}




static void
pc_DoTrigger (trigger, comp, index)
char *trigger, *comp;
int index;
/*
 * Cope with a trigger condition.
 */
{
	int seconds;
	PlatformId pid;
	char platform[80];
/*
 * Try to interpret the trigger as a time.
 */
	if (seconds = pc_TimeTrigger (trigger))
	{
		pc_SetTimeTrigger (seconds, comp);
		return;
	}
/*
 * Allow "platform" as a trigger, meaning it should look at the
 * platform in this component.
 */
	if (! strcmp (trigger, "platform"))
	{
		if (! pda_ReqSearch (Pd, comp, "platform", NULL, platform,
				SYMT_STRING))
			return;
		trigger = platform;
	}
/*
 * Now look up our platform and request notifications.
 */
	if ((pid = ds_LookupPlatform (trigger)) != BadPlatform)
		ds_RequestNotify (pid, index, pc_Notification);
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
	else if (trigger[0] == 'h' && trigger[1] == '\0')
		return (seconds*60*60);
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
	UItime t;
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
UItime *t;
char *comp;
/*
 * Deal with a timer alarm.
 */
{
	TC_UIToZt (t, &PlotTime);
	msg_ELog (EF_DEBUG, "Plot alarm at %d %d", t->ds_yymmdd, t->ds_hhmmss);
	Eq_AddEvent (PDisplay, pc_Plot, comp, 1 + strlen (comp),
		(strcmp (comp, "global") ? Bounce : OverrideQ));
}




static void
pc_Notification (pid, index, t)
PlatformId pid;
int index;
UItime *t;
/*
 * A data available notification has arrived.  Only do global updates for
 * the moment, until we decide how we're passing the component through
 * the PARAM field....
 */
{
	char **comps;
	char rep[40];
	int reroute;
	bool global;
/*
 * Look at times and components.  Florida change: Use the current time
 * for the plot, instead of the data time -- that way things like boundaries
 * plot right.
 */
	/* PlotTime = *t; */
	tl_Time (&PlotTime);
	comps = pd_CompList (Pd);
	msg_ELog (EF_DEBUG, "Data available on %s (c: %s) at %d %d", 
		ds_PlatformName (pid), comps[index], t->ds_yymmdd,
		t->ds_hhmmss);
/*
 * Look for cases in which global triggers should be rerouted into updates;
 * this is to make time series plots work right.
 */
	if (pd_Retrieve (Pd, "global", "plot-type", rep, SYMT_STRING) &&
		pda_Search (Pd, "global", "reroute-global-updates",
				rep, (char *) &reroute, SYMT_INT))
	{
		index = reroute;
		msg_ELog (EF_DEBUG, " (reroute to %d)", index);
	}
	else if (pda_Search (Pd, comps[index], "trigger-global", NULL,
			(char *) &global, SYMT_BOOL) && global)
		index = 0;
	Eq_AddEvent (PDisplay, pc_Plot, comps[index],
			strlen (comps[index]) + 1, 
			index == 0 ? Override : OverrideQ);
}




static void
pc_Plot (comp)
char	*comp;
/*
 * Actually execute the plot of the given component
 */
{
	msg_ELog (EF_DEBUG, "pc_Plot (%s)", comp); 
	px_PlotExec (comp);

	if (! MovieMode)
		I_ColorIcons (comp);
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


static void
pc_SetCoords (x0, y0, x1, y1)
float x0, y0, x1, y1;
/*
 * Actually store these coords.
 */
{
	extern void eq_ReturnPD ();
	float temp;
/*
 * Put the new stuff in the PD.
 */
	temp = (float) x0;
	pd_Store (Pd, "global", "x-min", (char *) &temp, SYMT_FLOAT);
	temp = (float) y0;
	pd_Store (Pd, "global", "y-min", (char *) &temp, SYMT_FLOAT);
	temp = (float) x1;
	pd_Store (Pd, "global", "x-max", (char *) &temp, SYMT_FLOAT);
	temp = (float) y1;
	pd_Store (Pd, "global", "y-max", (char *) &temp, SYMT_FLOAT);
/*
 * Now get a replot done, and ship the PD back to the display manager.
 */
	fc_InvalidateCache ();
	Eq_AddEvent (PDisplay, pc_PlotHandler, NULL, 0, Override);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
	pdm_ScheduleUpdate ();
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
		usy_g_symbol (Vtable, "boxx0", &type, &v); 
		x0 = XUSER(v.us_v_int);
		usy_g_symbol (Vtable, "boxy0", &type, &v); 
		y0 = YUSER(v.us_v_int);
		usy_g_symbol (Vtable, "boxx1", &type, &v); 
		x1 = XUSER(v.us_v_int);
		usy_g_symbol (Vtable, "boxy1", &type, &v); 
		y1 = YUSER(v.us_v_int);
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




pc_Zoom (cmds)
struct ui_command *cmds;
/*
 * Push a new set of coords onto the zoom stack.
 */
{
	SValue v;
	int type;
	float x0, y0, x1, y1;
/*
 * Get the box coordinates from the "drawbox" command
 * and set the zoom box
 */
	usy_g_symbol (Vtable, "boxx0", &type, &v); 
	x0 = (float)v.us_v_int / (float)GWWidth(Graphics);
	usy_g_symbol (Vtable, "boxy0", &type, &v); 
	y0 = 1.0 - (float)v.us_v_int / (float)GWHeight(Graphics);
	usy_g_symbol (Vtable, "boxx1", &type, &v); 
	x1 = (float)v.us_v_int / (float)GWWidth(Graphics);
	usy_g_symbol (Vtable, "boxy1", &type, &v); 
	y1 = 1.0 - (float)v.us_v_int / (float)GWHeight(Graphics);
        lc_Zoom ( x0, x1, y1, y0 );
/*
 * Force a replot and send the PD back to the display manager
 */
	fc_InvalidateCache ();
	Eq_AddEvent (PDisplay, pc_PlotHandler, NULL, 0, Override);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
	pdm_ScheduleUpdate ();
}




pc_UnZoom () 
/*
 * Pop one level of coords off the stack.
 */
{
/*
 * Try to unzoom a level and force a replot if we actually do
 */
	if (lc_UnZoom (1))
	{
		fc_InvalidateCache ();
		Eq_AddEvent (PDisplay, pc_PlotHandler, NULL, 0, Override);
		Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
		pdm_ScheduleUpdate ();
	}
}



void
pc_TriggerGlobal()
{
    Eq_AddEvent (PDisplay, pc_Plot, "global", 7, Override);
}
