/*
 * Window plot control routines.
 */
static char *rcsid = "$Id: PlotControl.c,v 1.1 1990-05-07 16:08:07 corbet Exp $";

# include <ctype.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include "../include/pd.h"
# include "GraphProc.h"
# include "EventQueue.h"




/*
 * Local stuff.
 */
# ifdef __STDC__
	static int pc_TimeTrigger (char *);
	static void pc_SetTimeTrigger (int, char *);
	static void pc_TimerAlarm (time *, char *);
# else
	static int pc_TimeTrigger ();
	static void pc_SetTimeTrigger ();
	static void pc_TimerAlarm ();
# endif



void
pc_SetUp ()
/*
 * Get all set up to deal with this plot.
 */
{
	char pmode[80];
/*
 * Cancel all existing timer requests.
 */
 	tl_AllCancel ();
/*
 * Figure out our plot mode.
 */
	PlotMode = RealTime;
	if (! pda_Search (Pd, "global", "plot-mode", 0, pmode, SYMT_STRING))
		msg_log ("No plot mode given -- Real Time used");
	else if (! strcmp (pmode, "real-time"))
		PlotMode = RealTime;
	else if (! strcmp (pmode, "history"))
		PlotMode = History;
	else
		msg_log ("Unknown plot mode '%s' -- Real time used", pmode);
/*
 * If we are running in real time mode, and the window is visible,
 * get our triggers set up.
 */
	if (PlotMode == RealTime && WindowState == UP)
		pc_SetUpTriggers ();
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
		msg_log ("No global trigger specified!");
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





pc_DoTrigger (trigger, param)
char *trigger, *param;
/*
 * Cope with a trigger condition.
 */
{
	int seconds;
/*
 * Try to interpret the trigger as a time.
 */
	if (seconds = pc_TimeTrigger (trigger))
		pc_SetTimeTrigger (seconds, param);
	else
		msg_log ("Funky trigger time: '%s' param %s", trigger, param);
}





static int pc_TimeTrigger (trigger)
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
 * If our trigger time is less than one minute, arrange things to line
 * up with the minute boundary.
 */
	if (seconds < 60)
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
	tl_AddAbsoluteEvent (pc_TimerAlarm, param, &t, seconds*INCFRAC);
}




static void
pc_TimerAlarm (t, param)
time *t;
char *param;
/*
 * Deal with a timer alarm.
 */
{
	PlotTime = *t;
	Eq_AddEvent (PDisplay, px_PlotExec, param, 0, Augment);
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
	Eq_AddEvent (PWhenever, eq_ResetAbort, 0, 0, Bounce);
/*
 * Cancel any timer requests.
 */
	tl_AllCancel ();
}





void
pc_DoPlot (component, clen)
char *component;
int clen;
/*
 * This is the routine which actually performs a display update.  If a
 * component is given, only a partial update is done on that component;
 * otherwise a full redisplay is done.
 */
{
	char **comps;
	int i;

	if (! strcmp (component, "global"))
	{
# ifdef notdef
		pl_NewPlot ();
		comps = pd_CompList (Pd);
		for (i = 0; comps[i]; i++)
			pl_AddComponent (comps[i]);
# endif
		msg_log ("Full replot at %d %d", PlotTime.ds_yymmdd,
			PlotTime.ds_hhmmss);
	}
	else
		msg_log ("Update %s at %d %d", component, PlotTime.ds_yymmdd,
			PlotTime.ds_hhmmss);
# ifdef notdef
			pl_UpdateComponent (comps[i]);
# endif
}
