/*
 * Display manager routines for time management.
 */
# include <stdio.h>
# include <unistd.h>
# include <string.h>
# include <errno.h>

# include <config.h>
# include <defs.h>
# include <twidget.h>
# include <timer.h>

# include <dm.h>
# include "dm_vars.h"
# include "dm_cmds.h"

RCSID ("$Id: dm_time.c,v 2.6 1995-09-07 21:29:26 granger Exp $")

#define TIME_FILE_LEN CFG_FILEPATH_LEN

/*
 * History mode control.
 */
static bool ForceHistory = FALSE;
static bool HistoryMode = FALSE;
static bool AutoAdvance = FALSE;
static ZebTime HistoryTime;
static char TimeFile[TIME_FILE_LEN] = { '\0' };
static bool InitTW = FALSE;

/*
 * Private prototypes
 */
static void dt_SetTimeMode FP ((struct cf_window *who, int history, 
				ZebTime *when));
static int dt_TWCallback FP ((int mode, ZebTime *t, int control_all,
			      char *window_name));
static void dt_TWHelp FP ((void));
static void dt_InitTW FP ((void));
static void dt_WriteTimeFile FP ((void));
static void dt_ReadTimeFile FP ((void));


/*ARGSUSED*/
static int
dt_AutoAdvance (symbol, arg, op, oldtype, oldvalue, newtype, newvalue)
char *symbol;
int arg, op;
int oldtype;
union usy_value *oldvalue;
int newtype;
union usy_value *newvalue;
{
	tw_AutoAdvance (newvalue->us_v_int);
	return (0);
}


void
dt_Init ()
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "forcehistory", &ForceHistory, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "autoadvance", &AutoAdvance, SYMT_BOOL, 0);
	usy_daemon (vtable, "autoadvance", SOP_WRITE, dt_AutoAdvance, NULL);
	usy_c_indirect (vtable, "timefile", TimeFile, SYMT_STRING, 
			TIME_FILE_LEN);
	tw_DefTimeWidget (dt_TWCallback, "System Time Control");
	aw_DefAlarmWidget ();
}



static void
dt_ReadTimeFile ()
{
#	define BUFLEN 128
	FILE *in;
	ZebTime zt;
	int visited;
	int n;
	char buffer[BUFLEN];
	char stime[BUFLEN];
	char note[BUFLEN];
	char *buf;

	if (! TimeFile[0])
		return;
	in = fopen (TimeFile, "r");
	if (! in)
	{
		msg_ELog (EF_PROBLEM, "could not read time file %s: error %d",
			  TimeFile, errno);
		return;
	}
	msg_ELog (EF_DEBUG, "reading time file: %s", TimeFile);
	while (fgets (buffer, BUFLEN, in))
	{
		buf = buffer;
		if (sscanf (buf, "%s %n", stime, &n) == 1 &&
		    TC_DecodeTime (stime, &zt))
		{
			buf += n;
			if (sscanf (buf, "%d %[^\n]", &visited, note) == 2)
				tw_AddVisited (&zt, visited, note);
			else if (sscanf (buf, "%d", &visited) == 1)
				tw_AddVisited (&zt, visited, "");
			else if (sscanf (buf, "%[^\n]", note) == 1)
				tw_AddHotTime (&zt, note);
			else
				tw_AddHotTime (&zt, "");
		}
		if (buf[strlen(buf) - 1] != '\n')
		{
			int c;
			msg_ELog (EF_PROBLEM, "line too long in %s", TimeFile);
			while ((c = fgetc (in)) && (c != '\n'))
				/* find next line */;
		}
#ifdef DEBUG_TIME
		buf[strlen(buf) - 1] = '\0';
		printf ("After reading: %s\n", buffer);
		dt_WriteTimeFile ();
#endif
	}
	fclose (in);
}



static void
dt_WriteTimeFile ()
{
	FILE *out;
	int n, i;
	const HotTime *ht;

#ifndef DEBUG_TIME
	if (! TimeFile[0])
		return;
	out = fopen (TimeFile, "w");
	if (! out)
	{
		msg_ELog (EF_PROBLEM, "could not write time file %s: error %d",
			  TimeFile, errno);
		return;
	}
#endif
	ht = tw_ListHotTimes (&n);
	msg_ELog (EF_DEBUG, "writing %d hot times to file: %s", n, TimeFile);
	for (i = 0; i < n; ++i)
	{
#ifdef DEBUG_TIME
		ui_printf ("%s %d %s\n", TC_AscTime(&ht[i].ht_zt, TC_Full),
			   ht[i].ht_visited, ht[i].ht_label);
#else
		fprintf (out, "%s %d %s\n", TC_AscTime(&ht[i].ht_zt, TC_Full),
			 ht[i].ht_visited, ht[i].ht_label);
#endif
	}
#ifndef DEBUG_TIME
	fclose (out);
#endif
}




static void
dt_InitTW ()
/*
 * Set the hot times and callbacks.  If a time file has been specified, 
 * read it and send the times to the time widget.  Sync AutoAdvance setting.
 */
{
	if (InitTW)
		return;
	else
		InitTW = TRUE;
	dt_ReadTimeFile ();
	tw_AddHTAddCallback (dt_WriteTimeFile);
	tw_AddHTDeleteCallback (dt_WriteTimeFile);
	tw_AddHelpCallback (dt_TWHelp);
	tw_AutoAdvance (AutoAdvance);
}



void
dt_SetWindowNames ()
/*
 * Send the time widget a list of the windows in the current
 * configuration.
 */
{
	struct config *cfg;
	char *names[MAXWIN];
	int i, nwin;

	nwin = 0;
	cfg = dg_CurrentConfig ();
	for (i = 0; cfg && (i < cfg->c_nwin); ++i)
	{
		if (IsGraphic (cfg->c_wins[i]))
		{
			names[nwin++] = cfg->c_wins[i]->cfw_name;
		}
	}
	tw_SetWindowNames (nwin, names);
}



void
dt_History (cmds)
struct ui_command *cmds;
/*
 * Throw one or more windows into history mode.
 */
{
	bool all = (cmds->uc_ctype == UTT_KW);
	struct cf_window *dwin = NULL;
	ZebTime when;
/*
 * If necessary, look up the window.
 */
	dt_InitTW ();
	if (! all)
	{
		if (cmds->uc_vptype != SYMT_STRING)
		{
			msg_ELog (EF_PROBLEM,
				"HISTORY error -- win name must be string");
			return;
		}
		dwin = dg_CurrentWindow (UPTR (*cmds));
		if (! dwin)
		{
			msg_ELog(EF_PROBLEM, "HISTORY on unavailable win '%s'",
				UPTR (*cmds));
			return;
		}
	}
/*
 * Update the masses.
 */
	if (cmds[1].uc_vptype != SYMT_DATE)
	{
		msg_ELog (EF_PROBLEM,
			"HISTORY error -- time must be a date expression");
		return;
	}
	TC_UIToZt (&UDATE (cmds[1]), &when);
	dt_SetTimeMode (all ? 0 : dwin, TRUE, &when);
/*
 * If this is a global change, update the time widget as well.
 */
	if (all)
		tw_SetTime (&when);
}




void
dt_Realtime (cmds)
struct ui_command *cmds;
/*
 * Throw one or more windows into real time mode.
 */
{
	bool all = (cmds->uc_ctype == UTT_KW);
	struct cf_window *dwin = NULL;
/*
 * If necessary, look up the window.
 */
	dt_InitTW ();
	if (! all)
	{
		if (cmds->uc_vptype != SYMT_STRING)
		{
			msg_log ("RT error -- win name must be string");
			return;
		}
		dwin = dg_CurrentWindow (UPTR (*cmds));
		if (! dwin)
		{
			msg_log ("RT on unavailable win '%s'", UPTR (*cmds));
			return;
		}
	}
/*
 * Tweak the mode.
 */
	dt_SetTimeMode (all ? 0 : dwin, FALSE, 0);
/*
 * It is debatable whether the time widget should be made to reflect the
 * real-time system time, as it is when setting a history.  For now we
 * assume the user will want to keep the history time to go back to later.
 */
}





static int
dt_TWCallback (mode, t, control_all, window)
int mode;
ZebTime *t;
int control_all;
char *window;		/* NULL for all, else the window name */
{
	struct cf_window *win = NULL;

	if (! window || ((win = dg_CurrentWindow (window))))
	{
		dt_SetTimeMode (win, (mode == History), t);
		return (1);
	}
	else
	{
		msg_ELog (EF_PROBLEM, "%s: window '%s' not in current config",
			  "time control", window);
		return (0);
	}
}



static void
dt_TWHelp ()
{
	ui_perform ("help historytime");
}



static void
dt_SetTimeMode (who, history, when)
struct cf_window *who;
int history;
ZebTime *when;
/*
 * Send out the time mode to one or more processes.  WHO is the target, 
 * unless it is null, in which case the time is sent to everybody.  HISTORY
 * is true if the process is to be put in history mode.  In the HISTORY 
 * case, WHEN is used as the history time.
 */
{
	struct dm_history dmh;
/*
 * Tweak up a message to send out.
 */
	if (history)
	{
	   	dmh.dmm_type = DM_HISTORY;
		dmh.dmm_time = *when;
	}
	else
	   	dmh.dmm_type = DM_REALTIME;
/*
 * Ship it out.
 */
	if (who)
		dmsg_SendWindow (who, &dmh, sizeof (dmh));
	else
		dmsg_Broadcast (GroupName, &dmh, sizeof(dmh));
/*
 * If this is a global change, then set our recordkeeping variables.
 */
	if (! who && (HistoryMode = history))
		HistoryTime = *when;
}





void
dt_SetTime (when)
UItime *when;
/*
 * Force pseudo real time mode at this time.
 */
{
	struct tm_prt prt;
	
	dt_InitTW ();
	prt.tr_type = TR_PRT;
	/* prt.tr_time = *when; */
	TC_UIToZt (when, &prt.tr_time);
	prt.tr_scale = 1;
	dmsg_SendTimer (&prt, sizeof (prt));
}




void
dt_SendTime (win)
struct cf_window *win;
{
	dt_InitTW ();
	if (ForceHistory && HistoryMode)
		dt_SetTimeMode (win, TRUE, &HistoryTime);
}

