/*
 * The zeb display manager.
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
# include <stdio.h>
# include <unistd.h>
# include <sys/types.h>
# include <time.h>
# include <math.h>
# include <fcntl.h>
# include <signal.h>
# include <errno.h>
# include <X11/Intrinsic.h>

# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <timer.h>
# include <DataStore.h>
# include <config.h>
# include <copyright.h>

#ifdef SUPPORT_XHELP
# include <xhelp.h>
#endif

# include <dm.h>
# include "dm_vars.h"
# include "dm_cmds.h"

MAKE_RCSID ("$Id: dm.c,v 2.76 2001-01-16 22:27:34 granger Exp $")

/*
 * Pick a help browser.
 */
#ifdef SUPPORT_XHELP
static void CallXHelp ();
static zbool UseXHelp =
# if UseMosaic
			FALSE;
# else
			TRUE;
# endif
#endif /* SUPPORT_XHELP */

/*
 * Definitions of globals.
 */
stbl Bmaps;
ButtonMap *Default_map;	/* The default button map	*/

/*
 * Non-zero when we've entered test mode, which prevets the actual
 * forking of processes and sending messages to windows.  Instead,
 * respond to message queries and print the messages that would
 * have been sent.
 */
int TestMode = 0;

/*
 * Non-zero when running in "backwards compatible" singleton mode.
 * Process and window names are identical and map 1-1, and the names
 * are only unique while a single display manager is running.
 * Defaults to non-zero and can be turned off with "-multiple" option.
 */
int SingletonMode = 1;

/*
 * Our unique group name for graphics processes
 */
char GroupName[ CFG_MSGNAME_LEN ] = { '\0' };

/*
 * Top level widget and display.
 */
Display *Dm_Display;
static Widget Top;
static char **Argv;		/* Save our args for a potential exec */

/*
 * Is sound enabled?
 */
static zbool SoundEnabled = FALSE;

/*
 * Do we restart windows which die?
 */
static zbool Restart = TRUE;

/*
 * What kind of computer is this, anyway?
 */
char SystemType[12] = 
# if defined(SYSV) || defined(SVR4) || defined (__osf__)
	"sysv";
# else
	"bsd";
# endif



/*
 * Forward routines.
 */
static void do_wbounds FP ((struct cf_window *, struct dm_rq_wbounds *));
int SEChange FP ((char *, int, int, int, SValue *, int, SValue *));
static void ForceRestart FP ((struct cf_window *, char *pcname));
static void dmgr_message FP ((char *from, struct dm_msg *dmsg));
static int dm_dispatcher FP ((int, struct ui_command *));
static int dm_msg_handler FP ((Message *));
static void mh_message FP ((struct message *msg));
static void EnterPosition FP ((struct ui_command *));
static void DieWindow FP ((struct cf_window *win));
static void KillWindow FP ((char *));
static void RestartWin FP ((struct cf_window *win, int force));
static void ProcessDeath FP ((char *client));
static int dm_shutdown FP ((void));
static int dm_cycle FP ((void));
static int WaitForDeath FP ((struct message *msg, void *param));
static int RealPlatform FP ((int, SValue *, int *, SValue *, int *));
static int HasData FP ((int, SValue *, int *, SValue *, int *));
static void MakeWindowList FP ((char *));
static void MakeDefaultMap FP ((void));
static void FreeButtonMaps FP ((void));
static int FreeBM FP ((char *name, int type, union usy_value *v, int));
static int AddToList FP ((char *, int, SValue *, long));
static void newpd FP ((char *window, char *pdesc));
static void send_param FP ((struct cf_window *win, char *comp, char *param,
			    char *value));
static void exchange FP ((char *cwin1, char *cwin2));
static void parameter FP ((char *name, char *comp, char *param, char *value));
static void z_remove FP ((char *pdn, char *comp));
static void add FP ((char *pdn, char *comp, char *dest, int position));
static void exec_button FP ((struct cf_window *win, struct dm_event *dme));



static void
usage (prog)
char *prog;
{
	printf ("usage: %s [options] [config-file]\n", prog);
	printf ("options:  [-name <name>][-test][-help][-single|-multiple]\n");
	printf ("where <name> is the message handle to use;\n");
	printf ("      -test enters test mode;\n");
	printf ("      -help shows this message;\n");
	printf ("      -single forces singleton mode, where process and\n");
	printf ("         window names are identical and only unique while\n");
	printf ("         a single display manager is running [default];\n");
	printf ("      -multiple selects multiple mode, where dm, group,\n");
	printf ("         and process names are unique so that more than\n");
	printf ("         one single display manager can be running;\n");
	printf ("      and <config-file> is a UI config file.\n");
	printf ("Options can be uniquely abbreviated to any length.\n");
}



static char *
Setup (argc, argv, msgname)
int argc;
char *argv[];
char *msgname;
/*
 * Parse the arg list for recognized options, and return a pointer
 * to the init file.
 */
{
	char buf[256];
	char display[256];
	int i = 1;
	char *config;

	buf[0] = 0;
	config = 0;
	while (i < argc)
	{
		int optlen = strlen (argv[i]);
		if (argv[i][0] != '-' || optlen < 2)
		{
			if (config)
			{
				printf ("specify only one config file\n");
				usage (argv[0]);
				exit (9);
			}
			config = argv[i];
		}
		else if (! strncmp (argv[i], "-name", optlen))
		{
			if (++i < argc)
				strcpy (buf, argv[i]);
			else
			{
				printf ("-name option needs argument\n");
				usage (argv[0]);
				exit (2);
			}
		}
		else if (! strncmp (argv[i], "-test", optlen))
		{
			TestMode = 1;
		}
		else if (! strncmp (argv[i], "-help", optlen))
		{
			usage (argv[0]);
			exit (0);
		}
		else if (! strncmp (argv[i], "-single", optlen))
		{
			SingletonMode = 1;
		}
		else if (! strncmp (argv[i], "-multiple", optlen))
		{
			SingletonMode = 0;
		}
		else
		{
			printf ("unrecognized option: %s\n", argv[i]);
			usage (argv[0]);
			exit (1);
		}
		++i;
	}
	if (! buf[0] && !SingletonMode)
	{
		char *number, *period;

		if (getenv ("DISPLAY"))
			strcpy (display, getenv ("DISPLAY"));
		else if (getenv ("HOST"))
			sprintf (display, "%s:0.0", getenv ("HOST"));
		else
			strcpy (display, "unix:0.0");
		/*
		 * Message mgr tables don't like :'s or long names
		 * so replace : with - and shorten host names.
		 */
		if ((number = (char *)strchr (display, ':')))
			*number++ = 0;
		if ((period = (char *)strchr (display, '.')))
			*period = 0;
		sprintf (buf, "%s-%s", display[0] ? display : "unix", 
			 number ? number : "0.0");
		/*
		 * Don't clutter the name with a pid unless we're running
		 * in test mode.
		 */
		if (TestMode)
			sprintf (buf+strlen(buf), "-%li", (long)getpid());
	}
	if (! SingletonMode)
	{
		strcpy (msgname, buf);
		sprintf (GroupName, "group-%s", msgname);
	}
	else
	{
		strcpy (msgname, "Displaymgr");
		strcpy (GroupName, "Graphproc");
	}
	return (config);
}


int
main (argc, argv)
int argc;
char *argv[];
{
	int type[4];
	char loadfile[100];
	char *initfile;
	stbl vtable;
	char msgname[CFG_MSGNAME_LEN];
/*
 * Read options from the command line and set our message name
 */
	initfile = Setup (argc, argv, msgname);
/*
 * Hook into the message handler.
 */
	if (! msg_connect (dm_msg_handler, msgname))
	{
		fprintf (stderr, "%s: could not connect to message manager\n",
			 argv[0]);
		exit (1);
	}
	if (TestMode)
		msg_ELPrintMask (EF_ALL);
	msg_DeathHandler (dm_shutdown);
	msg_SetQueryHandler (dg_Query);
	msg_join (MSG_CLIENT_EVENTS);
/*
 * Get the interface set up.
 */
	fixdir_t ("DMLOADFILE", GetLibDir (), "dm.lf", loadfile, ".lf");
	ui_init (loadfile, TRUE, FALSE);
	ui_setup ("DisplayMgr", &argc, argv, (char *) 0);
	SetupConfigVariables ();
	{ char rpath[CFG_FILEPATH_LEN];
	sprintf (rpath, "%s/dmlib", GetLibDir());
	SetRequirePath (rpath);
	}
	cp_SetupCmdProto ();
	if (! TestMode)
		ds_Initialize ();
#ifdef notyet
	else
		ds_Standalone ();
#endif
/*
 * Command line functions.
 */
	type[0] = type[1] = type[2] = type[3] = SYMT_STRING;
	uf_def_function ("pdesc", 1, type, get_pd);
	uf_def_function ("active", 1, type, is_active);
	uf_def_function ("pd_param", 4, type, pd_param);
	uf_def_function ("pdparam", 3, type, pd_param);
	uf_def_function ("pd_defined", 3, type, pd_defined);
	uf_def_function ("pd_complist", 1, type, pd_complist);
	uf_def_function ("nvalue", 3, type, nvalue);
	uf_def_function ("realplatform", 1, type, RealPlatform);
	uf_def_function ("hasdata", 1, type, HasData);
	type[1] = SYMT_INT;
	uf_def_function ("nthcomp", 2, type, NthComponent);
/*
 * Indirect variables.
 */
	vtable = usy_g_stbl ("ui$variable_table");
	usy_c_indirect (vtable, "dm$config", Cur_config, SYMT_STRING, MAXNAME);
	usy_c_indirect (vtable, "soundenabled", &SoundEnabled, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "restart", &Restart, SYMT_BOOL, 0);
	usy_daemon (vtable, "soundenabled", SOP_WRITE, SEChange, 0);
	usy_c_indirect (vtable, "systemtype", SystemType, SYMT_STRING, 12);
#ifdef SUPPORT_XHELP
	usy_c_indirect (vtable, "usexhelp", &UseXHelp, SYMT_BOOL, 0);
#endif
/*
 * Create our symbol tables.
 */
	Bmaps = usy_c_stbl ("ButtonMaps");
	dt_Init ();	/* time handling */
	dc_Init ();	/* color tables */
	dp_Init ();	/* processes */
	if (SingletonMode)
		dp_UseWindowNames (TRUE);
	dg_Init ();	/* configurations */
	dm_Init ();	/* help */
/*
 * Create the default button map.
 */
	MakeDefaultMap ();
/*
 * Watch for incoming messages.
 */
	tty_watch (msg_get_fd (), (void (*)()) msg_incoming);
# ifdef titan
/*
 * Hook into the dialbox.
 */
	dlb_Init ();
# endif
/*
 * Push into window mode, and get our display.
 */
 	uw_ForceWindowMode ((char *) 0, &Top, (XtAppContext *) 0);
	Dm_Display = XtDisplay (Top);
/*
 * If a file appears on the command line, open it.
 */
	if (initfile)
		ut_open_file (initfile, TRUE);
/*
 * Interpret commands.
 */
	Argv = argv;
	ui_get_command ("initial", "DM>", dm_dispatcher, 0);
	dm_shutdown ();
	return (0);
}




static int
dm_dispatcher (arg, cmds)
int arg;
struct ui_command *cmds;
/*
 * Deal with a display manager command.
 */
{
	char winname[40];
	union usy_value nv;
	struct cf_window *win;

	switch (UKEY (*cmds))
	{
	   case DMC_CONFIG:
	   	def_config (cmds + 1);
		break;
	/*
	 * Begin a plot description
	 */
	   case DMC_BEGINPD:
		def_pd (cmds + 1);
		break;

	   case DMC_DISPLAY:
	   	dg_Display (cmds + 1);
		dt_SetWindowNames ();
		break;

	   case DMC_LIST:
	   	dg_List (cmds[1].uc_ctype == UTT_END ? NULL : UPTR (cmds[1]));
		break;

	   case DMC_NEWPD:
		if (cmds[1].uc_vptype != SYMT_STRING || 
		    cmds[2].uc_vptype != SYMT_STRING)
		{
			ui_error ("both expressions must be strings; %s",
				  "enclose string constants in quotes");
		}
	   	newpd (UPTR (cmds[1]), UPTR (cmds[2]));
		break;

	   case DMC_BEEP:
	   	ui_printf ("\007");
		break;

	   case DMC_BUTTONMAP:
	   	def_bmap (UPTR (cmds[1]));
		break;

	   case DMC_EXCHANGE:
		if (cmds[1].uc_vptype != SYMT_STRING || 
		    cmds[2].uc_vptype != SYMT_STRING)
		{
			ui_error ("both expressions must be strings; %s",
				  "enclose string constants in quotes");
		}
	   	exchange (UPTR (cmds[1]), UPTR (cmds[2]));
		break;

	   case DMC_PDLOAD:
	   	pdload (UPTR (cmds[1]), UPTR (cmds[2]));
		break;

	   case DMC_PDDIR:
	   	pddir (UPTR (cmds[1]));
		break;
   	
	   case DMC_PARAMETER:
		parameter (UPTR (cmds[1]), UPTR (cmds[2]), UPTR (cmds[3]),
			UPTR (cmds[4]));
		break;

	   case DMC_REMOVE:
	   	z_remove (UPTR (cmds[1]), UPTR (cmds[2]));
		break;

	   case DMC_ADD:
	   	add (UPTR (cmds[1]), UPTR (cmds[2]), UPTR (cmds[3]),
			cmds[4].uc_ctype == UTT_END ? 0 : UINT (cmds[4]));
		break;

	   case DMC_HISTORY:
	   	dt_History (cmds + 1);
		break;

	   case DMC_REALTIME:
	   	dt_Realtime (cmds + 1);
		break;

	   case DMC_COLORTABLE:
	   	dc_Define (UPTR (cmds[1]));
		break;

	   case DMC_DIAL:
# ifdef titan
	   	dlb_Define (cmds + 1);
# endif
		break;

	   case DMC_TIME:
	   	dt_SetTime (&UDATE (cmds[1]));
		break;

	   case DMC_PICKWIN:
		/*
		 * Don't change the variable unless PickWin() succeeds
		 */
	   	if (PickWin (winname))
		{
			nv.us_v_ptr = winname;
			usy_s_symbol (usy_g_stbl ("ui$variable_table"), 
				      UPTR (cmds[1]), SYMT_STRING, &nv);
		}
		break;

	   case DMC_SHUTDOWN:
	   	dm_shutdown ();
		break;

	   case DMC_CYCLE:
		dm_cycle ();
		break;

	   case DMC_SOUND:
		if (SoundEnabled)
		   	DoSound (UPTR (cmds[1]));
		break;

	   case DMC_SHELL:
	   	if (cmds[1].uc_vptype != SYMT_STRING)
			msg_ELog (EF_PROBLEM, "Non-string shell command");
		else
			system (UPTR (cmds[1]));
		break;

	   case DMC_RESTART:
		win = dg_CurrentWindow (UPTR (cmds[1]));
		if (! win)
			ui_error ("%s not in current config", UPTR(cmds[1]));
		if (cmds[2].uc_ctype != UTT_END)
			ForceRestart (win, UPTR (cmds[2]));
		else
			ForceRestart (win, NULL);
		break;
	/*
	 * Saving of configurations.
	 */
	   case DMC_CFGSAVE:
		dg_SaveConfig (Cur_config, cmds[1].uc_ctype == UTT_END ?
			       Cur_config : UPTR (cmds[1]), /*update*/ TRUE);
		break;
	/*
	 * With one arg, re-save the config by the given name.  The optional
	 * second arg is for saving the named config with a different name.
	 */
	   case DMC_CFGCONVERT:
		dg_SaveConfig (UPTR (cmds[1]), cmds[2].uc_ctype == UTT_END ?
			       UPTR(cmds[1]) : UPTR(cmds[2]), /*update*/FALSE);
		break;
	/*
	 * New windows, configs, and process prototypes.
	 */
	   case DMC_NEWCONFIG:
	   	dg_PutConfigAs (UPTR (cmds[1]), cmds[2].uc_ctype == UTT_END ?
				"template" : UPTR (cmds[2]));
		break;

	   case DMC_NEWWINDOW:
	   	dg_PutNewWindow (DEFAULT_PROCESS, cmds + 1);
		dt_SetWindowNames ();
		break;
	   case DMC_NEWGRAPHIC:
	   	dg_PutNewWindow (UPTR(cmds[1]), cmds + 2);
		dt_SetWindowNames ();
		break;
	   case DMC_PROTOTYPE:
		Prototype (cmds + 1);
		break;
	/*
	 * Position entry.
	 */
	   case DMC_ENTER:
	   	EnterPosition (cmds + 1);
		break;
	/*
	 * Call for help.
	 */
	   case DMC_HELP:
#ifdef SUPPORT_XHELP
	        if (UseXHelp)
			CallXHelp (cmds);
		else
#endif
			dm_Help (cmds[1].uc_ctype == UTT_END ? "index.html"
				 : UPTR (cmds[1]));
		break;
	/*
	 * Do away with a window.
	 */
	   case DMC_KILL:
	   	KillWindow (UPTR (cmds[1]));
		dt_SetWindowNames ();
		break;
	/*
	 * Write a named plot description to the terminal as ASCII
	 */
	   case DMC_PDSHOW:
		WritePD (UPTR (cmds[1]), NULL);
		break;
	/*
	 * Write a named plot description to a named file as ASCII
	 */
	   case DMC_PDWRITE:
		WritePD (UPTR (cmds[1]), UPTR (cmds[2]));
		break;
	/*
	 * Write a named plot description (or window) using a different name
	 */
	   case DMC_PDSTORE:
		StorePD (UPTR (cmds[1]), UPTR(cmds[2]), UPTR(cmds[3]));
		break;
	/*
	 * Copy a named plot description (or window) into a new one
	 */
	   case DMC_PDCOPY:
		CopyPD (UPTR (cmds[1]), UPTR (cmds[2]));
		break;
	/*
	 * Lift a component from a named plot description (or window) and
	 * copy it into a new component description with the given name.
	 */
	   case DMC_PDLIFT:
		CopyComp (UPTR (cmds[1]), UPTR (cmds[2]), UPTR (cmds[3]));
		break;
	/*
	 * Drop a component from one named plot description (or window) and
	 * into a second component of another named plot description.
	 */
	   case DMC_PDDROP:
		MergeComp (UPTR (cmds[1]), UPTR (cmds[2]), 
			   UPTR (cmds[3]), UPTR (cmds[4]));
		break;
	/*
	 * They want a list of windows.
	 */
	    case DMC_WINDOWLIST:
		MakeWindowList (UPTR (cmds[1]));
		break;
	/*
	 * Want to see query information on the terminal
	 */
	   case DMC_QUERY:
		dg_Query (/* from ourselves */ NULL);
		break;

	/*
	 * Deal with a require.
	 */
	   case DMC_REQUIRE:
		Require (UPTR (cmds[1]));
		break;

	   default:
	   	ui_error ("(BUG): Unknown keyword: %d\n", UKEY (*cmds));
	}
	return (TRUE);
}




static int
dm_msg_handler (msg)
struct message *msg;
/*
 * Deal with incoming messages.
 */
{
	switch (msg->m_proto)
	{
	/*
	 * Display manager stuff.
	 */
	   case MT_DISPLAYMGR:
	   	dmgr_message (msg->m_from, (struct dm_msg *) msg->m_data);
		return (0);
	/*
	 * Stuff from the message handler itself.
	 */
	   case MT_MESSAGE:
	   	mh_message (msg);
		break;
	/*
	 * We use timer events at times.
	 */
	   case MT_TIMER:
	   	tl_DispatchEvent ((struct tm_time *) msg->m_data);
		break;
	/*
	 * Everything else we don't know about.
	 */
	   default:
	   	msg_ELog (EF_PROBLEM, "Funky message type %d in DM",
			msg->m_proto);
	}
	return (0);
}



static void
mh_message (msg)
struct message *msg;
/*
 * Deal with a MESSAGE protocol msg.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
	struct mh_clientevent *client;

	switch (tm->mh_type)
	{
	   case MH_SHUTDOWN:
	   	ui_printf ("Message handler shutdown -- I quit!\n");
		dm_ExitHelp ();
		ui_finish ();
		exit (1);
	/*
	 * For client events, we are really only interested in deaths.
	 */
	   case MH_CLIENT:
		client = (struct mh_clientevent *) msg->m_data;
		if (client->mh_evtype == MH_CE_DISCONNECT)
			ProcessDeath (client->mh_client);
		break;

	   default:
	   	ui_printf ("Unknown MESSAGE proto msg %d\n", tm->mh_type);
		break;
	}
}



static void
DieWindow (win)
struct cf_window *win;
/*
 * Tell a window's process to die.  There may still be hope for the window,
 * but we don't care about that here.
 */
{
	struct dm_msg dmsg;
/*
 * Send a DIE message to this window.
 */
	dmsg.dmm_type = DM_DIE;
	dmsg_SendWindow (win, &dmsg, sizeof (dmsg));
}



static void
ForceRestart (win, pcname)
struct cf_window *win;
char *pcname;
/*
 * Tell the window to die, and flag it to receive a brand new process whence
 * we receive its last words.  Optionally change the window's process class.
 * Lastly, don't count this death towards eternal damnation.
 */
{
	DieWindow (win);
	win->cfw_force_exec = TRUE;
	if (win->cfw_ncroak > 0)
		--win->cfw_ncroak;
/*
 * Take note of a requested change in the process class.
 */
	if (pcname)
	{
		strcpy (win->cfw_pcname, pcname);
	}
}




static void
KillWindow (who)
char *who;
/*
 * Terminate this poor process who is just trying to do his job make pictures
 * feed his child processes and all that stuff.  And don't let it ever
 * come back.
 */
{
	struct cf_window *win = dg_CurrentWindow (who);
	struct config *cfg = dg_LookupConfig (Cur_config);

	if (! win)
	{
	/*
	 * Needs to be an on-screen window.
	 */	
		msg_ELog (EF_PROBLEM, "(Kill) window %s nonexistent", who);
	}
	else
	{
	/*
	 * Tell the window to die, but remove it from the current
	 * configuration so that it won't really restart.  When
	 * ProcessDeath gets called, the process will have no window and
	 * its death will be ignored. 
	 */
		DieWindow (win);
		usy_z_symbol (Current, who);
		dg_DeleteWindow (cfg, win);
	}
}



static void
ProcessDeath (client)
char *client;
/*
 * A process has died.  If it was active, we may need to restart it.  If it
 * was not active, we need to delete all record of it.  If we have never
 * heard of this process, we are coldly indifferent to its death.
 */
{
#	define MAX_DEATHS 10
	struct config *cfg;
	struct cf_window *win;
	Process *proc;
	int active;
	int force;

	if (! (proc = dp_LookupProcess (client)))
		return;
	win = proc->p_cfw;
	active = (proc->p_state == P_ACTIVE);
	/*
	 * Delete the process that died.
	 * If the process was assigned a window, see if we should 'restart'
	 * the window by assigning it a new process.  A process without a 
	 * window gets dropped like a hot potato.  
	 */
	dp_DeleteProcess (proc);
	if (! win)
	{
		/*
		 * This could be the result of a 'kill', so it's not 
		 * necessarily an error, but it deserves recognition.
		 */
		msg_ELog (EF_INFO, "Process %s exited without a window",
			  client);
		return;
	}

	++win->cfw_ncroak;
	if (active && Restart && win->cfw_ncroak < MAX_DEATHS)
	{
		/*
		 * Find another process for the window.
		 */
		msg_ELog (EF_PROBLEM, 
			  "Win '%s' died -- restarting", win->cfw_name);
		force = win->cfw_force_exec;
		win->cfw_force_exec = FALSE;
		RestartWin (win, force);
	}
	else if (active)
	{
		/*
		 * We won't be restarting the process; the window is doomed.
		 * Remove the window from the current configuration.
		 */
		if (Restart)
			msg_ELog (EF_PROBLEM,
			  "Process %s: window %s died %d times -- I give up", 
			  client, win->cfw_name, MAX_DEATHS);
		else
			msg_ELog (EF_PROBLEM,
			  "Process %s: restarts disabled, window %s deleted",
			  client, win->cfw_name);
		/*
		 * Need to map the window to its config so that we can
		 * delete the window from the config.
		 */
		if (win && ((cfg = dg_FindOwner (win))))
		{
			if (cfg == dg_LookupConfig (Cur_config))
				usy_z_symbol (Current, win->cfw_name);
			dg_DeleteWindow (cfg, win);
		}
		dt_SetWindowNames ();
	}
	else
	{
		/*
		 * The window was not active or restarting is disabled.
		 * The process has already been deleted, but we'll keep
		 * the window in the configuration so that it can
		 * be assigned a process later.
		 */
		msg_ELog (EF_INFO,
			  "Process %s: [window %s] deceased, %s",
			  client, win->cfw_name,
			  (active) ? "and restarts disabled" :
			  "but was not active");
	}
}




static void
RestartWin (win, force)
struct cf_window *win;
int force;
/*
 * Give this window a new lease on life, but not necessarily a whole
 * new process.
 */
{
	struct config *cfg = dg_FindOwner (win);

	if (IsGraphic (win))
		win->cfw_graphic->g_tmpforce = TRUE;
	dg_SyncWindow (cfg, win, force);
}
	



static void
dmgr_message (from, dmsg)
char *from;
struct dm_msg *dmsg;
/*
 * Deal with a display manager message.
 *
 * The former "dm_message" but HP had their own designs on that name
 */
{
	struct dm_hello *dmh;
	struct dm_pdchange *dmp;
	struct cf_window *win;
	struct cf_graphic *g;
	raw_plot_description rpd;
	Process *proc;
/*
 * We only expect messages from active processes, i.e. processes realizing
 * windows in the current configuration.
 */
	proc = dp_LookupProcess (from);
	if (! proc)
	{
		msg_ELog (EF_PROBLEM, 
		  "whoa! dm message (type %d) from an unknown process %s", 
		  dmsg->dmm_type, from);
		return;
	}
	if (! proc->p_cfw)
	{
		msg_ELog (EF_PROBLEM, 
		  "process '%s' sent me a message (type %d) %s",
		  proc->p_name, dmsg->dmm_type, "when it has no window");
		return;
	}
	win = proc->p_cfw;
	if (! proc->p_state == P_ACTIVE)
	{
		msg_ELog (EF_PROBLEM, 
			  "msg %d from process '%s', window '%s': %s",
			  dmsg->dmm_type, proc->p_name, win->cfw_name,
			  "that process is not active in current config");
		return;
	}
/*
 * Now we know we have a process and its window from the current configuration
 */
	switch (dmsg->dmm_type)
	{
	/*
	 * A new window checking in.  Finalize the data structure, and
	 * configure the window.
	 */
	   case DM_HELLO:
		dmh = (struct dm_hello *) dmsg;   	
		msg_ELog (EF_DEBUG, "%s window '%s', process %s, win %#0lx",
			  "Hello received from", win->cfw_name, from, 
			  dmh->dmm_win);
		proc->p_win = dmh->dmm_win;
		dg_ConfigWindow (win);
		break;
	/*
	 * A client sending along its window ID after its already said
	 * hello and been configured.
	 */
	   case DM_WINDOW:
		dmh = (struct dm_hello *) dmsg;   	
		msg_ELog (EF_DEBUG, "%s from process '%s': %#0lx, %s %s", 
			  "Window ID received", from, dmh->dmm_win, 
			  "window named", win->cfw_name);
		proc->p_win = dmh->dmm_win;
		break;
	/*
	 * A button report
	 */
	   case DM_EVENT:
		exec_button (win, (struct dm_event *) dmsg);
		break;
	/*
	 * A color table request.
	 */
	   case DM_R_CTABLE:
		dc_TableRequest ((struct dm_ctr *) dmsg, from);
		break;
	/*
	 * A graphics process has had the temerity to change its own
	 * plot description.  Now we have to scramble to keep up with it.
	 */
	   case DM_PDCHANGE:
		msg_ELog (EF_DEBUG, "New PD from %s", from);
		g = win->cfw_graphic;
		pd_Release (g->g_pd);
		dmp = (struct dm_pdchange *) dmsg;
		rpd.rp_len = dmp->dmm_pdlen;
		rpd.rp_data = dmp->dmm_pdesc;
		g->g_pd = pd_Load (&rpd);
		/* The dreaded "zoom bug" squashed at last */
		if (g->g_linkpar && g->g_linksrc)
			g->g_linksrc->cfw_graphic->g_pd = g->g_pd;
		break;
	/*
	 * Nosy windows checking up on each other's coords.
	 */
	   case DM_WBOUNDS:
	   	do_wbounds (win, (struct dm_rq_wbounds *) dmsg);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "%s: type %d from '%s', window '%s'\n",
			  "Unknown DMSG", dmsg->dmm_type, from, win->cfw_name);
	}
}



void
ReleaseMemory ()
{
	FreeButtonMaps ();
	dc_FreeTables ();
	dg_FreeConfigs ();
	dp_FreeAll ();
	pda_ReleaseAll ();
}



static int
dm_shutdown ()
/*
 * Shutdown the display manager.
 */
{
	struct dm_msg dmsg;
/*
 * Send a DIE message to all of the graphics processes.
 */
	dmsg.dmm_type = DM_DIE;
	dmsg_Broadcast (GroupName, &dmsg, sizeof(dmsg));
	dm_ExitHelp ();
/*
 * Free up all known memory allocations so that leaks can be
 * detected.
 */
	ReleaseMemory ();
/*
 * Now finish up here and quit.
 */
	ui_finish ();
	exit (1);
	return (0);	/* for lint's sake */
}



static int
WaitForDeath (msg, param)
struct message *msg;
void *param;
/*
 * Search for an obituary notice for a particular client and process it.
 * Other messages and obituary notices get left in the message queue.
 */
{
	struct mh_template *tm;
	struct mh_clientevent *client;
	char *cname = (char *) param; /* name of client expecting obituary */
	int found = 0;

	tm = (struct mh_template *) msg->m_data;
	switch (tm->mh_type)
	{
	   case MH_SHUTDOWN:
	   	ui_printf ("Message handler shutdown -- I quit!\n");
		ui_finish ();
		exit (1);
	/*
	 * Our real interest is here
	 */
	   case MH_CLIENT:
		client = (struct mh_clientevent *) msg->m_data;
		if (client->mh_evtype == MH_CE_DISCONNECT &&
		    !strcmp (cname, client->mh_client))
		{
			found = 1;
			ProcessDeath (client->mh_client);
		}
		break;
	}
	return (found ? MSG_DONE : MSG_ENQUEUE);
}



/*ARGSUSED*/
static void
recycle (sig)
int sig;
{
/*
 * Finish up ui stuff, and disconnect our ipc connections
 */
	alarm (0);
	signal (SIGALRM, SIG_DFL);
	ui_printf ("Shutting down and starting over.\n");
	ui_finish ();
	XCloseDisplay (Dm_Display);
	msg_disconnect ();
	execvp (Argv[0], Argv);
	perror ("execvp");
	exit (1);
}



static int
dm_cycle ()
/*
 * Re-exec ourselves and just start all over
 */
{
	struct dm_msg dmsg;
	Process **pp;
/*
 * Send a DIE message to all of the graphics processes, and wait for them
 * to finish.  If we don't hear from them within 15 seconds, we cycle
 * anyway.
 */
	Restart = FALSE;
	dm_ExitHelp ();
	dmsg.dmm_type = DM_DIE;
	dmsg_Broadcast (GroupName, &dmsg, sizeof(dmsg));
	signal (SIGALRM, recycle);
	alarm (15);
	do {
		pp = dp_ProcessList (NULL);
		while (*pp && ( !(*pp)->p_cfw || ! IsGraphic((*pp)->p_cfw)))
			++pp;
		if (*pp)
			msg_Search (MT_MESSAGE, WaitForDeath, (*pp)->p_name);
	} while (*pp);
	alarm (0);
	recycle (-1);
	return (0);	/* never reached */
}



static void
newpd (window, pdesc)
char *window, *pdesc;
/*
 * Change the plot description for this window.
 */
{
	struct cf_window *win = dg_CurrentWindow (window);
	struct cf_graphic *g;
	plot_description pd;
/*
 * Only mapped windows, for now.
 */
 	if (! win)
	{
		msg_ELog (EF_PROBLEM, "NEWPD: Window '%s' is %s", 
			  window, "not currently active");
		return;
	}
/*
 * Find this pd.
 */
	if (! (pd = pda_GetPD (pdesc)))
	{
		msg_ELog (EF_PROBLEM, "NEWPD for win %s wants bad pd %s",
			window, pdesc);
		return;
	}
/*
 * If the window had an old one, return it.
 */
	dg_ReleasePD (win);
	g = win->cfw_graphic;
/*
 * Store the new info, and send it to the graphics process.
 */
	strcpy (g->g_desc, pdesc);
	g->g_pd = pd_CopyPD (pd);
	dg_SendPD (win);
/*
 * If we are shoving times down their throats, send the history time too.
 */
	dt_SendTime (win);
}



static void
exec_button (win, dme)
struct cf_window *win;
struct dm_event *dme;
/*
 * Execute a button event.
 */
{
	union usy_value v;
/*
 * Set up the symbols, and exec the command.
 */
	v.us_v_ptr = win->cfw_name;
	usy_s_symbol (usy_g_stbl ("ui$variable_table"), "dm$button_window",
		      SYMT_STRING, &v);
	ui_perform (dme->dmm_data);
}





static void
exchange (cwin1, cwin2)
char *cwin1, *cwin2;
/*
 * Exchange plot descriptions between these two windows.
 */
{
	struct cf_window *win1 = dg_CurrentWindow (cwin1);
	struct cf_window *win2 = dg_CurrentWindow (cwin2);
	struct cf_graphic *g1, *g2;
	char *tmp;
	plot_description tpd;
/*
 * Sanity checking.
 */
	if (! win1)
		badwin (cwin1);
	if (! win2)
		badwin (cwin2);
/*
 * Now set the pd's accordingly.
 */
	g1 = win1->cfw_graphic;
	g2 = win2->cfw_graphic;
	tmp = usy_string (g1->g_desc);
	tpd = g1->g_pd;
	strcpy (g1->g_desc, g2->g_desc);
	g1->g_pd = g2->g_pd;
	strcpy (g2->g_desc, tmp);
	g2->g_pd = tpd;
	usy_rel_string (tmp);
/*
 * Tell the graphprocs about it.
 */
	dg_SendPD (win1);
	dg_SendPD (win2);
}



plot_description
find_pd (name)
char *name;
/*
 * Try to find a command-line-given plot description (meaning we should
 * accept window names also).
 */
{
	struct cf_window *win = dg_CurrentWindow (name);
/*
 * If we get a window, we return its plot description.
 * Otherwise we try for a direct PD name.
 */
	if (! win)
		return (pda_GetPD (name));
	else if (! win->cfw_graphic)
		return (0);
	else
		return (win->cfw_graphic->g_pd);
}



static void
parameter (name, comp, param, value)
char *name, *comp, *param, *value;
/*
 * Change a parameter on a window.
 */
{
	plot_description pd = find_pd (name);
	struct cf_window *win = dg_CurrentWindow (name);
/*
 * Sanity check.
 */
	if (! pd)
	{
		msg_ELog (EF_PROBLEM, "Unable to find pd '%s'", name);
		return;
	}
/*
 * Do the change.
 */
	pd_Store (pd, comp, param, value, SYMT_STRING);
	if (win)
		send_param (win, comp, param, value);
}




static void
send_param (win, comp, param, value)
struct cf_window *win;
char *comp, *param, *value;
/*
 * Send this parameter to the given window.
 */
{
	struct dm_parchange dmp;

	dmp.dmm_type = DM_PARCHANGE;
	strcpy (dmp.dmm_comp, comp);
	strcpy (dmp.dmm_param, param);
	strcpy (dmp.dmm_value, value);
	dmsg_SendWindow (win, &dmp, sizeof (dmp));
}




static void
z_remove (pdn, comp)
char *pdn, *comp;
/*
 * Remove this component from this window.
 */
{
	struct cf_window *win = dg_CurrentWindow (pdn);
/*
 * Make sure the window is active.
 */
	if (! win)
	{
		msg_ELog (EF_PROBLEM, "Remove (%s %s) FAIL -- %s not active",
			pdn, comp, pdn);
		return;
	}
/*
 * Do the zap.
 */
	if (win->cfw_graphic && pd_RemoveComp (win->cfw_graphic->g_pd, comp))
		dg_SendPD (win);
}




static void
add (pdn, comp, dest, position)
char *pdn, *comp, *dest;
int position;
/*
 * Add this component from this PD to DEST.
 */
{
	plot_description pd = find_pd (pdn), pdcomp;
	struct cf_window *dwin = dg_CurrentWindow (dest);
	char newname[40];
	int i;
/*
 * Sanity checks.
 */
	if (! pd)
	{
		msg_ELog (EF_PROBLEM, "FAIL: PD '%s' not found", pdn);
		return;
	}
	if (! dwin)
	{
		msg_ELog (EF_PROBLEM, "FAIL: Dest win '%s' not active", dest);
		return;
	}
/*
 * Figure out the name for this component in the new PD, so as not to
 * wipe out one that already exists.
 */
	strcpy (newname, comp);
	for (i = 0; pd_CompExists (dwin->cfw_graphic->g_pd, newname); i++)
		sprintf (newname, "%s.%d", comp, i);
/*
 * Pull out the component.  If it fails, ReadComponent will gripe, so
 * we don't have to.
 */
	if (! (pdcomp = pd_ReadComponent (pd, comp, newname)))
		return;
/*
 * Add it to the destination.
 */
/*	pd_Merge (dwin->cfw_pd, pdcomp); */
	pd_AddComponent (dwin->cfw_graphic->g_pd, pdcomp, position);
	dg_SendPD (dwin);
}




static void
MakeDefaultMap ()
/*
 * Create the default default button map, just in case they don't redefine
 * themselves.
 */
{
	ButtonMap *map;
	union usy_value v;
/*
 * Create the structures.
 */
	map = Default_map = ALLOC (ButtonMap);
	v.us_v_ptr = (char *) Default_map;
	usy_s_symbol (Bmaps, "default", SYMT_POINTER, &v);
/*
 * Fill in the three mouse buttons to send back beeps.
 */
	strcpy (map->db_name, "default");
	map->db_nentry = 3;
	strcpy (map->db_bindings[0].dmm_code, "mb-left");
	strcpy (map->db_bindings[1].dmm_code, "mb-middle");
	strcpy (map->db_bindings[2].dmm_code, "mb-right");
	map->db_bindings[0].dmm_action = AC_Report;
	map->db_bindings[1].dmm_action = AC_Report;
	map->db_bindings[2].dmm_action = AC_Report;
	strcpy (map->db_bindings[0].dmm_adata, "beep");
	strcpy (map->db_bindings[1].dmm_adata, "beep");
	strcpy (map->db_bindings[2].dmm_adata, "beep");
}



static void
FreeButtonMaps ()
{
	usy_traverse (Bmaps, FreeBM, 0, FALSE);
	usy_z_stbl (Bmaps);
	Bmaps = 0;
}



static int
FreeBM (name, type, v, param)
char *name;
int type;
union usy_value *v;
int param;
{
	ButtonMap *map = (ButtonMap *) v->us_v_ptr;
	free (map);
	usy_z_symbol (Bmaps, name);
	return (TRUE);
}



int
SEChange (sym, arg, op, oldtype, oldv, newtype, newv)
char *sym;
int arg, op, oldtype, newtype;
SValue *oldv, *newv;
/*
 * The SoundEnabled variable has changed.  Ship off the new value to
 * the sound generator process.
 */
{
	zbool send = newv->us_v_int;

	dmsg_SendSound (&send, 1);
	return (0);
}





static void
do_wbounds (from, wb)
struct cf_window *from;
struct dm_rq_wbounds *wb;
/*
 * Deal with a window bounds request.
 */
{
	zbool ok;
	char string[40];
	struct cf_window *win = dg_CurrentWindow (wb->dmm_window);
	struct cf_graphic *g;
	struct dm_rp_wbounds reply;
/*
 * If this window is not active, we can't do anything.
 */
	reply.dmm_type = DM_WBOUNDS;
	reply.dmm_success = FALSE;
	if (! win || ! win->cfw_graphic)
		goto sendreply;
/*
 * Get the plot type
 */
	g = win->cfw_graphic;
	if (! g->g_pd || ! pda_Search (g->g_pd, "global", "plot-type", NULL,
				       reply.dmm_pltype, SYMT_STRING))
		goto sendreply;
/*
 * Pull out the params based on plot type.
 */
	if (! strcmp (reply.dmm_pltype, "CAP"))
	{
		ok = pda_Search (g->g_pd, "global", "x-min", NULL, 
			(char *) &reply.dmm_x0, SYMT_FLOAT);
		ok &= pda_Search (g->g_pd, "global", "x-max", NULL,
			(char *) &reply.dmm_x1, SYMT_FLOAT);
		ok &= pda_Search (g->g_pd, "global", "y-min", NULL,
			(char *) &reply.dmm_y0, SYMT_FLOAT);
		ok &= pda_Search (g->g_pd, "global", "y-max", NULL,
			(char *) &reply.dmm_y1, SYMT_FLOAT);

		reply.dmm_success = ok;
	}
	else if (! strcmp (reply.dmm_pltype, "xsect"))
	{
		ok = pda_Search (g->g_pd, "global", "left-endpoint", 
			NULL, string, SYMT_STRING);
		sscanf (string, "%f, %f", &reply.dmm_x0, &reply.dmm_y0);

		ok &= pda_Search (g->g_pd, "global", "right-endpoint", 
			NULL, string, SYMT_STRING);
		sscanf (string, "%f, %f", &reply.dmm_x1, &reply.dmm_y1);

		reply.dmm_success = ok;
	}
/*
 * Get altitude if it's there
 */
	if (! pda_Search (g->g_pd, "global", "altitude", NULL,
				(char *) &reply.dmm_alt, SYMT_FLOAT))
		reply.dmm_alt = 0;
/*
 * Send out the reply.
 */
sendreply:
	dmsg_SendWindow (from, &reply, sizeof (reply));
}





static void
EnterPosition (cmds)
struct ui_command *cmds;
/*
 * Manually enter a position.
 */
{
	PlatformId pid;
	Location where;
	DataChunk *dc;
	FieldId fid;
	ZebTime when;
	float dlat, dlon, mlat, mlon;

	fid = F_Lookup ("trans");
/*
 * Pull out basic info.
 */
	if ((pid = ds_LookupPlatform (UPTR (*cmds))) == BadPlatform)
		ui_error ("Bad platform %s", UPTR (*cmds));
	dlat = UFLOAT (cmds[1]);
	mlat = UFLOAT (cmds[2]);
	dlon = UFLOAT (cmds[3]);
	mlon = UFLOAT (cmds[4]);
/*
 * Use absolute value for minutes, but add them in the same direction
 * as the degrees.
 */
	where.l_lat = dlat + ((dlat < 0) ? (-1) : (1)) * fabs(mlat/60.0);
	where.l_lon = dlon + ((dlon < 0) ? (-1) : (1)) * fabs(mlon/60.0);
	where.l_alt = UFLOAT (cmds[5]);
/*
 * If they gave us a time, use it; otherwise we need to see when the last
 * point is.
 */
	if (cmds[6].uc_ctype != UTT_END)
	{
	    TC_UIToZt (&UDATE (cmds[6]), &when);
	    msg_ELog (EF_INFO, "entering %s location at %s",
		      ds_PlatformName(pid), TC_AscTime (&when, TC_Full));
	}
	else
	{
	    ZebTime now;
	    tl_Time (&now);
	    if (ds_DataTimes (pid, &now, 1, DsBefore, &when) != 1)
	    {
		when = now;
		msg_ELog (EF_PROBLEM, "no last point found for %s, using %s",
			  ds_PlatformName(pid), TC_AscTime (&when, TC_Full));
	    }
	    else
	    {
		msg_ELog (EF_INFO, "replacing %s location at %s",
			  ds_PlatformName(pid), TC_AscTime (&when, TC_Full));
	    }
	}
/*
 * Make our data chunk.
 */
	dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = pid;
	dc_SetScalarFields (dc, 1, &fid);
	dc_AddScalar (dc, &when, 0, fid, &where.l_lat);
	dc_SetLoc (dc, 0, &where);
/*
 * Store the point, free the data chunk, and we are done.
 */
	ds_Store (dc, FALSE, 0, 0);
	dc_DestroyDC (dc);
}



static int
RealPlatform (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * The "realstation" command line function -- return TRUE if the argument
 * is a real platform.
 */
{
	retv->us_v_int = (ds_LookupPlatform (argv->us_v_ptr) != BadPlatform);
	*rett = SYMT_BOOL;
	return (0);
}




static int
HasData (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * The "hasdata" command line function -- return true if a data file exists
 * for this platform.  This is meant to be a quick function and hence tries
 * to avoid actually opening any files to get sample times or anything like
 * that.  It merely tests for the existence of data from any source.
 */
{
    int result = 0;

    PlatformId pid = ds_LookupPlatform (argv->us_v_ptr);
    if (pid != BadPlatform)
    {
	result = (ds_LastFile (SRC_ALL, pid) != 0);
    }

    retv->us_v_int = result;
    *rett = SYMT_BOOL;
    return (0);
}




static void
MakeWindowList (sym)
char *sym;
/*
 * We getta make a list of graphic windows.
 */
{
	SValue v;
	char sbuf[512];

	sbuf[0] = '\0';
        usy_search (Current, AddToList, (long) sbuf, FALSE, NULL);
	v.us_v_ptr = sbuf;
	usy_s_symbol (usy_g_stbl ("ui$variable_table"), sym, SYMT_STRING, &v);
}



static int
AddToList (name, t, v, lsbuf)
char *name;
long lsbuf;
int t;
SValue *v;
/*
 * Add a graphic window to the list.
 */
{
	struct cf_window *win = (struct cf_window *) v->us_v_ptr;
	char *sbuf = (char *) lsbuf;

	if (IsGraphic (win))
	{
		strcat (sbuf, " ");
		strcat (sbuf, name);
	}
	return (TRUE);
}




#ifdef SUPPORT_XHELP
static void
CallXHelp (cmds)
struct ui_command *cmds;
/*
 * Fire up xhelp: help [topic] [help-file]
 * If no topic or help-file, intro of default help file used
 * If only topic specified, default help file used
 * If a non-default help file is specified, the topic must
 * be specified as well.  The file will be looked for in the 
 * library directory. "intro" topic becomes XHELP_INTRO_ID.
 *
 * Old, obsolete stuff.
 */
{
	char helpfile[120], topic[40], helpdir[120];
/*
 * Kludge up a help directory.  Should probably do something with
 * HelpPath, but this is old stuff anyway...
 */
	strcpy (helpdir, GetLibDir ());
	strcat (helpdir, "/help");
/*
 * Now fire things off.
 */
	fixdir ("ZEB_HELPFILE", helpdir, "zeb.hlp", helpfile);
	if (cmds[1].uc_ctype == UTT_END)
		strcpy (topic, XHELP_INTRO_ID);
	else
	{
		if (strcmp(UPTR (cmds[1]), "intro") == 0)
			strcpy (topic, XHELP_INTRO_ID);
		else
			strcpy (topic, UPTR (cmds[1]));
		strcat (topic, "             ");
		topic[13] = '\0';
		if (cmds[2].uc_ctype != UTT_END)
			fixdir ("ZEB_HELPFILE", GetLibDir (), 
					UPTR (cmds[2]), helpfile);
	}
	XhCallXHelp (Top, helpfile, topic, "Welcome to Zeb");
	XFlush(Dm_Display);	/* flush Xatoms to xhelp program */
}
#endif /* SUPPORT_XHELP */

