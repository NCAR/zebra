/*
 * The MOCCA display manager.
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
# include <varargs.h>
# include <fcntl.h>
# include <X11/Intrinsic.h>

# include <ui.h>
# include <ui_error.h>
# include "dm_vars.h"
# include "dm_cmds.h"
# include <timer.h>
# include <config.h>
# include <copyright.h>
MAKE_RCSID ("$Id: dm.c,v 2.14 1992-07-13 15:59:10 pai Exp $")


/*
 * Definitions of globals.
 */
stbl Configs;
stbl Windows;
stbl Current;
char Cur_config[MAXNAME];
stbl Bmaps;
ButtonMap *Default_map;	/* The default button map	*/
Display *Dm_Display;

char ConfigDir[200];	/* Default directory for display configs */
char ConfigPD[200];	/* Where to save plot descriptions	*/
char ConfigPath[512];	/* Path to search for display configs */

/*
 * Is sound enabled?
 */
static int SoundEnabled = FALSE;

/*
 * How long and how often to sleep while creating windows.
 */
int SleepAfter = 4, SleepFor = 1;

/*
 * Do we restart windows which die?
 */
static bool Restart = TRUE;


/*
 * Forward routines.
 */
static void do_wbounds FP ((char *, struct dm_rq_wbounds *));
int dm_shutdown FP ((void));
int SEChange FP ((char *, int, int, int, SValue *, int, SValue *));
static void ForceRestart FP ((char *));
static int dm_dispatcher FP ((int, struct ui_command *));
static int dm_msg_handler FP ((Message *));



main (argc, argv)
int argc;
char **argv;
{
	int msg_incoming (), get_pd ();
	int is_active (), type[4], pd_param (), pd_defined (), tw_cb ();
	char loadfile[100];
	Widget top;
	stbl vtable;
/*
 * Hook into the message handler.
 */
	msg_connect (dm_msg_handler, "Displaymgr");
	msg_DeathHandler (dm_shutdown);
	msg_join ("Client events");
/*
 * Get the interface set up.
 */
	fixdir_t ("DMLOADFILE", LIBDIR, "dm.lf", loadfile, ".lf");
	ui_init (loadfile, TRUE, FALSE);
	ui_setup ("DisplayMgr", &argc, argv, (char *) 0);
	SetupConfigVariables ();
/*
 * Create our symbol tables.
 */
	Configs = usy_c_stbl ("Configurations");
	Windows = usy_c_stbl ("Windows");
	Current = usy_c_stbl ("junk");
	Bmaps = usy_c_stbl ("ButtonMaps");
	strcpy (Cur_config, "(nothing)");
	dc_Init ();
/*
 * Create the default default button map.
 */
	MakeDefaultMap ();
/*
 * Command line functions.
 */
	type[0] = type[1] = type[2] = type[3] = SYMT_STRING;
	uf_def_function ("pdesc", 1, type, get_pd);
	uf_def_function ("active", 1, type, is_active);
	uf_def_function ("pd_param", 4, type, pd_param);
	uf_def_function ("pd_defined", 3, type, pd_defined);
/*
 * Indirect variables.
 */
	vtable = usy_g_stbl ("ui$variable_table");
	usy_c_indirect (vtable, "dm$config", Cur_config, SYMT_STRING, MAXNAME);
	usy_c_indirect (vtable, "soundenabled", &SoundEnabled, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "restart", &Restart, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "sleepafter", &SleepAfter, SYMT_INT, 0);
	usy_c_indirect (vtable, "sleepfor", &SleepFor, SYMT_INT, 0);
	usy_c_indirect (vtable, "configdir", ConfigDir, SYMT_STRING, 200);
	usy_c_indirect (vtable, "configpd", ConfigPD, SYMT_STRING, 200);
	usy_c_indirect (vtable, "configpath", ConfigPath, SYMT_STRING, 512);
	usy_daemon (vtable, "soundenabled", SOP_WRITE, SEChange, 0);
	tty_watch (msg_get_fd (), (void (*)()) msg_incoming);

/*
 * Can't figure out why these were in here.  Leaving them for now...
 *	strcpy (ConfigDir, ".");
 *	strcpy (ConfigPD, ".");
 */

# ifdef titan
/*
 * Hook into the dialbox.
 */
	dlb_Init ();
# endif
/*
 * Push into window mode, and get our display.
 */
 	uw_ForceWindowMode ((char *) 0, &top, (XtAppContext *) 0);
	Dm_Display = XtDisplay (top);
/*
 * If a file appears on the command line, open it.
 */
	if (argc > 1)
		ut_open_file (argv[1], TRUE);
/*
 * Interpret commands.
 */
	tw_DefTimeWidget (tw_cb, "Overall system time control");
	aw_DefAlarmWidget ();
	ui_get_command ("dm-initial", "DM>", dm_dispatcher, 0);
	dm_shutdown ();
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

	switch (UKEY (*cmds))
	{
	   case DMC_CONFIG:
	   	def_config (cmds + 1);
		break;

	   case DMC_DISPLAY:
	   	display (cmds + 1);
		break;

	   case DMC_LIST:
	   	list ();
		break;

	   case DMC_NEWPD:
	   	newpd (UPTR (cmds[1]), UPTR (cmds[2]));
		break;

	   case DMC_BEEP:
	   	ui_printf ("\007");
		break;

	   case DMC_BUTTONMAP:
	   	def_bmap (UPTR (cmds[1]));
		break;

	   case DMC_EXCHANGE:
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
	   	remove (UPTR (cmds[1]), UPTR (cmds[2]));
		break;

	   case DMC_ADD:
	   	add (UPTR (cmds[1]), UPTR (cmds[2]), UPTR (cmds[3]),
			cmds[4].uc_ctype == UTT_END ? 0 : UINT (cmds[4]));
		break;

	   case DMC_HISTORY:
	   	history (cmds + 1);
		break;

	   case DMC_REALTIME:
	   	realtime (cmds + 1);
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
	   	SetTime (&UDATE (cmds[1]));
		break;

	   case DMC_PICKWIN:
	   	PickWin (winname);
		nv.us_v_ptr = winname;
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), 
			UPTR (cmds[1]), SYMT_STRING, &nv);
		break;

	   case DMC_SHUTDOWN:
	   	dm_shutdown ();
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
	   	ForceRestart (UPTR (cmds[1]));
		break;
	/*
	 * Saving of configurations.
	 */
	   case DMC_CFGSAVE:
	   	SaveConfig (cmds[1].uc_ctype == UTT_END ?
					Cur_config : UPTR (cmds[1]));
		break;
	/*
	 * New windows and configs.
	 */
	   case DMC_NEWCONFIG:
	   	NewConfig (UPTR (cmds[1]), cmds[2].uc_ctype == UTT_END ?
				"template" : UPTR (cmds[2]));
		break;

	   case DMC_NEWWINDOW:
	   	NewWindow (cmds + 1);
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
	   	dm_message (msg->m_from, (struct dm_msg *) msg->m_data);
		return;
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
	};
}




mh_message (msg)
struct message *msg;
/*
 * Deal with a MESSAGE protocol msg.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
	struct mh_client *client;

	switch (tm->mh_type)
	{
	   case MH_SHUTDOWN:
	   	ui_printf ("Message handler shutdown -- I quit!\n");
		ui_finish ();
		exit (1);
	/*
	 * For client events, we are really only interested in deaths.
	 */
	   case MH_CLIENT:
		client = (struct mh_client *) msg->m_data;
		if (client->mh_evtype == MH_CE_DISCONNECT && Restart)
			ProcessDeath (client->mh_client);
		break;

	   default:
	   	ui_printf ("Unknown MESSAGE proto msg %d\n", tm->mh_type);
		break;
	}
}






static void
ForceRestart (window)
char *window;
/*
 * Force this window to quit and restart.
 */
{
	struct dm_msg dmsg;
/*
 * Send a DIE message to the affected process.  The auto restart mechanism
 * should take care of the rest.
 */
	dmsg.dmm_type = DM_DIE;
	msg_send (window, MT_DISPLAYMGR, FALSE, &dmsg, sizeof (dmsg));
}



ProcessDeath (client)
char *client;
/*
 * Deal with the fact that this client has died.
 */
{
	struct cf_window *win = lookup_win (client, TRUE);
/*
 * If this was a currently active window, let us simply restart it now.
 */
	if (win)
	{
		if (++win->cfw_ncroak < 10)
			RestartWin (win);
		else
			msg_ELog (EF_PROBLEM,
				"Win %s dies too often -- I give up", client);
	}
/*
 * Otherwise, if this is an existing window, but not in the current config,
 * we need to just mark it as being dead.
 */
	else if (win = lookup_win (client, FALSE))
		usy_z_symbol (Windows, client);
}





RestartWin (win)
struct cf_window *win;
/*
 * Give this window a new lease on life.
 */
{
	msg_ELog (EF_PROBLEM, "Win '%s' died -- restarting", win->cfw_name);
	win->cfw_tmpforce = TRUE;
	create_win (win);
}
	



struct cf_window *
lookup_win (name, curonly)
char *name;
bool curonly;
/*
 * Find the saved entry for this window, if it exists.
 */
{
	int type;
	union usy_value v;
/*
 * Look it up in our symbol table.
 */
	if (usy_g_symbol (Current, name, &type, &v))
		return ((struct cf_window *) v.us_v_ptr);
	if (curonly || ! usy_g_symbol (Windows, name, &type, &v))
		return (FALSE);
	return ((struct cf_window *) v.us_v_ptr);
}





struct config *
LookupConfig (name)
char *name;
/*
 * Try to find a configuration by this name.
 */
{
	int type;
	SValue v;
	char fname[200];
	char delim = ',';	/* a token delimiter for strtok() */
	char * ccd;		/* current configs directory 	*/
	int firstime = TRUE;	/* Check the default dir first 	*/
	
/*
 * If ConfigPath is defined, get it, otherwise set it to "."
 */
 	if (usy_g_symbol (Configs, "configpath", &type, &v) == FALSE)
		ccd = NULL;
	else
	{
		strcpy(ConfigPath, v.us_v_ptr);
		ccd = strtok(ConfigPath, &delim);
	}

/*
 * Look up this config in the configs table.
 */
 	if (usy_g_symbol (Configs, name, &type, &v))
		return ((struct config *) v.us_v_ptr);
/*
 * If we didn't find it in the table, search ConfigDir and ConfigPath to 
 * try and find it, load it if it exists, then try again.
 */
	while(1)
	{
		if(firstime == FALSE)
		{
			if(ccd != NULL)
				strcpy(ConfigDir, ccd);
			else
				strcpy(ConfigDir, ".");
		}

		sprintf (fname, "read %s/%s", ConfigDir, name);
		if (access (fname + 5, F_OK) == 0)
		{
			ui_perform (fname);
			if (usy_g_symbol (Configs, name, &type, &v))
				return ((struct config *) v.us_v_ptr);
			ui_error ("Unknown configuration: '%s'\n", name);
		}

		if(ccd == NULL)
			ui_error ("Unknown configuration: '%s'\n", name);
	
		if(firstime == FALSE)
			ccd = strtok(NULL, &delim);
		else
			firstime = FALSE;
	}
}






dm_message (from, dmsg)
char *from;
struct dm_msg *dmsg;
/*
 * Deal with a display manager message.
 */
{
	struct dm_hello *dmh;
	struct dm_pdchange *dmp;
	struct cf_window *win;
	raw_plot_description rpd;

	switch (dmsg->dmm_type)
	{
	/*
	 * A new window checking in.  Finalize the data structure, and
	 * configure the window.
	 */
	   case DM_HELLO:
		dmh = (struct dm_hello *) dmsg;   	
		if (! (win = lookup_win (from, TRUE)))
		{
			msg_ELog (EF_PROBLEM, "Funky hello from '%s'", from);
			return;
		}
		msg_ELog (EF_DEBUG, "Hello received from '%s' win %x", from,
			dmh->dmm_win);
		win->cfw_win = dmh->dmm_win;
		send_default (win);
		config_win (win);
		break;
	/*
	 * A button report
	 */
	   case DM_EVENT:
		exec_button (from, (struct dm_event *) dmsg);
		break;
	/*
	 * A color table request.
	 */
	   case DM_R_CTABLE:
		if (! (win = lookup_win (from, TRUE)))
		{
			msg_ELog (EF_PROBLEM, "Funky CTR from '%s'", from);
			return;
		}
		dc_TableRequest ((struct dm_ctr *) dmsg, win->cfw_name);
		break;
	/*
	 * A graphics process has had the temerity to change its own
	 * plot description.  Now we have to scramble to keep up with it.
	 */
	   case DM_PDCHANGE:
		if (! (win = lookup_win (from, TRUE)))
		{
			msg_ELog(EF_PROBLEM, "Funky PDCHANGE from '%s'", from);
			return;
		}
		msg_ELog (EF_DEBUG, "New PD from %s", from);
		pd_Release (win->cfw_pd);
		dmp = (struct dm_pdchange *) dmsg;
		rpd.rp_len = dmp->dmm_pdlen;
		rpd.rp_data = dmp->dmm_pdesc;
		win->cfw_pd = pd_Load (&rpd);
		/* The dreaded "zoom bug" squashed at last */
		if (win->cfw_linkpar && win->cfw_linksrc)
			win->cfw_linksrc->cfw_pd = win->cfw_pd;
		break;
	/*
	 * Nosy windows checking up on each other's coords.
	 */
	   case DM_WBOUNDS:
	   	do_wbounds (from, (struct dm_rq_wbounds *) dmsg);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Funky DMSG type %d from %s\n",
			dmsg->dmm_type, from);
	}
}





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
	msg_send ("Graphproc", MT_DISPLAYMGR, TRUE, &dmsg, sizeof (dmsg));
/*
 * Now finish up here and quit.
 */
	ui_finish ();
	exit (1);
}




newpd (window, pdesc)
char *window, *pdesc;
/*
 * Change the plot description for this window.
 */
{
	struct cf_window *win = lookup_win (window, TRUE);
	plot_description pd;
	struct dm_pdchange dmp;
/*
 * Only mapped windows, for now.
 */
 	if (! win)
		ui_error ("Window '%s' is not currently active", window);
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
	if (win->cfw_pd)
		pd_Release (win->cfw_pd);
/*
 * Store the new info, and send it to the graphproc.
 */
	strcpy (win->cfw_desc, pdesc);
	win->cfw_pd = pd_CopyPD (pd);
	send_pd (win);
}



exec_button (from, dme)
char *from;
struct dm_event *dme;
/*
 * Execute a button event.
 */
{
	struct cf_window *win = lookup_win (from, TRUE);
	union usy_value v;
/*
 * Only windows on the screen should be sending events.
 */
	if (! win)
	{
		msg_ELog (EF_PROBLEM, "Funky button from '%s'", from);
		return;
	}
/*
 * Set up the symbols, and exec the command.
 */
	v.us_v_ptr = from;
	usy_s_symbol (usy_g_stbl ("ui$variable_table"), "dm$button_window",
		SYMT_STRING, &v);
	ui_perform (dme->dmm_data);
}






exchange (cwin1, cwin2)
char *cwin1, *cwin2;
/*
 * Exchange plot descriptions between these two windows.
 */
{
	struct cf_window *win1 = lookup_win (cwin1, TRUE);
	struct cf_window *win2 = lookup_win (cwin2, TRUE);
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
	tmp = usy_string (win1->cfw_desc);
	tpd = win1->cfw_pd;
	strcpy (win1->cfw_desc, win2->cfw_desc);
	win1->cfw_pd = win2->cfw_pd;
	strcpy (win2->cfw_desc, tmp);
	win2->cfw_pd = tpd;
	usy_rel_string (tmp);
/*
 * Tell the graphprocs about it.
 */
	send_pd (win1);
	send_pd (win2);
}





send_pd (win)
struct cf_window *win;
/*
 * Send this window it's PD.
 */
{
	struct dm_pdchange *dmp;
	raw_plot_description *rpd = pd_Unload (win->cfw_pd);
	int len = sizeof (struct dm_pdchange) + rpd->rp_len;
/*
 * Allocate a sufficiently big pdchange structure.
 */
	dmp = (struct dm_pdchange *) malloc (len);
/*
 * Move over the stuff.
 */
	dmp->dmm_type = DM_PDCHANGE;
	dmp->dmm_pdlen = rpd->rp_len;
	memcpy (dmp->dmm_pdesc, rpd->rp_data, rpd->rp_len);
	msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, dmp, len);

	free (dmp);
}




send_default (win)
struct cf_window *win;
/*
 * Send this window it's defaults table.
 */
{
	struct dm_pdchange *dmp;
	plot_description def = pda_GetPD ("defaults");
	raw_plot_description *rpd;
	int len;
/*
 * If there is no defaults table, forget it.
 */
	if (! def)
		return;
/*
 * Convert it to external form.
 */
	rpd = pd_Unload (def);
	len = sizeof (struct dm_pdchange) + rpd->rp_len;
/*
 * Allocate a sufficiently big pdchange structure.
 */
	dmp = (struct dm_pdchange *) malloc (len);
/*
 * Move over the stuff.
 */
	dmp->dmm_type = DM_DEFAULTS;
	dmp->dmm_pdlen = rpd->rp_len;
	memcpy (dmp->dmm_pdesc, rpd->rp_data, rpd->rp_len);
	msg_ELog (EF_DEBUG, "Sending defaults to %s len %d", win->cfw_name,
		len);
	msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, dmp, len);

	free (dmp);
}






plot_description
find_pd (name)
char *name;
/*
 * Try to find a command-line given plot description.
 */
{
	struct cf_window *win = lookup_win (name, TRUE);
/*
 * If we get a window, we return its plot description.
 */
	if (win)
		return (win->cfw_pd);
/*
 * Otherwise we try for a direct PD name.
 */
	return (pda_GetPD (name));
}




parameter (name, comp, param, value)
char *name, *comp, *param, *value;
/*
 * Change a parameter on a window.
 */
{
	plot_description pd = find_pd (name);
	struct cf_window *win = lookup_win (name, TRUE);
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
	msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, &dmp, sizeof (dmp));
}





remove (pdn, comp)
char *pdn, *comp;
/*
 * Remove this component from this window.
 */
{
	struct cf_window *win = lookup_win (pdn, TRUE);
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
	if (pd_RemoveComp (win->cfw_pd, comp))
		send_pd (win);
}





add (pdn, comp, dest, position)
char *pdn, *comp, *dest;
int position;
/*
 * Add this component from this PD to DEST.
 */
{
	plot_description pd = find_pd (pdn), pdcomp;
	struct cf_window *dwin = lookup_win (dest, TRUE);
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
	for (i = 0; pd_CompExists (dwin->cfw_pd, newname); i++)
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
	pd_AddComponent (dwin->cfw_pd, pdcomp, position);
	send_pd (dwin);
}






history (cmds)
struct ui_command *cmds;
/*
 * Throw one or more windows into history mode.
 */
{
	bool all = (cmds->uc_ctype == UTT_KW);
	struct cf_window *dwin;
	struct dm_history dmh;
/*
 * If necessary, look up the window.
 */
	if (! all)
	{
		if (cmds->uc_vptype != SYMT_STRING)
		{
			msg_ELog (EF_PROBLEM,
				"HISTORY error -- win name must be string");
			return;
		}
		dwin = lookup_win (UPTR (*cmds), TRUE);
		if (! dwin)
		{
			msg_ELog(EF_PROBLEM, "HISTORY on unavailable win '%s'",
				UPTR (*cmds));
			return;
		}
	}
/*
 * Put together the message.
 */
	dmh.dmm_type = DM_HISTORY;
	/* dmh.dmm_time = UDATE (cmds[1]); */
	TC_UIToZt (&UDATE (cmds[1]), &dmh.dmm_time);
/*
 * Ship it out.
 */
	if (all)
		msg_send ("Graphproc", MT_DISPLAYMGR, TRUE, &dmh, sizeof(dmh));
	else
		msg_send (dwin->cfw_name, MT_DISPLAYMGR, FALSE, &dmh,
			sizeof(dmh));
}





realtime (cmds)
struct ui_command *cmds;
/*
 * Throw one or more windows into real time mode.
 */
{
	bool all = (cmds->uc_ctype == UTT_KW);
	struct cf_window *dwin;
	struct dm_history dmh;
/*
 * If necessary, look up the window.
 */
	if (! all)
	{
		if (cmds->uc_vptype != SYMT_STRING)
		{
			msg_log ("RT error -- win name must be string");
			return;
		}
		dwin = lookup_win (UPTR (*cmds), TRUE);
		if (! dwin)
		{
			msg_log ("RT on unavailable win '%s'", UPTR (*cmds));
			return;
		}
	}
/*
 * Put together the message.
 */
	dmh.dmm_type = DM_REALTIME;
/*
 * Ship it out.
 */
	if (all)
		msg_send ("Graphproc", MT_DISPLAYMGR, TRUE, &dmh, sizeof(dmh));
	else
		msg_send (dwin->cfw_name, MT_DISPLAYMGR, FALSE, &dmh,
			sizeof(dmh));
}





int
tw_cb (mode, t, control_all)
int mode;
ZebTime *t;
int control_all;
{
	struct dm_history dmh;
	char winname[40];
/*
 * See what window(s) to deal with.
 */
	if (! control_all)
		PickWin (winname);
/*
 * Decide what to do.
 */
	switch (mode)
	{
	   case History:
	   	dmh.dmm_type = DM_HISTORY;
		dmh.dmm_time = *t;
		break;

	   case RealTime:
	   	dmh.dmm_type = DM_REALTIME;
		break;
# ifdef notdef
	   case Movie:
	   	msg_log ("I can't put *everybody* in movie mode!");
		return;
# endif
	   default:
	   	msg_ELog (EF_PROBLEM, "Funky mode (%d) in tw_cb", mode);
		return;
	}
/*
 * Now send out the result.
 */
	if (control_all)
		msg_send ("Graphproc", MT_DISPLAYMGR, TRUE, &dmh, sizeof (dmh));
	else
		msg_send (winname, MT_DISPLAYMGR, FALSE, &dmh, sizeof (dmh));
}




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






SendButtonMap (win)
struct cf_window *win;
/*
 * Send the button map to this window.
 */
{
	static struct dm_ebchange *ebc = 0;
	ButtonMap *map = win->cfw_bmap;
/*
 * Make sure everything's cool.
 */
	if (! map)
	{
		msg_ELog(EF_INFO,"Window %s has no button map", win->cfw_name);
		return;
	}
/*
 * If we haven't allocated our binding change structure yet, do it now.  We
 * just get the biggest we could need and keep it, since this could be 
 * happening fairly often.
 */
	if (! ebc)
		ebc = (struct dm_ebchange *)
			malloc (sizeof (struct dm_ebchange) +
				MAXBINDING*sizeof (struct dm_evbind));
/*
 * Copy over the information into one place.
 */
	ebc->dmm_type = DM_EVBIND;
	ebc->dmm_nbind = map->db_nentry;
	memcpy (ebc->dmm_bindings, map->db_bindings, 
		map->db_nentry * sizeof (struct dm_evbind));
/*
 * Ship it out.
 */
	msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, ebc,
		sizeof (struct dm_ebchange) +
		(map->db_nentry - 1)*(sizeof (struct dm_evbind)));
}





SetTime (when)
time *when;
/*
 * Force pseudo real time mode at this time.
 */
{
	struct tm_prt prt;
	
	prt.tr_type = TR_PRT;
	/* prt.tr_time = *when; */
	TC_UIToZt (when, &prt.tr_time);
	prt.tr_scale = 1;
	msg_send ("Timer", MT_TIMER, FALSE, &prt, sizeof (prt));
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
	bool send = newv->us_v_int;

	msg_send ("Sound", MT_SOUND, FALSE, &send, 1);
	return (0);
}





static void
do_wbounds (from, wb)
char *from;
struct dm_rq_wbounds *wb;
/*
 * Deal with a window bounds request.
 */
{
	bool ok;
	char string[40];
	struct cf_window *win = lookup_win (wb->dmm_window, TRUE);
	struct dm_rp_wbounds reply;
/*
 * If this window is not active, we can't do anything.
 */
	reply.dmm_type = DM_WBOUNDS;
	reply.dmm_success = FALSE;
	if (! win)
		goto sendreply;
/*
 * Get the plot type
 */
	if (! win->cfw_pd ||
		! pda_Search (win->cfw_pd, "global", "plot-type", NULL,
			reply.dmm_pltype, SYMT_STRING))
		goto sendreply;
/*
 * Pull out the params based on plot type.
 */
	if (! strcmp (reply.dmm_pltype, "CAP"))
	{
		ok = pda_Search (win->cfw_pd, "global", "x-min", NULL, 
			(char *) &reply.dmm_x0, SYMT_FLOAT);
		ok &= pda_Search (win->cfw_pd, "global", "x-max", NULL,
			(char *) &reply.dmm_x1, SYMT_FLOAT);
		ok &= pda_Search (win->cfw_pd, "global", "y-min", NULL,
			(char *) &reply.dmm_y0, SYMT_FLOAT);
		ok &= pda_Search (win->cfw_pd, "global", "y-max", NULL,
			(char *) &reply.dmm_y1, SYMT_FLOAT);

		reply.dmm_success = ok;
	}
	else if (! strcmp (reply.dmm_pltype, "xsect"))
	{
		ok = pda_Search (win->cfw_pd, "global", "left-endpoint", 
			NULL, string, SYMT_STRING);
		sscanf (string, "%f, %f", &reply.dmm_x0, &reply.dmm_y0);

		ok &= pda_Search (win->cfw_pd, "global", "right-endpoint", 
			NULL, string, SYMT_STRING);
		sscanf (string, "%f, %f", &reply.dmm_x1, &reply.dmm_y1);

		reply.dmm_success = ok;
	}
/*
 * Get altitude if it's there
 */
	if (! pda_Search (win->cfw_pd, "global", "altitude", NULL,
				(char *) &reply.dmm_alt, SYMT_FLOAT))
		reply.dmm_alt = 0;
/*
 * Send out the reply.
 */
sendreply:
	msg_send (from, MT_DISPLAYMGR, FALSE, &reply, sizeof (reply));
}

