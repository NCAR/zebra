/* $Id: dm.c,v 1.3 1990-04-26 16:23:32 corbet Exp $ */
/*
 * The MOCCA display manager.
 */
# include <stdio.h>
# include <varargs.h>

# include <ui.h>
# include <ui_error.h>
# include "dm_vars.h"
# include "dm_cmds.h"
# include "../include/timer.h"


/*
 * Definitions of globals.
 */
stbl Configs;
stbl Windows;
static stbl Current;
static char Cur_config[MAXNAME];
stbl Bmaps;
char **Default_map;	/* The default button map	*/




main (argc, argv)
int argc;
char **argv;
{
	int dm_dispatcher (), dm_msg_handler (), msg_incoming (), i, get_pd ();
	int is_active (), type[4], pd_param (), pd_defined (), tw_cb ();
	union usy_value v;
	char loadfile[100];
/*
 * Get the interface set up.
 */
	fixdir_t ("DMLOADFILE", "/fcc/lib", "dm.lf", loadfile, ".lf");
	ui_init (loadfile, TRUE, FALSE);
	ui_setup ("DisplayMgr", argc, argv, (char *) 0);
/*
 * Create our symbol tables.
 */
	Configs = usy_c_stbl ("Configurations");
	Windows = usy_c_stbl ("Windows");
	Current = usy_c_stbl ("junk");
	Bmaps = usy_c_stbl ("ButtonMaps");
	strcpy (Cur_config, "nothing");
/*
 * Create the default button map.
 */
	Default_map = (char **) getvm (N_BUTTON * sizeof (char *));
	for (i = 0; i < N_BUTTON; i++)
		Default_map[i] = usy_pstring ("beep");
	v.us_v_ptr = (char *) Default_map;
	usy_s_symbol (Bmaps, "default", SYMT_POINTER, &v);
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
	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "dm$config",
		Cur_config, SYMT_STRING, MAXNAME);
/*
 * Hook into the message handler.
 */
	msg_connect (dm_msg_handler, "Displaymgr");
	tty_watch (msg_get_fd (), msg_incoming);
/*
 * Interpret commands.
 */
	tw_DefTimeWidget (tw_cb, "Overall system time control");
	ui_get_command ("dm-initial", "DM>", dm_dispatcher, 0);
	dm_shutdown ();
}





dm_dispatcher (arg, cmds)
int arg;
struct ui_command *cmds;
/*
 * Deal with a display manager command.
 */
{
	switch (UKEY (*cmds))
	{
	   case DMC_CONFIG:
	   	def_config (UPTR (cmds[1]));
		break;

	   case DMC_DISPLAY:
	   	display (UPTR (cmds[1]));
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
	   	add (UPTR (cmds[1]), UPTR (cmds[2]), UPTR (cmds[3]));
		break;

	   case DMC_HISTORY:
	   	history (cmds + 1);
		break;

	   case DMC_REALTIME:
	   	realtime (cmds + 1);
		break;

	   default:
	   	ui_error ("(BUG): Unknown keyword: %d\n", UKEY (*cmds));
	}
	return (TRUE);
}




dm_msg_handler (msg)
struct message *msg;
{
	switch (msg->m_proto)
	{
	   case MT_DISPLAYMGR:
	   	dm_message (msg->m_from, (struct dm_msg *) msg->m_data);
		return;

	   case MT_MESSAGE:
	   	mh_message (msg);
		break;

	   case MT_TIMER:
	   	tl_DispatchEvent ((struct tm_time *) msg->m_data);
		break;

	   default:
	   	log_printf ("Funky message type %d in DM", msg->m_proto);
	};
}




mh_message (msg)
struct message *msg;
/*
 * Deal with a MESSAGE protocol msg.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;

	switch (tm->mh_type)
	{
	   case MH_SHUTDOWN:
	   	ui_printf ("Message handler shutdown -- I quit!\n");
		ui_finish ();
		exit (1);
	   default:
	   	ui_printf ("Unknown MESSAGE proto msg %d\n", tm->mh_type);
		break;
	}
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





display (name)
char *name;
/*
 * Actually put up a given configuration.
 */
{
	struct config *cfg;
	int type, win, disp_suspend ();
	union usy_value v;
	stbl new_table;
	char cfg_sname[MAXNAME];
/*
 * If this config is already the current config, we do nothing.
 */
	sprintf (cfg_sname, "cfg_%s", name);
	if (! strcmp (cfg_sname, Cur_config))
		return;
/*
 * Look up this config.
 */
 	if (! usy_g_symbol (Configs, name, &type, &v))
		ui_error ("Unknown configuration: '%s'\n", name);
	cfg = (struct config *) v.us_v_ptr;
/*
 * Get a new symbol table for this display config.
 */
	new_table = usy_c_stbl (cfg_sname);
	strcpy (Cur_config, cfg_sname);
/*
 * Go through and configure every window.
 */
	for (win = 0; win < cfg->c_nwin; win++)
	{
		struct cf_window *wp = cfg->c_wins + win;
	/*
	 * Fix up this window on the screen.
	 */
		if (! lookup_win (wp->cfw_name, FALSE))
		{
			create_win (wp);
			/* msg_incoming (msg_get_fd ()); */
			if ((win % 4) == 0)
				sleep (1);
		}
		else
			config_win (wp);
	/*
	 * Add it to the new config table, and remove it from the old.
	 */
	 	v.us_v_ptr = (char *) wp;
		usy_s_symbol (new_table, wp->cfw_name, SYMT_POINTER, &v);
		usy_z_symbol (Current, wp->cfw_name);
	}
/*
 * If there are any entries left in the current table, they represent
 * windows which must be suspended.  Do that, then get rid of the old table.
 */
	usy_traverse (Current, disp_suspend, 0, FALSE);
	usy_z_stbl (Current);
	Current = new_table;
}





disp_suspend (name, type, v, junk)
char *name;
int type, junk;
union usy_value v;
/*
 * Suspend this window.
 */
{
	struct dm_msg dmsg;

	dmsg.dmm_type = DM_SUSPEND;
	msg_send (name, MT_DISPLAYMGR, FALSE, (char *) &dmsg,
		sizeof (struct dm_msg));
	return (TRUE);
}





create_win (win)
struct cf_window *win;
/*
 * Create a new window graphics proc.
 */
{
	union usy_value v;
/*
 * Create the symbol table entry for this process.
 */
	v.us_v_ptr = (char *) win;
	usy_s_symbol (Windows, win->cfw_name, SYMT_POINTER, &v);
/*
 * Dig out the plot description for this window.
 */
	if (! (win->cfw_pd = pda_GetPD (win->cfw_desc)))
	{
		log_printf ("Window %s wants bad PD %s", win->cfw_name,
			win->cfw_desc);
		return;
	}
/*
 * Now actually fire off the process.
 */
	if (fork () == 0)
	{
		/* close (0); close (1); close (2); */
		close (msg_get_fd ());
		execlp (win->cfw_prog, /* win->cfw_prog, */ win->cfw_name,
			(char *) 0);
		printf ("Unable to exec '%s'\n", win->cfw_prog);
		exit (1);
	}
}




config_win (win)
struct cf_window *win;
/*
 * Configure this window.
 */
{
	struct dm_msg msg;
/*
 * Fill in the message structure.
 */
 	msg.dmm_type = DM_RECONFIG;
	msg.dmm_x = win->cfw_x;
	msg.dmm_y = win->cfw_y;
	msg.dmm_dx = win->cfw_dx;
	msg.dmm_dy = win->cfw_dy;
	/* strcpy (msg.dmm_pdesc, win->cfw_desc); */
/*
 * Ship it out.
 */
	msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, (char *) &msg,
		sizeof (struct dm_msg));
/*
 * Then ship over the PD too.
 */
	send_pd (win);
}






log_printf (va_alist)
va_dcl
/*
 * Send a message to the event logger.
 */
{
	va_list args;
	char mbuf[300], *fmt;
/*
 * Print up our message.
 */
	va_start (args);
	fmt = va_arg (args, char *);
	vsprintf (mbuf, fmt, args);
	va_end (args);
/*
 * Send it to the event logger.
 */
	msg_send ("Event logger", MT_LOG, 0, mbuf, strlen (mbuf) + 1);
}





dm_message (from, dmsg)
char *from;
struct dm_msg *dmsg;
/*
 * Deal with a display manager message.
 */
{
	struct dm_hello *dmh;
	struct dm_button *dmb;
	struct cf_window *win;

	switch (dmsg->dmm_type)
	{
	/*
	 * A new window checking in.  Finalize the data structure, and
	 * configure the window.
	 */
	   case DM_HELLO:
		dmh = (struct dm_hello *) dmsg;   	
		if (! (win = lookup_win (from, FALSE)))
		{
			log_printf ("Funky hello from '%s'", from);
			return;
		}
		log_printf ("Hello received from '%s'", from);
		win->cfw_win = dmh->dmm_win;
		config_win (win);
		break;
	/*
	 * A button click.
	 */
	   case DM_BUTTON:
	   	dmb = (struct dm_button *) dmsg;
		exec_button (from, dmb);
		break;

	   default:
	   	log_printf ("Funky DMSG type %d from %s\n", dmsg->dmm_type,
			from);
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
		log_printf ("NEWPD for win %s wants bad pd %s", window, pdesc);
		return;
	}
/*
 * Store the new info, and send it to the graphproc.
 */
	strcpy (win->cfw_desc, pdesc);
	win->cfw_pd = pd;
	send_pd (win);
}




exec_button (from, dmb)
char *from;
struct dm_button *dmb;
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
		log_printf ("Funky button from '%s'", from);
		return;
	}
/*
 * Set up the symbols, and exec the command.
 */
	v.us_v_ptr = from;
	usy_s_symbol (usy_g_stbl ("ui$variable_table"), "dm$button_window",
		SYMT_STRING, &v);
	ui_perform (win->cfw_bmap[dmb->dmm_button]);
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
		log_printf ("Unable to find pd '%s'", name);
		return;
	}
/*
 * Do the change.
 */
	pd_Store (pd, comp, param, value, SYMT_STRING);
	if (win)
		send_pd (win);
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
		log_printf ("Remove (%s %s) FAIL -- %s not active", pdn,
			comp, pdn);
		return;
	}
/*
 * Do the zap.
 */
	if (pd_RemoveComp (win->cfw_pd, comp))
		send_pd (win);
}





add (pdn, comp, dest)
char *pdn, *comp, *dest;
/*
 * Add this component from this PD to DEST.
 */
{
	plot_description pd = find_pd (pdn), pdcomp;
	struct cf_window *dwin = lookup_win (dest, TRUE);
/*
 * Sanity checks.
 */
	if (! pd)
	{
		log_printf ("FAIL: PD '%s' not found", pdn);
		return;
	}
	if (! dwin)
	{
		log_printf ("FAIL: Dest win '%s' not active", dest);
		return;
	}
/*
 * Pull out the component.  If it fails, ReadComponent will gripe, so
 * we don't have to.
 */
	if (! (pdcomp = pd_ReadComponent (pd, comp)))
		return;
/*
 * Add it to the destination.
 */
	pd_Merge (dwin->cfw_pd, pdcomp);
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
			msg_log ("HISTORY error -- win name must be string");
			return;
		}
		dwin = lookup_win (UPTR (*cmds), TRUE);
		if (! dwin)
		{
			msg_log ("HISTORY on unavailable win '%s'",
				UPTR (*cmds));
			return;
		}
	}
/*
 * Put together the message.
 */
	dmh.dmm_type = DM_HISTORY;
	dmh.dmm_time = UDATE (cmds[1]);
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
tw_cb (mode, t)
int mode;
time *t;
{
	struct dm_history dmh;
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

	   case Movie:
	   	msg_log ("I can't put *everybody* in movie mode!");
		return;

	   default:
	   	msg_log ("Funky mode (%d) in tw_cb", mode);
		return;
	}
/*
 * Now broadcast the result.
 */
	msg_send ("Graphproc", MT_DISPLAYMGR, TRUE, &dmh, sizeof (dmh));
}
