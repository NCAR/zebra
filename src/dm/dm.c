/*
 * The MOCCA display manager.
 */
# include <stdio.h>
# include <varargs.h>
# include <X11/Intrinsic.h>

# include <ui.h>
# include <ui_error.h>
# include "dm_vars.h"
# include "dm_cmds.h"
# include "../include/timer.h"

static char *rcsid = "$Id: dm.c,v 1.14 1990-12-04 16:06:53 corbet Exp $";

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

/*
 * Is sound enabled?
 */
static int SoundEnabled = FALSE;

/*
 * Do we restart windows which die?
 */
static bool Restart = TRUE;


# ifdef __STDC__
	int dm_shutdown (void);
	static bool ResolveLinks (struct config *, struct ui_command *);
	void SEChange (char *, int, int, int, SValue *, int, SValue *);
# else
	int dm_shutdown (void);
	static bool ResolveLinks (struct config *, struct ui_command *);
	void SEChange ();
# endif



main (argc, argv)
int argc;
char **argv;
{
	int dm_dispatcher (), dm_msg_handler (), msg_incoming (), get_pd ();
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
	fixdir_t ("DMLOADFILE", "/fcc/lib", "dm.lf", loadfile, ".lf");
	ui_init (loadfile, TRUE, FALSE);
	ui_setup ("DisplayMgr", &argc, argv, (char *) 0);
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
	usy_daemon (vtable, "soundenabled", SOP_WRITE, SEChange, 0);
	tty_watch (msg_get_fd (), msg_incoming);
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
	   	PickWin (UPTR (cmds[1]));
		break;

	   case DMC_SHUTDOWN:
	   	dm_shutdown ();
		break;

	   case DMC_SOUND:
		if (SoundEnabled)
		   	DoSound (UPTR (cmds[1]));
		break;

	   default:
	   	ui_error ("(BUG): Unknown keyword: %d\n", UKEY (*cmds));
	}
	return (TRUE);
}




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





display (cmds)
struct ui_command *cmds;
/*
 * Actually put up a given configuration.
 */
{
	struct config *cfg;
	int type, win, disp_suspend ();
	union usy_value v;
	stbl new_table;
	char cfg_sname[MAXNAME], *name = UPTR (*cmds);
/*
 * If this config is already the current config, we do nothing.
 */
	if (! strcmp (name, Cur_config))
		return;
	sprintf (cfg_sname, "cfg_%s", name);
/*
 * Look up this config.
 */
 	if (! usy_g_symbol (Configs, name, &type, &v))
		ui_error ("Unknown configuration: '%s'\n", name);
	cfg = (struct config *) v.us_v_ptr;
/*
 * If this window has linked PD's, go through and resolve them now.
 */
	if (cfg->c_nlink > 0)
		if (! ResolveLinks (cfg, cmds + 1))
			return;
/*
 * Get a new symbol table for this display config.
 */
	new_table = usy_c_stbl (cfg_sname);
	strcpy (Cur_config, name);
/*
 * Go through and configure every window.
 */
	for (win = 0; win < cfg->c_nwin; win++)
	{
		struct cf_window *wp = cfg->c_wins + win, *exist;
	/*
	 * If this is a widget window, deal with it separately.
	 */
	 	if (wp->cfw_flags & CF_WIDGET)
		{
			uw_ForceOverride (wp->cfw_name);
			uw_SetGeometry (wp->cfw_name, wp->cfw_x, wp->cfw_y,
				wp->cfw_dx, wp->cfw_dy);
			uw_popup (wp->cfw_name);
		}
	/*
	 * Otherwise it's a graphics window.  If it does not yet exist, we
	 * have to create it.  The sleep is there to avoid "connection
	 * refused" problems caused by too many processes trying to hook
	 * into the X server at once.
	 */
		else if (! (exist = lookup_win (wp->cfw_name, FALSE)))
		{
			msg_ELog (EF_DEBUG, "Create win %s", wp->cfw_name);
			create_win (wp);
			if ((win % 4) == 0)
				sleep (1);
		}
	/*
	 * If it does exist, deal with the PD, and send it a new config.
	 */
		else
		{
			msg_ELog (EF_DEBUG, "Existing win %s", wp->cfw_name);
			if (! wp->cfw_linkpar && ! wp->cfw_forcepd)
				wp->cfw_pd = exist->cfw_pd; /* no copy! */
			wp->cfw_win = exist->cfw_win;
			config_win (wp);
		}
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




static bool
ResolveLinks (cfg, cmds)
struct config *cfg;
struct ui_command *cmds;
/*
 * Resolve all linked pd's in this config.
 */
{
	int i;
	struct cf_window *win;
/*
 * First, go through and make sure we have the right number of params.
 */
	for (i = 0; i < cfg->c_nlink; i++)
		if (cmds[i].uc_ctype == UTT_END)
		{
			msg_ELog (EF_PROBLEM,
				"%d link parameters needed, %d given",
				cfg->c_nlink, i - 1);
			return (FALSE);
		}
	if (cmds[i].uc_ctype != UTT_END)
		msg_ELog (EF_PROBLEM, "Too many link parameters given");
/*
 * Go through now and resolve each one.
 */
	for (i = 0; i < cfg->c_nwin; i++)
	{
		struct cf_window *linkwin = cfg->c_wins + i;
		int link = linkwin->cfw_linkpar;
	/*
	 * If no link parameter in this window, no work to do.
	 */
	 	if (! link)
			continue;
	/*
	 * Do some sanity checking.
	 */
	 	if (link > cfg->c_nlink)
		{
			msg_ELog (EF_PROBLEM, "Win %s wants too high link %d",
				linkwin->cfw_name, link);
			return (FALSE);
		}
	/*
	 * Dig out the link window.
	 */
	 	if (! (win = lookup_win (UPTR (cmds[link - 1]), TRUE)))
		{
			msg_ELog (EF_PROBLEM, "Link to bad window '%s'",
				UPTR (cmds[link - 1]));
			return (FALSE);
		}
	/*
	 * Link it.  Since the given win is required to be in the current
	 * display configuration, we know that the PD has to be realized.
	 */
	 	linkwin->cfw_pd = win->cfw_pd;
	}
	return (TRUE);
}



disp_suspend (name, type, v, junk)
char *name;
int type, junk;
SValue *v;
/*
 * Suspend this window.
 */
{
	struct dm_msg dmsg;
	struct cf_window *wp = (struct cf_window *) v->us_v_ptr;
/*
 * If it's a widget, just pop it down.
 */
	if (wp->cfw_flags & CF_WIDGET)
		uw_popdown (name);
/*
 * Otherwise we have to tell it to go away.
 */
 	else
	{
		dmsg.dmm_type = DM_SUSPEND;
		msg_send (name, MT_DISPLAYMGR, FALSE, (char *) &dmsg,
			sizeof (struct dm_msg));
	}
	return (TRUE);
}





create_win (win)
struct cf_window *win;
/*
 * Create a new window graphics proc.
 */
{
	union usy_value v;
	int i;
/*
 * Create the symbol table entry for this process.
 */
	v.us_v_ptr = (char *) win;
	usy_s_symbol (Windows, win->cfw_name, SYMT_POINTER, &v);
/*
 * If this is a nongraphic window, we don't create it.  We just ping it, and
 * wait for the hello.
 */
	win->cfw_args[0] = win->cfw_name;
	if (win->cfw_nongraph)
	{
		struct dm_hello dmh;
		dmh.dmm_type = DM_HELLO;
		msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, &dmh,
			sizeof (dmh));
	}
/*
 * Otherwise actually fire off the process.
 */
	else if (fork () == 0)
	{
		/* close (0); close (1); close (2); */
		close (msg_get_fd ());
		execv (win->cfw_prog, win->cfw_args);
		printf ("Unable to exec '%s'\n", win->cfw_prog);
		perror (win->cfw_prog);
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
	bool created = FALSE;
/*
 * Fill in the message structure.
 */
 	msg.dmm_type = DM_RECONFIG;
	msg.dmm_x = win->cfw_x;
	msg.dmm_y = win->cfw_y;
	msg.dmm_dx = win->cfw_dx;
	msg.dmm_dy = win->cfw_dy;
/*
 * Ship it out.  If this is a nongraphic window, we then quit.
 */
	msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, (char *) &msg,
		sizeof (struct dm_msg));
	if (win->cfw_nongraph)
		return;
/*
 * Dig out the plot description for this window.  Clone it so that our changes
 * do not affect the other invocations of this PD.
 */
	if (! win->cfw_pd)
	{
		if (win->cfw_pd = pda_GetPD (win->cfw_desc))
			win->cfw_pd = pd_CopyPD (win->cfw_pd);
		else
		{
			msg_ELog (EF_EMERGENCY, "Window %s wants bad PD %s",
				win->cfw_name, win->cfw_desc);
			return;
		}
		created = TRUE;
	}
/*
 * Then ship over the PD too.
 */
	if (win->cfw_linkpar || win->cfw_forcepd|| created ||win->cfw_tmpforce)
		send_pd (win);
	win->cfw_tmpforce = FALSE;
/*
 * And the button maps.
 */
	SendButtonMap (win);
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
 * Pull out the component.  If it fails, ReadComponent will gripe, so
 * we don't have to.
 */
	if (! (pdcomp = pd_ReadComponent (pd, comp)))
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
 * Now broadcast the result.
 */
	msg_send ("Graphproc", MT_DISPLAYMGR, TRUE, &dmh, sizeof (dmh));
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
	prt.tr_time = *when;
	prt.tr_scale = 1;
	msg_send ("Timer", MT_TIMER, FALSE, &prt, sizeof (prt));
}






void
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
}
