/*
 * The MOCCA display manager.
 */
# include <stdio.h>
# include <varargs.h>
# include <X11/Xlib.h>
# include "dm.h"
# include "dm_cmds.h"
# include "../msg/message.h"
# include <ui.h>
# include <ui_error.h>


/*
 * A configuration.
 */
# define MAXNAME	40
# define MAXPROG	80
struct cf_window
{
	char	cfw_name[MAXNAME];	/* The name of this window	*/
	Window	cfw_win;		/* It's X window		*/
	int	cfw_x, cfw_y;		/* The location of the window	*/
	int	cfw_dx, cfw_dy;		/* The size of the window	*/
	char	cfw_prog[MAXPROG];	/* The program it is running	*/
	char	cfw_desc[MAXNAME];	/* The plot description	name	*/
	char	**cfw_bmap;		/* The button map for this win	*/
};


# define MAXWIN		40
struct config
{
	char c_name[MAXNAME];		/* The name of this configuration */
	int c_nwin;			/* Number of windows in this config */
	struct cf_window c_wins[MAXWIN];/* The actual windows		*/
};


/*
 * The symbol tables holding configurations and windows.
 */
static stbl Configs;
static stbl Windows;

/*
 * This table holds info on the windows which are currently mapped.
 */
static stbl Current;
static char Cur_config[MAXNAME];

/*
 * The default window process.
 */
# define DEFPROG "graphproc"

/*
 * Button map information.
 */
# define B_LEFT 0
# define B_MIDDLE 1
# define B_RIGHT 2
# define N_BUTTON 3
typedef char *buttonmap[N_BUTTON];
stbl Bmaps;
char **Default_map;	/* The default button map	*/




main ()
{
	int dm_dispatcher (), dm_msg_handler (), msg_incoming (), i, get_pd ();
	int is_active ();
	union usy_value v;
	int type;
/*
 * Get the interface set up.
 */
	ui_init ("dm.lf", TRUE, FALSE);
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
	type = SYMT_STRING;
	uf_def_function ("pdesc", 1, &type, get_pd);
	uf_def_function ("active", 1, &type, is_active);
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
	   	
	   default:
	   	ui_error ("(BUG): Unknown keyword: %d\n", UKEY (*cmds));
	}
	return (TRUE);
}




def_config (name)
char *name;
/*
 * Define a new configuration.
 */
{
	struct config *cfg = NEW (struct config);
	int in_config ();
	union usy_value v;
/*
 * Initialize our new configuration structure.
 */
	strcpy (cfg->c_name, name);
	cfg->c_nwin = 0;
/*
 * Get the rest of the info.
 */
	ERRORCATCH
		ui_subcommand ("dm-config", "Config>", in_config, cfg);
	ON_ERROR
		relvm (cfg);
		RESIGNAL;
	ENDCATCH
/*
 * Define the new configuration.
 */
	v.us_v_ptr = (char *) cfg;
	usy_s_symbol (Configs, name, SYMT_POINTER, &v);
}





in_config (cfg, cmds)
struct config *cfg;
struct ui_command *cmds;
/*
 * Handle an internal config command.
 */
{
	struct cf_window *win;
	int in_window ();

	switch (UKEY (*cmds))
	{
	/*
	 * Add a new window.  Create the structure, add some default 
	 * values, then go off to get the specifics.
	 */
	   case DMC_WINDOW:
		win = cfg->c_wins + cfg->c_nwin;
		strcpy (win->cfw_name, UPTR (cmds[1]));
		win->cfw_x = UINT (cmds[2]);
		win->cfw_y = UINT (cmds[3]);
		win->cfw_dx = UINT (cmds[4]);
		win->cfw_dy = UINT (cmds[5]);
		win->cfw_bmap = Default_map;
		strcpy (win->cfw_prog, cmds[6].uc_ctype == UTT_END ?
			DEFPROG : UPTR (cmds[6]));
		strcpy (win->cfw_desc, "(undefined)");
		cfg->c_nwin++;
		ui_subcommand ("dm-window", "Window>", in_window, win);
		break;

	   case DMC_ENDCONFIG:
	   	return (FALSE);

	   default:
	   	ui_error ("(BUG): Unknown keyword: %d\n", UKEY (*cmds));
	}
	return (TRUE);
}






in_window (win, cmds)
struct cf_window *win;
struct ui_command *cmds;
/*
 * Deal with the internals of a window definition.
 */
{
	int type;
	union usy_value v;

	switch (UKEY (*cmds))
	{
	   case DMC_DESCRIPTION:
	   	strcpy (win->cfw_desc, UPTR (cmds[1]));
		break;

	   case DMC_BUTTONMAP:
	   	if (! usy_g_symbol (Bmaps, UPTR (cmds[1]), &type, &v))
			ui_cl_error (TRUE, UCOL (cmds[1]),
				"Unknown button map: '%s'", UPTR (cmds[1]));
		win->cfw_bmap = (char **) v.us_v_ptr;
		break;

	   case DMC_ENDWINDOW:
	   	return (FALSE);

	   default:
	   	ui_error ("(BUG) Unknown window kw: %d\n", UKEY (*cmds));
	}
	return (TRUE);
}






list ()
/*
 * List out the known configs.
 */
{
	int list_cfg ();

	usy_traverse (Configs, list_cfg, 0, FALSE);
}




list_cfg (name, type, v, junk)
char *name;
int type, junk;
union usy_value *v;
/*
 * List out a single configuration.
 */
{
	int i;
	struct config *cfg = (struct config *) v->us_v_ptr;

	ui_nf_printf ("Config '%s':\n", name);
	for (i = 0; i < cfg->c_nwin; i++)
	{
		struct cf_window *win = cfg->c_wins + i;

		ui_nf_printf ("\tWin '%s':\tat (%d, %d) size %dx%d, prog %s\n",
			win->cfw_name, win->cfw_x, win->cfw_y, win->cfw_dx,
			win->cfw_dy, win->cfw_prog);
		ui_nf_printf ("\t\tPD: %s\n", win->cfw_desc);
	}
	ui_printf ("\n");
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
 * Now actually fire off the process.
 */
	if (fork () == 0)
	{
		/* close (0); close (1); close (2); */
		execlp (win->cfw_prog, win->cfw_prog, win->cfw_name,
			(char *) 0);
		log_printf ("Unable to exec '%s'", win->cfw_prog);
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
	strcpy (msg.dmm_pdesc, win->cfw_desc);
/*
 * Ship it out.
 */
	msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, (char *) &msg,
		sizeof (struct dm_msg));
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
	struct dm_pdchange dmp;
/*
 * Only mapped windows, for now.
 */
 	if (! win)
		ui_error ("Window '%s' is not currently active", window);
/*
 * Put together a new configure message and send it out.
 */
	dmp.dmm_type = DM_PDCHANGE;
	strcpy (dmp.dmm_pdesc, pdesc);
	msg_send (window, MT_DISPLAYMGR, FALSE, &dmp, sizeof (dmp));
/*
 * Update the config entry.
 */
	strcpy (win->cfw_desc, pdesc);
}




def_bmap (name)
char *name;
/*
 * Define a button map by this name.
 */
{
	union usy_value v;
	int type, i, in_map ();
	char ** map;
/*
 * Try to look up the table and see if it already exists.
 */
	if (usy_g_symbol (Bmaps, name, &type, &v))
	{
		map = (char **) v.us_v_ptr;
		for (i = 0; i < N_BUTTON; i++)
			usy_rel_string (map[i]);
	}
	else
		map = (char **) getvm (N_BUTTON * sizeof (char *));
/*
 * All entries undefined by default.
 */
	for (i = 0; i < N_BUTTON; i++)
		map[i] = usy_string ("beep");
/*
 * Now parse the individual entries.
 */
	ui_subcommand ("dm-in-map", "Map>", in_map, map);
/*
 * Define this map.
 */
	v.us_v_ptr = (char *) map;
	usy_s_symbol (Bmaps, name, SYMT_POINTER, &v);
}






int
in_map (map, cmds)
char **map;
struct ui_command *cmds;
/*
 * Deal with the internal button map definitions.
 */
{
	switch (UKEY (*cmds))
	{
	   case B_LEFT:
	   case B_MIDDLE:
	   case B_RIGHT:
		map[UKEY (*cmds)] = usy_pstring (UPTR (cmds[1]));
		break;

	   case DMC_ENDMAP:
	   	return (FALSE);
	}
	return (TRUE);
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




get_pd (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * The "pdesc" command line function.
 */
{
	struct cf_window *win = lookup_win (argv->us_v_ptr, TRUE);
	
 	*rett = SYMT_STRING;
	retv->us_v_ptr = win ? usy_string (win->cfw_desc) :
			       usy_string ("INACTIVE");
}





is_active (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * The "active" command line function.  Return TRUE iff the given window
 * is currently active.
 */
{
	struct cf_window *win = lookup_win (argv->us_v_ptr, TRUE);
/*
 * Return the value.
 */
 	*rett = SYMT_BOOL;
	retv->us_v_int = win != 0;
}






badwin (name)
char *name;
/*
 * Complain about this window.
 */
{
	ui_error (lookup_win (name, FALSE) ? 
		"Window '%s' is not currently active" :
		"Window '%s' does not exist", name);
}



exchange (cwin1, cwin2)
char *cwin1, *cwin2;
/*
 * Exchange plot descriptions between these two windows.
 */
{
	struct cf_window *win1 = lookup_win (cwin1, TRUE);
	struct cf_window *win2 = lookup_win (cwin2, TRUE);
	struct dm_pdchange dmp;
	char *tmp;
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
	strcpy (win1->cfw_desc, win2->cfw_desc);
	strcpy (win2->cfw_desc, tmp);
	usy_rel_string (tmp);
/*
 * Tell the graphprocs about it.
 */
	dmp.dmm_type = DM_PDCHANGE;
	strcpy (dmp.dmm_pdesc, win1->cfw_desc);
	msg_send (cwin1, MT_DISPLAYMGR, FALSE, &dmp, sizeof (dmp));
	strcpy (dmp.dmm_pdesc, win2->cfw_desc);
	msg_send (cwin2, MT_DISPLAYMGR, FALSE, &dmp, sizeof (dmp));
}
