/* 
 * User interface related DM code.
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
# include <ui.h>
# include <ui_error.h>
# include "dm_vars.h"
# include "dm_cmds.h"
MAKE_RCSID ("$Id: dm_ui.c,v 2.3 1992-02-07 21:03:48 corbet Exp $")






def_config (cmds)
struct ui_command *cmds;
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
	strcpy (cfg->c_name, UPTR (*cmds));
	cfg->c_nwin = 0;
/*
 * Link counts.
 */
	if (cmds[1].uc_ctype == UTT_END)
		cfg->c_nlink = 0;
	else if (UINT (cmds[1]) < 0 || UINT (cmds[1]) >= 10)
		ui_cl_error (TRUE, cmds[1].uc_col, "Preposterous link count");
	else
		cfg->c_nlink = UINT (cmds[1]);
/*
 * Get the rest of the info.
 */
	ERRORCATCH
		ui_subcommand ("dm-config", "Config>", in_config, (long) cfg);
	ON_ERROR
		relvm (cfg);
		RESIGNAL;
	ENDCATCH
/*
 * Define the new configuration.
 */
	v.us_v_ptr = (char *) cfg;
	usy_s_symbol (Configs, UPTR (*cmds), SYMT_POINTER, &v);
}





in_config (cfg, cmds)
struct config *cfg;
struct ui_command *cmds;
/*
 * Handle an internal config command.
 */
{
	struct cf_window *win;
	int in_window (), arg;

	switch (UKEY (*cmds))
	{
	/*
	 * Add a new window.  Create the structure, add some default 
	 * values, then go off to get the specifics.
	 */
	   case DMC_WINDOW:
	   /*
	    * Get a new window structure and initialize it.
	    */
		win = cfg->c_wins + cfg->c_nwin;
		strcpy (win->cfw_name, UPTR (cmds[1]));
		win->cfw_win = 0;
		win->cfw_linkpar = 0;
		win->cfw_linksrc = 0;
		win->cfw_x = UINT (cmds[2]);
		win->cfw_y = UINT (cmds[3]);
		win->cfw_dx = UINT (cmds[4]);
		win->cfw_dy = UINT (cmds[5]);
		win->cfw_bmap = Default_map;
		win->cfw_nongraph = win->cfw_forcepd = FALSE;
		win->cfw_pd = 0;
		win->cfw_ncroak = win->cfw_flags = 0;
		strcpy (win->cfw_prog, cmds[6].uc_ctype == UTT_END ?
			DEFPROG : UPTR (cmds[6]));
		cmds += (cmds[6].uc_ctype == UTT_END) ? 5 : 6;
		strcpy (win->cfw_desc, "(undefined)");
	   /*
	    * Soak up the args.
	    */
	    	for (arg = 1; cmds[arg].uc_ctype != UTT_END; arg++)
			win->cfw_args[arg] = usy_string (UPTR (cmds[arg]));
		win->cfw_args[arg] = (char *) 0;
	   /*
	    * Now get the rest of the stuff for this window.
	    */
		cfg->c_nwin++;
		ui_subcommand ("dm-window", "Window>", in_window, (long) win);
		break;
	/* 
	 * Maybe it's a widget.
	 */
	   case DMC_WIDGET:
		win = cfg->c_wins + cfg->c_nwin;
		strcpy (win->cfw_name, UPTR (cmds[1]));
		win->cfw_win = 0;
		win->cfw_x = UINT (cmds[2]);
		win->cfw_y = UINT (cmds[3]);
		win->cfw_dx = UINT (cmds[4]);
		win->cfw_dy = UINT (cmds[5]);
		win->cfw_flags = CF_WIDGET;
		cfg->c_nwin++;
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
		if (win->cfw_linkpar)
			ui_warning ("LINKPD overrides DESCRIPTION");
		break;

	   case DMC_LINKPD:
		if (UINT (cmds[1]) <= 0 || UINT (cmds[1]) > 10)
			ui_cl_error (TRUE, cmds[1].uc_col,
				"Preposterous link count");
	   	if (! strcmp (win->cfw_desc, "(undefined"))
			ui_warning ("LINKPD overrides DESCRIPTION");
		win->cfw_linkpar = UINT (cmds[1]);
		sprintf (win->cfw_desc, "dm$link%02d", win->cfw_linkpar);
		break;

	   case DMC_BUTTONMAP:
	   	if (! usy_g_symbol (Bmaps, UPTR (cmds[1]), &type, &v))
			ui_cl_error (TRUE, UCOL (cmds[1]),
				"Unknown button map: '%s'", UPTR (cmds[1]));
		win->cfw_bmap = (ButtonMap *) v.us_v_ptr;
		break;

	   case DMC_ENDWINDOW:
	   	return (FALSE);

	   case DMC_NONGRAPHIC:
	   	win->cfw_nongraph = TRUE;
		break;

	   case DMC_FORCEPD:
	   	win->cfw_forcepd = TRUE;
		break;

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






def_bmap (name)
char *name;
/*
 * Define a button map by this name.
 */
{
	union usy_value v;
	int type, i, in_map ();
	ButtonMap *map;
/*
 * Try to look up the table and see if it already exists.
 */
	if (usy_g_symbol (Bmaps, name, &type, &v))
		map = (ButtonMap *) v.us_v_ptr;
	else
		map = ALLOC (ButtonMap);
	map->db_nentry = 0;
	strcpy (map->db_name, name);
/*
 * Now parse the individual entries.
 */
	ui_subcommand ("dm-in-map", "Map>", in_map, (long) map);
/*
 * Define this map.
 */
	v.us_v_ptr = (char *) map;
	usy_s_symbol (Bmaps, name, SYMT_POINTER, &v);
}






int
in_map (map, cmds)
ButtonMap *map;
struct ui_command *cmds;
/*
 * Deal with the internal button map definitions.
 */
{
	struct dm_evbind *bind = map->db_bindings + map->db_nentry++;

	switch (UKEY (*cmds))
	{
	   case DMC_IGNORE:
	   	bind->dmm_action = AC_Ignore;
		break;

	   case DMC_DM:
	   	bind->dmm_action = AC_Report;
		break;

	   case DMC_LOCAL:
		bind->dmm_action = AC_CommandText;
		break;

	   case DMC_MENU:
	   	bind->dmm_action = AC_PopupMenu;
		break;

	   case DMC_ENDMAP:
	   	map->db_nentry--;
	   	return (FALSE);
	}
/*
 * For commands that actually bind keys, remember the code, and, if
 * relevant, the action.
 */
	strcpy (bind->dmm_code, UPTR (cmds[1]));
	if (bind->dmm_action != AC_Ignore)
		strcpy (bind->dmm_adata, UPTR (cmds[2]));

	return (TRUE);
}






pd_defined (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * the pd_defined CLF.
 *
 *	pd_defined (pd, comp, param)
 */
{
	plot_description find_pd ();
	plot_description pd = find_pd (argv[0].us_v_ptr);
	char junk[200];

	*rett = SYMT_BOOL;
	if (! pd)
		retv->us_v_int = FALSE;
	else 
		retv->us_v_int = pd_Retrieve (pd, argv[1].us_v_ptr,
			argv[2].us_v_ptr, junk, SYMT_STRING);
}






pd_param (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * The pd_param command line function.
 *
 *	pd_param (pd, comp, param, type)
 */
{
	char tmp[500];
	plot_description find_pd ();
	plot_description pd = find_pd (argv[0].us_v_ptr);
	int type = uit_int_type (argv[3].us_v_ptr);

	*rett = SYMT_STRING;
	if (! pd)
		retv->us_v_ptr = usy_string ("Bad PD");
	else if (type == SYMT_UNDEFINED)
		retv->us_v_ptr = usy_string ("Bad type");
	else if (! pd_Retrieve (pd, argv[1].us_v_ptr, argv[2].us_v_ptr, 
			tmp, type))
		retv->us_v_ptr = usy_string ("(Undefined)");
	else if (type == SYMT_STRING)
		retv->us_v_ptr = usy_string (tmp);
	else
	{
		*rett = type;
		memcpy (retv, tmp, sizeof (date));	/* XXX */
	}
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



