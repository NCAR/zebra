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
# include <stdio.h>
# include <string.h>
# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <dm.h>
# include "dm_vars.h"
# include "dm_cmds.h"

MAKE_RCSID ("$Id: dm_ui.c,v 2.13 1995-05-24 00:14:33 granger Exp $")


static int in_pd FP((raw_plot_description *rpd, struct ui_command *cmds));
static int in_config FP((struct config *cfg, struct ui_command *cmds));
static int in_window FP((struct cf_window *win, struct ui_command *cmds));
static void rpd_append FP((raw_plot_description *rpd, char *src));
static int in_map FP((ButtonMap *map, struct ui_command *cmds));


void
def_config (cmds)
struct ui_command *cmds;
/*
 * Define a new configuration.
 */
{
	struct config *cfg;
/*
 * Get a new, clean configuration structure.
 */
	cfg = dg_NewConfig (UPTR (*cmds));
/*
 * Figure link counts.
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
	dg_TableAdd (cfg);
}




static int
in_config (cfg, cmds)
struct config *cfg;
struct ui_command *cmds;
/*
 * Handle an internal config command.
 */
{
	struct cf_window *win;
	char *exec;
	int narg;
	char *args[ MAXARG ]; /* XXX */
	ProcessClass *pc;
	WinClass wc = W_Unknown;
	int key;

	/*
	 * Determine our window class based on the command type
	 */
	switch ((key = UKEY (*cmds)))
	{
	   case DMC_WINDOW:
	   case DMC_GRAPHIC:
		wc = W_Graphic;
		break;
	   case DMC_PROCESS:
		wc = W_NonGraphic;
		break;
	   case DMC_WIDGET:
		wc = W_Widget;
		break;
	   case DMC_ENDCONFIG:		/* time to go... */
	   	return (FALSE);
	   default:
	   	ui_error ("(BUG): Unknown keyword: %d\n", UKEY (*cmds));
	}
	/*
	 * Now we can make a new configuration window and store the
	 * info common to all windows.
	 */
	win = dg_NewWindow (cfg, wc, UPTR (cmds[1]));
	win->cfw_x = UINT (cmds[2]);
	win->cfw_y = UINT (cmds[3]);
	win->cfw_dx = UINT (cmds[4]);
	win->cfw_dy = UINT (cmds[5]);
	/*
	 * If its a widget we're done, otherwise we need to grab
	 * either a process class or an argument list.
	 */
	if (wc == W_Widget)
		return (TRUE);
	cmds += 6;
	if (key == DMC_WINDOW)
	{
		exec = (cmds->uc_ctype == UTT_END) ?
			DEFAULT_EXEC : UPTR (*cmds++);
		/*
		 * Soak up the args and try to match them to an existing class.
		 * Note that this args list does not include an argv[0].  Arg
		 * matching and class defn expect just such an arg list.
		 */
		for (narg = 0; cmds[narg].uc_ctype != UTT_END; narg++)
			args[narg] = UPTR (cmds[narg]);
		args[narg] = (char *) 0;
		pc = dp_MatchArgs (exec, narg, args);
		/*
		 * If no class matched, define one using the window name
		 */
		if (! pc)
			pc = dp_DefineClass (win->cfw_name, exec, args, 0);
		strcpy (win->cfw_pcname, pc->pc_name);
	}
	else
	{
		/*
		 * The last command token will be a process class
		 */
		if (cmds->uc_ctype == UTT_END)
			strcpy (win->cfw_pcname, DEFAULT_PROCESS);
		else
			strcpy (win->cfw_pcname, UPTR (*cmds));
	}
	/*
	 * If this is a graphic window we need more information.
	 */
	if (wc == W_Graphic)
	{
		ui_subcommand ("dm-window", "Graphic>", in_window, (long)win);
	}
	return (TRUE);
}





static int
in_window (win, cmds)
struct cf_window *win;
struct ui_command *cmds;
/*
 * Deal with the internals of a window definition.
 */
{
	int type;
	union usy_value v;
	struct cf_graphic *g = win->cfw_graphic;

	if (UKEY (*cmds) == DMC_ENDWINDOW || UKEY (*cmds) == DMC_ENDGRAPHIC)
	   	return (FALSE);
/*
 * If we've been told we're nongraphic, everything else is irrelevant
 */
	if (! g)
	{
		ui_warning ("irrelevant keyword for nongraphic window");
		return (TRUE);
	}
/*
 * The rest of the keywords are only for graphic windows
 */
	switch (UKEY (*cmds))
	{
	   case DMC_DESCRIPTION:
		strcpy (g->g_desc, UPTR (cmds[1]));
		if (g->g_linkpar)
			ui_warning ("LINKPD overrides DESCRIPTION");
		break;

	   case DMC_LINKPD:
		if (UINT (cmds[1]) <= 0 || UINT (cmds[1]) > 10)
			ui_cl_error (TRUE, cmds[1].uc_col,
				"Preposterous link count");
	   	if (strcmp (g->g_desc, "(undefined)"))
			ui_warning ("LINKPD overrides DESCRIPTION");
		g->g_linkpar = UINT (cmds[1]);
		sprintf (g->g_desc, "dm$link%02d", g->g_linkpar);
		break;

	   case DMC_BUTTONMAP:
	   	if (! usy_g_symbol (Bmaps, UPTR (cmds[1]), &type, &v))
			ui_cl_error (TRUE, UCOL (cmds[1]),
				"Unknown button map: '%s'", UPTR (cmds[1]));
		g->g_bmap = (ButtonMap *) v.us_v_ptr;
		break;

	   case DMC_NONGRAPHIC:
		/*
		 * Damn.  Our original assumption has been reversed.
		 */
	   	win->cfw_class = W_NonGraphic;
		free (win->cfw_graphic);
		win->cfw_graphic = g = NULL;
		break;

	   case DMC_FORCEPD:
	   	g->g_forcepd = TRUE;
		break;

	   default:
	   	ui_error ("(BUG) Unknown window kw: %d\n", UKEY (*cmds));
	}
	return (TRUE);
}



void
def_pd (cmds)
struct ui_command *cmds;
/*
 * Define a plot description.  If a name argument is supplied, use it as
 * the name of the plot description.  Otherwise, use the global pd-name
 * parameter.  If all else fails, use the name of the first component,
 * else report an error.
 */
{
	raw_plot_description rpd;
	plot_description pd;
	char *pdname;
	char name[512];
/*
 * Enter the dm-description state and wait for parameter and component
 * commands.
 */
	rpd.rp_len = 0;
	ERRORCATCH
		ui_subcommand ("dm-description", "PD>", in_pd, (long) &rpd);
	ON_ERROR
		if (rpd.rp_len)
			free (rpd.rp_data);
		RESIGNAL;
	ENDCATCH
/*
 * Make sure we got something
 */
	if (! rpd.rp_len)
		return;
/*
 * Try to compile the raw plot description
 */
	pd = pd_Load (&rpd);
	if (! pd)
	{
		free (rpd.rp_data);
		return ;
	}
/*
 * Work on assigning a name to this plot description
 */
	pdname = NULL;
	if (cmds[0].uc_ctype != UTT_END)
	{
		pdname = UPTR(cmds[0]);
	}
	else if (pd_Retrieve (pd, "global", "pd-name", name, SYMT_STRING) &&
		 strcmp (name, "UNDEFINED"))
	{
		pdname = name;
	}
	else if (pd_Retrieve (pd, "defaults", "pd-name", name, SYMT_STRING))
	{
		pdname = name;
	}
	else
	{
		char **comps = pd_CompList (pd);
	/*
	 * Try the first component
	 */
		if (comps)
			pdname = comps[0];
	}
	if (! pdname || ! pdname[0])
	{
		ui_warning ("could not assign a name to plot description");
		pd_Release (pd);
	}
	else
	{
		/*
		 * Finally, we can store the new plot description
		 */
		pd_Store (pd, "global", "pd-name", pdname, SYMT_STRING);
		pda_StorePD (pd, pdname);
	}
	free (rpd.rp_data);
	return ;
}



static void
rpd_append (rpd, src)
raw_plot_description *rpd;
char *src;
/*
 * Make sure there is space for characters in src[] and copy them into rpd.
 * Note that for this to be done efficiently, it relies on realloc()
 * allocating in sufficient blocks to avoid copying on every call.
 */
{
	int len = rpd->rp_len + strlen(src) + 1;

	if (rpd->rp_len == 0)
	{	
		rpd->rp_data = (char *) malloc (len);
		rpd->rp_data[0] = '\0';
	}
	else
		rpd->rp_data = (char *) realloc (rpd->rp_data, len);

	strcat (rpd->rp_data, src);
	rpd->rp_len = len - 1;
}

	


static int
in_pd (rpd, cmds)
raw_plot_description *rpd;
struct ui_command *cmds;
/*
 * Handle an internal plot description command.  A parameter command simply
 * tacks on a new parameter to the raw plot description.  A component or global
 * command begins a new component and ends any previous one.
 *
 * If a component is defined before a global, insert the global component 
 * with a place-holder for the name.
 */
{
	switch (UKEY (*cmds))

	{
	   case DMC_GLOBAL:
		rpd_append (rpd, "global\n");
		break;

	   case DMC_COMPONENT:
		if (rpd->rp_len == 0)
			rpd_append (rpd, "global\n\tpd-name:\tUNDEFINED\n");
		rpd_append (rpd, UPTR(cmds[1]));
		rpd_append (rpd, "\n");
		break;
	   
	   case DMC_COMPPARAM:
		rpd_append (rpd, "\t");
		rpd_append (rpd, UPTR(cmds[1]));
		rpd_append (rpd, ":\t");
		rpd_append (rpd, UPTR(cmds[2]));
		rpd_append (rpd, "\n");
		break;

	   case DMC_ENDPD:
	   	return (FALSE);

	   default:
	   	ui_error ("(BUG): Unknown keyword: %d\n", UKEY (*cmds));
	}
	return (TRUE);
}



void
Prototype (cmds)
struct ui_command *cmds;
/*
 * Extract the args from the command list and define a process class
 */
{
	int explicit;
	int override;
	char *name;
	char *exec;
	int narg;
	char *args[MAXARG];

	override = 1;	/* the defaults */
	explicit = 0;
	while (cmds->uc_ctype == UTT_KW)
	{
		switch (UKEY (*cmds))
		{
		case DMC_EXPLICIT:
			explicit = 1;
			break;
		case DMC_REPLACE:
			override = 1;
			break;
		case DMC_FALLBACK:
			override = 0;
			break;
		}
		++cmds;
	}
	name = UPTR (*cmds++);
	exec = UPTR (*cmds++);
	for (narg = 0; cmds[narg].uc_ctype != UTT_END; narg++)
		args[narg] = UPTR (cmds[narg]);
	args[narg] = (char *) 0;
	if (override || (! dp_LookupClass (name)))
		dp_DefineClass (name, exec, args, explicit);
}



void
WritePD (name, filename)
char *name;	/* Name of the plot description to list 	*/
char *filename;	/* file to write or dir path, NULL for stdout	*/
{
	plot_description pd;

	pd = find_pd (name);
	if (! pd)
	{
		ui_error ("unknown plot description: '%s'\n", name);
	}
	else
		pdwrite (pd, filename);
}



void
StorePD (name, copyname, filename)
char *name;	/* Name of the plot description to list 	*/
char *copyname;	/* Name to give the pd when written		*/
char *filename;	/* file to write or dir path, NULL for stdout	*/
{
	plot_description pd, copy;

	pd = find_pd (name);
	if (! pd)
	{
		ui_error ("unknown plot description: '%s'\n", name);
		return;
	}
	copy = pd_CopyPD (pd);
	pd_Store (copy, "global", "pd-name", copyname, SYMT_STRING);
	pdwrite (copy, filename);
	pd_Release (copy);
}



void
CopyPD (name, copyname)
char *name;	/* Name of the source plot description (or window) */
char *copyname;	/* Name of the dest plot description		   */
{
	plot_description pd, copy;

	pd = find_pd (name);
	if (! pd)
	{
		ui_error ("unknown plot description: '%s'\n", name);
		return;
	}
	copy = pd_CopyPD (pd);
	pd_Store (copy, "global", "pd-name", copyname, SYMT_STRING);
	pda_StorePD (copy, copyname);
}



void
CopyComp (pdname, comp, copyname)
char *pdname;	/* Name of the source plot description (or window) */
char *comp;	/* Name of the source component			   */
char *copyname;	/* Name of the dest component description	   */
/*
 * We try to read a global component from the source pd in addition to the
 * named component.  If that fails we just store the single component
 * under its new name.
 */
{
	plot_description pd, copy, global;

	pd = find_pd (pdname);
	if (! pd)
	{
		ui_error ("unknown plot description: '%s'\n", pdname);
		return;
	}
	copy = pd_ReadComponent (pd, comp, copyname);
	if (! copy)
	{
		ui_error ("source component '%s' not found; could not copy",
			  comp);
		return;
	}
	global = pd_ReadComponent (pd, "global", "global");
	if (global)
	{
		/*
		 * Once we add a component to another plot description, it
		 * becomes part of that pd (rather than being copied) and 
		 * should not released.  I learned that the hard way.
		 */
		pd_AddComponent (global, copy, 1);
		pd_Store (global, "global", "pd-name", copyname, SYMT_STRING);
	}
	else
		global = copy;
	pda_StorePD (global, copyname);
}



void
MergeComp (pdname, comp, destname, destcomp)
char *pdname;	/* Name of the source plot description (or window) */
char *comp;	/* Name of the source component			   */
char *destname;	/* Name of the dest plot description		   */
char *destcomp; /* Name of the dest component			   */
{
	plot_description pd, dest;

	pd = find_pd (pdname);
	if (! pd)
	{
		ui_error ("unknown plot description: '%s'\n", pdname);
		return;
	}
	dest = find_pd (destname);
	if (! dest)
	{
		ui_error ("unknown plot description: '%s'\n", destname);
		return;
	}
	pd_MergeComp (dest, destcomp, pd, comp);
}



void
def_bmap (name)
char *name;
/*
 * Define a button map by this name.
 */
{
	union usy_value v;
	int type;
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






static int
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




int
pd_defined (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
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
	return (0);
}



int
pd_complist (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * Return a space-separated (foreachable) list of the components in
 * the given PD.
 */
{
	char tmp[500];
	plot_description find_pd ();
	plot_description pd = find_pd (argv[0].us_v_ptr);
	char **comps;

	*rett = SYMT_STRING;
	tmp[0] = '\0';
/*
 * If they gave us a bogus PD name, give them a bogus answer back.
 */
	if (! pd)
	{
		retv->us_v_ptr = usy_string ("bogus");
		return (0);
	}
/*
 * Get the components and make up a nice string.
 */
	for (comps = pd_CompList (pd); *comps; comps++)
	{
		strcat (tmp, *comps);
		if (comps[1])
			strcat (tmp, " ");
	}
/*
 * Done.
 */
	retv->us_v_ptr = usy_string (tmp);
	return (0);
}



int
nvalue (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * CLF: nvalue (pd, comp, param)
 *
 * Returns the number of values specified for this parameter.
 */
{
	char *vals[32], tmp[500];
	plot_description find_pd ();
	plot_description pd = find_pd (argv[0].us_v_ptr);

	*rett = SYMT_INT;
	if (pd && pd_Retrieve (pd, argv[1].us_v_ptr, argv[2].us_v_ptr, tmp,
			       SYMT_STRING))
		retv->us_v_int = CommaParse (tmp, vals);
	else
		retv->us_v_int = 0;
	return (0);
}



int
NthComponent (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * NthComp command line function:
 *
 *	set comp_name NthComp (pd, n)
 */
{
	plot_description find_pd ();
	plot_description pd = find_pd (argv[0].us_v_ptr);
	char **comps = pd_CompList (pd);
	int i;
/*
 * Pass through the list of components to make sure it doesn't end before
 * the one they want.
 */
	for (i = 0; i < argv[1].us_v_int; i++)
		if (! comps[i])
			break;
	if (argv[1].us_v_int < 0 || ! comps[i])
		retv->us_v_ptr = usy_string ("(Undefined)");
	else
		retv->us_v_ptr = usy_string (comps[i]);
	*rett = SYMT_STRING;
	return (0);
}




int
pd_param (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * The pd_param command line function.
 *
 *	pd_param (pd, comp, param, type)
 *	PDParam (pd, comp, param)
 */
{
	char tmp[500];
	plot_description find_pd ();
	plot_description pd = find_pd (argv[0].us_v_ptr);
	int type = (narg == 4) ? uit_int_type (argv[3].us_v_ptr) : SYMT_STRING;

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
	return (0);
}
	



int
get_pd (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * The "pdesc" command line function.
 */
{
	struct cf_window *win = dg_CurrentWindow (argv->us_v_ptr);
	
 	*rett = SYMT_STRING;
	if (IsGraphic (win))
	{
		if (win->cfw_process && win->cfw_process->p_state == P_ACTIVE)
			retv->us_v_ptr = usy_string (win->cfw_graphic->g_desc);
		else
			retv->us_v_ptr = usy_string ("INACTIVE");
	}
	else
	{
		retv->us_v_ptr = usy_string ("NONGRAPHIC");
	}
	return (0);
}




int
is_active (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * The "active" command line function.  Return TRUE iff the given window
 * is currently active.
 */
{
	struct cf_window *win = dg_CurrentWindow (argv->us_v_ptr);
/*
 * Return the value.
 */
 	*rett = SYMT_BOOL;
	retv->us_v_int = (win != 0);
	return (0);
}





void
badwin (name)
char *name;
/*
 * Complain about this window.
 */
{
	ui_error (dg_AnyWindow (name) ? 
		  "Window '%s' is not currently active" :
		  "Window '%s' does not exist", name);
}



