/*
 * Code for editing and saving of display configurations.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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
# include <fcntl.h>
# include <ctype.h>
# include <errno.h>
# include <string.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>

# include <ui.h>
# include <ui_error.h>

# include <dm.h>
# include "dm_vars.h"
# include "dm_cmds.h"

MAKE_RCSID ("$Id: dm_config.c,v 1.23 1995-05-24 00:13:48 granger Exp $")

/*
 * Exported variables
 */
stbl Configs = NULL;
stbl Current = NULL;
char Cur_config[MAXNAME];

/*
 * Private variables
 */
static char ConfigDir[CFG_FILEPATH_LEN]; /* Default directory for configs */
static char ConfigPD[CFG_FILEPATH_LEN];  /* Where to save plot descriptions */
static char ConfigPath[CFG_SEARCHPATH_LEN];/* Path to search for configs */

/*
 * The amount of space to subtract from the height of graphics windows --
 * this is here to allow people to easily compensate for window manager
 * obnoxiousness without having to rewrite configurations.
 */
static int TBSpace = 0;

/*
 * How long and how often to sleep while creating windows.
 */
static int SleepAfter = 4;
static int SleepFor = 1;

/*
 * Our local stuff.
 */
static void dg_UpdateConfig FP ((struct config *current, struct config *dest));
#ifdef notdef
static void dg_GetGeometry FP ((struct cf_window *win, 
				Position *x, Position *y,
				Dimension *width, Dimension *height));
static int  dg_AwaitGeometry FP((Message *msg, struct dm_msg *ret));
#endif
static void dg_SavePD FP ((FILE *, char *, struct cf_window *));
static bool dg_ResolveLinks FP ((struct config *, struct ui_command *));
static void dg_SuspendWindow FP ((struct cf_window *wp));
static int  dg_Rename FP ((char *newname));
static void dg_PutConfig FP ((struct config *));
static int  dg_SaveParameter FP ((char *name, char *value, FILE *fp));
static void dg_SendButtonMap FP ((struct cf_window *win));
static void dg_SendDefault FP ((struct cf_window *));
static int dg_ListConfig FP ((char *name, int type, union usy_value *, int));
static int dg_FreeOne FP ((char *name, int type, union usy_value *v, int));
static int dg_UnlinkPD FP ((char *name, int type, union usy_value *v, int));
static int dg_MemberOf FP ((char *name, int type, union usy_value *v, int));
static void dg_DeleteConfig FP ((struct config *cfg));
static struct cf_graphic *dg_NewGraphic FP ((void));
static struct config *dg_TryConfigDir FP ((char *dir, char *name));
static struct config *dg_CopyConfig FP ((struct config *copy, char *name));
static void dg_AssignWindow FP ((Process *proc, struct cf_window *wp));
static void dg_DisplayWidget FP ((struct cf_window *wp));
static int dg_DisplayProcess FP ((struct cf_window *wp));
static int dg_DisplayGraphic FP ((struct config *cfg, 
				  struct cf_window *wp, bool force));
static bool dg_DisplayWindow FP ((struct config *cfg, 
				  struct cf_window *wp, bool force));
static void dg_SyncStates FP ((void));
static Process *dg_FindExisting FP ((ProcessClass *pc, struct config *cfg, 
				     struct cf_window *wp, char *name));
static struct config *dg_LoadedConfig FP ((char *name));
static Widget UWShell FP ((char *name));



void
dg_Init ()
/*
 * Initialize our UI symbol tables and indirect variables
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	strcpy (Cur_config, "(nothing)");
	Configs = usy_c_stbl ("Configurations");
	Current = usy_c_stbl ("CurrentConfig");
	usy_c_indirect (vtable, "configdir", ConfigDir, SYMT_STRING, 
			CFG_FILEPATH_LEN);
	usy_c_indirect (vtable, "configpd", ConfigPD, SYMT_STRING, 
			CFG_FILEPATH_LEN);
	usy_c_indirect (vtable, "configpath", ConfigPath, SYMT_STRING, 
			CFG_SEARCHPATH_LEN);
/*
 * Initialize path variables to empty
 */
	ConfigDir[0] = '\0';
	ConfigPD[0] = '\0';
	ConfigPath[0] = '\0';
/*
 * More indirect variables
 */
	usy_c_indirect (vtable, "sleepafter", &SleepAfter, SYMT_INT, 0);
	usy_c_indirect (vtable, "sleepfor", &SleepFor, SYMT_INT, 0);
	usy_c_indirect (vtable, "tbspace", &TBSpace, SYMT_INT, 0);
}



void
dg_SaveConfig (source, name, update)
char *source;	/* config name to read */
char *name;	/* name to save config as, or NULL to copy source name */
int update;	/* do an update if nonzero */
/*
 * Write the current configuration out to a file.
 */
{
	char fname[200];
	char pdname[200];
	FILE *fp;
	int win, i;
	struct cf_window *wp;
	struct config *cfg;
	ProcessClass *pc;
	int saveas;
/*
 * Look up our configuration.
 */
	cfg = dg_LookupConfig (source);
	saveas = 0;
	if (update && !strcmp(source, Cur_config))
	{
		struct config *current = cfg;
		/*
		 * We are saving from the current config; if we're saving
		 * under a new name, copy the current config into the save
		 * name and update the new config, rather than updating
		 * (and changing) the current config.
		 */
		if (name && strcmp(name, source))
		{
			saveas = 1;
			cfg = dg_CopyConfig (current, name);
		}
		dg_UpdateConfig (current, cfg);
	}
	if (! name)
		name = source;
/*
 * Make a brief attempt to create the configuration directory if it doesn't
 * exist.
 */
	(void) mkdir (ConfigDir, 0777);
/*
 * Create the file and start writing.
 */
	sprintf (fname, "%s/%s%s", ConfigDir, name, SAVED_EXT);
	msg_ELog (EF_DEBUG, "Saving config %s to %s", name, fname);
	if ((fp = fopen (fname, "w")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Unable to create file %s", fname);
		ui_error ("Unable to create %s", fname);
	}
	fprintf (fp, "! Auto-saved display configuration.\n\n");
	if (cfg->c_nlink > 0)
		ui_warning ("Linked PD's not yet handled correctly.");
/*
 * Define the process classes used by the windows.
 */
	for (win = 0; win < cfg->c_nwin; win++)
	{
		wp = cfg->c_wins[win];
		if (! IsWidget (wp))	/* all but widgets have a process */
		{
			if (! (pc = dp_LookupClass (wp->cfw_pcname)))
				continue;
			fprintf (fp, "prototype fallback %s %s",
				 pc->pc_name, pc->pc_exec);
			for (i = 1; i < pc->pc_argc; i++)
				fprintf (fp, " %s", pc->pc_argv[i]);
			fprintf (fp, "\n");
		}
	}
/*
 * Go through and do each window.
 */
	fprintf (fp, "\nconfig %s\n", name);
	for (win = 0; win < cfg->c_nwin; win++)
	{
	/*
	 * Widgets are easy.
	 */
		wp = cfg->c_wins[win];
	 	if (IsWidget (wp))
		{
			fprintf (fp, "\twidget '%s' %d %d %d %d\n",
				wp->cfw_name, wp->cfw_x, wp->cfw_y, 
				wp->cfw_dx, wp->cfw_dy);
			continue;
		}
	/*
	 * Otherwise we have a window with a process and a process class.
	 */
		fprintf (fp, "\t%s '%s' %d %d %d %d ", 
			 (IsGraphic (wp)) ? "graphic" : "process",
			 wp->cfw_name,
			 wp->cfw_x, wp->cfw_y, wp->cfw_dx, wp->cfw_dy);
		fprintf (fp, "'%s'\n", wp->cfw_pcname);
	/*
	 * If this is a graphic window, note plot description and button map
	 * info; otherwise we mark it nongraphic.
	 */
		if (IsGraphic (wp))
		{
			sprintf (pdname, "%s-%s", name, wp->cfw_name);
			fprintf (fp, "\t\tdescription '%s'\n", pdname);
			fprintf (fp, "\t\tbuttonmap '%s'\n",
				 wp->cfw_graphic->g_bmap->db_name);
			if (wp->cfw_graphic->g_forcepd)
				fprintf (fp, "\t\tforcepd\n");
			fprintf (fp, "\tendgraphic\n");
		}
	}
/*
 * Close out the display config definition
 */
	fprintf (fp, "endconfig\n\n");
/*
 * Now dump out each of our plot descriptions into the config file for
 * each non-widget, graphical window
 */
	for (win = 0; win < cfg->c_nwin; win++)
	{
		wp = cfg->c_wins[win];
	 	if (IsGraphic (wp))
		{
			sprintf (pdname, "%s-%s", name, wp->cfw_name);
			dg_SavePD (fp, pdname, wp);
		}
	}
/*
 * Now we can finally close the config file and consider the job done.
 * If we just did the equivalent of a "save as", change to the config we
 * saved as.
 */	
	fclose (fp);
	if (saveas)
		dg_PutConfig (cfg);
}




static void 
dg_UpdateConfig (current, dest)
struct config *current;
struct config *dest;
/*
 * Sync this config structure with the current display.  The config
 * structure may or may not be the current config, but we have to use
 * the current config to find out what's in the display.  The config
 * being updated should at least be a copy of the current config.
 */
{
	Window root, child, realwin;
	int win;
#ifdef notdef
	Window qwin, parent, *children;
	int junk, nchild;;
#endif
	unsigned int bw, depth;

#ifdef notdef
	Widget shell;
	Dimension width, height;
	Position x, y;
	Arg args[5];
	int n;
/*
 * Find the current geometry of each window and widget.  We play some games
 * going back the window tree to find an immediate child of the root window
 * so that we can be reasonably sure of getting an absolute x,y rather than
 * a relative one. 
 */
#endif
	for (win = 0; win < current->c_nwin; win++)
	{
		struct cf_window *wp = current->c_wins[win]; /* source win */
		struct cf_window *dw = dest->c_wins[win]; /* dest win */
	/*
	 * For UI widgets, we want the enclosing shell's window, while for
	 * other windows we want the window itself. 
	 */
	 	if (IsWidget (wp))
		{
#ifdef notdef
			Widget tmp = uw_IWWidget (wp->cfw_name);
			realwin = XtWindow (XtParent (XtParent (tmp)));
#endif
			realwin = XtWindow (UWShell (wp->cfw_name));
		}
		else if (wp->cfw_process->p_win)
			realwin = wp->cfw_process->p_win;
		else
			continue;
#ifdef notdef
	/*
	 * Back up the window tree until we find our ancestor whose parent is
	 * root.  That one will be used below to get our x,y location.
	 */
		qwin = realwin;
		msg_ELog (EF_DEBUG, "Ancestor search for %s", wp->cfw_name);
		while (TRUE)
		{
			XQueryTree (Dm_Display, qwin, &root, &parent, 
				    &children, &nchild);
			XFree (children);
			msg_ELog (EF_DEBUG, "\t%d's root: %d, parent: %d",
				  (int) qwin, (int) root, (int) parent);
			if (parent == root)
				break;
			else
				qwin = parent;
		}
	/*
	 * Get the info.  The positioning info comes from the ancestor
	 * window we found above, while the size comes from the "real"
	 * window. 
	 */
		XGetGeometry (Dm_Display, qwin, &root, &dw->cfw_x,
			      &dw->cfw_y, (unsigned int *) &junk,
			      (unsigned int *) &junk, &bw, &depth);
		dw->cfw_x += bw;
		dw->cfw_y += bw;
#endif
	/*
	 * Get geometry info from the shell window.  Translate our
	 * parent-relative coordinates to root-relative, in case of
	 * reparenting.  Put the coordinates in the dest config window (dw)
	 * instead of the source config window (wp).
	 */
		XGetGeometry (Dm_Display, realwin, &root, &dw->cfw_x,
			      &dw->cfw_y, (unsigned int *) &dw->cfw_dx,
			      (unsigned int *) &dw->cfw_dy, &bw, &depth);
		XTranslateCoordinates (Dm_Display, realwin, root,
				       0, 0, &dw->cfw_x, &dw->cfw_y, &child);
	}
}



#ifdef notdef
static void
dg_GetGeometry (win, x, y, width, height)
struct cf_window *win;
Position *x, *y;
Dimension *width, *height;
/*
 * Request the geometry info from this window.
 */
{
	struct dm_msg msg;
/*
 * Fill in the message structure.
 */
 	msg.dmm_type = DM_GEOMETRY;
/*
 * Ship it out and wait for the response.
 */
	dmsg_SendWindow (win, (char *) &msg, sizeof (struct dm_msg));

	msg_Search (MT_DISPLAYMGR, dg_AwaitGeometry, &msg);
	*x = msg.dmm_x;
	*y = msg.dmm_y;
	*width = msg.dmm_dx;
	*height = msg.dmm_dy;
}


static int
dg_AwaitGeometry (msg, ret)
Message *msg;
struct dm_msg *ret;
{
	struct dm_msg *dmm = (struct dm_msg *) msg->m_data;

	if (dmm->dmm_type == DM_R_GEOMETRY)
	{
		*ret = *dmm;
		return (MSG_DONE);
	}
	/*
	 * There's no telling how re-entrant this code is, so
	 * just leave everything else on the queue for the moment
	 */
	return (MSG_ENQUEUE);
}
#endif /* notdef */



static void
dg_SavePD (fp, pdname, wp)
FILE *fp;
char *pdname;
struct cf_window *wp;
/*
 * Dump out the plot description for this window.
 */
{
	struct cf_graphic *g;
	plot_description pd;
	char **comps;
/*
 * In case we are saving a config which has never been displayed...
 */
	g = wp->cfw_graphic;
	if (! g->g_pd)
	{
		if ((g->g_pd = pda_GetPD (g->g_desc)) != 0)
			g->g_pd = pd_CopyPD (g->g_pd);
		else
		{
			msg_ELog(EF_EMERGENCY, "%s: window %s wants bad PD %s",
				 "savepd", wp->cfw_name, g->g_desc);
			return;
		}
	}
/*
 * Make a copy of the plot description for this window, and change it's name
 * to be what we think it should be.
 */
	pd = pd_CopyPD (g->g_pd);
	pd_Store (pd, "global", "pd-name", pdname, SYMT_STRING);
	fprintf (fp, "beginpd '%s'\n", pdname);
/*
 * For each parameter of each component, print the appropriate dm command.
 */
	for (comps = pd_CompList (pd); *comps; comps++)
	{
		if (!strcmp(*comps, "global"))
			fprintf (fp, "global\n");
		else
			fprintf (fp, "component %s\n", *comps);
		pd_TraverseParameters (pd, *comps, dg_SaveParameter, fp);
	}
/*
 * Clean up and we are done.
 */
	fprintf (fp, "endpd\n\n");
	pd_Release (pd);
}



static int
dg_SaveParameter (name, value, fp)
char *name;
char *value;
FILE *fp;
{
	fprintf (fp, "   parameter %-25s '%s'\n", name, value);
	return (TRUE);
}




void
dg_Display (cmds)
struct ui_command *cmds;
/*
 * Actually put up a given configuration.
 */
{
	static int deflt = 0;
	struct config *cfg;
	char *name = UPTR (*cmds);
/*
 * Make sure we have a default process class before trying to display anything
 */
	if (! deflt)
	{
		deflt = 1;
		dp_DefaultClass ();
	}
/*
 * If this config is already the current config, we do nothing.
 */
	if (! strcmp (name, Cur_config))
		return;
/*
 * Find the configuration.
 */
	cfg = dg_LookupConfig (name);
/*
 * If this window has linked PD's, go through and resolve them now.
 */
	if (cfg->c_nlink > 0)
		if (! dg_ResolveLinks (cfg, cmds + 1))
			return;
/*
 * Put the config up on the screen.
 */
	dg_PutConfig (cfg);
}




static void
dg_PutConfig (cfg)
struct config *cfg;
/*
 * Get this config on the screen.
 */
{
	int win, newcount;
	bool new;
	char cfg_sname[2 * MAXNAME];
/*
 * Remove the existing config symbol table and start a new one.
 */
	usy_z_stbl (Current);
	sprintf (cfg_sname, "cfg_%s", cfg->c_name);
	Current = usy_c_stbl (cfg_sname);
	strcpy (Cur_config, cfg->c_name);
/*
 * Go through and configure every window.
 */
	newcount = 0;
	for (win = 0; win < cfg->c_nwin; win++)
	{
		struct cf_window *wp = cfg->c_wins[win];
	/*
	 * Get this window on the screen.  The sleep is there to avoid
	 * "connection * refused" problems caused by too many processes
	 * trying to hook into the X server at once.  Of course, we don't
	 * need to sleep after the last window is displayed.
	 */
		new = dg_DisplayWindow (cfg, wp, /*force*/ FALSE);
		newcount += new ? 1 : 0;

		if (new && ((newcount % SleepAfter) == 0) && 
		    (win < cfg->c_nwin - 1))
		{
			msg_ELog (EF_DEBUG, "Sleeping for %d seconds (%d/%d)",
				  SleepFor, newcount, SleepAfter);
			sleep (SleepFor);
		}
	}
	dg_SyncStates ();
}



static void
dg_SyncStates ()
/*
 * If there are any processes left marked ACTIVE instead of ASSIGNED, we
 * need to suspend them.  Those marked ASSIGNED become ACTIVE.
 */
{
	Process *proc, **pp;

	pp = dp_ProcessList (NULL);
	while ((proc = *pp++))
	{
		if (proc->p_state == P_ACTIVE)
		{
			dg_SuspendWindow (proc->p_cfw);
			proc->p_state = P_UNMAPPED;
		}
		else if (proc->p_state == P_ASSIGNED)
		{
			proc->p_state = P_ACTIVE;
		}
	}
}



static bool
dg_ResolveLinks (cfg, cmds)
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
		int link;
		struct cf_window *linkwin = cfg->c_wins[i];

		if (! IsGraphic (linkwin))
			continue;
		link = linkwin->cfw_graphic->g_linkpar;
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
	 	if (! (win = dg_CurrentWindow (UPTR (cmds[link - 1]))))
		{
			msg_ELog (EF_PROBLEM, "Link to bad window '%s'",
				  UPTR (cmds[link - 1]));
			return (FALSE);
		}
	/*
	 * Link it.  Since the given win is required to be in the current
	 * display configuration, we know that the PD has to be realized.
	 *
	 * 7/91 jc	Also mark the source window as needing to have
	 *		it's PD pushed out to it next time around.
	 */
	 	linkwin->cfw_graphic->g_pd = win->cfw_graphic->g_pd;
		linkwin->cfw_graphic->g_linksrc = win;
		win->cfw_graphic->g_tmpforce = TRUE;
	}
	return (TRUE);
}




static void
dg_SuspendWindow (wp)
struct cf_window *wp;
/*
 * Suspend this window.
 */
{
	struct dm_msg dmsg;
/*
 * If it's a widget, just pop it down.
 */
	if (IsWidget (wp))
		uw_popdown (wp->cfw_name);
/*
 * Otherwise we have to tell it to go away.
 */
 	else
	{
		dmsg.dmm_type = DM_SUSPEND;
		dmsg_SendWindow (wp, &dmsg, sizeof (struct dm_msg));
	}
}




void
dg_ConfigWindow (win)
struct cf_window *win;
/*
 * Configure this window.
 */
{
	struct dm_reconfig msg;
	struct cf_graphic *g;
/*
 * Fill in the message structure.
 */
 	msg.dmr_type = DM_RECONFIG;
	msg.dmr_x = win->cfw_x;
	msg.dmr_y = win->cfw_y;
	msg.dmr_dx = win->cfw_dx;
	msg.dmr_dy = win->cfw_dy - TBSpace;
	strcpy (msg.dmr_name, win->cfw_name);
/*
 * Ship out the config.  The rest is only for graphic windows.
 */
	dmsg_SendWindow (win, (char *) &msg, sizeof (struct dm_reconfig));
	if (IsGraphic (win))
	{
		g = win->cfw_graphic;
		/*
		 * Ship over the PD, including the defaults.
		 */
		if (g->g_linkpar || g->g_forcepd || g->g_tmpforce)
		{
			dg_SendDefault (win);
			dg_SendPD (win);
		}
		g->g_tmpforce = FALSE;
		/*
		 * And the button maps.
		 */
		dg_SendButtonMap (win);
		/*
		 * If we are shoving times down their throats, 
		 * send the history time too.
		 */
		dt_SendTime (win);
	}
}




static void
dg_SendDefault (win)
struct cf_window *win;
/*
 * Send this graphic window its defaults table.
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
	dmsg_SendWindow (win, dmp, len);
	pd_RPDRelease (rpd);
	free (dmp);
}




void
dg_SendPD (win)
struct cf_window *win;
/*
 * Send this graphic window it's PD.
 */
{
	struct dm_pdchange *dmp;
	raw_plot_description *rpd;
	int len;
/*
 * No sense trying to send something we haven't got
 */
	if (! win->cfw_graphic->g_pd)
		return;
	rpd = pd_Unload (win->cfw_graphic->g_pd);
	len = sizeof (struct dm_pdchange) + rpd->rp_len;
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
	dmsg_SendWindow (win, dmp, len);
	pd_RPDRelease (rpd);
	free (dmp);
}




static void
dg_SendButtonMap (win)
struct cf_window *win;
/*
 * Send the button map to this graphic window.
 */
{
	static struct dm_ebchange *ebc = 0;
	ButtonMap *map = win->cfw_graphic->g_bmap;
/*
 * Make sure everything's cool.
 */
	if (! map)
	{
		msg_ELog (EF_INFO, "Window %s has no button map", 
			  win->cfw_name);
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
	dmsg_SendWindow (win, ebc,
			 sizeof (struct dm_ebchange) +
			 (map->db_nentry - 1)*(sizeof (struct dm_evbind)));
}




static Process *
dg_FindExisting (pc, cfg, wp, name)
ProcessClass *pc;
struct config *cfg;
struct cf_window *wp;
char *name;
/*
 * This is the crux of mapping a new window to display to an existing
 * process.  Either return a pointer to the chosen process, or NULL if
 * no match could be found.
 *
 * The search looks for a match among unassigned processes of the desired
 * class in this order of priority:
 *  1.  A process already attached to the given window.
 *  2.  A process whose name is identical to the name of the given window.
 *	This will only have an effect when running in singleton mode.
 *  3.  A process already attached to a window of the same name.
 *  4.  An active, unassigned process.
 *  5.  An inactive process.
 *
 * Use the config to make sure we don't appropriate a process which has
 * higher priority for another (yet-to-be-assigned) window in the config.
 */
{
	static Process *queue[4] = { 0, 0, 0, 0 }; /* static else cc gripes */
	Process *proc, **pp;
	int i, n;

	/* 1. */
	proc = (wp && wp->cfw_process) ? wp->cfw_process : NULL;
	if (proc && proc->p_state != P_ASSIGNED && proc->p_class == pc)
		return (proc);

	pp = dp_ProcessList (NULL);
	while ((proc = *pp++))
	{
		/* prerequisite for any process */
		if (proc->p_class != pc || proc->p_state == P_ASSIGNED)
			continue;

		/* 2. takes precedence over everything else */
		if (! strcmp (proc->p_name, name))
		{
			queue[0] = proc;
			break;
		}

		/* 3. */
		if (! queue[1] && proc->p_cfw && 
		    ! strcmp(proc->p_cfw->cfw_name, name))
			queue[1] = proc;

		/* 4. */
		if (! queue[2] && proc->p_state == P_ACTIVE)
			queue[2] = proc;

		/* 5. */
		if (! queue[3])
			queue[3] = proc;
	}
	/*
	 * Take the best we could find which does not violate rule #2
	 */
	for (i = 1; (! queue[0]) && (i < 4); ++i)
	{
		proc = queue[i];
		if (! proc)
			continue;
		if (! cfg)	/* only use config info if we have it */
			break;
		/*
		 * Look for a window which would take this process by rule #2.
		 */
		for (n = 0; n < cfg->c_nwin; ++n)
		{
			struct cf_window *win = cfg->c_wins[n];
			if (win != wp && IsGraphic(win) &&
			    ! strcmp (win->cfw_pcname, pc->pc_name) &&
			    (! win->cfw_process || 
			     win->cfw_process->p_state != P_ASSIGNED) &&
			    ! strcmp(win->cfw_name, proc->p_name))
				break;
		}
		/*
		 * If none found, then its ok to use this process
		 */
		if (n >= cfg->c_nwin)
			break;
		else
			proc = NULL;
	}
	return (proc);
}



struct cf_window *
dg_CurrentWindow (name)
char *name;
/*
 * Find the named window in the current configuration and return a pointer
 * to its structure.  Return NULL if not found.
 */
{
	int type;
	union usy_value v;
/*
 * Look it up in our symbol table for the current configuration.
 */
	if (usy_g_symbol (Current, name, &type, &v))
		return ((struct cf_window *) v.us_v_ptr);
	else
		return (NULL);
}




struct cf_window *
dg_AnyWindow (name)
char *name;
/*
 * Look for any process attached to a window by this name,
 * regardless of whether the process is active or inactive.
 * Return a pointer to the window, or NULL if not found.
 */
{
	Process *proc, **pp;

	pp = dp_ProcessList (NULL);
	while ((proc = *pp++))
	{
		if (proc->p_cfw && !strcmp(proc->p_cfw->cfw_name, name))
			break;
	}
	return (proc ? proc->p_cfw : 0);
}




static void
dg_AssignWindow (proc, wp)
Process *proc;
struct cf_window *wp;
/*
 * Assign this process to a window, possibly first detaching it from
 * another window.  Change the state of the process to ASSIGNED.
 */
{
	proc->p_state = P_ASSIGNED;
	/*
	 * See if this process is already attached to this window,
	 * in which case we have nothing to do.
	 */
	if (proc->p_cfw == wp)
		return;
	/*
	 * If the process has a different window, detach the old one.
	 */
	if (proc->p_cfw)
		proc->p_cfw->cfw_process = NULL;
	/*
	 * Now we can just detach the window from its old process, if any,
	 * and attach it to the new process.
	 */
	if (wp->cfw_process)
		wp->cfw_process->p_cfw = NULL;
	wp->cfw_process = proc;
	proc->p_cfw = wp;
}




static int
dg_DisplayGraphic (cfg, wp, force)
struct config *cfg;
struct cf_window *wp;
bool force;
/*
 * Set this graphic window up to be displayed, which means finding it a
 * process and tinkering with its pd.  Return non-zero if we must create a
 * new process. 
 */
{
	int created = FALSE;
	ProcessClass *pc;
	Process *proc;
	struct cf_window *exist;
	struct cf_graphic *g = wp->cfw_graphic;

	pc = dp_LookupClass (wp->cfw_pcname);
	if (! pc)
	{
		msg_ELog (EF_PROBLEM, "unknown class '%s' for window '%s'",
			  wp->cfw_pcname, wp->cfw_name);
		return (FALSE);
	}
	if (! force)
		proc = dg_FindExisting (pc, cfg, wp, wp->cfw_name);
	else
		proc = NULL;

	if (proc)
	{
		msg_ELog (EF_DEBUG, "Existing process %s for win %s", 
			  proc->p_name, wp->cfw_name);
	}
	else
	{
		/*
		 * Otherwise we actually try to fire off the process.
		 */
		msg_ELog (EF_DEBUG, "Need new process, class %s, for win %s",
			  pc->pc_name, wp->cfw_name);
		/*
		 * We may get a process from another class if names clashed.
		 */
		proc = dp_ExecProcess (pc, wp->cfw_name, GroupName, &created);
	}
	if (! proc)
	{
		/*
		 * What to do about processes which could not be exec'ed?
		 */
		msg_ELog (EF_PROBLEM, "Failed to create process for window %s",
			  wp->cfw_name);
		return (FALSE);
	}
	exist = proc->p_cfw;
	dg_AssignWindow (proc, wp);

	/*
	 * Set up the plot description for this window.  Do it now
	 * so that parameter changes that occur before the window
	 * checks in will be honored.  If the window already has a pd,
	 * leave it alone.  If not, check for links (non-forcepd)
	 * to an existing window.  Lastly, lookup up the pd by name and
	 * give the window its own copy.
	 */
	if (g->g_pd)
		/* Already there don't mung it */
		;
	else if (! g->g_linkpar && ! g->g_forcepd && exist &&
		 exist->cfw_graphic)
	{
		/* Share the pd with the existing window! */
		g->g_pd = exist->cfw_graphic->g_pd;
	}
	else if ((g->g_pd = pda_GetPD (g->g_desc)) != 0)
		/* We found it, make our own copy */
		g->g_pd = pd_CopyPD (g->g_pd);
	else
	{
		/* Oops, we couldn't find it. */
		msg_ELog (EF_EMERGENCY, 
			  "graphic window %s wants bad PD %s",
			  wp->cfw_name, g->g_desc);
	}
	g->g_tmpforce = TRUE;
	
	/*
	 * Configure existing processes now, but new processes must first
	 * say hello before we'll send them their configuration.
	 */
	if (! created)
		dg_ConfigWindow (wp);
	return (created);
}



static int
dg_DisplayProcess (wp)
struct cf_window *wp;
/*
 * See if we already know about this process, and ping it if not.
 */
{
	ProcessClass *pc;
	Process *proc;

	proc = dp_LookupProcess (wp->cfw_name);
	if (! proc)
	{
		struct dm_hello dmh;
		/*
		 * Since this is a nongraphic window, we don't exec it.  We
		 * just create a process place-holder, ping the process,
		 * and wait for the hello.  Note that for processes, the
		 * window name is the same as the process name. 
		 */
		pc = dp_LookupClass (wp->cfw_pcname);
		proc = dp_NamedProcess (pc, wp->cfw_name);
		dg_AssignWindow (proc, wp);
		dmh.dmm_type = DM_HELLO;
		dmsg_SendWindow (wp, &dmh, sizeof (dmh));
	}
	else
	{
		/*
		 * If we already knew about this process, then we just
		 * attach it to this window send it its new config.
		 */
		dg_AssignWindow (proc, wp);
		dg_ConfigWindow (wp);
	}
	return (0);
}



static void
dg_DisplayWidget (wp)
struct cf_window *wp;
{
#ifdef notdef
	Widget shell;
	Arg args[5];
	int n;
#endif
	/*
	 * We want the popup shell to have static gravity, hopefully
	 * preventing the window manager from moving it around all
	 * over the place.  Rather than forcing override, request
	 * transient status.
	 */
	uw_ForceOverride (wp->cfw_name);
#ifdef notdef
	shell = UWShell (wp->cfw_name);
	n = 0;
	XtSetArg (args[n], XtNwinGravity, StaticGravity); n++;
	XtSetArg (args[n], XtNtransient, True); n++;
	XtSetValues (shell, args, n);
#endif
	uw_SetGeometry (wp->cfw_name, wp->cfw_x, wp->cfw_y,
			wp->cfw_dx, wp->cfw_dy);
	uw_popup (wp->cfw_name);
}




static bool
dg_DisplayWindow (cfg, wp, force)
struct config *cfg;
struct cf_window *wp;
bool force;
/*
 * Get this window onto the screen.  Existing processes need to be
 * sent their new config, while new processes will get their config
 * once they have said hello.  Return TRUE iff we executed a new process.
 */
{
	bool created = FALSE;
	SValue v;
/*
 * Deal with window classes separately.  Forcing a new process is only
 * applicable to graphic windows.
 */
	msg_ELog (EF_DEBUG, "displaying window '%s' [class '%s']",
		  wp->cfw_name, WinClassName (wp));
	if (IsWidget (wp))
		/* IsWidget */
		dg_DisplayWidget (wp);
	else if (IsGraphic (wp))
		/* IsGraphic:
		 * Pass the config as well to allow some more intelligent
		 * mapping of processes to windows.
		 */
		created = dg_DisplayGraphic (cfg, wp, force);
	else
		/* IsProcess */
		created = dg_DisplayProcess (wp);
/*
 * Add it to the current config table
 */
	v.us_v_ptr = (char *) wp;
	usy_s_symbol (Current, wp->cfw_name, SYMT_POINTER, &v);
	return (created);
}




static int
dg_Rename (newname)
char *newname;
/*
 * Generate a unique window name from the name in newname.  Return non-zero
 * if we actually come up with a different name, zero if name is already
 * unique.
 */
{
	int i;
	int count;
	char *c;
/*
 * See if there is already a number at the end of this name
 */
	c = newname + strlen(newname) - 1;
	while (isdigit((int)*c) && (c >= newname))
		c--;
	c++;
	count = (*c) ? atoi(c) : 0;
/*
 * While we find a window, increment the count at the end.
 */
	i = 0;
	while (dg_CurrentWindow (newname) && ++i)
		sprintf (c, "%d", ++count);
	return (i);
}



void
dg_PutNewWindow (pcname, cmds)
char *pcname;
struct ui_command *cmds;
/*
 * Add a window to the current display configuration.  If rename is not
 * specified, the window name must be unique for this configuration, else
 * we fail.  If rename is specified, we'll generate a unique name here.
 *
 * If reuse keyword present, use the existing (suspended) window as is
 * rather than resetting it. 
 */
{
	struct config *cfg = dg_LookupConfig (Cur_config);
	struct cf_window *newwin;
	struct cf_window *exist;
	struct cf_graphic *g;
	bool rename = FALSE, reuse = FALSE;
	int type;
	SValue v;
	char *name;
	char newname[256];
	struct ui_command *cmd;
/*
 * Check for keywords which affect how we build the window.
 */
	for (cmd = cmds; cmd->uc_ctype != UTT_END; cmd++)
	{
		if (cmd->uc_ctype != UTT_KW)
			continue;
	 	else if (UKEY (*cmd) == DMC_RENAME)
			rename = TRUE;
	 	else if (UKEY (*cmd) == DMC_REUSE)
			reuse = TRUE;
	}
/*
 * Unless we've been specifically asked to generate a unique name, 
 * make sure there isn't already a window by the name they want.
 */
	name = (char *) UPTR (*cmds);
	if (!rename && dg_CurrentWindow (name))
	{
		msg_ELog (EF_PROBLEM, "Duplicate window %s", name);
		ui_error ("Window %s already exists", name);
		return;		/* never reached */
	}
/*
 * So we either have a unique window name, or it's time to come up with one.
 */
	strcpy (newname, name);
	if (rename && dg_Rename (newname))
	{
		msg_ELog (EF_INFO, "new window renamed to '%s'", newname);
	}
/*
 * If we're re-using, look for the given name which we're supposed to re-use.
 */
	exist = dg_AnyWindow (name);

	if (reuse && !exist)
	{
		reuse = FALSE;
		msg_ELog (EF_INFO, "window %s not found for re-use", name);
	}
/*
 * If we're supposed to re-use a window as is, copy it.  Otherwise, we
 * initialize it.
 */
	if (reuse && exist)
	{
		newwin = dg_NewWindow (cfg, exist->cfw_class, newname);
		g = newwin->cfw_graphic;
		memcpy (newwin, exist, sizeof(struct cf_window));
	/*
	 * Take back the graphic pointer of our original window,
	 * and copy the source window's graphic into it.
	 */
		if (g)
		{
			newwin->cfw_graphic = g;
			*g = *(exist->cfw_graphic);
			/*
			 * Leave the new pd as a copy of the source window's
			 * pd until we know whether forcepd will be set
			 */
		}
	/*
	 * Set the new name, in case its been renamed.  Offset the
	 * geometry if we're appearing over a current window.
	 */
		strcpy (newwin->cfw_name, newname);
		if (dg_CurrentWindow (name))
		{
			newwin->cfw_x += 20;
			newwin->cfw_y += 20;
		}
	}
	else
	{
		newwin = dg_NewWindow (cfg, W_Graphic, newname);
		g = newwin->cfw_graphic;
		strcpy (newwin->cfw_name, newname);
		strcpy (newwin->cfw_pcname, 
			(pcname) ? pcname : DEFAULT_PROCESS);
		newwin->cfw_x = newwin->cfw_y = 200;	/* pick something... */
		newwin->cfw_dx = newwin->cfw_dy = 400;
	}
/*
 * Now pass through the rest of the parameters.
 */
	for (++cmds; cmds->uc_ctype != UTT_END; cmds++)
	{
	/*
	 * Just a string parameter initially means a plot description.
	 */
		if (cmds->uc_ctype == UTT_VALUE && g)
		{
			strcpy (g->g_desc, UPTR (*cmds));
		}
	/*
	 * Or they could be giving a button map.
	 */
	 	else if (UKEY (*cmds) == DMC_BUTTONMAP && g)
		{
			if (! usy_g_symbol (Bmaps, UPTR (cmds[1]), &type, &v))
				ui_error ("Bad button map %s", UPTR (cmds[1]));
			g->g_bmap = (ButtonMap *) v.us_v_ptr;
			cmds++;
		}
	/*
	 * Maybe this is a nongraphic window.
	 */
	 	else if (UKEY (*cmds) == DMC_NONGRAPHIC && IsGraphic(newwin))
		{
			newwin->cfw_class = W_NonGraphic;
			free (newwin->cfw_graphic);
			newwin->cfw_graphic = g = NULL;
			break;	/* Nothing else matters */
		}
	/*
	 * Maybe they want forced pd's.
	 */
	 	else if (UKEY (*cmds) == DMC_FORCEPD && g)
		{
			g->g_forcepd = TRUE;
			g->g_pd = 0;
		}
	/*
	 * The rest we've handled already
	 */
	 	else if (UKEY(*cmds) != DMC_REUSE && UKEY(*cmds) != DMC_RENAME)
			ui_warning ("Weird kw %d", UKEY (*cmds));
	}
	dg_SyncWindow (cfg, newwin, /*force*/ FALSE);
}



void
dg_SyncWindow (cfg, newwin, force)
struct config *cfg;
struct cf_window *newwin;
bool force;	/* force the exec of a new process */
/*
 * Realize this window in the current config.  Mark processes in use in the
 * current config as ASSIGNED, then display the window, and then sync the
 * process states to whomever was or is newly active.
 */
{
	int i;
	struct cf_window *wp;

	for (i = 0; i < cfg->c_nwin; ++i)
	{
		wp = cfg->c_wins[i];

		if ((wp == newwin) && (wp->cfw_process))
		{
			wp->cfw_process->p_state = P_UNMAPPED;
		}
		else if (wp->cfw_process)
		{
			wp->cfw_process->p_state = P_ASSIGNED;
		}
	}
	dg_DisplayWindow (cfg, newwin, force);
	dg_SyncStates ();
}



static struct config *
dg_CopyConfig (copy, name)
struct config *copy;
char *name;
{
	struct config *new;
	int i;
/*
 * If a config by this name already exists, delete it.  The new one will
 * take precedence in the Configs symbol table.
 */
	if ((new = dg_LoadedConfig (name)))
	{
		msg_ELog (EF_INFO, "Overwriting existing config %s", name);
		dg_DeleteConfig (new);
	}
/*
 * Make the copy.
 */
	new = dg_NewConfig (name);
	new->c_nlink = copy->c_nlink;
	for (i = 0; i < copy->c_nwin; i++)
	{
		struct cf_window *wp, *win;

		win = copy->c_wins[i];
		wp = dg_NewWindow (new, win->cfw_class, win->cfw_name);
		strcpy (wp->cfw_pcname, win->cfw_pcname);
		wp->cfw_x = win->cfw_x;
		wp->cfw_y = win->cfw_y;
		wp->cfw_dx = win->cfw_dx;
		wp->cfw_dy = win->cfw_dy;
		if (IsGraphic(win) && win->cfw_graphic->g_pd)
		{
			*wp->cfw_graphic = *win->cfw_graphic;
			wp->cfw_graphic->g_pd = 
				pd_CopyPD (win->cfw_graphic->g_pd);
		}
	}
/*
 * Store and return the new config.
 */
	dg_TableAdd (new);
	return (new);
}



void
dg_PutConfigAs (name, template)
char *name, *template;
/*
 * Create a new display configuration on the fly.
 */
{
	struct config *copy, *new;
/*
 * Find the copy config.
 */
	if (! (copy = dg_LookupConfig (template)))
		ui_error ("Template config '%s' missing", template);
/*
 * Copy the source into a new config.
 */
	new = dg_CopyConfig (copy, name);
/*
 * Put the new one on the screen.
 */
	dg_PutConfig (new);
}




void
dg_List (name)
char *name;
/*
 * List out the known configs, or a single config if name is non-NULL.
 */
{
	struct config *cfg;
	SValue v;
	int type;

	if (name)
	{
		if (! (cfg = dg_LookupConfig (name)))
		{
			ui_error ("could not find config '%s'", name);
		}
		else
		{
			if (usy_g_symbol (Configs, name, &type, &v))
				(void) dg_ListConfig (name, type, &v, 0);
		}
	}
	else
	{
		usy_traverse (Configs, dg_ListConfig, 0, FALSE);
	}
}



static int
dg_ListConfig (name, type, v, junk)
char *name;
int type;
union usy_value *v;
int junk;
/*
 * List out a single configuration.
 */
{
	int i;
	struct config *cfg = (struct config *) v->us_v_ptr;

	ui_nf_printf ("Config '%s':\n", name);
	for (i = 0; i < cfg->c_nwin; i++)
	{
		struct cf_window *win = cfg->c_wins[i];
		struct cf_graphic *g = win->cfw_graphic;

		ui_nf_printf ("   %-10s '%-15s': at (%3d, %3d) size %dx%d",
			      IsGraphic (win) ? "Graphic" : 
			      (IsNonGraphic(win) ? "Nongraphic" : "Widget"),
			      win->cfw_name, win->cfw_x, win->cfw_y, 
			      win->cfw_dx, win->cfw_dy);
		if (IsWidget (win))
			ui_nf_printf ("\n");
		else if (IsGraphic (win))
		{
			ui_nf_printf ("; class %s\n", win->cfw_pcname);
			ui_nf_printf ("\tPD: %s\n", g->g_desc);
		}
		if (win->cfw_process)
			ui_nf_printf ("\twin id: %li  proc name: %s\n",
				      win->cfw_process->p_win,
				      win->cfw_process->p_name);
	}
	ui_printf ("\n");
	return (TRUE);
}



static struct config *
dg_LoadedConfig (name)
char *name;
/*
 * See if a configuration by this name has already been loaded.
 */
{
	int type;
	SValue v;
/*
 * Look up the config in the configs table.
 */
 	if (Configs && usy_g_symbol (Configs, name, &type, &v))
		return ((struct config *) v.us_v_ptr);
	else
		return (NULL);
}



struct config *
dg_CurrentConfig ()
{
	return (dg_LoadedConfig (Cur_config));
}



struct config *
dg_LookupConfig (name)
char *name;
/*
 * Try to find a configuration by this name.
 */
{
	int type, ndir, i;
	SValue v;
	char path[CFG_SEARCHPATH_LEN], *dirs[32];
	struct config *ret;
/*
 * Look up this config in the configs table.
 */
	if ((ret = dg_LoadedConfig (name)))
		return (ret);
/*
 * OK, not there.  Let's try ConfigDir first.
 */
	if ((ret = dg_TryConfigDir (ConfigDir, name)))
		return (ret);
/*
 * Still no go.  Now we plow through ConfigPath and see if we have any
 * more luck there.
 */
 	if (usy_g_symbol (usy_g_stbl ("ui$variable_table"), "configpath",
			&type, &v))
	{
		strcpy (path, v.us_v_ptr);
		ndir = CommaParse (path, dirs);
		for (i = 0; i < ndir; i++)
			if ((ret = dg_TryConfigDir (dirs[i], name)))
				return (ret);
	}
	ui_error ("Unknown configuration: %s", name);
	return (NULL);
}




static struct config *
dg_TryConfigDir (dir, name)
char *dir, *name;
/*
 * See if we can't pull this configuration in out of the current directory.
 */
{
	char fname[200];
	struct config *cfg;
/*
 * Make up a "read" command and try to run it.
 */
	sprintf (fname, "read %s/%s%s", dir, name, SAVED_EXT);
	if (access (fname + 5, F_OK) == 0)
	{
		ui_perform (fname);
		if ((cfg = dg_LookupConfig (name)))
		{
			msg_ELog(EF_DEBUG, "Found config %s in %s",name,fname);
			return (cfg);
		}
	}
/*
 * Failing that, try again without the extension.
 */
	sprintf (fname, "read %s/%s", dir, name);
	if (access (fname + 5, F_OK) == 0)
	{
		ui_perform (fname);
		if ((cfg = dg_LookupConfig (name)))
		{
			msg_ELog(EF_DEBUG, "Found config %s in %s",name,fname);
			return (cfg);
		}
	}
/*
 * No go.
 */
	return (NULL);
}




struct config *
dg_NewConfig (name)
char *name;
/*
 * Alloc a new config, initialize it, and return the pointer
 */
{
	struct config *cfg = NEW (struct config);

	strcpy (cfg->c_name, name);
	cfg->c_nwin = 0;
	cfg->c_nlink = 0;
	cfg->c_wins[0] = NULL;
	return (cfg);
}
		


void
dg_TableAdd (cfg)
struct config *cfg;
/*
 * If a config by this name already exists, the old one is removed
 * and the new one added in its place.
 */
{
	struct config *exists;
	SValue v;

	exists = dg_LoadedConfig (cfg->c_name);
	if (exists)
	{
		msg_ELog (EF_INFO, "config %s being replaced", cfg->c_name);
		dg_DeleteConfig (exists);
	}
	v.us_v_ptr = (char *) cfg;
	usy_s_symbol (Configs, cfg->c_name, SYMT_POINTER, &v);
	/*
	 * If we just replaced the current config, well then we need to
	 * put the new version up since the old one no longer exists
	 */
	if (! strcmp (cfg->c_name, Cur_config))
		dg_PutConfig (cfg);
}



static struct cf_graphic *
dg_NewGraphic ()
/*
 * Alloc a graphic struct and initialize it
 */
{
	struct cf_graphic *graph;

	graph = NEW (struct cf_graphic);
	graph->g_bmap = Default_map;
	graph->g_forcepd = FALSE;
	graph->g_pd = 0;
	graph->g_linkpar = 0;
	graph->g_linksrc = NULL;
	graph->g_tmpforce = 0;
	strcpy (graph->g_desc, "(undefined)");
	return (graph);
}



struct cf_window *
dg_NewWindow (cfg, wc, name)
struct config *cfg;
WinClass wc;
char *name;
/*
 * Create space for a window in a config, initialize the window,
 * and return a pointer to it.  Windows can only be created inside
 * a config.
 */
{
	struct cf_window *win;

	win = NEW (struct cf_window);
	cfg->c_wins[cfg->c_nwin++] = win;
	cfg->c_wins[cfg->c_nwin] = NULL;
	strcpy (win->cfw_name, name);
	win->cfw_class = wc;
	strcpy (win->cfw_pcname, DEFAULT_PROCESS);
	win->cfw_x = win->cfw_y = 0;
	win->cfw_dx = win->cfw_dy = 0;
	win->cfw_ncroak = 0;
	win->cfw_force_exec = 0;
	win->cfw_graphic = NULL;
	win->cfw_process = NULL;
	/*
	 * If we are a graphic window, we need some adjunct info
	 */
	if (IsGraphic (win))
		win->cfw_graphic = dg_NewGraphic ();
	return (win);
}



void
dg_FreeConfigs ()
/*
 * Remove all of our configs and their associated windows.
 */
{
	usy_traverse (Configs, dg_FreeOne, 0, FALSE);
	usy_z_stbl (Configs);
	Configs = 0;
	Cur_config[0] = 0;
	usy_z_stbl (Current);
}



static int
dg_FreeOne (name, type, v, param)
char *name;
int type;
union usy_value *v;
int param;
{
	struct config *cfg = (struct config *) v->us_v_ptr;

	dg_DeleteConfig (cfg);
	return (TRUE);
}



static void
dg_DeleteConfig (cfg)
struct config *cfg;
/*
 * Delete all record of this configuration.
 */
{
	/*
	 * While we have at least one window, delete the first one.
	 * We do this because DeleteWindow changes the cfg structure.
	 */
	while (cfg->c_nwin)
		dg_DeleteWindow (cfg, cfg->c_wins[0]);
	/*
	 * Now take care of the config structure itself.
	 */
	usy_z_symbol (Configs, cfg->c_name);
	free (cfg);
}




void
dg_DeleteWindow (cfg, win)
struct config *cfg;
struct cf_window *win;
/*
 * Delete this window from this configuration.  If the window has a
 * process, detach it.  If any other windows have links to this window,
 * break the link. Release the pd only if no other windows share pointers
 * to it.  Next, remove the window from the configuration by sliding all of
 * the window pointers above down one slot.  Finally, free the graphic and
 * the window itself. 
 */
{
	int i;

	if (win->cfw_process)
		win->cfw_process->p_cfw = NULL;
	/*
	 * Remove the window from the config 
	 */
	for (i = 0; i < cfg->c_nwin; ++i)
		if (cfg->c_wins[i] == win)
			break;
	--cfg->c_nwin;
	for ( ; i < cfg->c_nwin; ++i)
		cfg->c_wins[i] = cfg->c_wins[i + 1];
	cfg->c_wins[cfg->c_nwin] = NULL;
	/*
	 * We don't do anything with the graphic part if we're not graphic.
	 */
	if (IsGraphic (win))
		dg_ReleasePD (win);
	/*
	 * Free the graphic part
	 */
	if (win->cfw_graphic)
		free (win->cfw_graphic);
	/*
	 * Free the memory
	 */
	free (win);
}



void
dg_ReleasePD (win)
struct cf_window *win;
/*
 * If this window owns the only reference to this pd, release the
 * pd.  Otherwise the pd is "unlinked" by zero'ing the window's
 * reference to it.
 */
{
	struct cf_graphic *g = win->cfw_graphic;

	usy_traverse (Configs, dg_UnlinkPD, (int)win, FALSE);
	if (g->g_pd)
		pd_Release (g->g_pd);
}



static int
dg_UnlinkPD (name, type, v, param)
char *name;
int type;
union usy_value *v;
int param;
/*
 * In the config by this name, check all of the windows for 
 * pd's shared with the window being deleted.
 */
{
	struct cf_window *src = (struct cf_window *) param;
	struct config *cfg = (struct config *) v->us_v_ptr;
	int i;

	for (i = 0; i < cfg->c_nwin; ++i)
	{
		struct cf_graphic *g;

		if (cfg->c_wins[i] == src)	/* ignore the source window */
			continue;
		if (! (g = cfg->c_wins[i]->cfw_graphic))
			continue;
		if (g->g_linksrc == src)
		{
			/*
			 * Indicate that the source window no longer exists
			 */
			g->g_linksrc = NULL;
			g->g_linkpar = 0;
		}
		if (g->g_pd == src->cfw_graphic->g_pd)
		{
			/*
			 * Someone is using this pd, so release our
			 * reference to it.  It could have been a linked
			 * pd, or one passed via a non-forced pd.
			 */
			src->cfw_graphic->g_pd = 0;
		}
	}
	return (TRUE);
}
	



typedef struct _cfg_pair {
	struct cf_window *cp_win;
	struct config *cp_cfg;
} cfg_pair;


struct config *
dg_FindOwner (win)
struct cf_window *win;
{
	cfg_pair cp;

	cp.cp_win = win;
	cp.cp_cfg = NULL;
	if (Configs)
		usy_traverse (Configs, dg_MemberOf, (int) &cp, FALSE);
	return (cp.cp_cfg);
}


static int
dg_MemberOf (name, type, v, param)
char *name;
int type;
union usy_value *v;
int param;
{
	cfg_pair *cp = (cfg_pair *) param;
	struct config *cfg = (struct config *) v->us_v_ptr;
	int i;

	for (i = 0; i < cfg->c_nwin; ++i)
	{
		if (cfg->c_wins[i] == cp->cp_win)
		{
			cp->cp_cfg = cfg;
			return (FALSE);
		}
	}
	return (TRUE);
}



int
dg_Query (who)
char *who;
/*
 * Respond to the query request with info about all known processes,
 * their state, their attached windows, and info about the windows.
 */
{
	char buf[256];
	int i;
	struct cf_window *wp;
	ProcessClass **ppc;
	ProcessClass *pc;
	Process **pp;
	Process *proc;

	ppc = dp_ClassList (0);
	msg_AnswerQuery (who, "// Classes");
	while ((pc = *ppc++))
	{
		sprintf (buf, "%-20s %2i members (%s:",
			 pc->pc_name, pc->pc_nmembers, pc->pc_exec);
		for (i = 0; i < pc->pc_argc; ++i)
		{
			strcat (buf, " ");
			strcat (buf, pc->pc_argv[i]);
		}
		strcat (buf, ") ");
		strcat (buf, (pc->pc_xargs) ? "[- dm args] " : "[+ dm args] ");
		msg_AnswerQuery (who, buf);
	}

	msg_AnswerQuery (who, "// Processes");
	pp = dp_ProcessList (0);
	while ((proc = *pp++))
	{
		char *state;

		state = ((proc->p_state == P_ACTIVE) ? "active" :
			 (proc->p_state == P_UNMAPPED) ? "unmapped" :
			 (proc->p_state == P_ICONIFIED) ? "icon" :
			 (proc->p_state == P_ASSIGNED) ? "assigned" :
			 "unknown");
		sprintf (buf, 
			 "%-20s [%-8s] pid:%6li win:%#10lx  // class: %s", 
			 proc->p_name, state, (long)proc->p_pid, proc->p_win,
			 proc->p_class->pc_name);
		if ((wp = proc->p_cfw))
		{
			struct cf_graphic *g = wp->cfw_graphic;

			sprintf (buf+strlen(buf),
				 "\n%20s [%s] %s %ix%i+%i+%i ncroak: %hi",
				 " ", wp->cfw_name, WinClassName(wp), 
				 wp->cfw_dx, wp->cfw_dy, wp->cfw_x, wp->cfw_y, 
				 wp->cfw_ncroak);
			if (g)
			{
				sprintf (buf+strlen(buf),
					 "\n%25s pd: %s link: %s(%d) %s %s",
					 " ", g->g_desc, (g->g_linksrc) ? 
					 g->g_linksrc->cfw_name : "none",
					 g->g_linkpar,
					 (g->g_forcepd) ? "forcepd" : "",
					 (g->g_tmpforce) ? "tmpforce" : "");
			}
		}
		msg_AnswerQuery (who, buf);
	}
	msg_FinishQuery (who);
	return (0);
}




static Widget
UWShell (name)
char *name;
/*
 * Return the top-level shell widget for this UI widget
 */
{
	Widget shell = uw_IWWidget (name);

	/*
	 * Duh.  Need to use wmShellWidgetClass instead of shellWidgetClass
	 * since menubars use simplemenu, which is a subclass of the override
	 * shell class.
	 */
	while (shell && !XtIsSubclass (shell, wmShellWidgetClass))
		shell = XtParent(shell);
	return (shell);
}


