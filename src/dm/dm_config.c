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
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>

# include <ui.h>
# include <ui_error.h>
# include "dm.h"
# include "dm_vars.h"
# include "dm_cmds.h"

MAKE_RCSID ("$Id: dm_config.c,v 1.18 1994-11-20 19:11:22 granger Exp $")


/*
 * Our local stuff.
 */
static void UpdateConfig FP ((struct config *));
static void GetGeometry FP ((struct cf_window *win, Position *x, Position *y,
			     Dimension *width, Dimension *height));
static int  AwaitGeometry FP((Message *msg, struct dm_msg *ret));
static void SavePD FP ((FILE *, char *, struct cf_window *));
static bool ResolveLinks FP ((struct config *, struct ui_command *));
static bool DisplayWindow FP ((struct cf_window *));
static void SetupExec FP ((struct cf_window *));
static void PutConfig FP ((struct config *));
static void RunProgram FP ((char *, char **));
static int  SaveParameter FP ((char *name, char *value, FILE *fp));
static void send_default FP ((struct cf_window *));



void
SaveConfig (source, name, update)
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
/*
 * Look up our configuration.
 */
	cfg = LookupConfig (source);
	if (update && !strcmp(source, Cur_config))
		UpdateConfig (cfg);
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
	fprintf (fp, "! Auto-saved display configuration.\nconfig %s\n", name);
	if (cfg->c_nlink > 0)
		ui_warning ("Linked PD's not yet handled correctly.");
/*
 * Go through and do each window.
 */
	for (win = 0; win < cfg->c_nwin; win++)
	{
	/*
	 * Widgets are easy.
	 */
		wp = cfg->c_wins + win;
	 	if (wp->cfw_flags & CF_WIDGET)
		{
			fprintf (fp, "\twidget '%s' %d %d %d %d\n",
				wp->cfw_name, wp->cfw_x, wp->cfw_y, 
				wp->cfw_dx, wp->cfw_dy);
			continue;
		}
	/*
	 * Otherwise we have a real window.
	 */
		fprintf (fp, "\twindow '%s' %d %d %d %d %s", wp->cfw_name,
			wp->cfw_x, wp->cfw_y, wp->cfw_dx, wp->cfw_dy,
			wp->cfw_prog);
		for (i = 1; wp->cfw_args[i]; i++)
			fprintf (fp, " %s", wp->cfw_args[i]);
		fprintf (fp, "\n");
	/*
	 * If this is a graphic window, note plot description and button map
	 * info; otherwise we mark it nongraphic.
	 */
		if (wp->cfw_nongraph)
			fprintf (fp, "\t\tnongraphic\n");
		else
		{
			sprintf (pdname, "%s-%s", name, wp->cfw_name);
			fprintf (fp, "\t\tdescription '%s'\n", pdname);
			fprintf (fp, "\t\tbuttonmap '%s'\n",
				wp->cfw_bmap->db_name);
		}
		if (wp->cfw_forcepd)
			fprintf (fp, "\t\tforcepd\n");
		fprintf (fp, "\tendwindow\n");
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
		wp = cfg->c_wins + win;
	 	if ((wp->cfw_flags & CF_WIDGET) || wp->cfw_nongraph)
			continue;
		sprintf (pdname, "%s-%s", name, wp->cfw_name);
		SavePD (fp, pdname, wp);
	}


/*
 * Now we can finally close the config file and consider the job done.
 */	
	fclose (fp);
}




static void 
UpdateConfig (cfg)
struct config *cfg;
/*
 * Make sure no changes have happened to the current config while we
 * weren't looking.
 */
{
	Window root, realwin, qwin, parent, *children;
	int win, junk;
	unsigned int bw, depth, nchild;
#ifdef notdef
	Widget shell;
	Dimension width, height;
	Position x, y;
	Arg args[5];
	int n;
#endif
/*
 * Find the current geometry of each window and widget.  We play some games
 * going back the window tree to find an immediate child of the root window
 * so that we can be reasonably sure of getting an absolute x,y rather than
 * a relative one. 
 */
	for (win = 0; win < cfg->c_nwin; win++)
	{
		struct cf_window *wp = cfg->c_wins + win;
	/*
	 * For UI widgets, we want the enclosing shell's window, while for
	 * other windows we want the window itself. 
	 */
	 	if (wp->cfw_flags & CF_WIDGET)
		{
			Widget tmp = uw_IWWidget (wp->cfw_name);
			realwin = XtWindow (XtParent (XtParent (tmp)));
		}
		else if (wp->cfw_win)
			realwin = wp->cfw_win;
		else
			continue;
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
		XGetGeometry (Dm_Display, qwin, &root, &wp->cfw_x,
			      &wp->cfw_y, (unsigned int *) &junk,
			      (unsigned int *) &junk, &bw, &depth);
		wp->cfw_x += bw;
		wp->cfw_y += bw;

		XGetGeometry (Dm_Display, realwin, &root, &junk,
			      &junk, (unsigned int *) &wp->cfw_dx,
			      (unsigned int *) &wp->cfw_dy, &bw, &depth);
#ifdef notdef
	/*
	 * Record our latest geometry info
	 */
		wp->cfw_x = x;
		wp->cfw_y = y;
		wp->cfw_dx = width;
		wp->cfw_dy = height;

		if (wp->cfw_flags & CF_WIDGET)
		{
		/*
		 * Sync UI geometry with what's actually on the screen
		 */
			uw_SetGeometry (wp->cfw_name, wp->cfw_x, wp->cfw_y,
					wp->cfw_dx, wp->cfw_dy);
		}
#endif
	}
}




static void
GetGeometry (win, x, y, width, height)
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
	msg_send (win->cfw_name, MT_DISPLAYMGR, FALSE, (char *) &msg,
		sizeof (struct dm_msg));

	msg_Search (MT_DISPLAYMGR, AwaitGeometry, &msg);
	*x = msg.dmm_x;
	*y = msg.dmm_y;
	*width = msg.dmm_dx;
	*height = msg.dmm_dy;
}



static int
AwaitGeometry (msg, ret)
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



static void
SavePD (fp, pdname, wp)
FILE *fp;
char *pdname;
struct cf_window *wp;
/*
 * Dump out the plot description for this window.
 */
{
	plot_description pd;
	char **comps;
/*
 * In case we are saving a config which has never been displayed...
 */
	if (! wp->cfw_pd)
	{
		if (wp->cfw_pd = pda_GetPD (wp->cfw_desc))
			wp->cfw_pd = pd_CopyPD (wp->cfw_pd);
		else
		{
			msg_ELog(EF_EMERGENCY, "%s: window %s wants bad PD %s",
				 "savepd", wp->cfw_name, wp->cfw_desc);
			return;
		}
	}
/*
 * Make a copy of the plot description for this window, and change it's name
 * to be what we think it should be.
 */
	pd = pd_CopyPD (wp->cfw_pd);
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
		pd_TraverseParameters (pd, *comps, SaveParameter, fp);
	}
/*
 * Clean up and we are done.
 */
	fprintf (fp, "endpd\n\n");
	pd_Release (pd);
}



static int
SaveParameter (name, value, fp)
char *name;
char *value;
FILE *fp;
{
	fprintf (fp, "   parameter %-25s '%s'\n", name, value);
}




void
display (cmds)
struct ui_command *cmds;
/*
 * Actually put up a given configuration.
 */
{
	struct config *cfg;
	char *name = UPTR (*cmds);
/*
 * If this config is already the current config, we do nothing.
 */
	if (! strcmp (name, Cur_config))
		return;
/*
 * Find the configuration.
 */
	cfg = LookupConfig (name);
/*
 * If this window has linked PD's, go through and resolve them now.
 */
	if (cfg->c_nlink > 0)
		if (! ResolveLinks (cfg, cmds + 1))
			return;
/*
 * Put the config up on the screen.
 */
	PutConfig (cfg);
}




static void
PutConfig (cfg)
struct config *cfg;
/*
 * Get this config on the screen.
 */
{
	int win, newcount, disp_suspend ();
	stbl old_table;
	bool new;
	char cfg_sname[MAXNAME];
/*
 * Get a new symbol table for this display config.
 */
	old_table = Current;
	sprintf (cfg_sname, "cfg_%s", cfg->c_name);
	Current = usy_c_stbl (cfg_sname);
	strcpy (Cur_config, cfg->c_name);
/*
 * Go through and configure every window.
 */
	newcount = 0;
	for (win = 0; win < cfg->c_nwin; win++)
	{
		struct cf_window *wp = cfg->c_wins + win;
	/*
	 * Get this window on the screen.  The sleep is there to avoid
	 * "connection * refused" problems caused by too many processes
	 * trying to hook into the X server at once.  Of course, we don't
	 * need to sleep after the last window is displayed.
	 */
		new = DisplayWindow (wp);
		newcount += new ? 1 : 0;

		if (new && ((newcount % SleepAfter) == 0) && 
		    (win < cfg->c_nwin - 1))
		{
			msg_ELog (EF_DEBUG, "Sleeping for %d seconds (%d/%d)",
				  SleepFor, newcount, SleepAfter);
			sleep (SleepFor);
		}
	/*
	 * Zap it from the old config table.
	 */
		usy_z_symbol (old_table, wp->cfw_name);
	}
/*
 * If there are any entries left in the current table, they represent
 * windows which must be suspended.  Do that, then get rid of the old table.
 */
	usy_traverse (old_table, disp_suspend, 0, FALSE);
	usy_z_stbl (old_table);
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
	 *
	 * 7/91 jc	Also mark the source window as needing to have
	 *		it's PD pushed out to it next time around.
	 */
	 	linkwin->cfw_pd = win->cfw_pd;
		linkwin->cfw_linksrc = win;
		win->cfw_tmpforce = TRUE;
	}
	return (TRUE);
}



/*ARGSUSED*/
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
		return;
	}
/*
 * Otherwise actually fire off the process.
 */
	if (fork () == 0)
		RunProgram (win->cfw_prog, win->cfw_args);
/*
 * The parent process (that's us) can now go ahead and set up the plot 
 * description for this window.  Do it now so that parameter changes that
 * occur before the window checks in will be honored.
 */
	if (win->cfw_pd)
		;	/* Already there don't mung it */
	else if (win->cfw_pd = pda_GetPD (win->cfw_desc))
		win->cfw_pd = pd_CopyPD (win->cfw_pd);
	else
	{
		msg_ELog (EF_EMERGENCY, "%s: window %s wants bad PD %s",
			  "create_win", win->cfw_name, win->cfw_desc);
		return;
	}
	win->cfw_tmpforce = TRUE;
}





static void
RunProgram (program, args)
char *program, **args;
/*
 * Exec this program with the given arguments.
 */
{
	char abs_prog[128];
	char *strchr ();
/*
 * Close some relevant FD's first.
 */
	/* close (0); close (1); close (2); */
	close (msg_get_fd ());
/*
 * Try to locate and run the program.
 */
	if (FindFile (program, ExecPath, abs_prog))
		execv (abs_prog, args);
/*
 * No luck.  Just gripe.  Unfortunately, we can not use msg_ELog here because
 * we have disconnected from the message system above.  Someday when I am
 * less lazy I will put in some code to reconnect and log the grip properly.
 */
	printf ("Unable to exec '%s'\n", program);
	perror (program);
	exit (1);
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
	msg.dmm_dy = win->cfw_dy - TBSpace;
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
 *
 * (4/93 jc) NOTE:	The following code should really never be executed
 *			any more, since the plot description gets put into
 *			the window structure when the process is forked.
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
 * Then ship over the PD too, including the defaults.
 */
	if (win->cfw_linkpar || win->cfw_forcepd|| created ||win->cfw_tmpforce)
	{
		send_default (win);
		send_pd (win);
	}

	win->cfw_tmpforce = FALSE;
/*
 * And the button maps.
 */
	SendButtonMap (win);
/*
 * If we are shoving times down their throats, send the history time too.
 */
	if (ForceHistory && HistoryMode)
		SetTimeMode (win->cfw_name, TRUE, &HistoryTime);
}




static void
send_default (win)
struct cf_window *win;
/*
 * Send this window its defaults table.
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

	pd_RPDRelease (rpd);
	free (dmp);
}



static bool
DisplayWindow (wp)
struct cf_window *wp;
/*
 * Get this window onto the screen.  Return TRUE iff we created a new window.
 */
{
	SValue v;
	Arg args[10];
	int i;
	struct cf_window *exist;
	bool created = FALSE;
/*
 * If this is a widget window, deal with it separately.
 */
	if (wp->cfw_flags & CF_WIDGET)
	{
		Widget shell;
		uw_ForceOverride (wp->cfw_name);
#ifdef notdef
	/*
	 * First pop it up, and then move it, so that olwm gets its act
	 * together.  Make sure we set the shell location by setting
	 * it explicitly ourselves.
	 */
		uw_popup (wp->cfw_name);
#endif
		uw_SetGeometry (wp->cfw_name, wp->cfw_x, wp->cfw_y,
			wp->cfw_dx, wp->cfw_dy);
#ifdef notdef
		shell = UWShell (wp->cfw_name);
		i = 0;
		XtSetArg (args[i], XtNx, wp->cfw_x);	++i;
		XtSetArg (args[i], XtNy, wp->cfw_y);	++i;
		XtSetValues (shell, args, i);
#endif
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
		created = TRUE;
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
 * Add it to the current config table
 */
	v.us_v_ptr = (char *) wp;
	usy_s_symbol (Current, wp->cfw_name, SYMT_POINTER, &v);
	return (created);
}





void
NewWindow (cmds)
struct ui_command *cmds;
/*
 * Add a window to this display configuration.  If rename not specified,
 * the window name must be unique for this configuration, else we fail.
 * If rename is specified, we'll generate a unique name here.
 *
 * If reuse keyword present, use the existing (suspended) window as is
 * rather than resetting it.
 */
{
	struct config *cfg = LookupConfig (Cur_config);
	struct cf_window *newwin;
	struct cf_window *exist;
	bool rename = FALSE, reuse = FALSE;
	int i, type;
	SValue v;
	char *name;
	char newname[256];
	struct ui_command *cmd;
/*
 * Check our arguments for optional keywords
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
	if (!rename && lookup_win (name, TRUE /*current only*/))
	{
		msg_ELog (EF_PROBLEM, "Duplicate window %s", name);
		ui_error ("Window %s already exists", name);
	}
/*
 * So we either have a unique window name, or it's time come up with one.
 */
	strcpy (newname, name);
	i = 0;
	while (rename && lookup_win (newname, TRUE))
		sprintf (newname, "%s%d", name, ++i);
/*
 * See if a window with the new name exists elsewhere.
 */
	exist = lookup_win (newname, FALSE);
/*
 * Where we'll put our new window in the current config
 */
	newwin = cfg->c_wins + cfg->c_nwin++;
/*
 * If we're supposed to re-use a suspended window as is, copy it.
 * Otherwise, we initialize it.  The plot description is always reset;
 * to re-use an existing window's current pd (as opposed to its defined pd),
 * forcepd should be false.
 */
	if (reuse && exist)
	{
		memcpy (newwin, exist, sizeof(struct cf_window));
	}
	else
	{
		strcpy (newwin->cfw_name, newname);
		SetupExec (newwin);
		newwin->cfw_win = 0;
		newwin->cfw_x = newwin->cfw_y = 200;	/* pick something... */
		newwin->cfw_dx = newwin->cfw_dy = 400;
		strcpy (newwin->cfw_desc, "template");
		newwin->cfw_bmap = Default_map;
		newwin->cfw_flags = newwin->cfw_ncroak = 0;
		newwin->cfw_forcepd = 0;
		newwin->cfw_tmpforce = newwin->cfw_nongraph = 0;
	}
	newwin->cfw_pd = 0;

	if (reuse)
	{
		msg_ELog (EF_INFO, "window %s not found for re-use", newname);
	}
/*
 * Now pass through the rest of the parameters.
 */
	for (++cmds; cmds->uc_ctype != UTT_END; cmds++)
	{
	/*
	 * Just a string parameter initially means a plot description.
	 * Ignore it if we're re-using an existing window.
	 */
		if (cmds->uc_ctype == UTT_VALUE)
		{
			if (!reuse || !exist)
				strcpy (newwin->cfw_desc, UPTR (*cmds));
		}
	/*
	 * Or they could be giving a button map.
	 */
	 	else if (UKEY (*cmds) == DMC_BUTTONMAP)
		{
			if (! usy_g_symbol (Bmaps, UPTR (cmds[1]), &type, &v))
				ui_error ("Bad button map %s", UPTR (cmds[1]));
			newwin->cfw_bmap = (ButtonMap *) v.us_v_ptr;
			cmds++;
		}
	/*
	 * Maybe this is a nongraphic window.
	 */
	 	else if (UKEY (*cmds) == DMC_NONGRAPHIC)
		{
			newwin->cfw_nongraph = TRUE;
			break;	/* Nothing else matters */
		}
	/*
	 * Maybe they want forced pd's.
	 */
	 	else if (UKEY (*cmds) == DMC_FORCEPD)
			newwin->cfw_forcepd = TRUE;
	/*
	 * The rest we've handled already
	 */
	 	else if (UKEY(*cmds) != DMC_REUSE && UKEY(*cmds) != DMC_RENAME)
			ui_warning ("Weird kw %d", UKEY (*cmds));
	}
/*
 * Now realize this window.
 */
	DisplayWindow (newwin);
}





static void
SetupExec (win)
struct cf_window *win;
/*
 * Set up default parameters.
 */
{
	SValue v;
	int type, narg;
	stbl vtable = usy_g_stbl ("ui$variable_table");
/*
 * Get the default executable.
 */
	if (! usy_g_symbol (vtable, "default_exec", &type, &v))
		ui_error ("No default executable");
	strcpy (win->cfw_prog, v.us_v_ptr);
	win->cfw_args[0] = win->cfw_name;
/*
 * Look for args.
 */
	for (narg = 1; ; narg++)
	{
		char sname[30];
		sprintf (sname, "default_arg%d", narg);
		if (! usy_g_symbol (vtable, sname, &type, &v))
			break;
		win->cfw_args[narg] = usy_string (v.us_v_ptr);
	}
	win->cfw_args[narg] = 0;
}




void
NewConfig (name, template)
char *name, *template;
/*
 * Create a new display configuration on the fly.
 */
{
	struct config *new, *copy;
	SValue v;
	int win;
/*
 * Find the copy config.
 */
	if (! (copy = LookupConfig (template)))
		ui_error ("Template config '%s' missing", template);
# ifdef notdef
/*
 * See if this config exists; if so, we need to zap it.  We don't 
 * get rid of the dynamic strings though -- there may be other references.
 * Thus, they may drop on the ground.
 */
	if (new = LookupConfig (name))
	{
		warning ("Overwriting existing config %s", name);
		free (new);
	}
# endif
/*
 * Make the copy.
 */
	new = ALLOC (struct config);
	*new = *copy;
	for (win = 0; win < new->c_nwin; win++)
	{
		struct cf_window *wp = new->c_wins + win;
		if (wp->cfw_pd && ! wp->cfw_nongraph)
			wp->cfw_pd = pd_CopyPD (wp->cfw_pd);
	}
/*
 * Store the new config and put it on the screen.
 */
	strcpy (new->c_name, name);
	v.us_v_ptr = (char *) new;
	usy_s_symbol (Configs, name, SYMT_POINTER, &v);
	PutConfig (new);
}
