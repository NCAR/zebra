/*
 * Ingest scheduler
 */
static char    *rcsid = "$Id: is.c,v 1.6 1991-11-12 23:51:19 martin Exp $";

/*
 * Copyright (C) 1987,88,89,90,91 by UCAR University Corporation for
 * Atmospheric Research All rights reserved
 * 
 * No part of this work covered by the copyrights herein may be reproduced or
 * used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular
 * purpose. UCAR does not indemnify any infringement of copyright, patent, or
 * trademark through use or modification of this software.  UCAR does not
 * provide maintenance or updates for its software.
 */

#include <stdio.h>
#include <sys/wait.h>
#include <signal.h>
#include <varargs.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <ui.h>
#include <ui_error.h>
#include <dirent.h>
#include <X11/Intrinsic.h>

#include "is_vars.h"
#include "is_cmds.h"

#ifdef __STDC__
int             is_shutdown(void);
#else
is_shutdown();
#endif

/*
 * Definitions of globals.
 */
stbl            Configs;	/* will hold all configurations, accesable by
				 * config name */
int             exclose = 1;	/* true if stdout and stderr should be closed
				 * after fork */

main(argc, argv)
	int             argc;
	char          **argv;
{
	int             is_initial(), is_msg_handler(), msg_incoming();
	int             is_list();
	void            sigchldHandler();

	char            loadfile[100];
	stbl            vtable;
	Widget          top;

	/*
	 * Hook into the message handler.
	 */

	msg_connect(is_msg_handler, "is");
	/* is this necessary? */
	msg_DeathHandler(is_shutdown);

	msg_ELog(EF_INFO, "%s", rcsid);
	msg_ELog(EF_INFO, "is started ");
	/*
	 * Get the interface set up.
	 */

	fixdir_t("ISLOADFILE", ".", "is.lf", loadfile, ".lf");
	ui_init(loadfile, TRUE, FALSE);
	ui_setup("is", &argc, argv, (char *) 0);

	/*
	 * Create our symbol tables.
	 */
	Configs = usy_c_stbl("Configurations");

	/*
	 * Indirect variables.
	 */
	/*
	 * vtable = usy_g_stbl("ui$variable_table");
	 */
	tty_watch(msg_get_fd(), (void (*) ()) msg_incoming);

	/*
	 * force into window mode, since mode window followed by popup
	 * doesn't seem to work from a command file
	 */
	uw_ForceWindowMode((char *) 0, &top, (XtAppContext *) 0);

	/*
	 * If a file appears on the command line, open it.
	 */
	if (argc > 1)
		ut_open_file(argv[1], TRUE);

	/*
	 * catch the SIGCHLD signals so that we can keep track of spawned
	 * process status
	 */

	signal(SIGCHLD, sigchldHandler);

	/*
	 * Interpret commands.
	 */

	ui_get_command("is-initial", "IS>", is_initial, 0);
	is_shutdown();
}





is_initial(arg, cmds)
	int             arg;
	struct ui_command *cmds;
/*
 * Deal with a ingest scheduler command.
 */
{
	switch (UKEY(*cmds)) {
	case ISC_CONFIG:
		is_config(cmds + 1);
		break;

	case ISC_LIST:
		is_list(cmds + 1);
		break;

	case ISC_START:
		is_start(cmds + 1);
		break;

	case ISC_STOP:
		is_stop(cmds + 1);
		break;

	case ISC_SHUTDOWN:
		is_shutdown();
		break;

	case ISC_EXCLOSE:
		is_exclose(cmds + 1);
		break;

	default:
		ui_error("(BUG): Unknown keyword: %d\n", UKEY(*cmds));
	}
	return (TRUE);
}




is_msg_handler(msg)
	struct message *msg;
/*
 * Deal with incoming messages.
 */
{
	switch (msg->m_proto) {
		/*
		 * Stuff from the message handler itself.
		 */
	case MT_MESSAGE:
		mh_message(msg);
		break;
		/*
		 * We use timer events at times.
		 */
	case MT_TIMER:
		tl_DispatchEvent((struct tm_time *) msg->m_data);
		break;

	default:
		msg_ELog(EF_PROBLEM, "Funky message type %d in IS",
			 msg->m_proto);
	};
}



mh_message(msg)
	struct message *msg;
/*
 * Deal with a MESSAGE protocol msg.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
	struct mh_client *client;

	switch (tm->mh_type) {
	case MH_SHUTDOWN:
		ui_printf("Message handler shutdown -- I quit!\n");
		is_shutdown();

	default:
		ui_printf("Unknown MESSAGE proto msg %d\n", tm->mh_type);
		break;
	}
}

int
is_shutdown()
/*
 * Shutdown the ingest scheduler.
 */

{
	int             stop();
	/*
	 * Now finish up here and quit.
	 */

	/*
	 * call the stop routine for all configurations
	 */
	usy_traverse(Configs, stop, TRUE, FALSE);
	msg_ELog(EF_INFO, "is terminated");
	ui_finish();
	exit(0);
}


int
is_start(cmds)
	struct ui_command *cmds;
{

	/*
	 * parse the start command
	 */

	union usy_value v;

	int             start_cfg();

	int             type;

	switch (cmds[0].uc_ctype) {

	case UTT_VALUE:
		/*
		 * must be a configuration name, so look it up and do the
		 * buisness
		 */
		if (usy_g_symbol(Configs, UPTR(cmds[0]), &type, &v))
			start_cfg(UPTR(cmds[0]), type,
				  &v, FALSE);
		else
			ui_error("Unknown configuration %s\n", UPTR(cmds[0]));
		break;

	case UTT_KW:
		/*
		 * Keyword all has been specified, so start them all
		 */
		usy_traverse(Configs, start_cfg, TRUE, FALSE);
		break;

	default:
		ui_error("(BUG): Unknown keyword: %d\n", UKEY(cmds[0]));
	}

}

int
start_cfg(name, type, v, all)
	char           *name;
	int             type;
	union usy_value *v;
	int             all;

{
	/*
	 * merely start the timer so that this cfg will run in a tick. File
	 * type configurations will have a non-zero interval, and so will
	 * repeat. Continuous types will have a zero interval, and so will
	 * not be repeated.
	 */

	int             i;
	int             pid;
	struct is_config *cfg = (struct is_config *) v->us_v_ptr;

	extern int      errno;
	struct proc_entry *this_proc = NEW(struct proc_entry);
	void            timed_check();

	/*
	 * make sure that configuration is not already running
	 */

	if (cfg->timer || (((cfg->type == IS_CTYPE)
			    || (cfg->type == IS_PTYPE)) && cfg->active)) {
		ui_printf
			("%s is already in operation; must stop it first\n", cfg->name);
		return;
	}
	cfg->timer_slot = tl_AddRelativeEvent
		(timed_check, cfg, INCFRAC,
		 cfg->interval * INCFRAC);
	cfg->timer = TRUE;
	cfg->n_restarts = 0;
}

int
is_stop(cmds)
	struct ui_command *cmds;
{

	union usy_value v;

	int             stop();

	int             type;

	/*
	 * parse the stop command
	 */

	switch (cmds[0].uc_ctype) {

	case UTT_VALUE:
		/*
		 * must be a configuration name, so look it up and do the
		 * buisness
		 */
		if (usy_g_symbol(Configs, UPTR(cmds[0]), &type, &v))
			stop(UPTR(cmds[0]), type,
			     &v, FALSE);
		else
			ui_error("Unknown configuration %s\n", UPTR(cmds[0]));
		break;

	case UTT_KW:
		/*
		 * Keyword all has been specified, so stop them all
		 */
		usy_traverse(Configs, stop, TRUE, FALSE);
		break;

	default:
		ui_error("(BUG): Unknown keyword: %d\n", UKEY(cmds[0]));
	}

}

int
stop(name, type, v, all)
	char           *name;
	int             type;
	union usy_value *v;
	int             all;

{
	/*
	 * called when a stop command is issued for a configuration. Do the
	 * following:
	 * 
	 * 1. send an HUP to all active processes running under this
	 * configuration.
	 * 
	 * 2. cancel the timer associated with this configuration
	 * 
	 */
	struct is_config *cfg = (struct is_config *) v->us_v_ptr;
	struct proc_entry *this_proc = cfg->proc;

	for (this_proc = cfg->proc; this_proc; this_proc = this_proc->next) {
		kill(this_proc->pid, SIGHUP);
		clean_up_process_list(cfg, this_proc);
	}
	/* turn off the timer if present */
	if (cfg->timer) {
		tl_Cancel(cfg->timer_slot);
		cfg->timer = FALSE;
	}
	cfg->rollover = FALSE;
	cfg->ingest_file[0] = 0;
	cfg->timer_slot = -1;
}

is_config(cmds)
	struct ui_command *cmds;
/*
 * parse out the configuration specifications
 */
{
	/*
	 * parse the config command only
	 */

	int             do_config();
	union usy_value v;
	int             i;

	/* allocate a new configuration */
	struct is_config *cfg = NEW(struct is_config);


	/* initialize configuration */

	init_cfg(cmds, cfg, UPTR(cmds[0]));

	/* now look for the specifics of the configuration */
	ERRORCATCH
		ui_subcommand("is-config", "Config>", do_config, (long) cfg);
	ON_ERROR
		relvm(cfg);
	RESIGNAL;
	ENDCATCH

	/*
	 * now verify that the configuration is complete
	 */
		v.us_v_ptr = (char *) cfg;
	switch (cfg->type) {
	case IS_FTYPE:
		/*
		 * file type ingestor
		 */
		if
			(cfg->directory &&
			 cfg->filename &&
			 cfg->process &&
			 cfg->interval)
			/*
			 * Enter configuration in symbol table, using name as
			 * symbol
			 */
			usy_s_symbol(Configs, UPTR(*cmds), SYMT_POINTER, &v);
		else
			ui_error(
				 "Configuration %s is incomplete and will be ignored\n",
				 UPTR(cmds[0]));
		break;
	case IS_CTYPE:
		/*
		 * continuous type ingestor
		 */
		if (cfg->process)
			/*
			 * Enter configuration in symbol table, using name as
			 * symbol
			 */
			usy_s_symbol(Configs, UPTR(*cmds), SYMT_POINTER, &v);
		else
			ui_error(
				 "Configuration %s is incomplete and will be ignored\n",
				 UPTR(cmds[0]));

		break;
	case IS_PTYPE:
		/*
		 * periodic type ingestor
		 */
		if (cfg->process && cfg->interval)
			/*
			 * Enter configuration in symbol table, using name as
			 * symbol
			 */
			usy_s_symbol(Configs, UPTR(*cmds), SYMT_POINTER, &v);
		else
			ui_error(
				 "Configuration %s is incomplete and will be ignored\n",
				 UPTR(cmds[0]));

		break;
	}

}

int
do_config(cfg, cmds)
	struct is_config *cfg;
	struct ui_command *cmds;

{
	/*
	 * parse out the commands found between config ... endconfig
	 */

	int             i;

	switch (UKEY(*cmds)) {
	case ISC_PLAT:
		cfg->platform = usy_string(UPTR(cmds[1]));
		break;

	case ISC_DIR:
		cfg->directory = usy_string(UPTR(cmds[1]));
		break;

	case ISC_MOVEDIR:
		cfg->movedir = usy_string(UPTR(cmds[1]));
		break;

	case ISC_DELETE:
		cfg->delete = TRUE;
		break;

	case ISC_RESTART:
		cfg->restart = TRUE;
		break;

	case ISC_FILENAME:
		cfg->filename = usy_string(UPTR(cmds[1]));
		break;

	case ISC_PROCESS:
		cfg->process = usy_string(UPTR(cmds[1]));
		cmds += 1;
		for (i = 0; cmds[i].uc_ctype != UTT_END; i++)
			if (i < MAX_PROC_ARGS) {
				cfg->n_proc_args++;
				cfg->proc_args[i] = usy_string(UPTR(cmds[i]));
			} else {
				ui_error(
					 "Max %d proc args allowed, rest are ignored\n",
					 MAX_PROC_ARGS);
				break;
			}
		break;

	case ISC_INTERVAL:
		cfg->interval = UINT(cmds[1]);
		break;













	case ISC_ENDCON:
		return (FALSE);

	default:
		ui_error("(BUG): Unknown keyword: %d\n", UKEY(*cmds));
	}
	return (TRUE);
}

is_exclose(cmds)
	struct ui_command *cmds;
{

	switch (UKEY(*cmds)) {
	case ISC_TRUE:
		exclose = 1;
		break;

	case ISC_FALSE:
		exclose = 0;
		break;

	default:
		ui_error("(BUG): Unknown keyword: %d\n", UKEY(*cmds));
	}

}

is_list(cmds)
	struct ui_command *cmds;
{

	/*
	 * List out the known configs.
	 */
	int             list_cfg();

	union usy_value v;

	int             type;

	/*
	 * parse the list command
	 */

	switch (cmds[0].uc_ctype) {

	case UTT_VALUE:
		/*
		 * must be a configuration name, so look it up and do the
		 * buisness
		 */
		if (usy_g_symbol(Configs, UPTR(cmds[0]), &type, &v))
			list_cfg(UPTR(cmds[0]), type,
				 &v, FALSE);
		else
			ui_error("Unknown configuration %s\n", UPTR(cmds[0]));
		break;

	case UTT_KW:
		/*
		 * Keyword all has been specified, so list them all
		 */
		usy_traverse(Configs, list_cfg, TRUE, TRUE);
		break;

	default:
		ui_error("(BUG): Unknown keyword: %d\n", UKEY(cmds[0]));
	}

}

int
list_cfg(name, type, v, junk)
	char           *name;
	int             type, junk;
	union usy_value *v;
/*
 * List out a single configuration.
 */
{
	int             i;
	struct is_config *cfg = (struct is_config *) v->us_v_ptr;

	ui_nf_printf("Config '%s':\n", name);
	ui_nf_printf("\ttimer:\t\t%c\n", cfg->timer ? 'T' : 'F');
	ui_nf_printf("\tactive:\t\t%d\n", cfg->active);
	ui_nf_printf("\trestart:\t%c\n", cfg->restart ? 'T' : 'F');
	switch (cfg->type) {
	case IS_FTYPE:
		ui_nf_printf("\ttype:\t\t%s\n", "file");
		break;
	case IS_CTYPE:
		ui_nf_printf("\ttype:\t\t%s\n", "continuous");
		break;
	case IS_PTYPE:
		ui_nf_printf("\ttype:\t\t%s\n", "continuous");
		break;
	}
	ui_nf_printf("\tplatform:\t%s\n", cfg->platform);
	ui_nf_printf("\tdirectory:\t%s\n", cfg->directory);
	ui_nf_printf("\tmovedir:\t%s\n", cfg->movedir);
	ui_nf_printf("\tdelete:\t\t%c\n", cfg->delete ? 'T' : 'F');
	ui_nf_printf("\tfilename:\t%s\n", cfg->filename);
	ui_nf_printf("\tprocess:\t%s", cfg->process);
	for (i = 1; i < cfg->n_proc_args; i++)
		ui_nf_printf(" %s", cfg->proc_args[i]);
	ui_nf_printf("\n");
	ui_nf_printf("\tinterval:\t%d\n", cfg->interval);
	ui_nf_printf("\trollover:\t%c\n", cfg->rollover ? 'T' : 'F');
	ui_nf_printf("\ttimer slot:\t%d\n", cfg->timer_slot);
	ui_nf_printf("\tingest file:\t%s\n", cfg->ingest_file);
	ui_nf_printf("\tn_restarts:\t%d\n", cfg->n_restarts);

	return (TRUE);
}

void
cfg_go(cfg)
	struct is_config *cfg;
{
	int             i;
	int             pid;

	static char    *new_args[MAX_PROC_ARGS];
	extern int      errno;
	struct proc_entry *this_proc = NEW(struct proc_entry);

	/*
	 * this is the most important function in the whole mess. It is
	 * called when a particular configuration is to be activated. It
	 * makes parameter substitutions, and then forks and exec's the
	 * ingestor.
	 */

	/*
	 * check to see if $p or $f show up in the arguments, and if so, make
	 * the appropriate swap
	 */

	for (i = 0; i < MAX_PROC_ARGS; i++)
		new_args[i] = NULL;

	for (i = 0; i < cfg->n_proc_args; i++)
		if (!strcmp("$p", cfg->proc_args[i]))
			new_args[i] = cfg->platform;
		else if (!strcmp("$f", cfg->proc_args[i]))
			new_args[i] = cfg->ingest_file;
		else
			new_args[i] = cfg->proc_args[i];


	if ((pid = fork()) == 0) {
		close(0);
		if (exclose) {
			close(1);
			close(2);
		}
		close(msg_get_fd());
		/*
		 * sleep for a second. Otherwise, the exec'd job can run and
		 * exit before we have had time to record the details in cfg
		 */
		sleep(1);
		execv(cfg->process, new_args);
		printf("Unable to exec '%s'\n", cfg->process);
		perror(cfg->process);
		exit(1);
	} else {
		/*
		 * record the details in cfg
		 */
		this_proc->pid = pid;
		this_proc->next = cfg->proc;
		cfg->proc = this_proc;
		cfg->active++;
		if (cfg->type == IS_FTYPE || cfg->type == IS_PTYPE)
			cfg->timer = TRUE;
		else
			cfg->timer = FALSE;
		msg_ELog(EF_INFO,
			 "c=%s, p=%s, p=%d, f=%s",
			 cfg->name, cfg->process, pid, cfg->ingest_file);
	}
}

clean_up_process_list(cfg, proc)
	struct is_config *cfg;
	struct proc_entry *proc;
{
	struct proc_entry *this_proc;
	struct proc_entry *last_proc;

	/* delink an entry in a configuration's process list */

	if (proc == cfg->proc)
		cfg->proc = cfg->proc->next;
	else
		for (this_proc = cfg->proc;
		     this_proc;
		     this_proc = this_proc->next) {
			if (this_proc == proc) {
				/* got a match */
				last_proc->next = this_proc->next;
				free(proc);
			}
			last_proc = this_proc;
		}
	cfg->active--;
}

static int     *exit_status;
void
sigchldHandler()
{

	int             pid;
	int             process_term();

	/*
	 * this handler is called when an ingestor terminates. Get the pid
	 * and traverse the Configs table to find the matching entry.
	 * process_term() is called to identify the pid and then deal with
	 * the termination of this process.
	 * 
	 * NOTE: global exit_status is used to transmit the process status to
	 * process_term(). One could clean this up by having usy_traverse()
	 * pass a structure containing the pid and the exit status.
	 */

#ifdef DEBUG
	ui_printf("\rgot a sigchld\n");
#endif

	while ((pid =
		wait3(&exit_status, WNOHANG, (struct rusage *) 0)) > 0) {
		usy_traverse(Configs, process_term,
			     pid, FALSE);
	}
}

int
process_term(name, type, v, pid)
	char           *name;
	int             type, pid;
	union usy_value *v;

/**

This function is called by usy_traverse(Configs,...)

See if the pid matches the Configs entry. If it
matches, do the following:

1. clean up the process list entry.

for IS_FTYPE only:

2. if a file move has been specified, do so.

3. if a file delete has been specified, then do so

4. if another file is available for ingest, restart the cfg,
   and set the rollover flag to indicate this type processing

for IS_PTYPE only:

2. Do nothing. The job will be resheduled by the timer.

for IS_CTYPE only:

2. if restart is specified, and is < MAX_RESTARTS, restart it.

3. if restart >= MAX_RESTARTS, turn off restart

**/

{
	int             i;
	struct is_config *cfg = (struct is_config *) v->us_v_ptr;
	struct proc_entry *this_proc;
	char           *move_file;
	int             find_file();

	for (this_proc = cfg->proc; this_proc; this_proc = this_proc->next) {

		if (this_proc->pid == pid) {

			clean_up_process_list(cfg, this_proc);

			if (WIFEXITED(exit_status)) {

				/* process exited using exit() */
				msg_ELog(EF_INFO,
				 "%s p=%s, p=%d, has exited with status %d",
					 name, cfg->process, pid,
					 WEXITSTATUS(exit_status));
			} else {/* processes terminated due to a signal */
				msg_ELog(EF_INFO,
					 "%s (proc=%s, pid=%d) has terminated due to signal %d",
					 name, cfg->process, pid,
					 WTERMSIG(exit_status));
			}
			switch (cfg->type) {


			case IS_FTYPE:
				/*
				 * For file type ingestors, and if a move
				 * directory has been specified, go ahead and
				 * move the file . If only delete has been
				 * specified, merely delete it. Move will be
				 * honored over delete.
				 */
				if (cfg->movedir) {
					move_file = malloc(strlen(cfg->movedir)
						     + strlen(cfg->basename)
							   + 2);
					strcpy(move_file, cfg->movedir);

					if (move_file[strlen(move_file) - 1] != '/')
						strcat(move_file, "/");
					strcat(move_file, cfg->basename);

					if (rename(cfg->ingest_file, move_file))
						msg_ELog(EF_PROBLEM, "Error %d Can't move %s\nto %s\n",
							 errno, cfg->ingest_file, move_file);

				} else if (cfg->delete) {
					if (unlink(cfg->ingest_file))
						msg_ELog(EF_PROBLEM, "Error %d Can't unlink %s",
						   errno, cfg->ingest_file);

				}
				/*
				 * look for next file and start ingest if it
				 * is there
				 */
				if (find_file(cfg)) {
					/*
					 * indicate that we are processing
					 * files in the rollover mode, so
					 * that timer doesn't start them as
					 * well
					 */
					cfg->rollover = TRUE;
					cfg_go(cfg);
				} else
					cfg->rollover = FALSE;
				break;

			case IS_PTYPE:
				break;

			case IS_CTYPE:
				/*
				 * for continuous type ingestors, reshedule
				 * them under certain conditions
				 */
				if (cfg->restart)
					if (cfg->n_restarts++ < MAX_RESTARTS)
						cfg_go(cfg);
					else {
						msg_ELog(EF_PROBLEM,
							 "%s has exceeded restart limit of %d",
						   cfg->name, MAX_RESTARTS);
					}
				break;

			default:
				break;
			}	/** switch (cfg->type) **/

		}		/** if (this_proc->pid == pid) **/
	}			/** for (this_proc = cfg->proc;...) **/


	return (TRUE);
}

void
timed_check(t, cfg)
	time           *t;
	struct is_config *cfg;
{
	/*
	 * each configuration has a timer associated with it, which calls
	 * this routine. Fire off another ingestor if conditions warrent.
	 */

	int             find_file();
#ifdef DEBUG
	ui_printf("\rgot timer event for %s\n", cfg->name);
#endif

	/*
	 * For the IS_FTYPE, there may be multiple files ready to be ingested
	 * at once. In this case, after the sigchld is received, the cfg is
	 * activated with the next file (see sigchldhandler(). If these
	 * multiple ingests are in progress, the rollover flag is set.
	 * Regular timer scheduled ingests will not be activated under these
	 * conditions. We only want to have one ingestor running at a time
	 * for a configuration.
	 * 
	 * remember that the timer is used to run continuous type ingestors just
	 * once. They are not scheduled periodically
	 */
	switch (cfg->type) {
	case IS_CTYPE:
	case IS_PTYPE:
		cfg_go(cfg);
		break;

	case IS_FTYPE:
		if (!cfg->rollover)
			if (find_file(cfg)) {
				cfg_go(cfg);

			}
		break;
	}
}


int
find_file(cfg)
	struct is_config *cfg;
{
	DIR            *d;
	struct dirent  *e;
	struct stat     buff;
	struct stat    *buf = &buff;
	char           *ct;
	char            full_name[MAX_FILE_NAME];
	time_t          file_t;
	int             first = 1;
	int             ret = FALSE;

	/*
	 * find an elibible file for this configuration, and place
	 * information in the configurtion structure. return 1 if successful,
	 * 0 otherwise.
	 */

	cfg->ingest_file[0] = 0;

	if (!(d = opendir(cfg->directory))) {
		msg_ELog("can't open directory %s\n", cfg->directory);
		return (FALSE);
	}
	while (e = readdir(d)) {

		if (!strncmp(cfg->filename,
			     e->d_name,
			     strlen(cfg->filename))) {

			/*
			 * file matched!
			 */

			full_name[0] = 0;
			strcat(full_name, cfg->directory);
			strcat(full_name, "/");
			strcat(full_name, e->d_name);

			/*
			 * get the stats on this file
			 */

			if (!stat(full_name, buf)) {

				/*
				 * find the oldest file that matches
				 */
				if (first) {
					file_t = buf->st_mtime;
					strcpy(cfg->ingest_file, full_name);
					strcpy(cfg->basename, e->d_name);
					first = 0;
				} else {
					if (buf->st_mtime < file_t) {
						file_t = buf->st_mtime;
						strcpy(cfg->ingest_file, full_name);
						strcpy(cfg->basename, e->d_name);
					}
				}
				ret = TRUE;
			} else {
				/*
				 * if the stat fails, we have a problem for a
				 * file that is supposed to match
				 */
				msg_ELog
					("problem with stat(%s), errno %d",
					 full_name, errno);
			}
		}
	}

	closedir(d);

	return (ret);
}

init_cfg(cmds, cfg, name)
	struct ui_command *cmds;
	struct is_config *cfg;
	char           *name;
{

	/*
	 * initialize a configuration structure
	 */

	int             i;

	cfg->name = usy_string(name);
	switch (UKEY(cmds[1])) {
	case ISC_FTYPE:
		cfg->type = IS_FTYPE;
		break;
	case ISC_CTYPE:
		cfg->type = IS_CTYPE;
		break;
	case ISC_PTYPE:
		cfg->type = IS_PTYPE;
		break;
	}
	cfg->directory = NULL;
	cfg->platform = NULL;
	cfg->movedir = NULL;
	cfg->filename = NULL;
	cfg->process = NULL;
	cfg->interval = 0;
	cfg->n_proc_args = 0;
	for (i = 0; i < MAX_PROC_ARGS;
	     i++)
		cfg->proc_args[i] = NULL;
	cfg->active = FALSE;
	cfg->proc = NULL;
	cfg->active = FALSE;
	cfg->proc = NULL;
	cfg->ingest_file[0] = 0;
	cfg->timer = FALSE;
	cfg->timer_slot = -1;
	cfg->delete = FALSE;
	cfg->rollover = FALSE;
	cfg->restart = FALSE;
	cfg->n_restarts = 0;
}
