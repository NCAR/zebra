/*
 * Ingest scheduler
 */
static char    *rcsid = "$Id: is.c,v 1.27 2001-10-30 22:50:58 granger Exp $";

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
#include <string.h>
#include <sys/wait.h>
#include <signal.h>
#include <varargs.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/errno.h>

#include <config.h>
#include <ui.h>
#include <ui_error.h>
#include <dirent.h>
#ifdef IS_WINDOW_MODE
#include <X11/Intrinsic.h>
#endif

#include "is_vars.h"
#include "is_cmds.h"
void            sigchldHandler();


void init_cfg (struct ui_command *cmds, struct is_config *cfg, char *name);
int is_shutdown();
char *substitute(char *, char *, char *);
char *last_part(char *, int);
void cfg_done(struct is_config *);
int is_exit FP((int exitcode));
void start_cfg (struct is_config *cfg);
int is_msg_handler (struct message *msg);
int is_initial(int arg, struct ui_command *cmds);
void mh_message (struct message *msg);
int process_term(char *name, int type, union usy_value *v, int junk);
void stop (struct is_config *cfg);

/*
 * Definitions of globals.
 */
stbl            Configs;	/* will hold all configurations, accesable by
				 * config name */
stbl		Groups;		/* will hold configuration groups */
char           *redirect = "none";	/* where to redirect stdout and
					 * stderr when an ingest process is
					 * spawned */
zbool		Batch = FALSE;	/* Continue after user input is done?*/
						

int
is_CPHandler (char *cmd)
/*
 * Perform the command, then make sure the output makes it out.
 */
{
	ERRORCATCH
	    ui_perform (cmd);
	ON_ERROR
	ENDCATCH
	fflush (stdout);
	fflush (stderr);
	return 0;
}


int
main(argc, argv)
	int             argc;
	char          **argv;
{
	int		exitcode = 0;
	int		i;
	char           *lfdir = NULL;	/* points to user specified location of the .lf file */
	char            loadfile[100];
#ifdef IS_WINDOW_MODE
	Widget          top;
#endif
#ifdef SVR4
	struct sigaction act;
#endif

	/*
	 * Hook into the message handler.
	 */

	msg_connect(is_msg_handler, "is");
	/* is this necessary? */
	msg_DeathHandler(is_shutdown);

	msg_ELog(EF_INFO, "%s", rcsid);
	msg_ELog(EF_INFO, "is started ");
	/*
	 * Look for -batch in the arg list, removing it if it's there
	 */
	for (i = 1; i < argc; i++)
	{
		if (! strcmp (argv[i], "-batch"))
		{
			Batch = TRUE;
			argc--;
			for (;i < argc; i++)
				argv[i] = argv[i+1];

			break;
		}
	}
	/*
	 * Look for -lfdir in the arg list; capture argument, remove both
	 */
	lfdir = GetLibDir();
	for (i = 1; i < argc; i++)
	{
		if (! strcmp (argv[i], "-lfdir"))
		  {
		    if (argc > 2) {
		      lfdir = argv[i+1];
		      argc -= 2;
		      for (;i < argc; i++)
			argv[i] = argv[i+2];
		    } else {
		      msg_ELog(EF_PROBLEM, 
			       "usage: is [-batch] [-lfdir lf_library_path] [file]");
		      ui_printf("usage: is [-batch] [-lfdir lf_library_path] [file]");
		      is_exit(2);
		    }
		    break;
		}
	}
	/*
	 * Get the interface set up.
	 */
	ERRORCATCH
		fixdir_t ("ISLOADFILE", lfdir, "is.lf", loadfile, ".lf");
		ui_init (loadfile, ! Batch, FALSE);
		ui_setup ("is", &argc, argv, (char *) 0);
	ON_ERROR
		exit (3);
	ENDCATCH

	/* ui_OutputRoutine (uio_print, uio_nfprint);
	ui_ErrorOutputRoutine (uio_ErrorOut);
	ui_ErrorHook (uio_EHook); */
	cp_SetupCmdHandler (is_CPHandler);
	/* cp_SetupCmdProto ();*/

	/*
	 * Create our symbol tables.
	 */
	Configs = usy_c_stbl("Configurations");
	Groups = usy_c_stbl("Groups");

	/*
	 * Indirect variables.
	 */
	tty_watch(msg_get_fd(), (void (*) ()) msg_incoming);

#ifdef IS_WINDOW_MODE
	/*
	 * force into window mode, since mode window followed by popup
	 * doesn't seem to work from a command file
	 */
	if (! Batch)
		uw_ForceWindowMode((char *) 0, &top, (XtAppContext *) 0);
#endif

	/*
	 * If a file appears on the command line, open it.
	 */
	if (argc > 1)
	{
		ERRORCATCH
			ut_open_file(argv[1], TRUE);
		ON_ERROR
			is_exit (9);
		ENDCATCH
	}
	/*
	 * catch the SIGCHLD signals so that we can keep track of spawned
	 * process status
	 */

/*
Stevens (pg. 277) specifies the following behaviors for the signal() and sigaction calls:

Functions   Systems    Signal handler     Ability to         Auto restart of interrupted
                      remains installed   block signals      system calls?

signal()    4.3BSD          Y                  Y                 default

sigaction()  SVR4           Y                  Y                 optional

*/

#ifdef SVR4
	act.sa_handler = sigchldHandler;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	act.sa_flags |= SA_RESTART;
	sigaction(SIGCHLD, &act, (struct sigaction *)0);
#else
	signal(SIGCHLD, sigchldHandler);
#endif

	/*
	 * Interpret commands.
	 */
	ERRORCATCH
	{
		ui_get_command("is-initial", "IS>", is_initial, 0);

		/*
		 * For batch mode, continue running after input is done,
		 * otherwise quit when there's no more input
		 */
		if (Batch)
		{
			while (TRUE)
				msg_await ();
		}
	}
	ON_ERROR
		exitcode = 1;
	ENDCATCH

	is_exit(exitcode);
}





int
is_msg_handler (struct message *msg)
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
	return 0;
}



void
mh_message (struct message *msg)
/*
 * Deal with a MESSAGE protocol msg.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;

	switch (tm->mh_type) {
	case MH_SHUTDOWN:
		ui_printf("Message handler shutdown -- I quit!\n");
		is_shutdown();

	default:
		ui_printf("Unknown MESSAGE proto msg %d\n", tm->mh_type);
		break;
	}
}


/*
 * These are meant to be private to add_config() and get_configs().
 */
static struct is_config *ret[MAXGC+1];
static int nret = 0;

int
add_config (char *name, int type, union usy_value *v, int all)
{
	struct is_config *cfg = (struct is_config *) v->us_v_ptr;
	ret[nret++] = cfg;
	return 1;
}


/*
 * Return null-terminated array of pointers to configs for this name or group.
 * If name is null, return all configs.
 */
struct is_config **
get_configs (char *name, int *ncfg)
{
    union usy_value v;
    int type;
    int i;
    struct NList *g;

    nret = 0;
    /*
     * name of null implies get all configs
     */
    if (name == 0)
    {
	usy_traverse (Configs, add_config, TRUE, FALSE);
    }
    else if (usy_g_symbol(Configs, name, &type, &v))
    {
	/*
	 * must be a configuration or group name, so look it up
	 */
	ret[nret++] = (struct is_config *) v.us_v_ptr;
    }
    else if (usy_g_symbol(Groups, name, &type, &v))
    {
	g = (struct NList *)v.us_v_ptr;
	for ( i = 0; i < g->n; i++)
	{
	    if (usy_g_symbol(Configs, g->list[i], &type, &v))
	    {
		ret[nret++] = (struct is_config *) v.us_v_ptr;
	    }
	    else
	    {
		ui_warning("Unknown configuration %s in group %s\n", 
			   g->list[i], name);
	    }
	}
    }
    else
    {
	ui_warning("Unknown configuration or group %s\n", name);
    }

    if (ncfg) *ncfg = nret;
    ret[nret] = 0;
    return ret;
}


struct is_config **
parse_config_list (struct ui_command *cmds)
{
	/*
	 * parse the command for a config list
	 */
	struct is_config **cfg = 0;

	switch (cmds[0].uc_ctype)
	{

	case UTT_VALUE:
	    cfg = get_configs (UPTR(cmds[0]), 0);
	    break;

	case UTT_KW:
	    cfg = get_configs (0, 0);
	    break;

	default:
	    ui_warning ("(BUG): Unknown keyword: %d\n", UKEY(cmds[0]));
	    break;
	}

	return cfg;
}



int 
is_exit (int exitcode)
{
	/*
	 * Now finish up here and quit.
	 */

	/*
	 * call the stop routine for all configurations
	 */
        struct is_config **cfg = get_configs (0, 0);
	while (*cfg)
	{
	    stop (*cfg);
	    ++cfg;
	}
	msg_ELog(EF_INFO, "is terminated");
	ui_finish();
	exit (exitcode);
}


int
is_shutdown()
/*
 * Shutdown the ingest scheduler.
 */

{
	is_exit (0);
	return 0;
}


int
is_start(struct ui_command *cmds)
{
	struct is_config **cfg = parse_config_list (cmds);
	while (cfg && *cfg)
	{
	    start_cfg (*cfg);
	    ++cfg;
	}
	return 0;
}


void
start_cfg (struct is_config *cfg)
{
	/*
	 * merely start the timer so that this cfg will run in a tick. File
	 * type configurations will have a non-zero interval, and so will
	 * repeat. Continuous types will have a zero interval, and so will
	 * not be repeated.
	 */

	extern int      errno;
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



void
stop (struct is_config *cfg)
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
	if (cfg->pid) {
		kill(cfg->pid, SIGHUP);
		cfg->pid = 0;
		cfg->active--;
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


int
is_stop (struct ui_command *cmds)
{
	struct is_config **cfg = parse_config_list (cmds);
	while (cfg && *cfg)
	{
	    stop (*cfg);
	    ++cfg;
	}
	return 0;
}


int
is_group (struct ui_command *cmds)
/*
 * Collect several configurations into a group
 */
{
	
	union usy_value v;
	char		gname[64];
	int             type;

	/* allocate a new group */
	struct NList *g = NEW(struct NList);

	/* get the group name */
	strcpy ( gname, UPTR(cmds[0]) );
	cmds++;

	/* get the configuration names */
	g->n = 0;
	while ( cmds->uc_ctype != UTT_END && g->n < MAXGC )
	{
	    if (usy_g_symbol(Configs, UPTR(cmds[0]), &type, &v))
	    {
		    g->list[g->n] =(char*)calloc( 1, strlen(UPTR(cmds[0])) + 1);
		    strcpy ( g->list[g->n], UPTR(cmds[0]) );
		    g->n = g->n + 1;
	    }
	    else
	        ui_warning ("Unknown configuration %s -- ignored\n", 
			    UPTR(cmds[0]) );
	    cmds++;
	}
	v.us_v_ptr = (char *) g;
	usy_s_symbol(Groups, gname, SYMT_POINTER, &v);
	return 0;
}


int
is_config (struct ui_command *cmds)
/*
 * parse out the configuration specifications
 */
{
	/*
	 * parse the config command only
	 */

	int             do_config();
	union usy_value v;
	int             complete;

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
	complete = 0;
	switch (cfg->type) {
	case IS_FTYPE:
		/*
		 * file type ingestor
		 */
		complete = (cfg->filename && cfg->process && cfg->interval);
		break;
	case IS_CTYPE:
		/*
		 * continuous type ingestor
		 */
		complete = (cfg->process != 0);
		break;
	case IS_PTYPE:
		/*
		 * periodic type ingestor
		 */
		complete = (cfg->process && cfg->interval);
		break;
	}

	if (complete)
	{
	    /*
	     * Enter configuration in symbol table, using name as
	     * symbol
	     */
	    v.us_v_ptr = (char *) cfg;
	    usy_s_symbol(Configs, UPTR(*cmds), SYMT_POINTER, &v);
	}
	else
	{
	    ui_warning("Configuration %s is incomplete and will be ignored\n",
		       UPTR(cmds[0]));
	}
	return 0;
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
				ui_warning
			("Max %d proc args allowed, rest are ignored\n",
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
		ui_warning("(BUG): Unknown keyword: %d\n", UKEY(*cmds));
	}
	return (TRUE);
}


int
is_redirect (struct ui_command *cmds)
{
	char *check_redirect;

	/*
	 * set the place to send stdout and stderr when ingest process is
	 * spawned
	 */

	switch (UKEY(*cmds)) {
	case ISC_NONE:
		redirect = "none";
		break;

	case ISC_TERM:
		redirect = "term";
		break;

	default:
		check_redirect = usy_string(UPTR(cmds[0]));
		if (access(check_redirect, (int) (W_OK | F_OK)))
		{
			ui_nf_printf("access not available for %s, "
				     "redirect ignored\n", check_redirect);
		}
		else
		{
			redirect = check_redirect;
		}
		break;
	}
	return 0;
}


int
list_cfg (struct is_config *cfg)
/*
 * List out a single configuration.
 */
{
	int             i;

	ui_nf_printf("Config '%s':\n", cfg->name);
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
		ui_nf_printf("\ttype:\t\t%s\n", "periodic");
		break;
	}
	ui_nf_printf("\tplatform:\t%s\n", cfg->platform ? cfg->platform : "<none>");
	ui_nf_printf("\tmovedir:\t%s\n", cfg->movedir ? cfg->movedir : "<none>");
	ui_nf_printf("\tdelete:\t\t%c\n", cfg->delete ? 'T' : 'F');
	ui_nf_printf("\tfilename:\t%s\n", cfg->filename ? cfg->filename : "<none>");
	ui_nf_printf("\tpid:\t\t%d\n", cfg->pid);
	ui_nf_printf("\tprocess:\t%s", cfg->process ? cfg->process : "<none>");
	for (i = 1; i < cfg->n_proc_args; i++)
		ui_nf_printf(" %s", cfg->proc_args[i]);
	ui_nf_printf("\n");
	ui_nf_printf("\tinterval:\t%d\n", cfg->interval);
	ui_nf_printf("\trollover:\t%c\n", cfg->rollover ? 'T' : 'F');
	ui_nf_printf("\ttimer slot:\t%d\n", cfg->timer_slot);
	ui_nf_printf("\tingest file:\t%s\n", cfg->ingest_file ? cfg->ingest_file : "<none>");
	ui_nf_printf("\tn_restarts:\t%d\n", cfg->n_restarts);

	return (TRUE);
}


int 
is_list (struct ui_command *cmds)
{
	struct is_config **cfg = parse_config_list (cmds);
	while (cfg && *cfg)
	{
	    list_cfg (*cfg);
	    ++cfg;
	}
	return 0;
}


void
cfg_go (struct is_config *cfg)
{
	int             i;
#ifdef SVR4
	pid_t           pid;
#else
	int             pid;
#endif

	static char    *new_args[MAX_PROC_ARGS];
	extern int      errno;
	char           *ptr_proc, *ptr_ifile;
#ifdef SVR4
	sigset_t 	sig_set;
#endif

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
	{
	        char *sub;
		sub = substitute(cfg->proc_args[i], "$p", cfg->platform);
		new_args[i] = substitute(sub, "$f", cfg->ingest_file);
		free (sub);
	}

	pid = fork();
	if (pid == 0) {
		int             fd;
		close(0);

		if (!strcmp(redirect, "none")) {

			/* no redirection, so close stdout and stderr */
			close(1);
			close(2);
		} else {
			if (!strcmp(redirect, "term")) {

				/*
				 * keep stdout and stderr open and connected
				 * to is
				 */
			} else {

				/* redirect stderr and stdout to another file */
				close(1);
				close(2);
				if ((fd = open(redirect, O_WRONLY)) < 0) {
					msg_ELog(EF_PROBLEM, "cannot open %s, errno = %d\n", redirect, errno);
				} else {
					dup2(fd, 1);
				}
				if ((fd = open(redirect, O_WRONLY)) < 0) {
					msg_ELog(EF_PROBLEM, "cannot open %s, errno = %d\n", redirect, errno);
				} else {
					dup2(fd, 2);
				}
			}
		}

		close(msg_get_fd());

		/*
		 * sleep for a second. Otherwise, the exec'd job can run and
		 * exit before we have had time to record the details in cfg
		 */
		sleep(1);

		/*
		 * allow child to take full responsibilty and get all of his
		 * signals
		 */
#ifdef SVR4
		sigemptyset(&sig_set);
		sigprocmask(SIG_SETMASK, &sig_set, (sigset_t *)0);
#else
		sigsetmask(0);
#endif

		execvp(cfg->process, new_args);
		printf("Unable to exec %s\n", cfg->process);
		perror(cfg->process);
		exit(1);
	} else {
		if (pid < 0) {
			msg_ELog(EF_PROBLEM,
				 "unable to fork %s, errno is %d", cfg->process, errno);
		} else {
			/*
			 * record the details in cfg
			 */
			cfg->pid = pid;
			cfg->active++;
			if ((cfg->type == IS_FTYPE) || (cfg->type == IS_PTYPE))
				cfg->timer = TRUE;
			else
				cfg->timer = FALSE;


			switch (cfg->type) {

			case IS_FTYPE:
				ptr_proc = last_part(cfg->process, '/');
				ptr_ifile = last_part(cfg->ingest_file, '/');
				msg_ELog(EF_INFO,
					 "%s:file [%d]%s f=%s",
				       cfg->name, pid, ptr_proc, ptr_ifile);
				break;
			case IS_PTYPE:
				ptr_proc = last_part(cfg->process, '/');
				msg_ELog(EF_INFO,
					 "%s:periodic [%d]%s rep:%d",
				   cfg->name, pid, ptr_proc, cfg->interval);
				break;
			case IS_CTYPE:
				ptr_proc = last_part(cfg->process, '/');
				msg_ELog(EF_INFO,
					 "%s:continuous [%d]%s",
					 cfg->name, pid, ptr_proc);
				break;
			}
		}
	}
 	for (i = 0; i < cfg->n_proc_args; i++)
	{
	    free (new_args[i]);
	}
}


int
is_initial(int arg, struct ui_command *cmds)
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

	case ISC_REDIRECT:
		is_redirect(cmds + 1);
		break;

	case ISC_GROUP:
		is_group(cmds + 1);
		break;

	default:
		ui_warning ("(BUG): Unknown keyword: %d\n", UKEY(*cmds));
	}
	return (TRUE);
}


static int     exit_status;
void
sigchldHandler()
{
	/*
	 * this handler is called when an ingestor terminates. Traverse the
	 * Configs table. process_term() is called to check each pid
	 * associated with ingestors, and then check for the termination of
	 * this process.
	 * 
	 */

#ifdef DEBUG
	ui_printf("\rgot a sigchld\n");
#endif

	usy_traverse(Configs, process_term, 0, FALSE);
}


int
process_term(char *name, int type, union usy_value *v, int junk)

/**

This function is called by usy_traverse(Configs,...)

See if the pid matches the Configs entry. If it
does not match, it is probably the popen process
terminating. If it does matche, do the following:

**/

{
	struct is_config *cfg = (struct is_config *) v->us_v_ptr;
	char           *ptr_proc;
#ifdef SVR4
	pid_t           wait_ret;
	pid_t           pid;
#else
	int             wait_ret;
	int             pid;
#endif
	int             wait_err;

	pid = cfg->pid;

	if (!pid)
		return (TRUE);

	msg_ELog(EF_DEBUG, " calling wait4() for %d", pid);

	if ((wait_ret =
#ifdef SVR4
	     waitpid(pid,
		   &exit_status,
		   WNOHANG) == pid))
#else
	     wait4(pid,
		   &exit_status,
		   WNOHANG,
		   (struct rusage *) 0)) == pid) 
#endif
	  {

		msg_ELog(EF_DEBUG, " found pid %d", pid);

		ptr_proc = last_part(cfg->process, '/');

			/* process exited using exit() */
		if (WIFEXITED(exit_status)) {
			msg_ELog(EF_INFO,
				 "%s      [%d]%s has exited with status %d",
				 name, pid, ptr_proc,
				 WEXITSTATUS(exit_status));
		} else {

			/* processes terminated due to a signal */
			msg_ELog(EF_INFO,
			"%s      [%d]%s has has terminated due to signal %d",
				 name, pid, ptr_proc,
				 WTERMSIG(exit_status));
		}

		/*
		 * now deal with the terminated ingestor, as directed by the
		 * configuration
		 */

		cfg_done(cfg);

	} else {
		if (wait_ret == 0) {
			msg_ELog(EF_DEBUG,
				 " did not find pid %d",
				 pid);
		} else {
			wait_err = errno;
			if (wait_err != ECHILD) {
				msg_ELog(EF_PROBLEM,
					 " wait4() or waitpid returned %d, pid %d, errno is %d:%s",
					 wait_ret, pid, wait_err,
					 strerror(wait_err));
			} else {

				/*
				 * sometimes our wait4() does not find the
				 * ingest pid, and we get an ECHILD error. I
				 * think this happens because the pclose()
				 * call assocaited with file searches has
				 * reported the ingestor rather than the
				 * popen() job (A trace on a simple program
				 * using popen() and pclose() shows that
				 * pclose() calls wait4() with pid == 0!).
				 * Just assume that the ingest process has
				 * terminated, and handle normally. Put out
				 * an error message, of course
				 */

				msg_ELog(EF_PROBLEM,
					 "ECHILD received for process [%d] %s, treated as normal termination",
				    cfg->pid, last_part(cfg->process, '/'));
				cfg_done(cfg);

			}

		}
	}


	return (TRUE);
}


/*************************************************************/
/**

respond to the termination of an ingestor:

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

void
cfg_done(cfg)
	struct is_config *cfg;

{
	int             find_file();

	cfg->pid = 0;
	cfg->active--;

	switch (cfg->type) {


	case IS_FTYPE:
		/*
		 * For file type ingestors, and if a move directory has been
		 * specified, go ahead and move the file . If only delete has
		 * been specified, merely delete it. Move will be honored
		 * over delete.
		 */
		if (cfg->movedir) 
		{
			char *move_file;
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
			free (move_file);

		} else if (cfg->delete) {
			if (unlink(cfg->ingest_file))
				msg_ELog(EF_PROBLEM, "Error %d Can't unlink %s",
					 errno, cfg->ingest_file);

		}
		/*
		 * look for next file and start ingest if it is there
		 */
		if (find_file(cfg)) {
			/*
			 * indicate that we are processing files in the
			 * rollover mode, so that timer doesn't start them as
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
		 * for continuous type ingestors, reshedule them under
		 * certain conditions
		 */
		if (cfg->restart)
                {
			if (cfg->n_restarts++ < MAX_RESTARTS)
				cfg_go(cfg);
			else
                        {
				msg_ELog(EF_PROBLEM,
				      "%s has exceeded restart limit of %d",
					 cfg->name, MAX_RESTARTS);
			}
                }
		break;

	default:
		break;
	}			/** switch (cfg->type) **/

}

void
timed_check(t, cfg)
	UItime           *t;
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
		if (!cfg->rollover && !cfg->active)
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
	/*
	 * find an elibible file for this configuration, and place
	 * information in the configurtion structure. return 1 if successful,
	 * 0 otherwise.
	 */
	struct stat     buff;
	struct stat    *buf = &buff;
	char            full_name[MAX_FILE_NAME];
	time_t          file_t;
	int             first = 1;
	int             ret = FALSE;

	FILE           *dir_file;
	char           *ls_command = (char *) malloc
	(strlen(LS_BEGIN) + MAX_FILE_NAME + strlen(LS_END) + 10);

	strcpy(ls_command, LS_BEGIN);
	strcat(ls_command, cfg->filename);
	strcat(ls_command, LS_END);

	first = 1;
	cfg->ingest_file[0] = 0;

	if (!(dir_file = popen(ls_command, "r"))) {
		msg_ELog(EF_PROBLEM, "Unable to popen for ls command: %s\n", ls_command);
		free (ls_command);
		return (FALSE);
	}
	while (!feof(dir_file)) {

		full_name[0] = 0;

		fscanf(dir_file, "%s", full_name);

		if (strlen(full_name)) {
			/*
			 * file matched!
			 */

			/*
			 * get the stats on this file
			 */

			if (!stat(full_name, buf)) {

				/* is it a regular file? */

				if (S_ISREG(buf->st_mode)) {

					/*
					 * insure that we can read this file.
					 * Directories that match this
					 * pattern are ignored. Files that we
					 * do not have access to generate
					 * warning, but are ignored.
					 */
					if (!access(full_name, (int) (R_OK | F_OK))) {

						/*
						 * find the oldest file that
						 * matches
						 */

						if (first) {
							file_t = buf->st_mtime;
							strcpy(cfg->ingest_file, full_name);
							first = 0;
						} else {
							if (buf->st_mtime < file_t) {
								file_t = buf->st_mtime;
								strcpy(cfg->ingest_file, full_name);
							}
						}
					} else {
						msg_ELog(EF_PROBLEM, "access not available for %s, \nfile will be ignored\n",
							 full_name);
					}
					ret = TRUE;
				}
			} else {
				msg_ELog(EF_PROBLEM, "unable to stat %s\n", full_name);
			}
		}
	}

	/*
	 * pclose() should be used here, since it is supposed to wait for the
	 * popen process to terminate. However, I found that it would call
	 * wait, and then call wait4(), which would hang. Has something to do
	 * with the fact that is execv's its own processes. Now just close()
	 * dir_file and ASSUME! that the popen job has terminated.
	 */

	pclose(dir_file);
	/***
		fclose(dir_file);
	***/
	free(ls_command);

	if (strlen(cfg->ingest_file)) {
		if (strrchr(cfg->ingest_file, '/'))
			strcpy(cfg->basename, strrchr(cfg->ingest_file, '/'));
		else
			strcpy(cfg->basename, cfg->ingest_file);
		return (TRUE);
	} else {
		return (FALSE);
	}

}


void
init_cfg (struct ui_command *cmds, struct is_config *cfg, char *name)
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
	cfg->pid = 0;
	cfg->ingest_file[0] = 0;
	cfg->timer = FALSE;
	cfg->timer_slot = -1;
	cfg->delete = FALSE;
	cfg->rollover = FALSE;
	cfg->restart = FALSE;
	cfg->n_restarts = 0;
}
/**************************************************************************/
char           *
substitute(source, pattern, string)
	char           *source;
	char           *pattern;
	char           *string;
/*
 * replace only one occurance of pattern in source with string
 * 
 * return is dynamically allocated, and should be freed when finished.
 * 
 */
{
	char           *starts_at;
	char           *ends_at;
	char           *ret;

	if (strstr(source, pattern)) {
		ret = malloc(strlen(source) - strlen(pattern) + strlen(string) + 1);

		ret[0] = 0;

		starts_at = (char *)strstr(source, pattern);

		ends_at = starts_at + strlen(pattern);

		strncpy(ret, source, starts_at - source);
		strcat(ret, string);
		strcat(ret, ends_at);

	} else {
		/* pattern not found, just return original source */
		ret = malloc(strlen(source) + 1);
		strcpy(ret, source);
	}
	return (ret);
}
/**************************************************************************/
char           *
last_part(str, cc)
	char           *str;
	char            cc;

/*
 * returns pointer to the substring found after the character c, or the whole
 * string if it is not found
 */


{

	char           *temp;

	if (!str)
		return ((char *) 0);

	temp = (char *)strrchr(str, cc);

	if (temp) {
		temp++;
		if (strlen(temp))
			return (temp);
		else
			return (str);
	} else
		return (str);
}
