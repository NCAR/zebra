/* $Id: dm_process.h,v 2.4 1995-05-24 00:13:54 granger Exp $
 *
 * Define the ProcessClass and Process structures, and the public interface
 */
#ifndef _zeb_dm_process_h_
#define _zeb_dm_process_h_

#include <sys/types.h>
#include <config.h>
#include <defs.h>

#define PROC_NAME_LEN	40
#define MAXARG		30

/*
 * The default process class for a window.
 */
# define DEFAULT_PROCESS "Default"
# define DEFAULT_EXEC "gp"
# define DEFAULT_ARG "gp.init"

/*
 * A process class which tells how to execute (instantiate) a particular
 * class of processes, and also contains the list of processes of this
 * class which are already executing.  DM searches the pc_members list
 * to find an inactive but existing process.  Note that the significant
 * differences between classes, besides the class name, will be the
 * exectuable name and the command-line arguments.  It might be possible
 * to add environment settings later.
 *
 * The class might also distinguish between processes which will always
 * serve certain types of plots.  I.e., a graphics process in the CAP class
 * will never try to load skew-t or xy-graph modules, thus keeping the
 * process smaller.
 */
typedef struct s_ProcessClass
{
	char pc_name[PROC_NAME_LEN];	/* name of the class */
	char pc_exec[CFG_FILEPATH_LEN];	/* executable */
	char *pc_argv[MAXARG];		/* null-terminated argument list */
	int pc_xargs;			/* don't add args to argv list */
	int pc_argc;			/* argument count */
	int pc_nmembers;		/* number of members */
	struct s_Process **pc_members;	/* member processes of this class */
}
ProcessClass;

/*
 * State structure for an executing process.  It knows nothing about
 * configuration windows (graphic or non-graphic), only its class, its
 * window id, it's state, and how many times it has died.  It is mapped to
 * a configuration window (as opposed to the X window which the process
 * realizes and whose id is contained here) externally. 
 */
typedef struct s_Process
{
	char p_name[PROC_NAME_LEN];	/* message handler name */
	ProcessClass *p_class;		/* pointer to this process' class */
	Window	p_win;			/* X window id, if it has one */
	pid_t	p_pid;			/* Process id */
	enum {
		P_NONE, P_UNMAPPED, P_ICONIFIED, P_ACTIVE, P_ASSIGNED
	} p_state;			/* Current state */
	struct cf_window *p_cfw;	/* window currently handling, if any */
}
Process;

/* 
 * Public interface prototypes
 */
void dp_Init FP ((void));
void dp_UseWindowNames FP ((int));
Process *dp_LookupProcess FP ((char *name));
Process **dp_ProcessList FP ((int *n));
ProcessClass **dp_ClassList FP ((int *n));
ProcessClass *dp_LookupClass FP ((char *classname));
Process *dp_NamedProcess FP ((ProcessClass *pc, char *name));
ProcessClass *dp_DefineClass FP ((char *name, char *exec, char **args,
				  int xargs));
ProcessClass *dp_DefaultClass FP ((void));
ProcessClass *dp_MatchArgs FP ((char *exec, int narg, char **args));
Process *dp_ExecProcess FP ((ProcessClass *pc, char *name, char *group,
			     int *created));
void dp_DeleteProcess FP ((Process *proc));
void dp_DeleteClass FP ((ProcessClass *pc));
void dp_FreeAll FP ((void));

#endif /* ndef _zeb_dm_process_h_ */

