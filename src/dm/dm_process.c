/*
 * Manage, execute, and search process classes and instances
 * -----------------------------------------------------------------------
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
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>

# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <message.h>	/* need msg_get_fd() */

# include <dm.h>
# include "dm_vars.h"

MAKE_RCSID ("$Id: dm_process.c,v 2.4 1995-05-24 00:13:52 granger Exp $")

/*
 * Private symbol tables containing ProcessClass structures indexed by 
 * class name, and Process structures indexed by message handle.
 */
static stbl ClassTable;
static stbl ProcessTable;

/*
 * We also put pointers to the classes in a null-terminated array,
 * for those times we need to search all of them.  I was too lazy
 * to deal with usy_traverse.
 */
static ProcessClass **ClassList;
static int NClass;
static Process **ProcessList;
static int NProcess;

static char ExecPath[CFG_SEARCHPATH_LEN];	/* path for executables */

static int UseWindowNames = 0;	/* use window name for process name */

/*
 * Prototypes for private functions
 */
static void dp_ProcessTableAdd FP ((Process *p));
static void dp_ClassTableAdd FP ((ProcessClass *pc));
static Process *dp_NewProcess FP ((ProcessClass *pc));
static void dp_AddMember FP ((ProcessClass *pc, Process *proc));
static void dp_RemoveMember FP ((ProcessClass *pc, Process *proc));
static ProcessClass *dp_NewClass FP ((char *name));
static char **dp_BuildArgs FP ((ProcessClass *pc, char *name, char *window,
				char *group));
static char *dp_NextHandle FP ((ProcessClass *pc, char *window));
static void dp_LogExec FP ((char *context, char *prog, char *argv[]));
static void dp_TestProcess FP ((char *argv[]));


void
dp_Init ()
/*
 * Initialize the process class symbol table
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	ProcessTable = usy_c_stbl ("ProcessTable");
	ClassTable = usy_c_stbl ("ClassTable");
	NClass = 0;
	ClassList = (ProcessClass **) malloc
			((NClass + 1) * sizeof(ProcessClass *));
	ClassList[ NClass ] = NULL;
	NProcess = 0;
	ProcessList = (Process **) malloc
			((NProcess + 1) * sizeof(Process *));
	ProcessList[ NProcess ] = NULL;

	strcpy (ExecPath, GetBinDir ());
	usy_c_indirect (vtable, "execpath", ExecPath, SYMT_STRING,
			CFG_SEARCHPATH_LEN);
}



void
dp_UseWindowNames (flag)
int flag;
{
	UseWindowNames = (flag) ? 1 : 0;
}



Process *
dp_LookupProcess (name)
char *name;
/*
 * Lookup the given process name in the process table and return a
 * pointer to the process.  Return NULL if the process is not found.
 */
{
	Process *p;
	union usy_value v;
	int type;

	if (! usy_g_symbol (ProcessTable, name, &type, &v))
	{
		msg_ELog (EF_DEVELOP, "lookup: process '%s' not found", name);
		return (NULL);
	}
	p = (Process *) v.us_v_ptr;
	return (p);
}




ProcessClass *
dp_LookupClass (classname)
char *classname;
{
	ProcessClass *pc;
	union usy_value v;
	int type;

	if (! usy_g_symbol (ClassTable, classname, &type, &v))
	{
		return (NULL);
	}
	pc = (ProcessClass *) v.us_v_ptr;
	return (pc);
}



void
dp_FreeAll ()
/*
 * Release all allocated memory
 */
{
	/*
	 * First the processes
	 */
	while (NProcess)
		dp_DeleteProcess (ProcessList[0]);
	/*
	 * Then the classes
	 */
	while (NClass)
		dp_DeleteClass (ClassList[0]);
	/*
	 * Don't forget the tables themselves
	 */
	if (ProcessList)
		free (ProcessList);
	ProcessList = 0;
	if (ClassList)
		free (ClassList);
	ClassList = 0;
	usy_z_stbl (ProcessTable);
	ProcessTable = 0;
	usy_z_stbl (ClassTable);
	ClassTable = 0;
}



void
dp_DeleteProcess (proc)
Process *proc;
/*
 * Uh-oh.  A process is no more.  Reverse the process of adding it.
 */
{
	int i;

	/*
	 * Detach it from any window
	 */
	if (proc->p_cfw)
		proc->p_cfw->cfw_process = NULL;
	/*
	 * Remove it from its class and the process table.
	 */
	dp_RemoveMember (proc->p_class, proc);
	usy_z_symbol (ProcessTable, proc->p_name);
	/*
	 * Remove it from the process list.
	 */
	for (i = 0; i < NProcess; ++i)
		if (ProcessList[i] == proc)
			break;
	--NProcess;
	for ( ; i < NProcess; ++i)
		ProcessList[i] = ProcessList[i+1];
	ProcessList[ NProcess ] = 0;
	free (proc);
}



void
dp_DeleteClass (pc)
ProcessClass *pc;
/*
 * Reverse the process of adding the class.
 */
{
	int i;

	if (pc->pc_members)
		free (pc->pc_members);
	/*
	 * Free the argument strings
	 */
	for (i = 0; i < pc->pc_argc; ++i)
		usy_rel_string (pc->pc_argv[i]);
	/*
	 * Remove it from its symbol table.
	 */
	usy_z_symbol (ClassTable, pc->pc_name);
	/*
	 * Remove it from the class list.
	 */
	for (i = 0; i < NClass; ++i)
		if (ClassList[i] == pc)
			break;
	--NClass;
	for ( ; i < NClass; ++i)
		ClassList[i] = ClassList[i+1];
	ClassList[ NClass ] = 0;
	/*
	 * Finally free the class structure itself
	 */
	free (pc);
}



Process **
dp_ProcessList (n)
int *n;
/*
 * Return a null-terminated array of pointers to all known processes.
 */
{
	if (n)
		*n = NProcess;
	return (ProcessList);
}



ProcessClass **
dp_ClassList (n)
int *n;
/*
 * Return a null-terminated array of pointers to all known process classes.
 */
{
	if (n)
		*n = NClass;
	return (ClassList);
}



static Process *
dp_NewProcess (pc)
ProcessClass *pc;
/*
 * Alloc a process and initialize it.
 */
{
	Process *p;
	
	p = (Process *) malloc (sizeof (Process));
	p->p_name[0] = '\0';
	p->p_win = None;
	p->p_pid = -1;
	p->p_state = P_NONE;
	p->p_class = pc;
	p->p_cfw = NULL;
	return (p);
}



static void
dp_AddMember (pc, p)
ProcessClass *pc;
Process *p;
/*
 * Add a process to its class
 */
{
	++pc->pc_nmembers;
	if (! pc->pc_members)
		pc->pc_members = (Process **) malloc (sizeof (Process *));
	else
		pc->pc_members = (Process **) realloc
			(pc->pc_members, pc->pc_nmembers * sizeof(Process *));
	pc->pc_members[ pc->pc_nmembers - 1 ] = p;
}



static void
dp_RemoveMember (pc, proc)
ProcessClass *pc;
Process *proc;
/*
 * Remove a process from its class
 */
{
	int i;
	/*
	 * Find the process in the list of members.
	 */
	for (i = 0; i < pc->pc_nmembers; ++i)
		if (pc->pc_members[i] == proc)
			break;
	if (i >= pc->pc_nmembers)
		return;
	/*
	 * We don't need to realloc since that will happen the
	 * next time the class needs more space for processes.
	 * Just copy the rest of the array down one slot and 
	 * decrement the count.
	 */
	--pc->pc_nmembers;
	for ( ; i < pc->pc_nmembers; ++i)
		pc->pc_members[i] = pc->pc_members[i + 1];
}



static ProcessClass *
dp_NewClass (name)
char *name;
{
	ProcessClass *pc;

	pc = (ProcessClass *) malloc (sizeof (ProcessClass));
	strncpy (pc->pc_name, name, sizeof(pc->pc_name));
	pc->pc_name[sizeof(pc->pc_name) - 1] = '\0';
	pc->pc_exec[0] = '\0';
	pc->pc_argc = 0;
	pc->pc_argv[pc->pc_argc] = NULL;
	pc->pc_xargs = 0;
	pc->pc_nmembers = 0;
	pc->pc_members = NULL;
	return (pc);
}



static void
dp_ClassTableAdd (pc)
ProcessClass *pc;
/*
 * Add this process class to the symbol table and class array
 */
{
	char buf[256];
	union usy_value v;

	v.us_v_ptr = (char *)pc;
	usy_s_symbol (ClassTable, pc->pc_name, SYMT_POINTER, &v);
	++NClass;
	ClassList = (ProcessClass **) realloc
		(ClassList, (NClass + 1) * sizeof(ProcessClass *));
	ClassList[ NClass - 1 ] = pc;
	ClassList[ NClass ] = NULL;
	sprintf (buf, "new class %s", pc->pc_name);
	dp_LogExec (buf, pc->pc_exec, pc->pc_argv);
}




static void
dp_ProcessTableAdd (p)
Process *p;
/*
 * Add this process to the symbol table
 */
{
	union usy_value v;

	v.us_v_ptr = (char *)p;
	usy_s_symbol (ProcessTable, p->p_name, SYMT_POINTER, &v);
	++NProcess;
	ProcessList = (Process **) realloc
		(ProcessList, (NProcess + 1) * sizeof(Process *));
	ProcessList[ NProcess - 1 ] = p;
	ProcessList[ NProcess ] = NULL;
	msg_ELog (EF_DEBUG, "new process %s (class %s): pid %li",
		  p->p_name, p->p_class->pc_name, p->p_pid);
}




static char *
dp_NextHandle (pc, window)
ProcessClass *pc;
char *window;
/*
 * Return a unique message handle for a new process.  We must base the
 * name on our dm name to ensure it is different from processes started
 * by another display.  Throw in the name of the window for the sake of
 * descriptiveness.  The returned string is only valid until the next
 * call to dp_NextHandle.  Using the initial window name keeps us functional
 * in the old sense (and in existing configurations) where message handles
 * were exactly the window name.
 *
 * To generate unique process names, we just keep incrementing a number
 * which we tack onto the end of the base name.  We could put the id in the
 * class so that id numbers are sequential in a class, but not today...
 */
{
	static int next_id = 1;
	static char name[ PROC_NAME_LEN ];

	if (UseWindowNames)
		strcpy (name, window);
	else
		sprintf (name, "%s-%s-%d", window, msg_myname(), next_id++);
	return (name);
}



ProcessClass *
dp_DefineClass (name, exec, args, xargs)
char *name;
char *exec;
char **args;
int xargs;
/*
 * Create a class with the given name, exec, and argument list, where args
 * is null-terminated.  The argument list DOES NOT include argv[0], the
 * exec name.  The argv array must point to copies of the argument strings. 
 * If the class already exists, it gets redefined but it retains any 
 * existing members.
 */
{
	ProcessClass *pc;
	int add;
	int i;

	pc = dp_LookupClass (name);
	add = 0;
	if (pc)
	{
		for (i = 0; i < pc->pc_argc; ++i)
			usy_rel_string (pc->pc_argv[i]);
		pc->pc_argc = 0;
		pc->pc_argv[0] = NULL;
	}
	else
	{
		++add;
		pc = dp_NewClass (name);
	}
	strcpy (pc->pc_exec, exec);
	pc->pc_argv[0] = usy_string (exec);
	i = 0;
	while (args[i])
	{
		pc->pc_argv[i + 1] = usy_string (args[i]);
		++i;
	}
	pc->pc_argc = i + 1;
	pc->pc_argv[ pc->pc_argc ] = NULL;
	pc->pc_xargs = xargs;
	if (add)
		dp_ClassTableAdd (pc);
	return (pc);
}




ProcessClass *
dp_DefaultClass ()
/*
 * Set up the Default process class from the default_ variables, if they
 * exist, else supply our own.
 */
{
	SValue v;
	int type;
	ProcessClass *pc;
	stbl vtable = usy_g_stbl ("ui$variable_table");
/*
 * Get the default executable.
 */
	pc = dp_NewClass (DEFAULT_PROCESS);
	if (usy_g_symbol (vtable, "default_exec", &type, &v))
	{
		strcpy (pc->pc_exec, v.us_v_ptr);
		/*
		 * Look for default arg variables also.
		 */
		for (pc->pc_argc = 1; pc->pc_argc < MAXARG; ++pc->pc_argc)
		{
			char sname[30];
			sprintf (sname, "default_arg%d", pc->pc_argc);
			if (! usy_g_symbol (vtable, sname, &type, &v))
				break;
			pc->pc_argv[pc->pc_argc] = usy_string (v.us_v_ptr);
		}
	}
	else	/* Use the compile-time defaults */
	{
		msg_ELog (EF_DEBUG, "using compile-time exec default: %s %s",
			  DEFAULT_EXEC, DEFAULT_ARG);
		strcpy (pc->pc_exec, DEFAULT_EXEC);
		pc->pc_argv[1] = usy_string (DEFAULT_ARG);
		pc->pc_argc = 2;
	}
	pc->pc_argv[0] = usy_string (pc->pc_exec);
	pc->pc_argv[pc->pc_argc] = 0;
/*
 * Add this class to the symbol table
 */
	dp_ClassTableAdd (pc);
	return (pc);
}



ProcessClass *
dp_MatchArgs (exec, narg, args)
char *exec;
int narg;
char **args;
/*
 * Try to match the given exec name and arg list to an existing
 * process class, and return that class.  If no match, return NULL.
 * Note that we only match command-line arguments, those other than argv[0].
 * The first (0th) element of 'args' corresponds to argv[1] and 
 * not the exec name.
 */
{
	ProcessClass **pcp;
	ProcessClass *pc;

	pcp = ClassList;
	while (*pcp)
	{
		int i;
		pc = *pcp;
		if ((pc->pc_argc == narg+1) && !strcmp (pc->pc_exec, exec))
		{
			for (i = 1; i < pc->pc_argc; ++i)
			{
				if (strcmp (pc->pc_argv[i], args[i-1]))
					break;
			}
			if (i >= pc->pc_argc)
				break;
		}
		++pcp;
	}
	return (*pcp);
}




static char **
dp_BuildArgs (pc, name, window, group)
ProcessClass *pc;
char *name;
char *window;
char *group;
/*
 * Build an arg list which tells the process the message handler name
 * it should use, and the name of the display manager to which it should
 * connect.  The list has to be freed with dp_FreeArgs.
 */
{
	char **argv;
	int argc, i;

	argc = pc->pc_argc;
	argv = (char **) malloc ( (argc + 9) * sizeof(char *) );
	for (i = 0; i < argc; ++i)
		argv[i] = usy_string (pc->pc_argv[i]);
	argv[argc++] = usy_string ("-name");
	argv[argc++] = usy_string (name);
	argv[argc++] = usy_string ("-dm");
	argv[argc++] = usy_string ((char *)msg_myname());
	argv[argc++] = usy_string ("-window");
	argv[argc++] = usy_string (window);
	argv[argc++] = usy_string ("-join");
	argv[argc++] = usy_string (group);
	argv[argc] = NULL;
	if (pc->pc_xargs)	/* omit implicit dm client arguments */
	{
		/*
		 * Note omitted args so they can be pasted into things
		 * like debuggers, but then remove them from the arg list.
		 */
		dp_LogExec ("omitting implicit args", pc->pc_exec, argv);
		for (i = pc->pc_argc; i < argc; ++i)
			usy_rel_string (argv[i]);
		argv[pc->pc_argc] = NULL;
	}
	return (argv);
}



static void
dp_FreeArgs (argv)
char **argv;
/*
 * Free an arg list built with dp_BuildArgs.
 */
{
	char **arg;

	arg = argv;
	while (*arg)
		usy_rel_string (*arg++);
	free (argv);
}



static void
dp_LogExec (context, prog, argv)
char *context;
char *prog;
char *argv[];
{
	char buf[256];

	msg_ELog (EF_DEBUG, "%s: [%s]", context, prog);
	sprintf (buf, "argv: ");
	while (*argv)
		sprintf (buf+strlen(buf), "%s ", *argv++);
	msg_ELog (EF_DEBUG, "%s", buf);
}




static int
dp_handler ()
{
	return (0);
}



Process *
dp_ExecProcess (pc, window, group, created)
ProcessClass *pc;
char *window;	/* The name of the window we'll be realizing */
char *group;
int *created;	/* Non-zero if we return a newly forked process */
/*
 * Exec a process for this process class, with the given message name, and
 * adding it to the class' members.
 */
{
	char abs_prog[CFG_FILEPATH_LEN];
	Process *proc = NULL;
	char **argv;
	pid_t pid;
	char *name;

	/*
	 * Decide on the message handle for this process: either unique
	 * for this message session or identical to the window name.
	 */
	*created = 0;
	name = dp_NextHandle (pc, window);
	/*
	 * See if a process by this name already exists.
	 */
	if ((proc = dp_LookupProcess (name)))
	{
		/* 
		 * uh-oh --- someone has tried to create a process with
		 * the same name as another but under a different class.
		 * So we throw up our hands and hand back the existing
		 * process, hoping that doesn't break anything.
		 */
		msg_ELog (EF_PROBLEM, 
			  "process '%s' already exists in class '%s'",
			  proc->p_name, proc->p_class->pc_name);
		return (proc);
	}
	/*
	 * Look for the executable
	 */
	if (! FindFile (pc->pc_exec, ExecPath, abs_prog))
	{
		msg_ELog (EF_PROBLEM, "'%s' not in ExecPath '%s'",
			  pc->pc_exec, ExecPath);
		return (NULL);
	}
	/*
	 * We need to build an arg list by appending the dm command-line
	 * args to the class args.  The array should be freed by us.
	 */
	argv = dp_BuildArgs (pc, name, window, group);
	dp_LogExec ("exec", abs_prog, argv);
	if ( (pid = fork()) == 0 )
	{
		/*
		 * Have fork, will exec.
		 * Disconnect from message first.
		 */
		msg_disconnect ();
		/* ui_finish (); ... bad things happen */

		if (TestMode)
			dp_TestProcess (argv);
		else
			execv (abs_prog, argv);
		/*
		 * No luck.  Connect to message and log our complaints.
		 * Could also send a message directly to the display manager
		 * that our exec failed, but not yet...
		 */
		msg_connect (dp_handler, name);
		msg_ELog (EF_PROBLEM, "Unable to exec '%s'\n", abs_prog);
		perror (abs_prog);
		_exit (1);
	}
	else if (pid < 0)		/* fork failed */
	{
		perror ("fork");
	}
	else
	{
		/*
		 * Assume the exec was successful and add the process
		 * to the class. Also set the process's pid and name.
		 */
		proc = dp_NewProcess (pc);
		proc->p_pid = pid;
		strcpy (proc->p_name, name);
		dp_AddMember (pc, proc);
		dp_ProcessTableAdd (proc);
		*created = 1;
	}
	dp_FreeArgs (argv);		     
	return (proc);
}



Process *
dp_NamedProcess (pc, name)
ProcessClass *pc;
char *name;
/*
 * Just create a process structure with the given name, used for tracking
 * processes which already exist and do not need to be forked.
 */
{
	Process *proc = NULL;
	/*
	 * Add the process to the class and set the name.
	 */
	proc = dp_NewProcess (pc);
	strcpy (proc->p_name, name);
	dp_AddMember (pc, proc);
	dp_ProcessTableAdd (proc);
	return (proc);
}




static int
dp_TestHandler (msg)
struct message *msg;
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
	struct dm_msg *dmsg = (struct dm_msg *) msg->m_data;
/*
 * Just branch out on the message type.
 */
	switch (msg->m_proto)
	{
	/*
	 * Display manager messages.
	 */
	   case MT_DISPLAYMGR:
		dmsg_Show (msg->m_from, dmsg, msg->m_len, "received from");
		if (dmsg->dmm_type == DM_DIE)
			exit (0);
		else if (dmsg->dmm_type == DM_RECONFIG)
			dm_Reconfig ((struct dm_reconfig *)dmsg);
		break;
	/*
	 * Message handler stuff.  The only thing we know how to deal
	 * with now is SHUTDOWN.
	 */
	   case MT_MESSAGE:
	   	if (tm->mh_type == MH_SHUTDOWN)
		{
			msg_ELog (EF_DEBUG,
				  "shutting down by order of message manager");
			exit (0);
		}
		msg_ELog (EF_PROBLEM, "Unknown MESSAGE proto type: %d",
			  tm->mh_type);
		break;
	   default:
		msg_ELog (EF_PROBLEM, "unknown protocol %d", msg->m_proto);
		break;
	}
	return (0);
}




static void
dp_TestProcess (argv)
char *argv[];
{
	char **arg;
	int argc;

	/*
	 * Clean up from our past life before entering the after-life.
	 */
	ReleaseMemory ();
	/*
	 * Count our arguments and pass them into the library
	 */
	argc = 0;
	arg = argv;
	while (*arg++)
		++argc;
	dm_Setup (&argc, argv, NULL);
	msg_connect (dp_TestHandler, dm_MessageName());
	msg_join (dm_GroupName());
	msg_ELPrintMask (EF_ALL);
	msg_ELog (EF_DEBUG, "[handle %s] window %s, display mgr %s, group %s",
		  msg_myname(), dm_WindowName(), dm_ManagerName(),
		  dm_GroupName());
	msg_ELog (EF_DEBUG, "sending greeting to display manager");
	dm_Greet ();
	msg_ELog (EF_DEBUG, "sending window id of %li", getpid());
	dm_SendWindowID ((Window)getpid());
	/*
	 * Now just hang around and see what the display manager
	 * sends us.
	 */
	msg_await();
}


