/*
 * This is the main program for the data store daemon.
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

# include <sys/types.h>

# if defined(SVR4) || defined(__osf__)
#    include <sys/statvfs.h>
# else
# ifdef AIXV3
#    include <sys/statfs.h>
# else
#    include <sys/vfs.h>
# endif /* AIX */
# endif /* SVR4 */

# include <fcntl.h>
# include <errno.h>
# include <string.h>
# include <stdio.h>
# include <unistd.h>

# include <ui.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <zl_regex.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "Platforms.h"
# include "dslib.h"
# include "dfa.h"
# include "dsDaemon.h"
# include "commands.h"

RCSID ("$Id: Daemon.c,v 3.66 1998-12-17 17:17:47 burghart Exp $")

/*
 * Local forwards.
 */
static void	StartInit FP ((void));
static void	FinishInit FP ((void));
static void	OverrideSettings FP ((int argc, char **argv));
static int 	msg_Handler FP ((struct message *));
static int	ui_Handler FP ((int, struct ui_command *));
static void	mh_message FP ((struct message *));
static void	ds_message FP ((char *, struct dsp_Template *, int len));
static void	dp_NewFile FP ((char *, struct dsp_CreateFile *));
static void	dp_AbortNewFile FP ((struct dsp_AbortNewFile *));
static void	dp_UpdateFile FP ((char *, struct dsp_UpdateFile *));
static void	dp_DeleteData FP ((PlatformId pid, ZebTime *));
static void	dp_DeleteObs FP ((PlatformId pid, ZebTime *));
static void	ZapDF FP ((DataFile *));
static void	DoRescan FP ((struct ui_command *cmds));
static void	SetUpEvery FP ((struct ui_command *));
static void	ExecEvery FP ((ZebTime *, int));
static void	Truncate FP ((struct ui_command *));
static int	FreeSpace FP ((int, SValue *, int *, SValue *, int *));
static int	BCHandler FP ((int, char *, int));
static void	BCSetup FP ((char *, int));
static void	RemDataGone FP ((struct dsp_BCDataGone *));
static void 	SendNPlat FP ((char *));
static void	SendPlatStruct FP ((char *, struct dsp_GetPlatStruct *));
static void	SendClassStruct FP ((char *, struct dsp_GetPlatStruct *));
static void	SendPlatformList FP ((char *, struct dsp_PlatformSearch *));
static int 	MatchPlatform FP ((char *symbol, int type,
				   union usy_value *value,
				   struct SearchInfo *info));
static void	SendFileStruct FP ((char *, int));
static void	LockPlatform FP ((char *, PlatformId));
static void	UnlockPlatform FP ((char *, PlatformId, int));
static void	WriteLock FP ((char *, PlatformId));
static void	ReleaseWLock FP ((char *, PlatformId, int));
static void	CleanClientLocks FP ((char *who));
static int	CleanQueue FP ((Lock **q, char *who));
static int	AwaitUnlock FP ((Message *, int));
static Lock	*GetLockEntry FP ((void));
static void	DoLookup FP ((char *, char *));
static void	DoClassLookup FP ((char *who, char *name));
static void	FindDF FP ((char *, struct dsp_FindDF *));
static void	FindAfter FP ((char *, struct dsp_FindDF *));
static int	NewRevision FP ((Platform *plat, DataFile *df));
static void	ClientDefine FP ((char *who, struct dsp_ClassStruct *, int));
#ifdef notdef
static void	ClientAddSubplat FP ((PlatClassId id, SubPlatform *sp));
#endif
static void	ClientInstance FP ((char *who, PlatClassId cid, char *name,
				    PlatformId parent));


/*
 * Public forwards
 */
int		Shutdown FP ((void));
void		DataFileGone FP ((DataFile *df));

/*
 * Broadcast stuff.
 */
static int BCastSocket = -1;

/*
 * Our data structure for the EVERY command.
 */
# define MAXEVERY 10
int NEvery = 0;
char *ECmds[MAXEVERY];

/*
 * Lookaside list for lock structures.
 */
static Lock *FreeLocks = 0;

zbool InitialScan = TRUE;
int PlatformsScanned = 0;

zbool DelayDataDirs = FALSE;

time_t LastScan = 0;	/* Time of the most recent FULL scan */
time_t LastCache = 0;	/* Time to which cache files are up-to-date */
time_t Genesis;		/* In the beginning, there was Zeb...*/

/*
 * Caching options.
 */
zbool LDirConst = FALSE;		/* Nothing changes		*/
zbool RDirConst = FALSE;
zbool LFileConst = FALSE;	/* Files don't change (but they	*/
zbool RFileConst = FALSE;	/* can come and go)		*/
zbool CacheOnExit = FALSE;	/* Write cache on way out?	*/

/*
 * Memory allocation options.
 */
int PTableSize = 200;	/* Platform table initial size	*/
int PTableGrow = 50;	/* Amount to grow by		*/
int CTableSize = 100;	/* Class table initial size	*/
int CTableGrow = 50;	/* Amount to grow by		*/
int DFTableSize = 2000;	/* Data file table size		*/
int DFTableGrow = 500;	/* Amount to grow by		*/

/*
 * Other options
 */
zbool StatRevisions = TRUE;	/* Use stat() calls to get revision 
				   numbers, rather than using a counter	*/

zbool Debug = FALSE;		/* Produce voluminous output as it happens */
zbool ParseOnly = FALSE;	/* Parse the init file and abort	*/
int InvalidatesSent = 0;	/* Number of CacheInvalidate broadcasts */
int ReadLockRequests = 0;
int WriteLockRequests = 0;


static int Argc;
static char **Argv;


static inline int
Match(arg,opt)
char *arg;
char *opt;
{
	int len = strlen(arg);

	return (len > 1 && strncmp(arg, opt, len) == 0);
}


static void
usage (prog)
char *prog;
{
	printf("usage: %s [options] [variables] [initfile ...]\n", prog);
	printf("options: [can be abbreviated]\n");
	printf("   -help       Show this usage message\n");
	printf("   -parse      Parse config files and exit\n");
	printf("   -debug      Print log messages\n");
	printf("   -version    Print version information\n");
	printf("   -copyright  Print copyright information\n");
	printf("variables: \n");
	printf("   name=value  Override config file variable\n");
	printf("initfile: \n");
	printf("               %s\n",
	       "Multiple init files are read in the order given");
}



int
main (argc, argv)
int argc;
char **argv;
{
	char loadfile[80];
	int argt = SYMT_STRING;
	stbl vtable;
	int i;

#ifdef NoBuffer
	setvbuf (stdout, NULL, _IONBF, 0);
	setvbuf (stderr, NULL, _IONBF, 0);
#endif
/*
 * Set up the command-line options
 */
	Argc = argc;
	Argv = argv;
	for (i = 1; i < argc; ++i)
	{
		if (Match (argv[i], "-debug"))
			Debug = TRUE;
		else if (Match (argv[i], "-parse"))
			ParseOnly = TRUE;
		else if (Match (argv[i], "-help"))
		{
			usage (argv[0]);
			exit (0);
		}
		else if (Match (argv[i], "-version"))
		{
			printf ("%s%s", Z_version(), Z_cppsymbols());
			printf ("%s", Z_rcsid());
			printf ("DataStore protocol version: %#0x\n",
				DSProtocolVersion);
			printf ("Message protocol version: %s\n",
				MSG_PROTO_VERSION);
			exit (0);
		}
		else if (Match (argv[i], "-copyright"))
		{
			printf ("%s", Z_copyright());
			exit (0);
		}
		/* variable settings and init files handled later */
	}
/*
 * Hook into the message system.
 */
	if (! msg_connect (msg_Handler, DS_DAEMON_NAME))
	{
		printf ("%s: unable to connect to message handler\n", argv[0]);
		exit (1);
	}
		
	if (Debug)
		msg_ELPrintMask (EF_ALL);

#ifdef MSG_CLIENT_EVENTS
	msg_join (MSG_CLIENT_EVENTS);
#else
	msg_join ("Client events");
#endif
	msg_DeathHandler (Shutdown);
	msg_SetQueryHandler (dbg_AnswerQuery);
/*
 * Hook into the UI.
 */
	fixdir_t ("DSDLOADFILE", GetLibDir (), "dsDaemon.lf", loadfile, ".lf");
	ui_init (loadfile, FALSE, TRUE);
	SetupConfigVariables ();
	cp_SetupCmdProto ();
/*
 * Initialize.
 */
	vtable = usy_g_stbl ("ui$variable_table");
	dt_InitDirectories ();
	usy_c_indirect (vtable, "datadir", DefDataDir, SYMT_STRING, DDIR_LEN);
	usy_c_indirect (vtable, "remdatadir", RemDataDir, SYMT_STRING,
			DDIR_LEN);
	usy_c_indirect (vtable, "DisableRemote", &DisableRemote, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "StatRevisions", &StatRevisions, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "Debug", &Debug, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "ParseOnly", &ParseOnly, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "LDirConst", &LDirConst, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "RDirConst", &RDirConst, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "LFileConst", &LFileConst, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "RFileConst", &RFileConst, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "CacheOnExit", &CacheOnExit, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "DelayDataDirs", &DelayDataDirs, SYMT_BOOL, 0);
/*
 * Make DefaultKeep from Platforms.c accessible.
 */
	usy_c_indirect (vtable, "DefaultKeep", &DefaultKeep, SYMT_INT, 0);
/*
 * Indirects for the table parameters too.
 */
	usy_c_indirect (vtable, "PTableSize", &PTableSize, SYMT_INT, 0);
	usy_c_indirect (vtable, "PTableGrow", &PTableGrow, SYMT_INT, 0);
	usy_c_indirect (vtable, "CTableSize", &CTableSize, SYMT_INT, 0);
	usy_c_indirect (vtable, "CTableGrow", &CTableGrow, SYMT_INT, 0);
	usy_c_indirect (vtable, "DFTableSize", &DFTableSize, SYMT_INT, 0);
	usy_c_indirect (vtable, "DFTableGrow", &DFTableGrow, SYMT_INT, 0);
/*
 * Other initialization.
 */
	F_Init ();
	dap_Init ();
	uf_def_function ("freespace", 1, &argt, FreeSpace);
/*
 * Enter the initial command state, from where the ui$init procedure calls
 * our StartInit() hook.
 */
	ui_get_command ("initial", "dsd>", ui_Handler, 0);
	msg_ELog (EF_PROBLEM, "ui command interpreter failed; shutting down");
	Shutdown ();
	return (0);
}



static void
StartInit ()
/*
 * This is the first thing called by the load file ui$init procedure.
 * Parse our global argv array for init files and read each one.  When this
 * routine exits, all the init files have been read, and the ui$init
 * procedure calls 'done' to kick us into 'FinishInit'.
 */
{
	char cmd[256];
	int init = 0;
	int i;

	for (i = 1; i < Argc; ++i)
	{
		if ((Argv[i][0] != '-') && (strchr (Argv[i], '=') == 0))
		{
			sprintf (cmd, "read '%s'", Argv[i]);
			msg_ELog (EF_DEBUG, "Reading init file '%s'", 
				  Argv[i]);
			ui_perform (cmd);
			++init;
		}
	}
	if (! init)
	{
		char *msg = "No init file on datastore daemon command line!";
		msg_ELog (EF_INFO, "%s", msg);
		printf ("%s\n", msg);
	}
	else
	{
		msg_ELog (EF_DEBUG, "%d init files read", init);
	}
}



static void
OverrideSettings (argc, argv)
int argc;
char **argv;
/*
 * Translate non-option args of the form name=value into 'set name value'
 * UI commands and perform them.
 */
{
	int i;
	char *eq;
	char cmd[512];

	for (i = 1; i < argc; ++i)
	{
		if ((argv[i][0] != '-') && ((eq = strchr(argv[i], '='))))
		{
			*eq++ = '\0';
			sprintf (cmd, "set %s %s", argv[i], eq);
			printf ("%s\n", cmd);
			ui_perform (cmd);
		}
	}
}



static void
FinishInit ()
/*
 * Finish our initialization.
 */
{
	ZebTime t1, t2;
	int type;
	SValue v;
/*
 * Override config file variables with any cmd-line settings
 */
	OverrideSettings (Argc, Argv);
	if (Debug)
		msg_ELPrintMask (EF_ALL);
/*
 * Keep track of when we start.
 */
	Genesis = time (NULL);
/*
 * If they have defined a mondo cache file, pull it in now.
 */
	if (usy_g_symbol (usy_g_stbl ("ui$variable_table"), "cachefile",
			  &type, &v))
		ReadCacheFile (v.us_v_ptr, TRUE);
	if (usy_g_symbol (usy_g_stbl ("ui$variable_table"), "remotecachefile",
			  &type, &v))
		ReadCacheFile (v.us_v_ptr, FALSE);
/*
 * Perform the file scan to see what is out there.
 */
	msg_ELog (EF_INFO, "Starting file scan (%s)", DefDataDir);
	tl_Time (&t1);
	DataScan ();
	tl_Time (&t2);
	msg_ELog (EF_INFO, "Scan done, took %d secs", t2.zt_Sec - t1.zt_Sec);
	InitialScan = FALSE;
/*
 * If a cleanup_procedure exists, run it now.
 */
	if (usy_defined (0, "ui$procedure_table:cleanup"))
		ui_perform ("cleanup");
/*
 * Now just wait for something to happen.
 */
	while (1)
	{
		int ret = msg_await ();
		msg_ELog (EF_PROBLEM, "msg_await returned %d, errno is %d: %s",
			  ret, errno, "continuing");
	}
}



static int
msg_Handler (msg)
struct message *msg;
/*
 * Deal with an incoming message.
 */
{
/*
 * See just what sort of message we have here.
 */
	switch (msg->m_proto)
	{
	/*
	 * MH stuff.
	 */
	   case MT_MESSAGE:
	   	mh_message (msg);
		break;
	/*
	 * The really interesting stuff -- datastore protocol.
	 */
	   case MT_DATASTORE:
	   	ds_message (msg->m_from, (struct dsp_Template *) msg->m_data,
			    msg->m_len);
		break;
	}
	return (0);
}





static int
ui_Handler (junk, cmds)
int junk;
struct ui_command *cmds;
/*
 * Deal with a command.
 */
{
	static int ndone = 0;

	switch (UKEY (*cmds))
	{
	/*
	 * Platform definition.
	 */
	   case DK_PLATFORM:
		if (cmds[2].uc_ctype != UTT_END)
			dc_DefPlatform (UPTR (cmds[1]), UPTR (cmds[2]));
		else
			dc_DefPlatform (UPTR (cmds[1]), /*superclass*/NULL);
		break;

	   case DK_SUBPLATFORM:
	   	dc_SubPlatform (cmds + 1);
		break;
	/*
	 * Class definition.
	 */
	   case DK_CLASS:
		if (cmds[2].uc_ctype != UTT_END)
		   dc_DefPlatformClass (UPTR (cmds[1]), UPTR (cmds[2]), FALSE);
		else
		   dc_DefPlatformClass (UPTR (cmds[1]), /*super*/NULL, FALSE);
		break;
	/*
	 * Instance definitions
	 */
	   case DK_INSTANCE:
		dc_DefInstances (UPTR(cmds[1]), cmds + 2);
		break;
	/*
	 * Subplats additions to either classes or instances
	 */
	   case DK_SUBPLATS:
		dc_DefSubPlats ( UPTR(cmds[1]), UPTR(cmds[2]), cmds+3);
		break;
	/*
	 * Begin with reading our command-line init files
	 */
	   case DK_START:
		StartInit();
		break;
	/*
	 * Configuration done -- go operational.
	 */
	   case DK_DONE:
		if (Debug)
		{
			dbg_DumpStatus ();
		}
		if (ParseOnly)
		{
			printf ("Tables after reading init file:\n");
			dbg_DumpTables ();
			msg_ELog(EF_INFO, "finished init file; shutting down");
			Shutdown();
		}
	   	else if (ndone++)
			msg_ELog (EF_PROBLEM, "Repeated DONE command");
		else
			FinishInit ();
		break;
	/*
	 * Regularly-scheduled commands.
	 */
	   case DK_EVERY:
	   	SetUpEvery (cmds + 1);
		break;
	/*
	 * Time-based cleanup.
	 */
	   case DK_TRUNCATE:
	   	Truncate (cmds + 1);
		break;
	/*
	 * See if we're supposed to monitor a UDP port.
	 */
	   case DK_RECEIVE:
	   	msg_BCSetup (0, UINT (cmds[1]), BCHandler);
		break;
	/*
	 * Maybe we're suppose to create broadcasts.
	 */
	   case DK_BROADCAST:
	   	BCSetup (UPTR (cmds[1]), UINT (cmds[2]));
		break;
	/*
	 * Write out cache files.
	 */
	   case DK_CACHE:
		/* dirty option overrides unified cache file name */
		if (cmds[1].uc_ctype == UTT_END)
			WriteCache (NULL, FALSE);
		else if (cmds[1].uc_ctype == UTT_KW)
			WriteCache (NULL, TRUE);
		else
			WriteCache (UPTR (cmds[1]), FALSE);
		break;
	/*
	 * Force a rescan
	 */
	   case DK_RESCAN:
		DoRescan (cmds + 1);
		break;
	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown Daemon kw: %d", UKEY (*cmds));
		break;
	}
	return (TRUE);
}




static void
BCSetup (addr, port)
char *addr;
int port;
/*
 * Get set up to broadcast our changes to the world.
 */
{
	int a, b, c, d;
	int ipaddr;
/*
 * Figure out params.
 */
	if (sscanf (addr, "%d.%d.%d.%d", &a, &b, &c, &d) != 4)
	{
		msg_ELog (EF_EMERGENCY, "Bad broadcast addr '%s'", addr);
		exit (1);
	}
	ipaddr = d + (c << 8) + (b << 16) + (a << 24);
/*
 * Set things up.
 */
	if ((BCastSocket = msg_BCSetup (ipaddr, port, BCHandler)) < 0)
		msg_ELog (EF_PROBLEM, "Bc setup fail on addr %s port %d",
				addr, port);
}




static int
BCHandler (s, data, len)
int s, len;
char *data;
/*
 * Deal with an incoming broadcast packet.
 */
{
	struct dsp_Template *tmpl;
/*
 * If we are broadcasting ourselves, we assume this is our packet
 * coming back, and we ignore it.
 */
	if (BCastSocket > 0)
		return (TRUE);
/*
 * Look at what we got.
 */
	tmpl = (struct dsp_Template *) data;
	switch (tmpl->dsp_type)
	{
	/*
	 * Some remote data has gone away.
	 */
	   case dpt_BCDataGone:
	   	RemDataGone ((struct dsp_BCDataGone *) tmpl);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown BCast DPT %d", tmpl->dsp_type);
		break;
	}
	return (TRUE);
}





int
Shutdown ()
/*
 * Shut things down.
 */
{
/*
 * Clean up in UI land.
 */
	ui_finish ();
	msg_ELog (EF_INFO, "shutdown: disconnecting, exiting.");
	msg_disconnect ();
	exit (0);
	return (0);	/* keep compilers happy */
}



static void
FinalCache ()
/*
 * If there's a unified cache file set, write
 * that instead of individual cache files.
 */
{
	int type;
	union usy_value v;
	char *file = NULL;

	if (usy_g_symbol (usy_g_stbl ("ui$variable_table"), "cachefile",
			  &type, &v))
	{
		file = v.us_v_ptr;
	}
	msg_ELog (EF_DEBUG, "CacheOnExit: writing cache %s",
		  file ? file : "for each platform");
	WriteCache (file, /*onlydirty*/ FALSE);
}



static void
mh_message (msg)
struct message *msg;
/*
 * Deal with a message from the msg subsystem itself.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
	struct mh_client *client;

	switch (tm->mh_type)
	{
	   case MH_SHUTDOWN:
	/*
	 * Don't write cache files if we haven't finished scanning
	 */
		if (! InitialScan && CacheOnExit)
		{
			FinalCache ();
		}
		printf ("%s: normal shutdown\n", msg_myname() );
		Shutdown ();
		/* no return */
	/*
	 * For client events, we are really only interested in deaths, so
	 * that we can get rid of any notification requests.
	 */
	   case MH_CLIENT:
		client = (struct mh_client *) msg->m_data;
		if (client->mh_evtype == MH_CE_DISCONNECT)
		{
			dap_Disconnect (client->mh_client);
			CleanClientLocks (client->mh_client);
		}
		break;

	   default:
	   	ui_printf ("Unknown MESSAGE proto msg %d\n", tm->mh_type);
		break;
	}
}




static void
ds_message (from, dt, len)
char *from;
struct dsp_Template *dt;
int len;
/*
 * Deal with an incoming data store protocol message.
 */
{
	struct dsp_MarkArchived *dma;
	struct dsp_ProtoVersion dpv;
	struct dsp_Instance *im;

	switch (dt->dsp_type)
	{
	/*
	 * They want a new data file entry.
	 */
	   case dpt_NewFileRequest:
	   	dp_NewFile (from, (struct dsp_CreateFile *) dt);
		break;
	/*
	 * They want to abort that new file entry.
	 */
	   case dpt_AbortNewFile:
	   	dp_AbortNewFile ((struct dsp_AbortNewFile *) dt);
		break;
	/*
	 * File updating -- they've added data.
	 */
	   case dpt_UpdateFile:
	   	dp_UpdateFile (from, (struct dsp_UpdateFile *) dt);
		break;
	/*
	 * DeleteData -- get rid of data before given time.
	 */
	   case dpt_DeleteData:
	   	dp_DeleteData (((struct dsp_DeleteData *) dt)->dsp_plat,
				&((struct dsp_DeleteData *) dt)->dsp_when);
		break;
	/*
	 * DeleteObs -- get rid of observation at given time.
	 */
	   case dpt_DeleteObs:
		dp_DeleteObs (((struct dsp_DeleteData *) dt)->dsp_plat,
			      &((struct dsp_DeleteData *) dt)->dsp_when);
		break;
	/*
	 * Application notification details.
	 */
	   case dpt_NotifyRequest:
	   	dap_Request (from, (struct dsp_NotifyRequest *) dt);
		break;

	   case dpt_CancelNotify:
	   	dap_Cancel (from);
		break;

	   case dpt_CopyNotifyReq:
	   	dap_Copy (from);
		break;
	/*
	 * A file is archived.
	 */
	   case dpt_MarkArchived:
	   	dma = (struct dsp_MarkArchived *) dt;
		if (dma->dsp_FileIndex > 0)
			DFTable[dma->dsp_FileIndex].df_flags |= DFF_Archived;
		break;
	/*
	 * Do a rescan.
	 */
	   case dpt_Rescan:
	   	Rescan (((struct dsp_Rescan *)dt)->dsp_pid,
			((struct dsp_Rescan *)dt)->dsp_all);
		break;
	/*
	 * Platform info.
	 */
	   case dpt_GetNPlat:
	   	SendNPlat (from);
		break;
	   case dpt_GetPlatStruct:
	   	SendPlatStruct (from, ((struct dsp_GetPlatStruct *) dt));
		break;
	   case dpt_GetFileStruct:
	   	SendFileStruct (from,
			((struct dsp_GetFileStruct *) dt)->dsp_index);
		break;
	   case dpt_PlatformSearch:
		SendPlatformList (from, (struct dsp_PlatformSearch *)dt);
		break;
	/*
	 * Locking.
	 */
	   case dpt_PLock:
	   	LockPlatform (from, ((struct dsp_PLock *) dt)->dsp_pid);
		break;
	   case dpt_ReleasePLock:
	   	UnlockPlatform (from, ((struct dsp_PLock *) dt)->dsp_pid, 1);
		break;
	   case dpt_WriteLock:
	   	WriteLock (from, ((struct dsp_PLock *) dt)->dsp_pid);
		break;
	   case dpt_ReleaseWLock:
	   	ReleaseWLock (from, ((struct dsp_PLock *) dt)->dsp_pid, 1);
		break;
	/*
	 * Platform lookup.
	 */
	   case dpt_LookupPlatform:
	   	DoLookup (from, ((struct dsp_PLookup *) dt)->dsp_name);
		break;
	/*
	 * Class lookup.
	 */
	   case dpt_LookupClass:
	   	DoClassLookup (from, ((struct dsp_PLookup *) dt)->dsp_name);
		break;
	   case dpt_GetClassStruct:
	   	SendClassStruct (from, ((struct dsp_GetPlatStruct *) dt));
		break;
	/*
	 * Find a data file based on time.
	 */
	   case dpt_FindDF:
	   	FindDF (from, (struct dsp_FindDF *) dt);
		break;
	   case dpt_FindAfter:
	   	FindAfter (from, (struct dsp_FindDF *) dt);
		break;
	/*
	 * Greeting.
	 */
	   case dpt_Hello:
	   	dpv.dsp_type = dpt_R_ProtoVersion;
		dpv.dsp_version = DSProtocolVersion;
		msg_send (from, MT_DATASTORE, FALSE, &dpv, sizeof (dpv));
		break;
	/*
	 * Definitions and instances
	 */
	   case dpt_DefineClass:
		ClientDefine (from, (struct dsp_ClassStruct *) dt, len);
		break;
#ifdef notdef
	   case dpt_AddSubplat:
		asp = (struct dsp_AddSubplat *)dt;
		ClientAddSubplat (asp->dsp_class, &asp->dsp_subplat);
		break;
#endif
	   case dpt_Instantiate:
		im = (struct dsp_Instance *) dt;
		ClientInstance (from, im->dsp_class, im->dsp_name, 
				im->dsp_parent);
		break;
	/*
	 * Chaos.
	 */
	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown DSP request: %d", dt->dsp_type);
		break;
	}
}



static void
dp_NewFile (from, request)
char *from;
struct dsp_CreateFile *request;
/*
 * Deal with a new file request.
 */
{
	struct dsp_R_CreateFile response;
	DataFile *new = NULL;
	Platform *pi = PTable + request->dsp_plat;
/*
 * Get a new file entry to give back to them.  Make sure there isn't already
 * a tempfile sitting there from some other time -- if so we reuse it and
 * gripe.
 */
	if (pi->dp_Tfile)
	{
		msg_ELog (EF_PROBLEM, "Plat %s reusing tfile!", pi->dp_name);
		new = DFTable + pi->dp_Tfile;
	}
/*
 * Make sure the platform directory exists, and if not try to create it.
 * If we can't create it then we must abort the new file.
 */
	else if (! pi_DirExists (pi) && 
		 ! dt_CreateDataDir (pi_Dir(pi), pi_Name(pi), &pi->dp_flags))
	{
		msg_ELog (EF_PROBLEM, "Cannot create %s datadir for new file",
			  pi->dp_name);
	}
	else
		new = dt_NewFile ();

	if (! new)
	{
		response.dsp_type = dpt_R_NewFileFailure;
		msg_send (from, MT_DATASTORE, FALSE, &response,
			  sizeof (response));
		return;
	}
/*
 * Fill in the info and hook it into the platform.
 */
	ClearLocks (pi);
	dt_SetString (new->df_name, request->dsp_file, sizeof(new->df_name),
		      "request for new file");
	new->df_begin = new->df_end = request->dsp_time;
	new->df_rev = 0;
	new->df_FLink = new->df_BLink = new->df_nsample = 0;
	new->df_platform = request->dsp_plat;
	new->df_ftype = pi_FileType (pi);
	pi->dp_Tfile = new - DFTable;
/*
 * Respond back to the requester and quit.
 */
	response.dsp_type = dpt_R_NewFileSuccess;
	response.dsp_FileIndex = new - DFTable;
	msg_send (from, MT_DATASTORE, FALSE, &response, sizeof (response));
}





static void
dp_AbortNewFile (request)
struct dsp_AbortNewFile *request;
/*
 * They want to abort a new file request.
 */
{
	Platform *plat = PTable + request->dsp_pid;

	if (! plat->dp_Tfile)
		msg_ELog (EF_PROBLEM, "Abort NF on plat %s, but no tfile!",
			plat->dp_name);
	else
	{
		dt_FreeDFE (DFTable + plat->dp_Tfile);
		plat->dp_Tfile = 0;
	}
}






static void
dp_UpdateFile (from, request)
char *from;
struct dsp_UpdateFile *request;
/*
 * Deal with a file update notification.
 */
{
	DataFile *df = DFTable + request->dsp_FileIndex;
	Platform *plat = PTable + df->df_platform;
	int recent;
	int append = FALSE;
	int newfile = FALSE;
	struct dsp_FileStruct ack;
/*
 * Mark the changes in the datafile entry.  Update the rev count so that
 * reader processes know things have changed.
 */
	msg_ELog (EF_DEBUG,
		  "%s updated %s file %d (%s) ns %d ow %d last %d",
		  from, request->dsp_Local ? "local" : "remote",
		  request->dsp_FileIndex, plat->dp_name, 
		  request->dsp_NSamples, request->dsp_NOverwrite, 
		  request->dsp_Last);
	ClearLocks (plat);
/*
 * Since temp files are initialized to the same begin and end times, test
 * whether the file had samples to begin with also when detecting appends.
 */
	if ((df->df_nsample == 0) || TC_Less(df->df_end, request->dsp_EndTime))
	{
		df->df_end = request->dsp_EndTime;
		recent = (request->dsp_Local) ? 
			pi_LocalData(plat) : pi_RemoteData(plat);
		if (request->dsp_FileIndex == recent ||
		    request->dsp_FileIndex == plat->dp_Tfile)
		    	append = TRUE;
	}
/*
 * Get a new revision for the file.
 */
	df->df_rev = NewRevision (plat, df);
	df->df_nsample += request->dsp_NSamples;
	plat->dp_NewSamps += request->dsp_NSamples;
	plat->dp_OwSamps += request->dsp_NOverwrite;
	plat->dp_flags |= DPF_DIRTY;
/*
 * If this file is in the Tfile slot, now we move it over to the localdata
 * list.  The dt_AddToPlatform() may cause some CacheInvalidate messages.
 * We want these messages to be sent before the R_UpdateAck below, so that
 * the client will process them while waiting for the R_UpdateAck response
 * to the file update notification.
 */
	if (request->dsp_FileIndex == plat->dp_Tfile)
	{
		plat->dp_Tfile = 0;
		dt_AddToPlatform (plat, df, (zbool)request->dsp_Local);
		newfile = TRUE;
	}
/*
 * Where last=TRUE, the client ends up receiving the DFE twice.  Send the
 * CacheInvalidate first so that the client sending this FileUpdate will
 * process it before its UpdateAck message.  The other clients won't be
 * affected by receiving the invalidate here rather than after sending
 * the UpdateAck to the client with the write lock.
 *
 * And of course, if this is a new file its kind of pointless to invalidate
 * its cached entries.
 */
	if (request->dsp_Last && !newfile)
	{
		CacheInvalidate (df - DFTable);
	}
/*
 * Send an ack back to the updating process.
 */
	ack.dsp_type = dpt_R_UpdateAck;
	ack.dsp_file = *df;
	msg_send (from, MT_DATASTORE, FALSE, &ack, sizeof (ack));
/*
 * Now we do data available notifications.
 */
	if (request->dsp_Last)
	{
		dap_Notify (df->df_platform, &df->df_end, plat->dp_NewSamps,
			    plat->dp_OwSamps, append);
		plat->dp_NewSamps = plat->dp_OwSamps = 0;
	}
}





static void
dp_DeleteData (pid, t)
PlatformId pid;
ZebTime *t;
/*
 * Handle the delete data request.
 */
{
	int index;
	Platform *p;

	if (pid < 0 || pid >= NPlatform)
	{
		msg_ELog (EF_PROBLEM, "DeleteData on bad platform id");
		return;
	}
	p = PTable + pid;
	index = pi_LocalData (p);
/*
 * If there is no data at all, life is easy.
 */
	if (! index)
	{
		msg_ELog (EF_DEBUG, "DeleteData on %s -- empty", p->dp_name);
		return;
	}
/*
 * Sanity check.
 */
	if (pi_Subplatform(p))
	{
		msg_ELog (EF_PROBLEM, "Attempted DeleteData on subplat %s",
			p->dp_name);
		return;
	}
/*
 * Now we go through and remove every file which ends before this time.
 */
	ClearLocks (p);
	while (index)
	{
		DataFile *df;
	/*
	 * Remove this file if it ends at or before the specified time.
	 */
		df = DFTable + index;
	        index = df->df_FLink;
	 	if (TC_LessEq (df->df_end, *t))
		{
		/*
		 * OK, this one goes.
		 */
		        msg_ELog (EF_DEBUG, "DeleteData: zap %d (%s)", 
				  df->df_index, df->df_name);
		        ZapDF (df);
		        dt_RemoveDFE (p, df - DFTable);
		        p->dp_flags |= DPF_DIRTY;
	        }
	}
}




static void
dp_DeleteObs (pid, t)
PlatformId pid;
ZebTime *t;
/*
 * Handle the request to delete a single, whole observation.
 */
{
	int index;
	Platform *p;
	DataFile *df = NULL;

	if (pid < 0 || pid >= NPlatform)
	{
		msg_ELog (EF_PROBLEM, "DeleteObs on bad platform id");
		return;
	}
	p = PTable + pid;
	index = pi_LocalData (p);
/*
 * If there is no data at all, life is easy.
 */
	if (! index)
	{
		msg_ELog (EF_DEBUG, "DeleteObs on %s -- empty", p->dp_name);
		return;
	}
/*
 * Sanity check.
 */
	if (pi_Subplatform(p))
	{
		msg_ELog (EF_PROBLEM, "Attempted DeleteObs on subplat %s",
			p->dp_name);
		return;
	}
/*
 * Now we go through and find the file which contains the given time.
 */
	ClearLocks (p);
	while (index)
	{
		df = DFTable + index;
	/*
	 * If this file doesn't contain the specified time, move on.
	 */
		if (TC_Less (*t, df->df_begin))
		{
			index = df->df_FLink;
		}
		else if (TC_Less (df->df_end, *t))
		{
		/*
		 * The rest of the times will be earlier, so we won't
		 * find the observation at all.  Let such be known and abort.
		 */
			msg_ELog (EF_PROBLEM,
				  "DeleteObs: no observation at that time");
			return;
		}
		else
			break;
	}

	if (! index)
	{
	/*
	 * All of the files were newer than the desired time 
	 */
		msg_ELog (EF_PROBLEM, "DeleteObs: no observations that old");
		return;
	}
/*
 * OK, this one goes.  Send notices and unlink the file in ZapDF().  
 * Remove the DFE from the table and free it in dt_RemoveDFE().
 */
 	msg_ELog (EF_DEBUG, "DeleteObs: zap %d (%s)", 
		  (df - DFTable), df->df_name);
	ZapDF (df);
	dt_RemoveDFE (p, df - DFTable);
	p->dp_flags |= DPF_DIRTY;
}




static void
RemDataGone (dg)
struct dsp_BCDataGone *dg;
/*
 * A remotely accessible data file has gone away.
 */
{
	Platform *plat;
	int dfindex;
/*
 * Make sure this is a platform we know about.
 */
	if ((plat = dt_FindPlatform (dg->dsp_Plat)) == 0)
		return;
/*
 * Construct the file name from our perspective.
 */
	for (dfindex = pi_RemoteData (plat); dfindex;
			dfindex = DFTable[dfindex].df_FLink)
	{
		if (! strcmp (dg->dsp_File, DFTable[dfindex].df_name))
			break;
	}
	if (! dfindex)
		return;
/*
 * Now we unlink the file and remove the DFE.
 */
	ZapDF (DFTable + dfindex);
	dt_RemoveDFE (plat, dfindex);
}



void
DataFileGone (df)
DataFile *df;
/*
 * Notify clients that this DataFile has gone the way of the dinosaurs.
 */
{
	struct dsp_DataGone dg;
/*
 * If no one could have possibly heard of this file yet, then no
 * one wants to know that it's not there.
 */
	if (InitialScan)
		return;
/*
 * Broadcast a notification to the world.
 */
	dg.dsp_type = dpt_DataGone;
	dg.dsp_file = df - DFTable;
	msg_send ("DataStore", MT_DATASTORE, TRUE, &dg, sizeof (dg));
/*
 * If we are broadcasting to other worlds, we do that too.
 */
	if (BCastSocket > 0)
	{
		struct dsp_BCDataGone dg;
		dg.dsp_type = dpt_BCDataGone;
		strcpy (dg.dsp_Plat, PTable[df->df_platform].dp_name);
		strcpy (dg.dsp_File, df->df_name);
		msg_BCast (BCastSocket, &dg, sizeof (dg));
	}
/*
 * Make sure we have the file closed ourselves.  This may be pointless (for
 * the moment) since the daemon never keeps any files open.
 */
	dfa_ForceClose (df - DFTable);
}




static void
ZapDF (df)
DataFile *df;
/*
 * Make this file go away.
 */
{
  char **filenames;
  int  nfiles, i;

/*
 * Let everyone know this file is going away.
 */
        DataFileGone (df);
 
/*
 * Unlink the file from in the filesystem.
 */
	if (! (df->df_flags & DFF_Remote))
	{
           filenames=dfa_GetAssociatedFiles (df->df_index, &nfiles );
           for (i=0; i<nfiles; i++)
	   {

/* Maybe is better to show this message than in the caller function
		msg_ELog (EF_DEBUG, "File is : %s", filenames[i]);
*/

	     if (unlink (filenames[i] ) < 0)
	        msg_ELog (EF_PROBLEM, "Error %d unlinking %s", 
                           errno, filenames[i]);
             if (filenames[i])  free ( filenames[i]);
	   }
           if ( filenames ) free (filenames);
        }
}




static void
DoRescan (cmds)
struct ui_command *cmds;
{
	zbool all = FALSE;
	PlatformId platid = BadPlatform;
	Platform *plat;

	all = (cmds[0].uc_ctype == UTT_END) || (cmds[0].uc_ctype == UTT_KW);
	if (! all)
	{
		plat = dt_FindPlatform (UPTR (cmds[0]));
		if (plat)
			platid = plat - PTable;
		else
			msg_ELog (EF_PROBLEM, "rescan: no such platform '%s'",
				  cmds[0].uc_text);
	}
	if ((all) || (platid != BadPlatform))
		Rescan (platid, all);
}




static void
SetUpEvery (cmds)
struct ui_command *cmds;
/*
 * Set up an EVERY command.
 */
{
	int interval = UINT (cmds[0])*INCFRAC;
/*
 * Get the timer to let us know when it's time.  Then remember the command
 * to execute when things happen.
 */
	tl_RelativeReq (ExecEvery, (void *)(long) NEvery, interval, interval);
	ECmds[NEvery++] = usy_string (UPTR (cmds[1]));
}
	



static void
ExecEvery (t, slot)
ZebTime *t;
int slot;
/*
 * Run this EVERY command.
 */
{
	msg_ELog (EF_DEBUG, "EVERY timeout: '%s'", ECmds[slot]);
	ui_perform (ECmds[slot]);
}




static void
Truncate (cmds)
struct ui_command *cmds;
/*
 * Handle the TRUNCATE command.
 */
{
	int seconds, plat;
	Platform *p;
	ZebTime now, zaptime;
/*
 * Figure out how much data remains.
 */
	seconds = (cmds->uc_ctype == UTT_KW) ? -1 : UINT (*cmds);
/*
 * If they gave a specific platform, do it now.
 */
	tl_Time (&now);
	if (cmds[1].uc_ctype != UTT_KW)
	{
		if (! (p = dt_FindPlatform (UPTR (cmds[1]))))
			msg_ELog (EF_PROBLEM, "TRUNCATE on bad platform %s",
				UPTR (cmds[1]));
		else
		{
			zaptime = now;
			zaptime.zt_Sec -= (seconds > 0) ? seconds : pi_Keep(p);
			dp_DeleteData (p - PTable, &zaptime);
		}
		return;
	}
/*
 * Otherwise we go through the list and do everything.
 */
	for (plat = 0; plat < NPlatform; plat++)
		if (! (PTable[plat].dp_flags & DPF_SUBPLATFORM))
		{
			zaptime = now;
			zaptime.zt_Sec -= (seconds > 0) ? seconds :
				pi_Keep(PTable+plat);
			dp_DeleteData (plat, &zaptime);
		}
}




static int
FreeSpace (narg, argv, argt, rv, rt)
int narg, *argt, *rt;
union usy_value *argv, *rv;
/*
 * [Command line function] return the amount of the disk that is free.
 */
{
#if !defined(SVR4) && !defined(__osf__)
	struct statfs sfs;
#else
	struct statvfs sfs;
#endif
/*
 * Stat the file system.
 */
#if !defined(SVR4) && !defined(__osf__)
	if (statfs (argv->us_v_ptr, &sfs))
		msg_ELog (EF_PROBLEM, "Statfs failed, errno %d", errno);
#else
	if (statvfs (argv->us_v_ptr, &sfs))
		msg_ELog (EF_PROBLEM, "Statfs failed, errno %d", errno);
#endif
/*
 * Calculate the return value.
 */
	rv->us_v_int = sfs.f_bavail;
	*rt = SYMT_INT;
	return (0);
}




static void
SendNPlat (to)
char *to;
/*
 * Tell this person how many platforms we have.
 */
{
	struct dsp_NPlat np;

	np.dsp_type = dpt_R_NPlat;
	np.dsp_nplat = NPlatform;
	msg_send (to, MT_DATASTORE, FALSE, &np, sizeof (np));
}





static void
SendPlatStruct (to, req)
char *to;
struct dsp_GetPlatStruct *req;
/*
 * Answer this platform structure request.
 */
{
	struct dsp_PlatStruct answer;

	answer.dsp_type = dpt_R_PlatStruct;
	answer.dsp_pid = req->dsp_pid;
	if (dt_CheckId (req->dsp_pid))
	{
		dt_ClientPlatform (PTable + req->dsp_pid, &answer.dsp_plat);
		answer.dsp_result = 1;
	}
	else
	{
		answer.dsp_result = 0;
	}
	msg_send (to, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}




static void
SendClassStruct (to, req)
char *to;
struct dsp_GetPlatStruct *req;
/*
 * Answer this platform class structure request.
 */
{
	struct dsp_ClassStruct *answer, am;
	int len = sizeof (struct dsp_ClassStruct);
	
	answer = &am;
	answer->dsp_result = 0;
	if (dt_CheckClassId (req->dsp_pid))
	{
		PlatformClass *pc = CTable + req->dsp_pid;

		answer = dt_InjectClass (pc, &am, &len);
		answer->dsp_result = 1;
	}
	answer->dsp_type = dpt_R_ClassStruct;
	answer->dsp_cid = req->dsp_pid;
	msg_send (to, MT_DATASTORE, FALSE, answer, len);
	if (answer != &am)
		free (answer);
}




static void
SendPlatformList (to, req)
char *to;
struct dsp_PlatformSearch *req;
/*
 * Search the platforms and return the IDs of those that match.
 */
{
	struct dsp_PlatformList *answer;
	int npids;
	int len;
/*
 * Create space for all possible matches, but we'll send only as many as we
 * fill.
 */
	len = sizeof(struct dsp_PlatformList);
	len += (NPlatform <= 1) ? (0) : 
		(sizeof(PlatformId) * (NPlatform - 1)); 
	answer = (struct dsp_PlatformList *) malloc (len);
	answer->dsp_type = dpt_R_PlatformSearch;

	dt_SearchPlatforms (MatchPlatform, req, answer->dsp_pids, &npids);
	answer->dsp_npids = npids;
/*
 * Send the structures back if requested
 */
	if (req->dsp_sendplats)
	{
		struct dsp_PlatStructSearch send;
		int i;

		for (i = 0; i < npids; ++i)
		{
			Platform *plat = PTable + answer->dsp_pids[i];
			send.dsp_type = dpt_R_PlatStructSearch;
			dt_ClientPlatform (plat, &send.dsp_plat);
			send.dsp_pid = answer->dsp_pids[i];
			msg_send (to, MT_DATASTORE, FALSE, 
				  &send, sizeof (send));
		}
	}
/*
 * The response has been filled or left empty.  Send it off.
 */
	len = sizeof (struct dsp_PlatformList);
	len += (npids <= 1) ? (0) : (npids - 1) * sizeof(PlatformId);
	msg_send (to, MT_DATASTORE, FALSE, answer, len);
	free (answer);
}




static int
MatchPlatform (symbol, type, value, info)
char *symbol;
int type;
union usy_value *value;
struct SearchInfo *info;
/*
 * Use the request structure to see if there is a match with this
 * platform.
 */
{
	Platform *plat = (Platform *) value->us_v_ptr;
	struct dsp_PlatformSearch *req = info->si_req;
/*
 * The platform name table contains lots of abbreviated symbols which are
 * not the fully qualified hierarchical name, but the full hierarchical
 * name is the only symbol guaranteed to be unique.  So, categorically skip
 * all those symbols whose lengths do not equal the length of the fully
 * qualified name.
 */
	if (strlen(plat->dp_name) != strlen(symbol))
		return (TRUE);
/*
 * Ignore subplatforms if so requested
 */
	if (!req->dsp_subplats && pi_Subplatform(plat))
		return (TRUE);
/*
 * If we're looking for subplatforms of a particular parent, ignore
 * all other platforms.  We don't care whether the platform has the
 * DPF_SUBPLATFORM flag set, we just care about who the parent is.
 */
	if ((req->dsp_children) && (plat->dp_parent != req->dsp_parent))
		return (TRUE);
/*
 * The last remaining check is the regular expression, if there is one
 */
	if ((req->dsp_regexp[0] == '\0') || (zl_re_exec (plat->dp_name)))
	{
		info->si_pids[(info->si_npids)++] = (plat - PTable);
	}
/*
 * Done testing this platform
 */
	return (TRUE);
}




static void
SendFileStruct (to, index)
char *to;
int index;
/*
 * Send off this file structure.
 */
{
	struct dsp_FileStruct answer;

	answer.dsp_type = dpt_R_FileStruct;
	answer.dsp_file = DFTable[index];
	msg_send (to, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}





static Lock *
GetLockEntry ()
/*
 * Get a free lock entry.
 */
{
	if (FreeLocks)
	{
		Lock *ret = FreeLocks;
		FreeLocks = FreeLocks->l_Next;
		return (ret);
	}
	return ((Lock *) malloc (sizeof (Lock)));
}





static void
LockPlatform (who, which)
char *who;
PlatformId which;
/*
 * Lock platform WHICH for WHO.
 */
{
	Lock *lp;
	Platform *p;
	struct dsp_PLock answer;
/*
 * Do some testing for a case we are not prepared to handle.  Multiple locks
 * will work *most* of the time, so long as they unlock the same number of
 * times.  But if the daemon is waiting for an unlock on this platform, 
 * things will hang.
 */
	if (! dt_CheckId (which))
	{
		msg_ELog (EF_PROBLEM, 
			  "Read lock attempt for invalid plat id %d", which);
		return;
	}
	p = PTable + which;
	++ReadLockRequests;
	msg_ELog (EF_DEBUG, "Read lock on %s by %s", p->dp_name, who);
	for (lp = p->dp_RLockQ; lp; lp = lp->l_Next)
		if (! strcmp (who, lp->l_Owner))
		{
			msg_ELog (EF_PROBLEM, "Multiple lock on %s by %s",
				p->dp_name, who);
			break;
		}
/*
 * Fill in the lock structure.  Avoid the overhead for the time field, at
 * least until experience shows that we need it.
 */
	lp = GetLockEntry ();
	strcpy (lp->l_Owner, who);
	/* tl_Time (lp->l_When); */
	lp->l_Next = p->dp_RLockQ;
	p->dp_RLockQ = lp;
/*
 * Send an answer back to the process telling them to proceed.
 */
	answer.dsp_type = dpt_R_PLockGranted;
	answer.dsp_pid = which;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}




static void
UnlockPlatform (who, which, expect)
char *who;
PlatformId which;
int expect;
/*
 * Release a lock on this platform.  EXPECT is non-zero if we're expecting to
 * find and remove one lock for this client.  If EXPECT is zero, we remove
 * any and all locks owned by this client.
 */
{
	Lock *lp, *zap;
	Platform *p;

	if (! dt_CheckId (which))
	{
		msg_ELog (EF_PROBLEM, 
			  "Read lock release for invalid plat id %d", which);
		return;
	}
/*
 * Find the first lock (of possibly many) held by this process and remove it.
 */
	p = PTable + which;
	if ((zap = p->dp_RLockQ) && ! strcmp (zap->l_Owner, who))
		p->dp_RLockQ = zap->l_Next;
	else
	{
		for (lp = p->dp_RLockQ; lp && lp->l_Next; lp = lp->l_Next)
			if (! strcmp (lp->l_Next->l_Owner, who))
				break;
		if (! lp || ! lp->l_Next)
		{
			if (expect)
				msg_ELog (EF_PROBLEM,
					"Lock for %s on %s missing", who,
					p->dp_name);
			return;
		}
		zap = lp->l_Next;
		lp->l_Next = zap->l_Next;
	}
	msg_ELog (EF_DEBUG, "Lock on %s released by %s", p->dp_name, who);
/*
 * Put the zapped entry onto the free list.
 */
	zap->l_Next = FreeLocks;
	FreeLocks = zap;
/*
 * If necessary, remove the rest of this client's locks.
 */
	if (! expect)
		CleanQueue (&(p->dp_RLockQ), who);
}




void
ClearLocks (p)
Platform *p;
/* 
 * Wait until all read locks on this platform are clear.
 */
{
	while (p->dp_RLockQ)
		msg_Search (MT_DATASTORE, AwaitUnlock, 0);
}





static int
AwaitUnlock (msg, junk)
Message *msg;
int junk;
/*
 * Process another message in the hope that it will unlock something.
 */
{
	struct dsp_Template *dt = (struct dsp_Template *) msg->m_data;
	int ret = MSG_CONSUMED;
/*
 * Essentially we pass through all of the data store protocol messages
 * that we can safely service without threat of deadlock, and drop out 
 * whenever we hit an unlock.
 */
	switch (dt->dsp_type)
	{
	   case dpt_ReleasePLock:
	   	ret = MSG_DONE;
	   case dpt_NotifyRequest:
	   case dpt_CancelNotify:
	   case dpt_CopyNotifyReq:
	   case dpt_MarkArchived:
	   case dpt_GetNPlat:
	   case dpt_GetPlatStruct:
	   case dpt_GetFileStruct:
	   case dpt_PlatformSearch:
	   case dpt_FindDF:
	   case dpt_FindAfter:
	   case dpt_Hello:
	   case dpt_LookupPlatform:
	   case dpt_LookupClass:
#ifdef notdef
	/* These are not allowed because they might cause the platform and
	 * class tables to move, and the calling routine likely has pointers
	 * to platform and class entries.
	 */
	   case dpt_DefineClass:
	   case dpt_AddSubplat:
	   case dpt_Instantiate:
#endif
	   	ds_message (msg->m_from, dt, msg->m_len);
		return (ret);

	   default:
	   	return (MSG_ENQUEUE);
	}
}





static void
DoLookup (who, plat)
char *who, *plat;
/*
 * Look up this platform for somebody.
 */
{
	Platform *p = dt_FindPlatform (plat);
	struct dsp_PID answer;

	answer.dsp_type = dpt_R_PID;
	answer.dsp_pid = p ? (p - PTable) : BadPlatform;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}





static void
DoClassLookup (who, name)
char *who, *name;
/*
 * Look up this platform class for somebody.
 */
{
	PlatformClass *pc = dt_FindClass (name);
	struct dsp_PID answer;

	answer.dsp_type = dpt_R_CID;
	answer.dsp_pid = pc ? (pc - CTable) : BadClass;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}





static void
FindDF (who, req)
char *who;
struct dsp_FindDF *req;
/*
 * Find a data file entry based on time.  Find the closest of the files
 * containing or preceding the time among the specified sources.
 */
{
	int dfe, dfmatch;
	struct dsp_R_DFI answer;
/*
 * Search the platform's local list first.
 */
	dfmatch = 0;
	if (req->dsp_src <= 0)
	{
		for (dfe = pi_LocalData (PTable + req->dsp_pid); dfe;
		     dfe = DFTable[dfe].df_FLink)
		{
			if (TC_LessEq (DFTable[dfe].df_begin, req->dsp_when))
				break;
		}
		dfmatch = dfe;
	}
/*
 * If we didn't find a perfect match locally, see if there's anything in the
 * remote data table.
 */
	if ((! dfmatch || TC_Less (DFTable[dfmatch].df_end, req->dsp_when))
	    && (req->dsp_src == 1 || req->dsp_src == SRC_ALL))
	{
		for (dfe = pi_RemoteData (PTable + req->dsp_pid); dfe;
		     dfe = DFTable[dfe].df_FLink)
		{
			if (TC_LessEq (DFTable[dfe].df_begin, req->dsp_when))
			{
			/*
			 * If we have a local candidate, but this file ends
			 * later than the local one (it either contains the
			 * time or is closer to it), then use it.  If its
			 * end is less than the local, then it can't possibly
			 * contain the time and the local is closer.
			 */
				if ((! dfmatch) ||
				    TC_Less (DFTable[dfmatch].df_end, 
					     DFTable[dfe].df_end))
				{
					dfmatch = dfe;
				}
				break;
			}
		}
	}
/*
 * Now return our answer.
 */
	answer.dsp_type = dpt_R_DFIndex;
	answer.dsp_index = (dfmatch) ? dfmatch : -1;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}







static void
FindAfter (who, req)
char *who;
struct dsp_FindDF *req;
/*
 * Find the closest data file containing data after or including the
 * given time.  If a local file contains the time, use it.  Otherwise,
 * take the closest file, local or remote, after the time.
 */
{
	int dfe = pi_LocalData (PTable + req->dsp_pid);
	int last = 0, rlast = 0;
	struct dsp_R_DFI answer;
/*
 * Search the local list for the first DFE which *begins* before the given
 * time.
 */
	for (; dfe; dfe = DFTable[dfe].df_FLink)
	{
		if (TC_LessEq (DFTable[dfe].df_begin, req->dsp_when))
			break;
		last = dfe;
	}
/*
 * If we didn't find a local file containing the time, 
 * see if there's anything in the remote data table.
 */
	if (! dfe || TC_Less(DFTable[dfe].df_end, req->dsp_when))
	{
		for (dfe = pi_RemoteData (PTable + req->dsp_pid); dfe;
		     dfe = DFTable[dfe].df_FLink)
		{
			if (TC_LessEq (DFTable[dfe].df_begin, req->dsp_when))
				break;
			rlast = dfe;
		}
	/*
	 * If we still failed to find a file containing the time, choose
	 * the closest of the files backwards on the remote and local chains.
	 */
		if (! dfe || TC_Less(DFTable[dfe].df_end, req->dsp_when))
		{
			if (last && (rlast == 0 ||
				     TC_LessEq (DFTable[last].df_begin, 
						DFTable[rlast].df_begin)))
				dfe = last;
			else
				dfe = rlast;
		}
	}
/*
 * Return our answer.
 */
	answer.dsp_index = (dfe) ? dfe : -1 ;
	answer.dsp_type = dpt_R_DFIndex;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}





void
CacheInvalidate (index)
int index;
/*
 * Invalidate cache entries for this index.
 */
{
	struct dsp_CacheInvalidate msg;
/*
 * Nobody has a cache before the initial scan is done, so there is no
 * point in creating unbelievable amounts of traffic.
 */
	if (! InitialScan)
	{
		++InvalidatesSent;
		msg.dsp_type = dpt_CacheInvalidate;
		msg.dsp_dfe = DFTable[index];
		msg_send ("DataStore", MT_DATASTORE, TRUE, &msg, sizeof (msg));
	}
}





static void
WriteLock (who, which)
char *who;
PlatformId which;
/*
 * Write lock platform WHICH for WHO.
 */
{
	Lock *lp, *lq;
	Platform *p = PTable + which;
	struct dsp_PLock answer;
/*
 * Fill in the lock structure.  Avoid the overhead for the time field, at
 * least until experience shows that we need it.
 */
	lp = GetLockEntry ();
	lp->l_Next = NULL;
	strcpy (lp->l_Owner, who);
	++WriteLockRequests;
	/* tl_Time (lp->l_When); */
/*
 * Now...if there is no lock active we can add this one and send the
 * answer back.
 */
	if (! p->dp_WLockQ)
	{
		p->dp_WLockQ = lp;
		answer.dsp_type = dpt_R_PLockGranted;
		answer.dsp_pid = which;
		msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
		msg_ELog (EF_DEBUG, 
			  "Write lock on %s granted to %s",p->dp_name, who);
	}
	else
	{
		msg_ELog (EF_DEBUG, 
			  "%s waiting for write lock on %s", who, p->dp_name);
		lq = p->dp_WLockQ;
		while (lq->l_Next)
			lq = lq->l_Next;
		lq->l_Next = lp;
	}
}




static void
ReleaseWLock (who, which, expect)
char *who;
PlatformId which;
int expect;
/*
 * Release a lock on this platform.  EXPECT is non-zero if we're expecting
 * this client to release the lock which it currently holds.  If EXPECT is
 * zero, we remove all of this client's locks from the queue.
 */
{
	Lock *zap;
	Platform *p = PTable + which;
	zbool release = TRUE;
/*
 * Make sure there are locks to release.
 */
	if (! p->dp_WLockQ)
		return;
/*
 * Questions arise if this client does not own the current lock.
 */
	if (strcmp (p->dp_WLockQ->l_Owner, who))
	{
		if (expect)	
			/* can't release a lock this client does not own */
			return;
		else
			/* didn't expect to release the lock so we won't */
			release = FALSE;
	}
/*
 * Now do the right thing according to whether we're removing just this 
 * client's current lock or all of this client's pending locks also.
 */
	if (expect && release)
	{
		/*
		 * Pull the zapped entry off and put it on the free list.
		 */
		zap = p->dp_WLockQ;
		p->dp_WLockQ = zap->l_Next;
		zap->l_Next = FreeLocks;
		FreeLocks = zap;
	}
	else if (! expect)
	{
		/*
		 * If we're just cleaning dead clients, remove it completely
		 * from the queue, including the front of the queue if
		 * necessary.
		 */
		CleanQueue (&(p->dp_WLockQ), who);
	}
	if (release)
		msg_ELog (EF_DEBUG, "Write lock on %s released by %s",
			  p->dp_name, who);
/*
 * If we released the currently held lock, and there is somebody else
 * waiting for a lock, grant it now.
 */
 	if (release && p->dp_WLockQ)
	{
		struct dsp_PLock answer;
		answer.dsp_type = dpt_R_PLockGranted;
		answer.dsp_pid = which;
		msg_send (p->dp_WLockQ->l_Owner, MT_DATASTORE, FALSE,
				&answer, sizeof (answer));
		msg_ELog (EF_DEBUG, "Write lock on %s granted to %s",
			  p->dp_name, p->dp_WLockQ->l_Owner);
	}
}



static void
CleanClientLocks (who)
char *who;
/*
 * Release all locks for this client, disconnected and presumed dead.
 */
{
	int i;

	for (i = 0; i < NPlatform; i++)
	{
		if (PTable[i].dp_RLockQ)
			UnlockPlatform (who, i, 0);
		if (PTable[i].dp_WLockQ)
			ReleaseWLock (who, i, 0);
	}
}



static int
CleanQueue (q, who)
Lock **q;
char *who;
/*
 * Returns non-zero if a lock for this client was removed from the queue.
 */
{
	Lock *zap = NULL;
	Lock *lp;

	lp = *q;
	while (lp)
	{
		if (! strcmp (lp->l_Owner, who))
		{
		/*
		 * Pull this client's entry off and put it on the free list.
		 */
			zap = lp;
			lp = lp->l_Next;
			*q = lp;
			zap->l_Next = FreeLocks;
			FreeLocks = zap;
			msg_ELog (EF_DEBUG, "Lock removed for %s", who);
		}
		else
		{
			q = &(lp->l_Next);
			lp = lp->l_Next;
		}
	}
	return (zap != NULL);
}



static int
NewRevision (plat, df)
Platform *plat;
DataFile *df;
{
	int rev;

	if (StatRevisions)
	{
		rev = StatRevision (plat, df, &df->df_inode);
		/*
		 * Make sure we actually got a more recent revision number
		 */
		if (rev <= df->df_rev)
		{
#ifdef DEBUG
			msg_ELog (EF_DEBUG, "%s: warping %d to %d",
				  "outdated stat mtime", rev, df->df_rev+1);
#endif
			rev = df->df_rev + 1;
		}
	}
	else
	{
		rev = df->df_rev + 1;
	}
	return (rev);
}


/* -----
 * Handle and respond to client definitions and instances
 */

static void
ClientDefine (who, dsp, len)
char *who;
struct dsp_ClassStruct *dsp;
int len;
{
	struct dsp_PID answer;
	PlatformClass *pc;

	answer.dsp_type = dpt_R_PID;
	answer.dsp_pid = BadClass;
	/*
	 * Just get a clean entry in the table.  The superclass id, if any,
	 * will get set when we extract the class from the message.
	 * Likewise the client side should have already taken care of any
	 * derivations from a super class.
	 */
	pc = dt_NewClass (dsp->dsp_class.dpc_name, /*superclass*/NULL);
	if (pc)
	{
		PlatformClass *spc = NULL;

		dt_ExtractClass (pc, dsp, len);
		/*
		 * Set any necessary default directory paths.
		 */
		if ((pc->dpc_superclass != BadClass) && 
		    dt_CheckClassId (pc->dpc_superclass))
			spc = (CTable + pc->dpc_superclass);
		else
			pc->dpc_superclass = BadClass;
		dt_FillClassDirs (pc, spc);
		dt_ValidateClass (pc);
		answer.dsp_pid = pc - CTable;
	}
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}




static void
ClientInstance (who, cid, name, parent)
char *who;
PlatClassId cid;
char *name;
PlatformId parent;
{
	PlatformClass *pc;
	PlatformInstance *plat;
	struct dsp_PID answer;

	answer.dsp_type = dpt_R_PID;
	answer.dsp_pid = BadPlatform;
	if (dt_CheckClassId (cid))
	{
		pc = CTable + cid;
		plat = dt_Instantiate (pc, parent, name);
		answer.dsp_pid = plat - PTable;
	}
	else
	{
		msg_ELog (EF_PROBLEM, "instance msg: class %d not found", cid);
	}
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}

