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
# include <ui_error.h>
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

RCSID ("$Id: Daemon.c,v 3.73 2001-10-11 16:39:37 burghart Exp $")

/*
 * Private SourceId type, for convenience
 */
typedef int SourceId;

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
static void	dp_UpdateFile FP ((char *, struct dsp_UpdateFile *));
static DaemonPlatform* dp_DaemonPlatform (PlatformId pid);
static void	dp_DeleteData FP ((PlatformId pid, ZebraTime *));
static void	dp_DeleteObs FP ((PlatformId pid, ZebraTime *));
static void	ZapDF FP ((DataFile *));
static void	DoRescan FP ((struct ui_command *cmds));
static void	SetUpEvery FP ((struct ui_command *));
static void	ExecEvery FP ((ZebraTime *, int));
static void	Truncate FP ((struct ui_command *));
static int	FreeSpace FP ((int, SValue *, int *, SValue *, int *));
static int	BCHandler FP ((int, char *, int));
static void	BCSetup FP ((char *, int));
static void	RemDataGone FP ((struct dsp_BCDataGone *));
static void 	SendNPlat FP ((char *));
static void	SendPlatStruct FP ((char *, struct dsp_GetPlatStruct *));
static void	SendClassStruct FP ((char *, struct dsp_GetPlatStruct *));
static void	SendPlatformList FP ((char *, struct dsp_PlatformSearch *));
#ifdef notdef
static void	DoLookup FP ((char *, char *));
static void	DoClassLookup FP ((char *who, char *name));
#endif
static void	FindDF FP ((char *, struct dsp_FindDF *, int));
static const SourceId*	SourceList (SourceId wanted, int *nsrcs);
static void	FindDFLink (char *, struct dsp_FindDFLink *, int);
static int	NewRevision FP ((DataFile *df));
static void	ClientDefine FP ((char *who, struct dsp_ClassStruct *, int));
#ifdef notdef
static void	ClientAddSubplat FP ((PlatClassId id, SubPlatform *sp));
#endif
static void	ClientInstance (char *who, PlatClassId cid, char *name,
				PlatformId parent);
static void 	InitPlatform (Platform *pi);
static void	DestroyPlatform (Platform *pi);
static void	DefineSource (struct ui_command *cmds);
static int	InSourceDef (const char *srcname, struct ui_command *cmds);
static void	SendSourceInfo (char *who, struct dsp_GetSrcInfo *request);
static void	SendPlatDir (char *who, struct dsp_GetPlatDir *request);


/*
 * Prototypes
 */
int	Shutdown (void);

/*
 * Broadcast stuff.
 */
static int BCastSocket = -1;

/*
 * Our list of sources.  There's an arbitrary limit of 10 for now.
 */
# define MAXSOURCES 10
Source *Srcs[MAXSOURCES];
int NSrcs;

/*
 * Our data structure for the EVERY command.
 */
# define MAXEVERY 10
int NEvery = 0;
char *ECmds[MAXEVERY];

zbool InitialScan = TRUE;
int PlatformsScanned = 0;

time_t LastScan = 0;	/* Time of the most recent FULL scan */
time_t LastCache = 0;	/* Time to which cache files are up-to-date */
time_t Genesis;		/* In the beginning, there was Zeb...*/

# define DEFAULT_CACHE_NAME "Zebra.cache"

/*
 * Other options
 */
zbool StatRevisions = TRUE;      /* Use stat() calls to get revision 
                                   numbers, rather than using a counter */

zbool Debug = FALSE;             /* Produce voluminous output as it happens */
zbool ParseOnly = FALSE;         /* Parse the init file and abort        */

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
	dt_ExtendPlatforms (sizeof(DaemonPlatform), InitPlatform, 
			    DestroyPlatform);
	usy_c_indirect (vtable, "StatRevisions", &StatRevisions, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "Debug", &Debug, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "ParseOnly", &ParseOnly, SYMT_BOOL, 0);
/*
 * Make DefaultKeep from Platforms.c accessible.
 */
	usy_c_indirect (vtable, "DefaultKeep", &DefaultKeep, SYMT_INT, 0);
/*
 * Indirects for the platform table parameters.
 */
	usy_c_indirect (vtable, "PTableSize", &PTableSize, SYMT_INT, 0);
	usy_c_indirect (vtable, "PTableGrow", &PTableGrow, SYMT_INT, 0);
	usy_c_indirect (vtable, "CTableSize", &CTableSize, SYMT_INT, 0);
	usy_c_indirect (vtable, "CTableGrow", &CTableGrow, SYMT_INT, 0);
/*
 * Other initialization.
 */
	F_Init ();
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
	ZebraTime t1, t2;
	SourceId s;
	stbl uivars = usy_g_stbl ("ui$variable_table");
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
 * Inform them if they set some deprecated variables
 */
	if (usy_g_symbol (uivars, "cachefile", &type, &v))
	    msg_ELog (EF_INFO, "Variable 'cachefile' is now ignored");

	if (usy_g_symbol (uivars, "remotecachefile", &type, &v))
	    msg_ELog (EF_INFO, "Variable 'remcachefile' is now ignored");

	if (usy_g_symbol (uivars, "delaydatadirs", &type, &v))
	    msg_ELog (EF_INFO, "Variable 'delaydatadirs' is now ignored");
/*
 * If no sources were declared, give a warning and try for the old "DataDir"
 * variable, DS_DATA_DIR environment variable, or GetDataDir().
 */
	if (! NSrcs)
	{
	    char *dir;
	    char *datadir = (char*) malloc (128);
	    char *cachefile = (char*) malloc (256);
	    
	    if (usy_g_symbol (uivars, "DataDir", &type, &v))
		strcpy (datadir, v.us_v_ptr);
	    else if ((dir = getenv ("DS_DATA_DIR")) != NULL)
		strcpy (datadir, dir);
	    else
		strcpy (datadir, GetDataDir());

	    sprintf (cachefile, "%s/%s", datadir, DEFAULT_CACHE_NAME);

	    msg_ELog (EF_INFO, "No 'source' lines in DS config file");
	    msg_ELog (EF_INFO, "Opening %s as source 'default'", datadir);

	    if (! (Srcs[NSrcs++] = src_Open ("default", datadir, cachefile)))
	    {
		msg_ELog (EF_EMERGENCY, "Could not open default data source");
		exit (1);
	    }
	    
	    free (datadir);
	    free (cachefile);
	}
/*
 * Perform the file scan to see what is out there.
 */
	tl_Time (&t1);
	for (s = 0; s < NSrcs; s++)
	{
	    msg_ELog (EF_INFO, "Starting file scan for source '%s' (%s)", 
		      src_Name (Srcs[s]), src_RootDir (Srcs[s]));
	    DataScan (Srcs[s]);
	}

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
	 * Platform and class definitions are passed to the Config module.
	 */
	   case DK_PLATFORM:
	   case DK_SUBPLATFORM:
	   case DK_CLASS:
	   case DK_INSTANCE:
	   case DK_SUBPLATS:
		return (dc_Handler (junk, cmds));
	    /* break; */
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
		msg_ELog (EF_PROBLEM,
		  "'cache' command no longer meaningful nor functional");
# ifdef notdef	/* cache files are deprecated */
		/* dirty option overrides unified cache file name */
		if (cmds[1].uc_ctype == UTT_END)
			WriteCache (NULL, FALSE);
		else if (cmds[1].uc_ctype == UTT_KW)
			WriteCache (NULL, TRUE);
		else
			WriteCache (UPTR (cmds[1]), FALSE);
# endif
		break;
	/*
	 * Force a rescan
	 */
	   case DK_RESCAN:
		DoRescan (cmds + 1);
		break;
	/*
	 * Source definition
	 */
	   case DK_SOURCE:
		DefineSource (cmds + 1);
		break;

	/*
	 * Deal with a require.
	 */
	   case DK_REQUIRE:
		Require (UPTR (cmds[1]));
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



static void
DefineSource (struct ui_command *cmds)
/*
 * Add a source to our list
 */
{
    static char name[128];
    strcpy (name, UPTR (cmds[0]));
/*
 * Get the rest of the source definition
 */
    ERRORCATCH
	ui_subcommand ("in-source", "Source>", InSourceDef, (long)name);
    ENDCATCH
}


static int
InSourceDef (const char* srcname, struct ui_command *cmds)
{
    static int newdef = 1;
    static char dir[128], cachefile[128];
    static zbool dirconst, fileconst, rememberall, forcedirs;
    static char **plats, **platdirs;
    static int nplats;
    int s, p;
/*
 * Initialize if we're beginning a new source definition
 */
    if (newdef)
    {
	newdef = 0;
	dirconst = fileconst = rememberall = forcedirs = 0;
	plats = 0;
	platdirs = 0;
	nplats = 0;
	dir[0] = '\0';
	cachefile[0] = '\0';
    }
/*
 * Interpret this source definition command
 */
    switch (UKEY (cmds[0]))
    {
      case 0: /* endsource */
	break;
      case 1:	/* Directory <dir> */
	strcpy (dir, UPTR (cmds[1]));
	return 1;
      case 2:	/* CacheFile <cachefile> */
	strcpy (cachefile, UPTR (cmds[1]));
	return 1;
      case 3:	/* DirConst */
	dirconst = UINT (cmds[1]);
	return 1;
      case 4:	/* FileConst */
	fileconst = UINT (cmds[1]);
	return 1;
      case 5:	/* RememberAll */
	rememberall = UINT (cmds[1]);
	return 1;
      case 6:	/* PlatDir <platname> <dir> */
	p = nplats++;
	plats = (char**) realloc (plats, nplats * sizeof (char*));
	plats[p] = (char*) malloc (strlen (UPTR (cmds[1])) + 1);
	strcpy (plats[p], UPTR (cmds[1]));

	platdirs = (char**) realloc (platdirs, nplats * sizeof (char*));
	platdirs[p] = (char*) malloc (strlen (UPTR (cmds[2])) + 1);
	strcpy (platdirs[p], UPTR (cmds[2]));
	return 1;
      case 7:	/* forcedirs = !DelayDataDirs */
	forcedirs = ! UINT (cmds[1]);
	return 1;
      default:
	msg_ELog (EF_PROBLEM, "Unknown keyword number %d in InSourceDef",
		  UKEY (cmds[0]));
	return 1;
    }
/*
 * We got all we're going to get, so build the source.  But first, make sure
 * we start fresh next time we come into this function.
 */
    newdef = 1;
/*
 * Make sure we got a directory
 */
    if (! dir[0])
    {
	msg_ELog (EF_PROBLEM, "No directory given for source '%s'!", srcname);
	return 0;
    }
/*
 * If we got no cache file name, use:
 *	<data_dir>/<DEFAULT_CACHE_NAME>
 * Otherwise, if we got a relative path name, use:
 *	<data_dir>/<cachefile>
 * Otherwise, use the filename exactly as given:
 *	<cachefile>
 */
    if (! cachefile[0])
	sprintf (cachefile, "%s/%s", dir, DEFAULT_CACHE_NAME);
    else if (cachefile[0] != '/')
    {
	char temp[128];
	sprintf (temp, "%s/%s", dir, cachefile);
	strcpy (cachefile, temp);
    }
/*
 * Open the source and set the appropriate flags.
 */
    s = NSrcs++;
    if (! (Srcs[s] = src_Open (srcname, dir, cachefile)))
    {
	msg_ELog (EF_PROBLEM, "Failed to open source '%s'", srcname);
	NSrcs--;
	return 0;
    }
	
    src_SetDirConst (Srcs[s], dirconst);
    src_SetFileConst (Srcs[s], fileconst);
    src_SetForceDirs (Srcs[s], forcedirs);
    src_SetRememberAll (Srcs[s], rememberall);
/*
 * Set any fixed platform directories
 */
    for (p = 0; p < nplats; p++)
    {
	src_SetPlatDir (Srcs[s], plats[p], platdirs[p]);
	free (plats[p]);
	free (platdirs[p]);
    }

    return 0;
}

    

int
Shutdown ()
/*
 * Shut things down.
 */
{
    SourceId s;
/*
 * Close our sources cleanly
 */
    for (s = 0; s < NSrcs; s++)
	src_Close (Srcs[s]);
/*
 * Clean up in UI land.
 */
    ui_finish ();
    msg_ELog (EF_INFO, "shutdown: disconnecting, exiting.");
    msg_disconnect ();
    exit (0);
    return (0);	/* keep compilers happy */
}



# ifdef notdef
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
# endif


static void
mh_message (msg)
struct message *msg;
/*
 * Deal with a message from the msg subsystem itself.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
	struct mh_clientevent *client;

	switch (tm->mh_type)
	{
	   case MH_SHUTDOWN:
		printf ("%s: normal shutdown\n", msg_myname() );
		Shutdown ();
		/* no return */
	/*
	 * For client events, we are really only interested in deaths, so
	 * that we can get rid of any notification requests.
	 */
	   case MH_CLIENT:
		client = (struct mh_clientevent *) msg->m_data;
		if (client->mh_evtype == MH_CE_DISCONNECT)
			dap_Disconnect (client->mh_client);
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
#ifdef notdef
	struct dsp_MarkArchived *dma;
#endif
	struct dsp_ProtoVersion dpv;
	struct dsp_Instance *im;
	SourceId s;

	switch (dt->dsp_type)
	{
	/*
	 * They want a new data file entry.
	 */
	   case dpt_NewFileRequest:
	   	dp_NewFile (from, (struct dsp_CreateFile *) dt);
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
# ifdef notdef
	/*
	 * A file is archived.
	 */
	   case dpt_MarkArchived:
	   	dma = (struct dsp_MarkArchived *) dt;
		if (dma->dsp_FileIndex > 0)
			DFTable[dma->dsp_FileIndex].df_flags |= DFF_Archived;
		break;
# endif
	/*
	 * Do a rescan for the given platform(s)
	 */
	   case dpt_Rescan:
		for (s = 0; s < NSrcs; s++)
		{
			struct dsp_Rescan *dsr = (struct dsp_Rescan *)dt;
			const Platform *p = 0;
			if (! dsr->dsp_all)
				p = dt_FindPlatform (dsr->dsp_pid);
			if (dsr->dsp_all || p)
				Rescan (Srcs[s], p, dsr->dsp_all);
		}
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
	   case dpt_PlatformSearch:
		SendPlatformList (from, (struct dsp_PlatformSearch *)dt);
		break;
	/*
	 * Class lookup.
	 */
	   case dpt_GetClassStruct:
	   	SendClassStruct (from, ((struct dsp_GetPlatStruct *) dt));
		break;
	/*
	 * Find a data file based on time.
	 */
	   case dpt_FindDF:
	   	FindDF (from, (struct dsp_FindDF *) dt, 1);
		break;
	   case dpt_FindAfter:
	   	FindDF (from, (struct dsp_FindDF *) dt, 0);
		break;
	/*
	 * Find the next or previous file
	 */
	   case dpt_FindDFPrev:
		FindDFLink (from, (struct dsp_FindDFLink *) dt, 1);
		break;
	   case dpt_FindDFNext:
		FindDFLink (from, (struct dsp_FindDFLink *) dt, 0);
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
	   case dpt_GetSrcInfo:
		SendSourceInfo (from, (struct dsp_GetSrcInfo *)dt);
		break;
	   case dpt_GetPlatDir:
		SendPlatDir (from, (struct dsp_GetPlatDir *)dt);
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
    const SourceId *sids;
    Source *src;
    int nsids;
    DataFileCore core;
    PlatformId pid = request->dsp_plat;
    const Platform *p = dt_FindPlatform (pid);
/*
 * Try the first source that matches the source request
 */
    sids = SourceList (request->dsp_srcid, &nsids);
    src = nsids ? Srcs[sids[0]] : 0;

    if (! src || src_IsReadOnly (src))
    {
	msg_ELog (EF_PROBLEM, "Cannot create new file for %s, no good Source", 
		  pi_Name (p));

	response.dsp_type = dpt_R_NewFileFailure;
	msg_send (from, MT_DATASTORE, FALSE, &response, sizeof (response));
	return;
    }
/*
 * Make sure the platform directory exists, creating it if necessary.
 * If we can't create it then we must abort the new file.
 */
    if (! src_ConfirmDataDir (src, p))
    {
	msg_ELog (EF_PROBLEM, "Could not create %s datadir for new file",
		  pi_Name(p));

	response.dsp_type = dpt_R_NewFileFailure;
	msg_send (from, MT_DATASTORE, FALSE, &response, sizeof (response));
	return;
    }
/* 
 * Build a DataFileCore
 */
    dt_SetString (core.dfc_name, request->dsp_FileName, 
		  sizeof(core.dfc_name), "request for new file");
    core.dfc_begin = core.dfc_end = request->dsp_time;
    core.dfc_rev = 0;
    core.dfc_inode = 0;
    core.dfc_ftype = pi_FileType (p);
    core.dfc_nsample = 0;
/*
 * And now build a full-fledged DataFile from our pieces
 */
    BuildDataFile (&response.dsp_file, &core, src, p);
/*
 * Respond back to the requester and quit.
 */
    response.dsp_type = dpt_R_NewFileSuccess;
    msg_send (from, MT_DATASTORE, FALSE, &response, sizeof (response));
}




void
BuildDataFile (DataFile *df, const DataFileCore *core, Source *src, 
	       const Platform *p)
/*
 * Build up a full-fledged DataFile from a DataFileCore, Platform, and Source.
 */
{
    SourceId s;
/*
 * Find the source in our list, so we have the SourceId
 */
    for (s = 0; s < NSrcs; s++)
	if (Srcs[s] == src)
	    break;

    if (s == NSrcs)
    {
	msg_ELog (EF_PROBLEM, "BuildDataFile: Unknown source '%s'",
		  src_Name (src));
	s = -1; /* XXXXXX */
    }
/*
 * Initialize
 */
    df->df_core = *core;
    df->df_pid = pi_Id (p);
    df->df_srcid = s;
/*
 * Full file name: path from src_DataDir + the file name
 */
    strcpy (df->df_fullname, DataFilePath (src, p, core));
}


const char*
DataFilePath (Source *src, const Platform *p, const DataFileCore *dfc)
/*
 * Return a string containing the full data file path name given a
 * DataFileCore, Platform, and Source.  The string is valid until the 
 * next call to this function.
 */
{
    static char path[CFG_FILEPATH_LEN];
    sprintf (path, "%s/%s", src_DataDir (src, p), dfc->dfc_name);
    return (path);
}


    
static void
dp_UpdateFile (from, request)
char *from;
struct dsp_UpdateFile *request;
/*
 * Deal with a file update notification.
 */
{
    DataFile *df = &request->dsp_file;
    DataFileCore *core = &df->df_core;
    SourceId srcid;
    Source *src;
    PlatformId pid = df->df_pid;
    const Platform *p = dt_FindPlatform (pid);
    DaemonPlatform *dp = dp_DaemonPlatform (pid);
    int append = FALSE;
    struct dsp_UpdateAck ack;
    ZebraTime last;
/*
 * Source substitution for SRC_DEFAULT, and sanity check
 */
    srcid = (df->df_srcid == SRC_DEFAULT) ? 0 : df->df_srcid;
    if (srcid < 0 || srcid >= NSrcs)
    {
	msg_ELog (EF_PROBLEM, "Unable to update file '%s', with bad source %d",
		  df->df_fullname, srcid);
	msg_ELog (EF_PROBLEM, "File update failed due to bad source");
	return;
    }
/*
 * Message
 */
    src = Srcs[srcid];
    msg_ELog (EF_DEBUG,
	      "%s updated '%s' file %s (%s) ns %d ow %d last %d",
	      from, src_Name (src), df->df_core.dfc_name, pi_Name (p), 
	      request->dsp_NSamples, request->dsp_NOverwrite, 
	      request->dsp_Last);
/*
 * Count this as an append if the new end time is later than the latest
 * data we currently have for the given platform/source combo.
 */
    append = ! src_LastTime (src, p, &last) || TC_Less(last, core->dfc_end);
/*
 * Get a new revision for the file.
 */
    core->dfc_rev = NewRevision (df);

    dp->dp_NewSamps += request->dsp_NSamples;
    dp->dp_OwSamps += request->dsp_NOverwrite;
/*
 * Update or add this file in our source.
 */
    src_UpdateFile (src, p, core);
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
	dap_Notify (pid, &core->dfc_end, dp->dp_NewSamps, dp->dp_OwSamps, 
		    append);
	dp->dp_NewSamps = dp->dp_OwSamps = 0;
    }
}





static void
dp_DeleteData (pid, t)
PlatformId pid;
ZebraTime *t;
/*
 * Handle the delete data request.
 */
{
    SourceId s;
    const Platform *p = dt_FindPlatform (pid);
    
    if (! p)
    {
	msg_ELog (EF_PROBLEM, "DeleteData on bad platform id %d", pid);
	return;
    }
/*
 * Subplatform sanity check
 */
    if (pi_Subplatform(p))
    {
	msg_ELog (EF_PROBLEM, "Attempted DeleteData on subplat %s",
		  pi_Name (p));
	return;
    }
/*
 * Loop through all sources
 */
    for (s = 0; s < NSrcs; s++)
    {
	DataFileCore dfc;
	zbool ok;
	ZebraTime prev_btime;
    /*
     * Skip read-only sources
     */
	if (src_IsReadOnly (Srcs[s]))
	{
	    msg_ELog (EF_DEBUG, "DeleteData skipping read-only source %s",
		      src_Name (Srcs[s]));
	    continue;
	}
    /* 
     * Working from the earliest file, remove all files ending at or before
     * t, stopping when we hit one starting after t, or when we hit 
     * the end of the list. 
     */
	for (ok = src_First (Srcs[s], p, &dfc); ok;
	     ok = src_FindAfter (Srcs[s], p, &prev_btime, &dfc))
	{
	    if (TC_LessEq (dfc.dfc_end, *t))
	    {
		DataFile df;
	    /*
	     * Build the full DataFile structure for the file, then
	     * unlink it and send notifications of its demise.
	     */
		BuildDataFile (&df, &dfc, Srcs[s], p);
		ZapDF (&df);
	    /*
	     * Remove it from our list
	     */
		src_RemoveFile (Srcs[s], p, &dfc);
	    }
	    else
		break;

	    prev_btime = dfc.dfc_begin;
	}
    }
}




static void
dp_DeleteObs (pid, t)
PlatformId pid;
ZebraTime *t;
/*
 * Handle the request to delete a single, whole observation.
 */
{
    SourceId s;
    const Platform *p = dt_FindPlatform (pid);

    if (! p)
    {
	msg_ELog (EF_PROBLEM, "DeleteData on bad platform id %d", pid);
	return;
    }
/*
 * Subplatform sanity check
 */
    if (pi_Subplatform(p))
    {
	msg_ELog (EF_PROBLEM, "Attempted DeleteData on subplat %s",
		  pi_Name (p));
	return;
    }
/*
 * Loop through all sources
 */
    for (s = 0; s < NSrcs; s++)
    {
	DataFileCore dfc;
    /*
     * Skip read-only sources
     */
	if (src_IsReadOnly (Srcs[s]))
	{
	    msg_ELog (EF_DEBUG, "DeleteData skipping read-only source %s",
		      src_Name (Srcs[s]));
	    continue;
	}
    /* 
     * Get the latest file at or before t.  Delete it if it contains t
     * (i.e., if its end time is at or after t).
     */
	if (! src_FindBefore (Srcs[s], p, t, &dfc))
	    continue;
	    
	if (TC_LessEq (*t, dfc.dfc_end))
	{
	    DataFile df;
	/*
	 * Build the full DataFile structure for the file, then
	 * unlink it and send notifications of its demise.
	 */
	    BuildDataFile (&df, &dfc, Srcs[s], p);
	    ZapDF (&df);
	/*
	 * Remove it from our list
	 */
	    src_RemoveFile (Srcs[s], p, &dfc);
	}
    }
}




static void
RemDataGone (dg)
struct dsp_BCDataGone *dg;
/*
 * A remotely accessible data file has gone away.
 */
{
    msg_ELog (EF_INFO, 
	      "RemDataGone: Remote data removal notice currently ignored");
}



void
DataFileGone (DataFile *df)
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
    dg.dsp_file = *df;
    msg_send ("DataStore", MT_DATASTORE, TRUE, &dg, sizeof (dg));
/*
 * If we are broadcasting to other worlds, we do that too.
 */
    if (BCastSocket > 0)
    {
	const Platform *p = dt_FindPlatform (df->df_pid);
	
	struct dsp_BCDataGone dg;
	dg.dsp_type = dpt_BCDataGone;
	strcpy (dg.dsp_Plat, pi_Name (p));
	strcpy (dg.dsp_FileName, df->df_core.dfc_name);
	msg_BCast (BCastSocket, &dg, sizeof (dg));
    }
/*
 * Make sure we have the file closed ourselves.  This may be pointless (for
 * the moment) since the daemon never keeps any files open.
 */
    dfa_ForceClose (df);
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
    filenames = dfa_GetAssociatedFiles (df, &nfiles);
    for (i = 0; i < nfiles; i++)
    {
	if (unlink (filenames[i] ) < 0)
	    msg_ELog (EF_PROBLEM, "Error %d unlinking %s", 
		      errno, filenames[i]);
	if (filenames[i])  
	    free (filenames[i]);
    }

    if (filenames) 
	free (filenames);
}




static void
DoRescan (cmds)
struct ui_command *cmds;
{
	zbool all = FALSE;
	const Platform *plat = 0;

	all = (cmds[0].uc_ctype == UTT_END) || (cmds[0].uc_ctype == UTT_KW);
	if (! all)
	{
		plat = dt_FindPlatformName (UPTR (cmds[0]));
		if (! plat)
			msg_ELog (EF_PROBLEM, "rescan: no such platform '%s'",
				  cmds[0].uc_text);
	}
	if ((all) || (plat))
	{
	    SourceId s;
	    for (s = 0; s < NSrcs; s++)
		Rescan (Srcs[s], plat, all);
	}
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
ZebraTime *t;
int slot;
/*
 * Run this EVERY command.
 */
{
	msg_ELog (EF_DEBUG, "EVERY timeout: '%s'", ECmds[slot]);
	ui_perform (ECmds[slot]);
}




static void
Truncate (struct ui_command *cmds)
/*
 * Handle the TRUNCATE command.
 */
{
    int seconds;
    PlatformId pid;
    ZebraTime now, zaptime;
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
	char *pname = UPTR (cmds[1]);
	const Platform *p = dt_FindPlatformName (pname);
	    
	if (p)
	{
	    zaptime = now;
	    zaptime.zt_Sec -= (seconds > 0) ? seconds : pi_Keep(p);
	    dp_DeleteData (pi_Id (p), &zaptime);
	}
	else
	    msg_ELog (EF_PROBLEM, "TRUNCATE on bad platform %s",
		      pname);
	return;
    }
/*
 * Otherwise we go through the list and do everything.
 */
    for (pid = 0; pid < dt_NPlatform(); pid++)
    {
	const Platform *p = dt_FindPlatform (pid);
	    
	if (! pi_Subplatform (p))
	{
	    zaptime = now;
	    zaptime.zt_Sec -= (seconds > 0) ? seconds : pi_Keep (p);
	    dp_DeleteData (pid, &zaptime);
	}
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
SendSourceInfo (char *to, struct dsp_GetSrcInfo *request)
/*
 * Send source information to a client
 */
{
    struct dsp_R_SrcInfo answer;
    SourceId srcid = request->dsp_srcid;

    answer.dsp_type = dpt_R_SrcInfo;

    if (srcid < 0 || srcid >= NSrcs)
	answer.dsp_success = 0;
    else
    {
	Source *src = Srcs[srcid];
	SourceInfo *si = &(answer.dsp_srcinfo);
	
	answer.dsp_success = 1;
	si->src_Id = srcid;
	strcpy (si->src_Name, src_Name (src));
	strcpy (si->src_Dir, src_RootDir (src));
	si->src_ReadOnly = src_IsReadOnly (src);
    }

    msg_send (to, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}




static void	
SendPlatDir (char *to, struct dsp_GetPlatDir *request)
/*
 * Send source information to a client
 */
{
    struct dsp_R_PlatDir answer;
    const Platform *p = dt_FindPlatform (request->dsp_pid);
    const SourceId *sids;
    SourceId srcid;
    int nsids;

    answer.dsp_type = dpt_R_PlatDir;
/*
 * Use the first source that matches the source request
 */
    sids = SourceList (request->dsp_srcid, &nsids);
    srcid = nsids ? sids[0] : -1;

    if (srcid < 0 || srcid >= NSrcs || ! p)
	answer.dsp_success = 0;
    else
    {
	strcpy (answer.dsp_dir, src_DataDir (Srcs[srcid], p));
	answer.dsp_success = 1;
    }

    msg_send (to, MT_DATASTORE, FALSE, &answer, sizeof (answer));
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
	np.dsp_nplat = dt_NPlatform();
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
	const Platform *p;

	answer.dsp_type = dpt_R_PlatStruct;

	p = (req->dsp_id == BadPlatform) ? 
	    dt_FindPlatformName (req->dsp_name) :
	    dt_FindPlatform (req->dsp_id);
	

	if (! p)
	{
	    answer.dsp_result = 0;
	    answer.dsp_pid = BadPlatform;
	}
	else
	{
	    answer.dsp_plat = *p;
	    answer.dsp_pid = pi_Id (p);
	    answer.dsp_result = 1;
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
	const PlatformClass *pc;

	answer = &am;
	answer->dsp_type = dpt_R_ClassStruct;
	answer->dsp_result = 0;

	pc = (req->dsp_id == BadClass) ? 
	    dt_FindClassName (req->dsp_name) :
	    dt_FindClass (req->dsp_id);
	
	if (pc)
	{
		answer = dt_InjectClass (pc, &am, &len);
		answer->dsp_cid = pc_Id (pc);
		answer->dsp_result = 1;
	}

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
    struct dsp_PlatformList fail;
    int npids;
    int len;

    answer = dt_AnswerSearch (req, &npids, &len);
    if (! answer)
    {
	answer = &fail;
	len = sizeof (struct dsp_PlatformList);
	answer->dsp_npids = 0;
	answer->dsp_type = dpt_R_PlatformSearch;
    }
/*
 * The response has been filled or left empty.  Send it off.
 */
    msg_send (to, MT_DATASTORE, FALSE, answer, len);
    if (answer != &fail)
	free (answer);
}



/* Currently unused */
#ifdef notdef
static void
DoLookup (who, plat)
char *who, *plat;
/*
 * Look up this platform for somebody.
 */
{
	const Platform *p = dt_FindPlatformName (plat);
	struct dsp_PID answer;

	answer.dsp_type = dpt_R_PID;
	answer.dsp_pid = p ? pi_Id (p) : BadPlatform;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}
#endif



/* Currently unused */
#ifdef notdef
static void
DoClassLookup (who, name)
char *who, *name;
/*
 * Look up this platform class for somebody.
 */
{
	const PlatformClass *pc = dt_FindClassName (name);
	struct dsp_PID answer;

	answer.dsp_type = dpt_R_CID;
	answer.dsp_pid = pc ? (pc->dpc_id) : BadClass;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}
#endif




static void
FindDF (char *who, struct dsp_FindDF *req, int before)
/* 
 * Find a data file entry based on time.  If 'before' is true, find the
 * latest file containing data at or before the wanted time.  If 'before'
 * is false, find the earliest file containing data at or after the wanted
 * time. 
 */
{
    struct dsp_R_DataFile answer;
    PlatformId pid = req->dsp_pid;
    const Platform *p = dt_FindPlatform (pid);
    ZebraTime *t = &req->dsp_when;
    ZebraTime besttime;
    DataFileCore bestcore;
    const SourceId *sids;
    SourceId bestsrc;
    int s, nsids;
    zbool havematch;
/*
 * Get the source(s) to search
 */
    sids = SourceList (req->dsp_srcid, &nsids);
/*
 * Initialize our answer
 */
    answer.dsp_type = dpt_R_DataFile;
    answer.dsp_success = 0;
/* 
 * Best find so far.  Initialize to the beginning or end of time (at least
 * to the extent that we can represent it... :-)
 */
    havematch = 0;
    besttime = before ? ZT_ALPHA : ZT_OMEGA;
/*
 * Go through all the sources we're allowed.
 *
 * Note that we can't simply call src_FindBefore() or src_FindAfter() based
 * on the value of 'before'.  This is because we return a file *containing*
 * the given time if possible, but the src_* functions return files *starting*
 * at or before (at or after) the selected time.
 */	
    for (s = 0; s < nsids; s++)
    {
	DataFileCore core;
    /*
     * Start by looking for the the latest file starting before t.
     */
	zbool ok = src_FindBefore (Srcs[sids[s]], p, t, &core);
    /* 
     * If this is an "after" search, and either 1) no file was found
     * starting before t or 2) the file found ends before t (i.e., it
     * doesn't contain t), then we want the first file starting after t. 
     */
	if (! before && (! ok || TC_Less (core.dfc_end, *t)))
	    ok = src_FindAfter (Srcs[sids[s]], p, t, &core);
    /*
     * If we found an appropriate file from this source, check if it's the best
     * match so far.  This is true if:
     * 		o they asked for "before" and the end time is later than any
     *		  previous match
     *			OR
     *		o they asked fof "after" and the begin time is earlier than
     *		  any previous match
     */
	if (ok)
	{
	    if (before && (!havematch || 
			   !TC_Less (core.dfc_end, besttime)))
	    {
		bestcore = core;
		besttime = core.dfc_end;
		bestsrc = s;
	    }
	    else if (!before && (!havematch || 
				 TC_LessEq (core.dfc_begin, besttime)))
	    {
		bestcore = core;
		besttime = core.dfc_begin;
		bestsrc = s;
	    }

	    havematch = 1;
	}
    }
/*
 * Now return our answer.
 */
    if (havematch)
    {
	const Platform *p = dt_FindPlatform (pid);

	answer.dsp_success = 1;
	BuildDataFile (&answer.dsp_file, &bestcore, Srcs[bestsrc], p);
    }
	
    msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}




static const SourceId*
SourceList (SourceId wanted, int *nsrcs)
/*
 * Return a list of "true" SourceIds based on the wanted SourceId (which
 * may be one of the special values SRC_ALL, SRC_DEFAULT, SRC_DEF_W).  The
 * list is good until the next call to this function, and should not be
 * freed.
 */
{
    static SourceId srcs[MAXSOURCES];
    int i;

    switch (wanted)
    {
      case SRC_ALL:
	for (i = 0; i < NSrcs; i++)
	    srcs[i] = i;
	*nsrcs = NSrcs;
	return (srcs);
      case SRC_DEFAULT:
	srcs[0] = 0;
	*nsrcs = 1;
	return (srcs);
      case SRC_DEFAULT_W:
	for (i = 0; i < NSrcs; i++)
	{
	    if (src_IsReadOnly (Srcs[i]))
		continue;
	    srcs[0] = i;
	    *nsrcs = 1;
	    return (srcs);
	}

	*nsrcs = 0;
	return (srcs);
      default:
	if (wanted < 0 || wanted > NSrcs - 1)
	{
	    msg_ELog (EF_PROBLEM, "Request for bad SourceId %d", wanted);
	    *nsrcs = 0;
	    return (0);
	}
	    
	*nsrcs = 1;
	srcs[0] = wanted;
	return (srcs);
    }
}

	
	
	
static void
FindDFLink (char *who, struct dsp_FindDFLink *req, int prev)
/* 
 * Find the previous or next data file w.r.t. a given file.  Time comparisons
 * are based on the start times of the files.  All sources are checked.
 */
{
    struct dsp_R_DataFile answer;
    DataFile *df_in = &req->dsp_file;
    int s;
    PlatformId pid = df_in->df_pid;
    const Platform *p = dt_FindPlatform (pid);
    zbool haveone = FALSE;
    DataFileCore best;
    Source *bestsrc;
    ZTime t = df_in->df_core.dfc_begin;
    /*
     * Set our file-finding function based on whether we want the 
     * previous or next file.
     */
    zbool (*findFunc)(Source *src, const Platform *p, const ZebraTime *t, 
		      DataFileCore *dfc);
    findFunc = prev ? src_FindBefore : src_FindAfter;

    /*
     * Gotta adjust our time by epsilon, since src_FindBefore and src_FindAfter
     * will return exact time matches if found.
     */
    t.zt_MicroSec += prev ? -1 : 1;

    if (t.zt_MicroSec > 1000000)
    {
        t.zt_MicroSec -= 1000000; t.zt_Sec += 1;
    }
    else if (t.zt_MicroSec < 0)
    {
        t.zt_MicroSec += 1000000; t.zt_Sec -= 1;
    }
    
    /*
     * Loop through the sources to find the closest previous/next
     * file.
     */
    for (s = 0; s < NSrcs; s++)
    {
	Source *src = Srcs[s];
	DataFileCore core;
	
	if ((*findFunc)(src, p, &t, &core))
	{
	    if (! haveone ||
		(prev && TC_Less(best.dfc_begin, core.dfc_begin))||
		(!prev && TC_Less(core.dfc_begin, best.dfc_begin)))
	    {
		best = core;
		bestsrc = src;
		haveone = TRUE;
	    }
	}
    }
    
    /*
     * Build our answer message.
     */
    answer.dsp_type = dpt_R_DataFile;
    if (haveone)
    {
	answer.dsp_success = 1;
	BuildDataFile (&answer.dsp_file, &best, bestsrc, p);
    }
    else
    {
	answer.dsp_success = 0;
    }
    
    msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}	    




static int
NewRevision (DataFile *df)
{
    int rev;

    if (StatRevisions)
    {
	rev = StatRevision (df->df_fullname, &df->df_core.dfc_inode);
    /*
     * Make sure we actually got a more recent revision number
     */
	if (rev <= df->df_core.dfc_rev)
	{
#ifdef DEBUG
	    msg_ELog (EF_DEBUG, "%s: warping %d to %d",
		      "outdated stat mtime", rev, df->df_core.dfc_rev + 1);
#endif
	    rev = df->df_core.dfc_rev + 1;
	}
    }
    else
    {
	rev = df->df_core.dfc_rev + 1;
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
	 * Get a blank class into which we can extract the class in
	 * the message.  The client will have already derived the class
	 * if it has a superclass, so we don't need to do it again here.
	 * It would just get overwritten.  Once we have the class, we
	 * can just pass it to our local definition interface to assign
	 * it an id.
	 */
	pc = ds_NewClass (dsp->dsp_class.dpc_name);
	if (pc)
	{
		dt_ExtractClass (pc, dsp, len);
		answer.dsp_pid = ds_DefineClass (pc);
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
    struct dsp_PID answer;
    const PlatformClass *pc = dt_FindClass (cid);
    const Platform *p = 0;

    answer.dsp_type = dpt_R_PID;
    answer.dsp_pid = BadPlatform;
    if (pc)
    {
	answer.dsp_pid = ds_DefineSubPlatform (pc->dpc_id, name, parent);
	p = dt_FindPlatform (answer.dsp_pid);
    }
    else
	msg_ELog (EF_PROBLEM, "instance msg: class %d not found", cid);
/*
 * In case of on-the-fly definitions, scan the platform directory
 */
    if ( !InitialScan && p && ! pi_Subplatform (p) )
    {
    /*
     * Rescan this platform for all sources
     */
	SourceId s;
	for (s = 0; s < NSrcs; s++)
	    Rescan (Srcs[s], p, FALSE);
    }
    
    msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}



/* ----------------
 * Extension of platform instances to include daemon run-time info
 */

static void
DestroyPlatform (Platform *pi)
{
}



static void
InitPlatform (Platform *pi)
{
	DaemonPlatform *new = (DaemonPlatform *) pi;

	new->dp_NewSamps = 0;
	new->dp_OwSamps = 0;
}



static DaemonPlatform*
dp_DaemonPlatform (PlatformId pid)
/*
 * Return the (mutable) DaemonPlatform associated with the given PlatformId
 */
{
/*
 * XXXX: We explicitly cast away the "const" returned by dt_FindPlatform()...
 */
    return ((DaemonPlatform*) dt_FindPlatform (pid));
}

