/*
 * This is the main program for the data store daemon.
 */
static char *rcsid = "$Id: Daemon.c,v 1.1 1990-11-02 08:56:18 corbet Exp $";

# include <sys/types.h>
# include <dirent.h>
# include "../config/config.h"
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dsDaemon.h"
# include "commands.h"



/*
 * Local forwards.
 */
# ifdef __STDC__
	static int msg_Handler (struct message *);
	static int ui_Handler (int, struct ui_command *);
	void Shutdown (void);
	static void DataScan (void);
	static void ScanFile (Platform *, char *);
# else
	static int msg_Handler ();
	static int ui_Handler ();
	void Shutdown ();
	static void DataScan ();
	static void ScanFile ();
# endif



main (argc, argv)
int argc;
char **argv;
{
	char loadfile[80];
/*
 * Hook into the message system.
 */
	msg_connect (msg_Handler, DS_DAEMON_NAME);
	msg_join ("Client events");
	msg_DeathHandler (Shutdown);
/*
 * Hook into the UI.
 */
	fixdir_t ("DSDLOADFILE", LIBDIR, "dsDaemon.lf", loadfile, ".lf");
	ui_init (loadfile, FALSE, TRUE);
/*
 * Initialize.
 */
	strcpy (DefDataDir, DATADIR);
	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "datadir",
		DefDataDir, SYMT_STRING, 80);
	InitSharedMemory ();
	dt_InitTables ();
/*
 * Start reading commands.
 */
	ui_get_command ("initial", "dsd>", ui_Handler, 0);
	Shutdown ();
}


# ifdef notdef
/*
 * Arrange to read our init file.
 */
	sprintf (loadfile, "read %s/%s", ETCDIR, "DSDaemon.cfg");
	ui_perform (loadfile);
# endif


static void
FinishInit ()
/*
 * Finish our initialization.
 */
{
/*
 * Build the data table free list.
 */
	dt_FinishTables ();
/*
 * scan for existing data
 */
	DataScan ();
/*
 * announce to world?
 * arrange disk checks
 */
/*
 * Now just wait for something to happen.
 */
	msg_await ();
}



static int
msg_Handler (msg)
struct message *msg;
/*
 * Deal with an incoming message.
 */
{
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
		dc_DefPlatform (UPTR (cmds[1]));
		break;
	/*
	 * Configuration done -- go operational.
	 */
	   case DK_DONE:
	   	if (ndone++)
			msg_ELog (EF_PROBLEM, "Repeated DONE command");
		else
			FinishInit ();
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown Daemon kw: %d", UKEY (*cmds));
		break;
	}
	return (TRUE);
}




void
Shutdown ()
/*
 * Shut things down.
 */
{
/*
 * Clean up in UI land.
 */
	ui_finish ();
/*
 * Clean up the shared memory segment.
 */
	ShmCleanup ();
	exit (0);
}



static void
DataScan ()
/*
 * Go through the disk and see what data already exists.
 */
{
	int plat;
/*
 * Do this for every platform.
 */
	for (plat = 0; plat < ShmHeader->sm_nPlatform; plat++)
	{
		Platform *p = PTable + plat;
		DIR *dp = opendir (p->dp_dir);
		struct dirent *ent;
	/*
	 * Make sure there really is a directory.
	 */
		if (! dp)
		{
			msg_ELog (EF_PROBLEM,
				"Data dir %s (plat %s) nonexistent", p->dp_dir,
				p->dp_name);
			continue;
		}
	/*
	 * Go through the files.
	 */
	 	while (ent = readdir (dp))
			ScanFile (p, ent->d_name);
		closedir (dp);
	}
}





static void
ScanFile (p, file)
Platform *p;
char *file;
/*
 * Look at this file and see what we think of it.
 */
{
	time begin, end;
	DataFile *df;
/*
 * If DFA doesn't recognize it, we don't even bother.
 */
	if (! dfa_CheckName (p->dp_ftype, file))
		return;
/*
 * Grab a new datafile entry and begin to fill it in.
 */
	if (! (df = dt_NewFile ()))
		return;	/* bummer */
	sprintf (df->df_name, "%s/%s", p->dp_dir, file);
/*
 * Find the times for this file.
 */
	if (! dfa_QueryDate (p->dp_ftype, df->df_name, &df->df_begin,
			&df->df_end))
	{
		msg_ELog (EF_PROBLEM, "File '%s' inaccessible", df->df_name);
		dt_FreeDFE (df);
		return;
	}
# ifdef notdef
	msg_ELog (EF_DEBUG, "File '%s', %d %d to %d", file,
		df->df_begin.ds_yymmdd, df->df_begin.ds_hhmmss,
		df->df_end.ds_hhmmss);
# endif
/*
 * Finish the fillin and add it to this platform's list.
 */
	df->df_ftype = p->dp_ftype;
	dt_AddToPlatform (p, df, TRUE);
}





/*
 * Kludgery to make linking to DFA work.
 */
dsm_ShmLock ()
{
	msg_ELog (EF_PROBLEM, "BUG: dsm_ShmLock called in Daemon");
}

dsm_ShmUnlock ()
{
	msg_ELog (EF_PROBLEM, "BUG: dsm_ShmUnlock called in Daemon");
}
