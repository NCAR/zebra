/*
 * This is the main program for the data store daemon.
 */
static char *rcsid = "$Id: Daemon.c,v 1.4 1991-04-11 21:58:06 corbet Exp $";

# include <sys/types.h>
# include <dirent.h>
# include <errno.h>
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
	static void mh_message (struct message *);
	static void ds_message (char *, struct dsp_Template *);
	static void dp_NewFile (char *, struct dsp_CreateFile *);
	static void dp_AbortNewFile (struct dsp_AbortNewFile *);
	static void dp_UpdateFile (struct dsp_UpdateFile *);
	static void dp_DeleteData (Platform *, int);
	static void ZapDF (DataFile *);
	static void SetUpEvery (struct ui_command *);
	static void ExecEvery (time *, int);
	static void Truncate (struct ui_command *);
# else
	static int msg_Handler ();
	static int ui_Handler ();
	void Shutdown ();
	static void DataScan ();
	static void ScanFile ();
	static void mh_message ();
	static void ds_message ();
	static void dp_NewFile ();
	static void dp_AbortNewFile ();
	static void dp_UpdateFile ();
	static void dp_DeleteData ();
	static void ZapDF ();
	static void SetUpEvery ();
	static void ExecEvery ();
	static void Truncate ();
# endif


/*
 * Our data structure for the EVERY command.
 */
# define MAXEVERY 10
int NEvery = 0;
char *ECmds[MAXEVERY];






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
	dap_Init ();
/*
 * Set up the init file.
 */
	if (argc > 1)
	{
		SValue v;
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "initfile",
				SYMT_STRING, &v);
	}
/*
 * Start reading commands.
 */
	ui_get_command ("initial", "dsd>", ui_Handler, 0);
	Shutdown ();
}




static void
FinishInit ()
/*
 * Finish our initialization.
 */
{
/*
 * Build the data table free list, then fill it up by scanning the disk
 * to see which data is already present.
 */
 	ShmLock ();
	dt_FinishTables ();
	DataScan ();
	ShmUnlock ();
/*
 * If a cleanup_procedure exists, run it now.
 */
	if (usy_defined (0, "ui$procedure_table:cleanup"))
		ui_perform ("cleanup");
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
	   	ds_message (msg->m_from, (struct dsp_Template *) msg->m_data);
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
		dc_DefPlatform (UPTR (cmds[1]));
		break;

	   case DK_SUBPLATFORM:
	   	dc_SubPlatform (cmds + 1);
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
	 * Don't scan subplatforms.
	 */
		if (p->dp_flags & DPF_SUBPLATFORM)
			continue;
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
	int ns;
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
			&df->df_end, &ns))
	{
		msg_ELog (EF_PROBLEM, "File '%s' inaccessible", df->df_name);
		dt_FreeDFE (df);
		return;
	}
	df->df_nsample = ns;
	msg_ELog (EF_DEBUG, "File '%s', %d %d to %d ns %d", file,
		df->df_begin.ds_yymmdd, df->df_begin.ds_hhmmss,
		df->df_end.ds_hhmmss, df->df_nsample);
/*
 * Finish the fillin and add it to this platform's list.
 */
	df->df_ftype = p->dp_ftype;
	dt_AddToPlatform (p, df, TRUE);
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
		ui_finish ();
		exit (1);
	/*
	 * For client events, we are really only interested in deaths, so
	 * that we can get rid of any notification requests.
	 */
	   case MH_CLIENT:
		client = (struct mh_client *) msg->m_data;
		if (client->mh_evtype == MH_CE_DISCONNECT)
			dap_Cancel (client->mh_client, tm);
		break;

	   default:
	   	ui_printf ("Unknown MESSAGE proto msg %d\n", tm->mh_type);
		break;
	}
}




static void
ds_message (from, dt)
char *from;
struct dsp_Template *dt;
/*
 * Deal with an incoming data store protocol message.
 */
{
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
	   	dp_UpdateFile ((struct dsp_UpdateFile *) dt);
		break;
	/*
	 * Delete -- get rid of data.
	 */
	   case dpt_DeleteData:
	   	dp_DeleteData (PTable+((struct dsp_DeleteData *) dt)->dsp_plat,
				((struct dsp_DeleteData *) dt)->dsp_leave);
		break;
	/*
	 * Application notification details.
	 */
	   case dpt_NotifyRequest:
	   	dap_Request (from, (struct dsp_NotifyRequest *) dt);
		break;

	   case dpt_CancelNotify:
	   	dap_Cancel (from, dt);
		break;

	   case dpt_CopyNotifyReq:
	   	dap_Copy (from);
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
	DataFile *new;
/*
 * Get a new file entry to give back to them.  Make sure there isn't already
 * a tempfile sitting there from some other time -- if so we reuse it and
 * gripe.
 */
	if (PTable[request->dsp_plat].dp_Tfile)
	{
		msg_ELog (EF_PROBLEM, "Plat %s reusing tfile!", 
			PTable[request->dsp_plat].dp_name);
		new = DFTable + PTable[request->dsp_plat].dp_Tfile;
	}
	else if ((new = dt_NewFile ()) == 0)
	{
		response.dsp_type = dpt_R_NewFileFailure;
		msg_send (from, MT_DATASTORE, FALSE, &response,
				sizeof (response));
		return;
	}
/*
 * Fill in the info and hook it into the platform.
 */
	ShmLock ();
	strcpy (new->df_name, request->dsp_file);
	new->df_begin = new->df_end = request->dsp_time;
	new->df_rev = 0;
	new->df_FLink = new->df_BLink = new->df_nsample = 0;
	new->df_platform = request->dsp_plat;
	new->df_ftype = PTable[request->dsp_plat].dp_ftype;
	PTable[request->dsp_plat].dp_Tfile = new - DFTable;
	ShmUnlock ();
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
dp_UpdateFile (request)
struct dsp_UpdateFile *request;
/*
 * Deal with a file update notification.
 */
{
	DataFile *df = DFTable + request->dsp_FileIndex;
	Platform *plat = PTable + df->df_platform;
/*
 * Mark the changes in the datafile entry.  Update the rev count so that
 * reader processes know things have changed.
 */
	ShmLock ();
	df->df_end = request->dsp_EndTime;
	df->df_nsample += request->dsp_NSamples;
	df->df_rev++;
/*
 * If this file is in the Tfile slot, now we move it over to the localdata
 * list.
 */
	if (request->dsp_FileIndex == plat->dp_Tfile)
	{
		plat->dp_Tfile = 0;
		dt_AddToPlatform (plat, df, TRUE);
	}
	ShmUnlock ();
/*
 * Now we do data available notifications.
 */
	dap_Notify (df->df_platform, &df->df_end);
}





static void
dp_DeleteData (p, sub)
Platform *p;
int sub;
/*
 * Handle the delete data request.
 */
{
	int index = LOCALDATA (*p), last;
	time t;
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
	if (p->dp_flags & DPF_SUBPLATFORM)
	{
		msg_ELog (EF_PROBLEM, "Attempted DeleteData on subplat %s",
			p->dp_name);
		return;
	}
/*
 * Calculate the time before which all data is zapped.
 */
	t = DFTable[index].df_end;
	TC_SysToFcc (TC_FccToSys (&t) - sub, &t);
	msg_ELog (EF_DEBUG, "ZAP before %d %06d", t.ds_yymmdd, t.ds_hhmmss);
/*
 * Now we go through and remove every file which ends before this time.
 */
	last = index;
	ShmLock ();
	while (index)
	{
		DataFile *df = DFTable + index;
	/*
	 * If this file is too new, go on to the next one.
	 */
	 	if (DLT (t, df->df_end))
		{
			last = index;
			index = df->df_FLink;
			continue;
		}
	/*
	 * OK, this one goes.  Move the index forward NOW, then zap this
	 * entry.  NOTE: since DF entries are in reverse time order, we 
	 * know that once we've found the one to zap, we'll get them all
	 * from here on out.  So we leave "last" pointing at the last
	 * entry that will remain when we're done.
	 */
	 	msg_ELog (EF_DEBUG, "Zap %d (%s)", index, df->df_name);
		if (index == p->dp_LocalData)
			index = p->dp_LocalData = df->df_FLink;
		else
			index = DFTable[last].df_FLink = df->df_FLink;
		ZapDF (df);
	}
	ShmUnlock ();
}




static void
ZapDF (df)
DataFile *df;
/*
 * Make this file go away.
 */
{
	struct dsp_DataGone dg;
/*
 * Broadcast a notification to the world.
 */
	dg.dsp_type = dpt_DataGone;
	dg.dsp_file = df - DFTable;
	msg_send ("DataStore", MT_DATASTORE, TRUE, &dg, sizeof (dg));
/*
 * Make sure we have the file closed, then delete it and it's datafile
 * entry.
 */
	dfa_ForceClose (df - DFTable);
	if (unlink (df->df_name) < 0)
		msg_ELog (EF_PROBLEM, "Error %d unlinking '%s'", errno,
			df->df_name);
	dt_FreeDFE (df);
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
	tl_AddRelativeEvent (ExecEvery, (void *) NEvery, interval, interval);
	ECmds[NEvery++] = usy_string (UPTR (cmds[1]));
}
	



static void
ExecEvery (t, slot)
time *t;
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
/*
 * Figure out how much data remains.
 */
	seconds = (cmds->uc_ctype == UTT_KW) ? -1 : UINT (*cmds);
/*
 * If they gave a specific platform, do it now.
 */
	if (cmds[1].uc_ctype != UTT_KW)
	{
		if (! (p = dt_FindPlatform (UPTR (cmds[1]), FALSE)))
			msg_ELog (EF_PROBLEM, "TRUNCATE on bad platform %s",
				UPTR (cmds[1]));
		else
			dp_DeleteData (p, seconds > 0 ? seconds : p->dp_keep);
		return;
	}
/*
 * Otherwise we go through the list and do everything.
 */
	for (plat = 0; plat < ShmHeader->sm_nPlatform; plat++)
		if (! (PTable[plat].dp_flags & DPF_SUBPLATFORM))
			dp_DeleteData (PTable + plat,
				seconds > 0 ? seconds : PTable[plat].dp_keep);
}
