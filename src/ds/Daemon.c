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

# define DS_DAEMON

# include <sys/types.h>
# ifdef SVR4
#    include <sys/statvfs.h>
# else
#    include <sys/vfs.h>
# endif
# include <fcntl.h>
# include <errno.h>
# include <copyright.h>
# include "config.h"
# include "defs.h"
# include "message.h"
# include "timer.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dsDaemon.h"
# include "commands.h"
MAKE_RCSID ("$Id: Daemon.c,v 3.29 1993-11-02 20:34:49 corbet Exp $")



/*
 * Local forwards.
 */
static int 	msg_Handler FP ((struct message *));
static int	ui_Handler FP ((int, struct ui_command *));
static void	mh_message FP ((struct message *));
static void	ds_message FP ((char *, struct dsp_Template *));
static void	dp_NewFile FP ((char *, struct dsp_CreateFile *));
static void	dp_AbortNewFile FP ((struct dsp_AbortNewFile *));
static void	dp_UpdateFile FP ((char *, struct dsp_UpdateFile *));
static void	dp_DeleteData FP ((Platform *, ZebTime *));
static void	dp_DeleteObs FP ((Platform *, ZebTime *));
static void	ZapDF FP ((DataFile *));
static void	SetUpEvery FP ((struct ui_command *));
static void	ExecEvery FP ((ZebTime *, int));
static void	Truncate FP ((struct ui_command *));
static int	FreeSpace FP ((int, SValue *, int *, SValue *, int *));
static int	BCHandler FP ((int, char *, int));
static void	BCSetup FP ((char *, int));
static void	RemDataGone FP ((struct dsp_BCDataGone *));
static void 	SendNPlat FP ((char *));
static void	SendPlatStruct FP ((char *, struct dsp_GetPlatStruct *));
static void	SendFileStruct FP ((char *, int));
static void	LockPlatform FP ((char *, PlatformId));
static void	UnlockPlatform FP ((char *, PlatformId, int));
static void	WriteLock FP ((char *, PlatformId));
static void	ReleaseWLock FP ((char *, PlatformId, int));
static int	AwaitUnlock FP ((Message *, int));
static Lock	*GetLockEntry FP ((void));
static void	DoLookup FP ((char *, char *));
static void	FindDF FP ((char *, struct dsp_FindDF *));
static void	FindAfter FP ((char *, struct dsp_FindDF *));
static int	QueryHandler FP ((char *));

/*
 * Public forwards
 */
void		Shutdown FP ((void));
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

bool InitialScan = TRUE;

/*
 * Caching options.
 */
int LDirConst = FALSE;	/* Nothing changes		*/
int RDirConst = FALSE;
int LFileConst = FALSE;	/* Files don't change (but they	*/
int RFileConst = FALSE;	/* can come and go)		*/
int CacheOnExit = FALSE;	/* Write cache on way out?	*/

/*
 * Memory allocation options.
 */
int PTableSize = 200;	/* Platform table initial size	*/
int PTableGrow = 50;	/* Amount to grow by		*/
int DFTableSize = 2000;	/* Data file table size		*/
int DFTableGrow = 500;	/* Amount to grow by		*/


int DisableRemote = FALSE;




main (argc, argv)
int argc;
char **argv;
{
	char loadfile[80];
	int argt = SYMT_STRING;
	stbl vtable;
/*
 * Hook into the message system.
 */
	msg_connect (msg_Handler, DS_DAEMON_NAME);
	msg_join ("Client events");
	msg_DeathHandler (Shutdown);
	msg_SetQueryHandler (QueryHandler);
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
	strcpy (DefDataDir, DATADIR);
	RemDataDir[0] = '\0';
	usy_c_indirect (vtable, "datadir", DefDataDir, SYMT_STRING, DDIR_LEN);
	usy_c_indirect (vtable, "remdatadir", RemDataDir, SYMT_STRING,
			DDIR_LEN);
	usy_c_indirect (vtable, "DisableRemote", &DisableRemote, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "LDirConst", &LDirConst, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "RDirConst", &RDirConst, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "LFileConst", &LFileConst, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "RFileConst", &RFileConst, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "CacheOnExit", &CacheOnExit, SYMT_BOOL, 0);
/*
 * Indirects for the tables too.
 */
	usy_c_indirect (vtable, "PTableSize", &PTableSize, SYMT_INT, 0);
	usy_c_indirect (vtable, "PTableGrow", &PTableGrow, SYMT_INT, 0);
	usy_c_indirect (vtable, "DFTableSize", &DFTableSize, SYMT_INT, 0);
	usy_c_indirect (vtable, "DFTableGrow", &DFTableGrow, SYMT_INT, 0);
/*
 * Other initialization.
 */
/*	InitSharedMemory (); */
/*	dt_InitTables (); */
	dap_Init ();
	uf_def_function ("freespace", 1, &argt, FreeSpace);
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
	ZebTime t1, t2;
	int type;
	SValue v;
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
	msg_ELog (EF_INFO, "Starting file scan");
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
	   	WriteCache (cmds + 1);
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
	/* ShmCleanup (); */
	exit (0);
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
	int i;

	switch (tm->mh_type)
	{
	   case MH_SHUTDOWN:
		if (CacheOnExit)
			WriteCache (0);
		ui_finish ();
		exit (1);
	/*
	 * For client events, we are really only interested in deaths, so
	 * that we can get rid of any notification requests.
	 */
	   case MH_CLIENT:
		client = (struct mh_client *) msg->m_data;
		if (client->mh_evtype == MH_CE_DISCONNECT)
		{
			dap_Cancel (client->mh_client,
					(struct dsp_Template *) tm);
			for (i = 0; i < NPlatform; i++)
			{
				if (PTable[i].dp_RLockQ)
					UnlockPlatform (client->mh_client,i,0);
				if (PTable[i].dp_WLockQ)
					ReleaseWLock (client->mh_client,i,0);
			}
		}
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
	struct dsp_MarkArchived *dma;
	struct dsp_ProtoVersion dpv;

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
	   	dp_DeleteData (PTable+((struct dsp_DeleteData *) dt)->dsp_plat,
				&((struct dsp_DeleteData *) dt)->dsp_when);
		break;
	/*
	 * DeleteObs -- get rid of observation at given time.
	 */
	   case dpt_DeleteObs:
		dp_DeleteObs (PTable+((struct dsp_DeleteData *) dt)->dsp_plat,
			      &((struct dsp_DeleteData *) dt)->dsp_when);
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
	 * A file is archived.
	 */
	   case dpt_MarkArchived:
	   	dma = (struct dsp_MarkArchived *) dt;
		DFTable[dma->dsp_FileIndex].df_flags |= DFF_Archived;
		break;
	/*
	 * Do a rescan.
	 */
	   case dpt_Rescan:
	   	Rescan ((struct dsp_Rescan *) dt);
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
	ClearLocks (PTable + request->dsp_plat);
	strcpy (new->df_name, request->dsp_file);
	new->df_begin = new->df_end = request->dsp_time;
	new->df_rev = 0;
	new->df_FLink = new->df_BLink = new->df_nsample = 0;
	new->df_platform = request->dsp_plat;
	new->df_ftype = PTable[request->dsp_plat].dp_ftype;
	PTable[request->dsp_plat].dp_Tfile = new - DFTable;
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
	int append = FALSE;
	struct dsp_FileStruct ack;
/*
 * Mark the changes in the datafile entry.  Update the rev count so that
 * reader processes know things have changed.
 */
	msg_ELog (EF_DEBUG, "Update file %d plat %s, ns %d, ow %d last %d", 
		request->dsp_FileIndex, plat->dp_name, request->dsp_NSamples,
		request->dsp_NOverwrite, request->dsp_Last);
	ClearLocks (plat);
	if (TC_Less (df->df_end, request->dsp_EndTime))
	{
		df->df_end = request->dsp_EndTime;
		if (request->dsp_FileIndex == LOCALDATA (*plat) ||
		    request->dsp_FileIndex == plat->dp_Tfile)
		    	append = TRUE;
	}
	df->df_nsample += request->dsp_NSamples;
	df->df_rev = dfa_GetRevision (plat, df);
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
		dt_AddToPlatform (plat, df, TRUE);
	}
/*
 * Where last=TRUE, the client ends up receiving the DFE twice.  Send the
 * CacheInvalidate first so that the client sending this FileUpdate will
 * process it before its UpdateAck message.  The other clients won't be
 * affected by receiving the invalidate here rather than after sending
 * the UpdateAck to client with the write lock.
 */
	if (request->dsp_Last)
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
		/* CacheInvalidate (df - DFTable); */
		dap_Notify (df->df_platform, &df->df_end, plat->dp_NewSamps,
				plat->dp_OwSamps, append);
		plat->dp_NewSamps = plat->dp_OwSamps = 0;
	}
}





static void
dp_DeleteData (p, t)
Platform *p;
ZebTime *t;
/*
 * Handle the delete data request.
 */
{
	int index = LOCALDATA (*p);
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
#ifdef notdef	
		/* 
		 * We need to use dt_RemoveDFE() instead of the code below so
		 * that CacheInvalidate messages are sent as needed.
		 */
		        if (index == p->dp_LocalData)
				index = p->dp_LocalData = df->df_FLink;
			else
				index = DFTable[last].df_FLink = df->df_FLink;
#endif
		        msg_ELog (EF_DEBUG, "DeleteData: zap %d (%s)", 
				  df->df_index, df->df_name);
		        ZapDF (df);
		        dt_RemoveDFE (p, df - DFTable);
		        p->dp_flags |= DPF_DIRTY;
	        }
	}
}




static void
dp_DeleteObs (p, t)
Platform *p;
ZebTime *t;
/*
 * Handle the request to delete a single, whole observation.
 */
{
	int index = LOCALDATA (*p);
	DataFile *df;
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
	if (p->dp_flags & DPF_SUBPLATFORM)
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
	if ((plat = dt_FindPlatform (dg->dsp_Plat, TRUE)) == 0)
		return;
/*
 * Construct the file name from our perspective.
 */
	for (dfindex = REMOTEDATA (*plat); dfindex;
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
 * Make sure we have the file closed ourselves
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
/*
 * Let everyone know this file is going away.
 */
	DataFileGone (df);
/*
 * Unlink the file from in the filesystem.
 */
	if (! (df->df_flags & DFF_Remote))
		if (unlink (dfa_FilePath (PTable + df->df_platform, df)) < 0)
			msg_ELog (EF_PROBLEM, "Error %d unlinking '%s'", errno,
				df->df_name);
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
	tl_RelativeReq (ExecEvery, (void *) NEvery, interval, interval);
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
		if (! (p = dt_FindPlatform (UPTR (cmds[1]), FALSE)))
			msg_ELog (EF_PROBLEM, "TRUNCATE on bad platform %s",
				UPTR (cmds[1]));
		else
		{
			zaptime = now;
			zaptime.zt_Sec -= (seconds > 0) ? seconds : p->dp_keep;
			dp_DeleteData (p, &zaptime);
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
					PTable[plat].dp_keep;
			dp_DeleteData (PTable + plat, &zaptime);
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
#ifndef SVR4
	struct statfs sfs;
#else
	struct statvfs sfs;
#endif
/*
 * Stat the file system.
 */
#ifndef SVR4
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
	answer.dsp_plat = PTable[req->dsp_pid];
	msg_send (to, MT_DATASTORE, FALSE, &answer, sizeof (answer));
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
	Platform *p = PTable + which;
	struct dsp_PLock answer;
/*
 * Do some testing for a case we are not prepared to handle.  Multiple locks
 * will work *most* of the time, so long as they unlock the same number of
 * times.  But if the daemon is waiting for an unlock on this platform, 
 * things will hang.
 */
	if ((which < 0) || (which > NPlatform))
	{
		msg_ELog (EF_PROBLEM, 
			  "Read lock attempt for invalid plat id %d", which);
		return;
	}
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
 * Release a lock on this platform.
 */
{
	Lock *lp, *zap;
	Platform *p = PTable + which;

	if ((which < 0) || (which > NPlatform))
	{
		msg_ELog (EF_PROBLEM, 
			  "Read lock release for invalid plat id %d", which);
		return;
	}
	msg_ELog (EF_DEBUG, "Lock on %s released by %s", p->dp_name, who);
/*
 * Find the structure for the lock held by this process and remove it.
 */
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
/*
 * Put the zapped entry onto the free list.
 */
	zap->l_Next = FreeLocks;
	FreeLocks = zap;
}




void
ClearLocks (p)
Platform *p;
/* 
 * Wait until all locks on this platform are clear.
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
	   case dpt_FindDF:
	   case dpt_FindAfter:
	   case dpt_Hello:
	   case dpt_LookupPlatform:
	   	ds_message (msg->m_from, dt);
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
	Platform *p = dt_FindPlatform (plat, FALSE);
	struct dsp_PID answer;

	answer.dsp_type = dpt_R_PID;
	answer.dsp_pid = p ? (p - PTable) : BadPlatform;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}





static void
FindDF (who, req)
char *who;
struct dsp_FindDF *req;
/*
 * Find a data file entry based on time.
 */
{
	int dfe = 0;
	struct dsp_R_DFI answer;
/*
 * Search the platform's local list first.
 */
	if (req->dsp_src <= 0)
	{
		for (dfe = LOCALDATA (PTable[req->dsp_pid]); dfe;
					dfe = DFTable[dfe].df_FLink)
			if (TC_LessEq (DFTable[dfe].df_begin, req->dsp_when))
				break;
	}
/*
 * If we didn't find the data locally, see if there's anything in the
 * remote data table.
 */
	if (! dfe && (req->dsp_src == 1 || req->dsp_src == SRC_ALL))
	{
		for (dfe = REMOTEDATA (PTable[req->dsp_pid]); dfe;
				dfe = DFTable[dfe].df_FLink)
			if (TC_LessEq (DFTable[dfe].df_begin, req->dsp_when))
				break;
	}
/*
 * Now return our answer.
 */
	answer.dsp_type = dpt_R_DFIndex;
	answer.dsp_index = (dfe) ? dfe : -1;
	msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
}







static void
FindAfter (who, req)
char *who;
struct dsp_FindDF *req;
/*
 * Find the closest data file containing data after the given time.
 */
{
	int dfe = LOCALDATA (PTable[req->dsp_pid]), last = 0, rlast = 0;
	struct dsp_R_DFI answer;
/*
 * Search the local list for the first DFE which does *not* end after
 * the given time.
 */
	for (; dfe; dfe = DFTable[dfe].df_FLink)
	{
		if (TC_LessEq (DFTable[dfe].df_end, req->dsp_when))
			break;
		last = dfe;
	}
/*
 * If we didn't find the data locally, see if there's anything in the
 * remote data table.
 */
	if (! dfe)
		for (dfe = REMOTEDATA (PTable[req->dsp_pid]); dfe;
					dfe = DFTable[dfe].df_FLink)
		{
			if (TC_LessEq (DFTable[dfe].df_end, req->dsp_when))
				break;
			rlast = last;
		}
/*
 * If we found a DFE, it is a file *before* the time, so we need to back 
 * up one, if something is there.
 */
	if (dfe)
		dfe = DFTable[dfe].df_BLink;
/*
 * Now we need to pick out what we really send back.
 */
	if (dfe)
		answer.dsp_index = dfe;
	else if (last && (rlast == 0 ||
		TC_LessEq (DFTable[last].df_begin, DFTable[rlast].df_begin)))
		answer.dsp_index = last;
	else
		answer.dsp_index = (rlast != 0) ? rlast : -1;
/*
 * Return our answer.
 */
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
		msg.dsp_type = dpt_CacheInvalidate;
		msg.dsp_dfe = DFTable[index];
		msg_send ("DataStore", MT_DATASTORE, TRUE, &msg, sizeof (msg));
	}
}





static int
QueryHandler (who)
char *who;
/*
 * Handle a zquery.
 */
{
	char buf[120];

	msg_AnswerQuery (who, "Zeb data store daemon");
	sprintf (buf, "%d Platforms used of %d allocated", NPlatform,
		PTableSize);
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%d DataFile entries used of %d", NDTEUsed,
		DFTableSize);
	msg_AnswerQuery (who, buf);
	msg_FinishQuery (who);
}







static void
WriteLock (who, which)
char *who;
PlatformId which;
/*
 * Write lock platform WHICH for WHO.
 */
{
	Lock *lp;
	Platform *p = PTable + which;
	struct dsp_PLock answer;
/*
 * Fill in the lock structure.  Avoid the overhead for the time field, at
 * least until experience shows that we need it.
 */
	lp = GetLockEntry ();
	strcpy (lp->l_Owner, who);
	/* tl_Time (lp->l_When); */
/*
 * Now...if there is no lock active we can add this one and send the
 * answer back.
 */
	if (! p->dp_WLockQ)
	{
		lp->l_Next = 0;
		p->dp_WLockQ = lp;
		answer.dsp_type = dpt_R_PLockGranted;
		answer.dsp_pid = which;
		msg_send (who, MT_DATASTORE, FALSE, &answer, sizeof (answer));
		msg_ELog (EF_DEBUG, 
			  "Write lock on %s granted to %s",p->dp_name, who);
	}
/*
 * Otherwise the request needs to be enqueued.  We do this by putting it 
 * after the current lock.  NOTE that this does NOT implement proper
 * sequential ordering of locking, but it will be so rare that three processes
 * want locks at the same time that I don't think it is worth worrying
 * about.
 *
 *
 * do those sound like famous last words or what?
 */
	else
	{
		msg_ELog (EF_DEBUG, 
			  "%s waiting for write lock on %s", who, p->dp_name);
		lp->l_Next = p->dp_WLockQ->l_Next;
		p->dp_WLockQ->l_Next = lp;
	}
}




static void
ReleaseWLock (who, which, expect)
char *who;
PlatformId which;
int expect;
/*
 * Release a lock on this platform.
 */
{
	Lock *lp, *zap;
	Platform *p = PTable + which;
/*
 * Make sure there's a lock and that it's the owner doing the releasing.  
 * (We get called rather indiscriminately when a client disconnect occurs, 
 * so this test is necessary)
 */
	if (! p->dp_WLockQ || strcmp (p->dp_WLockQ->l_Owner, who))
		return;
/*
 * Pull the zapped entry off and put it on the free list.
 */
	zap = p->dp_WLockQ;
	p->dp_WLockQ = zap->l_Next;
	zap->l_Next = FreeLocks;
	FreeLocks = zap;

	msg_ELog (EF_DEBUG, "Write lock on %s released by %s",p->dp_name, who);
/*
 * If there is somebody else waiting for a lock, grant it.
 */
 	if (p->dp_WLockQ)
	{
		struct dsp_PLock answer;
		answer.dsp_type = dpt_R_PLockGranted;
		answer.dsp_pid = which;
		msg_send (p->dp_WLockQ->l_Owner, MT_DATASTORE, FALSE,
				&answer, sizeof (answer));
	}
}

