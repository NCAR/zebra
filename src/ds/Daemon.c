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
# include <sys/vfs.h>
# include <dirent.h>
# include <errno.h>
# include <copyright.h>
# include "../include/config.h"
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dsDaemon.h"
# include "commands.h"
MAKE_RCSID ("$Id: Daemon.c,v 3.3 1992-07-15 17:14:22 corbet Exp $")



/*
 * Local forwards.
 */
static int 	msg_Handler FP ((struct message *));
static int	ui_Handler FP ((int, struct ui_command *));
void		Shutdown FP ((void));
static void	DataScan FP ((void));
static void	ScanDirectory FP ((Platform *, int, int));
static void	ScanFile FP ((Platform *, char *, char *, int, int));
static void	mh_message FP ((struct message *));
static void	ds_message FP ((char *, struct dsp_Template *));
static void	dp_NewFile FP ((char *, struct dsp_CreateFile *));
static void	dp_AbortNewFile FP ((struct dsp_AbortNewFile *));
static void	dp_UpdateFile FP ((char *, struct dsp_UpdateFile *));
static void	dp_DeleteData FP ((Platform *, int));
static void	ZapDF FP ((DataFile *));
static void	SetUpEvery FP ((struct ui_command *));
static void	ExecEvery FP ((ZebTime *, int));
static void	Truncate FP ((struct ui_command *));
static int	FreeSpace FP ((int, SValue *, int *, SValue *, int *));
static int	BCHandler FP ((int, char *, int));
static void	BCSetup FP ((char *, int));
static void	RemDataGone FP ((struct dsp_BCDataGone *));
static void	Rescan FP ((struct dsp_Rescan *));
static void	RescanPlat FP ((Platform *));
static int	FileKnown FP ((Platform *, char *, char *, int));
static void	CleanChain FP ((Platform *, int));

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


int DisableRemote = FALSE;




main (argc, argv)
int argc;
char **argv;
{
	char loadfile[80];
	int argt = SYMT_STRING;
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
	SetupConfigVariables ();
/*
 * Initialize.
 */
	strcpy (DefDataDir, DATADIR);
	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "datadir",
		DefDataDir, SYMT_STRING, 80);
	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "DisableRemote",
		&DisableRemote, SYMT_BOOL, 0);
	InitSharedMemory ();
	dt_InitTables ();
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
	/*
	 * Don't scan subplatforms.
	 */
		if (p->dp_flags & DPF_SUBPLATFORM)
			continue;
	/*
	 * Scan the local directory, and the local one if it exists.
	 */
		ScanDirectory (p, TRUE, FALSE);
		if (p->dp_flags & DPF_REMOTE)
			ScanDirectory (p, FALSE, FALSE);
	}
}




static void
ScanDirectory (p, local, rescan)
Platform *p;
bool local, rescan;
/*
 * Scan a directory for this platform.
 */
{
	char *dir = local ? p->dp_dir : p->dp_rdir;
	DIR *dp = opendir (dir);
	struct dirent *ent;
/*
 * Make sure there really is a directory.
 */
	if (! dp)
	{
		msg_ELog (EF_PROBLEM,
			"Data dir %s (plat %s) nonexistent", dir, p->dp_name);
		return;
	}
/*
 * Go through the files.
 */
	while (ent = readdir (dp))
		ScanFile (p, dir, ent->d_name, local, rescan);
	closedir (dp);
}





static void
ScanFile (p, dir, file, local, rescan)
Platform *p;
char *file, *dir;
bool local, rescan;
/*
 * Look at this file and see what we think of it.
 */
{
	DataFile *df;
	int ns;
	char abegin[40], aend[40];
/*
 * If DFA doesn't recognize it, we don't even bother.
 */
	if (! dfa_CheckName (p->dp_ftype, file))
		return;
/*
 * If this is a rescan, check to see if we already know about this file.
 */
	if (rescan && FileKnown (p, dir, file, local))
		return;
/*
 * Grab a new datafile entry and begin to fill it in.
 */
	if (! (df = dt_NewFile ()))
		return;	/* bummer */
	sprintf (df->df_name, "%s/%s", dir, file);
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
/*
 * Debugging is nice sometimes.
 */
 	TC_EncodeTime (&df->df_begin, TC_Full, abegin);
	TC_EncodeTime (&df->df_end, TC_TimeOnly, aend);
	msg_ELog (EF_DEBUG, "%c File '%s', %s to %s ns %d",
		local ? 'L' : 'C', file, abegin, aend, df->df_nsample);
# ifdef notdef
	TC_UIToZt (&begin, &df->df_begin);
	TC_UIToZt (&end, &df->df_end);
# endif
/*
 * Finish the fillin and add it to this platform's list.
 */
	df->df_ftype = p->dp_ftype;
	dt_AddToPlatform (p, df, local);
}





static int
FileKnown (p, dir, file, local)
Platform *p;
char *dir, *file;
bool local;
/*
 * See if we already know about this file.
 */
{
	int dfi = local ? LOCALDATA (*p) : REMOTEDATA (*p);
	int dirlen = strlen (dir) + 1;
/*
 * Just pass through the list and see if we find it.
 */
	for (; dfi; dfi = DFTable[dfi].df_FLink)
		if (! strcmp (DFTable[dfi].df_name + dirlen, file))
		{
			msg_ELog (EF_DEBUG, "File %s known dfi %d", file, dfi);
			DFTable[dfi].df_flags |= DFF_Seen;
			return (TRUE);
		}
	msg_ELog (EF_DEBUG, "New file %s/%s", p->dp_name, file);
	return (FALSE);
}







static void
Rescan (req)
struct dsp_Rescan *req;
/*
 * Implement the rescan request.
 */
{
	if (req->dsp_all)
	{
		int plat;
		for (plat = 0; plat < ShmHeader->sm_nPlatform; plat++)
		{
			Platform *p = PTable + plat;
		/*
		 * Don't scan subplatforms.
		 */
			if ((p->dp_flags & DPF_SUBPLATFORM) == 0)
				RescanPlat (p);
		}
	}
	else
		RescanPlat (PTable + req->dsp_pid);
}




static void
RescanPlat (p)
Platform *p;
/*
 * Rescan the files for this platform.
 */
{
	int dfindex;
/*
 * Go through and clear the "seen" flags.
 */
	for (dfindex = LOCALDATA (*p); dfindex;
				dfindex = DFTable[dfindex].df_FLink)
		DFTable[dfindex].df_flags &= ~DFF_Seen;
	for (dfindex = REMOTEDATA (*p); dfindex;
				dfindex = DFTable[dfindex].df_FLink)
		DFTable[dfindex].df_flags &= ~DFF_Seen;
/*
 * Rescan the directory(ies).
 */
	ScanDirectory (p, TRUE, TRUE);
	if (p->dp_flags & DPF_REMOTE)
		ScanDirectory (p, FALSE, TRUE);
/*
 * Now get rid of anything that has disappeared.
 */
	CleanChain (p, LOCALDATA (*p));
	CleanChain (p, REMOTEDATA (*p));
}





static void
CleanChain (p, chain)
Platform *p;
int chain;
/*
 * Get rid of vanished files in this chain.
 */
{
	int dfi, next;

	for (dfi = chain; dfi; dfi = next)
	{
		next = DFTable[dfi].df_FLink;
		if ((DFTable[dfi].df_flags & DFF_Seen) == 0)
		{
			msg_ELog (EF_DEBUG, "File %s disappeared",
				DFTable[dfi].df_name);
			dt_RemoveDFE (p, dfi);
		}
	}
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
			dap_Cancel (client->mh_client,
					(struct dsp_Template *) tm);
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
	struct dsp_Template ack;
/*
 * Mark the changes in the datafile entry.  Update the rev count so that
 * reader processes know things have changed.
 */
	msg_ELog (EF_DEBUG, "Update file %d plat %s, ns %d, ow %d last %d", 
		request->dsp_FileIndex, plat->dp_name, request->dsp_NSamples,
		request->dsp_NOverwrite, request->dsp_Last);
	ShmLock ();
	if (TC_Less (df->df_end, request->dsp_EndTime))
	{
		df->df_end = request->dsp_EndTime;
		if (request->dsp_FileIndex == LOCALDATA (*plat) ||
		    request->dsp_FileIndex == plat->dp_Tfile)
		    	append = TRUE;
	}
	df->df_nsample += request->dsp_NSamples;
	df->df_rev++;
	plat->dp_NewSamps += request->dsp_NSamples;
	plat->dp_OwSamps += request->dsp_NOverwrite;
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
 * Send an ack back to the updating process.
 */
	ack.dsp_type = dpt_R_UpdateAck;
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
dp_DeleteData (p, sub)
Platform *p;
int sub;
/*
 * Handle the delete data request.
 */
{
	int index = LOCALDATA (*p), last;
	ZebTime t;
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
	TC_SysToZt (TC_ZtToSys (&t) - sub, &t);
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
	 	if (TC_Less (t, df->df_end))
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
RemDataGone (dg)
struct dsp_BCDataGone *dg;
/*
 * A remotely accessible data file has gone away.
 */
{
	Platform *plat;
	char fname[200];
	int dfindex;
	DataFile *df;
/*
 * Make sure this is a platform we know about.
 */
	if ((plat = dt_FindPlatform (dg->dsp_Plat, TRUE)) == 0)
		return;
/*
 * Construct the file name from our perspective.
 */
	sprintf (fname, "%s/%s", plat->dp_rdir, dg->dsp_File);
	for (dfindex = REMOTEDATA (*plat); dfindex;
			dfindex = DFTable[dfindex].df_FLink)
		if (! strcmp (fname, DFTable[dfindex].df_name))
			break;
	if (! dfindex)
	{
		msg_ELog (EF_INFO, "BCDataGone on nonex file '%s'", fname);
		return;
	}
/*
 * Now we get rid of it.
 */
	df = DFTable + dfindex;
	if (df->df_BLink)
		DFTable[df->df_BLink].df_FLink = df->df_FLink;
	if (df->df_FLink)
		DFTable[df->df_FLink].df_BLink = df->df_BLink;
	ZapDF (df);
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
 * If we are broadcasting to other worlds, we do that too.
 */
	if (BCastSocket > 0)
	{
		struct dsp_BCDataGone dg;
		dg.dsp_type = dpt_BCDataGone;
		strcpy (dg.dsp_Plat, PTable[df->df_platform].dp_name);
		strcpy (dg.dsp_File, df->df_name + 
			strlen (PTable[df->df_platform].dp_dir) + 1);
		msg_BCast (BCastSocket, &dg, sizeof (dg));
	}
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




static int
FreeSpace (narg, argv, argt, rv, rt)
int narg, *argt, *rt;
union usy_value *argv, *rv;
/*
 * [Command line function] return the amount of the disk that is free.
 */
{
	struct statfs sfs;
/*
 * Stat the file system.
 */
	if (statfs (argv->us_v_ptr, &sfs))
		msg_ELog (EF_PROBLEM, "Statfs failed, errno %d", errno);
/*
 * Calculate the return value.
 */
	rv->us_v_int = sfs.f_bavail;
	*rt = SYMT_INT;
}
