/*
 * The data store application interface.
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

#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <defs.h>
#include <zl_symbol.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "dsPrivate.h"
#include "Platforms.h"
#include "dslib.h"
#include "Appl.h"

RCSID ("$Id: Appl.c,v 3.52 2001-10-16 22:26:28 granger Exp $")

/*
 * Notification callbacks are void functions
 */
typedef void (*VFunc) ();

/*
 * Local private prototypes.
 */
static const DataFile *ds_FindFile (PlatformId pid, const ZebraTime *when,
				    int srcid, int before);
static const DataFile *ds_FindDFLink (const DataFile *df, int prev);
static void     ds_DispatchNotify FP ((struct dsp_Notify *));
static int	ds_AwaitDF (Message *msg, DataFile *df);
static int	ds_AwaitSrcInfo (Message *msg, struct dsp_R_SrcInfo *answer);
static int	ds_AwaitPlatDir (Message *msg, struct dsp_R_PlatDir *answer);
static int	ds_GreetDaemon FP ((void));
static int	ds_CheckProtocol FP ((Message *, int *));


/*
 * The application notification table.
 */
static VFunc ApplFuncs[MAXPLAT];
static VFunc CopyFunc = 0;

/*
 * How far in the future we are willing to accept data.  Public only so that
 * DFA_Appl.c has access.
 */
int MaxFuture = 3600;	/* One hour into future	*/

/*
 * True if we are running standalone, with no daemon or timer to help us.
 * Public so that DFA_Appl.c and SA_Appl.c routines have access.  As long
 * as ds_Standalone is not called, SA_Appl.c will not be linked and this
 * value will not change.
 */
int Standalone = 0;

/*
 * The structure which holds pointers to alternate method implementations,
 * such as for standalone mode.
 */
DS_Methods DSM = { NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
		   NULL, NULL, NULL, NULL, NULL, NULL };


static char Daemon[ 2*MSG_MAXNAMELEN ];


static int
ds_Connect ()
/*
 * Set the name of the datastore daemon to which we will send messages.
 * Greet the daemon and join the appropriate groups.  Return non-zero
 * on success.
 */
{
	const char *dhost;
/*
 * Set our protocol handler.
 */
	msg_AddProtoHandler (MT_DATASTORE, ds_DSMessage);
	if ((dhost = getenv ("DS_DAEMON_HOST")) != NULL)
	{
		char group[ 2*MSG_MAXNAMELEN ];
	/*
	 * Connect to a remote daemon and join that session's datastore
	 * group.
	 */
		sprintf (group, "DataStore@%s", dhost);
		msg_join (group);
		strcpy (Daemon, "DS_Daemon@");
		strcat (Daemon, dhost);
	}
	else
	{
	/* 
	 * Connect to the local session's daemon and group.
	 */
		msg_join ("DataStore");
		strcpy (Daemon, "DS_Daemon");
	}
	return (ds_GreetDaemon() == 0);
}



void
ds_InitAPI ()
/*
 * Initialize api, allowing for multiple calls for forked processes
 * or re-connections to possibly different datastores.
 */
{
	int i;

	zl_usy_init ();
	F_Reset ();
	for (i = 0; i < MAXPLAT; i++)
		ApplFuncs[i] = 0;
/*
 * Field derivation init
 */
	ds_DerivInit();
}




int
ds_Initialize()
/*
 * Hook into the data store.
 */
{
	Standalone = 0;
	ds_InitAPI ();
	dt_PlatformClientMode ();
	return (ds_Connect ());
}




int
ds_IsStandalone ()
/*
 * Return our standalone state.  Zero if not standalone, non-zero otherwise.
 */
{
	return (Standalone);
}




const char *
ds_ProtoVersion ()
{
	static char buf[32];
	sprintf (buf, "%#0x", DSProtocolVersion);
	return (buf);
}




static const DataFile*
ds_FindFile (PlatformId pid, const ZebraTime *when, int srcid, int before)
/*
 * Find the first datafile entry at or before (if 'before' is true) or
 * at or after (if 'before is false) this time.  DS private routine.
 * The returned DataFile* is good until the next call to this function.
 */
{
	struct dsp_FindDF req;
	static DataFile df;
/*
 * Just fire off a request to the daemon.
 */
	if (before && DSM.dsm_FindBefore)
	    return ((*DSM.dsm_FindBefore) (pid, when, srcid));
	else if (! before && DSM.dsm_FindAfter)
	    return ((*DSM.dsm_FindAfter) (pid, when, srcid));
	
 	req.dsp_type = before ? dpt_FindDF : dpt_FindAfter;
	req.dsp_pid = pid;
	req.dsp_when = *when;
	req.dsp_srcid = srcid;
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitDF, &df);
	if (df.df_pid == BadPlatform)
	    return (0);
	else
	    return (&df);
}


const DataFile*
ds_FindDFBefore (int srcid, PlatformId pid, const ZebraTime *when)
/*
 * Find the first datafile entry containing data at or before this time.
 *
 * The returned DataFile* is good until the next file find call.
 */
{
    return (ds_FindFile (pid, when, srcid, TRUE));
}
    

const DataFile*
ds_FindDFAfter (int srcid, PlatformId pid, const ZebraTime *when)
/*
 * Find the first datafile entry containing data at or after this time.
 *
 * The returned DataFile* is good until the next file find call.
 */
{
    return (ds_FindFile (pid, when, srcid, FALSE));
}


/*
 * The following two functions are deprecated.  Switch to ds_FindDFBefore()
 * and ds_FindDFAfter(), instead.
 */

const DataFile*
ds_FindBefore (PlatformId pid, const ZebraTime *when)
/*
 * Find the first datafile entry before this time, for all sources.
 *
 * The returned DataFile* is good until the next file find call.
 */
{
    return (ds_FindFile (pid, when, SRC_ALL, TRUE));
}




const DataFile*
ds_FindAfter (PlatformId pid, const ZebraTime *when)
/*
 * Find the first datafile entry containing data after this time, for
 * all sources.
 *
 * The returned DataFile* is good until the next file find call.
 */
{
    return (ds_FindFile (pid, when, SRC_ALL, FALSE));
}




const DataFile*	
ds_FirstFile (int srcid, PlatformId pid)
{
/*
 * Return the first file for the source after ZT_ALPHA (the earliest ZebraTime)
 *
 * The returned DataFile* is good until the next file find call.
 */
    return (ds_FindFile (pid, &ZT_ALPHA, srcid, FALSE));
}



const DataFile*	
ds_LastFile (int srcid, PlatformId pid)
{
/*
 * Return the first file for the source before ZT_OMEGA (the latest ZebraTime)
 *
 * The returned DataFile* is good until the next file find call.
 */
    return (ds_FindFile (pid, &ZT_OMEGA, srcid, TRUE));
}



const DataFile*
ds_FindDFLink (const DataFile *base_df, int prev)
/*
 * Find the datafile previous to or after (based on 'prev') to the given file;
 * return 0 if no such file exists.  The returned DataFile* is good until the 
 * next call to this function.
 */
{
    struct dsp_FindDFLink req;
    static DataFile df;
/*
 * 	Just fire off a request to the daemon.
 */
    if (DSM.dsm_FindDFLink)
	return ((*DSM.dsm_FindDFLink) (base_df, prev));

    req.dsp_type = prev ? dpt_FindDFPrev : dpt_FindDFNext;
    
    req.dsp_file = *base_df;
    ds_SendToDaemon (&req, sizeof (req));
    msg_Search (MT_DATASTORE, ds_AwaitDF, &df);
/*
 * A non-existent file is flagged with a bad platform id
 */
    if (df.df_pid == BadPlatform)
	return (0);
    else
	return (&df);
}



const DataFile*
ds_PrevFile (const DataFile *df)
{
    return (ds_FindDFLink (df, 1));
}

    

const DataFile*
ds_NextFile (const DataFile *df)
{
    return (ds_FindDFLink (df, 0));
}

    

int
ds_GetSourceInfo (int srcid, SourceInfo *si)
/*
 * Return true and fill in the SourceInfo, if there's a source associated with
 * the given id.  Otherwise return false.  If 'si' is NULL, we don't try to 
 * fill it.
 */
{
    struct dsp_GetSrcInfo req;
    struct dsp_R_SrcInfo answer;
/*
 * Just fire off a request to the daemon.
 */
    if (DSM.dsm_GetSrcInfo)
	return ((*DSM.dsm_GetSrcInfo) (srcid, si));

    req.dsp_type = dpt_GetSrcInfo;
    req.dsp_srcid = srcid;
    ds_SendToDaemon (&req, sizeof (req));
    msg_Search (MT_DATASTORE, ds_AwaitSrcInfo, &answer);
/*
 * Return the appropriate response
 */
    if (answer.dsp_success && si)
	*si = answer.dsp_srcinfo;

    return (answer.dsp_success);
}




static int
ds_AwaitDF (Message *msg, DataFile *df)
/*
 * Wait for a data file index to come back.  Mark failure by returning
 * BadPlatform in the df_pid member of the DataFile.
 */
{
	struct dsp_R_DataFile *ans = (struct dsp_R_DataFile *) msg->m_data;

	if (ans->dsp_type == dpt_R_DataFile)
	{
	    if (ans->dsp_success)
		*df = ans->dsp_file;
	    else
		df->df_pid = BadPlatform;

	    return (MSG_DONE);
	}
	return (MSG_ENQUEUE);
}

		


static int
ds_AwaitSrcInfo (Message *msg, struct dsp_R_SrcInfo *ret)
/*
 * Wait for a data file index to come back.  Mark failure by returning
 * BadPlatform in the df_pid member of the DataFile.
 */
{
	struct dsp_R_SrcInfo *ans = (struct dsp_R_SrcInfo *) msg->m_data;

	if (ans->dsp_type == dpt_R_SrcInfo)
	{
	    *ret = *ans;
	    return (MSG_DONE);
	}
	return (MSG_ENQUEUE);
}

		


int
ds_GetPlatDir (int srcid, PlatformId pid, char *dir)
/*
 * Return true and fill in the dir if the given src/plat pair is valid.
 * Otherwise return false.  The dir string must be long enough to hold the
 * returned string (CFG_FILEPATH_LEN is sufficient).
 */
{
    struct dsp_GetPlatDir req;
    struct dsp_R_PlatDir answer;
/*
 * Just fire off a request to the daemon.
 */
    if (DSM.dsm_GetPlatDir)
	return ((*DSM.dsm_GetPlatDir) (srcid, pid, dir));

    req.dsp_type = dpt_GetPlatDir;
    req.dsp_srcid = srcid;
    req.dsp_pid = pid;
    ds_SendToDaemon (&req, sizeof (req));
    msg_Search (MT_DATASTORE, ds_AwaitPlatDir, &answer);
/*
 * Return the appropriate response
 */
    if (answer.dsp_success)
	strcpy (dir, answer.dsp_dir);

    return (answer.dsp_success);
}




static int
ds_AwaitPlatDir (Message *msg, struct dsp_R_PlatDir *ret)
/*
 * Wait for a platform dir to come back.
 */
{
	struct dsp_R_PlatDir *ans = (struct dsp_R_PlatDir *) msg->m_data;

	if (ans->dsp_type == dpt_R_PlatDir)
	{
	    *ret = *ans;
	    return (MSG_DONE);
	}
	return (MSG_ENQUEUE);
}

		


int
ds_DSMessage(msg)
struct message *msg;
/*
 * Handle data store protocol messages which do not require data file
 * access.  Any message passed to this handler must not need file access
 * or the file handling (e.g. closing a file on DataGone) must have already
 * been performed by another handler (e.g. ds_DFAMessage).
 */
{
	struct dsp_Template *dt = (struct dsp_Template *) msg->m_data;
	struct dsp_DataGone *ddg;

	switch (dt->dsp_type)
	{
	    case dpt_DataGone:
		ddg = (struct dsp_DataGone *) dt;
		/*
		 * Either the file was closed by the DFA handler or a close
		 * is not necessary because we never could have opened it.
		 */
#ifdef DEBUG
		msg_ELog (EF_DEBUG, "DataGone received for %s", 
			  ddg->dsp_file.df_core.dfc_name);
#endif /* DEBUG */
		dfa_ForceClose (&(ddg->dsp_file));
		break;
	/*
	 * An application notification has arrived.  Dispatch it back
	 * to the appl.
	 */
	    case dpt_Notify:
		ds_DispatchNotify ((struct dsp_Notify *) dt);
		break;
	/*
	 * If we are getting notification requests, then dispatch
	 * them out to the copy function, which really should ought
	 * to exist.
	 */
	   case dpt_NotifyRequest:
	   case dpt_CancelNotify:
		if (CopyFunc)
			(*CopyFunc) (msg->m_data);
		break;

	   default:
		if (dt->dsp_type != MH_CLIENT)
			msg_ELog (EF_PROBLEM, "Unknown DS message type %d",
				 dt->dsp_type);
		break;
	}
	return (0);
}





void
ds_DeleteData (platform, zaptime)
PlatformId platform;
ZebTime *zaptime;
/*
 * Zap data from this platform -- everything before the given time.
 */
{
	struct dsp_DeleteData del;

	if (DSM.dsm_DeleteData)
	{
		(*DSM.dsm_DeleteData)(platform, zaptime);
		return;
	}
/*
 * Write lock the platform around the deletion just to be sure.
 */
	del.dsp_type = dpt_DeleteData;
	del.dsp_plat = platform;
	del.dsp_when = *zaptime;
	ds_SendToDaemon (&del, sizeof (del));
}






void
ds_DeleteObs (platform, zaptime)
PlatformId platform;
ZebTime *zaptime;
/*
 * Zap the observation containing the given ZAPTIME
 */
{
	struct dsp_DeleteData del;
/*
 * Write lock the platform around the deletion just to be sure.
 */
	if (DSM.dsm_DeleteObs)
	{
		(*DSM.dsm_DeleteObs)(platform, zaptime);
		return;
	}
	del.dsp_type = dpt_DeleteObs;
	del.dsp_plat = platform;
	del.dsp_when = *zaptime;
	ds_SendToDaemon (&del, sizeof (del));
}




void
ds_ForceRescan (platform, all)
PlatformId platform;
int all;
/*
 * Force a rescan of this platform, or all of them if ALL is TRUE.
 */
{
	struct dsp_Rescan req;

	if (Standalone)
		return;
	req.dsp_type = dpt_Rescan;
	req.dsp_pid = platform;
	req.dsp_all = all;
	ds_SendToDaemon (&req, sizeof (req));
}







void
ds_RequestNotify (platform, param, func)
PlatformId platform;
int param;
void (*func) ();
/*
 * Request a notification when data is available on this platform.
 */
{
	struct dsp_NotifyRequest req;
/*
 * Fill in our request and send it off.
 */
	if (Standalone)
		return;
	req.dsp_type = dpt_NotifyRequest;
	req.dsp_pid = platform;
	req.dsp_param = param;
	req.dsp_who[0] = '\0';
	ds_SendToDaemon (&req, sizeof(req));
/*
 * Stash away the function so we can call it when the notifications
 * arrive.
 */
	ApplFuncs[platform] = func;
}





void 
ds_CancelNotify ()
/*
 * Cancel all data available notifications.
 */
{
	struct dsp_NotifyCancel req;
	int i;

	if (Standalone)
		return;
	req.dsp_type = dpt_CancelNotify;
	req.dsp_who[0] = '\0';
	ds_SendToDaemon (&req, sizeof (req));
	for (i = 0; i < MAXPLAT; i++)
		ApplFuncs[i] = 0;
}





static void
ds_DispatchNotify (notify)
struct dsp_Notify *notify;
/*
 * An application notification has arrived -- give it back to those who
 * requested it.
 */
{
	if (ApplFuncs[notify->dsp_pid])
		(*ApplFuncs[notify->dsp_pid]) (notify->dsp_pid,
			       notify->dsp_param, &notify->dsp_when,
			       notify->dsp_nsample, notify->dsp_ucode);
}




void
ds_SnarfCopies (handler)
void (*handler) ();
/*
 * Request copies of notification request events.
 */
{
	struct dsp_Template req;

	if (Standalone)
		return;
	req.dsp_type = dpt_CopyNotifyReq;
	ds_SendToDaemon (&req, sizeof(req));
	CopyFunc = handler;
}




/*=======================================================================*/




void
ds_SendToDaemon (msg, len)
void *msg;
int len;
/*
 * Send a message off to the data store daemon.
 */
{
	char *dhost, group[80];

	if (Standalone)
	{
		msg_ELog (EF_PROBLEM, "Can't send to daemon while standalone");
		return;
	}
	msg_send (Daemon, MT_DATASTORE, FALSE, msg, len);
}





static int
ds_GreetDaemon ()
/*
 * Say hi to the daemon and check protocol versions.
 */
{
	struct dsp_Template dt;
	int error;

	dt.dsp_type = dpt_Hello;
	ds_SendToDaemon (&dt, sizeof (dt));
	error = 0;
	if (msg_SearchFrom (Daemon, MT_DATASTORE, ds_CheckProtocol, &error))
		error = 1;
	return (error);
}



/* ARGSUSED */
static int
ds_CheckProtocol (msg, error)
Message *msg;
int *error;
/*
 * Get the protocol version packet back and check the number.
 */
{
	struct dsp_ProtoVersion *dpv = (struct dsp_ProtoVersion *) msg->m_data;

	if (dpv->dsp_type != dpt_R_ProtoVersion)
		return (MSG_ENQUEUE);
	if (dpv->dsp_version != DSProtocolVersion)
	{
		msg_ELog (EF_PROBLEM, "DS Protocol version mismatch %x vs %x",
			  DSProtocolVersion, dpv->dsp_version);
		msg_ELog (EF_PROBLEM, "This program should be relinked");
		*error = 1;
	}

	return (MSG_DONE);
}
