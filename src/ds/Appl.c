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

#include <string.h>

#include <defs.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "dsPrivate.h"
#include "dslib.h"
#include "Appl.h"

RCSID ("$Id: Appl.c,v 3.44 1996-08-30 18:58:43 granger Exp $")

/*
 * Platform search lists
 */
typedef struct _PlatformList {
	PlatformId *pl_pids;
	int pl_npids;
} PlatformList;

/*
 * Notification callbacks are void functions
 */
typedef void (*VFunc) ();

/*
 * Local private prototypes.
 */
static void     ds_InitPFTable FP ((void));
static void     ds_DispatchNotify FP ((struct dsp_Notify *));
static int	ds_AwaitNPlat FP ((Message *, int *));
static int	ds_AwaitPlat FP ((Message *, ClientPlatform *));
static int	ds_AwaitPlatformList FP ((Message *msg, PlatformList *));
static void	ds_CachePlatform FP ((PlatformId pid, ClientPlatform *plat));
static int	ds_AwaitFile FP ((Message *, DataFile *));
static int	ds_AwaitGrant FP ((Message *, int));
static int	ds_AwaitPID FP ((Message *, PlatformId *));
static void	ds_SendSearch FP((char *regexp, int sort, int subs,
				  int sendplats));
static int	ds_AwaitDF FP ((Message *, int *));
static int	ds_GreetDaemon FP ((void));
static int	ds_CheckProtocol FP ((Message *, int *));

/*
 * The application notification table.
 */
static VFunc ApplFuncs[MAXPLAT];
static VFunc CopyFunc = 0;

/*
 * Platform structure caching.
 */
static ClientPlatform *PlatStructs[MAXPLAT] = { 0 };

/*
 * Data file entry cache structure.  The cache is intended to be small, 
 * fast and cheap...
 */
# define N_DF_CACHE 10
static DataFile DFCache[N_DF_CACHE];
static int DFZap = 0;	/* Next entry to zap	*/

/*
 * How many locks we have active at once.  This is a kludgy way to prevent
 * multiple locking for now.  It will prevent locking of more than one 
 * platform at once, which may be undesirable, but I don't think that will
 * break anything now.
 */
static int LockCount = 0;

/*
 * How far in the future we are willing to accept data.  Public so that
 * DFA_Appl.c has access.
 */
int MaxFuture = 3600;	/* One hour into future	*/

/*
 * The platform lookup table.
 */
static stbl Pf_Names;



int
ds_Initialize()
/*
 * Hook into the data store.
 */
{
	int i;
/*
 * Set up the platform lookup table.
 */
	ds_InitPFTable();
	for (i = 0; i < MAXPLAT; i++)
		ApplFuncs[i] = 0;
	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "maxfuture",
			&MaxFuture, SYMT_INT, 0);
/*
 * Join the data store group.
 */
	msg_join("DataStore");
	F_Init ();
	msg_AddProtoHandler(MT_DATASTORE, ds_DSMessage);
/*
 * Initialize the datafile cache.
 */
	for (i = 0; i < N_DF_CACHE; i++)
		DFCache[i].df_index = -1;
/*
 * Say hi to the daemon.
 */
	return (! ds_GreetDaemon ());
}





static void
ds_InitPFTable ()
/*
 * Create the platform lookup table.
 */
{
/*
 * Create the table itself.
 */
	Pf_Names = usy_c_stbl ("Platform_names");
}






PlatformId
ds_LookupPlatform(name)
char *name;
/*
 * Find this platform.
 */
{
	int             type;
	SValue          v;
	PlatformId pid;
	struct dsp_PLookup req;
/*
 * If we already know about this platform just send back the ID.
 */
	if (usy_g_symbol (Pf_Names, name, &type, &v))
		return (v.us_v_int);
/*
 * It can't be a valid platform if the name won't fit into the request.
 */
	if (strlen(name) >= sizeof(req.dsp_name))
		return (BadPlatform);
/*
 * Otherwise we need to ask mom.
 */
	req.dsp_type = dpt_LookupPlatform;
	strcpy (req.dsp_name, name);
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitPID, &pid);
/*
 * Remember this for next time.  We don't stash failures on the theory
 * that someday we will have dynamic platform creation and a name that
 * fails once could succeed later.
 */
	if (pid != BadPlatform)
	{
		v.us_v_int = pid;
		usy_s_symbol (Pf_Names, name, SYMT_INT, &v);
		/* PlatNames[pid] = usy_pstring (name);*/
	}
	return (pid);
}





static int
ds_AwaitPID (msg, pid)
Message *msg;
PlatformId *pid;
/*
 * Wait for the platform lookup to return.
 */
{
	struct dsp_PID *dp = (struct dsp_PID *) msg->m_data;

	if (dp->dsp_type == dpt_R_PID)
	{
		*pid = dp->dsp_pid;
		return (0);
	}
	return (1);
}



/* -----------------------------------------------------------------
 * NOTE: The search functions below are not officially part of the 
 * application interface.  Only the standard daemon clients use them
 * at present.  These functions may change, and there is no gaurantee
 * that the changes will be backwards compatible.
 */

static void
ds_SendSearch (regexp, sort, subs, sendplats)
char *regexp;
bool sort;
bool subs;
bool sendplats;	/* True if we want to receive platform structures as well */
/*
 * Send off a search request to the daemon
 */
{
	struct dsp_PlatformSearch search;

	search.dsp_type = dpt_PlatformSearch;
	search.dsp_sendplats = sendplats;
	search.dsp_alphabet = sort;
	search.dsp_subplats = subs;
	search.dsp_parent = BadPlatform;
	search.dsp_children = FALSE;
	if (regexp && (strlen(regexp) < sizeof(search.dsp_regexp)))
		strcpy(search.dsp_regexp, regexp);
	else
		search.dsp_regexp[0] = '\0';
	ds_SendToDaemon (&search, sizeof (search));
}



PlatformId *
ds_SearchPlatforms (regexp, nplats, sort, subs)
char *regexp;
int *nplats;
bool sort;
bool subs;
/*
 * Send a search message to the daemon, telling it we don't want platform
 * structures, and return the list of pids we receive back from the daemon.
 */
{
	PlatformList pl;

	ds_SendSearch (regexp, sort, subs, FALSE);
	msg_Search (MT_DATASTORE, ds_AwaitPlatformList, &pl);
	*nplats = pl.pl_npids;
/*
 * If no platform ids returned, then pl_pids will be NULL
 */
	return (pl.pl_pids);
}



PlatformId *
ds_GatherPlatforms (regexp, nplats, sort, subs)
char *regexp;
int *nplats;
bool sort;
bool subs;
/*
 * Send a search message to the daemon, telling it we do want platform
 * structures, and return the list of pids we receive back from the daemon.
 */
{
	PlatformList pl;

	ds_SendSearch (regexp, sort, subs, TRUE);
	msg_Search (MT_DATASTORE, ds_AwaitPlatformList, &pl);
	*nplats = pl.pl_npids;
	return (pl.pl_pids);
}



PlatformId *
ds_LookupSubplatforms (parent, nsubplat)
PlatformId parent;
int *nsubplat;
/*
 * Return a list of the ID's of any subplatforms of the given platform.
 * If none are found, returns NULL.  The returned list must be freed by
 * the application.
 */
{
	struct dsp_PlatformSearch search;
	PlatformList pl;

	search.dsp_type = dpt_PlatformSearch;
	search.dsp_parent = parent;
	search.dsp_children = TRUE;
	search.dsp_subplats = TRUE;
	search.dsp_alphabet = FALSE;
	search.dsp_sendplats = FALSE;
	search.dsp_regexp[0] = '\0';
	ds_SendToDaemon (&search, sizeof (search));
/*
 * Now wait for the response
 */
	msg_Search (MT_DATASTORE, ds_AwaitPlatformList, &pl);
	*nsubplat = pl.pl_npids;
	return (pl.pl_pids);
}



static int
ds_AwaitPlatformList (msg, pl)
Message *msg;
PlatformList *pl;
/*
 * See if this is our ack.
 */
{
	struct dsp_PlatformList *ans = (struct dsp_PlatformList *)msg->m_data;
	struct dsp_PlatStructSearch *pss = 
		(struct dsp_PlatStructSearch *) msg->m_data;
/*
 * If this is the list, copy it into new memory and set our argument to point
 * to this list.
 */
	if (ans->dsp_type == dpt_R_PlatformSearch)
	{
		pl->pl_npids = ans->dsp_npids;
		pl->pl_pids = NULL;
		if (ans->dsp_npids > 0)
		{
			int nbytes = ans->dsp_npids * sizeof(PlatformId);
			pl->pl_pids = (PlatformId *) malloc (nbytes);
			memcpy (pl->pl_pids, ans->dsp_pids, nbytes);
		}
		return (MSG_DONE);
	}
/*
 * Otherwise we need to be processing PlatStruct messages, in case this is
 * a search for which we requested that the structures be sent along.
 */
	switch (ans->dsp_type)
	{
	   case dpt_R_PlatStructSearch:
		ds_CachePlatform (pss->dsp_pid, &(pss->dsp_plat));
		return (MSG_CONSUMED);
	   case dpt_CacheInvalidate:
	   	ds_DSMessage (msg);
		return (MSG_CONSUMED);
	   default:
	   	return (MSG_ENQUEUE);
	}
}




char *
ds_PlatformName(id)
PlatformId id;
/*
 * Get back the name for this platform.
 */
{
	static char *badmsg = "(BadPlatformID)";
	if ((id == BadPlatform) || (id < 0) || (id >= MAXPLAT))
		return (badmsg);
	if (! PlatStructs[id])
	{
		int n = ds_GetNPlat ();
		ClientPlatform p;

		if (id >= n)
			return (badmsg);
		ds_GetPlatStruct (id, &p, FALSE);
	}
	return (PlatStructs[id]->cp_name);
}



int
ds_IsMobile(id)
PlatformId id;
/*
 * Return TRUE iff this is a mobile platform.
 */
{
	ClientPlatform p;
	ds_GetPlatStruct (id, &p, FALSE);
	return (p.cp_flags & DPF_MOBILE);
}





bool
ds_IsModelPlatform(id)
PlatformId id;
/*
 * Return TRUE iff this is a model platform.
 */
{
	ClientPlatform p;
	ds_GetPlatStruct (id, &p, FALSE);
	return ((p.cp_flags & DPF_MODEL) != 0);
}




int
ds_FindDF (pid, when, src)
PlatformId pid;
const ZebTime *when;
int src;
/*
 * Find the first datafile entry before this time.  DS private routine.
 */
{
	struct dsp_FindDF req;
	int index;
/*
 * Just fire off a request to the daemon.
 */
 	req.dsp_type = dpt_FindDF;
	req.dsp_pid = pid;
	req.dsp_when = *when;
	req.dsp_src = src;
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitDF, &index);
	return (index);
}




int
ds_FindBefore (pid, when)
PlatformId pid;
const ZebTime *when;
/*
 * Find the first datafile entry before this time, for all sources.
 */
{
	return (ds_FindDF (pid, when, SRC_ALL));
}




int
ds_FindAfter (pid, when)
PlatformId pid;
const ZebTime *when;
/*
 * Find the first datafile entry containing data after this time, for
 * all sources.
 */
{
	struct dsp_FindDF req;
	int index;
/*
 * Just fire off a request to the daemon.
 */
 	req.dsp_type = dpt_FindAfter;
	req.dsp_pid = pid;
	req.dsp_when = *when;
	req.dsp_src = SRC_ALL;
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitDF, &index);
	return (index);
}




static int
ds_AwaitDF (msg, index)
Message *msg;
int *index;
/*
 * Wait for a data file index to come back.
 */
{
	struct dsp_R_DFI *ans = (struct dsp_R_DFI *) msg->m_data;

	if (ans->dsp_type == dpt_R_DFIndex)
	{
		*index = ans->dsp_index;
		return (MSG_DONE);
	}
	return (MSG_ENQUEUE);
}
		




DataOrganization
ds_PlatformDataOrg(pid)
PlatformId pid;
/*
 * Return the organization of the data returned by this platform.
 */
{
	ClientPlatform p;
	
	ds_GetPlatStruct (pid, &p, FALSE);
	return (p.cp_org);
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
	int i;

	switch (dt->dsp_type)
	{
	    case dpt_DataGone:
		ddg = (struct dsp_DataGone *) dt;
		/*
		 * Either the file was closed by the DFA handler or a close
		 * is not necessary because we never could have opened it.
		 */
		/* dfa_ForceClose (ddg->dsp_file); */
		/*
		 * All we need to do here is invalidate any cache entry
		 */
		for (i = 0; i < N_DF_CACHE; i++)
		{
			if (DFCache[i].df_index == ddg->dsp_file)
			{
#ifdef DEBUG
				msg_ELog (EF_DEBUG,
				   "DataGone received for %s (%d)",
				   DFCache[i].df_name,
				   DFCache[i].df_index);
#endif /* DEBUG */
				DFCache[i].df_index = -1;
				break;
			}
		}
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

	/*
	 * Cache invalidations.
	 */
	   case dpt_CacheInvalidate:
	   	ds_ZapCache (&((struct dsp_CacheInvalidate *) dt)->dsp_dfe);
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
/*
 * Write lock the platform around the deletion just to be sure.
 */
	ds_WriteLock (platform);
	del.dsp_type = dpt_DeleteData;
	del.dsp_plat = platform;
	del.dsp_when = *zaptime;
	ds_SendToDaemon (&del, sizeof (del));
	ds_FreeWLock (platform);
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
	ds_WriteLock (platform);
	del.dsp_type = dpt_DeleteObs;
	del.dsp_plat = platform;
	del.dsp_when = *zaptime;
	ds_SendToDaemon (&del, sizeof (del));
	ds_FreeWLock (platform);
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

	req.dsp_type = dpt_Rescan;
	req.dsp_pid = platform;
	req.dsp_all = all;
	ds_SendToDaemon (&req, sizeof (req));
}







void
ds_RequestNotify(platform, param, func)
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
	req.dsp_type = dpt_NotifyRequest;
	req.dsp_pid = platform;
	req.dsp_param = param;
	ds_SendToDaemon (&req, sizeof(req));
/*
 * Stash away the function so we can call it when the notifications
 * arrive.
 */
	ApplFuncs[platform] = func;
}





void 
ds_CancelNotify()
/*
 * Cancel all data available notifications.
 */
{
	struct dsp_Template req;
	int             i;

	req.dsp_type = dpt_CancelNotify;
	ds_SendToDaemon (&req, sizeof (req));
	for (i = 0; i < MAXPLAT; i++)
		ApplFuncs[i] = 0;
}





static void
ds_DispatchNotify(notify)
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
ds_SnarfCopies(handler)
void (*handler) ();
/*
 * Request copies of notification request events.
 */
{
	struct dsp_Template req;

	req.dsp_type = dpt_CopyNotifyReq;
	ds_SendToDaemon (&req, sizeof(req));
	CopyFunc = handler;
}





int
ds_GetDetail (key, details, ndetail, v)
char *key;
dsDetail *details;
int ndetail;
SValue *v;
/*
 * Try to find this detail value in the list, returning TRUE iff it is
 * there.  If V is NULL, the detail value is not returned.
 */
{
	for (; ndetail > 0; details++, ndetail--)
		if (! strcmp (key, details->dd_Name))
		{
			if (v)
				*v = details->dd_V;
			return (TRUE);
		}
	return (FALSE);
}





int
ds_GetNPlat ()
/*
 * Return the number of platforms known to the system.
 */
{
	struct dsp_Template req;
	int nplat;
/*
 * Send off the request to the daemon.
 */
 	req.dsp_type = dpt_GetNPlat;
	ds_SendToDaemon ( &req, sizeof(req));
/*
 * Now we gotta wait for the answer.
 */
	msg_Search (MT_DATASTORE, ds_AwaitNPlat, &nplat);
	return (nplat);
}





static int
ds_AwaitNPlat (msg, np)
Message *msg;
int *np;
/*
 * See if this is our ack.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;

	if (tmpl->dsp_type == dpt_R_NPlat)
	{
		struct dsp_NPlat *dnp = (struct dsp_NPlat *) msg->m_data;
		*np = dnp->dsp_nplat;
		return (0);
	}
	return (1);
}




void
ds_GetPlatInfo (pid, pinfo)
PlatformId pid;
PlatformInfo *pinfo;
/*
 * Get some info about this plat.
 */
{
	ClientPlatform plat;
/*
 * Get the platform structure and reformat it into the user's structure.
 * The stuff which goes into the platform info never changes, so we
 * don't need to refresh the cache.
 */
	ds_GetPlatStruct (pid, &plat, FALSE);
	strcpy (pinfo->pl_Name, plat.cp_name);
	pinfo->pl_NDataSrc = (plat.cp_flags & DPF_REMOTE) ? 2 : 1;
	pinfo->pl_Mobile = plat.cp_flags & DPF_MOBILE;
	pinfo->pl_SubPlatform = plat.cp_flags & DPF_SUBPLATFORM;
	pinfo->pl_Parent = plat.cp_parent;
}




void
ds_GetPlatStruct (pid, plat, refresh)
PlatformId pid;
ClientPlatform *plat;
bool refresh;
/*
 * Get the platform structure for this PID.  Only re-fetch a cached struct
 * if the "refresh" flag is true.
 */
{
	struct dsp_GetPlatStruct req;
/*
 * See if we have it cached and that is sufficient.
 */
	if (PlatStructs[pid] && ! refresh)
	{
		*plat = *PlatStructs[pid];
		return;
	}
/*
 * Send off the request and wait for an answer.
 */
 	req.dsp_type = dpt_GetPlatStruct;
	req.dsp_pid = pid;
	ds_SendToDaemon ( &req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitPlat, plat);
/*
 * Cache the result.
 */
	ds_CachePlatform (pid, plat);
}




static int
ds_AwaitPlat (msg, p)
Message *msg;
ClientPlatform *p;
/*
 * See if this is our platform structure.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;

	if (tmpl->dsp_type == dpt_R_PlatStruct)
	{
		struct dsp_PlatStruct *dps = (struct dsp_PlatStruct *) tmpl;
	/*
	 * Sanity check
	 */
		if (msg->m_len != sizeof (struct dsp_PlatStruct))
		{
			msg_ELog (EF_EMERGENCY, 
				  "Plat structure size mismatch (%d vs. %d)!",
				  sizeof (struct dsp_PlatStruct), msg->m_len);
			msg_ELog (EF_EMERGENCY, 
			  "Make sure dsDaemon and I are compiled with the");
			msg_ELog (EF_EMERGENCY,
			  "same value of CFG_PLATNAME_LEN in config.h");

			exit (1);
		}
	/*
	 * Give them what we got
	 */
		*p = dps->dsp_plat;
		return (0);
	}
	return (1);
}



static void
ds_CachePlatform (pid, plat)
PlatformId pid;
ClientPlatform *plat;
{
	if (! PlatStructs[pid])
		PlatStructs[pid] = ALLOC (ClientPlatform);
	*PlatStructs[pid] = *plat;
}



int
ds_GetDataSource (pid, which, dsi)
PlatformId pid;
int which;
DataSrcInfo *dsi;
/*
 * Return information about a data source on this platform.
 */
{
	ClientPlatform plat;
	char *remname;
/*
 * We need the platform structure to get anywhere with this.  And we need
 * volatile info: the first data file in the source, so we force
 * a refresh of the platform structure cache.
 */
	ds_GetPlatStruct (pid, &plat, TRUE);
/*
 * Now see what they want.  This is currently the bleeding edge of the 
 * "data source" notion, so we fake it from the old scheme.
 */
	if (which == 0)		/* local source */
	{
		strcpy (dsi->dsrc_Name, "Local");
		strcpy (dsi->dsrc_Where, plat.cp_dir);
		dsi->dsrc_Type = dst_Local;
		dsi->dsrc_FFile = plat.cp_LocalData;
		return (TRUE);
	}
/*
 * "Remote" directory is 1, if it exists.
 */
	if (which != 1 || ! (plat.cp_flags & DPF_REMOTE))
		return (FALSE);
	strcpy (dsi->dsrc_Name,
		((remname = getenv ("REMOTE_NAME")) != NULL) ? 
		remname : "Secondary");
	strcpy (dsi->dsrc_Where, plat.cp_rdir);
	dsi->dsrc_Type = dst_Local;
	dsi->dsrc_FFile = plat.cp_RemoteData;
	return (TRUE);
}




void
ds_GetFileInfo (index, dfi)
int index;
DataFileInfo *dfi;
/*
 * Get some info about this file
 */
{
	DataFile df;
/*
 * Get the data file structure and reformat it into the user's structure.
 */
	ds_GetFileStruct (index, &df);
	strcpy (dfi->dfi_Name, df.df_name);
	dfi->dfi_Begin = df.df_begin;
	dfi->dfi_End = df.df_end;
	dfi->dfi_NSample = df.df_nsample;
	dfi->dfi_Plat = df.df_platform;
	dfi->dfi_Archived = df.df_flags & DFF_Archived;
	dfi->dfi_Next = df.df_FLink;
}




void
ds_GetFileStruct (index, df)
int index;
DataFile *df;
/*
 * Get the file structure for this index.
 */
{
	struct dsp_GetFileStruct req;
	int i;
/*
 * See if we have this one in the cache.
 */
	for (i = 0; i < N_DF_CACHE; i++)
		if (DFCache[i].df_index == index)
		{
			*df = DFCache[i];
			return;
		}
/*
 * Send off the request and wait for an answer.
 */
 	req.dsp_type = dpt_GetFileStruct;
	req.dsp_index = index;
	ds_SendToDaemon ( &req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitFile, df);
}




static int
ds_AwaitFile (msg, df)
Message *msg;
DataFile *df;
/*
 * See if this is our platform structure.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;
/*
 * See if this is it.  If so, add it to the cache and return it.
 */
	if (tmpl->dsp_type == dpt_R_FileStruct)
	{
		struct dsp_FileStruct *dfs = (struct dsp_FileStruct *) tmpl;
	/*
	 * Sanity check
	 */
		if (msg->m_len != sizeof (struct dsp_FileStruct))
		{
			msg_ELog (EF_EMERGENCY, 
				  "dsp_FileStruct size mismatch (%d vs. %d)!",
				  sizeof (struct dsp_FileStruct), msg->m_len);
			msg_ELog (EF_EMERGENCY, 
			  "Make sure dsDaemon and I are compiled with the");
			msg_ELog (EF_EMERGENCY,
			  "same value of CFG_DATAFILE_LEN in config.h");

			exit (1);
		}
	/*
	 * Stash what we got
	 */
		DFCache[DFZap++] = *df = dfs->dsp_file;
		if (DFZap >= N_DF_CACHE)
			DFZap = 0;
		return (0);
	}
	return (1);
}




void
ds_ZapCache (dfe)
DataFile *dfe;
/*
 * Deal with an invalidated cache entry.
 */
{
	int i;

	for (i = 0; i < N_DF_CACHE; i++)
		if (DFCache[i].df_index == dfe->df_index)
		{
		/*
		 * We only want to update the cache if this dfe is in fact
		 * more recent than the one in the cache.  It could be that
		 * the cache entry is newer by virtue of an update ack and
		 * the fact that the invalidate message may have been
		 * sitting in a queue for a while.  This is only a
		 * safeguard, and may not be failsafe. XXX.  A perhaps better
		 * method would be to use the message sequence number.
		 */
			if (DFCache[i].df_rev <=  dfe->df_rev)
			{
				DFCache[i] = *dfe;
#ifdef DEBUG
				msg_ELog (EF_DEBUG,
				   "updating client cache for %s (%d)",
				   dfe->df_name, dfe->df_index);
#endif /* DEBUG */
			}
			break;
		}
}





void
ds_LockPlatform (plat)
PlatformId plat;
/*
 * Take out a read lock on this platform.
 */
{
	struct dsp_PLock req;
/*
 * If we already have a lock active do nothing.
 */
	if (LockCount++)
		return;
/*
 * Send off the lock request, and wait for the answer saying that we
 * got it.
 */
	req.dsp_type = dpt_PLock;
	req.dsp_pid = plat;
	ds_SendToDaemon ( &req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitGrant, 0);
}



void
ds_WriteLock (plat)
PlatformId plat;
/*
 * Take out a write lock on this platform.
 */
{
	struct dsp_PLock req;
/*
 * Send off the lock request, and wait for the answer saying that we
 * got it.
 */
	req.dsp_type = dpt_WriteLock;
	req.dsp_pid = plat;
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitGrant, 0);
}




/* ARGSUSED */
static int
ds_AwaitGrant (msg, junk)
Message *msg;
int junk;
/*
 * Wait for a grant message.
 */
{
	struct dsp_Template *dt = (struct dsp_Template *) msg->m_data;
/*
 * If this is our grant, we're done.
 */
	if (dt->dsp_type == dpt_R_PLockGranted)
		return (MSG_DONE);
/*
 * Otherwise let's look for messages that are worth processing now.  It's
 * a good idea to be in sync with changes from the daemon before accepting
 * our lock on the platform.
 */
	switch (dt->dsp_type)
	{
	   case dpt_DataGone:	/* Could be dangerous, this one? */
	   case dpt_CacheInvalidate:
	   	ds_DSMessage (msg);
		return (MSG_CONSUMED);
	   default:
	   	return (MSG_ENQUEUE);
	}
}




void
ds_UnlockPlatform (plat)
PlatformId plat;
/*
 * Release the lock on this platform.
 */
{
	struct dsp_PLock req;

	if (--LockCount)
		return;	/* not yet */
	req.dsp_type = dpt_ReleasePLock;
	req.dsp_pid = plat;
	ds_SendToDaemon ( &req, sizeof (req));
}



void
ds_FreeWLock (plat)
PlatformId plat;
/*
 * Release the lock on this platform.
 */
{
	struct dsp_PLock req;

	req.dsp_type = dpt_ReleaseWLock;
	req.dsp_pid = plat;
	ds_SendToDaemon (&req, sizeof (req));
}





void
ds_SendToDaemon (msg, len)
void *msg;
int len;
/*
 * Send a message off to the data store daemon.
 */
{
	static char daemon[80];
	static int first = TRUE;
	char *dhost, group[80];

	if (first)
	{
		if ((dhost = getenv ("DS_DAEMON_HOST")) != NULL)
		{
			sprintf (group, "DataStore@%s", dhost);
			msg_join (group);
			strcpy (daemon, "DS_Daemon@");
			strcat (daemon, dhost);
		}
		else
			strcpy (daemon, "DS_Daemon");
		first = FALSE;
	}
	msg_send (daemon, MT_DATASTORE, FALSE, msg, len);
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
	if (msg_Search (MT_DATASTORE, ds_CheckProtocol, &error) || error)
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
		return (1);
	if (dpv->dsp_version != DSProtocolVersion)
	{
		msg_ELog (EF_PROBLEM, "DS Protocol version mismatch %x vs %x",
			  DSProtocolVersion, dpv->dsp_version);
		msg_ELog (EF_PROBLEM, "This program should be relinked");
		*error = 1;
	}
	return (0);
}





void
ds_MarkArchived (dfi)
int dfi;
/*
 * Send a message to the daemon saying that this file has been archived.
 */
{
	struct dsp_MarkArchived ma;

	ma.dsp_type = dpt_MarkArchived;
	ma.dsp_FileIndex = dfi;
	ds_SendToDaemon (&ma, sizeof (ma));
}
	



void
ds_FreeCache ()
/*
 * Free the platform cache.
 */
{
	int i;

	for (i = 0; i < MAXPLAT; ++i)
	{
		if (PlatStructs[i])
		{
			free (PlatStructs[i]);
			PlatStructs[i] = NULL;
		}
	}
}
