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

RCSID ("$Id: Appl.c,v 3.49 1997-11-21 18:09:27 burghart Exp $")

/*
 * Notification callbacks are void functions
 */
typedef void (*VFunc) ();

/*
 * Local private prototypes.
 */
static void     ds_InitPFCaches FP ((void));
static void     ds_DispatchNotify FP ((struct dsp_Notify *));
static int	ds_AwaitNPlat FP ((Message *, int *));
static int	ds_AwaitPlat FP ((Message *, struct dsp_GetPlatStruct *req));
static int	ds_AwaitClass FP ((Message *, struct dsp_GetPlatStruct *req));
static int	ds_AwaitPlatformList FP ((Message *msg, PlatformList *));
static int	ds_AwaitFile FP ((Message *, DataFile *));
static void	ds_PurgeFile FP ((int dfindex));
static int	ds_AwaitGrant FP ((Message *, int));
static void	ds_SendSearch FP((char *regexp, int sort, int subs,
				  int sendplats, PlatformList *pl));
static int	ds_AwaitDF FP ((Message *, int *));
static int	ds_GreetDaemon FP ((void));
static int	ds_CheckProtocol FP ((Message *, int *));


/*
 * The application notification table.
 */
static VFunc ApplFuncs[MAXPLAT];
static VFunc CopyFunc = 0;

/*
 * Platform and class structure caching.
 */
static ClientPlatform *PlatStructs[MAXPLAT] = { 0 };
static PlatformClass *ClassStructs[MAXPLAT] = { 0 };


/*
 * Data file entry cache structure.  The cache is intended to be small, 
 * fast and cheap... but dynamic according to different client needs,
 * so that it can grow at least as large as the maximum for open files
 * but within limits.
 */
static DataFile *DFCache = NULL;
static int DFCacheSize = 0;
static int DFZap = 0;	/* Next entry to zap	*/

/*
 * How many locks we have active at once.  This is a kludgy way to prevent
 * multiple locking for now.  It will prevent locking of more than one 
 * platform at once, which may be undesirable, but I don't think that will
 * break anything now.
 */
static int LockCount = 0;

/*
 * How far in the future we are willing to accept data.  Public only so that
 * DFA_Appl.c has access.
 */
int MaxFuture = 3600;	/* One hour into future	*/

/*
 * The platform lookup tables.
 */
static stbl Pf_Names = NULL;
static stbl Pc_Names = NULL;

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
DS_Methods DSM = { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };


static char Daemon[ 2*MAX_NAME_LEN ];


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
		char group[ 2*MAX_NAME_LEN ];
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
	static int once = 0;
	int i;

	zl_usy_init ();
	F_Reset ();
	for (i = 0; i < MAXPLAT; i++)
		ApplFuncs[i] = 0;
/*
 * Field derivation init
 */
	ds_DerivInit();
/*
 * Set up the platform lookup tables and caches.
 */
	if (once++)
		ds_FreeCache ();
	ds_InitPFCaches ();
/*
 * Initialize platform directories.
 */
	dt_InitDirectories ();
}




int
ds_Initialize()
/*
 * Hook into the data store.
 */
{
	Standalone = 0;
	ds_InitAPI ();
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




PlatClassId
ds_PlatformClass (pid)
PlatformId pid;
{
	ClientPlatform *cp;

	if ((cp = ds_GetPlatStruct (pid, NULL, FALSE)))
		return (cp->cp_class);
	else
		return (BadClass);
}



int
ds_SetDataDir (dir)
const char *dir;
/*
 * Return non-zero if the string did not fit
 */
{
	return (dt_SetString (DefDataDir, dir, sizeof(DefDataDir), 
			      "setting default data directory"));
}



int
ds_SetRemoteDataDir (dir)
const char *dir;
/*
 * Return non-zero if the string did not fit
 */
{
	return (dt_SetString (RemDataDir, dir, sizeof(DefDataDir), 
			      "setting default data directory"));
}



void
ds_DisableRemote (flag)
int flag;
/*
 * Disable remote directories iff flag is nonzero.
 */
{
	DisableRemote = flag;
}




static void
ds_GetPlatformList (search, pl)
struct dsp_PlatformSearch *search;
PlatformList *pl;
/*
 * Actually build the list, either by requesting from the daemon or
 * with an alternative method function.
 */
{
	pl->pl_npids = 0;
	pl->pl_pids = NULL;
	if (DSM.dsm_SearchPlatforms)
	{
		(*DSM.dsm_SearchPlatforms)(search, pl);
	}
	else
	{
		ds_SendToDaemon (search, sizeof(struct dsp_PlatformSearch));
		msg_Search (MT_DATASTORE, ds_AwaitPlatformList, pl);
	}
}




static void
ds_SendSearch (regexp, sort, subs, sendplats, pl)
char *regexp;
bool sort;
bool subs;
bool sendplats;	/* True if we want to receive platform structures as well */
PlatformList *pl;
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
	ds_GetPlatformList (&search, pl);
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

	ds_SendSearch (regexp, sort, subs, FALSE, &pl);
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

	ds_SendSearch (regexp, sort, subs, TRUE, &pl);
	*nplats = pl.pl_npids;
	return (pl.pl_pids);
}




PlatformId
ds_LookupParent (pid)
PlatformId pid;
/*
 * Return pid's parent (BadPlatform if pid is not a subplatform).
 */
{
	ClientPlatform *cp;

	if ((cp = ds_GetPlatStruct (pid, NULL, FALSE)))
		return (cp->cp_parent);

	return (BadPlatform);
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

	ds_GetPlatformList (&search, &pl);
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
	   	(msg_ProtoHandler (MT_DATASTORE)) (msg);
		return (MSG_CONSUMED);
	   default:
	   	return (MSG_ENQUEUE);
	}
}




char *
ds_PlatformName (id)
PlatformId id;
/*
 * Get back the name for this platform.
 */
{
	static const char *badmsg = "(BadPlatformID)";
	ClientPlatform *cp;

	if ((cp = ds_GetPlatStruct (id, NULL, FALSE)))
		return (cp->cp_name);
	return ((char *)badmsg);
}




const char *
ds_ClassName (id)
PlatClassId id;
/*
 * Give back the name for this platform class.  Return null if the id
 * is invalid.
 */
{
	static const char *badmsg = "(BadClassID)";
	const PlatformClass *pc;

	if ((pc = ds_GetClassStruct (id, NULL)))
		return (pc->dpc_name);
	return (badmsg);
}



int
ds_IsMobile(id)
PlatformId id;
/*
 * Return TRUE iff this is a mobile platform.
 */
{
	const ClientPlatform *p;
	p = ds_GetPlatStruct (id, NULL, FALSE);
	return ((p) ? (p->cp_flags & DPF_MOBILE) : FALSE);
}





int
ds_IsModelPlatform(id)
PlatformId id;
/*
 * Return TRUE iff this is a model platform.
 */
{
	const ClientPlatform *p;
	p = ds_GetPlatStruct (id, NULL, FALSE);
	return ((p) ? (p->cp_flags & DPF_MODEL) : FALSE);
}



int
ds_MaxSamples (id)
PlatformId id;
{
	const ClientPlatform *p;
	p = ds_GetPlatStruct (id, NULL, FALSE);
	return ((p) ? (p->cp_maxsamp) : 0);
}


char *
ds_FilePath (pid, dfid)
PlatformId pid;
int dfid;
/*
 * The returned string contains the path to this file.  The string is in
 * static memory and will be overwritten by subsequent calls to ds_FilePath()
 */
{
	static char fname[1024];
	DataFile df;
	ClientPlatform p;

	ds_GetFileStruct (dfid, &df);
	ds_GetPlatStruct (pid, &p, FALSE);
	sprintf (fname, "%s/%s", (df.df_flags & DFF_Remote) ?
		p.cp_rdir : p.cp_dir, df.df_name);
	return (fname);
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
	if (DSM.dsm_FindBefore)
		return ((*DSM.dsm_FindBefore) (pid, when, src));
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
	if (DSM.dsm_FindAfter)
		return ((*DSM.dsm_FindAfter) (pid, when));
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

		


int
ds_DataChain (p, which)
ClientPlatform *p;
int which;
/*
 * Return the beginning of the appropriate data chain.
 */
{
	ClientPlatform parent;
/*
 * If this is a subplatform we refer ourselves to the parent instead.
 */
	if (p->cp_flags & DPF_SUBPLATFORM)
	{
		ds_GetPlatStruct (p->cp_parent, &parent, TRUE);
		p = &parent;
	}
/*
 * Now just return what they want.
 */
	return (which == 0 ? p->cp_LocalData : p->cp_RemoteData);
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

	switch (dt->dsp_type)
	{
	    case dpt_DataGone:
		ddg = (struct dsp_DataGone *) dt;
		/*
		 * Either the file was closed by the DFA handler or a close
		 * is not necessary because we never could have opened it.
		 */
#ifdef DEBUG
		msg_ELog (EF_DEBUG,
			  "DataGone received for %d", ddg->dsp_file);
#endif /* DEBUG */
		/* dfa_ForceClose (ddg->dsp_file); */
		/*
		 * All we need to do here is invalidate any cache entry
		 */
		ds_PurgeFile (ddg->dsp_file);
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
	if (DSM.dsm_DeleteData)
	{
		(*DSM.dsm_DeleteData)(platform, zaptime);
		return;
	}
	ds_WriteLock (platform);
	del.dsp_type = dpt_DeleteData;
	del.dsp_plat = platform;
	del.dsp_when = *zaptime;
	ds_SendToDaemon (&del, sizeof (del));
	ds_FreeWriteLock (platform);
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
	ds_WriteLock (platform);
	del.dsp_type = dpt_DeleteObs;
	del.dsp_plat = platform;
	del.dsp_when = *zaptime;
	ds_SendToDaemon (&del, sizeof (del));
	ds_FreeWriteLock (platform);
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




int
ds_GetNPlat ()
/*
 * Return the number of platforms known to the system.
 */
{
	struct dsp_Template req;
	int nplat;

        if (DSM.dsm_NPlat)
                return ((*DSM.dsm_NPlat)());
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


/*-----------------------------------------------------------------------*/
/* Platform structure, class, and name caches */

#ifdef APPL_STATS
static int ClassNameHits = 0;
static int ClassNameMisses = 0;
static int PlatNameHits = 0;
static int PlatNameMisses = 0;
static int PlatHits = 0;
static int PlatMisses = 0;
static int PlatRefresh = 0;	/* i.e., a miss or refresh is true */
static int ClassHits = 0;
static int ClassMisses = 0;
#endif

static void
ds_InitPFCaches ()
/*
 * Create the platform caches.
 */
{
	int i;
/*
 * Create the tables.
 */
	Pf_Names = usy_c_stbl ("Platform_names");
	Pc_Names = usy_c_stbl ("Platform_classes");
/*
 * Initialize the cache arrays. 
 */
	for (i = 0; i < MAXPLAT; ++i)
	{
		PlatStructs[i] = NULL;
		ClassStructs[i] = NULL;
	}
}



static ClientPlatform *
ds_PlatCached (pid)
PlatformId pid;
{
	ClientPlatform *cp = NULL;

	if (pid >= 0 && pid < MAXPLAT)
		cp = PlatStructs[pid];
	return (cp);
}



void *
ds_PlatTable ()
{
	if (! Pf_Names)
		ds_InitPFCaches ();
	return ((void *)Pf_Names);
}



ClientPlatform *
ds_GetPlatStruct (pid, plat, refresh)
PlatformId pid;
ClientPlatform *plat;
int refresh;
/*
 * Get the platform structure for this PID.  Only re-fetch a cached struct
 * if the "refresh" flag is true.  Return NULL if the platform cannot be
 * found or the id is invalid.  Otherwise return a read-only pointer to the
 * cached structure.  If plat is non-NULL, copy the structure into *plat.
 */
{
	ClientPlatform *cp;
/*
 * See if we have it cached and that is sufficient.
 */
	if (pid < 0 || pid >= MAXPLAT)
		cp = NULL;
	else if ((! (cp = ds_PlatCached(pid)) || refresh) && ! Standalone)
	{
		struct dsp_GetPlatStruct req;
		/*
		 * Send off the request and wait for an answer.
		 */
#ifdef APPL_STATS
		if (! cp)
			++PlatMisses;
		else
			++PlatRefresh;
#endif
		req.dsp_type = dpt_GetPlatStruct;
		req.dsp_pid = pid;
		ds_SendToDaemon (&req, sizeof (req));
		msg_Search (MT_DATASTORE, ds_AwaitPlat, &req);
		cp = PlatStructs[pid];
	}
#ifdef APPL_STATS
	else if (cp)
		++PlatHits;
#endif
	if (plat && cp)
		*plat = *cp;
	return (cp);
}




static int
ds_AwaitPlat (msg, req)
Message *msg;
struct dsp_GetPlatStruct *req;
/*
 * See if this is our platform structure.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;

	if (tmpl->dsp_type == dpt_R_PlatStruct)
	{
		struct dsp_PlatStruct *ans;
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
		 * Doublecheck IDs for grins, because we can
		 */
		ans = (struct dsp_PlatStruct *) msg->m_data;
		if (req->dsp_pid != ans->dsp_pid)
		{
			msg_ELog (EF_PROBLEM, "%s %d after requesting %d",
				  "Got platform", ans->dsp_pid, req->dsp_pid);
		}
		else if (ans->dsp_result)
		{
		/*
		 * Cache a positive result.
		 */
			ds_CachePlatform (ans->dsp_pid, &ans->dsp_plat);
		}
		return (0);
	}
	return (1);
}




void
ds_CachePlatform (pid, plat)
PlatformId pid;
ClientPlatform *plat;
{
	if (! PlatStructs[pid])
	{
		PlatStructs[pid] = ALLOC (ClientPlatform);
		/*
		 * When we first cache a platform, cache its names as well.
		 */
		ds_CacheName (plat->cp_name, pid);
	}
/*
 * Make sure someone's not updating the cache from the cache...
 */
	if (PlatStructs[pid] != plat)
		*PlatStructs[pid] = *plat;
}




void
ds_CacheName (name, pid)
const char *name;
PlatformId pid;
/*
 * Enter this name and associated hierarchical names into the lookup table.
 */
{
	SValue v;
	const char *cp;

	if (! Pf_Names)
		ds_InitPFCaches ();
/*
 * We don't stash failures since we allow dynamic platform creation.  A
 * name that fails once could succeed later, or perhaps was not meant to
 * succeed at all because of an error.
 */
	if (pid != BadPlatform)
	{
		v.us_v_int = pid;
		cp = name;
		do
		{
			usy_s_symbol (Pf_Names, (char *)cp, SYMT_INT, &v);
			if ((cp = (const char *) strchr (cp, '/')) != 0)
				cp++;
		}
		while (cp);
	}
}



const PlatformClass *
ds_GetClassStruct (cid, plat)
PlatClassId cid;
PlatformClass *plat;
/*
 * Get the class structure for this class ID.  At this point classes are
 * immutable, so there is no 'refresh' flag.  Return NULL if the class
 * cannot be found or the id is invalid.  Otherwise return a read-only 
 * pointer to the cached structure.  If plat is non-NULL, copy the class
 * structure into *plat.
 */
{
	PlatformClass *pc;
/*
 * See if we have it cached.
 */
	if (cid < 0 || cid >= MAXPLAT)
		pc = NULL;
	else if ((! (pc = ClassStructs[cid])) && ! Standalone)
	{
		struct dsp_GetPlatStruct req;
		/*
		 * Send off the request and wait for an answer.
		 */
		req.dsp_type = dpt_GetClassStruct;
		req.dsp_pid = cid;
		ds_SendToDaemon (&req, sizeof (req));
		msg_Search (MT_DATASTORE, ds_AwaitClass, &req);
		pc = ClassStructs[cid];
#ifdef APPL_STATS
		++ClassMisses;
#endif
	}
#ifdef APPL_STATS
	else if (pc)
		++ClassHits;
#endif
	if (plat && pc)
		*plat = *pc;
	return (pc);
}




static int
ds_AwaitClass (msg, req)
Message *msg;
struct dsp_GetPlatStruct *req;
/*
 * See if this is our platform class structure.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;

	if (tmpl->dsp_type == dpt_R_ClassStruct)
	{
		struct dsp_ClassStruct *dsp;
	/*
	 * Sanity check: however, we only know there's a problem when the
	 * recieved length is less than our expected minimum.
	 */
		if (msg->m_len < sizeof (struct dsp_ClassStruct))
		{
			msg_ELog (EF_EMERGENCY, 
				  "Class structure size mismatch (%d > %d)!",
				  sizeof (struct dsp_PlatStruct), msg->m_len);
			msg_ELog (EF_EMERGENCY, 
			  "Make sure dsDaemon and I are compiled with the");
			msg_ELog (EF_EMERGENCY,
			  "same value of CFG_PLATNAME_LEN in config.h");

			exit (1);
		}
	/*
	 * Doublecheck IDs for grins, and because we can
	 */
		dsp = (struct dsp_ClassStruct *) msg->m_data;
		if (req->dsp_pid != dsp->dsp_cid)
		{
			msg_ELog (EF_PROBLEM, "%s %d after requesting %d",
				  "Got class", dsp->dsp_cid, req->dsp_pid);
		}
		else if (dsp->dsp_result)
		{
			PlatformClass pc;
		/*
		 * Extract and cache a positive result.
		 */
			dt_ExtractClass (&pc, dsp, msg->m_len);
			ds_CacheClass (dsp->dsp_cid, &pc);
			dt_EraseClass (&pc);
		}
		return (0);
	}
	return (1);
}




void
ds_CacheClass (cid, pc)
PlatClassId cid;
PlatformClass *pc;
/*
 * Cache a platform class.  An existing cache is erased before updating
 * with a new one.
 */
{
	PlatformClass *dest;

	if (cid < 0 || cid > MAXPLAT)
		return;
	/*
	 * Someone may not know that they updated the cache in place.
	 */
	if (pc == ClassStructs[cid])
		return;
	if ((dest = ClassStructs[cid]) != NULL)
	{
		dt_EraseClass (dest);
	}
	else
	{
		dest = ALLOC (PlatformClass);
		ClassStructs[cid] = dest;
		/*
		 * Once we know the structure we should remember the name too.
		 */
		ds_CacheClassName (pc->dpc_name, cid);
	}
	dt_CopyClass (dest, pc);
}



void
ds_CacheClassName (name, cid)
const char *name;
PlatClassId cid;
{
	SValue v;

	if (! Pc_Names)
		ds_InitPFCaches ();
/*
 * We don't stash failures since we allow dynamic platform creation.
 * A name that fails once could succeed later, or is a name that wasn't
 * meant to succeed in the first place.
 */
	if (cid != BadClass)
	{
		v.us_v_int = cid;
		usy_s_symbol (Pc_Names, (char *)name, SYMT_INT, &v);
	}
}




PlatformId
ds_LookupPlatform(name)
const char *name;
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
	if (Pf_Names && usy_g_symbol (Pf_Names, (char *)name, &type, &v))
	{
#ifdef APPL_STATS
		++PlatNameHits;
#endif
		return (v.us_v_int);
	}
#ifdef APPL_STATS
	++PlatNameMisses;
#endif
/*
 * It can't be a valid platform if the name won't fit into the request.
 */
	if (strlen((char *)name) >= sizeof(req.dsp_name))
		return (BadPlatform);
/*
 * If we're standalone and we don't know about it, not much we can do
 */
	if (Standalone)
		return (BadPlatform);
/*
 * Otherwise we need to ask mom.
 */
	req.dsp_type = dpt_LookupPlatform;
	strcpy (req.dsp_name, (char *)name);
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitPID, &pid);
/*
 * Remember this for next time.
 */
	ds_CacheName (name, pid);
	return (pid);
}



PlatClassId
ds_LookupClass (name)
const char *name;
/*
 * Find this platform class.
 */
{
	int             type;
	SValue          v;
	PlatClassId	cid;
	struct dsp_PLookup req;
/*
 * If we already know about this class just send back the ID.
 */
	if (Pc_Names && usy_g_symbol (Pc_Names, (char *)name, &type, &v))
	{
#ifdef APPL_STATS
		++ClassNameHits;
#endif
		return (v.us_v_int);
	}
#ifdef APPL_STATS
	++ClassNameMisses;
#endif
/*
 * It can't be a valid class if the name won't fit into the request.
 */
	if (strlen((char *)name) >= sizeof(req.dsp_name))
		return (BadClass);
/*
 * If we're standalone and we don't know about it, not much we can do.
 */
	if (Standalone)
		return (BadClass);
/*
 * Otherwise we need to ask mom.
 */
	req.dsp_type = dpt_LookupClass;
	strcpy (req.dsp_name, (char *)name);
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitPID, &cid);
/*
 * Remember this for next time.
 */
	ds_CacheClassName (name, cid);
	return (cid);
}



int
ds_AwaitPID (msg, pid)
Message *msg;
PlatformId *pid;
/*
 * Wait for the platform lookup to return.
 */
{
	struct dsp_PID *dp = (struct dsp_PID *) msg->m_data;

	if ((dp->dsp_type == dpt_R_PID) || (dp->dsp_type == dpt_R_CID))
	{
		*pid = dp->dsp_pid;
		return (0);
	}
	return (1);
}



/*=======================================================================*/



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



/*  ------------------------------------------- DataFile cache access --- */

#ifdef APPL_STATS
static int DFC_Hits = 0;
static int DFC_Misses = 0;
static int DFC_In = 0;		/* DataFiles copied into cache */
static int DFC_Out = 0;		/* DataFiles removed from cache to make room */
static int DFC_Updated = 0;	/* DataFiles updated in place */
static int DFC_Loops = 0;	/* Loops looped in SearchCache */
#endif



int
ds_GetFileStruct (index, df)
int index;
DataFile *df;
/*
 * Get the file structure for this index.  Return nonzero iff successful.
 */
{
	struct dsp_GetFileStruct req;
	DataFile *cdf;
/*
 * See if we have this one in the cache.
 */
	if ((cdf = ds_SearchCache (index)))
	{
		*df = *cdf;
		return (1);
	}
/*
 * If standalone and not in the cache, we don't have it
 */
	if (Standalone)
		return (0);
/*
 * Send off the request and wait for an answer.
 */
 	req.dsp_type = dpt_GetFileStruct;
	req.dsp_index = index;
	ds_SendToDaemon ( &req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitFile, df);
	return (1);
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
		*df = dfs->dsp_file;
		ds_CacheFile (df);
		return (0);
	}
	return (1);
}



DataFile *
ds_SearchCache (dfi)
int dfi;
/*
 * Return a pointer to the actual cached data file, if present, and NULL
 * otherwise.  If we're standalone, chances are the file index less one is
 * also the cache index.
 */
{
	int i;

	if (Standalone && DFCache[dfi - 1].df_index == dfi)
	{
		return (DFCache + dfi - 1);
	}
	for (i = 0; i < DFCacheSize; i++)
	{
		if (DFCache[i].df_index == dfi)
		{
#ifdef APPL_STATS
			++DFC_Hits;
			DFC_Loops += i;
#endif
			return (DFCache + i);
		}
	}
#ifdef APPL_STATS
	++DFC_Misses;
	DFC_Loops += i;
#endif
	return (NULL);
}



static void
ds_PurgeFile (dfindex)
int dfindex;
/*
 * Remove this cache entry, if present, without replacing it.
 */
{
	DataFile *df;

	if ((df = ds_SearchCache (dfindex)))
	{
		df->df_index = -1;
	}
}



void
ds_CreateFileCache (size)
int size;
{
	if (! DFCache)
	{
		int i;

		DFCacheSize = size;
		DFCache = (DataFile *) malloc (DFCacheSize * sizeof(DataFile));
		if (! DFCache)
		{
			msg_ELog (EF_EMERGENCY, 
				  "cannot allocate datafile cache");
			DFCacheSize = 0;
			return;
		}
		for (i = 0; i < DFCacheSize; ++i)
			DFCache[i].df_index = -1;
	}
}



DataFile *
ds_CacheFile (dfe)
DataFile *dfe;
/*
 * Initialize the cache if necessary, then cache this datafile structure. 
 * If dfe is NULL, just return the location the file would have been
 * cached, and expect the caller to set the index appropriately to indicate
 * the entry is being used.
 */
{
	DataFile *dfp;

	if (! DFCache)
	{
		ds_CreateFileCache (MAX_DF_CACHE);
	}
	if (DFZap >= DFCacheSize)
		return (NULL);
#ifdef APPL_STATS
	if (DFCache[DFZap].df_index != -1)
		++DFC_Out;
	++DFC_In;
#endif
	dfp = &(DFCache[DFZap++]);
	if (dfe)
		*dfp = *dfe;
	if (DFZap >= DFCacheSize)
	{
		DFZap = 0;
		if (Standalone)
		{
			msg_ELog (EF_INFO, "%s (%d entries): %s",
				  "datafile cache full", DFCacheSize,
				  "circling to overwrite old entries");
		}
	}
	return (dfp);
}



void
ds_ZapCache (dfe)
DataFile *dfe;
/*
 * Replace an invalidated cache entry with a fresh copy.
 */
{
	DataFile *df = ds_SearchCache (dfe->df_index);

	if (df)
	{
		/*
		 * We only want to update the cache if this dfe is in fact
		 * more recent than the one in the cache.  It could be that
		 * the cache entry is newer by virtue of an update ack and
		 * the fact that the invalidate message may have been
		 * sitting in a queue for a while.  This is only a
		 * safeguard, and may not be failsafe. XXX.  Perhaps a
		 * better method would be to use the message sequence
		 * number.
		 */
		if (df->df_rev <= dfe->df_rev)
		{
			*df = *dfe;
#ifdef APPL_STATS
			++DFC_Updated;
#endif
#ifdef DEBUG
			msg_ELog (EF_DEBUG,
				  "updating client cache for %s (%d)",
				  dfe->df_name, dfe->df_index);
#endif /* DEBUG */
		}
	}
}

/* --------------------------------------------------------------------- */




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
	if (Standalone)
	{
		++LockCount;
		return;
	}
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

	if (Standalone)
		return;
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
	   	(msg_ProtoHandler (MT_DATASTORE)) (msg);
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

	if (--LockCount || Standalone)
		return;	/* not yet */
	req.dsp_type = dpt_ReleasePLock;
	req.dsp_pid = plat;
	ds_SendToDaemon ( &req, sizeof (req));
}



void
ds_FreeWriteLock (plat)
PlatformId plat;
/*
 * Release the lock on this platform.
 */
{
	struct dsp_PLock req;

	if (Standalone)
		return;
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





void
ds_MarkArchived (dfi)
int dfi;
/*
 * Send a message to the daemon saying that this file has been archived.
 */
{
	struct dsp_MarkArchived ma;

	if (Standalone)
		return;
	ma.dsp_type = dpt_MarkArchived;
	ma.dsp_FileIndex = dfi;
	ds_SendToDaemon (&ma, sizeof (ma));
}
	


void
ds_FreeCache ()
/*
 * Free the platform, class, and file caches.
 * Destroy the class and platform name tables.
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
		if (ClassStructs[i])
		{
			dt_EraseClass (ClassStructs[i]);
			free (ClassStructs[i]);
			ClassStructs[i] = NULL;
		}
	}
	if (DFCache)
	{
		free (DFCache);
		DFCache = NULL;
		DFCacheSize = 0;
	}
	if (Pf_Names)
	{
		zl_z_stbl (Pf_Names);
		Pf_Names = NULL;
	}
	if (Pc_Names)
	{
		zl_z_stbl (Pc_Names);
		Pc_Names = NULL;
	}
}


