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


#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "dsPrivate.h"
#include "dslib.h"
#include "dfa.h"

#ifndef lint
MAKE_RCSID ("$Id: Appl.c,v 3.37 1994-11-21 22:46:40 granger Exp $")
#endif

/*
 * Platform search lists
 */
typedef struct _PlatformList {
	PlatformId *pl_pids;
	int pl_npids;
} PlatformList;


/*
 * Local prototypes.
 */
static void     ds_InitPFTable FP ((void));
static void     ds_NotifyDaemon FP ((ClientPlatform *, int, DataChunk *, 
				     int, int, int, int));
static void     ds_DispatchNotify FP ((struct dsp_Notify *));
int             ds_DSMessage FP ((struct message *));
static int      ds_AttrCheck FP ((int, ZebTime *, char *));
static int 	ds_FindDest FP ((DataChunk *, ClientPlatform *, int, 
				 int *dfile,
				 int *dfnext, WriteCode *, int, ZebTime *));
static bool	ds_SameDay FP ((ZebTime *, ZebTime *));
static int	ds_MakeNewFile FP ((DataChunk *, ClientPlatform *, int sample, 
				    dsDetail *details, int ndetail));
static int	ds_RequestNewDF FP ((PlatformId, char *, ZebTime *));
static int	ds_GetNDFResp FP ((struct message *,
				struct dsp_R_CreateFile *));
static void	ds_AbortNewDF FP ((PlatformId, int));
static int	ds_AwaitAck FP ((Message *, int));
static int	ds_AwaitNPlat FP ((Message *, int *));
static int	ds_AwaitPlat FP ((Message *, ClientPlatform *));
static int	ds_AwaitPlatformList FP ((Message *msg, PlatformList *));
static void	ds_CachePlatform FP ((PlatformId pid, ClientPlatform *plat));
static void 	ds_FProcGetList FP ((DataChunk *, GetList *, dsDetail *, int));
static int	ds_AwaitFile FP ((Message *, DataFile *));
static int	ds_AwaitGrant FP ((Message *, int));
static int	ds_AwaitPID FP ((Message *, PlatformId *));
static void	ds_SendToDaemon FP ((void *, int));
static void	ds_SendSearch FP((char *regexp, int sort, int subs,
				  int sendplats));
static int	ds_AwaitDF FP ((Message *, int *));
static void	ds_ZapCache FP ((DataFile *));
static void	ds_GreetDaemon FP ((void));
static int	ds_CheckProtocol FP ((Message *, int));
static int	ds_FindBlock FP((int dfile, int dfnext, 
				 DataChunk *dc, ClientPlatform *p,
				 int sample, WriteCode wc, int *nsample));
static int	ds_FindAfter FP ((PlatformId, ZebTime *));
static void	ds_WriteLock FP ((PlatformId));
static void	ds_FreeWLock FP ((PlatformId));


/*
 * The application notification table.
 */
typedef void (*VFunc) ();
VFunc ApplFuncs[MAXPLAT];
VFunc CopyFunc = 0;

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
 * How far in the future we are willing to accept data.
 */
static int MaxFuture = 3600;	/* One hour into future	*/

/*
 * The platform lookup table.
 */
stbl Pf_Names;





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
 * Say hi to the daemon.
 */
	ds_GreetDaemon ();
/*
 * Initialize the datafile cache.
 */
	for (i = 0; i < N_DF_CACHE; i++)
		DFCache[i].df_index = -1;
	return (TRUE);
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
ds_GetObsSamples (pid, when, times, locs, max)
PlatformId pid;
ZebTime *times, *when;
Location *locs;
int max;
/*
 * Get the time and location of up to "max" samples from the observation
 * enclosing "when".
 */
{
	int dfindex;
/*
 * Find the data file holding the observation of interest, then pass
 * off the real work to DFA.
 */
	if ((dfindex = ds_FindDF (pid, when, SRC_ALL)) < 0)
		return (0);
	return (dfa_GetObsSamples (dfindex, times, locs, max));
}



int
ds_FindDF (pid, when, src)
PlatformId pid;
ZebTime *when;
int src;
/*
 * Find the first datafile entry before this time.
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




static int
ds_FindAfter (pid, when)
PlatformId pid;
ZebTime *when;
/*
 * Find the first datafile entry containing data after this time.
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
		







int
ds_GetObsTimes (pid, when, times, ntime, attr)
PlatformId pid;
ZebTime *when, *times;
int ntime;
char *attr;
/*
 * Return the times for which observations are available.  Optionally test
 * against attributes.
 */
{
	int             df, i;
	DataFile dfe;
/*
 * Find the first datafile which works.
 */
	if ((df = ds_FindDF (pid, when, SRC_ALL)) < 0)
		return (0);
/*
 * Now return some times.
 */
	ds_LockPlatform (pid);
	for (i = 0; i < ntime && df;)
	{
		ds_GetFileStruct (df, &dfe);
		if (!attr || ds_AttrCheck (df, &dfe.df_begin, attr))
		{
			*times++ = dfe.df_begin;
			i++;
		}
		df = dfe.df_FLink;
	}
	ds_UnlockPlatform (pid);
	return (i);
}




static int
ds_AttrCheck (df, t, attr)
int df;
ZebTime *t;
char *attr;
/*
 * See if this attribute is found in the data.
 */
{
	char *dattr, *pattr[50], copy[200];
	int len, i;
/*
 * If no data attrs, assume yes.
 */
	if (! (dattr = dfa_GetAttr (df, t, &len)))
		return (TRUE);
/*
 * Parse up the attributes and see if any match.
 */
	strcpy (copy, dattr);
	free (dattr);
	len = CommaParse (copy, pattr);
	for (i = 0; i < len; i++)
		if (!strcmp (pattr[i], attr))
			return (TRUE);
	return (FALSE);
}







int
ds_GetFields (plat, t, nfld, flist)
PlatformId plat;
ZebTime *t;
int *nfld;
FieldId *flist;
/*
 * Return a list of the available fields in this platform at this time.
 */
{
	int dfindex;
/*
 * Find a file entry to look at.
 */
	if ((dfindex = ds_FindDF (plat, t, SRC_ALL)) < 0)
	{
		*nfld = 0;
		return (0);
	}
/*
 * Have the format driver actually look.
 */
	return (dfa_GetFields (dfindex, t, nfld, flist));
}




int
ds_DataTimes (platform, when, n, which, rettimes)
PlatformId platform;
ZebTime *when, *rettimes;
int n;
TimeSpec which;
/*
 * Return a list of up to "n" times related to "time" by the given spec.
 *
 * Reworked for new non-SHM scheme.  This routine fetches more DFE's than
 * might really be desired, but so it goes.
 */
{
	int ndone = 0, index;
	DataFile dfe;
/*
 * We don't do it all yet.
 */
	switch (which) {
	/*
	 * Handle dsBefore -- the usual case.
	 */
	   case DsBefore:
	/*
	 * Scan down the datafile list until we find the first entry
	 * which begins before the given time.
	 */
		if ((index = ds_FindDF (platform, when, SRC_ALL)) < 0)
			return (0);
	/*
	 * Now we plow through datafile entries until we have all we
	 * want.
	 */
		ds_LockPlatform (platform);
		while (index && ndone < n)
		{
			ndone += dfa_DataTimes (index, when, which, n - ndone,
					       rettimes + ndone);
			if (ndone < n)
			{
				ds_GetFileStruct (index, &dfe);
				index = dfe.df_FLink;
			}
		}
		ds_UnlockPlatform (platform);
		return (ndone);
/*
 * We now do DsAfter too.
 */
	   case DsAfter:
	/*
	 * Get positioned.
	 */
		ds_LockPlatform (platform);
		if ((index = ds_FindAfter (platform, when)) < 0)
			return (0);
	/*
	 * Now we move forward filling the array.
	 */
		for (; index && ndone < n; index = dfe.df_BLink)
		{
			ds_GetFileStruct (index, &dfe);
			ndone += dfa_DataTimes (index, when, which, n - ndone,
					       rettimes + n - ndone - 1);
		}
		ds_UnlockPlatform (platform);
	/*
	 * If we couldn't do it all, copy what we could do forward.
	 */
		if (ndone && ndone < n)
			memcpy (rettimes, rettimes + n - ndone,
			       (n - ndone) * sizeof (ZebTime));
		return (ndone);
	/*
	 * But that's all.
	 */
	   default:
		msg_ELog (EF_PROBLEM,
			"Only DsBefore and dsAfter TimeSpec handled");
		return (0);
	}
}




bool
ds_GetAlts (pid, fid, when, offset, alts, nalts, altunits)
PlatformId pid;
FieldId	fid;
ZebTime	*when;
int	offset;
float	*alts;
int	*nalts;
AltUnitType *altunits;
/*
 * Get the heights for this time, field, and forecast offset.
 */
{
	int	dfindex;
/*
 * Now find a datafile entry we can use.
 */
	if ((dfindex = ds_FindDF (pid, when, SRC_ALL)) < 0)
		return (FALSE);
/*
 * Get the rest from the format-specific code.
 */
	return (dfa_GetAlts (dfindex, fid, offset, alts, nalts, altunits));
}




bool
ds_GetForecastTimes (pid, when, times, ntimes)
PlatformId	pid;
ZebTime	*when;
int	*times, *ntimes;
/*
 * Get the heights for this time.
 */
{
	ClientPlatform p;
	int dfindex;
/*
 * First make sure it's a model platform
 */
	if (! ds_IsModelPlatform (pid))
		return (FALSE);
/*
 * Now find a datafile entry we can use.
 */
	if ((dfindex = ds_FindDF (pid, when, SRC_ALL)) < 0)
		return (FALSE);
/*
 * Get the rest from the format-specific code.
 */
	return (dfa_GetForecastTimes (dfindex, times, ntimes));
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
 * Deal with data store protocol messages.
 */
{
	struct dsp_Template *dt = (struct dsp_Template *) msg->m_data;
	struct dsp_DataGone *ddg;
	int i;

	switch (dt->dsp_type)
	{
	/*
	 * If they've gone and deleted data on us, we have to make
	 * sure that we close the file and forget about it.
	 */
	    case dpt_DataGone:
		ddg = (struct dsp_DataGone *) dt;
		dfa_ForceClose (ddg->dsp_file);
		for (i = 0; i < N_DF_CACHE; i++)
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






DataChunk *
ds_Fetch (pid, class, begin, end, fields, nfield, details, ndetail)
PlatformId pid;
DataClass class;
ZebTime *begin, *end;
FieldId *fields;
int nfield, ndetail;
dsDetail *details;
/*
 * The net data store fetch interface.
 * Entry:
 *	PID	is the name of the platform of interest.
 *	CLASS	is the class of the desired data chunk.
 *	BEGIN	is the desired time of the first datum
 *	END	is the end time
 *	FIELDS	is a list of desired fields
 *	NFIELD	is the length of that list
 *	DETAILS	is a list of fetch control details
 *	NDETAIL	is the length of that list.
 * Exit:
 *	If any data could be found then
 *		The return value is a data chunk containing that data
 *	else
 *		The return value is NULL.
 */
{
	DataChunk *dc;
	GetList *get;
/*
 * Make the get list describing where this data has to come from.
 */
	if (! (get = dgl_MakeGetList (pid, begin, end)))
	{
		msg_ELog (EF_DEBUG, "GetList get failure");
		return (NULL);
	}
/*
 * Now it is up to the format driver to get ready and create a data 
 * chunk for us.
 */
	if (! (dc = dfa_Setup (get, fields, nfield, class)))
	{
		msg_ELog (EF_DEBUG, "Setup failure");
		dgl_ReturnList (get);
		return (NULL);
	}
	dc->dc_Platform = pid;
/*
 * Pass through the get list, snarfing data for each entry.
 *
 * Hmm...the getlist is returned in the usual reverse-time order, which 
 * was never a problem in the past.  Now we need to reverse things again.
 */
	ds_FProcGetList (dc, get, details, ndetail);
	dgl_ReturnList (get);
/*
 * It is still possible that there were no times in the file between
 * the requested times, in which case we return null for no data found.
 */
	if (dc_GetNSample (dc) == 0)
	{
		dc_DestroyDC (dc);
		return (NULL);
	}
	else
		return (dc);
}





static void
ds_FProcGetList (dc, gp, details, ndetail)
DataChunk *dc;
GetList *gp;
dsDetail *details;
int ndetail;
/*
 * Process the getlist in reverse order.
 */
{
	if (gp->gl_next)
		ds_FProcGetList (dc, gp->gl_next, details, ndetail);
	dfa_GetData (dc, gp, details, ndetail);
}






DataChunk *
ds_FetchObs (pid, class, when, fields, nfield, details, ndetail)
PlatformId pid;
DataClass class;
ZebTime *when;
FieldId *fields;
int nfield, ndetail;
dsDetail *details;
/*
 * Get an observation from this source.
 * Entry:
 *	PID	is the name of the platform of interest.
 *	CLASS	is the class of the desired data chunk.
 *	WHEN	is the time of the desired observation.
 *	FIELDS	is a list of desired fields
 *	NFIELD	is the length of that list
 *	DETAILS	is a list of fetch control details
 *	NDETAIL	is the length of that list.
 * Exit:
 *	If any data could be found then
 *		The return value is a data chunk containing that data
 *	else
 *		The return value is NULL.
 */
{
	DataChunk *dc;
	GetList *get;
	DataFile dfe;
/*
 * Make the get list describing where this data has to come from.  Then 
 * expand it to cover the entire file.
 */
	if (! (get = dgl_MakeGetList (pid, when, when)))
	{
		msg_ELog (EF_DEBUG, "GetList get failure");
		return (NULL);
	}
	ds_GetFileStruct (get->gl_dfindex, &dfe);
	get->gl_begin = dfe.df_begin;
	get->gl_end = dfe.df_end;
/*
 * Now it is up to the format driver to get ready and create a data 
 * chunk for us.
 */
	if (! (dc = dfa_Setup (get, fields, nfield, class)))
	{
		msg_ELog (EF_DEBUG, "Setup failure");
		dgl_ReturnList (get);
		return (NULL);
	}
	dc->dc_Platform = pid;
/*
 * Now just do the snarf.
 */
	dfa_GetData (dc, get, details, ndetail);
	dgl_ReturnList (get);
	return (dc);
}





bool
ds_Store (dc, newfile, details, ndetail)
DataChunk *dc;
bool newfile;
dsDetail *details;
int ndetail;
/*
 * The storage interface to the data store.
 */
{
	int nsample, sample, dfile, nnew = 0, now = 0, ndone = 0;
	WriteCode wc;
	ClientPlatform p;
	int dfnext;
	ZebTime curtime;
/*
 * This is a reasonable spot to make sure we have a valid platform
 */
	if (dc->dc_Platform == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "attempting ds_Store of DataChunk %s",
			  "with bad platform id");
		return (FALSE);
	}
/*
 * Pull some details together.
 */
	ds_WriteLock (dc->dc_Platform);
	ds_GetPlatStruct (dc->dc_Platform, &p, TRUE);
	tl_Time (&curtime);
/*
 * For now (and maybe forever) we do the writing one sample at a time,
 * to ease the process of figuring out what goes where.
 */
	nsample = dc_GetNSample (dc);
	for (sample = 0; sample < nsample; sample++)
	{
		bool new = FALSE;
	/*
	 * Find a feasible location for this data.
	 */
		if (! ds_FindDest (dc, &p, sample, &dfile, &dfnext, 
				   &wc, newfile && (sample == 0), &curtime))
			continue;	/* Sigh */
	/*
	 * If a new file is called for, create it.  Then write the data.
	 */
	 	if (wc == wc_NewFile)
		{
			if ((dfile = ds_MakeNewFile (dc, &p, sample, 
						     details, ndetail)) < 0)
				break;	/* Bail completely */
			wc = wc_Append; /* Now that the file is around */
			new = TRUE;
		}
	/*
	 * Now we just shove the sample out.
	 */
		if (dfa_PutSample (dfile, dc, sample, wc, details, ndetail))
		{
		/*
		 * Keep track of this.
		 */
			ndone++;
			if (wc == wc_Overwrite)
				now++;
			else
				nnew++;
		}
	/*
	 * Fill in the daemon on what we have done.  If we added a new
	 * file we need to refresh the platform structure.
	 */
	 	ds_NotifyDaemon (&p, dfile, dc, now, nnew, sample, 
				 sample == (nsample - 1));
		if (new)
			ds_GetPlatStruct (dc->dc_Platform, &p, TRUE);
		now = nnew = 0;
	}
/*
 * Done.
 */
	ds_FreeWLock (dc->dc_Platform);
	return (ndone == nsample);
}




bool
ds_StoreBlocks (dc, newfile, details, ndetail)
DataChunk *dc;
bool newfile;
dsDetail *details;
int ndetail;
/*
 * Store samples in the largest blocks possible
 */
{
	int nsample, sample;
	int dfile;
	int dfnext;
	int nnew;	/* Number of samples added in 1 pass of the loop  */
	int now;	/* Number of samples overwritten in 1 pass	  */
	int ndone;	/* Total number of samples successfully stored	  */
	int block_size;
	WriteCode wc;
	ClientPlatform p;
	ZebTime curtime;
/*
 * This is a reasonable spot to make sure we have a valid platform
 */
	if (dc->dc_Platform == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "attempting ds_Store of DataChunk %s",
			  "with bad platform id");
		return (FALSE);
	}
/*
 * Setup time.
 */
	ds_WriteLock (dc->dc_Platform);
	ds_GetPlatStruct (dc->dc_Platform, &p, TRUE);
	tl_Time (&curtime);
	nsample = dc_GetNSample (dc);
	sample = 0;
/*
 * Start plowing through the data.
 */
	ndone = 0;
	sample = 0;
	while (sample < nsample)
	{
		bool new = FALSE;
	/*
	 * These count new and overwritten samples since the last notify.
	 * We have to notify the daemon each time through the loop since
	 * our changes may affect where the next block goes. (e.g. new file)
	 */
		nnew = now = 0;
	/*
	 * Find a feasible location for the next sample of the data chunk
	 */
		if (! ds_FindDest (dc, &p, sample, &dfile, &dfnext, &wc, 
				   newfile && (sample == 0), &curtime))
		{
			++sample;	/* Skip this sample */
			continue;	/* Sigh */
		}
	/*
	 * If a new file is called for, create it.
	 */
	 	if (wc == wc_NewFile)
		{
			if ((dfile = ds_MakeNewFile (dc, &p, sample,
						     details, ndetail)) < 0)
				break;	/* Bail completely */
			wc = wc_Append; /* Now that the file is around */
			new = TRUE;
		}
	/*
	 * Find out how many samples can be written to this file
	 * as a single block.  The answer is at least one.
	 */
		ds_FindBlock (dfile, dfnext, dc, &p, sample, wc, &block_size);
		msg_ELog((block_size >= 25) ? EF_INFO : EF_DEBUG,
			 "%s block of %i samples to %s",
			 (wc == wc_Append) ? "appending" :
			 ((wc == wc_Insert) ? "inserting" : 
			  "overwriting"), block_size, p.cp_name);
	/*
	 * Now we write whatever block we found
	 */
		if (dfa_PutBlock (dfile, dc, sample, block_size, wc, 
				  details, ndetail))
		{
		/*
		 * Keep track of successful writes
		 */
			ndone += block_size;
			if (wc == wc_Overwrite)
				now += block_size;
			else
				nnew += block_size;
		}
		/*
		 * Move on to the rest of the samples no matter what
		 */
		sample += block_size;

		/*
		 * Fill in the daemon on what we have done.  The last sample
		 * of the block is passed to Notify since its used to set
		 * the new end time of the file.
		 */
		ds_NotifyDaemon (&p, dfile, dc, now, nnew, 
				 sample - 1,
				 (sample == nsample));
		/*
		 * If we created a new file above, then our platform
		 * structure has changed and we need a new one
		 */
		if (new)
			ds_GetPlatStruct (dc->dc_Platform, &p, TRUE);

	} /* while (sample < nsample) */

	ds_FreeWLock (dc->dc_Platform);
	return (ndone == nsample);
}





static int
ds_FindBlock(dfile, dfnext, dc, plat, sample, wc, block_size)
int dfile;
int dfnext;		/* file following dfile, chronologically */
DataChunk *dc;
ClientPlatform *plat;
int sample;
WriteCode wc;
int *block_size;
/*
 * Starts at 'sample' in data chunk 'dc', and finds out how many
 * samples following constitute a block, suitable for storing
 * with dfa_PutBlock() into 'dfile'.  
 * Returns the number in 'block_size'.  Unless
 * 'sample' is out of range, 'nsample' will always hold at least 1.
 * At present, write code other wc_Insert automatically returns 1.
 */
{
	int smp, fut;		/* counters			*/
	int nsample;
	DataFile dfe, dfenext;
	ZebTime when, past;
	ZebTime next;		/* time dfnext starts		*/
	int avail;		/* samples available in the file*/
	ZebTime *future;	/* data times already in file	*/
	int nfuture;		/* returned by dfa_DataTimes	*/
/*
 * Only accept appends and overwrites
 */
	if (wc == wc_Insert)
	{
		*block_size = 1;
		return;
	}
/*
 * To be a block, times must be chronological (the order they'll
 * be written to the file), and the samples cannot overwrite or
 * overlap any existing data (in the append case), or they must
 * coincide with each and every sample in the file (overwrite case).
 * If overwriting, the maximum number of slots ever possibly available
 * is the number in the file already.
 */
	ds_LockPlatform (dc->dc_Platform);
	ds_GetFileStruct (dfile, &dfe);
	if (wc != wc_Overwrite)
		avail = plat->cp_maxsamp - dfe.df_nsample;
	else
		avail = dfe.df_nsample;

	if (dfnext)
	{
		ds_GetFileStruct (dfnext, &dfenext);
		next = dfenext.df_begin;
	}
/*
 * So we'll see how many samples we can get which
 *  a) are in chronological order, and
 *  b) precede the start of dfnext (if there is one), and
 *  c) are on the same day, iff DPF_SPLIT set, and
 *  d) will fit within the platform's maxsamples limit, and finally
 *  e) if overwriting, which coincide with a sample already in the file
 * ...all without exceeding the number of samples in the data chunk
 */
	nsample = dc_GetNSample(dc);
	dc_GetTime(dc, sample, &past);

	if (wc == wc_Overwrite)
	{
	/*
	 * Remember: for DsAfter, the times will be written chronologically
	 * beginning at the end of the future[] array and working backwards.
	 * nfuture will be the number of times in the future[] array, 
	 * beginning at future[avail - 1].
	 */
		future = (ZebTime *)malloc(avail * sizeof(ZebTime));
		fut = avail - 1;
		nfuture = dfa_DataTimes (dfile, &past, DsAfter, 
					 avail, future + fut);
	/*
	 * See if the oldest time we got is the one we already know
	 * we're overwriting.  If so, go to the next one.
	 */
		if (nfuture && TC_Eq(past, future[fut]))
			--fut;
	}
/*
 * We know that at least one sample remains in the DC and that there is
 * space in the file for at least that sample (ds_FindDest told us).
 * So start looking at the next sample.
 */
	smp = sample + 1;
	while ((smp < nsample) && (smp - sample < avail))
	{
		dc_GetTime(dc, smp, &when);
		if (! TC_LessEq (past, when))
			break;
		if (wc == wc_Append)
		{
			if (dfnext && (! TC_Less(when, next)))
				break;
			if ((plat->cp_flags & DPF_SPLIT) &&
			    (! ds_SameDay (&when, &past)))
				break;
		}
		else if (wc == wc_Overwrite)
		{
			if (avail - fut > nfuture) /* no more times in file */
				break;
			if (! TC_Eq(when, future[fut]))
				break;
			--fut;
		}
		past = when;
		smp++;
	}
/*
 * Finished.  Record the number of samples we found and return;
 * the size of the block will be at least one.
 */
	*block_size = smp - sample;
	ds_UnlockPlatform (dc->dc_Platform);
	if (wc == wc_Overwrite)
		free (future);
	return;
}





static int
ds_FindDest (dc, plat, sample, dfile, dfnext, wc, newfile, now)
DataChunk *dc;
ClientPlatform *plat;
int sample, *dfile, *dfnext, newfile;
WriteCode *wc;
ZebTime *now;
/*
 * Try to find an appropriate destination for this datum.
 * Return value is TRUE iff it was possible.  *dfnext returns with
 * the data file which chronologically follows *dfile, if it exists.
 */
{
	int df = ds_DataChain (plat, 0);

	DataFile dfe;
	ZebTime when, dftime;
/*
 * Do a quick sanity check to see if this data has a bizarre time.  Reject
 * it in that case.
 */
	dc_GetTime (dc, sample, &when);
	if ((when.zt_Sec - now->zt_Sec) > MaxFuture)
	{
		msg_ELog (EF_PROBLEM, "Rejecting %s sample %d sec in future",
			plat->cp_name, when.zt_Sec - now->zt_Sec);
		return (FALSE);
	}
/*
 * Find the first file in the local list which begins before the time
 * of interest.  This may seem like an inefficient search, and I suppose
 * it is, but the fact of the matter is that almost every time we are 
 * appending data and we'll stop at the first DFE.
 */
	ds_LockPlatform (dc->dc_Platform);
	*dfnext = 0;
	for (; df; df = dfe.df_FLink)
	{
		ds_GetFileStruct (df, &dfe);
		if (TC_LessEq (dfe.df_begin, when))
			break;
		*dfnext = df;
	}
/*
 * If there is none, then this data predates anything we have, so we
 * just return a new file case.
 */
	if (! df)
	{
		*dfile = -1;
		*wc = wc_NewFile;
		ds_UnlockPlatform (dc->dc_Platform);
		return (TRUE);
	}
/*
 * See if the datum actually falls after the end of this dfile (most common
 * case).  If so, we either append or newfile.
 */
	*dfile = df;
	if (TC_Less (dfe.df_end, when))
	{
		if (! newfile && dfe.df_nsample < plat->cp_maxsamp &&
				 (! (plat->cp_flags & DPF_SPLIT) ||
			 	ds_SameDay (&when, &dfe.df_end)) &&
				 (dfe.df_flags & DFF_Archived) == 0)
			*wc = wc_Append;
		else
			*wc = wc_NewFile;
		ds_UnlockPlatform (dc->dc_Platform);
		return (TRUE);
	}
/*
 * The simple cases are not to be.  Now we have to see whether we need to be
 * overwriting data, or stuffing it in between.
 */
	ds_UnlockPlatform (dc->dc_Platform);
	if (! dfa_DataTimes (df, &when, DsBefore, 1, &dftime) ||
			! TC_Eq (when, dftime))
		*wc = wc_Insert;
	else
		*wc = wc_Overwrite;
	return (TRUE);
}




static bool
ds_SameDay (t1, t2)
ZebTime *t1, *t2;
/*
 * Return TRUE iff the two times are on the same day.
 */
{
	int d1, d2;

	TC_ZtSplit (t1, 0, 0, &d1, 0, 0, 0, 0);
	TC_ZtSplit (t2, 0, 0, &d2, 0, 0, 0, 0);
	return (d1 == d2);
}




static int
ds_MakeNewFile (dc, plat, sample, details, ndetail)
DataChunk *dc;
ClientPlatform *plat;
int sample;
dsDetail *details;	/* dsDetail's needed for dfa_CreateFile() */
int ndetail;
/*
 * Make a new file that will contain this DC and sample.
 */
{
	char fname[256];
	int newdf;
	ZebTime when;
/*
 * Create the new file name and tell the daemon what we have in mind
 * to do.
 */
	dc_GetTime (dc, sample, &when);
	dfa_MakeFileName (plat, &when, fname);
	if ((newdf = ds_RequestNewDF (dc->dc_Platform, fname, &when)) < 0)
		return (-1);
/*
 * Have DFA get the file made for us.  They use the data object to know which
 * fields/platforms belong therein.  A bit kludgy, but it works.
 */
	if (! dfa_CreateFile (newdf, dc, &when, details, ndetail))
	{
		ds_AbortNewDF (dc->dc_Platform, newdf);
		return (-1);
	}
	return (newdf);
}




static int
ds_RequestNewDF (plat, file, t)
PlatformId plat;
char *file;
ZebTime *t;
/*
 * Get a new datafile entry from the DS daemon for this new file.
 * Entry:
 *	PLAT	is the platform for which this file is being created.
 *	FILE	is the name of the file.
 *	T	is the expected begin time of the data to put into the file.
 * Exit:
 *	On success, the return value is the new DF entry.  Otherwise a
 *	negative value is returned.
 */
{
	struct dsp_CreateFile dspcf;
	struct dsp_R_CreateFile dspresp;
/*
 * Put together the request for the daemon.
 */
	dspcf.dsp_type = dpt_NewFileRequest;
	dspcf.dsp_plat = plat;
	dspcf.dsp_time = *t;
	strcpy (dspcf.dsp_file, file);
/*
 * Ship it off, and pick out our response.
 */
	ds_SendToDaemon (&dspcf, sizeof (dspcf));
	msg_Search (MT_DATASTORE, ds_GetNDFResp, &dspresp);
	return ((dspresp.dsp_type == dpt_R_NewFileSuccess) ?
			dspresp.dsp_FileIndex : -1);
}





static int
ds_GetNDFResp (msg, dspresp)
struct message *msg;
struct dsp_R_CreateFile *dspresp;
/*
 * Pick out our response to the new file create request.
 */
{
	struct dsp_Template *t = (struct dsp_Template *) msg->m_data;

	if (t->dsp_type == dpt_R_NewFileSuccess ||
			t->dsp_type == dpt_R_NewFileFailure)
	{
		*dspresp = * (struct dsp_R_CreateFile *) t;
		return (0);
	}
	return (1);
}




static void
ds_AbortNewDF (plat, df)
PlatformId plat;
int df;
/*
 * Abort this DF create, for some reason.
 */
{
	struct dsp_AbortNewFile abort;

	abort.dsp_type = dpt_AbortNewFile;
	abort.dsp_FileIndex = df;
	abort.dsp_pid = plat;
	ds_SendToDaemon (&abort, sizeof (abort));
}





static void
ds_NotifyDaemon (p, dfile, dc, now, nnew, sample, last)
ClientPlatform *p;
int dfile, now, nnew, sample, last;
DataChunk *dc;
/*
 * Tell the data store daemon about this data.
 */
{
	struct dsp_UpdateFile update;
/*
 * Fire off the message.
 */
	update.dsp_type = dpt_UpdateFile;
	update.dsp_FileIndex = dfile;
	dc_GetTime (dc, sample, &update.dsp_EndTime);
	update.dsp_NSamples = nnew;
	update.dsp_NOverwrite = now;
	update.dsp_Last = last;
	update.dsp_Local = TRUE;
	ds_SendToDaemon ( &update, sizeof(update) );
/*
 * Wait for the update ack.  While waiting for the ack, process any
 * CacheInvalidate messages which are put in the message queue by
 * the Daemon's update process.
 */
	msg_Search (MT_DATASTORE, ds_AwaitAck, 0);
}





/* ARGSUSED */
static int
ds_AwaitAck (msg, junk)
Message *msg;
int junk;
/*
 * See if this is our ack.
 */
{
	struct dsp_FileStruct *fs = (struct dsp_FileStruct *) msg->m_data;
/*
 * If this is the ack, we're outta here.  This should also mean we've
 * cleared our message queue of the CacheInvalidate's generated by our
 * FileUpdate message.  Note the new revision for the file in DFA.
 */
	if (fs->dsp_type == dpt_R_UpdateAck)
	{
		ds_ZapCache (&fs->dsp_file);
		dfa_NoteRevision (fs->dsp_file.df_index, fs->dsp_file.df_rev);
		return (MSG_DONE);
	}
/*
 * Otherwise we need to be processing CacheInvalidate messages to keep
 * our data file cache up to date.
 */
	switch (fs->dsp_type)
	{
	   case dpt_CacheInvalidate:
	   	ds_DSMessage (msg);
		return (MSG_CONSUMED);
	   default:
	   	return (MSG_ENQUEUE);
	}
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




static void
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



static void
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



static void
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





static void
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





static void
ds_GreetDaemon ()
/*
 * Say hi to the daemon and check protocol versions.
 */
{
	struct dsp_Template dt;

	dt.dsp_type = dpt_Hello;
	ds_SendToDaemon (&dt, sizeof (dt));
	msg_Search (MT_DATASTORE, ds_CheckProtocol, 0);
}



/* ARGSUSED */
static int
ds_CheckProtocol (msg, junk)
Message *msg;
int junk;
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
ds_ForceClosure()
/*
 * Release whatever memory we have been holding onto, from open files,
 * to DataChunk free chains, to GetList free chains, to cached structures.
 */
{
	int i;

	dfa_ForceClosure();
	dc_ForceClosure();
	dgl_ForceClosure();
	for (i = 0; i < MAXPLAT; ++i)
	{
		if (PlatStructs[i])
		{
			free (PlatStructs[i]);
			PlatStructs[i] = NULL;
		}
	}
}




bool
ds_ScanFile (platid, filename, local)
PlatformId platid;
char *filename;
bool local;
/*
 * Look for the file in the platform directory, verify the file name with
 * DFA, and then retrieve time info about the file through DFA.
 */
{
	static char fpath[1024];
	ClientPlatform plat, *p = &plat;
	ZebTime begin;
	ZebTime end;
	int nsample;

	ds_GetPlatStruct (platid, &plat, FALSE);
	if (! dfa_CheckName (p->cp_ftype, filename))
	{
		msg_ELog (EF_PROBLEM, "scan file '%s': %s",
			  filename, "dfa name check failed");
		return (FALSE);
	}
/*
 * Now make sure we can open the file and retrieve some vital statistics
 */
	sprintf (fpath, "%s/%s", (local) ? p->cp_dir : p->cp_rdir, filename);
	if (! dfa_QueryDate (p->cp_ftype, fpath, &begin, &end, &nsample))
	{
		msg_ELog (EF_PROBLEM, "scan file '%s': %s", 
			  fpath, "inaccessible or incorrect format");
		return (FALSE);
	}
/*
 * Now we can pass on the rest of the work
 */
	ds_InsertFile (platid, filename, &begin, &end, nsample, local);
}




bool
ds_InsertFile (platid, filename, begin, end, nsample, local)
PlatformId platid;
char *filename;
ZebTime *begin;
ZebTime *end;
int nsample;
bool local;
/*
 * Tell the Daemon about this new file by simulating a ds_Store to it
 */
{
	int dfile;
	DataFile dfe;
	struct dsp_UpdateFile update;
	ClientPlatform p;
	int dfnext, df;
/*
 * Get a write lock and our platform structure.
 */
	ds_WriteLock (platid);
	ds_GetPlatStruct (platid, &p, TRUE);
/*
 * Make sure the bounds of this file do not overlap any existing files.  Find
 * the first file which begins before our end time.  If there is such a file
 * make sure it ends before our begin time.  Otherwise we have a conflict and
 * fail.
 */
	ds_LockPlatform (platid);
	df = (local) ? ds_DataChain (p, 0) : ds_DataChain (p, 1);
	for (; df; df = dfe.df_FLink)
	{
		ds_GetFileStruct (df, &dfe);
		if (TC_LessEq (dfe.df_begin, *end))
			break;
	};
	ds_UnlockPlatform (platid);
	if (df && !(TC_Less (dfe.df_end, *begin)))
	{
		msg_ELog (EF_PROBLEM, 
			"inserting file '%s': %s %s", filename,
			"times conflict with known file", dfe.df_name);
		ds_FreeWLock (platid);
		return (FALSE);
	}
/*
 * Get a datafile entry for the new file
 */
	if ((dfile = ds_RequestNewDF (platid, filename, begin)) < 0)
	{
		ds_FreeWLock (platid);
		return (FALSE);
	}
/*
 * The rest of the steps are essentially like a call to ds_NotifyDaemon,
 * except its possible we're updating a remote file rather than local.
 */
	update.dsp_type = dpt_UpdateFile;
	update.dsp_FileIndex = dfile;
	update.dsp_EndTime = *end;
	update.dsp_NSamples = nsample;
	update.dsp_NOverwrite = 0;
	update.dsp_Last = TRUE;
	update.dsp_Local = local;
	ds_SendToDaemon ( &update, sizeof(update));
/*
 * Wait for the update ack.
 */
	msg_Search (MT_DATASTORE, ds_AwaitAck, 0);
/*
 * We know this file is new, so we must update the platform structure.
 */
	ds_GetPlatStruct (platid, &p, TRUE);
/*
 * Done.
 */
	ds_FreeWLock (platid);
	return (TRUE);
}

