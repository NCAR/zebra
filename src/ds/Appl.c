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


#include "../include/defs.h"
#include "../include/message.h"
#include "DataStore.h"
#include "dsPrivate.h"
#include "dslib.h"
MAKE_RCSID ("$Id: Appl.c,v 3.3 1992-06-29 16:46:05 kris Exp $")


/*
 * Local stuff.
 */
static void     ds_InitPFTable FP ((void));
static void     ds_NotifyDaemon FP ((int, DataChunk *, int, int, int, int));
static void     ds_DispatchNotify FP ((struct dsp_Notify *));
int             ds_DSMessage FP ((struct message *));
static int      ds_AttrCheck FP ((int, char *));
static int	ds_FindDF FP ((PlatformId, ZebTime *));
static int 	ds_FindDest FP ((DataChunk *, int, int *, WriteCode *, int));
static bool	ds_SameDay FP ((ZebTime *, ZebTime *));
static int	ds_MakeNewFile FP ((DataChunk *, int));
static int	ds_RequestNewDF FP ((PlatformId, char *, ZebTime *));
static int	ds_GetNDFResp FP ((struct message *,
				struct dsp_R_CreateFile *));
static void	ds_AbortNewDF FP ((PlatformId, int));
static int	ds_AwaitAck FP ((Message *, int));


/*
 * The application notification table.
 */
#define MAXPLAT 1024
typedef void (*VFunc) ();
VFunc ApplFuncs[MAXPLAT];
VFunc CopyFunc = 0;



int
ds_Initialize()
/*
 * Hook into the data store.
 */
{
	int i;
/*
 * Hook into the shared memory segment.
 */
	if (!dsm_Init())
		return (FALSE);
/*
 * Set up the platform lookup table.
 */
	ds_InitPFTable();
	for (i = 0; i < MAXPLAT; i++)
		ApplFuncs[i] = 0;
/*
 * Join the data store group.
 */
	msg_join("DataStore");
	F_Init ();
	msg_AddProtoHandler(MT_DATASTORE, ds_DSMessage);

	return (TRUE);
}





static void
ds_InitPFTable ()
/*
 * Create the platform lookup table.
 */
{
	int             i;
	char           *slash, *strchr ();
	SValue          v;
/*
 * Create the table itself.
 */
	Pf_Names = usy_c_stbl ("Platform_names");
/*
 * Just go through the platform list and make all the entries.
 */
	dsm_ShmLock ();
	for (i = 0; i < SHeader->sm_nPlatform; i++)
	{
		v.us_v_int = i;
		usy_s_symbol (Pf_Names, PTable[i].dp_name, SYMT_INT, &v);
		slash = strchr (PTable[i].dp_name, '/');
		while (slash)
		{
			usy_s_symbol (Pf_Names, slash + 1, SYMT_INT, &v);
			slash = strchr (slash + 1, '/');
		}
	}
	dsm_ShmUnlock ();
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

	if (!usy_g_symbol (Pf_Names, name, &type, &v))
		return (BadPlatform);
	return (v.us_v_int);
}





char *
ds_PlatformName(id)
PlatformId id;
/*
 * Get back the name for this platform.
 */
{
	return (PTable[id].dp_name);
}



int
ds_IsMobile(id)
PlatformId id;
/*
 * Return TRUE iff this is a mobile platform.
 */
{
	return (PTable[id].dp_flags & DPF_MOBILE);
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
	int             dfindex;
/*
 * Find the data file holding the observation of interest, then pass
 * off the real work to DFA.
 */
	if ((dfindex = ds_FindDF (pid, when)) < 0)
		return (0);
	return (dfa_GetObsSamples (dfindex, times, locs, max));
}



static int
ds_FindDF (pid, when)
PlatformId pid;
ZebTime *when;
/*
 * Find the first datafile entry before this time.
 */
{
	int ret = LOCALDATA (PTable[pid]);

	for (; ret; ret = DFTable[ret].df_FLink)
		if (TC_LessEq (DFTable[ret].df_begin, *when))
			return (ret);
/*
 * If we didn't find the data locally, see if there's anything in the
 * remote data table.
 */
	for (ret = REMOTEDATA(PTable[pid]); ret; ret = DFTable[ret].df_FLink)
		if (TC_LessEq (DFTable[ret].df_begin, *when))
			return (ret);
	return (-1);
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
/*
 * Find the first datafile which works.
 */
	if ((df = ds_FindDF (pid, when)) < 0)
		return (0);
/*
 * Now return some times.
 */
	for (i = 0; i < ntime && df;)
	{
		if (!attr || ds_AttrCheck (df, attr))
		{
			*times++ = DFTable[df].df_begin;
			i++;
		}
		df = DFTable[df].df_FLink;
	}
	return (i);
}




static int
ds_AttrCheck (df, attr)
int df;
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
	if (! (dattr = dfa_GetAttr (df, &DFTable[df].df_begin, &len)))
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
	if ((dfindex = ds_FindDF (plat, t)) < 0)
		return (0);
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
 */
{
	int ndone = 0, index, last = 0;
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
		if ((index = ds_FindDF (platform, when)) < 0)
			return (0);
	/*
	 * Now we plow through datafile entries until we have all we
	 * want.
	 */
		while (index && ndone < n)
		{
			ndone += dfa_DataTimes (index, when, which, n - ndone,
					       rettimes + ndone);
			index = DFTable[index].df_FLink;
		}
		return (ndone);
/*
 * We now do DsAfter too.
 */
	   case DsAfter:
	/*
	 * Scan down the datafile list until we find the first entry
	 * which does not end after the time of interest.
	 */
		for (index = LOCALDATA(PTable[platform]); index;
					index = DFTable[index].df_FLink)
		{
			if (TC_LessEq (DFTable[index].df_end, *when))
				break;
			last = index;
		}
	/*
	 * Check the remote table too if need be.
	 */
		if (!index)
			for (index = REMOTEDATA(PTable[platform]); index;
			     index = DFTable[index].df_FLink)
			{
				if (TC_LessEq (DFTable[index].df_end, *when))
					break;
				last = index;
			}
	/*
	 * If we are still pointing at an entry, move back forward
	 * one.  Else start with the last entry in the list.
	 */
		if (index)
			index = DFTable[index].df_BLink;
		else if (!(index = last))
			return (0);
	/*
	 * Now we move forward filling the array.
	 */
		for (; index && ndone < n; index = DFTable[index].df_BLink)
			ndone += dfa_DataTimes(index, when, which, n - ndone,
					       rettimes + n - ndone - 1);
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
		msg_ELog (EF_PROBLEM, "Only DsBefore TimeSpec handled");
		return (0);
	}
}






bool
ds_GetRgridParams (pid, when, loc, rg)
PlatformId pid;
ZebTime *when;
Location *loc;
RGrid *rg;
/*
 * Get the rgrid params for this date.
 */
{
	Platform *p = PTable + pid;
	int dfindex;
/*
 * Make sure this makes sense.
 */
	if (p->dp_org != Org1dGrid && p->dp_org != Org2dGrid &&
					p->dp_org != Org3dGrid)
		return (FALSE);
/*
 * Now find a datafile entry we can use.
 */
	if ((dfindex = ds_FindDF (pid, when)) < 0)
		return (FALSE);
/*
 * Get the rest from the format-specific code.
 */
	loc->l_alt = 0;
	return (dfa_InqRGrid (dfindex, loc, rg));
}






DataOrganization
ds_PlatformDataOrg(pid)
PlatformId pid;
/*
 * Return the organization of the data returned by this platform.
 */
{
	return (PTable[pid].dp_org);
}



int
ds_DSMessage(msg)
struct message *msg;
/*
 * Deal with data store protocol messages.
 */
{
	struct dsp_Template *dt = (struct dsp_Template *) msg->m_data;

	switch (dt->dsp_type)
	{
	/*
	 * If they've gone and deleted data on us, we have to make
	 * sure that we close the file and forget about it.
	 */
	    case dpt_DataGone:
		dfa_ForceClose (((struct dsp_DataGone *) dt)->dsp_file);
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
ds_DeleteData(platform, leave)
PlatformId platform;
int leave;
/*
 * Zap all data from "platform", leaving only "leave" seconds worth.
 */
{
	struct dsp_DeleteData del;

	del.dsp_type = dpt_DeleteData;
	del.dsp_plat = platform;
	del.dsp_leave = leave;
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &del, sizeof(del));
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
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &req, sizeof(req));
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
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &req, sizeof (req));
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
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &req, sizeof(req));
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
	GetList *get, *gp;
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
 */
	for (gp = get; gp; gp = gp->gl_next)
		dfa_GetData (dc, gp, details, ndetail);
	dgl_ReturnList (get);
	return (dc);
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
/*
 * Make the get list describing where this data has to come from.  Then 
 * expand it to cover the entire file.
 */
	if (! (get = dgl_MakeGetList (pid, when, when)))
	{
		msg_ELog (EF_DEBUG, "GetList get failure");
		return (NULL);
	}
	get->gl_begin = DFTable[get->gl_dfindex].df_begin;
	get->gl_end = DFTable[get->gl_dfindex].df_end;
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
	int nsample, sample, dfile, nnew = 0, now = 0, olddf = -1, ndone = 0;
	WriteCode wc;
/*
 * For now (and maybe forever) we do the writing one sample at a time,
 * to ease the process of figuring out what goes where.
 */
	nsample = dc_GetNSample (dc);
	for (sample = 0; sample < nsample; sample++)
	{
	/*
	 * Find a feasible location for this data.
	 */
		if (! ds_FindDest (dc, sample, &dfile, &wc,
						newfile && (sample == 0)))
			continue;	/* Sigh */
	/*
	 * If a new file is called for, create it.  Then write the data.
	 */
	 	if (wc == wc_NewFile)
		{
			if ((dfile = ds_MakeNewFile (dc, sample)) < 0)
				break;	/* Bail completely */
			wc = wc_Append; /* Now that the file is around */
		}
	/*
	 * Now we just shove the sample out.
	 */
		if (dfa_PutSample (dfile, dc, sample, wc))
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
	 * Fill in the daemon on what we have done.
	 */
	 	ds_NotifyDaemon (dfile, dc, now, nnew, sample, 
			sample == (nsample - 1));
		now = nnew = 0;
	}
/*
 * Final daemon update.
 */
	return (ndone == nsample);
}






static int
ds_FindDest (dc, sample, dfile, wc, newfile)
DataChunk *dc;
int sample, *dfile, newfile;
WriteCode *wc;
/*
 * Try to find an appropriate destination for this datum.
 * Return value is TRUE iff it was possible.
 */
{
	int df = LOCALDATA (PTable[dc->dc_Platform]);
	DataFile *dp;
	ZebTime when, dftime;
/*
 * Find the first file in the local list which begins before the time
 * of interest.
 */
	dc_GetTime (dc, sample, &when);
	for (; df; df = DFTable[df].df_FLink)
		if (TC_LessEq (DFTable[df].df_begin, when))
			break;
/*
 * If there is none, then this data predates anything we have, so we
 * just return a new file case.
 */
	if (! df)
	{
		*dfile = -1;
		*wc = wc_NewFile;
		return (TRUE);
	}
/*
 * See if the datum actually falls after the end of this dfile (most common
 * case).  If so, we either append or newfile.
 */
	dp = DFTable + df;
	*dfile = df;
	if (TC_Less (dp->df_end, when))
	{
		if (! newfile &&
			 dp->df_nsample < PTable[dp->df_platform].dp_maxsamp &&
			 ds_SameDay (&when, &dp->df_end) &&
			 ! dp->df_archived)
			*wc = wc_Append;
		else
			*wc = wc_NewFile;
		return (TRUE);
	}
/*
 * The simple cases are not to be.  Now we have to see whether we need to be
 * overwriting data, or stuffing it in between.
 */
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
ds_MakeNewFile (dc, sample)
DataChunk *dc;
int sample;
/*
 * Make a new file that will contain this DC and sample.
 */
{
	char fname[256];
	int newdf;
	PlatformId plat = dc->dc_Platform;
	ZebTime when;
/*
 * Create the new file name and tell the daemon what we have in mind
 * to do.
 */
	dc_GetTime (dc, sample, &when);
	dfa_MakeFileName (PTable + plat, &when, fname);
	if ((newdf = ds_RequestNewDF (plat, fname, &when)) < 0)
		return (-1);
/*
 * Have DFA get the file made for us.  They use the data object to know which
 * fields/platforms belong therein.  A bit kludgy, but it works.
 */
	if (! dfa_CreateFile (newdf, dc, &when))
	{
		ds_AbortNewDF (plat, newdf);
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
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &dspcf, sizeof (dspcf));
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
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &abort, sizeof (abort));
}





static void
ds_NotifyDaemon (dfile, dc, now, nnew, sample, last)
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
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &update, sizeof(update));
/*
 * Then let DFA know that we've signalled a revision on this file.
 */
	dfa_NoteRevision (dfile);
/*
 * Wait for the update ack.
 */
	msg_Search (MT_DATASTORE, ds_AwaitAck, 0);
}





static int
ds_AwaitAck (msg, junk)
Message *msg;
int junk;
/*
 * See if this is our ack.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;

	return (tmpl->dsp_type != dpt_R_UpdateAck);
}





int
ds_GetDetail (key, details, ndetail, v)
char *key;
dsDetail *details;
int ndetail;
SValue *v;
/*
 * Try to find this detail value in the list, returning TRUE iff it is
 * there.
 */
{
	for (; ndetail > 0; details++, ndetail--)
		if (! strcmp (key, details->dd_Name))
		{
			*v = details->dd_V;
			return (TRUE);
		}
	return (FALSE);
}


