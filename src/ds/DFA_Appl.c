/*
 * The data store application interface to the data file access layer.
 * Appl.c holds only the message protocol layer and interface functions
 * which do not need DFA or file format libraries.
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
#include "dfa.h"
#include "Appl.h"

RCSID ("$Id: DFA_Appl.c,v 3.1 1995-07-06 10:21:21 granger Exp $")

/*
 * Local private prototypes.
 */
static int	ds_DFAMessage FP ((struct message *msg));
static int      ds_AttrCheck FP ((int, ZebTime *, char *));
static void     ds_NotifyDaemon FP ((ClientPlatform *, int, DataChunk *, 
				     int, int, int, int));
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
static void 	ds_FProcGetList FP ((DataChunk *, GetList *, dsDetail *, int));
static void	ds_FindBlock FP((int dfile, int dfnext, 
				 DataChunk *dc, ClientPlatform *p,
				 int sample, WriteCode wc, int *nsample));
static int	ds_DataChain FP ((ClientPlatform *p, int which));

/*
 * This is the fine line dividing the sides of the interface.  Once a program
 * calls an interface routine which may require dfa, the dfa message handler
 * gets installed and this variable gets set to non-zero.  Otherwise a 
 * program can merrily call all of the routines in Appl.c and never need
 * the dfa message handler in this file or any of the dfa routines or
 * modules.  The macro is a convenience for setting this flag at each of
 * the entry points.  We could inline this, but it's not complicated and
 * we really want to be sure it doesn't generate any extra calls.
 */
static int DFAInstalled = 0;

#define InstallDFA \
{ if (!DFAInstalled) \
  { DFAInstalled = 1; \
    msg_AddProtoHandler (MT_DATASTORE, ds_DFAMessage); } \
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
	InstallDFA;
	return (dfa_GetObsSamples (dfindex, times, locs, max));
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
	InstallDFA;
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
	InstallDFA;
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
	InstallDFA;
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
		if ((index = ds_FindAfter (platform, when)) < 0)
			return (0);
	/*
	 * Now we move forward filling the array.
	 */
		ds_LockPlatform (platform);
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
	InstallDFA;
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
	int dfindex;
/*
 * First make sure it's a model platform
 */
	InstallDFA;
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




static int
ds_DFAMessage (msg)
struct message *msg;
/*
 * Deal with data store protocol messages, some of which may require dfa
 * routines, such as to close deleted files or sync changed ones.  We only
 * do dfa-related work here.  The message is then passed on to ds_DSMessage
 * to do the rest of the work. 
 */
{
	struct dsp_Template *dt = (struct dsp_Template *) msg->m_data;
	struct dsp_DataGone *ddg;

	switch (dt->dsp_type)
	{
	/*
	 * If they've gone and deleted data on us, we have to make
	 * sure that we close the file and forget about it.  The rest
	 * of the work, such as invalidating the cache, is handled
	 * by the non-DFA handler, ds_DSMessage().
	 */
	   case dpt_DataGone:
		ddg = (struct dsp_DataGone *) dt;
		dfa_ForceClose (ddg->dsp_file);
		break;
	   default:
		break;
	}
	return (ds_DSMessage (msg));
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
	InstallDFA;
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
	InstallDFA;
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
	InstallDFA;
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
	InstallDFA;
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





static void
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
 * Return TRUE iff the two times are on the same day, 
 * of the same month and year
 */
{
	int y1, y2, m1, m2, d1, d2;

	TC_ZtSplit (t1, &y1, &m1, &d1, 0, 0, 0, 0);
	TC_ZtSplit (t2, &y2, &m2, &d2, 0, 0, 0, 0);
	return ((d1 == d2) && (m1 == m2) && (y1 == y2));
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



void
ds_ForceClosure()
/*
 * Release whatever memory we have been holding onto, from open files,
 * to DataChunk free chains, to GetList free chains, to cached structures.
 */
{
	InstallDFA;
	dfa_ForceClosure();
	dc_ForceClosure();
	dgl_ForceClosure();
	ds_FreeCache();
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

	InstallDFA;
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
	return (ds_InsertFile (platid, filename, &begin, &end, 
			       nsample, local));
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
	int df;
/*
 * Get a write lock and our platform structure.
 */
	InstallDFA;
	ds_WriteLock (platid);
	ds_GetPlatStruct (platid, &p, TRUE);
/*
 * Make sure the bounds of this file do not overlap any existing files.  Find
 * the first file which begins before our end time.  If there is such a file
 * make sure it ends before our begin time.  Otherwise we have a conflict and
 * fail.
 */
	ds_LockPlatform (platid);
	df = (local) ? ds_DataChain (&p, 0) : ds_DataChain (&p, 1);
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




static int
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


