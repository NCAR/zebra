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

#include <stdio.h>
#include <string.h>
#include <math.h>	/* for HUGE_VAL */

#include <defs.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "dsPrivate.h"
#include "GetList.h"
#include "dslib.h"
#include "dfa.h"
#include "Appl.h"

RCSID ("$Id: DFA_Appl.c,v 3.18 2001-10-16 22:26:28 granger Exp $")

/*
 * Local private prototypes.
 */
static int	ds_DFAMessage (struct message *msg);
static int      ds_AttrCheck (const DataFile *df, const ZebraTime *t, 
			      char *attr);
static void 	ds_NotifyDaemon (const Platform *p, const DataFile *df, 
				 int now, int nnew, int sample, int last);
static const DataFile* ds_FindDest (DataChunk *dc, const Platform *plat, 
				    int sample, WriteCode *wc, int newfile, 
				    const ZebraTime *now);
static zbool	ds_SameDay (const ZebraTime *, const ZebraTime *);
static int	ds_MakeNewFile (DataChunk *dc, const Platform *plat, 
				int sample, dsDetail *details, int ndetail, 
				DataFile *df);
static int	ds_RequestNewDF (PlatformId plat, const char *file, 
				 const ZebraTime *t, DataFile *df);
static int	ds_GetNDFResp (struct message *, struct dsp_R_CreateFile *);
static int	ds_AwaitAck (Message *, int);
static void 	ds_FProcGetList (DataChunk *, GetList *, dsDetail *, int);
static void	ds_FindBlock (const DataFile *dfp, const DataFile *dfnext, 
			      DataChunk *dc, const Platform *plat, int sample, 
			      WriteCode wc, int *block_size);
static DataChunk* Derive (PlatformId pid, DataClass class, GetList *get, 
			  FieldId *flds, int nflds, dsDetail *details, 
			  int ndetail);
static void	DeriveFld (DataChunk *dest_dc, DataChunk *extra_dc, 
			   FieldId fld, DerivMethod deriv);
static void	DerivationFailure (DataChunk *dest_dc, FieldId fld);
static zbool	DerivationCheck (PlatformId pid, DataClass class, 
				 GetList *get, FieldId *flds, int nflds);
static zbool	FldAvailable (FieldId fld, PlatformId pid, GetList *get, 
			      zbool *is_raw, DerivMethod *return_deriv);

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
    if (! Standalone) \
      msg_AddProtoHandler (MT_DATASTORE, ds_DFAMessage); } \
}

/* 
 * max # of fields we can handle from any given file
 */
# define MAXRAWFLDS 200

	 

int
ds_GetObsSamples (PlatformId pid, const ZebraTime *when, ZebraTime *times, 
		  Location *locs, int max)
/*
 * Get the time and location of up to "max" samples from the observation
 * enclosing "when".
 */
{
	const DataFile *df;
	int result = 0;
/*
 * Find the data file holding the observation of interest, then pass
 * off the real work to DFA.
 */
	if ((df = ds_FindBefore (pid, when)))
	{
		InstallDFA;
		result = dfa_GetObsSamples (df, times, locs, max);
	}
	return (result);
}






int
ds_GetObsTimes (pid, when, times, ntime, attr)
PlatformId pid;
ZebraTime *when, *times;
int ntime;
char *attr;
/*
 * Return the times for which observations are available.  Optionally test
 * against attributes.
 */
{
	int i;
	const DataFile *df;
/*
 * Find the first datafile which works.
 */
	if ((df = ds_FindBefore (pid, when)) < 0)
		return (0);
/*
 * Now return some times.
 */
	InstallDFA;
	for (i = 0; i < ntime && df;)
	{
		if (!attr || ds_AttrCheck (df, &df->df_core.dfc_begin, attr))
		{
			*times++ = df->df_core.dfc_begin;
			i++;
		}
		df = ds_PrevFile (df);
	}

	return (i);
}



#ifndef NO_GETATTR
static int
ds_AttrCheck (const DataFile *df, const ZebraTime *t, char *attr)
/*
 * See if this attribute is found in the data.
 */
{
	char *dattr;
	char *a;
	int len;
	int result = 0;
/*
 * If no data attrs, assume yes.
 */
	if (! (dattr = dfa_GetAttr (df, t, &len)))
		return (TRUE);
/*
 * Parse up the attributes and see if any match.  Technically
 * here we're comparing against both keys and values, but since
 * this is mostly a kludge for radar images anyway...
 *
 * Accept both nulls and commas as separators, for backwards
 * compatibiliity.
 */
	a = dattr;
	while (a - dattr < len)
	{
		char *comma;
		if ((comma = strchr (a, ',')))
			*comma = '\0';
		if ((result = (!strcmp (a, attr))))
			break;
		a += strlen (a) + 1;
	}
	free (dattr);
	return (result);
}
#endif



#ifdef NO_GETATTR
static int
ds_AttrCheck (df, t, attr)
DataFile *df;
ZebraTime *t;
char *attr;
/*
 * See if this attribute is found in the data.
 */
{
	DataChunk *dc;
	int result;
/*
 * If no data, assume yes.
 */
	if (! (dc = ds_Fetch (pid, DCC_Transparent, t, t, NULL, 0, NULL, 0)))
		return (TRUE);
/*
 * Otherwise check for the attribute
 */
	result = (dc_GetSampleAttr (dc, 0, attr) != NULL);
	dc_DestroyDC (dc);
	return (result);
}
#endif



inline static void
CheckDetails (details, ndetail)
dsDetail *details[];
int *ndetail;
{
	if (! *details || *ndetail < 1)
	{
		*details = NULL;
		*ndetail = 0;
	}
}




int
ds_GetFields (plat, t, nfld, flist)
PlatformId plat;
const ZebraTime *t;
int *nfld;
FieldId *flist;
/*
 * Return a list of the available fields in this platform at this time.
 */
{
        const PlatformClass *pc;
	const DataFile *df;
	int result;
	int max = *nfld;
/*
 * Find a file entry to look at.
 */
	if ((df = ds_FindBefore (plat, t)) == NULL)
	{
		*nfld = 0;
		return (0);
	}
/*
 * Have the format driver actually look.
 */
	InstallDFA;
	result = dfa_GetFields (df, t, nfld, flist);
/*
 * Now merge the list in the file with any defined for the platform.
 */
	if ((pc = dt_FindClass (ds_PlatformClass (plat))) != 0)
	{
	    int i;
	    for (i = 0; i < pc->dpc_nfields; ++i)
	    {
		/* Look for the name, and if it matches any field found
		 * already, override it.  Else add this field to the end.
		 */
		int found = 0;
		int f;
		char fname[256]; /* XXX */
		strcpy (fname, F_GetName (pc->dpc_fields[i]));
		for (f = 0; fname[0] && f < *nfld; ++f)
		{
		    if (! strcmp (fname, F_GetName (flist[f])))
		    {
			flist[f] = pc->dpc_fields[i];
			found = 1;
		    }
		}
		if (! found && *nfld < max)
		{
		    flist[(*nfld)++] = pc->dpc_fields[i];
		}
	    }
	}
	return (result);
}




static int
ds_QualifyTimes (df, which, times, n, key, value)
DataFile *df;
TimeSpec which;
ZebraTime *times;
int n;
char *key;
char *value;
/*
 * Given a list of times for the given datafile, remove the times
 * which do not match the attributes and return the new number of times.
 * Shift in the direction indicated by 'which'.
 */
{
	int i, skip, dir;

	if (n == 0 || !times || (!key && !value))
		return (n);

	switch (which) 
	{
	case DsBefore:
		i = 0;
		dir = 1;
		break;

	case DsAfter:
		i = n - 1;
		dir = -1;
		break;

	default:
		return (n);
	}

	skip = 0;
	for ( ; i >= 0 && i < n; )
	{
		if (! ds_AttrCheck (df, times+i, value))
		{
			--n;
			skip += dir;
		}
		else
		{
			i += dir;
		}
		if ((skip != 0) && (i+skip < n+skip) && (i+skip >= 0))
			times[i] = times[i+skip];
	}
	return (n);
}




int
ds_DataTimes (platform, when, n, which, rettimes)
PlatformId platform;
ZebraTime *when, *rettimes;
int n;
TimeSpec which;
/*
 * The same old usual DataTimes routine, except we pass the work on to
 * AttrTimes with no attributes.
 */
{
	return (ds_AttrTimes(platform, when, n, which, NULL, NULL, rettimes));
}




int
ds_AttrTimes (platform, when, n, which, key, value, rettimes)
PlatformId platform;
ZebraTime *when;
int n;
TimeSpec which;
char *key;		/* if null, value checked as key or value */
char *value;		/* simple string comparison for now */
ZebraTime *rettimes;
/*
 * Return a list of up to "n" times related to "time" by the given spec.
 * The times returned can be qualified an attribute key and/or value.
 *
 * Reworked for new non-SHM scheme.  This routine fetches more DFE's than
 * might really be desired, but so it goes.
 *
 * "n" is the limit of the samples to search, rather than return, else
 * we might search forever looking for a single qualified sample.
 */
{
	int ndone = 0;
	int nseen = 0;
	int max = n;
	const DataFile *df;
/*
 * We don't do it all yet.
 */
	InstallDFA;

	if (key && value)
	{
		msg_ELog (EF_PROBLEM, "don't do key & value attr checks yet");
		key = NULL;
	}
	switch (which) {
	/*
	 * Handle dsBefore -- the usual case.
	 */
	   case DsBefore:
	/*
	 * Scan down the datafile list until we find the first entry
	 * which begins before the given time.
	 */
		if ((df = ds_FindBefore (platform, when)) == NULL)
			return (0);
	/*
	 * Now we plow through datafile entries until we have all we
	 * want.
	 */
		do {
			int nnew;
			nnew = dfa_DataTimes (df, when, which, n - ndone,
					      rettimes + ndone);
			nseen += nnew;
			nnew = ds_QualifyTimes (df, which, 
						rettimes + ndone, nnew,
						key, value);
			ndone += nnew;
		}
		while (ndone < n && nseen < max && 
		       (df = ds_PrevFile(df)) != 0);
		return (ndone);
/*
 * We now do DsAfter too.
 */
	   case DsAfter:
	/*
	 * Get positioned.
	 */
		if ((df = ds_FindAfter (platform, when)) == NULL)
			return (0);
	/*
	 * Now we move forward filling the array.  The loop test at the
	 * end is meant to shortcut finding another file in the chain
	 * when the loop will be exited for other reasons.
	 */
		do {
			int nnew;
			nnew = dfa_DataTimes (df, when, which, n - ndone,
					      rettimes + n - ndone - 1);
			nseen += nnew;
			nnew = ds_QualifyTimes (df, which,
						rettimes + n - ndone - nnew,
						nnew, key, value);
			ndone += nnew;
		}
		while (ndone < n && nseen < max && 
		       (df = ds_NextFile (df)) != 0);
	/*
	 * If we couldn't do it all, copy what we could do forward.
	 */
		if (ndone && ndone < n)
			memcpy (rettimes, rettimes + n - ndone,
			       (n - ndone) * sizeof (ZebraTime));
		return (ndone);
	/*
	 * But that's all.
	 */
	   default:
		msg_ELog (EF_PROBLEM,
			"Only DsBefore and DsAfter TimeSpec handled");
		return (0);
	}
}




zbool
ds_GetAlts (pid, fid, when, offset, alts, nalts, altunits)
PlatformId pid;
FieldId	fid;
ZebraTime	*when;
int	offset;
float	*alts;
int	*nalts;
AltUnitType *altunits;
/*
 * Get the heights for this time, field, and forecast offset.
 */
{
	const DataFile *df;
	GetList gl;
	zbool is_raw;
	DerivMethod deriv;
	FieldId check_fid = fid;
/*
 * Now find a datafile entry we can use.
 */
	InstallDFA;

	if (! (df = ds_FindBefore (pid, when)))
	    return (FALSE);
/*
 * Check for availability (including derivation if necessary) of the 
 * chosen field.
 *
 * KLUGE: we build a fake GetList (with the datafile we found above
 * as its only entry) to call FldAvailable().
 */
	gl.gl_df = *df;
	gl.gl_begin = *when;
	gl.gl_end = *when;
	gl.gl_flags = 0;
	gl.gl_next = (GetList*) 0;

	if (! FldAvailable (fid, pid, &gl, &is_raw, &deriv))
	{
	    msg_ELog (EF_DEBUG, 
		      "ds_GetAlts: Field %s/%s unavailable; no alts returned.",
		      ds_PlatformName (pid), F_GetFullName (fid));
	    return (FALSE);
	}
/*
 * If the field has to be derived, we return alts for the first of the 
 * raw source fields that will be used for the derivation.
 */
	if (! is_raw)
	{
	    FieldId *srcflds;
	    int nsrcflds;
	    
	    srcflds = ds_DerivNeededFields (deriv, &nsrcflds);
	    check_fid = srcflds[0];
	    free (srcflds);
	}
/*
 * Get the rest from the format-specific code.
 */
	return (dfa_GetAlts (df, check_fid, offset, alts, nalts, altunits));
}




zbool
ds_GetForecastTimes (pid, when, times, ntimes)
PlatformId	pid;
ZebraTime	*when;
int	*times, *ntimes;
/*
 * Get the heights for this time.
 */
{
	const DataFile *df;
	int result;
/*
 * First make sure it's a model platform
 */
	InstallDFA;
	if (! ds_IsModelPlatform (pid))
		return (FALSE);
/*
 * Now find a datafile entry we can use.
 */
	if ((df = ds_FindBefore (pid, when)))
	{
		/*
		 * Get the rest from the format-specific code.
		 */
		result = dfa_GetForecastTimes (df, times, ntimes);
	}
	return (result);
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
		dfa_ForceClose (&ddg->dsp_file);
		break;
	   default:
		break;
	}
	return (ds_DSMessage (msg));
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
/*
 * Recurse down to the end of the list
 */
    if (gp->gl_next)
	ds_FProcGetList (dc, gp->gl_next, details, ndetail);
#ifdef DEBUG
    {
	char btime[80], etime[80];
		
	TC_EncodeTime (&gp->gl_begin, TC_Full, btime);
	TC_EncodeTime (&gp->gl_end, TC_Full, etime);
	msg_ELog (EF_DEBUG, "ds_FProcGetList: %s, %s to %s",
		  gp->gl_df.df_fullname, btime, etime);
    }
#endif
/*
 * Get the data from this getlist entry's data file.
 */
    dfa_GetData (dc, gp, details, ndetail);
}



static DataChunk *
ds_FetchData (pid, class, obs, begin, end, fields, nfield, details, ndetail)
PlatformId pid;
DataClass class;
int obs;		/* non-zero if fetching entire, single obs */
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
 * 	ALLOW_DERIVATION is true iff field derivation is to be allowed
 * Exit:
 *	If any data could be found then
 *		The return value is a data chunk containing that data
 *	else
 *		The return value is NULL.
 */
{
    DataChunk *return_dc;
    GetList *get;
/*
 * Make the get list describing where this data has to come from.
 */
    InstallDFA;
    CheckDetails (&details, &ndetail);
    if (! (get = dgl_MakeGetList (pid, begin, end)))
    {
	msg_ELog (EF_DEBUG, "GetList get failure");
	return (NULL);
    }
    if (obs)
    {
	get->gl_begin = get->gl_df.df_core.dfc_begin;
	get->gl_end = get->gl_df.df_core.dfc_end;
    }
/*
 * Pass the work off to Derive() if we're building a MetData datachunk
 * and at least one of the fields needs derivation.
 */
    if (DerivationCheck (pid, class, get, fields, nfield))
    {
	return_dc = Derive (pid, class, get, fields, nfield, details, ndetail);
    }
/*
 * Otherwise, we just use the old non-derivation method...
 */
    else
    {
    /*
     * Now it is up to the format driver to get ready and create a data 
     * chunk for us.
     */
	if (! (return_dc = dfa_Setup (get, fields, nfield, class)))
	{
	    msg_ELog (EF_DEBUG, "Setup failure");
	    dgl_ReturnList (get);
	    return (NULL);
	}
	dc_SetPlatform (return_dc, pid);
    /*
     * Finally, fill the data chunk.  For MetData data chunks, try to derive 
     * data if possible.
     */
	ds_FProcGetList (return_dc, get, details, ndetail);
	dgl_ReturnList (get);
    }
    
/*
 * Clean up
 */
    if (dc_GetNSample (return_dc) == 0)
    {
	dc_DestroyDC (return_dc);
	return (NULL);
    }
/*
 * Done
 */
    dc_ProcessDetails (return_dc, details, ndetail);
    return (return_dc);
}



DataChunk *
ds_Fetch (pid, class, begin, end, fields, nfield, details, ndetail)
PlatformId pid;
DataClass class;
ZebraTime *begin, *end;
FieldId *fields;
int nfield, ndetail;
dsDetail *details;
{
	return (ds_FetchData (pid, class, FALSE, begin, end, fields, nfield, 
			      details, ndetail));
}





DataChunk *
ds_FetchObs (pid, class, when, fields, nfield, details, ndetail)
PlatformId pid;
DataClass class;
ZebraTime *when;
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
	return (ds_FetchData (pid, class, TRUE, when, when, fields, nfield, 
			      details, ndetail));
}




/*
 * WHEREAS, I am lazy and tired of keeping two (2) storage algorithms
 * up-to-date and error free, and I have recently made fixes to
 * ds_StoreBlocks making it more complete and effective, and
 *
 * INASMUCH as the two routines ds_Store and ds_StoreBlocks have identical
 * functionality, and
 *
 * WHEREAS ds_StoreBlocks has been thoroughly tested and stable for quite
 * some time now, and
 *
 * LASTLY, being that ds_StoreBlocks is in general significantly more
 * efficient and faster and always at least as fast as the original
 * ds_Store implementation,
 *
 * be it DECREED that the original ds_Store is no longer compiled and is
 * now instead simply a direct call to ds_StoreBlocks.
 *
 * Be it so NOTED this 9th day of November, in the year 1995 A.D. 
 */
zbool
ds_Store (dc, newfile, details, ndetail)
DataChunk *dc;
zbool newfile;
dsDetail *details;
int ndetail;
{
	return (ds_StoreBlocks (dc, newfile, details, ndetail));
}



zbool
ds_StoreBlocks (dc, newfile, details, ndetail)
DataChunk *dc;
zbool newfile;
dsDetail *details;
int ndetail;
/*
 * Store samples in the largest blocks possible, while consolidating 
 * notifications to the daemon as much as possible.
 */
{
    int nsample, sample, endsamp = 0;
    const DataFile *dfp, *next;
    DataFile df;
    int ndone;		/* Total number of samples successfully stored	*/
    int block_size;
    WriteCode wc;
    const Platform *p;
    ZebraTime curtime;
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
    CheckDetails (&details, &ndetail);
/*
 * Setup time.
 */
    p = dt_FindPlatform (dc->dc_Platform);

    if (! Standalone)
	tl_Time (&curtime);
    else
	TC_SysToZt (time(0), &curtime);
/*
 * Start plowing through the data.
 */
    nsample = dc_GetNSample (dc);
    ndone = 0;
    sample = 0;

    while (sample < nsample)
    {
    /*
     * Find a feasible location for the next sample of the data chunk.
     */
	dfp = ds_FindDest (dc, p, sample, &wc, newfile && (sample == 0),
			   &curtime);
    /*
     * Do some WriteCode-specific stuff
     */
	if (wc == wc_SkipIt)
	{
	/*
	 * Skip the sample if we're told to
	 */
	    ++sample;
	    continue;	/* go on with the next sample */
	}
	else if (wc == wc_NewFile)
	{
	/*
	 * If a new file is called for, create it and tell the dsDaemon.
	 */
	    if (! ds_MakeNewFile (dc, p, sample, details, ndetail, &df))
		break;	/* Bail completely */

	    wc = wc_Append;	/* Now that the file is around */
	/*
	 * What's the next file after our file, if any?  We use ds_FindFile()
	 * here rather than ds_NextFile(df), since the daemon doesn't know
	 * about our new file yet.
	 */
	    next = ds_FindDFAfter (SRC_DEFAULT_W, pi_Id (p), 
				   &df.df_core.dfc_end);
	}
	else
	{
	/*
	 * Make sure we got a file from ds_FindDest()...
	 */
	    if (! dfp)
	    {
		msg_ELog (EF_PROBLEM, 
			  "ds_StoreBlocks: Good wcode, but no file to write");
		++sample;
		continue;
	    }

	    df = *dfp;
	/*
	 * What's the next file after our file, if any?
	 */
	    next = ds_NextFile (&df);
	}
    /*
     * Find out how many samples can be written to this file
     * as a single block.  The answer is at least one.
     */
	ds_FindBlock (&df, next, dc, p, sample, wc, &block_size);
	msg_ELog ((block_size >= 25) ? EF_DEBUG : EF_DEVELOP,
		  "%s block of %i samples to %s",
		  (wc == wc_Append) ? "appending" : 
		  ((wc == wc_Insert) ? "inserting" : 
		   "overwriting"), block_size, pi_Name (p));
    /*
     * Now we write whatever block we found
     */
	if (dfa_PutBlock (&df, dc, sample, block_size, wc, details, ndetail))
	{
	    ZebraTime endt;
	    int noverwritten = 0, nnew = 0;
	/*
	 * Keep track of successful writes.
	 */
	    ndone += block_size;
	    if (wc == wc_Overwrite)
		noverwritten = block_size;
	    else
	    {
		nnew = block_size;
		df.df_core.dfc_nsample += block_size;
	    }
        /*
         * Update the end time of the file, if necessary
         */
            endsamp = sample + block_size - 1;
            dc_GetTime (dc, endsamp, &endt);
            if (TC_Less (df.df_core.dfc_end, endt))
                df.df_core.dfc_end = endt;
        /*
         * Note that we've made modifications
         */
            ds_NotifyDaemon (p, &df, noverwritten, nnew, endsamp, TRUE);
        }
    /*
     * Move on to the rest of the samples no matter what
     */
        sample += block_size;
    } /* while (sample < nsample) */
/*
 * Release the write lock and we're outta here.
 */
    return (ndone == nsample);
}




static DataChunk*
Derive (PlatformId pid, DataClass class, GetList *gl, FieldId *destflds, 
	int ndestflds, dsDetail *details, int ndetail)
/* 
 * Build, fill, and return a new datachunk based on the given class,
 * getlist, and fields, and deriving fields where necessary and possible.
 * The given class must be MetData or one of its subclasses. 
 */
{
    int nextraflds, nsrcflds, nraw, df;
    FieldId extraflds[MAXRAWFLDS], aliases[MAXRAWFLDS];
    FieldId *srcflds;
    DerivMethod *derivs;
    DataChunk *dest_dc, *extra_dc;
/* 
 * For each destination field:
 *	Get a derivation method (if any).  If the field is available raw,
 *	do nothing special.  Otherwise, if we got a good derivation method, 
 *	then add the source fields necessary for the derivation to our list 
 *	of extra fields.
 */
    derivs = (DerivMethod*) malloc (ndestflds * sizeof (DerivMethod));
    
    nextraflds = 0;
    nraw = 0;

    for (df = 0; df < ndestflds; df++)
    {
	int sf;
	zbool is_raw;
    /*
     * If the field is unavailable (can't be gotten directly or derived),
     * or is available raw, we don't have to deal with extra derivation stuff
     * so just move on.
     */
	derivs[df] = 0;
	aliases[df] = BadField;

	if (! FldAvailable (destflds[df], pid, gl, &is_raw, &derivs[df]) ||
	    is_raw)
	{
	    if (is_raw)
	    {
		nraw++;
		ds_DestroyDeriv (derivs[df]);
		derivs[df] = 0;
	    }
	    continue;
	}
    /*
     * Get the list of source fields needed to derive this destination field
     */
	srcflds = ds_DerivNeededFields (derivs[df], &nsrcflds);
    /* 
     * Check for a simple alias case: we can set up to skip the derivation step
     * for fields that are effectively just renamed raw fields.  Our
     * shortcut is to grab the raw field directly into the destination data
     * chunk and then assign its desired FieldId later.  IMPORTANT NOTE:
     * because a data chunk can only hold one copy of any given field, this
     * shortcut can only work if the required raw field is not already in
     * the destination data chunk. Otherwise, we fall through and use the
     * normal (slow) derivation mechanism. 
     */
	if (ds_DerivIsAlias (derivs[df]))
	{
	    int d;
	    FieldId rawfld = srcflds[0];
	/*
	 * Check all of the current destination fields to make sure the
	 * raw field we want isn't already there.
	 */
	    for (d = 0; d < ndestflds; d++)
		if (rawfld == destflds[d])
		    break;
	/* 
	 * If the desired raw FieldId isn't already in the list of
	 * destination fields, then we can use the shortcut.  Stash the
	 * real destination field into the aliases list, and replace it
	 * in list destfld with the raw field.  The field will be renamed
	 * in the datachunk later.
	 */
	    if (d == ndestflds)
	    {
		char rname[128];
	    /* 
	     * Print a message.  Gotta copy one of the full names, since
	     * the string returned by F_GetFullName() is only valid until
	     * the next call, so we can't have two of them in one
	     * msg_ELog() call... 
	     */
		strcpy (rname, F_GetFullName (rawfld));
		msg_ELog (EF_DEBUG, "Alias shortcut: %s yields %s.  Cool!",
			  rname, F_GetFullName (destflds[df]));
	    /*
	     * Swap the fields around as described above
	     */
		aliases[df] = destflds[df];
		destflds[df] = rawfld;
		nraw++;
	    /*
	     * Get rid of the derivation method
	     */
		ds_DestroyDeriv (derivs[df]);
		derivs[df] = 0;
	    /*
	     * Move on to the next destination field
	     */
		continue;
	    }
	}
    /* 
     * Not raw or a simple alias, so we have to set up for a real derivation.
     * Figure out which source field(s) we need for deriving this
     * destination field, and add those to our list of extra fields,
     * dropping duplicates.
     */
	for (sf = 0; sf < nsrcflds; sf++)
	{
	/*
	 * Just continue if this field is already in the list
	 */
	    int ef;
	    for (ef = 0; ef < nextraflds; ef++)
		if (srcflds[sf] == extraflds[ef])
		    continue;
	/*
	 * Add this field to the end of the list
	 */
	    extraflds[nextraflds++] = srcflds[sf];
	}
	free (srcflds);
    }
/* 
 * KLUGE: If we are doing derivations, we need at least one raw field in
 * the destination data chunk in order to get the data geometry there
 * correct.  If necessary, temporarily replace the first derived field
 * with the first extra field (which we know is raw) via our alias shortcut.
 * We will rename the field to its desired name later, and the derivation 
 * process will overwrite the data. 
 */
    if (nraw == 0 && nextraflds > 0)
    {
	for (df = 0; df < ndestflds; df++)
	{
	    if (derivs[df])
	    {
		aliases[df] = destflds[df];
		destflds[df] = extraflds[0];
		break;
	    }
	}
    }
/*
 * Set up our return datachunk with the destination fields.  Also set up a 
 * data chunk for the extra fields needed for derivations.
 */
    dest_dc = dfa_Setup (gl, destflds, ndestflds, class);
    extra_dc = nextraflds ? dfa_Setup (gl, extraflds, nextraflds, class) : 0;
    
    if (! dest_dc || (nextraflds && ! extra_dc))
    {
	msg_ELog (EF_PROBLEM, "Derive: Error setting up datachunks");

	for (df = 0; df < ndestflds; df++)
	{
	    if (derivs[df])
		ds_DestroyDeriv (derivs[df]);
	}

	free (derivs);
	
	return ((DataChunk*) 0);
    }
/*
 * Get the extra data required for derivations (if any)
 */
    if (extra_dc)
    {
	dc_SetPlatform (extra_dc, pid);
	ds_FProcGetList (extra_dc, gl, details, ndetail);
    }
/*
 * Special handling for derived fields in NSpace datachunks.  The 
 * setup above knew nothing of the geometry for derived fields, so we have 
 * to redefine them here.  For each derived field, we just copy the geometry 
 * from one of the fields to be used in its derivation.
 */
    if (class == DCC_NSpace)
    {
	dc_NSAllowRedefine (dest_dc, 1);
	
	for (df = 0; df < ndestflds; df++)
	{
	    if (derivs[df])
	    {
		int i, ndims, is_static;
		char *names[DC_MaxDimension];
		FieldId dims[DC_MaxDimension];
	    /*
	     * Get the list of fields to be used for the derivation, and
	     * use the first in the list as a template for the derived
	     * field's geometry.
	     */
		srcflds = ds_DerivNeededFields (derivs[df], &nsrcflds);
		dc_NSGetField (extra_dc, srcflds[0], &ndims, names, 0, 
			       &is_static);

		for (i = 0; i < ndims; i++)
		    dims[i] = F_Lookup (names[i]);

		dc_NSDefineVariable (dest_dc, destflds[df], ndims, dims,
				     is_static);
	    }
	}
    }
/*
 * Fill the destination data chunk
 */	
    dc_SetPlatform (dest_dc, pid);
    ds_FProcGetList (dest_dc, gl, details, ndetail);
/*
 * Loop through the fields in the destination data chunk.  Rename fields where
 * the alias shortcut was used.  For destination fields that need to be 
 * derived, perform the derivation and replace their data in the destination 
 * data chunk.
 */
    for (df = 0; df < ndestflds; df++)
    {
	if (aliases[df] != BadField)
	{
	    dc_ChangeFld (dest_dc, destflds[df], aliases[df]);
	    destflds[df] = aliases[df];
	}

	if (derivs[df])
	{
	    DeriveFld (dest_dc, extra_dc, destflds[df], derivs[df]);
	    ds_DestroyDeriv (derivs[df]);
	}
    }
/*
 * Clean up
 */
    free (derivs);
    
    if (extra_dc)
	dc_DestroyDC (extra_dc);    
/*
 * Done
 */
    return (dest_dc);
}




static void
DeriveFld (DataChunk *dest_dc, DataChunk *src_dc, FieldId fld, 
	   DerivMethod deriv)
/*
 * Derive field 'fld' from data in src_dc, using derivation method 'deriv', 
 * and inserting the results into dest_dc.
 */
{
    int nsrcflds, sf, nsample, elemsize, elemcount, samp, firstelem;
    int uniform, *sampsizes, e;
    DC_ElemType elemtype;
    FieldId *srcflds;
    double **dblptrs, *dblresults;
    char *results;
    void *badvp;
    double badval;
    ZebraTime *samptimes;

    msg_ELog (EF_INFO, "Deriving %s", F_GetFullName (fld));
/*
 * Get the list of source fields required for the derivation
 */
    srcflds = ds_DerivNeededFields (deriv, &nsrcflds);
/*
 * How many samples are we deriving?
 * How big are our destination elements?
 */
    nsample = dc_GetNSample (dest_dc);
    elemtype = dc_Type (dest_dc, fld);
    elemsize = dc_SizeOfType (elemtype);
/*
 * Get the bad value flag
 */
    if ((badvp = dc_GetFieldBadval (dest_dc, fld)) != NULL)
	dc_ConvertDouble (&badval, badvp, elemtype);
    else
	badval = (double) dc_GetBadval (dest_dc);
/*
 * Get the size of each destination sample (in elements) and count how many
 * total elements we're deriving.  Check if the sample size is uniform,
 * since it can save us time putting data into the data chunk later. 
 */
    elemcount = 0;
    uniform = 1;

    sampsizes = (int*) malloc (nsample * sizeof (int));
    
    for (samp = 0; samp < nsample; samp++)
    {
	dc_GetMData (dest_dc, samp, fld, &(sampsizes[samp]));
	sampsizes[samp] /= elemsize;
	elemcount += sampsizes[samp];

	if (samp > 0)
	    uniform = uniform && (sampsizes[samp] == sampsizes[samp-1]);
    }
/*
 * Allocate a list to hold arrays of doubles for the source data for the
 * derivation.  Also allocate our sample time array.
 */
    dblptrs = (double**) malloc (nsrcflds * sizeof (double*));

    for (sf = 0; sf < nsrcflds; sf++)
	dblptrs[sf] = (double*) malloc (elemcount * sizeof (double));

    samptimes = (ZebraTime *) malloc (nsample * sizeof (ZebraTime));
/*
 * Run through the source data sample-by-sample, moving the data into
 * contiguous arrays of doubles.
 */    
    firstelem = 0;

    for (samp = 0; samp < nsample; samp++)
    {
    /*
     * Save the time of this sample
     */
	dc_GetTime (dest_dc, samp, samptimes + samp);
    /*
     * Copy the raw data for each source field into arrays of doubles (dblptrs)
     */
	for (sf = 0; sf < nsrcflds; sf++)
	{
	    int nelems, nbytes, src_elemsize;
	    DC_ElemType src_type;
	    DataPtr rawdp;
	    void *badvp;
	    double src_bad;
	/*
	 * Source data type, size per element, and number of data elements 
	 * this sample.
	 */
	    src_type = dc_Type (src_dc, srcflds[sf]);
	    src_elemsize = dc_SizeOfType (src_type);
	    rawdp = dc_GetMData (src_dc, samp, srcflds[sf], &nbytes);

	    nelems = nbytes / src_elemsize;
	/*
	 * Get the bad value for this source field, as a double
	 */
	    if ((badvp = dc_GetFieldBadval (src_dc, srcflds[sf])) != NULL)
		dc_ConvertDouble (&src_bad, badvp, src_type);
	    else
		src_bad = (double) dc_GetBadval (src_dc);
	/* 
	 * We need sample size uniformity among the fields, since the
	 * derivations simply work element-by-element.  If this uniformity
	 * is violated, use a null raw data pointer for the offending
	 * field. 
	 */
	    if (nelems != sampsizes[samp])
	    {
		msg_ELog (EF_PROBLEM, 
			  "DeriveFld: Sample size varies (%d != %d)",
			  nelems, sampsizes[samp]);
	    /*
	     * Fill the field with bad values, clean up, and return
	     */
		DerivationFailure (dest_dc, fld);
		free (samptimes);
		for (sf = 0; sf < nsrcflds; sf++)
		    free (dblptrs[sf]);
		free (dblptrs);
		free (sampsizes);
		free (srcflds);
		return;
	    }
	/*
	 * Put this sample into the double array for this source field.
	 * (Allocate the double array if this is the first sample)
	 */
	    for (e = 0; e < nelems; e++)
	    {
		double val;

		if (! dc_ConvertDouble (&val, (char*)rawdp + e * src_elemsize, 
					src_type) ||
		    val == src_bad)
		    dblptrs[sf][firstelem + e] = badval;
		else
		    dblptrs[sf][firstelem + e] = val;
	    }

	}

	firstelem += sampsizes[samp];
    }
/*
 * OK, now we have all the raw needed data in big arrays of doubles.
 * We can do our derivation.
 */
    dblresults = (double *) malloc (elemcount * sizeof (double));
    ds_DoDerivation (deriv, srcflds, nsrcflds, elemcount, dblptrs, dblresults, 
		    badval);
/* 
 * Convert to the necessary element type before stashing in the data chunk
 */
    results = (char *) malloc (elemcount * elemsize);

    for (e = 0; e < elemcount; e++)
	dc_DoubleToType ((void*)(results + e * elemsize), elemtype, 
			 dblresults[e]);
/*
 * We can now stash the derived data.  Data stash can happen as a unit for 
 * uniform samples, otherwise must happen sample by sample.
 */
    if (uniform)
    {
	if (! dc_AddMData (dest_dc, samptimes, fld, sampsizes[0] * elemsize, 
			   0, nsample, results))
	    msg_ELog (EF_PROBLEM, "Error inserting derived data for %s",
		      F_GetFullName (fld));
    }
    else
    {
	char *rp = results;
	
	for (samp = 0; samp < nsample; samp++)
	{
	    if (! dc_AddMData (dest_dc, samptimes + samp, fld, 
			       sampsizes[samp] * elemsize, samp, 1, rp))
	    {
		msg_ELog (EF_PROBLEM, "Error inserting derived data for %s",
			  F_GetFullName (fld));
		break;
	    }

	    rp += sampsizes[samp] * elemsize;
	}
    }
/*
 * Clean up
 */
    free (results);
    free (dblresults);
    free (samptimes);
    for (sf = 0; sf < nsrcflds; sf++)
	free (dblptrs[sf]);
    free (dblptrs);
    free (sampsizes);
    free (srcflds);
/*
 * We're done!
 */
    return;
}




static void
DerivationFailure (DataChunk *dest_dc, FieldId fld)
/*
 * Report derivation failure and fill with bad values.
 */
{
    int samp, nsample, sampsize, elemsize;

    msg_ELog (EF_PROBLEM, "Derivation of %s/%s failed",
	      ds_PlatformName (dc_PlatformId (dest_dc)), F_GetFullName (fld));
/*
 * Get the element size and sample count, then fill with bad values a sample
 * at a time.
 */
    elemsize = dc_SizeOfType (dc_Type (dest_dc, fld));

    nsample = dc_GetNSample (dest_dc);

    for (samp = 0; samp < nsample; samp++)
    {
	DataPtr where = dc_GetMData (dest_dc, samp, fld, &sampsize);
	dc_MetFillMissing (dest_dc, fld, where, sampsize / elemsize);
    }

    return;
}



static zbool
DerivationCheck (PlatformId pid, DataClass class, GetList *gl, FieldId *flds, 
		 int nflds)
/* 
 * Return true iff the DataClass is MetData or one of its subclasses, and
 * one or more of the fields given needs to be and can be derived, i.e., if
 * the following conditions are met for any of the dc's fields:
 *	- it is not available raw from any of the files in the getlist
 *	- it can be derived from fields available raw from at least one of
 *	  the files
 */
{
    int f;
    zbool is_raw;
/*
 * Derivation is only an option for MetData datachunks
 */
    if (! dc_IsSubClassOf (class, DCC_MetData))
	return (0);
/*
 * Return true iff any one of the fields is available, but is not raw.
 */
    for (f = 0; f < nflds; f++)
	if (FldAvailable (flds[f], pid, gl, &is_raw, 0) && ! is_raw)
	    return (1);

    return (0);
}

    

static zbool
FldAvailable (FieldId fld, PlatformId pid, GetList *gl, zbool *is_raw, 
		 DerivMethod *return_deriv)
/*
 * Return true if 'fld' is available raw from one or more of the files
 * in GetList 'gl' or if it can be derived from data in one or more of the
 * files.  If 'return_deriv' is non-NULL, return a DerivMethod 
 * for obtaining the field.  If a DerivMethod is returned, it is the 
 * caller's responsibility to call F_DestroyDeriv() when finished with it.
 */
{
    int nfflds, ff;
    FieldId fileflds[MAXRAWFLDS];
    GetList *gp;
/*
 * Loop through the get list, seeing if this field is available raw in
 * any of the files.
 */
    *is_raw = 0;

    for (gp = gl; gp; gp = gp->gl_next)
    {
    /*
     * Get the list of available fields in this getlist entry's file
     */
	nfflds = MAXRAWFLDS;
	dfa_GetFields (&(gp->gl_df), &(gp->gl_begin), &nfflds, fileflds);
    /*
     * See if this field is available raw from the data file
     */
	for (ff = 0; ff < nfflds; ff++)
	{
	    if (fld == fileflds[ff])
	    {
	    /*
	     * This field is available raw!
	     */
		*is_raw = 1;
	    /*
	     * If a returned derivation is required, create one for 
	     * deriving the field from itself
	     */
		if (return_deriv)
		    *return_deriv = ds_GetDerivation (pid, fld, &fld, 1);
		    
		return (1);
	    }
	}
    }
/*
 * Nope, the field is not available raw, so see if it can be derived from 
 * data in any of the getlist's files.
 */
    for (gp = gl; gp; gp = gp->gl_next)
    {
    /*
     * Get the list of available fields in this getlist entry's file
     */
	nfflds = MAXRAWFLDS;
	dfa_GetFields (&(gp->gl_df), &(gp->gl_begin), &nfflds, fileflds);
    /* 
     * Return true if this field is derivable from fields in the data file
     */
	if (ds_IsDerivable (pid, fld, fileflds, nfflds))
	{
	    if (return_deriv)
		*return_deriv = ds_GetDerivation (pid, fld, fileflds, nfflds);
		
	    return (1);
	}
    }
/*
 * If we get here, the field is not raw and cannot be derived
 */
    return (0);
}



static void
ds_FindBlock (const DataFile *df, const DataFile *dfnext, DataChunk *dc, 
	      const Platform *plat, int sample, WriteCode wc, int *block_size)
/* Starts at 'sample' in data chunk 'dc', and finds out how many samples
 * following constitute a block, suitable for storing with dfa_PutBlock()
 * into 'dfile'.  Returns the number in 'block_size'.  Unless 'sample' is
 * out of range, 'nsample' will always hold at least 1.  At present, write
 * code other wc_Insert automatically returns 1. 
 */
{
    int smp, fut = 0;	/* counters			*/
    int nsample;
    ZebraTime when, past;
    ZebraTime next;		/* time dfnext starts		*/
    int avail;		/* samples available in the file*/
    ZebraTime *future = NULL;	/* data times already in file	*/
    int nfuture = 0;	/* returned by dfa_DataTimes	*/
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
    if (wc != wc_Overwrite)
	avail = pi_MaxSamp (plat) - df->df_core.dfc_nsample;
    else
	avail = df->df_core.dfc_nsample;

    if (dfnext)
	next = dfnext->df_core.dfc_begin;
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
	future = (ZebraTime *)malloc(avail * sizeof(ZebraTime));
	fut = avail - 1;
	nfuture = dfa_DataTimes (df, &past, DsAfter, avail, future + fut);
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
	dc_GetTime (dc, smp, &when);

	if (! TC_LessEq (past, when))
	    break;

	if (wc == wc_Append)
	{
	    if (dfnext && (! TC_Less(when, next)))
		break;
	    if (pi_Daysplit (plat) && (! ds_SameDay (&when, &past)))
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
    if (wc == wc_Overwrite)
	free (future);
    return;
}





static const DataFile*
ds_FindDest (DataChunk *dc, const Platform *plat, int sample, WriteCode *wc, 
	     int newfile, const ZebraTime *now)
/*
 * Try to find and return an appropriate destination DataFile for this datum,
 * and the correct WriteCode.  Return NULL if the WriteCode is wc_NewFile or
 * wc_SkipIt.  The returned DataFile pointer is good until the next call
 * to one of the file-finding functions.
 */
{
    const DataFile *df;
    const DataFileCore *dfc;
    ZebraTime when, dftime;
/*
 * Do a quick sanity check to see if this data has a bizarre time.  Reject
 * it in that case.
 */
    dc_GetTime (dc, sample, &when);
    if ((when.zt_Sec - now->zt_Sec) > MaxFuture)
    {
	msg_ELog (EF_PROBLEM, "Rejecting %s sample %d sec in future",
		  pi_Name (plat), when.zt_Sec - now->zt_Sec);
	*wc = wc_SkipIt;
	return (0);
    }
/*
 * Find the latest file in the default writable source which begins before 
 * the time of interest.
 */
    df = ds_FindDFBefore (SRC_DEFAULT_W, pi_Id (plat), &when);
/*
 * If there is none, then this data predates anything we have, so we
 * just return a new file case.
 */
    if (! df)
    {
	*wc = wc_NewFile;
	return (0);
    }
/*
 * See if the datum actually falls after the end of this dfile (most common
 * case).  If so, we either append or newfile.
 */
    dfc = &df->df_core;
    
    if (TC_Less (dfc->dfc_end, when))
    {
	if (! newfile && dfc->dfc_nsample < pi_MaxSamp (plat) &&
	    (! pi_Daysplit (plat) || ds_SameDay (&when, &dfc->dfc_end)))
	    *wc = wc_Append;
	else
	{
	    *wc = wc_NewFile;
	    df = 0;
	}
    }
/*
 * Writes to empty files are automatically appends.
 */
    else if (dfc->dfc_nsample == 0)
	*wc = wc_Append;
/*
 * The simple cases are not to be.  Now we have to see whether we
 * need to be overwriting data, or stuffing it in between.
 */
    else if (! dfa_DataTimes (df, &when, DsBefore, 1, &dftime) ||
	     ! TC_Eq (when, dftime))
    {
	*wc = wc_Insert;
    }
    else
    {
	*wc = wc_Overwrite;
    }
    return (df);
}




static zbool
ds_SameDay (const ZebraTime *t1, const ZebraTime *t2)
/*
 * Return TRUE iff the two times are on the same day, 
 * of the same month and year.
 */
{
	int y1, y2, m1, m2, d1, d2;

	TC_ZtSplit (t1, &y1, &m1, &d1, 0, 0, 0, 0);
	TC_ZtSplit (t2, &y2, &m2, &d2, 0, 0, 0, 0);
	return ((d1 == d2) && (m1 == m2) && (y1 == y2));
}




static int
ds_MakeNewFile (DataChunk *dc, const Platform *plat, int sample, 
		dsDetail *details, int ndetail, DataFile *df)
/*
 * Make a new file that will contain this DC and sample.
 */
{
	char fname[256];
	ZebraTime when;
/*
 * Create the new file name and tell the daemon what we have in mind
 * to do.
 */
	dc_GetTime (dc, sample, &when);
	dfa_MakeFileName (plat, &when, fname, details, ndetail);
	if (! ds_RequestNewDF (dc->dc_Platform, fname, &when, df))
		return (0);
/*
 * Have DFA get the file made for us.  They use the data object to know which
 * fields/platforms belong therein.  A bit kludgy, but it works.
 */
	if (! dfa_CreateFile (df, dc, &when, details, ndetail))
		return (0);

	return (1);
}




static int
ds_RequestNewDF (PlatformId plat, const char *filename, const ZebraTime *t,
		 DataFile *df)
/*
 * Get a new datafile entry from the DS daemon for this new file.
 * Entry:
 *	PLAT	is the platform for which this file is being created.
 *	FILE	is the name of the file.
 *	T	is the expected begin time of the data to put into the file.
 * 	DF	is a pointer to the DataFile to fill in
 * Exit:
 *	Return non-zero on success, zero otherwise.
 */
{
	struct dsp_CreateFile dspcf;
	struct dsp_R_CreateFile dspresp;

	if (DSM.dsm_NewDataFile)
		return ((*DSM.dsm_NewDataFile) (plat, filename, t, df));
/*
 * Put together the request for the daemon.
 */
	dspcf.dsp_type = dpt_NewFileRequest;
	dspcf.dsp_plat = plat;
	dspcf.dsp_srcid = SRC_DEFAULT_W;
	dspcf.dsp_time = *t;
	strcpy (dspcf.dsp_FileName, filename);
/*
 * Ship it off, and pick out our response.
 */
	ds_SendToDaemon (&dspcf, sizeof (dspcf));
	msg_Search (MT_DATASTORE, ds_GetNDFResp, &dspresp);
	if (dspresp.dsp_type == dpt_R_NewFileSuccess)
	{
	    *df = dspresp.dsp_file;
	    return (1);
	}
	else
	    return (0);
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
ds_NotifyDaemon (const Platform *p, const DataFile *df, int now, int nnew, 
		 int sample, int last)
/*
 * Tell the data store daemon about this data.
 */
{
	struct dsp_UpdateFile update;

	if (DSM.dsm_NotifyFile)
	{
		(*DSM.dsm_NotifyFile) (p, df, now, nnew, sample, last);
		return;
	}
/*
 * Fire off the message.
 */
	update.dsp_type = dpt_UpdateFile;
	update.dsp_file = *df;
	update.dsp_NSamples = nnew;
	update.dsp_NOverwrite = now;
	update.dsp_Last = last;
	ds_SendToDaemon ( &update, sizeof(update) );
/*
 * Wait for the update ack.
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
	struct dsp_UpdateAck *fs = (struct dsp_UpdateAck *) msg->m_data;
/*
 * If this is the ack, we're outta here.
 */
	if (fs->dsp_type == dpt_R_UpdateAck)
	{
		dfa_NoteRevision (&fs->dsp_file);
		return (MSG_DONE);
	}
	else
	   	return (MSG_ENQUEUE);
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
	F_Closure();
}



zbool
ds_ScanFile (int srcid, PlatformId pid, char *filename)
/*
 * Look for the file in the platform directory, verify the file name with
 * DFA, and then retrieve time info about the file through DFA.  Finally,
 * tell the daemon about the file.
 */
{
    const Platform *p = dt_FindPlatform (pid);
    DataFile df;
    char *fullpath;
    ZebraTime begin;
    ZebraTime end;
    int nsample;

    if (Standalone)
	return (FALSE);

    InstallDFA;

    if (! dfa_CheckName (pi_FileType (p), filename))
    {
	msg_ELog (EF_PROBLEM, "scan file '%s' of %s: %s", filename, 
		  pi_Name (p), "dfa name check failed");
	return (FALSE);
    }
/*
 * Tell the Daemon about this new file by simulating a ds_Store to it
 */
    fullpath = df.df_fullname;
    ds_GetPlatDir (srcid, pid, fullpath);
    strcat (fullpath, "/");
    strcat (fullpath, filename);

    if (! dfa_QueryDate (pi_FileType (p), fullpath, &begin, &end, &nsample))
    {
	msg_ELog (EF_PROBLEM, "scan file '%s': %s", fullpath, 
		  "inaccessible or incorrect format");
	return (FALSE);
    }
/*
 * Now fill in the rest of the DataFile and notify the daemon
 */
    strcpy (df.df_core.dfc_name, filename);
    df.df_core.dfc_begin = begin;
    df.df_core.dfc_end = end;
    df.df_core.dfc_rev = 0;	/* Daemon will update */
    df.df_core.dfc_inode = 0;	/* Daemon will update */
    df.df_core.dfc_ftype = pi_FileType (p);
    df.df_core.dfc_nsample = nsample;
    df.df_pid = pid;
    df.df_srcid = srcid;

    ds_NotifyDaemon (p, &df, 0, nsample, nsample, 1);
    return (TRUE);
}
