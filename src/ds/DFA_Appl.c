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

RCSID ("$Id: DFA_Appl.c,v 3.11 1997-11-21 18:02:45 burghart Exp $")

/*
 * Local private prototypes.
 */
static int	ds_DFAMessage (struct message *msg);
static int      ds_AttrCheck (int, ZebTime *, char *);
static void     ds_NotifyDaemon (ClientPlatform *, int, DataChunk *, 
				 int, int, int, int);
static int 	ds_FindDest (DataChunk *, ClientPlatform *, DataFile *,
			     int sample, int *dfile, int *dfnext,
			     WriteCode *, int, ZebTime *);
static bool	ds_SameDay (ZebTime *, ZebTime *);
static int	ds_MakeNewFile (DataChunk *, ClientPlatform *, int sample, 
				dsDetail *details, int ndetail);
static int	ds_RequestNewDF (PlatformId, char *, ZebTime *);
static int	ds_GetNDFResp (struct message *, struct dsp_R_CreateFile *);
static void	ds_AbortNewDF (PlatformId, int);
static int	ds_AwaitAck (Message *, int);
static void 	ds_FProcGetList (DataChunk *, GetList *, dsDetail *, int);
static void	ds_FindBlock (DataFile *dfp, int dfnext, 
			      DataChunk *dc, ClientPlatform *p,
			      int sample, WriteCode wc, int *nsample);
static DataChunk* Derive (DataChunk *template_dc, GetList *get, 
			     dsDetail *details, int ndetail);
static void	DeriveFld (DataChunk *dest_dc, DataChunk *extra_dc, 
			      FieldId fld, FieldId surrogate, 
			      DerivMethod deriv);
static void	DerivationFailure (DataChunk *dest_dc, FieldId fld, 
				      FieldId surrogate);
static bool	DerivationCheck (PlatformId pid, GetList *get, 
				    FieldId *flds, int nflds);
static bool	FldAvailable (FieldId fld, PlatformId pid, GetList *get, 
				 bool *is_raw, DerivMethod *return_deriv);

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
	int df, i;
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
		if (! ds_GetFileStruct (df, &dfe))
			return (0);
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



#ifndef NO_GETATTR
static int
ds_AttrCheck (df, t, attr)
int df;
ZebTime *t;
char *attr;
/*
 * See if this attribute is found in the data.
 */
{
	char *dattr;
	char *a;
	int len, i;
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
int df;
ZebTime *t;
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




static int
ds_QualifyTimes (dfi, which, times, n, key, value)
int dfi;
TimeSpec which;
ZebTime *times;
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
		if (! ds_AttrCheck (dfi, times+i, value))
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
ZebTime *when, *rettimes;
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
ZebTime *when;
int n;
TimeSpec which;
char *key;		/* if null, value checked as key or value */
char *value;		/* simple string comparison for now */
ZebTime *rettimes;
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
	int ndone = 0, index;
	int nseen = 0;
	int max = n;
	DataFile dfe;
	int i;
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
		if ((index = ds_FindDF (platform, when, SRC_ALL)) < 0)
			return (0);
	/*
	 * Now we plow through datafile entries until we have all we
	 * want.
	 */
		ds_LockPlatform (platform);
		while (index && ndone < n && nseen < max)
		{
			int nnew;
			nnew = dfa_DataTimes (index, when, which, n - ndone,
					      rettimes + ndone);
			nseen += nnew;
			nnew = ds_QualifyTimes (index, which, 
						rettimes + ndone, nnew,
						key, value);
			ndone += nnew;
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
		for (; index && ndone < n && nseen < max; index = dfe.df_BLink)
		{
			int nnew;
			ds_GetFileStruct (index, &dfe);
			nnew = dfa_DataTimes (index, when, which, n - ndone,
					      rettimes + n - ndone - 1);
			nseen += nnew;
			nnew = ds_QualifyTimes (index, which,
						rettimes + n - ndone - nnew,
						nnew, key, value);
			ndone += nnew;
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
			"Only DsBefore and DsAfter TimeSpec handled");
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
	msg_ELog (EF_DEBUG, "getdata file #%d, getlist %s to %s",
		  gp->gl_dfindex, btime, etime);
    }
#endif
/*
 * Get the data from this getlist entry's data file.
 */
    dfa_GetData (dc, gp, details, ndetail);
}




static DataChunk*
Derive (DataChunk *template_dc, GetList *gl, dsDetail *details, int ndetail)
/* 
 * Build, fill, and return a new datachunk based on the template_dc, using
 * its fields, the given getlist, and deriving fields where necessary and 
 * possible.  The template_dc must be of class MetData or one of its 
 * subclasses.  
 */
{
    int nextraflds, ndestflds, df;
    FieldId extraflds[MAXRAWFLDS], surrogates[MAXRAWFLDS], *destflds;
    PlatformId pid = dc_PlatformId (template_dc);
    DerivMethod *derivs;
    DataClass class;
    DataChunk *dest_dc, *extra_dc;
/*
 * Get the list of fields we're supposed to return and the datachunk class.
 */
    destflds = dc_GetFields (template_dc, &ndestflds);
    class = dc_ClassId (template_dc);
/* 
 * For each destination field:
 *	o Get a derivation method (if any)
 *	If we have a derivation method
 *		o Find the source fields necessary for the derivation (srcflds)
 *		o Use the first srcfld as a temporary surrogate for the 
 *		  destination field
 *		o Add the rest of the srcflds to a list of extra needed 
 *		  fields (extraflds)
 */
    derivs = (DerivMethod*) malloc (ndestflds * sizeof (DerivMethod));
    
    nextraflds = 0;

    for (df = 0; df < ndestflds; df++)
    {
	FieldId	*srcflds;
	int sf, nsrcflds;
	bool is_raw;
    /*
     * Get a derivation for this field.  If it's either available raw or
     * it's underivable, the field will serve as a surrogate for itself and
     * we can go on to the next.
     */
	derivs[df] = (DerivMethod) 0;
	if (! FldAvailable (destflds[df], pid, gl, &is_raw, &derivs[df]) ||
	    is_raw)
	{
	    surrogates[df] = destflds[df];
	    continue;
	}
    /*
     * Get the list of source fields required for the derivation
     */
	srcflds = ds_DerivNeededFields (derivs[df], &nsrcflds);
    /* 
     * We'll use the first field required for the derivation as a temporary
     * surrogate in the datachunk we have to build.
     */
	surrogates[df] = srcflds[0];
    /*
     * Add the rest of the fields for this derivation (if any) to our 
     * complete list of extra needed raw fields, dropping duplicates.
     */
	for (sf = 1; sf < nsrcflds; sf++)
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
 * Set up our return datachunk with the surrogate fields, which will be
 * overwritten with the derived data later.  Also set up a data chunk for
 * the extra needed fields
 */
    dest_dc = dfa_Setup (gl, surrogates, ndestflds, class);

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
 * Fill the data chunks
 */	
    dc_SetPlatform (dest_dc, pid);
    ds_FProcGetList (dest_dc, gl, details, ndetail);

    if (extra_dc)
    {
	dc_SetPlatform (extra_dc, pid);
	ds_FProcGetList (extra_dc, gl, details, ndetail);
    }
/*
 * Derive each field that needs deriving, replacing the surrogate fields
 * with the desired fields.
 */
    for (df = 0; df < ndestflds; df++)
    {
	if (derivs[df])
	    DeriveFld (dest_dc, extra_dc, destflds[df], surrogates[df],
		       derivs[df]);
    }
/*
 * Clean up
 */
    for (df = 0; df < ndestflds; df++)
    {
	if (derivs[df])
	    ds_DestroyDeriv (derivs[df]);
    }

    free (derivs);
    
    if (extra_dc)
	dc_DestroyDC (extra_dc);
/*
 * Done
 */
    return (dest_dc);
}




static void
DeriveFld (DataChunk *dest_dc, DataChunk *extra_dc, FieldId fld, 
	   FieldId surrogate, DerivMethod deriv)
/*
 * Derive field 'fld' from data in dest_dc and extra_dc, using derivation
 * method 'deriv', and replacing field 'surrogate' in dest_dc.
 */
{
    int nsrcflds, sf, nsample, elemsize, elemcount, samp, firstelem;
    int uniform, *sampsizes, e;
    DC_ElemType elemtype;
    FieldId *srcflds;
    DataChunk **src_dc;
    double **dblptrs, *dblresults;
    char *results;
    void *badvp;
    double badval;
    ZebraTime *samptimes;
/*
 * Really, really easy if this is just an alias.  Just rename the field
 * in the data chunk and we're done!
 */
    if (ds_DerivIsAlias (deriv))
    {
	char sname[128];
    /*
     * Gotta copy one of the full names, since the string returned by 
     * F_GetFullName() is only valid until the next call, so we can't have
     * two of them in one msg_ELog() call...
     */
	strcpy (sname, F_GetFullName (surrogate));
	msg_ELog (EF_DEBUG, "Field %s yields %s directly.  Cool!",
		  sname, F_GetFullName (fld));

	dc_ChangeFld (dest_dc, surrogate, fld);
	return;
    }

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
    elemtype = dc_Type (dest_dc, surrogate);
    elemsize = dc_SizeOfType (elemtype);
/*
 * We'll continue using surrogate's bad value
 */
    if ((badvp = dc_GetFieldBadval (dest_dc, surrogate)) != NULL)
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
	dc_GetMData (dest_dc, samp, surrogate, &(sampsizes[samp]));
	sampsizes[samp] /= elemsize;
	elemcount += sampsizes[samp];

	if (samp > 0)
	    uniform = uniform && (sampsizes[samp] == sampsizes[samp-1]);
    }
/*
 * Find out which dc holds each of our source fields
 */
    src_dc = (DataChunk**) malloc (nsrcflds * sizeof (DataChunk*));

    for (sf = 0; sf < nsrcflds; sf++)
    {
    /*
     * Look for this field first in dest_dc, then in extra_dc, and stash
     * where we find it.
     */
	if (dc_GetFieldIndex (dest_dc, srcflds[sf]) >= 0)
	    src_dc[sf] = dest_dc;
	else if (extra_dc && dc_GetFieldIndex (extra_dc, srcflds[sf]) >= 0)
	    src_dc[sf] = extra_dc;
	else
	{
	/*
	 * Uh-oh, we should've found it but didn't...
	 */
	    msg_ELog (EF_PROBLEM, 
		      "DeriveFld: Expected field %s not found to derive %s",
		      F_GetFullName (srcflds[sf]), F_GetFullName (fld));
	    DerivationFailure (dest_dc, fld, surrogate);
	    free (src_dc);
	    free (sampsizes);
	    return;
	}
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
	    src_type = dc_Type (src_dc[sf], srcflds[sf]);
	    src_elemsize = dc_SizeOfType (src_type);
	    rawdp = dc_GetMData (src_dc[sf], samp, srcflds[sf], &nbytes);

	    nelems = nbytes / src_elemsize;
	/*
	 * Get the bad value for this source field, as a double
	 */
	    if ((badvp = dc_GetFieldBadval (src_dc[sf], srcflds[sf])) != NULL)
		dc_ConvertDouble (&src_bad, badvp, src_type);
	    else
		src_bad = (double) dc_GetBadval (src_dc[sf]);
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
		DerivationFailure (dest_dc, fld, surrogate);
		free (src_dc);
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
 * Convert to surrogate's element type before stashing in the data chunk
 */
    results = (char *) malloc (elemcount * elemsize);

    for (e = 0; e < elemcount; e++)
	dc_LongDoubleToType ((void*)(results + e * elemsize), elemtype,
			     (LongDouble)(dblresults[e]));
/*
 * We can now rename the surrogate field and stash the derived data.  Data
 * stash can happen as a unit for uniform samples, otherwise must happen 
 * sample by sample.
 */
    dc_ChangeFld (dest_dc, surrogate, fld);

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
    free (src_dc);
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
DerivationFailure (DataChunk *dest_dc, FieldId fld, FieldId surrogate)
/*
 * Rename the 'surrogate' to 'fld' (the field we were trying to derive), and
 * fill with bad values.
 */
{
    int samp, nsample, sampsize, elemsize;

    msg_ELog (EF_PROBLEM, "Derivation of %s/%s failed",
	      ds_PlatformName (dc_PlatformId (dest_dc)), F_GetFullName (fld));
/*
 * Change 'surrogate' to 'fld'
 */
    dc_ChangeFld (dest_dc, surrogate, fld);
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
    DataChunk *return_dc, *base_dc;
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
	DataFile dfe;

	ds_GetFileStruct (get->gl_dfindex, &dfe);
	get->gl_begin = dfe.df_begin;

	get->gl_end = dfe.df_end;
    }
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
    if (dc_IsSubClassOf (class, DCC_MetData) && 
	DerivationCheck (pid, get, fields, nfield))
    {
	DataChunk *template_dc = return_dc;

	return_dc = Derive (template_dc, get, details, ndetail);

	dc_DestroyDC (template_dc);
    }
    else
	ds_FProcGetList (return_dc, get, details, ndetail);

    dgl_ReturnList (get);
/*
 * It is still possible that there were no times in the file between
 * the requested times, in which case we return null for no data found.
 */
    if (dc_GetNSample (return_dc) == 0)
    {
	dc_DestroyDC (return_dc);
	return (NULL);
    }
/*
 * Finally, process any file-format-independent details, like bad values
 */
    dc_ProcessDetails (return_dc, details, ndetail);
    return (return_dc);
}



static bool
DerivationCheck (PlatformId pid, GetList *gl, FieldId *flds, int nflds)
/*
 * Return true iff one or more of the fields given needs to be and can be
 * derived, i.e., if the following conditions are met for any of the dc's
 * fields:
 *	- it is not available raw from any of the files in the getlist
 *	- it can be derived from fields available raw from at least one of
 *	  the files
 */
{
    int f;
    bool is_raw;
    FieldId fileflds[MAXRAWFLDS];
/*
 * Return true iff any one of the fields is available, but is not raw.
 */
    for (f = 0; f < nflds; f++)
	if (FldAvailable (flds[f], pid, gl, &is_raw, 0) && ! is_raw)
	    return (1);

    return (0);
}

    

static bool
FldAvailable (FieldId fld, PlatformId pid, GetList *gl, bool *is_raw, 
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
    bool raw;
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
	dfa_GetFields (gp->gl_dfindex, &(gp->gl_begin), &nfflds, fileflds);
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
	dfa_GetFields (gp->gl_dfindex, &(gp->gl_begin), &nfflds, fileflds);
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
	return (ds_FetchData (pid, class, FALSE, begin, end, fields, nfield, 
			      details, ndetail));
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
bool
ds_Store (dc, newfile, details, ndetail)
DataChunk *dc;
bool newfile;
dsDetail *details;
int ndetail;
{
	return (ds_StoreBlocks (dc, newfile, details, ndetail));
}




bool
ds_StoreBlocks (dc, newfile, details, ndetail)
DataChunk *dc;
bool newfile;
dsDetail *details;
int ndetail;
/*
 * Store samples in the largest blocks possible, while consolidating 
 * notifications to the daemon as much as possible.  To this end, we must
 * keep a copy of the DataFile structure for the current file we're writing,
 * so we can modify it and keep it up to date for the FindDest and FindBlock
 * routines.
 *
 * Hold off on notifying the daemon as long as we're just adding to a pending
 * change.  As soon as we begin writing to a new file or switching to a
 * new write code, send the pending notification.
 */
{
	int nsample, sample, endsamp = 0;
	int dfile, dfupdate;
	int dfnext;
	int nnew;
	int now;
	int ndone;	/* Total number of samples successfully stored	*/
	int block_size;
	DataFile dfe;	/* Stash for pending changes not yet notified 	*/
	DataFile *dfp;
	WriteCode wc;
	ClientPlatform p;
	ZebTime curtime;
/*
 * This is a reasonable spot to make sure we have a valid platform
 */
	InstallDFA;
	if (dc_PlatformId (dc) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "attempting ds_Store of DataChunk %s",
			  "with bad platform id");
		return (FALSE);
	}
	CheckDetails (&details, &ndetail);
/*
 * Setup time.
 */
	ds_WriteLock (dc_PlatformId (dc));
	ds_GetPlatStruct (dc_PlatformId (dc), &p, TRUE);
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
	nnew = now = 0;
	dfupdate = -1;
	dfp = NULL;
	while (sample < nsample)
	{
		bool new = FALSE;
	/*
	 * Find a feasible location for the next sample of the data chunk.
	 */
		if (! ds_FindDest (dc, &p, dfp, sample, &dfile, &dfnext, &wc, 
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
	 * See if we have an update pending on a file different from the
	 * next one.  Send the update now before writing the next file.
	 * Set 'last' to true since this is probably the last update (as
	 * far as we can guess anyway) for that file. 
	 */
		if ((dfupdate >= 0) && 
		    ((dfile != dfupdate) || 
		     (nnew && wc == wc_Overwrite) || 
		     (now && wc != wc_Overwrite)))
		{
		/*
		 * Fill in the daemon on what we did to the previus file.
		 * The last sample of the block updates the file end time.
		 */
			ds_NotifyDaemon (&p, dfupdate, dc, now, nnew, 
					 endsamp, /* last */ TRUE);
		/*
		 * These count new and overwritten samples since the last
		 * notify, so now's the time to reset them.
		 */
			nnew = now = 0;
		/* 
		 * No longer need to update this file.
		 */
			dfupdate = -1;
			dfp = NULL;
		}
	/*
	 * Get a copy of the DataFile structure whose information we will be
	 * modifying privately.
	 */
		if (! dfp)
		{
			ds_GetFileStruct (dfile, &dfe);
			dfp = &dfe;
		}
	/*
	 * Find out how many samples can be written to this file
	 * as a single block.  The answer is at least one.
	 */
		ds_FindBlock (dfp, dfnext, dc, &p, sample, wc, &block_size);
		msg_ELog((block_size >= 25) ? EF_DEBUG : EF_DEVELOP,
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
			ZebTime endt;
		/*
		 * Keep track of successful writes.  Likewise, keep a
		 * running tab of new samples in our private DataFile
		 * structure.
		 */
			ndone += block_size;
			if (wc == wc_Overwrite)
			{
				now += block_size;
			}
			else
			{
				nnew += block_size;
				dfp->df_nsample += block_size;
			}
		/*
		 * Set the end sample to the last sample of this block
		 */
			endsamp = sample + block_size - 1;
			dc_GetTime (dc, endsamp, &endt);
			if (TC_Less (dfp->df_end, endt))
				dfp->df_end = endt;
		/*
		 * Set this file to be updated in the next daemon notify.
		 * Either dfupdate is -1, or it already equals dfile.
		 */
			dfupdate = dfile;
		}
	/*
	 * Move on to the rest of the samples no matter what
	 */
		sample += block_size;
	/*
	 * If we created a new file above, then our platform structure
	 * needs to be updated before we can look for our next destination.
	 * Notify the daemon of the update to this file, so that it moves
	 * the Tfile into the file chain.  If the write failed above then
	 * we'll likely get a "reusing temp file" warning, which is as it
	 * should be.
	 */
		if (new && (dfupdate >= 0))
		{
		/*
		 * We have to assume 'last' is true in case the next write
		 * is to a different file.
		 */
			ds_NotifyDaemon (&p, dfupdate, dc, now, nnew, 
					 endsamp, /* last */ TRUE);
			nnew = now = 0;
			dfupdate = -1;
			dfp = NULL;
			ds_GetPlatStruct (dc_PlatformId (dc), &p, TRUE);
		}

	} /* while (sample < nsample) */
/*
 * Send any leftover update.
 */
	if (dfupdate >= 0)
	{
		ds_NotifyDaemon (&p, dfupdate, dc, now, nnew, 
				 endsamp, /* last */ TRUE);
	}
/*
 * Release the write lock and we're outta here.
 */
	ds_FreeWriteLock (dc_PlatformId (dc));
	return (ndone == nsample);
}





static void
ds_FindBlock (dfp, dfnext, dc, plat, sample, wc, block_size)
DataFile *dfp;
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
	int smp, fut = 0;	/* counters			*/
	int nsample;
	DataFile dfenext;
	ZebTime when, past;
	ZebTime next;		/* time dfnext starts		*/
	int avail;		/* samples available in the file*/
	ZebTime *future = NULL;	/* data times already in file	*/
	int nfuture = 0;	/* returned by dfa_DataTimes	*/
	int dfile = dfp->df_index;
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
	ds_LockPlatform (dc_PlatformId (dc));
	if (wc != wc_Overwrite)
		avail = plat->cp_maxsamp - dfp->df_nsample;
	else
		avail = dfp->df_nsample;

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
	ds_UnlockPlatform (dc_PlatformId (dc));
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
	if (wc == wc_Overwrite)
		free (future);
	return;
}





static int
ds_FindDest (dc, plat, cur, sample, dfile, dfnext, wc, newfile, now)
DataChunk *dc;
ClientPlatform *plat;
DataFile *cur;		/* the file most recently written but not notified */
int sample, *dfile, *dfnext, newfile;
WriteCode *wc;
ZebTime *now;
/*
 * Try to find an appropriate destination for this datum.
 * Return value is TRUE iff it was possible.  *dfnext returns with
 * the data file which chronologically follows *dfile, if it exists.
 */
{
	int df;
	DataFile dfe;
	DataFile *dfp = NULL;
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
	ds_LockPlatform (dc_PlatformId (dc));
	df = ds_DataChain (plat, 0);
	*dfnext = 0;
	for (; df; df = dfp->df_FLink)
	{
		if (cur && (df == cur->df_index))
			dfp = cur;
		else
		{
			ds_GetFileStruct (df, &dfe);
			dfp = &dfe;
		}
		if (TC_LessEq (dfp->df_begin, when))
			break;
		*dfnext = df;
	}
	*dfile = df;
/*
 * If there is none, then this data predates anything we have, so we
 * just return a new file case.
 */
	if (! df)
	{
		*dfile = -1;
		*wc = wc_NewFile;
	}
/*
 * See if the datum actually falls after the end of this dfile (most common
 * case).  If so, we either append or newfile.
 */
	else if (TC_Less (dfp->df_end, when))
	{
		if (! newfile && dfp->df_nsample < plat->cp_maxsamp &&
				 (! (plat->cp_flags & DPF_SPLIT) ||
			 	ds_SameDay (&when, &dfp->df_end)) &&
				 (dfp->df_flags & DFF_Archived) == 0)
			*wc = wc_Append;
		else
			*wc = wc_NewFile;
	}
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
	ds_UnlockPlatform (dc_PlatformId (dc));
	return (TRUE);
}




static bool
ds_SameDay (t1, t2)
ZebTime *t1, *t2;
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
	dfa_MakeFileName (plat, &when, fname, details, ndetail);
	if ((newdf = ds_RequestNewDF (dc_PlatformId (dc), fname, &when)) < 0)
		return (-1);
/*
 * Have DFA get the file made for us.  They use the data object to know which
 * fields/platforms belong therein.  A bit kludgy, but it works.
 */
	if (! dfa_CreateFile (newdf, dc, &when, details, ndetail))
	{
		ds_AbortNewDF (dc_PlatformId (dc), newdf);
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

	if (DSM.dsm_NewDataFile)
		return ((*DSM.dsm_NewDataFile) (plat, file, t));
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

	if (Standalone)
	{
		/* abandon the cached file entry */
		return;
	}
	abort.dsp_type = dpt_AbortNewFile;
	abort.dsp_FileIndex = df;
	abort.dsp_pid = plat;
	ds_SendToDaemon (&abort, sizeof (abort));
}





static void
ds_NotifyDaemon (p, dfile, dc, now, nnew, sample, last)
ClientPlatform *p;
int dfile;
DataChunk *dc;
int now, nnew, sample, last;
/*
 * Tell the data store daemon about this data.
 */
{
	struct dsp_UpdateFile update;

	if (DSM.dsm_NotifyFile)
	{
		(*DSM.dsm_NotifyFile) (p, dfile, dc, now, nnew, sample, last);
		return;
	}
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
	   	(msg_ProtoHandler (MT_DATASTORE)) (msg);
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
	F_Closure();
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

	if (Standalone)
		return (FALSE);
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

	if (Standalone)
		return (FALSE);
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
		if (! ds_GetFileStruct (df, &dfe))
			return (FALSE);
		if (TC_LessEq (dfe.df_begin, *end))
			break;
	};
	ds_UnlockPlatform (platid);
	if (df && !(TC_Less (dfe.df_end, *begin)))
	{
		msg_ELog (EF_PROBLEM, 
			"inserting file '%s': %s %s", filename,
			"times conflict with known file", dfe.df_name);
		ds_FreeWriteLock (platid);
		return (FALSE);
	}
/*
 * Get a datafile entry for the new file
 */
	if ((dfile = ds_RequestNewDF (platid, filename, begin)) < 0)
	{
		ds_FreeWriteLock (platid);
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
	ds_FreeWriteLock (platid);
	return (TRUE);
}

