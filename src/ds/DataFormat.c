/*
 * The data file access package, for use from deep within the data store.
 *
 * DFA itself is not much more than a minimal wrapper which makes the rest
 * of the data store be format-independent.  We push much of the real work
 * down to the format-specific stuff.
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

# include <string.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <errno.h>

# include <defs.h>
# include <message.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"

MAKE_RCSID ("$Id: DataFormat.c,v 3.1 1995-08-24 22:17:41 granger Exp $")

/*
 * Include the DataFormat structure and our format table
 */
# include "DataFormat.h"

/*
 * And here is the format table.  Indexing into this table is done through
 * the FileType enum in dsPrivate.h.
 */
static struct DataFormat *Formats[] =
{
	netcdfFormat,
	boundaryFormat,
	rasterFormat,
	cmpRasterFormat,
	zebraFormat,
	gribFormat,
	gribSfcFormat,
	gradsFormat
};

static const int NumFormats = (sizeof(Formats)/sizeof(Formats[0]));

/*********************************************************************/
/*
 * Stuff for the open file table.
 */
static int MaxOpenFiles = 15;		/* How many we can keep open	*/

static OpenFile *OpenFiles = 0;		/* Open file list head		*/
static OpenFile *OFFree = 0;		/* lookaside list		*/
static int OF_Lru = 0;			/* LRU count			*/
static int OF_NOpen = 0;		/* Number of open files		*/


/*
 * Private routines.
 */
static OpenFile *dfa_FileIsOpen FP((int dfindex));
static void	dfa_AddOpenFile FP((OpenFile *ofp));
static OpenFile *dfa_GetOF FP((DataFormat *fmt, int dfindex,
			       DataFile *df, int write));
static void	dfa_FreeOF FP((OpenFile *ofp));
static void 	dfa_CloseFile FP((OpenFile *));
static char 	*dfa_GetExt FP((DataFormat *fmt));
static int	dfa_MatchExt FP((int fmt, char *dot));
static void	dfa_Deslash FP((char *dest));



/* ================================================================
 * The public interface to data file access, where data files are
 * referenced by their index.
 * ================================================================ */


bool
dfa_CreateFile (df, dc, t, details, ndetail)
int df;
DataChunk *dc;
ZebTime *t;
dsDetail *details;
int ndetail;
/*
 * Cause this file to exist, if at all possible.
 */
{
	char *tag;
	DataFile dfe;
	ClientPlatform p;
	OpenFile *ofp;
	DataFormat *fmt;
/*
 * Make sure that it isn't somehow open now.  (Would be strange but 
 * can't hurt to be sure.)
 */
	dfa_ForceClose (df);
	ds_GetFileStruct (df, &dfe);
	ds_GetPlatStruct (dfe.df_platform, &p, FALSE);
	fmt = Formats[dfe.df_ftype];
/*
 * Try to open up the file.  If successful, do our accounting and return
 * our success.
 */
	ofp = dfa_GetOF (fmt, df, &dfe, TRUE);
	if ((*fmt->f_CreateFile) (ofp, dfa_FilePath (&p, &dfe), &dfe, dc,
				  details, ndetail))
	{
		dfa_AddOpenFile (ofp);
		return (TRUE);
	}
	else
	{
		dfa_FreeOF (ofp);
		return (FALSE);
	}
}




void
dfa_ForceClose (dfindex)
int dfindex;
/*
 * If this file index is opened, close it now.  See the comment for
 * dfa_CloseFile.
 */
{
	OpenFile *ofp;

	for (ofp = OpenFiles; ofp; ofp = ofp->of_next)
	{
		if (ofp->of_dfindex == dfindex)
		{
			dfa_CloseFile (ofp);
			return;
		}
	}
}




int
dfa_GetObsSamples (dfile, times, locs, max)
int dfile, max;
ZebTime *times;
Location *locs;
/*
 * Return sample info from this observation.
 */
{
	OpenFile *ofp;

	if ((ofp = dfa_OpenFile (dfile, 0)) &&
	    (ofp->of_fmt->f_GetObsSamples))
	{
		return ((*ofp->of_fmt->f_GetObsSamples)
			(ofp, times, locs, max));
	}
	return (0);
}




int
dfa_GetFields (dfile, t, nfld, flist)
int dfile, *nfld;
ZebTime *t;
FieldId *flist;
/*
 * Return the available fields.
 */
{
	OpenFile *ofp;

	if ((ofp = dfa_OpenFile (dfile, 0)) && ofp->f_GetFields)
	{
		int sample = dfa_TimeIndex (ofp, t, 0);
		if (sample < 0)
			sample = 0;
		return ((*fmt->f_GetFields) (ofp, sample, nfld, flist));
	}
	return (0);
}




char *
dfa_GetAttr (dfile, t, len)
int dfile;
ZebTime *t;
int *len;
/*
 * Get the attributes for this time if we can.
 */
{
	OpenFile *ofp;
	int sample;
	ZebTime *times;

	if (! (ofp = dfa_OpenFile (dfile, 0)))
		return (0);
	if ((sample = dfa_TimeIndex (ofp, t, 0)) < 0)
		sample = 0;
	return (ofp->of_fmt->f_GetAttrs ?
		(*ofp->of_fmt->f_GetAttrs) (dfile, sample, len) : 0);
}




int
dfa_QueryDate (type, name, begin, end, nsample)
int type;
char *name;
ZebTime *begin, *end;
int *nsample;
/*
 * Query the dates on this file.
 */
{
	return ((*Formats[type]->f_QueryTime) (name, begin, end, nsample));
}





DataChunk *
dfa_Setup (gl, fields, nfield, class)
GetList *gl;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Set up to grab the data described by this GetList entry.
 */
{
	OpenFile *ofp;
	DataFormat *fmt;
	DataFile dfe;
	DataOrganization org;

	ds_GetFileStruct (gl->gl_dfindex, &dfe);
	fmt = Formats[dfe.df_ftype];
/*
 * Check for compatibility of the request, then open the file and pass it on
 */
	org = ds_PlatformDataOrg (dfe.df_plat);
	if (! dfa_OrgClassCompat (fmt, org, class))
	{
		msg_ELog (EF_PROBLEM, "%s format: class-organization mismatch",
			  fmt->f_name);
		return (NULL);
	}
	if ((ofp = dfa_OpenFile (gl->gl_dfindex, 0)))
		return ((*ofp->of_fmt->f_Setup) (ofp, fields, nfield, class));
}





void
dfa_GetData (dc, gl, details, ndetail)
DataChunk *dc;
GetList *gl;
dsDetail *details;
int ndetail;
/*
 * Get the data from this getlist entry.
 */
{
	OpenFile *ofp;
	ZebTime *times;
	int ntime;
	int begin;
	int end;
/*
 * Open the file.
 */
	if (! (ofp = dfa_OpenFile (gl->gl_dfindex, 0)))
		return;
/*
 * Find the indices of the samples for the range of times we want
 */
	begin = dfa_TimeIndex (ofp, &gl->gl_begin, 0);
	end = dfa_TimeIndex (ofp, &gl->gl_end, 1);
/*
 * If the indices are both less than zero, or identical and not equal
 * to one of the desired times, then we found a gap in which we
 * don't actually have any data.
 */
	times = dfa_GetTimes (ofp, &ntime);
	if ((end < 0) || ((begin == end) && 
			  ! TC_Eq (times[begin], gl->gl_begin) &&
			  ! TC_Eq (times[end], gl->gl_end)))
		return;
/*
 * Lastly, start at the first sample when it's time is greater than the 
 * begin time.
 */
	if (begin < 0)
		++begin;
	(*ofp->of_fmt->f_GetData) (ofp, dc, begin, end - begin + 1,
				   details, ndetail);
}




int
dfa_PutSample (dfile, dc, sample, wc, details, ndetail)
int dfile, sample;
DataChunk *dc;
WriteCode wc;
dsDetail *details;
int ndetail;
/*
 * Add data to this file.
 */
{
	OpenFile *ofp = dfa_OpenFile (dfile, 1);

	if (ofp && !(ofp->of_fmt->f_PutSample))
	{
		msg_ELog (EF_PROBLEM, "%s format: no PutSample method",
			  ofp->of_fmt->f_name);
	}
	else if (ofp)
	{
		return ((*Formats[dfe.df_ftype].f_PutSample)
			(dfile, dc, sample, wc, details, ndetail));
	}
	return (FALSE);
}




int
dfa_PutBlock (dfile, dc, sample, nsample, wc, details, ndetail)
int dfile;
DataChunk *dc;
int sample, nsample;
WriteCode wc;
dsDetail *details;
int ndetail;
/*
 * If the file's format has a f_PutBlock() method, call it.
 * Otherwise call dfa_PutSample() for each sample in the block.
 */
{
	int i, result;
	DataFile dfe;

	ds_GetFileStruct (dfile, &dfe);
	if (Formats[dfe.df_ftype].f_PutBlock)
		return ((*Formats[dfe.df_ftype].f_PutBlock)
			(dfile, dc, sample, nsample, wc, details, ndetail));
/*
 * otherwise loop through the samples in the block
 */
	msg_ELog (EF_DEBUG, "%s: no block method, looping over %d samples",
		  Formats[dfe.df_ftype].f_name, nsample);
	result = TRUE;
	for (i = sample; i < sample + nsample; ++i)
		result &= dfa_PutSample(dfile, dc, i, wc, details, ndetail);
/*
 * Return FALSE if any of the dfa_PutSample() calls failed
 */
	return((result)?TRUE:FALSE);
}




#ifdef notdef
int
dfa_InqNPlat (index)
int index;
/*
 * Find out how many platforms are here.
 */
{
	DataFile dfe;
	ds_GetFileStruct (index, &dfe);
	OpenFile *ofp;

	if ( ! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (0);
	ofp = 
	if (Formats[dfe.df_ftype].f_InqNPlat)
		return ((*Formats[dfe.df_ftype].f_InqNPlat) (index));
	return (1);
}
#endif



int
dfa_GetAlts (index, fid, offset, alts, nalts, altunits)
int	index;
FieldId	fid;
int	offset;
float	*alts;
int	*nalts;
AltUnitType *altunits;
/*
 * Get the altitudes from the given file associated with the given fid and 
 * forecast offset.
 */
{
	OpenFile *ofp;

	if ((ofp = dfa_OpenFile (index, 0)) &&
	    (ofp->of_fmt->f_GetAlts))
	{
		return ((*Formats[dfe.df_ftype].f_GetAlts)
			(ofp, fid, offset, alts, nalts, altunits));
	}
	return (FALSE);
}




int
dfa_GetForecastTimes (dfindex, times, ntimes)
int dfindex;
int *times, *ntimes;
/*
 * Get the forecast times.
 */
{
	OpenFile *ofp;

	if ((ofp = dfa_OpenFile (dfindex, 0)) &&
	    (ofp->of_fmt->f_GetForecastTimes))
	{
		return ((*fmt->of_fmt->f_GetForecastTimes)
			(ofp, times, ntimes));
	}
	return (FALSE);
}




int
dfa_DataTimes (index, when, which, n, dest)
int index, n;
ZebTime *when, *dest;
TimeSpec which;
{
	DataFile dfe;
	ds_GetFileStruct (index, &dfe);
	DataFormat *fmt = Formats[dfe.df_ftype];
/*
 * Get available data times.
 */
	if (fmt->f_DataTimes)
		return ((*fmt->f_DataTimes) (ofp, when, which, n, dest));
	else
		return (0);
}



/* ================================================================
 * Semi-private routines intended for formats only.
 * ================================================================ */


static OpenFile *
dfa_OpenFile (dfindex, write)
int dfindex;
int write;
/*
 * See to it that this file is open.  On success, return the pointer 
 * to the open file structure.  Otherwise return NULL.
 * Upon return, TAG contains the tag value.
 */
{
	DataFile df;
	ClientPlatform p;
	OpenFile *ofp;
	DataFormat *fmt;

	ds_GetFileStruct (dfindex, &df);
	fmt = Formats[df.df_ftype];
/* 
 * Verify they don't want to open a read-only format for writing.
 */
	if (write && fmt->f_readonly)
	{
		msg_ELog (EF_PROBLEM, "%s format read-only: %s", fmt->f_name,
			  "cannot open for writing");
		return (NULL);
	}
/*
 * If the file is open, check the revision and access and return the tag.
 */
	if ((ofp = dfa_FileIsOpen (dfindex)))
	{
		if (write && ! ofp->of_write)
			dfa_CloseFile (ofp);
		else
		{
			if (df.df_rev > ofp->of_dfrev)
			{
			/*
			 * The latest data file entry has a new revision,
			 * so our open file's tag must be out of date.  Thus
			 * we must sync the file.
			 */
				msg_ELog (EF_DEBUG, "Out of rev file %s",
					  df.df_name);
				dfa_SyncFile (ofp);
				ofp->of_dfrev = df.df_rev;
			}
			return (ofp);
		}
	}
/*
 * Nope, open it now.
 */
	ds_GetPlatStruct (df.df_platform, &p, FALSE);
	ofp = dfa_GetOF (fmt, dfindex, &df, write);
	if ((*fmt->f_OpenFile) (ofp, dfa_FilePath (&p, &df), &df, write))
	{
		dfa_AddOpenFile (ofp);
		retv = TRUE;
	}
	else
	{
		dfa_FreeOF (ofp);
		retv = FALSE;
	}
	return (retv);
}



int
dfa_SyncFile (ofp)
OpenFile *ofp;
{
	if (ofp->of_fmt->f_SyncFile)
		return ((*ofp->of_fmt->f_SyncFile) (ofp->of_tag));
	else
		return (1);
}




ZebTime *
dfa_GetTimes (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	*ntime = 0;
	if (! ofp->of_fmt->f_GetTimes)
	{
		msg_ELog (EF_PROBLEM, "%s format: missing GetTimes method",
			  fmt->f_name);
		return (NULL);
	}
	return ((*ofp->of_fmt->f_GetTimes)(ofp, ntime));
}



int 
dfa_TimeIndex (ofp, when, last)
OpenFile *ofp;
ZebTime *when;
int last;
/*
 * Find the closest sample time to 'when' without going beyond 'when'.
 * Returns -1 if when precedes all times in the file.  Returns the
 * first time of a group of identical times unless last is non-zero.
 */
{
#	define MINTIME 25
	ZebTime *zt;
	ZebTime *times;
	int nsample;
	int step;
	int ret;

	times = dfa_GetTimes (ofp, &nsample);
	if (! times || (nsample <= 0))
		return (-1);
	/*
	 * Eliminate the case where 'when' precedes all times in the file.
	 */
	if (TC_Less (*when, times[0]))
		return (-1);
	/*
	 * Find one time which first matches or then just precedes
	 * the desired time.
	 */
	ret = -1;
	if (nsample == 1)
	{
		return (0);
	}
	else if (TC_Eq (*when, times[0]))
	{
		ret = 0;
		if (! last)
			return (ret);
	}
	else if (TC_LessEq (times[nsample - 1], *when))
	{
		ret = (nsample - 1);
		if (last)
			return (ret);
	}
	else if (nsample < MINTIME)
	/*
	 * We know it's not the first or last times, so the linear
	 * search can start one from the end, and the binary search
	 * can assume when is always less than the time at top.
	 */
	{
		ret = 0;
		for (i = nsample - 2; i >= 0; i--)
		{
			if (TC_LessEq (ofp->of_times[i], *when))
				ret = i;
		}
	}
	else	/* binary search */
	{
		int top = nsample - 1, bottom = 0;
		while ((ret < 0) && (top > bottom + 1))
		{
			int mid = (top + bottom)/2;

			if (TC_Eq (*when, ofp->of_times[mid]))
				ret = mid;
			else if (TC_Less (ofp->of_times[mid], *when))
				bottom = mid;
			else
				top = mid;
		}
		/*
		 * The time at top is always greater than when, and the
		 * time at mid != when (else we would have returned), so
		 * either mid was less than zt, in which case the answer is
		 * bottom (because bottom was set to mid), or bottom < mid
		 * (and top was set to mid), in which case the answer is
		 * bottom. 
		 */
		if (ret < 0)
			ret = bottom;
	}
	/*
	 * Now check for duplicate times and choose the one at the
	 * requested end.
	 */
	step = (last) ? 1 : -1;
	i = ret + step;
	while (i >= 0 && i < nsample && TC_Eq (times[i], *when))
	{
		ret += step;
		i += step;
	}
	return (ret);
}


/* ====================================================================
 * DataFormat abstract methods for inheritance in the specific methods
 * ==================================================================== */


int
fmt_DataTimes (ofp, when, which, n, start)
OpenFile *ofp;
ZebTime *when;
TimeSpec which;
int n;
ZebTime *start;
/*
 * Find out when data is available.  Return at most n times in the
 * dest array corresponding to the 'which' TimeSpec for reference time
 * 'when'.  All of the returned times will be unique, which means
 * duplicate times in the file are copied only once.
 */
{
	int t, i;
	ZebTime *times;
	ZebTime *dest;
	int ntime;
/*
 * Find the sample index for this time, the first sample whose time is
 * at or before 'when'.  Index is -1 if when is before all times in the 
 * file.  Take the last time when we want DsAfter, and the first time
 * for DsBefore, since we'll skip the duplicate entries.
 */
	t = dfa_TimeIndex (ofp, when, /*last*/(which == DsAfter));
	times = dfa_GetTimes (ofp, &ntime);
/*
 * Copy out the times.
 */
	dest = start;
	if (which == DsBefore)
	{
		/*
		 * Loop is skipped if t == -1, meaning no times before 'when'
		 */
		for (i = 0; t >= 0 && i < n; i++)
		{
			if ((i == 0) || ! TC_Eq (*(dest-1), times[t]))
				*dest++ = times[t];
			--t;
		}
	}
	else if (which == DsAfter)
	{
		if (t < 0)
			t = 0;
		else if (TC_Less (times[t], *when))
			++t;
		for (i = 0; t < ntime && i < n; i++)
		{
			if ((i == 0) || ! TC_Eq (*(dest+1), times[t]))
				*dest-- = times[t];
			++t;
		}
	}
	return ((dest < start) ? (start - dest) : (dest - start));
}



void
fmt_MakeFileName (fmt, plat, t, dest, details, ndetail)
DataFormat *fmt;
ClientPlatform *plat;
ZebTime *t;
char *dest;
dsDetail *details;
int ndetail;
/*
 * Create a new file name for this platform, at this time.  The DataFormat
 * method uses the platform name, date, and time separated by periods,
 * followed by the extension (which usually contains a period).
 * Support several details to change things like the base file name or
 * the extension.
 */
{
	SValue v;
	char *ext;
/*
 * See if we're supposed to use an alternative extension.
 */
	if (ds_GetDetail (DD_FILE_EXTENSION, details, ndetail, &v))
		ext = v.us_v_ptr;
	else
		ext = dfa_GetExt (fmt);

	if (ds_GetDetail (DD_FILE_NAME, details, ndetail, &v))
	{
		strcpy (dest, v.us_v_ptr);	/* don't de-slash this one */
	}
	else if (ds_GetDetail (DD_FILE_BASE, details, ndetail, &v))
	{
		sprintf (dest, "%s%s", v.us_v_ptr, ext);    /* or this one */
	}
	else
	{
		/*
		 * Here's where we supply a default, with a possible
		 * modified extension.
		 */
		UItime ut;

		TC_ZtToUI (t, &ut);
		sprintf (dest, "%s.%06ld.%06ld%s", plat->cp_name, 
			 ut.ds_yymmdd, ut.ds_hhmmss, ext);
		dfa_Deslash (dest);
	}
}




/* ================================================================
 * Public DFA routines not corresponding to format instance methods
 * ================================================================ */


void
dfa_MakeFileName (plat, t, dest, details, ndetail)
ClientPlatform *plat;
ZebTime *t;
char *dest;
dsDetail *details;
int ndetail;
{
	DataFormat *fmt = Formats[plat->cp_ftype];

	if (! fmt->f_MakeFileName)
	{
		msg_ELog (EF_EMERGENCY, 
			  "%s format is missing a MakeFileName method",
			  fmt->f_name);
		dest[0] = 0;
	}
	else
	{
		(*fmt->f_MakeFileName)(fmt, plat, t, dest, details, ndetail);
	}
}




void
dfa_NoteRevision (dfindex, rev)
int dfindex;
long rev;
/*
 * Note that a revision has been signalled on this file
 */
{
	OpenFile *ofp = dfa_FileIsOpen (dfindex);

	if (ofp)
		ofp->of_dfrev = rev;
}




void
dfa_ForceClosure ()
/*
 * Go through the list of open files and close each one, then release
 * memory in the OpenFile free list.
 */
{
	OpenFile *ofp, *next;

	while (OpenFiles)
		dfa_CloseFile (OpenFiles);
	/*
	 * Free the memory in the OpenFile list as well
	 */
	ofp = OFFree;
	while (ofp)
	{
		next = ofp->of_next;
		free (ofp);
		ofp = next;
	}
	OFFree = NULL;
}



int
dfa_CheckName (type, name)
int type;
char *name;
/*
 * See if this file name is consistent with this file type.
 */
{
	char *dot;

	dot = strrchr (name, '.');
	return (dot && dfa_MatchExt (type, dot));
}



int
dfa_FindFormat (file)
char *file;
/*
 * Get the format of this file.
 */
{
	int fmt;

	if (! (cp = strrchr (file, '.')))
		return (-1);
	for (fmt = 0; fmt < NumFormats; fmt++)
	{
		if (dfa_MatchExt (fmt, cp))
			return (fmt);
	}
	return (-1);
}



/* ================================================================
 * DFA private routines below here
 * ================================================================ */


static void
dfa_AddOpenFile (ofp)
/* 
 * Add an open file to the list.
 */
{
	OpenFile *zap;

	OpenFiles = ofp;
	ofp->of_next = OpenFiles;
	ofp->of_lru = OF_Lru++;
/*
 * If we have exceeded the maximum number of open files, we have to close
 * somebody.
 */
	if (++OF_NOpen > MaxOpenFiles)
	{
		zap = ofp;
		for (ofp = OpenFiles->of_next; ofp; ofp = ofp->of_next)
			if (ofp->of_lru < zap->of_lru)
				zap = ofp;
		dfa_CloseFile (zap);
	}
}




static int
dfa_OrgClassCompat (fmt, org, class)
DataFormat *fmt;
DataOrganization org;
DataClass class;
/*
 * Return TRUE iff these two are compatible for this file format.
 */
{
	int i;
	struct CO_Compat *coctable = fmt->f_compat;
	int ncoc = fmt->f_ncompat;
/*
 * If there's no table, just blindly pass the pair through.
 * Otherwise, go through and see if we find the combination in the table.
 */
	if (!ncompat || !coctable)
		return (TRUE);
	for (i = 0; i < ncompat; ++i)
	{
		if (class == coctable[i].c_class && org == coctable[i].c_org)
			return (TRUE);
	}
	return (FALSE);
}




static OpenFile *
dfa_GetOF (fmt, dfindex, df, write)
DataFormat *fmt;
int dfindex;
DataFile *df;
int write;
/*
 * Return an initialized open file entry, but don't add it to the
 * open files chain yet.
 */
{
	OpenFile *ofp;

#ifdef notdef
	if (OFFree)
	{
		ret = OFFree;
		OFFree = ret->of_next;
	}
	else
		ret = ALLOC (OpenFile);
#endif
	ofp = (OpenFile *) malloc (fmt->f_of_size);
/*
 * Fill in the structure best we can
 */
	ofp->of_dfindex = dfindex;
	ofp->of_dfrev = df->df_rev;
	ofp->of_dftype = df->df_ftype;
	ofp->of_lru = 0;
	ofp->of_write = write;
	ofp->of_fmt = fmt;
#ifdef notdef
	ofp->of_tag = tag;
#endif
#ifdef notdef
/* 
 * Set values to indicate the format has not given us this info.
 */
	ofp->of_times = NULL;
	ofp->of_ntime = -1;
	ofp->of_sloc.l_Lat = -999;
	ofp->of_sloc.l_Lon = -999;
	ofp->of_sloc.l_Alt = -999;
	ofp->of_plats = NULL;
	ofp->of_nplat = -1;
#endif
	return (ofp);
}



static void
dfa_FreeOF (ofp)
OpenFile *ofp;
/*
 * Put an open file structure on the free list
 */
{
#ifdef notdef
	ofp->of_next = OFFree;
	OFFree = ofp;
#endif
	free (ofp);
}



static void
dfa_CloseFile (victim)
OpenFile *victim;
/*
 * Close this file, whether it wants to be or not.  This function MUST
 * NOT query the daemon since it may be called while waiting (searching)
 * for a different response.  For example, a DataGone message being
 * handled while waiting for a platform lock calls this function.  In
 * general, functions like this which bring the client in sync with the
 * daemon must act independently.  Hence the reason the file type must
 * be stored in the open file structure: so that we don't need to request
 * a data file structure.
 */
{
	OpenFile *prev;
#ifdef notdef
	DataFile df;

	ds_GetFileStruct (victim->of_dfindex, &df);
#endif
/*
 * Find this guy in the open file list and yank him.
 */
	if (OpenFiles == victim)
		OpenFiles = victim->of_next;
	else
	{
		for (prev = OpenFiles; prev->of_next; prev = prev->of_next)
			if (prev->of_next == victim)
				break;
		if (! prev->of_next)
		{
			msg_ELog (EF_PROBLEM, "OF entry 0x%x missing",
				  victim);
			return;
		}
		prev->of_next = victim->of_next;
	}
/*
 * Get the actual file closed.
 */
	(*victim->of_fmt->f_CloseFile) (victim);
	OF_NOpen--;
	dfa_FreeOF (victim);
}





static OpenFile *
dfa_FileIsOpen (dfindex)
int dfindex;
/*
 * Check and see if this file is open.
 */
{
	OpenFile *ofp;

	for (ofp = OpenFiles; ofp; ofp = ofp->of_next)
		if (ofp->of_dfindex == dfindex)
			return (ofp);
	return (0);
}




static void
dfa_Deslash (dest)
char *dest;
/*
 * Remove slashes without relying on overlapping args.
 */
{
	char *slash;
	int skip = 0;

	if ( (slash = strchr(dest, '/')) )
	{
		while (*slash)
		{
			if (*slash == '/')
				++skip;
			else
				*(slash - skip) = *slash;
			++slash;
		}
		*(slash - skip) = '\0';
	}
}




static char *
dfa_GetExt (fmt)
DataFormat *fmt;
/*
 * Return the default extension for this format
 */
{
	static char buf[32];
	char *bar;
	char *ext;

	if (! (bar = strchr (fmt->f_ext, '|')))
		return (fmt->f_ext);
	ext = fmt->f_ext;
	strncpy (buf, ext, (bar - ext));
	buf[(bar - ext)] = 0;
	return (buf);
}




static int
dfa_MatchExt (fmt, dot)
int fmt;	/* file format */
char *dot;	/* file suffix to match agains this format's extensions */
{
	char *ext;
	int dotlen;

	dotlen = strlen(dot);
	ext = Formats[fmt].f_ext;
	while (ext)
	{
		/* note that the short circuit keeps us from testing
		 * ext[extlen] unless something is there */  
		if (!strncmp (dot, ext, dotlen) &&
		    (ext[dotlen] == '|' || ext[dotlen] == 0))
			return (TRUE);
		if ((ext = strchr (ext, '|')))
			++ext;
	};
	return (FALSE);
}





#ifdef notdef
/*
 * DataFormat methods are assigned inline functions here which try to use
 * the format-provided method, if any.  Otherwise, the dfa_ method is
 * called.
 */
inline static int
f_DataTimes (index, when, which, n, dest)
int index, n;
ZebTime *when, *dest;
TimeSpec which;
{
	DataFile dfe;
	ds_GetFileStruct (index, &dfe);
/*
 * Get available data times.
 */
	if (Formats[dfe.df_ftype].f_DataTimes)
		return ((*Formats[dfe.df_ftype].f_DataTimes) (index, when,
					which, n, dest));
	else
		return (dfa_DataTimes (index, when, which, n, dest));
}


inline static int
f_NSample (tag)
void *tag;
{
	DataFile dfe;
	ds_GetFileStruct (index, &dfe);
	DataFormat *fmt = Formats + dfe.df_ftype;

	if (fmt->f_NSample)
		return ((*fmt->f_NSample) (tag));
	else if (BaseFormat.f_NSample)
		return ((*BaseFormat.f_NSample) (tag));
	msg_ELog (EF_PROBLEM, "missing NSample method for filetype %i",
		  dfe.df_ftype);
	return (0);
}



inline static void
f_SampleTime (tag, n, zt)
void *tag;
int n;
ZebTime *zt;
{
	DataFile dfe;
	ds_GetFileStruct (index, &dfe);
	DataFormat *fmt = Formats + dfe.df_ftype;

	if (fmt->f_DataTimes)
		return ((*fmt->f_DataTimes) (index, when, which, n, dest));
	else
		return (dfa_DataTimes (index, when, which, n, dest));
}
#endif
