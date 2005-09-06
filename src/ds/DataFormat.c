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

# include <stdio.h>
# include <string.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <errno.h>
# include <memory.h>

# include <defs.h>
# include <message.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "GetList.h"
# include "dslib.h"
# include "dfa.h"

RCSID ("$Id: DataFormat.c,v 3.17 2005-09-06 23:41:03 burghart Exp $")

/*
 * Include the DataFormat structure definition, and the public and
 * semi-private prototypes.
 */
# include "DataFormat.h"

/*
 * Declarations for pointers to each format's structure.
 */
extern DataFormat *netcdfFormat;
extern DataFormat *boundaryFormat;
extern DataFormat *rasterFormat;
extern DataFormat *cmpRasterFormat;
extern DataFormat *zebraFormat;
extern DataFormat *gribFormat;
extern DataFormat *gribSfcFormat;
extern DataFormat *gradsFormat;
extern DataFormat *gradsmodelFormat;
extern DataFormat *hdfFormat;
extern DataFormat *sweepfileFormat;
extern DataFormat *opaqueFileFormat;

/*
 * And here is the format table.  Indexing into this table is done through
 * the FileType enum in dsPrivate.h.
 */
static DataFormat **Formats[] =
{
	&netcdfFormat,
	&boundaryFormat,
	&rasterFormat,
	&cmpRasterFormat,
	&zebraFormat,
	&gribFormat,
	&gribSfcFormat,
	&gradsFormat,
	&gradsmodelFormat,
	&hdfFormat,
	&sweepfileFormat,
	&opaqueFileFormat,
};

static const int NumFormats = (sizeof(Formats)/sizeof(Formats[0]));

#define FMTP(ft) (*(Formats[(ft)]))

/*********************************************************************/
/*
 * Stuff for the open file table.
 */
static int MaxOpenFiles = 45;		/* How many we can keep open	*/

static OpenFile *OpenFiles = 0;		/* Open file list head		*/
static int OF_Lru = 0;			/* LRU count			*/
static int OF_NOpen = 0;		/* Number of open files		*/
#ifdef DEBUG
static int OF_NPurged = 0;		/* Number closed to make room	*/
static int OF_Reopens = 0;		/* Number files re-opend r/w	*/
#endif

/*
 * Private routines.
 */
static OpenFile *dfa_FileIsOpen (const DataFile *df);
static void	dfa_AddOpenFile (OpenFile *ofp);
static OpenFile *dfa_GetOF (DataFormat *fmt, const DataFile *df, int write);
static void	dfa_FreeOF (OpenFile *ofp);
static void 	dfa_CloseFile (OpenFile *);
static char 	*dfa_GetExt (DataFormat *fmt);
static int	dfa_MatchExt (int fmt, char *dot);
static zbool	dfa_OpenFileMatches (OpenFile *ofp, const DataFile *df);
static void	dfa_Deslash (char *dest);
static void	dfa_AccessCount (OpenFile *ofp);
static int	dfa_OrgClassCompat (DataFormat *fmt, DataOrganization org,
				    DataClass class);


/* ================================================================
 * The public interface to data file access, where data files are
 * referenced by their index.
 * ================================================================ */


zbool
dfa_CreateFile (const DataFile *df, DataChunk *dc, ZebraTime *t, 
		dsDetail *details, int ndetail)
/*
 * Cause this file to exist, if at all possible.
 */
{
	PlatformId pid;
	OpenFile *ofp;
	DataFormat *fmt;
/*
 * Make sure that it isn't somehow open now.  (Would be strange but 
 * can't hurt to be sure.)
 */
	dfa_ForceClose (df);
	pid = df->df_pid;
	fmt = FMTP(df->df_core.dfc_ftype);
/*
 * Make sure the format isn't read-only
 */
	if (fmt->f_readonly)
	{
		msg_ELog (EF_PROBLEM, "%s format read-only: %s", fmt->f_name,
			  "cannot create file");
		return (FALSE);
	}
/*
 * Try to open up the file.  If successful, do our accounting and return
 * our success.
 */
	ofp = dfa_GetOF (fmt, df, TRUE);
	if ((*fmt->f_CreateFile) (ofp, dc, details, ndetail))
	{
#ifdef DEBUG
		msg_ELog (EF_DEBUG, "created file %s", df->df_fullname);
#endif
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
dfa_ForceClose (const DataFile *df)
/*
 * If this file is opened, close it now.  See the comment for dfa_CloseFile.
 */
{
    OpenFile *ofp;

    for (ofp = OpenFiles; ofp; ofp = ofp->of_next)
    {
	if (dfa_OpenFileMatches (ofp, df))
	{
	    dfa_CloseFile (ofp);
	    return;
	}
    }
}




int
dfa_GetObsSamples (const DataFile *df, ZebraTime *times, Location *locs, 
		   int max)
/*
 * Return sample info from this observation.
 */
{
	OpenFile *ofp;

	if ((ofp = dfa_OpenFile (df, 0)) && (ofp->of_fmt->f_GetObsSamples))
	{
		return ((*ofp->of_fmt->f_GetObsSamples)
			(ofp, times, locs, max));
	}
	return (0);
}




int
dfa_GetFields (const DataFile *df, const ZebraTime *t, int *nfld, 
	       FieldId *flist)
/*
 * Return the available fields.  Enforce the policy that nfld is set
 * on return, setting it to zero if anything fails along the way.
 */
{
	OpenFile *ofp;
	int r = 0;

	if ((ofp = dfa_OpenFile (df, 0)) && ofp->of_fmt->f_GetFields)
	{	
		int sample = dfa_TimeIndex (ofp, t, 0);
		if (sample < 0)
			sample = 0;
		r = (*ofp->of_fmt->f_GetFields)(ofp, sample, nfld, flist);
	}
	if (! r)
	    *nfld = 0;
	return (r);
}



#ifndef NO_GETATTR
char *
dfa_GetAttr (const DataFile *df, const ZebraTime *t, int *len)
/*
 * Get the attributes for this time if we can.
 */
{
	OpenFile *ofp;
	int sample;

	if (! (ofp = dfa_OpenFile (df, 0)))
		return (NULL);
	if ((sample = dfa_TimeIndex (ofp, t, 0)) < 0)
		sample = 0;
	return (ofp->of_fmt->f_GetAttrs ?
		(*ofp->of_fmt->f_GetAttrs) (ofp, sample, len) : NULL);
}
#endif



int
dfa_QueryDate (int type, const char *name, ZebraTime *begin, ZebraTime *end, 
	       int *nsample)
/*
 * Query the dates on this file.
 */
{
	return ((*FMTP(type)->f_QueryTime) (name, begin, end, nsample));
}




DataChunk *
dfa_Setup (GetList *gl, FieldId *fields, int nfield, DataClass class)
/*
 * Set up to grab the data described by this GetList entry.
 */
{
	OpenFile *ofp;
	DataFormat *fmt;
	DataFile *dfp = &(gl->gl_df);
	DataOrganization org;

	fmt = FMTP(dfp->df_core.dfc_ftype);
/*
 * Check for compatibility of the request, then open the file and pass it on
 */
	org = ds_PlatformDataOrg (dfp->df_pid);
	if (! dfa_OrgClassCompat (fmt, org, class))
	{
		msg_ELog (EF_PROBLEM, "%s format: class-organization mismatch",
			  fmt->f_name);
		return (NULL);
	}
	if ((ofp = dfa_OpenFile (dfp, 0)))
		return ((*ofp->of_fmt->f_Setup) (ofp, fields, nfield, class));
	return (NULL);
}




void
dfa_GetData (DataChunk *dc, GetList *gl, dsDetail *details, int ndetail)
/*
 * Get the data from this getlist entry.
 */
{
	OpenFile *ofp;
	ZebraTime *times;
	int ntime;
	int begin;
	int end;
/*
 * Just to be sure, if end is less than beginning then something really
 * screwy's going on.
 */
	if (TC_Less (gl->gl_end, gl->gl_begin))
	{
		msg_ELog (EF_PROBLEM, "getdata on file %s: end < begin",
			  gl->gl_df.df_fullname);
		return;
	}
/*
 * Open the file.
 */
	if (! (ofp = dfa_OpenFile (&(gl->gl_df), 0)))
		return;
/*
 * Find the indices of the samples for the range of times we want.
 * Note we take the outside of the range: the first of any duplicate
 * begin times and the last of any duplicate end times.
 */
	begin = dfa_TimeIndex (ofp, &gl->gl_begin, 0);
	end = dfa_TimeIndex (ofp, &gl->gl_end, 1);
	if (end < 0)	/* the end time precedes first sample in file */
		return;
	times = dfa_GetTimes (ofp, &ntime);
/*
 * Use the first sample whose time is greater than or equal to the
 * begin time.  We may have to skip a series of duplicate times.
 */
	while ((begin < end) &&
	       (begin < 0 || TC_Less (times[begin], gl->gl_begin)))
		++begin;
/*
 * If the indices identical and not equal to one of the desired times, then
 * we found a gap in which we don't actually have any data. 
 */
	if ((begin == end) && ! TC_Eq (times[begin], gl->gl_begin) &&
	    ! TC_Eq (times[end], gl->gl_end))
		return;
	(*ofp->of_fmt->f_GetData) (ofp, dc, begin, end - begin + 1,
				   details, ndetail);
}





int
dfa_PutBlock (const DataFile *df, DataChunk *dc, int sample, int nsample, 
	      WriteCode wc, dsDetail *details, int ndetail)
/*
 * If the file's format has a f_PutBlock() method, call it.
 * Otherwise call the f_PutSample() method for each sample in the block.
 */
{
	OpenFile *ofp = dfa_OpenFile (df, 1);
	int result, i;

	if (!ofp)
		return (FALSE);
	if (ofp->of_fmt->f_PutBlock)
	{
		return ((*ofp->of_fmt->f_PutBlock)
			(ofp, dc, sample, nsample, wc, details, ndetail));
	}
/*
 * If no block method, loop through each sample in the block.
 * Return FALSE if any of the f_PutSample() calls fail.
 */
	if (!ofp->of_fmt->f_PutSample)
	{
		msg_ELog (EF_PROBLEM, "%s format: no write sample methods",
			  ofp->of_fmt->f_name);
		return (FALSE);
	}
	msg_ELog (EF_DEVELOP, "%s format: %s (%i)", ofp->of_fmt->f_name,
		  "no block method; looping over samples", nsample);
	result = TRUE;
	for (i = sample; i < sample + nsample; ++i)
	{
		result &= (*ofp->of_fmt->f_PutSample)
			(ofp, dc, sample, wc, details, ndetail);
	}
	return ((result)?TRUE:FALSE);
}




int
dfa_GetAlts (const DataFile *df, FieldId fid, int offset, float *alts, 
	     int *nalts, AltUnitType *altunits)
/*
 * Get the altitudes from the given file associated with the given fid and 
 * forecast offset.
 */
{
	OpenFile *ofp;

	if ((ofp = dfa_OpenFile (df, 0)) && (ofp->of_fmt->f_GetAlts))
	{
		return ((*ofp->of_fmt->f_GetAlts)
			(ofp, fid, offset, alts, nalts, altunits));
	}
	return (FALSE);
}




int
dfa_GetForecastTimes (const DataFile *df, int *times, int *ntimes)
/*
 * Get the forecast times.
 */
{
	OpenFile *ofp;

	if ((ofp = dfa_OpenFile (df, 0)) &&
	    (ofp->of_fmt->f_GetForecastTimes))
	{
		return ((*ofp->of_fmt->f_GetForecastTimes)
			(ofp, times, ntimes));
	}
	return (FALSE);
}




int
dfa_DataTimes (const DataFile *df, const ZebraTime *when, TimeSpec which, 
	       int n, ZebraTime *dest)
{
	OpenFile *ofp;
	int count = 0;
	/*
	 * KLUGE: Force the "target" time to be less than or equal to the
	 * currently known end time in the DF entry.  For files that are
	 * being written in real time, and not rescanned for every addition,
	 * this keeps us from returning times later than the DF entry's
	 * end time (which can be a *big* problem later on).
	 */
	ZebraTime target = *when;
	if (TC_LessEq(*when, df->df_core.dfc_end))
	{
	  target = *when;
	}
	else
	{
	  msg_ELog(EF_DEBUG, "Tweaking target to the DF entry end time for %s",
		   df->df_core.dfc_name);
	  target = df->df_core.dfc_end;
	}

	if ((ofp = dfa_OpenFile (df, 0)))
	{
		if (ofp->of_fmt->f_DataTimes)
		{
			count = (*ofp->of_fmt->f_DataTimes)
				(ofp, &target, which, n, dest);
		}
		else
		{
			count = fmt_DataTimes (ofp, &target, which, n, dest);
		}
	}
	return (count);
}



char **
dfa_GetAssociatedFiles (const DataFile *df, int *nfiles)
/* 
 * Returns the full path name of associated files. The caller must free
 * filenames [0..n-1] and filenames. The number of files is returned in
 * nfiles. If there are associated files, the main file is returned in
 * filenames[0] and the associated files though [1..n-1].
 */ 
{
    DataFormat *fmt;
    char **filenames;

    fmt = FMTP(df->df_core.dfc_ftype);
    if (fmt->f_GetAssociatedFiles)
	return ((char **) (*fmt->f_GetAssociatedFiles) ( df, nfiles ));
    else
    {
        filenames = (char **) malloc ( sizeof(char *) );

        filenames[0]=(char *) malloc (strlen(df->df_fullname)+1);
	strcpy (filenames[0], df->df_fullname);

        *nfiles = 1;
        return filenames;
    }
}



/* ================================================================
 * Semi-private routines intended for internal DFA and formats only.
 * ================================================================ */


OpenFile *
dfa_OpenFile (const DataFile *df, int write)
/*
 * See to it that this file is open.  On success, return the pointer 
 * to the open file structure.  Otherwise return NULL.
 * Upon return, TAG contains the tag value.
 */
{
        PlatformId pid;
        OpenFile *ofp;
	DataFormat *fmt;

	fmt = FMTP(df->df_core.dfc_ftype);
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
 * If the file is open, check the revision and access, and return the pointer.
 */
        if ((ofp = dfa_FileIsOpen (df)))
        {
	/*
	 * Pointer to the rev in the DF portion of our open file
	 */
		long *ofrev = &(ofp->of_df.df_core.dfc_rev);
	    
                if (write && ! ofp->of_write)
                {
#ifdef DEBUG
			msg_ELog (EF_DEBUG, "re-opening r/o file for r/w: %s",
				  df->df_fullname);
			++OF_Reopens;
#endif
                        dfa_CloseFile (ofp);
                }
                else if (df->df_core.dfc_rev > *ofrev)
                {
                /*
                 * The latest data file entry has a new revision, so our
                 * open file's tag must be out of date.  Thus sync the file. 
                 */
                        msg_ELog (EF_DEBUG, "file %s out of sync: %d < %d",
                                  df->df_fullname, *ofrev,
				  df->df_core.dfc_rev);
#ifdef NCSYNC_FIXED
			dfa_SyncFile (ofp);
                        *ofrev = df->df_core.dfc_rev;
			dfa_AccessCount (ofp);
                        return (ofp);
#else
                /*
                 * close and re-open netCDF files to work around broken ncsync
                 */
                        if (df->df_core.dfc_ftype != FTNetCDF)
                        {
				dfa_SyncFile (ofp);
				*ofrev = df->df_core.dfc_rev;
				dfa_AccessCount (ofp);
				return (ofp);
                        }
                        else
                        {
                                msg_ELog (EF_DEBUG, "%s(%s #%d) to force sync",
                                          "ncsync bug: closing netCDF file ",
                                          df->df_fullname);
                                dfa_CloseFile (ofp);
                                /* it will get re-opened below */
                        }
#endif
                }
                else
                {
			dfa_AccessCount (ofp);
                        return (ofp);
                }
        }
/*
 * Nope, open it now.
 */
        pid = df->df_pid;
	ofp = dfa_GetOF (fmt, df, write);
	if ((*fmt->f_OpenFile) (ofp, write))
	{
#ifdef DEBUG
	    msg_ELog (EF_DEBUG, "opened file %s, %s", df->df_fullname,
                          (write) ? "read-write" : "read-only");
#endif
		dfa_AddOpenFile (ofp);
	}
	else	/* failure */
	{
		msg_ELog (EF_PROBLEM, "DFA: could not open file %s", 
			  df->df_fullname);
		dfa_FreeOF (ofp);
		ofp = NULL;
	}
	return (ofp);
}




int
dfa_SyncFile (ofp)
OpenFile *ofp;
{
	if (ofp->of_fmt->f_SyncFile)
		return ((*ofp->of_fmt->f_SyncFile) (ofp));
	else
		return (1);
}




ZebraTime *
dfa_GetTimes (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	*ntime = 0;
	if (! ofp->of_fmt->f_GetTimes)
	{
		msg_ELog (EF_PROBLEM, "%s format: missing GetTimes method",
			  ofp->of_fmt->f_name);
		return (NULL);
	}
	return ((*ofp->of_fmt->f_GetTimes)(ofp, ntime));
}



int 
dfa_TimeIndex (OpenFile *ofp, const ZebraTime *when, int last)
/*
 * Find the closest sample time to 'when' without going beyond 'when'.
 * Returns -1 if when precedes all times in the file.  Returns the
 * first time of a group of identical times unless last is non-zero.
 * If last is non-zero we return the last of a series of identical times.
 */
{
#	define MINTIME 25
	ZebraTime *times;
	int nsample;
	int step, i;
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
			if (TC_LessEq (times[i], *when))
			{
				ret = i;
				break;
			}
		}
	}
	else	/* binary search */
	{
		int top = nsample - 1, bottom = 0;
		while ((ret < 0) && (top > bottom + 1))
		{
			int mid = (top + bottom)/2;

			if (TC_Eq (*when, times[mid]))
				ret = mid;
			else if (TC_Less (times[mid], *when))
				bottom = mid;
			else
				top = mid;
		}
		/*
		 * The time at top is always greater than when, and the
		 * time at mid != when (else we would have returned), so
		 * either mid was less than when, in which case the answer is
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
	while (i >= 0 && i < nsample && TC_Eq (times[ret], times[i]))
	{
		ret += step;
		i += step;
	}
	return (ret);
}


/* ====================================================================
 * DataFormat abstract methods for inheritance in the 'sub-format' methods
 * ==================================================================== */


int
fmt_DataTimes (OpenFile *ofp, const ZebraTime *when, TimeSpec which, int n, 
	       ZebraTime *start)
/*
 * Find out when data is available.  Return at most n times in the
 * dest array corresponding to the 'which' TimeSpec for reference time
 * 'when'.  All of the returned times will be unique, which means
 * duplicate times in the file are copied only once.
 */
{
	int t, i;
	ZebraTime *times;
	ZebraTime *dest;
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
	else
	{
		msg_ELog (EF_PROBLEM, "%s datatimes: time spec not supported",
			  ofp->of_fmt->f_name);
		return (0);
	}
	return ((dest < start) ? (start - dest) : (dest - start));
}



int
fmt_MakeFileName (DataFormat *fmt, const char *plat_name, const ZebraTime *t, 
		  char *dest, dsDetail *details, int ndetail)
/*
 * Create a new file name for this platform, at this time.  The DataFormat
 * method uses the platform name, date, and time separated by periods,
 * followed by the extension (which usually contains a period).
 * Support several details to change things like the base file name or
 * the extension.
 */
{
	SValue v;
	char *ext = NULL;
/*
 * See if we're supposed to use an alternative extension.
 */
	if (ds_GetDetail (DD_FILE_EXT, details, ndetail, &v))
	{
		/* let the user know if this is not a recognized extension */
		ext = v.us_v_ptr;
		if (! ext)
		{
			msg_ELog (EF_PROBLEM, 
				  "DD_FILE_EXT detail with no string value");
		}
		else if (ext[0] != '.')
		{
			msg_ELog (EF_INFO, "%s '%s' %s",
				  "dfa will not recognize the file extension",
				  ext, "without a leading period");
		}
		else if (!dfa_MatchExt (fmt->f_ftype, ext))
		{
			msg_ELog (EF_INFO, "%s '%s' for %s format",
				  "dfa does not recognize the extension",
				  ext, fmt->f_name);
		}
	}
	if (! ext)			
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
		int year, mo, day, hr, min, sec, ms;
		char *c = dest;

		TC_ZtSplit (t, &year, &mo, &day, &hr, &min, &sec, &ms);

		sprintf (c, "%s", plat_name);
		c += strlen(c);
		sprintf (c, ".%04d%02d%02d", year, mo, day);
		c += strlen(c);
		sprintf (c, ".%02d%02d%02d", hr, min, sec);
		c += strlen(c);
		if (ms)	/* need microsecond resolution */
		{
			sprintf (c, ".%06d", ms);
			c += strlen(c);
		}
		sprintf (c, "%s", ext);
		dfa_Deslash (dest);
	}
	return (0);
}




/* ================================================================
 * Public DFA routines not corresponding to format instance methods
 * ================================================================ */


void
dfa_MakeFileName (const Platform *plat, const ZebraTime *t, char *dest, 
		  dsDetail *details, int ndetail)
{
	DataFormat *fmt = FMTP(pi_FileType (plat));
/*
 * Shouldn't need to make file names for format's we can't write
 */
	if (fmt->f_readonly)
	{
		msg_ELog (EF_PROBLEM, "%s format read-only: %s", fmt->f_name,
			  "cannot make file name");
		dest[0] = 0;
	}
	else if (! fmt->f_MakeFileName)
	{
		msg_ELog (EF_PROBLEM, 
			  "%s format is missing a MakeFileName method",
			  fmt->f_name);
		fmt_MakeFileName (fmt, pi_Name (plat), t, dest, 
				  details, ndetail);
	}
	else
	{
		(*fmt->f_MakeFileName)(fmt, pi_Name (plat), t, dest, 
				       details, ndetail);
	}
}




void
dfa_NoteRevision (const DataFile *df)
/*
 * Note that a revision has been signalled on this file
 */
{
	OpenFile *ofp = dfa_FileIsOpen (df);

	if (ofp)
		ofp->of_df.df_core.dfc_rev = df->df_core.dfc_rev;
}




void
dfa_ForceClosure (void)
/*
 * Go through the list of open files and close each one, then release
 * memory in the OpenFile free list.
 */
{
	OpenFile *ofp, *next;
	DataFormat *fmt;
	int i;

	while (OpenFiles)
		dfa_CloseFile (OpenFiles);
	/*
	 * Free the memory in the format lookaside lists as well
	 */
	for (i = 0; i < NumFormats; ++i)
	{
		fmt = *Formats[i];
		ofp = fmt->f_of_free;
		while (ofp)
		{
			next = ofp->of_next;
			free (ofp);
			ofp = next;
		}
		fmt->f_of_free = NULL;
		fmt->f_nfree = 0;
	}
}



int
dfa_CheckName (int type, const char *name)
/*
 * See if this file name is consistent with this file type.
 */
{
	char *ldot, *dot, *ext = FMTP(type)->f_ext;
/*
 * First hack of the day...a, um, certain file format puts its extension
 * at the beginning instead of the end.  Have a look for a ^ in the
 * match string, and test it directly at the beginning if it's there.
 * No "|" notation for now.
 */
	if (ext && ext[0] == '^' && !strncmp (ext + 1, name, strlen (ext + 1)))
		return (TRUE);
/*
 * Nope, the usual scheme applies.  A format without an extension matches
 * all files except dot-files.  dot-files are never matched.
 */
	dot = strrchr (name, '.');
	ldot = strchr (name, '.');
	return ((ldot != name) &&
		(!ext || (dot && dfa_MatchExt (type, dot))));
}



int
dfa_FindFormat (file)
char *file;
/*
 * Get the format of this file.
 */
{
	int fmt;
	char *dot;

	if (! (dot = strrchr (file, '.')) || (dot == file))
		return (-1);
	for (fmt = 0; fmt < NumFormats; fmt++)
	{
	/*
	 * Perform the kludgy front-end check here too.
	 */
		char *ext = FMTP(fmt)->f_ext;
		if (ext && ext[0] == '^' &&
				! strncmp (ext + 1, file, strlen (ext + 1)))
			return (fmt);
	/*
	 * Otherwise check the extension.
	 */
		if (dfa_MatchExt (fmt, dot))
			return (fmt);
	}
	return (-1);
}



/* ================================================================
 * DFA private routines below here
 * ================================================================ */


static void
dfa_AddOpenFile (ofp)
OpenFile *ofp;
/* 
 * Add an open file to the list.
 */
{
	OpenFile *zap;

	ofp->of_next = OpenFiles;
	OpenFiles = ofp;
	dfa_AccessCount (ofp);
	++ofp->of_fmt->f_nopened;
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
#ifdef DEBUG
		++OF_NPurged;
		msg_ELog (EF_DEBUG, "%s (%d): lru file %s being purged",
			  "DFA open limit", MaxOpenFiles, 
			  zap->of_df.df_core.dfc_name);
#endif
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
	CO_Compat *coctable = fmt->f_compat;
	int ncoc = fmt->f_ncompat;
/*
 * If there's no table, just blindly pass the pair through.
 * Otherwise, go through and see if we find the combination in the table.
 */
	if (!ncoc || !coctable)
		return (TRUE);
	for (i = 0; i < ncoc; ++i)
	{
		if (class == coctable[i].c_class && org == coctable[i].c_org)
			return (TRUE);
	}
	return (FALSE);
}




static OpenFile *
dfa_GetOF (DataFormat *fmt, const DataFile *df, int write)
/*
 * Return an initialized open file entry, but don't add it to the
 * open files chain yet.
 */
{
	OpenFile *ofp;

	if (fmt->f_of_free)
	{
		ofp = fmt->f_of_free;
		fmt->f_of_free = ofp->of_next;
		--fmt->f_nfree;
	}
	else
		ofp = (OpenFile *) malloc (fmt->f_of_size);
/*
 * Initialize the structure as best we can.
 */
	memset (ofp, 0, fmt->f_of_size);
	ofp->of_lru = 0;
	ofp->of_df = *df;
	ofp->of_next = NULL;
	ofp->of_write = write;
	ofp->of_fmt = fmt;
	return (ofp);
}



static void
dfa_FreeOF (ofp)
OpenFile *ofp;
/*
 * Put an open file structure on the free list
 */
{
	DataFormat *fmt = ofp->of_fmt;

	ofp->of_next = fmt->f_of_free;
	fmt->f_of_free = ofp;
	++fmt->f_nfree;
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
#ifdef DEBUG
	msg_ELog (EF_DEBUG, "closing file %s", victim->of_df.df_fullname);
#endif           
	(*victim->of_fmt->f_CloseFile) (victim);
	OF_NOpen--;
	dfa_FreeOF (victim);
}





static OpenFile *
dfa_FileIsOpen (const DataFile *df)
/*
 * Check and see if this file is open.
 */
{
	OpenFile *ofp;

	for (ofp = OpenFiles; ofp; ofp = ofp->of_next)
		if (dfa_OpenFileMatches (ofp, df))
			return (ofp);
	return (0);
}



static zbool
dfa_OpenFileMatches (OpenFile *ofp, const DataFile *df)
/*
 * Return true iff the platform id, source id, and file name are the same
 * for the given OpenFile and DataFile
 */
{
    return (ofp->of_df.df_pid == df->df_pid &&
	    ofp->of_df.df_srcid == df->df_srcid &&
	    ! strcmp (ofp->of_df.df_fullname, df->df_fullname));
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
 * Return the default extension for this format.  Returned string only
 * valid until the next call to dfa_GetExt.
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
char *dot;	/* file suffix to match against this format's extensions */
{
	char *ext;
	int dotlen;

	dotlen = strlen(dot);
	ext = FMTP(fmt)->f_ext;
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



static void
dfa_AccessCount (ofp)
OpenFile *ofp;
/*
 * Increment the access counter for an open file.  Currently
 * this is just a simple LRU algorithm.  Every public entry point to
 * the DFA routines first makes sure the file is open, so the access
 * counts in the open and create methods keep approximate track of the
 * application's references to a file.
 */
{
	ofp->of_lru = OF_Lru++;
}




