/*
 * The data file access package, for use from deep within the data store.
 *
 * DFA itself is not much more than a minimal wrapper which makes the rest
 * of the data store be format-independent.  We push most of the real work
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

# include <sys/types.h>
# include <sys/stat.h>
# include <errno.h>

# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# define NO_SHM
# include "dslib.h"
# include "dfa.h"
MAKE_RCSID ("$Id: DataFileAccess.c,v 3.12 1993-09-02 08:13:53 granger Exp $")


void	dfa_AddOpenFile FP ((int, DataFile *, int, void *));

/*
 * This is the structure which describes a format.
 */
/*
 * The DFA format-driver routines:
 *
 * f_QueryTime (file, begin, end, nsample)
 * char *file;
 * ZebTime *begin, *end;
 * int *nsample
 *
 *	Given the file name, return the begin and end times of the data
 *	found therein.  For a given format, it is acceptible to calculate
 *	these times from the file name, if it is certain that (1) the time
 *	period covered by the file is not greater than what is returned, 
 *	and (2) if the time period is less, no other file will cover the
 *	missing period.  NSAMPLE should be set to the number of data
 *	samples found in this file.  Returns TRUE on success.
 *
 * DataChunk *
 * f_Setup (glist, fields, nfield, class)
 * GetList *glist;
 * FieldId *fields;
 * int nfield;
 * DataClass class;
 *
 *	Get ready to do this data access.  The format driver should, at
 *	a minimum, create and return a data chunk with the appropriate
 *	class.  The get list is provided to aid setup for efficiency.
 *
 * f_OpenFile (dp, write, tag)
 * DataFile *dp;
 * bool write;
 * void **tag;
 *
 *	Open the given file, returning TRUE if success.  The TAG value is
 *	for the format-driver use only -- it will be passed into all other
 * 	file operations.
 *
 * f_CloseFile (tag)
 * void *tag;
 *
 *	Close this file.
 *
 * f_SyncFile (tag)
 * void *tag
 *
 *	Synchronize this file to catch up with updates which have occurred.
 *	Returns TRUE on success.  If this routine is missing, it is assumed
 *	that updates are automatically available.
 *
 * f_InqNPlat (dfindex)
 *
 *	Return the number of platforms contained here.
 *
 * f_GetData (dc, getlist, details, ndetail)
 * DataChunk *dc;
 * GetList *getlist;
 * dsDetail *details;
 * int ndetail;
 *
 *	Actually get the data called for here.
 *
 * f_GetIRGLoc (dfindex, locs)
 * int dfindex
 * Location *locs;
 *
 *	Copy over the locations for this IRGrid data.
 *
 * f_InqRGrid (dfindex, origin, rg)
 * Location *origin;
 * RGrid *rg;
 *
 *	Return the location and size info for this grid.
 *
 * f_DataTimes (index, time, which, n, dest, attrs)
 * int index, n;
 * ZebTime *time, *dest;
 * TimeSpec which;
 * char *attrs;
 *
 * 	Return a list of times for which data is available.
 *
 * f_MakeFileName (directory, platform, time, dest)
 * char *directory, *platform;
 * ZebTime *time;
 * char *dest;
 *
 *	Create an appropriate name for a new file in directory, starting
 *	at the given time.
 *
 * f_CreateFile (fname, dfile, dc, tag, details, ndetail)
 * char *fname;
 * DataFile *dfile;
 * DataChunk *dc;
 * void **tag;
 * dsDetail *details;
 * int ndetail;
 *
 *	Cause the given file to exist, returning TRUE and a tag if
 *	successful.  DC describes the current data put request, which
 *	should describe the layout of the file.
 *
 *	12/4/92 gg Added the dsDetail parameters.  As these are at the end
 *	of the parameter list, they do not need to be explicitly named in calls
 *	to the file format's create method.  But they're there if you need 'em.
 *
 * f_PutSample (dfile, dc, sample, wc)
 * int dfile, sample;
 * DataChunk *dc;
 * WriteCode wc;
 *
 *	Write the given sample from the DC into the indicated file, as
 *	controlled by the write code:
 *		wc_Append	Append the sample to the file
 *		wc_Insert	Insert before an existing sample.
 *		wc_OverWrite	Overwrite an existing sample.
 *	Returns true iff the data write was successful.
 *
 * f_PutBlock (dfile, dc, sample, nsample, wc)
 * int dfile;
 * DataChunk *dc;
 * int sample;
 * int nsample;
 * WriteCode wc;
 *
 * 	Write a block of 'nsample' samples, beginning at 'sample'
 *	in the data chunk, using write code 'wc', into the file
 * 	'dfile'.  A block is a contiguous series of samples
 *	to be written to the same file.
 *
 * f_PutData (dfile, dobj, begin, end)  This is no longer real
 * int *dfile;
 * DataObject *dobj;
 * int begin, end;
 *
 *	Put samples BEGIN through END (inclusive) from DOBJ into DFILE.
 *
 * f_GetObsSamples (dfile, times, locs, max)
 * int dfile, max;
 * time *times;
 * Location *locs;
 *
 *	Return information on each of the samples in this observation;
 *	return value is the number actually returned.
 *
 * f_GetFields (dfile, time, nfld, flist)
 * int dfile;
 * time *time;
 * int *nfld;
 * FieldId *flist;
 *
 *	Return a list of available fields in this platform.  "NFLD" starts
 * 	as the max the caller can accept; should be returned as the number
 *	of fields actually returned.
 *
 * char *
 * f_GetAttrs (dfile, time, len)
 * int dfile, *len;
 * ZebTime *time;
 *
 *	Get the attributes from the file for this time.  Returns a
 *	piece of dynamically allocated memory (whose length comes back
 *	in *len) containing the attribute table.  Return NULL if it
 *	can't be done.
 */

struct DataFormat
{
	char *f_name;			/* Name of this format		*/
	char *f_ext;			/* File name extension		*/
	/* Functions below here */
	int (*f_QueryTime)();		/* Query the times of a file	*/
	DataChunk *(*f_Setup) ();	/* Set up access		*/
	int (*f_OpenFile) ();		/* Open a file			*/
	int (*f_CloseFile) ();		/* Close a file.		*/
	int (*f_SyncFile) ();		/* Synchronize a file		*/
	int (*f_InqNPlat) ();		/* Inquire number platforms	*/
	int (*f_GetData) ();		/* Get the data			*/
	int (*f_GetIRGLoc) ();		/* Get irgrid locations		*/
	int (*f_InqRGrid) ();		/* Get RGrid info		*/
	int (*f_DataTimes) ();		/* Get data times		*/
	int (*f_MakeFileName) ();	/* Make new file name		*/
	int (*f_CreateFile) ();		/* Create a new file		*/
	int (*f_PutSample) ();		/* Put data to a file		*/
	int (*f_PutBlock) ();		/* Write a block to a file	*/
	int (*f_GetObsSamples) ();	/* Get observation samples	*/
	int (*f_GetFields) ();		/* Get fields			*/
	char *(*f_GetAttrs) ();		/* Get attributes		*/
};


/*
 * Function definitions for the format table.
 */
extern int dnc_QueryTime (), dnc_OpenFile (), dnc_CloseFile ();
extern int dnc_SyncFile (), dnc_InqPlat (), dnc_GetData ();
extern int dnc_GetIRGLoc (), dnc_GetRGrid (), dnc_DataTimes ();
extern int dnc_MakeFileName (), dnc_CreateFile (), dnc_PutSample ();
extern int dnc_PutBlock ();
extern int dnc_GetFields (), dnc_GetObsSamples ();
extern DataChunk *dnc_Setup ();

extern int bf_QueryTime (), bf_MakeFileName (), bf_CreateFile ();
extern int bf_PutSample (), bf_OpenFile (), bf_CloseFile (), bf_SyncFile ();
extern int bf_GetData (), bf_DataTimes (), bf_GetFields ();
extern DataChunk *bf_Setup ();

extern int drf_OpenFile (), drf_CloseFile (), drf_QueryTime ();
extern int drf_PutSample ();
extern int drf_MakeFileName (), drf_CreateFile (), drf_Sync ();
extern int drf_GetData (), drf_DataTimes (), drf_GetObsSamples ();
extern int drf_GetFields ();
extern char *drf_GetAttrs ();
extern DataChunk *drf_Setup ();

/*
 * Zeb Native Format.
 */
extern int	zn_CreateFile (), zn_QueryTime (), zn_GetIRGLoc (), zn_Sync ();
extern int	zn_PutSample (), zn_Close (), zn_Open (), zn_MakeFileName ();
extern int	zn_GetData (), zn_InqNPlat (), zn_Times ();
extern int	zn_GetObsSamples (), zn_Fields (), zn_InqRGrid ();
extern int	zn_PutSampleBlock ();
extern DataChunk *zn_Setup ();


# define ___ 0

/*
 * And here is the format table.  Indexing into this table is done through
 * the FileType enum in dsPrivate.h.
 */
struct DataFormat Formats[] =
{
/*
 * The netCDF format.
 */
    {
	"netCDF",	".cdf",
	dnc_QueryTime,			/* Query times			*/
	dnc_Setup,			/* setup			*/
	dnc_OpenFile,			/* Open				*/
	dnc_CloseFile,			/* Close			*/
	dnc_SyncFile,			/* Synchronize			*/
	dnc_InqPlat,			/* Inquire platforms		*/
	dnc_GetData,			/* Get the data			*/
	dnc_GetIRGLoc,			/* Get IRGrid locations		*/
	dnc_GetRGrid,			/* Get RGrid info		*/
	dnc_DataTimes,			/* Get data times		*/
	dnc_MakeFileName,		/* Make file name		*/
	dnc_CreateFile,			/* Create a new file		*/
	dnc_PutSample,			/* Write to file		*/
	dnc_PutBlock,			/* Write block to a file	*/
	dnc_GetObsSamples,		/* Get observation samples	*/
	dnc_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
    },
    {
	"Boundary",	".bf",
	bf_QueryTime,			/* Query times			*/
	bf_Setup,			/* setup			*/
	bf_OpenFile,			/* Open				*/
	bf_CloseFile,			/* Close			*/
	bf_SyncFile,			/* Synchronize			*/
	___,				/* Inquire platforms		*/
	bf_GetData,			/* Get the data			*/
	___,				/* Get IRGrid locations		*/
	___,				/* Get RGrid info		*/
	bf_DataTimes,			/* Get data times		*/
	bf_MakeFileName,		/* Make file name		*/
	bf_CreateFile,			/* Create a new file		*/
	bf_PutSample,			/* Write to file		*/
	___,				/* Write block to a file	*/
	___,				/* Get observation samples	*/
	bf_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
    },
    {
    	"Raster",	".rf",
	drf_QueryTime,			/* Query times			*/
	drf_Setup,			/* setup			*/
	drf_OpenFile,			/* Open				*/
	drf_CloseFile,			/* Close			*/
	drf_Sync,			/* Synchronize			*/
	___,				/* Inquire platforms		*/
	drf_GetData,			/* Get the data			*/
	___,				/* Get IRGrid locations		*/
	___,				/* Get RGrid info		*/
	drf_DataTimes,			/* Get data times		*/
	drf_MakeFileName,		/* Make file name		*/
	drf_CreateFile,			/* Create a new file		*/
	drf_PutSample,			/* Write to file		*/
	___,				/* Write block to a file	*/
	drf_GetObsSamples,		/* Get observation samples	*/
	drf_GetFields,			/* Get fields			*/
	drf_GetAttrs,			/* Get Attributes		*/
    },
    {
    	"CmpRaster",	".rf",
	drf_QueryTime,			/* Query times			*/
	drf_Setup,			/* setup			*/
	drf_OpenFile,			/* Open				*/
	drf_CloseFile,			/* Close			*/
	drf_Sync,			/* Synchronize			*/
	___,				/* Inquire platforms		*/
	drf_GetData,			/* Get the data			*/
	___,				/* Get IRGrid locations		*/
	___,				/* Get RGrid info		*/
	drf_DataTimes,			/* Get data times		*/
	drf_MakeFileName,		/* Make file name		*/
	drf_CreateFile,			/* Create a new file		*/
	drf_PutSample,			/* Write to file		*/
	___,				/* Write block to a file	*/
	drf_GetObsSamples,		/* Get observation samples	*/
	drf_GetFields,			/* Get fields			*/
	drf_GetAttrs,			/* Get Attributes		*/
    },
/*
 * Zeb Native.
 */
    {
	"Zeb",		".znf",
	zn_QueryTime,			/* Query times			*/
	zn_Setup,			/* setup			*/
	zn_Open,			/* Open				*/
	zn_Close,			/* Close			*/
	zn_Sync,			/* Synchronize			*/
	zn_InqNPlat,			/* Inquire platforms		*/
	zn_GetData,			/* Get the data			*/
	zn_GetIRGLoc,			/* Get IRGrid locations		*/
	zn_InqRGrid,			/* Get RGrid info		*/
	zn_Times,			/* Get data times		*/
	zn_MakeFileName,		/* Make file name		*/
	zn_CreateFile,			/* Create a new file		*/
	zn_PutSample,			/* Write to file		*/
	zn_PutSampleBlock,		/* Write block to a file	*/
	zn_GetObsSamples,		/* Get observation samples	*/
	zn_Fields,			/* Get fields			*/
	___,				/* Get Attributes		*/
    }
};



/*********************************************************************/
/*
 * Stuff for the open file table.
 */
static int MaxOpenFiles = 15;		/* How many we can keep open	*/

typedef struct _OpenFile
{
	int	of_lru;			/* Access count			*/
	void	*of_tag;		/* Format-specific tag		*/
	int	of_dfindex;		/* DF structure index		*/
	struct _OpenFile *of_next;	/* Next in chain		*/
	int	of_write;		/* File open for write access	*/
	DataFile of_dfe;		/* The data file entry		*/
} OpenFile;

static OpenFile *OpenFiles = 0;		/* Open file list head		*/
static OpenFile *OFFree = 0;		/* lookaside list		*/
static int OF_Lru = 0;			/* LRU count			*/
static int OF_NOpen = 0;		/* Number of open files		*/



/*
 * Local routines.
 */
# ifdef __STDC__
	static OpenFile *dfa_GetOF (void);
	static void 	dfa_CloseFile (OpenFile *);
# else
	static OpenFile *dfa_GetOF ();
	static void 	dfa_CloseFile ();
# endif




int
dfa_FindFormat (file)
char *file;
/*
 * Get the format of this file.
 */
{
	int fmt;
	char *cp, *strrchr ();

	if (! (cp = strrchr (file, '.')))
		return (-1);
	for (fmt = 0; fmt < sizeof(Formats)/sizeof(struct DataFormat); fmt++)
		if (! strcmp (cp, Formats[fmt].f_ext))
			return (fmt);
	return (-1);
}






int
dfa_CheckName (type, name)
int type;
char *name;
/*
 * See if this file name is consistent with this file type.
 */
{
	char *dot, *strrchr ();

	dot = strrchr (name, '.');
	return (dot && ! strcmp (dot, Formats[type].f_ext));
}





void
dfa_MakeFileName (plat, t, dest)
Platform *plat;
ZebTime *t;
char *dest;
/*
 * Create a new file name for this platform, and this time.
 */
{
	char *slash, *strchr ();
/*
 * Get the format-specific code to make up the name, then tweak any 
 * slashes out of it.
 */
	(*Formats[plat->dp_ftype].f_MakeFileName) (plat->dp_dir,
				plat->dp_name, t, dest);
	while (slash = strchr (dest, '/'))
		strcpy (slash, slash + 1);
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
	FileType ft;
	DataFile dfe;

	ds_GetFileStruct (dfile, &dfe);
	ft = dfe.df_ftype;

	return (Formats[ft].f_GetObsSamples ?
		((*Formats[ft].f_GetObsSamples)(dfile, times, locs, max)) : 0);
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
	FileType ft;
	DataFile dfe;

	ds_GetFileStruct (dfile, &dfe);
	ft = dfe.df_ftype;

	return (Formats[ft].f_GetFields ?
		((*Formats[ft].f_GetFields) (dfile, t, nfld, flist)) : 0);
}




char *
dfa_GetAttr (dfile, t, len)
int dfile, *len;
ZebTime *t;
/*
 * Get the attributes for this time if we can.
 */
{
	FileType ft;
	DataFile dfe;

	ds_GetFileStruct (dfile, &dfe);
	ft = dfe.df_ftype;

	return (Formats[ft].f_GetAttrs ?
		(*Formats[ft].f_GetAttrs) (dfile, t, len) : 0);
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
	return ((*Formats[type].f_QueryTime) (name, begin, end, nsample));
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
	FileType ft;
	DataFile dfe;

	ds_GetFileStruct (gl->gl_dfindex, &dfe);
	ft = dfe.df_ftype;

	return ((*Formats[ft].f_Setup) (gl, fields, nfield, class));
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
	DataFile dfe;
	ds_GetFileStruct (gl->gl_dfindex, &dfe);
/*
 * Do the snarf.
 */
	(*Formats[dfe.df_ftype].f_GetData) (dc, gl, details, ndetail);
}





# ifdef notdef
void
dfa_PutData (dfile, dobj, begin, end)
int dfile, begin, end;
DataObject *dobj;
/*
 * Add data to this file.
 */
{
	(*Formats[DFTable[dfile].df_ftype].f_PutData) (dfile, dobj, begin,end);
}

# endif


int
dfa_PutSample (dfile, dc, sample, wc)
int dfile, sample;
DataChunk *dc;
WriteCode wc;
/*
 * Add data to this file.
 */
{
	DataFile dfe;
	ds_GetFileStruct (dfile, &dfe);

	return ((*Formats[dfe.df_ftype].f_PutSample) (dfile, dc, sample,wc));
}


int
dfa_PutBlock (dfile, dc, sample, nsample, wc)
int dfile;
DataChunk *dc;
int sample, nsample;
WriteCode wc;
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
			(dfile, dc, sample, nsample, wc));
/*
 * otherwise loop through the samples in the block
 */
	msg_ELog (EF_DEBUG, "%s: no block method, looping over %d samples",
		  Formats[dfe.df_ftype].f_name, nsample);
	result = TRUE;
	for (i = sample; i < sample + nsample; ++i)
		result &= dfa_PutSample(dfile, dc, i, wc);
/*
 * Return FALSE if any of the dfa_PutSample() calls failed
 */
	return((result)?TRUE:FALSE);
}



int
dfa_InqNPlat (index)
int index;
/*
 * Find out how many platforms are here.
 */
{
	DataFile dfe;
	ds_GetFileStruct (index, &dfe);

	if (Formats[dfe.df_ftype].f_InqNPlat)
		return ((*Formats[dfe.df_ftype].f_InqNPlat) (index));
	return (1);
}




int
dfa_InqRGrid (index, origin, rg)
int index;
Location *origin;
RGrid *rg;
/*
 * Get the rgrid params.
 */
{
	DataFile dfe;
	ds_GetFileStruct (index, &dfe);

	if (Formats[dfe.df_ftype].f_InqRGrid)
		return ((*Formats[dfe.df_ftype].f_InqRGrid)
					(index, origin, rg));
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
/*
 * Get available data times.
 */
	if (Formats[dfe.df_ftype].f_DataTimes)
		return ((*Formats[dfe.df_ftype].f_DataTimes) (index, when,
					which, n, dest));
	else
		return (0);
}




/*ARGSUSED*/
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
	Platform p;
/*
 * Make sure that it isn't somehow open now.  (Would be strange but 
 * can't hurt to be sure.)
 */
	dfa_ForceClose (df);
	ds_GetFileStruct (df, &dfe);
	ds_GetPlatStruct (dfe.df_platform, &p, FALSE);
/*
 * Try to open up the file.  If successful, do our accounting and return
 * our success.
 */
	if (! (*Formats[dfe.df_ftype].f_CreateFile) (dfa_FilePath (&p, &dfe),
						     &dfe, dc, &tag,
						     details, ndetail))
		return (FALSE);
	dfa_AddOpenFile (df, &dfe, TRUE, tag);
	return (TRUE);
}



/*********
 * DFA private routines below here.
 *********/





void
dfa_AddOpenFile (dfindex, df, write, tag)
DataFile *df;
int write, dfindex;
void *tag;
/* 
 * Add an open file to the list.
 */
{
	OpenFile *ofp = dfa_GetOF (), *zap;
/*
 * Fill in the file structure and add it to the list.
 */
	ofp->of_dfindex = dfindex;
	ofp->of_lru = OF_Lru++;
	ofp->of_tag = tag;
	ofp->of_next = OpenFiles;
	ofp->of_write = write;
	ofp->of_dfe = *df;
	OpenFiles = ofp;
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





static OpenFile *
dfa_GetOF ()
/*
 * Return an open file entry.
 */
{
	OpenFile *ret;

	if (OFFree)
	{
		ret = OFFree;
		OFFree = ret->of_next;
	}
	else
		ret = ALLOC (OpenFile);
	return (ret);
}






static void
dfa_CloseFile (victim)
OpenFile *victim;
/*
 * Close this file, whether it wants to be or not.
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
			msg_ELog (EF_PROBLEM, "OF entry 0x%x (%s) missing",
				victim, victim->of_dfe.df_name);
			return;
		}
		prev->of_next = victim->of_next;
	}
/*
 * Get the actual file closed.
 */
	(*Formats[victim->of_dfe.df_ftype].f_CloseFile) (victim->of_tag);
	OF_NOpen--;
/*
 * Release the structure, and we're done.
 */
	victim->of_next = OFFree;
	OFFree = victim;
}





void
dfa_ForceClose (dfindex)
int dfindex;
/*
 * If this file index is opened, close it now.
 */
{
	OpenFile *ofp;

	for (ofp = OpenFiles; ofp; ofp = ofp->of_next)
		if (ofp->of_dfindex == dfindex)
		{
			dfa_CloseFile (ofp);
			return;
		}
}





void
dfa_ForceClosure ()
/*
 * Go through the list of open files and close each one, then release
 * memory in the OpenFile free list.
 */
{
	OpenFile *ofp, *next;

	for (ofp = OpenFiles; ofp; ofp = ofp->of_next)
		dfa_CloseFile (ofp);
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






void
dfa_NoteRevision (p, dfindex)
Platform *p;
int dfindex;
/*
 * Note that a revision has been signalled on this file.
 */
{
	OpenFile *ofp = dfa_FileIsOpen (dfindex);

	if (ofp)
		ofp->of_dfe.df_rev = dfa_GetRevision (p, &ofp->of_dfe);
		/* ofp->of_rev++; */
}



int
dfa_OpenFile (dfindex, write, tag)
int dfindex;
int write;
void **tag;
/*
 * See to it that this file is open.  On success, the return value is TRUE,
 * and TAG has the tag value.
 */
{
	DataFile df;
	Platform p;
	OpenFile *ofp;
	int retv = TRUE;

	ds_GetFileStruct (dfindex, &df);
/*
 * If the file is open, check the revision and access and return the tag.
 */
	if (ofp = dfa_FileIsOpen (dfindex))
	{
		*tag = ofp->of_tag;
		if (write && ! ofp->of_write)
			dfa_CloseFile (ofp);
		else
		{
			if (df.df_rev > ofp->of_dfe.df_rev)
			{
				msg_ELog (EF_DEBUG, "Out of rev file %s",
					df.df_name);
				retv = (*Formats[df.df_ftype].f_SyncFile)
						(*tag);
				ofp->of_dfe.df_rev = df.df_rev;
			}
			return (retv);
		}
	}
/*
 * Nope, open it now.
 */
	ds_GetPlatStruct (df.df_platform, &p, FALSE);
	if (! (*Formats[df.df_ftype].f_OpenFile) (dfa_FilePath (&p, &df), &df,
						write, tag))
		retv = FALSE;
	else 	/* success */
		dfa_AddOpenFile (dfindex, &df, write, *tag);

	return (retv);
}





char *
dfa_FilePath (p, df)
Platform *p;
DataFile *df;
/*
 * Generate the full name of this data file.  The name is returned in
 * static space and will get zapped with the next call.
 */
{
	static char fname[1024];

	sprintf (fname, "%s/%s", (df->df_flags & DFF_Remote) ?
		p->dp_rdir : p->dp_dir, df->df_name);
	return (fname);
}




long 
dfa_GetRevision (p, df)
Platform *p;
DataFile *df;
/*
 * Get a revision count for this file.
 */
{
	struct stat sbuf;

	if (stat (dfa_FilePath (p, df), &sbuf) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d on stat of %s", errno,
				dfa_FilePath (p, df));
		return (0);
	}
	return (sbuf.st_mtime);
}
