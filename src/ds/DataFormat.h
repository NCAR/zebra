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

/*
 * $Id: DataFormat.h,v 3.7 2002-09-17 18:28:43 granger Exp $
 */
#ifndef _zebra_dataformat_h_
#define _zebra_dataformat_h_

# ifdef __cplusplus
extern "C" {
# endif

#define N_COC(ra) (sizeof(ra)/sizeof((ra)[0]))

#define ___ 0


/* ========================================================================
 * The DFA abstract format structure
 * ------------------------------------------------------------------------
 * 
 * 'Class' methods: Those functions that do not operate on an existing
 * open file.  Some formats will in fact open a file to perform the function,
 * but that is not strictly necessary as it is for instance methods.
 *
 * For each method, define a macro which will expand to 
 * the parameter list for that method.
 */

#define M_QueryTime(fn) \
int fn (const char *file, ZebTime *begin, ZebTime *end, int *nsample)
#define P_QueryTime(fn) static M_QueryTime(fn)
/*
 *	Given the file name, return the begin and end times of the data
 *	found therein.  For a given format, it is acceptable to calculate
 *	these times from the file name, if it is certain that (1) the time
 *	period covered by the file is not greater than what is returned, 
 *	and (2) if the time period is less, no other file will cover the
 *	missing period.  NSAMPLE should be set to the number of data
 *	samples found in this file.  Returns TRUE on success.
 */

#ifdef notdef /* to be added eventually */
#define M_QueryPlatform(fn) \
int fn (char *file, ClassDef *cd)
#define P_QueryPlatform(fn) static M_QueryPlatform(fn)
/*
 * 	Given the file name, try to deduce from the contents of the file
 *	a platform class definition suitable for reading and possibly
 *	writing the file.  The class information could have been stored
 *	as attributes in the file, or it may have to be deduced from other
 *	characteristics of the file, such as the file name, fields, and
 *	number of samples.
 */
#endif

#define M_MakeFileName(fn) \
int fn (struct _DataFormat *fmt, const char *platform, const ZebraTime *time, \
	char *dest, dsDetail *details, int ndetail)
#define P_MakeFileName(fn) static M_MakeFileName(fn)
/*
 *	Create an appropriate name in dest for a new file for platform, 
 * 	starting at the given time.
 */

/* -----------------------------------------------------------------------
 * 'Instance' methods, which operate with an open file structure.  (Not
 * necessarily an open file yet or initialized structure.)
 */

#define M_CreateFile(fn) \
int fn (struct _OpenFile *ofp, DataChunk *dc, dsDetail *details, int ndetail)
#define P_CreateFile(fn) static M_CreateFile(fn)
/*
 *	Cause the given file to exist, returning TRUE iff
 *	successful.  DC describes the current data put request, which
 *	should describe the layout of the file.  OFP points to the open
 *	file instance structure whose format-specific part the format
 *	method should be filled in.
 */

#define M_OpenFile(fn) \
int fn (struct _OpenFile *of, int write)
#define P_OpenFile(fn) static M_OpenFile(fn)
/*
 *	Open the given filepath, returning TRUE if success.  The OpenFile
 *	structure will be of the correct format instance size; it is up
 *	to the format to fill it in.
 */

#define M_CloseFile(fn) \
void fn (struct _OpenFile *of)
#define P_CloseFile(fn) static M_CloseFile(fn)
/*
 *	Close this file.  The format should not free its tag, only any
 *	dynamic memory it allocated to which its tag members point.
 */

#define M_SyncFile(fn) \
int fn (struct _OpenFile *of)
#define P_SyncFile(fn) static M_SyncFile(fn)
/*
 *	Synchronize this file to catch up with updates which have occurred.
 *	Returns TRUE on success.  If this routine is missing, it is assumed
 *	that updates are automatically available and returns true.
 */

#define M_Setup(fn) \
DataChunk *fn (struct _OpenFile *of, FieldId *fields, int nfield, \
	       DataClass dclass)
#define P_Setup(fn) static M_Setup(fn)
/*
 *	Get ready to do this data access.  The format driver should, at
 *	a minimum, create and return a data chunk with the appropriate
 *	class.  Before being called, the file will have been opened
 *	and the requested class checked against the format's class-org
 *	compatibility table.  
 *
 * 	The Setup method should accept NULL fields and zero nfield,
 *	especially in the case of location and other classes which have
 *	no fields.  Some calls with zero fields will be fetches for
 *	sample attributes prior to fetching actual data.
 *
 *	For the moment, no format methods need time-specific information
 *	here.  This method sets up a datachunk for the given file, fields,
 *	and class with time-independent information.  Perhaps higher-level
 * 	DFA routines can call setup on each file to be accessed, but so far
 *	this is not necessary either.  Also, dsDetail parameters could be 
 * 	added later if needed.
 */

#define M_GetData(fn) \
int fn (struct _OpenFile *of, DataChunk *dc, int begin, int nsample, \
	dsDetail *details, int ndetail)
#define P_GetData(fn) static M_GetData(fn)
/*
 *	Actually get the data from the given series of samples and append
 *	the data to the datachunk.  The given range of samples is the
 *	inclusive times of desired data, and the given samples should exist
 *	in the file.  In the event of details or new parameters which
 *	specify sub-sampling or interleaving, fewer samples may be appended
 *	to the datachunk than 'nsample', but no samples will be appended
 *	outside of the given range.
 */

#define M_GetAlts(fn) \
int fn (struct _OpenFile *ofp, FieldId fid, int offset, float *alts, \
	int *nalts, AltUnitType *altunits)
#define P_GetAlts(fn) static M_GetAlts(fn)
/*
 *	Return the vertical levels associated with the given field and forecast
 *	offset time from the given file.  Return value is non-zero on success.
 *	The default method returns FALSE.  Altitudes are copied into the alts
 *	array without regard for how many altitudes alts points to.
 */

#define M_GetForecastTimes(fn) \
int fn (struct _OpenFile *of, int *times, int *ntimes)
#define P_GetForecastTimes(fn) static M_GetForecastTimes(fn)
/*
 *	Return the available model forecast offset times in the given file.
 */

#define M_DataTimes(fn) \
int fn (struct _OpenFile *of, const ZebraTime *time, TimeSpec which, int n, \
	ZebraTime *dest)
#define P_DataTimes(fn) static M_DataTimes(fn)
/*
 * 	Return a list of times for which data is available.  The return
 *	value holds the number of times copied into 'dest'.
 */

#define M_PutSample(fn) \
int fn (struct _OpenFile *of, DataChunk *dc, int sample, WriteCode wc, \
	dsDetail *details, int ndetail)
#define P_PutSample(fn) static M_PutSample(fn)
/*
 *	Write the given sample from the DC into the indicated file, as
 *	controlled by the write code:
 *		wc_Append	Append the sample to the file
 *		wc_Insert	Insert before an existing sample.
 *		wc_OverWrite	Overwrite an existing sample.
 *	Returns true iff the data write was successful.
 */

#define M_PutBlock(fn) \
int fn (struct _OpenFile *of, DataChunk *dc, int sample, int nsample, \
	WriteCode wc, dsDetail *details, int ndetail)
#define P_PutBlock(fn) static M_PutBlock(fn) 
/*
 * 	Write a block of 'nsample' samples, beginning at 'sample'
 *	in the data chunk, using write code 'wc', into the file
 * 	'dfile'.  A block is a contiguous series of samples
 *	to be written to the same file.
 */

#define M_GetObsSamples(fn) \
int fn (struct _OpenFile *of, ZebTime *times, Location *locs, int max)
#define P_GetObsSamples(fn) static M_GetObsSamples(fn)
/*
 *	Return the time and location for each of the samples in this
 *	observation; return value is the number actually returned.
 */

#define M_GetFields(fn) \
int fn (struct _OpenFile *of, int sample, int *nfld, FieldId *flist)
#define P_GetFields(fn) static M_GetFields(fn)
/*
 *	Return a list of available fields in this platform.  "NFLD" starts
 * 	as the max the caller can accept; should be returned as the number
 *	of fields actually returned.  The return value is non-zero if the
 *	call succeeds.
 */

#define M_GetAttrs(fn) \
char * fn (struct _OpenFile *of, int sample, int *len)
#define P_GetAttrs(fn) static M_GetAttrs(fn)
/*
 *	Get the attributes from the file for this sample.  Returns a
 *	piece of dynamically allocated memory (whose length comes back
 *	in *len) containing the attribute table.  Return NULL if it
 *	can't be done.
 *
 *	This should really be done as data: fetch a datachunk with zero
 *	fields at the desired time, then test the sample's attributes.
 *	Presently this method is very raster-specific and tied to the
 *	attribute format in raster files.
 */

/* ---------------------------------------------------------------------
 * Recent additions:
 */

#define M_GetTimes(fn) \
ZebTime * fn (struct _OpenFile *of, int *ntime)
#define P_GetTimes(fn) static M_GetTimes(fn)
/*
 * 	Return an array of sample times in the file.  The array should not
 *	be freed or modified by the caller.  The sample times will be in
 *	chronological order, but there may be duplicates.
 */

#define M_GetAssociatedFiles(fn) \
char ** fn (const DataFile *df, int *nfiles)
#define P_GetAssociatedFiles(fn) static M_GetAssociatedFiles(fn)
/*      Return an array of associated file names (in filenames) and the 
 *      number of files found (in nfiles). The array should be freed
 *      by the caller of the function. The function returns NULL if there
 *      are no files associated to that one.
 */

/* int
 * f_TimeIndex (OpenFile *of, const ZebraTime *time, int last)
 *	
 *	Find the sample index of the given time.  If last is true, return
 *	the last time of a series of identical times.  Otherwise return
 *	the first time.
 *
 * int
 * f_GetLocs (OpenFile *of, int begin, int nsample, Location *locns)
 *
 *	Copy the locations of the given series of samples into the
 *	locns array.  Return zero on failure, otherwise return the
 * 	number of locations actually copied, which should be nsample.
 */

/* ======================================================================= */

/*
 * The class/organization compatibility table.  If the desired class
 * and the given file organization appear together in a particular format's
 * compat table, then the format supports that combination.
 */
typedef struct _CO_Compat
{
	DataOrganization	c_org;
	DataClass		c_class;
} CO_Compat;

/*
 * The DataFormat description structure and method table.  Each format
 * defines its own structure and initializes it accordingly.  The DFA
 * interface refers to the specific format structures through public pointers.
 */
typedef struct _DataFormat
{
	char *f_name;			/* Name of this format		*/
	FileType f_ftype;		/* Our enumerated file type	*/
	/* 
	 * File name extension: Alternative extensions can be separated by
	 * a vertical bar, '|', with the first extension being the default
	 * when making file names.
	 */
	char *f_ext;
	CO_Compat *f_compat;		/* org-class compatibility table*/
	int f_ncompat;			/* number of org-class pairs	*/
	int f_of_size;			/* size of open file instance   */
	int f_readonly;			/* read-only format		*/

	/* Dynamic data members */
	int f_nopened;			/* Files opened in this format	*/
	int f_nfree;			/* Number in lookaside list	*/
	struct _OpenFile *f_of_free;	/* Lookaside list 		*/

	/* Class methods */
	M_QueryTime ((*f_QueryTime));	/* Query the times of a file	*/
	M_MakeFileName ((*f_MakeFileName));
					/* Make a new file name		*/

	/* Instance methods */
	M_Setup ((*f_Setup));		/* Set up access		*/
	M_OpenFile ((*f_OpenFile));	/* Open a file			*/
	M_CloseFile ((*f_CloseFile));	/* Close a file.		*/
	M_SyncFile ((*f_SyncFile));	/* Synchronize a file		*/
	M_GetData ((*f_GetData));	/* Get the data			*/
	M_GetAlts ((*f_GetAlts));	/* Get altitude info		*/
	M_DataTimes ((*f_DataTimes));	/* Get data times		*/
	M_GetForecastTimes ((*f_GetForecastTimes));
					/* Get forecast times (model)	*/
	M_CreateFile ((*f_CreateFile));	/* Create a new file		*/
	M_PutSample ((*f_PutSample));	/* Put data to a file		*/
	M_PutBlock ((*f_PutBlock));	/* Write a block to a file	*/
	M_GetObsSamples ((*f_GetObsSamples));
					/* Get observation samples	*/
	M_GetFields ((*f_GetFields));	/* Get fields			*/
	M_GetAttrs ((*f_GetAttrs));	/* Get attributes		*/
	M_GetTimes ((*f_GetTimes));	/* Return array of times	*/
        M_GetAssociatedFiles ((*f_GetAssociatedFiles));
                                        /* Get the associated files     */ 
} DataFormat;


/*
 * Define a macro for the initialization of the dynamic format members, in
 * case (when) they change.  Formats just need to include this macro in the
 * structure initialization.
 */
#define FORMAT_INIT \
	0,				/* Number opened		*/ \
	0,				/* Number avail in lookaside	*/ \
	NULL				/* Lookaside list		*/


/*
 * The OpenFile structure serves as the first part of the instance structure
 * for all formats.  It includes a pointer to the open file's format method
 * table (sort-of like a class structure) by which DFA functions can look
 * up a format's methods. 
 */
typedef struct _OpenFile
{
	int		of_lru;		/* Access count			*/
	DataFile	of_df;		/* DF structure			*/
	struct _OpenFile *of_next;	/* Next in chain		*/
	int		of_write;	/* File open for write access	*/
	DataFormat	*of_fmt;	/* Pointer to format class	*/
#ifdef notdef
	FileType of_dftype;		/* File type, shouldn't change	*/ 
	void	*of_tag;		/* Format-specific tag		*/
	ZebTime	*of_times;		/* Sample times			*/
	Location of_sloc;		/* Static location		*/
	int	of_nsample;		/* Number of samples		*/
	PlatformId *of_plats;		/* Platforms in the file	*/
	int	of_nplat;		/* Number of platforms		*/
#endif
} OpenFile;

#ifdef notdef
/*
 * Define a macro to access the only publicly useful OpenFile member
 */
# define fmt_FileIndex(ofp) (((OpenFile *)(ofp))->of_dfindex)
#endif

/*
 * Public prototypes for the DataFormat methods which can be 'inherited'
 * or called by other format methods.
 */
int dfa_TimeIndex FP ((OpenFile *ofp, const ZebraTime *when, int last));
M_DataTimes (fmt_DataTimes);
M_MakeFileName (fmt_MakeFileName);

#ifdef notdef
/*
 * Semi-private dfa routines, for accessing the OpenFile structure
 */
#define dfa_FileIndex(ofp) ((ofp)->of_dfindex)
#endif

OpenFile *dfa_OpenFile (const DataFile *df, int write);
ZebTime *dfa_GetTimes (OpenFile *ofp, int *ntime);
int dfa_SyncFile (OpenFile *ofp);
void dfa_ForceClose (const DataFile *df);

/*
 * Prototypes for non-compiled methods defined in DFA_None.c
 */
M_QueryTime (fmt_QueryNotCompiled);
M_OpenFile (fmt_OpenNotCompiled);
M_CreateFile (fmt_CreateNotCompiled);


# ifdef __cplusplus
}; // extern "C"
# endif


#endif /* ndef _zebra_dataformat_h_ */

