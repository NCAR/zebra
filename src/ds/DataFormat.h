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
 * $Id: DataFormat.h,v 3.1 1995-08-24 22:17:43 granger Exp $
 */
#ifndef _zeb_dataformat_h_
#define _zeb_dataformat_h_

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

#define P_QueryTime(fn) \
static int fn FP((char *file, ZebTime *begin, ZebTime *end, int *nsample))
/*
 *	Given the file name, return the begin and end times of the data
 *	found therein.  For a given format, it is acceptible to calculate
 *	these times from the file name, if it is certain that (1) the time
 *	period covered by the file is not greater than what is returned, 
 *	and (2) if the time period is less, no other file will cover the
 *	missing period.  NSAMPLE should be set to the number of data
 *	samples found in this file.  Returns TRUE on success.
 */

#define P_MakeFileName(fn) \
static int fn FP((DataFormat *fmt, char *platform, ZebTime *time, \
		  char *dest, dsDetail *details, int ndetail))
/*
 *	Create an appropriate name in dest for a new file for platform, 
 * 	starting at the given time.
 */

/* -----------------------------------------------------------------------
 * 'Instance' methods, which operate with an open file structure.  (Not
 * necessarily an open file yet or initialized structure.)
 */

#define P_CreateFile(fn) \
static int fn FP((OpenFile *ofp, char *fname, DataFile *dfile, \
		  DataChunk *dc, dsDetail *details, int ndetail))
/*
 *	Cause the given file to exist, returning TRUE if
 *	successful.  DC describes the current data put request, which
 *	should describe the layout of the file.  OFP points to the open
 *	file instance structure whose format-specific part the format
 *	method should fill in.
 */

#define P_OpenFile(fn) \
static int fn FP((OpenFile *of, char *filepath, DataFile *dp, int write))
/*
 *	Open the given filepath, returning TRUE if success.  The OpenFile
 *	structure will be of the correct format instance size; it is up
 *	to the format to fill it in.
 */

#define P_CloseFile(fn) \
static void fn FP((OpenFile *of))
/*
 *	Close this file.  The format should not free its tag, only any
 *	dynamic memory to which its tag members may point.
 */

#define P_SyncFile(fn) \
static int fn FP((OpenFile *of))
/*
 *	Synchronize this file to catch up with updates which have occurred.
 *	Returns TRUE on success.  If this routine is missing, it is assumed
 *	that updates are automatically available and returns true.
 */

#define P_Setup(fn) \
static DataChunk *fn FP((OpenFile *of, FieldId *fields, \
			 int nfield, DataClass dclass))
/*
 *	Get ready to do this data access.  The format driver should, at
 *	a minimum, create and return a data chunk with the appropriate
 *	class.  The get list is provided to aid setup for efficiency.
 *	Before being called, the file will have been opened (obviously)
 *	and the requested class checked against the formats class-org
 *	compatibility table.  
 *
 * 	The Setup method should accept NULL fields and zero nfield,
 *	especially in the case of location and other classes which have
 *	no fields.
 */

#define P_GetData(fn) \
static int fn FP((OpenFile *of, DataChunk *dc, int begin, int nsample, \
		  dsDetail *details, int ndetail))
/*
 *	Actually get the data from the given series of samples and append
 *	the sample data to the datachunk.
 */

/*
 * int
 * f_InqNPlat (dfindex
 * f_InqNPlat (tag)
 *
 *	Return the number of platforms contained here.  The DataFormat
 *	default method assumes support for only a single platform.
^^^^^^^^^^no longer used, remove?
 */

/*
 * f_GetIRGLoc (dfindex, locs)
 * int dfindex
 * Location *locs;
 *
 *	Copy over the locations for this IRGrid data.
^^^^^^^^^^^^^^^^^^ Remove?  no longer used.
 */


#define P_GetAlts(fn) \
static int fn FP((OpenFile *ofp, FieldId fid, int offset, float *alts, \
		  int *nalts, AltUnitType *altunits))
/*
 *	Return the vertical levels associated with the given field and forecast
 *	offset time from the given file.  Return value is non-zero on success.
 *	The default method returns FALSE.  Altitudes are copied into the alts
 *	array without regard for how many altitudes alts points to.
 */

#define P_GetForecastTimes(fn) \
static int fn FP((OpenFile *of, int *times, int *ntimes))
/*
 *	Return the available model forecast offset times in the given file.
 */

#define P_DataTimes(fn) \
static int fn FP((OpenFile *of, ZebTime *time, TimeSpec which, int n, \
		  ZebTime *dest))
/*
 * 	Return a list of times for which data is available.  The return
 *	value holds the number of times copied into 'dest'.
 */

#define P_PutSample(fn) \
static int fn FP((OpenFile *of, DataChunk *dc, int sample, WriteCode wc, \
		  dsDetail *details, int ndetail))
/*
 *	Write the given sample from the DC into the indicated file, as
 *	controlled by the write code:
 *		wc_Append	Append the sample to the file
 *		wc_Insert	Insert before an existing sample.
 *		wc_OverWrite	Overwrite an existing sample.
 *	Returns true iff the data write was successful.
 */

#define P_PutBlock(fn) \
static int fn FP((OpenFile *of, DataChunk *dc, int sample, int nsample, \
		  WriteCode wc, dsDetail *details, int ndetail))
/*
 * 	Write a block of 'nsample' samples, beginning at 'sample'
 *	in the data chunk, using write code 'wc', into the file
 * 	'dfile'.  A block is a contiguous series of samples
 *	to be written to the same file.
 */

#define P_GetObsSamples(fn) \
static int fn FP((OpenFile *of, ZebTime *times, Location *locs, int max))
/*
 *	Return the time and location for each of the samples in this
 *	observation; return value is the number actually returned.
 */

#define P_GetFields(fn) \
static int fn FP((OpenFile *of, int sample, int *nfld, FieldId *flist))
/*
 *	Return a list of available fields in this platform.  "NFLD" starts
 * 	as the max the caller can accept; should be returned as the number
 *	of fields actually returned.  The return value is non-zero if the
 *	call succeeds.
 */

#define P_GetAttrs(fn) \
static char * fn FP((OpenFile *of, int sample, int *len))
/*
 *	Get the attributes from the file for this sample.  Returns a
 *	piece of dynamically allocated memory (whose length comes back
 *	in *len) containing the attribute table.  Return NULL if it
 *	can't be done.
 */

/* ---------------------------------------------------------------------
 * Recent additions:
 */

#define P_GetTimes(fn) \
static ZebTime * fn FP((OpenFile *of, int *ntime))
/*
 * 	Return an array of sample times in the file.  The array should not
 *	be freed or modified by the caller.  The sample times will be in
 *	chronological order, but there may be duplicates.
 */

/* int
 * f_TimeIndex (OpenFile *of, ZebTime *time, int last)
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
	 * a vertical bar, '|', with the first being the default when making
	 * file names
	 */
	char *f_ext;
	CO_Compat *f_compat;		/* org-class compatibility table*/
	int f_ncompat;			/* number of org-class pairs	*/
	int f_of_size;			/* size of open file instance   */
	int f_readonly;			/* read-only format		*/

	/* Class methods */
	int (*f_QueryTime)();		/* Query the times of a file	*/
	void (*f_MakeFileName) ();	/* Make a new file name		*/

	/* Instance methods */
	DataChunk *(*f_Setup) ();	/* Set up access		*/
	int (*f_OpenFile) ();		/* Open a file			*/
	void (*f_CloseFile) ();		/* Close a file.		*/
	int (*f_SyncFile) ();		/* Synchronize a file		*/
	int (*f_InqNPlat) ();		/* Inquire number platforms	*/
	int (*f_GetData) ();		/* Get the data			*/
	int (*f_GetIRGLoc) ();		/* Get irgrid locations		*/
	int (*f_GetAlts) ();		/* Get altitude info		*/
	int (*f_DataTimes) ();		/* Get data times		*/
	int (*f_GetForecastTimes) ();	/* Get forecast times (model)	*/
	int (*f_CreateFile) ();		/* Create a new file		*/
	int (*f_PutSample) ();		/* Put data to a file		*/
	int (*f_PutBlock) ();		/* Write a block to a file	*/
	int (*f_GetObsSamples) ();	/* Get observation samples	*/
	int (*f_GetFields) ();		/* Get fields			*/
	char *(*f_GetAttrs) ();		/* Get attributes		*/

	ZebTime *(*f_GetTimes) ();	/* Return array of times	*/

} DataFormat;


/*
 * The OpenFile structure serves as the first part of the instance structure
 * for all formats.  It includes a pointer to the open file's format method
 * table (sort-of like a class structure) by which DFA functions can look
 * up a format's methods. 
 */
typedef struct _OpenFile
{
	int	of_lru;			/* Access count			*/
	int	of_dfindex;		/* DF structure index		*/
	struct _OpenFile *of_next;	/* Next in chain		*/
	int	of_write;		/* File open for write access	*/
	long	of_dfrev;		/* Revision we're sync'd to	*/
	DataFormat *of_fmt;		/* Pointer to format class	*/
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


/*
 * Declarations for pointers to each format's DataFormat structure.
 */
extern DataFormat *netcdfFormat;
extern DataFormat *boundaryFormat;
extern DataFormat *rasterFormat;
extern DataFormat *cmpRasterFormat;
extern DataFormat *zebraFormat;
extern DataFormat *gribFormat;
extern DataFormat *gribSfcFormat;
extern DataFormat *gradsFormat;

/*
 * Public prototypes for the DataFormat methods which can be 'inherited'
 * or called by other format methods.
 */
int dfa_TimeIndex FP ((OpenFile *ofp, ZebTime *when, int last));

int fmt_DataTimes FP ((OpenFile *ofp, ZebTime *when, TimeSpec which,
		       int n, ZebTime *start));
void fmt_MakeFileName FP ((DataFormat *fmt, ClientPlatform *plat, 
			   ZebTime *t, char *dest, dsDetail *details,
			   int ndetail));

/*
 * Semi-private dfa routines, for accessing the OpenFile structure
 */
static inline int dfa_FileIndex (ofp) OpenFile *ofp;
{ return (ofp->of_dfindex); }


#endif /* ndef _zeb_dataformat_h_ */

