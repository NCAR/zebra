/*
 * The data file access package, for use from deep within the data store.
 *
 * DFA itself is not much more than a minimal wrapper which makes the rest
 * of the data store be format-independent.  We push most of the real work
 * down to the format-specific stuff.
 */
static char *rcsid = "$Id: DataFileAccess.c,v 1.2 1991-01-16 22:06:46 corbet Exp $";


# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"



/*
 * This is the structure which describes a format.
 */
/*
 * The DFA format-driver routines:
 *
 * f_QueryTime (file, begin, end)
 * char *file;
 * time *begin, *end;
 *
 *	Given the file name, return the begin and end times of the data
 *	found therein.  For a given format, it is acceptible to calculate
 *	these times from the file name, if it is certain that (1) the time
 *	period covered by the file is not greater than what is returned, 
 *	and (2) if the time period is less, no other file will cover the
 *	missing period.  Returns TRUE on success.
 *
 * f_Setup (dlist)
 * GetList *dlist;
 *
 *	Get set up to do a data access.  Modifies the AccessList as needed --
 *	in particular, sets the sample count for each entry.  Should cache
 *	info wherever necessary to make the actual grab go faster.
 *
 * f_OpenFile (dp, tag)
 * DataFile *dp;
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
 * f_GetData (getlist)
 * GetList *getlist;
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
 * f_DataTimes (index, time, which, n, dest)
 * int index, n;
 * time *time, *dest;
 * TimeSpec which;
 *
 * 	Return a list of times for which data is available.
 *
 */

struct DataFormat
{
	char *f_name;			/* Name of this format		*/
	char *f_ext;			/* File name extension		*/
	/* Functions below here */
	int (*f_QueryTime)();		/* Query the times of a file	*/
	int (*f_Setup) ();		/* Set up access		*/
	int (*f_OpenFile) ();		/* Open a file			*/
	int (*f_CloseFile) ();		/* Close a file.		*/
	int (*f_SyncFile) ();		/* Synchronize a file		*/
	int (*f_InqNPlat) ();		/* Inquire number platforms	*/
	int (*f_GetData) ();		/* Get the data			*/
	int (*f_GetIRGLoc) ();		/* Get irgrid locations		*/
	int (*f_InqRGrid) ();		/* Get RGrid info		*/
	int (*f_DataTimes) ();		/* Get data times		*/
};


/*
 * Function definitions for the format table.
 */
extern int dnc_QueryTime (), dnc_OpenFile (), dnc_CloseFile ();
extern int dnc_SyncFile (), dnc_Setup (), dnc_InqPlat (), dnc_GetData ();
extern int dnc_GetIRGLoc (), dnc_GetRGrid (), dnc_DataTimes ();
# define ___ 0

/*
 * And here is the format table.
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
	int	of_format;		/* Format type			*/
	struct _OpenFile *of_next;	/* Next in chain		*/
	int	of_rev;			/* Revision count		*/
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





int
dfa_QueryDate (type, name, begin, end)
int type;
char *name;
time *begin, *end;
/*
 * Query the dates on this file.
 */
{
	return ((*Formats[type].f_QueryTime) (name, begin, end));
}





void
dfa_Setup (gl)
GetList *gl;
/*
 * Set up to grab the data described by this GetList entry.
 */
{
	DataFile *dp = DFTable + gl->gl_dfindex;
	
	if (dp->df_use != gl->gl_dfuse)
		msg_ELog (EF_INFO, "File '%s' use change: %d %d", dp->df_name,
			gl->gl_dfuse, dp->df_use);
	else
		(*Formats[dp->df_ftype].f_Setup) (gl);
}






void
dfa_GetData (gl)
GetList *gl;
/*
 * Get the data from this getlist entry.
 */
{
	DataFile *dp = DFTable + gl->gl_dfindex;
/*
 * Do the snarf.
 */
	(*Formats[dp->df_ftype].f_GetData) (gl);
/*
 * For some orgs, get the location info.
 */
	if (! gl->gl_next)	/* Kludge: last one only */
		switch (gl->gl_dobj->do_org)
		{
		   case OrgIRGrid:
		   	(*Formats[dp->df_ftype].f_GetIRGLoc) (gl->gl_dfindex,
				gl->gl_dobj->do_desc.d_irgrid.ir_loc);
		}
}






int
dfa_InqNPlat (index)
int index;
/*
 * Find out how many platforms are here.
 */
{
	DataFile *dp = DFTable + index;

	if (Formats[dp->df_ftype].f_InqNPlat)
		return ((*Formats[dp->df_ftype].f_InqNPlat) (index));
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
	DataFile *dp = DFTable + index;

	if (Formats[dp->df_ftype].f_InqRGrid)
		return ((*Formats[dp->df_ftype].f_InqRGrid)
					(index, origin, rg));
	return (FALSE);
}




int
dfa_DataTimes (index, when, which, n, dest)
int index, n;
time *when, *dest;
TimeSpec which;
{
	DataFile *dp = DFTable + index;
/*
 * Get available data times.
 */
	if (Formats[dp->df_ftype].f_DataTimes)
		return ((*Formats[dp->df_ftype].f_DataTimes) (index, when,
					which, n, dest));
	else
		return (0);
}






/*********
 * DFA private routines below here.
 *********/





void
dfa_AddOpenFile (dfindex, tag)
int dfindex;
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
	ofp->of_format = DFTable[dfindex].df_ftype;
	ofp->of_lru = OF_Lru++;
	ofp->of_tag = tag;
	ofp->of_next = OpenFiles;
	ofp->of_rev = DFTable[dfindex].df_rev;
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
				victim, DFTable[victim->of_dfindex].df_name);
			return;
		}
		prev->of_next = victim->of_next;
	}
/*
 * Get the actual file closed.
 */
	(*Formats[victim->of_format].f_CloseFile) (victim->of_tag);
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





int
dfa_OpenFile (dfindex, tag)
int dfindex;
void **tag;
/*
 * See to it that this file is open.  On success, the return value is TRUE,
 * and TAG has the tag value.
 */
{
	DataFile *dp = DFTable + dfindex;
	OpenFile *ofp;
	int retv = TRUE;

	dsm_ShmLock ();
/*
 * If the file is open, check the revision and return the tag.
 */
	if (ofp = dfa_FileIsOpen (dfindex, tag))
	{
		*tag = ofp->of_tag;
		if (dp->df_rev != ofp->of_rev)
			retv = (*Formats[dp->df_ftype].f_SyncFile) (*tag);
	}
/*
 * Nope, open it now.
 */
	else if (! (*Formats[dp->df_ftype].f_OpenFile) (dp->df_name, tag))
		retv = FALSE;
	else 	/* success */
		dfa_AddOpenFile (dfindex, *tag);

	dsm_ShmUnlock ();
	return (retv);
}
