/*
 * Deal with Boundary-format files.
 */
static char *rcsid = "$Id: DFA_Boundary.c,v 1.2 1991-02-26 19:00:51 corbet Exp $";

# include <sys/types.h>
# include <errno.h>
# include <fcntl.h>
# include <unistd.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "dfa.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "BoundaryFile.h"



/*
 * Our tag structure for open files.
 */
typedef struct s_BFTag
{
	int		bt_fd;		/* File descriptor		*/
	struct BFHeader	bt_hdr;		/* The file header		*/
	struct BFBTable *bt_BTable;	/* The boundary table		*/
} BFTag;




/*
 * Local routines.
 */
# ifdef __STDC__
	static void bf_Sync (BFTag *);
	static void bf_WriteBoundary (BFTag *, Location *, time *, int);
	static int bf_TimeIndex (BFTag *, time *);
# else
	static void bf_Sync ();
	static void bf_WriteBoundary ();
	static int bf_TimeIndex ();
# endif



int
bf_QueryTime (file, begin, end, nsample)
char *file;
time *begin, *end;
int *nsample;
/*
 * Tell the daemon what's in this file.
 */
{
	int fd;
	struct BFHeader hdr;
/*
 * Try to open the file.
 */
	if ((fd = open (file, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening '%s'", errno, file);
		return (FALSE);
	}
/*
 * Read the header.
 */
	if (read (fd, &hdr, sizeof (hdr)) < sizeof (hdr))
	{
		msg_ELog (EF_PROBLEM, "Header read error on %s", file);
		close (fd);
		return (FALSE);
	}
/*
 * Pull out the info and return.
 */
	*begin = hdr.bh_Begin;
	*end = hdr.bh_End;
	*nsample = hdr.bh_NBoundary;
	close (fd);
	return (TRUE);
}





void
bf_MakeFileName (dir, name, t, string)
time *t;
char *dir, *name, *string;
/*
 * Generate a new file name.
 */
{
	sprintf (string, "%s/%s.%06d.%04d.bf", dir, name, t->ds_yymmdd,
		t->ds_hhmmss/100);
}





int
bf_CreateFile (dfile, dobj, rtag)
DataFile *dfile;
DataObject *dobj;
char **rtag;
/*
 * Create a new boundary file.
 */
{
	BFTag *tag = ALLOC (BFTag);
/*
 * Start by trying to create the file.
 */
	if ((tag->bt_fd = open (dfile->df_name, O_RDWR | O_CREAT, 0664)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening '%s'", errno);
		free (tag);
		return (FALSE);
	}
/*
 * Fill in the header.
 */
	tag->bt_hdr.bh_Magic = BH_MAGIC;
	strcpy (tag->bt_hdr.bh_Platform, PTable[dobj->do_id].dp_name);
	tag->bt_hdr.bh_MaxBoundary = PTable[dobj->do_id].dp_maxsamp;
	tag->bt_hdr.bh_Begin = dobj->do_begin;
	tag->bt_hdr.bh_End = dobj->do_end;
	tag->bt_hdr.bh_NBoundary = 0;
/*
 * Allocate the boundary table.
 */
	tag->bt_BTable = (struct BFBTable *)
		malloc (tag->bt_hdr.bh_MaxBoundary * sizeof (struct BFBTable));
/*
 * Now synchronize the whole thing and return.
 */
	bf_Sync (tag);
	*rtag = (char *) tag;
	return (TRUE);
}






static void
bf_Sync (tag)
BFTag *tag;
/*
 * Write out changes to the header info.
 */
{
	lseek (tag->bt_fd, 0, SEEK_SET);
	write (tag->bt_fd, &tag->bt_hdr, sizeof (struct BFHeader));
	write (tag->bt_fd, tag->bt_BTable,
			tag->bt_hdr.bh_MaxBoundary*sizeof (struct BFBTable));
}





int
bf_PutData (dfile, dobj, begin, end)
int dfile, begin, end;
DataObject *dobj;
/*
 * Put data into this file.
 */
{
	BFTag *tag;
	struct BFHeader *hdr;
	int sample, offset;
/*
 * Open up the file.
 */
	if (! dfa_OpenFile (dfile, TRUE, (void *) &tag))
		return (FALSE);
	hdr = &tag->bt_hdr;
/*
 * Sanity checking.
 */
	if ((hdr->bh_NBoundary + end - begin + 1) > hdr->bh_MaxBoundary)
	{
		msg_ELog (EF_PROBLEM, "Too many samples in df %d", dfile);
		end = begin + (hdr->bh_MaxBoundary - hdr->bh_NBoundary) - 1;
	}
/*
 * Position to the first sample that we are writing.
 */
	offset = 0;
	for (sample = 0; sample < begin; sample++)
		offset += dobj->do_desc.d_length[sample];
/*
 * Write each sample.
 */
	for (; sample <= end; sample++)
	{
		bf_WriteBoundary (tag, dobj->do_aloc + offset, 
			dobj->do_times + offset,
			dobj->do_desc.d_length[sample]);
		offset += dobj->do_desc.d_length[sample];
	}
/*
 * Synchronize and we're done.
 */
	bf_Sync (tag);
	return (TRUE);
}





static void
bf_WriteBoundary (tag, loc, t, len)
BFTag *tag;
Location *loc;
time *t;
int len;
/*
 * Write out a new boundary.
 */
{
	int sample = tag->bt_hdr.bh_NBoundary++;
	struct BFBTable *bt = tag->bt_BTable + sample;
/*
 * Move to the end of the file and do the write.
 */
	bt->bt_Offset = lseek (tag->bt_fd, 0, SEEK_END);
	write (tag->bt_fd, loc, len * sizeof (Location));
/*
 * Update the housekeeping, and we're done.
 */
	bt->bt_NPoint = len;
	bt->bt_Time = *t;
	if (DLT (tag->bt_hdr.bh_End, *t))
		tag->bt_hdr.bh_End = *t;
}





int
bf_OpenFile (dp, write, rtag)
DataFile *dp;
bool write;
char **rtag;
/*
 * Open this file and return a tag.
 */
{
	BFTag *tag = ALLOC (BFTag);
	int btlen;
/*
 * See if we can really open the file.
 */
	if ((tag->bt_fd = open (dp->df_name, write ? O_RDWR : O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s",errno,dp->df_name);
		free (tag);
		return (FALSE);
	}
/*
 * Pull in the header info.
 */
	if (read (tag->bt_fd, &tag->bt_hdr, sizeof (struct BFHeader)) <
			sizeof (struct BFHeader))
	{
		msg_ELog (EF_PROBLEM, "Error %d reading BF hdr on %s", errno,
			dp->df_name);
		close (tag->bt_fd);
		free (tag);
		return (FALSE);
	}
	if (tag->bt_hdr.bh_Magic != BH_MAGIC)
		msg_ELog (EF_PROBLEM, "Bad magic (%x) on %s",
			tag->bt_hdr.bh_Magic, dp->df_name);
/*
 * Pull in the boundary table.
 */
	btlen = tag->bt_hdr.bh_MaxBoundary * sizeof (struct BFBTable);
	tag->bt_BTable = (struct BFBTable *) malloc (btlen);
	read (tag->bt_fd, tag->bt_BTable, btlen);

	*rtag = (char *) tag;
	return (TRUE);
}




void
bf_CloseFile (tag)
BFTag *tag;
/*
 * Close this file.
 */
{
	close (tag->bt_fd);
	free (tag->bt_BTable);
	free (tag);
}




int
bf_SyncFile (tag)
BFTag *tag;
/*
 * Catch up with changes in this file.
 */
{
	lseek (tag->bt_fd, 0, SEEK_SET);
	read (tag->bt_fd, &tag->bt_hdr, sizeof (tag->bt_hdr));
	read (tag->bt_fd, tag->bt_BTable,
		tag->bt_hdr.bh_MaxBoundary*sizeof (struct BFBTable));
	return (TRUE);
}




int
bf_Setup (gp)
GetList *gp;
/*
 * Get set up to do this data grab.
 */
{
	BFTag *tag;
	int tbegin, tend, sample;
/*
 * Open this file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Find the offsets for the desired times.
 */
	tbegin = bf_TimeIndex (tag, &gp->gl_begin);
	tend = bf_TimeIndex (tag, &gp->gl_end);
/*
 * Fill in the info.
 */
	gp->gl_nsample = 0;
	for (sample = tbegin; sample <= tend; sample++)
		gp->gl_nsample += tag->bt_BTable[sample].bt_NPoint;
	gp->gl_npoint = gp->gl_nsample;
	return (TRUE);
}




int
bf_GetData (gp)
GetList *gp;
/*
 * Get the data from this GetList entry.
 */
{
	BFTag *tag;
	int tbegin, tend, sample, pt;
	Location *lp = gp->gl_locs;
	time *tp = gp->gl_time;
/*
 * Open this file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Find the offsets for the desired times.
 */
	tbegin = bf_TimeIndex (tag, &gp->gl_begin);
	tend = bf_TimeIndex (tag, &gp->gl_end);
/*
 * Get the information.
 */
	gp->gl_nsample = 0;
	for (sample = tbegin; sample <= tend; sample++)
	{
		struct BFBTable *bt = tag->bt_BTable + sample;
	/*
	 * Position to the right spot in the file, and grab out the
	 * location info.
	 */
		lseek (tag->bt_fd, bt->bt_Offset, SEEK_SET);
		read (tag->bt_fd, lp, bt->bt_NPoint * sizeof (Location));
		lp += bt->bt_NPoint;
	/*
	 * Stuff in the appropriate number of times.
	 */
		for (pt = 0; pt < bt->bt_NPoint; pt++)
			*tp++ = bt->bt_Time;
	}
	return (TRUE);
}





static int
bf_TimeIndex (tag, t)
BFTag *tag;
time *t;
/*
 * Find the offset into this file for the first entry before or equal to
 * the given time.
 */
{
	int offset;
	struct BFBTable *bt = tag->bt_BTable;
/*
 * Just search back from the end until we find the first entry before the
 * one we went.  There will generally not be all that many boundaries in
 * a given file, so we can get away with a dumb sort here.  Famous last 
 * words if I ever heard them.
 */
	for (offset = tag->bt_hdr.bh_NBoundary - 1; offset >= 0; offset--)
		if (DLE (bt[offset].bt_Time, *t))
			return (offset);
	return (0);
}




int
bf_DataTimes (dfindex, t, which, n, dest)
int dfindex, n;
time *t, *dest;
TimeSpec *which;
/*
 * Return the times for which data is available.
 */
{
	BFTag *tag;
	int toff, i;
/*
 * Open this file.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (0);
/*
 * Find the offset to the time, and copy out info.
 */
	toff = bf_TimeIndex (tag, t);
	for (i = 0; toff >= 0 && i < n; i++)
		*dest++ = tag->bt_BTable[toff--].bt_Time;
	return (i);
}
