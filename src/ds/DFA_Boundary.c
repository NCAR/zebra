/*
 * Deal with Boundary-format files.
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
MAKE_RCSID ("$Id: DFA_Boundary.c,v 3.2 1992-08-10 17:30:54 corbet Exp $")



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
static void 	bf_Sync FP ((BFTag *));
static void 	bf_WriteBoundary FP ((BFTag *, int, Location *,ZebTime *,int));
static int 	bf_TimeIndex FP ((BFTag *, ZebTime *));



/*
 * The biggest boundary we expect to deal with.
 */
# define MAX_BOUNDARY	200	/* Should really ought to do it	*/




int
bf_QueryTime (file, begin, end, nsample)
char *file;
ZebTime *begin, *end;
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
# ifdef ancient_stuff
	*begin = hdr.bh_Begin;
	*end = hdr.bh_End;
# endif
	TC_UIToZt (&hdr.bh_Begin, begin);
	TC_UIToZt (&hdr.bh_End, end);
	*nsample = hdr.bh_NBoundary;
	close (fd);
	return (TRUE);
}





void
bf_MakeFileName (dir, name, zt, string)
ZebTime *zt;
char *dir, *name, *string;
/*
 * Generate a new file name.
 */
{
	time t;
	
	TC_ZtToUI (zt, &t);
	sprintf (string, "%s.%06d.%04d.bf", name, t.ds_yymmdd,
		t.ds_hhmmss/100);
}





int
bf_CreateFile (fname, dfile, dc, rtag)
char *fname;
DataFile *dfile;
DataChunk *dc;
char **rtag;
/*
 * Create a new boundary file.
 */
{
	BFTag *tag = ALLOC (BFTag);
	PlatformId id = dc->dc_Platform;
/*
 * Start by trying to create the file.
 */
	if ((tag->bt_fd = open (fname, O_RDWR | O_CREAT, 0664)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening '%s'", errno,
			fname);
		free (tag);
		return (FALSE);
	}
/*
 * Fill in the header.
 */
	tag->bt_hdr.bh_Magic = BH_MAGIC;
	strcpy (tag->bt_hdr.bh_Platform, PTable[id].dp_name);
	tag->bt_hdr.bh_MaxBoundary = PTable[id].dp_maxsamp;
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
bf_PutSample (dfile, dc, sample, wc)
int dfile, sample;
DataChunk *dc;
WriteCode wc;
/*
 * Put data into this file.
 */
{
	BFTag *tag;
	struct BFHeader *hdr;
	int offset, npt, i;
	ZebTime t;
	Location *locs;
/*
 * Open up the file.
 */
	if (! dfa_OpenFile (dfile, TRUE, (void *) &tag))
		return (FALSE);
	hdr = &tag->bt_hdr;
/*
 * Get the info on this boundary.
 */
	dc_GetTime (dc, sample, &t);
	locs = dc_BndGet (dc, sample, &npt);
/*
 * Sanity checking.
 */
	if (hdr->bh_NBoundary >= hdr->bh_MaxBoundary && wc != wc_Overwrite)
	{
		msg_ELog (EF_PROBLEM, "Too many samples in df %d", dfile);
		return (FALSE);
	}
/*
 * Figure out where this sample is to go.
 */
	switch (wc)
	{
	/*
	 * Appends are easy.
	 */
	   case wc_Append:
		offset = hdr->bh_NBoundary++;
		break;
	/*
	 * For an insert we need to open up a hole in the toc.
	 */
	   case wc_Insert:
	   	offset = bf_TimeIndex (tag, &t);
		for (i = hdr->bh_NBoundary - 1; i > offset; i--)
			tag->bt_BTable[i + 1] = tag->bt_BTable[i];
		offset++;
		hdr->bh_NBoundary++;
		break;
	/*
	 * For overwrites we find the unluck sample and we are done.
	 */
	   case wc_Overwrite:
	   	offset = bf_TimeIndex (tag, &t);
		if (offset < 0)
			offset = 0;
		break;
	}
/*
 * Write the boundary.
 */
	bf_WriteBoundary (tag, offset, locs, &t, npt);
/*
 * Synchronize and we're done.
 */
	bf_Sync (tag);
	return (TRUE);
}





static void
bf_WriteBoundary (tag, sample, loc, t, len)
BFTag *tag;
Location *loc;
ZebTime *t;
int len, sample;
/*
 * Write out a new boundary.
 */
{
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
	TC_ZtToUI (t, &bt->bt_Time);
	/* bt->bt_Time = *t; */
/*
 * Tweak times.
 */
	if (sample == 0)
		tag->bt_hdr.bh_End = tag->bt_hdr.bh_Begin = bt->bt_Time;
	else if (DLT (tag->bt_hdr.bh_End, bt->bt_Time))
		tag->bt_hdr.bh_End = bt->bt_Time;
}





int
bf_OpenFile (fname, dp, write, rtag)
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
	if ((tag->bt_fd = open (fname, write ? O_RDWR : O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s",errno,fname);
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
			fname);
		close (tag->bt_fd);
		free (tag);
		return (FALSE);
	}
	if (tag->bt_hdr.bh_Magic != BH_MAGIC)
		msg_ELog (EF_PROBLEM, "Bad magic (%x) on %s",
			tag->bt_hdr.bh_Magic, fname);
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




DataChunk *
bf_Setup (gp, fields, nfield, class)
GetList *gp;
char **fields;
int nfield;
DataClass class;
/*
 * Get set up to do this data grab.
 */
{
	BFTag *tag;
	int tbegin, tend, sample;
/*
 * Do some sanity checking.
 */
	if (class != DCC_Boundary)
	{
		msg_ELog (EF_PROBLEM, "Non-boundary fetch from boundary file");
		return (NULL);
	}
	if (nfield > 0)
		msg_ELog (EF_PROBLEM, "Fields in a boundary get?");
/*
 * Open this file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (NULL);
/*
 * Simply create and return a data chunk.
 */
	return (dc_CreateDC (DCC_Boundary));
}




int
bf_GetData (dc, gp)
DataChunk *dc;
GetList *gp;
/*
 * Get the data from this GetList entry.
 */
{
	BFTag *tag;
	int tbegin, tend, sample, pt;
	Location locs[MAX_BOUNDARY];
	ZebTime zt;
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
	for (sample = tbegin; sample <= tend; sample++)
	{
		struct BFBTable *bt = tag->bt_BTable + sample;
	/*
	 * Position to the right spot in the file, and grab out the
	 * location info.
	 */
		lseek (tag->bt_fd, bt->bt_Offset, SEEK_SET);
		read (tag->bt_fd, locs, bt->bt_NPoint * sizeof (Location));
	/*
	 * Add it to the data chunk.
	 */
		TC_UIToZt (&bt->bt_Time, &zt);
	 	dc_BndAdd (dc, &zt, dc->dc_Platform, locs, bt->bt_NPoint);
	}
	return (TRUE);
}





static int
bf_TimeIndex (tag, zt)
BFTag *tag;
ZebTime *zt;
/*
 * Find the offset into this file for the first entry before or equal to
 * the given time.
 */
{
	int offset;
	struct BFBTable *bt = tag->bt_BTable;
	time t;
/*
 * Just search back from the end until we find the first entry before the
 * one we went.  There will generally not be all that many boundaries in
 * a given file, so we can get away with a dumb search here.  Famous last 
 * words if I ever heard them.
 */
	TC_ZtToUI (zt, &t);
	for (offset = tag->bt_hdr.bh_NBoundary - 1; offset >= 0; offset--)
		if (DLE (bt[offset].bt_Time, t))
			return (offset);
	return (-1);
}




int
bf_DataTimes (dfindex, t, which, n, dest)
int dfindex, n;
ZebTime *t, *dest;
TimeSpec which;
/*
 * Return the times for which data is available.
 */
{
	BFTag *tag;
	struct BFBTable *bt;
	int toff, i;
/*
 * Open this file.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (0);
	bt = tag->bt_BTable;
/*
 * Find the offset to the time, and copy out info.
 */
	toff = bf_TimeIndex (tag, t);
	if (which == DsBefore)
		for (i = 0; toff >= 0 && i < n; i++)
			/* *dest++ = tag->bt_BTable[toff--].bt_Time; */
			TC_UIToZt (&tag->bt_BTable[toff--].bt_Time, dest++);
	else if (which == DsAfter)
	{
		toff++;
		for (i = 0; i < n && toff < tag->bt_hdr.bh_NBoundary; i++)
			/* *dest-- = bt[toff++].bt_Time; */
			TC_UIToZt (&bt[toff++].bt_Time, dest--);
	}
	return (i);
}




bf_GetFields (dfile, t, nfld, flist)
int dfile;
time *t;
int *nfld;
FieldId *flist;
/*
 * Return the field list.
 */
{
	*nfld = 0;
}
