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


# include <stdio.h>
# include <sys/types.h>
# include <errno.h>
# include <fcntl.h>
# include <string.h>
# include <unistd.h>

# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "BoundaryFile.h"
# include "DataFormat.h"

RCSID ("$Id: DFA_Boundary.c,v 3.19 2001-10-16 22:26:28 granger Exp $")


/*
 * Our tag structure for open files.
 */
typedef struct s_BFTag
{
    int bt_fd;			/* File descriptor		*/
    int bt_doswap;		/* swap byte endianness?	*/
    struct BFHeader bt_hdr;	/* The file header		*/
    struct BFBTable *bt_BTable;	/* The boundary table		*/
    ZebTime *bt_times;		/* Stash of ZebTime's		*/
} BFTag;


typedef struct _BoundaryOpenFile
{
	OpenFile	open_file;
	BFTag		bf_tag;
}
BoundaryOpenFile;

#define BF_TAGP(ofp)	(&((BoundaryOpenFile *)(ofp))->bf_tag)

/*
 * Boundary format methods
 */
P_OpenFile (bf_OpenFile);
P_CloseFile (bf_CloseFile);
P_SyncFile (bf_ReadSync);
P_QueryTime (bf_QueryTime);
P_PutSample (bf_PutSample);
P_CreateFile (bf_CreateFile);
P_GetData (bf_GetData);
P_GetFields (bf_GetFields);
P_Setup (bf_Setup);
P_GetTimes (bf_GetTimes);

static CO_Compat COCTable[] =
{
	{ OrgOutline,		DCC_Boundary	}
};

static DataFormat boundaryFormatRec =
{
	"Boundary",
	FTBoundary,
	".bf",
	COCTable,       		/* org/class compatibility table*/
	N_COC (COCTable),
	sizeof (BoundaryOpenFile),
	FALSE,				/* readonly */

	FORMAT_INIT,			/* dynamic members */

	bf_QueryTime,			/* Query times			*/
	fmt_MakeFileName,		/* Make file name		*/

	bf_Setup,			/* setup			*/
	bf_OpenFile,			/* Open				*/
	bf_CloseFile,			/* Close			*/
	bf_ReadSync,			/* Synchronize			*/
	bf_GetData,			/* Get the data			*/
	___,				/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	___,				/* Get forecast times		*/
	bf_CreateFile,			/* Create a new file		*/
	bf_PutSample,			/* Write to file		*/
	___,				/* Write block to a file	*/
	___,				/* Get observation samples	*/
	bf_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
	bf_GetTimes,			/* Get times			*/
	___                             /* Get the associated files     */
};


DataFormat *boundaryFormat = (DataFormat *) &boundaryFormatRec;


/*
 * Local routines.
 */
static void 	bf_WriteBoundary (BFTag *, int, Location *, ZebTime *, int);
static void	bf_SyncTimes (BFTag *tag);
static void	bf_WriteSync (OpenFile *ofp);
static int	bf_GetHeader (int fd, struct BFHeader* hdr, int* doswap);
static void	bf_GetTable (BFTag* tag);
static void	bf_SwapHeader (struct BFHeader* hdr);
static void	bf_SwapTable (struct BFBTable* tbl);
static void	bf_SwapLocation (struct Location* loc);
static void	bf_SwapFour (char* v);


/*
 * The biggest boundary we expect to deal with.
 */
# define MAX_BOUNDARY	200	/* Should really ought to do it	*/




static int
bf_QueryTime (const char *file, ZebraTime *begin, ZebraTime *end, int *nsample)
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
	if (! bf_GetHeader (fd, &hdr, 0))
	{
		msg_ELog (EF_PROBLEM, "Header read error on %s", file);
		close (fd);
		return (FALSE);
	}
/*
 * Pull out the info and return.  Normalize the dates, just to be sure.
 */
	TC_y2k (&hdr.bh_Begin);
	TC_UIToZt (&hdr.bh_Begin, begin);
	TC_y2k (&hdr.bh_End);
	TC_UIToZt (&hdr.bh_End, end);
	*nsample = hdr.bh_NBoundary;
	close (fd);
	return (TRUE);
}




static int
bf_CreateFile (ofp, dc, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
dsDetail *details;
int ndetail;
/*
 * Create a new boundary file.
 */
{
	BFTag *tag = BF_TAGP(ofp);
	PlatformId id = dc->dc_Platform;
	char *fname = ofp->of_df.df_fullname;
/*
 * Start by trying to create the file.
 */
	if ((tag->bt_fd = open (fname, O_RDWR | O_CREAT, 0664)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening '%s'", errno,
			fname);
		return (FALSE);
	}
/*
 * Fill in the header.
 */
	tag->bt_hdr.bh_Magic = BH_MAGIC;
	strcpy (tag->bt_hdr.bh_Platform, ds_PlatformName (id));
	tag->bt_hdr.bh_MaxBoundary = ds_MaxSamples (id);
	tag->bt_hdr.bh_NBoundary = 0;
/*
 * Allocate the boundary and times tables.
 */
	tag->bt_BTable = (struct BFBTable *)
		malloc (tag->bt_hdr.bh_MaxBoundary * sizeof (struct BFBTable));
	tag->bt_times = (ZebTime *) 
		malloc (tag->bt_hdr.bh_MaxBoundary * sizeof (ZebTime));
/*
 * Create the file using our native byte order; no swapping necessary.
 */
	tag->bt_doswap = 0;
/*
 * Now synchronize the whole thing and return.
 */
	bf_WriteSync (ofp);
	return (TRUE);
}






static void
bf_WriteSync (ofp)
OpenFile *ofp;
/*
 * Write out changes to the header info, including our internal array of
 * ZebTimes.
 */
{
    BFTag *tag = BF_TAGP(ofp);
    struct BFHeader *hdr;
    struct BFBTable *btable;
    int btlen = tag->bt_hdr.bh_MaxBoundary * sizeof (struct BFBTable);
/*
 * If the file does not have our endianness, make swapped copies of
 * the header and table to write out.  Otherwise use the header and
 * table direct from the tag.
 */
    if (tag->bt_doswap)
    {
	int b;
	
	hdr = (struct BFHeader *) malloc (sizeof (struct BFHeader));
	memcpy (hdr, &tag->bt_hdr, sizeof (struct BFHeader));
	bf_SwapHeader (hdr);

	btable = (struct BFBTable *) malloc (btlen);
	memcpy (btable, tag->bt_BTable, btlen);
	for (b = 0; b < tag->bt_hdr.bh_NBoundary; b++)
	    bf_SwapTable (btable + b);
    }
    else
    {
	hdr = &tag->bt_hdr;
	btable = tag->bt_BTable;
    }
/*
 * Write the header and table
 */
    lseek (tag->bt_fd, 0, SEEK_SET);
    write (tag->bt_fd, hdr, sizeof (struct BFHeader));
    write (tag->bt_fd, btable, btlen);

    bf_SyncTimes (tag);
/*
 * If we made swapped copies, free them now
 */
    if (tag->bt_doswap)
    {
	free (hdr);
	free (btable);
    }
}





static int
bf_PutSample (ofp, dc, sample, wc, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int sample;
WriteCode wc;
dsDetail *details;
int ndetail;
/*
 * Put data into this file.
 */
{
    BFTag *tag = BF_TAGP(ofp);
    struct BFHeader *hdr;
    int offset, npt, i;
    ZebTime t;
    Location *locs;

/*
 * Open up the file.
 */
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
	msg_ELog (EF_PROBLEM, "Too many samples in datafile");
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
	offset = dfa_TimeIndex (ofp, &t, 0);
	for (i = hdr->bh_NBoundary - 1; i > offset; i--)
	    tag->bt_BTable[i + 1] = tag->bt_BTable[i];
	offset++;
	hdr->bh_NBoundary++;
	break;
    /*
     * For overwrites we find the unluck sample and we are done.
     */
      case wc_Overwrite:
	offset = dfa_TimeIndex (ofp, &t, 0);
	if (offset < 0)
	    offset = 0;
	break;
    /*
     * Shouldn't happen, but just to satisfy compiler checks...
     */
      default:
	offset = hdr->bh_NBoundary++;
	break;
    }
/*
 * Write the boundary.
 */
    bf_WriteBoundary (tag, offset, locs, &t, npt);
/*
 * Synchronize and we're done.
 */
    bf_WriteSync (ofp);
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
    Location *wloc;
/*
 * If the file has different endianness, we copy and swap the given
 * locations before writing them.  Otherwise we can use them as-is.
 */
    if (tag->bt_doswap)
    {
	int l;
	
	wloc = (Location*) malloc (len * sizeof (Location));
	memcpy (wloc, loc, len * sizeof (Location));
	for (l = 0; l < len; l++)
	    bf_SwapLocation (wloc + l);
    }
    else
	wloc = loc;
/*
 * Move to the end of the file and do the write.
 */
    bt->bt_Offset = lseek (tag->bt_fd, 0, SEEK_END);
    write (tag->bt_fd, wloc, len * sizeof (Location));
/*
 * If we allocated space for swapped locations, free it
 */
    if (tag->bt_doswap)
	free (wloc);
/*
 * Update the housekeeping, and we're done.
 */
    bt->bt_NPoint = len;
    TC_ZtToUI (t, &bt->bt_Time);
    tag->bt_times[sample] = *t;
/*
 * Tweak times.
 */
    if (sample == 0)
	tag->bt_hdr.bh_End = tag->bt_hdr.bh_Begin = bt->bt_Time;
    else if (DLT (tag->bt_hdr.bh_End, bt->bt_Time))
	tag->bt_hdr.bh_End = bt->bt_Time;
}





static int
bf_OpenFile (ofp, write)
OpenFile *ofp;
zbool write;
/*
 * Open this file and return a tag.
 */
{
	BFTag *tag = BF_TAGP(ofp);
	int btlen;
	char *fname = ofp->of_df.df_fullname;
/*
 * See if we can really open the file.
 */
	if ((tag->bt_fd = open (fname, write ? O_RDWR : O_RDONLY)) < 0)
	{
	    msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, fname);
	    return (FALSE);
	}
/*
 * Pull in the header info, also stashing the hint about whether the
 * file is other-endian from our native byte ordering.
 */
	if (! bf_GetHeader (tag->bt_fd, &tag->bt_hdr, &tag->bt_doswap))
	{
	    msg_ELog (EF_PROBLEM, "Can't read header for boundary file %s",
		      fname);
	    close (tag->bt_fd);
	    return (FALSE);
	}
/*
 * Pull in the boundary table.
 */
	btlen = tag->bt_hdr.bh_MaxBoundary * sizeof (struct BFBTable);
	tag->bt_BTable = (struct BFBTable *) malloc (btlen);
	bf_GetTable (tag);
/*
 * We also keep a local ZebraTime array separate from the times in the 
 * table
 */
	tag->bt_times = (ZebTime *) 
		malloc (tag->bt_hdr.bh_MaxBoundary * sizeof (ZebTime));
	bf_SyncTimes (tag);
	return (TRUE);
}



static int
bf_GetHeader (int fd, struct BFHeader* hdr, int* doswap)
/*
 * Read the header into the given spot, swapping bytes if necessary and
 * setting doswap (if non-NULL) to tell whether swapping was performed.
 * The function returns true iff it gets a good header.
 */
{
    int revmagic;
    
    if (read (fd, hdr, sizeof (struct BFHeader)) < sizeof (struct BFHeader))
    {
	msg_ELog (EF_PROBLEM, "Error %d reading boundary file hdr", errno);
	return 0;
    }
/*
 * If the magic number is good, we have a good header and the file's 
 * endianness is the same as ours.
 */
    if (hdr->bh_Magic == BH_MAGIC)
    {
	if (doswap)
	    *doswap = 0;
	return 1;
    }
/*
 * If things look good with a swapped magic number, assume the file was 
 * written with endianness opposite from ours and swap everything in the
 * header.
 */ 
    revmagic = hdr->bh_Magic;
    bf_SwapFour ((char*) &revmagic);
    
    if (revmagic == BH_MAGIC)
    {
	if (doswap)
	    *doswap = 1;
	bf_SwapHeader (hdr);
	
	return 1;
    }
	
    msg_ELog (EF_PROBLEM, "Bad magic (%x) in boundary file", hdr->bh_Magic);
    return 0;
}




static void
bf_GetTable (BFTag* tag)
/*
 * Get the current table and stuff them into the tag
 */
{
    int btlen = tag->bt_hdr.bh_MaxBoundary * sizeof (struct BFBTable);
/*
 * The table and times are just past the header
 */
    lseek (tag->bt_fd, sizeof (struct BFHeader), SEEK_SET);
/*
 * Read the table and swap if necessary
 */
    read (tag->bt_fd, tag->bt_BTable, btlen);
    if (tag->bt_doswap)
    {
	int b;
	
	for (b = 0; b < tag->bt_hdr.bh_NBoundary; b++)
	    bf_SwapTable (tag->bt_BTable + b);
    }
}




static void
bf_CloseFile (ofp)
OpenFile *ofp;
/*
 * Close this file.
 */
{
	BFTag *tag = BF_TAGP(ofp);

	close (tag->bt_fd);
	free (tag->bt_BTable);
	free (tag->bt_times);
}




static int
bf_ReadSync (ofp)
OpenFile *ofp;
/*
 * Catch up with changes in this file.
 */
{
	BFTag *tag = BF_TAGP(ofp);

	lseek (tag->bt_fd, 0, SEEK_SET);
	bf_GetHeader (tag->bt_fd, &tag->bt_hdr, 0);
	bf_GetTable (tag);
	bf_SyncTimes (tag);
	return (TRUE);
}




static void
bf_SyncTimes (tag)
BFTag *tag;
/*
 * Sync the ZebTimes in the tag with the boundary table entries
 */
{
    int i;

    for (i = 0; i < tag->bt_hdr.bh_NBoundary; ++i)
    {
	TC_y2k (&tag->bt_BTable[i].bt_Time);
	TC_UIToZt (&tag->bt_BTable[i].bt_Time, tag->bt_times+i);
    }
}




/* ARGSUSED */
static DataChunk *
bf_Setup (ofp, fields, nfield, dclass)
OpenFile *ofp;
FieldId *fields;
int nfield;
DataClass dclass;
/*
 * Get set up to do this data grab.
 */
{
	if (nfield > 0)
		msg_ELog (EF_PROBLEM, "Fields in a boundary get?");
/*
 * Simply create and return a data chunk.
 */
	return (dc_CreateDC (DCC_Boundary));
}




static int
bf_GetData (ofp, dc, tbegin, nsample, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int tbegin, nsample;
dsDetail *details;
int ndetail;
/*
 * Get the data from these sample indices.
 */
{
	BFTag *tag = BF_TAGP(ofp);
	int tend, sample;
	Location locs[MAX_BOUNDARY];

	tend = tbegin + nsample - 1;
/*
 * Get the information.
 */
	dc_AddMoreSamples (dc, nsample, 0);
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
	 * Swap the location data if necessary
	 */
		if (tag->bt_doswap)
		{
		    int l;
		    for (l = 0; l < bt->bt_NPoint; l++)
			bf_SwapLocation (locs + l);
		}
	/*
	 * Add it to the data chunk.
	 */
	 	dc_BndAdd (dc, tag->bt_times + sample, dc->dc_Platform, 
			   locs, bt->bt_NPoint);
	}
	return (TRUE);
}




static ZebTime *
bf_GetTimes (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	BFTag *tag = BF_TAGP(ofp);

	*ntime = tag->bt_hdr.bh_NBoundary;
	return (tag->bt_times);
}




static int
bf_GetFields (ofp, sample, nfld, flist)
OpenFile *ofp;
int sample;
int *nfld;
FieldId *flist;
/*
 * Return the field list.
 */
{
	*nfld = 0;
	return (TRUE);
}



static void
bf_SwapHeader (struct BFHeader* hdr)
/*
 * Swap bytes as necessary to change the given header's endianness
 */
{
    bf_SwapFour ((char*) &hdr->bh_Magic);
    bf_SwapFour ((char*) &hdr->bh_MaxBoundary);
    bf_SwapFour ((char*) &hdr->bh_Begin.ds_yymmdd);
    bf_SwapFour ((char*) &hdr->bh_Begin.ds_hhmmss);
    bf_SwapFour ((char*) &hdr->bh_End.ds_yymmdd);
    bf_SwapFour ((char*) &hdr->bh_End.ds_hhmmss);
    bf_SwapFour ((char*) &hdr->bh_NBoundary);
}



static void
bf_SwapTable (struct BFBTable* tbl)
/*
 * Swap bytes as necessary to change the given BFBTable's endianness
 */
{
    bf_SwapFour ((char*) &tbl->bt_NPoint);
    bf_SwapFour ((char*) &tbl->bt_Time.ds_yymmdd);
    bf_SwapFour ((char*) &tbl->bt_Time.ds_hhmmss);
    bf_SwapFour ((char*) &tbl->bt_Offset);
}



static void
bf_SwapLocation (struct Location* loc)
/*
 * Swap bytes as necessary to change the given Location's endianness
 */
{
    bf_SwapFour ((char*) &loc->l_lat);
    bf_SwapFour ((char*) &loc->l_lon);
    bf_SwapFour ((char*) &loc->l_alt);
}



static void
bf_SwapFour (char* v)
/*
 * Four byte in-place swap
 */
{
    char c;
    c = v[0]; v[0] = v[3]; v[3] = c;
    c = v[1]; v[1] = v[2]; v[2] = c;
}
