/*
 * Code related to data file scanning.
 */

/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

# include <stdlib.h>
# include <stdio.h>
# include <unistd.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <string.h>
# include <fcntl.h>
# include <dirent.h>
# include <errno.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "dsDaemon.h"
# include "Platforms.h"		/* for dt_SetString */
# include "byteorder.h"

RCSID ("$Id: d_Scan.c,v 1.41 2005-01-16 18:16:31 granger Exp $")

/*
 * Define this to force changed files to be closed and re-opened by
 * datastore clients rather than sync'ed.
 */
/* #define FORCE_CLOSE_SYNC */

/*
 * Local stuff.
 */
static void ScanDirectory (Source *src, const Platform *p, zbool rescan);
static DataFileCore* GetSortedSrcFiles (Source *src, const Platform *p, 
					int *nsfiles);
static int CompareFiles (const void *v1, const void *v2);
static void ScanFile (const char *file, Source *src, const Platform *p,
		      DataFileCore *sfiles, int *nsfiles);
static int FileKnown (const char *file, const DataFileCore *sfiles,
		      const int nsfiles);
static int FileChanged (Source *src, const Platform *p, 
			const DataFileCore *dfc, ino_t *new_ino, 
			long *new_rev);
static void RescanPlat (Source *src, const Platform *p);

static int ScanProto[] =
{
	MT_MESSAGE, MT_ELOG, MT_PING, MT_CPING, MT_QUERY, MT_MTAP, MT_FINISH
};

static int NProto = sizeof (ScanProto) / sizeof (ScanProto[0]);

/*
 * File count, for user info messages
 */
static int FilesScanned;


void
DataScan (Source *src)
/*
 * Go through the disk and see what data already exists.
 */
{
    int plat;

    FilesScanned = 0;

    /*
     * If src is dirconst (meaning we wouldn't be comparing the directory
     * listing in ScanDirectory() with the cache listing) and data
     * directories do not need to be created if non-existent, then
     * the initial scan of this source is a no-op.  This avoids the 
     * automatic open and close of the data directory in ScanDirectory().
     */
    if (! src_IsDirConst (src) || src_DirsAreForced (src))
    {
	/*
	 * Do this for every platform.
	 */
	for (plat = 0; plat < dt_NPlatform(); plat++)
	{
	    const Platform *p = dt_FindPlatform (plat);
	    /*
	     * We sometimes hit a bad id if a platform has been deleted.
	     */
	    if (! p)
		continue;
	    /*
	     * Check and handle any pending messages except ds protocol
	     */
	    while (msg_PollProto (0, NProto, ScanProto) != MSG_TIMEOUT)
		/* handle messages besides our own */ ;

	    /*
	     * If it isn't a subplatform or virtual platform, scan its
	     * directory.
	     */
	    if (! pi_Subplatform(p) && ! pi_Virtual(p))
		ScanDirectory (src, p, FALSE);
	    /*
	     * Update the count of scanned platforms, i.e. the highest
	     * scanned PID.
	     */
	    ++PlatformsScanned;
	}
    }
/*
 * Update the time of this scan if we're not using stat revisions
 */
    if (!StatRevisions)
	LastScan = time (NULL);

    if (FilesScanned > 0)
	msg_ELog (EF_INFO, "%d files scanned.", FilesScanned);
}



void
Rescan (Source *src, const Platform *pin, zbool all)
/*
 * Implement the rescan request.
 */
{
    FilesScanned = 0;
/*
 * Reset the "file constant" flags -- we always want to check on a rescan.
 */
    src_SetFileConst (src, FALSE);
/*
 * If they want everything done, then we need to pass through the list.
 */
    if (all)
    {
	int plat;
	for (plat = 0; plat < dt_NPlatform(); plat++)
	{
	    const Platform *p = dt_FindPlatform (plat);
	/*
	 * Don't scan subplatforms or null platforms.
	 */
	    if (p && ! pi_Subplatform (p) && ! pi_Virtual (p))
		RescanPlat (src, p);
	}
    /*
     * Update the time for the last full scan
     */
	LastScan = time (NULL);
    }
/*
 * Otherwise just do the one they asked for.
 */
    else
	RescanPlat (src, pin);

    if (FilesScanned > 0)
	msg_ELog (EF_INFO, "%d files scanned.", FilesScanned);
}




static void
ScanDirectory (Source *src, const Platform *p, zbool rescan)
/*
 * Scan the directory for this source/platform pair.  This should really
 * become a Source method.
 */
{
    const char *dir = src_DataDir (src, p);
    DIR *dp;
    struct dirent *ent;
    int i;

    DataFileCore *sfiles = 0;
    int nsfiles = 0;
/*
 * Make sure there really is a directory.  If not, we may try to create it,
 * in which case we won't need to scan it or check for cache files.
 */
    if (! (dp = opendir (dir)))
    {
	if (src_DirsAreForced (src))
	    src_ConfirmDataDir (src, p);
    }
    else
    {
    /*
     * If it's not a forced rescan and the source is "directory constant",
     * we're done.
     */
	if (! rescan && src_IsDirConst (src))
	{
	    closedir (dp);
	    return; /* Cache is gospel in this case */
	}
    }
/*
 * Get the complete list of files currently known in the source, 
 * sorted by filename.  This is our list of unverified files, and
 * will be modified as we scan the files in the directory.
 */
    sfiles = GetSortedSrcFiles (src, p, &nsfiles);
/*
 * Go through the files in the directory, if we have a directory.  If the
 * directory did not exist above, then all the files known in the source
 * will be unverified and consequently removed from the source.
 */
    if (dp)
    {
	while ((ent = readdir (dp)))
	{
	    ScanFile (ent->d_name, src, p, sfiles, &nsfiles);
	    if ((++FilesScanned % 25) == 0)
	    {
		while (msg_PollProto (0, NProto, ScanProto) != MSG_TIMEOUT)
		/* handle messages */ ;
	    }
	    if ((FilesScanned % 1000) == 0)
		msg_ELog (EF_INFO, "%d files scanned so far", FilesScanned);
	}
	closedir (dp);
    }
/*
 * Any files remaining in our unverified file list are no longer in
 * the directory, or else there was no directory, so tell the source
 * to forget about them.
 */
    for (i = 0; i < nsfiles; i++)
    {
	DataFile df;
	    
	msg_ELog (EF_DEBUG, "File %s disappeared", sfiles[i].dfc_name);

	BuildDataFile (&df, sfiles + i, src, p);
	DataFileGone (&df);

	src_RemoveFile (src, p, sfiles + i);
    }

    free (sfiles);
}



static DataFileCore*
GetSortedSrcFiles (Source *src, const Platform *p, int *nsfiles)
/*
 * Return an array of DataFileCore structs known by the given source, sorted
 * by filename.  The array should be freed by the caller.
 */
{
    zbool ok;
    DataFileCore *files;
    int nfiles;
    int n;
/*
 * Allocate the array, one greater than needed in case nfiles is 0.
 */ 
    nfiles = src_NFiles (src, p);
    files = (DataFileCore*) malloc ((nfiles+1) * sizeof (DataFileCore));
    n = 0;
/*
 * Get all of the files from the source for this platform
 */
    for (ok = src_First (src, p, &files[n++]); ok; 
	 ok = src_Next (src, p, &files[n++]))
    {
	if (n > nfiles)
	{
	/*
	 * Oops, we wrote past our allocated memory... 
	 */
	    msg_ELog (EF_EMERGENCY, 
		      "%s:%s returned more files than expected (%d > %d)!",
		      src_Name (src), pi_Name (p), n, nfiles);
	    exit (1);
	}
    }

    if (n < nfiles)
    {
	msg_ELog (EF_PROBLEM, "%s:%s returned fewer files than expected!",
		  src_Name (src), pi_Name (p));
	nfiles = n - 1;
    }
/*
 * Now sort'em
 */
    qsort ((void*) files, nfiles, sizeof (*files), CompareFiles);
/*
 * Put stuff into the return parameters and we're done
 */
    *nsfiles = nfiles;
    return (files);
}



static int
CompareFiles (const void *v1, const void *v2)
/*
 * qsort comparison function for DataFileCore structs
 */
{
    const DataFileCore *dfc1 = (const DataFileCore*) v1;
    const DataFileCore *dfc2 = (const DataFileCore*) v2;
    return (strcmp (dfc1->dfc_name, dfc2->dfc_name));
}



static void
ScanFile (const char *file, Source *src, const Platform *p, 
	  DataFileCore *sfiles, int *nsfiles)
/*
 * Look at this file and see how it compares to what our source knows.
 * If we find it in the list of unverified files for the source, update
 * it in the source and remove it from the unverified list.  Otherwise,
 * add it to the source as a new file.
 */
{
    DataFileCore dfc, existing_dfc;
    zbool newfile = FALSE;
    int ndx, ns;
    ino_t new_ino;
    long new_rev;
    char abegin[40], aend[40];
    ZebraTime last_time;
    int isconst = src_IsFileConst (src);
/*
 * If DFA doesn't recognize it, we don't even bother.
 */
    if (! dfa_CheckName (pi_FileType(p), file))
	return;
/*
 * Check to see if this file is in our list of known (but unverified) files.
 */
    if ((ndx = FileKnown (file, sfiles, *nsfiles)) >= 0)
    {
	newfile = FALSE;
	dfc = sfiles[ndx];
    /* 
     * Remove this file from the list of unverified files.  By the time
     * we're done here, it's considered verified... 
     */
	memmove (sfiles + ndx, sfiles + ndx + 1, 
		 (*nsfiles - ndx - 1) * sizeof (DataFileCore));
	(*nsfiles)--;
    /*
     * If the file hasn't changed, then we need go no further
     */
	if (isconst || ! FileChanged (src, p, &dfc, &new_ino, &new_rev))
	    return;
    /*
     * But is it the same file (but different), or a new one?
     * If we're not using stat(), inodes aren't set, so we'll
     * be conservative and close and re-open the file.
     */
#ifndef FORCE_CLOSE_SYNC
	if (new_ino || !StatRevisions)
#else
	if (1)
#endif
	{
	    DataFile df;
	/* Uh-oh, the old file is history */
	    msg_ELog (EF_DEBUG, "File %s: changed, %s, %s",
		      file, 
#ifndef FORCE_CLOSE_SYNC
		      (StatRevisions) ? 
		      "new inode" : "inodes disabled",
#else
		      "syncs fail",
#endif
		      "removing datafile entry");
	/* 
	 * Remove the file from our source, and tell clients to close the file
	 */
	    src_RemoveFile (src, p, &dfc);

	    BuildDataFile (&df, &dfc, src, p);
	    DataFileGone (&df);

	    newfile = TRUE;	/* start over with a new df */
	}
	else
	    msg_ELog (EF_DEBUG, "File %s: changed, updating list", file);
    }
    else
	newfile = TRUE;
/*
 * If we get here, we have either a changed file or a new file
 */
    if (newfile)
    {
	int isfile = 0;

	msg_ELog (EF_DEBUG, "New file %s:%s:%s", src_Name (src), pi_Name (p),
		  file);
    /*
     * Allocate and initialize a whole new file entry
     */
	dt_SetString (dfc.dfc_name, file, sizeof(dfc.dfc_name),
		      "scanning new file");
	/*
	 * Always stat a new file just to make sure it's not a directory.
	 * After this it will never be checked with stat() again if 
	 * StatRevisions is false.
	 */
	dfc.dfc_ftype = pi_FileType(p);
	dfc.dfc_rev = StatRevision (DataFilePath (src, p, &dfc), 
				    &dfc.dfc_inode, &isfile);
	if (! isfile)
	{
	    msg_ELog (EF_DEBUG, "not a file or stat failed: skipping %s",
		      file);
	    return;
	}
	if (! StatRevisions)
	{
	    dfc.dfc_inode = 0;
	    dfc.dfc_rev = 0;
	}
    }
    else
    {
    /*
     * Update revisions so clients with this file open will sync with it
     */
	if (StatRevisions)
	    dfc.dfc_rev = new_rev;
	else
	    ++dfc.dfc_rev;
    }
/*
 * Check the file directly, and set the begin and end times and the sample 
 * count in our DataFileCore.
 */
    if (! dfa_QueryDate (pi_FileType(p), DataFilePath (src, p, &dfc), 
			 &dfc.dfc_begin, &dfc.dfc_end, &ns))
    {
	msg_ELog (EF_PROBLEM, "File '%s' inaccessible", 
		  DataFilePath (src, p, &dfc));

	if (! newfile)
	    src_RemoveFile (src, p, &dfc);

	return;
    }

    dfc.dfc_nsample = ns;
    if (pi_FileType(p) == FTOpaque)
    {
	int same = 0;
	/*
	 * Well here's a kludge if I've ever seen one.  For the opaque
	 * filetype, we don't care about overlapping sample times.  So
	 * overload the milliseconds field with a count to differentiate
	 * files with the same timestamp.
	 */
	while (newfile && src_FindExact (src, p, &dfc.dfc_begin, 
					 &existing_dfc))
	{
	    ++dfc.dfc_begin.zt_MicroSec;
	    ++same;
	}
	if (same > 0)
	    msg_ELog (EF_DEBUG, "%s: time tweaked to avoid collision with %d "
		      "other opaque files", file, same);
    }
/*
 * Report if we're going to stomp on a file already in the source with 
 * the same start time
 */
    else if (newfile && src_FindExact (src, p, &dfc.dfc_begin, &existing_dfc))
    {
	msg_ELog (EF_PROBLEM, "File %s being superceded", 
		  DataFilePath (src, p, &existing_dfc));
	msg_ELog (EF_PROBLEM, "by %s, with the same start time",
		  DataFilePath (src, p, &dfc));
    /*
     * If the existing file is in our list of unverified files, take it out
     * now, because it won't be in our source's list much longer.
     */
	if ((ndx = FileKnown (existing_dfc.dfc_name, sfiles, *nsfiles)) >= 0)
	{
	    memmove (sfiles + ndx, sfiles + ndx + 1, 
		     (*nsfiles - ndx - 1) * sizeof (DataFileCore));
	    (*nsfiles)--;
	}
    }
/*
 * Debugging is nice sometimes.
 */
    TC_EncodeTime (&dfc.dfc_begin, TC_Full, abegin);
    TC_EncodeTime (&dfc.dfc_end, TC_TimeOnly, aend);
    msg_ELog (EF_DEBUG, "%s: %s, %s to %s ns %d", src_Name (src), file, 
	      abegin, aend, ns);
/*
 * Add or update the file for this source/platform list.
 */
    src_UpdateFile (src, p, &dfc);
    
    if (! newfile)
    {
	DataFile df;
	BuildDataFile (&df, &dfc, src, p);
    }
/*
 * This is a rather ugly kludge.... if this becomes (or was already)
 * the most recent file for this platform, send out a notification
 * for it.
 */
    src_LastTime (src, p, &last_time);

    if (TC_Eq (dfc.dfc_end, last_time))
	dap_Notify (pi_Id (p), &last_time, ns, 0, TRUE);
}



static int
FileKnown (const char *file, const DataFileCore *sfiles, const int nsfiles)
/*
 * See if we already know about a file by this name; return its index
 * in the sfiles array if found, else -1.  The sfiles array must be
 * sorted by filename.
 */
{
    int bot, top, mid, test;
/*
 * Quick check against the ends of the list
 */
    if (nsfiles == 0)
	return (-1);

    test = strcmp (file, sfiles[0].dfc_name);
    if (test < 0)
	return (-1);	/* this file compares < any in the list */
    else if (test == 0)
	return (0);

    test = strcmp (file, sfiles[nsfiles-1].dfc_name);
    if (test > 0)
	return (-1);	/* this file compares > any in the list */
    else if (test == 0)
	return (nsfiles - 1);
/*
 * Binary search from here
 */
    bot = 0;
    top = nsfiles - 1;
    mid = (bot + top) / 2;

    for (; mid != bot; mid = (bot + top) / 2)
    {
	test = strcmp (file, sfiles[mid].dfc_name);

	if (test < 0)
	    top = mid;
	else if (test == 0)
	    return (mid);
	else
	    bot = mid;
    }
/*
 * If we get here, the file is not in the list
 */
    return (-1);
}



static int
FileChanged (Source *src, const Platform *p, const DataFileCore *dfc, 
	     ino_t *new_ino, long *new_rev)
/*
 * Try to determine whether this file has been modified behind our back.
 * Return non-zero if we think it has, zero otherwise.  If the inode has
 * changed as well, indicating a new file and not just a changed one,
 * return the new inode in *new_ino, else set *new_ino to 0.
 * If StatRevisions is false, dfe inodes have not been set, so *new_ino 
 * is set to zero.  
 *
 * If the df rev number has leaped forward because of multiple changes at
 * the same mtime, we won't catch them unless the stat() mtime is now
 * greater than the "warped" rev number.
 *
 * Return the stat() mtime of the file in *new_rev, so that we don't
 * have to be called a second time on this file to set the rev.
 */
{
    ino_t inode;

    *new_rev = StatRevision (DataFilePath (src, p, dfc), &inode, 0);
/*
 * If we're using stat() revision numbers, the answer is easy
 */
    if (StatRevisions)
    {
	*new_ino = (inode != dfc->dfc_inode) ? (inode) : 0;
#ifdef DEBUG
	msg_ELog (EF_DEBUG, 
		  "checking %s: current rev %li, stat mtime %li",
		  DataFilePath (src, p, dfc), dfc->dfc_rev, *new_rev);
#endif
	return ((*new_rev > dfc->dfc_rev) || (*new_ino));
    }
/*
 * Otherwise, compare the stat revision to the time of the last scan.
 * We don't include the inode check since df_inode is always zero
 * when stat() is not being used.
 */
    else
    {
	*new_ino = 0;
	return (*new_rev > LastScan);
    }
}




static void
RescanPlat (Source *src, const Platform *p)
/*
 * Rescan the files for this platform.
 */
{
    if (pi_Subplatform(p) || pi_Virtual(p))
	return;
/*
 * Rescan the directory(ies).
 */
    ScanDirectory (src, p, TRUE);
}



long 
StatRevision (const char *name, ino_t *inode, int *isfile)
/*
 * Get a revision count for this file from its modification time
 */
{
	struct stat sbuf;

	if (inode) *inode = 0;
	if (isfile) *isfile = 0;
	if (stat (name, &sbuf) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d on stat of %s", errno, name);
		return (0);
	}
	if (inode) *inode = sbuf.st_ino;
	if (isfile) *isfile = S_ISREG(sbuf.st_mode);
#ifdef DEBUG
	msg_ELog (EF_DEBUG, "stat file %s: rev %d inode %d", name,
		  sbuf.st_mtime, sbuf.st_ino);
#endif
	return (sbuf.st_mtime);
}
