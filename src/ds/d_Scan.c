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

RCSID ("$Id: d_Scan.c,v 1.32 1996-11-19 09:29:44 granger Exp $")

/*
 * Define this to force changed files to be closed and re-opened by
 * datastore clients rather than sync'ed.
 */
/* #define FORCE_CLOSE_SYNC */

/*
 * Local stuff.
 */
static void	ScanDirectory FP ((Platform *, int, int));
static void	ScanFile FP ((Platform *, char *, char *, int, int));
static int	FileKnown FP ((Platform *, char *, int local));
static int	FileChanged FP ((Platform *p, DataFile *df, 
				 ino_t *new_ino, long *new_rev));
static void	CleanChain FP ((Platform *, int));
static int	LoadCache FP ((Platform *, int));
static char     *CacheFileName FP((Platform *p, int local, int version));

static int ScanProto[] =
{
	MT_MESSAGE, MT_ELOG, MT_PING, MT_CPING, MT_QUERY, MT_MTAP, MT_FINISH
};

static int NProto = sizeof (ScanProto) / sizeof (ScanProto[0]);

void
DataScan ()
/*
 * Go through the disk and see what data already exists.
 */
{
	int plat;
/*
 * Do this for every platform.
 */
	for (plat = 0; plat < NPlatform; plat++)
	{
		Platform *p = PTable + plat;
	/*
	 * Check and handle any pending messages except ds protocol
	 */
		while (msg_PollProto (0, NProto, ScanProto) != MSG_TIMEOUT)
			/* handle messages besides our own */ ;
	/*
	 * Don't really scan subplatforms or virtual platforms.
	 */
		if (! pi_Subplatform(p) && ! pi_Virtual(p))
		{
		/*
		 * Scan the local directory, and the remote one if it exists.
		 */
			ScanDirectory (p, TRUE, FALSE);
			if (pi_Remote(p))
				ScanDirectory (p, FALSE, FALSE);
		}
	/*
	 * Update the count of scanned platforms, i.e. the highest scanned PID
	 */
		++PlatformsScanned;
	}
/*
 * Update the time of this scan if we're not using stat revisions
 */
	if (!StatRevisions)
		LastScan = time (NULL);
}




static void
ScanDirectory (p, local, rescan)
Platform *p;
bool local, rescan;
/*
 * Scan a directory for this platform.
 */
{
	char *dir = local ? p->dp_dir : p->dp_rdir;
	DIR *dp;
	struct dirent *ent;
	int cloaded = FALSE, cflag = local ? DPF_CLOADED : DPF_RCLOADED;
/*
 * Make sure there really is a directory.  If not, we may try to create it,
 * in which case we won't need to scan it or check for cache files.
 *
 * XXX Which is faster: access() directory to make sure it exists, open it
 * first but immediately close it if we don't need it, or the old way of
 * trying to open three cache files before the directory and only then
 * realizing the data directory didn't exist?
 */
	if (! (dp = opendir (dir)))
	{
		if (! local)		/* don't create remote directories */
		{
			msg_ELog(EF_DEBUG, "cannot access remote dir %s", dir);
		}				  
		else if (DelayDataDirs)	/* don't create local directories */
		{
			p->dp_flags &= ~DPF_DIREXISTS;
		}
		else if (! dt_MakeDataDir (dir))
		{
			msg_ELog (EF_PROBLEM, 
				  "Cannot open or create dir %s (plat %s)",
				  dir, p->dp_name);
		}
		else
		{
			msg_ELog (EF_INFO, 
				  "Created data dir %s (plat %s)", dir,
				  p->dp_name);
			p->dp_flags |= DPF_DIREXISTS;
			p->dp_flags |= DPF_DIRTY;
		}
	}
	else
	{
		int count = 0;

		if (local)
			p->dp_flags |= DPF_DIREXISTS;
	/*
	 * Try to load a cache file.
	 */
		if (! rescan)
			cloaded = (p->dp_flags & cflag) || LoadCache(p,local);
		if (cloaded && (local ? LDirConst : RDirConst))
		{
			closedir (dp);
			return; /* Cache is gospel in this case */
		}
	/*
	 * Go through the files.
	 */
		while ((ent = readdir (dp)))
		{
			ScanFile (p, dir, ent->d_name, local, 
				  rescan || cloaded);
			if ((++count % 25) == 0)
			{
				while (msg_PollProto (0, NProto, ScanProto)
				       != MSG_TIMEOUT)
					/* handle messages */ ;
			}
		}
		closedir (dp);
	/*
	 * If we loaded a cache, go through and preen out nonexistent files.
	 */
		if (cloaded)
			CleanChain (p, local ? pi_LocalData (p) : 
				    pi_RemoteData (p));
	}
}





static void
ScanFile (p, dir, file, local, rescan)
Platform *p;
char *file, *dir;
bool local, rescan;
/*
 * Look at this file and see what we think of it.
 */
{
	DataFile *df = NULL;
	int dfi;
	int ns;
	ino_t new_ino;
	long new_rev;
	char abegin[40], aend[40];
	int isconst = local ? LFileConst : RFileConst;
/*
 * If DFA doesn't recognize it, we don't even bother.
 */
	if (! dfa_CheckName (pi_FileType(p), file))
		return;
/*
 * If this is a rescan, check to see if we already know about this file.
 * If the file is known and has not changed, we're in the clear.  If this
 * isn't a rescan, then we automatically need a new file entry.
 */
	dfi = 0;   /* <-- non-zero means we're re-using an existing entry */
	if (rescan && ((dfi = FileKnown (p, file, local)) > 0))
	{
		df = DFTable + dfi;
		if (isconst || ! FileChanged(p, df, &new_ino, &new_rev))
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
			/* tell clients to close the file */
			DataFileGone (df);
			dt_RemoveDFE (p, dfi);
			dfi = 0;	/* start over with a new dfe */
		}
		else
		{
			msg_ELog (EF_DEBUG, "File %s: changed, %s", 
				  file, "re-sorting chain");
		}
	}
/*
 * If this is only a changed file, try to re-use the current entry but
 * update its revision number.  Else grab a new datafile entry and begin 
 * to fill it in.
 */
	if (dfi)
	{
	/*
	 * Update revisions so clients with this file open will sync with it
	 */
		if (StatRevisions)
			df->df_rev = new_rev;
		else
			++df->df_rev;
	}
	else if ((df = dt_NewFile ()) != NULL)
	{
	/*
	 * Initialize a whole new file entry
	 */
		dt_SetString (df->df_name, file, sizeof(df->df_name),
			      "scanning new file");
		df->df_flags = DFF_Seen;
		if (! local)
			df->df_flags |= DFF_Remote;
		df->df_platform = p - PTable;
		if (StatRevisions)
			df->df_rev = StatRevision (p, df, &df->df_inode);
		else
		{
			df->df_inode = 0;
			df->df_rev = 0;
		}
	}
	else
	{
		return;		/* bummer -- no new data files */
	}
/*
 * Find the times for this file.
 */
	if (! dfa_QueryDate(pi_FileType(p), dt_DFEFilePath(p, df), 
			    &df->df_begin, &df->df_end, &ns))
	{
		msg_ELog (EF_PROBLEM, "File '%s' inaccessible", df->df_name);
		if (dfi)
			dt_RemoveDFE (p, dfi);
		else
			dt_FreeDFE (df);
		return;
	}
	df->df_nsample = ns;
/*
 * Debugging is nice sometimes.
 */
 	TC_EncodeTime (&df->df_begin, TC_Full, abegin);
	TC_EncodeTime (&df->df_end, TC_TimeOnly, aend);
	msg_ELog (EF_DEBUG, "%c File '%s', %s to %s ns %d",
		  local ? 'L' : 'C', file, abegin, aend, df->df_nsample);
/*
 * Finish the fill-in and add it to this platform's list.  If the file
 * is not new, move it to its correct position and update the client
 * caches.  Otherwise just add the file to the platform chain as usual.
 */
	if (dfi)
	{
		dt_SortDFE (p, df, local);
		CacheInvalidate (dfi);
	}
	else
	{
		df->df_ftype = pi_FileType(p);
		dt_AddToPlatform (p, df, local);
	}
	p->dp_flags |= DPF_DIRTY;
	/*
	 * This is a rather ugly kludge.... if this becomes (or was already)
	 * the most recent file for this platform, send out a notification
	 * for it.
	 */
	if (rescan && (pi_LocalData(p) == (df - DFTable)))
		dap_Notify (df->df_platform, &df->df_end, ns, 0, TRUE);
}




static int
LoadCache (p, local)
Platform *p;
int local;
/*
 * Attempt to pull in a cache file.
 */
{
	int fd;
	unsigned long version;
/*
 * See if we can get the cache file.  First try the newest standard name,
 * which includes our protocol version, then the name without the version.
 * We no longer check for the old form "<dir>/.ds_cache", since it is
 * guaranteed to be the wrong version.
 */
	if ((fd = open (CacheFileName(p, local, TRUE), O_RDONLY)) < 0)
	{
		if ((fd = open (CacheFileName(p, local, FALSE), O_RDONLY)) < 0)
		{
			msg_ELog (EF_DEBUG, "No cache file for %s", 
				  p->dp_name);
			return (FALSE);
		}
	}
/*
 * Pull in the protocol version number.
 */
	if (read (fd, &version, sizeof (long)) < sizeof (long))
	{
		msg_ELog (EF_PROBLEM, "corrupted cache file: %s", p->dp_name);
		close (fd);
		return (FALSE);
	}
	else if (version != CacheKey && version != DSProtocolVersion)
	{
		msg_ELog(EF_PROBLEM, "cache version mismatch: %s", p->dp_name);
		close (fd);
		return (FALSE);
	}
/*
 * OK, we got one.  Now plow through it and load all the files.
 */
	msg_ELog (EF_DEBUG, "Loading %s cache", p->dp_name);
	for (;;)
	{
		DataFile *df = dt_NewFile ();
	/*
	 * Get the next entry and add it to the list.
	 */
		if (read (fd, df, sizeof (DataFile)) < sizeof (DataFile))
		{
			dt_FreeDFE (df);
			close (fd);
			return (TRUE);
		}
		df->df_index = df - DFTable;
	/*
	 * Get the remote flag right, since the point of view of the 
	 * process that wrote the cache may not agree with our own.
	 */
		df->df_flags &= ~DFF_Seen;
	 	if (! local)
			df->df_flags |= DFF_Remote;
		else
			df->df_flags &= ~DFF_Remote;
		dt_AddToPlatform (p, df, local);
	}
}





static int
FileKnown (p, file, local)
Platform *p;
char *file;
bool local;
/*
 * See if we already know about a file by this name; return its index if found.
 */
{
	int dfi = local ? pi_LocalData(p) : pi_RemoteData(p);
/*
 * Just pass through the list and see if we find it.
 */
	for (; dfi; dfi = DFTable[dfi].df_FLink)
		if (! strcmp (DFTable[dfi].df_name, file))
			break;
	if (! dfi)
	{
		msg_ELog (EF_DEBUG, "New file %s/%s", p->dp_name, file);
	}
	else
	{
		msg_ELog (EF_DEBUG, "File %s known dfi %d", file, dfi);
		DFTable[dfi].df_flags |= DFF_Seen;
	}
	return (dfi);
}



static int
FileChanged (p, df, new_ino, new_rev)
Platform *p;
DataFile *df;
ino_t *new_ino;
long *new_rev;
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

	*new_rev = StatRevision(p, df, &inode);
	/*
	 * If we're using stat() revision numbers, the answer is easy
	 */
	if (StatRevisions)
	{
		*new_ino = (inode != df->df_inode) ? (inode) : 0;
#ifdef DEBUG
		msg_ELog (EF_DEBUG, 
			  "checking %s: current rev %li, stat mtime %li",
			  dt_DFEFilePath (p, df), df->df_rev, *new_rev);
#endif
		return ((*new_rev > df->df_rev) || (*new_ino));
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




void
Rescan (platid, all)
PlatformId platid;
int all;
/*
 * Implement the rescan request.
 */
{
/*
 * Reset the "file constant" flags -- we always want to check on a rescan.
 */
	LFileConst = RFileConst = FALSE;
/*
 * If they want everything done, then we need to pass through the list.
 */
	if (all)
	{
		int plat;
		for (plat = 0; plat < NPlatform; plat++)
		{
			Platform *p = PTable + plat;
		/*
		 * Don't scan subplatforms.
		 */
			if (! pi_Subplatform (p) && ! pi_Virtual (p))
				RescanPlat (p);
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
		RescanPlat (PTable + platid);
}




void
RescanPlat (p)
Platform *p;
/*
 * Rescan the files for this platform.
 */
{
	int dfindex;

	if (pi_Subplatform(p) || pi_Virtual(p))
		return;
/*
 * Go through and clear the "seen" flags.
 */
	for (dfindex = pi_LocalData (p); dfindex;
				dfindex = DFTable[dfindex].df_FLink)
		DFTable[dfindex].df_flags &= ~DFF_Seen;
	for (dfindex = pi_RemoteData (p); dfindex;
				dfindex = DFTable[dfindex].df_FLink)
		DFTable[dfindex].df_flags &= ~DFF_Seen;
/*
 * Rescan the directory(ies).
 */
	ScanDirectory (p, TRUE, TRUE);
	if (pi_Remote(p) && ! RDirConst)
		ScanDirectory (p, FALSE, TRUE);
/*
 * Now get rid of anything that has disappeared.
 */
	CleanChain (p, pi_LocalData (p));
	if (p->dp_flags & DPF_REMOTE && ! RDirConst)
		CleanChain (p, pi_RemoteData (p));
}



long 
StatRevision (p, df, inode)
Platform *p;
DataFile *df;
ino_t *inode;
/*
 * Get a revision count for this file from its modification time
 */
{
	struct stat sbuf;

	if (stat (dt_DFEFilePath (p, df), &sbuf) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d on stat of %s", errno,
				dt_DFEFilePath (p, df));
		if (inode)
			*inode = 0;
		return (0);
	}
	if (inode)
		*inode = sbuf.st_ino;
#ifdef DEBUG
	msg_ELog (EF_DEBUG, "stat file #%d: rev %d inode %d", df->df_index,
		  sbuf.st_mtime, sbuf.st_ino);
#endif
	return (sbuf.st_mtime);
}



static void
CleanChain (p, chain)
Platform *p;
int chain;
/*
 * Get rid of vanished files in this chain.
 */
{
	int dfi, next;

	for (dfi = chain; dfi; dfi = next)
	{
		next = DFTable[dfi].df_FLink;
		if ((DFTable[dfi].df_flags & DFF_Seen) == 0)
		{
			msg_ELog (EF_DEBUG, "File %s disappeared",
				DFTable[dfi].df_name);
			DataFileGone (DFTable + dfi);
			dt_RemoveDFE (p, dfi);
			p->dp_flags |= DPF_DIRTY;
		}
	}
}




void
WriteCache (ufile, onlydirty)
const char *ufile;
int onlydirty;
/*
 * Dump out cache files for all local directores.  (Only those with changes
 * if "onlydirty" is set).  To write a unified cache file, 'ufile' must
 * point to the file name and 'onlydirty' must be false, else write
 * platform cache files.
 */
{
	int plat, df;
	int fd = -1;
	unsigned long version = CacheKey;
	char *fname;
	bool onefile = FALSE;
/*
 * Do they want a unified file?
 */
	if (ufile && ! onlydirty)
	{
		onefile = TRUE;
		if ((fd = open (ufile, O_WRONLY|O_CREAT|O_TRUNC,
				0664)) < 0)
		{
			msg_ELog (EF_PROBLEM, "Error %d opening %s",
				  errno, ufile);
			return;
		}
		write (fd, &version, sizeof (long));
	}
/*
 * Now plow through the platforms and do it.
 */
	for (plat = 0; plat < NPlatform; plat++)
	{
		Platform *p = PTable + plat;
	/*
	 * We don't dump subplatforms or virtual platforms.
	 */
		if (pi_Subplatform(p) || pi_Virtual(p))
			continue;
	/*
	 * Maybe they only want to write those which have changed.
	 */
	 	if (onlydirty && !pi_Dirty(p))
			continue;
	/*
	 * Create the dump file if we're doing individual files.
	 */
		if (! onefile)
		{
			if (! pi_DirExists (p))
				continue; /* can't cache what's not there */
 			fname = CacheFileName(p, TRUE, TRUE);
			if ((fd = open (fname, O_WRONLY|O_CREAT|O_TRUNC,
					0664)) < 0)
			{
				msg_ELog (EF_PROBLEM, "Error %d opening %s",
					  errno, fname);
				continue;
			}
			write (fd, &version, sizeof (long));
		}
	/*
	 * Otherwise put in the platform marker.
	 */
		else
		{
			DataFile fake;
			strcpy (fake.df_name, p->dp_name);
			fake.df_flags = DFF_PlatMarker;
			write (fd, &fake, sizeof (fake));
		}
	/*
	 * Follow the chain.
	 */
	 	for (df = pi_LocalData (p); df && DFTable[df].df_FLink;
					    df = DFTable[df].df_FLink)
			; /* Scan to end of chain */
		for (; df; df = DFTable[df].df_BLink)
			write (fd, DFTable + df, sizeof (DataFile));
		if (! onefile)
			close (fd);
	/*
	 * This platform is now clean.
	 */
	 	p->dp_flags &= ~DPF_DIRTY;
	}
/*
 * Close the file if there's only one.
 */
	if (onefile)
		close (fd);
/*
 * Update the cache time if all of the platforms are now clean
 */
	if (dbg_DirtyCount() == 0)
		LastCache = time (NULL);
}





void
ReadCacheFile (fname, local)
char *fname;
int local;
/*
 * Pull in a big cache file.
 */
{
	int fd, cflag = local ? DPF_CLOADED : DPF_RCLOADED;
	unsigned long version;
	Platform *p = NULL;
/*
 * Open up the file and check the version number.
 */
	if ((fd = open (fname, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, fname);
		return;
	}
	read (fd, &version, sizeof (long));
	if (version != CacheKey && version != DSProtocolVersion)
	{
		msg_ELog (EF_PROBLEM, "Cache version mismatch in %s", fname);
		close (fd);
		return;
	}
/*
 * Plow through the file.
 */
	for (;;)
	{
		DataFile *df = dt_NewFile ();
	/*
	 * Pull in the next entry.
	 */
		if (read (fd, df, sizeof (DataFile)) < sizeof (DataFile))
		{
			dt_FreeDFE (df);
			close (fd);
			return;
		}
		df->df_index = df - DFTable;
	/*
	 * See if we have a platform marker.
	 */
		if (df->df_flags & DFF_PlatMarker)
		{
			if ((p = dt_FindPlatform (df->df_name)) == NULL)
				msg_ELog (EF_PROBLEM, "Funky plat %s in cache",
					  df->df_name);
			else
				p->dp_flags |= cflag;
			dt_FreeDFE (df);
		}
	/*
	 * Otherwise store the file entry.
	 */
		else if (p)
		{
			df->df_flags &= ~(DFF_Seen | DFF_Remote);
			if (! local)
				df->df_flags |= DFF_Remote;
			dt_AddToPlatform (p, df, local);
		}
	}
}
			


static char *
CacheFileName (p, local, version)
Platform *p;
bool local;
bool version;	/* include the cache key if non-zero */
/*
 * Generate the cache file name for this platform.  The returned string
 * is only valid until the next call.  Follow the dfa_MakeFileName
 * convention of just removing the '/' characters.
 */
{
	static char fname[sizeof(p->dp_name)+sizeof(p->dp_dir)+32];
	char name[sizeof(p->dp_name)];
	char *c;
	int i = 0;

	for (c = p->dp_name; *c; ++c)
	{
		if (*c != '/')
			name[i++] = *c;
	}
	name[i] = '\0';
	if (version)
		sprintf (fname, "%s/%s.%lx%s.ds_cache", 
				local ? p->dp_dir : p->dp_rdir, name,
				CacheKey,
# ifdef LITTLE_ENDIAN
				"-l");
# else
				"");
# endif
	else
		sprintf (fname, "%s/%s.ds_cache", 
			 local ? p->dp_dir : p->dp_rdir, name);
	return (fname);
}

