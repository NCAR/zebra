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

# include <fcntl.h>
# include <dirent.h>
# include <errno.h>
# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dsDaemon.h"

MAKE_RCSID ("$Id: d_Scan.c,v 1.10 1994-02-22 22:11:33 corbet Exp $")


/*
 * Local stuff.
 */
static void	ScanDirectory FP ((Platform *, int, int));
static void	ScanFile FP ((Platform *, char *, char *, int, int));
static int	FileKnown FP ((Platform *, char *, char *, int));
static int	FileChanged FP ((Platform *p, DataFile *df));
static void	CleanChain FP ((Platform *, int));
static int	LoadCache FP ((Platform *, int));


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
	 * Don't scan subplatforms.
	 */
		if (p->dp_flags & DPF_SUBPLATFORM)
			continue;
	/*
	 * Scan the local directory, and the remote one if it exists.
	 */
		ScanDirectory (p, TRUE, FALSE);
		if (p->dp_flags & DPF_REMOTE)
			ScanDirectory (p, FALSE, FALSE);
	}
/*
 * Update the time of this scan if we're not using stat revisions
 */
	if (!StatRevisions)
	{
		ZebTime zt;

		tl_Time (&zt);
		LastScan = TC_ZtToSys (&zt);
	}
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
 * Try to load a cache file.
 */
	if (! rescan)
		cloaded = (p->dp_flags & cflag) || LoadCache (p, local);
	if (cloaded && (local ? LDirConst : RDirConst))
		return; /* Cache is gospel in this case */
/*
 * Make sure there really is a directory.
 */
	if (! (dp = opendir (dir)))
	{
		msg_ELog (EF_PROBLEM,
			"Data dir %s (plat %s) nonexistent", dir, p->dp_name);
		if (mkdir (dir, 0777))
		{
			msg_ELog (EF_PROBLEM, "...and unable to create.");
			return;
		}
		dp = opendir (dir);
	}
/*
 * Go through the files.
 */
	while (ent = readdir (dp))
		ScanFile (p, dir, ent->d_name, local, rescan || cloaded);
	closedir (dp);
/*
 * If we loaded a cache, go through and preen out nonexistent files.
 */
	if (cloaded)
		CleanChain (p, local ? LOCALDATA (*p) : REMOTEDATA (*p));
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
	DataFile *df;
	int ns;
	char abegin[40], aend[40];
/*
 * If DFA doesn't recognize it, we don't even bother.
 */
	if (! dfa_CheckName (p->dp_ftype, file))
		return;
/*
 * If this is a rescan, check to see if we already know about this file.
 */
	if (rescan && FileKnown (p, dir, file, local))
		return;
/*
 * Grab a new datafile entry and begin to fill it in.
 */
	if (! (df = dt_NewFile ()))
		return;	/* bummer */
	strncpy (df->df_name, file, sizeof(df->df_name));
	df->df_name[sizeof(df->df_name) - 1] = '\0';
	if (strlen(file) >= sizeof(df->df_name))
		msg_ELog (EF_PROBLEM, "%s: scanned filename too long", file);
	df->df_flags = DFF_Seen;
	if (! local)
		df->df_flags |= DFF_Remote;
	df->df_platform = p - PTable;
	if (StatRevisions)
		df->df_rev = dfa_StatRevision (p, df);
	else
		df->df_rev = 0;
/*
 * Find the times for this file.
 */
	if (! dfa_QueryDate (p->dp_ftype, dfa_FilePath (p, df), &df->df_begin,
			&df->df_end, &ns))
	{
		msg_ELog (EF_PROBLEM, "File '%s' inaccessible", df->df_name);
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
 * Finish the fillin and add it to this platform's list.
 */
	df->df_ftype = p->dp_ftype;
	dt_AddToPlatform (p, df, local);
	p->dp_flags |= DPF_DIRTY;
/*
 * This is a rather ugly kludge.....if this becomes the most recent file
 * for this platform, send out a notification for it.
 */
	if (rescan && LOCALDATA(*p) == (df - DFTable))
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
	int fd, version;
	char fname[300];
/*
 * See if we can get the cache file.
 */
	strcpy (fname, local ? p->dp_dir : p->dp_rdir);
	strcat (fname, "/.ds_cache");
	if ((fd = open (fname, O_RDONLY)) < 0)
	{
		msg_ELog (EF_DEBUG, "No cache file for %s", p->dp_name);
		return (FALSE);
	}
/*
 * Pull in the protocol version number.
 */
	read (fd, &version, sizeof (int));
	if (version != DSProtocolVersion)
	{
		msg_ELog (EF_PROBLEM, "Cache version mismatch for %s",
			p->dp_name);
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
		dt_AddToPlatform (p, df, local);
	}
}





static int
FileKnown (p, dir, file, local)
Platform *p;
char *dir, *file;
bool local;
/*
 * See if we already know about this file.
 */
{
	int dfi = local ? LOCALDATA (*p) : REMOTEDATA (*p);
	int isconst = local ? LFileConst : RFileConst;
/*
 * Just pass through the list and see if we find it.
 */
	for (; dfi; dfi = DFTable[dfi].df_FLink)
		if (! strcmp (DFTable[dfi].df_name, file))
			break;
	if (! dfi)
	{
		msg_ELog (EF_DEBUG, "New file %s/%s", p->dp_name, file);
		return (FALSE);
	}
/*
 * Now we need to see if maybe the file has changed on us.  If so,
 * we zap it from the list and start over.
 */
	msg_ELog (EF_DEBUG, "File %s known dfi %d", file, dfi);
	DFTable[dfi].df_flags |= DFF_Seen;
	if (isconst || (FileChanged (p, DFTable + dfi) == 0))
		return (TRUE);
	msg_ELog (EF_DEBUG, "File %s changed", file);
	dt_RemoveDFE (p, dfi);
	return (FALSE);
}



static int
FileChanged (p, df)
Platform *p;
DataFile *df;
/*
 * Try to determine whether this file has been modified behind our back.
 * Return non-zero if we think it has, zero otherwise.
 */
{
	long rev = dfa_StatRevision(p, df);

	/*
	 * If we're using stat() revision numbers, the answer is easy
	 */
	if (StatRevisions)
	{
		return (rev == df->df_rev);
	}
	/*
	 * Otherwise, compare the stat revision to the time of the last scan.
	 */
	else
	{
		return (rev > LastScan);
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
			if ((p->dp_flags & DPF_SUBPLATFORM) == 0)
				RescanPlat (p);
		}
	}
/*
 * Otherwise just do the one they asked for.
 */
	else
		RescanPlat (PTable + platid);
/*
 * Update the time for the last full scan when not using stat revisions
 */
	if (all && !StatRevisions)
	{
		ZebTime zt;

		tl_Time (&zt);
		LastScan = TC_ZtToSys (&zt);
	}
}




void
RescanPlat (p)
Platform *p;
/*
 * Rescan the files for this platform.
 */
{
	int dfindex;
/*
 * Go through and clear the "seen" flags.
 */
	for (dfindex = LOCALDATA (*p); dfindex;
				dfindex = DFTable[dfindex].df_FLink)
		DFTable[dfindex].df_flags &= ~DFF_Seen;
	for (dfindex = REMOTEDATA (*p); dfindex;
				dfindex = DFTable[dfindex].df_FLink)
		DFTable[dfindex].df_flags &= ~DFF_Seen;
/*
 * Rescan the directory(ies).
 */
	ScanDirectory (p, TRUE, TRUE);
	if (p->dp_flags & DPF_REMOTE && ! RDirConst)
		ScanDirectory (p, FALSE, TRUE);
/*
 * Now get rid of anything that has disappeared.
 */
	CleanChain (p, LOCALDATA (*p));
	if (p->dp_flags & DPF_REMOTE && ! RDirConst)
		CleanChain (p, REMOTEDATA (*p));
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
WriteCache (cmd)
struct ui_command *cmd;
/*
 * Dump out cache files for all local directores.  (Only those with changes
 * if "onlydirty" is set).
 */
{
	int plat, fd, df, version = DSProtocolVersion;
	char fname[300];
	bool onlydirty = FALSE, onefile;
/*
 * Do they want a unified file?
 */
	if (cmd && ! (onlydirty = (cmd->uc_ctype == UTT_KW)) &&
	    (onefile = (cmd->uc_ctype != UTT_END)))
	{
		if ((fd = open (UPTR (*cmd), O_WRONLY|O_CREAT|O_TRUNC,
				0664)) < 0)
		{
			msg_ELog (EF_PROBLEM, "Error %d opening %s",
				  errno, UPTR (*cmd));
			return;
		}
		write (fd, &version, sizeof (int));
	}
/*
 * Now plow through the platforms and do it.
 */
	for (plat = 0; plat < NPlatform; plat++)
	{
		Platform *p = PTable + plat;
	/*
	 * We don't dump subplatforms.
	 */
		if (p->dp_flags & DPF_SUBPLATFORM)
			continue;
	/*
	 * Maybe they only want to write those which have changed.
	 */
	 	if (onlydirty && ! (p->dp_flags & DPF_DIRTY))
			continue;
	/*
	 * Create the dump file if we're doing individual files.
	 */
		if (! onefile)
		{
			sprintf (fname, "%s/.ds_cache", p->dp_dir);
			if ((fd = open (fname, O_WRONLY|O_CREAT|O_TRUNC,
					0664)) < 0)
			{
				msg_ELog (EF_PROBLEM, "Error %d opening %s",
					  errno, fname);
				continue;
			}
			msg_ELog (EF_DEBUG, "Cache %s opened", fname);
			write (fd, &version, sizeof (int));
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
	 	for (df = LOCALDATA (*p); df; df = DFTable[df].df_FLink)
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
}





void
ReadCacheFile (fname, local)
char *fname;
int local;
/*
 * Pull in a big cache file.
 */
{
	int fd, version, cflag = local ? DPF_CLOADED : DPF_RCLOADED;
	Platform *p = NULL;
/*
 * Open up the file and check the version number.
 */
	if ((fd = open (fname, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, fname);
		return;
	}
	read (fd, &version, sizeof (int));
	if (version != DSProtocolVersion)
	{
		msg_ELog (EF_PROBLEM, "Cache version mismatch in %s", fname);
		close (fd);
		return;
	}
/*
 * Plow through it.
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
			if ((p = dt_FindPlatform (df->df_name, FALSE)) == NULL)
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
	close (fd);
}
			
