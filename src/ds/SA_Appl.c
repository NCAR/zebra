/*
 * The datastore standalone application module.  Define ds_Standalone,
 * which an application calls to initialize standalone mode.  Function
 * pointers are set in the ds methods structure to shortcut messages
 * to the daemon with internal client-side implementations.
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

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <defs.h>
#include <zl_symbol.h>
#include <zl_regex.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "dsPrivate.h"
#include "Platforms.h"
#include "GetList.h"
#include "dslib.h"
#include "dfa.h"
#include "Appl.h"
#include "d_Source.h"

RCSID ("$Id: SA_Appl.c,v 3.5 2002-09-17 18:28:43 granger Exp $")


/*
 * Our source
 */
Source *Src;

/*
 * Prototypes for stand-alone routines
 */
static void	sa_Notify (const Platform *p, const DataFile *df, 
			   int now, int nnew, int sample, int last);
static int	sa_MakeDataFile (PlatformId, const char *file, 
				 const ZebraTime *t, DataFile *df);
static const DataFile* sa_FindDFAfter (PlatformId pid, const ZebraTime *when, 
				      int srcid);
static const DataFile* sa_FindDFBefore (PlatformId pid, const ZebraTime *when, 
				       int srcid);
static void	sa_DeleteObs (PlatformId pid, const ZebraTime *when);
static void	sa_DeleteData (PlatformId pid, const ZebraTime *when);
static int	sa_GetSrcInfo (int srcid, SourceInfo *si);


int
ds_Standalone()
/*
 * Set up local structures WITHOUT connecting to the datastore.
 * DataStore info will be supplanted by the application and
 * internal standalone routines.
 */
{
    char datadir[128], cachefile[256], *dir;
/*
 * Set standalone mode and change our methods.
 */
    Standalone = 1;

    DSM.dsm_FindBefore = sa_FindDFBefore;
    DSM.dsm_FindAfter = sa_FindDFAfter;
    DSM.dsm_DeleteObs = sa_DeleteObs;
    DSM.dsm_DeleteData = sa_DeleteData;
    DSM.dsm_NewDataFile = sa_MakeDataFile;
    DSM.dsm_NotifyFile = sa_Notify;
    DSM.dsm_GetSrcInfo = sa_GetSrcInfo;

    ds_InitAPI ();
/*
 * Open our lone source
 */
    if ((dir = getenv ("DS_DATA_DIR")) != NULL)
	strcpy (datadir, dir);
    else
	strcpy (datadir, GetDataDir());

    sprintf (cachefile, "%s/%s", datadir, "Zebra.cache");

    msg_ELog (EF_INFO, "Opening %s as source 'standalone'", datadir);
    Src = src_Open ("standalone", datadir, cachefile);

    return (TRUE);
}



/* ======================================================================
 * Standalone routines using data file access.
 */

static void	
sa_Notify (const Platform *p, const DataFile *df, int now, int nnew, 
	   int sample, int last)
/*
 * Update or add the given file in our source's list.
 */
{
    DataFile new_df = *df;
/*
 * Increment the rev, and update or add the file in our list
 */
    new_df.df_core.dfc_rev += 1;
    src_UpdateFile (Src, dt_FindPlatform (df->df_pid), &new_df.df_core);
/*
 * Let DFA know in case it has this file in its open file list
 */
    dfa_NoteRevision (&new_df);
}



static int
sa_MakeDataFile (PlatformId pid, const char *file, const ZebraTime *t, 
		 DataFile *df)
/*
 * The standalone equivalent of ds_RequestNewDF.  Fill in a new file
 * but don't insert it into the Source.
 */
{
	const Platform *p;

	if (! (p = dt_FindPlatform (pid)))
		return (0);

	if (! src_ConfirmDataDir (Src, p))
	{
	    msg_ELog (EF_PROBLEM, "Cannot find or create data dir %s for %s\n",
		      src_DataDir (Src, p), pi_Name (p));
	    return (0);
	}

	strcpy (df->df_core.dfc_name, file);
	df->df_core.dfc_ftype = pi_FileType (p);
	df->df_core.dfc_begin = df->df_core.dfc_end = *t;
	df->df_core.dfc_rev = 0;
	df->df_core.dfc_inode = 0;
	df->df_core.dfc_nsample = 0;

	sprintf (df->df_fullname, "%s/%s", src_DataDir (Src, p), file);
	df->df_pid = pid;
	df->df_srcid = 0;	/* We only have one Source... */

	return (1);
}		



static const DataFile*
sa_FindDFBefore (PlatformId pid, const ZebraTime *when, int srcid)
/*
 * Standalone version of ds_FindDFBefore.  Return the first file
 * which precedes or contains the given time.  Pointer is good until the 
 * next call here.  Since we only have one source in standalone mode,
 * we ignore the srcid.
 */
{
    static DataFile df;
    const Platform *p = dt_FindPlatform (pid);
    
    if (src_FindBefore (Src, p, when, &df.df_core))
    {
	df.df_pid = pid;
	df.df_srcid = 0;
	return (&df);
    }
    else
	return (0);
}



static const DataFile*
sa_FindDFAfter (PlatformId pid, const ZebraTime *when, int srcid)
/*
 * Standalone version of ds_FindDFAfter.  Return the first file
 * which begins at or after the given time.  Pointer is good until the 
 * next call here.  Since we only have one source in standalone mode,
 * we ignore the srcid.
 */
{
    static DataFile df;
    const Platform *p = dt_FindPlatform (pid);
    
    if (src_FindAfter (Src, p, when, &df.df_core))
    {
	df.df_pid = pid;
	df.df_srcid = 0;
	return (&df);
    }
    else
	return (0);
}


static int
sa_GetSrcInfo (int srcid, SourceInfo *si)
{
    if (srcid != 0)
	return 0;	/* we've only got one source... */

    si->src_Id = 0;
    strcpy (si->src_Name, src_Name (Src));
    strcpy (si->src_Dir, src_RootDir (Src));
    return (1);
}

    
    


static void
sa_DeleteObs (PlatformId pid, const ZebraTime *when)
{
/* empty */
}

static void
sa_DeleteData (PlatformId pid, const ZebraTime *when)
{
/* empty */
}
