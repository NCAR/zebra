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

RCSID ("$Id: SA_Appl.c,v 3.2 1996-11-27 02:21:30 granger Exp $")


static int DFNext = 1;	/* Next df index to use when standalone */

/*
 * Keep track of the highest IDs so far
 */
static int MaxPlatform = 0;
static int MaxClass = 0;

/*
 * Prototypes for stand-alone routines
 */
static void	ds_NotifyLocal FP ((ClientPlatform *p, int dfile, 
				    DataChunk *dc, int now, int nnew, 
				    int sample, int last));
static int	ds_MakeDataFile FP ((PlatformId, char *file, ZebTime *t));
static void	ds_AddFile FP ((DataFile *df, int *link));
static PlatformId ds_ClientPlatform FP ((PlatClassId cid, const char *name,
					 PlatformId parent));
static void	ds_ClientSubPlats FP ((const PlatformClass *, 
				       PlatformId parent));
static PlatClassId ds_ClientDefineClass FP ((PlatformClass *pc));
static int	ds_LocalAfter FP ((PlatformId pid, const ZebTime *when));
static int	ds_LocalBefore FP ((PlatformId pid, const ZebTime *when,
				    int src));
static int	ds_MaxPlat FP ((void));
static void	ds_LocalSearch FP ((PlatformSearch *search, PlatformList *pl));


static void
ds_NoOP ()
{
       return;
}


int
ds_Standalone()
/*
 * Set up local structures WITHOUT connecting to the datastore.
 * DataStore info will be supplanted by the application and
 * internal standalone routines.
 */
{
/*
 * Set standalone mode and change our methods.
 */
	Standalone = 1;
	DSM.dsm_FindBefore = ds_LocalBefore;
	DSM.dsm_FindAfter = ds_LocalAfter;
	DSM.dsm_DeleteObs = ds_NoOP;
	DSM.dsm_DeleteData = ds_NoOP;
	DSM.dsm_DefineClass = ds_ClientDefineClass;
	DSM.dsm_DefinePlatform = ds_ClientPlatform;
	DSM.dsm_NewDataFile = ds_MakeDataFile;
	DSM.dsm_NotifyFile = ds_NotifyLocal;
	DSM.dsm_NPlat = ds_MaxPlat;
	DSM.dsm_SearchPlatforms = ds_LocalSearch;
	ds_InitAPI ();
/*
 * Until we have a handle on dynamically optimizing cache size,
 * just create a very large one. 
 */
	ds_CreateFileCache (5 * MAX_DF_CACHE);
	return (TRUE);
}




static int
ds_MaxPlat ()
{
	return (MaxPlatform);
}




static int
MatchPlatform (symbol, type, value, info)
char *symbol;
int type;
union usy_value *value;
struct SearchInfo *info;
/*
 * Use the request structure to see if there is a match with this
 * platform.  Counterpart to Daemon's MatchPlatform function, but it
 * uses ClientPlatform structures.
 */
{
	PlatformId pid = value->us_v_int;
	struct dsp_PlatformSearch *req = info->si_req;
	ClientPlatform *cp;

	if (! (cp = ds_GetPlatStruct (pid, NULL, FALSE)))
		return (TRUE);
	if (strlen(cp->cp_name) != strlen(symbol))
		return (TRUE);
	if (!req->dsp_subplats && (cp->cp_flags & DPF_SUBPLATFORM))
		return (TRUE);
	if ((req->dsp_children) && (cp->cp_parent != req->dsp_parent))
		return (TRUE);
	if ((req->dsp_regexp[0] == '\0') || (zl_re_exec (cp->cp_name)))
	{
		info->si_pids[(info->si_npids)++] = pid;
	}
	return (TRUE);
}





static void
ds_LocalSearch (req, pl)
PlatformSearch *req;
PlatformList *pl;
{
	int n = ds_GetNPlat();

	if (n > 0)
	{
		PlatformId *pids;
		int npids;
		pids = (PlatformId *) malloc (n * sizeof(PlatformId));

		ds_SearchPlatTable ((stbl) ds_PlatTable(), MatchPlatform, 
				    req, pids, &npids);
		if (npids == 0)
			free (pids);
		else
		{
			pl->pl_pids = pids;
			pl->pl_npids = npids;
		}
	}
}



/* ======================================================================
 * Standalone routines using data file access.
 */


static void
ds_NotifyLocal (p, dfile, dc, now, nnew, sample, last)
ClientPlatform *p;
int dfile;
DataChunk *dc;
int now, nnew, sample, last;
/*
 * If standalone, get a copy of this datafile and update it ourselves.
 * If this is the first time samples have been added to the file, then
 * add it to the platform chain.  Likewise update the file's end time.
 */
{
	DataFile *df;
	ZebTime end;

	df = ds_SearchCache (dfile);
	if (! df)
	{
		msg_ELog (EF_PROBLEM, "standalone notify file %d failed: %s",
			  dfile, "not in cache");
		return;
	}
	dc_GetTime (dc, sample, &end);
	if ((df->df_nsample == 0) || TC_Less(df->df_end, end))
		df->df_end = end;
	if (df->df_nsample == 0)
	{
		ds_AddFile (df, &p->cp_LocalData);
		ds_CachePlatform (df->df_platform, p);
	}
	df->df_nsample += nnew;
	df->df_rev += 1;
	dfa_NoteRevision (df->df_index, df->df_rev);
}



static void
ds_AddFile (df, link)
DataFile *df;
int *link;
/*
 * Add a data file to a platform chain.  Taken from dt_IPAdd on the
 * daemon side.
 */
{
	DataFile *chain = NULL;
	DataFile *last = NULL;
	int index;
/*
 * See if, by chance, this is the easy case.
 */
	if (! *link)
	{
		df->df_FLink = df->df_BLink = 0;
		*link = df->df_index;
		return;
	}
/* 
 * No such luck.  Scan for the place to put this entry.
 */
	for (index = *link; index; index = chain->df_FLink)
	{
		chain = ds_SearchCache (index);
		if (! chain)
			return;
		if (TC_Less (chain->df_begin, df->df_begin))
			break;
		else
			last = chain;
	}
/*
 * If there is still a chain value, then we insert this entry before.  
 * Otherwise it goes at the end.
 */
	if (index)
	{
	/*
	 * Fix the adjoining links.
	 */
		df->df_BLink = chain->df_BLink;
		if (chain->df_BLink)
		{
			DataFile *back;
			back = ds_SearchCache (chain->df_BLink);
			if (! back)
				return;
			back->df_FLink = df->df_index;
		}
		else
			*link = df->df_index;
		chain->df_BLink = df->df_index;
	 	df->df_FLink = chain->df_index;
	}
	else
	{
		last->df_FLink = df->df_index;
		df->df_BLink = last->df_index;
		df->df_FLink = 0;
	}
}




/* ======================================================================
 * Standalone routines for class and platform definition
 */


static PlatClassId
ds_ClientDefineClass (pc)
PlatformClass *pc;
/*
 * Add this class structure to our local cache and return its id.
 */
{
	PlatClassId cid;

	if (MaxClass < MAXPLAT)
	{
		cid = MaxClass++;
		ds_CacheClass (cid, pc);
	}
	else
	{
		msg_ELog (EF_PROBLEM, "cannot define %s: more than %d classes",
			  pc->dpc_name, MAXPLAT);
		cid = BadClass;
	}
	return (cid);
}



static PlatformId
ds_ClientPlatform (cid, name, parent)
PlatClassId cid;
const char *name;
PlatformId parent;
/*
 * Instantiate this class in our cache table and return the id.  The memory
 * pointed to by pc does not need to last beyond this call.  If an instance
 * by this name already exists, this new instance will take precedence.
 */
{
	const PlatformClass *pc;
	PlatformId pid;
	ClientPlatform cp;
	ClientPlatform *pp = NULL;
	char iname[1024];

	pc = ds_GetClassStruct (cid, NULL);
	if (! pc)
	{
		msg_ELog (EF_PROBLEM, "define %s: could not find class %d",
			  (char *)name, cid);
		return (BadPlatform);
	}
/*
 * Of course, we can't very well agree to instantiate an abstract base class
 */
	if (pc->dpc_flags & DPF_ABSTRACT)
	{
		msg_ELog (EF_PROBLEM, "class %s: abstract base class, %s %s",
			  pc->dpc_name, "cannot instantiate", name);
		return (BadPlatform);
	}

	cp.cp_parent = parent;
	cp.cp_flags = pc->dpc_flags;
	strcpy (iname, name);
	if (parent != BadPlatform)
	{
		if ((pp = ds_GetPlatStruct (parent, NULL, FALSE)) == NULL)
		{
			msg_ELog (EF_PROBLEM, "define %s: no parent %d",
				  (char *)name, parent);
			cp.cp_parent = BadPlatform;
		}
		else
		{
			/*
			 * If the parent platform is a composite, then the
			 * subplat must be a subplatform.
			 */
			if (pp->cp_flags & DPF_COMPOSITE)
			{
				cp.cp_flags |= DPF_SUBPLATFORM;
				cp.cp_flags &= ~DPF_COMPOSITE;
			}
			/*
			 * Subplats must be given unique names for the
			 * symbol table by prefixing the parent name.
			 */
			sprintf (iname, "%s/%s", pp->cp_name, name);
		}
	}
	/*
	 * Copy or initialize the rest of the client platform structure.
	 */
	dt_SetString(cp.cp_name, iname, sizeof(cp.cp_name),"client platform");
	cp.cp_class = cid;
	cp.cp_dir[0] = '\0';
	cp.cp_rdir[0] = '\0';
	dt_FillDirs (pc, name, cp.cp_dir, cp.cp_rdir,
		     (pp) ? pp->cp_dir : NULL, (pp) ? pp->cp_rdir : NULL);
	cp.cp_org = pc->dpc_org;
	cp.cp_ftype = pc->dpc_ftype;
	cp.cp_keep = pc->dpc_keep;
	cp.cp_maxsamp = pc->dpc_maxsamp;
	cp.cp_LocalData = 0;
	cp.cp_RemoteData = 0;
	/*
	 * Only set the remote flag if the directory is accessible.
	 */
	cp.cp_flags &= ~DPF_REMOTE;
	if (cp.cp_rdir[0] && ! DisableRemote)
	{
		if (access (cp.cp_rdir, X_OK) == 0)
			cp.cp_flags |= DPF_REMOTE;
		else
			msg_ELog (EF_DEBUG, "remote dir %s not accessible", 
				  cp.cp_rdir);
	}
	/*
	 * Assign IDs beginning with 0, just like the daemon.
	 */
	pid = MaxPlatform++;
	/*
	 * Cache the client platform structure and name.
	 */
	ds_CachePlatform (pid, &cp);
	/* ds_CacheName (iname, pid); */
	/*
	 * Instantiate any subplatforms designated by the platform class
	 */
	ds_ClientSubPlats (pc, pid);
	return (pid);
}




static void
ds_ClientSubPlats (pc, parent)
const PlatformClass *pc;
PlatformId parent;
/*
 * Loop through the subplats in the class and instantiate subplats for 
 * the new instance.
 *
 * Each subplat's parent will be the 'parent' platform.
 */
{
	int i;
	const PlatformClass *spc;
	PlatClassId cid;

	for (i = 0; i < pc->dpc_nsubplats; ++i)
	{
		cid = pc->dpc_subplats[i].dps_class;
		spc = ds_GetClassStruct (cid, NULL);
		if (! spc)
		{
			msg_ELog (EF_PROBLEM, "%s %s: class %d not found",
				  "instantiate subplat",
				  pc->dpc_subplats[i].dps_name,
				  pc->dpc_subplats[i].dps_class);
		}
		else
		{
			ds_ClientPlatform (cid, pc->dpc_subplats[i].dps_name, 
					   parent);
		}
	}
}




static int
ds_MakeDataFile (plat, file, t)
PlatformId plat;
char *file;
ZebTime *t;
/*
 * The standalone equivalent of ds_RequestNewDF.  Create a new file
 * with an index and cache it, but don't insert it into the platform
 * chain yet.
 */
{
	DataFile *new;
	ClientPlatform p;
	int dfi;

	if (! ds_GetPlatStruct (plat, &p, FALSE))
		return (0);
	if (! (p.cp_flags & DPF_DIREXISTS))
		dt_CreateDataDir (p.cp_dir, p.cp_name, &p.cp_flags);
	dfi = DFNext++;
	new = ds_CacheFile (NULL);
	new->df_index = dfi;
	strcpy (new->df_name, file);
	new->df_ftype = p.cp_ftype;
	new->df_begin = new->df_end = *t;
	new->df_rev = 0;
	new->df_inode = 0;
	new->df_FLink = new->df_BLink = 0;
	new->df_nsample = 0;
	new->df_platform = plat;
	new->df_flags = 0;
	return (dfi);
}		



static int
ds_LocalBefore (pid, when, src)
PlatformId pid;
const ZebTime *when;
int src;
/*
 * Standalone version of ds_FindDF (ds_FindBefore).  Return the first file
 * which precedes or contains the given time.
 */
{
	ClientPlatform *cp;
	DataFile df;
	int dfe;

	if (! (cp = ds_GetPlatStruct (pid, 0, FALSE)))
		return (-1);
	dfe = ds_DataChain (cp, 0);
	while (dfe)
	{
		if (! ds_GetFileStruct (dfe, &df))
			return (-1);
		if (TC_LessEq (df.df_begin, *when))
			return (dfe);
		dfe = df.df_FLink;
	}
	return (-1);
}



static int
ds_LocalAfter (pid, when)
PlatformId pid;
const ZebTime *when;
{
	ClientPlatform *cp;
	int dfe;
	DataFile df;
	int last = 0;
/*
 * Search the local list for the first DFE which begins before the given
 * time.  Then take either that file or the succeeding one, depending on
 * whether the earlier file contains the target time.
 */
	if (! (cp = ds_GetPlatStruct (pid, 0, FALSE)))
		return (-1);
	dfe = ds_DataChain (cp, 0);
	while (dfe)
	{
		if (! ds_GetFileStruct (dfe, &df))
			return (-1);
		if (TC_LessEq (df.df_begin, *when))
			break;
		last = dfe;
		dfe = df.df_FLink;
	}
	if (! dfe || TC_Less (df.df_end, *when))
		dfe = last;
	return ((dfe) ? dfe : -1);
}



