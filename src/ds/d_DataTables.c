/*
 * Maintenance of the data tables.
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

# include <unistd.h>
# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "commands.h"
# include "dsDaemon.h"
MAKE_RCSID("$Id: d_DataTables.c,v 3.9 1993-08-12 18:25:46 granger Exp $")


/*
 * We use this symbol table to find platforms quickly by name.
 */
static stbl Platforms;

/*
 * What is the default time period to keep data?
 */
static int DefaultKeep = 60;

/*
 * Have we initialized things?  Initialization is delayed as long as possible
 * so that the allocation params can be set in the startup file.
 */
static int Initialized = FALSE;



void
dt_InitTables ()
/*
 * Initialize the data tables.  Assumes that the shared memory segment
 * is already in place.
 */
{
	int i;

	Initialized = TRUE;
/*
 * Create the platform and data file tables.
 */
	PTable = (Platform *) malloc (PTableSize*sizeof (Platform));
	DFTable = (DataFile *) malloc (DFTableSize*sizeof (DataFile));
	NPlatform = NDTEUsed = 0;
/*
 * Create the symbol table to hold the platform names.
 */
	Platforms = usy_c_stbl ("Platforms");
/*
 * Make DefaultKeep available.
 */
	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "defaultkeep",
		&DefaultKeep, SYMT_INT, 0);
/*
 * Go through and build the free list.
 */
	DTFreeList = 1;	/* Waste first	*/
	for (i = 1; i < DFTableSize; i++)
	{
		DFTable[i].df_FLink = i + 1;
		DFTable[i].df_use = 0;
	}
	DFTable[DFTableSize - 1].df_FLink = 0;
}





static void
dt_SetNames (name, p)
char *name;
Platform *p;
/*
 * Associate this name (and all subnames) with p.
 */
{
	char *cp, *strchr ();
	SValue v;

	v.us_v_ptr = (char *) p;
	cp = name;
	do
	{
		usy_s_symbol (Platforms, cp, SYMT_POINTER, &v);
		if (cp = strchr (cp, '/'))
			cp++;
	}
	while (cp);
}





Platform *
dt_NewPlatform (name)
char *name;
/*
 * Add this platform to the list.
 */
{
	Platform *new;
	int i;
/*
 * Make sure we're initialized.
 */
	if (! Initialized)
		dt_InitTables ();
/*
 * See if this guy already exists.
 */
	if (new = dt_FindPlatform (name, TRUE))
	{
		msg_ELog (EF_INFO, "WARNING: platform '%s' redefined", name);
		new->dp_flags = 0;
		return (new);
	}
# ifdef notdef
/*
 * Nope.  However, if we're done with defining platforms, we must gripe
 * severely.
 */
	if (ShmHeader->sm_nDataTable)
	{
		msg_ELog (EF_EMERGENCY, "PANIC: new platform after closure");
		Shutdown ();
	}
# endif
/*
 * If the platform table is full expand it.  UGLINESS: we need to go 
 * and reset all of the platform name entries in the symbol table since
 * they are pointers into the old array.
 */
	if (++NPlatform >= PTableSize)
	{
		PTableSize += PTableGrow;
		msg_ELog (EF_INFO, "Expanding PTable to %d", PTableSize);
		PTable = (Platform *) realloc (PTable,
			PTableSize*sizeof (Platform));
		for (i = 0; i < NPlatform - 1; i++)
			dt_SetNames (PTable[i].dp_name, PTable + i);
	}
/*
 * Allocate a new platform table entry.
 */
	new = PTable + NPlatform - 1;
	dt_SetNames (name, new);
/*
 * Fill it in and return it.
 */
	strcpy (new->dp_name, name);
	sprintf (new->dp_dir, "%s/%s", DefDataDir, name);
	new->dp_Tfile = new->dp_flags = 0;
	new->dp_org = OrgUnknown;
	new->dp_ftype = FTUnknown;
	new->dp_keep = DefaultKeep;
	new->dp_maxsamp = 60;
	new->dp_LocalData = new->dp_RemoteData = 0;
	new->dp_RLockQ = new->dp_WLockQ = 0;
/*
 * Consider automatically establishing a remote data dir.
 */
	if (! DisableRemote || RemDataDir[0] != '\0')
	{
		sprintf (new->dp_rdir, "%s/%s", RemDataDir, name);
		if (! access (new->dp_rdir, X_OK))
			new->dp_flags |= DPF_REMOTE;
	}
	return (new);
}




Platform *
dt_FindPlatform (name, full)
char *name;
int full;
/*
 * Look up this platform.
 */
{
	SValue v;
	int type;

	if (! usy_g_symbol (Platforms, name, &type, &v))
		return (0);
	return ((Platform *) v.us_v_ptr);
}








DataFile *
dt_NewFile ()
/*
 * Return a free data file entry.  NULL if none remain.  Once returned
 * from dt_NewFile, the entry is "loose", and must be hooked back in with
 * either dt_FreeDFE() or dt_AddToPlatform (), or it will be lost forever.
 */
{
	DataFile *ret;
	int avail, i, nsize;
/*
 * If the free list is empty, it means we have to expand the table.
 */
	if (DTFreeList == 0)
	{
	/*
	 * Allocate a larger table.
	 */
		nsize = DFTableSize + DFTableGrow;
		msg_ELog (EF_INFO, "Expanding DFTable to %d", nsize);
		DFTable = (DataFile *) realloc (DFTable,
				nsize*sizeof (DataFile));
	/*
	 * Build a new free list out of the new entries.
	 */
		DTFreeList = DFTableSize;
		for (i = DFTableSize; i < nsize; i++)
		{
			DFTable[i].df_FLink = i + 1;
			DFTable[i].df_use = 0;
		}
		DFTable[nsize - 1].df_FLink = 0;
		DFTableSize = nsize;
	}
/*
 * OK, pull one out.
 */
	ret = DFTable + DTFreeList;
	DTFreeList = ret->df_FLink;
	NDTEUsed++;
/*
 * Give it back to them.
 */
	ret->df_FLink = 0;
	ret->df_rev = 1;
	ret->df_index = ret - DFTable;
	ret->df_flags = 0;
	return (ret);
}





void
dt_RemoveDFE (p, dfi)
Platform *p;
int dfi;
/*
 * Remove this entry from the given platform and free it.
 */
{
	DataFile *df = DFTable + dfi;

	ClearLocks (p);
/*
 * See if it is at the top of a list.
 */
	if (dfi == p->dp_LocalData)
		p->dp_LocalData = df->df_FLink;
	else if (dfi == p->dp_RemoteData)
		p->dp_RemoteData = df->df_FLink;
/*
 * Nope.  Adjust backward links.
 */
	else
		DFTable[df->df_BLink].df_FLink = df->df_FLink;
/*
 * Now adjust the backward links of the following DFI, if there is one.
 */
	if (df->df_FLink)
		DFTable[df->df_FLink].df_BLink = df->df_BLink;
/*
 * The entry is now out of the chain.  Free it and we are done.
 */
	dt_FreeDFE (df);
}






void
dt_FreeDFE (df)
DataFile *df;
/*
 * Add this df to the free list.  It is assumed that DF does not exist
 * in any other lists.
 */
{
	NDTEUsed--;
	df->df_FLink = DTFreeList;
	DTFreeList = df - DFTable;
}




void
dt_IPAdd (df, link)
DataFile *df;
int *link;
/*
 * The internal platform add.
 */
{
	DataFile *chain, *last;
	int index;
/*
 * See if, by chance, this is the easy case.
 */
	if (! *link)
	{
		df->df_FLink = df->df_BLink = 0;
		*link = df - DFTable;
		/* msg_ELog (EF_DEBUG, "AF %s, empty", df->df_name); */
		return;
	}
/* 
 * No such luck.  Scan for the place to put this entry.
 */
	for (index = *link; index; index = DFTable[index].df_FLink)
	{
		chain = DFTable + index;
		if (TC_Less (chain->df_begin, df->df_begin))
			break;
		else
			last = chain;
	}
/*
 * If there is still a chain value, then we insert this entry before.  
 * Otherwise it goes at the end.
 *
 * Note that we invalidate cache entries for DFE's whose links have 
 * changed, but not for the new entry.  That, we assume, is being done
 * elsewhere.
 */
	if (index)
	{
	/*
	 * Fix the adjoining links.
	 */
		if (df->df_BLink = chain->df_BLink)
		{
			DFTable[chain->df_BLink].df_FLink = df - DFTable;
			CacheInvalidate (chain->df_BLink);
		}
		else
			*link = df - DFTable;
		chain->df_BLink = df - DFTable;
	 	df->df_FLink = chain - DFTable;
		CacheInvalidate (chain - DFTable);
	}
	else
	{
		last->df_FLink = df - DFTable;
		df->df_BLink = last - DFTable;
		df->df_FLink = 0;	
		CacheInvalidate (last - DFTable);
	}
# ifdef notdef
	msg_ELog (EF_DEBUG, "AF %s, links %d %d", df->df_name, df->df_FLink,
		df->df_BLink);
# endif
}







void
dt_AddToPlatform (p, df, local)
Platform *p;
DataFile *df;
bool local;
/*
 * Add this data file to the given platform.
 */
{
	df->df_platform = p - PTable;
	df->df_use++;
	/* df->df_flags = DFF_Seen; */
	ClearLocks (p);
	if (local)
		dt_IPAdd (df, &p->dp_LocalData);
	else
		dt_IPAdd (df, &p->dp_RemoteData);
}
