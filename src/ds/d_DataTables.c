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
# include <stdio.h>
# include <string.h>

# include <defs.h>
# include <message.h>
/*
 * Implement our symbol tables for platform and class names with the
 * Zebra library module.  This does not affect using the full UI in
 * the daemon, since these tables are private to this module.
 */
# include <zl_symbol.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "Platforms.h"
# include "commands.h"
# include "dsDaemon.h"

RCSID("$Id: d_DataTables.c,v 3.24 2002-06-14 16:25:02 burghart Exp $")


/*
 * Declare global variables declared external in dsDaemon.h
 */
PlatformClass 	*CTable = NULL;
PlatformInstance *PTable = NULL;
DataFile 	*DFTable = NULL;

int NDTEUsed = 0;	/* How many data table entries used. 	*/
int DTFreeList = 0;	/* The datafile free list		*/
int NPlatform = 0;	/* How many platforms (instances)	*/
int NClass = 0;		/* Number of platform classes		*/


/*
 * We use these symbol tables to find platforms and classes quickly by name.
 */
static stbl Platforms = NULL;
static stbl Classes = NULL;

/*
 * Have we initialized things?  Initialization is delayed as long as possible
 * so that the allocation params can be set in the startup file.
 */
static int Initialized = FALSE;

/*-------------------------------------------------------------------------
 * Local forwards.
 */
static void dt_ExpandClassTable FP((int growth));
static void dt_InstantSubPlats FP((PlatformClass *pc, PlatformId parent));
static void dt_EraseSubPlats FP((PlatformInstance *plat));
static void dt_FillInstanceDirs FP((PlatformClass *pc, PlatformInstance *new,
				    const char *defname));
static PlatformInstance *dt_NewPlatform FP((char *name));
static void dt_ExpandPlatformTable FP((int growth));
static void dt_InitInstance FP((PlatformInstance *new, char *name));
static void dt_SetTableName FP((stbl table, const char *name, void *p));
static void dt_IPAdd FP((DataFile *df, int *link));

/*-------------------------------------------------------------------------*/



static void
dt_InitTables ()
/*
 * Initialize the data tables.
 */
{
	int i;

	Initialized = TRUE;
	msg_ELog (EF_DEBUG, "Initializing platform and class tables");
/*
 * Create the platform and data file tables.
 */
	CTable = (PlatformClass *) malloc (CTableSize*sizeof (PlatformClass));
	PTable = (PlatformInstance *) 
		malloc (PTableSize * sizeof (PlatformInstance));
	DFTable = (DataFile *) malloc (DFTableSize*sizeof (DataFile));
	NPlatform = NClass = NDTEUsed = 0;
	if (!CTable || !PTable || !DFTable)	/* uh-oh */
	{
		msg_ELog (EF_EMERGENCY, "%s: %s",
			  "memory alloc failure", 
			  "could not initialize platform or datafile tables");
		exit (9);
	}
/*
 * Create the symbol tables to hold the platform names and classes.
 */
	zl_usy_init ();
	Platforms = zl_c_stbl ("Platforms");
	Classes = zl_c_stbl ("Classes");
/*
 * Go through and build the free list.
 */
	DTFreeList = 1;	/* Waste first	*/
	for (i = 1; i < DFTableSize; i++)
	{
		DFTable[i].df_FLink = i + 1;
	}
	DFTable[DFTableSize - 1].df_FLink = 0;
}



int
dt_CheckId (id)
PlatformId id;
/*
 * Return non-zero if this is a valid platform id, zero otherwise.
 * Right now this just checks the range; someday it may check for
 * deleted or disabled platforms.
 */
{
	return ((id >= 0) && (id < NPlatform));
}



int
dt_CheckClassId (id)
PlatClassId id;
/*
 * Return non-zero if this is a valid platform class id, zero otherwise.
 * Right now this just checks the range; someday it may check for
 * deleted or disabled platform classes.
 */
{
	return ((id >= 0) && (id < NClass));
}



PlatformClass *
dt_NewClass (name, superclass)
const char *name;
const char *superclass;
/*
 * Given the name of the new class and a superclass, create a PlatformClass
 * structure.  If superclass non-NULL, inherit defaults from that class.
 * Returns NULL if the class cannot be created.
 */
{
	PlatformClass *pc;
	PlatformClass *spc;
/*
 * Make sure we're initialized.
 */
	if (! Initialized)
		dt_InitTables ();
/*
 * If this class already exists, return it.
 */
	if ((pc = dt_FindClass (name)) != NULL)
	{
		msg_ELog (EF_INFO, "WARNING: platform class '%s' redefined",
			  name);
		return (pc);
	}
/*
 * If we've reached the hardcoded limit, we must obey it.
 */
	if (NClass >= MAXPLAT)
	{
		static int max_warning = 0;
		if (! max_warning++)
			msg_ELog (EF_PROBLEM, "%s %d exceeded at '%s': %s",
				  "Max class count", MAXPLAT, (char *) name,
				  "further definitions ignored");
		return (NULL);
	}
/*
 * If the class table is full, expand it.
 */
	if (NClass >= CTableSize)
		dt_ExpandClassTable (CTableGrow);
/*
 * Allocate a new platform table entry.
 */
	pc = CTable + NClass;
	dt_SetTableName (Classes, name, pc);
	++NClass;
/*
 * If we have a superclass, copy it into the new class.
 */
	spc = NULL;
	if (superclass) 
	{
		spc = dt_FindClass (superclass);
		if (! spc)
		{
			msg_ELog (EF_PROBLEM,
				  "defining %s: superclass '%s' not defined",
				  name, superclass);
		}
	}
	if (spc) 
	{
		if (ParseOnly)
			printf("Subclass %s from %s\n", name, spc->dpc_name);
		dt_Subclass (spc - CTable, spc, pc, name);
	}
	else
		dt_InitClass (pc, name);
	return (pc);
}




static void
dt_ExpandClassTable (growth)
int growth;
/*
 * Expand class table by 'growth' entries.  UGLINESS: we need to go 
 * and reset all of the platform name entries in the symbol table since
 * they are pointers into the old array.  Expects the current number of
 * classes to be in NClass.
 */
{
	int i;

	if (NClass > MAXPLAT)
		return;
	CTableSize += growth;
	if (CTableSize > MAXPLAT)
		CTableSize = MAXPLAT;
	msg_ELog (EF_DEBUG, "Expanding CTable to %d entries", CTableSize);
	CTable = (PlatformClass *) realloc (CTable,
				    CTableSize * sizeof (PlatformClass));
	for (i = 0; i < NClass; i++)
		dt_SetTableName (Classes, CTable[i].dpc_name, CTable + i);
}




static void
dt_SetTableName (table, name, p)
stbl table;
const char *name;
void *p;
/*
 * Associate this name in table with pointer p.
 */
{
	SValue v;

	v.us_v_ptr = (char *) p;
	zl_s_symbol (table, (char *)name, SYMT_POINTER, &v);
}



static void
dt_SetPlatformNames (table, name, p)
stbl table;
const char *name;
void *p;
/*
 * Associate this name (and all subnames) with p.
 */
{
	const char *cp;
	SValue v;

	v.us_v_ptr = (char *) p;
	cp = name;
	do
	{
		zl_s_symbol (table, (char *) cp, SYMT_POINTER, &v);
		if ((cp = strchr (cp, '/')) != 0)
			cp++;
	}
	while (cp);
}




static PlatformInstance *
dt_NewPlatform (name)
char *name;
/*
 * Create a new platform instance in the list for the given name.
 * Return NULL if the platform cannot be created.
 */
{
	PlatformInstance *new;
/*
 * Make sure we're initialized.
 */
	if (! Initialized)
		dt_InitTables ();
/*
 * See if this guy already exists, and if so prepare it for re-conditioning
 */
	if ((new = dt_FindInstance (name)))
	{
		msg_ELog (EF_INFO, "WARNING: platform '%s' redefined", name);
		dt_EraseSubPlats (new);
		dt_InitInstance (new, name);
		return (new);
	}
/*
 * If we've reached our limit, disallow it.
 */
	if (NPlatform >= MAXPLAT)
	{
		static int max_warning = 0;
		if (! max_warning++)
			msg_ELog (EF_PROBLEM, "%s %d exceeded at '%s': %s",
				  "Max platform count", MAXPLAT, name,
				  "further definitions ignored");
		return (NULL);
	}
/*
 * If the platform table is full, expand it.
 */
	if (NPlatform >= PTableSize)
		dt_ExpandPlatformTable (PTableGrow);
/*
 * Allocate a new platform table entry.
 */
	new = PTable + NPlatform;
	dt_SetPlatformNames (Platforms, name, new);
	++NPlatform;
/*
 * Initialize the instance.  Note this is not the same as assigning defaults.
 * Defaults come later since they may be inherited or specified by the class.
 */
	dt_InitInstance (new, name);
	return (new);
}



static void
dt_InitInstance (new, name)
PlatformInstance *new;
char *name;
/*
 * Initialize this platform instance structure
 */
{
	dt_SetString (new->dp_name, name, sizeof(new->dp_name),
		      "assigning instance name");
	new->dp_class = BadClass;
	new->dp_parent = BadPlatform;

	new->dp_dir[0] = '\0';
	new->dp_rdir[0] = '\0';
	new->dp_LocalData = 0;
	new->dp_RemoteData = 0;
	new->dp_subplats = NULL;
	new->dp_nsubplats = 0;
	new->dp_flags = 0;
	new->dp_Tfile = 0;
	new->dp_NewSamps = 0;
	new->dp_OwSamps = 0;
	new->dp_RLockQ = NULL;
	new->dp_WLockQ = NULL;
}




static void
dt_ExpandPlatformTable (growth)
int growth;
/*
 * Grow the platform table by 'growth' entries.  UGLINESS: we need to go 
 * and reset all of the platform name entries in the symbol table since
 * they are pointers into the old array.  Expects NPlatform to hold the
 * current number of valid platforms.
 */
{
	int i;

	if (PTableSize >= MAXPLAT)
		return;
	PTableSize += growth;
	if (PTableSize > MAXPLAT)
		PTableSize = MAXPLAT;
	msg_ELog (EF_DEBUG, "Expanding PTable to %d entries", PTableSize);
	PTable = (PlatformInstance *) realloc (PTable,
		PTableSize * sizeof (PlatformInstance));
	for (i = 0; i < NPlatform; i++)
		dt_SetPlatformNames (Platforms, PTable[i].dp_name, PTable + i);
}



PlatformInstance *
dt_Instantiate (pc, parent, name)
PlatformClass *pc;	/* The platform class structure */
PlatformId parent;	/* The parent of the new instance or BadPlatform */
const char *name;	/* The name to be instantiated	*/
/*
 * Actually instantiate the given class from the platform instance 
 * structure, adjusting default directories and adding subplats.
 * If this is an instance from a 'platform' command, the name of the
 * class and the instance will be identical. 
 *
 * This code must be re-entrant!  It may be called recursively to instantiate
 * subplatforms defined in the platform's class.
 * 
 * Returns NULL if the platform cannot be created.
 */
{
	PlatformInstance *new;
	PlatformId newid;
	char iname[1024];
/*
 * Subplats must be given unique names for the symbol table by prefixing
 * the parent name.  If the parent name is also a subplatform, it will 
 * already contain its instance path name.  The defined name of this
 * subplatform will still be used for generating directory entries.
 */
	if (parent != BadPlatform)
		sprintf (iname, "%s/%s", pi_Name(PTable + parent), name);
	else
		strcpy (iname, name);
/*
 * Of course, we can't very well agree to instantiate an abstract base class
 */
	if (pc->dpc_flags & DPF_ABSTRACT)
	{
		msg_ELog (EF_PROBLEM, "class %s: abstract base class, %s %s",
			  pc->dpc_name, "cannot instantiate", name);
		return (NULL);
	}
/*
 * Create and initialize the platform instance in the platform table.
 */
	new = dt_NewPlatform (iname);
	if (! new)	/* so much for that idea */
		return (NULL);
	if (ParseOnly)
		printf ("IIIIIIII Instantiating %s (class %s)\n",
			iname, pc->dpc_name);
	newid = new - PTable;
/*
 * Set the stuff in the instance structure which comes from the class:
 * the directories, flags, and the automatic subplat instantiations.
 */
	new->dp_class = pc - CTable;
/*
 * This is not very orthodox, since many of the flags should remain
 * strictly separated between class and instance.  But for now they
 * shouldn't interfere with each other, and this will allow PlatformInstance
 * structures to serve as a replacement for Platform in most cases.
 */
	new->dp_flags = pc->dpc_flags;
	if (parent != BadPlatform)
	{
		new->dp_parent = parent;
	/*
	 * If the parent platform is a composite, then the subplat must be a
	 * subplatform (in the historic sense).  In any case, add a reference
	 * to this child to the parent.
	 */
		if (pi_Composite (PTable + parent))
		{
			new->dp_flags |= DPF_SUBPLATFORM;
			new->dp_flags &= ~DPF_COMPOSITE;
		}
		dt_AddSubPlat (PTable + parent, newid);
	}
/*
 * Fill our directory paths, using the definition name and not the full
 * instance path name for the default directory name
 */
	dt_FillInstanceDirs (pc, PTable + newid, name);
/*
 * Instantiate any subplatforms designated by the platform class
 */
	dt_InstantSubPlats (pc, newid);
/*
 * Our structure may have moved, so use the id to get a new reference
 */
	new = PTable + newid;
	if (ParseOnly)
	{
		dbg_DumpInstance (PTable + newid);
		printf ("IIIIIIII Done instantiating %s (class %s)\n",
			iname, pc->dpc_name);
	}
/*
 * In case of on-the-fly definitions, scan the platform directory
 */
	if ( !InitialScan && !pi_Subplatform(new) )
		RescanPlat (new);
	return (new);
}



static void
dt_FillInstanceDirs (pc, new, defname)
PlatformClass *pc;
PlatformInstance *new;
const char *defname;	/* Defined name, doesn't include parent if a subplat */
/* 
 * Perform the standard derivations of instance directories according to
 * the class settings.
 */
{
	PlatformInstance *parent;

	parent = pi_Parent (new);
	dt_FillDirs (pc, defname, new->dp_dir, new->dp_rdir,
		     (parent) ? parent->dp_dir : NULL,
		     (parent) ? parent->dp_rdir : NULL);

	if (new->dp_rdir[0] && !DisableRemote)
	{
		/*
		 * Only set the remote flag if the directory is accessible.
		 */
		if (access (new->dp_rdir, X_OK) == 0)
			new->dp_flags |= DPF_REMOTE;
		else
			msg_ELog (EF_DEBUG, "remote dir %s not accessible", 
				  new->dp_rdir);
	}
}




PlatformInstance *
dt_DefSubPlat (parent, spc, name)
PlatformId parent;		/* Parent of subplat to be instantiated */
PlatformClass *spc;		/* Class of subplat			*/
const char *name;		/* Name of subplat instance		*/
/*
 * Instantiate a platform of the given class, and make it a subplatform
 * of the given platform instance.  Don't allow platform to be instantiated
 * if another platform already exists with the same name.
 */
{
	PlatformInstance *sub;
	char iname[512];
/*
 * The unique symbol table lookup name of the subplat instance is prefixed 
 * by the parent name.
 */
	sprintf (iname, "%s/%s", pi_Name (PTable + parent), name);
	if ((sub = dt_FindInstance (iname)))
	{
		msg_ELog (EF_PROBLEM, "subplatform '%s' already exists",
			  sub->dp_name);
		return (sub);
	}
	sub = dt_Instantiate (spc, parent, name);
	return (sub);
}



static void
dt_InstantSubPlats (pc, newid)
PlatformClass *pc;
PlatformId newid;
/*
 * Loop through the subplats in the class and instantiate subplats for 
 * the new instance.
 *
 * Each subplat's parent will be the 'newid' platform.
 */
{
	int i;
	PlatformClass *spc;

	if (ParseOnly && pc->dpc_nsubplats)
	{
		printf ("Automatic subplats for instance %s (class %s)\n",
			(PTable + newid)->dp_name, pc->dpc_name);
		printf ("   Subplats:");
		for (i = 0; i < pc->dpc_nsubplats; ++i)
		{
			spc = CTable + pc->dpc_subplats[i].dps_class;
			printf (" {%s,%s}", spc->dpc_name,
				pc->dpc_subplats[i].dps_name);
		}
		printf ("\n");
	}
	for (i = 0; i < pc->dpc_nsubplats; ++i)
	{
		spc = CTable + pc->dpc_subplats[i].dps_class;
		dt_DefSubPlat (newid, spc, 
			       pc->dpc_subplats[i].dps_name);
	}
}




static void
dt_EraseSubPlats (plat)
PlatformInstance *plat;
/*
 * Remove the subplats array from this instance
 */
{
	if (plat->dp_subplats)
		free (plat->dp_subplats);
	plat->dp_subplats = NULL;
	plat->dp_nsubplats = 0;
}





PlatformInstance *
dt_FindInstance (name)
const char *name;
/*
 * Look up this platform instance in the platform table.
 */
{
	SValue v;
	int type;

	if (! Initialized)
		dt_InitTables ();
	if (ParseOnly)
		printf ("Looking for instance '%s'...", name);
	if (! zl_g_symbol (Platforms, (char *)name, &type, &v))
	{
		if (ParseOnly) printf ("not found.\n");
		return (0);
	}
	if (ParseOnly) printf ("found.\n");
	return ((PlatformInstance *) v.us_v_ptr);
}




PlatformInstance *
dt_FindPlatform (name)
const char *name;
/*
 * Look up this platform.
 */
{
	SValue v;
	int type;

	if (! Initialized)
		dt_InitTables ();
	if (! zl_g_symbol (Platforms, (char *)name, &type, &v))
	{
		return (0);
	}
	return ((PlatformInstance *) v.us_v_ptr);
}



PlatformClass *
dt_FindClass (name)
const char *name;
/*
 * Look up this platform class.
 */
{
	SValue v;
	int type;

	if (! Initialized)
		dt_InitTables ();
	if (! zl_g_symbol (Classes, (char *)name, &type, &v))
	{
		return (0);
	}
	return ((PlatformClass *) v.us_v_ptr);
}




void
dt_SearchPlatforms (function, req, pids, npids)
int (*function)();
struct dsp_PlatformSearch *req;
PlatformId *pids;
int *npids;
{
	if (! Initialized)
		dt_InitTables ();

	ds_SearchPlatTable (Platforms, function, req, pids, npids);
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
	int i, nsize;

	if (! Initialized)
		dt_InitTables ();
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
PlatformInstance *p;
int dfi;
/*
 * Remove this entry from the given platform and free it.  Don't forget to
 * send CacheInvalidate messages for all DFE's whose links we change.  This
 * is why it is important to do all deletions from the DF chain through
 * this function.  The notification of the removed DFE is presumably sent
 * separately in a DataGone message.
 */
{
	DataFile *df = DFTable + dfi;
/*
 * Remove the entry from the chain, free it, and we are done.
 */
	dt_CutDFE (p, dfi);
	dt_FreeDFE (df);
}



void
dt_CutDFE (p, dfi)
PlatformInstance *p;
int dfi;
/*
 * Remove this entry from the given platform chain but don't free it.
 * Send CacheInvalidate messages for all DFE's whose links we change.  This
 * is why it is IMPORTANT to do all deletions from the DF chain through
 * this function.  The notification of the removed DFE is presumably sent
 * separately in a DataGone message (if the DFE is actually being removed).
 * If the DFE is being re-sorted, its CacheInvalidate must be sent elsewhere
 * also.
 */
{
	DataFile *df = DFTable + dfi;

	ClearLocks (p);
/*
 * See if it is at the top of a list.  Though the platform structure
 * changes here, the client code is responsible for making sure the
 * platform structure caches are up-to-date as needed.
 */
	if (dfi == p->dp_LocalData)
		p->dp_LocalData = df->df_FLink;
	else if (dfi == p->dp_RemoteData)
		p->dp_RemoteData = df->df_FLink;
/*
 * Nope.  Adjust backward links.
 */
	else
	{
		DFTable[df->df_BLink].df_FLink = df->df_FLink;
		CacheInvalidate ( df->df_BLink );
	}
/*
 * Now adjust the backward links of the following DFI, if there is one.
 */
	if (df->df_FLink)
	{
		DFTable[df->df_FLink].df_BLink = df->df_BLink;
		CacheInvalidate ( df->df_FLink );
	}
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




static void
dt_IPAdd (df, link)
DataFile *df;
int *link;
/*
 * The internal platform add.
 */
{
	DataFile *chain = NULL, *last = NULL;
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
		df->df_BLink = chain->df_BLink;
		if (chain->df_BLink)
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
PlatformInstance *p;
DataFile *df;
int local;
/*
 * Add a new data file to the given platform.
 */
{
	df->df_platform = p - PTable;
	/* df->df_flags = DFF_Seen; */
	ClearLocks (p);
	if (local)
		dt_IPAdd (df, &p->dp_LocalData);
	else
		dt_IPAdd (df, &p->dp_RemoteData);
}




void
dt_SortDFE (p, df, local)
PlatformInstance *p;
DataFile *df;
int local;
/*
 * Move an existing data file to its correct spot in the chain while
 * disturbing as little as possible (as few CacheInvalidates as possible).
 */
{
	int ok;
	DataFile *next, *prev;

	ClearLocks (p);
/*
 * First see if the file entry is still o.k. where it is
 */
	prev = DFTable + df->df_FLink;
	next = DFTable + df->df_BLink;
	ok = ((!df->df_FLink) || TC_Less (prev->df_end, df->df_begin));
	ok = ok && ((!df->df_BLink) || TC_Less (df->df_end, next->df_begin));
	if (!ok)
	{
		int dfi = df - DFTable;
	/*
	 * Bummer, we have to move the file entry.  Just take it out of the
	 * chain altogether and then re-add it.  These functions will take
	 * care of invalidating caches for changed links for all files but
	 * the re-sorted DFE.
	 */
		dt_CutDFE (p, dfi);
		if (local)
			dt_IPAdd (df, &p->dp_LocalData);
		else
			dt_IPAdd (df, &p->dp_RemoteData);
	}
}




void
dt_ClientPlatform (pi, p)
PlatformInstance *pi;
ClientPlatform *p;
/*
 * Merge the platform instance and its parent class into a
 * Platform structure suitable for sending to a client.
 */
{
	PlatformClass *pc = pi_Class(pi);

	strcpy (p->cp_name, pi->dp_name);
	p->cp_class = pi->dp_class;
	strcpy (p->cp_dir, pi->dp_dir);
	strcpy (p->cp_rdir, pi->dp_rdir);
	p->cp_parent = pi->dp_parent;
	p->cp_flags = pi->dp_flags;
	p->cp_LocalData = pi->dp_LocalData;
	p->cp_RemoteData = pi->dp_RemoteData;
	p->cp_org = pc->dpc_org;
	p->cp_ftype = pc->dpc_ftype;
	p->cp_keep = pc->dpc_keep;
	p->cp_maxsamp = pc->dpc_maxsamp;
}




char *
dt_DFEFilePath (pi, df)
Platform *pi;
DataFile *df;
/*
 * Generate the full name of this data file.  The name is returned in
 * static space and will get zapped with the next call.
 */
{
	static char fname[sizeof(pi->dp_dir)+sizeof(df->df_name)+10];

	sprintf (fname, "%s/%s", (df->df_flags & DFF_Remote) ?
		pi->dp_rdir : pi->dp_dir, df->df_name);
	return (fname);
}

