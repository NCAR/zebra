/*
 * Platform class and instance tables.  The public interface defined here
 * is shared by both client and daemon, but only within the internal daemon
 * and library implementation.  Routines meant for public application
 * consumption, which are built on the routines here, are defined in
 * p_Appl.c and Appl.c.
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
# include <config.h>		/* CFG_MAX_PLATFORMS */
# include <message.h>
/*
 * Implement our symbol tables for platform and class names with the
 * Zebra library module.  This does not affect using the full UI in
 * the daemon, since these tables are private to this module.
 */
# include <zl_symbol.h>
# include <zl_regex.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "Platforms.h"
# include "dsDaemon.h"

RCSID("$Id: p_Table.c,v 3.2 2002-06-14 16:25:02 burghart Exp $")

/*
 * Memory allocation options.  External so the daemon can associate them
 * with UI indirect variables.
 */
int PTableSize = 200;	/* Platform table initial size	*/
int PTableGrow = 50;	/* Amount to grow by		*/
int CTableSize = 100;	/* Class table initial size	*/
int CTableGrow = 50;	/* Amount to grow by		*/

/*
 * Our method structure to insert client-side functions when needed.
 * On the daemon side this will always be NULL so that the client-side
 * routines which send messages to the daemon will never be linked.
 */
static PlatformMethods *Client = NULL;

/*
 * Counters for our tables.  In the client case, these are the known
 * number (i.e. highest cached), not necessarily in sync with the daemon.
 */
static int NPlatform = 0;	/* How many platforms (instances)	*/
static int NClass = 0;		/* Number of platform classes		*/

/*
 * The tables which contain pointers to each platform and class structure.
 * These are the id->structure mappings.
 */
static PlatformClass 	**CTable = NULL;
static PlatformInstance **PTable = NULL;

/*
 * We use these symbol tables to find platform and class id's quickly by name.
 * The id can then be mapped to a structure.
 */
static stbl Platforms = NULL;
static stbl Classes = NULL;

/*
 * Have we initialized things?  Initialization is delayed as long as possible
 * so that the allocation params can be set by an application configuration.
 */
static int Initialized = FALSE;

/*
 * Search criteria in a convenient package...
 */
struct SearchInfo {
	PlatformSearch *si_req;
	PlatformId *si_pids;
	int si_npids;
};

/*-------------------------------------------------------------------------
 * Local forwards.
 */
static PlatformClass *dt_AddClass (PlatformClass *, PlatClassId);
static PlatformInstance *dt_AddPlatform (PlatformInstance *, PlatformId);
static PlatformInstance *dt_Instantiate (const PlatformClass *,
					 const char *name, PlatformId parent);
static void dt_CachePlatformNames (const char *name, PlatformId pid);
static void dt_CacheClassName (const char *name, PlatClassId cid);
static void dt_DeletePlatform (PlatformInstance *pi);
static void dt_ExpandClassTable (int min);
static void dt_InstantSubPlats (const PlatformClass *pc, Platform *pp);
static const PlatformInstance* dt_DefSubPlat (Platform *pp, 
					      const PlatformClass *spc, 
					      const char *name);
static void dt_ExpandPlatformTable (int min);
static void dt_ListSearch (struct dsp_PlatformSearch *req, PlatformList *pl);
static void dt_SearchPlatforms (struct dsp_PlatformSearch *req,
				PlatformId *pids, int *npids);
static void dt_InitTables (void);
static int MatchPlatform (char *symbol, int type, union usy_value *value,
			  struct SearchInfo *info);

/*-------------------------------------------------------------------------*/


/*
 * Set the client methods the table module will use.
 */
void
dt_SetMethods (PlatformMethods *pm)
{
	Client = pm;
}


/* ================
 * Public interface for mapping names and identifiers to platform pointers,
 * and for mapping names to ids.
 * ----------------
 * Platform structure retrieval.  The returned pointers are const because
 * they belong to the underlying implementation and should not be modified
 * or free by the application.
 */

const Platform *
dt_FindPlatform (PlatformId id)
/*
 * See if this platform for this id is already in the
 * table.  If not, either it is an invalid platform or
 * we need to ask the daemon.
 */
{
	Platform *p = NULL;
/*
 * Make sure we're initialized.
 */
	if (! Initialized)
		dt_InitTables ();
/*
 * See if we have this one already, otherwise get the info from the daemon
 */
	if (id < 0 || (!Client && id >= NPlatform))
		p = NULL;
	else if (id < NPlatform && PTable[id])
		p = PTable[id];
	else if (Client)
	{
		if ((p = (*Client->PlatStruct) (id, NULL)))
			p = dt_AddPlatform (p, id);
	}
	return (p);
}



const PlatformInstance *
dt_FindPlatformName (name)
const char *name;
/*
 * Look up this platform by name.
 */
{
	SValue v;
	int type;
	Platform *p = NULL;

	if (Platforms && zl_g_symbol (Platforms, (char *)name, &type, &v))
	{
		p = (Platform *) dt_FindPlatform (v.us_v_int);
	}
	else if (Client)
	{
		if ((p = (*Client->PlatStruct) (BadPlatform, name)))
			p = dt_AddPlatform (p, p->dp_id);
	}
	return (p);
}



const PlatformClass *
dt_FindClass (PlatClassId id)
{
	PlatformClass *pc = NULL;

	if (! Initialized)
		dt_InitTables ();

	if (id < 0 || (!Client && id >= NClass))
		pc = NULL;
	else if (id < NClass && CTable[id])
		pc = CTable[id];
	else if (Client)
	{
		if ((pc = (*Client->ClassStruct) (id, NULL)))
			pc = dt_AddClass (pc, id);
	}
	return (pc);
}





const PlatformClass*
dt_FindClassName (name)
const char *name;
/*
 * Look up this platform class.
 */
{
	SValue v;
	int type;
	PlatformClass *pc = NULL;

	if (! Initialized)
		dt_InitTables ();

	if (Classes && zl_g_symbol (Classes, (char *)name, &type, &v))
	{
		pc = (PlatformClass *) dt_FindClass (v.us_v_int);
	}
	else if (Client)
	{
		if ((pc = (*Client->ClassStruct) (BadClass, name)))
			pc = dt_AddClass (pc, pc->dpc_id);
	}
	return (pc);
}



int
dt_NPlatform (void)
{
	int nplat;

	if (! Initialized)
		dt_InitTables ();

	if (Client)	/* always need to ask daemon, in case it's changed */
		nplat = (*Client->GetNPlat)();
	else
		nplat = NPlatform;

	if (nplat > NPlatform)
	{
		/*
		 * May as well make room now for as many platforms as we know
		 * about, to avoid making room incrementally later as we
		 * receive higher platform ID's.
		 */
		dt_ExpandPlatformTable (nplat);
		NPlatform = nplat;
	}

	return (nplat);
}



int
dt_NClass (void)
{
	/*
	 * Since presently there is no message for querying the class
	 * size from the daemon (perhaps it should just be tacked onto
	 * the get-nplatform message), just return what we know locally.
	 */
	return (NPlatform);
}



/* ====================================================================
 * External interface for defining classes and platforms.  Depending 
 * upon our client state, these send a message to the daemon and cache
 * the result, else just add the structure to our table and assign it
 * the corresponding id.
 * --------------------------------------------------------------------
 */


PlatformId
dt_DefineSubPlatform (cid, name, parent)
PlatClassId cid;
const char *name;
PlatformId parent;
/*
 * Instantiate this platform class with the given name and parent.
 * For platform instances on the client side, we don't have a structure
 * on hand to cache, so we just cache the names-to-id entries.
 */
{
	PlatformId pid = BadPlatform;

	if (! Initialized)
		dt_InitTables ();

	if (Client)
	{
		/*
		 * All we get back from the daemon is the id,
		 * so we don't bother to cache anything here.
		 */
		pid = (*Client->DefinePlatform) (cid, name, parent);
	}
	else
	{
		/* 
		 * This is the meat of platform instantiation.  We end up
		 * here either on the daemon side at a client's or the
		 * daemon's request, or on the client side when standalone.
		 * The instantiation adds the platforms to the table.
		 */
		const PlatformClass *pc = dt_FindClass (cid);

		if (! pc)
		{
			msg_ELog (EF_PROBLEM, 
				  "%s %d to instantiate platform %s",
				  "could not find class", cid, name);
		}
		else
		{
			Platform *pi = dt_Instantiate (pc, name, parent);
			pid = (pi) ? pi->dp_id : BadPlatform;
		}
	}
	return (pid);
}




PlatClassId
dt_DefineClass (pc)
PlatClassRef pc;
/*
 * Define this class, assigning it an ID, and return that ID.  Return
 * BadClass if the definition does not succeed.  If connected to the
 * daemon, we send this class structure to the daemon and await the
 * new id.  Otherwise we cache the structure and assign it a local id.
 * Destroy the structure once it has been passed to the daemon or
 * if the definition fails.
 */
{
	const PlatformClass *spc = NULL;
	PlatformClass *add;
	int cid;

	if (! Initialized)
		dt_InitTables ();

	/*
	 * Set any necessary default directory paths.
	 */
	if (pc->dpc_superclass != BadClass)
		spc = dt_FindClass (pc->dpc_superclass);
	dt_FillClassDir (pc, spc);

	/*
	 * Validate
	 */
	dt_ValidateClass (pc);

	/*
	 * Now we proceed with definition according to our connection
	 */
	if (Client)
	{
		cid = (*Client->DefineClass) (pc);
		if (cid != BadClass)
			add = dt_AddClass (pc, cid);
	}
	else
	{
		add = dt_AddClass (pc, BadClass);
		cid = (add) ? add->dpc_id : BadClass;
	}
	if (! add)
		dt_DestroyClass (pc);
	return (cid);
}



/* --------------------------------------------------------------------- */



static void
dt_InitTables ()
/*
 * Initialize the data tables.
 */
{
	int i, nbytes;

	Initialized = TRUE;
	msg_ELog (EF_DEBUG, "Initializing platform and class tables");
/*
 * Create the platform and data file tables.
 */
	nbytes = CTableSize * sizeof (PlatformClass*);
	CTable = (PlatformClass **) malloc (nbytes);
	memset (CTable, 0, nbytes);

	nbytes = PTableSize * sizeof (PlatformInstance*);
	PTable = (PlatformInstance **) malloc (nbytes);
	memset (PTable, 0, nbytes);

	NPlatform = NClass = 0;
	if (!CTable || !PTable)	/* uh-oh */
	{
		msg_ELog (EF_EMERGENCY, "%s: %s",
			  "memory alloc failure", 
			  "could not initialize platform tables");
		exit (9);
	}
/*
 * Create the symbol tables which map platform and class names to IDs.
 */
	zl_usy_init ();
	Platforms = zl_c_stbl ("Platforms");
	Classes = zl_c_stbl ("Classes");
}




static PlatformClass *
dt_AddClass (PlatformClass *pc, PlatClassId cid)
/*
 * Insert a class structure with the given id into the class table.  An
 * existing class is first freed.  If id == BadClass, the class is assigned
 * an id.  The pointer belongs to the table after exit and must remain valid.
 * On success, the pointer is returned, else NULL.
 */
{
    SValue v;
    int type;
    const PlatformClass *exist;
/*
 * Make sure we're initialized.
 */
    if (! Initialized)
	dt_InitTables ();
/*
 * If this class already exists, delete it.
 * 
 */
    if (Classes && zl_g_symbol (Classes, pc_Name (pc), &type, &v))
    {
	msg_ELog (EF_DEBUG, "deleting class '%s': redefined", 
		  pc_Name (pc));
    /*
     * XXX We explicitly cast away the "const" from the class pointer returned
     * by dt_FindClass().
     */
	dt_DestroyClass ((PlatformClass*) dt_FindClass (v.us_v_int));
    }
/*
 * Determine the class ID, i.e. the class' slot in the table.  This
 * affects how much space we may have to grow onto the table.
 */

    if (cid == BadClass)
    {
	pc->dpc_id = NClass++;
    }
    else
    {
	pc->dpc_id = cid;
	if (cid >= NClass)
	    NClass = cid + 1;
    }
/*
 * If we've reached the hardcoded limit, we must obey it.
 */
    if (NClass >= MAXPLAT)
    {
	static int max_warning = 0;
	if (! max_warning++)
	    msg_ELog (EF_PROBLEM, "%s %d exceeded at '%s': %s",
		      "Max class count", MAXPLAT, pc->dpc_name,
		      "further definitions ignored");
	NClass = MAXPLAT;
	return (NULL);
    }
/*
 * If the class table is not big enough, expand it.
 */
    if (NClass > CTableSize)
    {
	dt_ExpandClassTable (NClass);
    }
/*
 * Add the pointer to the table.
 */
    CTable[pc->dpc_id] = pc;
/*
 * Cache the class name in the symbol table.
 */
    dt_CacheClassName (pc->dpc_name, pc->dpc_id);
    return (pc);
}




static PlatformInstance *
dt_AddPlatform (PlatformInstance *new, PlatformId pid)
/*
 * Add a platform instance structure to the internal table.
 * If pid == BadPlatform, then we assign an id here.  The pointer
 * is consumed by the table and should not be freed.
 */
{
    SValue v;
    int type;
/*
 * Make sure we're initialized.
 */
    if (! Initialized)
	dt_InitTables ();
/*
 * See if this guy already exists, and if so prepare it for re-conditioning
 */
    if (Platforms && zl_g_symbol (Platforms, pi_Name (new), &type, &v))
    {
	msg_ELog (EF_INFO, "deleting platform '%s': redefined", pi_Name (new));
    /*
     * XXX We cast away the "const" from the return value of 
     * dt_FindPlatform()...
     */
	dt_DeletePlatform ((Platform*)dt_FindPlatform (v.us_v_int));
    }
/*
 * Set the id as needed.
 */
    if (pid == BadPlatform)
    {
	new->dp_id = NPlatform++;
    }
    else
    {
	new->dp_id = pid;
	if (pid >= NPlatform)
	    NPlatform = pid + 1;
    }
/*
 * If we've reached our limit, disallow it.
 */
    if (NPlatform >= MAXPLAT)
    {
	static int max_warning = 0;
	if (! max_warning++)
	    msg_ELog (EF_PROBLEM, "%s %d exceeded at '%s': %s",
		      "Max platform count", MAXPLAT, new->dp_name,
		      "further definitions ignored");
	NPlatform = MAXPLAT;
	return (NULL);
    }
/*
 * If the platform table is not big enough for the new id, expand it.
 */
    if (NPlatform > PTableSize)
    {
	dt_ExpandPlatformTable (NPlatform);
    }
/*
 * Check whether this slot NULL or not, and delete if not.
 */
    if (PTable[new->dp_id])
    {
	msg_ELog (EF_PROBLEM, "pid collision, deleting '%s'",
		  PTable[new->dp_id]->dp_name);
	dt_DeletePlatform (PTable[new->dp_id]);
    }
    PTable[new->dp_id] = new;
    dt_CachePlatformNames (new->dp_name, new->dp_id);
    return (new);
}




static void
dt_CachePlatformNames (name, pid)
const char *name;
PlatformId pid;
/*
 * Enter this name and associated hierarchical names into the lookup table.
 */
{
	SValue v;
	const char *cp;

	if (! Initialized)
		dt_InitTables ();
/*
 * We don't stash failures since we allow dynamic platform creation.  A
 * name that fails once could succeed later, or perhaps was not meant to
 * succeed at all because of an error.
 */
	if (pid != BadPlatform)
	{
		v.us_v_int = pid;
		cp = name;
		do
		{
			usy_s_symbol (Platforms, (char *)cp, SYMT_INT, &v);
			if ((cp = (const char *) strchr (cp, '/')) != 0)
				cp++;
		}
		while (cp);
	}
}



static void
dt_CacheClassName (name, cid)
const char *name;
PlatClassId cid;
{
	SValue v;

	if (! Initialized)
		dt_InitTables ();
/*
 * We don't stash failures since we allow dynamic platform creation.
 * A name that fails once could succeed later, or is a name that wasn't
 * meant to succeed in the first place.
 */
	if (cid != BadClass)
	{
		v.us_v_int = cid;
		usy_s_symbol (Classes, (char *)name, SYMT_INT, &v);
	}
}



static void
dt_DeletePlatform (PlatformInstance *pi)
{
	if (! Initialized)
		dt_InitTables ();

	if (pi->dp_id != BadPlatform)
		PTable[pi->dp_id] = NULL;
/*
 * XXX Still need to remove names from symbol table...
 */
	dt_FreePlatform (pi);
}



static void
dt_ExpandClassTable (int min)
/*
 * Expand class table by multiple of 'growth' entries with a given minimum
 * and a ceiling of MAXPLAT.
 */
{
	int growth, size;
	int i;

	if (CTableSize >= min)
		return;
	growth = ((int)((min - CTableSize) / CTableGrow) + 1) * CTableGrow;
	size = CTableSize + growth;
	if (size > MAXPLAT)
		size = MAXPLAT;
	msg_ELog (EF_DEBUG, "Expanding CTable to %d entries", size);
	CTable = (PlatformClass **) realloc (CTable,
					     size * sizeof (PlatformClass *));
	for (i = CTableSize; i < size; ++i)
		CTable[i] = NULL;
	CTableSize = size;
}



static void
dt_ExpandPlatformTable (int min)
/*
 * Grow the platform table by a multiple of 'growth' entries to a given
 * minimum and a ceiling of MAXPLAT.
 */
{
	int i;
	int growth, size;

	if (PTableSize >= min)
		return;
	growth = ((int)((min - PTableSize) / PTableGrow) + 1) * PTableGrow;
	size = PTableSize + growth;
	if (size > MAXPLAT)
		size = MAXPLAT;
	msg_ELog (EF_DEBUG, "Expanding PTable to %d entries", size);
	PTable = (Platform **) realloc (PTable, size * sizeof (Platform *));
	for (i = PTableSize; i < size; ++i)
		PTable[i] = NULL;
	PTableSize = size;
}



#ifdef notdef
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
 * Associate this platform name (and all subnames) with p.
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
#endif




static PlatformInstance *
dt_Instantiate (pc, name, ppid)
const PlatformClass *pc;/* The platform class structure */
const char *name;	/* The name to be instantiated	*/
PlatformId ppid;	/* The parent of the new instance or BadPlatform */
/*
 * Actually instantiate the given class from the platform class
 * structure, adjusting default directories and adding subplats.
 *
 * This code must be re-entrant!  It may be called recursively to instantiate
 * subplatforms defined in the platform's class.
 * 
 * Returns NULL if the platform cannot be created.
 */
{
	PlatformInstance *new, *add;
	const PlatformInstance *parent = 0;
	char iname[1024];

	if (! Initialized)
		dt_InitTables ();
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
 * Subplats must be given unique names for the symbol table by prefixing
 * the parent name.  If the parent name is also a subplatform, it will 
 * already contain its instance path name.  The defined name of this
 * subplatform will still be used for generating directory entries.
 */
	if ((ppid != BadPlatform) && (parent = dt_FindPlatform (ppid)))
		sprintf (iname, "%s/%s", pi_Name(parent), name);
	else
		strcpy (iname, name);
/*
 * Create and initialize a platform instance.
 */
	if (! (new = dt_NewPlatform (iname)))
		return (NULL);	/* so much for that idea */
/*
 * Set the stuff in the instance structure which comes from the class:
 * the directories, flags, and the automatic subplat instantiations.
 */
	new->dp_class = pc->dpc_id;
/*
 * This is not very orthodox, since many of the flags should remain
 * strictly separated between class and instance.  But for now they
 * shouldn't interfere with each other.
 */
	new->dp_flags = pc->dpc_flags;
/*
 * Now see if we can get it added to the table and assigned an id.
 */
	add = dt_AddPlatform (new, BadPlatform);
	if (! add)				/* oh well... */
	{
		dt_FreePlatform (new);
		return (NULL);
	}
	new = add;
	if (parent)
	{
		new->dp_parent = parent->dp_id;
	/*
	 * If the parent platform is a composite, then the subplat must be a
	 * subplatform (in the historic sense).  In any case, add a reference
	 * to this child to the parent.
	 */
		if (pi_Composite (parent))
		{
			new->dp_flags |= DPF_SUBPLATFORM;
			new->dp_flags &= ~DPF_COMPOSITE;
		}
		dt_AddSubPlat ((Platform *)parent, new->dp_id);
	}
/*
 * Instantiate any subplatforms designated by the platform class
 */
	dt_InstantSubPlats (pc, new);
	return (new);
}



static const PlatformInstance *
dt_DefSubPlat (pp, spc, name)
Platform *pp;			/* Parent of subplat to be instantiated */
const PlatformClass *spc;	/* Class of subplat			*/
const char *name;		/* Name of subplat instance		*/
/*
 * Instantiate a platform of the given class, and make it a subplatform
 * of the given platform instance.  Don't allow platform to be instantiated
 * if another platform already exists with the same name.
 */
{
	SValue v;
	int type;
	const PlatformInstance *sub;
	char iname[1024];

	if (! Initialized)
		dt_InitTables ();
/*
 * The unique symbol table lookup name of the subplat instance is prefixed 
 * by the parent name.
 */
	sprintf (iname, "%s/%s", pi_Name (pp), name);

	if (Platforms && zl_g_symbol (Platforms, iname, &type, &v))
	{
		msg_ELog (EF_PROBLEM, "subplatform '%s' already exists",
			  iname);
	}
	else
	{
		sub = dt_Instantiate (spc, name, pp->dp_id);
	}
	return (sub);
}




static void
dt_InstantSubPlats (pc, pp)
const PlatformClass *pc;
Platform *pp;
/*
 * Loop through the subplats in the class and instantiate subplats for 
 * the new instance.
 *
 * Each subplat's parent will be the 'newid' platform.
 */
{
	int i;
	const PlatformClass *spc;

	if (! Initialized)
		dt_InitTables ();
#ifdef notdef
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
#endif
	for (i = 0; i < pc->dpc_nsubplats; ++i)
	{
		spc = dt_FindClass (pc->dpc_subplats[i].dps_class);
		if (spc)
			dt_DefSubPlat (pp, spc, pc->dpc_subplats[i].dps_name);
	}
}


/* ================================================================
 * Platform searching interface
 */

/* ---------------- Internal entry points ---------------- */

void
dt_GetPlatformList (search, pl)
struct dsp_PlatformSearch *search;
PlatformList *pl;
/*
 * Actually build the list, either by requesting from the daemon or
 * with an alternative method function.
 */
{
	if (! Initialized)
		dt_InitTables ();

	pl->pl_npids = 0;
	pl->pl_pids = NULL;
	if (Client)
	{
		(*Client->SendSearch)(search, pl);
	}
	else
	{
		/*
		 * Use the local search routines
		 */
		dt_ListSearch (search, pl);
	}
}




struct dsp_PlatformList *
dt_AnswerSearch (struct dsp_PlatformSearch *req, int *npids_out, int *len_out)
/*
 * This is a convenience routine for the daemon.  Allocate and fill out
 * a search result answer for the daemon to send.
 * Return NULL if we could not allocate the answer message.
 */
{
	struct dsp_PlatformList *answer;
	int len;
	int npids;

	*npids_out = 0;
	*len_out = 0;
/*
 * Create space for all possible matches, but we'll send only as many as we
 * fill.
 */
	len = sizeof (struct dsp_PlatformList);
	if (NPlatform > 1)
		len += (sizeof(PlatformId) * (NPlatform - 1)); 
	answer = (struct dsp_PlatformList *) malloc (len);
	if (! answer)
	{
		msg_ELog (EF_PROBLEM,
			  "malloc failed for platform search result");
		return (NULL);
	}
	answer->dsp_type = dpt_R_PlatformSearch;
/*
 * Do the search.
 */
	dt_SearchPlatforms (req, answer->dsp_pids, &npids);
	answer->dsp_npids = npids;
/*
 * Calculate the actual length of the answer.
 */
	len = sizeof (struct dsp_PlatformList);
	len += (npids <= 1) ? (0) : (npids - 1) * sizeof(PlatformId);
	*len_out = len;
	*npids_out = npids;
	return (answer);
}



/* ---------------- Private ---------------- */


static int
MatchPlatform (symbol, type, value, info)
char *symbol;
int type;
union usy_value *value;
struct SearchInfo *info;
/*
 * Use the request structure to see if there is a match with this
 * platform.  Right now we always return true because the search should
 * continue through all of the platforms.
 */
{
	const Platform *plat = dt_FindPlatform (value->us_v_int);
	struct dsp_PlatformSearch *req = info->si_req;
/*
 * Just in case...
 */
	if (! plat)
		return (TRUE);
/*
 * The platform name table contains lots of abbreviated symbols which are
 * not the fully qualified hierarchical name, but the full hierarchical
 * name is the only symbol guaranteed to be unique.  So, categorically skip
 * all those symbols whose lengths do not equal the length of the fully
 * qualified name.
 */
	if (strlen(plat->dp_name) != strlen(symbol))
		return (TRUE);
/*
 * Ignore subplatforms if so requested
 */
	if (!req->dsp_subplats && pi_Subplatform(plat))
		return (TRUE);
/*
 * If we're looking for subplatforms of a particular parent, ignore
 * all other platforms.  We don't care whether the platform has the
 * DPF_SUBPLATFORM flag set, we just care about who the parent is.
 */
	if ((req->dsp_children) && (plat->dp_parent != req->dsp_parent))
		return (TRUE);
/*
 * The last remaining check is the regular expression, if there is one
 */
	if ((req->dsp_regexp[0] == '\0') || (zl_re_exec (plat->dp_name)))
	{
		info->si_pids[(info->si_npids)++] = plat->dp_id;
	}
/*
 * Done testing this platform
 */
	return (TRUE);
}




static void
dt_ListSearch (struct dsp_PlatformSearch *req, PlatformList *pl)
/*
 * This is the "Client local" search result which does not need to
 * allocate a full answer message, just the array of platform ids.
 */
{
	int len;
	PlatformId *fill;

	pl->pl_npids = 0;
	pl->pl_pids = NULL;
	if (NPlatform == 0)
		return;
	len = (sizeof(PlatformId) * NPlatform); 
	fill = (PlatformId *) malloc (len);
	dt_SearchPlatforms (req, fill, &(pl->pl_npids));
	if (pl->pl_npids > 0)
	{
		len = pl->pl_npids * sizeof (PlatformId);
		pl->pl_pids = (PlatformId *) realloc (fill, len);
	}
	else
	{
		free (fill);
		pl->pl_pids = NULL;
	}
}



static void
dt_SearchPlatforms (req, pids, npids)
struct dsp_PlatformSearch *req;
PlatformId *pids;
int *npids;
{
	char *re_result;
	struct SearchInfo info;

	*npids = 0;
	if (! Initialized)	/* no platforms yet to search */
		return;
/*
 * Store the info for the matching function
 */
	info.si_req = req;
	info.si_pids = pids;
	info.si_npids = 0;
/*
 * Prepare our regexp, if there is one.  If there isn't, we match everything.
 */
	re_result = NULL;
	if (req->dsp_regexp[0])
	{
		re_result = zl_re_comp (req->dsp_regexp);
		if (re_result)
			msg_ELog (EF_PROBLEM, "PlatformSearch, '%s': %s",
				  req->dsp_regexp, re_result);
	}
/*
 * Now loop through all of the symbol entries.
 */
	if (re_result == NULL)
	{
		zl_search (Platforms, MatchPlatform, (int)&info, 
			   req->dsp_alphabet, NULL);
		*npids = info.si_npids;
	}
}



