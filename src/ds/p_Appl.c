/* 
 * The datastore public interface for defining and manipulating platform
 * classes and definitions, shared by application, client library, and
 * daemon.  This interface is built on the interface in p_Table.c, which
 * actually talks to the daemon, if necessary, and manages the table of
 * cached structures.
 */

#include <defs.h>
#include <message.h>

#include "DataStore.h"
#include "dsPrivate.h"		/* platform and class type definitions */
#include "Platforms.h"

RCSID("$Id: p_Appl.c,v 3.3 2002-01-19 06:50:02 granger Exp $")


/* ================
 * General info
 */

int
ds_GetNPlat ()
{
	return (dt_NPlatform());
}


/* ================
 * Public interface for querying platform attributes by id
 *
 * Technically the daemon could use this interface, but usually it
 * shouldn't need to, since most of its access will be through a pointer.
 */



PlatformId
ds_LookupPlatform (name)
const char *name;
{
	const Platform *p;
	PlatformId pid = BadPlatform;

	if ((p = dt_FindPlatformName (name)))
	{
		pid = p->dp_id;
	}
	return (pid);
}




PlatClassId
ds_LookupClass (name)
const char *name;
{
	const PlatformClass *pc;
	PlatClassId cid = BadClass;

	if ((pc = dt_FindClassName (name)))
	{
		cid = pc->dpc_id;
	}
	return (cid);
}




PlatClassId
ds_PlatformClass (pid)
PlatformId pid;
{
	const Platform *cp;

	if ((cp = dt_FindPlatform (pid)))
		return (cp->dp_class);
	else
		return (BadClass);
}




const char *
ds_PlatformName (id)
PlatformId id;
/*
 * Get back the name for this platform.
 */
{
	static const char *badmsg = "(BadPlatformID)";
	const Platform *cp;

	if ((cp = dt_FindPlatform (id)))
		return (cp->dp_name);
	return (badmsg);
}




const char *
ds_ClassName (id)
PlatClassId id;
/*
 * Give back the name for this platform class.  Return an error string if
 * the id is invalid.
 */
{
	static const char *badmsg = "(BadClassID)";
	const PlatformClass *pc;

	if ((pc = dt_FindClass (id)))
		return (pc->dpc_name);
	return (badmsg);
}



int
ds_IsMobile(id)
PlatformId id;
/*
 * Return TRUE iff this is a mobile platform.
 */
{
	const Platform *p;
	p = dt_FindPlatform (id);
	return ((p) ? (p->dp_flags & DPF_MOBILE) : FALSE);
}





int
ds_IsModelPlatform(id)
PlatformId id;
/*
 * Return TRUE iff this is a model platform.
 */
{
	const Platform *p;
	p = dt_FindPlatform (id);
	return ((p) ? (p->dp_flags & DPF_MODEL) : FALSE);
}



int
ds_MaxSamples (id)
PlatformId id;
{
	const Platform *p;
	if ((p = dt_FindPlatform (id)))
	{
		const PlatformClass *pc = dt_FindClass (p->dp_class);
		if (pc)
			return (pc->dpc_maxsamp);
	}
	return (0);
}



DataOrganization
ds_PlatformDataOrg (pid)
PlatformId pid;
/*
 * Return the organization of the data returned by this platform.
 */
{
	const Platform *p;
	
	if ((p = dt_FindPlatform (pid)))
	{
		const PlatformClass *pc = dt_FindClass (p->dp_class);
		if (pc)
			return (pc->dpc_org);
	}
	return (OrgUnknown);
}


const FieldId *
ds_PlatformClassFields (PlatformId pid, int *nfield)
/*
 * Return the number of class fields defined in this platform and a pointer
 * to an array of them.  The array belongs to the platform definition and
 * should not be changed.
 */
{
	const Platform *p;
	FieldId *fields = 0;
	
	*nfield = 0;
	if ((p = dt_FindPlatform (pid)))
	{
		const PlatformClass *pc = dt_FindClass (p->dp_class);
		if (pc)
		{
		    *nfield = pc->dpc_nfields;
		    fields = pc->dpc_fields;
		}
	}
	return (fields);
}


/* =====================================================================
 * Platform searching
 *
 * Search functions return NULL on error or when no platforms satisfied
 * the search criteria.  Otherwise they return an array of platform ids
 * the number of ids in the array; the array must be freed by the caller.
 */

PlatformId *
ds_SearchPlatforms (regexp, nplats, sort, subs)
char *regexp;
int *nplats;
zbool sort;
zbool subs;
/*
 * Send a search message to the daemon, telling it we don't want platform
 * structures, and return the list of pids we receive back from the daemon.
 */
{
	PlatformList pl;
	struct dsp_PlatformSearch search;

	search.dsp_type = dpt_PlatformSearch;
	search.dsp_alphabet = sort;
	search.dsp_subplats = subs;
	search.dsp_parent = BadPlatform;
	search.dsp_children = FALSE;
	if (regexp && (strlen(regexp) < sizeof(search.dsp_regexp)))
		strcpy(search.dsp_regexp, regexp);
	else
		search.dsp_regexp[0] = '\0';

	dt_GetPlatformList (&search, &pl);

	*nplats = pl.pl_npids;
	return (pl.pl_pids);
}




PlatformId *
ds_LookupSubplatforms (parent, nsubplat)
PlatformId parent;
int *nsubplat;
/*
 * Return a list of the ID's of any subplatforms of the given platform.
 * If none are found, returns NULL.  The returned list must be freed by
 * the application.
 */
{
	struct dsp_PlatformSearch search;
	PlatformList pl;

	search.dsp_type = dpt_PlatformSearch;
	search.dsp_parent = parent;
	search.dsp_children = TRUE;
	search.dsp_subplats = TRUE;
	search.dsp_alphabet = FALSE;
	search.dsp_regexp[0] = '\0';

	dt_GetPlatformList (&search, &pl);
	*nsubplat = pl.pl_npids;
	return (pl.pl_pids);
}




/* ======================================================================
 * Platform definition routines
 *
 * Client interface for defining classes, adding subplatforms to classes,
 * and instantiating platforms through the datastore protocol.
 */

/* -----------------------------------------------------------------------
 * Platform class definitions
 */

PlatClassRef
ds_NewNamedClass (name, super)
const char *name;
const char *super;	/* Superclass name, or NULL if none */
{
	PlatClassId sid = BadClass;

	if (super)
	{
		const PlatformClass *pc = dt_FindClassName (super);
		
		if (! pc)
		{
			msg_ELog (EF_PROBLEM,
				  "defining %s: superclass '%s' not defined",
				  name, super);
		}
		else
		    sid = pc->dpc_id;
	}
	return (ds_NewSubClass (name, sid));
}




PlatClassRef
ds_NewSubClass (name, sid)
const char *name;
PlatClassId sid;	/* Superclass id, or BadClass if none */
/*
 * Return a newly allocated and initialized class structure, subclassed
 * from the given superclass, if any.  The class is not actually defined
 * and assigned an ID until ds_DefineClass() is called.  Therefore, the
 * pointer must be freed if the class is not defined.
 */
{
	PlatformClass *pc;
	const PlatformClass *spc = NULL;

	if (sid != BadClass)
	{
		spc = dt_FindClass (sid);
		if (! spc)
		{
			msg_ELog (EF_PROBLEM, "superclass %d unknown", sid);
		}
	}

	pc = dt_Subclass (spc, name);
	return (pc);
}



PlatClassRef
ds_NewClass (name)
const char *name;
/*
 * Returns a new class structure, just like NewSubclass, except no superclass
 */
{
	return (ds_NewSubClass (name, BadClass));
}



void
ds_AddClassSubplat (pc, subid, subname)
const PlatClassRef pc;
PlatClassId subid;
const char *subname;
/*
 * Add a subplatform of the given name and class to this class
 */
{
	dt_AddClassSubPlat (pc, subid, subname);
}



void
ds_EraseClassSubplat (pc)
PlatClassRef pc;
/*
 * Erase all subplatforms from this class.
 */
{
	dt_EraseClassSubPlats (pc);
}



void
ds_DestroyClass (pc)
PlatClassRef pc;
/*
 * Free an UNDEFINED class structure returned by ds_NewClass() or
 * ds_NewSubClass().
 */
{
	dt_DestroyClass (pc);
}



int
ds_ShowPlatformClass (fp, cid)
FILE *fp;
PlatClassId cid;
/*
 * Dump the config definition for this class to the given file pointer.
 * Return 0 if we succeed, negative otherwise.
 */
{
	const PlatformClass *pc, *spc = NULL;

	if (! (pc = dt_FindClass (cid)))
		return (-1);
	if (pc->dpc_superclass != BadClass)
	{
		if (! (spc = dt_FindClass (pc->dpc_superclass)))
			return (-2);
	}
	dt_DecodeClass (fp, pc, spc);
	return (0);
}




/* ----
 * These routines modify a class definition which has already been
 * initialized.
 */

inline static void
ds_SetClassFlag (pc, bit, flag)
PlatClassRef pc;
int bit;
int flag;
{
	if (flag)
		pc->dpc_flags |= bit;
	else
		pc->dpc_flags &= ~bit;
}


void
ds_AssignClass (pc, org, ftype, mobile)
PlatClassRef pc;
DataOrganization org;
FileType ftype;
int mobile;
/*
 * Assign these most common platform characteristics to the platform class.
 */
{
	pc->dpc_org = org;
	pc->dpc_ftype = ftype;
	ds_SetClassFlag (pc, DPF_MOBILE, mobile);
}


void
ds_SetOrg (cd, org)
PlatClassRef cd;
DataOrganization org;
{
	cd->dpc_org = org;
}


void
ds_SetFiletype (cd, ft)
PlatClassRef cd;
FileType ft;
{
	cd->dpc_ftype = ft;
}


void
ds_SetInheritDir (cd, id)
PlatClassRef cd;
InheritDirFlag id;
{
	cd->dpc_inherit = id;
}	


void
ds_SetInstanceDir (cd, id)
PlatClassRef cd;
InstanceDirFlag id;
{
	cd->dpc_instance = id;
}


void
ds_SetMobile (cd, mobile)
PlatClassRef cd;
int mobile;
{
	ds_SetClassFlag (cd, DPF_MOBILE, mobile);
}


void
ds_SetComposite (cd, composite)
PlatClassRef cd;
int composite;
{
	ds_SetClassFlag (cd, DPF_COMPOSITE, composite);
}


void
ds_SetModel (cd, flag)
PlatClassRef cd;
int flag;
{
	ds_SetClassFlag (cd, DPF_MODEL, flag);
}
	

void
ds_SetSplitSeconds (PlatClassRef cd, unsigned int secs)
{
    cd->dpc_splitseconds = secs;
}


void
ds_SetDaysplit (cd, split)
PlatClassRef cd;
int split;
{
    ds_SetSplitSeconds (cd, (split != 0) ? 24*3600 : 0);
}


void
ds_SetAbstract (cd)
PlatClassRef cd;
{
	cd->dpc_flags |= DPF_ABSTRACT;
}


void
ds_SetVirtual (cd)
PlatClassRef cd;
{
	cd->dpc_flags |= DPF_VIRTUAL;
}


void
ds_SetMaxSample (cd, maxsamp)
PlatClassRef cd;
int maxsamp;
{
	cd->dpc_maxsamp = (unsigned short) maxsamp;
}


void
ds_SetComment (cd, comment)
PlatClassRef cd;
const char *comment;
{
	dt_SetComment (cd, comment);
}


void
ds_SetDirectory (cd, dir)
PlatClassRef cd;
const char *dir;
{
	dt_SetString (cd->dpc_dir, dir, sizeof (cd->dpc_dir), "SetDirectory");
}


/* -----------------------
 * Actually define classes and instances to the daemon
 */

PlatClassId
ds_DefineClass (pc)
PlatClassRef pc;
/*
 * Define this class, assigning it an ID, and return that ID.  Return
 * BadClass if the definition does not succeed.  If connected to the
 * daemon, we send this class structure to the daemon and await the
 * new id.  Otherwise we cache the structure and assign it a local id.
 * Destroy the structure once it has been passed to the daemon.
 */
{
	return (dt_DefineClass (pc));
}




PlatformId
ds_DefineSubPlatform (cid, name, parent)
PlatClassId cid;
const char *name;
PlatformId parent;
/*
 * Instantiate this platform class with the given name and parent.
 */
{
	return (dt_DefineSubPlatform (cid, name, parent));
}




PlatformId
ds_DefinePlatform (cid, name)
PlatClassId cid;
const char *name;
/*
 * Instantiate this platform class with the given name and no parent.
 */
{
	return (dt_DefineSubPlatform (cid, name, BadPlatform));
}


void
ds_AddClassField (PlatClassRef pc, FieldId fid)
/*
 * Add a field to this class.
 */
{
    dt_AddClassField (pc, fid);
}


void
ds_AddClassDerivation (PlatClassRef pc, char *dtext)
/*
 * Add a derivation to this class.
 */
{
    dt_AddClassDerivation (pc, dtext);
}


void
ds_SetDerivations (PlatClassRef pc, char* dtext)
{
    dt_SetDerivations (pc, dtext);
}


/* =======================================================================
 * End public platform interface
 * ======================================================================= */

