/*
 * Routines for manipulating class definition structures.
 * Meant to be the basic level of access to be shared by daemon and client
 * alike.  
 *
 * The goal is this: define and implement a single interface for describing,
 * defining, and accessing platforms which can be used by any application,
 * client and daemon alike.  The table structure for storing platforms on
 * the daemon can be used by the client to cache platform structures.  The
 * daemon configuration code uses the same interface to define classes and
 * platforms read from the config file.  This keeps everything nice and
 * uniform and helps avoid redundancy, especially redundant errors...
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

# include <stdlib.h>
# include <unistd.h>
# include <stdio.h>
# include <string.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <errno.h>

# include <defs.h>
# include <message.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "Platforms.h"

RCSID("$Id: Platforms.c,v 3.12 2002-09-17 18:28:43 granger Exp $")



/*
 * Applications (i.e., the daemon) may want to tag extra information
 * onto the instance structure (derive), so we allow specification
 * of the size to allocate and the functions to initialize and destroy it.
 */
struct _Derive 
{
	int Size;
	void (*Init)(PlatformInstance *new);
	void (*Destroy)(PlatformInstance *new);
}
Derived =
{
	sizeof(PlatformInstance),
	NULL,
	NULL
};

/*
 * Blocks by which subplat and fieldid allocated arrays are increased.
 */
# define ALLOC_SUBPLATS	10
# define ALLOC_FIELDS	10

/*
 * Default file keep time for real-time operations.
 */
int DefaultKeep = 60;

/*
 * Private prototypes
 */
static void dt_InitClass (PlatformClass *pc, const char *name);
static int dt_FindSubplat (const PlatformClass *pc, SubPlatform *dps);
static SubPlatform *dt_NewClassSubPlat (PlatformClass *pc);
static void dt_InitInstance (PlatformInstance *new, const char *name);
static void dt_EraseSubPlats (PlatformInstance *plat);



void
dt_ExtendPlatforms (int size, void (*init)(Platform *), 
		    void (*destroy)(Platform *))
{
	Derived.Size = size;
	Derived.Init = init;
	Derived.Destroy = destroy;
}



static void
dt_InitClass (pc, name)
PlatformClass *pc;
const char *name;
/*
 * Initialize all of the members of a platform class, but do not assign
 * any defaults for the directories.  NULL 'name' leaves the class name
 * empty.
 */
{
	if (name)
		dt_SetString (pc->dpc_name, name, sizeof(pc->dpc_name),
			      "assigning name to class");
	else
		pc->dpc_name[0] = '\0';

	pc->dpc_dir[0] = '\0';
	pc->dpc_id = BadClass;
	pc->dpc_superclass = BadClass;
	pc->dpc_org = OrgUnknown;

	pc->dpc_ftype = FTUnknown;
	pc->dpc_keep = DefaultKeep;
	pc->dpc_maxsamp = 60;
	pc->dpc_splitseconds = 0;
	pc->dpc_flags = 0;
	pc->dpc_inherit = InheritDefault;	/* InheritNone */
	pc->dpc_instance = InstanceDefault;	/* InstanceRoot */
	pc->dpc_comment = NULL;
	pc->dpc_subplats = NULL;
	pc->dpc_nsubplats = 0;
	pc->dpc_fields = NULL;
	pc->dpc_nfields = 0;
	pc->dpc_derivations = NULL;
}




PlatformClass *
dt_Subclass (super, name)
const PlatformClass *super;
const char *name;	/* name to give to the subclass */
/*
 * Create a new class, then copy the super class to the sub class, taking
 * care of details like copying subplats and resetting the comment and
 * subclass name.
 */
{
	PlatformClass *sub;

	sub = (PlatformClass *) malloc (sizeof (PlatformClass));
	if (super)
	{
		dt_CopyClass (sub, super);
		sub->dpc_superclass = super->dpc_id;
	}
	else
	{
		dt_InitClass (sub, name);
		sub->dpc_superclass = BadClass;
	}
	sub->dpc_id = BadClass;
/*
 * For now, as an arbitrary convention, we won't inherit comments.
 */
	dt_SetComment (sub, NULL);
/*
 * A subclass isn't an abstract base class or virtual
 * just because it's superclass is!
 */
	sub->dpc_flags &= ~DPF_ABSTRACT;
	sub->dpc_flags &= ~DPF_VIRTUAL;
	dt_SetString (sub->dpc_name, name, sizeof(sub->dpc_name),
		      "assigning name to subclass");
/* 
 * Erase our directory since it will be given defaults later
 */
	sub->dpc_dir[0] = '\0';
	return (sub);
}




void
dt_CopyClass (dest, src)
PlatformClass *dest;
const PlatformClass *src;
/*
 * Copy the src class to the dest class, taking care of details like
 * copying subplats and the comment.  The destination class must be
 * uninitialized or erased.
 */
{
	*dest = *src;
	dest->dpc_comment = NULL;
	dt_SetComment (dest, src->dpc_comment);
	dest->dpc_subplats = NULL;
	dest->dpc_nsubplats = 0;
	dt_CopyClassSubPlats (src, dest);
	dest->dpc_fields = NULL;
	dest->dpc_nfields = 0;
	dt_CopyClassFields (src, dest);
	dest->dpc_derivations = NULL;
	dt_SetDerivations (dest, src->dpc_derivations);
}



void
dt_EraseClass (pc)
PlatformClass *pc;
/*
 * Re-initialize a class to the "blank slate state", from whence it can
 * be reused, recycled, or released back to the heap.
 */
{
	dt_EraseClassSubPlats (pc);
	dt_EraseClassFields (pc);
	if (pc->dpc_comment)
		free (pc->dpc_comment);
	if (pc->dpc_derivations)
		free (pc->dpc_derivations);
	dt_InitClass (pc, NULL);
}


void
dt_DestroyClass (pc)
PlatClassRef pc;
/*
 * Free an UNDEFINED class structure returned by dt_Subclass().
 */
{
	dt_EraseClass (pc);
	free (pc);
}



void
dt_SetComment (pc, comment)
PlatformClass *pc;
const char *comment;
{
	if (pc->dpc_comment)
		free (pc->dpc_comment);
	pc->dpc_comment = NULL;
	if (comment)
	{
		pc->dpc_comment = (char *) malloc (strlen(comment)+1);
		strcpy (pc->dpc_comment, (char *) comment);
	}
}



void
dt_SetDerivations (PlatformClass *pc, const char *dtext)
{
	if (pc->dpc_derivations)
		free (pc->dpc_derivations);
	pc->dpc_derivations = NULL;
	if (dtext)
	{
		pc->dpc_derivations = (char *) malloc (strlen(dtext)+1);
		strcpy (pc->dpc_derivations, (char *) dtext);
	}
}



void
dt_AddClassDerivation (PlatformClass *pc, char *dtext)
{
	if (dtext)
	{
		int len = strlen(dtext)+1;
		if (pc->dpc_derivations)
		{
			len += strlen (pc->dpc_derivations);
			pc->dpc_derivations =
			    (char*)realloc(pc->dpc_derivations, len);
		}
		else
		{
			pc->dpc_derivations = (char*)malloc(len);
			pc->dpc_derivations[0] = '\0';
		}
		strcat (pc->dpc_derivations, dtext);
	}
}



void
dt_AddClassSubPlat (PlatformClass *pc, PlatClassId sid, const char *name)
/*
 * Add a subplat of class 'spc' and name 'name' to class 'pc'
 */
{
	SubPlatform *sp;

	sp = dt_NewClassSubPlat (pc);
	sp->dps_class = sid;
	dt_SetString (sp->dps_name, name, sizeof(sp->dps_name), 
		      "subplats name");
}



static SubPlatform *
dt_NewClassSubPlat (PlatformClass *pc)
/*
 * Add a SubPlatform template to this class.
 * The subplats list will be allocated in blocks of ALLOC_SUBPLATS.  We
 * know we need to increase size whenever we reach a multiple of this
 * value.
 */
{
	SubPlatform *sp;
	int len;

	len = (pc->dpc_nsubplats + ALLOC_SUBPLATS) * sizeof(SubPlatform);
	if (pc->dpc_nsubplats == 0)
		pc->dpc_subplats = (SubPlatform *) malloc (len);
	else if ((pc->dpc_nsubplats % ALLOC_SUBPLATS) == 0)
		pc->dpc_subplats = (SubPlatform *)
			realloc (pc->dpc_subplats, len);
	sp = pc->dpc_subplats + pc->dpc_nsubplats;
	pc->dpc_nsubplats++;
	return (sp);
}



void
dt_EraseClassSubPlats (pc)
PlatformClass *pc;
/*
 * Remove the subplats from this class 
 */
{
	if (pc->dpc_subplats)
		free (pc->dpc_subplats);
	pc->dpc_subplats = NULL;
	pc->dpc_nsubplats = 0;
}



void
dt_CopyClassSubPlats (src, dest)
const PlatformClass *src;
PlatformClass *dest;
/*
 * Create space for the src subplats in dest and copy.
 * First erase any subplats from the dest class.
 */
{
	int len;

	dt_EraseClassSubPlats (dest);
	if (src->dpc_nsubplats == 0)
		return;
/*
 * Figure out how many blocks of ALLOC_SUBPLATS have been allocated in src,
 * and allocate this much for the dest class
 */
	len = ((src->dpc_nsubplats - 1) / ALLOC_SUBPLATS) + 1;
	len *= ALLOC_SUBPLATS * sizeof(SubPlatform);
	dest->dpc_subplats = (SubPlatform *) malloc (len);
	dest->dpc_nsubplats = src->dpc_nsubplats;
	memcpy (dest->dpc_subplats, src->dpc_subplats,
		src->dpc_nsubplats * sizeof(SubPlatform));
}



void
dt_AddClassField (PlatformClass *pc, FieldId id)
/*
 * Add a field to the platform.
 */
{
	int len;

	len = (pc->dpc_nfields + ALLOC_FIELDS) * sizeof(FieldId);
	if (pc->dpc_nfields == 0)
		pc->dpc_fields = (FieldId *) malloc (len);
	else if ((pc->dpc_nfields % ALLOC_FIELDS) == 0)
		pc->dpc_fields = (FieldId *)
			realloc (pc->dpc_fields, len);
	pc->dpc_fields[pc->dpc_nfields] = id;
	pc->dpc_nfields++;
}



void
dt_EraseClassFields (pc)
PlatformClass *pc;
/*
 * Remove the subplats from this class 
 */
{
	if (pc->dpc_fields)
		free (pc->dpc_fields);
	pc->dpc_fields = NULL;
	pc->dpc_nfields = 0;
}



void
dt_CopyClassFields (src, dest)
const PlatformClass *src;
PlatformClass *dest;
/*
 * Create space for the src fields in dest and copy.
 * First erase any fields from the dest class.
 */
{
	int len;

	dt_EraseClassFields (dest);
	if (src->dpc_nfields == 0)
		return;
/*
 * Figure out how many blocks of ALLOC_FIELDS have been allocated in src,
 * and allocate this much for the dest class
 */
	len = ((src->dpc_nfields - 1) / ALLOC_FIELDS) + 1;
	len *= ALLOC_FIELDS * sizeof(FieldId);
	dest->dpc_fields = (FieldId *) malloc (len);
	dest->dpc_nfields = src->dpc_nfields;
	memcpy (dest->dpc_fields, src->dpc_fields,
		src->dpc_nfields * sizeof(FieldId));
}



zbool
dt_ValidateClass (pc)
PlatformClass *pc;
/*
 * Make sure this platform class is properly filled out. This means making
 * sure the organization and filetype have been specified, at a minimum.
 * Of course, if this class is abstract only, we don't care what's in it
 * since the subclasses may fill in the rest.  Likewise if it's virtual,
 * then the rest of the checks are irrelevant since they deal with actually
 * storing data (i.e. org and format), which virtual platforms are not
 * meant to do.
 *
 * And how could I forget: subplatforms of course have but one
 * requirement at the moment, that they be scalar.  */
{
	zbool valid = TRUE;

	if (pc->dpc_flags & DPF_ABSTRACT)
		return (TRUE);
	if (pc->dpc_flags & DPF_VIRTUAL)
		return (TRUE);
	if (pc->dpc_flags & DPF_SUBPLATFORM)
	{
		if (pc->dpc_org != OrgScalar)
		{
			msg_ELog (EF_PROBLEM, 
				  "class %s: subplatforms must be scalar",
				  pc->dpc_name);
			valid = FALSE;
		}
		return (valid);
	}
	if (pc->dpc_org == OrgUnknown)
	{
		msg_ELog (EF_PROBLEM, 
			  "class %s: no organization", pc->dpc_name);
		valid = FALSE;
	}
	if (pc->dpc_ftype == FTUnknown)
	{
		msg_ELog (EF_PROBLEM,
			  "class %s: no file type", pc->dpc_name);
		valid = FALSE;
	}
	if (pc->dpc_dir[0] == '\0')
	{
		msg_ELog (EF_PROBLEM,
			  "class %s: no directory", pc->dpc_name);
		valid = FALSE;
	}
#ifdef notdef
	if (!DisableRemote && !pc->dpc_rdir)
	{
		msg_ELog (EF_DEBUG, "class %s: remote enabled but %s",
			  pc->dpc_name, "no remote data directory");
		/* just a warning, still valid class */
	}
#endif
	return (valid);
}



struct dsp_ClassStruct *
dt_InjectClass (pc, am, retlen)
const PlatformClass *pc;	/* the class to serialize */
struct dsp_ClassStruct *am;	/* pre-allocated memory (e.g. automatic)
				   for common unadorned classes */
int *retlen;			/* length of final message */
/*
 * Fit this class into a ClassStruct protocol message using all the
 * appropriate conventions.  The counterpart for the receiving end is
 * dt_ExtractClass().  The return value is either a pointer to the 
 * pre-allocated memory (retval == am), or a pointer to malloc'ed memory
 * which must be freed by the caller.
 */
{
    struct dsp_ClassStruct *answer;
    int i, len;

    answer = am;
    len = sizeof (struct dsp_ClassStruct);

/*
 * The length we calculate first is a maximum, not including any class
 * fields we need to send. We'll adjust it later to fit the space we
 * actually used. 
 */
    if (pc->dpc_nsubplats > 1)
	len += (pc->dpc_nsubplats - 1) * sizeof (SubPlatform);

    if (pc->dpc_comment)
	len += strlen (pc->dpc_comment) + 1;

    if (pc->dpc_derivations)
	len += strlen (pc->dpc_derivations) + 1;
/*
 * Allocate space separate from that given to us if our class has fields,
 * since we don't know how much space that will take.
 */
    if (len > sizeof (struct dsp_ClassStruct) || pc->dpc_nfields > 0)
	answer = (struct dsp_ClassStruct *) malloc (len);

    answer->dsp_type = dpt_R_ClassStruct;
    answer->dsp_class = *pc;

    len = sizeof (struct dsp_ClassStruct);
    if (pc->dpc_nsubplats > 0)
    {
    /* 
     * Pack these in, to save the empty space from short names.
     * First the class id's of each subplat, then their names.
     */
	len += (pc->dpc_nsubplats - 1) * sizeof (PlatClassId);
	for (i = 0; i < pc->dpc_nsubplats; ++i)
	{
	    answer->dsp_subplatid[i] = 
		pc->dpc_subplats[i].dps_class;
	    strcpy ((char *)answer + len, 
		    pc->dpc_subplats[i].dps_name);
	    len += strlen (pc->dpc_subplats[i].dps_name) + 1;
	}
    }
    if (pc->dpc_comment)
    {
	char *cp = (char *)answer + len;
	len += strlen (pc->dpc_comment) + 1;
	strcpy (cp, pc->dpc_comment);
    }
    if (pc->dpc_derivations)
    {
	char *cp = (char *)answer + len;
	len += strlen (pc->dpc_derivations) + 1;
	strcpy (cp, pc->dpc_derivations);
    }
    /* 
     * Pack the fields in, but we have to pass them by name since FieldIds
     * do not translate between processes.
     */
    for (i = 0; i < pc->dpc_nfields; ++i)
    {
	char *fname = F_GetFullName (pc->dpc_fields[i]);
	int c = len;
	len += strlen(fname) + 1;
	answer = (struct dsp_ClassStruct *) realloc (answer, len);
	strcpy ((char *)answer + c, fname);
	msg_ELog (EF_DEVELOP, "class %s: injected field %s",
		  pc->dpc_name, (char*)answer+c);
    }
    *retlen = len;
    if (answer != am && pc->dpc_nfields == 0)
	answer = (struct dsp_ClassStruct *) realloc (answer, len);
    return (answer);
}



int
dt_ExtractClass (pc, dsp, len)
PlatformClass *pc;
struct dsp_ClassStruct *dsp;
int len;
/*
 * Given a blank class structure, fill it in with the platform class
 * extracted from the ds protocol message of the given length.  This is the
 * receiving counterpart for the sender dt_InjectClass().  If we fail for
 * any reason, return zero.  Otherwise we return non-zero.
 */
{
	int c, i;

	/*
	 * Start with the basics.
	 */
	*pc = dsp->dsp_class;
	c = sizeof (struct dsp_ClassStruct);
	/*
	 * Now continue with the optional trailers.
	 */
	if (pc->dpc_nsubplats > 0)
	{
		int slen = ((pc->dpc_nsubplats - 1) / ALLOC_SUBPLATS) + 1;
		slen *= ALLOC_SUBPLATS * sizeof (SubPlatform);
		pc->dpc_subplats = (SubPlatform *) malloc (slen);
		c += (pc->dpc_nsubplats - 1) * sizeof (PlatClassId);
		for (i = 0; i < pc->dpc_nsubplats; ++i)
		{
			pc->dpc_subplats[i].dps_class = dsp->dsp_subplatid[i];
			strcpy (pc->dpc_subplats[i].dps_name, (char *)dsp + c);
			c += strlen (pc->dpc_subplats[i].dps_name) + 1;
		}
	}
	if (pc->dpc_comment != 0)
	{
		pc->dpc_comment = 0;
		dt_SetComment (pc, (char *)dsp + c);
		c += strlen ((char *)dsp + c) + 1;
	}
	if (pc->dpc_derivations != 0)
	{
		char *dtext = (char*)dsp + c;
		pc->dpc_derivations = 0;
		dt_SetDerivations (pc, dtext);
		c += strlen (dtext) + 1;
	}
	if (pc->dpc_nfields > 0)
	{
		int slen = ((pc->dpc_nfields - 1) / ALLOC_FIELDS) + 1;
		slen *= ALLOC_FIELDS * sizeof (FieldId);
		msg_ELog (EF_DEVELOP, "allocated %d bytes for %d class fields"
			  " for class %s", slen, pc->dpc_nfields, 
			  pc->dpc_name);
		pc->dpc_fields = (FieldId *) malloc (slen);
		for (i = 0; i < pc->dpc_nfields; ++i)
		{
		    char *cp = (char*)dsp + c;
		    msg_ELog (EF_DEVELOP, "parsing class field: %s", cp);
		    pc->dpc_fields[i] = F_Lookup (cp);
		    c += strlen (cp) + 1;
		}
	}
	/*
	 * Add future trailers here.
	 */
	return (1);
}




void
dt_FillClassDir (PlatformClass *pc, const PlatformClass *super)
/*
 * Determine the default directory name, using the inherit flags.
 *	InheritNone: just use our class name
 *	InheritCopy: use our superclass's directory name
 *	InheritAppend: append our class name as a subdirectory of our
 *		superclass's directory
 */
{
    char newdir[512];
    char *dirname;

    dirname = (pc->dpc_dir[0] == '\0') ? pc->dpc_name : pc->dpc_dir;

    if  ((pc->dpc_inherit == InheritNone) || (super == NULL))
	strcpy (newdir, dirname);
    else if (pc->dpc_inherit == InheritCopy)
	strcpy (newdir, super->dpc_dir);
    else 	/* InheritAppend */
	sprintf (newdir, "%s/%s", super->dpc_dir, dirname);

    dt_SetString (pc->dpc_dir, newdir, sizeof(pc->dpc_dir), 
		  "class default data directory");
}




void
dt_AddSubPlat (plat, subid)
PlatformInstance *plat;
PlatformId subid;
/*
 * Add space for a subplatform index to this instance.
 * The subplats list will be allocated in blocks of ALLOC_SUBPLATS.  We
 * know we need to increase size whenever we reach a multiple of this
 * value.
 */
{
	int len;

	len = (plat->dp_nsubplats + ALLOC_SUBPLATS) * sizeof(PlatformId);
	if (plat->dp_nsubplats == 0)
		plat->dp_subplats = (PlatformId *) malloc (len);
	else if ((plat->dp_nsubplats % ALLOC_SUBPLATS) == 0)
		plat->dp_subplats = (PlatformId *)
			realloc (plat->dp_subplats, len);
	plat->dp_subplats[plat->dp_nsubplats++] = subid;
}




PlatformInstance *
dt_NewPlatform (const char *name)
/*
 * Create a new platform instance with the given name.  The returned
 * structure has not been added to the table, so it needs to either
 * be added to the table or deleted.
 *
 * Return NULL if the platform could not be created.
 */
{
	PlatformInstance *new;
/*
 * Allocate a new platform table entry.
 */
	new = (PlatformInstance *) malloc (Derived.Size);
	if (! new)
	{
		msg_ELog (EF_PROBLEM,
			  "malloc failure for platform instance '%s'", name);
		return (NULL);
	}
/*
 * Initialize the instance.  Note this is not the same as assigning defaults.
 * Defaults come later since they may be inherited or specified by the class.
 */
	dt_InitInstance (new, name);
	if (Derived.Init)
	{
		(*Derived.Init)(new);
	}
	return (new);
}



void
dt_FreePlatform (PlatformInstance *pi)
{
	dt_EraseSubPlats (pi);
	if (Derived.Destroy)
		(*Derived.Destroy)(pi);
	free (pi);
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



static int
dt_FindSubplat (pc, dps)
const PlatformClass *pc;
SubPlatform *dps;
/*
 * Look for this subplatform in the given class.  Return its index if found,
 * else -1.
 */
{
	int j;

	for (j = 0; j < pc->dpc_nsubplats; ++j)
		if ((pc->dpc_subplats[j].dps_class == dps->dps_class) &&
		    ! strcmp (pc->dpc_subplats[j].dps_name, dps->dps_name))
			return (j);
	return (-1);
}



static void
dt_InitInstance (PlatformInstance *new, const char *name)
/*
 * Initialize this platform instance structure
 */
{
	dt_SetString (new->dp_name, name, sizeof(new->dp_name),
		      "assigning instance name");
	new->dp_class = BadClass;
	new->dp_parent = BadPlatform;
	new->dp_subplats = NULL;
	new->dp_nsubplats = 0;
	new->dp_flags = 0;
}




const char*
pi_SuggestedDir (const Platform *p)
/* 
 * Determine our instance subdirectory given our class's directory 
 * instance flag.  The string returned is valid until the next call here.
 */
{
    static char dir[CFG_FILEPATH_LEN];
    const char *uplatname;
    const PlatformClass *pc = pi_Class (p);
    const Platform *parent;
    int parentlen;
/*
 * First find our unique platform name (the "normal" platform name, less the
 * parent portion if any).
 */
    if ((parent = pi_Parent (p)) != NULL)
	parentlen = strlen (pi_Name (parent)) + 1;   /* +1 is for the slash */
    else
	parentlen = 0;

    uplatname = pi_Name (p) + parentlen;
/*
 * Now choose based on the class directory instance flag
 */
    switch (pc_InstanceDirFlag (pc))
    {
    /*
     * InstanceRoot: <uplatname>
     */
      case InstanceRoot:	/* == InstanceDefault */
	strcpy (dir, uplatname);
	break;
    /*
     * InstanceCopyClass: <class_dir>
     */
      case InstanceCopyClass:
	strcpy (dir, pc_SuggestedDir (pc));
	break;
    /*
     * InstanceSubdirClass: <class_dir>/<uplatname>
     */
      case InstanceSubdirClass:
	sprintf (dir, "%s/%s", pc_SuggestedDir (pc), uplatname);
	break;
    /*
     * InstanceCopyParent: <parent_dir>
     */
      case InstanceCopyParent:
	if (! parent)
	    strcpy (dir, uplatname);
	else
	    pi_SuggestedDir (parent);	/* sets our static "dir" variable */
	break;
    /*
     * InstanceSubdirParent: <parent_dir>/<uplatname>
     */
      case InstanceSubdirParent:
	if (! parent)
	    strcpy (dir, uplatname);
	else
	{
	    pi_SuggestedDir (parent);	/* sets our static "dir" variable */
	    strcat (dir, "/");
	    strcat (dir, uplatname);
	}
	break;
    /*
     * Oops.  Bad InstanceDir value.
     */
      default:
	msg_ELog (EF_PROBLEM, 
		  "pi_SuggestedDir: Bad InstanceDir value %d for plat %s",
		  pc_InstanceDirFlag (pc), pi_Name (p));
	strcpy (dir, uplatname);
    }

    return (dir);
}



/*
 * Return non-zero if the platform instance is a member of the given class
 * or a member of a subclass of the given class.
 */
int
pi_IsSubclass (const PlatformInstance *pi, const PlatformClass *spc)
{
    int result = 0;
    const PlatformClass *pc = pi_Class (pi);
    while (pc && spc && ! (result = (pc_Id(pc) == pc_Id(spc))))
    {
	pc = pc_SuperClass (pc);
    }
    return result;
}


int pi_Daysplit (const PlatformInstance *pi)
{
    const PlatformClass *pc = pi_Class (pi);
    return (pc && pc->dpc_splitseconds == 24*3600);
}


int
dt_SetString (dest, src, maxlen, op)
char *dest;
const char *src;
int maxlen;
char *op;	       	/* optional operation for error message */
/*
 * Set the dest string to the src string.  If the src is longer than
 * maxlen, issue a warning and return nonzero, else return zero.
 * In either case, the dest will hold the first maxlen-1 characters
 * and be null terminated.
 */
{
	int max = 0;

	if (strlen(src) >= (unsigned) maxlen)
	{
		msg_ELog (EF_PROBLEM, 
			  "%s%sstring '%s' too long",
			  (op) ? op : "", (op) ? ": " : "", src);
		max = 1;
	}
   	strncpy (dest, src, maxlen - 1);
	dest[maxlen - 1] = '\0';
	return (max);
}




/* =====================================================================
 * A little experiment.  See how hard it is to write a routine which
 * decodes a class structure into clear text datastore config commands.  We
 * have to make some assumptions: anything which would ordinarily be
 * inherited from a superclass and is identical to the superclass is left
 * unspecified.  If the default derived directories would be identical to
 * the defined directories, leave them unspecified also.
 */

/*
 * Private prototypes used for platform decoding.
 */
static void dt_DecodeDirs FP ((FILE *fp, PlatformClass *spc, 
			       PlatformClass *pc));
static void dt_DecodeSubplats FP ((FILE *fp, const PlatformClass *spc,
				   const PlatformClass *pc));


int
dt_DecodeClass (fp, pc, spc)
FILE *fp;		/* file pointer to write text to */
const PlatformClass *pc; /* class to decode */
const PlatformClass *spc;/* the class's superclass, or null */
{
	fprintf (fp, "class %s %s\n", pc->dpc_name,
		 (spc ? spc->dpc_name : ""));
	if (!spc || (spc->dpc_org != pc->dpc_org))
		fprintf (fp, "\torganization\t%s\n", ds_OrgName(pc->dpc_org));
	if (!spc || (spc->dpc_ftype != pc->dpc_ftype))
		fprintf (fp, "\tfiletype\t%s\n", ds_FTypeName(pc->dpc_ftype));
	if (!spc || (spc->dpc_maxsamp != pc->dpc_maxsamp))
		fprintf (fp, "\tmaxsamples\t%d\n", pc->dpc_maxsamp);
	if (!spc || (spc->dpc_splitseconds != pc->dpc_splitseconds))
		fprintf (fp, "\tsplitseconds\t%d\n", pc->dpc_splitseconds);
	/*
	 * Flags: if inherited or the default, don't print the command.
	 */
#define NEEDFLAG(spc,pc,flag) (((spc) && (((spc)->dpc_flags & (flag)) != \
					  ((pc)->dpc_flags & (flag)))) || \
			       (!(spc) && (((pc)->dpc_flags & (flag)))))
	if (NEEDFLAG(spc,pc,DPF_MOBILE))
		fprintf (fp, "\tmobile\n");
	if (NEEDFLAG(spc,pc,DPF_COMPOSITE))
		fprintf (fp, "\tcomposite\n");
	if (NEEDFLAG(spc,pc,DPF_MODEL))
		fprintf (fp, "\tmodel\n");
	if (NEEDFLAG(spc,pc,DPF_REGULAR))
		fprintf (fp, "\tregular\n");
	if (NEEDFLAG(spc,pc,DPF_DISCRETE))
		fprintf (fp, "\tdiscrete\n");
#undef	NEEDFLAG
	/*
	 * Abstract and virtual are not inherited
	 */
	if ((pc->dpc_flags & DPF_ABSTRACT))
		fprintf (fp, "\tabstract\n");
	if ((pc->dpc_flags & DPF_VIRTUAL))
		fprintf (fp, "\tvirtual\n");
	/*
	 * Directories.  The instance and inherit methods are straightforward,
	 * the tough part is backing out the directories themselves.
	 */
	if ((spc && (spc->dpc_inherit != pc->dpc_inherit)) ||
	    (!spc && (pc->dpc_inherit != InheritDefault)))
		fprintf (fp, "\tinheritdir\t%s\n", 
			 ds_InheritDirFlagName (pc_InheritDirFlag (pc)));
	if ((spc && (spc->dpc_instance != pc->dpc_instance)) ||
	    (!spc && (pc->dpc_instance != InstanceDefault)))
		fprintf (fp, "\tinstancedir\t%s\n", 
			 ds_InstanceDirFlagName (pc_InstanceDirFlag (pc)));
	dt_DecodeDirs (fp, (PlatformClass *)spc, (PlatformClass *)pc);
	/*
	 * Subplatforms.
	 */
	dt_DecodeSubplats (fp, spc, pc);
	if (pc->dpc_comment)
		fprintf (fp, "\tcomment\t'%s'\n", pc->dpc_comment);
	/*
	 * Class fields.  Note the original definition may have used the
	 * shortcut of including a derivation in the field spec, but this
	 * doesn't try to figure that out.  The effect is the same.
	 */
	{
	    int i;
	    for (i = 0; i < pc->dpc_nfields; ++i)
	    {
		fprintf (fp, "\tfield\t'%s'\n", 
			 F_GetFullName(pc->dpc_fields[i]));
	    }
	}
	if (pc->dpc_derivations)
		fprintf (fp, "\tderivation\t'%s'\n", pc->dpc_derivations);
	fprintf (fp, "endclass %s\n", pc->dpc_name);
	return (0);
}




static void
dt_DecodeSubplats (fp, spc, pc)
FILE *fp;
const PlatformClass *spc;
const PlatformClass *pc;
/*
 * Subplatforms.  If the subclass is a superset of the superclass,
 * only add the subplats unique to the subclass.  Otherwise, use
 * 'subplats none' to block the superclass inheritance and explicitly
 * add all of the subclass's subplats.
 */
{
	int i;
	int superset = 1;

	/*
	 * superset is false iff there exists a superclass and one of the
	 * superclass's subplats is not also one of the subclass's subplats
	 */
	if (spc && (spc->dpc_nsubplats > 0))
	{
		for (i = 0; i < spc->dpc_nsubplats; ++i)
		{
			if (dt_FindSubplat (pc, spc->dpc_subplats+i) < 0)
				break;
		}
		if (i < spc->dpc_nsubplats)
			superset = 0;
	}
	/*
	 * If its a superset and the numbers are equal, then all the subplats
	 * are inherited and there is nothing for us to do, or neither the
	 * superclass or subclass have any subclasses.
	 */
	if (superset && ((spc && (pc->dpc_nsubplats == spc->dpc_nsubplats)) ||
			 (!spc && pc->dpc_nsubplats == 0)))
		return ;

	/* The subclass is missing some of its superclass's subplats */
	if (! superset)
		fprintf (fp, "\tsubplats none\n");

	/*
	 * Now add the subclass's subplats which are not also in the
	 * superclass.  If superset is false, we add all of them because
	 * of the explicit 'subplats none'.
	 */
	i = 0;
	while (i < pc->dpc_nsubplats)
	{
		PlatClassId id = pc->dpc_subplats[i].dps_class;
		const char *cname;
		int len;

		if (superset && spc && 
		    (dt_FindSubplat (spc, pc->dpc_subplats+i) >= 0))
		{
			++i;
			continue;
		}
		cname = ds_ClassName (id);
		fprintf (fp, "\tsubplats %s '%s'", cname,
			 pc->dpc_subplats[i].dps_name);
		len = strlen (cname) + strlen (pc->dpc_subplats[i].dps_name);
		++i;
		while ((i < pc->dpc_nsubplats) &&
		       (id == pc->dpc_subplats[i].dps_class) &&
		       (len + strlen(pc->dpc_subplats[i].dps_name) < 50))
		{
			if (!superset || !spc || 
			    (dt_FindSubplat (spc, pc->dpc_subplats+i) < 0))
			{
				fprintf (fp, " '%s'", 
					 pc->dpc_subplats[i].dps_name);
				len += strlen(pc->dpc_subplats[i].dps_name)+3;
			}
			++i;
		}
		fputc ('\n', fp);
	}
}




static void
dt_DecodeDirs (fp, spc, pc)
FILE *fp;
PlatformClass *spc;
PlatformClass *pc;
/* 
 * Find out what the default directory would be, and if identical to the
 * existing one, leave out the definition.  Otherwise, if DataDir appears
 * at the front, back it out.  The remainder goes in a directory command.
 */
{
	char dir[sizeof(pc->dpc_dir)];
	/* char rdir[sizeof(pc->dpc_rdir)]; */

	/*
	 * Save the current dirs and reset them to empty.
	 */
	strcpy (dir, pc->dpc_dir);
	pc->dpc_dir[0] = '\0';
	dt_FillClassDir (pc, spc);
	/*
	 * Local:
	 */
	if (strcmp (pc->dpc_dir, dir))
	{
		fprintf (fp, "\tdirectory\t'%s'\n", dir);
	}
	/*
	 * Finally, restore the class directories
	 */
	strcpy (pc->dpc_dir, dir);
	/* strcpy (pc->dpc_rdir, rdir); */
}

