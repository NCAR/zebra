/*
 * Routines for manipulating class definition structures.
 * Meant to be the basic level of access to be shared by daemon and client
 * alike.  Instantiation is different between them so it's not included.
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
# include <zl_symbol.h>	/* for searching platform name tables */
# include <zl_regex.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "Platforms.h"

RCSID("$Id: Platforms.c,v 3.5 1998-12-17 17:17:51 burghart Exp $")


/*
 * The public default time period to keep data
 */
int DefaultKeep = 60;

/*
 * Default data directory.
 */
char DefDataDir[DDIR_LEN];
char RemDataDir[DDIR_LEN];
zbool DisableRemote = FALSE;	/* Disable use of remote directories 	*/

/*
 * Public access to names of enumerated types
 */
static const char *_OrgName[] =
{
	"unknown",	/* OrgUnknown	= 0 */
	"2dgrid",	/* Org2dGrid	= 1 */
	"irgrid",	/* OrgIRGrid	= 2 */
	"scalar", 	/* OrgScalar	= 3 */
	"image",	/* OrgImage	= 4 */
	"outline", 	/* OrgOutline	= 5 */
	"3dgrid",	/* Org3dGrid	= 6 */
	"cmpimage",	/* OrgCmpImage	= 7 */
        "1dgrid",	/* Org1dGrid       = 8 */
	"transparent",	/* OrgTransparent  = 9 */
	"fixedscalar",	/* OrgFixedScalar  = 10 */
	"nspace" 	/* OrgNSpace	= 11 */
};
#define OrgName(org) (_OrgName[(org)])
#define NOrg (sizeof(_OrgName)/sizeof(char *))

const char *
ds_OrgName (org)
DataOrganization org;
{
	return ((org >= 0 && org < NOrg) ? OrgName(org) : OrgName(OrgUnknown));
}

static const char *_FTypeName[] =
{
	"unknown",	/* FTUnknown = -1 */
	"netcdf",	/* FTNetCDF = 0 */
	"boundary",	/* FTBoundary = 1 */
	"raster",	/* FTRaster = 2 */
	"compressed_raster", /* FTCmpRaster = 3 */
	"zebra",	/* FTZebra = 4 */
	"grib",		/* FTGRIB = 5 */
	"grib_sfc", 	/* FTGRIBSfc = 6 */
	"grads",	/* FTGrads = 7 */
	"grads_model",  /* FTGradsModel = 8 */
	"hdf"		/* FTHDF = 9 */
};
#define FTypeName(ftype) (_FTypeName[(ftype)+1])
#define NFType ((sizeof(_FTypeName)/sizeof(char *)))

const char *
ds_FTypeName (ft)
FileType ft;
{
	return ((ft >= -1 && ft < NFType) ? FTypeName(ft) : 
		FTypeName(FTUnknown));
}

static const char *_InheritDirName[] =
{
	"none",		/* InheritNone = 0 */
	"append",	/* InheritAppend */
	"copy"		/* InheritCopy */
};
#define InheritDirName(id) (_InheritDirName[(id)])
#define NInheritDir ((sizeof(_InheritDirName)/sizeof(char *)))

const char *
ds_InheritDirName (id)
InheritDir id;
{
	return ((id >= 0 && id < NInheritDir) ? InheritDirName(id) : 
		InheritDirName(InheritDefault));
}


static const char *_InstanceDirName[] =
{
	"root", 	/* InstanceDefault = 0, InstanceRoot = 0 */
	"copyclass",	/* InstanceCopyClass */
	"subdirclass",	/* InstanceSubdirClass */
	"copyparent",	/* InstanceCopyParent */
	"subdirparent"	/* InstanceSubdirParent */
};
#define InstanceDirName(id) (_InstanceDirName[(id)])
#define NInstanceDir ((sizeof(_InstanceDirName)/sizeof(char *)))

const char *
ds_InstanceDirName (id)
InstanceDir id;
{
	return ((id >= 0 && id < NInstanceDir) ? InstanceDirName(id) : 
		InstanceDirName(InstanceDefault));
}



/*
 * Private prototypes
 */
static void dt_DecodeDirs FP ((FILE *fp, PlatformClass *spc, 
			       PlatformClass *pc));
static void dt_DecodeSubplats FP ((FILE *fp, const PlatformClass *spc,
				   const PlatformClass *pc));


void
dt_InitDirectories ()
{
	char *dir;

	strcpy (DefDataDir, GetDataDir());
	RemDataDir[0] = '\0';
	DisableRemote = FALSE;
	if ((dir = getenv("DS_DATA_DIR")) != NULL)
	{
		strcpy (DefDataDir, dir);
	}
}


void
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
	pc->dpc_rdir[0] = '\0';

	pc->dpc_superclass = BadClass;
	pc->dpc_org = OrgUnknown;

	pc->dpc_ftype = FTUnknown;
	pc->dpc_keep = DefaultKeep;
	pc->dpc_maxsamp = 60;
	pc->dpc_flags = 0;
	pc->dpc_inherit = InheritDefault;	/* InheritNone */
	pc->dpc_instance = InstanceDefault;	/* InstanceRoot */
	pc->dpc_comment = NULL;
	pc->dpc_subplats = NULL;
	pc->dpc_nsubplats = 0;
}



void
dt_Subclass (superid, super, sub, name)
PlatClassId superid;
const PlatformClass *super;
PlatformClass *sub;
const char *name;	/* name to give to the subclass */
/*
 * Copy the super class to the sub class, taking care of details like
 * copying subplats and resetting the comment and subclass name.
 */
{
	dt_CopyClass (sub, super);
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
	sub->dpc_superclass = superid;
/* 
 * Erase our directories since these will be given defaults later
 */
	sub->dpc_dir[0] = '\0';
	sub->dpc_rdir[0] = '\0';
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
	if (pc->dpc_comment)
		free (pc->dpc_comment);
	dt_InitClass (pc, NULL);
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
dt_AddClassSubPlat (pc, sid, name)
PlatformClass *pc;
PlatClassId sid;		/* id of subplatform class */
const char *name;
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



SubPlatform *
dt_NewClassSubPlat (pc)
PlatformClass *pc;
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



zbool
dt_ValidateClass (pc)
PlatformClass *pc;
/*
 * Make sure this platform class is properly filled out. This means
 * making sure the organization and filetype have been specified, at a
 * minimum.  Of course, if this class is abstract only, we don't care
 * what's in it since the subclasses may fill in the rest.
 *
 * And how could I forget: subplatforms of course have but one
 * requirement at the moment, that they be scalar.
 */
{
	zbool valid = TRUE;

	if (pc->dpc_flags & DPF_ABSTRACT)
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
	if (!DisableRemote && !pc->dpc_rdir)
	{
		msg_ELog (EF_DEBUG, "class %s: remote enabled but %s",
			  pc->dpc_name, "no remote data directory");
		/* just a warning, still valid class */
	}
	return (valid);
}



struct dsp_ClassStruct *
dt_InjectClass (pc, am, retlen)
PlatformClass *pc;		/* the class to serialize */
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
	 * The length we calculate first is a maximum. We'll adjust it
	 * later to fit the space we actually used.
	 */
	if (pc->dpc_nsubplats > 1)
		len += (pc->dpc_nsubplats - 1) * sizeof (SubPlatform);
	if (pc->dpc_comment)
	{	
		len += strlen (pc->dpc_comment) + 1;
	}
	if (len > sizeof (struct dsp_ClassStruct))
		answer = (struct dsp_ClassStruct *) malloc (len);
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
		strcpy ((char *)answer + len, pc->dpc_comment);
		len += strlen (pc->dpc_comment) + 1;
	}
	*retlen = len;
	if (answer != am)
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
	pc->dpc_comment = NULL;
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
	if (c < len)		/* still have a comment to include */
	{
		dt_SetComment (pc, (char *)dsp + c);
		c += strlen (pc->dpc_comment) + 1;
	}
	/*
	 * Add future trailers here.
	 */
	return (1);
}




void
dt_FillClassDirs (pc, super)
PlatformClass *pc;
const PlatformClass *super;
/*
 * Fill in the directories if the definition hasn't done it already.
 * If the directory is relative, use the inherit flags to determine whether
 * to prepend DataDir, the prepend the superclass dir, or copy the superclass
 * dir.  If its absolute, use the full path name without modifying it.
 * If the dir is nonexistent and inheritance is none, use DataDir/name.
 *
 * For remote directories, if remote is disabled or the remote directory
 * is absolute, we do nothing.
 * If we have no default remote datadir, there is nothing we *can* do.
 * Otherwise, prepend the default remote dir to either the name or the
 * relative path.
 */
{
	char newdir[512];
	char *dirname;

	dirname = (pc->dpc_dir[0] == '\0') ? pc->dpc_name : pc->dpc_dir;

	if (pc->dpc_dir[0] != '/')	/* absolute paths don't change */
	{
		if  (((InheritDir)pc->dpc_inherit == InheritNone) ||
		     (super == NULL))
		{
			sprintf (newdir, "%s/%s", DefDataDir, dirname);
		}
		else if ((InheritDir)pc->dpc_inherit == InheritCopy)
		{
			sprintf (newdir, "%s", super->dpc_dir);
		}
		else 	/* InheritAppend */
		{
			sprintf (newdir, "%s/%s", super->dpc_dir, dirname);
		}
		dt_SetString (pc->dpc_dir, newdir, sizeof(pc->dpc_dir),
			      "class data directory");
	}
	if (DisableRemote || (pc->dpc_rdir[0] == '/'))
		return;
	dirname = (pc->dpc_rdir[0] == '\0') ? pc->dpc_name : pc->dpc_rdir;
	if (RemDataDir[0] != '\0')
	{
		if  (((InheritDir)pc->dpc_inherit == InheritNone) ||
		     (super == NULL))
		{
			sprintf (newdir, "%s/%s", RemDataDir, dirname);
		}
		else if ((InheritDir)pc->dpc_inherit == InheritCopy)
		{
			sprintf (newdir, "%s", super->dpc_rdir);
		}
		else 	/* InheritAppend */
		{
			sprintf (newdir, "%s/%s", super->dpc_rdir, dirname);
		}
		dt_SetString (pc->dpc_rdir, newdir, sizeof(pc->dpc_rdir),
			      "class remote data directory");
	}
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

	len = (plat->dp_nsubplats + ALLOC_SUBPLATS) * sizeof(int);
	if (plat->dp_nsubplats == 0)
		plat->dp_subplats = (int *) malloc (len);
	else if ((plat->dp_nsubplats % ALLOC_SUBPLATS) == 0)
		plat->dp_subplats = (int *)
			realloc (plat->dp_subplats, len);
	plat->dp_subplats[plat->dp_nsubplats++] = subid;
}




/*
 * All the logic for deriving instance directories is in this function.
 * It does not use any instance structures so that both daemon and client
 * can use it to fill in their own instance structures.
 */
void
dt_FillDirs (pc, defname, dir, rdir, pdir, prdir)
const PlatformClass *pc;
const char *defname;	/* Defined name, doesn't include parent if a subplat */
char *dir;		/* Local directory to set */
char *rdir;		/* Remote directory to set */
const char *pdir;	/* Parent local directory, or NULL */
const char *prdir;	/* Parent remote directory, or NULL */
/* 
 * Determine our instance directory given our directory instance flags.
 * Otherwise we do the default of using a subdirectory of datadir.
 *
 * If the class specifies subdir, the instance directory is a subdir of the
 * class directory (DataDir/<class name> unless the class has a directory).
 *
 * If the class specified samedir, the instance directory is the same 
 * directory as the class (DataDir/<class name> unless the class has a 
 * valid directory).
 *
 * Platforms being instantiated via 'platform', whose class has the same
 * name, will have samedir set by default, so that the instance will
 * automatically be where it has always been.
 */
{
	char newdir[512];
	const char *dirname;

	dirname = defname;
/*
 * First lets do the local directory.
 */
	if ((InstanceDir)pc->dpc_instance == InstanceDefault ||
	    (InstanceDir)pc->dpc_instance == InstanceRoot)
		/* InstanceDefault and InstanceRoot */
		sprintf (newdir, "%s/%s", DefDataDir, dirname);
	else if ((InstanceDir)pc->dpc_instance == InstanceCopyClass)
		/* InstanceCopyClass */
		sprintf (newdir, "%s", pc->dpc_dir);
	else if ((InstanceDir)pc->dpc_instance == InstanceSubdirClass)
		/* InstanceSubdirClass */
		sprintf (newdir, "%s/%s", pc->dpc_dir, dirname);
	else if (pdir == NULL)
		/* No parent for parent instance types */
		sprintf (newdir, "%s/%s", DefDataDir, dirname);
	else if ((InstanceDir)pc->dpc_instance == InstanceCopyParent)
		/* InstanceCopyParent */
		sprintf (newdir, "%s", (char *) pdir);
	else
		/* InstanceSubdirParent */
		sprintf (newdir, "%s/%s", (char *) pdir, dirname);

	dt_SetString (dir, newdir, sizeof(pc->dpc_dir),
		      "instance data directory");
/*
 * Now we have to do the remote directory, but only if enabled,
 * and if this isn't a subplatform class.
 */
	if (DisableRemote || (pc->dpc_flags & DPF_SUBPLATFORM))
		return;
/*
 * Otherwise we treat remote dirs just like the data dirs above.  If 
 * RemDataDir is empty, then we must have a remote directory from our
 * parent or class in order to generate a new remote directory.
 */
	newdir[0] = '\0';
	if (((InstanceDir)pc->dpc_instance == InstanceDefault ||
	    (InstanceDir)pc->dpc_instance == InstanceRoot) && RemDataDir[0])
		/* InstanceDefault and InstanceRoot */
		sprintf (newdir, "%s/%s", RemDataDir, dirname);
	else if (((InstanceDir)pc->dpc_instance == InstanceCopyClass) 
		 && (pc->dpc_rdir[0]))
		/* InstanceCopyClass */
		sprintf (newdir, "%s", pc->dpc_rdir);
	else if (((InstanceDir)pc->dpc_instance == InstanceSubdirClass) 
		 && (pc->dpc_rdir[0]))
		/* InstanceSubdirClass */
		sprintf (newdir, "%s/%s", pc->dpc_rdir, dirname);
	else if (prdir == NULL)
	{
		/* No parent for parent instance types */
		if (RemDataDir[0])
			sprintf (newdir, "%s/%s", RemDataDir, dirname);
		else
			newdir[0] = '\0';
	}
	else if (((InstanceDir)pc->dpc_instance == InstanceCopyParent)
		 && (prdir[0]))
		/* InstanceCopyParent */
		sprintf (newdir, "%s", (char *) prdir);
	else if (prdir[0])
		/* InstanceSubdirParent */
		sprintf (newdir, "%s/%s", (char *) prdir, dirname);

	if (newdir[0])
	{
		dt_SetString (rdir, newdir, sizeof(pc->dpc_rdir),
			      "instance remote data directory");
	}
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



/*
 * A little experiment.  See how hard it is to write a routine which
 * decodes a class structure into clear text datastore config commands.  We
 * have to make some assumptions: anything which would ordinarily be
 * inherited from a superclass and is identical to the superclass is left
 * unspecified.  If the default derived directories would be identical to
 * the defined directories, leave them unspecified also.
 */

int
dt_DecodeClass (fp, pc, spc)
FILE *fp;		/* file pointer to write text to */
const PlatformClass *pc; /* class to decode */
const PlatformClass *spc;/* the class's superclass, or null */
{
	fprintf (fp, "class %s %s\n", pc->dpc_name,
		 (spc ? spc->dpc_name : ""));
	if (!spc || (spc->dpc_org != pc->dpc_org))
		fprintf (fp, "\torganization\t%s\n", OrgName(pc->dpc_org));
	if (!spc || (spc->dpc_ftype != pc->dpc_ftype))
		fprintf (fp, "\tfiletype\t%s\n", FTypeName(pc->dpc_ftype));
	if (!spc || (spc->dpc_maxsamp != pc->dpc_maxsamp))
		fprintf (fp, "\tmaxsamples\t%d\n", pc->dpc_maxsamp);
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
	if (NEEDFLAG(spc,pc,DPF_SPLIT))
		fprintf (fp, "\tdaysplit\n");
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
			 InheritDirName(pc->dpc_inherit));
	if ((spc && (spc->dpc_instance != pc->dpc_instance)) ||
	    (!spc && (pc->dpc_instance != InstanceDefault)))
		fprintf (fp, "\tinstancedir\t%s\n", 
			 InstanceDirName(pc->dpc_instance));
	dt_DecodeDirs (fp, (PlatformClass *)spc, (PlatformClass *)pc);
	/*
	 * Subplatforms.
	 */
	dt_DecodeSubplats (fp, spc, pc);
	if (pc->dpc_comment)
		fprintf (fp, "\tcomment\t'%s'\n", pc->dpc_comment);
	fprintf (fp, "endclass %s\n", pc->dpc_name);
	return (0);
}



int
dt_FindSubplat (pc, dps)
const PlatformClass *pc;
SubPlatform *dps;
/*
 * Look for this subplatform in the given class.  Return its index if found,
 * else -1
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



static char *
dt_ExtractDir (base, dir, cmd)
char *base;
char *dir;
{
	char *c = dir;
	int len = strlen(base);

	/* try to back out base dir */
	if (len && !strncmp (base, dir, len) &&
	    len != strlen(dir))
	{
		c += len;
		while ((*c) && (*c == '/'))	/* skip slashes */
			++c;
	}
	return ((*c) ? c : NULL);
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
	char rdir[sizeof(pc->dpc_rdir)];

	/*
	 * Save the current dirs and reset them to empty.
	 */
	strcpy (dir, pc->dpc_dir);
	strcpy (rdir, pc->dpc_rdir);
	pc->dpc_dir[0] = '\0';
	pc->dpc_rdir[0] = '\0';
	dt_FillClassDirs (pc, spc);
	/*
	 * Local:
	 */
	if (strcmp (pc->dpc_dir, dir))
	{
		char *c;

		if ((c = dt_ExtractDir (DefDataDir, dir)))
			fprintf (fp, "\tdirectory\t'%s'\n", c);
	}
	/*
	 * Remote:
	 */
	if (strcmp (pc->dpc_rdir, rdir))
	{
		char *c;

		if ((c = dt_ExtractDir (RemDataDir, rdir)))
			fprintf (fp, "\tremote\t'%s'\n", c);
	}
	/*
	 * Finally, restore the class directories
	 */
	strcpy (pc->dpc_dir, dir);
	strcpy (pc->dpc_rdir, rdir);
}




int
dt_CreateDataDir (dir, name, flags)
const char *dir;
const char *name;
unsigned short *flags;
/*
 * Check whether this platform's local data directory exists, and if not
 * try to create it.  If upon exit the directory exists and is accessible,
 * set the platform's DPF_DIREXISTS flag so that we don't try this again.
 * The flags parameter is ignored if NULL.
 */
{
	int succeed = 0;
	int check;

	check = access (dir, R_OK | W_OK | X_OK);
	if (check == 0)
	{
		succeed = 1;
	}
	else if (errno == EACCES)
	{
		msg_ELog (EF_PROBLEM, "Access denied to path %s", dir);
	}
	else if (errno == ENOENT)
	{
		if ((succeed = dt_MakeDataDir (dir)) != 0)
			msg_ELog (EF_INFO, "Created data dir %s (plat %s)",
				  dir, name);
		else
			msg_ELog (EF_PROBLEM, "Cannot create dir %s (plat %s)",
				  dir, name);
	}
	else
	{
		msg_ELog (EF_PROBLEM, "access() error %d checking dir %s",
			  errno, dir);
	}

	if (succeed && flags)
	{
		*flags |= DPF_DIREXISTS;
		*flags |= DPF_DIRTY;
	}
	return (succeed);
}




int
dt_MakeDataDir (dir)
const char *dir;
/*
 * Try to make the data directory.
 */
{
	char tmp[512];
	const char *slash = dir;
/*
 * Go through and try to make all of the parent directories.
 */
	while ((slash = (const char *) strchr (slash + 1, '/')))
	{
		strncpy (tmp, dir, slash - dir);
		tmp[slash - dir] = '\0';
		mkdir (tmp, 0777);
	}
/*
 * Now try for the whole thing.
 */
	return (mkdir (dir, 0777) == 0);
}




void
ds_SearchPlatTable (table, function, req, pids, npids)
void *table;
int (*function)();
struct dsp_PlatformSearch *req;
PlatformId *pids;
int *npids;
{
	char *re_result;
	struct SearchInfo info;
/*
 * Store the info for the matching function
 */
	info.si_req = req;
	info.si_pids = pids;
	info.si_npids = 0;
	*npids = 0;
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
		zl_search ((stbl) table, function, (long)&info, 
			   req->dsp_alphabet, NULL);
		*npids = info.si_npids;
	}
}



