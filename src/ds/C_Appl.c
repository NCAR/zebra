/*
 * API to class and platform definitions
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

#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <defs.h>
#include <zl_symbol.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "dsPrivate.h"
#include "Platforms.h"
#include "dslib.h"
#include "Appl.h"

RCSID ("$Id: C_Appl.c,v 3.2 1996-11-21 18:17:52 granger Exp $")


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
		spc = ds_GetClassStruct (sid, NULL);
		if (! spc)
		{
			msg_ELog (EF_PROBLEM, "superclass %d unknown", sid);
		}
	}
	pc = (PlatformClass *) malloc (sizeof (PlatformClass));
	if (spc)
		dt_Subclass (sid, spc, pc, name);
	else
		dt_InitClass (pc, name);
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
PlatClassRef pc;
PlatClassId subid;
const char *subname;
/*
 * Add a subplatform of the given name and class to this class
 */
{
	dt_AddClassSubPlat (pc, subid, subname);
}



void
ds_DestroyClass (pc)
PlatClassRef pc;
/*
 * Free an UNDEFINED class structure.  Class structures passed to DefineClass
 * are automatically freed.
 */
{
	dt_EraseClass (pc);
	free (pc);
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

	pc = ds_GetClassStruct (cid, NULL);
	if (!pc)
		return (-1);
	if (pc->dpc_superclass != BadClass)
	{
		spc = ds_GetClassStruct (pc->dpc_superclass, NULL);
		if (!spc)
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
ds_SetDirectory (cd, dir)
PlatClassRef cd;
const char *dir;
{
	dt_SetString (cd->dpc_dir, dir, sizeof (cd->dpc_dir), "SetDirectory");
}


void
ds_SetRemoteDir (cd, dir)
PlatClassRef cd;
const char *dir;
{
	dt_SetString(cd->dpc_rdir, dir, sizeof(cd->dpc_rdir), "SetRemoteDir");
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
InheritDir id;
{
	cd->dpc_inherit = id;
}	


void
ds_SetInstanceDir (cd, id)
PlatClassRef cd;
InstanceDir id;
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
ds_SetDaysplit (cd, split)
PlatClassRef cd;
int split;
{
	ds_SetClassFlag (cd, DPF_SPLIT, split);
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
	const PlatformClass *spc = NULL;
	int cid;
	int len;

	/*
	 * Set any necessary default directory paths.
	 */
	if (pc->dpc_superclass != BadClass)
		spc = ds_GetClassStruct (pc->dpc_superclass, NULL);
	dt_FillClassDirs (pc, spc);

	/*
	 * Validate
	 */
	dt_ValidateClass (pc);

	/*
	 * Now we proceed with definition according to our connection
	 */
	if (DSM.dsm_DefineClass)
	{
		cid = (*DSM.dsm_DefineClass) (pc);
	}
	else
	{
		struct dsp_ClassStruct cdm, *send;
		/*
		 * Inject the class structure into a dsp_ClassStruct
		 */
		send = dt_InjectClass (pc, &cdm, &len);
		send->dsp_type = dpt_DefineClass;
		ds_SendToDaemon (send, len);
		msg_Search (MT_DATASTORE, ds_AwaitPID, &cid);
		if (send != &cdm)
			free (send);
	}
	if (cid != BadClass)
	{
		ds_CacheClassName (pc->dpc_name, cid);
	}
	ds_DestroyClass (pc);
	return (cid);
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
	PlatformId pid;
	struct dsp_Instance im;

	if (DSM.dsm_DefinePlatform)
		return ((*DSM.dsm_DefinePlatform) (cid, name, parent));

	im.dsp_type = dpt_Instantiate;
	im.dsp_class = cid;
	dt_SetString (im.dsp_name, name, sizeof(im.dsp_name), 
		      "sending instantiation message");
	im.dsp_parent = parent;

	ds_SendToDaemon (&im, sizeof (im));
	msg_Search (MT_DATASTORE, ds_AwaitPID, &pid);
	/*
	 * Cache the full instance name(s) in the lookup table.
	 */
	{
		char iname[1024];
		ClientPlatform *cp = NULL;

		if ((parent != BadPlatform) &&
		    ((cp = ds_GetPlatStruct (parent, NULL, FALSE)) != NULL))
			sprintf (iname, "%s/%s", cp->cp_name, name);
		ds_CacheName ((cp) ? iname : name, pid);
	}
	return (pid);
}




PlatformId
ds_DefinePlatform (cid, name)
PlatClassId cid;
const char *name;
/*
 * Instantiate this platform class with the given name and no parent.
 */
{
	return (ds_DefineSubPlatform (cid, name, BadPlatform));
}



#ifdef notdef
void
ds_AddClassSubplat (cid, pclass, pname)
PlatClassId cid;
PlatClassId pclass;
char *pname;
{
	struct dsp_AddSubplat asp;
	SubPlatform *sp = &asp.dsp_subplat;

	sp->dps_class = pclass;
	dt_SetString (sp->dps_name, pname, sizeof (sp->dps_name), 
		      "add class subplat message");
	asp.dsp_class = cid;
	asp.dsp_type = dpt_AddSubplat;
	ds_SendToDaemon (&asp, sizeof (asp));
}
#endif

