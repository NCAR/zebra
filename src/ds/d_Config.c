/*
 * UI routines for dealing with the configuration process.
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
# include <string.h>
# include <stdio.h>
# include <stdlib.h>

# include <ui.h>
# include <defs.h>
# include <message.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "Platforms.h"
# include "dsDaemon.h"
# include "commands.h"
# include <ui_error.h>

RCSID("$Id: d_Config.c,v 2.15 1996-11-19 09:25:21 granger Exp $")

/*-----------------------------------------------------------------------
 * Local forwards.
 */
static int dc_InPlatformClass FP ((PlatformClass *, struct ui_command *));
static void dc_AddSubPlats FP ((PlatformClass *pc, char *classname,
				struct ui_command *cmds));

/*-----------------------------------------------------------------------*/


void
dc_DefPlatform (name, superclass)
char *name;
char *superclass;	/* NULL if not specified */
/*
 * Define a platform in the data store configuration.
 */
{
	PlatformInstance *plat;
	PlatformClass *pc;
/*
 * Defining a platform implicitly defines a class first, then instantiates
 * that class with the same name.  So first define a like-named class.
 */
	dc_DefPlatformClass (name, superclass, TRUE);
	pc = dt_FindClass (name);
/*
 * The directory of the platform will be the same as the class directory,
 * regardless of whether we inherited anything from a superclass.
 */
	pc->dpc_instance = InstanceCopyClass;
/*
 * Finally, instantiate this platform.
 */
	plat = dt_Instantiate (pc, /*parent*/BadPlatform, name);
/*
 * In case this is an on-the-fly definition, scan the platform directory
 */
	if (! InitialScan && plat)
		RescanPlat (plat);
}



void
dc_DefPlatformClass (name, superclass, platform)
char *name;
char *superclass;	/* NULL if not specified */
bool platform;		/* Implicit class creation from a platform command? */
/*
 * Define a platform in the data store configuration.
 */
{
	PlatformClass *pc;
	static char *next_state;	/* static to avoid gcc warning
					   "might be clobbered by 'longjmp'" */
/*
 * Grab a platform table entry for this guy.
 */
	if (ParseOnly)
		printf ("----->Defining%s class '%s', superclass '%s'\n",
			((platform) ? " implicit" : ""), name, 
			((superclass != NULL) ? superclass : "none"));
	pc = dt_NewClass (name, superclass);
	next_state = (platform) ? "in-platform" : "in-class";
/*
 * Now go pick up all the pieces.
 */
	ERRORCATCH
		ui_subcommand (next_state, "Class>", dc_InPlatformClass, 
			(long) pc);
	ENDCATCH
/*
 * The rest is pointless unless we got a class structure.
 */
	if (pc)
	{
	/*
	 * Set any necessary default directory paths.
	 */
		dt_FillClassDirs (pc, pc_SuperClass (pc));
	/*
	 * Some day we may remove this class or flag it if its wrong.
	 * For now there will only be a warning about invalid classes.
	 */
		dt_ValidateClass (pc);
		if (ParseOnly)
		{
			dbg_DumpClass (pc);
			printf ("----->Finished defining class %s\n", name);
		}
	}
}



void
dc_SubPlatform (cmds)
struct ui_command *cmds;
/*
 * Define a subplatform.  Define a 'subplatform' class for the parent,
 * (unless already defined), and then use that class in a call dc_DefSubPlats.
 */
{
	PlatformClass *spc, *parent_class;
	PlatformInstance *parent;
	PlatformId ppid;
	char spcname[512];
/*
 * Find our parent platform first.
 */
	if (! (parent = dt_FindInstance (UPTR (*cmds))))
	{
		msg_ELog (EF_PROBLEM, "Unknown parent platform '%s'",
			  UPTR (*cmds));
		return;
	}
	ppid = parent - PTable;
/*
 * Now try to find the subplatform class, else define it now.
 */
	sprintf (spcname, "%s.subplatform", parent->dp_name);
	spc = dt_FindClass (spcname);
	if (! spc)
	{
	/*
	 * Define the subplatform class, which is just a tweaked version
	 * of the parent's class (so start with a subclass of the parent's
	 * class).
	 */
		if (ParseOnly)
			printf ("----->Defining subplatform class %s\n",
				spcname);
		parent_class = CTable + parent->dp_class;
		spc = dt_NewClass (spcname, parent_class->dpc_name);
		if (! spc)	/* oops, some other time perhaps... */
			return;
		spc->dpc_inherit = InheritNone;
		spc->dpc_instance = InstanceCopyParent;
		spc->dpc_org = OrgScalar;
		spc->dpc_flags &= ~DPF_COMPOSITE;
		spc->dpc_flags |= DPF_SUBPLATFORM;
		dt_FillClassDirs (spc, pc_SuperClass (spc));

		if (ParseOnly)
		{
			dbg_DumpClass (spc);
			printf ("----->Finished defining class %s\n", spcname);
		}
	}
/*
 * Now pass the work on to the subplats command.
 */
	dc_DefSubPlats (pi_Name(PTable + ppid), spc->dpc_name, cmds+1);

}



static int
dc_InPlatformClass (pc, cmds)
PlatformClass *pc;
struct ui_command *cmds;
/*
 * Deal with an internal definition for this platform.
 */
{
	/*
	 * Check whether we're just skipping through a bad definition.
	 */
	if (! pc)
		return (TRUE);

	switch (UKEY (*cmds))
	{
	/*
	 * Maybe we're done.  In which case, verify the name for them.
	 */
	   case DK_ENDPLATFORM:
	   case DK_ENDCLASS:
		if ((cmds[1].uc_ctype != UTT_END) && 
		    (strcmp(UPTR(cmds[1]), pc->dpc_name)))
		{
			msg_ELog (EF_PROBLEM, 
			  "%s: class name %s does not match %s",
			  cmds[0].uc_text, pc->dpc_name, UPTR(cmds[1]));
		}
	   	return (FALSE);
	/*
	 * They want to tell us about the file organization.
	 */
	   case DK_ORGANIZATION:
	   	pc->dpc_org = (DataOrganization) UINT (cmds[1]);
		break;
	/*
	 * ...or the file type.
	 */
	   case DK_FILETYPE:
	   	pc->dpc_ftype = (FileType) UINT (cmds[1]);
		break;
	/*
	 * Keep time, in minutes.
	 */
	   case DK_KEEP:
	   	pc->dpc_keep = InterpDTime (UPTR (cmds[1]))*60;
		break;
	/*
	 * Maximum samples.
	 */
	   case DK_MAXSAMPLES:
		if (UINT (cmds[1]) > (unsigned int) 65535)
		{
			msg_ELog (EF_PROBLEM, "%s: maxsamples %u too large",
				  pc->dpc_name, UINT (cmds[1]));
			pc->dpc_maxsamp = 65535;
		}
		else
			pc->dpc_maxsamp = UINT (cmds[1]);
		break;
	/*
	 * Various flags.
	 */
	   case DK_REGULAR:	pc->dpc_flags |= DPF_REGULAR; break;
	   case DK_MOBILE:	pc->dpc_flags |= DPF_MOBILE; break;
	   case DK_COMPOSITE:	pc->dpc_flags |= DPF_COMPOSITE; break;
	   case DK_DISCRETE:	pc->dpc_flags |= DPF_DISCRETE; break;
	   case DK_MODEL:	pc->dpc_flags |= DPF_MODEL; break;
	   case DK_ABSTRACT:	pc->dpc_flags |= DPF_ABSTRACT; break;
	   case DK_VIRTUAL:	pc->dpc_flags |= DPF_VIRTUAL; break;
	/*
	 * Where the data lives.
	 */
	   case DK_DIRECTORY:
		dt_SetString (pc->dpc_dir, UPTR(cmds[1]), sizeof(pc->dpc_dir),
			      "data directory");
		break;
	/*
	 * Where remote data lives.
	 */
	   case DK_REMOTE:
		if (! DisableRemote)
		{
			dt_SetString (pc->dpc_rdir, UPTR(cmds[1]),
				      sizeof (pc->dpc_rdir), "remote dir");
		}
		break;
	/*
	 * Where instances of this class are put
	 */
	   case DK_INSTANCEDIR:
		switch (UKEY(cmds[1]))
		{
		   case DK_COPYCLASS:
			pc->dpc_instance = (InstanceDir)InstanceCopyClass; 
			break;
		   case DK_SUBDIRCLASS:
			pc->dpc_instance = (InstanceDir)InstanceSubdirClass; 
			break;
		   case DK_COPYPARENT:
			pc->dpc_instance = (InstanceDir)InstanceCopyParent; 
			break;
		   case DK_SUBDIRPARENT: 
			pc->dpc_instance = (InstanceDir)InstanceSubdirParent; 
			break;
		   case DK_DEFAULT:
		   case DK_ROOT:
			pc->dpc_instance = (InstanceDir)InstanceDefault; 
			break;
		}
		break;
	/*
	 * How to inherit directories in subclasses
	 */
	   case DK_INHERITDIR:
		switch (UKEY(cmds[1]))
		{
		   case DK_APPEND:
			pc->dpc_inherit = (InheritDir)InheritAppend; 
			break;
		   case DK_COPY:
			pc->dpc_inherit = (InheritDir)InheritCopy; 
			break;
		   case DK_NONE:
			pc->dpc_inherit = (InheritDir)InheritNone; 
			break;
		}
		break;
	/*
	 * Split files across days.
	 */
	   case DK_DAYSPLIT:
	   	pc->dpc_flags |= DPF_SPLIT;
		break;
	/*
	 * Subplat templates to be added.
	 */
	   case DK_SUBPLATS:
		if (cmds[1].uc_ctype == UTT_KW) /* None */
			dt_EraseClassSubPlats (pc);
		else
			dc_AddSubPlats (pc, UPTR(cmds[1]), cmds+2);
		break;
	/*
	 * Comments for this class.
	 */
	   case DK_COMMENT:
		dt_SetComment (pc, UPTR(cmds[1]));
		break;
	}
	return (TRUE);
}



void
dc_DefSubPlats (target, classname, cmds)
char *target;			/* Class or instance adding subplats to */
char *classname;		/* Class of these subplatforms 	*/
struct ui_command *cmds;	/* Instance names		*/
/*
 * Define some new subplats.  If the target is an instance, instantiate
 * the platforms in the list and add them to the parent instance.
 * Otherwise, if the target is a class, add the subplats to the class
 * as SubPlatform templates.
 */
{
	PlatformId ppid = BadPlatform;
	PlatformInstance *plat = NULL;
	PlatformClass *pc = NULL;
	PlatformClass *spc = NULL;
/*
 * Find the appropriate platform or class entry for our parent.
 */
	if ((plat = dt_FindInstance (target)) != NULL)
		ppid = plat - PTable;
	else
		pc = dt_FindClass (target);
	if (!plat && !pc)
	{
		msg_ELog (EF_PROBLEM, "subplats target '%s' not a known %s",
			  target, "class or instance");
		return ;
	}
	spc = dt_FindClass (classname);
	if (!spc)
	{
		msg_ELog (EF_PROBLEM, "subplats class '%s' for %s unknown",
			  classname, target);
		return;
	}
/*
 * Do each subplatform in this command line.  The class pointer is valid 
 * for all of the instantiations, but the parent platform may move if the
 * table is expanded.  Therefore we refer to the parent by ID rather than
 * a pointer.
 */
	if (ppid != BadPlatform)
	{
		for ( ; cmds->uc_ctype != UTT_END; ++cmds)
			dt_DefSubPlat (ppid, spc, UPTR (*cmds));
	}
	else
	{
		for ( ; cmds->uc_ctype != UTT_END; ++cmds)
			dt_AddClassSubPlat (pc, spc - CTable, UPTR (*cmds));
	}
}



static void
dc_AddSubPlats (pc, classname, cmds)
PlatformClass *pc;		/* Class to add subplats to	*/
char *classname;		/* Class of the subplatforms 	*/
struct ui_command *cmds;	/* Instance names		*/
/*
 * Add some SubPlatform templates to a class.
 */
{
	PlatformClass *spc;

	spc = dt_FindClass (classname);
	if (!spc)
	{
		msg_ELog (EF_PROBLEM, "subplats class '%s' for %s unknown",
			  classname, pc->dpc_name);
		return;
	}
/*
 * Do each subplatform name on the command line.
 */
	for ( ; cmds->uc_ctype != UTT_END; ++cmds)
	{
		dt_AddClassSubPlat (pc, spc - CTable, UPTR (*cmds));
	}
}



void
dc_DefInstances (classname, cmds)
char *classname;		/* Name of class to create instances of */
struct ui_command *cmds;	/* Name of the instances		*/
/*
 * Traverse the list of names in the cmds list, and create an instance for
 * each one using the given class.
 */
{
	PlatformClass *pc;
/*
 * Find the class they are asking for.  This pointer is valid through all
 * of the instantiations since dt_Instantiate does not create any classes.
 */
	pc = dt_FindClass (classname);
	if (! pc)
	{
		msg_ELog (EF_PROBLEM, "instance: class %s not defined",
			  classname);
		return;
	}
/*
 * Debuggery.
 */
	if (ParseOnly)
	{
		int i;

		printf ("Instantiating class %s:", pc->dpc_name);
		for (i = 0; cmds[i].uc_ctype != UTT_END; ++i)
		{
			printf (" %s", UPTR(cmds[i]));
		}
		printf ("\n");
	}
/*
 * Now go through and actually create the platforms.  In each case,
 * also force a scan if this is not the initial file read.
 */
	for ( ; cmds->uc_ctype != UTT_END; cmds++)
	{
		/* Platform *newpl = */
		dt_Instantiate (pc, BadPlatform, UPTR(*cmds));
#ifdef notdef
		if (newpl && ! InitialScan)
			RescanPlat (newpl);
#endif
	}
}

