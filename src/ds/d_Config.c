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

RCSID("$Id: d_Config.c,v 2.20 2002-01-19 06:50:02 granger Exp $")

/*-----------------------------------------------------------------------
 * Local forwards.
 */
static void dc_DefInstances (const char *classname, struct ui_command *cmds);
static void dc_DefSubPlats (const char *target, const char *classname, 
			    struct ui_command *cmds);
static void dc_SubPlatform (struct ui_command *cmds);
static PlatClassRef dc_DefPlatformClass (char *name, char *, zbool platform);
static void dc_DefPlatform (char *name, char *superclass);
static int dc_InPlatformClass FP ((PlatformClass *, struct ui_command *));
static void dc_AddSubPlats FP ((PlatformClass *pc, char *classname,
				struct ui_command *cmds));
static void dc_AddField (PlatformClass *pc, char *fname);
static void dc_AddDerivation (PlatformClass *pc, char *dtext);
/*-----------------------------------------------------------------------*/



/*
 * This is our one public function!  Through here we handle UI commands for
 * platforms and the various platform and class definition states.
 */
int
dc_Handler (junk, cmds)
int junk;
struct ui_command *cmds;
{
	switch (UKEY (*cmds))
	{
	/*
	 * Platform definition.
	 */
	   case DK_PLATFORM:
		if (cmds[2].uc_ctype != UTT_END)
			dc_DefPlatform (UPTR (cmds[1]), UPTR (cmds[2]));
		else
			dc_DefPlatform (UPTR (cmds[1]), /*superclass*/NULL);
		break;

	   case DK_SUBPLATFORM:
	   	dc_SubPlatform (cmds + 1);
		break;
	/*
	 * Class definition.
	 */
	   case DK_CLASS:
		if (cmds[2].uc_ctype != UTT_END)
		   dc_DefPlatformClass (UPTR (cmds[1]), UPTR (cmds[2]), FALSE);
		else
		   dc_DefPlatformClass (UPTR (cmds[1]), /*super*/NULL, FALSE);
		break;
	/*
	 * Instance definitions
	 */
	   case DK_INSTANCE:
		dc_DefInstances (UPTR(cmds[1]), cmds + 2);
		break;
	/*
	 * Subplats additions to either classes or instances
	 */
	   case DK_SUBPLATS:
		dc_DefSubPlats ( UPTR(cmds[1]), UPTR(cmds[2]), cmds+3);
		break;
	   default:
		msg_ELog (EF_PROBLEM, "unknown ui token passed to dc_Handler");
		break;
	}
	return (TRUE);
}



static void
dc_DefPlatform (name, superclass)
char *name;
char *superclass;	/* NULL if not specified */
/*
 * Define a platform in the data store configuration.
 */
{
    PlatformClass *pc;
/*
 * Defining a platform implicitly defines a class first, then instantiates
 * that class with the same name.  So first define a like-named class.
 */
    pc = dc_DefPlatformClass (name, superclass, TRUE);

    if (pc)
    {
    /* 
     * The directory of the platform will be the same as the
     * class directory, regardless of whether we inherited
     * anything from a superclass.
     */
	pc->dpc_instance = InstanceCopyClass;
    /*
     * Finally, instantiate this platform.
     */
	ds_DefinePlatform (pc->dpc_id, name);
    }
}



static PlatClassRef
dc_DefPlatformClass (char *name, char *superclass, zbool platform)
/*
 * Define a platform in the data store configuration.  The 'platform' zbool
 * should be true iff this is implicit class creation from a platform 
 * command.
 */
{
	PlatClassRef ref = 0;
	static char *next_state;	/* static to avoid gcc warning
					   "might be clobbered by 'longjmp'" */
/*
 * Grab a platform table entry for this guy.
 */
	if (ParseOnly)
		printf ("----->Defining%s class '%s', superclass '%s'\n",
			((platform) ? " implicit" : ""), name, 
			((superclass != NULL) ? superclass : "none"));
	ref = ds_NewNamedClass (name, superclass);
	next_state = (platform) ? "in-platform" : "in-class";
/*
 * Now go pick up all the pieces.
 */
	ERRORCATCH
		ui_subcommand (next_state, "Class>", dc_InPlatformClass, 
			(long) ref);
	ENDCATCH
/*
 * The rest is pointless unless we got a class structure.
 */
	if (ref)
	{
		ds_DefineClass (ref);

		if (ParseOnly)
		{
			dbg_DumpClass (ref);
			printf ("----->Finished defining class %s\n", name);
		}
	}

	return (ref);
}



static void
dc_SubPlatform (cmds)
struct ui_command *cmds;
/*
 * Define a subplatform.  Define a 'subplatform' class for the parent,
 * (unless already defined), and then use that class in a call dc_DefSubPlats.
 */
{
    const PlatformClass *spc, *parent_class;
    const Platform *parent;
    PlatformId ppid;
    char spcname[512];
/*
 * Find our parent platform first.
 */
    if (! (parent = dt_FindPlatformName (UPTR (*cmds))))
    {
	msg_ELog (EF_PROBLEM, "Unknown parent platform '%s'",
		  UPTR (*cmds));
	return;
    }
    ppid = pi_Id (parent);
/*
 * Now try to find the subplatform class, else define it now.
 */
    sprintf (spcname, "%s.subplatform", pi_Name (parent));
    spc = dt_FindClassName (spcname);
    if (! spc)
    {
	PlatformClass *newpc;
    /*
     * Define the subplatform class, which is just a tweaked version
     * of the parent's class (so start with a subclass of the parent's
     * class).
     */
	if (ParseOnly)
	    printf ("----->Defining subplatform class %s\n",
		    spcname);
	parent_class = dt_FindClass (pi_ClassId (parent));
	newpc = ds_NewSubClass (spcname, pi_ClassId (parent));
	if (! newpc)	/* oops, some other time perhaps... */
	    return;
    /* --- The subplatform tweaks ------------- */
	newpc->dpc_inherit = InheritNone;
	newpc->dpc_instance = InstanceCopyParent;
	newpc->dpc_org = OrgScalar;
	newpc->dpc_flags &= ~DPF_COMPOSITE;
	newpc->dpc_flags |= DPF_SUBPLATFORM;

	ds_DefineClass (newpc);
	if (ParseOnly)
	{
	    dbg_DumpClass (newpc);
	    printf ("----->Finished defining class %s\n", spcname);
	}

	spc = newpc;
    }
/*
 * Now pass the work on to the subplats command.
 */
    dc_DefSubPlats (pi_Name (parent), spc->dpc_name, cmds+1);
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
		pc->dpc_maxsamp = UINT (cmds[1]);
		break;
	/*
	 * File splits.
	 */
	   case DK_SPLITSECONDS:
		pc->dpc_splitseconds = UINT (cmds[1]);
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
		msg_ELog (EF_INFO, "ignoring remote dir %s for class %s",
			  UPTR(cmds[1]), pc->dpc_name);
		break;
	/*
	 * Where instances of this class are put
	 */
	   case DK_INSTANCEDIR:
		switch (UKEY(cmds[1]))
		{
		   case DK_COPYCLASS:
			pc->dpc_instance = InstanceCopyClass; 
			break;
		   case DK_SUBDIRCLASS:
			pc->dpc_instance = InstanceSubdirClass; 
			break;
		   case DK_COPYPARENT:
			pc->dpc_instance = InstanceCopyParent; 
			break;
		   case DK_SUBDIRPARENT: 
			pc->dpc_instance = InstanceSubdirParent; 
			break;
		   case DK_DEFAULT:
		   case DK_ROOT:
			pc->dpc_instance = InstanceDefault; 
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
			pc->dpc_inherit = InheritAppend; 
			break;
		   case DK_COPY:
			pc->dpc_inherit = InheritCopy; 
			break;
		   case DK_NONE:
			pc->dpc_inherit = InheritNone; 
			break;
		}
		break;
	/*
	 * Split files across days.
	 */
	   case DK_DAYSPLIT:
		pc->dpc_splitseconds = 24*3600;
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
	 * Field definition to be added.
	 */
	   case DK_FIELD:
	        dc_AddField (pc, UPTR(cmds[1]));
		break;
	/*
	 * Field derivation to be added.
	 */
	   case DK_DERIVATION:
	        dc_AddDerivation (pc, UPTR(cmds[1]));
		break;
	/*
	 * Comments for this class.
	 */
	   case DK_COMMENT:
		ds_SetComment (pc, UPTR(cmds[1]));
		break;
	}
	return (TRUE);
}



static void
dc_DefSubPlats (const char *target, const char *classname, 
		struct ui_command *cmds)
/*
 * Define some new subplats.  If the target is an instance, instantiate
 * the platforms in the list and add them to the parent instance.
 * Otherwise, if the target is a class, add the subplats to the class
 * as SubPlatform templates.
 */
{
	PlatformId ppid = BadPlatform;
	const Platform *plat = NULL;
	const PlatformClass *pc = NULL, *spc = NULL;
/*
 * Find the appropriate platform or class entry for our parent.
 */
	if ((plat = dt_FindPlatformName (target)) != NULL)
		ppid = pi_Id (plat);
	else
		pc = dt_FindClassName (target);

	if (!plat && !pc)
	{
		msg_ELog (EF_PROBLEM, "subplats target '%s' not a known %s",
			  target, "class or instance");
		return ;
	}

	spc = dt_FindClassName (classname);
	if (!spc)
	{
		msg_ELog (EF_PROBLEM, "subplats class '%s' for %s unknown",
			  classname, target);
		return;
	}
/*
 * Do each subplatform in this command line.
 */
	if (ppid != BadPlatform)
	{
		for ( ; cmds->uc_ctype != UTT_END; ++cmds)
			ds_DefineSubPlatform (spc->dpc_id, UPTR (*cmds), ppid);
	}
	else
	{
		for ( ; cmds->uc_ctype != UTT_END; ++cmds)
			ds_AddClassSubplat ((PlatClassRef)pc, spc->dpc_id, 
					    UPTR (*cmds));
	}
}



static void
dc_AddField (PlatformClass *pc, char *fname)
{
    FieldId fid;
    char *copy = (char *)malloc (strlen(fname)+1);
    char *eq;
    strcpy (copy, fname);
    fname = copy;
    eq = strchr(fname, '=');
    if (eq)
    {
	/* Add this line as a derivation */
	dc_AddDerivation (pc, fname);
	*eq = 0;
    }
    /* Now we can treat the text like a simple field specification */
    fid = F_Lookup (fname);
    if (fid == BadField)
    {
	msg_ELog (EF_PROBLEM, "bad field '%s' for class '%s'",
		  fname, pc->dpc_name);
    }
    else
    {
	ds_AddClassField (pc, fid);
    }
    free (copy);
}



static void
dc_AddDerivation (PlatformClass *pc, char *dtext)
{
    ds_AddClassDerivation (pc, dtext);
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
	const PlatformClass *spc = dt_FindClassName (classname);

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
		ds_AddClassSubplat (pc, spc->dpc_id, UPTR (*cmds));
	}
}



static void
dc_DefInstances (const char *classname, struct ui_command *cmds)
/*
 * Traverse the list of names in the cmds list, and create an instance for
 * each one using the given class.
 *	classname	Name of class to create instances of
 *	cmds		Name of the instances
 */
{
	const PlatformClass *pc = dt_FindClassName (classname);
/*
 * Find the class they are asking for.  This pointer is valid through all
 * of the instantiations since dt_Instantiate does not create any classes.
 */
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
		ds_DefinePlatform (pc->dpc_id, UPTR(*cmds));
	}
}

