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
static char *rcsid = "$Id: d_Config.c,v 2.2 1992-02-20 21:33:23 burghart Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dsDaemon.h"
# include "commands.h"
# include <ui_error.h>




# ifdef __STDC__
	static int dc_InPlatform (Platform *, struct ui_command *);
# else
	static int dc_InPlatform ();
# endif



void
dc_DefPlatform (name)
char *name;
/*
 * Define a platform in the data store configuration.
 */
{
	Platform *plat;
/*
 * Grab a platform table entry for this guy.
 */
	plat = dt_NewPlatform (name);
/*
 * Now go pick up all the pieces.
 */
	ERRORCATCH
		ui_subcommand ("in-platform", "Platform>", dc_InPlatform, 
			(long) plat);
	ENDCATCH
}






void
dc_SubPlatform (cmds)
struct ui_command *cmds;
/*
 * Define a subplatform.
 */
{
	Platform *parent, *sub;
/*
 * Find our parent platform first.
 */
	if (! (parent = dt_FindPlatform (UPTR (*cmds), 0)))
	{
		msg_ELog (EF_PROBLEM, "Unknown parent platform '%s'",
				UPTR (*cmds));
		return;
	}
/*
 * Now it's time to make some children.
 */
	for (cmds++; cmds->uc_ctype != UTT_END; cmds++)
	{
		char subname[80];
	/*
	 * We won't redefine an existing platform.
	 */
		sprintf (subname, "%s/%s", parent->dp_name, UPTR (*cmds));
	 	if (dt_FindPlatform (subname, TRUE))
			msg_ELog (EF_PROBLEM, "Subplatform %s already exists",
				subname);
	/*
	 * Get a new entry, clone the parent, and tweak.
	 */
		else
		{
		 	sub = dt_NewPlatform (subname);
			*sub = *parent;
			strcpy (sub->dp_name, subname);
			sub->dp_org = OrgScalar;
			sub->dp_flags |= DPF_SUBPLATFORM;
			sub->dp_parent = parent - PTable;
		}
	}
}




static int
dc_InPlatform (plat, cmds)
Platform *plat;
struct ui_command *cmds;
/*
 * Deal with an internal definition for this platform.
 */
{
	switch (UKEY (*cmds))
	{
	/*
	 * Maybe we're done.
	 */
	   case DK_ENDPLATFORM:
	   	return (FALSE);
	/*
	 * They want to tell us about the file organization.
	 */
	   case DK_ORGANIZATION:
	   	plat->dp_org = (DataOrganization) UINT (cmds[1]);
		break;
	/*
	 * ...or the file type.
	 */
	   case DK_FILETYPE:
	   	plat->dp_ftype = (FileType) UINT (cmds[1]);
		break;
	/*
	 * Keep time, in minutes.
	 */
	   case DK_KEEP:
	   	plat->dp_keep = InterpDTime (UPTR (cmds[1]))*60;
		break;
	/*
	 * Maximum samples.
	 */
	   case DK_MAXSAMPLES:
	   	plat->dp_maxsamp = UINT (cmds[1]);
		break;
	/*
	 * Various flags.
	 */
	   case DK_REGULAR:	plat->dp_flags |= DPF_REGULAR; break;
	   case DK_MOBILE:	plat->dp_flags |= DPF_MOBILE; break;
	   case DK_COMPOSITE:	plat->dp_flags |= DPF_COMPOSITE; break;
	   case DK_DISCRETE:	plat->dp_flags |= DPF_DISCRETE; break;
	/*
	 * Where the data lives.
	 */
	   case DK_DIRECTORY:
	   	strcpy (plat->dp_dir, UPTR (cmds[1]));
		break;
	/*
	 * Where remote data lives.
	 */
	   case DK_REMOTE:
		if (! DisableRemote)
		{
		   	strcpy (plat->dp_rdir, UPTR (cmds[1]));
			plat->dp_flags |= DPF_REMOTE;
		}
		break;
	}
	return (TRUE);
}
