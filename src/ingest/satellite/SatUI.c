/*
 * UI layer for satellite ingest programs.
 */
/*		Copyright (C) 1995 by UCAR
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
# include <errno.h>
# include <math.h>
# include <stdio.h>
# include <dirent.h>

# include <ui.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>
# include <DataChunk.h>

RCSID("$Id: SatUI.c,v 1.1 1997-03-11 19:39:08 granger Exp $")

# include "Area.h"
# include "keywords.h"


/*
 * Our platform
 */
static char Platname[CFG_PLATNAME_LEN] = "";

/*
 * Purge the list of area files of all but those with the most recent time.
 */
static bool CheckTimes = TRUE;

/*
 * Information on the grid to which we'll be ingesting images.
 */
static AreaGrid Grid;

/*
 * List of area files to ingest.
 */
static AreaFile *Infile = NULL;

/*
 * Keep a record of the type of areas we're expecting to ingest.
 */
static char Spec[20];

/*
 * Prototypes
 */
static int	Dispatcher FP ((int, struct ui_command *));
static int	MDispatcher FP ((struct message *));
static int	Die FP ((void));



void
EnterUI (name, spec, argc, argv)
char *name;
char *spec;	/* image type to expect */
int argc;
char **argv;
{
	SValue	v;
	stbl	vtable;
	char	loadfile[200], prompt[200], ourname[200];

	strcpy (Spec, spec);
/*
 * Connect to the message handler
 */
	strcpy (ourname, name);
	sprintf (ourname+5, "_%04lx", getpid ());
	msg_connect (MDispatcher, ourname);
	msg_DeathHandler ((int (*)()) Die);
/*
 * UI stuff
 */
	fixdir ("SI_LOAD_FILE", GetLibDir(), "SatIngest.lf", loadfile);

	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			      SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup (name, &argc, argv, 0);
/*
 * Initialization
 */
	F_Init ();
	ds_Initialize ();

	vtable = usy_g_stbl ("ui$variable_table");
	usy_c_indirect (vtable, "originLat", &Grid.origin_lat, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "kmResolution", &Grid.kmres, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "platform", Platname, SYMT_STRING, 
			CFG_PLATNAME_LEN);
	usy_c_indirect (vtable, "gridX", &Grid.gridX, SYMT_INT, 0);
	usy_c_indirect (vtable, "gridY", &Grid.gridY, SYMT_INT, 0);
	usy_c_indirect (vtable, "truncate", &Grid.truncate, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "checkTimes", &CheckTimes, SYMT_BOOL, 0);
/*
 * Get on with it
 */
	sprintf (prompt, "%s>", name);
	ui_get_command ("initial", prompt, Dispatcher, 0);
	ui_finish ();
	exit (0);
}




static int
Die ()
/*
 * Uh-oh.  Get out now.
 */
{
	ui_finish ();
	exit (1);
	return (0);
}




static int
Dispatcher (junk, cmds)
int	junk;
struct ui_command	*cmds;
/*
 * The command dispatcher.
 */
{
	switch (UKEY (*cmds))
	{
	/*
	 * Time to actually do things.  Ingest() is responsible for
	 * resetting the file list to zero in case we're told to 'go'
	 * more than once.
	 */
	    case KW_GO:
		/* Check data times in the files and leave only the
		 * latest one(s) for ingest.  */
		if (CheckTimes)
			Infile = TimeCheck (Infile, NULL);
		AreaIngest (Infile, &Grid, Platname, Spec);
		break;
	/*
	 * lat/lon limits
	 */
	    case KW_LIMITS:
		UserLimits (&Grid, UFLOAT(cmds[1]), UFLOAT(cmds[2]),
			    UFLOAT(cmds[3]), UFLOAT(cmds[4]));
		break;
	/*
	 * File
	 */
	    case KW_FILE:
		Infile = AddFile (Infile, UPTR(cmds[1]), UPTR(cmds[2]));
		break;
	/*
	 * Unknown command
	 */
	    default:
		msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
		break;
	}
	return (TRUE);
}





static int
MDispatcher (struct message *msg)
/*
 * Deal with a message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE)
		{
			Die ();
		}
		break;
	}
	return (0);
}   	
