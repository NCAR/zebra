/*
 * AREA format ingest program.
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
# include <ingest.h>

RCSID("$Id: SatIngest.c,v 1.17 2003-01-29 22:22:21 burghart Exp $")

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
static char **ExtraFiles = NULL;
static char *Initfile = NULL;

/*
 * GMS IR field name to map to temperatures.
 */
static char GMS_Field[CFG_PLATNAME_LEN] = { '\0' };

static char Field[CFG_PLATNAME_LEN] = "vis";

/*
 * Prototypes
 */
static void	EnterUI (char *name, int argc, char **argv);
static int	Dispatcher FP ((int, struct ui_command *));
/* static int	MDispatcher FP ((struct message *)); */
static int	Die FP ((void));


/*
 * Usage info.
 */

static char *Options[] =
{
	"-- Command-line options (overridden by init file) --", "",
	"-help", "Usage message",
	"-platform <name>", "Platform name",
	"-field <name>", "Field name",
	"-init <file>", "Initialization file",
	"-check", "Disable time checks on file set",
	"+check", "Enable time checks on file set",
	"", "",
	"-- Configuration variables --", "",
	"(*) defaults derived from the AREA file if not given", "",
	"originlat", "Latitude of projection onto target grid (*)",
	"kmresolution", "X and Y resolution of target grid in kilometers (*)",
	"platform", "Name of destination raster platform",
	"field", "Default field name for ingested image files (vis)",
	"gmsinfrared", "Name of GMS IR field to map to temperaturs (eg ir)",
#ifdef notdef
	"navigation", "Accept only one navigation (goes, gvar, ps, rect)",
#endif
	"gridx", "Number of target grid points in X",
	"gridy", "Number of target grid points in Y",
	"truncate", "Truncate multi-byte data to most significant byte",
	"checktimes", "Limit ingested image set to most recent time",
	"", "",
	"-- Configuration commands --", "",
	"limits", "limits <minlat> <minlon> <maxlat> <maxlon> (*)",
	"go", "Read and ingest a set of files using current settings",
	NULL, NULL
};



static void
Usage (char *prog)
{
	char **opt;

	printf ("Usage: %s [options] [-init <initfile>] [areafile ...]\n", 
		prog);
	opt = Options;
	while (*opt && *(opt+1))
	{
		fprintf (stderr, "   %-18s   %s\n", *opt, *(opt+1));
		opt += 2;
	}
	printf ("\n");
	IngestUsage ();
}



int
main (int argc, char *argv[])
{
	int i;

	if (argc < 2)
	{
		Usage (argv[0]);
		exit (1);
	}
	i = 1;
	while (i < argc)
	{
		char *opt = argv[i];
		char *arg = argv[i+1];
		int len = strlen(opt);
		int nargs = 1;

		if (len < 2)
		{
			++i;
			continue;
		}
		if (strncmp ("-help", opt, len) == 0)
		{
			Usage (argv[0]);
			exit (0);
		}
		else if (strncmp ("-platform", opt, len) == 0 && arg)
		{
			strcpy (Platname, arg);
			nargs = 2;
		}
		else if (strncmp ("-field", opt, len) == 0 && arg)
		{
			strcpy (Field, arg);
			nargs = 2;
		}
		else if (strncmp ("-init", opt, len) == 0 && arg)
		{
			Initfile = arg;
			nargs = 2;
		}
		else if (strncmp ("-check", opt, len) == 0)
		{
			CheckTimes = FALSE;
		}
		else if (strncmp ("+check", opt, len) == 0)
		{
			CheckTimes = TRUE;
		}
		else
		{
			++i;
			continue;
		}
		IngestRemoveOptions (&argc, argv, i, nargs);
	}
	IngestParseOptions (&argc, argv, Usage);
	EnterUI ("SatIngest", argc, argv);
	exit (0);
}



void
EnterUI (char *name, int argc, char *argv[])
{
	SValue	v;
	stbl	vtable;
	char	loadfile[200], prompt[200], ourname[200];

	InitGrid (&Grid);
/*
 * Initialize connections
 */
	strcpy (ourname, name);
	sprintf (ourname+5, "_%04lx", getpid ());
	IngestInitialize (ourname);
	/* msg_connect (MDispatcher, ourname); */
	msg_DeathHandler ((int (*)()) Die);
/*
 * UI stuff
 */
	usy_init ();
	fixdir ("SI_LOAD_FILE", GetLibDir(), "SatIngest.lf", loadfile);

	if (Initfile)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = Initfile;
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			      SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);
	ui_setup (name, &argc, argv, 0);
	ExtraFiles = argv+1;
	argv[argc] = NULL;

	vtable = usy_g_stbl ("ui$variable_table");
	usy_c_indirect (vtable, "originLat", &Grid.origin_lat, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "kmResolution", &Grid.kmres, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "platform", Platname, SYMT_STRING, 
			CFG_PLATNAME_LEN);
	usy_c_indirect (vtable, "gmsinfrared", GMS_Field, SYMT_STRING, 
			CFG_PLATNAME_LEN);
	usy_c_indirect (vtable, "field", Field, SYMT_STRING, CFG_PLATNAME_LEN);
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
		/* Check for command-line files */
		while (ExtraFiles && *ExtraFiles)
			Infile = AddFile (Infile, *ExtraFiles++, Field);
		/* Check data times in the files and leave only the
		 * latest one(s) for ingest.  */
		if (CheckTimes)
			Infile = TimeCheck (Infile, NULL);
		/* Check for a GMS ir mapping */
		if (GMS_Field[0])
			GMS_MapIRField (GMS_Field);
		AreaIngest (Infile, &Grid, Platname);
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




#ifdef notdef
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
#endif /* notdef */
