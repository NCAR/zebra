/*
 * Force a rescan of a platform.
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
# include <unistd.h>

# include <defs.h>
# include <message.h>
# include <copyright.h>
# include <DataStore.h>

RCSID ("$Id: dsrescan.c,v 1.12 1998-12-17 00:39:09 burghart Exp $")


void
usage (prog)
char *prog;
{
	printf ("Usage: %s -h\n", prog);
	printf ("   Print this usage message\n");
	printf ("Usage: %s [-remote] -all\n", prog);
	printf ("Usage: %s [-remote] regexp [regexp ...]\n", prog);
	printf ("   Rescan all platforms or only those matching the given");
	printf (" regular expressions.\n");
	printf ("   -all   \tRescan all platforms\n");
	printf ("   -remote\tReset remote-dirs-constant (RDirConst) %s\n",
		"flag before scan");
	printf ("Usage: %s [-remote] -file <filename> platform\n", prog);
	printf ("   Scan a new file in the named platform.\n");
	printf ("   -remote\tThe file is in the platform's remote dir\n");
	printf ("   -file  \tSpecify the name of the file\n");
	printf ("   Options can be abbreviated to any number of letters.\n");
#ifdef notdef
	printf ("Usage: %s -scan filename [filename ...]\n", prog);
	printf ("   Scan files and show sample times, without connecting\n");
	printf ("   to the datastore daemon.\n");
#endif
}


static int
handler ()
{
	return (0);
}


int
main (argc, argv)
int argc;
char **argv;
{
	PlatformId plat;
	PlatformId *plist;
	int all = FALSE;
	int fileonly = FALSE;
	int remote = FALSE;
	int scan = FALSE;	/* scan files only */
	char pname[50];
	char *filename;
	char **platname = NULL;
	int i, j, p, nplat, total;
/*
 * Check args.
 */
	if ((argc < 2) || (!strcmp (argv[1], "-h")))
	{
		usage (argv[0]);
		exit (0);
	}
/*
 * Figure out the params, then do the dirty work.
 */
	p = 0;
	if (argc > 1)
		platname = (char **)malloc((argc - 1)*sizeof(char *));
	for (i = 1; i < argc; ++i)
	{
		if ((argv[i][0] != '-') || (strlen(argv[i]) < (unsigned)2))
			platname[p++] = argv[i];
		else if (!strncmp(argv[i], "-all", strlen(argv[i])))
			all = TRUE;
		else if (!strncmp(argv[i],"-remote",strlen(argv[i])))
			remote = TRUE;
#ifdef notdef
		else if (!strncmp(argv[i],"-scan",strlen(argv[i])))
			scan = TRUE;
#endif
		else if (!strncmp(argv[i],"-file",strlen(argv[i])))
		{
			fileonly = TRUE;
			if (i+1 < argc)
				filename = argv[++i];
			else
			{
				printf("%s: filename required\n",
				       argv[0]);
				usage(argv[0]);
				exit(2);
			}
		}
		else
		{
			printf ("%s: invalid option '%s'\n",
				argv[0], argv[i]);
			usage (argv[0]);
			exit (99);
		}
		if (all && fileonly)
		{
			printf("%s: cannot combine -all and -file options\n", 
			       argv[0]);
			exit (99);
		}
	}
/*
 * Get initialized.
 */
	sprintf (pname, "Rescan-%d", getpid ());
	if (scan)
	{
#ifdef notdef
		msg_connect (0, pname);
		ds_Standalone ();
#endif
	}
	else if (! msg_connect (handler, pname) || !ds_Initialize ())
	{
		msg_ELog (EF_EMERGENCY, "could not connect to datastore");
		exit (1);
	}

	if (remote && !fileonly)
	{
		printf ("Resetting remote-dir-constant (RDirConst) flag.\n");
		cp_Exec (DS_DAEMON_NAME, "set RDirConst false");
	}

	if (all)
	{
		if (p > 0)
		{
			printf ("%s: -all takes no platform name arguments\n",
				argv[0]);
			exit (99);
		}
		ds_ForceRescan (BadPlatform, TRUE);
		printf ("All platforms being scanned.\n");
	}
	else if (scan)
	{
#ifdef notdef
		if (p == 0)
		{
			printf ("need at least one file name for -scan\n");
			usage (argv[0]);
			exit(3);
		}
		ds_ScanFile (plat, filename, remote ? FALSE : TRUE);
		printf ("File '%s' in platform '%s' scanned.\n", 
			filename, platname[0]);
#endif
	}
	else if (fileonly)
	{
		if (p != 1)
		{
			printf ("%s for adding file\n", (p < 1) ?
				"need 1 platform" : "too many platforms");
			usage (argv[0]);
			exit(3);
		}
		plat = ds_LookupPlatform (platname[0]);
		if (plat == BadPlatform)
		{
			printf ("%s: bad platform\n", platname[0]);
			usage(argv[0]);
			exit (4);
		}
		ds_ScanFile (plat, filename, remote ? FALSE : TRUE);
		printf ("File '%s' in platform '%s' scanned.\n", 
			filename, platname[0]);
	}
	else
	{
		total = 0;
		for (i = 0; i < p; ++i)
		{
			plist = ds_SearchPlatforms (platname[i], &nplat, 
						    FALSE, FALSE);
			if (plist == NULL)
				printf ("No matches for '%s'\n",
					platname[i]);
			else
			{
				for (j = 0; j < nplat; ++j)
				{
					ds_ForceRescan (plist[j], FALSE);
				}
				free (plist);
				total += nplat;
			}
		}
		printf ("%d platforms being scanned.\n", total);
	}
	if (platname)
		free (platname);

	exit (0);
}
