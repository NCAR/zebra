/*
 * Data Store dumpout.
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
# include "defs.h"
# include "message.h"
# include <copyright.h>
# include "DataStore.h"

MAKE_RCSID ("$Id: dsdump.c,v 3.9 1994-01-03 07:21:11 granger Exp $")


/*
 * Local prototypes
 */
static void DumpSubplatforms FP((PlatformId pid, PlatformInfo *pi));
static void DumpPlatform FP((PlatformId pid, PlatformInfo *pi, 
			     bool names, bool files));
static void PrintInfo FP((int index, DataFileInfo *dfi));


msg_handler ()
{ }


static void
usage (prog)
char *prog;
{
	printf("Usage: %s -h\n", prog);
	printf("       %s [options] [-e name] [regexp ...]\n", prog);
	printf("Lists all platforms if no regular expressions are given.\n");
	printf("\t-h\tPrint this usage information\n");
	printf("\t-a\tAlphabetize list for each regular expression\n");
	printf("\t-e\tMatch the following name exactly\n");
	printf("\t-s\tInclude subplatforms in the list\n");
	printf("\t-c\tShow subplatforms (children) for each platform\n");
	printf("\t-x\tExclude data files from listing\n");
	printf("\t-n\tList platform names only\n");
	printf("Examples:\n");
	printf("\tAlphabetized list of all platforms: %s -a\n", prog);
	printf("\tList 'ship' platforms, including subplatforms for each:\n");
	printf("\t%s -c '.*ship.*'\n", prog);
	printf("\tList radars, and the platform exactly named 'base':\n");
	printf("\t%s radars -e base\n", prog);
}



main (argc, argv)
int argc;
char **argv;
{
	int i, nplat, opt;
	PlatformInfo pi;
	PlatformId *platforms;
	PlatformId pid;
	char *pattern;
	bool sort, subs, tier, exact, first, files, names;
	char name[20];
	int matches;
/*
 * First check for the help option
 */
	if ((argc > 1) && (!strcmp(argv[1], "-h")))
	{
		usage (argv[0]);
		exit (0);
	}

	sprintf (name, "DSDump-%d", getpid());
	msg_connect (msg_handler, name);
	usy_init ();
	if (! ds_Initialize ())
	{
		printf("%s: could not connect to DataStore daemon\n",argv[0]);
		exit (1);
	}
/*
 * How many platforms?
 */
	nplat = ds_GetNPlat ();
	printf ("%d platforms, total.\n", nplat);
	matches = 0;
	platforms = NULL;
/*
 * Default options: don't sort and don't include subplatforms
 */
	sort = FALSE;
	subs = FALSE;
	tier = FALSE;
	exact = FALSE;
	files = TRUE;
	names = FALSE;
/*
 * Traverse the options, turning on options as encountered
 */
	opt = 1;
	first = FALSE;	/* true once we try at least one pattern */
	do {
		if ((opt < argc) && (argv[opt][0] == '-'))
		{
			if (exact)
			{
				printf ("%s: -e needs platform name\n",
					argv[0]);
				exit (2);
			}
			switch (argv[opt][1])
			{
			   case 's':
				subs = TRUE;
				break;
			   case 'a':
				sort = TRUE;
				break;
			   case 'c':
				tier = TRUE;
				break;
			   case 'e':
				exact = TRUE;
				break;
			   case 'x':
				files = FALSE;
				break;
			   case 'n':
				names = TRUE;
				break;
			   default:
				printf ("%s: illegal option '%s'\n",
					argv[0], argv[opt]);
				usage (argv[0]);
				exit (1);
				break;
			}
		}
		else if (exact)	/* match this name exactly */
		{
			pid = ds_LookupPlatform (argv[opt]);
			if (pid == BadPlatform)
				printf ("%s: bad platform\n", argv[opt]);
			else
			{
				ds_GetPlatInfo (pid, &pi);
				DumpPlatform (pid, &pi, names, files);
				if (tier)
					DumpSubplatforms (pid, &pi);
				++matches;
			}
			exact = FALSE;
			first = TRUE;
		}
		else
		{
			pattern = (opt < argc) ? (argv[opt]) : (NULL);
			platforms = ds_GatherPlatforms (pattern, &nplat, 
							sort, subs);
			matches += nplat;
			for (i = 0; i < nplat; i++)
			{
				ds_GetPlatInfo (platforms[i], &pi);
				DumpPlatform (platforms[i], &pi, names, files);
				if (tier)
					DumpSubplatforms (platforms[i], &pi);
			}
			if (pattern && (nplat == 0))
				printf ("No matches for '%s'\n", pattern);
			if (platforms)
				free (platforms);
			first = TRUE;
		}
		++opt;
	}
	while ((opt < argc) || (! first));
/*
 * Done.
 */
	printf ((matches == 1) ? "\n1 match found.\n" :	((matches) ?
		 "\n%d matches found.\n" : "\nNo matches found.\n"), matches);
	exit(0);
}



static void
DumpPlatform (pid, pi, names, files)
PlatformId pid;
PlatformInfo *pi;
bool names;		/* list names only when true */
bool files;		/* list files if true */
{
	int i, index;
	DataSrcInfo dsi;
	DataFileInfo dfi;
	char *name;

	ds_LockPlatform (pid);
/*
 * Add a newline only when not listing only the names, and when listing files
 */
	if (!names && files)
		ui_printf ("\n");
	if (name = strchr(pi->pl_Name, '/'))
		++name;
	else
		name = pi->pl_Name;
	ui_printf ("Platform %s, %d data sources", name, pi->pl_NDataSrc);
	if (pi->pl_Mobile)
		ui_printf (" (MOBILE)");
	ui_printf ("\n");

	if (!names)
	{
	/*
	 * Now dump out each source.
	 */
		for (i = 0; i < pi->pl_NDataSrc; i++)
		{
			ds_GetDataSource (pid, i, &dsi);
			ui_printf (" Data source '%s', in %s, type %d\n", 
				   dsi.dsrc_Name, dsi.dsrc_Where, 
				   dsi.dsrc_Type);
			if (! files)
				continue;
			for (index = dsi.dsrc_FFile; index > 0; 
			     index = dfi.dfi_Next)
			{
				ds_GetFileInfo (index, &dfi);
				PrintInfo (index, &dfi);
			}
		}
	}
	ds_UnlockPlatform (pid);
}



static void
DumpSubplatforms (pid, pi)
PlatformId pid;
PlatformInfo *pi;
{
	PlatformId *subplats;
	PlatformInfo spi;
	char buf[256];
	int buflen;
	int n, i;
/*
 * Request a list of subplatforms from the daemon
 */
	subplats = ds_LookupSubplatforms (pid, &n);
	if (!subplats)
		return;
	ui_printf (" Subplatforms:\n");
	buf[0] = '\0';
	buflen = 0;
	for (i = 0; i < n; ++i)
	{
		char *name;

		ds_GetPlatInfo (subplats[i], &spi);
		if (name = strchr(spi.pl_Name, '/'))
			++name;
		else
			name = spi.pl_Name;
		if (buflen && (buflen + strlen(name) + 3 >= 78))
		{
			ui_printf (" %s\n", buf);
			buf[0] = '\0';
			buflen = 0;
		}
		sprintf (buf+buflen, " '%s'", name);
		buflen += strlen(name) + 3;
	}
	if (buflen)
		ui_printf (" %s\n", buf);
	free (subplats);
}



static void
PrintInfo (index, dfi)
int index;
DataFileInfo *dfi;
/*
 * Dump out file info.
 */
{
	char abegin[40], aend[20];

/*
 * Pull out the date information and encode it.
 */
	TC_EncodeTime (&dfi->dfi_Begin, TC_Full, abegin);
	TC_EncodeTime (&dfi->dfi_End, TC_TimeOnly, aend);
/*
 * Now print.
 */
	ui_printf ("  %c %4d  %s  %s > %s [%hu]\n",
		dfi->dfi_Archived ? 'A' : 'N',
		index, dfi->dfi_Name, abegin, aend, dfi->dfi_NSample);
}		

