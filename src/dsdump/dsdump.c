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

# include <stdio.h>
# include <string.h>
# include "defs.h"
# include "message.h"
# include <copyright.h>
# include "DataStore.h"

MAKE_RCSID ("$Id: dsdump.c,v 3.12 1994-10-11 16:25:08 corbet Exp $")


/*
 * Local prototypes
 */
static void DumpSubplatforms FP((PlatformId pid, PlatformInfo *pi));
static void DumpPlatform FP((PlatformId pid, PlatformInfo *pi, ZebTime *since,
			     int names, int files, int obs));
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
	printf("\t-z\tList only the most recent observation\n");
	printf("\t-p '<number> [days|minutes|hours]'\n");
	printf("\t\tList data within a certain period of the current time,\n");
	printf("\t\twhere units can be abbreviated and defaults to days.\n");
	printf("Examples:\n");
	printf("\tAlphabetized list of all platforms: %s -a\n", prog);
	printf("\tList 'ship' platforms, including subplatforms for each:\n");
	printf("\t%s -c '.*ship.*'\n", prog);
	printf("\tList radars, and the platform exactly named 'base':\n");
	printf("\t%s radars -e base\n", prog);
	printf("\tShow the last two days worth of data for all platforms:\n");
	printf("\t%s -p '2 days'\n", prog);
}


long
GetPeriod (arg)
char *arg;
/*
 * Parse a period string and return the number of seconds it represents.
 */
{
	long result = 0;
	int offset;
	char *units;
	float num;

	if (arg == NULL)
	{
		fprintf (stderr, "missing period string\n");
		exit (9);
	}
	if (sscanf (arg, "%f %n", &num, &offset) != 1)
	{
		fprintf (stderr, "illegal period string: '%s'\n", arg);
		exit (3);
	}
	units = arg + offset;
	if (!units[0] || !strncmp("days", units, strlen(units)))
	{
		result = num * 24 * 60 * 60;
	}
	else if (!strncmp("hours", units, strlen(units)))
	{
		result = num * 60 * 60;
	}
	else if (!strncmp("minutes", units, strlen(units)))
	{
		result = num * 60;
	}
	else
	{
		fprintf (stderr, "illegal period units: '%s'\n", units);
		exit (4);
	}
	return (result);
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
	bool sort, subs, tier, exact, first, files, names, obs;
	char name[20];
	int matches;
	long period = 0;	/* Length of time to show data for */
	ZebTime now;
	ZebTime since;		/* Time to dump since */
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
	tl_Time (&now);
	now.zt_MicroSec = 0;
	since.zt_Sec = 0;	/* default: show all files */
	since.zt_MicroSec = 0;
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
	obs = FALSE;		/* only show most recent file */
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
			   case 'z':
				obs = TRUE;
				break;
			   case 'p':
				period = GetPeriod (argv[++opt]);
				since = now;
				since.zt_Sec -= period;
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
				DumpPlatform (pid, &pi, &since, names, files,
					      obs);
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
				DumpPlatform (platforms[i], &pi, &since,
					      names, files, obs);
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
DumpPlatform (pid, pi, since, names, files, obs)
PlatformId pid;
PlatformInfo *pi;
ZebTime *since;		/* Time since which to dump files */
bool names;		/* list names only when true */
bool files;		/* list files if true */
bool obs;		/* list only most recent file */
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
	
	if ((pi->pl_SubPlatform) && (name = strrchr(pi->pl_Name, '/')))
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
	 * Now dump out each source, quitting at the first file outside
	 * of our period, unless period == 0.
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
				if (TC_Less(dfi.dfi_End, *since))
					break;
				PrintInfo (index, &dfi);
				if (obs)
					break;
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

