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
# include <unistd.h>

# include <defs.h>
# include <copyright.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>

RCSID ("$Id: dsdump.c,v 3.18 1997-04-28 04:55:01 granger Exp $")

/*
 * Standalone scanning flag.
 */
static int Alone = 0;

/*
 * Options for displaying files
 */
#define NOFILES 0
#define SHOWFILES 1
#define ONLYFILES 2
#define FULLFILES 3

/*
 * The global options structure which gets passed around.
 */
struct dsdump_options
{
	bool sort;
	bool subs;
	bool tier;
	int files;
	bool names;
	bool obs;
	bool defn;
	bool toc;
	bool quiet;	/* skip default output if true */
};

typedef struct dsdump_options DumpOptions;

/*
 * Default options: don't sort and don't include subplatforms
 */
DumpOptions Options =
{
	FALSE,
	FALSE,
	FALSE,
	SHOWFILES,
	FALSE,
	FALSE,		/* only show most recent file */
	FALSE,		/* don't show class definitions */
	FALSE,		/* don't dump field list */
	FALSE
};
	

/*
 * Local prototypes
 */
static void DumpSubplatforms FP((PlatformId pid, PlatformInfo *pi));
static void DumpPlatform FP((PlatformId pid, PlatformInfo *pi, ZebTime *since,
			     DumpOptions *opts));
static void PrintInfo FP((int index, DataFileInfo *dfi, DumpOptions *opts));
static void PrintFilePath FP((DataSrcInfo *dsi, DataFileInfo *dfi, 
			      DumpOptions *opts));


static int
msg_handler ()
{ return (0); }


static void
usage (prog)
char *prog;
{
	printf("Usage: %s -h\n", prog);
	printf("       %s [options] [-e name] [regexp ...]\n", prog);
	printf("       %s -i filename [filename ...]\n", prog);
	printf("Lists all platforms if no regular expressions are given.\n");
	printf("\t-h\tPrint this usage information\n");
	printf("\t-a\tAlphabetize list for each regular expression\n");
	printf("\t-e\tMatch the following name exactly\n");
	printf("\t-s\tInclude subplatforms in the list\n");
	printf("\t-c\tShow subplatforms (children) for each platform\n");
	printf("\t-d\tShow class definitions for each platform\n");
	printf("\t-q\tQuiet: skip any default output\n");
	printf("\t-x\tExclude data files from listing\n");
	printf("\t-n\tList platform names only\n");
	printf("\t-z\tList only the most recent observation\n");
	printf("\t-t\tList fields in each observation ('%s')\n",
	       "table of contents");
	printf("\t-f\tList filenames only\n");
	printf("\t-g\tList filenames with their full pathnames\n");
	printf("\t-p '<number> [days|minutes|hours]'\n");
	printf("\t\tList data within a certain period of the current time,\n");
	printf("\t\twhere units can be abbreviated and defaults to days.\n");
#ifdef notyet
	printf("\t-i\tIndependently of the datastore, scan the given\n");
	printf("\t\tfiles and dump the results as usual.\n");
#endif
	printf("Examples:\n");
	printf("\tAlphabetized list of all platforms:\n\t   %s -a\n", prog);
	printf("\tList 'ship' platforms, including subplatforms for each:\n");
	printf("\t   %s -c '.*ship.*'\n", prog);
	printf("\tList radars, and the platform exactly named 'base':\n");
	printf("\t   %s radars -e base\n", prog);
	printf("\tShow the last two days worth of data for all platforms:\n");
	printf("\t   %s -p '2 days'\n", prog);
	printf("\tTar the last 6 hours of GOES data\n");
	printf("\t   tar cf goes.tar `%s -g -p '6 hours' goes`\n", prog);
	printf("\tList fields in most recent observation of each platform:\n");
	printf("\t   %s -z -t\n", prog);
#ifdef notyet
	printf("\tCreate a prelim ds config file from a set of files:\n");
	printf("\t   %s -i -q -d *.cdf > ds.config\n", prog);
#endif
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



static void
NextPlatform (PlatformId pid, ZebraTime *since, DumpOptions *opts)
{
	PlatformInfo pi;

	ds_GetPlatInfo (pid, &pi);
	if (! opts->quiet)
		DumpPlatform (pid, &pi, since, opts);
	if (opts->defn)
	{
		fprintf (stdout, "\n");
		ds_ShowPlatformClass (stdout, ds_PlatformClass(pid));
	}
	if (opts->tier)
		DumpSubplatforms (pid, &pi);
}



int
main (argc, argv)
int argc;
char **argv;
{
	int i, nplat, total, opt;
	PlatformId *platforms;
	PlatformId pid;
	char *pattern;
	bool first, exact;
	char name[20];
	int matches;
	long period = 0;	/* Length of time to show data for */
	ZebTime now;
	ZebTime since;		/* Time to dump since */
	DumpOptions *opts = &Options;
/*
 * First check for the help option
 */
	if ((argc > 1) && (!strcmp(argv[1], "-h")))
	{
		usage (argv[0]);
		exit (0);
	}
#ifdef notyet
	else if ((argc > 1) && (!strcmp(argv[1], "-i")))
	{
		Alone = 1;
	}
#endif

	sprintf (name, "DSDump-%d", getpid());
	if (Alone)
	{
		msg_connect (0, name);
		ds_Standalone ();
	}
	else if (! msg_connect (msg_handler, name) || (! ds_Initialize ()))
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
	nplat = total = ds_GetNPlat ();
	matches = 0;
	platforms = NULL;
/*
 * Traverse the options, turning on options as encountered
 */
	opt = 1;
	exact = FALSE;
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
				opts->subs = TRUE;
				break;
			   case 'a':
				opts->sort = TRUE;
				break;
			   case 'c':
				opts->tier = TRUE;
				break;
			   case 'd':
				opts->defn = TRUE;
				break;
			   case 'e':
				exact = TRUE;
				break;
			   case 'x':
				opts->files = NOFILES;
				break;
			   case 'n':
				opts->names = TRUE;
				break;
			   case 'z':
				opts->obs = TRUE;
				break;
			   case 't':
				opts->toc = TRUE;
				break;
			   case 'f':
				opts->files = ONLYFILES;
				break;
			   case 'g':
				opts->files = FULLFILES;
				break;
			   case 'p':
				period = GetPeriod (argv[++opt]);
				since = now;
				since.zt_Sec -= period;
				break;
			   case 'q':
				opts->quiet = TRUE;
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
				NextPlatform (pid, &since, opts);
				++matches;
			}
			exact = FALSE;
			first = TRUE;
		}
		else
		{
			pattern = (opt < argc) ? (argv[opt]) : (NULL);
			platforms = ds_GatherPlatforms (pattern, &nplat, 
							opts->sort, 
							opts->subs);
			matches += nplat;
			for (i = 0; i < nplat; i++)
			{
				NextPlatform (platforms[i], &since, opts);
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
	if (opts->files <= SHOWFILES && !opts->quiet)
	{
		printf ("%d platforms, total.\n", total);
		printf ((matches == 1) ? "\n1 match found.\n" :	
			((matches) ? "\n%d matches found.\n" :
			 "\nNo matches found.\n"), matches);
	}
	exit (0);
	return (0);
}



static void
DumpPlatform (pid, pi, since, opts)
PlatformId pid;
PlatformInfo *pi;
ZebTime *since;		/* Time since which to dump files */
DumpOptions *opts;
{
	int i, index;
	DataSrcInfo dsi;
	DataFileInfo dfi;
	char *name;
/*
 * Add a newline only when not listing only the names, and when listing files
 */
	if (!opts->names && (opts->files == SHOWFILES))
		printf ("\n");
	
	if ((pi->pl_SubPlatform) && (name = strrchr(pi->pl_Name, '/')))
		++name;
	else
		name = pi->pl_Name;
	if (opts->files <= SHOWFILES)
	{
		printf ("Platform %s, %d data sources", name, pi->pl_NDataSrc);
		if (pi->pl_Mobile)
			printf (" (MOBILE)");
		printf ("\n");
	}

	if (!opts->names)
	{
	/*
	 * Now dump out each source, quitting at the first file outside
	 * of our period, unless period == 0.
	 */
		ds_LockPlatform (pid);
		for (i = 0; i < pi->pl_NDataSrc; i++)
		{
			ds_GetDataSource (pid, i, &dsi);
			if (opts->files <= SHOWFILES)
			{
				printf (" Data source '%s', in %s, type %d\n", 
					dsi.dsrc_Name, dsi.dsrc_Where, 
					dsi.dsrc_Type);
				if (! opts->files)
					continue;
			}
			for (index = dsi.dsrc_FFile; index > 0; 
			     index = dfi.dfi_Next)
			{
				ds_GetFileInfo (index, &dfi);
				if (TC_Less(dfi.dfi_End, *since))
					break;
				if (opts->files >= ONLYFILES)
					PrintFilePath (&dsi, &dfi, opts);
				else
					PrintInfo (index, &dfi, opts);
				if (opts->obs)
					break;
			}
		}
		ds_UnlockPlatform (pid);
	}
}



static void
DumpSubplatforms (pid, pi)
PlatformId pid;
PlatformInfo *pi;
{
	PlatformId *subplats;
	PlatformInfo spi;
	char buf[256];
	unsigned int buflen;
	int n, i;
/*
 * Request a list of subplatforms from the daemon
 */
	subplats = ds_LookupSubplatforms (pid, &n);
	if (!subplats)
		return;
	printf (" Subplatforms:\n");
	buf[0] = '\0';
	buflen = 0;
	for (i = 0; i < n; ++i)
	{
		char *name;

		ds_GetPlatInfo (subplats[i], &spi);
		if ((name = strchr(spi.pl_Name, '/')))
			++name;
		else
			name = spi.pl_Name;
		if (buflen && (buflen + strlen(name) + 3 >= (unsigned)78))
		{
			printf (" %s\n", buf);
			buf[0] = '\0';
			buflen = 0;
		}
		sprintf (buf+buflen, " '%s'", name);
		buflen += strlen(name) + 3;
	}
	if (buflen)
		printf (" %s\n", buf);
	free (subplats);
}



static void
PrintInfo (index, dfi, opts)
int index;
DataFileInfo *dfi;
DumpOptions *opts;
/*
 * Dump out file info.
 */
{
	char abegin[40], aend[20];
	FieldId fields [DC_MaxField];
	int nfield = DC_MaxField;
/*
 * Pull out the date information and encode it.
 */
	TC_EncodeTime (&dfi->dfi_Begin, TC_Full, abegin);
	TC_EncodeTime (&dfi->dfi_End, TC_TimeOnly, aend);
/*
 * Now print.
 */
	printf ("  %c %4d  %s  %s > %s [%hu]\n",
		dfi->dfi_Archived ? 'A' : 'N',
		index, dfi->dfi_Name, abegin, aend, dfi->dfi_NSample);
/*
 * Perform GetFields on this file if enabled.
 */
	if (opts->toc && 
	    ds_GetFields (dfi->dfi_Plat, &dfi->dfi_Begin, &nfield, fields))
	{
		int i;
		for (i = 0; i < nfield; ++i)
			printf ("   %s (%s): %s\n", F_GetName (fields[i]),
				F_GetUnits (fields[i]),
				F_GetDesc (fields[i]));
	}
}		



static void
PrintFilePath (dsi, dfi, opts)
DataSrcInfo *dsi;
DataFileInfo *dfi;
DumpOptions *opts;
/*
 * Print just the pathname of this file on one line.
 */
{
	if (opts->files == ONLYFILES)
		printf ("%s\n", dfi->dfi_Name);
	else if (opts->files == FULLFILES)
		printf ("%s/%s\n", dsi->dsrc_Where, dfi->dfi_Name);
}

