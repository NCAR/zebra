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
# include <Platforms.h>

RCSID ("$Id: dsdump.c,v 3.22 1999-11-01 20:14:18 burghart Exp $")

/*
 * Standalone scanning flag.
 */
static int Alone = 0;

/*
 * Options for displaying files
 */
#define NOFILES 0
#define SHOWFILES 1

/*
 * Options for the format of file entries.  Default is the historic format.
 */
#define ONLYFILES 2	/* filepath only */
#define LONGFILES 4	/* dsnotice format */

#define TC_DIGITS 10

/*
 * The global options structure which gets passed around.
 */
struct dsdump_options
{
	zbool sort;
	zbool subs;
	zbool tier;
	int files;
	zbool names;
	zbool obs;
	zbool defn;
	zbool toc;
	zbool quiet;	/* skip default output if true */
	zbool full;	/* full datafile paths if true */
	int tcf;	/* time formats */
	ZebraTime since;
	ZebraTime before;
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
	FALSE,
	FALSE,
	TC_Full
};
	

/*
 * Local prototypes
 */
static void DumpSubplatforms (const Platform *p);
static void DumpPlatform (const Platform *p, const DumpOptions *opts);
static void PrintInfo (const DataFile *df, const DumpOptions *opts);
static void PrintFilePath (const DataFile *df, const DumpOptions *opts);
static void PrintTime (char *s, const ZebraTime *zt, const DumpOptions *opts);


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
	printf("\t-l\tList filenames in 'dsnotice' column format\n");
	/* Save -r for 'realtime' option to assume dsnotice functionality */
	printf("\t-p '<number> [days|minutes|hours]'\n");
	printf("\t\tList data within a certain period of the current time,\n");
	printf("\t\twhere units can be abbreviated and defaults to days.\n");
	printf("\t-p '<time>'\n");
	printf("\t\tList files with data since the given time.\n");
	printf("\t\tA second -p limits the end time of the period.\n");
	printf("\t-T {full|date|time|micro|digits}\n");
	printf("\t\tSet the format for printing times.\n");
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
	printf("\tShow one days worth of data, two days ago:\n");
	printf("\t   %s -p '2 days' -p '1 day'\n", prog);
	printf("\tTar the last 6 hours of GOES data\n");
	printf("\t   tar cf goes.tar `%s -g -p '6 hours' goes`\n", prog);
	printf("\tList fields in most recent observation of each platform:\n");
	printf("\t   %s -z -t\n", prog);
#ifdef notyet
	printf("\tCreate a prelim ds config file from a set of files:\n");
	printf("\t   %s -i -q -d *.cdf > ds.config\n", prog);
#endif
}



void
GetFormat (char *arg, int *fmt)
{
	if (!strncmp("full", arg, strlen(arg)))
	{
		*fmt = TC_Full;
	}
	else if (!strncmp("date", arg, strlen(arg)))
	{
		*fmt = TC_DateOnly;
	}
	else if (!strncmp("time", arg, strlen(arg)))
	{
		*fmt = TC_TimeOnly;
	}
	else if (!strncmp("micro", arg, strlen(arg)))
	{
		*fmt = TC_FullUSec;
	}
	else if (!strncmp("digits", arg, strlen(arg)))
	{
		*fmt = TC_DIGITS;
	}
}



long
GetPeriod (char *arg, ZebTime *when)
/*
 * Parse a period string and return the number of seconds it represents.
 */
{
	long result = 0;
	int offset;
	char *units;
	float num;
	ZebTime since;

	if (arg == NULL)
	{
		fprintf (stderr, "missing period string\n");
		exit (9);
	}
	/*
	 * First try for a full-fledged date and time
	 */
	if (TC_DecodeTime (arg, &since))
	{
		if (when)
			*when = since;
		return (0);
	}
	/*
	 * Otherwise look for a simple interval relative to now
	 */
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
	if (when)
	{
		TC_SysToZt (time(0), when);
		when->zt_Sec -= result;
	}
	return (result);
}



static void
NextPlatform (PlatformId pid, DumpOptions *opts)
{
    const Platform *p = dt_FindPlatform (pid);

    if (! opts->quiet)
	DumpPlatform (p, opts);
    if (opts->defn)
    {
	fprintf (stdout, "\n");
	ds_ShowPlatformClass (stdout, ds_PlatformClass(pid));
    }
    if (opts->tier)
	DumpSubplatforms (p);
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
	zbool first, exact;
	char name[20];
	int matches;
	long period = 0;	/* Length of time to show data for */
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
	opts->since = ZT_ALPHA;		/* default: show all files */
	opts->before = ZT_OMEGA;
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
				opts->files |= ONLYFILES;
				opts->files &= ~LONGFILES;
				break;
			   case 'l':
				opts->files &= ~ONLYFILES;
				opts->files |= LONGFILES;
				break;
			   case 'g':
				opts->full = TRUE;
				break;
			   case 'p':
				if (! argv[++opt])
				{
					usage (argv[0]);
					exit (1);
				}
				if (TC_Eq (opts->since, ZT_ALPHA))
				{
					GetPeriod (argv[opt], &opts->since);
				}
				else
				{
					GetPeriod (argv[opt], &opts->before);
				}
				break;
			   case 'T':
				if (! argv[++opt])
				{
					usage (argv[0]);
					exit (1);
				}
				GetFormat (argv[opt], &opts->tcf);
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
				NextPlatform (pid, opts);
				++matches;
			}
			exact = FALSE;
			first = TRUE;
		}
		else
		{
			pattern = (opt < argc) ? (argv[opt]) : (NULL);
			platforms = ds_SearchPlatforms (pattern, &nplat, 
							opts->sort, 
							opts->subs);
			matches += nplat;
			for (i = 0; i < nplat; i++)
			{
				NextPlatform (platforms[i], opts);
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
DumpPlatform (const Platform *p, const DumpOptions *opts)
{
    int i, index;
    PlatformId pid = pi_Id (p);
    DataFile dfi;
    const char *name;
/*
 * Add a newline only when not listing only the names, and when listing files
 */
    if (!opts->names && (opts->files == SHOWFILES))
	printf ("\n");
	
    if (pi_Subplatform (p) && (name = strrchr(pi_Name (p), '/')))
	++name;
    else
	name = pi_Name (p);

    if (opts->files <= SHOWFILES)
    {
	printf ("Platform %s ", name);
	if (pi_Mobile (p))
	    printf (" (MOBILE)");
	printf ("\n");
    }

    if (!opts->names)
    {
    /*
     * Now dump out each source, quitting at the first file outside
     * of our period, unless period == 0.
     */
	SourceInfo si;
	int src = 0;
	    
	for (src = 0; ds_GetSourceInfo (src, &si); src++)
	{
	    const DataFile *df;
	    
	    if (opts->files <= SHOWFILES)
	    {
		printf (" Data source '%s'\n", si.src_Name);

		if (! opts->files)
		    continue;
	    }

	    for (df = ds_LastFile (src, pid); df; df = ds_PrevFile (df))
	    {
		if (TC_Less(df->df_core.dfc_end, opts->since))
		    break;
		if (TC_Less(opts->before, df->df_core.dfc_begin))
		    continue;

		PrintInfo (df, opts);

		if (opts->obs)
		    break;
	    }
	}
    }
}



static void
DumpSubplatforms (const Platform *p)
{
    const PlatformId *subplats = p->dp_subplats;
    int nsubplats = p->dp_nsubplats;
    const Platform *sp;
    char buf[256];
    unsigned int buflen;
    int n, i;
/*
 * Request a list of subplatforms from the daemon
 */
    if (!nsubplats)
	return;

    printf (" Subplatforms:\n");
    buf[0] = '\0';
    buflen = 0;
    for (i = 0; i < nsubplats; ++i)
    {
	const char *name;

	const Platform *sp = dt_FindPlatform (subplats[i]);

	if ((name = strchr(pi_Name (sp), '/')))
	    ++name;
	else
	    name = pi_Name (sp);

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
}



static void
PrintInfo (const DataFile *df, const DumpOptions *opts)
/*
 * Dump out file info.
 */
{
    FieldId fields [DC_MaxField];
    int nfield = DC_MaxField;
    char abegin[40], aend[20];
    const Platform *p = dt_FindPlatform (df->df_pid);

    if (opts->files & LONGFILES)
    {
    /*
     * Copy out just the path portion of the full file name
     */
	char dir[CFG_FILEPATH_LEN];
	int dirlen = strlen (df->df_fullname) - 
	    strlen (df->df_core.dfc_name) - 1;
	strncpy (dir, df->df_fullname, dirlen);
	dir[dirlen] = '\0';
	
	printf ("%s ", pi_Name (p));
	PrintFilePath (df, opts);
	PrintTime (aend, &df->df_core.dfc_end, opts);
	printf (" %s %s %hu\n", dir, aend, df->df_core.dfc_nsample);
    }
    else if (opts->files & ONLYFILES)
    {
	PrintFilePath (df, opts);
	printf ("\n");
    }
    else if (opts->files & SHOWFILES)	/* The trusty default */
    {
    /*
     * Pull out the date information and encode it.
     */
	PrintTime (abegin, &df->df_core.dfc_begin, opts);
	PrintTime (aend, &df->df_core.dfc_end, opts);
	printf (" %s  %s . %s [%hu]\n",	df->df_core.dfc_name, abegin, 
		aend, df->df_core.dfc_nsample);
    }
/*
 * Perform GetFields on this file if enabled.
 */
    if (opts->toc && 
	ds_GetFields (df->df_pid, &df->df_core.dfc_begin, &nfield, fields))
    {
	int i;
	for (i = 0; i < nfield; ++i)
	    printf ("   %s (%s): %s\n", F_GetName (fields[i]),
		    F_GetUnits (fields[i]), F_GetDesc (fields[i]));
    }
}		



static void
PrintFilePath (const DataFile *df, const DumpOptions *opts)
/*
 * Print just the pathname of this file on one line.
 */
{
    if (opts->full)
	printf ("%s", df->df_fullname);
    else
	printf ("%s", df->df_core.dfc_name);
}



static void
PrintTime (char *s, const ZebraTime *zt, const DumpOptions *opts)
{
	if (opts->tcf == TC_DIGITS)
	{
		UItime uid;

		TC_ZtToUI (zt, &uid);
		sprintf (s, "%08d%04d", uid.ds_yymmdd, uid.ds_hhmmss/100);
	}
	else
	{
		TC_EncodeTime (zt, (TimePrintFormat)opts->tcf, s);
	}
}
