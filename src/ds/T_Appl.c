/*
 * Test application interface routines and future developments
 */

#define APPL_STATS
#define DEBUG
#include "Appl.c"

/*
 * Now the testing modules
 */
#include <stdio.h>
#include "apple.h"



static int
T_DumpClasses (show)
int show;
/*
 * First define all the test platforms, in case we're running standalone,
 * then dump all the known classes.
 */
{
	int i;
	int cid;
	int errors = 0;
	FILE *out = NULL;

	if (show && Verbose)
		out = stdout;
	else if (show)
	{
		char buf[64];
		sprintf (buf, "classes.%lu.o", (unsigned long)getpid());
		out = fopen (buf, "w");
		if (! out)
		{
			++errors;
			msg_ELog (EF_PROBLEM, "%s %s",
				  "dumpclasses: could not open", buf);
			out = stdout;
		}
	}
	for (i = 0; i < NUM_PLATFORMS; ++i)
	{
		PlatformId pid = NeedPlatform (TestPlatforms[i].name);

		if (pid == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "NeedPlatform failed for %s",
				  TestPlatforms[i].name);
			++errors;
			continue;
		}

		if ((cid = ds_PlatformClass (pid)) == BadClass)
		{
			msg_ELog (EF_PROBLEM, "%s for %s (%d)",
				  "ds_PlatformClass failed", 
				  TestPlatforms[i].name, pid);
			++errors;
		}
		else if (show)
		{
			ds_ShowPlatformClass (out, cid);
			fputc ('\n', out);
		}
	}
	if (out && !Verbose)
		fclose (out);
	return (errors);
}


static int
T_CacheClasses ()
{
	return (T_DumpClasses (FALSE));
}



static int
T_ShowClasses ()
/*
 * Dump the current class definitions.
 */
{
	PlatClassId cid = 0;

	if (! Verbose)
		return (0);
	while (ds_ShowPlatformClass (stdout, cid++) == 0)
		fputc ('\n', stdout);
	return (0);
}




static void DumpSubplatforms (const Platform *p);
static void DumpPlatform (const Platform *p, ZebTime *since, int names, 
			  int files, int obs);
static void PrintInfo (const DataFile *df);
static void PrintFilePath (const DataFile *df, int files);

/*
 * Options for displaying files
 */
#define NOFILES 0
#define SHOWFILES 1
#define ONLYFILES 2
#define FULLFILES 3


static int
T_DSDump ()
/*
 * Imitate dsdump; useful when running standalone, since it only dumps
 * those platforms we have cached.
 */
{
	int i;
	int tier = 0;
	ZebTime since;

	since = ZT_EPOCH;
	if (! Verbose && ! Debug)
		return (0);
	for (i = 0; i < dt_NPlatform(); i++)
	{
		const Platform *p = dt_FindPlatform (i);
	    
		if (! p)
			continue;

		if (pi_Subplatform (p))
			continue;
		DumpPlatform (p, &since, FALSE, SHOWFILES, FALSE);
		if (tier)
			DumpSubplatforms (p);
	}
	return (0);
}



static void
DumpPlatform (const Platform *p, ZebraTime *since, int names, int files, 
	      int obs)
{
	int i;
	SourceInfo dsi;
	const DataFile *df;
	const char *name;
/*
 * Add a newline only when not listing only the names, and when listing files
 */
	if (!names && (files == SHOWFILES))
		printf ("\n");
	
	if (pi_Subplatform (p) && (name = strrchr (pi_Name (p), '/')))
		++name;
	else
		name = pi_Name (p);

	if (files <= SHOWFILES)
	{
		printf ("Platform %s", name);
		if (pi_Mobile (p))
			printf (" (MOBILE)");
		printf ("\n");
	}

	if (!names)
	{
	/*
	 * Now dump out each source, quitting at the first file outside
	 * of our period, unless period == 0.
	 */
		for (i = 0; ds_GetSourceInfo (i, &dsi); i++)
		{
			if (files <= SHOWFILES)
			{
				printf (" Data source '%s', basedir %s\n", 
					dsi.src_Name, dsi.src_Dir);
				if (! files)
					continue;
			}

			for (df = ds_FirstFile (i, pi_Id (p)); df; 
			     df = ds_NextFile (df))
			{
				if (TC_Less(df->df_core.dfc_end, *since))
					break;
				if (files >= ONLYFILES)
					PrintFilePath (df, files);
				else
					PrintInfo (df);
				if (obs)
					break;
			}
		}
	}
}



static void
DumpSubplatforms (const Platform *p)
{
	PlatformId *subplats;
	const Platform *sp;
	char buf[256];
	unsigned int buflen;
	int n, i;
/*
 * Request a list of subplatforms from the daemon
 */
	n = p->dp_nsubplats;
	if (! n)
		return;

	subplats = p->dp_subplats;
	printf (" Subplatforms:\n");
	buf[0] = '\0';
	buflen = 0;
	for (i = 0; i < n; ++i)
	{
		const char *name;

		sp = dt_FindPlatform (subplats[i]);
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
PrintInfo (const DataFile *df)
/*
 * Dump out file info.
 */
{
	char abegin[40], aend[20];

/*
 * Pull out the date information and encode it.
 */
	TC_EncodeTime (&df->df_core.dfc_begin, TC_Full, abegin);
	TC_EncodeTime (&df->df_core.dfc_end, TC_TimeOnly, aend);
/*
 * Now print.
 */
	printf ("  %s  %s > %s [%hu]\n",
		df->df_core.dfc_name, abegin, aend, df->df_core.dfc_nsample);
}		



static void
PrintFilePath (const DataFile *df, int files)
/*
 * Print just the pathname of this file on one line.
 */
{
	if (files == ONLYFILES)
		printf ("%s\n", df->df_core.dfc_name);
	else if (files == FULLFILES)
		printf ("%s\n", df->df_fullname);
}



TestRoutine ApplTests[] = 
{
	{ "zshowclass", FTUnknown, DCC_None, TR_BEGIN, T_ShowClasses,
	  "dump class definitions of all known classes" },
	{ "zapplcache", FTUnknown, DCC_None, TR_BEGIN, T_CacheClasses,
	  "verify name and struct caching for classes and platforms" },
	{ "zdsdump", FTUnknown, DCC_None, TR_BEGIN, T_DSDump,
	  "dump file chains for cached platforms" },
	END_TESTS
};


