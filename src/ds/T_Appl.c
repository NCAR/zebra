/*
 * Test application interface routines and future developments
 */

#define APPL_STATS
#define DEBUG
#include "Appl.c"

int
ds_ShowPlatformClass (fp, cid)
FILE *fp;
PlatClassId cid;
/*
 * Dump the config definition for this class to the given file pointer.
 * Return 0 if we succeed, negative otherwise.
 */
{
	const PlatformClass *pc, *spc = NULL;

	pc = ds_GetClassStruct (cid, NULL);
	if (!pc)
		return (-1);
	if (pc->dpc_superclass != BadClass)
	{
		spc = ds_GetClassStruct (pc->dpc_superclass, NULL);
		if (!spc)
			return (-2);
	}
	dt_DecodeClass (fp, pc, spc);
	return (0);
}



/*
 * Now the testing modules
 */
#include <stdio.h>
#include "apple.h"


static int
T_VerifyCache (name, pid)
char *name;
PlatformId pid;
/* 
 * If standalone, verify we have a structure and name
 * cached for this platform.  Otherwise we should at least
 * have a name.
 */
{
	int type;
	SValue v;
	int errors = 0;

	if (! usy_g_symbol (Pf_Names, (char *)name, &type, &v))
	{
		++errors;
		msg_ELog (EF_PROBLEM, "platform %s (%d) was not cached",
			  name, pid);
	}
	else if (v.us_v_int != pid)
	{
		++errors;
		msg_ELog (EF_PROBLEM, "id cached for name %s (%d) != %d",
			  name, v.us_v_int, pid);
	}
	else if (Standalone)
	{
		if ((pid < 0) || (pid >= MAXPLAT) ||
		    (! PlatStructs[pid]))
		{
			msg_ELog (EF_PROBLEM, "standalone: %s for %s (%d)",
				  "no struct cache", name, pid);
			++errors;
		}
	} 
	return (errors);
}



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
		sprintf (buf, "classes.%d.o", getpid());
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
		errors += T_VerifyCache (TestPlatforms[i].name, pid);
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




void
T_ApplStats ()
{
	int i, ndf = 0;

	for (i = 0; i < DFCacheSize; ++i)
		ndf += (DFCache[i].df_index != -1) ? 1 : 0;
	msg_ELog (EF_STATS, "%s: size %d, filled %d, zap %d, loops %d",
		  "datafile cache stats", DFCacheSize, ndf, DFZap, DFC_Loops);
	msg_ELog (EF_STATS, "   hits %d; misses %d; in %d; out %d; up %d",
		  DFC_Hits, DFC_Misses, DFC_In, DFC_Out, DFC_Updated);
	msg_ELog (EF_STATS, "%s: size %d",
		  "platform and class cache stats", MAXPLAT);
	msg_ELog (EF_STATS, "   class names: hits %d; misses %d",
		  ClassNameHits, ClassNameMisses);
	msg_ELog (EF_STATS, "   plat names: hits %d; misses %d",
		  PlatNameHits, PlatNameMisses);
	msg_ELog (EF_STATS, "   class structs: hits %d; misses %d",
		  ClassHits, ClassMisses);
	msg_ELog (EF_STATS, "   plat structs: hits %d; misses %d; refresh %d",
		  PlatHits, PlatMisses, PlatRefresh);
}


static void DumpSubplatforms FP((PlatformId pid, PlatformInfo *pi));
static void DumpPlatform FP((PlatformId pid, PlatformInfo *pi, ZebTime *since,
			     int names, int files, int obs));
static void PrintInfo FP((int index, DataFileInfo *dfi));
static void PrintFilePath FP((DataSrcInfo *dsi, DataFileInfo *dfi, int files));

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
	PlatformInfo pi;
	int i;
	int tier = 0;
	ZebTime since = ZT_EPOCH;

	if (! Verbose && ! Debug)
		return (0);
	for (i = 0; i < MAXPLAT; i++)
	{
		if (! PlatStructs[i])
			continue;
		ds_GetPlatInfo (i, &pi);
		if (pi.pl_SubPlatform)
			continue;
		DumpPlatform (i, &pi, &since, FALSE, SHOWFILES, FALSE);
		if (tier)
			DumpSubplatforms (i, &pi);
	}
	return (0);
}



static void
DumpPlatform (pid, pi, since, names, files, obs)
PlatformId pid;
PlatformInfo *pi;
ZebTime *since;		/* Time since which to dump files */
int names;		/* list names only when true */
int files;		/* list files if true */
int obs;		/* list only most recent file */
{
	int i, index;
	DataSrcInfo dsi;
	DataFileInfo dfi;
	char *name;
/*
 * Add a newline only when not listing only the names, and when listing files
 */
	if (!names && (files == SHOWFILES))
		printf ("\n");
	
	if ((pi->pl_SubPlatform) && (name = strrchr(pi->pl_Name, '/')))
		++name;
	else
		name = pi->pl_Name;
	if (files <= SHOWFILES)
	{
		printf ("Platform %s, %d data sources", name, pi->pl_NDataSrc);
		if (pi->pl_Mobile)
			printf (" (MOBILE)");
		printf ("\n");
	}

	if (!names)
	{
	/*
	 * Now dump out each source, quitting at the first file outside
	 * of our period, unless period == 0.
	 */
		ds_LockPlatform (pid);
		for (i = 0; i < pi->pl_NDataSrc; i++)
		{
			ds_GetDataSource (pid, i, &dsi);
			if (files <= SHOWFILES)
			{
				printf (" Data source '%s', in %s, type %d\n", 
					dsi.dsrc_Name, dsi.dsrc_Where, 
					dsi.dsrc_Type);
				if (! files)
					continue;
			}
			for (index = dsi.dsrc_FFile; index > 0; 
			     index = dfi.dfi_Next)
			{
				ds_GetFileInfo (index, &dfi);
				if (TC_Less(dfi.dfi_End, *since))
					break;
				if (files >= ONLYFILES)
					PrintFilePath (&dsi, &dfi, files);
				else
					PrintInfo (index, &dfi);
				if (obs)
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
	printf ("  %c %4d  %s  %s > %s [%hu]\n",
		dfi->dfi_Archived ? 'A' : 'N',
		index, dfi->dfi_Name, abegin, aend, dfi->dfi_NSample);
}		



static void
PrintFilePath (dsi, dfi, files)
DataSrcInfo *dsi;
DataFileInfo *dfi;
int files;
/*
 * Print just the pathname of this file on one line.
 */
{
	if (files == ONLYFILES)
		printf ("%s\n", dfi->dfi_Name);
	else if (files == FULLFILES)
		printf ("%s/%s\n", dsi->dsrc_Where, dfi->dfi_Name);
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


