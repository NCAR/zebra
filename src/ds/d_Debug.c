/*
 * Debugging utilities for the daemon
 */
# include <unistd.h>
# include <string.h>

# include <ui.h>
# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "Platforms.h"		/* for DefDataDir global declaration */
# include "d_Source.h"
# include "commands.h"
# include "dsDaemon.h"

MAKE_RCSID("$Id: d_Debug.c,v 3.14 2002-01-19 06:50:02 granger Exp $")

static struct flagmask {
	unsigned short mask;
	char *name;
} flagstr[] = {
	{ DPF_MOBILE, "mobile" },
	{ DPF_COMPOSITE, "composite" },
	{ DPF_DISCRETE, "discrete" },
	{ DPF_REGULAR, "regular" },
	{ DPF_SUBPLATFORM, "subplatform" },
	{ DPF_REMOTE, "remote" },
	{ DPF_MODEL, "model" },
	{ DPF_ABSTRACT, "abstract" },
	{ DPF_VIRTUAL, "virtual" },
};

static int nflagstr = sizeof(flagstr)/sizeof(flagstr[0]);


void
dbg_DumpClass (const PlatformClass *pc)
{
	int i;

	printf ("::::::::: Class %s, id %d, superclass %s\n", pc->dpc_name,
		pc->dpc_id,
		(pc_SuperClass(pc)) ? pc_SuperClass(pc)->dpc_name : "none");
	printf ("     Dir: %s\n", pc_SuggestedDir(pc));
	printf ("     Org: %s\n", ds_OrgName(pc->dpc_org));
	printf ("   FType: %s\n", ds_FTypeName(pc->dpc_ftype));
	printf ("    Keep: %hu\n", pc->dpc_keep);
	printf (" MaxSamp: %u\n", pc->dpc_maxsamp);
	printf ("   Split: %u\n", pc->dpc_splitseconds);
	printf (" Inherit: %s\n", 
		ds_InheritDirFlagName (pc_InheritDirFlag (pc)));
	printf ("Instance: %s\n", 
		ds_InstanceDirFlagName (pc_InstanceDirFlag (pc)));
	printf ("   Flags:");
	for (i = 0; i < nflagstr; ++i)
	{
		if (pc->dpc_flags & flagstr[i].mask)
			printf (" %s", flagstr[i].name);
	}
	printf ("\n");
	printf ("Subplats: ");
	for (i = 0; i < pc->dpc_nsubplats; ++i)
	{
	    printf (" {%s,%s}", pc_Name (pc), pc->dpc_subplats[i].dps_name);
	}
	printf ("\n");
	printf ("::::::::: End   %s :::::\n", pc_Name (pc));
}



void
dbg_DumpInstance (const Platform *pi)
{
	int i;

	printf ("_______ Instance %s, id %d, class %s\n", 
		pi->dp_name, pi->dp_id, pi_Class(pi)->dpc_name);
	printf ("     Dir: %s\n", pi_SuggestedDir(pi));
	printf ("  Parent: %s\n", 
		(pi_Parent(pi) != NULL) ? pi_Parent(pi)->dp_name : "none");
	printf ("   Flags:");
	for (i = 0; i < nflagstr; ++i)
	{
		if (pi->dp_flags & flagstr[i].mask)
			printf (" %s", flagstr[i].name);
	}
	printf ("\n");
	if (pi->dp_nsubplats)
	{
		printf ("Subplats: ");
		for (i = 0; i < pi->dp_nsubplats; ++i)
		{
			const Platform *sub;

			sub = dt_FindPlatform (pi->dp_subplats[i]);
			printf (" {%s,%s}", pi_Class(sub)->dpc_name, 
				sub->dp_name);
		}
		printf ("\n");
	}
	printf ("_______ End %s _____\n", pi->dp_name);
}



void
dbg_DumpStatus ()
{
	printf ("Status...................................................\n");
	printf ("    Platforms: %d, space for %d in table, growth at %d\n",
		dt_NPlatform(), PTableSize, PTableGrow);
	printf ("      Classes: %d, space for %d in table, growth at %d\n",
		dt_NClass(), CTableSize, CTableGrow);
	printf (" Memory Usage: platforms = %li bytes, classes = %li bytes\n",
		dt_NPlatform() * sizeof(DaemonPlatform), 
		dt_NClass() * sizeof(PlatformClass));
	printf (".........................................................\n");
}


void
dbg_DumpTables ()
/*
 * Dump all of the entries in the class and platform tables 
 */
{
	int i;
	char *dash = "===================================";
	const PlatformClass *pc;
	const Platform *pi;

	printf ("%s Class Table ====================\n", dash);
	for (i = 0; i < dt_NClass(); ++i)
	{
		if ((pc = dt_FindClass (i)))
			dbg_DumpClass (pc);
	}
	printf ("%s Platform Table =================\n", dash);
	for (i = 0; i < dt_NPlatform(); ++i)
	{
		if ((pi = dt_FindPlatform (i)))
			dbg_DumpInstance (pi);
	}
	printf ("%s=================================\n", dash);
}



int
dbg_SubplatCount ()
/*
 * Count the number of platforms which are a 'subplatform'
 */
{
	int count = 0;
	int i;
	const Platform *pi;

	for (i = 0; i < dt_NPlatform(); ++i)
		if ((pi = dt_FindPlatform (i)) && pi_Subplatform (pi))
			++count;
	return (count);
}




int
dbg_CompositeCount ()
/*
 * Count the number of platforms which are 'dirty'
 */
{
	int count = 0;
	int i;
	const Platform *pi;

	for (i = 0; i < dt_NPlatform(); ++i)
		if ((pi = dt_FindPlatform (i)) && pi_Composite (pi))
			++count;
	return (count);
}



int
dbg_Append (buf, str, len)
char *buf;
char *str;
int len;
/*
 * Prevent overrunning 'len' bytes of space in buf when appending
 * string 'str'.  Mkes sure buf is null-terminated; returns -1 when
 * whole string did not fit, otherwise returns new length of the buffer.
 */
{
	int buflen;
	int slen;

	buflen = strlen(buf);
	slen = strlen(str);
	if (len - buflen - 1 > 0)
	{
		strncpy (buf+buflen, str, len - buflen - 1);
	}
	buf[len - 1] = '\0';
	if (buflen + slen >= len)
		return (-1);
	else
		return (buflen + slen);
}


	

void
dbg_EncodeElapsed (prefix, start, end, dest)
char *prefix;
time_t *start;
time_t *end;
char *dest;
{
	long elapsed = *end - *start;

	if (prefix)
	{
		strcpy (dest, prefix);
		dest += strlen(dest);
	}
	sprintf (dest, "%s", ctime(start));
	dest += strlen(dest) - 1;		/* overwrite \n */
	if (elapsed > 48*3600)
	{
		sprintf (dest, ", %ld days, %ld hrs, and %ld mins ago",
			 elapsed / (3600*24), 
			 (elapsed % (3600*24)) / 3600, 
			 (elapsed % 3600) / 60);
	}		
	else if (elapsed > 3*3600)
	{
		sprintf (dest, ", %ld hours and %ld mins ago",
			 elapsed / 3600, (elapsed % 3600) / 60);
	}
	else
	{
		sprintf (dest, ", %ld minutes and %ld secs ago",
			 elapsed / 60, elapsed % 60);
	}
}



int
dbg_AnswerQuery (who)
char *who;
/*
 * Respond to a query message.
 */
{
	char buf[1024];
	time_t now = time (NULL);
	extern Source **Srcs;	/* from Daemon.c */
	extern int NSrcs;	/* from Daemon.c */
	int s;

	sprintf (buf, "Zebra data store daemon, proto %08x",
		 DSProtocolVersion);
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%s", V_version());
	msg_AnswerQuery (who, buf);

	dbg_EncodeElapsed ("Up since ", &Genesis, &now, buf);
	msg_AnswerQuery (who, buf);

	if (InitialScan)
		sprintf (buf, "Initial scan: %d %s so far, %d to go...",
			 PlatformsScanned, "platforms scanned", 
			 dt_NPlatform() - PlatformsScanned);
	else if (LastScan)
		dbg_EncodeElapsed ("Last full rescan ", 
				   &LastScan, &now, buf);
	else
		sprintf (buf, "No full rescans have occurred.");
	msg_AnswerQuery (who, buf);

#ifdef notdef
	if (LastCache)
		dbg_EncodeElapsed ("Cache files written ",
				   &LastCache, &now, buf);
	else
		sprintf (buf, "No cache file writes have occurred.");
	msg_AnswerQuery (who, buf);
#endif

	sprintf (buf, "Sources: \n");
	for (s = 0; s < NSrcs; s++)
	{
	    sprintf (buf+strlen(buf), "%18s: %s%s%s%s%s\n", 
		     src_Name (Srcs + s),
		     src_IsDirConst (Srcs + s) ? "DirConst, " : "", 
		     src_IsFileConst (Srcs + s) ? "FileConst, " : "",
		     src_RemembersAll (Srcs + s) ? "RememberAll, " : "",
		     src_DirsAreForced (Srcs + s) ? "ForceDirs, " : "",
		     src_RootDir (Srcs + s));
	}
	msg_AnswerQuery (who, buf);

	sprintf (buf, "%18s: %d used of %d allocated, %s %d",
		 "Platform classes", dt_NClass(), CTableSize, "grow by", 
		 CTableGrow);
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%18s: %d used of %d allocated, %s %d\n", 
		 "Platforms", dt_NPlatform(), PTableSize, "grow by", 
		 PTableGrow);
	sprintf (buf+strlen(buf), "%18s  %d %s with %d subplatforms",
		 " ", dbg_CompositeCount(), "composite platforms",
		 dbg_SubplatCount());
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%18s: %li bytes for platforms\n",
		 "Memory usage", dt_NPlatform() * sizeof(DaemonPlatform));
	sprintf (buf+strlen(buf), "%18s  %li bytes for classes\n", " ",
		 dt_NClass() * sizeof(PlatformClass));
	msg_AnswerQuery (who, buf);

	sprintf (buf, "%18s: revision method: %s; \n", "Variables",
		 (StatRevisions) ? "stat()" : "count");
	sprintf (buf+strlen(buf), "%18s  debugging: %s; ", " ",
		 Debug ? "enabled" : "disabled");
	msg_AnswerQuery (who, buf);

	msg_FinishQuery (who);
	return (0);
}


