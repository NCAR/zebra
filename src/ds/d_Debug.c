/*
 * Debugging utilities for the daemon
 */
# include <unistd.h>
# include <string.h>

# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "commands.h"
# include "dsDaemon.h"

MAKE_RCSID("$Id: d_Debug.c,v 3.7 1995-08-31 09:49:02 granger Exp $")

#ifdef ORGANIZATIONS
typedef enum {
	OrgUnknown	= 0,
	Org2dGrid	= 1,
	OrgIRGrid	= 2,
	OrgScalar	= 3,
	OrgImage	= 4,
	OrgOutline	= 5,
	Org3dGrid	= 6,
	OrgCmpImage	= 7,
        Org1dGrid       = 8,
	OrgTransparent  = 9,
	OrgFixedScalar  = 10,	/* Inflexible scalar for DFA_Zeb */
	OrgNSpace	= 11
} DataOrganization;
#endif

static char *orgstr[] = {
	"unknown", "2dgrid", "irgrid", "scalar", "image",
	"outline", "3dgrid", "cmpimage", "1dgrid", "transparent",
	"fixedscalar", "nspace" };


#ifdef FILETYPES
typedef enum {
	FTUnknown = -1,
	FTNetCDF = 0,
	FTBoundary = 1,
	FTRaster = 2,
	FTCmpRaster = 3,
	FTZeb = 4,
	FTGRIB = 5,
	FTGRIBSfc = 6,	/* GRIB surface grids only */
	FTGrads = 7
	/* ... */
} FileType;
#endif

static char *ftypestr[] = {
	"unknown", "netcdf", "boundary", "raster", "cmpraster", "zeb",
	"grib", "grib_sfc", "grads"
};

static char *inheritdir[] = { "none", "append", "copy" };

static char *instancedir[] = { 
	"default", "copyclass", "subdirclass", "copyparent", "subdirparent"
};
				       

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
	{ DPF_SPLIT, "daysplit" },
	{ DPF_MODEL, "model" },
	{ DPF_ABSTRACT, "abstract" },
	{ DPF_VIRTUAL, "virtual" },
	{ DPF_DIRTY, "dirty" },
	{ DPF_CLOADED, "cache_loaded" },
	{ DPF_RCLOADED, "rcache_loaded" }
};

static int nflagstr = sizeof(flagstr)/sizeof(flagstr[0]);


void
dbg_DumpClass (pc)
PlatformClass *pc;
{
	int i;

	printf ("::::::::: Class %s, id %d, superclass %s\n", pc->dpc_name,
		pc - CTable,
		(pc_SuperClass(pc)) ? pc_SuperClass(pc)->dpc_name : "none");
	printf ("     Dir: %s\n", pc->dpc_dir);
	printf ("    RDir: %s\n", pc->dpc_rdir);
	printf ("     Org: %s\n", orgstr[pc->dpc_org]);
	printf ("   FType: %s\n", ftypestr[pc->dpc_ftype+1]);
	printf ("    Keep: %hu\n", pc->dpc_keep);
	printf (" MaxSamp: %hu\n", pc->dpc_maxsamp);
	printf (" Inherit: %s\n", inheritdir[pc->dpc_inherit]);
	printf ("Instance: %s\n", instancedir[pc->dpc_instance]);
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
		printf (" {%s,%s}", 
			(CTable + pc->dpc_subplats[i].dps_class)->dpc_name,
			pc->dpc_subplats[i].dps_name);
	}
	printf ("\n");
	printf ("::::::::: End   %s :::::\n", pc->dpc_name);
}



void
dbg_DumpInstance (pi)
PlatformInstance *pi;
{
	int i;

	printf ("_______ Instance %s, id %d, class %s\n", 
		pi->dp_name, pi - PTable, pi_Class(pi)->dpc_name);
	printf ("     Dir: %s\n", pi->dp_dir);
	printf ("    RDir: %s\n", pi->dp_rdir);
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
			PlatformInstance *sub;

			sub = PTable + pi->dp_subplats[i];
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
		NPlatform, PTableSize, PTableGrow);
	printf ("      Classes: %d, space for %d in table, growth at %d\n",
		NClass, CTableSize, CTableGrow);
	printf (" Memory Usage: platforms = %li bytes, classes = %li bytes\n",
		NPlatform * sizeof(Platform), NClass * sizeof(PlatformClass));
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

	printf ("%s Class Table ====================\n", dash);
	for (i = 0; i < NClass; ++i)
	{
		dbg_DumpClass (CTable + i);
	}
	printf ("%s Platform Table =================\n", dash);
	for (i = 0; i < NPlatform; ++i)
	{
		dbg_DumpInstance (PTable + i);
	}
	printf ("%s=================================\n", dash);
}



int
dbg_DirtyCount ()
/*
 * Count the number of platforms which are 'dirty'
 */
{
	int count = 0;
	int i;

	for (i = 0; i < NPlatform; ++i)
		if (pi_Dirty(PTable + i))
			++count;
	return (count);
}



int
dbg_SubplatCount ()
/*
 * Count the number of platforms which are 'dirty'
 */
{
	int count = 0;
	int i;

	for (i = 0; i < NPlatform; ++i)
		if (pi_Subplatform(PTable + i))
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

	for (i = 0; i < NPlatform; ++i)
		if (pi_Composite(PTable + i))
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
dbg_DumpLockQueue (p, lock, que, buf, len)
Platform *p;
Lock *lock;
char *que;
char *buf;
int len;
{
	char tmp[sizeof(lock->l_Owner) + 64];

	if (lock)
	{
		sprintf (tmp, "\t%s lock queue for '%s':", que, pi_Name(p));
		dbg_Append(buf, tmp, len);
		for ( ; lock; lock = lock->l_Next)
		{
			sprintf (tmp, " %s", lock->l_Owner);
			dbg_Append(buf, tmp, len);
		}
		if (dbg_Append (buf, "\n", len) < 0)
			return;
	}
}



void
dbg_DumpLocks (buf, len)
char *buf;	/* Buffer to dump info into	*/
int len;	/* Length of the buffer		*/
{
	int i;
	Platform *p;
	Lock *lock;

	/*
	 * Traverse the platform instances, printing the
	 * read and write locks on each one
	 */
	sprintf (buf, "Locks:\n");
	for (i = 0; i < NPlatform; ++i)
	{
		p = PTable + i;

		lock = p->dp_RLockQ;
		dbg_DumpLockQueue (p, lock, "Read", buf, len);
		lock = p->dp_WLockQ;
		dbg_DumpLockQueue (p, lock, "Write", buf, len);
	}
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

	sprintf (buf, "Zebra data store daemon, proto %08x, cache key %08x",
		 DSProtocolVersion, CacheKey);
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%s", 
	 "$Id: d_Debug.c,v 3.7 1995-08-31 09:49:02 granger Exp $");
	msg_AnswerQuery (who, buf);

	dbg_EncodeElapsed ("Up since ", &Genesis, &now, buf);
	msg_AnswerQuery (who, buf);

	if (InitialScan)
		sprintf (buf, "Initial scan: %d %s so far, %d to go...",
			 PlatformsScanned, "platforms scanned", 
			 NPlatform - PlatformsScanned);
	else if (LastScan)
		dbg_EncodeElapsed ("Last full rescan ", 
				   &LastScan, &now, buf);
	else
		sprintf (buf, "No full rescans have occurred.");
	msg_AnswerQuery (who, buf);

	if (LastCache)
		dbg_EncodeElapsed ("Cache files written ",
				   &LastCache, &now, buf);
	else
		sprintf (buf, "No cache file writes have occurred.");
	msg_AnswerQuery (who, buf);

	sprintf (buf, "%18s: %s\n", "Data directory", DefDataDir);
	sprintf (buf+strlen(buf), "%18s: %s", "Remote directory",
		 RemDataDir);
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%18s: %d used of %d allocated, %s %d",
		 "Platform classes", NClass, CTableSize, "grow by", 
		 CTableGrow);
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%18s: %d used of %d allocated, %s %d\n", 
		 "Platforms", NPlatform, PTableSize, "grow by", PTableGrow);
	sprintf (buf+strlen(buf), "%18s  %d %s with %d subplatforms",
		 " ", dbg_CompositeCount(), "composite platforms",
		 dbg_SubplatCount());
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%18s: %d used of %d, grow by %d", 
		 "DataFile entries", NDTEUsed, DFTableSize, DFTableGrow);
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%18s: %li bytes for platforms\n",
		 "Memory usage", NPlatform * sizeof(Platform));
	sprintf (buf+strlen(buf), "%18s  %li bytes for classes\n", " ",
		 NClass * sizeof(PlatformClass));
	sprintf (buf+strlen(buf), "%18s  %ld bytes for DFE's", " ",
		 NDTEUsed * sizeof (DataFile));
	msg_AnswerQuery (who, buf);

	sprintf (buf, "%18s: %d platforms marked dirty\n", 
		 "Statistics", dbg_DirtyCount());
	sprintf (buf+strlen(buf), "%18s  %d cache invalidate messages sent\n",
		 " ", InvalidatesSent);
	sprintf (buf+strlen(buf), "%18s  %d write lock requests\n",
		 " ", WriteLockRequests);
	sprintf (buf+strlen(buf), "%18s  %d read lock requests",
		 " ", ReadLockRequests);
	msg_AnswerQuery (who, buf);
	
	sprintf (buf, "%18s: revision method: %s; ", "Variables",
		 (StatRevisions) ? "stat()" : "count");
	sprintf (buf+strlen(buf), "cache on exit: %s\n",
		 CacheOnExit ? "true" : "false");
	sprintf (buf+strlen(buf), "%18s  local files const: %s; ", " ",
		 LFileConst ? "true" : "false");
	sprintf (buf+strlen(buf), "local dir const: %s\n",
		 LDirConst ? "true" : "false");
	sprintf (buf+strlen(buf), "%18s  remote files const: %s; ", " ",
		 RFileConst ? "true" : "false");
	sprintf (buf+strlen(buf), "remote dir const: %s\n",
		 RDirConst ? "true" : "false");
	sprintf (buf+strlen(buf), "%18s  debugging: %s; ", " ",
		 Debug ? "enabled" : "disabled");
	sprintf (buf+strlen(buf), "remote directories: %s\n",
		 DisableRemote ? "disabled" : "enabled");
	sprintf (buf+strlen(buf), "%18s  create local directories: %s",
		 " ", DelayDataDirs ? "delayed" : "on startup");
	msg_AnswerQuery (who, buf);

	dbg_DumpLocks (buf, sizeof(buf));
	msg_AnswerQuery (who, buf);

	msg_FinishQuery (who);
	return (0);
}


