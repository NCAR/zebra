/*
 * Debugging utilities for the daemon
 */
# include <unistd.h>
# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "commands.h"
# include "dsDaemon.h"

MAKE_RCSID("$Id: d_Debug.c,v 3.2 1994-04-27 08:24:17 granger Exp $")

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
} DataOrganization;
#endif

static char *orgstr[] = {
	"unknown", "2dgrid", "irgrid", "scalar", "image",
	"outline", "3dgrid", "cmpimage", "1dgrid", "transparent",
	"fixedscalar" };


#ifdef FILETYPES
typedef enum {
	FTUnknown = -1,
	FTNetCDF = 0,
	FTBoundary = 1,
	FTRaster = 2,
	FTCmpRaster = 3,
	FTZeb = 4,
	/* ... */
} FileType;
#endif

static char *ftypestr[] = {
	"unknown", "netcdf", "boundary", "raster", "cmpraster", "zeb" };

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
	printf (" Memory Usage: platforms = %i bytes, classes = %i bytes\n",
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
	char tmp[sizeof(lock->l_Owner) + 64];

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



int
dbg_AnswerQuery (who)
char *who;
/*
 * Handle a zquery.
 */
{
	char buf[1024];
	ZebTime now;

	sprintf (buf, "Zeb data store daemon, protocol version %08x",
		 DSProtocolVersion);
	msg_AnswerQuery (who, buf);
	sprintf (buf, "%s", 
	 "$Id: d_Debug.c,v 3.2 1994-04-27 08:24:17 granger Exp $");
	msg_AnswerQuery (who, buf);

	tl_Time (&now);
	sprintf (buf, "Up since ");
	TC_EncodeTime (&Genesis, TC_Full, buf+strlen(buf));
	sprintf (buf+strlen(buf), ", %d minutes and %d seconds ago",
		 (now.zt_Sec - Genesis.zt_Sec) / 60,
		 (now.zt_Sec - Genesis.zt_Sec) % 60);
	msg_AnswerQuery (who, buf);

	if (LastScan.zt_Sec)
	{
		sprintf (buf, "Last full rescan at ");
		TC_EncodeTime (&LastScan, TC_Full, buf+strlen(buf));
		sprintf (buf+strlen(buf), ", %d minutes and %d seconds ago",
			 (now.zt_Sec - LastScan.zt_Sec) / 60,
			 (now.zt_Sec - LastScan.zt_Sec) % 60);
	}
	else
		sprintf (buf, "No full rescans have occurred.");
	msg_AnswerQuery (who, buf);
	if (LastCache.zt_Sec)
	{
		sprintf (buf, "Cache files up-to-date as of ");
		TC_EncodeTime (&LastCache, TC_Full, buf+strlen(buf));
		sprintf (buf+strlen(buf), ", %d minutes and %d seconds ago",
			 (now.zt_Sec - LastCache.zt_Sec) / 60,
			 (now.zt_Sec - LastCache.zt_Sec) % 60);
	}
	else
		sprintf (buf, "No cache file updates have occurred.");
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
	sprintf (buf, "%18s: %i bytes for platforms\n",
		 "Memory usage", NPlatform * sizeof(Platform));
	sprintf (buf+strlen(buf), "%18s  %i bytes for classes\n", " ",
		 NClass * sizeof(PlatformClass));
	sprintf (buf+strlen(buf), "%18s  %d bytes for DFE's", " ",
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
	sprintf (buf+strlen(buf), "remote directories: %s",
		 DisableRemote ? "disabled" : "enabled");
	msg_AnswerQuery (who, buf);

	dbg_DumpLocks (buf, sizeof(buf));
	msg_AnswerQuery (who, buf);

	msg_FinishQuery (who);
}


