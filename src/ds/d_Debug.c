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
MAKE_RCSID("$Id: d_Debug.c,v 3.1 1994-04-23 17:59:34 granger Exp $")


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
	{ DPF_ABSTRACT, "abstract" },
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

	printf ("::::: Class %s ::::: (Superclass: %s)\n", pc->dpc_name,
		(pc_SuperClass(pc)) ? pc_SuperClass(pc)->dpc_name : "none");
	printf ("      Dir:  %s\n", pc->dpc_dir);
	printf ("     RDir:  %s\n", pc->dpc_rdir);
	printf ("      Org:  %s\n", orgstr[pc->dpc_org]);
	printf ("    FType:  %s\n", ftypestr[pc->dpc_ftype+1]);
	printf ("     Keep:  %hu\n", pc->dpc_keep);
	printf ("  MaxSamp:  %hu\n", pc->dpc_maxsamp);
	printf ("  Inherit:  %s\n", inheritdir[pc->dpc_inherit]);
	printf (" Instance:  %s\n", instancedir[pc->dpc_instance]);
	printf ("    Flags: ");
	for (i = 0; i < nflagstr; ++i)
	{
		if (pc->dpc_flags & flagstr[i].mask)
			printf (" %s", flagstr[i].name);
	}
	printf ("\n");
	printf (" Subplats: ");
	for (i = 0; i < pc->dpc_nsubplats; ++i)
	{
		printf (" {%s,%s}", 
			(CTable + pc->dpc_subplats[i].dps_class)->dpc_name,
			pc->dpc_subplats[i].dps_name);
	}
	printf ("\n");
	printf ("::::: End   %s :::::\n", pc->dpc_name);
}



void
dbg_DumpInstance (pi)
PlatformInstance *pi;
{
	int i;

	printf ("::::: Instance %s ::::: (Class: %s)\n", pi->dp_name,
		pi_Class(pi)->dpc_name);
	printf ("      Dir:  %s\n", pi->dp_dir);
	printf ("     RDir:  %s\n", pi->dp_rdir);
	printf ("   Parent:  %s\n", 
		(pi_Parent(pi) != NULL) ? pi_Parent(pi)->dp_name : "none");
	printf ("    Flags: ");
	for (i = 0; i < nflagstr; ++i)
	{
		if (pi->dp_flags & flagstr[i].mask)
			printf (" %s", flagstr[i].name);
	}
	printf ("\n");
	if (pi->dp_nsubplats)
	{
		printf (" Subplats: ");
		for (i = 0; i < pi->dp_nsubplats; ++i)
		{
			PlatformInstance *sub;

			sub = PTable + pi->dp_subplats[i];
			printf (" {%s,%s}", pi_Class(sub)->dpc_name, 
				sub->dp_name);
		}
		printf ("\n");
	}
	printf ("::::: End %s :::::\n", pi->dp_name);
}



void
dbg_DumpStatus ()
{
	printf ("........ Stats\n");
	printf ("    Platforms: %d, space for %d in table, growth at %d\n",
		NPlatform, PTableSize, PTableGrow);
	printf ("      Classes: %d, space for %d in table, growth at %d\n",
		NClass, CTableSize, CTableGrow);
	printf (" Memory Usage: platforms = %i bytes, classes = %i bytes\n",
		NPlatform * sizeof(Platform), NClass * sizeof(PlatformClass));
	printf ("........ End stats\n");
}


void
dbg_DumpTables ()
/*
 * Dump all of the entries in the class and platform tables 
 */
{
	int i;
	PlatformClass *pc;
	PlatformInstance *pi;

	printf ("============================= Class Table =======\n");
	for (i = 0; i < NClass; ++i)
	{
		pc = CTable + i;
		printf ("ID %4i: %s (superclass %s), directory %s\n", 
			i, pc->dpc_name, pc_SuperClass(pc) ? 
			pc_SuperClass(pc)->dpc_name : "none", pc->dpc_dir);
		dbg_DumpClass (pc);
	}
	printf ("============================= Platform Table ====\n");
	for (i = 0; i < NPlatform; ++i)
	{
		pi = PTable + i;
		printf ("ID %4i: %s (class %s), directory %s\n", 
			i, pi->dp_name, pi_Class(pi)->dpc_name, pi->dp_dir);
		dbg_DumpInstance (pi);
	}
	printf ("=================================================\n");
}

