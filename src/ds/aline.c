/*
 * $Id: aline.c,v 3.8 1996-01-23 19:56:47 granger Exp $
 *
 * An 'Assembly Line' test driver for the DataStore.
 *
 * Run two processes, one which is producing data and writing it to a 
 * platform, and another which is accepting notifications and consuming 
 * the data.
 *
 * Works as follows: first fork, then child becomes consumer and parent
 * is the producer.  Both connect to the DataStore.  The producer creates
 * a single-sample scalar datachunk and sends it off.  Then it waits some
 * specific period, and writes another, and so on.  The consumer requests
 * notifications on the platform.  When notifications arrive, it fetches
 * the new data and logs some distinguishing info about it. 
 *
 * Multiple consumers are possible.  The number can be specified on the
 * command line.  The default is 2.
 *
 * If started with the scan option, the consumer uses a platform whose
 * data files are links to the producer's data files.  The consumer must tell
 * the daemon to rescan the linked platform, and then test to make sure
 * the consumer is kept correctly synchronized with the daemon and the open
 * file.
 */

#include <stdio.h>
#include <limits.h>
#include <sys/types.h>
#include <errno.h>
#include <time.h>
#include <sys/wait.h>
#include <assert.h>

#include <config.h>
#include <defs.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "ds_fields.h"
#include "DataChunkP.h"


#if defined(SYSV) || defined(SVR4)
# ifndef RAND_MAX
#  define RAND_MAX ((double)(32767))
# endif
# define RAND() rand()
# define SRAND(seed) srand(seed)
#else
# ifndef RAND_MAX
#  define RAND_MAX (((double)LONG_MAX+1))
# endif
# define RAND() random()
# define SRAND(seed) srandom(seed)
#endif

/*
 * If consumers' platform is different from producer's, we'll create a link
 * from producer files to consumer files and tell the daemon to rescan the
 * consumer files to get our updates.  This tests the daemon's rescan
 * ability while files are changing outside its control.  
 */
char ProducerPlat[64] = "t_producer";	/* platform producer writes to 	*/
char ConsumerPlat[64] = "t_consumer";	/* platform consumer reads from	*/

static int NConsumers = 2;		/* number of children 		*/
static int RemoveDest = 1;		/* remove consumer file before copy */
static int Interval = 5;		/* seconds between writes	*/
static int FixedInterval = 1;		/* random or fixed interval	*/
static int Inventory = 10;		/* number to produce		*/
static int Debug = 0;
static int Blow = 0;			/* Dump data chunks		*/
static int LinkPlatforms = 1;	/* link/copy/rescan consumer file updates */
static int UseZNF = 0;
/*
 * If true, all consumers act on producer notifications concurrently, as
 * opposed to cascading.
 */
static int Concurrent = 0;

#define NFIELDS (sizeof(FieldNames)/sizeof(FieldNames[0]))

/* #define MPROF */
/* #define DEBUGGER */
#ifdef DEBUGGER
#undef DEBUGGER
#endif
#define NEWFILE 5	/* mod for starting a new file on shared platforms */

#define DumpDC(dc) \
{ \
	 if (Blow) dc_DumpDC (dc); \
}

char *FieldNames[] = {
	"altitude", "cpres0", "dp", "ept", "mr", "pres",
#ifdef ZNF
	"lat", "lon", "alt", 
#endif
	"rh", "tdry", "twet", "u_wind", "v_wind",
	"wdir", "wspd"
};

FieldId Fields[NFIELDS];

int ConsumerNumber = -1;
char OurName[50];

/*
 * Forwards
 */
int Produce();
void Consume();
void ReceiveNotify FP ((PlatformId plat_id, int param, ZebTime *when,
			int nsample, UpdCode ucode));
char *PlatDirectory FP ((PlatformId pid));
void ReqRescan ();
char *PlatFileName FP ((PlatformId pid));
int rnd();


/* ARGSUSED */
int
msg_handler (msg)
struct message *msg;
{
	msg_ELog (EF_INFO, "Message received");
	return (0);
}


void
Init (name)
char *name;
{
	int i;
	char buf[50];

	SRAND(10);
#ifdef MPROF
	sprintf (buf, "mprof.%s", name);
	mprof_restart (buf);
#endif
	usy_init();
	F_Init();
	if (!msg_connect (msg_handler, name) ||
	    !ds_Initialize())
	{
		printf ("%s: Cannot connect nor initialize DS!\n", name);
		exit(1);
	}
	/*
	 * Otherwise let everyone know we're here
	 */
	if (Debug)
		msg_ELPrintMask (EF_ALL);
	else
		msg_ELPrintMask (EF_INFO | EF_PROBLEM | EF_EMERGENCY);
	msg_ELog (EF_INFO, "Hello from '%s'", name);
	strcpy (OurName, name);
	for (i = 0; i < NFIELDS; ++i)
		Fields[i] = F_Lookup(FieldNames[i]);
	for (i = NFIELDS; i < NFIELDS+10; ++i)
		Fields[i] = Fields[0];
}



static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s [options]\n", prog);
	printf (" -nconsumers <number>   Number of consumer children\n");
	printf (" -share                 Share the same platform\n");
	printf (" -znf                   Use ZNF files instead of netCDF\n");
	printf (" -inventory <number>    Number of samples to produce\n");
	printf (" -debug                 Verbose debugging output\n");
	printf (" -blow                  Dump data chunks\n");
	printf (" -period <seconds>      Fixed delay between productions\n");
	printf (" -average <seconds>     Average wait between productions\n");
	printf (" -remove                Remove consumer file (default)\n");
	printf (" -overwrite             Overwrite file rather than remove\n");
	printf (" -concurrent            Concurrent consumer access\n");
	printf (" -help                  This message.\n");
}


static void
ParseOptions (argc, argv)
int argc;
char **argv;
{
	int opt;
	int optlen;

	opt = 0;
	while (++opt < argc)
	{
		optlen = strlen(argv[opt]);
		if (optlen < 2)
		{
			Usage (argv[0]);
			exit (1);
		}
		if (!strncmp(argv[opt], "-help", optlen))
		{
			Usage (argv[0]);
			exit (0);
		}
		else if (!strncmp(argv[opt], "-nconsumers", optlen))
		{
			if (++opt == argc)
			{
				printf ("-nconsumers needs arg\n");
				Usage (argv[0]);
				exit (1);
			}
			NConsumers = atoi(argv[opt]);
		}
		else if (!strncmp(argv[opt], "-share", optlen))
			LinkPlatforms = 0;
		else if (!strncmp(argv[opt], "-debug", optlen))
			Debug = 1;
		else if (!strncmp(argv[opt], "-blow", optlen))
			Blow = 1;
		else if (!strncmp(argv[opt], "-znf", optlen))
			UseZNF = 1;
		else if (!strncmp(argv[opt], "-inventory", optlen))
		{
			if (++opt == argc)
			{
				printf ("-inventory needs arg\n");
				Usage (argv[0]);
				exit (1);
			}
			Inventory = atoi(argv[opt]);
		}
		else if (!strncmp(argv[opt], "-period", optlen))
		{
			if (++opt == argc)
			{
				printf ("-period needs arg\n");
				Usage (argv[0]);
				exit (1);
			}
			Interval = atoi(argv[opt]);
			FixedInterval = 1;
		}
		else if (!strncmp(argv[opt], "-average", optlen))
		{
			if (++opt == argc)
			{
				printf ("-average needs arg\n");
				Usage (argv[0]);
				exit (1);
			}
			Interval = atoi(argv[opt]);
			FixedInterval = 0;
		}
		else if (!strncmp(argv[opt], "-remove", optlen))
			RemoveDest = 1;
		else if (!strncmp(argv[opt], "-overwrite", optlen))
			RemoveDest = 0;
		else if (!strncmp(argv[opt], "-concurrent", optlen))
			Concurrent = 1;
		else
		{
			printf ("unrecognized option: %s\n", argv[opt]);
			Usage (argv[0]);
			exit (1);
		}
	}
}



/*ARGSUSED*/
int
main (argc, argv)
	int argc;
	char *argv[];
{
	int i;
	char name[20];
	int nconsumers;
	PlatformId platid;
	ZebTime now;
	char dbg[256];
	int err = 0;
	int opt;

#ifdef NoBuffer
	setvbuf (stdout, NULL, _IONBF, 0);
	setvbuf (stderr, NULL, _IONBF, 0);
#endif
#ifdef MPROF
	mprof_stop ();
#endif
	ParseOptions(argc, argv);
	nconsumers = NConsumers;

	/*
	 * Setup platforms based on options
	 */
	if (UseZNF)
	{
		strcat (ProducerPlat, "_znf");
		strcat (ConsumerPlat, "_znf");
	}
	if (! LinkPlatforms)	/* share same platform */
	{
		strcpy (ConsumerPlat, ProducerPlat);
	}

	/*
	 * To fork multiple producers/consumers: the parent will be the
	 * First producer, and it creates the child consumers.
	 */
	printf ("Creating %d consumers...\n", nconsumers);
	i = 0;
	while (i < nconsumers)
	{
		int pid;
		if ((pid = fork()) == 0)
		{
			/*
			 * O.K., we're the child here, so we consume.  And
			 * we break out of this loop since we don't want
			 * to create any of our own children.
			 */
			sprintf (name, "Consumer_%d", i);
			ConsumerNumber = i;
			Init(name);
#ifdef DEBUGGER
			sprintf(dbg, "exec %s aline %d &", DEBUGGER, getpid());
			printf ("%s", dbg);
			system (dbg);
			sleep (10);
#endif
			Consume();		/* shouldn't return */
			return (0);
		}
		else if (pid == -1)
		{
			msg_ELog (EF_PROBLEM, "fork of consumer %d failed", i);
			--nconsumers;
			++err;
		}
		else
		{
			/*
			 * Parent continues with creating consumers
			 */
			++i;
		}
	}

	/*
	 * We're the parent, so we'll be the producer.
	 */
	Init("Producer");

	/*
	 * Make sure our platforms are starting out clean.  Remove the
	 * consumer first in case it is just a link to the producer.
	 */
	tl_Time (&now);
	now.zt_MicroSec = 0;
	msg_ELog (EF_DEBUG, "deleting files from consumer platform '%s'", 
		  ConsumerPlat);
	platid = ds_LookupPlatform (ConsumerPlat);
	ds_DeleteData (platid, &now);
	if (LinkPlatforms)
	{
		msg_ELog (EF_DEBUG, 
			  "deleting files from producer platform '%s'", 
			  ProducerPlat);
		platid = ds_LookupPlatform (ProducerPlat);
		ds_DeleteData (platid, &now);
		msg_ELog (EF_DEBUG, 
			  "using linked platforms and rescans for updates");
	}
	else
		msg_ELog (EF_DEBUG, 
			  "producers and consumers using the same platform");

	/*
	 * Produce our inventory, then wait for our children to exit.
	 */
#ifdef DEBUGGER
	sprintf (dbg, "exec %s aline %d &", DEBUGGER, getpid());
	printf ("%s", dbg);
	system (dbg);
	sleep (10);
#endif
	err = Produce(nconsumers);
	return(err);
}




int
Produce(nconsumers)
int nconsumers;
/*
 * Create Inventory number of samples, writing them to PLATFORM every 
 * Interval seconds.
 */
{
	DataChunk *dc;
	int i, fld;
	ZebTime when, now;
	float value;
	PlatformId plat_id;
	Location loc;
	dsDetail details[5];
	int ndetail = 0;
	char cmd[512];
	PlatformId destid;
	int newfile;
	int err = 0;
	int delay;
	
	details[ndetail++].dd_Name = DD_ZN_APPEND_SAMPLES;
	details[ndetail].dd_Name = DD_ZN_RESERVE_BLOCK;
	details[ndetail].dd_V.us_v_int = (NConsumers + 1) * Inventory * 64;
	ndetail++;
	details[ndetail].dd_Name = DD_ZN_HINT_NSAMPLES;
	details[ndetail].dd_V.us_v_int = 2 * Inventory;
	ndetail++;
	
	dc_CheckClass (FALSE);
	plat_id = ds_LookupPlatform(ProducerPlat);
	destid = ds_LookupPlatform (ConsumerPlat);
	value = 0.0;
	for (i = 0; i < Inventory; ++i)
	{
		while (msg_poll(0) != MSG_TIMEOUT) /* clear messages */;
		tl_Time(&when);
		when.zt_MicroSec = 0;

		dc = dc_CreateDC (DCC_Scalar);
		dc->dc_Platform = plat_id;
		dc_SetScalarFields (dc, NFIELDS, Fields);
		dc_SetBadval (dc, 9999.0);
		for (fld = 0; fld < NFIELDS; ++fld, value += 0.01)
			dc_AddScalar (dc, &when, 0,
				      Fields[fld], &value); 
		loc.l_lat = -90.0 + i*180.0/1000.0;
		loc.l_lon = -180.0 + i*360.0/1000.0;
		loc.l_alt = i;
		dc_SetLoc (dc, 0, &loc);
		dc_SetSampleAttr (dc, 0, "creator", "producer");
		/*
		 * Extra twist: start a new file every 5th sample for
		 * shared platforms
		 */
		newfile = ((i == 0) || 
			   (!LinkPlatforms && ((i % NEWFILE) == 0)));
		if (! ds_StoreBlocks (dc, newfile, details, ndetail))
			++err;
		dc_DestroyDC (dc);

		/*
		 * Once we have a file, set-up our command for updating the 
		 * consumer platform if indirect updates are required
		 */
		if (i == 0 && LinkPlatforms)
		{
			/*
			 * Try to make sure the consumer dir exists first
			 */
			mkdir (PlatDirectory (destid), 0775);
			if (RemoveDest)
				sprintf (cmd, "rm -f %s/%s; ", 
					 PlatDirectory (destid),
					 PlatFileName (plat_id));
			else
				cmd[0] = '\0';
			sprintf (cmd+strlen(cmd), "cp %s/%s ", 
				 PlatDirectory (plat_id),
				 PlatFileName (plat_id));
			strcat (cmd, PlatDirectory (destid));
		}

		/*
		 * Now that our file has changed, update the consumer file
		 * indirectly if the consumer platform differs from producer's.
		 * Force the rescan from here, which should generate the 
		 * update notify in the consumer.
		 */
		if (LinkPlatforms)
		{
			msg_ELog (EF_DEBUG, "updating consumer file:");
			msg_ELog (EF_DEBUG, "  %s", cmd);
			system (cmd);
			msg_ELog (EF_DEBUG, "forcing rescan of %s",
				  ds_PlatformName (destid));
			ds_ForceRescan (destid, 0);
		}
		value = (int)value + 1.0;
		/* wait Interval seconds on average, +/- Interval/2 */
		if (FixedInterval)
			delay = Interval;
		else
			delay = (Interval/2) + rnd(Interval);
		if ((i+1 < Inventory) && (delay > 0))
			sleep (delay);
	}

	msg_ELog (EF_INFO, "Producer finished, waiting for consumers");
	i = 0;
	while (i < nconsumers)
	{
		int status;
		int pid;

		pid = wait(&status);
		if (pid < 0)
		{
			if (errno == ECHILD)
				break;
			msg_ELog (EF_PROBLEM, "wait() error %d", errno);
		}
		else if (WIFEXITED(status))
		{
			msg_ELog (EF_INFO, "child %d exited with status %d",
				  pid, WEXITSTATUS(status));
			err += WEXITSTATUS(status);
			++i;
		}
		else if (WIFSTOPPED(status))
		{
			msg_ELog (EF_PROBLEM, "child %d stopped: signal %d",
				  pid, WSTOPSIG(status));
			++i;
		}
#ifdef WIFCONTINUED
		else if (WIFCONTINUED(status))
		{
			msg_ELog (EF_INFO, "child %d continued", pid);
		}
#endif
		else if (WIFSIGNALED(status))
		{
			msg_ELog (EF_PROBLEM, "child %d %s: signal %d",
				  pid, 
#ifdef WCOREDUMP
				  WCOREDUMP(status) ? "dumped core" :
#endif
				  "terminated", 
				  WTERMSIG(status));
			++err;
			++i;
		}
		else
		{
			msg_ELog (EF_PROBLEM, "unknown child state");
			++err;
		}
	}
	msg_ELog (EF_INFO,
		  "Consumer children have terminated, %d errors.", err);
	msg_ELog (EF_DEBUG, "Final state of observations:");
	tl_Time (&now);
	if (ds_GetObsTimes (plat_id, &now, &when, 1, NULL) != 1)
		++err;
	else
	{
		dc = ds_FetchObs (plat_id, DCC_Scalar, &when, Fields, 
				  NFIELDS, NULL, 0);
		if (! dc)
			++err;
		else
		{
			DumpDC(dc);
			dc_DestroyDC (dc);
		}
	}
	if (ds_GetObsTimes (destid, &now, &when, 1, NULL) != 1)
		++err;
	else
	{
		dc = ds_FetchObs (destid, DCC_Scalar, &when, Fields, 
				  NFIELDS, NULL, 0);
		if (! dc)
			++err;
		else
		{
			DumpDC(dc);
			dc_DestroyDC (dc);
		}
	}
	/*
	 * We've stored all of our chunks, so we're done
	 */
	ds_ForceClosure();
	msg_ELog (EF_INFO, "Producer exiting with status %d", err);
	return (err);
}



/*ARGSUSED*/
void
ReceiveNotify (plat_id, param, when, nsample, ucode)
PlatformId plat_id;
int param;
ZebTime *when;
int nsample;
UpdCode ucode;
/*
 * Fetch the data we have been notified about
 */
{
	static const char *CodeNames[] = { "overwrite", "insert", "append" };
	static int nreceived = 0;/* number of samples processed so far */
	static int noverwrite = 0;
	static ZebTime begin = { 0, 0 };
	static err = 0;		/* our error count and eventual exit value */
        float check, value;	/* to check our data values */
	DataChunk *dc;
	char btime[128], atime[128];
	int fld, i;

	if (Concurrent)
	{
		/*
		 * Ignore notifications which have no new samples.  We only
		 * want notifications from the producer, in which there are
		 * new samples.
		 */
		if (nsample == 0)
			return;
	}
	else
	{
		/*
		 * Act on the producer's appended samples in the order of
		 * our consumer number.  If we're consumer 0, act on the
		 * producer's notify.  If we're consumer i, i > 0, act on
		 * the i'th overwrite notify from the preceding consumer.
		 */
		if (nsample == 0)
			++noverwrite;
		else
			noverwrite = 0;
		if (noverwrite != ConsumerNumber)
			return;
	}
	if (!begin.zt_Sec)
		begin = *when;
	TC_EncodeTime (when, TC_Full, atime);
	TC_EncodeTime (&begin, TC_Full, btime);
	++nreceived;
	msg_ELog (EF_DEBUG, "notify #%d at %s, %s %d samples", 
		  nreceived, atime, CodeNames[ucode], nsample);
	msg_ELog (EF_DEBUG, "ds_Fetch %d fields from %s to %s",
		  NFIELDS, btime, atime);
	dc = ds_Fetch (plat_id, DCC_Scalar, &begin, when, 
		       Fields, NFIELDS, (dsDetail *)NULL, 0);
	if (!dc)
	{
		msg_ELog (EF_PROBLEM, "fetch unsuccessful");
		++err;
	}
	else
	{
	    int ndc = dc_GetNSample(dc);
	    DumpDC (dc);
	    if (nreceived != ndc)
	    {
		msg_ELog (EF_PROBLEM, "got %d samples from fetch, expected %d",
			  ndc, nreceived);
		++err;
	    }
	    value = nreceived - ndc;
	    for (i = 0; i < ndc; ++i)
	    {
		/*
		 * Make sure what we got matches what was sent
		 */
		for (fld = 0; fld < NFIELDS; ++fld, value += 0.01)
		{
			check = dc_GetScalar(dc, i, Fields[fld]);
			if ((int)(check*100.0+0.2) != (int)(value*100.0+0.2))
			{
				msg_ELog (EF_PROBLEM,
				   "%s, sample %d, fid %d",
				   "Data comparison failed", i, Fields[fld]);
				++err;
				break;
			}
		}
		value = (int)value + 1.0;
	    }
	    if (i > 0)
	    {
		    char name[128];
		    /*
		     * Add a sample attribute to the most recent sample
		     */
		    dc_SetSampleAttr (dc, i - 1, OurName, "consumed");
#ifdef notdef
		    /*
		     * And a field attribute to the 0th field
		     */
		    sprintf (name, "%s_notify%d", OurName, nreceived);
		    dc_SetFieldAttr (dc, Fields[0], name, atime);
#endif
		    /*
		     * Re-store this chunk with the added attribute
		     */
		    DumpDC (dc);
		    if (! ds_StoreBlocks (dc, FALSE, NULL, 0))
			    ++err;
	    }
	    dc_DestroyDC(dc);
	}

	/*
	 * See if we reached our limit
	 */
	if (nreceived >= Inventory)
	{
		msg_ELog (EF_INFO, "Consumer finished with %d errors.", err);
		ds_ForceClosure();
		exit (err);
	}
}




void
Consume()
/*
 * Consume all that we can from the given platform
 */
{
	static PlatformId plat_id;

	plat_id = ds_LookupPlatform(ConsumerPlat);
	dc_CheckClass (FALSE);
	/*
	 * If we're consuming the same platform as is being produced,
	 * just wait for notifies.  Otherwise we need to schedule rescans
	 * to keep us in sync.
	 */
	ds_RequestNotify (plat_id, 0, ReceiveNotify);
	/*
	 * If we're not through in a reasonable amount of time, then
	 * something went wrong.
	 */
	alarm (2*NConsumers*Interval*Inventory);
	/*
	 * Now we let the receiver handle everything.  After receiving
	 * all of the samples we're expecting, we'll exit.  If we return
	 * from msg_await, then something went wrong there.
	 */
	msg_await();
	exit (1);
}



int rnd( num )
int num;
{
   double ratio;
   double trunc;
   int result;

   ratio = (double)((double)RAND())/RAND_MAX;
   /* printf ("%lf       ",ratio); */
   trunc = (ratio*((double)(num+1)));
   /* printf ("%lf       ",trunc); */
   result = (int)(trunc);
   /* printf ("%i\n",result); */
   return result;
}



char *
PlatDirectory (pid)
PlatformId pid;
/*
 * Return a pointer to a platform's directory path.  The string is only
 * valid until the next call.  Returns NULL if this platform has no
 * local data source.
 */
{
	int i;
        PlatformInfo pinfo;
        static DataSrcInfo dsi;

        ds_LockPlatform (pid);
        ds_GetPlatInfo (pid, &pinfo);
        /*
         * Find the first local data source
         */
        for (i = 0; i < pinfo.pl_NDataSrc; i++)
        {
                ds_GetDataSource (pid, i, &dsi);
                if (dsi.dsrc_Type == dst_Local)
                        break;
        }
        ds_UnlockPlatform (pid);
        if (i < pinfo.pl_NDataSrc)
                return (dsi.dsrc_Where);
        else
                return (NULL);
}               



void
ReqRescan (zt, param)
ZebTime *zt;
void *param;
/*
 * Get the daemon to rescan for new consumer data
 */
{
	char buf[128];
	PlatformId pid = *(PlatformId *)param;
	TC_EncodeTime (zt, TC_Full, buf);
	msg_ELog (EF_DEBUG, "%s: requesting rescan", buf);
	ds_ForceRescan (pid, 0);
}



char *
PlatFileName (pid)
PlatformId pid;
{
        PlatformInfo pinfo;
        DataSrcInfo dsi;
        static DataFileInfo dfi;
        int i, findex;
	char *name = NULL;

        ds_LockPlatform (pid);
        ds_GetPlatInfo (pid, &pinfo);
        for (i = 0; i < pinfo.pl_NDataSrc; i++)
        {
                ds_GetDataSource (pid, i, &dsi);
                if ((dsi.dsrc_Type == dst_Local) &&
		    (dsi.dsrc_FFile > 0))
                        break;
        }
	if (i < pinfo.pl_NDataSrc)
	{
		ds_GetFileInfo (dsi.dsrc_FFile, &dfi);
		name = dfi.dfi_Name;
	}
        ds_UnlockPlatform (pid);
	return (name);
}

