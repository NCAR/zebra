/*
 * $Id: aline.c,v 3.4 1995-11-19 16:16:44 granger Exp $
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
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/wait.h>
#include <assert.h>

#include <config.h>
#include <defs.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "ds_fields.h"
#include "DataChunkP.h"

/*
 * If consumers' platform is different from producer's, we'll create a link
 * from producer files to consumer files and tell the daemon to rescan the
 * consumer files to get our updates.
 */
#define PRODUCER_PLAT	"t_producer"	/* platform producer writes to 	*/
#define CONSUMER_PLAT	"t_consumer"	/* platform consumer reads from	*/
#define INVENTORY	10		/* number to produce		*/
#define INTERVAL	5		/* seconds between writes	*/
#define REMOVE_DEST		/* remove consumer file before copying	*/

#define NFIELDS (sizeof(FieldNames)/sizeof(FieldNames[0]))

/* #define MPROF */
/* #define DEBUGGER */

#define dc_DumpDC(dc) {}

char *FieldNames[] = {
	"altitude", "cpres0", "dp", "ept", "mr", "pres",
#ifdef ZNF
	"lat", "lon", "alt", 
#endif
	"rh", "tdry", "twet", "u_wind", "v_wind",
	"wdir", "wspd"
};

FieldId Fields[NFIELDS];

int LinkPlatforms = 0;	/* use links and rescans to get file updates */
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
Init(name)
char *name;
{
	int i;
	char buf[50];

	srandom(10);
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
	msg_ELPrintMask (EF_INFO | EF_PROBLEM | EF_EMERGENCY);
	msg_ELog (EF_INFO, "Hello from '%s'", name);
	strcpy (OurName, name);
	for (i = 0; i < NFIELDS; ++i)
		Fields[i] = F_Lookup(FieldNames[i]);
	for (i = NFIELDS; i < NFIELDS+10; ++i)
		Fields[i] = Fields[0];
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
	struct timeb tp;
	char dbg[256];
	int err = 0;

#ifdef MPROF
	mprof_stop ();
#endif
	if (argc >= 2)
		nconsumers = atoi(argv[1]);
	else
		nconsumers = 2;			/* the default */

	if (strcmp(PRODUCER_PLAT, CONSUMER_PLAT))
		LinkPlatforms = 1;

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
			sprintf (dbg, "exec xgdb aline %d &", getpid());
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
	 * consumer first in case it just a link to the producer.
	 */
	ftime(&tp);
	now.zt_Sec = tp.time;
	now.zt_MicroSec = 0;
	msg_ELog (EF_DEBUG, "deleting files from consumer platform '%s'", 
		  CONSUMER_PLAT);
	platid = ds_LookupPlatform (CONSUMER_PLAT);
	ds_DeleteData (platid, &now);
	if (LinkPlatforms)
	{
		msg_ELog (EF_DEBUG, 
			  "deleting files from producer platform '%s'", 
			  PRODUCER_PLAT);
		platid = ds_LookupPlatform (PRODUCER_PLAT);
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
	sprintf (dbg, "exec xgdb aline %d &", getpid());
	printf ("%s", dbg);
	system (dbg);
	sleep (10);
#endif
	err = Produce(nconsumers);

	return(0);
}




int
Produce(nconsumers)
int nconsumers;
/*
 * Create INVENTORY number of samples, writing them to PLATFORM every 
 * INTERVAL seconds.
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
	int err = 0;
	
	details[ndetail++].dd_Name = DD_ZN_APPEND_SAMPLES;
	details[ndetail].dd_Name = DD_ZN_RESERVE_BLOCK;
	details[ndetail].dd_V.us_v_int = 150 * INVENTORY;
	ndetail++;
	details[ndetail].dd_Name = DD_ZN_HINT_NSAMPLES;
	details[ndetail].dd_V.us_v_int = 2 * INVENTORY;
	ndetail++;
	
	dc_CheckClass (FALSE);
	plat_id = ds_LookupPlatform(PRODUCER_PLAT);
	destid = ds_LookupPlatform (CONSUMER_PLAT);
	value = 0.0;
	for (i = 0; i < INVENTORY; ++i)
	{
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
		if (! ds_StoreBlocks (dc, (i == 0)?TRUE:FALSE, 
				      details, ndetail))
			++err;
		dc_DestroyDC (dc);

		/*
		 * Once we have a file, set-up our command for updating the 
		 * consumer platform if indirect updates are required
		 */
		if (i == 0 && LinkPlatforms)
		{
#ifdef REMOVE_DEST
			sprintf (cmd, "rm -f %s/%s; ", PlatDirectory (destid),
				 PlatFileName (plat_id));
#else
			cmd[0] = '\0';
#endif
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
			ds_ForceRescan (destid, 0);
		}
		value = (int)value + 1.0;
		/* wait INTERVAL seconds on average, +/- INTERVAL/2 */
		sleep((INTERVAL/2) + rnd(INTERVAL));
	}

	msg_ELog (EF_INFO, "Producer finished, waiting for consumers");
	for (i = 0; i < nconsumers; ++i)
	{
		int status;
		wait(&status);
		if (WIFEXITED(status))
			err += WEXITSTATUS(status);
		else if (WIFSIGNALED(status))
		{
			msg_ELog (EF_PROBLEM, "child received signal");
			++err;
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
			dc_DumpDC(dc);
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
			dc_DumpDC(dc);
			dc_DestroyDC (dc);
		}
	}
	/*
	 * We've stored all of our chunks, so we're done
	 */
	ds_ForceClosure();
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
	static nreceived = 0;	/* number of samples processed so far */
	static ZebTime begin = { 0, 0 };
	static err = 0;		/* our error count and eventual exit value */
        float check, value;	/* to check our data values */
	DataChunk *dc;
	char atime[128];
	int fld, i;

	/*
	 * Ignore notifications which have no new samples.  We only want
	 * notifications from the producer, in which there are new samples.
	 */
	if (nsample == 0)
		return;
	if (!begin.zt_Sec)
		begin = *when;
	TC_EncodeTime (when, TC_Full, atime);
	++nreceived;
	msg_ELog (EF_DEBUG, "notify #%d at %s, %s %d samples", 
		  nreceived, atime, CodeNames[ucode], nsample);
	dc = ds_Fetch (plat_id, DCC_Scalar, &begin, when, 
		       Fields, NFIELDS, (dsDetail *)NULL, 0);
	if (!dc)
	{
		msg_ELog (EF_EMERGENCY, "fetch unsuccessful");
		++err;
	}
	else
	{
	    int ndc = dc_GetNSample(dc);
	    if (nreceived != ndc)
	    {
		msg_ELog (EF_PROBLEM, "got %d samples from fetch, expected %d",
			  ndc, nreceived);
		++err;
	    }
	    value = nreceived - ndc;
	    for (i = 0; i < dc_GetNSample(dc); ++i)
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
	    /*
	     * Add a sample attribute to the most recent sample
	     */
	    dc_SetSampleAttr (dc, i - 1, OurName, "consumed");
	    /*
	     * Re-store this chunk with the added attribute
	     */
	    if (! ds_StoreBlocks (dc, FALSE, NULL, 0))
		    ++err;
	    dc_DestroyDC(dc);
	}

	/*
	 * See if we reached our limit
	 */
	if (nreceived >= INVENTORY)
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

	plat_id = ds_LookupPlatform(CONSUMER_PLAT);
	dc_CheckClass (FALSE);
	/*
	 * If we're consuming the same platform as is being produced,
	 * just wait for notifies.  Otherwise we need to schedule rescans
	 * to keep us in sync.
	 */
	ds_RequestNotify (plat_id, 0, ReceiveNotify);
#ifdef notdef
	if (LinkPlatforms)
	{
		int incr = INTERVAL*INCFRAC/2;
		tl_RelativeReq (ReqRescan, &plat_id, incr, incr);
	}
#endif
	/*
	 * If we're not through in a reasonable amount of time, then
	 * something went wrong.
	 */
	alarm (10*INTERVAL*INVENTORY);
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

   ratio = (double)((double)random())/(((double)LONG_MAX+1));
   /* printf ("%lf       ",ratio); */
   trunc = (ratio*((double)(num+1)));
   /* printf ("%lf       ",trunc); */
   result = (int)(trunc);
   /* printf ("%i\n",result); */
   return result;
};



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

