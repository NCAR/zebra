/*
 * $Id: aline.c,v 3.2 1994-01-03 07:17:47 granger Exp $
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
 * the new data and prints logs some distinguishing info about it. 
 *
 * Multiple consumers are possible.  The number can be specified on the
 * command line.  The default is 2.
 */

#include <config.h>
#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "ds_fields.h"
#include "DataChunkP.h"

#include <stdio.h>
#include <limits.h>
#include <sys/types.h>

#define PLATFORM	"t_scalar"	/* platform to use for now	*/
#define INVENTORY	20		/* number to produce		*/
#define INTERVAL	3		/* seconds between writes	*/


#define NFIELDS (sizeof(FieldNames)/sizeof(FieldNames[0]))

/* #define MPROF */

char *FieldNames[] = {
	"altitude", "cpres0", "dp", "ept", "mr", "pres",
	"lat", "lon", "alt", "rh", "tdry", "twet", "u_wind", "v_wind",
	"wdir", "wspd"
};

FieldId Fields[NFIELDS];

int ConsumerNumber = -1;
char OurName[50];

/*
 * Forwards
 */
void Produce();
void Consume();
void ReceiveNotify FP ((PlatformId plat_id, int param, ZebTime *when,
			int nsample, UpdCode ucode));
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

#ifdef MPROF
	mprof_stop ();
#endif
	if (argc >= 2)
		nconsumers = atoi(argv[1]);
	else
		nconsumers = 2;			/* the default */
	/*
	 * To fork multiple producers/consumers: the parent will be the
	 * First producer, and it creates the child consumers.
	 */
	printf ("Creating %d consumers...\n", nconsumers);
	for (i = 0; i < nconsumers; ++i)
	{
		if (fork() == 0)
		{
			/*
			 * O.K., we're the child here, so we consume.  And
			 * we break out of this loop since we don't want
			 * to create any of our own children.
			 */
			sprintf (name, "Consumer_%d", i);
			ConsumerNumber = i;
			Init(name);
			Consume();		/* shouldn't return */
			return (0);
		}
	}

	/*
	 * We're the parent, so we'll be the producer.
	 */
	Init("Producer");

	/*
	 * Produce our inventory, then wait for our children to exit.
	 */
	Produce(nconsumers);

	return(0);
}




void
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

	details[ndetail++].dd_Name = DD_ZN_APPEND_SAMPLES;
	details[ndetail].dd_Name = DD_ZN_RESERVE_BLOCK;
	details[ndetail].dd_V.us_v_int = 150 * INVENTORY;
	ndetail++;
	details[ndetail].dd_Name = DD_ZN_HINT_NSAMPLES;
	details[ndetail].dd_V.us_v_int = 2 * INVENTORY;
	ndetail++;
	
	dc_CheckClass (FALSE);
	plat_id = ds_LookupPlatform(PLATFORM);

	value = 0.0;
	for (i = 0; i < INVENTORY; ++i)
	{
		tl_Time(&when);

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
		ds_StoreBlocks (dc, (i == 0)?TRUE:FALSE, details, ndetail);
		dc_DestroyDC (dc);
		value = (int)value + 1.0;
		sleep(rnd(INTERVAL)+1);		/* wait at least 1 second */
	}

	msg_ELog (EF_INFO, "Producer finished, waiting for consumers");
	for (i = 0; i < nconsumers; ++i)
		wait(0);
	msg_ELog (EF_INFO, "Consumer children signalled, exiting");
	printf ("Final state of observation:\n");
	tl_Time (&now);
	ds_GetObsTimes (plat_id, &now, &when, 1, NULL);
	dc = ds_FetchObs (plat_id, DCC_Scalar, &when, Fields, NFIELDS,
			  NULL, 0);
	dc_DumpDC(dc);
	dc_DestroyDC (dc);

	/*
	 * We've stored all of our chunks, so we're done
	 */
	ds_ForceClosure();
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
	static nreceived = 0;	/* number of samples processed so far */
	static ZebTime begin = { 0, 0 };
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
	msg_ELog (EF_DEBUG, "notify #%d, fetching new data at %s", 
		  nreceived, atime);
	dc = ds_Fetch (plat_id, DCC_Scalar, &begin, when, 
		       Fields, NFIELDS, (dsDetail *)NULL, 0);
	if (!dc)
		msg_ELog (EF_EMERGENCY, "fetch unsuccessful");
	else
	{
	    value = 0.0;
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
	    ds_StoreBlocks (dc, FALSE, NULL, 0);
	    dc_DestroyDC(dc);
	}

	/*
	 * See if we reached our limit
	 */
	++nreceived;
	if (nreceived >= INVENTORY)
	{
		msg_ELog (EF_INFO, "Consumer finished.");
		ds_ForceClosure();
		exit (0);
	}
}




void
Consume()
/*
 * Consume all that we can from the given platform
 */
{
	PlatformId plat_id;

	plat_id = ds_LookupPlatform(PLATFORM);

	dc_CheckClass (FALSE);
	ds_RequestNotify (plat_id, 0, ReceiveNotify);

	/*
	 * Now we let the receiver handle everything.  After receiving
	 * all of the samples we're expecting, we'll exit.
	 */
	msg_await();
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

