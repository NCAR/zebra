/*
 * Ingest PAM data into the system.
 *
 * Usage:
 *	pam_ingest fcc-platform project f1 f2 ... fn
 *
 */
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include "../include/DataStore.h"
# include <mda.h>
# include <station.h>

static char *rcsid = "$Id: pam_ingest.c,v 1.1 1991-01-25 22:45:03 corbet Exp $";

# ifdef __STDC__
	static int incoming (struct message *);
# else
	static int incoming ();
# endif



/*
 * Data for interfacing to various packages.
 */
DataObject Dobj;
struct dstream *Ds;
int Sample, Report, Ngrab, Nds;
station Mda_plats[100];
field Mda_flds[40];
float *Data;		/* The data array */

# define BADVAL -9999.0


main (argc, argv)
int argc;
char **argv;
{
	time t;
/*
 * Basic arg check.
 */
	if (argc < 4)
	{
		printf ("Usage: pam_ingest platform project fields\n");
		exit (1);
	}
/*
 * Hook into the world.
 */
	usy_init ();
	msg_connect (incoming, "pam_ingest");
	ds_Initialize ();
	mda_declare_file ("/data/ppf", MDA_TYPE_DATABASE, MDA_F_PACKET,
		"pam", argv[2]);
/*
 * Figure our current time and force an init.
 */
	tl_GetTime (&t);
	mda_do_init (t.ds_yymmdd, t.ds_hhmmss);
/*
 * More initialization.
 */
	if ((Dobj.do_id = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		printf ("Unknown platform: %s\n", argv[1]);
		exit (1);
	}
	Dobj.do_org = OrgIRGrid;
	Dobj.do_badval = BADVAL;
	Dobj.do_aloc = 0;
	Dobj.do_flags = 0;
/*
 * Figure out our stations and fields.
 */
	Stations (argv[1]);
	Fields (argc - 3, argv + 3);
/*
 * Now we go for it.
 */
	SnarfLoop ();
}





Stations (plat)
char *plat;
/*
 * Do station-oriented initialization.
 */
{
	int nsta, slist[100], sta, elev;
	IRGrid *irg = &Dobj.do_desc.d_irgrid;
	Location *loc;
	char pname[60];
/*
 * Get our station list.
 */
	sta_g_slist (slist, &nsta);
	irg->ir_npoint = 0;
/*
 * Go through and fill in all of our irgrid info.
 */
	irg->ir_loc = (Location *) malloc (nsta * sizeof (Location));
	irg->ir_subplats = (PlatformId *) malloc (nsta * sizeof (PlatformId));
	for (sta = 0; sta < nsta; sta++)
	{
	/*
	 * Make sure that this station is known to the system.
	 */
		sprintf (pname, "%s/%s", plat, sta_g_name (slist[sta]));
		if ((irg->ir_subplats[irg->ir_npoint] =
				ds_LookupPlatform (pname)) == BadPlatform)
		{
			printf("Station %s unknown\n", pname);
			continue;
		}
	/*
	 * Get the rest of the info.
	 */
		loc = irg->ir_loc + irg->ir_npoint;
		sta_g_position (slist[sta], &loc->l_lat, &loc->l_lon, &elev);
		if (loc->l_lon > 0)
			loc->l_lon *= -1;	/* Assume west hemisphere */
		loc->l_alt = ((float) elev)/1000.0;
		Mda_plats[irg->ir_npoint++] = slist[sta];
	}
/*
 * Finish up by getting the sampling info, which we assume is the same
 * for all of the stations.
 */
	sta_g_rate (slist[0], &Sample, &Report);
	Sample /= NTICKSEC; 
	Report /= NTICKSEC;
	printf ("Sample rate %d, report %d\n", Sample, Report);
}





static int
incoming (msg)
struct message *msg;
/*
 * Deal with incoming messages.
 */
{
	switch (msg->m_proto)
	{
	   case MT_TIMER:
	   	tl_DispatchEvent ((struct tm_time *) msg->m_data);
		break;
	}
	return (0);
}





Fields (nfield, fields)
int nfield;
char **fields;
/*
 * Deal with the list of fields, and make our dstream structures.
 */
{
	int fld, sta;
	IRGrid *irg = &Dobj.do_desc.d_irgrid;
	struct dstream *dsp;
	float *datap;
/*
 * Go through our field list, and validate that we understand each one.
 */
	Dobj.do_nfield = 0;
	for (fld = 0; fld < nfield; fld++)
	{
	 	if (! (Mda_flds[Dobj.do_nfield] = fld_number (fields[fld])))
		{
			printf ("Bad field '%s'\n", fields[fld]);
			continue;
		}
		Dobj.do_fields[Dobj.do_nfield++] = usy_string (fields[fld]);
	}
/*
 * Allocate the space we will need for the data.
 */
	Dobj.do_npoint = Ngrab = Report/Sample;
	Nds = irg->ir_npoint * Dobj.do_nfield;
	Data = (float *) malloc (Ngrab * Nds * sizeof (float));
	Ds = (struct dstream *) malloc (Nds*sizeof (struct dstream));
	Dobj.do_times = (time *) malloc (Ngrab * sizeof (time));
/*
 * Now we go through and fill in all of the dstreams.
 */
	dsp = Ds;
	for (fld = 0; fld < Dobj.do_nfield; fld++)
	{
		datap = Data + fld*Ngrab*irg->ir_npoint;
		Dobj.do_data[fld] = datap;
		for (sta = 0; sta < irg->ir_npoint; sta++)
		{
			dsp->ds_plat = Mda_plats[sta];
			dsp->ds_field = Mda_flds[fld];
			dsp->ds_data = datap++;
			dsp->ds_stride = irg->ir_npoint;
			dsp++;
		}
	}
}







SnarfLoop ()
/*
 * Go through and move data.
 */
{
	time wakeup, t;
	int add;
	void DoSnarf ();
	
	add = (Report/60)*100 + Report % 60;
/*
 * Get our initial time.
 */
	tl_GetTime (&t);
	t.ds_hhmmss = rollback (t.ds_hhmmss, Report/60); /* Last full report */
	wakeup = t;
/*
 * Get the first set of samples.
 */
	DoSnarf (&wakeup, 0);
/*
 * Arrange for things to happen.
 */
	pmu_dadd (&wakeup.ds_yymmdd, &wakeup.ds_hhmmss, add);
	tl_AddAbsoluteEvent (DoSnarf, 0, &wakeup, Report*INCFRAC);
	msg_await ();
}




void
DoSnarf (t, junk)
time *t;
int junk;
/*
 * Handle the data snarf for this time.
 */
{
	int add = (Report/60)*100 + Report % 60, i;
	int sadd = (Sample/60)*100 + Sample % 60;
	time begin, end;
/*
 * Get the data from MDA.
 */
	begin = *t;
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, add);
	end = begin;
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, add);
	pmu_dadd (&begin.ds_yymmdd, &begin.ds_hhmmss, sadd);
	mda_fetch (Nds, Ds, &begin, &end, BADVAL, 0);
/*
 * Fill in the times array.
 */
	Dobj.do_begin = begin;
	Dobj.do_end = end;
	add = (Sample/60)*100 + Sample % 60;
	for (i = 0; i < Ngrab; i++)
	{
		Dobj.do_times[i] = begin;
		pmu_dadd (&begin.ds_yymmdd, &begin.ds_hhmmss, add);
	}
/*
 * Put it into the data store.
 */
	ds_PutData (&Dobj, FALSE);
}





int
rollback (t, period)
int t, period;
/*
 * Roll back the given time to the last even minute multiple of the given
 * period, which is in minutes.
 */
{
	int seconds = mda_t2s (t);
	seconds -= seconds % (period*60);
	return (mda_s2t (seconds));
}
