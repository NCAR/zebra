/*
 * $Id: nstest.c,v 1.19 1995-08-31 09:50:09 granger Exp $
 */

/*
 * Attempt to modularize the testing process:
 *
 * Provide routines for each of the different DataChunk classes which
 * create test DataChunks (perhaps these tests should be in the class
 * itself---perhaps even a DataChunk method).  The caller provides the
 * starting time, whether the chunk is mobile or stationary, and the number
 * of samples wanted, and maybe whether or not to add attributes, etc.
 *
 * Provide routines which store a given DataChunk to a named platform,
 * then fetch the DataChunk and compare it with the original.  This should
 * be independent of platform and datachunk class (especially if DataChunk
 * instance comparison we're also made a class method).

Example: each class performs comparisons relevant to its domain.
Transparent tests number of samples, times of each sample, mobility,
locations.  MetData tests the same fields.  Scalar knows how to test data
values.  The compare method calls the superclasses of the datachunk classes
from raw down to the class and returns with a list of diff messages, if
any.

Built-in test mode: which does things like omit history and creation date
info from netcdf files.  In test mode, have error log messages come back to
application, or to some routine which can match them against what was
expected.  Low-level development messages, like defining variables and
dimns in n-space might not be logged unless test mode enabled.

Perhaps a verify mode, where each call to a class method calls the opposite
call to make sure the modifications succeeded correctly.  E.g. SetFields
immediately calls GetFields and compares.

Diffs might be useful for editing data chunks and keeping a history of the
changes, perhaps even undo info.

If more than one process could receive event messages, the test program
could drive the data store and parse the event messages being logged.

 * At some point, it would be nice to construct a ds.config file from here
 * given what test we want to make, and then fork() and exec() a dsDaemon
 * on the config file ourselves.  Keep the "t_" prefix convention for all test
 * platforms.
 *
 * Bind the test functions to Tcl commands.  Each call to a Tcl command 
 * registers the desire to run that test, then on 'begin', the ds.config is
 * generated and the test routines are run.  Could we then pipe some output
 * to 'expect'?
 *

Perhaps use the ingest library to log messages to both stdout and
event logger...

 * See ~granger/zeb/ds-tests.notes */

/* #define MARK */
/*
 * #include <prof.h>
 */
#ifndef MARK
#define MARK(L) {}
#else
#undef MARK
#define MARK(L) {\
                asm("   .reserve        ."#L"., 4, \"data\", 4");\
                asm("M."#L":");\
                asm("   sethi   %hi(."#L".), %o0");\
                asm("   call    mcount");\
                asm("   or      %o0, %lo(."#L".), %o0");\
                }
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <assert.h>

#include "ingest.h"
#include "DataChunkP.h"

#ifdef notdef
#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "ds_fields.h"
#endif

/* ARGSUSED */
int
msg_handler (msg)
struct message *msg;
{
	IngestLog (EF_INFO, "Message received");
	return (0);
}

#define EXPECT(N) \
	IngestLog (EF_INFO, "Expect %d problem(s):", N)

#ifdef notdef
/* #define BACKWARDS */		/* backwards compatibility */
/* #define ZNF_TESTING */
#endif

#define DAYSPLIT
#define TIME_UNITS
#define EXAMPLE_ONLY
#define NSPACE_AERI
#define AERI_TYPES
#define ATTRIBUTES
#define TRANSPARENT
#define SCALAR
#define GETFIELDS
#define GRID_BLOCKS
#define DELETE_OBS	/* test observation deletes */
#define NSPACE
#define COPY_SCALAR
#define DUMMY_FILES
#define TEST4_STORE
#define SCALAR_NSPACE
#define FIELD_TYPES
#define ATTRIBUTES
#define NEXUS
#define RGRID		/* test DC RGrid interface */
#define DUMMY_FILES
#define DATA_TIMES
#define FETCH_GAP

#ifdef BACKWARDS
#define dc_CheckClass(a)		{}
#define DD_ZN_HINT_NSAMPLES		""
#define dc_HintNSamples(dc,n,b)		{}
#define dc_HintSampleSize(dc,s,b)	{}
#define dc_HintMoreSamples(dc,m,b)	{}
#define dc_AddMoreSamples(dc,n,b)	{}
#define ds_ForceClosure()		{}
#endif

struct TestField {
	char *name;
	char *desc;
	char *units;
} TestFields[] = {
	"scalar",	"Test Field",			"none",
	"field2",	"Test Field, too",		"none",
	"fld3",		"field #3",			"none",
	"no4",		"Number 4 field",		"none",
	"pres",		"Pressure",		       	"mb",
	"temp",		"Temperature",			"degC",
	"rh",		"Relative humidity",		"pct",
	"dpt",		"Dewpoint",			"K"
};


struct TestPlatform {
	char *name;
	long maxsamples;
	char *ftype;
	bool mobile;
	char *org;
	PlatformId platid;
} TestPlatforms[] = 
{
	{ "t_transparent", 500, "zeb", FALSE, "transparent" },
	{ "t_scalar" }, 
	{ "t_blocks" },
	{ "t_fixed" },
	{ "t_irgrid_cdf" },
	{ "t_1dgrid_cdf" },
	{ "t_irgrid_znf" },
	{ "t_1dgrid_znf" },
	{ "t_daysplit" },
#ifdef DUMMY_FILES
	{ "t_dummy_cdf" },
	{ "t_dummy_znf" },
	{ "t_dummy_single" },
#endif
#ifdef FETCH_GAP
	{ "t_gap_cdf" },
	{ "t_gap_znf" },
#endif
#ifdef DELETE_OBS
	{ "t_deletes" },
#endif
#ifdef NSPACE
	{ "t_nspace" },
	{ "t_nsscalar" },
	{ "t_nsblocks" },
	{ "t_test6" },
#endif
#ifdef COPY_SCALAR
	{ "t_copy_source" },
	{ "t_copy_dest" },
#endif
#ifdef AERI_TYPES
	{ "t_aeri_types_cdf" },
	{ "t_aeri_types_znf" },
#endif
	{ "t_virtual" },
	{ "t_getfields_cdf" },
	{ "t_getfields_znf" },
	{ "t_nsvsc_scalar" },
	{ "t_nsvsc_nspace" },
	{ "t_fieldtypes" },
#ifdef ATTRIBUTES
	{ "t_att_types_cdf" },
#endif
#ifdef TIME_UNITS
	{ "t_time_units" },
	{ "t_time_units_2" },
#endif
};

	

#define NUM_PLATFORMS	(sizeof(TestPlatforms)/sizeof(TestPlatforms[0]))
#define NUM_TESTFIELDS 	(sizeof(TestFields)/sizeof(TestFields[0]))

static float test_data[10000];


DataChunk *T_SimpleScalarChunk FP ((ZebTime *start, int interval, int nsample,
				    int nfield, bool is_mobile, bool addatts));
DataChunk *T_ScalarNSpaceChunk FP((ZebTime *start, int nsample, int nfield,
				   bool is_mobile, bool addatts));


static void
Announce(header)
char *header;
/*
 * Announce beginning of test sequence to event logger and to stdout
 */
{
	IngestLog (EF_INFO, "%s", header);
	printf ("%s\n", header);
	fflush (stdout);
	fflush (stderr);
}



static void
InitializePlatforms()
{
	int i;
	PlatformId platid;
	ZebTime now;
	struct timeb tp;

	Announce ("initializing data, erasing platforms");
	ftime(&tp);
	now.zt_Sec = tp.time;
	now.zt_MicroSec = 0;
	for (i = 0; i < NUM_PLATFORMS; ++i)
	{
		platid = ds_LookupPlatform (TestPlatforms[i].name);
		TestPlatforms[i].platid = platid;
		if (platid == BadPlatform)
		{
			fprintf (stderr, "platform '%s' unknown",
				 TestPlatforms[i].name);
			exit (1);
		}
		else
		{
			ds_DeleteData (platid, &now);
		}
	}
}

	

static void
Initialize()
{
	usy_init();
	F_Init();
	if (!msg_connect (msg_handler, "Test") ||
	    !ds_Initialize())
	{
		printf ("Cannot connect nor initialize DS!");
		exit(1);
	}
	InitializePlatforms();
}



/*ARGSUSED*/
int
main (argc, argv)
	int argc;
	char *argv[];
{
	DataChunk *dc, *ndc;
	int i,j;
	ZebTime when, begin, end;
	ZebTime now;
	struct timeb tp;
	FieldId *fields;
	int nfield;
	int ndim, nvar, rndim, rstatic;
	char *name;
	unsigned long size;
	float *retrieve;
	PlatformId plat_id;
	PlatformId t_delete_id;

	Initialize();

#ifdef ZNF_TESTING
	T_ZnfBlocks();
#endif

	TC_ZtAssemble (&when, 93, 1, 1, 0, 0, 0, 0);
	begin = when;
	end = when;

	for (i = 0; i < sizeof(test_data)/sizeof(test_data[0]); ++i)
		test_data[i] = i;

#ifdef TRANSPARENT
	T_Transparent ("t_transparent", &begin);
#endif

#ifdef GETFIELDS
	/*
	 * Test ds_GetFields() for ZNF and netCDF
	 */
	T_GetFields (&begin, "t_getfields_cdf");
	T_GetFields (&begin, "t_getfields_znf");
#endif
		
#ifdef DELETE_OBS
	T_Deletes (&begin);
#endif

#ifdef FETCH_GAP
	T_FetchGap (&begin, "t_gap_cdf");
	T_FetchGap (&begin, "t_gap_znf");
#endif

#if defined(DUMMY_FILES) && defined(DATA_TIMES)
	T_DataTimes ("t_dummy_cdf");
	T_DataTimes ("t_dummy_znf");
	T_DataTimes ("t_dummy_single");
#endif

#ifdef SCALAR
	Announce ("t_scalar: 4 fields, 10 samples, mobile, attributes");
	dc = T_SimpleScalarChunk (&begin, 1, 10, 4, TRUE, TRUE);
	plat_id = ds_LookupPlatform("t_scalar");
	dc->dc_Platform = plat_id;
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);

	/*
	 * Now really put it through the ringer.  Start 30 seconds
	 * back so that we precede the previous data, then overwrite
	 * it, and finally append the rest.  Note that the sample
	 * attributes should be deleted on the overwrite, since they 
	 * won't correspond to the samples with atts here.
	 */
	Announce("t_scalar: 4 fields, 3000 samples, starting 30 secs back");
	when.zt_Sec -= 30;
	dc = T_SimpleScalarChunk (&when, 1, 3000, 4, TRUE, TRUE);
	dc->dc_Platform = plat_id;
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	IngestLog (EF_INFO, 
		   "t_fixed: storing same datachunk as for t_scalar");
	dc->dc_Platform = ds_LookupPlatform("t_fixed");
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	when.zt_Sec += 3000;

	/*
	 * Now overwrite a huge block of the previously stored data.
	 */
	Announce("t_scalar: 4 fields, 1500 samples, 1500 secs prior to end");
	when.zt_Sec -= 1500;
	dc = T_SimpleScalarChunk (&when, 1, 1500, 4, TRUE, TRUE);
	dc->dc_Platform = plat_id;
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	Announce("t_fixed: same datachunk as for t_scalar above");
	dc->dc_Platform = ds_LookupPlatform("t_fixed");
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	when.zt_Sec += 1500;

	/*
	 * Now overwrite, but only with a subset of the fields
	 */
	Announce("t_scalar: 3 fields, 1000 samples, 1250 secs prior to end");
	when.zt_Sec -= 1250;
	dc = T_SimpleScalarChunk (&when, 1, 1000, 3, TRUE, TRUE);
	dc->dc_Platform = plat_id;
	{
		struct TestField tf;
		tf = TestFields[1];
		TestFields[1] = TestFields[3];
		TestFields[3] = tf;
	}
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	Announce("t_fixed: same datachunk as for t_scalar above");
	dc->dc_Platform = ds_LookupPlatform("t_fixed");
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	when.zt_Sec += 1000;
#endif /* SCALAR */

#ifdef NSPACE
	T_NSpace (&begin);
#endif

#ifdef NSPACE_AERI
	T_Aeri();
	fflush (stdout);
#endif

#ifdef NEXUS
	/* test nexus-specific considerations, such as big, block overwrites */
	T_Nexus (&begin);
#endif

#ifdef GRID_BLOCKS
	printf ("---------------------------------------------block tests\n");
	T_IRGridStoreBlocks();
	T_1DGridStoreBlocks();
#endif

#ifdef FIELD_TYPES
	T_FieldTypes (begin);
#endif

#ifdef AERI_TYPES
	T_AeriTypes (begin);
#endif

#ifdef SCALAR_NSPACE
	T_ScalarNSpace (begin);
#endif

#ifdef ATTRIBUTES
	T_Attributes (begin);
#endif

#ifdef RGRID
	T_RGrid (begin);
#endif

#ifdef COPY_SCALAR
        T_CopyScalar (&begin);
#endif

#ifdef TIME_UNITS
	T_TimeUnits (&begin);
#endif

#ifdef DAYSPLIT
	T_Daysplit (&begin);
#endif

	ds_ForceClosure();
	
	Announce ("Tests completed.");
	return(0);
}



#ifdef DUMMY_FILES
#ifdef DATA_TIMES
static void
CheckTimes (pid, desc, when, check, which, expect, first)
PlatformId pid;
char *desc;	/* description of the test */
ZebTime *when;	/* target time for DataTimes */
int check;	/* number of times to check */
TimeSpec which; /* timespec to pass DataTimes */
int expect;	/* number of times expected on return */
long first;	/* beginning of period (secs) expected */
/*
 * Call ds_DataTimes using given parameters and test for correct results.
 */
{
	ZebTime times[512];
	ZebTime test;
	char buf[256];
	int n, i;

	n = ds_DataTimes (pid, when, check, which, times);

	TC_EncodeTime (when, TC_Full, buf);
	printf ("DataTimes: %s\n", desc);
	printf ("           %d found %s %s\n", n,
		(which == DsBefore) ? "before" : "after", buf);
	if (n != expect)
		printf ("ERROR! DataTimes: %d expected, %d returned\n",
			expect, n);
	test.zt_MicroSec = 0;
	for (i = 0; i < n; ++i) 
	{
		TC_EncodeTime (times+i, TC_Full, buf);
		printf ("%s\n", buf);
		test.zt_Sec = first + expect - 1 - i;
		if (! TC_Eq(times[i], test))
		{
			TC_EncodeTime (&test, TC_Full, buf);
			printf ("ERROR! expected time was %s\n", buf);
		}
	}
}
#endif /* DATA_TIMES */


T_DataTimes (platform)
char *platform;
{
	DataChunk *dc;
	int i,j;
	ZebTime when, begin, end;
	ZebTime now, next;
	struct timeb tp;
#	define NS (60*5)	/* 5 60-sample files */
	ZebTime reftime;
	ZebTime times[NS];
	ZebTime first, last;
	PlatformId pid;
	int n;
	char buf[128];

	ftime(&tp);
	first.zt_MicroSec = 0;
	first.zt_Sec = tp.time;
	first.zt_Sec -= (first.zt_Sec % 60) + NS;
	dc = T_SimpleScalarChunk (&first, 1, NS, 4, FALSE, FALSE);
	pid = ds_LookupPlatform(platform);
	dc->dc_Platform = pid;
	dc_GetTime (dc, NS - 1, &last);
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
#ifdef DATA_TIMES 	/* use the dummy files to test ds_DataTimes */
	/*
	 * To test ds_DataTimes, lets pass it now as the reference time,
	 * and space for all of the times, and make sure we get the right
	 * times back.  Then try doing smaller sets of times.
	 */
	Announce ("---- Testing ds_DataTimes()...");
	printf ("DataTimes: platform '%s'\n", platform);
	tl_Time (&now);
	CheckTimes (pid, "all times before now", &now, NS, DsBefore,
		    NS, last.zt_Sec - NS + 1);
	CheckTimes (pid, "two most recent times", &now, 2, DsBefore,
		    2, last.zt_Sec - 2 + 1);
	CheckTimes (pid, "two times before first", &first, 2, DsBefore,
		    1, first.zt_Sec);
	next = first;
	next.zt_Sec += 60;
	CheckTimes (pid, "three times before 1st time of 2nd file", 
		    &next, 3, DsBefore, 3, next.zt_Sec - 3 + 1);
	next.zt_Sec += 60;
	CheckTimes (pid, "five times before 1st time of 3rd file", 
		    &next, 5, DsBefore, 5, next.zt_Sec - 5 + 1);
	next.zt_Sec += 30;
	CheckTimes (pid, "10 times after middle of 3rd file", 
		    &next, 10, DsAfter, 10, next.zt_Sec);
	CheckTimes (pid, "10 times after last time", 
		    &last, 10, DsAfter, 1, last.zt_Sec);
	CheckTimes (pid, "120 times after first time", 
		    &first, 120, DsAfter, 120, first.zt_Sec);
	next.zt_Sec = 0;
	CheckTimes (pid, "5 times after prehistoric time", 
		    &next, 5, DsAfter, 5, first.zt_Sec);
	Announce ("------------ DataTimes: done ------------");
#   endif
}
#endif	/* DUMMY_FILES */



#ifdef FETCH_GAP
T_FetchGap (begin, plat)
ZebTime *begin;
char *plat;
{
	ZebTime when, next;
	DataChunk *dc;
	PlatformId pid;
	FieldId fields[4];
	int nfield = 4;
	char buf[128];

	sprintf (buf, "FetchGap: testing platform '%s'", plat);
	Announce (buf);
	when = *begin;
	/* 10 samples, 30-second intervals */
	dc = T_SimpleScalarChunk (begin, 30, 10, 4, FALSE, FALSE);
	pid = ds_LookupPlatform(plat);
	dc->dc_Platform = pid;
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	/*
	 * Now fetch an interval within the data file but which does not
	 * contain any times in the file.  The result should be NULL.
	 */
	when.zt_Sec += 15;		/* halfway between samples */
	ds_GetFields (pid, &when, &nfield, fields);
	dc = ds_Fetch (pid, DCC_Scalar, &when, &when, fields, nfield, NULL, 0);
	if (dc)
	{
		IngestLog (EF_PROBLEM, "FetchGap: dc has %d samples",
			   dc_GetNSample (dc));
		dc_DestroyDC (dc);
	}
	/*
	 * Now try an interval of time
	 */
	when.zt_Sec += 150 - 5;	/* 10 seconds past a sample in the middle */
	next = when;
	next.zt_Sec += 10;
	dc = ds_Fetch (pid, DCC_Scalar, &when, &next, fields, nfield, NULL, 0);
	if (dc)
	{
		IngestLog (EF_PROBLEM, "FetchGap: dc has %d samples",
			   dc_GetNSample (dc));
		dc_DestroyDC (dc);
	}
	/*
	 * Finally, an interval which does contain samples but does not
	 * start at the same time as any sample
	 */
	next = when;
	next.zt_Sec += 120;
	dc = ds_Fetch (pid, DCC_Scalar, &when, &next, fields, nfield, NULL, 0);
	if (dc)
	{
		int ns = dc_GetNSample (dc);
		if (ns != 4)
		{
			IngestLog(EF_PROBLEM, "FetchGap: dc has %d samples %s",
				  dc_GetNSample (dc), "instead of 4");
		}
		dc_DestroyDC (dc);
	}
	else
		IngestLog(EF_PROBLEM, "FetchGap: fetch 120 sec returned null");
	/*
	 * When beginning and end of interval match first and last times
	 */
	when = *begin;
	next = when;
	next.zt_Sec += 300;
	dc = ds_Fetch (pid, DCC_Scalar, &when, &next, fields, nfield, NULL, 0);
	if (dc)
	{
		int ns = dc_GetNSample (dc);
		if (ns != 10)
		{
			IngestLog(EF_PROBLEM, "FetchGap: dc has %d samples %s",
				  dc_GetNSample (dc), "instead of 10");
		}
		dc_DestroyDC (dc);
	}
	else
		IngestLog(EF_PROBLEM, "FetchGap: fetch 320 sec returned null");
	/*
	 * When interval is outside first and last sample times
	 */
	when = *begin;
	next = when;
	when.zt_Sec -= 10;
	next.zt_Sec += 310;
	dc = ds_Fetch (pid, DCC_Scalar, &when, &next, fields, nfield, NULL, 0);
	if (dc)
	{
		int ns = dc_GetNSample (dc);
		if (ns != 10)
		{
			IngestLog(EF_PROBLEM, "FetchGap: dc has %d samples %s",
				  dc_GetNSample (dc), "instead of 10");
		}
		dc_DestroyDC (dc);
	}
	else
		IngestLog(EF_PROBLEM, "FetchGap: fetch 320 sec returned null");
	Announce ("FetchGap: test completed.");
}
#endif	/* FETCH_GAP */



T_ScalarNSpace (begin)
ZebTime begin;
{
	DataChunk *dc;
	PlatformId plat_id;
	FieldId fields[10];
	int nfield;
	float value;
	int i, f;
	char *sc_plat = "t_nsvsc_scalar";
	char *ns_plat = "t_nsvsc_nspace";
#	define NUMS 1000

	dc_CheckClass (FALSE);

	MARK(scbld);
	Announce ("t_nsvsc_scalar: 8 fields, mobile, attributes");
	dc = T_SimpleScalarChunk (&begin, 1, NUMS, 8, TRUE, TRUE);
	plat_id = ds_LookupPlatform(sc_plat);
	dc->dc_Platform = plat_id;
	MARK(scsto);
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	printf ("fetching the entire observation from '%s'...\n", sc_plat);
	nfield = 10;
	ds_GetFields (plat_id, &begin, &nfield, fields);
	MARK(scfet);
	dc = ds_FetchObs (plat_id, DCC_Scalar, &begin, 
			  fields, nfield, NULL, 0);
	printf ("Done.\n");
	MARK(scacc);
	for (i = 0; i < NUMS; ++i)
		for (f = 0; f < nfield; ++f)
			value = dc_GetScalar (dc, i, fields[f]);
	dc_DumpDC (dc);
	dc_DestroyDC(dc);

	MARK(nsbld);
	Announce ("t_nsvsc_nspace: 8 fields, mobile, attributes");
	dc = T_ScalarNSpaceChunk (&begin, NUMS, 8, TRUE, TRUE);
	plat_id = ds_LookupPlatform(ns_plat);
	dc->dc_Platform = plat_id;
	MARK(nssto);
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	printf ("fetching the entire observation from '%s'...\n", ns_plat);
	nfield = 10;
	ds_GetFields (plat_id, &begin, &nfield, fields);
	MARK(nsfet);
	dc = ds_FetchObs (plat_id, DCC_NSpace, &begin, 
			  fields, nfield, NULL, 0);
	printf ("Done.\n");
	MARK(nsacc);
	for (i = 0; i < NUMS; ++i)
		for (f = 0; f < nfield; ++f)
			value = *(float *)dc_NSGetSample (dc, i, fields[f], 0);
	dc_DumpDC (dc);
	dc_DestroyDC(dc);

	dc_CheckClass (TRUE);
}





#ifdef FIELD_TYPES
T_AddTypedScalarSample (dc, when, sample, fields)
DataChunk *dc;
ZebTime when;
int sample;
FieldId *fields;
{
	static char cvalue = 'A';
	static int ivalue = 1024;
	static double dvalue = 1.2345678901234;
	static short svalue = 1;
	static float fvalue = -1.1;

	dc_AddScalar (dc, &when, sample, fields[DCT_Float-1], &fvalue);
	fvalue -= 2.1;
	dc_AddScalar (dc, &when, sample, fields[DCT_Double-1], &dvalue);
	dvalue *= 2;
	dc_AddScalar (dc, &when, sample, fields[DCT_LongDouble-1], &dvalue);
	dvalue *= 2;
	dc_AddScalar (dc, &when, sample, fields[DCT_Char-1], &cvalue);
	++cvalue;
	dc_AddScalar (dc, &when, sample, fields[DCT_UnsignedChar-1], &cvalue);
	++cvalue;
	dc_AddScalar (dc, &when, sample, fields[DCT_ShortInt-1], &svalue);
	svalue <<= 1;
	dc_AddScalar (dc, &when, sample, fields[DCT_UnsignedShort-1], &svalue);
	svalue += 1;
	dc_AddScalar (dc, &when, sample, fields[DCT_Integer-1], &ivalue);
	++ivalue;
	dc_AddScalar (dc, &when, sample, fields[DCT_UnsignedInt-1], &ivalue);
	ivalue *= 2.0;
	dc_AddScalar (dc, &when, sample, fields[DCT_LongInt-1], &ivalue);
	++ivalue;
	dc_AddScalar (dc, &when, sample, fields[DCT_UnsignedLong-1], &ivalue);
	++ivalue;
}
#endif /* FIELD_TYPES */



#ifdef FIELD_TYPES
T_FieldTypes(when)
ZebTime when;
/*
 * Test definition of DataChunks with multiple field types.  Just define
 * the chunk as usual, change the field type, and dump the DC.  No storage
 * done yet.  We cheat and access the global sizes and names table to make
 * this easier.
 */
{
	FieldId fields[20];
	DC_ElemType types[20];
	int nfield = DCT_String - 1;	/* exclude String and Unknown */
	int i;
	DataChunk *dc;
	ZebTime begin = when;
	static Location loc = { 40.0, -160.0, 5280.0 };
	PlatformId plat_id = ds_LookupPlatform ("t_fieldtypes");

	Announce ("------------ Testing field types interfaces ---------");
	for (i = 1; i < nfield+1; ++i)
	{
		printf ("type name: %s, size %d\n", dc_TypeName(i), 
			dc_SizeOfType(i));
		fields[i-1] = F_DeclareField ((char *)dc_TypeName(i),
					    "Typed field","none");
		types[i-1] = i;
	}

	/*
	 * Now add all of these fields to each class of DataChunk and dump it
	 */
	dc_CheckClass (FALSE);
	dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = plat_id;
	dc_SetStaticLoc (dc, &loc);
	dc_HintNSamples (dc, 10, FALSE);
	dc_SetScalarFields (dc, nfield, fields);
	dc_SetFieldTypes (dc, nfield, fields, types);
	T_AddTypedScalarSample (dc, when, 0, fields);
	printf ("nsamples hint should be 10, but only 1 sample allocated\n");
	dc_DumpDC (dc);
	++when.zt_Sec;
	T_AddTypedScalarSample (dc, when, 1, fields);
	printf ("nsamples allocated should now be 10, \n");
	printf ("space for 8 samples between NextOffset and DataLen:\n");
	++when.zt_Sec;
	dc_DumpDC (dc);
	for (i = 2; i < 10; ++i, ++when.zt_Sec)
		T_AddTypedScalarSample (dc, when, i, fields);
	dc_DumpDC (dc);
	fflush (stdout);

	Announce ("ds_Store()'ing the typed DataChunk to t_fieldtypes");
	ds_Store (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	Announce ("Fetching the typed scalar observation");
	dc = ds_FetchObs (plat_id, DCC_Scalar, &begin, fields, nfield, 0, 0);
	dc_DumpDC (dc);
	dc_DestroyDC (dc);
}
#endif /* FIELD_TYPES */




void
T_Dedit (begin, platid)
ZebTime begin;
PlatformId platid;
{
	FieldId fields[30];
	int nfield = 30;
	DataChunk *dc;
	int i;
	char attval[128];

	Announce("simulating dedit read and overwrite with sample attributes");
	ds_GetFields (platid, &begin, &nfield, fields);
	dc = ds_FetchObs (platid, DCC_Scalar, &begin, fields, nfield, NULL, 0);

	/*
	 * Add some sample attributes
	 */
	for (i = 0; i < dc_GetNSample(dc); i += 5)
	{
		sprintf (attval, "sample %d", i);
		dc_SetSampleAttr (dc, i, "key", attval);
	}

	/* 
	 * now store it all back, overwriting what's already there
	 */
	ds_StoreBlocks (dc, FALSE, 0, 0);
	dc_DestroyDC(dc);

	/*
	 * Now do the fetch/store cycle several times to make sure it
	 * doesn't mess up the reserve space
	 */
	Announce ("performing repeated fetch/store cycles");
	for (i = 0; i < 50; ++i)
	{
		dc = ds_FetchObs (platid, DCC_Scalar, &begin, fields, 
				  nfield, NULL, 0);
		ds_StoreBlocks (dc, FALSE, 0, 0);
		dc_DestroyDC(dc);
	}
	Announce ("t_dedit done.");
}




T_Nexus (begin)
ZebTime *begin;
/*
 * The idea is this: create single-sample DataChunks and store them
 * individually to a single ZNF file, then fetch a DataChunk for the entire
 * file, then overwrite the entire file, and see what's so slow about it.
 * Halfway through, rewrite the current contents of the file and include
 * some sample attributes (ala dedit).
 */
{
	DataChunk *dc;
	char *pname = "t_virtual";
	PlatformId plat_id;
	FieldId fields[40];
	int i, n, fld;
	float value;
	Location loc;
	ZebTime when = *begin;
	dsDetail details[5];
	int ndetail;
	char buf[128];

	plat_id = ds_LookupPlatform(pname);
	n = 0;
	fields[n] = F_Lookup("pres"); ++n;
	fields[n] = F_Lookup("rh"); ++n;
	fields[n] = F_Lookup("uwind"); ++n;
	fields[n] = F_Lookup("vwind"); ++n;
	fields[n] = F_Lookup("wspd"); ++n;
	fields[n] = F_Lookup("wdir"); ++n;
	fields[n] = F_Lookup("temp"); ++n;
	fields[n] = F_Lookup("dpt"); ++n;
	fields[n] = F_Lookup("tdry"); ++n;
	fields[n] = F_Lookup("twet"); ++n;
	fields[n] = F_Lookup("Qpres"); ++n;
	fields[n] = F_Lookup("Qtdry"); ++n;
	fields[n] = F_Lookup("Qu"); ++n;
	fields[n] = F_Lookup("Qv"); ++n;
	fields[n] = F_Lookup("alt"); ++n;
	fields[n] = F_Lookup("theta"); ++n;
	fields[n] = F_Lookup("range"); ++n;
	fields[n] = F_Lookup("azimuth"); ++n;
	fields[n] = F_Lookup("elev"); ++n;
	fields[n] = F_Lookup("latitude"); ++n;
	fields[n] = F_Lookup("longitude"); ++n;
	fields[n] = F_Lookup("ascent"); ++n;
	fields[n] = F_Lookup("mr"); ++n;
	loc.l_lat = 40.0;
	loc.l_lon = -120.0;
	loc.l_alt = 1600.0;
	ndetail = 0;
#if !defined(BACKWARDS)
#ifdef DD_ZN_APPEND_SAMPLES
	details[ndetail++].dd_Name = DD_ZN_APPEND_SAMPLES;
#endif
#endif /* !BACKWARDS */

	/*
	 * Set some optimization parameters
	 */
	dc_CheckClass (FALSE);

	/* now begin creating a single sample DataChunk and storing it */
	sprintf (buf, "creating '%s' observation of 500 samples, 1 at a time",
		pname);
	Announce (buf);
	for (i = 0; i < 500; ++i)
	{
		dc = dc_CreateDC (DCC_Scalar);
		dc->dc_Platform = plat_id;
		dc_SetScalarFields (dc, n, fields);
		dc_SetBadval (dc, 999.9);
		/* dc_SetGlobalAttr (dc, "global_key", "global_value"); */

		for (fld = 0; fld < n; ++fld)
		{
			value = i*10.0 + fld*0.1;
			dc_AddScalar (dc, &when, 0, fields[fld], &value);
		}
		dc_SetLoc (dc, 0, &loc);

		/*
		 * For the first store, which creates the file, we have a
		 * few extra details to send.
		 */
		if (i == 0)
		{
#if !defined(BACKWARDS)
			details[ndetail].dd_Name = DD_ZN_HINT_NSAMPLES;
			details[ndetail].dd_V.us_v_int = 1000;
			++ndetail;
			details[ndetail].dd_Name = DD_ZN_RESERVE_BLOCK;
			/* Reserve space for 100 sample atts of 50 bytes */
			details[ndetail].dd_V.us_v_int = 100*50;
			++ndetail;
#endif /* !BACKWARDS */
			ds_StoreBlocks (dc, TRUE, details, ndetail);
#if !defined(BACKWARDS)
			ndetail -= 2;
#endif /* !BACKWARDS */
		}
		else
			ds_StoreBlocks (dc, FALSE, details, ndetail);
		dc_DestroyDC(dc);
		loc.l_alt += 5.0;
		when.zt_Sec += 4;

		/*
		 * If we're halfway, simulate interjection by dedit
		 */
		if (i == 250)
			T_Dedit (*begin, plat_id);
	}

	/* now have a file of 500 samples, try to fetch the whole thing */
	printf ("fetching the entire observation from '%s'...\n", pname);
	dc = ds_FetchObs (plat_id, DCC_Scalar, begin, fields, n, NULL, 0);
	dc_DumpDC (dc);
	/* now store it all back, overwriting what's already there; this
	 * is so that we can verify the fetch through what's in the file */
	printf ("overwriting observation with fetched datachunk... \n");
	ds_StoreBlocks (dc, FALSE, details, ndetail);
	printf ("Done.\n");
	dc_DestroyDC(dc);
}



#ifdef COPY_SCALAR
/*
 * Functions for copying attributes between datachunks
 */
FieldId DestFID;
DataChunk *DestDC;

int
CopyGlobalAtts (key, value)
char *key;
char *value;
{
	dc_SetGlobalAttr (DestDC, key, value);
	return (0);
}

int
CopyFieldAtts (key, value)
char *key;
char *value;
{
	dc_SetFieldAttr (DestDC, DestFID, key, value);
	return (0);
}


T_CopyScalar (begin)
ZebTime *begin;
/*
 * Store lots of scalar samples with lots of fields, then fetch it back
 * and try to copy the fetched datachunk into another datachunk with
 * one new field.
 */
{
	DataChunk *dc, *newdc;
	char *pname = "t_copy_source";
	char *pdest = "t_copy_dest";
	PlatformId plat_id;
	FieldId fields[40];
	int i, n, fld;
	float value;
	Location loc;
	ZebTime when = *begin;
	dsDetail details[5];
	int ndetail;
	char buf[128];

	Announce ("Copying one datachunk to another, adding one field");
	plat_id = ds_LookupPlatform(pname);
	n = 0;
	fields[n] = F_Lookup("pres"); ++n;
	fields[n] = F_Lookup("rh"); ++n;
	fields[n] = F_Lookup("uwind"); ++n;
	fields[n] = F_Lookup("vwind"); ++n;
	fields[n] = F_Lookup("wspd"); ++n;
	fields[n] = F_Lookup("wdir"); ++n;
	fields[n] = F_Lookup("temp"); ++n;
	fields[n] = F_Lookup("dpt"); ++n;
	fields[n] = F_Lookup("tdry"); ++n;
	fields[n] = F_Lookup("twet"); ++n;
	fields[n] = F_Lookup("Qpres"); ++n;
	fields[n] = F_Lookup("Qtdry"); ++n;
	fields[n] = F_Lookup("Qu"); ++n;
	fields[n] = F_Lookup("Qv"); ++n;
	fields[n] = F_Lookup("theta"); ++n;
	fields[n] = F_Lookup("range"); ++n;
	fields[n] = F_Lookup("azimuth"); ++n;
	fields[n] = F_Lookup("elev"); ++n;
	fields[n] = F_Lookup("latitude"); ++n;
	fields[n] = F_Lookup("longitude"); ++n;
	fields[n] = F_Lookup("ascent"); ++n;
	fields[n] = F_Lookup("mr"); ++n;
	loc.l_lat = 40.0;
	loc.l_lon = -120.0;
	loc.l_alt = 1600.0;
	ndetail = 0;

	/* now begin creating a single sample DataChunk and storing it */
	sprintf (buf, "creating '%s' observation of 1000 samples", pname);
	Announce (buf);
	dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = plat_id;
	dc_SetScalarFields (dc, n, fields);
	dc_SetBadval (dc, 999.9);
	for (i = 0; i < 1000; ++i)
	{
		for (fld = 0; fld < n; ++fld)
		{
			value = i*10.0 + fld*0.1;
			dc_AddScalar (dc, &when, i, fields[fld], &value);
		}
		dc_SetLoc (dc, i, &loc);

		loc.l_alt += 5.0;
		when.zt_Sec += 5;
	}
	ds_StoreBlocks (dc, TRUE, details, ndetail);
	dc_DestroyDC(dc);

	/* now have a file of 500 samples, try to fetch the whole thing */
	printf ("fetching the entire observation from '%s'...\n", pname);
	dc = ds_FetchObs (plat_id, DCC_Scalar, begin, fields, n, NULL, 0);
	dc_DumpDC (dc);

	/* now create our second dc, and go through the hassle of adding
	 * that one more field to it and copying attributes and data */
	printf ("creating our copy dc for '%s', adding field 'avg'\n", pdest);
	newdc = dc_CreateDC (DCC_Scalar);
	newdc->dc_Platform = ds_LookupPlatform (pdest);

	fields[n] = F_Lookup("avg"); ++n;
	dc_SetScalarFields (newdc, n, fields);
	dc_SetBadval (newdc, 999.9);
	DestDC = newdc;
	dc_ProcessAttrs (dc, NULL, CopyGlobalAtts);
	for (fld = 0; fld < n; ++fld)
	{
		DestFID = fields[fld];
		dc_ProcessFieldAttrs (dc, fields[fld], NULL, CopyFieldAtts);
	}

	/*
	 * Last but not least, copy all of the data, including locations.
	 */
	dc_DumpDC (newdc);
	printf ("copying data from src dc to dest dc...\n");
	for (i = 0; i < 1000; ++i)
	{
		dc_GetTime (dc, i, &when);
		for (fld = 0; fld < n-1; ++fld)
		{
			value = dc_GetScalar (dc, i, fields[fld]);
			dc_AddScalar (newdc, &when, i, fields[fld], &value);
		}
		value *= 2.0;
		dc_AddScalar (newdc, &when, i, fields[n-1], &value);

		dc_GetLoc (dc, i, &loc);
		dc_SetLoc (newdc, i, &loc);
	}
	dc_DumpDC (newdc);
	printf ("storing the new datachunk to '%s'\n", pdest);
	ds_StoreBlocks (newdc, TRUE, details, ndetail);
	dc_DestroyDC(newdc);
	dc_DestroyDC(dc);
	Announce ("Done with DataChunk copy test.");
}
#endif /* COPY_SCALAR */



#ifdef NSPACE
T_NSpace(now)
ZebTime *now;
{
	PlatformId plat_id;
	DataChunk *dc, *ndc;
	int i;
	static Location loc = { 40.0, -160.0, 5280.0 };
	ZebTime when = *now;
	ZebTime end = *now;
	ZebTime begin = *now;
	float *retrieve;
	long size;
	FieldId *fields;
	int nfield;


	plat_id = ds_LookupPlatform("t_nspace");
	Announce("Testing NSpace interface and storage");

	Announce("------------------- test1 ------------------------");
	{	/* an empty NSpace data chunk */
		
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = plat_id;
		dc_DumpDC (dc);
		dc_SetStaticLoc (dc, &loc);
		ds_Store (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	Announce("-------------------- test2 -----------------------");
	{	/* simple non-static variable over two dimensions */

		FieldId field, sfield;
		FieldId fids[2];
		char *dim_names[2];
		unsigned long dim_sizes[2];

		dim_names[0] = "x";	dim_names[1] = "y";
		dim_sizes[0] = 50;	dim_sizes[1] = 25;
		field = F_DeclareField ("curl","Long name","units");
		sfield = F_DeclareField ("static_curl","Long name","units");
		fids[0] = field;
		fids[1] = sfield;
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = plat_id;
		dc_NSDefineField (dc, field, 2, dim_names, dim_sizes, FALSE);
		dc_NSDefineField (dc, sfield, 2, dim_names, dim_sizes, TRUE);
		dc_DumpDC (dc);

		EXPECT(1);
		dc_NSAddSample (dc, &when, 0, sfield, test_data+1000);
		dc_NSAddSample (dc, &when, 0, field, test_data+1000);
		EXPECT(1);
		dc_NSAddStatic (dc, field, test_data);
		dc_NSAddStatic (dc, sfield, test_data);
		dc_SetSampleAttr (dc, 0, "test_key", "test_value");
		dc_SetSampleAttr (dc, 0, "test_key2", "test_value2");
		dc_SetSampleAttr (dc, 1, "sample_number", "0");
		++when.zt_Sec;
		dc_NSAddSample (dc, &when, 1, field, test_data+1005);
		dc_SetSampleAttr (dc, 1, "sample_number", "1");
		dc_DumpDC (dc);

		/* retrieve data and compare */
		printf ("retrieving static data with GetStatic...");
		retrieve = dc_NSGetStatic (dc, sfield, &size);
		T_CompareData(retrieve, test_data, size);
		printf ("retrieving static data with GetSample(1)...");
		retrieve = dc_NSGetSample (dc, 1, sfield, &size);
		T_CompareData(retrieve, test_data, size);
		EXPECT(1);
		printf ("retrieving static data with GetSample(100)...");
		retrieve = dc_NSGetSample (dc, 100, sfield, &size);
		T_CompareData(retrieve, test_data, size);
		retrieve = dc_NSGetSample (dc, 1, field, &size);
		printf ("dc_NSGetSample(%s) returns size = %lu,", 
			F_GetName(field), size);
		T_CompareData(retrieve, test_data+1005, size);

		T_NSGetField(dc, field);
		T_NSGetField(dc, sfield);
		/* T_NSGetAllDimensions(dc, field); */

		printf("Storing... "); fflush(stdout);
		dc_SetStaticLoc (dc, &loc);
		ds_Store (dc, TRUE, 0, 0);
		printf("Destroying... ");
		dc_DestroyDC (dc);

		/* now try to fetch what we just stored and see what we get */
		printf("Fetching data....   "); fflush(stdout);
		dc = ds_Fetch (plat_id, DCC_NSpace, &when, &when,
			       fids, 2, NULL, 0);
		printf("DataChunk returned by ds_Fetch():\n");
		dc_DumpDC (dc);

		/* retrieve data and compare */
		T_NSGetField(dc, field);
		T_NSGetField(dc, sfield);
		printf ("Comparing retrieved dynamic data...\n");
		retrieve = dc_NSGetSample (dc, 0, field, &size);
		T_CompareData(retrieve, test_data+1005, size);
		printf ("Comparing retrieved static data...\n");
		retrieve = dc_NSGetStatic (dc, sfield, &size);
		printf ("dc_NSGetStatic() returns size = %lu,", size);
		T_CompareData(retrieve, test_data, size);
		dc_DestroyDC (dc);

		/* now try again but with fields in reverse order */
		printf("Fetching data, fields reversed...  "); fflush(stdout);
		fids[0] = sfield;
		fids[1] = field;
		dc = ds_Fetch (plat_id, DCC_NSpace, &when, &when,
			       fids, 2, NULL, 0);
		printf("DataChunk returned by ds_Fetch():\n");
		dc_DumpDC (dc);

		/* retrieve data and compare */
		T_NSGetField(dc, field);
		T_NSGetField(dc, sfield);
		printf ("Comparing retrieved dynamic data...\n");
		retrieve = dc_NSGetSample (dc, 0, field, &size);
		T_CompareData(retrieve, test_data+1005, size);
		printf ("Comparing retrieved static data...\n");
		retrieve = dc_NSGetStatic (dc, sfield, &size);
		printf ("dc_NSGetStatic() returns size = %lu,", size);
		T_CompareData(retrieve, test_data, size);
		dc_DestroyDC (dc);
        }
	Announce("------------------ test3 -----------------");
	{	/* ARM SOW example using explicit dimn defs with field ids */

		FieldId wnum_id, therm_id;
		FieldId mean_rad_id, sd_rad_id;
		dsDetail dsd;
		
		dc = dc_CreateDC(DCC_NSpace);
		dc->dc_Platform = plat_id;
		++when.zt_Sec;

		wnum_id = F_DeclareField("wnum","Wave Number","1 / cm");
		dc_NSDefineDimension(dc, wnum_id, 624);
		dc_NSDefineVariable(dc, wnum_id, 1, &wnum_id, 
				    /*is_static*/TRUE);
		
		mean_rad_id = F_DeclareField("mean_rad",
					"Mean of Radiance spectra ensemble",
					"mW / m^2 sr 1 / cm");
		dc_NSDefineVariable(dc, mean_rad_id, 1, &wnum_id, FALSE);
		
		sd_rad_id = F_DeclareField("standard_dev_rad",
			   "standard deviation for Radiance spectra ensemble",
			   "1/cm");
		dc_NSDefineVariable(dc, sd_rad_id, 1, &wnum_id, FALSE);
		therm_id = F_DeclareField("thermistor1",
					  "Long name for thermistor",
					  "units");
		dc_NSDefineVariable(dc, therm_id, 0, NULL, FALSE);
		
		dc_DumpDC (dc);

		T_NSGetAllDimensions(dc);
		T_NSGetAllVariables(dc);

		/* Test some storage */
		begin = end = when;
		dc_NSAddStatic (dc, wnum_id, test_data+50);
		EXPECT(2);
		dc_NSAddStatic (dc, mean_rad_id, test_data+50);
		dc_NSAddSample (dc, &begin, 0, wnum_id, test_data);
		EXPECT(0);
		dc_NSAddSample (dc, &begin, 0, therm_id, test_data);
		dc_NSAddSample (dc, &begin, 0, mean_rad_id, test_data);
		dc_NSAddSample (dc, &begin, 0, sd_rad_id, test_data);

		/* store it, then add more data, and store again later  */
		/* with newfile flag FALSE				*/
		dc_SetStaticLoc (dc, &loc);
		ds_Store (dc, TRUE, 0, 0);
		++end.zt_Sec;
		dc_NSAddSample (dc, &end, 1, therm_id, test_data+100);
		dc_NSAddSample (dc, &end, 1, mean_rad_id, test_data+100);
		dc_NSAddSample (dc, &end, 1, sd_rad_id, test_data+100);
		dc_DumpDC (dc);

		/* test some retrieval */
		EXPECT(0);
		(void)dc_NSGetSample (dc, 0, wnum_id, &size); /*should pass*/
		retrieve = dc_NSGetStatic (dc, wnum_id, &size);
		retrieve = dc_NSGetStatic (dc, wnum_id, NULL);
		printf ("dc_NSGetStatic(%s) returns size = %lu,", 
			F_GetName(wnum_id), size);
		T_CompareData (retrieve, test_data+50, size);

		retrieve = dc_NSGetSample (dc, 0, therm_id, &size);
		printf("GetSample(0,%s): size=%lu, data=%s\n",
		       F_GetName(therm_id),size,(retrieve)?"non-NULL":"NULL");
		retrieve = dc_NSGetSample (dc, 1, mean_rad_id, &size);
		retrieve = dc_NSGetSample (dc, 1, mean_rad_id, NULL);
		printf ("dc_NSGetSample(%s) returns size = %lu,", 
			F_GetName(mean_rad_id), size);
		T_CompareData (retrieve, test_data+100, size);

		EXPECT(1);
		retrieve = dc_NSGetSample (dc, 0, BadField, NULL);
		printf("GetSample(2,BadField): data=%s\n",
		       (retrieve)?"non-NULL":"NULL");

		ds_Store (dc, FALSE, 0, 0);
		fields = dc_GetFields (dc, &nfield);

		/* now try to fetch what we just stored and see what we get */
		printf("Fetching data (detail: badval=999 .... "); 
		fflush(stdout);
		dsd.dd_Name = "badval";
		dsd.dd_V.us_v_float = 999.0;
		ndc = ds_Fetch (plat_id, DCC_NSpace, &begin, &end,
				fields, nfield, &dsd, 1);
		dc_DestroyDC (dc);
		dc = ndc;
		printf("DataChunk returned by ds_Fetch():\n");
		dc_DumpDC (dc);

		/* and re-do the data comparisions -- should be identical */
		T_NSGetAllDimensions(dc);
		T_NSGetAllVariables(dc);

		/* re-test some retrieval */
		printf ("Comparing fetched data with stored data...\n");
		EXPECT(0);
		(void)dc_NSGetSample (dc, 0, wnum_id, &size); /*should pass*/
		retrieve = dc_NSGetStatic (dc, wnum_id, &size);
		retrieve = dc_NSGetStatic (dc, wnum_id, NULL);
		printf ("dc_NSGetStatic(%s) returns size = %lu,", 
			F_GetName(wnum_id), size);
		T_CompareData (retrieve, test_data+50, size);

		retrieve = dc_NSGetSample (dc, 0, therm_id, &size);
		printf("GetSample(0,%s): size=%lu, data=%s\n",
		       F_GetName(therm_id),size,(retrieve)?"non-NULL":"NULL");
		retrieve = dc_NSGetSample (dc, 1, mean_rad_id, &size);
		retrieve = dc_NSGetSample (dc, 1, mean_rad_id, NULL);
		printf ("dc_NSGetSample(%s) returns size = %lu,", 
			F_GetName(mean_rad_id), size);
		T_CompareData (retrieve, test_data+100, size);

		EXPECT(1);
		retrieve = dc_NSGetSample (dc, 0, BadField, NULL);
		printf("GetSample(2,BadField): data=%s\n",
		       (retrieve)?"non-NULL":"NULL");

		/* try some block storage */
		printf("Storing on platform t_nsblocks using blocks\n");
		dc->dc_Platform = ds_LookupPlatform ("t_nsblocks");
		ds_StoreBlocks (dc, TRUE, 0, 0);
		printf("Storing on platform t_nsscalar w/o using blocks\n");
		dc->dc_Platform = ds_LookupPlatform ("t_nsscalar");
		ds_StoreBlocks (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	Announce("------------------ test4 -------------------");
	{	/* ARM SOW example with implicit dimn defs */

		FieldId mean_rad_id, sd_rad_id, wnum_id, therm_id;
		char *dimname[1];
		unsigned long dimsize[1];
		
		dimname[0] = "wnum";	dimsize[0] = 24;
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = plat_id;
		
		wnum_id = F_DeclareField("wnum","Wave Number","1 / cm");
		dc_NSDefineField(dc, wnum_id, 1, dimname, dimsize, FALSE);
		
		mean_rad_id = F_DeclareField("mean_rad",
				     "Mean of Radiance spectra ensemble",
				     "mW / m^2 sr 1 / cm");
		dc_NSDefineField(dc, mean_rad_id, 1, 
				 dimname, dimsize, FALSE);
		
		sd_rad_id = F_DeclareField("standard_dev_rad",
			   "standard deviation for Radiance spectra ensemble",
			   "1/cm");
		dc_NSDefineField(dc, sd_rad_id, 1, dimname, dimsize, FALSE);
		
		therm_id = F_DeclareField("thermistor1",
					  "Long name for thermistor",
					  "units");
		dc_NSDefineVariable(dc, therm_id, 0, NULL, FALSE);
		
		dc_DumpDC (dc);

		T_NSGetField (dc, sd_rad_id);

#ifdef TEST4_STORE
		/* Add lots of data and try to push some limits */
		printf ("Adding 500 samples to datachunk... "); 
		fflush(stdout);
		when.zt_Sec += 60;
		begin = when;
		dc_SetStaticLoc (dc, &loc);
		for (i = 0; i < 500; ++i)
		{
			dc_NSAddSample (dc, &when, i, wnum_id, test_data+i);
			dc_NSAddSample (dc, &when, i, therm_id, test_data+i);
			dc_NSAddSample (dc, &when, i, mean_rad_id, 
					test_data+i);
			dc_NSAddSample (dc, &when, i, sd_rad_id, test_data+i);
			++when.zt_Sec;
		}
		end = when;
		--end.zt_Sec;
		printf ("Done.\nPutSample to 't_nspace' ... "); fflush(stdout);
		ds_Store (dc, TRUE, 0, 0);
		printf ("and PutBlock to plat 't_nsblocks' ... "); 
		fflush(stdout);
		dc->dc_Platform = ds_LookupPlatform ("t_nsblocks");
		ds_StoreBlocks (dc, TRUE, 0, 0);
		printf ("Done storing.\n");

		/* now try to fetch what we just stored and see what we get */
		/* remember to keep the dc around which is holds the fields */
		fields = dc_GetFields (dc, &nfield);
		printf("Fetching from '%s' ....   \n", 
		       ds_PlatformName(plat_id));
		ndc = ds_Fetch (plat_id, DCC_NSpace, &begin, &end,
				fields, nfield, NULL, 0);
		/* compare the data we retrieved */
		for (i = 0; i < 500; i+=100)
		{
			printf ("Sample %d: ", i);
			retrieve = dc_NSGetSample (ndc, i, fields[0], &size);
			T_CompareData (retrieve, test_data+i, size);
			retrieve = dc_NSGetSample (ndc, i, fields[2], &size);
			T_CompareData (retrieve, test_data+i, size);
		}
		dc_DestroyDC (ndc);
		printf("Done.\n");
		printf("Fetching from 't_nsblocks' ...\n");
		ndc = ds_Fetch (ds_LookupPlatform("t_nsblocks"), 
				DCC_NSpace, &begin, &end,
				fields, nfield, NULL, 0);
		/* compare the data we retrieved */
		for (i = 0; i < 500; i+=75)
		{
			printf ("Sample %d: ", i);
			retrieve = dc_NSGetSample (ndc, i, fields[0], &size);
			T_CompareData (retrieve, test_data+i, size);
			retrieve = dc_NSGetSample (ndc, i, fields[2], &size);
			T_CompareData (retrieve, test_data+i, size);
		}
		dc_DestroyDC (ndc);
#endif
		printf("Done.\n");
		dc_DestroyDC (dc);
	}
	Announce("------------------ test5 -----------------");
	{	/* silly stuff */

		FieldId fid, did;
		char *dimname[4];
		unsigned long dimsize[4];
		
		dimname[0] = "thisdimensionhasareallylongnamethatwillnotfit";
		dimname[1] = "thisoneisnotsolongjustlong283032";
		dimname[2] = "third";
		dimname[3] = "fourth";
		dimsize[0] = 1; dimsize[1] = 2; dimsize[2] = 4; dimsize[3] = 6;

		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = plat_id;
		
		fid = F_DeclareField("tests","Testing 1, 2, 3","#");
		did = F_DeclareField(dimname[1],"Dimension","u");

		/* define a variable whose dimensions do not exist */
		EXPECT(1);
		dc_NSDefineVariable (dc, fid, 1, &did, TRUE);

		/* try defining a dimension with a long name */
		EXPECT(2);  	/* 2 dimn names too long */
		dc_NSDefineField(dc, fid, 4, dimname, dimsize, FALSE);
		dc_DumpDC (dc);
		
		/* now try redefining a dimension */
		EXPECT(3); /* 1 too long, 1 for redefined, 1 for new id */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		dc_DumpDC (dc);

		/* redefine a variable */
		EXPECT(2); /* 1 to redefine field, 1 for change in dimns */
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);
		/* re-define 'tests' to be dynamic but suppress the warning */
		EXPECT(0);
		dc_NSAllowRedefine (dc, TRUE);
		dc_NSDefineVariable (dc, fid, 1, &did, FALSE);
		dc_DumpDC (dc);

		EXPECT(0);
		/* test completion */
		printf("dc_NSDefineIsComplete() returns %s\n",
		       dc_NSDefineIsComplete(dc) ? "True" : "False");

		/* force completion */
		dc_NSDefineComplete(dc);
		printf("%s, dc_NSDefineIsComplete() returns %s\n",
		       "After dc_NSDefineComplete()",
		       dc_NSDefineIsComplete(dc) ? "True" : "False");

		EXPECT(2);
		/* try more definition after completion */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);

		/* quick addition of data just to create a file */
		when.zt_Sec += 60;
		dc_SetStaticLoc (dc, &loc);
		dc_NSAddSample (dc, &when, 0, fid, test_data+2000);
		
		dc_DumpDC (dc);
		ds_StoreBlocks (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	Announce("------------------- test6 -------------------");
	{	/* push limits of number of dims and fields in a chunk */

		char name[ 10 ];
		char *namep = name;
		FieldId fid, did;
		unsigned long size;
		int i;

		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = ds_LookupPlatform("t_test6");

		/* test dimn limit */
		for (i = 0; i < DC_MaxDimension + 2; ++i)
		{
			if (i == DC_MaxDimension)
			{ EXPECT(2); }
			sprintf (name, "dimn%i", i);
			did = F_DeclareField(name, "Dimension", "none");
			size = i;
			dc_NSDefineDimension(dc, did, size);
		}

		/* test field limit */
		did = F_Lookup("dimn12");
		for (i = 0; i < DC_MaxField + 2; ++i)
		{
			if (i == DC_MaxField)
			{ EXPECT(2); }
			sprintf (name, "field%i", i);
			fid = F_DeclareField(name, "Field", "units");
			dc_NSDefineVariable(dc, fid, 1, &did, i % 2);
		}

		EXPECT(1);
		/* test field limit with DefineField */
		dc_NSDefineField(dc, fid, 0, 0, 0, 0);

		EXPECT(2); /* 1 for dimn limit, 1 for aborted field defn */
		/* test dimn limit with DefineField by redefining field */
		dc_NSDefineField(dc, F_Lookup("field1"), 1, 
				 &namep, &size, TRUE);

		/* dc_DumpDC (dc); */

		/* see what it looks like after closing definition */
		dc_NSDefineComplete (dc);
		/* dc_DumpDC (dc); */

		ds_Store (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	Announce("-------------------- test7 -----------------------");
	{

		FieldId field, sfield;
		FieldId fids[3];
		char *dim_names[2];
		unsigned long dim_sizes[2];

		dim_names[0] = "x";	dim_names[1] = "y";
		dim_sizes[0] = 50;	dim_sizes[1] = 25;
		fids[0] = F_DeclareField ("curl1","Long name","units");
		fids[1] = F_DeclareField ("curl2","Long name","units");
		fids[2] = F_DeclareField ("curl3","Long name","units");
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = plat_id;
		dc_NSDefineField (dc, fids[0], 2, dim_names, dim_sizes, FALSE);
		dc_NSDefineField (dc, fids[1], 2, dim_names, dim_sizes, FALSE);
		dc_NSDefineField (dc, fids[2], 2, dim_names, dim_sizes, FALSE);
		dc_NSDefineComplete (dc);
		dc_NSAddSample (dc, &when, 0, fids[0], test_data);
		printf ("dynamic fields of same sizes, should be UNIFORM:\n");
		dc_DumpDC (dc);
		dc_DestroyDC (dc);
	}

}
#endif /* NSPACE */




DataChunk *
T_ScalarNSpaceChunk (start, nsample, nfield, is_mobile, addatts)
ZebTime *start;
int nsample;
int nfield;
bool is_mobile;
bool addatts;
{
	DataChunk *dc;
	FieldId fids[50];
	float value;
	int fld;
	int i;
	ZebTime when;
	struct TestField *tf;
	static Location loc;

	loc.l_lat = 40.0; loc.l_lon = -160.0; loc.l_alt = 5280.0;
	if (nfield > 50)
		nfield = 50;
	if (nfield > NUM_TESTFIELDS)
		nfield = NUM_TESTFIELDS;
	dc = dc_CreateDC (DCC_NSpace);
	for (i = 0; i < nfield; ++i)
	{
		tf = TestFields+i;
		fids[i] = F_DeclareField (tf->name, tf->desc, tf->units);
		dc_NSDefineField (dc, fids[i], 0, 0, 0, FALSE);
	}
	dc_NSDefineComplete (dc);
	dc_SetBadval (dc, -999.0);
	if (!is_mobile)
		dc_SetStaticLoc (dc, &loc);

	if (addatts)
	{
		dc_SetGlobalAttr (dc, "zeb_platform", "t_scalar");
		dc_SetGlobalAttr (dc, "date", "today");
	}

	when = *start;
	value = 1.0;
	dc_HintNSamples (dc, nsample, TRUE);
	for (i = 0; i < nsample; ++i)
	{
		for (fld = 0; fld < nfield; ++fld, ++value)
			dc_NSAddSample (dc, &when, i, 
					fids[fld], &value); 
		if (is_mobile)
		{
			loc.l_lat = -90.0 + i*180.0/3000.0;
			loc.l_lon = -180.0 + i*360.0/3000.0;
			loc.l_alt = i;
			dc_SetLoc (dc, i, &loc);
		}
		++when.zt_Sec;
		value -= nfield;
		value += 1.0;
	}
	if (addatts)
	{
		dc_SetSampleAttr (dc, 0, "key", "first sample");
		dc_SetSampleAttr (dc, 3, "sample_number", "3");
		dc_SetSampleAttr (dc, i/2, "median", "middle");
		dc_SetSampleAttr (dc, i-1, "key", "last sample");
	}
	return (dc);
}




DataChunk *
T_SimpleScalarChunk (start, interval, nsample, nfield, is_mobile, addatts)
ZebTime *start;
int interval;
int nsample;
int nfield;
bool is_mobile;
bool addatts;
{
	DataChunk *dc;
	FieldId fids[50];
	float value;
	int fld;
	int i;
	ZebTime when;
	struct TestField *tf;
	static Location loc;

	loc.l_lat = 40.0; loc.l_lon = -160.0; loc.l_alt = 5280.0;
	if (nfield > 50)
		nfield = 50;
	if (nfield > NUM_TESTFIELDS)
		nfield = NUM_TESTFIELDS;
	dc = dc_CreateDC (DCC_Scalar);
	for (i = 0; i < nfield; ++i)
	{
		tf = TestFields+i;
		fids[i] = F_DeclareField (tf->name, tf->desc, tf->units);
	}
	dc_SetScalarFields (dc, nfield, fids);
	dc_SetBadval (dc, -999.0);
	if (!is_mobile)
		dc_SetStaticLoc (dc, &loc);

	if (addatts)
	{
		dc_SetGlobalAttr (dc, "zeb_platform", "t_scalar");
		dc_SetGlobalAttr (dc, "date", "today");
	}

	when = *start;
	value = 1.0;
	dc_HintNSamples (dc, nsample, TRUE);
	for (i = 0; i < nsample; ++i)
	{
		for (fld = 0; fld < nfield; ++fld, ++value)
			dc_AddScalar (dc, &when, i, 
				      fids[fld], &value); 
		/* {
			char c[2];
			c[0] = (i % 10) + '0'; c[1] = '\0';
			dc_SetSampleAttr (dc, i, "ones", c);
		} */
		if (is_mobile)
		{
			loc.l_lat = -90.0 + i*180.0/3000.0;
			loc.l_lon = -180.0 + i*360.0/3000.0;
			loc.l_alt = i;
			dc_SetLoc (dc, i, &loc);
		}
		when.zt_Sec += interval;
		value -= nfield;
		value += 1.0;
	}
	if (addatts)
	{
		dc_SetSampleAttr (dc, 0, "key", "first sample");
		dc_SetSampleAttr (dc, 3, "sample_number", "3");
		dc_SetSampleAttr (dc, i/2, "median", "middle");
		dc_SetSampleAttr (dc, i-1, "key", "last sample");
	}
	return (dc);
}



T_TransparentAdd (dc, start, nsample, is_mobile, addatts)
DataChunk *dc;
ZebTime *start;
int nsample;
bool is_mobile;		/* Set locations 		*/
bool addatts;		/* per-sample atts only 	*/
/*
 * Just adds variable length text as opaque samples of varying sizes.
 * Tests the hint functions.
 */
{
	char *text[] = {
"burghart        - Died on level   8. Started on level   1.  Score:     7532.",
"Died on level  17",
"You are quite disappointing:",
"Started on level   1.  Score:     9542.",
" burghart        - Died on level   9. Started on level   1.  Score:     9542.
 burghart        - Died on level  18. Started on level  12.  Score:     8583.
 burghart        - Died on level   8. Started on level   1.  Score:     8420.
 burghart        - Died on level  17. Started on level  12.  Score:     8153.
 burghart        - Died on level  10. Started on level   1.  Score:     7905.
 burghart        - Died on level   8. Started on level   1.  Score:     7800.",
"You are quite disappointing: *granger" };
	int ntext = sizeof(text)/sizeof(text[0]);
	int i, len;
	char *data, *src;
	ZebTime when;
	static Location loc = { 1.0, 2.0, 4.0 };

	when = *start;
	for (i = 0; i < nsample; ++i)
	{
		src = text[i%ntext];
		dc_AddSample(dc, &when, src, strlen(src)+1);
		if (is_mobile)
			dc_SetLoc (dc, i, &loc);
		if (addatts)
			dc_SetSampleAttr(dc, i, "key", "value");
		++when.zt_Sec;
		data = dc_GetSample (dc, i, &len);
		assert(len == strlen(src)+1);
		assert(!strcmp(data, src));
	}
}




#ifdef NSPACE_AERI
T_Aeri()
{
	ZebTime begin, when;
	char *pname = "sgpaerich1C1.a1";
	PlatformId pid = ds_LookupPlatform(pname);
	FieldId fields[30];
	float *retrieve;
	DataChunk *dc;
	int n, i;
	unsigned long len;

	Announce("----------- aeri -----------");
	n = 0;
	fields[n] = F_Lookup("lat"); ++n;
	fields[n] = F_Lookup("lon"); ++n;
	fields[n] = F_Lookup("alt"); ++n;
	fields[n] = F_Lookup("hotBBTemp"); ++n;
	fields[n] = F_Lookup("reflectedTemp"); ++n;
	fields[n] = F_Lookup("thermistor0"); ++n;
	fields[n] = F_Lookup("thermistor1"); ++n;
	fields[n] = F_Lookup("thermistor5"); ++n;
	fields[n] = F_Lookup("thermistor13"); ++n;
	fields[n] = F_Lookup("pressure"); ++n;
	fields[n] = F_Lookup("dataType"); ++n;
	fields[n] = F_Lookup("wnum"); ++n;
	fields[n] = F_Lookup("mean_rad"); ++n;
	fields[n] = F_Lookup("wnum2"); ++n;
	fields[n] = F_Lookup("standard_dev_mean_rad"); ++n;

	/* find out the time of the data */
	tl_Time (&begin);
	ds_GetObsTimes (pid, &begin, &when, 1, NULL);

	/* fetch a DC from the aeri platform and dump its data */
	printf("Fetching %s data....   ", pname); fflush(stdout);
	dc = ds_FetchObs (pid, DCC_NSpace, &when,
			  fields, n, NULL, 0);
	printf("DataChunk returned by ds_Fetch():\n");
	if (! dc)
	{
		printf ("NONE!\n");
		msg_ELog (EF_PROBLEM, "Fetch of '%s' data failed.", pname);
		return (1);
	}
	dc_DumpDC (dc);

	for (i = 0; i < n; ++i)
		T_NSGetField(dc, fields[i]);
		
	/* retrieve data and dump it out */
	retrieve = dc_NSGetStatic (dc, F_Lookup("wnum"), &len);
	T_DumpData (retrieve, 12, len, "wnum");

	retrieve = dc_NSGetSample (dc, 0, F_Lookup("mean_rad"), &len);
	T_DumpData (retrieve, 12, len, "mean_rad");

	retrieve = dc_NSGetStatic (dc, F_Lookup("wnum2"), &len);
	T_DumpData (retrieve, 12, len, "wnum2");

	retrieve = dc_NSGetStatic (dc, F_Lookup("lat"), &len);
	printf ("Lat = %.2f, ", *retrieve);
	retrieve = dc_NSGetStatic (dc, F_Lookup("lon"), &len);
	printf ("Lon = %.2f, ", *retrieve);
	retrieve = dc_NSGetStatic (dc, F_Lookup("alt"), &len);
	printf ("Alt = %.2f\n", *retrieve);

	retrieve = dc_NSGetSample (dc, 0, 
				   F_Lookup("thermistor0"), &len);
	T_DumpData (retrieve, 5, len, "thermistor0");
	dc_DestroyDC(dc);
	return (0);
}
#endif /* NSPACE_AERI */


T_GetFields(start, plat)
ZebTime *start;
char *plat;
{
	FieldId fields[10];
	FieldId *dc_fields;
	DataChunk *dc;
	int dc_nfield;
	int nfield, i, j;
	char buf[128];

	/*
	 * Quick test of ds_GetFields()
	 */
	sprintf (buf, "testing ds_GetFields() for platform '%s'", plat);
	Announce (buf);
	dc = T_SimpleScalarChunk (start, 1, 10, 4, FALSE, FALSE);
	dc->dc_Platform = ds_LookupPlatform(plat);
	dc_fields = dc_GetFields (dc, &dc_nfield);
	ds_Store (dc, TRUE, NULL, 0);
	nfield = 10;
	if (!ds_GetFields(dc->dc_Platform, start, &nfield, fields))
	{
		IngestLog (EF_PROBLEM, 
			   "ds_GetFields('%s',nfld=10) failed", plat);
	}
	else if (nfield != dc_nfield)
	{
		IngestLog (EF_PROBLEM, 
		  "ds_GetFields('%s',nfld=10) returned %d fields, expected %d",
		  plat, nfield, dc_nfield);
	}
	else
	{
		for (i = 0; i < nfield; ++i)
		{
			for (j = 0; j < nfield; ++j)
				if (fields[i] == dc_fields[j])
					break;
			if (j >= nfield)
				break;
		}
		if (i < nfield)
			IngestLog (EF_PROBLEM,
				   "ds_GetFields(): field %d incorrect", 
				   fields[i]);
	}
	/*
	 * Now try asking for fewer fields than are in the file, and make
	 * sure we aren't given more than that.
	 */
	nfield = 2;
	if (!ds_GetFields(dc->dc_Platform, start, &nfield, fields))
	{
		IngestLog (EF_PROBLEM, 
			   "ds_GetFields('%s',nfld=2) failed", plat);
	}
	else if (nfield != 2)
	{
		IngestLog (EF_PROBLEM, 
		  "ds_GetFields('%s',nfld=2) returned %d fields, expected %d",
		  plat, nfield, 2);
	}
	dc_DestroyDC(dc);
}



T_Transparent (platform, now)
char *platform;
ZebTime *now;
{
	DataChunk *dc;
	char *data = "Transparent chunk holding text and newline\n";
	char buf[128];
	ZebTime when = *now;
	bool atts = FALSE;
	static Location loc = { 40.0, -160.0, 5280.0 };

	sprintf(buf,"storing transparent datachunk to platform '%s'",platform);
	Announce (buf);
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = ds_LookupPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	dc_AddSample (dc, &when, data, strlen(data)+1);
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC(dc);

	/* Now get a much larger one and overwrite the first sample above */
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = ds_LookupPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	T_TransparentAdd (dc, &when, 100, TRUE, atts);
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	ds_Store (dc, TRUE, NULL, 0);
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC(dc);

	/* Now try it with some hints set */
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = ds_LookupPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	dc_HintNSamples (dc, 25, FALSE);
	T_TransparentAdd (dc, &when, 50, TRUE, atts);
	when.zt_Sec += 25;
	dc_HintNSamples (dc, 10, TRUE);
	T_TransparentAdd (dc, &when, 10, TRUE, atts);
	when.zt_Sec += 10;
	dc_HintSampleSize (dc, 50, FALSE);
	dc_HintMoreSamples (dc, 50, FALSE);
	T_TransparentAdd (dc, &when, 50, TRUE, atts);
	when.zt_Sec += 50;
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
}



T_Deletes (start)
ZebTime *start;
/*
 * Here's an idea: use a platform whose maxsamples=1 to
 * create a bunch of observations, each of which contains
 * only one sample.  Then test out dsdelete and ds_DeleteObs
 * on that platform.
 */
{
	PlatformId t_delete_id;
	DataChunk *dc;
	ZebTime when, begin = *start;
	int i;
	char dsdump[256];
	char cmd[256];

	Announce ("Testing deletion: creating 10 files in t_deletes");
	dc = T_SimpleScalarChunk (&begin, 1, 10, 4, TRUE, TRUE);
	t_delete_id = ds_LookupPlatform("t_deletes");
	dc->dc_Platform = t_delete_id;
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	if (getenv("ZEB_TOPDIR"))
		sprintf (dsdump, "%s/bin/dsdump", getenv("ZEB_TOPDIR"));
	else
		strcpy (dsdump, "dsdump");
	sprintf (cmd, "%s t_deletes", dsdump);
	system(cmd);
	fflush (stdout);
	fflush (stderr);

	Announce ("Deleting even-second obs with DeleteObs");
	when = begin;
	for (i = 0; i < 10; ++i)
	{
		if (when.zt_Sec % 2 == 0)
			ds_DeleteObs (t_delete_id, &when);
		++when.zt_Sec;
	}
	Announce ("Finished deleting with DeleteObs");
	fflush (stdout);
	fflush (stderr);
	system(cmd);	
	fflush (stdout);
	fflush (stderr);
	Announce ("Trying to do the same deletes again");
	when = begin;
	for (i = 0; i < 10; ++i)
	{
		if (when.zt_Sec % 2 == 0)
			ds_DeleteObs (t_delete_id, &when);
		++when.zt_Sec;
	}
	Announce ("Storing the DataChunk again...");
	ds_StoreBlocks (dc, TRUE, NULL, 0);
	Announce ("Test extremes: time earlier than all obs,");
	when.zt_Sec = begin.zt_Sec - 3600*24;
	ds_DeleteObs (t_delete_id, &when);
	Announce ("Time later than all obs,");
	when.zt_Sec = begin.zt_Sec + 3600*12;
	ds_DeleteObs (t_delete_id, &when);
	Announce ("Using DeleteObs to delete all files...");
	when = begin;
	for (i = 0; i < 10; ++i)
	{
		ds_DeleteObs (t_delete_id, &when);
		++when.zt_Sec;
	}
	fflush (stdout);
	fflush (stderr);
	system(cmd);
	fflush (stdout);
	fflush (stderr);
	printf("Done deletes test.  Should be 0 files left in 't_deletes'.\n");
	dc_DestroyDC (dc);
}



T_CompareData(src1, src2, size)
float *src1, *src2;
int size;
{
	int i;

	printf ("comparing data... ");
	fflush (stdout);
	for (i = 0; i < size; ++i)
		if (src1[i] != src2[i]) break;
	printf ("  %s at %i\n",
		(i < size) ? "failed" : "succeeded", i);
	
}



T_DumpData (retrieve, n, len, fname) 
float *retrieve;
int n;
int len;
char *fname;
{
	int i, j;

	printf ("First %d '%s' values (len = %d):\n", n, fname, len);
	for (i = 0; (i < n) && (i < len); ++i)
		printf ("%9.2f%c", retrieve[i], (i+1)%7?' ':'\n');
	printf ("\nLast %d '%s' values (len = %d):\n", n, fname, len);
	for (i = 0, j = len-n+((n<=len)?0:(n-len));
	     (i < n) && (j < len); ++i, ++j)
		printf ("%9.2f%c", retrieve[j], (i+1)%7?' ':'\n');
	printf ("\n");
}



T_NSGetField(dc, field)
DataChunk *dc;
FieldId field;
{
	int i;
	char *names[ DC_MaxDimension ];
	unsigned long sizes[ DC_MaxDimension ];
	int ndim, nvar, rndim, rstatic;
	
	/* test dc_NSGetField */
	(void)dc_NSGetField(dc, field, &rndim, names, sizes,
			    &rstatic);
	printf("dc_NSGetField(%s:id %i): %i dims, %s static, ( ",
	       F_GetName(field), field, rndim,
	       (rstatic) ? "is" : "not");
	for (i = 0; i < rndim; ++i)
		printf("%s=%lu ",names[i],sizes[i]);
	printf(")\n");
}




T_NSGetAllDimensions(dc)
DataChunk *dc;
{
	int i;
	char *names[ DC_MaxDimension ];
	unsigned long sizes[ DC_MaxDimension ];
	FieldId *fields;
	int nfield;
	FieldId ids[ DC_MaxDimension + DC_MaxField ];
	int ndims[ DC_MaxDimension ];
	int ndim, nvar, rndim, rstatic;
	char *name;
	unsigned long size;

	ndim = dc_NSGetAllDimensions(dc, names, ids, sizes);
	printf ("dc_NSGetAllDimensions() returns %i dimensions:\n", 
		ndim);
	for (i = 0; i < ndim; ++i)
	{
		printf("   %-30s %-5i %10lu\n",names[i],ids[i],sizes[i]);
		dc_NSGetDimension (dc, ids[i], &name, &size);
		printf("   NSGetDimension(%s): id %i, size = %lu\n",
		       name, ids[i], size);
	}
}



T_NSGetAllVariables(dc)
DataChunk *dc;
{
	int i,j;
	unsigned long sizes[ DC_MaxDimension ];
	FieldId ids[ DC_MaxDimension + DC_MaxField ];
	FieldId rids[ DC_MaxDimension + DC_MaxField ];
	int ndims[ DC_MaxDimension ];
	int ndim, nvar, rndim, rstatic;
	char *name;

	nvar = dc_NSGetAllVariables(dc, ids, ndims);
	printf ("dc_NSGetAllVariables() returns %i variables:\n", 
		nvar);
	for (i = 0; i < nvar; ++i)
	{
		printf("   id: %-3i   ndims: %-5i", ids[i], ndims[i]);
		printf("   NSIsStatic(): %s\n",
		       (dc_NSIsStatic(dc, ids[i])) ? "True" : "False");
		dc_NSGetVariable(dc, ids[i], &rndim, rids, &rstatic);
		printf("   dc_NSGetVariable(%s: id %i): %i dims, %s static, ",
		       F_GetName(ids[i]), ids[i], rndim, (rstatic)?"is":"not");
		printf("  ( ");
		for (j = 0; j < rndim; ++j)
		{
			printf(" %s:id %i, ",
			       (rids[j] != BadField)?(F_GetName(rids[j])):
			       (""), rids[j]);
		}
		printf(")\n");
	}
}



#ifdef GRID_BLOCKS
T_1DGridStoreBlocks()
{
	DataChunk *dc;
	PlatformId src_id, dest_id;
	ZebTime when;
	int nfield, i;
	FieldId fields[5];

	/*
	 * All we want to do is fetch a datachunk from some known
	 * 1DGrid platform and store the same datachunk using StoreBlocks
	 * in the 't_1dgrid' platform.
	 */
	src_id = ds_LookupPlatform ("kapinga/prof915h");
	fields[0] = F_Lookup("height");
	fields[1] = F_Lookup("wspd");
	fields[2] = F_Lookup("wdir");
	fields[3] = F_Lookup("u_wind");
	fields[4] = F_Lookup("v_wind");
	nfield = 5;
	TC_ZtAssemble (&when, 92, 12, 2, 18, 47, 41, 0);
	printf("Fetching observation from kapinga/prof915h... ");
	fflush(stdout);
	dc = ds_FetchObs (src_id, DCC_RGrid, &when, fields, nfield, 0, 0);
	if (!dc)
	{
		printf ("kapinga/prof915h 1d observation not found.\n");
		return (1);
	}
	printf("Done\n");
	printf("PutBlock to 't_1dgrid_cdf' and 't_1dgrid_znf'... "); 
	fflush(stdout);
	dest_id = ds_LookupPlatform ("t_1dgrid_cdf");
	dc->dc_Platform = dest_id;
	ds_StoreBlocks (dc, TRUE, 0, 0);
	dest_id = ds_LookupPlatform ("t_1dgrid_znf");
	dc->dc_Platform = dest_id;
	ds_StoreBlocks (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	printf("Done.\n");
}



T_IRGridStoreBlocks()
{
	DataChunk *dc;
	PlatformId src_id, dest_id;
	ZebTime begin, end;
	int nfield;
	FieldId fields[5];

	/*
	 * Fetch some IRGrid data and store it using StoreBlocks
	 */
	src_id = ds_LookupPlatform ("t_mesonet");
	fields[0] = F_Lookup("pres");
	fields[1] = F_Lookup("cpres0");
	fields[2] = F_Lookup("tdry");
	fields[3] = F_Lookup("dp");
	nfield = 4;
	TC_ZtAssemble (&begin, 92, 3, 10, 0, 0, 0, 0);
	end = begin;
	end.zt_Sec += 3600*24;
	printf("Fetching one day from 't_mesonet'... ");
	fflush(stdout);
	dc = ds_Fetch (src_id, DCC_IRGrid, &begin, &end, 
		       fields, nfield, 0, 0);
	if (!dc)
	{
		printf ("t_mesonet irgrid observation not found.\n");
		return (1);
	}
	printf("Done\n");
	printf("PutBlock to 't_irgrid_cdf' and 't_irgrid_znf'... "); 
	fflush(stdout);
	dest_id = ds_LookupPlatform ("t_irgrid_cdf");
	dc->dc_Platform = dest_id;
	ds_StoreBlocks (dc, TRUE, 0, 0);
	dest_id = ds_LookupPlatform ("t_irgrid_znf");
	dc->dc_Platform = dest_id;
	ds_StoreBlocks (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	printf("Done.\n");
}
#endif



T_RGrid (begin)
ZebTime begin;
{
	DataChunk *dc;
	FieldId fids[50];
	int fld;
	int i;
	ZebTime when;
	struct TestField *tf;
	static Location loc;
	int nsample = 5;
	int nfield = 3;
	RGrid rgrid;

	loc.l_lat = 40.0; loc.l_lon = -160.0; loc.l_alt = 5280.0;
	dc = dc_CreateDC (DCC_RGrid);
	for (i = 0; i < nfield; ++i)
	{
		tf = TestFields+i;
		fids[i] = F_DeclareField (tf->name, tf->desc, tf->units);
	}
	dc_RGSetup (dc, nfield, fids);
	dc_SetBadval (dc, -999.0);
	dc_SetStaticLoc (dc, &loc);

	dc_SetGlobalAttr (dc, "zeb_platform", "t_scalar");
	dc_SetGlobalAttr (dc, "date", "today");

	when = begin;
	dc_HintNSamples (dc, nsample, TRUE);
	rgrid.rg_Xspacing = 0.5;
	rgrid.rg_Yspacing = 0.5;
	rgrid.rg_Zspacing = 0.5;
	rgrid.rg_nX = 20;
	rgrid.rg_nY = 1;
	rgrid.rg_nZ = 10;
	for (i = 0; i < nsample; ++i)
	{
		for (fld = 0; fld < nfield; ++fld)
			dc_RGAddGrid (dc, i, fids[fld], &loc, &rgrid,
				      &when, (void *)(test_data + i*fld), 0);
	}
	dc_SetSampleAttr (dc, 0, "key", "first sample");
	dc_SetSampleAttr (dc, 3, "sample_number", "3");
	dc_SetSampleAttr (dc, i/2, "median", "middle");
	dc_SetSampleAttr (dc, i-1, "key", "last sample");
	dc_DumpDC (dc);
	dc_DestroyDC (dc);
}




#ifdef ZNF_TESTING
T_ZnfBlocks ()
{
	/* Call znf free block testing routine */
	Announce ("Testing ZNF low-level free blocks, creating file 'test.znf'");
	zn_TestFreeBlocks ("test.znf");
	fflush (stdout);
	fflush (stderr);
}
#endif



#ifdef AERI_TYPES
T_AeriTypes(when)
ZebTime when;
/*
 * Try a typical NSpace chunk, but include use of field types.
 */
{
#define N_WNUM 65
#define N_SAMPLE 16
	DataChunk *dc, *ndc;
	PlatformId plat_id;
	ZebTime begin = when;
	int i;
	FieldId fields[10];
	int nfield = 10;
	FieldId bin_avg_id, wnum_id, mean_rad_id, therm_id, flags_id;
	static const unsigned char Check_bits[] = {
		0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
		0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
	};
	char *bitmap_names[] = { "row", "col8" };
	unsigned long bitmap_sizes[] = { 8, 2 };
	FieldId bitmap_id;
	char *text_dim[] = { "text" };
	unsigned long text_size[] = { 256 };
	FieldId obs_id;
	char obs[256];
	char *process_dims[] = { "bin", "name" };
	unsigned long process_sizes[] = { 7, 32 };
	FieldId process_id;
	static char process_names[7][32] = {
		"process one", "process two", "process three",
		"process four", "process five", "process six", "process seven"
	};
	double *mean_rads;
	static float bin_averages[] = { 0.0, 1.0, 2.0, 4.0, 2.0, 1.0, 0.0 };
	static char obs_types[5][256] = {
"overcast; light precipitation visible to the east",
"the sun is out",
"the moon is looking a brilliant yellow this evening, and it\
makes the beauty of your eyes glow with irresistable radiance",
"'twas brillig in the frothy toves...",
"Oregon: 50 million gallons of water and no place to go on a Saturday" };
	static Location loc = { 40.0, -160.0, 5280.0 };

	Announce ("------ Testing a typed AERI NSpace DataChunk ------");
	dc = dc_CreateDC (DCC_NSpace);
	plat_id = ds_LookupPlatform ("t_aeri_types_cdf");
	dc->dc_Platform = plat_id;
	dc_SetStaticLoc (dc, &loc);

	/*
	 * A bitmap to represent this platform
	 */
	bitmap_id = F_DeclareField("bitmap", "Bitmap icon for this platform",
				   "none");
	dc_NSDefineField (dc, bitmap_id, 2, bitmap_names, bitmap_sizes, TRUE);

	/*
	 * Text: on-site observations
	 */
	obs_id = F_DeclareField("observation", "Field observations", "text");
	dc_NSDefineField (dc, obs_id, 1, text_dim, text_size, FALSE);

	/*
	 * Example AERI data
	 */
	wnum_id = F_DeclareField("wnum", "Wave Number", "cm-1");
	dc_NSDefineDimension (dc, wnum_id, N_WNUM);
	dc_NSDefineVariable (dc, wnum_id, 1, &wnum_id, TRUE);
	mean_rad_id = F_DeclareField ("mean_rad", 
		      "Mean of radiance spectra ensemble", "mW/(m2 sr cm-1)");
	dc_NSDefineVariable (dc, mean_rad_id, 1, &wnum_id, FALSE);
	therm_id = F_DeclareField ("thermistor", "Thermistor", "C");
	dc_NSDefineField (dc, therm_id, 0, 0, 0, FALSE);
	process_id = F_DeclareField ("process", "Active process", "none");
	dc_NSDefineField(dc, process_id, 2, process_dims, process_sizes, TRUE);
	bin_avg_id = F_DeclareField ("bin_avg_rad", "Bin average radiance",
				     "mW/(m2 sr cm-1)");
	dc_NSDefineField (dc,bin_avg_id,1,process_dims,process_sizes,FALSE);

	/*
	 * Store error flag masks for each sample time
	 */
	flags_id = F_DeclareField ("flags", "Error flags mask", "none");
	dc_NSDefineField (dc, flags_id, 0, 0, 0, FALSE);

	/*
	 * Close out definition and set the non-float field types
	 */
	dc_NSDefineComplete (dc);
	dc_SetBadval (dc, 9999.0);
	dc_SetType (dc, bitmap_id, DCT_UnsignedChar);
	dc_SetType (dc, obs_id, DCT_Char);
	dc_SetType (dc, flags_id, DCT_UnsignedShort);
	dc_SetType (dc, mean_rad_id, DCT_Double);
	dc_SetType (dc, process_id, DCT_Char);
	dc_DumpDC (dc);

	/*
	 * Definition is complete.  Add the static data.
	 */
	dc_NSAddStatic (dc, bitmap_id, (void *)Check_bits);
	dc_NSAddStatic (dc, wnum_id, (void *)test_data);
	dc_NSAddStatic (dc, process_id, (void *)process_names);

	/*
	 * Add the dynamic data
	 */
	mean_rads = (double *)malloc(N_WNUM*sizeof(double));
	for (i = 0; i < N_WNUM; ++i)
		mean_rads[i] = (double)i/1000.0;
	dc_HintNSamples (dc, N_SAMPLE, TRUE);
	Announce ("Adding sample data...");
	for (i = 0; i < N_SAMPLE; ++i)
	{
		float therm = (float)i;
		short flag = (short)i%50;

		dc_NSAddSample(dc, &when, i, obs_id, (void *)(obs_types[i%5]));
		dc_NSAddSample(dc, &when, i, bin_avg_id, (void *)bin_averages);
		dc_NSAddSample(dc, &when, i, therm_id, (void *)&therm);
		dc_NSAddSample(dc, &when, i, flags_id, (void *)&flag);
		dc_NSAddSample(dc, &when, i, mean_rad_id, (void *)mean_rads);
		++when.zt_Sec;
	}

	/*
	 * And finally store and destroy
	 */
	dc_DumpDC (dc);
	Announce ("Storing and destroying.");
	ds_StoreBlocks (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	Announce ("Fetching...");
	ds_GetFields (plat_id, &when, &nfield, fields);
	dc = ds_FetchObs (plat_id, DCC_NSpace, &begin, fields, nfield, 0, 0);
	if (! dc)
		Announce ("AERI types observation could not be fetched back");
	else
	{
		dc_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	Announce ("AERI typed NSpace test done.");
	free (mean_rads);
}
#endif /* AERI_TYPES */


#ifdef ATTRIBUTES

struct AttrDesc {
	enum { Global, Field, Sample } which;
	DataChunk *dc;
	int sample;
	FieldId field;
};



T_RemoveAttr (key, value, nval, type, arg)
char *key;
void *value;
int nval;
DC_ElemType type;
void *arg;
{
	struct AttrDesc *ad = (struct AttrDesc *)arg;

	switch (ad->which)
	{
	   case Global:
		dc_RemoveGlobalAttr (ad->dc, key);
		break;
	   case Field:
		dc_RemoveFieldAttr (ad->dc, ad->field, key);
		break;
	   case Sample:
		dc_RemoveSampleAttr (ad->dc, ad->sample, key);
		break;
	}
	return (0);
}




T_Attributes(when)
ZebTime when;
/*
 * Create a simple DataChunk and add/remove/dump its attributes
 */
{
	DataChunk *dc;
	FieldId field;
	char *pname = "t_att_types_cdf";
	PlatformId plat = ds_LookupPlatform (pname);
	static Location loc = { 40.0, -160.0, 5280.0 };
	char key[30];
	char value[50];
	float data = 1.0;
	char **keys;
	void **values;
	int natts, nval, i;
	DC_ElemType type;
#	define NUM(ra) (sizeof(ra)/(sizeof((ra)[0])))
	double ddata[] = { 0.0, 1.0, 2.0, 4.0, 8.0 };
	double *dget;
	float fdata[] = { -1.2, -3.4, -5.6, -6.7, -8.9, -10.0 };
	float *fget;
	unsigned char bytes[] = { 1, 3, 7, 15, 31, 63, 127, 255 };
	unsigned char *uget;
	short sdata[] = { 512, 1024, 2048, 4096, 8192, 16384, 32767 };
	short *sget;
	char *cdata = "array of characters";
	char *cptr;
	struct AttrDesc ad;
	int nfield = 10;
	FieldId fields[10];
	char *dash = "=>=>=>=>=>=>";

	Announce ("------- testing DataChunk attributes ------------");
	dc = dc_CreateDC(DCC_Scalar);
	dc->dc_Platform = plat;
	dc_SetStaticLoc (dc, &loc);
	dc_SetGlobalAttr (dc, "global_key", "global_value");
	dc_SetGlobalAttrArray (dc, "global_doubles", DCT_Double, 
			       NUM(ddata), (void *)ddata);
	dc_SetGlobalAttrArray (dc, "global_floats", DCT_Float, 
			       NUM(fdata), (void *)fdata);
	dc_SetGlobalAttrArray (dc, "global_bytes", DCT_UnsignedChar, 
			       NUM(bytes), (void *)bytes);
	dc_SetGlobalAttrArray (dc, "global_shorts", DCT_ShortInt, 
			       NUM(sdata), (void *)sdata);
	dc_SetGlobalAttrArray (dc, "global_string", DCT_String,
			       1, "this is a global attribute string");
	dc_SetGlobalAttrArray (dc, "global_char", DCT_Char,
			       strlen(cdata), cdata); /* no null char */
	EXPECT(1);
	dc_SetGlobalAttrArray (dc, "global_string", DCT_String,
			       2, "this is a global attribute string");
	/*
	 * Get the typed arrays added above
	 */
	printf ("%s Double attribute: ", dash);
	dget = (double *)dc_GetGlobalAttrArray (dc, "global_doubles", 
						&type, &nval);
	for (i = 0; i < nval; ++i) printf (" %lf ", dget[i]);
	printf ("\n");
	printf ("%s Short attribute: ", dash);
	sget = (short *)dc_GetGlobalAttrArray (dc, "global_shorts", 
					       &type, &nval);
	for (i = 0; i < nval; ++i) printf (" %hd ", sget[i]);
	printf ("\n");
	field = F_Lookup ("temp");
	dc_SetScalarFields (dc, 1, &field);
	dc_AddScalar (dc, &when, 0, field, &data);
	dc_SetFieldAttr (dc, field, "field_key", "field_value");
	dc_SetSampleAttr (dc, 0, "sample_key", "sample_value");
#ifndef EXAMPLE_ONLY
	printf ("%s dc should include non-string attributes:\n", dash);
	dc_DumpDC (dc);
	/*
	 * Now really crank out some attributes
	 */
	for (i = 0; i < 50; ++i)
	{
		sprintf (key, "key_%d", i);
		sprintf (value, "value_%d", i);
		dc_SetGlobalAttr (dc, key, value);
		if (strcmp(dc_GetGlobalAttr(dc, key), value))
			printf ("Global att error: '%s' should equal '%s'\n", 
				key, value);
		dc_SetFieldAttr (dc, field, key, value);
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
			printf ("Field att error: '%s' should equal '%s'\n", 
				key, value);
		sprintf (key, "sample_key_%d", i);
		dc_SetSampleAttr (dc, 0, key, value);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
			printf ("Sample att error: '%s' should equal '%s'\n", 
				key, value);
	}
#endif /* ! EXAMPLE_ONLY */
	/*
	 * Add some more typed attributes
	 */
	dc_SetFieldAttrArray (dc, field, "field_doubles", DCT_Double, 
			       NUM(ddata), (void *)ddata);
	dc_SetFieldAttrArray (dc, field, "field_floats", DCT_Float, 
			       NUM(fdata), (void *)fdata);
	dc_SetFieldAttrArray (dc, field, "field_bytes", DCT_UnsignedChar, 
			       NUM(bytes), (void *)bytes);
	dc_SetFieldAttrArray (dc, field, "field_shorts", DCT_ShortInt, 
			       NUM(sdata), (void *)sdata);
	dc_SetFieldAttrArray (dc, field, "field_string", DCT_String,
			       1, "this is a field attribute string");
	dc_SetFieldAttrArray (dc, field, "field_char", DCT_Char,
			       strlen(cdata), cdata);
	dc_SetSampleAttrArray (dc, 0, "sample_doubles", DCT_Double, 
			       NUM(ddata), (void *)ddata);
	dc_SetSampleAttrArray (dc, 0, "sample_floats", DCT_Float, 
			       NUM(fdata), (void *)fdata);
	dc_SetSampleAttrArray (dc, 0, "sample_bytes", DCT_UnsignedChar, 
			       NUM(bytes), (void *)bytes);
	dc_SetSampleAttrArray (dc, 0, "sample_shorts", DCT_ShortInt, 
			       NUM(sdata), (void *)sdata);
	dc_SetSampleAttrArray (dc, 0, "sample_string", DCT_String,
			       1, "this is a sample attribute string");
	dc_DumpDC (dc);
	/*
	 * Store this datachunk and see what we get in the file
	 */
	printf ("%s Storing datachunk to platform '%s'\n", dash, pname);
	ds_Store (dc, TRUE, 0, 0);
#ifndef EXAMPLE_ONLY
	/*
	 * Now try doing all of them again, which will cause them all to be
	 * removed and then re-added.
	 */
	for (i = 0; i < 50; ++i)
	{
		sprintf (key, "key_%d", i);
		sprintf (value, "value_%d", i);
		dc_SetGlobalAttr (dc, key, value);
		if (strcmp(dc_GetGlobalAttr(dc, key), value))
			printf ("Global att error: '%s' should equal '%s'\n", 
				key, value);
		dc_SetFieldAttr (dc, field, key, value);
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
			printf ("Field att error: '%s' should equal '%s'\n", 
				key, value);
		sprintf (key, "sample_key_%d", i);
		dc_SetSampleAttr (dc, 0, key, value);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
			printf ("Sample att error: '%s' should equal '%s'\n", 
				key, value);
	}
	/*
	 * Now just verify all at once
	 */
	for (i = 0; i < 50; ++i)
	{
		sprintf (key, "key_%d", i);
		sprintf (value, "value_%d", i);
		if (strcmp(dc_GetGlobalAttr(dc, key), value))
			printf ("Global att error: '%s' should equal '%s'\n", 
				key, value);
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
			printf ("Field att error: '%s' should equal '%s'\n", 
				key, value);
		sprintf (key, "sample_key_%d", i);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
			printf ("Sample att error: '%s' should equal '%s'\n", 
				key, value);
	}
	printf ("%s Deleting the even keys...\n", dash);
	for (i = 0; i < 50; i += 2)
	{
		sprintf (key, "key_%d", i);
		dc_RemoveGlobalAttr (dc, key);
		dc_RemoveFieldAttr (dc, field, key);
		sprintf (key, "sample_key_%d", i);
		dc_RemoveSampleAttr (dc, 0, key);
	}
	printf ("%s Making sure they're gone...\n", dash);
	for (i = 0; i < 50; i += 2)
	{
		sprintf (key, "key_%d", i);
		if (dc_GetFieldAttr (dc, field, key) != NULL)
			printf ("   field key '%s' still exists\n", key);
		if (dc_GetGlobalAttr (dc, key) != NULL)
			printf ("   global key '%s' still exists\n", key);
		sprintf (key, "sample_key_%d", i);
		if (dc_GetSampleAttr (dc, 0, key) != NULL)
			printf ("   sample key '%s' still exists\n", key);
	}
	if (dc_GetNGlobalAttrs(dc) != 57 - 25)
		printf ("%d global attrs should be %d\n",
			dc_GetNGlobalAttrs(dc), 57 - 25);
	dc_DumpDC (dc);
	/*
	 * Test retrieval of an attribute list
	 */
	printf ("%s retrieving a key/value list for field atts\n", dash);
	keys = dc_GetFieldAttrList(dc, field, NULL, &values, &natts);
	if (natts != dc_GetNFieldAttrs (dc, field))
		printf ("error: getlist returns %d natts != getn %d\n",
			natts, dc_GetNFieldAttrs (dc, field));
	for (i = 0; i < natts; ++i)
	{
		dc_GetFieldAttrArray (dc, field, keys[i], &type, &nval);
		printf ("   %s=%s:%d%c", keys[i], (type != DCT_String) ?
			dc_ElemToString(values[i], type) : (char *)values[i],
			nval, (i+1)%3 ? ' ':'\n');
	}
	printf ("\n");
	printf ("%s keys matching the pattern 'key_.[12345]':\n", dash);
	keys = dc_GetFieldAttrList(dc, field, "key_.[12345]",
				   &values, &natts);
	for (i = 0; i < natts; ++i)
	{
		dc_GetFieldAttrArray (dc, field, keys[i], &type, &nval);
		printf ("   %s=%s:%d%c", keys[i], (type != DCT_String) ?
			dc_ElemToString(values[i], type) : (char *)values[i],
			nval, (i+1)%3 ? ' ':'\n');
	}
	printf ("\n");
	/*
	 * Get a keys list
	 */
	keys = dc_GetGlobalAttrKeys(dc, &natts);
	printf ("%s using dc_GetGlobalAttrKeys, %d atts, (getn=%d):\n", 
		dash, natts, dc_GetNGlobalAttrs (dc));
	for (i = 0; i < natts; ++i)
		printf ("   %s%c", keys[i], (i+1)%5 ? ' ':'\n');
	printf ("\n");

	/*
	 * Now for a clincher: process each attribute list to remove each key
	 */
	printf ("%s trying to remove all attributes...\n", dash);
	ad.dc = dc;
	ad.sample = 0;
	ad.field = field;
	ad.which = Global;
	dc_ProcessAttrArrays (dc, NULL, T_RemoveAttr, (void *)&ad);
	ad.which = Sample;
	dc_ProcSampleAttrArrays (dc, 0, NULL, T_RemoveAttr, (void *)&ad);
	ad.which = Field;
	dc_ProcFieldAttrArrays (dc, field, NULL, T_RemoveAttr, (void *)&ad);
	dc_DumpDC (dc);
	printf ("%s Attributes with zero values and empty strings:\n", dash);
	dc_SetGlobalAttrArray (dc, "global_flag", 0, 0, NULL);
	dc_SetGlobalAttr (dc, "global_empty", "");
	dc_DumpDC (dc);
	printf ("%s Deleting the empty- and zero-valued global atts\n", dash);
	ad.which = Global;
	dc_ProcessAttrArrays (dc, NULL, T_RemoveAttr, (void *)&ad);
	dc_DumpDC (dc);
	dc_DestroyDC (dc);
	/*
	 * Fetch the previously stored file, hopefully including all of the
	 * typed attributes, and dump it
	 */
	printf ("%s Fetching the attributes from '%s'\n", dash, pname);
	ds_GetFields (plat, &when, &nfield, fields);
	dc = ds_FetchObs (plat, DCC_Scalar, &when, 
			  fields, nfield, NULL, 0);
	dc_DumpDC (dc);
	/*
	 * Test that the character arrays came back as strings
	 */
	printf ("%s verifying char arrays were converted to strings\n", dash);
	cptr = dc_GetGlobalAttr(dc, "global_char");
	if (!cptr || strcmp(cptr, cdata))
		printf ("Global att error: '%s' should be string '%s'\n", 
			"global_char", cdata);
	cptr = dc_GetFieldAttr(dc, field, "field_char");
	if (!cptr || strcmp(cptr, cdata))
		printf ("Field att error: '%s' should be string '%s'\n", 
			"field_char", cdata);
#endif /* ! EXAMPLE_ONLY */
	dc_DestroyDC (dc);
	Announce ("T_Attributes done.");
}

#endif /* ATTRIBUTES */


#ifdef TIME_UNITS
T_StoreTimes (begin, pid, pid2, details, ndetail)
ZebTime *begin;
PlatformId pid;
dsDetail *details;
int ndetail;
{
	DataChunk *dc, *ndc;
	int nfield = 10;
	FieldId fields[10];
	dsDetail fdets[10];
	int nfdet = 0;

	dc = T_SimpleScalarChunk (begin, 1, 10, 4, FALSE, FALSE);
	dc->dc_Platform = pid;
	ds_Store (dc, TRUE, details, ndetail);
	ds_GetFields (pid, begin, &nfield, fields);
	fdets[nfdet].dd_Name = DD_FETCH_BADVAL;
	fdets[nfdet++].dd_V.us_v_float = dc_GetBadval (dc);
	ndc = ds_FetchObs (pid, DCC_Scalar, begin, fields, nfield, 
			   fdets, nfdet);
	ndc->dc_Platform = pid2;
	ds_Store (ndc, TRUE, details, ndetail);
	dc_DestroyDC (dc);
	dc_DestroyDC (ndc);
}


T_TimeUnits (begin)
ZebTime *begin;
/*
 * Just send a bunch of strings to the time units parser and check
 * the results.
 */
{
	ZebTime zt;
	char buf[256];
	dsDetail details[10];
	int ndetail = 0;
	char *units[] = {
		"seconds since 1992-10-8 15:15:42.5 -6:00",
		"seconds depuis 1992-10-8 15:15:42.5 -6:00",
		"seconds",
		" milliseconds ",
		"hours since 1992-10-8 15:15:42.5 0:00",
		"seconds since 1992-10-8 15:15:42.5 8",
		"SECONDS SINCE 1992-10-8 15:15:42.5 8",
		"seconds ref 1992-10-8 15:15:42.5 800",
		"seconds ref 1992-10-8 15:15:42.5 830",
		"seconds @ 1992-10-8 15:15:42.5 8:00",
		"seconds from 1992-10-8 15:15:42.5 8:30",
		"seconds after 1992-10-8 15:15:42.5 8:00",
		"  seconds since 1970-1-1 0:00:00 0:00   ",
		"seconds since 1994-09-15 06:15:00 0:00",
		"  ",
		"seconds since 1994-09-15 06:15:00 zone",
		"seconds since 1994-09-15 06:15:00",
		NULL
	};
	char **u;
	int result;
	PlatformId pid, pid2;
			
#ifdef TEST_TIME_UNITS
	Announce ("--- test of dnc_TimeUnits() -------------");
	u = units;
	while (*u)
	{
		result = dnc_TimeUnits (&zt, *u);
		printf ("'%s': %s", *u, (result) ? "true" : "false\n");
		if (result)
		{
			TC_EncodeTime (&zt, TC_FullUSec, buf);
			printf (", %s\n", buf);
		}
		++u;
	}
#endif

	/*
	 * Now that we've tested the units parsing, try creating and reading
	 * files using different details.  Then fetch the data from each
	 * time and re-store it in a different platform so the files can
	 * be compared.
	 */
	Announce ("--- testing details of defining and fetching 'time' --");
	zt = *begin;
	pid = ds_LookupPlatform ("t_time_units");
	pid2 = ds_LookupPlatform ("t_time_units_2");
	T_StoreTimes (&zt, pid, pid2, NULL, 0);

	zt.zt_Sec += 60;
	details[0].dd_Name = DD_NC_TIME_LONG;
	T_StoreTimes (&zt, pid, pid2, details, 1);

	zt.zt_Sec += 60;
	details[0].dd_Name = DD_NC_ONE_TIME;
	T_StoreTimes (&zt, pid, pid2, details, 1);

	zt.zt_Sec += 60;
	details[1].dd_Name = DD_NC_TIME_FLOAT;
	T_StoreTimes (&zt, pid, pid2, details, 2);

	zt.zt_Sec += 60;
	details[1].dd_Name = DD_NC_TIME_LONG;
	T_StoreTimes (&zt, pid, pid2, details, 2);
}	

#endif /* TIME_UNITS */


#ifdef DAYSPLIT
/*
 * Test that datachunk samples on different days get correctly split
 * into separate files.
 */
int
T_Daysplit (begin)
ZebTime *begin;
{
	ZebTime start, next;
	DataChunk *dc;
	char buf[128];
	char *plat = "t_daysplit";
	PlatformId pid;
	int y1, y2, m1, m2, d1, d2;
	ZebTime otimes[20];
	int nobs = 0;
	int n;
#	define YEAR (365*24*3600)
#	define DAY (24*3600)

	sprintf (buf, "testing daysplit on platform '%s'", plat);
	Announce (buf);
	pid = ds_LookupPlatform (plat);
	start = *begin;
	TC_ZtSplit (&start, &y1, &m1, &d1, 0, 0, 0, 0);
	y1 -= 2;
	TC_ZtAssemble (&start, y1, m1, d1, 0, 0, 0, 0);
	dc = T_SimpleScalarChunk (&start, YEAR, 2, 4, FALSE, FALSE);
	nobs += 2;
	dc->dc_Platform = pid;
	ds_Store (dc, /*newfile*/FALSE, NULL, 0);
	dc_DestroyDC (dc);

	++y1;
	++m1;
	TC_ZtAssemble (&start, y1, m1, d1, 0, 0, 0, 0);
	++m1;
	TC_ZtAssemble (&next, y1, m1, d1, 0, 0, 0, 0);
	dc = T_SimpleScalarChunk (&start, next.zt_Sec - start.zt_Sec,
				  2, 4, FALSE, FALSE);
	nobs += 2;
	dc->dc_Platform = pid;
	ds_StoreBlocks (dc, /*newfile*/FALSE, NULL, 0);
	dc_DestroyDC (dc);

	++m1;
	TC_ZtAssemble (&start, y1, m1, d1, 0, 0, 0, 0);
	++m1;
	TC_ZtAssemble (&next, y1, m1, d1, 0, 0, 0, 0);
	dc = T_SimpleScalarChunk (&start, next.zt_Sec - start.zt_Sec,
				  2, 4, FALSE, FALSE);
	nobs += 2;
	dc->dc_Platform = pid;
	ds_StoreBlocks (dc, /*newfile*/FALSE, NULL, 0);
	dc_DestroyDC (dc);

	m1 += 2;
	TC_ZtAssemble (&start, y1, m1, d1, 0, 0, 0, 0);
	dc = T_SimpleScalarChunk (&start, DAY, 5, 4, FALSE, FALSE);
	nobs += 5;
	dc->dc_Platform = pid;
	ds_StoreBlocks (dc, /*newfile*/FALSE, NULL, 0);
	dc_DestroyDC (dc);

	/*
	 * After all that, we should have exactly one observation for each
	 * sample stored.
	 */
	msg_ELog (EF_INFO, "Checking for %d observations...", nobs);
	tl_Time (&start);
	n = ds_GetObsTimes (pid, &start, otimes, 20, NULL);
	if (n != nobs)
	{
		msg_ELog (EF_PROBLEM, "Found %d observations, expected %d",
			  n, nobs);
		return (1);
	}
	msg_ELog (EF_INFO, "Passed.");
	return (0);
}
#endif /* DAYSPLIT */


