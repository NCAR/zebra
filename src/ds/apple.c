/*
 * $Id: apple.c,v 3.4 1996-01-10 21:02:51 granger Exp $
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

For each data chunk class,
	for each test data datachunk in the class
	   for each filetype which supports the organization and class
	   	define the appropriate platform for the org and filetype
		call the class method for creating a test datachunk
		assign the platform to the datachunk
		store the datachunk
		fetch the datachunk
		compare (report differences between) the stored and
			the fetched datachunks
		destroy the datachunks


Example: each class performs comparisons relevant to its domain.
Transparent tests number of samples, times of each sample, mobility,
locations.  MetData tests the same fields.  Scalar knows how to test data
values.  The compare method calls the superclasses of the datachunk classes
from raw down to the class and returns with a list of diff messages, if
any.

At some point, it would be nice to construct a ds.config file from here
given what test we want to make, and then fork() and exec() a dsDaemon
on the config file ourselves.  Keep the "t_" prefix convention for all test
platforms.

Allow dynamic platform definitions through the ds protocol, define exactly
the set of needed test platforms on the fly.  A generic ds.config file, or
possibly none at all, will suffice.

Bind the test functions to Tcl commands.  Each call to a Tcl command 
registers the desire to run that test, then on 'begin', the ds.config is
generated and the test routines are run.  Could we then pipe some output
to 'expect'?

 *********/

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
#include <time.h>
#include <assert.h>

#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "ds_fields.h"
#include "DataChunkP.h"

#define EXPECT(N) \
{ \
	  int _mask; \
	  msg_ELog (EF_DEBUG, "Expect %d problem(s):", N); \
	  _mask = msg_ELPrintMask(0); \
	  msg_ELPrintMask (_mask & ~EF_PROBLEM)

#define SEEN \
	  msg_ELPrintMask (_mask); \
}

/*
 * Skip all of the dumps unless we're really gluttons for painstaking
 * investigation.
 */
#define dc_DumpDC(dc) {}

/*
 * More functions that are not yet on the main line.
 */
#define ds_IntDetail(key,value,det,ndet) \
{ \
	  int i = (ndet); \
	  ((det)[i]).dd_Name = (key); \
	  ((det)[i]).dd_V.us_v_int = (value); \
}
#define ds_SetDetail(key,det,ndet) \
{ \
	  ((det)[(ndet)]).dd_Name = (key); \
}
#define ds_FloatDetail(key,val,det,ndet) \
{ \
	  int i = (ndet); \
	  ((det)[i]).dd_Name = (key); \
	  ((det)[i]).dd_V.us_v_float = (val); \
}

/*
 * Each test routine looks up its required platforms, and if not defined
 * looks them up in the table and defines it.  It then proceeds with the
 * test and returns the appropriate return value.
 *
 * Why so many duplicate platforms?  Why not delete all files from a
 * platform, then use it for the test, then delete the files?  Then other
 * test routines can re-use that platform....
 *
 * Some higher mechanism will pick which tests to run.
 */

/*
 * DataChunk diffs not yet supported in the main tree
 */
/* #define DIFF_DATACHUNKS */

#define GRID_BLOCKS
#define FETCH_GAP

#define TIME_UNITS
/* #define EXAMPLE_ONLY */
#define NSPACE_AERI
#define AERI_TYPES
#define TRANSPARENT
#define SCALAR
#define GETFIELDS
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

#define PRINT_MASK	(EF_INFO | EF_EMERGENCY | EF_PROBLEM)

struct TestField {
	char *name;
	char *desc;
	char *units;
} TestFields[] = {
	{ "scalar",	"Test Field",			"none" },
	{ "field2",	"Test Field, too",		"none" },
	{ "fld3",	"field #3",			"none" },
	{ "no4",	"Number 4 field",		"none" },
	{ "pres",	"Pressure",		       	"mb" },
	{ "temp",	"Temperature",			"degC" },
	{ "rh",		"Relative humidity",		"pct" },
	{ "dpt",	"Dewpoint",			"K" }
};


struct TestPlatform {
	char *name;
#ifdef FILETYPES
	FileType ftype;
#endif
	DataOrganization org;
	int maxsamples;
	bool mobile;
	PlatformId platid;
} TestPlatforms[] = 
{
#ifdef FILETYPES
	{ "t_dummy_cdf", FTNetCDF, OrgScalar, 60, FALSE },
	{ "t_dummy_znf", FTZeb, OrgScalar, 60, FALSE },
	{ "t_gap_cdf", FTNetCDF, OrgScalar, 60, FALSE },
	{ "t_gap_znf", FTZeb, OrgScalar, 60, FALSE },
	{ "t_deletes", FTZeb, OrgScalar, 1, FALSE },
	{ "t_nspace", FTNetCDF, OrgNSpace, 1000, FALSE },
	{ "t_nsscalar", FTNetCDF, OrgNSpace, 1000, FALSE },
	{ "t_nsblocks", FTNetCDF, OrgNSpace, 1000, FALSE },
	{ "t_test6", FTNetCDF, OrgNSpace, 1000, FALSE },
	{ "t_copy_source", FTZeb, OrgScalar, 1000, TRUE },
	{ "t_copy_dest", FTNetCDF, OrgScalar, 1000, TRUE },
#else
	{ "t_dummy_cdf", OrgScalar, 60, FALSE },
	{ "t_dummy_znf", OrgScalar, 60, FALSE },
	{ "t_gap_cdf", OrgScalar, 60, FALSE },
	{ "t_gap_znf", OrgScalar, 60, FALSE },
	{ "t_deletes", OrgScalar, 1, FALSE },
	{ "t_nspace", OrgNSpace, 1000, FALSE },
	{ "t_nsscalar", OrgNSpace, 1000, FALSE },
	{ "t_nsblocks", OrgNSpace, 1000, FALSE },
	{ "t_test6", OrgNSpace, 1000, FALSE },
	{ "t_copy_source", OrgScalar, 1000, TRUE },
	{ "t_copy_dest", OrgScalar, 1000, TRUE },
#endif
	{ "t_aeri_types_cdf" },
	{ "t_aeri_types_znf" },
	{ "t_virtual" },
	{ "t_getfields_cdf" },
	{ "t_getfields_znf" },
	{ "t_nsvsc_scalar" },
	{ "t_nsvsc_nspace" },
	{ "t_fieldtypes" },
	{ "t_att_types_cdf" },
	{ "t_time_units" },
	{ "t_time_units_2" },
};

	

#define NUM_PLATFORMS	(sizeof(TestPlatforms)/sizeof(TestPlatforms[0]))
#define NUM_TESTFIELDS 	(sizeof(TestFields)/sizeof(TestFields[0]))

static float test_data[10000];


DataChunk *T_SimpleScalarChunk FP ((ZebTime *start, int interval, int nsample,
				    int nfield, int is_mobile, int addatts));
DataChunk *T_ScalarNSpaceChunk FP((ZebTime *start, int nsample, int nfield,
				   int is_mobile, int addatts));
static void T_DumpData FP((float *retrieve, int n, int len, char *fname));


/* ARGSUSED */
int
msg_handler (msg)
struct message *msg;
{
	msg_ELog (EF_DEBUG, "Message received");
	return (0);
}


static void
Announce(header)
char *header;
/*
 * Announce beginning of test sequence to event logger and to stdout
 */
{
	msg_ELog (EF_INFO, "%s", header);
}



static PlatformId
NeedPlatform (name)
char *name;
/*
 * Look for this platform name in our table and try to define it, returning
 * its platform id.  Someday maybe we could just make up a name based on the
 * needed characteristics and not require a table.
 */
{
	PlatformId pid;
	int i;

	if ((pid = ds_LookupPlatform (name)) != BadPlatform)
		return (pid);	/* already have it */
#ifdef notdef
	/*
	 * Otherwise we need to find it and define it 
	 */
	for (i = 0; i < NUM_PLATFORMS; ++i)
	{
		if (! strcmp(TestPlatforms[i].name, name))
			break;
	}
	if (i >= NUM_PLATFORMS)
	{
		msg_ELog (EF_PROBLEM, "could not find needed plat '%s'", name);
		return (BadPlatform);
	}
	msg_ELog (EF_DEBUG, "needed platform '%s' being defined", name);
	{
		PlatformDef def;
		ds_DefaultPlat (&def);
		def.pl_Org = TestPlatforms[i].org;
		def.pl_Filetype = TestPlatforms[i].ftype;
		def.pl_Mobile = TestPlatforms[i].mobile;
		def.pl_Maxsamp = TestPlatforms[i].maxsamples;
		pid = ds_DefinePlatform (&def, name);
	}
	TestPlatforms[i].platid = pid;
#endif
	return (pid);
}



static void
InitializePlatforms()
{
	int i;
	PlatformId platid;
	ZebTime now;

	msg_ELog (EF_DEBUG, "initializing data, erasing platforms");
	tl_Time (&now);
	now.zt_MicroSec = 0;
	for (i = 0; i < NUM_PLATFORMS; ++i)
	{
		platid = ds_LookupPlatform (TestPlatforms[i].name);
		TestPlatforms[i].platid = platid;
		if (platid == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "platform '%s' unknown",
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
#ifdef notdef
/*
 * Example of standalone code
 */
	msg_connect (NULL, "Will-Tell");
	ds_Standalone ();
#endif
/*
 * Skip the platform definitions and leave it for the individual test
 * routines to make sure the needed platforms have been defined. 
 */
	if (!msg_connect (msg_handler, "Will-Tell") ||
	    !ds_Initialize())
	{
		msg_ELog (EF_EMERGENCY, "Cannot connect nor initialize DS!");
		exit(1);
	}
/*
 * Try to limit our output to what's important
 */
	msg_ELPrintMask (PRINT_MASK);
	msg_ELSendMask (0);
	InitializePlatforms();
}



/*ARGSUSED*/
int
main (argc, argv)
	int argc;
	char *argv[];
{
	ZebTime when, begin, end;
	int i;
	int errors;

	Initialize();
	errors = 0;

#ifdef ZNF_TESTING
	errors += T_ZnfBlocks();
#endif

	TC_ZtAssemble (&when, 93, 1, 1, 0, 0, 0, 0);
	begin = when;
	end = when;

	for (i = 0; i < sizeof(test_data)/sizeof(test_data[0]); ++i)
		test_data[i] = i;

#ifdef TRANSPARENT
	errors += T_Transparent ("t_transparent", &begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_Transparent", errors);
#endif

#ifdef GETFIELDS
	/*
	 * Test ds_GetFields() for ZNF and netCDF
	 */
	errors += T_GetFields (&begin, "t_getfields_cdf");
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_GetFields(cdf)", errors);
	errors += T_GetFields (&begin, "t_getfields_znf");
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_GetFields(znf)", errors);
#endif
		
#ifdef DELETE_OBS
	errors += T_Deletes (&begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_Deletes", errors);
#endif

#ifdef FETCH_GAP
	errors += T_FetchGap (&begin, "t_gap_cdf");
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_FetchGap(cdf)", errors);
	errors += T_FetchGap (&begin, "t_gap_znf");
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_FetchGap(znf)", errors);
#endif

#if defined(DUMMY_FILES) && defined(DATA_TIMES)
	errors += T_DataTimes ("t_dummy_cdf");
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_DataTimes(cdf)", errors);
	errors += T_DataTimes ("t_dummy_znf");
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_DataTimes(znf)", errors);
#endif

#ifdef SCALAR
	errors += T_Scalar (&begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_Scalar", errors);
#endif /* SCALAR */

#ifdef NSPACE
	errors += T_NSpace (&begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_NSpace", errors);
#endif

#ifdef NSPACE_AERI
	errors += T_Aeri();
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_Aeri", errors);
#endif

#ifdef NEXUS
	/* test nexus-specific considerations, such as big, block overwrites */
	errors += T_Nexus (&begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_Nexus", errors);
#endif

#ifdef GRID_BLOCKS
	errors += T_IRGridStoreBlocks();
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_IRGridStoreBlocks", errors);
	errors += T_1DGridStoreBlocks();
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_1DGridStoreBlocks", errors);
#endif

#ifdef FIELD_TYPES
	errors += T_FieldTypes (begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_FieldTypes", errors);
#endif

#ifdef AERI_TYPES
	errors += T_AeriTypes (begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_AeriTypes", errors);
#endif

#ifdef SCALAR_NSPACE
	errors += T_ScalarNSpace (begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_ScalarNSpace", errors);
#endif

#ifdef ATTRIBUTES
	errors += T_Attributes (begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_Attributes", errors);
#endif

#ifdef RGRID
	errors += T_RGrid (begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_RGrid", errors);
#endif

#ifdef COPY_SCALAR
        errors += T_CopyScalar (&begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_CopyScalar", errors);
#endif

#ifdef TIME_UNITS
	errors += T_TimeUnits (&begin);
	if (errors) msg_ELog (EF_PROBLEM,
			      "%d errors after T_TimeUnits", errors);
#endif

	ds_ForceClosure();
	msg_ELog (EF_INFO, "%d errors.", errors);
	return (errors);
}


#ifdef SCALAR
T_Scalar(begin)
ZebTime *begin;
{
	DataChunk *dc, *ndc;
	int i,j;
	ZebTime when, end;
	ZebTime now;
	FieldId *fields;
	int nfield;
	int ndim, nvar, rndim, rstatic;
	char *name;
	unsigned long size;
	float *retrieve;
	PlatformId plat_id;
	PlatformId t_delete_id;
	int err = 0;

	msg_ELog (EF_DEBUG, 
		  "t_scalar: 4 fields, 10 samples, mobile, attributes");
	dc = T_SimpleScalarChunk (begin, 1, 10, 4, TRUE, TRUE);
	plat_id = NeedPlatform("t_scalar");
	dc->dc_Platform = plat_id;
	err += ! ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);

	/*
	 * Now really put it through the ringer.  Start 30 seconds
	 * back so that we precede the previous data, then overwrite
	 * it, and finally append the rest.  Note that the sample
	 * attributes should be deleted on the overwrite, since they 
	 * won't correspond to the samples with atts here.
	 */
	msg_ELog (EF_DEBUG, 
		  "t_scalar: 4 fields, 3000 samples, starting 30 secs back");
	when.zt_Sec -= 30;
	dc = T_SimpleScalarChunk (&when, 1, 3000, 4, TRUE, TRUE);
	dc->dc_Platform = plat_id;
	err += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	msg_ELog (EF_DEBUG, "t_fixed: storing same datachunk as for t_scalar");
	dc->dc_Platform = NeedPlatform("t_fixed");
	err += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	when.zt_Sec += 3000;

	/*
	 * Now overwrite a huge block of the previously stored data.
	 */
	msg_ELog (EF_DEBUG, 
		  "t_scalar: 4 fields, 1500 samples, 1500 secs prior to end");
	when.zt_Sec -= 1500;
	dc = T_SimpleScalarChunk (&when, 1, 1500, 4, TRUE, TRUE);
	dc->dc_Platform = plat_id;
	err += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	msg_ELog (EF_DEBUG, "t_fixed: same datachunk as for t_scalar above");
	dc->dc_Platform = NeedPlatform("t_fixed");
	err += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	when.zt_Sec += 1500;

	/*
	 * Now overwrite, but only with a subset of the fields
	 */
	msg_ELog (EF_DEBUG, 
		  "t_scalar: 3 fields, 1000 samples, 1250 secs prior to end");
	when.zt_Sec -= 1250;
	dc = T_SimpleScalarChunk (&when, 1, 1000, 3, TRUE, TRUE);
	dc->dc_Platform = plat_id;
	{
		struct TestField tf;
		tf = TestFields[1];
		TestFields[1] = TestFields[3];
		TestFields[3] = tf;
	}
	err += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	msg_ELog (EF_DEBUG, "t_fixed: same datachunk as for t_scalar above");
	dc->dc_Platform = NeedPlatform("t_fixed");
	err += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	when.zt_Sec += 1000;
	return (err);
}
#endif /* SCALAR */



#ifdef DUMMY_FILES
#ifdef DATA_TIMES
static int
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
	int errors = 0;

	n = ds_DataTimes (pid, when, check, which, times);
	TC_EncodeTime (when, TC_Full, buf);
	msg_ELog (EF_DEBUG, "DataTimes: %s", desc);
	msg_ELog (EF_DEBUG, "           %d found %s %s", n,
		  (which == DsBefore) ? "before" : "after", buf);
	if (n != expect)
	{
		msg_ELog (EF_PROBLEM, "DataTimes: %d expected, %d returned",
			  expect, n);
		++errors;
	}
	test.zt_MicroSec = 0;
	for (i = 0; i < n; ++i) 
	{
		TC_EncodeTime (times+i, TC_Full, buf);
		msg_ELog (EF_DEVELOP, "%s", buf);
		test.zt_Sec = first + expect - 1 - i;
		if (! TC_Eq(times[i], test))
		{
			TC_EncodeTime (&test, TC_Full, buf);
			msg_ELog (EF_PROBLEM, "expected time was %s", buf);
			++errors;
		}
	}
	return (errors);
}
#endif /* DATA_TIMES */


T_DataTimes (platform)
char *platform;
{
	DataChunk *dc;
	int i,j;
	ZebTime when, begin, end;
	ZebTime now, next;
#	define NS (60*5)	/* 5 60-sample files */
	ZebTime reftime;
	ZebTime times[NS];
	ZebTime first, last;
	PlatformId pid;
	int n;
	char buf[128];
	int errors = 0;

	tl_Time (&first);
	first.zt_MicroSec = 0;
	first.zt_Sec -= (first.zt_Sec % 60) + NS;
	dc = T_SimpleScalarChunk (&first, 1, NS, 4, FALSE, FALSE);
	pid = NeedPlatform (platform);
	dc->dc_Platform = pid;
	dc_GetTime (dc, NS - 1, &last);
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
#ifdef DATA_TIMES 	/* use the dummy files to test ds_DataTimes */
	/*
	 * To test ds_DataTimes, lets pass it now as the reference time,
	 * and space for all of the times, and make sure we get the right
	 * times back.  Then try doing smaller sets of times.
	 */
	Announce ("Testing ds_DataTimes()...");
	msg_ELog (EF_DEBUG, "DataTimes: platform '%s'", platform);
	tl_Time (&now);
	errors += CheckTimes (pid, "all times before now", &now, NS, DsBefore,
			      NS, last.zt_Sec - NS + 1);
	errors += CheckTimes (pid, "two most recent times", &now, 2, DsBefore,
			      2, last.zt_Sec - 2 + 1);
	errors += CheckTimes (pid, "two times before first", &first, 2, 
			      DsBefore, 1, first.zt_Sec);
	next = first;
	next.zt_Sec += 60;
	errors += CheckTimes (pid, "three times before 1st time of 2nd file", 
		    &next, 3, DsBefore, 3, next.zt_Sec - 3 + 1);
	next.zt_Sec += 60;
	errors += CheckTimes (pid, "five times before 1st time of 3rd file", 
		    &next, 5, DsBefore, 5, next.zt_Sec - 5 + 1);
	next.zt_Sec += 30;
	errors += CheckTimes (pid, "10 times after middle of 3rd file", 
		    &next, 10, DsAfter, 10, next.zt_Sec);
	errors += CheckTimes (pid, "10 times after last time", 
		    &last, 10, DsAfter, 1, last.zt_Sec);
	errors += CheckTimes (pid, "120 times after first time", 
		    &first, 120, DsAfter, 120, first.zt_Sec);
	next.zt_Sec = 1;
	errors += CheckTimes (pid, "5 times after prehistoric time", 
		    &next, 5, DsAfter, 5, first.zt_Sec);
#   endif
	return (errors);
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
	int errors = 0;

	sprintf (buf, "FetchGap: testing platform '%s'", plat);
	Announce (buf);
	when = *begin;
	/* 10 samples, 30-second intervals */
	dc = T_SimpleScalarChunk (begin, 30, 10, 4, FALSE, FALSE);
	pid = NeedPlatform(plat);
	dc->dc_Platform = pid;
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	/*
	 * Now fetch an interval within the data file but which does not
	 * contain any times in the file.  The result should be NULL.
	 */
	when.zt_Sec += 15;		/* halfway between samples */
	errors += !ds_GetFields (pid, &when, &nfield, fields);
	dc = ds_Fetch (pid, DCC_Scalar, &when, &when, fields, nfield, NULL, 0);
	if (dc)
	{
		msg_ELog (EF_PROBLEM, "FetchGap: dc has %d samples",
			  dc_GetNSample (dc));
		dc_DestroyDC (dc);
		++errors;
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
		msg_ELog (EF_PROBLEM, "FetchGap: dc has %d samples",
			  dc_GetNSample (dc));
		dc_DestroyDC (dc);
		++errors;
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
			msg_ELog(EF_PROBLEM, "FetchGap: dc has %d samples %s",
				 ns, "instead of 4");
			++errors;
		}
		dc_DestroyDC (dc);
	}
	else
	{
		++errors;
		msg_ELog (EF_PROBLEM, "FetchGap: fetch 120 sec returned null");
	}
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
			++errors;
			msg_ELog (EF_PROBLEM, "FetchGap: dc has %d samples %s",
				  dc_GetNSample (dc), "instead of 10");
		}
		dc_DestroyDC (dc);
	}
	else
	{
		++errors;
		msg_ELog (EF_PROBLEM, "FetchGap: fetch 320 sec returned null");
	}
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
			msg_ELog (EF_PROBLEM, "FetchGap: dc has %d samples %s",
				  ns, "instead of 10");
			++errors;
		}
		dc_DestroyDC (dc);
	}
	else
	{
		++errors;
		msg_ELog (EF_PROBLEM, "FetchGap: fetch 320 sec returned null");
	}
	return (errors);
}
#endif	/* FETCH_GAP */



int
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
	int err = 0;
#	define NUMS 1000

	dc_CheckClass (FALSE);

	MARK(scbld);
	msg_ELog (EF_DEBUG, "t_nsvsc_scalar: 8 fields, mobile, attributes");
	dc = T_SimpleScalarChunk (&begin, 1, NUMS, 8, TRUE, TRUE);
	plat_id = NeedPlatform(sc_plat);
	dc->dc_Platform = plat_id;
	MARK(scsto);
	err += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	msg_ELog (EF_DEVELOP, "fetching the entire observation from '%s'", 
		  sc_plat);
	nfield = 10;
	err += !ds_GetFields (plat_id, &begin, &nfield, fields);
	MARK(scfet);
	dc = ds_FetchObs (plat_id, DCC_Scalar, &begin, 
			  fields, nfield, NULL, 0);
	msg_ELog (EF_DEBUG, "Done.");
	MARK(scacc);
	if (dc)
	{
		for (i = 0; i < NUMS; ++i)
			for (f = 0; f < nfield; ++f)
				value = dc_GetScalar (dc, i, fields[f]);
		dc_DumpDC (dc);
		dc_DestroyDC(dc);
	}
	else
		++err;

	MARK(nsbld);
	msg_ELog (EF_DEBUG, "t_nsvsc_nspace: 8 fields, mobile, attributes");
	dc = T_ScalarNSpaceChunk (&begin, NUMS, 8, TRUE, TRUE);
	plat_id = NeedPlatform(ns_plat);
	dc->dc_Platform = plat_id;
	MARK(nssto);
	err += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	msg_ELog (EF_DEBUG, "fetching the entire observation from '%s'...", 
		  ns_plat);
	nfield = 10;
	err += !ds_GetFields (plat_id, &begin, &nfield, fields);
	MARK(nsfet);
	dc = ds_FetchObs (plat_id, DCC_NSpace, &begin, 
			  fields, nfield, NULL, 0);
	MARK(nsacc);
	msg_ELog (EF_DEBUG, "Done.");
	if (dc)
	{
		for (i = 0; i < NUMS; ++i)
			for (f = 0; f < nfield; ++f)
				err += !dc_NSGetSample (dc, i, fields[f], 0);
		dc_DumpDC (dc);
		dc_DestroyDC(dc);
	}
	else 
		++err;
	dc_CheckClass (TRUE);
	return (err);
}





#ifdef FIELD_TYPES
static void
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
	PlatformId plat_id = NeedPlatform ("t_fieldtypes");
	int err = 0;

	Announce ("Testing field types interfaces");
	for (i = 1; i < nfield+1; ++i)
	{
		msg_ELog (EF_DEVELOP, "type name: %s, size %d", 
			  dc_TypeName(i), dc_SizeOfType(i));
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
	msg_ELog (EF_DEBUG, 
		  "nsamples hint should be 10, but only 1 sample allocated");
	dc_DumpDC (dc);
	++when.zt_Sec;
	T_AddTypedScalarSample (dc, when, 1, fields);
	msg_ELog (EF_DEBUG, "nsamples allocated should now be 10, ");
	msg_ELog (EF_DEBUG, 
		  "space for 8 samples between NextOffset and DataLen:");
	++when.zt_Sec;
	dc_DumpDC (dc);
	for (i = 2; i < 10; ++i, ++when.zt_Sec)
		T_AddTypedScalarSample (dc, when, i, fields);
	dc_DumpDC (dc);
	fflush (stdout);

	msg_ELog (EF_DEBUG, 
		  "ds_Store()'ing the typed DataChunk to t_fieldtypes");
	err += !ds_Store (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	msg_ELog (EF_DEBUG, "Fetching the typed scalar observation");
	dc = ds_FetchObs (plat_id, DCC_Scalar, &begin, fields, nfield, 0, 0);
	if (dc)
	{
		dc_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	else
		++err;
	return (err);
}
#endif /* FIELD_TYPES */




static int
T_Dedit (begin, platid)
ZebTime begin;
PlatformId platid;
{
	FieldId fields[30];
	int nfield = 30;
	DataChunk *dc;
	int i;
	char attval[128];
	int err = 0;

	Announce("dedit read and overwrite simulation...");
	err += !ds_GetFields (platid, &begin, &nfield, fields);
	dc = ds_FetchObs (platid, DCC_Scalar, &begin, fields, nfield, NULL, 0);
	if (! dc)
		return (++err);
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
	msg_ELog (EF_DEBUG, "performing repeated fetch/store cycles");
	for (i = 0; i < 50; ++i)
	{
		dc = ds_FetchObs (platid, DCC_Scalar, &begin, fields, 
				  nfield, NULL, 0);
		if (dc)
		{
			ds_StoreBlocks (dc, FALSE, 0, 0);
			dc_DestroyDC(dc);
		}
		else
			++err;
	}
	return (err);
}



int
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
	int errors = 0;

	plat_id = NeedPlatform(pname);
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
	ds_SetDetail (DD_ZN_APPEND_SAMPLES, details, ndetail++);
#endif
#endif /* !BACKWARDS */

	/*
	 * Set some optimization parameters
	 */
	dc_CheckClass (FALSE);

	/* now begin creating a single sample DataChunk and storing it */
	Announce ("Nexus simulation...");
	sprintf (buf, "creating '%s' observation of 500 samples, 1 at a time",
		 pname);
	msg_ELog (EF_DEBUG, buf);
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
			ds_IntDetail (DD_ZN_HINT_NSAMPLES, 1000, details,
				      ndetail++);
			/* Reserve space for 100 sample atts of 50 bytes */
			ds_IntDetail (DD_ZN_RESERVE_BLOCK, 100*50, details,
				      ndetail++);
#endif /* !BACKWARDS */
			errors += !ds_StoreBlocks (dc, TRUE, details, ndetail);
#if !defined(BACKWARDS)
			ndetail -= 2;
#endif /* !BACKWARDS */
		}
		else
			errors += !ds_StoreBlocks(dc, FALSE, details, ndetail);
		dc_DestroyDC(dc);
		loc.l_alt += 5.0;
		when.zt_Sec += 4;

		/*
		 * If we're halfway, simulate interjection by dedit
		 */
		if (i == 250)
			errors += T_Dedit (*begin, plat_id);
	}

	/* now have a file of 500 samples, try to fetch the whole thing */
	msg_ELog (EF_DEBUG, 
		  "fetching the entire observation from '%s'...", pname);
	dc = ds_FetchObs (plat_id, DCC_Scalar, begin, fields, n, NULL, 0);
	if (!dc)
		return (++errors);
	dc_DumpDC (dc);
	/* now store it all back, overwriting what's already there; this
	 * is so that we can verify the fetch through what's in the file */
	msg_ELog (EF_DEBUG, 
		  "overwriting observation with fetched datachunk...");
	errors += !ds_StoreBlocks (dc, FALSE, details, ndetail);
	msg_ELog (EF_DEBUG, "Done.");
	dc_DestroyDC(dc);
	return (errors);
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
	int err = 0;

	Announce ("Copying one datachunk to another, adding one field");
	plat_id = NeedPlatform(pname);
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
	msg_ELog (EF_DEBUG, buf);
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
	err += !ds_StoreBlocks (dc, TRUE, details, ndetail);
	dc_DestroyDC(dc);

	/* now have a file of 500 samples, try to fetch the whole thing */
	msg_ELog (EF_DEBUG, "fetching the entire observation from '%s'...",
		  pname);
	dc = ds_FetchObs (plat_id, DCC_Scalar, begin, fields, n, NULL, 0);
	if (!dc)
		return (++err);
	dc_DumpDC (dc);

	/* now create our second dc, and go through the hassle of adding
	 * that one more field to it and copying attributes and data */
	msg_ELog (EF_DEBUG, 
		  "creating our copy dc for '%s', adding field 'avg'", 
		  pdest);
	newdc = dc_CreateDC (DCC_Scalar);
	newdc->dc_Platform = NeedPlatform (pdest);

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
	msg_ELog (EF_DEBUG, "copying data from src dc to dest dc...");
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
	msg_ELog (EF_DEBUG, "storing the new datachunk to '%s'", pdest);
	err += !ds_StoreBlocks (newdc, TRUE, details, ndetail);
	dc_DestroyDC(newdc);
	dc_DestroyDC(dc);
	msg_ELog (EF_DEBUG, "Done with DataChunk copy test.");
	return (err);
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
	unsigned long size;
	FieldId *fields;
	int nfield;
	int errors = 0;

	plat_id = NeedPlatform("t_nspace");
	Announce("Testing NSpace interface and storage");

	msg_ELog (EF_DEBUG, "---- test1 ------");
	{	/* an empty NSpace data chunk */
		
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = plat_id;
		dc_DumpDC (dc);
		dc_SetStaticLoc (dc, &loc);
		errors += !ds_Store (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	msg_ELog (EF_DEBUG, "----- test2 ------");
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
		SEEN;
		dc_NSAddSample (dc, &when, 0, field, test_data+1000);
		EXPECT(1);
		dc_NSAddStatic (dc, field, test_data);
		SEEN;
		dc_NSAddStatic (dc, sfield, test_data);
		dc_SetSampleAttr (dc, 0, "test_key", "test_value");
		dc_SetSampleAttr (dc, 0, "test_key2", "test_value2");
		dc_SetSampleAttr (dc, 1, "sample_number", "0");
		++when.zt_Sec;
		dc_NSAddSample (dc, &when, 1, field, test_data+1005);
		dc_SetSampleAttr (dc, 1, "sample_number", "1");
		dc_DumpDC (dc);

		/* retrieve data and compare */
		msg_ELog (EF_DEVELOP, 
			  "retrieving static data with GetStatic...");
		retrieve = dc_NSGetStatic (dc, sfield, &size);
		errors += T_CompareData(retrieve, test_data, size);
		msg_ELog (EF_DEVELOP, 
			  "retrieving static data with GetSample(1)...");
		retrieve = dc_NSGetSample (dc, 1, sfield, &size);
		errors += T_CompareData(retrieve, test_data, size);
		msg_ELog (EF_DEVELOP, 
			  "retrieving static data with GetSample(100)...");
		EXPECT(1); /* there is no sample 100 */
		retrieve = dc_NSGetSample (dc, 100, sfield, &size);
		/* errors += T_CompareData(retrieve, test_data, size); */
		SEEN;
		retrieve = dc_NSGetSample (dc, 1, field, &size);
		msg_ELog (EF_DEVELOP,
			  "dc_NSGetSample(%s) returns size = %lu,", 
			  F_GetName(field), size);
		errors += T_CompareData(retrieve, test_data+1005, size);

		T_NSGetField(dc, field);
		T_NSGetField(dc, sfield);
		/* T_NSGetAllDimensions(dc, field); */

		msg_ELog (EF_DEBUG, "Storing... "); fflush(stdout);
		dc_SetStaticLoc (dc, &loc);
		errors += !ds_Store (dc, TRUE, 0, 0);
		msg_ELog (EF_DEBUG, "Destroying...");
		dc_DestroyDC (dc);

		/* now try to fetch what we just stored and see what we get */
		msg_ELog (EF_DEBUG, "Fetching data....   "); fflush(stdout);
		dc = ds_Fetch (plat_id, DCC_NSpace, &when, &when,
			       fids, 2, NULL, 0);
		if (! dc)
			++errors;
		else
		{
			msg_ELog (EF_DEBUG,
				  "DataChunk returned by ds_Fetch():");
			dc_DumpDC (dc);
			/* retrieve data and compare */
			T_NSGetField(dc, field);
			T_NSGetField(dc, sfield);
			msg_ELog (EF_DEVELOP,
				  "Comparing retrieved dynamic data...");
			retrieve = dc_NSGetSample (dc, 0, field, &size);
			errors += T_CompareData(retrieve,test_data+1005, size);
			msg_ELog (EF_DEVELOP,
				  "Comparing retrieved static data...");
			retrieve = dc_NSGetStatic (dc, sfield, &size);
			msg_ELog (EF_DEVELOP, 
			  "dc_NSGetStatic() returns size = %lu,", size);
			errors += T_CompareData(retrieve, test_data, size);
			dc_DestroyDC (dc);
		}

		/* now try again but with fields in reverse order */
		msg_ELog (EF_DEBUG, "Fetching data, fields reversed...  ");
		fflush(stdout);
		fids[0] = sfield;
		fids[1] = field;
		dc = ds_Fetch (plat_id, DCC_NSpace, &when, &when,
			       fids, 2, NULL, 0);
		if (! dc)
			++errors;
		else
		{
			msg_ELog (EF_DEVELOP,
				  "DataChunk returned by ds_Fetch():");
			dc_DumpDC (dc);
			
			/* retrieve data and compare */
			T_NSGetField(dc, field);
			T_NSGetField(dc, sfield);
			msg_ELog (EF_DEVELOP,
				  "Comparing retrieved dynamic data...");
			retrieve = dc_NSGetSample (dc, 0, field, &size);
			errors += T_CompareData(retrieve,test_data+1005, size);
			msg_ELog (EF_DEVELOP, 
				  "Comparing retrieved static data...");
			retrieve = dc_NSGetStatic (dc, sfield, &size);
			msg_ELog (EF_DEVELOP, 
				  "dc_NSGetStatic() returns size = %lu,", 
				  size);
			errors += T_CompareData(retrieve, test_data, size);
			dc_DestroyDC (dc);
		}
        }
	msg_ELog (EF_DEBUG, "------ test3 ------");
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
		SEEN;
		dc_NSAddSample (dc, &begin, 0, therm_id, test_data);
		dc_NSAddSample (dc, &begin, 0, mean_rad_id, test_data);
		dc_NSAddSample (dc, &begin, 0, sd_rad_id, test_data);

		/* store it, then add more data, and store again later  */
		/* with newfile flag FALSE				*/
		dc_SetStaticLoc (dc, &loc);
		errors += !ds_Store (dc, TRUE, 0, 0);
		++end.zt_Sec;
		dc_NSAddSample (dc, &end, 1, therm_id, test_data+100);
		dc_NSAddSample (dc, &end, 1, mean_rad_id, test_data+100);
		dc_NSAddSample (dc, &end, 1, sd_rad_id, test_data+100);
		dc_DumpDC (dc);

		/* test some retrieval */
		(void)dc_NSGetSample (dc, 0, wnum_id, &size); /*should pass*/
		retrieve = dc_NSGetStatic (dc, wnum_id, &size);
		retrieve = dc_NSGetStatic (dc, wnum_id, NULL);
		msg_ELog (EF_DEVELOP,
			  "dc_NSGetStatic(%s) returns size = %lu,", 
			  F_GetName(wnum_id), size);
		errors += T_CompareData (retrieve, test_data+50, size);

		retrieve = dc_NSGetSample (dc, 0, therm_id, &size);
		msg_ELog (EF_DEVELOP, "GetSample(0,%s): size=%lu, data=%s",
		  F_GetName(therm_id),size,(retrieve)?"non-NULL":"NULL");
		retrieve = dc_NSGetSample (dc, 1, mean_rad_id, &size);
		retrieve = dc_NSGetSample (dc, 1, mean_rad_id, NULL);
		msg_ELog (EF_DEVELOP,
			  "dc_NSGetSample(%s) returns size = %lu,", 
			  F_GetName(mean_rad_id), size);
		errors += T_CompareData (retrieve, test_data+100, size);

		EXPECT(1);
		retrieve = dc_NSGetSample (dc, 0, BadField, NULL);
		SEEN;
		msg_ELog (EF_DEVELOP, "GetSample(2,BadField): data=%s",
			  (retrieve)?"non-NULL":"NULL");

		errors += !ds_Store (dc, FALSE, 0, 0);
		fields = dc_GetFields (dc, &nfield);

		/* now try to fetch what we just stored and see what we get */
		msg_ELog (EF_DEBUG, 
			  "Fetching data (detail: badval=999 .... "); 
		fflush(stdout);
		ds_FloatDetail (DD_FETCH_BADVAL, 999.0, &dsd, 0);
		ndc = ds_Fetch (plat_id, DCC_NSpace, &begin, &end,
				fields, nfield, &dsd, 1);
		dc_DestroyDC (dc);
		dc = ndc;
		if (! dc)
			++errors;
		else
		{
			msg_ELog (EF_DEVELOP,
				  "DataChunk returned by ds_Fetch():");
			dc_DumpDC (dc);
			
			/* re-do the data comparisions, should be identical */
			T_NSGetAllDimensions(dc);
			T_NSGetAllVariables(dc);
			
			/* re-test some retrieval */
			msg_ELog (EF_DEVELOP, 
			  "Comparing fetched data with stored data...");
			dc_NSGetSample (dc, 0, wnum_id, &size); /*should pass*/
			retrieve = dc_NSGetStatic (dc, wnum_id, &size);
			retrieve = dc_NSGetStatic (dc, wnum_id, NULL);
			msg_ELog (EF_DEVELOP,
				  "dc_NSGetStatic(%s) returns size = %lu,", 
				  F_GetName(wnum_id), size);
			errors += T_CompareData (retrieve, test_data+50, size);
			
			retrieve = dc_NSGetSample (dc, 0, therm_id, &size);
			msg_ELog (EF_DEVELOP,
				  "GetSample(0,%s): size=%lu, data=%s",
				  F_GetName(therm_id),size,
				  (retrieve)?"non-NULL":"NULL");
			retrieve = dc_NSGetSample (dc, 1, mean_rad_id, &size);
			retrieve = dc_NSGetSample (dc, 1, mean_rad_id, NULL);
			msg_ELog (EF_DEVELOP,
				  "dc_NSGetSample(%s) returns size = %lu,", 
				  F_GetName(mean_rad_id), size);
			errors += T_CompareData(retrieve, test_data+100, size);
			
			EXPECT(1);
			retrieve = dc_NSGetSample (dc, 0, BadField, NULL);
			SEEN;
			msg_ELog (EF_DEVELOP, "GetSample(2,BadField): data=%s",
				  (retrieve)?"non-NULL":"NULL");
			
			/* try some block storage */
			msg_ELog (EF_DEBUG, 
			  "Storing on platform t_nsblocks using blocks");
			dc->dc_Platform = NeedPlatform ("t_nsblocks");
			ds_StoreBlocks (dc, TRUE, 0, 0);
			msg_ELog (EF_DEBUG, 
			  "Storing on platform t_nsscalar w/o using blocks");
			dc->dc_Platform = NeedPlatform ("t_nsscalar");
			ds_StoreBlocks (dc, TRUE, 0, 0);
			dc_DestroyDC (dc);
		}
	}
	msg_ELog (EF_DEBUG, "------ test4 ------");
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
		msg_ELog (EF_DEBUG, "Adding 500 samples to datachunk... "); 
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
		msg_ELog (EF_DEBUG, "PutSample to 't_nspace' ... ");
		errors += !ds_Store (dc, TRUE, 0, 0);
		msg_ELog (EF_DEBUG, "and PutBlock to plat 't_nsblocks' ... "); 
		dc->dc_Platform = NeedPlatform ("t_nsblocks");
		errors += !ds_StoreBlocks (dc, TRUE, 0, 0);

		/* now try to fetch what we just stored and see what we get */
		/* remember to keep the dc around which is holds the fields */
		fields = dc_GetFields (dc, &nfield);
		msg_ELog (EF_DEBUG, "Fetching from '%s' ....  ", 
			  ds_PlatformName(plat_id));
		ndc = ds_Fetch (plat_id, DCC_NSpace, &begin, &end,
				fields, nfield, NULL, 0);
		/* compare the data we retrieved */
		errors += (ndc == NULL);
		for (i = 0; ndc && i < 500; i+=100)
		{
			msg_ELog (EF_DEVELOP, "Sample %d: ", i);
			retrieve = dc_NSGetSample (ndc, i, fields[0], &size);
			errors += T_CompareData (retrieve, test_data+i, size);
			retrieve = dc_NSGetSample (ndc, i, fields[2], &size);
			errors += T_CompareData (retrieve, test_data+i, size);
		}
		if (ndc) dc_DestroyDC (ndc);
		msg_ELog (EF_DEBUG, "Fetching from 't_nsblocks' ...");
		ndc = ds_Fetch (NeedPlatform("t_nsblocks"), 
				DCC_NSpace, &begin, &end,
				fields, nfield, NULL, 0);
		/* compare the data we retrieved */
		errors += (ndc == NULL);
		for (i = 0; ndc && i < 500; i+=75)
		{
			msg_ELog (EF_DEVELOP, "Sample %d: ", i);
			retrieve = dc_NSGetSample (ndc, i, fields[0], &size);
			errors += T_CompareData (retrieve, test_data+i, size);
			retrieve = dc_NSGetSample (ndc, i, fields[2], &size);
			errors += T_CompareData (retrieve, test_data+i, size);
		}
		if (ndc) dc_DestroyDC (ndc);
#endif
		msg_ELog (EF_DEBUG, "Done.");
		dc_DestroyDC (dc);
	}
	msg_ELog (EF_DEBUG, "----- test5 -----");
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
		SEEN;

		/* try defining a dimension with a long name */
		EXPECT(2);  	/* 2 dimn names too long */
		dc_NSDefineField(dc, fid, 4, dimname, dimsize, FALSE);
		SEEN;
		dc_DumpDC (dc);
		
		/* now try redefining a dimension */
		EXPECT(3); /* 1 too long, 1 for redefined, 1 for new id */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		SEEN;
		dc_DumpDC (dc);

		/* redefine a variable */
		EXPECT(2); /* 1 to redefine field, 1 for change in dimns */
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);
		SEEN;
		/* re-define 'tests' to be dynamic but suppress the warning */
		dc_NSAllowRedefine (dc, TRUE);
		dc_NSDefineVariable (dc, fid, 1, &did, FALSE);
		dc_DumpDC (dc);

		/* test completion */
		msg_ELog (EF_DEVELOP, "dc_NSDefineIsComplete() returns %s",
			  dc_NSDefineIsComplete(dc) ? "True" : "False");

		/* force completion */
		dc_NSDefineComplete(dc);
		msg_ELog (EF_DEVELOP, "%s, dc_NSDefineIsComplete() returns %s",
			  "After dc_NSDefineComplete()",
			  dc_NSDefineIsComplete(dc) ? "True" : "False");

		EXPECT(2);
		/* try more definition after completion */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);
		SEEN;

		/* quick addition of data just to create a file */
		when.zt_Sec += 60;
		dc_SetStaticLoc (dc, &loc);
		dc_NSAddSample (dc, &when, 0, fid, test_data+2000);
		
		dc_DumpDC (dc);
		errors += !ds_StoreBlocks (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	msg_ELog (EF_DEBUG, "----- test6 ------");
	{	/* push limits of number of dims and fields in a chunk */

		char name[ 10 ];
		char *namep = name;
		FieldId fid, did;
		unsigned long size;
		int i;

		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = NeedPlatform("t_test6");

		/* test dimn limit */
		for (i = 0; i < DC_MaxDimension + 2; ++i)
		{
			if (i >= DC_MaxDimension)
			{ 
				EXPECT(2);
				sprintf (name, "dimn%i", i);
				did = F_DeclareField(name, "Dimension","none");
				size = i;
				dc_NSDefineDimension(dc, did, size);
				SEEN;
			}
			else
			{
				sprintf (name, "dimn%i", i);
				did = F_DeclareField(name, "Dimension","none");
				size = i;
				dc_NSDefineDimension(dc, did, size);
			}
		}

		/* test field limit */
		did = F_Lookup("dimn12");
		for (i = 0; i < DC_MaxField + 2; ++i)
		{
			if (i >= DC_MaxField)
			{ 
				EXPECT(2);
				sprintf (name, "field%i", i);
				fid = F_DeclareField(name, "Field", "units");
				dc_NSDefineVariable(dc, fid, 1, &did, i % 2);
				SEEN;
			}
			else
			{
				sprintf (name, "field%i", i);
				fid = F_DeclareField(name, "Field", "units");
				dc_NSDefineVariable(dc, fid, 1, &did, i % 2);
			}
		}

		EXPECT(1);
		/* test field limit with DefineField */
		dc_NSDefineField(dc, fid, 0, 0, 0, 0);
		SEEN;

		EXPECT(2); /* 1 for dimn limit, 1 for aborted field defn */
		/* test dimn limit with DefineField by redefining field */
		dc_NSDefineField(dc, F_Lookup("field1"), 1, 
				 &namep, &size, TRUE);
		SEEN;

		/* dc_DumpDC (dc); */

		/* see what it looks like after closing definition */
		dc_NSDefineComplete (dc);
		/* dc_DumpDC (dc); */

		errors += !ds_Store (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	msg_ELog (EF_DEBUG, "----- test7 ------");
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
		msg_ELog (EF_DEVELOP, 
			  "dynamic fields of same sizes, should be UNIFORM:");
		dc_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	return (errors);
}
#endif /* NSPACE */




DataChunk *
T_ScalarNSpaceChunk (start, nsample, nfield, is_mobile, addatts)
ZebTime *start;
int nsample;
int nfield;
int is_mobile;
int addatts;
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
int is_mobile;
int addatts;
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



static int
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
" burghart     - Died on level   9. Started on level   1.  Score:     9542.\n\
 burghart     - Died on level  18. Started on level  12.  Score:     8583.\n\
 burghart     - Died on level   8. Started on level   1.  Score:     8420.\n\
 burghart     - Died on level  17. Started on level  12.  Score:     8153.\n\
 burghart     - Died on level  10. Started on level   1.  Score:     7905.\n\
 burghart     - Died on level   8. Started on level   1.  Score:     7800.",
"You are quite disappointing: *granger" };
	int ntext = sizeof(text)/sizeof(text[0]);
	int i, len;
	char *data, *src;
	ZebTime when;
	static Location loc = { 1.0, 2.0, 4.0 };
	int errors = 0;

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
		if (len != strlen(src)+1)
			++errors;
		if (strcmp(data, src))
			++errors;
	}
	return (errors);
}




#ifdef NSPACE_AERI
T_Aeri()
{
	ZebTime begin, when;
	char *pname = "sgpaerich1C1.a1";
	PlatformId pid = NeedPlatform(pname);
	FieldId fields[30];
	float *retrieve;
	DataChunk *dc;
	int n, i;
	unsigned long len;
	int err = 0;

	Announce("AERI typed field test");
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
	if (! ds_GetObsTimes (pid, &begin, &when, 1, NULL))
		return (++err);

	/* fetch a DC from the aeri platform and dump its data */
	msg_ELog (EF_DEBUG, "Fetching %s data....   ", pname);
	dc = ds_FetchObs (pid, DCC_NSpace, &when, fields, n, NULL, 0);
	if (! dc)
	{
		msg_ELog (EF_PROBLEM, "could not fetch '%s' data", pname);
		return (++err);
	}
	msg_ELog (EF_DEVELOP, "DataChunk returned by ds_Fetch():");
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
	msg_ELog (EF_DEVELOP, "Lat = %.2f, ", *retrieve);
	retrieve = dc_NSGetStatic (dc, F_Lookup("lon"), &len);
	msg_ELog (EF_DEVELOP, "Lon = %.2f, ", *retrieve);
	retrieve = dc_NSGetStatic (dc, F_Lookup("alt"), &len);
	msg_ELog (EF_DEVELOP, "Alt = %.2f", *retrieve);

	retrieve = dc_NSGetSample (dc, 0, 
				   F_Lookup("thermistor0"), &len);
	T_DumpData (retrieve, 5, len, "thermistor0");
	dc_DestroyDC(dc);
	return (err);
}
#endif /* NSPACE_AERI */


int
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
	int errors = 0;

	/*
	 * Quick test of ds_GetFields()
	 */
	sprintf (buf, "Testing ds_GetFields() for platform '%s'", plat);
	Announce (buf);
	dc = T_SimpleScalarChunk (start, 1, 10, 4, FALSE, FALSE);
	dc->dc_Platform = NeedPlatform(plat);
	dc_fields = dc_GetFields (dc, &dc_nfield);
	ds_Store (dc, TRUE, NULL, 0);
	nfield = 10;
	if (!ds_GetFields(dc->dc_Platform, start, &nfield, fields))
	{
		++errors;
		msg_ELog (EF_PROBLEM, 
			  "ds_GetFields('%s',nfld=10) failed", plat);
	}
	else if (nfield != dc_nfield)
	{
		++errors;
		msg_ELog (EF_PROBLEM, 
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
		{
			++errors;
			msg_ELog (EF_PROBLEM,
				  "ds_GetFields(): field %d incorrect", 
				  fields[i]);
		}
	}
	/*
	 * Now try asking for fewer fields than are in the file, and make
	 * sure we aren't given more than that.
	 */
	nfield = 2;
	if (!ds_GetFields(dc->dc_Platform, start, &nfield, fields))
	{
		++errors;
		msg_ELog (EF_PROBLEM, 
			  "ds_GetFields('%s',nfld=2) failed", plat);
	}
	else if (nfield != 2)
	{
		++errors;
		msg_ELog (EF_PROBLEM, 
		  "ds_GetFields('%s',nfld=2) returned %d fields, expected %d",
		  plat, nfield, 2);
	}
	dc_DestroyDC(dc);
	return (errors);
}



int
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
	int errors = 0;

	sprintf(buf,"Testing transparent datachunks on '%s'",platform);
	Announce (buf);
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = NeedPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	dc_AddSample (dc, &when, data, strlen(data)+1);
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC(dc);

	/* Now get a much larger one and overwrite the first sample above */
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = NeedPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	errors += T_TransparentAdd (dc, &when, 100, TRUE, atts);
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	errors += !ds_Store (dc, TRUE, NULL, 0);
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC(dc);

	/* Now try it with some hints set */
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = NeedPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	dc_HintNSamples (dc, 25, FALSE);
	errors += T_TransparentAdd (dc, &when, 50, TRUE, atts);
	when.zt_Sec += 25;
	dc_HintNSamples (dc, 10, TRUE);
	errors += T_TransparentAdd (dc, &when, 10, TRUE, atts);
	when.zt_Sec += 10;
	dc_HintSampleSize (dc, 50, FALSE);
	dc_HintMoreSamples (dc, 50, FALSE);
	errors += T_TransparentAdd (dc, &when, 50, TRUE, atts);
	when.zt_Sec += 50;
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	return (errors);
}



int
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
	char *dsdump;
	char cmd[256];
	int errors = 0;

	Announce ("Testing deletion: creating 10 files in t_deletes");
	dc = T_SimpleScalarChunk (&begin, 1, 10, 4, TRUE, TRUE);
	t_delete_id = NeedPlatform("t_deletes");
	dc->dc_Platform = t_delete_id;
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dsdump = (char *)getenv("DSDUMP");
	if (! dsdump)
	{
		msg_ELog (EF_PROBLEM, 
			  "DSDUMP not defined; dumps not performed");
	}
	else
	{
		sprintf (cmd, "%s t_deletes > /dev/null 2>&1", dsdump);
		system(cmd);
		fflush (stdout);
		fflush (stderr);
	}

	msg_ELog (EF_DEBUG, "Deleting even-second obs with DeleteObs");
	when = begin;
	for (i = 0; i < 10; ++i)
	{
		if (when.zt_Sec % 2 == 0)
			ds_DeleteObs (t_delete_id, &when);
		++when.zt_Sec;
	}
	msg_ELog (EF_DEBUG, "Finished deleting with DeleteObs\n");
	if (dsdump)
	{
		fflush (stdout);
		fflush (stderr);
		system(cmd);	
		fflush (stdout);
		fflush (stderr);
	}
	msg_ELog (EF_DEBUG, "Trying to do the same deletes again");
	when = begin;
	for (i = 0; i < 10; ++i)
	{
		if (when.zt_Sec % 2 == 0)
			ds_DeleteObs (t_delete_id, &when);
		++when.zt_Sec;
	}
	msg_ELog (EF_DEBUG, "Storing the DataChunk again...");
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	msg_ELog (EF_DEBUG, "Test extremes: time earlier than all obs,");
	when.zt_Sec = begin.zt_Sec - 3600*24;
	ds_DeleteObs (t_delete_id, &when);
	msg_ELog (EF_DEBUG, "Time later than all obs,");
	when.zt_Sec = begin.zt_Sec + 3600*12;
	ds_DeleteObs (t_delete_id, &when);
	msg_ELog (EF_DEBUG, "Using DeleteObs to delete all files...");
	when = begin;
	for (i = 0; i < 10; ++i)
	{
		ds_DeleteObs (t_delete_id, &when);
		++when.zt_Sec;
	}
	if (dsdump)
	{
		fflush (stdout);
		fflush (stderr);
		system(cmd);	
		fflush (stdout);
		fflush (stderr);
	}
	msg_ELog (EF_DEBUG, "Done deletes test.");
	msg_ELog (EF_DEBUG, "Should be 0 files left in 't_deletes'.");
	dc_DestroyDC (dc);
	return (errors);
}



int
T_CompareData(src1, src2, size)
float *src1, *src2;
int size;
{
	int i;

	fflush (stdout);
	if (!src1 || !src2)
		return (1);
	for (i = 0; i < size; ++i)
		if (src1[i] != src2[i]) break;
	msg_ELog ((i < size) ? EF_PROBLEM : EF_DEVELOP, "  %s at %i",
		  (i < size) ? "failed" : "succeeded", i);
	return ((i < size) ? 1 : 0);
}



static void
T_DumpData (retrieve, n, len, fname) 
float *retrieve;
int n;
int len;
char *fname;
{
#ifndef dc_DumpDC	/* means we're not dumping data */
	int i, j;

	printf ("First %d '%s' values (len = %d):\n", n, fname, len);
	for (i = 0; (i < n) && (i < len); ++i)
		printf ("%9.2f%c", retrieve[i], (i+1)%7?' ':'\n');
	printf ("\nLast %d '%s' values (len = %d):\n", n, fname, len);
	for (i = 0, j = len-n+((n<=len)?0:(n-len));
	     (i < n) && (j < len); ++i, ++j)
		printf ("%9.2f%c", retrieve[j], (i+1)%7?' ':'\n');
	printf ("\n");
#endif
}



int
T_NSGetField(dc, field)
DataChunk *dc;
FieldId field;
{
	int i;
	char *names[ DC_MaxDimension ];
	unsigned long sizes[ DC_MaxDimension ];
	int rndim, rstatic;
	char buf[1024];
	
	/* test dc_NSGetField */
	(void)dc_NSGetField(dc, field, &rndim, names, sizes,
			    &rstatic);
	sprintf (buf, "dc_NSGetField(%s:id %i): %i dims, %s static, ( ",
		 F_GetName(field), field, rndim, (rstatic) ? "is" : "not");
	for (i = 0; i < rndim; ++i)
		sprintf (buf+strlen(buf), "%s=%lu ",names[i],sizes[i]);
	sprintf (buf+strlen(buf),")\n");
	msg_ELog (EF_DEVELOP, "%s", buf);
	return (0);
}



int
T_NSGetAllDimensions(dc)
DataChunk *dc;
{
	int i;
	char *names[ DC_MaxDimension ];
	unsigned long sizes[ DC_MaxDimension ];
	FieldId ids[ DC_MaxDimension + DC_MaxField ];
	int ndim;
	char *name;
	unsigned long size;

	ndim = dc_NSGetAllDimensions(dc, names, ids, sizes);
	msg_ELog (EF_DEVELOP,
		  "dc_NSGetAllDimensions() returns %i dimensions:", ndim);
	for (i = 0; i < ndim; ++i)
	{
		msg_ELog (EF_DEVELOP, "   %-30s %-5i %10lu",
			  names[i],ids[i],sizes[i]);
		dc_NSGetDimension (dc, ids[i], &name, &size);
		msg_ELog (EF_DEVELOP,
			  "   NSGetDimension(%s): id %i, size = %lu",
			  name, ids[i], size);
	}
	return (0);
}



int
T_NSGetAllVariables(dc)
DataChunk *dc;
{
	int i,j;
	FieldId ids[ DC_MaxDimension + DC_MaxField ];
	FieldId rids[ DC_MaxDimension + DC_MaxField ];
	int ndims[ DC_MaxDimension ];
	int nvar, rndim, rstatic;
	char buf[1024];

	nvar = dc_NSGetAllVariables(dc, ids, ndims);
	msg_ELog (EF_DEVELOP, "dc_NSGetAllVariables() returns %i variables:", 
		  nvar);
	for (i = 0; i < nvar; ++i)
	{
		msg_ELog (EF_DEVELOP, "   id: %-3i   ndims: %-5i", 
			  ids[i], ndims[i]);
		msg_ELog (EF_DEVELOP, "   NSIsStatic(): %s",
			  (dc_NSIsStatic(dc, ids[i])) ? "True" : "False");
		dc_NSGetVariable(dc, ids[i], &rndim, rids, &rstatic);
		msg_ELog (EF_DEVELOP, 
		  "   dc_NSGetVariable(%s: id %i): %i dims, %s static, ",
		  F_GetName(ids[i]), ids[i], rndim, (rstatic)?"is":"not");
		sprintf (buf, "  ( ");
		for (j = 0; j < rndim; ++j)
		{
			sprintf (buf+strlen(buf), " %s:id %i, ",
			       (rids[j] != BadField)?(F_GetName(rids[j])):
			       (""), rids[j]);
		}
		sprintf (buf+strlen(buf), ")");
		msg_ELog (EF_DEVELOP, "%s", buf);
	}
	return (0);
}


int
T_1DGridStoreBlocks()
{
	DataChunk *dc;
	PlatformId src_id, dest_id;
	ZebTime when;
	int nfield;
	FieldId fields[5];
	int err = 0;

	/*
	 * All we want to do is fetch a datachunk from some known
	 * 1DGrid platform and store the same datachunk using StoreBlocks
	 * in the 't_1dgrid' platform.
	 */
	src_id = NeedPlatform ("kapinga/prof915h");
	fields[0] = F_Lookup("height");
	fields[1] = F_Lookup("wspd");
	fields[2] = F_Lookup("wdir");
	fields[3] = F_Lookup("u_wind");
	fields[4] = F_Lookup("v_wind");
	nfield = 5;
	TC_ZtAssemble (&when, 92, 12, 2, 18, 47, 41, 0);
	Announce ("Fetching observation from kapinga/prof915h... ");
	dc = ds_FetchObs (src_id, DCC_RGrid, &when, fields, nfield, 0, 0);
	if (! dc)
		return (++err);
	msg_ELog (EF_DEBUG,
		  "PutBlock to 't_1dgrid_cdf' and 't_1dgrid_znf'... "); 
	fflush(stdout);
	dest_id = NeedPlatform ("t_1dgrid_cdf");
	dc->dc_Platform = dest_id;
	err += !ds_StoreBlocks (dc, TRUE, 0, 0);
	dest_id = NeedPlatform ("t_1dgrid_znf");
	dc->dc_Platform = dest_id;
	err += !ds_StoreBlocks (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	return (err);
}



int
T_IRGridStoreBlocks()
{
	DataChunk *dc;
	PlatformId src_id, dest_id;
	ZebTime begin, end;
	int nfield;
	FieldId fields[5];
	int err = 0;

	/*
	 * Fetch some IRGrid data and store it using StoreBlocks
	 */
	src_id = NeedPlatform ("t_mesonet");
	fields[0] = F_Lookup("pres");
	fields[1] = F_Lookup("cpres0");
	fields[2] = F_Lookup("tdry");
	fields[3] = F_Lookup("dp");
	nfield = 4;
	TC_ZtAssemble (&begin, 92, 3, 10, 0, 0, 0, 0);
	end = begin;
	end.zt_Sec += 3600*24;
	Announce ("Fetching one day from 't_mesonet'... ");
	dc = ds_Fetch (src_id, DCC_IRGrid, &begin, &end, 
		       fields, nfield, 0, 0);
	if (! dc)
		return (++err);
	dest_id = ds_LookupPlatform ("t_irgrid_cdf");
	if (dest_id != BadPlatform)
	{
		msg_ELog (EF_DEBUG, "PutBlock to 't_irgrid_cdf'...");
		fflush(stdout);
		dc->dc_Platform = dest_id;
		err += !ds_StoreBlocks (dc, TRUE, 0, 0);
	}
	dest_id = ds_LookupPlatform ("t_irgrid_znf");
	if (dest_id != BadPlatform)
	{
		msg_ELog (EF_DEBUG, "PutBlock to 't_irgrid_znf'..."); 
		fflush(stdout);
		dc->dc_Platform = dest_id;
		err += !ds_StoreBlocks (dc, TRUE, 0, 0);
	}
	dc_DestroyDC (dc);
	return (err);
}



int
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
	int err = 0;

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
	return (err);
}




#ifdef ZNF_TESTING
T_ZnfBlocks ()
{
	/* Call znf free block testing routine */
	Announce ("Testing ZNF low-level free blocks, file 'test.znf'");
	zn_TestFreeBlocks ("test.znf");
	fflush (stdout);
	fflush (stderr);
	return (0);
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
	int err = 0;

	Announce ("Testing a typed AERI NSpace DataChunk");
	dc = dc_CreateDC (DCC_NSpace);
	plat_id = NeedPlatform ("t_aeri_types_cdf");
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
	dc_NSDefineField (dc, process_id, 2, process_dims, process_sizes, 
			  TRUE);
	bin_avg_id = F_DeclareField ("bin_avg_rad", "Bin average radiance",
				     "mW/(m2 sr cm-1)");
	dc_NSDefineField (dc, bin_avg_id, 1, process_dims, process_sizes, 
			  FALSE);

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
	msg_ELog (EF_DEBUG, "Adding sample data...");
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
	msg_ELog (EF_DEBUG, "Storing and destroying.");
	err += !ds_StoreBlocks (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	msg_ELog (EF_DEBUG, "Fetching...");
	ds_GetFields (plat_id, &when, &nfield, fields);
	dc = ds_FetchObs (plat_id, DCC_NSpace, &begin, fields, nfield, 0, 0);
	if (!dc)
		++err;
	else
	{
		dc_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	msg_ELog (EF_DEBUG, "AERI typed NSpace test done.");
	free (mean_rads);
	return (0);
}
#endif /* AERI_TYPES */


#ifdef ATTRIBUTES

struct AttrDesc {
	enum { Global, Field, Sample } which;
	DataChunk *dc;
	int sample;
	FieldId field;
};



int
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



int
T_Attributes(when)
ZebTime when;
/*
 * Create a simple DataChunk and add/remove/dump its attributes
 */
{
	DataChunk *dc, *dc2;
	FieldId field;
	char *pname = "t_att_types_cdf";
	PlatformId plat = NeedPlatform (pname);
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
	unsigned char bytes[] = { 1, 3, 7, 15, 31, 63, 127, 255 };
	short sdata[] = { 512, 1024, 2048, 4096, 8192, 16384, 32767 };
	short *sget;
	char *cdata = "array of characters";
	char *cptr;
	struct AttrDesc ad;
	int nfield = 10;
	FieldId fields[10];
	char *dash = "=>=>=>=>=>=>";
	char diffs[2048];
	int errors = 0;
	char buf[1024];

	Announce ("Testing DataChunk attributes...");
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
	SEEN;
	/*
	 * Get the typed arrays added above
	 */
	sprintf (buf, "%s Double attribute: ", dash);
	dget = (double *)dc_GetGlobalAttrArray (dc, "global_doubles", 
						&type, &nval);
	for (i = 0; i < nval; ++i) sprintf (buf+strlen(buf), " %lf ", dget[i]);
	msg_ELog (EF_DEVELOP, "%s", buf);
	sprintf (buf, "%s Short attribute: ", dash);
	sget = (short *)dc_GetGlobalAttrArray (dc, "global_shorts", 
					       &type, &nval);
	for (i = 0; i < nval; ++i) sprintf (buf+strlen(buf), " %hd ", sget[i]);
	msg_ELog (EF_DEVELOP, "%s", buf);
	field = F_Lookup ("temp");
	dc_SetScalarFields (dc, 1, &field);
	dc_AddScalar (dc, &when, 0, field, &data);
	dc_SetFieldAttr (dc, field, "field_key", "field_value");
	dc_SetSampleAttr (dc, 0, "sample_key", "sample_value");
#ifndef EXAMPLE_ONLY
	msg_ELog (EF_DEVELOP, "%s dc should include non-string attributes:", 
		  dash);
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
		{
			msg_ELog (EF_PROBLEM, 
				  "Global att '%s' should equal '%s'", 
				  key, value);
			++errors;
		}
		dc_SetFieldAttr (dc, field, key, value);
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
		{
			msg_ELog (EF_PROBLEM, 
				  "Field att '%s' should equal '%s'", 
				  key, value);
			++errors;
		}
		sprintf (key, "sample_key_%d", i);
		dc_SetSampleAttr (dc, 0, key, value);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
		{
			msg_ELog (EF_PROBLEM,
				  "Sample att '%s' should equal '%s'", 
				  key, value);
			++errors;
		}
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
	msg_ELog (EF_DEBUG, "%s Storing datachunk to platform '%s'", 
		  dash, pname);
	errors += !ds_Store (dc, TRUE, 0, 0);
#ifndef EXAMPLE_ONLY
#ifdef DIFF_DATACHUNKS
	i = dc_CmpGlobalAttr (dc, dc, diffs, sizeof(diffs));
	msg_ELog (EF_DEBUG, "%d differences between same chunk", i);
	err += (i != 0);
	msg_ELog (EF_DEVELOP, "%s", diffs);
	/*
	 * A quick fetch to check the diffs before hacking away
	 */
	errors += !ds_GetFields (plat, &when, &nfield, fields);
	dc2 = ds_FetchObs (plat, DCC_Scalar, &when, fields, nfield, NULL, 0);
	if (! dc2)
		++err;
	else
	{
		i = dc_CmpGlobalAttr (dc, dc2, diffs, sizeof(diffs));
		msg_ELog (EF_DEBUG,
			  "%d differences between stored and fetched", i);
		msg_ELog (EF_DEBUG, "%s", diffs);
		dc_DestroyDC (dc2);
	}
#endif /* DIFF_DATACHUNKS */
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
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Global att '%s' should equal '%s'", 
				  key, value);
		}
		dc_SetFieldAttr (dc, field, key, value);
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Field att error: '%s' should equal '%s'", 
				  key, value);
		}
		sprintf (key, "sample_key_%d", i);
		dc_SetSampleAttr (dc, 0, key, value);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Sample att error: '%s' should equal '%s'", 
				  key, value);
		}
	}
	/*
	 * Now just verify all at once
	 */
	for (i = 0; i < 50; ++i)
	{
		sprintf (key, "key_%d", i);
		sprintf (value, "value_%d", i);
		if (strcmp(dc_GetGlobalAttr(dc, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Global att '%s' should equal '%s'", 
				  key, value);
		}
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Field att error: '%s' should equal '%s'", 
				  key, value);
		}
		sprintf (key, "sample_key_%d", i);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Sample att error: '%s' should equal '%s'", 
				  key, value);
		}
	}
	msg_ELog (EF_DEBUG, "%s Deleting the even keys...", dash);
	for (i = 0; i < 50; i += 2)
	{
		sprintf (key, "key_%d", i);
		dc_RemoveGlobalAttr (dc, key);
		dc_RemoveFieldAttr (dc, field, key);
		sprintf (key, "sample_key_%d", i);
		dc_RemoveSampleAttr (dc, 0, key);
	}
	msg_ELog (EF_DEBUG, "%s Making sure they're gone...", dash);
	for (i = 0; i < 50; i += 2)
	{
		sprintf (key, "key_%d", i);
		if (dc_GetFieldAttr (dc, field, key) != NULL)
		{
			++errors;
			msg_ELog (EF_PROBLEM,
				  "   field key '%s' still exists", key);
		}
		if (dc_GetGlobalAttr (dc, key) != NULL)
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "   global key '%s' still exists", key);
		}
		sprintf (key, "sample_key_%d", i);
		if (dc_GetSampleAttr (dc, 0, key) != NULL)
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "   sample key '%s' still exists", key);
		}
	}
	if (dc_GetNGlobalAttrs(dc) != 57 - 25)
	{
		++errors;
		msg_ELog (EF_PROBLEM, "%d global attrs should be %d",
			dc_GetNGlobalAttrs(dc), 57 - 25);
	}
	dc_DumpDC (dc);
	/*
	 * Test retrieval of an attribute list
	 */
	msg_ELog (EF_DEBUG, "%s retrieving a key/value list for field atts", 
		  dash);
	keys = dc_GetFieldAttrList(dc, field, NULL, &values, &natts);
	if (natts != dc_GetNFieldAttrs (dc, field))
	{
		++errors;
		msg_ELog (EF_PROBLEM, "getlist returns %d natts != getn %d",
			  natts, dc_GetNFieldAttrs (dc, field));
	}
	buf[0] = '\0';
	for (i = 0; i < natts; ++i)
	{
		dc_GetFieldAttrArray (dc, field, keys[i], &type, &nval);
		sprintf (buf+strlen(buf),
			 "   %s=%s:%d%c", keys[i], (type != DCT_String) ?
			 dc_ElemToString(values[i], type) : (char *)values[i],
			 nval, (i+1)%3 ? ' ':'\n');
	}
	msg_ELog (EF_DEVELOP, "%s", buf);
	msg_ELog (EF_DEVELOP, 
		  "%s keys matching the pattern 'key_.[12345]':", dash);
	keys = dc_GetFieldAttrList(dc, field, "key_.[12345]",
				   &values, &natts);
	buf[0] = '\0';
	for (i = 0; i < natts; ++i)
	{
		dc_GetFieldAttrArray (dc, field, keys[i], &type, &nval);
		sprintf (buf+strlen(buf),
			 "   %s=%s:%d%c", keys[i], (type != DCT_String) ?
			 dc_ElemToString(values[i], type) : (char *)values[i],
			 nval, (i+1)%3 ? ' ':'\n');
	}
	msg_ELog (EF_DEVELOP, "%s", buf);
	/*
	 * Get a keys list
	 */
	keys = dc_GetGlobalAttrKeys(dc, &natts);
	msg_ELog (EF_DEVELOP, 
		  "%s using dc_GetGlobalAttrKeys, %d atts, (getn=%d):", 
		  dash, natts, dc_GetNGlobalAttrs (dc));
	buf[0] = '\0';
	for (i = 0; i < natts; ++i)
		sprintf (buf+strlen(buf),
			 "   %s%c", keys[i], (i+1)%5 ? ' ':'\n');
	msg_ELog (EF_DEVELOP, "%s", buf);

	/*
	 * Now for a clincher: process each attribute list to remove each key
	 */
	msg_ELog (EF_DEBUG, "%s trying to remove all attributes...", dash);
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
	msg_ELog (EF_DEBUG,
		  "%s Attributes with zero values and empty strings:",
		  dash);
	dc_SetGlobalAttrArray (dc, "global_flag", 0, 0, NULL);
	dc_SetGlobalAttr (dc, "global_empty", "");
	dc_DumpDC (dc);
	msg_ELog (EF_DEBUG, "%s Deleting empty- and zero-valued global atts",
		  dash);
	ad.which = Global;
	dc_ProcessAttrArrays (dc, NULL, T_RemoveAttr, (void *)&ad);
	dc_DumpDC (dc);
	/*
	 * Fetch the previously stored file, hopefully including all of the
	 * typed attributes, and dump it
	 */
	msg_ELog (EF_DEBUG, "%s Fetching the attributes from '%s'", 
		  dash, pname);
	errors += !ds_GetFields (plat, &when, &nfield, fields);
	dc2 = ds_FetchObs (plat, DCC_Scalar, &when, 
			  fields, nfield, NULL, 0);
	if (!dc2)
		++errors;
	else
	{
		dc_DumpDC (dc2);
#ifdef DIFF_DATACHUNKS
		/*
		 * Diff the global attributes
		 */
		i = dc_CmpGlobalAttr (dc, dc2, diffs, sizeof(diffs));
		msg_ELog (EF_DEBUG, "CmpGlobalAttr reports %d differences", i);
		msg_ELog (EF_DEVELOP, "%s", diffs);
#endif
		dc_DestroyDC (dc);

		dc = dc2;
		/*
		 * Test that the character arrays came back as strings
		 */
		msg_ELog (EF_DEBUG,
			  "%s verifying char arrays converted to strings", 
			  dash);
		cptr = dc_GetGlobalAttr(dc, "global_char");
		if (!cptr || strcmp(cptr, cdata))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
			  "Global att error: '%s' should be string '%s'", 
			  "global_char", cdata);
		}
		cptr = dc_GetFieldAttr(dc, field, "field_char");
		if (!cptr || strcmp(cptr, cdata))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Field att '%s' should be string '%s'", 
				  "field_char", cdata);
		}
		dc_DestroyDC (dc);
	}
#else
	dc_DestroyDC (dc);
#endif /* ! EXAMPLE_ONLY */
	msg_ELog (EF_DEBUG, "T_Attributes done.");
	return (errors);
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
	int err = 0;

	dc = T_SimpleScalarChunk (begin, 1, 10, 4, FALSE, FALSE);
	dc->dc_Platform = pid;
	err += !ds_Store (dc, TRUE, details, ndetail);
	err += !ds_GetFields (pid, begin, &nfield, fields);
	ds_FloatDetail (DD_FETCH_BADVAL, dc_GetBadval (dc), fdets, nfdet++);
	ndc = ds_FetchObs (pid, DCC_Scalar, begin, fields, nfield, 
			   fdets, nfdet);
	if (!ndc)
		++err;
	else
	{
		ndc->dc_Platform = pid2;
		err += !ds_Store (ndc, TRUE, details, ndetail);
		dc_DestroyDC (ndc);
	}
	dc_DestroyDC (dc);
	return (err);
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
	msg_ELog (EF_DEBUG, "Testing dnc_TimeUnits()");
	u = units;
	while (*u)
	{
		result = dnc_TimeUnits (&zt, *u);
		msg_ELog (EF_DEVELOP, 
			  "'%s': %s", *u, (result) ? "true" : "false");
		if (result)
		{
			TC_EncodeTime (&zt, TC_FullUSec, buf);
			msg_ELog (EF_DEVELOP, "  %s", buf);
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
	msg_ELog (EF_DEBUG, "testing details of defining and fetching 'time'");
	zt = *begin;
	pid = NeedPlatform ("t_time_units");
	pid2 = NeedPlatform ("t_time_units_2");
	T_StoreTimes (&zt, pid, pid2, NULL, 0);

	zt.zt_Sec += 60;
	ds_SetDetail (DD_NC_TIME_LONG, details, 0);
	T_StoreTimes (&zt, pid, pid2, details, 1);

	zt.zt_Sec += 60;
	ds_SetDetail (DD_NC_ONE_TIME, details, 0);
	T_StoreTimes (&zt, pid, pid2, details, 1);

	zt.zt_Sec += 60;
	ds_SetDetail (DD_NC_TIME_FLOAT, details, 1);
	T_StoreTimes (&zt, pid, pid2, details, 2);

	zt.zt_Sec += 60;
	ds_SetDetail (DD_NC_TIME_LONG, details, 1);
	T_StoreTimes (&zt, pid, pid2, details, 2);
	return (0);
}	

#endif /* TIME_UNITS */

