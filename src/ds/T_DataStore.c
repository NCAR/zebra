
#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <assert.h>

#include <defs.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "DataChunkP.h"
#include "apple.h"



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



static int
T_DataTimes (when, platform)
ZebTime *when;
char *platform;
{
	DataChunk *dc;
	ZebTime now, next;
#	define NS (60*5)	/* 5 60-sample files */
	ZebTime first, last;
	PlatformId pid;
	int errors = 0;

	tl_Time (&first);
	first.zt_MicroSec = 0;
	first.zt_Sec -= (first.zt_Sec % 60) + NS;
	dc = T_SimpleScalarChunk (&first, 1, NS, 4, FALSE, FALSE);
	pid = NeedPlatform (platform);
	/*
	 * Make sure there are no residual files from previous tests
	 * prior to our 'first' time.  Later times will be overwritten
	 * if necessary. XXX This won't fix the problem in standalone
	 * tests, since data deletion is not implemented there.
	 */
	ds_DeleteData (pid, &first);
	dc->dc_Platform = pid;
	dc_GetTime (dc, NS - 1, &last);
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
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
	return (errors);
}



static int
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



static int
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
	ZebTime when, begin;
	int i;
	char *dsdump;
	char cmd[256];
	int errors = 0;

	begin = *start;
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
	msg_ELog (EF_DEBUG, "Finished deleting with DeleteObs");
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
	{
		ZebTime now;
		tl_Time (&now);
		if (ds_DataTimes (t_delete_id, &now, 1, DsBefore, &when) != 0)
			++errors;
	}
	return (errors);
}




static int
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
	else if (nfield < dc_nfield)
	{
		++errors;
		msg_ELog (EF_PROBLEM, 
		  "ds_GetFields('%s',nfld=10) returned %d fields, expected %d",
		  plat, nfield, dc_nfield);
	}
	else
	{
		for (i = 0; i < dc_nfield; ++i)
		{
			for (j = 0; j < nfield; ++j)
				if (fields[j] == dc_fields[i])
					break;
			if (j >= nfield)
				break;
		}
		if (i < dc_nfield)
		{
			++errors;
			msg_ELog (EF_PROBLEM,
				  "ds_GetFields(): field %d of %d incorrect", 
				  i, dc_nfield);
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




/* Define fields on a platform and see that they are returned. */
static int
T_ClassFields(ZebTime *start, char *plat)
{
	FieldId cfields[10];
	int ncfields = 0;
	FieldId fields[20];
	const FieldId *fieldp;
	FieldId *dc_fields;
	DataChunk *dc;
	DataChunk *fdc;
	int dc_nfield;
	int nfield, i, j;
	char buf[128];
	int errors = 0;
	PlatClassRef pc;
	PlatClassId cid;
	PlatformId pid;

	sprintf (buf, "Testing class fields for platform '%s'", plat);
	Announce (buf);

	/* Verify that we can get class fields from the daemon
	 * for a permanent platform.
	 */
	if ((pid = ds_LookupPlatform ("surface")) == BadPlatform)
	{
	    ++errors;
	    msg_ELog (EF_PROBLEM, "expected platform 'surface' to be defined");
	}
	else
	{
	    FieldId idmr = F_Lookup ("mr[w][%][Mixing Ratio]");
	    /* Get its fields */
	    fieldp = ds_PlatformClassFields (pid, &nfield);
	    if (nfield != 1 || idmr != fieldp[0])
	    {
		++errors;
		msg_ELog (EF_PROBLEM, "did not find class field '%s'",
			  F_GetFullName (idmr));
	    }
	}

	/* This one will override a field from the file. */
	ncfields = 0;
	cfields[ncfields++] = F_Lookup ("scalar[][degC][Basic Field]");
	cfields[ncfields++] = F_Lookup ("tdry[T][degC][Temperature]");
	cfields[ncfields++] = F_Lookup ("tbot[T][degC][Bottom Temperature]");
	cfields[ncfields++] = 
	    F_Lookup("ir[][kilowatt m-2][Infrared Radiation]");

	if ((cid = ds_LookupClass (plat)) == BadClass)
	{
	    pc = ds_NewClass (plat);
	    msg_ELog (EF_DEBUG, "defining platform class '%s'", plat);
	    ds_AssignClass (pc, OrgScalar, FTNetCDF, FALSE);
	    ds_SetMaxSample (pc, 1024);
	    ds_SetComment (pc, 
			   "automatically defined class for T_ClassFields");
	    for (i = 0; i < ncfields; ++i)
	    {
		char buf[256];
		ds_AddClassField (pc, cfields[i]);
		/* Add the derivation alias as well */
		sprintf (buf, "%s = %s;", F_GetFullName(cfields[i]),
			 F_GetName (cfields[i]));
		ds_AddClassDerivation (pc, buf);
	    }
	    cid = ds_DefineClass (pc);
	}
	if ((pid = ds_LookupPlatform (plat)) == BadPlatform)
	{
	    pid = ds_DefinePlatform (cid, plat);
	}

	/* Before data are stored, no fields should be returned. */
	CleanPlatform (pid);
	nfield = 20;
	if (ds_GetFields(pid, start, &nfield, fields) != 0
	    || nfield != 0)
	{
		++errors;
		msg_ELog (EF_PROBLEM, 
			  "ds_GetFields('%s',nfld=10) %s", plat,
			  "should have returned false with no data");
	}

	/* Now store data to this platform with 4 fields. */
	dc = T_SimpleScalarChunk (start, 1, 10, 4, FALSE, FALSE);
	dc->dc_Platform = pid;
	dc_fields = dc_GetFields (dc, &dc_nfield);
	ds_Store (dc, TRUE, NULL, 0);
	nfield = 20;
	if (!ds_GetFields(dc->dc_Platform, start, &nfield, fields))
	{
	    ++errors;
	    msg_ELog (EF_PROBLEM, 
		      "ds_GetFields('%s',nfld=20) failed", plat);
	}
	else
	{
	    for (i = 0; i < dc_nfield; ++i)
	    {
		for (j = 0; j < nfield; ++j)
		{
		    if (fields[j] == dc_fields[i])
			break;
		}
		if (strcmp(F_GetName(dc_fields[i]),"scalar") == 0)
		{
		    if (j < nfield)
		    {
			++errors;
			msg_ELog (EF_PROBLEM,
				  "%s not overridden by class field", 
				  F_GetFullName(dc_fields[i]));
		    }
		}
		else if (j >= nfield)
		{
		    ++errors;
		    msg_ELog (EF_PROBLEM,
			      "ds_GetFields(): field %s not found", 
			      F_GetName(dc_fields[i]));
		}
	    }
	    /* Now make sure the class fields made it into the list */
	    for (i = 0; i < ncfields; ++i)
	    {
		for (j = 0; j < nfield; ++j)
		    if (fields[j] == cfields[i])
			break;
		if (j >= nfield)
		{
		    ++errors;
		    msg_ELog (EF_PROBLEM, 
			      "class field %s not found",
			      F_GetFullName (cfields[i]));
		}
	    }
	}
	/* Now fetch the data and make sure we get data for the class field.
	 */
	{
	    ZebraTime end;
	    dc_GetTime (dc, dc_GetNSample(dc)-1, &end);
	    fdc = ds_Fetch (pid, DCC_Scalar, start, &end, 
			    fields, nfield, NULL, 0);
	}
	if (! fdc || dc_GetNSample (fdc) == 0 || 
	    dc_GetNSample(dc) != dc_GetNSample (fdc))
	{
	    ++errors;
	    msg_ELog (EF_PROBLEM, "classfields: error fetching data");
	}
	else
	{
	    for (i = 0; i < dc_GetNSample (fdc); ++i)
	    {
		float here = dc_GetScalar(dc, i, dc_fields[0]);
		float there = dc_GetScalar(fdc,i, cfields[0]);
		if (here != there)
		{
		    ++errors;
		    msg_ELog (EF_PROBLEM, "%s: %g != %g",
			      "fetched class field mismatch",
			      here, there);
		}
	    }
	}
	dc_DestroyDC(fdc);
	dc_DestroyDC(dc);
	return (errors);
}





TestRoutine DataStoreTests[] = 
{
	{ "fetchgapcdf", FTNetCDF, DCC_None, TR_PLAT, T_FetchGap,
	  "verify no data is fetched between sample times for netcdf",
          "t_gap_cdf" },
	{ "fetchgapznf", FTZebra, DCC_None, TR_PLAT, T_FetchGap,
	  "verify no data is fetched between sample times for znf",
          "t_gap_znf" },
	{ "classfields", FTNetCDF, DCC_None, TR_PLAT, T_ClassFields,
	  "ds_GetFields on platform with class fields", "t_classfields" },
	{ "getfieldscdf", FTNetCDF, DCC_None, TR_PLAT, T_GetFields,
	  "ds_GetFields on netcdf interface", "t_getfields_cdf" },
	{ "getfieldsznf", FTZebra, DCC_None, TR_PLAT, T_GetFields,
	  "ds_GetFields on znf interface", "t_getfields_znf" },
	{ "datatimescdf", FTNetCDF, DCC_None, TR_PLAT, T_DataTimes,
	  "ds_DataTimes", "t_dummy_cdf" },
	{ "datatimesznf", FTZebra, DCC_None, TR_PLAT, T_DataTimes,
	  "ds_DataTimes", "t_dummy_znf" },
	{ "deletes", FTUnknown, DCC_None, TR_BEGIN, T_Deletes,
	  "ds_DeleteObs" },
	END_TESTS
};


