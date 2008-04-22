

#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "dsPrivate.h"
#include "GetList.h"
#include "dslib.h"
#include "dfa.h"
#include "apple.h"


struct TestPlatform cdfPlats[] = 
{
	{ "t_scalar_cdf", FTNetCDF, OrgScalar, 1000, TRUE },
	{ "t_fixed_cdf", FTNetCDF, OrgScalar, 1000, TRUE }
};


struct TestPlatform znfPlats[] = 
{
	{ "t_scalar", FTZebra, OrgScalar, 1000, TRUE },
	{ "t_fixed", FTZebra, OrgFixedScalar, 1000, TRUE }
};



static int
T_Scalar (begin, plats)
ZebTime *begin;
struct TestPlatform *plats;
{
	DataChunk *dc;
	ZebTime when, check;
	PlatformId plat_id, fixed_id;
	int err = 0;

	when = *begin;
	msg_ELog (EF_DEBUG, 
		  "t_scalar: 4 fields, 10 samples, mobile, attributes");
	dc = T_SimpleScalarChunk (&when, 1, 10, 4, TRUE, TRUE);
	plat_id = NewPlatform(&plats[0]);
	dc->dc_Platform = plat_id;
	ds_RequestNotify (plat_id, 0, T_ReceiveNotify);
	T_ExpectNotify (plat_id, NULL, 10, UpdAppend);
	err += ! ds_StoreBlocks (dc, TRUE, NULL, 0);
	err += T_CatchNotify ();
	dc_DestroyDC (dc);
	err += T_VerifyObs (plat_id, &when, NULL, 10);

	/*
	 * Now really put it through the ringer.  Start 30 seconds
	 * back so that we precede the previous data, then overwrite
	 * it, and finally append the rest.  Note that the sample
	 * attributes should be deleted on the overwrite, since they 
	 * won't correspond to the samples with atts here.
	 */
	msg_ELog (EF_DEBUG, 
		  "t_scalar: 4 fields, 3000 samples, starting 30 secs back");
	check = when;
	when.zt_Sec -= 30;
	dc = T_SimpleScalarChunk (&when, 1, 3000, 4, TRUE, TRUE);
	dc->dc_Platform = plat_id;
	T_ExpectNotify (plat_id, NULL, 30, UpdInsert);
	T_ExpectNotify (plat_id, NULL, 10, UpdOverwrite);
	T_ExpectNotify (plat_id, NULL, 990, UpdAppend);
	T_ExpectNotify (plat_id, NULL, 1000, UpdAppend);
	T_ExpectNotify (plat_id, NULL, 970, UpdAppend);
	err += !TP_Store (dc, TRUE, NULL, 0);
	err += T_CatchNotify ();
	err += T_CatchNotify ();
	err += T_CatchNotify ();
	err += T_CatchNotify ();
	err += T_CatchNotify ();
	ds_CancelNotify ();
	err += TX_ClearAll (EF_PROBLEM);
	err += T_VerifyObs (plat_id, &when, NULL, 30);
	err += T_VerifyObs (plat_id, &check, NULL, 1000);
	check.zt_Sec += 1000;
	err += T_VerifyObs (plat_id, &check, NULL, 1000);
	check.zt_Sec += 1000;
	err += T_VerifyObs (plat_id, &check, NULL, 970);
	msg_ELog (EF_DEBUG, "t_fixed: storing same datachunk as for t_scalar");
	dc->dc_Platform = fixed_id = NewPlatform(&plats[1]);
	err += !TP_Store (dc, TRUE, NULL, 0);
	dc_GetTime (dc, 0, &check);
	dc_DestroyDC (dc);
	err += T_VerifyObs (fixed_id, &check, NULL, 1000);
	check.zt_Sec += 1000;
	err += T_VerifyObs (fixed_id, &check, NULL, 1000);
	check.zt_Sec += 1000;
	err += T_VerifyObs (fixed_id, &check, NULL, 1000);
	when.zt_Sec += 3000;

	/*
	 * Now overwrite a huge block of the previously stored data.
	 */
	msg_ELog (EF_DEBUG, 
		  "t_scalar: 4 fields, 1500 samples, 1500 secs prior to end");
	when.zt_Sec -= 1500;
	dc = T_SimpleScalarChunk (&when, 1, 1500, 4, TRUE, TRUE);
	dc->dc_Platform = plat_id;
	err += !TP_Store (dc, TRUE, NULL, 0);
	check.zt_Sec = when.zt_Sec - 1500;;
	err += T_VerifyObs (plat_id, &check, NULL, 30);
	check.zt_Sec += 30;
	err += T_VerifyObs (plat_id, &check, NULL, 1000);
	check.zt_Sec += 1000;
	err += T_VerifyObs (plat_id, &check, NULL, 1000);
	check.zt_Sec += 1000;
	err += T_VerifyObs (plat_id, &check, NULL, 970);
	msg_ELog (EF_DEBUG, "t_fixed: same datachunk as for t_scalar above");
	dc->dc_Platform = fixed_id;
	err += !TP_Store (dc, TRUE, NULL, 0);
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
		TestField tf;
		tf = TestFields[1];
		TestFields[1] = TestFields[3];
		TestFields[3] = tf;
	}
	err += !TP_Store (dc, TRUE, NULL, 0);
	msg_ELog (EF_DEBUG, "t_fixed: same datachunk as for t_scalar above");
	dc->dc_Platform = fixed_id;
	err += !TP_Store (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	when.zt_Sec += 1000;
	return (err);
}




static void
T_AddTypedScalarSample (dc, when, sample, fields)
DataChunk *dc;
ZebTime when;
int sample;
FieldId *fields;
/*
 * Make the values dependent upon the sample number so we can verify
 * them later.
 */
{
	char cvalue;
	int ivalue;
	long lvalue;
	double dvalue;
	short svalue;
	float fvalue;

#define FVAL(sample) (-1.1 - ((float)(sample) * 2.1))
#define DVAL(sample) (pow ((double)1.234567901234, (double) (sample)))
#define CVAL(sample) ('A' + (int)(sample))
#define SVAL(sample) (1 << (sample))
#define IVAL(sample) (1024 + (2*(sample)))
#define LVAL(sample) ((1 << 30) + (2*(sample)))

	fvalue = FVAL(sample);
	dc_AddScalar (dc, &when, sample, fields[DCT_Float-1], &fvalue);

	dvalue = DVAL(sample);
	dc_AddScalar (dc, &when, sample, fields[DCT_Double-1], &dvalue);

	cvalue = CVAL(sample);
	dc_AddScalar (dc, &when, sample, fields[DCT_Char-1], &cvalue);
	dc_AddScalar (dc, &when, sample, fields[DCT_UnsignedChar-1], &cvalue);

	svalue = SVAL(sample);
	dc_AddScalar (dc, &when, sample, fields[DCT_ShortInt-1], &svalue);
	dc_AddScalar (dc, &when, sample, fields[DCT_UnsignedShort-1], &svalue);

	ivalue = IVAL(sample);
	dc_AddScalar (dc, &when, sample, fields[DCT_Integer-1], &ivalue);
	dc_AddScalar (dc, &when, sample, fields[DCT_UnsignedInt-1], &ivalue);

	lvalue = LVAL(sample);
	dc_AddScalar (dc, &when, sample, fields[DCT_LongInt-1], &lvalue);
	dc_AddScalar (dc, &when, sample, fields[DCT_UnsignedLong-1], &lvalue);
}



static int
Equal (f1, f2)
float f1, f2;
{
	if (((double)fabs((double)(f1 - f2)) < (double)0.001))
		return (1);
	msg_ELog (EF_PROBLEM, "comparing values: %g != %g", f1, f2);
	return (0);
}



static int
T_VerifyTypedSample (dc, sample, fields)
DataChunk *dc;
int sample;
FieldId *fields;
{
	int errors = 0;
	float value;

	value = dc_GetScalar (dc, sample, fields[DCT_Float-1]);
	errors += ! Equal (value, (float)FVAL(sample));

	value = dc_GetScalar (dc, sample, fields[DCT_Double-1]);
	errors += ! Equal (value, (float)DVAL(sample));

	value = dc_GetScalar (dc, sample, fields[DCT_Char-1]);
	errors += ! Equal (value, (float)CVAL(sample));
	value = dc_GetScalar (dc, sample, fields[DCT_UnsignedChar-1]);
	errors += ! Equal (value, (float)CVAL(sample));

	value = dc_GetScalar (dc, sample, fields[DCT_ShortInt-1]);
	errors += ! Equal (value, (float)SVAL(sample));
	value = dc_GetScalar (dc, sample, fields[DCT_UnsignedShort-1]);
	errors += ! Equal (value, (float)SVAL(sample));

	value = dc_GetScalar (dc, sample, fields[DCT_Integer-1]);
	errors += ! Equal (value, (float)IVAL(sample));
	value = dc_GetScalar (dc, sample, fields[DCT_UnsignedInt-1]);
	errors += ! Equal (value, (float)IVAL(sample));
	value = dc_GetScalar (dc, sample, fields[DCT_LongInt-1]);
	errors += ! Equal (value, (float)LVAL(sample));
	value = dc_GetScalar (dc, sample, fields[DCT_UnsignedLong-1]);
	errors += ! Equal (value, (float)LVAL(sample));

	return (errors);
}




static int
T_FieldTypes (when)
ZebTime *when;
/*
 * Test definition of DataChunks with multiple field types.  Just define
 * the chunk as usual, change the field type, and dump the DC.  
 * We cheat and access the global sizes and names table to make
 * this easier.
 */
{
	FieldId fields[20];
	DC_ElemType types[20], e;
	int nfield = DCT_String - DCT_Float;/* exclude String and Unknown */
	int i, f;
	DataChunk *dc;
	ZebTime begin;
	static Location loc = { 40.0, -160.0, 5280.0 };
	PlatformId plat_id = NeedPlatform ("t_fieldtypes");
	int err = 0;

	begin = *when;
	Announce ("Testing field types interfaces");
	for (e = DCT_Float; e < DCT_String; ++e)
	{
		char buf[32];
		char *c;

		msg_ELog (EF_DEVELOP, "type name: %s, size %d", 
			  dc_TypeName(e), dc_SizeOfType(e));
		c = buf;
		strcpy (c, dc_TypeName(e));
		while ((c = strchr (c, ' ')) != NULL)
			*c++ = '_';
		fields[e-1] = F_DeclareField (buf, "Typed field","none");
		types[e-1] = e;
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
	T_AddTypedScalarSample (dc, *when, 0, fields);
	msg_ELog (EF_DEBUG, 
		  "nsamples hint should be 10, with 10 samples allocated");
	{
		int hns, nsa, ns;
		T_TrHints (dc, &ns, &nsa, &hns, 0, 0);
		if (ns != 1 || nsa != 10 || hns != 10)
		{
			++err;
			msg_ELog (EF_PROBLEM, "wrong transparent stats");
		}
	}
	T_DumpDC (dc);
	++when->zt_Sec;
	T_AddTypedScalarSample (dc, *when, 1, fields);
	msg_ELog (EF_DEBUG, "nsamples allocated should be 10, with");
	msg_ELog (EF_DEBUG, 
		  "space for 8 samples between NextOffset and DataLen:");
	{
		int hns, nsa, ns, hss, no;
		T_TrHints (dc, &ns, &nsa, &hns, &hss, &no);
		if (ns != 2 || nsa != 10)
		{
			++err;
			msg_ELog (EF_PROBLEM, "nsamples allocated %d not 10", 
				  nsa);
		}
		if (no + (8 * hss) != dc->dc_DataLen)
		{
			++err;
			msg_ELog (EF_PROBLEM, "incorrect buffer space");
		}
	}
	T_DumpDC (dc);
	++when->zt_Sec;
	for (i = 2; i < 10; ++i, ++when->zt_Sec)
		T_AddTypedScalarSample (dc, *when, i, fields);
	T_DumpDC (dc);
	fflush (stdout);

	msg_ELog (EF_DEBUG, "Verifying non-floats as floats:");
	for (i = 0; i < 10; ++i)
		err += T_VerifyTypedSample (dc, i, fields);
	for (f = 0; (Verbose) && (f < nfield); ++f)
	{
		printf ("%s: ", F_GetName (fields[f]));
		for (i = 0; i < 10; ++i)
		{
			printf ("%g, ", dc_GetScalar (dc, i, fields[f]));
		}
		printf ("\n");
	}
	
	msg_ELog (EF_DEBUG, 
		  "ds_Store()'ing the typed DataChunk to t_fieldtypes");
	TX_ExpectMany (EF_ALL, TRUE, 2, 
		       "Unable to convert long datachunk type.*to netcdf");
	err += !ds_Store (dc, TRUE, 0, 0);
	TX_Seen();
	dc_DestroyDC (dc);
	msg_ELog (EF_DEBUG, "Fetching the typed scalar observation");
	dc = ds_FetchObs (plat_id, DCC_Scalar, &begin, fields, nfield, 0, 0);
	if (dc)
	{
		msg_ELog (EF_DEBUG, "Verifying non-floats as floats:");
		for (i = 0; i < 10; ++i)
			err += T_VerifyTypedSample (dc, i, fields);
		T_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	else
		++err;
	return (err);
}




/*
 * Functions for copying attributes between datachunks
 */
static FieldId DestFID;
static DataChunk *DestDC;


static int
CopyGlobalAtts (key, value)
char *key;
char *value;
{
	dc_SetGlobalAttr (DestDC, key, value);
	return (0);
}


static int
CopyFieldAtts (key, value)
char *key;
char *value;
{
	dc_SetFieldAttr (DestDC, DestFID, key, value);
	return (0);
}


static struct TestPlatform copyPlats[] =
{
	{ "t-copy-source", FTZeb, OrgScalar, 1000, TRUE },
	{ "t-copy-dest", FTNetCDF, OrgScalar, 1000, TRUE }
};


static int
T_CopyScalar (begin)
ZebTime *begin;
/*
 * Store lots of scalar samples with lots of fields, then fetch it back
 * and try to copy the fetched datachunk into another datachunk with
 * one new field.
 */
{
	DataChunk *dc, *newdc;
	char *pname = copyPlats[0].name;
	char *pdest = copyPlats[1].name;
	PlatformId plat_id;
	FieldId fields[40];
	int i, n, fld;
	float value;
	Location loc;
	ZebTime when;
	dsDetail details[5];
	int ndetail;
	char buf[128];
	int err = 0;

	when = *begin;
	Announce ("Copying one datachunk to another, adding one field");
	plat_id = NewPlatform(copyPlats);
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
	T_DumpDC (dc);

	/* now create our second dc, and go through the hassle of adding
	 * that one more field to it and copying attributes and data */
	msg_ELog (EF_DEBUG, 
		  "creating our copy dc for '%s', adding field 'avg'", 
		  pdest);
	newdc = dc_CreateDC (DCC_Scalar);
	newdc->dc_Platform = NewPlatform (copyPlats+1);

	fields[n] = F_Lookup("avg"); ++n;
	dc_SetScalarFields (newdc, n, fields);
	dc_SetBadval (newdc, 999.9);
	DestDC = newdc;
	dc_ProcessAttrs (dc, NULL, CopyGlobalAtts);
	for (fld = 0; fld < n - 1; ++fld)
	{
		DestFID = fields[fld];
		dc_ProcessFieldAttrs (dc, fields[fld], NULL, CopyFieldAtts);
	}

	/*
	 * Last but not least, copy all of the data, including locations.
	 */
	T_DumpDC (newdc);
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
	T_DumpDC (newdc);
	msg_ELog (EF_DEBUG, "storing the new datachunk to '%s'", pdest);
	err += !ds_StoreBlocks (newdc, TRUE, details, ndetail);
	dc_DestroyDC(newdc);
	dc_DestroyDC(dc);
	msg_ELog (EF_DEBUG, "Done with DataChunk copy test.");
	return (err);
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
	TestField *tf;
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


static struct TestPlatform microPlat = 
{ "t_micro", FTNetCDF, OrgScalar, 100, FALSE };


static int
CheckTimes (when, base, pid, fid)
ZebTime *when;
int base;
PlatformId pid;
FieldId fid;
{
	ZebTime zt;
	int maxs = microPlat.maxsamples;
	int errors = 0;
	int i;
	float ms;
	DataChunk *dc;

	zt = *when;
	zt.zt_MicroSec = base;
	for (i = 0; i < maxs; ++i, ++zt.zt_MicroSec)
	{
		dc = ds_Fetch (pid, DCC_Scalar, &zt, &zt, &fid, 1, NULL, 0);
		if (! dc)
		{
			++errors;
			msg_ELog (EF_PROBLEM, "sample %d: not fetched", i);
			continue;
		}
		ms = dc_GetScalar (dc, 0, fid);	/* implicit conversion */
		if (ms != (float)zt.zt_MicroSec)
		{
			++errors;
			msg_ELog (EF_PROBLEM, "sample %d: %s %f != %s %d",
				  i, "fetched value", ms, "sample microsec",
				  zt.zt_MicroSec);
		}
		dc_DestroyDC (dc);
	}
	return (errors);
}



static int
TestMicroSec (when, base, details, ndetail)
ZebTime *when;
int base;	/* base time for microsecond offsets */
dsDetail *details;
int ndetail;	/* details to pass to storage to set time variables */
/*
 * Create a datachunk with samples at microsecond resolution.  
 * Store it, run datatimes for the period, and compare the times we
 * receive back.  Then fetch a few of the returned times and verify
 * that we get a single sample with the correct data value.  Test
 * field-specific bad values while we're at it.
 */
{
	int errors = 0;
	PlatformId pid;
	DataChunk *dc;
	FieldId fid = F_DeclareField("microsecs", "Microsecond samples", "ms");
	Location loc;
	ZebTime zt;
	int maxs = microPlat.maxsamples;
	int badval;
	int i;

	loc.l_lat = 40.0;
	loc.l_lon = -180.0;
	loc.l_alt = 5280.0;
	pid = MakePlatform (&microPlat);
	dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = pid;
	dc_SetScalarFields (dc, 1, &fid);
	dc_SetType (dc, fid, DCT_Integer);
	badval = -1;
	dc_SetGlobalBadval (dc, DCT_Integer, &badval);
	badval = 0;
	dc_SetFieldBadval (dc, fid, &badval);
	dc_SetStaticLoc (dc, &loc);

	zt = *when;
	zt.zt_MicroSec = base;	/* why start at zero? */
	for (i = 0; i < maxs; ++i)
	{
		dc_AddScalar (dc, &zt, i, fid, &zt.zt_MicroSec);
		zt.zt_MicroSec++;
	}

	/*
	 * Now we can store it to a new file
	 */
	errors += ! T_Store (dc, TRUE, details, ndetail);
	dc_DestroyDC (dc);

	/*
	 * Fetch single samples, use dc_GetScalar to convert
	 * the stored int to a float, and test against the microseconds
	 * of the sample time.
	 */
	msg_ELog (EF_DEBUG, "checking times while file still open...");
	errors += CheckTimes (when, base, pid, fid);

	dfa_ForceClosure ();	/* erase all memory of the file we wrote */
	msg_ELog (EF_DEBUG, "checking times after file has been closed...");
	errors += CheckTimes (when, base, pid, fid);

	return (errors);
}


/*
 * Try the above test for each of the netcdf time unit and type details.
 */
static int
T_MicroSec (when)
ZebTime *when;
{
	int maxs = microPlat.maxsamples;
	dsDetail details[5];
	PlatformId pid;
	int errors = 0;
	ZebTime *times = NULL;
	ZebTime zt;
	DataChunk *dc;
	FieldId fid;
	int nsamp;
	int n;

	zt = *when;
	pid = MakePlatform (&microPlat);
	CleanPlatform (pid);
	zt.zt_MicroSec = 0;

	msg_ELog (EF_DEBUG, "testing with default netcdf time variables:");
	errors += TestMicroSec (&zt, 0, NULL, 0);
	nsamp = maxs;

	msg_ELog (EF_DEBUG, "testing with single double 'time' variable:");
	ds_SetDetail (DD_NC_ONE_TIME, details, 0);
	errors += TestMicroSec (&zt, nsamp, details, 1);
	nsamp += maxs;

	msg_ELog (EF_DEBUG, "testing with single float 'time' variable:");
	ds_SetDetail (DD_NC_TIME_FLOAT, details, 1);
	errors += TestMicroSec (&zt, nsamp, details, 2);
	nsamp += maxs;

	msg_ELog (EF_DEBUG, "Microsecond access across multiple files:");
	/* dfa_ForceClosure (); */
	times = (ZebTime *) malloc (nsamp * sizeof(ZebTime));
	n = ds_DataTimes (pid, &zt, nsamp, DsAfter, times);
	fid = F_Lookup ("microsecs");
	dc = NULL;
	if (n != nsamp)
	{
		msg_ELog (EF_PROBLEM, "expected %d from DataTimes, not %d",
			  nsamp, n);
		++errors;
	}
	else if (! (dc = ds_Fetch (pid, DCC_Scalar, times+(nsamp-1), times,
				   &fid, 1, NULL, 0)))
	{
		msg_ELog (EF_PROBLEM, "fetch of all times failed");
		++errors;
	}
	else if (dc_GetNSample (dc) != nsamp)
	{
		++errors;
		msg_ELog (EF_PROBLEM, "fetched dc has %d samples, not %d",
			  dc_GetNSample (dc), nsamp);
	}
	else
	{
		int i;

		msg_ELog (EF_DEBUG, "comparing time and value of samples:");
		for (i = 0; i < nsamp; ++i)
		{
			float ms = dc_GetScalar (dc, i, fid);
			dc_GetTime (dc, i, &zt);
			if (! TC_Eq (zt, times[nsamp - i - 1]))
			{
				++errors;
				msg_ELog (EF_PROBLEM, "sample %d: %s",
					  i, "incorrect time");
			}
			if (ms != (float)zt.zt_MicroSec)
			{
				++errors;
				msg_ELog (EF_PROBLEM,
					  "sample %d: %s %f != %s %d", i,
					  "fetched value", ms,
					  "sample microsec", zt.zt_MicroSec);
			}
		}
	}
	if (dc)
		dc_DestroyDC (dc);
	free (times);
	return (errors);
}




TestRoutine ScalarTests[] = 
{
	{ "scalarcdf", FTNetCDF, DCC_Scalar, TR_PLAT, T_Scalar,
	  "generic scalar interface with netcdf files", (char *)cdfPlats },
	{ "scalarznf", FTZebra, DCC_Scalar, TR_PLAT, T_Scalar,
	  "generic scalar interface with zebra files", (char *)znfPlats },
	{ "fieldtypes", FTNetCDF, DCC_Scalar, TR_BEGIN, T_FieldTypes,
	  "scalar field types and transparent optimization checks" },
	{ "copyscalar", FTUnknown, DCC_Scalar, TR_BEGIN, T_CopyScalar,
	  "store, fetch, copy, and re-store a scalar datachunk" },
	{ "microsec", FTNetCDF, DCC_Scalar, TR_BEGIN, T_MicroSec,
	  "store and compare sample times with microsecond resolution" },
	END_TESTS
};

