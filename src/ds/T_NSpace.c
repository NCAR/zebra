
#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <assert.h>

#include <defs.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "DataChunkP.h"
#include "dsPrivate.h"
#include "dslib.h"
#include "dfa.h"
#include "apple.h"

static int
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
	sprintf (buf, "dc_NSGetField(%s): %i dims, %s static, ( ",
		 F_GetName(field), rndim, (rstatic) ? "is" : "not");
	for (i = 0; i < rndim; ++i)
		sprintf (buf+strlen(buf), "%s=%lu ",names[i],sizes[i]);
	sprintf (buf+strlen(buf),")\n");
	msg_ELog (EF_DEVELOP, "%s", buf);
	return (0);
}



static int
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



static int
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
			sprintf (buf+strlen(buf), " %s, ",
			       (rids[j] != BadField)?(F_GetName(rids[j])):
			       ("unknown"));
		}
		sprintf (buf+strlen(buf), ")");
		msg_ELog (EF_DEVELOP, "%s", buf);
	}
	return (0);
}



static int
T_NSpace_1 (now)
ZebTime *now;
{
	PlatformId plat_id;
	DataChunk *dc;
	static Location loc = { 40.0, -160.0, 5280.0 };
	int errors = 0;

	plat_id = NeedPlatform("t_nspace");
	/* an empty NSpace data chunk */
	dc = dc_CreateDC (DCC_NSpace);
	dc->dc_Platform = plat_id;
	T_DumpDC (dc);
	dc_SetStaticLoc (dc, &loc);
	errors += !ds_Store (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	return (errors);
}




static int
T_NSpace_2 (now)
ZebTime *now;
{
	PlatformId plat_id;
	DataChunk *dc;
	static Location loc = { 40.0, -160.0, 5280.0 };
	ZebTime when;
	ZebTime end;
	ZebTime begin;
	float *retrieve;
	unsigned long size;
	int errors = 0;

	begin = *now;
	begin.zt_Sec += 3600*24;
	end = when = begin;
	plat_id = NeedPlatform("t_nspace");
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
		T_DumpDC (dc);

		TX_Catch ("not dynamic");
		dc_NSAddSample (dc, &when, 0, sfield, test_data+1000);
		errors += TX_Caught();
		dc_NSAddSample (dc, &when, 0, field, test_data+1000);
		TX_Catch ("not static");
		dc_NSAddStatic (dc, field, test_data);
		errors += TX_Caught();
		dc_NSAddStatic (dc, sfield, test_data);
		dc_SetSampleAttr (dc, 0, "test_key", "test_value");
		dc_SetSampleAttr (dc, 0, "test_key2", "test_value2");
		dc_SetSampleAttr (dc, 1, "sample_number", "0");
		++when.zt_Sec;
		dc_NSAddSample (dc, &when, 1, field, test_data+1005);
		dc_SetSampleAttr (dc, 1, "sample_number", "1");
		T_DumpDC (dc);

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
		TX_Catch ("NSGetSample: static.*sample.*invalid");
		retrieve = dc_NSGetSample (dc, 100, sfield, &size);
		errors += TX_Caught();
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
			T_DumpDC (dc);
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
			T_DumpDC (dc);
			
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
	return (errors);
}




static int
T_NSpace_3 (now)
ZebTime *now;
{
	PlatformId plat_id;
	DataChunk *dc, *ndc;
	static Location loc = { 40.0, -160.0, 5280.0 };
	ZebTime when;
	ZebTime end;
	ZebTime begin;
	float *retrieve;
	unsigned long size;
	FieldId *fields;
	int nfield;
	int errors = 0;

	begin = *now;
	begin.zt_Sec += 3600*24*2;
	end = when = begin;
	plat_id = NeedPlatform("t_nspace");
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
		
		T_DumpDC (dc);

		T_NSGetAllDimensions(dc);
		T_NSGetAllVariables(dc);

		/* Test some storage */
		begin = end = when;
		dc_NSAddStatic (dc, wnum_id, test_data+50);
		TX_Catch ("NSAddStatic.*field mean_rad.*not static");
		TX_Catch ("NSAddSample.*field wnum.*not dynamic");
		dc_NSAddStatic (dc, mean_rad_id, test_data+50);
		dc_NSAddSample (dc, &begin, 0, wnum_id, test_data);
		errors += TX_ClearAll(EF_PROBLEM);
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
		T_DumpDC (dc);

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
		errors += TX_Seen();
		msg_ELog (EF_DEVELOP, "GetSample(2,BadField): data=%s",
			  (retrieve)?"non-NULL":"NULL");

		errors += !ds_Store (dc, FALSE, 0, 0);
		fields = dc_GetFields (dc, &nfield);

		/* now try to fetch what we just stored and see what we get */
		msg_ELog (EF_DEBUG, 
			  "Fetching data (detail: badval=999 .... "); 
		fflush(stdout);
		ds_SetFloatDetail (DD_FETCH_BADVAL, 999.0, &dsd, 0);
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
			T_DumpDC (dc);
			
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
			errors += TX_Seen();
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
	return (errors);
}




static int
T_NSpace_4 (now)
ZebTime *now;
{
	PlatformId plat_id;
	DataChunk *dc, *ndc;
	int i;
	static Location loc = { 40.0, -160.0, 5280.0 };
	ZebTime when;
	ZebTime end;
	ZebTime begin;
	float *retrieve;
	unsigned long size;
	FieldId *fields;
	int nfield;
	int errors = 0;

	begin = *now;
	begin.zt_Sec += 3600*24*3;
	end = when = begin;
	plat_id = NeedPlatform("t_nspace");
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
		
		T_DumpDC (dc);

		T_NSGetField (dc, sd_rad_id);
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
		msg_ELog (EF_DEBUG, "Done.");
		dc_DestroyDC (dc);
	}
	return (errors);
}




static int
T_NSpace_5 (now)
ZebTime *now;
{
	PlatformId plat_id;
	DataChunk *dc;
	static Location loc = { 40.0, -160.0, 5280.0 };
	ZebTime when;
	ZebTime end;
	ZebTime begin;
	int errors = 0;

	begin = *now;
	begin.zt_Sec += 3600*24*4;
	end = when = begin;
	plat_id = NeedPlatform("t_nspace");
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
		
		fid = F_Field("tests", 0, "Testing 1, 2, 3", "#");
		did = F_Field(dimname[1], 0, 0, 0);

		/* define a variable whose dimensions do not exist */
		EXPECT(1);
		dc_NSDefineVariable (dc, fid, 1, &did, TRUE);
		errors += TX_Seen();

		/* try defining a dimension with a long name */
		EXPECT(2);  	/* 2 dimn names too long */
		dc_NSDefineField(dc, fid, 4, dimname, dimsize, FALSE);
		errors += TX_Seen();
		T_DumpDC (dc);
		
		/* now try redefining a dimension */
		EXPECT(1); /* dimn redefined */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		errors += TX_Seen();
		T_DumpDC (dc);

		/* redefine a variable */
		EXPECT(2); /* 1 to redefine field, 1 for change in dimns */
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);
		errors += TX_Seen();
		/* re-define 'tests' to be dynamic but suppress the warning */
		dc_NSAllowRedefine (dc, TRUE);
		dc_NSDefineVariable (dc, fid, 1, &did, FALSE);
		T_DumpDC (dc);

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
		errors += TX_Seen();

		/* quick addition of data just to create a file */
		when.zt_Sec += 60;
		dc_SetStaticLoc (dc, &loc);
		dc_NSAddSample (dc, &when, 0, fid, test_data+2000);
		
		T_DumpDC (dc);
		errors += !ds_StoreBlocks (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	return (errors);
}




static int
T_NSpace_6 (now)
ZebTime *now;
{
	DataChunk *dc;
	ZebTime when;
	ZebTime end;
	ZebTime begin;
	int errors = 0;

	begin = *now;
	begin.zt_Sec += 3600*24*5;
	end = when = begin;
	/*
	 * Reset the field list before and after since this test uses
	 * so many.
	 */
	F_Reset ();
	{	/* push limits of number of dims and fields in a chunk */

		char name[ 10 ];
		char *namep = name;
		FieldId fid, did;
		unsigned long size;
		int i;

		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = NeedPlatform("t_test6");

		/* test dimn limit */
		TX_ExpectMany (EF_PROBLEM, TRUE, 2, "DefineDim.*no more room");
		for (i = 0; i < DC_MaxDimension + 2; ++i)
		{
			sprintf (name, "dimn%i", i);
			did = F_DeclareField(name, "Dimension","none");
			size = i;
			dc_NSDefineDimension(dc, did, size);
		}
		errors += TX_ClearAll(EF_PROBLEM);

		/* test field limit */
		did = F_DeclareField ("dimn12", "Dimension", "none");
		TX_ExpectMany (EF_PROBLEM, TRUE, 2, "DefineVar.*no more room");
		for (i = 0; i < DC_MaxField + 2; ++i)
		{
			sprintf (name, "field%i", i);
			fid = F_DeclareField(name, "Field", "units");
			dc_NSDefineVariable(dc, fid, 1, &did, i % 2);
		}
		errors += TX_ClearAll (EF_PROBLEM);

		TX_Problem(1);
		/* test field limit with DefineField */
		dc_NSDefineField(dc, fid, 0, 0, 0, 0);
		errors += TX_Seen();

		EXPECT(2); /* 1 for dimn limit, 1 for aborted field defn */
		/* test dimn limit with DefineField by redefining field */
		dc_NSDefineField(dc, F_Lookup("field1"), 1, 
				 &namep, &size, TRUE);
		errors += TX_Seen();

		errors += (dc_NSDefineIsComplete (dc));
		dc_NSDefineComplete (dc);
		errors += (dc_NSDefineIsComplete (dc) == FALSE);
		errors += !ds_Store (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	F_Reset ();
	return (errors);
}




static int
T_NSpace_7 (now)
ZebTime *now;
{
	PlatformId plat_id;
	DataChunk *dc;
	ZebTime when;
	ZebTime end;
	ZebTime begin;
	int errors = 0;

	begin = *now;
	begin.zt_Sec += 3600*24*6;
	end = when = begin;
	plat_id = NeedPlatform("t_nspace");
	{

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
		errors += T_MetUniform (dc);
		T_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	return (errors);
}



#define NUMS 1000


static DataChunk *
T_BuildScalar (begin, pid)
ZebTime *begin;
PlatformId pid;
{
	DataChunk *dc;

	msg_ELog (EF_DEBUG, "t_nsvsc_scalar: 8 fields, mobile, attributes");
	dc = T_SimpleScalarChunk (begin, 1, NUMS, 8, TRUE, TRUE);
	dc->dc_Platform = pid;
	return (dc);
}



static int T_StoreScalar (dc)
DataChunk *dc;
{
	return (ds_StoreBlocks (dc, TRUE, NULL, 0) ? 0 : 1);
}



DataChunk *T_FetchScalar (pid, begin, fields, nfield)
PlatformId pid;
ZebTime *begin;
FieldId *fields;
int nfield;
{
	return (ds_FetchObs (pid, DCC_Scalar, begin, 
			     fields, nfield, NULL, 0));
}


static int
T_AccessScalar (dc, fields, nfield)
DataChunk *dc;
FieldId *fields;
int nfield;
{
	float value;
	int err = 0;
	int i, f;

	if (dc)
	{
		for (i = 0; i < NUMS; ++i)
			for (f = 0; f < nfield; ++f)
				value = dc_GetScalar (dc, i, fields[f]);
		T_DumpDC (dc);
	}
	else
		err = 1;
	return (err);
}



static DataChunk *
T_BuildNSpace (begin, pid)
ZebTime *begin;
PlatformId pid;
{
	DataChunk *dc;

	msg_ELog (EF_DEBUG, "t_nsvsc_nspace: 8 fields, mobile, attributes");
	dc = T_ScalarNSpaceChunk (begin, NUMS, 8, TRUE, TRUE);
	dc->dc_Platform = pid;
	return (dc);
}


static int
T_StoreNSpace (dc)
DataChunk *dc;
{
	return (ds_StoreBlocks (dc, TRUE, NULL, 0) ? 0 : 1);
}



DataChunk *
T_FetchNSpace (pid, begin, fields, nfield)
PlatformId pid;
ZebTime *begin;
FieldId *fields;
int nfield;
{
	return (ds_FetchObs (pid, DCC_NSpace, begin, 
			     fields, nfield, NULL, 0));
}



static int 
T_AccessNSpace (dc, fields, nfield)
DataChunk *dc;
FieldId *fields;
int nfield;
{
	int err = 0;
	int i, f;
	
	if (dc)
	{
		for (i = 0; i < NUMS; ++i)
			for (f = 0; f < nfield; ++f)
				err += !dc_NSGetSample (dc, i, fields[f], 0);
		T_DumpDC (dc);
	}
	else 
		++err;
	return (err);
}



static int
T_ScalarNSpace (begin)
ZebTime *begin;
{
	DataChunk *dc;
	PlatformId plat_id;
	FieldId fields[10];
	int nfield;
	char *sc_plat = "t_nsvsc_scalar";
	char *ns_plat = "t_nsvsc_nspace";
	int err = 0;

	dc_CheckClass (FALSE);
	/*
	 * Scalar
	 */
	plat_id = NeedPlatform(sc_plat);
	dc = T_BuildScalar (begin, plat_id);
	err += T_StoreScalar (dc);
	dc_DestroyDC (dc);
	msg_ELog (EF_DEVELOP, "fetching the entire observation from '%s'", 
		  sc_plat);
	nfield = 10;
	err += !ds_GetFields (plat_id, begin, &nfield, fields);
	dc = T_FetchScalar (plat_id, begin, fields, nfield);
	err += T_AccessScalar (dc, fields, nfield);
	if (dc)
		dc_DestroyDC (dc);
	msg_ELog (EF_DEBUG, "Done.");

	/*
	 * NSpace
	 */
	plat_id = NeedPlatform (ns_plat);
	dc = T_BuildNSpace (begin, plat_id);
	err += T_StoreNSpace (dc);
	dc_DestroyDC (dc);
	msg_ELog (EF_DEBUG, "fetching the entire observation from '%s'...", 
		  ns_plat);
	nfield = 10;
	err += !ds_GetFields (plat_id, begin, &nfield, fields);
	dc = T_FetchNSpace (plat_id, begin, fields, nfield);
	err += T_AccessNSpace (dc, fields, nfield);
	if (dc)
		dc_DestroyDC(dc);
	msg_ELog (EF_DEBUG, "Done.");
	dc_CheckClass (TRUE);
	return (err);
}




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
	TestField *tf;
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


static float wnumFirst[] = {
    520.2368, 520.719, 521.2011, 521.6832, 522.1654, 522.6476, 523.1297, 
    523.6118, 524.094, 524.5762, 525.0583, 525.5404
};
static float wnumLast[] = {
    1794.552, 1795.034, 1795.516, 
    1795.998, 1796.48, 1796.963, 1797.445, 1797.927, 1798.409, 1798.891, 
    1799.373, 1799.855
};
static float meanradFirst[] = {
    131.5714, 136.2048, 133.0598, 127.105, 121.5019, 130.3238, 127.5575, 
    129.9059, 136.0601, 141.6882, 138.4667, 135.3472
};
static float meanradLast[] = {
    10.08288, 10.03311, 9.884606, 
    9.592488, 9.636732, 9.467347, 10.00038, 9.934047, 9.487029, 9.978018, 
    10.09118, 9.523479
};


static int
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
	float prec = 0.0005;

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
	T_DumpDC (dc);

	for (i = 0; i < n; ++i)
		T_NSGetField(dc, fields[i]);
		
	/* retrieve data and dump it out */
	retrieve = dc_NSGetStatic (dc, F_Lookup("wnum"), &len);
	T_DumpData (retrieve, 12, len, "wnum");
	err += T_CompareDataPrec (retrieve, wnumFirst, 12, prec);
	err += T_CompareDataPrec (retrieve+len-12, wnumLast, 12, prec);

	retrieve = dc_NSGetSample (dc, 0, F_Lookup("mean_rad"), &len);
	T_DumpData (retrieve, 12, len, "mean_rad");
	err += T_CompareDataPrec (retrieve, meanradFirst, 12, prec);
	err += T_CompareDataPrec (retrieve+len-12, meanradLast, 12, prec);

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



static int
T_AeriTypes (when)
ZebTime *when;
/*
 * Try a typical NSpace chunk, but include use of field types.
 */
{
#define N_WNUM 65
#define N_SAMPLE 16
	DataChunk *dc;
	PlatformId plat_id;
	ZebTime begin;
	int i;
	FieldId fields[10];
	int nfield = 10;
	FieldId bin_avg_id, wnum_id, mean_rad_id, therm_id, flags_id;
	static const unsigned char Check_bits[] = {
		0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
		0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
	};
	static char *bitmap_names[] = { "row", "col8" };
	static unsigned long bitmap_sizes[] = { 8, 2 };
	FieldId bitmap_id;
	static char *text_dim[] = { "text" };
	static unsigned long text_size[] = { 256 };
	FieldId obs_id;
	static char *process_dims[] = { "bin", "name" };
	static unsigned long process_sizes[] = { 7, 32 };
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

	begin = *when;
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
	T_DumpDC (dc);

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

		dc_NSAddSample(dc, when, i, obs_id, (void *)(obs_types[i%5]));
		dc_NSAddSample(dc, when, i, bin_avg_id, (void *)bin_averages);
		dc_NSAddSample(dc, when, i, therm_id, (void *)&therm);
		dc_NSAddSample(dc, when, i, flags_id, (void *)&flag);
		dc_NSAddSample(dc, when, i, mean_rad_id, (void *)mean_rads);
		++when->zt_Sec;
	}

	/*
	 * And finally store and destroy
	 */
	T_DumpDC (dc);
	msg_ELog (EF_DEBUG, "Storing and destroying.");
	err += !ds_StoreBlocks (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	msg_ELog (EF_DEBUG, "Fetching...");
	ds_GetFields (plat_id, when, &nfield, fields);
	dc = ds_FetchObs (plat_id, DCC_NSpace, &begin, fields, nfield, 0, 0);
	if (!dc)
		++err;
	else
	{
		T_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	msg_ELog (EF_DEBUG, "AERI typed NSpace test done.");
	free (mean_rads);
	return (0);
}



static struct TestPlatform sgpRWP = 
{ "t_sgp915rwpwind", FTNetCDF, OrgNSpace, 100, FALSE };


static int
T_NSpaceImplicitCV (when)
ZebTime *when;
/*
 * Test implicit fetch of coordinate variables in n-space chunks.
 */
{
	static Location loc = { 40.0, -160.0, 5280.0 };
	DataChunk *dc;
	FieldId dims[ DC_MaxDimension ];
	int i, xid;
	char buf[128];
	FieldId power, gate, angle, fid, ht_p;
	FieldId dfields[ DC_MaxField ];	/* dynamic field id's */
	FieldId *fields;
	int nfield;
	int err = 0;
	PlatformId pid; 

	pid = MakePlatform (&sgpRWP);
	CleanPlatform (pid);
	/*
	 * This test declares alot of temporary fields, so reset the
	 * fields table before and after we use it.
	 */
	F_Reset ();
	power = F_DeclareField ("power", "Power offset", "none");
	gate = F_DeclareField ("range_gate", "Height Index", "index");
	angle = F_DeclareField ("angle", "Beam elevation angle", "Deg");
	/*
	 * Create the datachunk and define the coordinate variables.
	 */
	dc = dc_Create (DCP_NSpace);
	dc_SetPlatform (dc, pid);
	dc_SetStaticLoc (dc, &loc);
	dc_NSDefineDimension (dc, power, 2);
	dc_NSDefineVariable (dc, power, 1, &power, TRUE);
	dc_NSDefineDimension (dc, gate, 75);
	dc_NSDefineVariable (dc, gate, 1, &gate, TRUE);
	dc_NSDefineDimension (dc, angle, 10);
	dc_NSDefineVariable (dc, angle, 1, &angle, TRUE);

	ht_p = F_DeclareField ("height_p", "Array of heights for each power",
			      "km");
	dims[0] = gate;
	dims[1] = power;
	dc_NSDefineVariable (dc, ht_p, 2, dims, TRUE);
	dc_SetFieldAttr (dc, ht_p, "resolution", "0.01");
	dc_SetFieldAttr (dc, ht_p, "field_index", "1");
	dims[2] = angle;
	nfield = 0;
	dfields[nfield++] = ht_p;
	msg_ELog (EF_TEST, "defining %d dynamic fields", DC_MaxField - 4);
	for (i = 0; i < DC_MaxField - 4; ++i)
	{
		sprintf (buf, "sgp915f%d", i);
		fid = F_DeclareField (buf, buf, "1");
		dc_NSDefineVariable (dc, fid, 3, dims, FALSE);
		dfields[nfield++] = fid;
	}
	/*
	 * Add the data, static first then dynamic.
	 */
	dc_NSAddStatic (dc, power, test_data);
	dc_NSAddStatic (dc, gate, test_data);
	dc_NSAddStatic (dc, angle, test_data);
	dc_NSAddStatic (dc, ht_p, test_data);

	for (i = 1; i < nfield; ++i)
	{
		dc_NSAddSample (dc, when, 0, dfields[i], test_data+i);
		dc_NSAddSample (dc, when, 1, dfields[i], test_data+i);
	}

	/*
	 * Store it
	 */
	Announce ("storing datachunk of 2 samples");
	ds_Store (dc, TRUE, NULL, 0);
	dc_Destroy (dc);
	dfa_ForceClosure ();

	/*
	 * Try to get it back with just the dynamic fields, and make sure
	 * the implicit coordinate variables are grabbed as well.  Also,
	 * we should have two samples, since both were at the same time.
	 */
	xid = TX_ExpectMany (EF_ALL, 0, 4, "fetching implicit coordinate");
	dc = ds_Fetch (pid, DCC_NSpace, when, when, dfields, nfield, NULL, 0);
	if (TX_Clear (xid, 0) != 1)	/* should have found exactly 3 */
	{
		++err;
		msg_ELog (EF_PROBLEM, "wrong number of implicit coord msgs");
	}
	if (! dc)
		return (++err);
	if (dc_GetNSample (dc) != 2)
	{
		++err;
		msg_ELog (EF_PROBLEM, "should have fetched 2 samples, got %d",
			  dc_GetNSample (dc));
	}
	if (dc_GetFieldIndex (dc, power) < 0 ||
	    dc_GetFieldIndex (dc, angle) < 0 ||
	    dc_GetFieldIndex (dc, gate) < 0)
	{
		++err;
		msg_ELog (EF_PROBLEM, "missing coordinate variable");
	}
	fields = dc_GetFields (dc, &nfield);
	if (nfield != DC_MaxField)
	{
		++err;
		msg_ELog (EF_PROBLEM, "expected to fetch %d fields, not %d",
			  DC_MaxField, nfield);
	}
	dc_Destroy (dc);
	F_Reset ();
	return (err);
}




TestRoutine NSpaceTests[] = 
{
	{ "nspace_1", FTNetCDF, DCC_NSpace, TR_BEGIN, T_NSpace_1,
	  "create, store, and destroy nspace datachunk with no fields" },
	{ "nspace_2", FTNetCDF, DCC_NSpace, TR_BEGIN, T_NSpace_2,
	  "define fields, add and retrieve data, store and fetch" },
	{ "nspace_3", FTNetCDF, DCC_NSpace, TR_BEGIN, T_NSpace_3,
	  "ARM example of dimn id defns and static variables" },
	{ "nspace_4", FTNetCDF, DCC_NSpace, TR_BEGIN, T_NSpace_4,
	  "ARM example of implicit dimns and huge datachunk write (long)" },
	{ "nspace_5", FTNetCDF, DCC_NSpace, TR_BEGIN, T_NSpace_5,
	  "long field names, redefinitions" },
	{ "nspace_6", FTNetCDF, DCC_NSpace, TR_BEGIN, T_NSpace_6,
	  "test limits on numbers and lengths of field and dimn names" },
	{ "nspace_7", FTNetCDF, DCC_NSpace, TR_BEGIN, T_NSpace_7,
	  "test met optimization of nspace datachunks" },
	{ "scalarnspace", FTNetCDF, DCC_NSpace, TR_BEGIN, T_ScalarNSpace,
	  "compare and profile nspace access and storage versus scalar" },
	{ "aerifetch", FTNetCDF, DCC_NSpace, TR_BEGIN | TF_NeedsData, T_Aeri,
	  "ARM typed fields: fetch for aeri platform" },
	{ "aeristore", FTNetCDF, DCC_NSpace, TR_BEGIN, T_AeriTypes,
	  "ARM typed fields: declare and store for aeri platform" },
	{ "implicitcv", FTNetCDF, DCC_NSpace, TR_BEGIN, T_NSpaceImplicitCV,
	  "test fetching of ARM sgp wind profiler implicit coordinates" },
	END_TESTS
};


