/*
 * $Id: nstest.c,v 1.2 1993-05-18 21:29:28 granger Exp $
 */

#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "ds_fields.h"
#include "DataChunkP.h"

/* ARGSUSED */
int
msg_handler (msg)
struct message *msg;
{
	msg_ELog (EF_INFO, "Message received");
	return (0);
}

#define SCALAR

static float test_data[10000];

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>


/*ARGSUSED*/
int
main (argc, argv)
	int argc;
	char *argv[];
{
	DataChunk *dc, *ndc;
	int i,j;
	ZebTime when, begin, end;
	struct timeb tp;
	FieldId *fields;
	int nfield;
	int ndim, nvar, rndim, rstatic;
	char *name;
	unsigned long size;
	float *retrieve;
	PlatformId plat_id;

	ftime(&tp);
	when.zt_Sec = tp.time - 3600*24;
	when.zt_MicroSec = 0;
	begin = when;
	end = when;

	for (i = 0; i < sizeof(test_data)/sizeof(test_data[0]); ++i)
		test_data[i] = i;

#if defined(NSPACE) || defined(BLOCKS)
	usy_init();
	F_Init();
	msg_connect (msg_handler, "NSpace Test");
	ds_Initialize();
	plat_id = ds_LookupPlatform("t_nspace");
#endif

#if defined(SCALAR)	/* Use only scalar chunks, for testing znf */
	{
		FieldId fid;
		float value;

		usy_init();
		F_Init();
		msg_connect (msg_handler, "ZNF Test");
		ds_Initialize();
		plat_id = ds_LookupPlatform("t_scalar");
		dc = dc_CreateDC (DCC_Scalar);
		dc->dc_Platform = plat_id;
		fid = F_DeclareField ("scalar","Test field for ZNF", "none");
		dc_SetScalarFields (dc, 1, &fid);
		dc_SetBadval (dc, -999.0);
		dc_SetGlobalAttr (dc, "zeb_platform", "t_scalar");
		dc_SetGlobalAttr (dc, "date", "today");
		value = 1.0;
		dc_AddScalar (dc, &when, 0, fid, &value); ++when.zt_Sec;
		value *= 2.0;
		dc_AddScalar (dc, &when, 1, fid, &value); ++when.zt_Sec;
		value *= 2.0;
		dc_AddScalar (dc, &when, 2, fid, &value); ++when.zt_Sec;
		value *= 2.0;
		dc_AddScalar (dc, &when, 3, fid, &value); ++when.zt_Sec;
		value *= 2.0;
		dc_AddScalar (dc, &when, 4, fid, &value); ++when.zt_Sec;
		dc_SetSampleAttr (dc, 0, "key", "first sample");
		dc_SetSampleAttr (dc, 3, "sample_number", "3");
		dc_SetSampleAttr (dc, 3, "median", "middle");
		ds_StoreBlocks (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
#endif

#ifdef NSPACE
	printf("----------------------------------------------------test1\n");
	{	/* an empty NSpace data chunk */
		
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = plat_id;
		dc_DumpDC (dc);
		ds_Store (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	printf("----------------------------------------------------test2\n");
	{	/* simple non-static variable over two dimensions */

		FieldId field, sfield;
		char *dim_names[2];
		unsigned long dim_sizes[2];

		dim_names[0] = "x";	dim_names[1] = "y";
		dim_sizes[0] = 50;	dim_sizes[1] = 25;
		field = F_DeclareField ("curl","Long name","units");
		sfield = F_DeclareField ("static_curl","Long name","units");
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = plat_id;
		dc_NSDefineField (dc, field, 2, dim_names, dim_sizes, FALSE);
		dc_NSDefineField (dc, sfield, 2, dim_names, dim_sizes, TRUE);
		dc_DumpDC (dc);

		dc_NSAddSample (dc, &when, 0, sfield, test_data+1000);
		dc_NSAddSample (dc, &when, 0, field, test_data+1000);
		dc_SetSampleAttr (dc, 0, "test_key", "test_value");
		dc_SetSampleAttr (dc, 0, "test_key2", "test_value2");
		dc_SetSampleAttr (dc, 1, "sample_number", "0");
		dc_NSAddStatic (dc, field, test_data);
		dc_NSAddStatic (dc, sfield, test_data);
		++when.zt_Sec;
		dc_NSAddSample (dc, &when, 1, field, test_data+1005);
		dc_SetSampleAttr (dc, 1, "sample_number", "1");
		dc_DumpDC (dc);

		/* retrieve data and compare */
		retrieve = dc_NSGetStatic (dc, field, &size);
		retrieve = dc_NSGetSample (dc, 1, field, &size);
		printf ("dc_NSGetStatic() returns size = %lu,", size);
		T_CompareData(retrieve, test_data+1005, size);

		T_NSGetField(dc, field);
		T_NSGetAllDimensions(dc, field);


		printf("Storing... "); fflush(stdout);
		ds_Store (dc, TRUE, 0, 0);
		printf("Destroying... ");
		dc_DestroyDC (dc);

		/* now try to fetch what we just stored and see what we get */
		printf("Fetching data....   "); fflush(stdout);
		dc = ds_Fetch (plat_id, DCC_NSpace, &when, &when,
			       &field, 1, NULL, 0);
		printf("DataChunk returned by ds_Fetch():\n");
		dc_DumpDC (dc);
		dc_DestroyDC (dc);
        }
	printf("----------------------------------------------------test3\n");
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
		dc_NSAddStatic (dc, mean_rad_id, test_data+50);
		dc_NSAddSample (dc, &begin, 0, wnum_id, test_data);
		dc_NSAddSample (dc, &begin, 0, therm_id, test_data);
		dc_NSAddSample (dc, &begin, 0, mean_rad_id, test_data);
		dc_NSAddSample (dc, &begin, 0, sd_rad_id, test_data);

		/* store it, then add more data, and store again later  */
		/* with newfile flag FALSE				*/
		ds_Store (dc, TRUE, 0, 0);
		++end.zt_Sec;
		dc_NSAddSample (dc, &end, 1, therm_id, test_data+100);
		dc_NSAddSample (dc, &end, 1, mean_rad_id, test_data+100);
		dc_NSAddSample (dc, &end, 1, sd_rad_id, test_data+100);
		dc_DumpDC (dc);

		/* test some retrieval */
		(void)dc_NSGetSample (dc, 0, wnum_id, &size); /*should fail*/
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

		printf("Storing on platform t_blocks using blocks\n");
		dc->dc_Platform = ds_LookupPlatform ("t_blocks");
		ds_StoreBlocks (dc, TRUE, 0, 0);
		printf("Storing on platform scalar without using blocks\n");
		dc->dc_Platform = ds_LookupPlatform ("t_scalar");
		ds_StoreBlocks (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
	printf("----------------------------------------------------test4\n");
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
		printf ("Adding 750 samples to datachunk... "); 
		fflush(stdout);
		when.zt_Sec += 60;
		begin = when;
		for (i = 0; i < 750; ++i)
		{
			++when.zt_Sec;
			dc_NSAddSample (dc, &when, i, wnum_id, test_data+i);
			dc_NSAddSample (dc, &when, i, therm_id, test_data+i);
			dc_NSAddSample (dc, &when, i, mean_rad_id, 
					test_data+i);
			dc_NSAddSample (dc, &when, i, sd_rad_id, test_data+i);
		}
		end = when;
		printf ("Done.\nPutSample to 't_nspace' ... "); fflush(stdout);
		ds_Store (dc, TRUE, 0, 0);
		printf ("and PutBlock to plat 't_blocks' ... "); 
		fflush(stdout);
		dc->dc_Platform = ds_LookupPlatform ("t_blocks");
		ds_StoreBlocks (dc, TRUE, 0, 0);
		printf ("Done storing.\n");
		fields = dc_GetFields (dc, &nfield);
		dc_DestroyDC (dc);

		/* now try to fetch what we just stored and see what we get */
		printf("Fetching from 't_nspace' ....   "); fflush(stdout);
		dc = ds_Fetch (plat_id, DCC_NSpace, &begin, &end,
			       fields, nfield, NULL, 0);
		printf("Done.\n");
		dc_DestroyDC (dc);
		printf("Fetching from 't_blocks' ..."); fflush(stdout);
		dc = ds_Fetch (plat_id, DCC_NSpace, &begin, &end,
			       fields, nfield, NULL, 0);
#endif
		printf("Done.\n");
		dc_DestroyDC (dc);
	}
	printf("----------------------------------------------------test5\n");
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
		dc_NSDefineVariable (dc, fid, 1, &did, TRUE);

		/* try defining a dimension with a long name */
		dc_NSDefineField(dc, fid, 4, dimname, dimsize, FALSE);
		dc_DumpDC (dc);
		
		/* now try redefining a dimension */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		dc_DumpDC (dc);

		/* redefine a variable */
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);
		dc_DumpDC (dc);

		/* test completion */
		printf("dc_NSDefineIsComplete() returns %s\n",
		       dc_NSDefineIsComplete(dc) ? "True" : "False");

		/* force completion */
		dc_NSDefineComplete(dc);
		printf("%s, dc_NSDefineIsComplete() returns %s\n",
		       "After dc_NSDefineComplete()",
		       dc_NSDefineIsComplete(dc) ? "True" : "False");

		/* try more definition after completion */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);

		/* quick addition of data just to create a file */
		when.zt_Sec += 60;
		dc_NSAddSample (dc, &when, 0, fid, test_data+2000);
		
		dc_DumpDC (dc);
		ds_StoreBlocks (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
#ifdef TEST6
	printf("----------------------------------------------------test6\n");
	{	/* push limits of number of dims and fields in a chunk */

		char name[ 10 ];
		char *namep = name;
		FieldId fid, did;
		unsigned long size;
		int i;

		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = ds_LookupPlatform("test6");

		/* test dimn limit */
		for (i = 0; i < DC_MaxDimension + 2; ++i)
		{
			sprintf (name, "dimn%i", i);
			did = F_DeclareField(name, "Dimension", "none");
			size = i;
			dc_NSDefineDimension(dc, did, size);
		}

		/* test field limit */
		did = F_Lookup("dimn12");
		for (i = 0; i < DC_MaxField + 2; ++i)
		{
			sprintf (name, "field%i", i);
			fid = F_DeclareField(name, "Field", "units");
			dc_NSDefineVariable(dc, fid, 1, &did, i % 2);
		}

		/* test field limit with DefineField */
v		dc_NSDefineField(dc, fid, 0, 0, 0, 0);

		/* test dimn limit with DefineField by redefining field */
		dc_NSDefineField(dc, F_Lookup("field1"), 1, 
				 &namep, &size, TRUE);

		dc_DumpDC (dc);

		/* see what it looks like after closing definition */
		dc_NSDefineComplete (dc);
		dc_DumpDC (dc);

		ds_Store (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
	}
#endif /* TEST6 */
#endif /* NSPACE */

#ifdef BLOCKS
	printf ("---------------------------------------------block tests\n");
	T_IRGridStoreBlocks();
	T_1DGridStoreBlocks();
#endif

	return(0);
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
	dest_id = ds_LookupPlatform ("t_1dgrid");

	fields[0] = F_Lookup("height");
	fields[1] = F_Lookup("wspd");
	fields[2] = F_Lookup("wdir");
	fields[3] = F_Lookup("u_wind");
	fields[4] = F_Lookup("v_wind");
	nfield = 5;
	TC_ZtAssemble (&when, 92, 12, 2, 18, 50, 0, 0);
	printf("Fetching observation from kapinga/prof915h... ");
	fflush(stdout);
	dc = ds_FetchObs (src_id, DCC_RGrid, &when, fields, nfield, 0, 0);
	printf("Done\n");
	printf("Storing via PutBlock to 't_1dgrid'... "); 
	fflush(stdout);
	dc->dc_Platform = dest_id;
	ds_StoreBlocks (dc, TRUE, 0, 0);
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
	dest_id = ds_LookupPlatform ("t_irgrid");


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
	printf("Done\n");
	printf("Storing via PutBlock to 't_irgrid'... "); 
	fflush(stdout);
	dc->dc_Platform = dest_id;
	ds_StoreBlocks (dc, TRUE, 0, 0);
	printf("Done.\n");
}




#ifdef notdef
T_ArmAeri()
{
	DataChunk *dc;
	PlatformId src_id;
	ZebTime when;
	int nfield, i;
	FieldId fields[5];

	/*
	 * Try to fetch an N-space datachunk from ARM sample data files
	 */
	src_id = ds_LookupPlatform ("t_aeri");

	fields[0] = F_Lookup("height");
	fields[1] = F_Lookup("wspd");
	fields[2] = F_Lookup("wdir");
	fields[3] = F_Lookup("u_wind");
	fields[4] = F_Lookup("v_wind");
	nfield = 5;
	TC_ZtAssemble (&when, 92, 12, 2, 18, 50, 0, 0);
	printf("Fetching observation from kapinga/prof915h... ");
	fflush(stdout);
	dc = ds_FetchObs (src_id, DCC_RGrid, &when, fields, nfield, 0, 0);
	printf("Done\n");
	printf("Storing via PutBlock to 't_1dgrid'... "); 
	fflush(stdout);
	dc->dc_Platform = dest_id;
	ds_StoreBlocks (dc, TRUE, 0, 0);
	printf("Done.\n");
}

#endif
