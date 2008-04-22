
#include <stdio.h>
#include <math.h>

#include <message.h>
#include "DataStore.h"
#include "dsPrivate.h"
#include "dslib.h"
#include "dfa.h"
#include "apple.h"


static int
T_1DGrid ()
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
	dc = TP_FetchObs (src_id, DCC_RGrid, &when, fields, nfield, 0, 0);
	if (! dc)
		return (++err);
	msg_ELog (EF_DEBUG,
		  "PutBlock to 't_1dgrid_cdf' and 't_1dgrid_znf'... "); 
	fflush(stdout);
	dest_id = NeedPlatform ("t_1dgrid_cdf");
	dc->dc_Platform = dest_id;
	Announce ("Storing observation to t_1dgrid_cdf... ");
	err += !TP_Store (dc, TRUE, 0, 0);
	dest_id = NeedPlatform ("t_1dgrid_znf");
	dc->dc_Platform = dest_id;
	Announce ("Storing observation to t_1dgrid_znf... ");
	err += !TP_Store (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	return (err);
}





static int
DefineMesonetClass (name, ftype)
char *name;
FileType ftype;
/*
 * Define a simple irgrid mesonet class with the given name and file type,
 * and return the class id.
 */
{
	static char *station = "BigStation";
	static int nstn = 500;
	char subname[20];
	PlatClassRef pc;
	PlatClassId pid, stn;
	int i;
	
	if ((pid = ds_LookupClass (name)) != BadClass)
		return (pid);
	pc = ds_NewClass (name);
	ds_AssignClass (pc, OrgIRGrid, ftype, /*mobile*/FALSE);
	ds_SetDaysplit (pc, TRUE);
	ds_SetComposite (pc, TRUE);
	ds_SetMaxSample (pc, 1000);
	ds_SetComment (pc, "test mesonet irregular grid");
	/*
	 * Make sure we have a class for our stations.
	 */
	stn = ds_LookupPlatform (station);
	if (stn == BadPlatform)
	{
		PlatClassRef spc = ds_NewClass (station);
		ds_AssignClass (spc, OrgScalar, ftype, /*mobile*/ 0);
		ds_SetInstanceDir (spc, InstanceCopyParent);
		stn = ds_DefineClass (spc);
	}
	/*
	 * Then we can add the stations to the mesonet.
	 */
	for (i = 0; i < nstn; ++i)
	{
		sprintf (subname, "station %d", i+1);
		ds_AddClassSubplat (pc, stn, subname);
	}
	pid = ds_DefineClass (pc);
	return (pid);
}




static int
CheckFill (dc, fields, nfield)
DataChunk *dc;
FieldId *fields;
int nfield;
{
	int fld;
	int nplat;
	int i;
	float *fdata;
	int err = 0;

	/*
	 * Check the first sample for fill values
	 */
	Announce ("checking fill values in first irgrid sample");
	nplat = dc_IRGetNPlatform (dc);
	for (fld = 0; fld < nfield; ++fld)
	{
		float miss = dc_FindFloatBadval (dc, fields[fld]);

		if ((miss) != 9990.0 + fld)
		{
			msg_ELog (EF_PROBLEM, "field %d badval %f != %f",
				  fields[fld], miss, 9990.0+fld);
			++err;
		}
		fdata = (float *) dc_IRGetGrid (dc, 0, fields[fld]);
		for (i = 0; i < nplat; ++i)
		{
			if (fdata[i] != miss)
				break;
		}
		if (i < nplat)
		{
			msg_ELog (EF_PROBLEM, "field %d, value %f %s %f",
				  fields[fld], fdata[i], "should equal", miss);
			++err;
		}
	}
	return (err);
}



static DataChunk *
FetchMesonet (pid, begin, fields, nfield, errout)
PlatformId pid;
ZebTime *begin;
FieldId *fields;
int nfield;
int *errout;
{
	DataChunk *dc;
	int err = 0;
	float *fdata;
	int nplat;
	int i, fld;
	ZebTime end;
	int nsample = 45;

	end = *begin;
	end.zt_Sec += (nsample - 1)*3600;
	msg_ELog (EF_TEST, "fetching %d samples from irgrid", nsample);
	dc = TP_Fetch (pid, DCC_IRGrid, begin, &end, fields, nfield, NULL, 0);
	if (! dc)
	{
		return (NULL);
	}
	if (dc_GetNSample (dc) != nsample)
	{
		msg_ELog (EF_PROBLEM, "%s %d samples, got %d",
			  "should have fetched", nsample, dc_GetNSample (dc));
		dc_Destroy (dc);
		return (NULL);
	}
	nplat = dc_IRGetNPlatform (dc);
	for (i = 1; i < nsample; ++i)	/* skip the "missing" first sample */
	{
		for (fld = 0; fld < nfield-1; ++fld)
		{
			fdata = (float *)dc_IRGetGrid (dc, i, fields[fld]);
			if (T_CompareData (fdata, test_data+i+fld, nplat))
			{
				msg_ELog (EF_PROBLEM, "%s %d, field %d (%s)",
					  "failed comparing sample", i,
					  fields[fld], F_GetName(fields[fld]));
				++err;
			}
		}
	}
	*errout += err;
	return (dc);
}




static DataChunk *
FetchScalar (pid, begin, fields, nfield, errout)
PlatformId pid;
ZebTime *begin;
FieldId *fields;
int nfield;
int *errout;
{
	DataChunk *dc;
	int err = 0;
	float value;
	int nplat;
	PlatformId *pids;
	int i, fld, pindex;
	ZebTime end;
	int nsample = 45;

	end = *begin;
	end.zt_Sec += (nsample - 1)*3600;
	pids = ds_LookupSubplatforms (pid, &nplat);
	if (!pids || nplat == 0)
	{
		msg_ELog (EF_PROBLEM, "ds_LookupSubplatforms failed");
		*errout += 1;
		return (NULL);
	}
	pindex = nplat/2;
	pid = pids[pindex];
	msg_ELog (EF_TEST, "fetching %d scalar samples from subplatform %d", 
		  nsample, pid);
	dc = TP_Fetch (pid, DCC_Scalar, begin, &end, fields, nfield, NULL, 0);
	if (! dc)
		++err;
	if (dc && (dc_GetNSample (dc) != nsample))
	{
		++err;
		msg_ELog (EF_PROBLEM, "%s %d samples, got %d",
			  "should have fetched", nsample, dc_GetNSample (dc));
		dc_Destroy (dc);
		dc = NULL;
	}
	for (i = 1; dc && (i < nsample); ++i)	/* skip bad value sample */
	{
		for (fld = 0; fld < nfield-1; ++fld)
		{
			value = dc_GetScalar (dc, i, fields[fld]);
			if (value != *(test_data+i+fld+pindex))
			{
				msg_ELog (EF_PROBLEM, 
					  "scalar subplat compare failed");
				++err;
				break;
			}
		}
	}
	free (pids);
	*errout += err;
	return (dc);
}




static int
T_MakeIRGrid (when, arg)
ZebTime *when;
char *arg;
{
	FileType ftype = (FileType)arg;
	static char *cdfname = "t_fakemeso_cdf";
	static char *znfname = "t_fakemeso_znf";
	DataChunk *dc;
	PlatformId pid;
	PlatClassId mesonet;
	ZebTime begin, end;
	int nfield;
	FieldId fields[10];
	int err = 0;
	int nplat, nsample;
	PlatformId *pids;
	Location *locs;
	int i, fld;

	/*
	 * Make some IRGrid data, store it, and fetch it
	 */
	mesonet = DefineMesonetClass ("BigMesonet", FTNetCDF);
	if (ftype == FTNetCDF)
	{
		if ((pid = ds_LookupPlatform (cdfname)) == BadPlatform)
			pid = ds_DefinePlatform (mesonet, cdfname);
	}
	else if ((pid = ds_LookupPlatform (znfname)) == BadPlatform)
	{
		PlatClassRef pc = ds_NewSubClass ("BigMesonetZNF", mesonet);
		ds_SetFiletype (pc, FTZebra);
		mesonet = ds_DefineClass (pc);
		pid = ds_DefinePlatform (mesonet, znfname);
	}

	CleanPlatform (pid);
	fields[0] = F_Lookup("pres");
	fields[1] = F_Lookup("cpres0");
	fields[2] = F_Lookup("tdry");
	fields[3] = F_Lookup("dp");
	fields[4] = F_Lookup("rain");
	nfield = 5;
	begin = *when;
	/*
	 * Create some data
	 */
	{
		char s[128];
		sprintf (s, "building big mesonet irgrid, plat %s", 
			 ds_PlatformName (pid));
		Announce (s);
	}
	pids = ds_LookupSubplatforms (pid, &nplat);
	if (! pids || nplat < 1)
	{
		msg_ELog (EF_PROBLEM, "LookupSubplatforms failed");
		return (++err);
	}
	locs = (Location *) malloc (nplat * sizeof(Location));
	for (i = 0; i < nplat; ++i)
	{
		double r = 2.0*i;
		double theta = 2.0*M_PI*i/100.0;
		locs[i].l_lat = 40.0 + r * sin(theta);
		locs[i].l_lon = -100.0 + r * cos(theta);
		locs[i].l_alt = 0.0;
	}
	dc = dc_Create (DCP_IRGrid);
	dc_SetPlatform (dc, pid);
	dc_IRSetup (dc, nplat, pids, locs, nfield, fields);
	dc_SetBadval (dc, -8888.0);
	/* 
	 * Set a different bad value for each field, and fill the zero
	 * sample with bad values.
	 */
	for (fld = 0; fld < nfield; ++fld)
	{
		float badval = 9990.0+fld;
		dc_SetFieldBadval (dc, fields[fld], &badval);
		dc_IRAddMissing (dc, &begin, 0, 1, fields[fld]);
	}
	err += CheckFill (dc, fields, nfield);
	err += !ds_Store (dc, TRUE, NULL, 0);
	dc_Destroy (dc);

	end = begin;
	dc = NULL;
	nsample = 0;
	msg_ELog (EF_TEST, "adding 2 days of hourly samples to datachunk");
	for (i = 1; i < 48; ++i)	/* 2 days of hourly samples */
	{
		if (! dc)
		{
			dc = dc_Create (DCP_IRGrid);
			dc_SetPlatform (dc, pid);
			dc_IRSetup (dc, nplat, pids, locs, nfield, fields);
			dc_SetBadval (dc, -8888.0);
			for (fld = 0; fld < nfield; ++fld)
			{
				float badval = 9990.0+fld;
				dc_SetFieldBadval (dc, fields[fld], &badval);
			}
			nsample = 0;
		}
		end.zt_Sec += 3600;
		for (fld = 0; fld < nfield; ++fld)
		{
			dc_IRAddGrid (dc, &end, nsample, fields[fld], 
				      test_data+i+fld);
		}
		++nsample;
		if (i % 16 == 0)
		{
			err += !TP_Store (dc, FALSE, NULL, 0);
			dc_Destroy (dc);
			dc = NULL;
		}
	}
	if (dc)
	{
		err += !TP_Store (dc, FALSE, NULL, 0);
		dc_Destroy (dc);
	}
	dfa_ForceClosure ();
	free (pids);
	free (locs);

	/*
	 * Verify daysplit caused two observations of 24 samples each.
	 */
	{
		ZebTime obs[3];
		ZebTime times[100];
		int ntime = ds_GetObsTimes (pid, &end, obs, 3, NULL);
		if (ntime != 2)
		{
			++err;
			msg_ELog (EF_PROBLEM, "expected two observations");
		}
		else if (ds_GetObsSamples(pid, obs, times, 0, 100) != 24)
			++err;
		else if (ds_GetObsSamples(pid, obs+1, times, 0, 100) != 24)
			++err;
	}

	/*
	 * Try a fetched copy, including an extra field.
	 */
	fields[nfield++] = F_Lookup ("missing");
	dc = FetchMesonet (pid, &begin, fields, nfield, &err);
	if (! dc)
	{
		return (++err);
	}
	else if (ftype == FTNetCDF)
	{
		float *fdata;
		float badval;
		FieldId fid = fields[nfield-1];

		err += CheckFill (dc, fields, nfield-1);
		/*
		 * Make sure the missing field got filled 
		 */
		badval = dc_FindFloatBadval (dc, fid);
		if (badval != -8888.0)
		{
			++err;
			msg_ELog (EF_PROBLEM, "field %d badval %f != %f",
				  fid, badval, -8888.0);
		}
		for (i = 0; i < dc_GetNSample(dc); ++i)
		{
			fdata = (float *)dc_IRGetGrid(dc, i, fid);
			if (fdata[0] != -8888.0 || fdata[nplat-1] != -8888.0)
			{
				msg_ELog (EF_PROBLEM, "%s with -8888.0",
					  "missing field not filled");
				++err;
				break;
			}
		}
		dc_Destroy (dc);
	}
	else	/* ZNF files not supporting field-specific bad values yet */
	{
		dc_Destroy (dc);
	}
	/*
	 * Try a Scalar fetch of a particular subplatform.
	 */
	dc = FetchScalar (pid, &begin, fields, nfield, &err);
	if (dc && ftype == FTNetCDF)
	{
		float badval;
		Announce ("testing first sample against field bad values");
		for (fld = 0; fld < nfield; ++fld)
		{
			float value = dc_GetScalar (dc, 0, fields[fld]);

			badval = dc_FindFloatBadval (dc, fields[fld]);
			if (value != badval)
			{
				msg_ELog (EF_PROBLEM, 
					  "%s %f != bad value %f",
					  "first sample value", 
					  value, badval);
				++err;
			}
		}
		Announce ("testing all samples of filled field");
		fld = nfield - 1;
		badval = dc_FindFloatBadval (dc, fields[fld]);
		for (i = 0; i < dc_GetNSample(dc); ++i)
		{
			float value = dc_GetScalar (dc, i, fields[fld]);
			if (value != badval)
			{
				msg_ELog (EF_PROBLEM, "scalar subplat: %s",
					  "missing field not filled");
				++err;
				break;
			}
		}
	}
	if (dc)
		dc_Destroy (dc);
	return (err);
}



#ifdef notdef
static int
T_IRGrid ()
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
	dest_id = NeedPlatform ("t_irgrid_cdf");
	if (dest_id != BadPlatform)
	{
		msg_ELog (EF_DEBUG, "PutBlock to 't_irgrid_cdf'...");
		fflush(stdout);
		dc->dc_Platform = dest_id;
		err += !ds_StoreBlocks (dc, TRUE, 0, 0);
	}
	dest_id = NeedPlatform ("t_irgrid_znf");
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
#endif


static int
T_RGrid (begin, pid)
ZebTime *begin;
PlatformId pid;
{
	DataChunk *dc;
	FieldId fids[50];
	int fld;
	int i;
	ZebTime when;
	TestField *tf;
	Location loc, origin;
	int len;
	int nsample = 20;
	int nfield = 3;
	RGrid rgrid;
	RGrid rg;
	dsDetail dets[5];
	float *grid;
	int err = 0;
#	define ALTUNITS AU_mMSL

	loc.l_lat = 40.0; loc.l_lon = -160.0; loc.l_alt = 10.0;
	dc = dc_CreateDC (DCC_RGrid);
	for (i = 0; i < nfield; ++i)
	{
		tf = TestFields+i;
		fids[i] = F_DeclareField (tf->name, tf->desc, tf->units);
	}
	dc_RGSetup (dc, nfield, fids);
	dc_SetBadval (dc, -999.0);
	dc_SetStaticLoc (dc, &loc);
	dc_SetLocAltUnits (dc, ALTUNITS);

	when = *begin;
	dc_HintNSamples (dc, nsample, TRUE);
	rgrid.rg_Xspacing = 0.5;	/* km */
	rgrid.rg_Yspacing = 0.5;
	rgrid.rg_Zspacing = 100;	/* m */
	rgrid.rg_nX = 40;
	rgrid.rg_nY = 40;
	rgrid.rg_nZ = 7;
	msg_ELog (EF_TEST, "building %d samples of %dx%dx%d regular grids",
		  nsample, rgrid.rg_nX, rgrid.rg_nY, rgrid.rg_nZ);
	for (i = 0; i < nsample; ++i)
	{
		for (fld = 0; fld < nfield; ++fld)
			dc_RGAddGrid (dc, i, fids[fld], &loc, &rgrid,
				      &when, (void *)(test_data), 0);
		when.zt_Sec += 6*3600;	/* 6-hour spacing */
	}
	/* someday verify that these get stored and retrieved */
	dc_SetSampleAttr (dc, 0, "key", "first sample");
	dc_SetSampleAttr (dc, 3, "sample_number", "3");
	dc_SetSampleAttr (dc, i/2, "median", "middle");
	dc_SetSampleAttr (dc, i-1, "key", "last sample");
	T_DumpDC (dc);

	/* Now store it to a platform, fetch, and compare the results */
	dc_SetPlatform (dc, pid);
	err += !TP_Store (dc, TRUE, NULL, 0);
	dc_Destroy (dc);
	dfa_ForceClosure ();

	/* First try fetching each altitude independently */
	msg_ELog (EF_TEST, "fetching each altitude separately for 2 fields");
	for (i = 0; i < rgrid.rg_nZ; ++i)
	{

		ds_SetFloatDetail (DD_FETCH_ALTITUDE, loc.l_alt + 
				   i*rgrid.rg_Zspacing, dets, 0);
		dc = TP_Fetch (pid, DCC_RGrid, begin, &when, fids, 2, dets, 1);
		if (! dc)
			break;
		/*
		 * Verify location and nZ, then compare data
		 */
		if (dc_GetNSample (dc) != nsample)
			break;
		grid = (float *)dc_RGGetGrid (dc, nsample/2, fids[1], &origin,
					      &rg, &len);
		len /= sizeof(float);
		if (len != rg.rg_nX * rg.rg_nY || rg.rg_nZ != 1)
			break;
		if (origin.l_lat != loc.l_lat || origin.l_lon != loc.l_lon ||
		    origin.l_alt != loc.l_alt + i*rg.rg_Zspacing)
			break;
		err += T_CompareData (grid, test_data + (i * len), len);
		/*
		 * Verify altitude units
		 */
		if (dc_GetLocAltUnits (dc) != ALTUNITS)
		{
			msg_ELog (EF_PROBLEM, "wrong altitude units fetched");
			++err;
		}
		dc_Destroy (dc);
	}
	if (i < rgrid.rg_nZ)
	{
		msg_ELog (EF_PROBLEM, "altitude slices failed");
		++err;
	}

	msg_ELog (EF_TEST, "fetching all altitudes, all fields, all times");
	dc = TP_Fetch (pid, DCC_RGrid, begin, &when, fids, nfield, NULL, 0);
	if (!dc)
		return (++err);
	if (dc_GetNSample (dc) != nsample)
		++err;

	/* Compare data for all altitudes */
	nsample = dc_GetNSample (dc);
	msg_ELog (EF_TEST, "comparing data for %d samples", nsample);
	for (i = 0; i < nsample; ++i)
	{
		for (fld = 0; fld < nfield; ++fld)
		{
			grid = (float *)dc_RGGetGrid (dc, i, fids[fld], 
						      &origin, &rg, &len);
			len /= sizeof(float);
			if (len != rgrid.rg_nX * rgrid.rg_nY * rgrid.rg_nZ)
				break;
			if (origin.l_lat != loc.l_lat || 
			    origin.l_lon != loc.l_lon ||
			    origin.l_alt != loc.l_alt)
				break;
			err += T_CompareData (grid, test_data, len);
		}
		if (fld < nfield)
			++err;
	}
	dc_Destroy (dc);
	msg_ELog (EF_TEST, "rgrid test done");
	return (err);
}



struct TestPlatform rgridPlats[2] = 
{
	{ "t_rgrid_cdf", FTNetCDF, Org3dGrid, 100, FALSE },
	{ "t_rgrid_znf", FTZebra, Org3dGrid, 100, FALSE }
};



static int
T_RGridCDF (when)
ZebTime *when;
{
	PlatformId pid;

	pid = MakePlatform (rgridPlats);
	CleanPlatform (pid);
	return (T_RGrid (when, pid));
}

	


static int
T_RGridZNF (when)
ZebTime *when;
{
	PlatformId pid;

	pid = MakePlatform (rgridPlats+1);
	CleanPlatform (pid);
	return (T_RGrid (when, pid));
}

	



TestRoutine GridTests[] = 
{
	{ "irgridcdf", FTNetCDF, DCC_IRGrid, TR_PLAT, T_MakeIRGrid,
	  "create, store, and fetch mesonet from netcdf", (char *)FTNetCDF },
	{ "irgridznf", FTZebra, DCC_IRGrid, TR_PLAT, T_MakeIRGrid,
	  "create, store, and fetch mesonet from znf", (char *)FTZebra },
	{ "rgridcdf", FTNetCDF, DCC_RGrid, TR_BEGIN, T_RGridCDF,
	  "rgrid dc interface, storage, and retrieval for netcdf" },
	{ "rgridznf", FTZebra, DCC_RGrid, TR_BEGIN, T_RGridZNF,
	  "rgrid dc interface, storage, and retrieval for znf" },
#ifdef notdef
	{ "realirgrid", FTNetCDF, DCC_IRGrid, TR_BEGIN, T_IRGrid,
	  "fetch mesonet irgrid" },
#endif
	{ "prof1dgrid", FTNetCDF, DCC_RGrid, TR_BEGIN, T_1DGrid,
	  "fetch and store 1-D profiler grids" },
	END_TESTS
};

