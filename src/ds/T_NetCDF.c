
#include "DFA_NetCDF.c"
#include "apple.h"

static int
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
	ds_SetFloatDetail (DD_FETCH_BADVAL, dc_GetBadval (dc), fdets, nfdet++);
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



static int
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
	int errors = 0;
	static char *units[] = {
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
		"seconds since 1994-09-15 06:15",
		NULL
	};
	char **u;
	int result;
	PlatformId pid, pid2;
			
	TestTimeUnits = 1;
	msg_ELog (EF_DEBUG, "Testing dnc_TimeUnits()");
	u = units;
	TX_Catch ("unknown reference string.*depuis");
	TX_ExpectMany (EF_ALL, TRUE, 2, "time units must be.*seconds");
	TX_Catch ("bad syntax in time units");
	TX_Catch ("could not understand time zone");
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
	errors += TX_ClearAll (EF_PROBLEM);

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
	TestTimeUnits = 0;
	return (errors);
}	



TestRoutine NetCDFTests[] = 
{
	{ "timeunits", FTNetCDF, DCC_None, TR_BEGIN, T_TimeUnits,
	  "test netcdf time units string interpretation" },
	END_TESTS
};



