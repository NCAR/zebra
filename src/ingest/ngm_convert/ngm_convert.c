/*
 * Convert original NGM netCDf files, which used RGrid datachunks,
 * to NSpace datachunks using the new conventions for regularly-spaced
 * lat/lon grids with irregularly-spaced altitudes in non-standard
 * units (pressure).
 */

/*----------------------------------------------------------------------

Do we 

(1) read the netCDF and convert to netCDF, or

(2) define another platform, read from netCDF, and ds_Store, or

(3) define another platform, fetch from original plat and ds_Store

We're going with the third because it will allow us to test lots of
additions for the handling of model grids, including irregularly-space
pressure altitudes and forecast times.

The grids are the same, except the pressure_level field becomes the

   altitude(time, altitude)

field, and the nspace grids must be defined as

   temp(time, altitude, longitude, latitude)

Should we change DFA_NetCDF to not write lat, lon, and alt when they
exist as fields in the DataChunk?  And when reading them, just
automatically get the (0,0,0,...,0) element to use for the Location
of the datachunk.
 ---> For now, using 'altitude', 'latitude', and 'longitude'.

Or provide details for not creating or writing lat, lon, alt?

We'll store the lat/lon arrays explicitly, using the x_spacing
and y_spacing variables from the original NGM netCDF files.

The original NGM netCDF files are implicitly the times of the 6 hour
forecast valid time.  So the issue time is 6 hours prior to the sample time
in the netCDF file.

---------------------------------------------------------------------*/


#include <stdio.h>

#include <defs.h>
#include <message.h>
#include <timer.h>
#include <DataStore.h>
#include <copyright.h>


/*
 * Prototypes 
 */
static void GenerateLatLon FP((const Location *origin, const RGrid *rgrid,
                               float *lats, float *lons));
static void GenerateAlts FP((DataChunk *rgdc, int sample, 
                             FieldId levelid, float *alts));
static DataChunk *ConvertRGridToNSpace FP((DataChunk *rgdc, PlatformId dest, 
                                           char *altfield, int forecast));
static void ConvertPlatforms FP((PlatformId src, char *destbasename,
                                 char *altfield, int forecast));
static bool DoPlatform FP((PlatformId pid, ZebTime *when, int forecast));
static void CopyAttributes FP((DataChunk *src, DataChunk *dest));


/*
 * Global options
 */
static bool PreserveDest = FALSE;	/* Don't overwrite dest obs */
static bool ConvertLatest = FALSE;	/* Convert latest src obs only */

#define MSGNAME "NGM"

static void
Die (code, msg)
int code;
char *msg;
{
	if (msg)
		printf ("%s\n", msg);
	exit (code);
}



static void
Usage (prog, error)
char *prog;
char *error;
{
	if (error)
		printf ("%s: %s\n", prog, error);
	printf ("usage: %s [-plh] <source platform> <dest platform>\n", prog);
	printf ("  -h  Print this usage message\n");
	printf ("  -p  Preserve destination observations---don't overwrite\n");
	printf ("  -l  Convert only the latest source observation\n");
	printf ("The destination platform name appended with '.issue' and\n");
	printf ("'.valid' to name platforms for storing the samples by\n");
	printf ("issue time and predicted time, respectively.\n");
}



static int
message (msg)
struct message *msg;
/*
 * More than likely we're being told to kill ourselves
 */
{
	struct mh_template *mh = (struct mh_template *)msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (mh->mh_type == MH_DIE)
			Die (0, "message handler orders abort");
	}
	msg_ELog (EF_PROBLEM, "errant message ignored");
	return (0);
}



static int
ParseOptions (argc, argv)
int argc;
char *argv[];
{
	int i;
	char *c;
	char buf[128];

	i = 1;
	while ((i < argc) && (argv[i][0] == '-'))
	{
		c = argv[i] + 1;
		while (*c)
		{
			switch (*c)
			{
			   case 'h':
				Usage (argv[0], NULL);
				Die (0, NULL);
				break;
			   case 'p':
				PreserveDest = TRUE;
				break;
			   case 'l':
				ConvertLatest = TRUE;
				break;
			   default:
				sprintf (buf, "illegal option '%c'", *c);
				Usage (argv[0], buf);
				Die (-1, NULL);
			}
			++c;
		}
		++i;
	}
	return (i);
}



int
main (argc, argv)
int argc;
char *argv[];
/*
 * Expect two arguments on the command line.  Initialize everything and
 * then pass the work on.
 */
{
	PlatformId src_id;
	int nextopt;
	char msgname[128];

	nextopt = ParseOptions (argc, argv);
	if (nextopt+1 >= argc)
	{
		Usage (argv[0], "too few arguments");
		Die (0, NULL);
	}

	/*
	 * Connect to message handler and initialize DataStore
	 */
	usy_init ();
	sprintf (msgname, "%s (%d)", MSGNAME, getpid());
	if (! msg_connect (message, msgname) ||
	    ! ds_Initialize ())
	{
		Die (99, "could not initialize message and/or datastore");
	}

	/*
	 * Make sure we have valid platforms and id's
	 */
	src_id = ds_LookupPlatform (argv[nextopt]);
	if (src_id == BadPlatform)
	{
		printf ("%s: bad source platform '%s'\n", argv[nextopt]);
		Die (1, "must have valid source platform name");
	}

	/*
	 * Now we can actually start processing
	 */
	F_DeclareField ("pressure_level", "Pressure altitude", "millibars");
	ConvertPlatforms (src_id, argv[nextopt+1], 
			  "pressure_level", (int)(6*3600));
	return (0);
}



static void
ConvertPlatforms (src, destbase, altfield, forecast)
PlatformId src;
char *destbase;		/* Base platform name for issue and valid platforms */
char *altfield;		/* Name of field to use to index altitudes */
int forecast;		/* The forecast offsets of the NGM samples */
/*
 * Traverse all of the observations available in the source platform,
 * converting each one and storing it in the destination platform.
 *
 * Someday we may want to check the destination for an existing platform
 * so as not to repeat the conversion.
 *
 * If an observation does already exist in the destination platform,
 * it should be deleted here before writing the new one.
 */
{
	char destvalid[128], destissue[128];
	PlatformId valid, issue;
	DataChunk *rgdc, *nsdc;
	int nfield;
	FieldId fields[DC_MaxField];
	ZebTime times[256];
	int maxtime = 256;
	int ntime, obs, ndone;
	ZebTime now;

	sprintf (destvalid, "%s.valid", destbase);
	sprintf (destissue, "%s.issue", destbase);
	valid = ds_LookupPlatform (destvalid);
	issue = ds_LookupPlatform (destissue);
	if (valid == BadPlatform)
		printf ("%s: bad valid platform '%s'\n", destvalid);
	if (issue == BadPlatform)
		printf ("%s: bad issue platform '%s'\n", destissue);
	if ((valid == BadPlatform) || (issue == BadPlatform))
		Die (1, "must have valid destination platform names");

	/*
	 * Get the times of all of the observations up til now.
	 * If we're only converting the most recent, we need only one obs.
	 */
	if (ConvertLatest)
		maxtime = 1;
	tl_Time (&now);
	ntime = ds_GetObsTimes (src, &now, times, maxtime, NULL);

	msg_ELog (EF_INFO, "processing %d observations from source '%s'",
		  ntime, ds_PlatformName (src));
	ndone = 0;
	/*
	 * Process observations backwards so that staggered issue-time
	 * samples will get appended to previously converted observations.
	 */
	for (obs = ntime - 1; obs >= 0; --obs)
	{
		/*
		 * Make sure we get all of the fields
		 */
		nfield = DC_MaxField;
		ds_GetFields (src, times+obs, &nfield, fields);

		/*
		 * Then fetch this observation
		 */
		rgdc = ds_FetchObs (src, DCC_RGrid, times+obs, fields, 
				    nfield, NULL, 0);
		if (!rgdc)
		{
			msg_ELog (EF_PROBLEM, "fetch failed");
			continue;
		}

		/*
		 * Convert this chunk into an n-space chunk and store it.
		 * Use a forecast offset of zero to get samples by predicted
		 * (valid) time, and use the given offset to get samples
		 * by issue time.
		 */
		if (DoPlatform (issue, times+obs, forecast))
		{
			nsdc = ConvertRGridToNSpace (rgdc, issue, altfield, 
						     forecast);
			ds_Store (nsdc, FALSE, NULL, 0);
			while (msg_poll (1) != MSG_TIMEOUT) ;
			dc_DestroyDC (nsdc);
			++ndone;
		}
		if (DoPlatform (valid, times+obs, 0))
		{
			nsdc = ConvertRGridToNSpace (rgdc, valid, altfield, 0);
			ds_Store (nsdc, FALSE, NULL, 0);
			while (msg_poll (1) != MSG_TIMEOUT) ;
			dc_DestroyDC (nsdc);
			++ndone;
		}
		dc_DestroyDC (rgdc);
	}
	msg_ELog (EF_INFO, "done: %d destination observations stored", ndone);
}



static bool
DoPlatform (pid, when, forecast)
PlatformId pid;		/* id of the destination platform		   */
ZebTime *when;		/* time of source platform conversion		   */
int forecast;		/* forecast offset of destination from source time */
{
	char buf[128];
	ZebTime obs;
	ZebTime check;

	/*
	 * The src time is the predicted time, so we'll be storing
	 * at a dest time 'forecast' seconds prior to the src time.
	 * Hence we need to subtract forecast seconds from the source
	 * time and check for a destination observation at this time.
	 */
	check = *when;
	check.zt_Sec -= forecast;
	if (ds_GetObsTimes (pid, &check, &obs, 1, NULL))
	{
		TC_EncodeTime (&check, TC_Full, buf);
		if (!PreserveDest && TC_Eq(check, obs))
		{
			msg_ELog (EF_INFO, 
				  "%s: removing existing observation at %s",
				  ds_PlatformName(pid), buf);
			ds_DeleteObs (pid, &obs);
			while (msg_poll (1) != MSG_TIMEOUT)
				; /* allow msgs to clear from deletion */
		}
		else if (PreserveDest && TC_Eq(check, obs))
		{
			msg_ELog (EF_INFO,
				  "%s: skipping existing observation at %s", 
				  ds_PlatformName(pid), buf);
			return (FALSE);
		}
	}
	TC_EncodeTime (when, TC_Full, buf);
	msg_ELog (EF_INFO, "%s: converting from source at %s", 
		  ds_PlatformName(pid), buf);
	return (TRUE);
}




static DataChunk *
ConvertRGridToNSpace (rgdc, dest, altfield, forecast)
DataChunk *rgdc;	/* the RGrid datachunk to convert */
PlatformId dest;
char *altfield;		/* the altitude field, if any	  */
int forecast;		/* the NGM samples' forecast offset, in seconds */
/*
 * Create a new NSpace datachunk and fill it with the grids in the
 * RGrid datachunk using the n-space conventions for rgrids.
 *
 * Assumes the x_ and y_ spacing are actually for latitude and longitude.
 * If z_spacing is valid and in kilometers, then altfield is NULL; 
 * otherwise altfield is the name of the RGrid field to use to
 * determine the irregularly spaced altitudes along z and which becomes
 * the alt() variable in the n-space chunk.
 *
 * The NGM samples are stored at the valid time of a 6 hour forecast, so
 * we must convert the times to issue times and then store the valid time
 * for each sample.  The actual
 */
{
	DataChunk *nsdc;
	RGrid rgrid;
	Location origin;
	FieldId *fields;
	int i, f;
	int nfield;
	int nsample;
	float *lats, *lons, *alts, *grid;
	FieldId latid, lonid, altid;
	FieldId levelid;
	FieldId dimns[3];
	AltUnitType alttype;
	ZebTime issue, valid;
	char *altunits;

	/*
	 * We only handle the case of explicit altfields for now
	 */
	if (!altfield)
	{
		msg_ELog (EF_PROBLEM, "need altfield to convert rgrid");
		return;
	}

	/*
	 * Get our list of fields and the number of samples we need to convert
	 */
	fields = dc_GetFields (rgdc, &nfield);
	nsample = dc_GetNSample (rgdc);

	/*
	 * Create our N-Space datachunk
	 */
	nsdc = dc_CreateDC (DCC_NSpace);
	nsdc->dc_Platform = dest;

	/*
	 * Use the first sample and first field to get some info about 
	 * the grids.  Use this info the create the n-space fields and
	 * dimensions.  For NGM, negate the origin lon and translate
	 * from NW corner to SW corner.  Sigh...
	 */
	(void) dc_RGGetGrid (rgdc, 0, fields[0], &origin, &rgrid, NULL);
	origin.l_lon = 0 - origin.l_lon;
	origin.l_lat = origin.l_lat - rgrid.rg_nY * rgrid.rg_Yspacing;
	
	/*
	 * Create the alt Dimension and variable.  Size is rg_nZ.  Since the
	 * altfield is originally an RGrid field dependent on time, we
	 * must assume dynamic in case the altitude levels actually *do*
	 * change between samples.  The units of the altitude field will
	 * be the units of altfield, which hopefully is recognized.
	 */
	levelid = F_Lookup (altfield);
	altunits = F_GetUnits (levelid);
	if (! au_ConvertName (altunits, &alttype))
	{
		msg_ELog (EF_PROBLEM, "altfield '%s' units '%s' not known",
			  altfield, altunits);
		altid = F_DeclareField ("altitude", "Altitude", altunits);
	}
	else
	{
		altid = F_DeclareField ("altitude", "Altitude", 
					(char *)au_LongUnitsName (alttype));
		dc_SetLocAltUnits (nsdc, alttype);
	}
	dc_NSDefineDimension (nsdc, altid, rgrid.rg_nZ);
	dc_NSDefineVariable (nsdc, altid, 1, &altid, FALSE);

	/*
	 * Create the lat/lon dimensions and coordinate variables
	 */
	latid = F_DeclareField ("latitude", "North Latitude", "degrees");
	lonid = F_DeclareField ("longitude", "East Longitude", "degrees");
	dc_NSDefineDimension (nsdc, latid, rgrid.rg_nY);
	dc_NSDefineDimension (nsdc, lonid, rgrid.rg_nX);
	dc_NSDefineVariable (nsdc, latid, 1, &latid, /*is_static*/TRUE);
	dc_NSDefineVariable (nsdc, lonid, 1, &lonid, /*is_static*/TRUE);

        /*
	 * All those fields other than altfield are defined over the
	 * lat, lon, and alt dimensions.	
	 */
	dimns[0] = altid;	/* Z */
	dimns[1] = latid;	/* Y */
	dimns[2] = lonid;	/* X */
	for (i = 0; i < nfield; ++i)
	{
		if (fields[i] != levelid)
			dc_NSDefineVariable (nsdc, fields[i], 3, dimns, FALSE);
	}
	dc_NSDefineComplete (nsdc);

	/*
	 * Define one forecast offset.  By default all of the samples will
	 * be associated with this offset.
	 */
	valid.zt_MicroSec = 0;
	valid.zt_Sec = forecast;
	dc_DefineForecastOffsets (nsdc, 1, &valid);

	/*
	 * Use the rgrid spacings and counts to figure the lat/lon arrays,
	 * then store them.
	 */
	lats = (float *) malloc (rgrid.rg_nY * sizeof(float));
	lons = (float *) malloc (rgrid.rg_nX * sizeof(float));
	GenerateLatLon (&origin, &rgrid, lats, lons);
	dc_NSAddStatic (nsdc, latid, lats);
	dc_NSAddStatic (nsdc, lonid, lons);
	free (lats);
	free (lons);

	/*
	 * Now traverse the samples in the RGrid chunk.  Altitude levels
	 * have to copied, but all of the other fields do not have to
	 * be modified at all.
	 */
	alts = (float *) malloc (rgrid.rg_nZ * sizeof(float));
	for (i = 0; i < nsample; ++i)
	{
		/*
		 * Once we get the original sample time, subtract the
		 * forecast offset to get the issue time, then use the
		 * sample time as the valid time for the new NSpace sample.
		 */
		dc_GetTime (rgdc, i, &valid);
		issue = valid;
		issue.zt_Sec -= forecast;
		GenerateAlts (rgdc, i, levelid, alts);
		dc_NSAddSample (nsdc, &issue, i, altid, alts);
		if (i == 0)
		{
			origin.l_alt = alts[0];
			dc_SetStaticLoc (nsdc, &origin);
		}
		for (f = 0; f < nfield; ++f)
		{
			if (fields[f] == levelid)
				continue;
			grid = dc_RGGetGrid (rgdc, i, fields[f], 0, 0, 0);
			dc_NSAddSample (nsdc, &issue, i, fields[f], grid);
	        }
	}
	free (alts);

	/*
	 * Copy the field and global attributes, and we're done.
	 */
	CopyAttributes (rgdc, nsdc);
	dc_SetBadval (nsdc, -99999.0);
	return (nsdc);
}




static void
GenerateAlts (rgdc, sample, levelid, alts)
DataChunk *rgdc;		/* The RGrid data chunk */
int sample;			/* The sample we're generating */
FieldId levelid;		/* The grid field containing altitude levels */
float *alts;			/* Array to hold altitudes */
/* 
 * Extract the grid for the altitude level field, and use it to generate
 * an array of altitudes.
 */
{
	RGrid rgrid;
	float *grid;
	int z;

	grid = dc_RGGetGrid (rgdc, sample, levelid, NULL, &rgrid, NULL);

	for (z = 0; z < rgrid.rg_nZ; ++z)
	{
		alts[z] = grid[ z * rgrid.rg_nY * rgrid.rg_nX ];
	}
}




static void
GenerateLatLon (origin, rgrid, lats, lons)
const Location *origin;
const RGrid *rgrid;
float *lats;
float *lons;
/*
 * Use the rgrid info to generate a coordinate array for lat and lon.
 * x_spacing refers to lon, and y_spacing refers to lat.  The lats and
 * lons arrays must point to enough space to hold the arrays.
 */
{
	int i;

	lats[0] = origin->l_lat;
	for (i = 1; i < rgrid->rg_nY; ++i)
		lats[i] = lats[i-1] + rgrid->rg_Yspacing;
	lons[0] = origin->l_lon;
	for (i = 1; i < rgrid->rg_nX; ++i)
		lons[i] = lons[i-1] + rgrid->rg_Xspacing;
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


static void
CopyAttributes (src, dest)
DataChunk *src;
DataChunk *dest;
{
	int fld;
	int nfield;
	FieldId *fields;

	fields = dc_GetFields (src, &nfield);
	DestDC = dest;
	dc_ProcessAttrs (src, NULL, CopyGlobalAtts);
	for (fld = 0; fld < nfield; ++fld)
	{
		DestFID = fields[fld];
		dc_ProcessFieldAttrs (src, fields[fld], NULL, CopyFieldAtts);
	}
}

