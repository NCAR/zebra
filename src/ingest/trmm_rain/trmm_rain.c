/*
 * Ingest module for TRMM rain gauge data.
 * Ingests all platforms into an irregular data chunk using the AddScalar()
 * interface on each file.
 */
/*
 *		Copyright (C) 1993 UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# ifndef lint
static char *rcsid = 
   "$Id: trmm_rain.c,v 1.4 1993-05-21 23:20:00 granger Exp $";
# endif

# include <time.h>
# include <errno.h>
# include <stdio.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ingest.h>

# define BADVAL	-9999.0

#define ds_PlatformName(a) "platname"

/*
 * Table of sites
 */
#define TIMEZONE_FILE 	"Australia/North"
#define NUM_SITES 	(sizeof(Sites)/sizeof(Sites[0]))
struct _sites
{
	char	name[5];
	char	platname[4];
	float	lat, lon;
} Sites[] = 
{
	{"Anna", "ann", -12.9142, 131.6743},
	{"Batc", "bat", -13.0517, 131.0217},
	{"Bath", "bah", -11.7761, 130.6147},
	{"Bell", "bel", -12.7597, 130.8817},
	{"Berr", "ber", -12.4572, 130.9253},
	{"Chan", "chp", -13.1665, 130.1185},
	{"Char", "car", -12.4160, 130.6246},
	{"Dum ", "dum", -12.6415, 130.3773},
	{"Gard", "gap", -11.3988, 130.4198},
	{"Good", "gom", -13.2119, 131.3715},
	{"Gunn", "gup", -12.2431, 131.0419},
	{"Hump", "hum", -12.6082, 131.2894},
	{"Kool", "kol", -12.3921, 131.1741},
	{"La B", "lab", -13.1127, 130.4893},
	{"Litc", "lit", -13.4338, 130.4800},
	{"Mand", "man", -12.4431, 130.7603},
	{"McMi", "mil", -12.5400, 131.0782},
	{"Mt. ", "mbu", -13.2295, 131.1372},
	{"Old ", "ops", -12.3510, 131.8110},
	{"Pick", "pic", -11.7671, 130.8759},
	{"Poin", "pst", -12.5851, 131.7522},
	{"Snak", "sbo", -11.4287, 130.6626},
	{"Wool", "wol", -12.3743, 131.4614},
	{"", "", 0.0000, 0.0000}
};


static DataChunk *InitScalarDC FP ((FILE *infile, FieldId fid, int *year));
static DataChunk *InitIRGridDC FP ((char *irplat_name, FieldId fid));
static void	GrabData FP ((FILE *infile, DataChunk *dc, FieldId, int year));
static time_t	YearSeconds FP ((int));
static void	ConvertYearDay FP((int year, int yday, int *month, int *mday));
static void	dc_IRAddScalarDC FP((DataChunk *irgrid_dc, 
				     DataChunk *scalar_dc,
				     int sample, int nsamples,
				     int nfield, FieldId *fields));


static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s [ingest options] [-ir <name>] file ...\n", prog);
	printf ("   -ir\tIngest to irregular platform <name> rather than\n");
	printf ("      \tindividually as scalar time series.\n");
	IngestUsage ();
}
	


main (argc, argv)
int	argc;
char	**argv;
{
	int i = 1;
	FILE *infile;
	int year;
	char *irplat_name;
	DataChunk *dc, *irdc;
	int option_ir = 0;
	FieldId fid;
	char env[50];

/*
 * Search for the IR option to see if we're supposed to do the IR plat
 * rather than individual scalars.
 */
	while (i < argc)
	{
		if (!strcmp(argv[i],"-ir") && (i + 1 < argc))
		{
			option_ir = 1;
			irplat_name = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else
			++i;
	}
/*
 * Parse general ingest options
 */
	IngestParseOptions (&argc, argv, Usage);
/*
 * We need to have at least one file name left on the command line
 */
	if (argc < 2)
	{
		Usage (argv[0]);
		exit (1);
	}
/*
 * Since we must convert from Darwin local to GMT, we require the TZ
 * environment variable to be set to TIMEZONE_FILE.  Anyone
 * know of a better way to do this?  Anyway to set out environment from
 * within the program?
 */
	sprintf (env, "TZ=%s", TIMEZONE_FILE);
	putenv (env);
#ifdef notdef
	if (!getenv("TZ") || strcmp(getenv("TZ"),TIMEZONE_FILE))
	{
		printf ("%s: environment variable 'TZ' must be set to '%s'\n",
			argv[0], TIMEZONE_FILE);
		exit (2);
	}
#endif
/*
 * Now we can set the timezone for our process
 */
	tzset();
/*
 * Connect to the data store, message, etc., and initialize
 * our irgrid data chunk, before we start barreling through the files.
 */
	IngestInitialize ("TRMM_Rain");
	fid = F_DeclareField ("rainr", "Rain gauge rates", "mm/hr");
	if (option_ir)
		irdc = InitIRGridDC (irplat_name, fid);
/*
 * Now ingest from each file left on the command line
 */
	i = 1;
	while (i < argc)
	{
	/*
	 * Open the input file
	 */
		infile = fopen (argv[i], "r");
		
		if (! infile)
		{
			printf ("Error %d opening  '%s'!\n", errno, argv[i]);
			exit (1);
		}
		dc = InitScalarDC (infile, fid, &year);
	/*
	 * Build the data chunk
	 */
		GrabData (infile, dc, fid, year);
		
	/*
	 * If not storing as an IRGrid, stored this scalar DC and continue
	 * on.  Otherwise, add the DC to the IR DC.
	 */
		if (! option_ir)
		{
			if (! ds_StoreBlocks (dc, FALSE, (dsDetail *) 0, 0))
				IngestLog (EF_EMERGENCY, 
					   "%s: Failure storing data", 
					   ds_PlatformName (dc->dc_Platform));
			else
				IngestLog (EF_INFO, "File %s ingested",
					   argv[i]);
		}
		else
		{
			IngestLog (EF_INFO,
				   "Merging scalar into IR plat '%s'",
				   irplat_name);
			dc_IRAddScalarDC (irdc, dc, 0, 0, 0, 0);
			IngestLog (EF_INFO,
				   "File %s added to IR plat '%s'",
				   argv[i], irplat_name);
		}
	/*
	 * Either way we're done with the file and the scalar DC
	 */
		fclose (infile);
		dc_DestroyDC (dc);
		++i;
	}
/*
 * If we're not doing the IR option, we're done.  Otherwise we must 
 * store the IR dc, then we're done.
 */
	if (! option_ir)
		exit (0);

	if (! ds_StoreBlocks (irdc, FALSE, (dsDetail *) 0, 0))
		IngestLog (EF_EMERGENCY, 
			   "%s: Failure storing data", 
			   ds_PlatformName (irdc->dc_Platform));
	else
		IngestLog (EF_INFO, "Successful ingestion of IR plat %s",
			   ds_PlatformName (irdc->dc_Platform));
	exit (0);
}



static DataChunk *
InitScalarDC (infile, fid, year)
FILE *infile;
FieldId fid;
int *year;
/*
 * Create our data chunk and initialize based on the first line of the input
 * file.  Initialize the irregular data chunk if that hasn't been done yet.
 */
{
	DataChunk *dc;
	int	azim, range, s;
	char	sitename[40];
	Location	loc;
/*
 * Get the site name, year, and radar-relative azimuth and range from the
 * first line of the file.  We don't use the azimuth and range since we
 * have lat/lon locations below.  We have to use the %[] construct because
 * some files use tabs rather than fixed-length character fields.  This
 * assumes the site name doesn't have any numbers in it.
 */
	fscanf (infile, " %[^0123456789]%d %d %d ", 
		sitename, /* (int *) */year, &azim, &range);
/*
 * Find this one in the site list
 */
	for (s = 0; Sites[s].name[0]; s++)
		if (! strncmp (sitename, Sites[s].name, 4))
			break;
	
	if (! Sites[s].name[0])
	{
		IngestLog (EF_EMERGENCY, "Bad site string '%s'", sitename);
		exit (1);
	}
/*
 * Create the data chunk and put in the platform ID, location, field ID, and
 * bad value flag
 */
	dc = dc_CreateDC (DCC_Scalar);

	if ((dc->dc_Platform = ds_LookupPlatform (Sites[s].platname)) == 
	    BadPlatform)
	{
		IngestLog (EF_EMERGENCY, "Cannot get  platform ID for '%s'",
			   Sites[s].platname);
		exit (1);
	}

	IngestLog (EF_INFO, "Ingesting data for %s", Sites[s].platname);

	loc.l_lat = Sites[s].lat;
	loc.l_lon = Sites[s].lon;
	loc.l_alt = 0.00;	/* We don't have altitudes available */
	dc_SetStaticLoc (dc, &loc);

	dc_SetScalarFields (dc, 1, &fid);
	dc_SetBadval (dc, BADVAL);

	return (dc);
}



static DataChunk *
InitIRGridDC (irplat_name, fid)
char *irplat_name;
FieldId fid;
{
	DataChunk *dc;
	int s;
	Location locs[ NUM_SITES ];
	PlatformId pids[ NUM_SITES ];
/*
 * Now do the IRGrid chunk using the platform info above
 */
	dc = dc_CreateDC (DCC_IRGrid);
	dc->dc_Platform = ds_LookupPlatform (irplat_name);
	if (dc->dc_Platform == BadPlatform)
	{
		IngestLog (EF_EMERGENCY, 
			   "Bad IR platform '%s', no platform id",
			   irplat_name);
		exit (1);
	}

	for (s = 0; s < NUM_SITES; ++s)
	{
		locs[s].l_lat = Sites[s].lat;
		locs[s].l_lon = Sites[s].lon;
		locs[s].l_alt = 0.00;
		pids[s] = ds_LookupPlatform (Sites[s].platname);
	}
	dc_IRSetup (dc, NUM_SITES, pids, locs, 1, &fid);
	dc_SetBadval (dc, BADVAL);

	return (dc);
}



static void
GrabData (infile, dc, fid, year)
FILE *infile;
DataChunk *dc;
FieldId fid;
int year;
/*
 * Put all the data into the data chunk
 */
{
	ZebTime	t;
	struct tm local;
	int	ndx = 0, prevday = 0, num, jday, hour, minute, second;
	int	month, mday;
	float	rate;
	char	msg[100];

	/* yearsec = YearSeconds(year); */
	local.tm_sec = 0;
	local.tm_year = year - 1900;
	local.tm_wday = 0;
	local.tm_yday = 0;
	local.tm_zone = NULL;
	local.tm_gmtoff = 0;
	t.zt_MicroSec = 0;

	while ((num = fscanf (infile, " %d %d:%d:%d %f ", 
			      &jday, &hour, &minute, &second, &rate)) == 5)
	{
	/*
	 * Handle year change if necessary
	 */
		if (jday < prevday)
		{
			++year;
			local.tm_year = year - 1900;
		}
		prevday = jday;
	/*
	 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	 * According to email from Matthias Steiner, times are LOCAL,
	 * so here we must adjust final time from local to GMT, assuming
	 * the email is correct.  Anybody know the timezone of Darwin?
	 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	 */
	/*
	 * Construct a tm structure and use it to convert the file's time
	 * to UTC using timelocal().  We're assuming that the program is
	 * running with the correct value for the TZ environ variable,
	 * set in main().
	 */
		ConvertYearDay (year, jday, &month, &mday);
		local.tm_min = minute;
		local.tm_hour = hour;
		local.tm_mday = mday;
		local.tm_mon = month - 1;
		t.zt_Sec = (unsigned long) timelocal (&local);
	/*
	 * Throw this sample into the data chunk
	 */
		dc_AddScalar (dc, &t, ndx++, fid, &rate);
		if ((ndx % 250) == 0)
			IngestLog (EF_DEBUG, "%d points read", ndx);
		TC_EncodeTime (&t, TC_Full, msg);
		sprintf (msg+strlen(msg)," %f ", rate);
		IngestLog (EF_DEVELOP, "%s", msg);
	}

	if (num > 0)
		IngestLog (EF_PROBLEM, "Stopping at bad data line");

	IngestLog (EF_INFO, "%d good data points", ndx);
}



static void
ConvertYearDay (year, yday, month, mday)
	int year;	/* year, four digits		*/
	int yday;	/* day of the year, 1..366	*/
/* RETURNS: */
	int *month;	/* month of year, 1..12 	*/
        int *mday;	/* day of month, 1..31 		*/
{
	static int Mdays[] = {0, 31, 28, 31, 30, 31, 30, 31, 
				      31, 30, 31, 30, 31};

	if ((year % 4) == 0)
		Mdays[2] = 29;	/* February has 29 days in leap years */
	*month = 1;
	while (yday > Mdays[*month])
		yday -= Mdays[(*month)++];
	Mdays[2] = 28;
	*mday = yday;
}



#ifdef notdef
static time_t
YearSeconds (year)
/*
 * Turn the year into a UNIX time
 */
{
	struct tm	t;

	t.tm_year = year - 1900;
	t.tm_sec = t.tm_min = t.tm_hour = t.tm_mon = 0;
	t.tm_mday = 1;
	t.tm_zone = (char *) 0;
	t.tm_wday = t.tm_isdst = t.tm_yday = 0;
	return (timegm (&t));
}
#endif



static void
dc_IRAddScalarDC (irgrid_dc, scalar_dc, sample, nsample, nfield, fields)
DataChunk *irgrid_dc;
DataChunk *scalar_dc;
int sample;
int nsample;
int nfield;
FieldId *fields;
/*
 * adds a Scalar chunk of data to an irgrid chunk, taking nsample's of data
 * beginning at sample, using nfield fields whose ids are listed in
 * 'fields'.  NULL 'fields' or zero 'nfield' implies use all those that
 * exist in the IRGrid DC.  Zero 'nsample' implies take them all.
 *
 * When a new sample is being created---either inserted, appended, or
 * prepended---fill it in with bad values first.  Otherwise, just change
 * the data value of the Scalar chunk's platform in the sample.  For now,
 * try to take advantage of non-chronological times.
 */
{
	FieldId *fids;
	ZebTime sc_zt;
	int i, f, d, s;
	int ir_nsample;
	PlatformId *platforms;
	int nplat;
	float badval;
	float *blank_grid;
	float *ir_data;
	ZebTime *ir_times;
	float sc_data;
	int sc_plat_idx;	/* index of scalar plat into irgrid list */
/*
 * Set up the list of samples and fields to use.
 */
	if (nsample == 0)	/* default to the whole thing */
	{
		sample = 0;
		nsample = dc_GetNSample (scalar_dc);
	}
	fids = fields;
	if (fids == NULL || nfield == 0)
	{
		fids = dc_GetFields (scalar_dc, &nfield);
	}
/*
 * Get some information about the irgrid we're storing to, and set up a
 * 'blank' irgrid sample full of bad values.
 */
	ir_nsample = dc_GetNSample (irgrid_dc);
	nplat = dc_IRGetNPlatform (irgrid_dc);
	platforms = (PlatformId *)malloc(nplat * sizeof(PlatformId));
	dc_IRGetPlatforms (irgrid_dc, platforms, NULL);
	badval = dc_GetBadval (irgrid_dc);
	blank_grid = (float *)malloc(nplat * sizeof(float));
	for (d = 0; d < nplat; ++d)
		blank_grid[d] = badval;
/*
 * Find the scalar platform in the list of irgrid platforms
 */
	for (i = 0; i < nplat; ++i)
	{
		if (platforms[i] == scalar_dc->dc_Platform)
			break;
	}
	if (i < nplat)
		sc_plat_idx = i;
	else
	{
		/* scalar platform not in IRGrid, abandon efforts */
		free (platforms);
		free (blank_grid);
		return;
	}
/*
 * Construct a list of times from the irgrid chunk so that we don't
 * have to query the chunk in each loop.
 */
	ir_times = (ZebTime *)malloc (sizeof(ZebTime)*(nsample + ir_nsample));
	for (i = 0; i < ir_nsample; ++i)
		dc_GetTime (irgrid_dc, i, ir_times+i);
/*
 * For each field in the field list, and then for each sample, extract
 * the data.  Try to retrieve a sample from the irgrid for the same time
 * and the same field.
 */
	for (f = 0; f < nfield; ++f)
	{
		for (i = sample; i < sample + nsample; ++i)
		{
			dc_GetTime (scalar_dc, i, &sc_zt);
			sc_data = dc_GetScalar (scalar_dc, i, fids[f]);
			for (s = 0; s < ir_nsample; ++s)
			{
				if (TC_Eq(ir_times[s], sc_zt))
					break;
			}
			if (s < ir_nsample)	/* found a sample */
			{
				ir_data = dc_IRGetGrid (irgrid_dc, i, fids[f]);
			}
			else			/* no sample, use bad values */
			{
				ir_data = blank_grid;
			}
		/*
		 * Now we have a grid to modify according to what platform
		 * we're trying to add
		 */
			ir_data[sc_plat_idx] = sc_data;
		/*
		 * If this was a new grid, we need to add it back to the ir_dc
		 */
			if (ir_data == blank_grid)
			{
				ir_times[ir_nsample] = sc_zt;
				dc_IRAddGrid (irgrid_dc, &sc_zt,
					      ir_nsample++, fids[f],
					      ir_data);
				blank_grid[sc_plat_idx] = badval;
			}
		/*
		 * Otherwise we're done and we can move on to the next
		 * sample from the scalar chunk.
		 */
		}
	}

	free (platforms);					
	free (blank_grid);
	free (ir_times);
}
