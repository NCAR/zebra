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
   "$Id: trmm_rain.c,v 1.9 1995-07-10 13:14:02 granger Exp $";
# endif

# include <assert.h>
# include <time.h>
# include <errno.h>
# include <stdio.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ingest.h>

# define BADVAL	-9999.0
# define DC_SIZE_LIMIT (65000)

# define PlatformName(pid) ((NoDataStore)?"platname":ds_PlatformName(pid))
# define StoreBlocks(a,b,c,d) ((NoDataStore)?(TRUE):\
			       (ds_StoreBlocks(a,b,c,d)))

/*
 * Table of sites
 */
#define TIMEZONE_FILE 	"Australia/North"
/*#define TIMEZONE_FILE	"GMT"	*/
#define NUM_SITES 	((sizeof(Sites)/sizeof(Sites[0])) - 1)
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

/*
 * The number of the month to limit our ingestion to, 1..12
 */
int Month = 0;
int Day = 0;	/* Day of the month to limit ingestion to */
short OptionIR = 0;	/* True when ingesting an irgrid  */

static DataChunk *InitScalarDC FP ((FILE *infile, FieldId fid, int *year));
static DataChunk *InitIRGridDC FP ((char *irplat_name, FieldId fid));
static DataChunk *CopyScalarDC FP ((DataChunk *src));
static void	GrabData FP ((FILE *infile, DataChunk **dc, int *ndc,
			      FieldId, int *year));
static void	ConvertYearDay FP((int year, int yday, int *month, int *mday));
static inline int MonthNDays FP((int month, int year));
static void IngestFiles FP((int argc, char **argv, char *irplat_name,
			    int *year));


static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s [ingest options] ", prog);
	printf ("[-ir <name>] [-m <month> [-d <day>]] file ...\n");
	printf ("   -ir\tIngest to irregular platform <name> rather than\n");
	printf ("      \tindividually as scalar time series.\n");
	printf ("   -m \tIngest data for this month only, where <month>\n");
	printf ("      \tis in the range 1..12 (UTC time).\n");
	printf ("      \tRecommended when using -ir with large datasets.\n");
	printf ("   -d \tIngest data for this day of the given month,");
	printf (" 1..31\n");
	printf ("      \tWithout -d, data for a whole month is ");
	printf ("ingested one day at a time.\n");
	IngestUsage ();
}
	

static void
ParseOptions (rargc, argv, irplat_name)
int *rargc;
char *argv[];
char **irplat_name;
{
	int i = 1;
	int argc = *rargc;

	while (i < argc)
	{
		if (!strcmp(argv[i],"-ir") && (i + 1 < argc))
		{
			OptionIR = 1;
			*irplat_name = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else if (!strcmp(argv[i],"-m") && (i + 1 < argc))
		{
			Month = atoi(argv[i+1]);
			if ((Month < 1) || (Month > 12))
			{
				printf ("%s: bad month number\n", argv[0]);
				Usage (argv[0]);
				exit (1);
			}
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else if (!strcmp(argv[i],"-d") && (i + 1 < argc))
		{
			Day = atoi(argv[i+1]);
			if ((Day < 1) || (Day > 31))
			{
				printf ("%s: bad day number\n", argv[0]);
				Usage (argv[0]);
				exit (1);
			}
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else
			++i;
	}
	*rargc = argc;
}



main (argc, argv)
int	argc;
char	**argv;
{
	char *irplat_name;
	char env[50];
	int year;

	ParseOptions (&argc, argv, &irplat_name);
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
 * environment variable to be set to TIMEZONE_FILE.
 */
	sprintf (env, "TZ=%s", TIMEZONE_FILE);
	putenv (env);
/*
 * Now we can set the timezone for our process
 */
	tzset();
/*
 * Connect to the data store, message, etc., and initialize
 * our irgrid data chunk, before we start barreling through the files.
 */
	IngestInitialize ("TRMM_Rain");
	IngestLog (EF_INFO, "Using '%s' as timezone of local times", env);
/*
 * Now ingest from each file left on the command line.  If we're limiting
 * to one whole month, do it one day at a time.
 */
	if (Month && (Day == 0))
	{
	        Day = 1;
		do {
			IngestFiles (argc, argv, irplat_name, &year);
			++Day;
		} while (Day <= MonthNDays(Month, year));
	}
	else
		IngestFiles (argc, argv, irplat_name, &year);

	exit (0);
}



static void
IngestFiles (argc, argv, irplat_name, year)
int argc;
char *argv[];
char *irplat_name;
int *year;
{
	FILE *infile;
	DataChunk *dc[10], *irdc[10];
	int ndc;
	int i,n;
	FieldId fid;

	fid = F_DeclareField ("rainr", "Rain gauge rates", "mm/hr");

	if (Month && Day)
		IngestLog (EF_INFO, "Reading month #%d, day #%d", 
			   Month, Day);
	else if (Month)
		IngestLog (EF_INFO, "Limiting data to #%d", Month);

	for (n = 0; n < 10; ++n)
		irdc[n] = NULL;

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
		dc[0] = InitScalarDC (infile, fid, year);
	/*
	 * Build the data chunks
	 */
		GrabData (infile, dc, &ndc, fid, year);
	/*
	 * If not storing as an IRGrid, store as a scalar DC and continue
	 * on.  Otherwise, add the DC to the IR DC.  Note we are counting
	 * on the gap-filling in GrabData() to mesh all of the scalar chunks
	 * and prevent BadValue's from appearing in any of the grids.
	 */
		if (dc_GetNSample (dc[0]) == 0)
			IngestLog (EF_PROBLEM, "No samples read from file");
		if (! OptionIR)
		{
		    for (n = 0; i < ndc; ++n)
		    {
			    if (DumpDataChunks)
				    dc_DumpDC (dc[n]);
			if (! StoreBlocks (dc[n], FALSE, (dsDetail *) 0, 0))
				IngestLog (EF_EMERGENCY, 
					   "%s: Failure storing data", 
					   PlatformName (dc[0]->dc_Platform));
			else
				IngestLog (EF_INFO, "File %s ingested",
					   argv[i]);
		    }
		}
		else
		{
			int nsamp = 0;

			IngestLog (EF_INFO,
				   "Merging scalar into IR plat '%s'",
				   irplat_name);
			for (n = 0; n < ndc; ++n)
			{
				if (!irdc[n])
					irdc[n] = InitIRGridDC (irplat_name,
								fid);
				dc_IRAddScalarDC (irdc[n], dc[n], 0, 0, 0, 0);
				nsamp += dc_GetNSample(irdc[n]);
			}
			IngestLog (EF_INFO,
		   "File %s merged into IR plat '%s'; now %i samples in DC",
		   argv[i], irplat_name, nsamp);
		}
	/*
	 * Either way we're done with the file and the scalar DC
	 */
		fclose (infile);
		for (n = 0; n < ndc; ++n)
			dc_DestroyDC (dc[n]);
		++i;
	}
/*
 * If we're not doing the IR option, we're done.  Otherwise we must sort
 * the IR dc to avoid any insert cases, store the IR dc, and then we're done.
 */
	if (! OptionIR)
		return ;

	for (n = 0; n < ndc; ++n)
	{
		IngestLog (EF_DEBUG, "Sorting irgrid samples...");
		dc_SortSamples (irdc[n]);
		IngestLog (EF_DEBUG, "Sorting complete.");
		if (DumpDataChunks)
			dc_DumpDC (irdc[n]);
		if (! StoreBlocks (irdc[n], FALSE, (dsDetail *) 0, 0))
		{
			IngestLog (EF_EMERGENCY, 
				   "%s: Failure storing datachunk %d", 
				   irplat_name, n);
			exit (5);
		}
		
		IngestLog (EF_INFO, "Successful ingest of IR plat '%s'",
			   irplat_name);
		dc_DestroyDC (irdc[n]);
	}
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
CopyScalarDC (src)
DataChunk *src;
/*
 * Create our data chunk and initialize based on the first line of the input
 * file.  Initialize the irregular data chunk if that hasn't been done yet.
 */
{
	DataChunk *dc;
	int nfields;
	FieldId *fids;
	Location	loc;
/*
 * Create the data chunk and put in the platform ID, location, field ID, and
 * bad value flag from the src datachunk
 */
	dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = src->dc_Platform;

	fids = dc_GetFields (src, &nfields);
	dc_SetScalarFields (dc, nfields, fids);
	dc_SetBadval (dc, dc_GetBadval(src));
	dc_GetLoc (src, 0, &loc);
	dc_SetStaticLoc (dc, &loc);
	
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
		if (pids[s] == BadPlatform)
		{
			IngestLog (EF_EMERGENCY,
			   "site '%s', plat '%s', bad sub-platform for IRGRID",
			   Sites[s].name, Sites[s].platname);
			exit (1);
		}
	}
	dc_IRSetup (dc, NUM_SITES, pids, locs, 1, &fid);
	dc_SetBadval (dc, BADVAL);

	return (dc);
}



static void
FillGap (dc, idc, t1, t2, ndx, fid)
DataChunk **dc;
int *idc;
unsigned long t1;
unsigned long t2;
int *ndx;
FieldId fid;
/*
 * Fills a gap between t1 and t2, NON-INCLUSIVE.  
 * No sample will be stored for either t1 or t2.
 * t1 and t2 should be multiples of 60, i.e. in even minutes.
 */
{
	ZebTime tmp;
	float data;
	unsigned long l;
	
	tmp.zt_MicroSec = 0;
	data = 0;
	if (t2 - t1 > 60)
		IngestLog (EF_DEBUG,
			   "Filling gap of %d minutes",
			   (t2 - t1)/60 - 1);
	for (l = t1 + 60; l < t2; l += 60)
	{
		tmp.zt_Sec = l;
		dc_AddScalar (dc[*idc], &tmp, (*ndx)++, fid, &data);
		if (*ndx >= DC_SIZE_LIMIT)
		{
			IngestLog (EF_DEBUG, "Creating DataChunk #%d", *idc+2);
			dc[*idc+1] = CopyScalarDC (dc[*idc]);
			++(*idc);
			*ndx = 0;
		}

	}
}




static void
GrabData (infile, dc, ndc, fid, ryear)
FILE *infile;
DataChunk **dc;
int *ndc;
FieldId fid;
int *ryear;	/* year used for the Month and Day being ingested */
/*
 * Put all the data in the file into the data chunk.
 * Fill in time gaps with zeroes.  If Month is not set, no limit is put
 * on the times of the samples, otherwise both Month and Day must be
 * valid, and data is limited to that Month and Day.
 *
 * Because of the gap filling, the datachunks get large fast.  Hence we
 * need to create new datachunks as needed.  We expect dc[0] to exist,
 * the others are copied from it.
 */
{
	ZebTime	t, last, end;
	struct tm local;
	int	ndx = 0, prevday = 0, num, jday, hour, minute, second;
	int	month, mday;
	float	rate;
	char	msg[100];
	int idc;
	int year = *ryear;

	assert((Month == 0) || ((Month > 0) && (Day > 0)));
	local.tm_sec = 0;	/* seconds of all sample times are 0'ed */
	local.tm_year = year - 1900;
	local.tm_wday = 0;
	local.tm_yday = 0;
	local.tm_isdst = -1;
	t.zt_MicroSec = 0;	/* we won't be using micro-seconds */
	last.zt_MicroSec = last.zt_Sec = 0;
	end.zt_MicroSec = end.zt_Sec = 0;
	idc = 0;

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
			*ryear = year;
		}
		prevday = jday;
	/*
	 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	 * According to email from Matthias Steiner, times are LOCAL,
	 * so here we must adjust final time from local to GMT, assuming
	 * the email is correct.  Anybody know the timezone of Darwin?
	 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	 */
		ConvertYearDay (year, jday, &month, &mday);
	/*
	 * Construct a tm structure and use it to convert the file's time
	 * to UTC using timelocal().  We're assuming that the program is
	 * running with the correct value for the TZ environ variable,
	 * set in main().
	 */
		local.tm_min = minute;
		local.tm_hour = hour;
		local.tm_mday = mday;
		local.tm_mon = month - 1;
		t.zt_Sec = mktime (&local);
	/*
	 * If we have an end time, and t is later, we know we can
	 * abort now.
	 */
		if (end.zt_Sec && (t.zt_Sec >= end.zt_Sec))
			break;
	/*
	 * If we've at least found the correct month, use it to
	 * set up our day boundaries.  If we find no data, the day
	 * will be filled with zeroes.
	 */
		TC_ZtSplit (&t, 0, &month, &mday, 0, 0, 0, 0);
		if (Month && (month != Month))
			continue;
	/*
	 * If restricting samples to a particular day, last should
	 * be set to the beginning of the day.  Find it by
	 * "zero"ing the hours and minutes.  The end of our day is set
	 * in 'end' by adding 24 hours.
	 */
		if (!last.zt_Sec && Month)
		{
			TC_ZtAssemble (&last, year - 1900, Month, 
				       Day, 0, 0, 0, 0);
			end.zt_Sec = last.zt_Sec + 24*60*60;
			last.zt_Sec -= 60;
		}
	/*
	 * Skip this point if not in the day wanted
	 */
		if (mday != Day)
			continue;
	/*
	 * If we have a last time, fill any gap between it and this sample
	 */
		if (last.zt_Sec)
		{
			FillGap (dc, &idc, last.zt_Sec, t.zt_Sec, &ndx, fid);
		}
		last.zt_Sec = t.zt_Sec;
	/*
	 * Throw (toss, lob, volley) this sample into the data chunk
	 */
		dc_AddScalar (dc[idc], &t, ndx++, fid, &rate);
		if ((ndx % 250) == 0)
			IngestLog (EF_DEBUG, "%d points read", ndx);
		if (ndx >= DC_SIZE_LIMIT)
		{
			IngestLog (EF_DEBUG, "Creating DataChunk #%d", idc+2);
			dc[idc+1] = CopyScalarDC (dc[idc]);
			++idc;
			ndx = 0;
		}
		TC_EncodeTime (&t, TC_Full, msg);
		sprintf (msg+strlen(msg)," %f ", rate);
		IngestLog (EF_DEVELOP, "%s", msg);
	}

	if ((num > 0) && (num != 5))
		IngestLog (EF_PROBLEM, "Stopping at bad data line");
/*
 * We have the first minute of the next in end, so fill any gap
 * between our last sample and the end of the day.  Note that
 * no sample is stored for the zero minute of the next day.
 */
	if (Month && last.zt_Sec)
	{
		FillGap (dc, &idc, last.zt_Sec, end.zt_Sec, &ndx, fid);
	}

	IngestLog (EF_INFO, "%d data points, including gaps", 
		   ndx + (idc * DC_SIZE_LIMIT));
	*ndc = idc + 1;
}



static void
ConvertYearDay (year, yday, month, mday)
	int year;	/* year, four digits		*/
	int yday;	/* day of the year, 1..366	*/
/* RETURNS: */
	int *month;	/* month of year, 1..12 	*/
        int *mday;	/* day of month, 1..31 		*/
{
	int mdays;

	*month = 1;
	while (yday > (mdays = MonthNDays(*month, year)))
	{
		yday -= mdays;
		++(*month);
	}
	*mday = yday;
}



static inline 
int
MonthNDays (month, year)
int month;		/* month of year, 1..12 */
int year;		/* year, four digits	*/
{
	static const int Mdays[] = {0, 31, 28, 31, 30, 31, 30, 31, 
				    31, 30, 31, 30, 31};

	if ((month == 2) && (year % 4 == 0))
		return (29);
	else
		return (Mdays[month]);
}
