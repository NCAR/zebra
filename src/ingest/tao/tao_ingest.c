/*
 * $Id: tao_ingest.c,v 1.1 1993-05-18 20:24:22 granger Exp $
 *
 * Ingest TAO moorings data as an irregular grid
 *
 */
/*		Copyright (C) 1987-92 by UCAR
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

#include "ingest.h"
#include <strings.h>
#include <copyright.h>

#ifndef lint
MAKE_RCSID("$Id: tao_ingest.c,v 1.1 1993-05-18 20:24:22 granger Exp $")
#endif

# define NUMBER(arr)		((unsigned long)(sizeof(arr)/sizeof(arr[0])))
# define INGEST_NAME 		"TAO Ingest"
# define BADVAL 		-999.0
# define FIRST_GUESS_SIZE	(14 * NUMBER(TaoFields))	/* 2 weeks worth    */
# define STEP_SIZE		(14 * NUMBER(TaoFields))	/* Add two weeks    */
# define DAYS(a,b)		(((b).zt_Sec - (a).zt_Sec)/(24 * 60 * 60))

/*
 * Describe the TAO fields and units in static global arrays.  The names and
 * units are the same order as in the files, except that the first column,
 * Julian day, is ignored.  The array of field id's is filled in elsewhere.
 */
struct field_s {
	char 	*long_name;
	char	*field_name;
	char 	*units;
}
TaoFields[] = 
	   {
		   { "u component of wind vector", "u_wind", "m/s" },
		   { "v component of wind vector", "v_wind", "m/s" },
		   { "relative humidity",	   "rh",     "%" },
		   { "sea-surface temperature",    "sst",    "degC" },
		   { "air temperature",		   "ta",     "degC" }
	   };

FieldId TaoFieldIds[ NUMBER(TaoFields) ];


static void	Usage FP((char *prog_name));
static void	GetLocation FP((Location *locn, const char *name));
static void	DefineFields();
static float    *ReadFileData FP((const char *filename, ZebTime *start, 
				  ZebTime *end));
static void 	ZebTimeFromJulian FP((ZebTime *zt, unsigned long jday));


int main (argc, argv)
	int argc;
	char **argv;
{
	ZebTime *times;		/* Times of daily samples from first to last */
	int ntimes;		/* Number daily samples bet first and last   */
	ZebTime first, last;	/* Earliest/latest sample times among plats  */
	ZebTime *begins, *ends;	/* Begin & end times of each platform's data */
	int nsamples;		/* Number of samples, or pts, in the file    */
	DataChunk *dc;   	/* The DataChunk we will be building 	     */
	Location *locns;	/* Locations for each platform 		     */
	PlatformId *pids;	/* Platform id of each subplatform	     */
	int nplats;		/* Number of files (plats) on command line   */
	char *plat;		/* Pts to plat name in file path of datafile */
	float **platdata;	/* Arrays of float data read from each file  */
	PlatformId parent;	/* parent platform of all of the subplatforms*/
	float *fieldgrid;	/* Data for 1 grid, 1 field, in plat order   */
	int t, i, fld;
	char echo[100];

	IngestParseOptions(&argc, argv, Usage);
/*
 * Initialize usy, message, DataStore, and fields all at once
 */
	IngestInitialize(INGEST_NAME);

	if (argc < 2)
	{
		fprintf(stderr,"Need a platform name!\n");
		Usage(argv[0]);
		exit(1);
	}
	else if ((parent = ds_LookupPlatform(argv[1])) == BadPlatform)
	{
		IngestLog(EF_PROBLEM,"%s: bad platform name\n",argv[1]);
		exit(1);
	}
	IngestRemoveOptions(&argc, argv, 1, 1);

	if (argc < 2)		/* Need a file name arg */
	{
		printf("%s: need at least one file name\n",argv[0]);
		Usage(argv[0]);
		exit(1);
	}

	nplats = argc - 1;
	locns = (Location *)malloc(nplats * sizeof(Location));
	pids = (PlatformId *)malloc(nplats * sizeof(PlatformId));
	platdata = (float **)malloc(nplats * sizeof(float *));
	fieldgrid = (float *)malloc(nplats * sizeof(float));
	begins = (ZebTime *)malloc(nplats * sizeof(ZebTime));
	ends = (ZebTime *)malloc(nplats * sizeof(ZebTime));
	for (i = 0; i < nplats; ++i)
	{
		if (!(plat = (char *)strrchr(argv[i+1],'/')))
			plat = argv[i+1];
		else
			plat++;
		if ((pids[i] = ds_LookupPlatform (plat)) == BadPlatform)
		{
			fprintf (stderr,"Unknown platform: %s", plat);
			exit(1);
		}
		GetLocation(&locns[i], plat);
		IngestLog(EF_DEBUG, 	
			  "Reading platform: %s, %3.0f lat, %3.0f lon", 
			  plat, locns[i].l_lat, locns[i].l_lon);
	/*
	 * Read this file.  Get its begin time, end time, and data.  Data is
	 * stored in sample-major order: 
	 * t1: f1, f2, f3, ...; t2: f1, f2, f3, ...; ...
	 */
		platdata[i] = ReadFileData(argv[i+1], &begins[i], &ends[i]);
	}

/*
 * At this point, we have the locations, ids, and data of all our
 * platforms.  Now, for each field, and then for each sample time, and
 * finally for each platform we put the data into a one commmon array, one
 * field value per platform.  Platforms without data for the sample time
 * are assigned bad values.  Then platform array is then added to the dc
 * with dc_IRAddGrid().  We loop over the smallest array of sample times
 * which includes all times for all platforms.  The earliest 'first' and
 * latest 'last' become the first and last for the 'times' array, and every
 * subplats data must be fit into an array of 'ntimes' samples.  Any
 * platform which has no data for a given sample will be filled in with bad
 * values.
 */
	first.zt_Sec = 0;
	last.zt_Sec = 0;
	for (i = 0; i < nplats; ++i)
	{
		if (!platdata[i])	/* skip platforms with no data */
			continue;
		if (!first.zt_Sec || TC_Less(begins[i],first))
			first = begins[i];
		if (!last.zt_Sec || TC_Less(last,ends[i]))
			last = ends[i];
	}
	if (!first.zt_Sec)
	{
		IngestLog(EF_PROBLEM,"No data from any platforms!");
		exit(10);
	}
/*
 * Generate the times array, daily samples from 'first' to 'last'
 */
	ntimes = DAYS(first,last) + 1;
	times = (ZebTime *)malloc(ntimes * sizeof(ZebTime));
	sprintf(echo,"Generating %i daily samples, ",ntimes);
	TC_EncodeTime(&first, TC_Full, echo+strlen(echo));
	strcat(echo," <-> ");
	TC_EncodeTime(&last, TC_Full, echo+strlen(echo));
	IngestLog(EF_INFO,"%s",echo);
	times[0] = first;
	for (t = 1; t < ntimes; ++t)
	{
		times[t].zt_MicroSec = 0;
		times[t].zt_Sec = times[t-1].zt_Sec + (24*60*60);
	}

	DefineFields();
	dc = dc_CreateDC(DCC_IRGrid);
	dc->dc_Platform = parent;
	dc_IRSetup (dc, nplats, pids, locns, NUMBER(TaoFields), TaoFieldIds);
	dc_SetBadval (dc, BADVAL);
	IngestLog(EF_DEBUG,"bad_value_flag set to %6.2f",BADVAL);
	for (fld = 0; fld < NUMBER(TaoFields); fld++)
	{
		for (t = 0; t < ntimes; t++)
		{
			for (i = 0; i < nplats; ++i)
			{
				if (!platdata[i]
				    || TC_Less(times[t],begins[i])
				    || TC_Less(ends[i],times[t]))
				{
					fieldgrid[i] = BADVAL;
				}
				else
				{
					fieldgrid[i] = 
					(platdata[i])[DAYS(begins[i],times[t])
						      *NUMBER(TaoFields)+fld];
				}
			}
			dc_IRAddGrid (dc, times+t, t, TaoFieldIds[fld], 
				      fieldgrid);
		}
	}
/*
 * Put it into the data store.
 */
	ds_Store (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);

	IngestLog(EF_DEBUG, "Exiting...");
	exit (0);
}



static void
ZebTimeFromJulian(zt, jday)
	ZebTime *zt;		/* where to store ZebTime */
	unsigned long jday;	/* Julian day to convert  */
/*
 * Convert Julian day to ZebTime
 *
 * Julian day - 2448622 = day of year 1992 (1..366)
 * Julian day - 2448988 = day of year 1993 (1..365)
 * so Julian day 2448989 is Jan 1, 1993
 */
{
	static ZebTime base = { 0, 0 };

	if (!base.zt_Sec)
		TC_ZtAssemble (&base, 93, 1, 1, 0, 0, 0, 0);
	zt->zt_Sec = base.zt_Sec + ((jday - 2448989UL) * 24 * 60 * 60);
}



static void
GetLocation (locn, name)
	Location *locn;
	const char *name;
/*
 * Parse the TAO platform name (file name) into a location.  The file name
 * should be of the form deg{n|s}deg{e|w}.  Anything else causes an error and 
 * exits.
 */
{
	int ilat, ilon;
	char hemlat, hemlon;

	if (sscanf(name,"%i%c%i%c", &ilat, &hemlat, &ilon, &hemlon) != 4)
	{
		IngestLog(EF_EMERGENCY,
			  "%s: could not extract location from name\n",name);
		exit(2);
	}
	locn->l_lat = (float)ilat;
	locn->l_lon = (float)ilon;
	locn->l_alt = 0;
	switch (hemlat)
	{
	   case 'N','n':
		break;
	   case 'S','s':
		locn->l_lat = -(locn->l_lat);
		break;
	   default:
		IngestLog(EF_EMERGENCY,
			  "%s: lat hemisphere: got %c, wanted N or S\n",
			  name, hemlat);
		exit(3);
		break;
	}
	switch (hemlon)
	{
	   case 'E','e':
		break;
	   case 'W','w':
		locn->l_lon = -(locn->l_lon);
		break;
	   default:
		IngestLog(EF_EMERGENCY,
			  "%s: lon hemisphere: got %c, wanted E or W\n",
			  name, hemlon);
		exit(4);
		break;
	}
}



static float *
ReadFileData(filename, start, end)
	const char *filename;
	ZebTime *start;
	ZebTime *end;
{
	FILE *infile;
	float *data;
	float val;
	int i, fld;		/* counters				    */
	unsigned long jday;	/* Julian day read for each line	    */
	unsigned long jyesterday;
	int ndata;		/* no. of floats available in data array    */
	int nread;		/* no. floats actually read into data array */
	char echo[100];		/* feedback messages			    */
	int code;		/* scanf return codes 			    */

	/*
	 * Open the file and start reading.  Start with an approximation of
	 * space required for 'data' and realloc if necessary.  Be aware
	 * that files may be empty, in which case we return NULL in *data
	 */
	infile = fopen(filename, "r");
	if (!infile)
	{
		IngestLog(EF_EMERGENCY, "Could not open file %s", filename);
		perror(filename);
		exit(5);
	}

	/*
	 * first guess for data array size
	 */
	ndata = FIRST_GUESS_SIZE;
	data = (float *)malloc(ndata * sizeof(float));

	nread = 0;
	while (!feof(infile))
	{
		/*
		 * Read this line of the file, first the date, which becomes
		 * the 'start' the first time around, and then a float for
		 * each of the TaoFields we need.  If any scanf fails, we exit.
		 */
		code = fscanf(infile," %lu ",&jday);
		if (code == EOF)
			break;
		else if (code != 1)
		{
			IngestLog(EF_EMERGENCY,
				  "%s: could not read date, %i data read\n",
				  filename, nread);
			exit(6);
		}
		/*
		 * This date, if not the start date, should be one day from
		 * the previous, else we fill in the skipped days with bad
		 * values.  Note this requires that the data lines at least
		 * be chronological.
		 */
		if (!nread)	/* first line */
		{
			ZebTimeFromJulian(start, jday);
		}
		else
		{
			if (jday - jyesterday != 1)
			{
				IngestLog(EF_PROBLEM,
			"%s: filling in %i missing days between %lu and %lu",
					  filename, (jday-jyesterday)-1, 
					  jyesterday, jday);
				data = (float *)
					realloc(data, 
						(ndata + (jday - jyesterday) *
						       NUMBER(TaoFields)) *
						sizeof(float));
				for (i = 1; i < (jday - jyesterday); ++i)
				{
					for (fld = 0; 
					     fld < NUMBER(TaoFields); ++fld)
						data[nread++] = BADVAL;
				}
			}
		}
		ZebTimeFromJulian(end, jday);
		jyesterday = jday;
		TC_EncodeTime(end, TC_DateOnly, echo);
		/*
		 * We now have a date, try to read the fields from the rest
		 * of the line.  First we make sure we'll enough room for
		 * another line of data.
		 */
		if (nread + NUMBER(TaoFields) > ndata)
		{
			data = (float *)realloc(data, 
					(ndata + STEP_SIZE) * sizeof(float));
			ndata += STEP_SIZE;
			IngestLog(EF_DEBUG,"%s: increasing data array to %i",
				  filename, ndata);
		}
		for (fld = 0; fld < NUMBER(TaoFields); ++fld)
		{
			if (fscanf(infile," %f ",&val) != 1)
			{
				IngestLog(EF_EMERGENCY,
			  "%s: after %i read, expected field #%i not found",
				  filename, nread, (fld+1));
				exit(7);
			}
			else
			{
				data[nread++] = val;
				sprintf(echo+strlen(echo)," %6.1f %s", 
					val, TaoFields[fld].units);
			}
		}
		IngestLog(EF_DEVELOP,"%s",echo);
		/*
		 * Now have this day's data, and 'start' and 'end' are valid.
		 * On to the next line!
		 */
	}
	if (!nread)	/* got nothin' */
	{
		free (data);
		data = NULL;
		IngestLog(EF_PROBLEM,"%s: file is empty",filename);
	}
	else
	{
		/* Reduce our space use to as little as we need */
		data = (float *)realloc(data, nread * sizeof(float));
		IngestLog(EF_DEBUG,
			  "%s: %i values read, %i days @ %i fields/day", 
			  filename, nread, nread/NUMBER(TaoFields), 
			  NUMBER(TaoFields));
	}
	return(data);
}


		

static void
Usage(prog)
	char *prog;
{
	printf ("Usage: %s [options] <platform> <datafile> ...\n",prog);
	printf ("<platform> is the name of the irregular grid platform\n");
	printf ("<datafile> names serve as the subplatform names and as\n");
	printf ("   the platform's location, of the form deg{n|s}deg{e|w}\n");
	IngestUsage();
}



static void
DefineFields()
{
	int f;

	for (f = 0; f < NUMBER(TaoFields); ++f)
	{
		TaoFieldIds[f] = F_DeclareField(TaoFields[f].field_name,
						TaoFields[f].long_name,
						TaoFields[f].units);
		IngestLog(EF_DEBUG,"defining %s (%s): %s",
			  TaoFields[f].field_name,
			  TaoFields[f].units,
			  TaoFields[f].long_name);
	}
}
