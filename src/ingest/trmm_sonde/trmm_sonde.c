/*
 * Ingest module for TRMM sonde and wind data.  Since both data files have
 * identical formats, this program is used to ingest both as scalar data.
 * The actual platform will be specified on the command line; the platform
 * name should probably include the station number.
 *
 * Each sounding begins on the hour, and successive samples in the same
 * sounding are separated by 1 second.  Source message codes are stored
 * as per-sample attributes, though the file format is not guaranteed to
 * preserve them.  The bad value for all fields is -9999.0.
 *
 * The data file is expected to only contain a single station (i.e. every
 * line contains the same station id).  The program will issue warnings
 * if this is not true and skip the offending stations.
 *
 * The wind file contains lines flagged with AAXX source codes.  It is 
 * unkown what this means, but such lines seem to have times 1 hour before
 * the time of the next series of sounding samples, and they seem to be
 * the only lines with a valid temperature and dew point.  For now,
 * they'll just be skipped.  Samples will be assumed to occur chronologically
 * in the file.  For that matter, just skip soundings which only have
 * one sample in them.
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
	"$Id: trmm_sonde.c,v 1.2 1993-06-07 19:00:23 granger Exp $";
# endif

# include <time.h>
# include <math.h>
# include <errno.h>
# include <stdio.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ingest.h>

# define BADVAL	-9999.0
# define DEG_TO_RAD(x)	((x)*(double)0.017453293)

struct Station {
	char *id;
	float lat;
	float lon;
	float alt;
}
Stations[] = 
{
	/* Uses coordinate of Darwin radar site at Berrimah */
	{ "94120",	-12.4572222,	130.9252778 }
};
#define NUM_STATIONS (sizeof(Stations)/sizeof(Stations[0]))

static FieldId *DeclareFields FP(( int *nfields ));
static DataChunk *InitializeDC FP(( char *platform, 
				   int nfields, FieldId *field_ids));
static void IngestSondeFile FP((FILE *infile, char *platform));
static struct Station *FindStation FP((char *idname));


static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s [ingest options] -p platform filename\n", prog);
	printf ("   where <platform> should include the station number,\n");
	printf ("   and <filename> is the name of a ");
	printf (    "TRMM sonde/wind data file\n");
	IngestUsage ();
}
	

int
main (argc, argv)
int	argc;
char	**argv;
{
	FILE *infile;
	int i;
	char ourname[20];
	char *platform = NULL;
/*
 * Parse general ingest options
 */
	IngestParseOptions (&argc, argv, Usage);
/*
 * Get our program-specific options
 */
	i = 1;
	while (i < argc)
	{
		if (!strcmp(argv[i], "-p") && (i+1 < argc))
		{
			platform = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else
			++i;
	}
	if (argc != 2)
	{
		Usage (argv[0]);
		exit (1);
	}
#ifdef notdef
/*
 * Find our station by the given platform name
 */
	station = FindStation (platform);
	if (!station)
	{
		printf ("%s: couldn't find station '%s'", argv[0], platform);
		exit (3);
	}
#endif
/*
 * Open the input file
 */
	infile = fopen (argv[1], "r");

	if (! infile)
	{
		printf ("Error %d opening  '%s'!\n", errno, argv[1]);
		perror (argv[1]);
		exit (1);
	}
/*
 * Connect to the data store, message, etc., and initialize our data chunk
 */
	sprintf (ourname,"TRMM_Sonde_%s", platform);
	IngestInitialize (ourname);
/*
 * Send the file and platform off to be ingested
 */
	IngestLog (EF_INFO, "Ingesting data for '%s' from file '%s'", 
		   platform, argv[1]);
	IngestSondeFile (infile, platform);
	IngestLog (EF_INFO, "File '%s' completed.", argv[1]);

	return (0);
}




static DataChunk *
InitializeDC (platform, nfields, fields)
char *platform;
int nfields;
FieldId *fields;
/*
 * Create our data chunk and initialize based on the first line of the input
 * file
 */
{
	DataChunk 	*dc;

/*
 * Create the data chunk and put in the platform ID, location, field ID, and
 * bad value flag
 */
	dc = dc_CreateDC (DCC_Scalar);
	if (!dc)
		return (NULL);

	if ((dc->dc_Platform = ds_LookupPlatform (platform)) == BadPlatform)
	{
		IngestLog (EF_EMERGENCY, "Cannot get platform ID for '%s'",
			   platform);
		dc_DestroyDC (dc);
		return (NULL);
	}
/*
 * No location info to set, as it will be dynamic (altitude changes).
 * Set the fields we'll work with, set the bad value, and vamoose.
 */
	dc_SetScalarFields (dc, nfields, fields);
	dc_SetBadval (dc, BADVAL);
	return (dc);
}




static FieldId *
DeclareFields (nfields)
int *nfields;
/*
 * Declare the standard set of fields expected in the data file
 */
{
#	define NUM_FIELDS (sizeof(Fields)/sizeof(Fields[0]))
	int i;
	static struct _Field {
		char name[15];
		char desc[40];
		char units[10];
	} Fields[] = {
		{ "pres", 	"Pressure", 		"mb" },
		{ "height", 	"Height", 		"m" },
		{ "tdry", 	"Temperature",		"degC" },
		{ "dp",		"Dewpoint temperature",	"degC" },
		{ "wdir",	"Wind direction",	"deg" },
		{ "wspd",	"Wind speed",		"m/s" },
/*
 * wind components will be calculated from wspd and wdir whenever possible
 */
		{ "u_wind",	"U wind component",	"m/s" },
		{ "v_wind",	"V wind component",	"m/s" }
	};
	static FieldId ids[ NUM_FIELDS ];
/*
 * If the field is already defined, just get its field id, otherwise
 * define it with the parameters above.
 */
	for (i = 0; i < NUM_FIELDS; ++i)
	{
		if ((ids[i] = F_Declared(Fields[i].name)) == BadField)
			ids[i] = F_DeclareField (Fields[i].name,
						 Fields[i].desc,
						 Fields[i].units);
	}
	*nfields = NUM_FIELDS;
	return (ids);
#	undef NUM_FIELDS
}



static struct Station *
FindStation (name)
char *name;
{
	int i;

	for (i = 0; i < NUM_STATIONS; ++i)
	{
		if (!strcmp(Stations[i].id, name))
			return (Stations+i);
	}
	return NULL;
}




static void
IngestSondeFile (infile, platform)
FILE *infile;
char *platform;
/*
 * Construct a datachunk from the data in <infile>
 */
{
	DataChunk *dc;
	int nsample;
	int nfields;
	FieldId *fids;
	char buf[256];
	char *bufp;
	int ibuf, i;
	char logmsg[256];
	char idname[10], source[10];
	int year, month, day, hour;
	ZebTime zt_now, zt_sounding;	/* time from file and time of snding */
	int seconds;			/* # of seconds into sounding */
	FieldId uw_id, vw_id, height_id;
	int wdir_idx = -1, wspd_idx = -1;
	float wspd, wdir, value;
	Location locn;
	struct Station *station = NULL;

	fids = DeclareFields (&nfields);
	height_id = F_Lookup ("height");
	uw_id = F_Lookup ("u_wind");
	vw_id = F_Lookup ("v_wind");
	for (i = 0; i < nfields; ++i)
	{
		if (fids[i] == F_Declared("wdir"))
			wdir_idx = i;
		else if (fids[i] == F_Declared("wspd"))
			wspd_idx = i;
	}
/*
 * Read a line into memory, parse it for field values, and derive the GMT
 * time.  If the hour is the same as the previous sample, then we're still
 * reading the same observation (i.e. sounding), and the sample time will
 * be stored as a 1-second offset from the previous sample time.
 *
 * At the end of a sounding, store the datachunk as a new file, then create
 * a new, empty data chunk.
 */
	dc = NULL;
	nsample = 0;
	seconds = 0;
	zt_sounding.zt_Sec = zt_sounding.zt_MicroSec = 0;

	while (fgets(buf, sizeof(buf)-1, infile) != NULL)
	{
		bufp = buf;
		if (sscanf (bufp, "%s %4d%2d%2d %d %s %n",
			    idname, &year, &month, &day, &hour, 
			    source, &ibuf) != 6)
		{
			IngestLog (EF_PROBLEM, 
				   "could not read %s from line: '%s'",
				   "station, time, and source", buf);
			continue;
		}
		bufp += ibuf;
	/*
	 * For the first line, setup our station info
	 */
		if (!station)
		{
			station = FindStation (idname);
			if (!station)	/* so much for this idea */
			{
				IngestLog (EF_EMERGENCY,
					   "Station '%s' not recognized!",
					   idname);
				return;
			}
		/*
		 * Set the lat and lon of the station, alt will be taken
		 * from the sounding height field
		 */
			locn.l_lat = station->lat;
			locn.l_lon = station->lon;
			
		}
	/*
	 * Else verify the station number
	 */
		else if ( strcmp(station->id, idname) )
		{
			IngestLog (EF_PROBLEM, "%s '%s', expecting '%s'",
				   "sample found from station", idname,
				   station->id);
			continue;
		}

		TC_ZtAssemble (&zt_now, year - 1900, month, day, hour,
			       0, 0, 0);
		if (TC_Eq(zt_now,zt_sounding))	/* the same sounding */
		{
			++seconds;
			zt_now.zt_Sec += seconds;
		}
		else
		{
		/*
		 * Beginning of new sounding; store our present datachunk
		 * in a new file, as long as there is something worth
		 * storing.
		 */
			if (nsample > 1)
			{
				IngestLog (EF_DEBUG, 
					   "Storing sounding of %d samples",
					   nsample);
				if (!ds_Store (dc, TRUE, (dsDetail *) 0, 0))
				{
					IngestLog (EF_EMERGENCY, 
						   "%s: Failure storing data", 
						   station->id);
				}
			}
			else
				IngestLog (EF_DEBUG,
				   "Skipping '%s' sounding of %d samples",
				   source, nsample);
		/*
		 * Start a whole new datachunk for the next sounding
		 */
			if (dc) 
				dc_DestroyDC (dc);
			dc = InitializeDC (platform, nfields, fids);
			if (!dc)
				break;
			zt_sounding = zt_now;
			nsample = 0;
			seconds = 0;
		}
	/*
	 * Add the current line of data to the given data chunk at zt_now.
	 * Read each field value from the line and add to the data chunk.
	 * Note that the order of the field id's is assumed to be the
	 * order of the fields in the line from the file.  Fields which
	 * are derived (u_wind, v_wind) come last.
	 */
		wspd = wdir = BADVAL;
		locn.l_alt = BADVAL;
		logmsg[0] = '\0';
		for (i = 0; i < nfields; ++i)
		{
			if ((fids[i] == uw_id) || (fids[i] == vw_id))
			{
				continue;
			}
			if ( sscanf (bufp, " %f %n", &value, &ibuf) != 1)
			{
				IngestLog (EF_PROBLEM,
				   "could not read field %i (%s) from line %s",
				   fids[i], F_GetName(fids[i]), buf);
				value = BADVAL;
			}
			bufp += ibuf;
			if (i == wdir_idx)
				wdir = value;
			else if (i == wspd_idx)
				wspd = value;
			if (fids[i] == height_id)
				locn.l_alt = value;
			dc_AddScalar (dc, &zt_now, nsample, fids[i], &value);
			sprintf (logmsg+strlen(logmsg), "%8.1f", value);
		}
	/*
	 * Derive v_wind and u_wind and add to dc
	 */
		if (wspd != BADVAL && wdir != BADVAL)
		{
			/* u_wind */
			value = wspd * cos (DEG_TO_RAD (90 - wdir));
			dc_AddScalar (dc, &zt_now, nsample, uw_id, &value);
			sprintf (logmsg+strlen(logmsg), "%8.1f", value);
			/* v_wind */
			value = wspd * sin (DEG_TO_RAD (90 - wdir));
			dc_AddScalar (dc, &zt_now, nsample, vw_id, &value);
			sprintf (logmsg+strlen(logmsg), "%8.1f", value);
		}
		else
		{
			value = BADVAL;
			dc_AddScalar (dc, &zt_now, nsample, vw_id, &value);
			dc_AddScalar (dc, &zt_now, nsample, uw_id, &value);
		}
	/*
	 * Add the source as a sample attribute
	 */
		dc_SetSampleAttr (dc, nsample, "source code", source);
	/*
	 * Last but not least, set the location for this sample.  The altitude
	 * was set inside the fields loop according to the 'height' field
	 */
		dc_SetLoc (dc, nsample, &locn);
	/*
	 * Finally done with that sample.  Keep plugging.
	 */
		IngestLog (EF_DEVELOP, "%3i, %s", nsample, logmsg);
		++nsample;
	}
/*
 * Finished, either because its the end of the file or we couldn't recover
 * from some error.
 */
	if (dc)
		dc_DestroyDC (dc);
}
