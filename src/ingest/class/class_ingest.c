/*
 * $Id: class_ingest.c,v 2.6 1992-07-31 16:11:06 pai Exp $
 *
 * Ingest CLASS data into the system.
 *
 * Type 'class_ingest -help' for usage info
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
#include <copyright.h>

#ifndef lint
MAKE_RCSID(
   "$Id: class_ingest.c,v 2.6 1992-07-31 16:11:06 pai Exp $")
#endif

static void	Usage FP((char *prog_name));
static ZebTime *GetTimes FP((int *npts));
static void	SetLocations FP((DataChunk *dc, int nsamples));
static void	GetPlatformName FP((char *classfile, char *platname));
static void	ParseCommandLineOptions FP((int *argc, char *argv[]));
static void   	ParseFieldNames FP((int argc, char *argv[],
				    FieldId *fields, int *nfields));
static void 	LoadFieldData FP((DataChunk *dc, ZebTime *times,
			  int nsamples, FieldId *fields, int nfields));
ZebTime *	GetTimes FP((int *npts));


# define INGEST_NAME "class_ingest"
# define SND	"snd"
# define BUFLEN	1024
# define BADVAL -9999.0
# define MAX_FIELDS 32

/*
 * Define all large data buffers globally:
 */
float	Pres[BUFLEN]; 		/* Holds pressure fields */
float	QPres[BUFLEN];		/* Holds pressure quality fields */
float	Buf[BUFLEN];		/* Used to read in the samples of 
				 * of data from the data file */
int	BadPts[BUFLEN];		/* Holds an index to each of the bad
				 * data points out of a file's list
				 * of data points.  Each field for each
				 * point included
				 * in this array is assigned BADVAL 
				 * when read */

/*
 * Global debugging flags set from command line
 */
char JustShowFields = (char)0;	/* Initially false */
char DumpDataChunk = (char)0;   /* Dump chunks AS BUILT rather than
				 * like ingest.c option which is
				 * WHEN STORED */


int main (argc, argv)
	int argc;
	char **argv;
{
	char 	*filename;	/* Name of the snding file, pts to argv[1] */
	char	plat[30];	/* The name of the CLASS platform */
	FieldId fields[MAX_FIELDS];
				/* The FieldId's of each field which the
				 * user has specified on the cmd-line */
	int nfields;		/* The number of fields to be stored */
	ZebTime *times;		/* Times for ea. sample in the s'nding file */
	int nsamples;		/* Number of samples, or pts, in the file */
	DataChunk *Dchunk;   	/* The DataChunk we will be building */
	struct ui_command end_cmd = { UTT_END };
	static char ctime[40];

/*
 * Get our command-line options, setting appropriate global variables
 * Only the file name and the names of the fields should remain
 */
	ParseCommandLineOptions(&argc, argv);
	if (argc < 2)		/* Need a file name arg */
	{
		printf("%s: need a file name\n",argv[0]);
		Usage(argv[0]);
		exit(1);
	}
	filename = argv[1];
	if (JustShowFields)
	{
		GetPlatformName(filename, plat);
		snd_show(&end_cmd);
		exit(0);
	}

/*
 * Hook into the world.
 */
ERRORCATCH
/*
 * Initialize usy, message, DataStore, and fields all at once
 */
	IngestInitialize(INGEST_NAME);

/*
 * Create a new data chunk, get the field list from the command line
 * and set up these fields in the Dchunk, including our bad_value_flag
 */
	Dchunk = dc_CreateDC(DCC_Scalar);

	ParseFieldNames(argc, argv, fields, &nfields);
	if (nfields == 0)
		ui_error("No valid fields specified.\n");
	dc_SetScalarFields(Dchunk, nfields, fields);
	IngestLog(EF_DEBUG,"%d scalar fields set in datachunk",nfields);
	dc_SetBadval(Dchunk, BADVAL);
	IngestLog(EF_DEBUG,"bad_value_flag set to %6.2f",BADVAL);

/*
 * Open sounding file, get platform name and check for validity
 */
	GetPlatformName(filename, plat);
	if (IngestLogFlags & (EF_DEBUG | EF_INFO)) snd_show(&end_cmd);
	IngestLog(EF_INFO, "Platform: %s, File: %s", plat, filename);
	if ((Dchunk->dc_Platform = ds_LookupPlatform (plat)) == BadPlatform)
		ui_error ("Unknown platform: %s", plat);
	if (DumpDataChunk)
		IngestLog(EF_DEBUG,
		    "Dumping data chunks to stdout");
	if (DumpDataChunk) dc_DumpDC(Dchunk);

/*
 * Get the times and locations
 */
	times = GetTimes (&nsamples);
	TC_EncodeTime(times, TC_Full, ctime);
	IngestLog(EF_INFO, "%s: %d samples found, starting at %s", 
		      plat, nsamples, ctime);

/*
 * Load the data for each field into the data chunk 
 */
	LoadFieldData(Dchunk, times, nsamples, fields, nfields);
	IngestLog(EF_DEBUG,"%s: each field has been loaded", plat);
	if (DumpDataChunk) dc_DumpDC(Dchunk);

/*
 * Set the locations for each sample in the data chunk
 */
	SetLocations(Dchunk, nsamples);
	IngestLog(EF_DEBUG, "%s: Locations set for each sample",plat);

/*
 * Send everything to the data store
 */
	IngestLog(EF_DEBUG,"%s: Sending data to DataStore",plat);
	if (!ds_Store( Dchunk, /*newfile*/ TRUE, (dsDetail *)0, 0))
	{
		IngestLog(EF_EMERGENCY,"%s: Data store failed",plat);
	}
	else
		IngestLog(EF_INFO,
		   "%s: CLASS data loaded into DataStore",plat);

ON_ERROR
	IngestLog(EF_EMERGENCY,"Error occurred.  Aborting...");
	exit (1);
ENDCATCH
	IngestLog(EF_DEBUG, "Exiting...");
	exit (0);
}



/* GetTimes ------------------------------------------------------------
 *   Allocate and fill in an array of ZebTime's for each point in the 
 *   sounding file, returning the array and the number of points
 */
static ZebTime *
GetTimes (npts)
	int *npts;
{
	date	start, t, snd_time ();
	int	i, hours, minutes, seconds, delta, snd_get_data ();
	int 	Npts;
	ZebTime *times;

/*
 * Get the start time and the time data for the sounding
 */
	start = snd_time (SND);

	Npts = snd_get_data (SND, Buf, BUFLEN, fd_num("time"), BADVAL);

/*
 * Allocate the times array
 */
	times = (ZebTime *) malloc (Npts * sizeof(ZebTime));

/*
 * Convert the sounding times, which are in seconds from sounding launch,
 * into absolute times and then convert them to ZebTime
 */
	for (i = 0; i < Npts; i++)
	{
		t = start;

		if (Buf[i] >= 0)
		{
			hours = (int)(Buf[i] / 3600);
			minutes = (int)((Buf[i] - 3600 * hours) / 60);
			seconds = (int)(Buf[i] - 3600 * hours - 60 * minutes);
			delta = 10000 * hours + 100 * minutes + seconds;
			pmu_dadd (&t.ds_yymmdd, &t.ds_hhmmss, delta);
		}

		TC_UIToZt( &t, times+i );
	}

	*npts = Npts;
	return (times);
}


/* SetLocations ---------------------------------------------------------
 *   Read the location of each data sample in the sounding file and set
 *   the location in the data chunk
 */
static void
SetLocations (dc, Npts)
	DataChunk *dc;
	int Npts;
{
	float	snd_s_lat (), snd_s_lon (), snd_s_alt ();
	int	snd_get_data ();
	int 	i;
	Location *locns;

	locns = (Location *) malloc (Npts * sizeof(Location));

#ifdef notdef  /* WHAT DO I DO WITH THE SITE LOCATION??? */
/*
 * Put in the site location
 */
	Dobj.do_loc.l_lat = snd_s_lat (SND);
	Dobj.do_loc.l_lon = snd_s_lon (SND);
	Dobj.do_loc.l_alt = 0.001 * snd_s_alt (SND);
#endif

/*
 * Get the latitude data
 */
	snd_get_data (SND, Buf, BUFLEN, fd_num ("latitude"), BADVAL);

	for (i = 0; i < Npts; i++)
		locns[i].l_lat = Buf[i];
/*
 * Get the longitude data
 */
	snd_get_data (SND, Buf, BUFLEN, fd_num ("longitude"), BADVAL);

	for (i = 0; i < Npts; i++)
		locns[i].l_lon = Buf[i];
/*
 * Get the altitude data, converting from m to km
 */
	snd_get_data (SND, Buf, BUFLEN, fd_num ("altitude"), BADVAL);

	for (i = 0; i < Npts; i++)
		locns[i].l_alt = 0.001 * Buf[i];

/*
 * Now store the locns in the data chunk for each sample
 */
	for (i = 0; i < Npts; i++)
		dc_SetLoc(dc, i, locns+i);
	
	free (locns);
}



/* GetPlatformName ----------------------------------------------------
 *   Opens 'classfile' and extracts an all-lowercase platform name
 */
static void
GetPlatformName (classfile, plat)
	char *classfile;
	char plat[];
{
	char *snd_site FP((char *));
	char *site;
	int i;

/*
 * Load the sounding file (0 indicates we're loading a CLASS format file)
 */
	snd_load_file (classfile, 0, SND);
/*
 * Get the site name and figure out the platform
 */
	site = snd_site (SND);

	if (sscanf (site, "FIXED, %s", plat) == 1)
		/* do nothing */;
	else if (sscanf (site, "FIXED %s", plat) == 1)
		/* do nothing */;
	else if (sscanf (site, "MOBILE, %s", plat) == 1)
		/* do nothing */;
	else if (sscanf (site, "MOBILE %s", plat) == 1)
		/* do nothing */;

        /* This is where it gets REALLY ugly */

        /* NWSI */
        else if (strncmp (site, "Albuquerque, NM", 15) == 0)
                strcpy (plat, "abq");
        else if (strncmp (site, "Amarillo, TX", 12) == 0)
                strcpy (plat, "ama");
        else if (strncmp (site, "Dodge City, KS", 14) == 0)
                strcpy (plat, "ddc");
        else if (strncmp (site, "Denver, CO", 10) == 0)
                strcpy (plat, "den");
        else if (strncmp (site, "El Paso, TX", 11) == 0)
                strcpy (plat, "elp");
        else if (strncmp (site, "Longview, TX", 12) == 0)
                strcpy (plat, "ggg");
        else if (strncmp (site, "Grand Junction, CO", 18) == 0)
                strcpy (plat, "gjt");
        else if (strncmp (site, "Green Bay, WI", 13) == 0)
                strcpy (plat, "grb");
        else if (strncmp (site, "Huron, SD", 9) == 0)
                strcpy (plat, "hon");
        else if (strncmp (site, "North Platte, NE", 16) == 0)
                strcpy (plat, "lbf");
        else if (strncmp (site, "North Little Rock, AR", 21) == 0)
                strcpy (plat, "lit");
        else if (strncmp (site, "Lander, WY", 10) == 0)
                strcpy (plat, "lno");
        else if (strncmp (site, "Midland, TX", 11) == 0)
                strcpy (plat, "maf");
        else if (strncmp (site, "Omaha, NE", 9) == 0)
                strcpy (plat, "oma");
        else if (strncmp (site, "Norman, OK", 10) == 0)
                strcpy (plat, "oun");
        else if (strncmp (site, "Paducah, KY", 11) == 0)
                strcpy (plat, "pah");
        else if (strncmp (site, "Peoria, IL", 10) == 0)
                strcpy (plat, "pia");
        else if (strncmp (site, "Rapid City, SD", 14) == 0)
                strcpy (plat, "rap");
        else if (strncmp (site, "Stephenville, TX", 16) == 0)
                strcpy (plat, "sep");
        else if (strncmp (site, "St Cloud, MN", 12) == 0)
                strcpy (plat, "stc");
        else if (strncmp (site, "Topeka, KS", 10) == 0)
                strcpy (plat, "top");
        else if (strncmp (site, "Monett, MO", 10) == 0)
                strcpy (plat, "umn");

        /* NWSO */
        else if (strncmp (site, "Bismarck, ND", 12) == 0)
                strcpy (plat, "bis");
        else if (strncmp (site, "Desert Rock, NV", 15) == 0)
                strcpy (plat, "dra");
        else if (strncmp (site, "Boise, ID", 9) == 0)
                strcpy (plat, "boi");
        else if (strncmp (site, "Ely, NV", 7) == 0)
                strcpy (plat, "ely");
        else if (strncmp (site, "Spokane, WA", 11) == 0)
                strcpy (plat, "geg");
        else if (strncmp (site, "Glasgow, MT", 11) == 0)
                strcpy (plat, "ggw");
        else if (strncmp (site, "Great Falls, MT", 15) == 0)
                strcpy (plat, "gtf");
        else if (strncmp (site, "Winslow, AZ", 11) == 0)
                strcpy (plat, "inw");
        else if (strncmp (site, "Salt Lake City, UT", 18) == 0)
                strcpy (plat, "slc");
        else if (strncmp (site, "Tucson, AZ", 10) == 0)
                strcpy (plat, "tus");
        else if (strncmp (site, "Winnemucca, NV", 14) == 0)
                strcpy (plat, "wmc");

        /* PICK */
        else if (strncmp (site, "Kodiak, AK", 10) == 0)
                strcpy (plat, "adq");
        else if (strncmp (site, "Medford, OR", 11) == 0)
                strcpy (plat, "mfr");
        else if (strncmp (site, "San Diego, CA", 13) == 0)
                strcpy (plat, "myf");
        else if (strncmp (site, "Oakland, CA", 11) == 0)
                strcpy (plat, "oak");
        else if (strncmp (site, "Salem, OR", 9) == 0)
                strcpy (plat, "sle");
        else if (strncmp (site, "Quillayute, WA", 14) == 0)
                strcpy (plat, "uil");
        else if (strncmp (site, "Annette, AK", 11) == 0)
                strcpy (plat, "ann");
        else if (strncmp (site, "Yakutat, AK", 11) == 0)
                strcpy (plat, "yak");

        /* It's over!  If we haven't got it yet, just give up! */
	else
		strcpy (plat, site);
/*
 * Make the platform name lower case
 */
	for (i = 0; i < strlen (site); i++)
		plat[i] = tolower (plat[i]);
}


/* ParseCommandLineOptions --------------------------------------------
 *    Set global variables from command-line options, leaving only
 *    the expected file and field names in the arg list
 */
static void
ParseCommandLineOptions(argc, argv)
	int *argc;
	char *argv[];
{
	int i, j;

/*
 * First parse any of the general ingest options
 */
	IngestParseOptions(argc, argv, Usage);

/*
 * Now check for any of our own debug flags on the command line
 */
	i = 1;
	while (i < *argc)
	{
		if (streq(argv[i],"-show") ||
			 streq(argv[i],"-s"))
		{
		   DumpDataChunk = (char)1;
		}
		else if (streq(argv[i],"-fields"))
		{
		   JustShowFields = (char)1;
		}
		else
		{
		   ++i;
		   continue;
		}

		RemoveOptions(argc, argv, i, 1);
	}
}


static void
Usage(prog)
	char *prog;
{
	printf ("Usage: %s [options] file fields\n",prog);
	printf ("       %s -fields file\n",prog);
	printf ("       %s -help\n",prog);
	printf ("\nOptions:\n");
	printf ("   -show, -s		Dump data chunk as it's built\n");
	printf ("   -fields		Describe the sounding file\n");
	printf ("\n");
	IngestUsage();
	printf ("\nExamples:\n");
	printf ("   %s -show -log pd i7282220.dpk pres temp rh\n", prog);
	printf ("   %s -fields i7282220.dpk\n\n", prog);
}


/* ParseFieldNames ----------------------------------------------------
 *    Fill in a FieldId array from field names starting with argv[2]
 */
static void
ParseFieldNames(argc, argv, fields, nfields)
	int argc;
	char *argv[];
	FieldId fields[];
	int *nfields;
{
	int f;

	*nfields = 0;

/*
 * The field names start with argv[2] ...
 */
	for (f = 2; f < argc; f++)
	{
	/*
	 * Attempt to find an ID for this field name 
	 */
		if ((fields[*nfields] = F_Lookup(argv[f])) == BadField)
		{
		   IngestLog(EF_PROBLEM,
		      "%s: %s not a recognized field.",argv[1],argv[f]);
		}
		else
		{
		   IngestLog(EF_DEBUG,"%s: Field %s has id %d",argv[1],
				     argv[f], fields[*nfields]);
		   ++(*nfields);
		}
	}
}



/* LoadFieldData --------------------------------------------------------
 *    Load data from the sounding file for each FieldId in the fields array
 *    into the DataChunk dc.
 *    Each field has nsamples, and times holds the time of each sample
 */
static void
LoadFieldData(dc, times, nsamples, fields, nfields)
	DataChunk *dc;
	ZebTime *times;
	int nsamples;
	FieldId *fields;
	int nfields;
{
	int Npts = nsamples;
	int NBad = 0;
	int i, f;
	float *newdata;

/*
 * Get pressure and pressure quality fields to build a data removal
 * list
 */
	snd_get_data (SND, Pres, BUFLEN, fd_num ("pres"), BADVAL);
	snd_get_data (SND, QPres, BUFLEN, fd_num ("qpres"), BADVAL);

	for (i = 0; i < Npts; i++)
		if (Pres[i] == BADVAL 	|| 
		    QPres[i] == BADVAL 	|| 
		    Pres[i] == 0.0 	|| 
				(QPres[i] > 1.5 && QPres[i] != 77 
						&& QPres[i] != 88
						&& QPres[i] != 99))
			BadPts[NBad++] = i;

	/* Report the number of bad points found */
	IngestLog(EF_INFO, "number of bad points found: %d", NBad);

	/* If there are no good points in this file, say so */
	if (NBad == Npts)
	{
		IngestLog(EF_PROBLEM,"no good samples in file");
		ui_error("No good points to ingest");
	}

/*
 * For each field id in the fields array, read data into the buffer,
 * correct for bad value points, and store in dc
 */
	for (f=0; f < nfields; ++f)
	{
	/*
	 * Read the nsamples of data into Buf for the current field id
	 */
		snd_get_data (SND, Buf, BUFLEN, 
			      fd_num(F_GetName(fields[f])), BADVAL);
	/*
	 * Remove the bad points
	 */
		for (i = 0; i < NBad; i++)
			Buf[BadPts[i]] = BADVAL;

	/*
	 * Store the data in the DataChunk dc (it will be copied from Buf)
	 */
		dc_AddMultScalar(dc, times, 0, nsamples, fields[f], Buf);

	} /* on to the next field... */
}


