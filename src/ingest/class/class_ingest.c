/*
 * $Id: class_ingest.c,v 2.21 2001-06-13 21:56:55 granger Exp $
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

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <ui.h>		/* need struct ui_command and ERRORCATCH */
#include <ui_error.h>
#include "ingest.h"

#ifndef lint
MAKE_RCSID(
   "$Id: class_ingest.c,v 2.21 2001-06-13 21:56:55 granger Exp $")
#endif

static void	Usage FP((char *prog_name));
static ZebTime *GetTimes FP((int *npts));
static void	SetLocations FP((DataChunk *dc, int nsamples));
static void	GetPlatformName FP((char *classfile, char *platname));
static void	ParseCommandLineOptions FP((int *argc, char *argv[]));
static void   	ParseFieldNames FP((int argc, char *argv[],
				    FieldId *fields, int *nfields));
static void 	LoadFieldData FP((DataChunk *dc, ZebTime *times,
			int nsamples, FieldId *fields, int nfields,
			bool reverse));
ZebTime *	GetTimes FP((int *npts));
static void	BuildTranslationTable FP ((void));
static char *	GetNextString FP ((char *, char*));
static void	FixBadTimes FP ((float *, int));

# define INGEST_NAME "class_ingest"
# define SND	"snd"
# define BUFLEN	10000
# define BADVAL -9999.0
# define MAX_FIELDS 32

#define ZEB_PROBLEM 1
#define FILE_PROBLEM 8
#define PROBLEM 99
#define NO_PROBLEM 0

/*
 * Structure to build a linked list of site name -> platform name translations
 */
typedef struct _SiteTrans {
	char	*site;
	int	slen;
	char	*plat;
	struct _SiteTrans	*next;
} SiteTranslation;

static SiteTranslation	*SiteTransList = (SiteTranslation *) 0;

/*
 * The name of the site name -> platform name translations file (if any)
 */
static char	*Tfilename = NULL;

/*
 * Pressure quality threshold (1.5 by default, but can be changed by
 * command line option)
 */
static float	QualThresh = 1.5;

/*
 * The list of fields available from CLASS files.  These are fixed in the
 * CLASS format.
 */
struct _ClassField
{
    char *name;
    char *type;		/* Zebra generic field type */
    char *units;
    char *description;
} ClassFields[] = 
{
    { "tdelta", "", "s", "time since launch" },
    { "pres", "P", "hPa", "pressure" },
    { "temp", "T", "degC", "temperature" },
    { "dp", "dp", "degC", "dewpoint" },
    { "rh", "rh", "%", "relative humidity" },
    { "u_wind", "uwind", "m/s", "u wind component" },
    { "v_wind", "vwind", "m/s", "v wind component" },
    { "wspd", "wspd", "m/s", "wind speed" },
    { "wdir", "wdir", "deg", "wind direction" },
    { "ascent", "", "m/s", "ascent rate" },
    { "lon", "", "deg", "longitude" },
    { "lat", "", "deg", "latitude" },
    { "range", "", "km", "range from launch site" },
    { "az", "", "deg", "azimuth from launch site" },
    { "alt", "" },
    { "qpres", "", "hPa", "pressure quality" },
    { "qtemp", "", "degC", "temperature quality" },
    { "qrh", "", "%", "humidity quality" },
    { "qu", "", "m/s", "u wind quality" },
    { "qv", "", "m/s", "v wind quality" },
    { "qwind", "", "m/s", "wind quality" }
};

int NClassFields = sizeof (ClassFields) / sizeof (struct _ClassField);


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

char *PlatformName = 0;

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
	ZebTime temp;
	int nsamples;		/* Number of samples, or pts, in the file */
	DataChunk *Dchunk;   	/* The DataChunk will be building */
	bool reverse;		/* Reverse samples? */
	int i;
	static struct ui_command end_cmd = { UTT_END };
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
		exit(PROBLEM);
	}
	filename = argv[1];
	if (JustShowFields)
	{
		GetPlatformName(filename, plat);
		snd_show(&end_cmd);
		exit(NO_PROBLEM);
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
 * Build the table for special site name -> platform name translations
 */ 
	BuildTranslationTable ();
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
	IngestLog (EF_INFO, "Ingesting '%s'", filename);

	GetPlatformName(filename, plat);
	if (IngestLogFlags & (EF_DEBUG | EF_INFO)) snd_show(&end_cmd);
/*
 * If running standalone, provide a default platform definition for this name.
 */
	if (ds_IsStandalone() && (ds_LookupPlatform(plat) == BadPlatform))
	{
		PlatClassId cid = ds_LookupClass ("CLASS");

		if (cid == BadClass)
		{
			PlatClassRef pc = ds_NewClass ("CLASS");
			IngestLog(EF_DEBUG, "default platform class 'CLASS'");
			/* relies on DefDataDir from ingest module */
			ds_AssignClass (pc, OrgScalar, FTNetCDF,
					TRUE/*mobile*/);
			ds_SetMaxSample (pc, 10000);
			ds_SetComment (pc, "standalone CLASS platform ");
			cid = ds_DefineClass (pc);
		}
		/*
		 * Create a new instance for this platform name.
		 */
		IngestLog (EF_DEBUG, "instantiating platform '%s'", plat);
		ds_DefinePlatform (cid, plat);
	}
	if ((Dchunk->dc_Platform = ds_LookupPlatform (plat)) == BadPlatform)
		ui_error ("Unknown platform: %s", plat);
	if (DumpDataChunk)
	{
		IngestLog (EF_DEBUG, "Dumping data chunks to stdout");
		dc_DumpDC (Dchunk);
	}

/*
 * Get the times and locations
 */
	times = GetTimes (&nsamples);
	reverse = TC_Less (times[nsamples-1], times[0]);
	if (reverse)
	{
		for (i = 0; i < nsamples / 2; i++)
		{
			temp = times[i];
			times[i] = times[nsamples - i - 1];
			times[nsamples - i - 1] = temp;
		}
	}

	TC_EncodeTime(times, TC_Full, ctime);
	IngestLog(EF_INFO, "%s: %d samples found, starting at %s", 
		      plat, nsamples, ctime);

/*
 * Load the data for each field into the data chunk 
 */
	LoadFieldData(Dchunk, times, nsamples, fields, nfields, reverse);
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
	if (!ds_StoreBlocks (Dchunk, /*newfile*/ TRUE, (dsDetail *)0, 0))
	{
		IngestLog(EF_EMERGENCY,"%s: Data store failed",plat);
	}
	else
		IngestLog(EF_INFO,
		   "%s: CLASS data loaded into DataStore",plat);

ON_ERROR
	IngestLog(EF_EMERGENCY,"Error occurred.  Aborting...");
	exit (FILE_PROBLEM);
ENDCATCH
	IngestLog(EF_DEBUG, "Finished...");
	exit (NO_PROBLEM);
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
 * Bad-value times do really nasty things to the result in the data
 * store.  You don't wanna know.  Fix them up here.
 */
	FixBadTimes (Buf, Npts);
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




static void
FixBadTimes (times, ntime)
float *times;
int ntime;
/*
 * Interpolate in for bad-value times.
 */
{
	int t, fwd, fix, nfix = 0;
/*
 * First time has gotta be good.
 */
	if (times[0] == BADVAL)
		times[0] = 0;	/* XXXXX */
/*
 * Plow through looking for bad ones.
 */
	for (t = 1; t < ntime; t++)
	{
		if (times[t] == BADVAL)
		{
			for (fwd = t + 1; fwd < ntime; fwd++)
				if (times[fwd] != BADVAL)
					break;
			if (fwd >= ntime)
			{
				for (fwd = t; fwd < ntime; fwd++) /* XXX */
					times[fwd] = times[fwd - 1] + 1;
				nfix += ntime - t;
				break;
			}
			else
			{
				for (fix = t; fix < fwd; fix++)
					times[fix] = times[t-1] +
						(times[fwd]-times[t-1])*
						(fix - t + 1)/(fwd - t + 1);
				nfix += fwd - t;
			}
		}
	}
	if (nfix)
		msg_ELog (EF_INFO, "%d times thrashed", nfix);
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
	SiteTranslation	*strans;
/*
 * Load the sounding file (0 indicates we're loading a CLASS format file)
 */
	snd_load_file (classfile, 0, SND);
/*
 * A platform name from the command line takes precedence.
 */
	if (PlatformName)
	{
	    strcpy (plat, PlatformName);
	    return;
	}
/*
 * Get the site name and make it lower case
 */
	site = snd_site (SND);

	for (i = 0; i < strlen (site); i++)
		site[i] = tolower (site[i]);
/*
 * Convert the site name to a platform name.
 * First check against the translation list, then look for the CLASS
 * standard "FIXED, xxx" or "MOBILE, xxx".  Otherwise, just use the
 * site name as the platform name.
 */
	strans = SiteTransList;
	while (strans)
	{
		if (! strncmp (site, strans->site, strans->slen))
		{
			strcpy (plat, strans->plat);
			break;
		}

		strans = strans->next;
	}

	if (! strans)
	{
		if ((sscanf (site, "fixed, %s", plat) == 1) ||
			(sscanf (site, "fixed %s", plat) == 1) ||
			(sscanf (site, "mobile, %s", plat) == 1) ||
			(sscanf (site, "mobile %s", plat) == 1))
			/* do nothing */;
		else
			strcpy (plat, site);
	}
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
	int i;

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
		   IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (streq(argv[i],"-fields"))
		{
		   JustShowFields = (char)1;
		   IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (! strncmp (argv[i], "-t", 2))
		{
		   Tfilename = strdup (argv[i+1]);
		   IngestRemoveOptions (argc, argv, i, 2);
		}
		else if (! strncmp (argv[i], "-q", 2))
		{
		   QualThresh = atof (argv[i+1]);
		   IngestRemoveOptions (argc, argv, i, 2);
		}
		else if (! strncmp (argv[i], "-p", 2))
		{
		   PlatformName = argv[i+1];
		   IngestRemoveOptions (argc, argv, i, 2);
		}
		else
		   ++i;
	}
}


static void
Usage(prog)
	char *prog;
{
	printf ("Usage: %s [options] <file> <fields>\n",prog);
	printf ("       %s -fields <file>\n",prog);
	printf ("       %s -help\n",prog);
	printf ("\nOptions:\n");
	printf ("   -show, -s		Dump data chunk as it's built\n");
	printf ("   -fields		Describe the sounding file\n");
	printf ("   -trans <tfile>	Use the site/platform translations in 'tfile'\n");
	printf ("   -q <qval>		Set pressure quality threshold\n");
	printf ("   -platform <name>    Set the platform name explicitly\n");
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
	    int c;
	/*
	 * Make sure this field is among those available from a CLASS file,
	 * then get an id and stash it in our field list.
	 */
	    for (c = 0; c < NClassFields; c++)
		if (! strcmp (argv[f], ClassFields[c].name))
		    break;

	    if (c == NClassFields)
	    {
		IngestLog(EF_PROBLEM,
			  "%s: %s not a recognized field.", argv[1], argv[f]);
	    }
	    else
	    {
		struct _ClassField *cf = ClassFields + c;
		
		fields[*nfields] = F_Field (cf->name, cf->type, 
					    cf->description, cf->units);

		IngestLog(EF_DEBUG,"%s: Field %s has id %d", argv[1],
			  argv[f], fields[*nfields]);
		++(*nfields);
	    }
	}
}



/* LoadFieldData --------------------------------------------------------
 *    Load data from the sounding file for each FieldId in the fields array
 *    into the DataChunk dc.
 *    Each field has nsamples, and times holds the time of each sample
 *    The order of samples should be reversed if 'reverse' is true
 */
static void
LoadFieldData(dc, times, nsamples, fields, nfields, reverse)
	DataChunk *dc;
	ZebTime *times;
	int nsamples;
	FieldId *fields;
	int nfields;
	bool reverse;
{
	int nbad = 0;
	int i, f;
	float temp;
/*
 * Get pressure and pressure quality fields to build a data removal
 * list
 */
	snd_get_data (SND, Pres, BUFLEN, fd_num ("pres"), BADVAL);
	snd_get_data (SND, QPres, BUFLEN, fd_num ("qpres"), BADVAL);

	for (i = 0; i < nsamples; i++)
		if (Pres[i] == BADVAL 	|| 
		    QPres[i] == BADVAL 	|| 
		    Pres[i] == 0.0 	|| 
				(QPres[i] > QualThresh && QPres[i] != 77 
						&& QPres[i] != 88
						&& QPres[i] != 99))
			BadPts[nbad++] = i;

	/* Report the number of bad points found */
	IngestLog(EF_INFO, "number of bad points found: %d", nbad);

	/* If there are no good points in this file, say so */
	if (nbad == nsamples)
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
		for (i = 0; i < nbad; i++)
			Buf[BadPts[i]] = BADVAL;
	/*
	 * Reverse samples if necessary
	 */
		if (reverse)
		{
			for (i = 0; i < nsamples / 2; i++)
			{
				temp = Buf[i];
				Buf[i] = Buf[nsamples - i - 1];
				Buf[nsamples - i - 1] = temp;
			}
		}

	/*
	 * Store the data in the DataChunk dc (it will be copied from Buf)
	 */
		dc_AddMultScalar(dc, times, 0, nsamples, fields[f], Buf);

	} /* on to the next field... */
}




static void
BuildTranslationTable ()
/*
 * Read 'tfile' and build a table of site name -> platform name translations
 */
{
	int	i;
	char	line[80], *lp, site[40], plat[40];
	FILE	*tfile;
	SiteTranslation	*newtrans;
/*
 * Open the file (or just return if there is no translations file)
 */
	if (! Tfilename)
		return;

	tfile = fopen (Tfilename, "r");

	if (! tfile)
	{
		IngestLog(EF_PROBLEM, 
			"Error %d opening translations file '%s'", errno, 
			Tfilename);
		ui_error ("Cannot open translations file");
	}
/*
 * Loop to read all the lines from the file
 */
	while (fgets (line, sizeof (line), tfile))
	{
		line[strlen (line) - 1] = '\0';
	/*
	 * Get the site and platform strings from this line
	 */
		lp = line;
		if ((lp = GetNextString (site, lp)) == 0 ||
			(lp = GetNextString (plat, lp)) == 0)
		{
			IngestLog (EF_PROBLEM, "Bad translation line '%s'", 
				line);
			ui_error ("Bad site -> platform translation");
		}
	/*
	 * Make things lower case
	 */
		for (i = 0; i < strlen (site); i++)
			site[i] = tolower (site[i]);

		for (i = 0; i < strlen (plat); i++)
			plat[i] = tolower (plat[i]);
	/*
	 * Allocate and build a new entry, making it the head of the
	 * linked list
	 */
		newtrans = (SiteTranslation *) 
			malloc (sizeof (SiteTranslation));

		newtrans->site = strdup (site);
		newtrans->slen = strlen (site);
		newtrans->plat = strdup (plat);
		newtrans->next = SiteTransList;
		SiteTransList = newtrans;

		IngestLog(EF_DEBUG, "Site '%s' -> Platform '%s'", site, plat);
	}
}




static char*
GetNextString (ret, text)
char	*ret, *text;
/*
 * Extract the next string from 'text', either quoted or terminated by
 * white space.  The string is written into 'ret', and the return value
 * of the function is a pointer to the first character after the string
 * (success) or null (failure).
 */
{
	char	*endquote;
/*
 * Remove leading white space
 */
	while (*text == ' ' || *text == '\t')
		text++;
/*
 * Quoted string?
 */
	if (*text == '\"' || *text == '\'')
	{
		if ((endquote = strchr (text + 1, *text)) == 0)
			return ((char *) 0);

		strncpy (ret, text + 1, endquote - text - 1);
		ret[endquote - text - 1] = '\0';

		return (endquote + 1);
	}
/*
 * No quotes, so delineate by whitespace
 */
	if (sscanf (text, "%s", ret) == 1)
		return (text + strlen (ret));
	else
		return ((char *) 0);
}


/*
 * Suds lib seems to need this these days.
 */
int main_dump_cmd () { return (0); }
