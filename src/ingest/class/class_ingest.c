/*
 * Ingest CLASS data into the system.
 *
 * See 
 *	class_ingest -help
 * -or- 
 *	usage() 
 * for usage info
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
MAKE_RCSID("$Id: class_ingest.c,v 2.3 1992-06-05 23:02:57 granger Exp $")
#endif

static void	usage FP((char *prog_name));
static ZebTime *GetTimes FP((int *npts));
static void	SetLocations FP((DataChunk *dc, int nsamples));
static void	GetPlatformName FP((char *classfile, char *platname));
static void	ParseCommandLineOptions FP((int *argc, char *argv[]));
static void   	ParseFieldNames FP((int argc, char *argv[],
				    FieldId *fields, int *nfields));
static void 	LoadFieldData FP((DataChunk *dc, ZebTime *times,
				  int nsamples, FieldId *fields, int nfields));
ZebTime *	GetTimes FP((int *npts));
static void	ListAvailableFields ();


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
char DumpDataChunk = (char)0;	/* Initially false */
char JustShowFields = (char)0;	/* Initially false */


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
	ZebTime *times;		/* Times for each sample in the s'nding file */
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
		usage(argv[0]);
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
		IngestLog(EF_INFO,"%s: CLASS data loaded into DataStore",plat);

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
	else if (sscanf (site, "FIXED  %s", plat) == 1)
		/* do nothing */;
	else if (strncmp (site, "MOBILE", 6) == 0)
		strcpy (plat, "mobile");
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
	IngestParseOptions(argc, argv, usage);

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

		/* Remove any options that are found */
		--(*argc);
		for (j = i; j < *argc; ++j) 
		   argv[j] = argv[j+1];
	}
}


static void
usage(prog)
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
	printf ("\nExamples:\n%s -show -log pd i7282220.dpk pres temp rh\n",
		prog);
	printf ("%s -fields i7282220.dpk\n", prog);
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
		if (Pres[i] == BADVAL || QPres[i] == BADVAL || 
			Pres[i] == 0.0 || 
			(QPres[i] > 1.5 && QPres[i] != 77 && QPres[i] != 88))
			BadPts[NBad++] = i;

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



#ifdef notdef
/*
 * ingest.c --- A common ingest interface and support routines for 
 *		Zeb ingest modules
 */

# include <stdio.h>
# include <varargs.h>
# include <copyright.h>
# include <ctype.h>
# include <ui_error.h>
# include "defs.h"
# include "message.h"
# include "timer.h"
# include "DataStore.h"
# include "DataChunk.h"
# include "ds_fields.h"

# ifndef streq
# define streq(a,b) (strcmp(a,b) == 0)
# endif

static void	IngestLog FP((int, va_dcl));
static int	incoming FP((struct message *));

static int IngestLogFlags = 0;	/* Specifies what types of messages are
				 * written to the terminal */
static char *IngestName;	/* Message name of ingest module */

void
ListAvailableFields()
{
/*
 * Print a list of valid field names, as well as the maximum number of
 * fields which class_ingest can accept on the command line
 */
/*
 * For now we have to cheat by knowing there cannot be more than
 * 128 fields.  It would be nice to have function in the Fields package
 * which returns the number of fields (i.e. the maximum field id)
 */
/*
 * This should probably be changed to just list the fields available 
 * in a named sounding file...  how to do that?
 */
	FieldId field;
	char *field_name;

	for (field = 0; field<128; ++field)
	{
		if ((field_name = F_GetName(field)))
			printf("%-10s%-50s%-10s\n",
				field_name,
				F_GetDesc(field),
				F_GetUnits(field));
	}
}



void
IngestLog(flags, va_alist)
int flags;
va_dcl
/*
 * Send messages to the event logger and to stdout, according
 * to the global IngestLogFlags variable
 */
{
	va_list args;
	struct msg_elog *el;
	static char cbuf[1024];
	char *fmt;

	va_start(args);
	fmt = va_arg(args, char *);

/*
 * First check our local debug flag
 */
	if ((flags & EF_EMERGENCY) || (IngestLogFlags & flags))
	{
		printf("%s: ",IngestName);
		vprintf(fmt, args);
		printf("\n");
	}

/*
 * Now create the message to the event logger
 */
	el = (struct msg_elog *) cbuf;
	vsprintf(el->el_text, fmt, args);
	va_end(args);

/*
 * Send the message
 */
	el->el_flag = flags;
	msg_send("Event logger", MT_ELOG, 0, el,
		sizeof(*el) + strlen(el->el_text));
}


static int
incoming (msg)
struct message *msg;
/*
 * Deal with incoming messages.
 */
{
	switch (msg->m_proto)
	{
	   case MT_TIMER:
	   	tl_DispatchEvent ((struct tm_time *) msg->m_data);
		break;
	}
	return (0);
}


void
IngestUsage()
{
	printf ("General ingest options:\n");
	printf ("   -log all|p|d|i	Set the log messages to write out\n");
	printf ("	all: 	all\n");
	printf ("	  p: 	problems\n");
	printf ("	  d: 	debugging\n");
	printf ("	  i: 	informational\n");
	printf ("   -help		Show this information\n");
}


void
IngestParseOptions(argc, argv)
	int *argc;
	char *argv[];
{
	int i,j;
	int get_msgs = 0;
	char *arg;

	i = 1;
	while (i < *argc)
	{
		if (get_msgs)
		{
		   arg = argv[i];
		   if (streq(arg,"all"))
		      IngestLogFlags = 0xff;
		   else
      		      while (!(*arg))
      		      {
         		 switch(*arg)
          		 {
    			    case 'd':
       			       IngestLogFlags &= EF_DEBUG;
			       break;
			    case 'p':
			       IngestLogFlags &= EF_PROBLEM;
			       break;
			    case 'i':
			       IngestLogFlags &= EF_INFO;
			       break;
			    default:
			       fprintf(stderr,"Invalid log flag: %c\n",*arg);
         		 }
			 ++arg;
      		      };
		   get_msgs = 0;
		}
		else if (streq(argv[i],"-help"))
		{
		   usage(argv[0]);
		   exit(0);
		}
		else if (streq(argv[i],"-log"))
		{
		   get_msgs = 1;
		}
		else
		{
		   ++i;
		   continue;
		}

		/* Remove any options that are found */
		--(*argc);
		for (j = i; j < *argc; ++j) 
		   argv[j] = argv[j+1];
	}
}


void
IngestInitialize(name)
	char *name;		/* Message name of this ingest module */
{

	IngestName = name;

	usy_init ();
	msg_connect (incoming, IngestName);
	if (! ds_Initialize ())
	{
		IngestLog(EF_EMERGENCY,"Error: ds_Initialize() failed.");
		exit(1);
	}
	F_Init();			/* Init field ID table */
}
#endif
