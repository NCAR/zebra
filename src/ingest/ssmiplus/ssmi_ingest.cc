/* -*- C++ -*-
 *
 * Ingest module for SSM/I satellite format tapes.  Reads blocks from tape
 * and sends them off to Remote Sensing System's (RSS) FORTRAN subroutine
 * for extracting data, DECODE.  The C interface is the function decode_ssmi()
 * in another file, which maps a structure, type OUTDAT_BLOCK, onto DECODE's
 * common block OUTDAT.
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

#ifndef lint
static char *rcsid = 
	"$Id: ssmi_ingest.cc,v 1.5 1993-10-22 22:48:44 granger Exp $";
#endif

#include <time.h>
#include <math.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <iostream.h>

#include "tcl.h"

#ifdef notdef
#include "tclExtend.h"
extern "C" {	 	// for benefit of tcl++.h declarations
			// these are declared in local tclXAppInit.c
int Tcl_AppInit (Tcl_Interp *interp);
int TclX_AppInit (Tcl_Interp *interp);
}
#include "tcl++.h"
#endif

extern "C" 
{

#include <defs.h>
#include <message.h>
#include <DataStore.h>
#include <ingest.h>

}

#include "SquareScan.h"
#include "GridMap.h"
#include "Source.h"
#include "Scan.h"


const char *PLATFORM = "ssmi";		// default platform name
const int DC_BLOCK_SIZE = 0;		// # samples before storing a DC
const float DEFAULT_RESOLUTION = 12.5;	// km

inline int StoreBlocks(DataChunk *dc, bool newfile, 
		       dsDetail *details, int ndetail)
{ return ((NoDataStore)?TRUE:ds_StoreBlocks(dc,newfile,details,ndetail)); }

static void SetupFields ();
static int ProcessRecords (SSMI_Source *source, ABScan *abscan);
static DataChunk *CreateDC ();
static void IngestSquare (SquareScan& square, int *nimages);
static void IngestImage (SquareScan *ss);
static void BuildImage (SquareScan *ss, DataChunk *dc);
static void Log (const int flags, const ABScan *abscan, const int nrec);

inline double TC_Sub (const ZebTime& a, const ZebTime& b)	// a - b
{
	double sub;

	sub = a.zt_Sec - b.zt_Sec;
	sub += (a.zt_MicroSec - b.zt_MicroSec) * 1e-6;
	return (sub);
}

/*
 * Global option flags and variables, linked to tcl
 */
static char *	Ourname = "SSMI Attack";	// message handler handle
static char *	Platform = NULL;
static double	Resolution = DEFAULT_RESOLUTION;
static int 	DisableLimits = 1;		// limits disabled by default
static double	BadValue = -9999.0;
static int	MinScans = 32;		// Minimum # scans to make an image
static double	Radius = 10;		// Grid cell radius to search
static SSMIFormat Format = F_L1B;	// Format of our source file or tape
static float	OriginLat = 0.0;
static float	OriginLon = 0.0;
static int	OriginSet = 0;		// true once origin initialized

struct ScanLimits {
	float wlon;
	float slat;
	float elon;
	float nlat;
};

ScanLimits Limits = { 0.0, 0.0, 0.0, 0.0 };

struct SSMI_Field {
	char *name;
	char *long_name;
	char *units;
	Channel channel;
};

SSMI_Field SSMIFields[] = {	// this holds the list we can choose from
	/*
	 * List the low-frequency fields first.
	 */
	{ "ta19v", "Antenna temperature, 19 GHz, v-pol", "Kelvin", ch19v },
	{ "ta19h", "Antenna temperature, 19 GHz, h-pol", "Kelvin", ch19h },
	{ "ta22v", "Antenna temperature, 22 GHz, v-pol", "Kelvin", ch22v },
	{ "ta37v", "Antenna temperature, 37 GHz, v-pol", "Kelvin", ch37v },
	{ "ta37h", "Antenna temperature, 37 GHz, h-pol", "Kelvin", ch37h },
	{ "tb19v", "Brightness temperature, 19 GHz, v-pol", "Kelvin", ch19v },
	{ "tb19h", "Brightness temperature, 19 GHz, h-pol", "Kelvin", ch19h },
	{ "tb22v", "Brightness temperature, 22 GHz, v-pol", "Kelvin", ch22v },
	{ "tb37v", "Brightness temperature, 37 GHz, v-pol", "Kelvin", ch37v },
	{ "tb37h", "Brightness temperature, 37 GHz, h-pol", "Kelvin", ch37h },
	/*
	 * High-frequency next
	 */
	{ "ta85v", "Antenna temperature, 85 GHz, v-pol", "Kelvin", ch85v },
	{ "ta85h", "Antenna temperature, 85 GHz, h-pol", "Kelvin", ch85h },
	{ "tb85v", "Brightness temperature, 85 GHz, v-pol", "Kelvin", ch85v },
	{ "tb85h", "Brightness temperature, 85 GHz, h-pol", "Kelvin", ch85h },
	/*
	 * Surface indices last
	 */
	{ "sfcidx", "Surface-type index", "none", sfcidx }
};
const int MAX_FIELDS = (sizeof(SSMIFields)/sizeof(SSMIFields[0]));


SSMI_Field Fields[ MAX_FIELDS ];	// this holds the list we're ingesting

FieldId Fids[ MAX_FIELDS ];
int NumFields = 0;

ScaleInfo SSMIScales[ MAX_FIELDS ] = /* real value = data/s_Scale + s_Offset */
{
	{ 1.0, 100.0 },	/* Assuming antenna temps (K) from 100 to 356 */
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },	/* Assuming brightness temps (K) from 100 to 356 */
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 0.0 }   	/* The surface-type index */
};

ScaleInfo Scales[ MAX_FIELDS ];


/* ========================== Tcl commands =========================== */

int
ScanLimitsCmd (ClientData clientData, Tcl_Interp *interp,
	       int argc, char *argv[])
{
	double wlon, slat, elon, nlat;

	if (argc != 5)
	{
		interp->result = "ScanLimits needs exactly four floats";
		return (TCL_ERROR);
	}

	if ((Tcl_GetDouble (interp, argv[1], &wlon) != TCL_OK) ||
	    (Tcl_GetDouble (interp, argv[2], &slat) != TCL_OK) ||
	    (Tcl_GetDouble (interp, argv[3], &elon) != TCL_OK) ||
	    (Tcl_GetDouble (interp, argv[4], &nlat) != TCL_OK))
	{
		return (TCL_ERROR);
	}

	if (wlon < -180.0 || wlon > 180.0 || elon < -180.0 || elon > 180.0)
	{
		interp->result = "longitude out of range";
		return (TCL_ERROR);
	}

	if (slat < -90.0 || slat > 90.0 || nlat < -90.0 || nlat > 90.0)
	{
		interp->result = "latitude out of range";
		return (TCL_ERROR);
	}

	/*
	 * Assume that setting the limits means they want them enabled
	 */
	DisableLimits = 0;
	Limits.wlon = (float)wlon;
	Limits.slat = (float)slat;
	Limits.elon = (float)elon;
	Limits.nlat = (float)nlat;

	IngestLog (EF_DEBUG, "new scan limits: %5.1f,%5.1f to %5.1f,%5.1f",
		   Limits.wlon, Limits.slat, Limits.elon, Limits.nlat);
	IngestLog (EF_DEBUG, "limits enabled");

	return (TCL_OK);
}


/*
 * Take a list of fields, make sure we know about each one, create an
 * id for each, and finally add them to our list of fields to ingest.  We
 * don't change the current table unless we get at least one valid field.
 * Don't let them set the same field twice.
 */
int
SetFieldsCmd (ClientData clientData, Tcl_Interp *interp, 
	      int argc, char *argv[])
{
	if (argc <= 1)
	{
		interp->result = "SetFields needs at least one field name";
		return (TCL_ERROR);
	}

	int n = 0;	// valid fields so far
	FieldId fid;

	int i, f;
	Tcl_SetResult (interp, "unknown fields: ", TCL_STATIC);
	for (i = 1; i < argc; ++i)
	{
		/*
		 * Make sure this name is not already in the new list
		 */
		for (f = 0; f < n; ++f)
		{
			if (streq (Fields[f].name, argv[i]))
				break;
		}
		if (f < n)
			continue;
		/*
		 * Search for this field name in our master table
		 */
		for (f = 0; f < MAX_FIELDS; ++f)
		{
			if (streq (SSMIFields[f].name, argv[i]))
			{
				fid = F_DeclareField (SSMIFields[f].name,
						      SSMIFields[f].long_name,
						      SSMIFields[f].units);
				Fields[n] = SSMIFields[f];
				Scales[n] = SSMIScales[f];
				Fids[n++] = fid;
				break;
			}
		}
		if (f >= MAX_FIELDS)		// couldn't find a field
		{
			Tcl_AppendElement (interp, argv[i]);
		}
	}
	if (n > 0)
		NumFields = n;

	// feedback
	IngestLog (EF_DEVELOP, "new fields list, check for consistency:");
	for (f = 0; f < n; ++f)
	{
		IngestLog (EF_DEBUG, "   %s %s %s %f %f",
			   F_GetName (Fids[f]), Fields[f].long_name,
			   Fields[f].units, Scales[f].s_Scale,
			   Scales[f].s_Offset);
	}

	if (n < argc - 1)
	{
		return (TCL_ERROR);
	}

	Tcl_ResetResult (interp);
	return (TCL_OK);
}


/*
 * Set the origin lat and lon.
 */
int
OriginCmd (ClientData clientData, Tcl_Interp *interp, 
	   int argc, char *argv[])
{
	double lat, lon;

	if (argc != 3)
	{
		interp->result = "Origin expects two float arguments";
		return (TCL_ERROR);
	}

	if ((Tcl_GetDouble (interp, argv[1], &lat) != TCL_OK) ||
	    (Tcl_GetDouble (interp, argv[2], &lon) != TCL_OK))
	{
		return (TCL_ERROR);
	}

	if (lat < -90.0 || lat > 90.0 || lon < -180.0 || lon > 180)
	{
		interp->result = "Origin arguments out of range";
		return (TCL_ERROR);
	}

	OriginLat = (float)lat;
	OriginLon = (float)lon;
	OriginSet = 1;
	IngestLog (EF_DEBUG, "origin set to %.1f lat %.1f lon",
		   OriginLat, OriginLon);

	return (TCL_OK);
}


/*
 * List the field names in the current ingest list
 */
int
ListFieldsCmd (ClientData clientData, Tcl_Interp *interp, 
	       int argc, char *argv[])
{
	if (argc > 1)
	{
		Tcl_SetResult (interp, "ListFields takes no arguments",
			       TCL_STATIC);
		return (TCL_ERROR);
	}

	for (int f = 0; f < NumFields; ++f)
	{
		Tcl_AppendElement (interp, F_GetName (Fids[f]));
	}
	IngestLog (EF_DEVELOP, "fields list returned to tcl");
	return (TCL_OK);
}


/*
 * Trace the "Format" variable and set the Format enum accordingly.
 */
char *
FormatTrace (ClientData clientData, Tcl_Interp *interp, 
	     char *name1, char *name2, int flags)
{
	char *value;

	value = Tcl_GetVar (interp, name1, 
			    flags | TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
	if (value == NULL)
	{
		return (NULL);
	}
	/*
	 * See if we recognize the new value as a format name
	 */
	if (streq (value, "level1b") || streq (value, "l1b"))
	{
		Format = F_L1B;
	}
	else if (streq (value, "rss"))
	{
		Format = F_RSS;
	}
	else
	{
		// change the variable back and return the error
		Tcl_SetVar (interp, "Format", (Format == F_L1B) ?
			    "level1b" : "rss", TCL_GLOBAL_ONLY);
		Tcl_AppendResult (interp, "format '", value, 
				  "' unknown; format change ignored", NULL);
		return ("invalid format name -- ignored");
	}

	IngestLog (EF_DEVELOP, "format set to '%s'",
		   (Format == F_L1B) ? "level1b" : "rss");

	return (NULL);
}



/*
 * Open the file and ingest the data.  Expects one argument, the file name.
 * Don't return errors for a particular file, otherwise the program will
 * exit while there may be more files which were to be read.
 */
int
ProcessFileCmd (ClientData clientData, Tcl_Interp *interp, 
		int argc, char *argv[])
{
	L1B_Source l1b_source;
	SSMI_Source *source = &l1b_source;
	L1B_ABScan l1b_scan;
	ABScan *abscan = &l1b_scan;

	if (argc != 2)
	{
		interp->result = "ProcessFile needs exactly one file path";
		return (TCL_ERROR);
	}

	if (Format == F_RSS)
	{
		interp->result =
			"RSS format not supported in this version.";
		return (TCL_ERROR);
	}

        /*
         * Set the lat/lon limits for the abscan
         */
	if (DisableLimits)
	{
		IngestLog (EF_DEBUG, "Scan limits disabled");
	}
	else
	{
		abscan->SetLimits (Limits.wlon, Limits.slat,
				   Limits.elon, Limits.nlat);
	}
	if (!OriginSet)
	{
		interp->result = "origin not set; use Origin <lat> <lon>";
		return (TCL_ERROR);
	}

	// Format == F_L1B
	char *path = argv[1];
	IngestLog (EF_INFO, "Reading level 1b file '%s'", path);

	if (source->Open(path))
	{
		Tcl_AppendResult (interp, "open failed for file '",
				  path, "'", NULL);
		IngestLog (EF_PROBLEM, "%s", interp->result);
		return (TCL_OK);
	}

#ifdef MPROF
	mprof_restart("mprof.data2");
#endif

	/*
	 * Abort if we get an error, EOF, or EOM
	 */
	if (source->ReadHeader( abscan->HeaderBuffer() ) <= 0)
	{
		interp->result = "Error trying to read header.";
		IngestLog (EF_PROBLEM, "%s", interp->result);
	}
	else
	{
		/*
		 * Otherwise, process the data records which follow the header.
		 */
		if (ProcessRecords (source, abscan) >= 0)
		{
			Tcl_AppendResult (interp, "Finished file ", path, 0);
			IngestLog (EF_INFO, "%s", interp->result);
		}
		else
		{
			Tcl_AppendResult (interp, 
			  "Problem reading data records for file ", path, 0);
			IngestLog (EF_PROBLEM, "%s", interp->result);
		}
	}

	source->Close();
	return (TCL_OK);
}



/* ======================================================================= */


/*
 * Let's talk strategy:
 *
 * We'll ingest these fields into a grid: ta19v, ta19h, ta22v, ta37v,
 * ta37h, ta85v, ta85h, and sfcidx.  The 'ta' fields are all antenna
 * temperatures, 'sfcidx' is the surface-type index.  At some point we may
 * want to ingest 'tb' (brightness temperatures) as well.
 *
 * The two 85 Ghz channels have twice the resolution of the 5 lower
 * frequency channels.  This means two different RGrid info structures, one
 * for the low channels and one for the high.
 *
 * Use a DCC_RGrid class DataChunk, until an appropriate scale and offset
 * can be found for each field.  In which case, the scale and offset will
 * be stored in an array parallel to the Fields and Fids arrays.  The array
 * of ScaleInfo structures will be needed in dc_ImgSetup() and used to fill
 * in the grid in FillGrid().
 */

static void
Usage (char *prog)
{
	printf ("Usage: %s [options] [-rss|-1b] [-p <plat>] ", prog);
	printf ("[-o <file>] [-r <res>] [-f <dev>] [tcl_script]\n");
	printf ("where...\n");
	printf ("   -rss implies Remote Sensing Systems format (default)\n");
	printf ("   -1b implies MSFC DAAC level 1b file format\n");
	printf ("   <plat> is the name of a platform, ");
	printf ("   -N <name> sets <name> as the message handler handle\n");
	printf ("default: '%s'\n", PLATFORM);
	printf ("   <file> is a file to echo valid logical records to;\n");
	printf ("      -o is ignored when using level 1b format\n");
	printf ("   <dev>, if not a valid tape device or level 1b file, \n");
	printf ("      is a file produced by -o\n");
	printf ("   <res> is the resolution in km.  The default ");
	printf ("resolution is %f km.\n", DEFAULT_RESOLUTION);
	IngestUsage ();
}
	


int
main (int argc, char *argv[])
{
//	TclInterp_cl interp;
	Tcl_Interp *interp;
	char *path = NULL;	// input device
	char *script = NULL;	// tcl script

#ifdef MPROF
	mprof_stop();
#endif
/*
 * Parse general ingest options
 */
	IngestParseOptions (&argc, argv, (void (*)(...))Usage);
/*
 * Get our program-specific defaults and options
 */
	int i = 1;
	while (i < argc)
	{
		if (!strcmp(argv[i], "-p") && (i+1 < argc))
		{
			Platform = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else if (!strcmp(argv[i], "-f") && (i+1 < argc))
		{
			path = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else if (!strcmp(argv[i], "-N") && (i+1 < argc))
		{
			Ourname = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
#ifdef RSS
		else if (!strcmp(argv[i], "-o") && (i+1 < argc))
		{
			ofile = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
#endif
		else if (!strcmp(argv[i], "-rss"))
		{
			Format = F_RSS;
			IngestRemoveOptions(&argc, argv, i, 1);
		}
		else if (!strcmp(argv[i], "-1b"))
		{
			Format = F_L1B;
			IngestRemoveOptions(&argc, argv, i, 1);
		}
		else if (!strcmp(argv[i], "-r") && (i+1 < argc))
		{
			Resolution = atof(argv[i+1]);
			IngestRemoveOptions(&argc, argv, i, 2);
			if (Resolution <= 0.0)
			{
				printf ("Illegal resolution: %f",
					Resolution);
				exit (1);
			}
		}
		else
			++i;
	}
/*
 * See if we have the right arguments remaining.  If we didn't get a
 * file or device path, we should have a tcl script to eval.
 */
	if (argc == 2)
	{
		script = argv[1];
	}
	else if (argc > 2)
	{
		fprintf (stderr, "too many arguments; use -h for help\n");
		exit (1);
	}
	else if (!path)
	{
		fprintf (stderr, "need a file path or tcl script, see -h\n");
		exit (2);
	}
/*
 * Initialize TclX.  Register our Tcl commands and variables.
 */
#ifdef notdef
	if (interp.AppInit() == TCL_ERROR)
	{
		fprintf (stderr, "Error initializing extended Tcl:\n");
		if (*interp.Result())
			fprintf (stderr, "%s\n", interp.Result());
		exit (99);
	}

	interp.CreateCommand ("ScanLimits", ScanLimitsCmd);
	interp.CreateCommand ("SetFields", SetFieldsCmd);
	interp.CreateCommand ("ListFields", ListFieldsCmd);
	interp.CreateCommand ("ProcessFile", ProcessFileCmd);

	Platform = (char *) malloc (strlen(PLATFORM) + 1);
	strcpy (Platform, PLATFORM);
	interp.LinkVar ("Platform", (char *) &Platform, TCL_LINK_STRING);
	interp.LinkVar ("Resolution", (char *) &Resolution, TCL_LINK_DOUBLE);
	interp.LinkVar ("DisableLimits", 
			(char *) &DisableLimits, TCL_LINK_BOOLEAN);
	interp.LinkVar ("BadValue", (char *) &BadValue, TCL_LINK_DOUBLE);
	interp.LinkVar ("MinScans", (char *) &MinScans, TCL_LINK_INT);
	interp.LinkVar ("Radius", (char *) &Radius, TCL_LINK_DOUBLE);
	interp.SetVar ("Format", (Format == F_L1B) ? "level1b" : "rss");
	interp.TraceVar ("Format", TCL_TRACE_WRITES, FormatTrace);
#else
	interp = Tcl_CreateInterp();

	Tcl_CreateCommand (interp, "ScanLimits", ScanLimitsCmd, NULL, NULL);
	Tcl_CreateCommand (interp, "SetFields", SetFieldsCmd, NULL, NULL);
	Tcl_CreateCommand (interp, "ListFields", ListFieldsCmd, NULL, NULL);
	Tcl_CreateCommand (interp, "ProcessFile", ProcessFileCmd, NULL, NULL);
	Tcl_CreateCommand (interp, "Origin", OriginCmd, NULL, NULL);

	Platform = (char *) malloc (strlen(PLATFORM) + 1);
	strcpy (Platform, PLATFORM);
	Tcl_LinkVar (interp, "Platform", (char *) &Platform, TCL_LINK_STRING);
	Tcl_LinkVar (interp, "Ourname", (char *) &Ourname, 
		     TCL_LINK_STRING | TCL_LINK_READ_ONLY );
	Tcl_LinkVar (interp, "Resolution", 
		     (char *) &Resolution, TCL_LINK_DOUBLE);
	Tcl_LinkVar (interp, "DisableLimits", 
		     (char *) &DisableLimits, TCL_LINK_BOOLEAN);
	Tcl_LinkVar (interp, "BadValue", 
		     (char *) &BadValue, TCL_LINK_DOUBLE);
	Tcl_LinkVar (interp, "MinScans", (char *) &MinScans, TCL_LINK_INT);
	Tcl_LinkVar (interp, "Radius", (char *) &Radius, TCL_LINK_DOUBLE);
	Tcl_SetVar (interp, "Format", 
		    (Format == F_L1B) ? "level1b" : "rss", 0);
	Tcl_TraceVar (interp, "Format", TCL_TRACE_WRITES, FormatTrace, NULL);
#endif
/*
 * Connect to the data store, message, etc.  Set our default fields list
 * to all of the fields.
 */
	IngestInitialize (Ourname);
	SetupFields ();
/*
 * Now, if we have a tcl script, just evaluate it.  Otherwise, 
 * evaluate the ProcessFile command immediately.
 */
	int code;
	if (script)
	{
//		code = interp.EvalFile (script);
		code = Tcl_EvalFile (interp, script);
	}
	else
	{
//		code = interp.VarEval ("ProcessFile \"", path, "\"", NULL);
		code = Tcl_VarEval (interp, 
				    "ProcessFile \"", path, "\"", NULL);
	}

//	if (*interp.Result() != 0)
	if (interp->result[0] != 0)
	{
//		printf ("%s\n", interp.Result() );
		IngestLog ((code != TCL_OK) ? EF_PROBLEM : EF_INFO,
			   "Program exited with: %s\n", interp->result);
	}
	if (code != TCL_OK)
	{
		exit (1);
	}
	exit (0);
}




static void
SetupFields (void)
{
	int i;

	for (i = 0; i < MAX_FIELDS; ++i)
	{
		Fids[i] = F_DeclareField (SSMIFields[i].name,
					  SSMIFields[i].long_name,
					  SSMIFields[i].units);
		Scales[i] = SSMIScales[i];
		Fields[i] = SSMIFields[i];
	}
	NumFields = MAX_FIELDS;
}



static int
ProcessRecords (SSMI_Source *source, ABScan *abscan)
/*
 * Read one logical record after another, combining scan pairs into
 * into SquareScan's and converting the squares into RGrid DataChunk's.
 * Returns number data records read or an error < 0.
 */
{
	SquareScan square;	// The scan square we are building
	
	ZebTime prev;		// Time of previous scan
	ZebTime next;		// Time of the next scan to process
	int nbad = 0;		// Number of bad scans skipped
	int nrecs = 0;		// number scan records processed
	int inbounds = 0;	// Whether previous scan was valid
	int intime = 0;		// We're in a sequence of temporal proximity
	long log_orbit = 0;	// orbit number to log next
	int nimages = 0;	// Number images ingested so far
	int reuse = 0;		// Re-use the scan we read last loop around
/*
 * Now start reading scan pairs.  For each scan pair, if its within
 * the region we're focusing on, add it to the square scan set.  Once
 * the square scan is full or the scan pairs jump either in location or
 * time, convert the square scan to an image and store it.
 */
	prev.zt_Sec = 0;
	prev.zt_MicroSec = 0;
	if (DisableLimits)
		inbounds = 1;
	intime = 0;
	reuse = 0;
	while (!source->eofile() && source->MoreScans())
	{
		if (!reuse)
		{
			if (source->ReadABScan(abscan->ScanBuffer()) == 0)
				break;
			++nrecs;
		}
		reuse = 0;

		/*
		 * Log where we are 4 times every orbit
		 */
		int orbit = abscan->Orbit();

		if (orbit >= log_orbit)
		{
			log_orbit = orbit + 2500;
			log_orbit -= log_orbit % 2500;
			Log (EF_INFO, abscan, nrecs);
		}

		/*
		 * Skip the bad scans
		 */
		if (abscan->Bad())
		{
			++nbad;
			IngestLog (EF_DEBUG,
				   "bad scan #%d in orbit %d", nbad, orbit);
			continue;
		}

		abscan->ScanATime (&next);

		/*
		 * If we still haven't found a scan we want, keep going.
		 * If the previous one was no good, we don't have any
		 * bookkeeping to do either.  Only check longitude limits
		 * if the previous scan was not inbounds (i.e. the square
		 * is empty).
                 */
		if ((DisableLimits == 0) && ! abscan->WithinLimits ())
		{
			if (!inbounds)
				continue;
			else
				inbounds = 0;
		}
		/*
		 * Maybe this scan is too far ahead of the last one and
		 * should not be counted.  The limit is hard-coded at 1 hour.
		 */
		else if (intime && prev.zt_Sec &&
			 (TC_Sub (next, prev) > 3600.0))
		{
			IngestLog (EF_INFO, "time gap of %.1lf secs detected",
				   TC_Sub (next, prev));
			intime = 0;	// begin a new sequence next time
			reuse = 1;	// but start with this scan
		}
		else if (!inbounds || !intime)
		{
			/*
			 * Beginning to find scans in the right place
			 */
			IngestLog (EF_INFO, 
			   "Beginning to read image of scans...");
			inbounds = 1;
			intime = 1;
		}
		Log ((nrecs % 10) ? EF_DEVELOP: EF_DEBUG, abscan, nrecs);
		prev = next;

                /*
                 * We're within range, so add this scan to our growing
                 * collection
                 */
		if (inbounds && intime)
		{
			/*
			 * Do we do this for both brightness and 
			 * antenna temperatures?
			 */
			if (abscan->Decode (C_OUTDAT) != 0)
			{
				nbad++;		 // couldn't convert
				continue;
			}
                        square.AddOutdat (C_OUTDAT);

			/*
			 * As long as we're getting scans inbounds,
			 * and our square is not full,
			 * keep reading and keep adding to the square
			 */
			if (square.NumScans() < SQ_MAX_SCANS)
			{
				continue;
			}
				
			IngestLog (EF_INFO, "%s, ingesting %d scans so far",
				   "square full", square.NumScans());
		}
		else
		{
			IngestLog (EF_DEBUG, "Out-of-%s scan closing image",
				   (inbounds) ? "time" : "range");
		}

		/*
		 * Try to ingest what we've got so far
		 */
		IngestSquare (square, &nimages);

	} /* while MoreScans() */

	/*
	 * Warn about unexpected aborts
	 */
	if (source->MoreScans())
	{
		IngestLog (EF_PROBLEM, 
			   "unexpected eof in file %s after %d records",
			   source->Path(), nrecs);
	}
	/*
	 * Ingest anything left in our square after reading all the records
	 */
	IngestSquare (square, &nimages);

	IngestLog(EF_INFO, "Read %i records, %d bad scans, created %i images", 
		  nrecs, nbad, nimages);
	return (nrecs);
}



static void
IngestSquare (SquareScan& square, int *nimages)
{
	/*
	 * Its time to build an image from the scans in our square.
	 * Of course, squares with few scans are useless, possibly
	 * mostly out of bounds, and ignored.
	 */
	if (square.NumScans() >= MinScans)
	{
		IngestImage (&square);
		++(*nimages);
		IngestLog (EF_DEBUG, "image square ingested");
	}
	else
	{
		IngestLog (EF_PROBLEM, 
			   "only %d scans, image not built",
			   square.NumScans());
	}

	/* Clear out scans */
	IngestLog (EF_DEBUG, "clearing scans");
	square.Clear ();
}



static void
Log (const int flags, const ABScan *abscan, const int nrec)
{
	ZebTime zt;
	char buf[30];
	
	/*
	 * Take this opportunity to log how we're doing
	 */
	abscan->ScanATime(&zt);
	TC_EncodeTime (&zt, TC_FullUSec, buf);
	IngestLog (flags, "Orbit %li, Rec %i, %s, %4.2f lat %4.2f lon", 
		   abscan->Orbit(), nrec, buf, 
		   abscan->Lat(), abscan->Lon());
}



static DataChunk *
CreateDC ()
{
DataChunk 	*dc;
/*
 * Create the data chunk and put in the platform ID, field IDs, and
 * the bad value flag (even though its never used)
 */
	dc = dc_CreateDC (DCC_Image);
	if (!dc)
		return (NULL);

	if ((dc->dc_Platform = ds_LookupPlatform (Platform)) == BadPlatform)
	{
		IngestLog (EF_EMERGENCY, "Cannot get platform ID for '%s'",
			   Platform);
		dc_DestroyDC (dc);
		return (NULL);
	}

	dc_ImgSetup (dc, NumFields, Fids, Scales);
	dc_SetBadval (dc, BadValue);
	return (dc);
}



static void
IngestImage (SquareScan *ss)
/*
 * Build an image out of this SquareScan, add it to a DataChunk,
 * and ingest the DataChunk.
 */
{
	DataChunk *dc;

	dc = CreateDC ();
	if (dc == NULL)
		return;
	IngestLog (EF_INFO, "Building an image from %d scans...", 
		   ss->NumScans());
	BuildImage (ss, dc);
	ds_Store (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
}



static void
BuildImage (SquareScan *ss, DataChunk *dc)
/*
 * Create grid maps, fill a grid of values, and add it to the DataChunk.
 * The grid mapping will be different between the low-frequency and
 * hi-frequency channels: lo-freq channels have only 64 values per scan,
 * every other scan.
 */
{
	GridMap logm;	/* Grid map used for lo-freq			  */
	GridMap higm;	/* Grid map for hi-freq				  */
	GridMap *gm;
	RGrid info;	/* info on the grid we're creating		  */
	Location origin;/* Origin of our grid				  */
	Location cvt_origin; // Origin for converting lat/lon to km
	ZebTime zt;	/* Time of the sample grid			  */
	int sample;	/* Sample of DataChunk we are creating		  */
	unsigned char *image;	/* Space for grid values for this sample  */
	int i;

	/*
	 * Establish an order for our scans
	 */
	ss->Order ();

	sample = dc_GetNSample(dc);
	ss->ZebTime (&zt);
	zt.zt_Sec += zt.zt_MicroSec / 1000000;
	zt.zt_MicroSec = 0;	// don't really need microsecond accuracy
	ss->Origin (&origin);

	/*
	 * Establish the origin for converting lat/lon to kilometer
	 */
	cvt_origin.l_lat = OriginLat;
	cvt_origin.l_lon = OriginLon;
	cvt_origin.l_alt = 0;

	/* 
	 * Apparently all fields have to have the same geometry,
	 * so we'll have to ingest lo-freq data in a higher res than
	 * actually exists.
	 */
	ss->GridInfo (Resolution, &info, cvt_origin);

	/*
	 * Allocate space for the grid values.  This memory is used for
	 * all channels.
	 */
	image = (unsigned char *)
		malloc(info.rg_nX * info.rg_nY * sizeof(unsigned char));

	/*
	 * Loop through our fields and use the correct map, lo- or high-
	 * frequency for each.  Build a map if not built already.
	 */
	IngestLog (EF_DEBUG, "Filling %d fields", NumFields);
	for (i = 0; i < NumFields; ++i)
	{
		Channel ch = Fields[i].channel;
		
		if (High (ch))
			gm = &higm;
		else if (Low (ch))
			gm = &logm;
		else
			continue;
		if (!gm->Built())
		{
			gm->SetFillRadius ((float)Radius);
			gm->SetCvtOrigin (cvt_origin);
			gm->BuildMap (ss, &info, ch);
		}
		gm->FillImageGrid (image, ss, ch, Scales + i, '\0');
		dc_ImgAddImage (dc, sample, Fids[i],
				&origin, &info, &zt, image, /*len*/ 0);
	}

	if (logm.Built())
	{
		IngestLog (EF_DEVELOP, "Low-frequency grid stats:");
		logm.ReportStats (ss);
		logm.FreeGridMap ();
	}
	if (higm.Built())
	{
		IngestLog (EF_DEVELOP, "High-frequency grid stats:");
		higm.ReportStats (ss);
		higm.FreeGridMap ();
	}

	free (image);
	IngestLog (EF_INFO, "Finished building and adding the image.");
}


