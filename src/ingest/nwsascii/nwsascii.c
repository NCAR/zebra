/*
 * Convert an ASCII surface data file to Zeb data store files.  Based on
 * surf_convert by Chris Burghart, which converted Storm Project Office
 * netCDF files from EBUFR to Zeb irgrid netCDF files.  Reads in
 * configuration information about subplatforms, subplatform locations, and
 * derived fields, and uses the information when reading station reports
 * sample by sample from the ASCII data file.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

# include <config.h>
# include <copyright.h>

# include <stdio.h>
# include <math.h>

# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>
# include <DataChunk.h>
# include <ingest.h>

MAKE_RCSID("$Id: nwsascii.c,v 1.1 1994-04-15 20:24:58 granger Exp $")

# include "sc_cmds.h"
# include "netcdf.h"

# define LOADFILE 	"nwsascii.lf"
# define MESSAGE_NAME 	"nws<--ascii"

# define STRLEN		80
# define CDF_STR_LEN	11
# define MAXFIELDS	50
# define MAXPLATS	1024
# define BADVAL		-9999.0
# define INTFILLVAL	-1

# define DEG_TO_RAD(x)	((x)*0.017453292)

#define DS_FETCH(a,b,c,d,e,f,g,h) \
	((DryRun)?(NULL):(ds_Fetch(a,b,c,d,e,f,g,h)))

/*
 * Subplatform names and locations.
 */
char		*SubPlats[MAXPLATS];
int		SubPids[MAXPLATS];
Location	Locs[MAXPLATS];

/*
 * Info for STORM Project Office netCDF vs. Zeb field names.
 */
struct field_list
{
	char	*fl_cdffield;
	char	*fl_zebfield;
	char	*fl_units;
	float	fl_scale, fl_offset;
	FieldId	fl_fid;
	float	fl_data[MAXPLATS];
}FieldList[MAXFIELDS];


/*
 * Our routines.
 */
int		Die FP ((void));
static int	Dispatcher FP ((int, struct ui_command *));
static void	Go FP ((void));
static void	ParseFields FP((char *buf, int plat_index));
static void	GetField FP((char *buf, float *val));
static void	StoreField FP((char *field, int plat, float val));
static void	StoreSample FP ((ZebTime *));
static void	NewField FP ((struct ui_command *));
static void	NewSubplatform FP ((struct ui_command *));
static bool	CdfToZt FP ((short, char, char, char, char, ZebTime *));
static int	ZtEqual FP ((ZebTime, ZebTime));
static void	InitFieldData FP ((void));
static int	GetPlatIndex FP ((char *));
static void	fix_up_plat FP ((char *));
static void	SetupLocals FP ((void));
static void	DoLocals FP ((int));

/*
 * Sample time, in seconds, for coercing data times.  (If it's less than zero,
 * just take the times as reported.)
 */
static int	Tsamp = -1;

/*
 * Global stuff.
 */
static PlatformId	Pid;		/* Platform id			*/
static int	NumFields = 0;		/* Number of fields.		*/
static FieldId	Fids[MAXFIELDS];
static int	NumFileFields = 0;	/* # of fields from input file	*/
static int	NumLocalFields = 0;	/* # of calculated fields	*/
static int	NumSubplats = 0;	/* Number of platforms.		*/
static char	InputFile[STRLEN];	/* Name of the input file.	*/
static char	PlatformName[STRLEN];	/* Name of the platform.	*/
static int	WspdNdx;		/* index of wind speed field	*/
static int	WdirNdx;		/* index of wind dir. field	*/


static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s <input_file> <platform> [<command_file>]\n",
		prog);
	IngestUsage();
}



main (argc, argv)
int	argc;
char	**argv;
{
	char	loadfile[200];
	int	i;
	SValue	v;
/*
 * Check arguments.
 */
	IngestParseOptions (&argc, argv, Usage);
	if ((argc < 3) || (argc > 4))
	{
		Usage (argv[0]);
		exit (0);
	}
	strcpy (InputFile, argv[1]);
	strcpy (PlatformName, argv[2]);
/*
 * Hook into the Zeb world.
 */
	usy_init ();
	F_Init ();
	fixdir ("DSI_LOAD_FILE", LIBDIR, LOADFILE, loadfile);
	if (argc > 3)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[3];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	if (! DryRun)
	{
		msg_connect (Die, MESSAGE_NAME);
		ds_Initialize ();
	}
	IngestLog (EF_INFO, "Ingesting surface data file '%s'", InputFile);
/*
 * Time to go into UI mode.
 */
	ui_get_command ("initial", "Surf>", Dispatcher, 0);
	
	IngestLog (EF_INFO, "Finished.");
	ui_finish ();

	exit (0);
}



static int
Dispatcher (junk, cmds)
int	junk;
struct ui_command	*cmds;
/*
 * The command dispatcher.
 */
{
	switch (UKEY (*cmds))
	{
	/*
	 * Time to actually do things.
	 */
	    case SC_GO:
		Go ();
		break;
	/*
	 * Define a sub-platform.
	 */
	    case SC_SUBPLATFORM:
		NewSubplatform (cmds + 1);
		break;
	/*
	 * Define a field.
	 */
	    case SC_FIELD:
		NewField (cmds + 1);
		break;
	/*
	 * Sample time, in seconds.
	 */
	    case SC_SAMPLE:
		Tsamp = UINT (cmds[1]);
		break;
	/*
	 * Complain.
	 */
	    default:
		IngestLog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
		break;
	}
	return (TRUE);
}



static void
ReadHeader (infile, zt)
FILE *infile;
ZebTime *zt;
/*
 * Read the first line of the NWS ASCII file, parse it for a time,
 * and return the time in *zt.  Abort if unsuccessful.
 */
{
	char buf[256];
	int match;
	int month, year, mday, day, hour, dump;

	if (! fgets (buf, 256, infile))
	{
		perror (InputFile);
		IngestLog (EF_PROBLEM, "error %d reading first line of %s",
			   errno, InputFile);
		Die ();
	}

	match = sscanf (buf, "HRLY-DTA-%d-%d-%d-%d ",
			&hour, &mday, &year, &dump);
	if (match != 4)
	{
		IngestLog (EF_PROBLEM, "header line does not match expected");
		Die ();
	}
	month = mday / 100;
	day = mday % 100;
	TC_ZtAssemble (zt, year, month, day, hour, 0, 0, 0);
}



static void
Go ()
/*
 * Start converting some data.
 */
{
	FILE    *infile;
	char	buf[256];
	int	i, f, t_offset;
	int	plat_index;
	long	r;
	int	hhmm, hour, minute;
	int 	year, month, day;
	ZebTime	report, zt, last_zt;
/*
 * Open the input file.
 */
	infile = fopen (InputFile, "r");
/*
 * Read the header of this file to verfiy type get the report time
 */
	ReadHeader (infile, &report);
	TC_ZtSplit (&report, &year, &month, &day, 0, 0, 0, 0);
/*
 * Set up the locally calculated variables
 */
	SetupLocals ();
/*
 * Get the Zeb fid's.
 */
	for (i = 0; i < NumFields; i++)
	{
		Fids[i] = F_DeclareField (FieldList[i].fl_zebfield, 
			FieldList[i].fl_zebfield, FieldList[i].fl_units);
		FieldList[i].fl_fid = Fids[i];
	}
/*
 * Get the main platform id.
 */
	if ((Pid = ds_LookupPlatform (PlatformName)) == BadPlatform)
	{
		IngestLog (EF_PROBLEM, "No platform '%s' in data store", 
			   PlatformName);
		Die ();
	}
/*
 * Initialize the field data.
 */
	InitFieldData ();
/*
 * Initialize last_zt
 */
	last_zt.zt_Sec = 0;
/*
 * Loop through all records, accessing the data.
 */
	r = 0;
	while ( fgets (buf, 256, infile) )
	{
		if (((r + 1) % 200) == 0)
			IngestLog (EF_DEBUG, "Record %d", r + 1);
	/*
	 * Get a sub-platform name.
	 */
		buf[5] = '\0';
		fix_up_plat (buf);
		if ((plat_index = GetPlatIndex (buf)) < 0)
		{
			IngestLog (EF_PROBLEM, 
				   "No match for '%s'", buf);
			continue;
		}
	/*
	 * Access the time.
	 */
		sscanf (buf+60,"%d", &hhmm);
		hour = hhmm / 100;
		minute = hhmm % 100;
	/*
	 * Get a ZebTime
	 */
		TC_ZtAssemble (&zt, year, month, day, hour, minute, 0, 0);
	/*
	 * "Round" to the nearest sample time if requested
	 */
		if (minute > 0 && minute < 45)
			IngestLog (EF_INFO, "Unexpected minute: %d", minute);

		if (Tsamp > 0)
		{
			int diff = zt.zt_Sec % Tsamp;
			zt.zt_Sec -= diff;
			if (diff > (Tsamp / 2))
				zt.zt_Sec += Tsamp;
		}
	/*
	 * Initialize last_zt if necessary
	 */
		if (last_zt.zt_Sec == 0)
			last_zt = zt;
	/*
	 * If we have a time change, we can write out the previous
	 * sample
	 */
		if (! ZtEqual (zt, last_zt))
		{
			StoreSample (&last_zt);
		/*
 		 * Fill the data list with bad values
		 */
			InitFieldData ();
		/*
 		 * Set last_zt to zt.
		 */
			last_zt = zt;
		}
	/*
	 * Grab the fields from the file
	 */
		ParseFields (buf, plat_index);
	/*
	 * Do the locally calculated fields
	 */
		DoLocals (plat_index);
	/*
	 * Feedback: print the data from this report and its time
	 */
		TC_EncodeTime (&zt, TC_Full, buf);
		sprintf (buf+strlen(buf), " %d ", plat_index);
		for (f = 0; f < NumFields; ++f)
		{
			sprintf (buf+strlen(buf), " %s:%g",
				 FieldList[f].fl_zebfield,
				 FieldList[f].fl_data[plat_index]);
		}
		IngestLog (EF_DEVELOP, "%s", buf);
	}
/*
 * Store the last sample
 */
	StoreSample (&last_zt);
/*
 * Close the input file.
 */
	fclose (infile);
}



static void
ParseFields (buf, plat_index)
char *buf;	/* The station ASCII record */
int plat_index;	/* Index of the subplat	    */
{
/*
 * Each of the fields is located by a specific range of characters in
 * the record.  The range is given as it is in the NWS memo on the format,
 * where the indexing starts at 1.  We must subtract one to get the
 * offset into buf[].  There must be room after each fields range
 * to insert a NUL byte to terminate the field string.
 */
	static struct field_token {
		char 	*name;	/* Name of the field		*/
		int	start;	/* Start of field in record 	*/
		int	last;	/* Last character in field	*/
	} Tokens[] =
	{
		{ "tdry",	66,	68 },
		{ "dp",		70,	72 },
		{ "horiz_vis",	74,	77 },
		{ "wdir",	90,	92 },
		{ "wspd",	94,	96 },
		{ "wmax",	98,	100 },
		{ "cpres0",	102,	104 },
		{ "rain6hr",	144,	145 }
	};
	static int NTokens = sizeof(Tokens)/sizeof(Tokens[0]);
	struct field_token *tok;
	float val;
	int i;

	for (i = 0, tok = Tokens; i < NTokens; ++i, ++tok)
	{
		buf[tok->last] = '\0';
		GetField (buf + tok->start - 1, &val);
		StoreField (tok->name, plat_index, val);
	}
}



static void
GetField (buf, val)
char *buf;
float *val;
{
	if (sscanf (buf, "%f", val) != 1)
		*val = BADVAL;
}



static void
StoreField (field, plat, val)
char *field;
int plat;
float val;
{
	FieldId fid;
	int f;

	fid = F_Lookup (field);
	if (fid == BadField)
	{
		IngestLog (EF_PROBLEM, "field '%s' unknown", field);
	}
	else
	{
		for (f = 0; f < NumFileFields; ++f)
		{
			if (FieldList[f].fl_fid == fid)
			{
			/*
			 * Store the value, applying the scale and offset
			 */
				if (val != BADVAL)
					FieldList[f].fl_data[plat] = 
						(val * FieldList[f].fl_scale)
						+ FieldList[f].fl_offset;
				else
					FieldList[f].fl_data[plat] = BADVAL;
				break;
			}
		}
	}
}




static void
StoreSample (zt)
ZebTime	*zt;
/*
 * We have to revise our approach here since the samples and the stations
 * will not be coming in any particular order.  It's possible that the
 * report we need to store does not contain data for a sample which was
 * stored previously, and so we don't want to overwrite it.
 */
{
	DataChunk	*dc;
	int	f, plat;
	bool	newchunk;
	float	*grid;
	dsDetail dsd;
/*
 * First try to fetch a datachunk for the particular time.
 */
	newchunk = FALSE;
	dsd.dd_Name = "badval";
	dsd.dd_V.us_v_float = BADVAL;
	dc = DS_FETCH (Pid, DCC_IRGrid, zt, zt, Fids, NumFields, &dsd, 1);
	if (! dc)
	{
		/*
		 * Create a new data chunk.
		 */	
		dc = dc_CreateDC (DCC_IRGrid);
		dc->dc_Platform = Pid;
		dc_IRSetup (dc, NumSubplats, SubPids, Locs, NumFields, Fids); 
		dc_SetBadval (dc, BADVAL);
		newchunk = TRUE;
	}
/*
 * Store all the grids in the data chunk.
 */
	for (f = 0; f < NumFields; f++)
	{
		if (newchunk)
		{
			dc_IRAddGrid(dc, zt, 0, Fids[f], FieldList[f].fl_data);
			continue;
		}
		grid = dc_IRGetGrid (dc, 0, Fids[f]);
		for (plat = 0; plat < NumSubplats; ++plat)
		{
			if (FieldList[f].fl_data[plat] != BADVAL)
				grid[plat] = FieldList[f].fl_data[plat];
		}
	}
/*
 * Store the data chunk in the data store.
 */
	ds_Store (dc, FALSE, NULL, 0);
/*
 * Free the data chunk.
 */
	dc_DestroyDC (dc);
}



static void
NewField (cmds)
struct ui_command *cmds;
/*
 * Add another field to the list.
 */
{
/*
 * Make sure we don't have too many.
 */
	if (NumFields >= MAXFIELDS)
	{
		IngestLog (EF_PROBLEM, "Too many fields.");
		return;
	}
/*
 * Remeber the stuff.
 */
	FieldList[NumFields].fl_cdffield = usy_string (UPTR (cmds[0]));
	FieldList[NumFields].fl_zebfield = usy_string (UPTR (cmds[1]));
	FieldList[NumFields].fl_units = usy_string (UPTR (cmds[2]));
	FieldList[NumFields].fl_scale = UFLOAT (cmds[3]);
	FieldList[NumFields].fl_offset = UFLOAT (cmds[4]);
	NumFileFields++;
	NumFields++;
}


static void
NewSubplatform (cmds)
struct ui_command *cmds;
/*
 * Add another sub-platform to the list.
 */
{
	int	p;
	char	fullname[STRLEN];
/*
 * Make sure we don't have too many.
 */
	if (NumSubplats >= MAXPLATS)
	{
		IngestLog (EF_PROBLEM, "Too many sub-platforms.");
		return;
	}
/*
 * Remember the stuff.
 */
	p = NumSubplats++;

	SubPlats[p] = usy_string (UPTR (cmds[0]));
	Locs[p].l_lat = UFLOAT (cmds[1]);
	Locs[p].l_lon = UFLOAT (cmds[2]);
	Locs[p].l_alt = UFLOAT (cmds[3]);

	sprintf (fullname, "%s/%s", PlatformName, SubPlats[p]);
	if ((SubPids[p] = ds_LookupPlatform (fullname)) == BadPlatform)
	{
		IngestLog (EF_PROBLEM, "Platform '%s' unknown to data store", 
			   fullname);
		Die ();
	}
}



static void
SetupLocals ()
/*
 * Set up for the locally calculated fields, which get added to the end
 * of the field list.
 */
{
	int	i;
/*
 * u wind
 */
	FieldList[NumFields].fl_cdffield = "(none)";
	FieldList[NumFields].fl_zebfield = "u_wind";
	FieldList[NumFields].fl_units = "m/s";
	FieldList[NumFields].fl_scale = 1.0;
	FieldList[NumFields].fl_offset = 0.0;
	NumFields++;
	NumLocalFields++;
/*
 * v wind
 */
	FieldList[NumFields].fl_cdffield = "(none)";
	FieldList[NumFields].fl_zebfield = "v_wind";
	FieldList[NumFields].fl_units = "m/s";
	FieldList[NumFields].fl_scale = 1.0;
	FieldList[NumFields].fl_offset = 0.0;
	NumFields++;
	NumLocalFields++;
/*
 * Find the indices for wspd and wdir
 */
	WspdNdx = WdirNdx = -1;
	for (i = 0; i < NumFileFields; i++)
	{
		if (! strcmp (FieldList[i].fl_zebfield, "wspd"))
			WspdNdx = i;

		if (! strcmp (FieldList[i].fl_zebfield, "wdir"))
			WdirNdx = i;
	}
}



static void
DoLocals (plat_index)
int	plat_index;
/*
 * Do the variables we aren't reading from the netCDF file
 */
{
	int	fndx = NumFileFields;
	float	u, v, ws, wd;
/*
 * u wind and v wind
 */
	u = BADVAL;
	v = BADVAL;

	if (WspdNdx >= 0 && WdirNdx >= 0)
	{
		ws = FieldList[WspdNdx].fl_data[plat_index];
		wd = FieldList[WdirNdx].fl_data[plat_index];

		if (ws != BADVAL && wd != BADVAL)
		{
			u = ws * cos (DEG_TO_RAD (270.0 - wd));
			v = ws * sin (DEG_TO_RAD (270.0 - wd));
		}
	}

	FieldList[fndx++].fl_data[plat_index] = u;
	FieldList[fndx++].fl_data[plat_index] = v;
}

		
	
int
Die ()
/*
 * Die gracefully.
 */
{
	IngestLog (EF_INFO, "Dying.");
	ui_finish ();
	exit (1);
}



static bool
CdfToZt (year, month, day, hour, minute, zt)
short	year;
char	month, day, hour, minute;
ZebTime	*zt;
/*
 * Convert time from its netcdf representation to ZebTime
 */
{
	int	nearest;
/*
 * Check for badvalues in the time.
 */	
	if ((year == INTFILLVAL) || (month == INTFILLVAL) || 
	    (day == INTFILLVAL))
		return (FALSE);

	if (hour == INTFILLVAL)
		hour = 0;

	if (minute == INTFILLVAL)
		minute = 0;
/*
 * Get a ZebTime.
 */
	TC_ZtAssemble (zt, (int)(year - 1900), (int) month, (int) day, 
		(int) hour, (int) minute, 0, 0);
	
	return (TRUE);
}



static int
ZtEqual (zt1, zt2)
ZebTime	zt1, zt2;
/*
 * Return true of zt1 equals zt2.
 */
{
	if ((zt1.zt_Sec == zt2.zt_Sec) && (zt1.zt_MicroSec == zt2.zt_MicroSec))
		return (TRUE);
	else return (FALSE);
}



static void
InitFieldData ()
/*
 * Initialize the field data.
 */
{
	int	i, j;

	for (i = 0; i < NumSubplats; i++)
	{
		for (j = 0; j < NumFields; j++)
			FieldList[j].fl_data[i] = BADVAL;
	}
}



static int
GetPlatIndex (subplat)
char	*subplat;
/*
 * Return the index into the platform list of the platform.
 */
{
	int	i;

	for (i = 0; i < NumSubplats; i++)
		if (! strcmp (subplat, SubPlats[i]))
			return (i);

	return (-1);
}




static void
fix_up_plat (platform)
char	*platform;
/*
 * Fix up a platform name by changing it to lower case and
 * trashing the spaces on the end of the string.
 */
{
	int	i = 0;
/*
 * Remove trailing spaces
 */
	for (i = strlen (platform) - 1; platform[i] == ' '; i--)
		platform[i] = '\0';
/*
 * Change to lower case
 */
	for (i = 0; i < strlen (platform); i++)
		platform[i] = tolower (platform[i]);
}
