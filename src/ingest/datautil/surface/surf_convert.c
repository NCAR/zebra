/*
 * Convert a composite surface data file to Zeb data store files. 
 */
static char *rcsid = "$Id: surf_convert.c,v 1.1 1992-11-11 17:29:17 burghart Exp $";
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

# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>
# include <DataChunk.h>

# include "sc_cmds.h"
# include "netcdf.h"


# define STRLEN		80
# define CDF_STR_LEN	11
# define MAXFIELDS	50
# define MAXPLATS	100
# define BADVAL		99999.0
# define CDF_BAD	-999.0
# define INTBADVAL	-127

/*
 * Subplatform names and other info.
 */
char		*Platforms[MAXPLATS];

/*
 * List of locations to accompany other subplatform info.
 */
Location	Locs[MAXPLATS];

/*
 * Info for netCDF vs. Zeb field names.
 */
struct field_list
{
	char	*fl_cdffield;
	char	*fl_zebfield;
	FieldId	fl_fid;
	float	fl_data[MAXPLATS];
}FieldList[MAXFIELDS];


/*
 * Our routines.
 */
int		Die FP ((void));
static int	Dispatcher FP ((int, struct ui_command *));
static void	Go FP ((void));
static void	NewField FP ((struct ui_command *));
static void	NewPlatform FP ((struct ui_command *));
static int	CdfToZt FP ((short, char, char, char, char, ZebTime *));
static int	ZtEqual FP ((ZebTime, ZebTime));
static void	InitFieldData FP ((void));
static int	GetPlatIndex FP ((char *));
static void	fix_up_plat FP ((char *));


/*
 * Global stuff.
 */
static int	NumFields = 0;		/* Number of fields.		*/
static int	NumPlats = 0;		/* Number of platforms.		*/
static char	InputFile[STRLEN];	/* Name of the input file.	*/
static char	PlatformName[STRLEN];	/* Name of the platform.	*/


main (argc, argv)
int	argc;
char	**argv;
{
	char	loadfile[200];
	SValue	v;
/*
 * Check arguments.
 */
	if (argc < 3)
	{
		printf ("Usage: surf_convert <input file> <platform> [surface fields]\n");
		exit (0);
	}
	strcpy (InputFile, argv[1]);
	strcpy (PlatformName, argv[2]);
/*
 * Hook into the Zeb world.
 */
	fixdir ("DSI_LOAD_FILE", LIBDIR, "surf_convert.lf", loadfile);
	if (argc > 3)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[3];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	msg_connect (Die, "Surf_Convert");
	ds_Initialize ();
/*
 * Time to go into UI mode.
 */
	ui_get_command ("initial", "Surf>", Dispatcher, 0);
	Die ();
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
		case SC_PLATFORM:
			NewPlatform (cmds + 1);
			break;
	/*
	 * Define a field.
	 */
		case SC_FIELD:
			NewField (cmds + 1);
			break;
	/*
	 * Complain.
	 */
		default:
			msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
			break;
	}
	return (TRUE);
}



static void
Go ()
/*
 * Start converting some data.
 */
{
	int	i, r, f;
	int	ndims, plat_index;
	int	cdfid, recordid, latid, lonid, altid, stnid;
	int	yearid, monthid, dayid, hourid, minuteid;
	int	varid[MAXFIELDS];
	char	station_id[STRLEN];
	long	records, index[1]; 
	long	start[1], start2[2], count[1], count2[2];
	short	year; 
	char	month, day, hour, minute, platform[STRLEN];
	float	lat, lon, alt;
	nc_type	type;
	float	float_val[1];
	short	short_val[1];
	char	byte_val[1];
	DataChunk	*dc;
	FieldId		fids[MAXFIELDS];
	PlatformId	pids[MAXPLATS];
	ZebTime	zt, last_zt;
/*
 * Open the input file.
 */
	cdfid = ncopen (InputFile, NC_NOWRITE);
/*
 * Get the dimension id's.
 */
	recordid = ncdimid (cdfid, "record");
/*
 * Get the variable id's for time.
 */
	yearid = ncvarid (cdfid, "year");
	monthid = ncvarid (cdfid, "month");
	dayid = ncvarid (cdfid, "day");
	hourid = ncvarid (cdfid, "hour");
	minuteid = ncvarid (cdfid, "minute");
/*
 * Get the variable id's for lat/lon/alt.
 */
	latid = ncvarid (cdfid, "lat");
	lonid = ncvarid (cdfid, "lon");
	altid = ncvarid (cdfid, "stn_height");
/*
 * Variable id for station_id.
 */
	stnid = ncvarid (cdfid, "stn_id");
/*
 * Get the rest of the variable id's and fid's.
 */
	for (i = 0; i < NumFields; i++)
	{
	/*
	 * NetCDF id.
	 */
		varid[i] = ncvarid (cdfid, FieldList[i].fl_cdffield);
	/*
	 * Zeb field id.
	 */
		fids[i] = F_Lookup (FieldList[i].fl_zebfield);
		FieldList[i].fl_fid = fids[i];
	}
/*
 * Get the platform id's.
 */
	for (i = 0; i < NumPlats; i++)
		pids[i] = ds_LookupPlatform (Platforms[i]);
/*
 * How many records in this file?
 */
	ncdiminq (cdfid, recordid, (char *) 0, &records); 
/* 
 * Initialize the last zeb time.
 */
	last_zt.zt_Sec = 0;
	last_zt.zt_MicroSec = 0;
/*
 * Initialize the field data.
 */
	InitFieldData ();
/*
 * Loop through all records, accessing the data.
 */
	for (r = 0; r < records; r++)
	{
	/*
	 * Set up the index into the file.
	 */
		index[0] = r;
	/*
	 * Access the time.
	 */
		ncvarget1 (cdfid, yearid, index, &year);
		ncvarget1 (cdfid, monthid, index, &month);
		ncvarget1 (cdfid, dayid, index, &day);
		ncvarget1 (cdfid, hourid, index, &hour);
		ncvarget1 (cdfid, minuteid, index, &minute);
	/*
	 * Get a ZebTime.
	 */
		if (! CdfToZt (year, month, day, hour, minute, &zt))
			continue;
	/*
	 * Get a sub-platform name.
	 */
		start2[0] = r;
		start2[1] = 0;
		count2[0] = 1;
		count2[1] = CDF_STR_LEN;
		ncvarget (cdfid, stnid, start2, count2, (void *) station_id);
		fix_up_plat (station_id);
		sprintf (platform, "%s/%s", PlatformName, station_id);
		if ((plat_index = GetPlatIndex (platform)) < 0)
		{
			msg_ELog (EF_PROBLEM, "No match for '%s'.", platform);
			break;
		}
	/*
	 * Access latitude, longitude, and altitude.
	 */
		ncvarget1 (cdfid, latid, index, &lat);
		ncvarget1 (cdfid, lonid, index, &lon);
		ncvarget1 (cdfid, altid, index, &alt);
	
		Locs[plat_index].l_lat = lat;
		Locs[plat_index].l_lon = lon;
		Locs[plat_index].l_alt = alt;
	/*
	 * Access all data fields.
	 */
		for (f = 0; f < NumFields; f++)
		{
		/*
		 * Get the dimensions and data type.
		 */
			ncvarinq (cdfid, varid[f], (char *) 0, &type, &ndims, 
				(int *) 0, (int *) 0);
			if (ndims == 1)
			{
				start[0] = r;
				count[0] = 1;
			}
			else
			{
				msg_ELog (EF_PROBLEM, "Unexpected dimension %d",
					ndims);
				continue;
			}
		/*
		 * Get the data.
		 */
			float_val[0] =  short_val[0] =  byte_val[0] = NULL;
			if (type == NC_FLOAT)
			{
				ncvarget (cdfid, varid[f], start, count, 
					(void *) float_val); 
			}
			else if (type == NC_SHORT)
			{
				ncvarget (cdfid, varid[f], start, count, 
					(void *) short_val); 
			}
			else if (type == NC_BYTE)
			{
				ncvarget (cdfid, varid[f], start, count, 
					(void *) byte_val); 
			}
		/*
		 * Save all the data as a float.
		 */	
			if (type == NC_FLOAT)
			{
				if (float_val[0] == CDF_BAD)
				    FieldList[f].fl_data[plat_index] = 
					BADVAL;
				else
				    FieldList[f].fl_data[plat_index] = 
					float_val[0]; 
			}
			else if (type == NC_SHORT)
			{
				if (short_val[0] == INTBADVAL)
				    FieldList[f].fl_data[plat_index] = 
					BADVAL;
				else
				    FieldList[f].fl_data[plat_index] = 
					(float) short_val[0]; 
			}
			else if (type == NC_BYTE)
			{
				if (byte_val[0] == INTBADVAL)
				    FieldList[f].fl_data[plat_index] = 
					BADVAL;
				else
				    FieldList[f].fl_data[plat_index] = 
					(float) byte_val[0];  
			}
		}
	/*
	 * Time to start a new data_chunk?
	 */
		if (! ZtEqual (zt, last_zt))
		{
		/*
 		 * Set last_zt to zt.
		 */
			last_zt.zt_Sec = zt.zt_Sec;
			last_zt.zt_MicroSec = zt.zt_MicroSec;
		/*
		 * Create a new data chunk.
		 */	
			dc = dc_CreateDC (DCC_IRGrid);
			if ((dc->dc_Platform = ds_LookupPlatform (PlatformName))
				== BadPlatform)
			{
				msg_ELog (EF_PROBLEM, "Bad Platform '%s'.", 
					PlatformName);
				continue;
			}
		/*
		 * Set up the new data chunk.
		 */
			dc_IRSetup (dc, NumPlats, pids, Locs, NumFields, fids); 
			dc_SetBadval (dc, BADVAL);
		/*
		 * Store all the grids in the data chunk.
		 */
			for (f = 0; f < NumFields; f++)
				dc_IRAddGrid (dc, &zt, 0, fids[f],
					FieldList[f].fl_data); 	
		/*
		 * Store the data chunk in the data store.
		 */
			ds_Store (dc, FALSE, NULL, 0);
		/*
		 * Free the data chunk.
		 */
			dc_DestroyDC (dc);
		/*
 		 * Initialize the platform list.
		 */
			InitFieldData ();
		}
	}
/*
 * Close the input file.
 */
	ncclose (cdfid);
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
		msg_ELog (EF_PROBLEM, "Too many fields.");
		return;
	}
/*
 * Remeber the stuff.
 */
	FieldList[NumFields].fl_cdffield = usy_string (UPTR (*cmds));
	FieldList[NumFields].fl_zebfield = usy_string (UPTR (cmds[1]));
	NumFields++;
}


static void
NewPlatform (cmds)
struct ui_command *cmds;
/*
 * Add another sub-platform to the list.
 */
{
/*
 * Make sure we don't have too many.
 */
	if (NumPlats >= MAXPLATS)
	{
		msg_ELog (EF_PROBLEM, "Too many sub-platforms.");
		return;
	}
/*
 * Remeber the stuff.
 */
	Platforms[NumPlats] = usy_string (UPTR (*cmds));
	NumPlats++;
}



int
Die ()
/*
 * Die gracefully.
 */
{
	msg_ELog (EF_INFO, "Dying.");
	ui_finish ();
	exit (0);
}



static int
CdfToZt (year, month, day, hour, minute, zt)
short	year;
char	month, day, hour, minute;
ZebTime	*zt;
/*
 * Convert time from its netcdf representation to ZebTime.
 */
{
	int	iyear, imonth, iday, ihour, iminute;
/*
 * Convert to integers.
 */
	iyear = (int) year - 1900;
	imonth = (int) month;
	iday = (int) day;
	ihour = (int) hour;
	iminute = (int) minute;
/*
 * Check for badvalues in the time.
 */	
	if ((iyear == INTBADVAL) || (imonth == INTBADVAL) ||
	    (iday == INTBADVAL) || (ihour == INTBADVAL))
		return (FALSE);

	if (iminute == INTBADVAL)
		iminute = 0;
/*
 * Get a ZebTime.
 */
	TC_ZtAssemble (zt, iyear, imonth, iday, ihour, iminute, 0, 0);
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

	for (i = 0; i < NumPlats; i++)
	{
		for (j = 0; j < NumFields; j++)
			FieldList[j].fl_data[i] = BADVAL;
		Locs[i].l_lat = BADVAL;
		Locs[i].l_lon = BADVAL;
		Locs[i].l_alt = BADVAL;
	}
}


static int
GetPlatIndex (platform)
char	*platform;
/*
 * Return the index into the platform list of the platform.
 */
{
	int	i;

	for (i = 0; i < NumPlats; i++)
	{
		if (strcmp (platform, Platforms[i]) == 0)
			return (i);
	}
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

	while (platform[i] != ' ')
	{
		platform[i] = tolower (platform[i]);
		i++;
	}
	platform[i] = '\0';
}

