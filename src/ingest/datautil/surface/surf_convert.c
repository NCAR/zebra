/*
 * Convert a composite surface data file to Zeb data store files. 
 */
static char *rcsid = "$Id: surf_convert.c,v 1.4 1993-08-10 20:09:58 burghart Exp $";
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

# include "sc_cmds.h"
# include "netcdf.h"



# define STRLEN		80
# define CDF_STR_LEN	11
# define MAXFIELDS	50
# define MAXPLATS	1024
# define BADVAL		-9999.0
# define CDF_BAD	-999.0
# define INTFILLVAL	-1

# define DEG_TO_RAD(x)	((x)*0.017453292)

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
	if (argc < 4)
	{
		printf ("Usage: %s <input_file> <platform> [<command_file>]\n",
			argv[0]);
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
	msg_ELog (EF_INFO, "Ingesting surface data file '%s'", InputFile);
	ds_Initialize ();
/*
 * Time to go into UI mode.
 */
	ui_get_command ("initial", "Surf>", Dispatcher, 0);
	
	msg_ELog (EF_INFO, "Finished.");
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
	int	i, f, t_offset;
	int	ndims, plat_index;
	int	cdfid, recordid, stnid;
	int	yearid, monthid, dayid, hourid, minuteid;
	int	varid[MAXFIELDS];
	char	station_id[STRLEN];
	long	records, r;
	long	start[1], start2[2], count[1], count2[2];
	short	year; 
	char	month, day, hour, minute;
	nc_type	type;
	float	val, float_val;
	short	short_val;
	char	byte_val;
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
 * Variable id for station_id.
 */
	stnid = ncvarid (cdfid, "stn_id");
/*
 * Set up the locally calculated variables
 */
	SetupLocals ();
/*
 * Get the netCDF variable id's and zeb fid's.
 */
	for (i = 0; i < NumFields; i++)
	{
	/*
	 * netCDF ID (only for those we're reading from the file
	 */
		if (i < NumFileFields)
			varid[i] = ncvarid (cdfid, FieldList[i].fl_cdffield);
	/*
	 * Zeb ID
	 */
		Fids[i] = F_DeclareField (FieldList[i].fl_zebfield, 
			FieldList[i].fl_zebfield, FieldList[i].fl_units);
		FieldList[i].fl_fid = Fids[i];
	}
/*
 * Get the main platform id.
 */
	if ((Pid = ds_LookupPlatform (PlatformName)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "No platform '%s' in data store", 
			PlatformName);
		Die ();
	}
/*
 * How many records in this file?
 */
	ncdiminq (cdfid, recordid, (char *) 0, &records); 
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
	for (r = 0; r < records; r++)
	{
		if (((r + 1) % 200) == 0)
			msg_ELog (EF_DEBUG, "Record %d", r + 1);
	/*
	 * Get a sub-platform name.
	 */
		start2[0] = r;
		start2[1] = 0;
		count2[0] = 1;
		count2[1] = CDF_STR_LEN;
		ncvarget (cdfid, stnid, start2, count2, (void *) station_id);
		fix_up_plat (station_id);
		if ((plat_index = GetPlatIndex (station_id)) < 0)
		{
			msg_ELog (EF_PROBLEM, "No match for '%s'", station_id);
			continue;
		}
	/*
	 * Access the time.
	 */
		ncvarget1 (cdfid, yearid, &r, &year);
		ncvarget1 (cdfid, monthid, &r, &month);
		ncvarget1 (cdfid, dayid, &r, &day);
		ncvarget1 (cdfid, hourid, &r, &hour);
		ncvarget1 (cdfid, minuteid, &r, &minute);
	/*
	 * Get a ZebTime
	 */
		if (! CdfToZt (year, month, day, hour, minute, &zt))
		{
			msg_ELog (EF_PROBLEM, 
				"\nBad time %02d%02d%02d %02d%02d00\n",
				year, month, day, hour, minute);
			continue;
		}
	/*
	 * "Round" to the nearest sample time if requested
	 */
		if (minute > 0 && minute < 45)
			msg_ELog (EF_INFO, "Unexpected minute: %d", minute);

		if (Tsamp > 0)
		{
			int	diff = zt.zt_Sec % Tsamp;
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
	 * Grab all requested fields from the file
	 */
		for (f = 0; f < NumFileFields; f++)
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
				msg_ELog (EF_PROBLEM, 
					"Unexpected dimension %d", ndims);
				continue;
			}
		/*
		 * Get the data.
		 */
			switch (type)
			{
			    case NC_FLOAT:
				ncvarget (cdfid, varid[f], start, count, 
					(void *)(&float_val)); 

				val = (float_val == CDF_BAD) ? BADVAL : 
					float_val;
				break;
			    case NC_SHORT:
				ncvarget (cdfid, varid[f], start, count, 
					(void *)(&short_val)); 

				val = (short_val == INTFILLVAL) ? BADVAL : 
					(short) short_val;
				break;
			    case NC_BYTE:
				ncvarget (cdfid, varid[f], start, count, 
					(void *)(&byte_val)); 

				val = (byte_val == INTFILLVAL) ? BADVAL :
					(float) byte_val;  
				break;
			    default:
				msg_ELog (EF_EMERGENCY, 
					"Can't deal with type %d source data",
					type);
				Die ();
			}
		/*
		 * Store the value, applying the scale and offset
		 */
			if (val != BADVAL)
				FieldList[f].fl_data[plat_index] = val *
					FieldList[f].fl_scale + 
					FieldList[f].fl_offset;
			else
				FieldList[f].fl_data[plat_index] = BADVAL;
		}
	/*
	 * Do the locally calculated fields
	 */
		DoLocals (plat_index);
	}
/*
 * Store the last sample
 */
	StoreSample (&last_zt);
/*
 * Close the input file.
 */
	ncclose (cdfid);
}




static void
StoreSample (zt)
ZebTime	*zt;
{
	DataChunk	*dc;
	int	f;
/*
 * Create a new data chunk.
 */	
	dc = dc_CreateDC (DCC_IRGrid);
	dc->dc_Platform = Pid;
	dc_IRSetup (dc, NumSubplats, SubPids, Locs, NumFields, Fids); 
	dc_SetBadval (dc, BADVAL);
/*
 * Store all the grids in the data chunk.
 */
	for (f = 0; f < NumFields; f++)
		dc_IRAddGrid (dc, zt, 0, Fids[f], FieldList[f].fl_data);
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
		msg_ELog (EF_PROBLEM, "Too many fields.");
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
		msg_ELog (EF_PROBLEM, "Too many sub-platforms.");
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
		msg_ELog (EF_PROBLEM, "Platform '%s' unknown to data store", 
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
	msg_ELog (EF_INFO, "Dying.");
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
