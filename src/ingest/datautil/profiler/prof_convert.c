/*
 * Convert a composite profiler data file to Zeb data store files. 
 */
static char *rcsid = "$Id: prof_convert.c,v 1.4 1994-11-17 03:42:18 granger Exp $";
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

# include "pc_cmds.h"
# include "netcdf.h"


# define STRLEN		80
# define MAXNAMES	50
# define MAXFIELDS	50
# define BADVAL		999999.0
# define INTBADVAL	-127
# define NX		36
# define XSPACING	250
# define BIG		999999.0

/*
 * Info for converting lat/lon to profiler name.
 */
struct name_list
{
	float	nl_lat, nl_lon;
	char 	*nl_name;
	int	nl_first_time;
} NameList[MAXNAMES];


/*
 * Info for netCDF vs. Zeb field names.
 */
struct field_list
{
	char	*fl_cdffield;
	char	*fl_zebfield;
	char	*fl_mode;
	FieldId	fl_fid;
}FieldList[MAXFIELDS];


/*
 * Our routines.
 */
int		Die FP ((void));
static int	Dispatcher FP ((int, struct ui_command *));
static void	Go FP ((void));
static void	NewName FP ((struct ui_command *));
static void	NewField FP ((struct ui_command *));
static int	GetPlatformName FP ((double, double, char *, int *));		


/*
 * Global stuff.
 */
static int	NumNames = 0;		/* Number of profiler names.	*/
static int	NumFields = 0;		/* Number of profiler fields.	*/
static char	InputFile[STRLEN];	/* Name of the input file.	*/


main (argc, argv)
int	argc;
char	**argv;
{
	char	loadfile[200];
	SValue	v;
/*
 * Check arguments.
 */
	if (argc < 2)
	{
		printf ("Usage: prof_convert <input file> [profiler names]\n");
		exit (0);
	}
	strcpy (InputFile, argv[1]);
/*
 * Hook into the Zeb world.
 */
	fixdir ("DSI_LOAD_FILE", GetLibDir (), "prof_convert.lf", loadfile);
	if (argc > 2)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[2];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	msg_connect (Die, "Prof_Convert");
	ds_Initialize ();
/*
 * Time to go into UI mode.
 */
	ui_get_command ("initial", "DSI>", Dispatcher, 0);
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
		case PC_GO:
			Go ();
			break;
	/*
	 * Define a profiler location and name.
	 */
		case PC_NAME:
			NewName (cmds + 1);
			break;
	/*
	 * Define a profiler field.
	 */
		case PC_FIELD:
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
	int	ndims;
	int	cdfid, recordid;
	int	yearid, monthid, dayid, hourid, minuteid;
	int	latid, lonid, altid;
	int	varid[MAXFIELDS];
	long	records, index[1], *start, *count;
	short	year; 
	char	month, day, hour, minute;
	int	iyear, imonth, iday, ihour, iminute;	
	float	lat, lon, alt;
	int	incid;
	float	inc;
	char	platform[STRLEN], platform_lo[STRLEN], platform_hi[STRLEN];
	char	atime[40];
	nc_type	type;
	float	*float_val;
	short	*short_val;
	char	*byte_val;
	DataChunk	*dc_lo, *dc_hi;
	FieldId		fid_lo[MAXFIELDS], fid_hi[MAXFIELDS];
	Location	loc;
	RGrid		rginfo;
	float		data[NX];
	int	num_lo_fields, num_hi_fields;
	ZebTime	zt;
	int	first_time;
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
 * Get the rest of the variable id's and fid's.
 */
	num_lo_fields = 0;
	num_hi_fields = 0;
	for (i = 0; i < NumFields; i++)
	{
	/*
	 * NetCDF id.
	 */
		varid[i] = ncvarid (cdfid, FieldList[i].fl_cdffield);
	/*
	 * Zeb field id.
	 */
                if (strcmp (FieldList[i].fl_mode, "lo") == 0)
                {
                        fid_lo[num_lo_fields] = 
                                F_Lookup (FieldList[i].fl_zebfield);
                        FieldList[i].fl_fid = fid_lo[num_lo_fields];
                        num_lo_fields++;
                }
                else if (strcmp (FieldList[i].fl_mode, "hi") == 0)
                {
                        fid_hi[num_hi_fields] = 
                                F_Lookup (FieldList[i].fl_zebfield);
                        FieldList[i].fl_fid = fid_hi[num_hi_fields];
                        num_hi_fields++;
                }
                else if (strcmp (FieldList[i].fl_mode, "both") == 0)
                {
                        fid_lo[num_lo_fields] = fid_hi[num_hi_fields] = 
                                F_Lookup (FieldList[i].fl_zebfield);
                        FieldList[i].fl_fid = fid_lo[num_lo_fields];
                        num_lo_fields++;
                        num_hi_fields++;
                }
        }
/*
 * NetCDF id's and field id's for the calculated fields.
 */
	incid = ncvarid (cdfid, "height_inc");
	fid_lo[num_lo_fields] = fid_hi[num_hi_fields] = 
		F_Lookup ("height");
	num_lo_fields++;
	num_hi_fields++;
/*
 * How many records in this file?
 */
	ncdiminq (cdfid, recordid, (char *) 0, &records); 
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
	 * Access latitude, longitude, and altitude and get a platform name.
	 */
		ncvarget1 (cdfid, latid, index, &lat);
		ncvarget1 (cdfid, lonid, index, &lon);
		ncvarget1 (cdfid, altid, index, &alt);
	
		loc.l_lat = lat;
		loc.l_lon = lon;
		loc.l_alt = alt;
	
		if (! GetPlatformName (lat, lon, platform, &first_time))
		{
			msg_ELog (EF_PROBLEM, "Unknown platform at %f %f",
				lat, lon);
			continue;
		}
		sprintf (platform_lo, "%s_l", platform); 
		sprintf (platform_hi, "%s_h", platform); 
	/*
	 * Get a data chunk set up.
	 */
		dc_lo = dc_CreateDC (DCC_RGrid);
		dc_hi = dc_CreateDC (DCC_RGrid);
		if ((dc_lo->dc_Platform = ds_LookupPlatform (platform_lo)) == 
			BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Bad Platform '%s'.",platform_lo);
			continue;
		}
		if ((dc_hi->dc_Platform = ds_LookupPlatform (platform_hi)) == 
			BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Bad Platform '%s'.",platform_hi);
			continue;
		}
	/*
	 * Set up the fields. 
	 */
		dc_RGSetup (dc_lo, num_lo_fields, fid_lo); 
		dc_RGSetup (dc_hi, num_hi_fields, fid_hi); 
	/*
	 * Set the badvalue.
	 */
		dc_SetBadval (dc_lo, BADVAL);
		dc_SetBadval (dc_hi, BADVAL);
	/*
	 * Access the time.
	 */
		ncvarget1 (cdfid, yearid, index, &year);
		ncvarget1 (cdfid, monthid, index, &month);
		ncvarget1 (cdfid, dayid, index, &day);
		ncvarget1 (cdfid, hourid, index, &hour);
		ncvarget1 (cdfid, minuteid, index, &minute);
	/*
	 * Convert to integers.
	 */
		iyear = (int) year - 1900;
		imonth = (int) month;
		iday = (int) day;
		ihour = (int) hour;
		iminute = (int) minute;
	/*
	 * Serious ugliness in these files.
	 */
	 	if (ihour < 0)
			ihour = 0;
		if (iminute < 0)
			iminute = 0;
	/*
	 * Check for badvalues in the time.
	 */	
		if ((iyear == INTBADVAL) || (imonth == INTBADVAL) ||
		    (iday == INTBADVAL) || (ihour == INTBADVAL))
			continue;

		if (iminute == INTBADVAL)
			iminute = 0;
	/*
	 * Get a ZebTime.
	 */
		TC_ZtAssemble (&zt, iyear, imonth, iday, ihour, iminute, 0, 0);
	/*
	 * Access all data fields for direct conversion.
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
				start = (long *) malloc (sizeof (long));
				count = (long *) malloc (sizeof (long));
				start[0] = r;
				count[0] = 1;
			}
			else if (ndims == 2)
			{
				start = (long *) malloc (2 * sizeof (long));
				count = (long *) malloc (2 * sizeof (long));
				start[0] = r;
				start[1] = 0;
				count[0] = 1;
				count[1] = NX;
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
			float_val = NULL;
			short_val = NULL;
			byte_val = NULL;
			if (type == NC_FLOAT)
			{
				float_val = (float *) malloc (sizeof (float)
						* NX); 
				ncvarget (cdfid, varid[f], start, count, 
					(void *) float_val); 
			}
			else if (type == NC_SHORT)
			{
				short_val = (short *) malloc (sizeof (short)
						* NX); 
				ncvarget (cdfid, varid[f], start, count, 
					(void *) short_val); 
			}
			else if (type == NC_BYTE)
			{
				byte_val = (char *) malloc (sizeof (char) * NX); 
				ncvarget (cdfid, varid[f], start, count, 
					(void *) byte_val); 
			}
		/*
		 * Save all the data as a 1d grid of floats.
		 */	
			if (ndims == 1)
			{
				for (i = 0; i < NX; i++)
				{
				    if (type == NC_FLOAT)
				    {
					if (float_val[0] > BIG)
					    data[i] = BADVAL;
					else
					    data[i] = float_val[0]; 
				    }
				    else if (type == NC_SHORT)
				    {
					if (short_val[0] == INTBADVAL)
					    data[i] = BADVAL;
					else
					    data[i] = (float) short_val[0]; 
				    }
				    else if (type == NC_BYTE)
				    {
					if (byte_val[0] == INTBADVAL)
					    data[i] = BADVAL;
					else
					    data[i] = (float) byte_val[0];  
				    }
				}
			}
			else if (ndims == 2)
			{
				for (i = 0; i < NX; i++)
				{
				    if (type == NC_FLOAT)
				    {
					if (float_val[i] > BIG)
					    data[i] = BADVAL;
					else
					    data[i] = float_val[i]; 
				    }
				    else if (type == NC_SHORT)
				    {
					if (short_val[i] == INTBADVAL)
					    data[i] = BADVAL;
					else
					    data[i] = (float) short_val[i]; 
				    }
				    else if (type == NC_BYTE)
				    {
					if (byte_val[i] == INTBADVAL)
					    data[i] = BADVAL;
					else
					    data[i] = (float) byte_val[i];  
				    }
				}
			}
		/*
		 * Fill in the grid information.
		 */
			rginfo.rg_Xspacing = XSPACING;
			rginfo.rg_Yspacing = 0;
			rginfo.rg_Zspacing = 0;
			rginfo.rg_nX = NX;
			rginfo.rg_nY = 1;
			rginfo.rg_nZ = 1;
		/*
		 * Store the data in the dc.
		 */
			if (strcmp (FieldList[f].fl_mode, "lo") == 0)
				dc_RGAddGrid (dc_lo, 0, FieldList[f].fl_fid, 
					&loc, &rginfo, &zt, data, 0);  
			else if (strcmp (FieldList[f].fl_mode, "hi") == 0)
				dc_RGAddGrid (dc_hi, 0, FieldList[f].fl_fid, 
					&loc, &rginfo, &zt, data, 0);  
			else if (strcmp (FieldList[f].fl_mode, "both") == 0)
			{
				dc_RGAddGrid (dc_lo, 0, FieldList[f].fl_fid, 
					&loc, &rginfo, &zt, data, 0);  
				dc_RGAddGrid (dc_hi, 0, FieldList[f].fl_fid, 
					&loc, &rginfo, &zt, data, 0);  
			}
		/*
		 * Free memory.
		 */
			free (start);
			free (count);
			if (float_val) free (float_val);
			if (short_val) free (short_val);
			if (byte_val) free (byte_val);
		}
	/*
	 * Calculated Fields (low mode).
	 */
		ncvarget1 (cdfid, incid, index, &inc);
		for (i = 0; i < NX; i++)
			data[i] = inc * i; 
		dc_RGAddGrid (dc_lo, 0, F_Lookup ("height"), &loc, &rginfo, 
			&zt, data, 0);  
	/*
	 * Calculated Fields (high mode).
	 */
		for (i = 0; i < NX; i++)
			data[i] = inc * (i + NX); 
		dc_RGAddGrid (dc_hi, 0, F_Lookup ("height"), &loc, &rginfo, 
			&zt, data, 0);  
	/*
	 * Store the data chunk (opening a new file the first time thru).
	 */
		TC_EncodeTime (&zt, TC_Full, atime);
		if (first_time)
		{
			msg_ELog (EF_INFO, "(F)Storing '%s' at %s.", platform,
				atime);
			ds_Store (dc_lo, TRUE, NULL, 0);
			ds_Store (dc_hi, TRUE, NULL, 0);
		}
		else
		{
			msg_ELog (EF_INFO, "Storing '%s' at %s.", platform,
				atime);
			ds_Store (dc_lo, FALSE, NULL, 0);
			ds_Store (dc_hi, FALSE, NULL, 0);
		}
	/*
	 * Free the data chunk.
	 */
		dc_DestroyDC (dc_lo);
		dc_DestroyDC (dc_hi);
	}
/*
 * Close the input file.
 */
	ncclose (cdfid);
}


static void
NewName (cmds)
struct ui_command *cmds;
/*
 * Add another name to the list.
 */
{
/*
 * Make sure we don't have too many.
 */
	if (NumNames >= MAXNAMES)
	{
		msg_ELog (EF_PROBLEM, "Too many names.");
		return;
	}
/*
 * Remeber the stuff.
 */
	NameList[NumNames].nl_name = usy_string (UPTR (*cmds));
	NameList[NumNames].nl_lat = UFLOAT (cmds[1]);
	NameList[NumNames].nl_lon = UFLOAT (cmds[2]);
	NameList[NumNames].nl_first_time = TRUE; 
	NumNames++;
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
	FieldList[NumFields].fl_mode = usy_string (UPTR (cmds[2]));
	NumFields++;
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
GetPlatformName (lat, lon, name, first_time)
float	lat, lon;
char	*name;
int	*first_time;
/*
 * Find the platform name associated with these lat/lon values.
 */
{
	int	i;
	float	latdiff, londiff;
/*
 * Compare with the known locations and names.
 */
	name[0] = '\0';	
	for (i = 0; i < NumNames; i++)
	{
		latdiff = lat - NameList[i].nl_lat;
		londiff = lon - NameList[i].nl_lon;

		if (((latdiff < .1) && (latdiff > -.1)) &&
		    ((londiff < .1) && (londiff > -.1)))
		{
			strcpy (name, NameList[i].nl_name);
			*first_time = NameList[i].nl_first_time;
			NameList[i].nl_first_time = FALSE;
			break;
		}
	}
/*
 * Return what we've found.
 */
	if (i == NumNames)
		return (FALSE);
	else return (TRUE);
}
