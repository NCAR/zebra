/*
 * Generic data ingest from RAP's data servers.
 */
static char *rcsid = "$Id: RAP_DsIngest.c,v 1.3 1993-07-08 22:46:32 burghart Exp $";
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

# include <signal.h>
# include <stdio.h>
# include <math.h>

# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>
# include <DataChunk.h>

# include "ds_cmds.h"
# include "cdata_server.h"


# define STRLEN		80
# define MAXFIELDS	10
# define MAXZ		30
# define BADVAL		999999.0
# define RIGHT		1
# define LEFT		0


/*
 * Info for field name to number conversion.
 */
struct field_list
{
	int	fl_number;
	char 	*fl_name;
} FieldList[MAXFIELDS];

/*
 * Our routines.
 */
int			Die FP ((void));
static int		Dispatcher FP ((int, struct ui_command *));
static void		Go FP ((void));
static void		NewField FP ((struct ui_command *));
static void		SetupIndirect FP ((void));
static cd_command_t	form_request FP ((int, int));
static void		request_data FP ((void));
static void		add_grid FP ((DataChunk *, FieldId, unsigned char *[], 
				cd_grid_info_t [], cd_reply_t [], int));

/*
 * Other routines.
 */
extern unsigned char	*get_cidd_data ();
extern int 		print_reply ();
extern int 		print_info ();

/*
 * Global stuff.
 */
static char	Host[STRLEN];		/* Host name where data server is */
					/*	running.		  */ 
static int	Port = 0;		/* Port number of data.		  */
static int	Frequency = 0;		/* Frequency of data requests in  */
					/*	seconds.		  */
static char	Platform[STRLEN];	/* Platform name.		  */

static int	Levels = 1;		/* Number of levels to ask for.	  */
static float	LevelSpacing = 1.0;	/* Spacing in km between levels.  */
	
static float	FirstLevel = 0.0;	/* First level for data.	  */
static int	NumFields = 0;		/* Number of fields.		  */
static ZebTime	LastDataTime;		/* Time we last got data.	  */

static float	SiteLat = -999.0;	/* Site lat in degrees		*/
static float	SiteLon = -999.0;	/* Site lon in degrees		*/



main (argc, argv)
int	argc;
char	**argv;
{
	char	loadfile[200], string[40];
	SValue	v;
/*
 * Check arguments.
 */
	if (argc < 2)
	{
		printf ("Usage: ds_ingest platform [parameter file]\n");
		exit (0);
	}
	strcpy (Platform, argv[1]);
/*
 * Hook into the Zeb world.
 */
	fixdir ("DSI_LOAD_FILE", LIBDIR, "RAP_DsIngest.lf", loadfile);
	if (argc > 2)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[2];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	SetupIndirect ();

	sprintf (string, "%s_ingest", Platform);
	msg_connect (Die, string);
	ds_Initialize ();
/*
 * Init some variables.
 */
	LastDataTime.zt_Sec = 0;
	LastDataTime.zt_MicroSec = 0;
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
		case DSI_GO:
			Go ();
			break;
	/*
	 * Define a field.
	 */
		case DSI_FIELD:
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
 * Start getting some data.
 */
{
/*
 * Sanity checks
 */
	if (NumFields == 0)
	{
		msg_ELog (EF_EMERGENCY, "No fields specified");
		exit (1);
	}

	if (SiteLat < -90 || SiteLat > 90 || SiteLon < -180 || SiteLon > 180)
	{
		msg_ELog (EF_EMERGENCY, "Bad site_lat and/or site_lon");
		exit (1);
	}

	if (Port == 0)
	{
		msg_ELog (EF_EMERGENCY, "Port number not specified");
		exit (1);
	}

	if (Frequency == 0)
	{
		msg_ELog (EF_EMERGENCY, "No request frequency specified");
		exit (1);
	}
/*
 * Get data periodically.
 */
	tl_AddRelativeEvent (request_data, NULL, 0, Frequency * INCFRAC);
/*
 * Wait.
 */
	msg_await ();
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
	if (NumFields >=MAXFIELDS)
	{
		msg_ELog (EF_PROBLEM, "Too many fields.");
		return;
	}
/*
 * Remeber the stuff.
 */
	FieldList[NumFields].fl_name = usy_string (UPTR (*cmds));
	FieldList[NumFields].fl_number = UINT (cmds[1]);
	NumFields++;
}


static void
SetupIndirect ()
/*
 * Set up indirect variables.
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "host", Host, SYMT_STRING, STRLEN);
	usy_c_indirect (vtable, "port", &Port, SYMT_INT, 0);
	usy_c_indirect (vtable, "frequency", &Frequency, SYMT_INT, 0);
	usy_c_indirect (vtable, "levels", &Levels, SYMT_INT, 0);
	usy_c_indirect (vtable, "levelspacing", &LevelSpacing, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "firstlevel", &FirstLevel, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "site_lat", &SiteLat, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "site_lon", &SiteLon, SYMT_FLOAT, 0);
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


static cd_command_t
form_request (field, level)
int	field, level;
/*
 * Form the command request.
 */
{
	ZebTime		zt;
	cd_command_t	com;
/*
 * What type of data to get.
 */
	com.primary_com = GET_INFO | GET_DATA | GET_NEW;
	com.second_com = GET_XY_PLANE;
/*
 * Last time we got data.
 */
	com.time_cent = LastDataTime.zt_Sec;
/*
 * Time.
 */
	tl_Time (&zt);
	com.time_min = zt.zt_Sec; 
	com.time_max = zt.zt_Sec;
/*
 * What area of the grid to get. (Just grab everything within 1000 km of
 * the site.)
 */
	com.lat_origin = SiteLat;
	com.lon_origin = SiteLon;
	com.ht_origin = 0;

	com.min_x = -1000;
	com.max_x = 1000;

	com.min_y = -1000;
	com.max_y = 1000;
/*
 * Vertical level to get.
 */
	com.min_z = (long)(FirstLevel + (level - 0.5) * LevelSpacing);
	com.max_z = (long)(FirstLevel + (level + 0.5) * LevelSpacing);
/*
 * What field to get.
 */
	com.data_field = field;
	com.data_type = CHAR;
/*
 * No additional request data.
 */
	com.add_data_len = 0;

	return (com);
}


static void
request_data ()
/*
 * Request data from the data server.
 */
{
	int		i, j, numz = 0;
	bool		gotsomedata = FALSE;
	cd_command_t	com;
	unsigned char	*buffer[MAXZ];
	cd_grid_info_t	info;
	cd_reply_t	reply[MAXZ];
	DataChunk	*dc;
	FieldId		fieldlist[MAXFIELDS];
/*
 * Get a data chunk set up.
 */
	dc = dc_CreateDC (DCC_RGrid);
	dc->dc_Platform = ds_LookupPlatform (Platform);
/*
 * Set up fields.
 */
	for (i = 0; i < NumFields; i++)
		fieldlist[i] = F_Lookup (FieldList[i].fl_name);

	dc_RGSetup (dc, NumFields, fieldlist);
/*
 * Set the badvalue.
 */
	dc_SetBadval (dc, BADVAL);
/*
 * Request data from each field.
 */
	for (i = 0; i < NumFields; i++)
	{
		numz = 0;
	/*
	 * Get some data.
	 */
		if (Levels > MAXZ) Levels = MAXZ;

		for (j = 0; j < Levels; j++)
		{
			com = form_request (FieldList[i].fl_number, j);
			buffer[numz] = NULL;
			buffer[numz] = get_cidd_data (&com, &reply[numz], 
				&info, Host, Port);
		/*
		 * Print out the reply.
		 */
# ifdef notdef
			print_reply (&reply[numz], stdout);
# endif
		/*
		 * And what is the status of the reply.
		 */
			if (reply[numz].status & REQUEST_SATISFIED)
			{
				msg_ELog (EF_DEBUG, "Data received for %s",
					info.field_name);
			}
			else if (reply[numz].status & NO_DATA)
			{
				msg_ELog (EF_PROBLEM, "No data available.");
				continue;
			}
			else if (reply[numz].status & VOLUME_LIMITS)
			{
				msg_ELog (EF_PROBLEM, 
					"No data in desired volume.");
				continue;
			}
			else if (reply[numz].status & TIME_LIMITS)
			{
				msg_ELog (EF_PROBLEM, "No data in time frame.");
				continue;
			}
		/*
		 * Print out the info we got.
		 */
# ifdef notdef
			print_info (&info, stdout);
# endif
		/*
 		 * See if we got any data.
		 */
			if (buffer[numz] != NULL)
			{
				numz++; 
				gotsomedata = TRUE;
			}
		}
	/*
	 * Add this grid to the data chunk.
	 */
		if (numz > 0)
			add_grid (dc, fieldlist[i], buffer, &info, reply, 
				  numz);
		for (j = 0; j < numz; j++)
			if (buffer[j])
				free (buffer[j]);
	}
/*
 * Store the data.
 */
	if (gotsomedata) ds_Store (dc, FALSE, NULL, 0);
/*
 * Free the data chunk.
 */
	dc_DestroyDC (dc);
}


static void
add_grid (dc, fid, buffer, info, reply, numz)
DataChunk	*dc;
FieldId		fid;
unsigned char	*buffer[MAXZ];
cd_grid_info_t	*info;
cd_reply_t	reply[MAXZ];
int		numz;
/*
 * Add a grid to a data chunk.
 */
{
	int		i, j, len, gridsize, index;
	int		x, y;
	Location	origin;
	RGrid		rg;
	ZebTime		when;
	float		*grid;
/*
 * Get the info we need.
 */
/*
 * Origin.
 */
	cvt_Origin (info->lat_origin, info->lon_origin);
	cvt_ToLatLon (info->min_x, info->min_y, &origin.l_lat, &origin.l_lon);
	origin.l_alt = FirstLevel; /* km */
/*
 * Grid spacing.
 */
	rg.rg_Xspacing = reply[0].dx;
	rg.rg_Yspacing = reply[0].dy;
	rg.rg_Zspacing = LevelSpacing;
	rg.rg_nX = reply[0].nx;
	rg.rg_nY = reply[0].ny;
	rg.rg_nZ = numz;
/*
 * Time.
 */
	LastDataTime.zt_Sec = reply[0].time_cent;
	LastDataTime.zt_MicroSec = 0;
	when.zt_Sec = reply[0].time_end;
	when.zt_MicroSec = 0;
/*
 * Data length.
 */
	len = rg.rg_nX * rg.rg_nY * rg.rg_nZ; 
/*
 * Convert the data from unsigned char to float.
 */
	if ((grid = (float *) malloc (len * sizeof (float)))	== NULL) 
	{
		msg_ELog (EF_PROBLEM, "Can't allocate grid.");
		return;
	}
/*
 * Loop through all the levels.
 */
	for (i = 0; i < numz; i++)
	{
	    gridsize = rg.rg_nX * rg.rg_nY;
	    index = i * gridsize;
	    for (j = 0; j < gridsize; j++)
	    {
		if (info[i].order == RIGHT)
		{
		    if ((unsigned char) buffer[i][j] == reply[i].bad_data_val)
			grid[index + j] = BADVAL;
		    else
			grid[index + j] = 
			    ((unsigned int) buffer[i][j]) * reply[i].scale + 
				reply[i].bias;
		}
		else
		{
		    y = j / rg.rg_nX;
		    if (y == 0) x = j;
		    else x = j % (y * rg.rg_nX);
		    if ((unsigned char) buffer[i][j] == reply[i].bad_data_val)
			grid[index + x + (rg.rg_nY-y-1) * rg.rg_nY] = BADVAL;
		    else
			grid[index + x + (rg.rg_nY-y-1) * rg.rg_nY] = 
			    ((unsigned int) buffer[i][j]) * reply[i].scale + 
				reply[i].bias;
		}
	    }
	}
/*
 * Put the grid in the data chunk.
 */
	dc_RGAddGrid (dc, 0, fid, &origin, &rg, &when, grid, 0);	
/*
 * Free memory.
 */
	free (grid);
}

