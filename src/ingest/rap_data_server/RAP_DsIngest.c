/*
 * Generic data ingest from RAP's data servers.
 */
static char *rcsid = "$Id: RAP_DsIngest.c,v 1.1 1992-07-01 21:03:54 kris Exp $";
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
static void		store_data FP ((unsigned char *[][], 
				cd_grid_info_t [][], cd_reply_t [][], int));

/*
 * Other routines.
 */
extern unsigned char	*get_data ();
extern int 		print_reply ();
extern int 		print_info ();

/*
 * Global stuff.
 */
static char	Host[STRLEN];		/* Host name where data server is */
					/*	running.		  */ 
static int	Port;			/* Port number of data.		  */
static int	Frequency;		/* Frequency of data requests in  */
					/*	seconds.		  */
static char	Platform[STRLEN];	/* Platform name.		  */
static int	Levels;			/* Number of levels to ask for.	  */	
static float	LevelSpacing;		/* Spacing in km between levels.  */	
static float	FirstLevel;		/* First level for data.	  */
static int	NumFields = 0;		/* Number of fields.		  */


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
	fixdir ("DSI_LOAD_FILE", LIBDIR, "ds_ingest.lf", loadfile);
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
	usy_c_indirect (vtable, "platform", Platform, SYMT_STRING, STRLEN);
	usy_c_indirect (vtable, "levels", &Levels, SYMT_INT, 0);
	usy_c_indirect (vtable, "levelspacing", &LevelSpacing, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "firstlevel", &FirstLevel, SYMT_FLOAT, 0);
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
	com.primary_com = GET_INFO | GET_DATA | GET_MOST_RECENT;
	com.second_com = GET_XY_PLANE;
/*
 * What area of the grid to get.
 */	
	com.min_x = -1000.0;
	com.max_x = 1000.0;
	com.min_y = -1000.0;
	com.max_y = 1000.0;

	com.min_z = FirstLevel + level * LevelSpacing - .5 * LevelSpacing;
	com.max_z = FirstLevel + level * LevelSpacing + .5 * LevelSpacing;
/*
 * What field to get.
 */
	com.data_field = field;
	com.data_type = CHAR;
/*
 * Time.
 */
	tl_Time (&zt);
	com.time_min = zt.zt_Sec; 
	com.time_max = zt.zt_Sec;

	return (com);
}


static void
request_data ()
/*
 * Request data from the data server.
 */
{
	int		i, j, numz = 0;
	cd_command_t	com;
	unsigned char	*buffer[MAXFIELDS][MAXZ];
	cd_grid_info_t	info[MAXFIELDS][MAXZ];
	cd_reply_t	reply[MAXFIELDS][MAXZ];
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
			buffer[i][numz] = NULL;
			buffer[i][numz] = get_data (&com, &reply[i][numz], 
				&info[i][numz], Host, Port);
		/*
		 * Print out the reply.
		 */
# ifdef notdef
			print_reply (&reply[i][numz], stdout);
# endif
		/*
		 * And what is the status of the reply.
		 */
			if (reply[i][numz].status & REQUEST_SATISFIED)
			{
				msg_ELog (EF_DEBUG, "Data received for %s",
					info[i][numz].field_name);
			}
			else if (reply[i][numz].status & NO_DATA)
			{
				msg_ELog (EF_PROBLEM, "No data available.");
				continue;
			}
			else if (reply[i][numz].status & VOLUME_LIMITS)
			{
				msg_ELog (EF_PROBLEM, 
					"No data in desired volume.");
				continue;
			}
			else if (reply[i][numz].status & TIME_LIMITS)
			{
				msg_ELog (EF_PROBLEM, "No data in time frame.");
				continue;
			}
		/*
		 * Print out the info we got.
		 */
# ifdef notdef
			print_info (&info[i][numz], stdout);
# endif
		/*
 		 * See if we got any data.
		 */
			if (buffer[i][numz] != NULL) numz++;
		}
	}
/*
 * Store the data.
 */
	if (numz > 0)
		store_data (buffer, info, reply, numz);
/*
 * Free memory.
 */
	for (i = 0; i < NumFields; i++)
		for (j = numz; j < 0; j++)
			if (buffer[i][j])
				free (buffer[i][j]);
}


static void
store_data (buffer, info, reply, numz)
unsigned char	*buffer[MAXFIELDS][MAXZ];
cd_grid_info_t	info[MAXFIELDS][MAXZ];
cd_reply_t	reply[MAXFIELDS][MAXZ];
int		numz;
/*
 * Create a data chunk out of this stuff.
 */
{
	int		i, j, k, len, gridsize;
	DataChunk	*dc;
	FieldId		fieldlist[MAXFIELDS];
	Location	origin;
	RGrid		rg;
	ZebTime		when;
	float		*grid[MAXFIELDS];
/*
 * Create the data chunk.
 */
	dc = dc_CreateDC (DCC_RGrid);
	dc->dc_Platform = ds_LookupPlatform (Platform);
/*
 * Set up the fields.
 */
	for (i = 0; i < NumFields; i++)
		fieldlist[i] = F_Lookup (FieldList[i].fl_name);

	dc_RGSetup (dc, NumFields, fieldlist); 
/*
 * Set the bad data value.
 */
	dc_SetBadval (dc, BADVAL);
/*
 * Add the grids.
 */
	for (i = 0; i < NumFields; i++)
	{
	/*
	 * Get the info we need.
	 */
	/*
	 * Origin.
	 */
		cvt_Origin (info[i][0].lat_origin, info[i][0].lon_origin);
		cvt_ToLatLon (info[i][0].min_x, info[i][0].min_y, 
			&origin.l_lat, &origin.l_lon);
		origin.l_alt = info[i][0].min_z;	/* in km */
	/*
	 * Grid spacing.
	 */
		rg.rg_Xspacing = reply[i][0].dx;
		rg.rg_Yspacing = reply[i][0].dy;
		rg.rg_Zspacing = reply[i][0].dz;
		rg.rg_nX = reply[i][0].nx;
		rg.rg_nY = reply[i][0].ny;
		rg.rg_nZ = numz;
	/*
	 * Time.
	 */
		when.zt_Sec = reply[i][0].time_end;
		when.zt_MicroSec = 0;
	/*
	 * Data length.
	 */
		len = rg.rg_nX * rg.rg_nY * rg.rg_nZ; 
	/*
	 * Convert the data from unsigned char to float.
	 */
		if ((grid[i] = (float *) malloc (len * sizeof (float)))					 == NULL) {
			msg_ELog (EF_PROBLEM, "Can't allocate grid.");
			return;
		}
	/*
	 * Loop through all the levels.
	 */
		for (j = 0; j < numz; j ++)
		{
		    gridsize = rg.rg_nX * rg.rg_nY;
		    for (k = 0; k < gridsize; k++)
		    {
			if ((unsigned char) buffer[i][j][k] == 
			    reply[i][j].bad_data_val)
				grid[i][j * gridsize + k] = BADVAL;
			else
				grid[i][j * gridsize + k] = 
				    ((unsigned int) buffer[i][j][k]) * 
				    reply[i][j].scale + reply[i][j].bias;
		    }
		}
	/*
	 * Put the grid in the data chunk.
	 */
		dc_RGAddGrid (dc, 0, fieldlist[i], &origin, &rg, &when,
			grid[i], 0);	
	}
/*
 * Send the data chunk to the data store.
 */
	ds_Store (dc, FALSE, NULL, 0);
/*
 * Free the data chunk.
 */
	dc_DestroyDC (dc);
/*
 * Free memory.
 */
	for (i = 0; i < NumFields; i++)
		free (grid[i]);
}

