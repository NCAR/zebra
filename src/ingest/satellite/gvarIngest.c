/*
 * Consume McIDAS-X GVAR area files into the data store.
 */
/*		Copyright (C) 1995 by UCAR
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

# include <unistd.h>
# include <errno.h>
# include <math.h>
# include <stdio.h>
# include <dirent.h>

# include <ui.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>
# include <DataChunk.h>

RCSID("$Id: gvarIngest.c,v 1.2 1997-01-21 17:42:26 granger Exp $")

# include "keywords.h"

/*
 * Define a few symbols needed by some McIDAS modules
 */
long	*uc = NULL, *neguc = NULL, *ttyfd = NULL;


/*
 * Our platform
 */
# define PF_LEN 80
char		Platname[PF_LEN] = "";
PlatformId	Plat;

/*
 * The image and its size
 */
unsigned char	*Image;
int	Nx, Ny;

/*
 * Grid information
 */
int	GridX = 0, GridY = 0;
float	KmResolution = 0.0;
float	Minlon, Maxlon, Minlat, Maxlat;
float	Latstep, Lonstep;

bool 	HaveLimits = FALSE;
bool	CheckTimes = TRUE;

/*
 * Image unpacking info.
 */
int	Nbytes, Prefixlen, Linelen, Xres, Yres;

/*
 * If Truncate is true, then the low order byte(s) of multi-byte data are 
 * truncated to give us one byte data.  Otherwise, we insist that we get
 * one byte data to start with.
 */
bool	Truncate = FALSE;

/*
 * Image limits in line/elem coordinates
 */
int	Minelem, Maxelem, Minline, Maxline;

/*
 * Origin latitude to use
 */
float	OriginLat = -999.0;

/*
 * Structure describing a file to ingest
 */
# define MAXFILES 24
struct _InFile
{
	char	*name;
	char	*field;
	FILE	*stream;
} Infile[MAXFILES];

int Nfiles = 0;

/*
 * Useful stuff
 */
# define BETWEEN(x,lower,upper)	(((x)-(lower))*((x)-(upper)) <= 0)
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */

int	Mdays[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/*
 * Prototypes
 */
static int	Dispatcher FP ((int, struct ui_command *));
static void	UserLimits FP ((struct ui_command *));
static void	AddFile FP ((struct ui_command *));
static void	Ingest FP ((void));
static void	RemoveOldFile FP ((int f));
static void	RemoveFile FP ((int f));
static void	TimeCheck FP ((ZebTime *));
static void	FileLimits FP ((void));
static int	MDispatcher FP ((struct message *));
static void *	DoFile FP ((int));
static void	GetFileTime FP ((int, ZebTime *));
static int	Die FP ((void));
static inline unsigned char	imageval FP ((int, int));




main (argc, argv)
int argc;
char **argv;
/*
 * Ingest a GOES visible and/or IR image
 */
{
	SValue	v;
	stbl	vtable;
	char	loadfile[200], ourname[50];
/*
 * Connect to the message handler
 */
	sprintf (ourname, "gvarIn_%04x", getpid ());
	msg_connect (MDispatcher, ourname);
	msg_DeathHandler (Die);
/*
 * UI stuff
 */
	fixdir ("SI_LOAD_FILE", GetLibDir(), "gvarIngest.lf", loadfile);

	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup ("gvarIngest", &argc, argv, 0);
/*
 * Initialization
 */
	ds_Initialize ();

	vtable = usy_g_stbl ("ui$variable_table");
	usy_c_indirect (vtable, "originLat", &OriginLat, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "kmResolution", &KmResolution, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "platform", Platname, SYMT_STRING, PF_LEN);
	usy_c_indirect (vtable, "gridX", &GridX, SYMT_INT, 0);
	usy_c_indirect (vtable, "gridY", &GridY, SYMT_INT, 0);
	usy_c_indirect (vtable, "truncate", &Truncate, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "checkTimes", &CheckTimes, SYMT_BOOL, 0);
/*
 * Get on with it
 */
	ui_get_command ("initial", "gvarIngest>", Dispatcher, 0);
	ui_finish ();
	exit (0);
}




static int
Die ()
/*
 * Uh-oh.  Get out now.
 */
{
	ui_finish ();
	exit (1);
	return (0);
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
	 * Time to actually do things.  Ingest() is responsible for
	 * resetting the file list to zero in case we're told to 'go'
	 * more than once.
	 */
	    case KW_GO:
		Ingest ();
		break;
	/*
	 * lat/lon limits
	 */
	    case KW_LIMITS:
		UserLimits (cmds + 1);
		break;
	/*
	 * File
	 */
	    case KW_FILE:
		AddFile (cmds + 1);
		break;
	/*
	 * Unknown command
	 */
	    default:
		msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
		break;
	}
	return (TRUE);
}




static void
UserLimits (cmds)
struct ui_command	*cmds;
/*
 * Get user specified lat/lon limits for the grid
 * ...and do some sanity checking...
 */
{
	Minlat = UFLOAT (cmds[0]);
	Minlon = UFLOAT (cmds[1]);
	Maxlat = UFLOAT (cmds[2]);
	Maxlon = UFLOAT (cmds[3]);
	if ((Minlat < Maxlat) && (Minlon < Maxlon))
	{
		/* values are valid, note as much */
		HaveLimits = TRUE;
	}
	else
	{
		/* illegal values: tell user */
		msg_ELog(EF_PROBLEM,"limits values illegal: %f %f %f %f",
			Minlat,Minlon,Maxlat,Maxlon);
		HaveLimits = FALSE;
	}
}




static void
AddFile (cmds)
struct ui_command	*cmds;
/*
 * Add a file to be ingested
 */
{
	char	*fname = UPTR (cmds[0]), *fld = UPTR (cmds[1]);
/*
 * Sanity check
 */
	if (Nfiles == MAXFILES) 
	{
		msg_ELog (EF_PROBLEM, "Only %d files can be ingested", 
			MAXFILES);
		return;
	}
/*
 * Make sure the file can be opened first, then add it
 */
	if ((Infile[Nfiles].stream = fopen (fname, "r")) != NULL)
	{
		Infile[Nfiles].name = (char *) malloc (strlen (fname) + 1);
		strcpy (Infile[Nfiles].name, fname);
		Infile[Nfiles].field = (char *) malloc (strlen (fld) + 1);
		strcpy (Infile[Nfiles].field, fld);
		Nfiles++;
	}
	else
		msg_ELog (EF_PROBLEM, "Error %d opening file '%s'", errno, 
			  fname);
}



static void
Ingest ()
/*
 * Begin the ingest process
 */
{
	int		f, i;
	int		nfields, ngood;
	void		*grid;
	Location	loc;
	RGrid		rg;
	FieldId		fid[MAXFILES];
	ScaleInfo	scale[MAXFILES];
	DataChunk	*dc;
	ZebTime		t;
	char		buf[128];
	bool		reset_gridsize;
/*
 * Make sure we have lat/lon limits and a platform name
 */
	if (! HaveLimits)
	{
		msg_ELog (EF_PROBLEM, "Lat/lon limits must be specified!");
		Die ();
	}

	if (! Platname[0])
	{
		msg_ELog (EF_PROBLEM, "No platform specified");
		Die ();
	}
/*
 * Figure out grid spacing
 */
	if (GridX && GridY)
	{
		if (KmResolution != 0.0)
			msg_ELog (EF_INFO, 
				"Gridsize overrides kmResolution setting");

		Latstep = (Maxlat - Minlat) / (GridY - 1);
		Lonstep = (Maxlon - Minlon) / (GridX - 1);
		reset_gridsize = FALSE;
	}
	else if (KmResolution != 0.0)
	{
		Latstep = Lonstep = KM_TO_DEG (KmResolution);

		GridX = (int)((Maxlon - Minlon) / Lonstep) + 1;
		GridY = (int)((Maxlat - Minlat) / Latstep) + 1;
	/*
	 * Reset grid sizes to zero when finished, since they were zero
	 * when we entered.
	 */
		reset_gridsize = TRUE;

		Maxlon = Minlon + Lonstep * (GridX - 1);
		Maxlat = Minlat + Latstep * (GridY - 1);
	}
	else
	{
		msg_ELog (EF_PROBLEM, 
			"gridX and gridY or kmResolution must be given");
		Die ();
	}

	msg_ELog (EF_INFO, "Lat. limits: %.2f to %.2f every %.2f",
		Minlat, Maxlat, Latstep);
	msg_ELog (EF_INFO, "Lon. limits: %.2f to %.2f every %.2f",
		Minlon, Maxlon, Lonstep);
/*
 * Build the location and rgrid information
 */
	loc.l_lat = Minlat;
	loc.l_lon = Minlon;
	loc.l_alt = 0.000;

	rg.rg_Xspacing = DEG_TO_KM (Lonstep) * cos (DEG_TO_RAD(OriginLat));
	rg.rg_Yspacing = DEG_TO_KM (Latstep);
	rg.rg_Zspacing = 0.0;

	rg.rg_nX = GridX;
	rg.rg_nY = GridY;
	rg.rg_nZ = 1;
/*
 * Check data times in the files and leave only the latest one(s) for ingest.
 */
	if (CheckTimes)
		TimeCheck (&t);
/*
 * Build a field list from all of the unique field names.
 */
	nfields = 0;
	for (f = 0; f < Nfiles; f++)
	{
		fid[nfields] = F_DeclareField (Infile[f].field, "", "");
	/*
	 * Don't add any fields which we already have
	 */
		for (i = 0; i < nfields; ++i)
			if (fid[nfields] == fid[i])
				break;
		if (i >= nfields)
		{
			scale[nfields].s_Scale = 1.0;
			scale[nfields].s_Offset = 0.0;
			++nfields;
		}
	}
/*
 * Get our platform
 */
	if ((Plat = ds_LookupPlatform (Platname)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", Platname);
		Die ();
	}
/*
 * Create and initialize a data chunk
 */
	dc = NULL;
	dc = dc_CreateDC (DCC_Image);
	dc->dc_Platform = Plat;
	dc_ImgSetup (dc, nfields, fid, scale);
/*
 * Build and insert the grids
 */
	ngood = 0;
	for (f = 0; f < Nfiles; f++)
	{
	/*
	 * If we're not checking times, then we need to know the time
	 * of this file.
	 */
		if (! CheckTimes)
			GetFileTime (f, &t);
		TC_EncodeTime (&t, TC_Full, buf);
		msg_ELog (EF_INFO, "Reading %s, field %s, %s", 
			  Infile[f].name, Infile[f].field, buf);
	/*
	 * We spend most of the time in DoFile(), so we'll poll
	 * for messages there instead of here
	 */
		if ((grid = DoFile (f)) == NULL)
			continue;
	/*
	 * If we already have a sample at this time, use it.  If we
	 * checked the times above, then the sample will always be 0.
	 */
		for (i = 0; i < dc_GetNSample(dc); ++i)
		{
			ZebTime stime;

			dc_GetTime (dc, i, &stime);
			if (TC_Eq(stime,t))
				break;
		}
		ngood++;
		dc_ImgAddImage (dc, i, F_Lookup(Infile[f].field), 
				&loc, &rg, &t, grid, GridX * GridY);
		free(grid); 		/* we're done with it */
	/*
	 * Free files as we finish with them.
	 */
		RemoveFile (f);
	}
/*
 * Write out the data chunk.  Finally.
 */
	if (ngood > 0)
	{
		ds_Store (dc, TRUE, NULL, 0);
		msg_ELog (EF_INFO, "Successfully ingested %d of %d images",
			ngood, Nfiles);
	}
	else
		msg_ELog (EF_INFO, "Exiting with nothing ingested");
/*
 * Destroy the DataChunk, and reset our file list and possibly grid sizes
 */
	dc_DestroyDC (dc);
	Nfiles = 0;
	if (reset_gridsize)
	{
		GridX = 0;
		GridY = 0;
	}
}
	


static void
RemoveOldFile(f)
int f;
/*
 * Remove files with older times from the list
 */
{
	msg_ELog (EF_INFO, 
		  "Not ingesting %s due to time mismatch",
		  Infile[f].name);
	RemoveFile (f);
}



static void
RemoveFile(f)
int f;
{
	free (Infile[f].name);
	free (Infile[f].field);
	if (Infile[f].stream)
		fclose (Infile[f].stream);
	Infile[f].name = NULL;
	Infile[f].field = NULL;
	Infile[f].stream = NULL;
}



static void
TimeCheck (t)
ZebTime *t;
/*
 * Check data times in the files and leave only the latest one(s) for ingest
 */
{
	ZebTime	ftime;
	int	f, prev, i;
	int 	ngood;
/*
 * Run through the file list, leaving only one(s) with the latest time
 */
	t->zt_Sec = t->zt_MicroSec = 0;

	ngood = 0;
	for (f = 0; f < Nfiles; f++)
	{
		GetFileTime (f, &ftime);
		if (ftime.zt_Sec < t->zt_Sec)
		{
			RemoveOldFile (f);
		}
		else if (ftime.zt_Sec > t->zt_Sec)
		{
		/*
		 * We have a new most recent time, so any files currently
		 * considered good are good no longer; remove them.
		 */
			for (i = 0; i < ngood; ++i)
				RemoveOldFile (i);
		/*
		 * Then copy our new most recent file to the front of the list.
		 * The first time through the loop, nothing changes.
		 */
			Infile[0] = Infile[f];
			ngood = 1;
			*t = ftime;
		}
		else
		{
		/*
		 * Last but not least, this file's time checks out with the
		 * current time, so move it to end of the 'good' list.
		 */
			Infile[ngood++] = Infile[f];
		}
	/*
	 * So either we're back to one good file, or this file's time equals
	 * the current time and our number of good files increased by one
	 */
	}

	Nfiles = ngood;
}




static void *
DoFile (fentry)
int	fentry;
/*
 * Read the fentry'th file, remapping into a grid and returning that
 * grid.  The caller is expected to free the grid when finished with it.
 * NULL is returned on failure.
 */
{
# define MAXNAVCHUNKS 16	
	int	header[64], nav_cod[128 * MAXNAVCHUNKS];
	unsigned char	*grid;
	int	i, j, line, elem, status, stuff[128], one = 1;
	int	imagelen, ngot;
	float	dummy, fline, felem, lat, lon;
	char	source[5], *c;

/*
 * Read the 256 byte "area directory" header and the variable length
 * navigation codicil
 */
	fread ((void *) header, 4, 64, Infile[fentry].stream);

	for (i = 0; i < MAXNAVCHUNKS; i++)
	{
		int	*cur_loc = nav_cod + 128 * i;
		fread ((void *) cur_loc, 4, 128, Infile[fentry].stream);

		if (strncmp ((char *)(cur_loc + 127), "MORE", 4))
			break;

		if (i == MAXNAVCHUNKS - 1)
		{
			msg_ELog (EF_EMERGENCY, 
				  "GVAR navigation codicil too big!");
			exit (1);
		}
	}
/*
 * Verify that this is a GVAR image
 */
	if (strncmp (nav_cod, "GVAR", 4))
	{
		char	imtype[4];

		strncpy (imtype, nav_cod, 4);
		imtype[4] = '\0';

		msg_ELog (EF_PROBLEM, 
			  "'%s' contains a '%s' image, not GVAR",
			  Infile[fentry].name, imtype);
		fclose (Infile[fentry].stream);
		return (NULL);
	}
/*
 * Resolution (# of satellite units per image unit)
 */
	Yres = header[11];
	Xres = header[12];
/*
 * If it isn't one byte data, we can't handle it (for now)
 */
	Nbytes = header[10];
	if (Nbytes != 1)
	{
		if (Truncate)
			msg_ELog (EF_INFO, 
				  "%d byte data will be truncated to one byte",
				  Nbytes);
		else
		{
			msg_ELog (EF_EMERGENCY, 
				  "Can't deal with %d byte GVAR data",
				  Nbytes);
			return (NULL);
		}
		
	}
/*
 * Image size (Nx x Ny), bytes per element and prefix length
 */
	Ny = header[8];
	Nx = header[9];
	Prefixlen = header[14];

	Linelen = Nx * Nbytes + Prefixlen;
/*
 * Source name from header word 51 (convert to lower case and remove spaces)
 */
	strncpy (source, header + 51, 4);
	source[4] = '\0';

	for (i = 0; i < 4; i++)
	{
		c = source + i;
		if (*c == ' ')
			*c = '\0';
		else
			*c = tolower (*c);
	}
 	msg_ELog (EF_DEBUG, "Source is '%s'", source);
/*
 * Read the image data
 */
	imagelen = Linelen * Ny;
	Image = (unsigned char *) malloc (imagelen);
	ngot = fread ((void *) Image, 1, imagelen, Infile[fentry].stream);
	if (ngot != imagelen)
	{
		if (feof (Infile[fentry].stream))
			msg_ELog (EF_PROBLEM, 
				"Premature EOF.  Got %d instead of %d bytes",
				ngot, imagelen);
		else
			msg_ELog (EF_PROBLEM, 
				"Read error %d.  Got %d instead of %d bytes",
				errno, ngot, imagelen);
	}
/*
 * We're done with the file
 */
	fclose (Infile[fentry].stream);
/*
 * Element and line limits
 */
	Minline = header[5];
	Maxline = Minline + (Ny - 1) * Yres;

	Minelem = header[6];
	Maxelem = Minelem + (Nx - 1) * Xres;
/*
 * Initialize the navigation stuff
 */
	status = nvxini_ (&one, nav_cod);
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Bad navigation initialization for file '%s'", 
				Infile[fentry].name);
		free (Image);
		return (NULL);
	}
/*
 * Allocate the grid
 */
	grid = (unsigned char *) malloc (GridX * GridY * sizeof (char));
/*
 * Fill in the grid (This is the meat of the program, the rest is more or
 * less incidental.)  (So this is where we'll occasionally check for 
 * messages and get them out of the way.)
 */
  	for (j = 0; j < GridY; j++)
  	{
  		if (! ((j+1) % 20))
 			msg_ELog (EF_DEBUG, "%s: line %d of %d, lat %.2f", 
 				Infile[fentry].name, j + 1, GridY, lat);
		while (j % 25 == 0 && msg_poll(0) != MSG_TIMEOUT);
  
		lat = Maxlat - j * Latstep;

		for (i = 0; i < GridX; i++)
		{
		/*
		 * Translate lat/lon into line and element in the image
		 * (NOTE: nvxeas expects west longitudes to be positive, hence
		 * the sign change)
		 */
			lon = -(Minlon + i * Lonstep);
			status = nvxeas_ (&lat, &lon, &dummy, &fline, &felem,
				&dummy);

			line = (int)(fline + 0.5);
			elem = (int)(felem + 0.5);
		/*
		 * Assign this grid point
		 */
			if (status == 0)
				grid[GridX * j + i] = imageval (line, elem);
			else
				grid[GridX * j + i] = 0;
		}
	}

	free (Image);
	return (grid);
}




static void
GetFileTime (fentry, t)
int	fentry;
ZebTime	*t;
/*
 * Return the time of the fentry'th file
 */
{
	int	year, month, day, hour, minute, second, header[5];
/*
 * Read the first piece of the area directory
 */
	fread ((void *) header, 4, 5, Infile[fentry].stream);
	fseek (Infile[fentry].stream, 0, 0);	/* rewind the file */
/*
 * Extract the date.
 */
	year = header[3] / 1000;
	if ((year % 4) == 0)
		Mdays[2] = 29;	/* February has 29 days in leap years */

	day = header[3] % 1000;
	month = 1;
	while (day > Mdays[month])
		day -= Mdays[month++];
	Mdays[2] = 28;		/* return to 28 days in case next file
				 * is in a different year */
/*
 * Time
 */
	hour = header[4] / 10000;
	minute = (header[4] / 100) % 100;
	second = header[4] % 100;
/*
 * Build a zeb time out of the pieces and we're done
 */
	TC_ZtAssemble (t, year, month, day, hour, minute, second, 0);
	return;
}





static void
FileLimits ()
{
	float	fline, felem, dummy, lat, lon;
	int	status;
/*
 * Find the lat/lon limits based on the corners of the image
 * (NOTE: nvxsae returns W longitudes as positive, so we change the sign)
 */
	Minlat = Minlon = 999.0;
	Maxlat = Maxlon = -999.0;
	status = 0;

	fline = Minline; felem = Minelem;
	status += nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	lon *= -1;
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Minline; felem = Maxelem;
	status += nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	lon *= -1;
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Maxline; felem = Minelem;
	status += nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	lon *= -1;
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Maxline; felem = Maxelem;
	status += nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	lon *= -1;
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;
/*
 * Look for problems.  If the status is < 0 here, it means one
 * or more of the "corners" of the image is off of the globe and the user
 * needs to choose the limits for the remapping explicitly.
 */
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Explicit bounds must be set for this image");
		exit (1);
	}
/*
 * Find the lat/lon steps
 */
	Latstep = (Maxlat - Minlat) / (GridY - 1);
	Lonstep = (Maxlon - Minlon) / (GridX - 1);

	msg_ELog (EF_INFO, "Lat. limits: %.2f to %.2f every %.2f",
		Minlat, Maxlat, Latstep);
	msg_ELog (EF_INFO, "Lon. limits: %.2f to %.2f every %.2f",
		Minlon, Maxlon, Lonstep);

	HaveLimits = TRUE;
}




static inline unsigned char
imageval (line, elem)
int	line, elem;
/*
 * Return the image value associated with satellite line/elem coordinates
 */
{
	int	image_x, image_y, pos;
	unsigned int	val;

	if (BETWEEN(line, Minline, Maxline) && BETWEEN(elem, Minelem, Maxelem))
	{
	/*
	 * Translate from satellite coordinates to image coordinates
	 */
		image_x = (int)((float)(elem - Minelem) / Xres + 0.5);
		image_y = (int)((float)(line - Minline) / Yres + 0.5);
	/*
	 * Find the offset into the image.  We add (Nbytes - 1) so that
	 * we end up pointing to the last byte of multi-byte data, since
	 * that's the MSB.
	 */
		pos = image_y * Linelen + Prefixlen + image_x * Nbytes + 
			(Nbytes - 1);
	/*
	 * Return the byte.
	 */
		return (Image[pos]);
	}
	else
		return ((unsigned char) 0xff);
}




static int
MDispatcher (msg)
struct message *msg;
/*
 * Deal with a message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE)
		{
			ui_finish ();
			exit (1);
		}
		break;
	}
	return (0);
}   	
