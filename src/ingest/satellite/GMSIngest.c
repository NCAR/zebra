/*
 * Consume GMS images into the data store.
 */
/*		Copyright (C) 1992 by UCAR
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

# include <copyright.h>
# include <unistd.h>
# include <errno.h>
# include <math.h>
# include <stdio.h>
# include <dirent.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>
# include <DataChunk.h>

MAKE_RCSID("$Id: GMSIngest.c,v 1.3 1994-06-29 21:28:38 case Exp $")

# include "keywords.h"


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
int 	HaveLimits = FALSE;

/*
 * Image unpacking info
 */
int	Nbytes, Prefixlen, Linelen, Xres, Yres;

/*
 * Lookup table for decoding GMS IR values.
 */
float GMS_table[256] = {
	349.438, 345.613, 342.456, 339.854, 337.711, 335.945, 334.488, 333.280,
	332.275, 331.430, 330.713, 330.096, 329.556, 329.075, 328.637, 328.232,
	327.850, 327.484, 327.127, 326.777, 326.430, 326.084, 325.737, 325.388,
	325.038, 324.685, 324.331, 323.973, 323.614, 323.254, 322.891, 322.528,
	322.164, 321.799, 321.433, 321.067, 320.700, 320.333, 319.966, 319.598,
	319.230, 318.861, 318.492, 318.122, 317.751, 317.380, 317.007, 316.633,
	316.258, 315.882, 315.504, 315.125, 314.745, 314.363, 313.980, 313.596,
	313.210, 312.823, 312.434, 312.045, 311.653, 311.261, 310.868, 310.473,
	310.077, 309.681, 309.283, 308.884, 308.484, 308.084, 307.682, 307.279,
	306.876, 306.472, 306.066, 305.660, 305.253, 304.844, 304.435, 304.024,
	303.613, 303.200, 302.786, 302.371, 301.954, 301.536, 301.116, 300.696,
	300.273, 299.849, 299.424, 298.997, 298.568, 298.138, 297.706, 297.272,
	296.836, 296.399, 295.960, 295.520, 295.077, 294.633, 294.187, 293.740,
	293.291, 292.840, 292.387, 291.933, 291.477, 291.019, 290.560, 290.099,
	289.636, 289.172, 288.706, 288.239, 287.769, 287.298, 286.826, 286.351,
	285.875, 285.397, 284.917, 284.435, 283.951, 283.465, 282.978, 282.488,
	281.996, 281.502, 281.005, 280.507, 280.006, 279.502, 278.997, 278.488,
	277.977, 277.464, 276.948, 276.429, 275.907, 275.383, 274.855, 274.325,
	273.792, 273.256, 272.717, 272.175, 271.630, 271.082, 270.531, 269.977,
	269.419, 268.859, 268.295, 267.728, 267.157, 266.583, 266.006, 265.426,
	264.842, 264.255, 263.664, 263.069, 262.471, 261.869, 261.264, 260.654,
	260.041, 259.423, 258.801, 258.175, 257.544, 256.909, 256.269, 255.625,
	254.975, 254.321, 253.661, 252.996, 252.325, 251.649, 250.967, 250.279,
	249.585, 248.885, 248.178, 247.464, 246.744, 246.017, 245.283, 244.542,
	243.749, 243.037, 242.274, 241.502, 240.722, 239.934, 239.138, 238.333,
	237.519, 236.696, 235.863, 235.021, 234.169, 233.307, 232.434, 231.549,
	230.654, 229.746, 228.826, 227.893, 226.946, 225.985, 225.009, 224.017,
	223.009, 221.984, 220.941, 219.878, 218.796, 217.692, 216.567, 215.418,
	214.245, 213.046, 211.820, 210.565, 209.280, 207.962, 206.611, 205.223,
	203.796, 202.327, 200.813, 199.251, 197.634, 195.959, 194.219, 192.405,
	190.511, 188.524, 186.433, 184.222, 181.874, 179.367, 176.678, 173.776,
	170.626, 167.188, 163.413, 159.246, 154.621, 149.460, 143.676, 137.163
};
unsigned char IRLookupTable[256];

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
# define MAXFILES 10
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
static void	TimeCheck FP ((ZebTime *));
static void	swapfour FP ((int *, int));
static void	FileLimits FP ((void));
static int	MDispatcher FP ((struct message *));
static void *	DoFile FP ((int));
static void	GetFileTime FP ((int, ZebTime *));
static void	Die FP ((void));
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
	char	loadfile[200];
/*
 * Connect to the message handler
 */
	msg_connect (MDispatcher, "GMSIngest");
	msg_DeathHandler (Die);
/*
 * UI stuff
 */
	fixdir ("SI_LOAD_FILE", LIBDIR, "SatIngest.lf", loadfile);

	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup ("GMSIngest", &argc, argv, 0);
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
/*
 * Get on with it
 */
	ui_get_command ("initial", "GMSIngest>", Dispatcher, 0);
	ui_finish ();
	exit (0);
}




static void
Die ()
/*
 * Uh-oh.  Get out now.
 */
{
	ui_finish ();
	exit (1);
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
 * Add the file to the list
 */
	Infile[Nfiles].name = (char *) malloc (strlen (fname) + 1);
	strcpy (Infile[Nfiles].name, fname);

	Infile[Nfiles].field = (char *) malloc (strlen (fld) + 1);
	strcpy (Infile[Nfiles].field, fld);
/*
 * Open it
 */
	if ((Infile[Nfiles].stream = fopen (fname, "r")) != NULL)
		Nfiles++;
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
	int	f, nfields = 0, ngood = 0, i;
	void	*grid;
	Location	loc;
	RGrid		rg;
	FieldId		fid[MAXFILES];
	ScaleInfo	scale[MAXFILES];
	DataChunk	*dc;
	ZebTime		t;
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
	}
	else if (KmResolution != 0.0)
	{
		Latstep = Lonstep = KM_TO_DEG (KmResolution);

		GridX = (int)((Maxlon - Minlon) / Lonstep) + 1;
		GridY = (int)((Maxlat - Minlat) / Latstep) + 1;

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
 * Check data times in the files and leave only the latest one(s) for ingest
 */
	TimeCheck (&t);
/*
 * Build a field list
 */
	nfields = Nfiles;

	F_Init ();
	for (f = 0; f < nfields; f++)
	{
		fid[f] = F_DeclareField (Infile[f].field, "", "");
	/*
	 * Throw together the IR calibration.
	 */
		scale[f].s_Scale = 1.0;
		scale[f].s_Offset = GMS_table[255];
		for (i = 0; i < 255; i++)
		{
			int ival = GMS_table[i] - scale[f].s_Offset;
			IRLookupTable[i] = (unsigned char) ival;
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
	dc = dc_CreateDC (DCC_Image);
	dc->dc_Platform = Plat;
	dc_ImgSetup (dc, nfields, fid, scale);
/*
 * Build and insert the grids
 */
	for (f = 0; f < Nfiles; f++)
	{
		if ((grid = DoFile (f)) == NULL)
			continue;

		ngood++;
		dc_ImgAddImage (dc, 0, fid[f], &loc, &rg, &t, grid, 
			GridX * GridY);
		free(grid); 		/* we're done with it */
	}
/*
 * Write out the data chunk.  Finally.
 */
	if (ngood > 0)
	{
		ds_Store (dc, 1, NULL, 0);
		msg_ELog (EF_INFO, "Successfully ingested %d of %d images",
			ngood, Nfiles);
	}
	else
		msg_ELog (EF_INFO, "Exiting with nothing ingested");
/*
 * Free the stuff we allocated
 */
	dc_DestroyDC (dc);
	for (f = 0; f < Nfiles; f++)
	{
		free (Infile[f].name);
		free (Infile[f].field);
	}
}
	




static void
TimeCheck (t)
ZebTime *t;
/*
 * Check data times in the files and leave only the latest one(s) for ingest
 */
{
	ZebTime	ftime;
	int	f, prev, ngood = 0;
/*
 * Run through the file list, leaving only one(s) with the latest time
 */
	t->zt_Sec = t->zt_MicroSec = 0;

	for (f = 0; f < Nfiles; f++)
	{
		GetFileTime (f, &ftime);
		if (ftime.zt_Sec < t->zt_Sec)
		{
		/*
		 * Remove files with older times from the list
		 */
			msg_ELog (EF_INFO, 
				"Not ingesting %s due to time mismatch",
				Infile[f].name);

			free (Infile[f].name);
			free (Infile[f].field);
			fclose (Infile[f].stream);

			continue;
		}
		else if (ftime.zt_Sec > t->zt_Sec)
		{
		/*
		 * Copy to the beginning of the list if we hit a later time
		 */
			Infile[0] = Infile[f];
			ngood = 0;
			*t = ftime;
		}
	/*
	 * Increment the good count and clean out the file entry if we
	 * have moved it
	 */
		if (f != ngood++)
		{
			free (Infile[f].name);
			free (Infile[f].field);
		}
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
	int	header[64], nav_cod[128];
	unsigned char	*grid;
	int	i, j, line, elem, status, stuff[128], one = 1;
	int	imagelen, ngot;
	float	dummy, fline, felem, lat, lon;
	char	source[5], *c;

	msg_ELog (EF_INFO, "Reading %s", Infile[fentry].name);
/*
 * Read the 256 byte "area directory" header and the 512 byte
 * navigation codicil and swap bytes around in each one.  
 * NOTE: We don't swap in those portions which contain text
 */
	fread ((void *) header, 4, 64, Infile[fentry].stream);
	swapfour (header, 20);
	swapfour (header + 32, 19);

	fread ((void *) nav_cod, 4, 128, Infile[fentry].stream);
	swapfour (nav_cod + 1, 39);
/*
 * Verify that this is a GOES image
 */
	if (strncmp (nav_cod, "GOES", 4))
	{
		char	imtype[4];

		strncpy (imtype, nav_cod, 4);
		imtype[4] = '\0';

		msg_ELog (EF_PROBLEM, "'%s' contains a '%s' image, not GOES",
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
		msg_ELog (EF_EMERGENCY, "Can't deal with %d byte GOES data",
			Nbytes);
		return (NULL);
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
 * 512 byte extra header for "aaa" areas (we ignore it for now)
 */
	if (! strcmp (source, "aaa"))
		fread ((void *) stuff, 4, 128, Infile[fentry].stream);
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
 * less incidental.)
 */
	for (j = 0; j < GridY; j++)
	{
		if (! ((j+1) % 20))
			msg_ELog (EF_DEBUG, "%s: line %d of %d, lat %.2f", 
				Infile[fentry].name, j + 1, GridY, lat);

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
 * Read the first piece of the area directory and do the appropriate byte
 * swapping.
 */
	fread ((void *) header, 4, 5, Infile[fentry].stream);
	fseek (Infile[fentry].stream, 0, 0);	/* rewind the file */
	swapfour (header, 5);
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
swapfour (array, count)
int	*array, count;
/*
 * Swap byte order (0123 -> 3210) for 'count' longwords in 'array'
 */
{
	int	i;
	char	*bytes, swapped[4];

	for (i = 0; i < count; i++)
	{
		bytes = (char *) &(array[i]);
		swapped[0] = bytes[3];
		swapped[1] = bytes[2];
		swapped[2] = bytes[1];
		swapped[3] = bytes[0];
		memcpy (bytes, swapped, 4);
	}
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
	 * Find the offset into the image
	 */
		pos = image_y * Linelen + Prefixlen + image_x * Nbytes;
	/*
	 * Unpack the appropriate number of bytes
	 */
		switch (Nbytes)
		{
		    case 1:
			return (IRLookupTable[Image[pos]]);
			break;
		    default:
			msg_ELog (EF_EMERGENCY, 
				"Exiting.  Cannot handle %d byte values.", 
				Nbytes);
			exit (1);
		}
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
