/*
 * Translate MUDRAS files to netcdf.
 */
static char *rcsid = "$Id: mudtocdf.c,v 1.5 1995-04-19 14:44:06 granger Exp $";

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
# include <math.h>
# include <netcdf.h>

# include <defs.h>	/* Time and coordinate conversions */

extern char *getenv ();

/*
 * Field info.
 */
# define MAXFLD 10
int Nfield = 0;				/* How many?		*/
char SrcFlds[MAXFLD][40];		/* Mudras field names	*/
char DstFlds[MAXFLD][40];		/* Netcdf field names	*/
int VFields[MAXFLD];			/* Netcdf var ID's	*/
int MFields[MAXFLD];			/* Mudras field numbers	*/
int VBTime;				/* Base time		*/
int VTOff;				/* Time offset		*/
int VLat, VLon, VAlt;			/* Origin position	*/
int VXs, VYs, VZs;			/* Grid spacing		*/

/*
 * MUDRAS kludge common block.
 */
extern struct MudHdr
{
	short mc_id[510];
	char  mc_namf[200];
	float mc_sclfld[25];
	int mc_nfl;
	float mc_csp[9];
	int mc_ncx[3];
	float mc_orlat[3], mc_orlon[3];
	int mc_ivdate, mc_ivtime;
} volume_;


int Lun = 10;			/* MUDRAS unit number		*/
int Nfile;
int DTime;			/* Time dimension		*/

# define CVTLL(a) ((a)[0] + ((a)[1]/60.0) + ((a)[2]/3600.0))
# define BADVAL -32768



main (argc, argv)
int argc;
char **argv;
{
	char *cdfname, *malloc (), *tmp;
	char junk[10];
	int print = -1, status, btime, level, nx, ny, toff = 60000;
	long i;
	float *grid, lat, lon, zero = 0, alt;
/*
 * Deal with file names.
 */
	if (argc < 5 || ! (argc & 0x1))
		usage ();
	cdopnr_ (&Lun, argv[1], junk, junk, junk, &print, &status, 
		strlen (argv[1]), 10, 10, 10);
	if (status)
	{
		printf ("Unable to open MUDRAS file '%s'\n", argv[1]);
		exit (1);
	}
	printf ("MUDRAS date: %d %d\n", volume_.mc_ivdate, volume_.mc_ivtime);
	cdfname = argv[2];
	argv += 3;
	argc -= 3;
/*
 * Now deal with field names.
 */
	for (Nfield = 0; argc > 0; Nfield++)
	{
		int len = strlen (argv[0]);
	/*
	 * Copy out the field info.
	 */
		strcpy (SrcFlds[Nfield], argv[0]);
		strcpy (DstFlds[Nfield], argv[1]);
	/*
	 * Find the mudras field number.
	 */
		for (i = 0; i < volume_.mc_nfl; i++)
			if (! strncmp (argv[0], volume_.mc_namf + 8*i, len))
				break;
		if (i >= volume_.mc_nfl)
		{
			printf ("No field '%s' in MUDRAS file\n", argv[0]);
			printf ("Fields are: ");
			for (i = 0; i < volume_.mc_nfl; i++)
			{
				char fn[9];
				strncpy (fn, volume_.mc_namf + 8*i, 8);
				fn[8] = '\0';
				printf ("%s ", fn);
			}
			printf ("\n");
			exit (1);
		}
		MFields[Nfield] = i + 1;	/* based at one	*/
	/*
	 * Move on.
	 */
		argv += 2;
		argc -= 2;
	}
/*
 * Create the netcdf file.
 */
 	if (getenv ("TIME_OFFSET"))
		toff = atoi (getenv ("TIME_OFFSET"));
	volume_.mc_ivtime += toff;	/* to GMT -- kludge	*/
	MakeCDF (cdfname);
/*
 * Fix up the times and put them in.
 */
	btime = TC_FccToSys (&volume_.mc_ivdate);	/* XXX	*/
	ncvarput1 (Nfile, VBTime, 0, &btime);
	i = 0;
	ncvarput1 (Nfile, VTOff, &i, &zero);
/*
 * Figure out our origin.
 */
	if (getenv ("ORIGIN_LAT"))
	{
		lat = atof (getenv ("ORIGIN_LAT"));
		lon = atof (getenv ("ORIGIN_LON"));
	}
	else
	{
		lat = CVTLL (volume_.mc_orlat);
		lon = CVTLL (volume_.mc_orlon);
		if (lat == 0.0 || lon == 0.0)
			printf ("WARNING: zero lat and/or lon!\n");
	}
	alt = volume_.mc_csp[6];		/* Mudras is in km */
	cvt_Origin (lat, lon);
	cvt_ToLatLon (volume_.mc_csp[0], volume_.mc_csp[3], &lat, &lon);
/*
 * Store the info into the file.
 */
	ncvarput1 (Nfile, VLat, 0, &lat);
	ncvarput1 (Nfile, VLon, 0, &lon);
	ncvarput1 (Nfile, VAlt, 0, &alt);
	ncvarput1 (Nfile, VXs, 0, volume_.mc_csp + 2);
	ncvarput1 (Nfile, VYs, 0, volume_.mc_csp + 5);
	ncvarput1 (Nfile, VZs, 0, volume_.mc_csp + 8);
/*
 * Allocate space for the data.
 */
	nx = volume_.mc_ncx[0];
	ny = volume_.mc_ncx[1];
	grid = (float *) malloc (nx * ny * sizeof (float));
	tmp = malloc (nx * ny * sizeof (short));
/*
 * Now we plow through the data.
 */
	for (level = 1; level <= volume_.mc_ncx[2]; level++)
		for (i = 0; i < Nfield; i++)
			MoveData (level, i, grid, nx, ny, tmp);

	ncclose (Nfile);
}







MakeCDF (name)
char *name;
/*
 * Make a cdf file by this name.
 */
{
	int dx, dy, dz, i;
	int dims[4];
	float bv = BADVAL;
	char fname[200];
/*
 * Make the file itself.
 */
	sprintf (fname, "%s%06d.%06d.cdf", name, volume_.mc_ivdate,
		volume_.mc_ivtime);
	Nfile = nccreate (fname, NC_CLOBBER);
/*
 * Make some dimensions.
 */
	DTime = ncdimdef (Nfile, "time", NC_UNLIMITED);
	dx = ncdimdef (Nfile, "x", volume_.mc_ncx[0]);
	dy = ncdimdef (Nfile, "y", volume_.mc_ncx[1]);
	dz = ncdimdef (Nfile, "z", volume_.mc_ncx[2]);
/*
 * Make the variables.
 */
	dims[0] = DTime;
	dims[1] = dz;
	dims[2] = dy;
	dims[3] = dx;

	for (i = 0; i < Nfield; i++)
	{
		VFields[i] = ncvardef (Nfile, DstFlds[i], NC_FLOAT, 4, dims);
		(void) ncattput (Nfile, VFields[i], "missing_value",
			NC_FLOAT, 1, &bv);
	}
/*
 * Times too.
 */
	VBTime = ncvardef (Nfile, "base_time", NC_LONG, 0, 0);
	VTOff = ncvardef (Nfile, "time_offset", NC_FLOAT, 1, dims);
/*
 * And positions.
 */
	VLat = ncvardef (Nfile, "lat", NC_FLOAT, 0, 0);
	VLon = ncvardef (Nfile, "lon", NC_FLOAT, 0, 0);
	VAlt = ncvardef (Nfile, "alt", NC_FLOAT, 0, 0);
	VXs = ncvardef (Nfile, "x_spacing", NC_FLOAT, 0, 0);
	VYs = ncvardef (Nfile, "y_spacing", NC_FLOAT, 0, 0);
	VZs = ncvardef (Nfile, "z_spacing", NC_FLOAT, 0, 0);
	ncendef (Nfile);
}





usage ()
{
	printf ("Usage: mudtocdf mudfile cdfdir mudfld cdffld...\n");
	exit (1);
}





MoveData (level, fld, grid, nx, ny, tmp)
int level, fld, nx, ny;
float *grid;
char *tmp;
/*
 * Move this data.
 */
{
	long start[4], count[4];
	int status, nid = 510, axis = 3;
	float bf = BADVAL;
/*
 * Pull the data from the mudras file.
 */
	fetchd_ (&Lun, &volume_.mc_id, &nid, &level, MFields + fld, tmp,
			grid, &nx, &ny, &axis, &bf, &status);
	if (status)
		printf ("FETCHD failure %d\n", status);
/*
 * Dump it into the netcdf file.
 */
	start[0] = start[2] = start[3] = 0;
	start[1] = level - 1;
	count[0] = 1;		/* time		*/
	count[1] = 1;		/* level	*/
	count[2] = ny;
	count[3] = nx;
	ncvarput (Nfile, VFields[fld], start, count, grid);
}







#ifdef notdef 	/* use the library versions, which now work even when
		   not connected to a message manager */

/*
 * lat,lon <-> x,y conversion utilities
 *
 */

# define PI 3.141592654

/*
 * Radius of the earth, in km
 */
# define R_EARTH	6372.

/*
 * Origin latitude and longitude (radians)
 */
static float	Origin_lat = -99.0, Origin_lon = -99.0;




void
cvt_ToXY (lat, lon, x, y)
float	lat, lon, *x, *y;
/* 
 * Convert lat and lon (deg) to x and y (km) using azimuthal 
 * orthographic projection
 */
{
	float	del_lat, del_lon;
/*
 * Convert the lat,lon to x,y
 */
	lat *= PI / 180.0;
	lon *= PI / 180.0;

	del_lat = lat - Origin_lat;
	del_lon = lon - Origin_lon;

	*x = R_EARTH * cos (lat) * sin (del_lon);
	*y = R_EARTH * sin (del_lat);
}




cvt_ToLatLon (x, y, lat, lon)
float	x, y, *lat, *lon;
/*
 * Convert x and y (km) to lat and lon (deg)
 */
{
	float	del_lat, del_lon;
/*
 * Convert the x,y to lat,lon
 */
	del_lat = asin (y / R_EARTH);
	*lat = Origin_lat + del_lat;

	del_lon = asin (x / (R_EARTH * cos (*lat)));
	*lon = Origin_lon + del_lon;
/*
 * Convert to degrees
 */
	*lat *= 180.0 / PI;
	*lon *= 180.0 / PI;
}






cvt_Origin (lat, lon)
float	lat, lon;
/*
 * Use lat,lon (deg) as the reference location for 
 * latitude,longitude <-> x,y conversions
 *
 * Return TRUE if we set the origin successfully, otherwise FALSE.
 */
{
/*
 * Store the values in radians
 */
	Origin_lat = lat * PI / 180.0;
	Origin_lon = lon * PI / 180.0;
}
#endif /* notdef */

