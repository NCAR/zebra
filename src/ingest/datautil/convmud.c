/*
 * Convert mudras files to netcdf.
 */
# include <stdio.h>
# include "netcdf.h"
# include <math.h>
# include <defs.h>
# include "mudutil.h"

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

int Nfile;
int DTime;			/* Time dimension		*/

# define BADVAL -32768


main (argc, argv)
int argc;
char **argv;
{
	Location origin;
	time t;
	CoordInfo xi, yi, zi;
	int nfld;
	char *cdfname, *malloc ();
	int i, btime, level, nx, ny, toff = 0;
	float *grid, lat, lon, zero = 0, alt;

/*
 * Deal with file names.
 */
	if (argc < 5 || ! (argc & 0x1))
		usage ();
	if (! MudOpen (argv[1], &t, &origin, &xi, &yi, &zi, &nfld))
		exit (1);
	printf ("MUDRAS date: %d %d\n", t.ds_yymmdd, t.ds_hhmmss);
	cdfname = argv[2];
	argv += 3;
	argc -= 3;
/*
 * Now deal with field names.
 */
	for (Nfield = 0; argc > 0; Nfield++)
	{
	/*
	 * Copy out the field info.
	 */
		strcpy (SrcFlds[Nfield], argv[0]);
		strcpy (DstFlds[Nfield], argv[1]);
	/*
	 * Find the mudras field number.
	 */
		for (i = 0; i < nfld; i++)
			if (! strcmp (argv[0], MudField (i)))
				break;
		if (i >= nfld)
		{
			printf ("No field '%s' in MUDRAS file\n", argv[0]);
			printf ("Fields are: ");
			for (i = 0; i < nfld; i++)
				printf ("%s ", MudField (i));
			printf ("\n");
			exit (1);
		}
		MFields[Nfield] = i;
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
	MakeCDF (cdfname, &t, xi.ci_NStep, yi.ci_NStep, zi.ci_NStep);
/*
 * Fix up the times and put them in.
 */
	btime = TC_FccToSys (&t) + toff*3600;
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
		lat = origin.l_lat;
		lon = origin.l_lon;
		if (lat == 0.0 || lon == 0.0)
			printf ("WARNING: zero lat and/or lon!\n");
	}
	alt = origin.l_alt;
	cvt_Origin (lat, lon);
	cvt_ToLatLon (xi.ci_MinVal, yi.ci_MinVal, &lat, &lon);
/*
 * Store the info into the file.
 */
	ncvarput1 (Nfile, VLat, 0, &lat);
	ncvarput1 (Nfile, VLon, 0, &lon);
	ncvarput1 (Nfile, VAlt, 0, &alt);
	ncvarput1 (Nfile, VXs, 0, &xi.ci_Spacing);
	ncvarput1 (Nfile, VYs, 0, &yi.ci_Spacing);
	ncvarput1 (Nfile, VZs, 0, &zi.ci_Spacing);
/*
 * Allocate space for the data.
 */
	nx = xi.ci_NStep;
	ny = yi.ci_NStep;
	grid = (float *) malloc (nx * ny * sizeof (float));
/*
 * Now we plow through the data.
 */
	for (level = 0; level < zi.ci_NStep; level++)
	{
		printf ("Doing level %d  \r", level);
		fflush (stdout);
		for (i = 0; i < Nfield; i++)
			MoveData (level, i, grid, nx, ny);
	}
	printf ("\n");
	ncclose (Nfile);
	exit (0);
}







MakeCDF (name, t, nx, ny, nz)
char *name;
time *t;
int nx, ny, nz;
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
	sprintf (fname, "%s%06d.%06d.cdf", name, t->ds_yymmdd, t->ds_hhmmss);
	Nfile = nccreate (fname, NC_CLOBBER);
/*
 * Make some dimensions.
 */
	DTime = ncdimdef (Nfile, "time", NC_UNLIMITED);
	dx = ncdimdef (Nfile, "x", nx);
	dy = ncdimdef (Nfile, "y", ny);
	dz = ncdimdef (Nfile, "z", nz);
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





MoveData (level, fld, grid, nx, ny)
int level, fld, nx, ny;
float *grid;
/*
 * Move this data.
 */
{
	int start[4], count[4];
	float bf = BADVAL;
/*
 * Pull the data from the mudras file.
 */
	if (! FetchGrid (MFields[fld], level, grid, bf))
		return;
# ifdef notdef
	fetchd_ (&Lun, &volume_.mc_id, &nid, &level, MFields + fld, tmp,
			grid, &nx, &ny, &axis, &bf, &status);
	if (status)
		printf ("FETCHD failure %d\n", status);
# endif
/*
 * Dump it into the netcdf file.
 */
	start[0] = start[2] = start[3] = 0;
	start[1] = level;
	count[0] = 1;		/* time		*/
	count[1] = 1;		/* level	*/
	count[2] = ny;
	count[3] = nx;
	ncvarput (Nfile, VFields[fld], start, count, grid);
}








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
