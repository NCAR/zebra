/*
 * Replace missing values in the latitude or longitude fields of a
 * netcdf file with interpolated values. 
 */
static char *rcsid = "$Id: cdf_interpolate.c,v 1.1 1992-11-05 20:28:58 kris Exp $";
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
# include "netcdf.h"

# define STRLEN		80
# define MAXRECORDS	20000
# define MISSING	0

/*
 * Our routines.
 */
static float	interpolate FP ((float *, int, double));
static float	extrapolate FP ((float *, int, double));

main (argc, argv)
int	argc;
char	**argv;
{
	int	i;
	int	cdfid;
	int	num_records;
	int	latid, lonid;
	long	start[1], count[1];
	float	missing_lat, missing_lon;
	float	*latitudes, *longitudes;
/*
 * Check arguments.
 */
	if (argc < 2)
	{
		printf ("Usage: cdf_interpolate <input file>\n");
		exit (0);
	}
/*
 * Open the input file.
 */
	cdfid = ncopen (argv[1], NC_WRITE);
/*
 * How many records in this file?
 */
	ncdiminq (cdfid, ncdimid (cdfid, "time"), (char *) 0, &num_records); 
	if (num_records > MAXRECORDS)
	{
		printf ("Error: Number of records %d greater than max %d.\n",
			num_records, MAXRECORDS);
		exit (0);
	}
/*
 * Get the variable id's for latitude and longitude.
 */
	latid = ncvarid (cdfid, "lat");
	lonid = ncvarid (cdfid, "lon");
/*
 * Allocate space for lat/lon values.
 */
	latitudes = (float *) malloc (num_records * sizeof (float));
	longitudes = (float *) malloc (num_records * sizeof (float));
/*
 * Read in the lat/lon data.
 */
	start[0] = 0;
	count[0] = num_records;
	ncvarget (cdfid, latid, start, count, latitudes);
	ncvarget (cdfid, lonid, start, count, longitudes);
/*
 * Get the missing values.
 */
# ifdef notdef
	ncattget (cdfid, latid, "missing_value", &missing_lat);
	ncattget (cdfid, lonid, "missing_value", &missing_lon);
# endif
	missing_lat = MISSING;
	missing_lon = MISSING;
/*
 * Test for a missing initial value and extrapolate.
 */
	if (latitudes[0] == missing_lat)
		latitudes[0] = extrapolate (latitudes, 0, missing_lat);
	if (longitudes[0] == missing_lon)
		longitudes[0] = extrapolate (longitudes, 0, missing_lon);
/*
 * Search for missing values and interpolate.
 */
	for (i = 1; i < num_records; i++)
	{
		if (latitudes[i] == missing_lat)
			latitudes[i] = interpolate(latitudes, i, missing_lat);
		if (longitudes[i] == missing_lon)
			longitudes[i] = interpolate(longitudes, i, missing_lon);
	}
/*
 * Store the lat/lon data.
 */
	ncvarput (cdfid, latid, start, count, latitudes);
	ncvarput (cdfid, lonid, start, count, longitudes);
	ncclose (cdfid);
}

static float
interpolate (array, index, missing_value)
float	*array, missing_value;
int	index;
/*
 * Calculate the interpolated value.
 */
{
	int	i = 0;
	float	x0, x1, y0, y1;
	float	x;
/*
 * Initialize x's and y's.
 */
	x0 = (float) (index - 1);
	y0 = array[index - 1];
	while (array[index + (++i)] == missing_value);
	if (i > 5)
		printf ("Warning: Gap of %d.\n", i);
	x1 = (float) (index + i);
	y1 = array[index + i]; 
	x = (float) index;
/*
 * Return the interpolated value.
 */
	return ((x - x1)/(x0 - x1) * y0 + (x - x0)/(x1 - x0) * y1);
}


static float
extrapolate (array, index, missing_value)
float	*array, missing_value;
int	index;
/*
 * Calculate the extrapolated value.
 */
{
	int	i = 0;
	float	x0, y0, x1, y1;
	float	x;

	printf ("Extrapolating\n");
/*
 * Initialize x's and y's.
 */
	while (array[index + (++i)] == missing_value);
	x0 = index + i;
	y0 = array[index + i];
	while (array[index + (++i)] == missing_value);
	x1 = index + i;
	y1 = array[index + i];
	x = index;
/*
 * Return the extrapolated value.
 */
	return (y0 - (y1 - y0)/(x1 - x0)*(x0 - x));
}

