/*
 * Replace missing values in the latitude or longitude fields of a
 * netcdf file with interpolated values. 
 */
static char *rcsid = "$Id: cdf_interpolate.c,v 1.4 1992-12-22 18:36:32 granger Exp $";
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
static void	get_missing_value FP ((int cdfid, int varid, float *value));

main (argc, argv)
int	argc;
char	**argv;
{
	int	i,j;
	int	cdfid;
	int	num_records;
	int	latid, lonid;
	long	start[1], count[1];
	float	missing_lat, missing_lon;
	float	*latitudes, *longitudes;
	int	nlat = 0, nlon = 0;		/* gap measurements */
		
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

	get_missing_value( cdfid, latid, &missing_lat );
	get_missing_value( cdfid, lonid, &missing_lon );
	printf("missing lat = %f,    missing lon = %f\n",
	       missing_lat, missing_lon);
/*
 * Test for a missing initial value and extrapolate.
 */
	if (latitudes[0] == missing_lat)
	{
		printf("Extrapolating lat\n");
		latitudes[0] = extrapolate (latitudes, 0, missing_lat);
	}
	if (longitudes[0] == missing_lon)
	{
		printf("Extrapolating lon\n");
		longitudes[0] = extrapolate (longitudes, 0, missing_lon);
	}
/*
 * Search for missing values and interpolate.
 */
	nlat = nlon = 0;
	for (i = 1; i < num_records; i++)
	{
		if (latitudes[i] == missing_lat)
		{
			latitudes[i] = interpolate(latitudes,i,missing_lat);
			++nlat;
		}
		else
		{
			if (nlat > 5)
				printf("Warning -- %s gap of %i at %i\n",
				       "Latitude", nlat, i - nlat);
			nlat = 0;
		}
		if (longitudes[i] == missing_lon)
		{
			++nlon;
			longitudes[i] = interpolate(longitudes,i,missing_lon);
		}
		else
		{
			if (nlon > 5)
				printf("Warning -- %s gap of %i at %i\n",
				       "Longitude", nlon, i - nlon);
			nlon = 0;
		}
	}
/*
 * Store the lat/lon data.
 */
	ncvarput (cdfid, latid, start, count, latitudes);
	ncvarput (cdfid, lonid, start, count, longitudes);
	ncclose (cdfid);
}



static void
get_missing_value( cdfid, varid, missing )
int cdfid;
int varid;
float *missing;
/*
 * Get the missing values.
 */
{
	char 	m_string[STRLEN];
	double	m_double;
	nc_type	m_type;
        long	m_len;
	int	opts = ncopts;
	double	atof();

	ncopts = 0;
	if (ncattinq (cdfid, varid, "missing_value", &m_type, &m_len) >= 0)
	{
		switch (m_type)
		{
		   case NC_CHAR:
			ncattget (cdfid, varid, "missing_value", m_string);
			*missing = atof(m_string);
			break;
		   case NC_FLOAT:
			ncattget (cdfid, varid, "missing_value", missing);
			break;
		   case NC_DOUBLE:
			ncattget (cdfid, varid, "missing_value", &m_double);
			*missing = (float)m_double;
			break;
		   default:
			printf (
			  "missing_value type not supported. Using %f.\n",
			   MISSING);
			*missing = MISSING;
			break;
		}
	}
	else if ((ncattinq (cdfid, NC_GLOBAL, "bad_value_flag",
			   &m_type, &m_len) >= 0) && (m_type == NC_CHAR))
	{
		ncattget (cdfid, NC_GLOBAL, "bad_value_flag", m_string);
		*missing = atof(m_string);
	}
	else
		*missing = MISSING;

	ncopts = opts;
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

