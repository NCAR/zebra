/*
 * lat,lon <-> x,y conversion utilities
 */
static char *rcsid = "$Id: convert.c,v 2.6 1994-06-29 20:56:02 case Exp $";
/*		Copyright (C) 1987,88,89,90,91 by UCAR
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
# include <defs.h>
# include <message.h>

# define PI	3.141592654

/*
 * Radius of the earth, in km
 */
# define R_EARTH	6378.

/*
 * Has the origin been set?
 */
int	OriginSet = FALSE;

/*
 * Origin latitude and longitude (radians)
 */
static float	Origin_lat = -99.0, Origin_lon = -99.0;





void
cvt_ToXY (lat, lon, x, y)
float	lat, lon, *x, *y;
/* 
 * Convert lat and lon (deg) to x and y (km) using equidistant 
 * cylindrical (rectangular) projection
 */
{
	float	del_lat, del_lon;	/* delta lat, delta lon, in radians */
/*
 * Make sure we have an origin
 */
	if (! OriginSet)
	{
		msg_ELog (EF_PROBLEM, 
			  "No origin for lat/lon -> x,y conversion");
		*x = *y = 0.0;
		return;
	}
/*
 * Convert the lat,lon to x,y, making sure to do the arithmetic in mod(180
 * deg).  The greatest difference between two longitudes will be +/- 180
 * deg (PI R), and the greatest difference between two latitudes will be
 * +/- 90 deg (PI/2 R).
 */
	lat *= PI / 180.0;
	lon *= PI / 180.0;

	del_lat = lat - Origin_lat;
	del_lon = lon - Origin_lon;
	if (fabs(del_lon) > PI)
	{
	/* 
	 * Longitudes across Intl Date Line,
	 * so add or subtract a full circle from our delta value.
	 * If lon < 0 (del_lon < 0), then add one circle to make lon > 0:
	 *	del_lon = (lon + 2*PI) - (Origin_lon) = del_lon + 2*PI;
	 * Else Origin_lon < 0 (del_lon > 0), so add one circle to Origin_lon:
	 *	del_lon = (lon) - (Origin_lon + 2*PI) = del_lon - 2*PI;
	 */
		if (del_lon < 0)
			del_lon += 2*PI;
		else
			del_lon -= 2*PI;
	}

	*x = R_EARTH * cos (Origin_lat) * del_lon;
	*y = R_EARTH * del_lat;
}




void
cvt_ToLatLon (x, y, lat, lon)
float	x, y, *lat, *lon;
/*
 * Convert x and y (km) to lat and lon (deg) using equidistant 
 * cylindrical (rectangular) projection
 */
{
	float	del_lat, del_lon;
/*
 * Make sure we have an origin
 */
	if (! OriginSet)
	{
		msg_ELog (EF_PROBLEM, 
			  "No origin for x,y -> lat/lon conversion!");
		*lat = *lon = 0.0;
		return;
	}
/*
 * Convert the x,y to lat,lon
 */
	del_lat = y / R_EARTH;
	*lat = Origin_lat + del_lat;

	del_lon = x / (R_EARTH * cos (Origin_lat));
	*lon = Origin_lon + del_lon;
/*
 * Convert to degrees
 */
	*lat *= 180.0 / PI;
	*lon *= 180.0 / PI;
/*
 * Center the longitude from -180 to 180
 */
	*lon = (float)fmod(*lon + 540.0, 360.0) - 180.0;
}




int
cvt_GetOrigin (lat, lon)
float	*lat, *lon;
/*
 * Return the current origin values (in degrees)
 */
{
/*
 * Make sure we have an origin
 */
	if (! OriginSet)
		return (FALSE);
	else
	{
		*lat = Origin_lat / PI * 180.0;
		*lon = Origin_lon / PI * 180.0;
		return (TRUE);
	}
}




bool
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
 * Test that the values are in range
 */
	if (lat > 90.0 || lat < -90.0)
	{
		msg_ELog (EF_INFO, "Latitude out of range (-90.0 to 90.0)");
		OriginSet = FALSE;
		return (FALSE);
	}
	if (lon > 180.0 || lon < -180.0)
	{
		msg_ELog (EF_INFO, "Longitude out of range (-180.0 to 180.0)");
		OriginSet = FALSE;
		return (FALSE);
	}
/*
 * Store the values in radians
 */
	Origin_lat = lat * PI / 180.0;
	Origin_lon = lon * PI / 180.0;
	OriginSet = TRUE;
	return (TRUE);
}



void
cvt_ShowOrigin ()
/*
 * Print out the current origin
 */
{
	if (! OriginSet)
	{
		msg_ELog (EF_INFO, 
			  "No current origin for lat/lon <-> x,y conversions");
		return;
	}

	msg_ELog (EF_INFO,
		"Current origin for lat/lon <-> x,y conversions: %.4f/%.4f",
		Origin_lat * 180.0 / PI, Origin_lon * 180.0 / PI);
	return;
}

