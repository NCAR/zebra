
#include <math.h>
#include "defs.h"

RCSID("$Id: cvt_test.c,v 2.1 1995-05-18 17:21:29 granger Exp $")

#define TOLERANCE 0.0001	/* ten-thousandths of a degree */


int
cvt_TestOrigin (olat, olon)
float olat;
float olon;
{
	float lat, lon;

	/*
	 * Set the origin and verify it 
	 */
	cvt_Origin (olat, olon);
	cvt_GetOrigin (&lat, &lon);
	if (fabs(lat - olat) > TOLERANCE || fabs(lon - olon) > TOLERANCE)
	{
		printf ("could not verify origin\n");
		return (1);
	}
	return (0);
}


int
cvt_Test (ilat, ilon, dx, dy)
float ilat;
float ilon;
float dx;
float dy;
{
	int errors = 0;
	float x, y;
	float lat, lon;

	/*
	 * Convert to x/y and back
	 */
	cvt_ToXY (ilat, ilon, &x, &y);
	if ((!(dx/3 <= x && x <= dx) && !(dx <= x && x <= dx/3)) ||
	    (!(dy/3 <= y && y <= dy) && !(dy <= y && y <= dy/3)))
	{
		++errors;
		printf ("(lat=%.3f,lon=%.3f)=>(x=%.3f,y=%.3f)%s(%.3f,%.3f)\n",
			ilat, ilon, x, y, " outside of ", dx, dy);
	}
	cvt_ToLatLon (x, y, &lat, &lon);
	if (fabs(lat - ilat) > TOLERANCE || fabs(lon - ilon) > TOLERANCE)
	{
		++errors;
		printf ("(lat=%.3f,lon=%.3f)=>(%.3f,%.3f) out of tolerance\n",
			lat, lon, ilat, ilon);
	}
	return (errors);
}


int
QuadTest (olat, olon)
float olat, olon;
{
	int errors = 0;
	int i;
	float delta;
	float lat, lon;
	float x, y;
#	define FIX(lon) ((lon)>180?((lon)-360):((lon)<-180?(lon)+360:(lon)))

	errors += cvt_TestOrigin (olat, olon);
	/*
	 * Test the center
	 */
	cvt_ToXY (olat, olon, &x, &y);
	if (fabs(x) > 0.001 || fabs(y) > 0.001)
	{
		printf ("origin (%.3f,%.3f) => (%.3f,%.3f) instead of (0,0)\n",
			olat, olon, x, y);
		++errors;
	}
	cvt_ToLatLon (x, y, &lat, &lon);
	if (fabs(lat - olat) > TOLERANCE || fabs(lon - olon) > TOLERANCE)
	{
		++errors;
		printf ("(lat=%.3f,lon=%.3f)=>(%.3f,%.3f) out of tolerance\n",
			lat, lon, olat, olon);
	}
	for (i = 2; i <= 20; i += 2)
	{
		delta = i * 115;	/* ~111 km per degree */
		errors += cvt_Test (olat + i, FIX(olon + i), delta, delta);
		errors += cvt_Test (olat - i, FIX(olon - i), -delta, -delta);
		errors += cvt_Test (olat - i, FIX(olon + i), delta, -delta);
		errors += cvt_Test (olat + i, FIX(olon - i), -delta, delta);
	}
	return (errors);
}


int
CvtTest ()
{
	int errors = 0;

	/* Quad-test a bunch of origins around the world */
	errors += QuadTest (40.0, -150.0);
	errors += QuadTest (0.0, 0.0);
	errors += QuadTest (50.0, 0.0);
	errors += QuadTest (-60.0, 120.0);
	errors += QuadTest (0.0, 175.0);
	errors += QuadTest (0.0, -175.0);
	errors += QuadTest (50.0, 175.0);
	errors += QuadTest (-50.0, -175.0);
	return (errors);
}



int
main ()
{
	int ret = CvtTest();
	if (ret)
	{
		printf ("cvt_test: failed with %d errors.\n", ret);
	}
	exit (ret);
}

