# include <stdio.h>
# include <stdlib.h>
# include <math.h>

# include <defs.h>

inline double deg_to_rad( double x ) { return( (x) * M_PI / 180.0 ); }

const double R_EARTH = 6378.;	// spherical earth radius, km


main( int argc, char* argv[] )
{
//
// Arg check
//
    if (argc != 5)
    {
	fprintf( stderr, "Usage: %s <lat0> <lon0> <lat1> <lon1>\n", argv[0] );
	exit( 1 );
    }
//
// Get the values
//
    int i;
    double vals[4];

    for (i = 0; i < 4; i++)
    {
	char *lastchar, *arg = argv[1 + i];
	vals[i] = strtod( arg, &lastchar );
	if (lastchar == arg)
	{
	    fprintf( stderr, "Bad value: '%s'\n", arg );
	    exit( 1 );
	}
    }

    double lat0 = vals[0];
    double lon0 = vals[1];
    double lat1 = vals[2];
    double lon1 = vals[3];

    if (lat0 < -90.0 || lat0 > 90.0 || lat1 < -90.0 || lat1 > 90.0)
    {
	fprintf( stderr, "Latitudes must be between -90.0 and 90.0\n" );
	exit( 1 );
    }

    if (lon0 < -180.0 || lon0 > 180.0 || lon1 < -180.0 || lon1 > 180.0)
    {
	fprintf( stderr, "Longitudes must be between -180.0 and 180.0\n" );
	exit( 1 );
    }
//
// Figure out the range and angle from lat0/lon0 to lat1/lon1.  This is
// quite crude, but should be good enough...
//
    double midlat_rad = deg_to_rad( (lat1 + lat0) * 0.5 );	// radians
    double dx = (lon1 - lon0) / 360.0 * 2 * M_PI * R_EARTH * cos( midlat_rad );
    double dy = (lat1 - lat0) / 360.0 * 2 * M_PI * R_EARTH;

    double base_range = hypot( dx, dy );
    double base_angle = atan2( dy, dx );
//
// Generate points assuming base_angle of zero
//
    int npts = 0;
    double di;
    double x[600], y[600];

    for (i = -60; i <= 240; i++)
    {
	di = (double) i;
	x[npts] = base_range * (0.5 + cos( deg_to_rad( i ) ));
	y[npts] = base_range * (sqrt(3.0) / 2.0 + sin( deg_to_rad( i ) ));
	npts++;
    }

    for (i = -239; i <= 59; i++)
    {
	di = (double) i;
	x[npts] = base_range * (0.5 + cos( deg_to_rad( i ) ));
	y[npts] = base_range * (-sqrt(3.0) / 2.0 + sin( deg_to_rad( i ) ));
	npts++;
    }
//
// Rotate the points around lat0/lon0 using the real baseline angle
//
    for (i = 0; i < 600; i++)
    {
	double range = hypot( x[i], y[i] );
	double ang = atan2( y[i], x[i] ) + base_angle;
	x[i] = range * cos( ang );
	y[i] = range * sin( ang );
    }
//
// Translate the x,y values into lat/lon space
//
    float lat[600], lon[600];
    double minlat = 90.0, maxlat = -90.0, minlon = 180.0, maxlon = -180.0;
    
    cvt_Origin( lat0, lon0 );
    for (i = 0; i < 600; i++)
    {
	cvt_ToLatLon (x[i], y[i], lat + i, lon + i);
	if (lat[i] < minlat)
	    minlat = lat[i];
	if (lat[i] > maxlat)
	    maxlat = lat[i];
	if (lon[i] < minlon)
	    minlon = lon[i];
	if (lon[i] > maxlon)
	    maxlon = lon[i];
    }
//
// Spit out a Zebra-style ASCII map file
//
    printf( "1200 %9.3f %9.3f %9.3f %9.3f\n", maxlat, minlat, maxlon, minlon );
    for (i = 0; i < 600; i++)
    {
	printf( " %9.3f %9.3f", lat[i], lon[i] );
	if (! ((i + 1) % 4))
	    printf( "\n" );
    }
}

	
    
    
	
	
    
