/*
 * Accept VOR coordinates and azimuth/range and calculate lat/lon.
 */

#include <math.h>
#include <string>
#include <map>
#include <vector>
#include <iostream>
#include <sstream>
#include <iterator>

#include "vor.h"
#include <defs.h>

RCSID("$Id: vor.cc,v 1.3 2003-05-19 20:26:40 burghart Exp $")


VOR::map_type VOR::lookup;


// Declination is taken to be the compass bearing to magnetic
// north relative to the true north compass.  So negative declination
// at a position means magnetic north is west of the position.
static VOR RON ("RON", "45 49 47", "13 28 48", 0);
static VOR TZO ("TZO", "45 33 30", "09 30 30", -1.0);
static VOR SRN ("SRN", "45 38 42", "09 01 19", -1.0);
static VOR TOP ("TOP", "44 55 29", "07 51 43", -1.5);
static VOR VIL ("VIL", "45 24 26", "10 54 24", 0);
static VOR GEN ("GEN", "44 25 25", "09 04 57", 0);
static VOR CHI ("CHI", "45 04 14", "12 16 54", 0);


VOR::VOR (std::string name_, std::string lat_, std::string lon_, double dec) :
    name(name_),
    declination(dec)
{
    // Compute lat and lon from strings with 'deg min sec'
    std::istringstream slat(lat_.c_str());
    std::istringstream slon(lon_.c_str());

    double deg, min, sec;
    slat >> deg >> min >> sec;
    lat = deg + (min/60.0) + (sec/3600.0);
    slon >> deg >> min >> sec;
    lon = deg + (min/60.0) + (sec/3600.0);
    // XXX
    lookup.insert (*(new std::pair<std::string,VOR *> (name, this)));
}


VOR *
VOR::find (std::string name)
{
    std::string n(name);
    for (std::string::iterator i = n.begin(); i != n.end(); ++i)
    {
	*i = toupper(*i);
    }
    return (lookup.find (n))->second;
}


// Return non-zero on success
int
VOR::Convert (std::string name, double azimuth, double range,
	      double *lat, double *lon)
{
    int success = 0;
    VOR *vor = VOR::find (name);
    if (vor)
    {
	// Adjust azimuth to true north compass degrees
	azimuth += vor->declination;
	cvt_Origin (vor->lat, vor->lon);

	// Get km x and y for the bearing and range from the origin.
	double x = range * sin (azimuth * M_PI / 180.0);
	double y = range * cos (azimuth * M_PI / 180.0);
	float flat, flon;
	cvt_ToLatLon (x, y, &flat, &flon);
	*lat = flat;
	*lon = flon;
	success = 1;
    }
    return success;
}


extern "C" {

int VOR_Convert (char *name, double a, double r, double *lat, double *lon)
{
    std::string n(name);
    return VOR::Convert (n, a, r, lat, lon);
}

}
