#include <iostream>
#include "vor.h"
#include <defs.h>

RCSID("$Id: vormain.cc,v 1.2 2003-05-19 20:26:40 burghart Exp $")

static void usage()
{
    std::cerr << "vor <name> <azimuth> <range>" << std::endl;
    std::cerr << "where" << std::endl;
    std::cerr << "   azimuth is in magnetic compass degrees" << std::endl;
    std::cerr << "   range is in kilometers" << std::endl;
    std::cerr << "   name is the name of vor:" << std::endl;

    // ostream_iterator out (std::cerr, "\n");
    // copy (VOR::lookup.begin(), VOR::lookup.end(), out);

    VOR::map_type::iterator i;
    for (i = VOR::lookup.begin(); i != VOR::lookup.end(); ++i)
    {
	VOR *vor = i->second;
	std::cerr << vor->name << " " << vor->lat << " " << vor->lon
	     << " " << vor->declination << std::endl;
    }
    exit (1);
}




int
main (int argc, char *argv[])
{
    int result = 1;

    if (argc != 4)
    {
	usage ();
    }

    std::string name(argv[1]);
    double azimuth = strtod (argv[2], 0);
    double range = strtod (argv[3], 0);
    double lat, lon;

    if (! VOR::find (name))
    {
	std::cerr << "vor name not found: " << name << std::endl;
	usage();
    }
    if (VOR::Convert (name, azimuth, range, &lat, &lon))
    {
	std::cout << lat << " " << lon << std::endl;
	result = 0;
    }
    else
    {
	std::cerr << "vor coordinate translation failed." << std::endl;
    }
    return result;
}

