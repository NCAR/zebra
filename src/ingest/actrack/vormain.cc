
#include "vor.h"
#include <defs.h>

RCSID("$Id: vormain.cc,v 1.1 2000-12-13 23:01:47 granger Exp $")

static void usage()
{
    cerr << "vor <name> <azimuth> <range>" << endl;
    cerr << "where" << endl;
    cerr << "   azimuth is in magnetic compass degrees" << endl;
    cerr << "   range is in kilometers" << endl;
    cerr << "   name is the name of vor:" << endl;

    // ostream_iterator out (cerr, "\n");
    // copy (VOR::lookup.begin(), VOR::lookup.end(), out);

    VOR::map_type::iterator i;
    for (i = VOR::lookup.begin(); i != VOR::lookup.end(); ++i)
    {
	VOR *vor = i->second;
	cerr << vor->name << " " << vor->lat << " " << vor->lon
	     << " " << vor->declination << endl;
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

    string name(argv[1]);
    double azimuth = strtod (argv[2], 0);
    double range = strtod (argv[3], 0);
    double lat, lon;

    if (! VOR::find (name))
    {
	cerr << "vor name not found: " << name << endl;
	usage();
    }
    if (VOR::Convert (name, azimuth, range, &lat, &lon))
    {
	cout << lat << " " << lon << endl;
	result = 0;
    }
    else
    {
	cerr << "vor coordinate translation failed." << endl;
    }
    return result;
}

