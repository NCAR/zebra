
#ifndef __vor_h_
#define __vor_h_

#ifdef __cplusplus

#include <string>
#include <map>
#include <vector>

class VOR
{
public:
    string name;
    double lat;		// degrees North
    double lon;		// degrees East
    double declination;	// compass degrees

    typedef map<string,VOR*> map_type;

    // static vector<VOR> list;
    static map_type lookup;

    static VOR *find (string n);

    VOR::VOR (string name_, string lat_, string lon_, double dec);

    // Return non-zero on success
    static int Convert (string name, double azimuth, double range,
			double *lat, double *lon);
};


#else

extern "C"
{

int
VOR_Convert (char *name, double a, double r, double *lat, double *lon);

}

#endif


#endif /* __vor_h_ */
