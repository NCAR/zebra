# ifndef _STATION_HH_
# define _STATION_HH_

# include <map>
# include <string>

class Station
{
public:
    Station(string name, float lat, float lon, float alt)
    {
	stName = name;
	stLat = lat;
	stLon = lon;
	stAlt = alt;		// meters
    }
	    
    inline string getName(void) const { return stName; }
    inline float getLat(void) const { return stLat; }
    inline float getLon(void) const { return stLon; }
    inline float getAlt(void) const { return stAlt; }

    inline bool operator==(Station& sta)
    {
	return(stName == sta.getName() && stLat == sta.getLat() && 
	       stLon == sta.getLon() && stAlt == sta.getAlt());
    }
private:
    string stName;
    float stLat;
    float stLon;
    float stAlt;
};

typedef map<string, Station*> StationMap;
typedef StationMap::iterator StationMapIterator;

# endif // ndef _STATION_HH_
