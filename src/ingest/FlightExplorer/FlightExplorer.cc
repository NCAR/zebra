# include <stdio.h>
# include <stdlib.h>
# include <unistd.h>
# include <errno.h>
# include <time.h>
# include <string>
# include <map>

//
// tail ID -> platform name map
//
typedef std::map<std::string,std::string> TailToPlatMap;

//
// location type
//
class Location
{
public:
    float lat;
    float lon;
    float alt; // km
};

//
// time -> Location map
//
typedef std::map<time_t,Location> LocationMap;

//
// tail num -> LocationMap map
//
typedef std::map<std::string,LocationMap> PlatLocs;

//
// feet to km
//
const float FeetToKm = 0.0003048;

//
// protos
//
TailToPlatMap& MakeMap(const std::string& aclistfile);
void usage(const char* myname);



main(int argc, char* argv[])
{
    if (argc < 3)
    {
	usage(argv[0]);
	exit(1);
    }

    //
    // build the tail number -> platform map
    //
    TailToPlatMap& acmap = MakeMap(argv[1]);

    //
    // make sure time conversions below use UTC
    //
    putenv("TZ=UTC");

    //
    // get the ZEB_TOPDIR environment variable
    //
    const char* ztopdir = getenv("ZEB_TOPDIR");
    if (! ztopdir)
	ztopdir = BASEDIR;
    
    //
    // go through the TZ files
    //
    PlatLocs platlocs;
    
    for (int i = 2; i < argc; i++)
    {
	//
	// open the file
	//
	FILE *tzfile = fopen(argv[i], "r");
	if (! tzfile)
	{
	    fprintf(stderr, "Error opening %s: %s\n", argv[i], 
		    strerror(errno));
	    exit(1);
	}
	//
	// Read lines from the TZ file.  The form is:
	//
	//	86869296,05/20/2003,195358,N425AS,212,084C,3833N,08936W
	//	-------- ---------- ------ ------ --- ---- ----- ------
	//          /      date     HHMMSS tail#  /      \   \   lon (DDMM)
	// Flight Explorer                       /        \ lat (DDMM)
	//   flight ID                     ground speed    \
	//                                (may be empty)    \
        //                                            altitude (100s feet)
	//
	char line[80];
	while (fgets(line, sizeof(line), tzfile))
	{
	    //
	    // Parse apart the line
	    //
	    int month, day, year;
	    int hhmmss;
	    char tailnum[16];
	    int ialt;
	    int lat_ddmm, lon_ddmm;
	    char lat_ns, lon_ew;

	    //
	    // Break the line as comma separated tokens, since it's 
	    // difficult to handle an empty speed item otherwise
	    //
	    char* token;
	    char* next_token = line;

	    //
	    // flight ID
	    //
	    token = strsep(&next_token, ",");

	    //
	    // date
	    //
	    token = strsep(&next_token, ",");
	    sscanf(token, "%d/%d/%d", &month, &day, &year);
	    
	    //
	    // time
	    //
	    token = strsep(&next_token, ",");
	    sscanf(token, "%d", &hhmmss);

	    //
	    // tail num
	    //
	    token = strsep(&next_token, ",");
	    strcpy(tailnum, token);

	    //
	    // speed (ignore)
	    //
	    token = strsep(&next_token, ",");

	    //
	    // altitude
	    //
	    token = strsep(&next_token, ",");
	    sscanf(token, "%d", &ialt);

	    //
	    // lat
	    //
	    token = strsep(&next_token, ",");
	    sscanf(token, "%d%c", &lat_ddmm, &lat_ns);

	    //
	    // lon
	    //
	    token = strsep(&next_token, ",");
	    sscanf(token, "%d%c", &lon_ddmm, &lon_ew);
	    
	    //
	    // verify that this is a plane we know about
	    //
	    if (! acmap[tailnum].length())
	    {
		fprintf(stderr, 
			"Unknown tail number %s ignored at %d/%d/%d %d\n",
			tailnum, month, day, year, hhmmss);
		continue;
	    }

	    //
	    // convert the time to time_t
	    //
	    struct tm time_tm;
	    time_tm.tm_sec = hhmmss % 100;
	    time_tm.tm_min = (hhmmss / 100) % 100;
	    time_tm.tm_hour = hhmmss / 10000;
	    time_tm.tm_mday = day;
	    time_tm.tm_mon = month - 1;
	    time_tm.tm_year = year - 1900;

	    time_t t = mktime(&time_tm);

	    //
	    // build the location and store it in the platlocs map
	    //
	    Location loc;
	    loc.lat = lat_ddmm / 100 + (lat_ddmm % 100) / 60.0;
	    if (lat_ns == 'S')
		loc.lat *= -1;
	    
	    loc.lon = lon_ddmm / 100 + (lon_ddmm % 100) / 60.0;
	    if (lon_ew == 'W')
		loc.lon *= -1;
	    
	    loc.alt = (ialt * 100) * FeetToKm;
	    if (loc.lat > 90.0)
		printf("%s @ %d\n", argv[i], hhmmss);
	    
	    platlocs[tailnum][t] = loc;
	}
	fclose(tzfile);
    }

    //
    // Loop through the platforms for which we got locs, and write out
    // the points
    //
    for (PlatLocs::iterator pi = platlocs.begin(); pi != platlocs.end(); pi++)
    {
	std::string tailnum = pi->first;
	LocationMap lmap = pi->second;

	//
	// traverse the locations, which the map type should sort for us
	// by key (time)
	//
	for (LocationMap::iterator li = lmap.begin(); li != lmap.end(); li++)
	{
	    time_t t = li->first;
	    Location loc = li->second;
	    std::string plat = acmap[tailnum];
	    char cmd[256];

	    //
	    // run actrack to ingest this point
	    //
	    sprintf(cmd, "%s/bin/actrack %s %.4f %.4f %.3f %d\n", ztopdir, 
		    plat.c_str(), loc.lat, loc.lon, loc.alt, t);
	    int status = system(cmd);
	    if (status != 0)
	    {
		fprintf(stderr, "exiting on actrack error!\n");
		exit(1);
	    }
	}
    }
}


TailToPlatMap&
MakeMap(const std::string& aclistfile)
{
    TailToPlatMap& map = *(new TailToPlatMap);
    FILE* infile = fopen(aclistfile.c_str(), "r");
    if (! infile)
    {
	fprintf(stderr, "Error opening '%s': %s\n", aclistfile.c_str(), 
		strerror(errno));
	exit(1);
    }

    //
    // Read lines from the map file.  A map line should contain a tail
    // number and a Zebra platform name.  Lines with a '#' in the first
    // column are treated as comments.
    //
    char line[80];
    while (fgets(line, sizeof(line), infile))
    {
	char tailnum[40];
	char platname[40];
	char junk[40];
	int nread;

	if (line[0] == '#')
	    continue;
	
	if ((nread = sscanf(line, "%s%s%s", tailnum, platname, junk)) == EOF)
	    continue;  // ignore empty lines
	else if (nread < 2 || (nread > 2 && junk[0] != '#'))
	{
	    // strip \n before we print the error
	    if (line[strlen(line) - 1] == '\n')
		line[strlen(line) - 1] = '\0';
	    fprintf(stderr, "Bad platform map line: '%s'\n", line);
	    exit(1);
	}
	//
	// Add this aircraft to the map
	//
	map[tailnum] = platname;
    }

    return map;
}


void
usage(const char* myname)
{
    fprintf(stderr, "Usage: %s <acmap_file> <TZfile> [<TZfile> ...]\n\n", 
	    myname);
    fprintf(stderr, "Where <acmap_file> is a text file mapping tail\n");
    fprintf(stderr, "numbers to Zebra platform names, with lines of the\n");
    fprintf(stderr, "form:\n\n");
    fprintf(stderr, "N425AS lear\n\n");
    fprintf(stderr, "and each <TZfile> is a FlightExplorer TZ file to\n");
    fprintf(stderr, "be ingested.\n");
}
    
