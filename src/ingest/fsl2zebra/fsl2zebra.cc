# include <fstream>
# include <map>
# include <stdio.h>
# include <string.h>
# include <time.h>

# include <netcdf.hh>
# include <udunits.h>

# include "outVar.hh"
# include "zebraMesonetOutFile.hh"

void usage(const char* name);
void makeStationMap(StationMap& map, const char* filename);


main(int argc, char* argv[])
{
    //
    // Check usage
    //
    char *myName = argv[0];

    if (argc < 4) 
	usage(myName);

    char *periodString = argv[1];
    char *endptr;
    int dataPeriodMinutes = strtol(periodString, &endptr, 10);
    if (endptr == periodString)
	usage(myName);
    
    char *stnListFileName = argv[2];
    char *fslFileName = argv[3];

    //
    // Create the list of stations of interest
    //
    StationMap stations;
    makeStationMap(stations, stnListFileName);

    //
    // Don't die on netCDF errors
    //
    NcError errState(NcError::silent_nonfatal);

    //
    // open the FSL mesonet file
    //
    NcFile infile(fslFileName);

    if (! infile.is_valid())
    {
	fprintf(stderr, "Cannot open FSL netCDF file: '%s'\n", fslFileName);
	exit(1);
    }

    doubleOutVar* timeVar;
    stringOutVar* idVar;

    floatVarList vars(0);

    //
    // Find the vars of interest in the FSL file
    //
    try 
    {
	//
	// gotta have time and station id
	//
	try {
	    timeVar = new doubleOutVar("time", 
				       "seconds since 1-1-1970 00:00:00 00:00",
				       infile, "observationTime");
	} catch (outVar::NoSuchVar nsv) {
	    timeVar = new doubleOutVar("time",
				       "seconds since 1-1-1970 00:00:00 00:00",
				       infile, "timeObs");
	}

	try {
	    idVar = new stringOutVar("stationId", infile, "stationId");
	} catch (outVar::NoSuchVar nsv) {
	    idVar = new stringOutVar("stationId", infile, "stationName");
	}
	
	//
	// everything else is what we want in our output file
	//
	vars.push_back(new floatOutVar("temperature", "degC", infile, 
				       "temperature"));
	vars.push_back(new floatOutVar("altimeter", "hPa", infile, 
				       "altimeter"));
	vars.push_back(new floatOutVar("dewpoint", "degC", infile, 
				       "dewpoint"));
	vars.push_back(new floatOutVar("wdir", "degrees", infile, "windDir"));
  	vars.push_back(new floatOutVar("wspd", "m/s", infile, "windSpeed"));
    } catch (outVar::VarError ve) {
	fprintf(stderr, "var error: %s\n", ve.c_str());
	exit(1);
    }

    //
    // Now loop through the records in the FSL file and dump the
    // stuff of interest into the Zebra file
    //
    vector<zebraMesonetOutFile*> outFiles;

    long now = time(0);

    const string* ids = idVar->getVals();
    const double* times = timeVar->getVals();

    for (int r = 0; r < idVar->getNVals(); r++)
    {
	//
	// Skip stations not in our list
	//
	if (stations.find(ids[r]) == stations.end())
	    continue;
	
	const Station* s = stations[ids[r]];
	//
	// Skip stuff that isn't at least 30 minutes old.  Otherwise
	// our most recent Zebra time record will almost always be sparsely
	// popuplated.
	//
	if ((now - times[r]) < 1800)
	    continue;
	//
	// See if one of our open files gets this sample
	//
	zebraMesonetOutFile* file = NULL;
	for (int f = 0; f < outFiles.size(); f++)
	{
	    if (outFiles[f]->containsTime((long)times[r]))
		file = outFiles[f];
	    continue;
	}
	//
	// If we didn't find a good open file, open one now
	//
	if (! file)
	{
	    try {
		file = new zebraMesonetOutFile((long)times[r], 
					       dataPeriodMinutes, stations, 
					       vars);
	    } catch (zebraMesonetOutFile::FileError fe) {
		fprintf(stderr, "file error: %s\n", fe.c_str());
		exit(1);
	    }
	    outFiles.push_back(file);
	}
	//
	// Write the vars
	//
	for (int i = 0; i < vars.size(); i++)
	{
	    const floatOutVar* var = vars[i];
	    float val = var->getVals()[r];

	    if (var->isMissing(val))
		continue;

	    file->putVar(s, var->getName(), times[r], val);
	}
    }
	
    // while (read a record)
    //	if (! want record)
    //    continue;
    //  find good time for record
    //  store record
    // endwhile
    //	if (! want record)
    //    continue;
    //  find good time for record
    //  store record
}


void
usage(const char* name)
{
    fprintf(stderr, 
	    "Usage: %s <data_period_minutes> <station_list_file> <fsl_file>\n",
	    name);
    exit(1);
}



void
makeStationMap(StationMap& map, const char* filename)
{
    ifstream infile(filename);

    char line[256];
    while (infile.getline(line, sizeof(line)) && ! infile.eof())
    {
	//
	// Comment lines begin with "#"
	//
	if (line[0] == '#')
	    continue;
	//
	// Ignore blank lines as well
	//
	char junk[2];
	if (sscanf(line, "%1s", junk) < 1)
	    continue;
	//
	// Other lines should define stations: 
	//   "<name> <lat> <lon> <alt>"
	//
	float lat, lon, alt;
	char name[32];
	if (sscanf(line, "%s%f%f%f%d", name, &lat, &lon, &alt) != 4)
	{
	    fprintf(stderr, "Bad station line '%s'!\n", line);
	    exit(1);
	}

	map[name] = new Station(name, lat, lon, alt);
    }
}
