# include <map>
# include <stdio.h>
# include <time.h>
# include "zebraMesonetOutFile.hh"



const string zebraMesonetOutFile::DATA_PERIOD_ATT("data_period");
string zebraMesonetOutFile::basedir(".");

const long SECS_PER_DAY = 24 * 60 * 60;



zebraMesonetOutFile::zebraMesonetOutFile(long time, int dataPeriodMinutes,
					 StationMap& needStations, 
					 floatVarList& outVars)
    throw (FileError)
{
    //
    // nominal sample period
    //
    period = dataPeriodMinutes * 60;
    //
    // use the start of the day as the file start time, being careful
    // with times late in the day that may round into the first sample 
    // of the next day
    //
    startTime = time + period / 2 - 1;
    startTime -= startTime % SECS_PER_DAY;
    endTime = startTime + SECS_PER_DAY - period;

    struct tm* daystart = gmtime(&startTime);
    char daystring[32];
    strftime(daystring, sizeof(daystring) - 1, "%Y%m%d", daystart);
    filename = string(basedir + "/" + daystring + ".nc");
    //
    // Open or create a file
    //
    ncfile = new NcFile(filename.c_str(), NcFile::Write);
    if (ncfile->is_valid())
	// file already exists, so make sure it's ok
	checkFile(needStations, outVars);
    else
	// Nope, need a new file
	createFile(needStations, outVars);
}


void
zebraMesonetOutFile::createFile(StationMap& needStations, 
				floatVarList& outVars) 
    throw (FileError)
{
    ncfile = new NcFile(filename.c_str(), NcFile::New);
    if (! ncfile->is_valid())
	throw FileError("could not create file " + filename);

    ncfile->set_fill(NcFile::Fill);

    //
    // time, station, and idLen dimensions
    //
    timeDim = ncfile->add_dim("time");
    if (! timeDim->is_valid())
	throw FileError("creating 'time' dimension");

    NcDim* stationDim = ncfile->add_dim("station", needStations.size());
    if (! stationDim->is_valid())
	throw FileError("creating 'stations' dimension");

    NcDim* idLenDim = ncfile->add_dim("idLen", 16); // aribtrary size choice
    if (! idLenDim->is_valid())
	throw FileError("creating 'idLen' dimension");

    //
    // period attribute
    //
    char p_string[40];
    sprintf(p_string, "%d minutes", period / 60);
    if (! ncfile->add_att(DATA_PERIOD_ATT.c_str(), p_string))
	throw FileError("adding '" + DATA_PERIOD_ATT + "' attribute");

    //
    // time var
    //
    timeVar = ncfile->add_var("time", ncDouble, timeDim);
    if (! timeVar->is_valid())
	throw FileError("creating 'time' var");
    if (! timeVar->add_att("units", "seconds since 1970-01-01 0:00:00 00:00"))
	throw FileError("adding 'units' attribute to 'time'");
    
    //
    // station vars
    //
    NcVar* stidvar = ncfile->add_var("station", ncChar, 
				     stationDim, idLenDim);
    if (! stidvar->is_valid())
	throw FileError("creating 'stationId' var");

    NcVar* latvar = ncfile->add_var("latitude", ncFloat, stationDim);
    if (! latvar->is_valid())
	throw FileError("creating 'latitude' var");

    NcVar* lonvar = ncfile->add_var("longitude", ncFloat, stationDim);
    if (! lonvar->is_valid())
	throw FileError("creating 'longitude' var");

    NcVar* altvar = ncfile->add_var("altitude", ncFloat, stationDim);
    if (! altvar->is_valid())
	throw FileError("creating 'altitude' var");

    //
    // data vars
    //
    for (int i = 0; i < outVars.size(); i++)
    {
	const string varname = outVars[i]->getName();
	NcVar* var = ncfile->add_var(varname.c_str(), ncFloat, timeDim,
				     stationDim);
	if (! var->is_valid())
	    throw FileError("creating '" + varname + "' var");

	const string units = outVars[i]->getUnitsName();
	if (! var->add_att("units", units.c_str()))
	    throw FileError("adding 'units' attribute to '" + varname + "'");

	//
	// treat fill and missing values the same
	//
	if (! var->add_att("missing_value", NC_FILL_FLOAT))
	    throw FileError("adding 'missing_value' attribute to '" + 
			    varname + "'");

	varmap[varname] = var;
    }

    //
    // store the station data
    //
    StationMapIterator si;
    int stindex;
    
    for (si = needStations.begin(), stindex = 0; si != needStations.end(); 
	 si++, stindex++)
    {
	const Station* s = si->second;

	stations[s->getName()] = new Station(*s);
	stationIndices[s->getName()] = stindex;

	string name = s->getName();
	stidvar->set_cur(stindex, 0);
	stidvar->put(name.c_str(), 1, name.length() + 1);

	float lat = s->getLat();
	latvar->set_cur(stindex);
	latvar->put(&lat, 1);
	
	float lon = s->getLon();
	lonvar->set_cur(stindex);
	lonvar->put(&lon, 1);
	
	float alt = s->getAlt();
	altvar->set_cur(stindex);
	altvar->put(&alt, 1);
    }
    ncfile->sync();
}


void
zebraMesonetOutFile::putVar(const Station* s, string varname, double time, 
			    float val) 
    throw (FileError)
{
    //
    // Get the index of the sample closest to the given time
    //
    if (! containsTime(time))
	throw FileError("putVar: given time does not belong in this file");
    
    int sample = ((int)(time - startTime) + (period / 2 - 1)) / period;

    //
    // Find the station
    //
    if (! stations[s->getName()])
	throw FileError("putVar: no station '" + s->getName() + "'");
    int stindex = stationIndices[s->getName()];
    
    //
    // Find the var
    //
    NcVar* var = varmap[varname];
    if (! var)
	throw FileError("putVar: non-existent var '" + varname + "'");
    //
    // Fill in unset times up to and including the one for this
    // sample
    //
    for (int ti = timeDim->size(); ti <= sample; ti++)
    {
	double sampletime = startTime + period * ti;

	timeVar->set_cur(ti);
	timeVar->put(&sampletime, 1);
    }
    //
    // Put the value
    //
    var->set_cur(sample, stindex);
    if (! var->put(&val, 1, 1))
	throw FileError("putVar: error writing var '" + varname + "'");

    ncfile->sync();
}


void
zebraMesonetOutFile::checkFile(StationMap& needStations, floatVarList& outVars)
    throw (FileError)
{
    //
    // verify the data period
    //
    char p_string[40];
    sprintf(p_string, "%d minutes", period / 60);
    char* fileperiod = ncfile->get_att(DATA_PERIOD_ATT.c_str())->as_string(0);
    if (strcmp(fileperiod, p_string))
	throw FileError(DATA_PERIOD_ATT + " of " + filename + " is not " +
			p_string);
    //
    // get the station list from the file
    //
    getStations();

    StationMapIterator nsi;
    for (nsi = needStations.begin(); nsi != needStations.end(); nsi++)
    {
	const Station* needsta = nsi->second;
	if (stations.find(needsta->getName()) == stations.end())
	    throw FileError("station '" + needsta->getName() + 
			    "' not defined in " + filename);
    }
    //
    // Ditto for vars
    //
    timeVar = ncfile->get_var("time");
    if (! timeVar || ! timeVar->is_valid())
	throw FileError("no 'time' var in " + filename);

    timeDim = timeVar->get_dim(0);
    if (! timeDim || ! timeDim->is_valid())
	throw FileError("bad time dimension in " + filename);
    
    for (int i = 0; i < outVars.size(); i++)
    {
	string varname = outVars[i]->getName();
	NcVar* var = ncfile->get_var(varname.c_str());
	if (! var || ! var->is_valid())
	    throw FileError("var '" + varname + "' not defined in " + 
			    filename);
	
	varmap[varname] = var;
    }
}


void
zebraMesonetOutFile::getStations()
    throw (FileError)
{
    //
    // Get the "station" dim
    //
    NcDim* stdim = ncfile->get_dim("station");
    if (!stdim)
	throw FileError("no 'station' dim in " + filename);
    //
    // Get the "station" var and its data
    //
    NcVar* stvar = ncfile->get_var("station");
    if (!stvar || !(stvar->num_dims() == 2) || !(stvar->get_dim(0) == stdim))
	throw FileError("bad 'station' var in " + filename);

    int nSta = stdim->size();
    int idLen = stvar->get_dim(1)->size();

    char* idData = new char[nSta * idLen];
    if (! stvar->get(idData, nSta, idLen))
	throw FileError("station get failed");

    //
    // latitude, longitude, altitude
    //
    NcVar* var;

    if (! (var = ncfile->get_var("latitude")) || ! (var->num_dims() == 1) ||
	! (var->get_dim(0) == stdim))
	throw FileError("bad or nonexistent 'latitude' var in " + filename);
    float* lats = new float[nSta];
    var->get(lats, nSta);

    if (! (var = ncfile->get_var("longitude")) || ! (var->num_dims() == 1) ||
	! (var->get_dim(0) == stdim))
	throw FileError("bad or nonexistent 'longitude' var in " + filename);
    float* lons = new float[nSta];
    var->get(lons, nSta);

    if (! (var = ncfile->get_var("altitude")) || ! (var->num_dims() == 1) ||
	! (var->get_dim(0) == stdim))
	throw FileError("bad or nonexistent 'altitude' var in " + filename);
    float* alts = new float[nSta];
    var->get(alts, nSta);

    //
    // Build the station map
    //
    for (int s = 0; s < nSta; s++)
    {
	char* name = idData + s * idLen;
	stations[name] = new Station(name, lats[s], lons[s], alts[s]);
	stationIndices[name] = s;
    }
}
