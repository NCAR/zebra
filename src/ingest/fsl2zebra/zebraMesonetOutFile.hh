# ifndef _ZEBRAMESONETOUTFILE_HH_
# define _ZEBRAMESONETOUTFILE_HH_

# include <netcdf.hh>
# include "Station.hh"
# include "outVar.hh"

class zebraMesonetOutFile
{
public:
    class FileError : public string 
    {
    public:
	FileError(const string& s = "") : string(s) {}
    };

    zebraMesonetOutFile(long time, int dataPeriodMinutes, 
			StationMap& needStations, floatVarList& outVars) 
	throw (FileError);
    void putVar(const Station* s, string varname, double time, float val)
	throw (FileError);

    inline bool containsTime(double time) const 
    {
	return (time > (startTime - period / 2) && 
		time <= (endTime + period / 2));
    }

    inline static void setDir(string dir) { basedir = dir; }
private:
    static const string DATA_PERIOD_ATT;
    static string basedir;

    string filename;
    NcFile* ncfile;
    StationMap stations;
    map<string,int> stationIndices;	// map of station names to indices
    NcVar* timeVar;
    NcDim* timeDim;
    map<string,NcVar*> varmap;
    long startTime;	// time of first sample in the file
    long endTime;	// time of last sample in the file
    int period;		// time between samples

    void createFile(StationMap& needStations, floatVarList& outVars) 
	throw (FileError);
    void checkFile(StationMap& needStations, floatVarList& outVars) 
	throw (FileError);
    void getStations() throw (FileError);
};

# endif // ndef _ZEBRAMESONETOUTFILE_HH_

