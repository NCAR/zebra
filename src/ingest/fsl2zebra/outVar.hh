# ifndef _OUTVAR_HH_
# define _OUTVAR_HH_

# include <string>
# include <vector>
# include <netcdf.hh>

# include "udunits.hh"
# include "fslMesonetVar.hh"


class outVar 
{
public:
    class VarError : public string 
    {
    public:
	VarError(const string& s = "") : string(s) {}
    };

    class NoSuchVar : public VarError
    {
    public:
	NoSuchVar(const string& s = "") : VarError(s) {}
    };

    inline const string getName() const { return name; }
    inline int getNVals() const { return nVals; }
protected:
    outVar(string outname);

    int nVals;
    string name;
};


class unitfulOutVar : public outVar
{
public:
    inline const utUnit* getUnits() const { return units; }
    inline const string getUnitsName() const { return unitsName; }
protected:
    unitfulOutVar(string outname, string unitsString) throw (VarError);
    void getConversion(const unitfulFslMesonetVar* fslVar, double* slope,
		       double* intercept) throw (VarError);

    string unitsName;
    utUnit* units;
};


class floatOutVar : public unitfulOutVar
{
public:
    floatOutVar(string outname, string units, NcFile& src, string inname)
	throw (VarError, NoSuchVar);
    inline float* getVals() const { return vals; }
    inline bool isMissing(float val) const { return(val == missingValue); }
private:
    static const float missingValue = NC_FILL_FLOAT;
    float* vals;
};

    
class doubleOutVar : public unitfulOutVar
{
public:
    doubleOutVar(string outname, string units, NcFile& src, string inname)
	throw (VarError, NoSuchVar);
    inline double* getVals() const { return vals; }
    inline bool isMissing(double val) const { return(val == missingValue); }
private:
    static const float missingValue = NC_FILL_DOUBLE;
    double* vals;
};


class stringOutVar : public outVar
{
public:
    stringOutVar(string outname, NcFile& src, string inname) 
	throw (VarError, NoSuchVar);
    inline const string* getVals() const { return fslVar->getVals(); }
    inline int getNVals() const { return fslVar->getNVals(); }
private:
    stringFslMesonetVar* fslVar;
};
	    
typedef vector<floatOutVar*> floatVarList;

# endif // ndef _OUTVAR_HH_
