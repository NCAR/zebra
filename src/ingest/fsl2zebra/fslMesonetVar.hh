# ifndef _FSLMESONETVAR_HH_
# define _FSLMESONETVAR_HH_

# include <string>
# include "udunits.hh"


class NcFile;

class fslMesonetVar 
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

    inline int getNVals() const { return nVals; }
    inline string getName() const { return name; }
protected:
    fslMesonetVar(NcFile& infile, string varname) throw (VarError, NoSuchVar);

    string name;
    int nVals;
    NcVar* ncVar;
};


class unitfulFslMesonetVar : public fslMesonetVar
{
public:
    inline const utUnit* getUnits() const { return units; }
protected:
    unitfulFslMesonetVar(NcFile& infile, string varname) 
	throw (VarError, NoSuchVar);
    utUnit* units;
};


class doubleFslMesonetVar : public unitfulFslMesonetVar 
{
public:
    doubleFslMesonetVar(NcFile& infile, string varname) 
	throw (VarError, NoSuchVar);
    inline const double* getVals() const { return vals; }
    inline bool isBogus(double val) const
    { 
	return (val == fill || val == missing);
    };
    inline double fillValue() const { return fill; }
    inline double missingValue() const { return missing; }
private:
    double* vals;
    double fill;
    double missing;
};


class floatFslMesonetVar : public unitfulFslMesonetVar 
{
public:
    floatFslMesonetVar(NcFile& infile, string varname) 
	throw (VarError, NoSuchVar);
    inline const float* getVals() const { return vals; }
    inline bool isBogus(float val) const
    { 
	return (val == fill || val == missing);
    };
    inline float fillValue() const { return fill; }
    inline float missingValue() const { return missing; }
private:
    float* vals;
    float fill;
    float missing;
};


class stringFslMesonetVar : public fslMesonetVar
{
public:
    stringFslMesonetVar(NcFile& infile, string varname) 
	throw (VarError, NoSuchVar);
    inline int getMaxLen() const { return maxLen; }
    inline const string* getVals() const { return vals; }
private:
    int maxLen;
    string* vals;
};

# endif // ndef _FSLMESONETVAR_HH_
