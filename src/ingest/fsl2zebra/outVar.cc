# include <stdio.h>
# include "outVar.hh"

outVar::outVar(string outname)
{
    name = outname;
}


unitfulOutVar::unitfulOutVar(string outname, string unitsString)
    throw (VarError) :
    outVar(outname)
{
    utInit();

    units = new utUnit;
    unitsName = unitsString;
    if (utScan(unitsName.c_str(), units) != 0)
	throw VarError("Can't parse units '" + unitsName + "' for " + name);
}


void
unitfulOutVar::getConversion(const unitfulFslMesonetVar* fslVar, 
			     double* slope, double* intercept)
    throw (VarError)
{
    if (utConvert(fslVar->getUnits(), units, slope, intercept) != 0)
    {
	char* uname;
	utPrint(fslVar->getUnits(), &uname);
	string inUnitName(uname);
	utPrint(units, &uname);
	string outUnitName(uname);
	
	throw VarError("Can't convert '" + inUnitName + "' to '" +
		       outUnitName + "' for " + name);
    }
}


floatOutVar::floatOutVar(string outname, string outunits, NcFile& src, 
			 string inname)
    throw (VarError, NoSuchVar) :
    unitfulOutVar(outname, outunits)
{
    floatFslMesonetVar* fslVar;

    try 
    {
	fslVar = new floatFslMesonetVar(src, inname);
    } catch (fslMesonetVar::NoSuchVar nsv) {
	throw NoSuchVar(nsv);
    } catch (fslMesonetVar::VarError ve) {
	throw VarError(ve); // cast into outVar::VarError
    }
    

    double slope, intercept;
    getConversion(fslVar, &slope, &intercept);
    
    nVals = fslVar->getNVals();

    const float* fslVals = fslVar->getVals();
    vals = new float[nVals];

    for (int i = 0; i < nVals; i++)
    {
	if (fslVar->isBogus(fslVals[i]))
	    vals[i] = missingValue;
	else
	    vals[i] = fslVals[i] * slope + intercept;
    }
}


doubleOutVar::doubleOutVar(string outname, string outunits, NcFile& src, 
			   string inname)
    throw (VarError, NoSuchVar) :
    unitfulOutVar(outname, outunits)
{
    doubleFslMesonetVar* fslVar;

    try {
	fslVar = new doubleFslMesonetVar(src, inname);
    } catch (fslMesonetVar::NoSuchVar nsv) {
	throw NoSuchVar(nsv);
    } catch (fslMesonetVar::VarError ve) {
	throw VarError(ve); // cast into outVar::VarError
    }

    double slope, intercept;
    getConversion(fslVar, &slope, &intercept);
    
    nVals = fslVar->getNVals();

    const double* fslVals = fslVar->getVals();
    vals = new double[nVals];

    for (int i = 0; i < nVals; i++)
    {
	if (fslVar->isBogus(fslVals[i]))
	    vals[i] = missingValue;
	else
	    vals[i] = fslVals[i] * slope + intercept;
    }
}


stringOutVar::stringOutVar(string outname, NcFile& src, string inname)
    throw (VarError, NoSuchVar) :
    outVar(outname)
{
    try {
	fslVar = new stringFslMesonetVar(src, inname);
    } catch (fslMesonetVar::NoSuchVar nsv) {
	throw NoSuchVar(nsv);
    } catch (fslMesonetVar::VarError ve) {
	throw VarError(ve); // cast into outVar::VarError
    }
}
