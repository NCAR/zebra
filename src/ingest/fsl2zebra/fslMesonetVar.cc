# include <math.h>
# include <stdio.h>
# include <netcdf.hh>
# include "fslMesonetVar.hh"


fslMesonetVar::fslMesonetVar(NcFile& infile, string varname)
    throw (VarError, NoSuchVar)
{
    // verify validity of the NcFile
    if (! infile.is_valid())
	throw VarError("invalid netCDF file");

    // get the "recNum" dimension
    NcDim* recdim = infile.get_dim("recNum");
    if (! recdim)
	throw VarError("missing recNum dimension");

    nVals = recdim->size();

    // find the variable
    name = varname;
    ncVar = infile.get_var(name.c_str());
    if (! ncVar)
	throw NoSuchVar("no " + name + " variable");

    // make sure the var's first dimension is recNum
    NcDim* firstdim = ncVar->get_dim(0);
    if (firstdim != recdim)
	throw VarError("first dimension of " + name + "is not recNum");
}



unitfulFslMesonetVar::unitfulFslMesonetVar(NcFile& infile, string varname)
    throw (VarError, NoSuchVar) :
    fslMesonetVar(infile, varname)
{
    NcAtt* units_att;

    utInit();
    
    if ((units_att = ncVar->get_att("units")) == 0)
	throw VarError("variable " + name + " needs a 'units' attribute");

    units = new utUnit;
    if (utScan(units_att->as_string(0), units) != 0)
	throw VarError("Can't parse units '" + 
		       string(units_att->as_string(0)) + "' for " + name);
}


doubleFslMesonetVar::doubleFslMesonetVar(NcFile& infile, string varname)
    throw (VarError, NoSuchVar) :
    unitfulFslMesonetVar(infile, varname)
{
    vals = new double[nVals];
    if (! ncVar->get(vals, nVals))
	throw VarError("data get for " + name + " failed");

    NcAtt* missing_att;
    if ((missing_att = ncVar->get_att("missing_value")) != 0)
	missing = missing_att->as_double(0);
    else
	missing = HUGE_VAL; // default if unspecified

    NcAtt* fill_att;
    if ((fill_att = ncVar->get_att("_FillValue")) != 0)
	fill = fill_att->as_double(0);
    else
	fill = NC_FILL_DOUBLE;
}


floatFslMesonetVar::floatFslMesonetVar(NcFile& infile, string varname)
    throw (VarError, NoSuchVar) :
    unitfulFslMesonetVar(infile, varname)
{
    vals = new float[nVals];
    if (! ncVar->get(vals, nVals))
	throw VarError("data get for " + name + " failed");

    NcAtt* missing_att;
    if ((missing_att = ncVar->get_att("missing_value")) != 0)
	missing = missing_att->as_float(0);
    else
	missing = HUGE_VAL; // default if unspecified

    NcAtt* fill_att;
    if ((fill_att = ncVar->get_att("_FillValue")) != 0)
	fill = fill_att->as_float(0);
    else
	fill = NC_FILL_FLOAT;
}


stringFslMesonetVar::stringFslMesonetVar(NcFile& infile, string varname)
    throw (VarError, NoSuchVar) :
    fslMesonetVar(infile, varname)
{
    if (ncVar->num_dims() != 2)
	throw VarError("number of dims for charFslMesonetVar " +
			   name + " must be exactly 2");
    
    int idLen = ncVar->get_dim(1)->size();
    char* alldata = new char[idLen*nVals];
    if (! ncVar->get(alldata, nVals, idLen))
	throw VarError("data get for " + name + " failed");

    // keep the maximum string length, allowing for a terminating null
    maxLen = idLen - 1;

    vals = new string[nVals];
    for (int i = 0; i < nVals; i++)
    {
	vals[i] = alldata + i * idLen;
	if (vals[i].size() > maxLen)
	    throw VarError("string too long in " + name + " data");
    }

    delete[] alldata;
}

    
