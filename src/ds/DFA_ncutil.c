
#ifndef _Zebra_DFA_ncutil_c_
#define _Zebra_DFA_ncutil_c_

#include <netcdf.h>
#include <ctype.h>
#include <string.h>

/*
 * Global attribute names which are automatically stored with the file
 */
#define GATT_PLATFORM	"zebra_platform"/* the platform name */
#define GATT_HISTORY	"history"	/* creation info */
/*
 * Variable attributes retrieved for a field
 */
#define VATT_LONGNAME	"long_name"	/* description of variable */
#define VATT_UNITS	"units"		/* units of variable	   */
#define VATT_MISSING	"missing_value" /* bad value		   */
#define VATT_FTYPE	"field_type"	/* field type, for derivations */
#define VATT_IS_SIGNED	"is_signed"	/* signed boolean, for byte data */

#ifndef NCU_STATIC
#define NCU_STATIC static
#endif

/*
 * Defined in DFA_ncutil.c.
 */
NCU_STATIC void dnc_NCError (char *);
NCU_STATIC int	dnc_Varid (int id, char *name);
NCU_STATIC int	dnc_Dimid (int id, char *name);
NCU_STATIC int	dnc_DecipherTime (int id, int *vtime, int *dtime,
				  long *ntime, nc_type *, ZebraTime *base);
NCU_STATIC int  dnc_TimeUnits (ZebraTime *zt, const char *time_units);
NCU_STATIC char *dnc_ValueToString (void *value, nc_type type, int len);
NCU_STATIC char *dnc_GetStringAtt (int cdfid, int varid, 
				   char *att_name, char *att_val, int len);
NCU_STATIC int	dnc_MatchVarName (int id, int platid, const char *name, 
				  const char *longname, int pri);


#ifndef EF_PROBLEM
#include <stdio.h>
#define msg_ELog fprintf
#define EF_PROBLEM stderr
#define EF_DEBUG stdout
#define ENDL "\n"
#else
#define ENDL
#endif

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

static void
strtolower (char *c)
{
	while (*c)
	{
		*c = (int) tolower ((int)*c);
		++c;
	}
}


NCU_STATIC int
dnc_Varid (int id, char *name)
{
	return dnc_MatchVarName (id, -1, name, 0, 50);
}



NCU_STATIC int
dnc_Dimid (int id, char *name)
/*
 * Search the dimension names for a match which ignores case.
 * Return -1 on failure, otherwise the dimid of the matching dimension.
 */
{
	int ndim, nvar, natt, rdim;
	int dimid;
	long size;
	char target[MAX_NC_NAME+1];
	char dname[MAX_NC_NAME+1];

	if (strlen(name) > (unsigned)MAX_NC_NAME) /* can't match if too long */
		return (-1);
	strcpy (target, name);
	strtolower (target);
	if (ncinquire (id, &ndim, &nvar, &natt, &rdim) < 0)
		return (-1);
	for (dimid = 0; dimid < ndim; ++dimid)
	{
		ncdiminq (id, dimid, dname, &size);
		strtolower (dname);
		if (strcmp (dname, target) == 0)
			break;
	}
	return ((dimid < ndim) ? dimid : -1);
}



NCU_STATIC int
dnc_DecipherTime (
int id,
int *vtime,	/* varid of the time offset array variable */
int *dtime,	/* dimid of the dimension of the time variable */
long *ntime,	/* number of times in the time array */
nc_type *ttype,	/* type of the time variable */
ZebraTime *base_time)
/*
 * Inquire a netcdf file and search its variables for names which
 * match any of our expected time variable names.  Interpret variables
 * and units as necessary to determine the base time.
 *
 * The idea is to do all of that deciphering here so that we can do
 * things like detect mere differences in case or slight name tweaks.
 */
{
	int ndim, nvar, natt, rdim, tvar, btime;
	nc_type dtype, atype;
	int dims[MAX_VAR_DIMS];
	char time_units[256];
	int alen;
	ZebraTime zt;
	int base;
	long dimlen;

	if (ncinquire (id, &ndim, &nvar, &natt, &rdim) < 0)
		return (FALSE);
	if (((tvar = dnc_Varid (id, "time_offset")) < 0) && 
	    ((tvar = dnc_Varid (id, "time")) < 0))
	{
		return (FALSE);
	}
/*
 * Find out whether the time offsets are long, float, or double.
 */
	if (ncvarinq (id, tvar, (char *) 0, &dtype, &ndim, dims, &natt) < 0)
	{
		dnc_NCError ("inquiring time variable");
		return (FALSE);
	}
/*
 * We only handle certain time types
 */
	if (dtype != NC_FLOAT && dtype != NC_DOUBLE && dtype != NC_LONG)
	{
		dnc_NCError ("time must be type float, double, or long");
		return (FALSE);
	}
/*
 * For now we must require the time array to have only a single dimension,
 * and the number of times is the size of that dimension.  Note that the 
 * time dimension does _not_ need to be the record dimension.
 */
	if (ndim == 1) /* most common case */
	{
	    if (ncdiminq (id, dims[0], NULL, &dimlen) < 0)
	    {
		dnc_NCError ("inquiring time dimension");
		return (FALSE);
	    }
	}
	else if (ndim == 0) /* one time in the file and no time dimension */
	{
	    dims[0] = -1;	/* mark empty time dimension */
	    dimlen = 1;
	}
	else
	{
		dnc_NCError ("time variable must have zero or one dimension");
		return (FALSE);
	}
/*
 * The base time comes from the base_time variable if it exists, otherwise
 * from the units of the time variable. 
 */
	if ((btime = dnc_Varid (id, "base_time")) >= 0)
	{
		if (ncvarget1 (id, btime, 0, &base) < 0)
		{
			dnc_NCError ("getting base_time value");
			return (FALSE);
		}
	}
	else if (ncattinq (id, tvar, "units", &atype, &alen) < 0)
	{
		dnc_NCError ("no 'units' attribute for 'time'");
		return (FALSE);
	}
	else if ((atype != NC_CHAR) || (alen >= sizeof(time_units)))
	{
		dnc_NCError ("invalid units for 'time' variable");
		return (FALSE);
	}
	else if (ncattget (id, tvar, "units", (void *)time_units) < 0)
	{
		dnc_NCError ("could not get 'units' attribute for 'time'");
		return (FALSE);
	}
	else if (! dnc_TimeUnits (&zt, time_units))
	{
		return (FALSE);
	}
	else
		base = (int) zt.zt_Sec;
	if (vtime) *vtime = tvar;
	if (dtime) *dtime = dims[0];
	if (ntime) *ntime = dimlen;
	if (ttype) *ttype = dtype;
	if (base_time)
	{
	    base_time->zt_Sec = base;
	    base_time->zt_MicroSec = 0;
	}
	return (TRUE);
}



NCU_STATIC int
dnc_LocationID (int id, int platid, const char *name)
{
	return dnc_MatchVarName (id, platid, name, 0, 0);
}


NCU_STATIC int
dnc_MatchVarName (int id, int platid, const char *name, 
		  const char *longname, int pri)
/*
 * Try to find a location variable by the given name which includes the
 * given dimension 'platid'.  For the moment we'll require that the
 * requested dimension is the first dimension in the variable.  If platid
 * is negative, skip the dim id check.
 *
 * Need to consider adding variable type to the search, since many parts
 * of this interface assume a particular type (usually float, but sometimes
 * int and long) based only on the name.  If we find a variable of the
 * wrong type, garbage will ensue.  Someday we can take advantage of
 * the netcdf library's automatic conversion.
 */
{
    /*
     * Possible matches are weighted by priority, and the caller can
     * choose the minimum acceptable weight of a match, as follows:
     *
     * exact match, case sensitive:		100
     * exact match, case insensitive:		50
     * The rest are all case insensitive...
     * prefix match, longname substring:	30
     * suffix match, longname substring:	25
     * prefix match, no longname:		20
     * suffix match, no longname:		0
     * substr, longname substr:			-5
     * substr, no longname:			-10
     * 
     * Calling with a weight of zero gives a match with the default
     * amount of "reasonability".  Use less than zero if you're desperate.
     * Probably higher priority should be required when writing over
     * just reading data.
     *
     * Given two variables with equal weighting, the one with the lower id
     * will be chosen first.
     */
	int ndim, nvar, natt, rdim;
	int varid;
	nc_type dtype;
	nc_type atype;
	int alen;
	char target[MAX_NC_NAME+1];
	char ltarget[256];
	char vname[MAX_NC_NAME+1];
	char lname[1024];
	int dimids[MAX_VAR_DIMS];
	int match = -1;
	int weight = -100;

	if (strlen(name) > (unsigned)MAX_NC_NAME) /* can't match if too long */
		return (-1);
	strcpy (target, name);
	strtolower (target);
	if (! longname) longname = "";
	strcpy (ltarget, longname);
	strtolower (ltarget);
	if (ncinquire (id, &ndim, &nvar, &natt, &rdim) < 0)
		return (-1);
	/*
	 * Try to match the target name with either the beginning or
	 * end of the variable name, and make sure the variable contains
	 * the desired platform dimension.  Check all the variables, and
	 * use an exact name match (case-insensitive) if found, or the 
	 * first close match.
	 */
	for (varid = 0; varid < nvar; ++varid)
	{
		if (ncvarinq (id, varid, vname, &dtype, &ndim, dimids, 0) < 0)
			continue;
		if ((platid >= 0) && (ndim == 0 || dimids[0] != platid))
			continue;
		if (strcmp (vname, target) == 0)
		{
			/* exact matches immediately exit the loop */
			match = varid;
			weight = 100;
			break;
		}
		strtolower (vname);
		if (strcmp (vname, target) == 0)
		{
			/* next-closest match can just continue the loop */
			match = varid;
			weight = 50;
			continue;
		}
		/* Fetch the long_name attribute in lower case so we can
		 * match against it.
		 */
		lname[0] = '\0';
		if (ncattinq (id, varid, VATT_LONGNAME, &atype, &alen) >= 0
		    && (atype == NC_CHAR) 
		    && (alen < 1024))
		{
		    ncattget (id, varid, VATT_LONGNAME, (void *)lname);
		    /* Either the value contains a null, and this one is
		     * extra, or it doesn't, and this one is necessary.
		     */
		    lname[alen] = '\0';
		}
		strtolower (lname);
		if (! strncmp (vname, target, strlen(target)) &&
		    strstr (lname, ltarget) &&
		    weight < 30)
		{
			match = varid;
			weight = 30;
		}
		else if (((unsigned)strlen (vname) > strlen (target)) &&
			 ! strcmp (vname+strlen(vname)-strlen(target),
				   target) &&
			 strstr (lname, ltarget) &&
			 weight < 25)
		{
			match = varid;
			weight = 25;
		}
		/* same prefix and suffix tests above but with no longname */
		else if (! strncmp (vname, target, strlen(target)) &&
			 ! lname[0] &&
			 weight < 20)
		{
			match = varid;
			weight = 20;
		}
		else if (((unsigned)strlen (vname) > strlen (target)) &&
			 ! strcmp (vname+strlen(vname)-strlen(target),
				   target) &&
			 ! lname[0] &&
			 weight < 0)
		{
			match = varid;
			weight = 0;
		}
		else if (strstr (vname, target) && strstr (lname, ltarget) &&
			 weight < -5)
		{
			match = varid;
			weight = -5;
		}
		else if (strstr (vname, target) && ! lname[0] &&
			 weight < -10)
		{
			match = varid;
			weight = -10;
		}
	}
	/* Lastly, verify our match weight meets the threshold. */
	if (pri > weight)
		match = -1;
	if (match >= 0 && weight < 50 &&
	    ncvarinq (id, match, vname, &dtype, &ndim, dimids, 0) >= 0)
	{
		msg_ELog (EF_DEBUG, "using nc variable '%s' in place of '%s'"
			  ENDL, vname, target);
	}
	return (match);
}




NCU_STATIC int
dnc_TimeUnits (ZebraTime *zt, const char *time_units)
/*
 * Convert the udunits-style time_units string into a GMT time.
 * Return non-zero on success, zero otherwise.
 */
{
	char units[256], ref[256], zone[256];
	int year, month, day, hour, minute;
	float fsecond;
	char *colon;
	int second, msec;
	int nscan;

	/* default to zero seconds and GMT zone if not given */
	strcpy (zone, "0:00");
	fsecond = 0;
	nscan = sscanf (time_units, "%s %s %d-%d-%d %d:%d:%f %s",
			units, ref, &year, &month, &day, &hour, &minute, 
			&fsecond, zone);
	if (nscan != 1 && nscan < 7)
	{
	    msg_ELog (EF_PROBLEM, "bad syntax in time units" ENDL);
	    return (FALSE);
	}
	/*
	 * Can only accept seconds for now
	 */
	strtolower (units);
	if (strcmp (units, "seconds"))
	{
		msg_ELog (EF_PROBLEM, "time units must be 'seconds'" ENDL);
		return (FALSE);
   	}
	/*
	 * If all we got was a unit and no reference time, assume epoch
	 */
	if (nscan == 1)
	{
		msg_ELog (EF_DEBUG, 
			  "no reference time in units, assuming %s" ENDL,
			  "01-Jan-1970 00:00:00 GMT");
		zt->zt_Sec = 0;
		zt->zt_MicroSec = 0;
		return (TRUE);
	}
	strtolower (ref);
	if (strcmp (ref, "@") && strcmp (ref, "since") &&
	    strcmp (ref, "from") && strcmp (ref, "after") &&
	    strcmp (ref, "ref"))
	{
		msg_ELog (EF_PROBLEM,
			  "unknown reference string '%s'" ENDL, ref);
		return (FALSE);
	}
	/*
	 * Convert our float seconds into integer seconds and microseconds
	 */
	second = (int) fsecond;
	msec = (int) ((fsecond - (float)second) * 1e+6);
	TC_ZtAssemble (zt, year, month, day, hour, minute, second, msec);
	/*
	 * Translate to UTC: negative is west of prime meridian.
	 * Accept hh:mm, hh, hmm, or hhmm.
	 */
	if ((colon = strchr (zone, ':')) != NULL)
	{
		if (sscanf (zone, "%d:%d", &hour, &minute) != 2)
		{
			msg_ELog(EF_PROBLEM, 
				 "could not understand time zone" ENDL);
			return (FALSE);
		}
		zt->zt_Sec -= (hour * 3600) + 
			(minute * 60 * (hour < 0 ? -1 : 1));
	}
	else
	{
		if (sscanf (zone, "%d", &hour) != 1)
		{
			msg_ELog(EF_PROBLEM, 
				 "could not understand time zone" ENDL);
			return (FALSE);
		}
		if (hour < 100)
		{
			zt->zt_Sec -= hour * 3600;
		}
		else
		{
			int sign = (hour < 0 ? -1 : 1);
			hour = (hour < 0 ? -1*hour : hour);
			zt->zt_Sec -= sign * ((hour / 100) * 3600);
			zt->zt_Sec -= sign * ((hour % 100) * 60);
		}
	}
	return (TRUE);
}


NCU_STATIC void
dnc_NCError (char *s)
/*
 * Report a NETCDF error.
 */
{
/*
 * Print the error message
 */
	msg_ELog (EF_PROBLEM, "%s: NetCDF error %d, %s" ENDL, s, ncerr, 
		  nc_strerror (ncerr));
}



NCU_STATIC char *
dnc_ValueToString (void *value, nc_type type, int len)
/*
 * Convert the netCDF value of nctype 'type' to a string.
 * Returns the string, which is only valid until the next call.
 */
{
#	define BUFSIZE 256
	static char buf[BUFSIZE];
	char hold[60];
	int i;

	if (type == NC_CHAR)
	{
		strncpy (buf, (char*)value, len);
		buf[len] = '\0';
		return(buf);
	}
	buf[0] = '\0';
	for (i = 0; i < len; ++i)
	{
		switch(type)
		{
		   case NC_BYTE:
			sprintf(hold,"%hu ",
				(unsigned short)*(unsigned char *)value);
			break;
		   case NC_SHORT:
			sprintf(hold,"%hi ",*(short int *)value);
			break;
		   case NC_LONG:
			sprintf(hold,"%li ",*(long int *)value);
			break;
		   case NC_FLOAT:
			sprintf(hold,"%f ",*(float *)value);
			break;
		   case NC_DOUBLE:
			sprintf(hold,"%f ",*(double *)value);
			break;
		   default:
			return("unknown");
		}
		if (strlen(buf) + strlen(hold) > (unsigned)(BUFSIZE - 5))
		{
			strcat(buf, " ...");
			return(buf);
		}
		else
			strcat(buf, hold);
		value = (void *)((char *)value + nctypelen(type));
	}
	return(buf);
}




NCU_STATIC char *
dnc_GetStringAtt(int cdfid, int varid, char *att_name, char *att_val, int len)
/*
 * Retrieve named attribute from given varid, making sure value is of
 * type NC_CHAR and not longer than len.  Returns att_val if 
 * if successful, else returns NULL without changing att_val[].
 */
{
	nc_type att_type;
	int att_len;
	int saveopts = ncopts;

	ncopts = 0;
	if ((ncattinq (cdfid, varid, att_name, &att_type, &att_len) >= 0) 
	    && (att_len+1 < len) && (att_type == NC_CHAR))
	{
		ncopts = saveopts;
		ncattget (cdfid, varid, att_name, (void *)att_val);
		att_val[att_len] = '\0';
		return (att_val);
	}
	ncopts = saveopts;
	return (NULL);

}


#endif
