/*
 * $Id: acqual.c,v 1.2 1992-12-09 23:17:02 granger Exp $
 *
 * A quickly-written (hopefully) program to test aircraft netCDF files.  Tries to
 * verify several criteria:
 *
 * 1) Samples are continuous, at 1-second intervals, and in chronological
 *    order.
 *
 * 2) Lat, lon, and alt are within a specific range and differences between
 *    samples are within a certain tolerance.
 */


/* Operation:
 * ==========
 *
 * Read the time_offset, lat, lon, and alt variables from the netCDF file
 * directly into memory, checking each one with the appropriate criteria.
 */
/*
 * 12/4/92 -- Checks for bad_value_flag (excepts NC_CHAR-type) in global atts
 *	      and uses it to count the number of bad values found in the data
 */

#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>

#define ABS(x)	(((x)<0)?(-(x)):(x))

#undef DEBUG

#define YES (1)
#define NO  (0)

typedef struct s_param_t {
	char *field;
	float min;
	float max;
	float delta;
	short check_zero;	/* currently not used */
} param_t;

/*
 * Parameters are currently set for STORM-FEST region, i.e. NW Quartisphere, US
 */
param_t Parameters[] = 
{
	{ "lat",   25.0,  80.0, 0.05, YES },
	{ "lon", -110.0, -40.0, 0.05, YES },
	{ "alt",    0.0,  20.0, 0.10,  NO }
};

#define NUMBER(arr)	((unsigned long)(sizeof(arr)/sizeof(arr[0])))

#define AttBadValue	"bad_value_flag"

void GetBadValue();
void VerifyTimes();
void VerifyFloatVariable();
double atof();

float BadValue;
short UseBadValue;

void
Usage(argc, argv)
	int argc;
	char *argv[];
{
	int i;

	fprintf(stderr,"Check for problems with netCDF-format aircraft files.\n");
	fprintf(stderr,
	"Verify that time offsets are in chronological order at 1-second intervals\n");
	fprintf(stderr,"Usage: %s <netcdf_file> ...\n", argv[0]);
	fprintf(stderr,"Current field tolerances:\n");
	fprintf(stderr,"%-15s %-15s %-15s %-15s\n",
		"FIELD","MIN","MAX","DELTA");
	for (i = 0; i < NUMBER(Parameters); ++i)
		fprintf(stderr,"%-15s %-15f %-15f %-15f\n",
			Parameters[i].field, 
			Parameters[i].min,
			Parameters[i].max,
			Parameters[i].delta);
	exit(1);
}


void
Abort(msg)
	char *msg;
{
	fprintf(stderr,"** Fatal error: %s **\n", msg);
	exit (2);
}



int
main(argc, argv)
	int argc;
	char *argv[];
{
	int cdfid;
	int i,fld;

	/*
	 * Get a file name from the command line
	 */
	if (argc < 2)
		Usage(argc, argv);

	for (i = 1; i < argc; ++i)
	{
		/*
		 * Note that we'll use netCDF's default error handling and just
		 * exit with the error message on any netCDF errors
		 */
		printf("------------------------------------ %s\n",argv[i]);
		cdfid = ncopen(argv[i], NC_NOWRITE);
		
		/* 
		 * Set up our bad value handling 
		 */
		GetBadValue(cdfid);

		/*
		 * Now check time offsets
		 */
		VerifyTimes(cdfid);
		
		for (fld = 0; fld < NUMBER(Parameters); ++fld)
		{
			VerifyFloatVariable(cdfid, 
					    Parameters[fld].field,
					    Parameters[fld].min,
					    Parameters[fld].max,
					    Parameters[fld].delta);
		}
	}

	/*
	 * Done
	 */
	return(0);
}



void
GetBadValue(cdfid)
	int cdfid;
{
	nc_type att_type;
	long att_len;
	char *att_val;

	UseBadValue = 0;
	/* 
	 * Query for the "bad_value_flag" global attribute
	 */
	ncattinq(cdfid, NC_GLOBAL, AttBadValue, &att_type, &att_len);
	if (att_type != NC_CHAR)
		printf("%s attribute is not of type NC_CHAR, ignoring...\n",
		       AttBadValue);
	else
	{
		att_val = (char *)malloc(att_len); /* expects \0 to be in att_len */
		ncattget(cdfid, NC_GLOBAL, AttBadValue, att_val);
		UseBadValue = 1;
		BadValue = (float)atof(att_val);
		printf("Using %s = %f\n", AttBadValue, BadValue);
		free(att_val);
	}
}



void
VerifyTimes(cdfid)
	int cdfid;
{
	void *t;
	void *tptr;
	int t_id;
	nc_type t_type;
	int t_ndims;
	int t_natts;
	int t_dims[MAX_VAR_DIMS];
	unsigned long start;
	unsigned long count;
	double present;
	double past;
	unsigned long i;
	int found;

	printf("Verifying time offsets...\n");

	t_id = ncvarid(cdfid, "time_offset");

	ncvarinq(cdfid, t_id, (char *)0, &t_type, &t_ndims, t_dims, &t_natts);

	if (t_ndims != 1)
		Abort("time_offset has more than one dimension");

	/*
	 * Find out how many samples there are
	 */
	ncdiminq(cdfid, t_dims[0], (char *)0, &count);
#ifdef DEBUF
	printf("	%i offsets to check\n",count);
#endif
	if (count == 0)
	{
		printf("!!!! NO TIME OFFSETS PRESENT !!!!\n");
		return;
	}
	/*
	 * Allocate memory and get the samples
	 */
	start = 0;
	t = (void *)malloc(nctypelen(t_type) * count);
	ncvarget(cdfid, t_id, &start, &count, t);

	/*
	 * Have our data in t, now check it in a simple loop
	 */
	tptr = t;
	past = -1;
	found = 0;
	for (i = 0; i < count; ++i)
	{
		switch (t_type)
		{
		   case NC_DOUBLE:
			present = *(double *)tptr;
			tptr += sizeof(double);
			break;
		   case NC_FLOAT:
			present = (double)*(float *)tptr;
			tptr += sizeof(float);
			break;
		   default:
			Abort("time_offset is not of type float or double");
		}

		if (present != past + 1)
		{
			printf("!!!! At %i, offset %lf following offset of %lf\n",
			       i, present, past);
			found++;
		}
		past = present;
	}

	printf("Finished checking time offsets.  ");
	if (found)
		printf("Time problems found: %i\n", found);
	else
		printf("No problems.\n");

	free(t);
}


void
VerifyFloatVariable(cdfid, var_name, min, max, delta)
	int cdfid;
	char *var_name;
	float min;
	float max;
	float delta;
{
	float *vals;
	float *vptr;
	int var_id;
	nc_type var_type;
	int var_ndims;
	int var_natts;
	int var_dims[MAX_VAR_DIMS];
	unsigned long start;
	unsigned long count;
	unsigned long i;
	float past;
	int found, bad_count;

	printf("Verifying variable %s...\n", var_name);
#ifdef DEBUG
	printf("	%f <= %s <= %f\n	abs(delta) <= %f\n",
	       min, var_name, max, delta);
#endif

	var_id = ncvarid(cdfid, var_name);

	ncvarinq(cdfid, var_id, (char *)0, &var_type, &var_ndims, 
		 var_dims, &var_natts);

	if (var_ndims != 1)
		Abort("%s has more than one dimension",var_name);

	if (var_type != NC_FLOAT)
		Abort("%s is not of type float", var_name);

	/*
	 * Find out how many samples there are
	 */
	ncdiminq(cdfid, var_dims[0], (char *)0, &count);
#ifdef DEBUG
	printf("	%lu values for %s\n", count, var_name);
#endif
	if (count == 0)
	{
		printf("!!!! VARIABLE %s CONTAINS NO VALUES !!!!\n", var_name);
		return;
	}
	/*
	 * Allocate memory and get the samples
	 */
	start = 0;
	vals = (void *)malloc(sizeof(float) * count);
	ncvarget(cdfid, var_id, &start, &count, vals);

	/*
	 * Now we can loop through values and test tolerances
	 */
	found = 0;
	past = 0;
	bad_count = 0;
	for (i = 0, vptr = vals; i < count; ++i, ++vptr)
	{
		if (UseBadValue && (*vptr == BadValue))
		{
			++bad_count;
			past = *vptr;
			continue;
		}
		if ((*vptr > max) || (*vptr < min))
		{
			found++;
			printf("!! At %i, %s = %f is out of range\n",
			       i, var_name, *vptr);
		}
		if ((i) && ( !UseBadValue || (past != BadValue)) &&
		    (ABS(*vptr - past) > delta))
		{
			found++;
			printf("!! At %i, %s = %f, delta = %f > %f\n",
			       i, var_name, *vptr, (*vptr - past), delta);
		}
		past = *vptr;
	}

	printf("Finished checking %s, ",var_name);
	if (found || bad_count)
	{
		printf("\n\tproblems found: %i\n", found);
		printf(  "\t    bad values: %i\n",bad_count);
	}
	else
		printf("no problems\n");

	free(vals);
}
