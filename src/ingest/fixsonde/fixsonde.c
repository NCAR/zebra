
#include <stdio.h>
#include <netcdf.h>

/*
 * NOTES:

 X	Only add missing value attribute to floating point variables.

 X	No need to check for existing---it will be overwritten.

 X	Allow multiple files on command line.

 Check out why LowerRight died with AttrArray error.  See core file.

 */


main(argc, argv)
int argc;
char **argv;
{
	int arg, verbose;

	if (argc < 2)
	{
		fprintf (stderr, "usage: %s [-v] netcdf-file ...\n", argv[0]);
		exit (1);
	}

	verbose = 0;
	for (arg = 1; arg < argc; ++arg)
	{
		if (strcmp(argv[arg],"-v") == 0)
			verbose = 1;
		else
			fix (argv[arg], verbose);
	}
}



fix (file, verbose)
char *file;
int verbose;
{
	int ncid;
	int nvars;
	float badvalue = -9999.0;
	int i;
	nc_type datatype;
	int ndims, natts, recdim;
	char name[MAX_NC_NAME];

	ncid = ncopen(file, NC_WRITE);
	if (ncid < 0)
	{
		fprintf (stderr, "could not open %s", file);
		return ;
	}
	ncredef (ncid);
	ncinquire (ncid, &ndims, &nvars, &natts, &recdim);
	if (verbose)
		printf ("File '%s' opened, %d variables\n", file, nvars);

	for (i = 0; i < nvars; ++i)
	{
		ncvarinq (ncid, i, name, &datatype, NULL, NULL, NULL);
		if (datatype == NC_FLOAT)
		{
			if (verbose)
				printf ("   adding '%s=%g' to %s %s\n",
					"missing_value", badvalue,
					"variable", name);
			ncattput (ncid, i, "missing_value", NC_FLOAT, 1,
				  &badvalue);
		}
	}
	if (verbose)
		printf ("Re-writing and closing file '%s'...", file);
	fflush(stdout);
	ncclose (ncid);
	if (verbose)
		printf ("Done.\n");
}


