/*
 * $Id: windsname.c,v 1.1 1992-12-09 23:18:20 granger Exp $
 *
 * Writes the "winds_filename" global attribute of a netCDF file to stdout.
 * Intended to be used in a script via the backquote operators:
 *
 * foreach f (*.cdf)
 * 	mv $f `windsname $f`.cdf
 * end
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>



int
main(argc, argv)
	int argc;
	char *argv[];
{
	int cdfid;
	nc_type att_type;
	long att_len;
	char att_val[128];

	/*
	 * Get a file name from the command line
	 */
	if (argc < 2)
	{
		fprintf(stderr,"Usage: %s <netCDF file>\n", argv[0]);
		exit(1);
	}

	cdfid = ncopen(argv[1], NC_NOWRITE);
	ncattget(cdfid, NC_GLOBAL, "winds_filename", att_val);
	printf("%s\n",att_val);

	/*
	 * Done
	 */
	return(0);
}

