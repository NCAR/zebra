/*
 * Get rid of bad time_offsets in a netcdf file.
 */
static char *rcsid = "$Id: fix_cdf.c,v 1.1 1993-06-25 10:20:39 granger Exp $";
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include <config.h>
# include <copyright.h>
# include <stdio.h>
# include <defs.h>
# include "netcdf.h"

# define STRLEN		80
# define MAXRECORDS	10000
# define MISSING	0

main (argc, argv)
int	argc;
char	**argv;
{
	int	i;
	int	cdfid;
	int	timeid;
	int	num_records;
	long	start[1], count[1];
	float	time_offset[1];
/*
 * Check arguments.
 */
	if (argc < 2)
	{
		printf ("Usage: fix_cdf <input file>\n");
		exit (0);
	}
/*
 * Open the input file.
 */
	cdfid = ncopen (argv[1], NC_WRITE);
/*
 * How many records in this file?
 */
	ncdiminq (cdfid, ncdimid (cdfid, "time"), (char *) 0, &num_records); 
	if (num_records > MAXRECORDS)
	{
		printf ("Error: Number of records %d greater than max %d.\n",
			num_records, MAXRECORDS);
		exit (0);
	}
/*
 * Get the id for time_offset.
 */
	timeid = ncvarid (cdfid, "time_offset");	
/*
 * Read in the time_offset data.
 */
	ncvarget (cdfid, timeid, start, count, time_offsets);
/*
 * Search for missing values and interpolate.
 */
	for (i = 1; i < num_records; i++)
	{
		start[0] = i - 1;
		count[0] = i;
	}
/*
 * Store the lat/lon data.
 */
	ncvarput (cdfid, latid, start, count, latitudes);
	ncvarput (cdfid, lonid, start, count, longitudes);
	ncclose (cdfid);
}
