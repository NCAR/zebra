/*
 * Program to invert longitudes in a netCDF file. The longitude variable
 * must be defined in the file as "float lon(platform);" for this to work.
 */
/*		Copyright (C) 1993 by UCAR
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
/* $Id: netcdf_fixlon.c,v 1.2 1994-02-02 19:14:26 burghart Exp $ */
# include <netcdf.h>

main (argc, argv)
int	argc;
char	**argv;
{
	long	i, cdfid, lonid, nplat, start = 0;
	float	*lons;
/*
 * Make sure we got a file name, then try to open the file
 */
	if (argc != 2)
	{
		printf ("Usage: %s <netcdf_file>\n", argv[0]);
		exit (1);
	}

	cdfid = ncopen (argv[1], NC_WRITE);
/*
 * Get the id for "lon" and find out how many platforms we have
 */
	lonid = ncvarid (cdfid, "lon");
	ncdiminq (cdfid, ncdimid (cdfid, "platform"), (char *) 0, &nplat);
/*
 * Get the longitudes from the file and invert them
 */	
	lons = (float *) malloc (nplat * sizeof (float));
	ncvarget (cdfid, lonid, &start, &nplat, (void *) lons);

	for (i = 0; i < nplat; i++)
		lons[i] = -lons[i];
/*
 * Now put 'em back and we're done
 */
	ncvarput (cdfid, lonid, &start, &nplat, (void *) lons);
	free (lons);
	ncclose (cdfid);
}

