/*
 * Turn GOES area files into lat/lon grid data
 */
/*		Copyright (C) 1995 by UCAR
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

# include <unistd.h>
# include <errno.h>
# include <math.h>
# include <stdio.h>

# include "Area.h"

/*
 * $Id: GriddedGOES.c,v 1.4 2003-01-29 22:22:21 burghart Exp $
 */


int
main (argc, argv)
int argc;
char **argv;
/*
 * Ingest a GOES visible and/or IR image
 */
{
	AreaGrid ag;
	AreaFile *f;
	FILE	*cmdfile;
	unsigned char *grid;
/*
 * Open our command file
 */
	if (argc != 3)
	{
		fprintf (stderr, "Usage: %s <cmdfile> <areafile>\n", argv[0]);
		exit (1);
	}


	if (! (cmdfile = fopen (argv[1], "r")))
	{
		fprintf (stderr, "Error %d opening command file '%s'\n", errno,
			 argv[1]);
		exit (1);
	}
	InitGrid (&ag);
	ag.truncate = TRUE;
/*
 * Lat/lon limits
 */
	fscanf (cmdfile, "%f%f%f%f", &ag.minlat, &ag.minlon, 
		&ag.maxlat, &ag.maxlon);
/*
 * nx & ny
 */
	fscanf (cmdfile, "%d%d", &ag.gridX, &ag.gridY);
	if (ag.gridY <= 0 || ag.gridX <= 0)
	{
		fprintf (stderr, "Bad image size: %d x %d\n", 
			 ag.gridX, ag.gridY);
		exit (1);
	}
/*
 * Calculate the grid spacing
 */
	ag.latstep = (ag.maxlat - ag.minlat) / (ag.gridY - 1);
	ag.lonstep = (ag.maxlon - ag.minlon) / (ag.gridX - 1);
	ag.origin_lat = NO_ORIGIN;
/*
 * Process the file
 */
	f = AddFile (NULL, argv[2], "null");
	if (! (grid = DoFile (f, &ag, /*map*/NULL)))
	{
		fprintf (stderr, "Failed to create grid\n");
		exit (1);
	}
	CloseAreaFile (f);
	RemoveFile (NULL, f);
/*
 * Spit out the gridded data
 */
	if (fwrite (grid, ag.gridX * ag.gridY, 1, stdout) != 1)
	{
		fprintf (stderr, "Error %d writing the grid\n", errno);
		exit (1);
	}
	free (grid);
	exit (0);
}
	


