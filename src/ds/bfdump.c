/*		Copyright (C) 1987,88,89,90,91 by UCAR
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

# include <stdio.h>
# include <fcntl.h>

# include <defs.h>
# include "BoundaryFile.h"


/* $Id: bfdump.c,v 2.5 2002-02-12 23:40:38 granger Exp $ */


main (argc, argv)
int argc;
char **argv;
{
	int fd, i;
	struct BFHeader hdr;
	struct BFBTable *btable;
/*
 * Arg check
 */
	if (argc != 2)
	{
		fprintf (stderr, "usage: %s boundary_file\n", argv[0]);
		exit (9);
	}
/*
 * Open.
 */
	if ((fd = open (argv[1], O_RDONLY)) < 0)
	{
		perror (argv[1]);
		exit (1);
	}
/*
 * Pull in the header.
 */
	read (fd, &hdr, sizeof (hdr));
	printf ("Magic: 0x%x, platform '%s'\n", hdr.bh_Magic, hdr.bh_Platform);
	printf ("Max boundary %d, current %d, from %d %06d to %d %06d\n",
		hdr.bh_MaxBoundary, hdr.bh_NBoundary, hdr.bh_Begin.ds_yymmdd,
		hdr.bh_Begin.ds_hhmmss, hdr.bh_End.ds_yymmdd,
		hdr.bh_End.ds_hhmmss);
/*
 * Boundary table.
 */
	btable = (struct BFBTable *) malloc
			(hdr.bh_NBoundary * sizeof (struct BFBTable));
	read (fd, btable, hdr.bh_NBoundary * sizeof (struct BFBTable));
	for (i = 0; i < hdr.bh_NBoundary; i++)
		printf ("Bnd %2d, np %d, time %d %06d, offset %d\n", i,
			btable[i].bt_NPoint, btable[i].bt_Time.ds_yymmdd,
			btable[i].bt_Time.ds_hhmmss, btable[i].bt_Offset);
}

