/*
 * Dump out a raster file.
 */
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
# include <unistd.h>

# include <copyright.h>
# include <defs.h>
# include "RasterFile.h"

RCSID("$Id: rfdump.c,v 2.4 1995-06-29 21:37:40 granger Exp $")



int
main (argc, argv)
int argc;
char **argv;
{
	int fd, fld, ntocb, i;
	RFHeader hdr;
	RFToc *toc;

	if (argc != 2)
	{
		printf ("Usage: %s rasterfile\n", argv[0]);
		exit (1);
	}
/*
 * Open the file.
 */
	if ((fd = open (argv[1], 0)) < 0)
	{
		perror (argv[1]);
		exit (1);
	}
/*
 * Get the header.
 */
	if (read (fd, &hdr, sizeof (hdr)) != sizeof (hdr))
	{
		perror ("Header read error");
		exit (1);
	}
/*
 * Print it.
 */
	printf ("Header magic = 0x%x, platform '%s' %s\n", hdr.rf_Magic,
		hdr.rf_Platform,
		hdr.rf_Flags & RFF_COMPRESS ? "compressed" : "not compressed");
	printf ("Currently %d of max %d samples, with %d fields\n", 
		hdr.rf_NSample, hdr.rf_MaxSample, hdr.rf_NField);
	for (fld = 0; fld < hdr.rf_NField; fld++)
		printf ("\tField %d, '%s'\n", fld,hdr.rf_Fields[fld].rff_Name);
/*
 * Get and read the table of contents.
 */
	ntocb = hdr.rf_NSample*sizeof (RFToc);
	toc = (RFToc *) malloc (ntocb);
	if (read (fd, toc, ntocb) != ntocb)
	{
		perror ("TOC read error");
		exit (1);
	}
/*
 * Dump it out.
 */
	for (i = 0; i < hdr.rf_NSample; i++)
	{
		char attr[200];
		printf (
	     "%2d: %ld %06ld at %8ld, (%dx%d) space %.2f, L %.2f %.2f %.2f\n",
			i,
			toc[i].rft_Time.ds_yymmdd, toc[i].rft_Time.ds_hhmmss,
			toc[i].rft_Offset[0], toc[i].rft_Rg.rg_nX,
			toc[i].rft_Rg.rg_nY, toc[i].rft_Rg.rg_Xspacing,
			toc[i].rft_Origin.l_lat, toc[i].rft_Origin.l_lon,
			toc[i].rft_Origin.l_alt);
		if (toc[i].rft_AttrLen > 0)
		{
			lseek (fd, toc[i].rft_AttrOffset, 0);
			read (fd, attr, toc[i].rft_AttrLen);
			printf ("\tAttributes: '%s'\n", attr);
		}
	}
	return (0);
}
