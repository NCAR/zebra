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

# include <defs.h>
# include "RasterFile.h"

RCSID("$Id: rfdump.c,v 2.6 1997-05-09 05:19:28 granger Exp $")

long BE_long (long l);
float BE_float (float f);

static void DumpFile (char *file);


int
main (argc, argv)
int argc;
char **argv;
{
    int i;

    if (argc < 2)
    {
	printf ("Usage: %s rasterfile ...\n", argv[0]);
	exit (1);
    }

    for (i = 1; i < argc; ++i)
    {
        printf ("File: %s\n", argv[i]);
	DumpFile (argv[i]);
    }

    return (0);
}




static void
DumpFile (char *file)
{
    int fd, fld, ntocb, i;
    RFHeader hdr;
    RFToc *toc;
/*
 * Open the file.
 */
    if ((fd = open (file, 0)) < 0)
    {
	perror (file);
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
    printf ("Header magic = 0x%x, platform '%s' %s\n", 
	    BE_long (hdr.rf_Magic), hdr.rf_Platform,
	    BE_long (hdr.rf_Flags) & RFF_COMPRESS ? 
	    "compressed" : "not compressed");
    printf ("Currently %d of max %d samples, with %d fields\n", 
	    BE_long (hdr.rf_NSample), BE_long (hdr.rf_MaxSample), 
	    BE_long (hdr.rf_NField));
    for (fld = 0; fld < BE_long (hdr.rf_NField); fld++)
	printf ("\tField %d, '%s'\n", fld, 
		hdr.rf_Fields[fld].rff_Name);
/*
 * Get and read the table of contents.
 */
    ntocb = BE_long (hdr.rf_NSample) * sizeof (RFToc);
    toc = (RFToc *) malloc (ntocb);
    if (read (fd, toc, ntocb) != ntocb)
    {
	perror ("TOC read error");
	exit (1);
    }
/*
 * Dump it out.
 */
    for (i = 0; i < BE_long (hdr.rf_NSample); i++)
    {
	char attr[200];
	printf ("%2d: %ld %06ld at %8ld, (%dx%d) space %.2f, L %.2f %.2f %.2f\n",
		i, BE_long (toc[i].rft_Time.ds_yymmdd), 
		BE_long (toc[i].rft_Time.ds_hhmmss),
		BE_long (toc[i].rft_Offset[0]), 
		BE_long (toc[i].rft_Rg.rg_nX),
		BE_long (toc[i].rft_Rg.rg_nY), 
		BE_float (toc[i].rft_Rg.rg_Xspacing),
		BE_float (toc[i].rft_Origin.l_lat), 
		BE_float (toc[i].rft_Origin.l_lon),
		BE_float (toc[i].rft_Origin.l_alt));
	if (BE_long (toc[i].rft_AttrLen) > 0)
	{
	    char *c;
	    int len = BE_long (toc[i].rft_AttrLen);
	    lseek (fd, BE_long (toc[i].rft_AttrOffset), 0);
	    read (fd, attr, len);
	    printf ("\tAttributes:");
	    c = attr;
	    while (c - attr < len)
	    {
		printf (" %s=", c);
		c += strlen (c) + 1;
		printf ("%s; ", c);
		c += strlen (c) + 1;
	    }
	    printf ("\n");
	}
    }
}


long
BE_long (long l)
{
    char c;
    char *bl = (char *)&l;

    c = bl[0]; bl[0] = bl[3]; bl[3] = c;
    c = bl[1]; bl[1] = bl[2]; bl[2] = c;

    return l;
}



float
BE_float (float f)
{
    char c;
    char *bf = (char *)&f;

    c = bf[0]; bf[0] = bf[3]; bf[3] = c;
    c = bf[1]; bf[1] = bf[2]; bf[2] = c;

    return f;
}
