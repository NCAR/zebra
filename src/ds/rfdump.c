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
# include <byteorder.h>
# include <message.h>
# include "RasterFile.h"

RCSID("$Id: rfdump.c,v 2.8 1997-06-19 20:19:32 granger Exp $")


static int DumpFile (char *file);


int
main (argc, argv)
int argc;
char **argv;
{
    int i;
    int status = 0;

    if (argc < 2)
    {
	printf ("Usage: %s rasterfile ...\n", argv[0]);
	exit (1);
    }

    msg_connect (NULL, "rfdump");
    for (i = 1; i < argc; ++i)
    {
        printf ("File: %s\n", argv[i]);
	status += DumpFile (argv[i]);
    }

    exit (status);
}




static int
DumpFile (char *file)
{
    int fd, fld, i;
    RFHeader hdr;
    RFToc *toc;
/*
 * Open the file.
 */
    if ((fd = open (file, 0)) < 0)
    {
	perror (file);
	return (-1);
    }
/*
 * Get the header.
 */
    if (drf_ReadHeader (fd, &hdr) < 0)
    {
	msg_ELog (EF_PROBLEM, "Read header failed for '%s'", file);
	close (fd);
	return (-1);
    }
/*
 * Print it.
 */
    printf ("Header magic = 0x%x, platform '%s' %s\n", 
	    hdr.rf_Magic, hdr.rf_Platform,
	    hdr.rf_Flags & RFF_COMPRESS ? 
	    "compressed" : "not compressed");
    printf ("Currently %d of max %d samples, with %d fields\n", 
	    hdr.rf_NSample, hdr.rf_MaxSample, 
	    hdr.rf_NField);
    for (fld = 0; fld < hdr.rf_NField; fld++)
	printf ("\tField %d, '%s'\n", fld, 
		hdr.rf_Fields[fld].rff_Name);
/*
 * Get and read the table of contents.
 */
    if (! (toc = drf_ReadTOC (&hdr, fd)))
    {
	    msg_ELog (EF_PROBLEM, "Read TOC failed for file '%s'", file);
	    close (fd);
	    return (-1);
    }
/*
 * Dump it out.
 */
    for (i = 0; i < hdr.rf_NSample; i++)
    {
	char attr[512];
	printf ("%2d: %ld %06ld at %8ld, (%dx%d) "
	        "space %.2f, L %.2f %.2f %.2f\n",
		i, toc[i].rft_Time.ds_yymmdd, 
		toc[i].rft_Time.ds_hhmmss,
		toc[i].rft_Offset[0], 
		toc[i].rft_Rg.rg_nX,
		toc[i].rft_Rg.rg_nY, 
		toc[i].rft_Rg.rg_Xspacing,
		toc[i].rft_Origin.l_lat, 
		toc[i].rft_Origin.l_lon,
		toc[i].rft_Origin.l_alt);
	if (toc[i].rft_AttrLen > 0)
	{
	    char *c;
	    int len = toc[i].rft_AttrLen;
	    lseek (fd, toc[i].rft_AttrOffset, 0);
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
    free (toc);
    return (0);
}


