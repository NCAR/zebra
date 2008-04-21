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
# include <sys/types.h>
# include <fcntl.h>
# include <unistd.h>
# include <errno.h>

# include <defs.h>
# include <byteorder.h>
# include <message.h>
# include "RasterFile.h"

RCSID("$Id: rfdump.c,v 2.11 2000-11-20 18:07:29 granger Exp $")


typedef struct s_RFile
{
	int fd;
	RFHeader hdr;
	RFToc *toc;
} RFile;

static int DumpFile (int fd, RFHeader *hdr, RFToc *toc);
static void CloseFile (RFile *rf);
static int OpenFile (char *file, RFile *rf);
static void WriteFile (RFile *src, RFile *dst, char *fname, int decompress);

static void Usage (char *p)
{
	printf ("Usage: %s [-help] [-decompress file] rasterfile ...\n", p);
	exit (1);
}

int
main (argc, argv)
int argc;
char **argv;
{
    int i;
    int status = 0;
    int decompress = 1; /* default is always decompress for now */
    char *dfile = 0;
    RFile rf;

    if (argc < 2)
    {
	Usage (argv[0]);
    }

    msg_connect (NULL, "rfdump");
    i = 1;
    while (i < argc)
    {
	int len = strlen (argv[i]);
	if (len > 1 && ! strncmp ("-help", argv[i], len))
	{
	    Usage (argv[0]);
	}
	else if (len > 1 && ! strncmp ("-decompress", argv[i], len))
	{
	    ++i;
	    if (i >= argc)
		    Usage (argv[0]);
	    dfile = argv[i];
	}
	else
	{
		RFile dst;
		printf ("File: %s\n", argv[i]);
		if (OpenFile (argv[i], &rf) >= 0)
		{
			if (dfile)
			{
				WriteFile (&rf, &dst, dfile, decompress);
				DumpFile (dst.fd, &dst.hdr, dst.toc);
				CloseFile (&dst);
			}
			else
			{
				status += DumpFile (rf.fd, &rf.hdr, rf.toc);
			}
			CloseFile (&rf);
		}
		dfile = 0;
	}
	++i;

    }

    exit (status);
}



static void
WriteFile (RFile *src, RFile *dst, char *fname, int decompress)
{
    int i, fld;

    if ((dst->fd = open (fname, O_RDWR | O_CREAT | O_TRUNC, 0664)) < 0)
    {
	msg_ELog (EF_PROBLEM, "Error %d opening '%s'", errno,
		  fname);
	return;
    }
    dst->hdr = src->hdr;
    dst->hdr.rf_Magic = RF_MAGIC;
    /*
     * Go ahead and trim the maxsamples to the actual number we need.
     */
    dst->hdr.rf_MaxSample = src->hdr.rf_NSample;
    dst->toc = (RFToc *) malloc (dst->hdr.rf_MaxSample * sizeof(RFToc));
    memcpy (dst->toc, src->toc, dst->hdr.rf_MaxSample * sizeof(RFToc));
    if (decompress)
    {
	    dst->hdr.rf_Flags &= ! RFF_COMPRESS;
    }
    drf_WriteHeader (dst->fd, &dst->hdr, dst->toc);
    /*
     * Now loop through the fields and write them, each will be
     * automatically decompressed and not re-compressed according to
     * the dst header.
     */
    for (i = 0; i < src->hdr.rf_NSample; i++)
    {
	int fnb = src->toc[i].rft_Rg.rg_nX * src->toc[i].rft_Rg.rg_nY;
	unsigned char *img;
	for (fld = 0; fld < src->hdr.rf_NField; fld++)
	{
	    dst->toc[i].rft_Offset[fld] = dst->toc[i].rft_Size[fld] = 0;
	    /* Get the field, then write it out. */
	    img = drf_GetField (src->fd, &src->hdr, src->toc+i, fld);
	    drf_WriteData (dst->fd, &dst->hdr, dst->toc+i, fld, img,
			   fnb, 0/*reuse*/);
	}
	/*
	 * We're not bothering to copy attributes just yet.
	 */
	dst->toc[i].rft_AttrLen = dst->toc[i].rft_AttrOffset = 0;
    }
    drf_WriteHeader (dst->fd, &dst->hdr, dst->toc);
}




static int
OpenFile (char *file, RFile *rf)
{
    int fd;
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
    rf->fd = fd;
    if (drf_ReadHeader (fd, &rf->hdr) < 0)
    {
	msg_ELog (EF_PROBLEM, "Read header failed for '%s'", file);
	close (fd);
	return (-1);
    }
/*
 * Get and read the table of contents.
 */
    if (! (rf->toc = drf_ReadTOC (&rf->hdr, fd)))
    {
	    msg_ELog (EF_PROBLEM, "Read TOC failed for file '%s'", file);
	    close (fd);
	    return (-1);
    }
    return fd;
}



static void
CloseFile (RFile *rf)
{
	close (rf->fd);
	free (rf->toc);
}



static int
DumpFile (int fd, RFHeader *hdr, RFToc *toc)
{
    int fld, i;
/*
 * Print the header.
 */
    printf ("Header magic = 0x%x, platform '%s' %s\n", 
	    hdr->rf_Magic, hdr->rf_Platform,
	    hdr->rf_Flags & RFF_COMPRESS ? 
	    "compressed" : "not compressed");
    printf ("Currently %d of max %d samples, with %d fields\n", 
	    hdr->rf_NSample, hdr->rf_MaxSample, 
	    hdr->rf_NField);
    for (fld = 0; fld < hdr->rf_NField; fld++)
	printf ("\tField %d, '%s'\n", fld, 
		hdr->rf_Fields[fld].rff_Name);
/*
 * Dump out the TOC.
 */
    for (i = 0; i < hdr->rf_NSample; i++)
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
	for (fld = 0; fld < hdr->rf_NField; fld++)
	    printf ("\tField %d, '%s' at offset: %ld\n", fld, 
		    hdr->rf_Fields[fld].rff_Name,(long)toc[i].rft_Offset[fld]);

	if (toc[i].rft_AttrLen > 0 && toc[i].rft_AttrLen < 512)
	{
	    char *c;
	    int len = toc[i].rft_AttrLen;
	    lseek (fd, toc[i].rft_AttrOffset, 0);
	    read (fd, attr, len);
	    attr[len] = '\0';
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
    return (0);
}


