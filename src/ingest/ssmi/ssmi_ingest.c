/*
 * Ingest module for SSM/I satellite format tapes.  Reads blocks from tape
 * and sends them off to Remote Sensing System's (RSS) FORTRAN subroutine
 * for extracting data, DECODE.  The C interface is the function decode_ssmi()
 * in another file, which maps a structure, type OUTDAT_BLOCK, onto DECODE's
 * common block OUTDAT.
 */
/*
 *		Copyright (C) 1993 UCAR
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

#ifndef lint
static char *rcsid = 
	"$Id: ssmi_ingest.c,v 1.1 1993-06-10 02:30:16 granger Exp $";
#endif

#include <time.h>
#include <math.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <defs.h>
#include <message.h>
#include <DataStore.h>
#include <ingest.h>

#include "outdat.h"
#include "ssmi_record.h"
#include "scan.h"

#define BADVAL	-9999.0
#define DEG_TO_RAD(x)	((x)*(double)0.017453293)
#define PLATFORM "ssmi"
#define DC_BLOCK_SIZE	8

#ifdef notdef
static FieldId *DeclareFields FP(( int *nfields ));
static DataChunk *InitializeDC FP(( char *platform, 
				   int nfields, FieldId *field_ids));
#endif

static int NextRec FP(( int fd, SSMI_LogicalRec *lrecs, int *nlogical ));
static void SetupFields ();
static void ProcessTape FP((int tape_fd, SSMI_LogicalRec *lrecs));
static DataChunk *CreateDC ();
static int ScanWithinLimits FP((OUTDAT_BLOCK *dat));
static void BuildImage FP((SquareScan *ss, DataChunk *dc));
static void FillGrid FP((float *grid, SquareScan *ss, 
			 GridMap *gm, Channel ch));

/*
 * Large buffer data that is best left global, holds one physical record
 * and 16 logical records
 */
SSMI_LogicalRec LRecs[16];


struct SSMI_Field {
	char *name;
	char *long_name;
	char *units;
	Channel channel;
} Fields[] = {
	{ "ta19v", "Antenna temperature, 19 GHz, vert", "Kelvin", ch19v },
	{ "ta19h", "Antenna temperature, 19 GHz, hoirz", "Kelvin", ch19h },
	{ "ta22v", "Antenna temperature, 22 GHz, vert", "Kelvin", ch22v },
	{ "ta37v", "Antenna temperature, 37 GHz, vert", "Kelvin", ch37v },
	{ "ta37h", "Antenna temperature, 37 GHz, horiz", "Kelvin", ch37h },
	/*
	 * The five low-freq channels should be first, followed by the
	 * two high-freq channels.
	 */
	{ "ta85v", "Antenna temperature, 85 GHz, vert", "Kelvin", ch85v },
	{ "ta85h", "Antenna temperature, 85 GHz, horiz", "Kelvin", ch85h }
};
#define NUM_FIELDS (sizeof(Fields)/sizeof(Fields[0]))

FieldId Fids[ NUM_FIELDS ];
	
/*
 * Let's talk strategy:
 *

 * We'll ingest these fields into a grid: ta19v, ta19h, ta22v, ta37v,
 * ta37h, ta85v, ta85h, and sfcidx.aaaa The 'ta' fields are all antenna
 * temperatures, 'sfcidx' is the surface-type index.  At some point we may
 * want to ingest 'tb' (brightness temperatures) as well.

 *

 * The two 85 Ghz channels have twice the resolution of the 5 lower
 * frequency channels.  This means two different RGrid info structures, one
 * for the low channels and one for the high.

 *

 * Use a DCC_Image class DataChunk.  For each low-frequency field,
 * calculate a scale and offset for the field: the minimum value is the
 * offset, the scale large enough to reduce the maximum value to less than
 * 256.  Ditto for the two high-frequency fields.  The sfcidx needs no
 * scaling, but it should be verified to be positive.

 *

 * We cannot possibly ingest every image, and we're only interested in
 * those near Darwin.  So define a lat/lon range and reject all images for
 * which xlatsc/xlonsc fall outside the range.

 *

 * To map the image pixels (given lat/lon coords) to the grid: Loop through
 * the elements of the grid, get lat/lon for the lower left and upper right
 * corners of the grid cell, and find the image pixel which falls inside
 * the grid cell.  Use the pixel value for the grid value.

 *	
 */


static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s [ingest options] tapedev\n", prog);
	IngestUsage ();
}
	

int
main (argc, argv)
int	argc;
char	**argv;
{
	int i;
	char ourname[20];
	int tape_fd;
	int nlog;
	SSMI_LogicalRec *lrec;
/*
 * Get our program-specific options
 */
#ifdef notdef
	i = 1;
	while (i < argc)
	{
		if (!strcmp(argv[i], "-p") && (i+1 < argc))
		{
			platform = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else
			++i;
	}
#endif
/*
 * Parse general ingest options
 */
	IngestParseOptions (&argc, argv, Usage);
/*
 * See if we have the right arguments remaining
 */
	if (argc != 2)
	{
		Usage (argv[0]);
		exit (1);
	}
/*
 * Open the tape device
 */
	if ((tape_fd = open (argv[1], O_RDONLY)) < 0)
	{
		printf ("Error %d opening device '%s'\n", errno, argv[1]);
		exit (1);
	}
/*
 * Dump the header to stdout, just for the user's benefit
 */
	printf ("\n\nTAPE HEADER\n-----------\n\n");

	NextRec (tape_fd, LRecs, &nlog);
	lrec = &LRecs[0];
	for (i = 0; i < nlog; i++)
		printf ("%.79s\n", (char *)(lrec + i));

	/* Get rid of the EOF after the header */
	NextRec (tape_fd, LRecs, &nlog);
/*
 * Connect to the data store, message, etc. Get FieldId's for our fields
 */
	sprintf (ourname,"SSMI Ingest");
	IngestInitialize (ourname);
	SetupFields ();
/*
 * Start poring through the data records from the tape
 */
	ProcessTape (tape_fd, LRecs);

	return (0);
}



static void
SetupFields()
{
	int i;

	for (i = 0; i < NUM_FIELDS; ++i)
	{
		Fids[i] = F_DeclareField (Fields[i].name,
					  Fields[i].long_name,
					  Fields[i].units);
	}
}



static void
ProcessTape (fd, lrecs)
int fd;
SSMI_LogicalRec *lrecs;
/*
 * Read one physical record after another, combining scan pairs in 
 * logical records into SquareScan's and converting the squares into
 * RGrid DataChunk's
 */
{
	SquareScan square;
	DataChunk *dc;
	int scan_count;		/* Number consecutive valid scans read */
	int nlog;
	int i;

	dc = CreateDC();
	SqClear (&square);
	scan_count = 0;
	while (NextRec (fd, lrecs, &nlog))
	{
		/*
		 * Process the individual scans in each logical record
		 */
		for (i = 0; i < nlog; i++)
		{
			decode_ssmi (1, 0, 1, i+1, lrecs+i);
			/*
			 * If this scan is within limits, accept it
			 */
			if (ScanWithinLimits (C_OUTDAT))
			{
				++scan_count;
				SqAddLRec (&square, C_OUTDAT);
				if (SqIsFull(&square) && scan_count % 64 == 0)
					BuildImage (&square, dc);
			}
			else
			{
				if (SqIsFull(&square))
					BuildImage (&square, dc);
				scan_count = 0;
				SqClear (&square);
			}
			if (dc_GetNSample(dc) > DC_BLOCK_SIZE)
			{
				ds_StoreBlocks (dc, FALSE, NULL, 0);
				dc_DestroyDC (dc);
				dc = CreateDC ();
			}
		}
	}
	dc_DestroyDC (dc);

	exit (0);
}




static int
NextRec (fd, lrecs, nlog)
int fd;
SSMI_LogicalRec *lrecs;
int *nlog;
/*
 * Read the next physical record from the tape into prec and put the
 * number of logical records into Nlog.  Return true for a good read,
 * false for an error or EOF.
 */
{
	int size;

	size = read (fd, (char *)lrecs, 16 * sizeof(SSMI_LogicalRec));
/*
 * Check for error or EOF
 */
	if (size <= 0)
	{
		if (size < 0)
			printf ("Error %d reading tape\n", errno);
		return (FALSE);
	}
	
	*nlog = size / sizeof (SSMI_LogicalRec);
	return (TRUE);
}



static int
ScanWithinLimits (dat)
OUTDAT_BLOCK *dat;
/*
 * Determine if this scan within lat/lon limits in which we're
 * interested in, using the xlatsc and xlonsc coords of block
 */
{
	/*
	 * Our current limits are 5 degrees E, W, N, and S of the
	 * Darwin radar site at Berrimah.
	 */
	if ((fabs(dat->xlatsc - (-12.45722)) <= 5.0) &&
	    (fabs(dat->xlonsc - (130.92528)) <= 5.0))
		return (TRUE);
	else
		return (FALSE);
}



static DataChunk *
CreateDC ()
{
DataChunk 	*dc;
/*
 * Create the data chunk and put in the platform ID, field IDs, and
 * the bad value flag (even though its never used)
 */
	dc = dc_CreateDC (DCC_RGrid);
	if (!dc)
		return (NULL);

	if ((dc->dc_Platform = ds_LookupPlatform (PLATFORM)) == BadPlatform)
	{
		IngestLog (EF_EMERGENCY, "Cannot get platform ID for '%s'",
			   PLATFORM);
		dc_DestroyDC (dc);
		return (NULL);
	}

	dc_RGSetup (dc, NUM_FIELDS, Fids);
	dc_SetBadval (dc, BADVAL);
	return (dc);
}



static void
BuildImage (ss, dc)
SquareScan *ss;
DataChunk *dc;
/*
 * Create grid maps, fill a grid of values, and add it to the
 * DataChunk.  The grid and the grid mapping will be different for the 
 * low-frequency channels: the grid is only 64x64 instead of 128x128 
 * for the two high-frequency channels.
 */
{
	GridMap gm;	/* Grid map used for lo-freq and re-used for high */
	RGrid info;	/* info on the grid we're creating		  */
	Location origin;/* Origin of our grid				  */
	ZebTime zt;	/* Time of the sample grid			  */
	int sample;	/* Sample of DataChunk we are creating		  */
	float *grid;	/* Space for grid values for this sample	  */
	int i;

	/*
	 * Establish an order for our scans
	 */
	SqOrder (ss);

	/*
	 * Start with low-frequency channels, 64x64 grid, 25km resolution
	 */
	info.rg_Xspacing = 25.0;
	info.rg_Yspacing = 25.0;
	info.rg_Zspacing = 0.0;
	info.rg_nX = 64;
	info.rg_nY = 64;
	info.rg_nZ = 1;

	/*
	 * Now just build our grid map by specifying a low-freq channel.
	 * The grid map will be the same for all 5 low-freq channels.
	 */
	BuildGridMap (ss, &gm, &info, ch19v);

	/*
	 * Allocate space for the grid values.  This memory is used for
	 * all channels.
	 */
	grid = (float *)malloc( 128*128*sizeof(float) );

	sample = dc_GetNSample(dc)+1;
	SqZebTime (ss, &zt);
	SqOrigin (ss, &origin);
	for (i = 0; i < 5; ++i)		/* The five lower frequencies */
	{
		FillGrid (grid, ss, &gm, Fields[i].channel);
		dc_RGAddGrid (dc, sample, Fids[i],
			      &origin, &info, &zt, grid, /*len*/ 0);
	}
	FreeGridMap (&gm);

	/*
	 * Now for high-frequency channels, 128x128 grid, 12.5 km resolution
	 */
	info.rg_Xspacing = 12.5;
	info.rg_Yspacing = 12.5;
	info.rg_Zspacing = 0.0;
	info.rg_nX = 128;
	info.rg_nY = 128;
	info.rg_nZ = 1;

	/*
	 * Now just build our grid map by specifying a high-freq channel.
	 */
	BuildGridMap (ss, &gm, &info, ch85v);

	for (i = 5; i < 7; ++i)		/* The two higher frequencies */
	{
		FillGrid (grid, ss, &gm, Fields[i].channel);
		dc_RGAddGrid (dc, sample, Fids[i],
			      &origin, &info, &zt, grid, /*len*/ 0);
	}

	FreeGridMap (&gm);
	free (grid);
}



static void
FillGrid (grid, ss, gm, ch)
float *grid;
SquareScan *ss;
GridMap *gm;
Channel ch;
/*
 * Given a square scan, a grid map, and a channel, fill in the grid
 * with the channel values from the square scan using the grid map.
 */
{
	int ix, iy;
	int scan, cell;

	for (iy = 0; iy < gm->gm_info.rg_nY; ++iy)
	{
		for (ix = 0; ix < gm->gm_info.rg_nX; ++ix)
		{
			GridToCell (gm, ix, iy, &scan, &cell);
			grid[ iy*(gm->gm_info.rg_nX) + ix ] = 
				SqCellValue (ss, scan, cell, ch);
		}
	}
}
