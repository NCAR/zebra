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
	"$Id: ssmi_ingest.c,v 1.2 1993-06-17 04:55:57 granger Exp $";
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

#define BADVAL		-9999.0		/* not really used, but set in DC */
#define DEG_TO_RAD(x)	((x)*(double)0.017453293)
#define PLATFORM 	"ssmi"		/* default platform name */
#define DC_BLOCK_SIZE	0		/* # samples before storing a DC  */
#define OURNAME		"SSMI IN"

static int NextRecs FP(( int fd, SSMI_LogicalRec *lrecs, int *nlogical ));
static void WriteRecs FP(( int fd, SSMI_LogicalRec *lrecs, int nlogical ));
static void SetupFields ();
static int ReadTape FP((int fd, SSMI_LogicalRec *lrecs, int *nlogical,
			int is_tape));
static int ProcessTape FP((int tape_fd, int echo_fd, SSMI_LogicalRec *lrecs,
			   int is_tape));
static DataChunk *CreateDC ();
static int ScanWithinLimits FP((SSMI_LogicalRec *lrec));
static int IsGarbage FP((OUTDAT_BLOCK *dat));
static void BuildImage FP((SquareScan *ss, DataChunk *dc));
static void FillGrid FP((unsigned char *image, SquareScan *ss, 
			 GridMap *gm, Channel ch));
static void Log FP((int flags, SSMI_LogicalRec *lrec, int nrec));

/*
 * Large buffer data that is best left global, holds one physical record
 * and 16 logical records
 */
SSMI_LogicalRec LRecs[16];
char 		*Platform = PLATFORM;

struct SSMI_Field {
	char *name;
	char *long_name;
	char *units;
	Channel channel;
} Fields[] = {
	{ "ta19v", "Antenna temperature, 19 GHz, v-pol", "Kelvin", ch19v },
	{ "ta19h", "Antenna temperature, 19 GHz, h-pol", "Kelvin", ch19h },
	{ "ta22v", "Antenna temperature, 22 GHz, v-pol", "Kelvin", ch22v },
	{ "ta37v", "Antenna temperature, 37 GHz, v-pol", "Kelvin", ch37v },
	{ "ta37h", "Antenna temperature, 37 GHz, h-pol", "Kelvin", ch37h },
	/*
	 * The five low-freq channels should be first, followed by the
	 * two high-freq channels.
	 */
	{ "ta85v", "Antenna temperature, 85 GHz, v-pol", "Kelvin", ch85v },
	{ "ta85h", "Antenna temperature, 85 GHz, h-pol", "Kelvin", ch85h },
	/*
	 * Surface indices
	 */
	{ "sfcidx", "Surface-type index", "none", sfcidx }
};
#define NUM_FIELDS (sizeof(Fields)/sizeof(Fields[0]))

FieldId Fids[ NUM_FIELDS ];

ScaleInfo Scales[ NUM_FIELDS ] = /* real value = data/s_Scale + s_Offset */
{
	{ 1.0, 200.0 },/* Assuming temperatures (K) from 200 (-73 C) to 456 */
	{ 1.0, 200.0 },
	{ 1.0, 200.0 },
	{ 1.0, 200.0 },
	{ 1.0, 200.0 },
	{ 1.0, 200.0 },
	{ 1.0, 200.0 },
	{ 1.0, 0.0 }   /* The surface-type index */
};


	
/*
 * Let's talk strategy:
 *

 * We'll ingest these fields into a grid: ta19v, ta19h, ta22v, ta37v,
 * ta37h, ta85v, ta85h, and sfcidx.  The 'ta' fields are all antenna
 * temperatures, 'sfcidx' is the surface-type index.  At some point we may
 * want to ingest 'tb' (brightness temperatures) as well.

 *

 * The two 85 Ghz channels have twice the resolution of the 5 lower
 * frequency channels.  This means two different RGrid info structures, one
 * for the low channels and one for the high.

 *

 * Use a DCC_RGrid class DataChunk, until an appropriate scale and offset
 * can be found for each field.  In which case, the scale and offset will
 * be stored in an array parallel to the Fields and Fids arrays.  The array
 * of ScaleInfo structures will be needed in dc_ImgSetup() and used to fill
 * in the grid in FillGrid().

 *

 * We cannot possibly ingest every image, and we're only interested in
 * those near Darwin.  So define a lat/lon range and reject all images for
 * which the lat/lon of the first scan cell is not in this range.

 *	
 */


static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s [ingest options] [-p <plat>] ", prog);
	printf ("[-o <file>] <tapedev>\n");
	printf ("where...\n   <plat> is the name of a platform, ");
	printf ("default: '%s'\n",PLATFORM);
	printf ("   <file> is a file to echo valid logical records to\n");
	printf ("   <tapedev>, if not a valid tape device, ");
	printf ("is a file produced by -o\n");
	IngestUsage ();
}
	

int
main (argc, argv)
int	argc;
char	**argv;
{
	int i;
	char *ofile;	/* File to echo records to */
	int fd;
	int echo_fd;
	int is_tape;	/* non-zero if we're reading a tape (== skip EOF) */
	int nlog;
	SSMI_LogicalRec *lrec;

	mprof_stop();
/*
 * Get our program-specific defaults and options
 */
	ofile = NULL;
	i = 1;
	while (i < argc)
	{
		if (!strcmp(argv[i], "-p") && (i+1 < argc))
		{
			Platform = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else if (!strcmp(argv[i], "-o") && (i+1 < argc))
		{
			ofile = argv[i+1];
			IngestRemoveOptions(&argc, argv, i, 2);
		}
		else
			++i;
	}
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
 * Open the device file.  If it begins with "/dev", we expect a tape
 * and try to skip EOF markers.  Otherwise we expect a file and just
 * read one block after another.
 */
	if ((fd = open (argv[1], O_RDONLY)) < 0)
	{
		printf ("Error %d opening device '%s'\n", errno, argv[1]);
		exit (1);
	}
	if (!strncmp(argv[1],"/dev",4))
		is_tape = 1;
	else
		is_tape = 0;

	echo_fd = -1;
	if (ofile && (echo_fd = open (ofile, O_CREAT|O_WRONLY, 0660)) < 0)
	{
		printf ("Error %d opening echo file '%s'\n", 
			errno, ofile);
		exit (2);
	}
/*
 * Dump the header to stdout, for the user's benefit, but only if this
 * is from tape, otherwise we skip the header and assume the file
 * starts with data.
 */
	if (is_tape)
	{
		printf ("\nTAPE HEADER\n-----------\n");
		NextRecs (fd, LRecs, &nlog);
		lrec = &LRecs[0];
		for (i = 0; i < nlog; i++)
			printf ("%.79s\n", (char *)(lrec + i));
		
		/* Get rid of the EOF after the header */
		NextRecs (fd, LRecs, &nlog);
	}
/*
 * Connect to the data store, message, etc. Get FieldId's for our fields
 */
	IngestInitialize (OURNAME);
	SetupFields ();
/*
 * Start poring through the data records from the tape
 */
/*	mprof_restart("mprof.data2");	*/

	ProcessTape (fd, echo_fd, LRecs, is_tape);

	if (ofile) 
		close(echo_fd);
	close(fd);
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


static int
ReadTape (fd, lrecs, nlog, is_tape)
int fd;
SSMI_LogicalRec *lrecs;
int *nlog;
int is_tape;
{
	int size;
	/*
	 * To make sure we read the whole tape, ignore errors and try
	 * to blast through them.  Don't stop unless we detect EOM,
	 * which will be two consecutive reads which return 0 bytes read.
	 *
	 * Of course, if we're reading a file, quit at the first EOF.
	 */
	while ((size = NextRecs (fd, lrecs, nlog)) <= 0)
	{
		if (size < 0)		/* skip errors */
			continue;
		if (is_tape)		/* otherwise (size==0) <==> EOF */
		{
			size = NextRecs (fd, lrecs, nlog); /* skip EOF */
			if (size == 0) 	/* EOM --- end of media */
				return FALSE;
			if (size > 0)  	/* got something, send it along */
				return TRUE;
			/* otherwise an error, so drop out and try again */
		}
		else
			return FALSE;			/* end of file */
	}
	return TRUE;
}


static int
ProcessTape (fd, echo_fd, lrecs, is_tape)
int fd;
int echo_fd;
SSMI_LogicalRec *lrecs;
int is_tape;
/*
 * Read one physical record after another, combining scan pairs in 
 * logical records into SquareScan's and converting the squares into
 * RGrid DataChunk's. Return non-zero if we successfully read some data.
 */
{
	SquareScan square;
	DataChunk *dc;
	int scanpairs;		/* Number consecutive A/B scans added 	*/
	int lastpair;		/* Number of last scan used to build img*/
	int nlog;
	int i;
	int nrecs = 0;		/* total number logical records read	*/
	int inbounds;
	long log_orbit;		/* orbit number to log next		*/

	dc = CreateDC();
	SqClear (&square);
	scanpairs = lastpair = 0;
	inbounds = 0;		/* Whether previous scan was valid 	*/
	log_orbit = 0;
	while (ReadTape (fd, lrecs, &nlog, is_tape))
	{
		/*
		 * Process the individual scans in each logical record
		 */
		for (i = 0; i < nlog; i++)
		{
			/*
			 * Log where we are 4 times every orbit
			 */
			if (lrecs[i].orbit >= log_orbit)
			{
				log_orbit = lrecs[i].orbit + 2500;
				log_orbit -= log_orbit % 2500;
				Log(EF_INFO, lrecs+i, nrecs + i);
			}

			/*
			 * If we still haven't found a scan we want,
			 * keep going.  If the previous one was no good,
			 * we don't have any bookkeeping to do either
			 */
			if (!ScanWithinLimits (lrecs+i))
			{
				if (!inbounds)
					continue;
				else
					inbounds = 0;
			}
			else
			{
				/*
				 * Beginning to find scans in the right place
				 */
				if (!inbounds)
				{
				   IngestLog (EF_INFO, 
				   "Beginning to read image of scans...");
				   inbounds = 1;
				}
			}

			Log (EF_DEBUG, lrecs+i, nrecs+i);
			if (inbounds)
			{
				decode_ssmi (1, 0, 1, i+1, (char *)(lrecs+i));
				/*
				 * More complications: the decoded scan pair
				 * may be garbage.  If so, ignore it
				 */
				if (IsGarbage(C_OUTDAT))
				{
				   IngestLog (EF_DEBUG,
				   "Above record decoded to garbage, ignored");
				   continue;
			        }
				else
				{
					++scanpairs;
					SqAddLRec (&square, C_OUTDAT);
				}
			}
			else
			{
				IngestLog (EF_DEBUG, 
					   "Out-of-limit scan ending image");
			}
			/*
			 * Write this record no matter what.  If inbounds,
			 * it will be part of the image.  If not, it will
			 * mark a dividing line between consecutive images.
			 */
			WriteRecs (echo_fd, lrecs+i, 1);

			if (SqIsFull(&square) && (scanpairs-lastpair >= 8) &&
			    ((scanpairs % 32 == 0) || (!inbounds)))
			{
				lastpair = scanpairs;
				BuildImage (&square, dc);
				if (dc_GetNSample(dc) >= DC_BLOCK_SIZE)
				{
					ds_StoreBlocks (dc, TRUE, NULL, 0);
					dc_DestroyDC (dc);
					dc = CreateDC ();
				}
			}

			if (!inbounds)	/* Clear out scans */
			{
				IngestLog (EF_DEBUG, 
				   "Scans now out of limits, clearing image");
				scanpairs = 0;
				lastpair = 0;
				SqClear (&square);
			}
		}
		nrecs += nlog;
	} /* while ReadTape() */

	if (dc_GetNSample(dc) > 0)
	{
		ds_StoreBlocks (dc, TRUE, NULL, 0);
	}
	dc_DestroyDC (dc);
	IngestLog(EF_INFO,"Finished file of %i logical records",nrecs);
	return (nrecs);
}




static int
NextRecs (fd, lrecs, nlog)
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
	*nlog = size / sizeof (SSMI_LogicalRec);
/*
 * Check for error or EOF
 */
	if (size <= 0)
	{
		if (size < 0)
			printf ("Error %d reading tape\n", errno);
	}
	return (size);
}






static void
WriteRecs (fd, lrecs, nlog)
int fd;
SSMI_LogicalRec *lrecs;
int nlog;
/*
 * Write the array of logical records to the given file descriptor.
 */
{
	if (fd >= 0)
	{
		write (fd, (char *)lrecs, nlog * sizeof(SSMI_LogicalRec));
		fsync(fd);
	}
}




static void
Log (flags, lrec, nrec)
int flags;
SSMI_LogicalRec *lrec;
int nrec;
{
	ZebTime zt;
	char buf[30];
	float lat, lon;
	
	/*
	 * Take this opportunity to log how we're doing
	 */
	zt.zt_MicroSec = 0;
	zt.zt_Sec = SSMI_TIME(lrec);
	TC_EncodeTime (&zt, TC_Full, buf);
	lat = lrec->a_lat[0] * 1e-2 - 90.0;
	lon = lrec->a_lon[0] * 1e-2;
	if (lon > 180.0)
		lon -= 360.0;
	IngestLog (flags, "Orbit %li, Rec %i, %s: %4.2f lat %4.2f lon", 
		   lrec->orbit, nrec, buf, lat, lon);
}




static int
IsGarbage (dat)
OUTDAT_BLOCK *dat;
/*
 * Do some quick tests for a valid block, such as xtime in the correct
 * range (basically nonzero) and xlat and xlon valid
 */
{
	int i;

	if ((dat->itime <= 0) || 	  /* i.e. at or before 1-1-1987 */
	    (dat->itime > 10*365*24*60*60))	/*     after 12-31-1996 */
		return TRUE;
	if (dat->rev <= 0)
		return TRUE;
	if ((dat->xlatsc < -90.0) ||
	    (dat->xlatsc > 90.0)  ||
	    (dat->xlonsc < 0.0) ||
	    (dat->xlonsc > 360.0))
		return TRUE;
	if ((dat->altsc) < 500 ||
	    (dat->altsc) > 50000)
		return TRUE;
	for (i = 0; i < 128; i+=32)
	{
		if ((dat->alat[i] < -90.0)  ||
		    (dat->alat[i] > 90.0)   ||
		    (dat->alon[i] < 0.0) ||
		    (dat->alon[i] > 360.0))
			return TRUE;
	}
	if (dat->talo[0][0] < 100.0 || dat->talo[1][1] < 100.0)
		return TRUE;
	if (dat->atahi[0][0] < 100.0 || dat->atahi[1][1] < 100.0)
		return TRUE;
	if (dat->talo[0][0] > 600.0 || dat->talo[1][1] > 600.0)
		return TRUE;
	if (dat->atahi[0][0] > 600.0 || dat->atahi[1][1] > 600.0)
		return TRUE;
	return FALSE;
}

	

static int
ScanWithinLimits (lrec)
SSMI_LogicalRec *lrec;
/*
 * Determine if this scan is within lat/lon limits in which we're
 * interested in, using the first A-scan lat and lon from logical record
 */
{
	/*
	 * Do the comparison with the biased/scaled short rather than
	 * convert every lat/lon from the logical record
	 */
	static const short
	   north_lat = (short)(((-12.45722 + 20.0) + 90.0)*1.0e+2);
	static const short
	   south_lat = (short)(((-12.45722 - 20.0) + 90.0)*1.0e+2);
	static const short
	   west_lon = (short)((130.92528 - 20.0)*1.0e+2);
	static const short
	   east_lon = (short)((130.92528 + 10.0)*1.0e+2);

	/*
	 * Our current limits are 20 degrees W, and 10 degrees N and S of the
	 * Darwin radar site at Berrimah.
	 */
	if ((lrec->a_lon[0] > east_lon) || (lrec->a_lon[0] < west_lon) ||
	    (lrec->a_lat[0] > north_lat) || (lrec->a_lat[0] < south_lat))
		return (FALSE);
	else
		return (TRUE);
}



static DataChunk *
CreateDC ()
{
DataChunk 	*dc;
/*
 * Create the data chunk and put in the platform ID, field IDs, and
 * the bad value flag (even though its never used)
 */
	dc = dc_CreateDC (DCC_Image);
	if (!dc)
		return (NULL);

	if ((dc->dc_Platform = ds_LookupPlatform (Platform)) == BadPlatform)
	{
		IngestLog (EF_EMERGENCY, "Cannot get platform ID for '%s'",
			   Platform);
		dc_DestroyDC (dc);
		return (NULL);
	}

	dc_ImgSetup (dc, NUM_FIELDS, Fids, Scales);
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
	unsigned char *image;	/* Space for grid values for this sample  */
	int i;

	IngestLog (EF_DEBUG, "Building an image...");
	/*
	 * Establish an order for our scans
	 */
	SqOrder (ss);

	/*
	 * Start with low-frequency channels, 64x64 grid, 25km resolution
	 */
#ifdef notdef	/* Apparently all fields have to have the same geometry,
		   so we'll have to ingest lo-freq data in a higher res than
		   actually exists.
		 */
	info.rg_Xspacing = 25.0;
	info.rg_Yspacing = 25.0;
	info.rg_Zspacing = 0.0;
	info.rg_nX = 64;
	info.rg_nY = 64;
	info.rg_nZ = 1;
#endif
	/*
	 * Instead of 128x128 at 12.5, we'll try 105x105 at 15km res to help
	 * smooth over the gaps from bad scans
	 */
	info.rg_Xspacing = 15;
	info.rg_Yspacing = 15;
	info.rg_Zspacing = 0.0;
	info.rg_nX = 100;
	info.rg_nY = 80;
	info.rg_nZ = 1;

	/*
	 * Allocate space for the grid values.  This memory is used for
	 * all channels.
	 */
	image = (unsigned char *)malloc( 128*128*sizeof(unsigned char) );

	/*
	 * Now just build our grid map by specifying a low-freq channel.
	 * The grid map will be the same for all 5 low-freq channels.
	 */
	BuildGridMap (ss, &gm, &info, ch19v);

	sample = dc_GetNSample(dc);
	SqZebTime (ss, &zt);
	SqOrigin (ss, &origin);
	for (i = 0; i < 5; ++i)		/* The five lower frequencies */
	{
		FillGrid (image, ss, &gm, Fields[i].channel);
		dc_ImgAddImage (dc, sample, Fids[i],
				&origin, &info, &zt, image, /*len*/ 0);
	}
	FreeGridMap (&gm);

	/*
	 * Now for high-frequency channels, 128x128 grid, 12.5 km resolution
	 */
#ifdef notdef				/* use the same one set above */
	info.rg_Xspacing = 12.5;
	info.rg_Yspacing = 12.5;
	info.rg_Zspacing = 0.0;
	info.rg_nX = 128;
	info.rg_nY = 128;
	info.rg_nZ = 1;
#endif

	/*
	 * Now just build our grid map by specifying a high-freq channel.
	 */
	BuildGridMap (ss, &gm, &info, ch85v);

	for (i = 5; i < 8; ++i)	/* The two higher frequencies and sfc-type */
	{
		FillGrid (image, ss, &gm, Fields[i].channel);
		dc_ImgAddImage (dc, sample, Fids[i],
				&origin, &info, &zt, image, /*len*/ 0);
	}

	FreeGridMap (&gm);
	free (image);
	IngestLog (EF_INFO, "Finished building and adding the image.");
}



static void
FillGrid (image, ss, gm, ch)
unsigned char *image;
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
	int fld;

	fld = (int)ch - 1;
	for (iy = 0; iy < gm->gm_info.rg_nY; ++iy)
	{
		for (ix = 0; ix < gm->gm_info.rg_nX; ++ix)
		{
			int row = (gm->gm_info.rg_nY - 1 - iy);
			GridToCell (gm, ix, iy, &scan, &cell);
		/*
		 * Since the grid (for some reason) is stored with Y (or row)
		 * beginning at the top and increasing downwards, the row
		 * must be flipped from the value used in the GridMap
		 */
			image[ row*(gm->gm_info.rg_nX) + ix ] = 
				(unsigned char)
				((SqCellValue (ss, scan, cell, ch) - 
				 Scales[fld].s_Offset) * Scales[fld].s_Scale);
		}
	}
}
