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
	"$Id: ssmi_ingest.c,v 1.5 1993-10-22 22:48:44 granger Exp $";
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
#define OURNAME		"SSMI Attack"
#define TAPE_ERRORS	10		/* # tape errors to abort after */
#define DEFAULT_RESOLUTION (12.5)	/* km */
#define MIN_IMAGE_SCANS (32)		/* Minimum # scans to make an image */

#define StoreBlocks(a,b,c,d) ((NoDataStore)?(TRUE):\
			      (ds_StoreBlocks(a,b,c,d)))

static int NextRecs FP(( int fd, SSMI_LogicalRec *lrecs, int *nlogical ));
static void WriteRecs FP(( int fd, SSMI_LogicalRec *lrecs, int nlogical ));
static void SetupFields ();
static int ReadHeader FP((int fd, SSMI_LogicalRec *lrecs, int *ndatafiles));
static int ReadTape FP((int fd, SSMI_LogicalRec *lrecs, int *nlogical,
			int *nfiles, int is_tape));
static int ProcessTape FP((int tape_fd, int echo_fd, SSMI_LogicalRec *lrecs,
			   int is_tape, int ndatafiles));
static DataChunk *CreateDC ();
static int ScanWithinLimits FP((SSMI_LogicalRec *lrec, int check_lon));
static void IngestImage FP((SquareScan *ss));
static void BuildImage FP((SquareScan *ss, DataChunk *dc));
static void FillGrid FP((unsigned char *image, SquareScan *ss, 
			 GridMap *gm, Channel ch));
static void Log FP((int flags, SSMI_LogicalRec *lrec, int nrec));
void ReportGridMapStats FP((SquareScan *ss, GridMap *gm));

/*
 * Large buffer data that is best left global, holds one physical record
 * and 16 logical records
 */
SSMI_LogicalRec LRecs[16];
char 		*Platform = PLATFORM;
float		Resolution = DEFAULT_RESOLUTION;

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
	{ 1.0, 100.0 },	/* Assuming temps (K) from 100 to 356 */
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 100.0 },
	{ 1.0, 0.0 }   	/* The surface-type index */
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
 */

static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s [ingest options] [-p <plat>] ", prog);
	printf ("[-o <file>] [-r <res>] <tapedev>\n");
	printf ("where...\n   <plat> is the name of a platform, ");
	printf ("default: '%s'\n",PLATFORM);
	printf ("   <file> is a file to echo valid logical records to\n");
	printf ("   <tapedev>, if not a valid tape device, ");
	printf ("is a file produced by -o\n");
	printf ("   <res> is the resolution in km.  The default ");
	printf ("resolution is %f km.\n", DEFAULT_RESOLUTION);
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
	int ndatafiles;
	SSMI_LogicalRec *lrec;
	int res;	/* return value from ProcessTape and ReadHeader */

#ifdef MPROF
	mprof_stop();
#endif
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
		else if (!strcmp(argv[i], "-r") && (i+1 < argc))
		{
			Resolution = atof(argv[i+1]);
			IngestRemoveOptions(&argc, argv, i, 2);
			if (Resolution <= 0.0)
			{
				printf ("Illegal resolution: %f",
					Resolution);
				Usage (argv[0]);
				exit (1);
			}
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
 * Connect to the data store, message, etc. Get FieldId's for our fields
 */
	IngestInitialize (OURNAME);
	SetupFields ();
/*
 * Start poring through the data records from the tape
 */
#ifdef MPROF
	mprof_restart("mprof.data2");
#endif
	i = 1;
	while (1)
	{
		ndatafiles = 1;		/* case for reading from a file */
		if (is_tape)
		{
			res = ReadHeader (fd, LRecs, &ndatafiles);
			if (res == 0)
			{
				IngestLog (EF_INFO, 
				   "End of tape trying to read header.");
			}
			if (res <= 0)
				break;
		}
		res = ProcessTape (fd, echo_fd, LRecs, is_tape, ndatafiles);
		if (res >= 0)
			IngestLog (EF_INFO,
			   "Finished data file sequence %d after %d records",
			   i, res);
		else
			break;
		if (!is_tape)
			break;
		++i;
	}

	if (ofile) 
		close(echo_fd);
	close(fd);
	return ((res < 0) ? res : 0);
}




static int
ReadHeader (fd, lrecs, ndatafiles)
int fd;
SSMI_LogicalRec *lrecs;
int *ndatafiles;
/*
 * Returns <= zero on failure to find a header, meaning abandon the tape
 */
{
	int nlog, i, rt;

	printf ("\nTAPE HEADER\n-----------\n");
	/*
	 * Use ReadTape() to skip over any EOF which precede the header
	 * we're trying to read.  If we just can't do it, we must be
	 * through with the tape.  The 0 is because we don't care about
	 * being informed of an EOF.
	 */
	rt = ReadTape (fd, lrecs, &nlog, 0, 1 /*is_tape*/);
	if (rt <= 0)
		return (rt);
	for (i = 0; i < nlog; i++)
		printf ("%.79s\n", (char *)(lrecs + i));
	
	/* Get rid of the EOF after the header */
	NextRecs (fd, lrecs, &nlog);

	/* 
	 * Extract the number of data files from the very first line.
	 * The number begins with a space at char 31 (index 30) and
	 * ends with a space at char 34 (index 33).
	 */
	*ndatafiles = atoi( (char *)lrecs + 31 );
	IngestLog (EF_INFO, "Expecting %d data files following header",
		   *ndatafiles);
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
ReadTape (fd, lrecs, nlog, nfiles, is_tape)
int fd;
SSMI_LogicalRec *lrecs;
int *nlog;
int *nfiles;		/* EOF counter, stop when it reaches zero */
int is_tape;
/*
 * Returns > 0 if another block of logical records has been read,
 *	   = 0 if EOF or EOM
 *	   < 0 if error, leaves error number in errno
 *
 * Stop when nfiles reaches 0, meaning we shouldn't go any further.
 */
{
	int size;
	int errcnt;

	/*
	 * To make sure we read the whole tape, ignore errors and try
	 * to blast through them.  Don't stop unless we detect EOM,
	 * which will be two consecutive reads which return 0 bytes read.
	 *
	 * Of course, if we're reading a file, quit at the first EOF.
	 */
	errcnt = 0;
	while ((size = NextRecs (fd, lrecs, nlog)) <= 0)
	{
		if (size < 0)		/* skip errors (size < 0) */
		{
			++errcnt;
			if (errcnt > TAPE_ERRORS)
			{
				IngestLog (EF_EMERGENCY,
				   "Aborting from tape error #%d", errno);
				return (size);
			}
		}
		if (nfiles && (--(*nfiles) == 0))
			return (0);	/* don't read past expected EOF */
		if ((size == 0) && !is_tape)		/* end of file */
			return FALSE;
		if ((size == 0) && is_tape)
		{
			size = NextRecs (fd, lrecs, nlog); /* skip EOF */
			if (size >= 0)	/* either EOM or some data */
				return (size);
		}
	}
	return (size);
}


static int
ProcessTape (fd, echo_fd, lrecs, is_tape, ndatafiles)
int fd;
int echo_fd;
SSMI_LogicalRec *lrecs;
int is_tape;
int ndatafiles;
/*
 * Read one physical record after another, combining scan pairs in 
 * logical records into SquareScan's and converting the squares into
 * RGrid DataChunk's. Return number records read or an error < 0.
 */
{
	SquareScan square;
	int nlog;		/* Number logical records in block	*/
	int nfiles = 0;		/* Number files read so far from tape	*/
	int rt;			/* return value from ReadTape		*/
	int i;			/* Loop over logical records in block	*/
	int nrecs = 0;		/* total number logical records read	*/
	int inbounds = 0;	/* Whether previous scan was valid 	*/
	long log_orbit = 0;	/* orbit number to log next		*/
	int nimages = 0;	/* Number images ingested so far	*/

	SqClear (&square);
	while ((rt = ReadTape (fd, lrecs, &nlog, &ndatafiles, is_tape)) > 0)
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
			 * we don't have any bookkeeping to do either.
			 * Only check longitude limits if the previous
			 * scan was not inbounds (i.e. the square is
			 * empty).
			 */
			if (!ScanWithinLimits (lrecs+i, !inbounds))
			{
				if (!inbounds)
					continue;
				else
					inbounds = 0;
			}
			else if (!inbounds)
			{
				/*
				 * Beginning to find scans in the right place
				 */
				IngestLog (EF_INFO, 
				   "Beginning to read image of scans...");
				inbounds = 1;
			}

			Log (EF_DEBUG, lrecs+i, nrecs+i);
			if (inbounds)
			{
				decode_ssmi (1, 0, 1, i+1, (char *)lrecs);
				SqAddLRec (&square, C_OUTDAT);
			}

			/*
			 * Write this record no matter what.  If inbounds,
			 * it will be part of the image.  If not, it will
			 * mark a dividing line between consecutive images.
			 */
			WriteRecs (echo_fd, lrecs+i, 1);

			if (inbounds)
			{
				/*
				 * As long as we're getting scans inbounds,
				 * keep reading and keep adding to the square
				 */
				continue;
			}
			IngestLog (EF_DEBUG, 
				   "Out-of-limit scan closing image");
			/*
			 * So we know we have now left our latitude
			 * boundaries, so its time to build an image from
			 * the scans in our square.  Of course, squares 
			 * with few scans are useless, possibly mostly
			 * out of bounds, and ignored.
			 */
			if (SqNumScans(&square) >= MIN_IMAGE_SCANS)
			{
				IngestImage (&square);
				++nimages;
			}
			else
			{
				IngestLog (EF_PROBLEM,
					   "Only %d scans, image not built",
					   SqNumScans(&square));
			}

			/* Clear out scans */
			IngestLog (EF_DEBUG, 
				   "Scans now out of limits, clearing image");
			SqClear (&square);
		}
		nrecs += nlog;

	} /* while ReadTape() */

	IngestLog(EF_INFO, "Read %i logical records, created %i images", 
		  nrecs, nimages);
	return ((rt < 0) ? rt : nrecs);
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
ScanWithinLimits (lrec, check_lon)
SSMI_LogicalRec *lrec;	/* the record to check				*/
int check_lon;		/* nonzero if interested in checking lon limits */
/*
 * Determine if this scan is within the lat/lon limits in which we're
 * interested in, using the first A-scan lat and lon from logical record
 */
{
	/*
	 * Do the comparison with the biased/scaled short rather than
	 * convert every lat/lon from the logical record.  Trying to center
	 * images over Berrimah: -12.45722 lat, 130.92528 lon.  One
	 * 128-cell (12.5 km/cell) is about 14 degrees wide.  The
	 * longitudinal separation between orbits is about 25 degrees, so
	 * use that distance to make sure we get at least one image every
	 * 24 hours.  Sun-synchronous satellites cover the whole earth
	 * every 24 hours in 14 orbits, passing over each spot at approx.
	 * the same local time every 24 hours.  Cells can be scanned either
	 * E-W or W-E, so check that lon of both ends of scan are within
	 * limits.  This means we'll at least get the one pass per day that
	 * passes closest to Darwin.  The wide lon limits allow series of
	 * scans which may eventually near Darwin; whether or not an image
	 * on the outskirts is worth building is left to another function.
	 */
	static const unsigned short
	   north_lat = ((0.0 + 90.0)*1.0e+2);
	static const unsigned short
	   south_lat = ((-25.0 + 90.0)*1.0e+2);
	static const unsigned short
	   west_lon = ((110.0)*1.0e+2);
	static const unsigned short
	   east_lon = ((150.0)*1.0e+2);

	if ((lrec->a_lat[0] > north_lat) || (lrec->a_lat[0] < south_lat))
		return (FALSE);
	if (!check_lon)
		return (TRUE);
	if ((lrec->a_lon[0] > east_lon) || (lrec->a_lon[0] < west_lon) ||
	    (lrec->a_lon[18] > east_lon) || (lrec->a_lon[18] < west_lon))
		return (FALSE);
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
IngestImage (ss)
SquareScan *ss;
/*
 * Build an image out of this SquareScan, add it to a DataChunk,
 * and ingest the DataChunk.
 */
{
	DataChunk *dc;

	dc = CreateDC ();
	IngestLog (EF_INFO, "Building an image from %d scans...", 
		   SqNumScans(ss));
	BuildImage (ss, dc);
	StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
}



static void
BuildImage (ss, dc)
SquareScan *ss;
DataChunk *dc;
/*
 * Create grid maps, fill a grid of values, and add it to the DataChunk.
 * The grid mapping will be different between the low-frequency and
 * hi-frequency channels: lo-freq channels have only 64 values per scan,
 * every other scan.
 */
{
	GridMap gm;	/* Grid map used for lo-freq and re-used for high */
	RGrid info;	/* info on the grid we're creating		  */
	Location origin;/* Origin of our grid				  */
	ZebTime zt;	/* Time of the sample grid			  */
	int sample;	/* Sample of DataChunk we are creating		  */
	unsigned char *image;	/* Space for grid values for this sample  */
	int i;

	/*
	 * Establish an order for our scans
	 */
	SqOrder (ss);
	sample = dc_GetNSample(dc);
	SqZebTime (ss, &zt);
	SqOrigin (ss, &origin);

	/* 
	 * Apparently all fields have to have the same geometry,
	 * so we'll have to ingest lo-freq data in a higher res than
	 * actually exists.  For now go with 12.5 km resolution.
	 */
	SqGridInfo (ss, Resolution, &info);

	/*
	 * Allocate space for the grid values.  This memory is used for
	 * all channels.
	 */
	image = (unsigned char *)
		malloc(info.rg_nX * info.rg_nY * sizeof(unsigned char));

	/*
	 * Now just build our grid map by specifying a low-freq channel.
	 * The grid map will be the same for all 5 low-freq channels.
	 */
	BuildGridMap (ss, &gm, &info, ch19v);
	IngestLog (EF_DEBUG, "Filling five lower frequencies");
	for (i = 0; i < 5; ++i)		/* The five lower frequencies */
	{
		FillGrid (image, ss, &gm, Fields[i].channel);
		dc_ImgAddImage (dc, sample, Fids[i],
				&origin, &info, &zt, image, /*len*/ 0);
	}
	ReportGridMapStats (ss, &gm);
	FreeGridMap (&gm);

	/*
	 * Now build our grid map by specifying a high-freq channel.
	 */
	BuildGridMap (ss, &gm, &info, ch85v);
	IngestLog (EF_DEBUG, "Filling two high frequencies and sfc types");
	for (i = 5; i < 8; ++i)
	{
		FillGrid (image, ss, &gm, Fields[i].channel);
		dc_ImgAddImage (dc, sample, Fids[i],
				&origin, &info, &zt, image, /*len*/ 0);
	}
	ReportGridMapStats (ss, &gm);
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


void
ReportGridMapStats (ss, gm)
SquareScan *ss;
GridMap *gm;
/*
 * Log grid map statistics to EF_DEVELOP log
 */
{
	char buf[128];
	int npts, ncells;

	npts = gm->gm_info.rg_nX * gm->gm_info.rg_nY;
	ncells = SqNumScans(ss) * 128;
	sprintf (buf, "%12s: npts=%d; ncells=%d; hits=%d; ","Scan>Buckets",
		 npts, ncells, gm->gm_sqr_hits);
	sprintf (buf+strlen(buf),"pctgrid=%5.2f; pctcells=%5.2f; ", 
		 (float)gm->gm_sqr_hits / npts * 100.0,
		 (float)gm->gm_sqr_hits / ncells * 100.0);
	sprintf (buf+strlen(buf), "avg c/b=%.1f",
		 (float)ncells / (float)gm->gm_sqr_hits);
	IngestLog (EF_DEVELOP, "%s", buf);
	sprintf (buf, "%12s: maxrad=%d; avgrad=%.1f; ","Buckets>Grid",
		 gm->gm_max_radius, gm->gm_avg_radius);
	sprintf (buf+strlen(buf), "pts_searched=%d, perbucket=%.1f; ",
		 gm->gm_cells, (float)gm->gm_cells / npts);
	IngestLog (EF_DEVELOP, "%s", buf);
	sprintf (buf, "%12s: avg_dist=%.1f; max_dist=%.1f; res=%.1f km; ",
		 "GridStats", gm->gm_avg_dist, gm->gm_max_dist,
		 gm->gm_info.rg_Xspacing);
	sprintf (buf+strlen(buf), "nX=%d; nY=%d", gm->gm_info.rg_nX,
		 gm->gm_info.rg_nY);
	IngestLog (EF_DEVELOP, "%s", buf);
}



#ifdef notdef		/* no longer needed, and hopefully never again */
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
#endif
