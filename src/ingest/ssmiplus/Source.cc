/* BTW, -*- C++ -*-
 *
 * $Id: Source.cc,v 1.1 1994-07-31 06:17:29 granger Exp $
 *
 * Define functions for the SSMI source classes which handle reading 
 * headers and logical records of various formats from files and tapes.
 */


#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/fcntlcom.h>

extern "C" {
#include <ingest.h>		// For IngestLog()
}

#include "Source.h"

#ifdef TAPES
const int TAPE_ERRORS = 10;		/* # tape errors to abort after */

/*
 * Forwards
 */
static int NextRecs (int fd, void *rec, int *nlog);
static int ReadTape (int fd, void *rec, int *nlog, int *nfiles, int is_tape);
#endif // TAPES



SSMI_Source::SSMI_Source ()
{
	filepath = NULL;
	eof = 0;
	fd = -1;
}


/*
 * Open a source.  Essentially this is just a matter of getting our
 * file descriptor.
 */
int
SSMI_Source::Open (const char *path)
{
	Close ();
	if ((fd = open (path, O_RDONLY)) < 0)
	{
		IngestLog (EF_PROBLEM, 
			   "Error %d opening device '%s'\n", errno, path);
		return (fd);
	}
	filepath = (char *) malloc (strlen(path) + 1);
	strcpy (filepath, path);
	return (0);
}


void
SSMI_Source::Close (void)
{
	if (fd >= 0)
		close (fd);
	fd = -1;
	if (filepath)
		free (filepath);
	filepath = NULL;
}


/*
 * Read nbytes from our file descriptor.  Return either nbytes
 * or a return code <= 0.  0 means end-of-file.
 */
int
SSMI_Source::Read (char *rec, const long nbytes)
{
	int nread = 0;
	int ret = 0;

	while (nread < nbytes)
	{
		ret = read (fd, rec + nread, (long)(nbytes - nread));
		if (ret > 0)
			nread += ret;
		else
			break;
	}
	if (ret == 0)
		eof = 1;
	return ((nread == nbytes) ? nread : ret);
}


/*
 * A default destructor to close files just in case
 */
SSMI_Source::~SSMI_Source (void)
{
	Close();
}



int
L1B_Source::ReadHeader (void *header)
/*
 * Returns <= zero on failure to find a header, meaning abandon the tape.
 * Otherwise returns the number of bytes read to get the header.
 */
{
	int rt;
	L1B_Header *hdr = (L1B_Header *)header;

	rt = Read ((char *)header, L1B_RECORD_SIZE);
	if (rt <= 0)
		return (rt);
	/* 
	 * Extract the number of data records (scan pairs) following the
	 * header.
	 */
	nscans = (int)hdr->num_scans;
	IngestLog (EF_INFO, "Expecting %d scans following level 1b header",
		   nscans);
	IngestLog (EF_DEBUG, "Craft ID %hu, sensor %hu", 
		   hdr->craft_id, hdr->sensor_id);
	IngestLog (EF_DEBUG, 
	  "Begin year %d day %d secs %.1f, end year %d day %d secs %.1f",
	  hdr->start_year, hdr->start_day, hdr->start_secs / 1000.0,
	  hdr->end_year, hdr->end_day, hdr->end_secs / 1000.0);
	IngestLog (EF_DEBUG,
		   "Number of gaps: %u", hdr->num_gaps);
	return (rt);
}


int
L1B_Source::ReadABScan (void *rec)
{
	int ret;
	ret = Read ((char *)rec, L1B_RECORD_SIZE);
	if (ret == L1B_RECORD_SIZE)
		++nread;
	return (ret);
}


int
L1B_Source::MoreScans (void)
{
	return (nread < nscans);
}



#ifdef TAPES
static int
ReadTape (int fd, void *rec, int *nlog,
	  int *nfiles,		// EOF counter, stop when it reaches zero
	  int is_tape)
/*
 * Returns > 0 if another block of logical records has been read,
 *	   = 0 if EOF or EOM
 *	   < 0 if error, leaves error number in errno
 *
 * Stop when nfiles reaches 0, meaning we shouldn't go any further.
 * Essentially, this routine calls NextRecs() to get more data records
 * while transparently skipping over data file boundaries in the case
 * of RSS data tapes.
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
	while ((size = NextRecs (fd, rec, nlog)) <= 0)
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
			size = NextRecs (fd, rec, nlog); /* skip EOF */
			if (size >= 0)	/* either EOM or some data */
				return (size);
		}
	}
	return (size);
}



static int
NextRecs (int fd, void *rec, int *nlog)
/*
 * Read the next physical record from the tape into prec and put the
 * number of logical records into Nlog.  Return true for a good read,
 * false for an error or EOF.
 */
{
	int size;
	int reclen;

	if (Format == F_RSS)
	{
		size = read (fd, (char *)rec, 16 * sizeof(RSS_LogicalRec));
		*nlog = size / sizeof (RSS_LogicalRec);
	}
	else
	{
		size = read (fd, (char *)rec, sizeof(L1B_DataRec));
		*nlog = 1;
	}
/*
 * Check for error or EOF
 */
	if (size <= 0)
	{
		if (size < 0)
			IngestLog (EF_PROBLEM, "Error %d reading device\n", 
				   errno);
	}
	return (size);
}
#endif // TAPES


#ifdef RSS		// left out for now
#ifdef RSS_ECHO		// not implemented at the moment
/*
 * RSS tape sources allow an optional file path to which data records
 * will be echoed.
 */
int
RSS_Tape::Open (char *path)
{
	char *ofile = NULL;

	if (ofile && (echo_fd = open (ofile, O_CREAT|O_WRONLY, 0660)) < 0)
	{
		IngestLog (EF_PROBLEM, "Error %d opening echo file '%s'\n", 
			   errno, ofile);
		return (-1);
	}
	return (0);
}


void
RSS_Tape::Close (void)
{
	SSMI_Source::Close();
	if (echo_fd >= 0)
		close (echo_fd);
	echo_fd = -1;
}
#endif


/*
 * Read an RSS header from a file (hah! there isn't one...)
 */
int
RSS_File::ReadHeader (void *header)
{
	return (1);
}



RSS_File::RSS_File (void)
{
	SSMI_Source::SSMI_Source ();	// do base class initialize as well
	format = F_RSS;
}



RSS_Tape::RSS_Tape (void)
{
	SSMI_Source::SSMI_Source ();	// do base class initialize as well
	format = F_RSS;
	nlogical = 0;
	ndatafiles = 0;
	echo_fd = -1;
	count = 0;
}


/*
 * Read an RSS header file from a tape into the buffer
 */
int
RSS_Tape::ReadHeader (void *header)
/*
 * Returns <= zero on failure to find a header, meaning abandon the tape.
 * Otherwise returns the number of bytes read to get the header.
 */
{
	int nlog, i, rt;

	printf ("\nRSS TAPE HEADER\n-----------\n");
	/*
	 * Use ReadTape() to skip over any EOF which precede the header
	 * we're trying to read.  If we just can't do it, we must be
	 * through with the tape.  The 0 is because we don't care about
	 * being informed of an EOF.
	 */
	rt = ReadTape (fd, lrec, &nlogical, 0, 1 /*is_tape*/);
	if (rt <= 0)
		return (rt);
	for (i = 0; i < nlogical; i++)
		printf ("%.79s\n", (char *)(lrec + i));
	
	/* Get rid of the EOF after the header */
	NextRecs (fd, lrec, &nlogical);

	/* 
	 * Extract the number of data files from the very first line.
	 * The number begins with a space at char 31 (index 30) and
	 * ends with a space at char 34 (index 33).
	 */
	ndatafiles = atoi( (char *)lrec + 31 );
	IngestLog (EF_INFO, "Expecting %d data files following header",
		   ndatafiles);
	return (rt);
}
	



#ifdef RSS_ECHO
void
RSS_Tape::EchoRSS (void)
/*
 * Write the array of logical records to the given file descriptor.
 */
{
	extern void fsync (int);

	if (echo_fd >= 0)
	{
		write (echo_fd, (char *)lrec, 
		       nlogical * sizeof(RSS_LogicalRec));
		fsync (echo_fd);
	}
}
#endif // RSS_ECHO

#endif // RSS suport
