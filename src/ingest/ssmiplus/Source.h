/* -*- C++ -*-
 *
 * $Id: Source.h,v 1.1 1994-07-31 06:17:29 granger Exp $
 * 
 * Define classes which encapsulate the different record formats and
 * sources under a polymorphic superclass.
 */

#ifndef _Source_h_
#define _Source_h_

/*
 * These are the low-level structures which map to each format's logical
 * records
 */
#include "outdat.h"
#include "rss.h"		/* For RSS tape format */
#include "level1b.h"		/* For MSFC DAAC Level 1b file format */


/*
 * The format types available
 */
typedef enum { F_Unknown, F_RSS, F_L1B } SSMIFormat;


/* ----------------------------------------------------------------
 * SSMI_Source
 * ----------------------------------------------------------------
 * A structure and methods for reading ABScan records of various
 * types from either files or tape devices.
 */
class SSMI_Source
{
public:
	SSMI_Source (void);
	virtual ~SSMI_Source (void);

	inline const char *Path (void) { return (filepath); }
	virtual int Open (const char *path);
	virtual void Close (void);
	virtual int Read (char *rec, const long bytes);
	virtual inline int eofile (void) { return (eof); }

	// Return non-zero if there are more scans to read
	virtual int MoreScans (void) = 0;

	// Return <= 0 on failure to read a data record or a header
	virtual int ReadABScan (void *record) = 0;
	virtual int ReadHeader (void *header) = 0;

private:
	char *filepath;		// save the path of our source device
	int fd;			// our file descriptor
	int eof;		// end of file indicator
};


/*
 * Level 1b sources can only be straight files, so we only have
 * a single class to implement which reads from a file descriptor, be it
 * a disk file or a tape drive.
 */
class L1B_Source : public SSMI_Source
{
public:
	L1B_Source (void)
	{
		nread = 0;
		nscans = 0;
	}

	int ReadABScan (void *record);
	int ReadHeader (void *header);
	int MoreScans (void);
private:
	int nread;		// number scans read from this source so far
	int nscans;	// keep track of the scans in the file, given in hdr
};



#ifdef RSS			// leaving out RSS support for now
class RSS_Tape : public SSMI_Source
{
public:
	RSS_Tape (void);
	~RSS_Tape (void);
#ifdef RSS_ECHO		// not needed since echo not implemented
	int Open (char *path, char *ofile);
	void Close (void);
#endif
	int ReadABScan (void *record);
	int ReadHeader (void *header);

	// Just always return 1 and count on aborting when trying
	// to read another header file.
	int MoreFiles (void) { return (1); }
	int MoreScans (void);

private:
	void EchoRSS (void);	// Echo our record to a file

	int ndatafiles;		// number of datafiles following header on tape
	int nlogical;		// number of logical records in physical rec
	int count;		// next logical record to 'read'
	int echo_fd;		// fd to echo records to, -1 if disabled

	// The RSS physical record is 16 logical records
	RSS_LogicalRec lrec[16];
};



class RSS_File : public SSMI_Source
{
public:
	RSS_File (void);
	int Open (char *path, char *ofile);
	void Close (void);
	int ReadABScan (void *record);
	int ReadHeader (void *header);

	// Just always return 1 and count on aborting when trying
	// to read another header file.
	int MoreFiles (void) { return (1); }
	int MoreScans (void);

private:
	int nlogical;		// number of logical records in physical rec
	int count;		// next logical record to 'read'
	int echo_fd;		// fd to echo records to, -1 if disabled

	// The RSS physical record is 16 logical records
	RSS_LogicalRec lrec[16];
};
#endif // RSS


#endif /* ! _Source_h_ */
