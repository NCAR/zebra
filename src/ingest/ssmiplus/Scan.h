/*
 * $Id: Scan.h,v 1.1 1994-07-31 06:17:29 granger Exp $
 * 
 * Define classes which encapsulate the different record formats and
 * sources under a polymorphic superclass.
 */

#ifndef _Scan_h_
#define _Scan_h_

/*
 * These are the low-level structures which map to each format's logical
 * records
 */
#include "outdat.h"		/* For Remote Sensing Systems decode */
#include "rss.h"		/* For RSS tape format */
#include "level1b.h"		/* For MSFC DAAC Level 1b file format */

/* ----------------------------------------------------------------
 * ABScan -- Abstract base class for scans from different formats
 * ----------------------------------------------------------------
 * Contains one pair of scans, A and B, from a SSMI satellite.  Defines
 * some general parameters which will be consistent among scans, but
 * most of the information is farmed out to the subclass to be calculated
 * according to the format's record structure.
 *
 * The scan region limits are set and tested in the base class functions.
 * The derived classes figure out the lat, lon, and alt of the scan.
 */
class ABScan
{

public:
	ABScan (void);
	virtual ~ABScan (void) {};

	virtual float Lat(void) const = 0;	// Some generic scan locn
	virtual float Lon(void) const = 0;
	virtual float Alt(void) const = 0;
	void Location (Location *locn) const;

	virtual unsigned long Orbit(void) const = 0;	// Orbit number

	virtual void ScanATime(ZebTime *zt) const = 0;	// scan time
	virtual void ScanBTime(ZebTime *zt) const = 0;

	// Return pointer to buffer large enough for a header record
	virtual void *HeaderBuffer() = 0;

	// Return pointer to a buffer for a logical data record
	virtual void *ScanBuffer() = 0;

	void SetLimits (float slat, float wlon, 
			float nlat, float elon);
	virtual int WithinLimits(void) const;
	virtual int Bad (void) const = 0;// nonzero when scan should be skipped

	// The crux routine: process a scan pair into an OUTDAT structure.
	virtual int Decode (OUTDAT_BLOCK *od) = 0;

private:
	float southlat, westlon, northlat, eastlon;	/* our scan limits */
};


/* ----------------------------------------------------------------
 * RSS_ABScan
 * ----------------------------------------------------------------
 * The ABScan subclass which stores an RSS logical record and
 * implements the ABScan methods by interpreting the record.
 * No new public methods, just the addition of fields for the
 * RSS header and data records.
 */
class RSS_ABScan : public ABScan
{
public:	
	RSS_ABScan (void);
	~RSS_ABScan (void);

	unsigned long Orbit(void) const;
	float Lat(void) const;
	float Lon(void) const;
	float Alt(void) const;
	void ScanATime(ZebTime *) const;	// scan times
	void ScanBTime(ZebTime *) const;
	int Bad (void) const;	// returns nonzero if scan should be skipped

	// Return pointer to buffer large enough for a header record
	void *HeaderBuffer();

	// Return pointer to a buffer for a logical data record
	void *ScanBuffer();

	// The crux routine: process a scan pair into an OUTDAT structure.
	int Decode (OUTDAT_BLOCK *od);

private:
	char *header;
	RSS_LogicalRec *lrec;
};



/* ----------------------------------------------------------------
 * L1B_ABScan
 * ----------------------------------------------------------------
 * Holds pointers to a MSFC DAAC level 1b header and data record
 * implements the SSMI_ABScan methods from those records.
 */
class L1B_ABScan : public ABScan
{
public:
	L1B_ABScan (void);
	~L1B_ABScan (void);

	unsigned long Orbit() const;		// Orbit number
	float Lat(void) const;
	float Lon(void) const;
	float Alt(void) const;
	void ScanATime(ZebTime *) const;	// scan time
	void ScanBTime(ZebTime *) const;

	int Bad (void) const;	// returns nonzero if scan should be skipped

	// Return pointer to buffer large enough for a header record
	void *HeaderBuffer();

	// Return pointer to a buffer for a logical data record
	void *ScanBuffer();

	// The crux routine: process a scan pair into an OUTDAT structure.
	int Decode (OUTDAT_BLOCK *od);

private:
	L1B_Header *header;
	L1B_DataRec *lrec;
};




#endif /* ! _Scan_h_ */
