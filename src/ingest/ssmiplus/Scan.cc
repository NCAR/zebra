/*							 -*- C++ -*-
 * $Id: Scan.cc,v 1.1 1994-07-31 06:17:29 granger Exp $
 *
 * Define the methods for the scan classes, where each class corresponds
 * to a different file format.
 */

extern "C" {
#include <defs.h>		/* For ZebTime */
}

#include "Scan.h"


void
ABScan::Location (Location *locn) const
{
	locn->l_lat = this->Lat(); 
	locn->l_lon = this->Lon(); 
	locn->l_alt = this->Alt();
}


ABScan::ABScan (void)
{
	SetLimits(0.0, 0.0, 0.0, 0.0);
}


void
ABScan::SetLimits (float slat, float wlon, 
		   float nlat, float elon)
{
	southlat = slat;
	westlon = wlon;
	northlat = nlat;
	eastlon = elon;
}



int
ABScan::WithinLimits (void) const
/*
 * Determine if this scan is within the lat/lon limits in which we're
 * interested in.
 */
{
}



/* ----------------------------------------------------------------
 * RSS_ABScan
 * ----------------------------------------------------------------
 * RSS methods, which must use the RSS format header.
 */

RSS_ABScan::RSS_ABScan (void) : ABScan()
{
	header = NULL;
	lrec = NULL;
}


RSS_ABScan::~RSS_ABScan (void)
{
	if (header)
		free ((char *)header);
	if (lrec)
		free ((char *)lrec);
}


void *
RSS_ABScan::HeaderBuffer (void)
{
	if (header == NULL)
		header = (char *) malloc (sizeof (RSS_Rec));
	return ((void *)header);
}


void *
RSS_ABScan::ScanBuffer (void)
{
	if (lrec == NULL)
		lrec = (RSS_Rec *) malloc (sizeof (RSS_Rec));
	return ((void *)lrec);
}


void
RSS_ABScan::ScanATime (ZebTime *zt) const
{
	zt->zt_MicroSec = 0;
	zt->zt_Sec = RSS_TIME(lrec);
}


void
RSS_ABScan::ScanBTime (ZebTime *zt) const
{
	ScanATime (zt);
	zt->zt_Sec += 1;
	zt->zt_MicroSec += 899 * 1000;	// 1899 ms == 1899000 us == 1.899 secs
}


float
RSS_ABScan::Lat (void) const
{
	return (lrec->a_lat[0] * 1e-2 - 90.0);
}


float
RSS_ABScan::Lon (void) const
{
	float _lon = lrec->a_lon[0] * 1e-2;
	if (_lon > 180.0)
		_lon -= 360.0;
	return (_lon);
}


float
RSS_ABScan::Alt (void) const
{
	return (lrec->alt);
}


unsigned long
RSS_ABScan::Orbit(void) const
{
	return (lrec->orbit);
}



int
RSS_ABScan::Decode(OUTDAT_BLOCK *od)
{
	/*
	 * We have a RSS logical record, so decode it into the
	 * global OUTDAT block.  The record index is always one:
	 * we only have one logical record and we aren't dealing
	 * in physical records.
	 */
	return (decode_rss (1, 0, 1, /*record index*/ 1, (char *)lrec));
}


int
RSS_ABScan::Bad(void) const
{
	return (0);	// always supposed to be good
}


/* ----------------------------------------------------------------
 * L1B_ABScan
 * ---------------------------------------------------------------- */


L1B_ABScan::L1B_ABScan (void) : ABScan()
{
	header = NULL;
	lrec = NULL;
}


L1B_ABScan::~L1B_ABScan (void)
{
	if (header)
		free ((char *)header);
	if (lrec)
		free ((char *)lrec);
}

void *
L1B_ABScan::HeaderBuffer (void)
{
	if (header == NULL)
		header = (L1B_Header *) malloc (sizeof (L1B_Header));
	return ((void *)header);
}


void *
L1B_ABScan::ScanBuffer (void)
{
	if (lrec == NULL)
		lrec = (L1B_DataRec *) malloc (sizeof (L1B_DataRec));
	return ((void *)lrec);
}



void
L1B_ABScan::ScanBTime (ZebTime *zt) const
{
	ZebTime base;	/* 1987 reference time */
	long secs;	/* seconds since 1987 */
	int yr;

	TC_ZtAssemble (zt, lrec->year, 1, 1, 0, 0, 0, 0);
	zt->zt_Sec += (lrec->day - 1) * 24 * 3600;
	zt->zt_Sec += lrec->msecs / 1000;
	zt->zt_MicroSec = (lrec->msecs % 1000) * 1000;
}


void
L1B_ABScan::ScanATime (ZebTime *zt) const
{
	ScanBTime (zt);
	zt->zt_Sec -= 2;			// subtract 1899 milliseconds
	zt->zt_MicroSec += (2000 - 1899) * 1000;
	zt->zt_Sec += zt->zt_MicroSec / 1000000;
	zt->zt_MicroSec = zt->zt_MicroSec % 1000000;
}



float
L1B_ABScan::Lat (void) const
{
	return ((float)lrec->locn_a[0] / 128.0);
}



float
L1B_ABScan::Lon (void) const
{
	return ((float)lrec->locn_a[1] / 128.0);
}


float
L1B_ABScan::Alt (void) const
{
	return (0);	// this info not contained in the level 1b record
}



unsigned long
L1B_ABScan::Orbit(void) const
{
	return (lrec->orbit);
}



int
L1B_ABScan::Bad(void) const
{
	/*
	 * For now, take anything with good location values and
	 * see where that gets us.  Later we may want to flag
	 * scans/channels individually.
	 */
	return ((lrec->earth_a != 0) || (lrec->earth_b != 0));
}



int
L1B_ABScan::Decode(OUTDAT_BLOCK *od)
{
	return (l1bta (header, lrec, od));
}



