/*
 * $Id: SquareScan.cc,v 1.3 1994-02-02 23:49:47 burghart Exp $
 *
 * Functions for manipulating scans and square-scans
 */

#include <string.h>
#include <memory.h>
#include <math.h>

extern "C" 
{

#include <defs.h>
#include <DataStore.h>

void cvt_Origin (double lat1, double lon1);
void cvt_ToXY (double lat2, double lon2, float *x2, float *y2);
}

#include "SquareScan.h"


void
SquareScan::AddScan (const Scan *sc)
/*
 * Add the given scan to the square
 */
{
	/*
	 * Copy scan into the next storage spot in the buffer
	 */
	memcpy ((char *)(sq_buf + sq_next),
		(char *)sc, sizeof(Scan));
	/*
	 * Increment the counters, accounting for rollover
	 */
	if (sq_nscans < SQ_MAX_SCANS)
		++sq_nscans;
	sq_next = (sq_next + 1) % SQ_MAX_SCANS;
	sq_flags |= SQ_RECALC | SQ_RESORT;
}



inline float
DegreesEastOf (float lon1, float lon2)
/*
 * Returns number degrees lon2 is east of lon1.
 * Guaranteed to be between -180 and 180.  Negative means lon1 is closest
 * to lon2 in the west direction.
 */
{
	if (fabs(lon2 - lon1) <= 180.0)
		return (lon2 - lon1);
	else if (lon2 - lon1 < -180.0)
		return (360 + lon2 - lon1);
	else
		return (lon2 - lon1 - 360);
}
			


void
Scan::MinMax (void)
/*
 * Calculate the scan's min and max lat/lon
 */
{
	int i;

	sc_minlat = sc_lat[0];
	sc_minlon = sc_lon[0];
	sc_maxlat = sc_lat[0];
	sc_maxlon = sc_lon[0];

	for (i = 1; i < 128; ++i)
	{
		if (sc_lat[i] < sc_minlat)
			sc_minlat = sc_lat[i];
		else if (sc_lat[i] > sc_maxlat)
			sc_maxlat = sc_lat[i];
		if (DegreesEastOf(sc_maxlon, sc_lon[i]) > 0)
			sc_maxlon = sc_lon[i];
		else if (DegreesEastOf(sc_minlon, sc_lon[i]) < 0)
			sc_minlon = sc_lon[i];
	}
}



void
SquareScan::MinMax (void)
/*
 * Calculate the min/max coords of the entire square scan from 
 * constituent scans
 */
{
	int i;
	int first;
	Scan *scan;

	if ((sq_flags & SQ_RECALC) == 0)	/* See if we really need it */
		return;

	if (sq_nscans == 0)		/* make sure we're not empty	*/
		return;

	first = (sq_next - sq_nscans + SQ_MAX_SCANS) % SQ_MAX_SCANS;
	scan = sq_buf + first;
	sq_minlat = scan->sc_minlat;
	sq_minlon = scan->sc_minlon;
	sq_maxlat = scan->sc_maxlat;
	sq_maxlon = scan->sc_maxlon;

	/*
	 * We don't use sq_ord[] since it may not be valid, and it may
	 * be costly to figure it out as well.  Besides, we don't need it.
	 */
	for (i = 1; i < sq_nscans; ++i)
	{
		scan = &(sq_buf[(first + i) % SQ_MAX_SCANS]);
		if (sq_minlat > scan->sc_minlat)
			sq_minlat = scan->sc_minlat;
		else if (sq_maxlat < scan->sc_maxlat)
			sq_maxlat = scan->sc_maxlat;
		if (DegreesEastOf(sq_maxlon, scan->sc_maxlon) > 0)
			sq_maxlon = scan->sc_maxlon;
		else if (DegreesEastOf(sq_minlon, scan->sc_minlon) < 0)
			sq_minlon = scan->sc_minlon;
	}
	sq_flags &= ~SQ_RECALC;
}



static void
ConvertLRecLon (float *lon)
/*
 * Convert from 0<->360 range to -180<->180
 */
{
	int i;

	for (i = 0; i < 128; ++i)
	{
		if (lon[i] > 180.0)
			lon[i] -= 360.0;
	}
}



void 
SquareScan::AddOutdat (const OUTDAT_BLOCK *dat)
/*
 * Add the A- and B- scan pair in the OUTDAT_BLOCK to the square scan
 */
{
	Scan *scan;

	/*
	 * Rather than create two intermediate scans, copy data directly
	 */
	scan = &(sq_buf[sq_next]);

	/*
	 * A-scan first
	 */
	scan->sc_type = A_scan;
	scan->sc_xtime = dat->xtime - 1.9; /* xtime actually time of B-scan */
	memcpy((char *)(scan->sc_lat), (char *)(dat->alat), 128*sizeof(float));
	memcpy((char *)(scan->sc_lon), (char *)(dat->alon), 128*sizeof(float));
	ConvertLRecLon (scan->sc_lon);
	memcpy((char *)(scan->sc_tahi), (char *)(dat->atahi), 
	       128 * 2 * sizeof(float));
	memcpy((char *)(scan->sc_itoil), (char *)(dat->iatoil),
	       128 * sizeof(int));
	/*
	 * Lo-frequencies are stored in the A-scans, so we need to copy talo[]
	 */
	memcpy((char *)(scan->sc_talo), (char *)(dat->talo),
	       64*5*sizeof(float));
	scan->MinMax();
	sq_next = (sq_next + 1) % SQ_MAX_SCANS;

	/*
	 * Now for the B-scan
	 */
	scan = &(sq_buf[sq_next]);
	scan->sc_type = B_scan;
	scan->sc_xtime = dat->xtime;
	memcpy((char *)(scan->sc_lat), (char *)(dat->blat), 128*sizeof(float));
	memcpy((char *)(scan->sc_lon), (char *)(dat->blon), 128*sizeof(float));
	ConvertLRecLon (scan->sc_lon);
	memcpy((char *)(scan->sc_tahi), (char *)(dat->btahi), 
	       128 * 2 * sizeof(float));
	memcpy((char *)(scan->sc_itoil), (char *)(dat->ibtoil),
	       128 * sizeof(int));
	/*
	 * Don't need to worry about dat->talo[] for B-scans
	 */
	scan->MinMax();
	sq_next = (sq_next + 1) % SQ_MAX_SCANS;

	if (sq_nscans < (SQ_MAX_SCANS - 2))
		sq_nscans += 2;
	else
		sq_nscans = SQ_MAX_SCANS;
	sq_flags |= (SQ_RECALC | SQ_RESORT);
}



void
SquareScan::Clear (void)
/*
 * Reset the SquareScan to emtpy
 */
{
	sq_flags = SQ_RESORT | SQ_RECALC;
	sq_nscans = 0;
	sq_next = 0;
}


SquareScan::SquareScan (void)
/*
 * The SquareScan constructor just initializes to empty
 */
{
	Clear();
}


void
SquareScan::Range (float *minlat, float *minlon, float *maxlat, float *maxlon)
/*
 * Return the range of lat/lon coordinates covered by this SquareScan.  Adjust
 * the range a little bit so that on average it fits inside and along the
 * border of the cells in the square.  If any of the arguments are NULL, no
 * value is returned for that parameter.
 */
{
	float latstep, lonstep;

	MinMax ();
	latstep = ((sq_maxlat - sq_minlat) / (sq_nscans)) * 2.5;
	lonstep = (DegreesEastOf(sq_minlon, sq_maxlon) / 128.0) * 2.5;

	if (minlat)
		*minlat = sq_minlat /* + latstep */;
	if (minlon)
	{
		*minlon = sq_minlon /* + lonstep */;
		if (*minlon > 180.0)
			*minlon -= 360;
	}
	if (maxlat)
		*maxlat = sq_maxlat /* - latstep */;
	if (maxlon)
	{
		*maxlon = sq_maxlon /* - lonstep */;
		if (*maxlon < -180.0)
			*maxlon += 360;
	}
}



void
SquareScan::Origin (Location *locn)
/*
 * Return an origin for the scans using the minimum lat and lon of all
 * scans in the SquareScan.  Returns the origin in *locn.  Undefined
 * if the SquareScan contains no scans.
 */
{
	Range (&locn->l_lat, &locn->l_lon, NULL, NULL);
	locn->l_alt = 0;
}



void
SquareScan::GridInfo (double res, RGrid *info, const Location& cvt_origin)
/*
 * Fill in the grid info structure with the optimum grid for the given
 * SquareScan and the specified resolution (km).
 */
{
	float lat1, lon1, lat2, lon2;
	float x1, y1;
	float x2, y2;

	/*
	 * Find the lat/lon range of our scans
	 */
	Range (&lat1, &lon1, &lat2, &lon2);

	/*
	 * Convert the range to x,y domain
	 */
	cvt_Origin (cvt_origin.l_lat, cvt_origin.l_lon);
	cvt_ToXY (lat1, lon1, &x1, &y1);
	cvt_ToXY (lat2, lon2, &x2, &y2);

	/*
	 * Fill in the info structure so that the grid matches
	 * the lat/lon range for the given resolution.
	 */
	info->rg_Xspacing = res;
	info->rg_Yspacing = res;
	info->rg_Zspacing = 0.0;
	info->rg_nZ = 1;
	info->rg_nX = (int)((x2 - x1)/res + 0.5);
	info->rg_nY = (int)((y2 - y1)/res + 0.5);
}



void
SquareScan::Order (void)
/*
 * For now, this sets up the sq_ord array in the order the scans were
 * added, since it is not clear at this point that ordering the scans
 * in any particular way will help speed up the gridding process
 */
{
	int i;
	int first;

	if (((sq_flags & SQ_RESORT) == 0) && ((sq_flags & SQ_ORDERED) != 0))
		return;
	first = (sq_next - sq_nscans + SQ_MAX_SCANS) % SQ_MAX_SCANS;
	for (i = 0; i < sq_nscans; ++i)
	{
		sq_ord[i] = &(sq_buf[(first + i) % SQ_MAX_SCANS]);
	}
	sq_flags |= SQ_ORDERED;
	sq_flags &= ~SQ_RESORT;
}



void
SquareScan::ZebTime (ZebTime *zt)
/*
 * Fill *zt with the time of the this square scan using the time of the
 * very first scan.  If there are no scans, the results are undefined.
 */
{
	Scan *scan;
	int first;

	if (sq_nscans == 0)
		return;
	first = (sq_next - sq_nscans + SQ_MAX_SCANS) % SQ_MAX_SCANS;
	scan = sq_buf + first;
	/*
	 * Time of the scan, scan->sc_xtime, is in seconds since 1-1-1987
	 */
	TC_ZtAssemble (zt, 87, 1, 1, 0, 0, 0, 0);
	zt->zt_Sec += (unsigned long) scan->sc_xtime;
	zt->zt_MicroSec = 
		(long)((scan->sc_xtime - (long)scan->sc_xtime)*10E+06);
}



float
SquareScan::CellValue (int scan, int cell, Channel ch)
/*
 * Return the float value of this channel at this scan/cell in the scan
 * square.  The scan is the scan line, cell is the scan cell.  Usually the
 * scan/cell will be determined by a call to GridToCell().  The scan/cell
 * coord gives the lat/lon cell in the SquareScan, but the lo-freq data for
 * that cell will be at cell/2 (since talo[] only stores 64 elements).  If
 * this is a low-freq channel, then our scan should not be a B_scan, else
 * the talo[] temperature is invalid.
 */
{
	Scan *scanp = sq_ord[scan];

	switch (ch)
	{
	   case ch19v:
	   case ch19h:
	   case ch22v:
	   case ch37v:
	   case ch37h:
		return (scanp->sc_talo[cell/2][(int)ch - 1]);
	   case ch85v:
	   case ch85h:
		return (scanp->sc_tahi[cell][(int)ch-(int)ch85v-1]);
	   case sfcidx:
		return ((float)(scanp->sc_itoil[cell]));
	   default:
		return (0);
	}
	return (0);
}
