/* Just so everyone knows, this is -*- C++ -*-
 *
 * The Scan structures
 *
 * $Id: SquareScan.h,v 1.4 1994-11-16 19:31:05 granger Exp $
 */

#ifndef _SquareScan_h_
#define _SquareScan_h_

#include <defs.h>		/* For ZebTime */
#include "outdat.h"		/* The C mapping to OUTDAT common block */

#define SQ_MAX_SCANS 500	/* Upper limit on no. of scans in a sqr */

typedef enum { NO_SCAN_TYPE = 0, A_scan, B_scan } ScanType;
/*
 * Low-frequency channels must be enumerated consecutively, as must
 * the high-frequency channels, so the inline High and Low functions work.
 */
typedef enum { NO_CHANNEL = 0, 
	       ch19v, ch19h, ch22v, ch37v, ch37h, 
	       ch85v, ch85h, sfcidx } Channel;

typedef enum { NOT_TEMPERATURE = 0, TA, TB } TempType;

inline int High (Channel ch)	// surface indices count as high-frequency
{
	return ((ch >= ch85v) && (ch <= sfcidx));
}

inline int Low (Channel ch)
{
	return ((ch >= ch19v) && (ch <= ch37h));
}

const unsigned short SQ_RECALC = 1;	/* recalc min/max lat/lon values*/
const unsigned short SQ_RESORT = 1<<1;	/* scan added, needs re-sort	*/
const unsigned short SQ_ORDERED = 1<<2;	/* sq_ord array valid		*/


typedef struct _Scan
{
	ScanType sc_type;	/* A or B scan				*/
	double sc_xtime;	/* taken from xtime of logical rec 	*/
	float sc_lat[128];	/* latitudes of 128 cells in scan 	*/
	float sc_lon[128];	/* longitudes of 128 cells in scan	*/
	float sc_talo[64][5];	/* Temperatures of A-scan lo-freq chnls */
	float sc_tahi[128][2];	/* Temperatures of hi-freq channels	*/
	int sc_itoil[128];	/* Surface-type index			*/
	float sc_minlat;	/* Lat/lon range of all 128 cells	*/
	float sc_minlon;	/* means Westernmost			*/
	float sc_maxlat;
	float sc_maxlon;

	void MinMax (void);	// Our only method unique to simple scans
} Scan;


class GridMap;


class SquareScan
{
public:
	friend GridMap;

	SquareScan (void);
	void AddOutdat (const OUTDAT_BLOCK *dat);
	void AddScan (const Scan *scan);
	void Clear (void);
	void Origin (Location *locn);
#ifdef notdef
	inline void Origin (Location *locn)
		{ *locn = origin; }
	inline void SetOrigin (const Location& _origin)
		{ origin = _origin; }
#endif
	void Range (float *minlat, float *minlon,
		    float *maxlat, float *maxlon);
	void GridInfo (double res, RGrid *info, const Location& cvt_origin);
	void Order (void);
	void MinMax (void);
	void ZebTime (ZebTime *zt);
	float CellValue (int scan, int cell, Channel ch);
	void BuildGridMap (GridMap *gm, RGrid *info, Channel ch);
	inline float CellLat (int scan, int cell)
		{ return (sq_ord[scan]->sc_lat[cell]); }
	inline float CellLon (int scan, int cell)
		{ return (sq_ord[scan]->sc_lon[cell]); }
	inline int IsFull (void) { return (sq_nscans == SQ_MAX_SCANS); }
	inline int IsEmpty (void) { return (sq_nscans == 0); }
	inline int NumScans (void) { return ((int)sq_nscans); }

private:
	Scan sq_buf[SQ_MAX_SCANS];/* 128 A- and B- scans in one square 	*/
	int sq_next;		/* Where to store next scan in buffer	*/
	int sq_nscans;		/* Number valid scans in buffer		*/
	Scan *sq_ord[SQ_MAX_SCANS];/* Sorted pointers into sq_buf[]	*/
	unsigned short sq_flags;/* Flag re-sorting and recalcing	*/
	float sq_minlat;	/* Lat/lon range of all scans in buffer	*/
	float sq_minlon;	/* means Westernmost			*/
	float sq_maxlat;
	float sq_maxlon;
};


#endif /* ! _SquareScan_h_ */
