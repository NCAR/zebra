/*
 * The Scan structures
 *
 * $Id: scan.h,v 1.1 1993-06-10 02:26:33 granger Exp $
 */

#ifndef _scan_h_
#define _scan_h_

#include <defs.h>		/* For FP() definition			*/
#include "outdat.h"		/* The C mapping to OUTDAT common block */

typedef enum { NO_SCAN_TYPE = 0, A_scan, B_scan } ScanType;
typedef enum { NO_CHANNEL = 0, 
	       ch19v, ch19h, ch22v, ch37v, ch37h, 
	       ch85v, ch85h } Channel;

#define SQ_RECALC (1)		/* recalc min/max lat/lon values	*/
#define SQ_RESORT (1<<1)	/* scan added, needs re-sort		*/
#define SQ_ORDERED (1<<2)	/* sq_ord array valid			*/

typedef struct _Scan {
	ScanType sc_type;	/* A or B scan				*/
	double sc_xtime;	/* taken from xtime of logical rec 	*/
	float sc_lat[128];	/* latitudes of 128 cells in scan 	*/
	float sc_lon[128];	/* longitudes of 128 cells in scan	*/
	float sc_talo[64][5];	/* Temperatures of A-scan lo-freq chnls */
	float sc_tahi[128][2];	/* Temperatures of hi-freq channels	*/
	float sc_minlat;	/* Lat/lon range of all 128 cells	*/
	float sc_minlon;	/* means Westernmost			*/
	float sc_maxlat;
	float sc_maxlon;
} Scan;


typedef struct _SquareScan {
	Scan sq_buf[128];	/* 128 A- and B- scans in one square 	*/
	int sq_next;		/* Where to store next scan in buffer	*/
	int sq_nscans;		/* Number valid scans in buffer		*/
	Scan *sq_ord[128];	/* Sorted pointers into sq_buf[]	*/
	unsigned short sq_flags;/* Flag re-sorting and recalcing	*/
	float sq_minlat;	/* Lat/lon range of all scans in buffer	*/
	float sq_minlon;	/* means Westernmost			*/
	float sq_maxlat;
	float sq_maxlon;
} SquareScan;


typedef struct _GridMap {
	RGrid gm_info;		/* Info on the grid we're mapping	*/
	Channel gm_channel;	/* Channel used to generate this mapping*/
	int *gm_scan;		/* grid-y-to-scan-number map goes here	*/
	int *gm_cell;		/* grid-x-to-scan-cell map goes here	*/
} GridMap;

/*
 * Function prototypes
 */
void SqAddLRec FP((SquareScan *ss, OUTDAT_BLOCK *dat));
void SqAddScan FP((SquareScan *ss, Scan *scan));
int SqIsFull FP((SquareScan *ss));
void SqClear FP((SquareScan *ss));
void SqOrigin FP((SquareScan *ss, Location *locn));
void SqOrder FP((SquareScan *ss));
void SqZebTime FP((SquareScan *ss, ZebTime *zt));
void BuildGridMap FP((SquareScan *ss, GridMap *gm, RGrid *info, Channel ch));
void FreeGridMap FP((GridMap *gm));
void GridToCell FP((GridMap *gm,
		    int ix, int iy,	/* Grid cells		 */
		    int *scan, int *cell/* SquareScan scan and cell*/));
float SqCellValue FP((SquareScan *ss, int scan, int cell, Channel ch));

/*
 * Macro interfaces
 */
#define SqCellLat(ss,scan,cell) (((ss)->sq_ord[(scan)])->sc_lat[(cell)])
#define SqCellLon(ss,scan,cell) (((ss)->sq_ord[(scan)])->sc_lon[(cell)])
#define SqIsFull(ss) ((ss)->sq_nscans == 128)
#define SqNumScans(ss) ((ss)->sq_nscans)

#endif
