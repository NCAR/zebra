/*
 * The Scan structures
 *
 * $Id: scan.h,v 1.3 1994-02-02 23:49:50 burghart Exp $
 */

#ifndef _scan_h_
#define _scan_h_

#include <defs.h>		/* For FP() definition			*/
#include "outdat.h"		/* The C mapping to OUTDAT common block */

#define SQ_MAX_SCANS 	(500)	/* Upper limit on no. of scans in a sqr */

typedef enum { NO_SCAN_TYPE = 0, A_scan, B_scan } ScanType;
typedef enum { NO_CHANNEL = 0, 
	       ch19v, ch19h, ch22v, ch37v, ch37h, 
	       ch85v, ch85h, sfcidx } Channel;

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
	int sc_itoil[128];	/* Surface-type index			*/
	float sc_minlat;	/* Lat/lon range of all 128 cells	*/
	float sc_minlon;	/* means Westernmost			*/
	float sc_maxlat;
	float sc_maxlon;
} Scan;


typedef struct _SquareScan {
	Scan sq_buf[SQ_MAX_SCANS];/* 128 A- and B- scans in one square 	*/
	int sq_next;		/* Where to store next scan in buffer	*/
	int sq_nscans;		/* Number valid scans in buffer		*/
	Scan *sq_ord[SQ_MAX_SCANS];/* Sorted pointers into sq_buf[]	*/
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

	/*
	 * Unbeknownst to the application, we'll collect some stats in the
	 * grid map for review by the developer.
	 */
	int gm_sqr_hits;	/* Number hits from SquareScan --> Grid */
	int gm_sqr_mcbs;	/* Max cells/bucket in SS --> Grid step */
	float gm_sqr_cbs;	/* Avg cells/bucket from SS --> Grid 	*/

	int gm_cells;		/* Total number of cells searched	*/
	int gm_max_radius;	/* Max radius searched in bucket filling*/
	float gm_avg_radius;	/* Avg radius searched			*/
	float gm_avg_dist;	/* Avg metric dist bet. cell & grid pt	*/
	float gm_max_dist;	/* Max dist bet. cell & grid pt		*/

} GridMap;

/*
 * Function prototypes
 */
void SqAddLRec FP((SquareScan *ss, OUTDAT_BLOCK *dat));
void SqAddScan FP((SquareScan *ss, Scan *scan));
int SqIsFull FP((SquareScan *ss));
void SqClear FP((SquareScan *ss));
void SqOrigin FP((SquareScan *ss, Location *locn));
void SqRange FP((SquareScan *ss, float *minlat, float *minlon,
		 float *maxlat, float *maxlon));
void SqGridInfo FP((SquareScan *ss, double res, RGrid *info));
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
#define SqIsFull(ss) ((ss)->sq_nscans == SQ_MAX_SCANS)
#define SqIsEmpty(ss) ((ss)->sq_nscans == 0)
#define SqNumScans(ss) ((int)(ss)->sq_nscans)

#endif
