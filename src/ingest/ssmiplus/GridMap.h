/* -*- C++ -*- 
 *
 * $Id: GridMap.h,v 1.1 1994-07-31 06:17:29 granger Exp $
 */

#ifndef _GridMap_h_
#define _GridMap_h_

#include "SquareScan.h"


typedef struct _CellBucket {
	struct _CellBucket *cb_next;	/* Next cell in bucket, or NULL  */
	int cb_scan;			/* SquareScan coords of the cell */
	int cb_cell;			/* falling into this bucket	 */
	float cb_x;			/* (x,y) of scan cell relative 	 */
	float cb_y;			/*    to grid origin.		 */
} CellBucket;


class GridMap
{
public:
	GridMap (void);
	~GridMap (void);
	inline int Built (void)		// non-zero if we have a map built
		{ return (built); }
	void GridToCell (int ix, int iy,	// Grid cells
			 int *scan, int *cell);	// SquareScan scan and cell
	void FillBucketsFromSquare (CellBucket *buckets,
				    SquareScan *ss, Channel ch);
	void FillFromSurrounding (CellBucket *buckets, int ix, int iy);
	void FillEmptyBuckets (CellBucket *buckets);
	void FillGridMap (CellBucket *buckets);
	void FreeGridMap (void);
	void BuildMap (SquareScan *ss, RGrid *info, Channel ch);
	void FillImageGrid (unsigned char *image, SquareScan *ss, 
			    Channel ch, ScaleInfo *scale, 
			    unsigned char fill = '\0');
	void ReportStats (SquareScan *ss);
	inline float FillRadius (void)
		{ return (gm_fill_radius); }
	inline void SetFillRadius (float radius)
		{ if (radius > 0) gm_fill_radius = radius; }
	inline void SetCvtOrigin (Location& origin)
		{ gm_cvt_origin = origin; }

private:
	RGrid gm_info;		/* Info on the grid we're mapping	*/
	Channel gm_channel;	/* Channel used to generate this mapping*/
	int *gm_scan;		/* grid-y-to-scan-number map goes here	*/
	int *gm_cell;		/* grid-x-to-scan-cell map goes here	*/
	float gm_fill_radius;	/* Search radius in km from grid point	*/
	Location gm_cvt_origin;	/* origin for lat/lon -> x,y conversion */
	float gm_x0, gm_y0;	/* km coords of origin of grid		*/

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
	int gm_nmapped;		/* Number of grid points mapped		*/

	int built;
	void InitStats (void);
};


#endif /* ! _GridMap_h_ */
