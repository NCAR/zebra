/*
 * $Id: scan.c,v 1.1 1993-06-10 02:26:30 granger Exp $
 *
 * Functions for manipulating scans and square-scans
 */

#include <string.h>
#include <memory.h>
#include <math.h>
#include <defs.h>
#include <DataStore.h>
#include "scan.h"

#define SQR(x) ((x)*(x))

/*
 * Privately used types and functions
 */

typedef struct _CellBucket {
	struct _CellBucket *cb_next;	/* Next cell in bucket, or NULL  */
	int cb_scan;			/* SquareScan coords of the cell */
	int cb_cell;			/* falling into this bucket	 */
	float cb_x;			/* (x,y) of scan cell relative 	 */
	float cb_y;			/*    to grid origin.		 */
} CellBucket;

static void FreeBuckets FP((CellBucket *buckets, int size));

void SqAddScan (ss, sc)
SquareScan *ss;
Scan *sc;
/*
 * Add the given scan to the square
 */
{
	/*
	 * Copy scan into the next storage spot in the buffer
	 */
	memcpy ((char *)(ss->sq_buf + ss->sq_next),
		(char *)sc, sizeof(Scan));
	/*
	 * Increment the counters, accounting for rollover
	 */
	if (ss->sq_nscans < 128)
		++(ss->sq_nscans);
	ss->sq_next = (ss->sq_next + 1) % 128;
	ss->sq_flags |= SQ_RECALC | SQ_RESORT;
}



inline float
DegreesEastOf (lon1, lon2)
float lon1, lon2;
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
ScanMinMax (scan)
Scan *scan;
/*
 * Calculate the scan's min and max lat/lon
 */
{
	int i;

	scan->sc_minlat = scan->sc_lat[0];
	scan->sc_minlon = scan->sc_lon[0];
	scan->sc_maxlat = scan->sc_lat[0];
	scan->sc_maxlon = scan->sc_lon[0];

	for (i = 1; i < 128; ++i)
	{
		if (scan->sc_lat[i] < scan->sc_minlat)
			scan->sc_minlat = scan->sc_lat[i];
		else if (scan->sc_lat[i] > scan->sc_maxlat)
			scan->sc_maxlat = scan->sc_lat[i];
		if (DegreesEastOf(scan->sc_maxlon, scan->sc_lon[i]) < 0)
			scan->sc_maxlon = scan->sc_lon[i];
		else if (DegreesEastOf(scan->sc_minlon, scan->sc_lon[i]) > 0)
			scan->sc_minlon = scan->sc_lon[i];
	}
}



void
SquareMinMax (ss)
SquareScan *ss;
/*
 * Calculate the min/max coords of the entire square scan from 
 * constituent scans
 */
{
	int i;
	int first;
	Scan *scan;

	if ((ss->sq_flags & SQ_RECALC) == 0)	/* See if its already done */
		return;

	if (ss->sq_nscans == 0)		/* make sure we're not empty	*/
		return;

	first = (ss->sq_next - ss->sq_nscans + 128) % 128;
	scan = ss->sq_buf + first;
	ss->sq_minlat = scan->sc_minlat;
	ss->sq_minlon = scan->sc_minlon;
	ss->sq_maxlat = scan->sc_maxlat;
	ss->sq_maxlon = scan->sc_maxlon;

	/*
	 * We don't use sq_ord[] since it may not be valid, and it may
	 * be costly to figure it out as well.  Besides, we don't need it.
	 */
	for (i = 1; i < ss->sq_nscans; ++i)
	{
		scan = &(ss->sq_buf[(first + i) % 128]);
		if (ss->sq_minlat > scan->sc_minlat)
			ss->sq_minlat = scan->sc_minlat;
		else if (ss->sq_maxlat < scan->sc_maxlat)
			ss->sq_maxlat = scan->sc_maxlat;
		if (DegreesEastOf(ss->sq_maxlon, scan->sc_maxlon) < 0)
			ss->sq_maxlon = scan->sc_maxlon;
		else if (DegreesEastOf(ss->sq_minlon, scan->sc_minlon) > 0)
			ss->sq_minlon = scan->sc_minlon;
	}
	ss->sq_flags &= ~SQ_RECALC;
}



void SqAddLRec (ss, dat)
SquareScan *ss;
OUTDAT_BLOCK *dat;
/*
 * Add the A- and B- scan pair in the OUTDAT_BLOCK to the square scan
 */
{
	Scan *scan;

	/*
	 * Rather than create two intermediate scans, copy data directly
	 */
	scan = ss->sq_buf + ss->sq_next;

	/*
	 * A-scan first
	 */
	scan->sc_type = A_scan;
	scan->sc_xtime = dat->xtime - 1.9; /* xtime actually time of B-scan */
	memcpy((char *)(scan->sc_lat), (char *)(dat->alat), 128*sizeof(float));
	memcpy((char *)(scan->sc_lon), (char *)(dat->alon), 128*sizeof(float));
	/*
	 * Don't need to worry about dat->talo[] for A-scans
	 */
	memcpy((char *)(scan->sc_tahi), (char *)(dat->atahi), 
	       128*sizeof(float));
	ScanMinMax (scan);
	ss->sq_next = (ss->sq_next + 1) % 128;

	/*
	 * Now for the B-scan
	 */
	scan = ss->sq_buf + ss->sq_next;
	scan->sc_type = A_scan;
	scan->sc_xtime = dat->xtime;
	memcpy((char *)(scan->sc_lat), (char *)(dat->blat), 128*sizeof(float));
	memcpy((char *)(scan->sc_lon), (char *)(dat->blon), 128*sizeof(float));
	memcpy((char *)(scan->sc_tahi), (char *)(dat->atahi), 
	       128*2*sizeof(float));
	memcpy((char *)(scan->sc_talo), (char *)(dat->talo),
	       64*5*sizeof(float));
	ScanMinMax (scan);
	ss->sq_next = (ss->sq_next + 1) % 128;

	if (ss->sq_nscans < 126)
		ss->sq_nscans += 2;
	else
		ss->sq_nscans = 128;
	ss->sq_flags |= (SQ_RECALC | SQ_RESORT);
}



void
SqClear (ss)
SquareScan *ss;
/*
 * Reset the SquareScan to emtpy
 */
{
	ss->sq_flags = SQ_RESORT | SQ_RECALC;
	ss->sq_nscans = 0;
	ss->sq_next = 0;
}



void
SqOrigin (ss, locn)
SquareScan *ss;
Location *locn;
/*
 * Return an origin for the scans using the minimum lat and lon of all
 * scans in the SquareScan.  Returns the origin in *locn.  Undefined
 * if the SquareScan contains no scans
 */
{
	SquareMinMax (ss);
	locn->l_lat = ss->sq_minlat;
	locn->l_lon = ss->sq_minlon;
	locn->l_alt = 0;
}



void SqOrder (ss)
SquareScan *ss;
/*
 * For now, this sets up the sq_ord array in the order the scans were
 * added, since it is not clear at this point that ordering the scans
 * in any particular way will help speed up the gridding process
 */
{
	int i;
	int first;

	if ((ss->sq_flags & SQ_RESORT == 0) &&
	    (ss->sq_flags & SQ_ORDERED != 0))
		return;
	first = (ss->sq_next - ss->sq_nscans + 128) % 128;
	for (i = 0; i < ss->sq_nscans; ++i)
	{
		ss->sq_ord[i] = &(ss->sq_buf[(first + i) % 128]);
	}
	ss->sq_flags |= SQ_ORDERED;
	ss->sq_flags &= ~SQ_RESORT;
}



void
SqZebTime (ss, zt)
SquareScan *ss;
ZebTime *zt;
/*
 * Fill *zt with the time of the this square scan using the time of the
 * very first scan.  If there are no scans, the results are undefined.
 */
{
	Scan *scan;
	int first;

	if (ss->sq_nscans == 0)
		return;
	first = (ss->sq_next - ss->sq_nscans + 128) % 128;
	scan = ss->sq_buf + first;
	/*
	 * Time of the scan, scan->sc_xtime, is in seconds since 1-1-1987
	 */
	TC_ZtAssemble (zt, 87, 1, 1, 0, 0, 0, 0);
	zt->zt_Sec += (unsigned long)scan->sc_xtime;
	zt->zt_MicroSec = 
		(long)((scan->sc_xtime - (long)scan->sc_xtime)*10E+06);
}


/*
 * Now for the tough part: create a grid map using the given RGrid info for
 * the given channel.  If the channel is hi-freq, then we use all of the
 * available cells and their lat/lon coords to match grid points to scan
 * cells.  If the channel is a lo-freq, then we can only use the A-scans
 * from the square scan, and the lat/lon coords of the A-scan lo-freq
 * channels, which are the odd elements (even indices) of the lat/lon
 * arrays in each scan.
 *
 * We want to save some stats in the grid map structure as well.
 */
static void
FillBucketsFromSquare (buckets, ss, gm, ch)
CellBucket *buckets;
SquareScan *ss;
GridMap *gm;
Channel ch;
/*
 * Put ALL SquareScan cells into closest possible grid element bucket.
 */
{
	Scan *scan;		/* The current scan we're traversing	*/
	CellBucket *cb;		/* The cell bucket we're inserting	*/
	int iscan, icell;	/* Current cell coord in SquareScan	*/
	Location origin;	/* lat/lon location of origin of grid	*/
	float lat, lon;		/* lat/lon of current cell		*/
	float x, y;		/* (x,y) of cell, relative to origin	*/
	int ix, iy;		/* Grid point of current cell		*/
	int i;

	SqOrigin (ss, &origin);
	cvt_Origin (origin.l_lat, origin.l_lon);
	/*
	 * Traverse all of the SquareScan cells and find a place for
	 * each.  Also put the (x,y) coordinate of the cell, relative
	 * to the grid origin, into the cell bucket.
	 */
	for (iscan = 0; iscan < ss->sq_nscans; ++iscan)
	{
		scan = ss->sq_ord[iscan];
		if ((ch < ch85v) && (scan->sc_type != A_scan))
			continue;
		for (icell = 0; icell < 128; icell += (ch < ch85v) ? 2 : 1)
		{
			lat = scan->sc_lat[icell];
			lon = scan->sc_lon[icell];
			cvt_ToXY (lat, lon, &x, &y);
			/*
			 * Since the origin is minlat and minlon, (x > 0)
			 * and (y > 0), so the grid element is:
			 */
			ix = (int)(x/gm->gm_info.rg_Xspacing + 0.5);
			iy = (int)(y/gm->gm_info.rg_Yspacing + 0.5);
			if (ix > gm->gm_info.rg_nX)
				ix = gm->gm_info.rg_nX - 1;
			if (iy > gm->gm_info.rg_nY)
				iy = gm->gm_info.rg_nY - 1;
			i = (iy * gm->gm_info.rg_nX) + ix;
			if (buckets[i].cb_scan == -1)	/* empty bucket */
			{
				cb = buckets + i;
				cb->cb_next = NULL;
			}
			else
			{
				/*
				 * We only need to allocate new cell
				 * buckets for grid points which are
				 * already non-empty
				 */
				cb = ALLOC(CellBucket);
				cb->cb_next = buckets[i].cb_next;
				buckets[i].cb_next = cb;
			}
			cb->cb_scan = iscan;
			cb->cb_cell = icell;
			cb->cb_x = x;
			cb->cb_y = y;
		}
	}
}



inline static void
FillFromBucket (buckets, info, ix, iy, cx, cy, mindist)
CellBucket *buckets;	/* Bucket grid */
RGrid *info;		/* Grid info */
int ix, iy;		/* Coord of bucket being filled */
int cx, cy;		/* Coord of bucket being filled from */
float *mindist;		/* Distance of closest cell to bucket so far */
{
	CellBucket *bucket, *closest;
	float least;
	float x, y;	/* x,y coords of current bucket */
	int i;

	/*
	 * Check cx,cy within limits
	 */
	if ((cx >= info->rg_nX) || (cx < 0) ||
	    (cy >= info->rg_ny) || (cy < 0))
		return;
	/*
	 * Check fill-from bucket is not empty
	 */
	i = cy * info->rg_nX + cx;
	if (buckets[i].cb_scan == -1)
		return;

	/*
	 * Search cells in the fill-from bucket
	 */
	cb = buckets + i;
	x = ix * info->rg_Xspacing;
	y = iy * info->rg_Yspacing;

	closest = cb;
	least = SQR(x - cb->cb_x) + SQR(y - cb->cb_y);
	while (cb = cb->cb_next)
	{
		metric = SQR(x - cb->cb_x) + SQR(y - cb->cb_y);
		if (metric < least)
		{
			least = metric;
			closest = cb;
		}
	}

	/*
	 * If target is empty or we found something closer, 
	 * take what we got
	 */
	bucket = buckets + (iy * info->rg_nX) + ix;
	if ((bucket->cb_scan == -1) || (least < *mindist))
	{
		*bucket = *closest;
		bucket->cb_next = NULL;
		*mindist = least;
	}
}




static void
FillFromSurrounding (buckets, info, ix, iy)
CellBucket *buckets;	/* The grid of buckets */
RGrid *info;		/* Grid info */
int ix, iy;		/* The bucket we're trying to fill */
{
	float metric;	/* Distance of found cell to target grid point */
	int cx, cy;	/* location of current search bucket */
	int radius;	/* Radius of current search */
	float mindist;	/* Minimum distance of cells found so far */
	float dx, dy;	/* Direction we are stepping around perimeter */
	int i;		/* index into grid and bucets array */
	CellBucket *bucket;	/* The bucket we're trying to fill */
	int min_spacing;

	bucket = buckets + (iy * info->rg_nX + ix);
	/*
	 * Start in a square one cell from target cell.  Search all cells
	 * at the perimeter of the square, inserting the closest into
	 * the target bucket.  As long as bucket is empty or radius is 
	 * less than mindist, keep searching.
	 */
	radius = 1;

	min_spacing = (info->rg_Xspacing < info->rg_Yspacing) ?
		(info->rg_Xspacing) : (info->rg_Yspacing);
	while ((bucket->cb_scan == -1) ||
	       ((radius - 0.5) * min_spacing < mindist))
	/*
	 * Search perimeter of square
	 */
	for (cy = iy - radius; cy <= iy + radius; ++cy)
	{
		if ((cy == iy - radius) || (cy == iy + radius))
		{
			for (cx = ix - radius; cx <= ix + radius; ++cx)
			{
				FillFromBucket (buckets, info, ix, iy,
						cx, cy, &mindist);
			}
		} 
		else
		{
			FillFromBucket (buckets, info, ix, iy,
					ix-radius, cy, &mindist);
			FillFromBucket (buckets, info, ix, iy,
					ix+radius, cy, &mindist);
		}
	}
}




/*
 * Now it gets really interesting... For each empty bucket, use the
 * surrounding non-empty buckets to fill it.  Keep track of the distance
 * between the grid point and the closest cell found so far.  Abandon
 * the search once any further searching would only discover cells which
 * have to be further away.
 */
static void
FillEmptyBuckets (buckets, ss, gm, ch)
CellBucket *buckets;
SquareScan *ss;
GridMap *gm;
Channel ch;
{
	int iy, ix;
	RGrid *info;
	int i;

	/*
	 * Traverse the buckets, skip those that aren't empty
	 */
	info = &(gm->gm_info);
	for (iy = 0; iy < info->rg_nY; ++iy)
	{
		i = iy*info->rg_nX;
		for (ix = 0; ix < info->rg_nX; ++ix, ++i)
		{
			if (buckets[i].cb_scan != -1)	/* non-empty bucket */
				continue;

			/*
			 * Search surrounding buckets to fill it
			 */
			FillFromSurrounding (buckets, info, ix, iy);

			cb = &buckets[i];
			/*
			 * If bucket contains only one cell, just use it
			 * and go on.  Otherwise, use it as a first
			 * possiblity and test it against the rest of the
			 * cells.  The hope is to avoid as much computation
			 * as possible.
			 */
			if (cb->cb_next == NULL)
			{
				gm->gm_scan[i] = cb->cb_scan;
				gm->gm_cell[i] = cb->cb_cell;
				continue;
			}
			x = ix * info->rg_Xspacing;
			y = iy * info->rg_Yspacing;
			closest = cb;
			least = SQR(x - cb->cb_x) + SQR(y - cb->cb_y);
			while (cb = cb->cb_next)
			{
				metric = SQR(x - cb->cb_x) + SQR(y - cb->cb_y);
				if (metric < least)
				{
					least = metric;
					closest = cb;
				}
			}
			gm->gm_scan[i] = closest->cb_scan;
			gm->gm_cell[i] = closest->cb_cell;
		}
	}

}




static void
FillGridMap (buckets, gm)
CellBucket *buckets;
GridMap *gm;
/*
 * For each bucket, put the cell in the bucket closest to the bucket's
 * grid point into the grid map
 */
{
	int i, ix, iy;
	CellBucket *cb, *closest;
	float metric, least;
	RGrid *info;
	float x,y;	/* Location of grid point relative to grid origin */

	info = &(gm->gm_info);
	for (iy = 0; iy < info->rg_nY; ++iy)
	{
		i = iy*info->rg_nX;
		for (ix = 0; ix < info->rg_nX; ++ix, ++i)
		{
			cb = &buckets[i];
			/*
			 * If bucket contains only one cell, just use it
			 * and go on.  Otherwise, use it as a first
			 * possiblity and test it against the rest of the
			 * cells.  The hope is to avoid as much computation
			 * as possible.
			 */
			if (cb->cb_next == NULL)
			{
				gm->gm_scan[i] = cb->cb_scan;
				gm->gm_cell[i] = cb->cb_cell;
				continue;
			}
			x = ix * info->rg_Xspacing;
			y = iy * info->rg_Yspacing;
			closest = cb;
			least = SQR(x - cb->cb_x) + SQR(y - cb->cb_y);
			while (cb = cb->cb_next)
			{
				metric = SQR(x - cb->cb_x) + SQR(y - cb->cb_y);
				if (metric < least)
				{
					least = metric;
					closest = cb;
				}
			}
			gm->gm_scan[i] = closest->cb_scan;
			gm->gm_cell[i] = closest->cb_cell;
		}
	}
}



/*

 * The steps in building the map: 
 * ------------------------------

 * Allocate space for the gm_scan and gm_cell arrays, each the size of the
 * grid, and initialize them to -1.

 * Get a first approximation by filling in gm_scan and gm_cell by
 * traversing all of the cells in the SquareScan.

 * Then try the other direction, traversing each grid point that was not
 * filled in.  Find the closest scan cell to each grid point by using the
 * closest, previously-filled grid point.  Bucket all scan cells which fall
 * into a grid cell so that nearby grid cells can select the closest scan
 * cell in the next step.

 * Then we should be done.  Any of that make any sense?  Essentially the
 * problem is that the 'scan cell --> grid element' mapping is not 1-1 nor
 * onto.  However we assume continuity when picking the closest scan cell
 * as the value for nearby grid cells.

 * 
 */
void
BuildGridMap (ss, gm, info, ch)
SquareScan *ss;
GridMap *gm;
RGrid *info;
Channel ch;
{
	int grid_size;
	int i;
	CellBucket *buckets;

	/*
	 * Allocate scan and cell maps to hold entries for every grid
	 * coordinate, then initialize all entries to -1, meaning empty
	 */
	grid_size = info->rg_nX * info->rg_nY;
	gm->gm_scan = (int *)malloc (grid_size * sizeof(int));
	gm->gm_cell = (int *)malloc (grid_size * sizeof(int));
	for (i = 0; i < grid_size; ++i)
		gm->gm_scan[i] = gm->gm_cell[i] = -1;

	/*
	 * Finish setting up the grid map
	 */
	gm->gm_info = *info;
	gm->gm_channel = ch;

	/*
	 * Create a cell bucket to hold the coords of all of the cells
	 * which fall into the area of each grid element.  Each element
	 * of the grid gets one bucket.
	 */
	buckets = (CellBucket *)malloc (grid_size * sizeof(CellBucket));
	for (i = 0; i < grid_size; ++i)
		buckets[i].cb_scan = buckets[i].cb_cell = -1;

	/*
	 * Fill the grid buckets with all of the SquareScan cells
	 */
	FillBucketsFromSquare (buckets, ss, gm, ch);

	/*
	 * Now fill in all of the buckets which are still empty with the
	 * nearest cells from the surrounding non-empty buckets.
	 */
	FillEmptyBuckets (buckets, ss, gm, ch);

	/*
	 * By now, every grid bucket should have at least one cell in it.
	 * Traverse all the buckets, taking the cell from each bucket
	 * which is closest to the grid point for the grid map.
	 */
	FillGridMap (buckets, gm);
	FreeBuckets (buckets, grid_size);
}



static void
FreeBuckets (buckets, size)
CellBucket *buckets;
int size;
{
	int i;

	for (i = 0; i < size; ++i)
	{
		CellBucket *next = buckets[i].cb_next;
		while (next)
		{
			CellBucket *this = next;
			next = this->cb_next;
			free (this);
		}
	}
	free (buckets);
}



void
FreeGridMap (gm)
GridMap *gm;
{
	free (gm->gm_scan);
	free (gm->gm_cell);
}



inline void GridToCell (gm, ix, iy, scan, cell)
GridMap *gm;
int ix, iy;		/* Grid cells		*/
int *scan, *cell;	/* SquareScan coords	*/
{
	*scan = gm->gm_scan[ iy*(gm->gm_info.rg_nX) + ix ];
	*cell = gm->gm_cell[ iy*(gm->gm_info.rg_nX) + ix ];
}




inline float
SqCellValue (ss, scan, cell, ch)
SquareScan *ss;
int scan, cell;
Channel ch;
/*
 * Return the float value of this channel at this scan/cell in the scan square.
 * The scan is the scan line, cell is the scan cell.  Usually the scan/cell
 * will be determined by a call to GridToSquareScan().  The scan/cell coord
 * gives the lat/lon cell in the SquareScan, but the lo-freq data for that
 * cell will be at cell/2 (since talo[] only stores 64 elements).  If this is
 * a low-freq channel, then our scan should not be a B_scan, else the talo[]
 * temperatures are invalid.
 */
{
	switch (ch)
	{
	   case ch19v:
	   case ch19h:
	   case ch22v:
	   case ch37v:
	   case ch37h:
		return ((ss->sq_ord[scan])->sc_talo[cell/2][(int)ch - 1]);
	   case ch85v:
	   case ch85h:
		return (ss->sq_ord[scan])->sc_tahi[cell][(int)ch-(int)ch85v-1];
	   default:
		return (0);
	}
}
