/* -*- C++ -*-
 *
 * $Id: GridMap.cc,v 1.2 1994-11-16 19:25:05 granger Exp $
 *
 * Functions for filling grid maps and cell buckets from square scans
 */

#include <string.h>
#include <memory.h>
#include <math.h>
#include <assert.h>

extern "C" 
{

#include <defs.h>
#include <DataStore.h>
#include <ingest.h>

void cvt_Origin (double lat1, double lon1);
void cvt_ToXY (double lat2, double lon2, float *x2, float *y2);
}

#include "GridMap.h"

/*
 * Privately used types and functions
 */
inline float square(float a) { return (a * a); }

static void FreeBuckets (CellBucket *buckets, int size);


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


void
GridMap::FillBucketsFromSquare (CellBucket *buckets, SquareScan *ss,
				Channel ch)
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
	int hits = 0;		/* Keep track of empty buckets we hit	*/

	ss->Origin (&origin);	// the origin of our grid

	// Set origin for conversions (NOT necessarily the same as the grid)
	// Essentially, cvt_origin is the origin for the kilometer grid
	// we use to find distances between scan cells, while origin = (x0,y0)
	// is the actually lower-left corner of the image -- where the
	// image will be placed.
	cvt_Origin (gm_cvt_origin.l_lat, gm_cvt_origin.l_lon);
	cvt_ToXY (origin.l_lat, origin.l_lon, &gm_x0, &gm_y0);
	/*
	 * Traverse all of the SquareScan cells and find a place for
	 * each.  Also put the (x,y) coordinate of the cell, relative
	 * to the grid origin, into the cell bucket.
	 */
	for (iscan = 0; iscan < ss->sq_nscans; ++iscan)
	{
		scan = ss->sq_ord[iscan];
		if (Low(ch) && (scan->sc_type != A_scan))
			continue;
		for (icell = 0; icell < 128;
		     icell += (Low(ch) ? 2 : 1))
		{
			lat = scan->sc_lat[icell];
			lon = scan->sc_lon[icell];
			cvt_ToXY (lat, lon, &x, &y);
			/*
			 * Adjust ix,iy to always fit onto the grid.
			 * Any SquareScan cells which actually
			 * fall outside the grid will still be used to find
			 * values for nearby points.
			 */
			ix = (int)((x - gm_x0)/gm_info.rg_Xspacing + 0.5);
			iy = (int)((y - gm_y0)/gm_info.rg_Yspacing + 0.5);
			if (ix >= gm_info.rg_nX)
				ix = gm_info.rg_nX - 1;
			else if (ix < 0)
				ix = 0;
			if (iy >= gm_info.rg_nY)
				iy = gm_info.rg_nY - 1;
			else if (iy < 0)
				iy = 0;
			i = (iy * gm_info.rg_nX) + ix;
			if (buckets[i].cb_scan == -1)	/* empty bucket */
			{
				cb = buckets + i;
				cb->cb_next = NULL;
				hits++;
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
	gm_sqr_hits = hits;
	gm_sqr_cbs = (float)(ss->sq_nscans * 128.0) / (float)hits;
}




/*
 * Now it gets really interesting... For each empty bucket, use the
 * surrounding non-empty buckets to fill it.  Keep track of the distance
 * between the grid point and the closest cell found so far.  Abandon
 * the search once any further searching would only discover cells which
 * have to be further away.
 *
 * Use our fill radius to limit how far out we will search before leaving
 * a grid cell unmapped.
 */
void
GridMap::FillFromSurrounding (CellBucket *buckets, // The grid of buckets
			      int ix, int iy)	   // Bucket to try to fill
{
	RGrid *info = &this->gm_info;	/* Grid map stats and info */
	float metric;	/* Distance of found cell to target grid point */
	float least;	/* Least metric in a single bucket's cells */
	int cx, cy;	/* location of current search bucket */
	int radius;	/* Radius of current search */
	float mindist;	/* Minimum distance of cells found so far */
	int ci;		/* index into array of fill-from bucket */
	CellBucket *bucket;	/* The bucket we're trying to fill */
	CellBucket *cb;		/* Cell inside a bucket's list of cells */
	CellBucket *closest;
	float x,y;		/* (x,y) of target relative to grid origin */
	float min_spacing;

	bucket = buckets + (iy * info->rg_nX + ix);
	x = ix * info->rg_Xspacing + gm_x0;
	y = iy * info->rg_Yspacing + gm_y0;

	/*
	 * Start in a square one cell from target cell.  Search all cells
	 * at the perimeter of the square, inserting the closest into
	 * the target bucket.  As long as bucket is empty or radius is 
	 * less than mindist (the closest square found so far), keep searching.
	 */
	radius = 1;
	mindist = 0.0;	// metric = dx^2 + dy^2
	min_spacing = (info->rg_Xspacing < info->rg_Yspacing) ?
		(info->rg_Xspacing) : (info->rg_Yspacing);

	while (((((float)radius - 0.5) * min_spacing) <= gm_fill_radius)
	       && ((bucket->cb_scan == -1) ||
		   (square(((float)radius - 0.5) * min_spacing) < mindist)))
	{
		/*
		 * Search perimeter of square
		 */
		for (cy = (iy - radius < 0) ? 0 : iy - radius; 
		     (cy <= iy + radius) && (cy < info->rg_nY); ++cy)
		{
			for (cx = (ix - radius < 0) ? 0 : ix - radius;
			     (cx <= ix + radius) && (cx < info->rg_nX); 
			     cx += ((cy != iy - radius) && 
				    (cy != iy + radius)) ? (2*radius) : (1))
			{
				/*
				 * Check fill-from bucket is not empty
				 */
				ci = cy * info->rg_nX + cx;
				if (buckets[ci].cb_scan == -1)
					continue;
				/*
				 * Search cells in the fill-from bucket
				 */
				cb = buckets + ci;
				closest = cb;
				least = square(x - cb->cb_x) + 
					square(y - cb->cb_y);
				++gm_cells;
				while ((cb = cb->cb_next) != NULL)
				{
					++gm_cells;
					metric = square(x - cb->cb_x) + 
						square(y - cb->cb_y);
					if (metric < least)
					{
						least = metric;
						closest = cb;
					}
				}
				/*
				 * Don't take this unless it is actually
				 * closer than our fill radius.
				 */
				if (((bucket->cb_scan == -1) || 
				     (least < mindist)) &&
				    (least <= square(gm_fill_radius)))
				{
					*bucket = *closest;
					bucket->cb_next = NULL;
					mindist = least;
				}
			}
		}
		/*
		 * Extend the perimeter of our square
		 */
		++radius;

	} /* while closest cell not found yet */

	if (radius == 1)
		IngestLog (EF_DEVELOP, "problem: search loop never entered");
	--radius;
	if (radius > gm_max_radius)
		gm_max_radius = radius;
	gm_avg_radius += radius;
	if (mindist > gm_max_dist)
		gm_max_dist = mindist;
	gm_avg_dist += sqrt((double)mindist);
}




void
GridMap::FillEmptyBuckets (CellBucket *buckets)
{
	int iy, ix;
	RGrid *info;
	int i;
	int nfilled = 0;	/* Number of empty cells we fill */

	/*
	 * Traverse the buckets, skip those that aren't empty
	 */
	info = &gm_info;
	for (iy = 0; iy < info->rg_nY; ++iy)
	{
		i = iy*info->rg_nX;
		for (ix = 0; ix < info->rg_nX; ++ix, ++i)
		{
			if (buckets[i].cb_scan != -1)	// non-empty bucket
				continue;
			/*
			 * Search surrounding buckets to fill it.
			 */
			FillFromSurrounding (buckets, ix, iy);
			if (buckets[i].cb_scan != -1)	// no longer empty
				++nfilled;
		}
	}
	/*
	 * Finish stat stuff.  Sums calculated above are divided here to
	 * get averages.
	 */
	gm_avg_radius /= (float)nfilled;
	IngestLog (EF_DEVELOP, "%d empty buckets mapped to closest cell",
		   nfilled);
	IngestLog (EF_DEVELOP, "average radius of search square was %.1f",
		   gm_avg_radius);
}




void
GridMap::FillGridMap (CellBucket *buckets)
/*
 * For each bucket, put the cell in the bucket closest to the bucket's
 * grid point into the grid map.  If the bucket is empty (because a limited
 * radius search couldn't find anything nearby), leave the grid map at -1.
 */
{
	int i, ix, iy;
	int nmapped = 0;	// Number of points which actually got mapped
	CellBucket *cb, *closest;
	float metric, least;
	RGrid *info;
	float x,y;	/* Location of grid point relative to grid origin */

	info = &gm_info;
	for (iy = 0; iy < info->rg_nY; ++iy)
	{
		i = iy*info->rg_nX;
		for (ix = 0; ix < info->rg_nX; ++ix, ++i)
		{
			cb = &buckets[i];
			/*
			 * We can't very well fill from a bucket which is empty
			 */
			if (cb->cb_scan == -1)
			{
				// Leave gm_scan and gm_cell maps empty (-1)
				continue;
			}

#ifdef notdef	// skip the single-cell check to get accurate distance stats
			/*
			 * If bucket contains only one cell, just use it
			 * and go on.  Otherwise, use it as a first
			 * possibility and test it against the rest of the
			 * cells.  The hope is to avoid as much computation
			 * as possible.  We can trust that the single
			 * cell in the bucket is within the fill radius
			 * because it got there one of two ways: (1) it
			 * mapped there directly, or (2) it was the closest
			 * cell to an unmapped grid point within the
			 * fill radius.
			 */
			if (cb->cb_next == NULL)
			{
				gm_scan[i] = cb->cb_scan;
				gm_cell[i] = cb->cb_cell;
				++nmapped;
				continue;
			}
#endif
			x = ix * info->rg_Xspacing + gm_x0;
			y = iy * info->rg_Yspacing + gm_y0;
			closest = cb;
			least = square(x - cb->cb_x) + square(y - cb->cb_y);
			while ((cb = cb->cb_next) != NULL)
			{
				metric = square(x - cb->cb_x) + 
					square(y - cb->cb_y);
				if (metric < least)
				{
					least = metric;
					closest = cb;
				}
			}
			/*
			 * Only take this closest cell if its closer than
			 * the fill radius.  Otherwise, leave this grid point
			 * without a mapping.
			 */
			if (least <= square(gm_fill_radius))
			{
				gm_scan[i] = closest->cb_scan;
				gm_cell[i] = closest->cb_cell;
				gm_avg_dist += sqrt((double)least);
				++nmapped;
				if (least > gm_max_dist)
					gm_max_dist = least;
			}
		}
	}
	gm_max_dist = sqrt((double)gm_max_dist);
	gm_nmapped = nmapped;
	if (nmapped)
		gm_avg_dist /= nmapped;
}


/*
 * The steps in building the map: 
 * ------------------------------
 * Allocate space for the gm_scan and gm_cell arrays, each the size of the
 * grid, and initialize them to -1.
 *
 * Get a first approximation by filling in gm_scan and gm_cell by
 * traversing all of the cells in the SquareScan.
 *
 * Then try the other direction, traversing each grid point that was not
 * filled in.  Find the closest scan cell to each grid point by using the
 * closest, previously-filled grid point.  Bucket all scan cells which fall
 * into a grid cell so that nearby grid cells can select the closest scan
 * cell in the next step.
 *
 * Then we should be done.  Any of that make any sense?  Essentially the
 * problem is that the 'scan cell --> grid element' mapping is not 1-1 nor
 * onto.  However we assume continuity when picking the closest scan cell
 * as the value for nearby grid cells.
 */
void
GridMap::BuildMap (SquareScan *ss, RGrid *info, Channel ch)
{
	int grid_size;
	int i;
	CellBucket *buckets;

	/*
	 * Allocate scan and cell maps to hold entries for every grid
	 * coordinate, then initialize all entries to -1, meaning empty
	 */
	grid_size = info->rg_nX * info->rg_nY;
	gm_scan = (int *)malloc (grid_size * sizeof(int));
	gm_cell = (int *)malloc (grid_size * sizeof(int));
	for (i = 0; i < grid_size; ++i)
		gm_scan[i] = gm_cell[i] = -1;

	/*
	 * Finish setting up the grid map
	 */
	gm_info = *info;
	gm_channel = ch;
	InitStats ();

	/*
	 * Create a cell bucket to hold the coords of all of the cells
	 * which fall into the area of each grid element.  Each element
	 * of the grid gets one bucket.
	 */
	buckets = (CellBucket *)malloc (grid_size * sizeof(CellBucket));
	for (i = 0; i < grid_size; ++i)
	{
		buckets[i].cb_scan = buckets[i].cb_cell = -1;
		buckets[i].cb_next = NULL;
	}

	/*
	 * Fill the grid buckets with all of the SquareScan cells
	 */
	FillBucketsFromSquare (buckets, ss, ch);

	/*
	 * Now fill in all of the buckets which are still empty with the
	 * nearest cells from the surrounding non-empty buckets.
	 */
	FillEmptyBuckets (buckets);

	/*
	 * By now, every grid bucket should have at least one cell in it.
	 * Traverse all the buckets, taking the cell from each bucket
	 * which is closest to the grid point for the grid map.
	 */
	FillGridMap (buckets);
	FreeBuckets (buckets, grid_size);

	built = 1;
}



void
GridMap::InitStats (void)
{
	gm_sqr_hits = 0;	/* Number hits from SquareScan --> Grid */
	gm_sqr_mcbs = 0;	/* Max cells/bucket in SS --> Grid step */
	gm_sqr_cbs = 0;		/* Avg cells/bucket from SS --> Grid 	*/

	gm_cells = 0;		/* Total number of cells searched 	*/
	gm_max_radius = 0;	/* Max radius searched in bucket filling*/
	gm_avg_radius = 0;	/* Avg radius searched			*/
	gm_avg_dist = 0;	/* Avg metric dist bet. cell & grid pt	*/
	gm_max_dist = 0;	/* Max dist bet. cell & grid pt		*/
	gm_nmapped = 0;
}



static void
FreeBuckets (CellBucket *buckets, int size)
{
	int i;

	/*
	 * Don't free the buckets which belong to the buckets[] array
	 */
	for (i = 0; i < size; ++i)
	{
		CellBucket *next = buckets[i].cb_next;
		while (next)
		{
			CellBucket *cb = next;
			next = cb->cb_next;
			free ((char *)cb);
		}
	}
	free ((char *)buckets);
}



void
GridMap::FreeGridMap (void)
{
	if (gm_scan)
		free ((char *)gm_scan);
	if (gm_cell)
		free ((char *)gm_cell);
	gm_scan = NULL;
	gm_cell = NULL;
	built = 0;
}


GridMap::~GridMap (void)
{
	if (gm_scan)
		free ((char *)gm_scan);
	if (gm_cell)
		free ((char *)gm_cell);
}




GridMap::GridMap (void)
{
	gm_scan = NULL;
	gm_cell = NULL;
	built = 0;
	InitStats ();
}



void 
GridMap::GridToCell (int ix, int iy, int *scan, int *cell)
{
	*scan = gm_scan[ iy*(gm_info.rg_nX) + ix ];
	*cell = gm_cell[ iy*(gm_info.rg_nX) + ix ];
}



void
GridMap::ReportStats (SquareScan *ss)
/*
 * Log grid map statistics to EF_DEVELOP log
 */
{
	char buf[128];
	int npts, ncells;

	npts = gm_info.rg_nX * gm_info.rg_nY;
	ncells = ss->NumScans() * 128;
	IngestLog (EF_DEVELOP, 
		   "%12s: npts=%d; ncells=%d; hits=%d; cvt@(%.1f,%.1f)",
		   "Scan>Buckets", npts, ncells, gm_sqr_hits,
		   gm_cvt_origin.l_lat, gm_cvt_origin.l_lon);
	IngestLog (EF_DEVELOP, 
		   "%14spctgrid=%5.2f; pctcells=%5.2f; avg c/b=%.1f", " ",
		   (float)gm_sqr_hits / npts * 100.0,
		   (float)gm_sqr_hits / ncells * 100.0,
		   (float)ncells / (float)gm_sqr_hits);
	sprintf (buf, "%12s: maxrad=%d; avgrad=%.1f; ","Buckets>Grid",
		 gm_max_radius, gm_avg_radius);
	sprintf (buf+strlen(buf), "pts_searched=%d, perbucket=%.1f; ",
		 gm_cells, (float)gm_cells / npts);
	IngestLog (EF_DEVELOP, "%s", buf);
	IngestLog (EF_DEVELOP,
		   "%14sfill_radius=%.1fkm; img_origin@(%.1fkm,%.1fkm)",
		   " ", gm_fill_radius, gm_x0, gm_y0);
	sprintf (buf, "%12s: avg_dist=%.1f; mx_dist=%.1f; r=%.1fkm; ",
		 "GridStats", gm_avg_dist, gm_max_dist,
		 gm_info.rg_Xspacing);
	sprintf (buf+strlen(buf), "nmap=%d; XxY=%dx%d", 
		 gm_nmapped, gm_info.rg_nX, gm_info.rg_nY);
	IngestLog (EF_DEVELOP, "%s", buf);
}



void
GridMap::FillImageGrid (unsigned char *image, SquareScan *ss, 
			Channel ch, ScaleInfo *scale, 
			unsigned char fill = '\0')
/*
 * Given a square scan, a grid map, and a channel, fill in the grid
 * with the channel values from the square scan using the grid map.
 */
{
	int ix, iy;
	int scan, cell;

	for (iy = 0; iy < gm_info.rg_nY; ++iy)
	{
		for (ix = 0; ix < gm_info.rg_nX; ++ix)
		{
			int row = (gm_info.rg_nY - 1 - iy);
			GridToCell (ix, iy, &scan, &cell);
		/*
		 * If we have no mapping at this grid point, fill in the
		 * image cell with the fill value.
		 */
			if (scan < 0 || cell < 0)
			{
				image[ row * gm_info.rg_nX + ix ] = 
					(unsigned char) fill;
			}
			else
		/*
		 * Since the grid (for some reason) is stored with Y (or row)
		 * beginning at the top and increasing downwards, the row
		 * must be flipped from the value used in the GridMap
		 */
			{
				image[ row * gm_info.rg_nX + ix ] = 
					(unsigned char)
					((ss->CellValue (scan, cell, ch) - 
					  scale->s_Offset) / scale->s_Scale);
			}
		}
	}
}


