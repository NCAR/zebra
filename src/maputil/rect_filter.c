/*
 * Simple Zebra map filter, which reads a Zebra map file from stdin, spitting 
 * out only those map segments which intersect a chosen lat/lon box.
 *
 * Usage: mapfilter <lonWest> <latSouth> <lonEast> <latNorth>
 */

/* $Id: rect_filter.c,v 1.1 1996-12-30 17:10:22 burghart Exp $ */

# include <stdio.h>
# include <stdlib.h>

# define TRUE	1
# define FALSE	0

typedef struct _Box
{
	float	latSouth;
	float	latNorth;
	float	lonWest;
	float	lonEast;
} Box;


/*
 * Prototypes
 */
static int	CheckIntersection (const Box *box0, const Box *box1);
static float	OverlapLen (const float a0, const float a1, const float b0, 
			    const float b1);




main (int argc, char **argv)
{
	char	line[128];
	Box	segbox, clipbox;
	int	npts, nlines, l;

	if (argc != 5)
	{
		fprintf (stderr, 
		      "Usage: %s <lonWest> <latSouth> <lonEast> <latNorth>\n",
		      argv[0]);
		exit (1);
	}
/*
 * Coordinates of our map clipping box
 */
	clipbox.lonWest = atof (argv[1]);
	clipbox.latSouth = atof (argv[2]);
	clipbox.lonEast = atof (argv[3]);
	clipbox.latNorth = atof (argv[4]);
/*
 * Read from stdin, spitting back out those segments whose bounding box 
 * intersects our clip box.
 */
	while (gets (line))
	{
		int	ok;
	/*
	 * Get the number of points and the bounding box for this polyline
	 */
		sscanf (line, "%d%f%f%f%f", &npts, &segbox.latNorth, 
			&segbox.latSouth, &segbox.lonEast, &segbox.lonWest);

		nlines = (npts + 7) / 8;
	/*
	 * We want left and right longitudes, rather than minlon
	 * and maxlon (they're opposite if the box spans 180 longitude).  
	 * Fix the box assuming that no segment will be wider than 180
	 * degrees in longitude.
	 */
		if ((segbox.lonEast - segbox.lonWest) > 180.0)
		{
			float	temp = segbox.lonEast;
			segbox.lonEast = segbox.lonWest;
			segbox.lonWest = temp;
		}
	/*
	 * Test against our clip box
	 */
		ok = CheckIntersection (&segbox, &clipbox);

		if (ok)
			printf ("%s\n", line);

		for (l = 0; l < nlines; l++)
		{
			gets (line);
			if (ok)
				printf ("%s\n", line);
		}
	}
}



static int
CheckIntersection (const Box *box0, const Box *box1)
/*
 * Return TRUE if box0 intersects box1, FALSE otherwise.
 */
{
	float	a0, b0, a1, b1;
	int	b;
	int	latlap, lonlap;

/*
 * First check for overlap in latitude.
 */
	a0 = box0->latSouth;
	a1 = box0->latNorth;
	b0 = box1->latSouth;
	b1 = box1->latNorth;
	if (OverlapLen (a0, a1, b0, b1) <= 0)
		return (FALSE);
/*
 * We have latitude overlap, so test for longitude overlap.  Be extra careful
 * with boxes that cross 180 longitude...
 */
	a0 = box0->lonWest;
	a1 = box0->lonEast;
	if ((a0 > 0.0) && (a1 < 0.0))
		a1 += 360.0;

	b0 = box1->lonWest;
	b1 = box1->lonEast;
	if ((b0 > 0.0) && (b1 < 0.0))
	{
		if (a0 < 0.0)
			b0 -= 360.0;
		else
			b1 += 360.0;
	}
	

	if (OverlapLen (a0, a1, b0, b1) <= 0)
		return (FALSE);

	return (TRUE);
}





static float
OverlapLen (const float a0, const float a1, const float b0, const float b1)
/*
 * Find the amount of overlap between two line segments (a0,a1) and (b0,b1) in
 * one-dimensional space.  The endpoints a0 and b0 must be *less than* a1 
 * and b1 respectively.
 */
{
	float	ostart, oend;

	ostart = (a0 < b0) ? b0 : a0;
	oend = (a1 < b1) ? a1 : b1;
	return (oend - ostart);
}
