/*
 * Simple Zebra map filter, which reads a Zebra map file from stdin,
 * traverses the polylines, and writes out a new map file, putting out
 * points only when it has traversed the specified "km_res" distance from
 * the last point written.  The first and last point of each polyline is
 * retained.
 *
 * Usage: map_decimate <km_filter>
 */

/* $Id: map_decimate.c,v 1.2 1997-02-10 20:25:58 granger Exp $ */

# include <stdio.h>
# include <stdlib.h>
# include <math.h>

# include <defs.h>
# include <map.h>

# define DEG_TO_RAD(x)	((x)*0.017453292)

/*
 * Earth circumference in km
 */
# define C_EARTH	40074.

float	MinKm;	/* Minimum travel distance between retained points */



int
main (int argc, char **argv)
{
    int		flags;
    int		npts, i, nwrite;
    float	north, south, east, west, traversed;
    float	latprev = 0.0, lonprev = 0.0;
    float	*lon = 0, *lat = 0, *outlon = 0, *outlat = 0;
    int	maxpts = 0;
    MapFile *mf, *out;
    
    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s <km_filter>\n", argv[0]);
	exit (1);
    }

    MinKm = atof (argv[1]);

    /*
     * Read from stdin, filtering the input line segments, and write stdout.
     */
    if (! (mf = MapRead ("-")) || ! (out = MapWrite ("-", MapGetFormat(mf))))

    {
	    fprintf (stderr, "failed to open standard input and output!\n");
	    exit (1);
    }
    while (MapReadHeader (mf, &npts, &south, &west, &north, &east, &flags))
    {
	int	ok;

	npts /= 2;
	/*
	 * Make sure we have enough point space
	 */
	if (npts > maxpts)
	{
	    maxpts = 1024 * ((npts / 1024) + 1);
	    lon = (float*) realloc (lon, maxpts * sizeof (float));
	    lat = (float*) realloc (lat, maxpts * sizeof (float));
	    outlon = (float*) realloc (outlon, maxpts * sizeof (float));
	    outlat = (float*) realloc (outlat, maxpts * sizeof (float));
	}

	/*
	 * Read all the points
	 */
	for (i = 0; i < npts; i++)
	    MapReadPoint (mf, lat + i, lon + i);

	/*
	 * Filter the points and write them out again.  We automatically
	 * keep the first and last points, and traverse the points between,
	 * putting out a point only when we've traversed at least "km_res"
	 * kilometers from the last point written.
	 */
	nwrite = 0;
	for (i = 0; i < npts; i++)
	{
	    double	dlat, dlon, dx, dy;

	    /*
	     * Add to our traversal distance, and add the point to the write
	     * list if we've gone far enough
	     */
	    dlat = fabs (lat[i] - latprev);
	    dlon = fabs (lon[i] - lonprev);
	    dx = (dlon/360.0 * C_EARTH) * cos (latprev);
	    dy = (dlat/360.0 * C_EARTH);
	    traversed += hypot (dx, dy);

	    /*
	     * Write this point if the traversal distance is big enough or
	     * if it's the first or last point in the original polyline
	     */
	    if (traversed > MinKm || i == 0 || i == (npts - 1))
	    {
		latprev = outlat[nwrite] = lat[i];
		lonprev = outlon[nwrite] = lon[i];
		nwrite++;
		traversed = 0.0;
		continue;
	    }
	}

	/*
	 * Write out the truncated polyline
	 */
	MapWriteHeader (out, nwrite*2, &south, &west, &north, &east, flags);
	for (i = 0; i < nwrite; i++)
	{
	    MapWritePoint (out, outlat+i, outlon+i);
	}
    }
    MapClose (mf);
    MapClose (out);
}
