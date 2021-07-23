/*
 * Convert a map file to an xdr-encoded map file.
 */
#include <stdlib.h>
#include <stdio.h>
# include <math.h>
# include <getopt.h>

#include <defs.h>
#include <message.h>
#include <Test.h>
#include <map.h>

# define DEG_TO_RAD(x)	((x)*0.017453292)

/*
 * Earth circumference in km
 */
# define C_EARTH	40074.

static int ReadFile (const char *path);
static int TimeTest (const char *file);
static int Convert (MapFile *in, MapFile *out);
static int Reduce (MapFile *mf, MapFile *out, double minkm);



static void
usage (const char *prog)
{

	fprintf (stderr, 
		 "Usage: %s %s [mapfile ...]\n", prog,
		 "[-t] [-a|-x] [-r <km>] [-o <mapfile>]");
	fprintf (stderr, "   -h   Print this usage information.\n");
	fprintf (stderr, "   -t   Time each mapfile read, except '-'.\n");
	fprintf (stderr, "   -d   Print debug messages (polylines).\n");
	fprintf (stderr, "   -v   Print development messages (all points).\n");
	fprintf (stderr, "   -r   Reduce output to <km> resolution.\n");
	fprintf (stderr, "   -a   Convert mapfiles to ASCII format.\n");
	fprintf (stderr, "   -x   Convert mapfiles to XDR binary format.\n");
	fprintf (stderr, "   -o   Specify the name of the output file.\n");
	fprintf (stderr, 
		 "If no map files are specified, the default input is stdin.\n"
		 "Likewise stdout is the default output if no -o option.\n"
		 "The default output is XDR.\n"
		 "If only -t is given, no output is written.\n"
		 "A filename of '-' indicates standard input or output.\n");
	fprintf (stderr, "Examples:\n"
		 "Time the reading of files:\n"
		 "   %s -t water.map\n"
		 "Convert multiple ascii maps to a single XDR map:\n"
		 "   %s -x -o complete.map fed.map state.map roads.map\n"
		 "Using stdin and stdout:\n"
		 "   cat fed.map state.map | %s > complete.map\n"
		 "Reduce map polylines to minimum 2 km between points:\n"
		 "   %s -r 2 detailed.map > smaller.map\n",
		 prog, prog, prog, prog);
}



int
main (int argc, char *argv[])
{
	char *input = "-";		/* command-line defaults */
	char *output = "-";
	MapFormat fmt = MF_UNKNOWN;
	int c;
	int profile = 0;
	int convert = 0;
	float reduce = 0.0;
	MapFile *out = NULL;
	MapFile *in = NULL;
	int mask;

	msg_connect (0, "");
	msg_ELPrintMask ((mask = msg_ELPrintMask (0)));

	while ((c = getopt(argc, argv, "htdvaxo:r:")) != EOF)
	{
		switch (c) 
		{
		case 'h':
			usage (argv[0]);
			exit (0);
			break;
		case 't':
			profile = 1;
			break;
		case 'v':
			mask |= EF_DEVELOP;
			/* fall through */
		case 'd':
			mask |= EF_DEBUG;
			msg_ELPrintMask (mask);
			break;
		case 'a':
			fmt = MF_ASCII;
			break;
		case 'x':
			fmt = MF_XDR;
			break;
		case 'o':
			output = optarg;
			break;
		case 'r':
			reduce = atof(optarg);
			if (reduce <= 0.0)
			{
				fprintf (stderr, "illegal -r argument: %s\n",
					 optarg);
				exit (1);
			}
			break;
		case '?':
			usage (argv[0]);
			exit (1);
			break;
		}
	}

	TP_Profile (profile);
	convert = (! profile || fmt != MF_UNKNOWN);
	if (fmt == MF_UNKNOWN)
		fmt = MF_XDR;
	if (convert && (! (out = MapWrite (output, fmt))))
	{
		msg_ELog (EF_PROBLEM, "Could not open output '%s'", output);
		exit (1);
	}
	do {
		if (optind < argc)
			input = argv[optind++];
		if (profile && (!convert || strcmp (input,"-")))
			TimeTest (input);
		if (convert)
		{
			if (! (in = MapRead (input)))
			{
				msg_ELog (EF_PROBLEM, "Could not read file %s",
					  input);
				exit (2);
			}
			if (reduce == 0.0)
				Convert (in, out);
			else
				Reduce (in, out, reduce);
			MapClose (in);
		}
	}
	while (optind < argc);
	     
	if (out)
		MapClose (out);
	exit (0);
	return (0);
}




static int
Convert (MapFile *in, MapFile *out)
{
	float lat, lon, lat1, lon1;
	int flags, npt;
	int err = 0;

	while (MapReadHeader (in, &npt, &lat, &lon, &lat1, &lon1, &flags))
	{
		int i;

		msg_ELog (EF_DEBUG, "%d %g %g %g %g %d",
			  npt, lat1, lat, lon1, lon, flags);
		MapWriteHeader (out, npt, &lat, &lon, &lat1, &lon1, flags);
		npt /= 2;
		for (i = 0; i < npt; ++i)
		{
			if (MapReadPoint (in, &lat, &lon))
			{
				msg_ELog (EF_DEVELOP, "%g %g", lat, lon);
				MapWritePoint (out, &lat, &lon);
			}
			else
				break;
		}
		if (i < npt)
		{
			++err;
			break;
		}
	}
	if (err)
		msg_ELog (EF_PROBLEM, "error converting map file");
	return (err);
}



static int
TimeTest (const char *file)
/*
 * Run a profile on reading this file.
 */
{
	TP_Push (file);
	ReadFile (file);
	TP_Pop ();
	return (0);
}



static int
ReadFile (const char *path)
{
	MapFile *mf;
	float lat, lon, lat1, lon1;
	int flags, npt;
	int err = 0;

	if (! (mf = MapRead (path)))
	{
		msg_ELog (EF_PROBLEM, "Could not read file %s", path);
		return (++err);
	}

	while (MapReadHeader (mf, &npt, &lat, &lon, &lat1, &lon1, &flags))
	{
		int i;

		npt /= 2;
		for (i = 0; i < npt; ++i)
		{
			if (! MapReadPoint (mf, &lat, &lon))
				break;
		}
		if (i < npt)
		{
			++err;
			break;
		}
	}
	return (err);
}




static int
Reduce (MapFile *mf, MapFile *out, double minkm)
{
    static float *lon = 0, *lat = 0, *outlon = 0, *outlat = 0;
    static int maxpts = 0;

    int		flags;
    int		npts, i, nwrite;
    float	north, south, east, west, traversed;
    float	latprev = 0.0, lonprev = 0.0;

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
	    if (traversed > minkm || i == 0 || i == (npts - 1))
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
    return (0);
}

