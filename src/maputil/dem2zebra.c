/*
 * USGS 1 degree Digital Elevation Model -> Zebra netCDF converter
 *
 *		Copyright (C) 1996 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide
 * maintenance or updates for its software.
 */
# include <stdlib.h>	/* for atof */
# include <math.h>
# include <stdio.h>
# include <errno.h>
# include <sys/stat.h>
# include <unistd.h>

# include <netcdf.h>

/* $Id: dem2zebra.c,v 1.10 2004-01-12 23:26:30 burghart Exp $ */

struct _Map
{
    int	nc_id;
    int	elev_var;
    /* all the following are in arc seconds */
    int	lat_spacing, lon_spacing;
    int north, south, east, west;
} Map;

/* default size of generated maps will be less than BIGDIM x BIGDIM */
const int BIGDIM = 2000;


void	InitMap (char *fname);
void	CreateMapFile (char *fname);
void	OpenMapFile (char *fname);
void	ProcessFiles (char *fnames[], int nfiles);
void	ReadHeader (FILE *infile, int *lat_spacing, int *lon_spacing,
		    int *ssec, int *nsec, int *wsec, int *esec);
short*	ReadColumn (FILE *infile, int firstneeded, int lastneeded, int step);




main (int argc, char *argv[])
{
    int	nfiles;
    char	**files, *outfname = argv[1];

    /*
     * Arg check
     */
    if (argc < 7)
    {
	fprintf (stderr, 
		 "Usage: %s outfile <S> <W> <N> <E> DEM_file [DEM_file ...]\n",
		 argv[0]);
	exit (1);
    }

    /*
     * Get the bounds for the file we're generating
     */
    if (!(Map.south = (int)(3600 * atof (argv[2]))) || 
	!(Map.west = (int)(3600 * atof (argv[3]))) ||
	!(Map.north = (int)(3600 * atof (argv[4]))) || 
	!(Map.east = (int)(3600 * atof (argv[5]))) ||
	(Map.north < Map.south) || (Map.east < Map.west))
    {
	fprintf (stderr, "Weird limits: S %.4f, W %.4f, N %.4f, E %.4f\n",
		 Map.south / 3600.0, Map.west / 3600.0, Map.north / 3600.0, 
		 Map.east / 3600.0);
	fprintf (stderr, 
		 "(Remember that longitudes in the US are negative!)\n");
	exit (1);
    }

    /*
     * Initialize the output map file
     */
    InitMap (outfname);

    /*
     * Extract from the input files
     */
    files = argv + 6;
    nfiles = argc - 6;
    ProcessFiles (files, nfiles);
    ncclose (Map.nc_id);
}


void
InitMap (char *fname)
/*
 * Open up and initialize our output map file
 */
{
    struct stat	sbuf;
    int		fnlen = strlen (fname), i;
    char	*fullname = malloc (fnlen + 5);
/*
 * Create the filename, appending ".nc" to the name if necessary
 */
    strcpy (fullname, fname);
    if (strcmp (fname + fnlen - 4, ".cdf") && 
	strcmp (fname + fnlen - 3, ".nc"))
	strcat (fullname, ".nc");
/*
 * Open the file if it already exists, otherwise create it.
 */
    if (stat (fullname, &sbuf) < 0 && errno == ENOENT)
	CreateMapFile (fullname);
    else
	OpenMapFile (fullname);

    free (fullname);
}



void
CreateMapFile (char *fname)
{
    int		ncid, i, time_dim, lat_dim, lon_dim, lat_var, lon_var, alt_var;
    int		base_var, time_var, dims[3];
    long	start, count, nlats, nlons;
    long	base;
    double	step, time;
    char	attrval[128];

    if ((ncid = nccreate (fname, NC_NOCLOBBER)) < 0)
    {
	fprintf (stderr, "Error creating map file %s\n", fname);
	exit (1);
    }
    
    Map.nc_id = ncid;

    /*
     * To match the DEM maps, we use 3 arc second longitude spacing if our
     * north edge is at or south of 50 degrees, 6 arc second if the north
     * edge is between 50 and 70 degrees, and 9 arc second north of 70
     * degrees.  We always use 3 second spacing in latitude. 
     */
    Map.lon_spacing = (Map.north <= (50 * 3600)) ? 3 : 
	              (Map.north <= (70 * 3600)) ? 6 : 9;
    Map.lat_spacing = 3;

    nlons = (int)((Map.east - Map.west) / Map.lon_spacing + 0.5) + 1;
    nlats = (int)((Map.north - Map.south) / Map.lat_spacing + 0.5) + 1;

    /*
     * Adjust spacing if necessary to get us below a BIGDIM x BIGDIM final map
     */
    if (nlons > BIGDIM)
    {
	Map.lon_spacing *= (1 + nlons / BIGDIM);
	nlons = (int)((Map.east - Map.west) / Map.lon_spacing + 0.5) + 1;
    }
    
    if (nlats > BIGDIM)
    {
	Map.lat_spacing *= (1 + nlats / BIGDIM);
	nlats = (int)((Map.north - Map.south) / Map.lat_spacing + 0.5) + 1;
    }

    printf ("\n");
    printf ("Using lon/lat spacing of %d/%d arc seconds\n", Map.lon_spacing,
	    Map.lat_spacing);
    printf ("for a resulting map size of %dx%d\n\n", nlons, nlats);

    /*
     * Adjust our north and east edges to exactly match the spacing we just 
     * determined
     */
    Map.north = Map.south + (nlats - 1) * Map.lat_spacing;
    Map.east = Map.west + (nlons - 1) * Map.lon_spacing;

    /*
     * Create our dimensions
     */
    time_dim = ncdimdef (ncid, "time", 1);
    lat_dim = ncdimdef (ncid, "latitude", nlats);
    lon_dim = ncdimdef (ncid, "longitude", nlons);
    /*
     * Latitude, longitude, and (nominal) altitude vars.  We set the
     * nominal altitude to zero, and put the 2-d grid of heights in the
     * "elevation" variable below
     */
    lat_var = ncvardef (ncid, "latitude", NC_FLOAT, 1, &lat_dim);
    strcpy (attrval, "degrees_north");
    ncattput (ncid, lat_var, "units", NC_CHAR, strlen (attrval) + 1, attrval);
    
    lon_var = ncvardef (ncid, "longitude", NC_FLOAT, 1, &lon_dim);
    strcpy (attrval, "degrees_east");
    ncattput (ncid, lon_var, "units", NC_CHAR, strlen (attrval) + 1, attrval);

    alt_var = ncvardef (ncid, "altitude", NC_FLOAT, 0, 0);
    strcpy (attrval, "altitude");
    ncattput (ncid, alt_var, "long_name", NC_CHAR, strlen (attrval) + 1, 
	      attrval);
    strcpy (attrval, "km");
    ncattput (ncid, alt_var, "units", NC_CHAR, strlen (attrval) + 1, attrval);

   /*
    * the elevation variable
    */
    dims[0] = time_dim;
    dims[1] = lat_dim;
    dims[2] = lon_dim;
    Map.elev_var = ncvardef (ncid, "elevation", NC_SHORT, 3, dims);
    strcpy (attrval, "elevation MSL");
    ncattput (ncid, Map.elev_var, "long_name", NC_CHAR, strlen (attrval) + 1,
	      attrval);
    strcpy (attrval, "meters");
    ncattput (ncid, Map.elev_var, "units", NC_CHAR, strlen (attrval) + 1, 
	      attrval);

    time_var = ncvardef (ncid, "time", NC_DOUBLE, 1, &time_dim);
    strcpy (attrval, "seconds since 1970-1-1 0:00:00 0:00");
    ncattput (ncid, time_var, "units", NC_CHAR, strlen (attrval) + 1, 
	      attrval);
    /*
     * Get out of definition mode and write our times (we just use zero...)
     */
    ncendef (ncid);

    time = 0.0;
    start = 0;
    count = 1;
    
    ncvarput (ncid, time_var, &start, &count, &time);
    /*
     * Write our lats & lons, and the nominal altitude (0)
     */
    for (i = 0; i < nlats; i++)
    {
	float	lat = (float)(Map.south + i * Map.lat_spacing) / 3600.0;

	start = i;
	count = 1;
	ncvarput (ncid, lat_var, &start, &count, &lat);
    }

    for (i = 0; i < nlons; i++)
    {
	float	lon = (float)(Map.west + i * Map.lon_spacing) / 3600.0;

	start = i;
	count = 1;
	ncvarput (ncid, lon_var, &start, &count, &lon);
    }
    
    {
	float alt = 0.0;
	ncvarput1 (ncid, alt_var, 0, &alt);
    }
    /*
     * Done here
     */
    return;
}



void
OpenMapFile (char *fname)
{
    int		ncid, i, lat_dim, lon_dim, lat_var, lon_var;
    nc_type	nctype;
    long	start, count, nlats, nlons;
    long	base;
    float	val;
    char	attrval[128];

    if ((ncid = ncopen (fname, NC_WRITE)) < 0)
    {
	fprintf (stderr, "Error opening map file %s\n", fname);
	exit (1);
    }
    
    Map.nc_id = ncid;
/*
 * Get the expected dimensions
 */
    
    if ((lat_dim = ncdimid (ncid, "latitude")) < 0)
    {
	fprintf (stderr, "Map file has no latitude dimension!\n");
	exit (1);
    }
    ncdiminq (ncid, lat_dim, 0, &nlats);


    if ((lon_dim = ncdimid (ncid, "longitude")) < 0)
    {
	fprintf (stderr, "Map file has no longitude dimension!\n");
	exit (1);
    }
    ncdiminq (ncid, lon_dim, 0, &nlons);
/*
 * Get the lat variable and range
 */
    if ((lat_var = ncvarid (ncid, "latitude")) < 0)
    {
	fprintf (stderr, "Map file has no latitude variable!\n");
	exit (1);
    }

    start = 0;
    count = 1;
    ncvarget (ncid, lat_var, &start, &count, &val);
    Map.south = (int)(rint (val * 3600));	// to seconds

    start = nlats - 1;
    count = 1;
    ncvarget (ncid, lat_var, &start, &count, &val);
    Map.north = (int)(rint (val * 3600));	// to seconds

    Map.lat_spacing = (Map.north - Map.south) / (nlats - 1);
    if ((Map.south + Map.lat_spacing * (nlats - 1)) != Map.north)
    {
	fprintf (stderr, "Unexpected latitude spacing in existing file...\n");
	exit (1);
    }
/*
 * Get the lon variable and range
 */
    if ((lon_var = ncvarid (ncid, "longitude")) < 0)
    {
	fprintf (stderr, "Map file has no longitude variable!\n");
	exit (1);
    }

    start = 0;
    count = 1;
    ncvarget (ncid, lon_var, &start, &count, &val);
    Map.west = (int)(rint (val * 3600));	// to seconds

    start = nlons - 1;
    count = 1;
    ncvarget (ncid, lon_var, &start, &count, &val);
    Map.east = (int)(rint (val * 3600));	// to seconds

    Map.lon_spacing = (Map.east - Map.west) / (nlons - 1);
    if ((Map.west + Map.lon_spacing * (nlons - 1)) != Map.east)
    {
	fprintf (stderr, "Unexpected longitude spacing in existing file...\n");
	exit (1);
    }
/*
 * Get the alt variable
 */
    if ((Map.elev_var = ncvarid (ncid, "elevation")) < 0)
    {
	fprintf (stderr, "Map file has no altitude variable!\n");
	exit (1);
    }
/*
 * Info
 */
    printf ("\n");
    printf ("Map has lon/lat spacing of %d/%d arc seconds\n", Map.lon_spacing,
	    Map.lat_spacing);
    printf ("and a size of %dx%d\n\n", nlons, nlats);
/*
 * Done here
 */
    return;
}



void
ProcessFiles (char *fnames[], int nfiles)
{
    int	f, lat_spacing, lon_spacing, ssec, nsec, wsec, esec;
    int	lat_start, lat_stop, lat, lon_start, lon_stop, lon;
    long start[3], count[3];
    char *uncompressCommand = 0;
    short *alts;
    FILE *infile = 0;
/*
 * Loop through the input files
 */
    for (f = 0; f < nfiles; f++)
    {
	if (infile)
	{
	    if (uncompressCommand)
		pclose (infile);
	    else
		fclose (infile);
	}

	printf ("%s\n", fnames[f]);
    /*
     * Simple filetype determination based on extension
     */
	if (! strcmp (fnames[f] + strlen (fnames[f]) - 4, ".bz2"))
	    uncompressCommand = "bzcat";
	else if (! strcmp (fnames[f] + strlen (fnames[f]) - 3, ".gz"))
	    uncompressCommand = "zcat";
	else
	    uncompressCommand = 0;
    /*
     * Open the file directly if it's uncompressed, otherwise
     * pipe it in via an uncompress command
     */
	if (uncompressCommand)
	{
	    char command[128];
	    sprintf (command, "%s \"%s\" 2>/dev/null ", uncompressCommand, 
		     fnames[f]);
	    if (! (infile = popen (command, "r")))
	    {
		perror (command);
		exit (1);
	    }
	}
	else
	{
	    if (! (infile = fopen (fnames[f], "r")))
	    {
		fprintf (stderr, "Error %d opening '%s'\n", errno, fnames[f]);
		exit (1);
	    }
	}

	ReadHeader (infile, &lat_spacing, &lon_spacing, &ssec, &nsec, &wsec, 
		    &esec);

	if ((Map.lat_spacing % lat_spacing) || (Map.lon_spacing % lon_spacing))
	{
	    fprintf (stderr, 
		     "lat_spacing or lon_spacing mismatch with file %s!\n",
		     fnames[f]);
	    continue;
	}

	/*
	 * Start/stop lats & lons, forced to multiples of the output file's
	 * lat and lon spacings.  
	 */
	lat_start = (ssec < Map.south) ? Map.south : ssec;
	lat_start = (lat_start / Map.lat_spacing) * Map.lat_spacing;
	if (lat_start < ssec)
	    lat_start += Map.lat_spacing;
	
	lat_stop = (nsec > Map.north) ? Map.north : nsec;
	lat_stop = (lat_stop / Map.lat_spacing) * Map.lat_spacing;
	if (lat_stop > nsec)
	    lat_stop -= Map.lat_spacing;

	if (lat_start > lat_stop)
	    continue;

	lon_start = (wsec < Map.west) ? Map.west : wsec;
	lon_start = (lon_start / Map.lon_spacing) * Map.lon_spacing;
	if (lon_start < wsec)
	    lon_start += Map.lon_spacing;
	
	lon_stop = (esec > Map.east) ? Map.east : esec;
	lon_stop = (lon_stop / Map.lon_spacing) * Map.lon_spacing;
	if (lon_stop > esec)
	    lon_stop -= Map.lon_spacing;

	if (lon_start > lon_stop)
	    continue;

	/*
	 * Loop through the columns in the file
	 */
	for (lon = wsec; lon <= lon_stop; lon += lon_spacing)
	{
	    int	firstneeded, lastneeded, step;

	    printf ("	longitude %.4f\r", (float)(lon) / 3600.0);
	    fflush (stdout);
	    
	    if (lon < lon_start || (lon % Map.lon_spacing))
	    {
		ReadColumn (infile, 0, -1, 0);
		continue;
	    }
	    /*
	     * Bounding indices of the lats we need from the next column
	     */
	    firstneeded = (lat_start - ssec) / lat_spacing;
	    lastneeded = (lat_stop - ssec) / lat_spacing;
	    step = Map.lat_spacing / lat_spacing;
	    
	    alts = ReadColumn (infile, firstneeded, lastneeded, step);
	    /*
	     * Position and size for the portion of the output file we cover
	     * with this column
	     */
	    start[0] = 0;
	    start[1] = (lat_start - Map.south) / Map.lat_spacing;
	    start[2] = (lon - Map.west) / Map.lon_spacing;

	    count[0]= 1;
	    count[1] = (lastneeded - firstneeded) / step + 1;
	    count[2] = 1;
	    /*
	     * Write the part of the column that goes into the output file
	     */
	    ncvarput (Map.nc_id, Map.elev_var, start, count, alts);
	}
	printf ("\n");
    }
}


void
ReadHeader (FILE *infile, int *lat_spacing, int *lon_spacing, int *ssec,
	    int *nsec, int *wsec, int *esec)
/*
 * Read the "DEM Type A Logical Record" header from the file, returning the
 * latitude spacing, longitude spacing, south, north, west, and east edges,
 * all in integer arc seconds.
 */
{
    char	buf[1024], foo[13];
    double	s, n, w, e, dlon, dlat;

    fread (buf, 1, sizeof (buf), infile);

/*
 * Grr.  We have to change from D to E notation.
 */
    buf[566] = 'E';	buf[590] = 'E';
    buf[662] = 'E';	buf[686] = 'E';

    sscanf (buf + 546, "%24lg%24lg", &w, &s);
    sscanf (buf + 642, "%24lg%24lg", &e, &n);
    *ssec = s;
    *nsec = n;
    *wsec = w;
    *esec = e;
/*
 * More Grr.  GNU changed the scanf parsing rules somewhere along the
 * way, so in order to maintain portability we now have to split out 
 * the dlat and dlon values that are jammed together.
 */
    foo[12] = '\0';
    strncpy (foo, buf + 816, 12);
    sscanf (foo, "%lf", &dlon);
    strncpy (foo, buf + 828, 12);
    sscanf (foo, "%lf", &dlat);

    *lat_spacing = dlat;
    *lon_spacing = dlon;
}


short*
ReadColumn (FILE *infile, int firstneeded, int lastneeded, int step)
{
    char	buf[8192];
    int		nlats, i, n;
    static short	alts[1201];

    fread (buf, 1, sizeof (buf), infile);
/*
 * We assume 1201 lats per column (1 degree column, 3 arc second spacing)
 */
    nlats = atoi (buf + 12);
    if (nlats != 1201)
    {
	fprintf (stderr, "Eek! Column does not have 1201 entries!\n");
	exit (1);
    }
/*
 * Read out the needed alts
 */
    for (i = firstneeded, n = 0; i <= lastneeded; i += step, n++)
    {
	int	offset;
    /*
     * Offset to the desired lat.  144 byte heading, 6 bytes per lat, plus
     * four spaces of filler at the end of each 1024 byte segment.
     */
	offset = 144 + 6 * i;
	offset += (offset / 1024) * 4;
	alts[n] = atoi (buf + offset);
    }

    return alts;
}

    
