/*
 * USGS 3 arc-second Digital Elevation Model -> Zebra netCDF converter
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
# include <sys/types.h>
# include <sys/stat.h>
# include <stdlib.h>	/* for atof */
# include <stdio.h>
# include <errno.h>
# include <netcdf.h>

/* $Id: dem2zebra.c,v 1.2 1998-06-02 15:31:44 burghart Exp $ */

struct _Map
{
    int	nc_id;
    int	alt_var;
    /* all of the following are in arc-seconds */
    int	lat_spacing, lon_spacing;
    int north, south, east, west;
} Map;


void Usage (char *ourname);
void InitMap (char *fname);
void CreateMapFile (char *fname);
void OpenMapFile (char *fname);
void ProcessFiles (char *fnames[], int nfiles);
void ReadHeader (FILE *infile, int *lat_spacing, int *lon_spacing,
		    int *ssec, int *nsec, int *wsec, int *esec);
short* ReadColumn (FILE *infile, int firstneeded, int lastneeded, int step);




main (int argc, char *argv[])
{
    int	nfiles;
    char	**files, *outfname = argv[1];
/*
 * Arg check
 */
    if (argc < 7)
    {
	Usage (argv[0]);
	exit (1);
    }
/*
 * Get the desired map bounds
 */
    if (!(Map.south = (int)(3600 * atof (argv[2]))) || 
	!(Map.west = (int)(3600 * atof (argv[3]))) ||
	!(Map.north = (int)(3600 * atof (argv[4]))) || 
	!(Map.east = (int)(3600 * atof (argv[5]))) ||
	(Map.north < Map.south) || (Map.east < Map.west))
    {
	fprintf (stderr, "Weird limits: S %.4f, W %.4f, N %.4f, E %.4f\n",
		 Map.south, Map.west, Map.north, Map.east);
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
Usage (char *ourname)
{
    fprintf (stderr, "Usage: %s [options] <out_file> [<DEM_file> ...]\n", 
	     ourname);
    fprintf (stderr, 
"Options: \n"
"  -d <DEM_directory>	directory containing source DEM files; files there \n"
"			should have names of the form: <lon>W<lat>N \n"
"  -b <S> <W> <N> <E>	lon/lat bounds (required if <out_file> is new, \n"
"			ignored otherwise)\n"
"  -s <spacing>		desired output spacing in arc-seconds, must be a \n"
"			multiple of 3; default varies to give reasonable \n"
"			final map size \n");
}




void
InitMap (char *fname)
/*
 * Open up and initialize our output map file
 */
{
    char	*fullname = malloc (strlen (fname) + 4);
    struct stat fstatus;
/*
 * Build the name of the netCDF output file, appending ".cdf" if necessary
 */
    strcpy (fullname, fname);
    if (! strstr (fullname, ".nc") && ! strstr (fullname, ".cdf"))
	strcat (fullname, ".cdf");
/*
 * Either open the file if it exists, or create it
 */
    if (stat (fullname, &fstatus) < 0)
	CreateMapFile (fullname);
    else
	OpenMapFile (fullname);

    free (fullname);
}



void
CreateMapFile (char *fname)
{
    int		ncid, i, time_dim, lat_dim, lon_dim, lat_var, lon_var;
    int		base_var, offset_var, dims[3];
    long	start, count, nlats, nlons;
    long	base;
    double	step, offset;
    char	attrval[128];
/*
 * Create the file
 */    
    if ((ncid = nccreate (fname, NC_NOCLOBBER)) < 0)
    {
	fprintf (stderr, "Error creating map file %s\n", fullname);
	exit (1);
    }
    
    Map.nc_id = ncid;
/*
 * To match the DEM maps, we start with 3 arc-second longitude spacing if
 * our north edge is at or south of 50 degrees, 6 arc-second if the north
 * edge is between 50 and 70 degrees, and 9 arc-second north of 70 degrees.
 * We always start with 3 arc-second spacing in latitude. 
 */
    Map.lon_spacing = (Map.north <= (50 * 3600)) ? 3 : 
	(Map.north <= (70 * 3600)) ? 6 : 9;
    Map.lat_spacing = 3;

    nlons = (int)((Map.east - Map.west) / Map.lon_spacing + 0.5) + 1;
    nlats = (int)((Map.north - Map.south) / Map.lat_spacing + 0.5) + 1;
/*
 * Adjust spacing if necessary to get us below a 750x750 final map
 */
    if (nlons > 750)
    {
	Map.lon_spacing *= (1 + nlons / 750);
	nlons = (int)((Map.east - Map.west) / Map.lon_spacing + 0.5) + 1;
    }
    
    if (nlats > 750)
    {
	Map.lat_spacing *= (1 + nlats / 750);
	nlats = (int)((Map.north - Map.south) / Map.lat_spacing + 0.5) + 1;
    }

    printf ("\n");
    printf ("Using lon/lat spacing of %d/%d arc-seconds\n", Map.lon_spacing,
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
 * and our variables
 */
    lat_var = ncvardef (ncid, "latitude", NC_FLOAT, 1, &lat_dim);
    strcpy (attrval, "north latitude");
    ncattput (ncid, lat_var, "long_name", NC_CHAR, strlen (attrval) + 1, 
	      attrval);
    strcpy (attrval, "degrees");
    ncattput (ncid, lat_var, "units", NC_CHAR, strlen (attrval) + 1, attrval);
    
    lon_var = ncvardef (ncid, "longitude", NC_FLOAT, 1, &lon_dim);
    strcpy (attrval, "east longitude");
    ncattput (ncid, lon_var, "long_name", NC_CHAR, strlen (attrval) + 1, 
	      attrval);
    strcpy (attrval, "degrees");
    ncattput (ncid, lon_var, "units", NC_CHAR, strlen (attrval) + 1, attrval);

    dims[0] = time_dim;
    dims[1] = lon_dim;
    dims[2] = lat_dim;
    Map.alt_var = ncvardef (ncid, "altitude", NC_SHORT, 3, dims);
    strcpy (attrval, "altitude");
    ncattput (ncid, Map.alt_var, "long_name", NC_CHAR, strlen (attrval) + 1, 
	      attrval);
    strcpy (attrval, "meters");
    ncattput (ncid, Map.alt_var, "units", NC_CHAR, strlen (attrval) + 1, 
	      attrval);

    base_var = ncvardef (ncid, "base_time", NC_LONG, 0, 0);
    strcpy (attrval, "Base time in Epoch");
    ncattput (ncid, base_var, "long_name", NC_CHAR, strlen (attrval) + 1, 
	      attrval);
    strcpy (attrval, "seconds since 1970-1-1 0:00:00 0:00");
    ncattput (ncid, base_var, "units", NC_CHAR, strlen (attrval) + 1, 
	      attrval);

    offset_var = ncvardef (ncid, "time_offset", NC_DOUBLE, 1, &time_dim);
    strcpy (attrval, "Time offset from base_time");
    ncattput (ncid, offset_var, "long_name", NC_CHAR, strlen (attrval) + 1, 
	      attrval);
    strcpy (attrval, "seconds since 1970-1-1 0:00:00 0:00");
    ncattput (ncid, offset_var, "units", NC_CHAR, strlen (attrval) + 1, 
	      attrval);
/*
 * Get out of definition mode and write our times (we just use zero...)
 */
    ncendef (ncid);

    base = 0;
    offset = 0.0;
    start = 0;
    count = 1;
    
    ncvarput (ncid, base_var, &start, &count, &base);
    ncvarput (ncid, offset_var, &start, &count, &offset);
/*
 * Write our lats & lons
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
    long	start[3], count[3];
    short	*alts;
    FILE	*infile = 0;
/*
 * Loop through the input files
 */
    for (f = 0; f < nfiles; f++)
    {
	if (infile)
	    fclose (infile);

	printf ("%s\n", fnames[f]);

	if (! (infile = fopen (fnames[f], "r")))
	{
	    fprintf (stderr, "Error %d opening '%s'\n", errno, fnames[f]);
	    continue;
	}

	ReadHeader (infile, &lat_spacing, &lon_spacing, &ssec, &nsec, &wsec, 
		    &esec);

	if ((Map.lat_spacing % lat_spacing) || (Map.lon_spacing % lon_spacing))
	{
	    fprintf (stderr, 
		     "lat_spacing or lon_spacing mismatch with file %s!\n",
		     fnames[f]);
	    exit (1);
	}
    /*
     * Start/stop lats & lons, forced to multiples of the output file's
     * lat and lon spacings
     */
	lat_start = (ssec < Map.south) ? Map.south : ssec;
	lat_start = (lat_start / Map.lat_spacing) * Map.lat_spacing;
	
	lat_stop = (nsec > Map.north) ? Map.north : nsec;
	lat_stop = (lat_stop / Map.lat_spacing) * Map.lat_spacing;

	if (lat_start > lat_stop)
	    continue;

	lon_start = (wsec < Map.west) ? Map.west : wsec;
	lon_start = (lon_start / Map.lon_spacing) * Map.lon_spacing;
	
	lon_stop = (esec > Map.east) ? Map.east : esec;
	lon_stop = (lon_stop / Map.lon_spacing) * Map.lon_spacing;

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
	    start[1] = (lon - Map.west) / Map.lon_spacing;
	    start[2] = (lat_start - Map.south) / Map.lat_spacing;

	    count[0]= 1;
	    count[1] = 1;
	    count[2] = (lastneeded - firstneeded) / step + 1;
	/*
	 * Write the part of the column that goes into the output file
	 */
	    ncvarput (Map.nc_id, Map.alt_var, start, count, alts);
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
 * all in integer arc-seconds.
 */
{
    char	buf[1024];
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
    
    sscanf (buf + 816, "%12lf%12lf", &dlon, &dlat);
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
 * We assume 1201 lats per column (1 degree column, 3 arc-second spacing)
 */
    nlats = atoi (buf + 12);

    if (nlats != 1201)
    {
	fprintf (stderr, "Eek! Column does not have 1201 entries!\n");
	exit (1);
    }
/*
 * Read out the altitudes at the needed lats
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
