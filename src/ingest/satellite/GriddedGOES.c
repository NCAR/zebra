/*
 * Turn GOES area files into lat/lon grid data
 */
/*		Copyright (C) 1995 by UCAR
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

# include <unistd.h>
# include <errno.h>
# include <math.h>
# include <stdio.h>

/*
 * $Id: GriddedGOES.c,v 1.1 1995-03-10 23:21:21 burghart Exp $
 */

typedef char	bool;
# define TRUE	-1
# define FALSE	0

/*
 * The raw image and its size
 */
unsigned char	*Image;
int	Nx, Ny;

/*
 * Grid information
 */
int	GridX = 0, GridY = 0;
float	KmResolution = 0.0;
float	Minlon, Maxlon, Minlat, Maxlat;
float	Latstep, Lonstep;

bool 	HaveLimits = FALSE;
bool	CheckTimes = TRUE;

/*
 * Image unpacking info.
 */
int	Nbytes, Prefixlen, Linelen, Xres, Yres;

/*
 * Image limits in line/elem coordinates
 */
int	Minelem, Maxelem, Minline, Maxline;

/*
 * Useful stuff
 */
# define BETWEEN(x,lower,upper)	(((x)-(lower))*((x)-(upper)) <= 0)
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)

int	Mdays[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/*
 * Prototypes
 */
static void	swapfour (int *, int);
static void *	DoFile (char *);
static inline unsigned char	imageval (int, int);




main (argc, argv)
int argc;
char **argv;
/*
 * Ingest a GOES visible and/or IR image
 */
{
	char	fname[128];
	FILE	*cmdfile;
/*
 * Open our command file
 */
	if (argc != 3)
	{
		fprintf (stderr, "Usage: %s <cmdfile> <areafile>\n", argv[0]);
		exit (1);
	}


	if (! (cmdfile = fopen (argv[1], "r")))
	{
		fprintf (stderr, "Error %d opening command file '%s'\n", errno,
			 argv[1]);
		exit (1);
	}
/*
 * Lat/lon limits
 */
	fscanf (cmdfile, "%f%f%f%f", &Minlat, &Minlon, &Maxlat, &Maxlon);

	if ((Minlat > Maxlat) || (Minlon > Maxlon))
	{
		fprintf (stderr, "limits values illegal: %f %f %f %f\n",
			 Minlat, Minlon, Maxlat, Maxlon);
		exit (1);
	}
/*
 * nx & ny
 */
	GridX = GridY = 0;
	fscanf (cmdfile, "%d%d", &GridX, &GridY);
	if ((GridX * GridY) <= 0)
	{
		fprintf (stderr, "Bad image size: %d x %d\n", GridX, GridY);
		exit (1);
	}

	Latstep = (Maxlat - Minlat) / (GridX - 1);
	Lonstep = (Maxlon - Minlon) / (GridY - 1);

	DoFile (argv[2]);
	exit (0);
}
	



static void *
DoFile (fname)
char	*fname;
/*
 * Read from area file "fname", remapping into a grid and writing that
 * grid.
 */
{
	int	header[64], nav_cod[128];
	int	year, month, day, hour, minute, second;
	unsigned char	*grid;
	int	i, j, line, elem, status, stuff[128], one = 1;
	int	imagelen, ngot;
	float	dummy, fline, felem, lat, lon;
	char	source[5], *c;
	FILE	*areafile;

	if (! (areafile = fopen (fname, "r")))
	{
		fprintf (stderr, "Error %d opening area file '%s'\n", errno,
			 fname);
		exit (1);
	}
/*
 * Read the 256 byte "area directory" header and the 512 byte
 * navigation codicil and swap bytes around in each one.  
 * NOTE: We don't swap in those portions which contain text
 */
	fread ((void *) header, 4, 64, areafile);
	swapfour (header, 20);
	swapfour (header + 32, 19);

	fread ((void *) nav_cod, 4, 128, areafile);
	swapfour (nav_cod + 1, 39);
/*
 * Verify that this is a GOES image
 */
	if (strncmp (nav_cod, "GOES", 4))
	{
		char	imtype[4];

		strncpy (imtype, nav_cod, 4);
		imtype[4] = '\0';

		fprintf (stderr, "'%s' contains a '%s' image, not GOES\n",
			 fname, imtype);
		exit (1);
	}
/*
 * Extract the date.
 */
	year = header[3] / 1000;
	if ((year % 4) == 0)
		Mdays[2] = 29;	/* February has 29 days in leap years */

	day = header[3] % 1000;
	month = 1;
	while (day > Mdays[month])
		day -= Mdays[month++];
/*
 * Time
 */
	hour = header[4] / 10000;
	minute = (header[4] / 100) % 100;
	second = header[4] % 100;
/*
 * Resolution (# of satellite units per image unit)
 */
	Yres = header[11];
	Xres = header[12];
/*
 * If it isn't one byte data, we can't handle it (for now)
 */
	Nbytes = header[10];
	if (Nbytes != 1)
	{
		fprintf (stderr, 
			 "%d byte data will be truncated to one byte\n",
			 Nbytes);
	}
/*
 * Image size (Nx x Ny), bytes per element and prefix length
 */
	Ny = header[8];
	Nx = header[9];
	Prefixlen = header[14];

	Linelen = Nx * Nbytes + Prefixlen;
/*
 * Source name from header word 51 (convert to lower case and remove spaces)
 */
	strncpy (source, header + 51, 4);
	source[4] = '\0';

	for (i = 0; i < 4; i++)
	{
		c = source + i;
		if (*c == ' ')
			*c = '\0';
		else
			*c = tolower (*c);
	}
/*
 * Write out descriptive stuff
 */
	printf ("GOES %s image %02d%02d%02d %02d%02d%02d\n", source, year, 
		month, day, hour, minute, second);
	printf ("%4d x %4d\n", GridX, GridY);
	printf ("NW corner lat: %8.5f  lon: %9.5f\n", Maxlat, Minlon);
	printf ("latstep: %7.5f  lonstep: %7.5f\n", Latstep, Lonstep);
/*
 * 512 byte extra header for "aaa" areas (we ignore it for now)
 */
	if (! strcmp (source, "aaa"))
		fread ((void *) stuff, 4, 128, areafile);
/*
 * Read the image data
 */
	imagelen = Linelen * Ny;
	Image = (unsigned char *) malloc (imagelen);
	ngot = fread ((void *) Image, 1, imagelen, areafile);
	if (ngot != imagelen)
	{
		if (feof (areafile))
			fprintf (stderr, 
				 "Premature EOF.  %d instead of %d bytes\n",
				 ngot, imagelen);
		else
			fprintf (stderr,
				 "Read error %d.  %d instead of %d bytes\n",
				 errno, ngot, imagelen);

		exit (1);
	}
/*
 * We're done with the file
 */
	fclose (areafile);
/*
 * Element and line limits
 */
	Minline = header[5];
	Maxline = Minline + (Ny - 1) * Yres;

	Minelem = header[6];
	Maxelem = Minelem + (Nx - 1) * Xres;
/*
 * Initialize the navigation stuff
 */
	status = nvxini_ (&one, nav_cod);
	if (status < 0)
	{
		fprintf (stderr, 
			"Bad navigation initialization for file '%s'\n", 
			 fname);
		exit (1);
	}
/*
 * Allocate the grid
 */
	grid = (unsigned char *) malloc (GridX * GridY * sizeof (char));
/*
 * Fill in the grid (This is the meat of the program, the rest is more or
 * less incidental.)
 */
  	for (j = 0; j < GridY; j++)
  	{
  		if (! ((j+1) % 20))
 			fprintf (stderr, "line %d of %d, lat %.2f\n", 
				 j + 1, GridY, lat);
  
		lat = Maxlat - j * Latstep;

		for (i = 0; i < GridX; i++)
		{
		/*
		 * Translate lat/lon into line and element in the image
		 * (NOTE: nvxeas expects west longitudes to be positive, hence
		 * the sign change)
		 */
			lon = -(Minlon + i * Lonstep);
			status = nvxeas_ (&lat, &lon, &dummy, &fline, &felem,
				&dummy);

			line = (int)(fline + 0.5);
			elem = (int)(felem + 0.5);
		/*
		 * Assign this grid point
		 */
			if (status == 0)
				grid[GridX * j + i] = imageval (line, elem);
			else
				grid[GridX * j + i] = 0;
		}
	}

	free (Image);
/*
 * Spit out the gridded data
 */
	if (fwrite (grid, GridX * GridY, 1, stdout) != 1)
	{
		fprintf (stderr, "Error %d writing the grid\n", errno);
		exit (1);
	}

	free (grid);
}




static void
swapfour (array, count)
int	*array, count;
/*
 * Swap byte order (0123 -> 3210) for 'count' longwords in 'array'
 */
{
	int	i;
	char	*bytes, swapped[4];

	for (i = 0; i < count; i++)
	{
		bytes = (char *) &(array[i]);
		swapped[0] = bytes[3];
		swapped[1] = bytes[2];
		swapped[2] = bytes[1];
		swapped[3] = bytes[0];
		memcpy (bytes, swapped, 4);
	}
}




static inline unsigned char
imageval (line, elem)
int	line, elem;
/*
 * Return the image value associated with satellite line/elem coordinates
 */
{
	int	image_x, image_y, pos;
	unsigned int	val;

	if (BETWEEN(line, Minline, Maxline) && BETWEEN(elem, Minelem, Maxelem))
	{
	/*
	 * Translate from satellite coordinates to image coordinates
	 */
		image_x = (int)((float)(elem - Minelem) / Xres + 0.5);
		image_y = (int)((float)(line - Minline) / Yres + 0.5);
	/*
	 * Find the offset into the image.  We add (Nbytes - 1) so that
	 * we end up pointing to the last byte of multi-byte data, since
	 * that's the MSB.
	 */
		pos = image_y * Linelen + Prefixlen + image_x * Nbytes + 
			(Nbytes - 1);
	/*
	 * Return the byte.
	 */
		return (Image[pos]);
	}
	else
		return ((unsigned char) 0xff);
}
