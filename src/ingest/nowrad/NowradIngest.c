/*
 * Consume NOWRAD images into the data store.
 */
/*		Copyright (C) 1992 by UCAR
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

static char *rcsid = "$Id";

# include <copyright.h>
# include <unistd.h>
# include <errno.h>
# include <math.h>
# include <stdio.h>
# include <dirent.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>


/*
 * The image and the grid to which we're mapping it
 */
unsigned char	*Image;

/*
 * Image unpacking info
 */
int	Prefixlen, Linelen, Xres, Yres;

/*
 * Image limits in line/elem and lat/lon coordinates
 */
int	Minelem, Maxelem, Minline, Maxline;
float	Minlon, Maxlon, Minlat, Maxlat;

/*
 * Origin latitude
 */
float	Origin_lat;

/*
 * Useful stuff
 */
# define BETWEEN(x,lower,upper)	(((x)-(lower))*((x)-(upper)) <= 0)
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */

int	Mdays[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/*
 * Prototypes
 */
static void	swapfour FP ((int *, int));
static void	ll_limits FP ((void));
static unsigned char	imageval FP ((int, int));
static int	MDispatcher FP ((struct message *));
static int	DoFile FP ((char *, char *));
static void	Die FP ((void));




main (argc, argv)
int argc;
char **argv;
{
/*
 * Checking
 */
	if (argc != 4)
	{
		printf ("Usage: %s <platform> <file> <origin_lat>\n", argv[0]);
		exit (1);
	}
/*
 * Initialize UI symbol stuff and connect to the message handler
 */
	usy_init ();
	msg_connect (MDispatcher, "NowradIngest");
/*
 * DS initialization
 */
	ds_Initialize ();

	if (sscanf (argv[3], "%f", &Origin_lat) != 1)
	{
		msg_ELog (EF_PROBLEM, "Bad origin latitude '%s'", argv[3]);
		Die ();
	}

	DoFile (argv[1], argv[2]);
	Die ();
}




static int
DoFile (platform, fname)
char	*platform, *fname;
/*
 * Ingest the named NOWRAD file.
 */
{
	FILE	*infile;
	int	header[64], nav_cod[128];
	int	i, j, nbytes, line, elem, status, year, month, day;
	int	hour, minute, second, nx, ny;
	int	one = 1, two = 2;
	float	dummy, fline, felem, lat, lon, latstep, lonstep;
	ZebTime	t;
	RGrid	rg;
	DataChunk	*dc;
	ScaleInfo	scale;
	Location	loc;
	PlatformId	pid;
	FieldId		fid;
	unsigned char	*grid;
/*
 * Platform ID
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", platform);
		Die ();
	}

/*
 * Open the file
 */
	infile = fopen (fname, "r");

	if (infile == 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening file '%s'\n", errno,
			fname);
		Die ();
	}
/*
 * Read the 256 byte "area directory" header and the 512 byte
 * navigation codicil and swap bytes around in each one.  
 * NOTE: We don't swap in those portions which contain text
 */
	fread ((void *) header, 4, 64, infile);
	swapfour (header, 20);
	swapfour (header + 32, 19);

	fread ((void *) nav_cod, 4, 128, infile);
	swapfour (nav_cod + 1, 39);
/*
 * Verify that this is a RECT (rectangular projection) image
 */
	if (strncmp (nav_cod, "RECT", 4))
	{
		char	imtype[4];

		strncpy (imtype, nav_cod, 4);
		imtype[4] = '\0';

		msg_ELog (EF_PROBLEM, "'%s' contains a '%s' image, not RECT",
			fname, imtype);
		fclose (infile);
		Die ();
	}
/*
 * Resolution (# of satellite units per image unit)
 */
	Yres = header[11];
	Xres = header[12];
/*
 * Image size (nx x ny), and prefix length
 */
	ny = header[8];
	nx = header[9];
	Prefixlen = header[14];

	Linelen = nx + Prefixlen;
/*
 * Data size test
 */
	nbytes = header[10];
	if (nbytes != 1)
	{
		msg_ELog (EF_EMERGENCY, "Cannot handle %d byte data!");
		exit (1);
	}
/*
 * Read the image data
 */
	Image = (unsigned char *) malloc (Linelen * ny);
	fread ((void *) Image, 1, Linelen * ny, infile);
/*
 * We're done with the file
 */
	fclose (infile);
/*
 * Element and line limits
 */
	Minline = header[5];
	Maxline = Minline + (ny - 1) * Yres;

	Minelem = header[6];
	Maxelem = Minelem + (nx - 1) * Xres;
/*
 * Initialize the navigation stuff
 */
	status = nvxini_ (&one, nav_cod);
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Bad navigation initialization for file '%s'", fname);
		Die ();
	}

	status = nvxini_ (&two, "LL  ");
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Can't use lan/lon coords for file '%s'", fname);
		Die ();
	}
/*
 * Get the lat/lon limits
 * (NOTE: W longitudes are positive here, because that's what nvxeas
 * expects)
 */
	ll_limits ();
/*
 * Find the lat/lon steps
 */
	latstep = (Maxlat - Minlat) / (ny - 1);
	lonstep = (Maxlon - Minlon) / (nx - 1);
/*
 * Allocate the grid
 */
	grid = (unsigned char *) malloc (nx * ny * sizeof (char));
/*
 * Loop through the points in the grid
 */
	for (j = 0; j < ny; j++)
	{
		if (! ((j+1) % 20))
			msg_ELog (EF_DEBUG, "%s: line %d of %d", fname,
				j + 1, ny);

		lat = Maxlat - j * latstep;

		for (i = 0; i < nx; i++)
		{
		/*
		 * Translate lat/lon into line and element in the image
		 */
			lon = Maxlon - i * lonstep;
			status = nvxeas_ (&lat, &lon, &dummy, &fline, &felem,
				&dummy);

			line = (int)(fline + 0.5);
			elem = (int)(felem + 0.5);
		/*
		 * Assign this grid point
		 */
			if (status == 0)
				grid[nx * j + i] = imageval (line, elem);
			else
				grid[nx * j + i] = 0;
		}
	}
/*
 * Get a field ID
 */
	F_Init ();
	fid = F_DeclareField ("intensity", "", "");
/*
 * Scaling
 */
	scale.s_Scale = 1.0;
	scale.s_Offset = 0.0;
/*
 * Location
 */
	loc.l_lat = Minlat;
	loc.l_lon = -Maxlon;
	loc.l_alt = 0.000;
/*
 * rgrid info
 */
	rg.rg_Xspacing = DEG_TO_KM (lonstep) * cos (DEG_TO_RAD (Origin_lat));
	rg.rg_Yspacing = DEG_TO_KM (latstep);
	rg.rg_Zspacing = 0.0;

	rg.rg_nX = nx;
	rg.rg_nY = ny;
	rg.rg_nZ = 1;
/*
 * Create the data chunk
 */
        dc = dc_CreateDC (DCC_Image);
        dc->dc_Platform = pid;
        dc_ImgSetup (dc, 1, &fid, &scale);
/*
 * Date and time (We have to convert from Julian date.  I hate that.)
 */
	year = header[3] / 1000;
	if ((year % 4) == 0)
		Mdays[2] = 29;	/* February has 29 days in leap years */

	day = header[3] % 1000;
	month = 1;
	while (day > Mdays[month])
		day -= Mdays[month++];

	hour = header[4] / 10000;
	minute = (header[4] / 100) % 100;
	second = header[4] % 100;

	TC_ZtAssemble (&t, year, month, day, hour, minute, second, 0);
/*
 * Shove the image into the data chunk
 */
	dc_ImgAddImage (dc, 0, fid, &loc, &rg, &t, grid, nx * ny);
	msg_ELog (EF_INFO, "Nowrad image %dx%d, %.1fx%.1f deg.", nx, ny,
		nx * lonstep, ny * latstep);
/*
 * Send it
 */
	ds_Store (dc, 1, NULL, 0);
/*
 * Free all that memory we hogged up
 */
	free (Image);
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




static void
ll_limits ()
{
	float	fline, felem, dummy, lat, lon;
/*
 * Find the lat/lon limits based on the corners of the image
 * (NOTE: nvxsae returns W longitudes as positive, but that's OK since
 *  the numbers should only used for calls to nvxeas, which expects
 *  just that)
 */
	Minlat = Minlon = 999.0;
	Maxlat = Maxlon = -999.0;

	fline = Minline; felem = Minelem;
	nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Minline; felem = Maxelem;
	nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Maxline; felem = Minelem;
	nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Maxline; felem = Maxelem;
	nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;
}




static unsigned char
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
	 * Find the offset into the image
	 */
		pos = image_y * Linelen + Prefixlen + image_x;
	/*
	 * Return the value
	 */
		return (Image[pos]);
	}
	else
		return ((unsigned char) 0xff);
}




static int
MDispatcher (msg)
struct message *msg;
/*
 * Deal with a message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE)
			exit (1);
		break;
	}
	return (0);
}   	




static void
Die ()
/*
 * Finish gracefully.
 */
{
	ui_finish ();
	exit (0);
}
