/*
 * Simple GRIB file dump program
 */
/*		Copyright (C) 1994 by UCAR
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
# include <stdio.h>
# include <string.h>
# include <errno.h>
# include <fcntl.h>
# include <math.h>

# include <copyright.h>
# include <defs.h>
# include <message.h>
# include "GRIB.h"

static void ShowGDS FP ((GFgds *gds_in));
static int DumpFile FP ((char *file));


int
main (argc, argv)
int	argc;
char	**argv;
{
	int	i;
	int	err;
/*
 * Arg check
 */
	if (argc < 2)
	{
		fprintf (stderr, "Usage: %s <file> [<file> ...]\n", argv[0]);
		exit (1);
	}

	err = 0;
	for (i = 1; i < argc; ++i)
		err += DumpFile (argv[i]);
	return (err);
}



static int
DumpFile (file)
char *file;
{
	static  char *buf = 0;
	static	int buflen = 0;
	int	fd;
	int	is_len, grib_len, pds_len;
	int	status, ng = 0, ncopy;
	char	is[8], *trailer;
	GFpds	pds;
	GFgds	*gds = 0;
	AltUnitType	altunits;
	ZebTime zt;
/*
 * Try to open the file
 */
	if ((fd = open (file, O_RDONLY)) < 0)
	{
		fprintf (stderr, "Error %d opening '%s'", errno, file);
		return (1);
	}
/*
 * Each grid starts with an Indicator Section.  Loop through grids
 * until we fail to get one of these, a read fails, or we reach eof.
 */
	is_len = 8;	/* Fixed length of the Indicator Section */
	while (1)
	{
	/*
	 * Make sure we have the "GRIB" tag at the beginning
	 */
		if ((status = grb_FindRecord (fd, is)) < 0)
		{
			fprintf (stderr, "%s: Could not locate 'GRIB' seq\n", 
				 file);
			return (1);
		}
		else if (status == 0)
			break;
		if (read (fd, is + 4, 4) != 4)
		{
			fprintf (stderr, "%s: Indicator Section incomplete!\n",
				 file);
			exit (1);
		}
	/*
	 * Get the length of this GRIB 'message' (one grid), make sure we
	 * have space for it.
	 */
		grib_len = grb_ThreeByteInt (is + 4);
		if (grib_len > buflen)
		{
			buflen = grib_len;

			if (buf)
				buf = realloc (buf, buflen);
			else
				buf = malloc (buflen);
		}
	/*
	 * Copy in the eight bytes we have, and read the rest
	 */
		memcpy (buf, is, is_len);
		status = read (fd, buf + is_len, grib_len - is_len);
		if (status < (grib_len - is_len))
		{
			fprintf (stderr, 
				 "%s: GRIB file ends with incomplete record",
				 file);
			status = 0;	/* Treat it like an EOF */
			break;
		}
	/*
	 * Read the first 3 bytes of the Product Definition Section to get
	 * the PDS length.  Our structure only holds the (required) first
	 * 28 bytes of the PDS, so we don't copy any more than that.  We
	 * accomodate illegal smaller ones though, by padding with zeros.
	 */
		pds_len = grb_ThreeByteInt (buf + is_len);
		ncopy = (pds_len < sizeof (GFpds)) ? pds_len : sizeof (GFpds);
		memcpy (&pds, buf + is_len, ncopy);
	/*
	 * If we have a Grid Description Section, print it first and just once
	 */
		if (!gds && (pds.section_flags & GDS_FLAG))
		{
			gds = (GFgds *) (buf + is_len + pds_len);
			ShowGDS (gds);
		}
	/*
	 * Print the grid number and reference time
	 */
		grb_ReferenceTime (&pds, &zt);
		printf ("%3d %-20s", ng++, TC_AscTime (&zt, TC_Full));
#ifdef notdef
		printf ("%3d %02d%02d%02d %02d%02d%02d", ng++, pds.year,
			pds.month, pds.day, pds.hour, pds.minute, 0);
#endif
	/*
	 * Grid ID
	 */
		printf ("Id: %3d", pds.grid_id);
	/*
	 * Field
	 */
		printf ("  Field: %3d", pds.field_id);
	/*
	 * Offset
	 */
		printf ("  Offset: %6ds", grb_Offset (&pds));
	/*
	 * Level
	 */
		if (! grb_NormalLevel (&pds))
			printf ("  Z: (level type %d)", pds.level_id);
		else
		{
			float z = grb_ZLevel (&pds, &altunits);
			printf ("  Z: %s", au_AltLabel ((double)z, altunits));
		}
		printf ("\n");
	/*
	 * Sanity check.  Make sure the last 4 bytes of the GRIB record are 
	 * the GRIB trailer "7777"
	 */
		trailer = buf + grib_len - 4;
		if (strncmp (trailer, "7777", 4))
		{
			fprintf (stderr, "%s: Bad GRIB trailer '%4s'", 
				 file, trailer);
			return (1);
		}
	}
/*
 * Complain if we exited on anything other than an EOF
 */
	if (status < 0)
	{
		fprintf (stderr, "Error %d reading GRIB file", errno);
		return (1);
	}

	return (0);
}



static void
PS_LLToXY (phi1, lambda0, phi, lambda, x, y)
float phi1, lambda0;
float phi, lambda;
float *x, *y;
{
	float k;
/*
 * Formula 21-4
 */
	k = 2 / (1 + sin (phi1) * sin (phi) + 
		 cos (phi1) * cos (phi) * cos (lambda - lambda0));
/*
 * Formulas 21-2 and 21-3
 */
	*x = R_Earth * k * cos (phi) * sin (lambda - lambda0);
	*y = R_Earth * k * (cos (phi1) * sin (phi) - 
		 sin (phi1) * cos (phi) * cos (lambda - lambda0));
}


static void
PS_XYToLL (phi1, lambda0, x, y, lat, lon)
float phi1, lambda0;
float x, y;
float *lat, *lon;
{
	double rho, c;
	double phi, lambda;
/*
 * Formulas 20-18 and 21-15
 */
	rho = hypot (x, y);
	c = 2 * atan ((double)(rho / (2 * R_Earth)));
/*
 * Formula 20-15
 */
	lambda = lambda0 + atan (x * sin (c) / 
				     (rho * cos (phi1) * 
				      cos (c) - y * sin (phi1) * sin (c)));
/*
 * Formula 20-14
 */
	phi = asin (cos (c) * sin (phi1) + (y * sin (c) * cos (phi1) / rho));
/*
 * Now convert to degrees and we're done
 */
	*lat = RAD_TO_DEG (phi);
	*lon = RAD_TO_DEG (lambda);
}


static void
ShowGDS (gds_in)
GFgds *gds_in;
/*
 * Summarize this GDS to stdout
 */
{
	GDSPolarStereo *gds;
	int Nx, Ny;
	float lov;
	float la1, lo1;
	float dx, dy;
	float phi1, lambda0, phi, lambda;
	float x0, y0, xpole, ypole, x, y;
	float ipole, jpole;
	float i, j;
	float lat, lon;

	printf ("-----\nGrid Description Section\n");
	printf ("Data Representation Type: %s\n", grb_GDSRepName (gds_in));
	/*
	 * Only care about polar stereographic
	 */
	if (gds_in->data_type != 5)
		return;

	gds = (GDSPolarStereo *) gds_in;
	Nx = grb_TwoByteInt (gds->gd_nx);
	Ny = grb_TwoByteInt (gds->gd_ny);
	printf ("Nx = %4i   Ny = %4i\n", Nx, Ny);
	la1 = grb_ThreeByteSignInt (gds->gd_la1) / 1000.0;
	lo1 = grb_ThreeByteSignInt (gds->gd_lo1) / 1000.0;
	printf ("grid(0,0) at (lon,lat) = (%.3f,%.3f)\n", lo1, la1);
	printf ("Resolution and Component Flags: %i\n", (int)gds->gd_res);
	printf ("[increments %sgiven; earth is %s; u,v relative to %s]\n",
		(gds->gd_res & (1<<7)) ? "" : "not ",
		(gds->gd_res & (1<<6)) ? "oblate spheroid" : "spherical",
		(gds->gd_res & (1<<3)) ? "grid x,y" : "E and N");
	lov = grb_ThreeByteInt (gds->gd_lov) / 1000.0;
	if (lov >= 180.0 && lov <= 360.0)
		lov -= 360.0;
	else if (lov > 360.0)	/* in case a negative lon, contrary to spec */
		lov = grb_ThreeByteSignInt (gds->gd_lov) / 1000.0;
	printf ("Longitude of orientation: %.3f\n", lov);
	dx = grb_ThreeByteInt (gds->gd_dx) / 1000.0;
	dy = grb_ThreeByteInt (gds->gd_dy) / 1000.0;
	printf ("Grid length (km) at lat 60, (dx, dy) = (%.3f km, %.3f km)\n", 
		dx, dy);
	printf ("Projection contains %s pole.\n",
		gds->gd_pole ? "South" : "North");
	printf ("Scan mode: %i ", gds->gd_scanmode);
	printf ("[%si, %sj, adjacent points consecutive along %s]\n",
		(gds->gd_scanmode & (1<<7)) ? "-" : "+",
		(gds->gd_scanmode & (1<<6)) ? "+" : "-",
		(gds->gd_scanmode & (1<<5)) ? "j" : "i");

	phi1 = DEG_TO_RAD(60.0);	/* 60 degrees N */
	lambda0 = DEG_TO_RAD(lov);	/* longitude of orientation */
	phi = DEG_TO_RAD(la1);		/* lat/lon of (0,0) grid point */
	lambda = DEG_TO_RAD(lo1);
	PS_LLToXY (phi1, lambda0, phi, lambda, &x0, &y0);
	printf ("(i=0,j=0) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		lo1, la1, x0, y0);
	phi = phi1;			/* should get back (0,0) */
	lambda = lambda0;
	PS_LLToXY (phi1, lambda0, phi, lambda, &x, &y);
	i = (x - x0) / dx;
	j = (y - y0) / dy;
	printf ("(i=%.2f,j=%.2f) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		i, j, lov, 60.0, x, y);
	phi = DEG_TO_RAD(90.0);		/* North pole */
	lambda = DEG_TO_RAD(lov);
	PS_LLToXY (phi1, lambda0, phi, lambda, &xpole, &ypole);
	jpole = (xpole - x0) / dx;
	ipole = (ypole - y0) / dy;
	printf ("(i=%.2f,j=%.2f) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		ipole, jpole, lov, 90.0, xpole, ypole);
	/*
	 * Upper right corner
	 */
	i = Nx - 1;
	j = Ny - 1;
	x = (dx * (i - ipole)) + xpole;
	y = (dy * (j - jpole)) + ypole;
	PS_XYToLL (phi1, lambda0, x, y, &lat, &lon);
	printf ("(i=%.2f,j=%.2f) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		i, j, lon, lat, x, y);
	/*
	 * Upper left
	 */
	i = 0;
	j = Ny - 1;
	x = (dx * (i - ipole)) + xpole;
	y = (dy * (j - jpole)) + ypole;
	PS_XYToLL (phi1, lambda0, x, y, &lat, &lon);
	printf ("(i=%.2f,j=%.2f) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		i, j, lon, lat, x, y);
	/*
	 * Lower right
	 */
	i = Nx - 1;
	j = 0;
	x = (dx * (i - ipole)) + xpole;
	y = (dy * (j - jpole)) + ypole;
	PS_XYToLL (phi1, lambda0, x, y, &lat, &lon);
	printf ("(i=%.2f,j=%.2f) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		i, j, lon, lat, x, y);

	printf ("-----\n");
}

