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
static void ShowGDS_LL FP ((GDSLatLon *gds_in));
static void ShowGDS_PS FP ((GDSPolarStereo *gds_in));
static void ShowGDS_LC FP ((GDSLambertConformal *gds_in));
static void ShowResCompFlags FP ((int rcflags));
static void ShowScanMode FP ((int scanmode));
static int DumpFile FP ((char *file));
static void DumpGrid FP ((int ng, GFgds *gds, GFpds *pds));


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

	msg_connect (NULL, "GRIBdump");
	msg_ELPrintMask (EF_EMERGENCY | EF_PROBLEM | EF_INFO);
	err = 0;
	for (i = 1; i < argc; ++i)
		err += DumpFile (argv[i]);
	return (err);
}



static int
DumpFile (file)
char *file;
{
	int	fd;
	int	len, pds_len, bms_len, bds_len;
	int	status, ng, ncopy, ednum, bds_pos;
	unsigned char buf[64];
	GFpds	PDS, *pds = &PDS;
	GFgds	GDS, *gds = &GDS;
	int 	showgds = 1;
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
 * looking for an IS.  In Edition 1 and beyond, the IS is 8 bytes long,
 * but it's 4 bytes long in Edition 0.
 * An added complication is that sometimes records are separated by 20 blanks.
 */
	ng = 0;
	while ((status = grb_FindRecord (fd, buf)) > 0)
	{
	/*
	 * Read the next 4 bytes and determine the GRIB edition.  In Edition 1
	 * and beyond, these are the the last 4 bytes of the IS.  For Edition
	 * 0 files, they're the first four bytes of the PDS...
	 */
		if (read (fd, buf, 4) < 4)
		{
			msg_ELog (EF_INFO, 
				  "GRIB file ends with incomplete record");
			status = 0;	/* Treat it like an EOF */
			break;
		}
	/*
	 * Figure out the GRIB Edition.  If the three byte length here is
	 * 24 (the length of an Edition 0 PDS), we assume that it's Edition 0.
	 * Otherwise, assume it's Edition 1 or later and we can get the real
	 * edition number from the fourth byte.
	 */
		len = grb_ThreeByteInt (buf);
		ednum = (len == 24) ? 0 : (int) buf[3];
	/*
	 * For Edition > 0, we still need to read the first four bytes of 
	 * the PDS.
	 */
		if (ednum != 0 && read (fd, buf, 4) < 4)
		{
			msg_ELog (EF_INFO, "Missing PDS at grid %d", ng + 1);
			status = 0;	/* Treat it like an EOF */
			break;
		}

		pds_len = grb_ThreeByteInt (buf);
	/*
	 * Read the rest of the PDS into our buffer. 
	 */
		if (read (fd, buf + 4, pds_len - 4) != pds_len - 4)
		{
			msg_ELog (EF_INFO, "Short PDS at grid %d", ng + 1);
			status = 0;	/* Treat it like an EOF */
			break;
		}
	/*
	 * Copy up to 28 bytes into our PDS space.  We don't need to keep
	 * the stuff (if any) beyond that.
	 */
		ncopy = (pds_len < sizeof (GFpds)) ? pds_len : sizeof (GFpds);
		memcpy (pds, buf, ncopy);
	/*
	 * If we have a Grid Description Section, read it
	 */
		if (pds->section_flags & GDS_FLAG)
		{
			gds = &GDS;
			if ((status = grb_ReadGDS (fd, gds, ng+1)) <= 0)
				break;
			if (showgds)
			{
				ShowGDS (gds);
				showgds = 0;
			}
		}
		else
			gds = NULL;
	/*
	 * If there's a Bit Map Section, bypass it.
	 */
		if (pds->section_flags & BMS_FLAG)
		{
		/*
		 * Read the first four bytes of the BMS and get its length
		 */
			if (read (fd, buf, 4) < 4)
			{
				msg_ELog (EF_INFO, 
					  "Missing BMS at grid %d", ng + 1);
				status = 0;	/* Treat it like an EOF */
				break;
			}

			bms_len = grb_ThreeByteInt (buf);
		/*
		 * Seek past the rest
		 */
			lseek (fd, bms_len - 4, SEEK_CUR);
		}
	/*
	 * We're at the Binary Data Section.
	 */
		bds_pos = lseek (fd, 0, SEEK_CUR);

		if (read (fd, buf, 4) < 4)
		{
			msg_ELog (EF_INFO, 
				  "Missing BDS at grid %d", ng + 1);
			status = 0;	/* Treat it like an EOF */
			break;
		}
		bds_len = grb_ThreeByteInt (buf);
	/*
	 * Skip over the rest of the BDS
	 */
		lseek (fd, bds_len - 4, SEEK_CUR);
	/*
	 * It looks like we really have a GRIB record here.
	 */
		++ng;
		DumpGrid (ng, gds, pds);
	/*
	 * Sanity check.  Make sure the last 4 bytes of the GRIB record are 
	 * the GRIB trailer "7777"
	 */
		if (read (fd, buf, 4) < 4)
		{
			msg_ELog (EF_INFO, "Missing trailer at grid %d", ng);
			status = 0;	/* Treat it as an EOF */
			break;
		}
		
		if (strncmp ((char *) buf, "7777", 4))
		{
			msg_ELog (EF_EMERGENCY, 
				  "Bad GRIB trailer '%4s' at grid %d",
				  buf, ng);
			return (1);
		}
	}
/*
 * Complain if we exited on anything other than an EOF
 */
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d reading GRIB file at grid %d", 
			  errno, ng);
		return (1);
	}
	else
		return (0);
}




static void
DumpGrid (ng, gds, pds)
int ng;
GFgds *gds;
GFpds *pds;
{
	AltUnitType altunits;
	ZebTime zt;
	float z;

	/*
	 * Print the grid number and reference time
	 */
	grb_ReferenceTime (pds, &zt);
	printf ("%3d %-20s", ng, TC_AscTime (&zt, TC_Full));
	/*
	 * Grid ID
	 */
	printf ("Id: %3d", pds->grid_id);
	/*
	 * Field
	 */
	printf ("  Field: %3d", pds->field_id);
	/*
	 * Offset
	 */
	printf ("  Offset: %6ds", grb_Offset (pds));
	/*
	 * Level
	 */
	altunits = -1;
	z = grb_ZLevel (pds, &altunits);

	if (altunits >= 0)
		printf ("  Z: %s", au_AltLabel ((double)z, altunits));
	else
		printf ("  Z: (level type %d)", pds->level_id);

	printf ("\n");
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

	printf ("-----\nGrid Description Section\n");
	printf ("Data Representation Type: %s\n", grb_GDSRepName (gds_in));

	switch (gds_in->data_type)
	{
	  case 0:
	    ShowGDS_LL ((GDSLatLon *)gds_in);
	    break;
	  case 5:
	    ShowGDS_PS ((GDSPolarStereo *)gds_in);
	    break;
	  case 3:
	    ShowGDS_LC ((GDSLambertConformal *)gds_in);
	    break;
	}

	printf ("-----\n");
}



static void
ShowGDS_LL (gds)
GDSLatLon *gds;
/*
 * Summarize a Lat/Lon GDS to stdout
 */
{
	int nx, ny;
	float lov;
	float la1, lo1;
	float la2, lo2;
	float dx, dy;
	float x0, y0, xpole, ypole, x, y;
	float ipole, jpole;
	float i, j;
	float lat, lon;

	nx = grb_TwoByteInt (gds->gd_ni);
	ny = grb_TwoByteInt (gds->gd_nj);
	printf ("Nx = %4i   Ny = %4i\n", nx, ny);
	la1 = grb_ThreeByteSignInt (gds->gd_lat1) / 1000.0;
	lo1 = grb_ThreeByteSignInt (gds->gd_lon1) / 1000.0;
	la2 = grb_ThreeByteSignInt (gds->gd_lat2) / 1000.0;
	lo2 = grb_ThreeByteSignInt (gds->gd_lon2) / 1000.0;
	printf ("grid(0,0) @ lon/lat %.3f/%.3f\n", lo1, la1);
	printf ("grid(%d,%d) @ lon/lat %.3f/%.3f\n", nx-1, ny-1, lo2, la2);
	dx = grb_TwoByteInt (gds->gd_di) / 1000.0;
	dy = grb_TwoByteInt (gds->gd_dj) / 1000.0;
	printf ("lon/lat step %.3f/%.3f\n", dx, dy);

	ShowResCompFlags ((int)gds->gd_res);
	ShowScanMode ((int)gds->gd_scanmode);
}




static void
ShowGDS_PS (gds)
GDSPolarStereo *gds;
/*
 * Summarize a Polar Stereographic GDS to stdout
 */
{
	int nx, ny;
	float lov;
	float la1, lo1;
	float dx, dy;
	float phi1, lambda0, phi, lambda;
	float x0, y0, xpole, ypole, x, y;
	float ipole, jpole;
	float i, j;
	float lat, lon;

	nx = grb_TwoByteInt (gds->gd_nx);
	ny = grb_TwoByteInt (gds->gd_ny);
	printf ("Nx = %4i   Ny = %4i\n", nx, ny);
	la1 = grb_ThreeByteSignInt (gds->gd_lat1) / 1000.0;
	lo1 = grb_ThreeByteSignInt (gds->gd_lon1) / 1000.0;
	printf ("grid(0,0) @ lon/lat %.3f/%.3f\n", lo1, la1);

	ShowResCompFlags ((int)gds->gd_res);

	lov = grb_ThreeByteInt (gds->gd_lov) / 1000.0;
	if (lov >= 180.0 && lov <= 360.0)
		lov -= 360.0;
	else if (lov > 360.0)	/* in case a negative lon, contrary to spec */
		lov = grb_ThreeByteSignInt (gds->gd_lov) / 1000.0;
	printf ("Longitude of orientation: %.3f\n", lov);
	dx = grb_ThreeByteInt (gds->gd_dx) / 1000.0;
	dy = grb_ThreeByteInt (gds->gd_dy) / 1000.0;
	printf ("Grid length at lat 60, (dx, dy) = (%.3f km, %.3f km)\n", 
		dx, dy);
	printf ("Projection contains %s pole.\n",
		gds->gd_pole ? "South" : "North");

	ShowScanMode ((int)gds->gd_scanmode);

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
	i = nx - 1;
	j = ny - 1;
	x = (dx * (i - ipole)) + xpole;
	y = (dy * (j - jpole)) + ypole;
	PS_XYToLL (phi1, lambda0, x, y, &lat, &lon);
	printf ("(i=%.2f,j=%.2f) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		i, j, lon, lat, x, y);
	/*
	 * Upper left
	 */
	i = 0;
	j = ny - 1;
	x = (dx * (i - ipole)) + xpole;
	y = (dy * (j - jpole)) + ypole;
	PS_XYToLL (phi1, lambda0, x, y, &lat, &lon);
	printf ("(i=%.2f,j=%.2f) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		i, j, lon, lat, x, y);
	/*
	 * Lower right
	 */
	i = nx - 1;
	j = 0;
	x = (dx * (i - ipole)) + xpole;
	y = (dy * (j - jpole)) + ypole;
	PS_XYToLL (phi1, lambda0, x, y, &lat, &lon);
	printf ("(i=%.2f,j=%.2f) : (%.3f,%.3f) : (x=%.3f km, y=%.3f km)\n",
		i, j, lon, lat, x, y);
}



static void
ShowGDS_LC (gds)
GDSLambertConformal *gds;
/*
 * Summarize a Lambert Conformal GDS to stdout
 */
{
	int nx, ny;
	float lov;
	float la1, lo1;
	float dx, dy;
	float phi1, phi2, lambda0;
	float spol_lat, spol_lon;

	nx = grb_TwoByteInt (gds->gd_nx);
	ny = grb_TwoByteInt (gds->gd_ny);
	printf ("Nx = %4i   Ny = %4i\n", nx, ny);
	la1 = grb_ThreeByteSignInt (gds->gd_lat1) / 1000.0;
	lo1 = grb_ThreeByteSignInt (gds->gd_lon1) / 1000.0;
	printf ("grid(0,0) @ lon/lat %.3f/%.3f\n", lo1, la1);

	ShowResCompFlags ((int)gds->gd_res);

	lov = grb_ThreeByteInt (gds->gd_lov) / 1000.0;
	if (lov >= 180.0 && lov <= 360.0)
		lov -= 360.0;
	else if (lov > 360.0)	/* in case a negative lon, contrary to spec */
		lov = grb_ThreeByteSignInt (gds->gd_lov) / 1000.0;
	printf ("Longitude of orientation: %.3f\n", lov);
	dx = grb_ThreeByteInt (gds->gd_dx) / 1000.0;
	dy = grb_ThreeByteInt (gds->gd_dy) / 1000.0;
	printf ("Grid length (dx, dy) = (%.3f km, %.3f km)\n", dx, dy);
	if (gds->gd_pole && 1<<6)
	    printf ("Projection is bipolar and symmetric\n");
	else
	    printf ("Projection contains %s pole.\n",
		    (gds->gd_pole && 1<<7) ? "South" : "North");

	ShowScanMode ((int) gds->gd_scanmode);

	phi1 = grb_ThreeByteSignInt (gds->gd_latin1) / 1000.0;
	phi2 = grb_ThreeByteSignInt (gds->gd_latin2) / 1000.0;
	printf ("Cone/Earth intersections -- phi1: %.3f, phi2: %.3f\n",
		phi1, phi2);

	spol_lat = grb_ThreeByteSignInt (gds->gd_spol_lat) / 1000.0;
	spol_lon = grb_ThreeByteSignInt (gds->gd_spol_lon) / 1000.0;
	printf ("Southern pole at (lon,lat) = (%.3f,%.3f)\n", spol_lon, 
		spol_lat);
}



static void
ShowResCompFlags (int rcflags)
/*
 * Decode and print the GDS resolution and component flags
 */
{
	printf ("Resolution and Component Flags: %i\n", rcflags);
	printf ("[increments %sgiven; earth is %s; u,v relative to %s]\n",
		(rcflags & (1<<7)) ? "" : "not ",
		(rcflags & (1<<6)) ? "oblate spheroid" : "spherical",
		(rcflags & (1<<3)) ? "grid x,y" : "E and N");
}



static void
ShowScanMode (int scanmode)
/*
 * Decode and print the GDS scan mode flags
 */
{
	printf ("Scan mode: %i ", scanmode);
	printf ("[%si, %sj, adjacent points consecutive along %s]\n",
		(scanmode & (1<<7)) ? "-" : "+",
		(scanmode & (1<<6)) ? "+" : "-",
		(scanmode & (1<<5)) ? "j" : "i");
}
