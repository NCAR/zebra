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

# include <stdio.h>
# include <errno.h>
# include <fcntl.h>

# include <copyright.h>
# include <defs.h>

/*
 * The GRIB product definition section (PDS)
 */
typedef struct s_GFpds
{
	unsigned char	len;		/* length of PDS		*/
	unsigned char	len1;		/* 2nd byte of length		*/
	unsigned char	len2;		/* 3rd byte of length		*/
	unsigned char	pt_version;	/* Param. Table version number	*/
	unsigned char	center_id;	/* ID of source center		*/
	unsigned char	process_id;	/* ID of generating process	*/
	unsigned char	grid_id;	/* grid type ID			*/
	unsigned char	section_flags;	/* do we have a GDS and/or BMS?	*/
	unsigned char	field_id;	/* parameter and units indicator */
	unsigned char	level_id;	/* level type indicator		*/
	unsigned char	level_val;	/* level value			*/
	unsigned char	level_val1;	/* level value (2nd byte)	*/
	unsigned char	year;		/* year % 100			*/
	unsigned char	month;
	unsigned char	day;
	unsigned char	hour;
	unsigned char	minute;
	unsigned char	time_unit;	/* time unit ID			*/
	unsigned char	p1;
	unsigned char	p2;
	unsigned char	range_id;	/* time range indicator		*/
	unsigned char	num_in_avg;	/* num. of points used if averaging */
	unsigned char	num_in_avg1;	/* 2nd byte			*/
	unsigned char	avg_missing;	/* num. missing when averaging	*/
	unsigned char	century;	/* century (20 until 1 Jan 2001)*/
	unsigned char	reserved0;
	/* 
	 * We can use short for ds_factor; it falls on an even byte boundary
	 */
	short		ds_factor;	/* decimal scale factor		*/
	unsigned char	reserved1;	/* any length of reserved data	*/
} GFpds;

/*
 * Prototypes
 */
static int	ThreeByteInt FP ((char *));
static int	TwoByteInt FP ((char *));
static bool	NormalLevel FP ((GFpds *));
static float	ZLevel FP ((GFpds *, AltUnitType *));
static int	Offset FP ((GFpds *));




main (argc, argv)
int	argc;
char	**argv;
{
	int	fd;
	int	is_len, grib_len, pds_len;
	int	status, ng = 0, ncopy, buflen = 0;
	char	is[8], *trailer, *buf = 0;
	GFpds	pds;
	AltUnitType	altunits;
/*
 * Arg check
 */
	if (argc != 2)
	{
		fprintf (stderr, "Usage: %s <grib_file>\n", argv[0]);
		exit (1);
	}
/*
 * Try to open the file
 */
	if ((fd = open (argv[1], O_RDONLY)) < 0)
	{
		fprintf (stderr, "Error %d opening '%s'", errno, argv[1]);
		exit (1);
	}
/*
 * Each grid starts with an Indicator Section.  Loop through grids
 * until we fail to get one of these.
 */
	is_len = 8;	/* Fixed length of the Indicator Section */

	while ((status = read (fd, is, is_len)) == is_len)
	{
	/*
	 * Make sure we have the "GRIB" tag at the beginning
	 */
		if (strncmp (is, "GRIB", 4))
		{
			fprintf (stderr, "Got '%4s' instead of 'GRIB'!", 
				  is);
			exit (1);
		}
	/*
	 * Get the length of this GRIB 'message' (one grid), make sure we
	 * have space for it.
	 */
		grib_len = ThreeByteInt (is + 4);
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
				  "GRIB file ends with incomplete record");
			status = 0;	/* Treat it like an EOF */
			break;
		}
	/*
	 * Read the first 3 bytes of the Product Definition Section to get
	 * the PDS length.  Our structure only holds the (required) first
	 * 28 bytes of the PDS, so we don't copy any more than that.  We
	 * accomodate illegal smaller ones though, by padding with zeros.
	 */
		pds_len = ThreeByteInt (buf + is_len);
		ncopy = (pds_len < sizeof (GFpds)) ? pds_len : sizeof (GFpds);
		memcpy (&pds, buf + is_len, ncopy);
	/*
	 * Print the grid number and time
	 */
		printf ("%3d %02d%02d%02d %02d%02d%02d", ng++, pds.year,
			pds.month, pds.day, pds.hour, pds.minute, 0);
	/*
	 * Grid ID
	 */
		printf ("  Id: %3d", pds.grid_id);
	/*
	 * Field
	 */
		printf ("  Field: %3d", pds.field_id);
	/*
	 * Offset
	 */
		printf ("  Offset: %6ds", Offset (&pds));
	/*
	 * Level
	 */
		if (! NormalLevel (&pds))
			printf ("  Z: (level type %d)", pds.level_id);
		else
			printf ("  Z: %.0f %s", ZLevel (&pds, &altunits), 
				au_UnitsName (altunits));

		printf ("\n");
	/*
	 * Sanity check.  Make sure the last 4 bytes of the GRIB record are 
	 * the GRIB trailer "7777"
	 */
		trailer = buf + grib_len - 4;
		if (strncmp (trailer, "7777", 4))
		{
			fprintf (stderr, "Bad GRIB trailer '%4s'", trailer);
			exit (1);
		}
	}
/*
 * Complain if we exited on anything other than an EOF
 */
	if (status < 0)
	{
		fprintf (stderr, "Error %d reading GRIB file", errno);
		exit (1);
	}

	exit (0);
}




static int
ThreeByteInt (buf)
char	*buf;
/*
 * Extract the first three bytes of buf into an int and return it.
 */
{
	int	i = 0;
	char	*cptr = (char *) &i;

	memcpy (cptr + 1, buf, 3);
	return (i);
}




static int
TwoByteInt (buf)
char	*buf;
/*
 * Extract the first three bytes of buf into an int and return it.
 */
{
	int	i = 0;
	char	*cptr = (char *) &i;

	memcpy (cptr + 2, buf, 2);
	return (i);
}




static int
Offset (pds)
GFpds	*pds;
/*
 * Return the forecast time (offset), in seconds, from the given PDS.
 */
{
	int	multiplier;
/*
 * Get the multiplier for our forecast time units
 */
	switch (pds->time_unit)
	{
	    case 0:
		multiplier = 60;	/* minute */
		break;
	    case 1:
		multiplier = 3600;	/* hour */
		break;
	    case 2:
		multiplier = 86400;	/* day */
		break;
	    case 254:
		multiplier = 1;		/* second */
		break;
	    default:
		fprintf (stderr, "Forecast time units too big!  Using zero.");
		return (0);
	}
/*
 * Figure out the valid time based on the time range indicator
 */
	switch (pds->range_id)
	{
	    case 0:
	    case 2:
	    case 3:
		return (multiplier * pds->p1);
	    case 1:
		return (0);
	/*
	 * The accumulation (4) and difference (5) types are considered valid
	 * at the reference time + p2
	 */
	    case 4:
	    case 5:
		return (multiplier * pds->p2);
	    default:
		fprintf (stderr, "Can't handle time range ID %d!", 
			 pds->range_id);
		return (0);
	}
}




static bool
NormalLevel (pds)
GFpds	*pds;
/*
 * Return TRUE if the level of this GRIB grid is "normal" (i.e., a specific
 * isobaric level or height, rather than earth surface, cloud base, tropopause,
 * etc.)
 */
{
	int	l_id = pds->level_id;

	return (l_id == 100 || l_id == 103);
}




static float
ZLevel (pds, units)
GFpds	*pds;
AltUnitType	*units;
/*
 * Return the vertical level from the given PDS.  If 'units' is non-NULL,
 * return the units type.
 */
{
	int	l_id = pds->level_id;

	switch (l_id)
	{
	    case 100:
		if (units)
			*units = AU_mb;		/* millibars */
		break;
	    case 103:
		if (units)
			*units = AU_mMSL;	/* meters MSL */
		break;
	    default:
		fprintf (stderr, "Can't deal with level type %d!", l_id);
		return (-1.0);
	}

	return ((float) TwoByteInt (&(pds->level_val)));
}
