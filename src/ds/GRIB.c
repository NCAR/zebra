/*
 * GRIB access routines which only require the library
 */
/*		Copyright (C) 1987-1995 by UCAR
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
# include <math.h>
# include <errno.h>
# include <fcntl.h>
# include <unistd.h>
# include <string.h>
# include <memory.h>

# include <copyright.h>
# include <defs.h>
# include <message.h>

# include "GRIB.h"

RCSID ("$Id: GRIB.c,v 3.1 1995-04-17 22:33:06 granger Exp $")

typedef struct s_GRB_DataRepType {
	int data_type;
	char *name;
} GRB_DataRepType;

static GRB_DataRepType DataRepTypes[] =
{
	{ 0, "Latitude/Longitude Grid" },
	{ 3, "Lambert Conformal" },
	{ 5, "Polar Stereographic Projection Grid" },
};

static const int NDataRepTypes = sizeof(DataRepTypes)/sizeof(DataRepTypes[0]);

static int grb_TimeUnits FP ((GFpds *pds));
static int grb_Level FP ((GFpds *pds, int sfc_only, AltUnitType *units,
			  float *altitude));


int
grb_TwoByteInt (buf)
char	*buf;
/*
 * Extract the first two bytes of buf into an int and return it.
 */
{
	int	i = 0;
	char	*cptr = (char *) &i;

	memcpy (cptr + 2, buf, 2);
	return (i);
}



int
grb_ThreeByteInt (buf)
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



int
grb_ThreeByteSignInt (buf)
char	*buf;
/*
 * Extract the first three bytes of buf into an int and return it.
 */
{
	int	i = 0, sign;
	char	*cptr = (char *) &i;
/*
 * Upper bit is the sign.
 */
	sign = (buf[0] & 0x80) ? -1 : 1;
/*
 * Extract the three bytes into an int and lose the top bit.
 */
	memcpy (cptr + 1, buf, 3);
	i &= 0x7fffff;
/*
 * Multiply in the sign.
 */
	i *= sign;
	return (i);
}



char *
grb_GDSRepName (gds)
GFgds *gds;
{
	static char buf[16];
	int i;

	for (i = 0; i < NDataRepTypes; ++i)
	{
		if (DataRepTypes[i].data_type == gds->data_type)
			return (DataRepTypes[i].name);
	}
	sprintf (buf, "%d (unknown)", gds->data_type);
	return (buf);
}




int
grb_FindRecord (fd, buf)
int fd;
char *buf;
/*
 * Look for the 'GRIB' string which starts a GRIB record.  Return -1 if we
 * couldn't find it, 0 on end of file, otherwise return the number of bytes
 * read to find it.  If successful, buf will contain the GRIB string and
 * the file descriptor will point to the byte following 'GRIB'.  Bail
 * if we don't find anything within 256 bytes. 
 */
{
	int fill, nread;
	int status;
	char *c;

	if ((status = read (fd, buf, 4)) != 4)
		return (status);
	nread = 4;
	while (nread < 256)
	{
		if (strncmp (buf, "GRIB", 4) == 0)
			return (nread);
		/*
		 * If there's a 'G' here somewhere it may be the beginning 
		 * of a string which was cut off.
		 */
		c = (char *) memchr (buf, (int) 'G', 4);
		if (c && (c > buf))
		{
			memcpy (buf, c, buf + 4 - c);
			fill = c - buf;
		}
		else
			fill = 4;
		if ((status = read (fd, buf + 4 - fill, fill)) != fill)
			return (status);
		nread += fill;
	}
	return (-1);
}

			

int
grb_ReadGDS (fd, gds_ret, ng)
int fd;
GFgds *gds_ret;
int ng;		/* number of this grid, for reference */
/*
 * Try to read a Grid Description Section from the file.  If buf non-NULL,
 * return the GDS there.  On success, the file points to the first byte
 * following the GDS.
 */
{
	char buf[16];
	char *gds;
	int gds_len;
	int status;

	gds = (gds_ret) ? (char *)gds_ret : buf;
	if (read (fd, gds, 4)  < 4)
	{
		msg_ELog (EF_INFO, "Missing GDS at grid %d", ng + 1);
		return (0);	/* Treat it like an EOF */
	}
	
	gds_len = grb_ThreeByteInt (gds);
	/*
	 * Read the rest of the GDS if they want, otherwise seek past it.
	 */
	if (! gds_ret)
	{
		/*
		 * Seek past the rest
		 */
		lseek (fd, gds_len - 4, SEEK_CUR);
		status = gds_len;
	}
	else if (read (fd, gds + 4, gds_len - 4) != gds_len - 4)
	{
		msg_ELog (EF_INFO, "Short GDS at grid %d", ng);
		status = 0;	/* Treat it like an EOF */
	}
	else
		status = gds_len;

	return (status);
}



void
grb_ReferenceTime (pds, zt)
GFpds *pds;
ZebTime *zt;
/*
 * Extract the reference time from the PDS and put it into zt
 */
{
	TC_ZtAssemble (zt, pds->year, pds->month, pds->day, pds->hour, 
		       pds->minute, 0, 0);
/*
 * XXX: If the generating center is NCAR and the process is MM5, then
 * interpret 'forecast' times as valid times.
 */
	if (pds->center_id == 60 && pds->process_id == 95)
		zt->zt_Sec += (pds->p1 * grb_TimeUnits (pds));
}



static int
grb_TimeUnits (pds)
GFpds *pds;
/*
 * Return the time units as seconds
 */
{
	int multiplier;

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
		msg_ELog (EF_EMERGENCY, 
			  "GRIB forecast time units too big!  Using zero.");
		multiplier = 1;
	}
	return (multiplier);
}



int
grb_Offset (pds)
GFpds	*pds;
/*
 * Return the forecast time (offset), in seconds, from the given PDS.
 */
{
	int multiplier = grb_TimeUnits (pds);
/*
 * XXX: If the generating center is NCAR and the process is MM5, then
 * interpret the "forecast" times as valid times, and all offsets are zero.
 */
	if (pds->center_id == 60 && pds->process_id == 95)
	{
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
		msg_ELog (EF_PROBLEM, 
			  "grb_Offset: Can't handle time range ID %d!",
			  pds->range_id);
		return (0);
	}
}



bool
grb_UsableLevel (pds, sfc_only)
GFpds	*pds;
bool	sfc_only;
/*
 * Return whether this GRIB grid is "usable".  If sfc_only is true, only
 * surface data are usable.  Otherwise, the data must have an associated single
 * vertical level (i.e., a specific isobaric level or height, as opposed to
 * earth surface, cloud base, tropopause, etc.) to be considered usable.
*/
{
	return (grb_Level (pds, sfc_only, NULL, NULL));
}




bool
grb_NormalLevel (pds)
GFpds	*pds;
/*
 * Return TRUE if the level of this GRIB grid is "normal" (i.e., a specific
 * isobaric level or height, rather than earth surface, cloud base, tropopause,
 * etc.)
 */
{
	return (grb_Level (pds, 0, NULL, NULL));
}



float
grb_ZLevel (pds, units)
GFpds	*pds;
AltUnitType	*units;
/*
 * Return the vertical level from the given PDS.  If 'units' is non-NULL,
 * return the units type.
 */
{
	float alt;

	if (! grb_Level (pds, -1, units, &alt))
	{
		msg_ELog (EF_PROBLEM, "Can't deal with GRIB level type %d!",
			  pds->level_id);
		alt = -1.0;
	}
	return (alt);
}



static int
grb_Level (pds, sfc_only, units, altitude)
GFpds	*pds;
int 	sfc_only;
AltUnitType *units;
float	*altitude;
/*
 * Return non-zero if this level can be interpreted according to the
 * value of sfc_only.  Return values in units and/or altitude if non-zero.
 * Units and altitude are unchanged if interpretation fails.
 * The idea is to limit switch statements on the level_id to this routine.
 */
{
	AltUnitType au;
	float alt;
	int sfc = 0;

	alt = (float) grb_TwoByteInt (&(pds->level_val));
	switch (pds->level_id)
	{
	    case 1:	/* Surface data (0 meters AGL) */
		au = AU_mAGL;
		sfc = 1;
		break;
	    case 100:	/* isobaric level (millibars) */
		au = AU_mb;
		break;
	    case 102:	/* 0 meters MSL */
		au = AU_mMSL;
		sfc = 1;
		break;
	    case 103:	/* fixed height (meters MSL) */
		au = AU_mMSL;
		break;
	    case 107:	/* sigma levels */
		au = AU_sigma;
		alt = alt / 10000.0;
		break;
	    case 109:	/* 'hybrid levels' (how ambiguous can you get?) */
		au = AU_level;
		break;
	   default:
		return (0);
		break;
	}
	/*
	 * If they only wanted surface levels, make sure that's what we found.
	 * -1 means ignore whether the level id is a surface or not.
	 */
	if ((sfc_only >= 0) && ((sfc_only && !sfc) || (sfc && !sfc_only)))
		return (0);
	if (altitude)
		*altitude = alt;
	if (units)
		*units = au;
	return (1);
}


