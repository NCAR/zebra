/*
 * Mudras access utilities.  Major pain in the ass.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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
# include <unistd.h>
# include <fcntl.h>
# include <defs.h>
# include "mudutil.h"

MAKE_RCSID ("$Id: mudutil.c,v 1.1 1991-12-19 22:31:09 kris Exp $");


/*
 * Field info struct.
 */
typedef struct _FldInfo
{
	char	fi_Name[8];	/* Name of the field	*/
	short	fi_Scale;	/* Scale factor		*/
} FldInfo;

/*
 * The mudras goddam header looks like this:
 */
typedef struct _mudras_goddam_header
{
	char	mgh_Id[20];		/* 1 Ident information		*/
	short	mgh_Fill1[22];		/* 11 fill */
	
	short	mgh_OrLatDeg;		/* 33 Latitude part 1		*/
	short	mgh_OrLatMin;		/* 34 Latitude part 2		*/
	short	mgh_OrLatSec;		/* 35 Latitude part 3 (scaled)	*/
	short	mgh_OrLonDeg;		/* 36 Longitude part 1		*/
	short	mgh_OrLonMin;		/* 36 Longitude part 2		*/
	short	mgh_OrLonSec;		/* 38 Longitude part 3 (scaled)	*/
	
	short	mgh_Fill39[23];
	char	mgh_Machine[2];		/* 62 2 char creating machine	*/
	short	mgh_Fill63[2];
	short	mgh_BlkSize;		/* 65 blocking	(3200)		*/
	short	mgh_Fill66;
	short	mgh_BadVal;		/* Bad value flag		*/
	short	mgh_Scale;		/* 68 Scale factor		*/
	
	short	mgh_Fill69[27];

	short	mgh_BlkPerRec;		/* 96 Blocks per record		*/
	short	mgh_Fill97[9];

	short	mgh_NLevel;		/* 106 Number of levels (nZ?)	*/
	short	mgh_Fill107[9];

	short	mgh_Year;		/* 116 Year			*/
	short	mgh_Month;		/* 117 Month			*/
	short	mgh_Day;		/* 118 Day			*/
	short	mgh_Hour;		/* 119 Hour			*/

	short	mgh_Minute;		/* 120 Minute			*/
	short	mgh_Second;		/* 121 Second			*/
	short	mgh_Year1;		/* 122 Year			*/
	short	mgh_Month1;		/* 123 Month			*/
	short	mgh_Day1;		/* 124 Day			*/
	short	mgh_Hour1;		/* 125 Hour			*/
	short	mgh_Minute1;		/* 126 Minute			*/
	short	mgh_Second1;		/* 127 Second			*/

	short	mgh_Fill128[32];

	short	mgh_XMin;		/* 160 Minimum X position	*/
	short	mgh_XMax;		/* 161 Max X position		*/
	short	mgh_nX;			/* 162 X resolution		*/
	short	mgh_XSpacing;		/* 163 X spacing		*/
	short	mgh_Fill164;
	short	mgh_YMin;		/* 165 Min y			*/
	short	mgh_YMax;		/* 166 Max   Y			*/
	short   mgh_nY;			/* 167 Y resolution		*/
	short	mgh_YSpacing;		/* 168 Y spacing		*/
	short	mgh_Fill169;
	
	short	mgh_ZMin;		/* 170 Min z			*/
	short	mgh_ZMax;		/* 171 Max Z			*/
	short	mgh_nZ;			/* 172 Z resolution		*/
	short	mgh_ZSpacing;		/* 173 Z spacing		*/
	short	mgh_Fill174;
	short	mgh_NField;		/* 175 number of fields		*/
	FldInfo mgh_Fields[25];		/* 176-300 Field information	*/
	
	short	mgh_WPerPlane;		/* 301 Words per plane		*/
	short	mgh_NLand1;		/* 302 number of landmarks (1)	*/
	short	mgh_NLand2;		/* 303 number of landmarks (2)	*/
	
	short	mgh_Fill[207];		/* Fill out the struct		*/
} MudHdr;




MudHdr Hdr;
int Fid;


MudOpen (file, t, origin, xi, yi, zi, nfld)
char *file;
time *t;
Location *origin;
CoordInfo *xi, *yi, *zi;
int *nfld;
/*
 * Open this file.
 */
{
	float scale;
/*
 * Open the file.
 */
	if ((Fid = open (file, O_RDONLY)) < 0)
	{
		perror (file);
		return (0);
	}
/*
 * Pull in the header.
 */
# ifdef notdef
	if (sizeof (Hdr) != 1020)
		printf ("Shit!  Header size is %d\n", sizeof (Hdr));
# endif
	if (read (Fid, &Hdr, sizeof (Hdr)) != sizeof (Hdr))
	{
		printf ("Mudras header read error\n");
		close (Fid);
		return (0);
	}
/*
 * Return the begin time of the volume.
 */
	t->ds_yymmdd = Hdr.mgh_Year*10000 + Hdr.mgh_Month*100 + Hdr.mgh_Day;
	t->ds_hhmmss = Hdr.mgh_Hour*10000 + Hdr.mgh_Minute*100 +Hdr.mgh_Second;
/*
 * Return the origin location.
 */
	origin->l_lat = Hdr.mgh_OrLatDeg + Hdr.mgh_OrLatMin/60.0 + 
			Hdr.mgh_OrLatSec/((float) Hdr.mgh_Scale*3600.0);
	origin->l_lon = Hdr.mgh_OrLonDeg + Hdr.mgh_OrLonMin/60.0 + 
			Hdr.mgh_OrLonSec/((float) Hdr.mgh_Scale*3600.0);
	/* What about alt? */
	origin->l_alt = Hdr.mgh_ZMin/(10.0*Hdr.mgh_Scale);
/*
 * Coordinate information.
 */
	scale = (float) Hdr.mgh_Scale;
	xi->ci_MinVal = Hdr.mgh_XMin/scale;
	xi->ci_MaxVal = Hdr.mgh_XMax/scale;
	xi->ci_Spacing = Hdr.mgh_XSpacing/1000.0;
	xi->ci_NStep = Hdr.mgh_nX;
	yi->ci_MinVal = Hdr.mgh_YMin/scale;
	yi->ci_MaxVal = Hdr.mgh_YMax/scale;
	yi->ci_Spacing = Hdr.mgh_YSpacing/1000.0;
	yi->ci_NStep = Hdr.mgh_nY;
	zi->ci_MinVal = Hdr.mgh_ZMin/(10.0*scale);
	zi->ci_MaxVal = Hdr.mgh_ZMax/(10.0*scale);
	zi->ci_Spacing = Hdr.mgh_ZSpacing/1000.0;
	zi->ci_NStep = Hdr.mgh_nZ;
/*
 * Field info.
 */
	*nfld = Hdr.mgh_NField;
	return (1);
}





char *
MudField (i)
int i;
/*
 * Return the name of the ith field.
 */
{
	static char Fname[9];
	int c;

	if (i < 0 || i >= Hdr.mgh_NField)
		return ("bullshit");
	for (c = 0; c < 8; c++)
		if ((Fname[c] = Hdr.mgh_Fields[i].fi_Name[c]) == ' ')
			break;
	Fname[c] = '\0';
	return (Fname);
}





int
FetchGrid (field, level, dest, badflag)
int field, level;
float *dest, badflag;
/*
 * Pull a grid out of this file.
 */
{
	static short *tmp = 0;
	static int ntmp = 0;
	short *tptr;
	int len = Hdr.mgh_nX*Hdr.mgh_nY, begin, plen;
	int blkperlev = (len*Hdr.mgh_NField + Hdr.mgh_BlkSize - 1)/Hdr.mgh_BlkSize;
	const short ibad = Hdr.mgh_BadVal;
	float scale = 1.0/Hdr.mgh_Fields[field].fi_Scale;
/*
 * Figure out how much temp space we need, and get if if necessary.
 */
	if (len > ntmp)
	{
		if (ntmp > 0)
			free (tmp);
		tmp = (short *) malloc (len * sizeof (short));
		ntmp = len;
	}
/*
 * Figure out where this field begins, and read it in.
 */
# ifdef notdef
	begin = 1 + (level*Hdr.mgh_NField + field)*Hdr.mgh_BlkPerRec;
	lseek (Fid, begin*Hdr.mgh_BlkSize*sizeof (short), SEEK_SET);
	begin = 1 + level*blkperlev;
	lseek (Fid, (begin*Hdr.mgh_BlkSize + field*len)*sizeof (short),
			SEEK_SET);
	/* Following works with cinnndy's file, but not others */
	begin = 524 + (len*Hdr.mgh_NField + Hdr.mgh_NField - 1)*level
			+ field*len;
# endif
/*
 * PLEN = padded length to calculate how much space is really occupied by
 * 	  a grid -- they pad it out to multiples of 4.
 * 
 * Calculate the beginning of this grid.  The components of this ugly thing
 * are:
 *	512	The size of the header, plus a couple of words they throw
 *		in for the hell of it.
 *	(level+1)*12 	There is a header at the beginning of each level.
 *	plen*nfld*level	The data contained in the previous levels.
 *	plen*field	The data in the previous fields.
 */
	plen = (len & 0x3) ? (len & ~0x3) + 4 : len;
	begin = 512 + (level+1)*12 + plen*(Hdr.mgh_NField*level + field);
/*
 * Move to the right place and pull out the data.
 */
	lseek (Fid, begin*sizeof (short), SEEK_SET);
	if (read (Fid, tmp, len*sizeof (short)) != len*sizeof (short))
	{
		printf ("Error reading field %d, lvl %d at rec %d\n", field,
			level, begin);
		return (0);
	}
/*
 * Convert it over to floating point.
 */
	for (tptr = tmp; len > 0; len--)
	{
		*dest++ = (*tptr == ibad) ? badflag : *tptr*scale;
		tptr++;
	}
	return (1);
}




# ifdef _SABER_

void DumpFloat (begin, nwd, scale)
int begin, nwd;
float scale;
{
	short *tmp = (short *) malloc (nwd * sizeof (short));
	int i;

	lseek (Fid, begin*sizeof (short), SEEK_SET);
	read (Fid, tmp, nwd*sizeof (short));
	for (i = 0; i < nwd; i++)
		printf ("%5d: 0x%04x  %6d  %.2f\n", i + begin, tmp[i], tmp[i],
			tmp[i]/scale);
}

# endif
