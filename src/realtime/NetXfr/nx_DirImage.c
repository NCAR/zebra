/*
 * Direct image transfer support.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
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
static char *rcsid = "$Id: nx_DirImage.c,v 2.1 1991-09-26 23:10:21 gracio Exp $";


# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include <ImageXfr.h>
# include "NetXfr.h"


/*
 * We're only set up to deal with one image source for now.  Should
 * be enough.
 */
static DataObject OutData;
struct _ix_desc *Desc;
static int XRes, YRes, NField;
static char *Fields[20];


# ifdef __STDC__
	static void TrimXY (const unsigned char * , int *, int *, int);
	static void AdjoinImages (DataObject *, unsigned char **, int,
			int, int, int);
# else
	static void TrimXY ();
	static void AdjoinImages ();
# endif

void
DirImage (plat)
char *plat;
/*
 * Set up to receive direct images.
 */
{
	int i;
/*
 * Look up our platform.
 */
	if ((OutData.do_id = ds_LookupPlatform (plat)) == BadPlatform)
	{
		msg_ELog (EF_EMERGENCY, "Bad DIRIMAGE plat %s", plat);
		return;
	}
/*
 * Try to hook into the SHM segment.
 */
	if (! (Desc = IX_HookIn (0x910425, "NetXfr", &XRes, &YRes,
				&NField, Fields)))
	{
		msg_ELog (EF_EMERGENCY, "NO SHM segment");
		return;
	}
/*
 * Do other static initialization.
 */
	OutData.do_org = OrgImage;
	OutData.do_npoint = 1;
	OutData.do_aloc = ALLOC (Location);
	for (i = 0; i < NField; i++)
	{
		msg_ELog (EF_INFO, "Field %s", Fields[i]);
		OutData.do_fields[i] = Fields[i];
	}
	OutData.do_nfield = NField;
	OutData.do_flags = 0;
}






void
DirImageAvail (set)
int set;
/*
 * Deal with the direct image which is alleged to be available on this set.
 */
{
	RGrid rg;
	Location loc;
	time t;
	ScaleInfo scale[2];
	char *images[4], *attr;
	int xmin, ymin, xmax, ymax, i, offset, fnb;
	static float PrevAlt = -99;
	float junk;
/*
 * Grab it.
 */
	if (! IX_GetReadFrame (Desc, set, images, &t, &rg, &loc, scale,
			&xmin, &ymin, &xmax, &ymax, &attr))
	{
		msg_ELog (EF_PROBLEM, "Can't get promised set %d", set);
		return;
	}
/*
 * Trim out blank data.
 */
	offset = TrimImage (images[0], &ymin, &ymax);
	TrimXY ((unsigned char *) images[0] + offset, &xmin, &xmax,
			ymax - ymin + 1);
/*
 * 7/91 jc	The "moving radar" bug.  Calculate the new longitude at
 *		the old latitude -- that which the corner of the grid
 *		was originally figured at.
 */
	cvt_Origin (loc.l_lat, loc.l_lon);
	cvt_ToLatLon (xmin*rg.rg_Xspacing, 0.0, &loc.l_lat, &loc.l_lon);
	cvt_ToLatLon (xmin*rg.rg_Xspacing, (YRes - ymax)*rg.rg_Yspacing,
			&loc.l_lat, &junk);
	rg.rg_nX = xmax - xmin + 1;
	rg.rg_nY = ymax - ymin + 1;
/*
 * Fill in the rest of our data object.
 */
	msg_ELog (EF_INFO, "Set %d, [%d, %d] to [%d, %d] el %.2f attr '%s'",
			set, xmin, ymin, xmax, ymax, loc.l_alt, attr);
	OutData.do_begin = OutData.do_end = t;
	OutData.do_times = &t;
	OutData.do_desc.d_img.ri_rg = &rg;
	OutData.do_desc.d_img.ri_scale = scale;
	OutData.do_aloc = &loc;
	OutData.do_nbyte = OutData.do_nfield*rg.rg_nX*rg.rg_nY;
	OutData.do_attr = attr;
/*
 * The broadcast code wants the frames to be contiguous, so let's make
 * that happen here.
 */
	if (rg.rg_nX > 0 && rg.rg_nY > 0)
		AdjoinImages (&OutData, (unsigned char **) images, xmin,
				ymin, xmax, ymax);
/*
 * Now we can give back the image frames.
 */
	for (i = 0; i < OutData.do_nfield; i++)
		memset (images[i] + ymin*XRes, 0xff, (ymax - ymin + 1)*XRes);
	IX_ReleaseFrame (Desc, set);
/*
 * Send it, but only if there's something real.
 */
	if (ymin < ymax && xmin < xmax)
		SendDObj (&OutData, PrevAlt > loc.l_alt);
	else
		msg_ELog (EF_INFO, "Dropping empty image");
	PrevAlt = loc.l_alt;
	free (OutData.do_data[0]);
}




int
TrimImage (image, min, max)
unsigned char *image;
int *min, *max;
/*
 * Trim off raster lines which are all badflags.
 */
{
	int i;
	unsigned char *cp = image + *min*XRes;
/*
 * Search forward to the first non-blank char.
 */
	for (i = *min*XRes; i < YRes*XRes; i++)
		if (*cp++ != 0xff)
			break;
	*min = ((cp - 1) - image)/XRes;
/*
 * Now go back from the far end and do the same thing.
 */
	cp = image + *max*XRes + XRes - 1;
	for (i = cp - image; i > 0; i--)
		if (*cp-- != 0xff)
			break;
	*max = ((cp + 1) - image)/XRes;

	return (*min * XRes);
}




static void
TrimXY (image, xmin, xmax, ny)
const unsigned char * image;
int *xmin, *xmax, ny;
/*
 * Shrink the images in on the sides.
 */
{
	int y, xmn, xmx, nx, rem;
	const unsigned char *dp = image;
/*
 * Go through each raster line and have a look.
 */
	xmn = XRes;
	xmx = 0;
	for (y = 0; y < ny; y++)
	{
	/*
	 * See how far the empty stuff goes in on the left.
	 */
		for (nx = *xmin; nx < xmn; nx++)
			if (dp[nx] != 0xff)
				xmn = nx;
	/*
	 * And on the right.
	 */
		for (nx = *xmax; nx > xmx; nx--)
			if (dp[nx] != 0xff)
				xmx = nx;
		dp += XRes;
	}
/*
 * Return the info and we're done.  Enforce a raster line width that is
 * divisible by four, however.
 */
	if (rem = ((xmx - xmn + 1) % 4))
		if ((xmn -= (4 - rem)) < 0)
		{
			xmx -= xmn;
			xmn = 0;
		}
	*xmin = xmn;
	*xmax = xmx;
}







static void
AdjoinImages (dobj, images, xmin, ymin, xmax, ymax)
DataObject *dobj;
unsigned char **images;
int xmin, ymin, xmax, ymax;
/*
 * Put these images together into a single chunk of memory, with
 * the empty part trimmed out.
 */
{
	int nx = (xmax - xmin + 1), ny = (ymax - ymin + 1), x, y, im;
	const int incr = XRes - nx;
	unsigned char *mem = (unsigned char *) malloc (dobj->do_nfield*nx*ny);
	const unsigned char *cp;
	unsigned char *mp = mem;
/*
 * We need to go through each raster line and trim off the part we want.
 */
	for (im = 0; im < dobj->do_nfield; im++)
	{
	/*
	 * Fix up the data object.
	 */
	 	dobj->do_data[im] = (float *) mp;
	/*
	 * Copy over the data.
	 */
		cp = images[im] + ymin*XRes + xmin;
		for (y = ymin; y <= ymax; y++)
		{
			for (x = xmin; x <= xmax; x++)
				*mp++ = *cp++;
			cp += incr;
		}
	}
}
