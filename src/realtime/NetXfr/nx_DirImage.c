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
# include <string.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include <ImageXfr.h>
# include "NetXfr.h"

RCSID("$Id: nx_DirImage.c,v 3.8 1996-01-10 19:06:41 granger Exp $")

/*
 * We're only set up to deal with one image source for now.  Should
 * be enough.
 */
/* static DataObject OutData; */
struct _ix_desc *Desc;
static int XRes, YRes, NField;
static char *Fields[20];
static FieldId Fids[20];
static PlatformId Plat;


static void TrimXY FP ((const unsigned char * , int *, int *, int));
static int TrimImage FP ((unsigned char *image, int *min, int *max));
static void AdjoinImage FP ((unsigned char *, int, int, int, int));

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
	if ((Plat = ds_LookupPlatform (plat)) == BadPlatform)
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
	for (i = 0; i < NField; i++)
	{
		msg_ELog (EF_INFO, "Field %s", Fields[i]);
		Fids[i] = F_Lookup (Fields[i]);
	}
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
	UItime t;
	ZebTime zt;
	ScaleInfo *scale = 0;
	unsigned char **images = 0;
	char *attr;
	int xmin, ymin, xmax, ymax, i, offset;
	float junk;
	DataChunk *dc;
/*
 * Initialize the first time around
 */
	if (! scale)
	{
		scale = (ScaleInfo *) malloc (NField * sizeof (ScaleInfo));
		images = (unsigned char **) malloc (NField * sizeof (char *));
	}
/*
 * Grab it.
 */
	if (! IX_GetReadFrame (Desc, set, (char **)images, &t, &rg, &loc, 
			       scale, &xmin, &ymin, &xmax, &ymax, &attr))
	{
		msg_ELog (EF_PROBLEM, "Can't get promised set %d", set);
		return;
	}
/*
 * Trim out blank data.
 */
	offset = TrimImage (images[0], &ymin, &ymax);
	TrimXY (images[0] + offset, &xmin, &xmax, ymax - ymin + 1);
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
 * Create a data chunk to hold this thing.
 */
	dc = dc_CreateDC (DCC_Image);
	dc->dc_Platform = Plat;
	dc_ImgSetup (dc, NField, Fids, scale);
	dc_SetGlobalAttr (dc, "radar_space", "true");
	dc_SetGlobalAttr (dc, "scan_type", attr + 6);  /* XXXXXXXXXXX */
/*
 * Add the images to it.
 */
	msg_ELog (EF_INFO, "Set %d, [%d, %d] to [%d, %d] el %.2f attr '%s'",
			set, xmin, ymin, xmax, ymax, loc.l_alt, attr);
	TC_UIToZt (&t, &zt);
	for (i = 0; i < NField; i++)
	{
		AdjoinImage (images[i], xmin, ymin, xmax, ymax);
		dc_ImgAddImage (dc, 0, Fids[i], &loc, &rg, &zt, images[i], 0);
	}
/*
 * Now we can give back the image frames.
 */
	for (i = 0; i < NField; i++)
		memset (images[i] + ymin*XRes, 0xff, (ymax - ymin + 1)*XRes);
	IX_ReleaseFrame (Desc, set);
/*
 * Send it, but only if there's something real.
 */
	if (ymin < ymax && xmin < xmax)
		SendDChunk (dc, (strstr (attr,"newfile") != NULL));
	else
		msg_ELog (EF_INFO, "Dropping empty image");
	dc_DestroyDC (dc);
}




static int
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
	if ((rem = ((xmx - xmn + 1) % 4)))
		if ((xmn -= (4 - rem)) < 0)
		{
			xmx -= xmn;
			xmn = 0;
		}
	*xmin = xmn;
	*xmax = xmx;
}







static void
AdjoinImage (image, xmin, ymin, xmax, ymax)
unsigned char *image;
int xmin, ymin, xmax, ymax;
/*
 * Reunite this image.
 */
{
	int nx = (xmax - xmin + 1);
	int x, y;
	const int incr = XRes - nx;
	const unsigned char *cp;
	unsigned char *mp = image;
/*
 * We need to go through each raster line and trim off the part we want.
 */
	cp = image + ymin*XRes + xmin;
	for (y = ymin; y <= ymax; y++)
	{
		for (x = xmin; x <= xmax; x++)
			*mp++ = *cp++;
		cp += incr;
	}
}
