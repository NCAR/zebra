/*
 * Consume images into the data store.
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

static char *rcsid = "$Id: ds_consumer.c,v 2.1 1991-09-16 22:19:36 burghart Exp $";

# include <copyright.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ImageXfr.h>



/*
 * Globals.
 */
static int XRes, YRes, NField;
static char *Fields[20];
struct _ix_desc *ShmDesc;

PlatformId Plat;

DataObject OutData;

# ifdef __STDC__
	static int MDispatcher (struct message *);
# else
# endif


main (argc, argv)
int argc;
char **argv;
{
	usy_init ();
	msg_connect (MDispatcher, "DS Consumer");
/*
 * Checking.
 */
	if (argc != 2)
	{
		msg_ELog (EF_PROBLEM, "ds_Consumer miscalled");
		exit (1);
	}
/*
 * Hook into our segment.
 */
	if (! (ShmDesc = IX_HookIn (0x910425, "DS Consumer", &XRes, &YRes,
				&NField, Fields)))
	{
		msg_ELog (EF_EMERGENCY, "NO SHM segment");
		exit (1);
	}
/*
 * DS initialization.
 */
	ds_Initialize ();
	if ((Plat = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		msg_ELog (EF_EMERGENCY, "Bad platform %s", argv[1]);
		exit (1);
	}
/*
 * Static data object initialization.
 */
	OutData.do_id = Plat;
	OutData.do_org = OrgImage;
	OutData.do_npoint = 1;
	OutData.do_aloc = ALLOC (Location);
	/* XXXXXXXXX */
	OutData.do_nfield = 2;
	OutData.do_fields[0] = "reflectivity";
	OutData.do_fields[1] = "velocity";
	/* XXXXXXXXX */
	OutData.do_flags = 0;
/*
 * Wait for something.
 */
	msg_await ();
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
		{
			IX_Detach (ShmDesc);
			exit (1);
		}
		break;

	   case MT_IMAGEXFR:
	   	DoImage (tmpl->mh_type);
		break;
	}
	return (0);
}   	





DoImage (set)
int set;
/*
 * Do this image set.
 */
{
	RGrid rg;
	Location loc;
	time t;
	ScaleInfo scale[2];
	char *images[4], attr;
	int xmin, ymin, xmax, ymax, i, offset;
	static float PrevAlt = -99;
/*
 * Grab it.
 */
	if (! IX_GetReadFrame (ShmDesc, set, images, &t, &rg, &loc, scale,
			&xmin, &ymin, &xmax, &ymax, &attr))
	{
		msg_ELog (EF_PROBLEM, "Can't get promised set %d", set);
		return;
	}
/*
 * Trim out blank data.
 */
	offset = TrimImage (images[0], &ymin, &ymax);
	cvt_Origin (loc.l_lat, loc.l_lon);
	cvt_ToLatLon (0.0, (YRes - ymax)*rg.rg_Yspacing, &loc.l_lat,
			&loc.l_lon);
	rg.rg_nY = ymax - ymin + 1;
/*
 * Fill in the rest of our data object.
 */
	msg_ELog (EF_INFO, "Set %d, [%d, %d] to [%d, %d]", set, xmin, ymin,
				xmax, ymax);
	OutData.do_begin = OutData.do_end = t;
	OutData.do_times = &t;
	OutData.do_desc.d_img.ri_rg = &rg;
	OutData.do_desc.d_img.ri_scale = scale;
	OutData.do_aloc = &loc;
	for (i = 0; i < OutData.do_nfield; i++)
		OutData.do_data[i] = (float *) (images[i] + offset);
/*
 * Send it, but only if there's something real.
 */
	if (ymin < ymax)
		ds_PutData (&OutData, PrevAlt > loc.l_alt);
	else
		msg_ELog (EF_INFO, "Dropping empty image");
	PrevAlt = loc.l_alt;
/*
 * Clear and return the frames.
 */
	for (i = 0; i < OutData.do_nfield; i++)
		memset (images[i] + ymin*XRes, 0xff, (ymax - ymin + 1)*XRes);
	msg_ELog (EF_INFO, "Release set %d", set);
	IX_ReleaseFrame (ShmDesc, set);
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
