/*
 * Consume images from a shared memory image segment into the data store.
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

static char *rcsid = "$Id: ds_consumer.c,v 2.7 1995-09-20 20:45:42 burghart Exp $";

# include <copyright.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ImageXfr.h>

/*
 * How many fields can we consume?
 */
# define MFIELD 8

/*
 * Globals.
 */
int 	XRes, YRes, NField;
char	*Fields[MFIELD];
FieldId	Fids[MFIELD];
PlatformId 	Plat;
struct _ix_desc	*ShmDesc;


static int MDispatcher FP ((struct message *));



main (argc, argv)
int argc;
char **argv;
{
	int i;
	char	ourname[20];

	usy_init ();

	sprintf (ourname, "Consumer_%x", getpid ());
	msg_connect (MDispatcher, ourname);
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
	if (! (ShmDesc = IX_HookIn (0x910425, ourname, &XRes, &YRes,
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
 * Field list
 */
	for (i = 0; i < NField; i++)
		Fids[i] = F_Lookup (Fields[i]);
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
			msg_ELog (EF_INFO, 
				  "Exiting on request from message system.");
			exit (1);
		}
		break;
	    case MT_FINISH:
		IX_Detach (ShmDesc);
		msg_ELog (EF_INFO, "Exiting on request from '%s'.",
			  msg->m_from);
		exit (*(int *)(msg->m_data));
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
	UItime t;
	ZebTime zt;
	ScaleInfo scale[MFIELD];
	char *images[MFIELD], *attr_string;
	int xmin, ymin, xmax, ymax, i, offset;
	static float PrevAlt = -99;
	DataChunk *dc;
/*
 * Grab it.
 */
	msg_ELog (EF_DEBUG, "Grabbing frame...");
	if (! IX_GetReadFrame (ShmDesc, set, images, &t, &rg, &loc, scale,
			&xmin, &ymin, &xmax, &ymax, &attr_string))
	{
		msg_ELog (EF_PROBLEM, "Can't get promised set %d", set);
		return;
	}

	TC_UIToZt (&t, &zt);
/*
 * Trim out blank data.
 */
	offset = TrimImage (images[0], &ymin, &ymax);
	cvt_Origin (loc.l_lat, loc.l_lon);
	cvt_ToLatLon (0.0, (YRes - ymax)*rg.rg_Yspacing, &loc.l_lat,
			&loc.l_lon);
	rg.rg_nY = ymax - ymin + 1;

	msg_ELog (EF_DEBUG, "Set %d, [%d, %d] to [%d, %d]", set, xmin, ymin,
				xmax, ymax);
/*
 * Fill in the rest of our data chunk.
 */
	dc = dc_CreateDC (DCC_Image);
	dc->dc_Platform = Plat;
	dc_ImgSetup (dc, NField, Fids, scale);
	for (i = 0; i < NField; i++)
		dc_ImgAddImage (dc, 0, Fids[i], &loc, &rg, &zt,
				images[i] + offset, 0);
/*
 * Add our attributes
 */
	dc_SetGlobalAttr (dc, attr_string, "");
/*
 * Send it, but only if there's something real.
 */
	if (ymin < ymax)
		ds_Store (dc, ! strncmp (attr_string, "newfile", 7), NULL, 0);
	else
		msg_ELog (EF_INFO, "Dropping empty image");

	dc_DestroyDC (dc);
	PrevAlt = loc.l_alt;
/*
 * Clear and return the frames.
 */
	for (i = 0; i < NField; i++)
		memset (images[i] + ymin*XRes, 0xff, (ymax - ymin + 1)*XRes);

	msg_ELog (EF_DEBUG, "Release set %d", set);
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
