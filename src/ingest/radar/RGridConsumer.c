/*
 * Consume RGrids from memory shared with SPRINT into the data store.
 */
/*		Copyright (C) 1995 by UCAR
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

static char *rcsid = "$Id: RGridConsumer.c,v 2.1 1995-08-10 21:16:40 burghart Exp $";

# include <copyright.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ImageXfr.h>



static int MDispatcher FP ((struct message *));

int	Gridsize, NField;
char	*Fields[MAXFIELD];
FieldId	Fids[MAXFIELD];
PlatformId 	Plat;
struct _ix_desc	*ShmDesc;



main (argc, argv)
int argc;
char **argv;
{
	int i, dummy;
	char	ourname[30];

	usy_init ();

	sprintf (ourname, "RGridConsumer_%x", getpid ());
	msg_connect (MDispatcher, ourname);
/*
 * Checking.
 */
	if (argc != 2)
	{
		msg_ELog (EF_PROBLEM, "RGridConsumer miscalled");
		exit (1);
	}
/*
 * Hook into our segment.
 */
	if (! (ShmDesc = IX_HookIn (0x950630, ourname, &Gridsize, &dummy,
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
	   	DoGrid (tmpl->mh_type);
		break;
	}
	return (0);
}   	





DoGrid (whichset)
int whichset;
/*
 * Do the given grid set
 */
{
	RGrid rg;
	Location origin;
	UItime t;
	ZebTime zt;
	ScaleInfo sdummy[MAXFIELD];
	int i, idummy, gridsize;
	char *dptrs[4], *attr;
	float *data;
	DataChunk *dc;
/*
 * Grab it.
 */
	msg_ELog (EF_DEBUG, "Grabbing grid set %d for storage...", whichset);
	if (! IX_GetReadFrame (ShmDesc, whichset, dptrs, &t, &rg, &origin, 
			       sdummy, &idummy, &idummy, &idummy, &idummy, 
			       &attr))
	{
		msg_ELog (EF_PROBLEM, "Can't get promised grid set %d", 
			  whichset);
		return;
	}

	TC_UIToZt (&t, &zt);
	data = (float *) dptrs[0];
	gridsize = rg.rg_nX * rg.rg_nY * rg.rg_nZ;
/*
 * Fill in the rest of our data chunk.
 */
	dc = dc_CreateDC (DCC_RGrid);
	dc->dc_Platform = Plat;
	dc_RGSetup (dc, NField, Fids);

	for (i = 0; i < NField; i++)
		dc_RGAddGrid(dc, 0, Fids[i], &origin, &rg, &zt, 
			     data + i * gridsize, 0);

	ds_Store (dc, TRUE, NULL, 0);

	dc_DestroyDC (dc);

	msg_ELog (EF_DEBUG, "Release set %d", whichset);
	IX_ReleaseFrame (ShmDesc, whichset);
}
