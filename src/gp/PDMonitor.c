/*
 * The implementation of the PD monitor facility.
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
# include <string.h>

# include <defs.h>
# include <message.h>
# include <pd.h>
# include <pdmon.h>
# include <dm.h>
# include "GraphProc.h"
# include "EventQueue.h"



MAKE_RCSID ("$Id: PDMonitor.c,v 1.6 1999-03-01 02:04:27 burghart Exp $")


/*
 * The info describing monitoring.  For now we only implement a single
 * monitoring process.
 */
static int MonitorActive = FALSE;	/* Nobody watching	*/
static char MonitorProc[MSG_MAXNAMELEN];/* Who is watching	*/
static int Suppress = FALSE;		/* Suppress one update	*/



/*
 * Forwards.
 */
static int pdm_Message FP ((Message *));
static void pdm_MonUpdate FP ((void));



void
pdm_Init ()
/*
 * Get set up to handle PD monitoring if need be.
 */
{
	msg_AddProtoHandler (MT_PDMON, pdm_Message);
}



static int
pdm_Message (msg)
Message *msg;
/*
 * Deal with an incoming message.
 */
{
	pdmTemplate *pt = (pdmTemplate *) msg->m_data;
	struct dm_pdchange *dpd;
	pdmPD *ppd;
/*
 * See what they want.
 */
	switch (pt->pt_Type)
	{
	/*
	 * Somebody hooking in.
	 */
	    case pdm_HookIn:
	    	if (MonitorActive)
			msg_ELog (EF_PROBLEM, "Overriding monitor %s",
				MonitorProc);
		MonitorActive = TRUE;
		strcpy (MonitorProc, msg->m_from);
		Suppress = FALSE;
		pdm_ScheduleUpdate ();
		msg_ELog (EF_INFO, "Monitor hookin by %s", msg->m_from);
		break;
	/*
	 * Hooking back out.
	 */
	    case pdm_UnHook:
	    	if (! MonitorActive)
			msg_ELog (EF_PROBLEM, "UnHook with no monitor");
		else if (strcmp (MonitorProc, msg->m_from))
			msg_ELog (EF_PROBLEM, "UnHook from %s, but mon is %s",
				msg->m_from, MonitorProc);
		else
		{
			msg_ELog (EF_INFO, "%s monitor unhooking",MonitorProc);
			MonitorActive = FALSE;
		}
		break;
	/*
	 * They have a new PD for us.  Fake things up so that this looks like
	 * it came from DM. (But make sure DM gets updated too).
	 */
	    case pdm_NewPD:
		ppd = (pdmPD *) pt;
		dpd = (struct dm_pdchange *)
			malloc (sizeof (struct dm_pdchange) + ppd->pt_Len - 1);
		dpd->dmm_type = DM_PDCHANGE;
		dpd->dmm_pdlen = ppd->pt_Len;
		memcpy (dpd->dmm_pdesc, ppd->pt_Pd, ppd->pt_Len);
		ChangePD (dpd);
		free (dpd);
		Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
		Suppress = TRUE;
		break;
	/*
	 * Oops.
	 */
	    default:
	    	msg_ELog (EF_PROBLEM, "Unknown pdmon code %d", pt->pt_Type);
		break;
	}
	return (0);
}





void
pdm_ScheduleUpdate ()
/*
 * Arrange for an updated copy of the plot description to be shipped
 * out to the monitor process.
 */
{
	if (MonitorActive)
		Eq_AddEvent (PWhenever, pdm_MonUpdate, 0, 0, Bounce);
}





static void
pdm_MonUpdate ()
/*
 * Ship out the PD to the monitor process.
 */
{
	pdmPD *ppd;
	raw_plot_description *rpd = pd_Unload (Pd);
	int len = sizeof (pdmPD) + rpd->rp_len;
/*
 * Make sure somebody still wants this.  It could have changed since
 * the update was scheduled.
 */
	if (! MonitorActive)
		return;
	if (Suppress)
	{
		Suppress = FALSE;
		pd_RPDRelease (rpd);
		return;
	}
/*
 * Allocate and fill in the update structure.
 */
	ppd = (pdmPD *) malloc (len);
	ppd->pt_Type = pdm_MyPD;
	ppd->pt_Len = rpd->rp_len;
	memcpy (ppd->pt_Pd, rpd->rp_data, rpd->rp_len);
/*
 * Ship it out, clean up, and we are done.
 */
	msg_send (MonitorProc, MT_PDMON, FALSE, ppd, len);
	free (ppd);
	pd_RPDRelease (rpd);
}



void
pdm_Finish ()
/*
 * We're quitting.
 */
{
	pdmTemplate pt;

	if (MonitorActive)
	{
		pt.pt_Type = pdm_Exit;
		msg_send (MonitorProc, MT_PDMON, FALSE, &pt, sizeof (pt));
	}
}
