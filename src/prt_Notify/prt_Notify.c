/*
 * Deal with the problem of notifications in pseudo-real time.
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

static char *rcsid = "$ID$";

#include <copyright.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include "DataStore.h"
# include "dsPrivate.h"


static int IMessage FP ((struct message *));
static void NotificationRequest FP ((struct dsp_Template *));
static void MakeTimerRequest FP ((PlatformId));
static void TimeToNotify FP ((ZebTime *, PlatformId));

/* kludge to keep acc compiler happy - these vars are not used */
PlatformClass    *CTable;
PlatformInstance *PTable;

/*
 * Keep track of timer events.
 */
static int TimeEvent[CFG_MAX_PLATFORMS];


main ()
{
	int i;
/*
 * Initialize and hook in to the daemon for copies.
 */
	msg_connect (IMessage, "Notifier");
	msg_join (MSG_CLIENT_EVENTS);
	usy_init ();
	ds_Initialize ();
	dap_Init ();
	ds_SnarfCopies (NotificationRequest);
/*
 * Time events too.
 */
	for (i = 0; i < CFG_MAX_PLATFORMS; i++)
		TimeEvent[i] = -1;
/*
 * Now we just wait for something to happen.
 */
	msg_await ();
}





static int
IMessage (msg)
struct message *msg;
/*
 * Something has happened.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
	struct mh_client *client;
/*
 * About the only thing we expect to see here is MH stuff.
 */
	if (msg->m_proto != MT_MESSAGE)
	{
		msg_ELog (EF_PROBLEM, "Funky msg proto %d", msg->m_proto);
		return (0);
	}
/*
 * See what MH wants.
 */
	switch (tm->mh_type)
	{
	/*
	 * Shutdown time?
	 */
	   case MH_SHUTDOWN:
		exit (0);
	/*
	 * If it's a client event, and somebody died, we clean up their
	 * notification requests.
	 */
	   case MH_CLIENT:
		client = (struct mh_client *) msg->m_data;
		if (client->mh_evtype == MH_CE_DISCONNECT)
			dap_Cancel (client->mh_client);
		break;
	/*
	 * Otherwise we're confused.
	 */
	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown MH msg type %d", tm->mh_type);
		break;
	}
	return (0);
}






static void
NotificationRequest (dmsg)
struct dsp_Template *dmsg;
/*
 * We just got a copy of a notification request.
 */
{
	struct dsp_NotifyRequest nrq;
	char *from;

	nrq = *(struct dsp_NotifyRequest *) dmsg;
/*
 * If this is a notification request, we first see if there are already
 * requests outstanding for this PID -- if so, there will also be a timer
 * request out there.  Otherwise we have to make one.
 */
	if (dmsg->dsp_type == dpt_NotifyRequest)
	{
		if (! dap_IsInterest (nrq.dsp_pid))
			MakeTimerRequest (nrq.dsp_pid);
		from = sizeof (nrq) + (char *) dmsg;
		dap_Request (from, &nrq);
		msg_ELog (EF_DEBUG, "Notif request for %s from %s",
			ds_PlatformName (nrq.dsp_pid), from);
	}
/*
 * For cancels, just clean it out of the notification data structure.
 */
	else if (dmsg->dsp_type == dpt_CancelNotify)
	{
		from = sizeof (*dmsg) + (char *) dmsg;
		dap_Cancel (from, dmsg);
	}
}





static void
MakeTimerRequest (pid)
PlatformId pid;
/*
 * Get the timer to tell us the next time there will be data "available"
 * from this platform.
 */
{
	ZebTime t[2], now;
	char *pname = ds_PlatformName (pid);
	int ntime;
/*
 * If there is already a timer event on this PID, we assume we don't need
 * to make another one.
 */
	if (TimeEvent[pid] >= 0)
		return;
/*
 * Find out what our time is now, and when the next data shows up.
 * If the most recent data time is now, then we've nothing to wait for.
 */
	tl_Time (&now);
	ntime = ds_DataTimes (pid, &now, 2, DsAfter, t);
	if ((ntime == 0) || TC_Eq(t[0], now))
		return;
/*
 * Set up our request for that time.
 */
	TimeEvent[pid] = tl_AbsoluteReq (TimeToNotify, (void *) pid, &t[0], 0);
}





static void
TimeToNotify (t, pid)
ZebTime *t;
PlatformId pid;
/*
 * It's time to send a notification on this platform.
 */
{
/*
 * If somebody is still interested, we do the notification and schedule
 * the next one.
 */
	t->zt_MicroSec = 0;	/* xxx */
	TimeEvent[pid] = -1;		/* Event is gone	*/
	if (dap_IsInterest (pid))
	{
		dap_Notify (pid, t, 1, 0, TRUE);
		MakeTimerRequest (pid);
	}
}
