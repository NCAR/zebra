/*
 * The application notification module.
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
# include "dsPrivate.h"

RCSID("$Id: d_Notify.c,v 3.8 1996-11-19 09:28:22 granger Exp $")

/*
 * Here we take advantage of the knowledge that PlatformID's are simply small
 * integers, and maintain an array of application notification queues.
 * Here is what an entry in one of these queues looks like:
 */
typedef struct s_NRequest
{
	char nr_from[MAX_NAME_LEN];	/* Who wants it		*/
	int nr_param;			/* The param they gave	*/
	struct s_NRequest *nr_next;	/* Next in chain	*/
} NRequest;

/*
 * The actual array of these requests.
 */
static NRequest *Requests[MAXPLAT];

static NRequest *NRFree = 0;	/* Free lookaside list		*/

static char CopyProc[128];	/* Who's getting copies?	*/
static bool Copies = FALSE;



void
dap_Init ()
/*
 * Initialize the notification mechanism.
 */
{
	int i;

	for (i = 0; i < MAXPLAT; i++)
		Requests[i] = (NRequest *) 0;
}



static NRequest *
dap_GetNR ()
/*
 * Get a new request structure.
 */
{
	NRequest *ret;

	if (NRFree)
	{
		ret = NRFree;
		NRFree = NRFree->nr_next;
	}
	else
		ret = ALLOC (NRequest);
	ret->nr_next = 0;
	return (ret);
}



static inline void
dap_FreeNR (nr)
NRequest *nr;
/*
 * Done with this one.
 */
{
	nr->nr_next = NRFree;
	NRFree = nr;
}



void
dap_Request (from, req)
char *from;
struct dsp_NotifyRequest *req;
/*
 * Deal with an incoming application notification request.
 */
{
	NRequest *nr = dap_GetNR ();
/*
 * Fix up our data structures.
 */
	strcpy (nr->nr_from, from);
	nr->nr_param = req->dsp_param;
	nr->nr_next = Requests[req->dsp_pid];
	Requests[req->dsp_pid] = nr;
/*
 * If somebody is snarfing copies, we send it on.
 */
	if (Copies)
	{
		strcpy (((char *) req) + sizeof (*req), from);
		msg_send (CopyProc, MT_DATASTORE, FALSE, req,
			  sizeof (*req) + strlen (from) + 1);
	}
}




void
dap_Cancel (proc)
char *proc;
/*
 * Cancel all requests by this proc.
 */
{
	int plat;
	NRequest *zap, *last;

	for (plat = 0; plat < MAXPLAT; plat++)
	{
		if (! Requests[plat])
			continue;
	/*
	 * Get rid of any entries at the head of the list.  This will
	 * usually get them all.
	 */
		while (Requests[plat] &&
		       ! strcmp (Requests[plat]->nr_from, proc))
		{
			zap = Requests[plat];
			Requests[plat] = zap->nr_next;
			dap_FreeNR (zap);
		}
	/*
	 * Now we have to get anything left after the list head.
	 */
		zap = last = Requests[plat];
		while (zap)
		{
			if (strcmp (zap->nr_from, proc))
				last = zap;
			else
			{
				last->nr_next = zap->nr_next;
				dap_FreeNR (zap);
			}
			zap = last->nr_next;
		}
	}
/*
 * Notify the copy process if there is one.  Append the client name to the
 * template message to tell the copy process which client should be
 * cancelled.
 */
	if (Copies)
	{
		char buf[256];
		struct dsp_Template *dt = (struct dsp_Template *) buf;

		dt->dsp_type = dpt_CancelNotify;
		strcpy (((char *) dt) + sizeof (*dt), proc);
		msg_send (CopyProc, MT_DATASTORE, FALSE, dt,
			  sizeof (*dt) + strlen (proc) + 1);
	}
}





void
dap_Notify (pid, t, nsample, now, append)
PlatformId pid;
ZebTime *t;
int nsample;	/* number of new samples */
int now;	/* number of overwritten samples */
int append;	/* non-zero if new samples are the most recent for this plat */
/*
 * Actually send out notifications that data is available for this platform
 * up through this time.
 */
{
	NRequest *notify;
	struct dsp_Notify msg;
/*
 * If nobody's interested, don't bother.
 */
	if (! Requests[pid])
		return;
/*
 * Fill in the notification structure.
 */
	msg.dsp_type = dpt_Notify;
	msg.dsp_pid = pid;
	msg.dsp_when = *t;
	msg.dsp_ucode = append ? UpdAppend : 
			((nsample > 0) ? UpdInsert : UpdOverwrite);
	msg.dsp_nsample = (msg.dsp_ucode == UpdOverwrite) ? now : nsample;
/*
 * Go through and tell everybody.
 */
	for (notify = Requests[pid]; notify; notify = notify->nr_next)
	{
		msg.dsp_param = notify->nr_param;
		msg_send (notify->nr_from, MT_DATASTORE, FALSE, &msg,
				sizeof (msg));
	}
}





void
dap_Copy (proc)
char *proc;
/*
 * Set up to send copies to this proc.
 */
{
	if (Copies)
		msg_ELog (EF_PROBLEM, "Overriding existing copyproc %s",
			CopyProc);
	Copies = TRUE;
	strcpy (CopyProc, proc);
}






int
dap_IsInterest (pid)
int pid;
/*
 * Return true iff some process has a notification request on this platform.
 */
{
	return (Requests[pid] != 0);
}
