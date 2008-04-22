/*
 * Routines specifically for the client side, where platform requests
 * need to be sent to the daemon.
 */
/*		Copyright (C) 1998 by UCAR
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

#include <defs.h>

#include "DataStore.h"
#include "dsPrivate.h"
#include "Platforms.h"
#include "Appl.h"		/* Need ds_SendToDaemon */

RCSID ("$Id")

/* ================ Local forwards 
 */
static PlatClassId dt_ClientDefineClass (PlatClassRef pc);
static PlatformId dt_ClientDefinePlatform (PlatClassId cid, const char *name, 
					   PlatformId parent);
static int dt_AwaitClass (Message *msg, PlatformClass **pc);
static PlatformClass *dt_ClientClassStruct (PlatClassId cid, const char *name);
static int dt_AwaitPlat (Message *msg, Platform **p);
static Platform *dt_ClientPlatStruct (PlatformId pid, const char *name);
static int dt_AwaitPID (Message *msg, PlatformId *pid);
static int dt_ClientGetNPlat ();
static int dt_AwaitNPlat (Message *msg, int *np);
static void dt_ClientSendSearch (struct dsp_PlatformSearch *search,
				 PlatformList *pl);
static int dt_AwaitPlatformList FP ((Message *msg, PlatformList *));


/*
 * Our table of platform methods for communicating with the daemon.
 */
static PlatformMethods ClientMethods =
{
	dt_ClientPlatStruct,
	dt_ClientClassStruct,
	dt_ClientDefinePlatform,
	dt_ClientDefineClass,
	dt_ClientSendSearch,
	dt_ClientGetNPlat
};


/*
 * Here's our only public routine, which sets the client methods
 * in the table module.
 */
void
dt_PlatformClientMode ()
{
	dt_SetMethods (&ClientMethods);
}


#ifdef notdef
/*
 * Abandon the lookup id messages since it will be rare if ever 
 * that an application wants to know an id but does not need the 
 * structure for further information.  Since platform and class
 * structures no longer change, it does not hurt to cache the
 * structure the first time it is referenced, and the id can come
 * from the structure.
 */

PlatformId
dt_ClientLookupPlatform (name)
const char *name;
/*
 * Find this platform.
 */
{
	int             type;
	SValue          v;
	PlatformId pid;
	struct dsp_PLookup req;
/*
 * It can't be a valid platform if the name won't fit into the request.
 */
	if (strlen((char *)name) >= sizeof(req.dsp_name))
		return (BadPlatform);
/*
 * Otherwise we need to ask mom.
 */
	req.dsp_type = dpt_LookupPlatform;
	strcpy (req.dsp_name, (char *)name);
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitPID, &pid);
	return (pid);
}



PlatClassId
dt_ClientLookupClass (name)
const char *name;
/*
 * Find this platform class.
 */
{
	int             type;
	SValue          v;
	PlatClassId	cid;
	struct dsp_PLookup req;
/*
 * It can't be a valid class if the name won't fit into the request.
 */
	if (strlen((char *)name) >= sizeof(req.dsp_name))
		return (BadClass);
/*
 * Otherwise we need to ask mom.
 */
	req.dsp_type = dpt_LookupClass;
	strcpy (req.dsp_name, (char *)name);
	ds_SendToDaemon (&req, sizeof (req));
	msg_Search (MT_DATASTORE, ds_AwaitPID, &cid);
	return (cid);
}
#endif



static int
dt_AwaitPID (msg, pid)
Message *msg;
PlatformId *pid;
/*
 * Wait for the platform lookup to return.
 */
{
	struct dsp_PID *dp = (struct dsp_PID *) msg->m_data;

	if ((dp->dsp_type == dpt_R_PID) || (dp->dsp_type == dpt_R_CID))
	{
		*pid = dp->dsp_pid;
		return (0);
	}
	return (1);
}




static Platform *
dt_ClientPlatStruct (PlatformId pid, const char *name)
/*
 * Request the platform structure for this PID from the daemon.
 * We are only called if the caller explicitly wants us to ask the daemon.
 * The pointer we return must be freed by the caller.
 */
{
	Platform in, *p;
	struct dsp_GetPlatStruct req;

	req.dsp_type = dpt_GetPlatStruct;
	req.dsp_id = pid;
	if (pid == BadPlatform)
	{
		if (!name || (strlen((char *)name) >= sizeof(req.dsp_name)))
			return (NULL);
		strcpy (req.dsp_name, (char *)name);
	}
	/*
	 * Send off the request and wait for an answer.
	 */
	ds_SendToDaemon (&req, sizeof (req));
	in.dp_id = pid;
	p = &in;
	msg_Search (MT_DATASTORE, dt_AwaitPlat, &p);
	return (p);
}



static void
dt_ClientFillSubplats (Platform *p)
/*
 * This platform needs its local array of subplatforms filled.
 * Ask the daemon to give us the list with a subplatform search.
 */
{
    int i;
    int npid = 0;
    PlatformId *pids;

    /* 
     * We could check for consistency here between the number of subplats
     * the parent platform thinks it should have and the number sent by
     * the daemon, but we give the daemon absolute authority instead.
     */
    p->dp_subplats = 0;
    p->dp_nsubplats = 0;
    pids = ds_LookupSubplatforms (p->dp_id, &npid);

    if (pids)
    {
	for (i = 0; i < npid; ++i)
	    dt_AddSubPlat (p, pids[i]);
	free (pids);
    }
}




static int
dt_AwaitPlat (msg, p)
Message *msg;
Platform **p;
/*
 * See if this is our platform structure.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;
	PlatformId want_id = (*p)->dp_id;

	if (tmpl->dsp_type == dpt_R_PlatStruct)
	{
		struct dsp_PlatStruct *ans;
	/*
	 * Sanity check
	 */
		if (msg->m_len != sizeof (struct dsp_PlatStruct))
		{
			msg_ELog (EF_EMERGENCY, 
				  "Plat structure size mismatch (%d vs. %d)!",
				  sizeof (struct dsp_PlatStruct), msg->m_len);
			msg_ELog (EF_EMERGENCY, 
			  "Make sure dsDaemon and I are compiled with the");
			msg_ELog (EF_EMERGENCY,
			  "same value of CFG_PLATNAME_LEN in config.h");

			exit (1);
		}
		ans = (struct dsp_PlatStruct *) msg->m_data;
		/*
		 * Doublecheck IDs
		 */
		if (want_id != BadPlatform && want_id != ans->dsp_plat.dp_id)
		{
			msg_ELog (EF_PROBLEM, "%s %d after requesting %d",
				  "Got platform", ans->dsp_plat.dp_id,
				  (*p)->dp_id);
			*p = NULL;
		}
		else if (ans->dsp_result)
		{
			*p = (Platform *) malloc (sizeof (Platform));
			**p = ans->dsp_plat;
			if ((*p)->dp_nsubplats > 0)
			{
			    dt_ClientFillSubplats (*p);
			}
		}
		else
			*p = NULL;
		return (0);
	}
	return (1);
}




static PlatformClass *
dt_ClientClassStruct (PlatClassId cid, const char *name)
/*
 * Ask the daemon for the class structure for this class ID.
 */
{
	PlatformClass *pc, in;
	struct dsp_GetPlatStruct req;

	req.dsp_type = dpt_GetClassStruct;
	req.dsp_id = cid;
	if (cid == BadPlatform)
	{
		if (!name || (strlen((char *)name) >= sizeof(req.dsp_name)))
			return (NULL);
		strcpy (req.dsp_name, (char *)name);
	}
	/*
	 * Send off the request and wait for an answer.
	 */
	ds_SendToDaemon (&req, sizeof (req));
	/* 
	 * Pass in the id we expect in a static structure, which will
	 * be replaced with either null or an allocated structure.
	 */
	pc = &in;
	in.dpc_id = cid;
	msg_Search (MT_DATASTORE, dt_AwaitClass, &pc);
	return (pc);
}




static int
dt_AwaitClass (msg, pc)
Message *msg;
PlatformClass **pc;
/*
 * See if this is our platform class structure.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;
	PlatClassId want_id = (*pc)->dpc_id;

	if (tmpl->dsp_type == dpt_R_ClassStruct)
	{
		struct dsp_ClassStruct *dsp;
	/*
	 * Sanity check: however, we only know there's a problem when the
	 * recieved length is less than our expected minimum.
	 */
		if (msg->m_len < sizeof (struct dsp_ClassStruct))
		{
			msg_ELog (EF_EMERGENCY, 
				  "Class structure size mismatch (%d > %d)!",
				  sizeof (struct dsp_PlatStruct), msg->m_len);
			msg_ELog (EF_EMERGENCY, 
			  "Make sure dsDaemon and I are compiled with the");
			msg_ELog (EF_EMERGENCY,
			  "same value of CFG_PLATNAME_LEN in config.h");

			exit (1);
		}
		dsp = (struct dsp_ClassStruct *) msg->m_data;
	/*
	 * Doublecheck IDs
	 */
		if (want_id != BadClass && want_id != dsp->dsp_class.dpc_id)
		{
			msg_ELog (EF_PROBLEM, "%s %d after requesting %d",
				  "Got class", dsp->dsp_class.dpc_id,
				  want_id);
		}
		if (dsp->dsp_result)
		{
		/*
		 * Extract a positive result.
		 */
			*pc = (PlatformClass *) malloc (sizeof (**pc));
			dt_ExtractClass (*pc, dsp, msg->m_len);
		}
		else
			*pc = NULL;
		return (0);
	}
	return (1);
}



static PlatformId
dt_ClientDefinePlatform (PlatClassId cid, const char *name, PlatformId parent)
/*
 * Send off a definition message to the daemon.
 */
{
	struct dsp_Instance im;
	PlatformId pid;

	im.dsp_type = dpt_Instantiate;
	im.dsp_class = cid;
	dt_SetString (im.dsp_name, name, sizeof(im.dsp_name), 
		      "sending instantiation message");
	im.dsp_parent = parent;

	ds_SendToDaemon (&im, sizeof (im));
	msg_Search (MT_DATASTORE, dt_AwaitPID, &pid);
	return (pid);
}



static PlatClassId
dt_ClientDefineClass (PlatClassRef pc)
{
	struct dsp_ClassStruct cdm, *send;
	int len;
	PlatClassId cid;
	/*
	 * Inject the class structure into a dsp_ClassStruct
	 */
	send = dt_InjectClass (pc, &cdm, &len);
	send->dsp_type = dpt_DefineClass;
	ds_SendToDaemon (send, len);
	msg_Search (MT_DATASTORE, dt_AwaitPID, &cid);
	if (send != &cdm)
		free (send);
	return (cid);
}



static int
dt_ClientGetNPlat ()
/*
 * Ask the daemon about the number of platforms it knows.
 */
{
	struct dsp_Template req;
	int nplat;
/*
 * Send off the request to the daemon.
 */
 	req.dsp_type = dpt_GetNPlat;
	ds_SendToDaemon ( &req, sizeof(req));
/*
 * Now we gotta wait for the answer.
 */
	msg_Search (MT_DATASTORE, dt_AwaitNPlat, &nplat);
	return (nplat);
}





static int
dt_AwaitNPlat (msg, np)
Message *msg;
int *np;
/*
 * See if this is our ack.
 */
{
	struct dsp_Template *tmpl = (struct dsp_Template *) msg->m_data;

	if (tmpl->dsp_type == dpt_R_NPlat)
	{
		struct dsp_NPlat *dnp = (struct dsp_NPlat *) msg->m_data;
		*np = dnp->dsp_nplat;
		return (0);
	}
	return (1);
}




static void
dt_ClientSendSearch (search, pl)
struct dsp_PlatformSearch *search;
PlatformList *pl;
{
	ds_SendToDaemon (search, sizeof(struct dsp_PlatformSearch));
	msg_Search (MT_DATASTORE, dt_AwaitPlatformList, pl);
}



static int
dt_AwaitPlatformList (msg, pl)
Message *msg;
PlatformList *pl;
/*
 * See if this is our ack.
 */
{
    struct dsp_PlatformList *ans = (struct dsp_PlatformList *)msg->m_data;

    if (ans->dsp_type == dpt_R_PlatformSearch)
    {
    /* 
     * If this is the list, copy it into new memory and set our argument to
     * point to this list. 
     */
	pl->pl_npids = ans->dsp_npids;
	pl->pl_pids = NULL;
	if (ans->dsp_npids > 0)
	{
	    int nbytes = ans->dsp_npids * sizeof(PlatformId);
	    pl->pl_pids = (PlatformId *) malloc (nbytes);
	    memcpy (pl->pl_pids, ans->dsp_pids, nbytes);
	}
	return (MSG_DONE);
    }

    return (MSG_ENQUEUE);
}



