/*
 * The application notification module.
 */
static char *rcsid = "$Id: d_Notify.c,v 1.1 1991-02-26 19:13:40 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dsDaemon.h"


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
# define MAXPLAT 256		/* Should do for now		*/
static NRequest *Requests[MAXPLAT];

static NRequest *NRFree = 0;	/* Free lookaside list		*/



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

	strcpy (nr->nr_from, from);
	nr->nr_param = req->dsp_param;
	nr->nr_next = Requests[req->dsp_pid];
	Requests[req->dsp_pid] = nr;
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
}





void
dap_Notify (pid, t)
PlatformId pid;
time *t;
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
