/*
 * Deal with the problem of notifications in pseudo-real time.
 */
# include <defs.h>
# include <message.h>
# include <timer.h>
# include "DataStore.h"
# include "dsPrivate.h"


# ifdef __STDC__
	static int Message (struct message *);
	static void NotificationRequest (struct dsp_Template *);
	static void MakeTimerRequest (PlatformId);
	static void TimeToNotify (time *, PlatformId);
# else
	static int Message ();
	static void NotificationRequest ();
	static void MakeTimerRequest ();
	static void TimeToNotify ();
# endif

/*
 * Keep track of timer events.
 */
# define MAXPLAT 256
static int TimeEvent[MAXPLAT];


main ()
{
	int i;
/*
 * Initialize and hook in to the daemon for copies.
 */
	msg_connect (Message, "Notifier");
	msg_join ("Client events");
	usy_init ();
	ds_Initialize ();
	dap_Init ();
	ds_SnarfCopies (NotificationRequest);
/*
 * Time events too.
 */
	for (i = 0; i < MAXPLAT; i++)
		TimeEvent[i] = -1;
/*
 * Now we just wait for something to happen.
 */
	msg_await ();
}





static int
Message (msg)
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
	struct dsp_NotifyRequest nrq = *(struct dsp_NotifyRequest *) dmsg;
	char *from;
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
	time t, now;
	char *pname = ds_PlatformName (pid);
/*
 * If there is already a timer event on this PID, we assume we don't need
 * to make another one.
 */
	if (TimeEvent[pid] >= 0)
		return;
/*
 * Find out what our time is now, and when the next data shows up.
 */
	tl_GetTime (&now);
	if (! ds_DataTimes (pid, &now, 1, DsAfter, &t))
	{
		msg_ELog (EF_INFO, "No future data for %s", pname);
		return;
	}
/*
 * Set up our request for that time.
 */
	TimeEvent[pid] = tl_AddAbsoluteEvent (TimeToNotify, (void *) pid,
			&t, 0);
}





static void
TimeToNotify (t, pid)
time *t;
PlatformId pid;
/*
 * It's time to send a notification on this platform.
 */
{
/*
 * If somebody is still interested, we do the notification and schedule
 * the next one.
 */
	TimeEvent[pid] = -1;		/* Event is gone	*/
	if (dap_IsInterest (pid))
	{
		dap_Notify (pid, t);
		MakeTimerRequest (pid);
	}
}
