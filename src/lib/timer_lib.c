/*
 * Access routines for the timer module.
 */
static char *rcsid = "$Id: timer_lib.c,v 1.3 1991-01-30 20:44:37 corbet Exp $";

# include "../include/defs.h"
# include "timer.h"
# include "../include/message.h"


typedef enum { Empty = 0, Active, Cancelled } sstatus;

/*
 * The list of pending timer events.
 */
# define MAXEVENT 10
static struct Tevent
{
	void	(*te_func) ();		/* Function to call		*/
	void	*te_param;		/* Param for func		*/
	sstatus	te_status;		/* This stuff in use		*/
	bool	te_recurring;		/* Does this one recurr?	*/
} Events[MAXEVENT] = { 0 };

static bool First = TRUE;


/*
 * Forward routines
 */
# ifdef __STDC__
	static void tl_SendToTimer (void *, int);
	static void tl_DispAlarm (struct tm_alarm *);
	static void tl_CancelAck (struct tm_alarm *);
	static int tl_ProtoHandler (struct message *);
# else
	static void tl_SendToTimer ();
	static void tl_DispAlarm ();
	static void tl_CancelAck ();
	static int tl_ProtoHandler ();
# endif




static void inline
tl_Init ()
{
	if (First)
	{
		First = FALSE;
		msg_AddProtoHandler (MT_TIMER, tl_ProtoHandler);
	}
}



int
tl_AddRelativeEvent (func, param, delay, incr)
void (*func) ();
void *param;
int delay, incr;
/*
 * Add a relative timer event.
 * Entry:
 *	FUNC	is the function to call when the event happens.
 *	PARAM	is a parameter to pass to FUNC
 *	DELAY	is the delay until the first timer event
 *	INCR	is the increment between successive events, or zero.
 * Exit:
 *	The event has been queued.
 */
{
	int i;
	struct tm_rel_alarm_req alr;

	tl_Init ();
/*
 * Find an empty slot.
 */
	for (i = 0; i < MAXEVENT && Events[i].te_status != Empty; i++)
		;
	if (i >= MAXEVENT)
	{
		msg_ELog (EF_EMERGENCY, "Out of event slots");
		return (-1);
	}
/*
 * Fill in the information.
 */
	Events[i].te_func = func;
	Events[i].te_param = param;
	Events[i].te_status = Active;
	Events[i].te_recurring = incr != 0;
/*
 * Pass on the request to the timer module.
 */
	alr.tr_type = TR_RELATIVE;
	alr.tr_delay = delay;
	alr.tr_inc = incr;
	alr.tr_param = i;
	tl_SendToTimer (&alr, sizeof (alr));
	return (i);
}





int
tl_AddAbsoluteEvent (func, param, when, incr)
void (*func) ();
void *param;
time *when;
int incr;
/*
 * Add an absolute timer event.
 * Entry:
 *	FUNC	is the function to call when the event happens.
 *	PARAM	is a parameter to pass to FUNC
 *	WHEN	is the time of the first alarm.
 *	INCR	is the increment between successive events, or zero.
 * Exit:
 *	The event has been queued.
 */
{
	int i;
	struct tm_abs_alarm_req alr;

	tl_Init ();
/*
 * Find an empty slot.
 */
	for (i = 0; i < MAXEVENT && Events[i].te_status != Empty; i++)
		;
	if (i >= MAXEVENT)
	{
		msg_ELog (EF_EMERGENCY, "Out of event slots");
		return (-1);
	}
/*
 * Fill in the information.
 */
	Events[i].te_func = func;
	Events[i].te_param = param;
	Events[i].te_status = Active;
	Events[i].te_recurring = incr != 0;
/*
 * Pass on the request to the timer module.
 */
	alr.tr_type = TR_ABSOLUTE;
	alr.tr_when = *when;
	alr.tr_inc = incr;
	alr.tr_param = i;
	tl_SendToTimer (&alr, sizeof (alr));
	return (i);
}





static int
tl_ProtoHandler (msg)
struct message *msg;
/*
 * The message protocol handler.  Done this way for hysterical reasons.
 */
{
	tl_DispatchEvent ((struct tm_time *) msg->m_data);
	return (0);
}




void
tl_DispatchEvent (te)
struct tm_time *te;
/*
 * Dispatch this timer event.
 */
{
	switch (te->tm_type)
	{
	   case TRR_ALARM:
	   	tl_DispAlarm ((struct tm_alarm *) te);
		break;

	   case TRR_CANCELACK:
	   	tl_CancelAck ((struct tm_alarm *) te);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM,
			"BUG: Funky timer resp type %d in tl_DispatchEvent",
			te->tm_type);
	}
}




static void
tl_DispAlarm (te)
struct tm_alarm *te;
{
	int slot = te->tm_param;
/*
 * Sanity check.
 */
	if (slot < 0 || slot >= MAXEVENT || Events[slot].te_status == Empty)
	{
		msg_ELog (EF_PROBLEM, "Funky param in alarm event: %d", slot);
		return;
	}
/*
 * If this event has been cancelled, then this is a spurious alarm that
 * crossed with our cancel request.  Ignore it.
 */
	if (Events[slot].te_status == Cancelled)
		return;
/*
 * Dispatch the event.
 */
	(*Events[slot].te_func) (&te->tm_time, Events[slot].te_param);
/*
 * If this is a non-recurring event, clear the entry.
 */
	if (! Events[slot].te_recurring)
		Events[slot].te_status = Empty;
}




void
tl_AllCancel ()
/*
 * Cancel all alarm events.
 */
{
	int i;

	for (i = 0; i < MAXEVENT; i++)
		if (Events[i].te_status == Active)
			tl_Cancel (i);
}




void
tl_Cancel (slot)
int slot;
/*
 * Cancel the alarm on this slot.
 */
{
	struct tm_cancel tr;

	tl_Init ();
/*
 * Sanity check.
 */
	if (slot < 0 || slot >= MAXEVENT || Events[slot].te_status != Active)
	{
		msg_ELog (EF_PROBLEM, "Funky param in tl_Cancel: %d", slot);
		return;
	}
/*
 * Send the cancel request.
 */
	tr.tm_type = TR_CANCEL;
	tr.tm_param = slot;
	tl_SendToTimer (&tr, sizeof (tr));
/*
 * Mark the slot as cancelled.
 */
	Events[slot].te_status = Cancelled;
}






static void
tl_CancelAck (te)
struct tm_alarm *te;
{
	int slot = te->tm_param;
/*
 * Sanity check.
 */
	if (slot < 0 || slot >= MAXEVENT || Events[slot].te_status !=Cancelled)
	{
		msg_ELog (EF_PROBLEM, "Funky param in cancel ACK: %d", slot);
		return;
	}
/*
 * We can now clear the slot.
 */
	Events[slot].te_status = Empty;
}





static void inline
tl_SendToTimer (data, len)
void *data;
int len;
/*
 * Send something to the timer.
 */
{
	msg_send (TIMER_PROC_NAME, MT_TIMER, FALSE, data, len);
}





void
tl_GetTime (t)
time *t;
/*
 * Return the current time in t.
 */
{
	static int tl_TimeHandler ();
	struct tm_req req;
/*
 * Send off the timer request.
 */
	req.tr_type = TR_TIME;
	tl_SendToTimer (&req, sizeof (req));
/*
 * Now wait for the reply.
 */
	msg_Search (MT_TIMER, tl_TimeHandler, t);
}




static int
tl_TimeHandler (msg, t)
struct message *msg;
time *t;
/*
 * The time handler, called out of msg_Search.
 */
{
	struct tm_time *repl = (struct tm_time *) msg->m_data;
/*
 * If this is not a TRR_TIME, blow it off.
 */
	if (repl->tm_type != TRR_TIME)
		return (1);
/*
 * Otherwise store the info.
 */
	*t = repl->tm_time;
	return (0);
}
