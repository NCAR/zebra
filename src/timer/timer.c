/*
 * The timer process.
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
char *Version = "$Revision: 2.7 $ $Date: 1994-11-17 06:42:22 $";

# include <sys/types.h>
# include <sys/time.h>
# include <signal.h>
# include <errno.h>

# include <defs.h>
# include <message.h>
# include <timer.h>
MAKE_RCSID ("$Id: timer.c,v 2.7 1994-11-17 06:42:22 granger Exp $")

/*
 * The timer queue is made up of these sorts of entries.
 */
struct tq_entry
{
	char	tqe_proc[MAX_NAME_LEN];	/* The process awaiting this TO */
	struct timeval tqe_when;	/* When this process wants it   */
	int	tqe_param;		/* Proc-supplied param		*/
	int	tqe_inc;		/* Alarm increment		*/
	int	tqe_nalarm;		/* How many alarms have fired	*/
	struct tq_entry *tqe_next;	/* Next in the chain		*/
};

/*
 * The actual timer queue, and the free list.
 */
struct tq_entry *Tq = 0;
struct tq_entry *T_free = 0;

/*
 * The time offset, in seconds, when we are running in pseudo real time
 * mode.
 */
int	T_offset = 0;

/*
 * Forward routines.
 */
struct tq_entry * new_tqe ();
void timer_alarm ();
int msg_handler ();
struct timeval *GetTime ();
void GetZebTime FP ((ZebTime *));
void ZtToTimeval FP ((ZebTime *, struct timeval *));
void TimevalToZt FP ((struct timeval *, ZebTime *));
int Status FP ((char *));



/*
 * TLT (t1, t2) is true iff t1 is before t2.
 */
# define TLT(t1,t2) (((t1)->tv_sec < (t2)->tv_sec) || \
	((t1)->tv_sec == (t2)->tv_sec && (t1)->tv_usec < (t2)->tv_usec))


int
main (argc, argv)
int argc;
char *argv[];
{
	struct timeval timeout;
	fd_set fds;
	int mfd;
/*
 * Connect to the message handler.  Hook into the client events group so
 * that we can zap requests for clients that die.
 */
	if (! msg_connect (msg_handler, TIMER_PROC_NAME))
	{
		printf ("%s: unable to connect to message handler\n", argv[0]);
		exit (1);
	}
	msg_join ("Client events");
	msg_SetQueryHandler (Status);
/*
 * Log a message telling the world we're here.
 */
	msg_ELog (EF_INFO, "--- Timer process version %s", Version);
/*
 * Now just wait for things to happen.
 */
	/* msg_await (); */
	FD_ZERO (&fds);
	mfd = msg_get_fd ();
	for (;;)
	{
		int nsel;
	/*
	 * Get everything set up.
	 */
		FD_SET (mfd, &fds);
		SetTimeout (&timeout);
	/*
	 * Now wait for something.
	 */
		if ((nsel = select (mfd + 1,
# ifdef hpux
				    (int *)
# endif
				           &fds, 0, 0, &timeout)) < 0)
		{
			msg_ELog (EF_EMERGENCY, "Select error %d", errno);
			exit (1);
		}
	/*
	 * React.
	 */
		if (nsel > 0 && FD_ISSET (mfd, &fds) && msg_incoming (mfd))
			exit (0);
		RunQueue ();
	}
}




int
msg_handler (msg)
struct message *msg;
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
/*
 * Just branch out on the message type.
 */
	switch (msg->m_proto)
	{
	/*
	 * Most of our stuff should be TR requests.
	 */
	   case MT_TIMER:
	   	timer_request (msg->m_from, (struct tm_req *) msg->m_data);
		break;
	/*
	 * Message handler stuff.
	 */
	   case MT_MESSAGE:
	   	if (tm->mh_type == MH_SHUTDOWN)
			exit (1);
		else if (tm->mh_type == MH_CLIENT)
			client_event ((struct mh_client *) tm);
		else
			msg_ELog (EF_PROBLEM, "Unknown MESSAGE proto type: %d",
				tm->mh_type);
		break;
	}
	return (0);
}





struct tq_entry *
new_tqe ()
/*
 * Return a pointer to a new timer queue entry.
 */
{
	struct tq_entry *ret;
/*
 * Pull it off the free list if at all possible.
 */
	if (T_free)
	{
		ret = T_free;
		T_free = T_free->tqe_next;
	}
/*
 * Otherwise allocate a new one.
 */
 	else
		ret = ALLOC (struct tq_entry);
	memset (ret, 0, sizeof (struct tq_entry));
	return (ret);
}









timer_request (who, tr)
char *who;
struct tm_req *tr;
/*
 * Deal with a timer request.
 */
{
	switch (tr->tr_type)
	{
	   case TR_TIME:
	   	SendTime (who);
		break;

	   case TR_RELATIVE:
		RelativeTR (who, (struct tm_rel_alarm_req *) tr);
		break;

	   case TR_CANCEL:
		ZapRequests (who, TRUE, ((struct tm_cancel *) tr)->tm_param,
				FALSE);
		break;

	   case TR_CANCELALL:
		ZapRequests (who, TRUE, 0, TRUE);
		break;

	   case TR_ABSOLUTE:
	   	AbsoluteTR (who, (struct tm_abs_alarm_req *) tr);
		break;
# ifdef notdef /* superseded by zquery */
	   case TR_STATUS:
	   	Status (who);
		break;
# endif
	   case TR_PRT:
	   	EnterPRT ((struct tm_prt *) tr);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown TR: %d from %s\n",
			tr->tr_type, who);
		break;
	}
}




client_event (ce)
struct mh_client *ce;
/*
 * Deal with a client event.
 */
{
/*
 * The only thing we are really interested in is client disconnects -- at
 * which point we go and delete all of there requests.
 */
	if (ce->mh_evtype == MH_CE_DISCONNECT)
		ZapRequests (ce->mh_client, FALSE, 0, TRUE);
}



void
timer_alarm ()
/*
 * Deal with a timer alarm.
 */
{
	msg_log ("Timer alarm");
}





SendTime (who)
char *who;
/*
 * Send the current time to this process.
 */
{
	struct tm_time repl;

	repl.tm_type = TRR_TIME;
	GetZebTime (&repl.tm_time);
	msg_send (who, MT_TIMER, FALSE, &repl, sizeof (repl));
}




void
GetZebTime (zt)
ZebTime *zt;
/*
 * Get the current time in zeb format.
 */
{
	TimevalToZt (GetTime (), zt);
}



struct timeval *
GetTime ()
/*
 * Return the current system time.
 */
{
	static struct timeval tv;
	struct timezone tz;

	gettimeofday (&tv, &tz);
	tv.tv_sec -= T_offset;
	return (&tv);
}





SetTimeout (t)
struct timeval *t;
/*
 * Set the time needed to wait until the next tqe fires.
 */
{
	struct timeval *ct = GetTime ();
/*
 * If the timer queue is empty, we wait for a long time.
 */
	if (! Tq)
	{
		t->tv_sec = 99999;
		t->tv_usec = 0;
	}
/*
 * Otherwise we need to figure something out.
 */
	else
	{
		t->tv_sec = Tq->tqe_when.tv_sec - ct->tv_sec;
		if ((t->tv_usec = Tq->tqe_when.tv_usec - ct->tv_usec) < 0)
		{
			t->tv_sec--;
			t->tv_usec += 1000000;
		}
		if (t->tv_sec < 0)
			t->tv_sec = t->tv_usec = 0;
	}
}





RunQueue ()
/*
 * Run through the timer queue, and see if anything is ready to go.
 */
{
	struct tq_entry *tq;
	struct timeval *ct = GetTime ();
/*
 * If the queue is empty, do nothing.
 */
	if (! Tq)
		return;
/*
 * Go through the queue, dealing with anything that has expired.
 */
	while (Tq && TLT (&Tq->tqe_when, ct))
	{
		tq = Tq;
		Tq = tq->tqe_next;
		SendAlarm (tq, ct);
	}
}





RelativeTR (who, tr)
char *who;
struct tm_rel_alarm_req *tr;
/*
 * Enqueue a relative time request.
 */
{
	struct tq_entry *tqe = new_tqe ();
	struct timeval *ct = GetTime ();
/*
 * Put together the tqe.
 */
	strcpy (tqe->tqe_proc, who);
	tqe->tqe_when = *ct;
	AddDelay (&tqe->tqe_when, tr->tr_delay);
	tqe->tqe_param = tr->tr_param;
	tqe->tqe_inc = tr->tr_inc;
	tqe->tqe_nalarm = 0;
/*
 * Add it to the queue.
 */
	Enqueue (tqe);
}



AbsoluteTR (who, tr)
char *who;
struct tm_abs_alarm_req *tr;
/*
 * Enqueue a absolute time request.
 */
{
	struct tq_entry *tqe = new_tqe ();
/*
 * Put together the tqe.
 */
	strcpy (tqe->tqe_proc, who);
	/* CvtFccToSys (&tr->tr_when, &tqe->tqe_when); */
	ZtToTimeval (&tr->tr_when, &tqe->tqe_when);
	tqe->tqe_param = tr->tr_param;
	tqe->tqe_inc = tr->tr_inc;
	tqe->tqe_nalarm = 0;
/*
 * Add it to the queue.
 */
	Enqueue (tqe);
}





AddDelay (t, delay)
struct timeval *t;
int delay;
/*
 * Add this delay (in fractional seconds) to the time.
 */
{
	if ((t->tv_usec += ((delay % INCFRAC) * 100000)) > 1000000)
	{
		t->tv_sec++;
		t->tv_usec -= 1000000;
	}
	t->tv_sec += delay/INCFRAC;
}





Enqueue (tqe)
struct tq_entry *tqe;
/*
 * Add this entry to the timer queue.
 */
{
	struct tq_entry *tp, *last = Tq;
	struct timeval *tv = &tqe->tqe_when;
/*
 * Check for the head of the list.
 */
	if (! Tq || TLT (tv, &Tq->tqe_when))
	{
		tqe->tqe_next = Tq;
		Tq = tqe;
		return;
	}
/*
 * Nope, we have to search for it.
 */
	for (tp = Tq->tqe_next; tp && TLT (&tp->tqe_when, tv);
			tp = tp->tqe_next)
		last = tp;
	tqe->tqe_next = last->tqe_next;
	last->tqe_next = tqe;
}





SendAlarm (tqe, ct)
struct tq_entry *tqe;
struct timeval *ct;
/*
 * Deal with an alarm whose time has come.
 */
{
	struct tm_alarm alarm;
/*
 * Send back the alarm packet.
 */
	alarm.tm_type = TRR_ALARM;
	/* CvtSysToFcc (ct, &alarm.tm_time); */
	TimevalToZt (ct, &alarm.tm_time);
	alarm.tm_param = tqe->tqe_param;
	msg_send (tqe->tqe_proc, MT_TIMER, FALSE, &alarm, sizeof (alarm));
/*
 * If this is a recurring alarm, requeue it.
 */
	if (tqe->tqe_inc)
	{
		AddDelay (&tqe->tqe_when, tqe->tqe_inc);
		tqe->tqe_nalarm++;
		Enqueue (tqe);
	}
/*
 * Otherwise get rid of it.
 */
	else
		Free_tqe (tqe);
}





Free_tqe (tqe)
struct tq_entry *tqe;
/*
 * Add this entry to the free list.
 */
{
	tqe->tqe_next = T_free;
	T_free = tqe;
}




ZapRequests (who, ack, param, all)
char *who;
int param;
bool ack, all;
/*
 * Delete timer requests belonging to this process.
 */
{
	struct tq_entry *tp, *last;
/*
 * Clean anything of the head of the queue first.
 */
	while (Tq && ! strcmp (Tq->tqe_proc, who) &&
		(all || Tq->tqe_param == param))
	{
		if (ack)
			SendCancelAck (Tq);
		tp = Tq;
		Tq = Tq->tqe_next;
		Free_tqe (tp);
	}
/*
 * Now we go into the depths of the list.
 */
	if (! Tq)
		return;
	last = Tq;
	tp = Tq->tqe_next;
	while (tp)
	{
		if (! strcmp (tp->tqe_proc, who) &&
			(all || param == tp->tqe_param))
		{
			if (ack)
				SendCancelAck (tp);
			last->tqe_next = tp->tqe_next;
			Free_tqe (tp);
		}
		else
			last = tp;
		tp = last->tqe_next;
	}
}





SendCancelAck (tqe)
struct tq_entry *tqe;
/*
 * Send an ack to the process that this timer request has been cancelled.
 */
{
	struct tm_alarm tr;

	tr.tm_type = TRR_CANCELACK;
	/* CvtSysToFcc (GetTime (), &tr.tm_time); */
	GetZebTime (&tr.tm_time);
	tr.tm_param = tqe->tqe_param;
	msg_send (tqe->tqe_proc, MT_TIMER, FALSE, &tr, sizeof (tr));
}





Status (who)
char *who;
/*
 * Send a status report back to "who".
 */
{
	char tbuf[2048];
	struct tq_entry *tqe;
	ZebTime zt;
/*
 * Send out a header line.
 */
	sprintf (tbuf, "Timer module version %s", Version);
	msg_AnswerQuery (who, tbuf);
	GetZebTime (&zt);
	strcpy (tbuf, "Current time is ");
	TC_EncodeTime (&zt, TC_Full, tbuf + strlen (tbuf));
	msg_AnswerQuery (who, tbuf);
/*
 * Now add each queue entry.
 */
	for (tqe = Tq; tqe; tqe = tqe->tqe_next)
	{
		sprintf (tbuf, "\t%s: alarm ", tqe->tqe_proc);
		TC_EncodeTime ((ZebTime *) &tqe->tqe_when, TC_FullUSec,
				tbuf + strlen (tbuf));
		if (tqe->tqe_inc)
			sprintf (tbuf + strlen (tbuf), " incr %d N %d",
				tqe->tqe_inc, tqe->tqe_nalarm);
		msg_AnswerQuery (who, tbuf);
	}
/*
 * All done.
 */
	msg_FinishQuery (who);
}





EnterPRT (prt)
struct tm_prt *prt;
/*
 * Throw the system into pseudo real time mode.
 */
{
	int oldoffset = T_offset;
	struct timeval newtime, *now;
/*
 * Convert the desired time into a system time, and make sure that it is
 * not in the future.
 */
	ZtToTimeval (&prt->tr_time, &newtime);
	T_offset = 0;	/* To get real time */
	now = GetTime ();
	if (TLT (now, &newtime))
	{
		T_offset = oldoffset;
		msg_ELog (EF_EMERGENCY,"*** Attempt to run PRT in the future");
		return;
	}
/*
 * Set the new offset, and inform the world.
 */
	T_offset = now->tv_sec - newtime.tv_sec;
	msg_ELog (EF_INFO, "Pseudo RT mode: offset = %d", T_offset);
	TimeChangeBc ();
}






TimeChangeBc ()
/*
 * Broadcast a time change to the world.
 */
{
	struct tm_tchange tc;
/*
 * Put together the broadcast.
 */
	tc.tm_type = TRR_TCHANGE;
	/* CvtSysToFcc (GetTime (), &tc.tm_time); */
	GetZebTime (&tc.tm_time);
	tc.tm_pseudo = (T_offset != 0);
/*
 * Send it.
 */
	msg_send ("TimeChange", MT_TIMER, TRUE, &tc, sizeof (tc));
}





void
ZtToTimeval (zt, tv)
ZebTime *zt;
struct timeval *tv;
/*
 * Convert a zeb-format time to system timeval.
 */
{
	*tv = * (struct timeval *) zt;
}



void
TimevalToZt (tv, zt)
struct timeval *tv;
ZebTime *zt;
/*
 * Go back the other way.
 */
{
	*zt = * (ZebTime *) tv;
}
