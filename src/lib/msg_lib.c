/*
 * Library routines for the message system.
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
# include <stdio.h>
# include <varargs.h>
# include <errno.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/socket.h>
# include <sys/un.h>
# include <sys/uio.h>
# include "defs.h"
# include "message.h"
# ifndef lint
MAKE_RCSID ("$Id: msg_lib.c,v 2.20 1994-02-02 20:21:53 granger Exp $")
# endif

/*
 * The array of functions linked with file descriptors.
 */
typedef int (*ifptr) ();
static ifptr Fd_funcs[64] = { 0 };
static ifptr Msg_handler;	/* Kludge */
static ifptr Death_handler = 0;
static ifptr Query_Handler;
static ifptr QueryRoutine = 0;

/*
 * The permanent list of file descriptors.
 */
static fd_set Fd_list;
static int Max_fd = 0;
static int Msg_fd;

/*
 * Our sequence number.
 */
static int Seq = 0;

/*
 * State info.  At the moment, this is only used to prevent sending messages,
 * especially log messages, after we've received a SHUTDOWN message.
 */
static int ShuttingDown = 0;
static char Identity[ sizeof(struct mh_ident) ];

/*
 * The queue used for holding messages while looking for something specific.
 */
static struct mqueue
{
	struct message *mq_msg;		/* The actual message		*/
	struct mqueue *mq_next;		/* Next queue entry		*/
} *Mq = 0, *Mq_free = 0, *Mq_tail;


/*
 * The list of protocol-specific handlers.
 */
# define MAXPROTO 25	/* Should last for a while	*/
static ifptr 
ProtoHandlers[MAXPROTO] = { 0 };

/*
 * Forward routines.
 */
static struct mqueue *msg_NewMq FP ((struct message *));
static void msg_RemQueue FP ((struct mqueue *));
static int msg_SrchAck FP ((struct message *, struct mh_ack *));
static int msg_ELHandler FP ((struct message *));
static int msg_PingHandler FP ((Message *));
static int msg_QueryHandler FP ((Message *));
static int msg_DefaultQH FP ((char *));
static void msg_SendPID FP ((void));
static int msg_CQReply FP ((struct message *, int *));
static void msg_free FP ((Message *));

/*
 * How much data we write at once.
 */
# define DCHUNK 512

/*
 * Ardent doesn't provide writev!
 */
# ifdef titan
# define writev fcc_writev
# endif




msg_connect (handler, ident)
ifptr handler;
char *ident;
/*
 * Make a connection to the message server.
 * Entry:
 *	HANDLER	is a function to handle incoming messages.
 *	IDENT	is the identity by which we are to be known to the 
 *		message system.
 * Exit:
 *	If the connection was successfully made then:
 *		The return value is TRUE;
 *	else
 *		FALSE is returned, and no connection exists.
 */
{
	struct sockaddr_un saddr;
	struct message msg;
	struct mh_greeting greet;
	struct mh_ident id;
	int msg_incoming ();
	char *getenv (), *sn = getenv ("ZEB_SOCKET");
/*
 * Presrve our identity
 */
	strcpy (Identity, ident);
/*
 * Create our master socket.
 */
	if ((Msg_fd = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
	{
		perror ("Unable to create UNIX socket");
		return (FALSE);
	}
/*
 * Connect.
 */
 	saddr.sun_family = AF_UNIX;
	strcpy (saddr.sun_path, sn ? sn : UN_SOCKET_NAME);
	if (connect (Msg_fd, (struct sockaddr *) &saddr,
			sizeof (struct sockaddr_un)) < 0)
	{
		if (errno != ENOENT && errno != ECONNREFUSED)
			perror ("Message server connect");
		return (FALSE);
	}
/*
 * Get the greeting message.
 */
	msg_netread (Msg_fd, &msg, sizeof (struct message));
	if (msg.m_proto != MT_MESSAGE || msg.m_len != sizeof (greet))
	{
		printf ("Funky greeting from message server: %d\n",
				msg.m_proto);
		close (Msg_fd);
		return (FALSE);
	}
	msg_netread (Msg_fd, &greet, sizeof (greet));
/*
 * Send our Ident packet.
 */
	msg.m_proto = MT_MESSAGE;
	msg.m_seq = Seq++;
	msg.m_flags = 0;
	msg.m_len = sizeof (id);
	msg.m_data = (char *) &id;
 	id.mh_type = MH_IDENTIFY;
	strcpy (id.mh_name, ident);
	if (! msg_xf_ack (&msg))
	{
		close (Msg_fd);
		return (FALSE);
	}
/*
 * Get our I/O situation together.
 */
	FD_ZERO (&Fd_list);
	FD_SET (Msg_fd, &Fd_list);
	Fd_funcs[Msg_fd] = msg_incoming;
	Msg_handler = handler;
	if (Msg_fd > Max_fd)
		Max_fd = Msg_fd;
/*
 * Establish the event logger handler, and we're done.
 */
	msg_AddProtoHandler (MT_ELOG, msg_ELHandler);
	msg_AddProtoHandler (MT_CPING, msg_PingHandler);
	msg_AddProtoHandler (MT_QUERY, msg_QueryHandler);
	Query_Handler = msg_DefaultQH;
	msg_SendPID ();
 	return (TRUE);
}




static void
msg_SendPID ()
/*
 * Tell the handler what our PID is.
 */
{
	struct mh_pid pid;

	pid.mh_type = MH_PID;
	pid.mh_pid = getpid ();
	msg_send (MSG_MGR_NAME, MT_MESSAGE, FALSE, &pid, sizeof (pid));
}




static int
msg_PingHandler (msg)
Message *msg;
/*
 * Deal with an incoming client ping.
 */
{
/*
 * All we do is turn the protocol back to MT_PING and ship it back to the
 * sender.
 */
	msg->m_proto = MT_PING;
	strcpy (msg->m_to, msg->m_from);
	if (write (Msg_fd, msg, sizeof (Message)) < sizeof (Message))
		perror ("Message structure write");
	return (0);
}




void
msg_AddProtoHandler (proto, handler)
int proto;
ifptr handler;
/*
 * Add a protocol-specific handler.
 */
{
	if (proto >= 0 && proto < MAXPROTO)
		ProtoHandlers[proto] = handler;
}





msg_xf_ack (msg)
struct message *msg;
/*
 * Send a message which expects a returning acknowledgement.
 */
{
	struct mh_ack ack;
	struct iovec iov[2];
/*
 * Put together the outgoing message.
 */
 	iov[0].iov_base = (caddr_t) msg;
	iov[0].iov_len = sizeof (struct message);
	iov[1].iov_base = msg->m_data;
	iov[1].iov_len = msg->m_len;
/*
 * Send it, and get the response back.
 */
 	if (! writev (Msg_fd, iov, 2))
		return (FALSE);
	msg_Search (MT_MESSAGE, msg_SrchAck, &ack);
	return (TRUE);
}





static int
msg_SrchAck (msg, ack)
struct message *msg;
struct mh_ack *ack;
/*
 * Search out an ack.
 */
{
	if (msg->m_len == sizeof (struct mh_ack))
	{
		*ack = * (struct mh_ack *) msg->m_data;
		return (0);
	}
	return (1);
}






void
msg_join (group)
char *group;
/*
 * Join a new message group.
 * Entry:
 *	A connection to the message server exists.
 *	GROUP	is the name of the group of interest.
 * Exit:
 *	The group has been joined.
 */
{
	struct message msg;
	struct mh_ident ident;
/*
 * Put together the JOIN message.
 */
	msg.m_proto = MT_MESSAGE;
	msg.m_seq = Seq++;
	msg.m_flags = 0;
	msg.m_len = sizeof (ident);
	msg.m_data = (char *) &ident;
 	ident.mh_type = MH_JOIN;
	strcpy (ident.mh_name, group);
/*
 * Send it out.
 */
	msg_xf_ack (&msg);
}






void
msg_quit (group)
char *group;
/*
 * Quit a message group.
 * Entry:
 *	A connection to the message server exists.
 *	GROUP	is the name of the group to quit.
 * Exit:
 *	We are no longer a member of GROUP.
 */
{
	struct message msg;
	struct mh_ident ident;
/*
 * Put together the QUIT message.
 */
	msg.m_proto = MT_MESSAGE;
	msg.m_seq = Seq++;
	msg.m_flags = 0;
	msg.m_len = sizeof (ident);
	msg.m_data = (char *) &ident;
 	ident.mh_type = MH_QUIT;
	strcpy (ident.mh_name, group);
/*
 * Send it out.
 */
	msg_xf_ack (&msg);
}






int
msg_await ()
/*
 * Wait for something to happen.
 */
{
	fd_set fds;
	int nsel, fd;

	for (;;)
	{
	/*
	 * Clean out the queue.
	 */
		msg_DispatchQueued ();
	/*
	 * Wait for something.
	 */
		fds = Fd_list;
		if ((nsel = select (Max_fd + 1, &fds, 0, 0, 0)) < 0)
		{
			if (errno == EINTR) /* gdb attach can cause this */
				continue;
			printf ("Return code %d from msg select", errno);
			return (-1);
		}
	/*
	 * Now dispatch the events.
         *
         * Make sure that callback has not been removed during
         * this select (ie. within a message handler) by msg_delete_fd
	 */
		for (fd = 0; fd <= Max_fd && nsel > 0; fd++)
			if (FD_ISSET (fd, &fds) && Fd_funcs[fd])
			{
				int ret;
				nsel--;
				if (ret = (*Fd_funcs[fd]) (fd))
					return (ret);
			}
	}
}






int
msg_get_fd ()
/*
 * Return the socket file descriptor.
 */
{
	return (Msg_fd);
}




msg_DeathHandler (f)
ifptr f;
{
	Death_handler = f;
}


struct message *
msg_read (fd)
int fd;
/*
 * Read one message directly from the pipe.  The message is returned in
 * static storage -- copy it if need be!
 *
 * (4/93 jc)	The message is no longer in static storage -- that burned
 *		us just like one might expect it to.  Now, instead, one needs
 *		to call msg_free to get rid of the message returned by this 
 *		routine.
 */
{
	Message *msg = ALLOC (Message);
/*
 * Read in the message structure.
 */
 	if (msg_netread (Msg_fd, msg, sizeof (Message)) <= 0)
	{
		perror ("Message handler disconnect");
		free (msg);
		if (Death_handler)
			(*Death_handler) ();
		return (NULL);
	}
/*
 * Read the data portion
 */
	if (msg->m_len > 0)
	{
		msg->m_data = malloc (msg->m_len);
		msg_netread (Msg_fd, msg->m_data, msg->m_len);
	}
	else
		msg->m_data = 0;
	return (msg);
}




static void
msg_free (msg)
Message *msg;
/*
 * Free the storage used with this message.
 */
{
	if (msg->m_len > 0)
		free (msg->m_data);
	free (msg);
}




int
msg_incoming (fd)
int fd;
/*
 * Get an incoming message and dispatch it.
 */
{
	struct message *msg;
	int ret;
/*
 * If there is something waiting in the deferred queue, dispatch it.
 */
	if (Mq)
		return (msg_DispatchQueued ());
/*
 * Otherwise read in a message and dispatch that.
 */
	else
	{
		if (! (msg = msg_read (fd)))
			return (1);
	/*
	 * Check for a shutdown message and note it internally
	 */
		if (msg->m_proto == MT_MESSAGE && msg->m_len > 0)
		{
			struct mh_template *tm;
			tm = (struct mh_template *) msg->m_data;
			if (tm->mh_type == MH_SHUTDOWN)
				ShuttingDown = TRUE;
		}
		if (msg->m_proto >= 0 && msg->m_proto < MAXPROTO &&
				ProtoHandlers[msg->m_proto])
			ret = (*ProtoHandlers[msg->m_proto]) (msg);
		else
			ret = (*Msg_handler) (msg);
		msg_free (msg);
		return (ret);
	}
}



int
msg_poll(timeout)
int timeout; /* seconds */
/*
 * Check the message queue and poll the fd set for any pending messages and
 * handle one of them as in msg_await().  The number of seconds to block on
 * the select() is set with the 'timeout' parameter.  Either the return
 * from message handler is returned or MSG_TIMEOUT if no messages.
 */
{
	fd_set fds;
	int nsel, fd;
	struct timeval delay;

	delay.tv_sec = timeout;
	delay.tv_usec = 0;

	for (;;)
	{
	/*
	 * Clean out the queue.
	 */
		if (Mq)
			return (msg_DispatchQueued ());
	/*
	 * Wait for something.
	 */
		fds = Fd_list;
		if ((nsel = select (Max_fd + 1, &fds, 0, 0, &delay)) < 0)
		{
			if (errno == EINTR) /* gdb attach can cause this */
				continue;
			printf ("Return code %d from msg select", errno);
			return (1);
		}
		else if (nsel == 0)		/* timeout occurred */
			return (MSG_TIMEOUT);
	/*
	 * Now dispatch a single message.
	 */
		for (fd = 0; fd <= Max_fd && nsel > 0; fd++)
		{
			if (FD_ISSET (fd, &fds) && Fd_funcs[fd])
			{
				nsel--;
				return ((*Fd_funcs[fd]) (fd));
			}
		}
	}
}



int
msg_Search (proto, func, param)
int proto;
int (*func) ();
void *param;
/*
 * Look for a specific message in the incoming stream.
 * Entry:
 *	PROTO	Is the protocol type of interest.
 *	FUNC	is a function to call with the message,
 *	PARAM	is an extra parameter for FUNC.
 * 
 * msg_search will wait for the next message with the given PROTO type,
 * then pass it to FUNC.  If FUNC returns zero, msg_search will also
 * return zero.  Otherwise the message is queued, and the cycle happens
 * again with the next message with the given PROTO type.
 *
 * Three possible return values for FUNC are now checked:
 *
 *	MSG_ENQUEUE	queue this message and keep looking, same as a
 *			nonzero return in past revisions
 *	MSG_CONSUMED	a special non-zero value indicating that this
 *			message can be discarded
 *	MSG_DONE	zero, with the same meaning as before: discard
 *			the message and return zero
 *
 * These symbols are defined in message.h
 */
{
	struct mqueue *queue, *nextq, *tail = 0;
	struct message *msg;
	int action;
/*
 * First search the queue for this protocol type.
 */
	nextq = Mq;
	while (nextq)
	{
		queue = nextq;
	/*
	 * Find the next queued msg first, before attempts to remove this one
	 */
		nextq = queue->mq_next;
		if (queue->mq_msg->m_proto == proto)
		{
			action = (*func) (queue->mq_msg, param);
			if (action == MSG_DONE)
			{
				msg_RemQueue (queue);
				return (0);
			}
			else if (action == MSG_CONSUMED)
				msg_RemQueue (queue);
		/*
		 * Otherwise leave this message in the queue
		 */
		}
	}
/*
 * No such luck there.  We'll have to start reading stuff.
 */
	queue = 0;
	for (;;)
	{
		action = MSG_ENQUEUE;
	/*
	 * Get a message.
	 */
		if (! (msg = msg_read (Msg_fd)))
			return (1);
	/*
	 * If it's the desired type, call the handler.
	 */
		if (msg->m_proto == proto)
		{
			action = (*func) (msg, param);
			if (action == MSG_DONE)
				break;
		}
	/*
	 * If they munched this one, recycle it.
	 */
	 	if (action == MSG_CONSUMED)
		{
			msg_free (msg);
			continue;
		}
	/*
	 * They didn't want it, and we need to put it into the queue.
	 */
		if (! queue)
			queue = tail = msg_NewMq (msg);
		else
		{
			tail->mq_next = msg_NewMq (msg);
			tail = tail->mq_next;
		}
		tail->mq_next = 0;
	}
/*
 * We finally got out.  Add this stuff to the end of the message queue, and
 * free up the one message they consumed.
 */
 	if (queue)
	{
		if (! Mq)
			Mq = queue;
		else
			Mq_tail->mq_next = queue;
		Mq_tail = tail;
	}
	msg_free (msg);
	return (0);
}





void
msg_add_fd (fd, handler)
int fd;
ifptr handler;
/*
 * Add this file descriptor to the list of FD's that we watch.
 */
{
	Fd_funcs[fd] = handler;
	FD_SET (fd, &Fd_list);
	if (fd > Max_fd)
		Max_fd = fd;
}


void
msg_delete_fd (fd)
int fd;

/*
 * Remove this file descriptor from the list of FD's that we watch.
 */
{
	Fd_funcs[fd] = NULL;
	FD_CLR (fd, &Fd_list);
}





void
msg_send (to, type, broadcast, data, datalen)
char *to;
void *data;
int type, broadcast, datalen;
/*
 * Send out a message.
 * Entry:
 *	TO	is the name of the recipient(s) of this message.
 *	TYPE	is the protocol of this message.
 *	BROADCAST is TRUE iff this is a broadcast message.
 *	DATA	is the data to be sent.
 *	DATALEN	is the length of that data.
 * Exit:
 *	The message has been sent, or an error message has been printed
 *	if a shutdown message has already been received.
 */
{
	struct message msg;
	int nsent = 0, len;
/*
 * We shouldn't try to send anything if the handler is shutting down
 */
	if (ShuttingDown)
	{
		printf ("%s: msg_send: attempt to send message %s\n",
			Identity, "after shutdown received");
		return;
	}
/*
 * Put together the message structure.
 */
 	msg.m_proto = type;
	strcpy (msg.m_to, to);
	msg.m_flags = broadcast ? MF_BROADCAST : 0;
	msg.m_seq = Seq++;
	msg.m_len = datalen;
/*
 * Send the message structure.
 */
	if (write (Msg_fd, &msg, sizeof (struct message)) <
			sizeof (struct message))
		perror ("Message structure write");
/*
 * Now send the data, in chunks.
 */
	while (nsent < datalen)
	{
		len = ((datalen - nsent) > DCHUNK) ? DCHUNK : 
				(datalen - nsent);
		if (write (Msg_fd, (char *) data + nsent, len) < len)
			perror ("Message data write");
		nsent += len;
	}
}




void
msg_log (va_alist)
va_dcl
/*
 * Send a message to the event logger.
 */
{
	va_list args;
	char mbuf[300], *fmt;
/*
 * Print up our message.
 */
	va_start (args);
	fmt = va_arg (args, char *);
	vsprintf (mbuf, fmt, args);
	va_end (args);
/*
 * Send it to the event logger.
 */
	msg_send ("Event logger", MT_LOG, 0, mbuf, strlen (mbuf) + 1);
}



/*
 * Event logger info.  We assume that we send all messages until told
 * otherwise.
 */
static int EMask = 0xff;


void
msg_ELog (va_alist)
va_dcl
/*
 * Extended message logging.
 */
{
	struct msg_elog *el;
	static char cbuf[10000];
	va_list args;
	int flags;
	char *fmt;
/*
 * Get and/or use all of our arguments from the va_alist first
 */
	va_start (args);
	flags = va_arg (args, int);
	fmt = va_arg (args, char *);
	el = (struct msg_elog *) cbuf;
	vsprintf (el->el_text, fmt, args);
	va_end (args);
/*
 * If this message won't get logged, don't even send it.
 */
	if (! (flags & EMask))
		return;
/*
 * If we're shutting down, we print the message rather than send it
 */
	if (ShuttingDown)
	{
		printf ("%s: %s\n", Identity, el->el_text); 
		return;
	}
/*
 * Otherwise, send it.
 */
	el->el_flag = flags;
	msg_send ("Event logger", MT_ELOG, 0, el,
		sizeof (*el) + strlen (el->el_text));
}




static int
msg_ELHandler (msg)
struct message *msg;
/*
 * Intercept extended logger protocol messages and see if somebody is
 * trying to set the event mask.  Otherwise we pass them through the
 * default handler for compatibility.
 */
{
	struct msg_elog *el = (struct msg_elog *) msg->m_data;

	if (el->el_flag & EF_SETMASK)
	{
		EMask = el->el_flag & ~EF_SETMASK;
		return (0);
	}
	else
		return ((*Msg_handler) (msg));
}




/*
 * Message queue routines.
 */

static struct mqueue *
msg_NewMq (msg)
struct message *msg;
/*
 * Get a new message queue entry, with this stuff in it.
 */
{
	struct mqueue *new;
/*
 * If there is something in the free list, use it.
 */
	if (Mq_free)
	{
		new = Mq_free;
		Mq_free = Mq_free->mq_next;
	}
/*
 * Otherwise we have to allocate a new one.
 */
	else
		new = ALLOC (struct mqueue);
/*
 * Save the info and return the structure.
 */
	new->mq_msg = msg;
	return (new);
}




static void
msg_RemQueue (zap)
struct mqueue *zap;
/*
 * Remove this entry from the message queue.
 */
{
	struct mqueue *qp, *last;
/*
 * If this one is at the beginning of the queue, life is easy.
 */
	if (zap == Mq)
		Mq = Mq->mq_next;
/*
 * Otherwise we have to find it.
 */
	else
	{
	/*
	 * Search it out.
	 */
		last = Mq;
		for (qp = Mq->mq_next; qp; qp = qp->mq_next)
			if (qp == zap)
				break;
			else
				last = qp;
		if (! qp)
		{
			msg_log ("RemQueue on missing entry 0x%x", zap);
			return;
		}
	/*
	 * Remove it from the list.
	 */
	 	last->mq_next = qp->mq_next;
	}
/*
 * Free up the data area, and put this entry on the free list.
 */
	msg_free (zap->mq_msg);
	zap->mq_next = Mq_free;
	Mq_free = zap;
}






int
msg_DispatchQueued ()
/*
 * Dispatch all of the queued up messages.
 */
{
	int ret;

	while (Mq)
	{
	/*
	 * Check for a shutdown message and note it internally
	 */
		if (Mq->mq_msg->m_proto == MT_MESSAGE)
		{
			struct mh_template *tm;
			tm = (struct mh_template *) Mq->mq_msg->m_data;
			if (tm->mh_type == MH_SHUTDOWN)
				ShuttingDown = TRUE;
		}
	/*
	 * Then process the message normally
	 */
		if (Mq->mq_msg->m_proto >= 0 &&
				Mq->mq_msg->m_proto < MAXPROTO &&
				ProtoHandlers[Mq->mq_msg->m_proto])
			ret =(*ProtoHandlers[Mq->mq_msg->m_proto])(Mq->mq_msg);
		else
			ret = (*Msg_handler) (Mq->mq_msg);
		msg_RemQueue (Mq);
		if (ret)
			return (ret);
	}
	return(0);
}





static int
msg_QueryHandler (msg)
Message *msg;
/*
 * Handle query protocol messages.
 */
{
	struct mh_template *mh = (struct mh_template *) msg->m_data;

	switch (mh->mh_type)
	{
	   case MHQ_QUERY:
	   	(*Query_Handler) (msg->m_from);
		break;

	  case MHQ_QTEXT:
	  	if (QueryRoutine)
			(*QueryRoutine) (msg->m_data + sizeof (int));
		break;

	  case MHQ_QDONE:
	  	if (QueryRoutine)
			(*QueryRoutine) (NULL);
		QueryRoutine = 0;
		break;
	}
	return (0);
}





static int
msg_DefaultQH (who)
char *who;
/*
 * The default query handler -- not very informative.
 */
{
	msg_AnswerQuery (who, "No query handler defined for this process.");
	msg_FinishQuery (who);
	return (0);
}





void
msg_SendQuery (whom, routine)
char *whom;
ifptr routine;
/*
 * Send a query to this process.
 */
{
	struct mh_template mh;
	
	mh.mh_type = MHQ_QUERY;
	msg_send (whom, MT_QUERY, FALSE, &mh, sizeof (mh));
	QueryRoutine = routine;
}






void
msg_AnswerQuery (to, text)
char *to, *text;
/*
 * Send this text back to the requestor.
 */
{
	int len = sizeof (struct mh_template) + strlen (text) + 1;
	char *resp = malloc (len);
	struct mh_template *mh = (struct mh_template *) resp;

	mh->mh_type = MHQ_QTEXT;
	strcpy (resp + sizeof (struct mh_template), text);
	
	msg_send (to, MT_QUERY, FALSE, resp, len);
	free (resp);
}



void
msg_FinishQuery (to)
char *to;
/*
 * Finish out this query.
 */
{
	struct mh_template mh;
	
	mh.mh_type = MHQ_QDONE;
	msg_send (to, MT_QUERY, FALSE, &mh, sizeof (mh));
}





void
msg_SetQueryHandler (func)
int (*func) ();
/*
 * Set the query handler for this process.
 */
{
	Query_Handler = func;
}




int
msg_QueryClient (client)
char *client;
/*
 * Return TRUE iff this client exists.
 */
{
	struct mh_ident mhi;
	int reply;
/*
 * Format and send the query.
 */
	mhi.mh_type = MH_CQUERY;
	strcpy (mhi.mh_name, client);
	msg_send (MSG_MGR_NAME, MT_MESSAGE, FALSE, &mhi, sizeof (mhi));
/*
 * Now we await the reply.
 */
	msg_Search (MT_MESSAGE, msg_CQReply, &reply);
	return (reply);
}






static int
msg_CQReply (msg, reply)
struct message *msg;
int *reply;
/*
 * Search out a message reply and return it.
 */
{
	struct mh_BoolRepl *mb = (struct mh_BoolRepl *) msg->m_data;

	if (mb->mh_type == MH_CQREPLY)
	{
		*reply = mb->mh_reply;
		return (0);
	}
	return (1);
}

