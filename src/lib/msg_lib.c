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
# include <signal.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/socket.h>
# include <sys/un.h>
# include <sys/uio.h>

# include "defs.h"
# define MESSAGE_LIBRARY	/* to get netread prototypes */
# include "message.h"

RCSID ("$Id: msg_lib.c,v 2.29 1995-05-05 17:42:32 granger Exp $")

/*
 * The array of functions linked with file descriptors.
 */
static ifptr Fd_funcs[ FD_MAP_SIZE ] = { 0 };
static ifptr Msg_handler = 0;	/* Kludge */
static ifptr Death_handler = 0;
static ifptr Query_Handler = 0;
static ifptr QueryRoutine = 0;

/*
 * The permanent list of file descriptors.
 */
static fd_set Fd_list;
static int Max_fd = 0;
static int Msg_fd = -1;

/*
 * Our sequence number.
 */
static int Seq = 0;

/*
 * State info.  At the moment, this is only used to prevent sending messages,
 * especially log messages, after we've received a SHUTDOWN message or
 * before msg_connect has been called.
 */
static int ShuttingDown = 0;
static int NoConnection = 1; 	/* True when in a non-connected state */

/*
 * Event logger info.  Initialize the masks so that everything is printed
 * and nothing is sent until we actually have a connection.  This way
 * library routines can print error messages even when the program is not
 * using the message library or a message connection.  These are reversed
 * when msg_connect succeeds.
 */
static int PrintMask = (EF_ALL & ~(EF_DEVELOP));
static int SendMask = 0x00;
static char LogBuffer[1024];	/* for msg_log and msg_ELog to share */

/*
 * Echo mode: a test mode where we first send all of our messages to 
 * ourselves and verify correct transmission before sending the message
 * to the intended recipient.  It is enabled by the ZEBRA_TEST_ECHO_MESSAGE
 * environment variable.
 */
static int EchoMode = 0;
static char Identity[ sizeof(struct mh_ident) ] = "unknown";

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
static ifptr 
ProtoHandlers[MT_MAX_PROTO] = { 0 };

/*
 * Forward routines.
 */
static struct mqueue *msg_NewMq FP ((struct message *));
static void msg_RemQueue FP ((struct mqueue *));
static void msg_AddQueue FP ((struct message *msg));
static int msg_SrchAck FP ((struct message *, struct mh_ack *));
static int msg_ELHandler FP ((struct message *));
static int msg_PingHandler FP ((Message *));
static int msg_QueryHandler FP ((Message *));
static int msg_DefaultQH FP ((char *));
static void msg_SendPID FP ((void));
static int msg_CQReply FP ((struct message *, int *));
static void msg_free FP ((Message *));
static int msg_HandleProto FP ((Message *msg, int n, int *plist, int *ret));
static int msg_write FP ((struct message *msg));
static int msg_echo FP ((struct message *msg, void *param));
static void msg_abort FP ((void));
static int msg_xf_ack FP ((struct message *msg));
static void msg_PError FP ((/* char *s, ... */));
static void msg_SendLog FP ((struct msg_elog *el));
static int msg_netwrite FP ((int, void *, int));

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



int
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
 *
 * If handler is passed as NULL, then we don't actually connect anywhere
 * or open any sockets.  Just set our identity and NoConnection mode.
 */
{
	struct sockaddr_un saddr;
	struct message msg;
	struct mh_greeting greet;
	struct mh_ident id;
	char *sn = getenv ("ZEB_SOCKET");
/*
 * Preserve our identity
 */
	strcpy (Identity, ident);
	if (! handler)
	{
		NoConnection = 1;
		/* 
		 * FALSE implies no connection, which is accurate.
		 */
		return (0);
	}
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
		if (errno != ENOENT /* no such file or directory */ && 
		    errno != ECONNREFUSED /* connection refused */ &&
		    errno != ENXIO /* no such device or address (solaris) */)
			perror ("Message server connect");
		return (FALSE);
	}
/*
 * Get the greeting message.
 */
	msg_netread (Msg_fd, (char *)&msg, sizeof (struct message));
	if (msg.m_proto != MT_MESSAGE || msg.m_len != sizeof (greet))
	{
		msg_PError ("Funky greeting from message server: %d",
			    msg.m_proto);
		close (Msg_fd);
		return (FALSE);
	}
	msg_netread (Msg_fd, (char *)&greet, sizeof (greet));
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
		msg_PError ("message manager failed to acknowledge IDENTIFY");
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
 * Establish the event logger handler.
 */
	msg_AddProtoHandler (MT_ELOG, msg_ELHandler);
	msg_AddProtoHandler (MT_CPING, msg_PingHandler);
	msg_AddProtoHandler (MT_QUERY, msg_QueryHandler);
	Query_Handler = msg_DefaultQH;
/*
 * Looks we're going to succeed with the connection, so note as much
 * and re-initialize event logging masks.
 */
	NoConnection = 0;
	PrintMask = 0x00;
	SendMask = (EF_ALL & (~EF_DEVELOP));
	msg_SendPID ();
/*
 * Now that everything's hunky-dory, see if we should operate in echo
 * mode.
 */
	if (getenv ("ZEBRA_TEST_ECHO_MESSAGE"))
	{
		msg_ELog (EF_INFO, "echo testing enabled");
		EchoMode = 1;
	}
 	return (TRUE);
}



void
msg_disconnect ()
/*
 * Close all file descriptors and sockets and reset all of our
 * handlers.  Make it look like we've never been connected.
 */
{
	int i;
	/*
	 * Free the queued messages
	 */
	while (Mq)
		msg_RemQueue (Mq);
	/*
	 * Close the message socket
	 */
	close (Msg_fd);
	/*
	 * Reset global variables and function tables
	 */
	for (i = 0; i < MT_MAX_PROTO; ++i)
		ProtoHandlers[i] = 0;
	for (i = 0; i < FD_MAP_SIZE; ++i)
		Fd_funcs[i] = 0;
	Msg_handler = 0;
	Death_handler = 0;
	Query_Handler = 0;
	QueryRoutine = 0;
	FD_ZERO (&Fd_list);
	Max_fd = 0;
	Msg_fd = -1;
	Seq = 0;

	ShuttingDown = 0;
	NoConnection = 0;
	EchoMode = 0;
	Identity[0] = 0;
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
	msg->m_len = 0;
	(void) msg_write (msg);
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
	if (proto >= 0 && proto < MT_MAX_PROTO)
		ProtoHandlers[proto] = handler;
}




static int
msg_xf_ack (msg)
struct message *msg;
/*
 * Send a message which expects a returning acknowledgement.
 */
{
	struct mh_ack ack;
#ifdef notdef
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
#endif
	if (msg_write (msg))
	{
		msg_Search (MT_MESSAGE, msg_SrchAck, &ack);
		return (TRUE);
	}
	else
		return (FALSE);
}





static int
msg_SrchAck (msg, ack)
struct message *msg;
struct mh_ack *ack;
/*
 * Search out an ack.
 */
{
	struct mh_template *mt = (struct mh_template *) msg->m_data;

	if (mt->mh_type == MH_ACK)
	{
		*ack = * (struct mh_ack *) msg->m_data;
		return (0);
	}
	else if (msg->m_len == sizeof (struct mh_ack))
	{
		msg_PError ("%s: accepting ack w/out MH_ACK type", 
			    "msg_SrchAck bug");
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
	if (! msg_xf_ack (&msg))
		msg_ELog (EF_PROBLEM, "failed to join group %s", group);
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
	if (! msg_xf_ack (&msg))
		msg_ELog (EF_PROBLEM, "failed to quit group %s", group);
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
		if ((nsel = select (Max_fd + 1, (SelectSet *)&fds, 
				    (SelectSet *)0, (SelectSet *) 0, 0)) < 0)
		{
			if (errno == EINTR) /* gdb attach can cause this */
				continue;
			msg_PError ("Return code %d from msg select", errno);
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
				if ((ret = (*Fd_funcs[fd]) (fd)))
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



const char *
msg_myname ()
/*
 * Return our message name
 */
{
	return (Identity);
}



void
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
 	if (msg_netread (Msg_fd, (char *)msg, sizeof (Message)) <= 0)
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




static int
msg_HandleProto (msg, nproto, protolist, ret)
Message *msg;
int nproto;
int *protolist;
int *ret;	/* return value of message handler, if called */
/*
 * Return nonzero if the message is handled, else return zero.  The
 * message must belong to one of the protocols listed in the proto array,
 * else protolist must be NULL or nproto equal to zero.
 */
{
	int i;

	if (protolist != NULL && nproto != 0)
	{
		/*
		 * Search the protocol list for this protocol
		 */
		for (i = 0; i < nproto; ++i)
			if (msg->m_proto == protolist[i])
				break;
		if (i >= nproto)
			return (0);
	}
	/*
	 * Check for a shutdown message and note it internally
	 */
	if ((msg->m_proto == MT_MESSAGE) && (msg->m_len > 0))
	{
		struct mh_template *tm;
		tm = (struct mh_template *) msg->m_data;
		if (tm->mh_type == MH_SHUTDOWN)
			ShuttingDown = TRUE;
	}
	if (msg->m_proto >= 0 && msg->m_proto < MT_MAX_PROTO &&
	    ProtoHandlers[msg->m_proto])
			*ret =(*ProtoHandlers[msg->m_proto])(msg);
		else
			*ret = (*Msg_handler) (msg);
	return (1);
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
		msg_HandleProto (msg, 0, NULL, &ret);
		msg_free (msg);
		return (ret);
	}
}



int
msg_poll (timeout)
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
		if ((nsel = select (Max_fd + 1, (SelectSet *)&fds, 
			    (SelectSet *)0, (SelectSet *)0, &delay)) < 0)
		{
			if (errno == EINTR) /* gdb attach can cause this */
				continue;
			msg_PError ("Return code %d from msg select", errno);
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
msg_PollProto (timeout, nproto, protolist)
int timeout; 	/* seconds */
int nproto;	/* number of protocols in proto array */
int *protolist;	/* array of protocols to handle */
/*
 * Check the message queue and poll the fd set for any pending messages of
 * the specified protocols.  Handle one queued message if any, else handle
 * one message from an active fd if a message is read before timeout
 * occurs.  The number of seconds to block on the select() is set with the
 * 'timeout' parameter.  Either the return from message handler is returned
 * or MSG_TIMEOUT if no messages.  The number of protocols to check is
 * 'nproto' and protolist is the list of the protocols.  If nproto is zero
 * or protolist is NULL, we'll accept messages from any protocol (equivalent
 * to calling msg_poll).
 */
{
	fd_set fds;
	int nsel, fd;
	struct timeval delay;
	struct mqueue *mq;
	struct message *msg;
	int ret;

	delay.tv_sec = timeout;
	delay.tv_usec = 0;

	/*
	 * Check the queue for a message of one of the specified protocols.
	 * This would be best handled by separate protocol queues, oh well...
	 */
	mq = Mq;
	while (mq)
	{
		if (msg_HandleProto (mq->mq_msg, nproto, protolist, &ret))
		{
			msg_RemQueue (mq);
			return (ret);
		}
		mq = mq->mq_next;
	}
	/*
	 * No queued messages, so poll for something.
	 */
	for (;;)
	{
		fds = Fd_list;
		if ((nsel = select (Max_fd + 1, (SelectSet *)&fds, 
			    (SelectSet *)0, (SelectSet *)0, &delay)) < 0)
		{
			if (errno == EINTR) /* gdb attach can cause this */
				continue;
			msg_PError ("Return code %d from msg select", errno);
			return (1);
		}
		else if (nsel == 0)		/* timeout occurred */
			return (MSG_TIMEOUT);
	/*
	 * Now dispatch a single message of one of the protocols.  Messages
	 * on fd's other than the Msg_fd are just sent to that fd handler.
	 * The handler will have to implement it's own queueing for that fd.
	 */
		for (fd = 0; fd <= Max_fd && nsel > 0; fd++)
		{
			if (FD_ISSET (fd, &fds) && Fd_funcs[fd])
			{
				nsel--;
				if (fd == Msg_fd)
				{
					if (! (msg = msg_read (fd)))
						return (1);
					if (msg_HandleProto (msg, nproto,
							     protolist, &ret))
					{
						msg_free (msg);
						return (ret);
					}
					else
						msg_AddQueue (msg);
				}			
				else
					return ((*Fd_funcs[fd]) (fd));
			}
		}
	/*
	 * If we get this far it's because none of the descriptors had
	 * messages pending in the protocol list.
	 */
		return (MSG_TIMEOUT);
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



static int
msg_write (msg)
struct message *msg;
/*
 * Write the message structure and its data.
 */
{
	unsigned int nsent;
	unsigned int len;
	int fail = 0;

	if (msg_netwrite (Msg_fd, msg, sizeof (Message)) < sizeof (Message))
	{
		perror ("Message structure write");
		fail = 1;
	}
/*
 * Now send the data, in chunks.  Chunking may no longer be necessary,
 * with msg_netwrite in place, but what the heck...it works...
 */
	nsent = 0;
	while ((!fail) && (nsent < (unsigned int)msg->m_len))
	{
		len = (((unsigned int)msg->m_len - nsent) > DCHUNK) ? DCHUNK : 
				(msg->m_len - nsent);
		if (msg_netwrite (Msg_fd, ((char *)msg->m_data) + nsent, len)
				< len)
		{
			perror ("Message data write");
			fail = 1;
		}
		nsent += len;
	}
	if (fail)
	{
		msg_PError ("failed message: %s --> %s, proto %d",
			    Identity, msg->m_to, msg->m_proto);
	}
	return (! fail);
}





static int
msg_netwrite (fd, data, len)
int fd, len;
void *data;
/*
 * Do a network-reliable write.
 */
{
	int nwrote = 0;
/*
 * Things are done this way because writes can fail for a number of
 * nonfatal reasons.  Linux will even fail them with errno=0.
 */
	while (nwrote < len)
	{
		nwrote += write (fd, (char *) data + nwrote, len - nwrote);
		if (nwrote < len && errno != 0 && errno != EAGAIN &&
				errno != EINTR)
			return (-1);
	}
	return (nwrote);
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
/*
 * We shouldn't try to send anything if the handler is shutting down
 */
	if (ShuttingDown || NoConnection)
	{
		msg_PError ("%s: msg_send: attempt to send message %s",
			    Identity, (NoConnection) ? "with no connection" :
			    "after shutdown received");
		return;
	}
/*
 * Put together the message structure.
 */
 	msg.m_proto = type;
	msg.m_len = datalen;
	msg.m_data = data;
/*
 * Check for echo mode
 */
	if (EchoMode)
	{
		/*
		 * Send a copy of this message to ourselves and make
		 * sure it comes back just as we sent it.
		 */
		strcpy (msg.m_to, Identity);
		msg.m_flags = 0;
		msg.m_seq = Seq++;
		if (msg_write (&msg))
		{
			signal (SIGALRM, (void(*)(/*int*/)) msg_abort);
			alarm (30);
			msg_Search (type, msg_echo, (void *) &msg);
			alarm (0);
		}
	}
/*
 * Finish the structure for transmission to the intended recipient
 */
	strcpy (msg.m_to, to);
	msg.m_flags = broadcast ? MF_BROADCAST : 0;
	msg.m_seq = Seq++;
	(void) msg_write (&msg);
}



static void
msg_abort ()
/*
 * Uh-oh, a search for an echo failed.
 */
{
	msg_PError ("echo: expected echo never found, aborting");
	exit (99);
}



static int
msg_echo (echo, param)
struct message *echo;	/* the presumed echo message from the manager */
void *param;		/* the source of the echo */
/*
 * Look for an echo message and verify it.
 */
{
	struct message *msg = (struct message *) param;
	int datamatch;
	int seqmatch;
/*
 * See if this message compares to what we're expecting.
 * If so, we'll consider the test successful.  Do lots of mundane
 * checks, just to be sure.
 */
	if (echo->m_proto != msg->m_proto)
	{
		msg_PError ("echo: unexpected proto %i from msg_Search",
			    echo->m_proto);
		return (MSG_ENQUEUE);
	}
/*
 * Make sure the echoed message was from ourself
 */
	if (strcmp (echo->m_from, Identity))
		return (MSG_ENQUEUE);
/*
 * Compare the data
 */
	datamatch = 0;
	if (echo->m_len == msg->m_len)
	{
		datamatch = (! memcmp (echo->m_data, msg->m_data, 
				       echo->m_len));
	}
/*
 * Check the sequence number.
 */
	seqmatch = (echo->m_seq == msg->m_seq);
/*
 * If neither match, we assume this wasn't our echo.  If one of them
 * did match but not the other, then something fishy is going on.
 * Either way we finish the search.
 */
	if (!datamatch && !seqmatch)
		return (MSG_ENQUEUE);
	if (!seqmatch)
		msg_PError ("echo: proto %i, %s [echo %i, sent %i]",
			    echo->m_proto, "sequence mismatch",
			    echo->m_seq, msg->m_seq);
	if (!datamatch)
		msg_PError ("echo: proto %i, %s [echo len %i, sent len %i]",
			    echo->m_proto, "data mismatch",
			    echo->m_len, msg->m_len);
	return (MSG_DONE);
}




int
msg_ELPrintMask (mask)
int mask;
/*
 * Set the mask of events we print to the terminal, and return the
 * old mask.
 */
{
	int old = PrintMask;

	PrintMask = mask;
	return (old);
}


int
msg_ELSendMask (mask)
int mask;
/*
 * Set the mask for messages which we send down our socket.
 * This is set to zero to disable message manager logging when
 * there is no message connection.  Return the old mask.  The
 * event logger may still tell us to use a different mask.
 */
{
	int old = SendMask;

	SendMask = mask;
	return (old);
}



static void
msg_PError (va_alist)
va_dcl
/*
 * Log an error message without sending it.  Meant for within msg_send, 
 * since using msg_ELog might lead to infinite recursion.
 */
{
	va_list args;
	char *fmt;
/*
 * Format and print the message to stderr.
 */
 	va_start (args);
	fmt = va_arg (args, char *);
	vsprintf (LogBuffer, fmt, args);
	va_end (args);
	fprintf (stderr, "%s: %s\n", Identity, LogBuffer);
}




void
msg_log (va_alist)
va_dcl
/*
 * Send a message to the event logger with a default mask of EF_INFO.
 */
{
	static struct msg_elog *el = (struct msg_elog *) LogBuffer;
	va_list args;
	char *fmt;
/*
 * Print up our message.
 */
	el->el_flag = EF_INFO;
	va_start (args);
	fmt = va_arg (args, char *);
	vsprintf (el->el_text, fmt, args);
	va_end (args);
	msg_SendLog (el);
}



void
msg_ELog (va_alist)
va_dcl
/*
 * Extended message logging interface.
 */
{
	static struct msg_elog *el = (struct msg_elog *) LogBuffer;
	va_list args;
	char *fmt;
/*
 * Get and/or use all of our arguments from the va_alist first
 */
	va_start (args);
	el->el_flag = va_arg (args, int);
	fmt = va_arg (args, char *);
	vsprintf (el->el_text, fmt, args);
	va_end (args);
	msg_SendLog (el);
}




static void
msg_SendLog (el)
struct msg_elog *el;
{
/*
 * If this message won't get logged, don't bother sending it.
 */
	if (! (el->el_flag & SendMask) && ! (el->el_flag & PrintMask))
		return;
/*
 * Print the message if we're shutting down or the message matches the 
 * print mask.
 */
	if ((ShuttingDown && (el->el_flag & SendMask)) || 
	    (el->el_flag & PrintMask))
	{
		printf ("%s: %s\n", Identity, el->el_text); 
	}
/*
 * Actually send the message only if it's in the send mask, we're not
 * shutting down, and we're connected.
 */
	if (! ShuttingDown && (el->el_flag & SendMask) && ! NoConnection)
	{
		msg_send (EVENT_LOGGER_NAME, MT_ELOG, 0, el,
			  sizeof (*el) + strlen (el->el_text));
	}
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
		SendMask = el->el_flag & ~EF_SETMASK;
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
msg_AddQueue (msg)
struct message *msg;
/*
 * This is the private, internal enqueueing function.  The message is not
 * copied before being added to the queue.
 */
{
	struct mqueue *tail;

	tail = msg_NewMq (msg);
	tail->mq_next = 0;
	if (! Mq)
		Mq = tail;
	else
		Mq_tail->mq_next = tail;
	Mq_tail = tail;
}



void
msg_Enqueue (msg)
Message *msg;
/*
 * The public interface for enqueuing a message from within a handler.
 * It is assumed the handler (msg_incoming) will free the message once
 * it is handled, so this function copies the message before placing it
 * on the queue.
 */
{
	Message *copy;

	copy = ALLOC(Message);
	*copy = *msg;
	if (msg->m_len)
	{
		copy->m_data = (char *) malloc (msg->m_len);
		memcpy (copy->m_data, msg->m_data, msg->m_len);
	}
	msg_AddQueue (copy);
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
			msg_ELog (EF_PROBLEM, 
				  "RemQueue on missing entry 0x%x", zap);
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
		if ((Mq->mq_msg->m_proto == MT_MESSAGE) && 
		    (Mq->mq_msg->m_len > 0))
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
				Mq->mq_msg->m_proto < MT_MAX_PROTO &&
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

