/*
 * Library routines for the message system.
 */
static char *rcsid = "$Id: msg_lib.c,v 2.0 1991-07-18 23:15:57 corbet Exp $";
# include <stdio.h>
# include <varargs.h>
# include <errno.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/un.h>
# include <sys/uio.h>
# include "../include/defs.h"
# include "message.h"

/*
 * The array of functions linked with file descriptors.
 */
typedef int (*ifptr) ();
static ifptr Fd_funcs[64] = { 0 };
static ifptr Msg_handler;	/* Kludge */
static ifptr Death_handler = 0;

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
 * The queue used for holding messages while looking for something specific.
 */
static struct mqueue
{
	struct message mq_msg;		/* The actual message		*/
	struct mqueue *mq_next;		/* Next queue entry		*/
} *Mq = 0, *Mq_free = 0, *Mq_tail;


/*
 * The list of protocol-specific handlers.
 */
# define MAXPROTO 20	/* Should last for a while	*/
static ifptr 
ProtoHandlers[MAXPROTO] = { 0 };

/*
 * Forward routines.
 */
# ifdef __STDC__
	static struct mqueue *msg_NewMq (struct message *);
	static void msg_RemQueue (struct mqueue *);
	static int msg_SrchAck (struct message *, struct mh_ack *);
	static int msg_ELHandler (struct message *);
	static int msg_PingHandler (Message *);
	static void msg_SendPID (void);
# else
	static struct mqueue *msg_NewMq ();
	static void msg_RemQueue ();
	static int msg_SrchAck ();
	static int msg_ELHandler ();
	static int msg_PingHandler ();
	static void msg_SendPID ();
# endif

/*
 * How much data we write at once.
 */
# define DCHUNK 512

/*
 * Ardent doesn't provide writev!  Berkeley my ass...
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
	strcpy (saddr.sun_path, UN_SOCKET_NAME);
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
	struct message ret;
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
			return (-1);
	/*
	 * Now dispatch the events.
	 */
		for (fd = 0; fd <= Max_fd && nsel > 0; fd++)
			if (FD_ISSET (fd, &fds))
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
 */
{
	static struct message msg;
	static char data[20480];	/* XXX */
/*
 * Read in the message, and possibly any extra text.
 */
 	if (msg_netread (Msg_fd, &msg, sizeof (struct message)) <= 0)
	{
		perror ("Message handler disconnect");
		if (Death_handler)
			(*Death_handler) ();
		return (0);
	}
	if (msg.m_len > 0)
		msg_netread (Msg_fd, data, msg.m_len);
	msg.m_data = data;
	
	return (&msg);
}




int
msg_incoming (fd)
int fd;
/*
 * Get an incoming message and dispatch it.
 */
{
	struct message *msg;
/*
 * If there is something waiting in the deferred queue, dispatch it.
 */
	if (Mq)
		msg_DispatchQueued ();
/*
 * Otherwise read in a message and dispatch that.
 */
	else
	{
		if (! (msg = msg_read (fd)))
			return (1);
		if (msg->m_proto >= 0 && msg->m_proto < MAXPROTO &&
				ProtoHandlers[msg->m_proto])
			return ((*ProtoHandlers[msg->m_proto]) (msg));
		else
			return ((*Msg_handler) (msg));
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
 */
{
	struct mqueue *queue = 0, *tail = 0;
	struct message *msg;
/*
 * First search the queue for this protocol type.
 */
	for (queue = Mq; queue; queue = queue->mq_next)
		if (queue->mq_msg.m_proto == proto &&
			(*func) (&queue->mq_msg, param) == 0)
		{
			msg_RemQueue (queue);
			return (0);
		}
/*
 * No such luck there.  We'll have to start reading stuff.
 */
	queue = 0;
	for (;;)
	{
	/*
	 * Get a message.
	 */
		if (! (msg = msg_read (Msg_fd)))
			return (1);
	/*
	 * If it's the desired type, call the handler.
	 */
		if (msg->m_proto == proto && (*func) (msg, param) == 0)
			break;
	/*
	 * For whatever reason, we don't want it.  Stash it for later.
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
 * We finally got out.  Add this stuff to the end of the message queue.
 */
 	if (queue)
	{
		if (! Mq)
			Mq = queue;
		else
			Mq_tail->mq_next = queue;
		Mq_tail = tail;
	}
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
 *	The message has been sent.
 */
{
	struct message msg;
	int nsent = 0;
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
		int len = ((datalen - nsent) > DCHUNK) ? DCHUNK : 
				(datalen - nsent);
		if (write (Msg_fd, data + nsent, len) < len)
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
msg_ELog (flags, va_alist)
va_dcl
/*
 * Extended message logging.
 */
{
	struct msg_elog *el;
	static char cbuf[10000];
	va_list args;
	char *fmt;
/*
 * If this message won't get logged, don't even send it.
 */
	if (! (flags & EMask))
		return;
/*
 * Print up our message.
 */
	el = (struct msg_elog *) cbuf;
	va_start (args);
	fmt = va_arg (args, char *);
	vsprintf (el->el_text, fmt, args);
	va_end (args);
/*
 * Send it.
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
 * Copy over the message.  We always allocate space for the message data.
 */
	new->mq_msg = *msg;
	new->mq_msg.m_data = malloc (msg->m_len);
	memcpy (new->mq_msg.m_data, msg->m_data, msg->m_len);
/*
 * Return the stuff.
 */
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
	free (zap->mq_msg.m_data);
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
		if (Mq->mq_msg.m_proto >= 0 && Mq->mq_msg.m_proto < MAXPROTO &&
				ProtoHandlers[Mq->mq_msg.m_proto])
			ret =(*ProtoHandlers[Mq->mq_msg.m_proto])(&Mq->mq_msg);
		else
			ret = (*Msg_handler) (&Mq->mq_msg);
		msg_RemQueue (Mq);
		if (ret)
			return (ret);
	}
}
