/*
 * Library routines for the message system.
 */
/* $Id: msg_lib.c,v 1.1 1990-03-19 11:30:02 corbet Exp $ */
# include <stdio.h>
# include <varargs.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/un.h>
# include <sys/uio.h>
# include "message.h"

# define TRUE 1
# define FALSE 0
/*
 * The array of functions linked with file descriptors.
 */
typedef int (*ifptr) ();
static ifptr Fd_funcs[64] = { 0 };
static ifptr Msg_handler;	/* Kludge */

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
		perror ("Message server connect");
		return (FALSE);
	}
/*
 * Get the greeting message.
 */
	read (Msg_fd, &msg, sizeof (struct message));
	if (msg.m_proto != MT_MESSAGE || msg.m_len != sizeof (greet))
	{
		printf ("Funky greeting from message server: %d\n",
				msg.m_proto);
		close (Msg_fd);
		return (FALSE);
	}
	read (Msg_fd, &greet, sizeof (greet));
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
 * Done!
 */
	FD_ZERO (&Fd_list);
	FD_SET (Msg_fd, &Fd_list);
	Fd_funcs[Msg_fd] = msg_incoming;
	Msg_handler = handler;
	if (Msg_fd > Max_fd)
		Max_fd = Msg_fd;
 	return (TRUE);
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
 * Send it, and get the response back.  We assume that ACK's take no
 * extra data.  We also assume, for the moment, that no other messages
 * intervene -- a bad assumption.
 */
 	if (! writev (Msg_fd, iov, 2))
		return (FALSE);
	if (read (Msg_fd, &ret, sizeof (struct message)) <= 0)
		return (FALSE);
/*
 * Make sure we got an ACK.
 */
 	if (ret.m_proto != MT_MESSAGE || ret.m_len != sizeof (ack))
	{
		printf ("Got %d when expecting ACK!\n", ret.m_len);
		return (FALSE);
	}
	read (Msg_fd, &ack, sizeof (ack));

	return (TRUE);
}





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
	return (msg_xf_ack (&msg));
}







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
	 * Wait for something.
	 */
		fds = Fd_list;
		nsel = select (Max_fd + 1, &fds, 0, 0, 0);
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




msg_incoming (fd)
int fd;
/*
 * Get an incoming message and dispatch it.
 */
{
	struct message msg;
	char data[2048];	/* XXX */
/*
 * Read in the message, and possibly any extra text.
 */
 	if (read (Msg_fd, &msg, sizeof (struct message)) <= 0)
	{
		printf ("Message handler disconnect\n");
		return (1);
	}
	if (msg.m_len > 0)
		read (Msg_fd, data, msg.m_len);
	msg.m_data = data;
/*
 * Dispatch it to the handler.
 */
	return ((*Msg_handler) (&msg));
}





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






msg_send (to, type, broadcast, data, datalen)
char *to, *data;
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
	struct iovec iov[2];
/*
 * Put together the message structure.
 */
 	msg.m_proto = type;
	strcpy (msg.m_to, to);
	msg.m_flags = broadcast ? MF_BROADCAST : 0;
	msg.m_seq = Seq++;
	msg.m_len = datalen;
/*
 * Put together the iovec.
 */
 	iov[0].iov_base = (caddr_t) &msg;
	iov[0].iov_len = sizeof (struct message);
	iov[1].iov_base = data;
	iov[1].iov_len = msg.m_len;
/*
 * Do it.
 */
 	if (writev (Msg_fd, iov, 2) <= 0)
		perror ("Message write");
}








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

