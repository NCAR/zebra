/*
 * The message handler.
 */
static char *rcsid = "$Id: message.c,v 1.5 1991-04-30 23:10:41 corbet Exp $";

# include <stdio.h>
# include <varargs.h>
# include <errno.h>
# include <netdb.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/un.h>
# include <sys/uio.h>
# include <netinet/in.h>
# include <sys/filio.h>
# include <signal.h>

# include "../include/defs.h"
# include "message.h"
# include <ui_symbol.h>

/*
 * Symbol tables.
 */
static stbl Proc_table;		/* Lookup table for processes	*/
static stbl Group_table;	/* Table for groups.		*/
static stbl Inet_table;		/* Internet connections		*/

/*
 * The master sockets for incoming connections.
 */
static int M_un_socket;		/* Unix domain socket		*/
static int M_in_socket;		/* Internet domain socket	*/

/*
 * An FD set which knows about all file descriptors that we are interested
 * in.
 */
static fd_set Allfds;
static int Port = DEFAULT_PORT;

/*
 * This structure describes a connection.
 */
struct connection 
{
	char	c_name[MAX_NAME_LEN];	/* The name of the connection	*/
	int	c_fd;			/* The file descriptor.		*/
	int	c_nsend;		/* Messages sent		*/
	int	c_bnsend;		/* Bytes sent			*/
	int	c_nrec;			/* Messages received		*/
	int	c_bnrec;		/* Bytes received		*/
	char	c_inet;			/* Internet connection		*/
};
struct connection *MH_conn;		/* Fake connection for local stuff */

/*
 * Process groups are maintained through a structure like this.
 */
struct group
{
	char	g_name[MAX_NAME_LEN];	/* The name of this group	*/
	short	g_nprocs;		/* The number of procs in this grp */
	struct connection **g_procs;	/* The array of connections.	*/
};


/*
 * This array maps file descriptors onto their associated connections.
 */
static struct connection *Fd_map[64] = { 0 };
static int Nfd;

/*
 * Who are we?
 */
char Hostname[40];

/*
 * Global statistics.
 */
static int S_nmessage = 0, S_bnmessage = 0, S_nbcast = 0, S_bnbcast = 0;
static int S_npipe = 0, S_ndisc = 0;



/*
 * Forwards.
 */
# ifdef __STDC__
	struct connection *FindRecipient (char *);
	struct connection *MakeConnection (char *);
	void	Greeting (int, struct message *);
	void	add_to_group (struct connection *, char *, struct message *);
	void	FixAddress (Message *);
	void	MsgProtocol (int, Message *);
	void	AnswerPing (int, Message *);
# else
	struct connection *FindRecipient ();
	struct connection *MakeConnection ();
	void	Greeting ();
	void	add_to_group ();
	void	FixAddress ();
	void	MsgProtocol ();
	void	AnswerPing ();
# endif




main ()
{
	int conn, nb;
	char msg[120], *host, *getenv ();
	fd_set fds;
	int psig ();
/*
 * Create our symbol tables.
 */
	usy_init ();
	Proc_table = usy_c_stbl ("Process_table");
	Group_table = usy_c_stbl ("Group_table");
	Inet_table = usy_c_stbl ("Inet_table");
	signal (SIGPIPE, psig);
/*
 * Create Unix and Internet sockets.
 */
	MakeUnixSocket ();
	MakeInetSocket ();
	if (! (host = getenv ("HOST")))
	{
		printf ("Who the hell are we?\n");
		exit (1);
	}
	strcpy (Hostname, host);
/*
 * Set up our select stuff.
 */
	FD_ZERO (&Allfds);
	FD_SET (M_un_socket, &Allfds);
	if (M_in_socket >= 0)
		FD_SET (M_in_socket, &Allfds);
/*	printf ("Master message socket: %d\n", M_un_socket); */
/*
 * Now it's loop time.
 */
	for (;;)
	{
		int nsel, fd;
	/*
	 * Do a select, waiting for something to happen.
	 */
		fds = Allfds;
		nsel = select (Nfd, &fds, (fd_set *) 0, (fd_set *) 0,
				(char *) 0);
		if (nsel < 0)
		{
			perror ("Select");
			exit (1);	/* XXX */
		}
	 	listen (M_un_socket, 5);	/* XXXX?? */
		/* printf ("Sel done, nsel %d\r\n", nsel); */
	/*
	 * If it's from our master socket, accept a new connection.
	 */
		if (FD_ISSET (M_un_socket, &fds))
		{
			new_un_connection ();
			nsel--;
		}
	/*
	 * Look for Internet connections too.
	 */
	 	if (FD_ISSET (M_in_socket, &fds))
		{
			NewInConnection ();
			nsel--;
		}
	/*
	 * See if we have data coming in from somewhere.
	 */
		if (nsel)
			inc_message (nsel, &fds);
	}	
}



MakeUnixSocket ()
/*
 * Create the Unix domain socket.
 */
{
	struct sockaddr_un saddr;

	if ((M_un_socket = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
	{
		perror ("Unable to create UNIX socket");
		exit (1);
	}
	Nfd = M_un_socket + 1;
/*
 * Bind it to it's name.
 */
 	saddr.sun_family = AF_UNIX;
	strcpy (saddr.sun_path, UN_SOCKET_NAME);
	if (bind (M_un_socket, (struct sockaddr *) &saddr,
			sizeof (struct sockaddr_un)) < 0)
	{
		perror ("Unable to bind UNIX socket");
		exit (1);
	}
/*
 * Tell the system that we want connections.
 */
 	listen (M_un_socket, 5);
}






MakeInetSocket ()
/*
 * Create our internet domain socket.
 */
{
	struct servent *service;
	struct sockaddr_in saddr;
/*
 * Try to look up our port number.
 */
	if (! (service = getservbyname (SERVICE_NAME, "tcp")))
		printf ("%s service not found, using default port\n",
				SERVICE_NAME);
	else
		Port = service->s_port;
/*
 * Get our socket.  Failures here are nonfatal, since we can, in some way,
 * get by without internet connectivity.
 */
	if ((M_in_socket = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	{
		perror ("Internet socket get");
		return;
	}
/*
 * Fill in the address info, and bind to this port.
 */
	saddr.sin_family = AF_INET;
	saddr.sin_addr.s_addr = INADDR_ANY;
	saddr.sin_port = Port;
	if (bind (M_in_socket, (struct sockaddr *) &saddr, sizeof (saddr)) < 0)
	{
		perror ("IN Socket bind");
		M_in_socket = -1;
	}
/*
 * Tell them we're ready.
 */
	listen (M_in_socket, 5);
	if (M_in_socket >= Nfd)
		Nfd = M_in_socket + 1;
}



new_un_connection ()
/*
 * Deal with an incoming unix-domain connection.
 */
{
	struct connection *conp;
	struct message msg;
	struct mh_greeting greet;
	int one = 1;
/*
 * Accept the new connection.
 */
	int conn = accept (M_un_socket, (struct sockaddr *) 0,
			(int *) 0);
	/* printf ("Accepted connection %d\n", conn); */
	if (conn < 0)
	{
		perror ("Accept error on UNIX socket");
		exit (1);
	}
/*
 * Put together a connection structure, and add it to the queue.
 */
 	conp = (struct connection *) malloc (sizeof (struct connection));
	conp->c_fd = conn;
	conp->c_nsend = conp->c_bnsend = 0;
	conp->c_nrec = conp->c_bnrec = 0;
	conp->c_inet = FALSE;
	strcpy (conp->c_name, "(Unknown)");
	Fd_map[conn] = conp;
/*
 * Add this one to our list of FD's.
 */
	FD_SET (conn, &Allfds);
	if (conn >= Nfd)
		Nfd = conn + 1;
/*
 * Mark this thing for nonblocking I/O.
 */
# ifdef notdef
 	ioctl (conn, FIONBIO, &one);
# endif
/*
 * Put together a greeting and send it out.
 */
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_to[0] = '\0';	/* No name yet */
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = 0;
	msg.m_len = sizeof (struct mh_greeting);
	msg.m_data = (char *) &greet;
	greet.mh_type = MH_GREETING;
	strcpy (greet.mh_version, "V-1.2");
	
	send_msg (conp, &msg);
}




NewInConnection ()
/*
 * Deal with an incoming internet-domain connection.
 */
{
	struct connection *conp;
	struct message msg;
	struct mh_greeting greet;
	int one = 1;
/*
 * Accept the new connection.
 */
	int conn = accept (M_in_socket, (struct sockaddr *) 0,
			(int *) 0);
	/* printf ("Accepted connection %d\n", conn); */
	if (conn < 0)
	{
		perror ("Accept error on UNIX socket");
		exit (1);
	}
/*
 * Put together a connection structure, and add it to the queue.
 */
 	conp = ALLOC (struct connection);
	conp->c_fd = conn;
	conp->c_nsend = conp->c_bnsend = 0;
	conp->c_nrec = conp->c_bnrec = 0;
	conp->c_inet = TRUE;
	strcpy (conp->c_name, "(Unknown)");
	Fd_map[conn] = conp;
/*
 * Add this one to our list of FD's.
 */
	FD_SET (conn, &Allfds);
	if (conn >= Nfd)
		Nfd = conn + 1;
/*
 * Mark this thing for nonblocking I/O.
 */
# ifdef notdef
 	ioctl (conn, FIONBIO, &one);
# endif
/*
 * Put together a greeting and send it out.
 */
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_to[0] = '\0';	/* No name yet */
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = 0;
	msg.m_len = sizeof (struct mh_greeting);
	msg.m_data = (char *) &greet;
	greet.mh_type = MH_GREETING;
	strcpy (greet.mh_version, "V-1.2");
	
	send_msg (conp, &msg);
}






send_msg (conp, msgp)
struct connection *conp;
struct message *msgp;
/*
 * Send the given message to this connection.
 */
{
	struct iovec iov[2];
/*
 * If this message is going out over the net, we need to append
 * our identity to the from field.
 */
	if (conp->c_inet)
	{
		strcat (msgp->m_from, "@");
		strcat (msgp->m_from, Hostname);
	}
/*
 * Put together our I/O vector describing the data.
 */
	iov[0].iov_base = (caddr_t) msgp;
	iov[0].iov_len = sizeof (struct message);
	iov[1].iov_base = (caddr_t) msgp->m_data;
	iov[1].iov_len = msgp->m_len;
/*
 * Keep stats.
 */
	conp->c_nrec++;
	conp->c_bnrec += msgp->m_len;
	S_nmessage++;
	S_bnmessage += msgp->m_len;
/*
 * Now write it.
 */
	if (writev (conp->c_fd, iov, 2) < 0)
	{
		deadconn (conp->c_fd);
		send_log ("Write failed for %s, errno %d", conp->c_name,
			errno);
	}
}









inc_message (nsel, fds)
int nsel;
fd_set *fds;
/*
 * Deal with an incoming message.
 */
{
	struct message msg;
	char data[4096];	/* XXX */
	int fd, nb;
/*
 * Pass through the list of file descriptors.
 */
	for (fd = 4; fd < Nfd && nsel; fd++)
	{
	/*
	 * See if there is something on this FD.
	 */
		if (! Fd_map[fd] || ! FD_ISSET (fd, fds))
			continue;
		nsel--;
	/*
	 * Pull in the message.
	 */
		nb = msg_netread (fd, &msg, sizeof (struct message));
	/*
	 * If we get nothing, the connection has been broken.
	 */
		if (nb <= 0)
		{
			deadconn (fd);
			continue;
		}
		else if (nb < sizeof (struct message))
			send_log ("Short message (%d) from %s", nb,
				Fd_map[fd]->c_name);
	/*
	 * Pull in the message text.
	 */
		msg.m_data = data;
		(void) msg_netread (fd, data, msg.m_len);
	/*
	 * Fix the dest address.
	 */
	 	FixAddress (&msg);
	/*
	 * Deal with this message.
	 */
		Fd_map[fd]->c_nsend++;
		Fd_map[fd]->c_bnsend += msg.m_len;
	 	dispatch (fd, &msg);
	}
}




void
FixAddress (msg)
Message *msg;
/*
 * Remove the "@host" from the destination, if we're that host.
 */
{
	char *at, *strrchr ();

	at = strrchr (msg->m_to, '@');
	if (at && ! strcmp (at + 1, Hostname))
		*at = '\0';
}





deadconn (fd)
int fd;
/*
 * Mark this connection as being dead.
 */
{
	int clear_group ();

	S_ndisc++;
/*
 * Send out the notification.
 */
 	ce_disconnect (Fd_map[fd]);
/*
 * Clean up our data structures.
 */
	if (Fd_map[fd]->c_inet)
		usy_z_symbol (Inet_table, Fd_map[fd]->c_name);
	else
		usy_z_symbol (Proc_table, Fd_map[fd]->c_name);
	usy_traverse (Group_table, clear_group,(int)Fd_map[fd], FALSE);
	FD_CLR (fd, &Allfds);
	free ((char *) Fd_map[fd]);
	Fd_map[fd] = 0;
	close (fd);
}




clear_group (name, type, v, conp)
char *name;
int type;
union usy_value *v;
struct connection *conp;
/*
 * Make sure that this connection is no longer part of this group.
 */
{
	int i, j;
	struct group *grp = (struct group *) v->us_v_ptr;

	for (i = 0; i < grp->g_nprocs; i++)
		if (grp->g_procs[i] == conp)
		{
			for (j = i; j < grp->g_nprocs; j++)
				grp->g_procs[j] = grp->g_procs[j+1];
			(grp->g_nprocs)--;
			break;
		}
	return (TRUE);
}





die ()
/*
 * Give up the ghost.
 */
{
	struct message msg;
	struct mh_template tmpl;
	int i;
/*
 * Send out a message saying that it's all over.  Note that we do not
 * explicitly broadcast to inet connections, or we could take down the
 * entire net.
 */
	strcpy (msg.m_to, "Everybody");
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = MF_BROADCAST;
	msg.m_seq = 0;
	msg.m_len = sizeof (tmpl);
	msg.m_data = (char *) &tmpl;
	tmpl.mh_type = MH_SHUTDOWN;
	broadcast (&msg, 0);
/*
 * Clear out our sockets and quit.
 */
	close (M_un_socket);
	unlink (UN_SOCKET_NAME);
	close (M_in_socket);
	for (i = 0; i < 64; i++)
		if (Fd_map[i])
			close (i);
	exit (0);
}






dispatch (fd, msg)
int fd;
struct message *msg;
/*
 * Do something about this message.
 */
{
/*
 * Branch out based on the protocol.  Most we just pass on through, but 
 * a couple are special.
 */
	switch (msg->m_proto)
	{
	/*
	 * MT_MESSAGE stuff is intended for us.
	 */
	   case MT_MESSAGE:
	   	MsgProtocol (fd, msg);
		break;
	/*
	 * We answer pings, but only if directed at us.
	 */
	   case MT_PING:
	   	if (! strcmp (msg->m_to, MSG_MGR_NAME))
		{
			AnswerPing (fd, msg);
			break;
		}
		/* Else fall through.... */
	/*
	 * Most stuff just gets sent through to the destination.
	 */
	   default:
		route (fd, msg);
		break;
	}
}




void
AnswerPing (fd, msg)
int fd;
Message *msg;
/*
 * Answer a ping message.
 */
{
	send_log ("Ping from %s", msg->m_from);
	strcpy (msg->m_to, msg->m_from);
	strcpy (msg->m_from, MSG_MGR_NAME);
	send_msg (Fd_map[fd], msg);
}





void
MsgProtocol (fd, msg)
int fd;
Message *msg;
/*
 * Deal with an MT_MESSAGE message.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;

	switch (tm->mh_type)
	{
	/*
	 * Some process identifying itself.
	 */
	   case MH_IDENTIFY:
		identify (fd, msg);
		break;
	/*
	 * The greeting from an internet connection we made before.
	 */
	   case MH_GREETING:
		Greeting (fd, msg);
		break;
	/*
	 * If they want us to die, we'll go along with it...
	 */
	   case MH_DIE:
		die ();
		break;
	/*
	 * Join a process group.
	 */
	   case MH_JOIN:
		join (fd, msg);
		break;
	/*
	 * Somebody wants statistics.
	 */
	   case MH_STATS:
		Stats (Fd_map[fd]);
		break;

	   default:
		send_log ("Funky MESSAGE type: %d\n", tm->mh_type);
		break;
	}
}






identify (fd, msg)
int fd;
struct message *msg;
/*
 * Deal with an identify request.
 */
{
	struct connection *conp = Fd_map[fd];
	union usy_value v;
	struct mh_ident *ident = (struct mh_ident *) msg->m_data;
/*
 * Make sure this person isn't changing his name.
 */
 	if (strcmp (conp->c_name, "(Unknown)"))
	{
		send_log ("Attempted name change %s to %s", conp->c_name,
			ident->mh_name);
		/* nak (fd, msg); */
		return;
	}
/*
 * Fix things up.
 */
 	strcpy (conp->c_name, ident->mh_name);
	v.us_v_ptr = (char *) conp;
	usy_s_symbol (conp->c_inet ? Inet_table : Proc_table, conp->c_name,
				SYMT_POINTER, &v);
/* 
 * Add this guy to the "everybody" group, now that he has a name, but only
 * if it's a local connection.
 */
	if (! conp->c_inet)
	{
	 	add_to_group (conp, "Everybody", 0);
		ack (conp, msg);
	}
/*
 * Finally, send out the client event for those who are interested.
 */
 	ce_connect (conp);
}





void
Greeting (fd, msg)
int fd;
struct message *msg;
/*
 * Deal with an internet greeting.
 */
{
	struct connection *conp = Fd_map[fd];
	char *strrchr (), *at;
/*
 * See to it that we agree with the remote machine as to their name.
 */
	at = strrchr (msg->m_from, '@');
	if (! at || strcmp (conp->c_name, at))
		send_log ("IN machine %s thinks its %s!", conp->c_name, at);
}




route (fd, msg)
int fd;
struct message *msg;
/*
 * Route this message on through to its destination.
 */
{
	union usy_value v;
	int type;
	struct connection *conp;
/*
 * Copy in the REAL sender of the message.
 */
	if (! Fd_map[fd]->c_inet)
	 	strcpy (msg->m_from, Fd_map[fd]->c_name);
/*
 * If this is a broadcast, deal with it separately.
 */
 	if (msg->m_flags & MF_BROADCAST)
	{
		broadcast (msg, Fd_map[fd]);
		return;
	}
/*
 * Look up the recipient.
 */
	if (conp = FindRecipient (msg->m_to))
		send_msg (conp, msg);
}





struct connection *
FindRecipient (recip)
char *recip;
/*
 * Try to find the connection to send this message through.
 */
{
	int type;
	SValue v;
	char *at, *strrchr ();
/*
 * See if this is a local connection; if so, we just look for somebody
 * we know directly.
 */
	if (! (at = strrchr (recip, '@')))
	{
		if (! usy_g_symbol (Proc_table, recip, &type, &v))
			send_log ("Message to unknown recipient %s", recip);
		return ((struct connection *) v.us_v_ptr);
	}
/*
 * If this is an already-established Internet connection, we ship things
 * out that way.
 */
	if (usy_g_symbol (Inet_table, at + 1, &type, &v))
		return ((struct connection *) v.us_v_ptr);
/*
 * Otherwise we gotta make the connection ourselves.
 */
	return (MakeConnection (at + 1));
}





struct connection *
MakeConnection (host)
char *host;
/*
 * Make a connection to this host.
 */
{
	int sock;
	struct sockaddr_in addr;
	struct hostent *hp;
	struct connection *conn;
	struct mh_ident ident;
	struct message msg;
	SValue v;
/*
 * Look up the name of the host to connect to.
 */
	if (! (hp = gethostbyname (host)))
	{
		send_log ("Attempt to connect to unknown host %s", host);
		return (0);
	}
/*
 * We need a socket to do this with.
 */
	if ((sock = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	{
		send_log ("Error %d getting new inet socket", errno);
		return (0);
	}
/*
 * Fill in the sockaddr structure, and try to make the connection.
 */
	memcpy (&addr.sin_addr, hp->h_addr, hp->h_length);
	addr.sin_family = AF_INET;
	addr.sin_port = Port;
	if (connect (sock, (struct sockaddr *) &addr, sizeof (addr)) < 0)
	{
		send_log ("Error %d connecting to host %s", errno, host);
		close (sock);
		return (0);
	}
/*
 * Now we're confident enough to allocate a new connection structure and
 * fill it in.
 */
	conn = ALLOC (struct connection);
	conn->c_fd = sock;
	conn->c_nsend = conn->c_bnsend = conn->c_nrec = conn->c_bnrec = 0;
	conn->c_inet = TRUE;
	strcpy (conn->c_name, host);
	v.us_v_ptr = (char *) conn;
	usy_s_symbol (Inet_table, host, SYMT_POINTER, &v);
/*
 * Shove an identify down the pipe.
 */
	ident.mh_type = MH_IDENTIFY;
	strcpy (ident.mh_name, Hostname);
	sprintf (msg.m_from, "%s@%s", MSG_MGR_NAME, Hostname);
	sprintf (msg.m_to, "%s@%s", MSG_MGR_NAME, host);
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = 0;
	msg.m_len = sizeof (ident);
	msg.m_data = (char *) &ident;
	send_msg (conn, &msg);
/*
 * Add this one to our list of FD's.
 */
	FD_SET (sock, &Allfds);
	if (sock >= Nfd)
		Nfd = sock + 1;
	Fd_map[sock] = conn;
/*
 * Rather than wait for the greeting, just return the connection now.
 */
	return (conn);
}




join (fd, msg)
int fd;
struct message *msg;
/*
 * This process wants to join a new group.  Let's let them.
 */
{
	struct mh_ident *ident = (struct mh_ident *) msg->m_data;
	struct connection *conp = Fd_map[fd];
/*
 * Do the actual addition.
 */
 	add_to_group (conp, ident->mh_name, msg);
/*
 * Send back an ack.
 */
 	ack (conp, msg);
/*
 * Send out the event.
 */
 	ce_join (conp, ident->mh_name);
}





ack (conp, msg)
struct connection *conp;
struct message *msg;
/*
 * Acknowledge this message back to the sender.
 */
{
	struct mh_ack ack;
	struct message out;
/*
 * Just fill it in.
 */
	strcpy (out.m_to, msg->m_from);
	strcpy (out.m_from, MSG_MGR_NAME);
	out.m_proto = MT_MESSAGE;
	out.m_len = sizeof (ack);
	out.m_data = (char *) &ack;
	ack.mh_type = MH_ACK;
	ack.mh_number = msg->m_seq;
/*
 * ...and send it.
 */
 	send_msg (conp, &out);
}



void
add_to_group (conp, name, msg)
struct connection *conp;
char *name;
struct message *msg;
/*
 * Add this connection to the group specified by the given name.
 */
{
	union usy_value v;
	int type, i;
	struct group *grp;
	char *at, *strrchr ();
	struct connection *remote;
/*
 * See if we're dealing with a remote group here.
 */
	if (at = strrchr (name, '@'))
		*at++ = '\0';
/*
 * Look up this group in our symbol table.
 */
 	if (! usy_g_symbol (Group_table, name, &type, &v))
	/*
	 * No such group -- we'll have to create it.
	 */
	 	create_group (conp, name);
/*
 * Add this entry.
 */
	else
	{
	/*
	 * Make sure this connection is not already a member.  That can
	 * happen easily with inet connections.
	 */
	 	grp = (struct group *) v.us_v_ptr;
		for (i = 0; i < grp->g_nprocs; i++)
			if (grp->g_procs[i] == conp)
				break;
	/*
	 * Add this one if necessary.
	 */
		if (i >= grp->g_nprocs)	/* Not there yet */
		{
			grp->g_nprocs++;
			grp->g_procs = (struct connection **)
				realloc (grp->g_procs,
				grp->g_nprocs*sizeof (struct connection *));
		 	grp->g_procs[grp->g_nprocs - 1] = conp;
		}
	}
/*
 * If this is a remote group join request, forward it on to the remote machine.
 */
	if (at)
	{
		sprintf (msg->m_to, "%s@%s", MSG_MGR_NAME, at);
		if (remote = FindRecipient (msg->m_to))
			send_msg (remote, msg);
	}
}





create_group (conp, name)
struct connection *conp;
char *name;
/*
 * Create a new group, with this connection as a member.
 */
{
	struct group *grp;
	union usy_value v;
/*
 * Get the new structure and fill it in.
 */
 	grp = (struct group *) malloc (sizeof (struct group));
	grp->g_procs = (struct connection **)
			malloc (sizeof (struct connection *));
	grp->g_procs[0] = conp;
	grp->g_nprocs = 1;
	strcpy (grp->g_name, name);
/*
 * Add it to our symbol table.
 */
 	v.us_v_ptr = (char *) grp;
	usy_s_symbol (Group_table, name, SYMT_POINTER, &v);
}






broadcast (msg, conp)
struct message *msg;
struct connection *conp;
/*
 * Broadcast a message to a whole group of processes.
 */
{
	struct group *grp;
	int i, type;
	union usy_value v;
/*
 * Lookup the group.
 */
 	if (! usy_g_symbol (Group_table, msg->m_to, &type, &v))
		return;
	grp = (struct group *) v.us_v_ptr;
/*
 * Stats.
 */
	S_nbcast++;
	S_bnbcast += msg->m_len;
/*
 * Now step through the connections, sending to each, but being careful
 * not to send to the originator of the message.
 */
 	for (i = 0; i < grp->g_nprocs; i++)
		if (grp->g_procs[i] != conp)
			send_msg (grp->g_procs[i], msg);
}







ce_connect (conp)
struct connection *conp;
/*
 * Send out a client event noting that this client has connected.
 */
{
	struct message msg;
	struct mh_client cl;
/*
 * Fill in our message.
 */
	msg.m_proto = MT_MESSAGE;
	strcpy (msg.m_from, MSG_MGR_NAME);
	strcpy (msg.m_to, "Client events");
	msg.m_seq = conp->c_fd;
	msg.m_flags = MF_BROADCAST;
	msg.m_len = sizeof (cl);
	msg.m_data = (char *) &cl;
	strcpy (cl.mh_client, conp->c_name);
	cl.mh_type = MH_CLIENT;
	cl.mh_evtype = MH_CE_CONNECT;
	cl.mh_inet = conp->c_inet;
/*
 * Broadcast it.
 */
 	broadcast (&msg, conp);
}




ce_disconnect (conp)
struct connection *conp;
/*
 * Send out a client disconnect message.
 */
{
	struct message msg;
	struct mh_client cl;
/*
 * Fill in our message.
 */
	msg.m_proto = MT_MESSAGE;
	strcpy (msg.m_from, MSG_MGR_NAME);
	strcpy (msg.m_to, "Client events");
	msg.m_seq = conp->c_fd;
	msg.m_flags = MF_BROADCAST;
	msg.m_len = sizeof (cl);
	msg.m_data = (char *) &cl;
	strcpy (cl.mh_client, conp->c_name);
	cl.mh_type = MH_CLIENT;
	cl.mh_evtype = MH_CE_DISCONNECT;
	cl.mh_inet = conp->c_inet;
/*
 * Broadcast it.
 */
 	broadcast (&msg, conp);
}




ce_join (conp, group)
struct connection *conp;
char *group;
/*
 * Send out a client group join message.
 */
{
	struct message msg;
	struct mh_client cl;
/*
 * Fill in our message.
 */
	msg.m_proto = MT_MESSAGE;
	strcpy (msg.m_from, MSG_MGR_NAME);
	strcpy (msg.m_to, "Client events");
	msg.m_seq = conp->c_fd;
	msg.m_flags = MF_BROADCAST;
	msg.m_len = sizeof (cl);
	msg.m_data = (char *) &cl;
	strcpy (cl.mh_client, conp->c_name);
	strcpy (cl.mh_group, group);
	cl.mh_type = MH_CLIENT;
	cl.mh_evtype = MH_CE_JOIN;
	cl.mh_inet = conp->c_inet;
/*
 * Broadcast it.
 */
 	broadcast (&msg, conp);
}





send_log (va_alist)
va_dcl
/*
 * Send a message to the event logger.
 */
{
	char mbuf[400];
	va_list args;
	char *fmt;
	struct message msg;
	int len, type;
	union usy_value v;
	struct connection *conp;
/*
 * If there is no event logger, there is no point.
 */
 	if (! usy_g_symbol (Proc_table, "Event logger", &type, &v))
		return;
	conp = (struct connection *) v.us_v_ptr;
/*
 * Format the message.
 */
 	va_start (args);
	fmt = va_arg (args, char *);
	vsprintf (mbuf, fmt, args);
	va_end (args);
/*
 * Now send this message out.
 */
	len = strlen (mbuf) + 1;
	msg.m_proto = MT_LOG;
	strcpy (msg.m_to, "Event logger");
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_flags = 0;
	msg.m_len = len;
	msg.m_data = mbuf;
	send_msg (conp, &msg);
}





psig ()
/*
 * Cope with pipe signals.  We don't have to actually *do* anything, since
 * the dead process will be noted later.
 */
{
	S_npipe++;
	send_log ("Pipe signal received");
}





Stats (conp)
struct connection *conp;
/*
 * Send statistics back to this guy.
 */
{
	char string[200];
	struct message msg;
	int i;
/*
 * Fill in a message structure to be used in sending back the data.
 */
	strcpy (msg.m_to, conp->c_name);
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = 0;
	msg.m_data = string;
/*
 * Header info.
 */
	sprintf (string, "%d messages sent, %d bytes (%d/%d broadcast)",
		S_nmessage, S_bnmessage, S_nbcast, S_bnbcast);
	msg.m_len = strlen (string) + 1;
	send_msg (conp, &msg);
	sprintf (string, "\t%d disconnects, with %d pipe signals",
		S_ndisc, S_npipe);
	msg.m_len = strlen (string) + 1;
	send_msg (conp, &msg);
/*
 * Now go through and report on each connection.
 */
	for (i = 0; i < Nfd; i++)
	{
		struct connection *c;
		if (! (c = Fd_map[i]))
			continue;
		sprintf (string, " %s '%s' on %d, send %d/%d, rec %d/%d",
			c->c_inet ? "Internet" : "Process ",
			c->c_name, i, c->c_nsend, c->c_bnsend,
			c->c_nrec, c->c_bnrec);
		msg.m_len = strlen (string) + 1;
		send_msg (conp, &msg);
	}
/*
 * Send the EOF and quit.
 */
	msg.m_len = 0;
	send_msg (conp, &msg);
}
