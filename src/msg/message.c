/*
 * The message handler.
 */
/* $Id: message.c,v 1.2 1990-07-08 13:02:12 corbet Exp $ */
# include <stdio.h>
# include <varargs.h>
# include <errno.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/un.h>
# include <sys/uio.h>
# ifndef titan
# include <sys/filio.h>
# endif
# include <signal.h>

# include "message.h"
# include <ui_symbol.h>

/*
 * Symbol tables.
 */
static stbl Proc_table;		/* Lookup table for processes	*/
static stbl Group_table;	/* Table for groups.		*/

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

/*
 * This structure describes a connection.
 */
struct connection 
{
	char	c_name[MAX_NAME_LEN];	/* The name of the connection	*/
	int	c_id;			/* The short ID for this conn	*/
	int	c_fd;			/* The file descriptor.		*/
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
static int Fd_count[64] = { 0 };
static int Nfd;


/*
 * Ardent doesn't provide writev!  Berkeley my ass...
 */
# ifdef titan
# define writev fcc_writev
# endif



main ()
{
	struct sockaddr_un saddr;
	int conn, nb;
	char msg[120];
	fd_set fds;
	int psig ();
	
	/* Nfd = getdtablesize (); */
/*
 * Create our symbol tables.
 */
	usy_init ();
	Proc_table = usy_c_stbl ("Process_table");
	Group_table = usy_c_stbl ("Group_table");
	signal (SIGPIPE, psig);
/*
 * Create our master socket.
 */
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
	FD_ZERO (&Allfds);
	FD_SET (M_un_socket, &Allfds);
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
	 * See if we have data coming in from somewhere.
	 */
		if (nsel)
			inc_message (nsel, &fds);
	}	
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
	conp->c_id = (Fd_count[conn]++ << 16) | conn;
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
# ifndef titan
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
 * Put together our I/O vector describing the data.
 */
	iov[0].iov_base = (caddr_t) msgp;
	iov[0].iov_len = sizeof (struct message);
	iov[1].iov_base = (caddr_t) msgp->m_data;
	iov[1].iov_len = msgp->m_len;
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
		nb = read (fd, &msg, sizeof (struct message));
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
		read (fd, data, msg.m_len);
	/*
	 * Deal with this message.
	 */
	 	dispatch (fd, &msg);
	}
}






deadconn (fd)
int fd;
/*
 * Mark this connection as being dead.
 */
{
	int clear_group ();
/*
 * Send out the notification.
 */
 	ce_disconnect (Fd_map[fd]);
/*
 * Clean up our data structures.
 */
	usy_z_symbol (Proc_table, Fd_map[fd]->c_name);
	FD_CLR (fd, &Allfds);
	usy_traverse (Group_table, clear_group, Fd_map[fd], FALSE);
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
/*
 * Send out a message saying that it's all over.
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
 * Clear out our socket and quit.
 */
	close (M_un_socket);
	unlink (UN_SOCKET_NAME);
	exit (0);
}






dispatch (fd, msg)
int fd;
struct message *msg;
/*
 * Do something about this message.
 */
{
	if (msg->m_proto == MT_MESSAGE)
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

		   default:
		   	send_log ("Funky MESSAGE type: %d\n", tm->mh_type);
			break;
		}
	}
	else
		route (fd, msg);
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
	usy_s_symbol (Proc_table, conp->c_name, SYMT_POINTER, &v);
/* 
 * Add this guy to the "everybody" group, now that he has a name.
 */
 	add_to_group (conp, "Everybody");
/*
 * Send back an acknowledge.
 */
	ack (conp, msg);
/*
 * Finally, send out the client event for those who are interested.
 */
 	ce_connect (conp);
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
	if (! usy_g_symbol (Proc_table, msg->m_to, &type, &v))
	{
		send_log ("Message from %s to unknown recipient %s",
			msg->m_from, msg->m_to);
		/* nak (fd, msg); */
		return;
	}
/*
 * Send it to them.
 */
 	conp = (struct connection *) v.us_v_ptr;
	send_msg (conp, msg);
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
 	add_to_group (conp, ident->mh_name);
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




add_to_group (conp, name)
struct connection *conp;
char *name;
/*
 * Add this connection to the group specified by the given name.
 */
{
	union usy_value v;
	int type;
	struct group *grp;
/*
 * Look up this group in our symbol table.
 */
 	if (! usy_g_symbol (Group_table, name, &type, &v))
	{
	/*
	 * No such group -- we'll have to create it.
	 */
	 	create_group (conp, name);
		return;
	}
/*
 * Make room for the new connection pointer.
 */
 	grp = (struct group *) v.us_v_ptr;
	grp->g_nprocs++;
	grp->g_procs = (struct connection **) realloc (grp->g_procs,
		grp->g_nprocs*sizeof (struct connection *));
/*
 * Add the group.
 */
 	grp->g_procs[grp->g_nprocs - 1] = conp;
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
	send_log ("Pipe signal received");
}
