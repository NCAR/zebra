/*
 * Network broadcast code.
 */
static char *rcsid = "$Id: msg_BCast.c,v 1.2 1991-06-14 22:11:41 corbet Exp $";

# include "../include/defs.h"
# include "message.h"

# include <sys/types.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <netinet/in.h>
# include <errno.h>


/*
 * An open UDP socket looks like this:
 */
typedef struct _BCConn
{
	int	c_fd;			/* socket file descriptor	*/
	struct sockaddr_in c_sin;	/* Socket address		*/
	int	c_port;			/* The port in use		*/
	int	(*c_handler) ();	/* Handler for incoming data	*/
} BCConn;

/*
 * The array of such connections, indexed by file descriptor.
 */
# define MAXCONN 64
static BCConn Connections[MAXCONN];



# ifdef __STDC__
	static int	msg_BCHandler (int);
# else
	static int	msg_BCHandler ();
# endif



int
msg_BCSetup (addr, port, handler)
int addr, port;
int (*handler) ();
/*
 * Set up a broadcast socket.
 */
{
	int s, on = 1;
	BCConn *cp;
/*
 * Get a socket to do all this with.
 */
	if ((s = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Unable to get BCast socket (%d)",errno);
		return (-1);
	}
	cp = Connections + s;
	cp->c_fd = s;
/*
 * Turn on broadcasting with this socket.
 */
	if (setsockopt (s, SOL_SOCKET, SO_BROADCAST, &on, sizeof (on)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d enabling broadcast", errno);
		close (s);
		return (-1);
	}
/*
 * Bind the socket to our port.
 */
	cp->c_sin.sin_family = AF_INET;
	cp->c_sin.sin_addr.s_addr = htonl (INADDR_ANY);
	cp->c_sin.sin_port = htons (port);
	if (bind (s, (struct sockaddr *) &cp->c_sin, sizeof (cp->c_sin)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d binding BC to port %d", errno,
					port);
		close (s);
		return (-1);
	}
/*
 * Store the real broadcast address we will use, and be done.
 */
	cp->c_sin.sin_addr.s_addr = htonl (addr);
	cp->c_handler = handler;
	cp->c_port = port;
	msg_add_fd (s, msg_BCHandler);
	return (s);
}






void
msg_BCast (s, data, len)
int s, len;
void *data;
/*
 * Send out a broadcast message over the net.
 */
{
	BCConn *cp = Connections + s;

	if (sendto (s, data, len, 0, (struct sockaddr *) &cp->c_sin,
			sizeof (struct sockaddr_in)) < 0)
		msg_ELog (EF_PROBLEM, "Error %d sending broadcast", errno);
}





msg_BCHandler (fd)
int fd;
/*
 * Something is available on this broadcast port.
 */
{
	int len, flen = 0;
	static char rbuf[MAXBCAST];
	BCConn *cp = Connections + fd;
/*
 * Pull in the message.
 */
	if ((len = recvfrom (fd, rbuf, MAXBCAST, 0, 0, &flen)) < 0)
		msg_ELog (EF_PROBLEM, "Error %d reading bcast", errno);
	else
		(*cp->c_handler) (cp->c_port, rbuf, len);
	return (0);
}





msg_PollBCast (fd)
int fd;
/*
 * Process any pending bcasts on this port without blocking.
 */
{
	fd_set fds;
	static struct timeval tv = { 0 , 0 };

	for (;;)
	{
	/*
	 * See if something is there.
	 */
		FD_ZERO (&fds);
		FD_SET (fd, &fds);
		if (select (fd + 1, &fds, 0, 0, &tv) <= 0)
			return;
	/*
	 * If so, dispatch it.
	 */
		msg_BCHandler (fd);
	}
}
