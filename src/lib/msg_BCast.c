/*
 * Network broadcast code.
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

# include <sys/types.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <netinet/in.h>
# include <errno.h>
# include <unistd.h>

# include "defs.h"
# define MESSAGE_LIBRARY
# include "message.h"

RCSID("$Id: msg_BCast.c,v 2.8 1995-06-29 23:09:41 granger Exp $")

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
static BCConn Connections[ FD_MAP_SIZE ];



# ifdef __STDC__
	int	msg_BCHandler (int);
# else
	int	msg_BCHandler ();
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
	if (setsockopt (s,SOL_SOCKET,SO_BROADCAST,(char *)&on,sizeof(on)) < 0)
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




int
msg_BCHandler (fd)
int fd;
/*
 * Something is available on this broadcast port.
 */
{
  	int len;
	socklen_t flen = 0;
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




int
msg_PollBCast (fd)
int fd;
/*
 * Process any pending bcasts on this port without blocking.
 */
{
	fd_set fds;
	int ret;
	static struct timeval tv = { 0 , 0 };

	for (;;)
	{
	/*
	 * See if something is there.
	 */
		FD_ZERO (&fds);
		FD_SET (fd, &fds);
		if ((ret = select (fd + 1, (SelectSet *)&fds, (SelectSet *)0,
				   (SelectSet *)0, &tv)) <= 0)
			return (ret);
	/*
	 * If so, dispatch it.
	 */
		msg_BCHandler (fd);
	}
}
