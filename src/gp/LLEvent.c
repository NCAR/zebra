/*
 * Low-level (FD) event management routines.
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
# include <sys/time.h>
# include <errno.h>

# include <defs.h>
# include <message.h>
# include "GraphProc.h"
# include "EventQueue.h"
# include "LLEvent.h"

RCSID ("$Id: LLEvent.c,v 2.3 1995-06-29 23:28:53 granger Exp $")

/*
 * The master fd_set, which always holds the list of FD's that we are watching.
 */
static fd_set Mfd;
static int Mfd_width = 0;

typedef void (*vfptr) ();
static vfptr Procs[256];

static void lle_DoInput FP((int block));

void
lle_AddFD (fd, proc)
int fd;
void (*proc) ();
/*
 * Add a file descriptor to the list of those being watched.
 * Entry:
 *	FD	is the FD of interest;
 *	PROC	is the procedure to call when there is input on FD.
 */
{
	if (Mfd_width == 0)
		FD_ZERO (&Mfd);
	FD_SET (fd, &Mfd);
	Procs[fd] = proc;
	if (Mfd_width < fd)
		Mfd_width = fd;
}




void
lle_MainLoop ()
/*
 * Run the main loop.
 */
{
	int did_work = TRUE;

	for (;;)
	{
	/*
	 * Check for input, blocking if there was nothing left in the
	 * event queue.
	 */
		lle_DoInput (! did_work);
	/*
	 * Clear out the event queues.
	 */
		did_work = Eq_Execute ();
	}
}




static void
lle_DoInput (block)
int block;
/*
 * Deal with our input streams.  We will block waiting for input iff
 * "block" is true.
 */
{
	int status, i, nready;
	fd_set fds;
	static struct timeval noblock = { 0, 0 };
/*
 * Get everything from the input stream, waiting for at least one item if
 * we're blocking
 */
	status = 1;

	while (status)
	{
		msg_DispatchQueued ();
		xtEvent (0);	/* XXX */
		fds = Mfd;
		while ((status = select (Mfd_width + 1, (SelectSet *)&fds,
					 (SelectSet *)0, (SelectSet *)0,
					 block ? (struct timeval *) 0 : 
					 &noblock)) < 0)
		{
		/*
		 * Just do another select if we were stopped by
		 * a signal
		 */
			if (errno == EINTR)
				continue;
		/*
		 * Bad error, shut down
		 */
			msg_log ("Select error %d", errno);
			GPShutDown ();
		}
	/*
	 * Deal with what came in.
	 */
		nready = status;
		for (i = 0; nready && i <= Mfd_width; i++)
			if (FD_ISSET (i, &fds))
			{
				nready--;
				(*Procs[i]) (i);
			}
	/*
	 * Don't block for subsequent selects
	 */
		block = FALSE;
	}
}
