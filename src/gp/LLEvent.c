/*
 * Low-level (FD) event management routines.
 */
# include <sys/types.h>
# include <sys/time.h>
# include <errno.h>

# include "../include/defs.h"
# include "EventQueue.h"
# include "LLEvent.h"



/*
 * The master fd_set, which always holds the list of FD's that we are watching.
 */
static fd_set Mfd;
static int Mfd_width = 0;

typedef void (*vfptr) ();
static vfptr Procs[256];


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





lle_MainLoop ()
/*
 * Run the main loop.
 */
{
	int status, i;
	fd_set fds;

	for (;;)
	{
	/*
	 * Clear out the event queues.
	 */
		while (Eq_Execute ())
			;
	/*
	 * Wait for something.
	 */
		fds = Mfd;
		while ((status = select (Mfd_width + 1, &fds, 0, 0, 0)) < 0)
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
			shutdown ();
		}
	/*
	 * Deal with what came in.
	 */
		for (i = 0; status && i <= Mfd_width; i++)
			if (FD_ISSET (i, &fds))
			{
				status--;
				(*Procs[i]) (i);
			}
	}
}
