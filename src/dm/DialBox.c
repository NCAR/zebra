/*
 * Deal with the titan dial box.
 */
static char *rcsid = "$Id: DialBox.c,v 1.1 1990-06-11 15:45:17 corbet Exp $";

# ifdef titan

# include <stropts.h>
# include <fcntl.h>
# include <machine/gin.h>
# include <errno.h>


/*
 * Local data.
 */
static int Dlb_fd = 0;		/* The dialbox stream file descriptor	*/

static void dlb_Event ();




dlb_Init ()
/*
 * Initialize the dial box.
 */
{
/*
 * Open and initialize the device.
 */
	if ((Dlb_fd = open ("/dev/dials", O_RDONLY)) < 0)
	{
		msg_log ("Open error %d on /dev/dials", errno);
		return;
	}
        if (ioctl (Dlb_fd, I_POP, 0) < 0)
	{
		msg_log ("I_POP error %d on /dev/dials", errno);
		return;
	}
        if (ioctl (Dlb_fd, I_PUSH, "dlb") < 0)
	{
		msg_log ("I_PUSH error %d on /dev/dials", errno);
		return;
	}
/*
 * Now we gotta arrange to be called when things happen.
 */
	tty_watch (Dlb_fd, dlb_Event);
}




static void
dlb_event ()
/*
 * Deal with a dialbox event.
 */
{
	struct gin g;

	if (read (Dlb_fd, &g, sizeof (g)) < 0)
	{
		msg_log ("Error %d on dialbox read", errno);
		tty_nowatch (Dlb_fd);
		close (Dlb_fd);
	}
	msg_log ("Dial %d moved %d", g.gin_data[0], g.gin_data[1]);
}



# endif
