/*
 * Send sound files to the audio port.
 */
static char *rcsid = "$Id: Sound.c,v 1.1 1990-11-14 17:34:04 corbet Exp $";
# include <fcntl.h>
# include <errno.h>
# include "defs.h"
# include "../config/config.h"
# include "../include/message.h"


int Message ();

main ()
{
/*
 * Hook into the message system.
 */
	if (! msg_connect (Message, "Sound"))
	{
		printf ("Unable to connect to message handler\n");
		exit (1);
	}
/*
 * Wait for things.
 */
	msg_await ();
}




Message (msg)
struct message *msg;
{
	if (msg->m_proto == MT_SOUND)
		DoSound (msg->m_data);
}




DoSound (file)
char *file;
{
	char fname[120];
	static char sbuf[2000];
	int fd, dev, len;

	sprintf (fname, "%s/audio/%s.au", LIBDIR, file);
	if ((fd = open (fname, O_RDONLY, 0)) < 0)
	{
		msg_ELog (EF_INFO, "Error %d opening sound file '%s'", errno,
			fname);
		return;
	}
	if ((dev = open ("/dev/audio", O_WRONLY, 0)) < 0)
	{
		msg_ELog (EF_INFO, "Error %d on /dev/audio", errno);
		close (fd);
		return;
	}
	while ((len = read (fd, sbuf, 2000)) > 0)
		write (dev, sbuf, len);
	close (fd);
	close (dev);
}
