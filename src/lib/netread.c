/*
 * Network reading.
 */
static char *rcsid = "$Id: netread.c,v 2.0 1991-07-18 23:15:57 corbet Exp $";

# include <errno.h>


int
msg_netread (fd, dest, len)
int fd, len;
char *dest;
/*
 * Read from this file descriptor until (1) an error occurs, or (2) the full
 * length has been read.
 */
{
	int nread = 0, status;

	while (nread < len)
	{
		if ((status = read (fd, dest + nread, len - nread)) <= 0)
			return (errno == EWOULDBLOCK ? nread : status);
		nread += status;
	}
	return (nread);
}



int
msg_XX_netread (fd, dest, len)
int fd, len;
char *dest;
/*
 * Read from this file descriptor until (1) an error occurs, or (2) the full
 * length has been read.  Brute force it with ewouldblock errors.
 */
{
	int nread = 0, status;

	while (nread < len)
	{
		if ((status = read (fd, dest + nread, len - nread)) <= 0)
		{
			if (errno == EWOULDBLOCK && nread > 0)
				continue;
			return (status);
		}
		nread += status;
	}
	return (nread);
}
