/*
 * Network reading.
 */
static char *rcsid = "$Id: netread.c,v 1.1 1990-08-01 17:04:01 corbet Exp $";




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
			return (status);
		nread += status;
	}
	return (nread);
}
