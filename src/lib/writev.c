# ifdef titan
/*
 * Emulation for (sadly) missing readv and writev routines.
 */
# include <sys/types.h>
# include <sys/uio.h>



fcc_writev (fd, iov, niov)
int fd, niov;
struct iovec *iov;
/*
 * Vectored write.
 */
{
	int i, nwrote, total = 0;

	for (i = 0; i < niov; i++)
	{
		if ((nwrote = write (fd, iov[i].iov_base, iov[i].iov_len))
				< iov[i].iov_len)
			return (nwrote < 0 ? nwrote : total + nwrote);
		total += nwrote;
	}
	return (total);
}





fcc_readv (fd, iov, niov)
int fd, niov;
struct iovec *iov;
/*
 * Vectored read.
 */
{
	int i, nread, total = 0;

	for (i = 0; i < niov; i++)
	{
		if ((nread = read (fd, iov[i].iov_base, iov[i].iov_len))
				< iov[i].iov_len)
			return (nread < 0 ? nread : total + nread);
		total += nread;
	}
	return (total);
}

# endif
