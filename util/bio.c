/* 10/88 jc */
/*
 * Unix versions of the Block I/O routines.
 */
# include <fcntl.h>
# include <sys/types.h>
# include <sys/file.h>


/*
 * Kludge: static info kept when emulating async bio operations, to be
 * returned by bio_wait.
 */
static int Status, Len;

# define BLOCK_SIZE 512


int bio_open (file, len, alq)
char *file;
int *len, *alq;
/*
 * Open a file for block I/O.
 */
{
	char fname[120];
	int lun;
/*
 * Copy the (fortran-compatible) string over.
 */
 	strncpy (fname, file, *len);
	fname[*len] = '\0';
/*
 * Make sure the file exists.
 */
 	if (access (fname, F_OK))
		return (-1);
/*
 * Open the file.
 */
	if ((lun = open (fname, O_RDWR)) < 0)
	{
		perror (fname);
		return (-2);
	}
	return (lun);
}




int bio_view (file, len, alq)
char *file;
int *len, *alq;
/*
 * Open a file for readonly block I/O.
 */
{
	char fname[120];
	int lun;
/*
 * Copy the (fortran-compatible) string over.
 */
 	strncpy (fname, file, *len);
	fname[*len] = '\0';
/*
 * Make sure the file exists.
 */
 	if (access (fname, F_OK))
		return (-1);
/*
 * Open the file.
 */
	if ((lun = open (fname, O_RDONLY)) < 0)
	{
		perror (fname);
		return (-2);
	}
	return (lun);
}





bio_create (file, len, alloc, extend)
char *file;
int *len, *alloc, *extend;
/*
 * Create a file.
 */
{
	char fname[120];
	int lun;
/*
 * Copy the (fortran-compatible) string over.
 */
 	strncpy (fname, file, *len);
	fname[*len] = '\0';
/*
 * Open the file.
 */
	if ((lun = open (fname, O_CREAT | O_RDWR | O_TRUNC, 0777)) < 0)
	{
		perror (fname);
		return (-2);
	}
	return (lun);
}





bio_temp (file, len, alloc, extend)
char *file;
int *len, *alloc, *extend;
/*
 * Create a temporary file.
 */
{
	char fname[120];
	int lun;
/*
 * Copy the (fortran-compatible) string over.
 */
 	strncpy (fname, file, *len);
	fname[*len] = '\0';
/*
 * Open the file.
 */
	if ((lun = open (fname, O_CREAT | O_RDWR | O_TRUNC, 0777)) < 0)
	{
		perror (fname);
		return (-2);
	}
/*
 * Now immediately unlink the file, which will cause it to go away altogether
 * when the fd is closed.
 */
 	unlink (fname);
	return (lun);
}




bio_close (lun)
int lun;
/*
 * Close this file.
 */
{
	close (lun);
}






bio_read (lun, block, buffer, nbytes, wait)
int *lun, *block, *nbytes, *wait;
char *buffer;
/*
 * Perform a block read.
 */
{
	int nread;
/*
 * If a block number was given, seek to it.
 */
	if (*block)
	{
		if (lseek (*lun, (off_t) ((*block-1)*BLOCK_SIZE), L_SET) < 0)
		{
			perror ("File seek");
			return (-1);
		}
	}
/*
 * Now do the read.
 */
	nread = read (*lun, buffer, *nbytes);
	Len = nread;
	if (nread < *nbytes)
	{
		Len = -1;
		return (-1);
	}
	return (nread);
}




bio_wait (lun)
int *lun;
/*
 * Fake an async wait.
 */
{
	return (Len);
}




bio_write (lun, block, buffer, nbytes, wait)
int *lun, *block, *nbytes, *wait;
char *buffer;
/*
 * Perform a block write.
 */
{
	int nwrite;
/*
 * If a block number was given, seek to it.
 */
	if (*block)
	{
		if (lseek (*lun, (off_t) ((*block-1)*BLOCK_SIZE), L_SET) < 0)
		{
			perror ("Write File seek");
			return (-1);
		}
	}
/*
 * Now do the write.
 */
	nwrite = write (*lun, buffer, *nbytes);
	Len = nwrite;
	if (nwrite < *nbytes)
	{
		Len = -1;
		return (-1);
	}
	return (nwrite);
}





bio_set_dfn (dfn)
char *dfn;
/*
 * Do nothing, for the time being.
 */
{
}
