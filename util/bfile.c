/* 1/88 jc */
/*
 * System-dependant binary file stuff.  These routines are needed because
 * the VMS-specific variable-length-record-format file does not exist in
 * other systems, and must be emulated.
 */




int
bfview (file)
char *file;
/*
 * Open, for read access, an existing binary file.
 */
{
# ifdef UNIX
	int fd;

	if ((fd = open (file, 0)) < 0)
		perror (file);
	return (fd < 0 ? 0 : fd);
# else
	return (dview (file));
# endif
}



int
bfopen (file)
char *file;
/*
 * Open, for read/write access, an existing binary file.
 */
{
# ifdef UNIX
	int fd;

	if ((fd = open (file, 2)) < 0)
		perror (file);
	return (fd < 0 ? 0 : fd);
# else
	return (dopen (file));
# endif
}




int
bfcreate (file)
char *file;
/*
 * Create a new binary file.
 */
{
# ifdef UNIX
	int fd;

	if ((fd = creat (file, 0777)) < 0)
	{
		perror (file);
		return (0);
	}
	return (fd);
# else
	return (dcreate (file));
# endif
}



bfclose (fd)
int fd;
/*
 * Close this file.
 */
{
# ifdef UNIX
	close (fd);
# else
	dclose (fd);
# endif
}




bfget (fd, buf, len)
int fd, len;
char *buf;
/*
 * Get a record from this file, with a max length of LEN.
 * Return value is the actual len, or < 0 for EOF.
 */
{
# ifdef UNIX
	unsigned int rlen;
/*
 * First, get the record length.
 */
	if (read (fd, &rlen, sizeof (int)) < sizeof (int))
		return (-1);
	if (rlen == 0)
		return (0);
	if (rlen > len)
	{
		ui_printf ("Data overrun, %d/%d\n", rlen, len);
		read (fd, buf, len);
		lseek (fd, (long) rlen - len, 1); /* skip rest of rec */
		return (len);
	}
	read (fd, buf, rlen);
	return (rlen);
# else
	return (dget (fd, buf, len));
# endif
}




bfput (fd, buf, len)
int fd, len;
char *buf;
/*
 * Write out a binary record.
 */
{
# ifdef UNIX
	unsigned int rlen = len;

	write (fd, &rlen, sizeof (unsigned int));
	if (rlen > 0)
		write (fd, buf, len);
# else
	dput (fd, buf, len);
# endif
}
