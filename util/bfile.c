/* 1/88 jc */
/*
 * System-dependant binary file stuff.  These routines are needed because
 * the VMS-specific variable-length-record-format file does not exist in
 * other systems, and must be emulated.
 */
# ifdef NETACCESS
#   include "netdisk.h"
# endif




int
bfview (file)
char *file;
/*
 * Open, for read access, an existing binary file.
 */
{
# ifdef UNIX
	int fd;

#   ifdef NETACCESS
	if (strchr (file, ':'))
		return (dview (file));
#   endif
	if ((fd = open (file, 0)) < 0)
		perror (file);
#   ifdef NETACCESS
	return (fd < 0 ? 0 : lun_assign (fd, LUN_LOCAL));
#   else
	return (fd < 0 ? 0 : fd));
#   endif

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

#   ifdef NETACCESS
	if (strchr (file, ':'))
		return (dopen (file));
	if ((fd = open (file, 2)) < 0)
		perror (file);
#    endif
#    ifdef NETACCESS
	return (fd < 0 ? 0 : lun_assign (fd, LUN_LOCAL));
#    else
	return (fd < 0 ? 0 : fd);
#    endif

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
#   ifdef NETACCESS
	if (strchr (file, ':'))
		return (dcreate (file));
#   endif
	if ((fd = creat (file, 0777)) < 0)
	{
		perror (file);
		return (0);
	}
#   ifdef NETACCESS 
	return (lun_assign (fd, LUN_LOCAL));
#   else
	return (fd);
#   endif
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
#    ifdef NETACCESS
	if (lun_type (fd) == LUN_NTDSK_DISK)
		dclose (fd);
	else
		close (lun_lookup (fd));
	lun_deassign (fd);
#    else
	close (fd);
#    endif
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
#   ifdef NETACCESS
	if (lun_type (fd) == LUN_NTDSK_DISK)
		return (dget (fd, buf, len));
	else
		fd = lun_lookup (fd);
#   endif
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

#   ifdef NETACCESS
	if (lun_type (fd) == LUN_NTDSK_DISK)
		return (dput (fd, buf, len));
	else
		fd = lun_lookup (fd);
# endif
	write (fd, &rlen, sizeof (unsigned int));
	if (rlen > 0)
		write (fd, buf, len);
# else
	dput (fd, buf, len);
# endif
}
