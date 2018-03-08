/* 1/88 jc */
static char *rcsid = "$Id: bfile.c,v 1.13 2002-07-11 19:30:55 burghart Exp $";
/*
 * System-dependant binary file stuff.  These routines are needed because
 * the VMS-specific variable-length-record-format file does not exist in
 * other systems, and must be emulated.
 */

#include <stdio.h>
#include <string.h>

# ifdef NETACCESS
#	include "netdisk.h"
# endif

# include <unistd.h>
# include <assert.h>
/* hack to make rfa work */
static long Offset = 0;



int
bfview (file)
char *file;
/*
 * Open, for read access, an existing binary file.
 */
{
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
	return (fd < 0 ? 0 : fd);
#   endif
}



int
bfopen (file)
char *file;
/*
 * Open, for read/write access, an existing binary file.
 */
{
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
}




int
bfcreate (file)
char *file;
/*
 * Create a new binary file.
 */
{
	int fd;
#   ifdef NETACCESS
	if (strchr (file, ':'))
		return (dcreate (file));
#   endif
	if ((fd = creat (file, 0666)) < 0)
	{
		perror (file);
		return (0);
	}
#   ifdef NETACCESS 
	return (lun_assign (fd, LUN_LOCAL));
#   else
	return (fd);
#   endif
}



void
bfclose (fd)
int fd;
/*
 * Close this file.
 */
{
#    ifdef NETACCESS
	if (lun_type (fd) == LUN_NTDSK_DISK)
		dclose (fd);
	else
		close (lun_lookup (fd));
	lun_deassign (fd);
#    else
	close (fd);
#    endif
}



int
bfget (fd, buf, len)
int fd, len;
char *buf;
/*
 * Get a record from this file, with a max length of LEN.
 * Return value is the actual len, or < 0 for EOF.
 */
{
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
/*
 * When the assumption that an unsigned int is 4 bytes long fails, then
 * we'll need to do something else
 */
	assert (sizeof (unsigned int) == 4);

	Offset = lseek (fd, 0, SEEK_CUR);
	if (read (fd, &rlen, 4) < 4)
		return (-1);
/*
 * Deal with null records
 */
	if (rlen == 0)
	{
		read (fd, &rlen, 4);	/* trailing record length */
		return (0);
	}
/*
 * Data overrun? 
 */
	if (rlen > len)
	{
		ui_printf ("Data overrun, %d/%d\n", rlen, len);
		read (fd, buf, len);
	/*
	 * Skip the rest of the record, including the 4 byte record length
	 * at the end of the record
	 */
		lseek (fd, (long) rlen - len + 4, SEEK_CUR);
		return (len);
	}
/*
 * Normal read
 */
	read (fd, buf, rlen);
	read (fd, &rlen, 4);	/* trailing record length */
	return (rlen);
}



void
bfput (fd, buf, len)
int fd, len;
char *buf;
/*
 * Write out a binary record.
 */
{
	unsigned int rlen = len;

#   ifdef NETACCESS
	if (lun_type (fd) == LUN_NTDSK_DISK)
	{
		dput (fd, buf, len);
		return;
	}
	else
		fd = lun_lookup (fd);
# endif
/*
 * When the assumption that an unsigned int is 4 bytes long fails, then
 * we'll need to do something else
 */
	assert (sizeof (unsigned int) == 4);

	Offset = lseek (fd, 0, SEEK_CUR);
	write (fd, &rlen, 4);
	if (rlen > 0)
		write (fd, buf, len);
	write (fd, &rlen, 4);
}



int
bfrfa (r, rfa)
int r;
short rfa[3];
/*
 * Return the RFA of the last disk operation in RFA.
 */
{
	int fd;
	long *temp = (long *) rfa;

#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
	{
		cli_drfa (lun_lookup (r), rfa);
		return (0);
	}
	else
		fd = lun_lookup (r);
#   endif
	*temp = Offset;

	return (1);
}



int
bffind (r, rfa)
int r;
short rfa[3];
/*
 * Position to the record indicated by the RFA.
 */
{
	int fd = r;

#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
	{
		cli_dfind (lun_lookup (r), rfa);
		return (0);
	}
	else
		fd = lun_lookup (r);
#   endif
	if (lseek (fd, *(long *) rfa, SEEK_SET) == -1)
		printf ("\nImproper seek\n");

	return (1);
}



void
bfrewind (fd)
int fd;
/*
 * Rewind this file.
 */
{
#    ifdef NETACCESS
	if (lun_type (fd) == LUN_NTDSK_DISK)
		drewind (fd);
	else
		lseek (lun_lookup (fd), 0, SEEK_SET);
#    else
	lseek (fd, 0, SEEK_SET);
#    endif
}
