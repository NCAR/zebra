/* 11/84 jc */
/* $Id: disk.c,v 1.8 2002-07-11 19:30:55 burghart Exp $ */
/*
 * Disk handling.
 *
 * Changes made 1/88 jc, for unix.  These routines are usable for text
 * files ONLY under unix.
 */
# include <stdio.h>
# include <errno.h>
# include <string.h>
# include <stdlib.h>
# ifdef NETACCESS
#   include "netdisk.h"
# endif
# define LTYPE long
FILE *fopen ();
long ftell (), Offset;

# define FALSE 0

long lun_assign();
long lun_lookup();


LTYPE
dcreate (file)
char *file;
/*
 * Try to create the given file.
 */
{
	FILE *fd;

#   ifdef NETACCESS
	if (strchr (file, ':'))
	{
		int	fnum;

		if (!(fnum = cli_dcreate (file)))
			return (FALSE);
		return (lun_assign (fnum, LUN_NTDSK_DISK));
	}
#   endif
	if ((fd = fopen (file, "w")) == NULL)
	{
		perror (file);
		printf ("\n");
		return (FALSE);
	}
#   ifdef NETACCESS
	return ((LTYPE) lun_assign ((long) fd, LUN_LOCAL));
#   else
	return ((LTYPE) fd);
#   endif
}




LTYPE
dopen (file)
char *file;
/*
 * Attempt to open an already existing file.
 */
{
	FILE *fd;

#   ifdef NETACCESS
	if (strchr (file, ':'))
	{
		int	fnum;

		if (!(fnum = cli_dopen (file)))
			return (FALSE);
		return (lun_assign (fnum, LUN_NTDSK_DISK));
	}
#   endif
	if ((fd = fopen (file, "rw")) == NULL)
	{
		if (errno != ENOENT)
		{
			perror (file);
			printf ("\n");
		}
		return (FALSE);
	}
#   ifdef NETACCESS
	return ((LTYPE) lun_assign ((long) fd, LUN_LOCAL));
#   else
	return ((LTYPE) fd);
#   endif
}





LTYPE
dview (file)
char *file;
/*
 * Attempt to open an already existing file, for read only access.
 */
{
	FILE *fd;

#   ifdef NETACCESS
	if (strchr (file, ':'))
	{
		int	fnum;

		if (!(fnum = cli_dview (file)))
			return (FALSE);
		return (lun_assign (fnum, LUN_NTDSK_DISK));
	}
#   endif
	if ((fd = fopen (file, "r")) == NULL)
	{
		if (errno != ENOENT)
		{
			perror (file);
			printf ("\n");
		}
		return (FALSE);
	}
#   ifdef NETACCESS
	return ((LTYPE) lun_assign ((long) fd, LUN_LOCAL));
#   else
	return ((LTYPE) fd);
#   endif
}



int
dput (r, buf, len)
LTYPE r;
char *buf;
int len;
/*
 * Attempt to write the given data to the file.
 */
{
	char cbuf[5000];
	FILE *fd = (FILE *) r;

#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
		return (cli_dput (lun_lookup (r), buf, len));
	else
		fd = (FILE *) lun_lookup (r);
#   endif
	memcpy (cbuf, buf, len);
	cbuf[len] = 0;
	fprintf (fd, "%s\n", cbuf);
	return (1);
}







int
dget (r, buf, max)
LTYPE r;
char *buf;
int max;
/*
 * Read a record from the file.  Normally, the length of the record read
 * is returned.  A return value of -1 implies EOF.  Negative return values
 * imply some other error.
 */
{
	int len;
	FILE *fd = (FILE *) r;

#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
		return (cli_dget (lun_lookup (r), buf, max));
	else
		fd = (FILE *) lun_lookup (r);
#   endif
	Offset = ftell (fd);
	if (! fgets (buf, max, fd))
		return (-1);
	len = strlen (buf);
	buf[len-1] = '\0';	/* Zap newline */
	return (len - 1);
}



int
drfa (r, rfa)
LTYPE r;
short rfa[3];
/*
 * Return the RFA of the last disk operation in RFA.
 */
{
	long *temp = (long *) rfa;
	FILE *fd = (FILE *) r;

#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
	{
		cli_drfa (lun_lookup (r), rfa);
		return (0);
	}
	else
		fd = (FILE *) lun_lookup (r);
#   endif
	*temp = ftell (fd);
	return (1);
}





int
dagain (r)
LTYPE r;
/*
 * Position to the beginning of the last record read.
 */
{
	FILE *fd = (FILE *) r;

#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
	{
		cli_dagain (lun_lookup (r));
		return (0);
	}
	else
		fd = (FILE *) lun_lookup (r);
#   endif
	if (fseek (fd, Offset, 0) == -1)
		printf ("\nImproper seek\n");
	return (1);
}




int
dfind (r, rfa)
LTYPE r;
short rfa[3];
/*
 * Position to the record indicated by the RFA.
 */
{
	FILE *fd = (FILE *) r;

#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
	{
		cli_dfind (lun_lookup (r), rfa);
		return (0);
	}
	else
		fd = (FILE *) lun_lookup (r);
#   endif
	if (fseek (fd, *(long *) rfa, 0) == -1)
		printf ("\nImproper seek\n");
	return (1);
}



void
dclose (r)
LTYPE r;
/*
 * Try to close the file
 */
{
#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
		cli_dclose (lun_lookup (r));
	else
		fclose ((FILE *) lun_lookup (r));
	lun_deassign (r);
#   else
	fclose ((FILE *) r);
#   endif
}




drewind (r)
LTYPE r;
/*
 * Rewind the given file.
 */
{
	FILE *fd = (FILE *) r;

#   ifdef NETACCESS
	if (lun_type (r) == LUN_NTDSK_DISK)
		return (cli_drewind (lun_lookup (r)));
	else
		fd = (FILE *) lun_lookup (r);
#   endif
	rewind (fd);
	return (1);
}
