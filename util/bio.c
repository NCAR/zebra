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
/*
 * lun type definitions
 */
# define LUN_LOCAL	1
# define LUN_NETDISK	2

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
 * Check to see if the file is on another machine
 */
	if (strchr (fname, ':'))
	{
		if ((lun = cli_bio_open (fname)) < 0)
			return (lun);
		return (lun_assign (lun, LUN_NETDISK));
	}
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
	return (lun_assign (lun, LUN_LOCAL));
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
 * On this machine?
 */
	if (strchr (fname, ':'))
	{
		if ((lun = cli_bio_view (fname)) < 0)
			return (lun);
		return (lun_assign (lun, LUN_NETDISK));
	}
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
	return (lun_assign (lun, LUN_LOCAL));
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
 * On this machine?
 */
	if (strchr (fname, ':'))
	{	
		if ((lun = cli_bio_create (fname, alloc, extend)) < 0)
			return (lun);
		return (lun_assign (lun, LUN_NETDISK));
	}
/*
 * Open the file.
 */
	if ((lun = open (fname, O_CREAT | O_RDWR | O_TRUNC, 0777)) < 0)
	{
		perror (fname);
		return (-2);
	}
	return (lun_assign (lun, LUN_LOCAL));
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
 * On this machine?
 */
	if (strchr (fname, ':'))
	{
		if ((lun = cli_bio_temp (fname, alloc, extend)) < 0)
			return (lun);
		return (lun_assign (lun, LUN_NETDISK));
	}
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
	return (lun_assign (lun, LUN_LOCAL));
}




bio_close (lun)
int lun;
/*
 * Close this file.
 */
{
	if (lun_type (lun) == LUN_NETDISK)
		cli_bio_close (lun_lookup (lun));
	else
		close (lun_lookup (lun));
	lun_deassign (lun);
}






bio_read (lun, block, buffer, nbytes, wait)
int *lun, *block, *nbytes, *wait;
char *buffer;
/*
 * Perform a block read.
 */
{
	int nread, fileid;
/*
 * Check to see if this is a file on another machine
 */
	if (lun_type (*lun) == LUN_NETDISK)
		return (cli_bio_read(lun_lookup(*lun), block, buffer, nbytes));
	else
		fileid = lun_lookup (*lun);
/*
 * If a block number was given, seek to it.
 */
	if (*block)
	{
		if (lseek (fileid, (off_t) ((*block-1)*BLOCK_SIZE), L_SET) < 0)
		{
			perror ("File seek");
			return (-1);
		}
	}
/*
 * Now do the read.
 */
	nread = read (fileid, buffer, *nbytes);
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
	if (lun_type (*lun) == LUN_NETDISK)
		return (cli_bio_wait (lun_lookup (*lun)));
	else
		return (Len);
}




bio_write (lun, block, buffer, nbytes, wait)
int *lun, *block, *nbytes, *wait;
char *buffer;
/*
 * Perform a block write.
 */
{
	int nwrite, fileid;

	if (lun_type (*lun) == LUN_NETDISK)
		return(cli_bio_write(lun_lookup(*lun), block, buffer, nbytes));
	else
		fileid = lun_lookup (*lun);
/*
 * If a block number was given, seek to it.
 */
	if (*block)
	{
		if (lseek (fileid, (off_t) ((*block-1)*BLOCK_SIZE), L_SET) < 0)
		{
			perror ("Write File seek");
			return (-1);
		}
	}
/*
 * Now do the write.
 */
	nwrite = write (fileid, buffer, *nbytes);
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





static int Lun_table[256][2];
static int Initialized = 0;
# define LUN_FREE	0

lun_type (lun)
int lun;
{
	return (Lun_table[lun - 1][0]);
}





lun_lookup (lun)
int lun;
{
	return (Lun_table[lun - 1][1]);
}





lun_assign (lun, type)
int lun, type;
{
	int	i;

	if (!Initialized)
		memset ((char *) Lun_table, (char) LUN_FREE, 512);
	for (i=0; i<255; i++)
		if (Lun_table[i][0] == LUN_FREE)
		{
			Lun_table[i][0] = type;
			Lun_table[i][1] = lun;
			return (i + 1);
		}
	perror ("Out of space in Lun_table!");
	return (0);
}





lun_deassign (lun)
int lun;
{
	Lun_table[lun - 1][0] = LUN_FREE;
}
