/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 MCREAD.C 1-May-95,16:27:46,`DWS' McIDAS-style file reading (5402)       */
/* 2 MCREAD.C 5-May-95,12:43:40,`JOHNB' change return code                   */
/* 3 MCREAD.C 6-Jun-95,15:09:24,`USER' Released                              */
/* 4 MCREAD.C 19-Feb-96,16:03:16,`DWS' reglue: modified file                 */
/* 5 MCREAD.C 20-Feb-96,12:00:16,`USER' Released                             */
/* 6 MCREAD.C 22-Mar-96,11:56:12,`DWS' reglue: modified file                 */
/* 7 MCREAD.C 25-Mar-96,13:57:58,`USER' Released                             */
/* 8 MCREAD.C 8-Oct-96,9:32:58,`BILLL' Edited programmer documentation (7100)*/
/* 9 MCREAD.C 21-Oct-96,16:27:22,`USER' Released                             */
/**** McIDAS Revision History *** */


#include <sys/types.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

#include "mcidas.h"
#include "mcidasp.h"

/**
*$ Name:
*$      Mcread - Reads data from a file.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      int
*$      Mcread(const char *file, off_t start, size_t count, void *target)
*$
*$ Input:
*$      file    - String containing file name.
*$      start   - First byte in the file to read.
*$      count   - Number of bytes to read.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      target  - Location into which bytes will be read.
*$
*$ Return values:
*$       0      - Success, data was read from file.
*$      -1      - Success, but some data was past end-of-file.
*$      -2      - Failure, bad value for one or more arguments.
*$      -3      - Failure opening file.
*$      -4      - Failure, read failed.
*$      -5      - Failure, cannot seek on the file.
*$
*$ Remarks:
*$      Uninitialized bytes in the file are read as if they had the
*$      value 0x80.
*$
*$      In Fortran, use lbi() or lwi().
*$
*$      A byte count less than 1 or first byte less than 0 is an error
*$      as is a null pointer as a filename.
*$
*$ Categories:
*$      file
*/

int
Mcread(const char *file, off_t start, size_t count, void *target)
{
	int             fd;	/* file descriptor */
	size_t          bytes_read;	/* the bytes in the file */
	size_t          bytes_fill;	/* the bytes we will fill */
	off_t           eof;	/* end-of-file offset */

	/* check for garbage input */
	if (start < 0 || count < 1 || file == 0)
		return -2;

	/* get the fd, opening file if necessary */
	fd = M0cacheopen(file);
	if (fd < 0)
		return -3;

	/* find the end of the file */
	eof = lseek(fd, (off_t) 0, SEEK_END);
	if (eof == -1)
	{
		return -5;
	}

	/* find the number of bytes to read */
	if (start < eof)
	{
		size_t          bytes_available = eof - start;

		bytes_read = MIN(count, bytes_available);
	}
	else
	{
		bytes_read = 0;
	}

	/* now do the part of the read which is in the file */
	if (bytes_read > 0)
	{
		if (lseek(fd, start, SEEK_SET) == -1)
			return -5;
		if (read(fd, target, bytes_read) != bytes_read)
			return -4;
	}

	/* figure out the overhang, if any */
	bytes_fill = count - bytes_read;

	/*
	 * If any part of the transfer is after eof, move in missing
	 * codes.
	 */

	if (bytes_fill > 0)
	{
		memset((char *) target + bytes_read, MCMISSING, bytes_fill);
		return -1;
	}

	return 0;
}


/**
*$ Name:
*$      lbi - Reads from a file using byte addressing.
*$
*$ Interface:
*$      integer function
*$      lbi(character*(*) file, integer start, integer count,
*$          integer buf(*))
*$
*$ Input:
*$      file    - String containing file name.
*$      start   - First byte in the file to read.
*$      count   - Number of bytes to read.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      buf     - Location into which bytes will be read.
*$
*$ Return values:
*$       0      - Success
*$      -1      - Failure, or some data was past end of file.
*$
*$ Remarks:
*$      Uninitialized bytes in the file are read as if they had the
*$      value 0x80.
*$
*$      A byte count less than 1 or first byte less than 0 is an error.
*$      In C, use Mcread().
*$
*$ Categories:
*$      file
*/

Fint
lbi_(const char *file, const Fint *start, const Fint *count,
     void *target, FsLen len)
{
        Fint           rc;
        char           name[1024];

        /* check for garbage input */
        if (*start < 0 || *count < 1 || file == 0)
                return -1;

	len = fslen(file, len);
	assert(0 < len && len < sizeof name);
	memcpy(name, file, len);
	name[len] = '\0';

        rc = (Fint) Mcread(name,
			(off_t) *start,
			(size_t) *count,
			target);

        return rc < 0 ? -1 : 0;
}



/**
*$ Name:
*$      lwi - Reads from a file using word addressing.
*$
*$ Interface:
*$      integer function
*$      lwi(character(*) file, integer start, integer count,
*$          integer buf(*))
*$
*$ Input:
*$      file    - String containing file name.
*$      start   - First word in the file to read.
*$      count   - Number of words to read.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      buf     - Location into which words will be read.
*$
*$ Return values:
*$       0      - Success.
*$      -1      - Failure, or some data was past end of file.
*$
*$ Remarks:
*$      Uninitialized bytes in the file are read as if they had the
*$      value 0x80.
*$
*$      A  word count less than 1 or a first word less than 0 is an
*$      error.
*$      In C, use Mcread.
*$
*$ Categories:
*$      file
*/

Fint
lwi_(const char *file, const Fint *start, const Fint *count,
     void *target, FsLen len)
{
        Fint           rc;
	char           name[1024];

        /* check for garbage input */
        if (*start < 0 || *count < 1 || file == 0)
                return -1;

	len = fslen(file, len);
	assert(0 < len && len < sizeof name);
	memcpy(name, file, len);
	name[len] = '\0';

        rc = (Fint) Mcread(name,
			4 * ((off_t) *start),
			4 * ((size_t) *count),
			target);

        return rc < 0 ? -1 : 0;
}
