/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 MCPRINTF.C 1-May-95,16:26:54,`DWS' printf-like routines for McIDAS (5402*/
/* 2 MCPRINTF.C 9-May-95,18:21:50,`DWS' MCPATH phase 3 (5429)                */
/* 3 MCPRINTF.C 6-Jun-95,15:09:16,`USER' Released                            */
/* 4 MCPRINTF.C 19-Feb-96,16:02:58,`DWS' reglue: modified file               */
/* 5 MCPRINTF.C 20-Feb-96,12:00:10,`USER' Released                           */
/* 6 MCPRINTF.C 10-Jun-96,8:33:08,`BILLL' Added programmer documentation     */
/*      (6653).                                                              */
/* 7 MCPRINTF.C 6-Sep-96,10:18:32,`USER' Released                            */
/**** McIDAS Revision History *** */

#include <fcntl.h>
#include <stdarg.h>
#include <stddef.h>     /* for ptrdiff_t */
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "mcidas.h"
#include "mcidasp.h"


/* synonym for type of M0?spout() functions */

typedef void (*SpoutFn)(const char *msg, size_t siz_msg);


/*
** Name:
**      flushbuf - flush the given buffer using the given function
**
** Interface:
**      static void
**      flushbuf(M0buf *buf, SpoutFn dest)
**
** Input:
**      dest    - pointer to a function to use to write out lines in
**                the buffer
**
** Input and Output:
**      buf     - buffer to flush
**
** Output:
**      none
**
** Return values:
**      none
**
** Remarks:
**      Given a NUL-terminated buffer and a pointer to a M0?spout()
**      function, flushbuf() calls the M0?spout() function for each
**      newline-terminated line in the buffer and removes the line from
**      the buffer.  It leaves the buffer with any trailing partial
**      line.
**
** Categories: 
**      none 
*/


static void
flushbuf(M0buf *buf, SpoutFn dest)
{
	char           *bufptr;
	char           *oldptr;
	char           *curptr;

	/*
	 * While we find newlines in the buffer, write out lines.
	 * Leave any trailing partial line in the buffer. Assumes the
	 * buffer is NUL-terminated.
	 */

	bufptr = M0bufptr(buf);
	oldptr = bufptr;
	while ((curptr = strchr(oldptr, '\n')) != NULL)
	{
		ptrdiff_t       linelen;

		/*
		 * Write out characters up to curptr (not including
		 * the newline, since dest() will append one).
		 *
		 * Then advance oldptr to point to the first char after
		 * the newline.
		 */

		linelen = curptr - oldptr;

		dest(oldptr, (size_t) linelen);

		oldptr = curptr + 1;
	}

	/*
	 * Shift remains of buffer to the beginning.
	 */

	if(bufptr < oldptr)
	{
		/* delete the first oldptr-bufptr bytes */
		M0bufsplice(buf, 0, oldptr - bufptr, (void *)0, 0);
	}
}

/*
** Name:
**      vprint - flush the given buffer using the given function
**
** Interface:
**      static void
**      vprint(M0buf *buf, SpoutFn dest, const char *format,
**             va_list args)
**
** Input:
**      dest    - pointer to a function to use to write out lines in
**                the buffer
**      format  - printf()-style format string
**      args    - stdarg argument for variable length argument list
**
** Input and Output:
**      buf     - buffer to store pending output
**
** Output:
**      none
**
** Return values:
**      none
**
** Remarks:
**      Given a NUL-terminated buffer and a pointer to a M0?spout()
**      function, flushbuf() calls the M0?spout() function for each
**      newline-terminated line in the buffer and removes the line from
**      the buffer.  It leaves the buffer with any trailing partial
**      line.
**
** Categories: 
**      none 
*/

static void
vprint(M0buf *buf, SpoutFn dest, const char *format, va_list args)
{
	static FILE *filebuf;

	size_t buflen;
	long filelen;

	if(!filebuf)
	{
		int fd;

		filebuf = tmpfile();
		assert(filebuf);

#ifdef FD_CLOEXEC
		/* set the close-on-exec bit if possible */
		fd = fileno(filebuf);

		fcntl(fd, F_SETFD, fcntl(fd, F_GETFD, 0) | FD_CLOEXEC);
#endif
	}

	/* print to our temporary file */

	rewind(filebuf);

	(void) vfprintf(filebuf, format, args);

	/* append the contents of the temporary file to our buffer */

	buflen = M0buflen(buf);
	filelen = ftell(filebuf);

	M0bufrealloc(buf, buflen + filelen);

	rewind(filebuf);
	fread(((char *)M0bufptr(buf)) + buflen, 1, filelen, filebuf);

	/* flush the buffer */

	flushbuf(buf, dest);
}

/*
*$ Name:
*$      Mcprintf - Prints to McIDAS standard output device or file.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      void
*$      Mcprintf(const char *format, ...)
*$
*$ Input:
*$      format  - Printf() format string.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      This is a printf()-like routine that sends the output
*$      to the McIDAS standard destination, whether it is a file
*$      or device.
*$
*$      It is subject to the DEV= global keyword.
*$
*$      The output is buffered and is transmitted one line at a time.
*$
*$ Categories: 
*$      utility 
*/

void
Mcprintf(const char *format, ...)
{
	static M0buf *buf;
	va_list args;

	if(!buf)
		buf = M0bufalloc();

	va_start(args, format);
	vprint(buf, M0sspout, format, args);
	va_end(args);
}

/*
*$ Name:
*$      Mceprintf - Prints a string to the currently defined 
*$                  McIDAS error destination, whether it is a device
*$                  or a file.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      void
*$      Mceprintf(const char *format, ...)
*$
*$ Input:
*$      format  - printf() format string
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      This is a printf()-like routine that sends the output
*$      to the McIDAS error destination.
*$
*$      It is subject to the DEV= global keyword.
*$
*$      The output is buffered and is transmitted one line at a time.
*$
*$ Categories: 
*$      utility 
*/

void
Mceprintf(const char *format, ...)
{
	static M0buf *buf;
	va_list args;

	if(!buf)
		buf = M0bufalloc();

	va_start(args, format);
	vprint(buf, M0espout, format, args);
	va_end(args);
}

/*
*$ Name:
*$      Mcdprintf - Prints a string to currently defined McIDAS debug 
*$                  destination, whether it is a file or device.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      void
*$      Mcdprintf(const char *format, ...)
*$
*$ Input:
*$      format  - printf() format string
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      This is a printf()-like routine that sends the output
*$      to the McIDAS debug destination.
*$
*$      It is subject to the DEV= global keyword.
*$
*$      The output is buffered and is transmitted one line at a time.
*$
*$ Categories: 
*$      utility 
*/

void
Mcdprintf(const char *format, ...)
{
	static M0buf *buf;
	va_list args;

	if(!buf)
		buf = M0bufalloc();

	va_start(args, format);
	vprint(buf, M0dspout, format, args);
	va_end(args);
}
