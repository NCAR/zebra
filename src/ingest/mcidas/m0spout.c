/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED */

/**** McIDAS Revision History *** */
/* 1 M0SPOUT.C 19-Feb-96,15:20:56,`DWS' reglue: new file                     */
/* 2 M0SPOUT.C 20-Feb-96,12:11:54,`USER' Released                            */
/* 3 M0SPOUT.C 22-Mar-96,11:52:50,`DWS' reglue: modified file                */
/* 4 M0SPOUT.C 25-Mar-96,13:56:24,`USER' Released                            */
/**** McIDAS Revision History *** */

/*
 * M0spout_.c
 *
 *  output text according to its routing
 *
 *  j.benson
 */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "mcidasp.h"
#include "m0glue.h"
#include "m0arg.h"

/* In OS/2, the screen controller expects lines to end in 0x0D ('\r') */

#ifdef __EMX__
static const char crt_eol[] = "\n";
static const char file_eol[] = "\r\n";
#else
static const char crt_eol[] = "\n";
static const char file_eol[] = "\n";
#endif

/* command name used in case arg fetching is not yet initialized */
static const char unkcmd[] = "Unknown command";


/*
*|  Name:
*|        M0spout  - low level text output function
*|
*|  Interface:
*|        #include "mcidasp.h"
*|
*|        void
*|        M0spout(int fd, const char *text, size_t textlen, int key,
*|                int win, int col)
*|
*|  Input:
*|        fd        - file descriptor of an open file
*|        text      - contents to be sent to the file
*|        textlen   - the number of bytes in text
*|        key       - format of output required (0 - 4)
*|        win       - window number
*|        col       - color number
*|
*|  Input and output:
*|        none
*|
*|  Output:
*|        none
*|
*|  Return values:
*|        none
*|
*|  Remarks:
*|        key = 0 means discard the output
*|        key = 1 means add escape sequences for window and color if
*|                the output will be read by the McIDAS text window
*|        key = 2 is printer
*|        key = 3 is ascii file
*|        key = 4 is 80-character record file
*|
*/

static void
M0spout(int fd, const char *text, size_t textlen, int key, int win, int color)
{
	textlen = fslen(text, (FsLen) textlen);

	switch(key)
	{
        case 0:             /* black hole  */
		break;

        case 1:             /* crt         */
		if (M0mctext())
		{
			static M0buf *buf;
			char buffer[4];

			if(!buf)
			    buf = M0bufalloc();

			buffer[0]=2;
			buffer[1]=win;
			buffer[2]=3;
			buffer[3]=color;

			/* start buf with 4 bytes of control codes */
			M0bufcpy(buf, buffer, 4);

			/* then add the text */
			M0bufcat(buf, text, textlen);

			/* and finally, append the newline */
			M0bufcat(buf, crt_eol, sizeof(crt_eol)-1);

			/* then, write it out as a unit */
			(void)write(fd, M0bufptr(buf), M0buflen(buf));
			break;
		}
		/* if not doing escapes, fall through to case 2 */
        case 2:             /* printer */
        case 3:             /* ascii file */
		(void)write(fd, text, textlen);
		(void)write(fd, file_eol, sizeof(file_eol)-1);
		break;

        case 4:             /* 80-character record file */
		{
			char buffer[80];

			memset(buffer, SPACE, sizeof buffer);
			memcpy(buffer, text, textlen > sizeof buffer
					? sizeof buffer : textlen);
			(void)write(fd, buffer, sizeof buffer);
		}
		break;

        default:
		break;   
	}
}

/* key value: ignore, crt, print, ascii, record */
#define KEY  (int) Mcluc(UC_SDEST_KEY)
#define KEYE (int) Mcluc(UC_EDEST_KEY)
#define KEYD (int) Mcluc(UC_DDEST_KEY)

/* file descriptor for SDEST, DDEST, EDEST */
#define FD  (int) Mcluc(UC_SDEST_FD)
#define FDE (int) Mcluc(UC_EDEST_FD)
#define FDD (int) Mcluc(UC_DDEST_FD)

/*
*|  Name:
*|        M0sspout  - low level output to McIDAS standard destination
*|
*|  Interface:
*|        #include "mcidasp.h"
*|
*|        void
*|        M0sspout(const char *text, size_t textlen)
*|
*|  Input:
*|        text      - text to be written
*|        textlen   - the number of bytes in text
*|
*|  Input and output:
*|        none
*|
*|  Output:
*|        none
*|
*|  Return values:
*|        none
*|
*|  Remarks:
*|        A newline is appended to the given text.
*/

void
M0sspout(const char *text, size_t textlen)
{
	textlen = fslen(text, (FsLen) textlen);
	M0spout(FD, text, textlen, KEY, Mcluc(UC_TWIN), Mcluc(UC_TCOL));
}

/*
*|  Name:
*|        M0espout0  - low level output to McIDAS error destination
*|
*|  Interface:
*|        #include "mcidasp.h"
*|
*|        void
*|        M0espout0(const char *text, size_t textlen)
*|
*|  Input:
*|        text      - text to be written
*|        textlen   - the number of bytes in text
*|
*|  Input and output:
*|        none
*|
*|  Output:
*|        none
*|
*|  Return values:
*|        none
*|
*|  Remarks:
*|        A newline is appended to the given text.
*/

void
M0espout0(const char *text, size_t textlen)
{
	/* message color for EDEST */
	int             color = TCOLOR_FOREGROUND_BRIGHT_YELLOW;

	static M0buf   *buf;	/* temporary place to copy message */

	/*
	 * We need to make a copy of the message that we know is
	 * NUL-terminated in order to use strstr().
	 */

	if (!buf)
		buf = M0bufalloc();

	/* body of message */
	M0bufcpy(buf, text, textlen);

	/* possible change of color due to done-ness of message */

	if (strstr(M0bufptr(buf), "DONE")
	    || strstr(M0bufptr(buf), "Done")
	    || strstr(M0bufptr(buf), "done"))
		color = TCOLOR_FOREGROUND_WHITE;

	M0spout(FDE, text, textlen, KEYE, Mcluc(UC_TWIN), color);
}

/*
*|  Name:
*|        M0espout  - low level output to McIDAS error destination
*|
*|  Interface:
*|        #include "mcidasp.h"
*|
*|        void
*|        M0espout(const char *text, size_t textlen)
*|
*|  Input:
*|        text      - text to be written
*|        textlen   - the number of bytes in text
*|
*|  Input and output:
*|        none
*|
*|  Output:
*|        none
*|
*|  Return values:
*|        none
*|
*|  Remarks:
*|        A newline is appended to the given text.
*|        The command name is prepended to the given text.
*/

void
M0espout(const char *text, size_t textlen)
{
	const char     *name;	/* pointer to program name string */
	static const char cs[] = {':', SPACE};

	static M0buf   *buf;	/* temporary place to construct
				 * message */

	if (!buf)
		buf = M0bufalloc();

	/* name of program */
	M0argget(0, (char *)0, 0, &name);
	if (!name)
		name = unkcmd;
	M0bufcpy(buf, name, strlen(name));

	/* colon-space */
	M0bufcat(buf, cs, sizeof cs);

	/* body of message */
	M0bufcat(buf, text, textlen);

	M0espout0(M0bufptr(buf), M0buflen(buf));
}

/*
*|  Name:
*|        M0dspout0  - low level output to McIDAS debug destination
*|
*|  Interface:
*|        #include "mcidasp.h"
*|
*|        void
*|        M0dspout0(const char *text, size_t textlen)
*|
*|  Input:
*|        text      - text to be written
*|        textlen   - the number of bytes in text
*|
*|  Input and output:
*|        none
*|
*|  Output:
*|        none
*|
*|  Return values:
*|        none
*|
*|  Remarks:
*|        A newline is appended to the given text.
*/

void
M0dspout0(const char *text, size_t textlen)
{
	M0spout(FDD, text, textlen, KEYD, Mcluc(UC_TWIN),
		TCOLOR_FOREGROUND_BRIGHT_YELLOW);
}

/*
*|  Name:
*|        M0dspout  - low level output to McIDAS debug destination
*|
*|  Interface:
*|        #include "mcidasp.h"
*|
*|        void
*|        M0dspout(const char *text, size_t textlen)
*|
*|  Input:
*|        text      - text to be written
*|        textlen   - the number of bytes in text
*|
*|  Input and output:
*|        none
*|
*|  Output:
*|        none
*|
*|  Return values:
*|        none
*|
*|  Remarks:
*|        A newline is appended to the given text.
*|        The command name is prepended to the given text.
*/

void
M0dspout(const char *text, size_t textlen)
{
	const char     *name;	/* pointer to program name string */
	static const char ss[] = {'*', SPACE};

	static M0buf   *buf;	/* temporary place to construct
				 * message */

	if (!buf)
		buf = M0bufalloc();

	/* name of program */
	M0argget(0, (char *)0, 0, &name);
	if (!name)
		name = unkcmd;
	M0bufcpy(buf, name, strlen(name));

	/* star-space */
	M0bufcat(buf, ss, sizeof ss);

	/* body of message */
	M0bufcat(buf, text, fslen(text, (FsLen) textlen));

	M0dspout0(M0bufptr(buf), M0buflen(buf));
}
