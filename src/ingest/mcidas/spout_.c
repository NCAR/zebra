/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED */

/**** McIDAS Revision History *** */
/* 1 SPOUT_.C 19-Feb-96,15:31:22,`DWS' reglue: new file                      */
/* 2 SPOUT_.C 20-Feb-96,12:13:16,`USER' Released                             */
/* 3 SPOUT_.C 23-Jul-96,8:33:44,`BILLL' Added programmer documentation       */
/*      (6653).                                                              */
/* 4 SPOUT_.C 30-Jul-96,10:10:04,`BILLL' Added programmer documentation      */
/*      (6653).                                                              */
/* 5 SPOUT_.C 21-Aug-96,10:44:16,`BILLL' Fixed column fault in help.         */
/* 6 SPOUT_.C 6-Sep-96,10:20:12,`USER' Released                              */
/**** McIDAS Revision History *** */

/*
 * spout_.c
 *
 *  These files output text according to UC setup.
 *
 *  these FORTRAN routines, spout, sdest, edest, ddest
 *  should not be called by c code, for which Mcprintf,
 *  Mceprintf, and Mcdprintf are recommended.
 *
 *  j.benson
 */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include "mcidasp.h"

#include "mcidas.h"

/* synonym for type of M0?spout() functions */

typedef void (*SpoutFn)(const char *msg, size_t siz_msg);

static void
mydest(const char *text, Fint value, FsLen textlen, SpoutFn spoutfn)
{
	/*
	 * the case of *value==0 is handled separately because it is
	 * the dominant case, and can avoid an extra string move by
	 * being handled explicitly
	 */
	if (value == 0)
	{
		spoutfn(text, (size_t) textlen);
	}
	else
	{
		static M0buf   *buf;
		char            numbuf[64];

		if(!buf)
			buf = M0bufalloc();

		M0bufcpy(buf, text, (size_t) textlen);

		sprintf(numbuf, " %ld", (long) value);
		M0bufcat(buf, numbuf, strlen(numbuf));

		spoutfn(M0bufptr(buf), M0buflen(buf));
	}
}



/*
*$ Name:
*$      spout      - Puts a string to standard I/O device or file.
*$
*$ Interface:
*$      subroutine
*$      spout(character*(*) charstring)
*$
*$ Input:
*$      charstring - Character string to be output.
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
*$      none
*$
*$ Categories: 
*$      text
*$      utility
*/

void
spout_(const char *text, FsLen textlen)
{
	M0sspout(text, (size_t) textlen);
}
/*
*$ Name:
*$  sdest           - Puts a string to standard I/O device or file.
*$
*$ Interface:
*$      subroutine
*$      sdest(character*(*) charstring, integer n)
*$
*$ Input:
*$      charstring  - Character string to be output.
*$      n           - An integer that will be output after the string.
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
*$      If 'n' is zero it is not printed.
*$
*$ Categories: 
*$       text
*$       utility
*/
void
sdest_(const char *text, const Fint *value, FsLen textlen)
{
	mydest(text, *value, textlen, M0sspout);
}
/*
*$ Name:
*$  edest           - Writes a string to currently defined device or file for
*$                    error messages.
*$
*$ Interface:
*$      subroutine
*$      edest(character*(*) charstring, integer n)
*$
*$ Input:
*$      charstring  - Character string to be output.
*$      n           - An integer that is output directly after the string.
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
*$  If 'n' is zero it is not printed.
*$
*$ Categories: 
*$  text
*$  utility
*/
void
edest_(const char *text, const Fint *value, FsLen textlen)
{
	mydest(text, *value, textlen, M0espout);
}
/*
*$ Name:
*$      ddest      - Writes a string to currently defined debug device or file.
*$
*$ Interface:
*$      subroutine
*$      ddest(character*(*) charstring, integer n)
*$
*$ Input:
*$      charstring - Character string to be output.
*$      n          - Integer which is printed immediately after the string.
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
*$      If n is zero it is not printed.
*$
*$ Categories: 
*$      text
*$      utility
*/
void
ddest_(const char *text, const Fint *value, FsLen textlen)
{
	mydest(text, *value, textlen, M0dspout);
}
