/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 FSLEN.C 19-Feb-96,15:12:14,`DWS' reglue: new file                       */
/* 2 FSLEN.C 20-Feb-96,12:09:12,`USER' Released                              */
/* 3 FSLEN.C 17-Jul-96,11:20:36,`BILLL' Added programmer documentation.      */
/* 4 FSLEN.C 29-Jul-96,8:12:20,`BILLL' Added programmer documentation        */
/*      (6653).                                                              */
/* 5 FSLEN.C 6-Sep-96,10:17:20,`USER' Released                               */
/**** McIDAS Revision History *** */

#include "mcidas.h"

/*
*$ Name:
*$      fslen   - Determines logical length of a Fortran string.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      FsLen
*$      fslen(const char *string, FsLen n)
*$
*$ Input:
*$      string  - Pointer to space-padded character array.
*$      n       - Number of bytes in string.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Length of the string up to but not including any trailing
*$      spaces.
*$
*$ Remarks:
*$      Given a space-padded character array (not NUL-terminated) of the
*$      given length, fslen() returns the number of bytes in the array
*$      up to but not including any trailing spaces.
*$
*$ Categories:
*$      utility
*/

FsLen
fslen(const char *string, FsLen n)
{
	/* look for the last non-blank */
	for (; n > 0; n--)
	{
		if (string[n - 1] != SPACE)
			return n;
	}

	/* default return 0 - no non-blanks found */
	return 0;
}
