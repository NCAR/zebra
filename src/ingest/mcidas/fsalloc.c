/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 FSALLOC.C 1-Mar-94,9:45:20,`RUSSD' Initial release into AIX             */
/* 2 FSALLOC.C 1-Mar-94,10:04:06,`RUSSD' Standardize function names          */
/* 3 FSALLOC.C 31-Mar-94,22:53:26,`BARRYR' Add proprietary statement         */
/* 4 FSALLOC.C 2-May-94,16:40:10,`USER' Released                             */
/* 5 FSALLOC.C 9-May-95,18:26:28,`DWS' MCPATH phase 3 (5429)                 */
/* 6 FSALLOC.C 6-Jun-95,15:02:18,`USER' Released                             */
/* 7 FSALLOC.C 17-Jul-96,11:20:34,`BILLL' Added programmer documentation.    */
/* 8 FSALLOC.C 29-Jul-96,8:11:30,`BILLL' Added programmer documentation      */
/*      (6653).                                                              */
/* 9 FSALLOC.C 6-Sep-96,10:17:14,`USER' Released                             */
/**** McIDAS Revision History *** */

#include <string.h>
#include <stdlib.h>

#include "mcidas.h"

/*
*$ Name:
*$      fsalloc - Allocates a C string copy of a Fortran string.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      char *
*$      fsalloc(const char *string, Fslen n)
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
*$        0     - Failure, malloc() failed.
*$      <>0     - Success, pointer to malloc()d string.
*$
*$ Remarks:
*$      Given a space-padded character array (not NUL-terminated) of the
*$      given length, fsalloc() returns a malloc()d copy of the array as
*$      a NUL-terminated string with trailing spaces removed.  It does
*$      not write into the given array.
*$
*$      In other words, fsalloc() returns a freshly allocated C string
*$      version of the given FORTRAN string.
*$
*$      The caller is responsible for free()ing the string.
*$
*$ Categories:
*$      utility
*/

char *
fsalloc(const char *string, FsLen n)
{
	char           *ans;

	n--;                    /* length -> index */
	while (n >= 0 && string[n] == SPACE)
	{
		/* string[n] = '\0'; */
		n--;
	}
	n++;                    /* index -> length */

	if ((ans = malloc(n + 1)) == (char *) 0)
		return (char *) 0;

	strncpy(ans, string, n);
	ans[n] = '\0';

	return ans;
}
