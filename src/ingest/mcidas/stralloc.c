/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 STRALLOC.C 7-Sep-93,10:51:16,`RUSSD' Initial release to AIX (4069)      */
/* 2 STRALLOC.C 5-Oct-93,8:12:10,`USER' Released for McIDAS-X Only           */
/* 3 STRALLOC.C 1-Mar-94,10:07:40,`RUSSD' Standardize function names         */
/* 4 STRALLOC.C 31-Mar-94,22:16:42,`BARRYR' Add proprietary statement        */
/* 5 STRALLOC.C 31-Mar-94,23:45:42,`BARRYR' Add proprietary statement        */
/* 6 STRALLOC.C 2-May-94,16:14:14,`USER' Released for McIDAS-X Only          */
/* 7 STRALLOC.C 3-May-95,9:21:16,`RICKK' Changed revision history cards and  */
/*      added #include - moving to COM library (5403)                        */
/* 8 STRALLOC.C 10-May-95,14:54:16,`RICKK' Changed revision history cards    */
/*      back to AIX                                                          */
/* 9 STRALLOC.C 10-May-95,15:07:14,`DWS' MCPATH phase 3 (5429)               */
/* 10 STRALLOC.C 6-Jun-95,12:25:08,`RICKK' Updated Revision History Cards -  */
/*      moving from aix to com.                                              */
/* 11 STRALLOC.C 6-Jun-95,15:12:52,`USER' Released                           */
/* 12 STRALLOC.C 17-Jul-96,11:21:00,`BILLL' Added programmer documentation   */
/*      (6653).                                                              */
/* 13 STRALLOC.C 6-Sep-96,10:20:20,`USER' Released                           */
/**** McIDAS Revision History *** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "mcidas.h"

/*
*| Name:
*|      stralloc - Dynamically allocates string from the concatenation
*|                 of the given strings.
*|
*| Interface:
*|      #include "mcidas.h"
*|
*|      char *
*|      stralloc(const char *s, ...)
*|
*| Input:
*|      s       - The first string.
*|      ...     - The remaining strings, ending with (char *)0
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0     - Failure, malloc() failed.
*|      <>0     - Success, pointer to the newly concatenated string.
*|
*| Remarks:
*|      This routine makes a new string by concatenating all the 
*|      strings given as arguments.
*|
*|      The last argument MUST be (char *)0, so that stralloc()
*|      knows where the end of the argument list is. For example:
*|
*|      str = stralloc(str1, str2, str3, ..., (char *)0);
*|
*|      The caller is responsible for free()ing the new string.
*|
*| Categories: 
*|      utility 
*/

extern char *
stralloc(const char *s, ...)
{
	va_list ap;

	const char *p = (char *)0;
	char *q = (char *)0;
	size_t l = 1;           /* for the terminal null */

	/* calculate the length of the string to allocate */

	va_start(ap, s);
	for(p = s; p != (char *)0; p = va_arg(ap, char *))
		l += strlen(p);
	va_end(ap);

	if((q = (char *)malloc(l)) == (char *)0)
		return (char *)0;
	*q = '\0';      /* initialize to null string */

	va_start(ap, s);
	for(p = s; p != (char *)0; p = va_arg(ap, char *))
		(void) strcat(q,p);
	va_end(ap);

	return q;
}
