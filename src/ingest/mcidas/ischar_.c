/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */
/**** McIDAS Revision History *** */
/* 1 ISCHAR.C 18-Sep-90,9:33:36,`SMG' Initial release                        */
/* 2 ISCHAR.C 11-Feb-91,14:55:46,`SMG' add conditional compile option        */
/* 3 ISCHAR.C 9-Jul-91,14:21:54,`SUEG' conform to ANSI standards             */
/* 4 ISCHAR.C 16-Feb-92,14:52:54,`USER' Released for McIDAS-X Only           */
/* 5 ISCHAR.C 1-Mar-94,10:05:00,`RUSSD' Standardize function names           */
/* 6 ISCHAR.C 31-Mar-94,22:54:10,`BARRYR' Add proprietary statement          */
/* 7 ISCHAR_.C 2-May-94,16:49:08,`USER' Released                             */
/* 8 ISCHAR_.C 17-Jul-96,11:20:38,`BILLL' Added programmer documentation.    */
/* 9 ISCHAR_.C 6-Sep-96,10:17:34,`USER' Released                             */
/**** McIDAS Revision History *** */

/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/*
*$ Name:
*$      ischar  - Checks whether the 4 characters in the input field
*$                can be printed as ascii characters.
*$
*$ Interface:
*$      subroutine
*$      ischar(character*4 value)
*$
*$ Input:
*$      value   - Field containing four characters to be tested. 
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      1       - The values can be printed.
*$      0       - The values can not be printed.
*$
*$ Remarks:
*$      Important use info, algorithm, etc.
*$
*$ Categories: 
*$      utility 
*/

#include <ctype.h>

#include "mcidas.h"

/* Returns 1 if all four characters are ASCII printable, 0 otherwise */

Fint4 
ischar_(void *value)
{
	unsigned char *val;
	int i, ival;

	val=value;
	for (i = 0 ; i < 4; i++) {
	    ival = val[i];
	    if (isprint(ival) == 0) return(0);
	}
	return(1);
}
