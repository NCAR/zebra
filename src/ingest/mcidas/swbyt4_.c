/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History ****/
/* 1 SWBYT4.C 18-Sep-90,8:33:48,`SMG' Initial release                        */
/* 2 SWBYT4.C 10-Jan-91,9:27:02,`SMG' make a stub                            */
/* 3 SWBYT4.C 12-Feb-91,12:17:24,`SMG' add conditional compile option        */
/* 4 SWBYT4.C 11-Jul-91,9:26:44,`SUEG' conform to ANSI standards             */
/* 5 SWBYT4.C 15-Aug-91,11:10:06,`SUEG' added voids                          */
/* 6 SWBYT4.C 16-Feb-92,14:51:40,`USER' Released for McIDAS-X Only           */
/* 7 SWBYT4.C 1-Mar-94,10:07:48,`RUSSD' Standardize function names           */
/* 8 SWBYT4.C 31-Mar-94,23:50:44,`BARRYR' Add proprietary statement          */
/* 9 SWBYT4_.C 2-May-94,17:31:56,`USER' Released                             */
/* 10 SWBYT4_.C 19-Feb-96,16:14:10,`DWS' reglue: modified file               */
/* 11 SWBYT4_.C 20-Feb-96,12:06:58,`USER' Released                           */
/* 12 SWBYT4_.C 17-Jul-96,11:21:08,`BILLL' Added programmer documentation    */
/*      (6653).                                                              */
/* 13 SWBYT4_.C 6-Sep-96,10:20:38,`USER' Released                            */
/**** McIDAS Revision History ****/

#include "mcidas.h"
#include "mcidasp.h"

/*
*$ Name:
*$      swbyt4   - Re-orders bytes if internal representation is not
*$                 big-endian.
*$
*$ Interface:
*$      subroutine
*$      swbyt4(integer buf(*), integer n)
*$
*$ Input:
*$      n        - Number of 4 byte swaps to be made if order is not
*$                 big-endian.
*$
*$ Input and Output:
*$      buf      - Array containing bytes to be swapped.
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
*$      utility 
*/

void
swbyt4_(void *buf, Fint *n)
{
	/* determine byte order from first principles */
	union
	{
		char            bytes[sizeof(Mcint4)];
		Mcint4          word;
	}               q;

	q.word = 1;
	if (q.bytes[3] == 1)
		return;

	/* swap bytes */
	fbyte4_(buf, n);
}
