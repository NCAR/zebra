/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 MOVB.C 17-Sep-90,14:07:18,`SMG' Initial release                         */
/* 2 MOVB.C 11-Feb-91,14:45:54,`SMG' add conditional compile option          */
/* 3 MOVB.C 10-Jul-91,10:05:34,`SUEG' conform to ANSI standards              */
/* 4 MOVB.C 16-Feb-92,14:50:12,`USER' Released for McIDAS-X Only             */
/* 5 MOVB.C 1-Mar-94,10:05:46,`RUSSD' Standardize function names             */
/* 6 MOVB.C 31-Mar-94,23:49:36,`BARRYR' Add proprietary statement            */
/* 7 MOVB_.C 2-May-94,17:10:18,`USER' Released                               */
/* 8 MOVB_.C 9-May-95,18:27:22,`DWS' MCPATH phase 3 (5429)                   */
/* 9 MOVB_.C 6-Jun-95,15:10:38,`USER' Released                               */
/* 10 MOVB_.C 19-Feb-96,16:05:34,`DWS' reglue: modified file                 */
/* 11 MOVB_.C 20-Feb-96,12:01:28,`USER' Released                             */
/* 12 MOVB_.C 17-Jul-96,11:20:42,`BILLL' Added programmer documentation.     */
/* 13 MOVB_.C 6-Sep-96,10:18:46,`USER' Released                              */
/**** McIDAS Revision History *** */

#include <string.h>

#include "mcidas.h"
/*
*$ Name:
*$      movb      - Moves bytes from one array to another array with a 
*$                  destination byte offset.
*$
*$ Interface:
*$      subroutine
*$      movb(integer num, integer inbuffer(*), integer outbuffer(*),
*$           integer offset)
*$
*$ Input:
*$      num       - Number of bytes to move. 
*$      inbuffer  - Input array.
*$      offset    - Byte offset in output buffer, zero based.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      outbuffer  - Output array.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      If the number of bytes to move is less than 1 no bytes are
*$      transferred.
*$
*$ Categories: 
*$      utility 
*/

void
movb_(Fint *num, void *inbuffer, void *outbuffer, Fint *offset)
{
	Fint            off = *offset;
	unsigned char  *outbuf = outbuffer;

	memcpy(&outbuf[off], inbuffer, *num);
}
