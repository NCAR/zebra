/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 MOVW.C 17-Sep-90,15:12:34,`SMG' Initial release                         */
/* 2 MOVW.C 11-Feb-91,14:55:44,`SMG' add conditional compile option          */
/* 3 MOVW.C 6-Mar-91,10:10:50,`DAVES' fix conditional                        */
/* 4 MOVW.C 10-Jul-91,13:21:32,`SUEG' ta e mpixel.c                          */
/* 5 MOVW.C 16-Feb-92,14:53:32,`USER' Released for McIDAS-X Only             */
/* 6 MOVW.C 1-Mar-94,10:06:02,`RUSSD' Standardize function names             */
/* 7 MOVW.C 31-Mar-94,23:49:56,`BARRYR' Add proprietary statement            */
/* 8 MOVW_.C 2-May-94,17:11:30,`USER' Released                               */
/* 9 MOVW_.C 9-May-95,18:27:48,`DWS' MCPATH phase 3 (5429)                   */
/* 10 MOVW_.C 6-Jun-95,15:10:54,`USER' Released                              */
/* 11 MOVW_.C 19-Feb-96,16:06:42,`DWS' reglue: modified file                 */
/* 12 MOVW_.C 20-Feb-96,12:01:40,`USER' Released                             */
/* 13 MOVW_.C 17-Jul-96,11:20:50,`BILLL' Added programmer documentation (6653*/
/* 14 MOVW_.C 6-Sep-96,10:19:14,`USER' Released                              */
/**** McIDAS Revision History *** */

#include <string.h>

#include "mcidas.h"
/*
*$ Name:
*$      movw    - Moves 4 byte words from one array to another array.
*$
*$ Interface:
*$      subroutine
*$      movw(integer num, integer inbuf(*), integer outbuf(*))
*$
*$ Input:
*$      num     - Number of words to move.
*$      inbuf   - Input array.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      outbuf  - Output array.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      If the number of bytes to move is less than 1 then no
*$      words are transferred.
*$
*$ Categories: 
*$      utility 
*/

void
movw_(Fint *num, void *inbuf, void *outbuf)
{
	memcpy(outbuf, inbuf, 4 * (*num));
}
