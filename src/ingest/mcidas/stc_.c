/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */
/**** McIDAS Revision History *** */
/* 1 STC.C 17-Sep-90,15:05:02,`SMG' Initial release                          */
/* 2 STC.C 11-Feb-91,14:55:42,`SMG' add conditional compile option           */
/* 3 STC.C 11-Jul-91,9:19:40,`SUEG' conform to ANSI standards                */
/* 4 STC.C 16-Feb-92,14:53:54,`USER' Released for McIDAS-X Only              */
/* 5 STC.C 1-Mar-94,10:07:32,`RUSSD' Standardize function names              */
/* 6 STC.C 31-Mar-94,23:50:30,`BARRYR' Add proprietary statement             */
/* 7 STC_.C 2-May-94,17:30:48,`USER' Released                                */
/* 8 STC_.C 17-Jul-96,11:20:58,`BILLL' Added programmer documentation (6653) */
/* 9 STC_.C 6-Sep-96,10:20:16,`USER' Released                                */
/**** McIDAS Revision History *** */

/*
*$ Name:
*$      stc     - Places least significant byte of an integer into a 
*$                designated position in a character array.
*$
*$ Interface:
*$      subroutine
*$      stc(integer val, character*1(*) buffer, integer offset)
*$
*$ Input:
*$      val     - Word containing byte to be stored.
*$      offset  - Position in buffer that will receive the value.  
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      buffer  - Array into which byte is stored.
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

#include "mcidas.h"

void stc_(Fint4 *val, void *buffer, Fint4 *offset)
{
	unsigned char *buf;

	buf=buffer;
	buf[*offset] = *val;
}

