/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */
/**** McIDAS Revision History *** */
/* 1 IC.C 1-Mar-94,14:09:38,`RUSSD' Standardize function name                */
/* 2 IC.C 31-Mar-94,22:52:20,`BARRYR' Add proprietary statement              */
/* 3 IC_.C 2-May-94,16:44:16,`USER' Released                                 */
/* 4 IC_.C 17-Jul-96,11:20:36,`BILLL' Added programmer documentation.        */
/* 5 IC_.C 6-Sep-96,10:17:24,`USER' Released                                 */
/**** McIDAS Revision History *** */

#include "mcidas.h"

/*
*$ Name:
*$      ic      - Retrieves a designated byte in an array.
*$
*$ Interface:
*$      subroutine
*$      ic(integer buffer(*), integer offset)
*$
*$ Input:
*$      buffer  - Array from which byte is retrieved.
*$      offset  - Offset of byte to be retrieved in designated array.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Value of byte which is being retrieved.
*$
*$ Remarks:
*$      none
*$
*$ Categories: 
*$      utility 
*/

Fint4 
ic_(void *buffer, Fint4 *offset)
{
   unsigned char *buf;

   buf=buffer;
   return( (Fint4) buf[*offset]);
}

