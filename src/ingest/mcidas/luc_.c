/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 LUC.C 25-Sep-90,13:47:26,`DAVES' First release                          */
/* 2 LUC.C 27-Sep-90,12:32:58,`DAVES' First release                          */
/* 3 LUC.C 11-Feb-91,14:55:22,`SMG' add conditional compile option           */
/* 4 LUC.C 9-Jul-91,15:08:10,`SUEG' conform to ANSI standards                */
/* 5 LUC.C 16-Feb-92,14:49:10,`USER' Released for McIDAS-X Only              */
/* 6 LUC.C 1-Mar-94,10:05:24,`RUSSD' Standardize function names              */
/* 7 LUC.C 31-Mar-94,23:49:08,`BARRYR' Add proprietary statement             */
/* 8 LUC_.C 2-May-94,16:59:38,`USER' Released                                */
/* 9 LUC_.C 23-Feb-95,11:38:24,`BARRYR' Fixed Revision History templates     */
/* 10 LUC_.C 27-Feb-95,9:42:38,`USER' Released                               */
/* 11 LUC_.C 4-Apr-95,10:21:24,`JUDYK' Added function Mcluc (C jacket).      */
/* 12 LUC_.C 5-May-95,11:03:14,`JMB' repackage (5401)                        */
/* 13 LUC_.C 5-May-95,15:06:58,`RICKK' Changed category (5401)               */
/* 14 LUC_.C 6-Jun-95,15:05:12,`USER' Released                               */
/* 15 LUC_.C 19-Feb-96,15:56:32,`DWS' reglue: modified file                  */
/* 16 LUC_.C 20-Feb-96,11:56:36,`USER' Released                              */
/* 17 LUC_.C 10-Jun-96,8:57:46,`BILLL' Added programmer documentation        */
/*      (6653).                                                              */
/* 18 LUC_.C 6-Sep-96,10:18:04,`USER' Released                               */
/**** McIDAS Revision History *** */

/*==============================luc.c=========================================*/

#include "mcidasp.h"
#include "m0glue.h"


#  define  OFFSET  1



/*==============================luc_==========================================*/
/*
*$ Name:
*$      luc   - Returns a value from user common.
*$
*$ Interface:
*$      integer function
*$      luc(integer index)
*$
*$ Input:
*$      index - Index into user common.  Points to value to be retrieved.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Designated user common value.
*$
*$ Remarks:
*$      Returns the value currently stored in McIDAS user common at the 
*$      designated index.
*$      For positive indexes, the value is from session based common;
*$      for negative indexes, it is from a process-based common.
*$      if uc is not initialized, all values are returned as 0
*$
*$ Categories:
*$      sys_config
*$      system
*/

/*
 *  these globals are the pointers to the memory blocks
 * 
 *  if they are zero, it means no setup has been done
 *  in which case luc_() will act as a source of zeroes
 *  and its companion puc_() will act as a null.
 */
Fint *m0posuc=0;
Fint *m0neguc=0;

/* os2 compatibility 
 *
 * initialization may set this global to 1
 * to cause the indexing to conform to the old "syscom"
 * convention, for SSEC device types
 * it remains 0 in McIDAS-X, and in PM display
 */
int m0uc_offset=0;

Fint 
luc_(Fint *index)
{
   /* if UC has not been initialized, all values are 0 */

   if (*index < OFFSET) 
      return  m0neguc ? m0neguc[- *index] : 0;
   else
      return  m0posuc ? m0posuc[*index -m0uc_offset] : 0;
}
/*==========================end luc_==========================================*/
