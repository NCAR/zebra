/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 PUC.C 25-Sep-90,13:47:28,`DAVES' First release                          */
/* 2 PUC.C 27-Sep-90,12:33:02,`DAVES' First release                          */
/* 3 PUC.C 11-Feb-91,14:55:22,`SMG' add conditional compile option           */
/* 4 PUC.C 5-Mar-91,9:38:14,`DAVES' Add CURAIX                               */
/* 5 PUC.C 6-Mar-91,9:24:48,`SMG' fix if block                               */
/* 6 PUC.C 11-Jul-91,7:50:50,`SUEG' conform to ANSI standards                */
/* 7 PUC.C 19-Sep-91,14:33:02,`SUEG' remove sendevnt call                    */
/* 8 PUC.C 16-Feb-92,14:49:02,`USER' Released for McIDAS-X Only              */
/* 9 PUC.C 1-Mar-94,10:06:36,`RUSSD' Standardize function names              */
/* 10 PUC.C 31-Mar-94,23:50:10,`BARRYR' Add proprietary statement            */
/* 11 PUC_.C 2-May-94,17:21:14,`USER' Released                               */
/* 12 PUC_.C 23-Feb-95,11:40:02,`BARRYR' Fixed Revision History templates    */
/* 13 PUC_.C 27-Feb-95,9:44:02,`USER' Released                               */
/* 14 PUC_.C 19-Feb-96,16:09:48,`DWS' reglue: modified file                  */
/* 15 PUC_.C 20-Feb-96,12:03:38,`USER' Released                              */
/* 16 PUC_.C 22-Mar-96,12:01:46,`DWS' reglue: modified file                  */
/* 17 PUC_.C 25-Mar-96,14:01:26,`USER' Released                              */
/* 18 PUC_.C 30-Jul-96,13:33:58,`BILLL' Added programmer documentation       */
/*      (6653).                                                              */
/* 19 PUC_.C 21-Aug-96,10:45:54,`BILLL' Fixed column fault in help.          */
/* 20 PUC_.C 6-Sep-96,10:19:48,`USER' Released                               */
/**** McIDAS Revision History *** */

#include <stdlib.h>
#include <assert.h>
#include "mcidasp.h"
#include "m0glue.h"

#define OFFSET 1

/*
 * If an attempt is made to store into an uninitialized positive
 * UC or negative UC, allocate a process-local copy.
 */

#include "mcidas.h"

/*
*$ Name:
*$      puc     - Places a value in user common.
*$
*$ Interface:
*$      subroutine puc(integer value, integer index)
*$
*$ Input:
*$      value   - The value to be stored.
*$      index   - The location in user common where the value is stored.
*$
*$ Input and Output:
*$      none
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
*$      system
*/


void
puc_(Fint *value, Fint *index)
{
	if (*index < OFFSET)
	{
		/* if we need a negative UC and lack one, make one */
		if (m0neguc == (Fint *) 0)
		{
			m0neguc = calloc(800, 1);
			assert(m0neguc);
		}
		m0neguc[-(*index)] = *value;
	}
	else
	{
		/* if we need a positive UC and lack one, make one */
		if (m0posuc == (Fint *) 0)
		{
			m0posuc = calloc(65536, 1);
			assert(m0posuc);
		}
		m0posuc[(*index) - m0uc_offset] = *value;
	}
}
