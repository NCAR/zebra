/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 STRTOFS.C 3-Aug-94,18:11:52,`DWS' Initial release (4731)                */
/* 2 STRTOFS.C 22-Aug-94,6:52:14,`USER' Released                             */
/* 3 STRTOFS.C 19-Feb-96,16:13:36,`DWS' reglue: modified file                */
/* 4 STRTOFS.C 20-Feb-96,12:06:24,`USER' Released                            */
/* 5 STRTOFS.C 26-Aug-96,8:09:50,`BILLL' Added programmer documention        */
/*      (6653).                                                              */
/* 6 STRTOFS.C 6-Sep-96,10:20:26,`USER' Released                             */
/**** McIDAS Revision History *** */

#include <string.h>

#include "mcidas.h"

/*
*$ Name:
*$      strtofs  - Copies a string to a destination and deletes the end mark.
*$                 It will pad the destination with blanks if necessary.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      void
*$      strtofs( char *dst, char *src, int ndst)
*$
*$ Input:
*$      src      - Pointer to array containing string to be copied.
*$      ndst     - Number of characters in the string.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      dst      - Pointer to array receiving string.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      Output will look like a Fortran string.
*$
*$ Categories: 
*$      text
*$      utility
*$      converter
*/

void
strtofs(char *dst, const char *src, FsLen ndst)
{
	size_t nlhs;	/* number of characters in string */

	/* copy the string */
	if(ndst > 0)
	{
		strncpy(dst, src, ndst);
	}

	/* pad with spaces if necessary */
	nlhs = strlen(src);
	if(ndst > nlhs)
	{
		memset(dst + nlhs, SPACE, ndst - nlhs);
	}
}
