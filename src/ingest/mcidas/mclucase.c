/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 MCLUCASE.C 9-May-95,18:19:22,`DWS' MCPATH phase 3 (5429)                */
/* 2 MCLUCASE.C 6-Jun-95,15:09:00,`USER' Released                            */
/* 3 MCLUCASE.C 19-Feb-96,16:02:04,`DWS' reglue: modified file               */
/* 4 MCLUCASE.C 20-Feb-96,11:59:50,`USER' Released                           */
/* 5 MCLUCASE.C 16-Jul-96,8:08:02,`BILLL' Added programmer documentation     */
/* 6 MCLUCASE.C 6-Sep-96,10:18:26,`USER' Released                            */
/**** McIDAS Revision History *** */

#include <ctype.h>

#include "mcidas.h"
#include "mcidasp.h"

/*
*$ Name:
*$	Mclocase - Converts all characters in a string to lower case.
*$
*$ Interface:
*$	#include "mcidas.h"
*$
*$	void
*$	Mclocase(char *string)
*$
*$ Input:
*$	none
*$
*$ Input and Output:
*$	string	- String to be converted.
*$
*$ Output:
*$	none
*$
*$ Return values:
*$	none
*$
*$ Remarks:
*$	none
*$
*$ Categories: 
*$	conversion
*/

void
Mclocase(char *string)
{
	for (; *string; string++)
	{
		if (isascii(*string) && isupper(*string))
			*string = tolower(*string);
	}
}

/*
*$ Name:
*$	mclocase - Converts all characters in a string to lower case.
*$
*$ Interface:
*$	subroutine 
*$	mclocase(character*(*) string)
*$
*$ Input:
*$	none
*$
*$ Input and Output:
*$	string	- String to be converted.
*$
*$ Output:
*$	none
*$
*$ Return values:
*$	none
*$
*$ Remarks:
*$	none
*$
*$ Categories: 
*$	conversion
*/

void
mclocase_(char *string, FsLen string_len)
{
	FsLen i;

	for (i = 0; i < string_len; i++, string++)
	{
		if (isascii(*string) && isupper(*string))
			*string = tolower(*string);
	}
}

/*
*$ Name:
*$	Mcupcase - Converts all characters in a string to upper case.
*$
*$ Interface:
*$	#include "mcidas.h"
*$
*$	void
*$	Mcupcase(char *string)
*$
*$ Input:
*$	none
*$
*$ Input and Output:
*$	string	- string to be converted
*$
*$ Output:
*$	none
*$
*$ Return values:
*$	none
*$
*$ Remarks:
*$	none
*$
*$ Categories: 
*$	conversion
*/

void
Mcupcase(char *string)
{
	for (; *string; string++)
	{
		if (isascii(*string) && islower(*string))
			*string = toupper(*string);
	}
}

/*
*$ Name:
*$	mcupcase - Converts all characters in a string to upper case.
*$
*$ Interface:
*$	subroutine 
*$	mcupcase(character*(*) string)
*$
*$ Input:
*$	none
*$
*$ Input and Output:
*$	string	- String to be converted.
*$
*$ Output:
*$	none
*$
*$ Return values:
*$	none
*$
*$ Remarks:
*$	none
*$
*$ Categories: 
*$	conversion
*/

void
mcupcase_(char *string, FsLen string_len)
{
	FsLen i;

	for (i = 0; i < string_len; i++, string++)
	{
		if (isascii(*string) && islower(*string))
			*string = toupper(*string);
	}
}
