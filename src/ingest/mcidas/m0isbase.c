/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0ISBASE.C 19-Feb-96,15:16:54,`DWS' reglue: new file                    */
/* 2 M0ISBASE.C 20-Feb-96,12:10:50,`USER' Released                           */
/**** McIDAS Revision History *** */

#include <string.h>
#include <ctype.h>

#include "mcidasp.h"

/*
*| Name:
*|	M0isbasename - tests whether a pathname is a simple filename
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|	int
*|	M0isbasename(const char *pathname)
*|
*| Input:
*|	pathname - pathname to check
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	 1	- success
*|	 0	- failure
*|
*| Remarks:
*|	Returns true unless the pathname contains a '/' (Unix), or
*|	contains a '/' or a '\' or begins with a letter followed by
*|	a ':' (OS/2).
*|
*| Categories: 
*|	sys_config
*/

int
M0isbasename(const char *pathname)
{
	if (!pathname || !*pathname)
		return 1;

#ifdef __EMX__
	if (isalpha(pathname[0]) && pathname[1] == ':')
		return 0;

	if (strpbrk(pathname, "/\\"))
		return 0;
#else
	if (strchr(pathname, '/'))
		return 0;
#endif

	return 1;
}
