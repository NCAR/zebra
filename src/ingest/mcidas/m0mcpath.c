/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS-AIX Revision History *** */
/* 1 M0MCPATH.C 5-Apr-95,13:52:52,`DWS' MCPATH phase 2 (5333)                */
/* 2 M0MCPATH.C 10-Apr-95,8:57:06,`RICKK' Included mcidassp.h.               */
/* 3 M0MCPATH.C 2-Jun-95,16:40:18,`DWS' remove temporary code (5333)         */
/* 4 M0MCPATH.C 6-Jun-95,14:52:40,`USER' Released for McIDAS-X Only          */
/* 5 M0MCPATH.C 19-Feb-96,15:58:30,`DWS' reglue: modified file               */
/* 6 M0MCPATH.C 20-Feb-96,12:36:28,`USER' Released for McIDAS-X Only         */
/**** McIDAS-AIX Revision History *** */

#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "mcidas.h"
#include "mcidasp.h"

/*
*| Name:
*|	M0mcpath - locate a file in the MCPATH directories
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|	const char *
*|	M0mcpath(const char *filename)
*|
*| Input:
*|	filename - name of the file
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	  0	 - failure, could not find the file
*|	<>0	 - success, pointer to full pathname of the file
*|
*| Remarks:
*|	M0mcpath() returns the pathname of the given file if it exists
*|	in the directories listed in the MCPATH environment variable.
*| 
*|	If the filename contains a '/', M0mcpath() simply returns a
*|	pointer to it and does not do any MCPATH resolution for it.
*| 
*|	The return pointer may point to a static data area overwritten
*|	on each call.
*|
*| Categories: 
*|	file 
*/

const char     *
M0mcpath(const char *filename)
{
	char          **vec = MCPATHvec();

	if (!vec || !filename || !*filename)
		return (char *) 0;

	/* attempt to locate the file */

	{
		const char     *ans;

		ans = pathvecsearch(vec, filename, F_OK, S_IFREG);

		if (ans)
		{
			return ans;
		}
	}

	/* otherwise give up */

	return (char *) 0;
}


/*
** Name:
**	firstwritable - returns first writable directory in given vector
**
** Interface:
**	char *
**	firstwritable(char **dirvec)
**
** Input:
**	dirvec  - vector of directory names
**
** Input and Output:
**	none
**
** Output:
**	none
**
** Return values:
**	  0	- failure, could not find a writable directory in the
**		  given vector
**	<>0	- success, pointer to pathname in the given vector
**
** Remarks:
**	The pointer points to one of the actual strings in the argument
**	vector.
**
** Categories: 
**	none
*/

static char    *
firstwritable(char **dirvec)
{
	if (!dirvec || !*dirvec)
		return (char *) 0;

	/*
	 * To create a file in a directory we must have write AND
	 * execute (search) permission.
	 */

	for (; *dirvec; dirvec++)
	{
		if (eaccess(*dirvec, W_OK | X_OK, S_IFDIR) == 0)
			return *dirvec;
	}

	return (char *) 0;
}


/*
** Name:
**	getdefdir - returns the first writable MCPATH directory
**
** Interface:
**	char *
**	getdefdir(void)
**
** Input:
**	none
**
** Input and Output:
**	none
**
** Output:
**	none
**
** Return values:
**	  0	- failure, could not find a writable directory in MCPATH
**	<>0	- success, pointer to name of the first writable
**		  directory in MCPATH
**
** Remarks:
**	This directory is only computed once, no matter how many times
**	this routine is called.
**
**	This is the directory where new files (whose names do not match
**	a REDIRECT pattern) are put.
**
** Categories: 
**	none 
*/

static char    *
getdefdir(void)
{
	static int      first = 1;

	/*
	 * points to name of first writeable directory in MCPATH, if
	 * any
	 */
	static char    *dirname = (char *) 0;

	if (first)
	{
		first = 0;

		dirname = firstwritable(MCPATHvec());
	}

	return dirname;
}


/*
*| Name:
*|	M0defpath - determines the default pathname of a file
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|	const char *
*|	M0defpath(const char *filename)
*|
*| Input:
*|	filename - name of the file
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	  0	 - failure, could not find a directory for the file
*|	<>0	 - success, pointer to full pathname to use for the file
*|
*| Remarks:
*|	M0defpath() returns the pathname of the given file as if it
*|	existed in the first writeable directory in MCPATH.
*| 
*|	If the filename contains a '/', M0defpath() simply returns a
*|	pointer to it and does not do any MCPATH resolution for it.
*| 
*|	The return pointer may point to a static data area overwritten
*|	on each call.
*|
*| Categories: 
*|	file 
*/

const char     *
M0defpath(const char *filename)
{
	if (!filename || !*filename)
	{
		return (char *) 0;
	}

	if (!M0isbasename(filename))
	{
		return filename;
	}

	/*
	 * If there is a writeable directory in MCPATH, put the file
	 * in that directory.
	 */

	{
		char           *defdir = getdefdir();

		if (defdir && *defdir)
		{
			return pathcat(defdir, filename);
		}
	}

	/* otherwise give up */

	return (char *) 0;
}
