/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 PATHNAME.C 1-May-95,16:28:54,`DWS' McIDAS filename resolver (5402)      */
/* 2 PATHNAME.C 2-Jun-95,16:42:56,`DWS' fix free() problem in VOLNAM() (5402)*/
/* 3 PATHNAME.C 6-Jun-95,15:11:54,`USER' Released                            */
/* 4 PATHNAME.C 19-Feb-96,16:08:52,`DWS' reglue: modified file               */
/* 5 PATHNAME.C 20-Feb-96,12:03:00,`USER' Released                           */
/**** McIDAS Revision History *** */

#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "mcidas.h"
#include "mcidasp.h"

/*
*$ Name:
*$	Mcpathname - determines the system pathname of a McIDAS file
*$
*$ Interface:
*$	#include "mcidas.h"
*$
*$	const char *
*$	Mcpathname(const char *filename)
*$
*$ Input:
*$	filename - name of the file
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	none
*$
*$ Return values:
*$	  0	 - failure, could not find the file
*$	<>0	 - success, pointer to full pathname of the file
*$
*$ Remarks:
*$	The return pointer may point to a static data area overwritten
*$	on each call.
*$
*$	The return pointer may also point to the filename argument.
*$
*$	On OS/2, if the file is not found via REDIRECT, the return
*$	value is simply the filename.
*$
*$	In Fortran, use volnam().
*$
*$ Categories: 
*$	file 
*/

const char *
Mcpathname(const char *file)
{
	const char *path;

	if (!file || !*file)
	{
		return (char *)0;
	}

	if ((path = M0redirect(file)) != (char *)0)
	{
		return pathcat(path, file);
	}

#ifdef	__EMX__

	return file;

#else	/* __EMX__ */

	if ((path = M0mcpath(file)) != (char *)0)
	{
		return path;
	}

	if ((path = M0defpath(file)) != (char *)0)
	{
		return path;
	}

	return (char *)0;

#endif	/* __EMX__ */
}

/*
*$ Name:
*$	volnam - determines the system pathname of a McIDAS file
*$
*$ Interface:
*$	integer function
*$	volnam(character*(*) filename, character*(*) pathname)
*$
*$ Input:
*$	filename - name of the file
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	pathname - full pathname to use for the file
*$
*$ Return values:
*$	<=0	 - error
*$	 >0	 - success, determined a pathname to use for the file
*$
*$ Remarks:
*$	volnam() fails if the pathname buffer is not large enough.
*$
*$	On OS/2, if the file is not found via REDIRECT, the pathname
*$	is simply the filename.
*$
*$	In C, use Mcpathname().
*$ 
*$ Categories: 
*$	file 
*/

Fint
volnam_(char *file, char *path, FsLen nfile, FsLen npath)
{
	char           *sfile;	/* 'file' as a C string */
	const char     *pathname;	/* answer from Mcpathname() */

	if ((sfile = fsalloc(file, nfile)) == (char *) 0)
		return -1;
	pathname = Mcpathname(sfile);

	/*
	 * Since Mcpathname may return sfile, we cannot free sfile
	 * until we are done with pathname.
	 */

	if (pathname == (char *) 0)
	{
		free(sfile);
		return -2;
	}
	if ((FsLen) strlen(pathname) > npath)
	{
		free(sfile);
		return -3;
	}

	strtofs(path, pathname, npath);
	free(sfile);

	return 1;
}
