/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS-AIX Revision History *** */
/* 1 PVSEARCH.C 7-Sep-93,10:50:32,`RUSSD' Initial release to AIX (4069)      */
/* 2 PVSEARCH.C 5-Oct-93,8:12:04,`USER' Released for McIDAS-X Only           */
/* 3 PVSEARCH.C 1-Mar-94,10:06:48,`RUSSD' Standardize function names         */
/* 4 PVSEARCH.C 31-Mar-94,22:16:06,`BARRYR' Add proprietary statement        */
/* 5 PVSEARCH.C 31-Mar-94,23:44:54,`BARRYR' Add proprietary statement        */
/* 6 PVSEARCH.C 2-May-94,16:09:54,`USER' Released for McIDAS-X Only          */
/* 7 PVSEARCH.C 19-Feb-96,16:09:58,`DWS' reglue: modified file               */
/* 8 PVSEARCH.C 20-Feb-96,12:39:06,`USER' Released for McIDAS-X Only         */
/**** McIDAS-AIX Revision History *** */ 

#include <string.h>

#include "mcidasp.h"

/*
 * pathvecsearch() searches for a file named 'name' of the given type
 * along the given path vector, accessible with the given mode.
 *
 * The 'type' and 'amode' arguments are exactly as in eaccess().
 *
 * If the name contains a '/' no path search occurs.
 * 
 * Returns a pointer to the full pathname of the file on success, and
 * (char *)0 if it cannot find the name.  The return value may point
 * to a static buffer which is overwritten on each call.
 */

const char *
pathvecsearch(char **pathvec, const char *name, int amode, int type)
{
	char          **v;

	if (!M0isbasename(name))
	{
		return (eaccess(name, amode, type) == 0) ? name : (char *) 0;
	}

	for (v = pathvec; v && *v; v++)
	{
		const char     *path;

		if ((path = pathcat(*v, name)) == (char *) 0)
		{
			continue;
		}
		if (eaccess(path, amode, type) == 0)
		{
			return path;
		}
	}

	return (char *) 0;
}
