/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 PATHCAT.C 7-Sep-93,10:49:14,`RUSSD' Initial release to AIX              */
/* 2 PATHCAT.C 5-Oct-93,8:11:40,`USER' Released for McIDAS-X Only            */
/* 3 PATHCAT.C 1-Mar-94,10:06:26,`RUSSD' Standardize function names          */
/* 4 PATHCAT.C 31-Mar-94,22:15:48,`BARRYR' Add proprietary statement         */
/* 5 PATHCAT.C 31-Mar-94,23:44:38,`BARRYR' Add proprietary statement         */
/* 6 PATHCAT.C 2-May-94,16:08:38,`USER' Released for McIDAS-X Only           */
/* 7 PATHCAT.C 6-Jun-95,15:33:58,`RICKK' Changed Revision history cards -    */
/*      Moved from aix to com.                                               */
/* 8 PATHCAT.C 6-Jun-95,15:34:56,`USER' Released                             */
/* 9 PATHCAT.C 19-Feb-96,16:08:44,`DWS' reglue: modified file                */
/* 10 PATHCAT.C 20-Feb-96,12:02:54,`USER' Released                           */
/* 11 PATHCAT.C 22-Mar-96,12:00:12,`DWS' reglue: modified file               */
/* 12 PATHCAT.C 25-Mar-96,14:00:12,`USER' Released                           */
/**** McIDAS Revision History *** */

#include <sys/types.h>
#include <string.h>
#include <stdlib.h>

#include "mcidas.h"

/*
 * pathcat() - returns a pointer to the concatenation of the two
 * given strings separated by a '/'.
 * 
 * The return value may point to a static buffer overwritten on each
 * call.
 */

const char *
pathcat(const char *prefix, const char *suffix)
{
	static char    *pathbuf = (char *) 0;
	static size_t   pathlen = 0;

	char           *s;
	size_t          plen;
	size_t          slen;
	size_t          len;
	int		addslash;

	if (*prefix == '\0')
		return suffix;
	if (*suffix == '\0')
		return prefix;

	plen = strlen(prefix);
	slen = strlen(suffix);
	len = plen + slen + 2;	/* one for '/', one for '\0' */
	if (pathlen < len)
	{
		pathlen = len;
		pathbuf = pathbuf ? realloc(pathbuf, pathlen) : malloc(pathlen);

		if (!pathbuf)
		{
			return (char *) 0;
		}
	}

	memcpy(pathbuf, prefix, plen);
	s = pathbuf + plen;

	/*
	 * Append a slash unless the directory already ends with one.
	 * In OS/2 we have to look for both slash and backslash.
	 */
	addslash = 1;
#ifdef __EMX__
	if (s[-1] == '\\')
		addslash = 0;
#endif
	if (s[-1] == '/')
		addslash = 0;
	if (addslash)
		*s++ = '/';

	memcpy(s, suffix, slen + 1);
	return pathbuf;
}
