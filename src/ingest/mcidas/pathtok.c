/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS-AIX Revision History *** */
/* 1 PATHTOK.C 7-Sep-93,10:48:36,`RUSSD' Initial release to AIX (4069)       */
/* 2 PATHTOK.C 5-Oct-93,8:11:46,`USER' Released for McIDAS-X Only            */
/* 3 PATHTOK.C 1-Mar-94,10:06:30,`RUSSD' Standardize function names          */
/* 4 PATHTOK.C 31-Mar-94,22:15:54,`BARRYR' Add proprietary statement         */
/* 5 PATHTOK.C 31-Mar-94,23:44:42,`BARRYR' Add proprietary statement         */
/* 6 PATHTOK.C 2-May-94,16:08:46,`USER' Released for McIDAS-X Only           */
/* 7 PATHTOK.C 19-Feb-96,16:08:56,`DWS' reglue: modified file                */
/* 8 PATHTOK.C 20-Feb-96,12:38:40,`USER' Released for McIDAS-X Only          */
/**** McIDAS-AIX Revision History *** */

#include <string.h>
#include <stdlib.h>

#include "mcidasp.h"

/*\
 * pathtok() - parse the given colon-separated path string.  Works
 * like strtok() - you call it with the path string first, then with
 * (char *)0 to keep working on the same string.  Each time you call
 * it, it returns a pointer to the next (possibly empty) path in the
 * string, and (char *)0 when there are no more paths in the string.
 * Note that like strtok() it writes NULs into the string you give it.
 * 
 * Unlike strtok() it does not treat colons like white space.
 * Implied directories in the path string appear as empty strings.
 * For example, it takes these initial paths to the listed sequence
 * of strings:
 *
 * ""		-> NULL
 * ":"		-> "", "", NULL
 * "a"		-> "a", NULL
 * "a:"		-> "a", "", NULL
 * ":a"		-> "", "a", NULL
 * "a::"	-> "a", "", "", NULL
 * "a::b"	-> "a", "", "b", NULL
 *
 * 1993 Apr 30	DaviD Sanderson (dws@ssec.wisc.edu)
\*/

char *
pathtok(char *str)
{
	static char    *wrk = (char *) 0;
	static int      endcolon = 0;
	char           *wrk0;
	char           *pc;

	if (str)
	{
		size_t          len;

		len = strlen(str);

		endcolon = 0;

		if (len && str[len - 1] == ':')
		{
			endcolon = 1;
		}

		wrk = str;
	}

	if (!wrk)
		return (char *) 0;

	wrk0 = wrk;

	/* if we find a colon, well and good */
	if ((pc = strchr(wrk, ':')) != (char *) 0)
	{
		*pc = 0;
		wrk = pc + 1;
		return wrk0;
	}

	/* by here, there are no more colons */

	wrk += strlen(wrk);

	if (wrk != wrk0)
		return wrk0;

	/* by here, the string is empty */

	if (endcolon)
	{
		/* return NULL next time */
		wrk = (char *) 0;

		return wrk0;
	}

	/*
	 * by here, the string is empty and the original string had
	 * no colon
	 */

	return (char *) 0;
}

#if 0
dotok(str)
	char           *str;
{
	char           *buf;
	char           *pbuf;
	char           *ans;

	puts("Begin:");

	buf = stralloc(str, (char *)0);
	pbuf = buf;

	while ((ans = pathtok(pbuf)) != (char *) 0)
	{
		if (!*ans)
			ans = ".";
		printf("\t'%s'\n", ans);
		pbuf = (char *) 0;
	}

	free(buf);
}
#endif
