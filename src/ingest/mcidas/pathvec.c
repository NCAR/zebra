/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS-AIX Revision History *** */
/* 1 PATHVEC.C 7-Sep-93,10:49:48,`RUSSD' Initial release to AIX (4069)       */
/* 2 PATHVEC.C 5-Oct-93,8:11:52,`USER' Released for McIDAS-X Only            */
/* 3 PATHVEC.C 1-Mar-94,10:06:34,`RUSSD' Standardize function names          */
/* 4 PATHVEC.C 31-Mar-94,22:15:56,`BARRYR' Add proprietary statement         */
/* 5 PATHVEC.C 31-Mar-94,23:44:44,`BARRYR' Add proprietary statement         */
/* 6 PATHVEC.C 2-May-94,16:08:54,`USER' Released for McIDAS-X Only           */
/* 7 PATHVEC.C 19-Feb-96,16:09:02,`DWS' reglue: modified file                */
/* 8 PATHVEC.C 20-Feb-96,12:38:46,`USER' Released for McIDAS-X Only          */
/**** McIDAS-AIX Revision History *** */

#include <stdlib.h>
#include <string.h>

#include "mcidasp.h"

/*
 * pathvec() - return the vector of strings constructed from
 * the given colon-separated path string.
 *
 * Returns (char **)0 on error.
 */

char   **
pathvec(const char *pathstr)
{
	char          **vec;

	/*
	 * pathstr - we should not modify this string. If, for
	 * instance, it happens to be in the process environment,
	 * then altering it would affect child processes.
	 * 
	 * buf - points to local copy of pathstr.
	 */
	char           *buf;
	char           *pbuf;
	char           *ans;

	if (!pathstr || !*pathstr)
		return (char **) 0;

	/*
	 * Build a vector of strings in pathstr.
	 */

	if((vec = VecNew()) == (char **)0)
		return (char **) 0;

	if((buf = stralloc(pathstr, (char *)0)) == (char *)0)
		return (char **) 0;

	pbuf = buf;

	while ((ans = pathtok(pbuf)) != (char *) 0)
	{
		pbuf = (char *) 0;

		vec = VecAdd(vec, ans);
	}

	/*
	 * Since VecAdd makes its own copy of the strings, we can
	 * free our local copy of pathstr.
	 */
	free(buf);

	return vec;
}

/*
 * dirpathvec() - return the vector of directory names constructed from
 * the given vector of strings.
 *
 * If a string in the source vector is null, put a "." in the
 * constructed vector.
 *
 * If the source vector does not exist or is empty, return a vector
 * containing the single string ".".
 *
 * Returns (char **)0 on error.
 */

char   **
dirpathvec(char **srcvec)
{
	static char dot[] = ".";
	char          **vec;

	if((vec = VecNew()) == (char **)0)
		return (char **) 0;

	if(!srcvec || !*srcvec)
	{
		return VecAdd(vec, dot);
	}

	for(; *srcvec; srcvec++)
	{
		char *str;

		str = ((**srcvec) ? *srcvec : dot);

		vec = VecAdd(vec, str);
	}

	return vec;
}

/*
 * envdirpathvec() - return the vector of directory names constructed
 * from the value of the given environment variable
 * 
 * If the environment variable is not set or is null, pretend the
 * variable was set to "." and return the appropriate vector.
 *
 * Returns (char **)0 on error.
 */

char   **
envdirpathvec(const char *name)
{
	char          **strvec;
	char          **dirvec;

	/*
	 * env - points to value of environment variable. We must not
	 * modify this string, since modifying it (i.e. by writing
	 * NULs into it) will affect child processes.
	 */
	char           *env = (char *) 0;

	if (!name || !*name)
		return (char **) 0;

	/*
	 * Build a vector of directories in the value of the given
	 * environment variable.
	 */

	if (((env = getenv(name)) == (char *) 0) || strlen(env) == 0)
	{
		static char dot[] = ".";

		env = dot;
	}

	if ((strvec = pathvec(env)) == (char **) 0)
	{
		return (char **) 0;
	}

	dirvec = dirpathvec(strvec);

	/*
	 * clean up
	 */

	VecOld(strvec);

	return dirvec;
}
