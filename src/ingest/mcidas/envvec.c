/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS-AIX Revision History *** */
/* 1 ENVVEC.C 7-Sep-93,10:46:58,`RUSSD' Initial release to AIX (4069)        */
/* 2 ENVVEC.C 5-Oct-93,8:11:22,`USER' Released for McIDAS-X Only             */
/* 3 ENVVEC.C 1-Mar-94,10:03:38,`RUSSD' Standardize function names           */
/* 4 ENVVEC.C 31-Mar-94,22:15:28,`BARRYR' Add proprietary statement          */
/* 5 ENVVEC.C 31-Mar-94,23:44:18,`BARRYR' Add proprietary statement          */
/* 6 ENVVEC.C 2-May-94,15:58:06,`USER' Released for McIDAS-X Only            */
/* 7 ENVVEC.C 19-Feb-96,15:45:06,`DWS' reglue: modified file                 */
/* 8 ENVVEC.C 20-Feb-96,12:33:40,`USER' Released for McIDAS-X Only           */
/**** McIDAS-AIX Revision History *** */

#include "mcidasp.h"

/*
 * PATHvec() - returns a vector of the directories listed in the
 * PATH environment variable.
 * 
 * Returns (char **)0 on failure.
 * 
 * The vector is only computed once, no matter how many times this
 * routine is called.
 */

char **
PATHvec(void)
{
	/* vector of directory names in PATH */
	static char   **vec = (char **) 0;

	/* controls initialization */
	static int      first = 1;

	if (first)
	{
		first = 0;

		vec = envdirpathvec("PATH");
	}

	return vec;
}

/*
 * MCPATHvec() - returns a vector of the directories listed in the
 * MCPATH environment variable.
 * 
 * Returns (char **)0 on failure.
 * 
 * The vector is only computed once, no matter how many times this
 * routine is called.
 */

char **
MCPATHvec(void)
{
	/* vector of directory names in MCPATH */
	static char   **vec = (char **) 0;

	/* controls initialization */
	static int      first = 1;

	if (first)
	{
		first = 0;

		vec = envdirpathvec("MCPATH");
	}

	return vec;
}
