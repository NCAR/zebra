/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 STRICMP.C 19-Feb-96,15:31:32,`DWS' reglue: new file                     */
/* 2 STRICMP.C 20-Feb-96,12:13:22,`USER' Released                            */
/* 3 STRICMP.C 17-Jul-96,11:21:02,`BILLL' Added programmer documentation     */
/*      (6653).                                                              */
/* 4 STRICMP.C 6-Sep-96,10:20:24,`USER' Released                             */
/**** McIDAS Revision History *** */

#include <ctype.h>
#include <assert.h>

#include "mcidas.h"

/*
*$ Name:
*$      Mcstricmp - Checks to see if two strings are equal.
*$                  Characters in each string can differ in case.
*$
*$ Interface:
*$      int
*$      Mcstricmp(char* s, char* t)
*$
*$ Input:
*$       s        - First string.
*$       t        - Second string.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0         - The strings are equal.
*$    <>0         - The relative difference between the strings.
*$
*$ Remarks:
*$      none
*$
*$ Categories: 
*$      utility 
*/

/* like strcmp(), but case-independent */
int Mcstricmp(const char *s, const char *t)
{
	size_t i;

	for (i = 0; tolower(s[i]) == tolower(t[i]); i++)
		if (s[i] == '\0')
			return 0;
	return tolower(s[i]) - tolower(t[i]);
}

/*
*$ Name:
*$      Mcstrnicmp - Checks to see if a subset of the characters in
*$                   two strings are equal.
*$                   The case of corresponding characters in each string
*$                   does not have to match.
*$
*$ Interface:
*$      Mcstrnicmp(char *s, char *t)
*$
*$ Input:
*$      s          - First string.
*$      t          - Second string.
*$      n          - Number of characters to compare, beginning at the first.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - The strings are equal.
*$    <>0       - The relative difference between the two substrings.
*$
*$ Remarks:
*$      none
*$
*$ Categories: 
*$      utility 
*/



/* like strncmp(), but case-independent */
int Mcstrnicmp(const char *s, const char *t, size_t n)
{
	size_t i;

	assert(n > 0);

	for (i = 0; i < n-1 && tolower(s[i]) == tolower(t[i]); i++)
		if (s[i] == '\0')
			return 0;
	return tolower(s[i]) - tolower(t[i]);
}
