/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/* old McIDAS-AIX revision history */
/* 1 VEC.C 7-Sep-93,10:52:24,`RUSSD' Initial release to AIX (4069)           */
/* 2 VEC.C 5-Oct-93,8:12:16,`USER' Released for McIDAS-X Only                */
/* 3 VEC.C 1-Mar-94,10:08:00,`RUSSD' Standardize function names              */
/* 4 VEC.C 31-Mar-94,22:16:52,`BARRYR' Add proprietary statement             */
/* 5 VEC.C 31-Mar-94,23:45:54,`BARRYR' Add proprietary statement             */
/* 6 VEC.C 2-May-94,16:16:14,`USER' Released for McIDAS-X Only               */
/* 7 VEC.C 9-May-95,18:28:46,`DWS' MCPATH phase 3 (5429)                     */
/* 8 VEC.C 6-Jun-95,14:56:14,`USER' Released for McIDAS-X Only               */
/* old McIDAS-AIX revision history */

/**** McIDAS Revision History *** */
/* 1 VEC.C 7-Sep-93,10:52:24,`RUSSD' Initial release to AIX (4069)           */
/* 2 VEC.C 5-Oct-93,8:12:16,`USER' Released for McIDAS-X Only                */
/* 3 VEC.C 1-Mar-94,10:08:00,`RUSSD' Standardize function names              */
/* 4 VEC.C 31-Mar-94,22:16:52,`BARRYR' Add proprietary statement             */
/* 5 VEC.C 31-Mar-94,23:45:54,`BARRYR' Add proprietary statement             */
/* 6 VEC.C 2-May-94,16:16:14,`USER' Released for McIDAS-X Only               */
/* 7 VEC.C 9-May-95,18:28:46,`DWS' MCPATH phase 3 (5429)                     */
/* 8 VEC.C 6-Jun-95,14:56:14,`USER' Released for McIDAS-X Only               */
/* 9 VEC.C 19-Feb-96,15:06:36,`DWS' reglue: moved from AIX to COM            */
/* 10 VEC.C 20-Feb-96,12:42:56,`USER' Released for McIDAS-X Only             */
/**** McIDAS Revision History *** */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "mcidas.h"

/*
 * v e c . c - Vector routines 
 *
 * These routines implement vector of pointers to strings. The last
 * pointer is always 0. 
 *
 */

/*
*| Name:
*|      VecNew - allocate a new vector
*|
*| Interface:
*|      #include "mcidas.h"
*|
*|      char **
*|      VecNew(void)
*|
*| Input:
*|      none
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0      - failure, malloc() failed
*|      <>0      - success, pointer to new, empty vector
*|
*| Remarks:
*|      The returned vector contains only the terminal NULL pointer.
*|
*| Categories: 
*|      utility 
*/

char **
VecNew(void)
{
	char **vec;

        /* allocate space for an array of 1 pointer - the terminal null
         * pointer
         */
	if((vec = (char **) malloc(sizeof(char *))) == (char **)0)
        {
		return (char **)0;
        }

	vec[0] = (char *) 0;	/* start with empty vector */

	return vec;
}

/*
*| Name:
*|      VecLen - return number of pointers in vector
*|
*| Interface:
*|      #include "mcidas.h"
*|
*|      int
*|      VecLen(char **vec)
*|
*| Input:
*|      vec     - vector to count
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|       -1     - failure, bad vector
*|      >=0     - success, number of pointers in the vector
*|
*| Remarks:
*|      The terminal NULL pointer is not included in the count.
*|
*| Categories: 
*|      utility 
*/

int 
VecLen(char **vec)
{
	int ans = 0;

	if(!vec)
	{
		return -1;
	}

	while(*vec++)
	{
		ans++;
	}

	return ans;
}

/*
*| Name:
*|      VecOld - delete the vector (free its storage)
*|
*| Interface:
*|      #include "mcidas.h"
*|
*|      int
*|      VecOld(char **vec)
*|
*| Input:
*|      vec     - vector to delete
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      -1       - failure, bad vector
*|       0       - success
*|
*| Remarks:
*|      none
*|
*| Categories: 
*|      utility 
*/

int 
VecOld(char **vec) 
{
	char **vec0 = vec;

	if(!vec)
	{
		return -1;
	}

	for(; *vec; vec++)
	{
		free(*vec);
	}
	free((char *)vec0);

	return 0;
}

/*
*| Name:
*|      VecAdd - append a copy of the given string to the given vector
*|
*| Interface:
*|      #include "mcidas.h"
*|
*|      char **
*|      VecAdd(char **vec, const char *s)
*|
*| Input:
*|      vec     - vector to update (invalidated by VecAdd())
*|      s       - string to add to vec
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0      - failure, bad vector or memory allocation failure
*|      <>0      - success, pointer to new vector
*|
*| Remarks:
*|      VecAdd() appends a copy of the string pointed to by s to the
*|      end of the given vector, and returns a pointer to the new
*|      vector.
*|
*|      After VecAdd() returns, the original vec pointer may be invalid
*|      because realloc() may have relocated the storage it pointed to.
*|      You should use the return value instead.  For example:
*|
*|      vec = VecAdd(vec, "string");
*|
*| Categories: 
*|      utility 
*/

char **
VecAdd(char **vec, const char *s)
{
	int veclen;
	char *copy;

	if(!vec || !s)
	{
		return (char **)0;
	}

	if((copy = stralloc(s, (char *)0)) == (char *)0)
	{
		return (char **)0;
	}

	veclen = VecLen(vec);

	/*
	 * Append the pointer.  We always have room for this since
	 * there is always a 0 pointer at the very end. 
	 */
	vec[veclen++] = copy;	/* logical vector grows by one */

	/*
	 * Expand the physical buffer if necessary to hold the
	 * terminal NUL. 
	 */
	vec = (char **) realloc((char *) vec, (veclen+1) * sizeof(char *));
	if(!vec)
        {
		return (char **)0;
	}

	/* ensure new pointer is followed by 0 pointer */
	vec[veclen] = (char *) 0;

	return vec;
}
