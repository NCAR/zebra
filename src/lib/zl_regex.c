/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include <defs.h>
# include <config.h>

/*
 * This header file defines exactly one of the following:
 *	ZL_RE_SVR4
 *	ZL_RE_POSIX
 *	ZL_RE_BSD
 * If adding support for a new OS, see zl_regex.h and define one
 * of these symbols for that OS, else create a different symbol
 */
# include "zl_regex.h"

static char *rcsid = 
   "$Id: zl_regex.c,v 2.1 1994-01-05 20:14:32 granger Exp $";

/*
 * Provide hooks to the system regular expression routines, confining all of
 * that ifdef nonsense to this file.  Zeb programs should use the zl_
 * (Zeb library) versions of the SunOS functions re_comp and re_exec, since
 * these are the lowest common denominator of functionality.
 */


/*
 * Declare any static structures that we'll need to hold compiled expressions
 */
#ifdef ZL_RE_POSIX
static regex_t Reg;
static regex_t Reg2;
static int RegValid = 0;
#endif
#ifdef ZL_RE_SVR4
static char *Buffer = NULL;
#endif

/*
 * Any needed prototypes
 */
#ifdef ZL_RE_BSD
char *re_comp ();
int re_exec ();
#endif



char *
zl_re_comp (re)
const char *re;
/*
 * Try to make a compatible call to a regular expression compiler routine.
 * Returns NULL on success, and an error message otherwise.  On an error,
 * the original compiled expression does not change.
 */
{
	static char *error = "regular expression would not compile";
	int istat;
	char *result;

#ifdef ZL_RE_SVR4
	/*
	 * Returns pointer to a malloc() compile buffer, NULL otherwise.
	 */
	result = regcmp((char *)re, (char*)NULL);
	/*
	 * If there is an error, don't change the current
	 * buffer, otherwise free the last one and save the new one.
	 */
	if (result == NULL)
		result = error;
	else
	{
		if (Buffer) free (Buffer);
		Buffer = result;
		result = NULL;
	}
#endif
#ifdef ZL_RE_POSIX
	/*
	 * Returns zero if successful, error code if not.  Only save the
	 * compiled regular expression if there was no error.
	 */
	istat = regcomp ( &Reg2, (char *)re, REG_EXTENDED );
	if (istat == 0)
	{
		result = NULL;
		Reg = Reg2;
		RegValid = 1;
	}
	else
		result = error;
#endif
#ifdef ZL_RE_BSD
	/* 
	 * Returns NULL if successful, error message otherwise
	 */
	result = re_comp ((char *)re);
#endif

	return (result);
}



int
zl_re_exec (s)
const char *s;
/*
 * Mimic BSD re_exec() function.  Return 
 * 	1	for a match
 *	0	for no match
 *	-1	for invalid regular expression
 */
{
	int result;
	char *ptr;

#ifdef ZL_RE_SVR4
	/*
	 * regex() returns NULL on failure
	 */
	if (Buffer)
		result = (regex (Buffer, (char *)s)) ? (1) : (0);
	else
		result = -1;
#endif
#ifdef ZL_RE_POSIX
	/*
	 * regexec() returns 0 on a match and non-zero otherwise
	 */
	if (RegValid)
	{
		if (regexec (&Reg, (char *)s, (size_t) 0, NULL, 0) == 0)
			result = 1;
		else
			result = 0;
	}
	else
		result = -1;
#endif
#ifdef ZL_RE_BSD
	result = re_exec ((char *)s);
#endif

	return (result);
}

