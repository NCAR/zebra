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

#ifndef _zeb_zl_regex_h_
#define _zeb_zl_regex_h_

/*
 * Takes care of ensuring that the correct files are included for regex
 * support, and declares our hook routines.
 */
# include "defs.h"		/* for FP and const defines */
# include <sys/types.h>

#if defined(hpux) || defined(_POSIX_SOURCE) || defined (__osf__) || defined (AIXV3)
#   define ZL_RE_POSIX
#else
#if defined(SVR4)	/* Solaris */
#   define ZL_RE_SVR4
#else
    /* everything else is assumed BSD */
#   define ZL_RE_BSD
#endif /* SVR4 */
#endif /* hpux and POSIX */

#ifdef ZL_RE_SVR4
#   include <libgen.h>
#endif
#ifdef ZL_RE_POSIX
#   include <regex.h>
#endif
#ifdef ZL_RE_BSD
    /* nothing */
#endif


/*
 * Prototypes for 4.2 BSD style regex hook routines.
 */
extern char *zl_re_comp FP ((const char *s));
extern int zl_re_exec FP ((const char *s));


/*
 * POSIX prototypes, looking to the future
 */
#ifdef notdef
extern int zl_regcomp FP ((regex_t *preg, const char *pattern, int cflags));
extern int zl_regexec FP ((const regex_t *preg, const char *string, 
			   size_t nmatch, regmatch_t pmatch[], int eflags));
extern size_t zl_regerror FP ((int errcode, const regex_t *preg, char *errbuf,
			       size_t errbuf_size));
extern void zl_regfree FP ((regex_t *preg));
#endif

#endif /* !_zeb_zl_regex_h_ */
