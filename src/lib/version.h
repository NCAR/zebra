/*
 * $Id: version.h,v 2.5 2002-02-12 23:36:22 granger Exp $
 *
 * Include various symbols, compilation, and version info into an object
 * file.  We try to take advantage of ANSI C pre-preprocessors as much as
 * possible, as well as allowing for the use what(1) or ident(1) to identify
 * version information.
 */

#ifndef _zebra_version_h__
#define _zebra_version_h__

#include <stdio.h>	/* need sprintf prototype */
#include "config.h"
#include "defs.h"

/*
 * Prototypes from version.c
 */
#ifdef __cplusplus
extern "C" {
const char *V_version (void);
const char *V_format (char *buf, const char *a, const char *b, 
		      const char *c, const char *d);
/* Full copyright text linked from version.c */
const char *V_copyright (void);
}
#else
const char *V_version FP ((void));
const char *V_format FP ((char *buf, const char *a, const char *b, 
			  const char *c, const char *d));
const char *V_copyright FP((void));
#endif

#if __STDC__ || defined(__cplusplus)
# define V_STDC 1
#endif


/* ----------------------------------------------------------------------
 * CPP symbols present at the time an object module is compiled.
 */
#if !defined(SABER) && !defined(lint) && !defined(LINT)

#if V_STDC

static const char cppsyms[] = "@(#)$Symbols: "
#ifdef __STDC__
# if __STDC__
" __STDC__!=0 "
# else
" __STDC__==0 "
# endif
#endif
#ifdef _POSIX_SOURCE
" _POSIX_SOURCE "
#endif
#ifdef __GNUC__
" __GNUC__ "
#endif
#ifdef __STRICT_ANSI__
" __STRICT_ANSI__ "
#endif
#ifdef X_NOT_STDC_ENV
" X_NOT_STDC_ENV "
#endif
#ifdef X_NOT_POSIX
" X_NOT_POSIX "
#endif
#ifdef SYSV
" SYSV "
#endif
#ifdef SVR4
" SVR4 "
#endif
#ifdef BSD
" BSD "
#endif
#ifdef SHM
" SHM "
#endif
#ifdef CFG_NC_DCATTS_OVERRIDE
" CFG_NC_DCATTS_OVERRIDE "
#endif
#ifdef CFG_NO_BADVALUES
" CFG_NO_BADVALUES "
#endif
#ifdef CFG_NC_NO_ALT_UNITS
" CFG_NC_NO_ALT_UNITS "
#endif
#ifdef ARM_PROJECT
" ARM_PROJECT "
#endif
#ifdef NEXUS_PROJECT
" NEXUS_PROJECT "
#endif
#ifdef BASEDIR
"BASEDIR:" BASEDIR " "
#endif
#ifdef BINDIR
"BINDIR:" BINDIR " "
#endif
#ifdef LIBDIR
"LIBDIR:" LIBDIR " "
#endif
#ifdef AutoBuild
" AutoBuild "
#endif
" $";
#else /* ! __STDC__ */
# ifndef __STDC__
static const char cppsyms[] = "@(#)$Symbols: __STDC__ not defined $";
# else
static const char cppsyms[] = "@(#)$Symbols: __STDC__ defined as 0 $";
# endif /* ndef __STDC */
#endif /* __STDC__ */

extern char V_buffer[];
#define Z_cppsymbols() V_format(V_buffer,cppsyms,0,0,0)

#endif /* !SABER && !lint */

/*-----------------------------------------------------------------------
 * Version management macros
 */

/* Note that any necessary semi-colons are already included and should
 * not be added when invoked in code.  Prepending "@(#)" allows version
 * info to be found by both ident(1) (RCS) and what(1) (SCCS).
 * The compile date and time are only included with RCSID for V_STDC.
 */

#if defined(lint) || defined(LINT) || defined(SABER)

#define RCSID(id) 

#else
#if V_STDC
#define RCSID(id) \
static const char* \
Z_rcsid() \
{ \
    static const char rcs_id[] = "@(#)" id ; \
    static const char compileid[] = \
	"@(#)" "$Compiled: " __FILE__ " on " __DATE__ " at " __TIME__ " $"; \
    static char buf[256]; \
    return (V_format (buf, rcs_id, compileid, 0, 0)); \
}

#else /* !V_STDC */
/*
 * These defs are not as complete as above. And the 'what' flags may be
 * lost on optimization or not left preceding the RCS string.
 */

#define RCSID(id) \
static char* \
Z_rcsid() { \
    static char i_sccsid[4] = { '@', '(', '#', ')' }; \
    static char rcs_id[] = id ; \
    static char buf[256]; \
    return (V_format (buf, rcs_id, 0, 0, 0)); \
}

#endif /* V_STDC */

#endif /* lint */

/*
 * The static function which forces the version library module to be linked, 
 * and allows applications to print version information.
 */
static inline const char*
Z_version()
{
	return (V_version ());
}


static inline const char *Z_copyright()
{ return (V_copyright()); }

#ifdef V_STDC
# undef V_STDC
#endif

#endif /* !_zebra_version_h__ */
