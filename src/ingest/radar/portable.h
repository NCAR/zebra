#ident "@(#)portable.h	1.17 91/05/22"

/*
**   ----------------------------------------------------------------- 
**          Copyright (C) 1986,1991  Sun Microsystems, Inc
**                      All rights reserved. 
**            Notice of copyright on this source code 
**            product does not indicate publication. 
**   
**                    RESTRICTED RIGHTS LEGEND: 
**   Use, duplication, or disclosure by the Government is subject 
**   to restrictions as set forth in subparagraph (c)(1)(ii) of 
**   the Rights in Technical Data and Computer Software clause at 
**   DFARS 52.227-7013 and in similar clauses in the FAR and NASA 
**   FAR Supplement. 
**   ----------------------------------------------------------------- 
*/

/*
** portable.h:
**	Used to map SunOS-isms to the platform du jour.
*/

#ifndef _portable_h_
#define _portable_h_

#ifdef SYSV
#  ifndef bcopy
#    define bcopy(s,d,l)	memmove(d,s,l)
#  endif
#  ifndef bzero
#    define bzero(p,l)		memset(p,(int)'\0',l)
#  endif
#  ifndef bcmp
#    define bcmp(p1,p2,l)	memcmp(p2,p1,l)
#  endif
#endif /* SYSV */

#if !defined(CONST)
#  if defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)
#    define CONST	const
#  else
#    define CONST	/* nothing */
#  endif
#endif

#if !defined(VOLATILE)
#  if defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)
#    define VOLATILE	volatile
#  else
#    define VOLATILE	/* nothing */
#  endif
#endif

#define Const CONST	/* For X11/ks_table.h */

#if defined(SYSV)
#  define SYMSTR(a) a
#elif defined(__STDC__)	/* ANSI-C builds on SunOS 4.1.1 */
#  define SYMSTR(a) "_" a
#else /* K&R builds on SunOS 4.1.1 */
#  define SYMSTR(a) (char *)strcat("_",a)
#endif /* SYSV */

#ifndef CPPCONCAT
#  ifdef __STDC__
#    define CPPCONCAT(token1, token2)	token1 ## token2
#  else
#    define CPPCONCAT(token1, token2)	token1/**/token2
#  endif
#endif

#ifdef DEBUG
#  define FAST	/* We don't like register variables when debugging */
#else
#  define FAST    register
#endif

/*
**	Machine specific word size definitions 
*/

#if defined(SUN4)

typedef long		Sgn32;
typedef short		Sgn16;
typedef char		Sgn8;
typedef unsigned long	Unsgn32;
typedef unsigned short	Unsgn16;
typedef unsigned char	Unsgn8;

#elif defined(SUN3)

::: error "SUN3: Unsupported configuration"

typedef long		Sgn32;
typedef short		Sgn16;
typedef char		Sgn8;
typedef unsigned long	Unsgn32;
typedef unsigned short	Unsgn16;
typedef unsigned char	Unsgn8;

#else
/* SUN 4 defaults for files that don't #define SUN4 (like places in Shapes) */
typedef long		Sgn32;
typedef short		Sgn16;
typedef char		Sgn8;
typedef unsigned long	Unsgn32;
typedef unsigned short	Unsgn16;
typedef unsigned char	Unsgn8;
#endif	/* SUN4 or SUN3 */

#endif	/*_portable_h_*/

