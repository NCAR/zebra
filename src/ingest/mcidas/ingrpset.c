/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */
  
/**** McIDAS-AIX Revision History *** */
/* 1 INGRPSET.C 7-Sep-93,10:47:50,`RUSSD' Initial release to AIX (4069)      */
/* 2 INGRPSET.C 29-Sep-93,10:51:38,`RUSSD' Fixed SGI define                  */
/* 3 INGRPSET.C 5-Oct-93,8:11:28,`USER' Released for McIDAS-X Only           */
/* 4 INGRPSET.C 1-Mar-94,10:04:40,`RUSSD' Standardize function names         */
/* 5 INGRPSET.C 31-Mar-94,22:17:26,`BARRYR' Add proprietary statement        */
/* 6 INGRPSET.C 31-Mar-94,23:46:30,`BARRYR' Add proprietary statement        */
/* 7 INGRPSET.C 2-May-94,16:01:12,`USER' Released for McIDAS-X Only          */
/* 8 INGRPSET.C 3-Aug-94,18:36:26,`DWS' Modified for Solaris (4731)          */
/* 9 INGRPSET.C 22-Aug-94,6:46:22,`USER' Released for McIDAS-X Only          */
/* 10 INGRPSET.C 19-Feb-96,15:52:38,`DWS' reglue: modified file              */
/* 11 INGRPSET.C 20-Feb-96,12:35:06,`USER' Released for McIDAS-X Only        */
/**** McIDAS-AIX Revision History *** */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <sys/types.h>
#include <stdlib.h>
#include <unistd.h>

#include "mcidasp.h"

#ifndef MAIN
#define MAIN 0
#endif

#if MAIN
#include <stdio.h>
#endif

/*
 * configuration of gidset_t
 * 
 * Define GETGROUPS_USES_GID_T as true if getgroups() takes a gid_t* as
 * its second argument.  While POSIX.1 says it does, on many systems,
 * gid_t is a short while getgroups() takes an int*.
 */

/*
 * platform specific settings
 */

/* AIX defaults */

#if _AIX
# ifndef	GETGROUPS_USES_GID_T
# define	GETGROUPS_USES_GID_T	1
# endif
#endif

/* Irix defaults */

#if __sgi
# ifndef	GETGROUPS_USES_GID_T
# define	GETGROUPS_USES_GID_T	1
# endif
#endif

/* HP-UX defaults */

#if __hpux
# ifndef	GETGROUPS_USES_GID_T
# define	GETGROUPS_USES_GID_T	1
# endif
#endif

/*
 * SunOS uses int
 * Solaris uses gid_t
 *
 * There's no good system-provided way to distinguish
 * between the two systems, so we use a heuristic.
 * The file <sys/types.h> uses different 'guard' symbols
 * on the different system.  On SunOS4, it uses '__sys_types_h',
 * while on SunOS5 (Solaris) it uses '_SYS_TYPES_H'.
 *
 * The code is careful to report the lack of a proper
 * configuration for GETGROUPS_USES_GID_T...
 */

#if sparc
# ifndef	GETGROUPS_USES_GID_T

#  ifdef __sys_types_h			/* SunOS 4 */
#   define	GETGROUPS_USES_GID_T	0
#  endif /* __sys_types_h */

#  ifdef _SYS_TYPES_H			/* SunOS 5 */
#   define	GETGROUPS_USES_GID_T	1
#  endif /* _SYS_TYPES_H */
	
#  ifndef	GETGROUPS_USES_GID_T	/* SunOS ?? */
#   error "Lack a definition for GETGROUPS_USES_GID_T!"
#  endif

# endif
#endif

/* OSF1 defaults */

#if __osf__
# ifndef	GETGROUPS_USES_GID_T
# define	GETGROUPS_USES_GID_T	1
# endif
#endif

/*
 * default defaults -- don't change this section
 */

#ifndef	GETGROUPS_USES_GID_T
#define	GETGROUPS_USES_GID_T	0
#endif

/*
 * Now at last we can declare gidset_t appropriately.
 */

#if	GETGROUPS_USES_GID_T
typedef gid_t		gidset_t;
#else
typedef int		gidset_t;
#endif

/*
 * ingroupset(gid) - returns true if the given gid is in the group
 * set of this process, false otherwise.  The "group set" may or
 * may not include the getgid() or getegid() groups of the process.
 */

int
ingroupset(int gid)
{
	static int first = 1;

	static gidset_t *groups = (gidset_t *)0;
	static int ngroups;

	if(first)
	{
		first = 0;

		ngroups = getgroups(0, (gidset_t *)0);
		/* no add'l groups if answer is -1 or 0 */
		if(ngroups < 1)
		{
			return 0;
		}

		groups = (gidset_t *)malloc(ngroups * sizeof(*groups));
		if(!groups)
		{
			return 0;
		}

		ngroups = getgroups(ngroups, groups);
	}

	if(ngroups < 1 || !groups)
	{
		return 0;
	}

#if MAIN
	{
		int i;

		for(i = 0; i < ngroups; i++)
		{
			printf("%d: gid %d\n", i, groups[i]);
		}
	}
#endif

	{
		int i;

		for(i = 0; i < ngroups; i++)
		{
			if(gid == groups[i])
			{
				return 1;
			}
		}
	}

	return 0;
}

#if MAIN
main(ac,av)
char **av;
{
	printf("GETGROUPS_USES_GID_T = %d\n", GETGROUPS_USES_GID_T);

	for(ac--,av++; ac; ac--,av++)
	{
		int gid;
		int ans;

		gid = atoi(*av);

		ans = ingroupset(gid);
		printf("ingroupset(%d) -> %d\n", gid, ans);
	}

	return 0;
}
#endif
