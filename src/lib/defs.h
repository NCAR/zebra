/*
 * Useful definitions.
 */
/* $Id: defs.h,v 2.14 1993-03-18 18:51:38 granger Exp $ */
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
# ifndef _DEFS_H_
# define _DEFS_H_

# include <ui.h>
# include <stdlib.h>
# if (! defined(__GNUC__) || __GNUC__ < 2)
# 	include <memory.h>   /* Conflicts with gcc2 builtin */
# endif

/*
 * FCC-specific defined types.
 */
typedef struct date_st time;	/* Different from UI "date" so we can 
				   change it. */
enum pmode { NoMode, History, RealTime };

/*
 * Here is the new zeb time format.  This looks suspiciously like the
 * BSD timeval structure....
 */
typedef struct _ZebTime
{
	long	zt_Sec;		/* Seconds since 1/1/70		*/
	long	zt_MicroSec;	/* Microseconds added to zt_Sec */
} ZebTime;

/*
 * Time conversion options.
 */
typedef enum 
{
	TC_DateOnly,		/* Only the date part		*/
	TC_TimeOnly,		/* Only the time part		*/
	TC_Full,		/* The whole works		*/
	TC_FullUSec,		/* Full with microseconds	*/
} TimePrintFormat;

/*
 * Locations.
 */
typedef struct s_Location
{
	float	l_lat;
	float	l_lon;
	float	l_alt;
} Location;

/*
 * A macro to make function prototypes a little easier across both STDC and
 * non-STDC implementations.
 */
# ifdef __STDC__
#  define FP(stuff) stuff
# else
#  define FP(stuff) ()
# endif

/*
 * Functions.
 */
void 	tw_DefTimeWidget FP ((int (*callback) (), char *title));
void	tw_SetTime FP ((ZebTime *init_or_null)); /* null <==> use system time */
void 	tw_DialAdjust FP ((int, int));
int 	InterpDTime FP ((char *));
void 	TC_SysToFcc FP ((long, time *));
long 	TC_FccToSys FP ((time *));
void	RL_Encode FP ((unsigned char *, unsigned char *, int, int, 
		int *, int *));
void 	RL_Decode FP ((unsigned char *, unsigned char *const, int));
int	CommaParse FP ((char *, char **));

void	SetupConfigVariables FP ((void));
char 	*GetBaseDir FP ((void));
char 	*GetBinDir FP ((void));
char 	*GetLibDir FP ((void));
char 	*GetProjDir FP ((void));

 /* New time format utilities */

long	TC_ZtToSys FP ((const ZebTime *));
void	TC_SysToZt FP ((long, ZebTime *));
void	TC_UIToZt FP ((const date *, ZebTime *));
void	TC_ZtToUI FP ((const ZebTime *, date *));
void	TC_EncodeTime FP ((const ZebTime *, TimePrintFormat, char *));
void	TC_ZtSplit FP ((const ZebTime *, int *, int *, int *, int *, int *,
		int *, int *));
void	TC_ZtAssemble FP ((ZebTime *, int, int, int, int, int, int, int));

/*
 * Some macros for the new time format.
 */
# define TC_Less(t1,t2) 				\
	(((t1).zt_Sec == (t2).zt_Sec) ? 		\
		((t1).zt_MicroSec < (t2).zt_MicroSec) :	\
		((t1).zt_Sec < (t2).zt_Sec))
# define TC_LessEq(t1,t2) 					\
	(((t1).zt_Sec == (t2).zt_Sec) ? 			\
		((t1).zt_MicroSec <= (t2).zt_MicroSec) :	\
		((t1).zt_Sec <= (t2).zt_Sec))

# define TC_Eq(t1,t2) (((t1).zt_Sec == (t2).zt_Sec) && \
			((t1).zt_MicroSec == (t2).zt_MicroSec))

/*
 * Macros
 */
# define ALLOC(type) ((type *) malloc (sizeof (type)))
# define ODD(v) ((v) & 0x1)
# define EVEN(v) (((v) & 0x1) == 0)

# define DLT(d1,d2) ((d1).ds_yymmdd < (d2).ds_yymmdd || \
	((d1).ds_yymmdd == (d2).ds_yymmdd && (d1).ds_hhmmss < (d2).ds_hhmmss))

# define DLE(d1,d2) ((d1).ds_yymmdd < (d2).ds_yymmdd || \
	((d1).ds_yymmdd == (d2).ds_yymmdd && (d1).ds_hhmmss <= (d2).ds_hhmmss))
/*
 * Set up inline and const so that we can use it.
 */
# ifndef __GNUC__
# define inline
# define const
# endif

/*
 * Saber-C generates warnings on RCSID's, so we leave them out.
 */
# ifdef SABER
#	define MAKE_RCSID(id)
# else
#	define MAKE_RCSID(id) \
	static char *rcsid = id;
# endif

# endif /* _DEFS_H_ */
