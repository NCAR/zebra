/*
 * Useful definitions.
 */
/* $Id: defs.h,v 2.44 2002-07-23 21:14:27 granger Exp $ */
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
# ifndef _zebra_defs_h_
# define _zebra_defs_h_

/*
 * This at least seems safe enough 
 */
# include <stdio.h>
# include <stdlib.h>

/*
 * Apparently if string.h gets included before memory.h on Solaris,
 * especially when including rpc.h, we get conflicting prototypes.
 */
# include <memory.h>
# include <string.h>

# ifdef __cplusplus
extern "C"
{
# endif

/*
 * A macro to make function prototypes a little easier across both STDC and
 * non-STDC implementations.
 */
# if __STDC__ || defined(__cplusplus)
#  define FP(stuff) stuff
# else
#  define FP(stuff) ()
#  define const
# endif

# if ! __GNUC__ && ! defined(__cplusplus)
#  define inline
# endif

# if __GNUC__
#  define GCC_UNUSED __attribute__ ((__unused__))
# else
#  define GCC_UNUSED
# endif

# include "version.h"
# include "zl_param.h"

/*
 * Zebra library types.
 */
typedef struct date_st UItime;	/* Different from UI "date" so we can 
				   change it. */

enum pmode { NoMode, History, RealTime };

#include "zebra.h"		/* Library types and XDR interfaces */

extern const Location LOC_NONE;

extern const ZebraTime ZT_NONE;
extern const ZebraTime ZT_ALPHA;
extern const ZebraTime ZT_OMEGA;
#define ZT_EPOCH ZT_ALPHA
#define ZT_END ZT_OMEGA

/*
 * OK, I give up.  egcs/g++ defines bool as a one-byte type, leading
 * to all kinds of really ugly confusion over the C/C++ interface.  Time
 * to dump bool for something more obscure.
 */
typedef int zbool;

/*
 * Time conversion options.
 */
typedef enum 
{
	TC_DateOnly,		/* Only the date part		*/
	TC_TimeOnly,		/* Only the time part		*/
	TC_Full,		/* The whole works		*/
	TC_FullUSec		/* Full with microseconds	*/
} TimePrintFormat;

/*
 * For lack of a better place, I define scan modes here.
 */
typedef enum
{
	SM_CAL = 0,
	SM_PPI = 1,
	SM_COP = 2,
	SM_RHI = 3,
	SM_VER = 4,
	SM_TAR = 5,
	SM_MAN = 6,
	SM_IDL = 7,
	SM_SUR = 8,
	SM_AIR = 9,
	SM_HOR = 10
} ScanMode;


/*
 * Functions.
 */
int 	InterpDTime FP ((char *));
void	RL_Encode FP ((unsigned char *, unsigned char *, int, int, 
		       int *, int *));
void 	RL_Decode FP ((unsigned char *, unsigned char *const, const int));
int	CommaParse FP ((char *, char **));
int	ParseLine FP ((char *string, char **substrings, int delim));

void	SetupConfigVariables FP ((void));
void    Require FP ((char *module));
char    *GetRequirePath FP ((void));
void    SetRequirePath FP ((char *path));
char 	*GetBaseDir FP ((void));
char 	*GetBinDir FP ((void));
char 	*GetLibDir FP ((void));
char 	*GetProjDir FP ((void));
char 	*GetDataDir FP ((void));

/* New time format utilities */

void 	TC_SysToFcc FP ((const long, UItime *));
long 	TC_FccToSys FP ((const UItime *));
long	TC_ZtToSys FP ((const ZebTime *));
void	TC_SysToZt FP ((const long, ZebTime *));
void	TC_UIToZt FP ((const date *, ZebTime *));
void	TC_ZtToUI FP ((const ZebTime *, date *));
void	TC_EncodeTime FP ((const ZebTime *, TimePrintFormat, char *));
const char *TC_AscTime FP ((const ZebTime *zt, TimePrintFormat format));
zbool	TC_DecodeTime FP ((const char *, ZebTime *));
void	TC_ZtSplit FP ((const ZebTime *, int *, int *, int *, int *, int *,
		int *, int *));
void	TC_ZtAssemble FP ((ZebTime *, int, int, int, int, int, int, int));
void	TC_y2k FP ((UItime *));

int     FindFile FP ((const char *, const char *, char *));

/*
 * Altitude units convenience utilities
 */
const char	*au_UnitsName FP ((AltUnitType atype));
const char	*au_LongUnitsName FP ((AltUnitType atype));
const char	*au_PrintFormat FP ((AltUnitType atype));
const char	*au_AltLabel FP ((double alt, AltUnitType atype));
const char	*au_LongAltLabel FP ((double alt, AltUnitType atype));
zbool		au_ConvertName FP ((char *name, AltUnitType *atype));

/* 
 * Alarm widget interface
 */
void aw_DefAlarmWidget FP ((void));

/*
 * Sounds interface
 */
void DoSound FP ((char *sound));

/*
 * Coordinate conversions
 */
void cvt_ToXY FP ((double lat, double lon, float *x, float *y));
void cvt_ToLatLon FP ((double x, double y, float *lat, float *lon));
int cvt_GetOrigin FP ((float *lat, float *lon));
zbool cvt_Origin FP ((double lat, double lon));
void cvt_ShowOrigin FP ((void));

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
 * Location macros
 */
# define loc_OutOfRange(loc) (((loc)->l_lat < -90 || (loc)->l_lat > 90 || \
			       (loc)->l_lon > 180 || (loc)->l_lon < -180))

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
 * Use more complete rcsid macro from version.h
 */
# define MAKE_RCSID(id) RCSID(id)


# ifdef __cplusplus
}
# endif

# endif /* !_zebra_defs_h_ */
