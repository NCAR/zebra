/*
 * Global definitions
 * $Id: globals.h,v 1.5 2000-04-24 19:08:37 burghart Exp $
 */
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

# include <math.h>
# include <ui.h>		/* To supercede UI replacements in defs.h */
# include <defs.h>		/* For FP macro needed by message.h */
# include <message.h>

/*
 * Trig stuff and convenience functions
 */
#ifndef PI
# define PI	3.141592654
#endif
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)

# define COSD(x)	(cos (DEG_TO_RAD (x)))
# define SIND(x)	(sin (DEG_TO_RAD (x)))
# define TAND(x)	(tan (DEG_TO_RAD (x)))
# define ATAND(x)	(RAD_TO_DEG (atan (x)))

# ifndef MIN	/* they're defined for us in Linux sys/param.h */
#   define MIN(x,y)	((x) < (y) ? (x) : (y))
#   define MAX(x,y)	((x) > (y) ? (x) : (y))
# endif

/*
 * Maximum number of radars we can handle
 */
# define MAXRAD	10

/*
 * Dummy time value for generating the fastest possible scan
 */
# define TIME_ASAP	-999.0

/*
 * User-specified desired spatial resolution and minimum beam separation
 */
extern float	Hres, Vres;
extern float	Hsep_min, Vsep_min;

/*
 * Volume description
 */
extern float	Vol_bot, Vol_top;

/*
 * Volume scan time to use (= TIME_ASAP if it isn't fixed)
 */
extern float	Vol_time;

/*
 * Current option and option limits
 */
# define NO_OPT	-999
extern int Opt;
extern int MinOpt, MaxOpt;

/*
 * The slow volume scan time for each option
 */
extern float	Slowtime[];

/*
 * Step to use for generating alternate resolutions
 */
# define RES_STEP	0.02

/*
 * Prototypes
 */
void bnd_InitBoundary FP((void));

