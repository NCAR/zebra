/*
 * Useful definitions.
 */
/* $Id: defs.h,v 2.2 1991-11-20 23:10:31 corbet Exp $ */
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
# include <memory.h>

/*
 * FCC-specific defined types.
 */
typedef struct date_st time;	/* Different from UI "date" so we can 
				   change it. */
enum pmode { NoMode, History, RealTime };

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
 * Functions.
 */
# ifdef __STDC__
	char *malloc (unsigned size);
	char *realloc (void *ptr, unsigned size);
	void tw_DefTimeWidget (int (*callback) (), char *title);
	void tw_DialAdjust (int, int);
	int InterpDTime (char *);
	void TC_SysToFcc (long, time *);
	long TC_FccToSys (time *);
	void	RL_Encode (unsigned char *, unsigned char *, int, int, 
			int *, int *);
	void 	RL_Decode (unsigned char *, unsigned char *const, int);
	int	CommaParse (char *, char **);
	void	SetupConfigVars (void);
# else
	char *malloc ();
	char *realloc ();
	void tw_DefTimeWidget ();
	void tw_DialAdjust ();
	int InterpDTime ();
	void TC_SysToFcc ();
	long TC_FccToSys ();
	void	RL_Encode ();
	void 	RL_Decode ();
	int	CommaParse ();
	void	SetupConfigVars ();
# endif


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
 * Set up inline so that we can use it.
 */
# ifndef __GNUC__
# define inline
# endif


# endif /* _DEFS_H_ */
