/*
 * Header file for private variables shared between the interface modules
 * Appl.c and DFA_Appl.c
 */

/* $Id: Appl.h,v 3.4 1999-03-01 02:03:20 burghart Exp $ */

/*		Copyright (C) 1987-1995 by UCAR
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

# ifndef _zebra_appl_h_
# define _zebra_appl_h_

# define MAX_DF_CACHE 30

/*
 * How far in the future we are willing to accept data.
 */
# define MaxFuture _ds_MaxFuture
extern int MaxFuture;

/*
 * The flag for standalone mode.
 */
# define Standalone _ds_Standalone
extern int Standalone;

/*
 * The API methods structure.  The standalone module uses this to supply
 * different (i.e., local) implementations of datastore interface methods.
 */
typedef struct _DS_Methods
{
	const DataFile*	(*dsm_FindBefore)(PlatformId pid, 
					  const ZebraTime *when, int srcid);
	const DataFile*	(*dsm_FindAfter)(PlatformId pid, 
					 const ZebraTime *when, int srcid);
	const DataFile*	(*dsm_FindDFLink)(const DataFile *df, int prev);
	void	(*dsm_DeleteObs)(PlatformId pid, const ZebraTime *when);
	void	(*dsm_DeleteData)(PlatformId pid, const ZebraTime *when);
	PlatClassId (*dsm_DefineClass)(const PlatformClass *pc);
	PlatformId (*dsm_DefinePlatform)(PlatClassId cid, const char *name, 
					 PlatformId parent);
	int	(*dsm_NewDataFile)(PlatformId pid, const char *filename, 
				   const ZebraTime *zt, DataFile *df);
	void	(*dsm_NotifyFile)(const Platform *p, const DataFile *df, 
				  int now, int nnew, int sample, int last);
	int 	(*dsm_NPlat)(void);
	void	(*dsm_SearchPlatforms)(PlatformSearch *search, 
				       PlatformList *pl);
	int	(*dsm_GetSrcInfo)(int srcid, SourceInfo *si);
	int	(*dsm_GetPlatDir)(int srcid, PlatformId pid, char *dir);
} DS_Methods;

extern DS_Methods DSM;

/*
 * Functions shared internally by the application interface modules,
 * but not for public consumption.
 */
void	ds_InitAPI FP ((void));
void	ds_SendToDaemon FP ((void *, int));


# endif /* ! _zebra_appl_h_ */

