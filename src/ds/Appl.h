/*
 * Header file for private variables shared between the interface modules
 * Appl.c and DFA_Appl.c
 */

/* $Id: Appl.h,v 3.3 1996-11-19 08:25:04 granger Exp $ */

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
	int	(*dsm_FindBefore)(/* pid, when, src */);
	int	(*dsm_FindAfter)(/* pid, when */);
	void	(*dsm_DeleteObs)(/* pid, when */);
	void	(*dsm_DeleteData)(/* pid, when */);
	PlatClassId (*dsm_DefineClass)(/* PlatformClass *pc */);
	PlatformId (*dsm_DefinePlatform)(/* cid, name, parent */);
	int	(*dsm_NewDataFile)(/* pid, filename, zt */);
	void	(*dsm_NotifyFile)(/* cp, dfi, dc, now, nnew, sample, last */);
	int 	(*dsm_NPlat)(/* void */);
	void	(*dsm_SearchPlatforms)(/* search, pl */);

} DS_Methods;

extern DS_Methods DSM;

/*
 * Platform search lists
 */
typedef struct _PlatformList {
	PlatformId *pl_pids;
	int pl_npids;
} PlatformList;

/*
 * Functions shared internally by the application interface modules,
 * but not for public consumption.
 */
void	ds_InitAPI FP ((void));
void	ds_SendToDaemon FP ((void *, int));
int	ds_AwaitPID FP ((Message *, PlatformId *));
void	ds_ZapCache FP ((DataFile *));
void	ds_WriteLock FP ((PlatformId));
void	ds_FreeWriteLock FP ((PlatformId));
void	ds_FreeCache FP((void));
void	ds_CreateFileCache FP ((int size));
DataFile *ds_SearchCache FP ((int dfi));
int	ds_DataChain FP ((ClientPlatform *p, int which));
void	ds_CachePlatform FP ((PlatformId pid, ClientPlatform *plat));
void	ds_CacheClass FP ((PlatClassId cid, PlatformClass *pc));
DataFile *ds_CacheFile FP ((DataFile *dfe));
void	ds_CacheName FP ((const char *name, PlatformId pid));
void	ds_CacheClassName FP ((const char *name, PlatClassId cid));
void	*ds_PlatTable FP ((void));
ClientPlatform *ds_GetPlatStruct FP ((PlatformId pid, ClientPlatform *,
				      int refresh));

# endif /* ! _zebra_appl_h_ */

