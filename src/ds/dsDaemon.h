/*
 * Data store daemon-specific definitions.
 */
/* $Id: dsDaemon.h,v 3.24 1996-11-19 09:36:01 granger Exp $ */
/*
 * The platform and data tables, via pointer.
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


#ifndef _zeb_dsDaemon_h_
#define _zeb_dsDaemon_h_

#include <time.h>

#include <defs.h>
#include <config.h>

/*
 * The table pointers and their counts are declared in d_DataTables.c.
 * The rest are in Daemon.c.
 */
extern PlatformClass 	*CTable;
extern PlatformInstance *PTable;
extern DataFile 	*DFTable;

extern int NDTEUsed;		/* How many data table entries used. 	*/
extern int DTFreeList;		/* The datafile free list		*/
extern int NPlatform;		/* How many platforms			*/
extern int NClass;		/* Number of platform classes		*/

/*
 * Allow the revision numbering to be chosen from the config file.
 */
extern bool StatRevisions;

/*
 * A global debug flag and some statistics variables
 */
extern bool Debug;
extern bool ParseOnly;

extern int InvalidatesSent;	/* Number of CacheInvalidate broadcasts */
extern int ReadLockRequests;
extern int WriteLockRequests;

/*
 * Cache options.
 */
extern bool LDirConst;		/* Nothing changes		*/
extern bool RDirConst;
extern bool LFileConst;		/* Files don't change (but they	*/
extern bool RFileConst;		/* can come and go)		*/
extern bool CacheOnExit;	/* Write cache on way out?	*/

/*
 * This variable is TRUE only during the initial file scan.
 */
extern bool InitialScan;	/* True implies first data scan	*/

/*
 * This variable reflects the number of platforms scanned so far
 * in the initial scan.  It should be ignored once InitialScan is false.
 */
extern int PlatformsScanned;

/*
 * Flag allowing the creation of data directories to be delayed until needed.
 */
extern bool DelayDataDirs;

/*
 * Some useful timing information 
 */
extern time_t LastScan;		/* Time of latest full scan	*/
extern time_t LastCache;	/* Time to which cache files are up-to-date */
extern time_t Genesis;		/* Time when daemon started	*/

/*
 * Metadata space
 */
extern int PTableSize, PTableGrow, DFTableSize, DFTableGrow;

extern int CTableSize;		/* Class table initial size	*/
extern int CTableGrow;		/* Amount to grow by		*/

/*
 * Internal functions.
 */
void InitSharedMemory FP ((void));

/*
 * Data table routines.
 */

/* --- class and platform tables --- */
int dt_CheckId FP ((PlatformId id));
int dt_CheckClassId FP ((PlatClassId id));
PlatformClass *dt_NewClass FP((const char *name, const char *superclass));
PlatformInstance *dt_Instantiate FP((PlatformClass *pc, 
				     PlatformId parent, 
				     const char *name));
PlatformInstance *dt_DefSubPlat FP((PlatformId parent, PlatformClass *spc,
				    const char *name));
PlatformInstance *dt_FindInstance FP((const char *name));
Platform *dt_FindPlatform FP ((const char *));
PlatformClass *dt_FindClass FP((const char *name));
void dt_ClientPlatform FP((PlatformInstance *pi, ClientPlatform *p));
void dt_SearchPlatforms FP((int (*function)(), struct dsp_PlatformSearch *req,
			    PlatformId *pids, int *npids));
const char *ds_ClassName FP ((PlatClassId id));

/* --- data file entries --- */
DataFile *dt_NewFile FP ((void));
void dt_RemoveDFE FP ((Platform *, int));
void dt_CutDFE FP ((PlatformInstance *p, int dfi));
void dt_FreeDFE FP ((DataFile *));
void dt_AddToPlatform FP ((Platform *, DataFile *, int local));
void dt_SortDFE FP ((PlatformInstance *p, DataFile *df, int local));
char *dt_DFEFilePath FP((Platform *pi, DataFile *df));

/*
 * Daemon public routines
 */
void ClearLocks FP ((Platform *));
void CacheInvalidate FP ((int));
void DataFileGone FP ((DataFile *df));

/*
 * Datascan
 */
void	DataScan FP ((void));
void	Rescan FP ((PlatformId platid, int all));
void	WriteCache FP ((const char *ufile, int onlydirty));
void	ReadCacheFile FP ((char *, int));
void	RescanPlat FP ((Platform *));
long	StatRevision FP ((Platform *, DataFile *, ino_t *));

# ifdef UI_H_SYMBOLS
/*
 * UI command parsing external prototypes; only called in Daemon.c
 */
void dc_DefPlatform FP((char *name, char *superclass));
void dc_DefPlatformClass FP((char *name, char *superclass, int platform));
void dc_SubPlatform FP((struct ui_command *cmds));
void dc_DefSubPlats FP((char *target, char *classname, 
			struct ui_command *cmds));
void dc_DefInstances FP((char *classname, struct ui_command *cmds));
# endif /* UI_H_SYMBOLS */

/*
 * Debuggin' routines
 */
void	dbg_DumpClass FP ((PlatformClass *pc));
void	dbg_DumpInstance FP ((PlatformInstance *pi));
void	dbg_DumpStatus FP ((void));
void	dbg_DumpTables FP((void));
int	dbg_AnswerQuery FP((char *who));
void	dbg_DumpLocks FP((char *buf, int len));
void	dbg_DumpLockQueue FP((Platform *p, Lock *lock, char *que,
			      char *buf, int len));
int	dbg_Append FP((char *buf, char *str, int len));
int	dbg_DirtyCount FP((void));
int	dbg_CompositeCount FP((void));
int	dbg_SubplatCount FP((void));
void	dbg_EncodeElapsed FP((char *prefix, time_t *start, time_t *end,
			      char *dest));

/*
 * Now that lots of platform info is split between class and instance,
 * some macros would be very useful for accessing common info.  Use static
 * inline functions, in hopes of C compilers which support inline and
 * getting away from clumsy cpp definitions. (static in case the compiler
 * doesn't support inline).
 */
/*
 * If inline has been defined because it is not supported by the compiler,
 * then use static.
 */
#define INLINE static inline

INLINE PlatformClass *pi_Class (pi)
PlatformInstance *pi;
{ return (CTable + pi->dp_class); }

INLINE PlatformClass *pc_SuperClass (pc)
PlatformClass *pc;
{ return ((pc->dpc_superclass == BadClass) ?
	  NULL : (CTable + pc->dpc_superclass)); }

INLINE PlatformInstance *pi_Parent (pi)
PlatformInstance *pi;
{ return ((pi->dp_parent == BadPlatform) ?
	  NULL : (PTable + pi->dp_parent)); }

/*
 * A bunch of boolean tests for class flags, which happen to be
 * copied into the instances also and thus can ge tested there.
 * This may change.
 */
INLINE int pi_Subplatform (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_SUBPLATFORM); }

INLINE int pi_Mobile (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_MOBILE); }

INLINE int pi_Composite (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_COMPOSITE); }

INLINE int pi_Regular (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_REGULAR); }

INLINE int pi_Remote (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_REMOTE); }

INLINE int pi_Daysplit (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_SPLIT); }

INLINE int pi_Model (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_MODEL); }

INLINE int pi_Virtual (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_VIRTUAL); }

/*
 * Actual instance-specific flags
 */
INLINE int pi_Dirty (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_DIRTY); }

INLINE int pi_CacheLoaded (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_CLOADED); }

INLINE int pi_RCacheLoaded (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_RCLOADED); }

INLINE int pi_DirExists (pi)
PlatformInstance *pi;
{ return (pi->dp_flags & DPF_DIREXISTS); }

/*
 * Access for other platform instance members
 */
INLINE char *pi_Name (pi)
PlatformInstance *pi;
{ return (pi->dp_name); }

INLINE char *pi_Dir (pi)
PlatformInstance *pi;
{ return (pi->dp_dir); }

INLINE char *pi_RDir (pi)
PlatformInstance *pi;
{ return (pi->dp_rdir); }

/*
 * Access to data file lists through instance structure
 */
INLINE int pi_LocalData (pi)
PlatformInstance *pi;
{ return (pi_Subplatform(pi) ? pi_Parent(pi)->dp_LocalData :
	  pi->dp_LocalData); }

INLINE int pi_RemoteData (pi)
PlatformInstance *pi;
{ return (pi_Subplatform(pi) ? pi_Parent(pi)->dp_RemoteData :
	  pi->dp_RemoteData); }

#define LOCALDATA(p) (((p).dp_flags & DPF_SUBPLATFORM) ? \
	PTable[(p).dp_parent].dp_LocalData : (p).dp_LocalData)
#define REMOTEDATA(p) (((p).dp_flags & DPF_SUBPLATFORM) ? \
	PTable[(p).dp_parent].dp_RemoteData : (p).dp_RemoteData)

/*
 * Access to class members through the instance structure
 */
INLINE DataOrganization pi_DataOrg (pi)
PlatformInstance *pi;
{ return (pi_Class(pi)->dpc_org); }

INLINE FileType pi_FileType (pi)
PlatformInstance *pi;
{ return (pi_Class(pi)->dpc_ftype); }

INLINE unsigned short pi_Keep (pi)
PlatformInstance *pi;
{ return (pi_Class(pi)->dpc_keep); }

INLINE unsigned short pi_MaxSamp (pi)
PlatformInstance *pi;
{ return (pi_Class(pi)->dpc_maxsamp); }

#endif /* ! _zeb_dsDaemon_h_ */
