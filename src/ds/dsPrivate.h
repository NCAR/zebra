/*
 * $Id: dsPrivate.h,v 3.37 2002-01-19 06:56:45 granger Exp $
 *
 * Data store information meant for DS (daemon and access) eyes only.
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

# ifndef _dsPrivate_h_
# define _dsPrivate_h_

# include <config.h>		/* CFG_ symbol definitions */
# include <message.h>		/* MSG_MAXNAMELEN */

# include "Platforms.h"		/* Platform and PlatformClass interface */
# include "DataFiles.h"		/* DataFile typedef and interface */

# if __cplusplus
extern "C" {
# endif

/*
 * The current data store protocol version.  CHANGE this when incompatible
 * protocol changes have been made.
 */
# define DSProtocolVersion	0x20020118

/*
 * Write codes -- used to note the placement of data by client and daemon.
 */
typedef enum
{
	wc_NewFile,		/* Create a new file		*/
	wc_Overwrite,		/* Overwrite existing datum	*/
	wc_Append,		/* Append to existing file	*/
	wc_Insert,		/* Insert before some data	*/
	wc_SkipIt		/* Do not write this sample	*/
} WriteCode;


/*----------------------------------------------------------------------
 * Below defines the messaging protocol used to communicate with the
 * data store daemon on the local node.
 *----------------------------------------------------------------------
 * The template request.
 */
struct dsp_Template
{
	int dsp_type;		/* Request type		*/
};

/*
 * All of the possible requests and replies.
 */
enum dsp_Types
{
	dpt_NewFileRequest,		/* Create new file		*/
	 dpt_R_NewFileSuccess,		/* Successful new file		*/
	 dpt_R_NewFileFailure,		/* Failed new file		*/
	dpt_AbortNewFile_UNUSED,	/* Abort new file creation	*/
	dpt_UpdateFile,			/* Notify of file update	*/
	dpt_NotifyRequest,		/* 5: Ask for data available notify*/
	 dpt_Notify,			/* Data available		*/
	dpt_CancelNotify,		/* Cancel notifications		*/
	 dpt_CancelAck,			/* Acknowledge cancel (unused)	*/
	dpt_DeleteData,			/* DANGER remove data		*/
	dpt_DataGone,			/* 10: Data deletion announcement*/
	dpt_CopyNotifyReq,		/* Get copies of notification rq*/
	dpt_MarkArchived,		/* Mark a file as archived	*/
	dpt_R_UpdateAck,		/* Acknowledge a file update	*/
	dpt_Rescan,			/* Force rescan of platform	*/
	dpt_BCDataGone,			/* 15: Data zapped		*/
	/*
	 * Start additions for non-SHM data store.
	 */
	dpt_GetNPlat,			/* Return the number of platforms */
	 dpt_R_NPlat,			/* the number of platforms	*/
	dpt_GetPlatStruct,		/* Get a platform structure	*/
	 dpt_R_PlatStruct,		/* Returned plat struct	*/
	dpt_GetFileStruct_UNUSED,	/* 20: Get a DFE		*/
	 dpt_R_FileStruct_UNUSED,
	dpt_PLock_UNUSED,		/* Lock a platform		*/
	 dpt_R_PLockGranted_UNUSED,	/* Acknowledge the lock		*/
	dpt_ReleasePLock_UNUSED,	/* Release platform lock	*/
	dpt_LookupPlatform_UNUSED,	/* 25: Plat name -> pid translation*/
	dpt_R_PID,			/* Return a PID result		*/
	dpt_FindDF,			/* Find DFE based on time	*/
	 dpt_R_DataFile,		/* Found DF			*/
	dpt_CacheInvalidate_UNUSED,	/* Invalidate a cache entry	*/
	dpt_Hello,			/* 30: New client greeting	*/
	 dpt_R_ProtoVersion,		/* Protocol version		*/
	dpt_FindAfter,			/* Find closest DFE after Time	*/
	dpt_WriteLock_UNUSED,		/* Write lock a platform	*/
	dpt_ReleaseWLock_UNUSED,	/* Release write lock		*/
	dpt_DeleteObs,			/* 35: DANGER remove an observation*/
	dpt_PlatformSearch,		/* Match regex to platform names*/
	 dpt_R_PlatformSearch,		/* Reply with list of plat ids	*/
	 dpt_R_PlatStructSearch_UNUSED,	/* For sending back structs 	*/
	dpt_LookupClass_UNUSED,		/* Class name -> ID		*/
	dpt_R_CID,			/* 40: Return a class ID result	*/
	dpt_GetClassStruct,		/* Request class struct by id	*/
	dpt_R_ClassStruct,		/* Return class struct		*/
	dpt_DefineClass,		/* Define a class to the daemon */
	dpt_AddSubplat,			/* Add a subplat to a class	*/
	dpt_Instantiate,		/* 45: Instantiate a platform   */
	dpt_ReadLock_UNUSED,		/* Read lock a platform file	*/
	 dpt_R_ReadLock_UNUSED,		/* Read lock response		*/
	dpt_ReleaseRLock_UNUSED,	/* Release a file read lock	*/
	dpt_FindDFNext,			/* Find the next file..		*/
	dpt_FindDFPrev,			/* 50: ..or the previous file	*/
	dpt_GetSrcInfo,			/* Request source info		*/
	 dpt_R_SrcInfo,			/* Return source info		*/
	dpt_GetPlatDir,			/* Request platform dir	*/
	 dpt_R_PlatDir,			/* Return platform dir		*/
};

# define DSP_FLEN	CFG_FILEPATH_LEN /* File name length		*/

#ifdef notdef
/*
 * Use the protocol version, P_NAMELEN, and FNAMELEN, the three parameters
 * affecting compatibility of cache files, to create a pseudo-unique
 * cache file key.
 */
# define CacheKey ((((unsigned long)DSProtocolVersion) << 8) + \
	 (unsigned long)(P_NAMELEN + FNAMELEN))
#endif

/*
 * Create a new data file.
 */
struct dsp_CreateFile
{
	enum dsp_Types dsp_type;	/* == dpt_NewFileRequest	*/
	PlatformId dsp_plat;		/* Platform ID			*/
	int	dsp_srcid;		/* Source ID			*/
	ZebTime	dsp_time;		/* Initial file time		*/
	char	dsp_FileName[DSP_FLEN];	/* The name of the file		*/
};

struct dsp_R_CreateFile
{
	enum dsp_Types dsp_type;	/* == dpt_R_NewFileSomething	*/
	DataFile dsp_file;		/* new DF			*/
};


/*
 * File update notification.
 */
struct dsp_UpdateFile
{
	enum dsp_Types dsp_type;	/* == dpt_UpdateFile		*/
	DataFile dsp_file;		/* updated file			*/
	int	dsp_NSamples;		/* How many samples added	*/
	int	dsp_NOverwrite;		/* How many overwritten		*/
	int	dsp_Last;		/* Last update			*/
};

struct dsp_UpdateAck
{
	enum dsp_Types dsp_type;	/* == dpt_R_UpdateAck		*/
	DataFile dsp_file;		/* updated file			*/
};


/*
 * Delete data from the disk.  Use with caution.
 */
struct dsp_DeleteData
{
	enum dsp_Types dsp_type;	/* == dpt_DeleteData/dpt_DeleteObs */
	PlatformId dsp_plat;		/* Target platform		   */
	ZebTime dsp_when;		/* Zorch obs before/at this time   */
};


/*
 * Deleted data notification sent from Daemon.
 */
struct dsp_DataGone
{
	enum dsp_Types dsp_type;	/* == dpt_DataGone		*/
	DataFile dsp_file;		/* say bye-bye to this file	*/
};



/*
 * The application notification mechanism.
 */
struct dsp_NotifyRequest
{
	enum dsp_Types dsp_type;	/* == dpt_NotifyRequest		*/
	PlatformId dsp_pid;		/* Platform of interest		*/
	int dsp_param;			/* Requestor parameter		*/
	char dsp_who[MSG_MAXNAMELEN];	/* client name for copies	*/
};


struct dsp_NotifyCancel
{
	enum dsp_Types dsp_type;	/* == dpt_CancelNotify		*/
	char dsp_who[MSG_MAXNAMELEN];	/* client name for copies	*/
};


struct dsp_Notify
{
	enum dsp_Types dsp_type;	/* == dpt_Notify		*/
	PlatformId dsp_pid;		/* Platform of interest		*/
	int dsp_param;			/* Requestor parameter		*/
	int dsp_nsample;		/* Number of new samples	*/
	ZebTime dsp_when;		/* The lastest time for data	*/
	UpdCode dsp_ucode;		/* The update code		*/
};


/*
 * A hook to allow an archiver process to mark files as having been 
 * archived.  Such a mark will prevent further writing to the file.
 */
struct dsp_MarkArchived
{
	enum dsp_Types 	dsp_type;	/* == dpt_MarkArchived		*/
	DataFile	dsp_file;	/* the archived file		*/
};



/*
 * Broadcast network notification.  For the moment, I have been lazy and
 * only implemented DataGone.  The assumption here is that network access
 * is of interest for older data, so this is the stuff that will be 
 * relevant to remote machines.
 */
struct dsp_BCDataGone
{
	enum dsp_Types dsp_type;	/* == dpt_BCDataGone		*/
	char dsp_Plat[60];		/* Platform name		*/
	char dsp_FileName[DSP_FLEN];	/* The file -- sans directory	*/
};


/*
 * Force rescan.
 */
struct dsp_Rescan
{
	enum dsp_Types dsp_type;	/* == dpt_Rescan		*/
	PlatformId dsp_pid;		/* Which platform...		*/
	int dsp_all;			/* ...or all of them.		*/
};


/*
 * Number of platforms.
 */
struct dsp_NPlat
{
	enum dsp_Types dsp_type;	/* == dpt_R_NPlat		*/
	int dsp_nplat;			/* the number			*/
};


/*
 * A request for a platform structure.
 */
struct dsp_GetPlatStruct
{
	enum dsp_Types dsp_type;	/* == dpt_Get{Plat|Class}Struct	*/
	int dsp_id;			/* ID of interest (precedence)	*/
	char dsp_name[CFG_PLATNAME_LEN];/* Name of interest		*/
};


struct dsp_PlatStruct
{
	enum dsp_Types dsp_type;	/* == dpt_R_PlatStruct		*/
	int dsp_result;			/* zero on failure		*/
	PlatformId dsp_pid;		/* Requested PlatformId		*/
	Platform dsp_plat;		/* Platform structure		*/
};
	

struct dsp_ClassStruct
{
	enum dsp_Types dsp_type;	/* == dpt_R_ClassStruct/DefineClass */
	int dsp_result;			/* zero on failure		*/
	PlatClassId dsp_cid;		/* Requested class id		*/
	PlatformClass dsp_class;	/* Platform class structure	*/
	PlatClassId dsp_subplatid[1];	/* dsp_class.dpc_nsubplats	*/
	/* followed by subplat names separated by null characters */
	/* followed by the null-terminated comment string */
        /* followed by dsp_class.dpc_nfields null-separated field strings */
};


/*
 * A search for platform structures
 */
struct dsp_PlatformSearch
{
	enum dsp_Types dsp_type;	/* == dpt_PlatformSearch	*/
	PlatformId dsp_parent;		/* Req'd if dsp_children TRUE	*/
	zbool dsp_children;		/* Want children of dsp_parent	*/
	zbool dsp_subplats;		/* Include subplatforms in search*/
	zbool dsp_alphabet;		/* Return list in alphabetical order*/
# ifdef notdef	/* no longer used */
	zbool dsp_sendplats;		/* Send structures as well 	*/
# endif
	char dsp_regexp[CFG_PLATNAME_LEN];/* regexp pattern to match names*/
};


struct dsp_PlatformList
{
	enum dsp_Types dsp_type;	/* == dpt_R_PlatformSearch	*/
					/* == dpt_R_SubplatsSearch	*/
	int dsp_npids;			/* Number matches in dsp_pids	*/
/*
 * The actual size of this member, and hence the whole structure,
 * depends upon the number of matches found
 */
	PlatformId dsp_pids[1];		/* Platform IDs matching search	*/
};


# ifdef notdef	/* no longer used */
struct dsp_PlatStructSearch
{
	enum dsp_Types dsp_type;	/* == dpt_R_PlatStructSearch	*/
	PlatformId dsp_pid;		/* The ID of the plat struct	*/
	Platform dsp_plat;		/* Platform structure		*/
};
	

/*
 * Locking stuff.
 */
struct dsp_PLock
{
	enum dsp_Types dsp_type;	/* == dpt_PLock || dpt_PLockGranted */
	PlatformId dsp_pid;		/* Which platform		*/
};


/*
 * Look up platform and class names.
 */
struct dsp_PLookup
{
	enum dsp_Types dsp_type;	/* == dpt_Lookup(Platform|Class)*/
	char dsp_name[CFG_PLATNAME_LEN];/* Name of interest		*/
};
#endif

struct dsp_PID
{
	enum dsp_Types dsp_type;	/* == dpt_R_(PID|CID)		*/
	PlatformId dsp_pid;		/* Plat/class id or BadPlatform	*/
};


/*
 * Structures for class definitions and instantiating platforms
 */
struct dsp_AddSubplat
{
	enum dsp_Types dsp_type;	/* == dpt_AddSubplat		*/
	int dsp_class;			/* Id of target class		*/
	SubPlatform dsp_subplat;	/* description of subplatform	*/
};

struct dsp_Instance
{
	enum dsp_Types dsp_type;	/* == dpt_Instantiate		*/
	int dsp_class;			/* class to instantiate		*/
	char dsp_name[CFG_PLATNAME_LEN];/* name of instance		*/
	PlatformId dsp_parent;		/* ID of parent platform	*/
};



/*
 * Find DFs based on the time of interest.
 */
struct dsp_FindDF
{
	enum dsp_Types dsp_type;	/* dpt_FindDF | dpt_FindAfter	*/
	PlatformId dsp_pid;		/* Platform of interest		*/
	ZebTime dsp_when;		/* Time				*/
	int dsp_srcid;			/* Which source?		*/
};


struct dsp_R_DataFile
{
	enum dsp_Types dsp_type;	/* == dpt_R_DataFile		*/
	int dsp_success;
	DataFile dsp_file;		/* The file found		*/
};

/*
 * Find next or previous DF, relative to a given DF
 */
struct dsp_FindDFLink
{
	enum dsp_Types dsp_type;	/* dpt_FindDFPrev | dpt_FindDFNext */
	DataFile dsp_file;		/* find relative to this file	*/
};


/*
 * Source info
 */
struct dsp_GetSrcInfo
{
	enum dsp_Types dsp_type;	/* dpt_GetSrcInfo */
	int dsp_srcid;			/* id of the desired source */
};

struct dsp_R_SrcInfo
{
	enum dsp_Types dsp_type;	/* == dpt_R_SrcInfo	*/
	int dsp_success;		/* non-zero for success */
	SourceInfo dsp_srcinfo;
};

/*
 * Platform directory
 */
struct dsp_GetPlatDir
{
	enum dsp_Types dsp_type;	/* dpt_GetPlatDir		*/
	int dsp_srcid;			/* id of the desired source	*/
	int dsp_pid;			/* id of the desired platform	*/
};

struct dsp_R_PlatDir
{
	enum dsp_Types dsp_type;	/* == dpt_R_PlatDir	*/
	int dsp_success;		/* non-zero for success */
	char dsp_dir[DSP_FLEN];		/* the directory 	*/
};

/*
 * Protocol stuff.  This is received by the client after the initial
 * greeting.
 */
struct dsp_ProtoVersion
{
	enum dsp_Types 	dsp_type;	/* == dpt_ProtoVersion	*/
	int		dsp_version;
};


/*
 * Data application notifications --- they go here rather than dsDaemon.h
 * since other programs (e.g. prt_Notify) call them, and the prototypes
 * require the protocol structures declared above.
 */
void dap_Request FP ((char *, struct dsp_NotifyRequest *));
void dap_Cancel FP ((char *client));
void dap_Disconnect FP ((char *client));
void dap_Notify FP ((PlatformId, ZebTime *, int, int, int));
void dap_Copy FP ((char *));
int dap_IsInterest FP ((int pid));

# if __cplusplus
}	// close extern "C"
# endif

#endif /* _dsPrivate_h_ */
