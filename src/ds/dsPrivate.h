/*
 * $Id: dsPrivate.h,v 3.27 1995-02-10 01:21:03 granger Exp $
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

# include <config.h>		/* CFG_ symbol definitions */
# include <sys/types.h>		/* inode type for DataFile */

/*
 * How many top level platforms do we allow?  Subplatforms are not included
 * in this count.
 */	
# define MAXPLAT	CFG_MAX_PLATFORMS

/*
 * The file types.  These are tied into the format table definition in
 * DFA, so don't mess with them.
 */
typedef enum {
	FTUnknown = -1,
	FTNetCDF = 0,
	FTBoundary = 1,
	FTRaster = 2,
	FTCmpRaster = 3,
	FTZeb = 4,
	FTGRIB = 5,
	FTGRIBSfc = 6,	/* GRIB surface grids only */
	FTGrads = 7
	/* ... */
} FileType;


/*
 * The following structure is used to keep track of locks held on platforms
 * by other processes.
 */
typedef struct s_Lock
{
	char	l_Owner[MAX_NAME_LEN];	/* Who owns the lock	*/
	ZebTime l_When;			/* Since when		*/
	struct s_Lock *l_Next;		/* Next in chain	*/
} Lock;


# define NAMELEN 	CFG_PLATNAME_LEN

/*
 * Structure for describing a subplatform 'template', where the class info
 * comes from the subplatform's class.  An instance name is also required
 * to create instances of classes which reference this subplatform.
 */
typedef struct ds_SubPlatform
{
	int	dps_class;		/* Class of this subplatform	*/
	char	dps_name[NAMELEN];	/* Name for instances		*/
} SubPlatform;

/*
 * Types of directory inheritance for classes
 */
typedef enum { InheritNone = 0, InheritAppend, InheritCopy } InheritDir;

/*
 * Types of directory instantiation for classes
 */
typedef enum { 
	InstanceDefault = 0, InstanceRoot = 0, 
	InstanceCopyClass, InstanceSubdirClass, 
	InstanceCopyParent, InstanceSubdirParent
} InstanceDir;

/*
 * Information that is common among platform instances and does not
 * change during run-time resides in the platform class structure.
 */
typedef struct ds_PlatformClass
{
	char	dpc_name[NAMELEN];	/* The full name of this class  */
	char	dpc_dir[NAMELEN];	/* File directory		*/
	char	dpc_rdir[NAMELEN];	/* Remote file directory	*/
	int	dpc_superclass;		/* Class hierarchy backpointer	*/
	DataOrganization dpc_org;	/* Native data organization	*/
	FileType dpc_ftype;		/* File type			*/
	unsigned short dpc_keep;	/* Minimum data keep		*/
	unsigned short dpc_maxsamp;	/* Maximum file samples		*/
	unsigned short dpc_flags;	/* Attribute flags -- see below	*/
	unsigned char dpc_inherit;	/* Directory inheritance flags	*/
	unsigned char dpc_instance;	/* Directory instance flags	*/
	char 	*dpc_comment;		/* Comment about this class	*/
	SubPlatform *dpc_subplats;	/* Subplatform templates	*/
	int	dpc_nsubplats;		/* Number of subplats in array	*/
} PlatformClass;

/*
 * The platform instance structure.  Most of the static information is
 * retrieved by following the pointer to the class structure in the class
 * table.  The instance structure has its own members for stuff that is
 * likely to be different among instances of a class, such as the data
 * directories.
 */
typedef struct ds_PlatformInstance
{
	char	dp_name[NAMELEN];	/* Full name of this platform 	*/
	int	dp_class;		/* The class of the platform	*/
	int	dp_parent;		/* Hierarchy backpointer	*/
	char	dp_dir[NAMELEN];	/* Local data directory		*/
	char	dp_rdir[NAMELEN];	/* Remote data directory	*/
	int	dp_LocalData;		/* The local data table		*/
	int	dp_RemoteData;		/* The remote data table	*/
	int	*dp_subplats;		/* Indices to subplat instances	*/
	int	dp_nsubplats;		/* Number of indices (not alloc)*/
	unsigned short dp_flags;	/* Attribute flags -- see below	*/
	int	dp_Tfile;	/* Temp file under creation	*/
	unsigned short dp_NewSamps;	/* New samps (not yet notified) */
	unsigned short dp_OwSamps;	/* Overwritten samps (n.y.n.)	*/
	Lock	*dp_RLockQ;		/* Read locks held		*/
	Lock	*dp_WLockQ;		/* The write lock 		*/
} PlatformInstance, Platform;

/*
 * The platform structure used by the client library, which unifies the
 * information in the class and instance structures.  This structure is
 * fixed in size and can be packed into messages, and only contains info
 * of use to the client.
 */
typedef struct ds_ClientPlatform
{
	char	cp_name[NAMELEN];	/* The full name of this platform */
	char	cp_class[NAMELEN];	/* The class of the platform	*/
	char	cp_dir[NAMELEN];	/* File directory		*/
	char	cp_rdir[NAMELEN];	/* Remote file directory	*/
	int	cp_parent;		/* Hierarchy backpointer	*/
	DataOrganization cp_org;	/* Native data organization	*/
	FileType cp_ftype;		/* File type			*/
	unsigned short cp_keep;		/* Minimum data keep		*/
	unsigned short cp_maxsamp;	/* Maximum file samples		*/
	int	cp_LocalData;		/* The local data table		*/
	int	cp_RemoteData;		/* The remote data table	*/
	unsigned short cp_flags;	/* Attribute flags -- see below	*/
} ClientPlatform;

/*
 * These flags belong to the class
 */
# define DPF_MOBILE	0x0001		/* Does this platform move?	*/
# define DPF_COMPOSITE	0x0002		/* A grouping of other plats?	*/
# define DPF_DISCRETE	0x0004		/* "Continuous" data?		*/
# define DPF_REGULAR	0x0008		/* Regularly-spaced (time) samples? */
# define DPF_SUBPLATFORM 0x010		/* This is a sub platform	*/
# define DPF_REMOTE	0x0020		/* A remote dir has been given	*/
# define DPF_SPLIT	0x0040		/* Split on day boundary 	*/
# define DPF_MODEL	0x0080		/* Model data, i.e., has	*/
					/* separate issue/valid times	*/
# define DPF_VIRTUAL	0x0100		/* Only a node in the hierarchy */
# define DPF_ABSTRACT	0x0200		/* Abstract platform class	*/

/*
 * These flags belong to the instance
 */
# define DPF_CLOADED	0x1000		/* Cache has been loaded	*/
# define DPF_RCLOADED	0x2000		/* Remote cache loaded		*/
# define DPF_DIRTY	0x4000		/* Cache needs updating		*/


/*
 * Blocks by which subplat allocated arrays are increased.
 */
# define ALLOC_SUBPLATS	10


# define FNAMELEN 	CFG_DATAFILE_LEN
/*
 * The structure describing a file full of data.
 */
typedef struct ds_DataFile
{
	char	df_name[FNAMELEN];	/* The name of the file		*/
	FileType df_ftype;		/* Type of this file		*/
	ZebTime	df_begin;		/* When the data begins		*/
	ZebTime	df_end;			/* When it ends			*/
	long	df_rev;			/* Revision count		*/
	ino_t	df_inode;		/* Inode number			*/
	int	df_FLink;		/* Data table forward link	*/
	int	df_BLink;		/* Data table backward link	*/
	unsigned short df_nsample;	/* How many samples in this file */
	short	df_platform;		/* Platform index		*/
#ifdef DF_USE
	short	df_use;			/* Structure use count		*/
#endif
	int	df_index;		/* Data file index		*/
	char	df_flags;		/* File flags			*/
} DataFile;

/*
 * Flags:
 */
# define DFF_Archived	0x01		/* File has been archived	*/
# define DFF_Seen	0x02		/* Seen during rescan		*/
# define DFF_Remote	0x04		/* File in remote list		*/
# define DFF_PlatMarker 0x08		/* Kludge: marker for cache	*/

/*
 * Write codes -- used to control the placement of data.
 */
typedef enum
{
	wc_NewFile,		/* Create a new file		*/
	wc_Overwrite,		/* Overwrite existing datum	*/
	wc_Append,		/* Append to existing file	*/
	wc_Insert		/* Insert before some data	*/
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
	dpt_AbortNewFile,		/* Abort new file creation	*/
	dpt_UpdateFile,			/* Notify of file update	*/
	dpt_NotifyRequest,		/* 5: Ask for data available notify*/
	 dpt_Notify,			/* Data available		*/
	dpt_CancelNotify,		/* Cancel notifications		*/
	 dpt_CancelAck,			/* Acknowledge cancel		*/
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
	dpt_GetFileStruct,		/* 20: Get a DFE		*/
	 dpt_R_FileStruct,
	dpt_PLock,			/* Lock a platform		*/
	 dpt_R_PLockGranted,		/* Acknowledge the lock		*/
	dpt_ReleasePLock,		/* Release platform lock	*/
	dpt_LookupPlatform,		/* 25: Plat name -> pid translation*/
	 dpt_R_PID,			/* Reply 			*/
	dpt_FindDF,			/* Find DFE based on time	*/
	 dpt_R_DFIndex,			/* Index of found DF		*/
	dpt_CacheInvalidate,		/* Invalidate a cache entry	*/
	dpt_Hello,			/* 30: New client greeting	*/
	 dpt_R_ProtoVersion,		/* Protocol version		*/
	dpt_FindAfter,			/* Find closest DFE after Time	*/
	dpt_WriteLock,			/* Write lock a platform	*/
	dpt_ReleaseWLock,		/* Release write lock		*/
	dpt_DeleteObs,			/* 35: DANGER remove an observation*/
	dpt_PlatformSearch,		/* Match regex to platform names*/
	 dpt_R_PlatformSearch,		/* Reply with list of plat ids	*/
	 dpt_R_PlatStructSearch		/* For sending back structs 	*/
};

# define DSP_FLEN	CFG_FILEPATH_LEN /* File name length		*/

/*
 * The current data store protocol version.  CHANGE this when incompatible
 * protocol changes have been made.
 */
# define DSProtocolVersion	0x950130

/*
 * Create a new data file.
 */
struct dsp_CreateFile
{
	enum dsp_Types dsp_type;	/* == dpt_NewFileRequest	*/
	PlatformId dsp_plat;		/* Platform ID			*/
	ZebTime	dsp_time;		/* Initial file time		*/
	char	dsp_file[DSP_FLEN];	/* The name of the file		*/
};

struct dsp_R_CreateFile
{
	enum dsp_Types dsp_type;	/* == dpt_R_NewFileSomething	*/
	int	dsp_FileIndex;		/* Index of new DFI		*/
};


/*
 * Abort new data file.
 */
struct dsp_AbortNewFile
{
	enum dsp_Types dsp_type;	/* == dpt_AbortNewFile		*/
	int	dsp_FileIndex;		/* Index of aborted file	*/
	PlatformId dsp_pid;		/* Associated platform		*/
};


/*
 * File update notification.
 */
struct dsp_UpdateFile
{
	enum dsp_Types dsp_type;	/* == dpt_UpdateFile		*/
	int	dsp_FileIndex;		/* The index of this file	*/
	ZebTime	dsp_EndTime;		/* The new end time		*/
	int	dsp_NSamples;		/* How many samples added	*/
	int	dsp_NOverwrite;		/* How many overwritten		*/
	int	dsp_Last;		/* Last update			*/
	int	dsp_Local;		/* true if updating local file	*/
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
	int dsp_file;			/* File index			*/
};



/*
 * The application notification mechanism.
 */
struct dsp_NotifyRequest
{
	enum dsp_Types dsp_type;	/* == dpt_NotifyRequest		*/
	PlatformId dsp_pid;		/* Platform of interest		*/
	int dsp_param;			/* Requestor parameter		*/
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
	int		dsp_FileIndex;	/* Index of the archived file	*/
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
	char dsp_File[DSP_FLEN];	/* The file -- sans directory	*/
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
	enum dsp_Types dsp_type;	/* == dpt_GetPlatStruct		*/
	PlatformId dsp_pid;		/* Platform of interest		*/
};


struct dsp_PlatStruct
{
	enum dsp_Types dsp_type;	/* == dpt_R_PlatStruct		*/
	ClientPlatform dsp_plat;	/* Platform structure		*/
};
	

/*
 * A search for platform structures
 */
struct dsp_PlatformSearch
{
	enum dsp_Types dsp_type;	/* == dpt_PlatformSearch	*/
	PlatformId dsp_parent;		/* Req'd if dsp_children TRUE	*/
	bool dsp_children;		/* Want children of dsp_parent	*/
	bool dsp_subplats;		/* Include subplatforms in search*/
	bool dsp_alphabet;		/* Return list in alphabetical order*/
	bool dsp_sendplats;		/* Send structures as well 	*/
	char dsp_regexp[NAMELEN];	/* regexp pattern to match names*/
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


struct dsp_PlatStructSearch
{
	enum dsp_Types dsp_type;	/* == dpt_R_PlatStructSearch	*/
	PlatformId dsp_pid;		/* The ID of the plat struct	*/
	ClientPlatform dsp_plat;	/* Platform structure		*/
};
	

/*
 * And file structure too.
 */
struct dsp_GetFileStruct
{
	enum dsp_Types dsp_type;	/* == dpt_GetFileStruct		*/
	int dsp_index;			/* Index of file of interest	*/
};

struct dsp_FileStruct
{
	enum dsp_Types dsp_type;	/* == dpt_R_FileStruct		*/
	DataFile dsp_file;		/* The file structure		*/
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
 * Look up platform names.
 */
struct dsp_PLookup
{
	enum dsp_Types dsp_type;	/* == dpt_LookupPlatform	*/
	char dsp_name[NAMELEN];		/* Name of interest		*/
};

struct dsp_PID
{
	enum dsp_Types dsp_type;	/* == dpt_R_PID			*/
	PlatformId dsp_pid;		/* Plat id or BadPlatform	*/
};


/*
 * Find DFE's based on the time of interest.
 */
struct dsp_FindDF
{
	enum dsp_Types dsp_type;	/* == dpt_FindDF		*/
	PlatformId dsp_pid;		/* Platform of interest		*/
	ZebTime dsp_when;		/* Time				*/
	int dsp_src;			/* Which source?		*/
};
# define SRC_ALL (-1)


struct dsp_R_DFI
{
	enum dsp_Types dsp_type;	/* == dpt_R_DFIndex		*/
	int dsp_index;			/* index of our file		*/
};


/*
 * Cache invalidations.
 */
struct dsp_CacheInvalidate
{
	enum dsp_Types dsp_type;	/* == dpt_CacheInvalidate	*/
	DataFile dsp_dfe;		/* New, updated DFE		*/
};


/*
 * Protocol stuff
 */
struct dsp_ProtoVersion
{
	enum dsp_Types dsp_type;	/* == dpt_ProtoVersion	*/
	int	dsp_version;
};
