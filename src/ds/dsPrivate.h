/*
 * $Id: dsPrivate.h,v 3.8 1993-02-08 22:35:50 corbet Exp $
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
	/* ... */
} FileType;


/*
 * The shared memory header.
 */
struct ds_ShmHeader
{
	int	sm_magic;		/* Magic ID number		*/
	int	sm_nPlatform;		/* How many platforms		*/
	int	sm_PTOffset;		/* Offset to platform table	*/
	int	sm_DTOffset;		/* Offset to the data table	*/
	int	sm_nDataTable;		/* How many data table entries	*/
	int	sm_nDTEUsed;		/* How many used		*/
	int	sm_DTFreeList;		/* First free entry		*/
};
# define SHM_MAGIC	0x80692		/* Change for incompatible changes */



/*
 * A platform in the data table is described by this structure.
 */
# define NAMELEN 80
typedef struct ds_Platform
{
	char	dp_name[NAMELEN];	/* The full name of this platform */
	char	dp_class[NAMELEN];	/* The class of the platform	*/
	char	dp_dir[NAMELEN];	/* File directory		*/
	char	dp_rdir[NAMELEN];	/* Remote file directory	*/
	int	dp_parent;		/* Hierarchy backpointer	*/
	DataOrganization dp_org;	/* Native data organization	*/
	FileType dp_ftype;		/* File type			*/
	unsigned short dp_keep;		/* Minimum data keep		*/
	unsigned short dp_maxsamp;	/* Maximum file samples		*/
	int	dp_LocalData;		/* The local data table		*/
	int	dp_RemoteData;		/* The remote data table	*/
	short	dp_flags;		/* Attribute flags -- see below	*/
	short	dp_Tfile;		/* Temp file under creation	*/
	unsigned short dp_NewSamps;	/* New samps (not yet notified) */
	unsigned short dp_OwSamps;	/* Overwritten samps (n.y.n.)	*/
} Platform;

# define DPF_MOBILE	0x0001		/* Does this platform move?	*/
# define DPF_COMPOSITE	0x0002		/* A grouping of other plats?	*/
# define DPF_DISCRETE	0x0004		/* "Continuous" data?		*/
# define DPF_REGULAR	0x0008		/* Regularly-spaced (time) samples? */
# define DPF_SUBPLATFORM 0x010		/* This is a sub platform	*/
# define DPF_REMOTE	0x0020		/* A remote dir has been given	*/
# define DPF_SPLIT	0x0040		/* Split on day boundary 	*/
/*
 * Macro to return the right data list for a platform.
 */
# define LOCALDATA(p) (((p).dp_flags & DPF_SUBPLATFORM) ? \
		PTable[(p).dp_parent].dp_LocalData : (p).dp_LocalData)
# define REMOTEDATA(p) (((p).dp_flags & DPF_SUBPLATFORM) ? \
		PTable[(p).dp_parent].dp_RemoteData : (p).dp_RemoteData)

/*
 * The structure describing a file full of data.
 */
# define FNAMELEN 32
typedef struct ds_DataFile
{
	char	df_name[FNAMELEN];	/* The name of the file		*/
	FileType df_ftype;		/* Type of this file		*/
	ZebTime	df_begin;		/* When the data begins		*/
	ZebTime	df_end;			/* When it ends			*/
	long	df_rev;			/* Revision count		*/
	short	df_FLink;		/* Data table forward link	*/
	short	df_BLink;		/* Data table backward link	*/
	unsigned short df_nsample;	/* How many samples in this file */
	short	df_platform;		/* Platform index		*/
	short	df_use;			/* Structure use count		*/
	char	df_flags;		/* File flags			*/
} DataFile;

/*
 * Flags:
 */
# define DFF_Archived	0x01		/* File has been archived	*/
# define DFF_Seen	0x02		/* Seen during rescan		*/
# define DFF_Remote	0x04		/* File in remote list		*/


/*
 * The size of the shared memory segment, and the pointer which will locate
 * it in each process.
 */
# define SHM_SIZE 65536*12
char *ShmSegment;
struct ds_ShmHeader *ShmHeader;

# define DS_KEY 0x072161

/*
 * The semaphores which control the shared memory setup.
 */
# define S_READ		0	/* The read semaphore	*/
# define S_WRITE	1	/* The write semaphore	*/


/*
 * Write codes -- used to control the placement of data.
 */
typedef enum
{
	wc_NewFile,		/* Create a new file		*/
	wc_Overwrite,		/* Overwrite existing datum	*/
	wc_Append,		/* Append to existing file	*/
	wc_Insert,		/* Insert before some data	*/
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
	dpt_NotifyRequest,		/* Ask for data available notify*/
	 dpt_Notify,			/* Data available		*/
	dpt_CancelNotify,		/* Cancel notifications		*/
	 dpt_CancelAck,			/* Acknowledge cancel		*/
	dpt_DeleteData,			/* DANGER remove data		*/
	dpt_DataGone,			/* Data deletion announcement	*/
	dpt_CopyNotifyReq,		/* Get copies of notification rq*/
	dpt_MarkArchived,		/* Mark a file as archived	*/
	dpt_R_UpdateAck,		/* Acknowledge a file update	*/
	dpt_Rescan,			/* Force rescan of platform	*/
/*
 * Cross-machine broadcast notifications.
 */
	dpt_BCDataGone,			/* Data zapped			*/
};
# define DSP_FLEN	256		/* File name length		*/

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
};


/*
 * Delete data from the disk.  Use with caution.
 */
struct dsp_DeleteData
{
	enum dsp_Types dsp_type;	/* == dpt_DeleteData		*/
	PlatformId dsp_plat;		/* Target platform		*/
	int 	dsp_leave;		/* Minimum seconds data left	*/
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
