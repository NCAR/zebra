/*
 * $Id: dsPrivate.h,v 1.3 1991-02-26 19:12:09 corbet Exp $
 *
 * Data store information meant for DS (daemon and access) eyes only.
 */

/*
 * The file types.  These are tied into the format table definition in
 * DFA, so don't mess with them.
 */
typedef enum {
	FTUnknown = -1,
	FTNetCDF = 0,
	FTBoundary = 1,
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
	int	sm_DTFreeList;		/* First free entry		*/
};
# define SHM_MAGIC	0x102390



/*
 * A platform in the data table is described by this structure.
 */
# define NAMELEN 60
typedef struct ds_Platform
{
	char	dp_name[NAMELEN];	/* The full name of this platform */
	char	dp_class[NAMELEN];	/* The class of the platform	*/
	char	dp_dir[NAMELEN];	/* File directory		*/
	int	dp_parent;		/* Hierarchy backpointer	*/
	DataOrganization dp_org;	/* Native data organization	*/
	FileType dp_ftype;		/* File type			*/
	short	dp_keep;		/* Minimum data keep		*/
	short	dp_maxsamp;		/* Maximum file samples		*/
	int	dp_LocalData;		/* The local data table		*/
	int	dp_RemoteData;		/* The remote data table	*/
	short	dp_flags;		/* Attribute flags -- see below	*/
	short	dp_Tfile;		/* Temp file under creation	*/
} Platform;

# define DPF_MOBILE	0x0001		/* Does this platform move?	*/
# define DPF_COMPOSITE	0x0002		/* A grouping of other plats?	*/
# define DPF_DISCRETE	0x0004		/* "Continuous" data?		*/
# define DPF_REGULAR	0x0008		/* Regularly-spaced (time) samples? */
# define DPF_SUBPLATFORM 0x010		/* This is a sub platform	*/

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
typedef struct ds_DataFile
{
	char	df_name[NAMELEN];	/* The name of the file		*/
	FileType df_ftype;		/* Type of this file		*/
	time	df_begin;		/* When the data begins		*/
	time	df_end;			/* When it ends			*/
	int	df_rev;			/* Revision count		*/
	short	df_FLink;		/* Data table forward link	*/
	short	df_BLink;		/* Data table backward link	*/
	short	df_nsample;		/* How many samples in this file */
	short	df_platform;		/* Platform index		*/
	int	df_use;			/* Structure use count		*/
} DataFile;



/*
 * The size of the shared memory segment, and the pointer which will locate
 * it in each process.
 */
# define SHM_SIZE 32768
char *ShmSegment;
struct ds_ShmHeader *ShmHeader;
# define DS_KEY 0x072161

/*
 * The semaphores which control the shared memory setup.
 */
# define S_READ		0	/* The read semaphore	*/
# define S_WRITE	1	/* The write semaphore	*/


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
};
# define DSP_FLEN	256		/* File name length		*/

/*
 * Create a new data file.
 */
struct dsp_CreateFile
{
	enum dsp_Types dsp_type;	/* == dpt_NewFileRequest	*/
	PlatformId dsp_plat;		/* Platform ID			*/
	time	dsp_time;		/* Initial file time		*/
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
	time	dsp_EndTime;		/* The new end time		*/
	int	dsp_NSamples;		/* How many samples added	*/
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
	time dsp_when;			/* The lastest time for data	*/
};

