/*
 * $Id: dsPrivate.h,v 1.2 1991-01-16 22:06:46 corbet Exp $
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
	FTMDA = 1,
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
	int	dp_flags;		/* Attribute flags -- see below	*/
	int	dp_parent;		/* Hierarchy backpointer	*/
	DataOrganization dp_org;	/* Native data organization	*/
	FileType dp_ftype;		/* File type			*/
	int	dp_keep;		/* Minimum data keep		*/
	int	dp_LocalData;		/* The local data table		*/
	int	dp_RemoteData;		/* The remote data table	*/
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
	int	df_FLink;		/* Data table forward link	*/
	int	df_BLink;		/* Data table backward link	*/
	int	df_platform;		/* Platform index		*/
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
