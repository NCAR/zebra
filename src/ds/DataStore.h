/*
 * $Id: DataStore.h,v 1.2 1991-01-16 22:06:46 corbet Exp $
 *
 * Public data store definitions.
 */

# define MAXFIELD	10	/* Max fields in one data object	*/

/*
 * Possible data organizations.
 */
typedef enum {
	OrgUnknown	= 0,
	Org2dGrid	= 1,
	OrgIRGrid	= 2,
	OrgScalar	= 3,
	OrgImage	= 4,
	OrgOutline	= 5,
	Org3dGrid	= 6,
} DataOrganization;

/*
 * The platform id format.
 */
typedef int PlatformId;
# define BadPlatform -1

/*
 * Irregular grids look like this.
 */
typedef struct _IRGrid
{
	int	ir_npoint;		/* Number of points in the grid	*/
	Location *ir_loc;		/* Location array		*/
} IRGrid;

/*
 * Regular grids, on the other hand, have this sort of appearance.
 */
typedef struct _RGrid
{
	float	rg_Xspacing;		/* X dimension spacing		*/
	float	rg_Yspacing;		/* Y (north/south) spacing	*/
	float	rg_Zspacing;		/* Vertical spacing		*/
	int	rg_nX, rg_nY, rg_nZ;	/* Dimensions			*/
} RGrid;


/*
 * These and more are crammed into the data object via one of these unions:
 */
typedef union _Dunion
{
	IRGrid	d_irgrid;
	RGrid	d_rgrid;
} Dunion;

/*
 * The data object format.
 */
typedef struct _DataObject
{
	PlatformId	do_id;		/* The platform represented here */
	time		do_begin;	/* Begin time of the data	*/
	time		do_end;		/* The end time.		*/
	DataOrganization do_org;	/* Organization of the data	*/
	int		do_npoint;	/* Number of samples here	*/
	Location	do_loc;		/* Location for fixed platforms	*/
	Location	*do_aloc;	/* Location for mobile platforms */
	time		*do_times;	/* Sample time array		*/
	Dunion		do_desc;	/* Data description		*/
	float		*do_data[MAXFIELD];/* The actual data		*/
	int		do_nfield;	/* How many fields		*/
	char		*do_fields[MAXFIELD];	/* The fields		*/
	int		do_flags;	/* Flags			*/
	float		do_badval;	/* Bad value flag		*/
} DataObject;

# define DOF_FREEDATA		0x0001	/* Free data[0]			*/
# define DOF_FREEALLDATA	0x0002	/* Free data for each field	*/
# define DOF_FREEALOC		0x0004	/* Free location array		*/
# define DOF_FREETIME		0x0008	/* Free times array		*/

/*
 * The name of the daemon, as known to the message system.
 */
# define DS_DAEMON_NAME "DS_Daemon"


/* # define ds_FreeDataObject ds_RealFreeDataObject	/* for now */
# ifdef __STDC__
	int		ds_Initialize (void);
	PlatformId	ds_LookupPlatform (char *);
	char *		ds_PlatformName (PlatformId);
	int		ds_IsMobile (PlatformId);
	void		ds_FreeDataObject (DataObject *);
	DataObject *	ds_GetData (PlatformId, char **, int, time *, time *,
				DataOrganization, double, double);
	DataObject *	ds_GetObservation (PlatformId, char **, int, time *, 
				DataOrganization, double, double);
# else
	int		ds_Initialize ();
	PlatformId	ds_LookupPlatform ();
	char *		ds_PlatformName ();
	int		ds_IsMobile ();
	void		ds_FreeDataObject ();
	DataObject *	ds_GetData ();
	DataObject *	ds_GetObservation ();
# endif


/*
 * Simple data object access functions.  They are put here in the hopes
 * that a reasonable compiler is being used, which will inline them.
 */
static inline Location *
ds_Where (DataObject *obj, int sample)
/*
 * Where is this sample?
 */
{
	return (ds_IsMobile (obj->do_id) ? obj->do_aloc+sample : &obj->do_loc);
}


/*
 * Time specification for ds_DataTimes.
 */
typedef enum
{
	DsBefore,
	DsAfter,
	DsNearest,
	DsCenter
} TimeSpec;
