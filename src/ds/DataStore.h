/*
 * $Id: DataStore.h,v 2.3 1991-11-07 22:21:21 corbet Exp $
 *
 * Public data store definitions.
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

# define MAXFIELD	30	/* Max fields in one data object	*/

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
	OrgCmpImage	= 7,
        Org1dGrid       = 8
} DataOrganization;

/*
 * The platform id format.
 */
typedef int PlatformId;
# define BadPlatform -1


/*
 * Scale and bias info for integer-encoded fields. (OrgImage)
 */
typedef struct _ScaleInfo
{
	float	s_Scale;		/* real value = data/s_scale	*/
	float	s_Offset;		/*   + s_Offset			*/
} ScaleInfo;


/*
 * Irregular grids look like this.
 */
typedef struct _IRGrid
{
	int	ir_npoint;		/* Number of points in the grid	*/
	Location *ir_loc;		/* Location array		*/
	PlatformId *ir_subplats;	/* Subplatform array		*/
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
 * Boundaries are organized around these.
 */
typedef struct _BndDesc
{
	PlatformId	bd_pid;		/* Id for this boundary		*/
	int		bd_begin;	/* Begin offset			*/
	int		bd_npoint;	/* Number of points		*/
} BndDesc;

/*
 * For raster image datasets, we have this info.
 */
typedef struct _RastImg
{
	RGrid 	*ri_rg;			/* Geometry info		*/
	ScaleInfo *ri_scale;		/* Scaling information		*/
} RastImg;

/*
 * These and more are crammed into the data object via one of these unions:
 */
typedef union _Dunion
{
	IRGrid	d_irgrid;
	RGrid	d_rgrid;
	/* int	*d_length; */
	RastImg d_img;			/* Image description		*/
	BndDesc	*d_bnd;			/* Boundary description		*/
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
	int		do_nbyte;	/* Data bytes per field		*/
	Location	do_loc;		/* Location for fixed platforms	*/
	Location	*do_aloc;	/* Location for mobile platforms */
	time		*do_times;	/* Sample time array		*/
	Dunion		do_desc;	/* Data description		*/
	float		*do_data[MAXFIELD];/* The actual data		*/
	int		do_nfield;	/* How many fields		*/
	char		*do_fields[MAXFIELD];	/* The fields		*/
	int		do_flags;	/* Flags			*/
	float		do_badval;	/* Bad value flag		*/
	char		*do_attr;	/* The table			*/
} DataObject;

# define DOF_FREEDATA		0x0001	/* Free data[0]			*/
# define DOF_FREEALLDATA	0x0002	/* Free data for each field	*/
# define DOF_FREEALOC		0x0004	/* Free location array		*/
# define DOF_FREETIME		0x0008	/* Free times array		*/
# define DOF_FREEATTR		0x0010	/* Free the attributes		*/

/*
 * The name of the daemon, as known to the message system.
 */
# define DS_DAEMON_NAME "DS_Daemon"


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
	void		ds_PutData (DataObject *, int);
	void		ds_DeleteData (PlatformId, int);
	void		ds_RequestNotify (PlatformId, int, void (*)());
	void		ds_CancelNotify (void);
	int		ds_DataTimes (PlatformId, time *, int,TimeSpec,time *);
	int		ds_GetObsSamples (PlatformId, time *, time *,
					Location *, int);
	int		ds_GetFields (PlatformId, time *, int *, char **);
	int		ds_GetObsTimes (PlatformId, time *, time *, int,
				char *);
# else
	int		ds_Initialize ();
	PlatformId	ds_LookupPlatform ();
	char *		ds_PlatformName ();
	int		ds_IsMobile ();
	void		ds_FreeDataObject ();
	DataObject *	ds_GetData ();
	DataObject *	ds_GetObservation ();
	void		ds_PutData ();
	void		ds_DeleteData ();
	void		ds_RequestNotify ();
	void		ds_CancelNotify ();
	int		ds_DataTimes ();
	int		ds_GetObsSamples ();
	int		ds_GetFields ();
	int		ds_GetObsTimes ();
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

