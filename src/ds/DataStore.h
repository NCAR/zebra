/*
 * $Id: DataStore.h,v 3.12 1993-08-12 18:23:33 granger Exp $
 *
 * Public data store definitions.
 */

# ifndef _DATACHUNK_H_
# define _DATACHUNK_H_
# include "ds_fields.h"
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

# define MAXFIELD	100	/* Max fields in one data object	*/

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
        Org1dGrid       = 8,
	OrgTransparent  = 9,
	OrgFixedScalar  = 10,	/* Inflexible scalar for DFA_Zeb */
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

/*
 * Update codes used with application notifications.
 */

typedef enum
{
	UpdOverwrite,		/* Only overwrote existing data	*/
	UpdInsert,		/* Inserted new data in middle	*/
	UpdAppend,		/* New data at the end		*/
} UpdCode;	


/*----------------------------------------------------------------
 * The "detail" mechanism for ds_Fetch.
 */
typedef struct _dsDetail
{
	char	*dd_Name;	/* String identifier 	*/
	SValue	dd_V;		/* Associated value	*/
} dsDetail;

/*----------------------------------------------------------------
 * Definitions of recognized detail identifiers.
 * File-format-specific ids should indicate the format in the name, 
 * e.g. NC for netCDF.
 */
/* 
 * These identifiers, if found in a detail list, specify the type of the
 * time_offset variable in netCDF files, either NC_FLOAT or NC_DOUBLE.
 * The SValue of the detail is ignored.  The appearance of both in the
 * same detail list is undefined.
 */
# define DD_NC_TIME_FLOAT	"dd_time_float" 
# define DD_NC_TIME_DOUBLE	"dd_time_double"

/*
 * Details for doing program testing: Force closure of files when finished
 * with them to free dynamic memory allocated for the opening.
 */
# define DD_FORCE_CLOSURE	"dd_force_closure"

/*
 * DataChunks -- the new "data object" format.
 *
 * Herein we define:
 *	- The raw data object format
 *	- Type codes
 *	- External access functions
 */


/*
 * Here is the list of possible data chunk classes.  It is done this way
 * (as opposed, say, to putting class structure pointers directly in the
 * DC) for network portability reasons.
 *
 * The numbers are used to find class method structures, and should not
 * be changed.
 */
typedef enum _DataClass
{
	DCC_None 	= 0,	/* No class at all -- Marxist data	*/
	DCC_Raw		= 1,
	DCC_Transparent = 2,
	DCC_Boundary	= 3,
	DCC_MetData	= 4,
	DCC_Scalar 	= 5,
	DCC_IRGrid	= 6,
	DCC_RGrid	= 7,
	DCC_Image	= 8,
	DCC_Location	= 9,
	DCC_NSpace	= 10,
	/* DCC_Text 	= 11, */
} DataClass;

/*
 * For the moment, data arrays are typeless.
 */
typedef void *DataPtr;

/*
 * Auxiliary data is linked up by these little guys:
 */
typedef struct _AuxDataEntry
{
	struct _AuxDataEntry	*dca_Next; /* Next in the chain		*/
	DataClass	dca_Class;	/* Class which owns this entry	*/
	short		dca_SubType;	/* Class-specific code		*/
	unsigned short	dca_Len;	/* Length of aux data		*/
	DataPtr	dca_Data;	/* Actual information		*/
	bool		dca_Free;	/* Free this one?		*/
} AuxDataEntry, *AuxDataChain;


/*
 * Raw data objects look like this:
 */ typedef struct _RawDataChunk {
	DataClass	dc_Class;	/* The particular type		*/
	PlatformId	dc_Platform;	/* The platform of interest	*/
	DataPtr	dc_Data;	/* The actual data		*/
	int		dc_DataLen;	/* The length of the data field	*/
	AuxDataChain	dc_AuxData;	/* Subclass data		*/
} DataChunk, RawDataChunk;
	

/*
 * Class-specific defines.
 */
# define DC_MaxField	MAXFIELD	/* MetData -- max # of fields	*/
# define DC_MaxDimension	30	/* Maximum number of dimensions */
# define DC_MaxDimName		32	/* Max name length, incl \0	*/


/*
 * Definitions of basic routines dealing with data chunks.
 */
bool		dc_IsSubClassOf FP((DataClass, DataClass));

/*
 * Basic data chunk methods.
 */
DataChunk 	*dc_CreateDC FP((DataClass));
void		dc_DestroyDC FP((DataChunk *));
void		dc_DumpDC FP((DataChunk *));
void 		Dc_RawAdd FP((DataChunk *, int));
void		dc_SetGlobalAttr FP ((DataChunk *, char *, char *));
char *		dc_GetGlobalAttr FP ((DataChunk *, char *));
int		dc_ProcessAttrs FP ((DataChunk *, char *, int (*) ()));
void		*dc_GetGlAttrBlock FP ((DataChunk *, int *));
void		dc_SetGlAttrBlock FP ((DataChunk *, void *, int));
int		dc_GetNGlobalAttrs FP ((DataChunk *));
/*
 * Transparent class methods.
 */
int		dc_GetNSample FP((DataChunk *));
DataPtr		dc_GetSample FP((DataChunk *, int, int *));
void		dc_AddSample FP((DataChunk *, ZebTime *, DataPtr, int));
void		dc_SetPlat FP((DataChunk *, int, PlatformId));
PlatformId	dc_GetPlat FP((DataChunk *, int));
bool		dc_GetTime FP((DataChunk *, int, ZebTime *));
void		dc_SortSamples FP((DataChunk *dc));
void		dc_AdjustSample FP((DataChunk *, int, int));
void		dc_SetStaticLoc FP((DataChunk *, Location *));
void		dc_SetLoc FP((DataChunk *, int, Location *));
void		dc_GetLoc FP((DataChunk *, int, Location *));
void		dc_SetSampleAttr FP ((DataChunk *, int, char *, char *));
char		*dc_GetSampleAttr FP ((DataChunk *, int, char *));
void		*dc_GetSaAttrBlock FP ((DataChunk *, int, int *));
void		dc_SetSaAttrBlock FP ((DataChunk *, int, void *, int));
/*
 * Boundary class methods.
 */
void		dc_BndAdd FP((DataChunk *, ZebTime *, PlatformId,
			Location *, int));
Location *	dc_BndGet FP((DataChunk *, int, int *));
/*
 * MetData class
 */
void		dc_SetupUniformFields FP((DataChunk *, int, int,
			FieldId *, int));
void		dc_SetupFields FP((DataChunk *, int, FieldId *));
double		dc_GetBadval FP((DataChunk *));
void		dc_SetBadval FP((DataChunk *, double));
int		dc_GetNField FP((DataChunk *));
void		dc_AddMData FP((DataChunk *, ZebTime *, FieldId, int, int,
			int, DataPtr));
DataPtr		dc_GetMData FP((DataChunk *, int, FieldId, int *));
FieldId		*dc_GetFields FP((DataChunk *, int *));
void		dc_SetFieldAttr FP ((DataChunk *, FieldId, char *, char *));
char		*dc_GetFieldAttr FP ((DataChunk *, FieldId, char *));
void		*dc_GetFiAttrBlock FP ((DataChunk *, FieldId, int *));
void		dc_SetFiAttrBlock FP ((DataChunk *, FieldId, void *, int));
int		dc_GetNFieldAttrs FP ((DataChunk *, FieldId));
char		**dc_GetFieldAttrList FP ((DataChunk *, FieldId, char *,
					   char **values[], int *));
int		dc_ProcessFieldAttrs
	            FP ((DataChunk *, FieldId, char *, int (*)()));
/*
 * Scalar class.
 */
void		dc_SetScalarFields FP((DataChunk *, int, FieldId *));
void		dc_AddScalar FP((DataChunk *, ZebTime *, int, FieldId,
			float *));
void		dc_AddMultScalar FP((DataChunk *, ZebTime *, int, int,
			FieldId, float *));
float		dc_GetScalar FP((DataChunk *, int, FieldId));
/*
 * IRGrid class.
 */
void		dc_IRSetup FP((DataChunk *, int, PlatformId *, Location *,
			int, FieldId *));
void		dc_IRAddGrid FP((DataChunk *, ZebTime *, int, FieldId,
			float *));
void		dc_IRAddScalarDC FP((DataChunk *irgrid_dc, 
				     DataChunk *scalar_dc, int sample,
				     int nsample, int nfield, FieldId *fids));
int		dc_IRGetNPlatform FP((DataChunk *));
void		dc_IRGetPlatforms FP((DataChunk *, PlatformId *, Location *));
float 		*dc_IRGetGrid FP((DataChunk *, int, FieldId));
/*
 * RGrid.
 */
void		dc_RGSetup FP((DataChunk *, int, FieldId *));
void		dc_RGAddGrid FP((DataChunk *, int, FieldId, Location *,
			RGrid *, ZebTime *, float *, int));
float *		dc_RGGetGrid FP((DataChunk *, int, FieldId, Location *,
			RGrid *, int *));
int		dc_RGGeometry FP ((DataChunk *, int, Location *, RGrid *));
/*
 * Image.
 */
void		dc_ImgSetup FP ((DataChunk *, int, FieldId *, ScaleInfo *));
void		dc_ImgAddImage FP ((DataChunk *, int, FieldId, Location *,
			RGrid *, ZebTime *, unsigned char *, int));
unsigned char *	dc_ImgGetImage FP ((DataChunk *, int, FieldId, Location *,
			RGrid *, int *, ScaleInfo *));

/*-------------------------------------------------------------------------
 * The NSpace class
 *-------------------------------------------------------------------------
 * Numerical and Boolean parameters, except for the size of a dimension,
 * will be ints.  Dimension sizes and lengths of data arrays will always be
 * unsigned long.  The 'length' of a data array is the number of floating
 * point elements.  This will be true for both definition and information
 * retrieval.  For retrieval, strings will be returned as pointers to
 * memory inside the datachunk.  The memory pointed to will be valid for
 * the life of the datachunk, but it SHOULD NOT be freed or modified.  Any
 * parameters being passed by reference to hold return values can be passed
 * as NULL, in which case nothing will be returned for that parameter.
 *
 * Interface functions from the NSpace parent class, MetData, can also be
 * useful, but only for retrieving information and data, and only once
 * definition has been completed:
 *
 * dc_GetNField (dc) 	-- For getting the number of fields (aka variables).
 * dc_GetFields (...)	-- For getting the fields and their field IDs.
 * dc_GetMData (...)	-- For retrieving data from a dynamic field at a
 *			   particular sample as an opaque block of bytes.
 *			   There is no way to retrieve static field data
 *			   outside of the NSpace interface.
 * dc_SetBadval(...), 
 * dc_GetBadval (...)	-- Bad values associated with fields (aka variables);
 *			   the bad value can only be set once definition is
 *			   complete.
 *---------------------------------------------------------------------------*/
/*
 * NSpace Definition interface
 */
void dc_NSDefineField FP((DataChunk *dc, FieldId field, int ndims, 
			  char **dimnames,
			  unsigned long *dimsizes,
			  int is_static));
void dc_NSDefineDimension FP((DataChunk *dc, FieldId field, 
			      unsigned long size));

void dc_NSDefineVariable FP((DataChunk *dc, FieldId field, 
			     int ndims, FieldId *dims,
			     int is_static));
void dc_NSDefineComplete FP((DataChunk *dc));
int dc_NSDefineIsComplete FP((DataChunk *dc));
/*
 * NSpace Information retrieval
 */
int dc_NSGetAllDimensions FP((DataChunk *dc, char **names, FieldId *fields, 
			      unsigned long *sizes));
int dc_NSGetAllVariables FP((DataChunk *dc, FieldId *fields, int *ndims));
int dc_NSGetField FP((DataChunk *dc, FieldId field, int *ndims, 
		      char **names, unsigned long *sizes, int *is_static));
int dc_NSGetDimension FP((DataChunk *dc, FieldId dimn, char **name,
			  unsigned long *size));
int dc_NSGetVariable FP((DataChunk *dc, FieldId field, int *ndims, 
			  FieldId *dims, int *is_static));
int dc_NSIsStatic FP((DataChunk *dc, FieldId field));
/*
 * NSpace Data addition
 */
void dc_NSAddSample FP((DataChunk *dc, ZebTime *when, int sample, 
			FieldId field, float *values));
void dc_NSAddStatic FP((DataChunk *dc, FieldId field, float *values));
/*
 * NSpace Data retrieval
 */
float *dc_NSGetSample FP((DataChunk *dc, int sample, FieldId field, 
			  unsigned long *size));
float *dc_NSGetStatic FP((DataChunk *dc, FieldId field, unsigned long *size));


/**************************************************************************
 * Other structures used at the data store application interface level.
 */

/*
 * The platform information structure for application queries.
 */
# define P_NAMELEN 80		/* Mirrors that in dsPrivate.h		*/

typedef struct s_PlatformInfo
{
	char	pl_Name[P_NAMELEN];	/* Name of this platform	*/
	int	pl_NDataSrc;		/* How many data sources	*/
	bool	pl_Mobile;		/* Does it move?		*/
	bool	pl_SubPlatform;		/* Is it a subplat?		*/
	PlatformId pl_Parent;		/* Parent plat for subplats	*/
} PlatformInfo;;

/*
 * Types of data sources.  This definition will move elsewhere some day 
 * as the data source notion moves back.
 */
typedef enum
{
	dst_Local,		/* Local disk source	*/
	dst_NFS,		/* Remotely mounted fs	*/
} DataSrcType;


/*
 * The platform data source structure.
 */
typedef struct s_DataSrcInfo
{
	char	dsrc_Name[P_NAMELEN];	/* Name of the data source	*/
	char	dsrc_Where[P_NAMELEN];	/* Where it lives		*/
	DataSrcType dsrc_Type;		/* Type of this data source	*/
	int	dsrc_FFile;		/* First file ID		*/
} DataSrcInfo;



/*
 * The application-visible notion of a data file.
 */
typedef struct s_DataFileInfo
{
	char	dfi_Name[P_NAMELEN];	/* Name of this file		*/
	ZebTime	dfi_Begin;		/* Begin time			*/
	ZebTime dfi_End;		/* End time			*/
	int	dfi_NSample;		/* How many samples		*/
	PlatformId dfi_Plat;		/* It's platform		*/
	bool	dfi_Archived;		/* Has been archived?		*/
	int	dfi_Next;		/* Next file in chain		*/
} DataFileInfo;



/**************************************************************************
 * General data store application interface routines.
 */
int		ds_Initialize FP ((void));
PlatformId	ds_LookupPlatform FP ((char *));
char *		ds_PlatformName FP ((PlatformId));
int		ds_IsMobile FP ((PlatformId));
DataChunk *	ds_Fetch FP ((PlatformId, DataClass, ZebTime *, ZebTime *,
			FieldId *, int, dsDetail *, int));
DataChunk *	ds_FetchObs FP ((PlatformId, DataClass, ZebTime *, FieldId *,
			int, dsDetail *, int));
void		ds_DeleteData FP ((PlatformId, ZebTime *));
void		ds_RequestNotify FP ((PlatformId, int, void (*)()));
void		ds_CancelNotify FP ((void));
int		ds_DataTimes FP ((PlatformId, ZebTime *, int,TimeSpec,
			ZebTime *));
int		ds_GetObsSamples FP ((PlatformId, ZebTime *, ZebTime *,
			Location *, int));
int		ds_GetFields FP ((PlatformId, ZebTime *, int *, FieldId *));
int		ds_GetObsTimes FP ((PlatformId, ZebTime *, ZebTime *, int,
			char *));
bool		ds_GetRgridParams FP ((PlatformId, ZebTime *, Location *,
			RGrid *));
int		ds_GetNPlat FP ((void));
void		ds_GetPlatInfo FP ((PlatformId, PlatformInfo *));
int		ds_GetDataSource FP ((PlatformId, int, DataSrcInfo *));
void		ds_GetFileInfo FP ((int, DataFileInfo *));
void		ds_LockPlatform FP ((PlatformId));
void		ds_UnlockPlatform FP ((PlatformId));

# endif  /* _DATACHUNK_H_ */
