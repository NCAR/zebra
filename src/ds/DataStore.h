/*
 * $Id: DataStore.h,v 3.52 2002-11-14 05:49:55 granger Exp $
 *
 * Public data store definitions.
 */

# ifndef __zebra_DataStore_h_
# define __zebra_DataStore_h_

# include <stdio.h>
# include <config.h>		/* CFG_ parameter definitions 	*/
# include <defs.h>		/* RGrid, ScaleInfo, and Svalue	*/
# include "ds_fields.h"		/* FieldId and function protos	*/
# include "DataFiles.h"		/* DataFile definition		*/
# include "DataTypes.h"

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


# if __cplusplus
extern "C"
{
# endif

/*
 * The platform id format.
 */
typedef int PlatformId;
typedef int PlatClassId;
# define BadPlatform -1
# define BadClass    -1

/* ================================================================
 * RGrid and ScaleInfo type definitions have moved to defs.h in the
 * Zebra library.
 * ================================================================
 */


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
	UpdAppend		/* New data at the end		*/
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
 * time_offset variable in netCDF files, either NC_FLOAT, NC_DOUBLE, or
 * NC_LONG.  The SValue of the detail is ignored.  The appearance of both
 * in the same detail list is undefined.
 */
# define DD_NC_TIME_FLOAT	"dd_time_float" 
# define DD_NC_TIME_DOUBLE	"dd_time_double"
# define DD_NC_TIME_LONG	"dd_time_long"
/*
 * This detail only has effect on creation. It directs the netCDF driver to
 * create a single time variable 'time', whose units attribute contains the
 * base time rather than a separate variable.  The type of 'time' defaults
 * to double but can be changed with any of the details above.
 */
# define DD_NC_ONE_TIME		"dd_nc_one_time"

/*
 * Details for the ZNF format.  Hint to the file about the projected number
 * of samples to be stored in this file.  The hint is in the integer member
 * of the value union.
 */
# define DD_ZN_HINT_NSAMPLES	"dd_hint_nsamples"

/*
 * Reserve an amount of space, such as for attributes or special which will
 * be added later.  Intended for reserving space in a ZNF file before adding
 * samples which will be part of a block.  The block size is in the 
 * integer member of the value union.  This detail is only recognized when
 * the file is created.
 */
# define DD_ZN_RESERVE_BLOCK	"dd_reserve_block"

/*
 * Force new samples to be appended to the end of a file, so that they will
 * be contiguous with previous samples and will not overwrite any reserved
 * free blocks.  The detail need only be present; the value is ignored.
 */
# define DD_ZN_APPEND_SAMPLES	"dd_append_samples"

/*
 * Forecast offset time (for model data), in seconds
 */
# define DD_FORECAST_OFFSET	"dd_forecast_offset"

/*
 * Fixed dimension indices for multi-dimensional data.  TWO details are
 * possible, one pair for each fixed dimension index.  The first detail is
 * DD_FIX_DIMENSION, containing a pointer to the name of the dimension to
 * fix.  The detail immediately following is DD_FIX_INDEX, containing an
 * integer index to which the dimension should be fixed.  If there is no
 * succeeding DD_FIX_INDEX, then the index will be set to zero by default.
 * In the future, it might be nice to allow a second detail type, such as
 * DD_FIX_VALUE, which supplies a float value to match to the dimension's
 * coordinate variable.  Not yet, though.
 */
# define DD_FIX_DIMENSION	"dd_fix_dimension"
# define DD_FIX_INDEX		"dd_fix_index"

/* 
 * Choose the bad value to use when fetching data.  Pass the value in the
 * floating point member.
 */
# define DD_FETCH_BADVAL	"badval"

/*
 * Choose the altitude to slice from a 3-D grid.  Pass the altitude, in the
 * same units as in the file, in the floating point member of the symbol value.
 */
# define DD_FETCH_ALTITUDE	"altitude"

/*
 * Force a filename or base name.  The format extension is appended
 * to base names, while a filename is used as is.  The symbol value pointer
 * points to the string containing the name, and need only be valid for
 * the life of the call.  FILENAME takes precedence when both present.
 */
# define DD_FILE_NAME		"filename"
# define DD_FILE_BASE		"basename"
# define DD_FILE_EXT		"extension"

/*
 * More filename details.  Existence of DD_FOUR_YEAR puts 4-digit years
 * into the filename, while DD_TWO_YEAR uses the historical 2-digit years.
 * DD_TWO_YEAR is the default (for now...); use CFG_FULL_YEARS to switch
 * the default to DD_FOUR_YEAR.
 */
# define DD_TWO_YEAR		"two-digit-years"
# define DD_FOUR_YEAR		"four-digit-years"

/*
 * DataChunks -- the new "data object" format.
 *
 * Herein we define:
 *	- The raw data object format
 *	- Type codes
 *	- External access functions
 */

/*
 * 'Elements' are the atomic components of any data.  A field is a set of data
 * containing similarly-typed elements.  The organization of the elements is
 * specified by the class.  The type of a field's elements is handled by the
 * MetData class, since the MetData class implements fields.  In order to
 * support multi-typed fields, MetData subclasses must use the MetData class
 * interface to retrieve field type and size.  This interface should allow
 * subclasses to be completely independent of any particular field's type.  IF
 * EITHER OF THESE TYPEDEFS CHANGES, UPDATE THE NAMES AND SIZES ARRAY IN
 * DC_METDATA.C --- ALSO, TYPES SHOULD ONLY BE ADDED TO THE END, SO THAT THE
 * INTEGER VALUE OF THE EXISTING ENUMS DOES NOT CHANGE.
 */
typedef enum _DC_ElemType { 
	DCT_Unknown = 0,
	DCT_Float, 
	DCT_Double, 
	DCT_Char, 
	DCT_UnsignedChar,
	DCT_ShortInt,
	DCT_UnsignedShort,
	DCT_Integer,
	DCT_UnsignedInt,
	DCT_LongInt,
	DCT_UnsignedLong,
	DCT_String,
	DCT_Boolean,
	DCT_ZebraTime,
	DCT_VoidPointer,
	DCT_Element	/* the element union type */
} DC_ElemType;

#define DCT_ZebTime DCT_ZebraTime

typedef union _DC_Element {
	float 		dcv_float;
	double 		dcv_double;
	char 		dcv_char;
	unsigned char	dcv_uchar;
	short 		dcv_shortint;
	unsigned short 	dcv_ushort;
	int 		dcv_int;
	unsigned int 	dcv_uint;
	long 		dcv_longint;
	unsigned long 	dcv_ulong;
	char *		dcv_string;
	unsigned char	dcv_boolean;
	ZebraTime	dcv_zebtime;
	void *		dcv_pointer;
} DC_Element;

#define DC_ElemTypeMaxSize	(sizeof(union _DC_Element))

/*
 * Here is the list of possible data chunk classes.  It is done this way
 * (as opposed, say, to putting class structure pointers directly in the
 * DC) for network portability reasons.
 *
 * The numbers are used to find class method structures, and should not
 * be changed.
 *
 * BAD EXPERIENCE OF THE DAY.  If you add a new class here, don't overlook
 * the fact that you also need to add an entry to ClassTable in dc_All.c,
 * and a methods declaration in DataChunkP.h.
 */
typedef enum _DataClassID
{
	DCID_None 	= 0,	/* No class at all -- Marxist data	*/
	DCID_Raw	= 1,
	DCID_Transparent = 2,
	DCID_Boundary	= 3,
	DCID_MetData	= 4,
	DCID_Scalar 	= 5,
	DCID_IRGrid	= 6,
	DCID_RGrid	= 7,
	DCID_Image	= 8,
	DCID_Location	= 9,
	DCID_NSpace	= 10,
	DCID_Polar	= 11,
	DCID_OutOfBounds
} DataClassID;

#define NUM_DATACLASS	((int)DCID_OutOfBounds)
#define MAX_DCC_LEVELS	5	/* maximum depth of any class, 0 - 4 */

/*
 * The original reference to data classes is through the enumerated
 * type and the DCC_ symbols.  The DCP_* symbols are actually class
 * pointers which the latest interface supports so that not all
 * class modules need to be linked by applications which don't need
 * them.
 */
typedef DataClassID DataClass;
typedef struct _RawClass *DataClassP;

#define DCC_None DCID_None
#define DCC_Raw DCID_Raw
#define DCC_Transparent DCID_Transparent
#define DCC_Boundary DCID_Boundary
#define DCC_MetData DCID_MetData
#define DCC_Scalar DCID_Scalar
#define DCC_IRGrid DCID_IRGrid
#define DCC_RGrid DCID_RGrid
#define DCC_Image DCID_Image
#define DCC_Location DCID_Location
#define DCC_NSpace DCID_NSpace
# define DCC_Polar DCID_Polar

extern DataClassP DCP_None;
extern DataClassP DCP_Raw;
extern DataClassP DCP_Transparent;
extern DataClassP DCP_Boundary;
extern DataClassP DCP_MetData;
extern DataClassP DCP_Scalar;
extern DataClassP DCP_IRGrid;
extern DataClassP DCP_RGrid;
extern DataClassP DCP_Image;
extern DataClassP DCP_Location;
extern DataClassP DCP_NSpace;
extern DataClassP DCP_Polar;

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
	DataClassID	dca_ClassId;	/* Class which owns this entry	*/
	short		dca_SubType;	/* Class-specific code		*/
	unsigned short	dca_Len;	/* Length of aux data		*/
	DataPtr		dca_Data;	/* Actual information		*/
	zbool		dca_Free;	/* Free this one?		*/
} AuxDataEntry, *AuxDataChain;

/*
 * Because the hash function only uses the rightmost 3 bits of the subtype,
 * each class should try to vary as much as possible these bits between ADE
 * subtypes.
 */
#define ADE_HASH_SIZE		8
#define ADE_HASH_TYPE(type)  	((type)&7)
#define ADE_DCC_LEVELS		MAX_DCC_LEVELS

/*
 * Raw data objects look like this:
 */
typedef struct _RawDataChunk {
	DataClassID	dc_Class;	/* The class id			*/
	DataClassP	dc_ClassP;	/* Pointer to class structure	*/
	PlatformId	dc_Platform;	/* The platform of interest	*/
	DataPtr		dc_Data;	/* The actual data		*/
	int		dc_DataLen;	/* The length of the data field	*/
	AuxDataChain	dc_AuxData[ADE_DCC_LEVELS][ADE_HASH_SIZE];
	                                /* Subclass data		*/
} RawDataChunk;

typedef RawDataChunk DataChunk;
	
/*
 * Class-specific defines.
 *
 * HEY YOU!  READ THIS IF YOU CHANGE THE DEFINITION OF 'DC_MaxField':
 * ------------------------------------------------------------------------
 * The field index hash size (MD_HASH_SIZE) depends on DC_MaxField, so we 
 * define it here.  It must be the least power of 2 greater than (not equal 
 * to) DC_MaxField.
 */
# define MAXFIELD		CFG_DC_MAXFIELD	/* Max fields in data object*/
# define DC_MaxField		MAXFIELD    /* MetData -- max # of fields   */
# define DC_MaxDimension	CFG_DC_MAXDIMN /* Max number of dimensions  */
# define DC_MaxDimName		CFG_DIMNAME_LEN /* Max name length, incl \0 */

# define MD_HASH_SIZE		CFG_DC_MF_HASH_SIZE

/*
 * The sweep and beam info structures used by DCC_Polar.
 */
typedef struct _SweepInfo
{
	int	si_NBeam;	/* How many beams in this sweep	*/
	float	si_FixedAngle;	/* Fixed scan angle		*/
	ScanMode si_Mode;	/* Scan mode			*/
} SweepInfo;

typedef struct _PolarBeam
{
	FieldId pb_Fid;		/* Which field is here		*/
	ScanMode pb_ScanMode;	/* Scan mode			*/
	float pb_Azimuth;	
	float pb_Elevation;
	float pb_RotAngle;	/* Rotation angle		*/
	float pb_FixedAngle;	/* Non-rotating angle		*/
	int pb_NGates;
	float pb_R0;		/* Range to first gate		*/
	float pb_GateSpacing;	/* Spaceing	       		*/
	float *pb_Data;
} PolarBeam;

/*
 * Symbol to pass as data pointer to MetData subclasses to have the field
 * filled with fill values.
 */
extern DataPtr DC_FillValues;

/*
 * Definitions of basic routines dealing with data chunks.
 */
extern zbool 	_CheckClass;

zbool		dc_IsSubClassOf FP((DataClassID, DataClassID));
zbool		dc_IsSubClass FP((DataClassP classp, DataClassP superclass));
#if __cplusplus
inline	PlatformId dc_PlatformId (DataChunk *dc)
		{ return (dc->dc_Platform); }
inline	DataClassID dc_ClassId (DataChunk *dc)
		{ return (dc->dc_Class); }
#else
#define dc_PlatformId(dc) ((dc)->dc_Platform)
#define dc_ClassId(dc) ((dc)->dc_Class)
#endif
DataClassID	dc_SuperClass FP((DataClassID subclass));
DataClassP	dc_Super FP((DataClassP subclass));
void		dc_CheckClass FP((int check));
PlatformId	dc_SetPlatform FP((DataChunk *dc, PlatformId pid));
DataClassP	dc_ClassP FP ((DataClassID id));

/*
 * ADE handling
 */
void		dc_ClearADE FP ((DataChunk *dc));
DataPtr		dc_AddADE FP ((DataChunk *, DataPtr, DataClassP,
			       int, int, int));
DataPtr		dc_FindADE FP ((DataChunk *, DataClassP, int, int *));
void		dc_ChangeADE FP ((DataChunk *, DataPtr, DataClassP,
				  int, int));
void		dc_RemoveADE FP ((DataChunk *, DataClassP, int));
void		dc_DestroyADE FP ((DataChunk *dc));
void		dc_FreeAllADE FP ((DataChunk *dc));
void		dc_CopyADE FP ((DataChunk *target, DataChunk *dc));
void		dc_CopyClassADE FP ((DataChunk *dest, DataChunk *src,
				     DataClassP classp));
void		dc_StatsADE FP ((DataChunk *dc, int *count, int *len));

/*
 * Basic data chunk methods.
 */
DataChunk 	*dc_CreateDC FP((DataClass id));
DataChunk 	*dc_Create FP((DataClassP classp));
void		dc_DestroyDC FP((DataChunk *));
void		dc_Destroy FP((DataChunk *));
DataChunk	*dc_Copy FP((DataChunk *src));
DataChunk	*dc_Serialize FP((DataChunk *));
DataChunk	*dc_Localize FP((DataChunk *));
int		dc_SizeOfDC FP((DataChunk *));
void		dc_DumpDC FP((DataChunk *));
void		dc_Dump FP((DataChunk *));
void 		dc_RawAdd FP((DataChunk *, int));
#define		Dc_RawAdd dc_RawAdd
void		dc_ForceClosure FP ((void));
void		dc_SetGlobalAttr FP ((DataChunk *, char *, char *));
char 		*dc_GetGlobalAttr FP ((DataChunk *, char *));
void		dc_SetGlobalAttrArray FP ((DataChunk *dc, char *key,
					   DC_ElemType type, int nval, 
					   void *values));
void 		*dc_GetGlobalAttrArray FP ((DataChunk *dc, char *key,
					    DC_ElemType *type, int *nval));
int		dc_CmpGlobalAttr FP ((DataChunk *dc1, DataChunk *dc2,
				      char *diffs, int len));
int		dc_ProcessAttrArrays FP ((DataChunk *dc, char *pattern,
			  int (*func) (/* char *key, void *vals, int nval,
					  DC_ElemType, void *arg */),
					  void *arg));
void		dc_RemoveGlobalAttr FP ((DataChunk *dc, char *key));
int		dc_ProcessAttrs FP ((DataChunk *, char *, int (*) ()));
void		*dc_GetGlAttrBlock FP ((DataChunk *, int *));
void		dc_SetGlAttrBlock FP ((DataChunk *, void *, int));
int		dc_GetNGlobalAttrs FP ((DataChunk *));
char 		**dc_GetGlobalAttrList FP ((DataChunk *dc, char *pattern,
					    void **values[], int *natts));
char 		**dc_GetGlobalAttrKeys FP ((DataChunk *dc, int *natts));
/*
 * Transparent class methods.
 */
void		dc_HintNSamples FP((DataChunk *dc, int nsample, int decrease));
void		dc_HintSampleSize FP((DataChunk *dc, int size, int override));
void		dc_HintMoreSamples FP((DataChunk *dc, int nsample, int decr));
void		dc_HintUseAverages FP((DataChunk *dc, int use));
void		dc_AddMoreSamples FP((DataChunk *dc, int nsample, int size));
int		dc_GetNSample FP((DataChunk *));
DataPtr		dc_GetSample FP((DataChunk *, int, int *));
DataPtr		dc_AddSample FP((DataChunk *, ZebTime *, DataPtr, int));
DataPtr		dc_AddAlignedSample FP((DataChunk *dc, ZebTime *, DataPtr data,
					int len, int align_size));
int		dc_ReserveStaticSpace FP((DataChunk *dc, int len));
void		dc_SetPlat FP((DataChunk *, int, PlatformId));
PlatformId	dc_GetPlat FP((DataChunk *, int));
zbool		dc_SetTime FP((DataChunk *, int, ZebTime *));
zbool		dc_GetTime FP((DataChunk *, int, ZebTime *));
void		dc_SortSamples FP((DataChunk *dc));
void		dc_AdjustSample FP((DataChunk *, int, int));
void		dc_SetStaticLoc FP((DataChunk *, Location *));
Location *	dc_SetMLoc FP((DataChunk *, int begin, int nsamp, Location *));
Location *	dc_GetMLoc FP((DataChunk *dc, int sample, int nsample));
void		dc_SetLoc FP((DataChunk *, int, Location *));
void		dc_GetLoc FP((DataChunk *, int, Location *));
void		dc_SetSampleAttrArray FP ((DataChunk *dc, int samp, char *key,
					   DC_ElemType type, int nval, 
					   void *values));
void		*dc_GetSampleAttrArray FP ((DataChunk *dc, int samp, char *key,
					    DC_ElemType *type, int *nval));
void		dc_RemoveSampleAttr FP((DataChunk *dc, int sample, char *key));
int		dc_ProcSampleAttrArrays FP ((DataChunk *dc, int s, char *patt,
		     int (*func) (/* char *key, void *vals, int nval, 
				     DC_ElemType, void *arg */), void *arg));
void		dc_SetSampleAttr FP ((DataChunk *, int, char *, char *));
char		*dc_GetSampleAttr FP ((DataChunk *, int, char *));
void		*dc_GetSaAttrBlock FP ((DataChunk *, int, int *));
void		dc_SetSaAttrBlock FP ((DataChunk *, int, void *, int));
int		dc_GetNSampleAttrs FP ((DataChunk *, int sample));
char 		**dc_GetSampleAttrList FP ((DataChunk *dc, int sample,
			    char *pattern, void **values[], int *natts));
char 		**dc_GetSampleAttrKeys FP ((DataChunk *dc, int sample,
					    int *natts));
AltUnitType	dc_GetLocAltUnits FP ((DataChunk *dc));
void		dc_SetLocAltUnits FP ((DataChunk *dc, AltUnitType units));
zbool		dc_ContiguousSamples (DataChunk *dc);
/*
 * Transparent class sub-sample abstractions
 */
void		dc_SetValidTime FP((DataChunk *, int sample, ZebTime *valid));
zbool		dc_GetIssueTime FP((DataChunk *dc, int sample, ZebTime *when));
int 		dc_GetForecastOffset FP((DataChunk *, int samp, ZebTime *val));
void		dc_SetForecastOffset FP((DataChunk *dc, int sample, 
					 int forecast_index));
const ZebTime   *dc_ListForecastOffsets FP((DataChunk *dc, int *noffsets));
const ZebTime 	*dc_DefineForecastOffsets FP((DataChunk *dc, int noffsets,
					      ZebTime *offsets));
int		dc_GetSubSample FP((DataChunk *dc, int sample, ZebTime *when));
void		dc_SetSubSample FP((DataChunk *dc, int sample, int subsample));
const ZebTime 	*dc_ListSubSamples FP((DataChunk *dc, int *nsubs));
const ZebTime 	*dc_DefineSubSamples FP((DataChunk *dc, int nsubs, 
					 ZebTime *offsets));
int		dc_GetNSubSample FP((DataChunk *dc));
/*
 * Location class methods.
 */
void 		dc_LocAdd FP ((DataChunk *dc, ZebTime *t, Location *loc));
Location *	dc_LocAddMany FP ((DataChunk *dc, int nsamp, ZebTime *t,
				   Location *loc));
int		dc_LocGet FP ((DataChunk *dc, int sample, ZebTime *t, 
			       Location *loc));
/*
 * Boundary class methods.
 */
void		dc_BndAdd FP((DataChunk *, ZebTime *, PlatformId,
			Location *, int));
Location *	dc_BndGet FP((DataChunk *, int, int *));
/*
 * MetData class
 */
void		dc_SetupFields FP((DataChunk *, int, FieldId *));
int		dc_ClearFields FP((DataChunk *dc));
void		dc_AddFields FP((DataChunk *dc, int nfield, FieldId *fields));
void		dc_ChangeFld FP((DataChunk *dc, FieldId oldfid, 
				 FieldId newfid));
#ifdef CFG_DC_OLDFIELDS
void		dc_SetupUniformFields FP((DataChunk *, int, int,
					  FieldId *, int));
void		dc_SetupUniformOrg FP((DataChunk *dc, int nsamples, int nfield,
				       FieldId *fields, int nelems));
void		dc_FixFieldSizes FP((DataChunk *dc, int *sizes));
#endif /* CFG_DC_OLDFIELDS */
void		dc_SetUniformFieldSize FP((DataChunk *dc, int size));
void		dc_SetUniformOrg FP((DataChunk *dc, int nelem));
void		dc_SetFieldSizes FP((DataChunk *dc, int nfield, 
				     FieldId *fids, int *sizes));
void		dc_SetFieldTypes FP((DataChunk *dc, int nfld, 
				     FieldId *fids, DC_ElemType *types));
void		dc_SetType FP((DataChunk *dc, FieldId fid, DC_ElemType type));
DC_ElemType	dc_Type FP((DataChunk *dc, FieldId fid));
int		dc_SizeOf FP((DataChunk *dc, FieldId fid));
void		dc_BlockChanges FP ((DataChunk *dc));
double		dc_GetBadval FP((DataChunk *));
int		dc_SetBadval FP((DataChunk *, double));
void *		dc_GetGlobalBadval FP((DataChunk *dc, DC_ElemType *type));
int		dc_SetGlobalBadval FP((DataChunk *dc, DC_ElemType type,
				       void *badval));
void *		dc_GetFieldBadval FP((DataChunk *dc, FieldId fid));
void *		dc_FindFieldBadval FP((DataChunk *dc, FieldId fid));
float		dc_FindFloatBadval FP((DataChunk *dc, FieldId fid));
int		dc_SetFieldBadval FP((DataChunk *dc, FieldId fid, 
				      void *badval));

int		dc_GetFieldIndex FP((DataChunk *dc, FieldId field));
int		dc_GetNField FP((DataChunk *));
DataPtr		dc_AddMData FP((DataChunk *, ZebTime *, FieldId, int, int,
				int, DataPtr));
DataPtr		dc_GetMData FP((DataChunk *, int, FieldId, int *));
DataPtr *	dc_GetMVector FP((DataChunk *dc, int sample, int nfield,
				  FieldId *fields, DataPtr *fdata, int *len));
int		dc_SampleStride FP((DataChunk *dc, int *stride));
int		dc_FillMissing FP((DataChunk *dc, ZebTime *when, FieldId fid,
				   int size, int start, int nsamp));
int		dc_MetFillMissing FP((DataChunk *dc, FieldId fid,
				      unsigned char *at, int len));
FieldId		*dc_GetFields FP((DataChunk *, int *));
DC_ElemType	*dc_GetFieldTypes FP((DataChunk *dc, int *nf));
void		dc_SetFieldAttrArray FP((DataChunk *dc, FieldId field, char *,
					 DC_ElemType type, int nval, void *));
void		*dc_GetFieldAttrArray FP((DataChunk *dc, FieldId field,
					  char *, DC_ElemType *type, int *n));
int		dc_ProcFieldAttrArrays FP((DataChunk *dc, FieldId field,
					   char *pattern, 
			   int (*func) (/* char *key, void *vals, int nval, 
					   DC_ElemType, void *arg */),
					   void *arg));
void		dc_RemoveFieldAttr FP((DataChunk *dc, FieldId field, char *));
void		dc_SetFieldAttr FP ((DataChunk *, FieldId, char *, char *));
char		*dc_GetFieldAttr FP ((DataChunk *, FieldId, char *));
int		dc_ProcessFieldAttrs
	            FP ((DataChunk *, FieldId, char *, int (*)()));
void		*dc_GetFiAttrBlock FP ((DataChunk *, FieldId, int *));
void		dc_SetFiAttrBlock FP ((DataChunk *, FieldId, void *, int));
int		dc_GetNFieldAttrs FP ((DataChunk *, FieldId));
char		**dc_GetFieldAttrList FP ((DataChunk *, FieldId, char *,
					   void **values[], int *));
char 		**dc_GetFieldAttrKeys FP ((DataChunk *dc, FieldId fid,
					   int *natts));
/*
 * Element handling
 */
const char *	dc_TypeName FP((DC_ElemType type));
int		dc_SizeOfType FP((DC_ElemType type));
void		dc_AssignElement FP((DC_Element *, void *, DC_ElemType type));
void		dc_AssignValue FP((void *, DC_Element *, DC_ElemType type));
const char	*dc_ElemToString FP((DC_Element *, DC_ElemType type));
const char	*dc_ValueToString FP((void *ptr, DC_ElemType type));
const char	*dc_PrintElement FP((DC_Element *, DC_ElemType type));
const char	*dc_PrintValue FP((void *ptr, DC_ElemType type));
int		dc_CompareElement FP((DC_Element *e, DC_Element *f, 
				      DC_ElemType type));
int		dc_CompareValue FP((void *e, void *f, DC_ElemType type));
int		dc_ConvertDouble FP((double *d, void *ptr, DC_ElemType type));
int		dc_ConvertFloat FP((float *f, void *ptr, DC_ElemType type));
int		dc_DoubleToType FP((void *ptr, DC_ElemType type, double d));
void *		dc_DefaultBadval FP((DC_ElemType type));

/*
 * Scalar class.
 */
void		dc_SetScalarFields FP((DataChunk *, int, FieldId *));
DataPtr		dc_AddScalar FP((DataChunk *, ZebTime *, int, FieldId,
				 void *));
DataPtr		dc_AddMultScalar FP((DataChunk *, ZebTime *, int, int,
				     FieldId, void *));
void		dc_AddScalarMissing FP((DataChunk *dc, ZebTime *t, int begin,
					int nsample, FieldId field));
float		dc_GetScalar FP((DataChunk *, int, FieldId));
void 		*dc_GetScalarData FP((DataChunk *dc, int sample, FieldId fid));
DC_Element	dc_GetScalarElement FP((DataChunk *dc, int sample, FieldId));
/*
 * IRGrid class.
 */
void		dc_IRSetup FP((DataChunk *, int, PlatformId *, Location *,
			       int, FieldId *));
DataPtr		dc_IRAddGrid FP((DataChunk *, ZebTime *, int, FieldId,
				 void *));
DataPtr		dc_IRAddMultGrid FP((DataChunk *dc, ZebTime *t, int begin,
				     int nsample, FieldId field, void *data));
void		dc_IRAddMissing FP((DataChunk *dc, ZebTime *t, int begin,
				    int nsample, FieldId field));
void		dc_IRAddScalarDC FP((DataChunk *irgrid_dc, 
				     DataChunk *scalar_dc, int sample,
				     int nsample, int nfield, FieldId *fids));
int		dc_IRGetNPlatform FP((DataChunk *));
int		dc_IRGetPlatforms FP((DataChunk *, PlatformId *, Location *));
void 		*dc_IRGetGrid FP((DataChunk *, int, FieldId));
/*
 * RGrid.
 */
void		dc_RGSetup FP((DataChunk *, int, FieldId *));
int		dc_RGAddGrid FP((DataChunk *, int, FieldId, Location *,
			RGrid *, ZebTime *, void *, int));
int		dc_RGAddMissing FP((DataChunk *dc, int sample, FieldId field,
				    Location *origin, RGrid *rg, 
				    ZebTime *t, int len));
void *		dc_RGGetGrid FP((DataChunk *, int, FieldId, Location *,
				 RGrid *, int *));
int		dc_RGGeometry FP ((DataChunk *, int, Location *, RGrid *));
/*
 * Image.
 */
void		dc_ImgSetup FP ((DataChunk *, int, FieldId *, ScaleInfo *));
void		dc_ImgAddImage FP ((DataChunk *, int, FieldId, Location *,
				    RGrid *, ZebTime *, unsigned char *, int));
void		dc_ImgAddMissing FP ((DataChunk *dc, int sample,
				      FieldId field, Location *origin,
				      RGrid *rg, ZebTime *t, int len));
unsigned char *	dc_ImgGetImage FP ((DataChunk *, int, FieldId, Location *,
				    RGrid *, int *, ScaleInfo *));

/*
 * The polar class.
 */
typedef PolarBeam *(*dcpGetFunc) (DataChunk *, const DataFile*, int, int, 
				  FieldId, FieldId, int, float);
void		dcp_Setup FP ((DataChunk *, int, FieldId *, dcpGetFunc));
void		dcp_AddSweep FP ((DataChunk *, ZebTime *, Location *, int,
				  float, ScanMode, const DataFile*, int));
int		dcp_GetSweepInfo FP ((DataChunk *, int, SweepInfo *));
PolarBeam *	dcp_GetBeam FP ((DataChunk *, int, int, FieldId, FieldId,
				 int, float));
void		dcp_FreeBeam FP ((PolarBeam *beam));

/*-------------------------------------------------------------------------
 * The NSpace class
 *-------------------------------------------------------------------------
 * Numerical and Boolean parameters, except for the size of a dimension,
 * will be ints.  Dimension sizes and lengths of data arrays will always be
 * unsigned long.  The 'length' of a data array is the number of metdata
 * elements, e.g. floating values.  This will be true for both definition
 * and information retrieval.  For retrieval, strings will be returned as
 * pointers to memory inside the datachunk.  The memory pointed to will be
 * valid for the life of the datachunk, but it SHOULD NOT be freed or
 * modified.  Any parameters being passed by reference to hold return
 * values can be passed as NULL, in which case nothing will be returned for
 * that parameter.
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
 * dc_SetBadval (...), 
 * dc_GetBadval (...)	-- Bad values associated with fields (aka variables);
 *			   the bad value can only be set once definition is
 *			   complete.
 * dc_SetType (...)	-- Change the element type for a field from the
 *			   default of float.  Must be called AFTER the NSpace
 *			   definition is complete and BEFORE any data, static
 *			   or dynamic, is added.
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
void dc_NSAllowRedefine FP((DataChunk *dc, int allow));
int dc_NSRedefineIsAllowed FP((DataChunk *dc));

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
DataPtr dc_NSAddSample FP((DataChunk *dc, ZebTime *when, int sample, 
			   FieldId field, void *values));
DataPtr dc_NSAddMultSamples FP((DataChunk *dc, ZebTime *when, int begin,
				int nsample, FieldId field, void *values));
DataPtr dc_NSAddStatic FP((DataChunk *dc, FieldId field, void *values));
void dc_NSAddMissing FP((DataChunk *dc, ZebTime *when, int begin, 
			 int nsample, FieldId field));
void dc_NSFillStatic FP((DataChunk *dc, FieldId field));
/*
 * NSpace Data retrieval
 */
void *dc_NSGetSample FP((DataChunk *dc, int sample, FieldId field, 
			 unsigned long *size));
void *dc_NSGetStatic FP((DataChunk *dc, FieldId field, unsigned long *size));

/*
 * NSpace fixed dimension detail handling
 */
int dc_NSFixedDimension FP((dsDetail *details, int ndetail,
			    char *name, int *dindex));
void dc_NSFixedDetails FP((char *list, dsDetail *details, int *ndetail));

/*
 * Generic datachunk processing interface
 */
int dc_ProcessDetails FP ((DataChunk *dc, dsDetail *details, int ndetail));

/**************************************************************************
 * Other structures used at the data store application interface level.
 */

/*
 * Info about one of the daemon's data sources
 */
typedef struct s_SourceInfo
{
    int src_Id;				/* id			*/
    char src_Name[CFG_PLATNAME_LEN];	/* source name		*/
    char src_Dir[CFG_FILEPATH_LEN];	/* base directory	*/
    zbool src_ReadOnly;			/* read-only source?	*/
} SourceInfo;


/*
 * Types of directory inheritance for classes
 */
typedef enum { 
	InheritNone = 0, InheritDefault = 0, 
	InheritAppend, InheritCopy 
} InheritDirFlag;

/*
 * Types of directory instantiation for classes
 */
typedef enum { 
	InstanceDefault = 0, InstanceRoot = 0, 
	InstanceCopyClass, InstanceSubdirClass, 
	InstanceCopyParent, InstanceSubdirParent
} InstanceDirFlag;


/**************************************************************************
 * General data store application interface routines.
 */
int		ds_Initialize FP ((void));
int		ds_Standalone FP ((void));
int		ds_IsStandalone FP ((void));
const char *	ds_ProtoVersion FP ((void));
PlatformId *	ds_SearchPlatforms FP ((char *regexp, int *nplat, 
					int alphabet, int subs));
PlatformId	ds_LookupParent FP ((PlatformId pid));
PlatformId *	ds_LookupSubplatforms FP ((PlatformId parent, int *nsubplat));
zbool		ds_Store FP ((DataChunk *dc, int newfile, 
			      dsDetail *details, int ndetail));
zbool		ds_StoreBlocks FP ((DataChunk *dc, int newfile,
				    dsDetail *details, int ndetail));
DataChunk *	ds_Fetch FP ((PlatformId, DataClass, ZebTime *, ZebTime *,
			      FieldId *, int, dsDetail *, int));
DataChunk *	ds_FetchObs FP ((PlatformId, DataClass, ZebTime *, FieldId *,
				 int, dsDetail *, int));
void		ds_DeleteData FP ((PlatformId, ZebTime *));
void		ds_DeleteObs FP ((PlatformId platform, ZebTime *zaptime));
zbool		ds_ScanFile (int srcid, PlatformId pid, char *fname);
# ifdef notdef	/* no longer supported */
zbool		ds_InsertFile FP ((PlatformId platform, char *filename,
				   ZebTime *begin, ZebTime *end,
				   int nsample, int local));
# endif
void		ds_RequestNotify FP ((PlatformId, int, void (*)()));
void		ds_CancelNotify FP ((void));
void		ds_MarkArchived FP ((int dfi));
void		ds_SnarfCopies FP ((void (*handler)()));

/*
 * Special source ids that can be used with the file-finding functions below
 */
# define SRC_ALL	(-1)	/* all sources */
# define SRC_DEFAULT	(-2)	/* default readable source */
# define SRC_DEFAULT_W	(-4)	/* default writable source */

/*
 * File finding
 */
const DataFile*	ds_FindDFBefore (int srcid, PlatformId pid, 
				 const ZebTime *when);
const DataFile*	ds_FindDFAfter (int srcid, PlatformId pid, 
				const ZebTime *when);
const DataFile*	ds_FirstFile (int srcid, PlatformId pid);
const DataFile*	ds_LastFile (int srcid, PlatformId pid);
const DataFile*	ds_PrevFile (const DataFile *df);
const DataFile*	ds_NextFile (const DataFile *df);
/* 
 * The following two functions are deprecated.  Switch to ds_FindDFBefore()
 * and ds_FindDFAfter().
 */
const DataFile*	ds_FindBefore (PlatformId pid, const ZebTime *when);
const DataFile*	ds_FindAfter (PlatformId pid, const ZebTime *when);


int		ds_DataTimes FP ((PlatformId, ZebTime *, int, TimeSpec,
				  ZebTime *));
int 		ds_AttrTimes FP ((PlatformId pid, ZebTime *when, int n,
				  TimeSpec which, char *key, char *value,
				  ZebTime *rettimes));
int		ds_GetObsSamples (PlatformId pid, const ZebTime *when, 
				  ZebTime *times, Location *locs, int max);
int		ds_GetFields (PlatformId pid, const ZebTime *t, int *nfld, 
			      FieldId *flist);
int		ds_GetObsTimes FP ((PlatformId, ZebTime *, ZebTime *, int,
				    char *));
zbool		ds_GetAlts FP((PlatformId pid, FieldId fid, ZebTime *when,
			       int offset, float *alts, int *nalts, 
			       AltUnitType *altunits));
zbool		ds_GetForecastTimes FP ((PlatformId, ZebTime *, int *, int *));
int		ds_GetNPlat FP ((void));
void 		ds_ForceRescan FP ((PlatformId platform, int all));
void		ds_ForceClosure FP ((void));

/* -------------------------------------------------------------------- */

/* protect for programs which don't need message.h */
#if defined(_ZEBRA_MESSAGE_H_) || defined(_ZEB_MESSAGE_H_)
int             ds_DSMessage FP ((struct message *));
#endif

/* -- the details interface in Details.c and dc_Process.c -- */
void		ds_SetDefaultDetails FP ((dsDetail *details, int ndetail));
int		ds_GetDetail FP ((char *key, dsDetail *, int ndet, SValue *v));
int		ds_GetIntDetail FP ((char *key, dsDetail *, int ndet, int *i));
int		ds_GetFloatDetail FP ((char *k, dsDetail *, int n, float *f));
char *		ds_GetStringDetail FP ((char *k, dsDetail *, int n));

int		ds_SetDetail FP ((char *k, dsDetail *, int n));
int		ds_SetIntDetail FP ((char *k, int i, dsDetail *, int n));
int		ds_SetFloatDetail FP ((char *k, double f, dsDetail *, int n));
int		ds_SetStringDetail FP ((char *k, char *s, dsDetail *, int n));

/* --------------------------------------------------------------------
 * Class and platform definition interface
 */

/*
 * Opaque pointer to platform class structure to prevent public access.
 */
typedef struct ds_PlatformClass *PlatClassRef;

/*
 * Basic platform and class query interface.
 */
PlatformId 	ds_LookupPlatform FP ((const char *name));
PlatClassId	ds_LookupClass FP ((const char *name));
PlatClassId	ds_PlatformClass FP ((PlatformId pid));
const char *	ds_PlatformName FP ((PlatformId));
const char *	ds_ClassName FP ((PlatClassId));
DataOrganization ds_PlatformDataOrg FP ((PlatformId pid));
int		ds_IsMobile FP ((PlatformId));
int		ds_IsModelPlatform FP ((PlatformId));
int		ds_MaxSamples FP ((PlatformId id));
const FieldId * ds_PlatformClassFields (PlatformId pid, int *nfield);

/*
 * Manipulate class references
 */
PlatClassRef	ds_NewSubClass FP ((const char *name, PlatClassId sid));
PlatClassRef	ds_NewClass FP ((const char *name));
PlatClassRef	ds_NewNamedClass (const char *name, const char *super);
void		ds_AddClassSubplat FP ((const PlatClassRef pc, 
					PlatClassId subid, 
					const char *subname));
void            ds_AddClassDerivation (PlatClassRef pc, char* dtext);
void            ds_SetDerivations (PlatClassRef pc, char* dtext);
void            ds_AddClassField (PlatClassRef pc, FieldId fid);
void		ds_EraseClassSubplat (PlatClassRef pc);
void		ds_DestroyClass FP ((PlatClassRef pc));
int		ds_ShowPlatformClass FP ((FILE *fp, PlatClassId cid));

/*
 * Modify class references
 */
void		ds_AssignClass FP ((PlatClassRef pc, DataOrganization org,
				    FileType ftype, int mobile));
void		ds_SetOrg FP ((PlatClassRef cd, DataOrganization org));
void		ds_SetFiletype FP ((PlatClassRef cd, FileType ft));
void		ds_SetInheritDir FP ((PlatClassRef cd, InheritDirFlag id));
void		ds_SetInstanceDir FP ((PlatClassRef cd, InstanceDirFlag id));
void		ds_SetMobile FP ((PlatClassRef cd, int mobile));
void		ds_SetComposite FP ((PlatClassRef cd, int composite));
void		ds_SetModel FP ((PlatClassRef cd, int flag));
void		ds_SetSplitSeconds (PlatClassRef cd, unsigned int secs);
void		ds_SetDaysplit FP ((PlatClassRef cd, int split));
void		ds_SetAbstract FP ((PlatClassRef cd));
void		ds_SetVirtual FP ((PlatClassRef cd));
void		ds_SetMaxSample FP ((PlatClassRef cd, int maxsamp));
void		ds_SetComment FP ((PlatClassRef cd, const char *comment));
void            ds_SetDirectory (PlatClassRef cd, const char *dir);

/*
 * Source info
 */
int		ds_GetSourceInfo (int srcid, SourceInfo *si);
int		ds_GetPlatDir (int srcid, PlatformId pid, char *dir);

/*
 * Define and instantiate class references
 */
PlatClassId	ds_DefineClass FP ((PlatClassRef pc));
PlatformId	ds_DefinePlatform FP ((PlatClassId cid, const char *name));
PlatformId	ds_DefineSubPlatform FP ((PlatClassId cid, const char *name,
					  PlatformId parent));

/*
 * String forms of enumerated types.  Defined in DataTypes.c so they're
 * available to both client and daemon.
 */
const char *	ds_InstanceDirFlagName FP ((InstanceDirFlag id));
const char *	ds_InheritDirFlagName FP ((InheritDirFlag id));
const char *	ds_FTypeName FP ((FileType ft));
const char *	ds_OrgName FP ((DataOrganization org));


/*
 * Field derivation interface
 */

typedef void* DerivMethod;

void		ds_DerivInit (void);
DerivMethod	ds_GetDerivation (PlatformId pid, FieldId wantid, 
				  FieldId raw_ids[], int nraw);
FieldId*	ds_DerivNeededFields (DerivMethod der, int *nneed);
int		ds_DerivIsAlias (DerivMethod der);
void		ds_DoDerivation (DerivMethod der, FieldId fids[], int nflds, 
				 int ndata, double* dptrs[], double results[], 
				 double badval);
int		ds_IsDerivable (PlatformId pid, FieldId wantid, 
				FieldId* raw_ids, int nraw);
void		ds_DestroyDeriv (DerivMethod der);

# if __cplusplus
} // end of extern "C"
# endif

# endif	/* !__zebra_DataStore_h_ */
