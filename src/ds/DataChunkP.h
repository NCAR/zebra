/* -*- mode: c++; c-basic-offset: 8; -*- */
/* $Id: DataChunkP.h,v 1.18 2002-11-14 05:49:55 granger Exp $ */
/*
 * Internal data chunk definitions.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

typedef struct _RawClass
{
	DataClassID	dcm_ClassId;		/* Class id		*/
	char		*dcm_Name;		/* Class name		*/
	DataClassP	dcm_Super;		/* Superclass pointer	*/
	int		dcm_Depth;		/* Class depth, Raw = 0 */
	DataChunk	*(*dcm_Create)();	/* Create one of these	*/
	void		(*dcm_Destroy)();	/* Get rid of it again	*/
	void		(*dcm_AddData)();	/* Add data		*/
	void		(*dcm_Dump)();		/* Dump out structures  */

	/* datachunks must be able to serialize and de-serialize (optimize) */
	void		(*dcm_Serialize)();
	void		(*dcm_Localize)();

	/* info for creating an instance of this class */
	int		dcm_Size;		/* size of instance struct */

} RawClass;

extern RawClass RawMethods;
extern RawClass TranspMethods;
extern RawClass BoundaryMethods;
extern RawClass MetDataMethods;
extern RawClass ScalarMethods;
extern RawClass IRGridMethods;
extern RawClass RGridMethods;
extern RawClass ImageMethods;
extern RawClass LocationMethods;
extern RawClass NSpaceMethods;
extern RawClass PolarMethods;

/*
 * Method inheritance is marked with this guy.
 */
#define InheritMethod	0

/* ====================================================================== */
/* Instance part structures for inheritance by subclass structures	  */
/* ====================================================================== */

/* ----------------------------------------------------------------------- */
/* Raw */

typedef RawDataChunk RawDataChunkPart;

/* ----------------------------------------------------------------------- */
/* Transparent */

/*
 * Structures for keeping track of sampling information in transparent
 * data objects.
 *
 * The following is used to keep track on one sample.  This can become
 * inefficient for large data chunks containing lots of small samples,
 * such as a day's worth of PAM data.  If that becomes a problem, this
 * will have to become smarter.
 */
typedef struct _TransSample
{
	ZebTime	ats_Time;	/* Time of this sample		*/
	int	ats_Offset;	/* Offset into data array	*/
	int	ats_Len;	/* Length of this sample	*/
} TransSample;


typedef struct _TransADE	/* our very own ADE placeholder */
{
	int	ta_len;
	DataPtr	ta_data;
} TransADE;

#define TR_NUM_SUBTYPES		5

/*
 * The transparent instance part which deals in samples:
 */
typedef struct _AuxTrans
{
	unsigned short at_NSample;	/* Number of samples in this DC	 */
	TransSample *at_Samples;	/* Dynamic samples array */
	unsigned short at_NSampAlloc;	/* Space allocated for this many */
	unsigned short at_HintNSample;	/* estimated # of samples to store */
	unsigned short at_HintSampSize;	/* estimate of a single sample's size*/
	unsigned short at_HintUseAvgs;	/* use average sample size as needed */
	unsigned short at_SampOverhead;	/* sample size overhead of subclasses*/
	unsigned short at_SampDataSize;	/* hint for size of data in a sample */
	unsigned short at_InOrder;	/* true if samples are contiguous */
	long at_NextOffset;		/* Next offset into buffered raw data,
					   equals dc_DataLen if no buffer */
	AltUnitType at_LocAltUnits;	/* Altitude units for Locations */
	Location at_SLoc;		/* Location for static platforms */
	unsigned short at_NSubSample;	/* Number of possible subdivisions */

	/* ADE shortcut array, indexed by the ST_ codes */
	TransADE at_ade[TR_NUM_SUBTYPES];

} AuxTrans;

typedef AuxTrans TranspDataChunkPart;

/*
 * The transparent instance structure.
 */
typedef struct _TranspDataChunk {
	RawDataChunkPart	rawpart;
	TranspDataChunkPart	transpart;
} TranspDataChunk;

/* ----------------------------------------------------------------------- */
/* Location */

typedef struct _LocationDataChunkPart
{
	int	lp_placeholder;

} LocationDataChunkPart;

/* ----------------------------------------------------------------------- */
/* Boundary */

typedef struct _BoundaryDataChunkPart
{
	int	bp_placeholder;

} BoundaryDataChunkPart;

/* ----------------------------------------------------------------------- */
/* MetData */

/* 
 * MD_HASH_SIZE is defined next to DC_MaxField, in DataStore.h, 
 * since it depends on DC_MaxField
 * 
 * HASH_FIELD_ID NOTE: FieldIds are now pointers, and assuming that they're 
 * aligned on 8-byte or 4-byte boundaries, we shift away those lower three 
 * bits that will always be zero (8-byte) or mostly be zero (4-byte).
 */
# define HASH_FIELD_ID(fid)	(((long)(fid)>>3)&(MD_HASH_SIZE-1))
# define HASH_SIZE		MD_HASH_SIZE


/*
 * The structure which describes our fields.
 */
typedef struct _FldInfo
{
	int		fi_NSample;		/* Shadows superclass 	 */
	int		fi_NField;
	FieldId		fi_Fields[DC_MaxField];
	DC_ElemType	fi_Types[DC_MaxField];	/* field element types	 */
	int		fi_Sizes[DC_MaxField];	/* field sizes, if fixed */
	int		fi_Offset[DC_MaxField];	/* offsets into sample	 */
	int		fi_HashIndex[HASH_SIZE];
	zbool		fi_BlockChanges;	/* block changes, esp types */
	zbool		fi_Uniform;		/* Uniform length fields */
	zbool		fi_UniformOrg;		/* Uniform elems/sample	 */
	int		fi_Size;		/* Size of uniform flds  */
	zbool		fi_FixedFields;		/* Field sizes fixed	 */
	zbool		fi_ReserveSpace;	/* Pre-arrange field space */
} FldInfo;

typedef FldInfo MetDataChunkPart;

typedef struct _MetDataChunk
{
	RawDataChunkPart	rawpart;
	TranspDataChunkPart	transpart;
	MetDataChunkPart	metpart;
} MetDataChunk;

#define FIP(dc) (&((MetDataChunk *)(dc))->metpart)

/*
 * Macros for fast access by subclasses
 */
#define dc_IndexType(dc,idx) (FIP(dc)->fi_Types[(idx)])
#define dc_FieldId(dc,idx) (FIP(dc)->fi_Fields[(idx)])

/* ----------------------------------------------------------------------- */
/* Scalar */

typedef struct _ScalarDataChunkPart
{
	int	       	placeholder;
} ScalarDataChunkPart;

/* ----------------------------------------------------------------------- */
/* NSpace */

typedef struct _NSpaceInfo
{
	int		ns_NField;	/* number of fields	*/
	int		ns_NDim;	/* no. of dimensions    */
	unsigned char	ns_Defined;	/* true once defn done	*/
	unsigned char	ns_HaveData;	/* have data, space set */
	unsigned char	ns_AllowRedefine;
					/* allow dim/var redefs?*/
	struct _NSpaceFldInfo	*ns_Fields;
	struct _NSpaceDimInfo	*ns_Dimns;
} NSpaceInfo;

typedef NSpaceInfo NSpaceDataChunkPart;

/* ----------------------------------------------------------------------- */
/* RGrid */

/*
 * The grid geometry array must grow with the number of samples, and hence
 * it must be dynamic.
 */
typedef struct _RGridDataChunkPart
{
	struct _GridGeometry *rg_gg;
	int		rg_Ngg;

} RGridDataChunkPart;

/* ----------------------------------------------------------------------- */
/* Image */

/*
 * The current approach tries to save space by allocating the scale info
 * according to how many fields are added.  But it would also be possible
 * expand the structure to hold DC_MaxField number of scales, then we
 * wouldn't have to worry about allocating and freeing.  We'll keep it
 * dynamic in case some day we want to store different scales for each
 * sample.
 */
typedef struct _ImageDataChunkPart
{
	ScaleInfo	*ip_scale;
	int		ip_Nscale;

} ImageDataChunkPart;

/* ----------------------------------------------------------------------- */
/* IRGrid */

/*
 * We have no idea how many subplatforms we'll hold, and there can be
 * alot of them, so we'll leave that information dynamic.
 */
typedef struct _IRGridDataChunkPart
{
	struct _PlatInfo *irg_pinfo;
	int		irg_nplat;

} IRGridDataChunkPart;


/* ======================================================================== */

/*
 * Calculate the address greater than or equal to ptr which falls on a
 * boundary for the given size.  If someone knows that sizes 4 or greater
 * only need to go on 4-byte boundaries, then by all means change (size) to
 * ((size)>=4?(4):(size)).  This is used primarily (if not exclusively) by 
 * DCC_Transparent and DCC_MetData.
 */
#define ALIGN(ptr,size) (((unsigned long)(ptr)+((size)-1)) & (~((size)-1)))


/*
 * Short-circuit calls to _dc_ReqSubClassOf if class checking is turned off.
 * If class checking has been configured to not be compiled, define it true.
 */
#if CFG_DC_CHECKCLASS
# define dc_ReqSubClass(dc, superclass, op) \
((_CheckClass)?_dc_ReqSubClass((dc), (superclass), (op)):TRUE)
#else
# define dc_ReqSubClass(dc, superclass, op) (1)
#endif


/*
 * Internally useful routines.
 */
zbool		_dc_ReqSubClass FP ((DataChunk *, DataClassP, char *op));

/*
 * Attribute handling
 */
void		dca_AddAttrArray FP ((DataChunk *dc, DataClassP, int code,
				      char *key, DC_ElemType type, int nval,
				      void *values));
void *		dca_GetAttrArray FP ((DataChunk *dc, DataClassP, int code,
				      char *key, DC_ElemType *, int *nval));
int		dca_ProcAttrArrays FP((DataChunk *dc, DataClassP,
				       int code, char *pattern, 
				       int (*func)(/* char *key, void *vals, 
						      int nval, DC_ElemType, 
						      void *arg */),
				       void *arg));
void		dca_RemoveAttr FP ((DataChunk *dc, DataClassP, int, char *));
void		dca_AddAttr FP((DataChunk *, DataClassP, int, char *, char *));
char *		dca_GetAttr FP ((DataChunk *, DataClassP, int, char *));
int		dca_ProcAttrs FP ((DataChunk *, DataClassP, int, char *,
				   int (*) ()));
void *		dca_GetBlock FP ((DataChunk *, DataClassP, int, int *));
void		dca_PutBlock FP ((DataChunk *, DataClassP, int, void *, int));
char **		dca_GetAttrList FP ((DataChunk *, DataClassP,
				     int code, char *pattern,
				     void **values[], int *natts));
int		dca_GetNAttrs FP ((DataChunk *, DataClassP, int));
int		dca_CmpAttrArrays FP ((DataChunk *dc1, DataChunk *dc2,
				       DataClassP class, int code, 
				       char *diffs, int len));
int		dca_PrintAttrArray FP ((char *key, void *value, int nval,
					DC_ElemType type, void *term));

void		dc_DumpAttrArrays FP ((DataChunk *dc));
void		dc_DumpFieldAttributes FP ((DataChunk *, FieldId *, int n));

void		dc_StoreFieldDefs FP ((DataChunk *dc));
FieldId		dc_RestoreFieldDef FP ((DataChunk *dc, int i));

/*
 * Transparent class space optimizations
 */
void		dc_HintSampleOverhead FP((DataChunk *dc, int size));
int		dc_NSamplesGrowthHint FP((DataChunk *dc, int nnew));

/*
 * Global private attributes interface
 */
void *		dcp_GetGlobalAttrArray FP((DataChunk *dc, char *key,
					   DC_ElemType *type, int *nval));
void		dcp_SetGlobalAttrArray FP((DataChunk *dc, char *key,
					   DC_ElemType type, int nval,
					   void *values));
