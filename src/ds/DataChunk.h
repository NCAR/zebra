/* $Id: DataChunk.h,v 1.3 1991-12-16 17:44:12 corbet Exp $ */
/*
 * DataChunks -- the new "data object" format.
 *
 * Herein we define:
 *	- The raw data object format
 *	- Type codes
 *	- External access functions
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
# include "ds_fields.h"

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

	DCC_Text 	= 9,
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
	short		dca_Len;	/* Length of aux data		*/
	DataPtr	dca_Data;	/* Actual information		*/
	bool		dca_Free;	/* Free this one?		*/
} AuxDataEntry, *AuxDataChain;


/*
 * Raw data objects look like this:
 */
typedef struct _RawDataChunk
{
	DataClass	dc_Class;	/* The particular type		*/
	PlatformId	dc_Platform;	/* The platform of interest	*/
	DataPtr	dc_Data;	/* The actual data		*/
	int		dc_DataLen;	/* The length of the data field	*/
	AuxDataChain	dc_AuxData;	/* Subclass data		*/
} DataChunk, RawDataChunk;
	

/*
 * Class-specific defines.
 */
# define DC_MaxField	30	/* MetData -- max # of fields		*/

/*
 * Definitions of basic routines dealing with data chunks.
 */
bool		dc_IsSubClassOf FP((DataClass, DataClass));
DataChunk 	*ConvertDObj FP((DataObject *));
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
/*
 * Transparent class methods.
 */
int		dc_GetNSample FP((DataChunk *));
DataPtr		dc_GetSample FP((DataChunk *, int, int *));
void		dc_AddSample FP((DataChunk *, time *, DataPtr, int));
void		dc_SetPlat FP((DataChunk *, int, PlatformId));
PlatformId	dc_GetPlat FP((DataChunk *, int));
bool		dc_GetTime FP((DataChunk *, int, time *));
void		dc_AdjustSample FP((DataChunk *, int, int));
void		dc_SetStaticLoc FP((DataChunk *, Location *));
void		dc_SetLoc FP((DataChunk *, int, Location *));
void		dc_GetLoc FP((DataChunk *, int, Location *));
/*
 * Boundary class methods.
 */
void		dc_BndAdd FP((DataChunk *, time *, PlatformId, 
			Location *, int));
Location *	dc_BndGet FP((DataChunk *, int, int *));
/*
 * MetData class
 */
void		dc_SetupUniformFields FP((DataChunk *, int, int,
			FieldId *, int));
void		dc_SetupFields FP((DataChunk *, int, FieldId *));
float		dc_GetBadval FP((DataChunk *));
void		dc_SetBadval FP((DataChunk *, double));
int		dc_GetNField FP((DataChunk *));
void		dc_AddMData FP((DataChunk *, time *, FieldId, int, int,
			int, DataPtr));
DataPtr		dc_GetMData FP((DataChunk *, int, FieldId,   int *));
FieldId		*dc_GetFields FP((DataChunk *, int *));
/*
 * Scalar class.
 */
void		dc_SetScalarFields FP((DataChunk *, int, FieldId *));
void		dc_AddScalar FP((DataChunk *, time *, int, FieldId, float *));
void		dc_AddMultScalar FP((DataChunk *, time *, int, int,
			FieldId, float *));
float		dc_GetScalar FP((DataChunk *, int, FieldId));
/*
 * IRGrid class.
 */
void		dc_IRSetup FP((DataChunk *, int, PlatformId *, Location *,
			int, FieldId *));
void		dc_IRAddGrid FP((DataChunk *, time *, int, FieldId, float *));
int		dc_IRGetNPlatform FP((DataChunk *));
void		dc_IRGetPlatforms FP((DataChunk *, PlatformId *, Location *));
float 		*dc_IRGetGrid FP((DataChunk *, int, FieldId));
/*
 * RGrid.
 */
void		dc_RGSetup FP((DataChunk *, int, FieldId *));
void		dc_RGAddGrid FP((DataChunk *, int, FieldId, Location *,
			RGrid *, time *, float *, int));
float *		dc_RGGetGrid FP((DataChunk *, int, FieldId, Location *,
			RGrid *, int *));
/*
 * Image.
 */
void		dc_ImgSetup FP ((DataChunk *, int, FieldId *, ScaleInfo *));
void		dc_ImgAddImage FP ((DataChunk *, int, FieldId, Location *,
			RGrid *, time *, unsigned char *, int));
unsigned char *	dc_ImgGetImage FP ((DataChunk *, int, FieldId, Location *,
			RGrid *, int *, ScaleInfo *));
