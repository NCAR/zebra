/* $Id: DataChunk.h,v 1.2 1991-12-04 23:44:38 corbet Exp $ */
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
# ifdef __STDC__
	bool		dc_IsSubClassOf (DataClass, DataClass);
/*
 * Basic data chunk methods.
 */
	DataChunk 	*dc_CreateDC (DataClass);
	void		dc_DestroyDC (DataChunk *);
	void		dc_DumpDC (DataChunk *);
	void 		Dc_RawAdd (DataChunk *, int);
/*
 * Transparent class methods.
 */
	int		dc_GetNSample (DataChunk *);
	DataPtr		dc_GetSample (DataChunk *, int, int *);
	void		dc_AddSample (DataChunk *, time *, DataPtr, int);
	void		dc_SetPlat (DataChunk *, int, PlatformId);
	PlatformId	dc_GetPlat (DataChunk *, int);
	bool		dc_GetTime (DataChunk *, int, time *);
	void		dc_AdjustSample (DataChunk *, int, int);
	void		dc_SetStaticLoc (DataChunk *, Location *);
	void		dc_SetLoc (DataChunk *, int, Location *);
	void		dc_GetLoc (DataChunk *, int, Location *);
/*
 * Boundary class methods.
 */
	void		dc_BndAdd (DataChunk *, time *, PlatformId, 
				Location *, int);
	Location *	dc_BndGet (DataChunk *, int, int *);
/*
 * MetData class
 */
	void		dc_SetupUniformFields (DataChunk *, int, int,
				FieldId *, int);
	void		dc_SetupFields (DataChunk *, int, FieldId *);
	float		dc_GetBadval (DataChunk *);
	void		dc_SetBadval (DataChunk *, double);
	int		dc_GetNField (DataChunk *);
	void		dc_AddMData (DataChunk *, time *, FieldId, int, int,
				int, DataPtr);
	DataPtr		dc_GetMData (DataChunk *, int, FieldId,   int *);
	FieldId		*dc_GetFields (DataChunk *, int *);
/*
 * Scalar class.
 */
	void		dc_SetScalarFields (DataChunk *, int, FieldId *);
	void		dc_AddScalar (DataChunk *, time *, int, FieldId,
				float *);
	void		dc_AddMultScalar (DataChunk *, time *, int, int,
				FieldId, float *);
	float		dc_GetScalar (DataChunk *, int, FieldId);
/*
 * IRGrid class.
 */
	void		dc_IRSetup (DataChunk *, int, PlatformId *, Location *,
				int, FieldId *);
	void		dc_IRAddGrid (DataChunk *, time *, int, FieldId,
				float *);
	int		dc_IRGetNPlatform (DataChunk *);
	void		dc_IRGetPlatforms (DataChunk *, PlatformId *,
				Location *);
	float 		*dc_IRGetGrid (DataChunk *, int, FieldId);
/*
 * RGrid.
 */
	void		dc_RGSetup (DataChunk *, int, FieldId *);
	void		dc_RGAddGrid (DataChunk *, int, FieldId, Location *,
				RGrid *, time *, float *, int);
	float *		dc_RGGetGrid (DataChunk *, int, FieldId, Location *,
				RGrid *, int *);
# else
	/* Later */
# endif
