/* $Id: DataChunk.h,v 1.1 1991-11-16 01:18:54 corbet Exp $ */
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


/*
 * Here is the list of possible data chunk classes.  It is done this way
 * (as opposed, say, to putting class structure pointers directly in the
 * DC) for network portability reasons.
 *
 * The numbers are used to find class method structures, and should not
 * be changed.
 */
typedef enum _DataChunkClass
{
	DCC_None = 0,		/* No class at all -- Marxist data	*/
	DCC_Raw = 1,
	DCC_Transparent = 2,
	DCC_Boundary = 3,
} DataChunkClass;

/*
 * For the moment, data arrays are typeless.
 */
typedef void *DataPointer;

/*
 * Auxiliary data is linked up by these little guys:
 */
typedef struct _AuxDataEntry
{
	struct _AuxDataEntry	*dca_Next; /* Next in the chain		*/
	DataChunkClass	dca_Class;	/* Class which owns this entry	*/
	short		dca_SubType;	/* Class-specific code		*/
	short		dca_Len;	/* Length of aux data		*/
	DataPointer	dca_Data;	/* Actual information		*/
	bool		dca_Free;	/* Free this one?		*/
} AuxDataEntry, *AuxDataChain;


/*
 * Raw data objects look like this:
 */
typedef struct _RawDataChunk
{
	DataChunkClass	dc_Class;	/* The particular type		*/
	PlatformId	dc_Platform;	/* The platform of interest	*/
	DataPointer	dc_Data;	/* The actual data		*/
	int		dc_DataLen;	/* The length of the data field	*/
	AuxDataChain	dc_AuxData;	/* Subclass data		*/
} DataChunk, RawDataChunk;
	


/*
 * Definitions of basic routines dealing with data chunks.
 */
# ifdef __STDC__
	bool		dc_IsSubClassOf (DataChunkClass, DataChunkClass);
/*
 * Basic data chunk methods.
 */
	DataChunk 	*dc_CreateDC (DataChunkClass);
	void		dc_DestroyDC (DataChunk *);
	void		dc_DumpDC (DataChunk *);
/*
 * Transparent class methods.
 */
	int		dc_TrGetNSample (DataChunk *);
	DataPointer	dc_TrGetSample (DataChunk *, int, int *);
	void		dc_TrAddSample (DataChunk *, time *, DataPointer, int);
/*
 * Boundary class methods.
 */
	void		dc_BndAdd (DataChunk *, time *, PlatformId, 
				Location *, int);
	Location *	dc_BndGet (DataChunk *, int, int *);
# else
	/* Later */
# endif
