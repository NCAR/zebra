/* $Id: DataChunkP.h,v 1.6 1994-01-03 07:17:37 granger Exp $ */
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

/*
 * Methods for the raw data chunk -- always included first in any other
 * data chunk class.
 */
# define RAW_DC_METHODS \
	char		*dcm_Name;		/* Class name		*/ \
	DataClass	dcm_Parent;		/* Parent class		*/ \
	int		dcm_Depth;		/* Class depth, Raw = 0 */ \
	DataChunk	*(*dcm_Create)();	/* Create one of these	*/ \
	void		(*dcm_Destroy)();	/* Get rid of it again	*/ \
	void		(*dcm_AddData)();	/* Add data		*/ \
	void		(*dcm_Dump)();		/* Dump out structures  */


/*
 * Method inheritance is marked with this guy.
 */
# define InheritMethod	0

typedef struct _RawDCClass
{
	RAW_DC_METHODS
} RawDCClass;


extern RawDCClass *ClassTable[];


/*
 * Calculate the address greater than or equal to ptr which falls on a
 * boundary for the given size.  If someone knows that sizes 4 or greater
 * only need to go on 4-byte boundaries, then by all means change (size) to
 * ((size)>=4?(4):(size)).  This is used primarily (if not exclusively) by 
 * DCC_Transparent and DCC_MetData.
 */
#define ALIGN(ptr,size) (((unsigned long)(ptr)+((size)-1)) & (~((size)-1)))


/*
 * Short-circuit calls to _dc_ReqSubClassOf if class checking is turned off
 */
# define dc_ReqSubClassOf(class, superclass, op) \
((_CheckClass)?_dc_ReqSubClassOf((class), (superclass), (op)):TRUE)

/*
 * The method of superclass creation within a DataClass.  More direct than
 * calling dc_CreateDC(), but does no checking of valid class.
 */
# define DC_ClassCreate(class) ((*ClassTable[(class)]->dcm_Create) (class))

/*
 * Internally useful routines.
 */
bool		_dc_ReqSubClassOf FP ((DataClass, DataClass, char *));
DataClass	dc_GetSuperClass FP ((DataClass));

/*
 * ADE handling
 */
void		dc_AddADE FP ((DataChunk *, DataPtr, DataClass,
			int, int, int));
DataPtr		dc_FindADE FP ((DataChunk *, DataClass, int, int *));
void		dc_ChangeADE FP ((DataChunk *, DataPtr, DataClass,
			int, int));
void		dc_RemoveADE FP ((DataChunk *, DataClass, int));


/*
 * Attribute handling
 */
void		dca_AddAttrArray FP ((DataChunk *dc, DataClass class, int code,
				      char *key, DC_ElemType type, int nval,
				      void *values));
void *		dca_GetAttrArray FP ((DataChunk *dc, DataClass class, int code,
				      char *key, DC_ElemType *, int *nval));
int		dca_ProcAttrArrays FP((DataChunk *dc, DataClass class,
				       int code, char *pattern, 
				       int (*func)(/* char *key, void *vals, 
						      int nval, DC_ElemType, 
						      void *arg */),
				       void *arg));
void		dca_RemoveAttr FP ((DataChunk *dc, DataClass, int, char *));
void		dca_AddAttr FP ((DataChunk *, DataClass, int, char *, char *));
char *		dca_GetAttr FP ((DataChunk *, DataClass, int, char *));
int		dca_ProcAttrs FP ((DataChunk *, DataClass, int, char *,
			int (*) ()));
void *		dca_GetBlock FP ((DataChunk *, DataClass, int, int *));
void		dca_PutBlock FP ((DataChunk *, DataClass, int, void *, int));
char **		dca_GetAttrList FP ((DataChunk *, DataClass,
				     int code, char *pattern,
				     char **values[], int *natts));
int		dca_GetNAttrs FP ((DataChunk *, DataClass, int));

/*
 * Transparent class space optimizations
 */
void		dc_HintSampleOverhead FP((DataChunk *dc, int size));
int		dc_NSamplesGrowthHint FP((DataChunk *dc, int nnew));
