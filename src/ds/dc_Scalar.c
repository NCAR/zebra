/*
 * The scalar data chunk class.
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

# include <string.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "ds_fields.h"
# include "DataChunk.h"
# include "DataChunkP.h"

RCSID ("$Id: dc_Scalar.c,v 1.9 1996-09-19 03:56:40 granger Exp $")

# define SUPERCLASS DCC_MetData

/*
 * Our class-specific AuxData structure types.
 */
# define ST_FLDINFO	1


/*
 * Local routines.
 */
static DataChunk *dc_ScCreate FP((DataClass));

RawDCClass ScalarMethods =
{
	"Scalar",
	SUPERCLASS,		/* Superclass			*/
	3,			/* Depth, Raw = 0		*/
	dc_ScCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	0,			/* Dump				*/
};





static DataChunk *
dc_ScCreate (class)
DataClass class;
/*
 * Create a chunk of this class.
 */
{
	DataChunk *dc;
/*
 * The usual.  Make a superclass chunk and tweak it to look like us.  We don't
 * add any field info here, because we don't know it yet.
 */
	dc = DC_ClassCreate (SUPERCLASS);
	dc->dc_Class = class;
	return (dc);
}





void
dc_SetScalarFields (dc, nfield, fields)
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Set the given list as the field list for this scalar data chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "SetScalarFields"))
		return;
/*
 * Our parent class knows enough to set the sample size hints, and we don't
 * have any per-sample overhead to add.  Scalar organization dictates only 1
 * element per field per sample.
 */
	dc_SetupUniformOrg (dc, 0, nfield, fields, 1);
}





void
dc_AddScalar (dc, t, sample, field, value)
DataChunk *dc;
ZebTime *t;
int sample;
FieldId field;
void *value;
/*
 * Add this scalar datum to the data chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "AddScalar"))
		return;
	dc_AddMData (dc, t, field, dc_SizeOf (dc, field), sample, 1, value);
}





void
dc_AddMultScalar (dc, t, begin, nsample, field, values)
DataChunk *dc;
ZebTime *t;
int begin, nsample;
FieldId field;
void *values;
/*
 * Add a set of values to this data chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "AddScalar"))
		return;
/*
 * We let MetData take care of hinting to our superclasses.
 */
	dc_AddMData(dc, t, field, dc_SizeOf(dc,field), begin, nsample, values);
}


static void dc_ConvertFloat FP ((float *f, void *ptr, DC_ElemType type));


float
dc_GetScalar (dc, sample, field)
DataChunk *dc;
int sample;
FieldId field;
/*
 * Get a scalar value back from this DC.  If the field is type not DCT_Float,
 * do our best to convert it.
 */
{
	DC_ElemType type;
	void *ptr;
	float ret;

	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "GetScalar"))
		return (0.0);
	if (! (ptr = (void *) dc_GetMData (dc, sample, field, NULL)))
		return (dc_GetBadval (dc));
	type = dc_Type (dc, field);
	/*
	 * Try to avoid some overhead by casting the most common types
	 */
	if (type == DCT_Float)
	{
		return (*(float *)ptr);
	}
	else if (type == DCT_Double)
	{
		return ((float)*(double *)ptr);
	}
	/*
	 * We've been stuck with converting a non-float to float...
	 */
	dc_ConvertFloat (&ret, ptr, type);
	return (ret);
}




void *
dc_GetScalarData (dc, sample, field)
DataChunk *dc;
int sample;
FieldId field;
/*
 * Get a pointer to a scalar value in this DC.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "GetScalarData"))
		return (NULL);
	return ((void *) dc_GetMData (dc, sample, field, NULL));
}




DC_Element
dc_GetScalarElement (dc, sample, field)
DataChunk *dc;
int sample;
FieldId field;
/*
 * Get a pointer to a scalar value in this DC.
 */
{
	DC_Element ret;

	memset ((char *)&ret, sizeof(ret), 0);
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "GetScalarElement"))
		return (ret);
	dc_AssignElement (&ret, (void *)dc_GetMData (dc, sample, field, NULL),
			  dc_Type(dc, field));
	return (ret);
}



/*
 * Add this here for patch purposes.  In later releases it should appear
 * with the other routines in the element interface.  For convenience,
 * this could be changed to convert to long double and all other
 * conversions can just convert from long double to their desired target.
 */
static void
dc_ConvertFloat (f, ptr, type)
float *f;
void *ptr;
DC_ElemType type;
/*
 * De-reference a void pointer to type, cast it to float and store it.
 */
{
	switch (type)
	{
	   case DCT_Float:
		*f = *(float *)ptr;
		break;
	   case DCT_Double:
		*f = (float) *(double *)ptr;
		break;
	   case DCT_LongDouble:
		*f = (float) *(LongDouble *)ptr;
		break;
	   case DCT_Char:
		*f = (float) *(char *)ptr;
		break;
	   case DCT_UnsignedChar:
		*f = (float) *(unsigned char *)ptr;
		break;
	   case DCT_ShortInt:
		*f = (float) *(short *)ptr;
		break;
	   case DCT_UnsignedShort:
		*f = (float) *(unsigned short *)ptr;
		break;
	   case DCT_Integer:
		*f = (float) *(int *)ptr;
		break;
	   case DCT_UnsignedInt:
		*f = (float) *(unsigned int *)ptr;
		break;
	   case DCT_LongInt:
		*f = (float) *(long int *)ptr;
		break;
	   case DCT_UnsignedLong:
		*f = (float) *(unsigned long *)ptr;
		break;
#ifdef notdef	/* would this be atof() or byte->float? */
	   case DCT_String:
		*f = *(char **)ptr;
		break;
#endif
	   case DCT_Boolean:
		*f = (float) *(unsigned char *)ptr;
		break;
	   case DCT_ZebTime:
		*f = (float) (((ZebTime *)ptr)->zt_Sec);
		break;
#ifdef notdef
	   case DCT_VoidPointer:
		*f = (float) ptr;
	        break;
	   case DCT_Element:
		*e = *(DC_Element *)ptr;
		break;
#endif
	   default:
		*f = 0;
		break;
	}
}


