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
# include "DataChunkP.h"

RCSID ("$Id: dc_Scalar.c,v 1.10 1996-11-19 09:50:41 granger Exp $")


/*
 * Local routines.
 */
static DataChunk *sc_Create FP((DataChunk *));

/*
 * Scalar datachunks have no instance information.
 */
typedef struct _ScalarDataChunk
{
	RawDataChunkPart	rawpart;
	TranspDataChunkPart	transpart;
	MetDataChunkPart	metpart;
	ScalarDataChunkPart	scalarpart;
} ScalarDataChunk;

# define SUPERCLASS ((DataClassP)&MetDataMethods)


RawClass ScalarMethods =
{
	DCID_Scalar,		/* Class id */
	"Scalar",		/* Class name */
	SUPERCLASS,		/* Superclass			*/
	3,			/* Depth, Raw = 0		*/
	sc_Create,
	0,			/* No special destroy		*/
	0,			/* Add				*/
	0,			/* Dump				*/

	0,			/* No ADEs to serialize		*/
	0,

	sizeof (ScalarDataChunk)
};


DataClassP DCP_Scalar = (DataClassP)&ScalarMethods;



static DataChunk *
sc_Create (dc)
DataChunk *dc;
/*
 * We have nothing to initialize.
 */
{
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
	if (! dc_ReqSubClass (dc, DCP_Scalar, "SetScalarFields"))
		return;
/*
 * Our parent class knows enough to set the sample size hints, and we don't
 * have any per-sample overhead to add.  Scalar organization dictates only 1
 * element per field per sample.
 */
	dc_SetupFields (dc, nfield, fields);
	dc_SetUniformOrg (dc, 1);
}





DataPtr
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
	if (! dc_ReqSubClass (dc, DCP_Scalar, "AddScalar"))
		return (NULL);
	return (dc_AddMData (dc, t, field, dc_SizeOf (dc, field), 
			     sample, 1, value));
}





DataPtr
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
	if (! dc_ReqSubClass (dc, DCP_Scalar, "AddScalar"))
		return (NULL);
/*
 * We let MetData take care of hinting to our superclasses.
 */
	return (dc_AddMData(dc, t, field, dc_SizeOf(dc,field), 
			    begin, nsample, values));
}




void
dc_AddScalarMissing (dc, t, begin, nsample, field)
DataChunk *dc;
ZebTime *t;
int begin, nsample;
FieldId field;
/*
 * Add missing values to this data chunk.
 */
{
	if (! dc_ReqSubClass (dc, DCP_Scalar, "AddScalarMissing"))
		return;
	dc_FillMissing (dc, t, field, dc_SizeOf(dc,field), begin, nsample);
}




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
	int idx;

	if (! (ptr = (void *) dc_GetMData (dc, sample, field, NULL)))
		return (dc_GetBadval (dc));
	idx = dc_GetFieldIndex (dc, field);
	type = dc_IndexType (dc, idx);
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
	if (! dc_ReqSubClass (dc, DCP_Scalar, "GetScalarData"))
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

	dc_AssignElement (&ret, (void *)dc_GetMData (dc, sample, field, NULL),
			  dc_Type (dc, field));
	return (ret);
}

