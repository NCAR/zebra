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
MAKE_RCSID ("$Id: dc_Scalar.c,v 1.8 1995-11-20 20:23:00 granger Exp $")

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




float
dc_GetScalar (dc, sample, field)
DataChunk *dc;
int sample;
FieldId field;
/*
 * Get a scalar value back from this DC.  Assumes the field is type DCT_Float.
 */
{
	float *ret;

	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "GetScalar"))
		return (0.0);
	if (dc_Type (dc, field) != DCT_Float)
	{
		msg_ELog (EF_PROBLEM, 
			  "cannot use GetScalar to get non-float data");
		return (0.0);
	}
	if (! (ret = (float *) dc_GetMData (dc, sample, field, NULL)))
		return (dc_GetBadval (dc));
	return (*ret);
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

