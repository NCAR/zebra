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

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "ds_fields.h"
# include "DataChunk.h"
# include "DataChunkP.h"
MAKE_RCSID ("$Id: dc_Scalar.c,v 1.3 1991-12-27 21:24:17 corbet Exp $")

# define SUPERCLASS DCC_MetData

/*
 * Our class-specific AuxData structure types.
 */
# define ST_FLDINFO	1000


/*
 * Local routines.
 */
static DataChunk *dc_ScCreate FP((DataClass));

RawDCClass ScalarMethods =
{
	"Scalar",
	SUPERCLASS,		/* Superclass			*/
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
	dc = dc_CreateDC (SUPERCLASS);
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
	dc_SetupUniformFields (dc, 0, nfield, fields, sizeof (float));
}





void
dc_AddScalar (dc, t, sample, field, value)
DataChunk *dc;
time *t;
int sample;
FieldId field;
float *value;
/*
 * Add this scalar datum to the data chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "AddScalar"))
		return;
	dc_AddMData (dc, t, field, sizeof (float), sample, 1, value);
}





void
dc_AddMultScalar (dc, t, begin, nsample, field, values)
DataChunk *dc;
time *t;
int begin, nsample;
FieldId field;
float *values;
/*
 * Add a set of values to this data chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "AddScalar"))
		return;
	dc_AddMData (dc, t, field, sizeof (float), begin, nsample, values);
}




float
dc_GetScalar (dc, sample, field)
DataChunk *dc;
int sample;
FieldId field;
/*
 * Get a scalar value back from this DC.
 */
{
	float *ret;

	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Scalar, "AddScalar"))
		return (0.0);
	if (! (ret = (float *) dc_GetMData (dc, sample, field, NULL)))
		return (dc_GetBadval (dc));
	return (*ret);
}
