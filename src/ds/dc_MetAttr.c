/*
 * The definition of the MetData data chunk class attribute methods.
 * These do not really depend on any private information in the class, so
 * we can consolidate them in a separate file.
 */
/*		Copyright (C) 1987-1996 by UCAR
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
# include <config.h>		/* For CFG_ symbols */
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"

RCSID ("$Id: dc_MetAttr.c,v 3.3 1996-11-27 02:24:12 granger Exp $")


/*
 * Forwards 
 */
static int dc_FieldKey FP ((DataChunk *dc, FieldId fid, int warn));



static int
dc_FieldKey (dc, fid, warn)
DataChunk *dc;
FieldId fid;
int warn;
/* 
 * Return the ADE sub-type to use for this field.  If the field does
 * not exist in the datachunk, returns less than zero.  Log warnings 
 * about missing fields if 'warn' is non-zero.
 */
{
	int st;

	if ((st = dc_GetFieldIndex (dc, fid)) >= 0)
	{
		st += 2000;
	}
	else if (warn)
	{
		msg_ELog (EF_PROBLEM, "%s: field %d (%s) not in datachunk",
			  "accessing attribute", fid, F_GetName (fid));
	}
	return (st);
}



void
dc_SetFieldAttrArray (dc, field, key, type, nval, vals)
DataChunk *dc;
FieldId field;
char *key;
DC_ElemType type;
int nval;
void *vals;
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "SetFieldAttrArray"))
		return;
	if ((st = dc_FieldKey (dc, field, 1)) >= 0)
		dca_AddAttrArray (dc, DCP_MetData, st, key, type, nval, vals);
}




void *
dc_GetFieldAttrArray (dc, field, key, type, nval)
DataChunk *dc;
FieldId field;
char *key;
DC_ElemType *type;
int *nval;
{
	void *values;
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFieldAttrArray"))
		return NULL;
	st = dc_FieldKey (dc, field, 1);
	if ((st >= 0) && (values = dca_GetAttrArray (dc, DCP_MetData, st,
						     key, type, nval)))
		return (values);
	return (dc_GetGlobalAttrArray (dc, key, type, nval));
}




int
dc_ProcFieldAttrArrays (dc, field, pattern, func, arg)
DataChunk *dc;
FieldId field;
char *pattern;
int (*func) (/* char *key, void *vals, int nval, DC_ElemType, void *arg */);
void *arg;
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "ProcFieldAttrArrays"))
		return (0);
	if ((st = dc_FieldKey (dc, field, 1)) >= 0)
		return (dca_ProcAttrArrays (dc, DCP_MetData, st, pattern, 
					    func, arg));
	return (-1);
}





void
dc_RemoveFieldAttr (dc, field, key)
DataChunk *dc;
FieldId field;
char *key;
/*
 * Remove a field attribute from this data chunk.
 */
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "RemoveFieldAttr"))
		return;
	if ((st = dc_FieldKey (dc, field, 0)) >= 0)
		dca_RemoveAttr (dc, DCP_MetData, st, key);
}




void
dc_SetFieldAttr (dc, fid, key, value)
DataChunk *dc;
FieldId fid;
char *key, *value;
/*
 * Store a field attribute into this data chunk.
 */
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "SetFieldAttr"))
		return;
	if ((st = dc_FieldKey (dc, fid, 1)) >= 0)
		dca_AddAttr (dc, DCP_MetData, st, key, value);
}




char *
dc_GetFieldAttr (dc, fid, key)
DataChunk *dc;
FieldId fid;
char *key;
/*
 * Look up a field attribute.
 */
{
	char *value;
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFieldAttr"))
		return(NULL);
	st = dc_FieldKey (dc, fid, 1);
	if ((st >= 0) && (value = dca_GetAttr (dc, DCP_MetData, st, key)))
		return (value);
	return (dc_GetGlobalAttr (dc, key));
}




void *
dc_GetFiAttrBlock (dc, fid, len)
DataChunk *dc;
FieldId fid;
int *len;
/*
 * Get the per-field attributes out as an opaque chunk.
 */
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFiAttrBlock"))
		return (NULL);
	st = dc_FieldKey (dc, fid, 1);
	return ((st >= 0) ? dca_GetBlock (dc, DCP_MetData, st, len) : NULL);
}




void
dc_SetFiAttrBlock (dc, fid, block, len)
DataChunk *dc;
FieldId fid;
void *block;
int len;
/*
 * Store a per-field attribute block back.
 */
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "SetFiAttrBlock"))
		return;
	if ((st = dc_FieldKey (dc, fid, 1)) >= 0)
		dca_PutBlock (dc, DCP_MetData, st, block, len);
}




int
dc_ProcessFieldAttrs (dc, fid, pattern, func)
DataChunk *dc;
FieldId fid;
char *pattern;
int (*func) ();
/*
 * Pass all of the attributes of this fid to the function
 */
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "ProcessFieldAttrs"))
		return(0);
	st = dc_FieldKey (dc, fid, 1);
	return ((st >= 0) ? dca_ProcAttrs (dc, DCP_MetData, st,
					   pattern, func) : NULL);
}




int
dc_GetNFieldAttrs (dc, fid)
DataChunk *dc;
FieldId fid;
/*
 * Return the number of field attributes in a datachunk.
 */
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "GetNFieldAttrs"))
		return (0);
	st = dc_FieldKey (dc, fid, 1);
	return((st >= 0) ? dca_GetNAttrs (dc, DCP_MetData, st) : 0);
}




char **
dc_GetFieldAttrList(dc, fid, pattern, values, natts)
DataChunk *dc;
FieldId fid;
char *pattern;
void **values[];
int *natts;
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFieldAttrList"))
		return (NULL);
	st = dc_FieldKey (dc, fid, 1);
	return ((st >= 0) ? dca_GetAttrList (dc, DCP_MetData, st,
					     pattern, values, natts) : NULL);
}



char **
dc_GetFieldAttrKeys (dc, fid, natts)
DataChunk *dc;
FieldId fid;
int *natts;
/*
 * Returns a list of keys for the field attributes in this data chunk.
 * Also puts into natt the number of attributes for this field.
 * The returned array of attribute keys is only valid until the next call
 * of any of the Get*AttrList or Get*AttrKeys functions.
 */
{
	int st;

	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFieldAttrKeys"))
		return (NULL);
	st = dc_FieldKey (dc, fid, 1);
	return ((st >= 0) ? dca_GetAttrList(dc, DCP_MetData, st,
					    NULL, NULL, natts) : NULL);
}
 


/* ====================================================================== */

/* 
 * Field-specific characteristics, such as bad values, are implemented
 * internally as private field attributes.  Though technically not private,
 * they are not accessible to the application without the ADE key.  Using
 * attributes allows us to extend the class structure with typed arrays of
 * any kind of information.
 */

/*
 * Defined names for standard attribute keys
 */
# define PK_BADVAL	"z-bad-value"
# define PK_MISSINGVAL	"z-missing-value"
# define PK_FILLVAL	"z-fill-value"

/*
 * The default bad value flag
 */
# define DefaultBadval 		CFG_DC_DEFAULT_BADVAL

static int dcp_FieldKey FP ((DataChunk *dc, FieldId fid));


static int
dcp_FieldKey (dc, fid)
DataChunk *dc;
FieldId fid;
/* 
 * Return the ADE sub-type to use for this field.  If the field does
 * not exist in the datachunk, returns less than zero.  
 */
{
	int st;

	if ((st = dc_GetFieldIndex (dc, fid)) >= 0)
	{
		st += 4000;
	}
	return (st);
}




static void
dcp_SetFieldAttrArray (dc, field, key, type, nval, values)
DataChunk *dc;
FieldId field;
char *key;
DC_ElemType type;
int nval;
void *values;
{
	int st;

	if ((st = dcp_FieldKey (dc, field)) >= 0)
		dca_AddAttrArray (dc, DCP_MetData, st,
				  key, type, nval, values);
}



static void *
dcp_GetFieldAttrArray (dc, field, key, type, nval)
DataChunk *dc;
FieldId field;
char *key;
DC_ElemType *type;
int *nval;
{
	void *values;
	int st;

	st = dcp_FieldKey (dc, field);
	if ((st >= 0) && (values = dca_GetAttrArray (dc, DCP_MetData, st,
						     key, type, nval)))
		return (values);
	return (NULL);
}



/* =======================================================================
 * Bad value handling
 * ======================================================================= */


double
dc_GetBadval (dc)
DataChunk *dc;
/*
 * Return the default bad value for this DC, which implies looking in
 * the global attributes.  Note that this returns a default value even if
 * no badvalue attribute has actually been set.
 */
{
	DC_ElemType type;
	void *ptr;
	
	if (! dc_ReqSubClass (dc, DCP_MetData, "GetBadval"))
		return (0);
	ptr = dcp_GetGlobalAttrArray (dc, PK_BADVAL, &type, NULL);
	if (! ptr)
		return ((double) DefaultBadval);
	switch (type)
	{
	   case DCT_Double:
		return (*(double *)ptr);
	   case DCT_Float:
		return ((double)*(float *)ptr);
	   case DCT_Integer:
		return ((double)*(int *)ptr);
	   case DCT_UnsignedInt:
		return ((double)*(unsigned int *)ptr);
	   case DCT_ShortInt:
		return ((double)*(short *)ptr);
	   case DCT_UnsignedShort:
		return ((double)*(unsigned short *)ptr);
	   default:
		return ((double) DefaultBadval);
	}
}




int
dc_SetBadval (dc, badval)
DataChunk *dc;
float badval;
/*
 * Store a floating point global bad value into this DC.  Return nonzero on
 * success, zero otherwise.
 */
{
	return (dc_SetGlobalBadval (dc, DCT_Float, &badval));
}



int
dc_SetFieldBadval (dc, fid, badval)
DataChunk *dc;
FieldId fid;
void *badval;
/*
 * Return non-zero on success, zero on failure.
 */
{
	DC_ElemType type;

	if (! dc_ReqSubClass (dc, DCP_MetData, "SetFieldBadval"))
		return (0);
	type = dc_Type (dc, fid);
	if (type == DCT_Unknown)
	{
		msg_ELog (EF_PROBLEM, "%s: %d",
			  "attempt to set bad value of unknown field", fid);
		return (0);
	}
	dcp_SetFieldAttrArray (dc, fid, PK_BADVAL, type, 1, badval);
	return (1);
}



void *
dc_GetFieldBadval (dc, fid)
DataChunk *dc;
FieldId fid;
/*
 * Look exclusively for this field's bad value, and return NULL if it
 * has not been set.
 */
{
	void *ptr;
	DC_ElemType ftype, type;

	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFieldBadval"))
		return (NULL);
	ftype = dc_Type (dc, fid);
	if (ftype == DCT_Unknown)
	{
		msg_ELog (EF_PROBLEM, "%s: %d",
			  "attempt to get bad value of unknown field", fid);
		return (NULL);
	}
	ptr = dcp_GetFieldAttrArray (dc, fid, PK_BADVAL, &type, NULL);
	if (ptr && (type != ftype))
	{
		msg_ELog (EF_PROBLEM, "badval type mismatch: field %d", fid);
		ptr = NULL;
	}
	return (ptr);
}



void *
dc_FindFieldBadval (dc, fid)
DataChunk *dc;
FieldId fid;
/*
 * Return a void pointer to this field's badvalue.  If the field's bad value
 * has not been set, check the global bad value.  If the global bad value
 * does not exist or is the wrong type, return NULL.  On any other error,
 * such as incorrect class, return NULL.
 */
{
	void *ptr;
	DC_ElemType ftype, type;

	if ((ptr = dc_GetFieldBadval (dc, fid)) == NULL)
	{
		/* 
		 * Try for global 
		 */
		ftype = dc_Type (dc, fid);
		ptr = dcp_GetGlobalAttrArray (dc, PK_BADVAL, &type, NULL);
		if (ptr && (type != ftype))
			ptr = NULL;
	}
	return (ptr);
}




float
dc_FindFloatBadval (dc, fid)
DataChunk *dc;
FieldId fid;
/*
 * Try to get the bad value for this field, but if non-existent or
 * non-float, return the floating point default.
 */
{
	float badval;
	void *att;
	DC_ElemType type;

	att = (float *) dc_FindFieldBadval (dc, fid);
	type = dc_Type (dc, fid);
	if (att && (type == DCT_Float))
		badval = *(float *)att;
	else if (att)
		dc_ConvertFloat (&badval, att, type);
	else
		badval = CFG_DC_DEFAULT_BADVAL;
	return (badval);
}




int
dc_SetGlobalBadval (dc, type, badval)
DataChunk *dc;
DC_ElemType type;
void *badval;
/*
 * Store a global bad value into this DC.  Return nonzero on success, zero
 * otherwise.
 */
{
	if (! dc_ReqSubClass (dc, DCP_MetData, "SetGlobalBadval"))
		return (0);
	dcp_SetGlobalAttrArray (dc, PK_BADVAL, type, 1, badval);
	return (1);
}




void *
dc_GetGlobalBadval (dc, type)
DataChunk *dc;
DC_ElemType *type;
/*
 * Return a void pointer to the global badvalue.  If type is non-NULL, returns
 * the type of the badvalue in *type.  Returns NULL on error.  If no global
 * bad value has been set, returns the default floating point bad value.
 */
{
	void *ptr;
	DC_ElemType t;

	if (! dc_ReqSubClass (dc, DCP_MetData, "GetGlobalBadval"))
		return (NULL);
	if ((ptr = dcp_GetGlobalAttrArray (dc, PK_BADVAL, &t, NULL)) && type)
		*type = t;
	return (ptr);
}


/* -------- End of bad value interface ----------------------------------- */



void
dc_DumpFieldAttributes (dc, fields, nfield)
DataChunk *dc;
FieldId *fields;
int nfield;
{
	int i;

	printf ("Field Attributes:\n");
	for (i = 0; i < nfield; i++)
	{
		if (dc_GetNFieldAttrs (dc, fields[i]) > 0)
		{
			printf (" %s:", F_GetName (fields[i]));
			dc_ProcFieldAttrArrays (dc, fields[i], NULL,
						dca_PrintAttrArray, ";");
			printf ("\n");
		}
	}
	printf ("Private field attributes:\n");
	for (i = 0; i < nfield; i++)
	{
		int st = dcp_FieldKey (dc, fields[i]);
		if (dca_GetNAttrs (dc, DCP_MetData, st) > 0)
		{
			printf (" %s:", F_GetName (fields[i]));
			dca_ProcAttrArrays (dc, DCP_MetData, st,
					    NULL, dca_PrintAttrArray, ";");
			printf ("\n");
		}
	}
}


