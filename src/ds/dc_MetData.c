/*
 * The definition of the MetData data chunk class.  This class adds the
 * concept of fields and stuff in general interpretable as meteorological
 * data.
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
# include <memory.h>

# include <defs.h>
# include <config.h>		/* For CFG_ symbols */
# include <message.h>
# include "DataStore.h"
# include "ds_fields.h"
# include "DataChunk.h"
# include "DataChunkP.h"
MAKE_RCSID ("$Id: dc_MetData.c,v 3.14 1995-06-09 16:49:15 granger Exp $")

# define SUPERCLASS DCC_Transparent

# ifndef CFG_NO_BADVALUES
# define SUPPORT_BADVALUES
# endif

/*
 * The default bad value flag, usually -99999.9
 */
# define DefaultBadval 		CFG_DC_DEFAULT_BADVAL

# define HASH_FIELD_ID(fid)	((fid)&(MD_HASH_SIZE-1))
# define HASH_SIZE		MD_HASH_SIZE
/* 
 * MD_HASH_SIZE is defined next to DC_MaxField, in DataStore.h, 
 * since it depends on DC_MaxField
 */

const char *DC_ElemTypeNames[] =
{
	"unknown",
	"float",
	"double",
	"long double",
	"char",
	"unsigned char",
	"short int",
	"unsigned short",
	"int",
	"unsigned int",
	"long int",
	"unsigned long",
	"string",
	"Boolean",
	"ZebTime",
	"void *"
};

const int DC_ElemTypeSizes[] = 
{
	0,
	sizeof(float),
	sizeof(double),
	sizeof(LongDouble),
	sizeof(char),
	sizeof(unsigned char),
	sizeof(short int),
	sizeof(unsigned short),
	sizeof(int),
	sizeof(unsigned int),
	sizeof(long int),
	sizeof(unsigned long),
	sizeof(char *),
	sizeof(unsigned char),
	sizeof(ZebTime),
	sizeof(void *)
};

const int DC_NumTypes = 
	sizeof (DC_ElemTypeSizes) / sizeof (DC_ElemTypeSizes[0]);

/*
 * If we have non-uniform, non-fixed, non-pre-arranged fields, then the 
 * beginning piece of every sample contains a list of the following:
 */
typedef struct _FieldTOC
{
	int	ft_Offset;		/* Offset of this field		*/
	int	ft_Len;			/* Length of this field		*/
} FieldTOC;


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
	FieldId		fi_HashIndex[HASH_SIZE];
	bool		fi_BlockChanges;	/* block changes, esp types */
	bool		fi_Uniform;		/* Uniform length fields */
	bool		fi_UniformOrg;		/* Uniform elems/sample	 */
	int		fi_Size;		/* Size of uniform flds  */
	bool		fi_FixedFields;		/* Field sizes fixed	 */
	bool		fi_ReserveSpace;	/* Pre-arrange field space */
	float		fi_Badval;		/* Bad value flag	 */
} FldInfo;


/*
 * Our class-specific AuxData structure types.
 */
# define ST_FLDINFO		1000
# define ST_FIELDATTR(x)	(2000+(x))


/*
 * Local routines.
 */
static DataChunk *dc_MDCreate FP((DataClass));
static int	dc_GetIndex FP((FldInfo *, FieldId));
static FldInfo *dc_BuildFieldInfo FP((DataChunk *dc, int nfld, FieldId *fids));
static void	dc_BuildIndexHash FP((FldInfo *finfo));
static inline void dc_MDSetType FP((DataChunk *dc, FldInfo *finfo, FieldId fid,
				    DC_ElemType type));
static void	dc_MDOptimize FP((DataChunk *dc, FldInfo *finfo));
static void	dc_AddUniform FP((DataChunk *dc, FldInfo *finfo, int findex,
				  ZebTime *t, int size, int start, int nsamp,
				  DataPtr data));
static void	dc_AddNonUniform FP((DataChunk *, FldInfo *, int, ZebTime *,
			int, int, int, DataPtr));
static void	dc_AddFixedField FP((DataChunk *dc, FldInfo *finfo, 
				     int findex, ZebTime *t, int size, 
				     int start, int nsamp, DataPtr data));
static void	dc_DumpMD FP((DataChunk *));
static int	dc_PrintFiAttr FP((char *key, void *value, int nval,
				   DC_ElemType type, void *arg));
static FldInfo *dc_ChangeInfo FP((DataChunk *dc, char *method));



RawDCClass MetDataMethods =
{
	"MetData",
	SUPERCLASS,		/* Superclass			*/
	2,			/* Depth, Raw = 0		*/
	dc_MDCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	dc_DumpMD,		/* Dump				*/
};




/* ARGSUSED */
static DataChunk *
dc_MDCreate (class)
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
	dc->dc_Class = DCC_MetData;
	return (dc);
}




void
dc_SetupFields (dc, nfield, fields)
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Set up this DC to take non-uniform fields.
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "SetupFields"))
		return;

	finfo = dc_BuildFieldInfo (dc, nfield, fields);
}



void
dc_SetupUniformFields (dc, nsamples, nfield, fields, size)
DataChunk *dc;
int nsamples, nfield;
FieldId *fields;
int size;
/*
 * Get this data chunk set up to have uniform-length fields.
 * Entry:
 *	DC	is a new data chunk, which is a subclass of DCC_MetData
 *	NSAMPLES is the best guess at the number of samples which this
 *		data chunk will have.  It is not an upper limit.
 *	NFIELD	is the number of fields this data chunk will have.
 *	FIELDS	is the list of ID's for the fields in this DC.
 *	SIZE	is the uniform size that each field will have.
 * Exit:
 *	The data chunk has been configured for this mode of operation.
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (!dc_ReqSubClassOf(dc->dc_Class, DCC_MetData, "SetupUniformFields"))
		return;

	finfo = dc_BuildFieldInfo (dc, nfield, fields);
	if (finfo)
	{
		finfo->fi_Uniform = TRUE;
		finfo->fi_Size = size;
	}

	dc_HintNSamples (dc, nsamples, TRUE);
}




void
dc_SetupUniformOrg (dc, nsamples, nfield, fields, nelems)
DataChunk *dc;
int nsamples, nfield;
FieldId *fields;
int nelems;
/*
 * Get this data chunk set up to have uniform-length fields.
 * Entry:
 *	DC	is a new data chunk, which is a subclass of DCC_MetData
 *	NSAMPLES is the best guess at the number of samples which this
 *		data chunk will have.  It is not an upper limit.
 *	NFIELD	is the number of fields this data chunk will have.
 *	FIELDS	is the list of ID's for the fields in this DC.
 *	NELEMS	is the uniform number of elements in each field.
 * Exit:
 *	The data chunk has been configured for this mode of operation.
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "SetupUniformOrg"))
		return;

	finfo = dc_BuildFieldInfo (dc, nfield, fields);
	if (finfo)
	{
		finfo->fi_UniformOrg = TRUE;
		finfo->fi_Size = nelems;
	}
	return;
}




static FldInfo *
dc_BuildFieldInfo (dc, nfield, fields)
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Check limits, set default values, and construct the hash index table.
 */
{
	FldInfo *finfo;
	int fld;
/*
 * Make sure someone hasn't already tried this.
 */
	if (dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
	{
		msg_ELog (EF_PROBLEM, 
			  "Fields already setup; setup only allowed once");
		return NULL;
	}
/*
 * Allocate and fill in our structure.
 */
	finfo = ALLOC (FldInfo);
/*
 * Make sure someone's not trying to overstuff us with fields.
 * If so, only the first DC_MaxField fields will be accepted.
 */
	if (nfield > DC_MaxField)
	{
		msg_ELog(EF_PROBLEM,
			"%i is too many fields, max is %i",
			nfield, DC_MaxField);
		finfo->fi_NField = DC_MaxField;
	}
	else
	{
		finfo->fi_NField = nfield;
	}
	for (fld = 0; fld < finfo->fi_NField; fld++)
	{
		finfo->fi_Fields[fld] = fields[fld];
		finfo->fi_Types[fld] = DCT_Float;
		finfo->fi_Sizes[fld] = 0;
		finfo->fi_Offset[fld] = 0;
	}
	finfo->fi_Badval = DefaultBadval;
	finfo->fi_BlockChanges = FALSE;
	finfo->fi_Uniform = FALSE;
	finfo->fi_UniformOrg = FALSE;
	finfo->fi_Size = 0;
	finfo->fi_FixedFields = FALSE;
	finfo->fi_ReserveSpace = TRUE;	/* pre-arrange space if possible */
	finfo->fi_NSample = 0;
/*
 * Create the hash table for finding the index of a given FieldId
 */
	dc_BuildIndexHash (finfo);
/* 
 * Attach it to the DC.
 */
	dc_AddADE (dc, (DataPtr) finfo, DCC_MetData, ST_FLDINFO, 
				sizeof (FldInfo), TRUE);
	return (finfo);
}




static void
dc_BuildIndexHash (finfo)
FldInfo *finfo;
/*
 * Initialize the hash table to the indices of all of our FieldIds.
 */
{
	int i, hash;

	for (i = 0; i < HASH_SIZE; ++i)
		finfo->fi_HashIndex[i] = -1;	/* fill with empty slots */
	/*
	 * Make sure HASH_SIZE has not gotten out of sync with DC_MaxField.
	 */
	if (DC_MaxField + 1 > HASH_SIZE)
	{
		msg_ELog (EF_PROBLEM, 
		  "Field index hash table too small, check MD_HASH_SIZE defn");
		if (finfo->fi_NField + 1 > HASH_SIZE)
			return;
	}
	/*
	 * Now pass through all of our fields, and place them in the 
	 * hash table.  Collisions are placed in the next available slot,
	 * where the next higher slot may circle around to the bottom.
	 */
	for (i = 0; i < finfo->fi_NField; ++i)
	{
		hash = HASH_FIELD_ID (finfo->fi_Fields[i]);
		while (finfo->fi_HashIndex[hash] != -1)
			if (++hash == HASH_SIZE)
				hash = 0;
		finfo->fi_HashIndex[hash] = i;
	}
}




void
dc_BlockChanges (dc)
DataChunk *dc;
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "BlockChanges"))
		return;
/*
 * Grab the field info structure, and set the bad value flag.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM, "Attempt to block changes with no fields");
		return;
	}
	finfo->fi_BlockChanges = TRUE;
}




/*
 * --------------------------------------------------------------------------
 * Element type handling
 * --------------------------------------------------------------------------
 */

static inline void
dc_MDSetType (dc, finfo, fid, type)
DataChunk *dc;
FldInfo *finfo;
FieldId fid;
DC_ElemType type;
/*
 * Find the field with the given 'fid' and set it to the given 'type'
 */
{
	int idx;

	idx = dc_GetIndex (finfo, fid);
	finfo->fi_Types[idx] = type;
}



static FldInfo *
dc_ChangeInfo (dc, method)
DataChunk *dc;
char *method;
/*
 * Returns the DataChunk finfo if change is legal, else returns NULL
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, method))
		return NULL;
/*
 * Grab the field info structure, and set the bad value flag.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM, 
			  "%s: attempt to change info with no fields", method);
		return NULL;
	}
/*
 * Don't allow changes to the field type once data has been added
 */
	if (finfo->fi_NSample > 0)
	{
		msg_ELog (EF_PROBLEM, 
		  "%s: cannot change info after samples added", method);
		return NULL;
	}
/*
 * Forbid change if they have been blocked by the subclass
 */
	if (finfo->fi_BlockChanges)
	{
		msg_ELog (EF_PROBLEM, 
			  "%s: changes blocked; cannot change info", method);
		return NULL;
	}
	return (finfo);
}



void
dc_SetUniformFieldSize (dc, size)
DataChunk *dc;
int size;
/*
 * Set the uniform size for all of the fields in this chunk.
 */
{
	FldInfo *finfo;

	finfo = dc_ChangeInfo (dc, "SetUniformFieldSize");
	if (finfo)
	{
		finfo->fi_Uniform = TRUE;
		finfo->fi_Size = size;
	}
}





void
dc_SetUniformOrg (dc, num)
DataChunk *dc;
int num;
/*
 * Set the number of elements/sample for all of the fields in this chunk.
 */
{
	FldInfo *finfo;

	finfo = dc_ChangeInfo (dc, "SetUniformOrg");
	if (finfo)
	{
		finfo->fi_UniformOrg = TRUE;
		finfo->fi_Size = num;
	}
}




void
dc_SetFieldTypes (dc, nfield_in, fields_in, types_in)
DataChunk *dc;
int nfield_in;
FieldId *fields_in;
DC_ElemType *types_in;
/*
 * Can be called only after the fields have been defined and before any
 * data has been added.  Each field in the 'fields_in' array is assigned
 * the corresponding type in the 'types_in' array.
 */
{
	FldInfo *finfo;
	int i;

	finfo = dc_ChangeInfo (dc, "SetFieldTypes");
	if (finfo)
	{
		for (i = 0; i < nfield_in; ++i)
			dc_MDSetType (dc, finfo, fields_in[i], types_in[i]);
	}
}




void
dc_FixFieldSizes (dc, sizes)
DataChunk *dc;
int *sizes;
/*
 * Inform us that all of the fields will have the fixed size given in sizes,
 * where sizes[] corresponds to the FieldId array passed into SetupFields.
 */
{
	FldInfo *finfo;
	int i;

	finfo = dc_ChangeInfo (dc, "FixFieldSizes");
	if (finfo)
	{
		for (i = 0; i < finfo->fi_NField; ++i)
			finfo->fi_Sizes[i] = sizes[i];
	}
	finfo->fi_FixedFields = TRUE;
}




void
dc_ReserveFieldSpace (dc, reserve)
DataChunk *dc;		/* subclass of DC_MetTypes 	*/
bool reserve;		/* FALSE disables the pre-allocation and forces TOC */
{
	FldInfo *finfo;

	finfo = dc_ChangeInfo (dc, "ReserveFieldSpace");
	if (finfo)
		finfo->fi_ReserveSpace = (reserve) ? TRUE : FALSE;
}




void
dc_SetType (dc, fid, type)
DataChunk *dc;		/* subclass of DC_MetTypes 	*/
FieldId fid;		/* field whose type to define 	*/
DC_ElemType type;	/* The type to assign		*/
/*
 * Note the type of the given field in this DataChunk.
 */
{
	FldInfo *finfo;

	finfo = dc_ChangeInfo (dc, "SetType");
	if (finfo)
		dc_MDSetType (dc, finfo, fid, type);
}




DC_ElemType
dc_Type (dc, fid)
DataChunk *dc;
FieldId fid;
/* 
 * Return the element type for this field
 */
{
	int idx;
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "Type"))
		return (DCT_Unknown);
/*
 * Grab the field info structure, find the field, and return its type
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM, "Attempt to get type with no fields");
		return (DCT_Unknown);
	}

	idx = dc_GetIndex (finfo, fid);
	if (idx >= 0)
		return (finfo->fi_Types[idx]);
	else
		return (DCT_Unknown);
}



int
dc_SizeOf (dc, fid)
DataChunk *dc;
FieldId fid;
/* 
 * Return the element size (in bytes) of this field
 */
{
	return (dc_SizeOfType (dc_Type(dc, fid)));
}



/* ----------------------------------------------------------------------
 * Utility routines for converting between elements, void pointers, and
 * strings.
 * ----------------------------------------------------------------------
 */
void
dc_AssignElement (e, ptr, type)
DC_Element *e;
void *ptr;
DC_ElemType type;
/*
 * De-reference a void pointer to type and store it into a DC_Element union.
 * Unknown types are copied opaquely if they fit, otherwise the union is
 * filled with zeros.
 */
{
	switch (type)
	{
	   case DCT_Float:
		e->dcv_float = *(float *)ptr;
		break;
	   case DCT_Double:
		e->dcv_double = *(double *)ptr;
		break;
	   case DCT_LongDouble:
		e->dcv_longdbl = *(LongDouble *)ptr;
		break;
	   case DCT_Char:
		e->dcv_char = *(char *)ptr;
		break;
	   case DCT_UnsignedChar:
		e->dcv_uchar = *(unsigned char *)ptr;
		break;
	   case DCT_ShortInt:
		e->dcv_shortint = *(short *)ptr;
		break;
	   case DCT_UnsignedShort:
		e->dcv_ushort = *(unsigned short *)ptr;
		break;
	   case DCT_Integer:
		e->dcv_int = *(int *)ptr;
		break;
	   case DCT_UnsignedInt:
		e->dcv_uint = *(unsigned int *)ptr;
		break;
	   case DCT_LongInt:
		e->dcv_longint = *(long int *)ptr;
		break;
	   case DCT_UnsignedLong:
		e->dcv_ulong = *(unsigned long *)ptr;
		break;
	   case DCT_String:
		e->dcv_string = *(char **)ptr;
		break;
	   case DCT_Boolean:
		e->dcv_boolean = *(unsigned char *)ptr;
		break;
	   case DCT_ZebTime:
		e->dcv_zebtime = *(ZebTime *)ptr;
		break;
	   case DCT_VoidPointer:
		e->dcv_pointer = ptr;
		break;
	   case DCT_Element:
		*e = *(DC_Element *)ptr;
		break;
	   default:
		if (dc_SizeOfType (type) <= sizeof(*e))
			memcpy ((void *)e, ptr, dc_SizeOfType (type));
		else
			memset ((void *)e, 0, sizeof (*e));
		break;
	}
}



void
dc_AssignValue (ptr, e, type)
void *ptr;
DC_Element *e;
DC_ElemType type;
/* 
 * Assign an element union containing the given type to space pointed
 * to by ptr.  Usually ptr is actually a pointer to the type which 
 * was cast to (void *) when passed into the function.
 */
{
	switch (type)
	{
	   case DCT_Float:
		*(float *)ptr = e->dcv_float;
		break;
	   case DCT_Double:
		*(double *)ptr = e->dcv_double;
		break;
	   case DCT_LongDouble:
		*(LongDouble *)ptr = e->dcv_longdbl;
		break;
	   case DCT_Char:
		*(char *)ptr = e->dcv_char;
		break;
	   case DCT_UnsignedChar:
		*(unsigned char *)ptr = e->dcv_uchar;
		break;
	   case DCT_ShortInt:
		*(short *)ptr = e->dcv_shortint;
		break;
	   case DCT_UnsignedShort:
		*(unsigned short *)ptr = e->dcv_ushort;
		break;
	   case DCT_Integer:
		*(int *)ptr = e->dcv_int;
		break;
	   case DCT_UnsignedInt:
		*(unsigned int *)ptr = e->dcv_uint;
		break;
	   case DCT_LongInt:
		*(long int *)ptr = e->dcv_longint;
		break;
	   case DCT_UnsignedLong:
		*(unsigned long *)ptr = e->dcv_ulong;
		break;
	   case DCT_String:
		*(char **)ptr = e->dcv_string;
		break;
	   case DCT_Boolean:
		*(unsigned char *)ptr = e->dcv_boolean;
		break;
	   case DCT_ZebTime:
		*(ZebTime *)ptr = e->dcv_zebtime;
		break;
	   case DCT_VoidPointer:
		ptr = e->dcv_pointer;
		break;
	   case DCT_Element:
		*(DC_Element *)ptr = *e;
		break;
	   default:
		if (dc_SizeOfType (type) <= sizeof(*e))
			memcpy (ptr, (void *)e, dc_SizeOfType (type));
		break;
	}
}



const char *
dc_ElemToString (e, type)
DC_Element *e;
DC_ElemType type;
/*
 * Convert an element to a string, and return the string.  The returned
 * buffer space is only valid until the next call to this function or to
 * dc_ValueToString.  If the type is a string, allocate dynamic space if
 * the string won't fit in static.  If the type is unknown, the byte values
 * are printed in hex.
 */
{
	static char *dest = NULL;
	static char buf[128]; 	/* holds all numbers and times, yes? */
	unsigned char *hex;

	/*
	 * If we used dynamic memory last time, free it.
	 */
	if ((dest != buf) && (dest != NULL))
		free (dest);
	dest = buf;		/* default to writing in static memory */
	switch (type)
	{
	   case DCT_Float:
		sprintf (dest, "%g", e->dcv_float);
		break;
	   case DCT_Double:
		sprintf (dest, "%g", e->dcv_double);
		break;
	   case DCT_LongDouble:
		sprintf (dest, "%Lg", e->dcv_longdbl);
		break;
	   case DCT_Char:
		sprintf (dest, "%c", e->dcv_char);
		break;
	   case DCT_UnsignedChar:
		sprintf (dest, "%#hx", e->dcv_uchar);
		break;
	   case DCT_ShortInt:
		sprintf (dest, "%hi", e->dcv_shortint);
		break;
	   case DCT_UnsignedShort:
		sprintf (dest, "%hu", e->dcv_ushort);
		break;
	   case DCT_Integer:
		sprintf (dest, "%d", e->dcv_int);
		break;
	   case DCT_UnsignedInt:
		sprintf (dest, "%u", e->dcv_uint);
		break;
	   case DCT_LongInt:
		sprintf (dest, "%li", e->dcv_longint);
		break;
	   case DCT_UnsignedLong:
		sprintf (dest, "%lu", e->dcv_ulong);
		break;
	   case DCT_String:
		/*
		 * Make sure we have enough space for arbitrary strings
		 */
		if (strlen(e->dcv_string) >= sizeof(buf))
			dest = (char *)malloc(strlen(e->dcv_string) + 1);
		strcpy (dest, e->dcv_string);
		break;
	   case DCT_Boolean:
		sprintf (dest, "%s", (e->dcv_boolean) ? "true" : "false");
		break;
	   case DCT_ZebTime:
		if (e->dcv_zebtime.zt_MicroSec)
			TC_EncodeTime (&e->dcv_zebtime, TC_FullUSec, dest);
		else
			TC_EncodeTime (&e->dcv_zebtime, TC_Full, dest);
		break;
	   case DCT_VoidPointer:
		sprintf (dest, "%#0x", (int) e->dcv_pointer);
		break;
	   default:
		hex = (unsigned char *)e;
		if (5 * sizeof (*e) >= sizeof(buf))
			dest = (char *)malloc(5 * sizeof (*e) + 1);
		dest[0] = '\0';
		while (hex < (unsigned char *)e + sizeof(*e))
			sprintf (dest+strlen(dest), "%#0x ", (int)*(hex++));
		break;
	}
	return (dest);
}



const char *
dc_ValueToString (ptr, type)
void *ptr;
DC_ElemType type;
/*
 * Convert the void pointer to an element union and then to a string.
 * The return value is identical to dc_ElemToString.
 */
{
	DC_Element e;

	dc_AssignElement (&e, ptr, type);
	return (dc_ElemToString(&e, type));
}



const char *
dc_PrintElement (e, type)
DC_Element *e;
DC_ElemType type;
/*
 * Same as dc_ElemToString except special characters are converted to their
 * escaped sequences.
 */
{
	static char *dest = NULL;
	static char buf[256];
	const char *result, *src;
	char *obj;

	if ((dest != buf) && (dest != NULL))
		free (dest);
	dest = buf;		/* default to writing in static memory */
	result = dc_ElemToString (e, type);
	/*
	 * We don't really have anything to do unless the type is
	 * character or string.
	 */
	if (type != DCT_String && type != DCT_Char)
		return (result);
	/*
	 * We'll need, at the most, twice as much space for backslashes
	 */
	if (2*strlen(result) >= sizeof(buf))
		dest = (char *)malloc((2 * strlen(result)) + 1);
	dest[0] = '\0';
	src = result;
	obj = dest;
	while (*src)
	{
		switch (*src)
		{
		   case '\t':
			strcat (obj, "\\t");
			break;
		   case '\0':
			strcat (obj, "\\0");
			break;
		   case '\n':
			strcat (obj, "\\n");
			break;
		   case '\b':
			strcat (obj, "\\b");
			break;
		   case '\f':
			strcat (obj, "\\f");
			break;
		   case '\r':
			strcat (obj, "\\r");
			break;
		   default:
			*obj = *src;
			--obj;
		}
		++src;
		obj += 2;
	}
	*obj = '\0';
	return (dest);
}



const char *
dc_PrintValue (ptr, type)
void *ptr;
DC_ElemType type;
/*
 * Same as dc_PrintElement except it first converts a pointer to void
 * to an element before calling dc_PrintElement
 */
{
	DC_Element e;

	dc_AssignElement (&e, ptr, type);
	return (dc_PrintElement (&e, type));
}



double
dc_GetBadval (dc)
DataChunk *dc;
/*
 * Return the bad value flag stored in this DC.
 */
{
	FldInfo *finfo;
	char *abad;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetBadval"))
		return (0);
#ifdef SUPPORT_BADVALUES
/*
 * Look first for a global attribute.
 */
	if ((abad = dc_GetGlobalAttr (dc, "bad_value_flag")))
	{
		float fbad;
		sscanf (abad, "%f", &fbad);
		return (fbad);
	}
/*
 * Grab the field info structure, and return the bad value flag stored
 * therein.  If the structure doesn't exist, return the default.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return (DefaultBadval);
	return (finfo->fi_Badval);
#else
	return (DefaultBadval);
#endif /* SUPPORT_BADVALUES */
}





void
dc_SetBadval (dc, badval)
DataChunk *dc;
float badval;
/*
 * Store the bad value flag into this DC.  This should be done after fields
 * are set up, but before data are added.
 */
{
	FldInfo *finfo;
	char sbad[60];
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "SetBadval"))
		return;
#ifdef SUPPORT_BADVALUES
/*
 * Grab the field info structure, and set the bad value flag.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM,"Attempt to set bad flag with no fields");
		return;
	}
	finfo->fi_Badval = badval;
/*
 * Also store it as a global attribute.
 */
	sprintf (sbad, "%f", badval);
	dc_SetGlobalAttr (dc, "bad_value_flag", sbad);
#endif /* SUPPORT_BADVALUES */
}





int
dc_GetNField (dc)
DataChunk *dc;
/*
 * Return the number of fields stored in this DC.
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetNField"))
		return (0);
/*
 * Grab the field info structure and return the number.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return (0);
	return (finfo->fi_NField);
}




FieldId *
dc_GetFields (dc, nf)
DataChunk *dc;
int *nf;
/*
 * Return the list of fields in this DC.
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetFields"))
		return (0);
/*
 * Grab the field info structure and return the stuff.
 */
	if (nf)
		*nf = 0;
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return (0);
	if (nf)
		*nf = finfo->fi_NField;
	return (finfo->fi_Fields);
}





DC_ElemType *
dc_GetFieldTypes (dc, nf)
DataChunk *dc;
int *nf;
/*
 * Return the list of field types in this DC.
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetFieldTypes"))
		return (0);
/*
 * Grab the field info structure and return the stuff.
 */
	if (nf)
		*nf = 0;
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return (0);
	if (nf)
		*nf = finfo->fi_NField;
	return (finfo->fi_Types);
}




static void
dc_MDOptimize (dc, finfo)
DataChunk *dc;
FldInfo *finfo;
/*
 * Check our uniformity flags and the types of our fields to see if its
 * possible to optimize the organization of our samples into uniform sizes.
 * If uniformity not possible, then check for fixed field sizes.  If we're
 * using fixed field sizes AND pre-arranging space, setup our offset table
 * now.  Otherwise, we'll have to resort to the FieldTOC method.
 *
 * If we know how much space the fields will take up, we also need to figure
 * out the space needed to align each sample and add that space to the
 * sample's total size.
 */
{
	int i, tsize, ssize;
	bool uniform;

/*
 * If holding space for all defined fields has been disabled, disable all
 * optimization flags so that we resort to non-uniform field storage
 */
	if (!finfo->fi_ReserveSpace)
	{
		finfo->fi_Uniform = FALSE;
		finfo->fi_UniformOrg = FALSE;
		finfo->fi_FixedFields = FALSE;
	}
	else if (!finfo->fi_Uniform && 
		 (finfo->fi_UniformOrg || finfo->fi_FixedFields))
	{
	/*
	 * If we have a uniform org, then we can use uniformity if all of the
	 * type sizes are the same.  Otherwise we at least use fixed-field.
	 */
		uniform = finfo->fi_UniformOrg;
		finfo->fi_Offset[0] = 0;
		for (i = 0; i < finfo->fi_NField; ++i)
		{
			if ((i > 0) && 
			    dc_SizeOfType(finfo->fi_Types[i]) != tsize)
				uniform = FALSE;
			tsize = dc_SizeOfType(finfo->fi_Types[i]);
		/*
		 * This particular field's size is the uniform elems/sample
		 * multiplied by the element's type size
		 */
			if (! finfo->fi_FixedFields)
				finfo->fi_Sizes[i] = tsize * finfo->fi_Size;
			else
				/* convert nelems to actual bytes */
				finfo->fi_Sizes[i] *= tsize;
		/*
		 * Go ahead and set the field's offset, in case we end up
		 * using fixed field sizes.
		 */
			if (i > 0)
			{
				int len = finfo->fi_Offset[i-1] + 
					finfo->fi_Sizes[i-1];
				finfo->fi_Offset[i] = (int)ALIGN(len,tsize);
			}
		}
	
		if (uniform)
		{
		/*
		 * For uniform orgs, the number of elements per sample is 
		 * stored in fi_Size, so multiply by type size to get the 
		 * uniform field size.
		 */
			finfo->fi_Size *= tsize;
			finfo->fi_Uniform = TRUE;
		}
		else
		{
		/*
		 * We must at least be able to use fixed fields.
		 */
			finfo->fi_FixedFields = TRUE;
		}
	}

	if (finfo->fi_Uniform)
	{
	/*
	 * Set up some optimization in our parent class based on our
	 * uniform field size.  Note that uniform metdata has no class
	 * overhead in the sample size, so we leave the overhead unchanged.
	 */
		dc_HintSampleSize (dc, finfo->fi_NField*finfo->fi_Size, FALSE);
	}
	else if (finfo->fi_FixedFields)
	{
	/*
	 * Find our sample size and adjust for alignment of each sample
	 */
		ssize = finfo->fi_Offset[finfo->fi_NField - 1];
		ssize += finfo->fi_Sizes[finfo->fi_NField - 1];
		ssize = (int) ALIGN(ssize,DC_ElemTypeMaxSize);
		dc_HintSampleSize (dc, ssize, FALSE);
	}
	else	/* we've no choice but to use FieldTOC's */
	{
		dc_HintSampleOverhead (dc, sizeof(FieldTOC)*finfo->fi_NField);
	}
	finfo->fi_BlockChanges = TRUE;
}




void
dc_AddMData (dc, t, field, size, start, nsamp, data)
DataChunk *dc;
ZebTime *t;
FieldId field;
int start, size, nsamp;
DataPtr data;
/*
 * Add data to this data chunk for one field.
 * Entry:
 *	DC	is a data chunk which is a subclass of MetData
 *	T	is an array of times for the incoming data
 *	FIELD	is the field for which this data is being added
 *	SIZE	is the size of one sample (all must be same size)
 *	START	is the index of the first sample to be modified.
 *	NSAMP	is the number of samples to add
 *	DATA	is the actual data.
 * Exit:
 *	The data has been added.
 */
{
	FldInfo *finfo;
	int findex;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "AddMData"))
		return;
/*
 * Grab the field info structure and return the number.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add data with no finfo");
		return;
	}
/*
 * If our first sample, check for possible optimizations
 */
	if (finfo->fi_NSample == 0)
		dc_MDOptimize (dc, finfo);
/*
 * Find the index of our field.
 */
	if ((findex = dc_GetIndex (finfo, field)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add field %d not in finfo",
				field);
		return;
	}
/*
 * Uniform and non-uniform data are handled differently, and hence separately.
 */
	if (finfo->fi_Uniform)
		dc_AddUniform(dc, finfo, findex, t, size, start, nsamp, data);
	else if (finfo->fi_FixedFields)
		dc_AddFixedField(dc,finfo,findex,t, size, start, nsamp, data);
	else
		dc_AddNonUniform(dc,finfo,findex,t, size, start, nsamp, data);
}




static void
dc_AddNonUniform (dc, finfo, findex, t, size, start, nsamp, data)
DataChunk *dc;
FldInfo *finfo;
int findex, size, start, nsamp;
ZebTime *t;
DataPtr data;
/*
 * Add this field to a DC with non-uniform data.
 */
{
	int samp, len, i;
	DataPtr space;
	FieldTOC *ft;
	int tsize;

	tsize = dc_SizeOfType(finfo->fi_Types[findex]);
	for (samp = start; samp < start + nsamp; samp++)
	{
		space = dc_GetSample (dc, samp, &len);
		ft = (FieldTOC *) space;
	/*
	 * If the sample exists and this field is not already a part of it
	 * or will not fit in existing space, then expand to the size we
	 * need.  Note that this may leave a hole equal to the original
	 * space occupied by the field, but it's easier than copying and
	 * shifting offsets, and it should hardly ever happen.
	 */
		if (space && ft[findex].ft_Len < size)
		{
			int align_len = (int) ALIGN(len,tsize);
			ft[findex].ft_Offset = align_len;
			ft[findex].ft_Len = size;
			dc_AdjustSample (dc, samp, align_len + size);
			space = dc_GetSample(dc,samp,&len);
			ft = (FieldTOC *) space;
		}
	/*
	 * If the sample did not exist, create it.  We need space to align
	 * the TOC structures, space for the field data being added, and
	 * then any space necessary to align the field data, if its size
	 * makes it necessary.
	 */
		else if (!space)
		{
			len = size + finfo->fi_NField*sizeof(FieldTOC);
			space = dc_AddAlignedSample (dc, t, NULL, len, 
						     sizeof(FieldTOC));
			++finfo->fi_NSample;
			ft = (FieldTOC *) space;
			for (i = 0; i < finfo->fi_NField; i++)
				ft[i].ft_Offset = ft[i].ft_Len = 0;
			ft[findex].ft_Offset = finfo->fi_NField *
				sizeof(FieldTOC);
			ft[findex].ft_Len = size;
		}
	/* 
	 * If both of the above tests failed, then we already have space
	 * for the field.  No matter what, we can just copy in the data.
	 */
		if (data)
			memcpy ((char *) space + ft[findex].ft_Offset, data,
					size);
	/*
	 * On to the next one.
	 */
		t++;
		if (data)
			data = (char *) data + size;
	}
}





static void
dc_AddFixedField (dc, finfo, findex, t, size, start, nsamp, data)
DataChunk *dc;
FldInfo *finfo;
int findex, size, start, nsamp;
ZebTime *t;
DataPtr data;
/*
 * Add this field to a DC with fixed field space arrangements
 */
{
	int samp, len;
	DataPtr space;

	for (samp = start; samp < start + nsamp; samp++)
	{
		space = dc_GetSample (dc, samp, &len);
	/*
	 * If we don't have this sample yet, create it.
	 */
		if (!space)
		{
			len = finfo->fi_Offset[finfo->fi_NField - 1] + 
				finfo->fi_Sizes[finfo->fi_NField - 1];
			space = dc_AddAlignedSample (dc, t, NULL, len, 
						     DC_ElemTypeMaxSize);
			++finfo->fi_NSample;
		}
	/*
	 * Now just copy the data into the pre-arranged offset
	 */
		if (data)
			memcpy ((char *) space + finfo->fi_Offset[findex], 
				data, size);
	/*
	 * On to the next one.
	 */
		t++;
		if (data)
			data = (char *) data + size;
	}
}





static void
dc_AddUniform (dc, finfo, findex, t, size, start, nsamp, data)
DataChunk *dc;
FldInfo *finfo;
int findex;
ZebTime *t;
int size, start, nsamp;
DataPtr data;
/*
 * Loop over each sample to be added.  Verify or create the space, then
 * add the data.
 */
{
	int samp;
	DataPtr dest;
	int offset = finfo->fi_Size * findex;
/*
 * Make sure the size is what we were promised.
 */
	if (size != finfo->fi_Size)
	{
		msg_ELog (EF_PROBLEM, "Size %d does not match finfo %d",
			size, finfo->fi_Size);
		return;
	}
/*
 * If they don't all exist, there better not be gaps in the middle.
 */
	if (start > finfo->fi_NSample)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add samp %d after %d exist",
			start, finfo->fi_NSample);
		return;
	}
	
	for (samp = start; samp < start + nsamp; ++samp)
	{
	/*
	 * If this is a new sample, add space for it.  Otherwise, overwrite
	 * the time in the existing sample.  Either way, "dest" ends up as
	 * the location for writing our data.
	 */
		if (samp >= finfo->fi_NSample)
		{
			dest = dc_AddSample (dc, t + samp - start, NULL,
					     finfo->fi_NField * size);
			++finfo->fi_NSample;
		}
		else
		{
			dc_SetTime (dc, samp, t + samp - start);
			dest = dc_GetSample (dc, samp, NULL);
		}
	/*
	 * This absolutely, undubitably, definately, certainly, without
	 * fail should never, ever happen, not even in a million years.
	 */
		if (! dest)
		{
			msg_ELog (EF_PROBLEM, 
				  "Sample %d disappeared anyway", samp);
			return;
		}
		if (data)
		{
			memcpy ((char *) dest + offset, data, finfo->fi_Size);
			data = (char *) data + finfo->fi_Size;
		}
	}
}





int
dc_GetFieldIndex (dc, field)
DataChunk *dc;
FieldId field;
/*
 * This is the subclass method of finding a field's index.  The index returned
 * corresponds to the field's index in the array of field ID's passed into
 * either of the SetupFields methods.  Returns -1 if the field is not found.
 */
{
	FldInfo *finfo;
/*
 * Make sure this one of our subclasses.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "AddMData"))
		return (-1);
/*
 * Retrieve the field info.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add data with no finfo");
		return (-1);
	}
/*
 * Use our private method to actually find the field.
 */
	return (dc_GetIndex (finfo, field));
}




static int
dc_GetIndex (finfo, field)
FldInfo *finfo;
FieldId field;
/*
 * Return the index of this field in this DC by looking it up in the hash table.
 * Note that we expect at least one more empty slot than the maximum possible
 * number of fields, so that we don't infinitely loop if we get a field that
 * is not in a datachunk with DC_MaxField fields.
 */
{
	int hash, f;

	hash = HASH_FIELD_ID(field);

	while (finfo->fi_HashIndex[hash] != -1)
	{
		if (finfo->fi_Fields[finfo->fi_HashIndex[hash]] == field)
			return (finfo->fi_HashIndex[hash]);
		if (++hash == HASH_SIZE)
			hash = 0;
	} 

	/*
	 * This is a backup for the hash table, in case it's not being used
	 * because of problems.  If someone is specifying wrong field id's,
	 * then they deserve the overhead of the automatic extra check.
	 */
	for (f = 0; f < finfo->fi_NField; f++)
		if (finfo->fi_Fields[f] == field)
			return (f);
	return (-1);
}





DataPtr
dc_GetMData (dc, sample, field, len)
DataChunk *dc;
int sample, *len;
FieldId field;
/*
 * Find a data sample in this DC.
 */
{
	FldInfo *finfo;
	FieldTOC *ft;
	int findex;
	DataPtr data;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetMData"))
		return (NULL);
/*
 * Grab the field info structure.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM, "Attempt to get data with no finfo");
		return (NULL);
	}
/*
 * Find the index of our field.
 */
	if ((findex = dc_GetIndex (finfo, field)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Attempt to get field %d not in finfo",
				field);
		return (NULL);
	}
/*
 * Find this sample.
 */
	if ((data = dc_GetSample (dc, sample, NULL)) == NULL)
		return (NULL);
/*
 * Now that we have everything, we're set.
 */
	if (finfo->fi_Uniform)
	{
		if (len)
			*len = finfo->fi_Size;
		return ((char *) data + findex*finfo->fi_Size);
	}
	else if (finfo->fi_FixedFields)
	{
	/*
	 * For fixed fields, the index is in the offset table
	 */
		if (len)
			*len = finfo->fi_Sizes[findex];
		return ((char *) data + finfo->fi_Offset[findex]);
	}
/*
 * In the non-uniform case, find the TOC and return the info from there.
 */
	ft = (FieldTOC *) data;
	if (ft[findex].ft_Len <= 0)
		return (NULL);
	if (len)
		*len = ft[findex].ft_Len;
	return ((char *) data + ft[findex].ft_Offset);
}





void
dc_SetFieldAttrArray (dc, field, key, type, nval, values)
DataChunk *dc;
FieldId field;
char *key;
DC_ElemType type;
int nval;
void *values;
{
	if (! dc_ReqSubClassOf(dc->dc_Class,
			       DCC_MetData, "SetFieldAttrArray"))
		return;
	dca_AddAttrArray (dc, DCC_MetData, ST_FIELDATTR(field),
			  key, type, nval, values);
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

	if (! dc_ReqSubClassOf(dc->dc_Class, DCC_MetData,
			       "GetFieldAttrArray"))
		return NULL;
	if ((values = dca_GetAttrArray (dc, DCC_MetData, ST_FIELDATTR(field),
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
	if (! dc_ReqSubClassOf(dc->dc_Class, DCC_MetData, 
			       "ProcFieldAttrArrays"))
		return (0);
	return (dca_ProcAttrArrays (dc, DCC_MetData, ST_FIELDATTR(field),
				    pattern, func, arg));
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
	if (! dc_ReqSubClassOf (dc->dc_Class, 
				DCC_MetData, "RemoveFieldAttr"))
		return;
	dca_RemoveAttr (dc, DCC_MetData, ST_FIELDATTR(field), key);
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
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "SetFieldAttr"))
		return;
	dca_AddAttr (dc, DCC_MetData, ST_FIELDATTR(fid), key, value);
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

	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetFieldAttr"))
		return(NULL);
	if ((value = dca_GetAttr (dc, DCC_MetData, ST_FIELDATTR(fid), key)))
		return(value);
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
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetFiAttrBlock"))
		return(NULL);
	return (dca_GetBlock (dc, DCC_MetData, ST_FIELDATTR(fid), len));
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
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "SetFiAttrBlock"))
		return;
	dca_PutBlock (dc, DCC_MetData, ST_FIELDATTR(fid), block, len);
}




int
dc_ProcessFieldAttrs(dc, fid, pattern, func)
DataChunk *dc;
FieldId fid;
char *pattern;
int (*func) ();
/*
 * Pass all of the attributes of this fid to the function
 */
{
	if (! dc_ReqSubClassOf(dc->dc_Class, DCC_MetData, "ProcessFieldAttrs"))
		return(0);
	return (dca_ProcAttrs (dc, DCC_MetData, ST_FIELDATTR(fid),
			       pattern, func));
}




int
dc_GetNFieldAttrs(dc, fid)
DataChunk *dc;
FieldId fid;
/*
 * Return the number of field attributes in a datachunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetNFieldAttrs"))
		return (0);
	return(dca_GetNAttrs(dc, DCC_MetData, ST_FIELDATTR(fid)));
}




char **
dc_GetFieldAttrList(dc, fid, pattern, values, natts)
DataChunk *dc;
FieldId fid;
char *pattern;
void **values[];
int *natts;
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetFieldAttrList"))
		return (NULL);
	return(dca_GetAttrList(dc, DCC_MetData, ST_FIELDATTR(fid),
			       pattern, values, natts));
}



char **
dc_GetFieldAttrKeys (dc, fid, natts)
DataChunk *dc;
FieldId fid;
int *natts;
{
/*
 * Returns a list of keys for the field attributes in this data chunk.
 * Also puts into natt the number of global attributes for this dc.
 * The returned array of attribute keys is only valid until the next call
 * of any of the Get*AttrList or Get*AttrKeys functions.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetFieldAttrKeys"))
		return (NULL);
	return(dca_GetAttrList(dc, DCC_MetData, ST_FIELDATTR(fid),
			       NULL, NULL, natts));
}
 


static void
dc_DumpMD (dc)
DataChunk *dc;
/*
 * Dump out this DC.
 */
{
	FldInfo *finfo;
	int i;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetMData"))
		return;
/*
 * Grab the field info structure.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return;
/*
 * Write.
 */
	printf("%s, %d fields, uniform %s, uniform org %s, size %d\n",
	       "METDATA class",
		finfo->fi_NField, finfo->fi_Uniform ? "True" : "False",
		finfo->fi_UniformOrg ? "True" : "False", finfo->fi_Size);
	printf("Fields: ");
	for (i = 0; i < finfo->fi_NField; i++)
	{
		printf (" %s(%s,%d);", F_GetName (finfo->fi_Fields[i]),
			dc_TypeName (finfo->fi_Types[i]), 
			dc_SizeOfType (finfo->fi_Types[i]));
	}
	printf ("\n");
/*
 * If non-uniform, outline the layout of the last sample, just to give some 
 * idea of space usage.  Uniform data should be mostly uninteresting.
 */
	if (finfo->fi_NSample > 0 && !finfo->fi_Uniform)
	{
		int len;
		DataPtr samp = dc_GetSample (dc, finfo->fi_NSample - 1, &len);
		FieldTOC *ft = (FieldTOC *)samp;

		printf ("%s, field offsets for sample %d:\n", 
			finfo->fi_FixedFields ? "Fixed-field-size" : 
			"Non-uniform", (finfo->fi_NSample - 1) );
		for (i = 0; i < finfo->fi_NField; ++i)
		{
			printf ("%s:", F_GetName (finfo->fi_Fields[i]));
			if (finfo->fi_FixedFields)
				printf ("%d,%d  ", finfo->fi_Offset[i], 
					finfo->fi_Sizes[i]);
			else
				printf ("%d,%d  ", 
					ft[i].ft_Offset, ft[i].ft_Len);
		}
		printf ("\n");
	}
	printf ("Field Attributes:\n");
	for (i = 0; i < finfo->fi_NField; i++)
	{
		if (dca_GetNAttrs(dc, DCC_MetData,
				  ST_FIELDATTR(finfo->fi_Fields[i])) > 0)
		{
			printf ("\t%s:\t", F_GetName (finfo->fi_Fields[i]));
			dca_ProcAttrArrays (dc, DCC_MetData, 
					    ST_FIELDATTR(finfo->fi_Fields[i]), 
					    NULL, dc_PrintFiAttr, NULL);
			printf ("\n");
		}
	}
}



static int
dc_PrintFiAttr (key, value, nval, type, arg)
char *key;
void *value;
int nval;
DC_ElemType type;
void *arg;
/*
 * Print out a field attribute
 */
{
	int i;

	if (nval && (type == DCT_String))
	{
		printf (" %s='%s';", key, (char *)value);
		return (0);
	}
	printf (" %s=[", key);
	for (i = 0; i < nval; ++i)
	{
		printf ("%s%s", dc_PrintValue (value, type),
			(i == nval - 1) ? "];" : ",");
		value = (char *)value + dc_SizeOfType (type);
	}
	if (nval == 0)
		printf ("];");
	return (0);
}
