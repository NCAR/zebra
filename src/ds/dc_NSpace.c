/*
 * The n-space data chunk class.
 *
 * For general-purpose, N-space, floating-point, regularly- or
 * irregularly-spaced meteorolgical data which does not fit existing
 * DataChunk data organizations.  Subclassed from DCC_MetData.  Perhaps
 * DCC_Scalar will some day be subclassed from here, as it logically should
 * be, but shouldn't as a matter of practicality.  This class is meant
 * principally as an interface to netCDF-format files, precipitated by the
 * needs of the ARM program.  The interface and functionality is based on
 * the netCDF data model and the use of 'coordinate variables' (also
 * referred to here as 'dimension variables').  See the netCDF
 * documentation for a definition of coordinate variables.
 *
 * Concerns about this class:
 *
 * The generality of the class conflicts with the separation of data classes
 * encouraged by the DataChunk class hierarchy.  The class could conceivably
 * be split into several different classes.  Perhaps this is a case for
 * subclasses of the NSpace class.
 *
 * The FieldId mechanism is technically being misused by associating a
 * field ID with a dimension, which may never actually have any data
 * associated with it.
 *
 * Implementation summary:
 *----------------------------------------------------------------------------
 * Dimensions will be defined as dimensions in the netCDF file with their
 * corresponding size.  Variables will be defined as netCDF variables over
 * the specified dimensions.  If a FieldId is defined as both a dimension
 * and a variable, then a variable will automatically be defined in the
 * netCDF file with the same name as a netCDF dimension.  Hence, by
 * convention, the variable will be a 'dimension variable', implying that
 * the domain of a variable defined over the dimension variable's dimension
 * is actually the range of the dimension variable rather than the ordinal
 * set of indices in the dimension.
 *
 * The number of dimensions in a DCC_NSpace variable will have to be
 * limited to the netCDF maximum, MAX_NC_DIMS = 32.
 *
 * The size each field requires will be equal to the product of its
 * dimensions; this size will be specified in setting up the fields for the
 * MetData class.  The sizes of the dimensions of a field are specified in
 * an array, either of unsigned longs or FieldIds which have been assigned
 * sizes.  The order of the dimensions in this array corresponds the order
 * the arguments in a 'mapping' of the field, so
 *
 * 	f(a, b, c, d) : [ m, n ]
 *
 * is defined by an array of dimensions [ 'a', 'b', 'c', 'd' ].  The linear
 * array will be stored in row-major order, so the 'd' dimension will vary
 * fastest in memory.  This follows the netCDF interface definition.
 * All field data will be stored and retrieved as a linear array of floats.
 *
 * Note that there is, as yet, no intention to include the equivalent to
 * the netCDF interface's hyperslab access in the DCC_NSpace interface.
 * But it should be easily implemented by just filling new samples with
 * BadValues before storing the hyperslab of data.  Theoretically though,
 * this is incorrect since the values not in the hyperslab would be
 * *missing* rather than *bad*.
 *
 * Once data has been added to any of the fields, no more dimensions or
 * fields can be defined in the DataChunk.  This is for coding convenience
 * and avoidance of impractical memory thrashing.
 *
 * On converting this class to supporting multiple data types:
 *------------------------------------------------------------------------
 * All of the field and dimension info is independent of a data size or
 * type.  At present floating point data is used to correspond to the
 * scalar class.  The parent MetData class has no element- or type-size
 * constraints---sample size is determined by the subclass.  Combining
 * multiple data types in one chunk might be possible, but it would violate
 * the whole reason for having the DataClass hierarchy and implementation
 * in the first place.  A more appropriate approach would be designing a
 * composite datachunk class which can 'contain' several instances of
 * different class datachunks.  Then the DFA interface would have to know
 * how to handle composite data chunks in order to combine different data
 * chunk classes in the same file.
 *
 * For a quick fix, add the data type size info to the field info
 * structure, and use this value when determining the size of the array
 * being passed to dc_AddMData() in dc_NSAddSample() and dc_NSAddStatic().
 * Then add the data type size to the interface.  That's it.  However,
 * interpretation and translation on the other end, such as in DFA_NetCDF
 * will be much more complicated.
 *
 * Possible Future Optimizations and Changes:
 *-------------------------------------------------------------------------
 * It probably wouldn't hurt to prefix the private function names with `_'
 * or `ns_', leaving the dc_ prefix for the public functions.
 *
 * Public functions should return non-zero on success and zero on failure.
 * This should be consistent among all functions where this makes sense.
 * At the moment this only exists for inquiry functions.
 *
 * Further Ideas
 *-------------------------------------------------------------------------
 * It would be nice if messages were printed noting the class of datachunk
 * being operated on at time of message.  When NSpace calls a MetData method,
 * the method should report the message as if from an NSpace method by 
 * noting that the class of the chunk is NSpace.
 *
 * For applications which are extracting data from a chunk, like plotting
 * or QC processes, it might be nice to have hyperslab access or some
 * kind of "class conversion" available.  For example, given an NSpace
 * chunk with dynamic field theta (w, x, y, z), return a DCC_Scalar chunk
 * with field theta over time with w, x, y, and z fixed at some coordinate.
 * Another possibility is creating an NSpace chunk from a hyperslab of
 * another NSpace chunk.
 */

/*=========================================================================
 *		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

#include <stdio.h>
#include <string.h>

#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "DataChunkP.h"

RCSID ("$Id: dc_NSpace.c,v 1.19 2001-10-16 22:26:30 granger Exp $")

/*
 * The DCC_NSpace public interface is included in DataStore.h, along with
 * some useful introductory information.
 */

typedef struct _NSpaceFldInfo
{
	unsigned short	nsf_NDimn;	/* number of dimensions */
	unsigned short	nsf_Dimns[DC_MaxDimension];
	                                /* index from BASE      */
	unsigned long	nsf_Size;	/* product of its dimns */
	unsigned char	nsf_Flags;	/* flags		*/
	unsigned long	nsf_StOffset;	/* Offset of static data*/
} NSpaceFldInfo;

typedef struct _NSpaceDimInfo
{
	char		nsd_Name[DC_MaxDimName];/* name of dimension	*/
	unsigned long	nsd_Size;		/* size of dimension	*/
	FieldId		nsd_Id;			/* Optional field id	*/
	unsigned short	nsd_Index;		/* index of the ADE	*/
} NSpaceDimInfo;

/* 
 * Names of dimensions are only required for fields defined with the
 * dc_NSDefineField() function without benefit (or overhead) of previous
 * calls to dc_NSDefineDimension().  If a dimension was specified with a
 * FieldId, the id will be stored in nsd_Id, else nsd_Id will be BadField.
 * For dimensions specified via a field id, the field name will be assumed
 * as the dimension name and stored in nsd_Name.  Field names which do not
 * fit in the space of nsd_Name[ DC_MaxDimName ] will be truncated to fit,
 * but the null terminator will be kept.  Dimension names will be used to
 * match dimensions.
 */

typedef struct _NSpaceDataChunk
{
	RawDataChunkPart	rawpart;
	TranspDataChunkPart	transpart;
	MetDataChunkPart	metpart;
	NSpaceDataChunkPart	nspacepart;
} NSpaceDataChunk;


/*------------------------------------------------------------------------
 * Semi-private routines.
 */
static DataChunk *NSCreate FP((DataChunk *dc));
static void NSDestroy FP((DataChunk *dc));
static void NSDump FP((DataChunk *dc));
static void NSSerialize FP((DataChunk *dc));
static void NSLocalize FP((DataChunk *dc));

/*------------------------------------------------------------------------
 * Initialization of class structure
 */
#define SUPERCLASS ((DataClassP)&MetDataMethods)
#define CLASSDEPTH 3

RawClass NSpaceMethods =
{
	DCID_NSpace,
	"NSpace",
	SUPERCLASS,		/* Superclass			*/
	CLASSDEPTH,		/* Class depth			*/
	NSCreate,
	NSDestroy,		/* Destroy			*/
	0,			/* Add				*/
	NSDump,			/* Dump				*/

	NSSerialize,		/* Serialize			*/
	NSLocalize,		/* Localize			*/

	sizeof (NSpaceDataChunk)
};

DataClassP DCP_NSpace = (DataClassP)&NSpaceMethods;

/*------------------------------------------------------------------------
 * Our class-specific AuxData structure types.
 *
 * Dimensions are stored in order of defn in the ADE of type ST_NSPACE_DIMNS.
 * Fields are stored in order of defn in the ADE of type ST_NSPACE_FIELDS.
 * Fields reference a dimension by its index in the array in ST_NSPACE_DIMNS.
 */
#define ST_NSPACE_FIELDS	2
#define ST_NSPACE_DIMNS		3

/*
 * Field bit flags
 */
#define NSF_STATIC	(1<<0)	/* present when field is static	 	*/
#define NSF_OFFSET	(1<<1)	/* present when static offset is valid 	*/

#define IsNSpace(dc,fn) \
	(dc_ReqSubClass (dc, DCP_NSpace, fn))

#define NSI(dc) (&((NSpaceDataChunk *)(dc))->nspacepart)
#define GetInfo(dc) NSI(dc)
#define GetFldInfo(dc) (NSI(dc)->ns_Fields)
#define GetDimInfo(dc) (NSI(dc)->ns_Dimns)

#define IsStatic(finfo) 	((finfo)->nsf_Flags & NSF_STATIC)
#define OffsetValid(finfo) 	((finfo)->nsf_Flags & NSF_OFFSET)

/*------------------------------------------------------------------------
 * Private routines
 */

static NSpaceDimInfo *DefineDimension FP((DataChunk *dc, NSpaceInfo *info,
					  char *name, FieldId id, 
					  unsigned long size, char *routine,
					  int warn_duplicates));
static NSpaceDimInfo *FindDimnByID FP((DataChunk *dc, NSpaceInfo *info,
				       FieldId id));
static NSpaceFldInfo *FindFieldByID FP((DataChunk *dc, NSpaceInfo *info,
					FieldId id, char *routine));
static NSpaceFldInfo *DefineField FP((DataChunk *dc, NSpaceInfo *info,
				      FieldId field, int ndims,
				      unsigned short *dim_indices, 
				      int is_static, char *routine));
static void SetFieldSizes FP((DataChunk *dc, char *routine));
static NSpaceFldInfo *AddDataCheck FP((DataChunk *dc, FieldId field,
				       int is_static, char *routine));
inline static int CheckStatic 
	FP((FieldId field, NSpaceFldInfo *finfo, int test, char *routine));
inline static int CheckOpen FP((NSpaceInfo *info, char *routine));

/*****************************************************************
// Public routines --- For defining, building, and inquiring
//		       NSpace DataChunks from applications
/----------------------------------------------------------------*/


/*-------------------------------------------- begin: dc_NSDefineDimension */
void
dc_NSDefineDimension(dc, field, size)
	DataChunk *dc; 
	FieldId field; 
	unsigned long size;
/*
 * The dimension will be referred to by its field id.  Optionally, a
 * variable may be defined for this field, implying that all variables
 * defined over this this dimension are indirectly indexed (e.g.
 * irregularly spaced).  Each dimension defined in a DataChunk must have a
 * unique name and FieldId.  If a dimension of the same name has already
 * been defined, the most recent definition, including field ID and size,
 * will override it.
 */
{
	NSpaceInfo *info;
	NSpaceDimInfo *dinfo;
	char *name;

	if (! IsNSpace(dc,"NSDefineDimension"))
		return;

	info = GetInfo(dc);
	if (! CheckOpen(info, "NSDefineDimension"))
		return;
/*
 * Use the existing name if we have a dimension with this ID already.  
 * Otherwise, get the name using F_GetName().  These will *not* always
 * be the same, especially for names like "alt" and "altitude" which may be
 * valid aliases for the same field/dimension.
 */
	if ((dinfo = FindDimnByID (dc, info, field)) != NULL)
		name = dinfo->nsd_Name;
	else
		name = F_GetName (field);
      
	(void) DefineDimension (dc, info, name, field, size, 
				"NSDefineDimension", TRUE);

}
/*---------------------------------------------- end: dc_NSDefineDimension */



/*--------------------------------------------- begin: dc_NSDefineVariable */
void
dc_NSDefineVariable( dc, field, ndims, dims, is_static )
DataChunk *dc; 
FieldId field;
int ndims;
FieldId *dims;
int is_static;
/*
 * Adds a variable, identified by the field id, to a class DCC_NSpace
 * DataChunk.  The variable will be defined over 'ndims' dimensions, where
 * the size of each dimension is obtained from the definition of each
 * FieldId id in dims[] in a previous call to dc_NSDefineDimension().  If
 * is_static is non-zero, the variable will not be implicitly defined over
 * time (i.e. data will be stored independent of sample numbers).  The
 * is_static flag is primarily intended for dimension variables whose
 * values do not change over the samples stored in the DataChunk.  It *is*
 * acceptable to define a variable with 0 dimensions, in which case the
 * data is exactly like the DCC_Scalar class.  Previous definitions of
 * a variable with the same name will be overridden by the most recent 
 * definition.
 */
{
	NSpaceInfo *info;
	NSpaceDimInfo *dinfo;
	int i;
	char *field_name;
	unsigned short dim_indices[ DC_MaxDimension ];

	if (! IsNSpace(dc, "NSDefineVariable"))
		return;

	info = GetInfo(dc);
	if (! CheckOpen(info, "NSDefineVariable"))
		return;

	/*
	 * Make sure we have a valid field id; it must have a non-empty name
	 */
	field_name = F_GetFullName (field);
	if (!field_name)
	{
		msg_ELog (EF_PROBLEM, 
			  "invalid field ID, %i, in NSDefineVariable()", 
			  field);
		return;
	}
	if (! strlen(field_name))
	{
		msg_ELog (EF_PROBLEM,
		  "%s: name of field ID, %i, must have non-zero length",
		  "NSDefineVariable", field);
	}

	/*
	 * Now loop through all of the requested dim ids, find the
	 * associated dinfo structure, and retrieve the index of the dimn ADE.
	 * If dimn ADE is not found, the variable definition fails.
	 */
	for (i = 0; i < ndims; ++i)
	{
		dinfo = FindDimnByID (dc, info, dims[i]);
		if (! dinfo)
		{
			msg_ELog (EF_PROBLEM,
			  "%s: dimn id %i not found for variable %s, id %i",
			  "NSDefineVariable", dims[i], field_name, field);
			return;
		}
		dim_indices[i] = dinfo->nsd_Index;
	}

	(void) DefineField (dc, info, field, ndims, dim_indices, is_static,
			    "NSDefineVariable");
}
/*-------------------------------------------- end: dc_NSDefineVariable ----*/




/*-------------------------------------------- begin: dc_NSDefineField -----*/
void
dc_NSDefineField(dc, field, ndims, dimnames, dimsizes, is_static)
	DataChunk *dc;
	FieldId field;
	int ndims;
	char **dimnames;
	unsigned long *dimsizes;
	int is_static;
/*
 * This is the simpler method of defining a field, intended for fields
 * whose dimensions will not be indexed by another field in the DataChunk.
 * This assumes that DFA_NetCDF will use the dimension names given when
 * defining the file, else the usefulness of the dimension variable
 * convention is lost.  Also, it assumes the programmer will pass the name
 * of another field in *dimnames if that field is meant to index a
 * dimension of the variable being defined.  It is up to the programmer to
 * define the variable for that field in a separate call to
 * dc_NSDefineField().  However, it is not clear from the interface that
 * there is (or can be) any correlation between a dimension of a variable
 * and another variable.  Lastly, it leaves it up to the programmer to make
 * sure all dimension names are unique, and when they are not it must be
 * assumed that like-named dimensions are identical, in which case the
 * sizes had better be equal.  Again, the 'like-named dimensions will be
 * considered identical' behavior is not apparent in the interface.  Sure,
 * it doesn't have to be, but likewise it doesn't hurt to be explicit.
 * Lastly, if the programmer intends to specify a field name as one of the
 * dimension names, but misspells the name, the interface cannot catch the
 * error.
 */
{
	NSpaceInfo *info;
	NSpaceDimInfo *dinfo;
	int i;
	char *field_name;
	unsigned short dim_indices[ DC_MaxDimension ];

	if (! IsNSpace(dc,"NSDefineField"))
		return;

	info = GetInfo(dc);
	if (! CheckOpen(info, "NSDefineField"))
		return;

	/*
	 * Test for valid field id and name
	 */
	field_name = F_GetFullName(field);
	if (!field_name || !strlen(field_name))
	{
		msg_ELog (EF_PROBLEM, 
		  "%s: field %i name must exist and have non-zero length",
		  "NSDefineField", field);
		return;
	}

	/*
	 * We must first define all of the dimensions listed; then
	 * we can actually define the field.
	 */
	for (i = 0; i < ndims; ++i)
	{
		dinfo = DefineDimension (dc, info, dimnames[i], 
					 F_Declared (dimnames[i]), dimsizes[i],
					 "NSDefineField", FALSE);
		if (! dinfo)
		{
			msg_ELog (EF_PROBLEM, 
				  "%s: aborting definition of field %i, '%s'",
				  "NSDefineField", field, field_name);
			return;
		}
		dim_indices[i] = dinfo->nsd_Index;
	}

	/*
	 * Dimensions all defined.  Pass on the rest of the work.
	 */
	(void) DefineField (dc, info, field, ndims, dim_indices, is_static,
			    "NSDefineField");
}
/*------------------------------------------- end: dc_NSDefineField --------*/



/*-------------------------------------------- begin: dc_NSDefineComplete --*/
void
dc_NSDefineComplete (dc)
	DataChunk *dc;
/*
 * Marks the definition as completed, which blocks any further
 * definition changes, and then calls SetFieldSizes() to calculate
 * the size of each field, as required by the data adding functions.
 * Does nothing if definition already complete.
 */
{
	NSpaceInfo *info;

	if (! IsNSpace(dc,"NSDefineComplete"))
		return;

	info = GetInfo(dc);
	if (! (info->ns_Defined))
	{
		SetFieldSizes (dc, "NSDefineComplete");
		info->ns_Defined = (unsigned char)TRUE;
	}
}
/*--------------------------------------------- end: dc_NSDefineComplete --*/




/*----------------------------------------- begin: dc_NSDefineIsComplete --*/
int
dc_NSDefineIsComplete (dc)
	DataChunk *dc;
{
	NSpaceInfo *info;

	if (! IsNSpace(dc,"NSDefineIsComplete"))
		return(0);

	info = GetInfo(dc);
	return ((int)(info->ns_Defined));
}
/*------------------------------------------- end: dc_NSDefineIsComplete --*/



/*------------------------------------------ begin: dc_NSAllowRedefine() --*/
void
dc_NSAllowRedefine(dc, allow)
	DataChunk *dc;
	int allow;
/*
 * Allow/disallow redefinition of variables or dimensions in the data chunk.
 */
{
	NSpaceInfo *info;

	if (! IsNSpace(dc,"NSAllowRedefine"))
		return;

	info = GetInfo(dc);
	info->ns_AllowRedefine = (unsigned char) allow;
}
/*-------------------------------------------- end: dc_NSAllowRedefine() --*/



/*-------------------------------------- begin: dc_NSRedefineIsAllowed() --*/
int
dc_NSRedefineIsAllowed(dc)
	DataChunk *dc;
{
	NSpaceInfo *info;

	if (! IsNSpace(dc,"NSRedefineIsAllowed"))
		return(0);

	info = GetInfo(dc);
	return ((int) info->ns_AllowRedefine);
}
/*---------------------------------------- end: dc_NSRedefineIsAllowed() --*/



/*--------------------------------------- begin: dc_NSGetAllDimensions() --*/
int
dc_NSGetAllDimensions(dc, names, fields, sizes)
	DataChunk *dc;
	char **names;
	FieldId *fields;
	unsigned long *sizes;
/*
 * Returns number of dimensions defined.  If fields is non-NULL, returns
 * the FieldId of each dimension in fields[], and if sizes is non-NULL,
 * returns size of each dimension in sizes[].  Likewise for ids.  If names
 * non-NULL, the location of each dimn name will be assigned to the
 * corresponding element of names[].  The memory pointed to is guaranteed
 * to exist as long as the DataChunk exists.  Do not try to free any of the
 * pointers returned in names[].
 *
 * Adequate space for the fields, names, and sizes array could be done with
 *
 * char *names[ DC_MaxDimension ];
 * FieldId fields[ DC_MaxDimension ];
 * unsigned long sizes[ DC_MaxDimension ];
 */
{
	NSpaceInfo *info;
	NSpaceDimInfo *dinfo;
	int i;

	if (! IsNSpace(dc,"NSGetAllDimensions"))
		return (0);

	/*
	 * Get the global info
	 */
	info = GetInfo(dc);
	if (!info)
	{
		msg_ELog (EF_PROBLEM, "NSGetAllDimensions: info not found");
		return (0);
	}

	/*
	 * If ids and sizes not being returned, just return with the number
	 */
	if (!sizes && !fields)
		return (info->ns_NDim);

	/*
	 * Otherwise we must loop through the dimensions copying info
	 */
	dinfo = GetDimInfo(dc);
	for (i = 0; i < info->ns_NDim; ++i)
	{
		if (sizes)
			sizes[i] = dinfo[i].nsd_Size;
		if (fields)
			fields[i] = dinfo[i].nsd_Id;
		if (names)
			names[i] = &(dinfo[i].nsd_Name[0]);
	}
	
	return (info->ns_NDim);
}	
/*------------------------------------------ end: dc_NSGetAllDimensions --*/



/*----------------------------------------- begin: dc_NSGetAllVariables --*/
int 
dc_NSGetAllVariables(dc, fields, ndims)
	DataChunk *dc;
	FieldId *fields;
	int *ndims;
/*
 * Returns number of variables in dc.  If fields is non-NULL, returns
 * field id of each variable in fields[].  If ndims is non-NULL, returns
 * number of dimensions in each variable in ndims[].
 *
 * To ensure adequate space for returned values, the following declarations
 * could be used:
 *
 * FieldId fields[ DC_MaxField ];
 * int ndims[ DC_MaxField ];
 */
{
	NSpaceInfo *info;
	NSpaceFldInfo *finfo;
	int i;

	if (! IsNSpace(dc,"NSGetAllVariables"))
		return (0);

	/*
	 * Get the global info
	 */
	info = GetInfo(dc);
	if (!info)
	{
		msg_ELog (EF_PROBLEM, "NSGetAllVariables: info not found");
		return (0);
	}

	/*
	 * If ids and ndims not being returned, 
	 * just return the number of fields
	 */
	if (!ndims && !fields)
		return (info->ns_NField);

	/*
	 * Otherwise we must loop through the variables copying info
	 */
	finfo = GetFldInfo(dc);
	for (i = 0; i < info->ns_NField; ++i)
	{
		if (ndims)
			ndims[i] = finfo[i].nsf_NDimn;
		if (fields)
			fields[i] = dc_FieldId (dc, i);
	}
	
	return (info->ns_NField);
}
/*----------------------------------------- end: dc_NSGetAllVariables -----*/



/*------------------------------------------------- begin: dc_NSGetField --*/
int 
dc_NSGetField (dc, field, ndims, names, sizes, is_static)
	DataChunk *dc;
	FieldId field;
	int *ndims;
	char **names;
	unsigned long *sizes;
	int *is_static;
/*
 * Information retrieval counterpart to NSDefineField().  For the specified
 * DataChunk and field, returns the field's number of dimensions, name and
 * size of each dimension, and whether field is static.  If any of the
 * reference parameters are NULL, no value will be returned.  The return
 * value of the function is nonzero (TRUE) iff no errors occur.
 *
 * The location of the each name will be assigned to the elements of
 * names[] if names is non-NULL.  The strings pointed to are guaranteed to
 * be valid as long as the DataChunk exists.  Do not try to free any of the
 * memory; it belongs to the datachunk.  To allocate adequate space for the
 * returned info, the simplest possibility is
 *
 * char *names[ DC_MaxDimension ];
 * unsigned long sizes[ DC_MaxDimension ];
 */
{
	NSpaceInfo *info;
	NSpaceFldInfo *finfo;
	NSpaceDimInfo *dinfo;
	unsigned short i;

	if (! IsNSpace(dc,"NSGetField"))
		return (0);

	info = GetInfo(dc);
	if (!(finfo = FindFieldByID (dc, info, field, "NSGetField")))
		return (0);

	if (ndims)
		*ndims = finfo->nsf_NDimn;
	if (is_static)
		*is_static = (int)(finfo->nsf_Flags & NSF_STATIC);
	if (!names && !sizes)
		return (TRUE);

	/*
	 * We have to traverse our dimensions and get info about them
	 */
	dinfo = GetDimInfo(dc);
	for (i = 0; i < finfo->nsf_NDimn; ++i)
	{
		if (names)
			names[i] = &(dinfo[ finfo->nsf_Dimns[i] ].nsd_Name[0]);
		if (sizes)
			sizes[i] = dinfo[ finfo->nsf_Dimns[i] ].nsd_Size;
	}
	return (TRUE);
}
/*--------------------------------------------------- end: dc_NSGetField --*/



/*------------------------------------------- begin: dc_NSIsStatic() ------*/
int
dc_NSIsStatic (dc, field)
	DataChunk *dc;
	FieldId field;
/*
 * Return non-zero iff field has been defined as static
 */
{
	NSpaceFldInfo *finfo;

	if (! IsNSpace(dc,"NSIsStatic"))
		return (0);

	if (!(finfo = FindFieldByID (dc, NULL, field, "NSIsStatic")))
		return (0);

	return (IsStatic(finfo));
}
/*---------------------------------------------- end: dc_NSIsStatic() -----*/



/*------------------------------------------- begin: dc_NSGetDimension() --*/
int 
dc_NSGetDimension (dc, dimn, name, size)
	DataChunk *dc;
	FieldId dimn;
	char **name;
	unsigned long *size;
/*
 * Return info about dimensions specified by id dimn.  No values returned in
 * reference parameters which are NULL.  Address of dimension name is stored
 * in *name.  The memory occupied by name exists until the DataChunk is 
 * destroyed; do not try to free it as it belongs to the datachunk.
 */
{
	NSpaceDimInfo *dinfo;

	if (! IsNSpace(dc,"NSGetDimension"))
		return FALSE;

	dinfo = FindDimnByID (dc, NULL, dimn);
	if (!dinfo)
	{
		msg_ELog (EF_PROBLEM, "%s: no dimension %i",
			  "NSGetDimension", dimn);
		return FALSE;
	}

	if (name)
		*name = &(dinfo->nsd_Name[0]);
	if (size)
		*size = dinfo->nsd_Size;
	return TRUE;
}
/*------------------------------------------------ end: dc_NSGetDimension -*/


	

/*----------------------------------------- begin: dc_NSFixedDimension() --*/
int 
dc_NSFixedDimension (details, ndetail, name, dindex)
	dsDetail *details;
	int ndetail;
	char *name;
	int *dindex;
/*
 * Look for DD_FIX_DIMENSION details and try to match them to the named
 * dimension.  On a match, return non-zero and the fixed index in *dindex.
 */
{
	int i;

	i = 0;
	while (i < ndetail)
	{
		if (!strcmp(details[i].dd_Name, DD_FIX_DIMENSION) &&
		    !strcmp(details[i].dd_V.us_v_ptr, name))
		{
			if (i+1 >= ndetail)
				*dindex = 0;
			else if (!strcmp(details[i+1].dd_Name, DD_FIX_INDEX))
				*dindex = details[i+1].dd_V.us_v_int;
			else
				*dindex = 0;
			return (TRUE);
		}
		++i;
	}
	return (FALSE);
}
/*---------------------------------------------- end: dc_NSFixedDimension -*/



/*----------------------------------------- begin: dc_NSFixedDetails() --*/
void
dc_NSFixedDetails (list, details, ndetail)
char *list;		/* comma-separated list of dimensions and indices*/
dsDetail *details;	/* array of details to fill			 */
int *ndetail;		/* return new number of details			 */
/*
 * Note that the details are only valid as long as the list memory is
 * valid and un-altered.  This function alters 'list'.
 */
{
	char *eq;
	int ndimns;
	char *dimns[ 2 * DC_MaxDimension ];
	int ndet;
	int i;

	ndet = 0;
	ndimns = CommaParse (list, dimns);
	for (i = 0; i < ndimns; ++i)
	{
		details[ndet].dd_Name = DD_FIX_DIMENSION;
		details[ndet].dd_V.us_v_ptr = dimns[i];
		++ndet;
		if ((eq = strrchr(dimns[i], '=')) != NULL)
		{
			*eq++ = '\0';
			details[ndet].dd_Name = DD_FIX_INDEX;
			details[ndet].dd_V.us_v_int = atoi(eq);
			++ndet;
		}
	}
	*ndetail = ndet;
}

	
/*---------------------------------------------- begin: dc_NSGetVariable --*/
int
dc_NSGetVariable (dc, field, ndims, dims, is_static)
	DataChunk *dc;
	FieldId field;
	int *ndims;
	FieldId *dims;
	int *is_static;
/*
 * Returns information about a specific variable.  If ndims non-NULL,
 * returns # dimensions in *ndims.  If dims non-NULL, returns array (of
 * length *ndims) of FieldId's, each defined as a dimension in the
 * DataChunk, over which variable is defined.  If this variable was defined
 * as static, *is_static will be non-zero.  Once the dim ids are known,
 * specifics about each dimension can be obtained with dc_NSGetDimension(),
 * assuming the dimensions were given an ID via dc_NSDefineDimensions().
 * If not defined through DefineDimension (i.e. DefineField was used), the
 * id will equal the BadField id, and the dimension info will have to be
 * retrieved through dc_NSGetField() or dc_NSGetAllDimensions().  Space for
 * the dims array can be allocated with FieldId dims[ DC_MaxDimension ];
 */
{
	NSpaceInfo *info;
	NSpaceFldInfo *finfo;
	NSpaceDimInfo *dinfo = NULL;
	unsigned short i;

	if (! IsNSpace(dc,"NSGetVariable"))
		return FALSE;

	info = GetInfo(dc);
	if (!(finfo = FindFieldByID (dc, info, field, "NSGetVariable")))
		return FALSE;

	if (ndims)
		*ndims = finfo->nsf_NDimn;
	if (is_static)
		*is_static = (int)(finfo->nsf_Flags & NSF_STATIC);
	if (! dims)
		return TRUE;

	if (finfo->nsf_NDimn)
		dinfo = GetDimInfo(dc);
	for (i = 0; i < finfo->nsf_NDimn; ++i)
	{
		dims[i] = dinfo[ finfo->nsf_Dimns[i] ].nsd_Id;
	}

	return TRUE;
}
/*---------------------------------------------- end: dc_NSGetVariable ----*/



/*------------------------------------------- begin: dc_NSOptimizeSpace ---*/
static void
dc_NSOptimizeSpace (dc, info)
DataChunk *dc;
NSpaceInfo *info;
/*
 * Set some optimization parameters in our superclasses if possible, and
 * reserve space for our static fields.  We have to wait until adding data
 * to do both of these functions since the field types may have changed
 * between DefineComplete and the addition of data.  This is called the first
 * time any data is added.
 *
 * We know each field size (number of elements) is fixed, so while 
 * calculating sizes, store the size in the sizes array.
 *
 * If all of the fields are dynamic and the same size, optimize our DC by
 * using uniform field organization.  Using uniform orgs when one of the
 * fields is static is a waste of space.
 *
 * If we cannot use uniform field organization, we can at least set fixed
 * field sizes with the static fields set to size zero, since they will
 * never be stored in a sample.  */
{
	int sizes[DC_MaxField];
	NSpaceFldInfo *finfo;
	FieldId *fields;
	int nfield;
	zbool uniform;
	unsigned long fsize = 0;
	int i;

	finfo = GetFldInfo(dc);
	uniform = TRUE;
	for (i = 0; i < info->ns_NField; ++i)
	{
		if (IsStatic(finfo+i))
		{
			finfo[i].nsf_StOffset = dc_ReserveStaticSpace 
				(dc, finfo[i].nsf_Size * 
				 dc_SizeOf (dc, dc_FieldId (dc, i)));
			sizes[i] = 0;
			uniform = FALSE;
			continue;
		}
		if ((i > 0) && (finfo[i].nsf_Size != fsize))
			uniform = FALSE;
		fsize = finfo[i].nsf_Size;
		sizes[i] = fsize;
	}
	if (uniform)
	{
		/*
		 * All the fields were dynamic and had the same number
		 * of elements, so the organization is uniform and fsize
		 * is the number of elements in that organization.
		 */
		dc_SetUniformOrg (dc, fsize);
	}
	else
	{
		/*
		 * The organization is not uniform, but the number of
		 * elements stored in a sample for each field will never
		 * change, so set those sizes here.
		 */
		fields = dc_GetFields (dc, &nfield); 
		dc_SetFieldSizes (dc, nfield, fields, sizes);
	}
	dc_BlockChanges (dc);
}
/*--------------------------------------------- end: dc_NSOptimizeSpace ---*/



static NSpaceFldInfo *
AddDataCheck (dc, field, is_static, routine)
DataChunk *dc;
FieldId field;
int is_static;
char *routine;
/*
 * Perform all the rudimentary checks and preparations for adding data.
 * Return NULL if the checks fail, pointer to finfo otherwise.
 */
{
	NSpaceInfo *info;
	NSpaceFldInfo *finfo;

	if (! IsNSpace (dc, routine))
		return (NULL);

	info = GetInfo(dc);
	if (!(finfo = FindFieldByID (dc, info, field, routine)))
		return (NULL);
	/*
	 * Make sure this field is dynamic or static
	 */
	if (!CheckStatic (field, finfo, is_static, routine))
		return (NULL);
	/*
	 * If this is the first addition of data to this datachunk,
	 * we must define our fields to our superclass first, and
	 * then we can add the data.
	 */
	if (! info->ns_Defined)
		dc_NSDefineComplete (dc);
	/*
	 * If this is the first addition of data, reserve static space
	 * and set optimizations.
	 */
	if (! info->ns_HaveData)
	{
		dc_NSOptimizeSpace (dc, info);
		info->ns_HaveData = TRUE;
	}
	return (finfo);
}



/*----------------------------------------------- begin: dc_NSAddSample ---*/
DataPtr
dc_NSAddSample(dc, when, sample, field, values)
	DataChunk *dc;
	ZebTime *when;
	int sample;
	FieldId field;
	void *values;
/*
 * Stores the array of 'values' at 'sample' associated with time 'when' for
 * variable 'field'.  The length of the values array equals the product of
 * the sizes of the dimensions over which the variable was defined.  The
 * array is in row-major order---the last dimension (the one defined at the
 * highest index into into the dims[] array) varies the fastest in linear
 * memory, following the interface used by netCDF.  The variable must have
 * been defined as dynamic.  The datachunks' Defined flag will be set.
 */
{
	NSpaceFldInfo *finfo;

	if (! (finfo = AddDataCheck (dc, field, FALSE, "NSAddSample")))
		return (NULL);
	/*
	 * Now add the data, retrieving the size from the finfo structure.
	 */
	return (dc_AddMData (dc, when, field, ((finfo->nsf_Size) * 
					       dc_SizeOf (dc, field)),
			     sample, /*nsample*/ 1, values));
}
/*---------------------------------------------------- end: dc_NSAddSample --*/



/*------------------------------------------- begin: dc_NSAddMultSamples ----*/
DataPtr
dc_NSAddMultSamples(dc, when, begin, nsample, field, values)
	DataChunk *dc;
	ZebTime *when;		/* an array of times, one per sample	*/
	int begin;		/* starting sample index	 	*/
	int nsample;		/* number of samples to add		*/
	FieldId field;
	void *values;
/*
 * Just like dc_NSAddSample, except more than one sample can be added.
 * The data in the values array should be in sample-major order, meaning
 * the data for sample i, for begin <= i < begin+nsample, begins at 
 *
 * values + ((i - begin) * finfo->nsf_Size * dc_SizeOf(dc, field))
 */
{
	NSpaceFldInfo *finfo;

	if (! (finfo = AddDataCheck (dc, field, FALSE, "NSAddMultSamples")))
		return (NULL);
	/*
	 * Now add the data, retrieving the size from the finfo structure.
	 */
	return (dc_AddMData (dc, when, field, ((finfo->nsf_Size) * 
					       dc_SizeOf (dc, field)),
			     begin, nsample, values));
}
/*---------------------------------------------- end: dc_NSAddMultSamples ---*/



/*------------------------------------------- begin: dc_NSAddMissing --------*/
void
dc_NSAddMissing (dc, when, begin, nsample, field)
	DataChunk *dc;
	ZebTime *when;		/* an array of times, one per sample	*/
	int begin;		/* starting sample index	 	*/
	int nsample;		/* number of samples to add		*/
	FieldId field;
/*
 * Fill multiple samples with missing data.
 */
{
	NSpaceFldInfo *finfo;

	if (! (finfo = AddDataCheck (dc, field, FALSE, "NSAddMissing")))
		return;
	/*
	 * Now fill the data.
	 */
	dc_FillMissing (dc, when, field, 
			((finfo->nsf_Size) * dc_SizeOf (dc, field)),
			begin, nsample);
}
/*---------------------------------------------- end: dc_NSAddMissing -------*/



/*-------------------------------------------------- begin: dc_NSAddStatic --*/
DataPtr
dc_NSAddStatic (dc, field, values)
	DataChunk *dc;
	FieldId field;
	void *values;
/*
 * Same as above except no time or sample is associated with the data.  The
 * field must have been defined as a static variable.  Each successive call
 * overwrites any previously stored data.  Static data will be added directly
 * to the raw data chunk since it is not associated with any particular
 * sample.  Note that the static fields are still lumped into the fields
 * definition for the MetData class, but no data is ever stored for the
 * fields via dc_AddMData().
 */
{
	NSpaceFldInfo *finfo;
	char *dest;

	if (! (finfo = AddDataCheck (dc, field, TRUE, "NSAddStatic")))
		return (NULL);
	/*
	 * We already have space reserved, so write to it and validate the
	 * offset.  Even if no values are being copied now, the offset
	 * must be validated in case data are added directly.  Until then,
	 * any attempts to get this data will retrieve garbage.
	 */
	dest = (char *)dc->dc_Data + finfo->nsf_StOffset;
	finfo->nsf_Flags |= NSF_OFFSET;
	if (values)
	{
		memcpy (dest, (char *) values, 
			(finfo->nsf_Size * dc_SizeOf (dc, field)));
	}
	return ((DataPtr)dest);
}
/*------------------------------------------------- end: dc_NSAddStatic ---*/



/*------------------------------------------------ begin: dc_NSFillStatic -*/
void
dc_NSFillStatic (dc, field)
	DataChunk *dc;
	FieldId field;
/*
 * Same as above except fill with the field's bad value.
 */
{
	NSpaceFldInfo *finfo;
	unsigned char *dest;

	if (! (finfo = AddDataCheck (dc, field, TRUE, "NSFillStatic")))
		return;
	/*
	 * Let MetData method fill the space with bad values.
	 */
	dest = (unsigned char *)dc->dc_Data + finfo->nsf_StOffset;
	dc_MetFillMissing (dc, field, dest, finfo->nsf_Size);
	finfo->nsf_Flags |= NSF_OFFSET;
}
/*------------------------------------------------- end: dc_NSFillStatic ---*/

/*
 * Note that there is, as yet, no intention to include the equivalent to
 * the netCDF interface's hyperslab access in the DCC_NSpace interface.
 * But it should be easily implemented by just filling new samples with
 * BadValues before storing the hyperslab of data.  Theoretically though,
 * this is incorrect since the values not in the hyperslab would be
 * *missing* rather than *bad*.  A hyperslab interface would require two
 * more functions, one for static and one for dynamic.
 */

/* ---- Data Retrieval ---- */


/*---------------------------------------------- begin: dc_NSGetSample ----*/
void *
dc_NSGetSample(dc, sample, field, size)
	DataChunk *dc;
	int sample;
	FieldId field;
	unsigned long *size;
/*
 * Returns pointer to field's data at sample.  If size non-NULL, returns
 * length of data array (number of elements) in *size.  Array is in row-major
 * order, just as it was stored.  If the data is not found, NULL is returned.
 *
 * GJG 1/28/94: Allow static variable data to be returned by this function.
 * Static data will be the same for every sample.  However, if the sample
 * number is invalid, then an error is logged and NULL returned.  We don't
 * have to check sample number for dynamic fields since GetMData does it
 * for us.
 */
{
	int len;
	DataPtr data;
	NSpaceFldInfo *finfo;

	if (! IsNSpace(dc,"NSGetSample"))
		return NULL;

	if (!(finfo = FindFieldByID (dc, NULL, field, "NSGetSample")))
		return NULL;

	data = NULL;
	if (!IsStatic (finfo))
		data = dc_GetMData (dc, sample, field, &len);
	else if ((sample < 0) || (sample >= dc_GetNSample(dc)))
		msg_ELog (EF_PROBLEM, "NSGetSample: %s, sample %d invalid",
			  "static retrieval", sample);
	else if (OffsetValid(finfo))
		data = (DataPtr) ((char *)dc->dc_Data + finfo->nsf_StOffset);

	if (size && data)
		*size = (unsigned long) (finfo->nsf_Size);
	else if (size)
		*size = 0;
	return ((void *)data);
}
/*------------------------------------------------ end: dc_NSGetSample ----*/



/*---------------------------------------------- begin: dc_NSGetStatic ----*/
void *
dc_NSGetStatic (dc, field, size)
	DataChunk *dc;
	FieldId field;
	unsigned long *size;
/*
 * Basically, same as dc_NSGetSample() except sample parameter not required.
 * Returns pointer to field's data at sample.  If size non-NULL, returns
 * length of data array (number of elements) in *size.  Array is in row-major
 * order, just as it was stored.  If the data is not found or there is an
 * error, NULL is returned.
 */
{
	DataPtr data;
	NSpaceFldInfo *finfo;

	if (! IsNSpace(dc,"NSGetStatic"))
		return NULL;

	if (!(finfo = FindFieldByID (dc, NULL, field, "NSGetStatic")))
		return NULL;

	/*
	 * Make sure this field is static
	 */
	if (!CheckStatic (field, finfo, TRUE, "NSGetStatic"))
		return NULL;

	/*
	 * Find our data, if there is any, and return a pointer to it
	 */
	if (OffsetValid(finfo))
	{
		data = (DataPtr) ((char *)dc->dc_Data + finfo->nsf_StOffset);
		if (size)
			*size = (unsigned long) (finfo->nsf_Size);
		return ((void *)data);
	}
	else
	{
		if (size)
			*size = 0;
		return NULL;
	}
}
/*------------------------------------------------ end: dc_NSGetStatic ----*/




/*****************************************************************
// Semi-private routines --- called through our datachunk class
//			     methods stucture
/----------------------------------------------------------------*/


/*--------------------------------------------------- begin: NSCreate() */
static DataChunk *
NSCreate (dc)
DataChunk *dc;
/*
 * Create a chunk of this class.
 */
{
	NSpaceInfo *info = NSI(dc);
/*
 * Initalize our global info structure
 */
	info->ns_NField = 0;
	info->ns_NDim = 0;
	info->ns_Defined = FALSE;
	info->ns_HaveData = FALSE;
	info->ns_AllowRedefine = FALSE;
	info->ns_Fields = NULL;
	info->ns_Dimns = NULL;
	return (dc);
}
/*-------------------------------------------------------- end: NSCreate() */




/*---------------------------------------------------- begin: NSLocalize() */
static void
NSLocalize (dc)
DataChunk *dc;
/*
 * Re-attach our ADEs
 */
{
	NSpaceInfo *info = NSI(dc);

	/*
	 * The NField and NDim info members should still be consistent,
	 * so we only set the pointers.
	 */
	info->ns_Fields = (NSpaceFldInfo *) 
		dc_FindADE (dc, DCP_NSpace, ST_NSPACE_FIELDS, 0);
	info->ns_Dimns =  (NSpaceDimInfo *) 
		dc_FindADE (dc, DCP_NSpace, ST_NSPACE_DIMNS, 0);
}
/*------------------------------------------------------ end: NSLocalize() */




/*--------------------------------------------------- begin: NSSerialize() */
static void
NSSerialize (dc)
DataChunk *dc;
/*
 * Add our pointers to dynamic data as ADEs.
 */
{
	NSpaceInfo *info = NSI(dc);

	if (info->ns_Fields)
		dc_AddADE (dc, (DataPtr)info->ns_Fields, DCP_NSpace,
			   ST_NSPACE_FIELDS, 
			   info->ns_NField * sizeof(NSpaceFldInfo), FALSE);
	if (info->ns_Dimns)
		dc_AddADE (dc, (DataPtr)info->ns_Dimns, DCP_NSpace,
			   ST_NSPACE_DIMNS, 
			   info->ns_NDim * sizeof(NSpaceDimInfo), FALSE);
}
/*----------------------------------------------------- end: NSSerialize() */




/*----------------------------------------------------- begin: NSDestroy() */
static void
NSDestroy (dc)
DataChunk *dc;
/*
 * Free our pointers to dynamic data.
 */
{
	NSpaceInfo *info = NSI(dc);

	if (info->ns_Fields)
		free (info->ns_Fields);
	if (info->ns_Dimns)
		free (info->ns_Dimns);
	info->ns_Fields = NULL;
	info->ns_Dimns = NULL;
}
/*------------------------------------------------------- end: NSDestroy() */




/*------------------------------------------------------- begin: NSDump() -*/
static void
NSDump (dc)
	DataChunk *dc;
/*
 * Our dump method --- we need to see our complicated structures for
 * debugging purposes, and for the sake of applications programmers.
 * Rather than show field sizes as zero until closing the definition,
 * we'll go ahead and do the calculations so that people can see what
 * we've got so far.
 */
{
	NSpaceInfo *info;
	NSpaceFldInfo *finfo;
	NSpaceDimInfo *dinfo;
	unsigned short j;
	int i;

	if (! IsNSpace(dc,"NSDump"))
		return;

	info = GetInfo(dc);
	if (!info)
	{
		msg_ELog (EF_PROBLEM, "NSDump: info not found");
		return;
	}

	finfo = GetFldInfo(dc);
	dinfo = GetDimInfo(dc);

	if (! info->ns_Defined)
		SetFieldSizes (dc, "NSDump");

	printf ("NSPACE class: definition %s",
		(info->ns_Defined) ? "completed" : "open");
	/*
	 * We'll follow suit with MetData's dump and not print much
	 * if we're completely empty.
	 */
	if ((info->ns_NField == 0) && (info->ns_NDim == 0))
	{
		printf (", nothing defined\n");
		return;
	}

	if ( !info->ns_Defined )
		printf (", re-definitions %s", (info->ns_AllowRedefine) ?
			"allowed" : "cause warnings");

	printf ("\nNumber of variables: %-5i    Number of dimensions: %-5i\n",
		info->ns_NField, info->ns_NDim);
	/*
	 * Print all info about each dimensions, and remember its ADE * for
	 * reference when printing out our fields
	 */
	for (i = 0; i < info->ns_NDim; ++i)
	{
		printf("   %35s: size %-6lu  -- ", 
		       dinfo[i].nsd_Name, dinfo[i].nsd_Size);
		if (dinfo[i].nsd_Id != BadField)
			printf("field '%s'\n", F_GetDesc(dinfo[i].nsd_Id));
		else
			printf("no field id\n");
	}
	/*
	 * Print each field, in the form: 
	 * NAME ( DIM1, DIM2, ... ) [static] 'DESC'
	 * The dim name is retrieved from the ADE * list in dinfo[].
	 */
	for (i = 0; i < info->ns_NField; ++i)
	{
		printf("  %s %s ( ", (info->ns_Defined) ? 
		       dc_TypeName (dc_IndexType(dc, i)) : "",
		       F_GetFullName(dc_FieldId (dc, i)));
		for (j = 0; j < finfo->nsf_NDimn; ++j)
			printf("%s%s", dinfo[finfo->nsf_Dimns[j]].nsd_Name,
			       (j == (finfo->nsf_NDimn - 1)) ? "" : ", ");
		printf(" )");
		if (IsStatic(finfo))
			printf(" static, offset=%ld, ", finfo->nsf_StOffset);
		printf("size = %lu, ", finfo->nsf_Size);
		printf("'%s'\n", F_GetDesc(dc_FieldId(dc, i)));
		++finfo;
	}
	/* 
	 * Done NSDump().
	 */
}
/*------------------------------------------------------- end: NSDump() --*/



/*****************************************************************
// Private routines --- for manipulating NSpace datachunks
/----------------------------------------------------------------*/

/*------------------------------------------------ begin: DefineDimension() */
static NSpaceDimInfo *
DefineDimension(dc, info, name, field, size, routine, warn_duplicates)
	DataChunk *dc;
	NSpaceInfo *info;
	char *name;
	FieldId field;
	unsigned long size;
	char *routine;		/* Name of public routine, in case of errors */
	int warn_duplicates;	/* Warn of duplicates; changes always noted  */
/*
 * Insert the specified dimension into the datachunk.  If id is unspecified,
 * it should be equal to BadField.  This function takes care of finding
 * duplicate dimension names and the like.  On return, the dimension has been
 * added or re-defined, and a pointer to the dimension's info structure is
 * returned.  On error, NULL returned.
 */
{
	NSpaceDimInfo *dinfo;
	char dim_name [ DC_MaxDimName ];
	int i;

	/*
	 * If the field id is not BadField, make sure it is a valid field id; 
	 * Also, field must have a non-empty name.
	 */
	if ((field != BadField) && !F_GetFullName(field))
	{
		msg_ELog (EF_PROBLEM, 
			  "%s: invalid field ID, %i", routine, field);
		return NULL;
	}
	if (!name || !strlen(name))
	{
		msg_ELog (EF_PROBLEM,
		  "%s: name of field must exist and have non-zero length",
		  routine);
		return NULL;
	}

	/*
	 * As names which are too long will get trunc'ed, we need to 
	 * use the trunc'ed name to search for a duplicate.
	 */
	if (strlen(name) >= (unsigned) DC_MaxDimName)
		msg_ELog (EF_PROBLEM, 
			  "%s: dimn name '%s' longer than %i, truncated",
			  routine, name, (DC_MaxDimName - 1));
	strncpy (dim_name, name, DC_MaxDimName);
	dim_name[DC_MaxDimName - 1] = '\0';

	/*
	 * Search for a dimension with the given name: if found, verify
	 * size and field id identical to original, else spout warnings
	 * and re-define it.  If original did not have an id, add the one
	 * given here.
	 */
	dinfo = GetDimInfo(dc);
	for (i = 0; i < info->ns_NDim; ++i)
	{
		if (!strcmp(dinfo[i].nsd_Name, dim_name))
			break;				/* match found */
	}

	if (i < info->ns_NDim)
	{
		/*
		 * A dimension is being re-defined; complain if this is
		 * undesirable, but override all previous settings
		 */
		if (!info->ns_AllowRedefine && warn_duplicates)
			msg_ELog (EF_PROBLEM, 
				  "%s: dimension %s being re-defined",
				  routine, dim_name);

		if (!info->ns_AllowRedefine && dinfo[i].nsd_Id != field)
		{
			if (field != BadField)
				msg_ELog (EF_PROBLEM, 
					  "%s: dimn %s given new id %i",
					  routine, dim_name, field);
			else
				msg_ELog (EF_PROBLEM,
					  "%s: dimn %s id removed",
					  routine, dim_name);
		}
	}
	else	/* i == info->ns_NDim */
	{
		/* 
		 * New dimension gets whole new entry, but only if 
		 * there's room.
		 */
		if (info->ns_NDim == DC_MaxDimension)
		{
			msg_ELog (EF_PROBLEM, 
				  "%s: no more room for dimn %s, limit = %i",
				  routine, dim_name,
				  DC_MaxDimension);
			return NULL;
		}
		msg_ELog (EF_DEVELOP, "%s: dimn %s, size %lu, id %i (%s)",
			  routine, dim_name, size, field, 
			  (field == BadField) ? 
			  "id not used" : F_GetFullName(field));
		if (!dinfo)
		{
			dinfo = ALLOC(NSpaceDimInfo);
		}
		else
		{
			int len = (info->ns_NDim+1)*sizeof(NSpaceDimInfo);
			dinfo = (NSpaceDimInfo *)realloc(dinfo, len);
		}
		info->ns_Dimns = dinfo;
		++(info->ns_NDim);
	}

	/*
	 * Now stash the dimension info.
	 */
	dinfo[i].nsd_Size = size; 
	dinfo[i].nsd_Id = field;
	strcpy (dinfo[i].nsd_Name, dim_name);
	dinfo[i].nsd_Index = i;

	/*
	 * We have either added or updated a dimension, and the
	 * dimn info has been updated in-place.
	 * Return a pointer to the dimn info.
	 */
	return (dinfo+i);
}
/*-------------------------------------------------- end: DefineDimension() */



/*-------------------------------------------------- begin: FindDimnByID() -*/
static NSpaceDimInfo *
FindDimnByID (dc, info, id)
	DataChunk *dc;
	NSpaceInfo *info;
	FieldId id;
/*
 * Return pointer to a dimn info structure for the dimn with specified id.
 * Returns NULL if dimn not found.
 */
{
	NSpaceDimInfo *dinfo;
	int i;

	if (!info)
		info = GetInfo(dc);
	dinfo = GetDimInfo(dc);
	for (i = 0; i < info->ns_NDim; ++i)
	{
		if (dinfo[i].nsd_Id == id)
			return (dinfo+i);
	}
	return NULL;
}
/*---------------------------------------------------- end: FindDimnByID() -*/



/*------------------------------------------------- begin: FindFieldByID() -*/
static NSpaceFldInfo *
FindFieldByID (dc, info, id, routine)
	DataChunk *dc;
	NSpaceInfo *info;
	FieldId id;
	char *routine;
/*
 * Return pointer to a field info structure for the field with specified id.
 * Returns NULL if field not found.  If we've already completed definition,
 * then we revert to the MetData method to get the index, which is faster.
 */
{
	NSpaceFldInfo *finfo = GetFldInfo (dc);
	int i;

	if ((i = dc_GetFieldIndex (dc, id)) < 0)
	{
		char *f = F_GetFullName (id);
		msg_ELog (EF_PROBLEM, "%s: field %s, id %d not in datachunk",
			  routine, (f) ? f : "unknown", id);
		return NULL;
	}
	return (finfo+i);
}
/*-------------------------------------------------- end: FindFieldByID() -*/




/*-------------------------------------------------- begin: DefineField() -*/
static NSpaceFldInfo *
DefineField (dc, info, field, ndims, dim_indices, is_static, routine)
	DataChunk *dc;
	NSpaceInfo *info;
	FieldId field;
	int ndims;
	unsigned short *dim_indices;
	short is_static;
	char *routine;		/* Name of caller, for error messages */
/*
 * Define the field as specified.  Overwrites any existing field with the
 * same ID, but not without sending a warning.
 */
{
	NSpaceFldInfo *finfo;
	char *field_name = F_GetFullName(field);
	int i, j;

	if (info->ns_Defined)
	{
		msg_ELog (EF_PROBLEM,
		  "%s: datachunk has data, no more definitions allowed",
		  routine);
		return NULL;
	}

	finfo = GetFldInfo(dc);
	i = dc_GetFieldIndex (dc, field);
	if (i >= 0)
	{
		/*
		 * A variable is being re-defined; complain if redefinitions
		 * are undesirable, but change the field ADE to reflect the
		 * new definition
		 */
		if (!info->ns_AllowRedefine)
		{
			msg_ELog (EF_PROBLEM, 
				  "%s: field %s being re-defined",
				  routine, field_name);

			if (finfo[i].nsf_NDimn != ndims)
				msg_ELog (EF_PROBLEM,
					  "%s: field %s now has %lu dimns",
					  routine, field_name, ndims);

			if (IsStatic(finfo+i) && (!is_static))
				msg_ELog (EF_PROBLEM,
					  "%s: field %s has changed to %s",
					  routine, field_name, 
					  (is_static) ? "static" : "dynamic");
		}
		/*
		 * The ADE is updated below
		 */
	}
	else /* a new field must be added */
	{
		i = info->ns_NField;
		/*
		 * Create space in field ADE and fill it in, if space allows
		 */
		if (info->ns_NField == DC_MaxField)
		{
			msg_ELog (EF_PROBLEM, 
			  "%s: no more room for field %s, limit = %i",
			  routine, field_name,
			  DC_MaxField);
			return NULL;
		}
		msg_ELog (EF_DEVELOP, "%s: defining field %s, id %i",
			  routine, field_name, field);
		if (!finfo)
		{
			finfo = ALLOC(NSpaceFldInfo);
		}
		else
		{
			int len = (info->ns_NField+1)*sizeof(NSpaceFldInfo);
			finfo = (NSpaceFldInfo *) realloc (finfo, len);
		}
		info->ns_Fields = finfo;
		++(info->ns_NField);
		dc_AddFields (dc, 1, &field);
	}
	/*
	 * Set the basic stuff in the ADE
	 */
	finfo[i].nsf_NDimn = ndims;
	finfo[i].nsf_Size = 0;
	finfo[i].nsf_Flags = (is_static) ? NSF_STATIC : 0;
	finfo[i].nsf_StOffset = 0;

	/*
	 * Now copy dimension list into field ADE.  Note that calculating
	 * field size is held off until definition is complete.
	 */
	for (j = 0; j < ndims; ++j)
	{
		finfo[i].nsf_Dimns[j] = dim_indices[j];
	}

	/*
	 * All done.
	 */
	return (finfo+i);
}
/*--------------------------------------------------- end: DefineField() -*/



/*------------------------------------------------ begin: SetFieldSizes --*/
static void
SetFieldSizes (dc, routine)
	DataChunk *dc;
	char *routine;		/* name of calling routine */
/*
 * For each field, set its Size to the product of the sizes of
 * each of its dimensions.
 */
{
	int i;
	unsigned short j;
	unsigned long size;
	NSpaceInfo *info = GetInfo(dc);
	NSpaceFldInfo *finfo = GetFldInfo (dc);
	NSpaceDimInfo *dinfo = GetDimInfo (dc);


	for (i = 0; i < info->ns_NField; ++i)	/* field loop */
	{
		size = 1;
		for (j = 0; j < finfo->nsf_NDimn; ++j)	/* dimn loop */
		{
			size *= dinfo[ finfo->nsf_Dimns[j] ].nsd_Size;
		}					/* dimn loop */
		finfo->nsf_Size = size;
		msg_ELog (EF_DEVELOP, "%s: field %s, size set to %lu",
			  routine, F_GetFullName(dc_FieldId (dc, i)), size);
		++finfo;
	}					/* field loop */
}
/*------------------------------------------------- end: SetFieldSizes --*/



/*---------------------------------------------- begin: CheckStatic -----*/
inline static int
CheckStatic(field, finfo, test, routine)
	FieldId field;
	NSpaceFldInfo *finfo;
	int test;
	char *routine;
/*
 * Make sure this field is in fact what's wanted, static or dynamic,
 * as determined by test. Static flag should match value of test.
 * Returns nonzero if test passes, zero if fails.
 */
{
	if (((finfo->nsf_Flags & NSF_STATIC) && !test) || 
	    (!(finfo->nsf_Flags & NSF_STATIC) && test))
	{
		msg_ELog (EF_PROBLEM, "%s: field %s, id %i, not %s", routine, 
			  F_GetFullName(field), field, 
			  (test) ? "static" : "dynamic");
		return (FALSE);
	}
	return (TRUE);
}
/*----------------------------------------------- end: CheckStatic -----*/



/*---------------------------------------------- begin: CheckOpen -----*/
inline static int
CheckOpen(info, routine)
	NSpaceInfo *info;
	char *routine;
/*
 * Make sure our definition is still open before allowing any further
 * definition.  Log a message if not.  Return TRUE if things o.k.
 */
{
	if (info->ns_Defined)
	{
		msg_ELog (EF_PROBLEM, 
			  "%s: defn closed, no more definitions allowed",
			  routine);
		return (FALSE);
	}
	return (TRUE);
}
/*----------------------------------------------- end: CheckOpen -------*/
