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
 * the netCDF data model and the use of 'dimension variables.'  See the
 * netCDF documentation for a definition of dimension variables.
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
 * We have to hold off setting up our field info with MetData's
 * dc_SetupFields() until we know we have all the fields and dimensions
 * defined (which is the first time an attempt is made to add data).
 * So our required auxiliary field and dimension info will be stored local
 * to our class until data is stored, at which time the info will be
 * passed on to MetData to construct our storage space.
 *
 * On converting this class to supporting multiple data types:
 *------------------------------------------------------------------------
 * All of the field and dimension info is independent of a data size or
 * type.  At present floating point data is a constraint of the MetData
 * superclass.  Theoretically this class could be an immediate subclass of 
 * Transparent, and type and size constraints could be specified through
 * subclasses of this class.  Combining multiple types in one chunk might
 * be possible, but it would violate the whole reason for having the
 * DataClass hierarchy and implementation in the first place.  A more
 * appropriate approach would be designing a composite datachunk class which
 * can 'contain' several instances of different class datachunks.  Then
 * the DFA interface would have to know how to handle composite data chunks
 * in order to combine different data chunk classes in the same file.
 *
 * For a quick fix, add the data type size info to the field info structure,
 * and use this value when determining the size of the array being passed
 * to dc_AddMData() in dc_NSAddSample() and dc_NSAddStatic().  Then add the
 * data type size to the interface.  That's it.
 *
 * Possible Future Optimizations:
 *-------------------------------------------------------------------------
 * The internal, private functions should pass the general info structure
 * between them so that it need only be retrieved from the ADE list once,
 * upon entry to one of the public interface functions.  And it probably
 * wouldn't hurt to prefix the private function names with `_' or `ns_'.
 *
 * A local, static variable could be added holding a pointer to a DC and
 * the info for the DC.  Functions called on the same DC can use the stored
 * info reference rather than re-retrieving, a cache of sorts.
 *
 * The Complete and IsComplete public interfaces should retrieve info and
 * then call private versions.  Then other functions, like adding data, can
 * call the private functions without incurring the overhead of re-finding
 * the info.  Of course, I don't know what the overhead may be for
 * finding the info ADE, so I don't know if it would be worth it to 
 * optimize it. [ In the case of IsComplete, _IsComplete(dc, info) could
 * be just a macro. ]
 *
 * Further Ideas
 *-------------------------------------------------------------------------
 * It would be nice if message were printed noting the class of datachunk
 * being operated on at time of message.  When NSpace calls a MetData method,
 * the method should report the message as if from an NSpace method by 
 * noting that the class of the chunk is NSpace.
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

#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "ds_fields.h"
#include "DataChunkP.h"
MAKE_RCSID ("$Id: dc_NSpace.c,v 1.1.1.1 1993-04-02 00:18:05 granger Exp $")

/*
 * The DCC_NSpace public interface
 */

#ifdef NSPACE_TEST /* actually prototyped in DataStore.h */

#define DC_MaxDimension		32	/* Maximum number of dimensions */
#define DC_MaxDimName		32	/* Max name length, incl \0	*/

/*-------------------------------------------------------------------------
 * Guidelines to the NSpace Interface
 *-------------------------------------------------------------------------
 * Numerical and Boolean parameters, except for the size of a dimension,
 * will be ints.  Dimension sizes and lengths of data arrays will always be
 * unsigned long.  The 'length' of a data array is the number of floating
 * point elements.  This will be true for both definition and information
 * retrieval.  For retrieval, strings will be returned as pointers to
 * memory inside the datachunk.  The memory pointed to will be valid for
 * the life of the datachunk, but it SHOULD NOT be freed or modified.  Any
 * parameters being passed by reference to hold return values can be passed
 * as NULL, in which case nothing will be returned for that parameter.
 *
 * Interface functions from the NSpace parent class, MetData, can also be
 * useful, but only for retrieving information and data, and only once
 * definition has been completed.  Definition is marked completed by adding
 * data to any of the fields; or, definition can be forced to completion by
 * calling dc_NSDefineComplete(dc).  Completion of the definition can be
 * tested with dc_NSDefineIsComplete(dc).  Once definition is completed, it
 * cannot be re-opened.  Some potentially useful MetData functions:
 *
 * dc_GetNField (dc) 	-- For getting the number of fields (aka variables).
 * dc_GetFields (...)	-- For getting the fields and their field IDs.
 * dc_GetMData (...)	-- For retrieving data from a dynamic field at a
 *			   particular sample as an opaque block of bytes.
 *			   There is no way to retrieve static field data
 *			   outside of the NSpace interface.
 * dc_SetBadval(...), 
 * dc_GetBadval (...)	-- Bad values associated with fields (aka variables);
 *			   the bad value can only be set once definition is
 *			   complete.
 *
 * See the DataChunk documentation for specifics about the MetData functions.
 *---------------------------------------------------------------------------*/

/*
 * NSpace Definition interface
 */
void dc_NSDefineField FP((DataChunk *dc, FieldId field, int ndims, 
			  char **dimnames,
			  unsigned long *dimsizes,
			  int is_static));
void dc_NSDefineDimension FP((DataChunk *dc, FieldId field, 
			      unsigned long size));

void dc_NSDefineVariable FP((DataChunk *dc, FieldId field, 
			     int ndims, FieldId *dims,
			     int is_static));
void dc_NSDefineComplete FP((DataChunk *dc));
int dc_NSDefineIsComplete FP((DataChunk *dc));
/*
 * NSpace Information retrieval
 */
int dc_NSGetAllDimensions FP((DataChunk *dc, char **names, FieldId *fields, 
			      unsigned long *sizes));
int dc_NSGetAllVariables FP((DataChunk *dc, FieldId *fields, int *ndims));
int dc_NSGetField FP((DataChunk *dc, FieldId field, int *ndims, 
		      char **names, unsigned long *sizes, int *is_static));
void dc_NSGetDimension FP((DataChunk *dc, FieldId dimn, char **name,
			   unsigned long *size));
void dc_NSGetVariable FP((DataChunk *dc, FieldId field, int *ndims, 
			  FieldId *dims, int *is_static));
int dc_NSIsStatic FP((DataChunk *dc, FieldId field));
/*
 * NSpace Data addition
 */
void dc_NSAddSample FP((DataChunk *dc, ZebTime *when, int sample, 
			FieldId field, float *values));
void dc_NSAddStatic FP((DataChunk *dc, FieldId field, float *values));
/*
 * NSpace Data retrieval
 */
float *dc_NSGetSample FP((DataChunk *dc, int sample, FieldId field, 
			  unsigned long *size));
float *dc_NSGetStatic FP((DataChunk *dc, FieldId field, unsigned long *size));

#endif


/*
 * Semi-private routines.
 */
static DataChunk *NSCreate FP((DataClass));
static void NSDump FP((DataChunk *dc));

/*
 * Initialization of class structure
 */
#define SUPERCLASS DCC_MetData

RawDCClass NSpaceMethods =
{
	"NSpace",
	SUPERCLASS,		/* Superclass			*/
	NSCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	NSDump			/* Dump				*/
};


/*
 * Our class-specific AuxData structure types.
 */
#define ST_NSPACE_INFO	1000	/* Our "global" info		*/

/*
 * Dimensions are stored in order of definition starting at ST_NSPACE_DIMNS.
 * Fields are stored in order of definition starting at ST_NSPACE_FIELDS.
 * Fields reference a dimension by its index based from ST_NSPACE_DIMNS.
 */
#define ST_NSPACE_FIELDS	2000
#define ST_NSPACE_DIMNS		3000
#define NSDimn(index)		(ST_NSPACE_DIMNS + (index))
#define NSFld(index)		(ST_NSPACE_FIELDS + (index))
#define NS_STATIC_SAMPLE	0

#define IsNSpace(dc,fn) \
	(dc_ReqSubClassOf (dc->dc_Class, DCC_NSpace, fn))
#define AddInfo(dc,info) \
	dc_AddADE (dc, (DataPtr) info, DCC_NSpace, ST_NSPACE_INFO, \
		   sizeof (NSpaceInfo), TRUE)
#define AddDimn(dc,dinfo,i)	\
	dc_AddADE (dc, (DataPtr) dinfo, DCC_NSpace, NSDimn(i),	\
		   sizeof (NSpaceDimInfo), TRUE)
#define AddFld(dc,vinfo,i)	\
	dc_AddADE (dc, (DataPtr) vinfo, DCC_NSpace, NSFld(i),	\
		   sizeof (NSpaceFldInfo), TRUE)
#define GetInfo(dc) \
	(NSpaceInfo *) dc_FindADE (dc, DCC_NSpace, ST_NSPACE_INFO, 0)
#define GetDimn(dc,i) \
	(NSpaceDimInfo *) dc_FindADE (dc, DCC_NSpace, NSDimn(i), 0)
#define GetFld(dc,i) \
	(NSpaceFldInfo *) dc_FindADE (dc, DCC_NSpace, NSFld(i), 0)

typedef struct _NSpaceInfo
{
	int		ns_NField;	/* number of fields	*/
	int		ns_NDim;	/* no. of dimensions    */
	unsigned char	ns_Defined;	/* true once data added */
} NSpaceInfo;

typedef struct _NSpaceFldInfo
{
	FieldId		nsf_Id;		/* field ID		*/
	unsigned short	nsf_NDimn;	/* number of dimensions */
	unsigned short	nsf_Dimns[DC_MaxDimension];
	                                /* index from BASE      */
	unsigned char	nsf_IsStatic;	/* static field flag	*/
	unsigned short	nsf_Index;	/* index of the ADE	*/
	unsigned long	nsf_Size;	/* product of its dimns */
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

/*----------------------------------------------------------------
 * Private routines
 */

static NSpaceDimInfo *DefineDimension FP((DataChunk *dc, char *name, 
					  FieldId id, 
					  unsigned long size, char *routine,
					  int warn_duplicates));
static NSpaceDimInfo *FindDimnByID FP((DataChunk *dc, FieldId id));
static NSpaceFldInfo *FindFieldByID FP((DataChunk *dc, FieldId id, 
					char *routine));
static NSpaceFldInfo *DefineField FP((DataChunk *dc, FieldId field, 
				      int ndims,
				      unsigned short *dim_indices, 
				      short is_static, char *routine));
static void SetFieldSizes FP((DataChunk *dc, NSpaceInfo *info, char *routine));
static int CheckStatic FP((NSpaceFldInfo *finfo, int test, char *routine));


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
	if (! IsNSpace(dc,"NSDefineDimension"))
		return;

	(void) DefineDimension (dc, F_GetName(field), field, size, 
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
	NSpaceDimInfo *dinfo;
	int i;
	char *field_name;
	unsigned short dim_indices[ DC_MaxDimension ];

	if (! IsNSpace(dc, "NSDefineVariable"))
		return;

	/*
	 * Make sure we have a valid field id; it must have a non-empty name
	 */
	field_name = F_GetName (field);
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
		dinfo = FindDimnByID (dc, dims[i]);
		if (! dinfo)
		{
			msg_ELog (EF_PROBLEM,
			  "%s: dimn id %i not found for variable %s, id %i",
			  "NSDefineVariable", dims[i], field_name, field);
			return;
		}
		dim_indices[i] = dinfo->nsd_Index;
	}

	(void) DefineField (dc, field, ndims, dim_indices, is_static,
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
	NSpaceDimInfo *dinfo;
	int i;
	char *field_name;
	unsigned short dim_indices[ DC_MaxDimension ];

	if (!IsNSpace(dc,"NSDefineField"))
		return;

	/*
	 * Test for valid field id and name
	 */
	field_name = F_GetName(field);
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
		dinfo = DefineDimension (dc, dimnames[i], BadField, 
					 dimsizes[i], "NSDefineField", FALSE);
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
	(void) DefineField (dc, field, ndims, dim_indices, is_static,
			    "NSDefineField");
}
/*------------------------------------------- end: dc_NSDefineField --------*/



/*-------------------------------------------- begin: dc_NSDefineComplete --*/
void
dc_NSDefineComplete (dc)
	DataChunk *dc;
/*
 * Marks the definition as completed, which blocks any further
 * definition changes, and then calls SetFieldsSizes() to calculate
 * the size of each field, as required by the data adding functions.
 * Does nothing if definition already complete.
 */
{
	NSpaceInfo *info = GetInfo(dc);
	int nfield;
	FieldId fields[ DC_MaxField ];

	if (! IsNSpace(dc,"NSDefineComplete"))
		return;

	if (! (info->ns_Defined))
	{
		nfield = dc_NSGetAllVariables (dc, fields, NULL);
		dc_SetupFields (dc, nfield, fields);
		info->ns_Defined = (unsigned char)TRUE;
		SetFieldSizes (dc, info, "NSDefineComplete");
	}
}
/*--------------------------------------------- end: dc_NSDefineComplete --*/




/*----------------------------------------- begin: dc_NSDefineIsComplete --*/
int
dc_NSDefineIsComplete (dc)
	DataChunk *dc;
{
	NSpaceInfo *info = GetInfo(dc);

	if (! IsNSpace(dc,"NSDefineIsComplete"))
		return(0);

	return ((int)(info->ns_Defined));
}
/*------------------------------------------- end: dc_NSDefineIsComplete --*/



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
	for (i = 0; i < info->ns_NDim; ++i)
	{
		dinfo = GetDimn(dc,i);
		if (sizes)
			sizes[i] = dinfo->nsd_Size;
		if (fields)
			fields[i] = dinfo->nsd_Id;
		if (names)
			names[i] = &(dinfo->nsd_Name[0]);
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
	for (i = 0; i < info->ns_NField; ++i)
	{
		finfo = GetFld(dc,i);
		if (ndims)
			ndims[i] = finfo->nsf_NDimn;
		if (fields)
			fields[i] = finfo->nsf_Id;
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
	NSpaceFldInfo *finfo;
	NSpaceDimInfo *dinfo;
	int i;

	if (! IsNSpace(dc,"NSGetField"))
		return (0);

	if (!(finfo = FindFieldByID (dc, field, "NSGetField")))
		return (0);

	if (ndims)
		*ndims = finfo->nsf_NDimn;
	if (is_static)
		*is_static = (int)(finfo->nsf_IsStatic);
	if (!names && !sizes)
		return (TRUE);

	/*
	 * We have to traverse our dimensions and get info about them
	 */
	for (i = 0; i < finfo->nsf_NDimn; ++i)
	{
		dinfo = GetDimn(dc, finfo->nsf_Dimns[i]);
		if (names)
			names[i] = &(dinfo->nsd_Name[0]);
		if (sizes)
			sizes[i] = dinfo->nsd_Size;
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

	if (!(finfo = FindFieldByID (dc, field, "NSIsStatic")))
		return (0);

	return (finfo->nsf_IsStatic);
}
/*---------------------------------------------- end: dc_NSIsStatic() -----*/



/*------------------------------------------- begin: dc_NSGetDimension() --*/
void 
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
		return;

	dinfo = FindDimnByID (dc, dimn);
	if (!dinfo)
	{
		msg_ELog (EF_PROBLEM, "%s: no dimension %i",
			  "NSGetDimension", dimn);
		return;
	}

	if (name)
		*name = &(dinfo->nsd_Name[0]);
	if (size)
		*size = dinfo->nsd_Size;
	return;
}
/*------------------------------------------------ end: dc_NSGetDimension -*/


	
/*---------------------------------------------- begin: dc_NSGetVariable --*/
void
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
	NSpaceFldInfo *finfo;
	NSpaceDimInfo *dinfo;
	int i;

	if (! IsNSpace(dc,"NSGetVariable"))
		return;

	if (!(finfo = FindFieldByID (dc, field, "NSGetVariable")))
		return;

	if (ndims)
		*ndims = finfo->nsf_NDimn;
	if (is_static)
		*is_static = finfo->nsf_IsStatic;
	if (! dims)
		return;

	for (i = 0; i < finfo->nsf_NDimn; ++i)
	{
		dinfo = GetDimn(dc,finfo->nsf_Dimns[i]);
		dims[i] = dinfo->nsd_Id;
	}

	return;
}
/*---------------------------------------------- end: dc_NSGetVariable ----*/




/*----------------------------------------------- begin: dc_NSAddSample ---*/
void
dc_NSAddSample(dc, when, sample, field, values)
	DataChunk *dc;
	ZebTime *when;
	int sample;
	FieldId field;
	float *values;
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

	if (! IsNSpace(dc,"NSAddSample"))
		return;

	if (!(finfo = FindFieldByID (dc, field, "NSAddSample")))
		return;

	/*
	 * Make sure this field is dynamic
	 */
	if (!CheckStatic (finfo, FALSE, "NSAddSample"))
		return;

	/*
	 * If this is the first addition of data to this datachunk,
	 * we must define our fields to our superclass first, and
	 * then we can add the data.
	 */
	dc_NSDefineComplete (dc);

	/*
	 * Now add the data, retrieving the size from the finfo structure.
	 */
	dc_AddMData (dc, when, field, ((finfo->nsf_Size) * sizeof(float)),
		     sample, /*nsample*/ 1, values);
}
/*---------------------------------------------------- end: dc_NSAddSample --*/



/*-------------------------------------------------- begin: dc_NSAddStatic --*/
void
dc_NSAddStatic (dc, field, values)
	DataChunk *dc;
	FieldId field;
	float *values;
/*
 * Same as above except no time or sample is associated with the data.  The
 * field must have been defined as a static variable.  Each successive call
 * overwrites any previously stored data.  Static data will be stored in the
 * zero'eth sample.
 */
{
	NSpaceFldInfo *finfo;
	ZebTime epoch;		/* serves as AddMData `when' parameter */

	epoch.zt_Sec = 0;
	epoch.zt_MicroSec = 0;

	if (! IsNSpace(dc,"NSAddStatic"))
		return;

	if (!(finfo = FindFieldByID (dc, field, "NSAddStatic")))
		return;

	/*
	 * Make sure this field is static
	 */
	if (!CheckStatic (finfo, TRUE, "NSAddStatic"))
		return;

	/*
	 * If this is the first addition of data to this datachunk,
	 * we must define our fields to our superclass first, and
	 * then we can add the data.
	 */
	dc_NSDefineComplete (dc);

	/*
	 * Now add the data, retrieving the size from finfo structure.
	 */
	dc_AddMData (dc, /*when*/ &epoch, field, 
		     (finfo->nsf_Size * sizeof(float)),
		     NS_STATIC_SAMPLE, /*nsample*/ 1, values);
}
/*------------------------------------------------- end: dc_NSAddStatic ---*/


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
float *
dc_NSGetSample(dc, sample, field, size)
	DataChunk *dc;
	int sample;
	FieldId field;
	unsigned long *size;
/*
 * Returns pointer to field's data at sample.  If size non-NULL, returns
 * length of data array (number of floats) in *size.  Array is in row-major
 * order, just as it was stored.  If the data is not found, NULL is returned.
 */
{
	int len;
	DataPtr data;
	NSpaceFldInfo *finfo;

	if (! IsNSpace(dc,"NSGetSample"))
		return NULL;

	if (!(finfo = FindFieldByID (dc, field, "NSGetSample")))
		return NULL;

	/*
	 * Make sure this field is dynamic
	 */
	if (!CheckStatic (finfo, FALSE, "NSGetSample"))
		return NULL;

	data = dc_GetMData (dc, sample, field, &len);
	if (size)
		*size = (unsigned long) (len / sizeof(float));
	return ((float *)data);
}
/*------------------------------------------------ end: dc_NSGetSample ----*/



/*---------------------------------------------- begin: dc_NSGetStatic ----*/
float *
dc_NSGetStatic (dc, field, size)
	DataChunk *dc;
	FieldId field;
	unsigned long *size;
/*
 * Basically, same as dc_NSGetSample() except sample parameter not required.
 * Returns pointer to field's data at sample.  If size non-NULL, returns
 * length of data array (number of floats) in *size.  Array is in row-major
 * order, just as it was stored.  If the data is not found or there is an
 * error, NULL is returned.
 */
{
	int len;
	DataPtr data;
	NSpaceFldInfo *finfo;

	if (! IsNSpace(dc,"NSGetStatic"))
		return NULL;

	if (!(finfo = FindFieldByID (dc, field, "NSGetStatic")))
		return NULL;

	/*
	 * Make sure this field is static
	 */
	if (!CheckStatic (finfo, TRUE, "NSGetStatic"))
		return NULL;

	data = dc_GetMData (dc, NS_STATIC_SAMPLE, field, &len);
	if (size)
		*size = (unsigned long) (len / sizeof(float));
	return ((float *)data);
}
/*------------------------------------------------ end: dc_NSGetStatic ----*/




/*****************************************************************
// Semi-private routines --- called through our datachunk class
//			     methods stucture
/----------------------------------------------------------------*/


/*--------------------------------------------------- begin: NSCreate() */
static DataChunk *
NSCreate (class)
DataClass class;
/*
 * Create a chunk of this class.
 */
{
	DataChunk *dc;
	NSpaceInfo *info;
/*
 * The usual.  Make a superclass chunk and tweak it to look like us.
 */
	dc = dc_CreateDC (SUPERCLASS);
	dc->dc_Class = class;
	info = (NSpaceInfo *) dc_FindADE (dc, DCC_NSpace, ST_NSPACE_INFO, 0);
/*
 * Create, initalize, and add our global info structure
 */
	info = ALLOC(NSpaceInfo);
	info->ns_NField = 0;
	info->ns_NDim = 0;
	info->ns_Defined = FALSE;

	AddInfo (dc, info);
	return (dc);
}
/*-------------------------------------------------------- end: NSCreate() */



/*------------------------------------------------------- begin: NSDump() -*/
static void
NSDump (dc)
	DataChunk *dc;
/*
 * Our dump method --- we need to see our complicated structures for
 * debugging purposes, and for the sake of applications programmers
 */
{
	NSpaceInfo *info;
	NSpaceFldInfo *finfo;
	NSpaceDimInfo *dinfo[DC_MaxDimension];
	int i,j;

	if (! IsNSpace(dc,"NSDump"))
		return;

	info = GetInfo(dc);
	if (!info)
	{
		msg_ELog (EF_PROBLEM, "NSDump: info not found");
		return;
	}

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

	printf ("\nNumber of variables: %-5i    Number of dimensions: %-5i\n",
		info->ns_NField, info->ns_NDim);
	/*
	 * Print all info about each dimensions, and remember its ADE * for
	 * reference when printing out our fields
	 */
	/*printf ("Dimensions:\n");*/
	for (i = 0; i < info->ns_NDim; ++i)
	{
		dinfo[i] = GetDimn(dc,i);
		printf("   %30s: size %-6lu  -- ", 
		       dinfo[i]->nsd_Name, dinfo[i]->nsd_Size);
		if (dinfo[i]->nsd_Id != BadField)
			printf("field %i, '%s'\n", 
			       dinfo[i]->nsd_Id, F_GetDesc(dinfo[i]->nsd_Id));
		else
			printf("no field id\n");
	}
	/*
	 * Print each field, in the form: 
	 * NAME ( DIM1, DIM2, ... ) [static] 'DESC'
	 * The dim name is retrieved from the ADE * list in dinfo[].
	 */
	/*printf ("Fields:\n");*/
	for (i = 0; i < info->ns_NField; ++i)
	{
		finfo = GetFld(dc,i);
		printf("   %s ( ", F_GetName(finfo->nsf_Id));
		for (j = 0; j < finfo->nsf_NDimn; ++j)
			printf("%s%s", dinfo[finfo->nsf_Dimns[j]]->nsd_Name,
			       (j == (finfo->nsf_NDimn - 1)) ? "" : ", ");
		printf(" )");
		printf("%s ", (finfo->nsf_IsStatic) ? " static," : "");
		printf("size = %lu, ", finfo->nsf_Size);
		printf("'%s'\n", F_GetDesc(finfo->nsf_Id));
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
DefineDimension(dc, name, field, size, routine, warn_duplicates)
	DataChunk *dc;
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
	NSpaceInfo *info;
	NSpaceDimInfo *dinfo;
	char dim_name [ DC_MaxDimName ];
	int i;

	/*
	 * Get our global info so that we know what we have to work with
	 */
	info = (NSpaceInfo *) dc_FindADE (dc, DCC_NSpace, ST_NSPACE_INFO, 0);
	if (!info)
	{
		msg_ELog (EF_PROBLEM, 
			  "%s: info not found, cannot continue", routine);
		return NULL;
	}

	if (info->ns_Defined)
	{
		msg_ELog (EF_PROBLEM,
		  "%s: datachunk has data, no more definitions allowed",
		  routine);
		return NULL;
	}

	/*
	 * If the field id is not BadField, make sure it is a valid field id; 
	 * Also, field must have a non-empty name.
	 */
	if ((field != BadField) && !F_GetName(field))
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
	if (strlen(name) >= DC_MaxDimName)
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
	for (i = 0; i < info->ns_NDim; ++i)
	{
		dinfo = GetDimn(dc,i);
		if (!strcmp(dinfo->nsd_Name, dim_name))
			break;				/* match found */
	}

	if (i < info->ns_NDim)
	{
		/*
		 * A dimension is being re-defined; note this to user and
		 * override all previous settings
		 */
		if (warn_duplicates)
			msg_ELog (EF_PROBLEM, 
				  "%s: dimension %s being re-defined", 
				  routine, dim_name);
		if (dinfo->nsd_Id != field)
		{
			if (field != BadField)
				msg_ELog (EF_PROBLEM, 
					  "%s: dimn %s given new id %i",
					  routine, dim_name, field);
			else
				msg_ELog (EF_PROBLEM,
					  "%s: dimn %s id removed",
					  routine, dim_name);
			dinfo->nsd_Id = field;
		}
	}
	else
	{
		/* 
		 * New dimension gets whole new entry, but only if 
		 * there's room.
		 *
		 * Note that i will be this dimn's ADE index for access
		 * via NSDimn(i) 
		 */
		if (info->ns_NDim == DC_MaxDimension)
		{
			msg_ELog (EF_PROBLEM, 
				  "%s: no more room for dimn %s, limit = %i",
				  routine, dim_name,
				  DC_MaxDimension);
			return NULL;
		}
		msg_ELog (EF_DEBUG, "%s: dimn %s, size %lu, id %i (%s)",
			  routine, dim_name, size, field, 
			  (field == BadField) ? 
			  "id not used" : F_GetName(field));
		dinfo = ALLOC(NSpaceDimInfo);
		dinfo->nsd_Size = size;
		dinfo->nsd_Id = field;
		strcpy (dinfo->nsd_Name, dim_name);
		dinfo->nsd_Index = i;
		AddDimn(dc,dinfo,i);
		++(info->ns_NDim);
	}

	/*
	 * We have either added or updated a dimension ADE, and the
	 * global info ADE has been updated in-place.  Done.  
	 * Return pointer to the dimn info.
	 */
	return (dinfo);
}
/*-------------------------------------------------- end: DefineDimension() */



/*-------------------------------------------------- begin: FindDimnByID() -*/
static NSpaceDimInfo *
FindDimnByID (dc, id)
	DataChunk *dc;
	FieldId id;
/*
 * Return pointer to a dimn info structure for the dimn with specified id.
 * Returns NULL if dimn not found.
 */
{
	NSpaceInfo *info;
	NSpaceDimInfo *dinfo;
	int i;

	info = GetInfo(dc);
	for (i = 0; i < info->ns_NDim; ++i)
	{
		dinfo = GetDimn(dc, i);
		if (dinfo->nsd_Id == id)
			return (dinfo);
	}
	return NULL;
}
/*---------------------------------------------------- end: FindDimnByID() -*/



/*------------------------------------------------- begin: FindFieldByID() -*/
static NSpaceFldInfo *
FindFieldByID (dc, id, routine)
	DataChunk *dc;
	FieldId id;
	char *routine;
/*
 * Return pointer to a field info structure for the field with specified id.
 * Returns NULL if field not found.
 */
{
	NSpaceInfo *info = GetInfo(dc);
	NSpaceFldInfo *finfo;
	int i;

	for (i = 0; i < info->ns_NField; ++i)
	{
		finfo = GetFld(dc, i);
		if (finfo->nsf_Id == id)
			return (finfo);
	}
	msg_ELog (EF_PROBLEM, "%s: no field %i in datachunk",
		  routine, id);
	return NULL;
}
/*-------------------------------------------------- end: FindFieldByID() -*/




/*-------------------------------------------------- begin: DefineField() -*/
static NSpaceFldInfo *
DefineField (dc, field, ndims, dim_indices, is_static, routine)
	DataChunk *dc;
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
	NSpaceInfo *info = GetInfo(dc);
	NSpaceFldInfo *finfo;
	char *field_name = F_GetName(field);
	int i;

	if (info->ns_Defined)
	{
		msg_ELog (EF_PROBLEM,
		  "%s: datachunk has data, no more definitions allowed",
		  routine);
		return NULL;
	}

	for (i = 0; i < info->ns_NField; ++i)
	{
		finfo = GetFld(dc,i);
		if (finfo->nsf_Id == field)
			break;				/* match found */
	}

	if (i < info->ns_NField)
	{
		/*
		 * A variable is being re-defined; warn user and change
		 * the field ADE
		 */
		msg_ELog (EF_PROBLEM, 
			  "%s: field %s being re-defined",
			  routine, field_name);
		if (finfo->nsf_NDimn != ndims)
		{
			msg_ELog (EF_PROBLEM,
				  "%s: field %s now has %lu dimns",
				  routine, field_name, ndims);
			finfo->nsf_NDimn = ndims;
		}
		if (finfo->nsf_IsStatic && (!is_static))
		{
			msg_ELog (EF_PROBLEM,
				  "%s: field %s has changed to %s",
				  routine, field_name, 
				  (is_static) ? "static" : "dynamic");
			finfo->nsf_IsStatic = (is_static) ? TRUE : FALSE;
		}
		/*
		 * The dimensions will be copied into the ADE further on
		 */
	}
	else
	{
		/*
		 * Create a new field ADE and fill it in, if space allows
		 */
		if (info->ns_NField == DC_MaxField)
		{
			msg_ELog (EF_PROBLEM, 
			  "%s: no more room for field %s, limit = %i",
			  routine, field_name,
			  DC_MaxField);
			return NULL;
		}
		msg_ELog (EF_DEBUG, "%s: defining field %s, id %i",
			  routine, field_name, field);
		finfo = ALLOC(NSpaceFldInfo);
		finfo->nsf_NDimn = ndims;
		finfo->nsf_Id = field;
		finfo->nsf_Index = i;
		finfo->nsf_Size = 0;
		finfo->nsf_IsStatic = (is_static) ? TRUE : FALSE;
		AddFld(dc,finfo,i);
		++(info->ns_NField);
	}

	/*
	 * Now copy dimension list into field ADE, regardless of whether
	 * this is a new or re-defined variable.  Note that calculating
	 * field size is held off until definition is complete.
	 */
	for (i = 0; i < ndims; ++i)
	{
		finfo->nsf_Dimns[i] = dim_indices[i];
	}

	/*
	 * All done.
	 */
	return (finfo);
}
/*--------------------------------------------------- end: DefineField() -*/



/*------------------------------------------------ begin: SetFieldSizes --*/
static void
SetFieldSizes (dc, info, routine)
	DataChunk *dc;
	NSpaceInfo *info;
	char *routine;		/* name of calling routine */
/*
 * For each field, set its Size to the product of the sizes of
 * each of its dimensions.
 */
{
	NSpaceFldInfo *finfo;
	NSpaceDimInfo *dinfo;
	int i, j;
	unsigned long size;

	for (i = 0; i < info->ns_NField; ++i)	/* field loop */
	{
		size = 1;
		finfo = GetFld(dc,i);
		for (j = 0; j < finfo->nsf_NDimn; ++j)	/* dimn loop */
		{
			dinfo = GetDimn(dc,finfo->nsf_Dimns[j]);
			size *= (dinfo->nsd_Size);
		}					/* dimn loop */
		finfo->nsf_Size = size;
		msg_ELog (EF_DEBUG, "%s: field %s, size set to %lu",
			  routine, F_GetName(finfo->nsf_Id), size);

	}					/* field loop */
}
/*------------------------------------------------- end: SetFieldSizes --*/



/*---------------------------------------------- begin: CheckStatic -----*/
static int
CheckStatic(finfo, test, routine)
	NSpaceFldInfo *finfo;
	int test;
	char *routine;
/*
 * Make sure this field is in fact what's wanted, static or dynamic,
 * as determined by test. IsStatic should match value of test.
 * Returns nonzero if test passes, zero if fails.
 */
{
	if ((finfo->nsf_IsStatic && !test) || 
	    (!(finfo->nsf_IsStatic) && test))
	{
		msg_ELog (EF_PROBLEM, 
			  "%s: field %s, id %i, must be %s",
			  routine, F_GetName(finfo->nsf_Id), finfo->nsf_Id,
			  (test) ? "static" : "dynamic");
		return (FALSE);
	}
	return (TRUE);
}
/*----------------------------------------------- end: CheckStatic -----*/


#ifdef NSPACE_TEST


/* ARGSUSED */
int
msg_handler (msg)
struct message *msg;
{
	msg_ELog (EF_INFO, "Message received");
	return (0);
}


static float test_data[10000];

#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>

/*ARGSUSED*/
int
main (argc, argv)
	int argc;
	char *argv[];
{
	DataChunk *dc;
	int i,j;
	ZebTime when;
	struct timeb tp;
	char *names[ DC_MaxDimension ];
	unsigned long sizes[ DC_MaxDimension ];
	FieldId ids[ DC_MaxDimension + DC_MaxField ];
	FieldId rids[ DC_MaxDimension + DC_MaxField ];
	int ndims[ DC_MaxDimension ];
	int ndim, nvar, rndim, rstatic;
	char *name;
	unsigned long size;
	float *retrieve;

	ftime(&tp);
	when.zt_Sec = tp.time;
	when.zt_MicroSec = 0;

	for (i = 0; i < 10000; ++i)
		test_data[i] = i;

	usy_init();
	F_Init();
	msg_connect (msg_handler, argv[0]);

	printf("---------------------------------------------------------\n");
	{	/* an empty NSpace data chunk */
		
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = BadPlatform;
		dc_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	printf("---------------------------------------------------------\n");
	{	/* simple static variable over two dimensions */

		FieldId field;
		char *dim_names[2];
		unsigned long dim_sizes[2];
		
		dim_names[0] = "x";	dim_names[1] = "y";
		dim_sizes[0] = 50;	dim_sizes[1] = 25;
		field = F_DeclareField ("curl","Long name","units");
		dc = dc_CreateDC (DCC_NSpace);
		dc_NSDefineField (dc, field, 2, dim_names, dim_sizes, TRUE);
		dc->dc_Platform = BadPlatform;
		dc_DumpDC (dc);

		dc_NSAddSample (dc, &when, 0, field, test_data);
		dc_NSAddStatic (dc, field, test_data);
		dc_DumpDC (dc);

		/* retrieve data and compare */
		retrieve = dc_NSGetSample (dc, 0, field, &size);
		retrieve = dc_NSGetStatic (dc, field, &size);
		printf ("dc_NSGetStatic() returns size = %lu,", size);
		for (i = 0; i < size; ++i)
			if (retrieve[i] != test_data[i]) break;
		printf ("  data comparison %s at %i\n",
			(i < size) ? "failed" : "succeeded", i);

		/* test dc_NSGetField */
		(void)dc_NSGetField(dc, field, &rndim, names, sizes,
				    &rstatic);
		printf("dc_NSGetField(%s:%i): %i dims, %s static, ( ",
		       F_GetName(field), field, rndim,
		       (rstatic) ? "is" : "not");
		for (i = 0; i < rndim; ++i)
			printf("%s=%lu ",names[i],sizes[i]);
		printf(")\n");

		dc_DestroyDC (dc);
        }
	printf("---------------------------------------------------------\n");
	{	/* ARM SOW example using explicit dimn defs with field ids */

		FieldId wnum_id, therm_id;
		FieldId mean_rad_id, sd_rad_id;
		
		dc = dc_CreateDC(DCC_NSpace);
		dc->dc_Platform = /*ds_LookupPlatform("aeri")*/BadPlatform;
		
		wnum_id = F_DeclareField("wnum","Wave Number","1 / cm");
		dc_NSDefineDimension(dc, wnum_id, 6224);
		dc_NSDefineVariable(dc, wnum_id, 1, &wnum_id, 
				    /*is_static*/TRUE);
		
		mean_rad_id = F_DeclareField("mean_rad",
					"Mean of Radiance spectra ensemble",
					"mW / m^2 sr 1 / cm");
		dc_NSDefineVariable(dc, mean_rad_id, 1, &wnum_id, FALSE);
		
		sd_rad_id = F_DeclareField("standard_dev_rad",
			   "standard deviation for Radiance spectra ensemble",
			   "1/cm");
		dc_NSDefineVariable(dc, sd_rad_id, 1, &wnum_id, FALSE);
		therm_id = F_DeclareField("thermistor1",
					  "Long name for thermistor",
					  "units");
		dc_NSDefineVariable(dc, therm_id, 0, NULL, FALSE);
		
		dc_DumpDC (dc);

		/* Test the information retrieval functions */
		ndim = dc_NSGetAllDimensions(dc, names, ids, sizes);
		printf ("dc_NSGetAllDimensions() returns %i dimensions:\n", 
			ndim);
		for (i = 0; i < ndim; ++i)
		{
			printf("   %-30s %-5i %10lu\n",
			       names[i],ids[i],sizes[i]);
			dc_NSGetDimension (dc, ids[i], &name, &size);
			printf(
			  "   NSGetDimension(id %i): name %s, size = %lu\n",
			  ids[i], name, size);
		}

		nvar = dc_NSGetAllVariables(dc, ids, ndims);
		printf ("dc_NSGetAllVariables() returns %i variables:\n", 
			nvar);
		for (i = 0; i < nvar; ++i)
		{
			printf("   id: %-3i   ndims: %-5i", ids[i], ndims[i]);
			printf("   NSIsStatic(): %s\n",
			       (dc_NSIsStatic(dc, ids[i])) ? "True" : "False");
			dc_NSGetVariable(dc, ids[i], &rndim, rids, &rstatic);
			printf("   dc_NSGetVariable(%i): %i dims, %s static, ",
			       ids[i], rndim, (rstatic)?"is":"not");
			printf("  ( ");
			for (j = 0; j < rndim; ++j)
				printf(" %i ",rids[j]);
			printf(")\n");
		}

		/* Test some storage */
		dc_NSAddStatic (dc, wnum_id, test_data);
		dc_NSAddStatic (dc, mean_rad_id, test_data);
		dc_NSAddSample (dc, &when, 0, wnum_id, test_data);
		dc_NSAddSample (dc, &when, 1, therm_id, test_data);
		dc_NSAddSample (dc, &when, 2, mean_rad_id, test_data);
		dc_NSAddSample (dc, &when, 2, sd_rad_id, test_data);
		dc_DumpDC (dc);

		/* test some retrieval */
		(void)dc_NSGetSample (dc, 0, wnum_id, &size); /*should fail*/
		retrieve = dc_NSGetStatic (dc, wnum_id, &size);
		retrieve = dc_NSGetStatic (dc, wnum_id, NULL);
		printf ("dc_NSGetStatic(%s) returns size = %lu,", 
			F_GetName(wnum_id), size);
		for (i = 0; i < size; ++i)
			if (retrieve[i] != test_data[i]) break;
		printf ("   data comparison %s at %i\n",
			(i < size) ? "failed" : "succeeded", i);
		retrieve = dc_NSGetSample (dc, 0, therm_id, &size);
		printf("GetSample(0,%s): size=%lu, data=%s\n",
		       F_GetName(therm_id),size,(retrieve)?"non-NULL":"NULL");
		retrieve = dc_NSGetSample (dc, 2, mean_rad_id, &size);
		retrieve = dc_NSGetSample (dc, 2, mean_rad_id, NULL);
		printf ("dc_NSGetSample(%s) returns size = %lu,", 
			F_GetName(mean_rad_id), size);
		for (i = 0; i < size; ++i)
			if (retrieve[i] != test_data[i]) break;
		printf ("   data comparison %s at %i\n",
			(i < size) ? "failed" : "succeeded", i);
		retrieve = dc_NSGetSample (dc, 2, BadField, NULL);
		printf("GetSample(2,BadField): data=%s\n",
		       (retrieve)?"non-NULL":"NULL");

		dc_DestroyDC (dc);
	}
	printf("---------------------------------------------------------\n");
	{	/* ARM SOW example with implicit dimn defs */

		FieldId mean_rad_id, sd_rad_id, wnum_id;
		char *dimname[1];
		unsigned long dimsize[1];
		
		dimname[0] = "wnum";	dimsize[0] = 6224;
		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = /*ds_LookupPlatform("aeri")*/BadPlatform;
		
		wnum_id = F_DeclareField("wnum","Wave Number","1 / cm");
		dc_NSDefineField(dc, wnum_id, 1, dimname, dimsize, FALSE);
		
		mean_rad_id = F_DeclareField("mean_rad",
				     "Mean of Radiance spectra ensemble",
				     "mW / m^2 sr 1 / cm");
		dc_NSDefineField(dc, mean_rad_id, 1, 
				 dimname, dimsize, FALSE);
		
		sd_rad_id = F_DeclareField("standard_dev_rad",
			   "standard deviation for Radiance spectra ensemble",
			   "1/cm");
		dc_NSDefineField(dc, sd_rad_id, 1, dimname, dimsize, FALSE);
		
		dc_DumpDC (dc);

		/* test dc_NSGetField */
		(void)dc_NSGetField(dc, sd_rad_id, &rndim, names, sizes,
				    &rstatic);
		printf("dc_NSGetField(%s:%i): %i dims, %s static, ( ",
		       F_GetName(sd_rad_id), sd_rad_id, rndim,
		       (rstatic) ? "is" : "not");
		for (i = 0; i < rndim; ++i)
			printf("%s=%lu ",names[i],sizes[i]);
		printf(")\n");

		dc_DestroyDC (dc);
	}
	printf("---------------------------------------------------------\n");
	{	/* silly stuff */

		FieldId fid, did;
		char *dimname[3];
		unsigned long dimsize[3];
		
		dimname[0] = "thisdimensionhasareallylongnamethatwillnotfit";
		dimname[1] = "thisoneisnotsolongjustlong283032";
		dimname[2] = "third";
		dimsize[0] = 20; dimsize[1] = 40; dimsize[2] = 60;

		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = 0;
		
		fid = F_DeclareField("tests","Testing 1, 2, 3","#");
		did = F_DeclareField(dimname[1],"Dimension","u");

		/* define a variable whose dimensions do not exist */
		dc_NSDefineVariable (dc, fid, 1, &did, TRUE);

		/* try defining a dimension with a long name */
		dc_NSDefineField(dc, fid, 3, dimname, dimsize, FALSE);
		NSDump (dc);
		
		/* now try redefining a dimension */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		NSDump (dc);

		/* redefine a variable */
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);
		NSDump (dc);

		/* test completion */
		printf("dc_NSDefineIsComplete() returns %s\n",
		       dc_NSDefineIsComplete(dc) ? "True" : "False");

		/* force completion */
		dc_NSDefineComplete(dc);
		printf("%s, dc_NSDefineIsComplete() returns %s\n",
		       "After dc_NSDefineComplete()",
		       dc_NSDefineIsComplete(dc) ? "True" : "False");

		/* try more definition after completion */
		dc_NSDefineDimension(dc, did, dimsize[2]);
		dc_NSDefineVariable(dc, fid, 1, &did, TRUE);
		
		dc_DumpDC (dc);
		dc_DestroyDC (dc);
	}
	printf("---------------------------------------------------------\n");
	{	/* push limits of number of dims and fields in a chunk */

		char name[ 10 ];
		char *namep = name;
		FieldId fid, did;
		unsigned long size;
		int i;

		dc = dc_CreateDC (DCC_NSpace);
		dc->dc_Platform = BadPlatform;

		/* test dimn limit */
		for (i = 0; i < DC_MaxDimension + 2; ++i)
		{
			sprintf (name, "dimn%i", i);
			did = F_DeclareField(name, "Dimension", "none");
			size = i;
			dc_NSDefineDimension(dc, did, size);
		}

		/* test field limit */
		did = F_Lookup("dimn12");
		for (i = 0; i < DC_MaxField + 2; ++i)
		{
			sprintf (name, "field%i", i);
			fid = F_DeclareField(name, "Field", "units");
			dc_NSDefineVariable(dc, fid, 1, &did, i % 2);
		}

		/* test field limit with DefineField */
		dc_NSDefineField(dc, fid, 0, 0, 0, 0);

		/* test dimn limit with DefineField by redefining field */
		dc_NSDefineField(dc, F_Lookup("field1"), 1, 
				 &namep, &size, TRUE);

		NSDump (dc);

		/* see what it looks like after closing definition */
		dc_NSDefineComplete (dc);
		NSDump (dc);

		dc_DestroyDC (dc);
	}

	return(0);
}

#endif
