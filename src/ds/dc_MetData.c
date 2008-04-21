/* -*- mode: c; c-basic-offset: 8; -*- 
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

# include <stdio.h>
# include <string.h>

# include <defs.h>
# include <config.h>		/* For CFG_ symbols */
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"

RCSID ("$Id: dc_MetData.c,v 3.29 2002-11-14 05:49:55 granger Exp $")

/*
 * If we have non-uniform, non-fixed, non-pre-arranged fields, then the 
 * beginning piece of every sample contains a list of the following:
 */
typedef struct _FieldTOC
{
	int	ft_Offset;		/* Offset of this field		*/
	int	ft_Len;			/* Length of this field		*/
} FieldTOC;


static int FillValuePointer = 0;
DataPtr DC_FillValues = (DataPtr)&FillValuePointer;


# define SUPERCLASS ((DataClassP)&TranspMethods)

/*
 * Class method prototypes
 */
static DataChunk *md_Create (DataChunk *);
static void	md_Dump (DataChunk *);
static void	md_Serialize (DataChunk *dc);
static void	md_Localize (DataChunk *dc);

RawClass MetDataMethods =
{
	DCID_MetData,		/* Class id */
	"MetData",
	SUPERCLASS,		/* Superclass			*/
	2,			/* Depth, Raw = 0		*/
	md_Create,
	0,			/* Destroy			*/
	0,			/* Add				*/
	md_Dump,		/* Dump				*/

	md_Serialize,		/* Serialize			*/
	md_Localize,		/* Localize			*/

	sizeof (MetDataChunk)
};

DataClassP DCP_MetData = (DataClassP)&MetDataMethods;


/*
 * Local routines.
 */
static FldInfo *dc_BuildFieldInfo FP((DataChunk *dc, int nfld, FieldId *fids));
static void	dc_BuildIndexHash FP((FldInfo *finfo, int begin, int end));
static inline void dc_MDSetType FP((DataChunk *dc, FldInfo *finfo, FieldId fid,
				    DC_ElemType type));
static void	dc_MDOptimize FP((DataChunk *dc, FldInfo *finfo));
static void *	dc_AddUniform FP((DataChunk *dc, FldInfo *finfo, int findex,
				  ZebTime *t, int size, int start, int nsamp,
				  DataPtr data));
static void *	dc_AddNonUniform FP((DataChunk *, FldInfo *, int, ZebTime *,
			int, int, int, DataPtr));
static void *	dc_AddFixedField FP((DataChunk *dc, FldInfo *finfo, 
				     int findex, ZebTime *t, int size, 
				     int start, int nsamp, DataPtr data));
static FldInfo *dc_ChangeInfo FP((DataChunk *dc, char *method));


#define dc_GetSample dc_MetGetSample

static inline DataPtr
dc_MetGetSample (dc, sample, len)
DataChunk *dc;
int sample, *len;
/*
 * Inlined version of Transparent class's dc_GetSample.
 */
{
	AuxTrans *tp = (&((TranspDataChunk *)(dc))->transpart);
/*
 * Make sure the sample exists.  If so, return the info.
 */
	if (sample < 0 || sample >= (unsigned) tp->at_NSample)
		return (NULL);
	if (len)
		*len = tp->at_Samples[sample].ats_Len;
	return ((DataPtr) ((char *) dc->dc_Data +
			tp->at_Samples[sample].ats_Offset));
}


#ifdef METDATA_STATS
static int GetIndexHits = 0;
static int GetIndexRolls = 0;
#endif


static inline int
dc_GetIndex (finfo, field)
FldInfo *finfo;
FieldId field;
/*
 * Return the index of this field in this DC by looking it up in the hash
 * table.  Note that we expect at least one more empty slot than the
 * maximum possible number of fields, so that we don't infinitely loop if
 * we get a field that is not in a datachunk with DC_MaxField fields.
 * Return -1 if the field does not exist in this datachunk.
 */
{
	int hash, index;

	hash = HASH_FIELD_ID(field);

	while ((index = finfo->fi_HashIndex[hash]) != -1)
	{
		if (finfo->fi_Fields[index] == field)
		{
#ifdef METDATA_STATS
			++GetIndexHits;
#endif
			return (index);
		}
#ifdef METDATA_STATS
		++GetIndexRolls;
#endif
		if (++hash == HASH_SIZE)
			hash = 0;
	} 
	return (-1);
}



static void
dc_ClearHash (finfo)
FldInfo *finfo;
{
	int i;

	for (i = 0; i < HASH_SIZE; ++i)
		finfo->fi_HashIndex[i] = -1;
}



int
dc_ClearFields (dc)
DataChunk *dc;
/*
 * Clear this datachunk of any fields, unless changes are no longer allowed.
 * Return non-zero if we succeed.
 */
{
	FldInfo *finfo;

	if ((finfo = dc_ChangeInfo (dc, "ClearFields")) && finfo->fi_NField)
	{
		/* empty the hash and reset the count */
		dc_ClearHash (finfo);
		finfo->fi_NField = 0;
	}
	return (finfo != NULL);
}




void
dc_SetupFields (dc, nfield, fields)
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Set datachunk fields to this list, by first clearing any existing fields
 * and adding the new ones.
 */
{
	FldInfo *finfo;

	if ((finfo = dc_ChangeInfo (dc, "SetupFields")))
	{
		dc_ClearFields (dc);
		dc_BuildFieldInfo (dc, nfield, fields);
	}
}




void
dc_AddFields (dc, nfield, fields)
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Add fields.
 */
{
	FldInfo *finfo;

	if ((finfo = dc_ChangeInfo (dc, "AddFields")))
	{
		dc_BuildFieldInfo (dc, nfield, fields);
	}
}



void
dc_ChangeFld (DataChunk *dc, FieldId oldfid, FieldId newfid)
/*
 * Change the field with FieldId 'oldfid' to have FieldId 'newfid'.
 */
{
    int idx;
    FldInfo *finfo = FIP(dc);

    if ((idx = dc_GetIndex (finfo, oldfid)) >= 0)
    {
	finfo->fi_Fields[idx] = newfid;
	dc_ClearHash (finfo);
	dc_BuildIndexHash (finfo, 0, finfo->fi_NField);
    }
    else
	msg_ELog (EF_PROBLEM, "dc_ChangeFld: failed to rename %s to %s",
		  F_GetFullName (oldfid), F_GetFullName (newfid));
}



    
#ifdef CFG_DC_OLDFIELDS	/* --------------------------------------- */
void
dc_SetupUniformFields (dc, nsamples, nfield, fields, size)
DataChunk *dc;
int nsamples, nfield;
FieldId *fields;
int size;
/*
 * Set this data chunk up to have uniform-length fields.
 * Entry:
 *	DC	is a new data chunk, which is a subclass of DCC_MetData
 *	NSAMPLES is the best guess at the number of samples which this
 *		data chunk will have.  It is not an upper limit.
 *	SIZE	is the uniform size that each field will have.
 */
{
	if (dc_ChangeInfo (dc, "SetupUniformFields"))
	{
		FldInfo *finfo;

		dc_ClearFields (dc);
		finfo = dc_BuildFieldInfo (dc, nfield, fields);
		dc_SetUniformFieldSize (dc, size);
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
 * Set this data chunk up to use uniform-length fields.
 */
{
	FldInfo *finfo;

	if (dc_ChangeInfo (dc, "SetupUniformOrg"))
	{
		dc_ClearFields (dc);
		finfo = dc_BuildFieldInfo (dc, nfield, fields);
		dc_SetUniformOrg (dc, nelems);
	}
	dc_HintNSamples (dc, nsamples, TRUE);
}



void
dc_FixFieldSizes (dc, sizes)
DataChunk *dc;
int *sizes;
/*
 * Inform us that all of the fields will have the fixed size given in sizes,
 * where sizes[] corresponds to the FieldId array fids[].
 */
{
	FldInfo *finfo;
	int i;

	finfo = dc_ChangeInfo (dc, "FixFieldSizes");
	if (finfo)
	{
		for (i = 0; i < finfo->fi_NField; ++i)
			finfo->fi_Sizes[i] = sizes[i];
		finfo->fi_FixedFields = TRUE;
	}
}
#endif	/* --------------------------------------- CFG_DC_OLDFIELDS */




static FldInfo *
dc_BuildFieldInfo (dc, nfield, fields)
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Add fields to the field id array and hash table.
 */
{
	FldInfo *finfo = FIP(dc);
	int fld, n;
/*
 * Make sure someone's not trying to overstuff us with fields.
 * If so, only up to DC_MaxField fields will be accepted.
 */
	if (finfo->fi_NField + nfield > DC_MaxField)
	{
		msg_ELog(EF_PROBLEM, "%i is too many fields, max is %i",
			 finfo->fi_NField + nfield, DC_MaxField);
	}

	n = finfo->fi_NField;
	for (fld = 0; (fld < nfield) && (n < DC_MaxField); fld++)
	{
		finfo->fi_Fields[n] = fields[fld];
		finfo->fi_Types[n] = DCT_Float;
		finfo->fi_Sizes[n] = 0;
		finfo->fi_Offset[n] = 0;
		++n;
	}
/*
 * Create the hash table for finding the index of a given FieldId
 */
	dc_BuildIndexHash (finfo, finfo->fi_NField, n);
	finfo->fi_NField = n;
	return (finfo);
}




static void
dc_BuildIndexHash (finfo, begin, end)
FldInfo *finfo;
int begin;
int end;
/*
 * Initialize the hash table to the indices of all of our FieldIds.
 */
{
	int i, hash;

	/*
	 * Now pass through all of our fields, and place them in the 
	 * hash table.  Collisions are placed in the next available slot,
	 * where the next higher slot may circle around to the bottom.
	 */
	for (i = begin; i < end; ++i)
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
	if (! dc_ReqSubClass (dc, DCP_MetData, "BlockChanges"))
		return;
/*
 * Grab the field info structure, and set the block changes flag.
 */
	finfo = FIP(dc);
	finfo->fi_BlockChanges = TRUE;
}




/*
 * --------------------------------------------------------------------------
 * Field element type handling
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

	if ((idx = dc_GetIndex (finfo, fid)) >= 0)
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
	if (! dc_ReqSubClass (dc, DCP_MetData, method))
		return NULL;
/*
 * Grab the field info structure.
 */
	finfo = FIP(dc);
/*
 * Don't allow changes to the fields or types once data has been added
 */
	if (finfo->fi_NSample > 0)
	{
		msg_ELog (EF_PROBLEM, 
		  "%s: cannot change info after samples added", method);
		return NULL;
	}
/*
 * Forbid changes if blocked by the subclass
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
dc_SetFieldSizes (dc, nfield, fids, sizes)
DataChunk *dc;
int nfield;
FieldId *fids;
int *sizes;
/*
 * Inform the Met class that all of the fields will have the fixed size
 * given in sizes, where sizes[] corresponds to the FieldId array fids[].
 * It is the callers responsibility to make sure every field eventually
 * gets a size set, unless that size is zero.
 */
{
	FldInfo *finfo;

	finfo = dc_ChangeInfo (dc, "SetFieldSizes");
	if (finfo)
	{
		int i, idx;

		for (i = 0; i < nfield; ++i)
			if ((idx = dc_GetIndex (finfo, fids[i])) >= 0)
				finfo->fi_Sizes[idx] = sizes[i];
		finfo->fi_FixedFields = TRUE;
	}
}




void
dc_ReserveFieldSpace (dc, reserve)
DataChunk *dc;		/* subclass of DC_MetTypes 	*/
zbool reserve;		/* FALSE disables the pre-allocation and forces TOC */
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




inline DC_ElemType
dc_Type (dc, fid)
DataChunk *dc;
FieldId fid;
/* 
 * Return the element type for this field
 */
{
	int idx;
	FldInfo *finfo;

#if CFG_DC_CHECKCLASS
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClass (dc, DCP_MetData, "Type"))
		return (DCT_Unknown);
#endif
/*
 * Grab the field info structure, find the field, and return its type
 */
	finfo = FIP(dc);
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
	if (! dc_ReqSubClass (dc, DCP_MetData, "GetNField"))
		return (0);
/*
 * Grab the field info structure and return the number.
 */
	finfo = FIP(dc);
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
	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFields"))
		return (0);
/*
 * Grab the field info structure and return the stuff.
 */
	finfo = FIP(dc);
	if (nf)
		*nf = finfo->fi_NField;
	return ((finfo->fi_NField) ? finfo->fi_Fields : NULL);
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
	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFieldTypes"))
		return (0);
/*
 * Grab the field info structure and return the stuff.
 */
	finfo = FIP(dc);
	if (nf)
		*nf = finfo->fi_NField;
	return ((finfo->fi_NField) ? finfo->fi_Types : NULL);
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
	int i, tsize = 0, ssize;
	zbool uniform;
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




DataPtr
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
 *
 * Return a pointer to the storage location of this field in the first
 * sample written.  This pointer can be combined with uniform sample
 * strides to directly access this field's data for the given range
 * of samples.  Returns NULL on error.
 */
{
	FldInfo *finfo;
	int findex, samp;
	DataPtr base = NULL;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClass (dc, DCP_MetData, "AddMData"))
		return (NULL);
/*
 * Grab the field info structure and return the number.
 */
	finfo = FIP(dc);
	if (finfo->fi_NField == 0)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add data with no fields");
		return (NULL);
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
		return (NULL);
	}
/*
 * Set the time here, since dc_AddUniform(), dc_AddFixedField(), and 
 * dc_AddNonUniform() only tweak the data portion of the data chunk when
 * the sample already exists.
 */
	for (samp = 0; samp < nsamp; samp++)
		dc_SetTime (dc, samp + start, t + samp);
/*
 * Uniform and non-uniform data are handled differently, and hence separately.
 */
	if (data == DC_FillValues)
		base = NULL;
	else
		base = data;
	if (finfo->fi_Uniform)
		base = dc_AddUniform(dc, finfo, findex, t, size, start, 
				     nsamp, base);
	else if (finfo->fi_FixedFields)
		base = dc_AddFixedField(dc,finfo,findex,t, size, start, 
					nsamp, base);
	else
		base = dc_AddNonUniform(dc,finfo,findex,t, size, start, 
					nsamp, base);
	if (data == DC_FillValues)
	{
		int len, i;
		DataPtr ptr;

		for (i = start; i < start + nsamp; ++i)
		{
			ptr = dc_GetMData (dc, i, field, &len);
			dc_MetFillMissing (dc, field, (unsigned char *)ptr, 
					   len);
		}
	}
	return (base);
}




static void *
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
	void *base = NULL;

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
	/*
	 * After everything is allocated and the dust has settled, _then_ we
	 * can compute the 'base' pointer into the first sample for the field.
	 */
	space = dc_GetSample (dc, start, &len);
	if (space)
	{
		ft = (FieldTOC *) space;
		base = (void *)((char *)space + ft[findex].ft_Offset);
	}
	return (base);
}





static void *
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
	void *base = NULL;

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
	space = dc_GetSample (dc, start, &len);
	if (space)
		base = (void *)((char *)space + finfo->fi_Offset[findex]);
	return (base);
}





static void *
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
	void *base = NULL;
/*
 * Make sure the size is what we were promised.
 */
	if (size != finfo->fi_Size)
	{
		msg_ELog (EF_PROBLEM, "Size %d does not match finfo %d",
			size, finfo->fi_Size);
		return (NULL);
	}
/*
 * If they don't all exist, there better not be gaps in the middle.
 */
	if (start > finfo->fi_NSample)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add samp %d after %d exist",
			start, finfo->fi_NSample);
		return (NULL);
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
			dest = dc_GetSample (dc, samp, NULL);
	/*
	 * This absolutely, undubitably, definitely, certainly, without
	 * fail should never, ever happen, not even in a million years.
	 */
		if (! dest)
		{
			msg_ELog (EF_PROBLEM, 
				  "Sample %d disappeared anyway", samp);
			return (NULL);
		}
		if (data)
		{
			memcpy ((char *) dest + offset, data, finfo->fi_Size);
			data = (char *) data + finfo->fi_Size;
		}
	}
	dest = dc_GetSample (dc, start, NULL);
	if (dest)
		base = (void *)((char *)dest + offset);
	return (base);
}



int
dc_FillMissing (dc, when, fid, size, start, nsamp)
DataChunk *dc;
ZebTime *when;
FieldId fid;
int size;
int start;
int nsamp;
/*
 * Essentially an alternate interface to dc_AddMData() which stores missing
 * values.  Use the class methods to make sure space is allocated for this
 * field for these samples.  Then loop through the samples getting the
 * location of each one and filling the field's data with missing
 * values.  Return zero if any of the fills fail, non-zero otherwise.
 */
{
	int i;
	void *ptr;
	int len;
	int ret = 1;

	dc_AddMData (dc, when, fid, size, start, nsamp, NULL);
	for (i = start; i < start + nsamp; ++i)
	{
		ptr = dc_GetMData (dc, i, fid, &len);
		ret &= dc_MetFillMissing (dc, fid, (unsigned char *)ptr, len);
	}
	return (ret);
}




int
dc_MetFillMissing (dc, fid, at, len)
DataChunk *dc;
FieldId fid;
unsigned char *at;
int len;
/*
 * Given the location and size of a field's contiguous array of elements to
 * add, fill the space with bad values instead.  The location is 'at',
 * the number of elements is 'len'.
 * If we don't have a bad value for the particular field or type,
 * fill the memory with zeros.  Return non-zero if we succeeded in filling
 * with bad values.  Return zero when an error occurs.
 */
{
	int i;
	DC_ElemType type;
	int size = 0;
	void *ptr = NULL;

	type = dc_Type (dc, fid);
	if (type == DCT_Unknown)
	{
		msg_ELog (EF_PROBLEM, "attempt to fill unknown field %s", 
			  F_GetFullName (fid));
		return (0);
	}
	ptr = dc_FindFieldBadval (dc, fid);
	if (! ptr && ((ptr = dc_DefaultBadval (type)) != NULL))
	{
		dc_SetFieldBadval (dc, fid, ptr);
	}
	if (ptr)
	{
		size = dc_SizeOfType (type);
		for (i = 0; i+size <= len; i += size)
			memcpy (at + i, ptr, size);
	}
	else
	{
		msg_ELog (EF_PROBLEM, "fill field %d: %s",
			  fid, "no default badval, using zero");
		memset (at, 0, len);
	}
	return ((ptr != NULL));
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
#if CFG_DC_CHECKCLASS
/*
 * Make sure this one of our subclasses.
 */
	if (! dc_ReqSubClass (dc, DCP_MetData, "GetFieldIndex"))
		return (-1);
#endif
/*
 * Use our private method to actually find the field.
 */
	return (dc_GetIndex (FIP(dc), field));
}




DataPtr
dc_GetMData (dc, sample, field, len)
DataChunk *dc;
int sample;
FieldId field;
int *len;
/*
 * Find a data sample in this DC.
 */
{
	FldInfo *finfo;
	FieldTOC *ft;
	int findex;
	DataPtr data;
#if CFG_DC_CHECKCLASS
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClass (dc, DCP_MetData, "GetMData"))
		return (NULL);
#endif
/*
 * Grab the field info structure.
 */
	finfo = FIP(dc);
/*
 * Find this sample.
 */
	if ((data = dc_GetSample (dc, sample, NULL)) == NULL)
		return (NULL);
/*
 * Find the index of our field.
 */
	if ((findex = dc_GetIndex (finfo, field)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Attempt to get field %s not in finfo",
			  F_GetFullName (field));
		return (NULL);
	}
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






DataPtr *
dc_GetMVector (dc, sample, nfield, fields, fdata, len)
DataChunk *dc;
int sample;
int nfield;
FieldId *fields;
DataPtr *fdata;
int *len;
/*
 * Find a data sample in this DC and return a vector of pointers to
 * the field data for each of the fields in the fields[] array.
 * Returns the fdata array if successful, NULL if not.
 */
{
	FldInfo *finfo;
	FieldTOC *ft;
	int findex = -1;
	int f;
	int fsize;
	int foffset = 0;
	DataPtr data;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClass (dc, DCP_MetData, "GetMVector"))
		return (NULL);
/*
 * Grab the field info structure.
 */
	finfo = FIP(dc);
/*
 * Find this sample.
 */
	if ((data = dc_GetSample (dc, sample, NULL)) == NULL)
		return (NULL);
/*
 * Loop for each field.  If not found the field's pointer is set to NULL.
 */
	for (f = 0; f < nfield; ++f)
	{
		if (len) len[f] = 0;
		fdata[f] = NULL;
		fsize = 0;
		if ((fields[f] == BadField) ||
		    (findex = dc_GetIndex (finfo, fields[f])) < 0)
			continue;
		/*
		 * Now that we have everything, we're set.
		 */
		if (finfo->fi_Uniform)
		{
			fsize = finfo->fi_Size;
			foffset = findex*finfo->fi_Size;
		}
		else if (finfo->fi_FixedFields)
		{
			/*
			 * For fixed fields, the index is in the offset table
			 */
			fsize = finfo->fi_Sizes[findex];
			foffset = finfo->fi_Offset[findex];
		}
		else
		{
			/*
			 * In the non-uniform case, find the TOC and return
			 * the info from there.
			 */
			ft = (FieldTOC *) data;
			if (ft[findex].ft_Len > 0)
			{
				fsize = ft[findex].ft_Len;
				foffset = ft[findex].ft_Offset;
			}
		}
		if (len) len[f] = fsize;
		/*
		 * Fields with a zero size are left with null pointers.
		 * This catches static fields, which are not stored in the
		 * data samples.
		 */
		if (fsize > 0)
			fdata[f] = (void *)((char *) data + foffset);
	}
	return (fdata);
}




int
dc_SampleStride (dc, stride)
DataChunk *dc;
int *stride;
/*
 * Checks whether this datachunk's fields can be accessed directly using
 * a constant stride between samples.  First, the samples must be stored
 * in the same order they are indexed, and second, the metdata field layout
 * must be FixedFields or Uniform.  If those two criteria are met, the
 * sample size is the sum of the field sizes, which is also the stride
 * from one field's sample data to the next.  If the criteria are not met,
 * return zero and leave stride unchanged.
 */
{
	FldInfo *finfo;
	int ssize;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClass (dc, DCP_MetData, "SampleStride"))
		return (0);
/*
 * Grab the field info structure.  If no data has been stored, abort.
 */
	finfo = FIP(dc);
	if (finfo->fi_NSample == 0)
		return (0);
/*
 * If our field layout is not uniform, no sense trying the rest.
 */
	if (! finfo->fi_FixedFields && ! finfo->fi_Uniform)
		return (0);
/*
 * Check for 'index order'.  This should be as accurate as actually looping
 * through all the samples and checking them explicitly, but much faster.
 */
	if (! dc_ContiguousSamples (dc))
		return (0);
/*
 * Determine what our sample stride should be.
 */
	if (finfo->fi_Uniform)
	{
		ssize = finfo->fi_NField*finfo->fi_Size;
	}
	else /* (finfo->fi_FixedFields) */
	{
		ssize = finfo->fi_Offset[finfo->fi_NField - 1];
		ssize += finfo->fi_Sizes[finfo->fi_NField - 1];
		ssize = (int) ALIGN(ssize,DC_ElemTypeMaxSize);
	}
#ifdef notdef
	{
	DataPtr data, next;
	int i;
	int nsample;
/*
 * Now loop through the samples in index order, and make sure each
 * starts exactly 'ssize' bytes from the previous.
 */
	nsample = finfo->fi_NSample;
	data = dc_GetSample (dc, 0, NULL);
	for (i = 1; i < nsample; ++i)
	{
		next = dc_GetSample (dc, i, NULL);
		if (next != (char *)data + ssize)
			return (0);
		data = next;
	}
	msg_ELog (EF_DEBUG, "sample stride check succeeded: %d bytes", ssize);
	}
#endif
	*stride = ssize;
	return (1);
}



/* ----------------------------------------------------------------------- */
/* MetData class methods */


static DataChunk *
md_Create (dc)
DataChunk *dc;
/*
 * Initialize the field info part of our instance stucture.
 */
{
	FldInfo *finfo = FIP(dc);
	int i;

	finfo->fi_NField = 0;
	finfo->fi_BlockChanges = FALSE;
	finfo->fi_Uniform = FALSE;
	finfo->fi_UniformOrg = FALSE;
	finfo->fi_Size = 0;
	finfo->fi_FixedFields = FALSE;
	finfo->fi_ReserveSpace = TRUE;	/* pre-arrange space if possible */
	finfo->fi_NSample = 0;
	for (i = 0; i < HASH_SIZE; ++i)
		finfo->fi_HashIndex[i] = -1;	/* fill with empty slots */
	/*
	 * Make sure HASH_SIZE has not gotten out of sync with DC_MaxField.
	 */
	if (DC_MaxField + 1 > HASH_SIZE)
	{
		msg_ELog (EF_PROBLEM, 
		  "Field index hash table too small, check MD_HASH_SIZE defn");
	}
	return (dc);
}




static void
md_Dump (dc)
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
	if (! dc_ReqSubClass (dc, DCP_MetData, "GetMData"))
		return;
/*
 * Grab the field info structure.
 */
	finfo = FIP(dc);
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
	dc_DumpFieldAttributes (dc, finfo->fi_Fields, finfo->fi_NField);
}




static void
md_Serialize (DataChunk *dc)
/*
 * Store information about our fields into our private attributes.
 */
{
	dc_StoreFieldDefs (dc);
}



static void
md_Localize (DataChunk *dc)
/*
 * Define new field ids using information stored in our private attributes,
 * then rebuild the hash table with the new ids.
 */
{
	int i;
	FldInfo *finfo = FIP(dc);

	dc_ClearHash (finfo);
	for (i = 0; i < finfo->fi_NField; ++i)
	{
		finfo->fi_Fields[i] = dc_RestoreFieldDef (dc, i);
	}
	dc_BuildIndexHash (finfo, 0, finfo->fi_NField);
}

