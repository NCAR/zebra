/*
 * Definitions and routines for accessing classes by ID, which requires
 * a lookup table for all of the class structures.  Programs which do not
 * need to de-serialize (localize) datachunks should not need to link with
 * this module, and hence should only need the modules for the classes they
 * are using.
 */
#include <stdio.h>

#include <defs.h>
#include <message.h>

#include "DataStore.h"
#include "DataChunkP.h"

RCSID("$Id: dc_All.c,v 3.3 1998-04-27 21:41:04 corbet Exp $")

static RawClass *ClassTable[] =
{
	0,			/* DCC_None		*/
	&RawMethods,		/* DCC_Raw		*/
	&TranspMethods,		/* DCC_Transparent	*/
	&BoundaryMethods,	/* DCC_Boundary		*/
	&MetDataMethods,	/* DCC_MetData		*/
	&ScalarMethods,		/* DCC_Scalar		*/
	&IRGridMethods,		/* DCC_IRGRID		*/
	&RGridMethods,		/* DCC_RGRID		*/
	&ImageMethods,		/* DCC_Image		*/
	&LocationMethods,	/* DCC_Location		*/
	&NSpaceMethods,		/* DCC_NSpace		*/
	&PolarMethods,		/* DCC_Polar		*/
};



DataClassP
dc_ClassP (id)
DataClassID id;
{
	return (ClassTable[id]);
}




DataClassID
dc_SuperClass (id)
DataClassID id;
{
	DataClassP super = (ClassTable[id]->dcm_Super);
	return ((super) ? super->dcm_ClassId : DCC_None);
}



DataChunk *
dc_CreateDC (id)
DataClass id;
/*
 * Create a datachunk with this class id.
 */
{
	return (dc_Create (dc_ClassP(id)));
}



bool
dc_IsSubClassOf (cid, sid)
DataClassID cid, sid;
{
	return (dc_IsSubClass (dc_ClassP(cid), dc_ClassP(sid)));
}



static void
dc_LocalChain (dc, class, super)
DataChunk *dc;
DataClassP class;
DataClassP super;
{
	if (super != DCP_Raw)
		dc_LocalChain (dc, class, dc_Super (super));
	if (super->dcm_Localize)
		(*(super->dcm_Localize)) (dc);
}




DataChunk *
dc_Localize (dc)
DataChunk *dc;
/*
 * Localize this datachunk by converting class and ADE references by ID
 * into more efficient pointers.  Start with the raw part by filling in
 * the pointer to the class structure from the class id, then call each
 * subclass' serialize method.
 */
{
	dc->dc_ClassP = (DataClassP) (ClassTable[dc->dc_Class]);
	dc_LocalChain (dc, dc->dc_ClassP, dc->dc_ClassP);
	return (dc);
}





DataChunk *
dc_Copy (src)
DataChunk *src;
{
	DataChunk *dest;
	DataClassP class = src->dc_ClassP;

	/* get the source chunk to register its ADEs */
	dc_Serialize (src);

	/* copy datachunk, data, and ADEs into new memory */
	dest = (DataChunk *) malloc (class->dcm_Size);
	memcpy (dest, src, class->dcm_Size);
	dc_ClearADE (dest);
	dc_CopyADE (dest, src);

	if (dest->dc_Data && dest->dc_DataLen)
	{
		dest->dc_Data = (DataPtr) malloc (src->dc_DataLen);
		memcpy (dest->dc_Data, src->dc_Data, src->dc_DataLen);
	}

	/* update internal pointers in the new datachunk */
	dc_Localize (dest);
	return (dest);
}



