/*
 * DataChunk auxiliary data entry routines.
 */

#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <defs.h>
#include <message.h>

#include "DataStore.h"
#include "DataChunkP.h"


/*
 * AuxData routines.
 */
static void dc_ReleaseADE FP ((AuxDataChain ade));
static void dc_IntZapADE FP ((DataChunk *dc, DataClassP, AuxDataChain ade));

/*
 * A lookaside list of AuxDataChain structures is kept around,
 * to save on malloc/free stuff.
 */
static AuxDataChain AD_Free = NULL;


void
dc_ForceClosure()
/*
 * Get rid of any memory we're hanging on to, such as the ADE free chain
 */
{
	AuxDataChain next, ade;

	next = AD_Free;
	while (next)
	{
		ade = next;
		next = ade->dca_Next;
		free (ade);
	}
	AD_Free = NULL;
}




void
dc_ClearADE (dc)
DataChunk *dc;
{
	memset ((void *)dc->dc_AuxData, 0, sizeof(dc->dc_AuxData));
}



static inline AuxDataChain 
dc_GetADC ()
/* 
 * Return a free AuxDataEntry structure.
 */
{
	AuxDataChain ret;

	if (AD_Free)
	{
		ret = AD_Free;
		AD_Free = ret->dca_Next;
	}
	else
		ret = ALLOC (AuxDataEntry);
	return (ret);
}



static inline void
dc_FreeADC (ade)
AuxDataChain ade;
/*
 * Free up this entry.
 */
{
	ade->dca_ClassId = DCID_None;
	ade->dca_Next = AD_Free;
	AD_Free = ade;
}




DataPtr
dc_AddADE (dc, data, class, subtype, len, free)
DataChunk *dc;
DataPtr data;
DataClassP class;
int subtype, len, free;
/*
 * Add an AuxData entry to this data chunk.  If an ADE of this class and
 * subtype already exists, remove it before adding this one.
 *
 * Entry:
 *	DC	is the data chunk to be modified.
 *	DATA	is the auxiliary data to add.
 *	CLASS	is the class that uses this entry.
 *	SUBTYPE	is a class-specific type code.
 *	LEN	is the length of DATA.
 *	FREE	is TRUE iff DATA should be freed when the data object is
 *		destroyed, if the class has not already removed it.
 * Exit:
 *	The data chain entry has been added.
 */
{
	AuxDataChain ade = NULL;
	AuxDataChain next, prev;
	int hash = ADE_HASH_TYPE(subtype);
	int depth = class->dcm_Depth;

	/*
	 * Adding zero length or null data is the same as not adding anything.
	 */
	if (data && len > 0)
	{
		/*
		 * Rather than insert new ADE's into the head of the chain,
		 * add them to the back.  The more frequently accessed
		 * ADE's are added first, and hence they should stay at the
		 * front.
		 */
		ade = dc_GetADC ();
		ade->dca_ClassId = dc_ClassID (class);
		ade->dca_SubType = subtype;
		ade->dca_Len = len;
		ade->dca_Data = data;
		ade->dca_Free = free;
		ade->dca_Next = NULL;
	}

	/*
	 * If we find this subtype, replace it.  Otherwise the new ade gets
	 * added to the end.
	 */
	next = dc->dc_AuxData[depth][hash];
	prev = NULL;
	while (next && (next->dca_SubType != subtype))
	{
		prev = next;
		next = next->dca_Next;
	}
	if (next)
	{
		/* replace or remove this ade */
		if (ade)
			ade->dca_Next = next->dca_Next;
		else
			ade = next->dca_Next;
		dc_ReleaseADE (next);
	}
	if (! prev)
		dc->dc_AuxData[depth][hash] = ade;
	else
		prev->dca_Next = ade;
	return (data);
}





static inline AuxDataChain
dc_IntFindADE (dc, class, subtype)
DataChunk *dc;
DataClassP class;
int subtype;
/*
 * Find the AuxData entry corresponding to this stuff.
 */
{
	AuxDataChain ade;
	int depth = class->dcm_Depth;

/*
 * Find the ADE chain to search given the class and subtype.  We know we'll
 * have the correct class, so we only need to test subtype.
 */
	ade = dc->dc_AuxData[depth][ADE_HASH_TYPE(subtype)];
	for ( ; ade; ade = ade->dca_Next)
		if (ade->dca_SubType == subtype)
			return (ade);
	return (NULL);
}





void
dc_StatsADE (dc, count, len)
DataChunk *dc;
int *count;
int *len;
/*
 * Find the AuxData entry corresponding to this stuff.
 */
{
	AuxDataChain ade;
	int i, j;

/*
 * Count the number of ADE's we're holding and total the data space
 */
	*len = 0;
	*count = 0;
	for (i = 0; i < ADE_DCC_LEVELS; ++i)
		for (j = 0; j < ADE_HASH_SIZE; ++j)
		{
			ade = dc->dc_AuxData[i][j];
			while (ade)
			{
				++(*count);
				*len += ade->dca_Len;
				ade = ade->dca_Next;
			}
		}
}





DataPtr 
dc_FindADE (dc, class, subtype, len)
DataChunk *dc;
DataClassP class;
int subtype, *len;
/*
 * Search for an AuxData entry in this data object.
 * Entry:
 *	DC	is the data chunk
 *	CLASS	is the class owning the AuxData entry
 *	SUBTYPE	is the class-specific type code
 * Exit:
 *	If the entry is found then:
 *		the return value is the data of interest
 *		if len is non-NULL then *len is the length of the data
 *	else
 *		the return value is NULL.
 */
{
	AuxDataChain ade = dc_IntFindADE (dc, class, subtype);

	if (len)
		*len = (ade) ? ade->dca_Len : 0;
	return ((ade) ? ade->dca_Data : NULL);
}





void
dc_ChangeADE (dc, data, class, subtype, len)
DataChunk *dc;
DataPtr data;
DataClassP class;
int subtype, len;
/*
 * Change the data stored in this ADE.  This routine only been be called
 * if the physical memory holding this ADE has changed.
 */
{
	AuxDataChain ade = dc_IntFindADE (dc, class, subtype);

	if (! ade)
		msg_ELog (EF_PROBLEM, "ChangeADE (%d, %d) on missing ADE",
				class, subtype);
	else
	{
		ade->dca_Data = data;
		ade->dca_Len = len;
	}
}



static void
dc_ReleaseADE (ade)
AuxDataChain ade;
/*
 * If called for, free up the data, then release the chain structure.
 */
{
	if (ade->dca_Free)
		free (ade->dca_Data);
	dc_FreeADC (ade);
}



static void
dc_IntZapADE (dc, class, ade)
DataChunk *dc;
DataClassP class;
AuxDataChain ade;
/*
 * Get rid of this ADE.  'top' is the head pointer of the chain containing
 * this ADE.  This could change if we're deleting the first ADE in the chain.
 */
{
	AuxDataChain zap, last;
	int hash = ADE_HASH_TYPE(ade->dca_SubType);
	int depth = class->dcm_Depth;
	AuxDataChain top = dc->dc_AuxData[depth][hash];
/*
 * First, we need to remove it from the list.
 */
	if (top == ade)
		dc->dc_AuxData[depth][hash] = ade->dca_Next;
	else
	{
	/*
	 * Find this entry in the chain.
	 */
		last = top;
		for (zap = top->dca_Next; zap; zap = zap->dca_Next)
		{
			if (zap == ade)
				break;
			last = zap;
		}
	/*
	 * Possibly it's not there.  But if it is, take it out.
	 */
	 	if (! zap)
			msg_ELog (EF_PROBLEM, "Asked to zap bad ADE 0x%x",ade);
		else
			last->dca_Next = ade->dca_Next;
	}
	dc_ReleaseADE (ade);
}




void
dc_RemoveADE (dc, class, subtype)
DataChunk *dc;
DataClassP class;
int subtype;
/*
 * Remove the AuxData entry corresponding to the given codes from
 * this data object.
 */
{
	AuxDataChain ade = dc_IntFindADE (dc, class, subtype);

	if (ade)
		dc_IntZapADE (dc, class, ade);
}




void
dc_DestroyADE (dc)
DataChunk *dc;
/*
 * Get rid of any AuxData entries by working our way up the
 * class hierarchy for this datachunk.
 */
{
	DataClassP class = dc->dc_ClassP;

	while (class)
	{
		AuxDataChain *ade = dc->dc_AuxData[class->dcm_Depth];
		int j;

		for (j = 0; j < ADE_HASH_SIZE; ++j)
		{
			while (ade[j])
				dc_IntZapADE (dc, class, ade[j]);
		}
		class = dc_Super (class);
	}
}





void
dc_CopyADE (target, dc)
DataChunk *target;
DataChunk *dc;
/*
 * Copy all AuxData entries from one chunk to the other using dc_AddADE.
 * If the source ADEs are not meant to augment target ADEs, then first call
 * dc_ClearADE or dc_DestroyADE according to whether the target chunk's
 * ADEs should be freed normally or just erased.
 */
{
	DataClassP class = dc->dc_ClassP;

	while (class)
	{
		AuxDataChain src;
		int j;

		for (j = 0; j < ADE_HASH_SIZE; ++j)
		{
			src = dc->dc_AuxData[class->dcm_Depth][j];
			while (src)
			{
				DataPtr data = (DataPtr) malloc (src->dca_Len);
				memcpy (data, src->dca_Data, src->dca_Len);
				dc_AddADE (target, data, class,
					   src->dca_SubType, src->dca_Len,
					   src->dca_Free);
				src = src->dca_Next;
			}
		}
		class = dc_Super (class);
	}
}



