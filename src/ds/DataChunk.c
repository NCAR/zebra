/*
 * General DataChunk management routines, and code for the DCC_Raw object.
 */
static char *rcsid = "$Id: DataChunk.c,v 1.1 1991-11-16 01:18:54 corbet Exp $";

/*		Copyright (C) 1987,88,89,90,91 by UCAR
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
# include "DataChunk.h"
# include "DataChunkP.h"


/*
 * The class methods structure for the raw class.
 */
# ifdef __STDC__
	DataChunk *Dc_RawCreate (DataChunkClass);
	void Dc_RawDestroy (DataChunk *);
	void Dc_RawAdd (DataChunk *, int);
	void Dc_RawDump (DataChunk *);
# else
	/* ....later.....*/
# endif

RawDCClass RawMethods =
{
	"Raw",
	DCC_None,		/* No superclass	*/
	Dc_RawCreate,
	Dc_RawDestroy,
	Dc_RawAdd,
	Dc_RawDump
};


/*
 * This is the table which allows us to find individual class records.  It
 * is dependent on the definition of DataChunkClass in DataChunk.h
 */
extern RawDCClass TranspMethods, BoundaryMethods;

static RawDCClass *ClassTable[] =
{
	0,			/* DCC_None		*/
	&RawMethods,		/* DCC_Raw		*/
	&TranspMethods,		/* DCC_Transparent	*/
	&BoundaryMethods,	/* DCC_Boundary	*/
};





bool
dc_IsSubClassOf (class, superclass)
DataChunkClass class, superclass;
/*
 * Return TRUE iff the given class is a subclass of "superclass".
 */
{
	while (class != DCC_None)
	{
		if (class == superclass)

return (TRUE);
		class = ClassTable[class]->dcm_Parent;
	}
	return (FALSE);
}





bool
ds_ReqSubClassOf (class, superclass, op)
DataChunkClass class, superclass;
char *op;
/*
 * Return TRUE iff the required subclass relationship exists.  If it does
 * not, a message is logged.
 */
{
	if (dc_IsSubClassOf (class, superclass))
		return (TRUE);
	msg_ELog (EF_PROBLEM, "%s not subclass of %s for op '%s'", 
		ClassTable[class]->dcm_Name,
		ClassTable[superclass]->dcm_Name, op);
	return (FALSE);
}





DataChunkClass
dc_GetSuperClass (class)
DataChunkClass class;
/*
 * Return the superclass of this class.
 */
{
	return ((class == DCC_None) ? DCC_None: ClassTable[class]->dcm_Parent);
}





DataChunk *
dc_CreateDC (class)
DataChunkClass class;
/*
 * Create a data chunk of this class.
 */
{
	if (class != DCC_None)
		return ((*ClassTable[class]->dcm_Create) (class));
	msg_ELog (EF_EMERGENCY, "Tried to create DC with class None!");
	return (0);
}




void
dc_DestroyDC (dchunk)
DataChunk *dchunk;
/*
 * Get rid of this data chunk.
 */
{
	DataChunkClass class = dchunk->dc_Class;
/*
 * Move up the class hierarchy until a destroy method is found.
 */
	while (class != DCC_None && 
			ClassTable[class]->dcm_Destroy == InheritMethod)
		class = ClassTable[class]->dcm_Parent;
	if (class != DCC_None)
		(*ClassTable[class]->dcm_Destroy) (dchunk);
}





void
dc_DumpDC (dc)
DataChunk *dc;
/*
 * Dump out this data chunk.
 */
{
	DataChunkClass class = dc->dc_Class;
/*
 * Go through and dump this one at every level which is prepared for it.
 */
	while (class != DCC_None)
	{
		if (ClassTable[class]->dcm_Dump)
			(*ClassTable[class]->dcm_Dump) (dc);
		class = ClassTable[class]->dcm_Parent;
	}
}





/*
 * AuxData routines.
 */

/*
 * A lookaside list of AuxDataChain structures is kept around,
 * to save on malloc/free stuff.
 */
static AuxDataChain AD_Free = NULL;


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
	ade->dca_Class = DCC_None;
	ade->dca_Next = AD_Free;
	AD_Free = ade;
}




void
dc_AddADE (dc, data, class, subtype, len, free)
DataChunk *dc;
DataPointer data;
DataChunkClass class;
int subtype, len, free;
/*
 * Add an AuxData entry to this data chunk.
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
	AuxDataChain ade = dc_GetADC ();

	ade->dca_Class = class;
	ade->dca_SubType = subtype;
	ade->dca_Len = len;
	ade->dca_Data = data;
	ade->dca_Next = dc->dc_AuxData;
	ade->dca_Free = free;
	dc->dc_AuxData = ade;
}





static inline AuxDataChain
dc_IntFindADE (dc, class, subtype)
DataChunk *dc;
DataChunkClass class;
int subtype;
/*
 * Find the AuxData entry corresponding to this stuff.
 */
{
	AuxDataChain ade;

	for (ade = dc->dc_AuxData; ade; ade = ade->dca_Next)
		if (ade->dca_Class == class && ade->dca_SubType == subtype)
			return (ade);
	return (NULL);
}





DataPointer 
dc_FindADE (dc, class, subtype, len)
DataChunk *dc;
DataChunkClass class;
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

	if (! ade)
		return (NULL);

	if (len)
		*len = ade->dca_Len;
	return (ade->dca_Data);
}





void
dc_ChangeADE (dc, data, class, subtype, len)
DataChunk *dc;
DataPointer data;
DataChunkClass class;
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
dc_IntZapADE (dc, ade)
DataChunk *dc;
AuxDataChain ade;
/*
 * Get rid of this ADE.
 */
{
	AuxDataChain zap, last;
/*
 * First, we need to remove it from the list.
 */
	if (dc->dc_AuxData == ade)
		dc->dc_AuxData = ade->dca_Next;
	else
	{
	/*
	 * Find this entry in the chain.
	 */
		last = dc->dc_AuxData;
		for (zap = dc->dc_AuxData->dca_Next; zap; zap = zap->dca_Next)
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
/*
 * If called for, free up the data, then release the chain structure.
 */
	if (ade->dca_Free)
		free (ade->dca_Data);
	dc_FreeADC (ade);
}




void
dc_RemoveADE (dc, class, subtype)
DataChunk *dc;
DataChunkClass class;
int subtype;
/*
 * Remove the AuxData entry corresponding to the given codes from
 * this data object.
 */
{
	AuxDataChain ade = dc_IntFindADE (dc, class, subtype);

	if (ade)
		dc_IntZapADE (dc, ade);
}





/************
 *
 * Stuff below here is the definition of the RAW data chunk class.
 *
 ************/




static DataChunk *
Dc_RawCreate (class)
DataChunkClass class;
/*
 * Create a raw data chunk.
 */
{
	DataChunk *dc;
/*
 * Sanity check.
 */
	if (class != DCC_Raw)
	{
		msg_ELog (EF_EMERGENCY, "Dc_RawCreate can't make %d", class);
		return (0);
	}
/*
 * Create our new data chunk, and fill it in.
 */
	dc = ALLOC (DataChunk);
	dc->dc_Class = DCC_Raw;
	dc->dc_Platform = BadPlatform;	/* They have to set this themselves */
	dc->dc_Data = (DataPointer) 0;	/* No data yet */
	dc->dc_DataLen = 0;
	dc->dc_AuxData = (AuxDataChain) 0;
/*
 * That's it!
 */
	return (dc);
}





static void
Dc_RawDestroy (dc)
DataChunk *dc;
/*
 * Get rid of this data object.
 */
{
/*
 * Free up the data array.
 */
	if (dc->dc_DataLen > 0)
		free ((char *) dc->dc_Data);
/*
 * Get rid of any remaining AuxData entries.
 */
	while (dc->dc_AuxData)
		dc_IntZapADE (dc, dc->dc_AuxData);
/*
 * Finally, zap the data chunk itself.
 */
	free ((char *) dc);
}





void
Dc_RawAdd (dc, len)
DataChunk *dc;
int len;
/*
 * Modify this data chunk to hold "len" more data.
 */
{
	if (dc->dc_DataLen > 0)
		dc->dc_Data = (DataPointer) realloc (dc->dc_Data,
						dc->dc_DataLen + len);
	else
		dc->dc_Data = (DataPointer) malloc (len);
	dc->dc_DataLen += len;
}





void
Dc_RawDump (dc)
DataChunk *dc;
/*
 * Dump out this data chunk.
 */
{
	printf ("RAW class, class = '%s', platform %d, data len %d\n",
		ClassTable[dc->dc_Class]->dcm_Name, dc->dc_Platform,
		dc->dc_DataLen);
}
