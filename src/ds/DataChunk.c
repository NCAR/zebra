/*
 * General DataChunk management routines, and code for the DCC_Raw object.
 */

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
# include "DataChunkP.h"
MAKE_RCSID ("$Id: DataChunk.c,v 3.6 1994-01-26 11:24:23 granger Exp $")

/*
 * ADE Codes for the raw data object.
 */
# define ST_GLOBATTR	3822

/*
 * The class methods structure for the raw class.
 */
static DataChunk *Dc_RawCreate FP((DataClass));
static void Dc_RawDestroy FP((DataChunk *));
void Dc_RawDump FP((DataChunk *));


RawDCClass RawMethods =
{
	"Raw",
	DCC_None,		/* No superclass	*/
	0,			/* class depth		*/
	Dc_RawCreate,
	Dc_RawDestroy,
	Dc_RawAdd,
	Dc_RawDump
};


/*
 * This is the table which allows us to find individual class records.  It
 * is dependent on the definition of DataClass in DataChunk.h
 */
extern RawDCClass TranspMethods, BoundaryMethods, MetDataMethods;
extern RawDCClass ScalarMethods, IRGridMethods, RGridMethods;
extern RawDCClass ImageMethods, LocationMethods, NSpaceMethods;

/*
 * We export the ClassTable to allow subclasses to directly call superclass
 * create methods through the DC_ClassCreate macro.
 */
RawDCClass *ClassTable[] =
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
};


bool _CheckClass = TRUE;


void
dc_CheckClass (on)
bool on;
{
	_CheckClass = on;
}



bool
dc_IsSubClassOf (class, superclass)
DataClass class, superclass;
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
_dc_ReqSubClassOf (class, superclass, op)
DataClass class, superclass;
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





DataClass
dc_GetSuperClass (class)
DataClass class;
/*
 * Return the superclass of this class.
 */
{
	return ((class == DCC_None) ? DCC_None: ClassTable[class]->dcm_Parent);
}





DataChunk *
dc_CreateDC (class)
DataClass class;
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
	DataClass class = dchunk->dc_Class;
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
	DataClass class = dc->dc_Class;
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
DataPtr data;
DataClass class;
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
	AuxDataChain next;
	int hash = ADE_HASH_TYPE(subtype);
	int depth = ClassTable[class]->dcm_Depth;

	/*
	 * Rather than insert new ADE's into the head of the chain, add
	 * them to the back.  The more frequently accessed ADE's are added
	 * first, and hence they should stay at the front.
	 */
	ade->dca_Class = class;
	ade->dca_SubType = subtype;
	ade->dca_Len = len;
	ade->dca_Data = data;
	ade->dca_Free = free;
	ade->dca_Next = NULL;

	next = dc->dc_AuxData[depth][hash];
	if (!next)
		dc->dc_AuxData[depth][hash] = ade;
	else
	{
		while (next->dca_Next)
			next = next->dca_Next;
		next->dca_Next = ade;
	}
}





static inline AuxDataChain
dc_IntFindADE (dc, class, subtype)
DataChunk *dc;
DataClass class;
int subtype;
/*
 * Find the AuxData entry corresponding to this stuff.
 */
{
	AuxDataChain ade;
	int depth = ClassTable[class]->dcm_Depth;

/*
 * Find the ADE chain to search given the class and subtype.  We know we'll
 * have the correct class, so we only need to test subtype.
 */
	ade = dc->dc_AuxData[depth][ADE_HASH_TYPE(subtype)];
	for ( ; ade; ade = ade->dca_Next)
		if (/* ade->dca_Class == class && */ ade->dca_SubType == subtype)
			return (ade);
	return (NULL);
}





static void
dc_IntStatsADE (dc, count, len)
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
DataClass class;
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
DataPtr data;
DataClass class;
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
 * Get rid of this ADE.  'top' is the head pointer of the chain containing
 * this ADE.  This could change if we're deleting the first ADE in the chain.
 */
{
	AuxDataChain zap, last;
	DataClass class = ade->dca_Class;
	int hash = ADE_HASH_TYPE(ade->dca_SubType);
	int depth = ClassTable[class]->dcm_Depth;
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
DataClass class;
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
DataClass class;
/*
 * Create a raw data chunk.
 */
{
	DataChunk *dc;
	int i, j;
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
	dc->dc_Data = (DataPtr) 0;	/* No data yet */
	dc->dc_DataLen = 0;
	memset ((void *)dc->dc_AuxData, 0, sizeof(dc->dc_AuxData));
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
	int i, j;
/*
 * Free up the data array.
 */
	if (dc->dc_DataLen > 0)
		free ((char *) dc->dc_Data);
/*
 * Get rid of any remaining AuxData entries.  No sense in checking the
 * AuxData slots for class DCC_None.
 */
	for (i = 0; i < ADE_DCC_LEVELS; ++i)
		for (j = 0; j < ADE_HASH_SIZE; ++j)
		{
			while (dc->dc_AuxData[i][j])
				dc_IntZapADE (dc, dc->dc_AuxData[i][j]);
		}
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
		dc->dc_Data = (DataPtr) realloc (dc->dc_Data,
						dc->dc_DataLen + len);
	else
		dc->dc_Data = (DataPtr) malloc (len);
	if (dc->dc_Data == NULL)
		msg_ELog (EF_EMERGENCY, 
		  "DC, class %s, plat '%s', malloc failed!",
		  ClassTable[dc->dc_Class]->dcm_Name, 
		  ds_PlatformName(dc->dc_Platform));
	dc->dc_DataLen += len;
}






static int
dc_PrintAttr (key, value, nval, type, arg)
char *key;
void *value;
int nval;
DC_ElemType type;
void *arg;
/*
 * Print out an attribute value.
 */
{
	int i;

	if (nval && (type == DCT_String))
	{
		printf ("\t%s --> '%s'\n", key, (char *)value);
		return (0);
	}
	printf ("\t%s --> ", key);
	for (i = 0; i < nval; ++i)
	{
		printf ("%s%s", dc_ElemToString(value, type),
			(i == nval - 1) ? "\n" : ", ");
		value = (char *)value + dc_SizeOfType (type);
	}
	if (nval == 0)
		printf ("\n");
	return (0);
}





void
Dc_RawDump (dc)
DataChunk *dc;
/*
 * Dump out this data chunk, and provide some statistics about storage
 */
{
	int n, len;

	printf ("RAW, class '%s', plat %d (%s), data len %d, ",
		ClassTable[dc->dc_Class]->dcm_Name, dc->dc_Platform,
		ds_PlatformName (dc->dc_Platform), dc->dc_DataLen);
	dc_IntStatsADE (dc, &n, &len);
	printf ("%d ADE totaling %d\n", n, len);
	printf ("internal class checking: %s\n",
		(_CheckClass) ? "enabled" : "disabled");
	printf ("Global attributes:\n");
	dc_ProcessAttrArrays (dc, NULL, dc_PrintAttr, NULL);
}





void
dc_SetGlobalAttrArray (dc, key, type, nval, values)
DataChunk *dc;
char *key;
DC_ElemType type;
int nval;
void *values;
/*
 * Store a global attribute into this data chunk.
 */
{
	dca_AddAttrArray (dc, DCC_Raw, ST_GLOBATTR, key, type, nval, values);
}




void *
dc_GetGlobalAttrArray (dc, key, type, nval)
DataChunk *dc;
char *key;
DC_ElemType *type;
int *nval;
/*
 * Store a global attribute into this data chunk.
 */
{
	return (dca_GetAttrArray (dc, DCC_Raw, ST_GLOBATTR, key, type, nval));
}




void
dc_SetGlobalAttr (dc, key, value)
DataChunk *dc;
char *key, *value;
/*
 * Store a global attribute into this data chunk.
 */
{
	dca_AddAttr (dc, DCC_Raw, ST_GLOBATTR, key, value);
}



char *
dc_GetGlobalAttr (dc, key)
DataChunk *dc;
char *key;
/*
 * Look up a global attribute.
 */
{
	return (dca_GetAttr (dc, DCC_Raw, ST_GLOBATTR, key));
}




int
dc_ProcessAttrArrays (dc, pattern, func, arg)
DataChunk *dc;
char *pattern;
int (*func) (/* char *key, void *vals, int nval, DC_ElemType, void *arg */);
void *arg;
/*
 * Go through and look at a bunch of attributes.  If PATTERN is non-null,
 * it is a regular expression used to filter out things.
 */
{
	return (dca_ProcAttrArrays (dc, DCC_Raw, ST_GLOBATTR, 
				    pattern, func, arg));
}




void
dc_RemoveGlobalAttr (dc, key)
DataChunk *dc;
char *key;
/*
 * Remove this global attribute key from this data chunk.
 */
{
	dca_RemoveAttr (dc, DCC_Raw, ST_GLOBATTR, key);
}




int
dc_ProcessAttrs (dc, pattern, func)
DataChunk *dc;
char *pattern;
int (*func) ();
/*
 * Go through and look at a bunch of attributes.  If PATTERN is non-null,
 * it is a regular expression used to filter out things.
 */
{
	return (dca_ProcAttrs (dc, DCC_Raw, ST_GLOBATTR, pattern, func));
}




void *
dc_GetGlAttrBlock (dc, len)
DataChunk *dc;
int *len;
/*
 * Get the global attributes out as an opaque chunk.
 */
{
	return (dca_GetBlock (dc, DCC_Raw, ST_GLOBATTR, len));
}



void
dc_SetGlAttrBlock (dc, block, len)
DataChunk *dc;
void *block;
int len;
/*
 * Store a global attribute block back.
 */
{
	dca_PutBlock (dc, DCC_Raw, ST_GLOBATTR, block, len);
}




int
dc_GetNGlobalAttrs(dc)
DataChunk *dc;
/*
 * Return the number of global attributes in a datachunk.  
 */
{
	return (dca_GetNAttrs (dc, DCC_Raw, ST_GLOBATTR));
}



char **
dc_GetGlobalAttrList(dc, pattern, values, natts)
DataChunk *dc;
char *pattern;
char **values[];
int *natts;
{
	return(	dca_GetAttrList(dc, DCC_Raw, ST_GLOBATTR,
				pattern, values, natts));
}



char **
dc_GetGlobalAttrKeys (dc, natts)
DataChunk *dc;
int *natts;
/*
 * Return an array of pointers to the global attribute keys.
 * The array is only valid until the next call to any one of
 * the Get*AttrList or Get*AttrKeys functions.
 */
{
	return( dca_GetAttrList(dc, DCC_Raw, ST_GLOBATTR,
				NULL, NULL, natts));
}

