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

# include <unistd.h>
# include <stdio.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"

RCSID ("$Id: DataChunk.c,v 3.13 2002-10-06 07:55:52 granger Exp $")

/*
 * The class methods structure for the raw class.
 */
static DataChunk *dc_RawCreate FP((DataChunk *));
static void dc_RawDestroy FP((DataChunk *));
static void dc_RawDump FP((DataChunk *));

DataClassP DCP_None = NULL;

RawClass RawMethods =
{
	DCID_Raw,
	"Raw",
	NULL,			/* No superclass	*/
	0,			/* class depth		*/
	dc_RawCreate,
	dc_RawDestroy,
	dc_RawAdd,
	dc_RawDump,
	NULL,			/* No ADEs to serialize or localize */
	NULL,
	sizeof (RawDataChunk)	/* every class has an instance size */
};

DataClassP DCP_Raw = (DataClassP)&RawMethods;


zbool _CheckClass = TRUE;


void
dc_CheckClass (on)
zbool on;
{
	_CheckClass = on;
}



zbool
dc_IsSubClass (class, superclass)
DataClassP class, superclass;
/*
 * Return TRUE iff the given class is a subclass of "superclass".
 */
{
	while (class != DCC_None)
	{
		if (class == superclass)
			return (TRUE);
		class = class->dcm_Super;
	}
	return (FALSE);
}




zbool
_dc_ReqSubClass (dc, superclass, op)
DataChunk *dc;
DataClassP superclass;
char *op;
/*
 * Return TRUE iff the required subclass relationship exists.  If it does
 * not, a message is logged.
 */
{
	if (dc_IsSubClass (dc->dc_ClassP, superclass))
		return (TRUE);
	msg_ELog (EF_PROBLEM, "%s not subclass of %s for op '%s'", 
		(dc->dc_ClassP)->dcm_Name,
		superclass->dcm_Name, op);
	return (FALSE);
}




DataClassP
dc_Super (class)
DataClassP class;
/*
 * Return the superclass of this class.
 */
{
	return ((class) ? class->dcm_Super : DCP_None);
}




PlatformId
dc_SetPlatform (dc, pid)
DataChunk *dc;
PlatformId pid;
{
	dc->dc_Platform = pid;
	return (dc->dc_Platform);
}




static DataChunk *
dc_CreateChain (dc, class, super)
DataChunk *dc;
DataClassP class;
DataClassP super;
{
	if (super != DCP_Raw)
		dc = dc_CreateChain (dc, class, dc_Super (super));
	if (super->dcm_Create)
		return ((*super->dcm_Create) (dc));
	return (dc);
}




DataChunk *
#ifndef T_DATACHUNKS
dc_Create (class)
#else
_dc_Create (class)
#endif
DataClassP class;
/*
 * Create a data chunk of this class.
 */
{
	DataChunk *dc;

	if (class == DCP_None)
	{
		msg_ELog (EF_EMERGENCY, "Tried to create DC with class None!");
		return (0);
	}
	dc = (DataChunk *) malloc (class->dcm_Size);
	dc->dc_ClassP = class;
	dc->dc_Class = class->dcm_ClassId;
	return (dc_CreateChain (dc, class, class));
}




void
#ifndef T_DATACHUNKS
dc_Destroy (dchunk)
#else
_dc_Destroy (dchunk)
#endif
DataChunk *dchunk;
/*
 * Get rid of this data chunk.
 */
{
	DataClassP class = dchunk->dc_ClassP;
/*
 * Move up the class hierarchy calling destroy methods.
 */
	while (class)
	{
		if (class->dcm_Destroy != InheritMethod &&
		    class->dcm_Destroy)
		{
			(*(class->dcm_Destroy)) (dchunk);
		}
		class = dc_Super (class);
	}
/*
 * Finally, zap the data chunk itself.
 */
	free ((char *) dchunk);
}


void
dc_DestroyDC (dc)
DataChunk *dc;
{
	dc_Destroy (dc);
}



void
dc_Dump (dc)
DataChunk *dc;
/*
 * Dump out this data chunk.
 */
{
	DataClassP class = dc->dc_ClassP;
/*
 * Go through and dump this one at every level which is prepared for it.
 */
	while (class != DCC_None)
	{
		if (class->dcm_Dump)
			(*class->dcm_Dump) (dc);
		class = dc_Super (class);
	}
}



void
dc_DumpDC (dc)
DataChunk *dc;
{
	dc_Dump (dc);
}



DataChunk *
dc_Serialize (dc)
DataChunk *dc;
/*
 * Serialize this datachunk.  Call each class's serialize method to attach
 * dynamic memory to ADEs.  The datachunk must remain valid after
 * serialization, it will just have a few more ADEs added to its chain.
 */
{
	DataClassP class = dc->dc_ClassP;

	while (class)
	{
		if (class->dcm_Serialize)
			(*(class->dcm_Serialize))(dc);
		class = dc_Super (class);
	}
	return (dc);
}



int
dc_SizeOfDC (dc)
DataChunk *dc;
/*
 * Return the size of this datachunk, from the class structure.
 */
{
	return ((dc->dc_ClassP)->dcm_Size);
}



/************
 *
 * Definition of the RAW data chunk class methods.
 *
 ************/


static DataChunk *
dc_RawCreate (dc)
DataChunk *dc;
/*
 * Create a raw data chunk.
 */
{
/*
 * Initialize the raw part of our datachunk.
 */
	dc->dc_Platform = BadPlatform;	/* They have to set this themselves */
	dc->dc_Data = (DataPtr) 0;	/* No data yet */
	dc->dc_DataLen = 0;
	dc_ClearADE (dc);		/* Erase the auxdata array */
	return (dc);
}





static void
dc_RawDestroy (dc)
DataChunk *dc;
/*
 * Get rid of this data object.
 */
{
/*
 * Free up the data array and ADEs.
 */
	if (dc->dc_DataLen > 0)
		free ((char *) dc->dc_Data);
	dc_DestroyADE (dc);
}





void
dc_RawAdd (dc, len)
DataChunk *dc;
int len;
/*
 * Modify this data chunk to hold "len" more data.  The initial size
 * setting is just a simple optimization to help smooth out the initial
 * realloc bumps when a datachunk first blooms. 
 */
{
        static const int initialSize = 512;

	if (dc->dc_DataLen == 0)
	{
	    int firstSize = (initialSize > len) ? initialSize : len;
	    dc->dc_Data = (DataPtr) malloc (firstSize);
	    msg_ELog (EF_DEVELOP, "dc_RawAdd(%s): malloc (%i) => %x", 
		      ds_PlatformName(dc->dc_Platform), firstSize, 
		      dc->dc_Data);
	}
	else if (dc->dc_DataLen + len > initialSize)
	{
	    void *pdata = dc->dc_Data;
	    dc->dc_Data = (DataPtr) realloc (dc->dc_Data,
					     dc->dc_DataLen + len);
	    msg_ELog (EF_DEVELOP, "dc_RawAdd(%s): realloc (%x, %i) => %x",
		      ds_PlatformName(dc->dc_Platform),
		      pdata, dc->dc_DataLen + len, dc->dc_Data);
	}
	if (dc->dc_Data == NULL)
		msg_ELog (EF_EMERGENCY, 
		  "DC, class %s, plat '%s', malloc failed!",
		  (dc->dc_ClassP)->dcm_Name, 
		  ds_PlatformName(dc->dc_Platform));
	dc->dc_DataLen += len;
}




static void
dc_RawDump (dc)
DataChunk *dc;
/*
 * Dump out this data chunk, and provide some statistics about storage
 */
{
	int n, len;

	printf ("RAW, class '%s', plat %d (%s), data len %d, ",
		(dc->dc_ClassP)->dcm_Name, dc->dc_Platform,
		ds_PlatformName (dc->dc_Platform), dc->dc_DataLen);
	dc_StatsADE (dc, &n, &len);
	printf ("%d ADE totaling %d\n", n, len);
	printf ("internal class checking: %s\n",
		(_CheckClass) ? "enabled" : "disabled");
	dc_DumpAttrArrays (dc);
}



