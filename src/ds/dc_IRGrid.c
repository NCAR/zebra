/*
 * The irregular grid data class.
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

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "ds_fields.h"
# include "DataChunk.h"
# include "DataChunkP.h"
MAKE_RCSID ("$Id: dc_IRGrid.c,v 1.3 1991-12-27 21:22:22 corbet Exp $")

# define SUPERCLASS DCC_MetData

/*
 * Our class-specific AuxData structure types.
 */
# define ST_PLATFORMS	42


/*
 * The format of the platform list.
 */
typedef struct _PlatInfo
{
	PlatformId	pi_Id;		/* The ID of this platform	*/
	Location	pi_Loc;		/* Where it is.			*/
} PlatInfo;




/*
 * Local routines.
 */
static DataChunk *dc_IRGCreate FP((DataClass));
static bool dc_GetPlatList FP((DataChunk *, PlatInfo **, int *));
static void dc_IRDump FP((DataChunk *));

RawDCClass IRGridMethods =
{
	"IRGrid",
	SUPERCLASS,		/* Superclass			*/
	dc_IRGCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	dc_IRDump,		/* Dump				*/
};





static DataChunk *
dc_IRGCreate (class)
DataClass class;
/*
 * Create a chunk of this class.
 */
{
	DataChunk *dc;
/*
 * The usual.  Make a superclass chunk and tweak it to look like us.  We don't
 * add any info here, because we don't know it yet.
 */
	dc = dc_CreateDC (SUPERCLASS);
	dc->dc_Class = class;
	return (dc);
}





void
dc_IRSetup (dc, nplat, pids, locs, nfld, fields)
DataChunk *dc;
int nplat, nfld;
PlatformId *pids;
Location *locs;
FieldId *fields;
/*
 * Initialize this IRGrid data chunk.
 * Entry:
 *	DC	is a new data chunk which is a subclass of DCC_IRGrid.
 *	NPLAT	is the number of platforms to be stored in this DC.
 *	PIDS	is a list of platform ID's for these platforms.
 *	LOCS	is a list of locations for the platforms.
 *	NFLD	is the number of fields to be stored in this DC.
 *	FIELDS	is the list of those fields.
 * Exit:
 *	The data chunk has been set up.
 */
{
	PlatInfo *pinfo;
	int plat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_IRGrid, "IRSetup"))
		return;
/*
 * Do the field setup.
 */
	dc_SetupUniformFields (dc, 0, nfld, fields, nplat*sizeof (float));
/*
 * Allocate the platform space and set that up too.
 */
	pinfo = (PlatInfo *) malloc (nplat * sizeof (PlatInfo));
	for (plat = 0; plat < nplat; plat++)
	{
		pinfo[plat].pi_Id  = pids[plat];
		pinfo[plat].pi_Loc = locs[plat];
	}
	dc_AddADE (dc, pinfo, DCC_IRGrid, ST_PLATFORMS,
				nplat*sizeof (PlatInfo), TRUE);
}






static bool
dc_GetPlatList (dc, plist, nplat)
DataChunk *dc;
PlatInfo **plist;
int *nplat;
/*
 * Get the platform info list out of this DC.
 */
{
	int len;

	if (! (*plist = (PlatInfo *) dc_FindADE (dc, DCC_IRGrid,
			ST_PLATFORMS, &len)))
		return (FALSE);
	*nplat = len/sizeof (PlatInfo);
	return (TRUE);
}






void
dc_IRAddGrid (dc, t, sample, field, data)
DataChunk *dc;
time *t;
int sample;
FieldId field;
float *data;
/*
 * Add data for one field, one sample to this data chunk.  The data is assumed
 * to be NPLAT samples, in the same order as the platform list.
 */
{
	PlatInfo *pinfo;
	int nplat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_IRGrid, "IRAddGrid"))
		return;
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for AddGrid");
		return;
	}
/*
 * Just do the add.
 */
	dc_AddMData (dc, t, field, nplat*sizeof (float), sample, 1, data);
}






void
dc_IRAddMultGrid (dc, t, begin, nsample, field, data)
DataChunk *dc;
time *t;
int begin, nsample;
FieldId field;
float *data;
/*
 * Add data for one field, multiple samples to this data chunk.
 * The data is assumed
 * to be NPLAT samples, in the same order as the platform list.
 */
{
	PlatInfo *pinfo;
	int nplat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_IRGrid, "IRAddMultGrid"))
		return;
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for AddGrid");
		return;
	}
/*
 * Just do the add.
 */
	dc_AddMData (dc, t, field, nplat*sizeof (float), begin, nsample, data);
}





int
dc_IRGetNPlatform (dc)
DataChunk *dc;
/*
 * Return the number of platforms in this DC.
 */
{
	PlatInfo *pinfo;
	int nplat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_IRGrid, "IRGetNPlatform"))
		return (0);
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
		return (0);
	return (nplat);
}





void
dc_IRGetPlatforms (dc, pids, locs)
DataChunk *dc;
PlatformId *pids;
Location *locs;
/*
 * Return the list of platform ID's and locations for this DC.  Either pointer
 * may be NULL, in which case that info is not returned.
 */
{
	PlatInfo *pinfo;
	int nplat, plat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_IRGrid, "IRGetPlatforms"))
		return;
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for GetPlatforms");
		return;
	}
/*
 * Return the info.
 */
	for (plat = 0; plat < nplat; plat++)
	{
		if (pids)
			*pids++ = pinfo[plat].pi_Id;
		if (locs)
			*locs++ = pinfo[plat].pi_Loc;
	}
}





float *
dc_IRGetGrid (dc, sample, field)
DataChunk *dc;
int sample;
FieldId field;
/*
 * Locate this grid for this field and sample.
 */
{
	PlatInfo *pinfo;
	int nplat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_IRGrid, "IRGetPlatforms"))
		return (NULL);
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for GetPlatforms");
		return (NULL);
	}
/*
 * Get the info.
 */
	return ((float *) dc_GetMData (dc, sample, field, NULL));
}




static void
dc_IRDump (dc)
DataChunk *dc;
/*
 * Dump this think out.
 */
{
	PlatInfo *pinfo;
	int nplat, plat;
/*
 * Get our platform list.
 */
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for GetPlatforms");
		return;
	}
/*
 * Dump it out.
 */
	printf ("IRGRID class, %d platforms\n", nplat);
	for (plat = 0; plat < nplat; plat++)
		printf ("\t%2d: (%s) at %.4f %.4f %.2f\n", pinfo[plat].pi_Id,
			/* ds_PlatformName (pinfo[plat].pi_Id) */"(name here)",
			pinfo[plat].pi_Loc.l_lat, pinfo[plat].pi_Loc.l_lon,
			pinfo[plat].pi_Loc.l_alt);
}
