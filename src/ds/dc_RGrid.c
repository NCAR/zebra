/*
 * The regular grid data class.
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
MAKE_RCSID ("$Id: dc_RGrid.c,v 1.1 1991-11-21 22:31:44 corbet Exp $")

# define SUPERCLASS DCC_MetData

/*
 * Our class-specific AuxData structure types.
 */
# define ST_DIMENSIONS	42




/*
 * Local routines.
 */
# ifdef __STDC__
	static DataChunk *dc_RGCreate (DataClass);
	/* static void dc_IRDump (DataChunk *); */
# else
# endif

RawDCClass RGridMethods =
{
	"RGrid",
	SUPERCLASS,		/* Superclass			*/
	dc_RGCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	0,		/* Dump				*/
};





static DataChunk *
dc_RGCreate (class)
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
dc_RGSetup (dc, nfld, fields)
DataChunk *dc;
int nfld;
FieldId *fields;
/*
 * Initialize this RGrid data chunk.
 * Entry:
 *	DC	is a new data chunk which is a subclass of DCC_IRGrid.
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
/*	dc_SetupUniformFields (dc, 0, nfld, fields, nplat*sizeof (float)); */
	dc_SetupFields (dc, nfld, fields);
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
