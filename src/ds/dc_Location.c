/*
 * The location data chunk class.
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
# include "DataChunk.h"
# include "DataChunkP.h"
MAKE_RCSID ("$Id: dc_Location.c,v 1.3 1993-08-04 17:15:55 granger Exp $")




/*
 * Forwards.
 */
static DataChunk *dc_LocCreate FP ((DataClass));



/*
 * The basic methods structure.
 */
# define SUPERCLASS DCC_Transparent

RawDCClass LocationMethods =
{
	"Location",
	SUPERCLASS,		/* Superclass			*/
	dc_LocCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	0,
};






static DataChunk *
dc_LocCreate (class)
DataClass class;
/*
 * Create a boundary data chunk.
 */
{
	DataChunk *dc;
/*
 * Start by creating a superclass chunk.
 */
	dc = dc_CreateDC (SUPERCLASS);
/*
 * No AuxData at all for boundaries, so we just set the class and return.
 */
	dc->dc_Class = class;
	return (dc);
}





void
dc_LocAdd (dc, t, loc)
DataChunk *dc;
ZebTime *t;
Location *loc;
/*
 * Add a location to this data chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Location, "Loc add"))
		return;
	dc_AddSample (dc, t, 0, 0);
	dc_SetLoc (dc, dc_GetNSample (dc) - 1, loc);
}




int
dc_LocGet (dc, sample, t, loc)
DataChunk *dc;
int sample;
ZebTime *t;
Location *loc;
/*
 * Look up a location in this data chunk.
 */
{
/*
 * Sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Location, "Loc get"))
		return (0);
	if (sample < 0 || sample >= dc_GetNSample (dc))
		return (0);
/*
 * Now just pull out the info.
 */
	dc_GetLoc (dc, sample, loc);
	return (dc_GetTime (dc, sample, t));
}
