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
# include "DataChunkP.h"


RCSID ("$Id: dc_Location.c,v 1.5 1996-11-19 09:34:01 granger Exp $")


typedef struct _LocationDataChunk
{
	RawDataChunkPart	rawpart;
	TranspDataChunkPart	transpart;
	LocationDataChunkPart	locnpart;

} LocationDataChunk;

#define LP(dc) (&((LocationDataChunk *)(dc))->locnpart)


/*
 * Class method prototypes.
 */
static DataChunk *loc_Create FP ((DataChunk *));

/*
 * The basic methods structure.
 */
# define SUPERCLASS ((DataClassP)&TranspMethods)
# define CLASSDEPTH 2

RawClass LocationMethods =
{
	DCID_Location,
	"Location",
	SUPERCLASS,		/* Superclass			*/
	CLASSDEPTH,		/* Depth, Raw = 0		*/
	loc_Create,
	0,			/* No special destroy		*/
	0,			/* Add				*/
	0,			/* Dump				*/
	0,
	0,
	sizeof (LocationDataChunk)
};

DataClassP DCP_Location = ((DataClassP)&LocationMethods);



/*-----------------------------------------------------------------------*/
/* Location class methods */

static DataChunk *
loc_Create (dc)
DataChunk *dc;
/*
 * Initialize a location data chunk.
 */
{
/*
 * No AuxData at all for locations.
 */
	return (dc);
}

/*========================================================================*/



void
dc_LocAdd (dc, t, loc)
DataChunk *dc;
ZebTime *t;
Location *loc;
/*
 * Add a location to this data chunk.
 */
{
	if (! dc_ReqSubClass (dc, DCP_Location, "LocAdd"))
		return;
	dc_AddSample (dc, t, 0, 0);
	dc_SetLoc (dc, dc_GetNSample (dc) - 1, loc);
}




Location *
dc_LocAddMany (dc, nsamp, t, loc)
DataChunk *dc;
int nsamp;
ZebTime *t;
Location *loc;
/*
 * Add multiple locations to this data chunk.  If loc is NULL, allocates
 * space only.  On success, returns a pointer to the datachunk's array of
 * locations.
 */
{
	int begin;
	int i;

	if (! dc_ReqSubClass (dc, DCP_Location, "LocAddMany"))
		return (NULL);
	begin = dc_GetNSample (dc);
	dc_AddMoreSamples (dc, nsamp, 0);	/* this is a hint */
	for (i = 0; i < nsamp; ++i)
		dc_AddSample (dc, t+i, 0, 0);	/* this adds each sample */
	return (dc_SetMLoc (dc, begin, nsamp, loc));
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
	if (! dc_ReqSubClass (dc, DCP_Location, "Loc get"))
		return (0);
	if (sample < 0 || sample >= dc_GetNSample (dc))
		return (0);
/*
 * Now just pull out the info.
 */
	dc_GetLoc (dc, sample, loc);
	return (dc_GetTime (dc, sample, t));
}
