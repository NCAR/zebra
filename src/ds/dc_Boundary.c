/*
 * The boundary data chunk class.
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
MAKE_RCSID ("$Id: dc_Boundary.c,v 1.2 1991-12-04 23:44:38 corbet Exp $")


/*
 * TODO:	Make boundary subplatforms work right.
 */

/*
 * AuxData structure for boundary samples.
 */
typedef struct _BndSamp
{
	int	bs_NPoint;		/* Number of points in this boundary */
} BndSamp;



/*
 * Local routines.
 */
# ifdef __STDC__
	static DataChunk *dc_BndCreate (DataClass);
	static void dc_BndDump (DataChunk *);
# endif

/*
 * The basic methods structure.
 */
# define SUPERCLASS DCC_Transparent

RawDCClass BoundaryMethods =
{
	"Boundary",
	SUPERCLASS,		/* Superclass			*/
	dc_BndCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	dc_BndDump
};






static DataChunk *
dc_BndCreate (class)
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
dc_BndAdd (dc, t, plat, pts, npt)
DataChunk *dc;
time *t;
PlatformId plat;
Location *pts;
int npt;
/*
 * Add a boundary to this data object.
 * Entry:
 *	DC	is the data chunk of interest.
 *	T	is the time of this boundary.
 *	PLAT	is the platform of this boundary.  It must either be
 *		the platform associated with the data object, or a
 *		subplatform thereof.
 *	PTS	is the list of points comprising this boundary.
 *	NPT	is the length of that list.
 * Exit:
 *	The boundary has been added to the data object.
 */
{
/*
 * Subplatform checking.
 */
	if (plat != dc->dc_Platform)
	{
		msg_ELog (EF_PROBLEM, "BndAdd plat mismatch");
		return;
	}
/*
 * Just stuff in the sample and be done with it.
 */
	dc_AddSample (dc, t, (DataPtr) pts, npt*sizeof (Location));
}






Location *
dc_BndGet (dc, sample, npt)
DataChunk *dc;
int sample, *npt;
/*
 * Find a boundary inside this data chunk.
 */
{
	int len;
	Location *ret;

	if (! (ret = (Location *) dc_GetSample (dc, sample, &len)))
		return (NULL);
	*npt = len/sizeof (Location);
	return (ret);
}





static void
dc_BndDump (dc)
DataChunk *dc;
/*
 * Dump out this data chunk.
 */
{
	int i, nbnd, pt;

	printf ("BOUNDARY class, %d boundaries\n", nbnd = dc_GetNSample(dc));
	for (i = 0; i < nbnd; i++)
	{
		Location *locs;
		int len;
		printf ("\t%2d: ", i);
		locs = dc_BndGet (dc, i, &len);
		for (pt = 0; pt < len; pt++)
			printf ("[%.2f, %.2f] ",locs[pt].l_lat,locs[pt].l_lon);
		printf ("\n");
	}
}
