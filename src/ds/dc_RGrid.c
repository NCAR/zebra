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

# include <stdio.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"

RCSID ("$Id: dc_RGrid.c,v 3.5 1996-11-19 09:49:44 granger Exp $")

/*
 * Our class-specific AuxData structure types.
 */
# define ST_GRID_GEOM	99

/*
 * One of these for every sample to keep track of our grid information.  Note
 * that the use of the old structures forces the same origin and spacing for
 * each field, but that shouldn't be a problem.
 */
typedef struct _GridGeometry
{
	RGrid		gg_Rg;		/* Dims and spacing	*/
} GridGeometry;

typedef struct _RGridDataChunk
{
	RawDataChunkPart	rawpart;
	TranspDataChunkPart	transpart;
	MetDataChunkPart	metpart;
	RGridDataChunkPart	rgridpart;

} RGridDataChunk;

#define GGP(dc) (((RGridDataChunk *)(dc))->rgridpart.rg_gg)
#define RGP(dc) (&((RGridDataChunk *)(dc))->rgridpart)

/*
 * Local routines.
 */
static DataChunk *rg_Create FP((DataChunk *));
static void rg_Dump FP ((DataChunk *));
static void rg_Localize FP ((DataChunk *dc));
static void rg_Serialize FP ((DataChunk *dc));
static void rg_Destroy FP ((DataChunk *dc));

# define SUPERCLASS ((DataClassP)&MetDataMethods)

RawClass RGridMethods =
{
	DCID_RGrid,
	"RGrid",
	SUPERCLASS,		/* Superclass			*/
	3,			/* Depth, Raw = 0		*/
	rg_Create,		/* Create			*/
	rg_Destroy,		/* Destroy			*/
	0,			/* Add				*/
	rg_Dump,		/* Dump				*/

	rg_Serialize,		/* Serialize			*/
	rg_Localize,		/* Localize			*/

	sizeof (RGridDataChunk)
};

DataClassP DCP_RGrid = (DataClassP)&RGridMethods;



/*----------------------------------------------------------------------*/
/* RGrid class methods */


static DataChunk *
rg_Create (dc)
DataChunk *dc;
/*
 * Initialize our instance part.
 */
{
	RGridDataChunkPart *rg = RGP(dc); 
	
	rg->rg_gg = NULL;
	rg->rg_Ngg = 0;
	return (dc);
}



static void
rg_Destroy (dc)
DataChunk *dc;
{
	RGridDataChunkPart *rg = RGP(dc); 

	if (rg->rg_gg)
		free (rg->rg_gg);
	rg->rg_gg = NULL;
	rg->rg_Ngg = 0;
}	



static void
rg_Serialize (dc)
DataChunk *dc;
{
	RGridDataChunkPart *rg = RGP(dc); 

	if (rg->rg_gg && rg->rg_Ngg)
		dc_AddADE (dc, rg->rg_gg, DCP_RGrid, ST_GRID_GEOM,
			   rg->rg_Ngg * sizeof (GridGeometry), FALSE);
}	



static void
rg_Localize (dc)
DataChunk *dc;
{
	RGridDataChunkPart *rg = RGP(dc); 

	rg->rg_gg = (GridGeometry *)
		dc_FindADE (dc, DCP_RGrid, ST_GRID_GEOM, 0);
}	



static void
rg_Dump (dc)
DataChunk *dc;
/*
 * Dump out some info.
 */
{
	RGridDataChunkPart *rg = RGP(dc); 
	GridGeometry *gg;
	int nsamples = dc_GetNSample (dc), samp;
/*
 * If there is no data, bail.
 */
	if (nsamples <= 0)
		return;
/*
 * Get the grid info.
 */
	if (! (gg = rg->rg_gg))
	{
		msg_ELog (EF_PROBLEM, "Grid geometry block vanished!");
		return;
	}
/*
 * Dump out each sample.
 */
	printf ("RGRID Class:\n");
	for (samp = 0; samp < nsamples; samp++)
	{
		Location loc;
		dc_GetLoc (dc, samp, &loc);
		printf ("\tGrid at %.4f %.4f %.3f, %dx%dx%d\n",
			loc.l_lat, loc.l_lon, loc.l_alt, gg[samp].gg_Rg.rg_nX,
			gg[samp].gg_Rg.rg_nY, gg[samp].gg_Rg.rg_nZ);
	}
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
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_RGrid, "RGSetup"))
		return;
/*
 * Do the field setup.
 */
	dc_SetupFields (dc, nfld, fields);
}




int
dc_RGAddGrid (dc, sample, field, origin, rg, t, data, len)
DataChunk *dc;
int sample;
FieldId field;
Location *origin;
RGrid *rg;
ZebTime *t;
void *data;
int len;
/*
 * Add this field to the DC.
 * Entry:
 *	DC	is the data chunk to be modified.
 *	SAMPLE	is the sample to be added.
 *	FIELD	is the field ID for this grid.
 *	ORIGIN	is the origin of the grid.
 *	RG	is the spacing information
 *	T	is the time of this data
 *	DATA	is the actual data
 *	LEN	is the length of the grid data, IN BYTES.  If it is specified
 *		as zero, the length will be calculated from the dimensions.
 * Exit:
 *	The grid has been added.  Return zero on failure, non-zero otherwise.
 */
{
	RGridDataChunkPart *rgdc = RGP(dc); 
	GridGeometry *gg;
	int nsamples = dc_GetNSample (dc);
	int newlen, newsamp;
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_RGrid, "RGAddGrid"))
		return (0);
/*
 * Make them add samples in order.
 */
	if (sample > nsamples)
	{
		msg_ELog (EF_PROBLEM, "Trying to add grid %d when exist %d",
			sample, nsamples);
		return (0);
	}
	if (len == 0)
		len = rg->rg_nX * rg->rg_nY * rg->rg_nZ * dc_SizeOf(dc, field);
/*
 * If there is no data in this grid, create our geometry information now.
 */
	if (nsamples <= 0)
	{
	/*
	 * Initialize a new Geometry structure.
	 */
		gg = ALLOC (GridGeometry);
		gg->gg_Rg = *rg;
		rgdc->rg_gg = gg;
		rgdc->rg_Ngg = 1;
	/*
	 * Set some sample size hints, assuming that all of the samples will
	 * have the same grid geometry (which is usually a safe assumption).
	 * If this is a bad assumption, the application should change the hint,
	 * or reset it to zero so that the per-sample average is used.
	 */
		dc_HintSampleSize (dc, len * dc_GetNField(dc), FALSE);
	}
/*
 * This is not the first data, so there should already be a GridGeometry
 * structure.
 */
	else if (! (gg = rgdc->rg_gg))
	{
		msg_ELog (EF_PROBLEM, "Grid geometry block vanished!");
		return (0);
	}
/*
 * If they are adding a new sample to the DC, expand our grid info now.  Use
 * the growth hint to try to reduce the number of realloc's we have to do.
 * Note that (sample == nsamples) only for the first field stored for a 
 * particular sample, so we aren't trying to grow every time a field is added
 * the last sample in the chunk.  We only check for growth when trying to
 * add to a sample which at present does not exist.
 */
	else if (sample == nsamples)
	{
	/*
	 * We need at least one more slot, but perhaps there's hints for more
	 */
		newsamp = dc_NSamplesGrowthHint (dc, 1);
		if (rgdc->rg_Ngg < newsamp)
		{
			newlen = (newsamp) * sizeof (GridGeometry);
			gg = (GridGeometry *) realloc (gg, newlen);
			rgdc->rg_gg = gg;
			rgdc->rg_Ngg = newsamp;
		}
	}
/*
 * Otherwise, everything is set up.  Now we just add the new data.
 */
	gg[sample].gg_Rg = *rg;
	dc_AddMData (dc, t, field, len, sample, 1, data);
	dc_SetLoc (dc, sample, origin);
	return (1);
}




int
dc_RGAddMissing (dc, sample, field, origin, rg, t, len)
DataChunk *dc;
int sample;
FieldId field;
Location *origin;
RGrid *rg;
ZebTime *t;
int len;
/*
 * Fill this grid with the field missing value.
 */
{
	if (! dc_RGAddGrid (dc, sample, field, origin, rg, t, NULL, len))
		return (0);
	if (len == 0)
		len = rg->rg_nX * rg->rg_nY * rg->rg_nZ * dc_SizeOf(dc, field);
	return (dc_FillMissing (dc, t, field, len, sample, 1));
}




void *
dc_RGGetGrid (dc, sample, field, origin, rg, len)
DataChunk *dc;
int sample;
FieldId field;
Location *origin;
RGrid *rg;
int *len;
/*
 * Retrieve a grid from this DC.
 */
{
	GridGeometry *gg;
	DataPtr data;
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_RGrid, "RGGetGrid"))
		return (NULL);
/*
 * Find the data itself.
 */
	if (! (data = dc_GetMData (dc, sample, field, len)))
		return (NULL);
/*
 * Now look up our dimension info.
 */
	if (! (origin || rg))  /* Maybe they don't want it. */
		return ((void *) data);
	if (! (gg = GGP(dc)))
	{
		msg_ELog (EF_PROBLEM, "GridGeometry does not exist");
		return (NULL);
	}
/*
 * Return the stuff they want.
 */
	if (origin)
		dc_GetLoc (dc, sample, origin);
	if (rg)
		*rg = gg[sample].gg_Rg;
	return ((void *) data);
}







int
dc_RGGeometry (dc, sample, origin, rg)
DataChunk *dc;
int sample;
Location *origin;
RGrid *rg;
/*
 * Pull out the geometry of a grid in this DC.
 */
{
	RGridDataChunkPart *rgdc = RGP(dc); 
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_RGrid, "RGGetGrid"))
		return (FALSE);
	if (sample < 0 || sample >= dc_GetNSample (dc))
	{
		msg_ELog (EF_PROBLEM, "illegal sample %d in RGGeometry",
			  sample);
		return (FALSE);
	}
/*
 * Return the stuff they want.
 */
	if (origin)
		dc_GetLoc (dc, sample, origin);
	if (rg)
		*rg = rgdc->rg_gg[sample].gg_Rg;
	return (TRUE);
}
