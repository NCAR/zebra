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
# include <stdio.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"

RCSID ("$Id: dc_IRGrid.c,v 3.6 1998-10-28 21:21:04 corbet Exp $")

/*
 * Our class-specific AuxData structure types.
 */
#define ST_PLATFORMS	42

/*
 * The format of the platform list.
 */
typedef struct _PlatInfo
{
	PlatformId	pi_Id;		/* The ID of this platform	*/
	Location	pi_Loc;		/* Where it is.			*/
} PlatInfo;


typedef struct _IRGridDataChunk
{
	RawDataChunkPart	rawpart;
	TranspDataChunkPart	transpart;
	MetDataChunkPart	metpart;
	IRGridDataChunkPart	irgridpart;

} IRGridDataChunk;

typedef IRGridDataChunkPart IRG;	/* type shortcut */

/*
 * Local routines.
 */
static zbool dc_GetPlatList FP((DataChunk *, PlatInfo **, int *));

/*
 * Class method prototypes
 */
static DataChunk *irg_Create FP((DataChunk *dc));
static void irg_Destroy FP((DataChunk *));
static void irg_Dump FP((DataChunk *));
static void irg_Serialize FP((DataChunk *));
static void irg_Localize FP((DataChunk *));

#define SUPERCLASS ((DataClassP)&MetDataMethods)

RawClass IRGridMethods =
{
	DCID_IRGrid,
	"IRGrid",
	SUPERCLASS,		/* Superclass			*/
	3,			/* Depth, Raw = 0		*/
	irg_Create,
	irg_Destroy,		/* Destroy			*/
	0,			/* Add				*/
	irg_Dump,		/* Dump				*/

	irg_Serialize,
	irg_Localize,

	sizeof (IRGridDataChunk)
};

DataClassP DCP_IRGrid = ((DataClassP)&IRGridMethods);

#define IRP(dc) (&((IRGridDataChunk *)(dc))->irgridpart)

/*----------------------------------------------------------------------*/
/* IRGrid class methods */


static DataChunk *
irg_Create (dc)
DataChunk *dc;
/*
 * Create a chunk of this class.
 */
{
	IRG *irg = IRP(dc);

	irg->irg_pinfo = NULL;
	irg->irg_nplat = 0;
	return (dc);
}



static void
irg_Destroy (dc)
DataChunk *dc;
{
	IRG *irg = IRP(dc);

	if (irg->irg_pinfo)
		free (irg->irg_pinfo);
	irg->irg_pinfo = NULL;
	irg->irg_nplat = 0;
}



static void
irg_Serialize (dc)
DataChunk *dc;
{
	IRG *irg = IRP(dc);

	if (irg->irg_pinfo)
		dc_AddADE (dc, irg->irg_pinfo, DCP_IRGrid, ST_PLATFORMS,
			   irg->irg_nplat * sizeof (PlatInfo), FALSE);
}



static void
irg_Localize (dc)
DataChunk *dc;
{
	IRG *irg = IRP(dc);

	irg->irg_pinfo = (PlatInfo *) dc_FindADE (dc, DCP_IRGrid,
						  ST_PLATFORMS, 0);
}



static void
irg_Dump (dc)
DataChunk *dc;
/*
 * Dump this thing out.
 */
{
	PlatInfo *pinfo;
	int nplat, plat;
/*
 * Get our platform list.
 */
	dc_GetPlatList (dc, &pinfo, &nplat);
/*
 * Dump it out.
 */
	printf ("IRGRID class, %d platforms\n", nplat);
	for (plat = 0; plat < nplat; plat++)
		printf ("\t%2ld: (%s) at %.4f %.4f %.2f\n", 
			(long)pinfo[plat].pi_Id,
			ds_PlatformName (pinfo[plat].pi_Id),
			pinfo[plat].pi_Loc.l_lat, pinfo[plat].pi_Loc.l_lon,
			pinfo[plat].pi_Loc.l_alt);
}



/*-- End class methods -- */
/*========================================================================*/


static zbool
dc_GetPlatList (dc, plist, nplat)
DataChunk *dc;
PlatInfo **plist;
int *nplat;
/*
 * Get the platform info list out of this DC.
 */
{
	IRG *irg = IRP(dc);

	if (nplat)
		*nplat = irg->irg_nplat;
	if (plist)
		*plist = irg->irg_pinfo;
	return ((irg->irg_nplat > 0) ? TRUE : FALSE);
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
	IRG *irg = IRP(dc);
	PlatInfo *pinfo;
	int plat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_IRGrid, "IRSetup"))
		return;
	if (irg->irg_nplat > 0)
	{
		msg_ELog (EF_PROBLEM, "irgrid already setup");
		return;
	}
/*
 * Do the field setup.
 */
	dc_SetupFields (dc, nfld, fields);
	dc_SetUniformOrg (dc, nplat);
/*
 * Allocate the platform space and set that up too.
 */
	pinfo = (PlatInfo *) malloc (nplat * sizeof (PlatInfo));
	for (plat = 0; plat < nplat; plat++)
	{
		pinfo[plat].pi_Id  = pids[plat];
		pinfo[plat].pi_Loc = locs[plat];
	}
	irg->irg_pinfo = pinfo;
	irg->irg_nplat = nplat;
}





DataPtr
dc_IRAddGrid (dc, t, sample, field, data)
DataChunk *dc;
ZebTime *t;
int sample;
FieldId field;
void *data;
/*
 * Add data for one field, one sample to this data chunk.  The data is
 * assumed to be NPLAT samples, in the same order as the platform list.
 * Returns a pointer to the storage location of this grid in the datachunk.
 * If data is NULL, the space is allocated but no data is copied.
 */
{
	PlatInfo *pinfo;
	int nplat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_IRGrid, "IRAddGrid"))
		return (NULL);
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for IRAddGrid");
		return (NULL);
	}
/*
 * Just do the add.
 */
	return (dc_AddMData(dc, t, field, nplat*dc_SizeOf(dc, field), 
			    sample, 1, data));
}





DataPtr
dc_IRAddMultGrid (dc, t, begin, nsample, field, data)
DataChunk *dc;
ZebTime *t;
int begin, nsample;
FieldId field;
void *data;
/*
 * Add data for one field, multiple samples to this data chunk.  The data
 * is assumed to be NPLAT samples, in the same order as the platform list.
 */
{
	PlatInfo *pinfo;
	int nplat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_IRGrid, "IRAddMultGrid"))
		return (NULL);
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for AddGrid");
		return (NULL);
	}
/*
 * Just do the add.
 */
	return (dc_AddMData (dc, t, field, nplat * dc_SizeOf(dc, field), 
			     begin, nsample, data));
}




void
dc_IRAddMissing (dc, t, begin, nsample, field)
DataChunk *dc;
ZebTime *t;
int begin, nsample;
FieldId field;
/*
 * Fill missing data for one field, multiple samples to this data chunk.
 */
{
	PlatInfo *pinfo;
	int nplat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_IRGrid, "IRAddMissing"))
		return;
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for IRAddMissing");
		return;
	}
/*
 * Just do the fill.
 */
	dc_FillMissing (dc, t, field, nplat * dc_SizeOf(dc, field), 
			begin, nsample);
}



void
dc_IRAddScalarDC (irgrid_dc, scalar_dc, sample, nsample, nfield, fields)
DataChunk *irgrid_dc;
DataChunk *scalar_dc;
int sample;
int nsample;
int nfield;
FieldId *fields;
/*
 * adds a Scalar chunk of data to an irgrid chunk, taking nsample's of data
 * beginning at sample, using nfield fields whose ids are listed in
 * 'fields'.  NULL 'fields' or zero 'nfield' implies use all those that
 * exist in the IRGrid DC.  Zero 'nsample' implies take them all.
 *
 * When a new sample is being created---either inserted, appended, or
 * prepended---fill it in with bad values first.  Otherwise, just change
 * the data value of the Scalar chunk's platform in the sample.  For now,
 * take advantage of the fact that samples can be non-chronological.
 *
 * This is a memory- and processing- intensive function, so use it 
 * sparingly and don't have any expectations of efficiency or speed.
 * This could be made alot faster by putting class-private info in 
 * 'private' header files, including this info in a dc_Utilities.c file,
 * and writing this routine to take advantage of the internals of the
 * transparent, metdata, scalar, and irgrid classes.  But for now...
 */
{
	FieldId *fids;
	ZebTime sc_zt;
	int i, f, d, s;
	int ir_nsample;
	PlatformId *platforms;
	int nplat;
	float badval;
	float *blank_grid;
	float *ir_data;
	ZebTime *ir_times;
	float sc_data;
	int sc_plat_idx;	/* index of scalar plat into irgrid list */

	if (! dc_ReqSubClass (irgrid_dc, DCP_IRGrid, "IRAddScalarDC") ||
	    ! dc_ReqSubClass (scalar_dc, DCP_Scalar, "IRAddScalarDC"))
		return;
/*
 * Set up the list of samples and fields to use.
 */
	if (nsample == 0)	/* default to the whole thing */
	{
		sample = 0;
		nsample = dc_GetNSample (scalar_dc);
	}
/*
 * Let the IRGrid dc know we'll need space for at least 'nsample' samples
 */
	dc_HintNSamples (irgrid_dc, nsample, FALSE);
	fids = fields;
	if (fids == NULL || nfield == 0)
	{
		fids = dc_GetFields (scalar_dc, &nfield);
	}
/*
 * Get some information about the irgrid we're storing to
 */
	ir_nsample = dc_GetNSample (irgrid_dc);
	nplat = dc_IRGetNPlatform (irgrid_dc);
	platforms = (PlatformId *)malloc(nplat * sizeof(PlatformId));
	dc_IRGetPlatforms (irgrid_dc, platforms, NULL);
	badval = dc_GetBadval (irgrid_dc);
/*
 * Find the scalar platform in the list of irgrid platforms
 */
	for (i = 0; i < nplat; ++i)
	{
		if (platforms[i] == scalar_dc->dc_Platform)
			break;
	}
	if (i < nplat)
		sc_plat_idx = i;
	else
	{
		/* scalar platform not in IRGrid, abandon efforts */
		msg_ELog (EF_PROBLEM, 
			  "%s: scalar platform '%s' not in irgrid for '%s'",
			  "IRAddScalarDC", 
			  ds_PlatformName (scalar_dc->dc_Platform),
			  ds_PlatformName (irgrid_dc->dc_Platform));
		free (platforms);
		return;
	}
/*
 * Set up a 'blank' irgrid sample full of bad values.
 */
	blank_grid = (float *)malloc(nplat * sizeof(float));
	for (d = 0; d < nplat; ++d)
		blank_grid[d] = badval;
/*
 * Construct a list of times from the irgrid chunk so that we don't
 * have to query the chunk in each loop.
 */
	ir_times = (ZebTime *)malloc (sizeof(ZebTime)*(nsample + ir_nsample));
	for (i = 0; i < ir_nsample; ++i)
		dc_GetTime (irgrid_dc, i, ir_times+i);
/*
 * For each field in the field list, and then for each sample, extract
 * the data.  Try to retrieve a sample from the irgrid for the same time
 * and the same field, and then merge the data.
 */
	for (f = 0; f < nfield; ++f)
	{
		if (dc_Type (scalar_dc, fids[f]) != DCT_Float ||
		    dc_Type (irgrid_dc, fids[f]) != DCT_Float)
		{
			msg_ELog (EF_PROBLEM, "cannot add non-float fid %d %s",
				  fids[f], "to non-float irgrid field");
			continue;
		}
		for (i = sample; i < sample + nsample; ++i)
		{
			dc_GetTime (scalar_dc, i, &sc_zt);
			sc_data = dc_GetScalar (scalar_dc, i, fids[f]);
			for (s = 0; s < ir_nsample; ++s)
			{
				if (TC_Eq(ir_times[s], sc_zt))
					break;
			}
			if (s < ir_nsample)	/* found a sample */
			{
				ir_data = dc_IRGetGrid (irgrid_dc, s, fids[f]);
			}
			else			/* no sample, use bad values */
			{
				ir_data = blank_grid;
			}
		/*
		 * Now we have a grid to modify according to what platform
		 * we're trying to add
		 */
			ir_data[sc_plat_idx] = sc_data;
		/*
		 * If this was a new grid, we need to add it back to the ir_dc
		 */
			if (ir_data == blank_grid)
			{
				ir_times[ir_nsample] = sc_zt;
				dc_IRAddGrid (irgrid_dc, &sc_zt,
					      ir_nsample++, fids[f],
					      ir_data);
				blank_grid[sc_plat_idx] = badval;
			}
		/*
		 * Otherwise we're done and we can move on to the next
		 * sample from the scalar chunk.
		 */
		}
	}

	free (platforms);					
	free (blank_grid);
	free (ir_times);
}



int
dc_IRGetNPlatform (dc)
DataChunk *dc;
/*
 * Return the number of platforms in this DC.
 */
{
	return (dc_IRGetPlatforms (dc, NULL, NULL));
}




int
dc_IRGetPlatforms (dc, pids, locs)
DataChunk *dc;
PlatformId *pids;
Location *locs;
/*
 * Return the list of platform ID's and locations for this DC.  Either pointer
 * may be NULL, in which case that info is not returned.  Returns number
 * of platforms.
 */
{
	PlatInfo *pinfo;
	int nplat, plat;
/*
 * Checking time.
 */
	if (! dc_ReqSubClass (dc, DCP_IRGrid, "IRGetPlatforms"))
		return (0);
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for GetPlatforms");
		return (0);
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
	return (nplat);
}





void *
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
	if (! dc_ReqSubClass (dc, DCP_IRGrid, "IRGetPlatforms"))
		return (NULL);
	if (! dc_GetPlatList (dc, &pinfo, &nplat))
	{
		msg_ELog (EF_PROBLEM, "No platform list for GetPlatforms");
		return (NULL);
	}
/*
 * Get the info.
 */
	return ((void *) dc_GetMData (dc, sample, field, NULL));
}



