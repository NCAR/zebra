/*
 * A data chunk class for polar (actually spherical) coordinate data.  Here
 * we track beam-oriented data and it's parameters.  This class is unusual
 * in that it allows the actual data to be stored externally and fetched
 * one beam at a time.  Thus we avoid thrashing large amounts of data
 * into memory while preserving a datachunkish interface.
 */

# include <stdio.h>
# include <memory.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"

RCSID ("$Id: dc_Polar.c,v 3.3 1999-03-01 02:03:43 burghart Exp $");



# define SUPERCLASS ((DataClassP) &MetDataMethods)

/*
 * The per-sample information.
 */
typedef struct _PolarSample
{
	int	ps_NBeams;		/* How many beams it has 	*/
	float	ps_FixedAngle;		/* Fixed angle			*/
	ScanMode ps_Mode;		/* The scan mode		*/
	const DataFile *ps_df;		/* The data file		*/
	int	ps_FSample;		/* Sample within the file	*/
} PolarSample;

/*
 * The actual data chunk structure.
 */
typedef struct _PolarPart
{
	int po_NSweeps;
	int po_NSweepAlloc;
	PolarSample *po_Sweeps;
	dcpGetFunc  po_GetBeam;
} PolarPart;

typedef struct _PolarDataChunk
{
	RawDataChunkPart	rawpart;
	TranspDataChunkPart	transpart;
	MetDataChunkPart	metpart;
	PolarPart		polarpart;
} PolarDataChunk;
	
#define GetPolarPart(dc) (&((PolarDataChunk *) dc)->polarpart)

/*
 * Our class methods.
 */
static DataChunk *dcp_Create (DataChunk *);
static void dcp_Destroy (DataChunk *);
static void dcp_Dump (DataChunk *);

/*
 * The method structure.
 */
RawClass PolarMethods =
{
	DCID_Polar,			/* Class ID */
	"Polar",
	SUPERCLASS,			/* Gee might this be the superclass? */
	3,				/* Hardcoded depth, below metdata */
	dcp_Create,			/* Make one of these		*/
	dcp_Destroy,			/* Zorch it			*/
	0,				/* Add				*/
	dcp_Dump,			/* Dump				*/
	0,				/* Serialize			*/
	0,				/* Localize			*/
	sizeof (PolarDataChunk)
};

/*
 * The methods pointer used elsewhere.  I think.  Maybe.  Actually, I'm
 * not sure what the hell happens with this.
 */
DataClassP DCP_Polar = (DataClassP) &PolarMethods;





static DataChunk *
dcp_Create (DataChunk *dc)
/*
 * Create one of these dudes.
 */
{
	PolarPart *polar = GetPolarPart (dc);

	polar->po_NSweeps = 0;
	polar->po_NSweepAlloc = 2; /* One more than we'll need */
	polar->po_Sweeps = (PolarSample *) malloc (
		polar->po_NSweepAlloc*sizeof (PolarSample));
	polar->po_GetBeam = 0;

	return (dc);
}




static void
dcp_Destroy (DataChunk *dc)
/*
 * Get rid of this one.
 */
{
	PolarPart *polar = GetPolarPart (dc);

	if (polar->po_Sweeps)
		free (polar->po_Sweeps);
}




void
dcp_Setup (DataChunk *dc, int nfield, FieldId *fids, dcpGetFunc getfcn)
/*
 * Get one of these guys set up.
 */
{
	PolarPart *polar = GetPolarPart (dc);
/*
 * For now, we insist on a get function.
 */
	if (! getfcn)
	{
		msg_ELog (EF_PROBLEM, "dc_Polar needs get function");
		msg_ELog (EF_PROBLEM,"Armageddon is a likely near-term event");
		return;
	}
/*
 * Initialize our superclass layer.
 */
	dc_SetupFields (dc, nfield, fids);
	dc_SetUniformOrg (dc, 1);
/*
 * Remember our own stuff.
 */
	polar->po_GetBeam = getfcn;
}




void
dcp_AddSweep (DataChunk *dc, ZebTime *when, Location *where, int nbeam,
		float fixang, ScanMode mode, const DataFile *df, int fsample)
/*
 * Add a sweep to this data chunk.
 */
{
	PolarPart *polar = GetPolarPart (dc);
	PolarSample *ps;
	FieldId *fids;
	float *fakedata = 0;

	if (! dc_ReqSubClass (dc, DCP_Polar, "AddSweep"))
		return;
/*
 * Make sure we have the space for it.
 */
	if (++(polar->po_NSweeps) >= polar->po_NSweepAlloc)
	{
		polar->po_NSweepAlloc *= 2;

		polar->po_Sweeps = (PolarSample *) realloc (polar->po_Sweeps,
				polar->po_NSweepAlloc*sizeof (PolarSample));
	}
/*
 * Fill in the info.
 */
	ps = polar->po_Sweeps + polar->po_NSweeps - 1;
	ps->ps_NBeams = nbeam;
	ps->ps_FixedAngle = fixang;
	ps->ps_Mode = mode;
	ps->ps_df = df;
	ps->ps_FSample = fsample;
/*
 * Make sure MetData knows about this too.
 */
	fids = dc_GetFields (dc, 0);
	dc_AddMData(dc, when, fids[0], sizeof (float), polar->po_NSweeps - 1,
			1, &fakedata);
	dc_SetLoc (dc, polar->po_NSweeps - 1, where);
}




int
dcp_GetSweepInfo (DataChunk *dc, int sweep, SweepInfo *info)
/*
 * Fill in the info structure for this sweep.
 */
{
	PolarPart *polar = GetPolarPart (dc);
	PolarSample *ps;
/*
 * Sanity checking.
 */
	if (! dc_ReqSubClass (dc, DCP_Polar, "GetSweepInfo"))
		return (FALSE);
	if (sweep < 0 || sweep >= polar->po_NSweeps)
		return (FALSE);
/*
 * Just copy out the stuff.
 */
	ps = polar->po_Sweeps + sweep;
	info->si_NBeam = ps->ps_NBeams;
	info->si_FixedAngle = ps->ps_FixedAngle;
	info->si_Mode = ps->ps_Mode;
	return (TRUE);
}
	




PolarBeam *
dcp_GetBeam (DataChunk *dc, int sweep, int beam, FieldId fid, FieldId tfid,
		int throver, float tvalue)
/*
 * Extract a beam from this data chunk.  The returned beam structure is
 * dynamic memory, and should be returned with dcp_FreeBeam when you're
 * done with it.
 *
 * Thresholding: if tfid is not BadField thresholding will be performed
 * using that field.  The threshold field should be present in the data
 * chunk for this to be guaranteed to happen (though sweepfiles don't
 * require that).  If throver is nonzero, data will be zapped when the
 * threshold field is over tvalue; otherwise when it's below that value.
 */
{
	PolarPart *polar = GetPolarPart (dc);
	PolarSample *ps;
/*
 * Sanity checking.
 */
	if (! dc_ReqSubClass (dc, DCP_Polar, "GetSweepInfo"))
		return (0);
	if (sweep < 0 || sweep >= polar->po_NSweeps)
		return (0);
	ps = polar->po_Sweeps + sweep;
	if (beam < 0 || beam > ps->ps_NBeams)
		return (0);
/*
 * Just pass the info on to the routine that knows what it is doing.
 */
	return ((*polar->po_GetBeam) (dc, ps->ps_df, ps->ps_FSample, beam,
			fid, tfid, throver, tvalue));
}




void
dcp_FreeBeam (PolarBeam *beam)
/*
 * Free this beam, which should ought to have come from dcp_GetBeam.
 */
{
	if (beam->pb_Data)
		free (beam->pb_Data);
	free (beam);
}




static void
dcp_Dump (DataChunk *dc)
/*
 * Dump this one out.
 */
{
	PolarPart *polar = GetPolarPart (dc);
	PolarSample *ps;
	int sweep;
/*
 * Check that this makes sense.
 */
	if (! dc_ReqSubClass (dc, DCP_Polar, "Dump"))
		return;
/*
 * Start printing.
 */
	printf ("DCC_Polar, %d sweeps (%d alloc)\n", polar->po_NSweeps,
			polar->po_NSweepAlloc);
	ps = polar->po_Sweeps;
	for (sweep = 0; sweep < polar->po_NSweeps; sweep++)
	{
	    printf (" Sweep %d, %d beams, fixed %.1f, mode %d, ",
				sweep, ps->ps_NBeams, ps->ps_FixedAngle,
				ps->ps_Mode);
		printf ("fs %d, %s\n", ps->ps_FSample, 
			ps->ps_df->df_core.dfc_name);
		
		ps++;
	}
}
			
