//
// Read-only polar access for sweep files.
//
// At this point, we only provide data in the polar format.  In the future,
// it's well worth considering adding:
//
// - A 1d grid version, or
// - An nspace version
//
// Both would require lots of memory, but could allow some interesting things 
// (like time-height contours).
//

# include <stdio.h>
# include <string.h>
# include <ctype.h>
# include <memory.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "DataFormat.h"


RCSID ("$Id: DFA_SweepFile.cc,v 3.4 2002-04-20 22:11:10 vanandel Exp $")

//
// From here down only if sweepfile access is enabled.
//
// There is a non-compiled version of the format record at the end.
//
# ifdef SWEEPFILE_INTERFACE
# include <dd_sweepfiles.hh>


//
// Scan types.
//
static char *ScanTypes[] =
{
	"cal", "ppi", "cop", "rhi", "ver", "tar", "man", "idl",
	"sur", "air", "hor"
};

//
// We gotta have a tag, of course.
//
class SFTag
{
private:
//
// Field info.
//
	int NField;
	FieldId *Fids;

	FieldId GetFID (int f)
	{
		char *fn = Mapper->field_name (f);
		char lcstring[30];
		for (int i = 0; i <= strlen (fn); i++)
			lcstring[i] = isupper (fn[i]) ? tolower(fn[i]) : fn[i];
		return (F_Lookup (lcstring));
	}


public:
	dd_sweepfile_access *SFile;	// The actual sweepfile
	dd_mapper *Mapper;		// Associated mapper
	Location Loc;			// Where the radar is
	ZebTime ZTime;			// When this particular sweep starts
//
// Initialize this thingie.  No constructor since it gets 'created' in 
// the C code somewhere.
//
	void Init ()
	{
		SFile = new dd_sweepfile_access();
		Mapper = new dd_mapper;
		NField = 0;
		Fids = 0;
	}
//
// Ditto destructor.
//
	void Destroy ()
	{
		SFile->close_sweepfile ();
		if (Fids)
			delete [] Fids;
		delete SFile;
		delete Mapper;
	}
//
// Set up our field array.
//
	void SetupFields ()
	{
		NField = Mapper->num_fields ();
		Fids = new FieldId [NField];
		for (int f = 0; f < NField; f++)
			Fids[f] = GetFID (f);
	}
//
// Other stuff to set up
//
	void SetupOther ()
	{
	//
	// Get the time info.
	//
		ZTime.zt_Sec = Mapper->unix_time ();
		ZTime.zt_MicroSec = 0;
	//
	// The radar location, kludging in, as always, the fixed angle
	// as the altitude.  Sigh.
	//
		Loc.l_lat = Mapper->latitude ();
		Loc.l_lon = Mapper->longitude ();
		Loc.l_alt = Mapper->fixed_angle ();
	}
//
// Field queries.
//
	int NumFields () { return (NField); };
	FieldId NthFid (int n) { return (Fids[n]); };

	char *FName (FieldId fid)
	{
		for (int i = 0; i < NField; i++)
			if (Fids[i] == fid)
				return (Mapper->field_name (i));
		return (0);
	}
};

class SFOpenFile
{
public:
	OpenFile 	open_file;
	SFTag		sf_tag;
};

//
// Get a tag from an openfile struct.
//
static inline SFTag &
dsf_GetTag (OpenFile *ofp)
{
	return (((SFOpenFile *) ofp)->sf_tag);
}



//
// Our access methods.
//
extern "C"
{
P_OpenFile (dsf_OpenFile);
P_CloseFile (dsf_CloseFile);
P_QueryTime (dsf_QueryTime);
P_GetFields (dsf_GetFields);
P_GetTimes (dsf_GetTimes);
P_GetObsSamples (dsf_GetObsSamples);
P_GetAttrs (dsf_GetAttrs);
P_Setup (dsf_Setup);
P_GetData (dsf_GetData);


static DataFormat sweepfileFormatRec =
{
	"SweepFile",
	FTSweepFile,
	"^swp.",

	___,	/* FIX ME */		// Compatibility table
	0,				// Length of that table
	sizeof (SFOpenFile),		// Open file size
	TRUE,				// This is a read-only format

	FORMAT_INIT,			// Weird open file stuff

	dsf_QueryTime,			// Query times
	___,				// Make file name

	dsf_Setup,			// Setup
	dsf_OpenFile,			// open
	dsf_CloseFile,			// Close
	___, 				// Synchronize
	dsf_GetData,			// GetData
	___,				// AltitudeInfo
	fmt_DataTimes,			// DataTimes
	___,				// Forecast tiems
	___,				// Create
	___,				// PutSample
	___,				// WriteBlock
	dsf_GetObsSamples,		// GetObsSamples
	dsf_GetFields,			// GetFields
	dsf_GetAttrs,			// GetAttrs
	dsf_GetTimes,			// GetTimes
	___,				// Associated files
};

DataFormat *sweepfileFormat = &sweepfileFormatRec;




static int
dsf_NameFilter (const char *name)
//
// Filter out files we don't want to deal with.
//
{
	int l = strlen (name);

	return (strcmp (name + l - 4, ".tmp"));
}




static int
dsf_QueryTime (const char *file, ZebTime *begin, ZebTime *end, int *nsample)
//
// Query this file to see what's in it.
//
{
	dd_sweepfile_access sfile;
	dd_mapper mapper;
//
// Open up the file.
//
	int ok = dsf_NameFilter (file); 
	if (! ok)
	{
		msg_ELog (EF_INFO, "dsf_NameFilter failure on %s", file);
		return (FALSE);
	}
	ok = sfile.access_sweepfile (file, &mapper);
	if (! ok) 
	{
		msg_ELog (EF_INFO, "sfile.access_sweepfile failure on %s", file);
		return (FALSE);
	}
 
// Let's just be sure the file isn't empty.  Barring that, it will have
// exactly one sample in it.
//
	if (mapper.rays_in_sweep () < 10)
	{
		msg_ELog (EF_INFO, "< 10 rays in %s", file);
		sfile.close_sweepfile ();
		return (FALSE);
	}
//
// Filter out files with funky modes.
//
	int mode = mapper.scan_mode ();
	if (mode != PPI && mode != RHI && mode != SUR)
	{
		msg_ELog (EF_INFO, "Filter funky mode %s", file);
		sfile.close_sweepfile ();
		return (FALSE);
	}
//
// Fake up one sample and return it.
//
	*nsample = 1;
	begin->zt_Sec = mapper.unix_time ();
	begin->zt_MicroSec = 0;
	*end = *begin;
	sfile.close_sweepfile ();	// Just to be sure
	return (TRUE);
}




static int
dsf_OpenFile (OpenFile *ofp, int write)
//
// Open up one of these guys.
//
{
	SFTag &tag = dsf_GetTag (ofp);
	char *fname = ofp->of_df.df_fullname;
//
// Don't even think about writing one of these guys.  This "should never
// happen".
//
	if (write)
	{
		msg_ELog (EF_PROBLEM, "Attempted write open on sweepfile %s",
				fname);
		return (FALSE);
	}
	if (! dsf_NameFilter (fname))
		return (FALSE);
//	msg_ELog (EF_INFO, "Open file %d %s", fmt_FileIndex (ofp),fname);
//
// Actually open it.
//
	tag.Init ();
	int ok = tag.SFile->access_sweepfile (fname, tag.Mapper);
	if (! ok)
	{
		tag.Destroy ();
		msg_ELog (EF_PROBLEM, "Unable to open %s", fname);
		return (FALSE);
	}
//
// The tag needs a field array, which it can now handle itself.
//
	tag.SetupFields ();
	tag.SetupOther ();
//
// Good enough.
//
	return (TRUE);
}




static void
dsf_CloseFile (OpenFile *ofp)
//
// Close this file, of course.
//
{
	SFTag &tag = dsf_GetTag (ofp);
//	msg_ELog (EF_INFO, "Close file %d", fmt_FileIndex (ofp));
	tag.Destroy ();
}



static int
dsf_GetFields (OpenFile *ofp, int sample, int *nfield, FieldId *flist)
//
// Return the list of available fields.
//
{
	SFTag &tag = dsf_GetTag (ofp);
	
	*nfield = tag.NumFields ();
	for (int f = 0; f < *nfield; f++)
		flist[f] = tag.NthFid (f);
	return (*nfield);
}



static ZebTime *
dsf_GetTimes (OpenFile *ofp, int *ntime)
//
// Return our time "array".
//
{
	SFTag &tag = dsf_GetTag (ofp);
	*ntime = 1;
	return (&tag.ZTime);
}



static int
dsf_GetObsSamples (OpenFile *ofp, ZebTime *times, Location *locs, int max)
//
// Return all of the 'samples' in this observation.  We make it look like
// there is only one.
//
{
	SFTag &tag = dsf_GetTag (ofp);
//
// Protect against something really weird.
//
	if (max < 1)
		return (0);
//
// Hand back the info for the one 'sample' we have.
//
	*times = tag.ZTime;
	*locs = tag.Loc;
	return (1);
}




static char *
dsf_GetAttrs (OpenFile *ofp, int sample, int *len)
//
// Perpetuate the radar attributes kludge.
//
{
	SFTag &tag = dsf_GetTag (ofp);
//
// Just checking...
//
	if (sample != 0)
		return (0);
//
// Hack something up.
//
	const int attralloc = 500;	// More than enough
	char *attrs = (char *) malloc (attralloc), *ap = attrs;
	int mode = tag.Mapper->scan_mode ();
	strcpy (ap, "radar_space");  ap += strlen (ap) + 1;
	strcpy (ap, "true");   ap += strlen (ap) + 1;
	strcpy (ap, "scan_mode");  ap += strlen (ap) + 1;
	strcpy (ap, (mode == PPI) ? "ppi" :((mode == RHI) ? "rhi" : "sur"));  
	ap += strlen (ap) + 1;
	strcpy (ap, "fixed_angle");  ap += strlen (ap) + 1;
	sprintf (ap, "%.1f", tag.Loc.l_alt);
	ap += strlen (ap) + 1;
//
// Return it.
//
	*len = ap - attrs;
	return (attrs);
}




static PolarBeam *
dsf_GetBeam (DataChunk *dc, const DataFile *df, int fsample, int beam, 
	     FieldId fid, FieldId tfid, int throver, float tvalue)
//
// Actually extract a beam and pass it back to the user.
//
{
	char *fname, *tfname;
//
// Since this guy is called asynchronously, from who knows where, we may
// have to open the file ourselves.
//
	OpenFile *ofp = dfa_OpenFile (df, 0);
	if (! ofp)
	{
		msg_ELog (EF_PROBLEM, "dsf_GetBeam open fail on %s", 
			  df->df_fullname);
		return (NULL);
	}
//
// Pull out our tag, and position to the desired beam..
//
	SFTag &tag = dsf_GetTag (ofp);
	if (tag.SFile->seek_ray (beam) < 0)
	{
		msg_ELog (EF_PROBLEM, "Unable to seek to beam %d", beam);
		return (NULL);
	}
//
// Make sure we have the field they are asking for.
//
	if (! (fname = tag.FName (fid)))
		return (NULL);
//
// Allocate our storage, and fill in the details.
//
	PolarBeam *ret = (PolarBeam *) malloc (sizeof (PolarBeam));
	ret->pb_Fid = fid;
	ret->pb_ScanMode = (ScanMode) tag.Mapper->scan_mode ();
	ret->pb_Azimuth = tag.Mapper->azimuth ();
	ret->pb_Elevation = tag.Mapper->elevation ();
	if (tag.Mapper->scan_mode () == RHI)
	{
		ret->pb_FixedAngle = tag.Mapper->azimuth ();
		ret->pb_RotAngle = tag.Mapper->elevation ();
	}
	else
	{
		ret->pb_RotAngle = tag.Mapper->azimuth ();
		ret->pb_FixedAngle = tag.Mapper->elevation ();
	}
	ret->pb_NGates = tag.Mapper->number_of_cells ();
	ret->pb_R0 = tag.Mapper->meters_to_first_cell ()/1000.0;
	ret->pb_GateSpacing = tag.Mapper->meters_between_cells ()/1000.0;
	ret->pb_Data = (float *) malloc (ret->pb_NGates*sizeof (float));
//
// If we are thresholding, set that up.  Then get the data.
//
	float badval = dc_GetBadval (dc);
	if (tfid != BadField && (tfname = tag.FName (tfid)) != 0)
	{
		tag.Mapper->threshold_setup (throver ? REMOVE_GT : REMOVE_LT,
				tvalue, tvalue, tfname);
		tag.Mapper->return_thresholded_field (fname, ret->pb_Data, 
				&badval);
	}
	else
		tag.Mapper->return_field (fname, ret->pb_Data, &badval);
//
// That's it.
//
	return (ret);
}





static DataChunk *
dsf_Setup (OpenFile *ofp, FieldId *fids, int nfid, DataClass dclass)
//
// Get ready to snarf some data.
//
{
	SFTag &tag = dsf_GetTag (ofp);
	DataChunk *dc;
//
// Create our data chunk. and return it.
//
	dc = dc_CreateDC (dclass);
	if (dclass == DCC_Polar)
		dcp_Setup (dc, nfid, fids, dsf_GetBeam);
	return (dc);
}




static int
dsf_GetData (OpenFile *ofp, DataChunk *dc, int begin, int nsample, 
		dsDetail *details, int ndetail)
//
// The GetData interface.  Here we don't actually do much, just stash
// the necessary info into our data chunk so that we can respond to getbeam
// calls later on.
//
{
	SFTag &tag = dsf_GetTag (ofp);
//
// Make sure they aren't asking for something really weird.
//
	if (begin > 0)
		return (0);
//
// Do they want a location?  If so, store it.
//
	if (dc->dc_Class == DCC_Location)
	{
		dc_LocAdd (dc, &tag.ZTime, &tag.Loc);
		dc_SetSampleAttr (dc, 0, "scan_type",
				ScanTypes[tag.Mapper->scan_mode()]);
	}
//
// Otherwise they want data; stash the info.
//
	else
		dcp_AddSweep (dc, &tag.ZTime, &tag.Loc, 
				tag.Mapper->rays_in_sweep (), tag.Loc.l_alt, 
				(ScanMode) tag.Mapper->scan_mode (),
				&(ofp->of_df), 0);
}




};  // extern "C"



//
// Bit of obnoxiousness required by the dorade libraries.
//
void dd_Testify (char *msg)
{
	printf (msg);
}




# else // SWEEPFILE_INTERFACE

static DataFormat sweepfileFormatRec =
{
	"SweepFile-uncompiled",
	FTSweepFile,
	"^swp.",

	___,	/* FIX ME */		// Compatibility table
	0,				// Length of that table
	0,				// Open file size
	TRUE,				// This is a read-only format

	FORMAT_INIT,			// Weird open file stuff

	fmt_QueryNotCompiled,		// Query times
	___,				// Make file name

	___,				// Setup
	fmt_OpenNotCompiled,		// open
	___,				// Close
	___, 				// Synchronize
	___,				// GetData
	___,				// AltitudeInfo
	___,				// DataTimes
	___,				// Forecast tiems
	___,				// Create
	___,				// PutSample
	___,				// WriteBlock
	___,				// GetObsSamples
	___,				// GetFields
	___,				// GetAttrs
	___,				// GetTimes
	___,				// Associated files
};

DataFormat *sweepfileFormat = &sweepfileFormatRec;

# endif // SWEEPFILE_ACCESS
