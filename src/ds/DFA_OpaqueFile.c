//
// Read-only support for opaque files.  Return the file timestamp as the
// time of the one and only sample of the file.  Only return Transparent
// DataChunk fetches of the contents of the entire file.
//

# include <stdio.h>
# include <string.h>
# include <errno.h>
# include <ctype.h>
# include <memory.h>
# include <sys/stat.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "DataFormat.h"


RCSID ("$Id: DFA_OpaqueFile.c,v 3.2 2002-10-24 17:02:52 granger Exp $")


typedef struct OpaqueOpenFile
{
	OpenFile 	open_file;
	FILE*           opaque_handle;
	ZebraTime       opaque_time;
	off_t		opaque_length;
} OpaqueOpenFile;

//
// Our access methods.
//
P_OpenFile (dof_OpenFile);
P_CloseFile (dof_CloseFile);
P_QueryTime (dof_QueryTime);
P_GetFields (dof_GetFields);
P_GetTimes (dof_GetTimes);
P_GetObsSamples (dof_GetObsSamples);
// P_GetAttrs (dof_GetAttrs);
P_Setup (dof_Setup);
P_GetData (dof_GetData);


static CO_Compat COCTable[] =
{
	{ OrgTransparent,	DCC_Transparent	},
};


static DataFormat opaqueFileFormatRec =
{
	"OpaqueFile",
	FTOpaque,
	NULL,

	COCTable,			// Compatibility table
	N_COC (COCTable),		// Length of that table
	sizeof (OpaqueOpenFile),	// Open file size
	TRUE,				// This is a read-only format

	FORMAT_INIT,			// Weird open file stuff

	dof_QueryTime,			// Query times
	___,				// Make file name

	dof_Setup,			// Setup
	dof_OpenFile,			// open
	dof_CloseFile,			// Close
	___, 				// Synchronize
	dof_GetData,			// GetData
	___,				// AltitudeInfo
	fmt_DataTimes,			// DataTimes
	___,				// Forecast times
	___,				// Create
	___,				// PutSample
	___,				// WriteBlock
	dof_GetObsSamples,		// GetObsSamples
	dof_GetFields,			// GetFields
	___,				// GetAttrs
	dof_GetTimes,			// GetTimes
	___,				// Associated files
};

DataFormat *opaqueFileFormat = &opaqueFileFormatRec;


static int
dof_StatFile (const char *file, ZebraTime *begin, off_t *size)
{
    struct stat sbuf;

    if (stat (file, &sbuf) < 0)
    {
	msg_ELog (EF_PROBLEM, "Error %d on stat of %s", errno, file);
	return (0);
    }
    if (begin)
    {
	begin->zt_Sec = sbuf.st_mtime;
	begin->zt_MicroSec = 0;
    }
    if (size)
    {
	*size = sbuf.st_size;
    }
}


static int
dof_QueryTime (const char *file, ZebraTime *begin, ZebraTime *end, 
	       int *nsample)
//
// Query this file to see what's in it.
//
{
//
// Fake up one sample and return it.
//
    if (dof_StatFile (file, begin, 0))
    {
	*nsample = 1;
	*end = *begin;
	return (TRUE);
    }
    return FALSE;
}




static int
dof_OpenFile (OpenFile *ofp, int write)
//
// Open up one of these guys.
//
{
    ZebraTime end;
    int nsample;
    OpaqueOpenFile *oof = (OpaqueOpenFile *)ofp;
    char *fname = ofp->of_df.df_fullname;

    oof->opaque_handle = 0;

    if (dof_StatFile (fname, &oof->opaque_time, &oof->opaque_length))
    {
	oof->opaque_handle = fopen (fname, "r");
    }
    if (! oof->opaque_handle)
    {
	msg_ELog (EF_PROBLEM, "Unable to open %s", fname);
	oof->opaque_handle = 0;
	return (FALSE);
    }
    return (TRUE);
}




static void
dof_CloseFile (OpenFile *ofp)
{
    OpaqueOpenFile *oof = (OpaqueOpenFile *)ofp;
    if (oof->opaque_handle)
	fclose (oof->opaque_handle);
    oof->opaque_handle = 0;
}



static int
dof_GetFields (OpenFile *ofp, int sample, int *nfield, FieldId *flist)
//
// Return one field by the name of 'opaquefilecontents'.
//
{
    *nfield = 1;
    flist[0] = F_DeclareField ("opaquefilecontents", 
			       "Contents of an opaque file",
			       "");
    return (*nfield);
}



static ZebraTime *
dof_GetTimes (OpenFile *ofp, int *ntime)
//
// Return our time "array".
//
{
    OpaqueOpenFile *oof = (OpaqueOpenFile *)ofp;
    *ntime = 1;
    return (&oof->opaque_time);
}



static int
dof_GetObsSamples (OpenFile *ofp, ZebraTime *times, Location *locs, int max)
//
// Return all of the 'samples' in this observation.  We make it look like
// there is only one.
//
{
    static Location Boulder = { 40.0, -105, 1500 };
    OpaqueOpenFile *oof = (OpaqueOpenFile *)ofp;
    //
    // Protect against something really weird.
    //
    if (max < 1)
	return (0);
    //
    // Hand back the info for the one 'sample' we have.
    //
    *times = oof->opaque_time;
    *locs = Boulder;   // Any other suggestions?
    return (1);
}



static DataChunk *
dof_Setup (OpenFile *ofp, FieldId *fids, int nfid, DataClass dclass)

{
    DataChunk *dc;
//
// Create our data chunk and return it.
//
    dc = dc_CreateDC (dclass);
    return (dc);
}




static int
dof_GetData (OpenFile *ofp, DataChunk *dc, int begin, int nsample, 
	     dsDetail *details, int ndetail)
{
    OpaqueOpenFile *oof = (OpaqueOpenFile *)ofp;
    FILE *fin = oof->opaque_handle;
    size_t len = oof->opaque_length;
    char *fname = ofp->of_df.df_fullname;
    ZebraTime when = oof->opaque_time;
    void *data = 0;
    size_t wrote;

    if (begin > 0)
	return (0);

    // Read the file and stash it as a sample.
    data = dc_AddSample (dc, &oof->opaque_time, 0, len);
    if ((wrote = fread (data, len, 1, fin)) != len)
    {
	msg_ELog (EF_PROBLEM, "short read on opaque file %s, only %d bytes",
		  fname, wrote);
	memset (data+wrote, 0, len-wrote);
	return FALSE;
    }
    return TRUE;
}


