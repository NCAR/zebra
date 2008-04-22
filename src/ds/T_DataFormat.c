
#include <stdio.h>

/* 
 * Include the C file so we get all the private functions
 */
#define DEBUG
#include "DataFormat.c"
#include "dsPrivate.h"
#include "GetList.h"
#include "Appl.h"
#include "dslib.h"
#include "dfa.h"
#include "apple.h"


static char *DC_ClassName[] =
{
	"DCC_None",	/* = 0 */
	"DCC_Raw",
	"DCC_Transparent",
	"DCC_Boundary",
	"DCC_MetData",
	"DCC_Scalar",
	"DCC_IRGrid", 
	"DCC_RGrid",
	"DCC_Image",
	"DCC_Location",
	"DCC_NSpace",
	/* DCC_Text 	= 11, */
	"DCC_OutOfBounds"
};

static char *DS_OrgName[] =
{
	"OrgUnknown",
	"Org2dGrid",
	"OrgIRGrid",
	"OrgScalar",
	"OrgImage",
	"OrgOutline",
	"Org3dGrid",
	"OrgCmpImage",
        "Org1dGrid",
	"OrgTransparent",
	"OrgFixedScalar",
	"OrgNSpace"
};


typedef void(*Method)();


static int
VerifyMethod (char *name, Method member, Method inherit)
/*
 * name		name of the method being verified
 * member	value of member function pointer
 * inherit	function which could be inherited
 *
 * Verify that the member function is non-NULL, and note if it points
 * to the inherited method.
 */
{
	if (member == NULL)
	{
		msg_ELog (EF_PROBLEM, "  * Method '%s': does not exist.",
			  name);
		return (1);
	}
	if (! Verbose)
		return (0);
	if (member == inherit)
		printf ("    Method '%s': inherited from DataFormat.\n", name);
	else
		printf ("    Method '%s' is defined.\n", name);
	return (0);
}



static int
VerifyFiletype (ft, name, target)
FileType ft;
char *name;
DataFormat *target;
/*
 * Map this file type to a format and make sure the format pointer and
 * name matches.
 */
{
	DataFormat *fmt;

	if (! getFormat(ft))
	{
		msg_ELog (EF_PROBLEM, 
			  "Skipping filetype '%s' check: NULL format", name);
		return (1);
	}

	fmt = getFormat(ft);
	if (fmt != target)
	{
		msg_ELog (EF_PROBLEM, " * Filetype '%s' does not map to %s",
			  name, target->f_name);
		return (1);
	}
	if (strcmp (fmt->f_name, name))
	{
		msg_ELog (EF_PROBLEM, " * Filetype '%s' format has name '%s'",
			 name, fmt->f_name);
		return (1);
	}
	if (fmt->f_ftype != ft)
	{
		msg_ELog (EF_PROBLEM, " * Strange: '%s' format filetype is %d",
			 name, fmt->f_ftype);
		return (1);
	}
	return (0);
}


static int
MatchType (ft, fn, match)
FileType ft; 	/* file type */
char *fn; 	/* file name */
int match;	/* nonzero if the name and type should match */
/*
 * Check dfa_CheckName
 */
{
	int findt;

	if ((!dfa_CheckName(ft,fn)) != (!match))
	{
		msg_ELog (EF_PROBLEM, " * CheckName(%i,%s) should return %s",
			  (int)ft, fn, (match ? "TRUE" : "FALSE"));
		return (1);
	}
	/*
	 * Now reverse the check.
	 */
	findt = dfa_FindFormat (fn);
	if ((!match) != (findt != ft))
	{
		msg_ELog (EF_PROBLEM, " * FindFormat(%s) mismatch", fn);
		return (1);
	}
	return (0);
}



static int
TestFormats ()
/*
 * Loop through each format and print information about it 
 */
{
    int nerror = 0;
    int i, j;
    DataFormat *fmt;

    for (i = 0; i < NumFormats; ++i)
    {
        if (! getFormat((FileType)i))
	{
	    msg_ELog (EF_PROBLEM, " * Format %d: %s\n", i,
		      "NULL table pointer");
	    ++nerror;
	    continue;
	}
	fmt = getFormat((FileType)i);
    /*
     * Start printing information about the format
     */
	if (Verbose)
	{
	    printf ("--- Format: %s; %s: %s; filetype %d; %s\n",
		    fmt->f_name, "extensions", fmt->f_ext, 
		    fmt->f_ftype, (fmt->f_readonly) ?
		    "read-only" : "read-write");
	    printf ("    Org/class pairs:\n");
	}
	for (j = 0; j < fmt->f_ncompat; ++j)
	{
	    if (! dfa_OrgClassCompat (fmt, fmt->f_compat[j].c_org,
				      fmt->f_compat[j].c_class))
	    {
	    /* Now this is strange */
		msg_ELog (EF_PROBLEM, "%s %s",
			  "org/class pair in fmt",
			  "fails OrgClassCompat test");
		++nerror;
	    }
	    if (! Verbose)
		continue;
	    printf ("\t%s <-> %s\n",
		    DS_OrgName[fmt->f_compat[j].c_org],
		    DC_ClassName[fmt->f_compat[j].c_class]);
	}
	if (Verbose)
	    printf ("    %s: %i  NOpened: %i  NFree: %i\n",
		    "Open instance size", fmt->f_of_size,
		    fmt->f_nopened, fmt->f_nfree);

    /*
     * Check for minimal set of methods, unless not compiled 
     */
	if ((fmt->f_OpenFile == fmt_OpenNotCompiled &&
	     fmt->f_QueryTime == fmt_QueryNotCompiled) ||
	    fmt->f_CreateFile == fmt_CreateNotCompiled)
	{
	    if (Verbose)
		printf ("    Format not compiled.\n");
	    continue;
	}
    /*
     * We require every format to define: QueryTime, 
     * Open, Close, GetData, GetTimes, GetFields, and DataTimes.
     * Read-write formats must also include:
     * MakeFileName, CreateFile, PutSample (check for PutBlock)
     */
	nerror += VerifyMethod ("QueryTime", (Method)fmt->f_QueryTime, NULL);
	nerror += VerifyMethod ("OpenFile", (Method)fmt->f_OpenFile, NULL);
	nerror += VerifyMethod ("CloseFile", (Method)fmt->f_CloseFile, NULL);
	nerror += VerifyMethod ("GetData", (Method)fmt->f_GetData, NULL);
	nerror += VerifyMethod ("DataTimes", (Method)fmt->f_DataTimes, 
				(Method)fmt_DataTimes);
	nerror += VerifyMethod ("GetTimes", (Method)fmt->f_GetTimes, NULL);
	nerror += VerifyMethod ("GetFields", (Method)fmt->f_GetFields, NULL);
	if (! fmt->f_readonly)
	{
	    nerror += VerifyMethod ("MakeFileName", 
				    (Method)fmt->f_MakeFileName,
				    (Method)fmt_MakeFileName);
	    nerror += VerifyMethod ("CreateFile", (Method)fmt->f_CreateFile, 
				    NULL);
	    if (fmt->f_PutBlock && Verbose)
	    {
		printf ("    (PutBlock is defined)\n");
		if (fmt->f_PutSample && Verbose)
		    printf ("    (PutSample also defined)\n");
	    }
	    if (! fmt->f_PutBlock)
		nerror += VerifyMethod ("PutSample", (Method)fmt->f_PutSample,
					NULL);
	}
    }

/*
 * Now make sure the map between FileType enum and DataFormat
 * pointer is correct.
 */
    nerror += VerifyFiletype (FTNetCDF, "netCDF", netcdfFormat);
    nerror += VerifyFiletype (FTBoundary, "Boundary", boundaryFormat);
    nerror += VerifyFiletype (FTRaster, "Raster", rasterFormat);
    nerror += VerifyFiletype (FTCmpRaster, "CmpRaster", cmpRasterFormat);
    nerror += VerifyFiletype (FTZebra, "Zebra", zebraFormat);
    nerror += VerifyFiletype (FTZeb, "Zebra", zebraFormat);
    nerror += VerifyFiletype (FTGRIB, "GRIB", gribFormat);
    nerror += VerifyFiletype (FTGRIBSfc, "GRIB_sfc", gribSfcFormat);
    nerror += VerifyFiletype (FTGrads, "GRADS", gradsFormat);
    nerror += VerifyFiletype (FTGradsModel,"GRADSModel",gradsmodelFormat);
    if (hdfFormat->f_OpenFile != fmt_OpenNotCompiled)
	nerror += VerifyFiletype (FTHDF, "HDF", hdfFormat);

/*
 * Test file name checks
 */
    nerror += MatchType (FTZebra, "file.znf", TRUE);
    nerror += MatchType (FTZebra, "really.really.long.file.znf", TRUE);
    nerror += MatchType (FTZebra, "file.znf.Z", FALSE);
    nerror += MatchType (FTRaster, "file.rf", TRUE);

#ifdef notyet
/* 
 * We expect these to fail because the reverse map from
 * file extension to file type finds the other file type which
 * shares the extension.
 */
    nerror += (MatchType (FTCmpRaster, "file.rf", TRUE) != 1) ? 1 : 0;
    nerror += (MatchType (FTGRIBSfc, "file.grib", TRUE) != 1) ? 1 : 0;
    nerror += (MatchType (FTGRIBSfc, "file.grb", TRUE) != 1) ? 1 : 0;
#endif

    nerror += MatchType (FTGRIB, "file.grib", TRUE);
    nerror += MatchType (FTGRIB, "file.grb", TRUE);
    nerror += MatchType (FTBoundary, "boundary.bf", TRUE);
    nerror += MatchType (FTGrads, "gradscontrolfile1.ctl", TRUE);
    nerror += MatchType (FTGrads, "gradsdatafile1.data", FALSE);
    nerror += MatchType (FTZebra, "file.hdf", FALSE);
    nerror += MatchType (FTHDF, "file.hdf", TRUE);
    nerror += MatchType (FTHDF, "file.df", FALSE);
    nerror += MatchType (FTHDF, "hdf", FALSE);
    nerror += MatchType (FTNetCDF, "netcdffile.cdf", TRUE);
    nerror += MatchType (FTNetCDF, "netcdffile.nc", TRUE);
    nerror += MatchType (FTNetCDF, "netcdffile.ncf", FALSE);
    nerror += MatchType (FTNetCDF, "netcdffile.nc.cdf", TRUE);
    nerror += MatchType (FTNetCDF, "netcdffile.cdf.nc", TRUE);
    nerror += MatchType (FTNetCDF, ".cdf", FALSE);
    nerror += MatchType (FTNetCDF, ".ncrc", FALSE);

    return (nerror);
}




static int
CheckFileName (p, when, name, details, ndetail)
const Platform *p;
ZebTime *when;
char *name;
dsDetail *details;
int ndetail;
{
	DataFormat *fmt = getFormat(pi_FileType (p));
	int errors = 0;
	char dest[256];

	if (fmt->f_readonly)
		TX_Catch ("format read-only: cannot make file name");
	dfa_MakeFileName (p, when, dest, details, ndetail);
	if (fmt->f_readonly)
		errors += TX_Caught();
	else if (strcmp (name, dest))
	{
		++errors;
		msg_ELog (EF_PROBLEM, "expected filename %s, got %s",
			  name, dest);
	}
	return (errors);
}




static int
T_MakeFileName ()
/*
 * Just grab a platform structure for each of the file formats,
 * then verify that dfa_MakeFileName gives us the correct file names
 * for a given set of details.
 */
{
	static TestPlatform 
		fplat = { "", FTUnknown, OrgScalar, 100, FALSE };
	int errors = 0;
	FileType ft;
	ZebTime when;
	PlatformId pid;
	const Platform *p;
	char name[256];
	char stime[64];
	DataFormat *fmt;
	char *bar;
	char *ext, alternate_ext[16];
	int ndetail;
	dsDetail details[5];
	
	TC_ZtAssemble (&when, 1996, 7, 13, 12, 0, 0, 0);
	for (ft = FTNetCDF; ft <= FTHDF; ++ft)
	{
		fplat.ftype = ft;
		fplat.name = (char *) ds_FTypeName (fplat.ftype);
		pid = MakePlatform (&fplat);
		fmt = getFormat(fplat.ftype);
		p = dt_FindPlatform (pid);
		/* try with four-digit years */
		strcpy (stime, "19960713.120000");
		sprintf (name, "%s.%s%s", fplat.name, stime, fmt->f_ext);
		if ((bar = strrchr (name, '|')))
			*bar++ = '\0';
		/* see what we get */
		ds_SetDetail (DD_FOUR_YEAR, details, 0);
		errors += CheckFileName (p, &when, name, details, 1);
		/* try with two-digit years, which are now deprecated and
		   thus should return four-digit years. */
		strcpy (stime, "19960713.120000");
		sprintf (name, "%s.%s%s", fplat.name, stime, fmt->f_ext);
		if ((bar = strrchr (name, '|')))
			*bar++ = '\0';
		/* see what we get */
		ds_SetDetail (DD_TWO_YEAR, details, 0);
		errors += CheckFileName (p, &when, name, details, 1);
		/* try the default, which is now always 4-digit years */
		strcpy (stime, "19960713.120000");
		sprintf (name, "%s.%s%s", fplat.name, stime, fmt->f_ext);
		if ((bar = strrchr (name, '|')))
			*bar++ = '\0';
		/* see what we get */
		errors += CheckFileName (p, &when, name, NULL, 0);
		/* try no extension */
		ds_SetStringDetail (DD_FILE_NAME, "filename", details, 0);
		errors += CheckFileName (p, &when, "filename", details, 1);
		/* try choosing the other extension */
		ndetail = 0;
		if (bar)
		{
		    strcpy(alternate_ext, bar);
		    ext = alternate_ext;
		    ndetail = ds_SetStringDetail (DD_FILE_EXT, ext,
						  details, ndetail);
		    sprintf (name, "%s.%s%s", fplat.name, stime, ext);

		    errors += CheckFileName (p, &when, name, details, ndetail);
		}
		else
		    ext = dfa_GetExt (fmt);
		ndetail = ds_SetStringDetail (DD_FILE_BASE, "basename",
					      details, ndetail);
		sprintf (name, "%s%s", "basename", ext);
		errors += CheckFileName (p, &when, name, details, ndetail);
		/*
		 * Test error messages for faulty extensions
		 */
		if (fmt->f_readonly)
			continue;
		ds_SetStringDetail (DD_FILE_EXT, "x", details, 0);
		TX_Catch ("will not recognize.*without a leading period");
		dfa_MakeFileName (p, &when, name, details, 1);
		errors += TX_Caught();
		ds_SetStringDetail (DD_FILE_EXT, ".x", details, 0);
		TX_Catch ("dfa does not recognize");
		dfa_MakeFileName (p, &when, name, details, 1);
		errors += TX_Caught();
		ds_SetStringDetail (DD_FILE_EXT, 0, details, 0);
		TX_Catch ("no string value");
		dfa_MakeFileName (p, &when, name, details, 1);
		errors += TX_Caught();
	}
	return (errors);
}



static int
T_DFAStatus ()
/*
 * Show the current status of the DataFormat interface, such as
 * number of open files, what those files are, LRU count, space reserved
 * in linked lists of free open files for each format.
 */
{
	OpenFile *ofp;
	DataFormat *fmt;
	int num, space;
	int err = 0;
	int i;

	if (!Verbose && !Debug)
		return (0);
	printf ("Open files: (max %d, open %d, purged %d, re-opens %d)\n",
		MaxOpenFiles, OF_NOpen, OF_NPurged, OF_Reopens);
	ofp = OpenFiles;
	num = space = 0;
	while (ofp)
	{
		printf(" %3d %8li %2s %10s %45s\n", ofp->of_lru, 
		       (long)ofp->of_df.df_core.dfc_rev, 
		       ofp->of_write ? "rw" : "ro", ofp->of_fmt->f_name, 
		       ofp->of_df.df_core.dfc_name);
		num++;
		space += ofp->of_fmt->f_of_size;
		ofp = ofp->of_next;
	}
	if (num != OF_NOpen)
	{
		++err;
		msg_ELog (EF_PROBLEM, "OF_NOpen != count of link list");
	}
	printf ("-----\n %3d open files, taking %i bytes.\n", num, space);
	/*
	 * Check the memory in the format lookaside lists as well
	 */
	printf ("\nFormats:\n");
	for (i = 0; i < NumFormats; ++i)
	{
	  	fmt = getFormat((FileType)i);
		num = space = 0;
		ofp = fmt->f_of_free;
		while (ofp)
		{
			++num;
			space += fmt->f_of_size;
			ofp = ofp->of_next;
		}
		printf (" %10s opened:%-3d freed:%-3d %s:%-3d for %3i bytes\n",
			fmt->f_name, fmt->f_nopened, fmt->f_nfree,
			"lookaside", num, space);
	}
	return (err);
}




TestRoutine DataFormatTests[] = 
{
	{ "checkformat", FTUnknown, DCC_None, TR_BEGIN, TestFormats,
	  "check DataFormat static structures and method inheritance" },
	{ "makefilename", FTUnknown, DCC_None, TR_BEGIN, T_MakeFileName,
	  "check makefilename methods and detail handling" },
	{ "dfastats", FTUnknown, DCC_None, TR_BEGIN, T_DFAStatus,
	  "report dynamic status of open files and formats" },
	END_TESTS
};

