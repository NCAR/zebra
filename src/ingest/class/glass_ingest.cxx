/* -*- mode: c++; c-basic-offset: 8; -*-
 * $Id: glass_ingest.cxx,v 2.22 2002-11-19 22:15:56 granger Exp $
 *
 * Ingest GLASS data into the system.
 *
 * Type 'glass_ingest -help' for usage info
 *
 */
/*
  NOTES:

  altitude units in glass are m above msl

  the 3-digit seconds problem is being handled by zero'ing any out-of-range
  seconds values in the launch time in the header

  zebra skew-t plotting needs dewpoint (dp) field, but dp not in D-file,
  so we calculate and insert it.

  apparently class_ingest accounts for files stored in reverse chronological
  order.  We can account for this by testing after samples are stored and
  calling sort samples if necessary.

  fix for positive offset in first line: if launch offset on first line is
  0 < 10000, then it is a negative launch time whose negative sign has been
  truncated by field width.  Change it to a negative time rather than
  skipping it, since that line is the surface data, and the surface data
  should still be ok.  If >= 10000, then the launch button was not pressed,
  so we don't know what time to assign to the surface data.  Use -999 as a
  valid pre-launch time which hopefully flags the unusual circumstances.

  The first sample in the file is the surface data.  It should have a 
  negative time offset (less than -30), and it is stored in different
  fields than the sounding samples.

*/

/*		Copyright (C) 1987-92 by UCAR
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

#include <errno.h>
#include <stdio.h>
#include <math.h>
#include <string>

#include <iostream>
#include <fstream>
#include <sstream>

using std::string;
using std::ostream;
using std::istream;
using std::cout;
using std::cerr;
using std::endl;

#include <ctype.h>
#include <defs.h>
#include <message.h>
#include <timer.h>
#include <DataStore.h>
extern "C" 
{
#include <ingest.h>
#include <met_formulas.h>
}

RCSID("$Id: glass_ingest.cxx,v 2.22 2002-11-19 22:15:56 granger Exp $")

#include <ZTime.h>
#define FC_DEFINE_FIELDS
#include "FieldClass.h"
#undef FC_DEFINE_FIELDS

// Solaris 8 defines this and it messes things up
#ifdef tdelta
#undef tdelta
#endif

class AppException
{
public:
	AppException (const string &msg) : m_msg (msg)
	{}

	AppException () : m_msg("unknown application exception") { }

	string message () const { return m_msg; }

protected:
	string m_msg;
};


class LibraryException : public AppException
{
public:
	LibraryException (unsigned int err, const string &msg) :
		AppException (msg),
		m_errno (err)
	{}

	LibraryException (const string &msg) :
		AppException (strerror (errno)),
		m_errno (errno)
	{
		m_msg += ": ";
		m_msg += msg;
	}

	LibraryException () :
		AppException (strerror (errno)),
		m_errno (errno)
	{}

	unsigned int error () const { return m_errno; }

protected:
	unsigned int m_errno;
};


/*
 * Structure to build a linked list of site name -> platform name translations
 */
typedef struct _SiteTrans {
	char	*site;
	int	slen;
	char	*plat;
	struct _SiteTrans	*next;
} SiteTranslation;

static SiteTranslation	*SiteTransList = (SiteTranslation *) 0;

extern const char *FT_VWind;	// Bug in older versions of Field.h

/*
 * Define the field types we need to represent soundings.
 */
DefineField(F_tdelta, "tdelta", "", "s", "time since launch")
DefineField(F_pres, "pres", FT_Pres, "hPa", "pressure")
DefineField(F_temp, "temp", FT_Temp, "degC", "temperature")
DefineField(F_rh, "rh", FT_RH, "%", "relative humidity")
DefineField(F_wdir, "wdir", FT_WDir, "degrees", "wind direction")
DefineField(F_wspd, "wspd", FT_WSpd, "m/s", "wind speed")
DefineField(F_dz, "dz", "", "m/s", "ascent rate")
DefineField(F_lon, "lon", "", "degrees", "east longitude")
DefineField(F_lat, "lat", "", "degrees", "north latitude")
DefineField(F_alt, "alt", "", "m", "altitude above MSL")
DefineField(F_sa, "sa", "", "1", "GPS satellites")

DefineField(F_stime, "stime", "", "s", "time since launch time")
DefineField(F_spres, "spres", FT_Pres, "hPa", "surface pressure")
DefineField(F_stemp, "stemp", FT_Temp, "degC", "surface temperature")
DefineField(F_srh, "srh", FT_RH, "%", "surface relative humidity")
DefineField(F_swdir, "swdir", FT_WDir, "degrees", "surface wind direction")
DefineField(F_swspd, "swspd", FT_WSpd, "m/s", "surface wind speed")
DefineField(F_sdz, "sdz", "", "m/s", "pre-launch ascent rate")
DefineField(F_slon, "slon", "", "degrees", "pre-launch east longitude")
DefineField(F_slat, "slat", "", "degrees", "pre-launch north latitude")
DefineField(F_salt, "salt", "", "m", "pre-launch altitude above MSL")
DefineField(F_ssa, "ssa", "", "1", "pre-launch GPS satellites")

DefineField(F_dewpoint, "dp", FT_DP, "degC", "dewpoint")
DefineField(F_sdewpoint, "sdp", FT_DP, "degC", "surface dewpoint")

DefineField(F_u, "u_wind", FT_UWind, "m/s", "u wind component")
DefineField(F_v, "v_wind", FT_VWind, "m/s", "v wind component")
DefineField(F_range, "range", "", "km", "range from launch site")
DefineField(F_az, "az", "", "degrees", "azimuth from launch site")
DefineField(F_qpres, "qpres", "", "hPa", "pressure quality")
DefineField(F_qtemp, "qtemp", "", "degC", "temperature quality")
DefineField(F_qrh, "qrh", "", "%", "humidity quality")
DefineField(F_qu, "qu", "", "m/s", "u wind quality")
DefineField(F_qv, "qv", "", "m/s", "v wind quality")
DefineField(F_qwind, "qwind", "", "m/s", "wind quality")

/*
 * The list of fields available from CLASS files.  These are fixed in the
 * CLASS format.
 */
struct ClassFileRecord
{
	F_tdelta tdelta;
	F_pres pres;
	F_temp temp;
	F_dewpoint dp;
	F_rh rh;
	F_u u;
	F_v v;
	F_wspd wspd;
	F_wdir wdir;
	F_dz dz;
	F_lon lon;
	F_lat lat;
	F_range range;
	F_az az;
	F_alt alt;
	F_qpres qpres;
	F_qtemp qtemp;
	F_qrh qrh;
	F_qu qu;
	F_qv qv;
	F_qwind qwind;

	template <class E>
	void enumerate (E &e)
	{
		e << tdelta << pres << temp << dp << rh << u << v;
		e << wspd << wdir << dz;
		e << lon << lat << range << az << alt;
		e << qpres << qtemp << qrh << qu << qv << qwind;
	}
};


struct GlassFileRecord 
{
	static const int MAX_SURFACE = 100;

	F_tdelta tdelta;
	F_pres pres;
	F_temp temp;
	F_rh rh;
	F_wdir wdir;
	F_wspd wspd;
	F_dz dz;
	F_lon lon;
	F_lat lat;
	F_alt alt;
	F_sa sa;

	template <class E>
	void enumerate (E &e)
	{
		e << tdelta << pres << temp << rh;
		e << wdir << wspd << dz;
		e << lon << lat << alt;
		e << sa;
	}

	F_stime stime[MAX_SURFACE];
	F_spres spres[MAX_SURFACE];
	F_stemp stemp[MAX_SURFACE];
	F_srh srh[MAX_SURFACE];
	F_swdir swdir[MAX_SURFACE];
	F_swspd swspd[MAX_SURFACE];
	F_sdz sdz[MAX_SURFACE];
	F_slon slon[MAX_SURFACE];
	F_slat slat[MAX_SURFACE];
	F_salt salt[MAX_SURFACE];
	F_ssa ssa[MAX_SURFACE];

	F_sdewpoint sdp[MAX_SURFACE];

	template <class E>
	void enumerateSurfaceFields (E &e)
	{
		e << stime[nsurface] << spres[nsurface];
		e << stemp[nsurface] << srh[nsurface];
		e << swdir[nsurface] << swspd[nsurface];
		e << sdz[nsurface] << slon[nsurface];
		e << slat[nsurface] << salt[nsurface];
		e << ssa[nsurface];
	}

	int nsurface;
};


#define F_REDIRECT(field) \
	F_##field & field() { switch (rectype) { \
		case GLASS: \
			return gl->field; break; \
		case CLASS: \
		default: \
			return cl->field; break; \
		} \
	}

struct Sounding
{
	enum RecordType { GLASS, CLASS };

	string site;
	string location;
	string launch_time;
	ZTime tlaunch;

	RecordType rectype;

	GlassFileRecord *gl;
	ClassFileRecord *cl;

	Sounding (RecordType rt = GLASS) : rectype(rt), gl(0), cl(0)
	{
		switch (rectype)
		{
		case GLASS:
			gl = new GlassFileRecord;
			break;
		case CLASS:
			cl = new ClassFileRecord;
			break;
		}
	}

	~Sounding ()
	{
		if (gl) delete gl;
		if (cl) delete cl;
	}

	template <class E>
	void enumerate (E &e)
	{
		// Divert the enumeration to the current record tupe.
		switch (rectype)
		{
		case GLASS:
			gl->enumerate (e);
			break;
		case CLASS:
			cl->enumerate (e);
			break;
		}
	}

	F_REDIRECT(pres)
	F_REDIRECT(temp)
	F_REDIRECT(rh)
	F_REDIRECT(tdelta)		
	F_REDIRECT(wdir)		
	F_REDIRECT(wspd)		
	F_REDIRECT(dz)		
	F_REDIRECT(lat)		
	F_REDIRECT(lon)		
	F_REDIRECT(alt)		

};


/* Define a class to print field info to an output stream */
class ofield
{
public:
	ofield (ostream &out_) : out(out_)
	{}

	ofield &operator<< (const ZField &f)
	{
		out << f.FullName() << endl;
		return *this;
	}

private:
	ostream &out;
};


/* Define an operator for checking the existence of a field in a sounding
 * record.
 */
class CheckForField
{
public:
	CheckForField &find (string name)
	{
		m_name = name;
		m_found = 0;
		return *this;
	}

	CheckForField & operator << (const ZField &f)
	{
		if (m_name == f.Name())
			m_found = &f;
		return *this;
	}

	const ZField *found() { return m_found; }

private:
	const ZField *m_found;
	string m_name;
};


static bool
InternalField (const ZField &f)
{
	return (f.fieldId() == F_lat::fieldId() ||
		f.fieldId() == F_lon::fieldId() ||
		f.fieldId() == F_alt::fieldId());
}


struct CollectFields
{
	CollectFields (int *nfields_, FieldId *fields_) :
		nfields(nfields_), fields(fields_)
	{
		*nfields = 0;
	}
	CollectFields & operator << (const ZField &f)
	{
		if (! InternalField (f))
		{
			fields[(*nfields)++] = f.fieldId();
		}
		return *this;
	}
	int *nfields;
	FieldId *fields;
};



static void	ReadHeader (DataChunk *dc, char *file, Sounding &snd);
static void	ReadSamples (DataChunk *dc, char *file, Sounding &snd);
static void	SetPlatform (DataChunk *dc, char PlatformName[]);
static void 	GetPlatformName (const Sounding &snd, char plat[]);
static void	BuildTranslationTable (const char *tfilename);
static void	ParseCommandLineOptions (int *argc, char *argv[]);
static char *	GetNextString (char *, char*);
static void 	StoreSurfaceRecords (DataChunk *dc, Sounding &snd);
extern "C"
{ 
	static void Usage(char *prog_name);
}

# define MAX_FIELDS 32

static const char *INGEST_NAME = "glass_ingest";
static const float BADVAL = -999.0; 	/* Specific to GLASS files? */
static const int ZEB_PROBLEM = 1;
static const int FILE_PROBLEM = 8;
static const int PROBLEM = 99;
static const int NO_PROBLEM = 0;


/*
 * The name of the site name -> platform name translations file (if any)
 */
static char	*Tfilename = NULL;


/*
 * Global debugging flags set from command line
 */
char JustShowFields = (char)0;	/* Initially false */
char DumpDataChunk = (char)0;   /* Dump chunks AS BUILT rather than
				 * like ingest.c option which is
				 * WHEN STORED */
char PlatformName[256] = "";
Sounding::RecordType SoundingType = Sounding::GLASS;

/* Use one time variable convention in files, rather than base_time. */
static bool OptionBaseTime = false;

static void GlassIngest (int argc, char *argv[]);


int main (int argc, char **argv)
{
	try {
		GlassIngest (argc, argv);
	}
	catch (const AppException &e)
	{
		cerr << e.message() << endl;
		IngestLog(EF_PROBLEM, "%s", e.message().c_str());
		exit (PROBLEM);
	}

	IngestLog(EF_DEBUG, "Finished...");
	return (NO_PROBLEM);
}


static DataChunk *
CreateSoundingDC (Sounding &snd)
{
	DataChunk *dc;
	FieldId fields[MAX_FIELDS];
	int nfields;
	int i;

	dc = dc_CreateDC (DCC_NSpace);

	// Collect the sounding fields (different from the surface fields)
	CollectFields cf (&nfields, fields);
	snd.enumerate (cf);
	// Add the dewpoint field which we'll derive (from D-files)
	fields[nfields++] = F_dewpoint::fieldId();
	for (i = 0; i < nfields; ++i)
	{
		dc_NSDefineVariable (dc, fields[i], 0, 0, 0);
	}
	IngestLog(EF_DEBUG,"%d sounding fields set in datachunk",nfields);

	dc_SetBadval(dc, BADVAL);
	IngestLog(EF_DEBUG,"bad_value_flag set to %6.2f",BADVAL);

	dc_NSAllowRedefine (dc, 1);
	return dc;
}


static void 
GlassIngest (int argc, char *argv[])
{
	char 	*filename;	/* Name of the snding file, pts to argv[1] */
	DataChunk *Dchunk;   	/* The DataChunk we'll be building */
	dsDetail details[4];
	int ndetail = 0;
/*
 * Build the command-line history before removing any options.
 **/
	string history;
	for (int i = 0; i < argc; ++i)
	{
		history += argv[i];
		history += " ";
	}
	history += "\n";
	history += Z_rcsid();
/*
 * Get our command-line options, setting appropriate global variables.
 * Only the file name should remain.
 */
	ParseCommandLineOptions(&argc, argv);
/*
 * Always ingest to the GLASS data model.  If the SoundingType is
 * actually class format, then fields will be copied from the
 * class records.
 */
	Sounding snd(Sounding::GLASS);
	if (JustShowFields)
	{
		ofield of(cout);
		snd.enumerate (of);
		cout << endl;
		return;
	}
	if (argc < 2)		/* Need a file name arg */
	{
		Usage(argv[0]);
		throw AppException ("need a file name on command line");
	}
	filename = argv[1];
/*
 * Initialize usy, message, DataStore, and fields all at once
 */
	IngestInitialize(const_cast<char *>(INGEST_NAME));
/*
 * Build the table for special site name -> platform name translations
 */ 
	if (Tfilename)
		BuildTranslationTable (Tfilename);
/*
 * Create a new data chunk.
 */
	Dchunk = CreateSoundingDC(snd);
	dc_SetGlobalAttr (Dchunk, "history", 
			  const_cast<char*>(history.c_str()));
/*
 * Open sounding file, get platform name and check for validity
 */
	IngestLog (EF_INFO, "Ingesting '%s'", filename);

	ReadHeader (Dchunk, filename, snd);

	// Check whether name set on command line
	if (! PlatformName[0])
	{
		GetPlatformName (snd, PlatformName);
	}

	SetPlatform (Dchunk, PlatformName);

	if (DumpDataChunk)
	{
		IngestLog (EF_DEBUG, "Dumping data chunks to stdout");
		dc_DumpDC (Dchunk);
	}
/*
 * Now just read the sounding data into the datachunk.
 */
	ReadSamples (Dchunk, filename, snd);
	IngestLog(EF_DEBUG,"%s: each field has been loaded", PlatformName);
	if (DumpDataChunk) dc_DumpDC(Dchunk);
/*
 * Send everything to the data store
 */
	ds_SetStringDetail (DD_FILE_EXT, ".nc", details, ndetail++);
	ds_SetDetail (DD_FOUR_YEAR, details, ndetail++);
	if (! OptionBaseTime)
		ds_SetDetail (DD_NC_ONE_TIME, details, ndetail++);
	IngestLog(EF_DEBUG,"%s: Sending data to DataStore", PlatformName);
	if (!ds_StoreBlocks (Dchunk, /*newfile*/ TRUE, details, ndetail))
	{
		IngestLog(EF_EMERGENCY,"%s: Data store failed", PlatformName);
	}
	else
		IngestLog(EF_INFO,
		   "%s: CLASS data loaded into DataStore", PlatformName);
	dc_DestroyDC (Dchunk);
}



static void
SetPlatform (DataChunk *dc, char PlatformName[])
/*
 * If running standalone, provide a default platform definition for this name.
 */
{
	if (ds_IsStandalone() && 
	    (ds_LookupPlatform(PlatformName) == BadPlatform))
	{
		PlatClassId cid = ds_LookupClass ("CLASS");

		if (cid == BadClass)
		{
			PlatClassRef pc = ds_NewClass ("CLASS");
			IngestLog(EF_DEBUG, "default platform class 'CLASS'");
			/* relies on DefDataDir from ingest module */
			ds_AssignClass (pc, OrgScalar, FTNetCDF,
					TRUE/*mobile*/);
			ds_SetMaxSample (pc, 20000);
			ds_SetComment (pc, "standalone CLASS platform ");
			cid = ds_DefineClass (pc);
		}
		/*
		 * Create a new instance for this platform name.
		 */
		IngestLog (EF_DEBUG, "instantiating platform '%s'", 
			   PlatformName);
		ds_DefinePlatform (cid, PlatformName);
	}
	if ((dc->dc_Platform = ds_LookupPlatform (PlatformName)) == 
	    BadPlatform)
	{
		throw AppException (string("Unknown platform: ")
				    + string(PlatformName));
	}
}



/* GetPlatformName ----------------------------------------------------
 *   Opens 'classfile' and extracts an all-lowercase platform name
 */
static void
GetPlatformName (const Sounding &snd, char plat[])
{
	string site;
	unsigned int i;
	SiteTranslation	*strans;
/*
 * Get the site name and make it lower case
 */
	site = snd.site;
	for (i = 0; i < site.length(); i++)
		site[i] = tolower (site[i]);
/*
 * Convert the site name to a platform name.
 * First check against the translation list, then look for the CLASS
 * standard "FIXED, xxx" or "MOBILE, xxx".  Otherwise, just use the
 * site name as the platform name.
 */
	strans = SiteTransList;
	while (strans)
	{
		if (! strncmp (site.c_str(), strans->site, strans->slen))
		{
			strcpy (plat, strans->plat);
			break;
		}

		strans = strans->next;
	}

	const char *ss = site.c_str();
	if (! strans)
	{
		if ((sscanf (ss, "fixed, %s", plat) == 1) ||
			(sscanf (ss, "fixed %s", plat) == 1) ||
			(sscanf (ss, "mobile, %s", plat) == 1) ||
			(sscanf (ss, "mobile %s", plat) == 1))
			/* do nothing */;
		else
			strcpy (plat, site.c_str());
	}
}


/* ParseCommandLineOptions --------------------------------------------
 *    Set global variables from command-line options, leaving only
 *    the expected file and field names in the arg list
 */
static void
ParseCommandLineOptions (int *argc, char *argv[])
{
	int i;
/*
 * First parse any of the general ingest options
 */
	IngestParseOptions(argc, argv, (void (*)())Usage);
/*
 * Now check for any of our own debug flags on the command line
 */
	i = 1;
	while (i < *argc)
	{
		if (streq(argv[i],"-show") ||
		    streq(argv[i],"-s"))
		{
			DumpDataChunk = (char)1;
			IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (streq(argv[i],"-nobasetime"))
		{
			OptionBaseTime = false;
			IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (streq(argv[i],"-basetime"))
		{
			OptionBaseTime = true;
			IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (streq(argv[i],"-fields"))
		{
			JustShowFields = (char)1;
			IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (streq(argv[i],"-glass") || streq(argv[i],"-class"))
		{
			IngestLog (EF_INFO, "%s option deprecated. "
				   "Format will be determined automatically.",
				   argv[i]);
			IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (! strncmp (argv[i], "-t", 2))
		{
			Tfilename = strdup (argv[i+1]);
			IngestRemoveOptions (argc, argv, i, 2);
		}
		else if (! strncmp (argv[i], "-p", 2))
		{
			strcpy (PlatformName, argv[i+1]);
			IngestRemoveOptions (argc, argv, i, 2);
		}
		else
			++i;
	}
}


static void
Usage (char *prog)
{
	// printf ("Usage: %s [options] <file> <fields>\n",prog);
	printf ("Usage: %s [options] <file>\n",prog);
	printf ("       %s -help\n",prog);
	printf ("\nOptions:\n");
	printf ("   -show, -s		Dump data chunk as it's built\n");
	printf ("   -basetime		Include base_time in netcdf file\n");
	printf ("   -nobasetime		Do not include base_time (default)\n");
	printf ("   -fields		List the sounding fields\n");
	printf ("   -trans <tfile>	Use the site/platform translations in 'tfile'\n");
	printf ("   -platform <name>	Explicitly set the platform name\n");
	printf ("\n");
	IngestUsage();
	printf ("\nExamples:\n");
	printf ("   %s -show -log pd i7282220.dpk pres temp rh\n", prog);
	printf ("   %s -fields -class\n\n", prog);
}




static void
BuildTranslationTable (const char *Tfilename)
/*
 * Read 'tfile' and build a table of site name -> platform name translations
 */
{
	unsigned int	i;
	char	line[80], *lp, site[40], plat[40];
	FILE	*tfile;
	SiteTranslation	*newtrans;
/*
 * Open the file (or just return if there is no translations file)
 */
	if (! Tfilename)
		return;

	tfile = fopen (Tfilename, "r");

	if (! tfile)
	{
		throw LibraryException
			(string("cannot open translations file: ") 
			 + string(Tfilename));
	}
/*
 * Loop to read all the lines from the file
 */
	while (fgets (line, sizeof (line), tfile))
	{
		line[strlen (line) - 1] = '\0';
	/*
	 * Get the site and platform strings from this line
	 */
		lp = line;
		if ((lp = GetNextString (site, lp)) == 0 ||
			(lp = GetNextString (plat, lp)) == 0)
		{
			IngestLog (EF_PROBLEM, "Bad translation line '%s'", 
				line);
			throw AppException 
				("Bad site -> platform translation");
		}
	/*
	 * Make things lower case
	 */
		for (i = 0; i < strlen (site); i++)
			site[i] = tolower (site[i]);

		for (i = 0; i < strlen (plat); i++)
			plat[i] = tolower (plat[i]);
	/*
	 * Allocate and build a new entry, making it the head of the
	 * linked list
	 */
		newtrans = (SiteTranslation *) 
			malloc (sizeof (SiteTranslation));

		newtrans->site = strdup (site);
		newtrans->slen = strlen (site);
		newtrans->plat = strdup (plat);
		newtrans->next = SiteTransList;
		SiteTransList = newtrans;

		IngestLog(EF_DEBUG, "Site '%s' -> Platform '%s'", site, plat);
	}
}




static char*
GetNextString (char *ret, char *text)
/*
 * Extract the next string from 'text', either quoted or terminated by
 * white space.  The string is written into 'ret', and the return value
 * of the function is a pointer to the first character after the string
 * (success) or null (failure).
 */
{
	char	*endquote;
/*
 * Remove leading white space
 */
	while (*text == ' ' || *text == '\t')
		text++;
/*
 * Quoted string?
 */
	if (*text == '\"' || *text == '\'')
	{
		if ((endquote = strchr (text + 1, *text)) == 0)
			return ((char *) 0);

		strncpy (ret, text + 1, endquote - text - 1);
		ret[endquote - text - 1] = '\0';

		return (endquote + 1);
	}
/*
 * No quotes, so delineate by whitespace
 */
	if (sscanf (text, "%s", ret) == 1)
		return (text + strlen (ret));
	else
		return ((char *) 0);
}


static string &
trim (string &s)
{
	string::size_type i = 0;
	while (i < s.length() && isspace (s[i]))
		++i;
	s.erase (0, i);
	i = s.length() - 1;
	while (i > 0 && isspace (s[i]))
		--i;
	s.erase (i + 1, string::npos);
	return s;
}
	       


/*
 * Parse the header of the glass file for useful attributes,
 * especially launch time and location, and add all of them
 * as attributes to the datachunk.  Returns zero on success.
 */
static void
ReadHeader (DataChunk *dc, char *file, Sounding &snd)
{
	std::ifstream fin (file);
	if (! fin)
	{
		throw LibraryException (file);
	}
	snd.tlaunch = 0;

	/* Nothing graceful here.  Just read the first 12 lines, parsing
	   those with a colon.  Everything before the colon becomes an
	   attribute, everything after is the value.  Check for specific
	   attributes for the location and launch time.  Lines without
	   colons are accumulated as is.
	*/
	   
	string line;
	string::size_type n;
	int lino = 0;
	char attr[16];
	while (lino < 12 && getline (fin, line))
	{
		++lino;
		trim (line);
		// Just store the entire line as an attribute, since netcdf
		// will not accept the unconventional characters in the
		// header labels.
		sprintf (attr, "header%02d", lino);
		dc_SetGlobalAttr (dc, attr, const_cast<char *>(line.c_str()));
		if ((n = line.find (':')) == string::npos)
		{
			continue;
		}
		// Extract name and value from the line.
		string left(line, 0, n);
		string right(line, n+1, line.length());
		trim (left);
		trim (right);
		IngestLog (EF_DEBUG, "'%s': '%s'", 
			   left.c_str(), right.c_str());

		// Now check for expected attributes.
		int year, mon, day, hour, min, sec;
		if (left.find("GMT Launch Time") != string::npos ||
		    left.find("UTC Release Time") != string::npos)
		{
			// Found the launch time.
			snd.launch_time = right;
			IngestLog (EF_DEBUG, "launch time header found: %s",
				   right.c_str());
			int n = sscanf (right.c_str(), "%d, %d, %d, %d:%d:%d",
					&year, &mon, &day, &hour, &min, &sec);
			IngestLog ((n == 6) ? EF_DEBUG : EF_PROBLEM, 
				   "%d fields matched (should be 6)", n);

			// Zero the seconds if they are out of range, since
			// some D-files have 3-digit seconds for some as
			// yet unexplained reason.
			if (sec < 0 || sec >= 60)
				sec = 0;

			snd.tlaunch.assemble (year, mon, day, hour, min, sec);
			IngestLog (EF_INFO, "parsed launch time: %s",
				   snd.tlaunch.ascTime());
		}

		float eastd, eastm, northd, northm, alt;
		if (left.find ("Launch Location") != string::npos &&
		    sscanf (right.c_str(), "%f %f'E, %f %f'N, %f",
			    &eastd, &eastm, &northd, &northm, &alt) == 5)
		{
			snd.location = right;
		}

		if (left.find ("Release Location") != string::npos)
		{
			snd.location = right;
		}

		if (left.find ("Launch Site") != string::npos ||
		    left.find ("Release Site") != string::npos)
		{
			snd.site = right;
		}
	}

	// If we didn't get a launch time, we've got problems.
	if (snd.tlaunch == 0)
	{
		throw AppException ("Could not find launch time in header");
	}

	// We've also got problems if we didn't read enough header lines.
	if (lino < 12)
	{
		throw AppException ("Failed to read 12 header lines, "
				    "unexpected end of the file.");
	}

	// We should now be at the column header line.  See how many
	// columns we can parse to distinguish "class" format files and
	// D-files.
	if (! getline (fin, line))
	{
		throw AppException ("Could not read column headings, "
				    "file shorter than expected.");
	}
	const char* cp = line.c_str();
	char c1[16], c2[16], c3[16], c4[16], c5[16];
	if (sscanf (cp, " %s %s %s %s %s ", c1, c2, c3, c4, c5) != 5)
		throw AppException ("Could not parse column headers.");
	if (strcmp (c4, "Dewpt") == 0 && strcmp (c5, "RH") == 0)
	{
		SoundingType = Sounding::CLASS;
		IngestLog (EF_INFO, "Reading 'class format' file.");
	}
	else
	{
		SoundingType = Sounding::GLASS;
		IngestLog (EF_INFO, "Reading GLASS D-file format.");
	}

}


/*
 * Define a helper class which can be enumerated through a sounding record
 * to extract each field's value.
 */
class ReadRecord
{
public:
	class InvalidRecord {};

	ReadRecord (istream &in_) : in(in_)
	{}

	template <class F>
	ReadRecord & operator << (F &f)
	{
		if (! (in >> f.value()))
			throw InvalidRecord();
		return *this;
	}		

private:
	istream &in;
};


struct SetRecord
{
	SetRecord (float f) : value(f) {}
	template <class F>
	const SetRecord & operator << (F &f) const
	{ f.value() = value; return *this; }
	float value;
};


struct PrintRecord
{
	PrintRecord (ostream &out_, const string &sep = string(" ")) : 
		out(out_), separator (sep) {}
	template <class F>
	const PrintRecord & operator << (F &f) const
	{ out << separator << f.value(); return *this; }
	ostream &out;
	const string &separator;
};


struct AddRecord
{
	AddRecord (DataChunk *dc_, ZTime &when_, int samp_) :
		dc(dc_), when(when_), sample(samp_) {}
	template <class F>
	AddRecord & operator << (F &f) 
	{
		if (! InternalField (f))
		{
			dc_NSAddSample (dc, &when, sample, 
					f.fieldId(), &f.value());
		}
		return *this;
	}
	DataChunk *dc;
	ZTime when;
	int sample;
};


double ComputeDewpoint (Sounding& snd)
{
	// From Chris Burghart:
	//
	// From relative humidity (%) and temperature (K!), you can
	// get to dewpoint (also K) using two functions:
	//
	//   dp = dewpoint (0.01 * rh * e_sw (t)); */
	//
	double ddp = BADVAL;
	if (snd.rh() != BADVAL && snd.temp() != BADVAL)
	{
		ddp = dewpoint (0.01 * snd.rh() * 
				e_sw(snd.temp() + 273.15));
		ddp -= 273.15;
		if (isnan(ddp))
			ddp = BADVAL;
	}
	return ddp;
}


void
CopyFields (GlassFileRecord& snd, ClassFileRecord& cfr)
{
	snd.tdelta = cfr.tdelta;
	snd.pres = cfr.pres;
	snd.temp = cfr.temp;
	snd.rh = cfr.rh;
	snd.wdir = cfr.wdir;
	snd.wspd = cfr.wspd;
	snd.dz = cfr.dz;
	snd.lon = cfr.lon;
	snd.lat = cfr.lat;
	snd.alt = cfr.alt;
	snd.sa = BADVAL;
}


/*
 * Read the samples from the sounding file into the datachunk, skipping
 * samples with bad times, setting all fields to bad values when important
 * fields are bad.  Throw an exception if anything unexpected happens.  Use
 * a Sounding for the expected format type to input the records before
 * storing them in a glass sounding.
 */
static void
ReadSamples (DataChunk *dc, char *file, Sounding &snd)
{
	ClassFileRecord cfinput;
	std::ifstream fin (file);
	if (! fin)
	{
		throw LibraryException (file);
	}
	
	// Give a guess about how many samples in a typical sounding
	// to avoid some memory thrashing.
	dc_HintNSamples (dc, 10000, /*decrease*/ 0);
	dc_SetLocAltUnits (dc, AU_kmMSL);

	// Skip the header
	string line;
	int lino = 0;
	int badlines = 0;
	while (getline (fin, line) && line.find ("-----") == string::npos)
	{
		++lino;
		// keep reading
	} 

	// Start reading data values
	GlassFileRecord *gl = snd.gl;
	gl->nsurface = 0;
	ZTime when = 0;
	while (getline (fin, line))
	{
		++lino;
		std::istringstream ss(line.c_str());
		try {
			ReadRecord rr(ss);
			if (SoundingType == Sounding::GLASS)
			{
				snd.enumerate (rr);
			}
			else
			{
				cfinput.enumerate (rr);
				CopyFields (*snd.gl, cfinput);
			}
		}
		catch (ReadRecord::InvalidRecord &)
		{
			IngestLog (EF_PROBLEM, 
				   "Invalid record on line %d: %s", 
				   lino, line.c_str());
			continue;
		}			

		// Fix for positive offset in first line: 
		// -------------------------------------------------------
		// If launch offset on first line is 0 < 10000, then it is
		// a negative launch time whose negative sign has been
		// truncated by field width.  Change it to a negative time
		// rather than skipping it, since that line is the surface
		// data, and the surface data should still be ok.  If >=
		// 10000, then the launch button was not pressed, so we
		// don't know what time to assign to the surface data.  Use
		// -999 as a valid pre-launch time which hopefully flags
		// the unusual circumstances.
		if (when == 0 && snd.tdelta() > 0)
		{
			if (snd.tdelta() < 10000)
			{
				snd.tdelta() = 0 - snd.tdelta();
			}
			else
			{
				snd.tdelta() = -999;
			}
		}
		else if (when != 0 && snd.tdelta() == BADVAL)
		{
			IngestLog (EF_DEBUG, 
				   "Bad time value on line %d: %s",
				   lino, line.c_str());
			++badlines;
			continue;
		}

		// Now we should have the fields, we need to insert a sample.
		when = snd.tlaunch;
		when += snd.tdelta().value();

		// Translate various other bad values used by Aspen "class
		// format" output.
		if (snd.pres() == 9999.0) snd.pres() = BADVAL;
		if (snd.temp() == 999.0) snd.temp() = BADVAL;
		if (snd.rh() == 999.0) snd.rh() = BADVAL;
		if (snd.wdir() == 999.0) snd.wdir() = BADVAL;
		if (snd.wspd() == 999.0) snd.wspd() = BADVAL;
		if (snd.dz() == 99.0) snd.dz() = BADVAL;
		if (snd.lon() == 999.0) snd.lon() = BADVAL;
		if (snd.lat() == 999.0) snd.lat() = BADVAL;
		if (snd.alt() == 99999.0) snd.alt() = BADVAL;
		    
		if (snd.pres() == BADVAL)
		{
#ifdef notdef
			// If the pressure value is bad, store all fields
			// as bad.
			SetRecord sr(BADVAL);
			snd.enumerate (sr);
#endif
			++badlines;
		}

		// Class format already contains the dewpoint so no need
		// to compute it.
		F_dewpoint dp;
		if (SoundingType == Sounding::GLASS)
		{
			dp = ComputeDewpoint (snd);
		}
		else
		{
			dp = cfinput.dp;
			if (dp == 999.0) dp = BADVAL;
		}

		// Records with negative tdelta are stored as surface fields.
		int sample = dc_GetNSample(dc);
		if (snd.tdelta() < 0 && sample == 0)
		{
			if (gl->nsurface >= GlassFileRecord::MAX_SURFACE)
				continue;
			int i = gl->nsurface;
			gl->stime[i] = gl->tdelta.value();
			gl->spres[i] = gl->pres.value();
			gl->stemp[i] = gl->temp.value();
			gl->srh[i] = gl->rh.value();
			gl->swdir[i] = gl->wdir.value();
			gl->swspd[i] = gl->wspd.value();
			gl->sdz[i] = gl->dz.value();
			gl->slon[i] = gl->lon.value();
			gl->slat[i] = gl->lat.value();
			gl->salt[i] = gl->alt.value();
			gl->ssa[i] = gl->sa.value();
			gl->sdp[i] = dp.value();
			++gl->nsurface;
			continue;
		}

		// Add the surface data to the datachunk at the first
		// post-launch point, but only if we found some.
		if (sample == 0)
		{
			if (gl->nsurface > 0)
			{
				StoreSurfaceRecords (dc, snd);
				msg_ELog (EF_DEBUG,
					  "%d surface records found.",
					  gl->nsurface);
			}
			else
			{
				msg_ELog (EF_DEBUG, "No surface records.");
			}
		}

		// Continue with the normal sounding record case.
		Location loc;
		loc.l_lat = snd.lat();
		loc.l_lon = snd.lon();
		loc.l_alt = snd.alt();
		if (loc.l_alt != BADVAL)
			loc.l_alt /= 1000.0;	// convert to km

		AddRecord estore(dc, when, sample);
		snd.enumerate (estore);
		dc_SetLoc (dc, sample, &loc);

		dc_NSAddSample (dc, &when, sample, dp.fieldId(), &dp.value());
	}
	IngestLog (EF_INFO,
		   "%d bad times or pressure points, %d good samples", 
		   badlines, dc_GetNSample(dc));

	// If we didn't get anything useful, throw up our hands.
	if (dc_GetNSample (dc) == 0)
	{
		throw AppException ("No good sounding points in file");
	}

	// Check whether sample times are reversed and need to be sorted.
	ZTime first;
	ZTime last;
	dc_GetTime (dc, 0, &first);
	dc_GetTime (dc, dc_GetNSample(dc)-1, &last);

	if (first > last)
	{
		dc_SortSamples (dc);
	}
}



static void
StoreSurfaceRecords (DataChunk *dc, Sounding &snd)
{
	GlassFileRecord *gl = snd.gl;
	int nfields;
	FieldId fields[MAX_FIELDS];

	// Define the dimension for surface points
	dc_NSDefineDimension (dc, F_stime::fieldId(), gl->nsurface);
	CollectFields sf (&nfields, fields);
	gl->enumerateSurfaceFields (sf);
	fields[nfields++] = F_sdewpoint::fieldId();
	FieldId dimn = F_stime::fieldId();
	for (int i = 0; i < nfields; ++i)
	{
		dc_NSDefineVariable (dc, fields[i], 1, &dimn, /*static*/TRUE);
	}
	IngestLog(EF_DEBUG,"%d surface fields set in datachunk",nfields);

	dc_NSAddStatic (dc, F_stime::fieldId(), gl->stime);
	dc_NSAddStatic (dc, F_spres::fieldId(), gl->spres);
	dc_NSAddStatic (dc, F_stemp::fieldId(), gl->stemp);
	dc_NSAddStatic (dc, F_srh::fieldId(), gl->srh);
	dc_NSAddStatic (dc, F_swdir::fieldId(), gl->swdir);
	dc_NSAddStatic (dc, F_swspd::fieldId(), gl->swspd);

	dc_NSAddStatic (dc, F_sdz::fieldId(), gl->sdz);
	dc_NSAddStatic (dc, F_slon::fieldId(), gl->slon);
	dc_NSAddStatic (dc, F_slat::fieldId(), gl->slat);
	dc_NSAddStatic (dc, F_salt::fieldId(), gl->salt);
	dc_NSAddStatic (dc, F_ssa::fieldId(), gl->ssa);
	dc_NSAddStatic (dc, F_sdewpoint::fieldId(), gl->sdp);
}
