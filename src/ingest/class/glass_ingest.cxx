/* -*- mode: c++; c-basic-offset: 8; -*-
 * $Id: glass_ingest.cxx,v 2.7 1999-11-22 20:37:39 granger Exp $
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

  zebra skew-t plotting needs dewpoint (dp) field, but dp not in D-file.
  should we calculate and insert it, or see if derivations can handle it
  automagically?

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
#include <string>
#include <iostream.h>
#include <fstream.h>
#include <strstream.h>

// using std::string;

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

RCSID("$Id: glass_ingest.cxx,v 2.7 1999-11-22 20:37:39 granger Exp $")

#include "ZTime.hh"
#include "FieldClass.h"


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
		AppException (strerror (::errno)),
		m_errno (::errno)
	{
		m_msg += ": ";
		m_msg += msg;
	}

	LibraryException () :
		AppException (strerror (::errno)),
		m_errno (::errno)
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
DefineField(F_dewpoint, "dp", FT_DP, "degC", "dewpoint")
DefineField(F_rh, "rh", FT_RH, "%", "relative humidity")
DefineField(F_u, "u_wind", FT_UWind, "m/s", "u wind component")
DefineField(F_v, "v_wind", FT_VWind, "m/s", "v wind component")
DefineField(F_wspd, "wspd", FT_WSpd, "m/s", "wind speed")
DefineField(F_wdir, "wdir", FT_WDir, "degrees", "wind direction")
DefineField(F_ascent, "ascent", "", "m/s", "ascent rate")
DefineField(F_dz, "dz", "", "m/s", "ascent rate")
DefineField(F_lon, "lon", "", "degrees_north", "longitude")
DefineField(F_lat, "lat", "", "degrees_east", "latitude")
DefineField(F_range, "range", "", "km", "range from launch site")
DefineField(F_az, "az", "", "degrees", "azimuth from launch site")
DefineField(F_alt, "alt", "", "m", "altitude above MSL")
DefineField(F_qpres, "qpres", "", "hPa", "pressure quality")
DefineField(F_qtemp, "qtemp", "", "degC", "temperature quality")
DefineField(F_qrh, "qrh", "", "%", "humidity quality")
DefineField(F_qu, "qu", "", "m/s", "u wind quality")
DefineField(F_qv, "qv", "", "m/s", "v wind quality")
DefineField(F_qwind, "qwind", "", "m/s", "wind quality")
DefineField(F_sa, "sa", "", "1", "GPS satellites")

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
	F_ascent ascent;
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
		e << wspd << wdir << ascent;
		e << lon << lat << range << az << alt;
		e << qpres << qtemp << qrh << qu << qv << qwind;
	}
};



struct GlassFileRecord 
{
	F_tdelta tdelta;
	F_pres pres;
	F_temp temp;
	// F_dp dp;
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
		e << wdir <<wspd << dz;
		e << lon << lat << alt;
		e << sa;
	}
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


#ifdef notdef
struct _ClassField
{
    char *name;
    char *type;		/* Zebra generic field type */
    char *units;
    char *description;
} ClassFields[] = 
{
    { "tdelta", "", "s", "time since launch" },
    { "pres", "P", "hPa", "pressure" },
    { "temp", "T", "degC", "temperature" },
    { "dp", "dp", "degC", "dewpoint" },
    { "rh", "rh", "%", "relative humidity" },
    { "u_wind", "uwind", "m/s", "u wind component" },
    { "v_wind", "vwind", "m/s", "v wind component" },
    { "wspd", "wspd", "m/s", "wind speed" },
    { "wdir", "wdir", "deg", "wind direction" },
    { "ascent", "", "m/s", "ascent rate" },
    { "dz", "", "m/s", "ascent rate" },
    { "lon", "", "deg", "longitude" },
    { "lat", "", "deg", "latitude" },
    { "range", "", "km", "range from launch site" },
    { "az", "", "deg", "azimuth from launch site" },
    { "alt", "", "m", "altitude above MSL" },
    { "qpres", "", "hPa", "pressure quality" },
    { "qtemp", "", "degC", "temperature quality" },
    { "qrh", "", "%", "humidity quality" },
    { "qu", "", "m/s", "u wind quality" },
    { "qv", "", "m/s", "v wind quality" },
    { "qwind", "", "m/s", "wind quality" }
};

int NClassFields = sizeof (ClassFields) / sizeof (struct _ClassField);
#endif


static void	ReadHeader (DataChunk *dc, char *file, Sounding &snd);
static void	ReadSamples (DataChunk *dc, char *file, Sounding &snd);
static void	SetPlatform (DataChunk *dc, char PlatformName[]);
static void 	GetPlatformName (const Sounding &snd, char plat[]);
static void	BuildTranslationTable (const char *tfilename);
static void	ParseCommandLineOptions (int *argc, char *argv[]);
static char *	GetNextString (char *, char*);
extern "C"	void Usage(char *prog_name);

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
 * Pressure quality threshold (1.5 by default, but can be changed by
 * command line option)
 */
static float	QualThresh = 1.5;


/*
 * Global debugging flags set from command line
 */
char JustShowFields = (char)0;	/* Initially false */
char DumpDataChunk = (char)0;   /* Dump chunks AS BUILT rather than
				 * like ingest.c option which is
				 * WHEN STORED */
char PlatformName[256] = "";
Sounding::RecordType SoundingType = Sounding::GLASS;

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


static void 
GlassIngest (int argc, char *argv[])
{
	char 	*filename;	/* Name of the snding file, pts to argv[1] */
	FieldId fields[MAX_FIELDS];
				/* The FieldId's of each field which the
				 * user has specified on the cmd-line */
	int nfields;		/* The number of fields to be stored */
	//	ZebTime *times;		/* Times for ea. sample in the s'nding file */
	//	ZebTime temp;
	//	int nsamples;		/* Number of samples, or pts, in the file */
	DataChunk *Dchunk;   	/* The DataChunk we'll be building */
	// bool reverse;		/* Reverse samples? */
	// int i;
	// static struct ui_command end_cmd = { UTT_END };
	// static char ctime[40];
/*
 * Get our command-line options, setting appropriate global variables
 * Only the file name and the names of the fields should remain
 */
	ParseCommandLineOptions(&argc, argv);
	Sounding snd(SoundingType);
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
 * Create a new data chunk, get the field list from the command line
 * and set up these fields in the Dchunk, including our bad_value_flag
 */
	Dchunk = dc_CreateDC(DCC_Scalar);

	// ParseFieldNames(argc, argv, fields, &nfields);
	// if (nfields == 0)
	//	throw AppException ("No valid fields specified.");
	// Collect the record fields into a list for the datachunk setup.
	CollectFields cf (&nfields, fields);
	snd.enumerate (cf);
	// Add the dewpoint field which we'll derive
	fields[nfields++] = F_dewpoint::fieldId();
	dc_SetScalarFields(Dchunk, nfields, fields);
	IngestLog(EF_DEBUG,"%d scalar fields set in datachunk",nfields);
	dc_SetBadval(Dchunk, BADVAL);
	IngestLog(EF_DEBUG,"bad_value_flag set to %6.2f",BADVAL);
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

	if (IngestLogFlags & (EF_DEBUG | EF_INFO)) 
	{
		//snd_show(&end_cmd);
	}

	SetPlatform (Dchunk, PlatformName);

	if (DumpDataChunk)
	{
		IngestLog (EF_DEBUG, "Dumping data chunks to stdout");
		dc_DumpDC (Dchunk);
	}

#ifdef notdef
/*
 * Get the times and locations
 */
	times = GetTimes (&nsamples);
	reverse = TC_Less (times[nsamples-1], times[0]);
	if (reverse)
	{
		for (i = 0; i < nsamples / 2; i++)
		{
			temp = times[i];
			times[i] = times[nsamples - i - 1];
			times[nsamples - i - 1] = temp;
		}
	}

	TC_EncodeTime(times, TC_Full, ctime);
	IngestLog(EF_INFO, "%s: %d samples found, starting at %s", 
		      plat, nsamples, ctime);
#endif

/*
 * Now just read the sounding data into the datachunk.
 */
	ReadSamples (Dchunk, filename, snd);

/*
 * Load the data for each field into the data chunk 
 */
	// LoadFieldData(Dchunk, times, nsamples, fields, nfields, reverse);
	IngestLog(EF_DEBUG,"%s: each field has been loaded", PlatformName);
	if (DumpDataChunk) dc_DumpDC(Dchunk);

/*
 * Set the locations for each sample in the data chunk
 */
	// SetLocations(Dchunk, nsamples);
	//IngestLog(EF_DEBUG, "%s: Locations set for each sample",plat);

/*
 * Send everything to the data store
 */
	IngestLog(EF_DEBUG,"%s: Sending data to DataStore", PlatformName);
	if (!ds_StoreBlocks (Dchunk, /*newfile*/ TRUE, (dsDetail *)0, 0))
	{
		IngestLog(EF_EMERGENCY,"%s: Data store failed", PlatformName);
	}
	else
		IngestLog(EF_INFO,
		   "%s: CLASS data loaded into DataStore", PlatformName);
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
			ds_SetMaxSample (pc, 10000);
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
	IngestParseOptions(argc, argv, (void (*)(...))Usage);

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
		else if (streq(argv[i],"-fields"))
		{
			JustShowFields = (char)1;
			IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (streq(argv[i],"-glass"))
		{
			SoundingType = Sounding::GLASS;
			IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (streq(argv[i],"-class"))
		{
			SoundingType = Sounding::CLASS;
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
		else if (! strncmp (argv[i], "-q", 2))
		{
			QualThresh = atof (argv[i+1]);
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
	printf ("   -fields		List the sounding fields\n");
	printf ("   -trans <tfile>	Use the site/platform translations in 'tfile'\n");
	printf ("   -platform <name>	Explicitly set the platform name\n");
//	printf ("   -q <qval>		Set pressure quality threshold\n");
	printf ("   -glass		Specify GLASS mode\n");
	printf ("   -class		Specify CLASS mode\n");
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
	ifstream fin (file);
	if (! fin)
	{
		throw LibraryException (file);
	}
	snd.tlaunch = 0;

	/* Nothing graceful here.  Just read lines until we reach
	   a line without a semicolon.  Everything before the semicolon
	   becomes an attribute, everything after is the value.
	   Check for specific attributes for the location and launch time.
	*/
	   
	string line;
	string::size_type n;
	int lino = 0;
	char attr[16];
	while (getline (fin, line) && (n = line.find (':')) != string::npos)
	{
		++lino;
		trim (line);
		// Extract name and value from the line.
		string left(line, 0, n);
		string right(line, n+1, line.length());
		trim (left);
		trim (right);
		IngestLog (EF_DEBUG, "'%s': '%s'", 
			   left.c_str(), right.c_str());

		// Just store the entire line as an attribute, since netcdf
		// will not accept the unconventional characters in the
		// header labels.
		sprintf (attr, "header%02d", lino);
		dc_SetGlobalAttr (dc, attr, const_cast<char *>(line.c_str()));
		// Now check for expected attributes.
		int year, mon, day, hour, min, sec;
		if (left.find("GMT Launch Time") != string::npos)
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
			// Found the site location.
			snd.location = right;
		}

		if (left.find ("Launch Site") != string::npos)
		{
			snd.site = right;
		}
	}

	// If we didn't get a launch time, we've got problems.
	if (snd.tlaunch == 0)
	{
		throw AppException ("Could not find launch time in header");
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
			dc_AddScalar (dc, &when, sample, 
				      f.fieldId(), &f.value());
		}
		return *this;
	}
	DataChunk *dc;
	ZTime when;
	int sample;
};


/*
 * Read the samples from the sounding file into the datachunk,
 * skipping samples with bad times, setting all fields to bad values
 * when important fields are bad.  Throw an exception if anything
 * unexpected happens.
 */
static void
ReadSamples (DataChunk *dc, char *file, Sounding &snd)
{
	ifstream fin (file);
	if (! fin)
	{
		throw LibraryException (file);
	}
	
	// Give a guess about how many samples in a typical sounding
	// to avoid some memory thrashing.
	dc_HintNSamples (dc, 5000, /*decrease*/ 0);
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
	ZTime when = 0;
	while (getline (fin, line))
	{
		++lino;
		istrstream ss(line.c_str());
		ReadRecord rr(ss);
		try {
			snd.enumerate (rr);
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

		// Now we should have the fields we need to insert a sample.
		when = snd.tlaunch;
		when += snd.tdelta();

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

		Location loc;
		loc.l_lat = snd.lat();
		loc.l_lon = snd.lon();
		loc.l_alt = snd.alt();
		if (loc.l_alt != BADVAL)
			loc.l_alt /= 1000.0;	// convert to km
		int sample = dc_GetNSample(dc);
		AddRecord estore(dc, when, sample);
		snd.enumerate (estore);
		dc_SetLoc (dc, sample, &loc);

/* From Chris Burghart:

   From relative humidity (%) and temperature (K!), you can get to
   dewpoint (also K) using two functions:

   dp = dewpoint (0.01 * rh * e_sw (t)); */

		F_dewpoint dp = BADVAL;
		if (snd.rh() != BADVAL && snd.temp() != BADVAL)
		{
			dp = dewpoint (0.01 * snd.rh() * 
				       e_sw(snd.temp() + 273.15));
			dp -= 273.15;
		}
		dc_AddScalar (dc, &when, sample, dp.fieldId(), &dp.value());

#ifdef notdef
		cout << when << endl;
		rec.enumerate (PrintRecord(cout));
		cout << endl;
#endif

#ifdef notdef
		int n = sscanf (line.c_str(), 
				"%f %f %f %f %f %f %f %f %f %f %i",
				&t, &pres, &temp, &rh, &wdir, &wspd, &dz,
				&lon, &lat, &alt, &sa);
		if (n != 11)
		{
		}
#endif
	}
	IngestLog (EF_INFO,
		   "%d bad times or pressure points, %d good samples", 
		   badlines, dc_GetNSample(dc));

	// If we didn't get anything useful, throw up our hands.
	if (dc_GetNSample (dc) == 0)
	{
		throw AppException ("No good samples in file");
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
