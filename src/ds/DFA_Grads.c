/*
 * Input-only (for now) format driver module for the GRADS file format.
 */
# include <stdio.h>
# include <errno.h>
# include <stdlib.h>
# include <sys/types.h>
# include <fcntl.h>
# include <unistd.h>
# include <math.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include <byteorder.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "DataFormat.h"

RCSID ("$Id: DFA_Grads.c,v 3.17 2002-09-17 18:28:43 granger Exp $")


/*
 * Grads time increments need to be handled a little differently, depending
 * on what the increment value is.
 */
typedef enum
{
	IT_Seconds,		/* Increment by a fixed number of secs	*/
	IT_Months,		/* Increment by months			*/
	IT_Years		/* Increment by years			*/
} TIType;

/*
 * Open GRADS files have a tag that looks an awful lot like the following.
 *
 * Note that the control file is the "file name" that DFA knows about; we
 * hide the data file internally.  The control file is read when the file is
 * opened then forgotten about; data file will stay open as long as the file
 * is considered to be open.
 */
typedef struct _GradsTag
{
	int	gt_dfd;			/* FD for the data file		*/
	float	gt_badflag;		/* Bad value flag		*/
	RGrid	gt_rg;			/* RGrid structure for reg spacing */
	Location gt_origin;		/* Origin of the whole thing */
	float	*gt_xc;			/* discrete coords for LEVELS	*/
	float	*gt_yc;
	float	*gt_zc;
	ZebTime	gt_begin;		/* Initial time			*/
	int	gt_ntime;		/* Number of times		*/
	int	gt_tincr;		/* Time increment 		*/
	TIType	gt_itype;		/* Type increment type		*/
	ZebTime *gt_times;		/* Internal array of ZebTimes	*/
	int	gt_nfield;		/* How many fields do we have?	*/
	FieldId *gt_fids;		/* The field ID's		*/
	int	*gt_nlevels;		/* How many levels/field	*/
	int	gt_swap;		/* Data need to be swapped	*/
        zbool    gt_ismodel;             /* It is a Model platform       */
} GradsTag;


typedef struct _GradsOpenFile 
{
	OpenFile	open_file;
	GradsTag	grads_tag;
}
GradsOpenFile;

#define TAGP(ofp) (&((GradsOpenFile *)(ofp))->grads_tag)

static CO_Compat COCTable [] =
{
	{ OrgNSpace,		DCC_NSpace	},
	{ Org2dGrid,		DCC_NSpace	},
	{ Org3dGrid,		DCC_NSpace	}
};

/*
 * Grads format methods.
 */
P_OpenFile (dgr_OpenFile);
P_CloseFile (dgr_CloseFile);
P_QueryTime (dgr_QueryTime);
P_GetData (dgr_GetData);
P_GetObsSamples (dgr_GetObsSamples);
P_GetFields (dgr_GetFields);
P_Setup (dgr_Setup);
P_GetAlts (dgr_GetAlts);
P_GetTimes (dgr_GetTimes);
P_GetAssociatedFiles (dgr_GetAssociatedFiles);


/*
 * Grads_model format methods.
 */
P_QueryTime (dgr_ModelQueryTime);
P_GetForecastTimes (dgr_GetForecastTimes);

/*
 * GRADS.
 */
static DataFormat gradsFormatRec =
{
	"GRADS",
	FTGrads,
	".ctl",				/* hope ".ctl" covers it 	*/

	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof (GradsOpenFile),
	TRUE,				/* read-only			*/

	FORMAT_INIT,			/* dynamic class members	*/

	dgr_QueryTime,			/* Query times			*/
	___,				/* Make file name		*/

	dgr_Setup,			/* setup			*/
	dgr_OpenFile,			/* Open				*/
	dgr_CloseFile,			/* Close			*/
	___,				/* Synchronize			*/
	dgr_GetData,			/* Get the data			*/
	dgr_GetAlts,			/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	___,            		/* Get forecast times		*/
	___,				/* Create a new file		*/
	___,				/* Write to file		*/
	___,				/* Write block to a file	*/
	dgr_GetObsSamples,		/* Get observation samples	*/
	dgr_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
	dgr_GetTimes,			/* Get times			*/
	dgr_GetAssociatedFiles          /* Get associated files         */
};


DataFormat *gradsFormat = (DataFormat *) &gradsFormatRec;

/*
 * GRADS_MODEL.
 */
static DataFormat gradsmodelFormatRec =
{
	"GRADSModel",
	FTGradsModel,
	".ctl",				/* hope ".ctl" covers it 	*/

	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof (GradsOpenFile),
	TRUE,				/* read-only			*/

	FORMAT_INIT,			/* dynamic class members	*/

	dgr_ModelQueryTime,		/* Query times			*/
	___,				/* Make file name		*/

	dgr_Setup,			/* setup			*/
	dgr_OpenFile,			/* Open				*/
	dgr_CloseFile,			/* Close			*/
	___,				/* Synchronize			*/
	dgr_GetData,      		/* Get the data			*/
	dgr_GetAlts,			/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	dgr_GetForecastTimes,		/* Get forecast times		*/
	___,				/* Create a new file		*/
	___,				/* Write to file		*/
	___,				/* Write block to a file	*/
	dgr_GetObsSamples,		/* Get observation samples	*/
	dgr_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
	dgr_GetTimes,	       	        /* Get times			*/
	dgr_GetAssociatedFiles          /* Get associated files         */
};


DataFormat *gradsmodelFormat = (DataFormat *) &gradsmodelFormatRec;

/*
 * Forwards.
 */
static void	dgr_CleanTag FP ((GradsTag *));
static int	dgr_DoOpen FP ((GradsTag *tag, const char *, int));
static void	dgr_HandleDDef FP ((GradsTag *, char **, int, FILE *));
static void	dgr_HandleTDef FP ((GradsTag *, char **, int));
static int	dgr_GetCtlLine FP ((FILE *, char **, int *));
static void	dgr_HandleVars FP ((GradsTag *, int, FILE *));
static int	dgr_OpenData FP ((GradsTag *, const char *, const char *));
static void	dgr_CalcTime FP ((GradsTag *, int, ZebTime *));
static void	dgr_SetTimes FP ((GradsTag *tag));
static int	dgr_FindIndex FP ((GradsTag *, FieldId));
static off_t	dgr_FOffset FP ((GradsTag *, int, int, int));
static float	dgr_Swap FP ((float *));
static void	dgr_SwapFloats FP ((float *, int));


/*
 * OK, time to get into some real code.
 */

static int
dgr_DoOpen (tag, file, opendata)
GradsTag *tag;
const char *file;
zbool opendata;
/*
 * Actually open the given file; only open the associated data file if
 * OPENDATA is TRUE.  Returns non-zero iff the open succeeds.
 */
{
	FILE *cfile;
	char *cfwords[32];
	int nw;
/*
 * Try opening up our control file.
 */
	if ((cfile = fopen (file, "r")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Error opening %s, %d", file, errno);
		return (0);
	}
/*
 * Now we blast through it and see what we get.
 */
	memset (tag, 0, sizeof (GradsTag));
	tag->gt_dfd = -1;

	/* Make sure there is a bad value set */
	tag->gt_badflag=(float) CFG_DC_DEFAULT_BADVAL;

	while (dgr_GetCtlLine (cfile, cfwords, &nw))
	{
		char fc = cfwords[0][0];
	/*
	 * See what this line is.  Life would be more efficient with a
	 * symbol table here, but so it goes.
	 */
		if (fc == '*' || ! strcasecmp (cfwords[0], "TITLE"))
			continue; /* Ignore comments and titles */
		else if (! strcasecmp (cfwords[0], "UNDEF"))
			tag->gt_badflag = atof (cfwords[1]);
		else if (fc >= 'X' && fc <= 'Z')
			dgr_HandleDDef (tag, cfwords, nw, cfile);
		else if (! strcasecmp (cfwords[0], "TDEF"))
			dgr_HandleTDef (tag, cfwords, nw);
		else if (! strcasecmp (cfwords[0], "VARS"))
			dgr_HandleVars (tag, atoi (cfwords[1]), cfile);
		else if (! strcasecmp (cfwords[0], "DSET"))
		{
			if (opendata && ! dgr_OpenData (tag, cfwords[1], file))
			{
				dgr_CleanTag (tag);
				fclose (cfile);
				return (0);
			}
		}
		else
			msg_ELog (EF_PROBLEM, "Unrecogized ctl word: %s",
					cfwords[0]);
	}
	fclose (cfile);
	return (1);
}





static void
dgr_HandleDDef (tag, words, nw, fp)
GradsTag *tag;
char **words;
int nw;
FILE *fp;
/*
 * Handle a dimension definition.
 */
{
	int np = atoi (words[1]), level, word;
	float base, spacing, *levels;
	char which = words[0][0]; /* Save it now */
/*
 * Figure out if we're dealing with linear or discrete levels here.
 */
	levels = (float *) malloc (np * sizeof (float));
	if (! strcasecmp (words[2], "LINEAR"))
	{
	/*
	 * Linear.  Just figure the increments and we're set.
	 */
		base = atof (words[3]);
		sscanf (words[4], "%f", &spacing);
		for (level = 0; level < np; level++)
		{
			levels[level] = base + level*spacing;
			if (words[0][0] == 'X' && levels[level] > 180.0)
				levels[level] -= 360.0;
		}
	}
/*
 * OK, we better have levels.  Cope.
 */
	else if (strcasecmp (words[2], "LEVELS"))
	{
		msg_ELog (EF_PROBLEM, "Funky %s: %s", words[0], words[2]);
		return;
	}
	else
	{
	/*
	 * Pull in the levels.
	 */
		word = 3;
		for (level = 0; level < np; level++)
		{
		/*
		 * See if we need to move to a new line, then get the
		 * next level.
		 */
			if (word >= nw)
			{
				dgr_GetCtlLine (fp, words, &nw);
				word = 0;
			}
			levels[level] = atof (words[word++]);
		}
		spacing = levels[1] - levels[0];
		if (spacing < 0)
			spacing = -spacing;
	}
/*
 * Now we just have to find a place to store all this in the tag.
 */
	switch (which)
	{
	    case 'X':
		tag->gt_rg.rg_nX = np;
		tag->gt_xc = levels;
		tag->gt_origin.l_lon = levels[0];
		tag->gt_rg.rg_Xspacing = spacing;
		break;

	    case 'Y':
		tag->gt_rg.rg_nY = np;
		tag->gt_yc = levels;
		tag->gt_origin.l_lat = levels[0];
		tag->gt_rg.rg_Yspacing = spacing;
		break;
		
	    case 'Z':
		tag->gt_rg.rg_nZ = np;
		tag->gt_zc = levels;
		tag->gt_origin.l_alt = levels[0];
		tag->gt_rg.rg_Zspacing = spacing;
		break;
	}
}






static int
dgr_GetCtlLine (fp, cfwords, nw)
FILE *fp;
char **cfwords;
int *nw;
/*
 * Read in a control line and parse it up into words.
 */
{
	static char cfline[100];	/* should be <= 80		*/
	int i;
/*
 * Read in the stuff.
 */
	if (! fgets (cfline, 100, fp))
		return (FALSE);
/*
 * Parsing kludge.  First go through and replace tabs with blanks,
 * then we can parse on blanks with confidence.
 */
	for (i = 0; cfline[i]; i++)
		if (cfline[i] == '\t' || cfline[i] == '\n')
			cfline[i] = ' ';
	*nw = ParseLine (cfline, cfwords, ' ');
	return (TRUE);
}




static void
dgr_CleanTag (tag)
GradsTag *tag;
/*
 * Get rid of this tag.
 */
{
	if (tag->gt_xc)
		free (tag->gt_xc);
	if (tag->gt_yc)
		free (tag->gt_yc);
	if (tag->gt_zc)
		free (tag->gt_zc);
	if (tag->gt_fids)
		free (tag->gt_fids);
	if (tag->gt_times)
		free (tag->gt_times);
	/* free (tag); */
}



static char *Months[] = { "jan", "feb", "mar", "apr", "may", "jun",
			  "jul", "aug", "sep", "oct", "nov", "dec" };


static void
dgr_HandleTDef (tag, words, nw)
GradsTag *tag;
char **words;
int nw;
/*
 * Try to make sense out of a funky GRADS time definition line.
 *
 * The format is:
 *
 *	hh:mmZddmmmyyyy
 *
 * where everything but the month and year are optional.  Sigh.
 */
{
	char *cp = words[3];
	int year, month, day = 1, hour = 0, minute = 0;

	tag->gt_ntime = atoi (words[1]);
/*
 * Look for hours.
 */
	if (cp[2] == 'z' || cp[2] == 'Z' || cp[2] == ':') /* hours present */
	{
		hour = (cp[0] - '0')*10 + cp[1] - '0';
		cp += 2;
	}
/*
 * If we are looking at a colon now, we have minutes too.
 */
	if (*cp == ':')
	{
		minute = (cp[1] - '0')*10 + cp[2] - '0';
		cp += 3;
	}
/*
 * If we have a Z skip it then move into the date portion.
 */
	if (*cp == 'Z' || *cp == 'z')
		cp++;
/*
 * Is this a number?  Then it's gotta be a date.
 */
	if (*cp >= '0' && *cp <= '9')
	{
		day = (cp[0] - '0')*10 + cp[1] - '0';
		cp += 2;
	}
/*
 * Now we really need to have a date.
 */
	for (month = 0; month < 12; month++)
		if (! strncasecmp (cp, Months[month], 3))
			break;
	month++;
/*
 * Finally, there needs to be a year.
 */
	year = atoi (cp + 3);
	TC_ZtAssemble (&tag->gt_begin, year, month, day, hour, minute, 0, 0);
/*
 * Whew.  But, tragically, we are not done yet.  Now we get to figure out
 * how the hell they plan to increment times.  The format here is:
 *
 *	nnKW
 *
 * where KW is in { mn, hr, dy, mo, yr }.  They sez that nn should be two
 * digits, but experience shows otherwise.
 */
	cp = words[4];
	tag->gt_tincr = cp[0] - '0';
	if (cp[1] >= '0' && cp[1] <= '9')
		tag->gt_tincr = tag->gt_tincr*10 + *++cp - '0';
	cp++;
	if (! strcasecmp (cp, "mn"))
	{
		tag->gt_tincr *= 60;
		tag->gt_itype = IT_Seconds;
	}
	if (! strcasecmp (cp, "hr"))
	{
		tag->gt_tincr *= 3600;
		tag->gt_itype = IT_Seconds;
	}
	if (! strcasecmp (cp, "dy"))
	{
		tag->gt_tincr *= 3600*24;
		tag->gt_itype = IT_Seconds;
	}
	if (! strcasecmp (cp, "mo"))
		tag->gt_itype = IT_Months;
	if (! strcasecmp (cp, "yr"))
		tag->gt_itype = IT_Years;
}





static void
dgr_HandleVars (tag, nv, fp)
GradsTag *tag;
int nv;
FILE *fp;
/*
 * Get the variable info from the file.
 */
{
	char dstring[80], *words[32];
	int v, nw, w;
/*
 * Get a FieldId array to work with, then plow through the variable list.
 */
	tag->gt_fids = (FieldId *) malloc (nv*sizeof (FieldId));
	tag->gt_nlevels = (int *) malloc (nv*sizeof (int));
	for (v = 0; v < nv; v++)
	{
	/*
	 * Get a line.  Then we need to reassemble the description from
	 * the parsed remnants.
	 */
		dgr_GetCtlLine (fp, words, &nw);
		strcpy (dstring, words[3]);
		for (w = 4; w < nw; w++)
		{
			strcat (dstring, " ");
			strcat (dstring, words[w]);
		}
	/*
	 * Assign values.
	 */
		tag->gt_fids[v] = F_DeclareField (words[0], dstring,"unknown");
		if ((tag->gt_nlevels[v] = atoi (words[1])) == 0) /* sfc lvl */
			tag->gt_nlevels[v] = 1;	/* kludge */
	}
/*
 * Get the endvars line and quit.
 */
	tag->gt_nfield = nv;
	dgr_GetCtlLine (fp, words, &nw);
	if (strcasecmp (words[0], "endvars"))
		msg_ELog (EF_PROBLEM, "Found %s instead of endvars", words[0]);
}




static int
dgr_OpenData (tag, dfname, cfname)
GradsTag *tag;
const char *dfname, *cfname;
/*
 * Open the data file associated with this control file.
 */
{
	char realname[120], *slash;
	float data[10];
	int i;
/*
 * Grads convention allows the data file name to start with "^", meaning
 * look in the same directory as the control file.
 */
	if (dfname[0] == '^')
	{
		strcpy (realname, cfname);
		if ((slash = strrchr (realname, '/')))
			slash++;
		else
			slash = realname;
		strcpy (slash, dfname + 1);
	}
	else
		strcpy (realname, dfname);
/*
 * Now just try to open it.
 */
	if ((tag->gt_dfd = open (realname, O_RDONLY)) < 0)
		return (0);
/*
 * Try to figure out which byte ordering is in use here.  It can be either,
 * as it turns out, with no real indication of what's going on.
 */
	if (read (tag->gt_dfd, data, 10*sizeof (float)) < 10*sizeof (float))
		return (0);		/* No data, who cares about order? */
	tag->gt_swap = 0;
	for (i = 0; i < 10; i++)
		if (fabs (data[i]) > 1e10 && dgr_Swap (data + i) < 1e10)
			tag->gt_swap = 1;

	return (1);
}





/* ARGSUSED */
static int
dgr_OpenFile (of, write)
OpenFile *of;
zbool write;
/*
 * The DFA open routine.
 */
{
	GradsTag *tag = TAGP(of);
	char *fname = of->of_df.df_fullname;
/*
 * Just open the file.
 */
	if (dgr_DoOpen (tag, fname, TRUE))
	{
		dgr_SetTimes (tag);
		/* Set this field for model platorms */
		if (ds_IsModelPlatform (of->of_df.df_pid)) 
		    tag->gt_ismodel=TRUE;
		else tag->gt_ismodel=FALSE;
		return (TRUE);
	}
	else
	{
		dgr_CleanTag (tag);
		return (FALSE);
	}
}





static int
dgr_QueryTime (const char *file, ZebraTime *begin, ZebraTime *end, 
	       int *nsample)
/*
 * Query times in this file.
 */
{
	GradsTag *tag;
	int ret;

	tag = (GradsTag *) malloc (sizeof(GradsTag));
	ret = dgr_DoOpen (tag, file, FALSE);
	if (ret)
	{
		*begin = tag->gt_begin;
		*nsample = tag->gt_ntime;
		dgr_CalcTime (tag, tag->gt_ntime - 1, end);
	}
	dgr_CleanTag (tag);
	free (tag);
	return (ret);
}

static int
dgr_ModelQueryTime (const char *file, ZebraTime *begin, ZebraTime *end, 
		    int *nsample)
/*
 * Query times in this file.
 */
{
	GradsTag *tag;
	int ret;

	tag = (GradsTag *) malloc (sizeof(GradsTag));
	ret = dgr_DoOpen (tag, file, FALSE);
	if (ret)
	{
		*begin = tag->gt_begin;
		*nsample = 1;
		*end=*begin;
	
	}
	dgr_CleanTag (tag);
	free (tag);
	return (ret);
}




static void
dgr_CalcTime (tag, sample, zt)
GradsTag *tag;
int sample;
ZebTime *zt;
/*
 * Figure out what time the given sample is at.
 */
{
	int year, month, day, hour, minute;
	
	*zt = tag->gt_begin;
	switch (tag->gt_itype)
	{
	    case IT_Seconds:
		zt->zt_Sec += tag->gt_tincr*sample;
		break;

	    case IT_Months:
		TC_ZtSplit (zt, &year, &month, &day, &hour, &minute, 0, 0);
		month += tag->gt_tincr*sample;
		if (month > 12)
		{
			year++;
			month -= 12;
		}
		TC_ZtAssemble (zt, year, month, day, hour, minute, 0, 0);
		break;

	    case IT_Years:
		TC_ZtSplit (zt, &year, &month, &day, &hour, &minute, 0, 0);
		year += tag->gt_tincr*sample;
		TC_ZtAssemble (zt, year, month, day, hour, minute, 0, 0);
		break;
	}
}




static void
dgr_SetTimes (tag)
GradsTag *tag;
/*
 * Calculate our private array of ZebTimes for each sample according
 * to the regular interval.
 */
{
	int i;

	tag->gt_times = (ZebTime *) malloc (tag->gt_ntime * sizeof(ZebTime));
	for (i = 0; i < tag->gt_ntime; ++i)
		dgr_CalcTime (tag, i, tag->gt_times+i);
}




static void
dgr_CloseFile (ofp)
OpenFile *ofp;
/*
 * Close this file.
 */
{
	GradsTag *tag = TAGP(ofp);

	if (tag->gt_dfd >= 0)
		close (tag->gt_dfd);
	dgr_CleanTag (tag);
}




static DataChunk *
dgr_Setup (ofp, fields, nfield, class)
OpenFile *ofp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * The DFA setup routine.
 */
{
	GradsTag *tag = TAGP(ofp);
	DataChunk *dc;
	FieldId dims[3];
	int f, findex = -1;
/*
 * Find a field that we actually have; we will use that to set the
 * number of levels.
 */
	for (f = 0; f < nfield; f++)
		if ((findex = dgr_FindIndex (tag, fields[0])) >= 0)
			break;
	if (findex < 0)
		return (NULL);
/*
 * Make our data chunk.
 */
	dc = dc_CreateDC (DCC_NSpace);
/*
 * Define three coordinate variables for our three dimensions: lat, lon, alt.
 */
	dims[0] = F_Lookup ("alt");
	dc_NSDefineDimension (dc, dims[0], tag->gt_nlevels[findex]);
	dc_NSDefineVariable (dc, dims[0], 1, dims, TRUE);
	dc_SetLocAltUnits (dc, tag->gt_nlevels[findex] > 1 ? AU_mb : AU_mAGL);

	dims[1] = F_Lookup ("lat");
	dc_NSDefineDimension (dc, dims[1], tag->gt_rg.rg_nY);
	dc_NSDefineVariable (dc, dims[1], 1, dims + 1, TRUE);
	
	dims[2] = F_Lookup ("lon");
	dc_NSDefineDimension (dc, dims[2], tag->gt_rg.rg_nX);
	dc_NSDefineVariable (dc, dims[2], 1, dims + 2, TRUE);
/*
 * Now define the user's fields using these dimensions
 */
	for (f = 0; f < nfield; f++)
		dc_NSDefineVariable (dc, fields[f], 3, dims, FALSE);
	dc_NSAllowRedefine (dc, TRUE);
	return (dc);
}





static int
dgr_FindIndex (tag, fid)
GradsTag *tag;
FieldId fid;
/*
 * Find the index of this field.
 */
{
	int index;

	for (index = 0; index < tag->gt_nfield; index++)
		if (tag->gt_fids[index] == fid)
			return (index);
	msg_ELog (EF_PROBLEM, "dgr_FindIndex: Field %s (%d) not found!", 
		  F_GetFullName(fid), fid);
	return (-1);
}





static int
dgr_GetData (ofp, dc, begin, nsample, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int begin;
int nsample;
dsDetail *details;
int ndetail;
/*
 * The DFA get data routine.
 */
{
	GradsTag *tag = TAGP(ofp);
	FieldId lat, lon, alt, fids[10];
	int nfield, sample, field, index, lvlsize, level = -1, i;
	off_t offset;
	float *grid, alttarget;
	SValue v;
	int start;	/* where to start adding samples to dc */
	int forecast_offset; /* Stores the forecast offset */ 
	int fileoffset;      /* will store the forecast time offset in file */
/*
 * Find the dimensions first.
 */
	lon = F_Lookup ("lon");
	lat = F_Lookup ("lat");
	alt = F_Lookup ("alt");
/*
 * See if we need to split off a level.
 */
	if (ds_GetDetail ("altitude", details, ndetail, &v))
	{
		float diff, bestdiff = 999999;
		alttarget = v.us_v_float;
		level = 0; /* -1 screws us up if no levels */
		for (i = 0; i < tag->gt_rg.rg_nZ; i++)
		{
			diff = fabs (alttarget - tag->gt_zc[i]);
			if (diff < bestdiff)
			{
				bestdiff = diff;
				level = i;
			}
		}
		alttarget = tag->gt_zc[level];
		msg_ELog (EF_DEBUG, "Get alt %.2f, lvl %d", alttarget, level);
	}
/*
 * See if we need to finish tweaking the data chunk.
 */
	if (! dc_NSDefineIsComplete (dc))
	{
		if (level >= 0)
			dc_NSDefineDimension (dc, alt, 1);
		dc_NSDefineComplete (dc);
	/*
	 * Store the dimension variables now.
	 */
		dc_NSAddStatic (dc, alt, tag->gt_zc + ((level > 0) ? level:0));
		dc_NSAddStatic (dc, lat, tag->gt_yc);
		dc_NSAddStatic (dc, lon, tag->gt_xc);
	}

/*
 * Set the badval in the data chunk, either based on the details we're given
 * or by using the default.
 */
	dc_SetBadval (dc, tag->gt_badflag);

/*
 * Now get the whole ugly list of variables out of the data chunk.
 */
	nfield = dc_NSGetAllVariables (dc, fids, NULL);
/*
 * Allocate a temporary grid to hold the data on its way in.
 */
	lvlsize = tag->gt_rg.rg_nX * tag->gt_rg.rg_nY * sizeof (float);
	grid = (float *) malloc (lvlsize*((level >= 0) ? 1: tag->gt_rg.rg_nZ));

	if (tag->gt_ismodel)
	{

/*
 * We are handling model platform data. We need to handle forecast times 
 * here. The forecast_offset is given in seconds here.
 */
          forecast_offset = ds_GetDetail (DD_FORECAST_OFFSET, 
		details, ndetail, &v) ? v.us_v_int : 0;

          if (tag->gt_itype!=IT_Seconds)
	  {
		msg_ELog (EF_PROBLEM, 
			"IT_Months not accepted for Model yet");
		sample=0;	
	  }
	  if (forecast_offset)
	  {
		fileoffset=tag->gt_tincr;
		for (i=1;i<tag->gt_ntime; i++) 
			if (fileoffset!=forecast_offset) 
				fileoffset+=tag->gt_tincr;
			else break;
		fileoffset=i;
	  } else fileoffset=0;


/*
 * Get the sample and store the data.
 */
	  start = dc_GetNSample (dc);
	  sample = fileoffset;

	/* dgr_CalcTime (tag, sample, &zt); */
	  for (field = 0; field < nfield; field++)
	  {
		int nlev;
	/*
	 * Blow off coord vars and fields we don't have
	 */
		if (fids[field] == lat || fids[field] == lon ||
			fids[field] == alt)
			continue;
		if ((index = dgr_FindIndex (tag, fids[field])) < 0)
			continue;
		nlev = (level >= 0) ? 1 : tag->gt_nlevels[index];
	/*
	 * Find the offset for this field, snarf it, and store.
	 */
		offset = dgr_FOffset (tag, sample, index,
			(level > 0) ? level : 0);

		lseek (tag->gt_dfd, offset, SEEK_SET);
		read (tag->gt_dfd, grid, lvlsize*nlev);
		if (tag->gt_swap)
			dgr_SwapFloats (grid,
				(lvlsize*nlev)/sizeof (float));
		dc_NSAddSample (dc, tag->gt_times+sample, 
				start, fids[field], grid);
	  }
	}
	else
	{
/*
 * We are looking for a non model platform. Time to blast through the 
 * samples and store the data.
 */
	  start = dc_GetNSample (dc);
	  for (sample = begin; sample < begin + nsample; sample++)
	  {
		/* dgr_CalcTime (tag, sample, &zt); */
		for (field = 0; field < nfield; field++)
		{
			int nlev;
		/*
		 * Blow off coord vars and fields we don't have
		 */
			if (fids[field] == lat || fids[field] == lon ||
					fids[field] == alt)
				continue;
			if ((index = dgr_FindIndex (tag, fids[field])) < 0)
				continue;
			nlev = (level >= 0) ? 1 : tag->gt_nlevels[index];
		/*
		 * Find the offset for this field, snarf it, and store.
		 */
			offset = dgr_FOffset (tag, sample, index,
				(level > 0) ? level : 0);

			lseek (tag->gt_dfd, offset, SEEK_SET);
			read (tag->gt_dfd, grid, lvlsize*nlev);
			if (tag->gt_swap)
				dgr_SwapFloats (grid,
						(lvlsize*nlev)/sizeof (float));
			dc_NSAddSample (dc, tag->gt_times+sample, 
					start+sample-begin, fids[field], grid);
		}
	  }
	}
	free (grid);
	return (TRUE);
}





static void
dgr_SwapFloats (grid, n)
float *grid;
int n;
/*
 * Swap up an array of floats.
 */
{
	for (; n > 0; n--)
	{
		*grid = dgr_Swap (grid);
		grid++;
	}
}



static float
dgr_Swap (float *v)
/*
 * Swap a floating point value.
 */
{
	float ret;
	char *cp = (char *) v, *r = (char *) &ret;

	r[0] = cp[3];
	r[1] = cp[2];
	r[2] = cp[1];
	r[3] = cp[0];
	return (ret);
}





static off_t
dgr_FOffset (tag, sample, index, level)
GradsTag *tag;
int sample, index, level;
/*
 * Return the offset in the data file for the given sample, field index
 * and level.
 */
{
	int levsize = tag->gt_rg.rg_nX*tag->gt_rg.rg_nY*sizeof (float);
	int i, soffset = 0, ssize = 0;
/*
 * Sanity checking on the level.
 */
	if (level < 0)
		level = 0;
	else if (level >= tag->gt_nlevels[index])
		level = tag->gt_nlevels[index] - 1;	/* Careful here */

/*
 * Figure the offset into a sample for this field.  Then if we want the
 * first sample we're done.
 */
	for (i = 0; i < index; i++)
		soffset += tag->gt_nlevels[i]*levsize;

/*
 * OK, we have to figure the size of an entire sample.
 */
	for (i = 0; i < tag->gt_nfield; i++)
		ssize += tag->gt_nlevels[i]*levsize;

	if (sample == 0) 
		return ((off_t) (soffset + level*levsize));

	return ((off_t) (sample*ssize + soffset + level*levsize));
}





static int
dgr_GetObsSamples (ofp, times, locs, max)
OpenFile *ofp;
ZebTime *times;
Location *locs;
int max;
/*
 * Tell about this observation.
 */
{
	GradsTag *tag = TAGP(ofp);
	int i;
/*
 * Give them as many as they have room for.
 */
	for (i = 0; i < tag->gt_ntime && i < max; i++)
	{
		/* dgr_CalcTime (tag, i, times++); */
		*times++ = tag->gt_times[i];
		*locs++ = tag->gt_origin;
	}
	return (i);
}




static int
dgr_GetFields (ofp, sample, nfld, fids)
OpenFile *ofp;
int sample;
int *nfld;
FieldId *fids;
/*
 * Return a list of available fields.
 */
{
	GradsTag *tag = TAGP(ofp);
/*
 * Just copy over the info and we're set.
 */
	memcpy (fids, tag->gt_fids, tag->gt_nfield*sizeof (FieldId));
	return (*nfld = tag->gt_nfield);
}





static int
dgr_GetAlts (ofp, fid, offset, alts, nalts, units)
OpenFile *ofp;
FieldId fid;
int offset;
float *alts;
int *nalts;
AltUnitType *units;
/*
 * The GetAlts DFA routine.
 */
{
	GradsTag *tag = TAGP(ofp);
	int index;

	if ((index = dgr_FindIndex (tag, fid)) < 0)
		return (FALSE);
/*
 * If this is a surface level return the appropriate information.
 */
	if (nalts)
		*nalts = tag->gt_nlevels[index];
	if (tag->gt_nlevels[index] == 1)
	{
		if (units)
			*units = AU_mAGL;
		if (alts)
			*alts = 0.0;
		return (TRUE);
	}
/*
 * OK copy info.
 */
	if (units)
		*units = AU_mb;
	if (alts)
		memcpy (alts, tag->gt_zc,
				tag->gt_nlevels[index]*sizeof (float));
	return (TRUE);
}




static ZebTime *
dgr_GetTimes (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	GradsTag *tag = TAGP(ofp);

	/* Different handling for model platforms */
	if (tag->gt_ismodel) *ntime=1;
	else *ntime = tag->gt_ntime;

	if (!tag->gt_times)
		dgr_SetTimes (tag);
	return (tag->gt_times);
}




static char **
dgr_GetAssociatedFiles (const DataFile *df, int *nfiles)
{
  /* Some pieces of code borrowed from dgr_DoOpen and dgr_OpenData. Thanks! */

  const char *cfname;
  char **filenames;
  char realname[120], *slash;
  FILE *cfile;
  char *cfwords[32];
  int nw;

  cfname = df->df_fullname;

/*
 * Try opening up our control file.
 */
        if ((cfile = fopen (cfname, "r")) == NULL)
        {
                msg_ELog (EF_PROBLEM, "Error opening %s, %d", cfname, errno);
                return (0);
        }
/*
 * Now we blast through it and look for the data file name.
 */
 
        while (dgr_GetCtlLine (cfile, cfwords, &nw))
               if (!strcasecmp (cfwords[0], "DSET")) break;

 	fclose (cfile);
/*
 * Grads convention allows the data file name to start with "^", meaning
 * look in the same directory as the control file.
 */
        if (cfwords[1][0] == '^')
        {
                strcpy (realname, cfname);
                if ((slash = strrchr (realname, '/')))
                        slash++;
                else
                        slash = realname;
                strcpy (slash, cfwords[1] + 1);
        }
        else
                strcpy (realname, cfwords[1]);

/* 
 * Allocate memory to store this file name and return. 
 */
        filenames = (char **) malloc ( 2*sizeof(char *) );

	filenames[0]=(char *) malloc (strlen(cfname)+1);
	sprintf (filenames[0],"%s",cfname);

        filenames[1]=(char *) malloc (strlen(realname)+1);
	sprintf (filenames[1],"%s", realname );

	*nfiles=2;
	return filenames;
}


int
dgr_GetForecastTimes (of, times, ntimes)
OpenFile *of;
int *times;
int *ntimes;
{
/*
 * Return an array of available forecast offset times (in seconds) for 
 * this file.
 */

	GradsTag   *tag = TAGP (of);
	int     count=0, i, offset, forecasttime=0;

	offset = tag->gt_tincr;
	for (i = 1; i < tag->gt_ntime; i++)
	{
		forecasttime+=offset;
		times[count++] = forecasttime;
	}

   if (ntimes)	 *ntimes = count;

   return (TRUE);
}


