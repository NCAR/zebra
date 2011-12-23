/*
 * Access to netCDF files.
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

# include <math.h>
# include <sys/time.h>
# include <string.h>
# include <ctype.h>

# include <defs.h>
# include <config.h>
# include <message.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "DataFormat.h"
# include "DFA_ncutil.c"

RCSID ("$Id: DFA_NetCDF.c,v 3.90 2005-08-17 16:59:42 burghart Exp $")

/*
 * Location fields: standard attributes
 */
#define DEF_ALT_UNITS 	CFG_ALTITUDE_UNITS
#define ALT_LONGNAME	"altitude"
#define LAT_UNITS	"degrees"
#define LAT_LONGNAME	"north latitude"
#define LON_UNITS	"degrees"
#define LON_LONGNAME	"east longitude"

/*
 * Grid organizations: standard attributes
 */
#define X_UNITS		"km"
#define Y_UNITS		"km"

#define X_LONGNAME	"grid spacing in west->east direction"
#define Y_LONGNAME	"grid spacing in south->north direction"
#define Z_LONGNAME	"grid spacing along vertical"

/*
 * This is our tag structure.
 */
typedef struct _nctag
{
	int             nc_id;		/* netCDF ID value		*/
        int             nc_base;	/* Base time value from the file*/
	int             nc_vTime;	/* Time variable ID		*/
	zbool		nc_haveTimeDim;	/* Is there a time dimension?	*/
	int             nc_dTime;	/* The time dimension ID	*/
	nc_type		nc_timeType;	/* Type of time or time_offset	*/
	ZebTime        *nc_times;	/* The time offset array	*/
	int             nc_ntime;	/* The number of time records	*/
	int             nc_nrec;	/* How many records in the file	*/
	DataOrganization nc_org;	/* The purported organization	*/
	Location       *nc_locs;	/* Location array (IRGRID)	*/
	Location        nc_sloc;	/* Static location		*/
	RGrid           nc_rgrid;	/* Regular grid info		*/
	long 		nc_nPlat;	/* Number of platforms		*/
	PlatformId      nc_plat;	/* The base platform ID		*/
	PlatformId     *nc_subplats;	/* IRGRID subplatform ID's	*/
	int	       *nc_subindex;	/* Subplatform index hash	*/
	int		nc_nVar;	/* Number of variables in the file */
	FieldId		*nc_FMap;	/* The field map		*/
	AltUnitType	nc_altUnits;	/* alt units for Locations and	*/
					/*     RGrid z spacing		*/
	float		*nc_alts;	/* Alts for 3dgrid and nspace	*/ 
	int 		nc_nalts;	/* Number of available altitudes*/
	FieldId		nc_altvar;	/* Varid of source of altitudes	*/
} NCTag;


/*
 * Our open file instance structure
 */
typedef struct _NetCDFOpenFile 
{
	OpenFile	open_file;
	NCTag		nc_tag;
} 
NetCDFOpenFile;

#define TAGP(ofp) (&((NetCDFOpenFile *)ofp)->nc_tag)

/*
 * NetCDF format methods
 */
P_QueryTime (dnc_QueryTime);
P_OpenFile (dnc_OpenFile);
P_CloseFile (dnc_CloseFile);
P_SyncFile (dnc_SyncFile);
P_GetData (dnc_GetData);
P_GetAlts (dnc_GetAlts);
P_CreateFile (dnc_CreateFile);
P_PutBlock (dnc_PutBlock);
P_GetFields (dnc_GetFields);
P_GetObsSamples (dnc_GetObsSamples);
P_Setup (dnc_Setup);
P_GetTimes (dnc_TimesArray);

/*
 * The class/organization compatibility table.  If the desired class
 * and the given file organization appear together here, we can do it.
 */
static CO_Compat COCTable [] =
{
	{ Org1dGrid,		DCC_RGrid	},
	{ Org2dGrid,		DCC_RGrid	},
	{ Org3dGrid,		DCC_RGrid	},
	{ OrgIRGrid,		DCC_IRGrid	},
#ifdef notyet
	{ OrgIRGrid,		DCC_RGrid	},/* 1D profiles from irgrid */
#endif
	{ OrgIRGrid,		DCC_Scalar	},
	{ OrgScalar,		DCC_Scalar	},
	{ OrgScalar,		DCC_Location	},
	{ Org1dGrid,		DCC_Location	},
	{ Org2dGrid,		DCC_Location	},
	{ Org3dGrid,		DCC_Location	},
	{ OrgNSpace,		DCC_Location	},
	{ OrgScalar,		DCC_NSpace	},
	{ OrgNSpace,		DCC_NSpace	},
/*
 * While any organization can be read as NSpace, the application must know
 * how to interpret the naming and dimn conventions of the grid classes.
 */
	{ Org1dGrid,		DCC_NSpace	},
	{ Org2dGrid,		DCC_NSpace	},
	{ Org3dGrid,		DCC_NSpace	}
};


static DataFormat netcdfFormatRec =
{
	"netCDF",			/* Name				*/
	FTNetCDF,			/* Our enumerated file type	*/
	".nc|.cdf",			/* Alternate extensions		*/
	COCTable,       		/* org-class compatibility table*/
	N_COC (COCTable),		/* number of org-class pairs	*/
	sizeof(NetCDFOpenFile),		/* size of open file instance	*/
	0,				/* Read-only			*/

	FORMAT_INIT,

	/* class */
	dnc_QueryTime,			/* Query times			*/
	fmt_MakeFileName,		/* Make file name		*/

	/* open file */
	dnc_Setup,			/* setup			*/
	dnc_OpenFile,			/* Open				*/
	dnc_CloseFile,			/* Close			*/
	dnc_SyncFile,			/* Synchronize			*/
	dnc_GetData,			/* Get the data			*/
	dnc_GetAlts,			/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	___,				/* Get forecast times		*/
	dnc_CreateFile,			/* Create a new file		*/
	___ /*dnc_PutSample*/,		/* Write to file		*/
	dnc_PutBlock,			/* Write block to a file	*/
	dnc_GetObsSamples,		/* Get observation samples	*/
	dnc_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
	dnc_TimesArray,			/* Return the times array	*/
	___                             /* Get the associated files     */
};


DataFormat *netcdfFormat = (DataFormat *) &netcdfFormatRec;


/*
 * This will always be zero unless changed by the T_NetCDF module. 
 */
static int TestTimeUnits = 0;

/*
 * We use this buffer for field queries.
 */
#define MAXFLDBUF 	512
static char     FldBuf[MAXFLDBUF];

/*
 * Structure for passing attribute info among process functions
 */
struct AttArg {
	NCTag *tag;
	int varid;
};

/*
 * Locally used private stuff.
 */
static int      dnc_OFTimes FP ((NCTag *));
static int	dnc_LoadFields FP ((NCTag *tag));
static int      dnc_GetTimes FP ((NCTag *));
static int      dnc_OFIRGrid FP ((NCTag *));
static int	dnc_OFRGrid FP ((NCTag *tag));
static int	dnc_GetRGridAlts FP ((NCTag *tag, FieldId fid, 
				      int offset, int opening));
static int	dnc_GetNSpaceAlts FP ((NCTag *tag, FieldId fid,
				       int offset, int opening));
static int	dnc_GetIRGridAlts FP ((NCTag *tag, FieldId fid, int offset));
static int	dnc_AltIndex FP ((NCTag *tag, dsDetail *, int ndetail));
static void     dnc_LoadLocation FP ((NCTag *, Location *, long, long));
static void	dnc_SPBuildIndex FP ((NCTag *tag));
static int	dnc_SPIndex FP ((NCTag *tag, PlatformId pid));
static int      dnc_BuildPMap FP ((NCTag *, int pdim));
static void     dnc_CFMakeDims FP ((NCTag *, DataChunk *, int *, int *,
				    dsDetail *details, int ndetail));
static void     dnc_CFMakeVars FP ((NCTag *, DataChunk *));
static void     dnc_CFScalarVars FP ((NCTag *, DataChunk *));
static void     dnc_CFGridVars FP ((NCTag *, DataChunk *));
static void     dnc_CFIRGridVars FP ((NCTag *, DataChunk *));
static zbool    dnc_OverheadField FP ((char *const));
static void	dnc_SetFieldTypes FP ((NCTag *tag, DataChunk *dc, int nfield,
				       FieldId *fields));
static void	dnc_NSpaceSetup FP((NCTag *tag, DataChunk *dc, 
				    int nfield, FieldId *fields));
static void	dnc_NSpaceFinishSetup FP ((NCTag *tag, DataChunk *dc,
					   dsDetail *details, int ndetail));
static void	dnc_Slice FP ((NCTag *tag, int varid, long *start,
			       long *count, dsDetail *, int));
static int 	dnc_ReadScalar FP((DataChunk *, NCTag *, long, long,
				   dsDetail *, int ndetail));
static int	dnc_ReadSubplat FP((DataChunk *dc, NCTag *tag, long begin, 
				    long nsamp, long *start, long *count));
static void 	dnc_ReadIRGrid FP ((DataChunk *, NCTag *, int, int,
				    dsDetail *, int ndetail));
static void 	dnc_ReadRGrid FP ((DataChunk *, NCTag *, int, int,
				   dsDetail *, int));
static int	dnc_ReadNSpace FP((DataChunk *, NCTag *, long,
				   long, dsDetail *, int));
static void	dnc_ReadNSpaceScalar FP((DataChunk *dc, NCTag *tag, 
					 int begin, int nsamp, 
					 dsDetail *details, int ndetail));
static int	dnc_ReadLocation FP ((DataChunk *, NCTag *, long, long));
static int 	dnc_VarAlts FP ((int ncid, int varid, AltUnitType *au));
static int	dnc_GetFieldVar FP ((NCTag *, FieldId));
static double	dnc_ZtToOffset FP ((NCTag *, ZebTime *));
static void	dnc_DoWriteCoords FP ((NCTag *, DataChunk *, int, long *,
			long *, int *));
static int	dnc_NSpaceVarPut FP ((NCTag *tag, DataChunk *dc, int varid, 
				      FieldId fid, long *start, long *count, 
				      int ndim, DataPtr data));
static int	dnc_PutRecords FP ((OpenFile *ofp, DataChunk *dc,
				    int sample, int nsample, 
				    long start[], long count[], int ndim));
static void	dnc_PutStatic FP ((OpenFile *ofp, DataChunk *dc));
static void	dnc_DefineVars FP ((NCTag *tag, DataChunk *dc, int ndim,
				    int *dims));
static void     dnc_DefineNSpaceDims(NCTag *tag, DataChunk *dc, 
				     dsDetail *details, int n);
static int	dnc_DefineNSpaceVar FP ((NCTag *tag, DataChunk *dc,
					 FieldId fid, int ndim, int *dims));
static void	dnc_DefineLocVars FP ((NCTag *tag, DataChunk *dc,
				       int ndims, int *dims,
				       int *vlat, int *vlon, int *valt));
static void	dnc_PutGlobalAttributes FP ((NCTag *tag, DataChunk *dc));
static int	dnc_PutAttribute FP ((char *key, void *value, int nval,
				      DC_ElemType type, void *arg));
static int	dnc_FindTimes FP ((OpenFile *ofp, DataChunk *dc, int sample, 
				   int count, WriteCode wc, long *start));
static DC_ElemType dnc_ElemType FP ((nc_type type));
static nc_type	dnc_NCType FP ((DC_ElemType type));
static int	dnc_ReadGlobalAtts FP ((DataChunk *, NCTag *));
static void	dnc_ReadFieldAtts FP ((DataChunk *, NCTag *, FieldId *, int));
static int	dnc_TimeUnits FP ((ZebTime *zt, const char *time_units));
static FieldId	dnc_GetFieldByName FP ((NCTag *tag, char *fname));
#if 0
static zbool	dnc_TimeIsBad (int ncid, int timevar, nc_type ttype, 
			       double val);
#endif
static int
dnc_defineDimension (NCTag* tag, char* name, int len,
		     dsDetail* details, int ndetail);


static inline double
dnc_ZtToOffset (tag, zt)
NCTag *tag;
ZebTime *zt;
/*
 * Turn this time into a suitable offset for this file.
 */
{
	return ((double)zt->zt_Sec - (double)tag->nc_base + 
		((double)zt->zt_MicroSec * (1e-6)));
}


static inline void
dnc_OffsetToZt (tag, offset, zt)
NCTag *tag;
double offset;
ZebTime *zt;
/*
 * Convert the offset from basetime into a ZebTime
 */
{
	zt->zt_Sec = tag->nc_base + (long)offset;
	zt->zt_MicroSec = (long)((1e+6) * (offset - (long)offset) + 0.5);
}



static int
dnc_QueryTime (const char *file, ZebraTime *begin, ZebraTime *end, int *nsamp)
/*
 * Query the times on this file.
 */
{
	int id, tvar, tdim;
	long ntime;
	ZebTime base;
	long index;
	nc_type dtype;
	float foffset;
	double offset;
	int loffset;
/*
 * Try opening the file.
 */
	ncopts = 0;	/* Change default error behavior */
	if ((id = ncopen (file, NC_NOWRITE)) < 0)
		return (FALSE);
/*
 * Look up the time array.
 */
	if (! dnc_DecipherTime (id, &tvar, &tdim, &ntime, &dtype, &base))
		return (FALSE);

	index = 0;
	switch (dtype)
	{
	   case NC_FLOAT:
		ncvarget1 (id, tvar, &index, &foffset);
		offset = (double) foffset;
		break;
	   case NC_DOUBLE:
		ncvarget1 (id, tvar, &index, &offset);
		break;
	   case NC_LONG:
		ncvarget1 (id, tvar, &index, &loffset);
		offset = (double) loffset;
		break;
	   default:
		return (FALSE);
	}
	begin->zt_Sec = base.zt_Sec + (long)offset;
	begin->zt_MicroSec = 1e+6 * (offset - (long)offset);

	index = ntime - 1;

	switch (dtype)
	{
	  case NC_FLOAT:
	    ncvarget1 (id, tvar, &index, &foffset);
	    offset = (double) foffset;
	    break;
	  case NC_DOUBLE:
	    ncvarget1 (id, tvar, &index, &offset);
# if 0 /* I think Java netCDF syncing is fixed now */
	    /*
	     * Big time kluge for Java-generated files, which may not be
	     * properly synced in real-time.  Look for a missing value
	     * attribute for time, and ignore the last time if it matches
	     * the missing value...
	     * 
	     * This kluge must parallel the one in dnc_GetTimes() below.  */
	    if (dnc_TimeIsBad(id, tvar, dtype, offset))
	    {
		index--;
		ntime--;
		ncvarget1 (id, tvar, &index, &offset);
	    }
	    /* End-of-kluge */
# endif
	    break;
	  case NC_LONG:
	    ncvarget1 (id, tvar, &index, &loffset);
	    offset = (double) loffset;
	    break;
	  default:
	    return (FALSE);
	}
	
	end->zt_Sec = base.zt_Sec + (long)offset;
	end->zt_MicroSec = 1e+6 * (offset - (long)offset);
/*
 * Clean up and return.
 */
	*nsamp = ntime;
	ncclose (id);
	return (TRUE);
}




static int
dnc_OpenFile (of, write)
OpenFile *of;
int write;
/*
 * Try to open this file.
 */
{
	NCTag *tag = TAGP(of);
	char *fname = of->of_df.df_fullname;
	const Platform *p = dt_FindPlatform (of->of_df.df_pid);
	int ret, v;
	int flags;
/*
 * Try to open the file.
 */
	ncopts = 0;	/* Change default error behavior */
	flags = (write ? NC_WRITE : NC_NOWRITE);
	if ((tag->nc_id = ncopen (fname, flags)) < 0)
	{
		dnc_NCError ("file open");
		return (FALSE);
	}
/*
 * Do some filling in.
 */
	tag->nc_ntime = 0;
	tag->nc_locs = (Location *) NULL;
	tag->nc_alts = (float *) NULL;
	tag->nc_subplats = (PlatformId *) NULL;
	tag->nc_subindex = (int *) NULL;
	tag->nc_nalts = 0;
	tag->nc_altvar = BadField;
	tag->nc_FMap = 0;
/*
 * We let the platform tell us what kind of file this is.  Use the parent
 * platform if the requested one is a subplatform.
 */
	while (pi_Subplatform (p))
	    p = pi_Parent (p);
	
	tag->nc_org = pi_DataOrg (p);
	tag->nc_plat = pi_Id (p);
/*
 * Deal with the time and field information.
 */
	if (!dnc_OFTimes (tag) || ! dnc_LoadFields (tag))
	{
		dnc_CloseFile (of);
		return (FALSE);
	}
/*
 * Figure out our altitude units, using those of the "alt" field if present,
 * or setting to our default.
 */
	tag->nc_altUnits = DEF_ALT_UNITS;	/* default */

	if ((v = dnc_Varid (tag->nc_id, "alt")) >= 0 ||
	     (v = dnc_Varid (tag->nc_id, "altitude")) >= 0)
	{
		if (! dnc_VarAlts (tag->nc_id, v, &tag->nc_altUnits))
		{
			msg_ELog (EF_PROBLEM, "using ambiguous 'level' units");
			tag->nc_altUnits = AU_level;
		}
	}
	else
	{
		msg_ELog (EF_DEBUG, "netcdf altitude variable not found: %s",
			  "ignoring altitudes");
	}
/*
 * The rest of the setup is organization-specific.
 */
	switch (tag->nc_org)
	{
	/*
	 * Irregular grid platform location info.
	 */
	   case OrgIRGrid:
		ret = dnc_OFIRGrid (tag);
		break;
	/*
	 * Load regular grid geometry info and available altitudes.
	 */
	   case Org1dGrid:
	   case Org2dGrid:
	   case Org3dGrid:
		ret = dnc_OFRGrid (tag);
		if (ret)
			ret = dnc_GetRGridAlts (tag, BadField, 0, 1);
		break;
	/*
	 * For scalar and nspace files, set the static location.
	 */
	   case OrgScalar:
		/* if (! ds_IsMobile (dp->df_platform)) */
		dnc_LoadLocation (tag, &tag->nc_sloc, 0, 1);
	        ret = TRUE;
		break;
	/*
	 * For nspace, also check for available altitudes
	 */
	   case OrgNSpace:
		/* if (! ds_IsMobile (dp->df_platform)) */
		dnc_LoadLocation (tag, &tag->nc_sloc, 0, 1);
	        ret = dnc_GetNSpaceAlts (tag, BadField, 0, 1);
		break;

	   default:
		msg_ELog (EF_PROBLEM, "Can't deal with org %d", tag->nc_org);
		ret = FALSE;
		break;
	}

	if (!ret)
	{
		dnc_CloseFile (of);
	}
	return (ret);
}





static int
dnc_LoadFields (tag)
NCTag *tag;
/*
 * Pull in the field information.
 */
{
	int ndim, nvar, natt, rdim, fld;
	char *cp = FldBuf;
	char longname[256];	/* anything bigger we'll just ignore */
	char units[128];
	char typename[128];
/*
 * Do an inquire to see how many vars there are.
 */
	ncinquire (tag->nc_id, &ndim, &nvar, &natt, &rdim);
	tag->nc_FMap = (FieldId *) malloc (nvar * sizeof (FieldId));
	tag->nc_nVar = nvar;
/*
 * Pass through the fields.
 */
	for (fld = 0; fld < nvar; fld++)
	{
		int ndim, dims[MAX_VAR_DIMS], natt;
		nc_type type;
	/*
	 * Look up the variable and remember it's FID.  Try to get the
	 * units and description for the variable as well and use these
	 * when declaring the field.  If the field has already been 
	 * declared, the original declaration wins out.
	 */
		ncvarinq (tag->nc_id, fld, cp, &type, &ndim, dims, &natt);
		strcpy (longname, cp);
		strcpy (units, "unknown");
		strcpy (typename, "");
		
		(void)dnc_GetStringAtt (tag->nc_id, fld, VATT_LONGNAME, 
					longname, sizeof(longname));
		(void)dnc_GetStringAtt (tag->nc_id, fld, VATT_UNITS, 
					units, sizeof(units));
		(void)dnc_GetStringAtt (tag->nc_id, fld, VATT_FTYPE, 
					typename, sizeof(typename));
		tag->nc_FMap[fld] = F_Field (cp, typename, longname, units);
	}
	return (TRUE);
}



static int
dnc_OFIRGrid (tag)
NCTag *tag;
/*
 * Figure out the setup for this IRGRID file.
 */
{
	int dim, lat, lon, alt;
	long start = 0, stop;
	long stride[1];
	long imap[1];
/*
 * See how many platforms there are.  Try to be flexible about the
 * dimension names which we'll accept.
 */
	if ((dim = dnc_Dimid (tag->nc_id, "platform")) < 0 &&
	    (dim = dnc_Dimid (tag->nc_id, "station")) < 0 &&
	    (dim = dnc_Dimid (tag->nc_id, "site")) < 0 &&
	    (dim = dnc_Dimid (tag->nc_id, "stn")) < 0 &&
	    (dim = dnc_Dimid (tag->nc_id, "plat")) < 0 &&
	    (dim = dnc_Dimid (tag->nc_id, "facility")) < 0)
	{
		dnc_NCError ("platform dimid");
		return (FALSE);
	}
	if (ncdiminq (tag->nc_id, dim, (char *) 0, &tag->nc_nPlat) < 0)
	{
		dnc_NCError ("platform diminq");
		return (FALSE);
	}
/*
 * Find the positioning data.  We don't want the regular lat/lon/alt if
 * they don't have the platform dimension.  So look around some.
 */
	if ((lat = dnc_LocationID (tag->nc_id, dim, "lat")) < 0 ||
	    (lon = dnc_LocationID (tag->nc_id, dim, "lon")) < 0 ||
	    (alt = dnc_LocationID (tag->nc_id, dim, "alt")) < 0)
	{
		dnc_NCError ("could not find station location variables");
		return (FALSE);
	}
/*
 * OK, assume this is going to work.  Allocate the space we need.
 */
	tag->nc_locs = (Location *) malloc (tag->nc_nPlat * sizeof(Location));
/*
 * Now we go through and grab each piece of the location.
 */
	stop = tag->nc_nPlat;
	stride[0] = 1;
	imap[0] = sizeof(Location);
	if (ncvargetg (tag->nc_id, lat, &start, &stop, stride, imap,
		       &(tag->nc_locs[0].l_lat)) < 0)
		msg_ELog (EF_PROBLEM, "Lat get failure %d", ncerr);

	if (ncvargetg (tag->nc_id, lon, &start, &stop, stride, imap,
		       &(tag->nc_locs[0].l_lon)) < 0)
		msg_ELog (EF_PROBLEM, "Lon get failure %d", ncerr);

	if (ncvargetg (tag->nc_id, alt, &start, &stop, stride, imap,
		      &(tag->nc_locs[0].l_alt)) < 0)
		msg_ELog (EF_PROBLEM, "Alt get failure %d", ncerr);
/*
 * Build the subplatform map.
 */
	if (!dnc_BuildPMap (tag, dim))
		return (FALSE);
/*
 * All done.
 */
	return (TRUE);
}


/*
 * Abandon the huge persistent subplatform map in favor of something which
 * allows the list of subplatforms stored in each file to vary.  Keep the
 * nc_subplats array to map the file subplat dimension to platform ID, and
 * use the nc_subindex array to hash a platform ID into the file dimension
 * coordinate.  Take advantage of the fact that subplatform ID's are
 * virtually always consecutive with a simple modulo hash.
 */

static void
dnc_SPBuildIndex (tag)
NCTag *tag;
/*
 * Create and initialize the subplat index array according to the number
 * of subplatforms.  The extra slot in the index array is for a perpetual
 * -1 to stop hash searches on a non-existent id.
 */
{
	int nplat = tag->nc_nPlat;
	int i;

	tag->nc_subindex = (int *) malloc ((nplat + 1) * sizeof(int));
	/*
	 * Create an empty hash array.
	 */
	for (i = 0; i < nplat+1; ++i)
	{
		tag->nc_subindex[i] = -1;
	}
	/*
	 * Fill it according to the id's in the subplats array.
	 */
	for (i = 0; i < nplat; ++i)
	{
		int hash;

		if (tag->nc_subplats[i] == BadPlatform)
			continue;
		hash = tag->nc_subplats[i] % tag->nc_nPlat;
		while (tag->nc_subindex[hash] != -1)
			if (++hash == (tag->nc_nPlat + 1))
				hash = 0;
		tag->nc_subindex[hash] = i;
	}
}
	


static int
dnc_SPIndex (tag, pid)
NCTag *tag;
PlatformId pid;
/*
 * Map this subplatform id into its netcdf coordinate.  Return -1
 * if the id is not contained in the subplatform list.
 */
{
	int idx = -1;

	if (tag->nc_subindex && pid != BadPlatform)
	{
		int hash = (pid % tag->nc_nPlat);
		idx = tag->nc_subindex[hash];
		while ((idx != -1) && (tag->nc_subplats[idx] != pid))
		{
			if (++hash == (tag->nc_nPlat + 1))
				hash = 0;
			idx = tag->nc_subindex[hash];
		}
	}
	return (idx);
}



static const char *PlatDimnNames[] = {
	"platform", "station", "site_num", "stn_num", "plat_num", "site_id", 
	"stn_id", "plat_id", "num_site", "num_stn", "num_plat", "subplat", 
	"facility", NULL };


static int
dnc_BuildPMap (tag, pdim)
NCTag *tag;
int pdim;
/*
 * Build the subplatform lookup map.
 */
{
	int i, name_id = -1, len_id, plat;
	int ndims;
	int dimids[MAX_VAR_DIMS];
	nc_type vtype;
	long fldlen;
	long start[MAX_VAR_DIMS];
	long count[MAX_VAR_DIMS];
	char *name, *fullname;
	const char *base;
	char value[sizeof(double)];
/*
 * Get set up to start snooping through platform names.
 */
	base = ds_PlatformName (tag->nc_plat);
/*
 * Look for a subplat 'id' variable, and check it's type for either
 * string or numeric.  Numeric id's will be converted to string names.
 * For strings, we assume the last dimension of the variable is the
 * max length of the strings.
 */
	i = 0;
	while (PlatDimnNames[i])
	{
		name_id = dnc_LocationID (tag->nc_id, pdim, PlatDimnNames[i]);
		if (name_id >= 0)
			break;
		i++;
	}
	if (! PlatDimnNames[i])
	{
		msg_ELog (EF_PROBLEM, "No platform names for %s", base);
		return (FALSE);
	}
/*
 * Now check the type
 */
	if (ncvarinq (tag->nc_id, name_id, 0, &vtype, &ndims, dimids, 0) < 0)
	{
		dnc_NCError ("inquiring sub-platform id variable");
		return (FALSE);
	}
	for (i = 0; i < ndims; ++i)
	{
		start[i] = 0;
		count[i] = 1;
	}
/*
 * Find out how long the names are and allocate appropriate space.
 */
	if (vtype == NC_CHAR)
	{
		if (ndims < 2)
		{
			dnc_NCError ("platform names without length dimn");
			return (FALSE);
		}
		len_id = dimids[ndims - 1];
		if (ncdiminq (tag->nc_id, len_id, NULL, &fldlen) < 0)
		{
			dnc_NCError ("inquiring length of platform names");
			return (FALSE);
		}
		count[ndims - 1] = fldlen;
	}
	else
	{
		fldlen = 15;	/* large enough to hold a number */
	}

	name = (char *) malloc (fldlen + 1);
	fullname = (char *) malloc (fldlen + strlen (base) + 1);
/*
 * Go through and read back all the platform ids.
 */
	tag->nc_subplats = 
		(PlatformId *) malloc (tag->nc_nPlat * sizeof(PlatformId));
	for (i = 0; i < tag->nc_nPlat; i++)
	{
	/*
	 * Read the name of this platform.
	 */
		start[0] = i;
		if (((vtype == NC_CHAR) ?
			ncvarget (tag->nc_id, name_id, start, count, name) :
		        ncvarget (tag->nc_id, name_id, start, count, value))
				< 0)
		{
			msg_ELog (EF_PROBLEM,
			       "Error %d reading subplat %d from %s", ncerr,
				 i, base);
			break;
		}
		if (vtype != NC_CHAR)	/* convert to string */
		{
			int id;
			id = (int) atoi (dnc_ValueToString (value, vtype, 1));
			sprintf (fullname, "%s/%i", base, id);
		}
		else
			sprintf (fullname, "%s/%s", base, name);
	/*
	 * Create the full name of this subplatform, and look it up.
	 */
		if ((plat = ds_LookupPlatform (fullname)) == BadPlatform)
			msg_ELog (EF_INFO, "NC Platform %s unknown", fullname);
		tag->nc_subplats[i] = plat;
	}
	free (name);
	free (fullname);
	if (i < tag->nc_nPlat)
	{
		free (tag->nc_subplats);
		tag->nc_subplats = NULL;
		return (FALSE);
	}
	dnc_SPBuildIndex (tag);
	return (TRUE);
}




static int
dnc_OFRGrid (tag)
NCTag *tag;
/*
 * Finish opening a regular grid file.
 */
{
	int v, d;
	long ldim;
/*
 * Get the grid origin.
 */
/*
 * Get the grid origin.
 */
	if ((v = dnc_Varid (tag->nc_id, "lat")) < 0 &&
	    (v = dnc_Varid (tag->nc_id, "latitude")) < 0) 
	{
		dnc_NCError ("No 'lat' or 'latitude' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_sloc.l_lat);
	if ((v = dnc_Varid (tag->nc_id, "lon")) < 0 &&
	    (v = dnc_Varid (tag->nc_id, "longitude")) < 0)
	{
		dnc_NCError ("No 'lon' or 'longitude' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_sloc.l_lon);
	if ((v = dnc_Varid (tag->nc_id, "alt")) < 0 &&
	    (v = dnc_Varid (tag->nc_id, "altitude")) < 0) 
	{
		dnc_NCError ("No 'alt' or 'altitude' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_sloc.l_alt);
/*
 * Now the grid dimensions.
 */
	tag->nc_rgrid.rg_nZ = tag->nc_rgrid.rg_nY = 1;
	switch (tag->nc_org)
	{
	   case Org3dGrid:
		if ((d = ncdimid (tag->nc_id, "z")) < 0)
		{
			dnc_NCError ("No z dimension");
			return (FALSE);
		}
		ncdiminq (tag->nc_id, d, (char *) 0, &ldim);
		tag->nc_rgrid.rg_nZ = ldim;

	     /* fall into */
	   case Org2dGrid:
		if ((d = ncdimid (tag->nc_id, "y")) < 0)
		{
			dnc_NCError ("No y dimension");
			return (FALSE);
		}
		ncdiminq (tag->nc_id, d, (char *) 0, &ldim);
		tag->nc_rgrid.rg_nY = ldim;

	     /* fall into */
	   case Org1dGrid:
		if ((d = ncdimid (tag->nc_id, "x")) < 0)
		{
			dnc_NCError("No x dimension");
			return (FALSE);
		}
		ncdiminq (tag->nc_id, d, (char *) 0, &ldim);
		tag->nc_rgrid.rg_nX = ldim;
		break;
	   default:
		/* silence warnings */
		break;
	}
/*
 * Finally the grid spacings.
 */
	tag->nc_rgrid.rg_Yspacing = tag->nc_rgrid.rg_Zspacing = 0.0;
	if ((v = dnc_Varid (tag->nc_id, "x_spacing")) < 0)
	{
		dnc_NCError ("No 'x_spacing' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_rgrid.rg_Xspacing);
	if ((tag->nc_org == Org2dGrid) || (tag->nc_org == Org3dGrid))
	{
		if ((v = dnc_Varid (tag->nc_id, "y_spacing")) < 0)
		{
			dnc_NCError ("No 'y_spacing' variable");
			return (FALSE);
		}
		ncvarget1 (tag->nc_id, v, 0, &tag->nc_rgrid.rg_Yspacing);
		if (tag->nc_org == Org3dGrid)
		{
			if ((v = dnc_Varid (tag->nc_id, "z_spacing")) < 0)
			{
				dnc_NCError ("No 'z_spacing' variable");
				return (FALSE);
			}
			ncvarget1 (tag->nc_id, v,0,&tag->nc_rgrid.rg_Zspacing);
		}
	}
	return (TRUE);
}



static int
dnc_GetRGridAlts (tag, fid, offset, opening)
NCTag *tag;
FieldId fid;
int offset;
int opening;
/*
 * Store the altitude levels from this rgrid org file into the tag
 */
{
	int i;
	RGrid *rg;
	int nalts;
	float *alts;

	if (! opening)		/* keep what we figured earlier */
		return (TRUE);
/*
 * Organization determines what happens here.
 */
	rg = &tag->nc_rgrid;
	nalts = rg->rg_nZ;
	alts = (float *)malloc (nalts * sizeof(float));
	alts[0] = tag->nc_sloc.l_alt;

	for (i = 1; i < rg->rg_nZ; i++)
		alts[i] = alts[i - 1] + rg->rg_Zspacing;
	tag->nc_alts = alts;
	tag->nc_nalts = nalts;
	return (TRUE);
}



static int
dnc_GetNSpaceAlts (tag, fid, offset, op)
NCTag *tag;
FieldId fid;
int offset;
int op;		/* opening a file; initialize the altitudes */
/*
 * Look for an 'altitude' coordinate variable with dimensions
 * other than time.  Take the values along the coordinate dimension as the
 * available altitudes.  If no coordinate variable can be found, then
 * leave the altitudes array empty.
 *
 * All of the variables should already be read and fields defined.
 * At the moment, we always return TRUE since it is valid for nspace org
 * not to have an altitude array.
 *
 * If we already have some altitudes, keep them.  For now NSpace does not
 * allow different arrays of altitudes in the same file.  And we ignore
 * fid and offset.
 */
{
	float *alts;
	int nalts;
	FieldId altid;
	int varid;
	int ndims;
	char *anamelist[] = { "altitude", "alt" };
	char *altname;
	int dimids[MAX_VAR_DIMS];
	long start[MAX_VAR_DIMS];
	long count[MAX_VAR_DIMS];
	int altdimn;
	nc_type vtype;
	int i;

	if (! op)		/* don't change unless it's a new tag */
		return (TRUE);
/*
 * Find the first name in our altitude name list for which we have a 
 * variable defined (if any).
 */
	for (i = 0; i < sizeof(anamelist) / sizeof(anamelist[0]); i++)
	    if ((altid = dnc_GetFieldByName (tag, anamelist[i])) != BadField)
		break;

	if (altid == BadField)
	    return (TRUE);

	altname = anamelist[i];
/*
 * Get the ncvar and its dimensions
 */
	varid = dnc_GetFieldVar (tag, altid);

	ncvarinq (tag->nc_id, varid, NULL, &vtype, &ndims, dimids, NULL);
	if (vtype != NC_FLOAT)
	{
		msg_ELog (EF_DEBUG, "%s: '%s' variable not float type",
			  "netcdf nspace", altname);
		return (TRUE);
	}
/*
 * For each dimension of this variable, verify that one of the
 * dimensions has the same name as the variable (making it a coordinate
 * variable), and set the start and count arrays accordingly to read along
 * that dimension.
 */
	altdimn = -1;
	nalts = 1;
	for (i = 0; i < ndims; ++i)
	{
		char dimname[MAX_NC_NAME];
		long dimsize;

		start[i] = 0;
		ncdiminq (tag->nc_id, dimids[i], dimname, &dimsize);
		if (! strcmp (dimname, altname))
		{
			altdimn = i;
			count[i] = dimsize;
		}
		else
		{
			count[i] = 1;
		}
		nalts *= count[i];
	}
/*
 * It is possible to accept an altitude variable with zero dimensions, in
 * which case nalts is 1, and the altitudes do not vary with time.
 */
	if ((ndims > 0) && (altdimn == -1))
	{
		msg_ELog (EF_DEBUG, "%s: '%s' not a coordinate variable",
			  "netcdf nspace", altname);
		return (TRUE);
	}
/*
 * Allocate space for the altitudes, then read them.
 */
	alts = (float *) malloc (sizeof(float) * nalts);
	if (ncvarget (tag->nc_id, varid, start, count, (void *)alts) < 0)
	{
		dnc_NCError ("GetNSpaceAlts ncvarget failed");
		free (alts);
		return (TRUE);
	}
/*
 * Update our tag and we're done.
 */
	tag->nc_nalts = nalts;
	tag->nc_alts = alts;
	return (TRUE);
}





static int
dnc_OFTimes (tag)
NCTag *tag;
/*
 * Deal with the time info in this file.
 */
{
	long ntime;
	ZebTime base;
/*
 * We ignore the number of times returned since dnc_GetTimes takes care
 * of syncing the number of times already in the tag with the number now
 * recorded in the file.
 */
	if (! dnc_DecipherTime (tag->nc_id, &tag->nc_vTime, &tag->nc_dTime,
				&ntime, &tag->nc_timeType, &base))
	{
		dnc_NCError ("could not decipher time variables");
		return (FALSE);
	}
	tag->nc_base = base.zt_Sec;

	tag->nc_haveTimeDim = (tag->nc_dTime >= 0);
/*
 * Pull in the time array, and we're done.
 */
	return (dnc_GetTimes (tag));
}




static int
dnc_GetTimes (tag)
NCTag *tag;
/*
 * Pull in the times.
 */
{
	long ntime, zero = 0;
	double *dtime;
	float *ftime;
	int *ltime;
	int status, i;
/*
 * If the number of times available exceeds the space allocated,
 * start over.
 */
	if (tag->nc_haveTimeDim)
	{
	    if (ncdiminq (tag->nc_id, tag->nc_dTime, (char *) 0, &ntime) < 0)
	    {
		dnc_NCError ("time_offset dim inq");
		return (FALSE);
	    }
	}
	else
	    ntime = 1;
	
	if (ntime > tag->nc_ntime)
	{
		if (tag->nc_ntime)
			free (tag->nc_times);
		tag->nc_times = (ZebTime *) malloc (ntime * sizeof (ZebTime));
	}
	tag->nc_ntime = ntime;
	msg_ELog (EF_DEBUG, "cdf times: %d samps", ntime);
/*
 * Now read in the entire array.  Later, for updates, this call
 * should be a bit more careful and not read the entire array -- it
 * could be expensive.
 */
	if (tag->nc_timeType == NC_DOUBLE)
	{
	/*
	 * Read times as doubles, then convert to ZebTimes.
	 */
		dtime = (double *) malloc (ntime * sizeof (double));
		status = ncvarget (tag->nc_id, tag->nc_vTime, &zero, &ntime, 
				   dtime);
		if (status >= 0)
			for (i = 0; i < ntime; i++)
				dnc_OffsetToZt (tag, dtime[i], 
						tag->nc_times + i);
# if 0 /* I think Java netCDF syncing is fixed now */
	/*
	 * Big time kluge for Java-generated files, which may not be
	 * properly synced in real-time.  Look for a missing value
	 * attribute for time, and ignore the last time if it matches
	 * the missing value...
	 *
	 * This kluge must parallel the one in dnc_QueryTime() above.
	 */
		if (dnc_TimeIsBad(tag->nc_id, tag->nc_vTime, tag->nc_timeType,
				  dtime[ntime-1]))
		    tag->nc_ntime--;
	/* End-of-kluge */
# endif
		
		free (dtime);
	}
	else if (tag->nc_timeType == NC_FLOAT)
	{
	/*
	 * Read times as floats, then move them to the ZebTime array.
	 */
		ftime = (float *) malloc (ntime * sizeof (float));
		status = ncvarget (tag->nc_id, tag->nc_vTime, &zero, &ntime, 
				   ftime);
		if (status >= 0)
			for (i = 0; i < ntime; i++)
				dnc_OffsetToZt (tag, (double) ftime[i], 
						tag->nc_times + i);
		free (ftime);
	}
	else /* tag->nc_timeType == NC_LONG or NC_INT */
	{
	/*
	 * Read times as ints, then move them to the ZebTime array.
	 */
		ltime = (int *) malloc (ntime * sizeof (ltime[0]));
		status = ncvarget (tag->nc_id, tag->nc_vTime, &zero, &ntime, 
				   ltime);
		if (status >= 0)
			for (i = 0; i < ntime; i++)
				dnc_OffsetToZt (tag, (double) ltime[i], 
						tag->nc_times + i);
		free (ltime);
	}

	if (status < 0)
	{
		dnc_NCError ("time_offset get");
		return (FALSE);
	}
	return (TRUE);
}



static ZebTime *
dnc_TimesArray (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	NCTag *tag = TAGP (ofp);

	*ntime = tag->nc_ntime;
	return (tag->nc_times);
}




static void
dnc_CloseFile (ofp)
OpenFile *ofp;
/*
 * Close this file.
 */
{
	NCTag *tag = TAGP (ofp);

	ncclose (tag->nc_id);
	if (tag->nc_ntime)
		free (tag->nc_times);
	if (tag->nc_locs)
		free (tag->nc_locs);
	if (tag->nc_FMap)
		free (tag->nc_FMap);
	if (tag->nc_alts)
		free (tag->nc_alts);
	if (tag->nc_subplats)
		free (tag->nc_subplats);
	if (tag->nc_subindex)
		free (tag->nc_subindex);
}




static int
dnc_SyncFile (ofp)
OpenFile *ofp;
/*
 * Synchronize this file.
 */
{
	NCTag *tag = TAGP (ofp);
/*
 * Update to the file itself, then reload the times array.
 */
	if (ncsync (tag->nc_id) < 0)
		msg_ELog (EF_PROBLEM, "ncsync failed");
	return (dnc_GetTimes (tag));
}





static DataChunk *
dnc_Setup (ofp, fields, nfield, class)
OpenFile *ofp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Get set up for this piece of data access.
 */
{
	NCTag *tag = TAGP (ofp);
	DataChunk *dc;
/*
 * Create a data chunk with the desired organization.
 */
	dc = dc_CreateDC (class);
/*
 * Make sure we have altitudes, and put the units into the data chunk.
 */
	if (nfield > 0)
		dnc_GetAlts (ofp, fields[0], 0, NULL, NULL, NULL);
	dc_SetLocAltUnits (dc, tag->nc_altUnits);
/*
 * Now we try to get everything together.
 */
	switch (class)
	{
	/*
	 * Irgrids need field and platform info.
	 */
	   case DCC_IRGrid:
	   	dc_IRSetup (dc, tag->nc_nPlat, tag->nc_subplats, tag->nc_locs,
			    nfield, fields);
		dnc_SetFieldTypes (tag, dc, nfield, fields);
		break;
	/*
	 * All rgrids need is the set of fields.
	 */
	   case DCC_RGrid:
	   	dc_RGSetup (dc, nfield, fields);
		dnc_SetFieldTypes (tag, dc, nfield, fields);
		break;
	/*
	 * Scalars need to know the type of the fields.
	 */
	   case DCC_Scalar:
		dc_SetScalarFields (dc, nfield, fields);
		dnc_SetFieldTypes (tag, dc, nfield, fields);
		break;
	/*
	 * NSpace must inquire about dimensions for each field
	 */
	   case DCC_NSpace:
		dnc_NSpaceSetup (tag, dc, nfield, fields);
		break;
	/*
	 * Locations are truly simple.
	 */
	   case DCC_Location:
	   	break;
	/*
	 * Hmm....
	 */
	   default:
	   	msg_ELog (EF_PROBLEM, "Unsupported data class %d", class);
		dc_DestroyDC (dc);
		return (0);
	}
/*
 * Read global and field attributes.  Any attributes set in the DataChunk
 * when data is retrieved will override any attributes set here
 * (e.g. a change in the bad value due to a dsDetail)
 */
	dnc_ReadGlobalAtts(dc, tag);	/* read global attributes */

	if (class != DCC_Location)	/* read field attributes */
	{
		dnc_ReadFieldAtts(dc, tag, fields, nfield);
	}

	return (dc);
}





static void
dnc_SetFieldTypes (tag, dc, nfield, fields)
NCTag *tag;
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Inquire the type of our variables and set the types in our DataChunk
 */
{
	int i;
	int varid;
	int ndims, natts;
	nc_type vtype;
	int dims[ MAX_VAR_DIMS ];
	DC_ElemType types[ MAX_NC_VARS ];

	/*
	 * For each field, see if its varid is in the tag and get the type.
	 * Unknown fields default to Float so that they'll be filled with
	 * bad values when data are retrieved.
	 */
	for (i = 0; i < nfield; ++i)
	{
	    types[i] = DCT_Float;
	    if ((varid = dnc_GetFieldVar (tag, fields[i])) < 0)
		continue;
	    ncvarinq (tag->nc_id, varid, 0, &vtype, &ndims, dims, &natts);
	    if (vtype != NC_BYTE)
		types[i] = dnc_ElemType (vtype);
	    else
	    {
		/*
		 * NC_BYTE vars would get mapped to DCT_UnsignedChar by
		 * default, but we map to (signed) DCT_Char instead
		 * if there's a non-zero "is_signed" attribute.  The test
		 * below should work if the attribute is any of the
		 * integer types (NC_CHAR, NC_BYTE, NC_SHORT, NC_INT).
		 */
		int s;
		if (ncattget(tag->nc_id, varid, VATT_IS_SIGNED, &s) != -1 && s)
		    types[i] = DCT_Char;
		else
		    types[i] = DCT_UnsignedChar;
	    }
	}
	dc_SetFieldTypes (dc, nfield, fields, types);
}




static void
dnc_DefineNSpaceField (tag, dc, fid)
NCTag *tag;
DataChunk *dc;
FieldId fid;
{
	int j;
	int varid;
	nc_type vtype;
	DC_ElemType elemtype;
	int is_static;
	int ndims, natts, nsdims, startdim;
	int dims[ MAX_VAR_DIMS ];
	unsigned long sizes[ MAX_NC_DIMS ];
	static char dim_names[ MAX_NC_DIMS ][ MAX_NC_NAME ];
	static char *names[ MAX_NC_DIMS ] = { NULL };

	if (names[0] == NULL)
	{
		for (j = 0; j < MAX_NC_DIMS; ++j)
			names[j] = &(dim_names[j][0]);
	}
	/*
	 * See if this field's varid is in the tag.  If not, default
	 * to scalar float.  Else inquire the varid and get its dimensions.
	 */
	if ((varid = dnc_GetFieldVar (tag, fid)) < 0)
	{
		/*
		 * Field unknown, so default to a static scalar float.
		 * This will later get filled with bad values.
		 * It can be static since it will be all bad values
		 * for every sample time.
		 *
		 * XXX Unless the next file read does have this field!
		 */
		ndims = 0;
		is_static = TRUE;
		vtype = NC_FLOAT;
	}
	else
	{
		/*
		 * Have a varid for the field.  Get the dimensions.
		 * If not static, then we skip the first dimension,
		 * time, when defining the field for the NSpace
		 * chunk.
		 */
		ncvarinq (tag->nc_id, varid, 0, &vtype, &ndims, 
			  dims, &natts);
		/*
		 * Consider the field static if:
		 *   o this file has a time dimension
		 *		AND
		 *   o the time dimension is not the first dimension
		 *     of the field
		 */
		is_static = tag->nc_haveTimeDim &&
		    ((ndims == 0) || (dims[0] != tag->nc_dTime));
	}
	nsdims = 0;
	startdim = ((!tag->nc_haveTimeDim || is_static) ? 0 : 1);
	for (j = startdim; j < ndims; ++j)
	{
		ncdiminq (tag->nc_id, dims[j], names[nsdims], 
			  (long *) &(sizes[nsdims]));
		++nsdims;
	}
	/*
	 * Have all the dimensions.  Define the field.
	 */
	dc_NSDefineField (dc, fid, nsdims, names, sizes, is_static);

	/*
	 * Set the type for this field in the dc.  Look for the "is_signed"
	 * attribute if this is a one-byte var, otherwise use the simple
	 * mapping from dnc_ElemType().
	 */
	if (vtype == NC_BYTE)
	{
	  int s;
	  if (ncattget(tag->nc_id, varid, VATT_IS_SIGNED, &s) != -1 && s)
	    elemtype = DCT_Char;
	  else
	    elemtype = DCT_UnsignedChar;
	}
	else
	  elemtype = dnc_ElemType (vtype);

	dc_SetType (dc, fid, elemtype);
}




static void
dnc_NSpaceSetup (tag, dc, nfield, fids)
NCTag *tag;
DataChunk *dc;
int nfield;
FieldId *fids;
/*
 * Define the given empty NSpace datachunk with the given fields
 * according to the dimensions of the variables in the netCDF file.
 * Fields which do not have a time dimension (assumed to be the
 * first dimension) will be defined static.  Fields which are not
 * found in the file are not put in the datachunk (for now).
 * At exit, the definition of the chunk is completed and closed.
 */
{
	int i;
	char *names[ DC_MaxDimension ];

	/*
	 * First define the explicitly requested fields.
	 */
	dc_NSAllowRedefine (dc, TRUE);
	for (i = 0; i < nfield; ++i)
	{
		dnc_DefineNSpaceField (tag, dc, fids[i]);
	}
	/*
	 * Once the fields are defined, check each field for dimensions
	 * which are coordinate variables, and add these to the datachunk
	 * also.
	 */
	for (i = 0; i < nfield; ++i)
	{
		int ndims, j;
		FieldId fid;

		if (! dc_NSGetField (dc, fids[i], &ndims, names, NULL, NULL))
			continue;
		for (j = 0; j < ndims; ++j)
		{
		/*
		 * Try to find a field in this file with the same name as
		 * the dimension.  If the name is not known then it cannot
		 * be a variable in this file, so we can ignore it.
		 * Otherwise, see if the field has a corresponding varid,
		 * which would mean this dimension is a coordinate
		 * variable.  If so, add the dimension name's field id to
		 * the list of variables to define, unless that field id is
		 * already in the list. 
		 */
			fid = dnc_GetFieldByName (tag, names[j]);
		    
			if ((fid != BadField) 
			    && (dnc_GetFieldVar (tag, fid) >= 0)
			    && (dc_GetFieldIndex (dc, fid) < 0))
			{
				msg_ELog (EF_DEBUG, "%s variable '%s'", 
					  "fetching implicit coordinate",
					  names[j]);
				dnc_DefineNSpaceField (tag, dc, fid);
			}
		}
	}
	/* 
	 * Must put off completion until we know whether to slice alts.
	 */
}





static void
dnc_NSpaceFinishSetup (tag, dc, details, ndetail)
NCTag *tag;
DataChunk *dc;
dsDetail *details;
int ndetail;
/* 
 * We must redefine an altitude dimension to size 1 if we will be
 * slicing at a particular altitude level.
 * 
 * If any dimensions must be fixed, redefine them to their new size of
 * one here.
 *
 * Since the DataChunk definition is not complete yet, we must
 * use the DataChunk to get the fields after completing the definition.
 */
{
	int nfield;
	FieldId *fids;
	FieldId altid;
	FieldId dimid;
	int i;

	dc_NSAllowRedefine (dc, TRUE);

	altid = F_Lookup ("altitude");
	if ((altid != BadField) &&
	    ds_GetDetail ("altitude", details, ndetail, NULL))
		dc_NSDefineDimension (dc, altid, (long)1 );

	for (i = 0; i < ndetail; ++i)
	{
		if (strcmp(details[i].dd_Name, DD_FIX_DIMENSION))
			continue;
		dimid = F_Lookup (details[i].dd_V.us_v_ptr);
		dc_NSDefineDimension (dc, dimid, (long)1 );
	}

	dc_NSDefineComplete (dc);
	fids = dc_GetFields (dc, &nfield);
	dnc_SetFieldTypes (tag, dc, nfield, fids);
}




static int
dnc_GetData (ofp, dc, begin, nsamp, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int begin;		/* sample number to grab */
int nsamp;		/* number of samples to grab */
dsDetail *details;
int ndetail;
/*
 * Actually get the data that all that work has been done for.  Append
 * the samples to the given data chunk.
 */
{
	NCTag *tag = TAGP (ofp);
/*
 * Verify alt units of this file match the datachunk we're adding to
 */
	if (tag->nc_altUnits != dc_GetLocAltUnits (dc))
	{
		msg_ELog (EF_PROBLEM, "%s: altitude units mismatch",
			  "netCDF get data");
		return (0);
	}
/*
 * If nspace chunk, we need to finish the definition using the details
 * BEFORE we can retrieve the field list from the datachunk.  Of course, we
 * don't need to do anything if the definition was finished while reading
 * a previous file.
 */
	if ((dc->dc_Class == DCC_NSpace) && !dc_NSDefineIsComplete(dc))
		dnc_NSpaceFinishSetup(tag, dc, details, ndetail);
/*
 * Prepare the DataChunk for the samples we're about to add using the current
 * sample size hint.
 */
	dc_AddMoreSamples (dc, nsamp, /*sample size*/ 0 );
/*
 * Now we have to split out based on the class they want.
 */
	switch (dc->dc_Class)
	{
	/*
	 * Scalars.
	 */
	   case DCC_Scalar:
		dnc_ReadScalar (dc, tag, begin, nsamp, details, ndetail);
		break;
	/*
	 * IRGrids.
	 */
	   case DCC_IRGrid:
	   	dnc_ReadIRGrid (dc, tag, begin, nsamp, details, ndetail);
		break;
	/*
	 * Regular grids.
	 */
	   case DCC_RGrid:
	   	dnc_ReadRGrid (dc, tag, begin, nsamp, details, ndetail);
		break;
	/*
	 * Locations.
	 */
	   case DCC_Location:
	   	dnc_ReadLocation (dc, tag, begin, nsamp);
		break;
	/*
	 * NSpace.
	 */
	   case DCC_NSpace:
		dnc_ReadNSpace (dc, tag, begin, nsamp, details, ndetail);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Don't handle class %d yet",
				dc->dc_Class);
		return (FALSE);
	}
	return (TRUE);
}





static void
dnc_Slice (tag, varid, start, count, details, ndetail)
NCTag *tag;
int varid;
long start[MAX_VAR_DIMS];
long count[MAX_VAR_DIMS];
dsDetail *details;
int ndetail;
/*
 * Touch up the start and count arrays according to the dimensions of
 * this field and the list of details.  Dimensions which are not fixed
 * by a detail default to index 0.
 */
{
	int id = tag->nc_id;
	int ndims;
	int dimids[ MAX_VAR_DIMS ];
	char dname[ MAX_NC_NAME+1 ];
	int dindex;
	long size;
	int dim;
	int firstdim;
	int altindex;

	/* 
	 * Find the variable's dimensions, and loop through them slicing
	 * those that need to be sliced.
	 */
	if (ncvarinq (id, varid, NULL, NULL, &ndims, dimids, NULL) < 0)
		return;

	/*
	 * What altitude index?
	 */
	altindex = dnc_AltIndex (tag, details, ndetail);

	/*
	 * The time dimension, if any, will be first, followed by the
	 * irgrid station dimension, if any.  We skip these.
	 */
	firstdim = (tag->nc_haveTimeDim ? 1 : 0) +
	    ((tag->nc_org == OrgIRGrid) ? 1 : 0);

	for (dim = firstdim; dim < ndims; dim++)
	{
		if ((tag->nc_org == OrgIRGrid) && (altindex >= 0) &&
		    (dim == 2))
		{
			/*
			 * The dimension following time and station is an 
			 * altitude, so use the altitude index.
			 */
			dindex = altindex;
			size = tag->nc_nalts;
		}
		/*
		 * Get the dimension's name and check for relevant details
		 */
		else if (ncdiminq (id, dimids[dim], dname, &size) < 0)
			return;
		else if (! dc_NSFixedDimension (details, ndetail, 
						dname, &dindex))
		{
			msg_ELog (EF_INFO, 
				  "slicing dimn %s at index 0 by default",
				  dname);
			dindex = 0;
		}
		if (dindex >= size)
			dindex = size - 1;
		if (dindex < 0)
			dindex = 0;
		start[dim] = dindex;
		count[dim] = 1;
	}
}





static int
dnc_ReadScalar (dc, tag, begin, nsamp, details, ndetail)
DataChunk *dc;
NCTag *tag;
long begin, nsamp;
dsDetail *details;
int ndetail;
/*
 * Retrieve scalar data from a subplatform of this file.
 */
{
	void *data;
	long start[MAX_VAR_DIMS], count[MAX_VAR_DIMS];
	long imap[MAX_VAR_DIMS];
	int field;
	int vfield = -1;
	int sbegin = dc_GetNSample (dc);
	int fill;
	int i, sampsize;
	int spix = -1;	/* subplatform index */
	int nfield;
	FieldId *fids = dc_GetFields (dc, &nfield);
/*
 *  Figure out our coords.
 */
	if (tag->nc_haveTimeDim)
	{
	    start[0] = begin;
	    count[0] = nsamp;
	}
	
	for (i = 0; i < MAX_VAR_DIMS; ++i)
		imap[i] = 0;
/*
 * Check for reading a station from an irgrid.
 */
	if (tag->nc_org == OrgIRGrid)
		spix = dnc_ReadSubplat (dc, tag, begin, nsamp, start, count);
/*
 * Now we can actually pull out the data.  We can't use ncrecget because
 * some of the fields may be sliced according to the details and any extra
 * dimensions.  Make space first, then find the sample stride so we can
 * read directly into the datachunk.
 */
	for (field = 0; field < nfield; field++)
	{
		data = dc_AddMultScalar (dc, tag->nc_times + begin, sbegin, 
					 nsamp, fids[field], NULL);
	}
	if (! dc_SampleStride (dc, &sampsize))
	{
		msg_ELog (EF_PROBLEM, "read subplat: %s",
			  "How can scalar not have uniform sample size?");
		return (0);
	}
	imap[0] = sampsize;
	for (field = 0; field < nfield; field++)
	{
	/*
	 * Look up the field and try to pull in the data.
	 */
		fill = ((tag->nc_org == OrgIRGrid) && (spix < 0));
		fill = (fill || 
			(vfield = dnc_GetFieldVar (tag, fids[field])) < 0);
		if (!fill)
		{
		        int ndims;
			dnc_Slice (tag, vfield, start, count,
				   details, ndetail);
			data = dc_AddMultScalar (dc, tag->nc_times + begin, 
				sbegin, nsamp, fids[field], NULL);
			/*
			 * As a special case for fields without any dimensions,
			 * which includes special Zebra fields like lat, lon,
			 * and alt, replicate them across the time samples.
			 */
			nc_inq_varndims (tag->nc_id, vfield, &ndims);
			if (ncvargetg (tag->nc_id, vfield, start, count,
				       /*stride*/NULL, imap, data) < 0)
			{
				dnc_NCError ("Scalar read");
				fill = 1;
			}
			else if (ndims == 0)
			{
			    /* replicate the first value */
			    float f = *(float*)data;
			    int j;
			    for (j = 1; j < count[0]; ++j)
				*(float*)((char*)data + j*imap[0]) = f;
			}
		}
		if (fill)
			dc_AddScalarMissing (dc, tag->nc_times + begin, 
					     sbegin, nsamp, fids[field]);
	}
/*
 * Do something about locations.
 */
	if (ds_IsMobile (dc->dc_Platform))
	{
		Location *locs = dc_SetMLoc (dc, sbegin, nsamp, NULL);
		dnc_LoadLocation (tag, locs, begin, nsamp);
	}
	else if (spix >= 0)
		dc_SetStaticLoc (dc, tag->nc_locs + spix);
	else
		dc_SetStaticLoc (dc, &tag->nc_sloc);
	return (nsamp);
}





static int
dnc_ReadSubplat (dc, tag, begin, nsamp, start, count)
DataChunk *dc;
NCTag *tag;
long begin, nsamp;
long start[];
long count[];
/*
 * Retrieve scalar data from a subplatform of this file.
 */
{
	int spix = -1;	/* subplatform index */
	int subplatdim;

	if (dc->dc_Platform == tag->nc_plat)
	{
	/*
	 * If the platform is our base plat, then someone is probably doing
	 * something they didn't intend and should be warned about it.
	 */
		msg_ELog (EF_PROBLEM, "%s whole irgrid %s",
			  "Illegal scalar access:",
			  ds_PlatformName (dc->dc_Platform));
	}
	else if ((spix = dnc_SPIndex (tag, dc->dc_Platform)) < 0)
	{
	/*
	 * If we can't find this platform in our subplatform index, then
	 * we have to fill it as missing.
	 */
		msg_ELog (EF_PROBLEM, "Unknown plat %s",
			  ds_PlatformName (dc->dc_Platform));
	}
/*
 * Add our subplatform coords.  Subplatform is the second dimension if
 * we have a time dimension, otherwise first.
 */
	subplatdim = (tag->nc_haveTimeDim) ? 1 : 0;
	start[subplatdim] = spix;
	count[subplatdim] = 1;
	return (spix);
}




static int
dnc_AltIndex (tag, details, ndetail)
NCTag *tag;
dsDetail *details;
int ndetail;
{
	float closest;
	SValue v;
	int i;
	int altindex = -1;

	/*
	 * Determine the altitude index to read grids at
	 */
	if ((tag->nc_nalts > 0) &&
	    ds_GetDetail ("altitude", details, ndetail, &v))
	{
		altindex = 0;
		closest = fabs (tag->nc_alts[0] - v.us_v_float);
		for (i = 1; (closest) && (i < tag->nc_nalts); ++i)
		{
			if (fabs (tag->nc_alts[i] - v.us_v_float) < closest)
			{
				closest = fabs(tag->nc_alts[i] - v.us_v_float);
				altindex = i;
			}
		}
	}
	return (altindex);
}
	




static int
dnc_ReadNSpace (dc, tag, begin, nsamp, details, ndetail)
DataChunk *dc;
NCTag *tag;
long begin, nsamp;
dsDetail *details;
int ndetail;
/*
 * Retrieve multi-dimensional fields from this file's particular
 * organization and add the data to the NSpace datachunk.
 */
{
	int sbegin = dc_GetNSample (dc);

	/*
	 * Split up task based on platform organization.
	 */
	switch (tag->nc_org)
	{
	   case OrgScalar:
	   case OrgNSpace:
	        dnc_ReadNSpaceScalar (dc, tag, begin, nsamp, 
				      details, ndetail);
		break;
	   default:
		return (0);
	}
	/*
	 * Do something about locations.
	 */
	if (ds_IsMobile (dc->dc_Platform))
	{
		Location *locs = dc_SetMLoc (dc, sbegin, nsamp, NULL);
		dnc_LoadLocation (tag, locs, begin, nsamp);
	}
	else
		dc_SetStaticLoc (dc, &tag->nc_sloc);

	return (nsamp);
}




static void
dnc_ReadNSpaceScalar (dc, tag, begin, nsamp, details, ndetail)
DataChunk *dc;
NCTag *tag;
int begin, nsamp;
dsDetail *details;
int ndetail;
/*
 * Retrieve multi-dimensional scalar data from this file and add it to
 * the NSpace datachunk.
 */
{
	long start[ MAX_VAR_DIMS ];
	long count[ MAX_VAR_DIMS ];
	long imap[ MAX_VAR_DIMS ];
	unsigned long sizes[ DC_MaxDimension ];
	char *names[ DC_MaxDimension ];
	void *data;
	int elemsize;
	int field, i;
	int varid;
	int sbegin = dc_GetNSample (dc);
	int dim, ndim, is_static;
	int altindex, dindex;
	int nfield;
	int all_static;
	FieldId altid;
	FieldId *fids = dc_GetFields (dc, &nfield);

	altid = BadField;
	altindex = dnc_AltIndex (tag, details, ndetail);
	if (altindex >= 0)
		altid = F_Lookup ("altitude");
	/*
	 * Each field will have different hyperslab coordinates, depending
	 * on whether or not the field is static, and depending on the
	 * field's dimensions.  So we'll loop through the fields and handle
	 * each one entirely separately.
	 */
	all_static = 1; /* set to false if we have any dynamic fields */
	
	for (field = 0; field < nfield; ++field)
	{
		if ((varid = dnc_GetFieldVar (tag, fids[field])) < 0)
		{
			msg_ELog (EF_DEBUG, "netcdf: missing field %s %s", 
				  F_GetFullName (fids[field]), 
				  "will be filled with bad values");
		}
		/*
		 * All other limits get the whole slab, unless it's an
		 * altitude dimension and we're slicing at an altitude level.
		 */
		dc_NSGetField (dc, fids[field], &ndim, 
			       names, sizes, &is_static);

		all_static = all_static && is_static;

		if (is_static)
		{
		/*
		 * Don't need to check sample stride for static fields
		 */
			data = dc_NSAddStatic (dc, fids[field], NULL);
		}
		else
		{
			int sampsize = 0;
		/*
		 * Dynamic fields will need these coords for the 1st dimn
		 */
			data = dc_NSAddMultSamples (dc, tag->nc_times + begin,
					     sbegin, nsamp, fids[field], NULL);
			if (! dc_SampleStride (dc, &sampsize))
			{
				msg_ELog (EF_PROBLEM, "read nspace: %s",
					  "sample size not uniform?!");
				varid = -1;
			}
			imap[0] = sampsize;
		}
#ifdef DEBUG
		msg_ELog (EF_DEVELOP,
			  "reading%s field '%s', acquired data pointer: %x",
			  is_static ? " static" : "",
			  F_GetName (fids[field]), data);
#endif
		dim = 0;
		if (tag->nc_haveTimeDim && !is_static)
		{
		    dim = 1;
		    start[0] = begin;
		    count[0] = nsamp;
		}

		dim += (ndim-1);
		elemsize = dc_SizeOf (dc, fids[field]);
		for (i = ndim - 1; i >= 0; --i, --dim)
		{
			start[dim] = 0;
			count[dim] = sizes[i];
			if ((altindex >= 0) && 
			    (!strcmp (F_GetName(altid), names[i])))
			{
				start[dim] = altindex;
				count[dim] = 1;
			}
			else if (dc_NSFixedDimension (details, ndetail,
						      names[i], &dindex))
			{
				start[dim] = dindex;
				count[dim] = 1;
			}
			if (i == (ndim - 1))
				imap[dim] = elemsize;
			else
				imap[dim] = imap[dim+1] * count[dim+1];
		}
		msg_ELog (EF_DEVELOP, "ncvargetg(varid:%i,data:%x)",
			  varid, data);
		/*
		 * imap is only needed for non-static data interleaved
		 * across samples.
		 */
		if ((varid < 0) ||
		    ncvargetg (tag->nc_id, varid, start, count, 
			       /*stride*/NULL, is_static ? 0 : imap, data) < 0)
		{
			if (varid >= 0)
				msg_ELog (EF_PROBLEM, 
				  "dnc_ReadNSpace, error on field %s", 
				  F_GetFullName(fids[field]));
			if (! is_static)
				dc_NSAddMissing (dc, tag->nc_times + begin,
						 sbegin, nsamp, fids[field]);
			else
				dc_NSFillStatic (dc, fids[field]);
		}
	}
/*
 * Print a message if all fields are static, which will likely result
 * in a later "get failed" message...  This can help to clarify it.
 */
	if (all_static)
	    msg_ELog(EF_PROBLEM, "Fields requested for %s are all static; %s",
		     ds_PlatformName(dc->dc_Platform), 
		     "no time samples will be returned");
}






static int
dnc_ReadLocation (dc, tag, begin, nsamp)
DataChunk *dc;
NCTag *tag;
long begin, nsamp;
/*
 * Pull in a location-class data chunk.  If we're a static platform,
 * then they'll get our location repeated for every sample.
 */
{
	int i;
/*
 * Fill the location array based on whether we actually move or not.
 */
	if (ds_IsMobile (tag->nc_plat))
	{
		Location *locs;

		locs = dc_LocAddMany (dc, nsamp, tag->nc_times+begin, NULL);
		dnc_LoadLocation (tag, locs, begin, nsamp);
	}
	else
	{
		for (i = 0; i < nsamp; i++)
			dc_LocAdd (dc, tag->nc_times+begin+i, &tag->nc_sloc);
	}		
	return (nsamp);
}




static void
dnc_ReadIRGrid (dc, tag, begin, nsamp, details, ndetail)
DataChunk *dc;
NCTag *tag;
int begin, nsamp;
dsDetail *details;
int ndetail;
/*
 * Pull some number of IRGrids out of this file.
 */
{
	long start[MAX_VAR_DIMS], count[MAX_VAR_DIMS];
	void *grid;
	int platdim, sample, f, vfield, dsamp = dc_GetNSample (dc);
	int nfield;
	FieldId *fids = dc_GetFields (dc, &nfield);
/*
 * Coords.
 */
	if (tag->nc_haveTimeDim)
	{
	    start[0] = begin;
	    count[0] = 1;
	    platdim = 1;
	}
	else
	    platdim = 0;

	start[platdim] = 0;
	count[platdim] = tag->nc_nPlat;
/*
 * Plow through each desired sample.
 */
 	for (sample = 0; sample < nsamp; sample++)
	{
	/*
	 * Now do each field.
	 */
		for (f = 0; f < nfield; f++)
		{
		/*
		 * Look up the field and get the data from the file.  If
		 * either step fails, fill the array with bad value flags.
		 */
			grid = NULL;
			if ((vfield = dnc_GetFieldVar (tag, fids[f])) >= 0)
			{
				dnc_Slice (tag, vfield, start, count,
					   details, ndetail);
				grid = dc_IRAddGrid (dc, tag->nc_times + 
					     begin + sample, dsamp + sample, 
					     fids[f], NULL);
				if (grid && ncvarget (tag->nc_id, vfield, 
						      start, count, grid) < 0)
				{
					dnc_NCError ("Irgrid read");
					grid = NULL;
				}
			}
			if (! grid)
			{
				dc_IRAddMissing (dc, tag->nc_times + begin
				 + sample, dsamp + sample, 1, fids[f]);
			}
		}
		start[0]++;
	}
}




static void
dnc_ReadRGrid (dc, tag, begin, nsamp, dets, ndet)
DataChunk *dc;
NCTag *tag;
int begin, nsamp, ndet;
dsDetail *dets;
/*
 * Pull some number of RGrids out of this file.
 */
{
	long start[MAX_VAR_DIMS], count[MAX_VAR_DIMS];
	int sample, field, vfield, dsamp = dc_GetNSample (dc);
	int dim, xdim, ydim, zdim = 0;
	SValue v;
	Location origin;
	RGrid rg;
	DataPtr dp;
	int nfield;
	FieldId *fids = dc_GetFields (dc, &nfield);
/*
 * Make an initial set of coords that pull in the entire grid.
 */
	if (tag->nc_haveTimeDim)
	{
	    start[0] = begin;
	    count[0] = 1;
	    dim = 1;
	}
	else
	    dim = 0;
	
	switch (tag->nc_org)
	{
	   case Org3dGrid:
		zdim = dim++;
		start[zdim] = 0;
	   	count[zdim] = tag->nc_rgrid.rg_nZ;
	   case Org2dGrid:
		ydim = dim++;
		start[ydim] = 0;
	   	count[ydim] = tag->nc_rgrid.rg_nY;
	   case Org1dGrid:
		xdim = dim++;
		start[xdim] = 0;
	   	count[xdim] = tag->nc_rgrid.rg_nX;
		break;
	   default:
		/* suppress warnings */
		break;
	}
/*
 * Initialize rgrid info.
 */
	origin = tag->nc_sloc;
	rg = tag->nc_rgrid;
	dc_SetStaticLoc (dc, &tag->nc_sloc);
/*
 * If the file contains 3d grids, they may wish to subsection things.
 */
	if (tag->nc_org == Org3dGrid &&
	    ds_GetDetail (DD_FETCH_ALTITUDE, dets, ndet, &v))
	{
	/*
	 * Figure out what level we want.
	 */
		start[zdim] = (v.us_v_float - origin.l_alt) / rg.rg_Zspacing + 
		    0.5;
		if (start[zdim] < 0)
			start[zdim] = 0;
		else if (start[zdim] >= rg.rg_nZ)
			start[zdim] = rg.rg_nZ - 1;
		count[zdim] = 1;
	/*
	 * Update our info accordingly.
	 */
	 	rg.rg_nZ = 1;
		origin.l_alt += start[zdim]*rg.rg_Zspacing;
	}
/*
 * Plow through each desired sample.
 */
 	for (sample = 0; sample < nsamp; sample++)
	{
	/*
	 * Now do each field.
	 */
	 	for (field = 0; field < nfield; field++)
		{
		/*
		 * Add the field first (no data) then find out where it 
		 * got put.  Then we can read the data directly into the
		 * data chunk.
		 */
		 	dc_RGAddGrid (dc, dsamp + sample, fids[field], &origin,
				      &rg, tag->nc_times + begin + sample,
				      NULL, 0);
			dp = dc_GetMData (dc, dsamp + sample, fids[field], 0);
		/*
		 * Look up the field and try to read it in.
		 */
			if (((vfield = dnc_GetFieldVar (tag, fids[field])) < 0)
			    || (ncvarget (tag->nc_id, vfield, start, count, 
					  dp) < 0))
			{
			    	if (vfield >= 0) dnc_NCError ("Rgrid read");
				dc_RGAddMissing (dc, dsamp + sample, 
					 fids[field], &origin, &rg, 
					 tag->nc_times + begin + sample, 0);
			}
		}
		start[0]++;
	}
}




static int
dnc_GetFieldVar (tag, fid)
NCTag *tag;
FieldId fid;
/*
 * Find the netCDF variable corresponding to this field.
 */
{
	int var;

	for (var = 0; var < tag->nc_nVar; var++)
		if (tag->nc_FMap[var] == fid)
			return (var);
	return (-1);
}




static void
dnc_LoadLocation (tag, locs, begin, count)
NCTag *tag;
Location *locs;
long begin, count;
/*
 * Load in mobile platform location info.  If we're a static location,
 * then we return the same location for all of the samples, where the
 * location either comes from zero-dimensional lat, lon, alt fields
 * or from the very first samples of those fields.  We try to be more
 * flexible about finding appropriate location fields by using
 * dnc_MatchVarName.
 */
{
	int i, var;
	int mobile;
	int fail;
	int nlocs = count;
	long stride[1];
	long imap[1];

	stride[0] = 1;
	imap[0] = sizeof(Location);
	if (! (mobile = ds_IsMobile (tag->nc_plat)))
	{
		begin = 0;
		count = 1;
	}
/*
 * Just do it one piece at a time.  Latitude.
 */
	fail = 0;
	if ((var = dnc_MatchVarName(tag->nc_id, -1, "lat", "latitude", 0)) < 0)
	{
		msg_ELog (EF_DEBUG, "No match for 'lat' field");
		fail = 1;
	}
	else if (ncvargetg (tag->nc_id, var, &begin, &count, 
			    stride, imap, &(locs[0].l_lat)) < 0)
	{
		dnc_NCError ("Latitude read");
		fail = 1;
	}
	if (fail)
		locs[0].l_lat = 0.0;
	if (! mobile || fail)
	{
		for (i = 1; i < nlocs; i++)
			locs[i].l_lat = locs[0].l_lat;
	}
/*
 * Longitude.
 */
	fail = 0;
	if ((var = 
	     dnc_MatchVarName(tag->nc_id, -1, "lon", "longitude", 0)) < 0)
	{
		msg_ELog (EF_DEBUG, "No match for 'lon' field");
		fail = 1;
	}
	else if (ncvargetg (tag->nc_id, var, &begin, &count, 
			    stride, imap, &(locs[0].l_lon)) < 0)
	{
		dnc_NCError ("Longitude read");
		fail = 1;
	}
	if (fail)
		locs[0].l_lon = 0.0;
	if (! mobile || fail)
	{
		for (i = 1; i < nlocs; i++)
			locs[i].l_lon = locs[0].l_lon;
	}
/*
 * Altitude.
 */
	fail = 0;
	if ((var = dnc_MatchVarName(tag->nc_id, -1, 
				    "alt", "altitude", 0)) >= 0)
	{
	    if (ncvargetg (tag->nc_id, var, &begin, &count, 
			   stride, imap, &(locs[0].l_alt)) < 0) 
	    {
		dnc_NCError ("Altitude read");
		fail = 1;
	    }
	}
	else
	{
	    msg_ELog (EF_DEBUG, "No match for 'alt' field");
	    fail = 1;
	}

	if (fail)
		locs[0].l_alt = 0.0;

	if (! mobile || fail)
	{
		for (i = 1; i < nlocs; i++)
			locs[i].l_alt = locs[0].l_alt;
	}
}



static void
dnc_PutLocation (tag, start, count, pos)
NCTag *tag;
long start, count;
Location *pos;
/*
 * Write location info to a file.
 */
{
	int var;
	long imap[1];
	long stride[1];

	imap[0] = sizeof(Location);
	stride[0] = 1;

	if ((var = dnc_Varid (tag->nc_id, "lat")) < 0)
	{
		dnc_NCError ("No latitude");
	}
	else if (ncvarputg (tag->nc_id, var, &start, &count, 
			    stride, imap, &(pos[0].l_lat)) < 0)
	{
		dnc_NCError ("Latitude put");
	}
	if ((var = dnc_Varid (tag->nc_id, "lon")) < 0)
	{
		dnc_NCError ("No longitude");
	}
	else if (ncvarputg (tag->nc_id, var, &start, &count, 
			    stride, imap, &(pos[0].l_lon)) < 0)
	{
		dnc_NCError ("Longitude put");
	}
	if ((var = dnc_Varid (tag->nc_id, "alt")) < 0)
	{
		dnc_NCError ("No altitude");
	}
	else if (ncvarputg (tag->nc_id, var, &start, &count, 
			    stride, imap, &(pos[0].l_alt)) < 0)
	{
		dnc_NCError ("Altitude put");
	}
}		





static int 
dnc_VarAlts (ncid, varid, au)
int ncid;
int varid;
AltUnitType *au;
/*
 * Try to convert the units of this variable to an altitude unit type.
 * Return TRUE on success, FALSE otherwise.
 */
{
	char units[256];
	AltUnitType altunit;

	altunit = DEF_ALT_UNITS;
	if (dnc_GetStringAtt (ncid, varid, VATT_UNITS, 
			      units, sizeof(units)))
	{
		if (units[0] == '\0')
		{
			msg_ELog (EF_PROBLEM, 
				  "NetCDF: alt units empty, using default %s",
				  au_LongUnitsName(altunit));
		}
		else if (! au_ConvertName (units, &altunit))
		{
			msg_ELog (EF_PROBLEM,
				  "NetCDF: Unknown alt units: '%s'", units);
			return (FALSE);
		}
	}
	/*
	 * Either we got alts above, or the variable has no units, in 
	 * which case we return the default.
	 */
	*au = altunit;
	return (TRUE);
}




static int
dnc_GetAlts (ofp, fid, offset, alts, nalts, altunits)
OpenFile *ofp;
FieldId	fid;
int offset;
float *alts;
int *nalts;
AltUnitType *altunits;
/*
 * Return the altitude information from this file for the given fid.
 */
{
	NCTag *tag = TAGP (ofp);
	int i;
	int ret;
/*
 * Let orgs do their own work.
 */
	switch (tag->nc_org)
	{
	   case OrgNSpace:
		ret = dnc_GetNSpaceAlts (tag, fid, offset, 0);
		break;
	   case OrgIRGrid:
		ret = dnc_GetIRGridAlts (tag, fid, offset);
		break;
	   case Org3dGrid:
	   case Org2dGrid:
	   case Org1dGrid:
		ret = dnc_GetRGridAlts (tag, fid, offset, 0);
		break;
	   default:
		return (FALSE);
	}
/*
 * Now return whatever we were asked for
 */
	if (nalts)
		*nalts = tag->nc_nalts;
	if (altunits)
		*altunits = tag->nc_altUnits;
	if (alts)
		for (i = 0; i < tag->nc_nalts; i++)
			alts[i] = tag->nc_alts[i];
	return (TRUE);
}



static int
dnc_GetIRGridAlts (tag, fid, offset)
NCTag *tag;
FieldId fid;
int offset;
/*
 * Find some altitudes for this field at this forecast offset
 */
{
	int varid, altdimn, altvar;
	int ndims;
	int dimids[ MAX_VAR_DIMS ];
	long start[ MAX_VAR_DIMS ];
	long count[ MAX_VAR_DIMS ];
	char altname[ MAX_NC_NAME ];
	long nalt;
	AltUnitType au;
	nc_type vtype;
	float *alts;
	int i;
/*
 * If we already have altitudes for this field, we're cool
 */
	if (fid == tag->nc_altvar)
		return (TRUE);
/*
 * Free what's there and try to find some new ones, since they depend
 * upon the field being looked up.
 */
	if (tag->nc_alts)
	{
		free (tag->nc_alts);
		tag->nc_alts = NULL;
		tag->nc_nalts = 0;
		tag->nc_altUnits = DEF_ALT_UNITS;
		tag->nc_altvar = BadField;
	}
/*
 * Now try to build an array of altitudes for this field.  Find the
 * variable for this field, and check its dimensions for one which
 * implies altitudes.  If that dimension has a coordinate variable,
 * use it to fill the alts.  Otherwise return the indices of the
 * dimension as levels.
 */
	if ((varid = dnc_GetFieldVar (tag, fid)) < 0)
	{
		msg_ELog (EF_PROBLEM, "GetAlts on field %s not in file",
			  F_GetName (fid));
		return (FALSE);
	}
	if (ncvarinq (tag->nc_id, varid, NULL,
		      &vtype, &ndims, dimids, NULL) < 0)
	{
		dnc_NCError ("inquiring variable for get alts");
		return (FALSE);
	}
/*
 * Since we're an irgrid, use the dimension after time and station, if any.
 */
	if (ndims > 2)
	{
		altdimn = dimids[2];
	}
	else
	{
		msg_ELog (EF_DEBUG, "netcdf irgrid: no altitudes in file");
		return (FALSE);
	}
/*
 * Now find the name of this dimension and see if it has a coordinate
 * variable.
 */	
	if (ncdiminq (tag->nc_id, altdimn, altname, &nalt) < 0)
	{
		dnc_NCError ("inquiring dimns for get alts variable");
		return (FALSE);
	}
	if ((altvar = dnc_Varid (tag->nc_id, altname)) >= 0)
	{
		if (ncvarinq (tag->nc_id, altvar, NULL,
			      &vtype, &ndims, dimids, NULL) < 0)
		{
			dnc_NCError ("inquiring alt coord variable");
			return (FALSE);
		}
		else if (vtype != NC_FLOAT)
		{
			dnc_NCError ("expected float alt variable");
			return (FALSE);
		}
		/* use the variable values */
		msg_ELog (EF_DEBUG, 
			  "found coordinate variable %s for alts", altname);
		if (! dnc_VarAlts (tag->nc_id, altvar, &au))
			return (FALSE);
	}
	else	/* use the indices */
	{
		msg_ELog (EF_PROBLEM, "%s: no coord variable, using indices",
			  "netcdf altitudes");
		au = AU_level;
	}

	alts = (float *) malloc (sizeof(float) * nalt);
	if (altvar >= 0)
	{
		for (i = 0; i < ndims; ++i)
		{
			start[i] = 0;
			if (dimids[i] == altdimn)
				count[i] = nalt;
			else
				count[i] = 1;
		}
		if (ncvarget (tag->nc_id, altvar, start, 
			      count, (void *)alts) < 0)
		{
			dnc_NCError ("reading altitude variable");
			free (alts);
			return (FALSE);
		}
	}
	else
	{
		for (i = 0; i < nalt; ++i)
			alts[i] = (float) i;
	}

	tag->nc_nalts = nalt;
	tag->nc_altUnits = au;
	tag->nc_alts = alts;
	return (TRUE);
}




static int
dnc_CreateFile (ofp, dc, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
dsDetail *details;
int ndetail;
/*
 * This is the hairy routine wherein we try to create properly data files for
 * all of the organizations we know.
 */
{
	NCTag *tag = TAGP (ofp);
	int ndim, dims[6], vbase;
	ZebTime t;
	nc_type time_type;
	char full_time[50];
	char *fname = ofp->of_df.df_fullname;
	int year, month, day, hour, minute, second;
	int flags;
	const Platform *p = dt_FindPlatform (dc->dc_Platform);

#	define BT_LONGNAME "Base time in Epoch"
#	define BT_UNITS	   "seconds since 1970-1-1 0:00:00 0:00"
#	define OT_LONGNAME "Time offset from base_time"
#	define TIME_LONGNAME "Time"
/*
 * We might as well start by creating the actual file.  After all,
 * that, at least, is common to all of the organizations.
 */
	flags = (NC_CLOBBER | NC_NOFILL);
	if ((tag->nc_id = nccreate (fname, flags)) < 0)
	{
		dnc_NCError ("File create");
		return (FALSE);
	}
/*
 * Check for details which we handle here
 */
	if (ds_GetDetail(DD_NC_TIME_DOUBLE, details, ndetail, NULL))
		time_type = NC_DOUBLE;
	else if (ds_GetDetail(DD_NC_TIME_FLOAT, details, ndetail, NULL))
		time_type = NC_FLOAT;
	else if (ds_GetDetail(DD_NC_TIME_LONG, details, ndetail, NULL))
		time_type = NC_LONG;
	else
		time_type = NC_DOUBLE;
/*
 * Fill in some basic tag info.
 */
	tag->nc_times = (ZebTime *) 0;
	tag->nc_ntime = tag->nc_nrec = 0;
	tag->nc_timeType = time_type;
	tag->nc_locs = (Location *) 0;
	tag->nc_altUnits = dc_GetLocAltUnits (dc);
	tag->nc_alts = NULL;
	tag->nc_nalts = 0;
/*
 * We let the platform tell us what kind of file this is.  Use the parent
 * platform if the requested one is a subplatform.
 */
	while (pi_Subplatform (p))
	    p = pi_Parent (p);
	
	tag->nc_org = pi_DataOrg (p);
	tag->nc_plat = pi_Id (p);
/*
 * Create the time dimension.  If this platform has the "discrete"
 * flag set, or it's an IRGRID organization, then we make time
 * unlimited.  Otherwise we wire time to the maxsample value, in
 * hopes of getting better performance out of large, scalar data.
 * 
 * XXX WIRE IT UNLIMITED FOR NOW, UNTIL WE FIGURE OUT HOW TO KEEP TRACK
 * OF HOW MANY SAMPLES ARE ACTUALLY WRITTEN.
 *
 * Keeping time on the record dimension allows us to use the ncrec* 
 * routines to read and write consecutive records...
 */
	tag->nc_dTime = ncdimdef (tag->nc_id, "time", NC_UNLIMITED);
	tag->nc_haveTimeDim = TRUE;
/*
 * Create the other dimensions that we need for variables.
 */
	ndim = 1;
	dims[0] = tag->nc_dTime;
	dnc_CFMakeDims (tag, dc, &ndim, dims, details, ndetail);
/*
 * Make the time variables.  The time_offset field is now stored as a double
 * so we have sufficient precision to represent reasonable time offsets down
 * to the microsecond.  The DD_NC_ONE_TIME detail tells us to use a single
 * 'time' variable and no 'base_time'.  Put the base time in the units 
 * attribute of 'time'.
 */
	dc_GetTime (dc, 0, &t);
	tag->nc_base = t.zt_Sec;
	t.zt_MicroSec = 0;
	if (1 || ds_GetDetail (DD_NC_ONE_TIME, details, ndetail, NULL))
	{
	  tag->nc_vTime = ncvardef (tag->nc_id, "time",
				    tag->nc_timeType, 1, &tag->nc_dTime);
	  ncattput (tag->nc_id, tag->nc_vTime, VATT_LONGNAME, NC_CHAR,
		    strlen(TIME_LONGNAME)+1, TIME_LONGNAME);
	}
	else
	{
	  tag->nc_vTime = ncvardef (tag->nc_id, "time_offset",
				    tag->nc_timeType, 1, &tag->nc_dTime);
	  ncattput (tag->nc_id, tag->nc_vTime, VATT_LONGNAME, NC_CHAR,
		    strlen(OT_LONGNAME)+1, OT_LONGNAME);
	}
	/* define base_time no matter the time variable */
	{
	  vbase = ncvardef (tag->nc_id, "base_time", NC_LONG, 0, 0);
	  TC_EncodeTime (&t, TC_Full, full_time);
	  strcat (full_time, " GMT");
	  ncattput (tag->nc_id, vbase, "string", NC_CHAR,
		    strlen(full_time)+1, full_time);
	  ncattput (tag->nc_id, vbase, VATT_LONGNAME, NC_CHAR,
		    strlen(BT_LONGNAME)+1, BT_LONGNAME);
	  ncattput (tag->nc_id, vbase, VATT_UNITS, NC_CHAR,
		    strlen(BT_UNITS)+1, BT_UNITS);
	}
	TC_ZtSplit (&t, &year, &month, &day, &hour, &minute, &second, 0);
	sprintf (full_time, 
		 "seconds since %4i-%02i-%02i %02i:%02i:%02i 0:00",
		 year, month, day, hour, minute, second);
	ncattput (tag->nc_id, tag->nc_vTime, VATT_UNITS, NC_CHAR,
		  strlen(full_time)+1, full_time);

/*
 * If the DC is NSpace, we have some dimensions to define first
 */
	if (dc->dc_Class == DCC_NSpace)
		dnc_DefineNSpaceDims(tag, dc, details, ndetail);
	dnc_DefineVars(tag, dc, ndim, dims);
	dnc_PutGlobalAttributes(tag, dc);
/*
 * Create the organization-specific variables.  Since some of these
 * need to be initialized here, this routine also takes us out of
 * definition mode, so we can put in the base time thereafter.
 */
	dnc_CFMakeVars (tag, dc);
	dnc_LoadFields (tag);
	ncvarput1 (tag->nc_id, vbase, 0, &tag->nc_base);
	ncsync (tag->nc_id);
	return (TRUE);
}



static void 
dnc_DefineVars (tag, dc, ndim, dims)
NCTag *tag;
DataChunk *dc;
int ndim;
int *dims;
/*
 * Create the actual fields that we are storing here, and while we're at
 * it supply the field attributes from the data chunk.
 */
{
	void *badval;
	FieldId *fids;
	int nfield;
	char *attr;
	int var;
	long varid;
	struct AttArg attarg;

	fids = dc_GetFields (dc, &nfield);
	for (var = 0; var < nfield; var++)
	{
		nc_type type = dnc_NCType (dc_Type(dc, fids[var]));
	/*
	 * NSpace fields may have dimensions in addition to the base
	 * dimensions, and static fields will have only their NSpace
	 * dimensions.
	 */
		if (dc->dc_Class == DCC_NSpace)
		{
			varid = dnc_DefineNSpaceVar(tag, dc, fids[var],
						    ndim, dims);
		}
		else
		{
			varid = ncvardef (tag->nc_id, F_GetName (fids[var]),
					  type, ndim, dims);
		}
	/*
	 * Add the conventional attributes first.  If these attributes have
	 * been explicitly set in the datachunk, then the datachunk will
	 * take precedence in dnc_PutAttribute().
	 */
		attr = F_GetDesc(fids[var]);
		(void) ncattput (tag->nc_id, varid, VATT_LONGNAME,
				NC_CHAR, strlen(attr)+1, attr);
		attr = F_GetUnits(fids[var]);
		(void) ncattput (tag->nc_id, varid, VATT_UNITS,
				NC_CHAR, strlen(attr)+1, attr);
		attr = F_GetTypeName(fids[var]);
		(void) ncattput (tag->nc_id, varid, VATT_FTYPE,
				NC_CHAR, strlen(attr)+1, attr);
	/*
	 * NetCDF has no explicit unsigned byte type, so we add an
	 * "is_signed" attribute for our byte data.
	 */
		DC_ElemType vartype = dc_Type (dc, fids[var]);
		if (vartype == DCT_Char || vartype == DCT_UnsignedChar)
		{
		  int is_signed = (vartype == DCT_Char) ? 1 : 0;
		  (void) ncattput (tag->nc_id, varid, VATT_IS_SIGNED, 
				   NC_INT, 1, &is_signed);
		}
	/*
	 * Add the missing_value attribute for this field, but only if the
	 * attribute has not already been explicitly set in the datachunk.
	 * dc_FindFieldBadval() will return the field-specific bad value,
	 * if any, otherwise the global bad value, if any.  In Zebra's
	 * interpretation of a global missing_value, fields without a
	 * setting inherit the global value.  The netcdf convention is that
	 * all variables have their own missing_value attribute, so in
	 * effect we're explicitly doing the inheritance here to conform to
	 * convention.  When read, Zebra will just accept the per-field
	 * missing values, even though they may all be the same.
	 **/
		if ((badval = dc_FindFieldBadval (dc, fids[var])) &&
		    (! dc_GetFieldAttrArray (dc, fids[var], VATT_MISSING,
					     NULL, NULL)))
		{
			(void) ncattput (tag->nc_id, varid, VATT_MISSING,
					 dnc_NCType (dc_Type (dc, fids[var])),
					 1, badval);
		}
	/* 
	 * Add the field attributes from the DataChunk,
	 * passing the tag and varid via global variables.  
	 */
		attarg.tag = tag;
		attarg.varid = varid;
		dc_ProcFieldAttrArrays(dc, fids[var], NULL, 
				       dnc_PutAttribute, (void *)&attarg);
	}
}



static void
dnc_DefineNSpaceDims(tag, dc, details, ndetail)
NCTag *tag;
DataChunk *dc;
dsDetail *details;
int ndetail;
{
	char *names[ DC_MaxDimension ];
	unsigned long sizes[ DC_MaxDimension ];
	int i, ndim;

	ndim = dc_NSGetAllDimensions (dc, names, /*dimn ids*/NULL, sizes);

	/* Allow the fixed dimension detail to specify the length instead
	 * of using the dimension size from this datachunk, in case larger
	 * dimensions are coming.
	 */
	for (i = 0; i < ndim; ++i)
	{
	  dnc_defineDimension(tag, names[i], sizes[i], details, ndetail);
	}
}



static int
dnc_DefineNSpaceVar(tag, dc, fid, ndim, dims)
NCTag *tag;
DataChunk *dc;
FieldId fid;
int ndim;
int *dims;
{
	int alldims[ MAX_VAR_DIMS ];
	int nalldim, nfdim;
	nc_type type;
	int i;
	char *names[ DC_MaxDimension ];
/*
 * Copy whatever dimensions we'll need from what we've been passed in dims[].
 * In the case of static fields, we don't need any of them.
 */
	nalldim = 0;
	if (! dc_NSIsStatic (dc, fid))
	{
		for (i = 0; i < ndim; ++i)
			alldims[i] = dims[i];
		nalldim = ndim;
	}
/*
 * Add on the dimensions from the NSpace chunk for this field.
 */
	dc_NSGetField (dc, fid, &nfdim, names, /*sizes*/NULL, /*static*/NULL);
	for (i = 0; i < nfdim; ++nalldim, ++i)
	{
		alldims[ nalldim ] = ncdimid (tag->nc_id, names[i]);
	}

	type = dnc_NCType ( dc_Type(dc, fid) );
	return (ncvardef (tag->nc_id, F_GetName (fid),
			  type, nalldim, alldims));
}



static void
dnc_PutGlobalAttributes(tag, dc)
NCTag *tag;
DataChunk *dc;
/*
 * Add global attributes from the data chunk and create the global
 * 'history' attribute.  Presently, this only happens when a file is
 * created.  Any additional datachunks' global attributes will not be
 * written.  XXX What would the behavior be if the same global attribute
 * was given different values in different datachunks?
 */
{
	const char *attr;
	char* history = 0;
	struct timeval tv;
	struct AttArg attarg;
	void *badval;
	DC_ElemType type;
	ZebTime now;

	attarg.tag = tag;
	attarg.varid = NC_GLOBAL;
	dc_ProcessAttrArrays (dc, NULL, dnc_PutAttribute, (void *)&attarg);

	if (TestTimeUnits)
	{
		/*
		 * Skip time- and platform- specific info so that files can
		 * be compared for testing.
		 */
		return;
	}
	/* 
	 * Pull out the global bad value, if any, and store it as a
	 * global attribute.
	 */
	if ((badval = dc_GetGlobalBadval (dc, &type)))
	{
		ncattput (tag->nc_id, NC_GLOBAL, VATT_MISSING,
			  dnc_NCType (type), 1, badval);
	}
	attr = ds_PlatformName(dc->dc_Platform);
	(void)ncattput(tag->nc_id, NC_GLOBAL, GATT_PLATFORM,
		       NC_CHAR, strlen(attr)+1, attr);

	attr = dc_GetGlobalAttr (dc, GATT_HISTORY);
	if (attr)
	{
	    history = (char*) malloc (strlen(attr) + 256);
	    strcpy (history, attr);
	}
	else
	{
	    history = (char*) malloc (256);
	    history[0] = '\0';
	}
	strcat (history, "Created by the Zebra DataStore library, ");
	(void)gettimeofday(&tv, NULL);
	now.zt_Sec = tv.tv_sec;
	now.zt_MicroSec = tv.tv_usec;
	TC_EncodeTime(&now, TC_Full, history+strlen(history));
	strcat(history,", $RCSfile: DFA_NetCDF.c,v $ $Revision: 3.90 $\n");
	(void)ncattput(tag->nc_id, NC_GLOBAL, GATT_HISTORY,
		       NC_CHAR, strlen(history)+1, history);
	free (history);
}




static int
dnc_PutAttribute(key, value, nval, type, arg)
char *key;
void *value;
int nval;
DC_ElemType type;
void *arg;
/*
 * Add this datachunk attribute to the file's attributes for VarId
 */
{
	struct AttArg *attarg = (struct AttArg *)arg;

	if (type == DCT_String)
		(void)ncattput(attarg->tag->nc_id, attarg->varid,
			       key, NC_CHAR, strlen(value)+1, value);
	else
		(void)ncattput(attarg->tag->nc_id, attarg->varid,
			       key, dnc_NCType(type), nval, value);
	return(0);
}



static int
dnc_defineDimension (NCTag* tag, char* name, int len,
		     dsDetail* details, int ndetail)
{
  int fixedlength;
  if (dc_NSFixedDimension (details, ndetail, name, &fixedlength))
  {
    len = fixedlength;
    msg_ELog (EF_DEBUG, "fixing dimn %s length at %i", name, len);
  }
  return ncdimdef (tag->nc_id, name, len);
}


static void
dnc_CFMakeDims (tag, dc, ndim, dims, details, ndetail)
NCTag *tag;
DataChunk *dc;
int *ndim, *dims;
dsDetail *details;
int ndetail;
/*
 * Create the dimensions for this file organization.
 */
{
	RGrid rg;

	switch (tag->nc_org)
	{
	/*
	 * Scalar and nspace files are easy -- we're done!
	 */
	   case OrgScalar:
	   case OrgNSpace:
		break;
	/*
	 * Regular grid files need to have the grid dimensions
	 * defined.
	 */
	   case Org3dGrid:
	   case Org2dGrid:
	   case Org1dGrid:
	   	dc_RGGeometry (dc, 0, 0, &rg);
		if (tag->nc_org == Org3dGrid)
		{
		  dims[(*ndim)++] = 
		    dnc_defineDimension(tag, "z", rg.rg_nZ, details, ndetail);
		}
		if (tag->nc_org == Org3dGrid || tag->nc_org == Org2dGrid)
		{
		  dims[(*ndim)++] = 
		    dnc_defineDimension(tag, "y", rg.rg_nY, details, ndetail);
		}
		dims[(*ndim)++] = 
		  dnc_defineDimension(tag, "x", rg.rg_nX, details, ndetail);
		break;
	/*
	 * IRGrids are funky.
	 */
	   case OrgIRGrid:
		tag->nc_nPlat = dc_IRGetNPlatform (dc);
		tag->nc_locs = (Location *) malloc (tag->nc_nPlat *
						   sizeof(Location));
		dims[(*ndim)++] = ncdimdef (tag->nc_id, "platform",
					   tag->nc_nPlat);
		(void) ncdimdef (tag->nc_id, "fldlen", 20);
		break;
	   default:
		/* do nothing */
		break;
	}
}





static void
dnc_CFMakeVars (tag, dc)
NCTag *tag;
DataChunk *dc;
/*
 * Make the organization-specific variables.
 */
{
/*
 * Just farm this obnoxious stuff out, depending on the organization.
 */
	switch (tag->nc_org)
	{
	/*
	 * For scalar data, we need location info, which, in turn, is
	 * different depending on whether we have a mobile platform
	 * or not.
	 */
	   case OrgScalar:
	   case OrgNSpace:
		dnc_CFScalarVars (tag, dc);
		break;
	/*
	 * Grids have origin and spacing info.
	 */
	   case Org1dGrid:
	   case Org2dGrid:
	   case Org3dGrid:
		dnc_CFGridVars (tag, dc);
		break;
	/*
	 * Irregular grids have all that funky platform information.
	 */
	   case OrgIRGrid:
		dnc_CFIRGridVars (tag, dc);
		break;
	/*
	 * Other orgs are not handled or need no more variables
	 */
	   default:
		break;
	}
}





static void
dnc_CFScalarVars (tag, dc)
NCTag *tag;
DataChunk *dc;
/*
 * Create the variables for a scalar organization file.
 */
{
	int vlat, vlon, valt, ndim = 0, tdim = tag->nc_dTime;
	Location loc;
/*
 * If this is a static platform, then our position info is also
 * static. Otherwise it is indexed by our time variable.
 */
	if (ds_IsMobile (dc->dc_Platform))
		ndim = 1;
	dnc_DefineLocVars (tag, dc, ndim, &tdim, &vlat, &vlon, &valt);
/*
 * If we are static, initialize the location info now.
 */
	ncendef (tag->nc_id);
	if (!ds_IsMobile (dc->dc_Platform))
	{
		dc_GetLoc (dc, 0, &loc);
		ncvarput1 (tag->nc_id, vlat, 0, &loc.l_lat);
		ncvarput1 (tag->nc_id, vlon, 0, &loc.l_lon);
		ncvarput1 (tag->nc_id, valt, 0, &loc.l_alt);
		tag->nc_sloc = loc;
	}
}




static void
dnc_CFGridVars (tag, dc)
NCTag *tag;
DataChunk *dc;
/*
 * Get the grid-specific variables set up.  For now, we make the questionable
 * assumption that platforms returning grids are not mobile.  We'll get away
 * with it, I think, until ELDORA comes on line.
 */
{
	int vlat, vlon, valt, vx, vy = -1, vz = -1;
	const char *units;
	Location loc;

	dc_RGGeometry (dc, 0, &loc, &tag->nc_rgrid);
/*
 * Create variables for the origin and spacing information.
 * Add a description and units for each variable defined.
 */
	dnc_DefineLocVars (tag, dc, 0, 0, &vlat, &vlon, &valt);
	vx = ncvardef (tag->nc_id, "x_spacing", NC_FLOAT, 0, 0);
	(void)ncattput(tag->nc_id, vx, VATT_LONGNAME,
		       NC_CHAR, strlen(X_LONGNAME)+1, X_LONGNAME);
	(void)ncattput(tag->nc_id, vx, VATT_UNITS, NC_CHAR, 
		       strlen(X_UNITS)+1, X_UNITS);
	if ((tag->nc_org == Org2dGrid) || (tag->nc_org == Org3dGrid))
	{
		vy = ncvardef(tag->nc_id, "y_spacing", NC_FLOAT, 0, 0);
		(void)ncattput(tag->nc_id, vy, VATT_LONGNAME,
			       NC_CHAR, strlen(Y_LONGNAME)+1, Y_LONGNAME);
		(void)ncattput(tag->nc_id, vy, VATT_UNITS, NC_CHAR, 
			       strlen(Y_UNITS)+1, Y_UNITS);
		if (tag->nc_org == Org3dGrid)
		{
			vz = ncvardef(tag->nc_id, "z_spacing", NC_FLOAT, 0, 0);
			(void)ncattput(tag->nc_id, vz, VATT_LONGNAME, NC_CHAR,
				       strlen(Z_LONGNAME)+1, Z_LONGNAME);
			units = au_LongUnitsName (tag->nc_altUnits);
			(void) ncattput (tag->nc_id, vz, VATT_UNITS, NC_CHAR, 
					 strlen (units) + 1, units);
		}
	}
/*
 * Now, out of definition mode and initialize all of those variables.
 */
	ncendef (tag->nc_id);
	ncvarput1 (tag->nc_id, vlat, 0, &loc.l_lat);
	ncvarput1 (tag->nc_id, vlon, 0, &loc.l_lon);
	ncvarput1 (tag->nc_id, valt, 0, &loc.l_alt);
	tag->nc_sloc = loc;
	ncvarput1 (tag->nc_id, vx, 0, &tag->nc_rgrid.rg_Xspacing);
	if ((tag->nc_org == Org2dGrid) || (tag->nc_org == Org3dGrid))
	{
		ncvarput1(tag->nc_id, vy, 0, &tag->nc_rgrid.rg_Yspacing);
		if (tag->nc_org == Org3dGrid)
			ncvarput1 (tag->nc_id, vz, 0,
						&tag->nc_rgrid.rg_Zspacing);
	}
}





static void
dnc_CFIRGridVars (tag, dc)
NCTag *tag;
DataChunk *dc;
/*
 * Make the IRGrid variables.
 */
{
	int dims[2], vplat, vlat, vlon, valt;
	long start[2], count[2], plat;
	const char *name, *subname;
	PlatformId *plats;
/*
 * Look up a couple of dimensions that we have already made, then
 * create the variables to hold the platform names and locations.
 */
	dims[0] = ncdimid (tag->nc_id, "platform");
	dims[1] = ncdimid (tag->nc_id, "fldlen");
	vplat = ncvardef (tag->nc_id, "platform", NC_CHAR, 2, dims);
	dnc_DefineLocVars (tag, dc, 1, dims, &vlat, &vlon, &valt);
/*
 * Pull the actual platform info out of the dc.
 */
	plats = (PlatformId *) malloc (tag->nc_nPlat * sizeof (PlatformId));
	dc_IRGetPlatforms (dc, plats, tag->nc_locs);
/*
 * Store the information for each platform.
 */
	ncendef (tag->nc_id);
	for (plat = 0; plat < tag->nc_nPlat; plat++)
	{
	/*
	 * Find and store the name of this platform.  Trim off any
	 * leading path components, leaving just the subplatform name
	 * part.
	 */
		name = ds_PlatformName (plats[plat]);
		if ((subname = strrchr(name, '/')) == 0)
			subname = name;
		else
			subname++;	/* Go past / */
		start[0] = plat;
		count[0] = 1;
		start[1] = 0;
		count[1] = strlen (subname) + 1;
		ncvarput (tag->nc_id, vplat, start, count, subname);
	/*
	 * Deal with the location info too.
	 */
		ncvarput1 (tag->nc_id, vlat, &plat, &tag->nc_locs[plat].l_lat);
		ncvarput1 (tag->nc_id, vlon, &plat, &tag->nc_locs[plat].l_lon);
		ncvarput1 (tag->nc_id, valt, &plat, &tag->nc_locs[plat].l_alt);
	}
/*
 * Sigh.  We really ought to put this into the tag structure while we
 * are at it.
 */
	tag->nc_subplats = plats;
	dnc_SPBuildIndex (tag);
}



static void
dnc_DefineLocVars (NCTag *tag, DataChunk *dc, 
		   int ndims, int *dims, int *vlat, int *vlon, int *valt)
/*
 * Define the lat, lon, and alt variables with the given dimensions,
 * adding the appropriate attributes as well.  Return the ids given to each.
 */
{
	float lat_range[2];
	float lon_range[2];
	char buf[256];
	float badval;
	int usebadval;

	/*
	 * Since locations are specially handled by datachunks, they do not
	 * have field-specific attributes, hence no field badvalues.  So to
	 * set the missing_value for a location variable, we must expect
	 * the global bad value to apply to the location variable. However,
	 * to show some restraint and to follow the practice of regular
	 * variables, we'll require a global bad value to have been set
	 * explicitly rather than defaulted.  Unfortunately, when reading
	 * this file Zebra will ignore the missing_value for the location
	 * variables.
	 **/
	badval = dc_GetBadval (dc);
	usebadval = (dc_GetGlobalBadval (dc, 0) != NULL);

	*vlat = ncvardef (tag->nc_id, "lat", NC_FLOAT, ndims, dims);
	lat_range[0] = -90.0;
	lat_range[1] = 90.0;
	(void)ncattput(tag->nc_id, *vlat, VATT_LONGNAME,
		       NC_CHAR, strlen(LAT_LONGNAME)+1, LAT_LONGNAME);
	(void)ncattput(tag->nc_id, *vlat, VATT_UNITS, NC_CHAR, 
		       strlen(LAT_UNITS)+1, LAT_UNITS);
	(void)ncattput(tag->nc_id, *vlat, "valid_range", NC_FLOAT,
		       2, lat_range);
	if (usebadval)
	{
	    (void) ncattput (tag->nc_id, *vlat, VATT_MISSING,
			     NC_FLOAT, 1, &badval);
	}

	*vlon = ncvardef (tag->nc_id, "lon", NC_FLOAT, ndims, dims);
	lon_range[0] = -180.0;
	lon_range[1] = 180.0;
	(void)ncattput(tag->nc_id, *vlon, VATT_LONGNAME,
		       NC_CHAR, strlen(LON_LONGNAME)+1, LON_LONGNAME);
	(void)ncattput(tag->nc_id, *vlon, VATT_UNITS, NC_CHAR, 
		       strlen(LON_UNITS)+1, LON_UNITS);
	(void)ncattput(tag->nc_id, *vlon, "valid_range", NC_FLOAT,
		       2, lon_range);
	if (usebadval)
	{
	    (void) ncattput (tag->nc_id, *vlon, VATT_MISSING,
			     NC_FLOAT, 1, &badval);
	}

	*valt = ncvardef (tag->nc_id, "alt", NC_FLOAT, ndims, dims);
	/*
	 * As a special case for MSL altitude units, the MSL part is left
	 * off since that is the default for systems like VisAD as well as
	 * for Zebra.  So the units will still be interpreted correctly by
	 * Zebra, and the units attribute will be correct for packages
	 * which expect udunits syntax, which cannot distinguish different
	 * altitude dimensions like AGL and MSL.  The full units name,
	 * including the MSL and AGL parts, will be included in the
	 * long_name attribute so there is still some indication of the
	 * dimension.
	 */
	strcpy (buf, "Altitude");
	if (tag->nc_altUnits != AU_unknown)
	    sprintf (buf, "Altitude (%s)", au_LongUnitsName(tag->nc_altUnits));
	(void)ncattput(tag->nc_id, *valt, VATT_LONGNAME,
		       NC_CHAR, strlen(buf)+1, buf);
	switch (tag->nc_altUnits)
	{
	case AU_kmMSL:
	    strcpy (buf, "kilometers");
	    break;
	case AU_mMSL:
	    strcpy (buf, "meters");
	    break;
	default:
	    strcpy (buf, au_LongUnitsName (tag->nc_altUnits));
	    break;
	}
	(void)ncattput(tag->nc_id, *valt, VATT_UNITS, NC_CHAR, 
		       strlen (buf) + 1, buf);
	if (usebadval)
	{
	    (void) ncattput (tag->nc_id, *valt, VATT_MISSING,
			     NC_FLOAT, 1, &badval);
	}
}





static int
dnc_NSpaceVarPut (tag, dc, varid, fid, start, count, ndim, data)
NCTag *tag;
DataChunk *dc;
int varid;
FieldId fid;
long *start, *count;
int ndim;
DataPtr data;
{
	int nsdim, is_static;
	unsigned long sizes[ DC_MaxDimension ];
	int i;

	/*
	 * Add NSpace dimensions onto the start and count arrays,
	 * which are assumed to have enough space for them
	 */
	dc_NSGetField (dc, fid, &nsdim, NULL, sizes, &is_static);

	for (i = 0; i < nsdim; ++i)
	{
		start[ndim + i] = 0;
		count[ndim + i] = sizes[i];
	}
	if (is_static)
		return (ncvarput (tag->nc_id, varid, 
				  &(start[ndim]), &(count[ndim]), data));
	else
		return (ncvarput (tag->nc_id, varid, 
				  start, count, data));
}



static int
dnc_FindTimes (ofp, dc, sample, count, wc, start)
OpenFile *ofp;
DataChunk *dc;
int sample, count;
WriteCode wc;
long *start;
/*
 * Write a block of sample times to the tag->nc_times array and to the file
 * identified by tag->nc_id.  Returns the starting indices of the samples
 * in 'start'.  Returns 0 if failure, non-zero if success.  Append and
 * overwrite cases are supported; overwrite assumes that the sample times
 * in the block (even if only 1 sample) already coincide exactly with the
 * times in the file.  Insertion requires a lot more juggling.
 *
 * This function replaces the original dnc_FindDest(), which relied on 
 * single-sample changes.
 */
{
	NCTag *tag = TAGP (ofp);
	int status;
	double *dtime = NULL;
	float *ftime = NULL;
	int *ltime = NULL;
	unsigned long i;
	ZebTime t;
	unsigned long nsample = (unsigned long)count;
/*
 * It all depends on the write code.
 */
	switch (wc)
	{
	   case wc_Append:
		if (tag->nc_ntime)
			tag->nc_times = (ZebTime *) realloc (tag->nc_times,
			    (tag->nc_ntime + nsample) * sizeof(ZebTime));
		else
			tag->nc_times = (ZebTime *) malloc (nsample *
							   sizeof(ZebTime));
		for (i = 0; i < nsample; ++i)
		{
			dc_GetTime(dc, sample + i, 
				   tag->nc_times + tag->nc_ntime + i);
		}
		*start = tag->nc_ntime;
		tag->nc_ntime += nsample;
		break;
	/*
	 * In the overwrite case we need to find the beginning of the
	 * doomed samples.  All of the samples are overwriting existing
	 * samples with identical times, so we don't have to change the
	 * times or re-write the times array to the file; just return.
	 */
	   case wc_Overwrite:
		dc_GetTime(dc, sample, &t);
		*start = dfa_TimeIndex (ofp, &t, FALSE);
		return(TRUE);
	/*
	 * For insertion, we must create space for the new samples by
	 * moving everything back.  Essentially, take all of the arrays
	 * indexed by the record dimension time and write them out
	 * at a higher index, leaving space for the samples to be inserted.
	 * Does that sound like something you'd be interested in doing?
	 */
	   case wc_Insert:

	   default:
		return(FALSE);
	}
/*
 * Flush out the new TOC entries.
 */
	if (tag->nc_timeType == NC_DOUBLE)
	{
		dtime = (double *) malloc (nsample * sizeof(double));
		for (i = 0; i < nsample; i++)
			dtime[i] = (double) dnc_ZtToOffset
				(tag, tag->nc_times + *start + i);
		status = ncvarput (tag->nc_id, tag->nc_vTime, start,
				   (long *) &nsample, (void *) dtime);
		free (dtime);
	}
	else if (tag->nc_timeType == NC_FLOAT)
	{
		ftime = (float *) malloc (nsample * sizeof(float));
		for (i = 0; i < nsample; i++)
			ftime[i] = (float) dnc_ZtToOffset
				(tag, tag->nc_times + *start + i);
		status = ncvarput (tag->nc_id, tag->nc_vTime, start, 
				   (long *) &nsample, (void *) ftime);
		free (ftime);
	}
	else /* tag->nc_timeType == NC_LONG aka NC_INT */
	{
		ltime = (int *) malloc (nsample * sizeof(int));
		for (i = 0; i < nsample; i++)
			ltime[i] = (int) dnc_ZtToOffset
				(tag, tag->nc_times + *start + i);
		status = ncvarput (tag->nc_id, tag->nc_vTime, start, 
				   (long *) &nsample, (void *) ltime);
		free (ltime);
	}
	if (status < 0)
		dnc_NCError("New times write");

	return (TRUE);
}




static int
dnc_PutBlock (ofp, dc, sample, nsample, wc, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int sample;
int nsample;
WriteCode wc;
dsDetail *details;
int ndetail;
/*
 * Write a block of samples into this file
 */
{
	NCTag *tag = TAGP (ofp);
	long start[ MAX_VAR_DIMS ], count[ MAX_VAR_DIMS ];
	Location *locns;
	int ndim;
/*
 * Make sure the altitude units are consistent with what we expect
 */
	if (dc_GetLocAltUnits (dc) != tag->nc_altUnits)
	{
		msg_ELog (EF_PROBLEM, 
			  "PutBlock: Alt units inconsistent.  Write failed.");
		return (0);
	}
/*
 * Figure out where this sample is supposed to go.
 */
	start[0] = start[1] = start[2] = start[3] = 0;
	count[0] = count[1] = count[2] = count[3] = (long)nsample;
	if (! dnc_FindTimes (ofp, dc, sample, (long)nsample, wc, start))
		return (0);
/*
 * Work out coords and data stuff now.
 */
	dnc_DoWriteCoords (tag, dc, sample, start, count, &ndim);
	dnc_PutRecords (ofp, dc, sample, nsample, start, count, ndim);
	dnc_PutStatic (ofp, dc);
/*
 * For mobile platforms, we need to store the location info too.
 */
	if (ds_IsMobile (tag->nc_plat))
	{
		locns = dc_GetMLoc (dc, sample, nsample);
		dnc_PutLocation (tag, start[0], count[0], locns);
	}
/*
 * Synchronize.
 */
	ncsync (tag->nc_id);
	return (1);
}




static int
dnc_InquireRecord (tag, dc, nrvars, rvarids, dpids)
NCTag *tag;
DataChunk *dc;
int *nrvars;
int rvarids[];
FieldId dpids[];
{
	int f, i;
	int nfield;
	FieldId *fids;

	fids = dc_GetFields (dc, &nfield);
	ncrecinq (tag->nc_id, nrvars, rvarids, NULL);
	/*
	 * Map the requested FieldIds into an array of the same order as
	 * their corresponding record variables.
	 */
	for (f = 0; f < *nrvars; ++f)
	{
		dpids[f] = BadField;
		for (i = 0; i < nfield; ++i)
		{
			if (tag->nc_FMap[rvarids[f]] == fids[i])
			{
				dpids[f] = fids[i];
			}
		}
	}
	return (1);
}



static int
dnc_PutRecords (ofp, dc, sample, nsample, start, count, ndim)
OpenFile *ofp;
DataChunk *dc;
int sample;
int nsample;
long start[];
long count[];
int ndim;
/*
 * Store the record variables sample by sample, taking advantage of a uniform
 * sample stride when possible.
 */
{
	NCTag *tag = TAGP (ofp);
	int i, f;
	int sampsize = 0;
	int nrvars, rvarids[ MAXFIELD ];
	FieldId dpids[ MAXFIELD ];
	int len[ MAXFIELD ];
	void *dp[ MAXFIELD ];

	dnc_InquireRecord (tag, dc, &nrvars, rvarids, dpids);
	if (dc_SampleStride (dc, &sampsize))
	{
	  dc_GetMVector (dc, sample, nrvars, dpids, dp, len);
	}
	/* We're working one sample at a time. */
	count[0] = 1;
	for (i = 0; i < nsample; ++i)
	{
	  if (! sampsize)
	    dc_GetMVector (dc, sample+i, nrvars, dpids, dp, len);
	  for (f = 0; f < nrvars; ++f)
	  {
	    if (! dp[f])
	      continue;
	    if (dc->dc_Class == DCC_NSpace)
	    {
	      dnc_NSpaceVarPut(tag, dc, rvarids[f], dpids[f],
			       start, count, ndim, dp[f]);
	    }
	    else
	    {
	      ncvarput(tag->nc_id, rvarids[f], start, count, dp[f]);
	    }
	    if (sampsize)
	      dp[f] = (char *)dp[f] + sampsize;
	  }
	  start[0] += 1;
	}
	start[0] -= nsample;
	return (0);
}




static void
dnc_PutStatic (ofp, dc)
OpenFile *ofp;
DataChunk *dc;
/*
 * Extract static data from this dc and write it to the file.
 */
{
	NCTag *tag = TAGP (ofp);
	int nfield;
	FieldId *fids;
	int vfield;
	long start[MAXFIELD];
	long count[MAXFIELD];
	int f;
	void *mdata;

	/*
	 * If this is not an N-Space datachunk, we have naught to do.
	 */
	if (dc->dc_Class != DCC_NSpace)
		return;
	fids = dc_GetFields (dc, &nfield);

	for (f = 0; f < nfield; ++f)
	{
		if (! dc_NSIsStatic (dc, fids[f]))
			continue;
		if ((vfield = dnc_GetFieldVar (tag, fids[f])) < 0)
		{
			msg_ELog (EF_PROBLEM, "PutStatic: Can't find fld %s",
				  F_GetName (fids[f]));
			continue;
		}
		mdata = (char *)dc_NSGetStatic (dc, fids[f], NULL);
		if (mdata == NULL)
		{
			msg_ELog (EF_PROBLEM, 
				  "PutStatic: no data for field %s",
				  F_GetName (fids[f]));
			continue;
		}
		if (dnc_NSpaceVarPut (tag, dc, vfield, fids[f], start, 
				      count, 0, (DataPtr)mdata) < 0)
		{
				dnc_NCError ("NSpace data write");
		}
	}
}





/* ARGSUSED */
static void
dnc_DoWriteCoords (tag, dc, sample, start, count, ndim)
NCTag *tag;
DataChunk *dc;
int sample;
long *start, *count;
int *ndim;
/*
 * Set up the coordinates to do the write for this data.
 */
{
	int c = 1;
	RGrid rg;

	switch (tag->nc_org)
	{
	/*
	 * For grids take the geometry into account.
	 */
	   case Org1dGrid:
	   case Org2dGrid:
	   case Org3dGrid:
	   	dc_RGGeometry (dc, sample, 0, &rg);
		if (tag->nc_org == Org3dGrid)
			count[c++] = rg.rg_nZ;
		if (tag->nc_org == Org3dGrid || tag->nc_org == Org2dGrid)
			count[c++] = rg.rg_nY;
		count[c++] = rg.rg_nX;
		break;
	/*
	 * In irregular grid land we just qualify by the number of platforms.
	 */
	   case OrgIRGrid:
	   	count[c++] = tag->nc_nPlat;
		break;
	/*
	 * Everything else takes care of itself.
	 */
	   default:
		break;
	}
	*ndim = c;
}





static zbool
dnc_OverheadField(fld)
char *const fld;
/*
 * See if this is an "overhead" field, as opposed to real data.
 */
{
/*
 * While alt, lat, and lon may be a bit annoying in field lists, we should not
 * hide them from users.  Let somebody higher up do the hiding...
 */
	static char *OFields[] =	/* NOTE these are alphabetical */
	{
	    /* "alt",	*/
		"base_time",
	    /* "lat",	*/
	    /* "lon",	*/
		"platform",
		"time",
		"time_offset",
		"x_spacing",
		"y_spacing",
		"z_spacing",
		0
	};
	int i, cmp;
/*
 * See if this field is in our list.  Exit the loop as soon as the fld
 * is lexically less than or equal to an overhead field.
 */
	for (i = 0; OFields[i]; i++)
		if ((cmp = strcmp (fld, OFields[i])) <= 0)
			return (!cmp);
	return (FALSE);
}



/* ARGSUSED */
static int
dnc_GetFields (ofp, sample, nfld, flist)
OpenFile *ofp;
int sample;
int *nfld;
FieldId *flist;
/*
 * Return the list of available fields at this sample.
 */
{
	NCTag *tag = TAGP (ofp);
	int max = *nfld, fld;
/*
 * Open the file.
 */
	*nfld = 0;
/*
 * Pass through the fields.
 */
	for (fld = 0; fld < tag->nc_nVar && *nfld < max; fld++)
	{
		char *name = F_GetName (tag->nc_FMap[fld]);
		if (dnc_OverheadField (name))
			continue;
		flist[*nfld] = tag->nc_FMap[fld];
		(*nfld)++;
	}
	return (TRUE);
}



static FieldId
dnc_GetFieldByName (NCTag *tag, char *fname)
/*
 * Find a field with the given "fname" in this file, returning its FieldId.
 * Only the field name is used; units, description, etc., are ignored.
 */
{
    int f;

    for (f = 0; f < tag->nc_nVar; f++)
    {
	if (! strcmp (fname, F_GetName (tag->nc_FMap[f])))
	    return (tag->nc_FMap[f]);
    }

    return (BadField);
}





static int
dnc_GetObsSamples (ofp, times, locs, max)
OpenFile *ofp;
ZebTime *times;
Location *locs;
int max;
/*
 * Copy times and locations for all samples.
 */
{
	NCTag *tag = TAGP (ofp);
	int i, nsamp;

	nsamp = (max <= tag->nc_ntime) ? max : tag->nc_ntime;
	if (locs)
		dnc_LoadLocation (tag, locs, 0, nsamp);
	for (i = 0; (times) && (i < nsamp); i++)
	{
		*times++ = tag->nc_times[i];
	}
	return (nsamp);
}





static DC_ElemType
dnc_ElemType (type)
nc_type type;
/*
 * Convert the netCDF type to corresponding DC_ElemType.
 */
{
	switch(type)
	{
	   case NC_BYTE:
		return (DCT_UnsignedChar);
	   case NC_CHAR:
		return (DCT_Char);
	   case NC_SHORT:
		return (DCT_ShortInt);
	   case NC_LONG:
		return (DCT_Integer);
	   case NC_FLOAT:
		return (DCT_Float);
	   case NC_DOUBLE:
		return (DCT_Double);
	   default:
		return(DCT_Unknown);
	}
}





static nc_type
dnc_NCType (type)
DC_ElemType type;
/*
 * Convert the DC_ElemType type to corresponding nc_type.
 */
{
	switch(type)
	{
	   case DCT_Float:
		return (NC_FLOAT);
	   case DCT_Double:
		return (NC_DOUBLE);
	   case DCT_Char:
		return (NC_CHAR);
	   case DCT_UnsignedChar:
		return (NC_BYTE);
	   case DCT_ShortInt:
	   case DCT_UnsignedShort:
		return (NC_SHORT);
	   case DCT_Integer:
	   case DCT_UnsignedInt:
		return (NC_LONG);
	   case DCT_LongInt:
	   case DCT_UnsignedLong:
	        msg_ELog (EF_PROBLEM, 
			  "Unable to convert long datachunk type "
			  "(DCT_LongInt or DCT_UnsignedLong) to netcdf type!");
	   default:
	        return (NC_LONG);	/* no good answer here... */
	}
}




static int
dnc_ReadGlobalAtts(dc,tag)
DataChunk *dc;
NCTag *tag;
/*
 * This function reads the global attributes from the netcdf file
 * and stuffs them into the datachunk.
 */
{
	int nglatt, ndim, nvar, rdim, g, len;
	char key[MAX_NC_NAME];
	void *value;
	nc_type gltype;
/*
 * First, do the inquiry...
 */
	ncinquire(tag->nc_id, &ndim, &nvar, &nglatt, &rdim);
/* 
 * Now, loop over the glatts, and pull out the info we want.
 */
	for (g = 0; g < nglatt; g++)
	{
	/* 
	 * get key name, length, type, all that good stuff.
	 */
		ncattname(tag->nc_id, NC_GLOBAL, g, key);
		ncattinq(tag->nc_id, NC_GLOBAL, key, &gltype, &len);
	/*
	 * now allocate space for the value, and get that too; the +1 is
	 * in case we need to add a null terminator to character arrays
	 */
		value = (void *) malloc ((len+1)*nctypelen(gltype));
		ncattget(tag->nc_id, NC_GLOBAL, key, value);
	/* 
	 * Add the attribute.  Convert character arrays to strings
	 * and NULL-terminate them.
	 */
		if (gltype == NC_CHAR)
		{
			if (((char *)value)[len - 1] != '\0')
				((char *)value)[len++] = '\0';
			dc_SetGlobalAttrArray (dc, key, DCT_String, 1, value);
		}
		else
			dc_SetGlobalAttrArray (dc, key, dnc_ElemType(gltype),
					       len, value);
	/*
	 * Look for an explicit global bad value attribute.
	 */
		if (! strcmp (key, VATT_MISSING))
		{
			dc_SetGlobalBadval (dc, dnc_ElemType(gltype),
					    value);
		}
		free(value);
	}
	return (nglatt);
}



static void
dnc_ReadFieldAtts(dc, tag, fids, nfield)
DataChunk *dc;
NCTag *tag;
FieldId *fids;
int nfield;
/*
 * This function reads the field attributes from the netcdf file
 * for nfield fields and stuffs them into the datachunk.
 */
{
	int natts, f,len, var, i;
	int ndim, dim[MAX_VAR_DIMS];  /* useless but neccessary */
	char key[MAX_NC_NAME];
	void *value;
	nc_type ftype;
	
	for (f = 0; f < nfield; f++) 
	{
	/* 
	 * Before anything else we need the varid for this field
	 */
		if ((var = dnc_GetFieldVar(tag,fids[f])) < 0) 
			continue;
	/* 
	 * How many variable attributes do we have?
	 */
		ncvarinq(tag->nc_id, var, NULL, &ftype,
			 &ndim, &dim[0], &natts);
	/* 
	 * Now, loop over the variable attributes, and pull
	 * out the info we want.
	 */
		for (i = 0; i < natts; i++)
		{
		/* 
		 * get key name, length, type, all that good stuff
		 */
			ncattname(tag->nc_id, var, i, key);
			ncattinq(tag->nc_id, var, key, &ftype, &len);
		/* 
		 * now allocate space for the value, and get that too; the +1
		 * is in case we need to add a null terminator to char arrays
		 */
			value = (void *) malloc ((len+1)*nctypelen(ftype));
			ncattget(tag->nc_id, var, key, value);
		/* 
		 * Now just stuff it into the data chunk, doing our best to
		 * detect character arrays intended to be strings.
		 */
			if (ftype == NC_CHAR)
			{
				if (((char *)value)[len-1] != '\0')
					((char *)value)[len++] = '\0';
				dc_SetFieldAttrArray (dc, fids[f], key,
						      DCT_String, 1, value);
			}
			else
				dc_SetFieldAttrArray (dc, fids[f], key,
					      dnc_ElemType(ftype), len, value);
			/*
			 * Check for an explicit bad value.
			 */
			if (! strcmp (key, VATT_MISSING) &&
			    (dnc_ElemType (ftype) == dc_Type (dc, fids[f])))
			{
				dc_SetFieldBadval (dc, fids[f], value);
			}
			free(value);
		}
	}  /* nfields */
}


#ifdef notdef
/*
 * Return true iff the given time var has a missing_value flag and the
 * given value matches the flag.
 */
static zbool
dnc_TimeIsBad (int ncid, int timevar, nc_type ttype, double val)
{
    double missing;
    float fmissing;
    int lmissing;

    switch (ttype)
    {
      case NC_DOUBLE:
	if (ncattget (ncid, timevar, VATT_MISSING, (void *)(&missing)) < 0)
	    return (FALSE);
	break;
      case NC_FLOAT:
	if (ncattget (ncid, timevar, VATT_MISSING, (void *)(&fmissing)) < 0)
	    return (FALSE);
	missing = (double)fmissing;
	break;
      case NC_LONG:
	if (ncattget (ncid, timevar, VATT_MISSING, (void *)(&lmissing)) < 0)
	    return (FALSE);
	missing = (double)lmissing;
	break;
      default:
	return (FALSE);
	break;
    }

    return (val == missing);
}
#endif
