/*
 * REMAINING DETAILS:
 *	Bad value flags (and attrs in general)
 */

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
# include "../include/defs.h"
# include "../include/message.h"
# include "dfa.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
MAKE_RCSID ("$Id: DFA_NetCDF.c,v 3.10 1992-11-14 00:06:12 burghart Exp $")

# include "netcdf.h"


/*
 * This is our tag structure.
 */
typedef struct _nctag
{
	int             nc_id;		/* netCDF ID value		 */
	long            nc_base;	/* Base time value		 */
	int             nc_vTime;	/* Time variable ID		 */
	int             nc_dTime;	/* The time dimension		 */
	double         *nc_times;	/* The time offset array	 */
	int		nc_timeIsFloat; /* Float or double time?	 */
	int             nc_dTOffset;	/* The dimension of time_offset	 */
	int             nc_ntime;	/* The number of time records	 */
	int             nc_nrec;	/* How many records in the file	 */
	DataOrganization nc_org;	/* The purported organization	 */
	Location       *nc_locs;	/* Location array (IRGRID)	 */
	Location        nc_sloc;	/* Static location		 */
	RGrid           nc_rgrid;	/* Regular grid info		 */
	long 		nc_nPlat;	/* Number of platforms		 */
	PlatformId      nc_plat;	/* The base platform ID		 */
	PlatformId     *nc_subplats;	/* IRGRID subplatform ID's	 */
	int		nc_nVar;	/* Number of variables in the file */
	FieldId		*nc_FMap;	/* The field map		*/
	char		nc_buffered;	/* Is buffering on?		*/
} NCTag;


/*
 * The platform name mapping table.  This is here to allow quick lookup of
 * the index of a subplatform in an irgrid file.  It is all built on the
 * assumption that all the files for a given platform are organized the same
 * way, which should be safe.
 */
#define MAXPLAT	1024		/* How many different platforms we expect */
#define BASEDONE	-1	/* Flag to mark bases which are done	 */
#define UNKNOWN	-2
static int      SPMap[MAXPLAT] = {0};
static bool     SPMapInited = FALSE;
static NCTag *Tag;	/* for passing tag parameter to dnc_PutGlobalAttribute */

/*
 * We also maintain a subplatform list for IRGrid platforms, which ends up in
 * the ir_subplats field of the data object eventually.
 */
static PlatformId *SubPlats[MAXPLAT] = {0};

/*
 * We use this buffer for field queries.
 */
#define MAXFLDBUF 512
static char     FldBuf[MAXFLDBUF];

/*
 * The class/organization compatibility table.  If the desired class
 * and the given file organization appear together here, we can do it.
 */
static struct CO_Compat
{
	DataOrganization	c_org;
	DataClass		c_class;
} COCTable [] =
{
	{ Org1dGrid,		DCC_RGrid	},
	{ Org2dGrid,		DCC_RGrid	},
	{ Org3dGrid,		DCC_RGrid	},
	{ OrgIRGrid,		DCC_IRGrid	},
	{ OrgIRGrid,		DCC_Scalar	},
	{ OrgScalar,		DCC_Scalar	},
	{ OrgScalar,		DCC_Location	},
};
# define N_COC (sizeof (COCTable)/sizeof (struct CO_Compat))

/*
 * Locally used stuff.
 */
static void     dnc_NCError FP ((char *));
static int      dnc_OFTimes FP ((NCTag *));
static int      dnc_GetTimes FP ((NCTag *));
static int      dnc_OFIRGrid FP ((NCTag *));
static int     	dnc_TimeIndex FP ((NCTag *, ZebTime *));
static void     dnc_LoadLocation FP ((NCTag *, Location *, int, int));
static int      dnc_BuildPMap FP ((NCTag *));
static void     dnc_CFMakeDims FP ((NCTag *, DataChunk *, int *, int *));
static void     dnc_CFMakeVars FP ((NCTag *, DataChunk *));
static void     dnc_CFScalarVars FP ((NCTag *, DataChunk *));
static void     dnc_CFGridVars FP ((NCTag *, DataChunk *));
static void     dnc_CFIRGridVars FP ((NCTag *, DataChunk *));
static bool     dnc_OverheadField FP ((char *const));
static int	dnc_OrgClassCompat FP ((DataOrganization, DataClass));
static void	dnc_ConvTimes FP ((NCTag *, int, int, ZebTime *));
static int 	dnc_ReadScalar FP ((DataChunk *, NCTag *, int, int, FieldId *,
			int, double));
static int 	dnc_ReadIRGrid FP ((DataChunk *, NCTag *, int, int, FieldId *,
			int, double));
static int 	dnc_ReadRGrid FP ((DataChunk *, NCTag *, int, int, FieldId *,
			int, double, dsDetail *, int));
static int	dnc_ReadLocation FP ((DataChunk *, NCTag *, int, int));
static int	dnc_GetFieldVar FP ((NCTag *, FieldId));
static void	dnc_ApplyBadval FP ((NCTag *, int, double, float *, int));
static double	dnc_ZtToOffset FP ((NCTag *, ZebTime *));
static void	dnc_DoWriteCoords FP ((NCTag *, DataChunk *, int, long *,
			long *));
static int	dnc_PutGlobalAttribute FP ((char *key, char *value));

/*
 * The minimum size of a time list before it's worthwhile to do a binary
 * search.
 */
#define MINTIME 10



int
dnc_QueryTime (file, begin, end, nsamp)
char *file;
ZebTime *begin, *end;
int *nsamp;
/*
 * Query the time1s on this file.
 */
{
	int id, ndim, nvar, natt, rdim, tvar, btime;
	nc_type dtype;
	int dims[MAX_VAR_DIMS];
	long base, maxrec, index;
	float foffset;
	double offset;
/*
 * Try opening the file.
 */
	ncopts = 0;		/* Change default error behavior	 */
	if ((id = ncopen (file, NC_NOWRITE)) < 0)
		return (FALSE);
/*
 * Look up the time array.
 */
	ncinquire (id, &ndim, &nvar, &natt, &rdim);
	ncdiminq (id, rdim, (char *) 0, &maxrec);
	btime = ncvarid (id, "base_time");
	tvar = ncvarid (id, "time_offset");
/*
 * Find out whether the time offsets are float (old files) or double (newer
 * files)
 */
	ncvarinq (id, tvar, (char *) 0, &dtype, &ndim, dims, &natt);
/*
 * Now pull out the times.
 */
	ncvarget1 (id, btime, 0, &base);

	index = 0;
	if (dtype == NC_FLOAT)
	{
		ncvarget1 (id, tvar, &index, &foffset);
		offset = (double) foffset;
	}
	else
		ncvarget1 (id, tvar, &index, &offset);

	begin->zt_Sec = base + (int) offset;
	begin->zt_MicroSec = offset - aint (offset);

	index = maxrec - 1;
	if (dtype == NC_FLOAT)
	{
		ncvarget1 (id, tvar, &index, &foffset);
		offset = (double) foffset;
	}
	else
		ncvarget1 (id, tvar, &index, &offset);

	end->zt_Sec = base + (int) offset;
	end->zt_MicroSec = offset - aint (offset);
/*
 * Clean up and return.
 */
	*nsamp = maxrec;
	ncclose (id);
	return (TRUE);
}





int
dnc_OpenFile (fname, dp, write, rtag)
char *fname;
DataFile *dp;
int write;
NCTag **rtag;
/*
 * Try to open this file.
 */
{
	NCTag *tag = ALLOC (NCTag);
	int ret;
/*
 * Try to open the file.
 */
	ncopts = 0;		/* Change default error behavior	 */
	if ((tag->nc_id = ncopen (fname, write ? NC_WRITE : NC_NOWRITE))
			    < 0)
	{
		free (tag);
		dnc_NCError ("file open");
		return (FALSE);
	}
/*
 * Do some filling in.
 */
	tag->nc_org = PTable[dp->df_platform].dp_org;
	tag->nc_plat = dp->df_platform;
	tag->nc_ntime = 0;
	tag->nc_locs = (Location *) 0;
	tag->nc_FMap = 0;
	tag->nc_buffered = TRUE;
/*
 * Deal with the time and field information.
 */
	if (!dnc_OFTimes (tag) || ! dnc_LoadFields (tag))
	{
		dnc_CloseFile (tag);
		return (FALSE);
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
	 * Load regular grid geometry info.
	 */
	   case Org1dGrid:
	   case Org2dGrid:
	   case Org3dGrid:
		ret = dnc_OFRGrid (tag);
		break;
	/*
	 * For scalar files, pull in the location if it is static.
	 */
	   case OrgScalar:
		if (! ds_IsMobile (dp->df_platform))
			dnc_LoadLocation (tag, &tag->nc_sloc, 0, 1);
	        ret = TRUE;
		break;

	   default:
		msg_ELog (EF_PROBLEM, "Can't deal with org %d", tag->nc_org);
		ret = FALSE;
		break;
	}

	if (!ret)
		dnc_CloseFile (tag);
	*rtag = tag;
	return (ret);
}





int
dnc_LoadFields (tag)
NCTag *tag;
/*
 * Pull in the field information.
 */
{
	int ndim, nvar, natt, rdim, fld;
	char *cp = FldBuf;
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
	 * Look up the variable and remember it's FID.
	 */
		ncvarinq (tag->nc_id, fld, cp, &type, &ndim, dims, &natt);
		tag->nc_FMap[fld] = F_Lookup (cp);
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
	int dim, lat, lon, alt, i;
	long start = 0, stop;
	float *pos;
/*
 * See how many platforms there are.
 */
	if ((dim = ncdimid (tag->nc_id, "platform")) < 0)
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
 * Find the positioning data.
 */
	if ((lat = ncvarid (tag->nc_id, "lat")) < 0 ||
	    (lon = ncvarid (tag->nc_id, "lon")) < 0 ||
	    (alt = ncvarid (tag->nc_id, "alt")) < 0)
	{
		dnc_NCError ("lat/lon/alt varid");
		return (FALSE);
	}
/*
 * OK, assume this is going to work.  Allocate the space we need.
 */
	tag->nc_locs = (Location *) malloc (tag->nc_nPlat * sizeof(Location));
	pos = (float *) malloc (tag->nc_nPlat * sizeof (float));
/*
 * Now we go through and grab each piece of the location.
 */
	stop = tag->nc_nPlat;
	if (ncvarget (tag->nc_id, lat, &start, &stop, pos) < 0)
		msg_ELog (EF_PROBLEM, "Lat get failure %d", ncerr);
	for (i = 0; i < tag->nc_nPlat; i++)
		tag->nc_locs[i].l_lat = pos[i];

	if (ncvarget (tag->nc_id, lon, &start, &stop, pos) < 0)
		msg_ELog (EF_PROBLEM, "Lon get failure %d", ncerr);
	for (i = 0; i < tag->nc_nPlat; i++)
		tag->nc_locs[i].l_lon = (pos[i] > 0) ? -pos[i] : pos[i];

	if (ncvarget (tag->nc_id, alt, &start, &stop, pos) < 0)
		msg_ELog (EF_PROBLEM, "Lon get failure %d", ncerr);
	for (i = 0; i < tag->nc_nPlat; i++)
		tag->nc_locs[i].l_alt = pos[i];
	free (pos);
/*
 * Build the subplatform map.
 */
	if (!dnc_BuildPMap (tag))
		return (FALSE);
	tag->nc_subplats = SubPlats[tag->nc_plat];
/*
 * All done.
 */
	return (TRUE);
}





static int
dnc_BuildPMap (tag)
NCTag *tag;
/*
 * Build the subplatform lookup map.
 */
{
	int i, name_id, len_id, fldlen, plat;
	long start[2], count[2];
	char *name, *base, *fullname;
/*
 * Initialize the map if necessary.
 */
	if (!SPMapInited)
	{
		for (i = 0; i < MAXPLAT; i++)
			SPMap[i] = UNKNOWN;
		SPMapInited = TRUE;
	}
/*
 * Before we do anything else, let's make sure that this job hasn't
 * already been done.
 */
	if (SPMap[tag->nc_plat] == BASEDONE)
		return (TRUE);
/*
 * Get set up to start snooping through platform names.
 */
	base = ds_PlatformName (tag->nc_plat);
	if ((name_id = ncvarid (tag->nc_id, "platform")) < 0)
	{
		msg_ELog (EF_PROBLEM, "No platform names for %s", base);
		return (FALSE);
	}
	start[1] = 0;		/* Read full name */
/*
 * Find out how long the names are and allocate appropriate space.
 */
	if ((len_id = ncdimid (tag->nc_id, "fldlen")) < 0 ||
		ncdiminq (tag->nc_id, len_id, NULL, &fldlen) < 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Bad or nonexistent 'fldlen' in netCDF file");
		return (FALSE);
	}
	count[0] = 1;
	count[1] = fldlen;

	name = (char *) malloc (fldlen + 1);
	fullname = (char *) malloc (fldlen + strlen (base) + 1);
/*
 * Go through and read back all the platform names.  We should
 * someday be smart and look at the length dimension, but for now
 * I'll assume that 10 will always work.
 */
	tag->nc_subplats = SubPlats[tag->nc_plat] =
		(PlatformId *) malloc (tag->nc_nPlat * sizeof(PlatformId));
	for (i = 0; i < tag->nc_nPlat; i++)
	{
	/*
	 * Read the name of this platform.
	 */
		start[0] = i;
		if (ncvarget (tag->nc_id, name_id, start, count, name) < 0)
		{
			msg_ELog (EF_PROBLEM,
			       "Error %d reading subplat %d from %s", ncerr,
				 i, base);
			return (FALSE);
		}
	/*
	 * Create the full name of this subplatform, and look it up.
	 */
		sprintf (fullname, "%s/%s", base, name);
		if ((plat = ds_LookupPlatform (fullname)) == BadPlatform)
			msg_ELog (EF_INFO, "NC Platform %s unknown", fullname);
		else
			SPMap[plat] = i;
		tag->nc_subplats[i] = plat;
	}
	SPMap[tag->nc_plat] = BASEDONE;
	free (name);
	free (fullname);
	return (TRUE);
}





dnc_OFRGrid (tag)
NCTag *tag;
/*
 * Finish opening a regular grid file.
 */
{
	int v, d;
/*
 * Get the grid origin.
 */
	if ((v = ncvarid (tag->nc_id, "lat")) < 0)
	{
		dnc_NCError ("No 'lat' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_sloc.l_lat);
	if ((v = ncvarid (tag->nc_id, "lon")) < 0)
	{
		dnc_NCError ("No 'lon' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_sloc.l_lon);
	if ((v = ncvarid (tag->nc_id, "alt")) < 0)
	{
		dnc_NCError ("No 'alt' variable");
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
		ncdiminq (tag->nc_id, d, (char *) 0,
					(long *) &tag->nc_rgrid.rg_nZ);

	     /* fall into */
	   case Org2dGrid:
		if ((d = ncdimid (tag->nc_id, "y")) < 0)
		{
			dnc_NCError ("No y dimension");
			return (FALSE);
		}
		ncdiminq (tag->nc_id, d, (char *) 0,
					(long *) &tag->nc_rgrid.rg_nY);

	     /* fall into */
	   case Org1dGrid:
		if ((d = ncdimid (tag->nc_id, "x")) < 0)
		{
			dnc_NCError("No x dimension");
			return (FALSE);
		}
		ncdiminq (tag->nc_id, d, (char *) 0,
					(long *) &tag->nc_rgrid.rg_nX);
	}
/*
 * Finally the grid spacings.
 */
	if ((v = ncvarid (tag->nc_id, "x_spacing")) < 0)
	{
		dnc_NCError ("No 'x_spacing' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_rgrid.rg_Xspacing);
	if ((tag->nc_org == Org2dGrid) || (tag->nc_org == Org3dGrid))
	{
		if ((v = ncvarid (tag->nc_id, "y_spacing")) < 0)
		{
			dnc_NCError ("No 'y_spacing' variable");
			return (FALSE);
		}
		ncvarget1 (tag->nc_id, v, 0, &tag->nc_rgrid.rg_Yspacing);
		if (tag->nc_org == Org3dGrid)
		{
			if ((v = ncvarid (tag->nc_id, "z_spacing")) < 0)
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
dnc_OFTimes (tag)
NCTag *tag;
/*
 * Deal with the time info in this file.
 */
{
	int ndim, dims[MAX_VAR_DIMS], natt, vbase;
	nc_type dtype;
/*
 * Get the base time.
 */
	if ((vbase = ncvarid (tag->nc_id, "base_time")) < 0)
	{
		dnc_NCError ("base_time variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, vbase, 0, &tag->nc_base);
/*
 * There better be a time offset field.  Determine whether we're dealing
 * with a file with float or double times.
 */
	if ((tag->nc_vTime = ncvarid (tag->nc_id, "time_offset")) < 0)
	{
		dnc_NCError ("time_offset variable");
		return (FALSE);
	}
	if (ncvarinq (tag->nc_id, tag->nc_vTime, (char *) 0, &dtype,
		     &ndim, dims, &natt) < 0)
	{
		dnc_NCError ("time_offset varinq");
		return (FALSE);
	}
	if (ndim != 1 /* || dims[0] != tag->nc_dTime */ )
	{
		msg_ELog (EF_PROBLEM, "Bad time_offset var");
		return (FALSE);
	}
	tag->nc_dTOffset = dims[0];
	tag->nc_timeIsFloat = (dtype == NC_FLOAT);
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
	float *ftime;
	int status, i;
/*
 * If the number of times available exceeds the space allocated,
 * start over.
 */
	if (ncdiminq (tag->nc_id, tag->nc_dTOffset, (char *) 0, &ntime) < 0)
	{
		dnc_NCError ("time_offset dim inq");
		return (FALSE);
	}
	if (ntime > tag->nc_ntime)
	{
		if (tag->nc_ntime)
			free (tag->nc_times);
		tag->nc_times = (double *) malloc (ntime * sizeof (double));
	}
	tag->nc_ntime = ntime;
	msg_ELog (EF_DEBUG, "cdf times: %d samps", ntime);
/*
 * Now read in the entire array.  Later, for updates, this call
 * should be a bit more careful and not read the entire array -- it
 * could be expensive.
 */
	if (tag->nc_timeIsFloat)
	{
	/*
	 * Read the time as floats, then move it to the double array.
	 */
		ftime = (float *) malloc (ntime * sizeof (float));
		status = ncvarget (tag->nc_id, tag->nc_vTime, &zero, &ntime, 
			ftime);

		if (status >= 0)
			for (i = 0; i < ntime; i++)
				tag->nc_times[i] = (double) ftime[i];

		free (ftime);
	}
	else
		status = ncvarget (tag->nc_id, tag->nc_vTime, &zero, &ntime, 
			tag->nc_times);

	if (status < 0)
	{
		dnc_NCError ("time_offset get");
		return (FALSE);
	}
	return (TRUE);
}







static void
dnc_NCError (s)
char *s;
/*
 * Report a NETCDF error.
 */
{
/*
 * error number -> message mapping (taken from netcdf.h)
 */
	static char *errmsg[] =
	{ 
	  "no error", "bad NetCDF id", "too many files open", 
	  "can't overwrite file", "invalid argument", "write to read only",
	  "op not allowed in data mode", "op not allowed in define mode",
	  "coordinates out of domain", "MAX_NC_DIMS exceeded", "name in use",
	  "attribute not found", "MAX_NC_ATTRS exceeded", 
	  "not a NetCDF data type", "invalid dimension id", 
	  "NC_UNLIMITED in wrong index", "MAX_NC_VARS exceeded", 
	  "variable not found", "bad action on NC_GLOBAL varid", 
	  "not a NetCDF file", "string too short", "MAX_NC_NAME exceeded",
	  "NC_UNLIMITED size already in use"
	};
/*
 * Print the error message
 */
	if (ncerr <= 22)
		msg_ELog (EF_PROBLEM, "NetCDF error %d (%s) -- %s", ncerr, 
			errmsg[ncerr], s);
	else
		msg_ELog (EF_PROBLEM, "NetCDF error %d -- %s", ncerr, s);
}





dnc_CloseFile(tag)
NCTag *tag;
/*
 * Close this file.
 */
{
	ncclose (tag->nc_id);
	if (tag->nc_ntime)
		free (tag->nc_times);
	if (tag->nc_locs)
		free (tag->nc_locs);
	if (tag->nc_FMap)
		free (tag->nc_FMap);
	free(tag);
}





dnc_SyncFile (tag)
NCTag *tag;
/*
 * Synchronize this file.
 */
{
/*
 * If buffering is still enabled, we need to turn it off now.
 */
	if (tag->nc_buffered)
	{
		ncnobuf (tag->nc_id);
		tag->nc_buffered = FALSE;
	}
/*
 * Update to the file itself, then reload the times array.
 */
	ncsync (tag->nc_id);
	return (dnc_GetTimes (tag));
}




static int
dnc_OrgClassCompat (org, class)
DataOrganization org;
DataClass class;
/*
 * Return TRUE iff these two are compatible.
 */
{
	int i;
/*
 * Go through and see if we find the combination in the table.
 */
	for (i = 0; i < N_COC; i++)
		if (class == COCTable[i].c_class && org == COCTable[i].c_org)
			return (TRUE);
	return (FALSE);
}





DataChunk *
dnc_Setup (gp, fields, nfield, class)
GetList *gp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Get set up for this piece of data access.
 */
{
	NCTag *tag;
	DataChunk *dc;
/*
 * Start by opening the first file.  We'll need it soon.
 */
	if (!dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (0);
/*
 * Make sure this is a combination we can do.
 */
	if (! dnc_OrgClassCompat (tag->nc_org, class))
	{
		msg_ELog (EF_PROBLEM, "File org/class mismatch");
		return (NULL);
	}
/*
 * Create a data chunk with the desired organization.
 */
	dc = dc_CreateDC (class);
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
		break;
	/*
	 * All rgrids need is the set of fields.
	 */
	   case DCC_RGrid:
	   	dc_RGSetup (dc, nfield, fields);
		break;
	/*
	 * Similar with Scalars.
	 */
	   case DCC_Scalar:
	   	dc_SetScalarFields (dc, nfield, fields);
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
	return (dc);
}






int
dnc_GetData (dc, gp, details, ndetail)
DataChunk *dc;
GetList *gp;
dsDetail *details;
int ndetail;
/*
 * Actually get the data that all that work has been done for.
 */
{
	NCTag *tag;
	int tbegin, tend, nfield, nsamp;
	FieldId *fids;
	SValue v;
	float badval;
/*
 * Open the data file.
 */
	if (!dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (0);
/*
 * Figure out what bad value flag they want, and make sure it is stored
 * in the DC.
 */
	if (dc->dc_Class != DCC_Location)
	{
		badval = ds_GetDetail ("badval", details, ndetail, &v) ?
				v.us_v_float : 99999.9;
		dc_SetBadval (dc, badval);
		fids = dc_GetFields (dc, &nfield);
	}
/*
 * Get the time indices.
 */
	tbegin = dnc_TimeIndex (tag, &gp->gl_begin);
	tend = dnc_TimeIndex (tag, &gp->gl_end);
	nsamp = tend - tbegin + 1;
/*
 * Now we have to split out based on the class they want.
 */
	switch (dc->dc_Class)
	{
	/*
	 * Scalars.
	 */
	   case DCC_Scalar:
	   	dnc_ReadScalar (dc, tag, tbegin, nsamp, fids, nfield, badval);
		break;
	/*
	 * IRGridses.
	 */
	   case DCC_IRGrid:
	   	dnc_ReadIRGrid (dc, tag, tbegin, nsamp, fids, nfield, badval);
		break;
	/*
	 * Regular grids.
	 */
	   case DCC_RGrid:
	   	dnc_ReadRGrid (dc, tag, tbegin, nsamp, fids, nfield, badval,
				details, ndetail);
		break;
	/*
	 * Locations.
	 */
	   case DCC_Location:
	   	dnc_ReadLocation (dc, tag, tbegin, nsamp);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Don't handle class %d yet",
				dc->dc_Class);
		return (FALSE);
	}
	return (TRUE);
}






static int
dnc_ReadScalar (dc, tag, begin, nsamp, fids, nfield, badval)
DataChunk *dc;
NCTag *tag;
int begin, nsamp, nfield;
FieldId *fids;
float badval;
/*
 * Retrieve scalar data from this file.
 */
{
	float *temp;
	long start[4], count[4];
	int field, vfield, sbegin = dc_GetNSample (dc), i;
	ZebTime *t;
/*
 *  Figure out our coords.
 */
	start[0] = begin;
	count[0] = nsamp;
/*
 * If the file is really an IRGrid, then we need to figure out the index
 * for the platform.
 */
	if (tag->nc_org == OrgIRGrid)
	{
	/*
	 * Be absolutely sure we're dealing with a known platform here.
	 */
		if (SPMap[dc->dc_Platform] == UNKNOWN)
		{
			msg_ELog (EF_PROBLEM, "Unknown plat %s",
				ds_PlatformName (dc->dc_Platform));
			return (0);
		}
	/*
	 * Be sure they don't want a scalar from the grid as a whole.
	 */
		else if (SPMap[dc->dc_Platform] == BASEDONE)
		{
			msg_ELog (EF_PROBLEM, "Scalar access from grid %s",
				ds_PlatformName (dc->dc_Platform));
			return (0);
		}
	/*
	 * Looks like we can really do this.
	 */
	 	start[1] = SPMap[dc->dc_Platform];
	}
	else
		start[1] = 0;
	count[1] = 1;
/*
 * Get the time array.
 */
	t = (ZebTime *) malloc (nsamp * sizeof (ZebTime));
	dnc_ConvTimes (tag, begin, nsamp, t);
/*
 * Now we can actually pull out the data.
 */
	temp = (float *) malloc (nsamp*sizeof (float));
	for (field = 0; field < nfield; field++)
	{
	/*
	 * Look up the field and try to pull in the data.  Make sure we
	 * get bad value flags right.
	 */
		if (((vfield = dnc_GetFieldVar (tag, fids[field])) < 0) ||
			(ncvarget(tag->nc_id, vfield, start, count, temp) < 0))
		{
			float bad = dc_GetBadval (dc);
			for (i = 0; i < nsamp; i++)
				temp[i] = bad;
		}
		else
			dnc_ApplyBadval (tag, vfield, badval, temp, nsamp);
	/*
	 * Add it to the data chunk.
	 */
		dc_AddMultScalar (dc, t, sbegin, nsamp, fids[field], temp);
	}
	free (temp);
	free (t);
/*
 * Do something about locations.
 */
	if (ds_IsMobile (dc->dc_Platform))
	{
		Location *locs = (Location *) malloc (nsamp*sizeof (Location));
		dnc_LoadLocation (tag, locs, begin, nsamp);
		for (i = 0; i < nsamp; i++)
			dc_SetLoc (dc, sbegin + i, locs + i);
		free (locs);
	}
	else if (tag->nc_org == OrgIRGrid)
		dc_SetStaticLoc (dc, tag->nc_locs + SPMap[dc->dc_Platform]);
	else
		dc_SetStaticLoc (dc, &tag->nc_sloc);
	/* Whew! */
	return (nsamp);
}





static int
dnc_ReadLocation (dc, tag, begin, nsamp)
DataChunk *dc;
NCTag *tag;
int begin, nsamp;
/*
 * Pull in a location-class data chunk.
 */
{
	ZebTime *t = (ZebTime *) malloc (nsamp * sizeof (ZebTime));
	Location *locs = (Location *) malloc (nsamp*sizeof (Location));
	int i;
/*
 * Get the time and location arrays.
 */
	dnc_ConvTimes (tag, begin, nsamp, t);
	dnc_LoadLocation (tag, locs, begin, nsamp);
/*
 * Now we just stuff them into the DC and we're done.
 */
	for (i = 0; i < nsamp; i++)
		dc_LocAdd (dc, t + i, locs + i);
	free (t);
	free (locs);
	return (nsamp);
}







static int
dnc_ReadIRGrid (dc, tag, begin, nsamp, fids, nfield, badval)
DataChunk *dc;
NCTag *tag;
int begin, nsamp, nfield;
FieldId *fids;
float badval;
/*
 * Pull some number of IRGrids out of this file.
 */
{
	long start[4], count[4];
	float *grid = (float *) malloc (tag->nc_nPlat * sizeof (float));
	int sample, field, vfield, dsamp = dc_GetNSample (dc);
	ZebTime t;
/*
 * Coords.
 */
	start[0] = begin;	count[0] = 1;
	start[1] = 0;		count[1] = tag->nc_nPlat;
/*
 * Plow through each desired sample.
 */
 	for (sample = 0; sample < nsamp; sample++)
	{
		dnc_ConvTimes (tag, sample + begin, 1, &t);
	/*
	 * Now do each field.
	 */
	 	for (field = 0; field < nfield; field++)
		{
		/*
		 * Look up the field and try to read it in.
		 */
			if ((vfield = dnc_GetFieldVar (tag, fids[field])) < 0)
				continue;
			if (ncvarget (tag->nc_id, vfield,start,count,grid) < 0)
				dnc_NCError ("Irgrid read");
		/*
		 * If that works, we can apply the bad value flag and
		 * store the data away.
		 */
			else
			{
				dnc_ApplyBadval (tag, vfield, badval, grid,
						count[1]);
				dc_IRAddGrid (dc, &t, dsamp + sample,
						fids[field], grid);
			}
		}
		start[0]++;
	}
	free (grid);
}




static int
dnc_ReadRGrid (dc, tag, begin, nsamp, fids, nfield, badval, dets, ndet)
DataChunk *dc;
NCTag *tag;
int begin, nsamp, nfield, ndet;
FieldId *fids;
float badval;
dsDetail *dets;
/*
 * Pull some number of RGrids out of this file.
 */
{
	long start[4], count[4];
	int sample, field, vfield, dsamp = dc_GetNSample (dc), nc = 1;
	ZebTime t;
	SValue v;
	Location origin;
	RGrid rg;
	DataPtr dp;
/*
 * Make an initial set of coords that pull in the entire grid.
 */
	count[2] = count[3] = 1;
	start[0] = begin;	count[0] = 1;
	start[1] = start[2] = start[3] = 0;
	switch (tag->nc_org)
	{
	   case Org3dGrid:
	   	count[nc++] = tag->nc_rgrid.rg_nZ;
	   case Org2dGrid:
	   	count[nc++] = tag->nc_rgrid.rg_nY;
	   case Org1dGrid:
	   	count[nc++] = tag->nc_rgrid.rg_nX;
	}
/*
 * Initialize rgrid info.
 */
	origin = tag->nc_sloc;
	rg = tag->nc_rgrid;
/*
 * If the file contains 3d grids, they may wish to subsection things.
 */
	if (tag->nc_org == Org3dGrid &&
				ds_GetDetail ("altitude", dets, ndet, &v))
	{
	/*
	 * Figure out what level we want.
	 */
		start[1] = (v.us_v_float - origin.l_alt)/ rg.rg_Zspacing + 0.5;
		if (start[1] < 0)
			start[1] = 0;
		else if (start[1] >= rg.rg_nZ)
			start[1] = rg.rg_nZ - 1;
		count[1] = 1;
	/*
	 * Update our info accordingly.
	 */
	 	rg.rg_nZ = 1;
		origin.l_alt += start[1]*rg.rg_Zspacing;
	}
/*
 * Plow through each desired sample.
 */
 	for (sample = 0; sample < nsamp; sample++)
	{
		dnc_ConvTimes (tag, sample + begin, 1, &t);
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
				&rg, &t, (float *) 0, 0);
			dp = dc_GetMData (dc, dsamp + sample, fids[field], 0);
		/*
		 * Look up the field and try to read it in.
		 */
			if ((vfield = dnc_GetFieldVar (tag, fids[field])) < 0)
				continue;
			if (ncvarget (tag->nc_id, vfield,start, count, dp) < 0)
				dnc_NCError ("Rgrid read");
		/*
		 * If that works, we can apply the bad value flag and
		 * store the data away.
		 */
			else
				dnc_ApplyBadval (tag, vfield, badval, dp,
						count[1]*count[2]*count[3]);
		}
		start[0]++;
	}
}




static void
dnc_ApplyBadval (tag, vfield, badval, data, ndata)
NCTag *tag;
int vfield, ndata;
float badval, *data;
/*
 * Turn the bad value flag stored with the data, if any, into the user-
 * supplied one.
 */
{
	float ncbadval;
	int i;

	if (ncattget (tag->nc_id, vfield, "missing_value", &ncbadval) > 0 &&
				    ncbadval != badval)
	{
		for (i = 0; i < ndata; i++)
			if (data[i] == ncbadval)
				data[i] = badval;
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
dnc_ConvTimes (tag, begin, nsamp, dest)
NCTag *tag;
int begin, nsamp;
ZebTime *dest;
/*
 * Pull out a series of times from the file.
 */
{
	int i;

	for (i = 0; i < nsamp; i++)
	{
		double t = tag->nc_times[begin + i];
		dest->zt_Sec = tag->nc_base + (long) t;
		dest->zt_MicroSec = (long) ((t - aint (t)) * 1000000);
		dest++;
	}
}





static void
dnc_LoadLocation (tag, locs, begin, count)
NCTag *tag;
Location *locs;
int begin, count;
/*
 * Load in mobile platform location info.
 */
{
	int i, var;
	float *ltemp = (float *) malloc (count * sizeof (float));
/*
 * Just do it one piece at a time.  Latitude.
 */
	if ((var = ncvarid (tag->nc_id, "lat")) < 0) {
		dnc_NCError ("No latitude field");
		return;
	}
	if (ncvarget (tag->nc_id, var, &begin, &count, ltemp) < 0) {
		dnc_NCError ("Latitude read");
		return;
	}
	for (i = 0; i < count; i++)
		locs[i].l_lat = ltemp[i];
/*
 * Longitude.
 */
	if ((var = ncvarid (tag->nc_id, "lon")) < 0) {
		dnc_NCError ("No longitude field");
		return;
	}
	if (ncvarget (tag->nc_id, var, &begin, &count, ltemp) < 0) {
		dnc_NCError ("Longitude read");
		return;
	}
	for (i = 0; i < count; i++)
		locs[i].l_lon = ltemp[i];
/*
 * Altitude.
 */
	if ((var = ncvarid (tag->nc_id, "alt")) < 0) {
		dnc_NCError ("No altitude field");
		return;
	}
	if (ncvarget (tag->nc_id, var, &begin, &count, ltemp) < 0) {
		dnc_NCError ("Altitude read");
		return;
	}
	for (i = 0; i < count; i++)
		locs[i].l_alt = ltemp[i];
	free (ltemp);
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
	int var, i;
	float *ltemp;
/*
 * Allocate temp space.
 */
	ltemp = (float *) malloc (count * sizeof(float));
/*
 * Write latitudes.
 */
	if ((var = ncvarid (tag->nc_id, "lat")) < 0)
	{
		dnc_NCError ("No latitude");
		return;
	}
	for (i = 0; i < count; i++)
		ltemp[i] = pos[i].l_lat;
	if (ncvarput (tag->nc_id, var, &start, &count, ltemp) < 0)
	{
		dnc_NCError ("Latitude put");
		return;
	}
/*
 * Write longitudes.
 */
	if ((var = ncvarid (tag->nc_id, "lon")) < 0)
	{
		dnc_NCError ("No longitude");
		return;
	}
	for (i = 0; i < count; i++)
		ltemp[i] = pos[i].l_lon;
	if (ncvarput (tag->nc_id, var, &start, &count, ltemp) < 0)
	{
		dnc_NCError ("Longitude put");
		return;
	}
/*
 * Write latitudes.
 */
	if ((var = ncvarid (tag->nc_id, "alt")) < 0)
	{
		dnc_NCError ("No altitude");
		return;
	}
	for (i = 0; i < count; i++)
		ltemp[i] = pos[i].l_alt;
	if (ncvarput (tag->nc_id, var, &start, &count, ltemp) < 0)
	{
		dnc_NCError ("Altitude put");
		return;
	}
/*
 * 7/91 jc	Ah, yes, we really outta free up the ltemp array
 * after we're done with it.  Those CAPE folks are getting awful
 * tired of bloated processes sitting around.
 */
	free (ltemp);
}







static int
dnc_TimeIndex(tag, t)
NCTag *tag;
ZebTime *t;
/*
 * Figure out how far into this file we have to go to get to this time.
 */
{
	double offset;
	int i;
/*
 * Find out the time offset from the beginning of the file.
 */
	offset = t->zt_Sec - tag->nc_base + (t->zt_MicroSec/1000000.0);
/*
 * Check the extreme cases.  STORE CASE WILL NEED A BIT DIFFERENT.
 */
	if (offset <= tag->nc_times[0])
		return (0);
	else if (offset >= tag->nc_times[tag->nc_ntime - 1])
		return (tag->nc_ntime - 1);
/*
 * OK, search for it.  Someday we'll make this a binary search or
 * something, but, for now....
 *
 * Well, someday we did it....
 */
	if (tag->nc_ntime < MINTIME)
	{
		for (i = tag->nc_ntime - 1; i >= 0; i--)
			if (tag->nc_times[i] <= offset)
				return (i);
	}
	else
	{
		int top = tag->nc_ntime - 1, bottom = 0;
		while (top > bottom + 1)
		{
			int mid = (top + bottom)/2;
			long toff = (long) tag->nc_times[mid];
			if (toff == offset)	/* Might as well try */
				return (mid);
			else if (toff < offset)
				bottom = mid;
			else
				top = mid;
		}
		return (tag->nc_times[top] < offset ? top : bottom);
	}
	return (0);
}






int
dnc_InqPlat (dfindex)
int dfindex;
/*
 * Find out how many platforms are to be found here.
 */
{
	NCTag *tag;

	if ( ! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (0);
	return (tag->nc_nPlat);
}






int
dnc_GetIRGLoc (dfindex, loc)
int dfindex;
Location *loc;
/*
 * Return the location array for this grid.
 */
{
	NCTag *tag;

	if ( ! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return;
	memcpy (loc, tag->nc_locs, tag->nc_nPlat * sizeof (Location));
}






int
dnc_GetRGrid (dfindex, origin, rg)
int dfindex;
Location *origin;
RGrid *rg;
/*
 * Fill in the location info for this grid file.
 */
{
	NCTag *tag;
	float alt;
	int level;
/*
 * Do some checking.
 */
	if (!dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (FALSE);
	if (tag->nc_org != Org1dGrid && tag->nc_org != Org2dGrid
	    && tag->nc_org != Org3dGrid)
	{
		msg_ELog(EF_PROBLEM, "BUG: GetRGrid on non-grid file");
		return (FALSE);
	}
/*
 * Yank the info out of the file.
 */
	alt = origin->l_alt;	/* Kludgy	 */
	*origin = tag->nc_sloc;
	*rg = tag->nc_rgrid;
	level = (alt - tag->nc_sloc.l_alt) / tag->nc_rgrid.rg_Zspacing + 0.5;
	origin->l_alt = tag->nc_rgrid.rg_Zspacing * level + tag->nc_sloc.l_alt;
	return (TRUE);
}






int
dnc_DataTimes (index, when, which, n, dest)
int index, n;
ZebTime *when, *dest;
TimeSpec which;
/*
 * Find out when data is available.
 */
{
	NCTag *tag;
	int t, i;
	double offset;
/*
 * Get the file open.
 */
	if (! dfa_OpenFile (index, FALSE, (void *) &tag))
		return (0);
/*
 * PATCH: since dnc_TimeIndex returns 0 no-matter what when
 * there is only one data point in the file and then there
 * is no way to know whether "when" is greater than or
 * less than.
 */
	if (tag->nc_ntime == 1)
	{
		offset = when->zt_Sec - tag->nc_base +
				when->zt_MicroSec/1000000.0;
		if (offset < tag->nc_times[0])
			t=-1;
		else if (offset >= tag->nc_times[tag->nc_ntime - 1])
			t = tag->nc_ntime-1;
	}
	else
	    t = dnc_TimeIndex (tag, when);
/*
 * Copy out the info.
 */
	if (which == DsBefore)
		for (i = 0; t >= 0 && i < n; i++)
		{
			dest->zt_Sec = tag->nc_base + (int) tag->nc_times[t];
			dest->zt_MicroSec = 1000000*(tag->nc_times[t] -
					aint (tag->nc_times[t]));
			dest++;
			t--;
		}
	else if (which == DsAfter)
	{
		t++;
		for (i = 0; t < tag->nc_ntime && i < n; i++)
		{
			dest->zt_Sec = tag->nc_base + (int) tag->nc_times[t];
			dest->zt_MicroSec = 1000000*(tag->nc_times[t] -
					aint (tag->nc_times[t]));
			dest--;
			t++;
		}
	}
	return (i);
}




dnc_MakeFileName (dir, platform, zt, dest)
char *dir, *platform, *dest;
ZebTime *zt;
/*
 * Generate a file name.
 */
{
	date t;

	TC_ZtToUI (zt, &t);
	sprintf (dest, "%s.%06d.%04d.cdf", platform, t.ds_yymmdd,
		t.ds_hhmmss / 100);
}






dnc_CreateFile (fname, df, dc, rtag)
char *fname;
DataFile *df;
DataChunk *dc;
NCTag **rtag;
/*
 * This is the hairy routine wherein we try to create properly data files for
 * all of the organizations we know.
 */
{
	NCTag *tag = ALLOC(NCTag);
	Platform *plat = PTable + df->df_platform;
	int ndim, dims[6], vars[MAXFIELD], var, vbase, nfield;
	ZebTime t;
	float badval;
	FieldId *fids;
	char *attr, history[128];
/*
 * We might as well start by creating the actual file.  After all,
 * that, at least, is common to all of the organizations.
 */
	if ((tag->nc_id = nccreate (fname, NC_CLOBBER)) < 0)
	{
		free (tag);
		dnc_NCError ("File create");
		return (FALSE);
	}
/*
 * Fill in some basic tag info.
 */
	tag->nc_times = (double *) 0;
	tag->nc_ntime = tag->nc_nrec = 0;
	tag->nc_timeIsFloat = FALSE;
	tag->nc_org = plat->dp_org;
	tag->nc_locs = (Location *) 0;
	tag->nc_plat = dc->dc_Platform;;
	tag->nc_buffered = TRUE;
/*
 * Create the time dimension.  If this platform has the "discrete"
 * flag set, or it's an IRGRID organization, then we make time
 * unlimited.  Otherwise we wire time to the maxsample value, in
 * hopes of getting better performance out of large, scalar data.
 * 
 * XXX WIRE IT UNLIMITED FOR NOW, UNTIL WE FIGURE OUT HOW TO KEEP TRACK
 * OF HOW MANY SAMPLES ARE ACTUALLY WRITTEN.
 */
	if (TRUE || plat->dp_org == OrgIRGrid || plat->dp_flags & DPF_DISCRETE)
		tag->nc_dTime = ncdimdef (tag->nc_id, "time", NC_UNLIMITED);
	else
		tag->nc_dTime = ncdimdef(tag->nc_id, "time", plat->dp_maxsamp);
	tag->nc_dTOffset = tag->nc_dTime;
/*
 * Create the other dimensions that we need for variables.
 */
	ndim = 1;
	dims[0] = tag->nc_dTime;
	dnc_CFMakeDims (tag, dc, &ndim, dims);
/*
 * Make the time variables.  The time_offset field is now stored as a double
 * so we have sufficient precision to represent reasonable time offsets down
 * to the microsecond.
 */
	vbase = ncvardef (tag->nc_id, "base_time", NC_LONG, 0, 0);
	tag->nc_vTime = ncvardef (tag->nc_id, "time_offset", NC_DOUBLE,
					 1, &tag->nc_dTime);
	dc_GetTime (dc, 0, &t);
	tag->nc_base = t.zt_Sec;
/*
 * Create the actual fields that we are storing here.  Add the bad
 * value flag for now and conventional long_name and units attributes.
 */
	badval = dc_GetBadval (dc);
	fids = dc_GetFields (dc, &nfield);
	for (var = 0; var < nfield; var++)
	{
		vars[var] = ncvardef (tag->nc_id, F_GetName (fids[var]),
				     NC_FLOAT, ndim, dims);
		(void) ncattput (tag->nc_id, vars[var], "missing_value",
				NC_FLOAT, 1, &badval);
		attr = F_GetUnits(fids[var]);
		(void) ncattput (tag->nc_id, vars[var], "units",
				NC_CHAR, strlen(attr)+1, attr);
		attr = F_GetDesc(fids[var]);
		(void) ncattput (tag->nc_id, vars[var], "long_name",
				NC_CHAR, strlen(attr)+1, attr);
	}
/*
 * Add global attributes from the data chunk and create the global
 * 'history' attribute.
 * Unfortunately, dc_ProcessAttrs does not provide for a 'tag' argument
 * to the function, so the tag must be passed via a global variable.
 */
	Tag = tag;
	(void)dc_ProcessAttrs(dc, NULL, dnc_PutGlobalAttribute);
	attr = ds_PlatformName(dc->dc_Platform);
	(void)ncattput(tag->nc_id, NC_GLOBAL, "platform", 
		       NC_CHAR, strlen(attr)+1, attr);
	{
		struct timeval tv;

		sprintf(history,"created by Zeb DataStore, ");
		(void)gettimeofday(&tv, NULL);
		TC_EncodeTime((ZebTime *)&tv, TC_Full, history+strlen(history));
		strcat(history,", $RCSfile: DFA_NetCDF.c,v $ $Revision: 3.10 $\n");
		(void)ncattput(tag->nc_id, NC_GLOBAL, "history",
			       NC_CHAR, strlen(history)+1, history);
	}
/*
 * Create the organization-specific variables.  Since some of these
 * need to be initialized here, this routine also takes us out of
 * definition mode, so we can put in the base time thereafter.
 */
	dnc_CFMakeVars (tag, dc);
	dnc_LoadFields (tag);
	ncvarput1 (tag->nc_id, vbase, 0, &tag->nc_base);
	*rtag = tag;
	return (TRUE);
}




int
dnc_PutGlobalAttribute(key, value)
char *key;
char *value;
/*
 * Add this datachunk attribute to the file's global attributes
 * Presently, this only happens when a file is created.  Any additional
 * datachunks' global attributes will not be written.  XXX What would the
 * behavior be if the same global attribute was given different values
 * in different datachunks?
 */
{
	(void)ncattput(Tag->nc_id, NC_GLOBAL,
		       key, NC_CHAR, strlen(value)+1, value);
	return(0);
}





static void
dnc_CFMakeDims (tag, dc, ndim, dims)
NCTag *tag;
DataChunk *dc;
int *ndim, *dims;
/*
 * Create the dimensions for this file organization.
 */
{
	RGrid rg;

	switch (tag->nc_org)
	{
	/*
	  Scalar files are easy -- we're done!
	 */
	   case OrgScalar:
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
			dims[(*ndim)++] = ncdimdef (tag->nc_id, "z", rg.rg_nZ);
		if (tag->nc_org == Org3dGrid || tag->nc_org == Org2dGrid)
			dims[(*ndim)++] = ncdimdef (tag->nc_id, "y", rg.rg_nY);
		dims[(*ndim)++] = ncdimdef (tag->nc_id, "x", rg.rg_nX);
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
	vlat = ncvardef (tag->nc_id, "lat", NC_FLOAT, ndim, &tdim);
	vlon = ncvardef (tag->nc_id, "lon", NC_FLOAT, ndim, &tdim);
	valt = ncvardef (tag->nc_id, "alt", NC_FLOAT, ndim, &tdim);
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
	int vlat, vlon, valt, vx, vy, vz;
	Location loc;

	dc_RGGeometry (dc, 0, &loc, &tag->nc_rgrid);
/*
 * Create variables for the origin and spacing information.
 */
	vlat = ncvardef (tag->nc_id, "lat", NC_FLOAT, 0, 0);
	vlon = ncvardef (tag->nc_id, "lon", NC_FLOAT, 0, 0);
	valt = ncvardef (tag->nc_id, "alt", NC_FLOAT, 0, 0);
	vx = ncvardef (tag->nc_id, "x_spacing", NC_FLOAT, 0, 0);
	if ((tag->nc_org == Org2dGrid) || (tag->nc_org == Org3dGrid))
	{
		vy = ncvardef(tag->nc_id, "y_spacing", NC_FLOAT, 0, 0);
		if (tag->nc_org == Org3dGrid)
			vz = ncvardef(tag->nc_id, "z_spacing", NC_FLOAT, 0, 0);
	}
/*
 * Now, out of definition mode and initialize all of those variables.
 */
	ncendef (tag->nc_id);
	ncvarput1 (tag->nc_id, vlat, 0, &loc.l_lat);
	ncvarput1 (tag->nc_id, vlon, 0, &loc.l_lon);
	ncvarput1 (tag->nc_id, valt, 0, &loc.l_alt);
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
	char *name, *subname, *strrchr ();
	PlatformId *plats;
/*
 * Look up a couple of dimensions that we have already made, then
 * create the variables to hold the platform names and locations.
 */
	dims[0] = ncdimid (tag->nc_id, "platform");
	dims[1] = ncdimid (tag->nc_id, "fldlen");
	vplat = ncvardef (tag->nc_id, "platform", NC_CHAR, 2, dims);
	vlat = ncvardef (tag->nc_id, "lat", NC_FLOAT, 1, dims);
	vlon = ncvardef (tag->nc_id, "lon", NC_FLOAT, 1, dims);
	valt = ncvardef (tag->nc_id, "alt", NC_FLOAT, 1, dims);
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
 * If necessary, we'll work on the platform map as well.
 */
	if (!SPMapInited)
	{
		for (plat = 0; plat < MAXPLAT; plat++)
			SPMap[plat] = UNKNOWN;
		SPMapInited = TRUE;
	}
	if (SPMap[dc->dc_Platform] != BASEDONE)
	{
		for (plat = 0; plat < tag->nc_nPlat; plat++)
			SPMap[plats[plat]] = plat;
		SPMap[dc->dc_Platform] = BASEDONE;
	}
/*
 * Sigh.  We really ought to put this into the tag structure while we
 * are at it.
 */
	SubPlats[tag->nc_plat] = tag->nc_subplats = plats;
}





static double
dnc_ZtToOffset (tag, zt)
NCTag *tag;
ZebTime *zt;
/*
 * Turn this time into a suitable offset for this file.
 */
{
	return (zt->zt_Sec - tag->nc_base + (zt->zt_MicroSec/1000000.0));
}





static int
dnc_FindDest (tag, wc, t, fsample)
NCTag *tag;
WriteCode wc;
ZebTime *t;
long *fsample;
/*
 * Figure out just where this sample is supposed to go.
 */
{
	int status;
	long one = 1;
	float ftime;
/*
 * It all depends on the write code.
 */
	switch (wc)
	{
	/*
	 * The append case is like in the good old days.  We allocate a
	 * new TOC entry and set this up to be written at the end of
	 * the file.
	 */
	   case wc_Append:
		if (tag->nc_ntime)
			tag->nc_times = (double *) realloc (tag->nc_times,
				    (tag->nc_ntime + 1) * sizeof(double));
		else
			tag->nc_times = (double *) malloc (sizeof(double));
		tag->nc_times[tag->nc_ntime] = dnc_ZtToOffset (tag, t);
		*fsample = tag->nc_ntime++;
		break;
	/*
	 * The insert case is hopeless, for now.  The only way to really
	 * do this is to copy all of the data from here to the end of the
	 * file on down.  Ugly.
	 */
	   case wc_Insert:
	   	return (FALSE);
	/*
	 * In the overwrite case we need to find the doomed sample and 
	 * tweak the TOC accordingly.
	 */
	   case wc_Overwrite:
	   	*fsample = dnc_TimeIndex (tag, t);
		tag->nc_times[*fsample] = dnc_ZtToOffset (tag, t);
		break;
	}
/*
 * Flush out the new TOC entry.
 */
	if (tag->nc_timeIsFloat)
	{
		ftime = (float) tag->nc_times[*fsample];
		status = ncvarput (tag->nc_id, tag->nc_vTime, fsample, &one, 
			&ftime);
	}
	else
		status = ncvarput (tag->nc_id, tag->nc_vTime, fsample, &one,
			    tag->nc_times + *fsample);

	if (status < 0)
		dnc_NCError("New time write");

	return (TRUE);
}






dnc_PutSample (dfile, dc, sample, wc)
int dfile, sample;
DataChunk *dc;
WriteCode wc;
/*
 * Write one sample from the DC into this file.
 */
{
	NCTag *tag;
	long start[4], count[4];
	int vfield, field, nfield;
	DataPtr data;
	ZebTime zt;
	FieldId *fids;
	Location loc;
/*
 * Gotta open up the file before we do anything.
 */
	if (!dfa_OpenFile (dfile, TRUE, (void *) &tag))
		return (0);
/*
 * Figure out where this sample is supposed to go.
 */
	start[0] = start[1] = start[2] = start[3] = 0;
	count[0] = count[1] = count[2] = count[3] = 1;
	dc_GetTime (dc, sample, &zt);
	if (! dnc_FindDest (tag, wc, &zt, start))
		return (0);
/*
 * Work out coords and data stuff now.
 */
	dnc_DoWriteCoords (tag, dc, sample, start, count);
/*
 * Time for the humungo write to dump it all into the file.
 */
	fids = dc_GetFields (dc, &nfield);
	for (field = 0; field < nfield; field++)
	{
	/*
	 * Find everything.
	 */
		if ((vfield = dnc_GetFieldVar (tag, fids[field])) < 0)
		{
			msg_ELog (EF_PROBLEM, "(PUT) Can't find fld %s",
						 F_GetName (fids[field]));
			continue;
		}
		data = dc_GetMData (dc, sample, fids[field], 0);
	/*
	 * Write it to the file.
	 */
		if (ncvarput (tag->nc_id, vfield, start, count, data) < 0)
			dnc_NCError ("Data write");
	}
/*
 * For mobile platforms, we need to store the location info too.
 */
	if (PTable[tag->nc_plat].dp_flags & DPF_MOBILE)
	{
		dc_GetLoc (dc, sample, &loc);
		dnc_PutLocation (tag, start[0], count[0], &loc);
	}
/*
 * Synchronize.
 */
	ncsync (tag->nc_id);
	return (1);
}





/* ARGSUSED */
static void
dnc_DoWriteCoords (tag, dc, sample, start, count)
NCTag *tag;
DataChunk *dc;
int sample;
long *start, *count;
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
		count[c] = rg.rg_nX;
		break;
	/*
	 * In irregular grid land we just qualify by the number of platforms.
	 */
	   case OrgIRGrid:
	   	count[1] = tag->nc_nPlat;
		break;
	/*
	 * Everything else takes care of itself.
	 */
	}
}





static bool
dnc_OverheadField(fld)
char *const fld;
/*
 * See if this is an "overhead" field, as opposed to real data.
 */
{
	static char *OFields[] =
	{
		"base_time",
		"time_offset",
		"platform",
		"lat",
		"lon",
		"alt",
		"x_spacing",
		"y_spacing",
		"z_spacing",
		0
	};
	int i;
/*
 * See if this field is in our list.  This could really be smarter.
 */
	for (i = 0; OFields[i]; i++)
		if (!strcmp (fld, OFields[i]))
			return (TRUE);
	return (FALSE);
}



/* ARGSUSED */
int
dnc_GetFields (dfile, t, nfld, flist)
int dfile, *nfld;
ZebTime *t;
FieldId *flist;
/*
 * Return the list of available fields.
 */
{
	NCTag *tag;
	int max = *nfld, fld;
/*
 * Open the file.
 */
	*nfld = 0;
	if (!dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
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





int
dnc_GetObsSamples (dfile, times, locs, max)
int dfile, max;
ZebTime *times;
Location *locs;
/*
 * Return sample info.
 */
{
	NCTag *tag;
	int i;
/*
 * Get the file open.
 */
	if (!dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);

	for (i = 0; i < tag->nc_ntime && i < max; i++)
	{
		times->zt_Sec = tag->nc_base + (int) tag->nc_times[i];
		times->zt_MicroSec = 1000000*(tag->nc_times[i] -
					aint (tag->nc_times[i]));
		times++;
		*locs++ = (tag->nc_locs) ? tag->nc_locs[i] : tag->nc_sloc;
	}
}
