/*
 * Access to netCDF files.
 */
static char *rcsid = "$Id: DFA_NetCDF.c,v 1.2 1991-01-16 22:06:46 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "dfa.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"

# include "netcdf.h"


/*
 * This is our tag structure.
 */
typedef struct _nctag
{
	int	nc_id;		/* netCDF ID value		*/
	long	nc_base;	/* Base time value		*/
	int	nc_vTime;	/* Time variable ID		*/
	int	nc_dTime;	/* The time dimension		*/
	float	*nc_times;	/* The time offset array	*/
	int	nc_dTOffset;	/* The dimension of time_offset */
	int	nc_ntime;	/* The number of time records	*/
	int	nc_nrec;	/* How many records in the file	*/
	DataOrganization nc_org;/* The purported organization	*/
	Location *nc_locs;	/* Location array (IRGRID)	*/
	Location nc_sloc;	/* Static location		*/
	RGrid	nc_rgrid;	/* Regular grid info		*/
	int	nc_nPlat;	/* Number of platforms		*/
	PlatformId nc_plat;	/* The base platform ID		*/
} NCTag;


/*
 * The platform name mapping table.  This is here to allow quick lookup
 * of the index of a subplatform in an irgrid file.  It is all built on
 * the assumption that all the files for a given platform are organized
 * the same way, which should be safe.
 */
# define MAXPLAT	256	/* How many different platforms we expect */
# define BASEDONE	-1	/* Flag to mark bases which are done	*/
# define UNKNOWN	-2
static int SPMap[MAXPLAT] = { 0 };
static bool SPMapInited = FALSE;

/*
 * Locally used stuff.
 */
# ifdef __STDC__
	static void	dnc_NCError (char *);
	static int	dnc_OFTimes (NCTag *);
	static int	dnc_GetTimes (NCTag *);
	static int	dnc_OFIRGrid (NCTag *);
	static int	dnc_TimeIndex (NCTag *, time *);
	static void 	dnc_GField (NCTag *, char *, float *, int, int,
					double, DataObject *);
	static void	dnc_MakeCoords (NCTag *, DataObject *, int *, int *);
	static void	dnc_LoadLocation (NCTag *, GetList *, int, int);
	static int	dnc_BuildPMap (NCTag *);
# else
	static void 	dnc_NCError ();
	static int	dnc_OFTimes ();
	static int	dnc_GetTimes ();
	static int	dnc_OFIRGrid ();
	static int	dnc_TimeIndex ();
	static void 	dnc_GField ();
	static void	dnc_LoadLocation ();
	static int	dnc_BuildPMap ();
# endif


/*
 * The minimum size of a time list before it's worthwhile to do a binary
 * search.
 */
# define MINTIME 10




int
dnc_QueryTime (file, begin, end)
char *file;
time *begin, *end;
/*
 * Query the times on this file.
 */
{
	int id, ndim, nvar, natt, rdim, maxrec, tvar, btime, index;
	long base, t;
	float offset;
/*
 * Try opening the file.
 */
	ncopts = 0;		/* Change default error behavior	*/
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
 * Now pull out the times.
 */
 	ncvarget1 (id, btime, 0, &base);
	index = 0;
	ncvarget1 (id, tvar, &index, &offset);
	t = base + (int) offset;
	TC_SysToFcc (t, begin);
	index = maxrec - 1;
	ncvarget1 (id, tvar, &index, &offset);
	t = base + (int) offset;
	TC_SysToFcc (t, end);
/*
 * Clean up and return.
 */
	ncclose (id);
	return (TRUE);
}





int
dnc_OpenFile (dp, rtag)
DataFile *dp;
NCTag **rtag;
/*
 * Try to open this file.
 */
{
	NCTag *tag = ALLOC (NCTag);
	int vbase, ret;
/*
 * Try to open the file.
 */
	ncopts = 0;		/* Change default error behavior	*/
	if ((tag->nc_id = ncopen (dp->df_name, NC_NOWRITE)) < 0)
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
/*
 * Deal with the time information.
 */
	if (! dnc_OFTimes (tag))
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
	   case Org2dGrid:
	   case Org3dGrid:
	   	ret = dnc_OFRGrid (tag, tag->nc_org == Org3dGrid);
		break;
	/*
	 * Nothing to do for scalar files, for now.
	 */
	   case OrgScalar:
	   	break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Can't deal with org %d", tag->nc_org);
		ret = FALSE;
		break;
	}

	if (! ret)
		dnc_CloseFile (tag);
	*rtag = tag;
	return (ret);
}







static int
dnc_OFIRGrid (tag)
NCTag *tag;
/*
 * Figure out the setup for this IRGRID file.
 */
{
	int dim, lat, lon, alt, i, start = 0, stop;
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
	tag->nc_locs = (Location *) malloc (tag->nc_nPlat * sizeof (Location));
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
	if (! dnc_BuildPMap (tag))
		return (FALSE);
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
	int i, start[2], count[2], vname, plat;
	char name[10], base[20], fullname[30];
/*
 * Initialize the map if necessary.
 */
	if (! SPMapInited)
	{
		for (i = 0; i < MAXPLAT; i++)
			SPMap[i] = UNKNOWN;
		SPMapInited = TRUE;
	}
/*
 * Before we do anything else, let's make sure that this job hasn't already
 * been done.
 */
	if (SPMap[tag->nc_plat] == BASEDONE)
		return (TRUE);
/*
 * Get set up to start snooping through platform names.
 */
	strcpy (base, ds_PlatformName (tag->nc_plat));
	if ((vname = ncvarid (tag->nc_id, "platform")) < 0)
	{
		msg_ELog (EF_PROBLEM, "No platform names for %s", base);
		return (FALSE);
	}
	start[1] = 0;	/* Read full name */
	count[0] = 1;
	count[1] = 10;	/* XXX */
/*
 * Go through and read back all the platform names.  We should
 * someday be smart and look at the length dimension, but for now I'll
 * assume that 10 will always work.
 */
 	for (i = 0; i < tag->nc_nPlat; i++)
	{
	/*
	 * Read the name of this platform.
	 */
		start[0] = i;
		if (ncvarget (tag->nc_id, vname, start, count, name) < 0)
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
			msg_ELog (EF_INFO, "Platform %s unknown", fullname);
		else
			SPMap[plat] = i;
	}
	SPMap[tag->nc_plat] = BASEDONE;
	return (TRUE);
}





dnc_OFRGrid (tag, threed)
NCTag *tag;
bool threed;
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
	if ((d = ncdimid (tag->nc_id, "x")) < 0)
	{
		dnc_NCError ("No x dimension");
		return (FALSE);
	}
	ncdiminq (tag->nc_id, d, (char *) 0, &tag->nc_rgrid.rg_nX);
	if ((d = ncdimid (tag->nc_id, "y")) < 0)
	{
		dnc_NCError ("No y dimension");
		return (FALSE);
	}
	ncdiminq (tag->nc_id, d, (char *) 0, &tag->nc_rgrid.rg_nY);
	if ((d = ncdimid (tag->nc_id, "z")) < 0)
	{
		dnc_NCError ("No z dimension");
		return (FALSE);
	}
	ncdiminq (tag->nc_id, d, (char *) 0, &tag->nc_rgrid.rg_nZ);
/*
 * Finally the grid spacings.
 */
	if ((v = ncvarid (tag->nc_id, "x_spacing")) < 0)
	{
		dnc_NCError ("No 'x_spacing' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_rgrid.rg_Xspacing);
	if ((v = ncvarid (tag->nc_id, "y_spacing")) < 0)
	{
		dnc_NCError ("No 'y_spacing' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_rgrid.rg_Yspacing);
	if ((v = ncvarid (tag->nc_id, "z_spacing")) < 0)
	{
		dnc_NCError ("No 'z_spacing' variable");
		return (FALSE);
	}
	ncvarget1 (tag->nc_id, v, 0, &tag->nc_rgrid.rg_Zspacing);
	return (TRUE);
}





static int
dnc_OFTimes (tag)
NCTag *tag;
/*
 * Deal with the time info in this file.
 */
{
	int dtype, ndim, dims[MAX_VAR_DIMS], natt, vbase;
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
 * There better be a time offset field.  We will assume that it is a float
 * field for now.
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
	int ntime, zero = 0;
/*
 * If the number of times available exceeds the space allocated, start
 * over.
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
		tag->nc_times = (float *) malloc (ntime * sizeof (float));
	}
	tag->nc_ntime = ntime;
/*
 * Now read in the entire array.  Later, for updates, this call should be
 * a bit more careful and not read the entire array -- it could be expensive.
 */
	if (ncvarget (tag->nc_id, tag->nc_vTime, &zero, &ntime, tag->nc_times)
			< 0)
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
	msg_ELog (EF_PROBLEM, "NetCDF error %d -- %s", ncerr, s);
}





dnc_CloseFile (tag)
NCTag *tag;
/*
 * Close this file.
 */
{
	ncclose (tag->nc_id);
	if (tag->nc_ntime)
		free (tag->nc_times);
	free (tag);
}





dnc_SyncFile (tag)
NCTag *tag;
/*
 * Synchronize this file.
 */
{
	ncsync (tag->nc_id);
	msg_ELog (EF_PROBLEM, "NC sync, but I'm not really ready");
}





int
dnc_Setup (gp)
GetList *gp;
/*
 * Get set up for this piece of data access.
 */
{
	DataFile *dp = DFTable + gp->gl_dfindex;
	NCTag *tag;
	int tbegin, tend;
/*
 * Start by opening the file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, (void *) &tag))
		return;
/*
 * Find the time indices of the desired data range and stuff in the
 * point count.
 */
	tbegin = dnc_TimeIndex (tag, &gp->gl_begin);
	tend = dnc_TimeIndex (tag, &gp->gl_end);
	gp->gl_nsample = tend - tbegin + 1;
	switch (gp->gl_dobj->do_org)
	{
	/*
	 * For irregular grids, there is one point for each platform.
	 */
	   case OrgIRGrid:
		gp->gl_npoint = (tend - tbegin + 1);
		if (gp->gl_dobj->do_id == tag->nc_plat)
			gp->gl_npoint *= tag->nc_nPlat;
		break;
	/*
	 * Regular grids.
	 */
	   case Org2dGrid:
	   	gp->gl_npoint = (tend - tbegin + 1) * tag->nc_rgrid.rg_nX *
				tag->nc_rgrid.rg_nY;
		break;
	   case Org3dGrid:
	   	gp->gl_npoint = (tend - tbegin + 1) * tag->nc_rgrid.rg_nX *
				tag->nc_rgrid.rg_nY * tag->nc_rgrid.rg_nZ;
	  /*
	   * Scalar data, by definition, has the number of points equal
	   * to the number of samples.
	   */
	   case OrgScalar:
	     	gp->gl_npoint = gp->gl_nsample;
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Setup on unknown org %d",
				gp->gl_dobj->do_org);
		break;
	}
}






int
dnc_GetData (gp)
GetList *gp;
/*
 * Actually get the data that all that work has been done for.
 */
{
	DataObject *dobj = gp->gl_dobj;
	NCTag *tag;
	int t, tbegin, tend, field, count;
	float *ltemp;
/*
 * Open the data file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, (void *) &tag))
		return;
/*
 * Get the time indices.
 */
	tbegin = dnc_TimeIndex (tag, &gp->gl_begin);
	tend = dnc_TimeIndex (tag, &gp->gl_end);
/*
 * Go through and snarf each field.
 */
	for (field = 0; field < dobj->do_nfield; field++)
		dnc_GField (tag, dobj->do_fields[field], gp->gl_data[field],
				tbegin, tend, dobj->do_badval, dobj);
/*
 * Fix up the time values.
 */
	count = tend - tbegin + 1;
	for (t = 0; t < count; t++)
	{
		long st = tag->nc_base + (int) tag->nc_times[t + tbegin];
		TC_SysToFcc (st, &gp->gl_time[t]);
	}
/*
 * If need be, we also snarf up location info.
 */
	if (gp->gl_locs)
		dnc_LoadLocation (tag, gp, tbegin, count);
}






static void
dnc_LoadLocation (tag, gp, begin, count)
NCTag *tag;
GetList *gp;
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
	if ((var = ncvarid (tag->nc_id, "lat")) < 0)
	{
		dnc_NCError ("No latitude field");
		return;
	}
	if (ncvarget (tag->nc_id, var, &begin, &count, ltemp) < 0)
	{
		dnc_NCError ("Latitude read");
		return;
	}
	for (i = 0; i < count; i++)
		gp->gl_locs[i].l_lat = ltemp[i];
/*
 * Longitude.
 */
	if ((var = ncvarid (tag->nc_id, "lon")) < 0)
	{
		dnc_NCError ("No longitude field");
		return;
	}
	if (ncvarget (tag->nc_id, var, &begin, &count, ltemp) < 0)
	{
		dnc_NCError ("Longitude read");
		return;
	}
	for (i = 0; i < count; i++)
		gp->gl_locs[i].l_lon = ltemp[i];
/*
 * Altitude.
 */
	if ((var = ncvarid (tag->nc_id, "alt")) < 0)
	{
		dnc_NCError ("No altitude field");
		return;
	}
	if (ncvarget (tag->nc_id, var, &begin, &count, ltemp) < 0)
	{
		dnc_NCError ("Altitude read");
		return;
	}
	for (i = 0; i < count; i++)
		gp->gl_locs[i].l_alt = ltemp[i];
	free (ltemp);
}







static void
dnc_GField (tag, fname, data, tbegin, tend, badval, dobj)
NCTag *tag;
char *fname;
float *data, badval;
int tbegin, tend;
DataObject *dobj;
/*
 * Grab this field.
 * Entry:
 *	TAG	is the open file tag.
 *	FNAME	is the name of the field to get.
 *	DATA	is the destination data array.
 *	TBEGIN, TEND	define the time period of interest.
 *	BADVAL	is the bad data flag.
 *	DOBJ	is the destination data object.
 */
{
	int vfield, start[4], count[4], i, np;
	float ncbadval;
/*
 * Figure out how to tell netCDF what we want.
 */
	if ((vfield = ncvarid (tag->nc_id, fname)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Field '%s' missing!", fname);
		return;
	}
/*
 * Figure out the data coordinates based on the file organization.
 */
	start[0] = tbegin;
	count[0] = tend - tbegin + 1;
	dnc_MakeCoords (tag, dobj, start, count);
/*
 * Do it.
 */
	if (ncvarget (tag->nc_id, vfield, start, count, data) < 0)
		dnc_NCError ("Data read");
/*
 * If there is a bad value flag associated with this field, go through
 * and change it to the one stored in the data object.
 */
	np = count[0] * count[1] * count[2] * count[3];
	if (ncattget (tag->nc_id, vfield, "missing_value", &ncbadval) > 0 &&
			ncbadval != badval)
		for (i = 0; i < np; i++)
			if (data[i] == ncbadval)
				data[i] = badval;
}





static void
dnc_MakeCoords (tag, dobj, start, count)
NCTag *tag;
DataObject *dobj;
int *start, *count;
/*
 * Figure out the ncvarget coords for this data grab.
 * Times are already assumed to be in [0].
 */
{
	count[2] = count[3] = 1;
	switch (tag->nc_org)
	{
	/*
	 * When pulling IRGrid data, we ordinarily go for all platforms --
	 * unless they want scalar data.
	 */
	   case OrgIRGrid:
	   	if (dobj->do_org == OrgIRGrid)
		{
		   	start[1] = 0;
			count[1] = tag->nc_nPlat;
		}
		else if (dobj->do_org == OrgScalar)
		{
			if (SPMap[dobj->do_id] == UNKNOWN)
				msg_ELog (EF_PROBLEM,
					"Scalar access from unknown plat %s",
					ds_PlatformName (dobj->do_id));
			else if (SPMap[dobj->do_id] == BASEDONE)
				msg_ELog (EF_PROBLEM,
					"Scalar access from %s impossible",
					ds_PlatformName (dobj->do_id));
			else
			{
				start[1] = SPMap[dobj->do_id];
				count[1] = 1;
			}
		}
		else
			msg_ELog (EF_PROBLEM, "Orgs %d / %d incompatible",
				tag->nc_org, dobj->do_org);
		break;
	/*
	 * If we have 3D grid data, we assume that grids are what they want,
	 * and look into yanking out one level.
	 */
	   case Org3dGrid:
	   	if (dobj->do_org == Org2dGrid)
		{
			start[1] = (dobj->do_loc.l_alt - tag->nc_sloc.l_alt)/
					tag->nc_rgrid.rg_Zspacing + 0.5;
			if (start[1] < 0)
				start[1] = 0;
			else if (start[1] >= tag->nc_rgrid.rg_nZ)
				start[1] = tag->nc_rgrid.rg_nZ - 1;
msg_ELog (EF_DEBUG, "Alt %.2f -> level %d", dobj->do_loc.l_alt, start[1]);
			dobj->do_loc.l_alt = tag->nc_sloc.l_alt + 
				start[1]*tag->nc_rgrid.rg_Zspacing;
			count[1] = 1;
		}
		else if (dobj->do_org == Org3dGrid)
		{
			start[1] = 0;
			count[1] = tag->nc_rgrid.rg_nZ;
		}
		else
		{
			msg_ELog (EF_PROBLEM, "Orgs %d / %d incompatible",
				tag->nc_org, dobj->do_org);
			break;
		}
		start[2] = start[3] = 0;
		count[2] = tag->nc_rgrid.rg_nY;
		count[3] = tag->nc_rgrid.rg_nX;
		break;
	/*
	 * Scalar files index only by time, so we do nothing here.
	 */
	   case OrgScalar:
	   	count[1] = 1;
	   	break;

	   default:
	   	msg_ELog (EF_PROBLEM, "File org %d not handled", tag->nc_org);
		break;
	}
}








/* static */ int
dnc_TimeIndex (tag, t)
NCTag *tag;
time *t;
/*
 * Figure out how far into this file we have to go to get to this time.
 */
{
	long offset;
	int i;
/*
 * Find out the time offset from the beginning of the file.
 */
	offset = TC_FccToSys (t) - tag->nc_base;
/*
 * Check the extreme cases.
 */
	if (offset <= tag->nc_times[0])
		return (0);
	else if (offset >= tag->nc_times[tag->nc_ntime - 1])
		return (tag->nc_ntime - 1);
/* 
 * OK, search for it.  Someday we'll make this a binary search or something,
 * but, for now....
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

	if (! dfa_OpenFile (dfindex, (void *) &tag))
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

	if (! dfa_OpenFile (dfindex, (void *) &tag))
		return;
	memcpy (loc, tag->nc_locs, tag->nc_nPlat*sizeof (Location));
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
	if (! dfa_OpenFile (dfindex, (void *) &tag))
		return (FALSE);
	if (tag->nc_org != Org2dGrid && tag->nc_org != Org3dGrid)
	{
		msg_ELog (EF_PROBLEM, "BUG: GetRGrid on non-grid file");
		return (FALSE);
	}
/*
 * Yank the info out of the file.
 */
	alt = origin->l_alt;	/* Kludgy	*/
	*origin = tag->nc_sloc;
	*rg = tag->nc_rgrid;
	level = (alt - tag->nc_sloc.l_alt)/tag->nc_rgrid.rg_Zspacing + 0.5;
	origin->l_alt = tag->nc_rgrid.rg_Zspacing*level + tag->nc_sloc.l_alt;
	return (TRUE);
}






int
dnc_DataTimes (index, when, which, n, dest)
int index, n;
time *when, *dest;
TimeSpec which;
/*
 * Find out when data is available.
 */
{
	NCTag *tag;
	int t, i;
/*
 * Get the file open.
 */
	if (! dfa_OpenFile (index, (void *) &tag))
		return;
	t = dnc_TimeIndex (tag, when);
/*
 * Copy out the info.
 */
	for (i = 0; t >= 0 && i < n; i++)
	{
		long st = tag->nc_base + (int) tag->nc_times[t];
		TC_SysToFcc (st, dest);
		dest++;
		t--;
	}
	return (i);
}
