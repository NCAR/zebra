/*
 * Access to netCDF files.
 */
static char *rcsid = "$Id: DFA_NetCDF.c,v 1.1 1990-11-02 08:56:17 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "dfa.h"
# include "DataStore.h"
# include "dsPrivate.h"

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
	int	nc_nPlat;	/* Number of platforms		*/
} NCTag;




# ifdef __STDC__
	static void	dnc_NCError (char *);
	static int	dnc_OFTimes (NCTag *);
	static int	dnc_GetTimes (NCTag *);
	static int	dnc_OFIRGrid (NCTag *);
# else
	static void 	dnc_NCError ();
	static int	dnc_OFTimes ();
	static int	dnc_GetTimes ();
	static int	dnc_OFIRGrid ();
# endif





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
	tag->nc_org = dp->df_ftype;
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
	   case OrgIRGrid:
	   	ret = dnc_OFIRGrid (tag);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Can't deal with org %d", tag->nc_org);
		ret = FALSE;
		break;
	}

	if (! ret)
		dnc_CloseFile (tag);
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
			(lon = ncvarid (tag->nc_id, "lon") < 0) ||
			(alt = ncvarid (tag->nc_id, "alt") < 0))
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
	stop = tag->nc_nPlat - 1;
	ncvarget (tag->nc_id, lat, &start, &stop, pos);
	for (i = 0; i < tag->nc_nPlat; i++)
		tag->nc_locs[i].l_lat = pos[i];
	ncvarget (tag->nc_id, lon, &start, &stop, pos);
	for (i = 0; i < tag->nc_nPlat; i++)
		tag->nc_locs[i].l_lon = pos[i];
	ncvarget (tag->nc_id, alt, &start, &stop, pos);
	for (i = 0; i < tag->nc_nPlat; i++)
		tag->nc_locs[i].l_alt = pos[i];
/*
 * All done.
 */
	free (pos);
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
	if (ndim != 1 || dims[0] != NC_FLOAT)
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
	ntime--;
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
