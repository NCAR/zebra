/*
 * Deal with GRIB format files
 */
/*		Copyright (C) 1993 by UCAR
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


# include <sys/types.h>
# include <math.h>
# include <errno.h>
# include <fcntl.h>
# include <unistd.h>

# include "defs.h"
# include "message.h"
# include "dfa.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"

MAKE_RCSID ("$Id: DFA_GRIB.c,v 3.2 1994-02-24 20:03:08 burghart Exp $")

/*
 * The GRIB product definition section (PDS)
 */
typedef struct s_GFpds
{
	unsigned char	len;		/* length of PDS		*/
	unsigned char	len1;		/* 2nd byte of length		*/
	unsigned char	len2;		/* 3rd byte of length		*/
	unsigned char	pt_version;	/* Param. Table version number	*/
	unsigned char	center_id;	/* ID of source center		*/
	unsigned char	process_id;	/* ID of generating process	*/
	unsigned char	grid_id;	/* grid type ID			*/
	unsigned char	section_flags;	/* do we have a GDS and/or BMS?	*/
	unsigned char	field_id;	/* parameter and units indicator */
	unsigned char	level_id;	/* level type indicator		*/
	unsigned char	level_val;	/* level value			*/
	unsigned char	level_val1;	/* level value (2nd byte)	*/
	unsigned char	year;		/* year % 100			*/
	unsigned char	month;
	unsigned char	day;
	unsigned char	hour;
	unsigned char	minute;
	unsigned char	time_unit;	/* time unit ID			*/
	unsigned char	p1;
	unsigned char	p2;
	unsigned char	range_id;	/* time range indicator		*/
	unsigned char	num_in_avg;	/* num. of points used if averaging */
	unsigned char	num_in_avg1;	/* 2nd byte			*/
	unsigned char	avg_missing;	/* num. missing when averaging	*/
	unsigned char	century;	/* century (20 until 1 Jan 2001)*/
	unsigned char	reserved0;
	/* 
	 * We can use short for ds_factor; it falls on an even byte boundary
	 */
	short		ds_factor;	/* decimal scale factor		*/
	unsigned char	reserved1;	/* any length of reserved data	*/
} GFpds;

/*
 * Flag bits for section_flags
 */
# define GDS_FLAG	1
# define BMS_FLAG	2

/*
 * Binary Data Section header
 */
typedef struct s_BDShdr
{
	char		bds_len;	/* length of BDS		*/
	char		bds_len1;	/* 2nd byte of length		*/
	char		bds_len2;	/* 3rd byte of length		*/
	unsigned char	flag_ubits;	/* Flag & number of unused bits	*/
	/* 
	 * We can use short for bs_factor; it falls on an even byte boundary
	 */
	short		bs_factor;	/* binary scale factor		*/
	char		ref_top;	/* sign & characteristic bits	*/
					/* of reference value		*/
	char		ref_mant;	/* mantissa of reference value	*/
	char		ref_mant1;	/* 2nd byte of mantissa		*/
	char		ref_mant2;	/* 3rd byte of mantissa		*/
	unsigned char	n_bits;		/* bits per datum		*/
} BDShdr;

/*
 * GRIB record descriptor
 */
typedef struct s_GRIBdesc
{
	GFpds	*gd_pds;	/* Product definition section	*/
	long	gd_doffset;	/* offset to binary data section*/
	int	gd_bds_len;	/* binary data section length	*/
	ZebTime	gd_time;
} GRIBdesc;


/*
 * Our tag structure for GRIB files.
 */
typedef struct s_GFTag
{
	int		gt_tagid;	/* unique local ID		*/
	int		gt_fd;		/* file descriptor		*/
	int		gt_ngrids;	/* grid count			*/
	int		gt_maxgrids;	/* how many grids can we hold?	*/
	GRIBdesc	*gt_grib;	/* Descriptors for each grid	*/
} GFTag;


/*
 * For each GRIB type we understand, we keep the following concerning the 
 * original (source) grid:
 *
 *	gg_type:	integer grid type
 *	gg_snx, gg_sny:	width and height of the GRIB source grid
 *	gg_ndx_module:	routine for converting from lat/lon to source grid
 *			indices
 *	gg_ll_module:	routine for converting from source grid indices to
 *			lat/lon
 *
 * and for the destination grid into which we remap the data, we keep
 * the following:
 *
 *	gg_dnx, gg_dny:		width and height of the destination grid
 *	gg_dlat, gg_dlon:	lat/lon origin of the destination grid
 *	gg_dlatstep, gg_dlonstep:	lat and lon step in the dest. grid
 */
typedef struct s_GRB_TypeInfo
{
	int	gg_type;
	int	gg_snx, gg_sny;
	void	(*gg_ndx_module)();
	void	(*gg_ll_module)();
	int	gg_dnx, gg_dny;
	float	gg_dlat, gg_dlon;
	float	gg_dlatstep, gg_dlonstep;
} GRB_TypeInfo;

/*
 * One reasonable spherical radius for the earth, in km
 */
const double	R_Earth = 6367.47;

/*
 * Degree to radian converter and vice versa
 */
# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define RAD_TO_DEG(x)	((x) * 57.29577951)

/*
 * Local prototypes
 */
static GFTag	*grb_Open FP ((char *));
static int	grb_ScanFile FP ((GFTag *));
static int	grb_TimeIndex FP ((GFTag *, ZebTime *, int));
static void	grb_DestroyTag FP ((GFTag *));
static int	grb_TwoByteInt FP ((char *));
static int	grb_ThreeByteInt FP ((char *));
static void	grb_ReadRGrid FP ((DataChunk *, GFTag *, GRB_TypeInfo *, int, 
				   int, int, FieldId, float *, dsDetail *, 
				   int));
static FieldId	grb_Field FP ((GFpds *, ScaleInfo *));
static int	grb_Offset FP ((GFpds *));
static bool	grb_NormalLevel FP ((GFpds *));
static float	grb_ZLevel FP ((GFpds *, AltUnitType *));
static void	grb_105Index FP ((double, double, float *, float *));
static void	grb_105LatLon FP ((double, double, float *, float *));
static void	grb_UnpackBDS FP ((GFTag *, int, float *, int, int));
static void	grb_ResetWind FP ((void));
static void	grb_UnpackWind FP ((GFTag *, FieldId, int, int, float *, int,
				    int, void (*)(), void (*)()));
static void	grb_FixWind FP ((float *, float *, int, int, void (*)(), 
				 void (*)()));
static void	grb_DCInit FP ((DataChunk *, FieldId *, int));
static void	grb_DCFinishDefs FP ((DataChunk *, GRB_TypeInfo *, int));

/*
 * Field list.  We only include here the fields for which we have
 * established names.  Other fields just become "gribX" where X is 
 * the GRIB field number.
 */
struct s_GRB_FList 
{
	int	fnum;
	char	*fname;
	float	scale, offset;
} GRB_FList[] = 
{
	/* Pressure (Pa), scale to mb */
	{ 1, "pres", 100.0, 0.0 },
	/* Pressure reduced to MSL (Pa), scale to mb */
	{ 2, "cpres0", 100.0, 0.0 },
	/* Geometric height (m) */
	{ 8, "height", 1.0, 0.0 },
	/* Temperature (K), scale to C */
	{ 11, "tdry", 1.0, -273.15 },
	/* Virtual temperature (K) */
	{ 12, "vt", 1.0, 0.0 },
	/* Potential temperature (K) */
	{ 13, "pt", 1.0, 0.0 },
	/* Dew point temperature (K) */
	{ 17, "dp", 1.0, -273.15 },
	/* Wind direction (deg. true) */
	{ 31, "wdir", 1.0, 0.0 },
	/* Wind speed (m/s) */
	{ 32, "wspd", 1.0, 0.0 },
	/* u component of wind (m/s) */
	{ 33, "u_wind", 1.0, 0.0 },
	/* v component of wind (m/s) */
	{ 34, "v_wind", 1.0, 0.0 },
	/* geometric vertical velocity (m/s) */
	{ 40, "w_wind", 1.0, 0.0 },
	/* Absolute vorticity ( /s) */
	{ 41, "vort", 1.0, 0.0 },
	/* Absolute divergence ( /s) */
	{ 42, "dvrg", 1.0, 0.0 },
	/* Relative humidity (%) */
	{ 52, "rh", 1.0, 0.0 },
	/* Humidity mixing ratio (kg/kg), scale to g/kg */
	{ 53, "mr", 0.001, 0.0 },
};

int GRB_FList_len = sizeof (GRB_FList) / sizeof (struct s_GRB_FList);
	


/*
 * Info for GRIB grid types we know how to unpack.
 */
GRB_TypeInfo GRB_Types[] =
{
	/*
	 * 105: 6889-point (83x83) N. Hemisphere polar stereographic grid
	 * oriented 105W; pole at (39.5,87.5) [C type indexing].  (U.S. area
	 * subset of NGM Super C grid, used by ETA model).  90.75464 km
	 * spacing at 60N.
	 */
	{ 105, 83, 83, grb_105Index, grb_105LatLon, 71, 41, 20.0, -130.0, 
		  1.0, 1.0 },
};

int GRB_NTypes = sizeof (GRB_Types) / sizeof (GRB_TypeInfo);

/*
 * How many vertical levels can we handle?
 */
# define MAXLEVELS	40

/*
 * Winds info.  Since this stuff takes a while to calculate, we keep track
 * of the ones we've already extracted so we can use them again if possible.
 */
int	WindsTagID = -1;
int	WindsCount = 0;

int	U_gridnum[MAXLEVELS], V_gridnum[MAXLEVELS];
float	*U_data[MAXLEVELS], *V_data[MAXLEVELS];






int
grb_QueryTime (file, begin, end, nsample)
char	*file;
ZebTime	*begin, *end;
int	*nsample;
/*
 * Tell the daemon what's in this file.
 */
{
	int fd, year, month, day, hour, minute;
	GFpds	*pds;
	GFTag	*tag;
/*
 * Get the file tag.
 */
	if (! (tag = grb_Open (file)))
		return (FALSE);
/*
 * Make the (not guaranteed but somewhat reasonable) assumption that the
 * data were written in chronological order.  I.e., get the begin and end
 * times from the first and last grids from the file, respectively.
 */
	*begin = tag->gt_grib[0].gd_time;
	*end = tag->gt_grib[tag->gt_ngrids - 1].gd_time;
/*
 * Get rid of the tag and return
 */
	close (tag->gt_fd);
	grb_DestroyTag (tag);

	return (TRUE);
}





void
grb_MakeFileName (dir, name, zt, string)
ZebTime	*zt;
char	*dir, *name, *string;
/*
 * Generate a new file name.
 */
{
	UItime t;
	
	TC_ZtToUI (zt, &t);
	sprintf (string, "%s.%06d.%06d.grib", name, t.ds_yymmdd, t.ds_hhmmss);
}





int
grb_CreateFile (fname, dfile, dc, rtag)
char	*fname;
DataFile	*dfile;
DataChunk	*dc;
char	**rtag;
/*
 * Create a new GRIB file.
 */
{
	msg_ELog (EF_PROBLEM, "grb_CreateFile: Can't create GRIB files yet!");
	return (FALSE);
}






int
grb_PutSample (dfile, dc, sample, wc)
int	dfile, sample;
DataChunk	*dc;
WriteCode	wc;
/*
 * Put data into this file.
 */
{
	msg_ELog (EF_PROBLEM, "grb_PutSample: Can't write GRIB files yet!");
	return (FALSE);
}





int
grb_OpenFile (fname, dp, write, rtag)
char	*fname;
DataFile	*dp;
bool	write;
char	**rtag;
/*
 * DFA routine to open a file and return a tag.
 */
{
	GFTag	*tag;

	if (! (tag = grb_Open (fname)))
		return (FALSE);

	*rtag = (char *) tag;
	return (TRUE);
}




void
grb_CloseFile (vtag)
void	*vtag;
/*
 * Close this file.
 */
{
	GFTag	*tag = (GFTag *) vtag;

	close (tag->gt_fd);
	grb_DestroyTag (tag);
}




int
grb_SyncFile (vtag)
void	*vtag;
/*
 * Catch up with changes in this file.
 */
{
	GFTag	*tag = (GFTag *) vtag;

	grb_ScanFile (tag);
	return (TRUE);
}




DataChunk *
grb_Setup (gp, fields, nfield, class)
GetList	*gp;
FieldId	*fields;
int	nfield;
DataClass	class;
/*
 * Get set up to do this data grab.
 */
{
	GFTag	*tag;
	DataChunk	*dc;
/*
 * Do some sanity checking.
 */
	if (class != DCC_NSpace)
	{
		msg_ELog (EF_PROBLEM, "Non-NSpace fetch from GRIB file");
		return (NULL);
	}
/*
 * Open this file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (NULL);
/*
 * Create an NSpace data chunk
 */
	dc = dc_CreateDC (DCC_NSpace);
/*
 * Put in preliminary dimension and field definitions.  The real dimension
 * sizes get set by grb_DCFinishDefs() when the data grab happens.  We have
 * to do some here, though, because the data chunk is the only source for
 * field information when the data grab occurs.  Note that dc_GetFields()
 * will return zero fields for the data chunk in the state we return it,
 * since the fields aren't actually "defined" for NSpace data chunks until
 * we call dc_NSDefineComplete(), and that doesn't happen until the call
 * to grb_DCFinishDefs().  Is that sufficiently confusing?
 */
	grb_DCInit (dc, fields, nfield);
	dc_NSAllowRedefine (dc, TRUE);
	
	return (dc);
}




int
grb_GetData (dc, gp, details, ndetail)
DataChunk	*dc;
GetList		*gp;
dsDetail	*details;
int		ndetail;
/*
 * Get the data from this GetList entry.
 */
{
	int	ndx, firstndx, lastndx, nfield, samp, sbegin, send, f;
	int	nalts, gtype, i;
	float  	badval, ztarget, *lats, *lons, alts[MAXLEVELS];
	bool	onelevel;
	SValue	v;
	GFTag	*tag;
	FieldId	*fids, lat_id, lon_id, alt_id;
	ZebTime	stime;
	GRB_TypeInfo	*grbinfo;
	AltUnitType	altunits;
/*
 * Open this file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Get the grid type and get the entry from our types table.  (We use the
 * first grid in the file, assuming that all grids in the file are of the
 * same type).
 */
	gtype = tag->gt_grib[0].gd_pds->grid_id;

	grbinfo = NULL;
	for (i = 0; i < GRB_NTypes; i++)
	{
		if (GRB_Types[i].gg_type == gtype)
		{
			grbinfo = GRB_Types + i;
			break;
		}
	}

	if (! grbinfo)
	{
		msg_ELog (EF_PROBLEM, "Cannot unpack GRIB grid type %d\n", 
			  gtype);
		grb_CloseFile ((void *) tag);
		return (FALSE);
	}
/*
 * Get the list of available alts and set the altitude units in our
 * data chunk.  We assume that we have the same number of levels (alts)
 * for all fields.
 */
	grb_GetAlts (gp->gl_dfindex, alts, &nalts, &altunits);
	dc_SetLocAltUnits (dc, altunits);
/*
 * If they just want one level, find the closest one.
 */
	onelevel = ds_GetDetail ("altitude", details, ndetail, &v);
	if (onelevel)
	{
		float	diff, bestdiff = 99e99;
		int	best;

		ztarget = v.us_v_float;

		for (i = 0; i < nalts; i++)
		{
			diff = abs (ztarget - alts[i]);
			if (diff < bestdiff)
			{
				bestdiff = diff;
				best = i;
			}
		}

		ztarget = alts[best];
	}
/*
 * Make the dimension info in the data chunk correct if we haven't done so
 * already.
 */
	if (! dc_NSDefineIsComplete (dc))
		grb_DCFinishDefs (dc, grbinfo, onelevel ? 1 : nalts);
/*
 * Set the values for the lat and lon coordinate variables.
 */
	lons = (float *) malloc (grbinfo->gg_dnx * sizeof (float));
	for (i = 0; i < grbinfo->gg_dnx; i++)
		lons[i] = grbinfo->gg_dlon + i * grbinfo->gg_dlonstep;

	lon_id = F_Lookup ("lon");
	dc_NSAddStatic (dc, lon_id, (void *) lons);

	free (lons);
	
	lats = (float *) malloc (grbinfo->gg_dny * sizeof (float));
	for (i = 0; i < grbinfo->gg_dny; i++)
		lats[i] = grbinfo->gg_dlat + i * grbinfo->gg_dlatstep;

	lat_id = F_Lookup ("lat");
	dc_NSAddStatic (dc, lat_id, (void *) lats);

	free (lats);
/*
 * Same for altitude.
 */
	alt_id = F_Lookup ("alt");
	dc_NSAddStatic (dc, alt_id, (void *) (onelevel ? &ztarget : alts));
/*
 * Set the badval in the data chunk, either based on the details we're given
 * or by using the default.
 */
	badval = ds_GetDetail ("badval", details, ndetail, &v) ?
		v.us_v_float : -9999.0;

	dc_SetBadval (dc, badval);
/*
 * Find the indices that bound our data search.
 */
	firstndx = grb_TimeIndex (tag, &gp->gl_begin, TRUE);
	lastndx = grb_TimeIndex (tag, &gp->gl_end, FALSE);
/*
 * Do things on a sample by sample basis
 */
	samp = 0;

	for (sbegin = firstndx; sbegin <= lastndx; sbegin = send + 1)
	{
	/*
	 * Time for this sample
	 */
		stime = tag->gt_grib[sbegin].gd_time;
	/*
	 * Find the index limits of the next sample
	 */
		send = sbegin;

		for (ndx = sbegin + 1; ndx <= lastndx; ndx++)
		{
			if (TC_Eq (stime, tag->gt_grib[ndx].gd_time))
				send = ndx;
			else
				break;
		}
	/*
	 * Get the field list
	 */
		fids = dc_GetFields (dc, &nfield);
	/*
	 * Do each field for this sample
	 */
		for (f = 0; f < nfield; f++)
		{
		/*
		 * Ignore our coordinate variables
		 */
			if (fids[f] == lat_id || fids[f] == lon_id ||
			    fids[f] == alt_id)
				continue;
		/*
		 * "Real" data field
		 */
			grb_ReadRGrid (dc, tag, grbinfo, samp, sbegin, send, 
				       fids[f], onelevel ? &ztarget : NULL,
				       details, ndetail);
		}
		

		samp++;
	}
	return (TRUE);
}





static int
grb_TimeIndex (tag, zt, first)
GFTag	*tag;
ZebTime	*zt;
int	first;
/*
 * Find the offset into this file for the first time before or equal to
 * the given time.  If 'first' is true, return the index of the first
 * grid at the time we find.  Otherwise, return the index of the last grid
 * at that time.
 */
{
	int	ndx;
	ZebTime	foundtime;
/*
 * Search backward through the grids until we find one before the requested
 * time.
 */
	for (ndx = tag->gt_ngrids - 1; ndx >= 0; ndx--)
		if (TC_LessEq (tag->gt_grib[ndx].gd_time, *zt))
			break;

	if (ndx < 0)
		return (-1);
/*
 * If they want the last grid for this time, we're there
 */
	if (! first)
		return (ndx);
/*
 * Otherwise, go back so that we return the index of the first grid at
 * the time we just found.  We first try a shortcut, since a lot of files
 * contain only one time anyway.
 */
	foundtime = tag->gt_grib[ndx].gd_time;

	if (TC_Eq (tag->gt_grib[0].gd_time, foundtime))
		return (0);

	for (ndx-- ; ndx >= 0; ndx--)
		if (TC_Less (tag->gt_grib[ndx].gd_time, foundtime))
			break;

	return (++ndx);
}




int
grb_GetAlts (dfindex, alts, nalts, altunits)
int	dfindex;
float	*alts;
int	*nalts;
AltUnitType	*altunits;
/*
 * Fill in the rgrid info for this grid file.
 */
{
	int	i, offset, count;
	FieldId	fid;
	GFpds	*pds;
	GFTag	*tag;
	ZebTime	t;
/*
 * Make sure that the file is open.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Vertical level units from the first grid.  We only deal with
 * type 100 (pressure in mb) and type 103 (altitude in meters MSL)
 */
	switch (tag->gt_grib[0].gd_pds->level_id)
	{
	    case 100:
		if (altunits)
			*altunits = AU_mb;	/* millibars */
		break;
	    case 103:
		if (altunits)
			*altunits = AU_mMSL;	/* meters MSL */
		break;
	    default:
		msg_ELog (EF_PROBLEM, "Can't deal with GRIB level type %d!",
			  tag->gt_grib[0].gd_pds->level_id);
		return (FALSE);
	}
/*
 * Base things on the first grid.  Get altitudes from "normal" grids with 
 * the same field, time, and forecast (offset) time as the first grid.
 */
	fid = grb_Field (tag->gt_grib[0].gd_pds, NULL);
	offset = grb_Offset (tag->gt_grib[0].gd_pds);
	t = tag->gt_grib[0].gd_time;

	count = 0;
	for (i = 0; i < tag->gt_ngrids; i++)
	{
		if (! TC_Eq (tag->gt_grib[i].gd_time, t))
			break;
	/*
	 * Grab the altitude from this grid if it has the same field and time
	 * offset as the first grid, and is a "normal" grid.
	 */
		pds = tag->gt_grib[i].gd_pds;

		if (fid == grb_Field (pds, NULL) &&
		    grb_NormalLevel (pds) && offset == grb_Offset (pds))
		{
			if (alts)
				alts[count++] = (float) 
					grb_TwoByteInt (&(pds->level_val));
			else
				count++;
		}
	}

	if (nalts)
		*nalts = count;

	return (TRUE);
}




int
grb_DataTimes (dfindex, t, which, n, dest)
int	dfindex, n;
ZebTime	*t, *dest;
TimeSpec	which;
/*
 * Return the times for which data is available.
 */
{
	GFTag	*tag;
	int	ndx, tcount;
/*
 * Open this file.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (0);
/*
 * Find the index to the time
 */
	if ((ndx = grb_TimeIndex (tag, t, TRUE)) == -1)
		return (0);
/*
 * Get as many times as requested (or as many as we have)
 */
	*dest = tag->gt_grib[ndx].gd_time;
	tcount = 1;
	
	if (which == DsBefore)
	{
		for (--ndx; ndx >= 0 && tcount < n; ndx--)
		{
			if (! TC_Eq (tag->gt_grib[ndx].gd_time, *dest))
			{
				*++dest = tag->gt_grib[ndx].gd_time;
				tcount++;
			}
		}
	}
	else if (which == DsAfter)
	{
		for (++ndx; ndx < tag->gt_ngrids && tcount < n; ndx++)
		{
			if (! TC_Eq (tag->gt_grib[ndx].gd_time, *dest))
			{
				*++dest = tag->gt_grib[ndx].gd_time;
				tcount++;
			}
		}
	}

	return (tcount);
}




grb_GetFields (dfindex, t, nfld, flist)
int	dfindex;
ZebTime	*t;
int	*nfld;
FieldId	*flist;
/*
 * Return the field list.
 */
{
	int	i, f;
	FieldId	fid;
	GFpds	*pds;
	GFTag	*tag;

	*nfld = 0;
/*
 * Open this file.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return;
/*
 * Get the fields
 */
	for (i = 0; i < tag->gt_ngrids; i++)
	{
		pds = tag->gt_grib[i].gd_pds;
		fid = grb_Field (pds, NULL);
	/*
	 * Add this field to the list if it isn't there already
	 */
		for (f = 0; f < *nfld; f++)
			if (fid == flist[f])
				break;

		if (f == *nfld)
			flist[(*nfld)++] = fid;
	}
}




int
grb_GetObsSamples (dfile, times, locs, max)
int	dfile, max;
ZebTime	*times;
Location	*locs;
/*
 * Return sample info.
 */
{
	GFTag	*tag;
	int	i, gtype, ntimes, ngrids;
	Location	gloc;
/*
 * Get the file open.
 */
	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
/*
 * Get the grid type and use it to determine location
 */
	gtype = tag->gt_grib[0].gd_pds->grid_id;
	for (i = 0; i < GRB_NTypes; i++)
	{
		if (GRB_Types[i].gg_type == gtype)
		{
			gloc.l_lat = GRB_Types[i].gg_dlat;
			gloc.l_lon = GRB_Types[i].gg_dlon;
			gloc.l_alt = 1000.0;	/* BOGUS! */
			break;
		}
	}

	if (i == GRB_NTypes)
	{
		msg_ELog (EF_EMERGENCY, "Cannot unpack GRIB grid type %d\n", 
			  gtype);
		return (0);
	}
/*
 * Shortcut:  See if the first and last grid times in the file are the same.
 * If so, we assume all the ones between are the same, too.
 */
	ngrids = tag->gt_ngrids;
	if (TC_Eq (tag->gt_grib[0].gd_time, tag->gt_grib[ngrids-1].gd_time))
	{
		times[0] = tag->gt_grib[0].gd_time;
		locs[0] = gloc;
		return (1);
	}
/*
 * Nope, we have to go through every grid
 */
	times[0] = tag->gt_grib[0].gd_time;
	locs[0] = gloc;
	ntimes = 1;

	for (i = 1; i < ngrids; i++)
	{
	/*
	 * Skip this one if it's the same time as the last one
	 */
		if (TC_Eq (tag->gt_grib[i].gd_time, times[ntimes-1]))
			continue;
	/*
	 * We have a new time
	 */
		times[ntimes] = tag->gt_grib[i].gd_time;
		locs[ntimes] = gloc;
		ntimes++;

		if (ntimes == max)
			return (max);
	}
	return (ntimes);
}




static GFTag *
grb_Open (fname)
char	*fname;
/*
 * Local file open routine
 */
{
	GFTag	*tag = (GFTag *) calloc (1, sizeof (GFTag));
	static int	tagid = 0; /* Unique id for each tag we create */
/*
 * Try to open the file
 */
	if ((tag->gt_fd = open (fname, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "grb_Open: Error %d opening '%s'", 
			  errno, fname);
		free (tag);
		return (NULL);
	}
/*
 * Now fill in the tag
 */
	if (! grb_ScanFile (tag))
	{
		close (tag->gt_fd);
		grb_DestroyTag (tag);
		return (NULL);
	}

	tag->gt_tagid = tagid++;
	return (tag);
}




static void
grb_DestroyTag (tag)
GFTag	*tag;
/*
 * Destroy the given tag.  Assume that the associated file descriptor has
 * been closed.
 */
{
	int	i;

	for (i = 0; i < tag->gt_ngrids; i++)
		free (tag->gt_grib[i].gd_pds);

	free (tag->gt_grib);
	free (tag);
}



	
static int
grb_ScanFile (tag)
GFTag	*tag;
/*
 * Scan the given file, updating in everything except the file descriptor
 * in the tag.  Return TRUE if everything goes OK.
 */
{
	int	fd = tag->gt_fd, pds_len, gds_len, bms_len, bds_len;
	int	status, ng;
	char	is[8], threebytes[3], trailer[4];
	GFpds	*pds;
/*
 * Rewind the file first
 */
	lseek (fd, 0, SEEK_SET);
/*
 * Each grid starts with an 8 byte Indicator Section.  Loop through grids
 * until we fail to get one of these.
 */
	while ((status = read (fd, is, 8)) == 8)
	{
	/*
	 * Make sure we have the "GRIB" tag at the beginning
	 */
		if (strncmp (is, "GRIB", 4))
		{
			msg_ELog (EF_PROBLEM, "Got '%4s' instead of 'GRIB'!", 
				  is);
			return (FALSE);
		}
	/*
	 * It looks like we really have a GRIB record here, so update
	 * the file tag.
	 */
		ng = ++tag->gt_ngrids;

		if (ng == 1)
		{
			tag->gt_maxgrids = 100;
			tag->gt_grib = (GRIBdesc *) malloc (tag->gt_maxgrids *
							    sizeof (GRIBdesc));
		}
		else if (ng > tag->gt_maxgrids)
		{
			tag->gt_maxgrids += 100;
			tag->gt_grib = (GRIBdesc *) 
				realloc (tag->gt_grib, 
					 tag->gt_maxgrids * sizeof (GRIBdesc));
		}
	/*
	 * Read the first 3 bytes of the Product Definition Section to
	 * get the PDS length.
	 */
		pds = (GFpds *) malloc (sizeof (GFpds));
		read (fd, pds, 3);

		pds_len = grb_ThreeByteInt (&(pds->len));
	/*
	 * Now read the remainder of the PDS, allocating more space if it's
	 * bigger than the normal 28 bytes, and update the tag.
	 */
		if (pds_len > 28)
			pds = realloc (pds, pds_len);

		read (fd, (char *) pds + 3, pds_len - 3);

		tag->gt_grib[ng-1].gd_pds = pds;
	/*
	 * Extract the time from the PDS
	 */
		TC_ZtAssemble (&(tag->gt_grib[ng-1].gd_time), pds->year, 
			       pds->month, pds->day, pds->hour, pds->minute,
			       0, 0);
	/*
	 * If we have a Grid Description Section, read its length and skip it.
	 */
		if (pds->section_flags && GDS_FLAG)
		{
			read (fd, threebytes, 3);
			gds_len = grb_ThreeByteInt (threebytes);
			lseek (fd, gds_len - 3, SEEK_CUR);
		}
	/*
	 * If we have a Bit Map Section, read its length and skip it.
	 */
		if (pds->section_flags && BMS_FLAG)
		{
			read (fd, threebytes, 3);
			bms_len = grb_ThreeByteInt (threebytes);
			lseek (fd, bms_len - 3, SEEK_CUR);
		}
	/*
	 * We should be at the beginning of the Binary Data Section now,
	 * so save our position, get the length of the BDS, and skip it.
	 */
		tag->gt_grib[ng-1].gd_doffset = tell (fd);

		read (fd, threebytes, 3);
		bds_len = tag->gt_grib[ng-1].gd_bds_len = 
			grb_ThreeByteInt (threebytes);
		lseek (fd, bds_len - 3, SEEK_CUR);
	/*
	 * Sanity check.  Make sure the next 4 bytes are the GRIB trailer
	 * "7777"
	 */
		read (fd, trailer, 4);
		if (strncmp (trailer, "7777", 4))
		{
			msg_ELog (EF_EMERGENCY, 
				  "Bad GRIB trailer '%4s' at grid %d",
				  trailer, ng);
			return (FALSE);
		}
	}
/*
 * Complain if we exited on anything other than an EOF
 */
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d reading GRIB file", errno);
		return (FALSE);
	}
	else
		return (TRUE);
}




static int
grb_TwoByteInt (buf)
char	*buf;
/*
 * Extract the first two bytes of buf into an int and return it.
 */
{
	int	i = 0;
	char	*cptr = (char *) &i;

	memcpy (cptr + 2, buf, 2);
	return (i);
}




static int
grb_ThreeByteInt (buf)
char	*buf;
/*
 * Extract the first three bytes of buf into an int and return it.
 */
{
	int	i = 0;
	char	*cptr = (char *) &i;

	memcpy (cptr + 1, buf, 3);
	return (i);
}




static void
grb_ReadRGrid (dc, tag, ginfo, samp, sbegin, send, fid, ztarget, details, 
	       ndetail)
DataChunk	*dc;
GFTag		*tag;
GRB_TypeInfo	*ginfo;
int		samp, sbegin, send, ndetail;
FieldId		fid;
float		*ztarget;
dsDetail	*details;
/*
 * Build a grid of the chosen field, using the GRIB grids between indices
 * sbegin and send inclusive, and stuff it into the data chunk.  If 
 * 'ztarget' is non-NULL, then only one horizontal plane is desired, at the
 * given z level.  Otherwise, return all planes.
 */
{
	int	offset, nsx, nsy, si, sj, i, j;
	int	indices[MAXLEVELS], u_indices[MAXLEVELS], v_indices[MAXLEVELS];
	float	zvals[MAXLEVELS], badval = dc_GetBadval (dc);
	int	level, nlevels, ulevels, vlevels, ndx;
	float	*sgrid, *sp, *dgrid, *dp, *lats, *lons;
	float	x, y, z, di, dj, val0, val1, val2, val3;
	GFpds	*pds;
	ZebTime	time;
	bool	onelevel, u_or_v;
	void	(*index_module)(), (*ll_module)();
	FieldId	grid_fid, u_fid, v_fid;
	SValue	v;
	ScaleInfo	sc;
	unsigned long	dc_nlevels, nlat, nlon;
/*
 * Get the requested forecast offset, in seconds.
 */
	offset = ds_GetDetail (DD_FORECAST_OFFSET, details, ndetail, &v) ?
		v.us_v_int : 0;
/*
 * Are we getting u or v wind?  If so, we actually have to get both.
 */
	u_fid = F_Lookup ("u_wind");
	v_fid = F_Lookup ("v_wind");
	u_or_v = (fid == u_fid || fid == v_fid);
/*
 * Get the lists of lats and lons from the data chunk.
 */
	lats = (float *) dc_NSGetStatic (dc, F_Lookup ("lat"), &nlat);
	lons = (float *) dc_NSGetStatic (dc, F_Lookup ("lon"), &nlon);
/*
 * Build a list of grids that contain our field and have the right forecast
 * time
 */
	nlevels = ulevels = vlevels = 0;

	for (ndx = sbegin; ndx <= send; ndx++)
	{
		pds = tag->gt_grib[ndx].gd_pds;
	/*
	 * Bag this grid now if the forecast time is wrong or it's a
	 * "special" level
	 */
		if (! grb_NormalLevel (pds) || grb_Offset (pds) != offset)
			continue;
	/*
	 * If we just want one level, make sure we get the right one.
	 */
		z = grb_ZLevel (pds, NULL);
		if (ztarget && z != *ztarget)
			continue;
	/*
	 * Now check the field
	 */
		grid_fid = grb_Field (pds, NULL);

		if (grid_fid == fid)
		{
			zvals[nlevels] = z;
			indices[nlevels++] = ndx;
		}

		if (u_or_v)
		{
			if (grid_fid == u_fid)
				u_indices[ulevels++] = ndx;
			else if (grid_fid == v_fid)
				v_indices[vlevels++] = ndx;
		}
	}
/*
 * If we're doing wind, make sure we got the same number of levels for
 * both u and v
 */
	if (u_or_v && ulevels != vlevels)
	{
		msg_ELog (EF_PROBLEM, 
			  "GRIB u_wind and v_wind levels don't match!");
		nlevels = 0;
	}
/*
 * Make sure we're copacetic with the number of levels defined in the
 * data chunk.  This is the verification of the assumption made in 
 * grb_GetData() that we have the same number of vertical levels for all 
 * fields.
 */
	dc_NSGetDimension (dc, F_Lookup ("alt"), NULL, &dc_nlevels);
	if (nlevels && dc_nlevels != nlevels)
	{
		msg_ELog (EF_PROBLEM, "*BUG*: GRIB level count mismatch!");
		nlevels = 0;
	}
/*
 * If we have no levels, create a grid full of badvals and put that in the
 * data chunk.
 */
	if (nlevels == 0)
	{
		float	*grid;
		int	gridsize;

		msg_ELog (EF_INFO, "GRIB: No '%s' data for '%s'", 
			  F_GetName (fid), ds_PlatformName(dc->dc_Platform));
		
		time = tag->gt_grib[sbegin].gd_time;

		gridsize = nlat * nlon * dc_nlevels;
		grid = (float *) malloc (gridsize * sizeof (float));

		for (i = 0; i < gridsize; i++)
			grid[i] = badval;

		dc_NSAddSample (dc, &time, samp, fid, grid);

		free (grid);
		return;
	}
/*
 * Grab some info on the GRIB grid type we're unpacking
 */
	nsx = ginfo->gg_snx;	/* source grid width	*/
	nsy = ginfo->gg_sny;	/* source grid height	*/
	index_module = ginfo->gg_ndx_module;
	ll_module = ginfo->gg_ll_module;
/*
 * Allocate space for the source and destination grids
 */
	sgrid = (float *) calloc (nsx * nsy, sizeof (float));
	dgrid = (float *) malloc (nlat * nlon * nlevels * sizeof (float));
/*
 * Loop through our list of GRIB records, remapping their data into the
 * destination grid.
 */
	dp = dgrid;
	
	for (level = nlevels - 1; level >= 0; level--)
	{
	/*
	 * Get the scaling information for our field and unpack the GRIB
	 * Binary Data Section into sgrid.
	 */
		grb_Field (tag->gt_grib[indices[level]].gd_pds, &sc);

		if (u_or_v)
			grb_UnpackWind (tag, fid, u_indices[level], 
					v_indices[level], sgrid, nsx, nsy,
					index_module, ll_module);
		else
			grb_UnpackBDS (tag, indices[level], sgrid, nsx, nsy);
	/*
	 * Now fill in our destination grid, using a bilinear interpolation
	 * of data from the source grid
	 */
		for (j = 0; j < nlat; j++)
		{
			for (i = 0; i < nlon; i++)
			{
			/*
			 * Get the bounding indices in the source array.
			 * If we're outside the source array, we can get out
			 * quickly.
			 */
				(*index_module)(lats[j], lons[i], &x, &y);

				if (x < 0 || x > nsx-1 || y < 0 || y > nsy-1)
				{
					*dp++ = badval;
					continue;
				}

				si = (int) x;
				sj = (int) y;
			/*
			 * Do a bilinear interpolation using the four source 
			 * grid points (.) surrounding the destination grid 
			 * point (+).  
			 *
			 * Point 0 is at grid position (si,sj) and di and dj 
			 * are fractions of the source grid spacing.
			 *
			 *     2	 3
			 *	.	.
			 *	     +    -
			 *		   |
			 *		   | dj
			 *	.	. -
			 *     0	 1
			 *
			 *	|____|
			 *        di
			 *
			 *
			 *	  val =	(1-di)(1-dj) val0 + (di)(1-dj) val1 +
			 *		(1-di)(dj) val2 + (di)(dj) val3
			 */
				di = x - si;
				dj = y - sj;

				sp = sgrid + sj * nsx + si;
				val0 = *sp;
				val1 = *(sp + 1);
				val2 = *(sp + nsx);
				val3 = *(sp + nsx + 1);

				*dp++ = ((1 - di) * (1 - dj) * val0 +
					di * (1 - dj) * val1 + 
					(1 - di) * dj * val2 + 
					di * dj * val3) / sc.s_Scale + 
					sc.s_Offset;
			}
		}
	}
/*
 * Stuff the grid we just built into the data chunk
 */
	time = tag->gt_grib[sbegin].gd_time;
	dc_NSAddSample (dc, &time, samp, fid, (void *) dgrid);
/*
 * Free our grids and lat/lon arrays
 */
	free (sgrid);
	free (dgrid);
	free (lats);
	free (lons);
}




static void
grb_DCInit (dc, flds, nfld)
DataChunk	*dc;
FieldId	*flds;
int	nfld;
/*
 * Do field and dimension definition for our data chunk.  We set all
 * dimension sizes to one for now, since we don't know the real sizes.
 */
{
	int	f;
	FieldId	dims[3], lat_id, lon_id, alt_id;
/*
 * Define three coordinate variables for our three dimensions: lat, lon, alt.
 */
	lat_id = F_Lookup ("lat");
	dc_NSDefineDimension (dc, lat_id, 1);
	dc_NSDefineVariable (dc, lat_id, 1, &lat_id, TRUE);
	
	lon_id = F_Lookup ("lon");
	dc_NSDefineDimension (dc, lon_id, 1);
	dc_NSDefineVariable (dc, lon_id, 1, &lon_id, TRUE);

	alt_id = F_Lookup ("alt");
	dc_NSDefineDimension (dc, alt_id, 1);
	dc_NSDefineVariable (dc, alt_id, 1, &alt_id, TRUE);
/*
 * Now define the user's fields using these dimensions
 */
	dims[0] = alt_id;
	dims[1] = lat_id;
	dims[2] = lon_id;

	for (f = 0; f < nfld; f++)
		dc_NSDefineVariable (dc, flds[f], 3, dims, FALSE);

	return;
}




static void
grb_DCFinishDefs (dc, grbinfo, nalt)
DataChunk	*dc;
GRB_TypeInfo	*grbinfo;
int		nalt;
/*
 * Redefine the dimensions of our data chunk now, using the correct sizes.
 */
{
/*
 * Three dimensions: lat, lon, alt
 */
	dc_NSDefineDimension (dc, F_Lookup ("lat"), grbinfo->gg_dny);
	dc_NSDefineDimension (dc, F_Lookup ("lon"), grbinfo->gg_dnx);
	dc_NSDefineDimension (dc, F_Lookup ("alt"), nalt);
/*
 * Close out definitions.
 */
	dc_NSDefineComplete (dc);

	return;
}




static FieldId
grb_Field (pds, sc)
GFpds	*pds;
ScaleInfo	*sc;
/*
 * Return the field from this Product Definition Section.  If we are given
 * an address for scale info, return the scale and offset to convert the 
 * units of this field to something amenable to the rest of Zeb.  (New
 * units = original units/s_Scale + s_Offset).
 */
{
	int	i;
	float	scale, offset;
	char	fname[8];
	FieldId	fid;
/*
 * Search through the field table first, to see if we've associated a
 * "real" name with the field
 */
	for (i = 0; i < GRB_FList_len; i++)
	{
		if (pds->field_id == GRB_FList[i].fnum)
		{
			fid = F_Lookup (GRB_FList[i].fname);
			scale = GRB_FList[i].scale;
			offset = GRB_FList[i].offset;
			break;
		}
	}
/*
 * If we didn't find the field in the list, then it becomes "gribX", where
 * X is the GRIB field number.
 */	
	if (i == GRB_FList_len)
	{
		sprintf (fname, "grib%d", pds->field_id);
		fid = F_Lookup (fname);
		scale = 1.0;
		offset = 0.0;
	}
/*
 * Stash the scale and offset if we were given a place for them
 */	
	if (sc)
	{
		sc->s_Scale = scale;
		sc->s_Offset = offset;
	}
/*
 * Return the field id
 */
	return (fid);
}




static int
grb_Offset (pds)
GFpds	*pds;
/*
 * Return the forecast time (offset), in seconds, from the given PDS.
 */
{
	int	multiplier;
/*
 * Get the multiplier for our forecast time units
 */
	switch (pds->time_unit)
	{
	    case 0:
		multiplier = 60;	/* minute */
		break;
	    case 1:
		multiplier = 3600;	/* hour */
		break;
	    case 2:
		multiplier = 86400;	/* day */
		break;
	    case 254:
		multiplier = 1;		/* second */
		break;
	    default:
		msg_ELog (EF_EMERGENCY, 
			  "GRIB forecast time units too big!  Using zero.");
		return (0);
	}
/*
 * For now, we only deal with time range indicators 0 and 1.
 */
	if (pds->range_id > 1)
	{
		msg_ELog (EF_EMERGENCY,
		  "Can't deal with GRIB range indicator %d! Using zero.",
		  pds->range_id);
		return (0);
	}

	if (pds->range_id == 0)
		return (multiplier * pds->p1);
	else if (pds->range_id == 1)
		return (0);
}




static bool
grb_NormalLevel (pds)
GFpds	*pds;
/*
 * Return TRUE if the level of this GRIB grid is "normal" (i.e., a specific
 * isobaric level or height, rather than earth surface, cloud base, tropopause,
 * etc.)
 */
{
	int	l_id = pds->level_id;

	return (l_id == 100 || l_id == 103);
}




static float
grb_ZLevel (pds, units)
GFpds	*pds;
AltUnitType	*units;
/*
 * Return the vertical level from the given PDS.  If 'units' is non-NULL,
 * return the units type.
 */
{
	int	l_id = pds->level_id;

	switch (l_id)
	{
	    case 100:
		if (units)
			*units = AU_mb;		/* millibars */
		break;
	    case 103:
		if (units)
			*units = AU_mMSL;	/* meters MSL */
		break;
	    default:
		msg_ELog (EF_PROBLEM, "Can't deal with GRIB level type %d!",
			  l_id);
		return (-1.0);
	}

	return ((float) grb_TwoByteInt (&(pds->level_val)));
}




static void
grb_105Index (lat, lon, ifloat, jfloat)
double	lat, lon;
float	*ifloat, *jfloat;
/*
 * Return the (floating point) array indices for a GRIB 105 type grid, given a
 * latitude and longitude.  The formulas used here come from Section 21
 * (Stereographic Projection) of "Map Projections--A Working Manual", USGS
 * Professional Paper 1395.  Variable names have been chosen to correspond to
 * those used in the book, and equation numbers from the book are referenced
 * in the comments.
 */
{
	float	x, y, k, psi = DEG_TO_RAD (lat), lambda = DEG_TO_RAD (lon);
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 105 [6889-point (83x83) N. hemisphere polar stereographic
 * grid].  For type 105, the pole is at grid location (40.5, 88.5), in
 * Fortran terms, or (39.5, 87.5) here in the C world.
 */
	const float	psi1 = 1.047197551;	/* 60.0 deg. north */
	const float	lambda0 = -1.832595715;	/* 105.0 deg. west */
	const float	scale = 90.75464;
	const float	ipole = 39.5;
	const float	jpole = 87.5;
/*
 * Applying the formulas below to the north pole yields the following x 
 * and y.
 */
	const float	xpole = 0.0;
	const float	ypole = 3412.31689;
/*
 * Formula 21-4
 */
	k = 2 / (1 + sin (psi1) * sin (psi) + 
		 cos (psi1) * cos (psi) * cos (lambda - lambda0));
/*
 * Formulas 21-2 and 21-3
 */
	x = R_Earth * k * cos (psi) * sin (lambda - lambda0);
	y = R_Earth * k * (cos (psi1) * sin (psi) - 
		 sin (psi1) * cos (psi) * cos (lambda - lambda0));
/*
 * Now turn x and y into grid coordinates based on the north pole, which is
 * the only reference point for which we have grid coordinates.
 */
	*ifloat = ipole + (x - xpole) / scale;
	*jfloat = jpole + (y - ypole) / scale;
}




static void
grb_105LatLon (idouble, jdouble, lat, lon)
double	idouble, jdouble;
float	*lat, *lon;
/*
 * Turn the (double precision) indices into a GRIB 105 type grid into
 * a latitude and longitude.  The formulas used here come from Section 21
 * (Stereographic Projection) of "Map Projections--A Working Manual", USGS
 * Professional Paper 1395.  Variable names have been chosen to correspond to
 * those used in the book, and equation numbers from the book are referenced
 * in the comments.
 */
{
	float	x, y, rho, c, psi, lambda;
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 105 [6889-point (83x83) N. hemisphere polar stereographic
 * grid].  For type 105, the pole is at grid location (40.5, 88.5), in
 * Fortran terms, or (39.5, 87.5) here in the C world.
 */
	const float	psi1 = 1.047197551;	/* 60.0 deg. north */
	const float	lambda0 = -1.832595715;	/* 105.0 deg. west */
	const float	scale = 90.75464;
	const float	ipole = 39.5;
	const float	jpole = 87.5;
/*
 * The x and y values below for the pole were calculated given the
 * constants above.
 */
	const float	xpole = 0.0;
	const float	ypole = 3412.31689;
/*
 * First turn our indices into x and y in km.
 */
	x = (scale * (idouble - ipole)) + xpole;
	y = (scale * (jdouble - jpole)) + ypole;
/*
 * Formulas 20-18 and 21-15
 */
	rho = hypot (x, y);
	c = 2 * atan ((double)(rho / (2 * R_Earth)));
/*
 * Formula 20-15
 */
	lambda = lambda0 + 
		atan (x * sin (c) / 
		      (rho * cos (psi1) * cos (c) - y * sin (psi1) * sin (c)));
/*
 * Formula 20-14
 */
	psi = asin (cos (c) * sin (psi1) + (y * sin (c) * cos (psi1) / rho));
/*
 * Now convert to degrees and we're done
 */
	*lat = RAD_TO_DEG (psi);
	*lon = RAD_TO_DEG (lambda);
}




static void
grb_UnpackBDS (tag, which, grid, nx, ny)
GFTag	*tag;
int	which, nx, ny;
float	*grid;
/*
 * Unpack the Binary Data Section from the which'th GRIB record in tag into 
 * grid, which is (nx x ny).
 */
{
	int	bds_len, sign, mantissa, exponent, n_bits, i, j;
	int	longbits, firstbit, firstbyte, shift;
	unsigned long	bits, mask;
	bool	int_data;
	char	flag, *bds;
	float	ref, bscale, dscale, *gp;
	GFpds	*pds = tag->gt_grib[which].gd_pds;
	BDShdr	*bds_hdr;
/*
 * Allocate space for the BDS, move to the beginning of the BDS in the file 
 * and read it.
 */
	bds_len = tag->gt_grib[which].gd_bds_len;

	bds = (char *) malloc (bds_len);
	bds_hdr = (BDShdr *) bds;

	lseek (tag->gt_fd, tag->gt_grib[which].gd_doffset, SEEK_SET);
	read (tag->gt_fd, bds, bds_len);
/*
 * Four bit BDS flag, the top four bits of flag_ubits:
 *
 *		1	2	3	4
 *		|	|	|	|
 *   grid point data?	| 	|    additional flags in byte 14?
 *			|	|
 *	simple packing (0)   original data were integers?
 *		or
 *	2nd order packing (1)
 *
 * We only deal with grid point data, simple packing, and no additional
 * flags.
 */
	flag = (bds_hdr->flag_ubits >> 4) & 0xF;
	if ((flag & 0x0d) != 0)
	{
		msg_ELog (EF_EMERGENCY,
			  "Can't unpack GRIB BDS with flags: %x", flag);
		return;
	}

	int_data = (flag & 0x2);
/*
 * Build our reference value.  The GRIB documentation gives us:
 *
 *	        S    -24         (A - 64)
 *	R = (-1)  * 2    * B * 16
 *
 * where the 8-bit value 'SAAAAAAA' is ref_top and the 24 bit unsigned 
 * mantissa B is ref_mant in the code below.
 *
 * A simplification of part of the formula:
 *
 *	 -24     (A - 64)             (4A - 280)
 *	2    * 16           -->      2
 *
 * yields the formula for the exponent below.
 */
	sign = (bds_hdr->ref_top & 0x80) ? -1 : 1;

	exponent = 4 * (bds_hdr->ref_top & 0x7F) - 280;
	mantissa = grb_ThreeByteInt (&(bds_hdr->ref_mant));

	ref = sign * mantissa * exp2 ((double) exponent);
/*
 * From GRIB documentation:
 *	               E       -D
 *	Y = (R + (X * 2 )) * 10
 *                                   -D                     E
 * We calculate the decimal scale (10  ) and binary scale (2 ) here.
 */
	dscale = exp10 (-(pds->ds_factor));
	bscale = exp2 (bds_hdr->bs_factor);
/*
 * Check the bit count and build a mask with the appropriate number of bits set
 */
	n_bits = bds_hdr->n_bits;
	longbits = 8 * sizeof (long);

	if (n_bits > longbits - 8)
	{
		msg_ELog (EF_EMERGENCY, "Can't unpack GRIB data > %d bits!",
			  longbits - 8);
		return;
	}

	mask = 0;
	for (i = 0; i < n_bits; i++)
		mask = (mask << 1 | 0x1);
/*
 * Constant field if n_bits == 0
 */
	if (n_bits == 0)
	{
		for (i = 0; i < nx * ny; i++)
			grid[i] = ref * dscale;

		return;
	}
/*
 * We have all the scaling factors, so now loop through and extract all the
 * values.  The data in the BDS start at the southwest corner (at least for
 * northern hemisphere data) and have the longitude index [columns] varying
 * most rapidly).
 */
	firstbit = -n_bits;
	gp = grid;

	for (j = 0; j < ny; j++)
	{
		for (i = 0; i < nx; i++)
		{
		/*
		 * Find the first bit and first byte of this point and 
		 * calculate the shift to move our bits to the bottom of 
		 * a long.
		 */
			firstbit += n_bits;
			firstbyte = firstbit / 8;
			shift = longbits - firstbit % 8 - n_bits;
		/*
		 * Copy from the BDS into a long, starting with the first
		 * byte with bits of interest to us.  Remember that the
		 * data in the BDS start at byte 11.
		 */
			memcpy (&bits, bds + firstbyte + 11, sizeof (long));
		/*
		 * Shift the bits of interest to the bottom of our long and
		 * mask them.
		 */
			bits >>= shift;
			bits &= mask;
		/*
		 * Now we just scale and throw it into the grid
		 */
			*gp++ = (ref + bits * bscale) * dscale;
		}
	}

	free (bds);
}




static void
grb_FixWind (u, v, nx, ny, index_module, ll_module)
float	*u, *v;
int	nx, ny;
void	(*index_module)(), (*ll_module)();
/*
 * Given the (nx x ny) grids with projection-relative u_wind and v_wind, and 
 * modules for converting indices <-> lat/lon, derive the "true" u_wind
 * and v_wind values (i.e., the eastward and northward components of the wind).
 */
{
	int	i, j;
	float	lat, lon, newi, newj, latang, lonang, u_true, v_true;

	for (j = 0; j < ny; j++)
	{
		for (i = 0; i < nx; i++)
		{
		/*
		 * Approximate the local angle of a line of latitude (w.r.t.
		 * a horizontal line on the map projection) at this point.
		 * Start with the lat/lon at the current grid point, then
		 * find the new grid indices if we take a small step east.
		 * The angle approximation is then just arctan (dj / di).
		 */
			(*ll_module)((double) i, (double) j, &lat, &lon);
			(*index_module)((double)(lat), (double)(lon + 0.05),
					&newi, &newj);

			latang = atan2 (newj - j, newi - i);
		/*
		 * Calculate a similar angle for a line of longitude.
		 */
			(*index_module)((double)(lat + 0.05), (double)(lon),
					&newi, &newj);
			lonang = atan2 (newj - j, newi - i);
		/*
		 * Now convert the projection-relative u and v winds into
		 * eastward and northward wind components.
		 */
			u_true = *u * cos (latang) + *v * sin (latang);
			v_true = *u * cos (lonang) + *v * sin (lonang);

			*u++ = u_true;
			*v++ = v_true;
		}
	}
}




static void
grb_ResetWind ()
/*
 * Lose any previous wind information, since we may be grabbing data from
 * a different file now.
 */
{
	int	i;

	for (i = 0; i < WindsCount; i++)
	{
		free (U_data[i]);
		free (V_data[i]);
		U_gridnum[i] = V_gridnum[i] = -1;
	}

	WindsCount = 0;
}




static void
grb_UnpackWind (tag, fid, undx, vndx, grid, nx, ny, index_module, ll_module)
GFTag	*tag;
FieldId	fid;
int	undx, vndx;
float	*grid;
int	nx, ny;
void	(*index_module)(), (*ll_module)();
/*
 * Get the u- and v-wind data from the undx'th and vndx'th grids in our
 * data file and return the "true" u- or v-wind grid in grid, which is
 * of size (nx x ny).  When possible, we return data that we've already
 * transmogrified.
 */
{
	int	i;
	FieldId	u_id = F_Lookup ("u_wind"), v_id = F_Lookup ("v_wind");
	float	*ugrid, *vgrid;
/*
 * If this is a different tag than the one we have info for (if any), 
 * then wipe out any previous winds stuff
 */
	if (tag->gt_tagid != WindsTagID)
		grb_ResetWind ();

	WindsTagID = tag->gt_tagid;
/*
 * Easy if we already have the data
 */
	for (i = 0; i < WindsCount; i++)
	{
		if (fid == u_id && U_gridnum[i] == undx)
		{
			memcpy (grid, U_data[i], nx * ny * sizeof (float));
			return;
		}
		else if (fid == v_id && V_gridnum[i] == vndx)
		{
			memcpy (grid, V_data[i], nx * ny * sizeof (float));
			return;
		}
	}
/*
 * We don't have the data already, so set up to get it
 */
	ugrid = U_data[WindsCount] = 
		(float *) malloc (nx * ny * sizeof (float));
	vgrid = V_data[WindsCount] = 
		(float *) malloc (nx * ny * sizeof (float));

	U_gridnum[WindsCount] = undx;
	V_gridnum[WindsCount] = vndx;

	WindsCount++;
/*
 * Extract the grids of projection-relative u and v wind
 */
	grb_UnpackBDS (tag, undx, ugrid, nx, ny);
	grb_UnpackBDS (tag, vndx, vgrid, nx, ny);
/*
 * Convert to true u and v wind
 */
	grb_FixWind (ugrid, vgrid, nx, ny, index_module, ll_module);
/*
 * Finally, copy the appropriate data into the caller's grid
 */
	if (fid == u_id)
		memcpy (grid, ugrid, nx * ny * sizeof (float));
	else
		memcpy (grid, vgrid, nx * ny * sizeof (float));
}

	

		
/*
 * Test program
 */
# ifdef notdef
main ()
{
	int	i;
	char	time[30];
	GFTag	*tag;
	GRIBdesc	*gd;

	msg_connect (NULL, "GRIBtest");
	tag = grb_Open ("/dt/burghart/grib/eta/eta.grib");

	for (i = 0; i < tag->gt_ngrids; i++)
		printf ("%d: %d %d \n", i, tag->gt_grib[i].gd_pds->field_id,
			tag->gt_grib[i].gd_time.zt_Sec);
}
# endif
