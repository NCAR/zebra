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

# include <copyright.h>
# include <defs.h>
# include <message.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"

MAKE_RCSID ("$Id: DFA_GRIB.c,v 3.10 1994-06-10 21:50:29 burghart Exp $")

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
	int		gt_sfc_only;	/* Surface grids only?		*/
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

 *	gg_slatang,
 *	gg_slonang:	(snx x sny) floating point arrays of the local angles
 *			for lines of constant lat. and lon. through each source
 *			grid point
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
 *	gg_dsi, gg_dsj:		(dnx x dny) floating point arrays of equivalent
 *				*source* array indices for destination grid
 *				points
 */
typedef struct s_GRB_TypeInfo
{
	int	gg_type;
	int	gg_snx, gg_sny;
	float	*gg_slatang, *gg_slonang;
	void	(*gg_ndx_module)();
	void	(*gg_ll_module)();
	int	gg_dnx, gg_dny;
	float	gg_dlat, gg_dlon;
	float	gg_dlatstep, gg_dlonstep;
	float	*gg_dsi, *gg_dsj;
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
				   int, int, FieldId, int, float *));
static FieldId	grb_Field FP ((GFpds *, ScaleInfo *));
static int	grb_Offset FP ((GFpds *));
static bool	grb_UsableLevel FP ((GFpds *, int));
static float	grb_ZLevel FP ((GFpds *, AltUnitType *));
static void	grb_105Index FP ((double, double, float *, float *));
static void	grb_105LatLon FP ((double, double, float *, float *));
static void	grb_36Index FP ((double, double, float *, float *));
static void	grb_36LatLon FP ((double, double, float *, float *));
static void	grb_27Index FP ((double, double, float *, float *));
static void	grb_27LatLon FP ((double, double, float *, float *));
static void	grb_UnpackBDS FP ((GFTag *, int, float *, int, int));
static void	grb_ResetWind FP ((void));
static void	grb_UnpackWind FP ((GFTag *, int, FieldId, int, int, float *, 
				    GRB_TypeInfo *));
static void	grb_DCFinishDefs FP ((DataChunk *, GRB_TypeInfo *, int));
static void	grb_InitGInfo FP ((GRB_TypeInfo	*));
static GRB_TypeInfo	*grb_GridTypeInfo FP ((GFpds *));

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
	/* Geopotential height (m) */
	{ 7, "gpalt", 1.0, 0.0 },
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
	/* pressure vertical velocity (Pa/s) */
	{ 39, "pres_w", 1.0, 0.0 },
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
	/* Total precipitation (kg/m**2)	*/
	{ 61, "precip", 1.0, 0.0 },
	/* Convective precipitation (kg/m**2)	*/
	{ 63, "conv_precip", 1.0, 0.0 },
};

int GRB_FList_len = sizeof (GRB_FList) / sizeof (struct s_GRB_FList);
	


/*
 * Info for GRIB grid types we know how to unpack.
 */
GRB_TypeInfo GRB_Types[] =
{
	/*
	 * 27: 4225-point (65x65) N. Hemisphere polar stereographic grid
	 * oriented 80W; pole at (32,32) [C type indexing].  381.0 km
	 * spacing at 60N.
	 */
	{ 27, 65, 65, NULL, NULL, grb_27Index, grb_27LatLon, 19, 11, 
		  20.0, -130.0, 4.0, 4.0, NULL, NULL },
	/*
	 * 36: 1558-point (41x38) N. Hemisphere polar stereographic grid
	 * oriented 105W; pole at (18,41) [C type indexing].  The TDL grid
	 * (N. America) is used to archive LFM and NGM data.  190.5 km
	 * spacing at 60N.
	 */
	{ 36, 41, 38, NULL, NULL, grb_36Index, grb_36LatLon, 36, 21, 
		  20.0, -130.0, 2.0, 2.0, NULL, NULL },
	/*
	 * 105: 6889-point (83x83) N. Hemisphere polar stereographic grid
	 * oriented 105W; pole at (39.5,87.5) [C type indexing].  (U.S. area
	 * subset of NGM Super C grid, used by ETA model).  90.75464 km
	 * spacing at 60N.
	 */
	{ 105, 83, 83, NULL, NULL, grb_105Index, grb_105LatLon, 71, 41, 
		  20.0, -130.0, 1.0, 1.0, NULL, NULL },
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
int	WindsFOffset = -1;
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
/*
 * We can do this relatively quickly, assuming that all the grids in the file 
 * have the same time, or *SLOWLY* if we give up that assumption.
 */
# ifdef GRIB_SLOWSCAN

	GFTag	*tag;
/*
 * Get the file tag.
 */
	if (! (tag = grb_Open (file)))
		return (FALSE);
/*
 * Make the (not guaranteed but somewhat reasonable) assumption that the
 * data were written in chronological order.
 */
	*begin = *end = tag->gt_grib[0].gd_time;
	*nsample = 1;

	for (g = 1; g < tag->gt_ngrids; g++)
	{
		if (TC_Less (*end, tag->gt_grib[g].gd_time))
		{
			*nsample++;
			*end = tag->gt_grib[g].gd_time;
		}
	}
/*
 * Get rid of the tag and return
 */
	close (tag->gt_fd);
	grb_DestroyTag (tag);

	return (TRUE);
	
# else /* ! GRIB_SLOWSCAN */

/*
 * Fast QueryTime, assuming there's only one time in the file.
 */
	int	fd;
	char	is[8];
	GFpds	pds;
/*
 * Try to open the file
 */
	if ((fd = open (file, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "grb_QueryTime: Error %d opening '%s'", 
			  errno, file);
		return (FALSE);
	}
/*
 * Get the first Indicator Section (8 bytes) and the first Product Definition
 * Section.
 */
	if ((read (fd, is, 8) != 8) || 
	    (read (fd, &pds, sizeof (GFpds)) != sizeof (GFpds)))
	{
		msg_ELog (EF_PROBLEM, 
			  "grb_QueryTime: Can't get first grid time from '%s'",
			  file);
		return (FALSE);
	}
/*
 * Extract the time from the PDS
 */
	TC_ZtAssemble (begin, pds.year, pds.month, pds.day, pds.hour, 
		       pds.minute, 0, 0);
	*end = *begin;
	*nsample = 1;

	close (fd);
	return (TRUE);

# endif /* GRIB_SLOWSCAN */
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

	tag->gt_sfc_only = FALSE;

	*rtag = (char *) tag;
	return (TRUE);
}




int
grb_SfcOpenFile (fname, dp, write, rtag)
char	*fname;
DataFile	*dp;
bool	write;
char	**rtag;
/*
 * DFA routine to open a file (for access to surface data only) and return 
 * a tag.
 */
{
	GFTag	*tag;

	if (! (tag = grb_Open (fname)))
		return (FALSE);

	tag->gt_sfc_only = TRUE;

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
	int	f;
	GFTag	*tag;
	DataChunk	*dc;
	FieldId	dims[3], lat_id, lon_id, alt_id;
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

	for (f = 0; f < nfield; f++)
		dc_NSDefineVariable (dc, fields[f], 3, dims, FALSE);
/*
 * Set the data chunk to allow field redefinition, since we'll be putting
 * in the real dimension sizes later.
 */
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
	int	offset, nalts, test, gtype, i;
	float  	badval, ztarget, *lats, *lons, alts[MAXLEVELS];
	bool	onelevel;
	SValue	v;
	GFTag	*tag;
	GFpds	*pds;
	FieldId	fids[5], lat_id, lon_id, alt_id, checkfld;
	ZebTime	stime;
	GRB_TypeInfo	*grbinfo;
	AltUnitType	altunits;
/*
 * Open this file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Field IDs for our dimension variables
 */
	lon_id = F_Lookup ("lon");
	lat_id = F_Lookup ("lat");
	alt_id = F_Lookup ("alt");
/*
 * Get the field list
 */
	nfield = dc_NSGetAllVariables (dc, fids, NULL);
/*
 * Find the first non-dimension field
 */
	for (i = 0; i < nfield; i++)
	{
		checkfld = fids[i];
		if (checkfld != lon_id && checkfld != lat_id && 
		    checkfld != alt_id)
			break;
	}
/*
 * Get the requested forecast offset, in seconds.
 */
	offset = ds_GetDetail (DD_FORECAST_OFFSET, details, ndetail, &v) ?
		v.us_v_int : 0;
/*
 * Get the grid type and get the entry from our types table.  We look for the
 * first grid in the file that is of a type we can unpack, matches our first
 * (non-dimension) field, is a usable vertical level, and has the right
 * forecast offset.  Once we find such a grid, we only accept grids of the
 * same GRIB grid type thereafter.
 */
	for (i = 0; i < tag->gt_ngrids; i++)
	{
		pds = tag->gt_grib[i].gd_pds;

		if ((grbinfo = grb_GridTypeInfo (pds)) != NULL &&
		    grb_UsableLevel (pds, tag->gt_sfc_only) && 
		    grb_Field (pds, NULL) == checkfld &&
		    grb_Offset (pds) == offset)
			break;
	}
/*
 * Bail out if no grids met our criteria above
 */
	if (i == tag->gt_ngrids)
	{
		msg_ELog (EF_INFO, 
			  "GRIB: No unpackable %d hr forecast for %s/%s", 
			  offset / 3600, ds_PlatformName(dc->dc_Platform), 
			  F_GetName (checkfld));

		if (! dc_NSDefineIsComplete (dc))
		{
		/*
		 * If dimension definition hasn't been closed off yet,
		 * set them all to zero and close it now.
		 */
			dc_NSDefineDimension (dc, F_Lookup ("lat"), 0);
			dc_NSDefineDimension (dc, F_Lookup ("lon"), 0);
			dc_NSDefineDimension (dc, F_Lookup ("alt"), 0);
			dc_NSDefineComplete (dc);
		}
		return (TRUE);
	}
/*
 * Make sure the arrays for unpacking this grid type have been built
 */
	grb_InitGInfo (grbinfo);
/*
 * Get the list of available alts and set the altitude units in our
 * data chunk.  We test to make sure we have the same number of alts for all
 * fields (although we don't verify that the actual alts are the same...).
 */
	test = 0;

	for (i = 0; i < nfield; i++)
	{
	/*
	 * Skip the dimension variables "lat", "lon", and "alt"
	 */
		if (fids[i]==lon_id || fids[i]==lat_id || fids[i]==alt_id)
			continue;
	/*
	 * Get the alts for this field, and make sure the count matches
	 * with the other fields we've checked so far
	 */
		grb_GetAlts (gp->gl_dfindex, fids[i], offset, alts, &nalts, 
			     &altunits);
		if (test && nalts != test)
		{
			msg_ELog (EF_PROBLEM, 
				"grb_GetData: Flds have different # of alts!");
			return (FALSE);
		}

		test = nalts;
	}
	
	dc_SetLocAltUnits (dc, altunits);
/*
 * If they just want one level, find the closest one.  (Ignore the "altitude"
 * detail if the file is open for access to surface data only).
 */
	onelevel = (!tag->gt_sfc_only &&
		    ds_GetDetail ("altitude", details, ndetail, &v));
	if (onelevel)
	{
		float	diff, bestdiff = 99e99;
		int	best;

		ztarget = v.us_v_float;

		for (i = 0; i < nalts; i++)
		{
			diff = fabs (ztarget - alts[i]);
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

	dc_NSAddStatic (dc, lon_id, (void *) lons);

	free (lons);
	
	lats = (float *) malloc (grbinfo->gg_dny * sizeof (float));
	for (i = 0; i < grbinfo->gg_dny; i++)
		lats[i] = grbinfo->gg_dlat + i * grbinfo->gg_dlatstep;

	dc_NSAddStatic (dc, lat_id, (void *) lats);

	free (lats);
/*
 * Same for altitude.
 */
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
				       fids[f], offset, 
				       onelevel ? &ztarget : NULL);
		}
		

		samp++;
	}
	return (TRUE);
}




static void
grb_InitGInfo (ginfo)
GRB_TypeInfo	*ginfo;
/*
 * Build the various arrays to assist in unpacking the given grid type
 */
{
	int	dnx, dny, snx, sny, i, j;
	float	lat, lon, latstep, lonstep, newi, newj;
	float	*si, *sj, *latang, *lonang;
/*
 * Return if the arrays have already been built
 */
	if (ginfo->gg_dsi)
		return;
/*
 * Build arrays mapping destination grid indices into equivalent floating 
 * point source grid indices.
 */
	dnx = ginfo->gg_dnx;
	dny = ginfo->gg_dny;

	si = ginfo->gg_dsi = (float *) malloc (dnx * dny * sizeof (float));
	sj = ginfo->gg_dsj = (float *) malloc (dnx * dny * sizeof (float));

	for (j = 0; j < dny; j++)
	{
		lat = ginfo->gg_dlat + j * ginfo->gg_dlatstep;
		for (i = 0; i < dnx; i++)
		{
			lon = ginfo->gg_dlon + i * ginfo->gg_dlonstep;
			(*ginfo->gg_ndx_module)(lat, lon, si++, sj++);
		}
	}
/*
 * Build arrays of local angles for lines of constant lat and lon through
 * source grid points.  These are used when converting projection-relative u 
 * and v winds to true east-west and north-south u and v.
 */
	snx = ginfo->gg_snx;
	sny = ginfo->gg_sny;

	latang = ginfo->gg_slatang = 
		(float *) malloc (snx * sny * sizeof (float));
	lonang = ginfo->gg_slonang = 
		(float *) malloc (snx * sny * sizeof (float));

	for (j = 0; j < sny; j++)
	{
		for (i = 0; i < snx; i++)
		{
		/*
		 * Approximate the local angle of a line of latitude (w.r.t.
		 * a horizontal line on the map projection) at this point.
		 * Start with the lat/lon at the current grid point, then
		 * find the new grid indices if we take a small step east.
		 * The angle approximation is then just arctan (dj / di).
		 */
			(*ginfo->gg_ll_module)((double) i, (double) j, 
					       &lat, &lon);
			(*ginfo->gg_ndx_module)(lat, lon + 0.05, &newi, &newj);

			*latang++ = atan2 (newj - j, newi - i);
		/*
		 * Calculate a similar angle for a line of longitude.
		 */
			(*ginfo->gg_ndx_module)(lat + 0.05, lon, &newi, &newj);
			*lonang++ = atan2 (newj - j, newi - i);
		}
	}
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
grb_GetAlts (dfindex, fid, offset, alts, nalts, altunits)
int	dfindex;
FieldId	fid;
int	offset;
float	*alts;
int	*nalts;
AltUnitType	*altunits;
/*
 * Return an array of altitudes available in this file for the given field
 * and forecast offset time, along with the altitude units.  The array is
 * sorted in increasing order.
 */
{
	int	i, count;
	float	temp;
	GFpds	*pds;
	GFTag	*tag;
	GRB_TypeInfo	*grbinfo;
	ZebTime	t;
/*
 * Make sure that the file is open.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Find the first usable grid for the chosen field and forecast offset.
 */
	for (i = 0; i < tag->gt_ngrids; i++)
	{
		pds = tag->gt_grib[i].gd_pds;

		if ((grbinfo = grb_GridTypeInfo (pds)) != NULL &&
		    grb_UsableLevel (pds, tag->gt_sfc_only) && 
		    grb_Field (pds, NULL) == fid &&
		    grb_Offset (pds) == offset)
		{
			t = tag->gt_grib[i].gd_time;
			break;
		}
	}
/*
 * Simple if the file is open for surface data only
 */
	if (tag->gt_sfc_only)
	{
		alts[0] = grb_ZLevel (pds, altunits);
		*nalts = 1;
		return (TRUE);
	}
/*
 * Count the grids matching the field, offset, and time, building the
 * altitude array as we go.
 */
	count = 0;
	for (; i < tag->gt_ngrids; i++)
	{
		if (! TC_Eq (tag->gt_grib[i].gd_time, t))
			break;
	/*
	 * Grab the altitude from this grid if it has the same type, field, 
	 * and time offset as the first grid, and is a usable grid.
	 */
		pds = tag->gt_grib[i].gd_pds;

		if (pds->grid_id == grbinfo->gg_type  &&
		    grb_UsableLevel (pds, tag->gt_sfc_only) && 
		    grb_Field (pds, NULL) == fid &&
		    grb_Offset (pds) == offset)
		{
			if (alts)
				alts[count++] = grb_ZLevel (pds, altunits);
			else
				count++;
		}
	}

	if (! count)
		return (FALSE);

	if (nalts)
		*nalts = count;
/*
 * Sort into increasing order (internal calls to grb_GetAlts() require this)
 */
	if (alts)
	{
		for (i = 1; i < count; i++)
		{
			int	si = i;

			while (si && alts[si] < alts[si-1])
			{
				temp = alts[si-1];
				alts[si-1] = alts[si];
				alts[si] = temp;

				si--;
			}
		}
	}
/*
 * Done
 */	
	return (TRUE);
}




int
grb_GetForecastTimes (dfindex, times, ntimes)
int	dfindex;
int	*times;
int	*ntimes;
/*
 * Return an array of available forecast offset times (in seconds) for 
 * this file.
 */
{
	int	count, i, offset, t;
	GFpds	*pds;
	GFTag	*tag;
/*
 * Make sure that the file is open.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Ugly linear thing.  Just loop through the grids and insert their offsets
 * into the array if they aren't there already.
 */
	count = 0;
	for (i = 0; i < tag->gt_ngrids; i++)
	{
		pds = tag->gt_grib[i].gd_pds;

		if (! grb_UsableLevel (pds, tag->gt_sfc_only))
			continue;

		offset = grb_Offset (pds);

		for (t = 0; t < count && times[t] != offset; t++)
			/* nothing */;

		if (t == count)
			times[count++] = offset;
	}

	if (ntimes)
		*ntimes = count;

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
	/*
	 * We don't deal with the weird level types
	 */
		if (! grb_UsableLevel (pds, tag->gt_sfc_only))
			continue;
	/*
	 * Add this field to the list if it isn't there already
	 */
		fid = grb_Field (pds, NULL);
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
	int	i, ntimes, ngrids = tag->gt_ngrids;
	GRB_TypeInfo	*grbinfo;
	Location	gloc;
/*
 * Get the file open.
 */
	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
/*
 * Find the first grid we know how to unpack and use its type to determine 
 * location
 */
	for (i = 0; i < ngrids; i++)
	{
		
		grbinfo = grb_GridTypeInfo (tag->gt_grib[i].gd_pds);
		if (grbinfo)
		{
			gloc.l_lat = grbinfo->gg_dlat;
			gloc.l_lon = grbinfo->gg_dlon;
			gloc.l_alt = 1000.0;	/* BOGUS! */
			break;
		}
	}
	

	if (i == ngrids)
	{
		msg_ELog (EF_EMERGENCY, "No unpackable GRIB grid types!");
		return (0);
	}
/*
 * Shortcut:  See if the first and last grid times in the file are the same.
 * If so, we assume all the ones between are the same, too.
 */
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
	int	fd = tag->gt_fd;
	int	is_len, grib_len, pds_len, gds_len, bms_len, bds_len;
	int	status, ng, ncopy, grib_start, buflen = 0;
	char	is[8], *trailer, *buf = 0;
	GFpds	*pds;
/*
 * Rewind the file first
 */
	lseek (fd, 0, SEEK_SET);
/*
 * File offset to the start of the current GRIB record.
 */
	grib_start = tell (fd);
/*
 * Each grid starts with an Indicator Section.  Loop through grids
 * until we fail to get one of these.
 */
	is_len = 8;	/* Fixed length of the Indicator Section */

	while ((status = read (fd, is, is_len)) == is_len)
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
	 * Get the length of this GRIB 'message' (one grid), make sure we
	 * have space for it.
	 */
		grib_len = grb_ThreeByteInt (is + 4);
		if (grib_len > buflen)
		{
			buflen = grib_len;

			if (buf)
				buf = realloc (buf, buflen);
			else
				buf = malloc (buflen);
		}
	/*
	 * Copy in the eight bytes we have, and read the rest
	 */
		memcpy (buf, is, is_len);
		status = read (fd, buf + is_len, grib_len - is_len);
		if (status < (grib_len - is_len))
		{
			msg_ELog (EF_INFO, 
				  "GRIB file ends with incomplete record");
			status = 0;	/* Treat it like an EOF */
			break;
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
	 * get the PDS length, then make a copy of the PDS and stash it
	 * in the tag.  Our structure only holds the (required) first 28 bytes
	 * of the PDS, so we don't copy any more than that.  We accomodate
	 * illegal smaller ones though, by padding with zeros.
	 */
		pds_len = grb_ThreeByteInt (buf + is_len);
		pds = (GFpds *) calloc (1, sizeof (GFpds));

		ncopy = (pds_len < sizeof (GFpds)) ? pds_len : sizeof (GFpds);
		memcpy (pds, buf + is_len, ncopy);
		tag->gt_grib[ng-1].gd_pds = pds;
	/*
	 * Extract the time from the PDS
	 */
		TC_ZtAssemble (&(tag->gt_grib[ng-1].gd_time), pds->year, 
			       pds->month, pds->day, pds->hour, pds->minute,
			       0, 0);
	/*
	 * If we have a Grid Description Section, read its length from the
	 * first three bytes of the GDS.
	 */
		if (pds->section_flags && GDS_FLAG)
			gds_len = grb_ThreeByteInt (buf + is_len + pds_len);
		else
			gds_len = 0;
	/*
	 * Same for the Bit Map Section
	 */
		if (pds->section_flags && BMS_FLAG)
			bms_len = grb_ThreeByteInt (buf + is_len + 
						    pds_len + gds_len);
		else
			bms_len = 0;
	/*
	 * Save the position of the Binary Data Section relative to the start
	 * of the file, and get its length.
	 */
		tag->gt_grib[ng-1].gd_doffset = grib_start + is_len + pds_len +
			gds_len + bms_len;

		bds_len = tag->gt_grib[ng-1].gd_bds_len = 
			grb_ThreeByteInt (buf + is_len + pds_len + gds_len +
					  bms_len);
	/*
	 * Sanity check.  Make sure the last 4 bytes of the GRIB record are 
	 * the GRIB trailer "7777"
	 */
		trailer = buf + grib_len - 4;
		if (strncmp (trailer, "7777", 4))
		{
			msg_ELog (EF_EMERGENCY, 
				  "Bad GRIB trailer '%4s' at grid %d",
				  trailer, ng);
			return (FALSE);
		}
	/*
	 * Keep the position of the start of the next GRIB record
	 */
		grib_start = tell (fd);
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
grb_ReadRGrid (dc, tag, ginfo, samp, sbegin, send, fid, offset, ztarget)
DataChunk	*dc;
GFTag		*tag;
GRB_TypeInfo	*ginfo;
int		samp, sbegin, send;
FieldId		fid;
int		offset;
float		*ztarget;
/*
 * Build a grid of the chosen field, using the GRIB grids between indices
 * sbegin and send inclusive, and stuff it into the data chunk.  The
 * desired forecast offset time in seconds is passed in 'offset'.  If
 * 'ztarget' is non-NULL, then only one horizontal plane is desired, at the
 * given z level.  Otherwise, return all planes.
 */
{
	int	nsx, nsy, si, sj, i, j, itemp;
	int	indices[MAXLEVELS], u_indices[MAXLEVELS], v_indices[MAXLEVELS];
	float	zvals[MAXLEVELS], badval = dc_GetBadval (dc), ftemp;
	int	level, nlevels, ulevels, vlevels, ndx;
	float	*sgrid, *sp, *dgrid, *dp, *lats, *lons;
	float	x, y, z, di, dj, val0, val1, val2, val3, *fsi, *fsj;
	GFpds	*pds;
	ZebTime	time;
	bool	onelevel, u_or_v;
	FieldId	grid_fid, u_fid, v_fid;
	SValue	v;
	ScaleInfo	sc;
	unsigned long	dc_nlevels, nlat, nlon;
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
	 * Bag this grid now if the type is wrong, the forecast time is wrong,
	 * or it's a not a usable level
	 */
		if (pds->grid_id != ginfo->gg_type ||
		    ! grb_UsableLevel (pds, tag->gt_sfc_only) || 
		    grb_Offset (pds) != offset)
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
	/*
	 * Semi-kluge: For surface-only access, just take the first "surface"
	 * grid we get.  Some files have both 0m MSL (level type 102) and 
	 * 0m AGL (level type 1) grids for a given field.  We count either one
	 * as a "surface" grid, so we'll just give them whichever one we see
	 * first.
	 */
		if (tag->gt_sfc_only && nlevels == 1)
			break;
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
 * data chunk.
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

		msg_ELog (EF_INFO, "GRIB: No %d hr forecast for %s/%s", 
			  offset / 3600, ds_PlatformName(dc->dc_Platform), 
			  F_GetName (fid));
		
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
 * Sort the z levels increasing so that we return them in some reasonable order
 */
	for (level = 1; level < nlevels; level++)
	{
		int	sl = level;

		while (sl && zvals[sl] < zvals[sl-1])
		{
			ftemp = zvals[sl-1];
			zvals[sl-1] = zvals[sl];
			zvals[sl] = ftemp;

			itemp = indices[sl-1];
			indices[sl-1] = indices[sl];
			indices[sl] = itemp;

			itemp = u_indices[sl-1];
			u_indices[sl-1] = u_indices[sl];
			u_indices[sl] = itemp;

			itemp = v_indices[sl-1];
			v_indices[sl-1] = v_indices[sl];
			v_indices[sl] = itemp;

			sl--;
		}
	}
/*
 * Grab some info on the GRIB grid type we're unpacking
 */
	nsx = ginfo->gg_snx;	/* source grid width	*/
	nsy = ginfo->gg_sny;	/* source grid height	*/
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
	
	for (level = 0; level < nlevels; level++)
	{
	/*
	 * Get the scaling information for our field and unpack the GRIB
	 * Binary Data Section into sgrid.
	 */
		grb_Field (tag->gt_grib[indices[level]].gd_pds, &sc);

		if (u_or_v)
			grb_UnpackWind (tag, offset, fid, u_indices[level], 
					v_indices[level], sgrid, ginfo);
		else
			grb_UnpackBDS (tag, indices[level], sgrid, nsx, nsy);
	/*
	 * Get the arrays mapping the destination grid indices into (floating
	 * point) source grid indices.
	 */
		fsi = ginfo->gg_dsi;
		fsj = ginfo->gg_dsj;
	/*
	 * Now fill in our destination grid, using a bilinear interpolation
	 * of data from the source grid
	 */
		for (j = 0; j < nlat; j++)
		{
			for (i = 0; i < nlon; i++)
			{
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
				si = (int)(*fsi);
				sj = (int)(*fsj);
				
				if (si >= 0 && si < (nsx - 1) &&
				    sj >= 0 && sj < (nsy - 1))
				{
					di = *fsi - si;
					dj = *fsj - sj;

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
				else
					*dp++ = badval;
			/*
			 * Increment the pointers to the source grid index
			 * arrays.
			 */
				fsi++;
				fsj++;
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
 * Figure out the valid time based on the time range indicator
 */
	switch (pds->range_id)
	{
	    case 0:
	    case 2:
	    case 3:
		return (multiplier * pds->p1);
	    case 1:
		return (0);
	/*
	 * The accumulation (4) and difference (5) types are considered valid
	 * at the reference time + p2
	 */
	    case 4:
	    case 5:
		return (multiplier * pds->p2);
	    default:
		msg_ELog (EF_PROBLEM, 
			  "grb_Offset: Can't handle time range ID %d!",
			  pds->range_id);
		return (0);
	}
}




static bool
grb_UsableLevel (pds, sfc_only)
GFpds	*pds;
bool	sfc_only;
/*
 * Return whether this GRIB grid is "usable".  If sfc_only is true, only
 * surface data are usable.  Otherwise, the data must have an associated single
 * vertical level (i.e., a specific isobaric level or height, as opposed to
 * earth surface, cloud base, tropopause, etc.) to be considered usable.
*/
{
	int	l_id = pds->level_id;

	if (sfc_only)
		return (l_id == 1 || l_id == 102);
	else
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
	    case 1:	/* Surface data (0 meters AGL) */
		if (units)
			*units = AU_mAGL;
		return (0.0);
	    case 100:	/* isobaric level (millibars) */
		if (units)
			*units = AU_mb;
		break;
	    case 102:	/* 0 meters MSL */
		if (units)
			*units = AU_mMSL;
		return (0.0);
	    case 103:	/* fixed height (meters MSL) */
		if (units)
			*units = AU_mMSL;
		break;
	    default:
		msg_ELog (EF_PROBLEM, "Can't deal with GRIB level type %d!",
			  l_id);
		return (-1.0);
	}

	return ((float) grb_TwoByteInt (&(pds->level_val)));
}




static void
grb_27Index (lat, lon, ifloat, jfloat)
double	lat, lon;
float	*ifloat, *jfloat;
/*
 * Return the (floating point) array indices for a GRIB 27 type grid, given a
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
 * GRIB grid type 27 [4225-point (65x65) N. Hemisphere polar stereographic 
 * grid oriented 80W].  For type 27, the pole is at grid location (33,33), 
 * in Fortran terms, or (32,32) here in the C world.
 */
	const float	psi1 = 1.047197551;	/* 60.0 deg. north */
	const float	lambda0 = -1.396263402;	/* 80.0 deg. west */
	const float	scale = 381;
	const float	ipole = 32;
	const float	jpole = 32;
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
grb_27LatLon (idouble, jdouble, lat, lon)
double	idouble, jdouble;
float	*lat, *lon;
/*
 * Turn the (double precision) indices into a GRIB 27 type grid into
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
 * GRIB grid type 27 [4225-point (65x65) N. Hemisphere polar stereographic 
 * grid oriented 80W].  For type 27, the pole is at grid location (33,33), 
 * in Fortran terms, or (32,32) here in the C world.
 */
	const float	psi1 = 1.047197551;	/* 60.0 deg. north */
	const float	lambda0 = -1.396263402;	/* 80.0 deg. west */
	const float	scale = 381;
	const float	ipole = 32;
	const float	jpole = 32;
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
grb_36Index (lat, lon, ifloat, jfloat)
double	lat, lon;
float	*ifloat, *jfloat;
/*
 * Return the (floating point) array indices for a GRIB 36 type grid, given a
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
 * GRIB grid type 36 [1558-point (41x38) N. Hemisphere polar stereographic 
 * grid oriented 105W].  For type 36, the pole is at grid location (19,42), 
 * in Fortran terms, or (18,41) here in the C world.
 */
	const float	psi1 = 1.047197551;	/* 60.0 deg. north */
	const float	lambda0 = -1.832595715;	/* 105.0 deg. west */
	const float	scale = 190.5;
	const float	ipole = 18;
	const float	jpole = 41;
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
grb_36LatLon (idouble, jdouble, lat, lon)
double	idouble, jdouble;
float	*lat, *lon;
/*
 * Turn the (double precision) indices into a GRIB 36 type grid into
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
 * GRIB grid type 36 [1558-point (41x38) N. Hemisphere polar stereographic 
 * grid oriented 105W].  For type 36, the pole is at grid location (19,42), 
 * in Fortran terms, or (18,41) here in the C world.
 */
	const float	psi1 = 1.047197551;	/* 60.0 deg. north */
	const float	lambda0 = -1.832595715;	/* 105.0 deg. west */
	const float	scale = 190.5;
	const float	ipole = 18;
	const float	jpole = 41;
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
 * grid oriented 105W].  For type 105, the pole is at grid location 
 * (40.5, 88.5), in Fortran terms, or (39.5, 87.5) here in the C world.
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
 * grid oriented 105W].  For type 105, the pole is at grid location 
 * (40.5, 88.5), in Fortran terms, or (39.5, 87.5) here in the C world.
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
 * and read it.  We allocate some extra space at the end of the BDS, since 
 * during unpacking we potentially copy (but don't use) a few bytes from past 
 * the end of the data.
 */
	bds_len = tag->gt_grib[which].gd_bds_len;

	bds = (char *) malloc (bds_len + sizeof (bits));
	memset (bds + bds_len, 0, sizeof (bits));
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
 *
 * The topmost bit of ref_top is reserved for the sign.  If the bit is set,
 * the value is negative.
 */
	sign = (bds_hdr->ref_top & 0x80) ? -1 : 1;

	exponent = 4 * (bds_hdr->ref_top & 0x7F) - 280;
	mantissa = grb_ThreeByteInt (&(bds_hdr->ref_mant));

	ref = sign * mantissa * pow (2, (double) exponent);
/*
 * From GRIB documentation:
 *	               E       -D
 *	Y = (R + (X * 2 )) * 10
 *                                   -D                     E
 * We calculate the decimal scale (10  ) and binary scale (2 ) here.
 *
 * The topmost bit of both ds_factor and bs_factor is reserved for the sign.
 * If the bit is set, the value is negative.
 */
	sign = (pds->ds_factor & 0x8000) ? -1 : 1;
	dscale = pow (10, (double)(-sign * (pds->ds_factor & 0x7FFF)));

	sign = (bds_hdr->bs_factor & 0x8000) ? -1 : 1;
	bscale = pow (2, (double)(sign * (bds_hdr->bs_factor & 0x7FFF)));
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
grb_ResetWind ()
/*
 * Release any cached grids of precalculated u and v winds.
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
grb_UnpackWind (tag, foffset, fid, undx, vndx, grid, ginfo)
GFTag	*tag;
int	foffset;
FieldId	fid;
int	undx, vndx;
float	*grid;
GRB_TypeInfo	*ginfo;
/*
 * Get the u- and v-wind data from the undx'th and vndx'th grids in our
 * data file and return the "true" u- or v-wind grid in grid, which is
 * of size (nx x ny).  When possible, we return data that we've already
 * transmogrified.  Calculated winds are cached and only released if winds
 * from a different file or forecast offset are requested.
 */
{
	int	i, j, nx, ny;
	FieldId	u_id = F_Lookup ("u_wind"), v_id = F_Lookup ("v_wind");
	float	*ugrid, *vgrid, *u, *v, *latang, *lonang;
	float	u_true, v_true;
/*
 * If this is a different tag than the one we have info for (if any), 
 * then wipe out any previous winds stuff
 */
	if (tag->gt_tagid != WindsTagID || foffset != WindsFOffset)
		grb_ResetWind ();

	WindsTagID = tag->gt_tagid;
	WindsFOffset = foffset;
/*
 * Grid size info
 */
	nx = ginfo->gg_snx;
	ny = ginfo->gg_sny;
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
	ugrid = (float *) malloc (nx * ny * sizeof (float));
	vgrid = (float *) malloc (nx * ny * sizeof (float));
/*
 * Extract the grids of projection-relative u and v wind
 */
	grb_UnpackBDS (tag, undx, ugrid, nx, ny);
	grb_UnpackBDS (tag, vndx, vgrid, nx, ny);
/*
 * Grab the arrays of local angles for lines of constant lat and lon at
 * each of our grid points.
 */
	latang = ginfo->gg_slatang;
	lonang = ginfo->gg_slonang;
/*
 * Convert to true u and v wind
 */
	u = ugrid;
	v = vgrid;

	for (j = 0; j < ny; j++)
	{
		for (i = 0; i < nx; i++)
		{
		/*
		 * Now convert the projection-relative u and v winds into
		 * eastward and northward wind components.
		 */
			u_true = *u * cos (*latang) + *v * sin (*latang);
			v_true = *u * cos (*lonang) + *v * sin (*lonang);

			*u++ = u_true;
			*v++ = v_true;
		/*
		 * Step to the next grid position in the latang and lonang
		 * arrays.
		 */
			latang++;
			lonang++;
		}
	}
/*
 * Copy the appropriate data into the caller's grid
 */
	if (fid == u_id)
		memcpy (grid, ugrid, nx * ny * sizeof (float));
	else
		memcpy (grid, vgrid, nx * ny * sizeof (float));
/*
 * Cache these grids (if we have space)
 */
	if (WindsCount < MAXLEVELS)
	{
		U_data[WindsCount] = ugrid;
		U_gridnum[WindsCount] = undx;

		V_data[WindsCount] = vgrid;
		V_gridnum[WindsCount] = vndx;

		WindsCount++;
	}
	else
	{
		msg_ELog (EF_PROBLEM, 
			  "grb_UnpackWind: Can't cache more than %d grids!",
			  MAXLEVELS);
		free (ugrid);
		free (vgrid);
	}
}




GRB_TypeInfo *
grb_GridTypeInfo (pds)
GFpds	*pds;
/*
 * Return a pointer to a GRB_TypeInfo structure for the type of grid associated
 * with the given PDS.  If we don't understand this grid type, return NULL.
 */
{
	int	i;
	GRB_TypeInfo	*grbinfo = NULL;

	for (i = 0; i < GRB_NTypes; i++)
	{
		if (GRB_Types[i].gg_type == pds->grid_id)
		{
			grbinfo = GRB_Types + i;
			break;
		}
	}

	return (grbinfo);
}
