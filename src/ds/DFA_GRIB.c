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
# include <string.h>

# include <copyright.h>
# include <defs.h>
# include <message.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "DataFormat.h"
# include "GRIB.h"

RCSID ("$Id: DFA_GRIB.c,v 3.26 1997-02-03 22:16:50 granger Exp $")


/*
 * GRIB record descriptor
 */
typedef struct s_GRIBdesc
{
	GFpds	*gd_pds;	/* Product definition section	*/
	GFgds   *gd_gds;	/* Grid description section	*/
	long	gd_doffset;	/* offset to binary data section*/
	int	gd_bds_len;	/* binary data section length	*/
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
	ZebTime		*gt_times;	/* Times for each grid		*/
} GFTag;


typedef struct _GRIBOpenFile
{
	OpenFile 	open_file;
	GFTag		gf_tag;
}
GRIBOpenFile;

#define GFTAGP(ofp) (&((GRIBOpenFile *)(ofp))->gf_tag)

/*
 * The class/organization compatibility table.  If the desired class
 * and the given file organization appear together here, we can do it.
 */
static CO_Compat COCTable [] =
{
	{ OrgNSpace,		DCC_NSpace	},
	{ Org2dGrid,		DCC_NSpace	},
	{ Org3dGrid,		DCC_NSpace	},
};

/*
 * GRIB format methods (both normal and surface only)
 */
P_QueryTime (grb_QueryTime);
P_GetObsSamples (grb_GetObsSamples);
P_SyncFile (grb_SyncFile);
P_CloseFile (grb_CloseFile);
P_OpenFile (grb_OpenFile);
P_GetData (grb_GetData);
P_GetFields (grb_GetFields);
P_GetAlts (grb_GetAlts);
P_GetForecastTimes (grb_GetForecastTimes);
P_Setup (grb_Setup);
P_GetTimes (grb_GetTimes);

P_OpenFile (grb_SfcOpenFile);

/*
 * GRIB
 */
static DataFormat gribFormatRec =
{
	"GRIB",
	FTGRIB,
	".grib|.grb",
	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof(GRIBOpenFile),
	TRUE,				/* read-only flag		*/

	FORMAT_INIT,

	grb_QueryTime,			/* Query times			*/
	___,				/* Make file name		*/

	grb_Setup,			/* setup			*/
	grb_OpenFile,			/* Open				*/
	grb_CloseFile,			/* Close			*/
	grb_SyncFile,			/* Synchronize			*/
	grb_GetData,			/* Get the data			*/
	grb_GetAlts,			/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	grb_GetForecastTimes,		/* Get forecast times		*/
	___,				/* Create a new file		*/
	___,				/* Write to file		*/
	___,				/* Write block to a file	*/
	grb_GetObsSamples,		/* Get observation samples	*/
	grb_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
	grb_GetTimes			/* Return array of times	*/
};

DataFormat *gribFormat = (DataFormat *) &gribFormatRec;


static DataFormat gribSfcFormatRec =
{
	"GRIB_sfc",
	FTGRIBSfc,
	".grib|.grb",
	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof(GRIBOpenFile),
	TRUE,				/* read-only flag		*/

	FORMAT_INIT,

	grb_QueryTime,			/* Query times			*/
	___,				/* Make file name		*/

	grb_Setup,			/* setup			*/
	grb_SfcOpenFile,		/* Open				*/
	grb_CloseFile,			/* Close			*/
	grb_SyncFile,			/* Synchronize			*/
	grb_GetData,			/* Get the data			*/
	grb_GetAlts,			/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	grb_GetForecastTimes,		/* Get forecast times		*/
	___,				/* Create a new file		*/
	___,				/* Write to file		*/
	___,				/* Write block to a file	*/
	grb_GetObsSamples,		/* Get observation samples	*/
	grb_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
	grb_GetTimes			/* Return array of times	*/
};

DataFormat *gribSfcFormat = (DataFormat *) &gribSfcFormatRec;


/*
 * Model ID's
 */
#define ANY -1
#define MM5 95


/*
 * Field list.  We only include here the fields for which we have
 * established names.  Other fields just become "gribX" where X is 
 * the GRIB field number.  Parameter id's are actually specific to
 * the model, so some fields include a specific model id.  These
 * must precede any fields which use a generic id.
 */
static struct s_GRB_FList 
{
	int	fnum;
	int	process_id;
	char	*fname;
	char 	*fdesc;
	char	*funits;
	float	scale, offset;
} GRB_FList[] = 
{
	/* Pressure (Pa), scale to mb */
	{ 1, ANY, "pres", "Pressure", "mb", 100.0, 0.0 },
	/* Pressure reduced to MSL (Pa), scale to mb */
	{ 2, ANY, "cpres0", "Pressure reduced to MSL", "mb", 100.0, 0.0 },
	/* Station pressure (Pa), scale to mb */
	{ 4, MM5, "stn_pres", "Station pressure", "mb", 100.0, 0.0 },
	/* Ground elevation (m) */
	{ 5, MM5, "topography", "Ground elevation", "m above MSL", 1.0, 0.0 },
	/* Geopotential height (m) */
	{ 7, ANY, "gpalt", "Geopotential height", "m", 1.0, 0.0 },
	/* Geometric height (m) */
	{ 8, ANY, "height", "Geometric height", "m", 1.0, 0.0 },
	/* Temperature (K), scale to C */
	{ 11, ANY, "tdry", "Temperature", "deg C", 1.0, -273.15 },
	/* Virtual temperature (K) */
	{ 12, ANY, "vt", "Virtual temperature", "K", 1.0, 0.0 },
	/* Potential temperature (K) */
	{ 13, ANY, "pt", "Potential temperature", "K", 1.0, 0.0 },
	/* Dew point temperature (K) */
	{ 17, ANY, "dp", "Dew point temperature", "K", 1.0, -273.15 },
	/* Wind direction (deg. true) */
	{ 31, ANY, "wdir", "Wind direction", "deg", 1.0, 0.0 },
	/* Wind speed (m/s) */
	{ 32, ANY, "wspd", "Wind speed", "m/s", 1.0, 0.0 },
	/* u component of wind (m/s) */
	{ 33, ANY, "u_wind", "u component of wind", "m/s", 1.0, 0.0 },
	/* v component of wind (m/s) */
	{ 34, ANY, "v_wind", "v component of wind", "m/s", 1.0, 0.0 },
	/* Montgomery stream function */
	{ 37, ANY, "monty_stream", "Montgomery stream function", 
		  "m**2/s**2", 1.0, 0.0 },
	/* pressure vertical velocity (Pa/s) */
	{ 39, ANY, "pres_w", "pressure vertical velocity", "Pa/s", 1.0, 0.0 },
	/* geometric vertical velocity (m/s) */
	{ 40, ANY, "w_wind", "geometric vertical velocity", "m/s", 1.0, 0.0 },
	/* Absolute vorticity (1/s) */
	{ 41, ANY, "vort", "Absolute vorticity", "1/s", 1.0, 0.0 },
	/* Absolute divergence (1/s) */
	{ 42, ANY, "dvrg", "Absolute divergence", "1/s", 1.0, 0.0 },
	/* Specific humidity */
	{ 51, ANY, "sph", "Specific humidity", "kg/kg", 1.0, 0.0 },
	/* Relative humidity (%) */
	{ 52, ANY, "rh", "Relative humidity", "%", 1.0, 0.0 },
	/* Humidity mixing ratio (kg/kg), scale to g/kg */
	{ 53, ANY, "mr", "Humidity mixing ratio", "g/kg", 0.001, 0.0 },
	/* Total precipitation (kg/m**2)	*/
	{ 61, ANY, "precip", "Total precipitation", "kg/m**2", 1.0, 0.0 },
	/* Large-scale precipitation */
	{ 62, ANY, "ls_precip", "Large-scale precipitation", 
		  "kg/m**2", 1.0, 0.0 },
	/* Convective precipitation (kg/m**2)	*/
	{ 63, ANY, "conv_precip", "Convective precipitation", 
		  "kg/m**2", 1.0, 0.0 },
	/* Soil temperature (scale to C) */
	{ 85, ANY, "soil_temp", "Soil temperature", "deg C", 1.0, -273.15 },
/*
 * ECMWF fields
 */
	/* Geopotential (m**2/s)		*/
	{ 129, ANY, "geopotential", "Geopotential", "m**2/s", 0.3048, 0.0 },
	/* Temperature (K), scale to C 		*/
	{ 130, ANY, "tdry", "Temperature", "deg C", 1.0, -273.15 },
	/* u component of wind (m/s)		*/
	{ 131, ANY, "u_wind", "u component of wind", "m/s", 1.0, 0.0 },
	/* v component of wind (m/s)		*/
	{ 132, ANY, "v_wind", "v component of wind", "m/s", 1.0, 0.0 },
	/* Surface pressure (Pa), scale to mb	*/
	{ 134, ANY, "sfc_pres", "Surface pressure", "Pa", 100.0, 0.0 },
	/* pressure vertical velocity (Pa/s)	*/
	{ 135, ANY, "pres_w", "pressure vertical velocity", "Pa/s", 1.0, 0.0 },
	/* Surface temperature (K), scale to C	*/
	{ 139, ANY, "sfc_temp", "Surface temperature", "deg C", 1.0, -273.15 },
/*
 * MM5 fields
 */
	/* 144 Cloud water specific humidity (kg/kg) */
	{ 144, MM5, "cloud_sph", "Cloud water specific humidity", 
		  "kg/kg", 1.0, 0.0 },
	/* 145 Rain water specific humidity (kg/kg) */
	{ 145, MM5, "rain_sph", "Rain water specific humidity", 
		  "kg/kg", 1.0, 0.0 },
	/* 146 Snow specific humidity (kg/kg) */
	{ 146, MM5, "snow_sph", "Snow specific humidity", "kg/kg", 1.0, 0.0 },
	/* 147 Ice specific humidity (kg/kg) */
	{ 147, MM5, "ice_sph", "Ice specific humidity", "kg/kg", 1.0, 0.0 },
/*
 * ECMWF fields (cont)
 */
	/* Pressure reduced to MSL (Pa), scale to mb */
	{ 151, ANY, "cpres0", "Pressure reduced to MSL", "mb", 100.0, 0.0 },
	/* Relative humidity (%) */
	{ 157, ANY, "rh", "Relative humidity", "%", 1.0, 0.0 },
	/* Condenstation pressure (Pa) scale to mb */
	{ 159, ANY, "condp", "Condenstation pressure", "mb", 100.0, 0.0 },
	/* u component of wind at 10m (m/s)	*/
	{ 165, ANY, "u_wind_10m", "u component of wind at 10m", 
		  "m/s", 1.0, 0.0 },
	/* v component of wind at 10m (m/s)	*/
	{ 166, ANY, "v_wind_10m", "v component of wind at 10m", 
		  "m/s", 1.0, 0.0 },
	/* temperature at 2m (K), scale to C	*/
	{ 167, ANY, "temp_2m", "temperature at 2m", "deg C", 1.0, -273.15 },
	/* dewpoint at 2m (K), scale to C	*/
	{ 168, ANY, "dp_2m", "dewpoint at 2m", "deg C", 1.0, -273.15 },
	/* land/sea (0/1)			*/
	{ 172, ANY, "land/sea", "land/sea (0/1)", "none", 1.0, 0.0 },
/*
 * More MM5 fields
 */
	/* 176 */
	{ 176, MM5, "mm5_176", "mm5_176", "unknown", 1.0, 0.0 },
	/* 177 */
	{ 177, MM5, "mm5_177", "mm5_177", "unknown", 1.0, 0.0 },
};

static int GRB_FList_len = sizeof (GRB_FList) / sizeof (struct s_GRB_FList);
	

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
 *	gg_transform:		pointer to an auxiliary structure used by
 *				types of projections
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
	void	*gg_transform;
} GRB_TypeInfo;


/*
 * Regular lat/lon grids only need the degree spacing in each direction.
 */
typedef struct s_Regular
{
	float	lat_spacing;
	float	lon_spacing;
} Regular;

static Regular Regular2 = 
/*
 * 2.5 degree spacing in both directions
 */
{
	/* lat_spacing */	2.5,
	/* lon_spacing */	2.5
};


/*
 * Polar stereographic grids use this structure to pass transformation
 * parameters to the transformation functions.
 */
typedef struct s_PolarStereo
{
	float	phi1;	  /* latitude of center of projection (radians) */
	float	lambda0;  /* longitude of center of projection (radians) */
	float	scale;	  /* grid spacing (km) at latitude of true scale */
	float	ipole;	  /* i grid index of north pole (C indexing) */
	float	jpole;	  /* j grid index of north pole (C indexing) */
/*
 * Applying the formulas to the north pole (phi = pi/2, lambda = 0)
 * yields the following x and y.
 */
	float	xpole;
	float	ypole;
} PolarStereo;


static PolarStereo Transform27 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 27 [4225-point (65x65) N. Hemisphere polar stereographic 
 * grid oriented 80W].  For type 27, the pole is at grid location (33,33), 
 * in Fortran terms, or (32,32) here in the C world.
 */
{
	/* phi1 */ 	1.047197551,	/* 60.0 deg. north */
	/* lambda0 */ 	-1.396263402,	/* 80.0 deg. west */
	/* scale */	381,
	/* ipole */	32,
	/* jpole */	32,
	/* xpole */	0.0,
	/* ypole */	3412.31689
};

static PolarStereo Transform36 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 36 [1558-point (41x38) N. Hemisphere polar stereographic 
 * grid oriented 105W].  For type 36, the pole is at grid location (19,42), 
 * in Fortran terms, or (18,41) here in the C world.
 */
{
	/* phi1 */	1.047197551,	/* 60.0 deg. north */
	/* lambda0 */	-1.832595715,	/* 105.0 deg. west */
	/* scale */	190.5,
	/* ipole */	18,
	/* jpole */	41,
	/* xpole */	0.0,
	/* ypole */	3412.31689
};

static PolarStereo Transform87 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 87 [5022 point (81x62) N. Hemisphere polar stereographic 
 * grid oriented at 40N, 105W].  Pole at grid location (30.91, 111.53),
 * where the origin of the grid is (0,0).
 */
{
	/* phi1 */	0.6981317,	/* 40.0 deg. north */
	/* lambda0 */	-1.832595715,	/* 105.0 deg. west */
	/* scale */	60.0,		/* 60 km at 40N	   */
	/* ipole */	30.91,
	/* jpole */	111.53,
	/* xpole */	0.0,
	/* ypole */	5938.4
};

static PolarStereo Transform105 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 105 [6889-point (83x83) N. hemisphere polar stereographic
 * grid oriented 105W].  For type 105, the pole is at grid location 
 * (40.5, 88.5), in Fortran terms, or (39.5, 87.5) here in the C world.
 */
{
	/* phi1 */	1.047197551,	/* 60.0 deg. north */
	/* lambda0 */	-1.832595715,	/* 105.0 deg. west */
	/* scale */	90.75464,	/* ~90 km at 60 N */
	/* ipole */	39.5,
	/* jpole */	87.5,
	/* xpole */	0.0,
	/* ypole */	3412.31689
};

static PolarStereo Transform150 =
/*
 * GRIB grid type 150 for MM5: (73x73) N. hemisphere polar stereographic
 * grid oriented 105W].  60 km true spacing at 40 N, 7.573 km spacing at 
 * 60 N.
 */
{
	/* phi1 */	DEG_TO_RAD(60.0),	/* 60.0 deg. north */
	/* lambda0 */	DEG_TO_RAD(-105.0),	/* 105.0 deg. west */
	/* scale */	7.573,			/* km at 60 N */
	/* ipole */	-61.43,
	/* jpole */	825.36,
	/* xpole */	0.0,
	/* ypole */	3412.31689
};

static void	grb_PolarStereoIndex FP ((GRB_TypeInfo *gg, 
		  double lat, double lon, float *ifloat, float *jfloat));
static void	grb_PolarStereoLatLon FP ((GRB_TypeInfo *gg, 
		  double idouble, double jdouble, float *lat, float *lon));
static void	grb_RegularIndex FP ((GRB_TypeInfo *gg, 
		  double lat, double lon, float *ifloat, float *jfloat));
static void	grb_RegularLatLon FP ((GRB_TypeInfo *gg, 
		  double idouble, double jdouble, float *lat, float *lon));

/*
 * Info for GRIB grid types we know how to unpack.
 */
static GRB_TypeInfo GRB_Types[] =
{
	/*
	 * 2: 10512 point (144x73) global longitude-latitude grid
	 * (0,0) at 0E, 90N [C type indexing] matrix layout.  2.5
	 * degree grid, prime meridian not duplicated.
	 */
	{ 2, 144, 73, NULL, NULL, 
		  grb_RegularIndex, grb_RegularLatLon, 144, 73, 
		  -90.0, 0.0, 2.5, 2.5, NULL, NULL, &Regular2 },
	/*
	 * 27: 4225-point (65x65) N. Hemisphere polar stereographic grid
	 * oriented 80W; pole at (32,32) [C type indexing].  381.0 km
	 * spacing at 60N.
	 */
	{ 27, 65, 65, NULL, NULL, 
		  grb_PolarStereoIndex, grb_PolarStereoLatLon, 19, 11, 
		  20.0, -130.0, 4.0, 4.0, NULL, NULL, &Transform27 },
	/*
	 * 36: 1558-point (41x38) N. Hemisphere polar stereographic grid
	 * oriented 105W; pole at (18,41) [C type indexing].  The TDL grid
	 * (N. America) is used to archive LFM and NGM data.  190.5 km
	 * spacing at 60N.
	 */
	{ 36, 41, 38, NULL, NULL, 
		  grb_PolarStereoIndex, grb_PolarStereoLatLon, 36, 21, 
		  20.0, -130.0, 2.0, 2.0, NULL, NULL, &Transform36 },
	/*
	 * 87: 5022 point (81x62) N. Hemisphere polar stereographic grid
	 * oriented at 105W.  Pole at (30.91, 111.53) [C indexing].
	 * Used for RUC. 60 km at 40N.  68.153 km at 60N.
	 */
	{ 87, 81, 62, NULL, NULL, 
		  grb_PolarStereoIndex, grb_PolarStereoLatLon, 60, 24,
		  20.0, -130.0, 1.0, 1.0, NULL, NULL, &Transform87 },
	/*
	 * 105: 6889-point (83x83) N. Hemisphere polar stereographic grid
	 * oriented 105W; pole at (39.5,87.5) [C type indexing].  (U.S. area
	 * subset of NGM Super C grid, used by ETA model).  90.75464 km
	 * spacing at 60N.
	 */
	{ 105, 83, 83, NULL, NULL, 
		  grb_PolarStereoIndex, grb_PolarStereoLatLon, 71, 41, 
		  20.0, -130.0, 1.0, 1.0, NULL, NULL, &Transform105 },
	/*
	 * 148, 149, 150: The assumption at the moment is that these are
	 * the same projections as 87, but with increasing (geometrically
	 * by factor 3) resolution.  The grid parameters change accordingly.
	 */
	{ 150, 73, 73, NULL, NULL, 
		  grb_PolarStereoIndex, grb_PolarStereoLatLon, 73, 73, 
		  34.0, -101.0, 0.1, 0.1, NULL, NULL, &Transform150 },
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


/*
 * Local prototypes
 */
static GFTag	*grb_Open FP ((char *, GFTag *));
static int	grb_ScanFile FP ((GFTag *));
static void	grb_DestroyTag FP ((GFTag *));
static void	grb_ReadRGrid FP ((DataChunk *, GFTag *, GRB_TypeInfo *, int, 
				   int, int, FieldId, int, float *));
static FieldId	grb_Field FP ((GFpds *, ScaleInfo *));
static void	grb_UnpackBDS FP ((GFTag *, int, float *, int, int));
static void	grb_ResetWind FP ((void));
static void	grb_UnpackWind FP ((GFTag *, int, FieldId, int, int, float *, 
				    GRB_TypeInfo *));
static void	grb_DCFinishDefs FP ((DataChunk *, GRB_TypeInfo *, int));
static void	grb_InitGInfo FP ((GRB_TypeInfo *));
static GRB_TypeInfo *grb_GridTypeInfo FP ((GFpds *, GFgds *));
static GRB_TypeInfo *grb_Build255GInfo FP ((GFgds *));



static int
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

	GFTag	tag;
	int	g;
/*
 * Get the file tag.
 */
	if (! grb_Open (file, &tag))
		return (FALSE);
/*
 * Make the (not guaranteed but somewhat reasonable) assumption that the
 * data were written in chronological order.
 */
	*begin = *end = tag.gt_times[0];
	*nsample = 1;

	for (g = 1; g < tag.gt_ngrids; g++)
	{
		if (TC_Less (*end, tag.gt_times[g]))
		{
			*nsample++;
			*end = tag.gt_times[g];
		}
	}
/*
 * Get rid of the tag and return
 */
	close (tag.gt_fd);
	grb_DestroyTag (&tag);

	return (TRUE);
	
# else /* ! GRIB_SLOWSCAN */

/*
 * Fast QueryTime, assuming there's only one time in the file.
 */
	int	fd, len, ednum;
	unsigned char buf[8];
	GFpds	pds;
/*
 * Open the file
 */
	if ((fd = open (file, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "grb_QueryTime: Error %d opening '%s'", 
			  errno, file);
		return (FALSE);
	}
/*
 * Get the first eight bytes ("GRIB" + [octets 5-8 of IS (Edition > 0) ||
 * octets 1-4 of PDS (Edition 0)]) and determine our GRIB edition.
 */
	if ((grb_FindRecord (fd, buf) <= 0) || (read (fd, buf+4, 4) != 4))
	{
		msg_ELog (EF_PROBLEM, 
			  "grb_QueryTime: Can't get first IS from '%s'", file);
		return (FALSE);
	}

	len = grb_ThreeByteInt (buf + 4);
	ednum = (len == 24) ? 0 : (int) buf[7];
/*
 * If we have the first 4 bytes of the PDS, move them over, otherwise read
 * them.
 */
	if (ednum == 0)
		memcpy (&pds, buf + 4, 4);
	else
		read (fd, &pds, 4);
/*
 * Read the rest of the PDS
 */
	read (fd, (char *)(&pds) + 4, sizeof (GFpds) - 4);
/*
 * Extract the time from the PDS
 */
	grb_ReferenceTime (&pds, begin);
	*end = *begin;
	*nsample = 1;

	close (fd);
	return (TRUE);

# endif /* GRIB_SLOWSCAN */
}




static int
grb_OpenFile (of, fname, dp, write)
OpenFile	*of;
char	*fname;
DataFile	*dp;
bool		write;
/*
 * DFA routine to open a file and return a tag.
 */
{
	GFTag *tag = GFTAGP(of);

	if (! grb_Open (fname, tag))
		return (FALSE);

	tag->gt_sfc_only = FALSE;
	return (TRUE);
}




static int
grb_SfcOpenFile (of, fname, dp, write)
OpenFile	*of;
char		*fname;
DataFile	*dp;
bool		write;
/*
 * DFA routine to open a file (for access to surface data only).
 */
{
	GFTag	*tag = GFTAGP(of);

	if (! grb_Open (fname, tag))
		return (FALSE);

	tag->gt_sfc_only = TRUE;
	return (TRUE);
}




static void
grb_CloseFile (ofp)
OpenFile *ofp;
/*
 * Close this file.
 */
{
	GFTag	*tag = GFTAGP(ofp);

	close (tag->gt_fd);
	grb_DestroyTag (tag);
}




static int
grb_SyncFile (ofp)
OpenFile *ofp;
/*
 * Catch up with changes in this file.
 */
{
	GFTag	*tag = GFTAGP(ofp);

	grb_ScanFile (tag);
	return (TRUE);
}



static ZebTime *
grb_GetTimes (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	GFTag *tag = GFTAGP (ofp);

	if (ntime)
		*ntime = tag->gt_ngrids;
	return (tag->gt_times);
}



static DataChunk *
grb_Setup (ofp, fields, nfield, class)
OpenFile *ofp;
FieldId	*fields;
int	nfield;
DataClass	class;
/*
 * Get set up to do this data grab.
 */
{
	int		f;
	DataChunk	*dc;
	FieldId	dims[3], lat_id, lon_id, alt_id;
/*
 * Create an NSpace data chunk
 */
	dc = dc_CreateDC (class);
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




static int
grb_GetData (of, dc, begin, nsample, details, ndetail)
OpenFile	*of;
DataChunk	*dc;
int 		begin;
int 		nsample;
dsDetail	*details;
int		ndetail;
/*
 * Get the data from these sample indices.
 */
{
	int	ndx, nfield, samp, sbegin, send, f;
	int	offset, nalts, test, i;
	float  	badval, ztarget, *lats, *lons, alts[MAXLEVELS];
	bool	onelevel;
	SValue	v;
	GFTag	*tag = GFTAGP (of);
	GFpds	*pds;
	GFgds	*gds;
	FieldId	fids[5], lat_id, lon_id, alt_id, checkfld = -1;
	ZebTime	stime;
	GRB_TypeInfo	*grbinfo = NULL;
	AltUnitType	altunits;
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
		gds = tag->gt_grib[i].gd_gds;

		if ((grbinfo = grb_GridTypeInfo (pds, gds)) != NULL &&
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
			  offset / 3600, ds_PlatformName (dc->dc_Platform), 
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
		grb_GetAlts (of, fids[i], offset, alts, &nalts, &altunits);
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
		float	diff, bestdiff = 99e9;
		int	best = -1;

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
		v.us_v_float : CFG_DC_DEFAULT_BADVAL;

	dc_SetBadval (dc, badval);
/*
 * Do things on a sample by sample basis
 */
	samp = 0;

	for (sbegin = begin; sbegin < begin + nsample; sbegin = send + 1)
	{
	/*
	 * Time for this sample
	 */
		stime = tag->gt_times[sbegin];
	/*
	 * Find the index limits of the next sample
	 */
		send = sbegin;

		for (ndx = sbegin + 1; ndx < begin + nsample; ndx++)
		{
			if (TC_Eq (stime, tag->gt_times[ndx]))
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
	float	lat, lon, newi, newj;
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
			(*ginfo->gg_ndx_module)(ginfo, lat, lon, si++, sj++);
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
			(*ginfo->gg_ll_module)(ginfo, (double) i, (double) j, 
					       &lat, &lon);
			(*ginfo->gg_ndx_module)(ginfo, lat, lon + 0.05,
						&newi, &newj);
			*latang++ = atan2 (newj - j, newi - i);
		/*
		 * Calculate a similar angle for a line of longitude.
		 */
			(*ginfo->gg_ndx_module)(ginfo, lat + 0.05, lon, 
						&newi, &newj);
			*lonang++ = atan2 (newj - j, newi - i);
		}
	}
}
		
			
			

static int
grb_GetAlts (of, fid, offset, alts, nalts, altunits)
OpenFile *of;
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
	GFTag	*tag = GFTAGP (of);
	int	i, count;
	float	temp;
	GFpds	*pds = NULL;
	GFgds	*gds;
	GRB_TypeInfo	*grbinfo = NULL;
	ZebTime	t;
/*
 * Find the first usable grid for the chosen field and forecast offset.
 */
	for (i = 0; i < tag->gt_ngrids; i++)
	{
		pds = tag->gt_grib[i].gd_pds;
		gds = tag->gt_grib[i].gd_gds;

		if ((grbinfo = grb_GridTypeInfo (pds, gds)) != NULL &&
		    grb_UsableLevel (pds, tag->gt_sfc_only) && 
		    grb_Field (pds, NULL) == fid &&
		    grb_Offset (pds) == offset)
		{
			t = tag->gt_times[i];
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
		if (! TC_Eq (tag->gt_times[i], t))
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




static int
grb_GetForecastTimes (of, times, ntimes)
OpenFile *of;
int	*times;
int	*ntimes;
/*
 * Return an array of available forecast offset times (in seconds) for 
 * this file.
 */
{
	GFTag	*tag = GFTAGP (of);
	int	count, i, offset, t;
	GFpds	*pds;
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




static int
grb_GetFields (ofp, sample, nfld, flist)
OpenFile *ofp;
int sample;
int *nfld;
FieldId	*flist;
/*
 * Return the field list at this sample.
 */
{
	int	i, f;
	FieldId	fid;
	GFpds	*pds;
	GFTag	*tag = GFTAGP (ofp);

	*nfld = 0;
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

	return ((*nfld > 0));
}




static int
grb_GetObsSamples (ofp, times, locs, max)
OpenFile *ofp;
ZebTime	*times;
Location *locs;
int max;
/*
 * Return sample info.
 */
{
	GFTag	*tag = GFTAGP (ofp);
	int	i, ntimes, ngrids;
	GRB_TypeInfo	*grbinfo;
	Location	gloc;

	ngrids = tag->gt_ngrids;
/*
 * Find the first grid we know how to unpack and use its type to determine 
 * location
 */
	for (i = 0; i < ngrids; i++)
	{
		
		grbinfo = grb_GridTypeInfo (tag->gt_grib[i].gd_pds,
			 tag->gt_grib[i].gd_gds);
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
	if (TC_Eq (tag->gt_times[0], tag->gt_times[ngrids-1]))
	{
		times[0] = tag->gt_times[0];
		locs[0] = gloc;
		return (1);
	}
/*
 * Nope, we have to go through every grid
 */
	/* times[0] = tag->gt_grib[0].gd_time; */
	times[0] = tag->gt_times[0];
	locs[0] = gloc;
	ntimes = 1;

	for (i = 1; i < ngrids; i++)
	{
	/*
	 * Skip this one if it's the same time as the last one
	 */
		/* if (TC_Eq (tag->gt_grib[i].gd_time, times[ntimes-1])) */
		if (TC_Eq (tag->gt_times[i], times[ntimes-1]))
			continue;
	/*
	 * We have a new time
	 */
		/* times[ntimes] = tag->gt_grib[i].gd_time; */
		times[ntimes] = tag->gt_times[i];
		locs[ntimes] = gloc;
		ntimes++;

		if (ntimes == max)
			return (max);
	}
	return (ntimes);
}




static GFTag *
grb_Open (fname, tag)
char	*fname;
GFTag	*tag;
/*
 * Local file open routine
 */
{
	static int	tagid = 0; /* Unique id for each tag we create */
/*
 * Try to open the file
 */
	if ((tag->gt_fd = open (fname, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "grb_Open: Error %d opening '%s'", 
			  errno, fname);
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
	{
		free (tag->gt_grib[i].gd_pds);
		if (tag->gt_grib[i].gd_gds)
			free (tag->gt_grib[i].gd_gds);
	}
	if (tag->gt_grib)
		free (tag->gt_grib);
	if (tag->gt_times)
		free (tag->gt_times);
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
	int	len, pds_len, bms_len, bds_len;
	int	status, ng, ncopy, ednum, bds_pos;
	unsigned char buf[64];
/*
 * Rewind the file first
 */
	lseek (fd, 0, SEEK_SET);
/*
 * Each grid starts with an Indicator Section.  Loop through grids
 * looking for an IS.  In Edition 1 and beyond, the IS is 8 bytes long,
 * but it's 4 bytes long in Edition 0.
 * An added complication is that sometimes records are separated by 20 blanks.
 */
	ng = tag->gt_ngrids;
	while ((status = grb_FindRecord (fd, buf)) > 0)
	{
		GFpds *pds;
		GFgds *gds = 0;
	/*
	 * Read the next 4 bytes and determine the GRIB edition.  In Edition 1
	 * and beyond, these are the the last 4 bytes of the IS.  For Edition
	 * 0 files, they're the first four bytes of the PDS...
	 */
		if (read (fd, buf, 4) < 4)
		{
			msg_ELog (EF_INFO, 
				  "GRIB file ends with incomplete record");
			status = 0;	/* Treat it like an EOF */
			break;
		}
	/*
	 * Figure out the GRIB Edition.  If the three byte length here is
	 * 24 (the length of an Edition 0 PDS), we assume that it's Edition 0.
	 * Otherwise, assume it's Edition 1 or later and we can get the real
	 * edition number from the fourth byte.
	 */
		len = grb_ThreeByteInt (buf);
		ednum = (len == 24) ? 0 : (int) buf[3];
	/*
	 * Allocate space for a PDS.
	 */
		pds = (GFpds *) calloc (1, sizeof (GFpds));
	/*
	 * For Edition > 0, we still need to read the first four bytes of 
	 * the PDS.
	 */
		if (ednum != 0 && read (fd, buf, 4) < 4)
		{
			msg_ELog (EF_INFO, "Missing PDS at grid %d", ng + 1);
			status = 0;	/* Treat it like an EOF */
			break;
		}

		pds_len = grb_ThreeByteInt (buf);
	/*
	 * Read the rest of the PDS into our buffer. 
	 */
		if (read (fd, buf + 4, pds_len - 4) != pds_len - 4)
		{
			msg_ELog (EF_INFO, "Short PDS at grid %d", ng + 1);
			status = 0;	/* Treat it like an EOF */
			break;
		}
	/*
	 * Copy up to 28 bytes into our PDS space.  We don't need to keep
	 * the stuff (if any) beyond that.
	 */
		ncopy = (pds_len < sizeof (GFpds)) ? pds_len : sizeof (GFpds);
		memcpy (pds, buf, ncopy);
	/*
	 * If we have a Grid Description Section, read it
	 */
		if (pds->section_flags & GDS_FLAG)
		{
		/*
		 * Allocate space for a GDS and read it
		 */
			gds = (GFgds *) calloc (1, sizeof (GFgds));

			if ((status = grb_ReadGDS (fd, gds, ng+1)) <= 0)
				break;
		}
	/*
	 * If there's a Bit Map Section, bypass it.
	 */
		if (pds->section_flags & BMS_FLAG)
		{
		/*
		 * Read the first four bytes of the BMS and get its length
		 */
			if (read (fd, buf, 4) < 4)
			{
				msg_ELog (EF_INFO, 
					  "Missing BMS at grid %d", ng + 1);
				status = 0;	/* Treat it like an EOF */
				break;
			}

			bms_len = grb_ThreeByteInt (buf);
		/*
		 * Seek past the rest
		 */
			lseek (fd, bms_len - 4, SEEK_CUR);
		}
	/*
	 * We're at the Binary Data Section.
	 */
		bds_pos = lseek (fd, 0, SEEK_CUR);

		if (read (fd, buf, 4) < 4)
		{
			msg_ELog (EF_INFO, 
				  "Missing BDS at grid %d", ng + 1);
			status = 0;	/* Treat it like an EOF */
			break;
		}
		bds_len = grb_ThreeByteInt (buf);
	/*
	 * Skip over the rest of the BDS
	 */
		lseek (fd, bds_len - 4, SEEK_CUR);
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
			tag->gt_times = (ZebTime *) malloc (tag->gt_maxgrids *
							    sizeof (ZebTime));
		}
		else if (ng > tag->gt_maxgrids)
		{
			tag->gt_maxgrids += 100;
			tag->gt_grib = (GRIBdesc *) 
				realloc (tag->gt_grib, 
					 tag->gt_maxgrids * sizeof (GRIBdesc));
			tag->gt_times = (ZebTime *) 
				realloc (tag->gt_times, 
					 tag->gt_maxgrids * sizeof (ZebTime));
		}

		tag->gt_grib[ng-1].gd_pds = pds;
		tag->gt_grib[ng-1].gd_gds = gds;
	/*
	 * Extract the reference time from the PDS
	 */
		grb_ReferenceTime (pds, &(tag->gt_times[ng-1]));
	/*
	 * Save the position of the Binary Data Section relative to the 
	 * start of the file, and its length.
	 */
		tag->gt_grib[ng-1].gd_doffset = bds_pos;
		tag->gt_grib[ng-1].gd_bds_len = bds_len;
	/*
	 * Sanity check.  Make sure the last 4 bytes of the GRIB record are 
	 * the GRIB trailer "7777"
	 */
		if (read (fd, buf, 4) < 4)
		{
			msg_ELog (EF_INFO, "Missing trailer at grid %d", ng);
			status = 0;	/* Treat it as an EOF */
			break;
		}
		
		if (strncmp ((char *) buf, "7777", 4))
		{
			msg_ELog (EF_EMERGENCY, 
				  "Bad GRIB trailer '%4s' at grid %d",
				  buf, ng);
			return (FALSE);
		}
	}
/*
 * Complain if we exited on anything other than an EOF
 */
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d reading GRIB file at grid %d", 
			  errno, ng);
		return (FALSE);
	}
	else
		return (TRUE);
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
	float	z, di, dj, val0, val1, val2, val3, *fsi, *fsj;
	GFpds	*pds;
	ZebTime	time;
	bool	u_or_v;
	FieldId	grid_fid, u_fid, v_fid;
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
/*		msg_ELog (EF_PROBLEM, "*BUG*: GRIB level count mismatch!"); */
		msg_ELog (EF_INFO, "grb_ReadRGrid: Can't get %s/%s data",
			  ds_PlatformName (dc->dc_Platform), F_GetName (fid));
		nlevels = 0;
	}
/*
 * If we have no levels, create a grid full of badvals and put that in the
 * data chunk.
 */
	if (nlevels == 0)
	{
		msg_ELog (EF_INFO, "GRIB: No %d hr forecast for %s/%s", 
			  offset / 3600, ds_PlatformName (dc->dc_Platform), 
			  F_GetName (fid));
		
		time = tag->gt_times[sbegin];

		dc_NSAddSample (dc, &time, samp, fid, DC_FillValues);
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
	/* time = tag->gt_grib[sbegin].gd_time;*/
	time = tag->gt_times[sbegin];
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
	float	scale = 1.0, offset = 0.0;
	char	fname[8];
	FieldId	fid = -1;
/*
 * Search through the field table first, to see if we've associated a
 * "real" name with the field.  Fields associated with the particular model
 * take precedence.
 */
	for (i = 0; i < GRB_FList_len; i++)
	{
		if ((GRB_FList[i].process_id == ANY ||
		     GRB_FList[i].process_id == pds->process_id) &&
		    pds->field_id == GRB_FList[i].fnum)
		{
		/*
		 * If the Data store doesn't know about this field yet, 
		 * declare it properly with units.
		 */
			fid = F_Declared (GRB_FList[i].fname);
			if (fid == BadField)
				fid = F_DeclareField (GRB_FList[i].fname,
						      GRB_FList[i].fdesc,
						      GRB_FList[i].funits);
				
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




static void
grb_RegularIndex (gg, lat, lon, ifloat, jfloat)
GRB_TypeInfo *gg;
double	lat, lon;
float	*ifloat, *jfloat;
/*
 * Return the (floating point) array indices for a GRIB regularly-spaced
 * lat/lon grid, given the latitude and longitude of the point.
 */
{
	Regular *reg = (Regular *) gg->gg_transform;

	if (lon < 0.0)
		lon += 360.0;
	
	*ifloat = lon / reg->lon_spacing;
	*jfloat = (lat + 90.0) / reg->lat_spacing;
}




static void
grb_RegularLatLon (gg, idouble, jdouble, lat, lon)
GRB_TypeInfo *gg;
double	idouble, jdouble;
float	*lat, *lon;
/*
 * Turn the (double precision) indices into a GRIB 2 type grid into
 * a latitude and longitude.
 */
{
/*
 * Simple 2.5 degree grid.
 */
	*lon = idouble * 2.5;
	*lat = (jdouble * 2.5) - 90.0;
}




static void
grb_PolarStereoIndex (gg, lat, lon, ifloat, jfloat)
GRB_TypeInfo *gg;
double	lat, lon;
float	*ifloat, *jfloat;
/*
 * Return the (floating point) array indices for a GRIB polar stereographic
 * grid, given a latitude and longitude, and some parameters particular to
 * the grid type.  The formulas used here come from Section 21
 * (Stereographic Projection) of "Map Projections--A Working Manual", USGS
 * Professional Paper 1395.  Parameter names, members of the PolarStereo
 * structure, have been chosen to correspond to those used in the book, and
 * equation numbers from the book are referenced in the comments.  k0, the
 * central scale factor, is taken as 1. 
 */
{
	PolarStereo *ps = (PolarStereo *) gg->gg_transform;
	float	x, y, k, phi = DEG_TO_RAD (lat), lambda = DEG_TO_RAD (lon);
/*
 * Formula 21-4
 */
	k = 2 / (1 + sin (ps->phi1) * sin (phi) + 
		 cos (ps->phi1) * cos (phi) * cos (lambda - ps->lambda0));
/*
 * Formulas 21-2 and 21-3
 */
	x = R_Earth * k * cos (phi) * sin (lambda - ps->lambda0);
	y = R_Earth * k * (cos (ps->phi1) * sin (phi) - 
		 sin (ps->phi1) * cos (phi) * cos (lambda - ps->lambda0));
/*
 * Now turn x and y into grid coordinates based on the north pole, which is
 * the only reference point for which we have grid coordinates.
 */
	*ifloat = ps->ipole + (x - ps->xpole) / ps->scale;
	*jfloat = ps->jpole + (y - ps->ypole) / ps->scale;
}




static void
grb_PolarStereoLatLon (gg, idouble, jdouble, lat, lon)
GRB_TypeInfo *gg;
double	idouble, jdouble;
float	*lat, *lon;
/*
 * Turn the (double precision) indices of a GRIB polar stereographic grid
 * into a latitude and longitude.  The formulas used here come from Section
 * 21 (Stereographic Projection) of "Map Projections--A Working Manual",
 * USGS Professional Paper 1395.  Parameter names, members of the
 * PolarStereo structure, have been chosen to correspond to the variable
 * names in the book, and equation numbers from the book are referenced in
 * the comments. 
 */
{
	PolarStereo *ps = (PolarStereo *) gg->gg_transform;
	float	x, y, rho, c, phi, lambda;
/*
 * First turn our indices into x and y in km.
 */
	x = (ps->scale * (idouble - ps->ipole)) + ps->xpole;
	y = (ps->scale * (jdouble - ps->jpole)) + ps->ypole;
/*
 * Formulas 20-18 and 21-15
 */
	rho = hypot (x, y);
	c = 2 * atan ((double)(rho / (2 * R_Earth)));
/*
 * Formula 20-15
 */
	lambda = ps->lambda0 + atan (x * sin (c) / 
				     (rho * cos (ps->phi1) * 
				      cos (c) - y * sin (ps->phi1) * sin (c)));
/*
 * Formula 20-14
 */
	phi = asin (cos (c) * sin (ps->phi1) + 
		    (y * sin (c) * cos (ps->phi1) / rho));
/*
 * Now convert to degrees and we're done
 */
	*lat = RAD_TO_DEG (phi);
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
	mantissa = grb_ThreeByteInt ((unsigned char *)&(bds_hdr->ref_mant));

	ref = sign * mantissa * pow (2.0, (double) exponent);
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
	dscale = pow (10.0, (double)(-sign * (pds->ds_factor & 0x7FFF)));

	sign = (bds_hdr->bs_factor & 0x8000) ? -1 : 1;
	bscale = pow (2.0, (double)(sign * (bds_hdr->bs_factor & 0x7FFF)));
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




static GRB_TypeInfo *
grb_GridTypeInfo (pds, gds)
GFpds	*pds;
GFgds	*gds;
/*
 * Return a pointer to a GRB_TypeInfo structure for the type of grid associated
 * with the given PDS.  If we don't understand this grid type, return NULL.
 */
{
	int	i;
	GRB_TypeInfo	*grbinfo = NULL;
	static GRB_TypeInfo	*ginfo255 = NULL;
/*
 * Special handling for 255 type grids.  We make the (almost certainly 
 * dangerous) assumption that all 255 grids we see will be similar...
 * At some point, different GRB_TypeInfo's should be created, cached, and 
 * accessed based on the GDS.
 */
	if (pds->grid_id == 255)
	{
		if (! ginfo255)
			ginfo255 = grb_Build255GInfo (gds);

		return (ginfo255);
	}
/*
 * Otherwise, check our list of types we understand
 */
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



static GRB_TypeInfo *
grb_Build255GInfo (gds)
GFgds	*gds;
/*
 * Check the gds data type and if it is one that we know, set up the fields
 * in grbinfo using info from the gds.  Set grbinfo to NULL if it is an 
 * unknown data type.
 */
{
	int	nlat, nlon, j, i;
	float	lat1, lat2, lon1, lon2, latstep, lonstep, *fi, *fj;
	GRB_TypeInfo	*grbinfo;
/*
 * For now we only know how to build a latitude/longitude grid
 *  (data type 0 or 4 in GDS byte 6)
 */
	if ((gds->data_type != 0) && (gds->data_type != 4))
	{
		msg_ELog (EF_PROBLEM, 
			  "grb_Build255GInfo: Unknown data type %d",
			  gds->data_type);
		return (NULL);
	}
/*
 * Allocate a GRB_TypeInfo structure
 */
	grbinfo = (GRB_TypeInfo *) malloc (sizeof (GRB_TypeInfo));
	grbinfo->gg_type = 255;
/*
 * Extract info from the GDS
 */
	nlon = grbinfo->gg_snx = grbinfo->gg_dnx = 
		grb_TwoByteInt (&gds->gd_ni);
	nlat = grbinfo->gg_sny = grbinfo->gg_dny = 
		grb_TwoByteInt (&gds->gd_nj);

	lat1 = 0.001 * (float) grb_ThreeByteSignInt (&gds->gd_1lat);
	lon1 = 0.001 * (float) grb_ThreeByteSignInt (&gds->gd_1lon);

	lat2 = 0.001 * (float) grb_ThreeByteSignInt (&gds->gd_2lat);
	lon2 = 0.001 * (float) grb_ThreeByteSignInt (&gds->gd_2lon);

	latstep = (lat2 - lat1) / (nlat - 1);
	lonstep = (lon2 - lon1) / (nlon - 1);
/*
 * Set our destination grid origin and steps
 */
	grbinfo->gg_dlat = (lat1 < lat2) ? lat1 : lat2;
	grbinfo->gg_dlatstep = fabs (latstep);

	grbinfo->gg_dlon = (lon1 < lon2) ? lon1 : lon2;
	grbinfo->gg_dlonstep = fabs (lonstep);
/*
 * Make the GRB_TypeInfo translation arrays now, since we won't have the 
 * GDS (which we need for this) later.
 */
/*
 * Build arrays mapping destination grid indices into equivalent floating 
 * point source grid indices.
 */
	fi = grbinfo->gg_dsi = (float *) malloc (nlat * nlon * sizeof (float));
	fj = grbinfo->gg_dsj = (float *) malloc (nlat * nlon * sizeof (float));

	for (j = 0; j < nlat; j++)
	{
		for (i = 0; i < nlon; i++)
		{
			*fi++ = (float)((lonstep > 0) ? i : (nlon - i - 1));
			*fj++ = (float)((latstep > 0) ? j : (nlat - j - 1));
		}
	}
/*
 * Build arrays of local angles for lines of constant lat and lon through
 * source grid points.  Since our source is a rectangular grid, the latangs
 * are all zero, and the lonangs are all pi/2.
 */
	fi = grbinfo->gg_slonang = 
		(float *) malloc (nlat * nlon * sizeof (float));
	fj = grbinfo->gg_slatang = 
		(float *) malloc (nlat * nlon * sizeof (float));

	for (j = 0; j < nlat; j++)
	{
		for (i = 0; i < nlon; i++)
		{
			*fi++ = 1.5707963;	/* PI/2 */
			*fj++ = 0.0;
		}
	}
/*
 * Done
 */
	return (grbinfo);
}


