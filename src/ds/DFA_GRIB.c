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
# include <ctype.h>
# include <errno.h>
# include <fcntl.h>
# include <unistd.h>
# include <string.h>

# include <copyright.h>
# include <defs.h>
# include <message.h>
# include <byteorder.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "DataFormat.h"
# include "GRIB.h"

RCSID ("$Id: DFA_GRIB.c,v 3.48 2002-09-17 18:28:43 granger Exp $")


/*
 * GRIB record descriptor
 */
typedef struct s_GRIBdesc
{
	GFpds	*gd_pds;	/* Product definition section	*/
	GFgds   *gd_gds;	/* Grid description section	*/
	GFbmshdr *gd_bmshdr;	/* Bit map section header	*/
	long	gd_bmoffset;	/* offset to the bitmap (if any)*/
	long	gd_doffset;	/* offset to binary data section*/
	int	gd_bds_len;	/* binary data section length	*/
} GRIBdesc;


/*
 * Our tag structure for GRIB files.
 */
typedef struct s_GRB_TypeInfo GRB_TypeInfo;	/* struct defined below */

typedef struct s_GFTag
{
	int		gt_tagid;	/* unique local ID		*/
	int		gt_fd;		/* file descriptor		*/
	int		gt_sfc_only;	/* Surface grids only?		*/
	int		gt_ngrids;	/* grid count			*/
	int		gt_maxgrids;	/* how many grids can we hold?	*/
	GRIBdesc	*gt_grib;	/* Descriptors for each grid	*/
	ZebTime		*gt_times;	/* Times for each grid		*/
	GRB_TypeInfo	*gt_ginfoGDS;	/* grid type info built from GDS*/
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
	grb_GetTimes,			/* Return array of times	*/
	___                             /* Get the associated files     */
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
	grb_GetTimes,			/* Return array of times	*/
	___                             /* Get the associated files     */
};

DataFormat *gribSfcFormat = (DataFormat *) &gribSfcFormatRec;


/*
 * Originating centers
 */
# define OC_ANY		-1
# define OC_NCEP	 7
/* originating center 39 appears in MM5 GRIB files from Dan Hansen in MMM */
# define OC_NCARMMM	39
# define OC_NCAR	60
# define OC_ECMWF	98


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
	int	originating_center;
	char	*fname;
	char 	*fdesc;
	char	*funits;
	float	scale, offset;
} GRB_FList[] = 
{
	/* Pressure (Pa), scale to mb */
	{ 1, OC_ANY, "pres", "pressure", "mb", 100.0, 0.0 },
	/* Pressure reduced to MSL (Pa), scale to mb */
	{ 2, OC_ANY, "cpres0", "pressure reduced to MSL", "mb", 100.0, 0.0 },
	/* Station pressure (Pa), scale to mb */
	{ 4, OC_NCAR, "stn_pres", "station pressure", "mb", 100.0, 0.0 },
	/* NCAR MM5: Ground elevation (m) */
	{ 5, OC_NCAR, "topography", "ground elevation", "m", 1.0, 0.0 },
	/* Geopotential height (m) */
	{ 7, OC_ANY, "gpalt", "geopotential height", "m", 1.0, 0.0 },
	/* Geometric height (m) */
	{ 8, OC_ANY, "height", "geometric height", "m", 1.0, 0.0 },
	/* Temperature (K), scale to C */
	{ 11, OC_ANY, "tdry", "temperature", "degC", 1.0, -273.15 },
	/* Virtual temperature (K) */
	{ 12, OC_ANY, "vt", "virtual temperature", "K", 1.0, 0.0 },
	/* Potential temperature (K) */
	{ 13, OC_ANY, "pt", "potential temperature", "K", 1.0, 0.0 },
	/* Dew point temperature (K) */
	{ 17, OC_ANY, "dp", "dew point temperature", "K", 1.0, -273.15 },
	/* Dew point depression (K) */
	{ 18, OC_ANY, "dp_depression", "dew point depression", "K", 1.0, 
	  -273.15 },
	/* Wind direction (deg. true) */
	{ 31, OC_ANY, "wdir", "wind direction", "deg", 1.0, 0.0 },
	/* Wind speed (m/s) */
	{ 32, OC_ANY, "wspd", "wind speed", "m/s", 1.0, 0.0 },
	/* u component of wind (m/s) */
	{ 33, OC_ANY, "u_wind", "u component of wind", "m/s", 1.0, 0.0 },
	/* v component of wind (m/s) */
	{ 34, OC_ANY, "v_wind", "v component of wind", "m/s", 1.0, 0.0 },
	/* Stream function */
	{ 35, OC_ANY, "stream_fn", "stream function", "km2/s", 1.0e6, 0.0 },
	/* Montgomery stream function */
	{ 37, OC_ANY, "monty_stream", "montgomery stream function", 
	  "m2 s-2", 1.0, 0.0 },
	/* pressure vertical velocity (Pa/s) */
	{ 39, OC_ANY, "pres_w", "pressure vertical velocity", "Pa/s", 1.0, 
	  0.0 },
	/* geometric vertical velocity (m/s) */
	{ 40, OC_ANY, "w_wind", "geometric vertical velocity", "m/s", 1.0, 
	  0.0 },
	/* Absolute vorticity (1/s) */
	{ 41, OC_ANY, "vort", "absolute vorticity", "1/s", 1.0, 0.0 },
	/* Absolute divergence (1/s) */
	{ 42, OC_ANY, "dvrg", "absolute divergence", "1/s", 1.0, 0.0 },
	/* Relative vorticity (1/s) */
	{ 43, OC_ANY, "rel_vort", "relative vorticity", "1/s", 1.0, 0.0 },
	/* Relative divergence (1/s) */
	{ 44, OC_ANY, "rel_dvrg", "relative divergence", "1/s", 1.0, 0.0 },
	/* Specific humidity */
	{ 51, OC_ANY, "sph", "specific humidity", "kg/kg", 1.0, 0.0 },
	/* Relative humidity (%) */
	{ 52, OC_ANY, "rh", "relative humidity", "%", 1.0, 0.0 },
	/* Humidity mixing ratio (kg/kg), scale to g/kg */
	{ 53, OC_ANY, "mr", "humidity mixing ratio", "g/kg", 0.001, 0.0 },
	/* Cloud ice (kg/m**2)	*/
	{ 58, OC_ANY, "cloud_ice", "cloud ice", "kg m-2", 1.0, 0.0 },
	/* Precipitation rate kg m-2 s-1	*/
	{ 59, OC_ANY, "precip_rate", "precipitation rate", 
	  "kg m-2 s-1", 1.0, 0.0 },
	/* Total precipitation (kg/m**2)	*/
	{ 61, OC_ANY, "precip", "total precipitation", "kg m-2", 1.0, 0.0 },
	/* Large-scale precipitation */
	{ 62, OC_ANY, "ls_precip", "large-scale precipitation", "kg m-2", 
	  1.0, 0.0 },
	/* Convective precipitation (kg/m**2)	*/
	{ 63, OC_ANY, "conv_precip", "convective precipitation", "kg m-2", 
	  1.0, 0.0 },
	/* Surface roughness (m) */
	{ 83, OC_ANY, "roughness", "surface roughness", "m", 1.0, 0.0 },
	/* Soil temperature (scale to C) */
	{ 85, OC_ANY, "soil_temp", "soil temperature", "degC", 1.0, -273.15 },
	/* Water runoff (kg m-2) */
	{ 90, OC_ANY, "water_runoff", "water runoff", "kg m-2", 1.0, 0.0 },
	/* Ice concentration (ice = 0; no ice = 0) */
	{ 91, OC_ANY, "ice", "ice concentration", "degC", 1.0, 0.0 },
	/* Latent heat net flux (W m-2) */
	{ 121, OC_ANY, "lhtfl", "latent heat flux", "W m-2", 1.0, 0.0 },
	/* Sensible heat net flux (W m-2) */
	{ 122, OC_ANY, "shtfl", "sensible heat net flux", "W m-2", 1.0, 0.0 },
	/* Momentum flux, u component (N m-2) */
	{ 124, OC_ANY, "uflx", "momentum flux, u", "N m-2", 1.0, 0.0 },
	/* Momentum flux, v component (N m-2) */
	{ 125, OC_ANY, "vflx", "momentum flux, v", "N m-2", 1.0, 0.0 },
    /*
     * The rest of the values (128-254) are reserved for definition by the
     * individual originating centers.
     */
	/* ECMWF: geopotential (m2 s-2) */
	{ 129, OC_ECMWF, "geopotential", "geopotential", "m2 s-2", 1.0, 0.0 },
	/* ECMWF: temperature (K), scale to C 		*/
	{ 130, OC_ECMWF, "temp", "temperature", "degC", 1.0, -273.15 },
	/* ECMWF: u component of wind (m/s)		*/
	{ 131, OC_ECMWF, "u_wind", "u component of wind", "m/s", 1.0, 0.0 },
	/* ECMWF: v component of wind (m/s)		*/
	{ 132, OC_ECMWF, "v_wind", "v component of wind", "m/s", 1.0, 0.0 },
	/* ECMWF: specific humidity		*/
	{ 133, OC_ECMWF, "sph", "specific humidity", "kg/kg", 1.0, 0.0 },
	/* ECMWF: surface pressure (Pa), scale to mb	*/
	{ 134, OC_ECMWF, "sfc_pres", "surface pressure", "hPa", 100.0, 0.0 },
	/* ECMWF: pressure vertical velocity (Pa/s)	*/
	{ 135, OC_ECMWF, "pres_w", "pressure vertical velocity", "Pa/s", 1.0, 
	  0.0 },
	/* ECMWF: vorticity	*/
	{ 138, OC_ECMWF, "vort", "vorticity", "1/s", 1.0, 0.0 },
	/* ECMWF: surface temperature (K), scale to C	*/
	{ 139, OC_ECMWF, "sfc_temp", "surface temperature", "deg C", 1.0, 
	  -273.15 },
	/* ECMWF: convective precipitation	*/
	{ 143, OC_ECMWF, "conv_precip", "convective precipitation", "m", 1.0, 
	  0.0 },
	/* NCAR MM5: Cloud water specific humidity (kg/kg) */
	{ 144, OC_NCAR, "cloud_sph", "cloud water specific humidity", 
	  "kg/kg", 1.0, 0.0 },
	/* NCAR MM5: Rain water specific humidity (kg/kg) */
	{ 145, OC_NCAR, "rain_sph", "rain water specific humidity", 
	  "kg/kg", 1.0, 0.0 },
	/* NCAR MM5: Snow specific humidity (kg/kg) */
	{ 146, OC_NCAR, "snow_sph", "snow specific humidity", "kg/kg", 1.0, 
	  0.0 },
	/* NCAR MM5: Ice specific humidity (kg/kg) */
	{ 147, OC_NCAR, "ice_sph", "ice specific humidity", "kg/kg", 1.0, 
	  0.0 },
	/* Zonal flux of gravity wave stress (N m-2)*/
	{ 147, OC_NCEP, "u_gwd", "zonal flux, gravity wave stress", 
	  "N m-2", 1.0, 0.0 },
	/* Meriodonal flux of gravity wave stress (N m-2) */
	{ 148, OC_NCEP, "v_gwd", "meriodonal flux, gravity wave stress", 
	  "N m-2", 1.0, 0.0 },
	/* ECMWF: pressure reduced to MSL (Pa), scale to mb */
	{ 151, OC_ECMWF, "cpres0", "pressure reduced to MSL", "mb", 100.0, 
	  0.0 },
	/* ECMWF: relative humidity (%) */
	{ 157, OC_ECMWF, "rh", "relative humidity", "%", 1.0, 0.0 },
	/* Turbulent kinetic energy (TKE) (J/kg) */
	{ 158, OC_NCEP, "tke", "turbulent kinetic energy", "J/kg", 1.0, 0.0 },
	/* Condenstation pressure (Pa) scale to mb */
	{ 159, OC_NCEP, "condp", "condenstation pressure", "mb", 100.0, 0.0 },
	/* Clear sky upward solar flux (W m-2) */
	{ 160, OC_NCEP, "csusf", "clear sky upward solar flux", 
	  "W m-2", 1.0, 0.0 },
	/* Clear sky downward solar flux (W m-2) */
	{ 161, OC_NCEP, "csdsf", "clear sky downward solar flux", 
	  "W m-2", 1.0, 0.0 },
	/* Clear sky downward longwave flux (W m-2) */
	{ 163, OC_NCEP, "csdlf", "clear sky downward longwave flux", 
	  "W m-2", 1.0, 0.0 },
	/* ECMWF: u component of wind at 10m (m/s)	*/
	{ 165, OC_ECMWF, "u_wind_10m", "u component of wind at 10m", 
	  "m/s", 1.0, 0.0 },
	/* ECMWF: v component of wind at 10m (m/s)	*/
	{ 166, OC_ECMWF, "v_wind_10m", "v component of wind at 10m", 
	  "m/s", 1.0, 0.0 },
	/* ECMWF: temperature at 2m (K), scale to C	*/
	{ 167, OC_ECMWF, "temp_2m", "temperature at 2m", "deg C", 1.0, 
	  -273.15 },
	/* ECMWF: dewpoint at 2m (K), scale to C	*/
	{ 168, OC_ECMWF, "dp_2m", "dewpoint at 2m", "deg C", 1.0, -273.15 },
	/* ECMWF: land/sea (0/1)			*/
	{ 172, OC_ECMWF, "land/sea", "land/sea (0/1)", "none", 1.0, 0.0 },
	/* latitude */
	{ 176, OC_NCARMMM, "latitude", "latitude", "deg", 1.0, 0.0 },
	{ 176, OC_NCEP, "latitude", "latitude", "deg", 1.0, 0.0 },
	/* longitude */
	{ 177, OC_NCARMMM, "longitude", "longitude", "deg", 1.0, 0.0 },
	{ 177, OC_NCEP, "longitude", "longitude", "deg", 1.0, 0.0 },
	/* downward shortwave radiation flux (W m-2) */
	{ 204, OC_NCEP, "dswrf", "downward shortwave radiation flux", 
	  "W m-2", 1.0, 0.0 },
	/* downward longwave radiation flux (W m-2) */
	{ 205, OC_NCEP, "dlwrf", "downward longwave radiation flux", 
	  "W m-2", 1.0, 0.0 },
	/* upward shortwave radiation flux (W m-2) */
	{ 211, OC_NCEP, "uswrf", "upward shortwave radiation flux", 
	  "W m-2", 1.0, 0.0 },
	/* upward longwave radiation flux (W m-2) */
	{ 212, OC_NCEP, "ulwrf", "upward longwave radiation flux", 
	  "W m-2", 1.0, 0.0 },
	/* Convective precipitation rate (kg m-2 s-1) */
	{ 214, OC_NCEP, "cprat", "convective precip rate", "kg m-2 s-1", 1.0, 
	  0.0 },
	/* ECMWF: total precipitation	*/
	{ 228, OC_ECMWF, "precip", "total precipitation", "m", 1.0, 0.0 },
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
struct s_GRB_TypeInfo
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
}; /* this struct is typedef'ed above to GRB_TypeInfo */


/*
 * Regular lat/lon grids need the degree spacing in each direction and the
 * coordinates of the first point.
 */
typedef struct s_Regular
{
	float	lat_spacing;
	float	lon_spacing;
	float	lat0;	/* lat @ (0,0) */
	float	lon0;	/* lon @ (0,0) */
} Regular;

static Regular Regular2 = 
/*
 * 2.5 degree spacing in both directions
 */
{
    -2.5,	/* lat_spacing */
    2.5,	/* lon_spacing */
    90.0,	/* lat @ (0,0) */
    0.0,	/* lon @ (0,0) */
};


/*
 * Polar stereographic grids use this structure to pass transformation
 * parameters to the transformation functions.
 */
typedef struct s_PolarStereo
{
	char	pole;	  /* 'N' or 'S' */
	float	lambda0;  /* longitude of center of projection (radians) */
	float	scale;	  /* grid spacing (km) at scale_lat */
	float	scale_lat;/* latitude (radians) at which 'scale' is measured */
	float	ipole;	  /* i grid index of north pole (C indexing) */
	float	jpole;	  /* j grid index of north pole (C indexing) */
} PolarStereo;


static PolarStereo Transform27 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 27 [4225-point (65x65) N. Hemisphere polar stereographic 
 * grid oriented 80W].  For type 27, the pole is at grid location (33,33), 
 * in Fortran terms, or (32,32) here in the C world.
 */
{
	/* pole */	'N',
	/* lambda0 */ 	DEG_TO_RAD(-80.0),	/* 80.0 deg. west */
	/* scale */	381,
	/* scale_lat */	DEG_TO_RAD(60.0),	/* 60.0 deg. north */
	/* ipole */	32,
	/* jpole */	32,
};

static PolarStereo Transform36 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 36 [1558-point (41x38) N. Hemisphere polar stereographic 
 * grid oriented 105W].  For type 36, the pole is at grid location (19,42), 
 * in Fortran terms, or (18,41) here in the C world.
 */
{
	/* pole */	'N',
	/* lambda0 */	DEG_TO_RAD(-105.0),	/* 105.0 deg. west */
	/* scale */	190.5,
	/* scale_lat */	DEG_TO_RAD(60.0),	/* 60.0 deg. north */
	/* ipole */	18,
	/* jpole */	41,
};

static PolarStereo Transform87 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 87 [5022 point (81x62) N. Hemisphere polar stereographic 
 * grid oriented at 40N, 105W].  Pole at grid location (30.91, 111.53),
 * where the origin of the grid is (0,0).
 */
{
	/* pole */	'N',
	/* lambda0 */	DEG_TO_RAD(-105.0),	/* 105.0 deg. west */
	/* scale */	60.0,			/* 60 km at 40N	   */
	/* scale_lat */	DEG_TO_RAD(40.0),	/* 40.0 deg. north */
	/* ipole */	30.91,
	/* jpole */	111.53,
};

static PolarStereo Transform104 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 104 [NGM Super C grid, 16170-point (147x110) N. hemisphere 
 * polar stereographic grid oriented 105W].  For type 104, the pole is at grid
 * location (75.5, 109.5), in Fortran terms, or (74.5, 108.5) here in the C 
 * world.
 */
{
	/* pole */	'N',
	/* lambda0 */	DEG_TO_RAD(-105.0),	/* 105.0 deg. west */
	/* scale */	90.75464,		/* ~90 km at 60 N */
	/* scale_lat */	DEG_TO_RAD(60.0),	/* 60.0 deg. north */
	/* ipole */	74.5,
	/* jpole */	108.5,
};



static PolarStereo Transform105 =
/*
 * Origin lat & long, in radians, and scale factor at the origin for
 * GRIB grid type 105 [6889-point (83x83) N. hemisphere polar stereographic
 * grid oriented 105W].  For type 105, the pole is at grid location 
 * (40.5, 88.5), in Fortran terms, or (39.5, 87.5) here in the C world.
 */
{
	/* pole */	'N',
	/* lambda0 */	DEG_TO_RAD(-105.0),	/* 105.0 deg. west */
    	/* scale */	90.75464,		/* ~90 km at 60 N */
	/* scale_lat */	DEG_TO_RAD(60.0),	/* 60.0 deg. north */
	/* ipole */	39.5,
	/* jpole */	87.5,
};

static PolarStereo Transform150 =
/*
 * GRIB grid type 150: (73x73) N. hemisphere polar stereographic
 * grid oriented 105W].  60 km true spacing at 40 N, 7.573 km spacing at 
 * 60 N.
 */
{
	/* pole */	'N',
	/* lambda0 */	DEG_TO_RAD(-105.0),	/* 105.0 deg. west */
	/* scale */	7.573,			/* km at 60 N */
	/* scale_lat */	DEG_TO_RAD(60.0),	/* 60.0 deg. north */
	/* ipole */	-61.43,
	/* jpole */	825.36,
};


static PolarStereo Transform213 =
/*
 * GRIB grid type 213: (129x85) US National - CONUS - Double resolution
 */
{
	/* pole */	'N',
	/* lambda0 */	DEG_TO_RAD(-141.028),	/* 141.028 W */
	/* scale */	95.25,
	/* scale_lat */	DEG_TO_RAD(60.0),	/* 60 N (?) */
	/* ipole */	64.00,
	/* jpole */	88.00,
};


static PolarStereo Transform214 =
/*
 * GRIB grid type 214: (97x69) Regional - Alaska - Double resolution
 */
{
	/* pole */	'N',
	/* lambda0 */	DEG_TO_RAD(-175.641),	/* 175.641 W */
	/* scale */	47.625,
	/* scale_lat */	DEG_TO_RAD(60.0),	/* 60 N (?) */
	/* ipole */	48.00,
	/* jpole */	100.00,
};


static PolarStereo Transform217 =
/*
 * GRIB grid type 217: (277x213) AWIPS Grid over Alaska - double resolution
 */
{
	/* pole */	'N',
	/* lambda0 */	DEG_TO_RAD(-173.0),	/* 173.0 W */
	/* scale */	22.50,
	/* scale_lat */	DEG_TO_RAD(60.0),	/* 60.0 N (?) */
	/* ipole */	187.818,
	/* jpole */	240.397,
};


/*
 * Lambert conformal grids use this structure to pass transformation
 * parameters to the transformation functions.
 */
typedef struct s_LambertConformal
{
	float lambda0;	/* longitude of projection origin (radians) */
	float xspacing;	/* grid spacing (km) in x, at (phi1,lambda0) */
	float yspacing;	/* grid spacing (km) in x, at (phi1,lambda0) */
	float x0;	/* x (km) at grid point (0,0) */
	float y0;	/* y (km) at grid point (0,0) */
	float rho0;	/* projection constant	*/
	float n;	/* projection constant	*/
	float f;	/* projection constant */
} LambertConformal;


static void	grb_RegularIndex FP ((GRB_TypeInfo *gg, 
		  double lat, double lon, float *ifloat, float *jfloat));
static void	grb_RegularLatLon FP ((GRB_TypeInfo *gg, 
		  double idouble, double jdouble, float *lat, float *lon));
static void	grb_PolarStereoIndex FP ((GRB_TypeInfo *gg, 
		  double lat, double lon, float *ifloat, float *jfloat));
static void	grb_PolarStereoLatLon FP ((GRB_TypeInfo *gg, 
		  double idouble, double jdouble, float *lat, float *lon));
static void	grb_LambertConfIndex FP ((GRB_TypeInfo *gg, 
		  double lat, double lon, float *ifloat, float *jfloat));
static void	grb_LambertConfLatLon FP ((GRB_TypeInfo *gg, 
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
	 * 104: 16170-point (147x110) N. hemisphere polar stereographic
	 * grid oriented 105W; pole at (74.5, 108.5) [C type indexing].
	 * (NGM Super C Grid, used by the Eta model).  90.75464 km spacing
	 * at 60N.
	 */
	{ 104, 147, 110, NULL, NULL, 
		  grb_PolarStereoIndex, grb_PolarStereoLatLon, 191, 91, 
		  0.0, -190.0, 1.0, 1.0, NULL, NULL, &Transform104 },
	/*
	 * 105: 6889-point (83x83) N. Hemisphere polar stereographic grid
	 * oriented 105W; pole at (39.5,87.5) [C type indexing].  (U.S. area
	 * subset of NGM Super C grid, used by Eta model).  90.75464 km
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
	/*
	 * 213: (129x85) US National - CONUS - Double resolution
	 */
	{ 213, 129, 85, NULL, NULL, 
	  grb_PolarStereoIndex, grb_PolarStereoLatLon, 221, 111, 
	  20.0, -170.0, 0.5, 0.5, NULL, NULL, &Transform213 },

	/*
	 * 214: (97x69) Regional - Alaska - Double resolution
	 */
	{ 214, 97, 69, NULL, NULL, 
	  grb_PolarStereoIndex, grb_PolarStereoLatLon, 81, 71, 
	  40.0, -170.0, 0.5, 0.5, NULL, NULL, &Transform214 },

	/*
	 * 217: (277x213) AWIPS Grid over Alaska - double resolution
	 */
	{ 213, 277, 213, NULL, NULL, 
	  grb_PolarStereoIndex, grb_PolarStereoLatLon, 401, 301, 
	  32.0, -180.0, 0.2, 0.4, NULL, NULL, &Transform217 },
};

int GRB_NTypes = sizeof (GRB_Types) / sizeof (GRB_TypeInfo);

/*
 * How many vertical levels can we handle? 
 */
# define MAXLEVELS	50

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
				   int, int, FieldId, int, float *, int,
				   AltUnitType));
static FieldId	grb_Field FP ((GFpds *, ScaleInfo *));
static void	grb_UnpackBDS FP ((GFTag *, int, float *, int, int, double));
static void	grb_ResetWind FP ((void));
static void	grb_UnpackWind FP ((GFTag *, int, FieldId, int, int, float *, 
				    GRB_TypeInfo *, double));
static void	grb_DCFinishDefs FP ((DataChunk *, GRB_TypeInfo *, int));
static void	grb_InitGInfo FP ((GRB_TypeInfo *));
static GRB_TypeInfo *grb_GridTypeInfo FP ((GFTag *, GFpds *, GFgds *));
static GRB_TypeInfo *grb_LLGridInfo FP ((GDSLatLon *));
static GRB_TypeInfo *grb_PStereoGridInfo FP ((GDSPolarStereo *));
static GRB_TypeInfo *grb_LambertGridInfo FP ((GDSLambertConformal *));



static int
grb_QueryTime (const char *file, ZebraTime *begin, ZebraTime *end, 
	       int *nsample)
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
		close (fd);
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
grb_OpenFile (of, write)
OpenFile	*of;
zbool		write;
/*
 * DFA routine to open a file and return a tag.
 */
{
	GFTag *tag = GFTAGP(of);
	char *fname = of->of_df.df_fullname;

	if (! grb_Open (fname, tag))
		return (FALSE);

	tag->gt_sfc_only = FALSE;
	return (TRUE);
}




static int
grb_SfcOpenFile (of, write)
OpenFile	*of;
zbool		write;
/*
 * DFA routine to open a file (for access to surface data only).
 */
{
	GFTag	*tag = GFTAGP(of);
	char *fname = of->of_df.df_fullname;

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
	float  	badval, *lats, *lons, alts[MAXLEVELS];
	zbool	onelevel;
	SValue	v;
	GFTag	*tag = GFTAGP (of);
	GFpds	*pds;
	GFgds	*gds;
	FieldId	fids[5], lat_id, lon_id, alt_id, checkfld = BadField;
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
	offset = (ds_IsModelPlatform (dc->dc_Platform) && 
		  ds_GetDetail (DD_FORECAST_OFFSET, details, ndetail, &v)) ?
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

		if ((grbinfo = grb_GridTypeInfo (tag, pds, gds)) != NULL &&
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
		float	ztarget = v.us_v_float;

		for (i = 0; i < nalts; i++)
		{
			diff = fabs (ztarget - alts[i]);
			if (diff < bestdiff)
			{
				bestdiff = diff;
				best = i;
			}
		}

		alts[0] = alts[best];
		nalts = 1;
	}
/*
 * Make the dimension info in the data chunk correct if we haven't done so
 * already.
 */
	if (! dc_NSDefineIsComplete (dc))
		grb_DCFinishDefs (dc, grbinfo, nalts);
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
	dc_NSAddStatic (dc, alt_id, (void *) (alts));
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
				       fids[f], offset, alts, nalts, altunits);
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
    AltUnitType units;
/*
 * Find the first usable grid for the chosen field and forecast offset.
 */
    for (i = 0; i < tag->gt_ngrids; i++)
    {
	pds = tag->gt_grib[i].gd_pds;
	gds = tag->gt_grib[i].gd_gds;

	if ((grbinfo = grb_GridTypeInfo (tag, pds, gds)) != NULL &&
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
	    float alt = grb_ZLevel (pds, &units);
	/*
	 * Sanity check on the count
	 */
	    if (count == MAXLEVELS)
	    {
		msg_ELog (EF_PROBLEM, 
			  "grb_GetAlts: number of levels exceeds max of %d",
			  MAXLEVELS);
		break;
	    }
	/*
	 * Units KLUGE: We sometimes have multiple level types in the same
	 * file, e.g., pressures in mb, sigma levels, altitudes, etc.  We
	 * want to return a list in consistent units, so we arbitrarily
	 * choose levels in mb, if they exist, otherwise return those
	 * matching the first type we find. 
	 */
	    if (! count)
		*altunits = units;
	    else if (units != *altunits)
	    {
		if (units == AU_mb)
		{
		    msg_ELog (EF_DEBUG, "grb_GetAlts switched from %s to mb",
			      au_UnitsName (*altunits));
		    *altunits = units;
		    count = 0;	/* We found our preferred type, so switch! */
		}
		else
		{
		    msg_ELog (EF_DEBUG, 
			      "grb_GetAlts dropped a level because of type");
		    continue;
		}
	    }
	/*
	 * Add this altitude to our list
	 */
	    if (alts)
		alts[count++] = alt;
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
		
		grbinfo = grb_GridTypeInfo (tag, tag->gt_grib[i].gd_pds,
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
/*
 * If we built a custom GRB_TypeInfo from the GDS, release it now.
 */
	if (tag->gt_ginfoGDS)
	{
	    GRB_TypeInfo *ginfo = tag->gt_ginfoGDS;
	/*
	 * Destroy the extra mapping arrays if we ever got far enough to have
	 * grb_InitGInfo() create them.
	 */
	    if (ginfo->gg_dsi)
	    {
		free (ginfo->gg_dsi);
		free (ginfo->gg_dsj);
		free (ginfo->gg_slatang);
		free (ginfo->gg_slonang);
	    }

	    free (ginfo->gg_transform);
	    free (ginfo);
	}
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
	int	len, pds_len, bds_len;
	int	status, ng, ncopy, ednum, bm_pos = 0, bds_pos;
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
		GFbmshdr *bmshdr = 0;
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
	 * If there's a Bit Map Section, save the header and skip past the 
	 * bitmap itself
	 */
		if (pds->section_flags & BMS_FLAG)
		{
			int bmslen;
			int hdrlen = sizeof (GFbmshdr);

			bmshdr = (GFbmshdr *) malloc (hdrlen);
		/*
		 * Read the BMS header
		 */
			if (read (fd, bmshdr, hdrlen) < hdrlen)
			{
				msg_ELog (EF_INFO, 
					  "Missing BMS at grid %d", ng + 1);
				status = 0;	/* Treat it like an EOF */
				break;
			}
		/*
		 * Remember where the bitmap itself begins
		 */
			bm_pos = lseek (fd, 0, SEEK_CUR);
		/*
		 * Seek past the rest
		 */
			bmslen = grb_ThreeByteInt (&(bmshdr->len));
			lseek (fd, bmslen - hdrlen, SEEK_CUR);
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
	 * For the (maybe non-existent) BMS, save the header and the offset 
	 * to the bitmap itself
	 */
		tag->gt_grib[ng-1].gd_bmshdr = bmshdr;
		tag->gt_grib[ng-1].gd_bmoffset = bm_pos;
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
 * Grid info built from a GDS may be added later, but it starts out empty.
 */
	tag->gt_ginfoGDS = NULL;
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
grb_ReadRGrid (dc, tag, ginfo, samp, sbegin, send, fid, offset, alts, nalts, 
	       altunits)
DataChunk	*dc;
GFTag		*tag;
GRB_TypeInfo	*ginfo;
int		samp, sbegin, send;
FieldId		fid;
int		offset;
float		*alts;
int		nalts;
AltUnitType	altunits;
/*
 * Build a grid of the chosen field, using the GRIB grids between indices
 * sbegin and send inclusive, and stuff it into the data chunk.  The
 * desired forecast offset time in seconds is passed in 'offset'.  
 * Return planes for the given altitudes.
 */
{
    int	nsx, nsy, si, sj, i, j;
    int	indices[MAXLEVELS], u_indices[MAXLEVELS], v_indices[MAXLEVELS];
    float	badval = dc_GetBadval (dc);
    int		level, gndx;
    float	*sgrid, *sp, *dgrid, *dp, *lats, *lons;
    float	z, di, dj, val0, val1, val2, val3, *fsi, *fsj;
    GFpds	*pds;
    ZebTime	time;
    zbool	u_or_v;
    FieldId	grid_fid, u_fid, v_fid;
    ScaleInfo	sc;
    unsigned long	nlat, nlon;
    AltUnitType		units;
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
 * Initialize our tables of grid indices
 */
    for (level = 0; level < nalts; level++)
	indices[level] = u_indices[level] = v_indices[level] = -1;
/*
 * Build a list of grids that contain our field and have the right forecast
 * time
 */
    for (gndx = sbegin; gndx <= send; gndx++)
    {
	int	unfilled;
	pds = tag->gt_grib[gndx].gd_pds;
    /*
     * Bag this grid now if the type is wrong, the forecast time is wrong,
     * or it's a not a usable level
     */
	if (pds->grid_id != ginfo->gg_type ||
	    ! grb_UsableLevel (pds, tag->gt_sfc_only) || 
	    grb_Offset (pds) != offset)
	    continue;
    /*
     * Move on if this grid doesn't have a field we want.
     */
	grid_fid = grb_Field (pds, NULL);

	if (grid_fid != fid  &&
	    ! (u_or_v && ((grid_fid == u_fid) || (grid_fid == v_fid))))
	    continue;
    /*
     * Finally, use this grid iff its altitude is in our list of wanted
     * alts.
     */
	z = grb_ZLevel (pds, &units);
	if (units != altunits)
	    continue;

	unfilled = 0;	/* to count how many levels remain unfilled */

	for (level = 0; level < nalts; level++)
	{
	    if (z == alts[level])
	    {
		if (grid_fid == fid)
		    indices[level] = gndx;

		if (grid_fid == u_fid)
		    u_indices[level] = gndx;

		if (grid_fid == v_fid)
		    v_indices[level] = gndx;
	    }

	    if (indices[level] < 0 ||
		(u_or_v && (u_indices[level] < 0 || v_indices[level] < 0)))
		unfilled++;
	}
    /*
     * Break out if we have data for all the wanted vertical levels
     */
	if (unfilled == 0)
	    break;
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
    dgrid = (float *) malloc (nlat * nlon * nalts * sizeof (float));
/*
 * Loop through our list of GRIB records, remapping their data into the
 * destination grid.
 */
    dp = dgrid;
	
    for (level = 0; level < nalts; level++)
    {
    /*
     * If no data at this level, stuff in bad values
     */
	if (indices[level] < 0 ||
	    (u_or_v && (u_indices[level] < 0 || v_indices[level] < 0)))
	{
	    msg_ELog (EF_INFO, "grb_ReadRGrid: No %s/%s data at %.2f %s",
		      ds_PlatformName (dc->dc_Platform), F_GetName (fid),
		      alts[level], au_UnitsName (altunits));
		      
	    for (j = 0; j < nlat; j++)
		for (i = 0; i < nlon; i++)
		    *dp++ = badval;

	    continue;
	}
    /*
     * Get the scaling information for our field and unpack the GRIB
     * Binary Data Section into sgrid.
     */
	grb_Field (tag->gt_grib[indices[level]].gd_pds, &sc);

	if (u_or_v)
	    grb_UnpackWind (tag, offset, fid, u_indices[level], 
			    v_indices[level], sgrid, ginfo, badval);
	else
	    grb_UnpackBDS (tag, indices[level], sgrid, nsx, nsy, badval);
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
	    /*
	     * Get the integer i and j indices for the source grid.
	     */
		si = (int)(*fsi);
		sj = (int)(*fsj);
	    /*
	     * Kludge to allow us to get data *exactly* at the last
	     * index in each direction
	     */
		if (si == (nsx - 1) && si == *fsi)
		    si -= 1;
		
		if (sj == (nsy - 1) && sj == *fsj)
		    sj -= 1;
	    /*
	     * Get the four surrounding points and do the bilinear 
	     * interpolation
	     */
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

		    if (val0 == badval || val1 == badval || 
			val2 == badval || val3 == badval)
			*dp++ = badval;
		    else
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
	FieldId	fid;
/*
 * Search through the field table first, to see if we've associated a
 * "real" name with the field.  Fields associated with the particular model
 * take precedence.
 */
	for (i = 0; i < GRB_FList_len; i++)
	{
		if ((GRB_FList[i].originating_center == OC_ANY ||
		     GRB_FList[i].originating_center == pds->center_id) &&
		    pds->field_id == GRB_FList[i].fnum)
		{
		/*
		 * Get a field id for this field/description/units combo and
		 * grab the scale and offset info.
		 */
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
		fid = F_DeclareField (fname, fname, "unknown");
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
	float dlon = lon - reg->lon0;
	float dlat = lat - reg->lat0;

	if (dlon < 0.0)
		dlon += 360.0;

	*ifloat = dlon / reg->lon_spacing;
	*jfloat = dlat / reg->lat_spacing;
}




static void
grb_RegularLatLon (gg, idouble, jdouble, lat, lon)
GRB_TypeInfo *gg;
double	idouble, jdouble;
float	*lat, *lon;
/*
 * Turn the (double precision) indices of a GRIB regularly-spaced lat/lon
 * grid into a latitude and longitude.
 */
{
	Regular *reg = (Regular *) gg->gg_transform;

	*lon = reg->lon0 + idouble * reg->lon_spacing;
	*lat = reg->lat0 + (jdouble * reg->lat_spacing);
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
 * equation numbers from the book are referenced in the comments.
 */
{
	PolarStereo *ps = (PolarStereo *) gg->gg_transform;
	float	x, y, phi = DEG_TO_RAD (lat), lambda = DEG_TO_RAD (lon);
	float sign = (toupper(ps->pole) == 'N') ? 1.0 : -1.0;
/*
 * Invert formula 21-7/21-11 to get k0.  (Our given scale factor is 1/k
 * at scale_lat)
 */
	float k0 = (1 + sign * sin (ps->scale_lat)) / (2 * ps->scale);
/*
 * Formula 21-8/21-12
 */
	float rho = 2 * R_Earth * k0 * tan (0.25 * M_PI - sign * 0.5 * phi);
/*
 * Formulas 21-5 and 21-6 (using rho)
 */
	float dlon = lambda - ps->lambda0;
	x = rho * sin (dlon);
	y = -rho * cos (dlon);
/*
 * Now turn x and y into grid coordinates based on the grid coordinates
 * of the pole
 */
	*ifloat = ps->ipole + x;
	*jfloat = ps->jpole + y;
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
	float sign = (toupper(ps->pole) == 'N') ? 1.0 : -1.0;
/*
 * Invert formula 21-7/21-11 to get k0.  (Our given scale factor is 1/k
 * at scale_lat)
 */
	float k0 = (1 + sign * sin (ps->scale_lat)) / (2 * ps->scale);
/*
 * First turn our indices into scaled x and y
 */
	x = idouble - ps->ipole;
	y = jdouble - ps->jpole;
/*
 * Formulas 20-18 and 21-15
 */
	rho = hypot (x, y);
	c = 2 * atan2 (rho, 2 * R_Earth * k0);
/*
 * Formula 20-16/20-17
 */
	lambda = ps->lambda0 + atan2 (x, -1 * sign * y);
/*
 * Formula 20-14 (simplified for phi1 = pi/2)
 */
	phi = asin (cos (c));
/*
 * Now convert to degrees and we're done
 */
	*lat = RAD_TO_DEG (phi);
	*lon = RAD_TO_DEG (lambda);
}




static void
grb_LambertConfIndex (gg, lat, lon, ifloat, jfloat)
GRB_TypeInfo *gg;
double	lat, lon;
float	*ifloat, *jfloat;
/*
 * Return the (floating point) array indices for a GRIB Lambert conformal
 * grid, given a latitude and longitude, and some parameters particular to
 * the grid type.  The formulas used here come from Section 15
 * (Lambert Conformal) of "Map Projections--A Working Manual", USGS
 * Professional Paper 1395.  Parameter names, members of the LambertConformal
 * structure, have been chosen to correspond to those used in the book, and
 * equation numbers from the book are referenced in the comments.
 */
{
    LambertConformal *lc = (LambertConformal *) gg->gg_transform;
    float phi = DEG_TO_RAD (lat), lambda = DEG_TO_RAD (lon);
    float theta, rho, x, y;

    rho = R_Earth * lc->f / pow (tan (M_PI / 4 + phi / 2), lc->n); /* 15-1 */
    theta = lc->n * (lambda - lc->lambda0);			   /* 14-4 */

    x = rho * sin (theta);		/* 14-1 */
    y = lc->rho0 - rho * cos (theta);	/* 14-2 */
/*
 * Now turn x and y into grid coordinates based on the north pole, which is
 * the only reference point for which we have grid coordinates.
 */
    *ifloat = (x - lc->x0) / lc->xspacing;
    *jfloat = (y - lc->y0) / lc->yspacing;
}




static void
grb_LambertConfLatLon (gg, idouble, jdouble, lat, lon)
GRB_TypeInfo *gg;
double	idouble, jdouble;
float	*lat, *lon;
/*
 * Turn the (double precision) indices of a GRIB Lambert conformal grid
 * into a latitude and longitude.  The formulas used here come from Section
 * 15 (Lambert Conformal Projection) of "Map Projections--A Working Manual",
 * USGS Professional Paper 1395.  Parameter names, members of the
 * LambertConformal structure, have been chosen to correspond to the variable
 * names in the book, and equation numbers from the book are referenced in
 * the comments. 
 */
{
    LambertConformal *lc = (LambertConformal *) gg->gg_transform;
    float x, y, rho, theta, phi, lambda;
/*
 * First turn our indices into x and y in km.
 */
    x = lc->x0 + lc->xspacing * idouble;
    y = lc->y0 + lc->yspacing * jdouble;
/*
 * Intermediates
 */
    rho = hypot (x, (lc->rho0 - y));	/* 14-10 */
    theta = atan2 (x, lc->rho0 - y);	/* 14-11 */

    if (lc->n < 0)
    {
	rho *= -1;
	theta += M_PI;
    }
/*
 * Phi and lambda
 */
    phi = 2 * atan (pow (R_Earth * lc->f / rho, 1 / lc->n)) - 
	M_PI / 2;						/* 15-5 */
    lambda = theta / lc->n + lc->lambda0;			/* 14-9 */
/*
 * Now convert to degrees and we're done
 */
    *lat = RAD_TO_DEG (phi);
    *lon = RAD_TO_DEG (lambda);
}




static void
grb_UnpackBDS (tag, which, grid, nx, ny, badval)
GFTag	*tag;
int	which, nx, ny;
float	*grid;
double	badval;
/*
 * Unpack the Binary Data Section from the which'th GRIB record in tag into 
 * grid, which is (nx x ny).  Insert the given badval for any missing points.
 */
{
	int	bds_len, sign, mantissa, exponent, n_bits, i, j;
	int	longbits, shift, bfactor, dfactor;
	int	firstbit, firstbyte, lastbit, lastbyte, n_bytes;
	unsigned long	longval, mask;
	zbool	int_data;
	char	flag, *bds, *bitmap;
	float	ref, bscale, dscale, *gp;
	GFpds	*pds = tag->gt_grib[which].gd_pds;
	BDShdr	*bds_hdr;
	GFbmshdr *bms_hdr = tag->gt_grib[which].gd_bmshdr;
/*
 * Get the bitmap, if we have a BMS
 */
	bitmap = 0;

	if (bms_hdr)
	{
	    int	id;
	    int	bmlen;
	/*
	 * The BMS header has an id: 
	 *	0 = the bitmap is here in the file,
	 *	other = an ID for a predefined bitmap (of which we have none)
	 */
	    if ((id = grb_TwoByteInt (&(bms_hdr->bitmap_id))) != 0)
		msg_ELog (EF_PROBLEM, 
			  "grb_UnpackBDS: don't have predefined BMS %d!", id);
	    else
	    {
	    /*
	     * Calculate the length of the bitmap, allocate space, and read 
	     * it out of the file.
	     */
		bmlen = grb_ThreeByteInt (&(bms_hdr->len)) - sizeof (GFbmshdr);
		bitmap = (char *) malloc (bmlen);
		lseek (tag->gt_fd, tag->gt_grib[which].gd_bmoffset, SEEK_SET);
		read (tag->gt_fd, bitmap, bmlen);
	    }
	}
/*
 * Allocate space for the BDS, move to the beginning of the BDS in the file 
 * and read it.  We allocate some extra space at the end of the BDS, since 
 * during unpacking we potentially copy (but don't use) a few bytes from past 
 * the end of the data.
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
 * grid point data (0)	| 	|    additional flags in byte 14?
 *	or		|	|
 * S.H. coeff. (1)	|   original data were integers?
 *			|
 *		simple packing (0)
 *			or
 *		2nd order packing (1)
 *
 * We only deal with grid point data, simple packing, and no additional
 * flags.
 */
	flag = (bds_hdr->flag_ubits >> 4) & 0xF;
	if ((flag & 0x0d) != 0)
	{
		msg_ELog (EF_EMERGENCY,
			  "Can't unpack GRIB BDS with flags: %x", flag);

		free (bds);
		if (bitmap)
		    free (bitmap);
		
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
 */
	dfactor = grb_TwoByteSignInt ((unsigned char *)&(pds->ds_factor));
	dscale = pow (10.0, -(double)dfactor);

	bfactor = grb_TwoByteSignInt ((unsigned char *)&(bds_hdr->bs_factor));
	bscale = pow (2.0, (double)bfactor);
/*
 * Check the bit count and build a mask with the appropriate number of bits set
 */
	n_bits = bds_hdr->n_bits;
	longbits = 8 * sizeof (long);

	if (n_bits > (longbits - 8))
	{
		msg_ELog (EF_EMERGENCY, "Can't unpack GRIB data > %d bits!",
			  longbits - 8);

		free (bds);
		if (bitmap)
		    free (bitmap);
		
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

		free (bds);
		if (bitmap)
		    free (bitmap);

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
		    char *lvp;
		    int b;
		/*
		 * If we have a bitmap, see if this point's bit is set.  If
		 * not, put in the bad value flag and move on.
		 */
		    if (bitmap)
		    {
			int bmbit = j * nx + i;
			int bmbyte = bmbit / 8;
			int havepoint;

			bmbit %= 8;
			havepoint = bitmap[bmbyte] & (1 << (7 - bmbit));

			if (! havepoint)
			{
			    *gp++ = badval;
			    continue;
			}
		    }
		/*
		 * Find the first bit and first byte of this point and 
		 * calculate the shift to move our bits to the bottom of 
		 * a long.
		 */
			firstbit += n_bits;
			lastbit = firstbit + n_bits - 1;
			firstbyte = firstbit / 8;
			lastbyte = lastbit / 8;
			n_bytes = lastbyte - firstbyte + 1;
			shift = 7 - lastbit % 8;

			if (lastbyte >= bds_len)
			    msg_ELog (EF_PROBLEM, 
				      "Bad GRIB unpack, reading beyond BDS!");
		/* 
		 * Copy the required bytes from the BDS into the low-order
		 * bytes of a long.  Remember that the data in the BDS
		 * start at byte 11.
		 */
			longval = 0;
			lvp = (char *)&longval;
			
			for (b = 0; b < n_bytes; b++)
			{
			    if (LittleEndian())
				lvp[n_bytes - b - 1] = bds[11 + firstbyte + b];
			    else
				lvp[sizeof(long) - n_bytes + b] = 
				    bds[11 + firstbyte + b];
			}
		/*
		 * Shift the bits of interest to the bottom of our long and
		 * mask them.
		 */
			longval >>= shift;
			longval &= mask;
		/*
		 * Now we just scale and throw it into the grid
		 */
			*gp++ = (ref + longval * bscale) * dscale;
		}
	}

	free (bds);
	if (bitmap)
	    free (bitmap);
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
grb_UnpackWind (tag, foffset, fid, undx, vndx, grid, ginfo, badval)
GFTag	*tag;
int	foffset;
FieldId	fid;
int	undx, vndx;
float	*grid;
GRB_TypeInfo	*ginfo;
double	badval;
/*
 * Get the u- and v-wind data from the undx'th and vndx'th grids in our
 * data file and return the "true" u- or v-wind grid in grid, which is
 * of size (nx x ny).  When possible, we return data that we've already
 * transmogrified.  Calculated winds are cached and only released if winds
 * from a different file or forecast offset are requested.  Insert the given
 * badval for any missing points.
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
	grb_UnpackBDS (tag, undx, ugrid, nx, ny, badval);
	grb_UnpackBDS (tag, vndx, vgrid, nx, ny, badval);
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
		    if (*u == badval || *v == badval)
			u_true = v_true = badval;
		    else
		    {
			u_true = *u * cos (*latang) + *v * sin (*latang);
			v_true = *u * cos (*lonang) + *v * sin (*lonang);
		    }

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
grb_GridTypeInfo (tag, pds, gds)
GFTag	*tag;
GFpds	*pds;
GFgds	*gds;
/*
 * Return a pointer to a GRB_TypeInfo structure for the type of grid associated
 * with the given PDS.  If we don't understand this grid type, return NULL.
 */
{
    int	i;
    GRB_TypeInfo	*grbinfo;
/*
 * First, check our list of types with prebuilt GRB_TypeInfo structures.
 */
    for (i = 0; i < GRB_NTypes; i++)
    {
	if (GRB_Types[i].gg_type == pds->grid_id)
	    return (GRB_Types + i);
    }
/* 
 * Not in our prebuilt list.  If our tag has a non-null gt_ginfoGDS (i.e.,
 * a GRB_TypeInfo built from a GDS), return that.  This makes the assumption
 * that all GDSs in a file will be the same.  Of course, somebody's
 * bound to violate this assumption at some point... 
 */
    if (tag->gt_ginfoGDS)
	return (tag->gt_ginfoGDS);
/*
 * No info already built from a GDS either.  If we got a GDS, try to unpack 
 * it and build a GRB_TypeInfo structure from it.
 */
    grbinfo = NULL;
    
    if (gds)
    {
	switch (gds->data_type)
	{
	/*
	 * lat/lon grid and Gaussian lat/lon grid
	 */
	  case 0:
	  case 4:
	    grbinfo = grb_LLGridInfo ((GDSLatLon*) gds);
	    break;
	/*
	 * Lambert conformal grid
	 */
	  case 3:
	    grbinfo = grb_LambertGridInfo ((GDSLambertConformal*) gds);
	    break;
	/*
	 * Polar stereographic grid
	 */
	  case 5:
	    grbinfo = grb_PStereoGridInfo ((GDSPolarStereo*) gds);
	    break;
	  default:
	    msg_ELog (EF_PROBLEM, 
		      "grb_GridTypeInfo: Cannot unpack GDS of type %d",
		      gds->data_type);
	    grbinfo = NULL;
	}

	if (grbinfo)
	    grbinfo->gg_type = pds->grid_id;
    /*
     * Stash it in the tag.  We'll assume that all GDSs in this file are
     * the same so we only have to build this struct once.
     */
	tag->gt_ginfoGDS = grbinfo;
    }
	    
    return (grbinfo);
}



static GRB_TypeInfo *
grb_LLGridInfo (gds)
GDSLatLon *gds;
/*
 * Try to build a GRB_TypeInfo using the given lat/lon grid GDS.
 */
{
	float lat1, lat2, lon1, lon2, latstep, lonstep;
	int nlat, nlon;
	GRB_TypeInfo *grbinfo;
	Regular	*transform;
/*
 * Allocate a GRB_TypeInfo structure
 */
	grbinfo = (GRB_TypeInfo *) malloc (sizeof (GRB_TypeInfo));
/*
 * Use the already-defined index and lat/lon modules
 */
	grbinfo->gg_ndx_module = grb_RegularIndex;
	grbinfo->gg_ll_module = grb_RegularLatLon;
/*
 * The big arrays are allocated and built later, if we actually end up
 * unpacking a grid of this type.
 */
	grbinfo->gg_slatang = grbinfo->gg_slonang = NULL;
	grbinfo->gg_dsi = grbinfo->gg_dsj = NULL;
/*
 * Lat and lon counts
 */
	nlon = grbinfo->gg_snx = grbinfo->gg_dnx = grb_TwoByteInt (gds->gd_ni);
	nlat = grbinfo->gg_sny = grbinfo->gg_dny = grb_TwoByteInt (gds->gd_nj);
/*
 * Lat and lon of first and last points in the data
 */
	lat1 = 0.001 * (float) grb_ThreeByteSignInt (gds->gd_lat1);
	lon1 = 0.001 * (float) grb_ThreeByteSignInt (gds->gd_lon1);

	lat2 = 0.001 * (float) grb_ThreeByteSignInt (gds->gd_lat2);
	lon2 = 0.001 * (float) grb_ThreeByteSignInt (gds->gd_lon2);
/*
 * Lat and lon step in the GDS are absolute values, with the signs coming
 * from the scanning mode flags
 */
	lonstep = 0.001 * (float) grb_TwoByteInt (gds->gd_di);
	latstep = 0.001 * (float) grb_TwoByteInt (gds->gd_dj);

	lonstep *= (gds->gd_scanmode & (1<<7)) ? -1 : 1;
	latstep *= (gds->gd_scanmode & (1<<6)) ? 1 : -1;
/*
 * Consistency check
 */
	if ((latstep < 0) && (lat1 < lat2))
	{
	    msg_ELog (EF_INFO, "grb_LLGridInfo: %s",
		      "Latitude scan mode inconsistent with lat1/lat2");
	    msg_ELog (EF_INFO, "grb_LLGridInfo: Believing lat1/lat2, so %s",
		      "data may be flipped N <-> S.");
	    latstep *= -1.0;
	}

	if ((lonstep < 0) && (lon1 < lon2))
	{
	    msg_ELog (EF_INFO, "grb_LLGridInfo: %s",
		      "Longitude scan mode inconsistent with lon1/lon2");
	    msg_ELog (EF_INFO, "grb_LLGridInfo: Believing lon1/lon2, so %s",
		      "data may be flipped E <-> W.");
	    lonstep *= -1.0;

	}
/*
 * Define the destination grid info.
 */
	grbinfo->gg_dlat = (latstep > 0) ? lat1 : lat2;
	grbinfo->gg_dlatstep = fabs (latstep);

	grbinfo->gg_dlon = (lonstep > 0) ? lon1 : lon2;
	grbinfo->gg_dlonstep = fabs (lonstep);
/*
 * Allocate and build our transform structure
 */
	transform = (Regular*) malloc (sizeof (Regular));

	transform->lat_spacing = latstep;
	transform->lon_spacing = lonstep;
	transform->lat0 = lat1;
	transform->lon0 = lon1;

	grbinfo->gg_transform = (void*) transform;
/*
 * Done
 */
	return (grbinfo);
}



static GRB_TypeInfo *
grb_PStereoGridInfo (gds)
GDSPolarStereo *gds;
/*
 * Try to build a GRB_TypeInfo using the given polar stereographic grid GDS.
 */
{
	GRB_TypeInfo *grbinfo;
	PolarStereo *transform;
	int snx, sny;
	float lat1, lon1, ifloat1, jfloat1, latcenter, loncenter;
	float latstep, lonstep;
/*
 * Allocate a GRB_TypeInfo structure
 */
	grbinfo = (GRB_TypeInfo *) malloc (sizeof (GRB_TypeInfo));
/*
 * Use the already-defined index and lat/lon modules
 */
	grbinfo->gg_ndx_module = grb_PolarStereoIndex;
	grbinfo->gg_ll_module = grb_PolarStereoLatLon;
/*
 * Source grid size comes straight from the GDS
 */
	snx = grb_TwoByteInt (gds->gd_nx);
	grbinfo->gg_snx = snx;
	sny = grb_TwoByteInt (gds->gd_ny);
	grbinfo->gg_sny = sny;
/*
 * The big arrays are allocated and built later, if we actually end up
 * unpacking a grid of this type.
 */
	grbinfo->gg_slatang = grbinfo->gg_slonang = NULL;
	grbinfo->gg_dsi = grbinfo->gg_dsj = NULL;
/*
 * Allocate and build our transform structure
 */
	transform = (PolarStereo*) malloc (sizeof (PolarStereo));

	transform->pole = (gds->gd_pole == 0) ? 'N' : 'S';
	transform->lambda0 = DEG_TO_RAD(grb_ThreeByteSignInt (gds->gd_lov) * 
					0.001);
	transform->scale = grb_ThreeByteSignInt (gds->gd_dx) * 0.001;
	transform->scale_lat = DEG_TO_RAD (60.0); /* implied in doc... */

	grbinfo->gg_transform = (void*) transform;
/*
 * Use a the first point lat/lon to calculate the pole indices
 */
	lat1 = grb_ThreeByteSignInt (gds->gd_lat1) * 0.001;
	lon1 = grb_ThreeByteSignInt (gds->gd_lon1) * 0.001;
	
	transform->ipole = 0.0;
	transform->jpole = 0.0;

	grb_PolarStereoIndex (grbinfo, lat1, lon1, &ifloat1, &jfloat1);

	transform->ipole = -ifloat1;
	transform->jpole = -jfloat1;
/*
 * Define the destination grid info.
 */
	grbinfo->gg_dnx = 1.5 * snx;
	grbinfo->gg_dny = 1.5 * sny;

	grb_PolarStereoLatLon (grbinfo, (double)(snx / 2), (double)(sny / 2), 
			       &latcenter, &loncenter);

	latstep = RAD_TO_DEG(transform->scale / R_Earth);
	lonstep = latstep / cos (DEG_TO_RAD(latcenter));
	
	grbinfo->gg_dlat = latcenter - snx / 2 * latstep;
	grbinfo->gg_dlatstep = latstep;

	grbinfo->gg_dlon = loncenter - sny / 2 * lonstep;
	grbinfo->gg_dlonstep = lonstep;
/*
 * Done
 */
	return (grbinfo);
}



static GRB_TypeInfo *
grb_LambertGridInfo (gds)
GDSLambertConformal *gds;
/* 
 * Try to build a GRB_TypeInfo using the given Lambert conformal grid GDS.
 * Formula numbers in comments refer to section 15 (Lambert Conformal
 * Projection) of "Map Projections--A Working Manual", USGS Professional
 * Paper 1395.  
 */
{
    float phi1, phi2, n, f, rho0, lambda0, lambda, phi, theta, rho, dx, dy;
    float centerlat, centerlon, lat, lon;
    int nsi, nsj, ndi, ndj;
    GRB_TypeInfo *ginfo;
    LambertConformal *transform;
/*
 * Allocate a GRB_TypeInfo structure
 */
    ginfo = (GRB_TypeInfo *) malloc (sizeof (GRB_TypeInfo));
/*
 * Use the already-defined index and lat/lon modules
 */
    ginfo->gg_ndx_module = grb_LambertConfIndex;
    ginfo->gg_ll_module = grb_LambertConfLatLon;
/*
 * Source grid size comes straight from the GDS
 */
    ginfo->gg_snx = nsi = grb_TwoByteInt (gds->gd_nx);
    ginfo->gg_sny = nsj = grb_TwoByteInt (gds->gd_ny);
/*
 * The big arrays are allocated and built later, if we actually end up
 * unpacking a grid of this type.
 */
    ginfo->gg_slatang = ginfo->gg_slonang = NULL;
    ginfo->gg_dsi = ginfo->gg_dsj = NULL;
/*
 * Allocate and build our transform structure
 */
    transform = (LambertConformal*) malloc (sizeof (LambertConformal));

    transform->lambda0 = lambda0 = 
	DEG_TO_RAD (grb_ThreeByteSignInt (gds->gd_lov) * 0.001);
    phi1 = DEG_TO_RAD (grb_ThreeByteSignInt (gds->gd_latin1) * 0.001);
    phi2 = DEG_TO_RAD (grb_ThreeByteSignInt (gds->gd_latin2) * 0.001);

    dx = grb_ThreeByteSignInt (gds->gd_dx) * 0.001;	/* km */
    if (gds->gd_scanmode & (1 << 7))
	dx *= -1;
    transform->xspacing = dx;

    dy = grb_ThreeByteSignInt (gds->gd_dy) * 0.001;	/* km */
    if (! (gds->gd_scanmode & (1 << 6)))
	dy *= -1;
    transform->yspacing = dy;
/* 
 * Calculate transform constants using equations 15-3, 15-2, and 15-1a 
 * (abitrarily choosing zero for phi0 to simplify calculation of rho0)
 */
    if (phi1 == phi2)
	n = sin (phi1);
    else
	n = log (cos (phi1) / cos (phi2)) / 
	    log (tan (M_PI / 4 + phi2 / 2) / 
		 tan (M_PI / 4 + phi1 / 2));	/* 15-3 */
    transform->n = n;

    transform->f = f = 
	cos (phi1) * pow (tan (M_PI / 4 + phi1 / 2), n) / n;	/* 15-2 */

    transform->rho0 = rho0 = R_Earth * f;			/* 15-1a */
/*
 * Figure out x and y for the first point of the grid using equations 15-1,
 * 14-4, 14-1 and 14-2.
 */
    phi = DEG_TO_RAD (grb_ThreeByteSignInt (gds->gd_lat1) * 0.001);
    lambda = DEG_TO_RAD (grb_ThreeByteSignInt (gds->gd_lon1) * 0.001);

    rho = R_Earth * f / pow (tan (M_PI / 4 + phi / 2), n);	/* 15-1 */
    theta = n * (lambda - lambda0);				/* 14-4 */

    transform->x0 = rho * sin (theta);			/* 14-1 */
    transform->y0 = rho0 - rho * cos (theta);		/* 14-2 */
/*
 * Stash our transform in ginfo.
 */
    ginfo->gg_transform = (void*) transform;
/*
 * Destination grid info.  Choice of parameters here is somewhat arbitrary,
 * but the goal is to cover most of the area in the source grid, and have
 * roughly the same resolution (at least at the grid centers).
 *
 * We use a few more points in the destination grid than in the source grid.
 */
    ginfo->gg_dnx = ndi = (int)(1.3 * nsi);
    ginfo->gg_dny = ndj = (int)(1.3 * nsj);
/*
 * Use the center of the source (Lambert conformal) grid as the center of our 
 * destination lat/lon grid, and choose destination grid lat/lon spacing 
 * to roughly match those at the center of the source grid.
 */
    grb_LambertConfLatLon (ginfo, 0.5 * (nsi - 1), 0.5 * (nsj - 1),
			   &centerlat, &centerlon);
    grb_LambertConfLatLon (ginfo, 0.5 * (nsi - 1) + 1, 0.5 * (nsj - 1) + 1,
			   &lat, &lon); /* (center_i + 1, center_j + 1) */

    ginfo->gg_dlatstep = fabs (lat - centerlat);
    ginfo->gg_dlonstep = fabs (lon - centerlon);

    ginfo->gg_dlat = centerlat - 0.5 * (ndj - 1) * ginfo->gg_dlatstep;
    ginfo->gg_dlon = centerlon - 0.5 * (ndi - 1) * ginfo->gg_dlonstep;
/*
 * That's all folks...
 */
    return (ginfo);
}
