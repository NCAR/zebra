/*
 * $Id: GRIB.h,v 3.8 1997-08-22 14:19:53 burghart Exp $
 *
 * GRIB file access structures and utility prototypes.
 */


#ifndef _zebra_GRIB_h_
#define _zebra_GRIB_h_

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
	unsigned char	ds_factor;	/* decimal scale factor		*/
	unsigned char	ds_factor1;	/* 2nd byte			*/
	unsigned char	reserved1;	/* any length of reserved data	*/
} GFpds;

/*
 * The GRIB bit map section (BMS) header
 */
typedef struct s_GFbmshdr
{
    unsigned char	len;
    unsigned char	len1;
    unsigned char	len2;
    unsigned char	unused_bits;
    unsigned char	bitmap_id;
    unsigned char	bitmap_id1;
} GFbmshdr;

/*
 * The GRIB grid description section (GDS)
 */
typedef struct s_GFgds
{
	unsigned char	len[3];		/* length of GDS		*/
	unsigned char	nv;		/* Number of vertical levels	*/
	unsigned char	pv_or_pl;	/* PV or PL			*/
	unsigned char	data_type;	/* Data representation type	*/
/*
 * Data-type specific section after octet 6
 */
	unsigned char	gd_buf[256];	/* allow for a big GDS 		*/
} GFgds;


typedef struct s_GDSLatLon
{
	unsigned char	len[3];		/* length of GDS		*/
	unsigned char	nv;		/* Number of vertical levels	*/
	unsigned char	pv_or_pl;	/* PV or PL			*/
	unsigned char	data_type;	/* Data representation type	*/
/*
 * Lat/lon grid type-specific stuff
 */
	unsigned char	gd_ni[2];	/* Number of longitude points 	*/
	unsigned char	gd_nj[2];	/* Number of latitude points   */
	unsigned char	gd_lat1[3];	/* latitude of 1st grid point	*/
	unsigned char	gd_lon1[3];	/* longitude of 1st grid point	*/
	unsigned char	gd_res;		/* resolution and component flags */
	unsigned char	gd_lat2[3];	/* latitude of last grid point	*/
	unsigned char	gd_lon2[3];	/* longitude of last grid point	*/
	unsigned char	gd_di[2];	/* longitudinal increment	*/
	unsigned char	gd_dj[2];	/* latitudinal increment	*/
	unsigned char	gd_scnmd;	/* scanning mode flags		*/
	unsigned char	gd_resv[4];	/* reserved - set to 0		*/
} GDSLatLon;

/*
 * A GDS for polar stereographic grids
 */
typedef struct s_GDSPolarStereo
{
	unsigned char	len[3];		/* length of GDS		*/
	unsigned char	nv;		/* Number of vertical levels	*/
	unsigned char	pv_or_pl;	/* PV or PL			*/
	unsigned char	data_type;	/* Data representation type	*/
/*
 * Polar stereographic type-specific stuff
 */
	unsigned char	gd_nx[2];	/* Number of points along x-axis*/
	unsigned char	gd_ny[2];	/* Number of points along y-axis*/
	unsigned char	gd_lat1[3];	/* lat of 1st grid point (m-deg)*/
	unsigned char	gd_lon1[3];	/* lon of 1st grid point (m-deg)*/
	unsigned char	gd_res;		/* resolution and component flags */
	unsigned char	gd_lov[3];	/* east longitude of orientation*/
	unsigned char	gd_dx[3];	/* grid length along x at lat 60 (m)*/
	unsigned char	gd_dy[3];	/* grid length along y at lat 60 (m)*/
	unsigned char	gd_pole;	/* pole in plane (0:North, 1:South)*/
	unsigned char	gd_scanmode;	/* Scan mode			*/
	unsigned char	gd_reserved[4]; /* Set to 0			*/
} GDSPolarStereo;

/*
 * A GDS for Lambert conformal grids
 */
typedef struct s_GDSLambertConformal
{
	unsigned char	len[3];		/* length of GDS		*/
	unsigned char	nv;		/* Number of vertical levels	*/
	unsigned char	pv_or_pl;	/* PV or PL			*/
	unsigned char	data_type;	/* Data representation type	*/
/*
 * Lambert conformal type-specific stuff
 */
	unsigned char	gd_nx[2];	/* Number of points along x-axis*/
	unsigned char	gd_ny[2];	/* Number of points along y-axis*/
	unsigned char	gd_lat1[3];	/* lat of 1st grid point (m-deg)*/
	unsigned char	gd_lon1[3];	/* lon of 1st grid point (m-deg)*/
	unsigned char	gd_res;		/* resolution and component flags */
	unsigned char	gd_lov[3];	/* east longitude of orientation*/
	unsigned char	gd_dx[3];	/* grid length along x (m)	*/
	unsigned char	gd_dy[3];	/* grid length along y (m)	*/
	unsigned char	gd_pole;	/* projection center flag	*/
	unsigned char	gd_scanmode;	/* Scan mode			*/
	unsigned char	gd_latin1[3];	/* 1st lat of cone/earth intersection*/
	unsigned char	gd_latin2[3];	/* 2nd lat of cone/earth intersection*/
	unsigned char	gd_spol_lat[3];	/* lat of southern pole		*/
	unsigned char	gd_spol_lon[3];	/* lon of southern pole		*/
	unsigned char	gd_reserved[2]; /* Set to 0			*/
} GDSLambertConformal;

/*
 * Flag bits for section_flags
 */
# define GDS_FLAG	(1<<7)
# define BMS_FLAG	(1<<6)

/*
 * Binary Data Section header
 */
typedef struct s_BDShdr
{
	char		bds_len;	/* length of BDS		*/
	char		bds_len1;	/* 2nd byte of length		*/
	char		bds_len2;	/* 3rd byte of length		*/
	unsigned char	flag_ubits;	/* Flag & number of unused bits	*/
	char		bs_factor;	/* binary scale factor		*/
	char		bs_factor1;	/* 2nd byte			*/
	char		ref_top;	/* sign & characteristic bits	*/
					/* of reference value		*/
	char		ref_mant;	/* mantissa of reference value	*/
	char		ref_mant1;	/* 2nd byte of mantissa		*/
	char		ref_mant2;	/* 3rd byte of mantissa		*/
	unsigned char	n_bits;		/* bits per datum		*/
} BDShdr;

/*
 * One reasonable spherical radius for the earth, in km
 */
static const double R_Earth = 6367.47;

/*
 * Degree to radian converter and vice versa
 */
# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define RAD_TO_DEG(x)	((x) * 57.29577951)

extern int grb_FindRecord FP ((int fd, unsigned char *buf));
extern char *grb_GDSRepName FP ((GFgds *gds));
extern int grb_TwoByteInt FP ((unsigned char *));
extern int grb_TwoByteSignInt FP ((unsigned char *));
extern int grb_ThreeByteInt FP ((unsigned char *));
extern int grb_ThreeByteSignInt FP ((unsigned char *));
extern int grb_ReadGDS FP ((int fd, GFgds *gds_ret, int ng));
extern void grb_ReferenceTime FP ((GFpds *pds, ZebTime *zt));
extern int grb_Offset FP ((GFpds *));
extern bool grb_UsableLevel FP ((GFpds *, int));
extern float grb_ZLevel FP ((GFpds *, AltUnitType *));

#endif /* ndef _zebra_GRIB_h_ */
