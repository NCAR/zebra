/*
 * $Id: level1b.h,v 1.1 1994-07-31 06:17:29 granger Exp $
 *
 * Header and data scan record structures for Marshall SFC DAAC 
 * Level 1B SSMI data.  Requires Ushort, Ulong, and Uchar
 * definitions in rss.h.
 */

#ifndef _level1b_h_
#define _level1b_h_

#include "rss.h"

/* WARNING: level 1b records, by someone's brilliant planning,
 * have a size of 3150 bytes, which is not divisible by 4.  Therefore
 * the level 1b data structures below actually have size *3152*
 * presumably for alignment.  Therefore, use these structures to
 * map into the useful parts of the data record members, but DO NOT
 * try to read sizeof(L1B_Header) or sizeof(L1B_DataRec), else you'll
 * read past the beginning of the next record in the file.
 */

#define L1B_RECORD_SIZE	((long)3150)

/*
 * QC summary, all scaled by 100
 */
typedef struct _QCSummary {

	Ushort	good;		/* percentage of good */
	Ushort	bad;		/* percentage of bad */
	Ushort	maybe;		/* percentage of questionable */

} QCSummary;


typedef struct _L1B_Header {
/*
 * Identification block
 */
	Ushort	craft_id;		/* spacecraft id */
	Ushort	sensor_id;		/* sensor id */
	Ushort	start_year;		/* year of the century */
	Ushort  start_day;		/* day of the year */
	Ulong	start_secs;		/* seconds, scaled by 1,000 */
	Ulong	num_scans;		/* Number scans after header */
	Ushort	end_year;		/* end year of century */
	Ushort	end_day;		/* end day of year */
	Ulong	end_secs;		/* seconds, scaled by 1,000 */
	Ulong	num_gaps;		/* number of data gaps */
	Ulong	block_id[2];		/* 8-byte processing block id */
/*
 * Preflight calibration data, ---all but on/off flags scaled by 100---
 */
	short	therm_conv[18];		/* thermistor conversion values */
	short	rad_conv[6];		/* radiator conversion values */
	short	rfmix_conv[6];		/* RF mixer conversion values */
	short	hotload_grad;		/* hot load gradient coefficients */
	Ushort	onoff_flags[3];		/* thermistor on/off flags */
/*
 * Bias coefficients
 */
	char	bias_offset[35];	/* scaled by 10,000 */
	char	bias_slope[35];		/* scaled by 10,000 */
/*
 * Antenna Pattern Correction Data, not used at the moment
 */
	short	regn_counters[5];	/* region counters */
	short	regn_1_coeff[42];	/* scaled by 10,000 */
	short	regn_1_factors[28];
	short	regn_2_coeff[42];	/* scaled by 10,000 */
	short	regn_2_factors[28];
	short	regn_3_coeff[42];	/* scaled by 10,000 */
	short	regn_3_factors[28];
	short	regn_4_coeff[42];	/* scaled by 10,000 */
	short	regn_4_factors[28];
	short	regn_5_coeff[42];	/* scaled by 10,000 */
	short	regn_5_factors[28];
/*
 * Data limits
 */
	Ushort	raw_count[14];		/* raw count limits */
	Ushort	cal_count[14];		/* calibration count limits */
/*
 * QC summaries for scan A
 */
	/* Earth locations */
	QCSummary earth_a;

	/* Scene data */
	QCSummary scene_a;

	/* Calibration counts for each channel */
	QCSummary channel_a[7];
/*
 * QC summaries for scan B
 */
	/* Earth locations */
	QCSummary earth_b;

	/* Scene data */
	QCSummary scene_b;

	/* Calibration counts channels 6 and 7 only */
	QCSummary channel_b[2];
/*
 * Spare space
 */
	short	spare[1066];

} L1B_Header;



typedef struct _L1B_DataRec {
/*
 * Scan information, time is start of scan B; scan A is 1899 ms before B
 */
	Ushort	scan_num;		/* Scan number */
	short	vectors[3];		/* orientation vectors */
	short	altitude;		/* reserved */
	short	angle;			/* reserved */
	Ushort	year;			/* year of century */
	Ushort	day;			/* day of year */
	Ulong	msecs;			/* start of scan B in milliseconds */
	Ulong	orbit;			/* orbit number */
/*
 * Scan A calibration, first dimension is the channel, 0..6
 */
//	short	cold_cal_a[7][5];	/* Cold calibration values */
//	short	hot_cal_a[7][5];	/* Hot calibration values */
	short	cold_cal_a[35];		/* Cold calibration values */
	short	hot_cal_a[35];		/* Hot calibration values */
	short	cold_avg_a[7];		/* scaled by 4 */
	short	hot_avg_a[7];		/* scaled by 4 */
	short	offsets_a[7];		/* scaled by 50 */
	short	slopes_a[7];		/* scaled by 50,000 */
/*
 * Scan B calibration, first dimension is [channel = 6, channel = 7]
 */
//	short	cold_cal_b[2][5];	/* Cold calibration values */
//	short	hot_cal_b[2][5];	/* Hot calibration values */
	short	cold_cal_b[10];		/* Cold calibration values */
	short	hot_cal_b[10];		/* Hot calibration values */
	short	cold_avg_b[2];		/* scaled by 4 */
	short	hot_avg_b[2];		/* scaled by 4 */
	short	offsets_b[2];		/* scaled by 50 */
	short	slopes_b[2];		/* scaled by 50,000 */
/*
 * Scan A and B calibration
 */	
	short	hot_therm_avg;		/* hot thermistor avg, scaled by 10 */
	short	tref_ret;		/* temp ref return, scaled by 10 */
	short	tref_volt;		/* temp ref volts, scaled by 10 */
	short	trad;			/* fwd radiator temp, scaled by 10 */
	short	trfmix;			/* RF mixer temp, scaled by 10 */
/*
 * These are pairs of shorts rather than a single long to avoid
 * any bytes being inserted for alignment.
 */
	short	agc_a[2];		/* Scan A automatic gain control */
	short	agc_b[2];		/* Scan B automatic gain control */
/*
 * Earth location data, 1st dimn is cell #, 2nd dimn is [lat, lon]
 */
//	short	locn_a[128][2];		/* scaled by 128 */		
//	short	locn_b[128][2];		/* scaled by 128 */
	short	locn_a[256];		/* scaled by 128 */		
	short	locn_b[256];		/* scaled by 128 */
/* 
 * Scene data, raw counts for channels
 */
//	short	scan_a[64][9];
//	short	scan_b[128][2];
	short	scan_a[576];
	short	scan_b[256];
/*
 * QC control data, scan A
 */
	Uchar	scene_limits_a[72];	/* scene data limits, 576-bit field */
	Uchar	cold_limits_a[5];	/* calibration cold limits, 35 bits */
	Uchar	hot_limits_a[5];	/* calibration hot limits, 35 bits */
	Uchar	cold_avg_limits_a;	/* calib cold avg limits, 7 bits */
	Uchar	hot_avg_limits_a;	/* calib hot avg limits, 7 bits */
	Uchar	offset_limits_a;	/* calib offset limits, 7 bits */
	Uchar	slope_limits_a;		/* calib slope limits, 7 bits */
/*
 * QC control data, scan B
 */
	Uchar	scene_limits_b[32];	/* scene data limits, 256-bit field */
	Uchar	cold_limits_b[2];	/* calibration cold limits, 10 bits */
	Uchar	hot_limits_b[2];	/* calibration hot limits, 10 bits */
	Uchar	cold_avg_limits_b;	/* calib cold avg limits, 2 bits */
	Uchar	hot_avg_limits_b;	/* calib hot avg limits, 2 bits */
	Uchar	offset_limits_b;	/* calib offset limits, 2 bits */
	Uchar	slope_limits_b;		/* calib slope limits, 2 bits */
/*
 * QC control data, scans A and B
 */
	Uchar	data_limit;		/* data limit flags, bits 2-5 */
	Uchar	agc_flags;		/* AGC data existence flags, 2 bits */
	Ushort	quality_info;		/* quality information flags, 6 bits */
/*
 * QC summary flags, scan A, channels 1..7
 */
	Ushort	earth_a;
	Ushort	scene_qc_a;
	Ushort	calibration_a[7];
/*
 * QC summary flags, scan B, channels 6..7
 */
	Ushort	earth_b;
	Ushort	scene_qc_b;
	Ushort	calibration_b[2];
/*
 * Spare
 */
	Uchar	spare[12];

} L1B_DataRec;

/*
 * Prototype for calculating an OUTDAT block from level 1b records.
 */
int l1bta (L1B_Header *hdr, L1B_DataRec *rec, OUTDAT_BLOCK *od);

#endif /* ! _level1b_h_ */
