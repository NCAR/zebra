/*
 * Record structure of SSM/I Antenna Temperature tapes from Remote Sensing 
 * Systems
 *
 * $Id: rss.h,v 1.4 1994-11-16 19:30:02 granger Exp $
 */
/*
 *		Copyright (C) 1993 UCAR
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

#ifndef _rss_h_
#define _rss_h_

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This structure is derived from RSS Technical Report 120191, "User's
 * Manual: SSM/I Antenna Temperature Tapes", Revision 1, by Frank J.
 * Wentz, issued December 1, 1991, from Remote Sensing Systems, 1101
 * College Ave., Suite 220, Santa Rosa, CA 95404.
 *
 * Note 1: For T_A tapes prior to January 1, 1989, "field3" is the time for
 * the spacecraft location.  For T_A tapes from January 1, 1989 through
 * July 31, 1991, "field3" is the precise orbit number.  For the August 1,
 * 1991 T_A tape and thereafter, "field3" contains the satellite
 * identification number and the incidence angle.
 *
 * Note 2: For T_A tapes prior to September 1, 1989, fields "asc_node"
 * through "perigee_ang" actually contain diagnostic data.  Thereafter,
 * the fields contain the orbit parameters shown.
 */

typedef unsigned char	LoData[10];
typedef unsigned char	HiData[12];

typedef unsigned char	Uchar;
typedef unsigned short	Ushort;
typedef unsigned long	Ulong;

/*
 * Some useful defs for dealing with times in SSMI records
 */
#define EPOCH_1987	536457600L	/* 00:00 Jan. 1, 1987 in UNIX epoch */
#define RSS_TIME(rec)	((rec)->time_sec + EPOCH_1987)
#define RSS_LAT(rec)	((rec)->sat_lat * 1.0e-6 - 90.0)
#define RSS_LON(rec)	((rec)->sat_lon * 1.0e-6)
#define RSS_ORBIT(rec)	((rec)->orbit * 1.0e-4)

/*
 * RSS Logical Record 
 */
typedef struct _RSS_Rec
{
	long	time_sec;	/* integer time for scan from begin of 1987 */
	long	orbit;		/* orbit number */
	long	field3;		/* see Note 1 above */
	long	sat_lat;	/* geodetic latitude of spacecraft */
	long	time_frac;	/* fractional time for scan from 1987 (secs) */
	long	sat_lon;	/* east longitude of spacecraft */
	long	alt;		/* altitude of spacecraft (km) */
	short	hl_temp[3];	/* hot load temperatures (K) */
	short	ref_volt[2];	/* reference voltages (counts) */
	short	rf_mix_temp;	/* RF mixer temperature (K) */
	short	radiator_temp;	/* forward radiator temperature (K) */
	short	agc_a[3];	/* AGC readings for A-scan (counts) */
	long	asc_node;	/* ascending node time, secs since 1987 */
	long	period;		/* period of spacecraft orbit (seconds) */
	long	asc_node_local;	/* local time of ascending node (seconds) */
	long	inv_inclination;/* 180 deg. minus orbit inclination */
	long	semimajor;	/* major semi-axis of orbit (km) */
	long	eccentricity;	/* orbit eccentricity */
	long	perigee_ang;	/* angle of perigee of orbit */
	Ushort	cold_a[35];	/* cold counts for A-scan */
	Ushort	hot_a[35];	/* hot counts for A-scan */
	Ushort	agc_b[3];	/* AGC readings for B-scan (counts) */
	Ushort	cold_b[10];	/* cold counts for B-scan */
	Ushort	hot_b[10];	/* hot counts for B-scan */
	Ushort	a_lat[19];	/* geodetic latitudes for A-scan */
	Ushort	a_lon[19];	/* east longitudes for A-scan */
	short	ab_diff[19];	/* packed B-scan minus A-scan lat/lon diffs */
	LoData	lo_data[64];	/* 19, 22, 37 GHz T_A's & sfc. types (K) */
	HiData	hi_data[64];	/* 85 GHz T_A's (K) */
} RSS_Rec, RSS_LogicalRec;


/*
 * The decode function for calculating an OUTDAT block from a RSS
 * logical record.
 */
int decode_rss (
  int i85ghz,	/* decode 85 Ghz channels */
  int itb,	/* provide antenna temps if 0, brightness if 1 */
  int iadj,	/* If 1, along-track correction for F08 prior to 1989 */
  int irec,	/* which logical record in physical rec, 1 to 16 */
  char *lrec	/* the logical record */
);

int decode_tb (void);

#ifdef __cplusplus
}
#endif

#endif /* !_rss_h_ */
