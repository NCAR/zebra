/*
 * Record structure of SSM/I Antenna Temperature tapes from Remote Sensing 
 * Systems
 *
 * $Id: ssmi_record.h,v 1.2 1993-06-07 18:35:40 granger Exp $
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

/*
 * SSMI Logical Record 
 */
typedef struct _SSMI_Rec
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
	short	cold_a[35];	/* cold counts for A-scan */
	short	hot_a[35];	/* hot counts for A-scan */
	short	agc_b[3];	/* AGC readings for B-scan (counts) */
	short	cold_b[10];	/* cold counts for B-scan */
	short	hot_b[10];	/* hot counts for B-scan */
	short	a_lat[19];	/* geodetic latitudes for A-scan */
	short	a_lon[19];	/* east longitudes for A-scan */
	short	ab_diff[19];	/* packed B-scan minus A-scan lat/lon diffs */
	LoData	lo_data[64];	/* 19, 22, 37 GHz T_A's & sfc. types (K) */
	HiData	hi_data[64];	/* 85 GHz T_A's (K) */
} SSMI_Rec, SSMI_LogicalRec;


/* 
 * SSMI Physical Record is an array of at most 16 logical records
 */
typedef SSMI_LogicalRec SSMI_PhysicalRec[16];
