/*
 * RP7 housekeeping definitions.  This is taken almost directly from 
 * the RSF radar display code.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
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


#include "Types.h"

#define CORR_FACT  (182.04444444)	/* angles(degrees) to natural binary *
					 * 2^16 / 360.0 */
#define DEG_TO_BIN (182.04444444)	/* angles(degrees) to natural binary *
					 * 2^16 / 360.0 */

#define BIN_TO_DEG   (0.00549316)	/* natural binary angles to degrees *
					 * 360.0 / 2^16 */

#define LAT_CF (4.0 * DEG_TO_BIN)
#define LON_CF (2.0 * DEG_TO_BIN)

/*
 * Structure describing parameter scaling and bias info.
 */
typedef struct _ParmInfo
{
	unsigned short pi_scale;	/* Scale factor	* 100	*/
	short pi_bias;			/* Bias factor * 100 	*/
} ParmInfo, SCALING;


/******************************************************************************
*                                                                             *
* housekeeping header                                                         *
*                                                                             *
* NOTE: the indexes in the left correspond to those in the MHR housekeeping   *
*       *NOT ALL INDEXES MAY BE IN USE*      				      *
*                                                                             *
+                                                                             *
******************************************************************************/

typedef struct {
/*001*/	 short log_rec_num;	/* logical record # for this beam */
/*002*/	USHORT field_tape_seq;	/* field tape sequence # =0 */
/*003*/	USHORT rec_type;	/* record type 0 = data */
/*004*/	USHORT year;		/* last two digits */
/*005*/	USHORT month;
/*006*/	USHORT day;
/*007*/	USHORT hour;
/*008*/	USHORT minute;
/*009*/	USHORT second;
/*010*/	USHORT azimuth;		/* degrees * CF */
/*011*/	USHORT elevation;	/* degrees * CF */
/*012*/	USHORT rhozero1;	/* range to leading edge of 1st gate*/
/*013*/	USHORT rhozero2;	/* = rhozero1 + rhozero2/1000 (in km) */
/*014*/	USHORT gate_spacing;	/* gate spacing (m) = */
				/*0 225 for Batch or Doppler,
				 * 450 for Reflectivity mode */
/*015*/	USHORT gates_per_beam;	/* gates per beam = 1024 */
/*016*/	USHORT samples_per_beam;/* number of samples per beam */
/*017*/	 short tp_level; 	/* test pulse power CW measured by operator
				 *(dBm * 10) */
/*018*/	USHORT avg_xmit_pwr;	/* dBm * 10 */
/*019*/	USHORT pulse_width;	/* 1.67 us (nanoseconds )*/
/*020*/	USHORT prfx10;		/* PRF (Hz * 10), typ. 1235 Hz */
/*021*/	USHORT wavelength;	/* wavelength (cm * 100) = 10.44 cm */
/*022*/	USHORT seq_sweep;	/* running counter of elevation scans
				 * since last start of operations */
/*023*/	USHORT sweep_index;	/* identifies the sweep (scan) in the *
				 * volume scan (#'s 1 - 16) */
/*024*/	USHORT unused_24;
/*025*/	USHORT unused_25;
/*026*/	USHORT scan_mode;	/* 8 = Surveillance */
/*027*/	USHORT cw_az_lim;	/* azimuth angle of first dwell */
/*028*/	USHORT ccw_az_lim;	/* azimuth angle of last dwell */
/*029*/	USHORT up_elev_lim;	/* 0 */
/*030*/	USHORT lo_elev_lim;	/* 0 */
/*031*/	USHORT fixed;		/* Fixed angle	*/
/*032*/	USHORT sig_source;	/* signal source (0 = radar) */
/*033*/	 short coupler_loss;	/* includes all fixed loss (dB * 10) */
/*034*/	USHORT tp_strt;		/* test pulse start (km * 100) */
/*035*/	USHORT tp_width;        /* test pulse width (km * 100) */
/*036*/	USHORT pri_co_bl;
/*037*/	USHORT scnd_co_bl;
/*038*/	 short tp_atten;	/* tp attenuator setting (dB * 10) */
/*039*/	 short sys_gain;	/* system gain (dB * 10) */
/*040*/	USHORT fix_tape;	/* for corrections during post-processing*/
/*041*/	 short tp_freq_off;	/* tp frequency offset (Hz * 10) */
/*042*/	USHORT log_bw;          /* Log channel bandwidth, 
				   primary wavelength (kHz) */
/*043*/	USHORT lin_bw;		/* bandwidth (kHz) */
/*044*/	USHORT ant_bw;		/* antenna beamwidth degrees * 100*/
/*045*/	USHORT ant_scan_rate;	/* deg/sec * CF */
/*046*/	USHORT radar_const;	/*  (dB * 10); this is the real-time */
		      	        /* constant used by RP7 to compute dBZ. */
/*047*/	USHORT unused_47;
/*048*/	USHORT vol_count;	/* running count of full or partial
				 * volume scans since last start of 
				 * operations */
/*049*/	USHORT clutter_filter;	/* 0  = OFF */
/*050*/	USHORT polarization;	/*0 = horizontal */
/*051*/	USHORT prf1;
/*052*/	USHORT prf2;
/*053*/	USHORT prf3;
/*054*/	USHORT prf4;
/*055*/	USHORT prf5;
/*056*/	USHORT ped_id;		/* pedestal id */
/*057*/	USHORT rec_cnt_of;	/* record count overflow */
/*058*/	USHORT altitude;
/*059*/	 short latitude;	/* degrees * LAT_CF */
/*060*/	 short longitude;	/* degrees * LON_CF */
/*061*/	USHORT transit;		/* 0 = in a scan */
/*062*/	USHORT ds_id;		/* -1 */
/*063*/	USHORT rs_id;		/* 0x4d48 - 'MH' */
/*064*/	USHORT proj_num;	
/*065*/	USHORT sz_hsk;		/* # of words of housekeeping = 100 */
/*066*/	USHORT sz_cur_log;	/* 100 + 4*512 */
/*067*/	USHORT num_log_rcd;	/* Number of logical records in the current
				 * physical record (upper 8 bits), 
				 * and the number of the current logical
				 * record (lower 8 bits)
				 *
				 * 10|1-N on exabyte *
				 * 1|1 on 1/2" tape 
				 */
/*068*/	USHORT parm_per_gate;
# ifdef notdef
/*069*/	USHORT parm1_desc;
/*070*/	USHORT parm2_desc;
/*071*/	USHORT parm3_desc;
/*072*/	USHORT parm4_desc;
/*073*/	USHORT parm5_desc;
/*074*/	USHORT parm6_desc;
# endif
	USHORT parm_desc[6];
/*075*/	 short tp_max;		/* test pulse maximum (dBm * 10) */
/*076*/	 short tp_min;		/* test pulse minimum (dBm * 10) */
/*077*/	 short tp_step;	/* test pulse step (dB * 10) */
/*078*/	USHORT vol_scan_prg;
# ifdef notdef
/*079*/	USHORT parm1_scale;	/* scale * 100 for Z */
/*080*/	 short parm1_bias;	/* bias * 100 for Z */
/*081*/	USHORT parm2_scale;	/* scale * 100 for V */
/*082*/	 short parm2_bias;	/* bias * 100 for V */
/*083*/	USHORT parm3_scale;	/* scale * 100 for W */
/*084*/	 short parm3_bias;	/* bias * 100 for W */
/*085*/	USHORT parm4_scale;
/*086*/	 short parm4_bias;
/*087*/	USHORT parm5_scale;
/*088*/	 short parm5_bias;
/*089*/	USHORT parm6_scale;
/*090*/	 short parm6_bias;
# endif
	ParmInfo parm_info[6];	/* Parameter scaling info	*/
/*091*/	USHORT rp7_bit_flags;	/* RP7 bit flags. */
                     /* 0) RP7 beam indexing (1=on)		 
                      * 1) test mode indicator (1=in test mode; this 
                      *    flag is used by real-time algorithms to ignore
                      *    data that contain known artificats such as test
                      *    pulses, test patterns, imposed test noise, etc).
                      * 2-14) currently undefined, and should be set to 0.
                      * 15) live or simulated flag (1=simulated or replayed)
		      */

/*092*/	USHORT proc_noise_floor;
		     /* Radar processor noise floor expressed in internal RP7
                      *  power units; this variable is dynamic in real time.
                      * [ -10 log( pnoise ), units of dB( RP7 pcount) * 10 ]
		      */

/*093*/	USHORT slope_power_cal;
		    /*  The assumed slope of the receiver-RP7 power calibration
                     * expressed in milliwatts per internal RP7 power unit;
                     * note that this variable is somewhat dynamic in real time.
		     * [ -10 log( krcvr ), units of dB(mw/RP7 pcount) * 10 ]
		     */

/*094*/	USHORT unused_94;
/*095*/	USHORT unused_95;
/*096*/	USHORT unused_96;
/*097*/	USHORT unused_97;
/*098*/	USHORT unused_98;
/*099*/	USHORT unused_99;
/*100*/	USHORT live_or_sim;	/* LIVE or SIM */
/*101-160*/ unsigned short unused5[60];
/*161-170*/ unsigned short parm_desc2[10];
/*171*/ unsigned short src_test_bus;
/*172*/ unsigned short add_test_bus;
/*173*/ unsigned short half_prf;
/*174*/ unsigned short ptape_unit;
/*175*/ unsigned short stape_unit;
/*176*/ unsigned short word_176;
/*177*/ unsigned short word_177;
/*178*/ unsigned short word_178;
/*179*/ unsigned short cal_attn_step;
/*180*/ unsigned short cal_freq_step;
/*181*/ unsigned short r_sq_offset;
/*182*/ unsigned short refl_thres;
/*183*/ unsigned short shifter_cnts;
/*184*/ unsigned short attn_setting;
/*185*/ unsigned short swp_center;
/*186*/ unsigned short cp2_mode;
/*187*/ unsigned short non_dual_mode;
/*188*/ unsigned short word_188;
/*189-200*/ unsigned short unused6[12];
/*201*/ unsigned short wavelength2;	/* wavelength, secondary system */
/*202*/ unsigned short atp2;		/* average tx pwr, secondary wavlen */
/*203*/ unsigned short pulse_width2;
/*204*/ unsigned short prf_secondary;
/*205*/ unsigned short sys_gain2;
/*206*/ unsigned short log_bw2;
/*207*/ unsigned short ant_bw2;
/*208*/ unsigned short polarization2;
/*209-211*/ unsigned short unused7[3];
/*212-231*/
 	SCALING cal_info2[10];
/*232-240*/ unsigned short unused8[9];
/*241-246*/ unsigned short aircraft_info[6];
/*247*/ unsigned short dual_pol_mode;
/*248*/ unsigned short dual_pol_switch;
/*249-256*/ unsigned short unused9[8];

} Housekeeping;

/* bit flag definitions for rp7_bit_flags field */

#define RP7_HSK_BEAM_INDEX  (1<<0)
#define RP7_HSK_TEST_MODE  (1<<1)
#define RP7_HSK_SIMULATED  (1<<15)

#define LIVE 0xAAAA
#define SIM 0x5555

#define PPI_SCAN (1)
#define RHI_SCAN (3)
#define SUR_SCAN (8)	
#define COP_SCAN (2)	/* co-planes */

/*
 * actual calculations are of the form:
 *	Z (dBZ) = reflect * parm1_scale/100.0 + parm1_bias/100.0
 *	V (m/s) = velocity * parm2_scale/100.0 + parm2_bias/100.0
 *	W (m/s) = width * parm3_scale/100.0 + parm3_bias/100.0
 */

/* defined constants for "parameter descriptors" */
#define HSK_PD_PWRCNT 0x1008	/* power counts */
/* #define HSK_PD_NCP  0x6008 *//* normalized coherent power */
#define HSK_PD_XPWR 0x6008	/* X-band power */
#define HSK_PD_ZDR  0x2008	/* ZDR */
#define HSK_PD_VEL  0x8008	/* velocity in m/s */
#define HSK_PD_WIDTH 0x3008	/* width in m/s */
#define HSK_PD_DBZ 0x1008	/* reflectivity in dBZ */
#define HSK_PD_SNR 0x4008	/* SNR */

/*
 * Scan modes.
 */
# define SM_CAL		0
# define SM_PPI		1
# define SM_COP		2
# define SM_RHI		3
# define SM_SUR		8


/*
 * Describe the dual polarization mode word.
 */
struct dual_pol_mode {
    unsigned int unused     :  8;
    unsigned int half_nyq   :  1; /* Half Nyquist Enable (Velocity interface
				   * 0 = HV pairs
				   * 1 = HH and VV pairs
				   */
    unsigned int ldr_scale  :  2; /* 00 +-  6 db
				   * 01 +- 12 db
				   * 10 +- 24 db
				   * 11 +- 48 db
				   */
    unsigned int ldr_limit  :  1; /* LDR limit enable (0 = folding) */
    unsigned int zdr_scale  :  2; /* 00 +-  3 db
				   * 01 +-  6 db
				   * 10 +- 12 db
				   * 11 +- 24 db
				   */
    unsigned int zdr_limit  :  1; /* ZDR limit enable (0 = folding) */
    unsigned int dual_polar :  1; /* Dual Polarization Mode Enable */
};
