/*
 * RP7 housekeeping definitions.  This is taken almost directly from 
 * the RSF radar display code.
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
/*031*/	USHORT target_elev;	/* the elevation angle from the
				 * scan strategy table */
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
/*069*/	USHORT parm1_desc;
/*070*/	USHORT parm2_desc;
/*071*/	USHORT parm3_desc;
/*072*/	USHORT parm4_desc;
/*073*/	USHORT parm5_desc;
/*074*/	USHORT parm6_desc;
/*075*/	 short tp_max;		/* test pulse maximum (dBm * 10) */
/*076*/	 short tp_min;		/* test pulse minimum (dBm * 10) */
/*077*/	 short tp_step;	/* test pulse step (dB * 10) */
/*078*/	USHORT vol_scan_prg;
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
/*101*/ 			/** TRIP POINT FOR LISTING HSKP **/
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
#define HSK_PD_NCP  0x0608	/* normalized coherent power */
#define HSK_PD_VEL  0x0208	/* velocity in m/s */
#define HSK_PD_WIDTH 0x0308	/* width in m/s */
#define HSK_PD_DBZ 0x0108	/* reflectivity in dBZ */
#define HSK_PD_SNR 0x0408	/* SNR */
