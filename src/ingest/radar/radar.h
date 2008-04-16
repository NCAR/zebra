/*HI***************************************************************************
 *
 * 	radar.h -- generic processor control header
 *
 ******************************************************************************
 *
 * HEADER INFORMATION
 *
 *	Software Suite		- RADEX
 *	Package			- Global
 *
 *	Reference number	- SP1/HDR/02000041
 *	
 *	Revision number		- $Revision: 2.3 $
 *	Release state		- $State: Exp $
 *
 *	Author, designer	- B. Smith
 *
 *	Modification date	- $Date: 2008-04-16 18:26:54 $
 *	Modified by		- $Author: granger $
 * 
 * COPYRIGHT NOTICE
 *
 * 	Copyright (c) 1991 by Lassen Research
 *	All Rights Reserved
 *
 *	This program is copyright by Lassen Research, Chico, California,
 *	95928, (916) 343-6421.  It is licensed for use on a specific cpu
 *	and is not to be transferred or otherwise divulged.  Copies or
 *	modifications of this program must carry this copyright notice.
 * 
 * DESCRIPTION
 *
 *	Defines the psp32 processor control and storm structures along with
 *	the radar status structure.
 *
 * MODIFICATION RECORD
 *
 * Revision 2.2  1999/03/01 02:04:38  burghart
 * It's time to take the plunge.  This is the merge of the large file database
 * changes made for the U. of Washington, specifically Branch_Seattle_980430.
 * See the logs of that branch for details.
 *
 * Revision 2.1  1993/08/18  15:34:44  burghart
 * Created new adrad_ingest program to deal with data from Texas A&M's ADRAD
 * (Aggie Doppler Radar).  The Rasterize() function in Rasterize.c was
 * changed slightly to allow for interleaved or non-interleaved data.  Changes
 * in radar_ingest.c are only to adapt to the new interface to Rasterize() and
 * also a new interface to IX_GetWFrame() (in /zeb/src/lib/ImageXfr.c).
 *
 * Revision 8.1  92/04/27  10:31:34  stafford
 * Version used during FAT
 * 
 * Revision 1.4  92/04/09  14:00:38  amca
 * Added module reference number
 * 
 * Revision 1.3  92/04/09  12:02:02  amca
 * After level three documentation header added and after a full merge of
 * the include directory with the latest code from Cowes. 
 * Full recompilation of RADEX made to ensure integrity of changes
 * 
 * Revision 1.2  92/04/03  14:25:32  kenb
 * After merging code from Chico
 *
 * $Source: /code/cvs/rdss/zebra/source/src/ingest/radar/radar.h,v $
 *
 ******************************************************************************/

#ifndef _PROCCONT_
#define _PROCCONT_

/* defines for sweep_type						*/
#define SWEEP_POINT 	0	/* point mode				*/
#define SWEEP_PPI	1	/* ppi mode				*/
#define SWEEP_RHI	2	/* rhi mode				*/
#define SWEEP_SEC	3	/* sector mode				*/

struct processor_control
{
	unsigned short	
		sweep_type,	/* see the #defines above		*/
		a_start,	/* variable start angle -- left
				   justified binary angle in degrees	*/
		a_stop,		/* variable stop angle -- left justified
				   binary angle in degrees		*/
		a_rate,		/* variable angle rate -- (deg/sec * 10)*/
		max_height,	/* maximum height -- Km			*/
		n_steps,	/* number of fixed angle steps-- 1-30	*/
		steps[30],	/* fixed angles 0-n_steps-1 : for RHI	
				   it contains the azimuth angles -- left
				   justified binary angle in degrees	*/
		cur_step,
		spare[3];	/* for future use			*/
		struct
		{
			unsigned noise		: 1; /* noise sample	*/
			unsigned proc_mode	: 1; /* 0-free 1-sync	*/
			unsigned rad 		: 1; /* xmitter on=1	*/
			unsigned direction	: 1; /* 0=CW, 1=CCW	*/
			unsigned nfts_enable	: 1; /* PSP-32		*/
			unsigned spares 	: 27;
		} flags;
};

union status_layout
{
	struct
	{
		unsigned int 		: 13;
		unsigned int ups_status : 1;	/* ups status
						   0-running on regular
						   power,
						   1-running on ups	*/
		unsigned int eht_status : 1;	/* magnetron status
						   0 - not radiating
						   1 - radiating	*/
		unsigned int afc_status : 1;	/* automatic frequency
						   control
						   0 - STALO unlocked
						   1 - STALO locked	*/
	} bits;
	unsigned short stat_word;
	int dummy; /* Force to 32 bits */
};
struct processor_status
{
	unsigned short	
		cur_el,		/* current elevation of radar -- left
				   justified binary angle in degrees	*/
		cur_az;		/* current azimuth of radar -- left
				   justified binary angle in degrees	*/
	union 	status_layout stat;
};

struct storm
{
	unsigned short
		prf1,		/* primary prf in Hz * 10		*/
		prf2,		/* secondary prf in Hz * 10		*/
		gatewid,	/* gatewidth in meters			*/
		numgates,	/* number of gates			*/
		p_width,	/* pulse width -- usec * 20		*/
		rangeg1,	/* range to gate 1 in meters		*/
		max_rn,		/* maximum range normalization - km	*/
		pulse,		/* transmitter pulse selection (0-3)	*/
		n_pulses,	/* number of pulses integrated (>=1)	*/
		cfilter,	/* clutter filter id# (0:none - 15:max)	*/
		cfil_rng,	/* max range for selected filter - km	*/
		sqi_th,		/* sqi threshold 0-99 (0:none)		*/
		ccor_th,	/* clutter correction threshold		*/
		log_th,		/* UNUSED -- look at log_pth		*/
		agc_dec,	/* agc decay constant			*/
		log_pth,	/* log power thres. for wid in 1/16 dB 	*/
		slope[4],	/* log channel slope in 1000 dB/bit	*/
		cal[4];		/* calibration constant for each
					possible pulse width		*/
		struct
		{
			unsigned	uz  : 1; /* uz required		*/
			unsigned	cz  : 1; /* cz required		*/
			unsigned	vel : 1; /* vel required	*/
			unsigned	wid : 1; /* wid required	*/
			unsigned	zdr : 1; /* zdr required	*/
			unsigned       time : 1; /* time series required*/
			unsigned       fz   : 1; /* float Z required    */
			unsigned     spares : 25;
		} mmt_enables;

		struct
		{
			unsigned z_spec  : 1;	/* remove Z speckle	*/
			unsigned d_spec  : 1;	/* remove doppler spckl	*/
			unsigned agc_stc : 1;	/* 1 if agc, 0 if stc	*/
			unsigned sqi_th  : 1;	/* sqi test enabled	*/
			unsigned clt_th  : 1;	/* cltr-correction test */
			unsigned r_norm  : 1;	/* range normalization	*/
			unsigned rf_stc  : 1;	/* rf stc turned on	*/
			unsigned if_stc  : 1;	/* if agc/stc turned on	*/
			unsigned spares  : 24;	
		} thres_enables;
};

#endif _PROCCONT_

/*MODULE END*******************************************************************/
