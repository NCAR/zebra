/*HI***************************************************************************
 *
 * raw.h - volume scan structure definition header file
 *
 ******************************************************************************
 *
 * HEADER INFORMATION
 *
 *	Software Suite		- RADEX
 *	Package			- Global
 *
 *	Reference number	- SP1/HDR/02000043
 *	
 *	Revision number		- $Revision: 2.3 $
 *	Release state		- $State: Exp $
 *
 *	Author, designer	- Ian McAfee
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
 *	Defines the volume structure into which raw data is accumulated
 *	by RADEX. 
 *	
 *
 * MODIFICATION RECORD
 *
 * Revision 2.2  1999/03/01 02:04:39  burghart
 * It's time to take the plunge.  This is the merge of the large file database
 * changes made for the U. of Washington, specifically Branch_Seattle_980430.
 * See the logs of that branch for details.
 *
 * Revision 2.1  1993/08/18  15:34:54  burghart
 * Created new adrad_ingest program to deal with data from Texas A&M's ADRAD
 * (Aggie Doppler Radar).  The Rasterize() function in Rasterize.c was
 * changed slightly to allow for interleaved or non-interleaved data.  Changes
 * in radar_ingest.c are only to adapt to the new interface to Rasterize() and
 * also a new interface to IX_GetWFrame() (in /zeb/src/lib/ImageXfr.c).
 *
 * Revision 8.1  92/04/27  10:31:36  stafford
 * Version used during FAT
 * 
 * Revision 1.4  92/04/09  14:00:43  amca
 * Added module reference number
 * 
 * Revision 1.3  92/04/09  12:02:06  amca
 * After level three documentation header added and after a full merge of
 * the include directory with the latest code from Cowes. 
 * Full recompilation of RADEX made to ensure integrity of changes
 * 
 * $Source: /code/cvs/rdss/zebra/source/src/ingest/radar/raw.h,v $
 *
 ******************************************************************************/


/**static char rcs_id[] = "$Id: raw.h,v 2.3 2008-04-16 18:26:54 granger Exp $";*/

#ifndef SUNRISE_RAW
#define SUNRISE_RAW
#include "radinfo.h"
#include "radar.h"

/* version number							*/
#define RAW_VERSION 13

#define NO_HEAD 99

/* for vol->filled							*/
#define VOL_EMPTY -1
#define VOL_FILLING 0
#define VOL_FULL 1

/* constants								*/
#define MAX_ELV 30
#define MAX_BINS 1024

/* offset numbers							*/
#define NUMOFFSETS 5
#define OFF_UZ	0
#define OFF_CZ	1
#define OFF_VEL	2
#define OFF_WID 3
#define OFF_ZDR 4
#define NUMSPARES (10-NUMOFFSETS)

/* for sweep_type variable						*/
#define VOL_PPI SWEEP_PPI	
#define VOL_RHI SWEEP_RHI	
#define VOL_SEC SWEEP_SEC 

/* locations								*/
#define VOL_ON_SUN	0xa5a5
#define VOL_ON_FORCE	0x5c5c

/*
 * Ray Header Structure:
 *
 * This structure contains information regarding a specific ray.  The
 * sweep_index normally contains 360 pointers to these headers.  The
 * actual data for the ray is not contained within the structure, but
 * resides in memory after the end of the structure.  The moment offsets
 * represent the physical location, in bytes, of a particular moment's
 * data, relative to the START of the ray header; i.e. if the intensity
 * moment's offset was 400, then the intensity data would start at the
 * 400th byte after the start of the ray header.  The moment data may be
 * in any order, and has a length in bytes equal to numgates.  An offset
 * of zero for a moment represents a lack of data for that moment.
 *
 * As noted before, the moments may be in any order, but this order must
 * be followed uniformly though each sweep.  This is extremely important.
 */
struct ray_header
{
	unsigned short	vangle,		/* variable angle 		*/
			fanglet,	/* target fixed angle		*/
			fanglea;	/* actual fixed angle		*/

	unsigned short	a_start,	/* variable angle start		*/
			a_stop,		/* variable angle stop		*/

			status;		/* hardware status word		*/

	unsigned char	max_height,	/* maximum height, km		*/
			volume,		/* volume serial number		*/
			sweep,		/* sweep index 1 -> SMAX	*/
			sweep_type;	/* sweep type code		*/

	unsigned short	gatewid,	/* gate width, meters		*/
			rangeg1,	/* range to gate 1, meters	*/
			numgates;	/* number of gates		*/

	unsigned short	prf,		/* primary prf, hz		*/
			prflow,		/* secondary prf, hz		*/
			n_pulses;	/* sample size in pulses	*/

	unsigned char	p_width,	/* pulse width, .05 us units	*/
			cfilter,	/* clutter filter code		*/
			filler1[2];


	struct				/* software flags		*/
	{
		unsigned packed 	: 1; /* is the data packed?	*/
		unsigned good_data 	: 1; /* is the data good?	*/
		unsigned uz  		: 1; /* uz required		*/
		unsigned cz  		: 1; /* cz required		*/
		unsigned vel 		: 1; /* vel required	*/
		unsigned wid 		: 1; /* wid required	*/
		unsigned zdr 		: 1; /* zdr required	*/
		unsigned time 		: 1; /* time series required*/
#ifdef CXREF
		unsigned spares		: 8;
#else
		unsigned spares		: 24;
#endif
	} flags;


	/*
	 * The offsets are in an array in order to make
	 * products much easier to write.  i.e. instead of
	 * using off_uz to get the uncorrected dbZ and
	 * off_wid to get the spectral width, you can
	 * use offset[OFF_UZ] and offset[OFF_WID]
	 * respectively.  This gains the flexibility to
	 * be able to use the same code with different
	 * array offsets to do such things as ppi's without
	 * code for each moment.
	 */
	unsigned short	offset[NUMOFFSETS],
			spares[NUMSPARES];	/* spare 16-bit int's	*/

	unsigned char	year,		/* last two digits of year	*/
			month,		/* month 1-12			*/
			day,		/* day 1-31			*/
			hour,		/* hour 0-23			*/
			minute,		/* minute 0-59			*/
			second,		/* second 0-59			*/
			filler2[2];
};

/*
 * Sweep Index Structure:
 *
 * This structure contains information for an entire sweep (beam) of up to
 * 360 rays.  The volume_summary structure may have up to 30 pointers to
 * sweep_index's.  Each sweep_index has up to 360 pointers to ray_headers.  
 * The number of rays in a given sweep is stored in numrays.
 *
 * The offsets for moment data are duplicated in this structure so that
 * a program which needs this information does not have to go down to
 * the ray level to get it.  The information is copied from the ray_header
 * at the array position 0.  (which will always have data if there is any
 * good data at all in the structure)
 */
struct sweep_index
{
	unsigned short	volume,		/* volume serial number		*/
			sweep,		/* sweep index 1 -> SMAX	*/
			sweep_type,	/* sweep type code		*/
			max_height;	/* maximum height, km		*/


	unsigned short	fangle,		/* fixed angle			*/
			min_var,	/* 'leftmost' variable angle	*/
			max_var,	/* 'rightmost' variable angle	*/
			a_start,	/* variable angle start		*/
			a_stop;		/* variable angle stop		*/

	unsigned short	gatewid,	/* gate width, meters		*/
			rangeg1,	/* range to gate 1, meters	*/
			numgates,	/* number of gates		*/
			numrays;	/* number of rays this sweep	*/

	unsigned short	prf,		/* primary prf, hz		*/
			prflow,		/* secondary prf, hz		*/
			n_pulses,	/* sample size in pulses	*/
			p_width,	/* pulse width, .05 us units	*/
			cfilter;	/* clutter filter code		*/

	unsigned short	offset[NUMOFFSETS],
			spares[NUMSPARES];	/* spare 16-bit int's	*/

	unsigned char	year,		/* last two digits of year	*/
			month,		/* month 1-12			*/
			day,		/* day 1-31			*/
			shour,		/* start hour 0-23		*/
			sminute,	/* start minute 0-59		*/
			ssecond,	/* start second 0-59		*/
			ehour,		/* end hour 0-23		*/
			eminute,	/* end minute 0-59		*/
			esecond,	/* end second 0-59		*/
			spareb[3];	/* spare			*/

	unsigned short	status,		/* status word			*/
			filler;

	struct ray_header *ray[360];	/* pointers to data ray headers	*/
};

/*
 * Volume Summary Structure:
 *
 * This is the structure which contains information for an entire volume.
 * A volume may contain up to 30 sweeps.  The number of sweeps present
 * in a volume is stored in numsweeps.  The pointers to sweep_index's
 * are always stored from array position 0 to array position n.
 *
 * The offsets for moment data are duplicated in this structure so that
 * a program which needs this information does not have to go down to
 * the ray level to get it.  This information is copied for each ray
 * from the sweep_index structure.
 */

struct volume_summary
{
	unsigned short	version;	/* raw version number		*/
	short		filled;		/* <0=empty 0=filling >0=full	*/

	unsigned int	volume;		/* volume serial number		*/
	unsigned short	sweep,		/* sweep index 1 -> SMAX	*/
			sweep_type,	/* sweep type code		*/
			max_height;	/* maximum height, km		*/

	unsigned short	status;		/* status word			*/

	unsigned short	min_fangle,	/* minimum fixed angle		*/
			max_fangle,	/* maximum fixed angle		*/
			min_var,	/* minimum variable angle	*/
			max_var,	/* maximum variable angle	*/
			a_start,	/* variable angle start		*/
			a_stop,		/* variable angle stop		*/
			numsweeps,	/* number of sweeps in volume	*/

			fangles[30];	/* fixed angles for each sweep	*/

	unsigned short	gatewid,	/* gate width, meters		*/
			rangeg1,	/* range to gate 1, meters	*/

			numgates[30],	/* gates for each sweep		*/
			maxgates;	/* max # of gates in volume	*/

	unsigned short	prf,		/* primary prf, hz		*/
			prflow,		/* secondary prf, hz		*/
			n_pulses,	/* sample size in pulses	*/
			p_width,	/* pulse width, .05 us units	*/
			cfilter,	/* clutter filter code		*/
			local;		/* used as volume lock: nonzero,*/
					/* radex will not auto-delete   */

	unsigned int	freq;		/* Mhz * 10			*/

	unsigned short	offset[30][NUMOFFSETS],
			spares[30][NUMSPARES];	/* spare 16-bit int's	*/

	unsigned char	year,		/* last two digits of year	*/
			month,		/* month 1-12			*/
			day,		/* day 1-31			*/
			shour,		/* start hour 0-23		*/
			sminute,	/* start minute 0-59		*/
			ssecond,	/* start second 0-59		*/
			ehour,		/* end hour 0-23		*/
			eminute,	/* end minute 0-59		*/
			esecond,	/* end second 0-59		*/
			spareb[3];

	struct 				/* software status flags	*/
	{
		unsigned compress : 1;
#ifdef CXREF
		unsigned spares : 15;
#else
		unsigned spares : 31;
#endif
	} volflags;

	struct radar_info  radinfo;	/* radar parameter information	*/

			/* 120 bytes */
	struct sweep_index *index[30];	/* pointers to sweep indx strcts*/
};

/* returns a bin value given a volume header, a sweep number, a ray
   number, a bin number, and a moment number				*/
#define VOL_BIN_MMT(v,e,r,b,m)	*((unsigned char *)v.index[e]->ray[r]+v.offset[e][m]+b)
/* same thing except with a volume pointer				*/
#define VPTR_BIN_MMT(v,e,r,b,m)	*((unsigned char *)v->index[e]->ray[r]+v->offset[e][m]+b)

/* circular_inc and circular_dec routines				*/
#ifndef circular_inc
#define circular_inc(ang) ang<359?ang+1:0
#define circular_dec(ang) ang>0?ang-1:359
#endif
#endif


/*MODULE END******************************************************************/
