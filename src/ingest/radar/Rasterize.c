/*
 * Rasterize incoming radar data.
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

static char *rcsid = "$Id: Rasterize.c,v 2.3 1992-07-31 16:54:31 corbet Exp $";

# include <defs.h>
# include <message.h>
# include <sys/time.h>
# include <math.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"


static char *Modes[] = { "CAL", "PPI", "COP", "RHI", "??4", "??5", "??6",
		"??7", "SUR" };

typedef enum { Unknown, Clockwise, CounterClockwise } Direction;

# define DegToRad(theta) ((theta)*M_PI/180.0)
# ifndef TRUE
# define ABS(v) ((v) < 0 ? (-(v)) : (v))
# define TRUE 1
# define FALSE 0
# endif

# define ELTOLERANCE	CORR_FACT	/* One degree	*/

/*
 * Rasterization info for each chunk of data.
 */
struct RastInfo
{
	int	ri_major1, ri_major2;		/* Major endpoints	*/
	double	ri_minor1, ri_minor2;		/* Minor at beginning	*/
	double	ri_gate;			/* Gate at beginning	*/
};

/*
 * Intermediate storage for thresholded data.
 */
# define MAXGATES 4096
static unsigned char ThreshBuf[4*MAXGATES];
static unsigned char *TBuf[4] = { ThreshBuf, ThreshBuf + MAXGATES,
	ThreshBuf + 2*MAXGATES, ThreshBuf + 3*MAXGATES };

/*
 * We keep track of the maximum extents in every direction, so that we
 * don't save more of the image than we have to.
 */
static int MaxLeft, MaxRight, MaxUp, MaxDown;

static Direction Dir = Unknown;
/*
 * Are we currently in a sweep?
 */
static int InSweep = FALSE;
static int NBeam = 0;		/* Number of beams in this sweep	*/


/*
 * Local routines.
 */
static void CalcParams FP ((Beam, int *, double *, double *, double *,
	double *, struct RastInfo *));
static float FindIntersection FP ((double, double, double));
static void GetHIncrements FP ((double, double, double, double, double,
	double, double *, double *, double *, double *));
static void GetVIncrements FP ((double, double, double, double, double,
	double, double *, double *, double *, double *));
static void GetEndpoints FP ((Beam, int, double, int, double, double,
	double, double, double, double, struct RastInfo *, int));
static void PFillR FP ((struct RastInfo *, double, double, double,
	double, unsigned char *, unsigned char *));
static void PFillC FP ((struct RastInfo *, double, double, double,
	double, unsigned char *, unsigned char *));
static void PFill2R FP ((struct RastInfo *, double, double, double,
	double, unsigned char *, unsigned char *, unsigned char *,
	unsigned char *));
static void PFill2C FP ((struct RastInfo *, double, double, double,
	double, unsigned char *, unsigned char *, unsigned char *,
	unsigned char *));
static void Threshold FP ((Beam, unsigned char *, int, int));
static void CheckSweep FP ((Beam));
static void ScanConvert FP ((struct RastInfo *, int, double, double,
	double, double, RDest *, int));
static inline void CheckMax FP ((int, int, int, int));
static int DirCheck FP ((Direction *, Beam, int, double));

/*
 * This is used for debugging only.
 */
static unsigned char FakeBeam[512];


static /* inline */ void
DSwap (d1, d2)
double *d1, *d2;
/*
 * Swap two doubles.
 */
{
	double tmp;

	tmp = *d1;
	*d1 = *d2;
	*d2 = tmp;
}



static inline double
FYInvert (y)
double y;
{
	return (YRes - y - 1);
}


static inline int
YInvert (y)
int y;
{
	return (YRes - y - 1);
}



static inline double
abs (a)
double a;
{
	return (a > 0 ? a : -a);
}



/*
 * Routines for maintaining max values.
 */
static inline void
CheckMax (xmin, xmax, ymin, ymax)
int xmin, xmax, ymin, ymax;
{
	if (xmin < MaxLeft)
		MaxLeft = xmin;
	if (xmax > MaxRight)
		MaxRight = xmax;
	if (ymax > MaxDown)
		MaxDown = ymax;
	if (ymin < MaxUp)
		MaxUp = ymin;
}




# ifdef notdef

InitFake ()
{
	unsigned int i;

	for (i = 0; i < 512; i++)
		FakeBeam[i] = i/2;
	FakeBeam[0] = 128;
	FakeBeam[511] = FakeBeam[128] = FakeBeam[127] = FakeBeam[193] = 1;
	FakeBeam[400] = FakeBeam[401] = 0;
	FakeBeam[510] = FakeBeam[511] = 128;
}


# endif


void
Rasterize (beam, rd, nrd)
Beam beam;
RDest *rd;
int nrd;
/*
 * Actually rasterize this beam.
 */
{
	Housekeeping *hk = beam->b_hk;
	int chunk, fld;
	double inc1, inc2, rowinc, colinc;
	int vertical;
	static struct RastInfo rinfo[MAXGD];
	float lastaz;
	static float slastaz;
/*
 * Filter out obvious junk.
 */
	if (! InSweep && hk->scan_mode != SM_PPI && hk->scan_mode != SM_SUR)
		return;
	if (hk->minute > 60 || hk->gates_per_beam > 2048 || hk->month > 12)
	{
		msg_ELog (EF_INFO, "Dropped funky beam");
		return;
	}
	lastaz = slastaz = hk->azimuth/CORR_FACT;
/*
 * See if we're at the end of a sweep.
 */
	CheckSweep (beam);
	if (! InSweep)
		return;
	NBeam++;
/*
 * Threshold the data.
 */
	for (fld = 0; fld < nrd; fld++)
		Threshold (beam, TBuf[fld], rd[fld].rd_foffset,
			hk->parm_per_gate);
/*
 * Calculate the rasterization parameters.
 */
	beam->b_npart = 1;	/* XXXXXX */
	beam->b_gdesc[0].gd_ngate = hk->gates_per_beam;
	CalcParams (beam, &vertical, &inc1, &inc2, &colinc, &rowinc, rinfo);
/*
 * Do the scan conversion.
 */
	for (chunk = 0; chunk < beam->b_npart; chunk++)
	{
		GDesc *gd = beam->b_gdesc + chunk;
		ScanConvert (rinfo + chunk, vertical, colinc, rowinc,
			inc1, inc2, rd, nrd);
	}

# ifdef notdef
	ui_printf ("Beam %d, %d/%d/%d %d:%d:%d, az %6.2f el %5.2f, fixed %6.2f mode %s swp %d/%d\n",
		hk->log_rec_num, hk->year, hk->month, hk->day, hk->hour,
		hk->minute, hk->second, hk->azimuth/CORR_FACT,
		hk->elevation/CORR_FACT, hk->fixed/CORR_FACT,
		Modes[hk->scan_mode], hk->seq_sweep, hk->sweep_index);
# endif
}




static void
Threshold (beam, dest, offset, skip)
Beam beam;
register unsigned char *dest;
const int offset, skip;
/*
 * Threshold this data.
 */
{
	register int chunk, gate;
	const bool dothresh = DoThresholding;
	unsigned char tvalue = ThrCounts;

	for (chunk = 0; chunk < beam->b_npart; chunk++)
	{
		const int ngate = beam->b_gdesc[chunk].gd_ngate;
		unsigned char *data = beam->b_gdesc[chunk].gd_data + offset;
	/*
	 * Do the thresholding check here, instead of at every gate.
	 */
	 	if (dothresh)
		{
			register unsigned char *thresh = data - offset +
							  ThrFldOffset;
			for (gate = 0; gate < ngate; gate++)
			{
				*dest++ = (*thresh >= ThrCounts) ? *data : 0xFF;
				data += skip;
				thresh += skip;
			}
		}
		else
			for (gate = 0; gate < ngate; gate++)
			{
				/* *dest++ = CMap[*data]; */
				*dest ++ = *data;
				data += skip;
			}
	}
}






static void
ScanConvert (rinfo, vertical, colinc, rowinc, inc1, inc2, rd, nrd)
struct RastInfo *rinfo;
double colinc, rowinc, inc1, inc2;
RDest *rd;
int vertical, nrd;
/*
 * Now that all the parameters are in place, actually do the scan conversion.
 */
{
/*
 * Split out based on the number of fields we are doing -- this is done to
 * facilitate loop unrolling below.
 */
	switch (nrd)
	{
	   case 1:
		if (vertical)
			PFillR (rinfo, colinc, rowinc, inc1, inc2,
				TBuf[0], rd->rd_image);
		else
			PFillC (rinfo, colinc, rowinc, inc1, inc2,
				TBuf[0], rd->rd_image);
		break;

	   case 2:
		if (vertical)
			PFill2R (rinfo, colinc, rowinc, inc1, inc2,
				TBuf[0], rd->rd_image,
				TBuf[1], rd[1].rd_image);
		else
			PFill2C (rinfo, colinc, rowinc, inc1, inc2,
				TBuf[0], rd->rd_image,
				TBuf[1], rd[1].rd_image);
		break;
	}
}






static void
CheckSweep (beam)
Beam beam;
/*
 * Check sweep parameters, and output an image if necessary.
 */
{
	Housekeeping *hk = beam->b_hk;
	static int mode = -1, fixed = -1, scan = 0, lastfixed = -999;
	static int firstbeam, firstaz, firstel, gs, ng;
	static int sweep, vol;
	static time begintime;
	static struct timeval oldtime, newtime;
	static Direction dir = Unknown;
/*
 * See if we have entered a new sweep.
 */
	if (! InSweep)
		;	/* New sweep by definition */
	else if (mode != hk->scan_mode)		/* Scan mode change */
		msg_ELog (EF_DEBUG, "Mode change break");
	else if (TrustSweep && (hk->sweep_index != sweep || hk->transit))
		msg_ELog (EF_DEBUG, "Sweep count/transition break");
	else if (fixed != hk->fixed)		/* Fixed angle change */
		msg_ELog (EF_DEBUG, "Fixed angle break");
	else if (ABS (hk->fixed - hk->elevation) > ELTOLERANCE)
		msg_ELog (EF_DEBUG, "Fixed/real difference break");
	else if (DirCheck (&dir, beam, hk->log_rec_num - firstbeam,
			firstaz/CORR_FACT))
		msg_ELog (EF_DEBUG, "DirCheck break");
	else if (gs != hk->gate_spacing)
		msg_ELog (EF_DEBUG, "Gate spacing change break");
	else if (ng != hk->gates_per_beam)
		msg_ELog (EF_DEBUG, "Gates per beam change break");
	else
		return;	/* Business as usual */

# ifdef notdef	
	if (! InSweep || mode != hk->scan_mode || fixed != hk->fixed ||
		ABS (hk->fixed - hk->elevation) > ELTOLERANCE ||
		DirCheck (&dir, beam, hk->log_rec_num - firstbeam,
				firstaz/CORR_FACT) ||
		gs != hk->gate_spacing || ng != hk->gates_per_beam)
# endif
/*
 * If we were currently rasterizing an old sweep, shove it out.
 */
	gettimeofday (&newtime, 0);
	if (InSweep)
	{
		bool newvol;
# ifdef notdef
		float tdiff = newtime.tv_sec - oldtime.tv_sec;
		tdiff += (newtime.tv_usec - oldtime.tv_usec)/1000000.0;
		ui_printf ("%d, v %d ll %6.2f %6.2f F %6.2f m %s ",
			scan, hk->vol_count, hk->latitude/LAT_CF,
			hk->longitude/LON_CF,
			fixed/CORR_FACT, Modes[mode]);
		ui_printf ("%d b, %.2f s %.1f b/s\n", NBeam, tdiff,
				NBeam/tdiff);
# endif
	/*
	 * See if a new volume is warranted.
	 */
		newvol = (mode != hk->scan_mode) ||
			(TrustVol ? hk->vol_count != vol : fixed <= lastfixed);
# ifdef notdef
		msg_ELog (EF_INFO, "MHR ck fix %.2f last %.2f top %.2f",
			fixed/CORR_FACT, lastfixed/CORR_FACT, MhrTop);
		if (MhrMode && fixed/CORR_FACT < 0.7 &&
					lastfixed/CORR_FACT < MhrTop)
			msg_ELog (EF_DEBUG, "Dropping .5 sweep, last %.2f",
				lastfixed);
		else
# endif
			if (NBeam > MinSweep)
		{
			OutputSweep (&begintime, fixed/CORR_FACT,
			   newvol, MaxLeft, MaxRight, MaxUp, MaxDown,
			   hk->scan_mode);
			lastfixed = fixed;
			dir = Unknown;
		}
	}
/*
 * If this is not a type of sweep we deal with, bail now.
 * 7/18/91 jc (CaPE fix): Throw out anything with a zero elevation,
 *	in a simple attempt to filter out obnoxious between-sweep
 *	behavior.
 */
	if (hk->scan_mode != SM_SUR && hk->scan_mode != SM_PPI ||
			ABS (hk->elevation - hk->fixed) > ELTOLERANCE
			|| hk->elevation <= (int) (0.1*CORR_FACT))
	{
		InSweep = FALSE;
		return;
	}
	NBeam = 0;
/*
 * Remember the info about this sweep, and get started.
 */
	if (! BeginSweep ())
	{
		InSweep = FALSE;
		return;
	}
	InSweep = TRUE;
	scan++;
	oldtime = newtime;
	firstbeam = hk->log_rec_num;
	fixed = hk->fixed;
	mode = hk->scan_mode;
	firstaz = hk->azimuth;
	firstel = hk->elevation;
	begintime.ds_yymmdd = hk->year*10000 + hk->month*100 + hk->day;
	begintime.ds_hhmmss = hk->hour*10000 + hk->minute*100 + hk->second;
	vol = hk->vol_count;
	sweep = hk->sweep_index;
/*
 * Come up with a new pixel scaling.
 */
	ng = hk->gates_per_beam;
	gs = hk->gate_spacing;
	PixScale = 1000*XRes/(2.0*gs*ng);
/*
 * Reset our max parameters.
 */
	MaxLeft = MaxRight = XRadar;
	MaxUp = MaxDown = YRadar;
}




static int
DirCheck (dir, beam, nbeam, first)
Direction *dir;
Beam beam;
int nbeam;
float first;
/*
 * Check directions.
 */
{
	float diff, az = beam->b_hk->azimuth/CORR_FACT;
	int retv = FALSE;
/*
 * Check for non-moving antenna.
 */
	if (nbeam == 4 && ABS (first - az) < 0.1)
	{
		msg_ELog (EF_DEBUG, "DirCheck 4 beam trigger");
		return (TRUE);
	}
/*
 * Find the difference here.
 */
   	diff = az - first;
	if (diff < -180)
		diff += 360;
	else if (diff > 180)
		diff -= 360;
/*
 * Now figure out what to do.
 */
	switch (*dir)
	{
	   case Unknown:
		*dir = (diff > 0) ? Clockwise : CounterClockwise;
		return (FALSE);

	   case Clockwise:
	   	retv =  (nbeam > 100 && diff > 0 && diff < 10);
		break;

	   case CounterClockwise:
	   	retv =  (nbeam > 100 && diff < 0 && diff > -10);
		break;
	}
	if (retv)
		msg_ELog (EF_DEBUG,
		    "DIRCHECK, dir %d, nb %d, first %.2f, az %.2f, diff %.2f",
				*dir, nbeam, first, az, diff);
	return (retv);
}





static void
CalcParams (beam, vertical, inc1, inc2, colinc, rowinc, rinfo)
Beam beam;
int *vertical;
double *inc1, *inc2, *colinc, *rowinc;
struct RastInfo *rinfo;
/*
 * Figure out the rasterization params.
 */
{
	static float lastaz = -999;
	float az, az1, az2, ng, ngr, pr0, pgs, adiff;
	double sin1, cos1, sin2, cos2, sinc, cosc;
	Housekeeping *hk = beam->b_hk;
	int far = 0;
/*
 * Figure out azimuths.
 */
	az = hk->azimuth/CORR_FACT;
	if ((adiff = abs (az - lastaz)) > 1.5)
		adiff = AzFill;
	if ((az1 = az - adiff) < 0)
		az1 += 360.0;
	if ((az2 = az + adiff) >= 360.0)
		az2 -= 360.0;
/*
 * Figure out the orientation of the beam.  Then, if need be, swap the two
 * angles to correspond to the direction in which we will be rasterizing.
 */
	*vertical = (az <= 45.0 || (az > 135.0 && az < 225.0) || az > 315.0);
	if ((*vertical && az > 90.0 && az < 270.0) ||
	    (! *vertical && az > 180.0))
	{
		float tmp = az1;
		az1 = az2;
		az2 = tmp;
		/* DSwap (&az1, &az2); */
	}
# ifdef SDEBUG
	printf ("%6.2f [%6.2f--%6.2f] v:%c ", az, az1, az2,
		*vertical ? 'T' : 'F');
# endif
/*
 * Calculate some trig params we'll need.
 */
	sincos (DegToRad (az1), &sin1, &cos1);
	sincos (DegToRad (az2), &sin2, &cos2);
/*
 * Find the edge which is furthest right or below the other.
 */
	if ((*vertical && az < 180.0) || (az > 90.0 && az < 270 && !*vertical))
	{
		sinc = sin2;
		cosc = cos2;
		far++;
	}
	else
	{
		sinc = sin1;
		cosc = cos1;
	}
/*
 * Get the gate spacing info, and turn it into gates/pixel.
 */
	pr0 = (hk->rhozero1 + hk->rhozero2/1000.0)*PixScale;
	pgs = 1.0/((hk->gate_spacing/1000.0)*PixScale);
	if (Project)
		pgs /= cos (DegToRad (hk->elevation/CORR_FACT));
	/* printf ("Gt %5.3f/%5.3f ", pr0, pgs); */
/*
 * Figure the number of gates until the shortest intersection with an edge
 * of the array.
 */
	ng = FindIntersection (az1, sin1, cos1);
	if ((ngr = FindIntersection (az2, sin2, cos2)) < ng)
		ng = ngr;
	if ((ng *= pgs) > hk->gates_per_beam) /* Deal with r0? */
		ng = hk->gates_per_beam;
# ifdef SDEBUG
	printf ("d %6.2f ", ng);
# endif
/*
 * Get the increments for rasterization.
 */
	if (*vertical)
		GetVIncrements (pgs, az, sin1, cos1, sin2, cos2, inc1,
			inc2, rowinc, colinc);
	else
		GetHIncrements (pgs, az, sin1, cos1, sin2, cos2, inc1,
			inc2, rowinc, colinc);
# ifdef SDEBUG
	printf ("I: %.2f %.2f, r %.2f c %.2f ", *inc1, *inc2, *rowinc,*colinc);
# endif
/*
 * Endpoints.
 */
	GetEndpoints (beam, *vertical, pgs, ng, *colinc, *rowinc, *inc1, *inc2,
		sinc, cosc, rinfo, far);
# ifdef SDEBUG
	printf ("\n");
# endif
}





static void
GetEndpoints (beam, vertical, pgs, ng, colinc, rowinc, inc1, inc2, sinaz,
	cosaz, rinfo, far)
Beam beam;
int vertical, ng;
double pgs, inc1, inc2, sinaz, cosaz, rowinc, colinc;
struct RastInfo *rinfo;
int far;
/*
 * Find the end points for each chunk of data.
 */
{
	int chunk, bext1, bext2;
	GDesc *gd = beam->b_gdesc;

	for (chunk = 0; chunk < beam->b_npart; chunk++)
	{
		int maxg = (gd->gd_ngate + gd->gd_first - 1) > ng ?
					ng - gd->gd_first : gd->gd_ngate;
	/*
	 * For vertical beams, the major coordinates are in the Y direction.
	 * At this point, we also invert the coords to be right for the
	 * real array.
	 */
		if (vertical)
		{
		/*
		 * Calculate the end points, based on the number of gates
		 * in this chunk.
		 */
			double y0 = YRadar + (gd->gd_first/pgs)*cosaz;
			double y1 = YRadar +
				((gd->gd_first + maxg - 1)/pgs)*cosaz;
			rinfo->ri_gate = gd->gd_first;
		/*
		 * Figure the width of the beam at the far (from radar) end,
		 * so that we can do max checking later.
		 */
		 	bext1 = XRadar + (int) ((YRadar - y1)*inc1 - 0.5);
			bext2 = XRadar + (int) ((YRadar - y1)*inc2 + 0.5);
		/*
		 * Swap them if need be.
		 */
			if (y0 < y1)
			{
				DSwap (&y0, &y1);
				rinfo->ri_gate += maxg - 1;
			}
		/*
		 * Store the info.
		 */
			rinfo->ri_major1 = YInvert ((int) y0);
			rinfo->ri_major2 = YInvert ((int) y1);
			rinfo->ri_minor1 = XRadar +
					(YRadar - y0)*inc1;
			rinfo->ri_minor2 = XRadar +
					(YRadar - y0)*inc2;
			CheckMax (bext1, bext2, rinfo->ri_major1,
				rinfo->ri_major2);
			if (far)
				rinfo->ri_gate -=
				  colinc*(rinfo->ri_minor2 - rinfo->ri_minor1);
		}
	/*
	 * Horizontal beam -- major coords are in the X direction.
	 */
		else
		{
		/*
		 * Calculate the end points.
		 */
			double x0 = XRadar + (gd->gd_first/pgs)*sinaz;
			double x1 = XRadar +
				((gd->gd_first + maxg - 1)/pgs)*sinaz;
			rinfo->ri_gate = gd->gd_first;
		/*
		 * Figure the width of the beam at the far (from radar) end,
		 * so that we can do max checking later.
		 */
		 	bext1 = YInvert ((int) ((XRadar - x1)*inc1 - 0.5) +
				YRadar);
			bext2 = YInvert ((int) ((XRadar - x1)*inc2 + 0.5) +
				YRadar);
		/*
		 * Swap them if necessary.
		 */
			if (x0 > x1)
			{
				DSwap (&x0, &x1);
				rinfo->ri_gate += maxg - 1;
			}
		/*
		 * Store everything.
		 */
			rinfo->ri_major1 = (int) x0;
			rinfo->ri_major2 = (int) x1;
			rinfo->ri_minor1 = FYInvert (YRadar +
					(XRadar - x0)*inc1);
			rinfo->ri_minor2 = FYInvert (YRadar +
					(XRadar - x0)*inc2);
			CheckMax (rinfo->ri_major1, rinfo->ri_major2,
				bext1, bext2);
			if (far)
				rinfo->ri_gate -=
				  rowinc*(rinfo->ri_minor2 - rinfo->ri_minor1);
		}
	/*
	 * Move on to the next chunk.
	 */
		gd++;
# ifdef SDEBUG
	ui_printf ("M: %3d->%3d, m: %.1f %.1f g %.1f \n", rinfo->ri_major1,
		rinfo->ri_major2, rinfo->ri_minor1, rinfo->ri_minor2,
		rinfo->ri_gate);
		rinfo++;
# endif
	}
}





static float
FindIntersection (az, sinaz, cosaz)
double az, sinaz, cosaz;
/*
 * Find the closest intersection of this radial with the edge of the box.
 */
{
	double distv, disth;
/*
 * Get the distance from the horizontal edge.  If this is an upward-pointing
 * beam, then to go the top edge.  Otherwise bottom.  Don't forget Y inversion!
 */
	if (ABS (cosaz) < 0.001)
		disth = 99999.9;	/* Horizontal beam	*/
	else if (az < 90.0 || az > 270.0)
		disth = (YRes - YRadar)/cosaz;
	else
		disth = -YRadar/cosaz;
/*
 * Now do a similar thing with the vertical edges.
 */	
	if (ABS (sinaz) < 0.001)
		distv = 99999.9;	/* Vertical beam	*/
	else if (az < 180.0)
		distv = (XRes - XRadar)/sinaz;
	else
		distv = (-XRadar)/sinaz;
/*
 * Return the smaller of the two.
 */
	return (disth < distv ? disth : distv);
}



static void
GetHIncrements (pgs, az, sin1, cos1, sin2, cos2, inc1, inc2, rowinc,
	colinc)
double pgs, az, sin1, cos1, sin2, cos2;
double *inc1, *inc2, *rowinc, *colinc;
/*
 * Calculate increments for horizontal beams.
 */
{
/*
 * The width increments are simply the cotangents of the 
 * relevant angles.
 */
	*inc1 = -cos1/sin1;
	*inc2 = -cos2/sin2;
/*
 * The per-row gate increment.
 */
	*rowinc = -pgs*cos (DegToRad (az));
# ifdef notdef
	*colinc = pgs/sin (DegToRad (az));
# endif
	*colinc = (cos1 > cos2) ? pgs/sin1 : pgs/sin2;
}






static void
GetVIncrements (pgs, az, sin1, cos1, sin2, cos2, inc1, inc2, rowinc,
	colinc)
double pgs, az, sin1, cos1, sin2, cos2;
double *inc1, *inc2, *rowinc, *colinc;
/*
 * Calculate increments for horizontal beams.
 */
{
/*
 * The width increments are simply the tangents of the 
 * relevant angles.
 */
	*inc1 = -sin1/cos1;
	*inc2 = -sin2/cos2;
/*
 * The per-row gate increment.
 */
# ifdef notdef
	*rowinc = -pgs/cos (DegToRad (az));
# endif
	*rowinc = (sin1 < sin2) ? -pgs/cos1 : -pgs/cos2;
	*colinc = pgs*sin (DegToRad (az));
}




/***********************************************************************
 *
 * The various "pfill" routines live below here.
 *
 ***********************************************************************/



/*
 * Inner rastorization loops.
 *
 * This is some of the more time-consuming code in the entire program, and
 * is thus somewhat twisted, as I have tried very hard to optimize things.
 * Essentially, what I have done is to eliminate entirely the use of
 * floating point arithmetic inside the plot loops by putting all of the
 * FP values into integer variables.  The multiplication by 256*256 puts
 * the integer part of the value into the upper half of the longword, making
 * it easily accessible through trickery with a short pointer.
 *
 * This code assumes 32-bit words, and big-endian architecture.  Small-
 * endian machines can be handled by incrementing all of the (short *)'s
 * by one.
 */


# define FakeFloat(v) ((int) ((v)*65536))


static void
PFillR (rinfo, colinc, rowinc, inc1, inc2, data, dest)
struct RastInfo *const rinfo;
const double colinc, rowinc, inc1, inc2;
unsigned char *data, *dest;
/*
 * Scan convert a single, vertical beam.
 */
{
	register int row, col;
	int gate = FakeFloat (rinfo->ri_gate - 1);
	int gate_col;	/* Can't be register, alas */
	register int col_inc = FakeFloat (colinc);
	int row_inc = FakeFloat (rowinc);
	int left = FakeFloat (rinfo->ri_minor1 + 1);
	int right = FakeFloat (rinfo->ri_minor2);
	int left_inc = FakeFloat (inc1);
	int right_inc = FakeFloat (inc2);
	register short *Pgate_col = (short *) &gate_col;
	register short *Pleft = (short *) &left, *Pright = (short *) &right;
	const int xr = XRes;
	unsigned char *dp = dest + ((int) rinfo->ri_major1)*xr;
/*
 * Move down the rows.
 */
	for (row = rinfo->ri_major1; row < rinfo->ri_major2; row++)
	{
	/*
	 * Set up raster and gate pointers.
	 */
		/* unsigned char *dp = dest + row*XRes; */
		gate_col = gate;
	/*
	 * Move through the columns, filling in the pixels.
	 */
		for (col = *Pleft; col <= *Pright; col++)
		{
			dp[col] = data[*Pgate_col];
			gate_col += col_inc;
		}
	/*
	 * Update the gate and beam width info.
	 */
		gate += row_inc;
		left += left_inc;
		right += right_inc;
		dp += xr;
	}
}




static void
PFillC (rinfo, colinc, rowinc, inc1, inc2, data, dest)
struct RastInfo *const rinfo;
const double colinc, rowinc, inc1, inc2;
unsigned char *data, *dest;
/*
 * Scan convert a horizontal, single field beam.
 */
{
	register int row, col;
	int gate = FakeFloat (rinfo->ri_gate - 1);
	int gate_row;
	register int row_inc = FakeFloat (rowinc);
	int col_inc = FakeFloat (colinc);
	int top = FakeFloat (rinfo->ri_minor1 + 1);
	int bottom = FakeFloat (rinfo->ri_minor2);
	int top_inc = FakeFloat (inc1);
	int bottom_inc = FakeFloat (inc2);
	register short *Pgate_row = (short *) &gate_row;
	register short *Ptop = (short *) &top, *Pbottom = (short *) &bottom;
	const int xr = XRes;
/*
 * Step through the columns of this beam.
 */
	for (col = rinfo->ri_major1; col < rinfo->ri_major2; col++)
	{
	/*
	 * Initialize raster line and gate pointers.
	 */
		unsigned char *dp = dest + *Ptop*xr + col;
		gate_row = gate;
	/*
	 * Go down the column, filling in the pixel values.
	 */
		for (row = *Ptop; row <= *Pbottom; row++)
		{
			*dp = data[*Pgate_row];
			dp += xr;	/* Go to next scan line */
			gate_row += row_inc;
		}
	/*
	 * Update the gate values and the width of the beam.
	 */
		gate += col_inc;
		top += top_inc;
		bottom += bottom_inc;
	}
}






static void
PFill2R (rinfo, colinc, rowinc, inc1, inc2, data, dest, data2, dest2)
struct RastInfo *const rinfo;
const double colinc, rowinc, inc1, inc2;
unsigned char *data, *dest, *data2, *dest2;
/*
 * Scan convert a single, vertical beam.
 */
{
	register int row, col;
	int gate = FakeFloat (rinfo->ri_gate - 1);
	int gate_col;	/* Can't be register, alas */
	register int col_inc = FakeFloat (colinc);
	int row_inc = FakeFloat (rowinc);
	int left = FakeFloat (rinfo->ri_minor1 + 1);
	int right = FakeFloat (rinfo->ri_minor2);
	int left_inc = FakeFloat (inc1);
	int right_inc = FakeFloat (inc2);
	register short *Pgate_col = (short *) &gate_col;
	register short *Pleft = (short *) &left, *Pright = (short *) &right;
	const int xr = XRes;
	unsigned char *dp = dest + ((int) rinfo->ri_major1)*xr;
	unsigned char *dp2 = dest2 + ((int) rinfo->ri_major1)*xr;
/*
 * Move down the rows.
 */
	for (row = rinfo->ri_major1; row < rinfo->ri_major2; row++)
	{
	/*
	 * Set up raster and gate pointers.
	 */
		/* unsigned char *dp = dest + row*XRes; */
		gate_col = gate;
	/*
	 * Move through the columns, filling in the pixels.
	 */
		for (col = *Pleft; col <= *Pright; col++)
		{
			int gateno = *Pgate_col;
			dp[col] = data[gateno];
			dp2[col] = data2[gateno];
			gate_col += col_inc;
		}
	/*
	 * Update the gate and beam width info.
	 */
		gate += row_inc;
		left += left_inc;
		right += right_inc;
		dp += xr;
		dp2 += xr;
	}
}




static void
PFill2C (rinfo, colinc, rowinc, inc1, inc2, data, dest, data2, dest2)
struct RastInfo *const rinfo;
const double colinc, rowinc, inc1, inc2;
unsigned char *data, *dest, *data2, *dest2;
/*
 * Scan convert a horizontal, single field beam.
 */
{
	register int row, col;
	int gate = FakeFloat (rinfo->ri_gate - 1);
	int gate_row;
	register int row_inc = FakeFloat (rowinc);
	int col_inc = FakeFloat (colinc);
	int top = FakeFloat (rinfo->ri_minor1 + 1);
	int bottom = FakeFloat (rinfo->ri_minor2);
	int top_inc = FakeFloat (inc1);
	int bottom_inc = FakeFloat (inc2);
	register short *Pgate_row = (short *) &gate_row;
	register short *Ptop = (short *) &top, *Pbottom = (short *) &bottom;
	const int xr = XRes;
/*
 * Step through the columns of this beam.
 */
	for (col = rinfo->ri_major1; col < rinfo->ri_major2; col++)
	{
	/*
	 * Initialize raster line and gate pointers.
	 */
		int offset = *Ptop * xr + col;
		unsigned char *dp = dest + offset;
		unsigned char *dp2 = dest2 + offset;
		gate_row = gate;
	/*
	 * Go down the column, filling in the pixel values.
	 */
		for (row = *Ptop; row <= *Pbottom; row++)
		{
			int gateno = *Pgate_row;
			*dp = data[gateno];
			dp += xr;	/* Go to next scan line */
			*dp2 = data2[gateno];
			dp2 += xr;	/* Go to next scan line */
			gate_row += row_inc;
		}
	/*
	 * Update the gate values and the width of the beam.
	 */
		gate += col_inc;
		top += top_inc;
		bottom += bottom_inc;
	}
}






# ifdef notdef



PFill3c_ (left, right, rad, cinc, linc, tand, tanu, y1, y2, beam, xmap, ymap,
	xo, yo, xo2, yo2, doff)
int *left, *right, *xmap, *ymap, *xo, *yo, *doff, *xo2, *yo2;
float *rad, *cinc, *linc, *tanu, *tand, *y1, *y2;
register unsigned char *beam;
{
	register int row, col;
	int Xrad = (int) ((*rad - 1.0)*256.0*256.0);
	int Xrad1;
	register int Xr1i = (int) (*linc * 256.0 * 256.0);
	int Xri = (int) (*cinc * 256.0 * 256.0);
	int Yl1 = (int) ((*y1 + 1) * 256.0 * 256.0);
	int Yl2 = (int) (*y2 * 256.0 * 256.0);
	int Yl1i = (int) (*tand * 256.0 * 256.0);
	int Yl2i = (int) (*tanu * 256.0 * 256.0);
	register short *Xr1p = (short *) &Xrad1;
	short *Yl1p = (short *) &Yl1, *Yl2p = (short *) &Yl2;
	int lxo = *xo, lyo = *yo, ldoff = *doff;
	int lxo2 = *xo2, lyo2 = *yo2;
	register unsigned char *dp = Pmap->pm_data;
	
	for (col = *left; col < *right; col++)
	{
		Xrad1 = Xrad;
		for (row = *Yl1p; row <= *Yl2p; row++)
		{
			dp[xmap[col] + ymap[row]] = beam[*Xr1p];
			dp[xmap[col+lxo] + ymap[row+lyo]] =
				beam[*Xr1p+ldoff];
			dp[xmap[col+lxo2] + ymap[row+lyo2]] =
				beam[*Xr1p + 2*ldoff];
			Xrad1 += Xr1i;
		}
		Xrad += Xri;
		Yl1 += Yl1i;
		Yl2 += Yl2i;
	}
	Pmap->pm_flags |= PF_ENTIRE;
}





PFill3r_ (low, high, rad, cinc, linc, cotl, cotr, x1, x2, beam, xmap, ymap,
  xo, yo, xo2, yo2, doff)
int *low, *high, *xmap, *ymap, *xo, *yo, *doff, *xo2, *yo2;
float *rad, *cinc, *linc, *cotl, *cotr, *x1, *x2;
register unsigned char *beam;
{
	register int row, col;
	int Xrad = (int) ((*rad - 1.0)*256.0*256.0);
	int Xrad1;	/* Can't be register, alas */
	register int Xr1i = (int) (*cinc * 256.0 * 256.0);
	int Xri = (int) (*linc * 256.0 * 256.0);
	int Xl1 = (int) ((*x1 + 1) * 256.0 * 256.0);
	int Xl2 = (int) (*x2 * 256.0 * 256.0);
	int Xl1i = (int) (*cotl * 256.0 * 256.0);
	int Xl2i = (int) (*cotr * 256.0 * 256.0);
	register short *Xr1p = (short *) &Xrad1;
	short *Xl1p = (short *) &Xl1, *Xl2p = (short *) &Xl2;
	int lxo = *xo, lyo = *yo, ldoff = *doff;
	int lxo2 = *xo2, lyo2 = *yo2;
	register unsigned char *dp = Pmap->pm_data;
	
	for (row = *low; row < *high; row++)
	{
		Xrad1 = Xrad;
		for (col = *Xl1p; col <= *Xl2p; col++)
		{
			dp[xmap[col] + ymap[row]] = beam[*Xr1p];
			dp[xmap[col+lxo]+ymap[row+lyo]] = beam[*Xr1p + ldoff];
			dp[xmap[col+lxo2]+ymap[row+lyo2]] =
				beam[*Xr1p + 2*ldoff];
			Xrad1 += Xr1i;
		}
		Xrad += Xri;
		Xl1 += Xl1i;
		Xl2 += Xl2i;
	}
	Pmap->pm_flags |= PF_ENTIRE;
}





PFill4c_ (left, right, rad, cinc, linc, tand, tanu, y1, y2, beam, xmap, ymap,
	xo, yo, xo2, yo2, xo3, yo3, doff)
int *left, *right, *xmap, *ymap, *xo, *yo, *doff, *xo2, *yo2, *xo3, *yo3;
float *rad, *cinc, *linc, *tanu, *tand, *y1, *y2;
register unsigned char *beam;
{
	register int row, col;
	int Xrad = (int) ((*rad - 1.0)*256.0*256.0);
	int Xrad1;
	register int Xr1i = (int) (*linc * 256.0 * 256.0);
	int Xri = (int) (*cinc * 256.0 * 256.0);
	int Yl1 = (int) ((*y1 + 1) * 256.0 * 256.0);
	int Yl2 = (int) (*y2 * 256.0 * 256.0);
	int Yl1i = (int) (*tand * 256.0 * 256.0);
	int Yl2i = (int) (*tanu * 256.0 * 256.0);
	register short *Xr1p = (short *) &Xrad1;
	short *Yl1p = (short *) &Yl1, *Yl2p = (short *) &Yl2;
	int lxo = *xo, lyo = *yo, ldoff = *doff;
	int lxo2 = *xo2, lyo2 = *yo2, lxo3 = *xo3, lyo3 = *yo3;
	register unsigned char *dp = Pmap->pm_data;
	
	for (col = *left; col < *right; col++)
	{
		Xrad1 = Xrad;
		for (row = *Yl1p; row <= *Yl2p; row++)
		{
			dp[xmap[col] + ymap[row]] = beam[*Xr1p];
			dp[xmap[col+lxo] + ymap[row+lyo]] =
				beam[*Xr1p+ldoff];
			dp[xmap[col+lxo2] + ymap[row+lyo2]] =
				beam[*Xr1p + 2*ldoff];
			dp[xmap[col+lxo3] + ymap[row+lyo3]] =
				beam[*Xr1p + 3*ldoff];
			Xrad1 += Xr1i;
		}
		Xrad += Xri;
		Yl1 += Yl1i;
		Yl2 += Yl2i;
	}
	Pmap->pm_flags |= PF_ENTIRE;
}





PFill4r_ (low, high, rad, cinc, linc, cotl, cotr, x1, x2, beam, xmap, ymap,
  xo, yo, xo2, yo2, xo3, yo3, doff)
int *low, *high, *xmap, *ymap, *xo, *yo, *doff, *xo2, *yo2, *xo3, *yo3;
float *rad, *cinc, *linc, *cotl, *cotr, *x1, *x2;
register unsigned char *beam;
{
	register int row, col;
	int Xrad = (int) ((*rad - 1.0)*256.0*256.0);
	int Xrad1;	/* Can't be register, alas */
	register int Xr1i = (int) (*cinc * 256.0 * 256.0);
	int Xri = (int) (*linc * 256.0 * 256.0);
	int Xl1 = (int) ((*x1 + 1) * 256.0 * 256.0);
	int Xl2 = (int) (*x2 * 256.0 * 256.0);
	int Xl1i = (int) (*cotl * 256.0 * 256.0);
	int Xl2i = (int) (*cotr * 256.0 * 256.0);
	register short *Xr1p = (short *) &Xrad1;
	short *Xl1p = (short *) &Xl1, *Xl2p = (short *) &Xl2;
	int lxo = *xo, lyo = *yo, ldoff = *doff;
	int lxo2 = *xo2, lyo2 = *yo2, lxo3 = *xo3, lyo3 = *yo3;
	register unsigned char *dp = Pmap->pm_data;
	
	for (row = *low; row < *high; row++)
	{
		Xrad1 = Xrad;
		for (col = *Xl1p; col <= *Xl2p; col++)
		{
			dp[xmap[col] + ymap[row]] = beam[*Xr1p];
			dp[xmap[col+lxo]+ymap[row+lyo]] = beam[*Xr1p + ldoff];
			dp[xmap[col+lxo2]+ymap[row+lyo2]] =
				beam[*Xr1p + 2*ldoff];
			dp[xmap[col+lxo3]+ymap[row+lyo3]] =
				beam[*Xr1p + 3*ldoff];
			Xrad1 += Xr1i;
		}
		Xrad += Xri;
		Xl1 += Xl1i;
		Xl2 += Xl2i;
	}
	Pmap->pm_flags |= PF_ENTIRE;
}

# endif
