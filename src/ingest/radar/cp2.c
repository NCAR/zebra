/*
 * Isolation (quarantine) of all the obnoxiousness relating to CP2 data.
 */

/*		Copyright (C) 1987-1995 by UCAR
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
# include <math.h>

# include <config.h>
# include <defs.h>
# include <message.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"

MAKE_RCSID ("$Id: cp2.c,v 2.1 1995-04-07 21:05:35 corbet Exp $")


/*
 * Here we keep track of the stuff we use to demangle cp2 data.  This thing
 * is indexed by the field index.
 */
typedef struct _cp2derinfo
{
	int cd_offsets[3];	/* Offsets to constituent fields */
	unsigned char cd_table[256];	/* Static lookup table	*/
	void (*cd_func) ();	/* Function to do the calculation */
	float *cd_rangec;	/* Range correction array	*/
	float *cd_cal;		/* Calibration array 		*/
	ScaleInfo cd_scale;
/* What else? */
} CP2DerInfo;

CP2DerInfo DerInfo[10];

/*
 * Here we save parameters that were used in the calculation of our
 * calibration tables.  Thus we can notice changes and recalculate things.
 */
static USHORT SvGS;		/* Gate spacing */
static USHORT SvPRF;		/* PRF		*/
static USHORT SvATP;		/* Power;	*/
static USHORT SvNGate;		/* Number of gates */

/*
 * Radar calibrations.
 */
static float SCal[256];
static float XCal[256];
static float SRangeC[2048];	/* Range correction */
static float XRangeC[2048];	/* Range correction */
static int IsScal = 0;

/*
 * Remember where S and X are so we can do the difference if they want.
 */
static int SIndex = -1, XIndex = -1;

/*
 * Local stuff.
 */
static void CP2_SetupVelocity FP ((Housekeeping *, CP2DerInfo *, ScaleInfo *));
static void CP2_SetupPower FP ((Housekeeping *, CP2DerInfo *, ScaleInfo *));
static void CP2_SetupRefl FP ((Housekeeping *, CP2DerInfo *, ScaleInfo *,
		int));
static void CP2_SetupZDR FP ((Housekeeping *, CP2DerInfo *, ScaleInfo *));
static void CP2_SetupSX FP ((Housekeeping *, CP2DerInfo *, ScaleInfo *));
static int CP2_FindParam FP ((Housekeeping *, int));
static float CP2_s_radcon FP ((Housekeeping *));
static float CP2_x_radcon FP ((Housekeeping *));
/* static unsigned char CP2_EnScale FP ((double, ScaleInfo *)); */

static void CP2_dp_FillBad FP ((Housekeeping *, unsigned char *, int, int,
		CP2DerInfo *, unsigned char *));
static void CP2_dp_Lookup FP ((Housekeeping *, unsigned char *, int, int,
		CP2DerInfo *, unsigned char *));
static void CP2_dp_Refl FP ((Housekeeping *, unsigned char *, int, int,
		CP2DerInfo *, unsigned char *));
static void CP2_dp_SX FP ((Housekeeping *, unsigned char *, int, int,
		CP2DerInfo *, unsigned char *));
static int CP2_CalPair FP ((float *, struct ui_command *));





inline static unsigned char
CP2_EnScale (v, scale)
float v;
ScaleInfo *scale;
/*
 * Scale this into an 8-bit value.
 */
{
	int ret = (v - scale->s_Offset)/scale->s_Scale;
	return ((ret < 0 || ret > 255) ? 0xff : ret);
}




void
HandleCP2Mess (beam, hk, scale)
Beam beam;
Housekeeping *hk;
ScaleInfo *scale;
/*
 * This is the routine that tries, desperately, to figure out how we are
 * going to derive the fields that have been requested of us.
 */
{
	int field;
/*
 * Plow through each field and see what we come up with.
 */
	for (field = 0; field < NField; field++)
	{
		if (! strcmp (Fields[field], "velocity"))
			CP2_SetupVelocity (hk, DerInfo + field, scale + field);
		else if (! strcmp (Fields[field], "power"))
			CP2_SetupPower (hk, DerInfo + field, scale + field);
		else if (! strcmp (Fields[field], "reflectivity"))
		{
			CP2_SetupRefl (hk, DerInfo + field, scale + field,
					TRUE);
			SIndex = field;
		}
		else if (! strcmp (Fields[field], "xrefl"))
		{
			CP2_SetupRefl (hk, DerInfo + field, scale + field,
					FALSE);
			XIndex = field;
		}
		else if (! strcmp (Fields[field], "zdr"))
			CP2_SetupZDR (hk, DerInfo + field, scale + field);
		else if (! strcmp (Fields[field], "sminusx"))
			CP2_SetupSX (hk, DerInfo + field, scale + field);
		else
			ui_printf ("Unknown field: %s\n", Fields[field]);
		DerInfo[field].cd_scale = scale[field];
	}
/*
 * Stash our parameters.
 */
	SvGS = hk->gate_spacing;
	SvNGate = hk->gates_per_beam;
	SvATP = hk->avg_xmit_pwr;
	SvPRF = hk->prfx10;
}




void
CP2_CheckParams (beam, hk, scale)
Beam beam;
Housekeeping *hk;
ScaleInfo *scale;
/*
 * Check the parameters in this beam and recalculate things if need be.
 */
{
	if (hk->gate_spacing != SvGS ||
			hk->gates_per_beam != SvNGate ||
			ABS (hk->avg_xmit_pwr - SvATP) > 5 ||
			hk->prfx10 != SvPRF)
	{
		msg_ELog (EF_INFO, "CP2 parameter change");
		HandleCP2Mess (beam, hk, scale);
	}
}
			



static void
CP2_SetupVelocity (hk, dip, scale)
Housekeeping *hk;
CP2DerInfo *dip;
ScaleInfo *scale;
/*
 * Figure out how in the hell we're ever going to get velocity out of
 * this stuff.
 */
{
	float vscale;
	int counts;
/*
 * Find the offset to the vel field.
 */
	if ((dip->cd_offsets[0] = CP2_FindParam (hk, 0x8008)) < 0)
	{
		ui_printf ("Can't find velocity!\n");
		dip->cd_func = CP2_dp_FillBad;
		return;
	}
/*
 * Find the threshold field too.
 */
	if ((dip->cd_offsets[1] = CP2_FindParam (hk, HSK_PD_PWRCNT)) < 0)
		ui_printf ("No power for threshold!\n");
/*
 * Figure out scaling and make the lookup table.
 */
	scale->s_Scale = 0.25;
	scale->s_Offset = -32.0;
	vscale = ((float) hk->wavelength)/10000.0 * ((float) hk->prfx10)/10.0 *
		0.25/127.0;
	for (counts = 0; counts < 128; counts++)
		dip->cd_table[counts] = CP2_EnScale (counts*vscale, scale);
	for (counts = 128; counts < 256; counts++)
		dip->cd_table[counts] = CP2_EnScale ((counts - 255)*vscale,
				scale);
	dip->cd_func = CP2_dp_Lookup;
}





static void
CP2_SetupPower (hk, dip, scale)
Housekeeping *hk;
CP2DerInfo *dip;
ScaleInfo *scale;
/*
 * Figure out how in the hell we're ever going to get log power out of
 * this stuff.
 */
{
	float vscale;
	int counts;
/*
 * Find the offset to the power field.
 */
	if ((dip->cd_offsets[0] = CP2_FindParam (hk, HSK_PD_PWRCNT)) < 0)
	{
		ui_printf ("Can't find power!\n");
		dip->cd_func = CP2_dp_FillBad;
		return;
	}
	dip->cd_offsets[1] = dip->cd_offsets[0];
/*
 * Figure out scaling and make the lookup table.  Assume they've loaded up
 * a calibration.
 */
	scale->s_Scale = 0.5;
	scale->s_Offset = -130.0;
	for (counts = 0; counts < 255; counts++)
		dip->cd_table[counts] = CP2_EnScale (SCal[counts], scale);
	dip->cd_func = CP2_dp_Lookup;
}





static void
CP2_SetupZDR (hk, dip, scale)
Housekeeping *hk;
CP2DerInfo *dip;
ScaleInfo *scale;
/*
 * Get ZDR together.
 */
{
	float zrange;
	int counts;
	struct dual_pol_mode dpm;
/*
 * Find the offset to the ZDR field.
 */
	if ((dip->cd_offsets[0] = CP2_FindParam (hk, HSK_PD_ZDR)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Can't find ZDR!");
		dip->cd_func = CP2_dp_FillBad;
		return;
	}
/*
 * Find the threshold field too.
 */
	if ((dip->cd_offsets[1] = CP2_FindParam (hk, HSK_PD_PWRCNT)) < 0)
		ui_printf ("No power for threshold!\n");
/*
 * Figure out scaling and make the lookup table.  Assume they've loaded up
 * a calibration.
 */
	memcpy (&dpm, &hk->dual_pol_mode, sizeof (short));
	zrange = 3.0*(1 << dpm.zdr_scale);
	msg_ELog (EF_INFO, "ZDR range is %.2f", zrange);
	scale->s_Scale = zrange/128.0;
	scale->s_Offset = -zrange;
	for (counts = 0; counts < 127; counts++)
		dip->cd_table[counts] = CP2_EnScale (counts*zrange/127.,scale);
	for (counts = 128; counts < 256; counts++)
		dip->cd_table[counts] = CP2_EnScale((counts - 255)*zrange/127.,
				scale);
	dip->cd_func = CP2_dp_Lookup;
}





static void
CP2_SetupRefl (hk, dip, scale, sband)
Housekeeping *hk;
CP2DerInfo *dip;
ScaleInfo *scale;
int sband;
/*
 * Get our act together for reflectivity.
 */
{
	int gate, counts, pf = sband ? HSK_PD_PWRCNT : HSK_PD_XPWR;
	float radcon, gspace = hk->gate_spacing/1000.0;
	float *rangec = sband ? SRangeC : XRangeC;
/*
 * Find the offset to the power field.
 */
	if ((dip->cd_offsets[0] = CP2_FindParam (hk, pf)) < 0)
	{
		ui_printf ("Can't find %s-band power!\n", sband ? "S" : "X");
		dip->cd_func = CP2_dp_FillBad;
		return;
	}
	dip->cd_offsets[1] = dip->cd_offsets[0];
/*
 * Figure out scaling and make the lookup table.  Assume they've loaded up
 * a calibration.
 */
	scale->s_Scale = 0.4;
	scale->s_Offset = -20.0;
/*
 * Get a radar constant.
 */
	radcon = sband ? CP2_s_radcon (hk) : CP2_x_radcon (hk);
	msg_ELog (EF_INFO, "Radar constant is %.2f", radcon);
/*
 * Now we can create the range correction array.
 */
	for (gate = 0; gate < hk->gates_per_beam; gate++)
		rangec[gate] = 20*log10(gate*gspace) + radcon;
	dip->cd_func = CP2_dp_Refl;
	dip->cd_rangec = rangec;
	dip->cd_cal = sband ? SCal : XCal;
}



static void
CP2_SetupSX (hk, dip, scale)
Housekeeping *hk;
CP2DerInfo *dip;
ScaleInfo *scale;
/*
 * Set up to derive S-X.
 */
{
/*
 * Find the indices.
 */
	if ((dip->cd_offsets[0] = CP2_FindParam (hk, HSK_PD_PWRCNT)) < 0 ||
	    (dip->cd_offsets[1] = CP2_FindParam (hk, HSK_PD_XPWR)) < 0)
	{
		ui_printf ("Can't find power for S-X\n");
		dip->cd_func = CP2_dp_FillBad;
		return;
	}
/*
 * Now we just point to our function which will have to deal with the rest.
 */
	scale->s_Scale = .25;
	scale->s_Offset = -30.0;
	dip->cd_func = CP2_dp_SX;
}






static float
CP2_s_radcon (hk)
Housekeeping *hk;
/*
 * Return the S-band radar constant.
 */
{
/*
 * This bit of awful nastiness gives us something approximating the
 * radar constant.  The 1.5 is from Jing's code, seems to be a kludge
 * shorthand for various losses...
 */
	return (168.8 + 10.0*log10 (hk->prfx10/10.0) -
		2.0*hk->sys_gain/10.0 - hk->avg_xmit_pwr/10.0 +
		20.0*log10 (hk->wavelength/10000.0) -
		20.0*log10 ((hk->ant_bw/100.0)*(M_PI/180.0)) -
		10.0*log10 (0.93) + 1.5);
}





static float
CP2_x_radcon (hk)
Housekeeping *hk;
/*
 * Return the X-band radar constant.
 */
{
/*
 * This bit of awful nastiness gives us something approximating the
 * radar constant.  The 3.0 is from Jing's code, seems to be a kludge
 * shorthand for various losses...
 */
	return (168.8 + 10.0*log10 (hk->prf_secondary/10.0) -
		2.0*hk->sys_gain2/10.0 - hk->atp2/10.0 +
		20.0*log10 (hk->wavelength2/10000.0) -
		20.0*log10 ((hk->ant_bw2/100.0)*(M_PI/180.0)) -
		10.0*log10 (0.93) + 3.0);
}


void
CP2_LoadCal (which)
int which;
/*
 * Pull in a calibration.
 */
{
	int i, last, next;
	float *calp = (which == 0) ? SCal : XCal;
/*
 * Init the cal and read in the pairs.
 */
	for (i = 0; i < 256; i++)
		calp[i] = -9999.0;
	ui_subcommand ("cal-initial", "Cal>", CP2_CalPair, (long) calp);
/*
 * Now we need to smooth out all the holes.
 */
	for (last = 0; last < 256; last++)
		if (calp[last] != -9999.0)
			break;
	for (next = last + 1; next < 256; next++)
	{
		if (calp[next] != -9999.0)
		{
			int tweak;
			float step = (calp[next] - calp[last])/(next - last);
			for (tweak = 1; last + tweak < next; tweak++)
				calp[last+tweak] = calp[last] + (tweak*step);
			last = next;
		}
	}
}





static int
CP2_CalPair (cal, cmds)
float *cal;
struct ui_command *cmds;
/*
 * Deal with a calibration command.
 */
{
	if (UKEY(cmds[0]) == RIC_ENDCAL)
		return (FALSE);
	cal[cmds[1].uc_v.us_v_int] = UFLOAT (cmds[2]);
	return (TRUE);
}






static int
CP2_FindParam (hk, param)
Housekeeping *hk;
unsigned short param;
/*
 * Find a given source field.
 */
{
	int ret;
	for (ret = 0; ret < 6; ret++)	/* don't know about the 6?? */
		if (hk->parm_desc[ret] == param)
			return (ret);
	return (-1);
}







/*
 * Actual derivation routines.
 */


void
CP2_DoDerivation (hk, beam, field, dest)
Housekeeping *hk;
Beam beam;
int field;
unsigned char *dest;
/*
 * Actually do derivation on this beam.
 */
{
	int chunk;
	CP2DerInfo *dip = DerInfo + field;
	
	for (chunk = 0; chunk < beam->b_npart; chunk++)
	{
		int ngate = beam->b_gdesc[chunk].gd_ngate;
		(*dip->cd_func) (hk, beam->b_gdesc[chunk].gd_data, ngate,
				hk->parm_per_gate, dip, dest);
		dest += beam->b_gdesc[chunk].gd_ngate;
	}
}



static void
CP2_dp_FillBad (hk, data, ngate, skip, dip, dest)
Housekeeping *hk;
int ngate, skip;
CP2DerInfo *dip;
unsigned char *data, *dest;
/*
 * Fill the world with bad flags.
 */
{
	memset (dest, 0xff, ngate);
}




static void
CP2_dp_Lookup (hk, data, ngate, skip, dip, dest)
Housekeeping *hk;
const int ngate, skip;
CP2DerInfo *dip;
register unsigned char *data, *dest;
/*
 * Calibrate a basic table lookup field.
 */
{
	const unsigned char *lt = dip->cd_table, thresh = ThrCounts;
	unsigned char *thr = data + dip->cd_offsets[1];
	register int gate;

	data += dip->cd_offsets[0];
	if (DoThresholding)
		for (gate = 0; gate < ngate; gate++)
		{
			*dest++ = (*thr > thresh) ? lt[*data] : 0xff;
			data += skip;
			thr += skip;
		}
	else
		for (gate = 0; gate < ngate; gate++)
		{
			*dest++ = lt[*data];
			data += skip;
		}
}




static void
CP2_dp_Refl (hk, data, ngate, skip, dip, dest)
Housekeeping *hk;
const int ngate, skip;
CP2DerInfo *dip;
register unsigned char *data, *dest;
/*
 * Do reflectivity
 */
{
	const ScaleInfo *sc = &dip->cd_scale;
	register int gate;
	const float *rangec = dip->cd_rangec;
	const float *cal = dip->cd_cal;

	data += dip->cd_offsets[0];
	for (gate = 0; gate < ngate; gate++)
	{
		if (*data > ThrCounts)
			*dest++ = CP2_EnScale(cal[*data] + rangec[gate], sc);
		else
			*dest++ = 0xff;
		data += skip;
	}
}



static void
CP2_dp_SX (hk, data, ngate, skip, dip, dest)
Housekeeping *hk;
const int ngate, skip;
CP2DerInfo *dip;
unsigned char *data;
register unsigned char *dest;
/*
 * Do the reflectivity difference field.
 */
{
	const ScaleInfo *sc = &dip->cd_scale;
	static bool griped = FALSE;
	register unsigned char *sdata, *xdata;
	register int gate;
/*
 * Make sure that both power fields are around.
 */
	if (SIndex < 0 || XIndex < 0)
	{
		if (! griped)
		{
			msg_ELog (EF_PROBLEM,
					"Both S and X power needed for S-X");
			griped = TRUE;
		}
		CP2_dp_FillBad (hk, data, ngate, skip, dip, dest);
		return;
	}
/*
 * Find our data fields.
 */
	sdata = data + dip->cd_offsets[0];
	xdata = data + dip->cd_offsets[1];
/*
 * Now we blast through and do the calculation.
 */
	for (gate = 0; gate < ngate; gate++)
	{
		if (*sdata <= ThrCounts || *xdata <= ThrCounts)
			*dest++ = 0xff;
		else
			*dest++ = CP2_EnScale (SCal[*sdata] + SRangeC[gate] -
					(XCal[*xdata] + XRangeC[gate]), sc);
		sdata += skip;
		xdata += skip;
	}
}
