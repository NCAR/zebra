/* -*- C++ -*-
 *
 * A C interface to the FORTRAN DECODE routine supplied by RSS with
 * the Ta SSM/I tapes.  The main routine, decode_ssmi(), takes care of
 * calling DECODE with the correct C<-->FORTRAN calling conventions.
 */

#include <math.h>

#include <defs.h>
#include "outdat.h"
#include "rss.h"
#include "level1b.h"

/*
 * Global reference to FORTRAN common block OUTDAT
 */
OUTDAT_BLOCK *C_OUTDAT = (struct _OUTDAT *)(&outdat_);

#define outdat_1 (*C_OUTDAT)

int
decode_rss (
	    int i85ghz,	// decode 85 Ghz channels
	    int itb,	// provide antenna temps if 0, brightness if 1
	    int iadj,	// If 1, along-track correction for F08 prior to 1989
	    int irec,	// which logical record in physical rec, 1 to 16
	    char *lrec)	// the logical record
{
	decode_ (&i85ghz, &itb, &iadj, &irec, lrec);
        return (0);	// Count on RSS records to always be good
}


/* ====================================================================
 * GULP.
 *
 * This is FORTRAN code converted to C, taken from ssmil1btatb.f,
 * distributed with the level 1b from the MSFC DAAC.  The hope is that
 * this code will calculate temperatures from a level 1b data record
 * and store them into the OUTDAT block used by RSS.  The code in
 * ssmil1btatb.f, interestingly enough, was taken from the RSS decode
 * source used by the routine above.
 */

/* ssmil1btatb.f -- translated by f2c (version of 22 July 1992  22:54:52).
   You must link the resulting object file with the libraries:
	-lF77 -lI77 -lm -lc   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

#ifdef notdef		// defined in outdat.h
struct {
    doublereal rev, xtime;
    integer itime, itimsc;
    real xlatsc, xlonsc, altsc, tht, hltemp[3];
    integer ivolt[2];
    real rftemp, frtemp;
    integer iagc[6], iasctm;
    real period, ascloc, anginc, axis, ecc, angper;
    integer isat;
    real spacer[6];
    integer icolda[35]	/* was [5][7] */, ihota[35]	/* was [5][7] */, 
	    icoldb[10]	/* was [5][2] */, ihotb[10]	/* was [5][2] */;
    real alat[128], alon[128], blat[128], blon[128], talo[320]	/* was [5][64]
	     */, atahi[256]	/* was [2][128] */, btahi[256]	/* was [2][
	    128] */;
    integer iatoil[128], ibtoil[128];
} outdat_;
#endif

/* ====================================================================== */
/*   This subroutine takes NESDIS SSM/I level 1b header and data records, */
/*   computes antenna temperatures, and stores the temperatures in an 	  */
/*   OUTDAT_BLOCK structure used by RSS.  It is based on a program called */
/*   SSMI1BPC written in Jan. 1991 by Ralph Ferraro, SMSRC/NESDIS. 	  */
/* ====================================================================== */

/*
 * Return 0 when all goes well.  Return nonzero if the record is bad and
 * should be skipped.
 */
int
l1bta(L1B_Header *hdr, L1B_DataRec *rec, OUTDAT_BLOCK *od)
{
    /* Local variables */
    static real aoff[7], boff[2];
    static integer ioff;
    static integer ndays, ttime, count[7];
    static integer ix, jx, iz, jz;
    static real aslope[7], bslope[2];
    static integer iz1, jz1;
    static integer loc, ies;
    static integer icx, iss, iyr;
/*
 * We need some information from the header record.
 */
	iss = hdr->start_secs / 1000;		// seconds is scaled by 1000
	ies = hdr->end_secs / 1000;		// seconds is scaled by 1000
/*
 * Determine the satellite ID from the spacecraft code in the header
 */
	switch (hdr->craft_id)
	{
	   case 2:
		od->isat = 8;	// F8
		break;
	   case 4:
		od->isat = 10;	// F10
		break;
	   case 10:
		od->isat = 11;	// F11
		break;
	   default:
		od->isat = -1;	// anyone care to guess?
	}
/*
 * Now start hassling with the data record.  Assume it has already been
 * passed through QC.
 */
	od->rev = rec->orbit;

/* ---compute XTIME (seconds since 0z 1 Jan 1987)--- */
/* ---------------------------------------------------------------- */
/*     NOTE:  XTIME is the start time of the B-scan, not */
/*            the A-scan.  To get the A-scan start time, */
/*            subtract 1.899 sec. from XTIME below. */
/* ---------------------------------------------------------------- */

    ttime = 0;
/* initialize ttime */
    iyr = 1987;
/* starting at 1987, */
    while(iyr < rec->year) {
/*   count up seconds in previous years */
	if (iyr % 4 == 0) {
	    ndays = 366;
	} else {
	    ndays = 365;
	}
	ttime += ndays * 86400;
/* 86400 sec/day */
	++iyr;
    }
    ttime += (rec->day - 1) * 86400;
/* add previous days this year */
    od->xtime = (doublereal) ttime + (doublereal) (rec->msecs) / (float)1e3;

/* add seconds */
    od->itime = nint(od->xtime);

/* ---get slopes and offsets--- */
/*   Note:  slopes are scaled by 50000, and offsets are scaled by 50 */

/* loop through the 7 A-scan channels */
    for (icx = 1; icx <= 7; ++icx) 
    {
	aslope[icx - 1] = (real) rec->slopes_a[icx - 1] / (float)5e4;
	ioff = rec->offsets_a[icx - 1];
	if (ioff > 0) {
	    ioff += -65536;
	}
	aoff[icx - 1] = (real) ioff / (float)50.;
    }

/* loop through the 2 B-scan channels */
    for (icx = 1; icx <= 2; ++icx) 
    {
	bslope[icx - 1] = (real) rec->slopes_b[icx - 1] / (float)5e4;
	ioff = rec->offsets_b[icx - 1];
	if (ioff > 0) {
	    ioff += -65536;
	}
	boff[icx - 1] = (real) ioff / (float)50.;
    }

/* ---compute low-res antenna temperatures--- */
    for (ix = 1; ix <= 64; ++ix) 
    {
	iz = (ix << 1) - 1;
	iz1 = iz + 1;
	loc = (ix - 1) * 9;
/* scana offset */
	for (icx = 1; icx <= 5; ++icx) 
	{
	    count[icx - 1] = rec->scan_a[loc + icx - 1];
//	    od->talo[icx + ix * 5 - 6] = count[icx - 1] * aslope[icx - 1]
//		     + aoff[icx - 1];
	    od->talo[ix - 1][icx - 1] = count[icx - 1] * aslope[icx - 1]
		     + aoff[icx - 1];
	}
    }

/* ----------do high-res stuff---------- */
    for (jx = 1; jx <= 128; ++jx) {
	jz = (jx << 1) - 1;
	jz1 = jz + 1;
	loc = (jx + 1) / 2 * 5;
/* ---compute A-scan high-res antenna temperatures--- */
/* scana offset */
	count[5] = rec->scan_a[loc + jz - 1];
	count[6] = rec->scan_a[loc + jz1 - 1];
//	od->atahi[(jx << 1) - 2] = count[5] * aslope[5] + aoff[5];
//	od->atahi[(jx << 1) - 1] = count[6] * aslope[6] + aoff[6];
	od->atahi[jx - 1][0] = count[5] * aslope[5] + aoff[5];
	od->atahi[jx - 1][1] = count[6] * aslope[6] + aoff[6];
/* ---compute B-scan high-res antenna temperatures--- */
	count[5] = rec->scan_b[jz - 1];
	count[6] = rec->scan_b[jz1 - 1];
	od->btahi[jx - 1][0] = count[5] * bslope[0] + boff[0];
	od->btahi[jx - 1][1] = count[6] * bslope[1] + boff[1];
/* ---get earth locations--- */
/*   Note:  earth locations are scaled by 128 */
	od->alat[jx - 1] = (real) rec->locn_a[jz - 1] / (float)128.;
	od->blat[jx - 1] = (real) rec->locn_b[jz - 1] / (float)128.;
	od->alon[jx - 1] = (real) rec->locn_a[jz1 - 1] / (float)128.;
	od->blon[jx - 1] = (real) rec->locn_b[jz1 - 1] / (float)128.;
/* ---convert longitudes from [-180..180] to [0..359.99]--- */
	if (od->alon[jx - 1] < (float)0.) {
	    od->alon[jx - 1] += (float)360.;
	}
	if (od->blon[jx - 1] < (float)0.) {
	    od->blon[jx - 1] += (float)360.;
	}
    }

    return (0);

} /* l1bta */


#ifdef notdef

/* ======================================================================= */
/*     The following subroutines, FDTB08 and FDTB00, are part of the */
/*     DECODE package published in "User's Manual, SSM/I Antenna */
/*     Temperature Tapes, Revision 1" by Frank J. Wentz, Remote Sensing */
/*     Systems, Santa Rosa, CA, December 1991, and are included by */
/*     permission. */
/* ======================================================================= */
/* Subroutine */ 
int decode_fdtb08(integer *i85ghz)
{
    /* Initialized data */

    static integer istart = 1;
    static real delta[4] = { (float).03199,(float).02685,(float).01434,(float)
	    .01186 };
    static real chi[8]	/* was [2][4] */ = { (float).00379,(float).00525,(
	    float).00983,(float)0.,(float).02136,(float).02664,(float).01387,(
	    float).01967 };
    static real avgta[5] = { (float)190.93,(float)130.14,(float)215.42,(float)
	    211.39,(float)158.16 };
    static real abias[5] = { (float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0. };
    static real sbias1[64] = { (float)-.03,(float).01,(float).04,(float).06,(
	    float).09,(float).1,(float).12,(float).12,(float).13,(float).13,(
	    float).13,(float).13,(float).13,(float).14,(float).14,(float).14,(
	    float).14,(float).14,(float).15,(float).15,(float).14,(float).14,(
	    float).13,(float).12,(float).12,(float).11,(float).11,(float).11,(
	    float).11,(float).11,(float).12,(float).11,(float).1,(float).1,(
	    float).09,(float).07,(float).06,(float).05,(float).05,(float).04,(
	    float).03,(float).03,(float).02,(float).01,(float)-.01,(float)
	    -.02,(float)-.04,(float)-.05,(float)-.06,(float)-.08,(float)-.09,(
	    float)-.09,(float)-.11,(float)-.12,(float)-.14,(float)-.18,(float)
	    -.21,(float)-.26,(float)-.3,(float)-.36,(float)-.43,(float)-.52,(
	    float)-.63,(float)-.75 };
    static real sbias2[64] = { (float)-.03,(float)-.01,(float)0.,(float).01,(
	    float).01,(float).01,(float).01,(float).01,(float).01,(float).01,(
	    float).01,(float).02,(float).03,(float).04,(float).04,(float).05,(
	    float).05,(float).05,(float).05,(float).05,(float).04,(float).04,(
	    float).04,(float).04,(float).04,(float).04,(float).04,(float).04,(
	    float).04,(float).05,(float).06,(float).07,(float).07,(float).08,(
	    float).07,(float).07,(float).06,(float).07,(float).07,(float).06,(
	    float).06,(float).06,(float).06,(float).06,(float).05,(float).04,(
	    float).04,(float).04,(float).04,(float).02,(float)0.,(float)-.01,(
	    float)-.02,(float)-.03,(float)-.04,(float)-.06,(float)-.09,(float)
	    -.12,(float)-.16,(float)-.2,(float)-.26,(float)-.32,(float)-.39,(
	    float)-.48 };
    static real sbias3[64] = { (float)-.05,(float)0.,(float).04,(float).07,(
	    float).08,(float).09,(float).1,(float).11,(float).11,(float).12,(
	    float).12,(float).13,(float).14,(float).14,(float).15,(float).15,(
	    float).17,(float).17,(float).16,(float).16,(float).15,(float).14,(
	    float).14,(float).14,(float).13,(float).13,(float).13,(float).13,(
	    float).13,(float).13,(float).14,(float).13,(float).14,(float).13,(
	    float).12,(float).11,(float).1,(float).09,(float).09,(float).08,(
	    float).07,(float).05,(float).04,(float).03,(float).02,(float)0.,(
	    float)-.01,(float)-.02,(float)-.04,(float)-.05,(float)-.07,(float)
	    -.1,(float)-.12,(float)-.14,(float)-.17,(float)-.2,(float)-.25,(
	    float)-.31,(float)-.36,(float)-.42,(float)-.5,(float)-.63,(float)
	    -.79,(float)-.97 };
    static real sbias4[64] = { (float)-.06,(float)-.03,(float)0.,(float).01,(
	    float).03,(float).04,(float).05,(float).06,(float).06,(float).07,(
	    float).09,(float).1,(float).11,(float).13,(float).14,(float).15,(
	    float).15,(float).16,(float).16,(float).16,(float).16,(float).16,(
	    float).16,(float).15,(float).14,(float).14,(float).14,(float).13,(
	    float).13,(float).13,(float).14,(float).13,(float).13,(float).13,(
	    float).12,(float).11,(float).1,(float).1,(float).09,(float).08,(
	    float).08,(float).06,(float).06,(float).05,(float).04,(float).03,(
	    float).02,(float).01,(float)0.,(float)-.02,(float)-.05,(float)
	    -.07,(float)-.09,(float)-.12,(float)-.16,(float)-.18,(float)-.22,(
	    float)-.27,(float)-.33,(float)-.42,(float)-.5,(float)-.6,(float)
	    -.74,(float)-.9 };
    static real sbias5[64] = { (float)-.23,(float)-.2,(float)-.17,(float)-.15,
	    (float)-.14,(float)-.13,(float)-.12,(float)-.1,(float)-.09,(float)
	    -.1,(float)-.09,(float)-.07,(float)-.05,(float)-.04,(float)-.03,(
	    float)0.,(float).01,(float).03,(float).04,(float).04,(float).04,(
	    float).05,(float).05,(float).05,(float).04,(float).04,(float).04,(
	    float).05,(float).05,(float).07,(float).08,(float).09,(float).1,(
	    float).11,(float).1,(float).1,(float).09,(float).1,(float).1,(
	    float).09,(float).09,(float).09,(float).09,(float).08,(float).07,(
	    float).07,(float).07,(float).09,(float).09,(float).08,(float).08,(
	    float).08,(float).08,(float).08,(float).06,(float).04,(float)0.,(
	    float)-.03,(float)-.07,(float)-.11,(float)-.16,(float)-.22,(float)
	    -.29,(float)-.41 };

    static integer icel;
    static real xfac, ta19h, ta37h, tb85h, ta22v, ta19v, ta37v, tb85v;
    static integer ifreq;
    static real ahh[4], aoh[4], ahv[4], avh[4], aov[4], avv[4];


/*     THIS SUBROUTINE CONVERTS ANTENNA TEMPS. TO BRIGHTNESS TEMPS. */
/*     F08 ALONG-SCAN AND OFFSET CORRECTIONS */


/*     DATA INITIALIZATION */


/*     ********************************************************** */
/*     IN ORDER TO APPLY WENTZ [1992] OFFSET TO TB'S, */
/*     ACTIVATE NEXT STATEMENT AND COMMENT OUT PREVIOUS STATEMENT */
/*     DATA ABIAS/2.0,3.5,1.32,-1.57,-0.22/ */
/*     ********************************************************** */


/*     BEGIN EXECUTION */

    if (istart == 0) {
	goto L30;
    }
    istart = 0;
    for (ifreq = 1; ifreq <= 4; ++ifreq) {
	if (ifreq == 2) {
	    goto L10;
	}
	xfac = ((float)1. - chi[(ifreq << 1) - 2] * chi[(ifreq << 1) - 1]) * (
		(float)1. - delta[ifreq - 1]);
	avv[ifreq - 1] = (chi[(ifreq << 1) - 2] + (float)1.) / xfac;
	ahv[ifreq - 1] = -(doublereal)chi[(ifreq << 1) - 2] * (chi[(ifreq << 
		1) - 1] + (float)1.) / xfac;
	aov[ifreq - 1] = ((float)1. - avv[ifreq - 1] - ahv[ifreq - 1]) * (
		float)2.7;
	ahh[ifreq - 1] = (chi[(ifreq << 1) - 1] + (float)1.) / xfac;
	avh[ifreq - 1] = -(doublereal)chi[(ifreq << 1) - 1] * (chi[(ifreq << 
		1) - 2] + (float)1.) / xfac;
	aoh[ifreq - 1] = ((float)1. - ahh[ifreq - 1] - avh[ifreq - 1]) * (
		float)2.7;
L10:
	;
    }

    for (icel = 1; icel <= 64; ++icel) {
	sbias1[icel - 1] = (float)1. - sbias1[icel - 1] / avgta[0];
	sbias2[icel - 1] = (float)1. - sbias2[icel - 1] / avgta[1];
	sbias3[icel - 1] = (float)1. - sbias3[icel - 1] / avgta[2];
	sbias4[icel - 1] = (float)1. - sbias4[icel - 1] / avgta[3];
	sbias5[icel - 1] = (float)1. - sbias5[icel - 1] / avgta[4];
/* L20: */
    }

L30:

    for (icel = 1; icel <= 64; ++icel) {
//	ta19v = outdat_1.talo[icel * 5 - 5] * sbias1[icel - 1] - abias[0];
//	ta19h = outdat_1.talo[icel * 5 - 4] * sbias2[icel - 1] - abias[1];
//	ta22v = outdat_1.talo[icel * 5 - 3] * sbias3[icel - 1] - abias[2];
//	ta37v = outdat_1.talo[icel * 5 - 2] * sbias4[icel - 1] - abias[3];
//	ta37h = outdat_1.talo[icel * 5 - 1] * sbias5[icel - 1] - abias[4];
	ta19v = outdat_1.talo[icel - 1][0] * sbias1[icel - 1] - abias[0];
	ta19h = outdat_1.talo[icel - 1][1] * sbias2[icel - 1] - abias[1];
	ta22v = outdat_1.talo[icel - 1][2] * sbias3[icel - 1] - abias[2];
	ta37v = outdat_1.talo[icel - 1][3] * sbias4[icel - 1] - abias[3];
	ta37h = outdat_1.talo[icel - 1][4] * sbias5[icel - 1] - abias[4];

	outdat_1.talo[icel * 5 - 5] = avv[0] * ta19v + ahv[0] * ta19h + aov[0]
		;
	outdat_1.talo[icel * 5 - 4] = ahh[0] * ta19h + avh[0] * ta19v + aoh[0]
		;
	outdat_1.talo[icel * 5 - 3] = ta22v * (float)1.01993 + (float)1.994;
	outdat_1.talo[icel * 5 - 2] = avv[2] * ta37v + ahv[2] * ta37h + aov[2]
		;
	outdat_1.talo[icel * 5 - 1] = ahh[2] * ta37h + avh[2] * ta37v + aoh[2]
		;
/* L100: */
    }

    if (*i85ghz == 0) {
	return 0;
    }

    for (icel = 1; icel <= 128; ++icel) {
	tb85v = avv[3] * outdat_1.atahi[(icel << 1) - 2] + ahv[3] * 
		outdat_1.atahi[(icel << 1) - 1] + aov[3];
	tb85h = ahh[3] * outdat_1.atahi[(icel << 1) - 1] + avh[3] * 
		outdat_1.atahi[(icel << 1) - 2] + aoh[3];
	outdat_1.atahi[(icel << 1) - 2] = tb85v;
	outdat_1.atahi[(icel << 1) - 1] = tb85h;
	tb85v = avv[3] * outdat_1.btahi[(icel << 1) - 2] + ahv[3] * 
		outdat_1.btahi[(icel << 1) - 1] + aov[3];
	tb85h = ahh[3] * outdat_1.btahi[(icel << 1) - 1] + avh[3] * 
		outdat_1.btahi[(icel << 1) - 2] + aoh[3];
	outdat_1.btahi[(icel << 1) - 2] = tb85v;
	outdat_1.btahi[(icel << 1) - 1] = tb85h;
/* L200: */
    }
    return 0;
} /* fdtb08_ */


int
decode_fdtb00(i85ghz)
integer *i85ghz;
{
    /* Initialized data */

    static integer istart = 1;
    static real delta[4] = { (float).03199,(float).02685,(float).01434,(float)
	    .01186 };
    static real chi[8]	/* was [2][4] */ = { (float).00379,(float).00525,(
	    float).00983,(float)0.,(float).02136,(float).02664,(float).01387,(
	    float).01967 };
    static real avgta[5] = { (float)190.93,(float)130.14,(float)215.42,(float)
	    211.39,(float)158.16 };
    static real abias[5] = { (float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0. };
    static real sbias1[64] = { (float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0. };
    static real sbias2[64] = { (float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0. };
    static real sbias3[64] = { (float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0. };
    static real sbias4[64] = { (float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0. };
    static real sbias5[64] = { (float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0. };

    static integer icel;
    static real xfac, ta19h, ta37h, tb85h, ta22v, ta19v, ta37v, tb85v;
    static integer ifreq;
    static real ahh[4], aoh[4], ahv[4], avh[4], aov[4], avv[4];


/*     THIS SUBROUTINE CONVERTS ANTENNA TEMPS. TO BRIGHTNESS TEMPS. */
/*     ALONG-SCAN AND OFFSET CORRECTIONS SET TO ZERO */


/*     SPECIFY COMMON /OUTDAT/ */


/*     DATA INITIALIZATION */


/*     BEGIN EXECUTION */

    if (istart == 0) {
	goto L30;
    }
    istart = 0;
    for (ifreq = 1; ifreq <= 4; ++ifreq) {
	if (ifreq == 2) {
	    goto L10;
	}
	xfac = ((float)1. - chi[(ifreq << 1) - 2] * chi[(ifreq << 1) - 1]) * (
		(float)1. - delta[ifreq - 1]);
	avv[ifreq - 1] = (chi[(ifreq << 1) - 2] + (float)1.) / xfac;
	ahv[ifreq - 1] = -(doublereal)chi[(ifreq << 1) - 2] * (chi[(ifreq << 
		1) - 1] + (float)1.) / xfac;
	aov[ifreq - 1] = ((float)1. - avv[ifreq - 1] - ahv[ifreq - 1]) * (
		float)2.7;
	ahh[ifreq - 1] = (chi[(ifreq << 1) - 1] + (float)1.) / xfac;
	avh[ifreq - 1] = -(doublereal)chi[(ifreq << 1) - 1] * (chi[(ifreq << 
		1) - 2] + (float)1.) / xfac;
	aoh[ifreq - 1] = ((float)1. - ahh[ifreq - 1] - avh[ifreq - 1]) * (
		float)2.7;
L10:
	;
    }

    for (icel = 1; icel <= 64; ++icel) {
	sbias1[icel - 1] = (float)1. - sbias1[icel - 1] / avgta[0];
	sbias2[icel - 1] = (float)1. - sbias2[icel - 1] / avgta[1];
	sbias3[icel - 1] = (float)1. - sbias3[icel - 1] / avgta[2];
	sbias4[icel - 1] = (float)1. - sbias4[icel - 1] / avgta[3];
	sbias5[icel - 1] = (float)1. - sbias5[icel - 1] / avgta[4];
/* L20: */
    }

L30:

    for (icel = 1; icel <= 64; ++icel) {
	ta19v = outdat_1.talo[icel * 5 - 5] * sbias1[icel - 1] - abias[0];
	ta19h = outdat_1.talo[icel * 5 - 4] * sbias2[icel - 1] - abias[1];
	ta22v = outdat_1.talo[icel * 5 - 3] * sbias3[icel - 1] - abias[2];
	ta37v = outdat_1.talo[icel * 5 - 2] * sbias4[icel - 1] - abias[3];
	ta37h = outdat_1.talo[icel * 5 - 1] * sbias5[icel - 1] - abias[4];

	outdat_1.talo[icel * 5 - 5] = avv[0] * ta19v + ahv[0] * ta19h + aov[0]
		;
	outdat_1.talo[icel * 5 - 4] = ahh[0] * ta19h + avh[0] * ta19v + aoh[0]
		;
	outdat_1.talo[icel * 5 - 3] = ta22v * (float)1.01993 + (float)1.994;
	outdat_1.talo[icel * 5 - 2] = avv[2] * ta37v + ahv[2] * ta37h + aov[2]
		;
	outdat_1.talo[icel * 5 - 1] = ahh[2] * ta37h + avh[2] * ta37v + aoh[2]
		;
/* L100: */
    }

    if (*i85ghz == 0) {
	return 0;
    }

    for (icel = 1; icel <= 128; ++icel) {
	tb85v = avv[3] * outdat_1.atahi[(icel << 1) - 2] + ahv[3] * 
		outdat_1.atahi[(icel << 1) - 1] + aov[3];
	tb85h = ahh[3] * outdat_1.atahi[(icel << 1) - 1] + avh[3] * 
		outdat_1.atahi[(icel << 1) - 2] + aoh[3];
	outdat_1.atahi[(icel << 1) - 2] = tb85v;
	outdat_1.atahi[(icel << 1) - 1] = tb85h;
	tb85v = avv[3] * outdat_1.btahi[(icel << 1) - 2] + ahv[3] * 
		outdat_1.btahi[(icel << 1) - 1] + aov[3];
	tb85h = ahh[3] * outdat_1.btahi[(icel << 1) - 1] + avh[3] * 
		outdat_1.btahi[(icel << 1) - 2] + aoh[3];
	outdat_1.btahi[(icel << 1) - 2] = tb85v;
	outdat_1.btahi[(icel << 1) - 1] = tb85h;
/* L200: */
    }
    return 0;
} /* fdtb00_ */

#endif /* endif */
