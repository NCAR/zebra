/* rgrid.f -- translated by f2c (version 19960315). */

#include "f2c.h"

/* Common Block Declarations */

struct fltrpl_1_ {
    integer ns[3];
};

#define fltrpl_1 (*(struct fltrpl_1_ *) &fltrpl_)

/* Initialized data */

struct {
    integer e_1[3];
    } fltrpl_ = { 0, 0, 0 };


/* Table of constant values */

static real c_b7 = (float)1.;
static real c_b8 = (float).5;
static integer c__1 = 1;

/* Subroutine */
int bints_(z__, isize, jsize, xsta, ysta, data, dz, dzr, 
	   nsta, ip, r__, rmx, nqd, nlflt, bad)
real *z__;
integer *isize, *jsize;
real *xsta, *ysta, *data, *dz, *dzr;
integer *nsta, *ip;
real *r__, *rmx;
integer *nqd, *nlflt;
real *bad;
{
    /* Initialized data */

    static real chng = (float)1.;
    static real big = (float)1e10;
    static integer ig = 1;

    /* Format strings */
    static char fmt_129[] = "(/\002 INSUFFICIENT DATA FOR CORRECTOR ITERATIO\
N.\002/)";
    static char fmt_130[] = "(/\002 RMS RESIDUAL INCREASED BEFORE SPECIFIED \
NUMBER OF\002,\002 PASSES COMPLETED.\002,/\002 PASS NUMBER = \002,i2,\002. T\
OTAL NUMBER SPECIFIED = \002,i2,\002.\002,/\002 ITERATION STOPPED AT THIS PO\
INT.\002/)";

    /* System generated locals */
    integer z_dim1, z_offset, dzr_dim1, dzr_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2;

    /* Builtin functions */
    double log();
    /*integer i_nint();*/
    double /*r_sign(), */sqrt();
#ifdef FIO
    integer s_wsfe(), e_wsfe();
#endif
    double exp();

    /* Local variables */
    static real rfac, rreg;
    static integer ista, kntr, nrms;
    static real sumw;
    static integer i__, j, l, m;
    static real w, x, y;
    static integer iquad[4];
    static real rquad[4];
    static integer ipass, kntqd;
    static real r2, w1, w2, w3, w4, rmxsq;
    extern /* Subroutine */ int t5fltr_(), confld_();
    static real oldrms, rdspdx, rt2;
    static integer ind;
    static real rdn, xof, yof, rms, sum;

    /* Fortran I/O blocks */
    static cilist io___29 = { 0, 6, 0, fmt_129, 0 };
    static cilist io___30 = { 0, 6, 0, fmt_130, 0 };



/* $Id: bintsf2c.c,v 2.1 1996-04-16 00:18:12 granger Exp $ */

/* Implementation of the Barnes objective analysis technique: */

/*          Barnes,S.L., 1964: A technique for maximizing details in */
/*             numerical weather map analysis. J. Appl. Meteor., */
/*             3, 396-409. */

/*         ------ ,1973: Mesoscale objective map analysis using weighted 
*/
/*             time-series observations. NOAA Tech. Memo. ERL NSSL-62, */
/*             60 pp. [ NTIS COM-73-10781 ]. */

/* Leise Telescoped Filter: */

/*         Leise, J.A., 1981: A multidimensional scale-telescoped */
/*             filter and data extension package. NOAA Tech. Memo. */
/*             ERL/WPL-82, Wave Propogation Lab, Boulder, 20pp. */

/* History: */
/*    1980 -- Programmed by Jim Heimbach, Montana State Univ. */
/*    1981 -- Converted to the NCAR CRAY BY Tom Engle, CRAY/NCAR */
/*    1982 -- Enhanced by Carl Mohr, NCAR/CSD for use */
/*            with CCOPE data as documented in: */

/*         Mohr,C.G., L.J. Miller, R.L. Vaughan and H.W. Frank, 1986: */
/*             The merger of mesoscale datasets into a common Cartesian */
/*             format for effiecient and systematic analyses. */
/*             J. Atmos. and Ocean. Tech., Vol. 3, No. 1, */
/*             (March, 1986), 144-161. */

/*    1989 -- Cleaned up for general usage by Carl Mohr (NCAR/ATD) */
/*            In this version 2 features have been disabled... */
/*               1- Non-spatial differential weighting (such as */
/*                  utilized for off-time or noisy reports) is */
/*                  not implemented -- it's a red herring anyway. */
/*               2- In a similar vein, adaptive adjustment of the */
/*                  radius of influence is also disabled. Sure, */
/*                  it preserves the scales of the original when */
/*                  reports are non-uniformly spaced. The resultant */
/*                  gridded field is also a noisy mess requiring post- */
/*                  filtering. */

/*    Questions??? */
/*                  Carl Mohr */
/*                  NCAR/ATD/RAP */
/*                  P.O. Box 3000 */
/*                  Boulder, CO 80307-3000 */
/*                  (303) 497-8968 */
/*                  E-mail: mohr@ncar.ucar.edu */


/*       An implementation of the Barnes Objective Analysis Technique is 
*/
/*   employed for remapping the irregularly spaced input values to the uni
form*/
/*   2-D Cartesian grid. At each destination grid location all input value
s,*/
/*   no matter how distant, are included in the calculation of the estimat
e.*/
/*   The weight W assigned to any datum at distance D from the destination
*/
/*    Cartesian coordinate location is given by the following gaussian */
/*    function: */

/*       W = EXP (ln(0.1) * D**2 / R**2 ) */

/*    where R is the distance (in kilometers) where the gaussian weighting
 */
/*    function will be equal to 1/10. Every input value */
/*    is multiplied by it's calculated weight, all weighted values are */
/*    accumulated and a final normalized estimate is assigned to the grid 
*/
/*    location in question. R controls the response of the gaussian */
/*    weighting function. Satisfactory results are usually attained with 
*/
/*   R set approximately equal to the maximum spacing of the input values.
*/

/*       An iterative correction technique for reducing the Root Mean */
/*   Square (RMS) difference between the original input values and the nea
rest*/
/*   Cartesian estimates may also be invoked. If IP is set greater than 1,
*/
/*    the residual between each original datum and the */
/*    Cartesian field estimate is computed immediately following the first
 */
/*   pass (described above) of the objective analysis procedure. Using the
*/
/*   set of residuals as input, the objective analysis procedure is invoke
d*/
/*    and the resultant field of gridded residuals is used to adjust the 
*/
/*   current grid of Cartesian estimates. This operation is repeated until
*/
/*    IP iterations have been performed or if the gross residual starts to
 */
/*    increase. Experience with this feature suggests a value for IP of */
/*    2 to 4. */

/*      The capability for 2-D filtering of the remapped Cartesian estimat
es*/
/*   using Leise's Multi-dimensional Telescoped Filter (see references) is
 also*/
/*   available. Each iteration of the filter will reduce the scales retain
ed*/
/*   by a factor of two. 2-D filtering of the Cartesian space estimates sh
ould*/
/*   NOT be necessary if all preceeding computational steps have been prop
erly*/
/*    parameterized. */

/*      A final option for throwing out "unbounded" gridded estimates beyo
nd*/
/*    a specified distance from original input reports is also available. 
*/
/*    This is done using two criteria: */
/*          1) Gridded estimate is first tested to see if it is "bounded";
 */
/*            the user specifies the minimum acceptable number of quadrant
s*/
/*            in which original input reports must be present with respect
*/
/*             to the (X,Y) grid location. */
/*          2) If the gridded estimate IS bounded, it is retained. If */
/*             it is NOT bounded, then an additional check is performed to
 */
/*             see if it is within a user-specified distance of an input 
*/
/*             report. If it fails that test, the gridded estimate is */
/*             replaced with the missing data flag. */

/*       A "backdoor" also exists for relocating each input report to */
/*    the nearest grid location in the output array. See the description 
*/
/*    of the variable called IP if you wish to invoke this option. */


/*    Restrictions: */

/*    -- All spatial variables must be normalized to the */
/*       index space of the output grid by the calling program. */
/*    -- Grid spacing along the X and Y axes must be equal. */
/*    -- Missing data will be represented by the variable BAD */
/*       (on both input and output). */


/*    Variables: */

/*      Z- (OUTPUT)  Two dimensional array of gridded estimates; */
/*                   dimensioned (ISIZE x JSIZE) */
/*  ISIZE- ( INPUT)  X-dimension of Z array */
/*  JSIZE- ( INPUT)  Y-dimension of Z array */
/*   XSTA- ( INPUT)  One dimensional array of input report X-locations */
/*                   -- normalized to INDEX space (in GRID units); */
/*                   dimensioned to NSTA */
/*   YSTA- ( INPUT)  One dimensional array of input report Y-locations */
/*                   -- normalized to INDEX space (in GRID units); */
/*                   dimensioned to NSTA */
/*   DATA- ( INPUT)  One dimensional array of input report values; */
/*                   dimensioned to NSTA */
/*     DZ- (SCRATCH) One dimensional array; dimensioned to NSTA */
/*    DZR- (SCRATCH) One dimensional array; dimensioned to (ISIZE x JSIZE)
 */
/*   NSTA- ( INPUT)  Dimension of 1-D arrays described above; */
/*                   corresponds to the number of irregularly spaced */
/*                   input reports to be remapped to the Z grid. */
/*    IP- ( INPUT)  Number of passes. The first pass of the remapping gene
rates*/
/*                   an estimate at each grid location using the distance 
*/
/*                   weighting procedure described above. If IP is greater
 */
/*                  than 1, the residual between each original data value 
and*/
/*                   the nearest Cartesian field estimate is computed */
/*                  immediately after the first pass of the objective anal
ysis.*/
/*                  Using the residuals as input, the analysis is performe
d*/
/*                   again and adjustments are made to the current grid of
 */
/*                  Cartesian estimates. This iterative improvement proced
ure*/
/*                   is performed IP-1 times. Suggest a value from 2 to 4.
 */
/*                   --- Special backdoor for choosing closest point */
/*                       instead of the objective analysis procedure: */
/*                          If IP is less than or equal to 0; each input 
*/
/*                          report will be mapped to the nearest grid */
/*                          location in the output array. All other */
/*                          objective analysis parameterization will be */
/*                          ignored. */
/*      R- ( INPUT)  Radius of "influence". Distance in GRID units */
/*                   at which the exponential weighting function = 0.1 */
/*                   Weights are assigned to each input datum */
/*                  according to it's distance from the destination Cartes
ian*/
/*                   location in the 2-D grid. The final estimate at the 
*/
/*                  Cartesian location is computed by multiplying each inp
ut*/
/*                  datum by the weight, summing the results and normalizi
ng*/
/*                  by the sum of all the weights. The weight W assigned t
o any*/
/*                  datum at distance D from the destination Cartesian gri
d*/
/*                   location is given by: */
/*                       W = EXP ( ln(0.1) * D**2 / R**2 ) */
/*                  and is in the range 0 to 1. Suggest a value approximat
ely*/
/*                   equal to the coarsest spacing of the input reports. 
*/
/*   RMX- ( INPUT)  Maximum distance in GRID units to extrapolate estimate
s.*/
/*                  Unbounded grid locations (as determined by NQD - below
)*/
/*                   beyond this distance from an input report will be set
 */
/*                  to the missing data flag just prior to procedure exit.
*/
/*    NQD- ( INPUT)  Gridded array locations with original input reports 
*/
/*                  contained in fewer than NQD quadrants around the locat
ion*/
/*                  will be identified as unbounded and subject to decimat
ion*/
/*                   based upon the value of RMX - above. Suggest a value 
*/
/*                   of 3 or 4 if you wish to utilize this feature. */
/*  NLFLT- ( INPUT)  Number of 2-D Leise filtering steps to perform on */
/*                   the gridded estimates. Each step reduces the retained
 */
/*                   scale sizes by a factor of 2. Suggest NOT using this 
*/
/*                   feature; increase the value of R instead to produce 
*/
/*                   a smoother result. */
/*    BAD- ( INPUT)  Missing data flag. Missing data in the input report 
*/
/*                   array (DATA) should be represented by this value. */
/*                  Missing data in the 2-D output array (Z) as determined
*/
/*                   by decimation criteria will be set to this as well. 
*/



/*        Transfer function is designed to perform gaussian weighting */
/*           with 0.1 assigned at radius R (2.3025 is -ln(0.1) ). */
/*           CHNG can be used to alter the response as follows: */
/*                    CHNG.LT.1 (Faster rolloff) or */
/*                    CHNG.GT.1 (Slower rolloff) . */

    /* Parameter adjustments */
    dzr_dim1 = *isize;
    dzr_offset = dzr_dim1 + 1;
    dzr -= dzr_offset;
    z_dim1 = *isize;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --xsta;
    --ysta;
    --data;
    --dz;

    /* Function Body */

/* ...Use average station value as first guess -- hardwired. */


/* ...Set RDSPDX to R -- disables the adaptive radius scheme. */

    rdspdx = *r__;
/* cc - 2/28/92 */
/* ... activate adaptive adjustment of the radius of influence. */
/*     It preserves the scales of the original when reports are */
/*     non-uniformly spaced.  (The resultant gridded field is also */
/*     a noisy mess requiring post-filtering.) */

/*      RDSPDX = R+4. */

    rfac = log((float).1) / chng;
    rms = big;

/* ...Check if closest point option is enabled (IP.LE.0) */

    if (*ip <= 0) {
	i__1 = *isize * *jsize;
	confld_(&z__[z_offset], &i__1, bad);
	i__1 = *isize * *jsize;
	confld_(&dzr[dzr_offset], &i__1, &big);
	i__1 = *nsta;
	for (m = 1; m <= i__1; ++m) {
	    if (data[m] == *bad) {
		goto L30;
	    }
	    i__ = i_nint(&xsta[m]);
	    j = i_nint(&ysta[m]);
	    if (i__ < 1 || i__ > *isize) {
		goto L30;
	    }
	    if (j < 1 || j > *jsize) {
		goto L30;
	    }
/* Computing 2nd power */
	    r__1 = xsta[m] - i__;
/* Computing 2nd power */
	    r__2 = ysta[m] - j;
	    r2 = r__1 * r__1 + r__2 * r__2;
	    if (r2 > dzr[i__ + j * dzr_dim1]) {
		goto L30;
	    }
	    dzr[i__ + j * dzr_dim1] = r2;
	    z__[i__ + j * z_dim1] = data[m];
L30:
	    ;
	}
	goto L90;
    }

/* ...Proceed with regular objective analysis */


/* ...Initialize radius array to the primary radius */

    rt2 = *r__ * (float)2.;
    rreg = (float)1. / (*r__ * *r__);
    i__1 = *isize * *jsize;
    confld_(&dzr[dzr_offset], &i__1, &rreg);

    if (rdspdx > *r__) {

/*...Adaptive radius technique -- determine the radius at each grid lo
cation*/

	i__1 = *jsize;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *isize;
	    for (i__ = 1; i__ <= i__2; ++i__) {

		y = (real) j;
		x = (real) i__;


/* ......Compute a radius depending upon the data density */

		for (l = 1; l <= 4; ++l) {
		    rquad[l - 1] = big;
/* L40: */
		}

		i__3 = *nsta;
		for (m = 1; m <= i__3; ++m) {
		    if (data[m] == *bad) {
			goto L45;
		    }
		    xof = xsta[m] - x;
		    yof = ysta[m] - y;
		    ind = r_sign(&c_b7, &xof) + r_sign(&c_b8, &yof) + (float)
			    3.;
		    rdn = sqrt(xof * xof + yof * yof);
/* Computing MIN */
		    r__1 = rdn, r__2 = rquad[ind - 1];
		    rquad[ind - 1] = dmin(r__1,r__2);
L45:
		    ;
		}
		rdn = (float)-1.;
		for (l = 1; l <= 4; ++l) {

/* .........Select the max distance to any quadrant (may b
e less than 4) */

		    if (rquad[l - 1] == big) {
			goto L46;
		    }
/* Computing MAX */
		    r__1 = rdn, r__2 = rquad[l - 1];
		    rdn = dmax(r__1,r__2);
L46:
		    ;
		}
		rdn = dmin(rdn,rdspdx);
		rdn = dmax(rdn,*r__);

		dzr[i__ + j * dzr_dim1] = (float)1. / (rdn * rdn);

/* L50: */
	    }
	}

    }

/* ...Guess at initial field -- IG=1, use average station value */
/*                            IG=2, provided by calling routine in Z array
*/
/* ...Hardwired to 1 (7/89 -- cgm) */

    switch ((int)(ig)) {
	case 1:  goto L1;
	case 2:  goto L2;
    }
L1:
    i__2 = *isize;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = *jsize;
	for (j = 1; j <= i__1; ++j) {
	    z__[i__ + j * z_dim1] = (float)0.;
/* L3: */
	}
    }

/* ...Perform IP passes -- main loop of analysis */

L2:

    i__1 = *ip;
    for (ipass = 1; ipass <= i__1; ++ipass) {
	oldrms = rms;
	rms = (float)0.;
	nrms = 0;

/* ...If average of data is used as first guess and currently on first
 */
/*      pass, use all data points. */

	if (ipass > 1 || ig > 1) {
	    goto L6;
	}

	i__2 = *nsta;
	for (ista = 1; ista <= i__2; ++ista) {
	    dz[ista] = *bad;
	    if (data[ista] == *bad) {
		goto L10;
	    }
	    dz[ista] = data[ista];
/* Computing 2nd power */
	    r__1 = data[ista];
	    rms += r__1 * r__1;
	    ++nrms;
L10:
	    ;
	}

	goto L11;

/* ...Find correction factors. */

L6:

	i__2 = *nsta;
	for (ista = 1; ista <= i__2; ++ista) {
	    dz[ista] = *bad;
	    if (data[ista] == *bad) {
		goto L5;
	    }
	    i__ = xsta[ista];
	    j = ysta[ista];

/* ...If station is outside of the specified grid, can't generate 
a */
/*      corrector estimate for it. */

	    if (i__ <= 0 || j <= 0) {
		goto L5;
	    }
	    if (i__ >= *isize || j >= *jsize) {
		goto L5;
	    }

/* ......Linear interpolation to estimate station location values 
*/

	    w1 = xsta[ista] - i__;
	    w2 = ysta[ista] - j;
	    w3 = (float)1. - w1;
	    w4 = (float)1. - w2;
	    dz[ista] = data[ista] - ((z__[i__ + j * z_dim1] * w3 + z__[i__ + 
		    1 + j * z_dim1] * w1) * w4 + (z__[i__ + (j + 1) * z_dim1] 
		    * w3 + z__[i__ + 1 + (j + 1) * z_dim1] * w1) * w2);
/* Computing 2nd power */
	    r__1 = dz[ista];
	    rms += r__1 * r__1;
	    ++nrms;
L5:
	    ;
	}

L11:

/* ...Check to verify continued convergence */

	if (nrms <= 0) {
#ifdef FIO
	    s_wsfe(&io___29);
	    e_wsfe();
#endif
	    goto L65;
	}

	rms = sqrt(rms / nrms);
	if (oldrms >= rms) {
	    goto L12;
	}
#ifdef FIO
	s_wsfe(&io___30);
	do_fio(&c__1, (char *)&ipass, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&(*ip), (ftnlen)sizeof(integer));
	e_wsfe();
#endif
	goto L65;

/* ...Still converging. On a grid point by grid point basis, compute 
*/
/*         weights, calculate net corrections and apply them. */
/*         The weights are recalculated on each pass to save space. */

L12:

/* ...Initiate the objective analysis procedure for this pass. */

	i__2 = *isize;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = *jsize;
	    for (j = 1; j <= i__3; ++j) {
		sumw = (float)0.;
		sum = (float)0.;
		i__4 = *nsta;
		for (ista = 1; ista <= i__4; ++ista) {
		    if (xsta[ista] != *bad && ysta[ista] != *bad) {
/* Computing 2nd power */
			r__1 = xsta[ista] - i__;
/* Computing 2nd power */
			r__2 = ysta[ista] - j;
			r2 = r__1 * r__1 + r__2 * r__2;
		    } else {
			r2 = big;
		    }

/* ...Check for underflow.  If no stations are within dece
nt range, */
/*         this scheme will give a simple arithmetic avera
ge. */

/* Computing MAX */
		    r__1 = r2 * rfac * dzr[i__ + j * dzr_dim1];
		    w = dmax(r__1,(float)-20.);
		    if (dz[ista] != *bad) {
			w = exp(w);
		    } else {
			w = (float)0.;
		    }

		    sum += w * dz[ista];
		    sumw += w;
/* L8: */
		}

		z__[i__ + j * z_dim1] += sum / sumw;

/* L7: */
	    }
	}

/* L4: */
    }

L65:

/* write the grided data to a file */

/*      do 141,js=1,jsize */
/*       do 141 is=1,isize */
/*            write(23,'(2i4,f9.3)') is,js,z(is,js) */
/*  41    format(' i,j,z ',2i5,f9.4) */
/* 141  continue */

/* ..Analysis completed- check for filtering/decimation options. */

    if (*nlflt > 0) {

/* ......Leise filtering of the output field. */

	t5fltr_(&z__[z_offset], isize, jsize, &c__1, nlflt);
    }

    if (*rmx <= (float)0. && *nqd <= 0) {
	goto L90;
    }

/* ......Decimation of distant unbounded grid locations. */

    rmxsq = *rmx * *rmx;
    if (*rmx <= (float)0.) {
	rmxsq = big;
    }
    i__1 = *jsize;
    for (j = 1; j <= i__1; ++j) {
	i__3 = *isize;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    for (l = 1; l <= 4; ++l) {
		iquad[l - 1] = 0;
/* L66: */
	    }
	    kntr = 0;
	    i__2 = *nsta;
	    for (ista = 1; ista <= i__2; ++ista) {
		if (data[ista] != *bad) {
		    xof = xsta[ista] - i__;
		    yof = ysta[ista] - j;
		    ind = r_sign(&c_b7, &xof) + r_sign(&c_b8, &yof) + (float)
			    3.;
		    ++iquad[ind - 1];
		    r2 = xof * xof + yof * yof;
		    if (r2 <= rmxsq) {
			++kntr;
		    }
		}
/* L70: */
	    }
	    if (*nqd > 0) {
		kntqd = 0;
		for (l = 1; l <= 4; ++l) {
		    if (iquad[l - 1] > 0) {
			++kntqd;
		    }
/* L71: */
		}
		if (kntqd >= *nqd) {
		    goto L75;
		}
	    }
	    if (kntr > 0) {
		goto L75;
	    }
	    z__[i__ + j * z_dim1] = *bad;
L75:
	    ;
	}
/* L80: */
    }

L90:

    return 0;
} /* bints_ */

/* Subroutine */ int confld_(rbuf, nplane, con)
real *rbuf;
integer *nplane;
real *con;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;


/*        FILLS AN ARRAY WITH A CONSTANT VALUE- CON */

    /* Parameter adjustments */
    --rbuf;

    /* Function Body */
    i__1 = *nplane;
    for (i__ = 1; i__ <= i__1; ++i__) {
	rbuf[i__] = *con;
/* L10: */
    }
    return 0;
} /* confld_ */

/* Subroutine */ int t5fltr_(y, n1, n2, n3, nstep)
real *y;
integer *n1, *n2, *n3, *nstep;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;

    /* Local variables */
    static integer main, ndim, kord[5], i__, j, k, m, n;
    static real ysave;
    static integer mstep, k1, m1, m2, m3, istop, jstop, kstop, kstrt, mstop, 
	    mstrt, kn, ln, mpyrmd;
    static real ym1, ym2;
    static integer net[5], nns[3];
    static real ykn, yln, ykn1, yln1;

/*                                               JIM LEISE 8/80 */
/*    ***************************************************************** */
/*    HELLO, */
/*    I AM A MULTIDIMENSIONAL LOW-PASS FILTER WHICH NEEDS NO EXTRA */
/*    ARRAY SPACE.  THUS, THE FILTERED ANSWER IS RETURNED IN THE SAME */
/*    ARRAY Y(N1,N2,N3) THAT THE DATA IS INPUT.  THE CENTRAL FILTER */
/*    IS A LINEAR 5-PT FILTER AND THE BOUNDARY FILTER IS COMPUTED */
/*    USING A MIRROR EXTENSION.  THUS, THE TOTAL FILTER IS LINEAR. */

/*          ********** NSTEP CONTROL FOR 1-DIM ********** */
/*        STEP RESTRICTION:  5*2**(NSTEP-1) .LE. MAX(N1,N2,N3) */
/*         PASSBAND .LE. 2**(NSTEP+2)  POINTS/CYCLE */
/*         STOPBAND .GE. 2**(NSTEP)  POINTS/CYCLE. */

/*          ********** MULTIDIMENSIONAL USE ********** */
/*    PARAMETER CONTROL FOR THE THREE DIMENSIONS CAN BE REALIZED */
/*    VIA COMMON/FLTRPL/ WHERE NS CORRESPONDS TO NSTEP.  IF THIS */
/*    COMMON IS NOT USED, THE VALUES OF NS ARE DEFAULTED TO NSTEP */
/*    -I.E. NSTEP IS USED IN PLACE OF ANY ZEROS. */
/*    ****************************************************************** 
*/

/*    INITIALIZATION OF NS FOR CSD APPLICATIONS (4/15/82) */
    /* Parameter adjustments */
    --y;

    /* Function Body */

/*    INITIALIZE THE 3-D ARITHMETIC. */
    ndim = 1;
    if (*n2 > 1) {
	ndim = 2;
    }
    if (*n3 > 1) {
	ndim = 3;
    }
    kord[0] = max(1,*n1);
    kord[1] = max(1,*n2);
    kord[2] = max(1,*n3);
    kord[3] = kord[0];
    kord[4] = kord[1];
    net[0] = 1;
    net[1] = kord[0];
    net[2] = kord[0] * kord[1];
    net[3] = net[0];
    net[4] = net[1];

/*    DEFAULT PARAMETER TRANSFER. */
    mpyrmd = 0;
    i__1 = ndim;
    for (n = 1; n <= i__1; ++n) {
	nns[n - 1] = fltrpl_1.ns[n - 1];
	if (fltrpl_1.ns[n - 1] == 0) {
	    nns[n - 1] = *nstep;
	}
	if (kord[n - 1] < 5) {
	    nns[n - 1] = 0;
	}
/* L10: */
/* Computing MAX */
	i__2 = mpyrmd, i__3 = nns[n - 1] + nns[n - 1] - 1;
	mpyrmd = max(i__2,i__3);
    }
    if (mpyrmd <= 0) {
	return 0;
    }
    mstep = (mpyrmd + 1) / 2;

/*    ***** START THE MAIN LOOP ***** */
    k1 = 1;
    i__2 = mpyrmd;
    for (main = 1; main <= i__2; ++main) {
	i__3 = ndim;
	for (n = 1; n <= i__3; ++n) {
/*    SAMPLING CHECKS. */
	    if (k1 * 10 > kord[n - 1]) {
/* Computing MIN */
		i__1 = nns[n - 1];
		nns[n - 1] = min(i__1,main);
	    }
	    if (main >= nns[n - 1] && mpyrmd - main >= nns[n - 1]) {
		goto L40;
	    }

/*    THE 3-D ARITHMETIC. */
	    m1 = k1 * net[n - 1];
	    m2 = m1 + m1;
	    m3 = m2 + m1;
	    istop = kord[n];
	    jstop = kord[n + 1];
	    i__1 = istop;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		i__4 = jstop;
		for (j = 1; j <= i__4; ++j) {
		    kstrt = (i__ - 1) * net[n] + 1 + (j - 1) * net[n + 1];
		    kstop = kstrt + (kord[n - 1] - 1) * net[n - 1];
		    kn = kstrt - net[n - 1];
		    i__5 = k1;
		    for (k = 1; k <= i__5; ++k) {
			kn += net[n - 1];
			ln = kn + (kstop - kn) / m1 * m1;

/*    FILTER THE ENDS USING A MIRROR EXTENSION. */
			ykn = y[kn] * (float).875 + y[kn + m1] * (float).1875 
				- y[kn + m2] * (float).0625;
			yln = y[ln] * (float).875 + y[ln - m1] * (float).1875 
				- y[ln - m2] * (float).0625;
			ykn1 = y[kn] * (float).1875 + y[kn + m1] * (float)
				.625 + y[kn + m2] * (float).25 - y[kn + m3] * 
				(float).0625;
			yln1 = y[ln] * (float).1875 + y[ln - m1] * (float)
				.625 + y[ln - m2] * (float).25 - y[ln - m3] * 
				(float).0625;

/*    DO THE CENTRAL 5-PT FILTER. */
			ym2 = y[kn];
			ym1 = y[kn + m1];
			mstrt = kn + m2;
			mstop = ln - m2;

			i__6 = mstop;
			i__7 = m1;
			for (m = mstrt; i__7 < 0 ? m >= i__6 : m <= i__6; m +=
				 i__7) {
			    ysave = y[m];
			    y[m] = y[m] * (float).625 + (ym1 + y[m + m1]) * (
				    float).25 - (ym2 + y[m + m2]) * (float)
				    .0625;
			    ym2 = ym1;
/* L20: */
			    ym1 = ysave;
			}

			y[kn + m1] = ykn1;
			y[ln - m1] = yln1;
			y[kn] = ykn;
			y[ln] = yln;

/* L30: */
		    }
		}
	    }
L40:
	    ;
	}
/*    UPDATE THE SAMPLING INCREMENT. */
	k1 += k1;
	if (main >= mstep) {
	    k1 /= 4;
	}
/* L50: */
    }

    return 0;
} /* t5fltr_ */

