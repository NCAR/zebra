/* rgrid.f -- translated by f2c (version 19960315). */

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer np, itrs, iused[800], itrngl[4800]	/* was [800][6] */, naprox, 
	    ibptno, ibpts, ibndry[800], nstns, nx, ny, ipar, ialt[800], nfltr;
    logical deleted[800];
    integer nusrno[800], idate, itime, ntplot, nstep, iparov, jw[625]	/* 
	    was [25][25] */;
    real x[800], y[800], flag__, xmax, xmin, ymax, ymin, x0, y0, dx, dy, 
	    ovrpam[800], area[800], u[800], v[800], z__[800], rlat[800], rlon[
	    800], weight[4800]	/* was [800][6] */, ruplim[200], rlolim[200];
} triagd_;

#define triagd_1 triagd_

struct {
    char fstrng[7], ovfld[7], name__[10];
} trgchr_;

#define trgchr_1 trgchr_

/* Table of constant values */

static integer c__1 = 1;
static integer c__6 = 6;
static integer c__5 = 5;
static integer c__4 = 4;
static integer c__0 = 0;
static integer c__2 = 2;
static integer c__3 = 3;
static real c_b229 = (float).01;

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced */
/*or used in any form or by any means -- graphic, electronic, or mechanical,*/
/* including photocopying, recording, taping, or information storage and */
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties of*/
/* any kind, either express or implied, including but not limited to the */
/*implied warranties of merchantibility and fitness for a particular purpose.
*/
/*UCAR does not indemnify any infringement of copyright, patent, or trademark
*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */


/* DO_RGRID Generic rectangular gridding routine */

/* ENTRY: */
/* 	grid	The grid to be filled */
/* 	xdim	x-dimension of the grid */
/* 	ydim	y-dimension of the grid */
/* 	nsta	number of raw data points */
/* 	rawdata	array of raw data values */
/* 	badflag	bad data flag */
/* 	xloc	array of x locations of the raw data points */
/* 	yloc	array of y locations of the raw data points */
/* 	xlo	x value of the left side of the grid */
/* 	ylo	y value of the bottom of the grid */
/* 	xhi	x value of the right side of the grid */
/* 	yhi	y value of the top of the grid */
/* 	scratch	scratch array at least (xdim x ydim) in size */
/* EXIT: */
/* 	0 is returned and the grid has been filled with data */
/* 	interpolated to the grid locations */
/* 		--OR-- */
/* 	A status other than 0 is returned and the grid is unchanged. */
/* 	Possible status values are in rg_status.h */

integer dorgrid_(grid, xdim, ydim, npts, rawdata, badflag, xloc, yloc, xlo, 
	ylo, xhi, yhi, scratch)
real *grid;
integer *xdim, *ydim, *npts;
real *rawdata, *badflag, *xloc, *yloc, *xlo, *ylo, *xhi, *yhi, *scratch;
{
    /* System generated locals */
    integer grid_dim1, grid_offset, scratch_dim1, scratch_offset, ret_val, 
	    i__1;

    /* Local variables */
    static integer i__, index;
    static logical dotri;
    extern /* Subroutine */ int compx_(), wghts_(), stats_(), triang_(), 
	    approx_();
    static integer status;
    static logical del;
    static real xin[16800]	/* was [21][800] */;

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* If the number of points, x-dimension, or y-dimension changed */
/* we need to triangulate again */

    /* Parameter adjustments */
    scratch_dim1 = *xdim;
    scratch_offset = scratch_dim1 + 1;
    scratch -= scratch_offset;
    grid_dim1 = *xdim;
    grid_offset = grid_dim1 + 1;
    grid -= grid_offset;
    --yloc;
    --xloc;
    --rawdata;

    /* Function Body */
    dotri = *npts != triagd_1.np || *xdim != triagd_1.nx || *ydim != 
	    triagd_1.ny;

/* Initialize some variables in the common block */

    triagd_1.nx = *xdim;
    triagd_1.ny = *ydim;
    triagd_1.np = *npts;
    triagd_1.xmin = *xlo;
    triagd_1.xmax = *xhi;
    triagd_1.ymin = *ylo;
    triagd_1.ymax = *yhi;
    triagd_1.flag__ = *badflag;
    triagd_1.ipar = 0;
    triagd_1.iparov = 0;
    i__1 = *npts;
    for (i__ = 1; i__ <= i__1; ++i__) {

/* 	We have to triangulate again if locations changed */

	dotri = dotri || xloc[i__] != triagd_1.x[i__ - 1];
	dotri = dotri || yloc[i__] != triagd_1.y[i__ - 1];

/* 	Put the locations in the X and Y arrays */

	triagd_1.x[i__ - 1] = xloc[i__];
	triagd_1.y[i__ - 1] = yloc[i__];

/* 	Is this a good point?  We have to triangulate again if */
/* 	the deleted status of the point changed. */

	del = rawdata[i__] == *badflag;
	dotri = dotri || del != triagd_1.deleted[i__ - 1];
	triagd_1.deleted[i__ - 1] = del;
/* L10: */
    }

/* Use linear fit */

    triagd_1.naprox = 0;

/* Calculate spacing information and statistics */

    stats_(&status);
    if (status != 0) {
	ret_val = status;
	return ret_val;
    }

/* Do the triangulation and the weighting, if necessary */

    if (dotri) {
	triang_();
	wghts_();
    }

/* Set up for the polynomial fit if we're using it */

    if (triagd_1.naprox == 1) {
	compx_(xin, &index);
    }

/* Actually fill in the grid */

    approx_(&rawdata[1], &grid[grid_offset], &scratch[scratch_offset], &c__1, 
	    xin, &index);
    ret_val = 0;
    return ret_val;
} /* dorgrid_ */

/* Subroutine */ int triang_()
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static real v2cv1, xmid, ymid;
    extern /* Subroutine */ int cont_();
    static integer ibpt1, ibpt2, i__, j, k, index, nrmvd;
    extern /* Subroutine */ int dsort_();
    static real v1;
    static integer itrsx;
    static real v2;
    extern /* Subroutine */ int cclock_(), clockw_(), orient_();
    static real distmn;
    extern /* Subroutine */ int lawson_();
    static real distsq;
    static integer ip1;
    static real v1x, v1y, v2x, v2y;
    static integer ijk, ipt[3];

/* ROUTINE THAT DETERMINES THE TRIANGLES USED FOR THE INTERPOLATION. */
/* IBPTS = THE NUMBER OF POINTS THAT FORM THE OUTER BOUNDARY OF THE */
/* TRIANGULATION. */
/* IBNDRY(*) CONTAINS THE POINTS THAT FORM THE BOUNDARY.  IBNDRY(I) IS */
/* CLOCKWISE FROM IBNDRY(I+1). IBNDRY(IBPTS) IS CLOCKWISE FROM IBNDRY(1). 
*/
/* ITRS = THE NUMBER OF TRIANGLES. */
/* ITRNGL(*) CONTAINS INFORMATION ABOUT THE TRIANGULATION. */
/* ITRNGL(I,1-3) = THE THREE POINTS THAT FORM TRIANGLE I. */
/* THE THREE POINTS ARE ORIENTED COUNTERCLOCKWISE, I.E. ITRNGL(I,1) */
/*IS CLOCKWISE FROM ITRNGL(I,2), ITRNGL(I,2) IS CLOCKWISE FROM ITRNGL(I,3)
*/
/* AND ITRNLG(I,3) IS CLOCKWISE FROM ITRNGL(I,1). */
/* ITRNGL(I,4) = THE TRIANGLE THAT SHARES THE EDGE FORMED BY ITRNGL(I,1) 
*/
/* AND ITRNGL(I,2). */
/* ITRNGL(I,5) = THE TRIANGLE THAT SHARES THE EDGE FORMED BY ITRNGL(I,2) 
*/
/* AND ITRNGL(I,3). */
/* ITRNGL(I,6) = THE TRIANGLE THAT SHARES THE EDGE FORMED BY ITRNGL(I,1) 
*/
/* AND ITRNGL(I,3). */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/*     INITIALIZATION. */

    triagd_1.itrs = 0;
    triagd_1.ibpts = 0;
    triagd_1.ibptno = 0;
    for (i__ = 1; i__ <= 3; ++i__) {
/* L5: */
	ipt[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 800; ++i__) {
/* L10: */
	triagd_1.ibndry[i__ - 1] = 0;
    }
    for (j = 1; j <= 6; ++j) {
	for (i__ = 1; i__ <= 800; ++i__) {
	    triagd_1.itrngl[i__ + j * 800 - 801] = 0;
/* L15: */
	}
    }

/*     TEST THE DATA. */

/* DEGENERATE CASE: ONLY THREE STATIONS. */
    if (triagd_1.nstns == 3) {
/* GET THE THREE STATIONS. */
	ijk = 0;
	i__1 = triagd_1.np;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (! triagd_1.deleted[i__ - 1]) {
		++ijk;
		ipt[ijk - 1] = i__;
	    }
/* L35: */
	}
/* ORIENT THE THREE STATIONS COUNTERCLOCKWISE. */
	orient_(ipt, &ipt[1], &ipt[2], triagd_1.ibndry, &triagd_1.ibndry[1], &
		triagd_1.ibndry[2]);
/* ONE TRIANGLE AND THREE BOUNDARY POINTS. */
	triagd_1.itrs = 1;
	triagd_1.ibpts = 3;
	for (i__ = 1; i__ <= 3; ++i__) {
	    triagd_1.itrngl[i__ * 800 - 800] = triagd_1.ibndry[i__ - 1];
/* L45: */
	    triagd_1.itrngl[(i__ + 3) * 800 - 800] = 0;
	}
	goto L900;
    }

/*     FIND THE MIDPOINT OF THE RECTANGLE AND SORT THE PAM STATIONS */
/*     BY THERE DISTANCE FROM THE MIDPOINT. */

    dsort_();

/*     GET THE FIRST TRIANGLE. */

    ipt[0] = triagd_1.iused[0];
    ipt[1] = triagd_1.iused[1];
    ipt[2] = triagd_1.iused[2];
/* FIND THE COUNTERCLOCKWISE ORIENTATION OF THE POINTS IPT(*). */
    orient_(ipt, &ipt[1], &ipt[2], triagd_1.ibndry, &triagd_1.ibndry[1], &
	    triagd_1.ibndry[2]);
/* NO. OF POINTS THAT FORM THE BOUNDARY. */
    triagd_1.ibpts = 3;
/* FIRST TRIANGLE. */
    triagd_1.itrs = 1;
    triagd_1.itrngl[triagd_1.itrs - 1] = triagd_1.ibndry[0];
    triagd_1.itrngl[triagd_1.itrs + 799] = triagd_1.ibndry[1];
    triagd_1.itrngl[triagd_1.itrs + 1599] = triagd_1.ibndry[2];
    i__1 = triagd_1.nstns - 3;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* GET NEXT POINT THAT IS CLOSEST TO MIDPOINT OF CLOSEST TWO POINTS. 
*/
	ipt[2] = triagd_1.iused[i__ + 2];
/* FIND BOUNDARY SEGMENT THAT IS CLOSEST TO IPT(3). */
	distmn = (float)1e30;
	i__2 = triagd_1.ibpts;
	for (j = 1; j <= i__2; ++j) {
	    ipt[0] = triagd_1.ibndry[j - 1];
	    if (j < triagd_1.ibpts) {
		ipt[1] = triagd_1.ibndry[j];
		ip1 = j + 1;
	    } else {
		ipt[1] = triagd_1.ibndry[0];
		ip1 = 1;
	    }
/* DON'T CONSIDER JTH BOUNDARY SEGMENT IF IT IS HIDDEN FROM IPT(3)
. */
/* VECTOR FROM IPT(1) TO IPT(2) - BOUNDARY SEGMENT. */
	    v1x = triagd_1.x[ipt[1] - 1] - triagd_1.x[ipt[0] - 1];
	    v1y = triagd_1.y[ipt[1] - 1] - triagd_1.y[ipt[0] - 1];
/* VECTOR FROM IPT(1) TO IPT(3). */
	    v2x = triagd_1.x[ipt[2] - 1] - triagd_1.x[ipt[0] - 1];
	    v2y = triagd_1.y[ipt[2] - 1] - triagd_1.y[ipt[0] - 1];
/* CROSS PRODUCT V2 X V1. */
	    v2cv1 = v2x * v1y - v2y * v1x;
	    if (v2cv1 > (float)0.) {
/* MIDPOINT OF BOUNDARY SEGMENT. */
		xmid = triagd_1.x[ipt[0] - 1] + (triagd_1.x[ipt[1] - 1] - 
			triagd_1.x[ipt[0] - 1]) / (float)2.;
		ymid = triagd_1.y[ipt[0] - 1] + (triagd_1.y[ipt[1] - 1] - 
			triagd_1.y[ipt[0] - 1]) / (float)2.;
/* DISTANCE TO MIDPOINT. */
/* Computing 2nd power */
		r__1 = triagd_1.x[ipt[2] - 1] - xmid;
/* Computing 2nd power */
		r__2 = triagd_1.y[ipt[2] - 1] - ymid;
		distsq = r__1 * r__1 + r__2 * r__2;
		if (distsq < distmn) {
		    distmn = distsq;
		    ibpt1 = ipt[0];
		    ibpt2 = ipt[1];
		    triagd_1.ibptno = ip1;
		}
	    }
/* L50: */
	}
/* NEW TRIANGLE. */
	++triagd_1.itrs;
	triagd_1.itrngl[triagd_1.itrs - 1] = ibpt1;
	triagd_1.itrngl[triagd_1.itrs + 799] = ipt[2];
	triagd_1.itrngl[triagd_1.itrs + 1599] = ibpt2;
	cont_(&ibpt1, &ibpt2, &c__6, &triagd_1.itrs);
/* LAWSON'S DIAGONAL EXCHANGE ON ALL QUADRALATERALS FORMED BY ITRS AND
 */
/* NEIGHBORING TRIANGLES. */
	for (j = 4; j <= 6; ++j) {
	    if (triagd_1.itrngl[triagd_1.itrs + j * 800 - 801] != 0) {
		itrsx = triagd_1.itrngl[triagd_1.itrs + j * 800 - 801];
		lawson_(&triagd_1.itrs, &itrsx);
	    }
/* L55: */
	}
/*ADD THE NEW BOUNDARY PT. BETWEEN THE POINTS CLOCKWISE AND COUNTERCLO
CKWISE.*/
	i__2 = triagd_1.ibptno;
	for (j = triagd_1.ibpts; j >= i__2; --j) {
	    triagd_1.ibndry[j] = triagd_1.ibndry[j - 1];
/* L60: */
	}
	++triagd_1.ibpts;
	triagd_1.ibndry[triagd_1.ibptno - 1] = ipt[2];

/*     SEE IF NEED TO ADD TRIANGLES CLOCKWISE FROM THE NEW BOUNDARY PO
INT. */

/* ADD UP TO 3 NEW TRIANGLES. */
	for (j = 1; j <= 3; ++j) {
	    clockw_(&ipt[2], &nrmvd);
	    if (nrmvd == 0) {
		goto L77;
	    }
/* L75: */
	}

/*     SEE IF ADD TRIANGLES COUNTERCLOCKWISE FROM IPT(3). */

/* ADD UP TO 3 NEW TRIANGLES. */
L77:
	for (j = 1; j <= 3; ++j) {
	    cclock_(&ipt[2], &nrmvd);
	    if (nrmvd == 0) {
		goto L100;
	    }
/* L85: */
	}
L100:
	;
    }

/*     TRY TO OPTIMIZE THE TRIANGULATION. */

/* SKIP OPTIMIZATION IF USE POLYNOMIAL CURVE-FIT AND NOT PLOTTING */
/* EDDY FIELD.  ONLY WANT CONVEX HULL */
    if (triagd_1.naprox == 1 && triagd_1.ipar != 61 && triagd_1.iparov != 61) 
	    {
	goto L900;
    }
/* LAWSON'S DIAGONAL EXCHANGE ON EVERY PAIR OF ADJACENT TRIANGLES. */
    i__1 = triagd_1.itrs - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = triagd_1.itrs;
	for (j = i__ + 1; j <= i__2; ++j) {
/* SEE IF TRIANGLE I IS ADJACENT TO TRIANGLE J. */
	    index = 0;
	    for (k = 4; k <= 6; ++k) {
		if (triagd_1.itrngl[i__ + k * 800 - 801] == j) {
		    index = 1;
		}
/* L105: */
	    }
/* LAWSON'S DIAGONAL EXCHANGE IF ADJACENT. */
	    if (index == 1) {
		lawson_(&i__, &j);
	    }
/* L110: */
	}
    }

/*CHECK FOR SMALL TRIANGLES AND DO LAWSON'S DIAGONAL EXCHANGE ON ANY FOUND
.*/
    i__2 = triagd_1.itrs;
    for (i__ = 1; i__ <= i__2; ++i__) {
	ipt[0] = triagd_1.itrngl[i__ - 1];
	ipt[1] = triagd_1.itrngl[i__ + 799];
	ipt[2] = triagd_1.itrngl[i__ + 1599];
/* VECTOR FROM IP2 TO IP1. */
	v1x = triagd_1.x[ipt[0] - 1] - triagd_1.x[ipt[1] - 1];
	v1y = triagd_1.y[ipt[0] - 1] - triagd_1.y[ipt[1] - 1];
/* VECTOR FROM IP2 TO IP3. */
	v2x = triagd_1.x[ipt[2] - 1] - triagd_1.x[ipt[1] - 1];
	v2y = triagd_1.y[ipt[2] - 1] - triagd_1.y[ipt[1] - 1];
/* CROSS PRODUCT .5*(V2 X V1) = .5*V1*V2*SIN(ANGLE BETWEEN) = */
/* (AREA OF TRIANGLE I). */
	v2cv1 = (r__1 = v2x * v1y - v2y * v1x, dabs(r__1)) * (float).5;
/* Computing 2nd power */
	r__1 = v1x;
/* Computing 2nd power */
	r__2 = v1y;
	v1 = sqrt(r__1 * r__1 + r__2 * r__2);
/* Computing 2nd power */
	r__1 = v2x;
/* Computing 2nd power */
	r__2 = v2y;
	v2 = sqrt(r__1 * r__1 + r__2 * r__2);
/* V1*V2*.25 = .5*(AREA OF TRIANGLE FORMED BY POINTS OF TRIANGLE I */
/* IF V1 AND V2 ARE ORTHOGONAL).  IF AREA OF TRIANGLE I IS TOO SMALL, 
*/
/* TRY LAWSON'S DIAGONAL EXCHANGE. */
	if (v2cv1 <= v1 * v2 * (float).25) {
	    for (j = 4; j <= 6; ++j) {
		if (triagd_1.itrngl[i__ + j * 800 - 801] != 0) {
		    itrsx = triagd_1.itrngl[i__ + j * 800 - 801];
		    lawson_(&i__, &itrsx);
		}
/* L120: */
	    }
	}
/* L150: */
    }

L900:
    return 0;
} /* triang_ */

/* Subroutine */ int dsort_()
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1, r__2;

    /* Local variables */
    static real dist[800];
    static integer i__, j, k;
    static real dstsq;
    static integer ix;
    static real distnc, xmidpt, ymidpt;

/* ROUTINE TO FIND THE MIDPOINT OF THE RECTANGLE CONTAINING THE PAM */
/* STATIONS AND SORTING THE STATIONS BY DISTANCE FROM THE MIDPOINT. */
/* IUSED(1) = STATION NEAREST MIDPOINT. */
/* IUSED(NSTNS) = STATION FURTHEST FROM MIDPOINT. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/*     INITIALIZATION */

    for (i__ = 1; i__ <= 800; ++i__) {
	triagd_1.iused[i__ - 1] = 0;
/* L10: */
	dist[i__ - 1] = (float)0.;
    }

/* GET PAM STATION NEAREST TO RECTANGLE MIDPOINT. */
    distnc = (float)1e30;
    xmidpt = (triagd_1.xmax + triagd_1.xmin) / (float)2.;
    ymidpt = (triagd_1.ymax + triagd_1.ymin) / (float)2.;
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! triagd_1.deleted[i__ - 1]) {
/* Computing 2nd power */
	    r__1 = triagd_1.x[i__ - 1] - xmidpt;
/* Computing 2nd power */
	    r__2 = triagd_1.y[i__ - 1] - ymidpt;
	    dstsq = r__1 * r__1 + r__2 * r__2;
	    if (dstsq < distnc) {
		distnc = dstsq;
		triagd_1.iused[0] = i__;
	    }
	}
/* L20: */
    }

/* ONE STATION IN SORTED LIST. */
    ix = 1;
/* GET DISTANCE FROM IUSED(1) FOR OTHER STATIONS AND SORT IN ASCENDING */
/* ORDER OF DISTANCE FROM IUSED(1). */
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ != triagd_1.iused[0] && ! triagd_1.deleted[i__ - 1]) {
/* Computing 2nd power */
	    r__1 = triagd_1.x[i__ - 1] - triagd_1.x[triagd_1.iused[0] - 1];
/* Computing 2nd power */
	    r__2 = triagd_1.y[i__ - 1] - triagd_1.y[triagd_1.iused[0] - 1];
	    distnc = r__1 * r__1 + r__2 * r__2;
	    i__2 = ix;
	    for (j = 1; j <= i__2; ++j) {
		if (distnc < dist[j - 1]) {
		    i__3 = j;
		    for (k = ix; k >= i__3; --k) {
			triagd_1.iused[k] = triagd_1.iused[k - 1];
			dist[k] = dist[k - 1];
/* L80: */
		    }
		    ++ix;
		    triagd_1.iused[j - 1] = i__;
		    dist[j - 1] = distnc;
		    goto L100;
		}
/* L90: */
	    }
	    ++ix;
	    triagd_1.iused[ix - 1] = i__;
	    dist[ix - 1] = distnc;
	}
L100:
	;
    }

    return 0;
} /* dsort_ */

/* Subroutine */ int cont_(ipnt1, ipnt2, ilocat, itrian)
integer *ipnt1, *ipnt2, *ilocat, *itrian;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer iloc1, iloc2, i__, j, minloc, nghbrs, maxloc;

/* ROUTINE TO FIND THE TRIANGLES THAT SHARE THE EDGE FORMED BY POINTS */
/* IPNT1 AND IPNT2 OF TRIANGLE ITRIAN. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    nghbrs = 0;
    i__1 = triagd_1.itrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ != *itrian) {
/* SEE IF IPNT1 IS PART OF TRIANGLE I. */
	    for (j = 1; j <= 3; ++j) {
		if (triagd_1.itrngl[i__ + j * 800 - 801] == *ipnt1) {
		    goto L20;
		}
/* L10: */
	    }
/* IPNT1 NOT IN I.  GO TO NEXT TRIANGLE. */
	    goto L100;
/* IPNT1 IS PART OF I.  SEE IF IPNT2 IS ALSO PART OF I. */
L20:
	    iloc1 = j;
	    for (j = 1; j <= 3; ++j) {
		if (triagd_1.itrngl[i__ + j * 800 - 801] == *ipnt2) {
		    goto L40;
		}
/* L30: */
	    }
/* IPNT2 IS NOT PART OF I.  GO TO NEXT TRIANGLE. */
	    goto L100;
/* IPNT1 AND IPNT2 ARE BOTH IN I.  ADD ITRIAN TO THE LIST OF NEIGH
BOR */
/* TRIANGLES OF I. */
L40:
	    iloc2 = j;
	    ++nghbrs;
	    maxloc = max(iloc1,iloc2);
	    minloc = min(iloc1,iloc2);
	    if (maxloc == 2 && minloc == 1) {
		triagd_1.itrngl[i__ + 2399] = *itrian;
		goto L60;
	    }
	    if (maxloc == 3 && minloc == 2) {
		triagd_1.itrngl[i__ + 3199] = *itrian;
		goto L60;
	    }
	    if (maxloc == 3 && minloc == 1) {
		triagd_1.itrngl[i__ + 3999] = *itrian;
		goto L60;
	    }
/* ADD TRIANGLE I TO THE LIST OF NEIGHBOR TRIANGLES OF ITRIAN. */
L60:
	    triagd_1.itrngl[*itrian + *ilocat * 800 - 801] = i__;
/* QUIT IF HAVE FOUND THE TWO NEIGHBORS OF ITRIAN. */
/* L80: */
	    if (nghbrs == 2) {
		return 0;
	    }
	}
L100:
	;
    }
    return 0;
} /* cont_ */

/* Subroutine */ int lawson_(ntr1, ntr2)
integer *ntr1, *ntr2;
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static real v1cv2, w1cw2;
    extern /* Subroutine */ int cont_();
    static integer i__, j;
    static real snv1u1, snu1v2, snu1w2, snu1w1, snu2w1, snu2w2, snu2v1, 
	    snu2v2, u1, v1, v2, w1, w2, u2, rmntr1, rmntr2;
    static integer nshard;
    static real rmnalt;
    extern /* Subroutine */ int orient_();
    static real rmncur, rmntra, rmntrb;
    static integer notshr;
    static real u1x, v1x, v1y, v2x, v2y, w1x, w1y, w2x, w2y, u1y, u2x, u2y;
    static integer ipt[4], mpt[3];

/* ROUTINE THAT IMPLEMENTS LAWSON'S DIAGONAL EXCHANGE ON THE QUADRILATERAL
 */
/* FORMED BY NEIGHBORING TRIANGLES NTR1 AND NTR2. */
/* REFERENCE: "SOFTWARE FOR C1 SURFACE INTERPOLATION," C.L. LAWSON IN */
/* MATHEMATICAL SOFTWARE III, ED. JOHN RICE, 1977. */
/* IPT(1) AND IPT(2) ARE THE POINTS SHARED BY NTR1 AND NTR2.  IPT(3) IS */
/* THE UNSHARED POINT OF NTR1.  IPT(4) IS THE UNSHARED POINT OF NTR2. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* SKIP IF USE POLYNOMIAL CURVE-FIT AND NOT PLOTTING EDDY FIELD. */
/* ONLY INTERESTED IN CONVEX HULL.  NO NEED TO OPTIMIZE TRIANGULATION. */
    if (triagd_1.naprox == 1 && triagd_1.ipar != 61 && triagd_1.iparov != 61) 
	    {
	return 0;
    }
/* FIND THE SHARED POINTS. */
    nshard = 0;
    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    if (triagd_1.itrngl[*ntr1 + i__ * 800 - 801] == triagd_1.itrngl[*
		    ntr2 + j * 800 - 801]) {
		++nshard;
		ipt[nshard - 1] = triagd_1.itrngl[*ntr1 + i__ * 800 - 801];
		if (nshard == 2) {
		    goto L20;
		}
	    }
/* L10: */
	}
    }
/* FIND POINTS NOT SHARED. */
L20:
    for (i__ = 1; i__ <= 3; ++i__) {
/* TRIANGLE NTR1. */
	if (triagd_1.itrngl[*ntr1 + i__ * 800 - 801] != ipt[0] && 
		triagd_1.itrngl[*ntr1 + i__ * 800 - 801] != ipt[1]) {
	    ipt[2] = triagd_1.itrngl[*ntr1 + i__ * 800 - 801];
	}
/* TRIANGLE NTR2. */
	if (triagd_1.itrngl[*ntr2 + i__ * 800 - 801] != ipt[0] && 
		triagd_1.itrngl[*ntr2 + i__ * 800 - 801] != ipt[1]) {
	    ipt[3] = triagd_1.itrngl[*ntr2 + i__ * 800 - 801];
	}
/* L30: */
    }
/* ORIENT THE THREE POINTS FROM TRIANGLE NTR1 COUNTERCLOCKWISE. */
    orient_(ipt, &ipt[1], &ipt[2], mpt, &mpt[1], &mpt[2]);
/* SHUFFLE THE POINTS MPT(1-3) AND COPY THEM INTO IPT(*) SO THAT */
/* IPT(1) AND IPT(2) ARE THE POINTS SHARED BY NTR1 AND NTR2 AND IPT(3) */
/* IS THE POINT OF NTR1 THAT IS NOT SHARED.  IPT(1-3) WILL THEN BE THE */
/* POINTS OF NTR1 ORIENTED COUNTERCLOCKWISE. */
    for (i__ = 1; i__ <= 3; ++i__) {
/* FIND THE POINT OF NTR1 NOT SHARED. */
	if (mpt[i__ - 1] == ipt[2]) {
	    notshr = i__;
	}
/* L35: */
    }
    if (notshr == 1) {
	ipt[0] = mpt[1];
	ipt[1] = mpt[2];
	ipt[2] = mpt[0];
    }
    if (notshr == 2) {
	ipt[0] = mpt[2];
	ipt[1] = mpt[0];
	ipt[2] = mpt[1];
    }
    if (notshr == 3) {
	ipt[0] = mpt[0];
	ipt[1] = mpt[1];
	ipt[2] = mpt[2];
    }
/* TEST TO SEE IF QUADRALATERAL IS CONVEX. */
    v1x = triagd_1.x[ipt[2] - 1] - triagd_1.x[ipt[0] - 1];
    v1y = triagd_1.y[ipt[2] - 1] - triagd_1.y[ipt[0] - 1];
    v2x = triagd_1.x[ipt[0] - 1] - triagd_1.x[ipt[3] - 1];
    v2y = triagd_1.y[ipt[0] - 1] - triagd_1.y[ipt[3] - 1];
/* CROSS PRODUCT V1 X V2. */
    v1cv2 = v1x * v2y - v2x * v1y;
/* OTHER SIDE OF QUADRALATERAL. */
    w1x = triagd_1.x[ipt[2] - 1] - triagd_1.x[ipt[1] - 1];
    w1y = triagd_1.y[ipt[2] - 1] - triagd_1.y[ipt[1] - 1];
    w2x = triagd_1.x[ipt[1] - 1] - triagd_1.x[ipt[3] - 1];
    w2y = triagd_1.y[ipt[1] - 1] - triagd_1.y[ipt[3] - 1];
/* CROSS PRODUCT W1 X W2. */
    w1cw2 = w1x * w2y - w1y * w2x;
/* IF QUADRALATERAL IS CONVEX, CROSS PRODUCTS WILL HAVE OPPOSITE SIGNS. */
/* QUIT IF THEY DON'T. */
    if (v1cv2 * w1cw2 >= (float)0.) {
	return 0;
    }
/* QUADRALATERAL IS CONVEX.  GET THE CURRENT DIAGONAL U1. */
    u1x = triagd_1.x[ipt[0] - 1] - triagd_1.x[ipt[1] - 1];
    u1y = triagd_1.y[ipt[0] - 1] - triagd_1.y[ipt[1] - 1];
/* ABSOLUTE VALUES OF ALL VECTORS. */
/* Computing 2nd power */
    r__1 = u1x;
/* Computing 2nd power */
    r__2 = u1y;
    u1 = sqrt(r__1 * r__1 + r__2 * r__2);
/* Computing 2nd power */
    r__1 = v1x;
/* Computing 2nd power */
    r__2 = v1y;
    v1 = sqrt(r__1 * r__1 + r__2 * r__2);
/* Computing 2nd power */
    r__1 = v2x;
/* Computing 2nd power */
    r__2 = v2y;
    v2 = sqrt(r__1 * r__1 + r__2 * r__2);
/* Computing 2nd power */
    r__1 = w1x;
/* Computing 2nd power */
    r__2 = w1y;
    w1 = sqrt(r__1 * r__1 + r__2 * r__2);
/* Computing 2nd power */
    r__1 = w2x;
/* Computing 2nd power */
    r__2 = w2y;
    w2 = sqrt(r__1 * r__1 + r__2 * r__2);
/* THE ABSOLUTE VALUES OF THE SINES OF THE INTERNAL ANGLES OF NTR1. */
    snu1w2 = (r__1 = (u1x * w2y - w2x * u1y) / (u1 * w2), dabs(r__1));
    snu1v2 = (r__1 = (u1y * v2x - u1x * v2y) / (u1 * v2), dabs(r__1));
/* THE MINIMUM INTERNAL ANGLE. */
    rmntr1 = dmin(snu1w2,snu1v2);
/* THE ABSOLUTE VALUES OF THE SINES OF THE INTERNAL ANGLES OF NTR2. */
    snu1w1 = (r__1 = (w1x * u1y - w1y * u1x) / (u1 * w1), dabs(r__1));
    snv1u1 = (r__1 = (u1y * v1x - u1x * v1y) / (u1 * v1), dabs(r__1));
/* MINIMUM OF INTERIOR ANGLES. */
    rmntr2 = dmin(snu1w1,snv1u1);
/* MAXIMUM OF THE MINIMUM INTERIOR ANGLES FOR THE CURRENT DIAGONALIZATION.
 */
    rmncur = dmin(rmntr1,rmntr2);
/* TEST ALTERNATE DIAGONALIZATION. */
/* U2 IS ALTERNATE DIAGONAL. */
    u2x = triagd_1.x[ipt[3] - 1] - triagd_1.x[ipt[2] - 1];
    u2y = triagd_1.y[ipt[3] - 1] - triagd_1.y[ipt[2] - 1];
/* Computing 2nd power */
    r__1 = u2x;
/* Computing 2nd power */
    r__2 = u2y;
    u2 = sqrt(r__1 * r__1 + r__2 * r__2);
/*ABSOLUTE VALUES OF THE SINES OF THE INTERNAL ANGLES OF THE FIRST TRIANGL
E.*/
    snu2w1 = (r__1 = (u2y * w1x - u2x * w1y) / (u2 * w1), dabs(r__1));
    snu2w2 = (r__1 = (u2y * w2x - u2x * w2y) / (u2 * w2), dabs(r__1));
/*ABSOLUTE VALUES OF THE SINES OF THE INTERNAL ANGLES OF THE SECOND TRIANG
LE.*/
    snu2v1 = (r__1 = (u2y * v1x - u2x * v1y) / (u2 * v1), dabs(r__1));
    snu2v2 = (r__1 = (u2x * v2y - u2y * v2x) / (u2 * v2), dabs(r__1));
/* MINIMUM INTERNAL ANGLES IN EACH TRIANGLE. */
    rmntra = dmin(snu2w1,snu2w2);
    rmntrb = dmin(snu2v1,snu2v2);
/* MAXIMUM OF THE MINIMUM INTERNAL ANGLES. */
    rmnalt = dmin(rmntra,rmntrb);
/* QUIT IF CURRENT DIAGONALIZATION HAS GREATEST MAX. MIN. INTERNAL ANGLE. 
*/
    if (rmncur >= rmnalt) {
	return 0;
    }
/* ALTERNATE DIAGONALIZATION IS PREFERRED. */
/* ELIMINATE TRIANGLES NTR1 AND NTR2 FROM TRIANGLE LIST. */
    for (i__ = 1; i__ <= 6; ++i__) {
	triagd_1.itrngl[*ntr1 + i__ * 800 - 801] = 0;
/* L60: */
	triagd_1.itrngl[*ntr2 + i__ * 800 - 801] = 0;
    }
/* ELIMINATE NTR1 AND NTR2 AS NEIGHBORING TRIANGLES. */
    i__1 = triagd_1.itrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ != *ntr1 && i__ != *ntr2) {
	    for (j = 4; j <= 6; ++j) {
		if (triagd_1.itrngl[i__ + j * 800 - 801] == *ntr1 || 
			triagd_1.itrngl[i__ + j * 800 - 801] == *ntr2) {
		    triagd_1.itrngl[i__ + j * 800 - 801] = 0;
		}
/* L70: */
	    }
	}
/* L80: */
    }
/* ADD TWO NEW TRIANGLES.  BOTH ORIENTED COUNTERCLOCKWISE. */
    triagd_1.itrngl[*ntr1 - 1] = ipt[3];
    triagd_1.itrngl[*ntr1 + 799] = ipt[1];
    triagd_1.itrngl[*ntr1 + 1599] = ipt[2];
    triagd_1.itrngl[*ntr2 - 1] = ipt[2];
    triagd_1.itrngl[*ntr2 + 799] = ipt[0];
    triagd_1.itrngl[*ntr2 + 1599] = ipt[3];
/* NTR1 AND NTR2 ARE NEIGHBORS. */
    triagd_1.itrngl[*ntr1 + 3999] = *ntr2;
    triagd_1.itrngl[*ntr2 + 3999] = *ntr1;
/* SET TABLE FOR OTHER NEIGHBORS. */
    cont_(&ipt[2], &ipt[1], &c__5, ntr1);
    cont_(&ipt[1], &ipt[3], &c__4, ntr1);
    cont_(&ipt[2], ipt, &c__4, ntr2);
    cont_(ipt, &ipt[3], &c__5, ntr2);
    return 0;
} /* lawson_ */

/* Subroutine */ int clockw_(ntr, nrmvd)
integer *ntr, *nrmvd;
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static real v1cv2;
    extern /* Subroutine */ int cont_();
    static real vsin;
    static integer j;
    static real v1, v2;
    static integer itrsx;
    static real sangle;
    extern /* Subroutine */ int lawson_();
    static integer ip1, ip2;
    static real v1x, v1y, v2x, v2y;
    static integer ipt[2];

/* ROUTINE TO CHECK THE THREE BOUNDARY POINTS - NTR AND THE TWO POINTS */
/* CLOCKWISE FROM NTR - TO SEE IF THEY ARE CONCAVE AND TO ADD THE TRIANGLE
 */
/* FORMED BY THEM TO THE TRIANGULATION IF THEY ARE CONCAVE. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/*    SEE IF NEED TO ADD A TRIANGLE CLOCKWISE FROM THE NEW BOUNDARY POINT.
*/

/*SANGLE IS THE SINE OF AN ANGLE.  IF THE SINE OF THE ANGLE BETWEEN TWO BO
UNDARY*/
/*POINTS IS GREATER THAN SANGLE, THEN A TRIANGLE IS ADDED TO THE TRIANGULA
TION.*/
/* SINE OF AN ANGLE OF APPROX. 20 DEGREES. */
    sangle = (float).3;
    *nrmvd = 0;
/* GET THE TWO POINTS THAT ARE CLOCKWISE FROM NTR. */
    if (triagd_1.ibptno > 2) {
	ip1 = triagd_1.ibptno - 1;
	ip2 = triagd_1.ibptno - 2;
    } else {
	if (triagd_1.ibptno == 2) {
	    ip1 = 1;
	    ip2 = triagd_1.ibpts;
	} else {
	    ip1 = triagd_1.ibpts;
	    ip2 = triagd_1.ibpts - 1;
	}
    }
    ipt[0] = triagd_1.ibndry[ip1 - 1];
    ipt[1] = triagd_1.ibndry[ip2 - 1];
/* GET THE BOUNDARY SEGMENT VECTORS IPT(1) TO IPT(2) AND IPT(1) TO NTR. */
    v1x = triagd_1.x[ipt[1] - 1] - triagd_1.x[ipt[0] - 1];
    v1y = triagd_1.y[ipt[1] - 1] - triagd_1.y[ipt[0] - 1];
    v2x = triagd_1.x[*ntr - 1] - triagd_1.x[ipt[0] - 1];
    v2y = triagd_1.y[*ntr - 1] - triagd_1.y[ipt[0] - 1];
/* LENGTHS OF THE VECTORS. */
/* Computing 2nd power */
    r__1 = v1x;
/* Computing 2nd power */
    r__2 = v1y;
    v1 = sqrt(r__1 * r__1 + r__2 * r__2);
/* Computing 2nd power */
    r__1 = v2x;
/* Computing 2nd power */
    r__2 = v2y;
    v2 = sqrt(r__1 * r__1 + r__2 * r__2);
/* CROSS PRODUCT V1 X V2. */
    v1cv2 = v1x * v2y - v1y * v2x;
/* SINE OF ANGLE BETWEEN THEM. */
    vsin = v1cv2 / (v1 * v2);
/* TEST TO SEE IF ADD NEW TRIANGLE. */
    if (v1cv2 > (float)1e-4 && vsin > sangle) {
/* YES. ADD TRIANGLE FORMED BY IPT(1),IPT(2) AND NTR. */
	*nrmvd = 1;
	++triagd_1.itrs;
/* TRIANGLE ORIENTED COUNTERCLOCKWISE. */
	triagd_1.itrngl[triagd_1.itrs - 1] = *ntr;
	triagd_1.itrngl[triagd_1.itrs + 799] = ipt[0];
	triagd_1.itrngl[triagd_1.itrs + 1599] = ipt[1];
	cont_(ipt, &ipt[1], &c__5, &triagd_1.itrs);
	cont_(ipt, ntr, &c__4, &triagd_1.itrs);
/* LAWSON'S DIAGONAL EXCHANGE. */
	for (j = 4; j <= 6; ++j) {
	    if (triagd_1.itrngl[triagd_1.itrs + j * 800 - 801] != 0) {
		itrsx = triagd_1.itrngl[triagd_1.itrs + j * 800 - 801];
		lawson_(&triagd_1.itrs, &itrsx);
	    }
/* L75: */
	}
/* REMOVE IPT(1) FROM LIST OF BOUNDARY POINTS. */
	if (ip1 < triagd_1.ibpts) {
	    i__1 = triagd_1.ibpts - 1;
	    for (j = ip1; j <= i__1; ++j) {
		triagd_1.ibndry[j - 1] = triagd_1.ibndry[j];
/* L70: */
	    }
	}
	--triagd_1.ibpts;
/* ADJUST LOCATION OF POINT NTR IN THE LIST OF BOUNDARY POINTS. */
	if (triagd_1.ibptno > 1) {
	    --triagd_1.ibptno;
	}
    }
    return 0;
} /* clockw_ */

/* Subroutine */ int cclock_(ntr, nrmvd)
integer *ntr, *nrmvd;
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static real v1cv2;
    extern /* Subroutine */ int cont_();
    static real vsin;
    static integer j;
    static real v1, v2;
    static integer itrsx;
    static real sangle;
    extern /* Subroutine */ int lawson_();
    static integer ip1, ip2;
    static real v1x, v1y, v2x, v2y;
    static integer ipt[2];

/* ROUTINE TO CHECK THE THREE POINTS - NTR AND THE TWO POINTS THAT */
/* ARE COUNTERCLOCKWISE FROM NTR - TO SEE IF THEY ARE CONCAVE AND TO */
/*ADD THE TRIANGLE FORMED BY THEM TO THE TRIANGULATION IF THEY ARE CONCAVE
.*/
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* SINE OF AN ANGLE. SEE COMMENTS IN SUBROUTINE CLOCK. */
    sangle = (float).3;

/*     SEE IF ADD A TRIANGLE COUNTERCLOCKWISE FROM IPT(3). */

    *nrmvd = 0;
/* GET TWO BOUNDARY SEGMENTS COUNTERCLOCKWISE FROM NTR. */
    if (triagd_1.ibptno < triagd_1.ibpts - 1) {
	ip1 = triagd_1.ibptno + 1;
	ip2 = triagd_1.ibptno + 2;
    } else {
	if (triagd_1.ibptno == triagd_1.ibpts - 1) {
	    ip1 = triagd_1.ibpts;
	    ip2 = 1;
	} else {
	    ip1 = 1;
	    ip2 = 2;
	}
    }
    ipt[0] = triagd_1.ibndry[ip1 - 1];
    ipt[1] = triagd_1.ibndry[ip2 - 1];
/* GET VECTORS IPT(1) TO NTR AND IPT(1) TO IPT(2). */
    v1x = triagd_1.x[*ntr - 1] - triagd_1.x[ipt[0] - 1];
    v1y = triagd_1.y[*ntr - 1] - triagd_1.y[ipt[0] - 1];
    v2x = triagd_1.x[ipt[1] - 1] - triagd_1.x[ipt[0] - 1];
    v2y = triagd_1.y[ipt[1] - 1] - triagd_1.y[ipt[0] - 1];
/* LENGTHS OF VECTORS. */
/* Computing 2nd power */
    r__1 = v1x;
/* Computing 2nd power */
    r__2 = v1y;
    v1 = sqrt(r__1 * r__1 + r__2 * r__2);
/* Computing 2nd power */
    r__1 = v2x;
/* Computing 2nd power */
    r__2 = v2y;
    v2 = sqrt(r__1 * r__1 + r__2 * r__2);
/* CROSS PRODUCT V1 X V2. */
    v1cv2 = v1x * v2y - v1y * v2x;
/* SINE OF ANGLE BETWEEN THEM. */
    vsin = v1cv2 / (v1 * v2);
/* TEST TO SEE IF ADD NEW TRIANGLE. */
    if (v1cv2 > (float)1e-4 && vsin > sangle) {
/* YES. ADD TRIANGLE FORMED BY NTR,IPT(1) AND IPT(2). */
	*nrmvd = 1;
	++triagd_1.itrs;
/* TRIANGLE ORIENTED COUNTERCLOCKWISE. */
	triagd_1.itrngl[triagd_1.itrs - 1] = *ntr;
	triagd_1.itrngl[triagd_1.itrs + 799] = ipt[1];
	triagd_1.itrngl[triagd_1.itrs + 1599] = ipt[0];
	cont_(ipt, &ipt[1], &c__5, &triagd_1.itrs);
	cont_(ipt, ntr, &c__6, &triagd_1.itrs);
/* LAWSON'S DIAGONAL EXCHANGE. */
	for (j = 4; j <= 6; ++j) {
	    if (triagd_1.itrngl[triagd_1.itrs + j * 800 - 801] != 0) {
		itrsx = triagd_1.itrngl[triagd_1.itrs + j * 800 - 801];
		lawson_(&triagd_1.itrs, &itrsx);
	    }
/* L85: */
	}
/* REMOVE POINT IPT(1) FROM THE LIST OF BOUNDARY POINTS. */
	if (ip1 < triagd_1.ibpts) {
	    i__1 = triagd_1.ibpts - 1;
	    for (j = ip1; j <= i__1; ++j) {
		triagd_1.ibndry[j - 1] = triagd_1.ibndry[j];
/* L90: */
	    }
	}
	if (triagd_1.ibptno == triagd_1.ibpts) {
	    --triagd_1.ibptno;
	}
	--triagd_1.ibpts;
    }
    return 0;
} /* cclock_ */

/* Subroutine */ int orient_(npt1, npt2, npt3, mpt1, mpt2, mpt3)
integer *npt1, *npt2, *npt3, *mpt1, *mpt2, *mpt3;
{
    /* System generated locals */
    real r__1;

    /* Local variables */
    static real v2cv1;
    static integer last, minx, maxx, i__;
    static real v1x, v1y, v2x, v2y;
    static integer npt[3];
    static real xmn, xmx;

/* ROUTINE TO DETERMINE THE COUNTERCLOCKWISE ORIENTATION OF THE THREE */
/* POINTS NPT1, NPT2 AND NPT3.  MPT1, MPT2 AND MPT3 ARE THE RETURNED */
/* POINTS IN COUNTERCLOCKWISE ORIENTATION.  MPT1 IS COUNTERCLOCKWISE */
/* FROM MPT3, MPT2 IS COUNTERCLOCKWISE FROM MPT1, AND MPT3 IS */
/* COUNTERCLOCKWISE FROM MPT2. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    npt[0] = *npt1;
    npt[1] = *npt2;
    npt[2] = *npt3;
/* GET THE POINT THAT HAS THE SMALLEST X VALUE. */
    xmn = (float)1e30;
    for (i__ = 1; i__ <= 3; ++i__) {
	if (triagd_1.x[npt[i__ - 1] - 1] < xmn) {
	    xmn = triagd_1.x[npt[i__ - 1] - 1];
	    minx = npt[i__ - 1];
	}
/* L30: */
    }
/* OF THE REMAINING POINTS, GET THE ONE WITH THE GREATEST X VALUE. */
    xmx = (float)-1e30;
    for (i__ = 1; i__ <= 3; ++i__) {
	if (npt[i__ - 1] != minx && triagd_1.x[npt[i__ - 1] - 1] > xmx) {
	    xmx = triagd_1.x[npt[i__ - 1] - 1];
	    maxx = npt[i__ - 1];
	}
/* L40: */
    }
/* GET THE REMAINING POINT. */
    for (i__ = 1; i__ <= 3; ++i__) {
	if (npt[i__ - 1] != maxx && npt[i__ - 1] != minx) {
	    last = npt[i__ - 1];
	}
/* L45: */
    }
/* VECTOR FROM MINX TO MAXX. */
    v1x = triagd_1.x[maxx - 1] - triagd_1.x[minx - 1];
    v1y = triagd_1.y[maxx - 1] - triagd_1.y[minx - 1];
/* VECTOR FROM MINX TO LAST. */
    v2x = triagd_1.x[last - 1] - triagd_1.x[minx - 1];
    v2y = triagd_1.y[last - 1] - triagd_1.y[minx - 1];
/* CROSS PRODUCT V2 X V1. */
    v2cv1 = v2x * v1y - v2y * v1x;
/* TEST TO SEE IF LAST IS ABOVE OR BELOW LINE FORMED BY MINX AND MAXX. */
    if (v2cv1 < (float)0. || (r__1 = triagd_1.x[last - 1] - triagd_1.x[minx - 
	    1], dabs(r__1)) <= (float)1e-6 && triagd_1.y[last - 1] >= 
	    triagd_1.y[minx - 1]) {
/* ABOVE. */
	*mpt2 = maxx;
	*mpt3 = last;
    } else {
/* BELOW. */
	*mpt2 = last;
	*mpt3 = maxx;
    }
    *mpt1 = minx;
    return 0;
} /* orient_ */

/* Subroutine */ int ptsrch_(xx, yy, ntrin, ncontn, ntrout)
real *xx, *yy;
integer *ntrin, *ncontn, *ntrout;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int cntain_();
    static integer itrndx[800];

/* ROUTINE TO SEARCH FOR THE TRIANGLE THAT CONTAINS POINT (XX,YY). */
/* NTRIN IS AN ESTIMATE OF THE TRIANGLE THAT CONTAINS (XX,YY) AND IS */
/* USED AS A STARTING TRIANGLE FOR THE SEARCH.  NTROUT IS THE LAST */
/* TRIANGLE SEARCHED. */
/* NCONTN IS THE SAME VARIABLE */
/* USED IN SUBROUTINE CNTAIN.  NCONTN IS 0 IF (XX,YY) IS WITHIN THE AREA 
*/
/* COVERED BY STATIONS AND IS NONZERO OTHERWISE. */
/* ITRNDX(I) = 0 IF TRIANGLE I HAS NOT BEEN SEARCHED YET. */
/* ITRNDX(I) = 1 IF TRIANGLE I HAS BEEN SEARCHED. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    *ntrout = *ntrin;
    for (i__ = 1; i__ <= 800; ++i__) {
/* L10: */
	itrndx[i__ - 1] = 0;
    }
    i__1 = triagd_1.itrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	cntain_(xx, yy, ntrout, ncontn);
/* QUIT IF (XX,YY) IS IN TRIANGLE NTROUT. */
	if (*ncontn == 0) {
	    return 0;
	}
/* OTHERWISE MARK TRIANGLE NTROUT AS HAVING BEEN SEARCHED AND GET */
/* A TRIANGLE CONTIGUOUS WITH IT AS THE NEXT TRIANGLE TO BE TRIED. */
	itrndx[*ntrout - 1] = 1;
	*ntrout = triagd_1.itrngl[*ntrout + *ncontn * 800 - 801];
/* QUIT THIS SECTION OF ROUTINE IF THERE IS NO CONTIGUOUS TRIANGLE */
/* ON THE NCONTN SIDE OF TRIANGLE NTROUT. */
	if (*ntrout == 0) {
	    goto L110;
	}
/* L100: */
    }

/* SEARCH THROUGH ALL TRIANGLES THAT HAVE NOT BEEN TRIED YET. */
L110:
    i__1 = triagd_1.itrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (itrndx[i__ - 1] == 0) {
	    cntain_(xx, yy, &i__, ncontn);
	    if (*ncontn == 0) {
		*ntrout = i__;
		return 0;
	    }
	}
/* L200: */
    }
    return 0;
} /* ptsrch_ */

/* Subroutine */ int cntain_(xx, yy, ntr, ncontn)
real *xx, *yy;
integer *ntr, *ncontn;
{
    static real edge[6]	/* was [3][2] */, v1cei;
    static integer i__;
    static real v1x, v1y;
    static integer ipt[3];

/* ROUTINE TO DETERMINE IF POINT (XX,YY) IS CONTAINED IN TRIANGLE NTR. */
/* IF (XX,YY) IS IN TRIANGLE NTR, NCONTN = 0.  OTHERWISE, NCONTN IS */
/* THE EDGE OF THE TRIANGLE THAT (XX,YY) IS BEYOND. */
/* THE EDGE NUMBERS (4,5 OR 6) CORRESPOND TO THE ENTRIES IN ARRAY */
/* ITRNGL(*) FOR THE CONTIGUOUS EDGES OF EACH TRIANGLE. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    *ncontn = 0;
/* THREE POINTS OF TRIANGLE NTR. */
    ipt[0] = triagd_1.itrngl[*ntr - 1];
    ipt[1] = triagd_1.itrngl[*ntr + 799];
    ipt[2] = triagd_1.itrngl[*ntr + 1599];
/* EDGE 1. IPT(1) TO IPT(2). */
    edge[0] = triagd_1.x[ipt[1] - 1] - triagd_1.x[ipt[0] - 1];
    edge[3] = triagd_1.y[ipt[1] - 1] - triagd_1.y[ipt[0] - 1];
/* EDGE 2. IPT(2) TO IPT(3). */
    edge[1] = triagd_1.x[ipt[2] - 1] - triagd_1.x[ipt[1] - 1];
    edge[4] = triagd_1.y[ipt[2] - 1] - triagd_1.y[ipt[1] - 1];
/* EDGE 3. IPT(3) TO IPT(1). */
    edge[2] = triagd_1.x[ipt[0] - 1] - triagd_1.x[ipt[2] - 1];
    edge[5] = triagd_1.y[ipt[0] - 1] - triagd_1.y[ipt[2] - 1];

    for (i__ = 1; i__ <= 3; ++i__) {
/* VECTOR FROM ORIGIN PT. OF EDGE I TO (XX,YY). */
	v1x = *xx - triagd_1.x[ipt[i__ - 1] - 1];
	v1y = *yy - triagd_1.y[ipt[i__ - 1] - 1];
/* CROSS PRODUCT V1 X EDGE I. */
	v1cei = v1x * edge[i__ + 2] - v1y * edge[i__ - 1];
/*CROSS PRODUCT MUST BE ZERO OR NEGATIVE FOR (XX,YY) TO BE IN TRIANGLE
 NTR.*/
	if (v1cei > (float)0.) {
/* NOT IN TRIANGLE.  QUIT. */
	    *ncontn = i__ + 3;
	    return 0;
	}
/* L10: */
    }
    return 0;
} /* cntain_ */

/* Subroutine */ int linwt_(ntr, pam, w1, w2, w3)
integer *ntr;
real *pam, *w1, *w2, *w3;
{
    static integer ip1, ip2, ip3;

/* CALCULATE THE INTERPOLATION WEIGHTS(W1,W2,W3) FOR TRIANGLE NTR */
/* USING A LINEAR INTERPOLATION. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    /* Parameter adjustments */
    --pam;

    /* Function Body */
    *w1 = (float)0.;
    *w2 = (float)0.;
    *w3 = (float)0.;
    ip1 = triagd_1.itrngl[*ntr - 1];
    ip2 = triagd_1.itrngl[*ntr + 799];
    ip3 = triagd_1.itrngl[*ntr + 1599];

/* SOLVE THE FOLLOWING EQUATION */
/* FOR THE THREE PAM STATIONS TO GET THE WEIGHTS: */
/* PAM(I) = W1 + W2*X(I) + W3*Y(I) */

    *w3 = pam[ip1] * (triagd_1.x[ip3 - 1] - triagd_1.x[ip2 - 1]) + pam[ip2] * 
	    (triagd_1.x[ip1 - 1] - triagd_1.x[ip3 - 1]) + pam[ip3] * (
	    triagd_1.x[ip2 - 1] - triagd_1.x[ip1 - 1]);
    *w3 /= triagd_1.y[ip1 - 1] * (triagd_1.x[ip3 - 1] - triagd_1.x[ip2 - 1]) 
	    + triagd_1.y[ip2 - 1] * (triagd_1.x[ip1 - 1] - triagd_1.x[ip3 - 1]
	    ) + triagd_1.y[ip3 - 1] * (triagd_1.x[ip2 - 1] - triagd_1.x[ip1 - 
	    1]);
    *w2 = pam[ip2] - pam[ip1] + *w3 * (triagd_1.y[ip1 - 1] - triagd_1.y[ip2 - 
	    1]);
    *w2 /= triagd_1.x[ip2 - 1] - triagd_1.x[ip1 - 1];
    *w1 = pam[ip1] - *w3 * triagd_1.y[ip1 - 1];
    *w1 -= triagd_1.x[ip1 - 1] * (pam[ip2] - pam[ip1] + *w3 * (triagd_1.y[ip1 
	    - 1] - triagd_1.y[ip2 - 1])) / (triagd_1.x[ip2 - 1] - triagd_1.x[
	    ip1 - 1]);
    return 0;
} /* linwt_ */

/* Subroutine */ int genwt_(pam)
real *pam;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int linwt_();

/* CALCULATE THE INTERPOLATION WEIGHTS FOR ALL TRIANGLES USING A LINEAR */
/* INTERPOLATION. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    /* Parameter adjustments */
    --pam;

    /* Function Body */
    i__1 = triagd_1.itrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	linwt_(&i__, &pam[1], &triagd_1.weight[i__ - 1], &triagd_1.weight[i__ 
		+ 799], &triagd_1.weight[i__ + 1599]);
	triagd_1.weight[i__ + 2399] = (float)0.;
	triagd_1.weight[i__ + 3199] = (float)0.;
	triagd_1.weight[i__ + 3999] = (float)0.;
/* L10: */
    }
    return 0;
} /* genwt_ */

/* Subroutine */ int wghts_()
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, j, k, ntrin;
    static real xx, yy;
    static integer ncontn;
    extern /* Subroutine */ int ptsrch_();
    static integer ntrout;

/* ROUTINE TO FIND THE TRIANGLE CONTAINING EACH GRID POINT. */
/* JW(I,J) - THE TRIANGLE CONTAINING IGRID POINT I,J. */
/* JW(I,J) = -1 IF GRID POINT I,J IS NOT IN THE AREA COVERED BY PAM */
/* STATIONS. */
/*SEE THE CODE FOR THE CALCULATION OF THE X AND Y VALUES OF GRID POINT I,J
.*/
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/*     INITIALIZATION */

    for (k = 1; k <= 25; ++k) {
	for (j = 1; j <= 25; ++j) {
/* L20: */
	    triagd_1.jw[j + k * 25 - 26] = 0;
	}
    }

/*ESTIMATE OF THE TRIANGLE THAT CONTAINS THE GRID POINT CURRENTLY CONSIDER
ED.*/
    ntrin = 1;
/* GENERATE THE GRID POINTS, FIND WHICH TRIANGLE EACH IS IN, AND */
/* STORE THE INTERPOLATION WEIGHTS FOR EACH POINT. */
    i__1 = triagd_1.ny;
    for (j = 1; j <= i__1; ++j) {
	xx = triagd_1.x0;
	yy = triagd_1.y0 + (j - 1) * triagd_1.dy;
	i__2 = triagd_1.nx;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    ptsrch_(&xx, &yy, &ntrin, &ncontn, &ntrout);
/* SEE IF (XX,YY) IS WITHIN TRIANGULATED AREA. */
	    if (ncontn > 0) {
/* NO. */
		triagd_1.jw[i__ + j * 25 - 26] = -1;
	    } else {
/* YES. */
		triagd_1.jw[i__ + j * 25 - 26] = ntrout;
		ntrin = ntrout;
	    }
	    xx += triagd_1.dx;
/* L40: */
	}
/* L50: */
    }
    return 0;
} /* wghts_ */

/* Subroutine */ int starea_()
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    extern doublereal farea_();
    static real x1, x2, x3, y1, y2, y3, xb, yb, xm1, xm2, xm3, ym1, ym2, ym3;

/* FIND THE AREA OF INFLUENCE OF EACH STATION LOCATION. */
/* AREA(I) = AREA OF INFLUENCE OF STATION I. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    for (i__ = 1; i__ <= 800; ++i__) {
/* L10: */
	triagd_1.area[i__ - 1] = (float)0.;
    }

    i__1 = triagd_1.itrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x1 = triagd_1.x[triagd_1.itrngl[i__ - 1] - 1];
	x2 = triagd_1.x[triagd_1.itrngl[i__ + 799] - 1];
	x3 = triagd_1.x[triagd_1.itrngl[i__ + 1599] - 1];
	y1 = triagd_1.y[triagd_1.itrngl[i__ - 1] - 1];
	y2 = triagd_1.y[triagd_1.itrngl[i__ + 799] - 1];
	y3 = triagd_1.y[triagd_1.itrngl[i__ + 1599] - 1];

	xm1 = (x2 + x3) / (float)2.;
	xm2 = (x1 + x3) / (float)2.;
	xm3 = (x1 + x2) / (float)2.;
	ym1 = (y2 + y3) / (float)2.;
	ym2 = (y1 + y3) / (float)2.;
	ym3 = (y1 + y2) / (float)2.;

	xb = (x1 + x2 + x3) / (float)3.;
	yb = (y1 + y2 + y3) / (float)3.;

	triagd_1.area[triagd_1.itrngl[i__ - 1] - 1] = triagd_1.area[
		triagd_1.itrngl[i__ - 1] - 1] + farea_(&x1, &y1, &xm3, &ym3, &
		xb, &yb) + farea_(&x1, &y1, &xm2, &ym2, &xb, &yb);
	triagd_1.area[triagd_1.itrngl[i__ + 799] - 1] = triagd_1.area[
		triagd_1.itrngl[i__ + 799] - 1] + farea_(&x2, &y2, &xm1, &ym1,
		 &xb, &yb) + farea_(&x2, &y2, &xm3, &ym3, &xb, &yb);
	triagd_1.area[triagd_1.itrngl[i__ + 1599] - 1] = triagd_1.area[
		triagd_1.itrngl[i__ + 1599] - 1] + farea_(&x3, &y3, &xm1, &
		ym1, &xb, &yb) + farea_(&x3, &y3, &xm2, &ym2, &xb, &yb);
/* L100: */
    }
    return 0;
} /* starea_ */

doublereal farea_(x1, y1, x2, y2, x3, y3)
real *x1, *y1, *x2, *y2, *x3, *y3;
{
    /* System generated locals */
    real ret_val, r__1;

/* ROUTINE TO FIND THE AREA OF THE TRIANGLE FORMED BY THE THREE POINTS */
/* (X1,Y1),(X2,Y2) AND (X3,Y3). */

    ret_val = (r__1 = (*x1 - *x3) * (*y2 - *y3) - (*x2 - *x3) * (*y1 - *y3), 
	    dabs(r__1)) / (float)2.;
    return ret_val;
} /* farea_ */

/* Subroutine */ int stats_(status)
integer *status;
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static integer i__;
    static real denom, xmean, ymean, slope, xsqrd, rtest, y1, xy, res;

/* ROUTINE TO TEST THE STATION LOCATIONS TO SEE THAT THERE IS ENOUGH */
/* SPREAD TO DO A REASONABLE TRIANGULATION. */
/* STATION LOCATION STATISTICS THAT ARE USED FOR PLOTTING ARE ALSO */
/* CALCULATED. */

/* STATUS is 0 if the points are spread out enough, */
/* otherwise STATUS reflects some problem in point spacing. */
/* See rgrid.h for other status values. */

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    *status = 0;

/*     STATISTICS. */

    xmean = (float)0.;
    ymean = xmean;
    xy = xmean;
    xsqrd = xmean;
    triagd_1.nstns = 0;
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! triagd_1.deleted[i__ - 1]) {
	    xmean += triagd_1.x[i__ - 1];
	    ymean += triagd_1.y[i__ - 1];
	    xy += triagd_1.x[i__ - 1] * triagd_1.y[i__ - 1];
	    xsqrd += triagd_1.x[i__ - 1] * triagd_1.x[i__ - 1];
	    ++triagd_1.nstns;
	}
/* L10: */
    }
/* QUIT IF NOT ENOUGH STATIONS. */
    if (triagd_1.nstns < 3) {
	*status = 1;
	return 0;
    }
/* X DISTANCE BETWEEN GRID POINTS. */
    triagd_1.dx = (triagd_1.xmax - triagd_1.xmin) / (triagd_1.nx + 1);
/* Y DISTANCE BETWEEN GRID POINTS. */
    triagd_1.dy = (triagd_1.ymax - triagd_1.ymin) / (triagd_1.ny + 1);
/* REFERENCE POINT. */
    triagd_1.x0 = triagd_1.xmin + triagd_1.dx;
    triagd_1.y0 = triagd_1.ymin + triagd_1.dy;

/*     CALCULATE THE LINE OF REGRESSION. */

/* QUIT AT THIS POINT IF USE POLYNOMIAL REGRESSION. */
    if (triagd_1.naprox == 1) {
	return 0;
    }
/* CALCULATE THE RESIDUALS FROM THE LINE OF REGRESSION. */
    xmean /= (real) triagd_1.nstns;
    ymean /= (real) triagd_1.nstns;
    denom = xsqrd - triagd_1.nstns * xmean * xmean;
    if (dabs(denom) > (float)1e-6) {
	slope = (xy - triagd_1.nstns * xmean * ymean) / denom;
    } else {
	slope = (float)1e6;
    }
    res = (float)0.;
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! triagd_1.deleted[i__ - 1]) {
	    y1 = ymean + slope * (triagd_1.x[i__ - 1] - xmean);
	    res += (r__1 = y1 - triagd_1.y[i__ - 1], dabs(r__1));
	}
/* L20: */
    }
/* MEAN OF RESIDUALS. */
    res /= (real) triagd_1.nstns;
/* Computing 2nd power */
    r__1 = triagd_1.xmax - triagd_1.xmin;
/* Computing 2nd power */
    r__2 = triagd_1.ymax - triagd_1.ymin;
    rtest = sqrt(r__1 * r__1 + r__2 * r__2);
/* IF RMS OF RESIDUALS IS WITHIN 3 % OF RTEST, STATIONS ARE NOT FAR */
/* ENOUGH APART TO TRIANGULATE. */
    if (res <= rtest * (float).03) {
	*status = 2;
    }
    return 0;
} /* stats_ */

/* Subroutine */ int rgrid1_(uu, vv, zz, ovrgrd, scr1, scr2)
real *uu, *vv, *zz, *ovrgrd, *scr1, *scr2;
{
    /* System generated locals */
    integer scr1_dim1, scr1_offset, scr2_dim1, scr2_offset, uu_dim1, 
	    uu_offset, vv_dim1, vv_offset, zz_dim1, zz_offset, ovrgrd_dim1, 
	    ovrgrd_offset, i__1, i__2;

    /* Local variables */
    extern /* Subroutine */ int eddy_();
    static integer i__, j;
    extern /* Subroutine */ int cfitd_();
    static integer index;
    extern /* Subroutine */ int compx_(), approx_(), lintrp_();
    static real xin[16800]	/* was [21][800] */;

/* ROUTINE TO COMPUTE AND INTERPOLATE THE FIELD CHOSEN BY THE OPERATOR. */
/* VARIABLE IPAR INDICATES THE TYPE OF FIELD. */
/*THESE FIELDS ARE IN THE ROBOT FIELD TABLE.  THE ROBOT FIELD NUMBER INDEX
:*/
/* IPAR < 56 OR IPAR > 61 - STANDARD ROBOT FIELDS */
/* IPAR = 56 - DIVERGENCE(1/1.E-6*SEC) */
/* IPAR = 57 - VORTICITY(1/1.E-6*SEC) */
/* IPAR = 58 - ENERGY FLUX CONVERGENCE(DEG/1.E-3*SEC) */
/* IPAR = 59 - ALTITUDE(METERS) */
/* IPAR = 60 - WIND FIELD(METERS/SEC) */
/* IPAR = 61 - EDDY FIELD(METERS/SEC) */
/* IPAR = 77 - STRETCHING DEFORMATON(1/1.E-6*SEC) */
/* IPAR = 78 - SHEARING DEFORMATION(1/1.E-6*SEC) */
/* IPAR = 88 - TOTAL DEFORMATION(1/1.E-6*SEC) */
/* IPAR = 143 - VORTICITY STRETCHING (1/SEC^2 * 1.E-6) (added 4/17/89 cb) 
*/
/* IPAR = 999 - AXIS OF DILATATION(DEGREES) */

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */


/*     INITIALIZATION. */

    /* Parameter adjustments */
    scr2_dim1 = triagd_1.nx;
    scr2_offset = scr2_dim1 + 1;
    scr2 -= scr2_offset;
    scr1_dim1 = triagd_1.nx;
    scr1_offset = scr1_dim1 + 1;
    scr1 -= scr1_offset;
    ovrgrd_dim1 = triagd_1.nx;
    ovrgrd_offset = ovrgrd_dim1 + 1;
    ovrgrd -= ovrgrd_offset;
    zz_dim1 = triagd_1.nx;
    zz_offset = zz_dim1 + 1;
    zz -= zz_offset;
    vv_dim1 = triagd_1.nx;
    vv_offset = vv_dim1 + 1;
    vv -= vv_offset;
    uu_dim1 = triagd_1.nx;
    uu_offset = uu_dim1 + 1;
    uu -= uu_offset;

    /* Function Body */
    i__1 = triagd_1.ny;
    for (j = 1; j <= i__1; ++j) {
	i__2 = triagd_1.nx;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    zz[i__ + j * zz_dim1] = (float)0.;
	    uu[i__ + j * uu_dim1] = (float)0.;
	    vv[i__ + j * vv_dim1] = (float)0.;
	    ovrgrd[i__ + j * ovrgrd_dim1] = (float)0.;
/* L5: */
	}
    }
    if (triagd_1.naprox == 1) {
	compx_(xin, &index);
    }

/*     WIND FIELD AND EDDY FIELD VECTOR PLOTS. */

    if (triagd_1.ipar == 60 || triagd_1.ipar == 61) {
	if (triagd_1.ipar == 61) {
	    eddy_();
	}
	approx_(triagd_1.u, &uu[uu_offset], &scr1[scr1_offset], &c__1, xin, &
		index);
	approx_(triagd_1.v, &vv[vv_offset], &scr1[scr1_offset], &c__1, xin, &
		index);
/* PLOT DATA. */
    }

/*     BASIC FIELDS AND ALTITUDE CONTOUR PLOTS. */

    if (triagd_1.ipar != 56 && triagd_1.ipar != 57 && triagd_1.ipar != 58 && 
	    triagd_1.ipar != 60 && triagd_1.ipar != 61 && triagd_1.ipar != 77 
	    && triagd_1.ipar != 78 && triagd_1.ipar != 88 && triagd_1.ipar != 
	    143 && triagd_1.ipar != 999) {
/* BASIC FIELDS AND ALTITUDE. */
/* INTERPOLATE. */
	approx_(triagd_1.z__, &zz[zz_offset], &scr1[scr1_offset], &c__1, xin, 
		&index);
/* INTERPOLATE WIND OR EDDY FIELD DATA IF OVERLAID ON CONTOUR PLOTS. 
*/
	if (triagd_1.iparov != 0) {
	    if (abs(triagd_1.iparov) == 61) {
		eddy_();
	    }
	    if (triagd_1.iparov == 60 || triagd_1.iparov == 61) {
		approx_(triagd_1.u, &uu[uu_offset], &scr1[scr1_offset], &c__1,
			 xin, &index);
		approx_(triagd_1.v, &vv[vv_offset], &scr1[scr1_offset], &c__1,
			 xin, &index);
	    }
	}
/* PLOT DATA. */
    }

/*     SCALAR OVERLAY FIELD. */

    if (triagd_1.iparov > 0 && triagd_1.iparov != 56 && triagd_1.iparov != 57 
	    && triagd_1.iparov != 58 && triagd_1.iparov != 60 && 
	    triagd_1.iparov != 61 && triagd_1.iparov != 77 && triagd_1.iparov 
	    != 78 && triagd_1.iparov != 88 && triagd_1.iparov != 143 && 
	    triagd_1.iparov != 999) {
	if (triagd_1.iparov == triagd_1.ipar) {
/* OVERLAY FIELD SAME AS CONTOUR FIELD. */
	    i__2 = triagd_1.ny;
	    for (j = 1; j <= i__2; ++j) {
		i__1 = triagd_1.nx;
		for (i__ = 1; i__ <= i__1; ++i__) {
/* L60: */
		    ovrgrd[i__ + j * ovrgrd_dim1] = zz[i__ + j * zz_dim1];
		}
	    }
	} else {
/* OVERLAY FIELD DIFFERENT FROM CONTOURED FIELD. */
	    approx_(triagd_1.ovrpam, &ovrgrd[ovrgrd_offset], &scr1[
		    scr1_offset], &c__1, xin, &index);
	}
    }

/*     DIVERGENCE, VORTICITY, ENERGY FLUX CONVERGENCE, STRETCHING */
/*     DEFORMATION, SHEARING DEFORMATION, TOTAL DEFORMATION, AND */
/*     VORTICITY STRETCHING CONTOUR PLOTS. */

    if (triagd_1.ipar == 56 || triagd_1.ipar == 57 || triagd_1.ipar == 58 || 
	    triagd_1.ipar == 77 || triagd_1.ipar == 78 || triagd_1.ipar == 88 
	    || triagd_1.ipar == 143 || triagd_1.ipar == 999) {
	if (triagd_1.naprox == 0) {
	    lintrp_(&uu[uu_offset], &vv[vv_offset], &zz[zz_offset], &scr1[
		    scr1_offset], &scr2[scr2_offset]);
	} else {
	    cfitd_(xin, &index, &c__0, &uu[uu_offset], &vv[vv_offset], &zz[
		    zz_offset], &scr1[scr1_offset], &scr2[scr2_offset]);
	}

/* SET DATA TO FLAG AT ALL GRID PTS. WHERE THERE IS NO DATA. */
/* AND MULTIPLY EVERYTHING ELSE BY 1000 SO THAT THE UNITS ARE */
/* 1/1.E-6*SEC FOR DIVERGENCE, VORTICITY, SHEARING DEFORMATION, */
/* STRETCHING DEFORMATION, AND TOTAL DEFORMATION. */
/* MULTIPLY BY 1000 TO GET VORTICITY STRETCHING IN UNITS OF 1/S^2 x 1E
-9 */

	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		if (triagd_1.jw[i__ + j * 25 - 26] < 0) {
		    zz[i__ + j * zz_dim1] = triagd_1.flag__;
		} else if (triagd_1.ipar == 143) {
		    zz[i__ + j * zz_dim1] *= (float)1e3;
		} else {
		    if (triagd_1.ipar != 58 && triagd_1.ipar != 999) {
			zz[i__ + j * zz_dim1] *= (float)1e3;
		    }
		}
/* L40: */
	    }
	}
/* DO THE SAME FOR WIND OR EDDY FIELD DATA IF IT IS TO BE OVERLAYED ON
 */
/* THE CONTOUR PLOT. */
	if (triagd_1.iparov != 0) {
	    if (abs(triagd_1.iparov) == 61) {
		eddy_();
		if (triagd_1.iparov > 0) {
		    approx_(triagd_1.u, &uu[uu_offset], &scr1[scr1_offset], &
			    c__1, xin, &index);
		    approx_(triagd_1.v, &vv[vv_offset], &scr1[scr1_offset], &
			    c__1, xin, &index);
		}
	    }
	    if (triagd_1.iparov == 60) {
		i__2 = triagd_1.ny;
		for (j = 1; j <= i__2; ++j) {
		    i__1 = triagd_1.nx;
		    for (i__ = 1; i__ <= i__1; ++i__) {
			if (triagd_1.jw[i__ + j * 25 - 26] < 0) {
			    uu[i__ + j * uu_dim1] = triagd_1.flag__;
			    vv[i__ + j * vv_dim1] = triagd_1.flag__;
			}
/* L70: */
		    }
		}
	    }
/* OVERLAY FIELD IS DIVERGENCE, VORTICITY OR EFC. */
	    if (triagd_1.iparov == triagd_1.ipar) {
		i__1 = triagd_1.ny;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = triagd_1.nx;
		    for (i__ = 1; i__ <= i__2; ++i__) {
/* L80: */
			ovrgrd[i__ + j * ovrgrd_dim1] = zz[i__ + j * zz_dim1];
		    }
		}
	    }
	}
/* PLOT DIVERGENCE, VORTICITY OR ENERGY FLUX CONVERGENCE. */
    }

    return 0;
} /* rgrid1_ */

/* Subroutine */ int lintrp_(uu, vv, zz, scr1, scr2)
real *uu, *vv, *zz, *scr1, *scr2;
{
    /* Initialized data */

    static real pi = (float)3.1415927;

    /* System generated locals */
    integer uu_dim1, uu_offset, vv_dim1, vv_offset, zz_dim1, zz_offset, 
	    scr1_dim1, scr1_offset, scr2_dim1, scr2_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(), atan();

    /* Local variables */
    extern /* Subroutine */ int dvrg_();
    static integer i__, j, index;
    extern /* Subroutine */ int approx_();
    static real xin;

/* CALCULATION OF DIVERGENCE, VORTICITY AND ENERGY FLUX DENSITY */
/* USING LINEAR INTERPOLATION. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */
    /* Parameter adjustments */
    scr2_dim1 = triagd_1.nx;
    scr2_offset = scr2_dim1 + 1;
    scr2 -= scr2_offset;
    scr1_dim1 = triagd_1.nx;
    scr1_offset = scr1_dim1 + 1;
    scr1 -= scr1_offset;
    zz_dim1 = triagd_1.nx;
    zz_offset = zz_dim1 + 1;
    zz -= zz_offset;
    vv_dim1 = triagd_1.nx;
    vv_offset = vv_dim1 + 1;
    vv -= vv_offset;
    uu_dim1 = triagd_1.nx;
    uu_offset = uu_dim1 + 1;
    uu -= uu_offset;

    /* Function Body */

/* DIVERGENCE, VORTICITY AND ENERGY FLUX DENSITY.  INTERPOLATE WIND DATA. 
*/
    approx_(triagd_1.u, &uu[uu_offset], &scr1[scr1_offset], &c__0, &xin, &
	    index);
    approx_(triagd_1.v, &vv[vv_offset], &scr1[scr1_offset], &c__0, &xin, &
	    index);

/* DIVERGENCE. */
    if (triagd_1.ipar == 56) {
	dvrg_(&uu[uu_offset], &vv[vv_offset], &zz[zz_offset]);
    }

    if (triagd_1.ipar == 57) {
/* VORTICITY. */
	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		scr1[i__ + j * scr1_dim1] = uu[i__ + j * uu_dim1] * (float)
			-1.;
/* L50: */
	    }
	}
	dvrg_(&vv[vv_offset], &scr1[scr1_offset], &zz[zz_offset]);
    }

    if (triagd_1.ipar == 58) {
/* ENERGY FLUX DENSITY. */
	approx_(triagd_1.z__, &zz[zz_offset], &scr1[scr1_offset], &c__0, &xin,
		 &index);
	i__2 = triagd_1.ny;
	for (j = 1; j <= i__2; ++j) {
	    i__1 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		scr1[i__ + j * scr1_dim1] = uu[i__ + j * uu_dim1] * zz[i__ + 
			j * zz_dim1];
		scr2[i__ + j * scr2_dim1] = vv[i__ + j * vv_dim1] * zz[i__ + 
			j * zz_dim1];
/* L60: */
	    }
	}
	dvrg_(&scr1[scr1_offset], &scr2[scr2_offset], &zz[zz_offset]);
    }

    if (triagd_1.ipar == 77) {
/* STRETCHING DEFORMATION. */
	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/* L70: */
		scr2[i__ + j * scr2_dim1] = vv[i__ + j * vv_dim1] * (float)
			-1.;
	    }
	}
	dvrg_(&uu[uu_offset], &scr2[scr2_offset], &zz[zz_offset]);
    }

/* SHEARING DEFORMATION. */
    if (triagd_1.ipar == 78) {
	dvrg_(&vv[vv_offset], &uu[uu_offset], &zz[zz_offset]);
    }

/* TOTAL DEFORMATION. */
    if (triagd_1.ipar == 88) {
	i__2 = triagd_1.ny;
	for (j = 1; j <= i__2; ++j) {
	    i__1 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		scr2[i__ + j * scr2_dim1] = vv[i__ + j * vv_dim1] * (float)
			-1.;
/* L100: */
	    }
	}
/* STRETCHING DEFORMATION */
	dvrg_(&uu[uu_offset], &scr2[scr2_offset], &zz[zz_offset]);
/* SQUARE IT AND SAVE RESULT. */
	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		scr1[i__ + j * scr1_dim1] = zz[i__ + j * zz_dim1] * zz[i__ + 
			j * zz_dim1];
/* L80: */
		zz[i__ + j * zz_dim1] = (float)0.;
	    }
	}
/* SHEARING DEFORMATION */
	dvrg_(&vv[vv_offset], &uu[uu_offset], &zz[zz_offset]);
/* TOTAL DEFORMATION */
	i__2 = triagd_1.ny;
	for (j = 1; j <= i__2; ++j) {
	    i__1 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* L90: */
		zz[i__ + j * zz_dim1] = sqrt(scr1[i__ + j * scr1_dim1] + zz[
			i__ + j * zz_dim1] * zz[i__ + j * zz_dim1]);
	    }
	}
    }

    if (triagd_1.ipar == 999) {
/* AXIS OF DILATATION */
	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/* L110: */
		scr2[i__ + j * scr2_dim1] = vv[i__ + j * vv_dim1] * (float)
			-1.;
	    }
	}
/* STRETCHING DEFORMATION IN SCR1() */
	dvrg_(&uu[uu_offset], &scr2[scr2_offset], &scr1[scr1_offset]);
/* SHEARING DEFORMATION IN SCR2(). */
	dvrg_(&vv[vv_offset], &uu[uu_offset], &scr2[scr2_offset]);
	i__2 = triagd_1.ny;
	for (j = 1; j <= i__2; ++j) {
	    i__1 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* AXIS OF DILATATION IN RADIANS. */
		if (scr2[i__ + j * scr2_dim1] > (float)0.) {
		    if (scr2[i__ + j * scr2_dim1] < (float)1e-10) {
			zz[i__ + j * zz_dim1] = pi / (float)4.;
		    } else {
			zz[i__ + j * zz_dim1] = pi / (float)4. - atan(scr1[
				i__ + j * scr1_dim1] / scr2[i__ + j * 
				scr2_dim1]);
		    }
		} else {
		    if (scr2[i__ + j * scr2_dim1] > (float)-1e-10) {
			zz[i__ + j * zz_dim1] = pi * (float)3. / (float)4.;
		    } else {
			zz[i__ + j * zz_dim1] = pi * (float)3. / (float)4. - 
				atan(scr1[i__ + j * scr1_dim1] / scr2[i__ + j 
				* scr2_dim1]);
		    }
		}
/* AXIS OF DILATATION IN DEGREES. */
		zz[i__ + j * zz_dim1] = zz[i__ + j * zz_dim1] * (float)180. / 
			pi;
/* L120: */
	    }
	}
    }

/* vorticity stretching (added 4/17/89 cb) */

    if (triagd_1.ipar == 143) {

/* 	Put the divergence into the first scratch array */

	dvrg_(&uu[uu_offset], &vv[vv_offset], &scr1[scr1_offset]);

/* 	Put the u wind into the second scratch array with reversed signs 
*/

	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		scr2[i__ + j * scr2_dim1] = -uu[i__ + j * uu_dim1];
/* L140: */
	    }
/* L130: */
	}

/* 	Calculate the vorticity and put it in array zz */

	dvrg_(&vv[vv_offset], &scr2[scr2_offset], &zz[zz_offset]);

/* 	Calculate the vorticity-divergence product (vorticity stretching) 
*/

	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		zz[i__ + j * zz_dim1] = -scr1[i__ + j * scr1_dim1] * zz[i__ + 
			j * zz_dim1];
/* L160: */
	    }
/* L150: */
	}
    }

    return 0;
} /* lintrp_ */

/* Subroutine */ int eddy_()
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern doublereal avrg1_();
    static integer i__;
    static real umean, vmean;
    extern /* Subroutine */ int starea_();

/* ROUTINE TO GET THE MEAN WIND AND SUBTRACT IT FROM THE MEASURED WIND */
/* SPEED FOR CALCULATING THE WIND EDDY FIELD. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* GET AREA OF INFLUENCE OF EACH PAM STATION. */
    starea_();
/*EAST WIND.  POSITIVE - WIND FROM THE WEST.  NEGATIVE - WIND FROM THE EAS
T.*/
    umean = avrg1_(triagd_1.u);
/*NORTH WIND.  POSITIVE - WIND FROM THE SOUTH.  NEGATIVE - WIND FROM THE N
ORTH.*/
    vmean = avrg1_(triagd_1.v);
/* SUBTRACT MEAN WIND FROM WIND SPEED AT EACH PAM STATION. */
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! triagd_1.deleted[i__ - 1]) {
	    triagd_1.u[i__ - 1] -= umean;
	    triagd_1.v[i__ - 1] -= vmean;
	}
/* L10: */
    }
    return 0;
} /* eddy_ */

doublereal avrg1_(array)
real *array;
{
    /* System generated locals */
    integer i__1;
    real ret_val, r__1;

    /* Local variables */
    static real suma;
    static integer i__;
    static real sum;


/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */
    /* Parameter adjustments */
    --array;

    /* Function Body */
    suma = (float)0.;
    sum = (float)0.;
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! triagd_1.deleted[i__ - 1] && (r__1 = array[i__] - 
		triagd_1.flag__, dabs(r__1)) > (float)1e-6) {
	    suma += triagd_1.area[i__ - 1];
	    sum += array[i__] * triagd_1.area[i__ - 1];
	}
/* L100: */
    }
    ret_val = triagd_1.flag__;
    if (dabs(suma) > (float)1e-6) {
	ret_val = sum / suma;
    }
    return ret_val;
} /* avrg1_ */

/* Subroutine */ int intrp_(grid, scr1, mode)
real *grid, *scr1;
integer *mode;
{
    /* System generated locals */
    integer grid_dim1, grid_offset, scr1_dim1, scr1_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    extern /* Subroutine */ int lfltr_(), extrp_();
    static real x1, y1, xx, yy;
    static integer ntr;

/* 	SCALER INTERPOLATION AND FILTER ROUTINE */

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* 	INTERPOLATION */
    /* Parameter adjustments */
    scr1_dim1 = triagd_1.nx;
    scr1_offset = scr1_dim1 + 1;
    scr1 -= scr1_offset;
    grid_dim1 = triagd_1.nx;
    grid_offset = grid_dim1 + 1;
    grid -= grid_offset;

    /* Function Body */
    i__1 = triagd_1.ny;
    for (j = 1; j <= i__1; ++j) {
	xx = triagd_1.x0;
	yy = triagd_1.y0 + (j - 1) * triagd_1.dy;
	i__2 = triagd_1.nx;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (triagd_1.jw[i__ + j * 25 - 26] >= 0) {
		ntr = triagd_1.jw[i__ + j * 25 - 26];
		if (triagd_1.naprox == 0) {
/* LINEAR INTERPOLATION */
		    grid[i__ + j * grid_dim1] = triagd_1.weight[ntr - 1] + xx 
			    * triagd_1.weight[ntr + 799] + yy * 
			    triagd_1.weight[ntr + 1599];
		} else {
/* QUADRATIC INTERPOLATION */
		    x1 = (xx - triagd_1.xmin) * (float)9. / (triagd_1.xmax - 
			    triagd_1.xmin) + (float)1.;
		    y1 = (yy - triagd_1.ymin) * (float)9. / (triagd_1.ymax - 
			    triagd_1.ymin) + (float)1.;
		    grid[i__ + j * grid_dim1] = triagd_1.weight[ntr - 1] + x1 
			    * triagd_1.weight[ntr + 799] + y1 * 
			    triagd_1.weight[ntr + 1599] + x1 * y1 * 
			    triagd_1.weight[ntr + 2399] + x1 * x1 * 
			    triagd_1.weight[ntr + 3199] + y1 * y1 * 
			    triagd_1.weight[ntr + 3999];
		}
	    }
	    xx += triagd_1.dx;
/* L100: */
	}
    }
/* 	EXTRAPOLATE */
    if (triagd_1.ipar == 56 || triagd_1.ipar == 57 || triagd_1.ipar == 58 || 
	    triagd_1.ipar == 77 || triagd_1.ipar == 78 || triagd_1.nfltr == 1)
	     {
	extrp_(&grid[grid_offset]);
    }
/* 	FILTER AND FLAG */
    if (triagd_1.nfltr == 1) {
	lfltr_(&scr1[scr1_offset], &grid[grid_offset]);
    }
    if (*mode != 0) {
	i__2 = triagd_1.ny;
	for (j = 1; j <= i__2; ++j) {
	    i__1 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (triagd_1.jw[i__ + j * 25 - 26] < 0) {
		    grid[i__ + j * grid_dim1] = triagd_1.flag__;
		}
/* L200: */
	    }
	}
    }
    return 0;
} /* intrp_ */

/*---------------------------------------------------------------------------
---*/
/* Subroutine */ int extrp_(array)
real *array;
{
    /* Format strings */
    static char fmt_10[] = "(\002 *** WARNING: MIDPOINT OF GRID IS NOT WITHI\
N A\002,\002 TRIANGLE. ***\002/\002  FILTER AND DERIVATIVE CALCULATIONS MAY \
BE\002,\002 INCORRECT.\002)";

    /* System generated locals */
    integer array_dim1, array_offset, i__1, i__2;

#ifdef FIO
    /* Builtin functions */
    integer s_wsfe(), e_wsfe();
#endif

    /* Local variables */
    static integer midx, midy, k;
    static real z1;
    static integer ix, iy, jx, jy, mx, my, jxh, jyh, jxl, jyl, ixm1, iym1, 
	    jxp1, jyp1;

    /* Fortran I/O blocks */
    static cilist io___194 = { 0, 6, 0, fmt_10, 0 };



/* 	WES WILSON		APRIL, 1980 */
/* 	THIS IS A SIMPLE EXTRAPOLATION ROUTINE WHICH DAMPS THE DERIVATIVES */
/* 	OF THE EXTRAPOLATED DATA. */

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    /* Parameter adjustments */
    array_dim1 = triagd_1.nx;
    array_offset = array_dim1 + 1;
    array -= array_offset;

    /* Function Body */
    midx = triagd_1.nx / 2;
    midy = triagd_1.ny / 2;
    if (triagd_1.jw[midx + midy * 25 - 26] < 0) {
#ifdef FIO
	s_wsfe(&io___194);
	e_wsfe();
#endif
    }
/* 	POSITIVE X-AXIS FROM THE MIDPOINT */
    jxh = triagd_1.nx;
    i__1 = triagd_1.nx;
    for (ix = midx; ix <= i__1; ++ix) {
	jx = ix;
	if (triagd_1.jw[jx + midy * 25 - 26] < 0) {
	    goto L260;
	}
/* L250: */
    }
    goto L280;
L260:
    jxh = jx - 1;
    z1 = array[jxh + midy * array_dim1];
    i__1 = triagd_1.nx;
    for (k = jx; k <= i__1; ++k) {
	array[k + midy * array_dim1] = z1;
/* L270: */
    }
L280:
/* 	NEGATIVE X-AXIS FROM THE MIDPOINT */
    jxl = 1;
    jx = midx + 1;
    i__1 = midx;
    for (ix = 1; ix <= i__1; ++ix) {
	--jx;
	if (triagd_1.jw[jx + midy * 25 - 26] < 0) {
	    goto L300;
	}
/* L290: */
    }
    goto L320;
L300:
    jxl = jx + 1;
    z1 = array[jxl + midy * array_dim1];
    i__1 = jx;
    for (k = 1; k <= i__1; ++k) {
	array[k + midy * array_dim1] = z1;
/* L310: */
    }
L320:
/* 	POSITIVE Y-AXIS FROM THE MIDPOINT */
    jyh = triagd_1.ny;
    i__1 = triagd_1.ny;
    for (iy = midy; iy <= i__1; ++iy) {
	jy = iy;
	if (triagd_1.jw[midx + jy * 25 - 26] < 0) {
	    goto L340;
	}
/* L330: */
    }
    goto L360;
L340:
    jyh = jy - 1;
    z1 = array[midx + jyh * array_dim1];
    i__1 = triagd_1.ny;
    for (k = jy; k <= i__1; ++k) {
	array[midx + k * array_dim1] = z1;
/* L350: */
    }
L360:
/* 	NEGATIVE Y-AXIS FROM THE MIDPOINT */
    jyl = 1;
    jy = midy + 1;
    i__1 = midy;
    for (iy = 1; iy <= i__1; ++iy) {
	--jy;
	if (triagd_1.jw[midx + jy * 25 - 26] < 0) {
	    goto L380;
	}
/* L370: */
    }
    goto L400;
L380:
    jyl = jy + 1;
    z1 = array[midx + jyl * array_dim1];
    i__1 = jy;
    for (k = 1; k <= i__1; ++k) {
	array[midx + k * array_dim1] = z1;
/* L390: */
    }
L400:
/* 	FIRST QUADRANT */
    mx = midx + 1;
    my = midy + 1;
    i__1 = triagd_1.ny;
    for (iy = my; iy <= i__1; ++iy) {
	i__2 = triagd_1.nx;
	for (ix = mx; ix <= i__2; ++ix) {
	    if (triagd_1.jw[ix + iy * 25 - 26] > 0) {
		goto L410;
	    }
	    ixm1 = ix - 1;
	    iym1 = iy - 1;
	    array[ix + iy * array_dim1] = (array[ixm1 + iy * array_dim1] + 
		    array[ix + iym1 * array_dim1]) / (float)2.;
L410:
	    ;
	}
    }
/* 	SECOND QUADRANT */
    mx = midx - 1;
    i__2 = triagd_1.ny;
    for (iy = my; iy <= i__2; ++iy) {
	jx = midx;
	i__1 = mx;
	for (ix = 1; ix <= i__1; ++ix) {
	    --jx;
	    if (triagd_1.jw[jx + iy * 25 - 26] > 0) {
		goto L420;
	    }
	    jxp1 = jx + 1;
	    iym1 = iy - 1;
	    array[jx + iy * array_dim1] = (array[jxp1 + iy * array_dim1] + 
		    array[jx + iym1 * array_dim1]) / (float)2.;
L420:
	    ;
	}
    }
/* 	THIRD QUADRANT */
    my = midy - 1;
    jy = midy;
    i__1 = my;
    for (iy = 1; iy <= i__1; ++iy) {
	--jy;
	jx = midx;
	i__2 = mx;
	for (ix = 1; ix <= i__2; ++ix) {
	    --jx;
	    if (triagd_1.jw[jx + jy * 25 - 26] > 0) {
		goto L430;
	    }
	    jxp1 = jx + 1;
	    jyp1 = jy + 1;
	    array[jx + jy * array_dim1] = (array[jxp1 + jy * array_dim1] + 
		    array[jx + jyp1 * array_dim1]) / (float)2.;
L430:
	    ;
	}
    }
/* 	FOURTH QUADRANT */
    mx = midx + 1;
    jy = midy;
    i__2 = my;
    for (iy = 1; iy <= i__2; ++iy) {
	--jy;
	i__1 = triagd_1.nx;
	for (ix = mx; ix <= i__1; ++ix) {
	    if (triagd_1.jw[ix + jy * 25 - 26] > 0) {
		goto L440;
	    }
	    ixm1 = ix - 1;
	    jyp1 = jy + 1;
	    array[ix + jy * array_dim1] = (array[ixm1 + jy * array_dim1] + 
		    array[ix + jyp1 * array_dim1]) / (float)2.;
L440:
	    ;
	}
    }
    return 0;
} /* extrp_ */

/* Subroutine */ int lfltr_(array1, array2)
real *array1, *array2;
{
    /* System generated locals */
    integer array2_dim1, array2_offset, i__1, i__2, i__3, i__4, i__5, i__6, 
	    i__7;

    /* Local variables */
    static integer main, kord[5], i__, j, k, m, n, ndimx;
    static real ysave;
    static integer mstep, k1, m1, m2, m3, n3, istop, jstop, kstop, mstop, 
	    kstrt, mstrt, kn, ln, mpyrmd, mm1, mm2, np1, np2;
    static real ym1, ym2;
    static integer ijk, net[5], nns[3];
    static real ykn, yln;
    static integer knm1, knm2, knm3, lnm1, lnm2, lnm3;
    static real ykn1, yln1;

/*               JIM LEISE 8/80 */
/*   *********************************************************** */
/*   THIS IS A LINEAR LOW-PASS FILTER WHICH NEEDS NO EXTRA */
/*   ARRAY SPACE. THUS, THE FILTERED ANSWER IS RETURNED IN THE SAME */
/*   ARRAY ARRAY2(NX,NY,N3) THAT THE DATA IS INPUT. THE CENTRAL FILTER */
/*   IS A LINEAR 5-PT FILTER AND THE BOUNDARY FILTER IS COMPUTED */
/*   USING A MIRROR EXTENSION. THUS, THE TOTAL FILTER IS LINEAR. */
/*         ********** NSTEP CONTROL FOR 1-DIM ********** */
/*        STEP RESTRICTION:  5*2**(NSTEP-1) .LE. MAX(NX,NY,N3) */
/*         PASSBAND .LE. 2**(NSTEP+2)  POINTS/CYCLE */
/*          STOPBAND .GE. 2**(NSTEP)  POINTS/CYCLE. */
/*         ************ MULTIDIMENSIONAL USE ************ */
/*   PARAMETER CONTROL FOR THE THREE DIMENSIONS CAN BE REALIZED */
/*   VIA COMON/FLTRPL/ WHERE NS CORRESPONDS TO NSTEP. IF THIS */
/*   COMON IS NOT USED, THE VALUES OF NS ARE DEFAULTED TO NSTEP */
/*   -I.E. NSTEP IS USED IN PLACE OF ANY ZEROS. */
/*   *********************************************************** */
/* THE DIMENSION OF ARRAY2 HAS BEEN CHANGED FROM (NX, NY) TO (1) */
/* THE ARRAY IS USED AS ONE DIMENSIONAL INSIDE AND IS PASSED */
/* WITH TWO DIMENSIONAL INFORMATION. */
/* MICHAEL CARPENTER SEPTEMBER 25, 1982 */
/*   *********************************************************** */

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    /* Parameter adjustments */
    array2_dim1 = triagd_1.nx;
    array2_offset = array2_dim1 + 1;
    array2 -= array2_offset;
    --array1;

    /* Function Body */
    i__1 = triagd_1.ny;
    for (j = 1; j <= i__1; ++j) {
	i__2 = triagd_1.nx;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    ijk = i__ + (j - 1) * triagd_1.nx;
	    array1[ijk] = array2[i__ + j * array2_dim1];
/* L5: */
	}
    }

    if (triagd_1.nstep <= 0) {
	return 0;
    }
    n3 = 1;
/*   INITIALIZE THE 3-D ARITHMETIC. */
    ndimx = 1;
    if (triagd_1.ny > 1) {
	ndimx = 2;
    }
    kord[0] = max(1,triagd_1.nx);
    kord[1] = max(1,triagd_1.ny);
    kord[2] = max(1,n3);
    kord[3] = kord[0];
    kord[4] = kord[1];
    net[0] = 1;
    net[1] = kord[0];
    net[2] = kord[0] * kord[1];
    net[3] = net[0];
    net[4] = net[1];
/*   DEFAULT PARAMETER TRANSFER. */
    i__2 = ndimx;
    for (n = 1; n <= i__2; ++n) {
	nns[n - 1] = triagd_1.nstep;
/* L10: */
    }
    mpyrmd = triagd_1.nstep + triagd_1.nstep - 1;
    if (mpyrmd <= 0) {
	return 0;
    }
    mstep = (mpyrmd + 1) / 2;
/*   ***** START THE MAIN LOOP ***** */
    k1 = 1;
    i__2 = mpyrmd;
    for (main = 1; main <= i__2; ++main) {
	i__1 = ndimx;
	for (n = 1; n <= i__1; ++n) {
/*   SAMPLING CHECKS. */
	    if (k1 * 10 > kord[n - 1]) {
/* Computing MIN */
		i__3 = nns[n - 1];
		nns[n - 1] = min(i__3,main);
	    }
	    if (main >= nns[n - 1] && mpyrmd - main >= nns[n - 1]) {
		goto L40;
	    }
/*   THE 3-D ARITHMETIC. */
	    np1 = n + 1;
	    np2 = n + 2;
	    m1 = k1 * net[n - 1];
	    m2 = m1 + m1;
	    m3 = m2 + m1;
	    istop = kord[n];
	    jstop = kord[n + 1];
	    i__3 = istop;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		i__4 = jstop;
		for (j = 1; j <= i__4; ++j) {
		    kstrt = (i__ - 1) * net[np1 - 1] + 1 + (j - 1) * net[np2 
			    - 1];
		    kstop = kstrt + (kord[n - 1] - 1) * net[n - 1];
		    kn = kstrt - net[n - 1];
		    i__5 = k1;
		    for (k = 1; k <= i__5; ++k) {
			kn += net[n - 1];
			ln = kn + (kstop - kn) / m1 * m1;
			knm1 = kn + m1;
			knm2 = kn + m2;
			knm3 = kn + m3;
			lnm1 = ln - m1;
			lnm2 = ln - m2;
			lnm3 = ln - m3;
/*   FILTER THE ENDS USING A MIRROR EXTENSION. */
			ykn = array1[kn] * (float).875 + array1[knm1] * (
				float).1875 - array1[knm2] * (float).0625;
			yln = array1[ln] * (float).875 + array1[lnm1] * (
				float).1875 - array1[lnm2] * (float).0625;
			ykn1 = array1[knm1] * (float).5625 + (array1[kn] + 
				array1[knm2]) * (float).25 - array1[knm3] * (
				float).0625;
			yln1 = array1[lnm1] * (float).5625 + (array1[ln] + 
				array1[lnm2]) * (float).25 - array1[lnm3] * (
				float).0625;
/*   DO THE CENTRAL 5-PT FILTER. */
			ym2 = array1[kn];
			ym1 = array1[knm1];
			mstrt = knm2;
			mstop = lnm2;
			i__6 = mstop;
			i__7 = m1;
			for (m = mstrt; i__7 < 0 ? m >= i__6 : m <= i__6; m +=
				 i__7) {
			    mm1 = m + m1;
			    mm2 = m + m2;
			    ysave = array1[m];
			    array1[m] = array1[m] * (float).625 + (ym1 + 
				    array1[mm1]) * (float).25 - (ym2 + array1[
				    mm2]) * (float).0625;
			    ym2 = ym1;
			    ym1 = ysave;
/* L20: */
			}
			array1[knm1] = ykn1;
			array1[lnm1] = yln1;
			array1[kn] = ykn;
			array1[ln] = yln;
/* L30: */
		    }
		}
	    }
L40:
	    ;
	}
/*   UPDATE THE SAMPLING INCREMENT. */
	k1 += k1;
	if (main >= mstep) {
	    k1 /= 4;
	}
/* L50: */
    }
    i__2 = triagd_1.ny;
    for (j = 1; j <= i__2; ++j) {
	i__1 = triagd_1.nx;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ijk = i__ + (j - 1) * triagd_1.nx;
	    array2[i__ + j * array2_dim1] = array1[ijk];
/* L60: */
	}
    }
    return 0;
} /* lfltr_ */

/* Subroutine */ int dvrg_(a1, a2, dvg)
real *a1, *a2, *dvg;
{
    /* System generated locals */
    integer a1_dim1, a1_offset, a2_dim1, a2_offset, dvg_dim1, dvg_offset, 
	    i__1, i__2, i__3, i__4, i__5, i__6;

    /* Local variables */
    static integer i__, j;
    extern doublereal du_(), dv_();
    static real ddx, ddy;
    static integer nxm1, nym1;

/*       WES WILSON              JUNE,1979 */
/*     THIS ROUTINE COMPUTES THE DIVERGENCE OF THE FIELD (A1,A2) ON */
/*     THE REGULAR GRID OF SIZE (NX,NY) AND WITH X-INCREMENT DX AND */
/*     Y-INCREMENT DY.  THE DERIVATIVES ARE APPROXIMATED BY CENTERED */
/*     DIFFERENCES WHERE POSSIBLE AND BY ONE-SIDED DIFFERENCES NEAR */
/*     THE BOUNDARY AS NECESSARY. */

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/*     COMPUTE THE DIVERGENCE AROUND THE BOUNDARY. */
    /* Parameter adjustments */
    dvg_dim1 = triagd_1.nx;
    dvg_offset = dvg_dim1 + 1;
    dvg -= dvg_offset;
    a2_dim1 = triagd_1.nx;
    a2_offset = a2_dim1 + 1;
    a2 -= a2_offset;
    a1_dim1 = triagd_1.nx;
    a1_offset = a1_dim1 + 1;
    a1 -= a1_offset;

    /* Function Body */
    ddx = triagd_1.dx + triagd_1.dx;
    ddy = triagd_1.dy + triagd_1.dy;
    nxm1 = triagd_1.nx - 1;
    nym1 = triagd_1.ny - 1;
    dvg[dvg_dim1 + 1] = du_(&a1[a1_offset], &c__2, &c__1, &c__1) / 
	    triagd_1.dx + dv_(&a2[a2_offset], &c__1, &c__2, &c__1) / 
	    triagd_1.dy;
    dvg[triagd_1.nx + dvg_dim1] = du_(&a1[a1_offset], &triagd_1.nx, &nxm1, &
	    c__1) / triagd_1.dx + dv_(&a2[a2_offset], &triagd_1.nx, &c__2, &
	    c__1) / triagd_1.dy;
    dvg[triagd_1.ny * dvg_dim1 + 1] = du_(&a1[a1_offset], &c__2, &c__1, &
	    triagd_1.ny) / triagd_1.dx + dv_(&a2[a2_offset], &c__1, &
	    triagd_1.ny, &nym1) / triagd_1.dy;
    dvg[triagd_1.nx + triagd_1.ny * dvg_dim1] = du_(&a1[a1_offset], &
	    triagd_1.nx, &nxm1, &triagd_1.ny) / triagd_1.dx + dv_(&a2[
	    a2_offset], &triagd_1.nx, &triagd_1.ny, &nym1) / triagd_1.dy;
    i__1 = nxm1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	i__2 = i__ + 1;
	i__3 = i__ - 1;
	dvg[i__ + dvg_dim1] = du_(&a1[a1_offset], &i__2, &i__3, &c__1) / ddx 
		+ dv_(&a2[a2_offset], &i__, &c__2, &c__1) / triagd_1.dy;
	i__2 = i__ + 1;
	i__3 = i__ - 1;
	dvg[i__ + triagd_1.ny * dvg_dim1] = du_(&a1[a1_offset], &i__2, &i__3, 
		&triagd_1.ny) / ddx + dv_(&a2[a2_offset], &i__, &triagd_1.ny, 
		&nym1) / triagd_1.dy;
/* L100: */
    }
    i__1 = nym1;
    for (j = 2; j <= i__1; ++j) {
	i__2 = j + 1;
	i__3 = j - 1;
	dvg[j * dvg_dim1 + 1] = du_(&a1[a1_offset], &c__2, &c__1, &j) / 
		triagd_1.dx + dv_(&a2[a2_offset], &c__1, &i__2, &i__3) / ddy;
	i__2 = j + 1;
	i__3 = j - 1;
	dvg[triagd_1.nx + j * dvg_dim1] = du_(&a1[a1_offset], &triagd_1.nx, &
		nxm1, &j) / triagd_1.dx + dv_(&a2[a2_offset], &triagd_1.nx, &
		i__2, &i__3) / ddy;
/* L200: */
    }
/*     COMPUTE THE DIVERGENCE ON THE INTERIOR. */
    i__1 = nym1;
    for (j = 2; j <= i__1; ++j) {
	i__2 = nxm1;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    i__3 = i__ + 1;
	    i__4 = i__ - 1;
	    i__5 = j + 1;
	    i__6 = j - 1;
	    dvg[i__ + j * dvg_dim1] = du_(&a1[a1_offset], &i__3, &i__4, &j) / 
		    ddx + dv_(&a2[a2_offset], &i__, &i__5, &i__6) / ddy;
/* L300: */
	}
    }
    return 0;
} /* dvrg_ */

doublereal du_(array, i__, j, k)
real *array;
integer *i__, *j, *k;
{
    /* System generated locals */
    integer array_dim1, array_offset;
    real ret_val;

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    /* Parameter adjustments */
    array_dim1 = triagd_1.nx;
    array_offset = array_dim1 + 1;
    array -= array_offset;

    /* Function Body */
    ret_val = array[*i__ + *k * array_dim1] - array[*j + *k * array_dim1];
    return ret_val;
} /* du_ */

doublereal dv_(array, i__, j, k)
real *array;
integer *i__, *j, *k;
{
    /* System generated locals */
    integer array_dim1, array_offset;
    real ret_val;

/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    /* Parameter adjustments */
    array_dim1 = triagd_1.nx;
    array_offset = array_dim1 + 1;
    array -= array_offset;

    /* Function Body */
    ret_val = array[*i__ + *j * array_dim1] - array[*i__ + *k * array_dim1];
    return ret_val;
} /* dv_ */

/* Subroutine */ int approx_(pam, grid, scr1, mode, xin, index)
real *pam, *grid, *scr1;
integer *mode;
real *xin;
integer *index;
{
    /* System generated locals */
    integer grid_dim1, grid_offset, scr1_dim1, scr1_offset;

    /* Local variables */
    extern /* Subroutine */ int eval_(), quad_();
    static real b[21];
    extern /* Subroutine */ int patch_(), genwt_(), intrp_(), polyfit_();

/* ROUTINE TO CALCULATE THE APPROXIMATION TO THE RAW PAM DATA USING EITHER
 */
/* THE LINEAR INTERPOLATION TECHNIQUE OR A POLYNOMIAL REGRESSION. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* LINEAR INTERPOLATION */
    /* Parameter adjustments */
    xin -= 22;
    scr1_dim1 = triagd_1.nx;
    scr1_offset = scr1_dim1 + 1;
    scr1 -= scr1_offset;
    grid_dim1 = triagd_1.nx;
    grid_offset = grid_dim1 + 1;
    grid -= grid_offset;
    --pam;

    /* Function Body */
    if (triagd_1.naprox == 0) {
	genwt_(&pam[1]);
	intrp_(&grid[grid_offset], &scr1[scr1_offset], mode);
    }
/* POLYNOMIAL CURVE-FIT */
    if (triagd_1.naprox == 1) {
	polyfit_(&pam[1], &xin[22], index, b);
	eval_(&grid[grid_offset], b, index, mode);
    }
/* QUADRATIC INTERPOLATION */
    if (triagd_1.naprox == 2) {
	quad_(&pam[1]);
	intrp_(&grid[grid_offset], &scr1[scr1_offset], mode);
    }
/* FIT A CURVE TO PATCHES OF THE GRID. */
    if (triagd_1.naprox == 3) {
	patch_(&pam[1], &grid[grid_offset], mode, &xin[22]);
    }
    return 0;
} /* approx_ */

/* Subroutine */ int test_()
{
    /* Format strings */
    static char fmt_10[] = "(\002 CHANGE DATA(0=NO,1=YES)\002)";
    static char fmt_30[] = "(\002 ENTER NO. STATIONS\002)";
    static char fmt_50[] = "(\002 ENTER X,Y,Z FOR STATION\002,i6)";

    /* System generated locals */
    integer i__1;

#ifdef FIO
    /* Builtin functions */
    integer s_wsfe(), e_wsfe(), s_rsle(), do_lio(), e_rsle();
    integer do_fio();
#endif

    /* Local variables */
    static integer i__, ix;

    /* Fortran I/O blocks */
    static cilist io___261 = { 0, 6, 0, fmt_10, 0 };
    static cilist io___262 = { 0, 5, 0, 0, 0 };
    static cilist io___264 = { 0, 6, 0, fmt_30, 0 };
    static cilist io___265 = { 0, 5, 0, 0, 0 };
    static cilist io___267 = { 0, 6, 0, fmt_50, 0 };
    static cilist io___268 = { 0, 5, 0, 0, 0 };


/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

L1:
#ifdef FIO
    s_wsfe(&io___261);
    e_wsfe();
    s_rsle(&io___262);
    do_lio(&c__3, &c__1, (char *)&ix, (ftnlen)sizeof(integer));
    e_rsle();
#endif
    if (ix != 0 && ix != 1) {
	goto L1;
    }
    if (ix == 0) {
	return 0;
    }

/* L20: */
#ifdef FIO
    s_wsfe(&io___264);
    e_wsfe();
    s_rsle(&io___265);
    do_lio(&c__3, &c__1, (char *)&triagd_1.np, (ftnlen)sizeof(integer));
    e_rsle();
#endif
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	triagd_1.deleted[i__ - 1] = FALSE_;
/* L40: */
#ifdef FIO
	s_wsfe(&io___267);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	e_wsfe();
	s_rsle(&io___268);
	do_lio(&c__4, &c__1, (char *)&triagd_1.x[i__ - 1], (ftnlen)sizeof(
		real));
	do_lio(&c__4, &c__1, (char *)&triagd_1.y[i__ - 1], (ftnlen)sizeof(
		real));
	do_lio(&c__4, &c__1, (char *)&triagd_1.z__[i__ - 1], (ftnlen)sizeof(
		real));
	e_rsle();
#endif
/* L100: */
    }
    return 0;
} /* test_ */

/* Subroutine */ int quad_(pam)
real *pam;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int quadwt_();

/* ROUTINE TO COMPUTE THE INTERPOLATION WEIGHTS FOR ALL TRIANGLES */
/* USING A QUADRATIC INTERPOLATION POLYNOMIAL. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* LOOP THROUGH THE TRIANGLES. */
    /* Parameter adjustments */
    --pam;

    /* Function Body */
    i__1 = triagd_1.itrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	quadwt_(&i__, &pam[1]);
/* L10: */
    }
    return 0;
} /* quad_ */

/* Subroutine */ int quadwt_(ntr, pam)
integer *ntr;
real *pam;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ista[6], jsta;
    static real b[21];
    static integer i__, j, k;
    static real w[800];
    static integer index, istns;
    static real x1, x2, x3, y1, y2, y3;
    static integer iadjtr[3];
    static real xx, yy;
    extern /* Subroutine */ int curfit_();
    static real val, xin[16800]	/* was [21][800] */;

/* ROUTINE TO CALCULATE THE INTERPOLATION WEIGHTS FOR TRIANGLE NTR */
/* USING A QUADRATIC INTERPOLATION POLYNOMIAL.  PAM() IS THE FIELD */
/* DATA AT THE PAM STATIONS */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* FIND THE PAM STATIONS COMPRISING TRIANGLE NTR AND THE THREE */
/* TRIANGLES ADJACENT TO NTR. */
/* INDEX IS ELEMENT OF XIN() IN WHICH THE INDEPENDENT VARIABLE IS STORED. 
*/
    /* Parameter adjustments */
    --pam;

    /* Function Body */
    index = 6;
    for (i__ = 1; i__ <= 3; ++i__) {
/* STATIONS IN NTR. */
	ista[i__ - 1] = triagd_1.itrngl[*ntr + i__ * 800 - 801];
/* ADJACENT TRIANGLES. */
	iadjtr[i__ - 1] = triagd_1.itrngl[*ntr + (i__ + 3) * 800 - 801];
/* SET FLAG IF NOT THREE ADJACENT TRIANGLES. */
	if (iadjtr[i__ - 1] <= 0) {
	    index = 3;
	}
/* L10: */
    }
/* USE LINEAR INTERPOLATION IF DON'T HAVE 3 ADJACENT TRIANGLES. */
    if (index == 3) {
	goto L100;
    }

/* FIND THE STATIONS OF THE ADJACENT TRIANGLES THAT ARE NOT IN NTR. */
    istns = 3;
    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
/* JTH STATION OF ITH ADJACENT TRIANGLE. */
	    jsta = triagd_1.itrngl[iadjtr[i__ - 1] + j * 800 - 801];
/* SEE IF ALREADY HAVE IT. */
	    i__1 = istns;
	    for (k = 1; k <= i__1; ++k) {
		if (ista[k - 1] == jsta) {
		    goto L20;
		}
/* L30: */
	    }
/* JSTA IS A NEW STATION. */
	    ++istns;
	    ista[istns - 1] = jsta;
L20:
	    ;
	}
/* L50: */
    }
/* USE A LINEAR INTERPOLATION IF DON'T HAVE 6 DIFFERENT PAM STATIONS. */
    if (istns < 6) {
	index = 3;
    }

/*IF NOT THREE ADJACENT TRIANGLES, INTERPOLATE USING A LINEAR INTERPOLATIO
N.*/
L100:
    if (index != 6) {
	x1 = (triagd_1.x[ista[0] - 1] - triagd_1.xmin) * (float)9. / (
		triagd_1.xmax - triagd_1.xmin) + (float)1.;
	x2 = (triagd_1.x[ista[1] - 1] - triagd_1.xmin) * (float)9. / (
		triagd_1.xmax - triagd_1.xmin) + (float)1.;
	x3 = (triagd_1.x[ista[2] - 1] - triagd_1.xmin) * (float)9. / (
		triagd_1.xmax - triagd_1.xmin) + (float)1.;
	y1 = (triagd_1.y[ista[0] - 1] - triagd_1.ymin) * (float)9. / (
		triagd_1.ymax - triagd_1.ymin) + (float)1.;
	y2 = (triagd_1.y[ista[1] - 1] - triagd_1.ymin) * (float)9. / (
		triagd_1.ymax - triagd_1.ymin) + (float)1.;
	y3 = (triagd_1.y[ista[2] - 1] - triagd_1.ymin) * (float)9. / (
		triagd_1.ymax - triagd_1.ymin) + (float)1.;
	triagd_1.weight[*ntr + 1599] = pam[ista[0]] * (x3 - x2) + pam[ista[1]]
		 * (x1 - x3) + pam[ista[2]] * (x2 - x1);
	triagd_1.weight[*ntr + 1599] /= y1 * (x3 - x2) + y2 * (x1 - x3) + y3 *
		 (x2 - x1);
	triagd_1.weight[*ntr + 799] = pam[ista[1]] - pam[ista[0]] + 
		triagd_1.weight[*ntr + 1599] * (y1 - y2);
	triagd_1.weight[*ntr + 799] /= x2 - x1;
	triagd_1.weight[*ntr - 1] = pam[ista[0]] - triagd_1.weight[*ntr + 
		1599] * y1;
	triagd_1.weight[*ntr - 1] -= x1 * (pam[ista[1]] - pam[ista[0]] + 
		triagd_1.weight[*ntr + 1599] * (y1 - y2)) / (x2 - x1);
	triagd_1.weight[*ntr + 2399] = (float)0.;
	triagd_1.weight[*ntr + 3199] = (float)0.;
	triagd_1.weight[*ntr + 3999] = (float)0.;
	return 0;
    }

/* WEIGHT THE STATIONS OF THE INNER TRIANGLE MORE THAN THE STATIONS */
/* OF THE ADJACENT TRIANGLES. */
    for (i__ = 1; i__ <= 3; ++i__) {
	w[i__ - 1] = (float)1.;
/* L190: */
	w[i__ + 2] = (float).5;
    }
/* COMPUTE THE MATRIX INPUT TO THE CURVE-FIT SUBROUTINE. */
    i__1 = istns;
    for (i__ = 1; i__ <= i__1; ++i__) {
	xx = (triagd_1.x[ista[i__ - 1] - 1] - triagd_1.xmin) * (float)9. / (
		triagd_1.xmax - triagd_1.xmin) + (float)1.;
	yy = (triagd_1.y[ista[i__ - 1] - 1] - triagd_1.ymin) * (float)9. / (
		triagd_1.ymax - triagd_1.ymin) + (float)1.;
	xin[i__ * 21 - 21] = xx;
	xin[i__ * 21 - 20] = yy;
	xin[i__ * 21 - 19] = xx * yy;
	xin[i__ * 21 - 18] = xx * xx;
	xin[i__ * 21 - 17] = yy * yy;
	xin[index + i__ * 21 - 22] = pam[ista[i__ - 1]];
/* L200: */
    }
/* FIT THE CURVE */
    val = (float).01;
    if (triagd_1.nstep == 2) {
	val = (float).5;
    }
    curfit_(&index, &index, xin, w, &val, &val, b);
/* SAVE THE COEFFICIENTS OF THE EQUATION. */
    triagd_1.weight[*ntr - 1] = b[index - 1];
    i__1 = index - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	triagd_1.weight[*ntr + (i__ + 1) * 800 - 801] = b[i__ - 1];
/* L210: */
    }
    return 0;
} /* quadwt_ */

/* Subroutine */ int patch_(pam, grid, mode, xin)
real *pam, *grid;
integer *mode;
real *xin;
{
    /* Format strings */
    static char fmt_3[] = "(\002 ENTER NSIZE\002)";

    /* System generated locals */
    integer grid_dim1, grid_offset, i__1, i__2, i__3, i__4, i__5, i__6;

#ifdef FIO
    /* Builtin functions */
    integer s_wsfe(), e_wsfe(), s_rsle(), do_lio(), e_rsle();
#endif

    /* Local variables */
    static integer npts, i__, j, k, l, igrpt[300]	/* was [150][2] */, 
	    nsize;
    extern /* Subroutine */ int fit_();

    /* Fortran I/O blocks */
    static cilist io___290 = { 0, 6, 0, fmt_3, 0 };
    static cilist io___291 = { 0, 5, 0, 0, 0 };


/* FIT A CURVE TO EACH PATCH OF NSIZE BY NSIZE GRID POINTS AND USE */
/* IT TO INTERPOLATE THE FIELD VALUES AT THE GRID POINTS. */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    /* Parameter adjustments */
    xin -= 22;
    grid_dim1 = triagd_1.nx;
    grid_offset = grid_dim1 + 1;
    grid -= grid_offset;
    --pam;

    /* Function Body */
#ifdef FIO
    s_wsfe(&io___290);
    e_wsfe();
    s_rsle(&io___291);
    do_lio(&c__3, &c__1, (char *)&nsize, (ftnlen)sizeof(integer));
    e_rsle();
#endif
    i__1 = triagd_1.ny;
    i__2 = nsize;
    for (j = 1; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {
	i__3 = triagd_1.nx;
	i__4 = nsize;
	for (i__ = 1; i__4 < 0 ? i__ >= i__3 : i__ <= i__3; i__ += i__4) {
	    npts = 0;
	    i__5 = nsize - 1;
	    for (k = 0; k <= i__5; ++k) {
		i__6 = nsize - 1;
		for (l = 0; l <= i__6; ++l) {
		    if (k + i__ <= triagd_1.nx && l + j <= triagd_1.ny) {
			if (triagd_1.jw[k + i__ + (l + j) * 25 - 26] > 0) {
			    ++npts;
			    igrpt[npts - 1] = k + i__;
			    igrpt[npts + 149] = l + j;
			} else {
			    if (*mode != 0) {
				grid[k + i__ + (l + j) * grid_dim1] = 
					triagd_1.flag__;
			    }
			}
		    }
/* L80: */
		}
	    }
	    if (npts > 0) {
		fit_(&npts, igrpt, &pam[1], &grid[grid_offset], &xin[22]);
	    }
/* L90: */
	}
/* L100: */
    }
    return 0;
} /* patch_ */

/* Subroutine */ int fit_(npts, igrpt, pam, grid, xin)
integer *npts, *igrpt;
real *pam, *grid, *xin;
{
    /* Format strings */
    static char fmt_123[] = "(\002 NTRS,NPTS\002,2i6)";

    /* System generated locals */
    integer grid_dim1, grid_offset, i__1, i__2;

#ifdef FIO
    /* Builtin functions */
    integer s_wsfe(), e_wsfe();
    integer do_fio();
#endif

    /* Local variables */
    static integer ntrs;
    static real b[21];
    static integer i__, j, k;
    static real w[800];
    static integer index, i1, j1;
    extern /* Subroutine */ int linwt_();
    static real w1, w2, w3, xx, yy;
    extern /* Subroutine */ int curfit_();
    static integer ioldtr, itlist[800], ipt, ntr;
    extern /* Subroutine */ int cfitval_();

    /* Fortran I/O blocks */
    static cilist io___307 = { 0, 6, 0, fmt_123, 0 };


/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

/* GET THE MESONET STATIONS THAT CONTAIN THE NPTS GRID POINTS IN IGRPT(). 
*/
    /* Parameter adjustments */
    xin -= 22;
    grid_dim1 = triagd_1.nx;
    grid_offset = grid_dim1 + 1;
    grid -= grid_offset;
    --pam;
    igrpt -= 151;

    /* Function Body */
    ntrs = 0;
    i__1 = *npts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i1 = igrpt[i__ + 150];
	j1 = igrpt[i__ + 300];
/* GET THE THREE MESONET STATIONS THAT FORM TRIANGLE NTR. */
	for (j = 1; j <= 3; ++j) {
	    ntr = triagd_1.jw[i1 + j1 * 25 - 26];
/* SEE IF STATIONS THAT FORM TRIANGLE NTR ARE ALREADY IN LIST. */
	    if (ntrs > 0) {
		i__2 = ntrs;
		for (k = 1; k <= i__2; ++k) {
		    if (triagd_1.itrngl[ntr + j * 800 - 801] == itlist[k - 1])
			     {
			goto L90;
		    }
/* L80: */
		}
	    }
/* NOT ALREADY IN LIST. ADD IT. */
	    ++ntrs;
	    itlist[ntrs - 1] = triagd_1.itrngl[ntr + j * 800 - 801];
L90:
	    ;
	}
/* L100: */
    }

#ifdef FIO
    s_wsfe(&io___307);
    do_fio(&c__1, (char *)&ntrs, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*npts), (ftnlen)sizeof(integer));
    e_wsfe();
#endif
    if (ntrs < 6) {
/* USE A LINEAR INTERPOLATION FOR ALL GRID POINTS. */
	ioldtr = 0;
	i__1 = *npts;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i1 = igrpt[i__ + 150];
	    j1 = igrpt[i__ + 300];
	    ntr = triagd_1.jw[i1 + j1 * 25 - 26];
	    if (ioldtr != ntr) {
		linwt_(&ntr, &pam[1], &w1, &w2, &w3);
	    }
	    ioldtr = ntr;
	    xx = triagd_1.x0 + triagd_1.dx * (i1 - 1);
	    yy = triagd_1.y0 + triagd_1.dy * (j1 - 1);
	    grid[i1 + j1 * grid_dim1] = w1 + xx * w2 + yy * w3;
/* L400: */
	}
	return 0;
    }
    index = 6;
    if (ntrs >= 10) {
	index = 10;
    }
    if (ntrs >= 15) {
	index = 15;
    }
/* COMPUTE MATRIX INPUT TO CURVE-FIT ROUTINE. */
    i__1 = ntrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ipt = itlist[i__ - 1];
	xx = (triagd_1.x[ipt - 1] - triagd_1.xmin) * (float)9. / (
		triagd_1.xmax - triagd_1.xmin) + (float)1.;
	yy = (triagd_1.y[ipt - 1] - triagd_1.ymin) * (float)9. / (
		triagd_1.ymax - triagd_1.ymin) + (float)1.;
	xin[i__ * 21 + 1] = xx;
	xin[i__ * 21 + 2] = yy;
	if (index > 3) {
	    xin[i__ * 21 + 3] = xx * yy;
	    xin[i__ * 21 + 4] = xx * xx;
	    xin[i__ * 21 + 5] = yy * yy;
	    if (index > 6) {
		xin[i__ * 21 + 6] = xin[i__ * 21 + 4] * yy;
		xin[i__ * 21 + 7] = xin[i__ * 21 + 5] * xx;
		xin[i__ * 21 + 8] = xin[i__ * 21 + 4] * xx;
		xin[i__ * 21 + 9] = xin[i__ * 21 + 5] * yy;
		if (index > 10) {
		    xin[i__ * 21 + 10] = xin[i__ * 21 + 9] * xx;
		    xin[i__ * 21 + 11] = xin[i__ * 21 + 4] * xin[i__ * 21 + 5]
			    ;
		    xin[i__ * 21 + 12] = xin[i__ * 21 + 8] * yy;
		    xin[i__ * 21 + 13] = xin[i__ * 21 + 4] * xin[i__ * 21 + 4]
			    ;
		    xin[i__ * 21 + 14] = xin[i__ * 21 + 5] * xin[i__ * 21 + 5]
			    ;
		}
	    }
	}
	xin[index + i__ * 21] = pam[ipt];
/* L200: */
    }

/* FIT THE CURVE. */
    w[0] = (float)0.;
    curfit_(&index, &ntrs, &xin[22], w, &c_b229, &c_b229, b);

/* EVALUATE THE CURVE AT THE GRID POINTS. */
    i__1 = *npts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i1 = igrpt[i__ + 150];
	j1 = igrpt[i__ + 300];
	xx = triagd_1.x0 + triagd_1.dx * (i1 - 1);
	yy = triagd_1.y0 + triagd_1.dy * (j1 - 1);
	cfitval_(&xx, &yy, b, &index, &grid[i1 + j1 * grid_dim1]);
/* L300: */
    }
    return 0;
} /* fit_ */

/* Subroutine */ int linear_(ista, npts, igrpt, pam, grid)
integer *ista, *npts, *igrpt;
real *pam, *grid;
{
    /* System generated locals */
    integer grid_dim1, grid_offset, i__1;

    /* Local variables */
    static real wght[3];
    static integer i__, i1, j1;
    static real x1, x2, x3, y1, y2, y3, xx, yy;

/* CALCULATE THE LINEAR INTERPOLATION WEIGHTS FOR THE TRIANGLE */
/* CONTAINING MESONET STATION ISTA(1-3).  USE THE WEIGHTS TO COMPUTE */
/* INTERPOLATED VALUES AT THE NPTS GRID POINTS IGRPT(). */
/* 		Copyright (C) 1987,88,89,90,91 by UCAR */
/* 	University Corporation for Atmospheric Research */
/* 		   All rights reserved */

/* No part of this work covered by the copyrights herein may be reproduced
 */
/*or used in any form or by any means -- graphic, electronic, or mechanica
l,*/
/* including photocopying, recording, taping, or information storage and 
*/
/* retrieval systems -- without permission of the copyright owner. */

/*This software and any accompanying written materials are provided "as is
"*/
/*without warranty of any kind.  UCAR expressly disclaims all warranties o
f*/
/* any kind, either express or implied, including but not limited to the 
*/
/*implied warranties of merchantibility and fitness for a particular purpo
se.*/
/*UCAR does not indemnify any infringement of copyright, patent, or tradem
ark*/
/* through use or modification of this software.  UCAR does not provide */
/* maintenance or updates for its software. */

/* MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA. 
*/
/* MAXSTN = MAXIMUM NO. OF PAM STATIONS. */
/* LUIN = LOGICAL UNIT NO. FOR INPUT. */
/* LUOUT = LOGICAL UNIT NO. FOR OUTPUT. */

    /* Parameter adjustments */
    grid_dim1 = triagd_1.nx;
    grid_offset = grid_dim1 + 1;
    grid -= grid_offset;
    --pam;
    igrpt -= 151;
    --ista;

    /* Function Body */
    x1 = (triagd_1.x[ista[1] - 1] - triagd_1.xmin) * (float)9. / (
	    triagd_1.xmax - triagd_1.xmin) + (float)1.;
    x2 = (triagd_1.x[ista[2] - 1] - triagd_1.xmin) * (float)9. / (
	    triagd_1.xmax - triagd_1.xmin) + (float)1.;
    x3 = (triagd_1.x[ista[3] - 1] - triagd_1.xmin) * (float)9. / (
	    triagd_1.xmax - triagd_1.xmin) + (float)1.;
    y1 = (triagd_1.y[ista[1] - 1] - triagd_1.ymin) * (float)9. / (
	    triagd_1.ymax - triagd_1.ymin) + (float)1.;
    y2 = (triagd_1.y[ista[2] - 1] - triagd_1.ymin) * (float)9. / (
	    triagd_1.ymax - triagd_1.ymin) + (float)1.;
    y3 = (triagd_1.y[ista[3] - 1] - triagd_1.ymin) * (float)9. / (
	    triagd_1.ymax - triagd_1.ymin) + (float)1.;
    wght[2] = pam[ista[1]] * (x3 - x2) + pam[ista[2]] * (x1 - x3) + pam[ista[
	    3]] * (x2 - x1);
    wght[2] /= y1 * (x3 - x2) + y2 * (x1 - x3) + y3 * (x2 - x1);
    wght[1] = pam[ista[2]] - pam[ista[1]] + wght[2] * (y1 - y2);
    wght[1] /= x2 - x1;
    wght[0] = pam[ista[1]] - wght[2] * y1;
    wght[0] -= x1 * (pam[ista[2]] - pam[ista[1]] + wght[2] * (y1 - y2)) / (x2 
	    - x1);

/* EVALUATE THE INTERPOLATION AT THE GRID POINTS. */
    i__1 = *npts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i1 = igrpt[i__ + 150];
	j1 = igrpt[i__ + 300];
	xx = triagd_1.x0 + triagd_1.dx * (i1 - 1);
	yy = triagd_1.y0 + triagd_1.dy * (j1 - 1);
	xx = (xx - triagd_1.xmin) * (float)9. / (triagd_1.xmax - 
		triagd_1.xmin) + (float)1.;
	yy = (yy - triagd_1.ymin) * (float)9. / (triagd_1.ymax - 
		triagd_1.ymin) + (float)1.;
	grid[i1 + j1 * grid_dim1] = wght[0] + xx * wght[1] + yy * wght[2];
/* L10: */
    }
    return 0;
} /* linear_ */

