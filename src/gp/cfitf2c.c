/* cfit.f -- translated by f2c (version 19960315). */

#include "f2c.h"

/* Common Block Declarations */

/* defined in rgrid.c */
extern struct {
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

/* defined in rgrid.c */
extern struct {
    char fstrng[7], ovfld[7], name__[10];
} trgchr_;

#define trgchr_1 trgchr_

/* Table of constant values */

static real c_b6 = (float).01;
static integer c__1 = 1;

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

/* Subroutine */ int compx_(xin, index)
real *xin;
integer *index;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, ix;
    static real xx, yy;

/* ROUTINE TO COMPUTE THE ARRAY XIN WHICH IS THE INPUT TO THE */
/* CURVE-FIT ROUTINE CURFIT. */
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

/* INDEX OF THE LAST TERM OF THE POLYNOMIAL TO BE FIT PLUS 1. */
    /* Parameter adjustments */
    xin -= 22;

    /* Function Body */
    *index = 0;
    if (triagd_1.nstns > 1) {
	*index = 3;
    }
    if (triagd_1.nstns > 4) {
	*index = 6;
    }
    if (triagd_1.nstns > 8) {
	*index = 10;
    }
    if (triagd_1.nstns > 13) {
	*index = 15;
    }
    if (triagd_1.nstns > 19) {
	*index = 21;
    }
    ix = 0;
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! triagd_1.deleted[i__ - 1]) {
	    ++ix;
	    xx = (triagd_1.x[i__ - 1] - triagd_1.xmin) * (float)9. / (
		    triagd_1.xmax - triagd_1.xmin) + (float)1.;
	    yy = (triagd_1.y[i__ - 1] - triagd_1.ymin) * (float)9. / (
		    triagd_1.ymax - triagd_1.ymin) + (float)1.;
	    xin[ix * 21 + 1] = xx;
	    xin[ix * 21 + 2] = yy;
	    if (*index > 3) {
		xin[ix * 21 + 3] = xx * yy;
		xin[ix * 21 + 4] = xx * xx;
		xin[ix * 21 + 5] = yy * yy;
		if (*index > 6) {
		    xin[ix * 21 + 6] = xin[ix * 21 + 4] * yy;
		    xin[ix * 21 + 7] = xin[ix * 21 + 5] * xx;
		    xin[ix * 21 + 8] = xin[ix * 21 + 4] * xx;
		    xin[ix * 21 + 9] = xin[ix * 21 + 5] * yy;
		    if (*index > 10) {
			xin[ix * 21 + 10] = xin[ix * 21 + 9] * xx;
			xin[ix * 21 + 11] = xin[ix * 21 + 4] * xin[ix * 21 + 
				5];
			xin[ix * 21 + 12] = xin[ix * 21 + 8] * yy;
			xin[ix * 21 + 13] = xin[ix * 21 + 4] * xin[ix * 21 + 
				4];
			xin[ix * 21 + 14] = xin[ix * 21 + 5] * xin[ix * 21 + 
				5];
			if (*index > 15) {
			    xin[ix * 21 + 15] = xin[ix * 21 + 14] * xx;
			    xin[ix * 21 + 16] = xin[ix * 21 + 4] * xin[ix * 
				    21 + 9];
			    xin[ix * 21 + 17] = xin[ix * 21 + 8] * xin[ix * 
				    21 + 5];
			    xin[ix * 21 + 18] = xin[ix * 21 + 13] * yy;
			    xin[ix * 21 + 19] = xin[ix * 21 + 13] * xx;
			    xin[ix * 21 + 20] = xin[ix * 21 + 14] * yy;
			}
		    }
		}
	    }
	}
/* L100: */
    }
    return 0;
} /* compx_ */

/* Subroutine */ int polyfit_(pam, xin, index, b)
real *pam, *xin;
integer *index;
real *b;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    static real w[800];
    static integer ix;
    extern /* Subroutine */ int curfit_();

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
    --b;
    xin -= 22;
    --pam;

    /* Function Body */
    for (i__ = 1; i__ <= 21; ++i__) {
/* L10: */
	b[i__] = (float)0.;
    }
    ix = 0;
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! triagd_1.deleted[i__ - 1]) {
	    ++ix;
	    xin[*index + ix * 21] = pam[i__];
	}
/* L100: */
    }

    w[0] = (float)0.;
    curfit_(index, &triagd_1.nstns, &xin[22], w, &c_b6, &c_b6, &b[1]);
    return 0;
} /* polyfit_ */

/* Subroutine */ int eval_(grid, b, index, mode)
real *grid, *b;
integer *index, *mode;
{
    /* System generated locals */
    integer grid_dim1, grid_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    static real xx, yy;
    extern /* Subroutine */ int cfitval_();

/* EVALUATE THE POLYNOMIAL AT THE POINTS OF A RECTANGULAR GRID. */
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
    --b;
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
		cfitval_(&xx, &yy, &b[1], index, &grid[i__ + j * grid_dim1]);
	    } else {
		if (*mode != 0) {
		    grid[i__ + j * grid_dim1] = triagd_1.flag__;
		}
	    }
	    xx += triagd_1.dx;
/* L100: */
	}
    }
    return 0;
} /* eval_ */

/* Subroutine */ int cfitval_(xx, yy, b, index, value)
real *xx, *yy, *b;
integer *index;
real *value;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer k;
    static real x1, y1, bx[20];

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
    --b;

    /* Function Body */
    x1 = (*xx - triagd_1.xmin) * (float)9. / (triagd_1.xmax - triagd_1.xmin) 
	    + (float)1.;
    y1 = (*yy - triagd_1.ymin) * (float)9. / (triagd_1.ymax - triagd_1.ymin) 
	    + (float)1.;
    bx[0] = x1;
    bx[1] = y1;
    if (*index > 3) {
	bx[2] = x1 * y1;
	bx[3] = x1 * x1;
	bx[4] = y1 * y1;
	if (*index > 6) {
	    bx[5] = bx[3] * y1;
	    bx[6] = bx[4] * x1;
	    bx[7] = bx[3] * x1;
	    bx[8] = bx[4] * y1;
	    if (*index > 10) {
		bx[9] = bx[8] * x1;
		bx[10] = bx[3] * bx[4];
		bx[11] = bx[7] * y1;
		bx[12] = bx[3] * bx[3];
		bx[13] = bx[4] * bx[4];
		if (*index > 15) {
		    bx[14] = bx[13] * x1;
		    bx[15] = bx[3] * bx[8];
		    bx[16] = bx[4] * bx[7];
		    bx[17] = bx[12] * y1;
		    bx[18] = bx[12] * x1;
		    bx[19] = bx[13] * y1;
		}
	    }
	}
    }
    *value = b[*index];
    i__1 = *index - 1;
    for (k = 1; k <= i__1; ++k) {
	*value += b[k] * bx[k - 1];
/* L90: */
    }
    return 0;
} /* cfitval_ */

/* Subroutine */ int curfit_(n, m, xin, w, f1, f2, bb)
integer *n, *m;
real *xin, *w, *f1, *f2, *bb;
{
    /* Format strings */
    static char fmt_123[] = "(\002 ERRMEAN,ERRMAX\002,2f10.4)";
    static char fmt_4008[] = "(\0020\002,\002MULTIPLE CORRELATION COEFFICIEN\
TS\002,8x,\002STANDARD ERROR OF ESTIMATE\002/10x,f10.5,29x,f10.5)";

    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1;
    doublereal d__1;

    /* Builtin functions */
    double sqrt();
#ifdef FIO
    integer s_wsfe(), do_fio(), e_wsfe();
#endif

    /* Local variables */
    static integer idof, jdof;
    static real ssab;
    static integer nmin, iter, nmax;
    static real errm;
    static doublereal vmin, vmax, a[441]	/* was [21][21] */, b[21];
    static integer i__, j, k, l;
    static doublereal r__[441]	/* was [21][21] */, s[441]	/* was [21][
	    21] */;
    static real rmsab;
    static doublereal b0;
    static real ssdue, sterr, sstot;
    static doublereal sb[21];
    static real pe;
    static doublereal xb[21];
    static real devabs, yp, tw;
    static doublereal sy;
    static real errmax, rmsdue;
    static integer iprint;
    static real sb0;
    static integer ip1, nm1;
    static real rsquar;
    static doublereal f1x, f2x;
    static real dev, phi;
    static doublereal sig[21], var[21];

    /* Fortran I/O blocks */
    static cilist io___51 = { 0, 6, 0, fmt_123, 0 };
    static cilist io___59 = { 0, 6, 0, fmt_4008, 0 };


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
    --bb;
    --w;
    xin -= 22;

    /* Function Body */
    if (w[1] <= (float)1e-6) {
	i__1 = *m;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L90: */
	    w[i__] = (float)1.;
	}
    }
    tw = (float)0.;
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L97: */
	tw += w[i__];
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	var[i__ - 1] = (float)0.;
	i__2 = *m;
	for (l = 1; l <= i__2; ++l) {
/* L99: */
	    var[i__ - 1] += w[l] * xin[i__ + l * 21];
	}
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    a[i__ + j * 21 - 22] = (float)0.;
	    i__3 = *m;
	    for (k = 1; k <= i__3; ++k) {
/* L100: */
		a[i__ + j * 21 - 22] += w[k] * xin[i__ + k * 21] * xin[j + k *
			 21];
	    }
	}
    }
    i__3 = *n;
    for (i__ = 1; i__ <= i__3; ++i__) {
	xb[i__ - 1] = var[i__ - 1] / (doublereal) tw;
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
/* L110: */
	    s[i__ + j * 21 - 22] = ((doublereal) tw * a[i__ + j * 21 - 22] - 
		    var[i__ - 1] * var[j - 1]) / (doublereal) tw;
	}
	r__[i__ + i__ * 21 - 22] = (float)1.;
/* L120: */
	sig[i__ - 1] = sqrt(s[i__ + i__ * 21 - 22]);
    }
    nm1 = *n - 1;
    i__3 = nm1;
    for (i__ = 1; i__ <= i__3; ++i__) {
	ip1 = i__ + 1;
	i__2 = *n;
	for (j = ip1; j <= i__2; ++j) {
	    if ((d__1 = sig[i__ - 1] * sig[j - 1], abs(d__1)) >= (float)1e-4) 
		    {
		goto L400;
	    }
	    r__[i__ + j * 21 - 22] = (float)0.;
	    goto L130;
L400:
	    r__[i__ + j * 21 - 22] = s[i__ + j * 21 - 22] / (sig[i__ - 1] * 
		    sig[j - 1]);
L130:
	    r__[j + i__ * 21 - 22] = r__[i__ + j * 21 - 22];
	}
    }
    i__2 = *n;
    for (j = 1; j <= i__2; ++j) {
	i__3 = *n;
	for (i__ = 1; i__ <= i__3; ++i__) {
/* L150: */
	    a[i__ + j * 21 - 22] = r__[i__ + j * 21 - 22];
	}
    }
    phi = tw - (float)1.;
    iter = 0;
L7:
    i__3 = nm1;
    for (i__ = 1; i__ <= i__3; ++i__) {
	sb[i__ - 1] = (float)0.;
/* L140: */
	b[i__ - 1] = (float)0.;
    }
    vmin = 1e35;
    vmax = (float)0.;
    nmin = 0;
    nmax = 0;
    sy = sig[*n - 1] * sqrt(a[*n + *n * 21 - 22] / phi);
    i__ = 1;
L4:
    if (a[i__ + i__ * 21 - 22] <= 1e-6) {
	goto L3;
    }
    var[i__ - 1] = a[i__ + *n * 21 - 22] * a[*n + i__ * 21 - 22] / a[i__ + 
	    i__ * 21 - 22];
    if ((d__1 = var[i__ - 1]) < 0.) {
	goto L1;
    } else if (d__1 == 0) {
	goto L3;
    } else {
	goto L160;
    }
L160:
    if (var[i__ - 1] <= vmax) {
	goto L3;
    }
/* L2: */
    vmax = var[i__ - 1];
    nmax = i__;
    goto L3;
L1:
    b[i__ - 1] = a[i__ + *n * 21 - 22] * sig[*n - 1] / sig[i__ - 1];
    sb[i__ - 1] = sy * sqrt(a[i__ + i__ * 21 - 22]) / sig[i__ - 1];
    if ((d__1 = var[i__ - 1], abs(d__1)) >= abs(vmin)) {
	goto L3;
    }
    vmin = var[i__ - 1];
    nmin = i__;
L3:
    if (i__ >= nm1) {
	goto L170;
    }
    ++i__;
    goto L4;
L170:
    b0 = xb[*n - 1];
    i__3 = nm1;
    for (j = 1; j <= i__3; ++j) {
/* L180: */
	b0 -= b[j - 1] * xb[j - 1];
    }
    f2x = abs(vmin) * phi / a[*n + *n * 21 - 22];
    if (f2x >= *f2) {
	goto L6;
    }
    k = nmin;
    phi += (float)1.;
    goto L5;
L6:
    f1x = vmax * (phi - (float)1.) / (a[*n + *n * 21 - 22] - vmax);
    if (f1x <= *f1) {
	goto L22;
    }
/* L8: */
    k = nmax;
    phi += (float)-1.;
L5:
    i__3 = *n;
    for (j = 1; j <= i__3; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L4237: */
	    r__[i__ + j * 21 - 22] = a[i__ + j * 21 - 22];
	}
    }
    i__2 = *n;
    for (j = 1; j <= i__2; ++j) {
	i__3 = *n;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    if (i__ == k) {
		goto L11;
	    }
	    if (j == k) {
		goto L20;
	    }
/* L10: */
	    a[i__ + j * 21 - 22] = r__[i__ + j * 21 - 22] - r__[i__ + k * 21 
		    - 22] * r__[k + j * 21 - 22] / r__[k + k * 21 - 22];
	    goto L190;
L11:
	    if (j == k) {
		goto L30;
	    }
/* L40: */
	    a[k + j * 21 - 22] = r__[k + j * 21 - 22] / r__[k + k * 21 - 22];
	    goto L190;
L30:
	    a[k + k * 21 - 22] = (float)1. / r__[k + k * 21 - 22];
	    goto L190;
L20:
	    a[i__ + k * 21 - 22] = r__[i__ + k * 21 - 22] * (float)-1. / r__[
		    k + k * 21 - 22];
L190:
	    ;
	}
    }
    ++iter;
    if (iter < *n << 1) {
	goto L7;
    }
L22:
    b[*n - 1] = b0;
    i__3 = *n - 1;
    for (i__ = 1; i__ <= i__3; ++i__) {
/* L23: */
	bb[i__] = b[i__ - 1];
    }
    bb[*n] = b0;
    iprint = 0;
    if (iprint == 0) {
	return 0;
    }
/* Computing 2nd power */
    d__1 = sy;
    sb0 = sqrt(d__1 * d__1 / phi);
/*       WRITE(LUOUT,2000) SY,F1,F2 */
/*       WRITE(LUOUT,5000) B0,SB0,(I,B(I),I,SB(I),I=1,NM1) */
/*       WRITE(LUOUT,5002) */
    errmax = (float)0.;
    errm = (float)0.;
    ssab = (float)0.;
    ssdue = (float)0.;
    i__3 = *m;
    for (i__ = 1; i__ <= i__3; ++i__) {
	yp = b0;
	i__2 = nm1;
	for (j = 1; j <= i__2; ++j) {
/* L200: */
	    yp += b[j - 1] * xin[j + i__ * 21];
	}
	dev = yp - xin[*n + i__ * 21];
	devabs = dabs(dev);
	errmax = dmax(errmax,devabs);
	errm += devabs;
	ssab += dev * dev;
/* Computing 2nd power */
	d__1 = yp - xb[*n - 1];
	ssdue += d__1 * d__1;
	if ((r__1 = xin[*n + i__ * 21], dabs(r__1)) >= (float)1e-4) {
	    goto L3000;
	}
	pe = (float)1e6;
	goto L210;
L3000:
	pe = dev * (float)100. / xin[*n + i__ * 21];
/*       WRITE(LUOUT,5001) XIN(N,I),YP,DEV,PE */
L210:
	;
    }
    errm /= (real) (*m);
#ifdef FIO
    s_wsfe(&io___51);
    do_fio(&c__1, (char *)&errm, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&errmax, (ftnlen)sizeof(real));
    e_wsfe();
#endif
    sstot = ssab + ssdue;
    if (sstot >= (float)1e-4) {
	goto L900;
    }
    rsquar = (float)0.;
    goto L901;
L900:
    rsquar = ssdue / sstot;
L901:
    idof = 1;
    i__3 = nm1;
    for (i__ = 1; i__ <= i__3; ++i__) {
	if ((d__1 = b[i__ - 1], abs(d__1)) <= (float)1e-25) {
	    goto L300;
	}
	++idof;
L300:
	;
    }
    jdof = *m - idof;
    rmsdue = ssdue / idof;
    rmsab = ssab / jdof;
    sterr = sqrt(rmsab);
/*       WRITE(LUOUT,4001) */
/* L4001: */
/*       WRITE(LUOUT,4002) IDOF,SSDUE,RMSDUE */
/* L4002: */
/*       WRITE(LUOUT,4003) JDOF,SSAB,RMSAB */
/* L4003: */
/*       WRITE(LUOUT,4006) M,SSTOT */
/* L4006: */
#ifdef FIO
    s_wsfe(&io___59);
    do_fio(&c__1, (char *)&rsquar, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&sterr, (ftnlen)sizeof(real));
    e_wsfe();
#endif
/* L2000: */
/* L5000: */
/* L5001: */
/* L5002: */
    return 0;
} /* curfit_ */

/* Subroutine */ int ierror_(array1, array2, index)
real *array1, *array2;
integer *index;
{
    /* Format strings */
    static char fmt_110[] = "(\002 AVERR,ERRMAX,AVEFLD,IX\002,3f10.4,i6)";

    /* System generated locals */
    integer array1_dim1, array1_offset, i__1;
    real r__1;

#ifdef FIO
    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();
#endif

    /* Local variables */
    static integer i__;
    static real averr, f0, f1, f2;
    static integer i1, j1;
    static real avefld;
    static integer ix;
    static real errabs, xx, yy, errmax;

    /* Fortran I/O blocks */
    static cilist io___73 = { 0, 6, 0, fmt_110, 0 };


/* COMPUTE APPROXIMATION ERRORS AT PAM STATIONS */
/* INDEX = 1 IF EXCLUDE ALL PAM STATIONS THAT ARE NEAR GRID POINTS */
/* OUTSIDE THE AREA COVERED BY PAM STATIONS */
/* INDEX = 0 IF INCLUDE ALL PAM STATIONS */
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
    --array2;
    array1_dim1 = triagd_1.nx;
    array1_offset = array1_dim1 + 1;
    array1 -= array1_offset;

    /* Function Body */
    averr = (float)0.;
    errmax = (float)0.;
    avefld = (float)0.;
    ix = 0;
    i__1 = triagd_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (! triagd_1.deleted[i__ - 1]) {
	    if (triagd_1.x[i__ - 1] >= triagd_1.xmin + triagd_1.dx && 
		    triagd_1.x[i__ - 1] <= triagd_1.xmax && triagd_1.y[i__ - 
		    1] >= triagd_1.ymin + triagd_1.dy && triagd_1.y[i__ - 1] 
		    <= triagd_1.ymax) {
		i1 = (integer) ((triagd_1.x[i__ - 1] - triagd_1.xmin) / 
			triagd_1.dx);
		j1 = (integer) ((triagd_1.y[i__ - 1] - triagd_1.ymin) / 
			triagd_1.dy);
		if (i1 <= 0 || j1 <= 0 || i1 > triagd_1.nx || j1 > 
			triagd_1.ny) {
		    goto L100;
		}
		if ((triagd_1.jw[i1 + j1 * 25 - 26] <= 0 || triagd_1.jw[i1 + 
			1 + j1 * 25 - 26] <= 0 || triagd_1.jw[i1 + (j1 + 1) * 
			25 - 26] <= 0 || triagd_1.jw[i1 + 1 + (j1 + 1) * 25 - 
			26] <= 0) && *index == 0) {
		    goto L100;
		}
		xx = triagd_1.x[i__ - 1] - (real) (i1 - 1) * triagd_1.dx - 
			triagd_1.x0;
		yy = triagd_1.y[i__ - 1] - (real) (j1 - 1) * triagd_1.dy - 
			triagd_1.y0;
		f0 = array1[i1 + j1 * array1_dim1] + xx / triagd_1.dx * (
			array1[i1 + (j1 + 1) * array1_dim1] - array1[i1 + j1 *
			 array1_dim1]);
		f1 = array1[i1 + 1 + j1 * array1_dim1] + xx / triagd_1.dx * (
			array1[i1 + 1 + (j1 + 1) * array1_dim1] - array1[i1 + 
			1 + j1 * array1_dim1]);
		f2 = f0 + yy / triagd_1.dy * (f1 - f0);
		errabs = (r__1 = f2 - array2[i__], dabs(r__1));
		errmax = dmax(errmax,errabs);
		averr += errabs;
		avefld += (r__1 = array2[i__], dabs(r__1));
		++ix;
	    }
	}
L100:
	;
    }
    averr /= (real) ix;
    avefld /= (real) ix;
#ifdef FIO
    s_wsfe(&io___73);
    do_fio(&c__1, (char *)&averr, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&errmax, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&avefld, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&ix, (ftnlen)sizeof(integer));
    e_wsfe();
#endif
    return 0;
} /* ierror_ */

/* Subroutine */ int cfitd_(xin, index, mode, uu, vv, zz, scr1, scr2)
real *xin;
integer *index, *mode;
real *uu, *vv, *zz, *scr1, *scr2;
{
    /* Initialized data */

    static real pi = (float)3.1415927;

    /* System generated locals */
    integer scr1_dim1, scr1_offset, scr2_dim1, scr2_offset, uu_dim1, 
	    uu_offset, vv_dim1, vv_offset, zz_dim1, zz_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(), atan();

    /* Local variables */
    extern /* Subroutine */ int eval_();
    static integer i__, j;
    static real shdef, stdef, b1[21], b2[21], b3[21];
    extern /* Subroutine */ int evaldx_(), evaldy_(), polyfit_();

/* ROUTINE TO COMPUTE FIELDS THAT REQUIRE DIFFERENTIATION USING */
/* THE POLYNOMIAL CURVE-FIT. */
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
    xin -= 22;

    /* Function Body */

    if (triagd_1.ipar == 56 || triagd_1.ipar == 77) {
/* DIVERGENCE OR STRETCHING DEFORMATION. */
	polyfit_(triagd_1.u, &xin[22], index, b1);
	evaldx_(&scr1[scr1_offset], b1, index, mode);
	polyfit_(triagd_1.v, &xin[22], index, b2);
	evaldy_(&scr2[scr2_offset], b2, index, mode);
/* DIVERGENCE. */
	if (triagd_1.ipar == 56) {
	    i__1 = triagd_1.ny;
	    for (j = 1; j <= i__1; ++j) {
		i__2 = triagd_1.nx;
		for (i__ = 1; i__ <= i__2; ++i__) {
/* L10: */
		    zz[i__ + j * zz_dim1] = scr1[i__ + j * scr1_dim1] + scr2[
			    i__ + j * scr2_dim1];
		}
	    }
	}
/* STRETCHING DEFORMATION. */
	if (triagd_1.ipar == 77) {
	    i__2 = triagd_1.ny;
	    for (j = 1; j <= i__2; ++j) {
		i__1 = triagd_1.nx;
		for (i__ = 1; i__ <= i__1; ++i__) {
/* L50: */
		    zz[i__ + j * zz_dim1] = scr1[i__ + j * scr1_dim1] - scr2[
			    i__ + j * scr2_dim1];
		}
	    }
	}
    }

    if (triagd_1.ipar == 57 || triagd_1.ipar == 78) {
/* VORTICITY OR SHEARING DEFORMATION. */
	polyfit_(triagd_1.u, &xin[22], index, b1);
	evaldy_(&scr1[scr1_offset], b1, index, mode);
	polyfit_(triagd_1.v, &xin[22], index, b2);
	evaldx_(&scr2[scr2_offset], b2, index, mode);
/* VORTICITY. */
	if (triagd_1.ipar == 57) {
	    i__1 = triagd_1.ny;
	    for (j = 1; j <= i__1; ++j) {
		i__2 = triagd_1.nx;
		for (i__ = 1; i__ <= i__2; ++i__) {
/* L20: */
		    zz[i__ + j * zz_dim1] = scr2[i__ + j * scr2_dim1] - scr1[
			    i__ + j * scr1_dim1];
		}
	    }
	}
/* SHEARING DEFORMATION. */
	if (triagd_1.ipar == 78) {
	    i__2 = triagd_1.ny;
	    for (j = 1; j <= i__2; ++j) {
		i__1 = triagd_1.nx;
		for (i__ = 1; i__ <= i__1; ++i__) {
/* L60: */
		    zz[i__ + j * zz_dim1] = scr2[i__ + j * scr2_dim1] + scr1[
			    i__ + j * scr1_dim1];
		}
	    }
	}
    }

    if (triagd_1.ipar == 58) {
/* ENERGY FLUX CONVERGENCE. */
	polyfit_(triagd_1.u, &xin[22], index, b1);
	evaldx_(&uu[uu_offset], b1, index, mode);
	polyfit_(triagd_1.v, &xin[22], index, b2);
	evaldy_(&vv[vv_offset], b2, index, mode);
	polyfit_(triagd_1.z__, &xin[22], index, b3);
	eval_(&zz[zz_offset], b3, index, mode);
	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/* L30: */
		zz[i__ + j * zz_dim1] *= uu[i__ + j * uu_dim1] + vv[i__ + j * 
			vv_dim1];
	    }
	}
	evaldx_(&scr1[scr1_offset], b3, index, mode);
	evaldy_(&scr2[scr2_offset], b3, index, mode);
	eval_(&uu[uu_offset], b1, index, mode);
	eval_(&vv[vv_offset], b2, index, mode);
	i__2 = triagd_1.ny;
	for (j = 1; j <= i__2; ++j) {
	    i__1 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* L40: */
		zz[i__ + j * zz_dim1] = zz[i__ + j * zz_dim1] + uu[i__ + j * 
			uu_dim1] * scr1[i__ + j * scr1_dim1] + vv[i__ + j * 
			vv_dim1] * scr2[i__ + j * scr2_dim1];
	    }
	}
    }

    if (triagd_1.ipar == 88 || triagd_1.ipar == 999) {
/* TOTAL DEFORMATION */
	polyfit_(triagd_1.u, &xin[22], index, b1);
	polyfit_(triagd_1.v, &xin[22], index, b2);
/* DU/DX */
	evaldx_(&uu[uu_offset], b1, index, mode);
/* DV/DX */
	evaldx_(&vv[vv_offset], b2, index, mode);
/* DU/DY */
	evaldy_(&scr1[scr1_offset], b1, index, mode);
/* DV/DY */
	evaldy_(&scr2[scr2_offset], b2, index, mode);
	i__1 = triagd_1.ny;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		shdef = uu[i__ + j * uu_dim1] - scr2[i__ + j * scr2_dim1];
		stdef = scr1[i__ + j * scr1_dim1] + vv[i__ + j * vv_dim1];
		if (triagd_1.ipar == 88) {
/* TOTAL DEFORMATION */
		    zz[i__ + j * zz_dim1] = sqrt(shdef * shdef + stdef * 
			    stdef);
		} else {
/* AXIS OF DILATATION IN RADIANS. */
		    if (shdef > (float)0.) {
			if (shdef < (float)1e-10) {
			    zz[i__ + j * zz_dim1] = pi / (float)4.;
			} else {
			    zz[i__ + j * zz_dim1] = pi / (float)4. - atan(
				    stdef / shdef);
			}
		    } else {
			if (shdef > (float)-1e-10) {
			    zz[i__ + j * zz_dim1] = pi * (float)3. / (float)
				    4.;
			} else {
			    zz[i__ + j * zz_dim1] = pi * (float)3. / (float)
				    4. - atan(stdef / shdef);
			}
		    }
/* AXIS OF DILATATION IN DEGREES. */
		    zz[i__ + j * zz_dim1] = zz[i__ + j * zz_dim1] * (float)
			    180. / pi;
		}
/* L70: */
	    }
	}
    }

/* Vorticity stretching (added 4/17/89 cb) */

    if (triagd_1.ipar == 143) {

/* 	Get the du/dx terms into array scr1 */

	polyfit_(triagd_1.u, &xin[22], index, b1);
	evaldx_(&scr1[scr1_offset], b1, index, mode);

/* 	Get the dv/dy terms into array scr2 */

	polyfit_(triagd_1.v, &xin[22], index, b2);
	evaldy_(&scr2[scr2_offset], b2, index, mode);

/* 	zz = du/dx + dv/dy (divergence) */

	i__2 = triagd_1.ny;
	for (j = 1; j <= i__2; ++j) {
	    i__1 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		zz[i__ + j * zz_dim1] = scr1[i__ + j * scr1_dim1] + scr2[i__ 
			+ j * scr2_dim1];
/* L90: */
	    }
/* L80: */
	}

/* 	Get the du/dy terms into array scr2 and the dv/dx terms */
/* 	into array scr1 */

	evaldy_(&scr2[scr2_offset], b1, index, mode);
	evaldx_(&scr1[scr1_offset], b2, index, mode);

/* 	zz = -divergence * (dv/dx - du/dy) */
/* 	   = -divergence * vorticity		(vorticity stretching) */

	i__2 = triagd_1.ny;
	for (j = 1; j <= i__2; ++j) {
	    i__1 = triagd_1.nx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		zz[i__ + j * zz_dim1] = -zz[i__ + j * zz_dim1] * (scr1[i__ + 
			j * scr1_dim1] - scr2[i__ + j * scr2_dim1]);
/* L110: */
	    }
/* L100: */
	}
    }

/* COMPUTE U_WIND AND V_WIND AT GRID POINTS IF WIND VECTORS ARE OVERLAYED.
 */
    if (triagd_1.iparov == 60 && triagd_1.ipar != 58) {
	eval_(&uu[uu_offset], b1, index, mode);
	eval_(&vv[vv_offset], b2, index, mode);
    }

    return 0;
} /* cfitd_ */

/* Subroutine */ int cfitdx_(xx, yy, b, index, value)
real *xx, *yy, *b;
integer *index;
real *value;
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static integer i__;
    static real x1, y1, bx[20];

/* ROUTINE TO CALCULATE DERIVATIVE W.R.T. X OF THE POLYNOMIAL WHOSE */
/* COEFFICIENTS ARE B(*) AT (XX,YY). */
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
    --b;

    /* Function Body */
    x1 = (*xx - triagd_1.xmin) * (float)9. / (triagd_1.xmax - triagd_1.xmin) 
	    + (float)1.;
    y1 = (*yy - triagd_1.ymin) * (float)9. / (triagd_1.ymax - triagd_1.ymin) 
	    + (float)1.;
    bx[0] = (float)1.;
    bx[1] = (float)0.;
    if (*index > 3) {
	bx[2] = y1;
	bx[3] = x1 * (float)2.;
	bx[4] = (float)0.;
	if (*index > 6) {
	    bx[5] = x1 * (float)2. * y1;
	    bx[6] = y1 * y1;
	    bx[7] = x1 * (float)3. * x1;
	    bx[8] = (float)0.;
	    if (*index > 10) {
		bx[9] = bx[6] * y1;
		bx[10] = x1 * (float)2. * bx[6];
		bx[11] = bx[7] * y1;
		bx[12] = x1 * (float)4. * x1 * x1;
		bx[13] = (float)0.;
		if (*index > 15) {
		    bx[14] = bx[6] * bx[6];
		    bx[15] = x1 * (float)2. * bx[9];
		    bx[16] = bx[7] * bx[6];
		    bx[17] = bx[12] * y1;
/* Computing 4th power */
		    r__1 = x1, r__1 *= r__1;
		    bx[18] = r__1 * r__1 * (float)5.;
		    bx[19] = (float)0.;
		}
	    }
	}
    }
    *value = (float)0.;
    i__1 = *index - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L10: */
	*value += b[i__] * bx[i__ - 1];
    }
    *value = *value * (float)9. / (triagd_1.xmax - triagd_1.xmin);
    return 0;
} /* cfitdx_ */

/* Subroutine */ int cfitdy_(xx, yy, b, index, value)
real *xx, *yy, *b;
integer *index;
real *value;
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static integer i__;
    static real x1, y1, bx[20];

/* ROUTINE TO CALCULATE DERIVATIVE W.R.T. Y AT (XX,YY) OF POLYNOMIAL */
/* WHOSE COEFFICIENTS ARE B(*). */
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
    --b;

    /* Function Body */
    x1 = (*xx - triagd_1.xmin) * (float)9. / (triagd_1.xmax - triagd_1.xmin) 
	    + (float)1.;
    y1 = (*yy - triagd_1.ymin) * (float)9. / (triagd_1.ymax - triagd_1.ymin) 
	    + (float)1.;
    bx[0] = (float)0.;
    bx[1] = (float)1.;
    if (*index > 3) {
	bx[2] = x1;
	bx[3] = (float)0.;
	bx[4] = y1 * (float)2.;
	if (*index > 6) {
	    bx[5] = x1 * x1;
	    bx[6] = y1 * (float)2. * x1;
	    bx[7] = (float)0.;
	    bx[8] = y1 * (float)3. * y1;
	    if (*index > 10) {
		bx[9] = bx[8] * x1;
		bx[10] = bx[5] * bx[4];
		bx[11] = bx[5] * x1;
		bx[12] = (float)0.;
		bx[13] = y1 * (float)4. * y1 * y1;
		if (*index > 15) {
		    bx[14] = bx[13] * x1;
		    bx[15] = bx[8] * bx[5];
		    bx[16] = bx[4] * bx[11];
		    bx[17] = bx[5] * bx[5];
		    bx[18] = (float)0.;
/* Computing 4th power */
		    r__1 = y1, r__1 *= r__1;
		    bx[19] = r__1 * r__1 * (float)5.;
		}
	    }
	}
    }
    *value = (float)0.;
    i__1 = *index - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L10: */
	*value += b[i__] * bx[i__ - 1];
    }
    *value = *value * (float)9. / (triagd_1.ymax - triagd_1.ymin);
    return 0;
} /* cfitdy_ */

/* Subroutine */ int evaldx_(grid, b, index, mode)
real *grid, *b;
integer *index, *mode;
{
    /* System generated locals */
    integer grid_dim1, grid_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    static real xx;
    extern /* Subroutine */ int cfitdx_();
    static real yy;

/* COMPUTE DERIVATIVE W.R.T. X AT ALL GRID POINTS. */
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
    --b;
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
		cfitdx_(&xx, &yy, &b[1], index, &grid[i__ + j * grid_dim1]);
	    } else {
		if (*mode != 0) {
		    grid[i__ + j * grid_dim1] = triagd_1.flag__;
		}
	    }
	    xx += triagd_1.dx;
/* L100: */
	}
    }
    return 0;
} /* evaldx_ */

/* Subroutine */ int evaldy_(grid, b, index, mode)
real *grid, *b;
integer *index, *mode;
{
    /* System generated locals */
    integer grid_dim1, grid_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    static real xx, yy;
    extern /* Subroutine */ int cfitdy_();

/* COMPUTE DERIVATIVE W.R.T. Y AT ALL GRID POINTS. */
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
    --b;
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
		cfitdy_(&xx, &yy, &b[1], index, &grid[i__ + j * grid_dim1]);
	    } else {
		if (*mode != 0) {
		    grid[i__ + j * grid_dim1] = triagd_1.flag__;
		}
	    }
	    xx += triagd_1.dx;
/* L100: */
	}
    }
    return 0;
} /* evaldy_ */

