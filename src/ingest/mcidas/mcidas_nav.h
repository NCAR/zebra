/*
 * $Id: mcidas_nav.h,v 1.1 1997-12-08 18:03:55 burghart Exp $
 *
 * Definitions for the C interface to MCIDAS FORTRAN routines.
 */


int nvxini (int *navcod);
int nvxopt (int ifunc, float *xin, float *xout);
int nvxsae (float xlin, float xele, float xdum, 
	    float *xpar, float *ypar, float *zpar);
int nvxeas (float xpar, float ypar, float zpar, 
	    float *xlin, float *xele, float *xdum);

