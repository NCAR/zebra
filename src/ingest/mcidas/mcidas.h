/*
 * $Id: mcidas.h,v 1.1 1997-06-17 09:30:31 granger Exp $
 *
 * Definitions for the C interface to MCIDAS FORTRAN routines.
 */


int nvxini (int ifunc, int *navcod);
int nvxopt (int ifunc, float *xin, float *xout);
int nvxsae (float xlin, float xele, float xdum, 
	    float *xpar, float *ypar, float *zpar);
int nvxeas (float xpar, float ypar, float zpar, 
	    float *xlin, float *xele, float *xdum);

