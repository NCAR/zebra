/* rafthetae -- Kerry Emanuel's equivalent potential temperature calc */
/* Make supersaturation possible -- 9 Dec 92. */
/* Also compute saturation mixing ratio -- 15 Dec 92. */
#include <math.h>

#define LV0 2.501e6
#define P0 1000.
#define RD 287.04
#define RV 461.5
#define CPD 1005.
#define CL 4190.
#define CTOK 273.15
#define EPS .622

static float escalc(), the();

/*
 * Convenient interface to the(), taking p (mb), t (C), dewpoint (C), and 
 * liquid water content (g/m**3) and returning theta_e (K)
 */
float raf_thetae (p, t, dp, l)
float	p, t, dp, l;
{
	float	es, w, lw;

	t += CTOK;
	dp += CTOK;

	es = escalc (dp);		/* vapor pressure */
	w = EPS * es / (p - es);	/* vapor mixing ratio */

	lw = l * (1.0e-5 * RD * t / p);	/* liquid water mixing ratio */

	return (the (t, p, w, lw));
}



/* do the actual computation given temp (K), pres (mb), and mixrat (g/g) */
static float the(t,p,r,l)
float t,p,r,l;
{
  float es,rh,x1,x2,a1,a2,lv,rs;

/* don't let r exceed saturation value */
  es = escalc(t);
  rs = EPS*es/(p - es);
  if (r > rs) r = rs;
  rh = r/rs;

/* get actual L */
  lv = LV0 - 2320.*(t - CTOK);
  if (lv > LV0) lv = LV0;

/* do the rest */
  x1 = RD/(CPD + (r + l)*CL);
  x2 = -r*RV/(CPD + (r + l)*CL);
  a1 = P0*(1. + 1.608*r)/p;
  a2 = lv*r/((CPD + (r + l)*CL)*t);
  return(t*pow(a1,x1)*pow(rh,x2)*exp(a2));
}

/* compute saturation vapor pressure from dewpoint */
static float escalc(td)
float td;
{
  return(6.112*exp(17.67*(td - CTOK)/(td - 29.65)));
}
