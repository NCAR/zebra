/* c_subs.c -- McIDAS fortran subroutines rewritten in C.
 *
 * Written by:  Nancy Rehak
 *              30 June 1992
 */


#include <stdio.h>
#include <math.h>
#include <memory.h>

#include "mcidas_server.h"


/* function declarations */
static void     init_statics();
static double   flalo_c();
static double   ftime_c();
static double   geolat_c();
static long     itime_c();
static long     icon1_c();
static void     epoch_c();
static void     satvec_c();
static void     nllxyz_c();
static void     nxyzll_c();
static void     angles_c();
static void     satpos_c();
static double   raerac_c();
static double   racrae_c();
static void     solarp_c();
static double   timdif_c();


/* constant data used in calculations */
#define A           6378.388
#define B           6356.912
#define EMEGA       0.26251617
#define EPSILON     0.00000001
#define GRACON      0.07436574
#define GRACON_RE   474.333543627    /* GRACON*RE */
#define IRAHMS      0L               /* HHMMSS when celestial coor. system */
                                     /*   ... coincides with earth coord   */
#define IRAYD       74001L           /* YYDDD when celestial coor. system */
                                     /*   ... coincides with earth coord  */
#define MISVAL      0x80808080
#define PI          3.14159265
#define PI12        0.261799388      /* PI/12.0 */
#define PI720       0.004363323125   /* PI/720.0 */
#define R           6371.221
#define RE          6378.388
#define PI180       0.0174532925     /* PI/180.0 */
#define RR          6371.221
#define SHA         100.26467
#define SHA_PI180   1.749948612926   /* SHA*PI180 */
#define SOLSID      1.00273791
#define SOLSID4     0.2506844775     /* SOLSID/4.0 */
#define SOLSID_PI720  0.004375269511 /* SOLSID*PI270 */
#define TWOPI       6.283185300      /* PI*2.0 */

#define leapyr(iy)  (366-((iy%4)+3)/4)



/* common NAVCOM variables */
struct {
  long    navday;
  long    ietimy;
  long    ietimh;
  double  semima;
  double  oeccen;        /* eccentricity of earth's orbit */
  double  orbinc;
  double  perhel;        /* perihelion */
  double  asnode;        /* ascending node */
  double  piclin;
} navcom = { 0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0,
	       0.0 };


/* common VASCOM variables */
struct {
  double  scan1;
  double  time1;
  double  scan2;
  double  time2;
} vascom = { 0.0, 0.0, 0.0, 0.0 };


/* common NAVINI variables */
struct {
  double  ab;
  double  asq;
  double  bsq;
  double  r;
  double  rsq;
  long    numsen;
  double  radlin;
  double  radele;
  double  picele;
  double  rfact;
  double  roasin;
  double  tmpscl;
  double  b11, b12, b13, b21, b22, b23, b31, b32, b33;
  double  gamma_var;
  double  gamdot;
  double  rotm11, rotm13, rotm21, rotm23, rotm31, rotm33;
  double  pictim;
  double  xref;
} navini = { 0.0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0.0,
	       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
	       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
	       0.0, 0.0 };

/* common NVUNIT variables */
struct {
  long    llsw;
  long    iold;
} nvunit = { 0, 0 };
  

/**************************************************************/

/* flalo_c - Converts a signed packed integer (DDDMMSS)
 *           latitude or longitude to a real value.
 *
 * Inputs:  m - latitude/longitude in +/- DDDMMSS format.
 *
 * Outputs: degrees - latitude/longitude in real format.
 */

static double flalo_c(m)
     long  m;
{
  double  degrees;
  double  sign = 1.0;

  /* save the sign of the input */
  if (m < 0)
    {
      sign = -1.0;
      m = -m;
    }

  /* convert the DDDMMSS format to real degrees */
  degrees = (double)(m/10000) + (double)((m/100) % 100)/60.0
                + (double)(m % 100)/3600.0;

  /* return the correct sign for the resulting degrees */
  degrees = sign * degrees;

  return(degrees);

} /* end of flalo_c */


/**************************************************************/

/* ftime_c - Converts a signed packed integer (HHMMSS) time to
 *           real hours.
 *
 * Inputs:  m - time in +/- HHMMSS format.
 *
 * Outputs: hours - time in real format.
 */

static double     ftime_c(m)
     long    m;
{
  double   hours;
  double   sign = 1.0;

  /* save the sign of the input */
  if (m < 0)
    {
      sign = -1.0;
      m = -m;
    }

  /* convert the packed integer (HHMMSS) to real hours */
  hours = (double)(m/10000) + (double)((m/100)%100)/60.0
                    + (double)(m%100)/3600.0;

  /* return the correct sign for the resulting hours */
  hours = sign * hours;

  return(hours);

} /* end of ftime_c */



/**************************************************************/

/* geolat_c - Geocentric/geodetic latitude conversion.
 *
 * Inputs:  xlat - input latitude in radians.
 *          idir - type of conversion to perform:
 *                 1 = geodedic to geocentric conversion
 *                 2 = geocentric to geodedic conversion
 *                 other = geodedic to geocentric conversion
 *
 * Outputs: ret_val - converted latitude in radians
 */

static double    geolat_c(xlat, idir)
     double   xlat;
     long     idir;
{
  /* constants used in this routine only */
  static double ASQ = A * A;
  static double BSQ = B * B;

  double        cx, sx;

  sincos(xlat, &sx, &cx);

  /* perform geocentric to geodedic conversion */
  if (idir == 2)
    return(atan2(ASQ*sx, BSQ*cx));

  /* perform geodedic to geocentric conversion */
  return(atan2(BSQ*sx, ASQ*cx));

} /* end of geolat_c */


/**************************************************************/

/* itime_c - Convert signed real hours to packed signed integer
 *           (HHMMSS) format.
 *
 * Inputs:  hours - signed real hours.
 *
 * Outputs: time - time in packed integer format +/- HHMMSS.
 */

static long itime_c(hours)
     double   hours;
{
  long  time;
  long  sign = 1.0;
  long  seconds;

  /* save the sign of the input time */
  if (hours < 0.0)
    {
      sign = -1;
      hours = -hours;
    }

  /* convert the hours to seconds (add 0.5 for rounding?) */
  seconds = 3600.0 * hours + 0.5;

  /* convert the seconds to packed integer format */
  time = 10000 * (seconds/3600) + 100 * ((seconds/60) % 60) + (seconds % 60);
  time = sign * time;

  return(time);

}  /* end of itime_c */


/**************************************************************/

/* nvxini_c - Calculates initial values for the static structures
 *            navcom and navini for use by the navigation transformation
 *            routines nvxsae and nvxeas.  nvxini should be recalled
 *            every time a transformation is desired for a picture
 *            with a different time that the previous call.
 *
 * Inputs:  ifunc - Function to perform:
 *                  1 = initialize values in static structures
 *                  2 = accept/produce all Earth coordinates in
 *                      lat/lon form if iarr is "LL  " or in the
 *                      x, y, z coordinate frame if iarr is "XYZ ".
 *                      This affects all subsequent nvxeas or
 *                      nvxsae calls.
 *          iarr - integer array (dimension 128) containing:
 *                 navigation parameters if ifunc = 1,
 *                 "LL  " or "XYZ " if ifunc = 2.
 *
 * Outputs: ret_val - status of function:
 *                    0 = success
 *                    -1 = failure
 */

int nvxini_c(ifunc, inbuf)
     long        ifunc;
     nx_nav_goes *inbuf;

{

  static long jdaysv = 0, jtimsv = 0;
  static long jinit = 0;
  static long goes_type = 0;

  char        cllsw[3];
  long        iss;
  double      ras, sinras, cosras;
  double      dec, sindec, cosdec;
  double      stp, ctp;
  double      sty, cty;
  double      str, ctr;
  double      xmeana;
  long        jday, jtime;
  long        lintot;
  float       deglin;
  long        ieltot;
  double      degele;
  double      spinra;
  double      declin;
  double      rascen;
  double      pitch;
  double      yaw;
  double      roll;
  double      skew;
  double      totlin;
  double      totele;
  double      cpitch;
  double      cyaw;
  double      croll;
  double      pskew;
/*  long        process_data = 0; */


  /* perform initializations for first time through routine */
  if (jinit == 0)
    {
      jinit = 1;
      nvunit.llsw = 0;     /* use lat/lon coordinates by default */
      jdaysv = -1;
      jtimsv = -1;
      nvunit.iold = 0;
      memcpy((char *) &goes_type, "GOES", 4);
    } /* endif */

  /* set the coordinate system to use and exit */
  if (ifunc == 2)
    {
      /* extract the coordinate system from the integer array */
      memcpy((char *) cllsw, (char *)inbuf, 2);
      cllsw[2] = '\0';

      /* check for lat/long coordinates */
      if (!strcmp(cllsw, "LL"))
	nvunit.llsw = 0;

      /* check for x, y, z coordinates */
      else if (!strcmp(cllsw, "XY"))
	nvunit.llsw = 1;
      return(0);
    } /* endif */

  /* only process GOES satellite data */
  if (inbuf->nav_type != goes_type)
    return(-1);

  jday = inbuf->sensor_source;
  jtime = inbuf->start_time;
  if (jday == jdaysv && jtime == jtimsv)
    return(0);

  /*************** initialize navcom ***********************/

  navcom.navday = jday % 100000;

  /* Cannot process data if semimajor axis, orbital eccentricity, orbital
   * inclination, mean anomaly, argument of perigee and right ascension
   * of ascending node are all less than or equal to 0.
   */
  if (inbuf->semimajor_axis <= 0 &&
      inbuf->orbital_ecc    <= 0 &&
      inbuf->orbital_incl   <= 0 &&
      inbuf->mean_anomaly   <= 0 &&
      inbuf->arg_of_perigee <= 0 &&
      inbuf->right_asc_asc  <= 0)
    return(-1);

/*  for (n = 6; n < 12; n++)
/*    if (iarr[n] > 0) process_data = 1;
/*  if (!process_data)
/*    return(-1);
*/

  navcom.ietimy = icon1_c(inbuf->epoch_date);
  navcom.ietimh = 100 * (inbuf->epoch_time/100)
                          + nint(0.6 * (inbuf->epoch_time % 100));
  navcom.semima = (double)(inbuf->semimajor_axis) / 100.0;
  navcom.oeccen = (double)(inbuf->orbital_ecc) / 1000000.0;
  navcom.orbinc = (double)(inbuf->orbital_incl) / 1000.0;
  xmeana = (double)(inbuf->mean_anomaly) / 1000.0;
  navcom.perhel = (double)(inbuf->arg_of_perigee) / 1000.0;
  navcom.asnode = (double)(inbuf->right_asc_asc) / 1000.0;
  epoch_c(&navcom.ietimy, &navcom.ietimh, navcom.semima, navcom.oeccen, xmeana);
  if (inbuf->epoch_date == 0)
    return(-1);
  declin = flalo_c(inbuf->decl_axis);
  rascen = flalo_c(inbuf->right_asc_axis);
  navcom.piclin = inbuf->pict_center;
  if (inbuf->pict_center >= 1000000)
    navcom.piclin = navcom.piclin / 10000.0;
  if (inbuf->decl_axis      == 0 &&
      inbuf->right_asc_axis == 0 &&
      inbuf->pict_center    == 0)
    return(-1);

  spinra = inbuf->spin_period / 1000.0;
  if (inbuf->spin_period != 0 && spinra < 300.0)
    spinra = 60000.0 / spinra;
  if (inbuf->spin_period == 0)
    return(-1);
  deglin = flalo_c(inbuf->sweep_ang_line);
  lintot = inbuf->scan_lines;
  degele = flalo_c(inbuf->sweep_ang_elem);
  ieltot = inbuf->num_elem;
  pitch = flalo_c(inbuf->pitch);
  yaw = flalo_c(inbuf->yaw);
  roll = flalo_c(inbuf->rotation);
  skew = inbuf->skew / 100000.0;
  if (inbuf->skew == MISVAL)
    skew = 0.0;
  
  /************************** initialize navini ************************/

  navini.ab = 40546851.22;
  navini.asq = 40683833.48;
  navini.bsq = 40410330.18;
  navini.r = 6371.221;
  navini.rsq = navini.r * navini.r;
  navini.numsen = (lintot / 100000) % 100;
  if (navini.numsen < 1)
    navini.numsen = 1;

  totlin = navini.numsen * (lintot % 100000);
  navini.radlin = PI180 * deglin / (totlin - 1.0);
  totele = ieltot;
  navini.radele = PI180 * degele / (totele - 1.0);
  navini.picele = (1.0 + totele) / 2.0;
  cpitch = PI180 * pitch;
  cyaw = PI180 * yaw;
  croll = PI180 * roll;
  pskew = atan2(skew, navini.radlin / navini.radele);

  sincos(cpitch, &stp, &ctp);
  sincos(cyaw - pskew, &sty, &cty);
  sincos(croll, &str, &ctr);

  navini.rotm11 = ctr * ctp;
  navini.rotm13 = sty * str * ctp + cty * stp;
  navini.rotm21 = -str;
  navini.rotm23 = sty * ctr;
  navini.rotm31 = -ctr * stp;
  navini.rotm33 = cty * ctp - sty * str * stp;
  navini.rfact = (navini.rotm31 * navini.rotm31) +
                     (navini.rotm33 * navini.rotm33);
  navini.roasin = atan2(navini.rotm31, navini.rotm33);
  navini.tmpscl = spinra / 3600000.0;
  dec = declin * PI180;
  sincos(dec, &sindec, &cosdec);
  ras = rascen * PI180;
  sincos(ras, &sinras, &cosras);

  navini.b11 = -sinras;
  navini.b12 = cosras;
  navini.b13 = 0.0;
  navini.b21 = -sindec * cosras;
  navini.b22 = -sindec * sinras;
  navini.b23 = cosdec;
  navini.b31 = cosdec * cosras;
  navini.b32 = cosdec * sinras;
  navini.b33 = sindec;

  navini.xref = raerac_c(navcom.navday, 0L, 0.0) * PI180;

  /* time-specific parameters (including gamma) */
  navini.pictim = flalo_c(jtime);
  navini.gamma_var = (double)(inbuf->gamma) / 100.0;
  navini.gamdot = (double)(inbuf->gamma_dot) / 100.0;

  /**************************** initialize vascom ************************/

  iss = jday / 100000;
  if ((iss > 25 || iss == 12) && inbuf->scan_1 > 0)
    {
      /* This section does VAS birds and GMS.
       * It uses times and scan line from beta records.
       */
      vascom.scan1 = (double)(inbuf->scan_1);
      vascom.time1 = flalo_c(inbuf->scan_1_time);
      vascom.scan2 = (double)(inbuf->scan_2);
      vascom.time2 = flalo_c(inbuf->scan_2_time);
    }
  else
    {
      /* This section does the old GOES birds */
      vascom.scan1 = 1.0;
      vascom.time1 = flalo_c(jtime);
      vascom.scan2 = (double)(lintot % 100000);
      vascom.time2 = vascom.time1 + vascom.scan2 * navini.tmpscl;
    }  /* endif */

  nvunit.iold = 0;

  /* All done.  Everything okay.  */
  jdaysv = jday;
  jtimsv = jtime;

  return(0);

} /* end of nvxini_c */



/**************************************************************/

/* nvxsae_c - Transform satellite coordinates to earth coordinates.
 *
 * Inputs:  xlin - satellite line image coordinate.
 *          xele - satellite element image coordinate.
 *          xdum - dummy argument - ignore.
 *
 * Outputs: xpar - latitude or x depending on option set in prior
 *                 nvxini call with ifunc = 2.
 *          ypar - longitude or y depending on option set in prior
 *                 nvxini call with ifunc = 2.
 *          zpar - dummy or z depending on option set in prior
 *                 nvxini call with ifunc = 2.
 *          ret_val - status of function:
 *                    0 = function executed properly
 *                    -1 = function error (e.g. off of earth)
 */

/*ARGSUSED*/

int nvxsae_c(xlin, xele, xdum, xpar, ypar, zpar)
     double    xlin;
     double    xele;
     double    xdum;
     double    *xpar;
     double    *ypar;
     double    *zpar;

{


  double          xsat, ysat;
  double          samtim;
  double          framet;
  double          parlin;
  long            ilin;
  double          st, ct;
  double          x, y, z;
  double          s;
  double          rad;
  double          aq, bq, cq;
  double          onemsq;
  double          basq;
  double          elo, emo, eno;
  double          temp;
  double          eli, emi, eni;
  double          coslin, sinlin;
  double          cosele, sinele;
  double          rot;
  double          xcor, ycor;
  double          ylin, yele;
  double          zsat;
  double          xx1, yy1;

  ilin = irint(xlin);
  parlin = (ilin - 1)/navini.numsen + 1;
  framet = navini.tmpscl * parlin;
  samtim = framet + navini.pictim;
  satvec_c(samtim, &xsat, &ysat, &zsat);
  ylin = (xlin - navcom.piclin) * navini.radlin;
  yele = (xele - navini.picele + navini.gamma_var + navini.gamdot * samtim)
                  * navini.radele;
  xcor = navini.b11 * xsat + navini.b12 * ysat + navini.b13 * zsat;
  ycor = navini.b21 * xsat + navini.b22 * ysat + navini.b23 * zsat;
  rot = atan2(ycor, xcor) + PI;
  yele = yele - rot;

  sincos(ylin, &sinlin, &coslin);
  sincos(yele, &sinele, &cosele);

  eli = navini.rotm11 * coslin - navini.rotm13 * sinlin;
  emi = navini.rotm21 * coslin - navini.rotm23 * sinlin;
  eni = navini.rotm31 * coslin - navini.rotm33 * sinlin;
  temp = eli;
  eli = cosele * eli + sinele * emi;
  emi = -sinele * temp + cosele * emi;
  elo = navini.b11 * eli + navini.b21 * emi + navini.b31 * eni;
  emo = navini.b12 * eli + navini.b22 * emi + navini.b32 * eni;
  eno = navini.b13 * eli + navini.b23 * emi + navini.b33 * eni;

  basq = navini.bsq / navini.asq;
  onemsq = 1.0 - basq;
  aq = basq + onemsq * eno * eno;
  bq = 2.0 * ((elo*xsat + emo*ysat) * basq + eno*zsat);
  cq = (xsat*xsat + ysat*ysat) * basq + zsat*zsat - navini.bsq;
  rad = bq*bq - 4.0*aq*cq;
  if (rad < 1.0)
    return(-1);
  s = -(bq + sqrt(rad)) / (2.0 * aq);
  x = xsat + elo * s;
  y = ysat + emo * s;
  z = zsat + eno * s;

  sincos(EMEGA * samtim + navini.xref, &st, &ct);
  xx1 = ct * x + st * y;
  yy1 = -st * x + ct * y;

  if (nvunit.llsw == 0)
    {
      nxyzll_c(xx1, yy1, z, xpar, ypar);
      *zpar = 0.0;
    }
  else
    {
      *xpar = xx1;
      *ypar = yy1;
      *zpar = z;
    } /* endif */

  return(0);

} /* end of nvxsae_c */

/**************************************************************/

/* nvxeas_c - Transform earth coordinates to satellite coordinates.
 *
 * Inputs:  xpar - latitude or x depending on the option set in prior
 *                 nvxini call with ifunc = 2.
 *          ypar - longitude or y depending on the option set in prior
 *                 nvxini call with ifunc = 2.
 *          zpar - dummy or z depending on the option set in prior
 *                 nvxini call with ifunc = 2.
 *
 * Outputs: xlin - satellite line image coordinate.
 *          xele - satellite element image coordinate.
 *          xdum - dummy (ignored).
 *          ret_val - function status value:
 *                    0 = function performed properly.
 *                    -1 = function error (e.g. bad latitude/longitude).
 */

int nvxeas_c(xpar, ypar, zpar, xlin, xele, xdum)
     double    xpar;
     double    ypar;
     double    zpar;
     double    *xlin;
     double    *xele;
     double    *xdum;

{

  static double   oldlin = 910.0;
  static double   orbtim = -99999.0;
  static double   xht = 0.0;

  long            ret_val = 0;
  double          scnnum;
/*  double          parlin; */
  double          umv;
  double          x3;
  double          xnorm, ynorm, znorm;
  double          vcses1, vcses2, vcses3;
  double          vcste1, vcste2, vcste3;
  double          x, y, z;
  double          xx1, yy1;
  double          ct, st;
  double          ctst;
  double          xsat, ysat, zsat;
  double          xsats1, ysats2;
  long            i;
  double          samtim;
  double          u, v;
  double          slin, clin;
  double          cosa;
  double          scnfrc;


  /* using latitude/longitude coordinates */
  if (nvunit.llsw == 0)
    {
      if (fabs(xpar) > 90.0)
        return(-1);
      nllxyz_c(xpar, ypar, &xx1, &yy1, &z);
    }
  /* using x, y, z coordinates */
  else
    {
      xx1 = xpar;
      yy1 = ypar;
      z = zpar;
    } /* endif */

  *xdum = 0.0;
  samtim = vascom.time1;

  for (i = 1; i <= 2; i++)
    {
      if (fabs(samtim-orbtim) >= 0.0005)
	{
	  satvec_c(samtim, &xsat, &ysat, &zsat);
	  orbtim = samtim;
	  xht = sqrt(xsat*xsat + ysat*ysat + zsat*zsat);
	} /* endif */

      sincos(EMEGA * samtim + navini.xref, &st, &ct);
      x = ct * xx1 - st * yy1;
      y = st * xx1 + ct * yy1;
      vcste1 = x - xsat;
      vcste2 = y - ysat;
      vcste3 = z - zsat;
      vcses3 = navini.b31*vcste1 + navini.b32*vcste2 + navini.b33*vcste3;
      znorm = sqrt(vcste1*vcste1 + vcste2*vcste2 + vcste3*vcste3);
      x3 = vcses3 / znorm;
      umv = atan2(x3, sqrt(navini.rfact - x3*x3)) - navini.roasin;
      *xlin = navcom.piclin - umv/navini.radlin;
/*      parlin = (long)(*xlin-1.0) / navini.numsen; */
      if (i != 2)
	{
	  samtim = vascom.time2;
	  oldlin = *xlin;
	} /* endif */
    } /* endfor */

  scnnum = ((long)(oldlin+*xlin)/2.0 - 1.0) / navini.numsen;
  scnfrc = (scnnum - vascom.scan1) / (vascom.scan2 - vascom.scan1);
  *xlin = oldlin + scnfrc * (*xlin - oldlin);
  samtim = vascom.time1 + navini.tmpscl * (scnnum - vascom.scan1);
  satvec_c(samtim, &xsat, &ysat, &zsat);
  cosa = x*xsat + y*ysat + z*zsat;
  ctst = 0.0001 * navini.r * xht + navini.rsq;
  if (cosa < ctst)
    ret_val = -1;
  xsats1 = navini.b11*xsat + navini.b12*ysat + navini.b13*zsat;
  ysats2 = navini.b21*xsat + navini.b22*ysat + navini.b23*zsat;
  sincos(EMEGA * samtim + navini.xref, &st, &ct);
  x = ct * xx1 - st * yy1;
  y = st * xx1 + ct * yy1;

  vcste1 = x - xsat;
  vcste2 = y - ysat;
  vcste3 = z - zsat;
  vcses1 = navini.b11*vcste1 + navini.b12*vcste2 + navini.b13*vcste3;
  vcses2 = navini.b21*vcste1 + navini.b22*vcste2 + navini.b23*vcste3;
  vcses3 = navini.b31*vcste1 + navini.b32*vcste2 + navini.b33*vcste3;
  xnorm = sqrt(znorm*znorm - vcses3*vcses3);
  ynorm = sqrt(xsats1*xsats1 + ysats2*ysats2);
  znorm = sqrt(vcste1*vcste1 + vcste2*vcste2 + vcste3*vcste3);

  x3 = vcses3 / znorm;
  umv = atan2(x3, sqrt(navini.rfact - x3*x3)) - navini.roasin;
  sincos(umv, &slin, &clin);
  u = navini.rotm11 * clin + navini.rotm13 * slin;
  v = navini.rotm21 * clin + navini.rotm23 * slin;
  *xele = navini.picele
             + sin((xsats1*vcses2 - ysats2*vcses1)/(xnorm*ynorm)) / navini.radele;
  *xele = *xele + atan2(v,u)/navini.radele;
  *xele = *xele - navini.gamma_var - navini.gamdot * samtim;

  return(ret_val);

} /* end of nvxeas */


/**************************************************************/

/* nvxopt_c - ???
 *
 * Inputs:  ifunc - indicates function to perform:
 *                  "SPOS" = subsatellite latitude/longitude.
 *                  "ANG " = angles.
 *                  "HGT " = input height for parallax.
 *          xin - input parameter array:
 *                if ifunc = "SPOS":
 *                   xin not used.
 *                if ifunc = "ANG ":
 *                   xin[0] = SSYYDDD.
 *                   xin[1] = time (hours).
 *                   xin[2] = latitude.
 *                   xin[3] = longitude (*** west negative ***)
 *                if ifunc = "HGT ":
 *                   xin[0] = height in km.
 *
 * Outputs: xout - output value array:
 *                 if ifunc = "SPOS":
 *                    xout[0] = sub-satellite latitude.
 *                    xout[1] = sub-satellite longitude.
 *                 if ifunc = "ANG ":
 *                    xout[0] = satellite angle.
 *                    xout[1] = sun angle.
 *                    xout[2] = relative angle.
 *                 if ifunc = "HGT ":
 *                    xout not used.
 */

int nvxopt_c(ifunc, xin, xout)
     long    ifunc;
     double  *xin;
     double  *xout;
{

  char     cfunc[5];
  int      ret_val;
  long     jday, jtime;
  double   x, y, z;
  long     ntime;
  long     inorb;
  double   flat, flon;
  double   gha;
  double   dec;
  double   xlat, xlon;
  double   zenloc;
  double   szen;
  double   relang;
  double   a_hgt, b_hgt;

  static long   lasday = -1;
  static long   lastim = -1;


  memcpy((char *) cfunc, (char *) &ifunc, 4);
  cfunc[4] = '\0';
  ret_val = 0;

  /* perform sub-satellite latitude/longitude function */
  if (!strcmp(cfunc, "SPOS"))
    {
      inorb = 0;
      ntime = itime_c(navini.pictim);
      satpos_c(&inorb, ntime, &x, &y, &z);
      nxyzll_c(x, y, z, &xout[0], &xout[1]);
    }

  /* perform angles function */
  else if (!strcmp(cfunc, "ANG "))
    {
      jday = irint(xin[0]);
      jtime = itime_c(xin[1]);
      flat = xin[2];
      flon = xin[3];
      if (jday != lasday || jtime != lastim)
        {
          solarp_c(jday, jtime, &gha, &dec, &xlat, &xlon);
          lasday = jday;
          lastim = jtime;
	} /* endif */
      angles_c(jday, jtime, flat, flon, gha, dec, &zenloc, &szen, &relang);
      xout[0] = zenloc;
      xout[1] = szen;
      xout[2] = relang;
    }

  /* perform input height for parallax function */
  else if (!strcmp(cfunc, "HGT "))
    {
      a_hgt = xin[0] + A;
      b_hgt = xin[0] + B;
      navini.asq = a_hgt * a_hgt;
      navini.bsq = b_hgt * b_hgt;
      navini.ab = a_hgt * b_hgt;
      navini.r = RR + xin[0];
      navini.rsq = navini.r * navini.r;
    }

  /* invalid function requested */
  else
    ret_val = 1;

  return(ret_val);

} /* end of nvxopt_c */

/************* Subsidiary Subprograms *************************/


/* icon1_c - Convert date format from YYMMDD to YYDDD (Julian)
 *           format.
 *
 * Inputs:  yymmdd - Date in YYMMDD format.
 *
 * Outputs: ret_val - Date in YYDDD (Julian) format.
 */

static long icon1_c(yymmdd)
    long   yymmdd;
{
  static long num[] = {0,31,59,90,120,151,181,212,243,273,304,334};

  long       year;
  long       month;
  long       day;
  long       julday;

  /* retrieve the year, month and day from the packed YYMMDD format */
  year = (yymmdd/10000) % 100;
  month = (yymmdd/100) % 100;
  day = yymmdd % 100;

  /* check for invalid month */
  if (month < 1 || month > 12)
    month = 1;

  /* calculate the Julian day (taking leap year into account) */
  julday = day + num[month-1];
  if ((year % 4) == 0 && month > 1)
    julday = julday + 1;

  /* return the date in YYDDD format */
  return(1000 * year + julday);

} /* end of icon1 */


/**************************************************************/

/* epoch_c - Find time of perigee from Keplerian epoch.
 *
 * Inputs:  semima - semimajor axis.
 *          oeccen - orbital eccentricity.
 *          xmeana - mean anomaly.
 *
 * Outputs: ietimy - perigee date in YYDDD (Julian) format.
 *          ietimh - perigee time in HHMMSS format.
 */

static void    epoch_c(ietimy, ietimh, semima, oeccen, xmeana)
     long     *ietimy;
     long     *ietimh;
     double   semima;
     double   oeccen;
     double   xmeana;

{

  double  xmmc;
  double  xmanom;     /* mean anomoly */
  double  time;
  double  time1;
  long    iday;
  long    jyear;
  long    jday;
  long    jtot;

  xmmc = GRACON * pow(sqrt(RE/semima), 3.0);
  xmanom = PI180 * xmeana;
  time = (xmanom - oeccen * sin(xmanom))/(60.0 * xmmc);
  time1 = flalo_c(*ietimh);
  time = time1 - time;
  iday = 0;

  if (time > 48.0)
    {
      time = time - 48.0;
      iday = 2;
    }
  if (time > 24.0)
    {
      time = time - 24.0;
      iday = 1;
    }
  if (time < -24.0)
    {
      time = time + 48.0;
      iday = -2;
    }
  if (time < 0.0)
    {
      time = time + 24.0;
      iday = -1;
    }

  *ietimh = itime_c(time);
  if (iday == 0)
    return;
  jyear = (*ietimy/1000) % 100;
  jday = *ietimy % 1000;
  jday = jday + iday;
  if (jday < 1)
    {
      jyear = jyear - 1;
      jday = leapyr(jyear) + jday;
    }
  else
    {
      jtot = leapyr(jyear);
      if (jday > jtot)
	{
	  jyear = jyear + 1;
	  jday = jday - jtot;
	}
    }

  *ietimy = 1000 * jyear + jday;
  return;

} /* end of epoch_c */


/**************************************************************/

/* satvec_c - Compute earth satellite as a function of time vector
 *            earth-center-to-satellite (function of time).
 *
 * Inputs:  samtim - ???.
 *
 * Outputs: x - x coordinate.
 *          y - y coordinate.
 *          z - z coordinate.
 */

static void    satvec_c(samtim, x, y, z)
     double    samtim;
     double    *x;
     double    *y;
     double    *z;
{
  /* constants used only in this routine */
  static long      IRAD = IRAYD % 1000;
  static long      IRAY = IRAYD / 1000;

  static double    tdife = 0.0;
  static double    px = 0.0, py = 0.0, pz = 0.0;
  static double    xmmc = 0.0;
  static double    srome2;
  static double    qx = 0.0, qy = 0.0, qz = 0.0;
/*  static long      navsav = 0; */
  static double    de = 0.0;
  static double    te = 0.0;
/*  static double    dra = 0.0;
  static double    tra = 0.0; */
  static double    dnav = 0.0;
/*  static double    tdifra = 0.0; */
  static double    sa = 0.0, ca = 0.0;
  static double    sp = 0.0, cp = 0.0;
  static double    so = 0.0, co = 0.0;
  static double    o = 0.0, p = 0.0, a = 0.0;
  static long      infac = 0;
  static long      inavd = 0;
  static long      inavy = 0;
/*  static long      irafac = 0; */
  static long      iefac = 0;
  static long      ied = 0;
  static long      iey = 0;

  double    diftim;
  double    ecanm1;
  double    ecanom;
  double    xmanom;              /* mean anomoly */
  double    xomega, yomega;
  long      i;
  double    timsam;


  if (nvunit.iold != 1)
    {
      nvunit.iold = 1;
/*      navsav = navcom.navday; */

      o = PI180 * navcom.orbinc;
      p = PI180 * navcom.perhel;
      a = PI180 * navcom.asnode;
      sincos(o, &so, &co);
      sp = sin(p) * navcom.semima;
      cp = cos(p) * navcom.semima;
      sincos(a, &sa, &ca);
      px = cp * ca - sp * sa * co;
      py = cp * sa + sp * ca * co;
      pz = sp * so;
      qx = -sp * ca - cp * sa * co;
      qy = -sp * sa + cp * ca * co;
      qz = cp * so;

      srome2 = sqrt(1.0 - navcom.oeccen) * sqrt(1.0 + navcom.oeccen);
      xmmc = GRACON_RE * sqrt(RE/navcom.semima) / navcom.semima;
      iey = (navcom.ietimy/1000) % 100;
      ied = navcom.ietimy % 1000;
      iefac = (iey - 1)/4 + 1;
      de = 365 * (iey-1) + iefac + ied - 1;
      te = 1440.0 * de + 60.0 * flalo_c(navcom.ietimh);
/*      irafac = (IRAY - 1)/4 + 1;
      dra = 365 * (IRAY - 1) + irafac + IRAD - 1;
      tra = 1440.0 * dra + 60.0 * flalo_c(IRAHMS); */
      inavy = (navcom.navday/1000) % 100;
      inavd = navcom.navday % 1000;
      infac = (inavy - 1)/4 + 1;
      dnav = 365 * (inavy - 1) + infac + inavd - 1;
      tdife = dnav * 1440.0 - te;
/*      tdifra = dnav * 1440.0 - tra; */
    } /* endif */

  timsam = samtim * 60.0;
  diftim = tdife + timsam;
  xmanom = xmmc * diftim;
  ecanm1 = xmanom;

  for (i = 0; i < 20; i++)
    {
      ecanom = xmanom + navcom.oeccen * sin(ecanm1);
      if (fabs(ecanom-ecanm1) < EPSILON)
	break;
      ecanm1 = ecanom;
    } /* endfor */

  xomega = cos(ecanom) - navcom.oeccen;
  yomega = srome2 * sin(ecanom);
  *z = xomega * pz + yomega * qz;
  *y = xomega * py + yomega * qy;
  *x = xomega * px + yomega * qx;

  return;

} /* end of satvec_c */


/**************************************************************/

/* nllxyz_c - Convert latitude/longitude to earth centered x, y, z.
 *
 * Inputs:  xlat - latitude in degrees (*** north positive ***)
 *          xlon - longitude in degrees (*** west positive ***)
 *
 * Outputs: x - x coordinate in km.
 *          y - y coordinate in km.
 *          z - z coordinate in km.
 *          (Note:  These are the coordinates in a rectangular frame
 *                  with origin at the earth center, whose positive
 *                  x-axis pierces teh equator at longitude 0 degrees,
 *                  whose positive y-axis pierces the equator at
 *                  longitude 90 degrees,and whose positive z-axis
 *                  intersects the north pole.)
 */

static void    nllxyz_c(xlat, xlon, x, y, z)
     double     xlat;
     double     xlon;
     double     *x;
     double     *y;
     double     *z;

{

  double     csln;
  double     cslt;
  double     snlt;
  double     snln;
  double     tnlt;
  double     ylat, ylon;
  double     r;

  ylat = PI180 * xlat;

  /* Convert to geocentric (spherical) latitude */
  ylat = atan2(navini.bsq * sin(ylat), navini.asq * cos(ylat));
  ylon = -PI180 * xlon;

  sincos(ylat, &snlt, &cslt);
  sincos(ylon, &snln, &csln);
  tnlt = (snlt/cslt) * (snlt/cslt);

  r = navini.ab * sqrt((1.0+tnlt)/(navini.bsq + navini.asq*tnlt));
  *x = r * cslt * csln;
  *y = r * cslt * snln;
  *z = r * snlt;

  return;

} /* end of nllxyz_c */

/**************************************************************/

/* nxyzll_c - Convert earth centered x, y, z to latitude and 
 *            longitude.
 *
 * Inputs: x - x coordinate in km.
 *         y - y coordinate in km.
 *         z - z coordinate in km.
 *          (Note:  These are the coordinates in a rectangular frame
 *                  with origin at the earth center, whose positive
 *                  x-axis pierces teh equator at longitude 0 degrees,
 *                  whose positive y-axis pierces the equator at
 *                  longitude 90 degrees,and whose positive z-axis
 *                  intersects the north pole.)
 *
 * Outputs: xlat - latitude in degrees (*** north positive ***)
 *          xlon - longitude in degrees (*** west positive ***)
 */

static void    nxyzll_c(x, y, z, xlat, xlon)
     double    x;
     double    y;
     double    z;
     double    *xlat;
     double    *xlon;

{

  double     a;

  *xlat = 100.0;
  *xlon = 200.0;

  if (x == 0.0 && y == 0.0 && z == 0.0)
    return;

  a = atan(z / sqrt(x*x + y*y));

  *xlat = atan2(navini.asq * sin(a), navini.bsq * cos(a)) / PI180;
  *xlon = -atan2(y, x) / PI180;

  return;

} /* end of nxyzll_c */

/**************************************************************/

/* angles_c - Compute zenith angles of sun and satellite and relative
 *            azimuth angle.
 *
 * Inputs:  jday - Picture day in YYDDD (Julian) format.
 *          jtime - Picture start time in HHMMSS format.
 *          xlat - Latitude of point in degrees.
 *          xlon - Longitude of point in degrees.
 *          gha - Greenwich hour angle of sun.
 *          dec - Declination of sun.
 *
 * Outputs: satang - Zenith angle of satellite.
 *          sunang - Zenith angle of sun.
 *          relang - Relative angle of sun.
 */

static void    angles_c(jday, jtime, xlat, xlon, gha, dec, satang, sunang, relang)
     long     jday;
     long     jtime;
     double   xlat;
     double   xlon;
     double   gha;
     double   dec;
     double   *satang;
     double   *sunang;
     double   *relang;

{

  long            iday = 0;

  double          xfact;
  double          xvec, yvec, zvec;
  double          us, vs, ws;
  double          cosdec;
  double          sndc;
  double          snlg;
  double          xsam, ysam, zsam;
  double          slat, clat, slon, clon;
  double          ylat, ylon;
  double          height;
  double          xsat, ysat, zsat;
  long            inorb;
  double          xan1, xan2, xan3;
  double          yan1, yan2, yan3;
  double          xx1, yy1, z1;
  double          x2, y2;
  double          x3, y3, z3;
  double          xc1, yc1, zc1;
  double          xc2, yc2, zc2;
  double          pictim;

  if (iday != jday)
    {
      iday = jday;
      inorb = 0;
    } /* endif */

  pictim = ftime_c(jtime);

  /* Determine satellite position */
  satpos_c(&inorb, jtime, &xsat, &ysat, &zsat);
  height = sqrt(xsat*xsat + ysat*ysat + zsat*zsat);
  ylat = PI180 * xlat;
  ylat = geolat_c(ylat, 1L);
  ylon = PI180 * xlon;
  sincos(ylat, &slat, &clat);
  sincos(ylon, &slon, &clon);
  xsam = R * clat * clon;
  ysam = R * clat * slon;
  zsam = R * slat;


  /* Determine the zenith angle of the sun */

  snlg = -pictim * PI12 - PI180 * gha;
  sndc = PI180 * dec;
  cosdec = cos(sndc);
  us = cos(snlg) * cosdec;
  vs = sin(snlg) * cosdec;
  ws = sin(sndc);
  *sunang = cos((us*xsam + vs*ysam + ws*zsam) / R) / PI180;


  /* Determine the zenith angle of the satellite */

  xvec = xsat - xsam;
  yvec = ysat - ysam;
  zvec = zsat - zsam;
  xfact = sqrt(xvec*xvec + yvec*yvec + zvec*zvec);
  *satang = cos((xvec*xsam + yvec*ysam + zvec*zsam) / (R * xfact)) / PI180;


  /* Determine the relative angle */

  xx1 = clat * clon;
  yy1 = clat*slon;
  z1 = slat;
  x2 = slon;
  y2 = -clon;
  x3 = -slat * clon;
  y3 = -slat * slon;
  z3 = clat;

  xc1 = us - xx1;
  yc1 = vs - yy1;
  zc1 = ws - z1;
  xc2 = xsat / height - xx1;
  yc2 = ysat / height - yy1;
  zc2 = zsat / height - z1;

  xan1 = xc1*x3 + yc1*y3 + zc1*z3;
  xan2 = xc2*x3 + yc2*y3 + zc2*z3;
  yan1 = xc1*x2 + yc1*y2;
  yan2 = xc2*x2 + yc2*y2;
  xan3 = xan1*xan2 + yan1*yan2;
  yan3 = -yan1*xan2 + xan1*yan2;

  *relang = atan2(yan3,xan3)/PI180;
  *relang = fabs(*relang);

  return;

} /* end of angles_c */

/**************************************************************/

/* satpos_c - Calculate the satellite position vector from the 
 *            earth's center.
 *
 * Inputs:  inorb - Input initialization flag - should = 0 on first
 *                  call to satpos_c, 1 on all subsequent calls.
 *          ntime - input time in HHMMSS format.
 *
 * Outputs: x - x coordinate of position vector.
 *          y - y coordinate of position vector.
 *          z - z coordinate of position vector.
 */

static void    satpos_c(inorb, ntime, x, y, z)
     long      *inorb;
     long      ntime;
     double    *x;
     double    *y;
     double    *z;

{
  static double   xmmc;
  static double   srome2;
  static double   qx, qy, qz;
  static double   px, py, pz;
  static double   o, p, a;
  static double   so, co, sp, cp, sa, ca;

  double   cra, sra;
  double   ra, ras;
  double   xs, ys, zs;
  double   xomega, yomega;
  double   ecanom;
  long     i;
  double   ecanm1;
  double   xmanom;                  /* mean anomoly */
  double   diftim;
  

  /* perform initializations on first call */
  if (*inorb == 0)
    {
      *inorb = 1;

      o = PI180 * navcom.orbinc;
      p = PI180 * navcom.perhel;
      a = PI180 * navcom.asnode;
      sincos(o, &so, &co);
      sp = sin(p) * navcom.semima;
      cp = cos(p) * navcom.semima;
      
      sincos(a, &sa, &ca);

      px = cp*ca - sp*sa*co;
      py = cp*sa + sp*ca*co;
      pz = sp * so;

      qx = -sp*ca - cp*sa*co;
      qy = -sp*sa + cp*ca*co;
      qz = cp * so;

      srome2 = sqrt(1.0 - navcom.oeccen) * sqrt(1.0 + navcom.oeccen);
      xmmc = GRACON * RE * sqrt(RE/navcom.semima) / navcom.semima;
    } /* endif */

  diftim = timdif_c(navcom.ietimy, navcom.ietimh, navcom.navday, ntime);
  xmanom = xmmc * diftim;
  ecanm1 = xmanom;

  for (i = 0; i < 20; i++)
    {
      ecanom = xmanom + navcom.oeccen * sin(ecanm1);
      if(fabs(ecanom-ecanm1) < EPSILON)
	break;
      ecanm1 = ecanom;
    } /* endfor */

  xomega = cos(ecanom) - navcom.oeccen;
  yomega = srome2 * sin(ecanom);
  xs = xomega*px + yomega*qx;
  ys = xomega*py + yomega*qy;
  zs = xomega*pz + yomega*qz;
  diftim = timdif_c(IRAYD, IRAHMS, navcom.navday, ntime);
  ra = diftim * SOLSID_PI720 + SHA_PI180;
  ras = ra - (long)(ra/(TWOPI)) * (TWOPI);
  sincos(ras, &sra, &cra);
  *x = cra*xs + sra*ys;
  *y = -sra*xs + cra*ys;
  *z = zs;

  return;

} /* end of satpos_c */

/**************************************************************/

/* raerac_c - Convert earth longitude to celestial longitude.
 *
 * Inputs:  iyrdy - Date in YYDDD (Julian) format.
 *          ihms - Time in HHMMSS format.
 *          rae - Earth longitude in degrees.
 *
 * Outputs: rac - Celestial longitude in degrees.
 *
 * Reference:  Escobal, "Methods of Orbit Determination."
 *             Wiley & Sons, 1965.
 * Greenwich sidereal time := angle between prime meridian
 *      and 0 degrees R.A.
 * Julian date := # days elapsed since 12 noon on Jan. 1, 4713 B.C.
 * Approximate formula for Greenwich sidereal time at 0Z:
 *      GST (deg) = S(0) = 99.6909833 + 36000.7689*C + 0.00038708*C*C,
 *          where: C = time in centuries = (J.D. - 2415020.0) / 36525
 * For GST at other times of (same) day, use:
 *      GST at time T = S(T) = S(0) + (T * DS/DT)
 *          DS/DT = 1 + (1 / 365.24219879) = 1.00273790827 revolutions/day
 *                = 0.250684477 degrees/minute
 *
 * From table, J.D. at 0Z on Jan. 1, 1974 = 2442048.5
 *      then S(0) at 0Z on Jan. 1, 1974 = 100.2601800
 */

static double   raerac_c(iyrdy, ihms, rae)
     long   iyrdy;
     long   ihms;
     double  rae;

{
  double   raha;
  double   rac;

  raha = rae + timdif_c(IRAYD, IRAHMS, iyrdy, ihms) * SOLSID4 + SHA;
  rac = raha - (long)(raha/360.0) * 360.0;
  if (rac < 0.0)
    rac += 360.0;

  return(rac);

} /* end of raerac_c */


/**************************************************************/

/* racrae_c - Convert celestial longitude to earth longitude.
 *
 * Inputs:  iyrdy - Date in YYDDD (Julian) format.
 *          ihms - Time in HHMMSS format;
 *          rac - Celestial longitude in degrees.
 *
 * Outputs: rae - Earth longitude in degrees.
 */

static double   racrae_c(iyrdy, ihms, rac)
     long    iyrdy;
     long    ihms;
     double  rac;

{

  double    raha;
  double    rae;

  raha = rac - SHA + timdif_c(iyrdy, ihms, IRAYD, IRAHMS) * SOLSID4;
  rae = raha - (long)(raha/360.0)*360.0;
  if (rae < 0.0)
    rae = rae + 360.0;

  return(rae);

} /* end of racrae_c */

/**************************************************************/

/* solarp_c - Compute Greenwich hour angle and declination of sun.
 *
 * Inputs:  jday - Satellite and date in SSYYDDD (Julian) format.
 *          jtime - Time in HHMMSS format.
 *
 * Outputs: gha - Greenwich hour angle.
 *          dec - Declination of sun.
 *          xlat - Latitude of sun position.
 *          xlon - Longitude of sun position.
 */

static void    solarp_c(jday, jtime, gha, dec, xlat, xlon)
     long    jday;
     long    jtime;
     double  *gha;
     double  *dec;
     double  *xlat;
     double  *xlon;

{
  /* constants used only in this routine */
  static long   IEPYD  = 74004;            /* epoch year-day */
  static long   IEPHMS = 0;                /* epoch hour-minute-second */
  static double XMMC   = 0.01720209895/1440.0;
  static double OECCEN = 0.016722;
  static double OECCEN2 = 0.000279625;     /* OECCEN * OECCEN */

  /* constants depending on a function call (cannot be initialized here) */
  static double PX, PY, PZ;
  static double QX, QY, QZ;
  static double SINC, CINC;
  static double SPER, CPER;
  static double SAND, CAND;
  static double OINCLI;           /* inclination to celstial equator */
  static double PERHEL;           /* perihelion */
  static double ASNODE;           /* ascending node */

  static long       init = 0;

  double    diftim;
  double    ecanm1;
  double    ecanom;
  double    raha;
  double    xha;
  double    xmanom;           /* mean anomoly */
  double    slra;
  double    xs, ys, zs;
  double    xomega, yomega;
  long      i;
  double    ptime;
  long      iday;
  double    xfact;

  if (init == 0)
    {
      init = 1;

      OINCLI = PI180 * flalo_c(232700L);
      PERHEL = PI180 * flalo_c(1011311L) + PI;
      ASNODE = PI180 * flalo_c(0L);

      sincos(OINCLI, &SINC, &CINC);
      sincos(PERHEL, &SPER, &CPER);
      sincos(ASNODE, &SAND, &CAND);

      PX = CPER*CAND - SPER*SAND*CINC;
      PY = CPER*SAND + SPER*CAND*CINC;
      PZ = SPER*SINC;
      QX = -SPER*CAND - CPER*SAND*CINC;
      QY = -SPER*SAND + CPER*CAND*CINC;
      QZ = CPER * SINC;
    } /* endif */

  iday = jday % 100000;
  ptime = ftime_c(jtime);
  diftim = timdif_c(IEPYD, IEPHMS, iday, jtime);
  xmanom = XMMC * diftim;
  ecanm1 = xmanom;

  for (i = 0; i < 20; i++)
    {
      ecanom = xmanom + OECCEN * sin(ecanm1);
      if (fabs(ecanom-ecanm1) < EPSILON)
	break;
      ecanm1 = ecanom;
    } /* endfor */

  xomega = cos(ecanom) - OECCEN;
  yomega = sqrt(1.0 - OECCEN2) * sin(ecanom);
  xfact = 1.0 / sqrt(xomega*xomega + yomega*yomega);

  xomega = xomega*xfact;
  yomega = yomega*xfact;
  xs = xomega*PX + yomega*QX;
  ys = xomega*PY + yomega*QY;
  zs = xomega*PZ + yomega*QZ;

  slra = atan2(ys, xs) / PI180;
  raha = timdif_c(IRAYD, IRAHMS, iday, jtime) * SOLSID4;
  *gha = ptime * 15.0;
  xha = 360.0 - SHA - raha + slra + *gha;
  *gha = xha - (long)(xha/360.0)*360.0;
  *gha = 360.0 - *gha - 2.0;
  *dec = atan2(zs, sqrt(xs*xs +ys*ys)) / PI180;
  *xlat = geolat_c(*dec * PI180, 1L) / PI180;
  *xlon = -*gha - ptime * 15.0 + 720.0;
  *xlon = *xlon - (long)(*xlon/360.0)*360.0;

  return;

} /* end of solarp_c */

/**************************************************************/

/* timdif_c - Computes time difference in minutes.
 *
 * Inputs:  iyrda1 - First date in YYDDD (Julian) format.
 *          ihms1 - First time in HHMMSS format.
 *          iyrda2 - Second date in YYDDD (Julian) format.
 *          ihms2 - Second time in HHMMSS format.
 *
 * Outputs: ret_val - Time difference in minutes.
 */

static double   timdif_c(iyrda1, ihms1, iyrda2, ihms2)
     long    iyrda1;
     long    ihms1;
     long    iyrda2;
     long    ihms2;

{

  double   d1, d2;
  double   t1, t2;
  long     iy1, id1, ifac1, iy2, id2, ifac2;

  /* convert the first time into minutes */
  iy1 = (iyrda1 / 1000) % 100;
  id1 = iyrda1 % 1000;
  ifac1 = (iy1 - 1)/4 + 1;
  d1 = 365 * (iy1 - 1) + ifac1 + id1 - 1;
  t1 = 1440.0*d1 + 60.0*flalo_c(ihms1);

  /* convert the second time into minutes */
  iy2 = (iyrda2 / 1000) % 100;
  id2 = iyrda2 % 1000;
  ifac2 = (iy2 - 1)/4 + 1;
  d2 = 365 * (iy2 - 1) + ifac2 + id2 - 1;
  t2 = 1440.0*d2 + 60.0*flalo_c(ihms2);

  return(t2 - t1);

} /* end of timdif_c */

