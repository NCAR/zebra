/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */
 
/**** McIDAS Revision History *** */
/* 1 DAYTIME.C 13-Feb-96,13:06:06,`SUEG' initial release for johnp (6023)    */
/* 2 DAYTIME.C 14-Feb-96,9:34:04,`JOHNP' m0cydok and m0hmsok now Mc's        */
/* 3 DAYTIME.C 20-Mar-96,11:59:18,`SUEG' new improved version.               */
/* 4 DAYTIME.C 4-Apr-96,14:14:42,`SUEG' fixed problems with HP               */
/* 5 DAYTIME.C 5-Apr-96,10:56:44,`JOHNP' Remove n_mcidas.h include file      */
/* 6 DAYTIME.C 5-Apr-96,12:11:26,`JOHNP' :q!                                 */
/* 7 DAYTIME.C 17-Apr-96,14:48:04,`USER' Released                            */
/* 8 DAYTIME.C 30-May-96,7:06:00,`BILLL' Modified programmer helps           */
/* 9 DAYTIME.C 4-Jun-96,11:38:02,`JOHNP' added many new functions            */
/*      changed Mchmsok for hms < 0                                          */
/* 10 DAYTIME.C 7-Jun-96,15:36:46,`JOHNP' added leading 0 for mchmstostr     */
/*      when hours < 10                                                      */
/* 11 DAYTIME.C 26-Jun-96,9:46:18,`USER' Released                            */
/* 12 DAYTIME.C 26-Jul-96,7:15:08,`BILLL' Added programmer documentation     */
/*      (6653).                                                              */
/* 13 DAYTIME.C 15-Aug-96,12:20:06,`JOHNP' added form=4-6 to mchmstostr()    */
/*      also billl's doc changes (6798)                                      */
/* 14 DAYTIME.C 22-Aug-96,8:15:58,`BILLL' Fixed column fault in help         */
/* 15 DAYTIME.C 23-Sep-96,12:37:08,`USER' Released                           */
/* 16 DAYTIME.C 7-Oct-96,8:08:44,`BILLL' Fixed programmer interface block    */
/*      typos (7055).                                                        */
/* 17 DAYTIME.C 8-Oct-96,13:40:00,`BILLL' Column alignment fix in remarks.   */
/* 18 DAYTIME.C 21-Oct-96,16:22:18,`USER' Released                           */
/**** McIDAS Revision History *** */
 
#include <time.h> 
#include <string.h>
#include <stdlib.h>
#include "mcidas.h"

static int     
  M0basesecs       (time_t *);
static int     
  M0tmtocydhms     (struct tm * , int *, int *);

/*
*$ Name:
*$      gettim - Gets the current system time in hhmmss format.
*$
*$ Interface:
*$      subroutine
*$      gettim (integer hms)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      hms   - Current time in hhmmss format.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      The C language version of this subroutine is Mcgettime().
*$
*$ Categories:
*$      day/time
*/

void gettim_ (Fint *hms)
{
  (void) Mcgettime ((int *) hms);
  return;
}

/*
*$ Name:
*$      Mcgettime - Gets the current system time in hhmmss format.
*$
*$ Interface:
*$      int
*$      Mcgettime (int *hms)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      hms       - Current time in hhmmss format.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Unable to get current system time.
*$     -2         - Unable to convert to gmt.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mcgettime (int *hms)
{
  int ok;
  int day;

  ok = Mcgetdaytime (&day, hms);

  return (ok);
}

/*
*$ Name:
*$      Mcgetdaytime - Gets the current system day and time.
*$
*$ Interface:
*$      int
*$      Mcgetdaytime (*day , *hms)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day          - Current day in ccyyddd format.
*$      hms          - Current time in hhmmss format.
*$
*$ Return values:
*$      0            - Success.
*$     -1            - Unable to get current system time.
*$     -2            - Unable to convert current time to gmt.
*$     -3            - Unable to convert to ccyyddd and hhmmss.
*$
*$ Remarks:
*$      The time returned will be in UTC.
*$
*$ Categories:
*$      day/time
*/

int Mcgetdaytime (int *day , int *hms)
{
  struct tm             *t_tm;           /* temporary time structure */
  time_t                 current_time;   /* current time offset */
  int                    ok;             /* function return value */

  /* get the current system time */

  current_time = time ((time_t *) NULL);

  /* if you can't get the current time */

  if (current_time == (time_t) NULL)
  {
    return (-1);
  }

  /* convert the current time to the time structure for gmt */

  t_tm = gmtime (&current_time);
  if (t_tm == (struct tm *) NULL)
  {
    return (-2);
  }

  /* convert to ccyyddd and hhmmsss format */

  ok = M0tmtocydhms (t_tm , day , hms);
  if (ok < 0)
  {
    return (-3);
  }

  return (0);
}

/*
*$ Name:
*$      getday - Gets the current system day in yyddd format.
*$
*$ Interface:
*$      subroutine
*$      getday (integer day)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day   - Current day in yyddd format.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      This is FORTRAN-callable.
*$      The first two digits (cc) of the output field are removed.
*$
*$ Categories:
*$      day/time
*$
*/

void getday_ (Fint *day)
{
  (void) Mcgetday (day);
  *day = (Fint) ((*day) % 100000);

  return;
}

/*
*$ Name:
*$      Mcgetday - Gets the current system day in ccyyddd format.
*$
*$ Interface:
*$      int
*$      Mcgetday (*day)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day       - Current day in ccyyddd format.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Unable to get current system day.
*$     -2         - Unable to convert current day to gmt.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*$
*/

int Mcgetday (int *day)
{
  int    ok;
  int    hms;

  ok = Mcgetdaytime (day, &hms);

  return (ok);
}

/*
*$ Name:
*$      mcgetday - Gets the current system day in ccyyddd format.
*$
*$ Interface:
*$      integer function
*$      mcgetday (integer day)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day    - Current day in ccyyddd format.
*$
*$ Return values:
*$      0      - Success.
*$     -1      - Unable to get current system day.
*$     -2      - Unable to convert current day to gmt.
*$
*$ Remarks:
*$      The C language version of this function is Mcgetday().
*$
*$ Categories:
*$      day/time
*/

Fint mcgetday_ (Fint *day)
{
  return ((Fint) Mcgetday ((Fint *) day));
}

/*
*$ Name:
*$      mcgetdaytime - Gets the current system day and time.
*$
*$ Interface:
*$      integer function
*$      mcgetdaytime (integer day , integer time)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day         - Current day in ccyyddd format.
*$      time        - Current time in hhmmss format.
*$
*$ Return values:
*$      0           - Success.
*$     -1           - Unable to get current system time.
*$     -2           - Unable to convert current time to gmt.
*$
*$ Remarks:
*$      The C language version of this function is Mcgetdaytime ().
*$
*$ Categories:
*$      day/time
*/

Fint mcgetdaytime_ (Fint *day , Fint *time)
{
  int ok;

  ok =  Mcgetdaytime ((Fint *) day, (Fint *) time);

  return ((Fint) ok);
}

/*
*$ Name:
*$      mcsectodaytime - Converts seconds since 1 January 1970 to
*$                       ccyyddd/hhmmss.
*$
*$ Interface:
*$      integer
*$      mcsectodaytime (integer secs , integer day, integer hms)
*$
*$ Input:
*$      secs    - Seconds since 1 January 1970.
*$
*$ Input and Output:
*$    none
*$
*$ Output:
*$      day     - Day to convert in ccyyddd format.
*$      hms     - Time to convert in hhmmss format.
*$
*$ Return values:
*$      0       - Success.
*$     -1       - Unable to convert seconds to system structure.
*$     -2       - Unable to convert to ccyyddd and hhmmss.
*$
*$ Remarks:
*$      This is the FORTRAN callable version of Mcsectodaytime().
*$
*$ Categories:
*$      day/time
*/

Fint mcsectodaytime_ (Fint *secs , Fint *day, Fint *hms)
{
  int ok;

  ok = Mcsectodaytime ((time_t) *secs, (int *) day, (int *) hms);

  return ((Fint) ok);

}

/*
*$ Name:
*$      mcdaytimetosec - Converts day/time to the number of seconds since
*$                       00z 1 January, 1970.
*$
*$ Interface:
*$      integer
*$      mcdaytimetosec (integer day, integer hms, integer secs)
*$
*$ Input:
*$      day   - Day to convert in ccyyddd format.
*$      hms   - Time to convert in hhmmss format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      secs  - Number of seconds since 00Z 1 January 1970.
*$
*$ Return values:
*$      0     - Success.
*$     -1     - Unable to convert base time to system value.
*$     -2     - Unable to convert source time to system value.
*$     -3     - Invalid day format entered.
*$     -4     - Invalid time format entered.
*$
*$ Remarks:
*$      This is the FORTRAN callable version of Mcdaytimetosec().
*$
*$ Categories:
*$      day/time
*/

Fint mcdaytimetosec_ (Fint *day, Fint *hms, Fint *secs)
{
  int ok;

  ok = Mcdaytimetosec ((int) *day, (int) *hms, (time_t *) secs);

  return ((Fint) ok);

}

/*
*$ Name:
*$      Mcdaytimetosec - Converts the system's clock to absolute number of
*$                       seconds since 00z on 1 January 1970
*$
*$ Interface:
*$      include "time.h"
*$      int
*$      Mcdaytimetosec (int day, int hms, time_t *secs)
*$
*$ Input:
*$      day     - Day to convert in ccyyddd format.
*$      hms     - Time to convert in hhmmss format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      secs    - Number of seconds since 00Z 1 January 1970.
*$
*$ Return values:
*$      0       - Success.
*$     -1       - Unable to convert base time to system value.
*$     -2       - Unable to convert source time to system value.
*$     -3       - Invalid day format.
*$     -4       - Invalid time format.
*$
*$ Remarks:
*$      The base time is 00:00:00 on 1 January 1970.
*$
*$      This function should work fine until approximately the year
*$      2099. At that time the number of seconds since 1 Jan 1970 will
*$      no longer fit in a 32 bit unsigned integer.
*$
*$ Categories:
*$      day/time
*/

int Mcdaytimetosec (int day, int hms, time_t *secs)
{

  struct tm       t = {0};           /* temporary time structure */
  time_t          tsecs;             /* temporary seconds since 1970 */
  time_t          basesecs;          /* base seconds of 1970 */
  int             ok;                /* function return value */

  /* check for valid inputs */

  if (Mccydok (day) != 0)
  {
    return (-3);
  }

  if (Mchmsok (hms) != 0)
  {
    return (-4);
  }

  /* ok, if we have made it to here, the data should be good */

  t.tm_sec    = hms % 100;
  t.tm_min    = ((hms / 100) % 100);
  t.tm_hour   = hms / 10000;
  t.tm_mday   = (day % 1000);
  t.tm_year   = (day / 1000) - 1900;

  tsecs = mktime (&t);
  if (tsecs == (time_t) -1)
  {
    return (-2);
  }

  /* now create the base time seconds for 1 january 1970 */

  ok = M0basesecs (&basesecs);
  if (ok < 0)
  {
    return (-1);
  }

  *secs = tsecs - basesecs;

  return (0);
}

/*
*$ Name:
*$      Mcinctime - Increments a julian day and time by hhmmss increment.
*$
*$ Interface:
*$      int
*$      Mcinctime (int srcday , int srchms , int n_hms , int *destday,
*$                 int *desthms)
*$
*$ Input:
*$      srcday    - Julian day to be converted in ccyyddd format.
*$      srchms    - Hour of day to be converted in hhmmss format.
*$      n_hms     - Time increment in hhmmss format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      destday   - New Julian day.
*$      desthms   - New hour of day in hhmmss format.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Invalid day entered.
*$     -2         - Invalid time entered.
*$     -3         - Unable to convert from cyd to seconds.
*$     -4         - Unable to convert from seconds to cyd.
*$     -5         - Invalid time increment entered.
*$
*$ Remarks:
*$      Set n_hms > 0 to increment forward in time. Set n_hms < 0 to
*$      decrement.
*$
*$ Categories:
*$      day/time
*$
*/

int Mcinctime (int srcday , int srchms , int n_hms , int *destday,
               int *desthms)
{
  static const int seconds_per_hour = 3600;
  static const int seconds_per_min  = 60;

  time_t           seconds;                  /* absolute seconds */
  int              hrs;                      /* hours to increment in n_hms */
  int              mins;                     /* minutes to increment */
  int              secs;                     /* seconds to increment */
  int              total_inc;                /* total number of seconds to
					      * increment */
  int              ok;                       /* function return value */

  /* check for input error */

  ok = Mccydok (srcday);
  if (ok < 0)
  {
    return (-1);
  }
  ok = Mchmsok (srchms);
  if (ok < 0)
  {
    return (-2);
  }

  /*  check to see if increment is okay */
  ok = Mchmsok (abs (n_hms) % 10000);
  if (ok < 0)
  {
    return (-5);
  }

  /* convert day to seconds since 1 January 1970 */

  ok = Mcdaytimetosec (srcday , srchms , &seconds);
  if (ok < 0)
  {
    return (-3);
  }

  /* increment appropriately */

  hrs        = (abs(n_hms) / 10000);
  total_inc  = hrs  * seconds_per_hour;

  mins       = ((abs (n_hms) / 100) % 100);
  total_inc += mins * seconds_per_min;

  secs       = (abs (n_hms) % 100);
  total_inc += secs;

  total_inc  = (n_hms >= 0) ? total_inc : (-1 * total_inc);

  seconds += total_inc;

  /* convert new number of seconds back to julian day */

  ok = Mcsectodaytime (seconds , destday , desthms);

  if (ok < 0)
  {
    return (-4);
  }

  return (0);

}

/*
*$ Name:
*$      mcinctime - Increments a julian day and time by hhmmss time
*$                increment.
*$
*$ Interface:
*$      integer function
*$      mcinctime (integer srcday , integer srchms , integer nhhmmss ,
*$               integer dstday, integer dsthms)
*$
*$ Input:
*$      srcday    - Julian day in ccyyddd format to be converted.
*$      srchms    - Hour of day to be converted in hhmmss format.
*$      nhhmmss   - Number of hours to increment.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      dstday    - New Julian day.
*$      dsthms    - New hour of day in hhmmss format.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Invalid day entered.
*$     -2         - Invalid time entered.
*$     -3         - Unable to convert from cyd to seconds.
*$     -4         - Unable to convert from seconds to cyd.
*$     -5         - Invalid time increment entered.
*$
*$ Remarks:
*$      Set nhhmmss > 0 to increment forward in time. Set nhhmmss < 0 to
*$      decrement.
*$
*$      This is the FORTRAN-callable version of Mcinctime().
*$
*$ Categories:
*$      day/time
*$
*/

Fint mcinctime_ (Fint *srcday , Fint *srchms , Fint *nhhmmss , Fint *dstday,
                 Fint *dsthms)
{
  int ok;

  ok = Mcinctime ((int) *srcday , (int) *srchms , (int) *nhhmmss ,
                  (int *) dstday , (int *) dsthms);
  return (ok);
}

/*
*$ Name:
*$      mcincday - Increments a julian day by a day count.
*$
*$ Interface:
*$      integer function
*$      mcincday (integer srcday , integer ndays , integer dstday)
*$
*$ Input:
*$      srcday   - Julian day in ccyyddd format to be converted.
*$      ndays    - Number of days to increment.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      dstday   - New Julian day.
*$
*$ Return values:
*$      0        - Success.
*$     -1        - Invalid day entered.
*$     -2        - Unable to convert from cyd to seconds.
*$     -3        - Unable to convert from seconds to cyd.
*$
*$ Remarks:
*$      Set ndays > 0 to increment forward in time. Set ndays < 0 to
*$      decrement.
*$
*$      This is the FORTRAN-callable version of Mcincday().
*$
*$ Categories:
*$      day/time
*$
*/

Fint mcincday_ (Fint *srcday , Fint *ndays , Fint *dstday)
{
  int ok;

  ok = Mcincday ((int) *srcday , (int) *ndays , (int *) dstday);
  return (ok);
}

/*
*$ Name:
*$      Mcincday - Increments a julian day by a day count.
*$
*$ Interface:
*$      int
*$      Mcincday (int srcday , int n_days , int *destday)
*$
*$ Input:
*$      srcday   - Julian day in ccyyddd format to be converted.
*$      n_days   - Number of days to increment.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      destday  - New Julian day.
*$
*$ Return values:
*$      0        - Success.
*$     -1        - Invalid day entered.
*$     -2        - Unable to convert from cyd to seconds.
*$     -3        - Unable to convert from seconds to cyd.
*$
*$ Remarks:
*$      Set n_days > 0 to increment forward in time. Set n_days < 0 to
*$      decrement.
*$
*$ Categories:
*$      day/time
*$
*/

int Mcincday (int srcday , int n_days , int *destday)
{
  static int       seconds_per_day = 86400;
  time_t           seconds;                  /* absolute seconds */
  int              hms;                      /* time of day */
  int              ok;                       /* function return value */

  /* check for input error */

  ok = Mccydok (srcday);
  if (ok < 0)
  {
    return (-1);
  }

  /* convert day to seconds since 1 January 1970 */

  hms = 0;
  ok = Mcdaytimetosec (srcday , hms , &seconds);
  if (ok < 0)
  {
    return (-2);
  }

  /* increment appropriately */

  seconds += (seconds_per_day * n_days);

  /* convert new number of seconds back to julian day */

  ok = Mcsectodaytime (seconds , destday , &hms);

  if (ok < 0)
  {
    return (-3);
  }

  return (0);

}

/*
*$ Name:
*$      Mcdmytocyd - Converts day/month/year to ccyyddd.
*$
*$ Interface:
*$      int
*$      Mcdmytocyd (int day, int month, int year, int *ccyyddd)
*$
*$ Input:
*$      day        - Day of month
*$      month      - Month of year (1 = january)
*$      year       - Year
*$
*$ Input and Output:
*$      none
*$ 
*$ Output:
*$      ccyyddd    - Julian day
*$
*$ Return values:
*$      0          - Success.
*$     -1          - Year is out of range, 1970 - 2100.
*$     -2          - Month doesn't make sense, range (1-12).
*$     -3          - Day is not consistent with month and year.
*$     -4          - Unable to convert entered day to system time.
*$     -5          - Unable to convert to ccyyddd and hhmmss.
*$
*$ Remarks:
*$      The range of years must be between 1970 and 2100.
*$      The month range is 1 to 12.
*$
*$      Valid day numbers will vary with the month and year.
*$
*$ Categories:
*$      day/time
*/

int Mcdmytocyd (int day, int month, int year, int *ccyyddd)
{
  static int    days_in_month[] =
                                 {31,28,31,30,31,30,31,31,30,31,30,31};
  struct tm     t = {0};         /* structure to build mktime request */
  time_t        tsecs;           /* system time from mktime */
  int           ok;              /* function return value */
  int           hms;             /* time in hhmmss */

  /* check inputs */

  if (year < 1970 || year > 2100)
  {
    return (-1);
  }
  if (month < 1 || month > 12)
  {
    return (-2);
  }

  /* if a leap year, february has 29 days, otherwise it has 28 days */

  days_in_month[1] = (year % 4) ? 28 : 29;

  /* make certain the day makes sense for the given month */

  if (day < 1 || (day > days_in_month[month-1]))
  {
    return (-3);
  }

  /* build the time structure and get the system time */

  t.tm_year  = year - 1900;
  t.tm_mon   = month - 1;
  t.tm_mday  = day;

  tsecs      = mktime (&t);
  if (tsecs == (time_t) -1)
  {
    return (-4);
  }

  ok = M0tmtocydhms (&t , ccyyddd , &hms);
  if (ok < 0)
  {
    return (-5);
  }

  return (0);
}

/*
*$ Name:
*$      mcdmytocyd - Converts day/month/year to ccyyddd.
*$
*$ Interface:
*$      integer function
*$      mcdmytocyd (integer day, integer month, integer year,
*$                  integer ccyyddd)
*$
*$ Input:
*$      day       - Day of month.
*$      month     - Month of year (1 = january).
*$      year      - Year.
*$
*$ Input and Output:
*$      none
*$ 
*$ Output:
*$      ccyyddd   - Julian day.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Year is out of range, 1970 - 2100.
*$     -2         - Month doesn't make sense, range (1-12).
*$     -3         - Day is not consistent with month and year.
*$     -4         - Unable to convert entered day to system time.
*$
*$ Remarks:
*$      This is the FORTRAN-callable version of Mcdmytocyd().
*$
*$ Categories:
*$      day/time
*/

int mcdmytocyd_ (Fint *day, Fint *month, Fint *year, Fint *ccyyddd)
{
  int ok;

  ok = Mcdmytocyd ((int) *day, (int) *month, (int) *year, (int *) ccyyddd);

  return (ok);
}

/*
*$ Name:
*$      Mccydtodmy - Converts ccyyddd to day/month/year.
*$
*$ Interface:
*$      int
*$      Mccydtodmy (int ccyyddd, int *day, int *month, int *year)
*$
*$ Input:
*$      ccyyddd    - Julian day in ccyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day        - Day of the month  (range 1 - 31).
*$      month      - Month of the year (range 1 - 12).
*$      year       - Year (range 1970 - 2100).
*$
*$ Return values:
*$      0          - Success.
*$     -1          - Invalid input format.
*$     -2          - General error.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mccydtodmy (int ccyyddd , int *day, int *mon, int *year)
{
  time_t        tsecs;           /* absolute system seconds */
  struct tm     t = {0};         /* structure to build mktime request */

  if (Mccydok (ccyyddd) != 0)
  {
    return (-1);
  }

  /* ok, if we have made it to here, the data should be good */

  t.tm_mday   = (ccyyddd % 1000);
  t.tm_year   = (ccyyddd / 1000) - 1900;

  tsecs = mktime (&t);
  if (tsecs == (time_t) -1)
  {
    return (-2);
  }

  *day  = t.tm_mday;
  *mon  = t.tm_mon + 1;
  *year = 1900 + t.tm_year;

  return (0);
}

/*
*$ Name:
*$      mccydtodmy - Converts ccyyddd to day/month/year.
*$
*$ Interface:
*$      integer function
*$      mccydtodmy (integer ccyyddd, integer day, integer month,
*$                  integer year)
*$
*$ Input:
*$      ccyyddd    - Julian day in ccyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day        - Day of the month  (range 1 - 31).
*$      month      - Month of the year (range 1 - 12).
*$      year       - Year (range 1970 - 2100).
*$
*$ Return values:
*$      0          - Success.
*$     -1          - Invalid input format.
*$     -2          - General error.
*$
*$ Remarks:
*$      This is the FORTRAN-callable version of Mccydtodmy().
*$
*$ Categories:
*$      day/time
*/

int mccydtodmy_ (Fint *ccyyddd , Fint *day, Fint *mon, Fint *year)
{
  int ok;
 
  ok = Mccydtodmy ((int) *ccyyddd, (int *) day, (int *) mon,
                   (int *) year);

  return (ok);

}

/*
*$ Name:
*$      Mcsectodaytime - Converts number of seconds since 1 January 1970 to
*$                       day/time.
*$
*$ Interface:
*$      int
*$      Mcsectodaytime (time_t secs, int *cyd, int *hms)
*$
*$ Input:
*$      secs           - Seconds since 1 January 1970.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      cyd           - Julian date in ccyyddd format.
*$      hms           - Time in hhmmss format.
*$
*$ Return values:
*$      0             - Success.
*$     -1             - Unable to convert seconds to system structure.
*$     -2             - Unable to convert to ccyyddd and hhmmss.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mcsectodaytime (time_t secs, int *cyd, int *hms)
{
  struct tm             *t_tm;           /* temporary time structure */
  int                    ok;             /* function return value */

  /* convert the seconds to the tm structure */

  t_tm = gmtime (&secs);
  if (t_tm == (struct tm *) NULL)
  {
    return (-1);
  }

  /* convert the tm structure to ccyyddd and hhmmss */

  ok = M0tmtocydhms (t_tm , cyd , hms);
  if (ok < 0)
  {
    return (-2);
  }
  
  return (0);
}

/*
*| Name:
*|      M0basesecs - Gets the system's base seconds at 00:00:00UTC
*|                 on 1 January 1970
*| Interface:
*|      include <time.h>
*|      int
*|      M0basesecs (time_t *secs)
*|
*| Input:
*|      none
*|
*| Input and Output
*|      none
*|
*| Output:
*|      secs        - System seconds at 00:00:00UTC on 1 January 1970.
*|
*| Return values:
*|      0           - Success.
*|     -1           - Unable to calculate the base time.
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      day/time
*/

static int M0basesecs(time_t *secs)
{
  static time_t          seconds = 0;       /* base number of seconds */

  /*
   * if the base seconds have not previously been acquired, or there
   * was an error, get the base seconds
   */

  if (seconds == 0 || seconds == -1)
  {
    struct tm       bt = {0};               /* base time structure */

    (void) memset (&bt, 0, sizeof (struct tm));

   /* build the structure for 00:00:00 on 1 January 1970 */

    bt.tm_year  = 70;
    bt.tm_mday  = 1;

    seconds = mktime (&bt);

    /* if there was a error, return and try again later */

    if (seconds == (time_t) -1)
    {
      *secs = seconds;
      return (-1);
    }
  }

  *secs = seconds;

  return (0);
}

/*
*| Name:
*|      M0tmtocydhms - Converts the C time structure,tm , to ccyyddd and
*|                   hhmmss format.
*|
*| Interface:
*|      include <time.h>
*|      int M0tmtocydhms (struct tm *time_struct, int *cyd, int *hms)
*|
*| Input:
*|      time_struct  - Time structure to be converted.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      cyd          - Julian day in ccyyddd format.
*|      hms          - Time in hhmmsss format.
*|
*| Return values:
*|      0            - Success.
*|     -1            - Unable to convert.
*| 
*| Remarks:
*|      For a complete listing of the time structure see page 345 in the
*|      book "C a Reference Manual" third edition by Harbison and Steele.
*|
*| Categories:
*|      day/time
*| 
*/

static int M0tmtocydhms (struct tm *time_str, int *cyd, int *hms)
{

  /* convert the julian day */

  *cyd   = (1900 + time_str->tm_year) * 1000 + 
           (time_str->tm_yday + 1);

  /* convert the necessary portions of the time structure to hhmmss */

  *hms   = (time_str->tm_hour * 10000) +
           (time_str->tm_min  * 100) +
            time_str->tm_sec;

  return (0);
}

/*
*$ Name:
*$      Mccydok - Verifies that the ccyyddd format is correct.
*$
*$ Interface:
*$      int
*$      Mccydok (int ccyyddd)
*$
*$ Input:
*$      ccyyddd - Julian day to test.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - Success.
*$     -1       - (ccyyddd < 1970000) or (ccyyddd > 2100000).
*$     -2       - The day portion > 365 or 366 for leap years.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mccydok (int ccyyddd)
{
  int             jday;              /* input julian day */
  int             year;              /* input year */
  int             leap;              /* leap year flag */

  /* 
   * The valid day should be between 1 jan 1970 and about
   * the year 2100.  You can't store enough seconds in a 32 bit
   * unsigned integer to go beyond about 130 years.
   */

  if (ccyyddd < 1970001 || ccyyddd > 2100001)
  {
    return (-1);
  }

  year    = ccyyddd / 1000;
  leap    = (year % 4) ? 0 : 1;
  jday    = ccyyddd % 1000;

  /*
   * if the julian day is greater that 366 or greater than 365 and
   * it is not a leap year, or the julian day is 0
   */

  if ((jday > 366) || (jday == 366 && leap == 0) || jday == 0)
  {
    return (-2);
  }

  return (0);
}

/*
*$ Name:
*$      mccydok - Verifies that the ccyyddd format is correct.
*$
*$ Interface:
*$      integer function
*$      mccydok (integer ccyyddd)
*$
*$ Input:
*$      ccyyddd - Julian day to test.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - Success.
*$     -1       - (ccyyddd < 1970000) or (ccyyddd > 2100000).
*$     -2       - The day portion > 365 or 366 for leap years.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

Fint
  mccydok_ (Fint *ccyyddd)
{
  return ((Fint) Mccydok ((int) *ccyyddd));
}

/*
*$ Name:
*$      Mchmsok - Checks format of the input field (HHMMSS).
*$
*$ Interface:
*$      int
*$      Mchmsok (int hhmmss)
*$
*$ Input:
*$      hhmmss  - Field whose format is being tested.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - Success.
*$     -3       - minute > 59.
*$     -4       - second > 59.
*$
*$ Remarks:
*$      This function just checks to be certain that neither the
*$      minute nor the second count is greater that 59.
*$
*$      The input time can be positive or negative. If you want
*$      to specifically check for a valid time of day call McIsTimeOfDay().
*$
*$ Categories:
*$      day/time
*/

int Mchmsok (int hms)
{

  int        abshms;       /* absolute value of entered hms */
  int        minute;       /* minute of the hour */
  int        second;       /* second of the minute */

  abshms = abs (hms);

  /* do specific checks for time */

  minute  = ((abshms / 100) % 100);
  second  = abshms % 100;

  /* check to make certain the minute and second is ok */

  if (minute > 59)
  {
    return (-3);
  }
  if (second > 59)
  {
    return (-4);
  }
  return (0);
}

/*
*$ Name:
*$      mchmsok - Checks the format of the input field (HHMMSS).
*$
*$ Interface:
*$      integer function
*$      mchmsok (integer hhmmss)
*$
*$ Input:
*$      hhmmss  - Field being tested.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - Success.
*$     -3       - minute > 59.
*$     -4       - second > 59.
*$
*$ Remarks:
*$      This function just checks to be certain that neither the
*$      minute nor the second count is greater that 59.
*$
*$      The input time can be positive or negative. If you want
*$      to specifically check for a valid time of day call
*$      mcistimeofday().
*$
*$ Categories:
*$      day/time
*/

Fint
  mchmsok_ (Fint *hhmmss)
{
  return ((Fint) Mchmsok ((int) *hhmmss));
}

/*
*$ Name:
*$      McIsTimeOfDay - Determines whether a time value is actually
*$                      a time of day.
*$
*$ Interface:
*$      int
*$      McIsTimeOfDay (int hms)
*$
*$ Input:
*$      hms       - time value to test
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0         - Entered value is a time of day.
*$     <0         - Entered value is not a time of day.
*$
*$ Remarks:
*$      This function checks to make certain that the entered time
*$      is greater or equal to 0 and less than or equal to 235959. It
*$      also verifies that both the minutes and seconds portions are
*$      less than 60.
*$
*$ Categories:
*$      day/time
*/

int
  McIsTimeOfDay (int hms)
{
  int        ok;           /* function return value */

  if (hms < 0 || hms > 235959)
  {
    return (-1);
  }

  return (Mchmsok (hms));
}

/*
*$ Name:
*$      mcistimeofday - determines whether a time value is actually
*$                      a time of day
*$
*$ Interface:
*$      integer function
*$      mcistimeofday (integer hms)
*$
*$ Input:
*$      hms       - time value to test
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0         - entered value is a time of day
*$     <0         - entered value is not a time of day
*$
*$ Remarks:
*$      This function checks to make certain that the entered time
*$      is greater or equal to 0 and less than or equal to 235959. It
*$      also verifies that both the minutes and seconds portions are
*$      less than 60.
*$
*$ Categories:
*$      day/time
*/

Fint
  mcistimeofday_ (Fint *hms)
{
  return ((Fint) McIsTimeOfDay ((int) *hms));
}

/*
*$ Name:
*$      Mccydtoyd - Converts a Julian day of the form ccyyddd to a Julian
*$                  day of the form yyddd.
*$
*$ Interface:
*$      int
*$      Mccydtoyd (int ccyyddd, int *yyddd)
*$
*$ Input:
*$      ccyyddd       - Source Julian day of the form ccyyddd.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      yyddd         - Resulting day of the form yydddc.
*$
*$ Return values:
*$      0             - Success.
*$     -1             - Failure.
*$
*$ Remarks:
*$      This function just performs a modulus of the source day.
*$
*$      If the source day is already in yyddd format, it will just
*$      move the same value into the destination day.
*$
*$ Categories:
*$      day/time
*/

int
  Mccydtoyd (int ccyyddd, int *yyddd)
{

  if (ccyyddd <= 0)
  {
    return (-1);
  }

  *yyddd = (ccyyddd % 100000);
  return (0);
}

/*
*$ Name:
*$      mccydtoyd - Converts a Julian day of the form ccyyddd to a Julian
*$                  day of the form yyddd.
*$
*$ Interface:
*$      integer function
*$      mccydtoyd (integer ccyyddd, integer yyddd)
*$
*$ Input:
*$      ccyyddd       - Source Julian day of the form ccyyddd.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      yyddd         - Resulting day of the form yyddd.
*$
*$ Return values:
*$      0             - Success.
*$     -1             - Failure.
*$
*$ Remarks:
*$      This function just performs a modulus of the source day.
*$
*$      If the source day is already in yyddd format, it will just
*$      move the same value into the destination day.
*$
*$ Categories:
*$      day/time
*/

Fint
  mccydtoyd_ (Fint *src, Fint *des)
{
  return ((Fint) Mccydtoyd ((int) *src, (int *) des));
}

/*
*$ Name:
*$      Mcydtocyd - Converts a Julian day of the form yyddd to a Julian
*$                  day of the form ccyyddd.
*$
*$ Interface:
*$      int
*$      Mcydtocyd (int yyddd, int *ccyyddd)
*$
*$ Input:
*$      yyddd      - Source Julian day of the form yyddd.
*$ 
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ccyyddd    - Resulting day of the form ccyyddd.
*$ 
*$ Return values:
*$      0          - Success.
*$     <0          - Failure.
*$
*$ Remarks:
*$
*$      If the source day is already in yyddd format, it will just
*$      move the same value into the destination day.
*$
*$      If the year portion of yyddd is between 70 and 99, inclusive,
*$      this function will assume that the century you want is the 1900's.
*$
*$      If the year portion of yyddd is less than 70 this function will
*$      assume that the century you want is in the 2000s.
*$
*$ Categories:
*$      day/time
*/

int
  Mcydtocyd (int yyddd, int *ccyyddd)
{
  int      yd;                    /* guaranteed yyddd form */

  if (yyddd <= 0)
  {
    return (-1);
  }

  *ccyyddd = yyddd;

  yd = (yyddd % 100000);

  /* if yyddd is <= yd, assume it src was in yyddd form */

  if (yyddd <= yd)
  {
    int      year;                  /* year of century */

    year = yd / 1000;

    if (year < 70)
    {
      *ccyyddd = 2000000 + yd;
    }
    else
    {
      *ccyyddd = 1900000 + yd;
    }
  }
  return (0);
}

/*
*$ Name:
*$      mcydtocyd - Converts a Julian day of the form yyddd to a Julian
*$                  day of the form ccyyddd.
*$
*$ Interface:
*$      integer
*$      mcydtocyd (integer yyddd, integer ccyyddd)
*$
*$ Input:
*$      yyddd      - Source Julian day of the form yyddd.
*$ 
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ccyyddd    - Resulting day of the form ccyyddd.
*$ 
*$ Return values:
*$      0          - Success.
*$     <0          - Failure.
*$
*$ Remarks:
*$
*$      If the source day is already in yyddd format, it will just
*$      move the same value into the destination day.
*$
*$      If the year portion of yyddd is between 70 and 99, inclusive,
*$      this function will assume that the century you want is the 1900's.
*$
*$      If the year portion of yyddd is less than 70 this function will
*$      assume that the century you want is in the 2000s.
*$
*$ Categories:
*$      day/time
*/

Fint
  mcydtocyd_ (Fint *src, Fint *des)
{
  return ((Fint) Mcydtocyd ((int) *src, (int *) des));
}

/*
*$ Name:
*$      Mccydtostr - Converts a Julian day to a character string.
*$
*$ Interface:
*$      int
*$      Mccydtostr (int day, int form, char **string)
*$
*$ Input:
*$      day          - Source Julian day to convert.
*$      form         - Output format desired for string.
*$
*$ Input and Output:
*$      none
*$ 
*$ Output:
*$      string       - Destination character string.
*$
*$ Return values:
*$      0            - Success.
*$     -8            - Invalid form specified.
*$     -9            - Unable to convert to day/month/year.
*$    -10            - Memory allocation error.
*$      other neg    - Invalid input day.
*$
*$ Remarks:
*$      This function allocates memory for the output string. It is up
*$      to the calling program to free the memory when finished.
*$
*$      If the input value for yyddd is 95017 then:
*$           form       string
*$            1         01/17/95
*$            2         17/01/95
*$            3         Jan 17,1995
*$            4         17 Jan 1995
*$            5         January 17,1995
*$            6         17 January 1995
*$            7         1995/01/17   (ISO Standard)
*$            8         95/01/17     (ISO Standard)
*$
*$ Categories:
*$      day/time
*/

int
  Mccydtostr (int ccyyddd, int form, char **string)
{
  int   ok;                 /* function return value */
  int   day;                /* day of month */
  int   month;              /* month of year */
  int   year;               /* year portion of ccyyddd */

  static char t_string[80]; /* temporary string */

  static const char *Smonths[] = {"Jan",
                                  "Feb",
                                  "Mar",
                                  "Apr",
                                  "May",
                                  "Jun",
                                  "Jul",
                                  "Aug",
                                  "Sep",
                                  "Oct",
                                  "Nov",
                                  "Dec"
                                };
  static const char *Lmonths[] = {"January",
                                  "February",
                                  "March",
                                  "April",
                                  "May",
                                  "June",
                                  "July",
                                  "August",
                                  "September",
                                  "October",
                                  "November",
                                  "December"
                                };

  /* verify input format */

  ok = Mccydok (ccyyddd);
  if (ok < 0)
  {
    return (ok);
  }

  /* if we make it to here we know the input value is valid */

  ok = Mccydtodmy (ccyyddd, &day, &month, &year);
  if (ok < 0)
  {
    return (-9);
  }

  /* now convert to a temporary string based on the form */

  switch (form)
  {
    case 1:
      (void) sprintf (t_string, 
                     "%02d/%02d/%02d", day, month, (year % 100));
      break;
    case 2:
      (void) sprintf (t_string, 
                     "%02d/%02d/%02d", month, day, (year % 100));
      break;
    case 3:
      (void) sprintf (t_string,
                     "%s %02d,%d", Smonths[month-1], day, year);
      break;
    case 4:
      (void) sprintf (t_string,
                     "%02d %s %d", day, Smonths[month-1], year);
      break;
    case 5:
      (void) sprintf (t_string,
                     "%s %02d,%d", Lmonths[month-1], day, year);
      break;
    case 6:
      (void) sprintf (t_string,
                     "%02d %s %d", day, Lmonths[month-1], year);
      break;
    case 7:
      (void) sprintf (t_string,
                     "%d/%02d/%02d", year, month, day);
      break;
    case 8:
      (void) sprintf (t_string,
                     "%d/%02d/%02d", (year % 100), month, day);
      break;

    default:
      return (-8);
  }

  *string = strdup (t_string);
  if (*string == (char *) NULL)
  {
    return (-10);
  }
  return (0);
}

/*
*$ Name:
*$      mccydtostr - Converts a Julian day to a character string.
*$
*$ Interface:
*$      integer
*$      mccydtostr (integer day, integer form, chararacter*(*) string)
*$
*$ Input:
*$      day          - Source Julian day to convert.
*$      form         - Output format desired for string.
*$
*$ Input and Output:
*$      none
*$ 
*$ Output:
*$      string       - Destination character string.
*$
*$ Return values:
*$      0            - Success.
*$     -7            - Not enough room for output string.
*$     -8            - Invalid form specified.
*$     -9            - Unable to convert to day/month/year.
*$    -10            - Memory allocation error.
*$      other neg    - Invalid input day.
*$
*$   Remarks:
*$      If the input value for yyddd is 95017 then:
*$           form       string
*$            1         01/17/95
*$            2         17/01/95
*$            3         Jan 17,1995
*$            4         17 Jan 1995
*$            5         January 17,1995
*$            6         17 January 1995
*$            7         1995/01/17   (ISO Standard)
*$            8         95/01/17     (ISO Standard)
*$
*$ Categories:
*$      day/time
*/

Fint
  mccydtostr_ (Fint *day, Fint *form, char *string, FsLen s_len)
{
  char        *t_string;
  size_t       t_len;              /* length of temporary string */
  int          ok;                 /* function return value */

  ok = Mccydtostr ((int) *day, (int) *form, &t_string);
  if (ok < 0)
  {
    return (ok);
  }

  t_len = strlen (t_string);
  if (t_len > s_len)
  {

    free (t_string);
    return (-7);
  }

  strtofs (string, t_string, s_len);
  free (t_string);

  return (0);
}

/*
*$ Name:
*$      Mchmstostr - Converts a time to a character string.
*$
*$ Interface:
*$      int
*$      Mchmstostr (int hms, int form, char **string)
*$
*$ Input:
*$      hms          - Time of the form hhmmss.
*$      form         - Output format desired for string.
*$
*$ Input and Output:
*$      none
*$ 
*$ Output:
*$      string       - Destination character string.
*$
*$ Return values:
*$      0            - Success.
*$     -1            - Invalid value for hms.
*$     -2            - Invalid value for form.
*$     -3            - Memory allocation error.
*$
*$ Remarks:
*$      This function allocates memory for the output string. It is up
*$      to the calling program to free the memory when finished.
*$
*$        If the input value for HHMMSS is 23444 then:
*$              form       string
*$               1         02:34:44Z  (UTC, i.e. zulu time)
*$               2         02:34:44
*$               3         02:34:44UTC
*$               4         02:34:44 Z
*$               5         02:34:44 UTC
*$               6         2:34:44
*$
*$ Categories:
*$      converter
*$      day/time
*/

int
  Mchmstostr (int hms, int form, char **string)
{
  static char    t_string[80];  /* temporary string variable */
  int            ok;            /* function return value */
  int            abshms;        /* absolute time */
  int            hour;          /* number of hours in hms */
  int            minute;        /* number of minutes in hms */
  int            seconds;       /* number of seconds in hms */
  char           sign[2];       /* sign flag */
  char           format[100];   /* output format to use */
  int            leading_zeros; /* flag indicating whether you need
                                 * to specify leading zeros */

  /* validate the input */

  ok = Mchmsok (hms);
  if (ok < 0)
  {
    return (-1);
  }

  /* if we make it to here we know that the input value is hip */

  /*
   * we have to do something a bit goofy here to ensure that the 
   * minus sign gets printed in the appropriate place. the value
   * stored in the string 'sign' will either be a NULL if positive
   * or a '-' if hms < 0
   */

  sign[0] = '\0';
  if (hms < 0)
  {
    (void) strcpy (sign, "-");
  }

  /* extract the elements for the time */

  abshms = abs (hms);

  hour    = abshms / 10000;
  minute  = ((abshms / 100) % 100);
  seconds = abshms % 100;

  /* set a flag if you need leading zeros */

  leading_zeros = (hour < 10) ? 1 : 0;

  /* set up the output format string */

  switch (form)
  {
    case 1: case 2: case 3: case 4: case 5:

      /* if you need leading zeros for the hour format */

      if (leading_zeros == 1)
      {
        (void) strcpy (format, "%s%02d:%02d:%02d");
      }

      /* if you don't need leading zeros for the hour format */

      else
      {
        (void) strcpy (format, "%s%d:%02d:%02d");
      }

      /* if they specify the cute little suffix, append it here */

      switch (form)
      {
        case 1:
          (void) strcat (format, "Z");
          break;
        case 3:
          (void) strcat (format, "UTC");
          break;
        case 4:
          (void) strcat (format, " Z");
          break;
        case 5:
          (void) strcat (format, " UTC");
          break;
        default:
          break;
      }

      break;

    case 6:
      (void) strcpy (format, "%s%d:%02d:%02d");
      break;

    default:
      return (-2);
  }

  (void) sprintf (t_string, format , sign, hour, minute, seconds);

  /* if we have made it to here, allocate the space and exit */

  *string = strdup (t_string);
  if (*string == (char *) NULL)
  {
    return (-3);
  }
  return (0);
}

/*
*$ Name:
*$      mchmstostr - Converts a time to a character string.
*$
*$ Interface:
*$      integer
*$      mchmstostr (integer hms, integer form, character*(*) string)
*$
*$ Input:
*$      hms          - Time of the form hhmmss.
*$      form         - Output format desired for string.
*$
*$ Input and Output:
*$      none
*$ 
*$ Output:
*$      string       - Destination character string.
*$
*$ Return values:
*$      0            - Success.
*$     -1            - Invalid value for hms.
*$     -2            - Invalid value for form.
*$     -4            - Destination string not long enough.
*$
*$ Remarks:
*$        If the input value for HHMMSS is 23444 then:
*$              form       string
*$               1         02:34:44Z  (UTC, i.e. zulu time)
*$               2         02:34:44
*$               3         02:34:44UTC
*$               4         02:34:44 Z
*$               5         02:34:44 UTC
*$               6         2:34:44
*$
*$ Categories:
*$      converter
*$      day/time
*/

Fint
  mchmstostr_ (Fint *hms, Fint *form, char *string, FsLen s_len)
{
  char        *t_string;
  size_t       t_len;              /* length of temporary string */
  int          ok;                 /* function return value */

  ok = Mchmstostr ((int) *hms, (int) *form, &t_string);
  if (ok < 0)
  {
    return (ok);
  }

  t_len = strlen (t_string);
  if (t_len > s_len)
  {

    free (t_string);
    return (-4);
  }

  strtofs (string, t_string, s_len);
  free (t_string);
  return (0);
}
