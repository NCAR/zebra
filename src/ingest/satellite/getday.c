#include <time.h>

#if defined(UNDERSCORE)
  void getday_(int *);
#else
  void getday(int *);
#endif

#if defined(UNDERSCORE)
  void getday_(int *iday)
#else
  void getday(int *iday)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 GETDAY.C 25-Sep-90,11:46:52,`DAVES' First release                       */
/* 2 GETDAY.C 2-Jan-91,11:54:18,`SMG' change revision history lines          */
/* 3 GETDAY.C 3-Jan-91,10:48:38,`DAVES' Released                             */
/* 4 GETDAY.C 11-Feb-91,14:55:24,`SMG' add conditional compile option        */
/* 5 GETDAY.C 8-Jul-91,15:52:00,`SUEG' conform to ANSI standards             */
/* 6 GETDAY.C 9-Jul-91,13:43:04,`SUEG' ta e getgrax.c                        */
/* 7 GETDAY.C 16-Feb-92,14:37:02,`USER' Released for McIDAS-X Only           */
/**** McIDAS-AIX Revision History *** */
/*
   Return current day (YYDDD) 
*/
{
   struct tm *timexx;
   time_t nowtime;

   time(&nowtime);
   timexx = gmtime(&nowtime);
   *iday = timexx->tm_year * 1000 + timexx->tm_yday + 1; 
}
