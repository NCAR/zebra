#include <time.h>

#if defined(sun)
   void gettim_(long int *);
#else
   void gettim(long int *);
#endif

#if defined(sun)
   void gettim_(long int *itime)
#else
   void gettim(long int *itime)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 GETTIM.C 25-Sep-90,11:46:46,`DAVES' First release                       */
/* 2 GETTIM.C 2-Jan-91,11:54:18,`SMG' change revision history comment lines  */
/* 3 GETTIM.C 3-Jan-91,12:12:20,`SMG' released                               */
/* 4 GETTIM.C 11-Feb-91,14:55:22,`SMG' add conditional compile option        */
/* 5 GETTIM.C 8-Jul-91,16:41:24,`SUEG' conform to ANSI standards             */
/* 6 GETTIM.C 9-Jul-91,13:43:56,`SUEG' resequence                            */
/* 7 GETTIM.C 16-Feb-92,14:37:10,`USER' Released for McIDAS-X Only           */
/**** McIDAS-AIX Revision History *** */
/*
   Return current time (HHMMSS)
*/
{
   struct tm *timexx;
   time_t nowtime;

   time(&nowtime);
   timexx = gmtime(&nowtime);
   *itime = timexx->tm_hour * 10000 + timexx->tm_min * 100 + timexx->tm_sec;
}
