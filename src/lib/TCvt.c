/*
 * Time conversions and other utilities.
 */
static char *rcsid = "$Id: TCvt.c,v 1.1 1990-11-21 11:07:14 corbet Exp $";

# include "defs.h"
# include <sys/types.h>
# include <sys/time.h>





void
TC_SysToFcc (sys, fcc)
long sys;
time *fcc;
/*
 * Convert a system time to an fcc time.
 */
{
	struct tm *t = gmtime (&sys);

	fcc->ds_yymmdd = t->tm_year*10000 + (t->tm_mon + 1)*100 + t->tm_mday;
	fcc->ds_hhmmss = t->tm_hour*10000 + t->tm_min*100 + t->tm_sec;
}





long
TC_FccToSys (fcc)
time *fcc;
/*
 * Convert an FCC time into a system time.
 */
{
	struct tm t;

	t.tm_year = fcc->ds_yymmdd/10000;
	t.tm_mon = (fcc->ds_yymmdd/100) % 100 - 1;
	t.tm_mday = fcc->ds_yymmdd % 100;
	t.tm_hour = fcc->ds_hhmmss/10000;
	t.tm_min = (fcc->ds_hhmmss/100) % 100;
	t.tm_sec = fcc->ds_hhmmss % 100;
	t.tm_zone = (char *) 0;
	t.tm_wday = t.tm_isdst = t.tm_yday = 0;

	return (timegm (&t));
}

