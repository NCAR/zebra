/*
 * Time conversions and other utilities.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include "defs.h"
# include <time.h>
# include <ctype.h>
# include <sys/types.h>

MAKE_RCSID ("$Id: TCvt.c,v 2.12 1994-03-21 20:27:02 burghart Exp $")

/*
 * The months of the year.
 */
static char *Months[] =
{
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};



void
TC_SysToFcc (sys, fcc)
long sys;
UItime *fcc;
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
UItime *fcc;
/*
 * Convert an FCC time into a system time.
 */
{
	struct tm t;
#if defined(SVR4) || defined(SYSV)
        char tz[20];
#endif

	t.tm_year = fcc->ds_yymmdd/10000;
	t.tm_mon = (fcc->ds_yymmdd/100) % 100 - 1;
	t.tm_mday = fcc->ds_yymmdd % 100;
	t.tm_hour = fcc->ds_hhmmss/10000;
	t.tm_min = (fcc->ds_hhmmss/100) % 100;
	t.tm_sec = fcc->ds_hhmmss % 100;
#if defined(SVR4) || defined(SYSV)
        strcpy (tz, "TZ=GMT");
        putenv (tz);
        timezone = 0;
        /* altzone = 0; */
        daylight = 0;
        t.tm_wday = t.tm_yday = 0;
        t.tm_isdst = -1;
	return (mktime (&t));
#else
        t.tm_zone = (char *) 0;
	t.tm_wday = t.tm_yday = t.tm_isdst = 0;
	return (timegm (&t));
#endif

}




long
TC_ZtToSys (zt)
const ZebTime *zt;
/*
 * Convert a zeb format time into a basic system format representation.
 */
{
	return (zt->zt_Sec);
}



void
TC_SysToZt (sys, zt)
long sys;
ZebTime *zt;
/*
 * Convert a system time to zeb format.
 */
{
	zt->zt_Sec = sys;
	zt->zt_MicroSec = 0;
}






void
TC_ZtToUI (zt, ui)
const ZebTime *zt;
UItime *ui;
/*
 * Convert a system time to an fcc time.
 */
{
	struct tm *t = gmtime (&zt->zt_Sec);

	ui->ds_yymmdd = t->tm_year*10000 + (t->tm_mon + 1)*100 + t->tm_mday;
	ui->ds_hhmmss = t->tm_hour*10000 + t->tm_min*100 + t->tm_sec;
}


void
TC_UIToZt (ui, zt)
const UItime *ui;
ZebTime *zt;
/*
 * Convert an FCC time into a system time.
 */
{
	struct tm t;
#if defined(SVR4) || defined(SYSV)
	char tz[20];
#endif
	t.tm_year = ui->ds_yymmdd/10000;
	t.tm_mon = (ui->ds_yymmdd/100) % 100 - 1;
	t.tm_mday = ui->ds_yymmdd % 100;
	t.tm_hour = ui->ds_hhmmss/10000;
	t.tm_min = (ui->ds_hhmmss/100) % 100;
	t.tm_sec = ui->ds_hhmmss % 100;
	zt->zt_MicroSec = 0;
#if defined(SVR4) || defined(SYSV)
	strcpy (tz, "TZ=GMT");
        putenv (tz);
        timezone = 0;
        /* altzone = 0; */
        daylight = 0;
        t.tm_wday = t.tm_yday = 0;
        t.tm_isdst = -1;
	zt->zt_Sec = mktime (&t);
#else
        t.tm_zone = (char *) 0;
        t.tm_wday = t.tm_yday = t.tm_isdst = 0;
	zt->zt_Sec = timegm (&t);
#endif

}





void
TC_EncodeTime (zt, format, dest)
const ZebTime *zt;
TimePrintFormat format;
char *dest;
/*
 * Encode this date/time value.
 */
{
	struct tm *t = gmtime (&zt->zt_Sec);
/*
 * Just switch out depending on what they want.
 */
	switch (format)
	{
	   case TC_Full:	/* Everything */
		sprintf (dest, "%d-%s-%d,%d:%02d:%02d", t->tm_mday,
			Months[t->tm_mon], t->tm_year, t->tm_hour,
			t->tm_min, t->tm_sec);
		break;

	   case TC_FullUSec:	/* Everything plus the microseconds field */
		sprintf (dest, "%d-%s-%d,%d:%02d:%02d.%06d", t->tm_mday,
			Months[t->tm_mon], t->tm_year, t->tm_hour,
			t->tm_min, t->tm_sec, zt->zt_MicroSec);
		break;

	   case TC_DateOnly:	/* Day of year only */
		sprintf (dest, "%d-%s-%d", t->tm_mday, Months[t->tm_mon],
			t->tm_year);
		break;

	   case TC_TimeOnly:	/* Time of day only */
		sprintf (dest, "%d:%02d:%02d", t->tm_hour, t->tm_min,
				t->tm_sec);
		break;
	}
}




bool
TC_DecodeTime (string, zt)
const char	*string;
ZebTime		*zt;
/*
 * Attempt a simple decoding of the string of the form 
 * "dd-mmm-yy,hh:mm:ss.uuuuuu" into a ZebTime.  Return FALSE
 * if we don't get at least the first three fields.
 */
{
	char	cmonth[3];
	int	nfields, year, month, day, hour, minute;
	float	fsecond;
	struct tm	t;
# if defined(SVR4) || defined(SYSV)
	char	tz[20];
# endif
/*
 * Initialize our pieces and scan the string
 */
	fsecond = 0.0;
	year = month = day = hour = minute = 0;

	nfields = sscanf (string, "%d-%3c-%d,%d:%d:%f", &day, &cmonth, &year,
			  &hour, &minute, &fsecond);
/*
 * If we didn't make it at least to the year, we failed
 */
	if (nfields < 3)
		return (FALSE);
/*
 * See if we got a reasonable month
 */
	cmonth[0] = toupper (cmonth[0]);
	cmonth[1] = tolower (cmonth[1]);
	cmonth[2] = tolower (cmonth[2]);

	for (month = 0; month < 12; month++)
		if (! strncmp (cmonth, Months[month], 3))
			break;

	if (month == 12)
		return (FALSE);
/*
 * Trust the rest and use it to build a "struct tm", that we then convert
 * into seconds.
 */
	t.tm_year = year;
	t.tm_mon = month;
	t.tm_mday = day;
	t.tm_hour = hour;
	t.tm_min = minute;
	t.tm_sec = (int) fsecond;
	zt->zt_MicroSec = (fsecond - (int)fsecond) * 1000000;
#if defined(SVR4) || defined(SYSV)
	strcpy (tz, "TZ=GMT");
        putenv (tz);
        timezone = 0;
        /* altzone = 0; */
        daylight = 0;
        t.tm_wday = t.tm_yday = 0;
        t.tm_isdst = -1;
	zt->zt_Sec = mktime (&t);
#else
        t.tm_zone = (char *) 0;
        t.tm_wday = t.tm_yday = t.tm_isdst = 0;
	zt->zt_Sec = timegm (&t);
#endif

	return (TRUE);
}




void
TC_ZtSplit (zt, year, month, day, hour, minute, second, microsec)
const ZebTime *zt;
int *year, *month, *day, *hour, *minute, *second, *microsec;
/*
 * Split a zeb time into useful chunks.  Only stores into pieces
 * which are non-NULL.
 */
{
	struct tm *t = gmtime (&zt->zt_Sec);

	if (year)	*year = t->tm_year;
	if (month)	*month = t->tm_mon+1;
	if (day)	*day = t->tm_mday;
	if (hour)	*hour = t->tm_hour;
	if (minute)	*minute = t->tm_min;
	if (second)	*second = t->tm_sec;
	if (microsec)	*microsec = zt->zt_MicroSec;
}




void
TC_ZtAssemble (zt, year, month, day, hour, minute, second, microsec)
ZebTime *zt;
int year, month, day, hour, minute, second, microsec;
/*
 * Put together a zeb time out of these constituents.
 */
{
	struct tm t;
#if defined(SVR4) || defined(SYSV)
	char tz[20];
#endif

	t.tm_year = year;
	t.tm_mon = month-1;
	t.tm_mday = day;
	t.tm_hour = hour;
	t.tm_min = minute;
	t.tm_sec = second;
	zt->zt_MicroSec = 0;
#if defined(SVR4) || defined(SYSV)
	strcpy (tz, "TZ=GMT");
        putenv (tz);
        timezone = 0;
        /* altzone = 0; */
        daylight = 0;
        t.tm_wday = t.tm_yday = 0;
        t.tm_isdst = -1;
	zt->zt_Sec = mktime (&t);
#else
        t.tm_zone = (char *) 0;
        t.tm_wday = t.tm_yday = t.tm_isdst = 0;
	zt->zt_Sec = timegm (&t);
#endif

}
