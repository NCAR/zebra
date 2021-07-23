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
# include <time.h>
# include <ctype.h>
# include <sys/types.h>
# include <string.h>
# include <unistd.h>
# include <limits.h>

# include "defs.h"

RCSID ("$Id: TCvt.c,v 2.28 2000-01-24 18:31:05 burghart Exp $")

/*
 * Public time constants
 */
const ZebTime ZT_ALPHA = { LONG_MIN, 0 };
const ZebTime ZT_OMEGA = { LONG_MAX, 999999 };
const ZebTime ZT_NONE = { -1, -1 };

static char *TC_GMTZONE = "TZ=GMT";

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
const long sys;
UItime *fcc;
/*
 * Convert a system time to an fcc time.
 */
{
	struct tm *t = gmtime ((time_t *)&sys);

	fcc->ds_yymmdd = (t->tm_year + 1900)*10000 +
		(t->tm_mon + 1)*100 + t->tm_mday;
	fcc->ds_hhmmss = t->tm_hour*10000 + t->tm_min*100 + t->tm_sec;
}





long
TC_FccToSys (fcc)
const UItime *fcc;
/*
 * Convert an FCC time into a system time.
 */
{
	struct tm t;
    /*
     * Get t.tm_year, defined as years since 1900
     */
	if ((t.tm_year = fcc->ds_yymmdd/10000) > 1000)
	    t.tm_year -= 1900;
	else if (t.tm_year < 70) /* kluge for a >= 2000 2-digit year */
	    t.tm_year += 100;
    /*
     * And get the rest
     */
	t.tm_mon = (fcc->ds_yymmdd/100) % 100 - 1;
	t.tm_mday = fcc->ds_yymmdd % 100;
	t.tm_hour = fcc->ds_hhmmss/10000;
	t.tm_min = (fcc->ds_hhmmss/100) % 100;
	t.tm_sec = fcc->ds_hhmmss % 100;
        putenv (TC_GMTZONE);
        t.tm_wday = t.tm_yday = 0;
        t.tm_isdst = -1;
	return (mktime (&t));
}




long
TC_ZtToSys (zt)
const ZebTime *zt;
/*
 * Convert a zebra format time into a basic system format representation.
 * Includes microseconds and rounds to the nearest second.
 */
{
	return (zt->zt_Sec);
}



void
TC_SysToZt (sys, zt)
const long sys;
ZebTime *zt;
/*
 * Convert a system time to zebra format.
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
	time_t syst = TC_ZtToSys (zt);
	struct tm *t = gmtime (&syst);

	ui->ds_yymmdd = (t->tm_year + 1900)*10000 +
		(t->tm_mon + 1)*100 + t->tm_mday;
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
	if ((t.tm_year = ui->ds_yymmdd/10000) > 1000)
		t.tm_year -= 1900;
	t.tm_mon = (ui->ds_yymmdd/100) % 100 - 1;
	t.tm_mday = ui->ds_yymmdd % 100;
	t.tm_hour = ui->ds_hhmmss/10000;
	t.tm_min = (ui->ds_hhmmss/100) % 100;
	t.tm_sec = ui->ds_hhmmss % 100;
	zt->zt_MicroSec = 0;
        putenv (TC_GMTZONE);
        t.tm_wday = t.tm_yday = 0;
        t.tm_isdst = -1;
	zt->zt_Sec = mktime (&t);
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
	struct tm *t = gmtime ((time_t *)&zt->zt_Sec);
/*
 * Just switch out depending on what they want.
 */
	switch (format)
	{
	   case TC_Full:	/* Everything */
		sprintf (dest, "%d-%s-%d,%d:%02d:%02d", t->tm_mday,
			Months[t->tm_mon], t->tm_year + 1900, t->tm_hour,
			t->tm_min, t->tm_sec);
		break;

	   case TC_FullUSec:	/* Everything plus the microseconds field */
		sprintf (dest, "%d-%s-%d,%d:%02d:%02d.%06li", t->tm_mday,
			Months[t->tm_mon], t->tm_year + 1900, t->tm_hour,
			t->tm_min, t->tm_sec, zt->zt_MicroSec);
		break;

	   case TC_DateOnly:	/* Day of year only */
		sprintf (dest, "%d-%s-%d", t->tm_mday, Months[t->tm_mon],
			t->tm_year + 1900);
		break;

	   case TC_TimeOnly:	/* Time of day only */
		sprintf (dest, "%d:%02d:%02d", t->tm_hour, t->tm_min,
				t->tm_sec);
		break;
	}
}



const char *
TC_AscTime (zt, format)
const ZebTime *zt;
TimePrintFormat format;
/*
 * Encodes the time into a string according to format and returns a pointer
 * to the string.  Like the library asctime() function but without that
 * annoying trailing newline.  The string is only valid until the next call.
 */
{
	static char dest[128];

	TC_EncodeTime (zt, format, dest);
	return (dest);
}



zbool
TC_DecodeTime (string, zt)
const char	*string;
ZebTime		*zt;
/*
 * Attempt a simple decoding of the string of the form 
 * "dd-mmm-{yy|yyyy},hh:mm:ss.uuuuuu" into a ZebTime.  Return FALSE
 * if we don't get at least the first three fields.  Either 2- or
 * 4-digit years are supported because TC_ZtAssemble accepts
 * either one.
 */
{
	char	cmonth[3];
	int	nfields, year, month, day, hour, minute;
	float	fsecond;
	int second, microsec;
/*
 * Initialize our pieces and scan the string
 */
	fsecond = 0.0;
	year = month = day = hour = minute = 0;

	nfields = sscanf (string, "%d-%3c-%d,%d:%d:%f", &day, cmonth, &year,
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
 * Trust the rest and use it to assemble a ZebTime structure.
 */
	second = (int)fsecond;
	/* round to the nearest microsecond */
	microsec = (int)(((fsecond - second) * 1e+6) + 0.5);
	++month;	/* ZtAssemble expects 1..12 */
	TC_ZtAssemble (zt, year, month, day, hour, minute, second, microsec);
	return (TRUE);
}




void
TC_ZtSplit (zt, year, month, day, hour, minute, second, microsec)
const ZebTime *zt;
int *year, *month, *day, *hour, *minute, *second, *microsec;
/*
 * Split a zebra time into useful chunks.  Only stores into pieces
 * which are non-NULL.
 */
{
	struct tm *t = gmtime ((time_t *)&zt->zt_Sec);

	if (year)	*year = t->tm_year + 1900;
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
 * Put together a zebra time out of these constituents.  For the tm
 * structure, the year field is the two- or three-digit value (year - 1900).
 * Make the sure the year we're passed is converted to this convention.
 * It seems safe to assume that any year less than 70 is a two-digit
 * year which was intended for the 21st century.
 */
{
	struct tm t;

	if (year < 70)
		year += 100;
	if (year >= 1900)
		year -= 1900;
	t.tm_year = year;
	t.tm_mon = month-1;
	t.tm_mday = day;
	t.tm_hour = hour;
	t.tm_min = minute;
	t.tm_sec = second;
	zt->zt_MicroSec = microsec;
        putenv (TC_GMTZONE);
        t.tm_wday = t.tm_yday = 0;
        t.tm_isdst = -1;
	zt->zt_Sec = mktime (&t);
}




void TC_y2k (UItime *d)
/*
 * Rationalize this date.
 */
{
	int y = d->ds_yymmdd/10000;
	
	if (y <= 0)
		return; /* Relative date */
	if (y < 10)
		d->ds_yymmdd += 20000000;
	else if (y < 100)
		d->ds_yymmdd += 19000000;
}
