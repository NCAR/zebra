/* 11/84 jc */
/*
 * Various utilities.
 *
 * 6/2/87 cb	Routines which had been pmu_get_* were renamed to pmu_g_*
 *		so they would work on the CRAY
 *
 * 10/9/98 jc	(Only 11 years after that last comment...) y2k stuff.
 *		Also hacked out VMS and Cray stuff, don't think we'll need it.
 *
 */

# include <string.h>
# include <stdio.h>
# include <ctype.h>
# define TRUE 1
# define FALSE 0

# include <time.h>

/*
 * Table for conversion from time zones to offsets from greenwich.
 */
struct tofftab
{
	char	*to_tzone;		/* Time zone name	*/
	int	to_offset;		/* The corresponding offset */
} T_offset[] =
{
	{	"MST",		7	},
	{	"MDT",		6	},
	{	"CST",		6	},
	{	"CDT",		5	},
	{	"EST",		5	},
	{	"EDT",		4	},
	{	"PST",		8	},
	{	"PDT",		7	},
	{	"GMT",		0	},
	{	0,		0	}
};

/*
 * Prototypes
 */
void pmu_g_yesterday (int *date);
void pmu_check_end (char *string);
void pmu_split_date (char *string, char *dest, char **date, char **time);





char *
pmu_strend (string)
char *string;
/*
 * Return a pointer to the byte past the end of the given string.
 */
{
	while (*string++) ;
	return (string);
}





static char *Month_names[] =
{
	0,
	"jan", "feb", "mar", "apr", "may", "jun",
	"jul", "aug", "sep", "oct", "nov", "dec"
};

static int Month_days[] =
{
	0, 31, 28, 31, 30, 31, 30,
	   31, 31, 30, 31, 30, 31
};





void
pmu_encode_date (date, time, string)
int date, time;
char *string;
/*
 * Take the numeric date/time, and encode it into the given string.
 */
{
	int year;
	if ((year = date/10000) < 1000)
		year += 1900;
	sprintf (string, "%2d-%s-%4d %2d:%02d:%02d (GMT)", date % 100,
		Month_names[(date/100) % 100], year,
		time/10000, (time/100) % 100, time % 100);
}




int
pmu_parse_time (string, date, time)
char *string;
int *time, *date;
/*
 * Parse a date/time string into numerical form.
 * Entry:
 *	String 	is the time string.
 * Exit:
 *	Date	contains the date, as YYMMDD.
 *	Time	contains the time, as HHMMSS.
 *	The return value is TRUE iff the date/time was successfully parsed.
 */
{
	char dstr[128], *ds, *ts;
/*
 * Split the string up into date/time.
 */
	pmu_split_date (string, dstr, &ds, &ts);
/*
 * Make sure we got something.
 */
	if (! *ds)
	{
		ui_error ("Null date/time");
		return (FALSE);
	}
	*date = *time = 0;
/*
 * See which time syntax is being used.
 *
 * today hh:mm:ss
 */
	if (! strncmp (ds, "today", strlen (ds)))
	{
		pmu_g_today (date);
		return (pmu_g_time (ts, time));
	}
/*
 * yesterday hh:mm:ss
 */
	else if (! strncmp (ds, "yesterday", strlen (ds)))
	{
		pmu_g_yesterday (date);
		return (pmu_g_time (ts, time));
	}
/*
 * now.
 */
	else if (! strcmp (ds, "now"))
		return (pmu_g_now (date, time));
/*
 * -hh:mm:ss.
 */
	else if (*ds == '-')
		return (pmu_g_minus (ds, date, time));
/*
 * dd-mmm-yy hh:mm:ss
 */
	else
	{
		if (pmu_g_date (ds, date))
			return (pmu_g_time (ts, time));
		return (FALSE);
	}
}




int
pmu_g_date (ds, date)
char *ds;
int *date;
/*
 * Parse the date string for a date.
 */
{
	int day, month, year;
	char *cp;

/*
 * Pull out the day number from the date.
 */
	day = 0;
	while (*ds && isdigit (*ds))
		day = (day*10) + *ds++ - '0';
	if (! day || day > 31)
	{
		ui_error ("Bad day number: %d", day);
		return (FALSE);
	}
	if (*ds != '-')
	{
		ui_error ("date format is DD-MMM-YY");
		return (FALSE);
	}
/*
 * Now find the end of the month name.
 */
	ds++;
	for (cp = ds; *cp && *cp != '-'; cp++) ;
	if (*cp != '-')
	{
		ui_error ("date format is DD-MMM-YY");
		return (FALSE);
	}
/*
 * Try to recognize the date name.
 */
	*cp = 0;	/* Zap - */
	for (month = 1; month <= 12; month++)
		if (! strcmp (ds, Month_names[month]))
			break;
	if (month > 12)
	{
		ui_error ("Bad month name: %s", ds);
		return (FALSE);
	}
/*
 * Decode the year.
 */
	*cp++ = '-';	/* Put the - back -- somebody might miss it. */
	year = 0;
	while (*cp && isdigit (*cp))
		year = year*10 + *cp++ - '0';
# ifdef WHY_DID_I_EVER_THINK_THIS_WAS_A_GOOD_IDEA
	if (year > 1900)
		year -= 1900;
	if (year < 70 || year > 99)
	{
		ui_error ("Bad year number: %d", year);
		return (FALSE);
	}
# endif
	if (year < 10)	/* Early 2000's? */
		year += 2000;
	else if (year < 100) /* Late 1900's */
		year += 1900;
/*
 * Wow!  We actually made it through all that.  Return the date.
 */
	*date = year*10000 + month*100 + day;
	return (TRUE);
}



int
pmu_g_time (ts, time)
char *ts;
int *time;
/*
 * Parse the given time string.
 */
{
	int hour, minute, second;
/*
 * We allow the time portion to be null...
 */
	*time = 0;
	if (! ts || ! *ts)
		return (TRUE);
	if (! isdigit (*ts))
	{
		ui_error ("Time format is HH:MM:SS");
		return (FALSE);
	}
/*
 * Pick off the hours.
 */
	hour = 0;
	while (*ts && isdigit (*ts))
		hour = hour*10 + *ts++ - '0';
	if (hour > 24) /* 3/13/85 jc 24 to allow -24:00:00 (kludge) */
	{
		ui_error ("bad hour number: %d", hour);
		return (FALSE);
	}
/*
 * If this is the end of the string, we can quit.  If there is something,
 * it had better be a colon.
 */
	if (! *ts)
	{
		*time = hour*10000;
		return (TRUE);
	}
	if (*ts++ != ':' || ! isdigit (*ts))
	{
		ui_error ("Time format is HH:MM:SS");
		return (FALSE);
	}
/*
 * Pick off the minute value.
 */
	minute = 0;
	while (*ts && isdigit (*ts))
		minute = minute*10 + *ts++ - '0';
	if (minute > 59)
	{
		ui_error ("Bad minute number: %d", minute);
		return (FALSE);
	}
/*
 * If this is the end of the string, we can quit.  If there is something,
 * it had better be a colon.
 */
	if (! *ts)
	{
		*time = hour*10000 + minute*100;
		return (TRUE);
	}
	if (*ts++ != ':' || ! isdigit (*ts))
	{
		ui_error ("Time format is HH:MM:SS");
		return (FALSE);
	}
/*
 * Finally, pick off the seconds value.
 */
	second = 0;
	while (*ts && isdigit (*ts))
		second = second*10 + *ts++ - '0';
	if (second > 59)
	{
		ui_error ("Bad second number: %d", second);
		return (FALSE);
	}
	pmu_check_end (ts);
	*time = hour*10000 + minute*100 + second;
	return (TRUE);
}





int
pmu_g_today (date)
int *date;
/*
 * The the day/month/year values for today, in greenwich.
 */
{
/*
 * Get today's date, here.
 * jc y2k: tm_year is defined as "number of years since 1900".
 */
	time_t clk = time ((long) 0);
	struct tm *t = gmtime (&clk);
	*date = (t->tm_year + 1900)*10000 + (t->tm_mon + 1)*100 + t->tm_mday;
	return (1);
}




void
pmu_g_yesterday (date)
int *date;
/*
 * Get the numbers for yesterday.
 */
{
	int d, m, y;
/*
 * Start by getting the numbers for today.
 */
	pmu_g_today (date);
	y = *date/10000; 
	m = (*date/100) % 100;
	d = *date % 100;
/*
 * Now subtract one day.
 */
	if (--d <= 0)
	{
	/*
	 * We backed up into the previous month.
	 */
		if (--m <= 0)
		{
		/*
		 * Back into the previous year.
		 */
			m = 12;
			y--;
		}
	/*
	 * Figure the new day number.
	 */
		if ((m == 2) && (y % 4) == 0)
			d = 29;
		else
			d = Month_days[m];
	}
/*
 * Put the date back together.
 */
	*date = y*10000 + m*100 + d;
}




void
pmu_check_end (string)
char *string;
/*
 * Make sure we are at the end of the command, and issue a warning if
 * we are not.
 */
{
	char *s = string;

	while (*s)
		if (*s != ' ' && *s != '\t' && *s != '\n')
		{
			warning ("Junk past end of legal command: '%s'",
				 string);
			return;
		}
		else
			s++;
}





int
pmu_dadd (date, time, incr)
int *date, *time, incr;
/*
 * Add INCR (which is a time value) to the given date/time.
 */
{
	int year, month, day, hour, minute, second, ih, im, is, carry;
/*
 * Split up the times.
 */
	ih = incr/10000;
	im = (incr/100) % 100;
	is = incr % 100;
	hour = *time/10000;
	minute = (*time/100) % 100;
	second = *time % 100;
/* 
 * Do the addition.
 */
	if ((second += is) > 59)
	{
		second -= 60;
		carry = 1;
	}
	else
		carry = 0;
	if ((minute += im + carry) > 59)
	{
		minute -= 60;
		carry = 1;
	}
	else
		carry = 0;
	if ((hour += ih + carry) > 23)
	{
		hour -= 24;
		carry = 1;
	}
	else
		carry = 0;
	*time = hour*10000 + minute*100 + second;
	if (! carry)
		return (1);
/*
 * We spilled over to the next day.
 */
	year = *date/10000;
	month = (*date/100) % 100;
	day = (*date % 100) + 1;
	if (month == 2 && day == 29 && (year % 4) == 0)
		;
	else if (day > Month_days[month])
	{
		day = 1;
		if (++month > 12)
		{
			month = 1;
			year++;
		}
	}
	*date = year*10000 + month*100 + day;

	return (1);
}





int
pmu_dsub (date, time, incr)
int *date, *time, incr;
/*
 * Subtract INCR (which is a time value) from the given date/time.
 */
{
	int year, month, day, hour, minute, second, ih, im, is, borrow;
/*
 * Split up the times.
 */
	ih = incr/10000;
	im = (incr/100) % 100;
	is = incr % 100;
	hour = *time/10000;
	minute = (*time/100) % 100;
	second = *time % 100;
/* 
 * Do the subtraction.
 */
	if ((second -= is) < 0)
	{
		second += 60;
		borrow = 1;
	}
	else
		borrow = 0;
	if ((minute -= im + borrow) < 0)
	{
		minute += 60;
		borrow = 1;
	}
	else
		borrow = 0;
	if ((hour -= ih + borrow) < 0)
	{
		hour += 24;
		borrow = 1;
	}
	else
		borrow = 0;
	*time = hour*10000 + minute*100 + second;
	if (! borrow)
		return (1);
/*
 * We spilled over to the next day.
 */
	year = *date/10000;
	month = (*date/100) % 100;
	day = (*date % 100) - 1;
	if (day <= 0)
	{
		if (--month <= 0)
		{
			month = 12;
			year--;
			day = 31;
		}
		else if (month == 2 && (year % 4) == 0)
			day = 29;
		else
			day = Month_days[month];
	}
	*date = year*10000 + month*100 + day;

	return (1);
}






int
pmu_mindif (d1, t1, d2, t2)
int d1, t1, d2, t2;
/*
 * Return the time difference between d1/t1 and d2/t2, in minutes.
 */
{
	int y1, y2, mo1, mo2, h1, h2, m1, m2, nd1, nd2, min1, min2;

/*
 * Split the date/time values up into their respective components.
 */
	y1  = d1/10000;		y2  = d2/10000;
	mo1 = (d1/100) % 100;	mo2 = (d2/100) % 100;
	d1 %= 100;		d2 %= 100;
	h1  = t1/10000;		h2  = t2/10000;
	m1  = (t1/100) % 100;	m2  = (t2/100) % 100;
/*
 * Find out how many days into the year each day is.
 */
	mo1--;
	for (nd1 = d1; mo1 > 0; mo1--)
	{
		if (mo1 == 2 && (y1 % 4) == 0)
			nd1 += 29;
		else
			nd1 += Month_days[mo1];
	}
	mo2--;
	for (nd2 = d2; mo2 > 0; mo2--)
	{
		if (mo2 == 2 && (y1 % 4) == 0)
			nd2 += 29;
		else
			nd2 += Month_days[mo2];
	}
/*
 * Now find out how many minutes into the day each date is.
 */
	min1 = h1*60 + m1;
	min2 = h2*60 + m2;
/*
 * Return the result.
 */
	return ((nd2 - nd1)*1440 + (min2 - min1));
}





int
pmu_g_gmt_offset ()
/*
 * Return the number of hours to add to a local time to get GMT.
 */
{
	char *tz, *getenv ();
	struct tofftab *tp = T_offset;
/*
 * See if the logical name PAM_TIMEZONE is defined.
 */
	if ((tz = getenv ("PAM_TIMEZONE")) == 0)
		tz = "MST";	/* Nope, assume standard time */
/*
 * Now look at the timezone.
 */
	for (; tp->to_tzone; tp++)
		if (! strcmp (tz, tp->to_tzone))
			return (tp->to_offset);
	warning ("Unknown time zone: '%s'", tz);
	return (7);
}




int
pmu_g_now (date, rtime)
int *date, *rtime;
/*
 * Return today's date/time, in greenwich.
 */
{
/*
 * Get today's date, here.
 */
	time_t clk = time ((long) 0);
	struct tm *t = gmtime (&clk);
	*date = (t->tm_year + 1900)*10000 + (t->tm_mon+1)*100 + t->tm_mday;
	*rtime = t->tm_hour*10000 + t->tm_min*100 + t->tm_sec;
	return (1);
}



int
pmu_g_minus (ds, date, time)
char *ds;
int *date, *time;
/*
 * Parse a delta time, which is always negative, since I haven't gotten
 * around to implementing the "future data" feature yet.
 */
{
	int delta;
/*
 * Figure out the current time.
 */
	pmu_g_now (date, time);
/*
 * Parse out the delta amount.	( the +1 is to skip the minus sign)
 */
	if (! pmu_g_time (ds + 1, &delta))
		return (FALSE);
/*
 * Adjust the current time by the delta.
 */
	pmu_dsub (date, time, delta);
	return (TRUE);
}





void
pmu_split_date (string, dest, date, time)
char *string, *dest, **date, **time;
/*
 * Split the date/time string into its respective components.  Note that this
 * is a very simple routine, simply splitting things at the imbedded blank.
 */
{
	char *blank, *pmu_strend ();
/*
 * Find the blank.
 */
	if ((blank = strchr (string, ' ')) == 0)
	{
	/*
	 * No blank -- assume date only.
	 */
		strcpy (dest, string);
		*date = dest;
		*time = pmu_strend (string) - 1;
	}
	else
	{
	/*
	 * Got a blank -- split the string apart.
	 */
		strncpy (dest, string, blank - string);
		dest[blank - string] = '\0';
		*date = dest;
		*time = dest + (blank - string) + 1;
		while (*blank == ' ')
			blank++;	/* Skip as many blanks as necessary. */
		strcpy (*time, blank);
	}
}
