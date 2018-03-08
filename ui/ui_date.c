/*
 * Date handling routines.
 */
# include "ui_param.h"
# include "ui_date.h"


/*
 * Static information of use.
 */
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


/*
 * Prototypes
 */
void ud_dayn_to_date (long daynum, long year, long *month, long *day);



void
ud_y2k_date (struct date_st *d)
/*
 * Rationalize a date structure.
 */
{
	int y = d->ds_yymmdd/10000;
	
	if (y <= 0)
		return; /* Relative date */
	if (y < 50)
		d->ds_yymmdd += 20000000;
	else if (y < 100)
		d->ds_yymmdd += 19000000;
}



char *
ud_format_date (buffer, dte, opts)
char *buffer;
struct date_st *dte;
int opts;
/*
 * Format a date.
 * Entry:
 *	BUFFER	is the string buffer into which to format the date.
 *	DTE	is the actual date.
 *	OPTS	describes how to do the format.
 * Exit:
 *	The date has been formatted into BUFFER.
 */
{
	ud_y2k_date (dte);
	switch (opts)
	{
	   case UDF_FULL:
		if (dte->ds_yymmdd > 10000)
			sprintf (buffer, "%2d-%s-%4d,%02d:%02d:%02d",
				dte->ds_yymmdd % 100,
				Month_names[(dte->ds_yymmdd/100) % 100],
				dte->ds_yymmdd/10000,
				dte->ds_hhmmss/10000,
				(dte->ds_hhmmss/100) % 100,
				dte->ds_hhmmss % 100);
		else
			sprintf (buffer, "%d,%02d:%02d:%02d", dte->ds_yymmdd,
				dte->ds_hhmmss/10000,
				(dte->ds_hhmmss/100) % 100,
				dte->ds_hhmmss % 100);
		break;
	   default:
	   	ui_error ("(BUG) Bad date opt: %d", opts);
	}
	return (buffer);
}



void
ud_add_date (d1, d2, result)
date *d1, *d2, *result;
/*
 * Add these two dates, giving the result.
 */
{
	long sec1, sec2, day1 = 0, day2 = 0, month, year, day;
	bool leap;
/*
 * At least one of our date values needs to be an offset, or we have trouble.
 */
	ud_y2k_date (d1);
	ud_y2k_date (d2);
	if (d1->ds_yymmdd > 10000 && d2->ds_yymmdd > 10000)
		ui_error ("Attempt to add two ABSOLUTE dates");
/*
 * Convert everything into seconds and days.
 */
 	sec1 = (d1->ds_hhmmss/10000)*3600 + ((d1->ds_hhmmss/100) % 100)*60 +
		d1->ds_hhmmss % 100;
 	sec2 = (d2->ds_hhmmss/10000)*3600 + ((d2->ds_hhmmss/100) % 100)*60 +
		d2->ds_hhmmss % 100;
	day1 = ud_day_of_year (d1->ds_yymmdd);
	day2 = ud_day_of_year (d2->ds_yymmdd);
/*
 * Get a year number -- we will want it.  If the year number ends up being
 * zero, it means we are adding two date offsets.
 */
 	year = d1->ds_yymmdd/10000;
	if (year == 0)
		year = d2->ds_yymmdd/10000;
	leap = (year % 4) == 0;  /* Screws up in 2100 */
/*
 * Time to add up the seconds, and carry over the day if necessary.
 */
 	sec1 += sec2;
	if (sec1 >= 24*60*60)
	{
		day1++;
		sec1 -= 24*60*60;
	}
	result->ds_hhmmss = (sec1/3600)*10000 + (sec1/60 % 60)*100 + sec1 % 60;
/*
 * Add up the days and see what we get.  If we are dealing in only offset
 * dates, we are done.
 */
	day1 += day2;
	if (year == 0)
	{
		result->ds_yymmdd = day1;
		return;
	}
/*
 * OK, now we need to reconstruct the yymmdd portion of the date.  The first
 * thing to do is to deal with any wrap in the year portion.
 */
	if ((leap && day1 > 366) || (! leap && day1 > 365))
	{
		year++;
		day1 -= leap ? 366 : 365;
		leap = (year % 4) == 0;
	}
/*
 * Finally, put the date back together.
 */
 	ud_dayn_to_date (day1, year, &month, &day);
	result->ds_yymmdd = year*10000 + month*100 + day;
}






int
ud_day_of_year (dte)
long dte;
/*
 * Return the day of year from this date.
 */
{
	long month, m, days = 0;
/*
 * If this is an offset date, life is easy.
 */
 	if (dte < 10000)
		return (dte);
/*
 * Otherwise we have to work.
 */
 	month = (dte/100) % 100;
	for (m = 1; m < month; m++)
		days += Month_days[m];
	days += dte % 100;	/* Days for the last month. */
	if (((dte/10000) % 4) == 0 && month > 2)
		days++;		/* Leap year */
	return (days);
}
	
	
	
void	
ud_dayn_to_date (long daynum, long year, long *month, long *day)
/*
 * Convert the day number into a month/day pair.  YEAR is needed for the
 * leap year case.
 */
{
/*
 * Figure out what our month is.
 */
	for (*month = 1; daynum > Month_days[*month] && *month <= 12; 
		(*month)++)
		daynum -= Month_days[*month];
/*
 * Do leap year checking.  There is a small chance that we went too many
 * months.
 */
 	if ((year % 4) == 0 && *month > 2)
	{
		if (daynum == 1)
		{
			(*month)--;
			daynum = (*month == 2) ? 29 : Month_days[*month];
		}
		else
			daynum--;
	}
/*
 * All done.
 */
 	*day = daynum;
}




void
ud_sub_date (d1, d2, result)
date *d1, *d2, *result;
/*
 * Subtract these two dates, giving the result.
 */
{
	long sec1, sec2, day1 = 0, day2 = 0, month, year, day, year2;
	bool leap;

	ud_y2k_date (d1);
	ud_y2k_date (d2);
/*
 * Convert everything into seconds and days.
 */
 	sec1 = (d1->ds_hhmmss/10000)*3600 + ((d1->ds_hhmmss/100) % 100)*60 +
		d1->ds_hhmmss % 100;
 	sec2 = (d2->ds_hhmmss/10000)*3600 + ((d2->ds_hhmmss/100) % 100)*60 +
		d2->ds_hhmmss % 100;
	day1 = ud_day_of_year (d1->ds_yymmdd);
	day2 = ud_day_of_year (d2->ds_yymmdd);
/*
 * Get a year number -- we will want it.  If the year number ends up being
 * zero, it means we are adding two date offsets.
 *
 * 2/88 jc	The logic has been changed enough to support the subtraction
 *		of dates that cross the year-end boundary.  It seems to
 *		work, as long as the higher-year date is before leap day
 *		on leap years...differences of over a year are still
 *		bad news.
 */
 	year = d1->ds_yymmdd/10000;
	if (year == 0)
		year = d2->ds_yymmdd/10000;
	else
	{
		year2 = d2->ds_yymmdd/10000;
		if (year2 != 0)
		{
			if (year > year2)
				day1 += ((year2 % 4) == 0) ? 366 : 365;
			else if (year2 > year)
				day2 += ((year % 4) == 0) ? 366 : 365;
			year = year2 = 0;
		}
	}
/*
 * Time to subtract the minutes, and borrow the day if necessary.
 */
	sec1 -= sec2;
	if (sec1 < 0)
	{
		day1--;
		sec1 += 24*60*60;
	}
	result->ds_hhmmss = (sec1/3600)*10000 + ((sec1/60) % 60)*100 +
		sec1 % 60;
/*
 * Subtract the days and see what we get.  If we are dealing in only offset
 * dates, we are done.
 */
	day1 -= day2;
	if (year == 0)
	{
		result->ds_yymmdd = day1;
		return;
	}
/*
 * OK, now we need to reconstruct the yymmdd portion of the date.  The first
 * thing to do is to deal with any wrap in the year portion.
 */
	if (day1 <= 0)
	{
		year--;
		leap = (year % 4) == 0;
		day1 += leap ? 366 : 365;
	}
/*
 * Finally, put the date back together.
 */
 	ud_dayn_to_date (day1, year, &month, &day);
	result->ds_yymmdd = year*10000 + month*100 + day;
}

