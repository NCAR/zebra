/*************************************************************************
 * TIME_DATE.C: Routines to do time/date convertions.
 *		Note. Use Full year, i.e. 1991, not '91. (Unix time routines
 *		often leave off the '19'.) Watch out!
 *		Jan is month 1.
 *
 */

	/* This structure is used in the convert to/from Unix time routines */
typedef struct {
	long    year,month,day,hour,min,sec;
	long    unix_time;
} date_time_t;

/*************************************************************************
 *	CONVERT_TO_UNIX_TIME:  Take the separate fields in a date_time_t struct
 * 	and calculate the unix time. Pass in a pointer to a date_time_t.
 *	Returns the unix time and places it in the struct too.
 */

convert_to_unix_time(date)
	date_time_t *date;
{
	long	u_day,day,days;
	long	u_time;

	u_day = julian_date(1,1,1970);
	day = julian_date(date->day,date->month,date->year);

	days = day - u_day;

	u_time = (days * 86400) + (date->hour * 3600) + (date->min * 60) + date->sec;

	date->unix_time = u_time;

	return u_time;
}

/*************************************************************************
 * CONVERT_FROM_UNIX_TIME: Take the unix time in the date_time_t and
 *	calculate the separate year,month,day,hour,min,sec fields.
 *	Return values are in the struct.
 */

convert_from_unix_time(utime,date)
	long	utime;
	date_time_t	*date;
{
	long	u_day,day;

	u_day = julian_date(1,1,1970);

	day = (utime / 86400);

	calandar_date((u_day + day),&date->day,&date->month,&date->year);

	day = (utime % 86400);
	date->hour = day / 3600;
	date->min = (day / 60) - (date->hour * 60);
	date->sec = day % 60;
	date->unix_time = utime;

	return 0;
}
 
/*************************************************************************
 *	JULIAN_DATE: Calc the Julian calandar Day Number
 *	As Taken from Computer Language- Dec 1990, pg 58
 */

julian_date(day,month,year)
	int	day,month,year;
{
	int	a,b;
	double	yr_corr;

	/* correct for negative year */
	yr_corr = (year > 0? 0.0: 0.75);
	if(month <=2) {
		year--;
		month += 12;
	}
	b=0;

	/* Cope with Gregorian Calandar reform */
	if(year * 10000.0 + month * 100.0 + day >= 15821015.0) {
		a = year / 100;
		b = 2 - a + a / 4;
	}
	
	return ((365.25 * year - yr_corr) + (long) (30.6001 * (month +1)) + day + 1720994L + b);
}

/*************************************************************************
 *	CALANDAR_DATE: Calc the calandar Day from the Julian date
 *	As Taken from Computer Language- Dec 1990, pg 58
 *	Sets day,month,year as return values.
 */

calandar_date(jdate,day,month,year)
	int	jdate;
	int	*day,*month,*year;
{
	long	a,b,c,d,e,z,alpha;

	z = jdate +1;

	/* Gregorian reform correction */
	if (z < 2299161) { 
		a = z; 
	} else {
		alpha = (long) ((z - 1867216.25) / 36524.25);
		a = z + 1 + alpha - alpha / 4;
	}

	b = a + 1524;
	c = (long) ((b - 122.1) / 365.25);
	d = (long) (365.25 * c);
	e = (long) ((b - d) / 30.6001);
	*day = (int) b - d - (long) (30.6001 * e);
	*month = (int) (e < 13.5)? e - 1 : e - 13;
	*year = (int) (*month > 2.5)? (c - 4716) : c - 4715;

	 return 0;
}
