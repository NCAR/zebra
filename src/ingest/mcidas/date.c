/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 DATE.C 27-Feb-96,13:13:46,`ROBERTM' initial checkin of DMSP nav         */
/* 2 DATE.C 17-Apr-96,14:47:56,`USER' Released                               */
/* 3 DATE.C 27-Sep-96,16:29:34,`ROBERTM' Include mcidasp.h; reorganize;      */
/*      improve documentation style (7018)                                   */
/* 4 DATE.C 22-Oct-96,19:41:00,`USER' Released                               */
/**** McIDAS Revision History *** */

/* date.c
 *
 * Implementation of high precision date and time functions.
 *
 * Change log:
 * 96.02.05	rtm	Move design notes here from m0frame.h
 * 95.10.12	rtm	Change naming conventions to conform
 *			to McIDAS core style. Add documentation
 *			blocks. Many routines not called
 *			elsewhere in generic navigation were
 * 			made private.
 * 95.01.18	rtm	jdateCurrent added. Unit test
 *			in this module repeated successfully
 * 95.01.18	rtm	Added jdateAverage yesterday, and
 *			while doing unit test, uncovered
 *			bug in jdateExtract.
 * 95.01.03	rtm	successful unit test. See jdate.h
 *			for a description
 */

/* Design notes on high-precision dates and times */

/* M0TM INTERCHANGEABLE WITH 'DOUBLE':
 * The use of type 'M0TM' is just for clarity. M0TMs are inter-
 * changeable with 'doubles' in prototypes, etc. (the compiler
 * 'casts' one to the other with no complaints). Aslo, M0TMs
 * can be used directly in mathematical calculations without
 * casting. */


/* PRECISION:
 * Tests with 8-byte 'double' for 'M0TM' have shown that it
 * can go to 48 days with nanosecond precision and 136 YEARS
 * with microsecond precision. 'Precision' was determined by
 * finding the largest power of two, in seconds, for which
 * time+time_inc - time == time_inc for a time_inc of
 * one microsecond and one nanosecond, respectively
 *
 * Another limit on size arises in TMextract where the integral
 * part of a double 'M0TM' (the seconds) is converted to
 * a 'int'. The largest 'int' (4 byte) is 2^31-1, which works out
 * to 68 years. M0DTextract is under no such limitation because
 * the 'M0TM' part is limited to a day or less for a legal M0DT.
 * Still, the more strict 68 year limit has been built into
 * the module as an assertion. 
 *
 * The module has a built-in unit test when compiled with
 * UNIT_TEST defined. The test program has been run with a time
 * interval of 60 years broken into approx. 2000 intervals;
 * summing the intervals together is able to reproduce the
 * interval end time with microsecond precision.
 */

/* JULIAN DAY:
 * The date component of an M0DT type is very closely related to
 * the "Julian Day."
 * This is a very confusing term. It has two meanings that I know
 * of. One is the number of days since the beginning of the year,
 * with 1 January = 1. This I call a 'day number,' but McIDAS
 * often calls it a Julian date. The other definition, used by
 * astronomers, is the number of days since 1 January 4713 BC.
 * That is the definition I use here, with a few 'wrinkles.'
 * First, the astronomical Julian day starts at 12 UTC, not 00 UTC
 * (probably because the European astronomers wanted to have a
 * night's observations all happen on the same 'day.'). Second,
 * my Julian Day algorithms only work after 1584 A.D. So, if
 * you want to navigate any Roman or Babylonian satellites, you're
 * on your own ;).  The latter (4713 BC) form of Julian Date
 * is handy for two reasons; first, they are just about in the
 * form that the Brouwer-Lyddane orbit model wants anyway, and
 * second, they are absolute; you can take the difference between
 * two of them and have the number of days in between (plus one);
 * no worries about months, leap years, etc. */


#include <math.h>
#include <stdlib.h>
#include <time.h>
#include "mcidasp.h"
#include "m0frame.h"


#ifdef M0DEBUG

/*
*| Name:
*|      hmsmValid - Validates components of time field.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|	M0flag
*|      hmsmValid(int hour, int minute, int second, int microsecond)
*|
*| Input:
*|      hour         - Hours component of time field.
*|      minute       - Minutes component of time field.
*|      second       - Seconds component of time field.
*|      microsecond  - Microseconds component of time field.
*|
*| Input and Output:
*|     none
*|
*| Output:
*|     none
*|
*| Return values:
*|      M0FALSE if any time components are out of range, M0TRUE
*|      otherwise
*|
*| Remarks:
*|      This is a PRIVATE routine.
*|
*| Categories: 
*|      navigation
*/

static M0flag
hmsmValid(int hour, int minute, int second, int microsecond)
{
	int	test_d;		/* test value of day		*/
	int	test_h;		/* test value of hour		*/
	int	test_m;		/* test value of minute		*/
	int	test_ms;	/* test value of microsecond	*/
	int	test_s;		/* test value of second		*/

	M0TM	TMtest;		/* time since 00 UTC in seconds	*/

	/* The routine first tests that all components are
	 * positive. It then constructs a M0TM type time and
	 * then re-extracts its components. If any differ, the
	 * input components were invalid */

	if ( hour < 0 || minute < 0 || second < 0
	  || microsecond < 0 ) {
		return M0FALSE;
	}

	TMtest = M0TMmake( 0., (double)hour, (double)minute,
	  (double)second, (double)microsecond );
	M0TMextract(TMtest, &test_d, &test_h, &test_m, &test_s, &test_ms);

	if ( test_d != 0
	  || test_h != hour
	  || test_m != minute
	  || test_s != second
	  || test_ms!= microsecond ) {
		return M0FALSE;
	}
	else {
		return M0TRUE;
	}
}



/*
*| Name:
*|      ymdValid - Validates components of date field.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|	M0flag
*|      hmsmValid(int year, int month, int day)
*|
*| Input:
*|      year     - Year component of date field.
*|      month    - Month component of date field.
*|      day      - Day component of date field.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      M0FALSE if any date components are out of range, M0TRUE
*|      otherwise
*|
*| Remarks: 
*|	    This is a PRIVATE routine. It knows all about leap
*|      years, exceptions at turns of centuries, etc., provided
*|      the input year is 1584 or later.
*|
*| Categories: 
*|      navigation
*/

static M0flag
ymdValid(int year, int month, int day)
{
	int	test_d;		/* test value of day	*/
	int	test_jd;	/* test Julian Date	*/
	int	test_m;		/* test value of month	*/
	int	test_y;		/* test value of year	*/

	test_jd = M0jd_make(year, month, day);
	M0jd_extract(test_jd, &test_y, &test_m, &test_d);
	
	if ( test_y != year ||
	     test_m != month ||
	     test_d != day ) {

		return M0FALSE;
	}
	else {
		return M0TRUE;
	}
}

#endif
/*
*| Name:
*|       M0dabtim - compute 'julian date' for orbital prediction
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      double
*|      M0dabtim(M0DT epoch)
*|
*| Input:
*|      epoch   - high-precision date for orbit model call
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      Julian Date
*|
*| Remarks:
*|	    M0dabtim() computes an epoch date ('Julian Date')
*|      in days and fractions of days from a high-precision
*|      date variable of type M0DT. The Julian Date begins at
*|      12 UTC, not 00 UTC.
*|
*|	    The results of M0dabtim() are of lower precision
*|      than the originating M0DT type; therefore you should
*|      not use them for time calculations where high (micro-
*|      second) precision is needed. Use M0DT and M0TM
*|      variables and the functions in the M0DT and M0TM series
*|      for high-precision calendar and clock manipulations,
*|      as for navigation.
*|
*| Categories: 
*|      navigation
*/

double
M0dabtim(M0DT date)

{
	/* The present implementation of M0DT is with the
	 * day portion stored as the Julian Date, only 
	 * with the time portion beginning at 00 UTC
	 * rather than 12 UTC. So the computation of
	 * 'dabtim' is very simple */

	return ( (double)date.julday
		+ date.seconds/(double)SEC_PER_DAY
		- 0.5 );
}




/*
*| Name:
*|      M0DTavg - Computes the average of two dates.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|	M0DT
*|      M0DTavg(M0DT DTa, M0DT DTb)
*|
*| Input:
*|      DTa - The first date to be averaged.
*|      DTb - The second date to be averaged.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      average of DTa and DTb
*|
*| Remarks:
*|      None.
*|
*| Categories: 
*|      navigation
*/


M0DT
M0DTavg(M0DT DTa, M0DT DTb)
{
	M0DT	DTavg;		/* average date			*/
	M0TM	TMdiff;		/* time difference between dates*/

	TMdiff	= M0DTdiff(DTa, DTb);
	DTavg	= M0DTinc(DTa, TMdiff/2.);

	return DTavg;
}



	
/*
*| Name:
*|      M0DTcurrent - Returns current time in high-precision form.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      M0DT
*|      M0DTcurrent(void)
*|
*| Input:
*|      None
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      current time
*|
*| Remarks:
*|      Current time (UTC) is fetched from the system clock.
*|
*| Categories: 
*|      navigation
*/


M0DT
M0DTcurrent(void)
{
	M0DT		current;/* current absolute time to return */
	time_t		curtm;	/* current system time (local)     */
	time_t		status;	/* time() function return code	   */
	struct tm *	pUTC;	/* pointer to struct tm in static
				 * data; no need to free */

	status	= time ( &curtm );
	M0ASSERT ( status != -1 );
	pUTC	= gmtime ( &curtm );
	M0ASSERT ( pUTC != NULL );

	/* According to C run-time library documentation, the
	 * tm_year contains 'years since 1900.' I am trusting
	 * them to get 2000 and beyond right */

	current = M0DTmake(
		pUTC->tm_year + 1900,
		pUTC->tm_mon  + 1,
		pUTC->tm_mday,
		pUTC->tm_hour,
		pUTC->tm_min,
		pUTC->tm_sec,
		0 );
	return current;
}


/*
*| Name:
*|      M0DTdaynum - Returns day number of high-precision date.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      int
*|      M0DTdaynum(M0DT date)
*|
*| Input:
*|      date    - Input high-precision date.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      The day number.
*|
*| Remarks:
*|      The day number for 1 Jan is 1. Leap years (including special
*|      cases at turns of century) are accounted for.
*|
*| Categories: 
*|      navigation
*/

int
M0DTdaynum(M0DT date)
{
	M0DT	DTnew_year;	/* absolute time of 00 UTC 1 Jan
				 * of year containing 'date'	*/
	int	day;
	int	hour;
	int	microsecond;
	int	minute;
	int	month;
	int	second;
	int	year;

	/* Break 'date' into components, compute absolute time for
	 * 00 UTC 1 Jan of the year of 'date', and subtract the
	 * day components to get the day number */

	M0DTextract(date, &year, &month, &day, &hour,
	  &minute, &second, &microsecond);
	DTnew_year = M0DTmake(year, 1, 1, 0, 0, 0, 0);
	return(date.julday - DTnew_year.julday + 1);
}





/*
*| Name:
*|      M0DTdiff - Computes time increment between dates.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      M0TM
*|      M0DTdiff(M0DT DTstart, M0DT DTend)
*|
*| Input:
*|      DTstart - Start date (high-precision).
*|      DTend   - End date (high-precision).
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      Difference between DTend and DTstart as high-precision time.
*|
*| Remarks:
*|      The difference is in seconds (the units of M0TM), and is
*|      accurate to +/- 1 microsecond or better provided that
*|      the dates are less than 136 years apart (valid for 64-
*|      bit 'double').  Nanosecond precision is retained for
*|      differences of less than 48 days.
*|
*| Categories: 
*|      navigation
*/


M0TM
M0DTdiff(M0DT DTstart, M0DT DTend)
{
	/* routine directly differences the absolute day and 
	 * second components and then calls M0TMmake to
	 * reconstitute them into an increment in seconds */

	M0TM 	TMdiff;		/* time difference	*/

	TMdiff = M0TMmake ( (double)(DTend.julday-DTstart.julday),
	  0., 0., DTend.seconds-DTstart.seconds, 0. );
	return TMdiff; 
}



/*
*| Name:
*|      M0DTearlier - Verify that one date is before another.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      M0flag
*|      M0DTearlier(M0DT DTearly, M0DT DTlate)
*|
*| Input:
*|      DTearly - Expected earlier high precision date.
*|      DTlate  - Expected later   high precision date.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      M0TRUE if DTearly is indeed before DTlate, otherwise
*|      M0FALSE if the same or later.
*|
*| Remarks:
*|      None
*|
*| Categories: 
*|      navigation
*/

M0flag
M0DTearlier(M0DT DTearly, M0DT DTlate)
{
	M0TM TMdiff;	/* time difference	*/


	TMdiff = M0DTdiff(DTearly, DTlate);
	if ( TMdiff > 0. )  {
		return M0TRUE;
	}
	else {
		return M0FALSE;
	}
}


/*
*| Name:
*|      M0DTidentical - Compare absolute dates.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      M0flag
*|      M0DTidentical(M0DT DTa, M0DT DTb)
*|
*| Input:
*|      DTa       - First high-precision date to compare.
*|      DTb       - Second date in comparison.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|	M0TRUE if DTa and DTb are identical, M0FALSE otherwise.
*|
*| Remarks:
*|      This is an exact comparison used to detect copies of
*|      M0DTs. No provision is made for values within some
*|      tolerance of each other but slightly different due
*|      to roundoff during calculations.
*|
*| Categories: 
*|      navigation
*/


M0flag
M0DTidentical(M0DT DTa, M0DT DTb)
{
	if ( (DTa.julday  == DTb.julday  ) &&
	   (DTa.seconds == DTb.seconds ) ) {
		return M0TRUE;
	}
	else {
		return M0FALSE;
	}
}





/*
*| Name:
*|      M0DTextract - Breaks high-precision date into components.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      void
*|      M0DTextract(M0DT date, int *pYear, int *pMonth,
*|      int *pDay, int *pHour, int *pMinute, int *pSecond,
*|      int *pMicrosecond)
*|
*| Input:
*|      date          - High-precision date whose whose subfields
*|                      are extracted.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      *pYear        - Year (yyyy) component of date.
*|      *pMonth       - Month (1 to 12).
*|      *pDay         - Day of month.
*|      *pHour        - Since 00 UTC.
*|      *pMinute      - Minute.
*|      *pSecond      - Seconds in the hour.
*|      *pMicrosecond - Microseconds.
*|
*| Return values:
*|      none
*|
*| Remarks:
*|      The results are rounded to the nearest microsecond.
*|
*| Categories: 
*|      navigation
*/

void
M0DTextract(M0DT date, int *pYear, int *pMonth, int *pDay,
  int *pHour, int *pMinute, int *pSecond, int *pMicrosecond)
{
	/* Implementation note: The routine uses M0TMextract()
	 * to break the 'seconds' portion of the M0DT high-precision
	 * date into hours, minutes, seconds, and microseconds.
	 * Because M0TMextract() rounds to the nearest microsecond,
	 * a value of very nearly a day (86400-e, e< 0.5 microsecond)
	 * will be converted to 1 day, 0 h, 0 m, 0 s, 0 micro. This
	 * 'extra' day must be captured (as tinc_day) and added
	 * onto the absolute day (Julian day) component of the 
	 * 'date' before extracting its components */
	
	int	tinc_day;	/* (see note above)	*/

	/* Validate that incoming 'date' is legal	*/

	M0ASSERT ( date.seconds <= (double)SEC_PER_DAY );
	M0ASSERT ( date.seconds >= 0. );

	/* Split the time portion into constituents. Capture
	 * the 'day' portion that may be nonzero due to
	 * round-up (see implementation note above). */

	M0TMextract(date.seconds, &tinc_day,
	  pHour, pMinute, pSecond, pMicrosecond );

	/* Now split the Julian day portion including the
	 * 'extra' day from any round-up, into components */

	M0jd_extract(date.julday+tinc_day, pYear, pMonth, pDay);

	return;
}



/*
*| Name:
*|      M0DTinc - Changes a date by a time increment.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|	M0DT
*|      M0DTinc(M0DT DTstart, M0TM TMinc)
*|
*| Input:
*|      DTstart - Initial high-precision date.
*|      TMinc   - High-precision time increment.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      DTstart+TMinc in the form of a high-precision date.
*|
*| Remarks:
*|      Microsecond precision is retained if TMinc is less than
*|      136 years (48 days for nanosecond precision). When compiled
*|      with M0DEBUG defined, TMinc is verified to be less
*|      than 68 years (a limit in M0TMextract).
*|
*| Categories: 
*|      navigation
*/

M0DT
M0DTinc(M0DT DTstart, M0TM TMinc)
{
	M0DT	DTend;	/* new date		*/

	DTend	= DTstart;

	/* add the increment onto the 'seconds' portion of
	 * DTstart and verify that no precision was lost */

	DTend.seconds += TMinc;
	M0ASSERT ( fabs(DTend.seconds) < (double)MAX_TM );

	/* Force seconds portion into range 0-86400 (SEC_PER_DAY)
	 * and adjust day portion accordingly. */


	/* This may be very inefficient for big TMinc. Consider
	 * using M0TMextract() instead to get the 'tinc'
	 * in standard units and days (what about negative,
	 * though?   */

	while ( DTend.seconds >= (double)SEC_PER_DAY ) {

		DTend.seconds -= (double)SEC_PER_DAY;
		DTend.julday  += 1;
	}
	while ( DTend.seconds < 0. ) {
		DTend.seconds += (double)SEC_PER_DAY;
		DTend.julday  -= 1;
	}
	return DTend;
}


/*
*| Name:
*|      M0DTmake - Constructs high-precision date from components.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      M0DT
*|      M0DTmake(int year, int month, int day,
*|      int hour, int minute, int second, int microsecond)
*|
*| Input:
*|      year        - Year component of date (yyyy).
*|      month       - Month ( 1-12).
*|      day         - Calendar day of month.
*|      hour        - Since midnight.
*|      minute      - Minute of hour.
*|      second      - Second.
*|      microsecond - Microseconds component of date.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      High-precision date.
*|
*| Remarks:
*|      The routine assumes that the input components are
*|      all in range.  When compiled with M0DEBUG defined, this
*|      assumption will be explicitly evaluated with assertions.
*|
*| Categories: 
*|      navigation
*/

M0DT
M0DTmake(int year, int month, int day,
  int hour, int minute, int second, int microsecond)
{
	M0DT	DTnew;	/* new high-precision date	*/

	/* Validate time components and assemble into
	 * seconds since midnight */

	M0ASSERT ( hmsmValid(hour, minute, second, microsecond));
	DTnew.seconds = M0TMmake(0., (double)hour, (double)minute,
	  (double)second, (double)microsecond);

	/* Validate date components (if M0DEBUG defined) and
	 * assemble into Julian day */

	M0ASSERT ( ymdValid(year, month, day ));
	DTnew.julday  = M0jd_make(year, month, day);
	return DTnew;
}










/*
*| Name:
*|      M0jd_extract - Splits a Julian day into components.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|	void
*|      M0jd_extract(int julday, int *pYear, int *pMonth, int *pDay);
*|
*| Input:
*|      julday    - Julian day.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      *pYear    - Year component of Julian day.
*|      *pMonth   - Month component of Julian day.
*|      *pDay     - Day compontent of Julian day.
*|
*| Return values:
*|      The 'Julian date' corresponding to the inputs.
*|
*| Remarks:
*|	    The 'Julian date' is measured from 1 January 4713 BC.
*|      For astronomical purposes, Julian days start at 12 UTC,
*|      not 00 UTC. Here, the Julian date used as a component
*|      of the high-precision date M0DT is assumed to start at
*|      00 UTC. The present algorithm, adapted from that in
*|      the "Microsoft Quick-C Programmer's Tool Box," is only
*|      valid for years 1584 and later. When compiled with
*|      M0DEBUG defined, this condition is validated with
*|      an assertion.
*|
*| Categories: 
*|      navigation
*/

void
M0jd_extract(int julday, int *pYear, int *pMonth, int *pDay)
{
	int	x, y, d, m;	    /* intermediate results */

	M0ASSERT(julday >= 2299604);

	x = 4 * julday - 6884477;
	y = ( x / 146097 ) * 100;
	d = ( x % 146097 ) / 4;

	x = 4 * d + 3;
	y = ( x / 1461 ) + y;
	d = ( x % 1461 ) / 4;
	d++;

	x = 5 * d - 3;
	m = x / 153;
	m++;
	d = ( x % 153 ) / 5;
	d++;

	if ( m<11 ) {
		*pMonth = m + 2;
	}
	else {
		*pMonth = m - 10;
	}

	*pDay = d;
	*pYear = y + m / 11;
}



/*
*| Name:
*|      M0jd_make - Computes a Julian day for year, month, day.
*|
*| Interface:
*|
*|      int
*|      M0jd_make(int year, int month, int day);
*|
*| Input:
*|      year      - Year component of Julian day.
*|      month     - Month component of Julian day.
*|      day       - Day component of Julian day.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      The 'Julian date' corresponding to the inputs.
*|
*| Remarks:
*|      See M0jd_extract() for the definition of 'Julian Day' and
*|      the origin and limitation of the present algorithm.
*|
*| Categories: 
*|      navigation
*/

int
M0jd_make(int year, int month, int day)
{
	int	ta;		/* intermediate results	*/
	int	tb;
	int	tc;
	int	tm = month;
	int	ty = year;

	M0ASSERT ( year >= 1584 );

	if ( tm > 2 ) {
		tm -= 3;
	}
    	else {
		tm += 9;
		ty--;
	}

	ta = 146097 * ( ty/100 ) / 4;
	tb =   1461 * ( ty%100 ) / 4;
	tc =  ( 153 *   tm+2   ) / 5 + day + 1721119;

	return (ta+tb+tc);
}




/*
*| Name:
*|      M0TMextract - Extracts components of high-precision time.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|	void
*|      M0TMextract(M0TM time, int *pHour, int *pMinute, int *pSecond,
*|      int *pMicrosecond)
*|
*| Input:
*|      time            - Time field being split.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      *pHour          - Hours component of time field.
*|      *pMinute        - Minutes component of time field.
*|      *pSecond        - Seconds component of time field.
*|      *pMicrosecond   - Microseconds component of field.
*|
*| Return values:
*|      none
*|
*| Remarks:
*|      A negative time increment 'time' will result in negative
*|      values of all of the constituent pieces. All components
*|      of the time (hours, minutes, seconds, and microseconds)
*|      are reduced to be in range; any excess over 24 h is reported
*|      as a number of days.
*|      If the 'time' exceeds 68 years, an error due to truncation
*|      will be trapped with an assertion if compiled with M0DEBUG
*|      defined.
*|
*| Categories: 
*|      navigation
*/

void
M0TMextract(M0TM TM, int *pDay, int *pHour, int *pMinute,
  int *pSecond, int *pMicrosecond)
{
	double		abtime;	/* absolute value of time	*
				 * rounded to nearest		*
				 * microsecond			*/
	double		second;	/* intermediate number of	*
				 * seconds, fractional		*/
	ldiv_t		q;	/* integer division result	*/
	long		lsecond;/* number of seconds, long int	*/

	/* Input time TM is in seconds. Compute its absolute	*
	 * value and strip off the fractional part; convert that*
	 * to microseconds. Adding 0.5 forces rouding to nearest*
	 * microsecond */

	abtime  = fabs((double)TM);
	abtime += 0.5/(double)MICROSEC_PER_SEC;
	*pMicrosecond = (int) ((double)MICROSEC_PER_SEC
		*modf(abtime,&second) );

	/* Trap possible loss of accuracy if an 'int' is too
	 * small to take the integer part of a 'double' (happens
	 * when the number of seconds exceeds 2^31-1 or 68 YEARS.
	 */

	M0ASSERT ( second < (double)MAX_TM );
	lsecond = (long)second;

	/* Now strip off the number of days, hours, and
	 * minutes making up the whole seconds in 'tinc'
	 * using integer math */

	q 	= ldiv ( lsecond, SEC_PER_DAY );
	*pDay	= (int)q.quot;
	q	= ldiv ( q.rem, SEC_PER_HOUR );
	*pHour	= (int)q.quot;
	q	= ldiv ( q.rem, SEC_PER_MINUTE );
	*pMinute= (int)q.quot;
	*pSecond= (int)q.rem;

	/* Apply range checking	*/

	M0ASSERT ( *pMicrosecond	< MICROSEC_PER_SEC );
	M0ASSERT ( *pHour		< 24		);
	M0ASSERT ( *pMinute		< 60		);
	M0ASSERT ( *pSecond		< 60		);

	/* if input time was negative, flip signs on all
	 * components before returning */

	if ( TM < 0. ) {
		*pDay		= -(*pDay);
		*pHour		= -(*pHour);
		*pMinute	= -(*pMinute);
		*pSecond	= -(*pSecond);
		*pMicrosecond	= -(*pMicrosecond);
	}
	return;
}

/*
*| Name:
*|  M0TMmake - Assembles high-precision time from components.
*|
*| Interface:
*|      #include "m0frame.h"
*|
*|      M0TM
*|      M0TMmake(double day, double hour, double minute,
*|      double second, double microsecond)
*|
*| Input:
*|      day          - Days component of time field.
*|      hour         - Hour component.
*|      minute       - Minutes component.
*|      second       - Seconds component.
*|      microsecond  - Microseconds component.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      high-precision time (type M0TM).
*|
*| Remarks:
*|	    M0TMmake() makes NO assumptions about the range of its
*|      inputs. In addition, input type 'double' allows non-integral
*|      values of any or all components of time.
*|      If the 'time' exceeds 68 years, an error due to truncation
*|      will be trapped with an assertion if compiled with M0DEBUG
*|      defined.
*|
*| Categories: 
*|      navigation
*/


M0TM
M0TMmake(double day, double hour, double minute, double second,
  double microsecond )
{
	M0ASSERT(sizeof(M0TM)>=4);	/* should never fire on
					 * 32-bit machines. On 16-bit
					 * machine, make M0TM a 'long'
					 * to retain necessary
					 * accuracy */
	M0ASSERT ( 
	  ( SEC_PER_DAY    * day
	  + SEC_PER_HOUR   * hour
	  + SEC_PER_MINUTE * minute
	  + second
	  + microsecond/MICROSEC_PER_SEC ) < MAX_TM );

	return ( (M0TM)
	  ( SEC_PER_DAY    * day
	  + SEC_PER_HOUR   * hour
	  + SEC_PER_MINUTE * minute
	  + second
	  + microsecond/MICROSEC_PER_SEC ) );
}

	
#if defined ( UNIT_TEST )

static void
DTput(FILE *stream, const char *szLabel, M0DT DT);

static M0DT
DTget(FILE *instream, FILE *msgstream, char *szPrompt);

static void
TMput(FILE *stream, const char *szLabel, M0TM TM);

static M0TM
TMget(FILE *instream, FILE *msgstream, char *szPrompt);

#include <stdio.h>
#include <string.h>


int main ( const int argc, char ** const argv ) 

/* Unit test package for high-precision dates and times:
 *
 * Test philosophy:
 *
 *	M0dabtim	No test. Problems should be apparent
 *			when used to drive navigation. Because
 *			components of 'Julian date' are used
 *			in all high-precision dates, if other
 *			date functions work, 'M0dabtim' should
 *			either be right or wrong all the time.
 *	M0DTavg		verify that M0DTavg(DT1,DT2) ==
 *			DT1 + M0DTinc(DT1, M0DTdiff(DT2,DT1)/2. )
 *			for many values of DT1 and DT2, including
 *			DT1 < DT2, DT1==DT2, and DT1>DT2
 *	M0DTcurrent	Does it 'look' right?
 *	M0DTdaynum	Verify that each day is one day greater
 *			than the previous ones except at New
 *			Year's. Related: use ymdValid to
 *			prepare a list of leap years.
 *	M0DTdiff	Test with M0DTinc. Prompt for two
 *			dates and a number of increments. Compute
 *			the high-precision time increment and
 *			print intermediate and final dates. They
 *			should match to nearest microsecond.
 *	M0DTmake	Test with M0DTextract. Repeat above test
 *			with increment containing an even multiple
 *			of microseconds; do an extract and remake
 *			every step; the remade high-precision date
 *			should match the original.
 * (total time spent writing the above: 30 minutes) */

{
	char	szBuffer[BUFSIZ];	/* text input buffer	*/

	int	day;		/* day			*/
	int	hou;		/* hour			*/
	int	mic;		/* microsecond		*/
	int	min;		/* minute		*/
	int	mnYear;		/* earliest year	*/
	int	mon;		/* month		*/
	int	mxYear;		/* latest year		*/
	int	sec;		/* second		*/
	int	yea;		/* year			*/

	M0DT	DTa;		/* High precision date A	*/
	M0DT	DTb;		/* High precision date B	*/
	M0DT	DTc;		/* High precision date C	*/
	M0DT	DTd;		/* High precision date D	*/

	M0TM	TMab;		/* High precision time B-A	*/
	M0TM	TMad;		/* High precision time D-A	*/
	M0TM	TMinc;		/* High precision increment	*/

	size_t	cInc;		/* Number of increments		*/
	size_t	inc;		/* current increment		*/


	printf("\nStarting date.c unit test...%s\n\n", argv[0] );


	/* Fetch current date and display (took 2h to get this
	 * portion, including DTput, working. I was modifying
	 * the "Current" string inside DTput, rather than making
	 * a local copy */

	DTa = M0DTcurrent();
	DTput(stdout,"Current",DTa);


	/* Averaging test	*/

	printf("\nAveraging test (M0DTdiff, M0DTinc, M0DTavg)\n\n");

	DTa = DTget(stdin, stdout, "Date A");
	DTb = DTget(stdin, stdout, "Date B");
	DTput(stdout, "Date A", DTa);
	DTput(stdout, "Date B", DTb);
	TMab = M0DTdiff(DTa,DTb);
	printf("TMab = %f\n", TMab);
	DTc = M0DTinc(DTa,TMab/2.);
	DTput(stdout, "Computed average=", DTc);
	DTc = M0DTavg(DTa, DTb);
	DTput(stdout, "M0DTavg  average=", DTc);

	/* Incrementing test */

	printf("\nIncrementing test (M0DTdiff, M0DTinc)\n\n");

	printf("Enter number of intervals: ");
	fgets(szBuffer, BUFSIZ, stdin);
	szBuffer[strlen(szBuffer)-1] = '\0';	/* clobber newline */
	sscanf(szBuffer,"%u", &cInc);
	TMinc = TMab / cInc;
	printf("cInc=%d, TMinc=%f\n", cInc, TMinc);

	DTc = DTa;
	DTput(stdout, "Start  =",DTc);
	for ( inc=0; inc<cInc; inc++ ) {
		sprintf(szBuffer,"%8d", inc+1);
		DTc = M0DTinc(DTc,TMinc);
		DTput(stdout,szBuffer,DTc);
	}

	/* Make and extract test */

	printf("\nMake/Extract test (M0DTmake, M0DTextract)\n\n");


	TMinc = TMget(stdin, stdout, "TMinc" );
	cInc = TMab / TMinc;			/* approximate	*/
	printf("cInc=%d\n", cInc);
	DTc = DTa;
	for ( inc=0; inc<cInc; inc++) {
		DTc = M0DTinc(DTc, TMinc);
		M0DTextract(DTc, &yea, &mon, &day, &hou, &min,
		  &sec, &mic);
		DTd = M0DTmake(yea, mon, day, hou, min, sec, mic);
		DTput(stdout,"extract/remake ",DTd);
		TMad= M0DTdiff(DTc,DTd);
		M0ASSERT(fabs(TMad)<0.5/MICROSEC_PER_SEC);
	}
	printf("Complete!\n",0);


	/* M0DTdaynum and ymdValid test (detect leap years)	*/

	printf("\nTesting M0DTdaynum() and ymdValid() with leap "
	  "years\n\n");
	printf("Enter two years (early and late): ");
	fgets(szBuffer, BUFSIZ, stdin);
	szBuffer[strlen(szBuffer)-1] = '\0';	/* clobber newline*/
	sscanf(szBuffer, "%d %d", &mnYear, &mxYear);

	printf("\nList of all leap years from %d to %d\n",
	  mnYear, mxYear);

	for ( yea=mnYear; yea<=mxYear; yea++) {
		if ( ymdValid(yea,2,29) ) {
			printf("%d\n",yea);
			DTa = M0DTmake(yea,12,31,0,0,0,0);
			M0ASSERT(M0DTdaynum(DTa) == 366 );
		}
		else {
			if( (yea % 4) == 0 ) {
				printf("%d NOT leap year\n", yea);
			}
			DTa = M0DTmake(yea,12,31,0,0,0,0);
			M0ASSERT(M0DTdaynum(DTa) == 365 );
		}
	}
		
	printf("Unit test \"%s\" done\n\n", argv[0]);


	return 0;
}


static void
DTput(FILE *stream, const char *szLabel, M0DT DT)
{
	/* label is truncated or blankfilled to LBLLEN characters */

	enum	{LBLLEN=8};	/* label length		*/

	char	szFormat[BUFSIZ];	/* output format	*/
	char	szNewLabel[BUFSIZ];	/* Modified label	*/
	char	szWidth[4];		/* Character form of
					 * label field width	*/

	int	ich;		/* index into buffer	*/
	int	day;		/* day			*/
	int	hou;		/* hour			*/
	int	mic;		/* microsecond		*/
	int	min;		/* minute		*/
	int	mon;		/* month		*/
	int	sec;		/* second		*/
	int	yea;		/* year			*/

	size_t	len;		/* string length	*/


	/* Blank fill to LBLLEN characters if length is less.
	 * Truncation to LBLLEN characters will be done by
	 * fprintf() */

	strcpy(szNewLabel,szLabel);	/* make local copy	*/
	len = strlen(szNewLabel);
	if(len < LBLLEN ) {
		for(ich=len; ich<LBLLEN; ich++ ) {
			szNewLabel[ich] = ' ';
		}
		szNewLabel[LBLLEN] = '\0';
	}
	sprintf(szWidth,"%1d", LBLLEN);
		
	strcpy(szFormat,"%Ns: %9d, %14.7f: %4d.%2d.%2d "
	  "%2d:%2d:%2d.%6.6d\n");
	szFormat[1] = szWidth[0];	/* character assignment
					 * to tack in width	*/
	M0DTextract(DT, &yea, &mon, &day, &hou, &min, &sec, &mic);

	fprintf(stream, szFormat,
	  szNewLabel, DT.julday, DT.seconds, yea, mon, day, hou, min,
	  sec, mic);
	return;
}

static M0DT
DTget(FILE *instream, FILE *msgstream, char *szPrompt)
{
	char	szBuffer[BUFSIZ];	/* character input buffer*/

	int	day;			/* day			*/
	int	hou;			/* hour			*/
	int	mic;			/* microsecond		*/
	int	min;			/* minute		*/
	int	mon;			/* month		*/
	int	sec;			/* second		*/
	int	yea;			/* year			*/

	M0DT	DTin;			/* High-precision date
					 * constructed from
					 * inputs	*/

	size_t	len;			/* string length	*/

	M0ASSERT(instream != NULL);	/* no input stream !	*/

	/* Input the date and time components. Strip the newline
	 * off the end of the buffer to keep from confusing
	 * sscanf()	*/

	if(msgstream != NULL) {
		fprintf(msgstream,"Enter date (y m d h m s mic) for "
		  "%s\n", szPrompt);
	}
	fgets(szBuffer, BUFSIZ, instream);
	len = strlen(szBuffer);
	if(szBuffer[len] == '\n') 
		szBuffer[len] = '\0';
	sscanf(szBuffer, "%d %d %d %d %d %d %d", 
	  &yea, &mon, &day, &hou, &min, &sec, &mic);
	DTin = M0DTmake(yea, mon, day, hou, min, sec, mic);
	return DTin;
}

static M0TM
TMget(FILE *instream, FILE *msgstream, char *szPrompt)
{
	char	szBuffer[BUFSIZ];	/* input buffer		*/

	double	day;			/* day			*/
	double	hou;			/* hour			*/
	double	mic;			/* microsecond		*/
	double	min;			/* minute		*/
	double	sec;			/* second		*/

	int	status;			/* sscanf() returns	*/

	M0TM	TMin;			/* high-precision time	*/

	size_t	len;			/* string length	*/


	M0ASSERT(instream != NULL);	/* no input stream !	*/

	/* Input the date and time components. Strip the newline
	 * off the end of the buffer to keep from confusing
	 * sscanf()	*/

	if(msgstream != NULL) {
		fprintf(msgstream,"Enter time (d h m s mic) for %s\n",
		  szPrompt);
	}
	fgets(szBuffer, BUFSIZ, instream);
	len = strlen(szBuffer);
	if(szBuffer[len-1] == '\n') 
		szBuffer[len-1] = '\0';
	printf("Buffer=[%s]\n", szBuffer);

	status = sscanf(szBuffer, "%lf %lf %lf %lf %lf", 
	  &day, &hou, &min, &sec, &mic);
	M0ASSERT(status == 5);	/* did not read needed quantities */
	printf("double d h m s m = %f %f %f %f %f\n",
	  day, hou, min, sec, mic);

	TMin = M0TMmake(day, hou, min, sec, mic);
	return TMin;
}


static void
TMput(FILE *stream, const char *szLabel, M0TM TM)
{
	/* label is truncated or blankfilled to LBLLEN characters */

	enum	{LBLLEN=8};

	char	szFormat[BUFSIZ];	/* output format	*/
	char	szNewLabel[BUFSIZ];	/* Modified label	*/
	char	szWidth[4];		/* Character form of
					 * label field width	*/

	int	ich;			/* index into buffer	*/
	int	day;			/* day			*/
	int	hou;			/* hour			*/
	int	mic;			/* microsecond		*/
	int	min;			/* minute		*/
	int	sec;			/* second		*/

	size_t	len;			/* string length	*/

	/* Blank fill to LBLLEN characters if length is less. Truncation
	 * to LBLLEN characters will be done by fprintf() */

	strcpy(szNewLabel,szLabel);	/* make local copy	*/
	len = strlen(szNewLabel);
	if(len < LBLLEN ) {
		for(ich=len; ich<LBLLEN; ich++ ) {
			szNewLabel[ich] = ' ';
		}
		szNewLabel[LBLLEN] = '\0';
	}
	sprintf(szWidth,"%1d", LBLLEN);
		
	strcpy(szFormat,"%Ns: 14.7f: %dd %2.2dh %2.2dm %2.2d.6.6ds\n");
	szFormat[1] = szWidth[0];	/* character assignment to
					 * tack in width	*/
	M0TMextract(TM, &day, &hou, &min, &sec, &mic);

	fprintf(stream, szFormat,
	  szNewLabel, TM, day, hou, min, sec, mic);
	return;
}

#endif	/* defined (UNIT_TEST) */
