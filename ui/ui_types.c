/* 1/87 jc */
/*
 * Routines pertaining to types -- identification, interpretation, coercion...
 */
# include <string.h>
# include <ctype.h>
# include "ui_param.h"
# include "ui_symbol.h"

static char *rcsid = "$Id: ui_types.c,v 1.9 1999-06-25 19:16:30 burghart Exp $";

/*
 * Month info.
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
 * The coercion table.  This is a two dimensional table that may be 
 * used to look up the coercion operator for any two types.
 *
 * Note that it depends on the definitions in ui_symbol.h
 */
void uit_bad (), uit_ftoi (), uit_itof (), uit_ident (), uit_ftod ();
void uit_itod (), uit_ftob (), uit_btof (), uit_btoi ();

# define CVT_WIDTH 5	/* Table width */
typedef void (*ifptr) ();
ifptr Cvt_tbl[CVT_WIDTH * CVT_WIDTH] =
{
/* float	int		string		date		bool	*/
  uit_ident,	uit_ftoi,	uit_bad,	uit_ftod, 	uit_ftob,
  uit_itof,	uit_ident,	uit_bad,	uit_itod, 	uit_ident,
  uit_bad,	uit_bad,	uit_ident,	uit_bad, 	uit_bad,
  uit_bad,	uit_bad,	uit_bad,	uit_ident, 	uit_bad,
  uit_btof,	uit_btoi,	uit_bad,	uit_bad, 	uit_ident
};

/*
 * Prototypes
 */
void uit_split_date (char *string, char *dest, char **dte, char **time);




void
uit_interp (string, type, v)
char *string;
int *type;
union usy_value *v;
/*
 * Interpret the given string.
 * Entry:
 *	STRING	is a value of unknown type.
 * Exit:
 *	TYPE	is the "least common denominator" type found for this string.
 *	V	is the value of the string, in the given type.  Note that no
 *		new storage is allocated for string types.
 */
{
/*
 * First, see if we can make a date out of this.
 */
 	if (uit_parse_date (string, v, TRUE))
		*type = SYMT_DATE;
/*
 * Try an integer.
 */
 	else if (uit_int_parse (string, v))
		*type = SYMT_INT;
/*
 * Try a real number.
 */
 	else if (uit_real_parse (string, v))
		*type = SYMT_FLOAT;
/*
 * Maybe bool will work.
 */
 	else if (uit_bool_parse (string, v, TRUE))
		*type = SYMT_BOOL;
/*
 * Nope, this thing is good for nothing other than a basic string.
 */
 	else
	{
		*type = SYMT_STRING;
		v->us_v_ptr = string;
	}
}




int
uit_parse_date (string, v, strict)
char *string;
union usy_value *v;
bool strict;
/*
 * Attempt to parse the date given in "string".
 */
{
	char dstr[128], *ds, *ts;
	int len;
/*
 * Split the string up into date/time.
 */
	uit_split_date (string, dstr, &ds, &ts);
	len = strict ? 4 : strlen (ds);
/*
 * Make sure we got something.
 */
	if (! *ds)
		return (FALSE);
	v->us_v_date.ds_yymmdd = v->us_v_date.ds_hhmmss = 0;
/*
 * See which time syntax is being used.
 *
 * today,hh:mm:ss
 */
	if (! strncmp (ds, "today", len))
	{
		pmu_g_today (&v->us_v_date.ds_yymmdd);
		return (uit_time_get (ts, &v->us_v_date.ds_hhmmss));
	}
/*
 * yesterday hh:mm:ss
 */
	else if (! strncmp (ds, "yesterday", len))
	{
		pmu_g_yesterday (&v->us_v_date.ds_yymmdd);
		return (uit_time_get (ts, &v->us_v_date.ds_hhmmss));
	}
/*
 * now.
 */
	else if (! strcmp (ds, "now"))
		return (pmu_g_now (&v->us_v_date.ds_yymmdd,
				&v->us_v_date.ds_hhmmss));
/*
 * -hh:mm:ss.
 */
	else if (*ds == '-')
		return (pmu_g_minus (ts, &v->us_v_date.ds_yymmdd,
				&v->us_v_date.ds_hhmmss));
/*
 * dd-mmm-yy,hh:mm:ss
 */
	else
	{
		if (! uit_date_get (ds, ts, &v->us_v_date.ds_yymmdd))
			return (FALSE);
		return (*ts ? uit_time_get(ts, &v->us_v_date.ds_hhmmss) :
					TRUE);
	}
}




static int
mcmp (test, month)
char *test, *month;
/*
 * Compare the test string against the month name, in a case-insensitive way.
 */
{
	return (tolower(test[0]) == month[0] &&
		tolower(test[1]) == month[1] &&
		tolower(test[2]) == month[2]);
}







int
uit_date_get (ds, ts, dte)
char *ds, *ts;
int *dte;
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
/*
 * Check for an offset date.
 */
 	if (*ts && *ds == '\0')
	{
		*dte = day;
		return (TRUE);
	}
	if (! day || day > 31 || *ds != '-')
		return (FALSE);
/*
 * Now find the end of the month name.
 */
	ds++;
	for (cp = ds; *cp && *cp != '-'; cp++) ;
	if (*cp != '-')
		return (FALSE);
/*
 * Try to recognize the date name.
 */
	*cp = 0;	/* Zap - */
	for (month = 1; month <= 12; month++)
		if (mcmp (ds, Month_names[month]))
			break;
	if (month > 12)
		return (FALSE);
/*
 * Decode the year.
 */
	*cp++ = '-';	/* Put the - back -- somebody might miss it. */
	year = 0;
	while (*cp && isdigit (*cp))
		year = year*10 + *cp++ - '0';
# ifdef STUPID_STUFF
	if (year > 1900)
		year -= 1900;
	if (year < 70 || year > 99)
		return (FALSE);
# endif
	if (year < 10)
		year += 2000;
	else if (year < 100)
		year += 1900;
/*
 * Wow!  We actually made it through all that.  Return the date.
 */
	*dte = year*10000 + month*100 + day;
	return (TRUE);
}



int
uit_time_get (ts, time)
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
		return (FALSE);
/*
 * Pick off the hours.
 */
	hour = 0;
	while (*ts && isdigit (*ts))
		hour = hour*10 + *ts++ - '0';
	if (hour > 24) /* 3/13/85 jc 24 to allow -24:00:00 (kludge) */
		return (FALSE);
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
		return (FALSE);
/*
 * Pick off the minute value.
 */
	minute = 0;
	while (*ts && isdigit (*ts))
		minute = minute*10 + *ts++ - '0';
	if (minute > 59)
		return (FALSE);
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
		return (FALSE);
/*
 * Finally, pick off the seconds value.
 */
	second = 0;
	while (*ts && isdigit (*ts))
		second = second*10 + *ts++ - '0';
	if (second > 59)
		return (FALSE);
	pmu_check_end (ts);
	*time = hour*10000 + minute*100 + second;
	return (TRUE);
}





void
uit_split_date (string, dest, dte, time)
char *string, *dest, **dte, **time;
/*
 * Split the date/time string into its respective components.  Note that this
 * is a very simple routine, simply splitting things at the imbedded comma.
 */
{
	char *comma, *pmu_strend ();
/*
 * Find the comma.
 */
	if ((comma = strchr (string, ',')) == 0)
	{
	/*
	 * No comma -- assume date only.
	 */
		strcpy (dest, string);
		*dte = dest;
		*time = pmu_strend (string) - 1;
	}
	else
	{
	/*
	 * Got a comma -- split the string apart.
	 */
		strncpy (dest, string, comma - string);
		dest[comma - string] = '\0';
		*dte = dest;
		*time = dest + (comma - string) + 1;
		strcpy (*time, comma + 1);
	}
}




int
uit_int_parse (string, v)
char *string;
union usy_value *v;
/*
 * Attempt to parse this string as an integer.
 */
{
	int val = 0;
	bool minus = FALSE;
	char *fulls = string;
/*
 * Check for negative numbers.
 */
 	if (string[0] == '-')
	{
		minus = TRUE;
		string++;
	}
/*
 * Now pass through the number.
 */
 	while (*string)
	{
		if (*string < '0' || *string > '9')
			return (FALSE);
		val = val*10 + *string - '0';
		string++;
	}
/*
 * OK, the number has been successfully decoded.  Let's just finish up.
 */
 	v->us_v_int = minus ? -val : val;
	return (TRUE);
}




int
uit_real_parse (string, v)
char *string;
union usy_value *v;
/*
 * Attempt to parse a real number.
 */
{
	float val = 0, frac;
	bool minus = FALSE;
	char *fulls = string;
/*
 * Check for negative numbers.
 */
 	if (string[0] == '-')
	{
		minus = TRUE;
		string++;
	}
/*
 * Now pass through the integral part of the number.
 */
 	while (*string && *string != '.')
	{
		if (*string < '0' || *string > '9')
			return (FALSE);
		val = val*10 + *string - '0';
		string++;
	}
/*
 * OK, now do the fractional part, if there is one.
 */
 	if (*string)
		string++;
	frac = 0.1;
	while (*string)
	{
		if (*string < '0' || *string > '9')
			return (FALSE);
		val += (*string++ - '0') * frac;
		frac /= 10.0;
	}
/*
 * Parse completed.
 */
 	v->us_v_float = minus ? -val : val;
	return (TRUE);
}




int
uit_bool_parse (string, v, strict)
char *string;
union usy_value *v;
bool strict;
/*
 * Try to parse a boolean value.
 */
{
	int len = strict ? 4 : strlen (string);

 	if (! strncmp (string, "true", len) ||
		! strncmp (string, "True", len) ||
		! strncmp (string, "TRUE", len))
	{
		v->us_v_int = TRUE;
		return (TRUE);
	}
 	else if (! strncmp (string, "false", len) ||
		 ! strncmp (string, "False", len) ||
		 ! strncmp (string, "FALSE", len))
	{
		v->us_v_int = FALSE;
		return (TRUE);
	}
	return (FALSE);
}




void
uit_coerce (value, from, to)
union usy_value *value;
int from, to;
/*
 * Coerce this value from FROM to TO.
 */
{
	(*Cvt_tbl[from*CVT_WIDTH + to]) (value, from, to);
}


void
uit_bad (value, from, to)
union usy_value *value;
int from, to;
/*
 * This is the "no can do" routine.
 */
{
	static char *types[] = { "float", "int", "string", "date", "bool",
				 "symbol table", "pointer" };
	ui_error ("Expression error -- unable to convert from %s to %s",
		types[from], types[to]);
}


void
uit_ident ()
/*
 * Identity transformation.
 */
{ /* This is a rough one */ }


void
uit_ftoi (v)
union usy_value *v;
/*
 * Convert a floating value to an integer.
 */
{
	v->us_v_int = (int) v->us_v_float;
}


void
uit_itof (v)
union usy_value *v;
/*
 * Convert an integer value to float.
 */
{
	v->us_v_float = (float) v->us_v_int;
}



void
uit_itod (v)
union usy_value *v;
/*
 * Convert an integer value to a date (offset).
 */
{
	v->us_v_date.ds_yymmdd = v->us_v_int;
	v->us_v_date.ds_hhmmss = 0;
}



void
uit_ftod (v)
union usy_value *v;
/*
 * Convert a floating value to a date.
 */
{
	int ndays, minutes;

	ndays = (int) v->us_v_float;
	minutes = (int) ((v->us_v_float - (float) ndays) * 1440.0);
	v->us_v_date.ds_yymmdd = ndays;
	v->us_v_date.ds_hhmmss = (minutes/60)*10000 + (minutes % 60) * 100;
}




void
uit_ftob (v)
union usy_value *v;
/*
 * floating to boolean.
 */
{
	v->us_v_int = (v->us_v_float == 0.0);
}


void
uit_btof (v)
union usy_value *v;
/*
 * Convert boolean to floating.
 */
{
	v->us_v_float = v->us_v_int ? 1.0 : 0.0;
}


void
uit_btoi (v)
union usy_value *v;
/*
 * Convert boolean to int.
 */
{
	if (v->us_v_int != 0)
		v->us_v_int = TRUE;
}




int uit_int_type (name)
char *name;
/*
 * Given a type name, convert it to an internal type.
 */
{
/* 
 * Kludge, for now.
 */
	if (! strcmp (name, "integer"))
		return (SYMT_INT);
	else if (! strcmp (name, "float"))
		return (SYMT_FLOAT);
	else if (! strcmp (name, "bool"))
		return (SYMT_BOOL);
	else if (! strcmp (name, "date"))
		return (SYMT_DATE);
	else if (! strcmp (name, "string"))
		return (SYMT_STRING);
	return (SYMT_UNDEFINED);
}
