/* $Id: ui_date.h,v 1.2 1990-09-11 16:12:51 corbet Exp $ */
/*
 * Date stuff.
 */

# ifndef UI_DATE_SYMBOLS
# define UI_DATE_SYMBOLS
/*
 * Options for ud_format_date ()
 */
# ifdef __STDC__
	char *ud_format_date (char *, struct date_st *, int);
# else
	char * ud_format_date ();
# endif
# define UDF_FULL	0	/* Format the date in its entirety. 	*/

/*
 * Macro to recognize an offset date.
 */
# define D_OFFSET(date) (ABS((date).ds_yymmdd) < 10000)
# endif
