/* $Id: ui_date.h,v 1.3 1998-10-28 21:23:02 corbet Exp $ */
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
	void ud_y2k_date (struct date_st *);
# else
	char * ud_format_date ();
# endif
# define UDF_FULL	0	/* Format the date in its entirety. 	*/

/*
 * Macro to recognize an offset date.
 */
# define D_OFFSET(date) (ABS((date).ds_yymmdd) < 10000)
# endif
