/* $Header: /code/cvs/rdss/rdsslibs/ui/ui_date.h,v 1.1 1989-02-08 13:28:14 corbet Exp $ */
/*
 * Date stuff.
 */

# ifndef UI_DATE_SYMBOLS
# define UI_DATE_SYMBOLS
/*
 * Options for ud_format_date ()
 */
char * ud_format_date ();
# define UDF_FULL	0	/* Format the date in its entirety. 	*/

/*
 * Macro to recognize an offset date.
 */
# define D_OFFSET(date) (ABS((date).ds_yymmdd) < 10000)
# endif
