/*
 * Special versions of output and error signalling routines, which allow
 * the application to divert things.
 */
# include "ui_param.h"

# ifdef __STDC__
# include <stdarg.h>
# else
# include <varargs.h>
# endif

static char *rcsid = "$Id: ui_OutHook.c,v 1.4 1994-11-01 02:56:49 case Exp $";

/*
 * The routines we use to output normal (ui_printf) text.
 */

#ifndef hpux
extern int printf ();
#endif 
static void errprint ();

typedef void (*vfptr) ();
static vfptr Normal_out = ( void (*) () ) printf;  /* Normal output routine */
static vfptr Nf_out = ( void (*) () ) printf;	/* No flush output routine */

/*
 * And for error output.
 */
static vfptr Error_out = errprint;
static vfptr Error_hook = 0;


ui_OutputRoutine (normal, nf)
vfptr normal, nf;
/*
 * Change the output routines.
 */
{
	Normal_out = normal;
	Nf_out = nf;
}



ui_ErrorOutputRoutine (err)
vfptr err;
/*
 * Change the error reporting routine.
 */
{
	Error_out = err;
}


ui_ErrorHook (hook)
vfptr hook;
/*
 * Add an error processing hook.
 */
{
	Error_hook = hook;
}



# ifdef __STDC__


/* use the ANSI standard variable args */

ui_printf (char *fmt, ...)

/*
 * Perform a printf() through the user interface I/O.
 */
{
        char buf[1000];
	va_list args;

/*
 * Encode the output from the variable list
 */
	va_start (args, fmt);


# else


/*
 * We still need the K&R version for some compilers, unfortunately
 */ 

ui_printf (va_alist)
va_dcl

/*
 * Perform a printf() through the user interface I/O.
 */
{
        char buf[1000];
        char *fmt;
        va_list args;

  	va_start (args);
        fmt = va_arg (args, char *);

# endif /* ifdef __STDC __ */


/*
 * Encode the output.
 */

        vsprintf (buf, fmt, args);


/*
 * We're done with the variable list.
 */
        va_end (args);


/*
 * Output it.
 */

        (*Normal_out) (buf);

}



# ifdef __STDC__


/* use the ANSI standard variable args */

ui_nf_printf (char *fmt, ...)

/*
 * Perform an unflushed printf() through the user interface I/O.
 */
{
        char buf[1000];
        va_list args;

        va_start (args, fmt);

# else


/* use K&R for non-ANSI compilers */

ui_nf_printf (va_alist)
va_dcl

{
        char buf[1000];
        char *fmt;
        va_list (args);

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* ifdef __STDC__ */

        vsprintf (buf, fmt, args);

/*
 * We're done with the variable list.
 */
        va_end (args);

/*
 * Output it.
 */

        (*Nf_out) (buf);

}



ui_ErrOut (line)
char *line;
/*
 * Output an error line.
 */
{
	if (Error_hook)
		(*Error_hook) ();
	(*Error_out) (line);
}



ui_WarnOut (line)
char *line;
/*
 * Output a warning line.
 */
{
	(*Error_out) (line);
}





static void
errprint (line)
char *line;
/*
 * Default error printer.
 */
{
	printf ("%s\n", line);
}
