/*
 * Special versions of output and error signalling routines, which allow
 * the application to divert things.
 */
# include "ui_param.h"

static char *rcsid = "$Id: ui_OutHook.c,v 1.1 1990-06-09 13:21:57 corbet Exp $";

/*
 * The routines we use to output normal (ui_printf) text.
 */
extern void printf ();
static void errprint ();

typedef void (*vfptr) ();
static vfptr Normal_out = printf;	/* Normal output routine	*/
static vfptr Nf_out = printf;		/* No flush output routine	*/

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





ui_printf (fmt, ARGS)
char *fmt;
int ARGS;
/*
 * Perform a printf() through the user interface I/O.
 */
{
	char buf[1000];
	int nadd;
/*
 * Encode the output.
 */
 	sprintrmt (buf, fmt, SPRINTARGS);
/*
 * Output it.
 */
	(*Normal_out) (buf);
}



ui_nf_printf (fmt, ARGS)
char *fmt;
int ARGS;
/*
 * Perform an unflushed printf() through the user interface I/O.
 */
{
	char buf[1000];
	int nadd;
/*
 * Encode the output.
 */
 	sprintrmt (buf, fmt, SPRINTARGS);
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
