/* 11/84 jc */
/*
 * Error handling.
 */
# include <setjmp.h>
# include "ui_param.h"
# include "ui_symbol.h"

# ifdef UNIX
# ifndef SYSV
# define longjmp _longjmp
# endif /* SYSV */
# endif /* UNIX */

static char *rcsid = "$Id: ui_error.c,v 1.6 1991-02-22 23:25:48 burghart Exp $";
/*
 * Stack stuff.
 */
static struct jstack
{
	struct jstack *js_next;	/* Next entry in the stack	*/
	int *js_value;		/* The value we pushed		*/
} *Stack = 0;

/*
 * Errorbell is true iff a bell is to be rung on errors.
 */
# ifdef CRAY
	static bool Errorbell = FALSE;
# else
	static bool Errorbell = TRUE;
# endif

/*
 * The lookaside list for jstack structures.
 */
static struct jstack *Js_lal = 0;


ui_errinit ()
/*
 * Initialize the error handling stuff.
 */
{
/*
 * Just create indirect variables.
 */
 	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "ui$errorbell",
		&Errorbell, SYMT_BOOL, 0);
}






ui_error (fmt, ARGS)
char *fmt;
int ARGS;
/*
 * Put out an error message.
 */
{
	char buf[200], buf1[200];
/*
 * Write out the message.
 */
	sprintrmt (buf, fmt, SPRINTARGS);
	sprintf (buf1, Errorbell ? "*** %s\007" : "*** %s", buf);
	ui_ErrOut (buf1);
/*
 * Return to the command interpreter.
 * (10/11/85 jc)	If there is an alternate jbuf on the stack, we will
 *			jump to that instead.
 */
	if (Stack)
		longjmp (Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}





ui_cl_error (jump, col, fmt, ARGS)
bool jump;
int col;
char *fmt;
int ARGS;
/*
 * Put out a command-line related error.
 * Entry:
 *	JUMP	is true if an error longjmp is to be executed after
 *		outputting the message.
 *	COL	is the column number of the error.
 *	FMT	is a printf-style format string
 *	ARGS	is zero or more arguments to fill into FMT.
 * Exit:
 *	The message has been put out.
 */
{
	char buf[200], buf1[200], *bp;
	int len, ndash;
/*
 * Get the command line echoed out.
 */
 	/* ut_out_lines (); */
	col += 7;	/* For extra garbage from ut_out_lines */
/*
 * Write out the message.
 */
	sprintrmt (buf, fmt, SPRINTARGS);
	len = strlen (buf);
	if ((len + 3) < col)
	{
		strcpy (buf1, buf);
		bp = buf1 + len;
		for (ndash = col - len; ndash > 0; ndash--)
			*bp++ = '-';
		*bp++ = '^';
		*bp = '\0';
	}
	else
	{
		bp = buf1;
		for (ndash = 0; ndash < col; ndash++)
			*bp++ = '-';
		*bp++ = '^';
		*bp++ = ' ';
		strcpy (bp, buf);
		bp += len;
	}
	if (Errorbell)
	{
		*bp++ = '\007';
		*bp++ = '\0';
	}
	ui_ErrOut (buf1);
/*
 * Return to the top entry in the error jump stack, if called for.
 */
 	if (jump)
	{
		if (Stack)
			longjmp (Stack->js_value, TRUE);
		else
			c_panic ("No error handler specified");
	}
}


ui_ns_error (fmt, ARGS)
char *fmt;
int ARGS;
/*
 * Put out an error message, but don't actually signal it.
 */
{
	char buf[200], buf1[200];
/*
 * Write out the message.
 */
	sprintrmt (buf, fmt, SPRINTARGS);
	sprintf (buf1, Errorbell ? "*** %s\007" : "*** %s", buf);
	ui_ErrOut (buf1);
}



ui_bailout (fmt, ARGS)
char *fmt;
int ARGS;
/*
 * Give an informative message, then longjmp back to the last catch.
 */
{
	char buf[200], buf1[200];
/*
 * Write out the message.
 */
 	if (fmt)
	{
		sprintrmt (buf, fmt, SPRINTARGS);
		sprintf (buf1, "--> %s", buf);
		ui_WarnOut (buf1);
	}
/*
 * Jump through the error stack.
 */
	if (Stack)
		longjmp (Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}




ui_warning (fmt, ARGS)
char *fmt;
int ARGS;
/*
 * Put out a warning message.
 */
{
	char buf[200], buf1[200];

	sprintrmt (buf, fmt, SPRINTARGS);
	sprintf (buf1, "*** WARNING: %s", buf);
	ui_WarnOut (buf1);
}




err_push (label)
jmp_buf label;
{
	ui_epush (label);
}




ui_epush (label)
jmp_buf label;
/*
 * Push an error catch onto the stack.
 */
{
	struct jstack *jp;

	if (jp = Js_lal)
		Js_lal = Js_lal->js_next;
	else
		jp = (struct jstack *) getvm (sizeof (struct jstack));
	jp->js_value = label;
	jp->js_next = Stack;
	Stack = jp;
}



err_pop ()
{
	ui_epop ();
}




ui_epop ()
/*
 * Pull a value off the stack.
 */
{
	struct jstack *jp = Stack;

	if (! Stack)
		c_panic ("Popped an empty error stack");
	Stack = Stack->js_next;
	jp->js_next = Js_lal;
	Js_lal = jp;
}




bailout (fmt, ARGS)
char *fmt;
int ARGS;
/*
 * Give an informative message, then longjmp back to the last catch.
 */
{
	char buf[200];
/*
 * Write out the message.
 */
	if (fmt)
	{
		sprintrmt (buf, fmt, SPRINTARGS);
		printf ("\n\t--> %s\n", buf);
	}
/*
 * Return to the command interpreter.
 * (10/11/85 jc)	If there is an alternate jbuf on the stack, we will
 *			jump to that instead.
 * (10/xx/85 jc)	We no longer use J_buf at all, and just expect that
 *			somebody will have put something on the stack.
 */
	if (Stack)
		longjmp (Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}





sys_error (status, fmt, ARGS)
int status;
char *fmt;
int ARGS;
/*
 * Deal with a system error.  Like the normal "error" except that the
 * system error text is output first.
 */
{
/*
 * Put out the system error message.
 */
# ifdef VMS
	errmes (&status);
	printf ("\n");
# else
	printf ("System error status: %d\n", status);
# endif
/*
 * Now add the program text.
 */
	ui_error (fmt, ARGS);
}



warning (fmt, ARGS)
char *fmt;
int ARGS;
/*
 * Put out a warning message.
 */
{
	char buf[200];

	sprintrmt (buf, fmt, SPRINTARGS);
	printf ("\t*** WARNING:\t%s\n", buf);
}




err_resignal ()
{
	ui_eresignal ();
}




ui_eresignal ()
/*
 * Resignal a trapped error.  The effect of this routine is (1) to perform
 * an err_pop(), then to longjmp to the next entry in the error stack.
 */
{
	err_pop ();
	if (Stack)
		longjmp (Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}
