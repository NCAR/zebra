/* 11/84 jc */
/*
 * Error handling.
 */
# include "ui_param.h"

# if __STDC__
# include <stdarg.h>
# else
# include <varargs.h>
# endif

# include <setjmp.h>
# include <string.h>
# include "ui_symbol.h"


static char *rcsid =
   "$Id: ui_error.c,v 1.19 2000-02-14 22:50:00 burghart Exp $";
/*
 * Stack stuff.
 */
static struct jstack
{
	struct jstack *js_next;	/* Next entry in the stack	*/
	jmp_buf *js_value;
} *Stack = 0;

/*
 * Errorbell is true iff a bell is to be rung on errors.
 */
static bool Errorbell = TRUE;

/*
 * The lookaside list for jstack structures.
 */
static struct jstack *Js_lal = 0;


/*
 * Prototypes
 */




void
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





# if __STDC__  /* Use the ANSI variable args list */


void
ui_error (char *fmt, ...)
/*
 * Put out an error message.
 */
{
	char buf[200], buf1[200];
        va_list args;

        va_start (args, fmt);


# else  /* Use K&R version for non-ANSI compilers */

void
ui_error ( va_alist )
va_dcl
{
        char buf[200], buf1[200];
        char *fmt;
        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* __STDC__ */

/*
 * Write out the message.
 */

        vsprintf (buf, fmt, args);
	sprintf (buf1, Errorbell ? "*** %s\007" : "*** %s", buf);
	ui_ErrOut (buf1);
/*
 * Return to the command interpreter.
 * (10/11/85 jc)	If there is an alternate jbuf on the stack, we will
 *			jump to that instead.
 */
	if (Stack)
		longjmp (*Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");

/*
 * We're done with the args list.
 */
        va_end (args);

}



#if __STDC__  /* Use ANSI variable args */


void
ui_cl_error ( bool jump, int col, char *fmt, ...)

/*
 * Put out a command-line related error.
 * Entry:
 *	JUMP	is true if an error longjmp is to be executed after
 *		outputting the message.
 *	COL	is the column number of the error.
 *	FMT	is a printf-style format string
 *	...	is zero or more arguments to fill into FMT.
 * Exit:
 *	The message has been put out.
 */
{
	char buf[200], buf1[200], *bp;
	int len, ndash;
        va_list args;
        
        va_start (args, fmt);


# else  /*  Use K&R variable args list for non-ANSI compilers */

void
ui_cl_error ( va_alist )
va_dcl

/*
 * Put out a command-line related error.
 * Entry:
 *	JUMP	is true if an error longjmp is to be executed after
 *		outputting the message.
 *	COL	is the column number of the error.
 *	FMT	is a printf-style format string
 * VA_ALIST	is zero or more arguments to fill into FMT.
 * Exit:
 *	The message has been put out.
 */


{
        char buf[200], buf1[200], *bp;
        bool jump;
        char *fmt;
        int len, ndash, col;
        va_list args;

        va_start (args);

        jump = va_arg (args, bool);
        col = va_arg (args, int);
        fmt = va_arg (args, char *);


# endif /* __STDC__ */

  
/*
 * Get the command line echoed out.
 */
 	/* ut_out_lines (); */
	col += 7;	/* For extra garbage from ut_out_lines */
/*
 * Write out the message.
 */

        vsprintf (buf, fmt, args);
        va_end (args);

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
			longjmp (*Stack->js_value, TRUE);
		else
			c_panic ("No error handler specified");
	}
}



# if __STDC__  /* ANSI variable args */

void
ui_ns_error (char *fmt, ...)
/*
 * Put out an error message, but don't actually signal it.
 */
{
	char buf[200], buf1[200];
        va_list args;
        va_start (args, fmt);

# else  /* non-ANSI */

void
ui_ns_error ( va_alist )
va_dcl
{

        char buf[200], buf1[200];
        char *fmt;
        va_list args;
        va_start (args);
        fmt = va_arg (args, char *);

# endif /* __STDC __ */

/*
 * Write out the message.
 */
	vsprintf (buf, fmt, args);
	sprintf (buf1, Errorbell ? "*** %s\007" : "*** %s", buf);

        va_end (args);

	ui_ErrOut (buf1);
}



# if __STDC__  /* ANSI variable args */

void
ui_bailout ( char *fmt, ...)

/*
 * Give an informative message, then longjmp back to the last catch.
 */
{
	char buf[200], buf1[200];
        va_list args;

        va_start (args, fmt);

# else  /* non-ANSI */

void
ui_bailout ( va_alist )
va_dcl

{
        char buf[200], buf1[200];
        char *fmt;
        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* __STDC__ */

/*
 * Write out the message.
 */
 	if (fmt)
	{
		vsprintf (buf, fmt, args);
		sprintf (buf1, "--> %s", buf);
		ui_WarnOut (buf1);
	}

        va_end (args);
/*
 * Jump through the error stack.
 */
	if (Stack)
		longjmp (*Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}



# if __STDC__  /* ANSI variable args */

void
ui_warning (char *fmt, ...)

/*
 * Put out a warning message.
 */
{
	char buf[200], buf1[200];
        va_list args;

        va_start (args, fmt);

# else /* non-ANSI */

void
ui_warning ( va_alist )
va_dcl
{
        char buf[200], buf1[200];
        char *fmt;
        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* __STDC__ */   

	vsprintf (buf, fmt, args);
        va_end (args);
	sprintf (buf1, "*** WARNING: %s", buf);
	ui_WarnOut (buf1);
}



void
ui_epush (jmp_buf *label)
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



void
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



# if __STDC__  /* Use ANSI variable args */

void
bailout ( char *fmt, ...)


/*
 * Give an informative message, then longjmp back to the last catch.
 */
{
	char buf[200];
        va_list args;

        va_start (args, fmt);

# else  /* Use non-ANSI variable args */

void
bailout ( va_alist )
va_dcl

/*
 * Give an informative message, then longjmp back to the last catch.
 */
{
	char buf[200];
        char *fmt;
        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* __STDC__ */

        
/*
 * Write out the message.
 */
	if (fmt)
	{
		vsprintf (buf, fmt, args);
		printf ("\n\t--> %s\n", buf);
	}

        va_end (args);

/*
 * Return to the command interpreter.
 * (10/11/85 jc)	If there is an alternate jbuf on the stack, we will
 *			jump to that instead.
 * (10/xx/85 jc)	We no longer use J_buf at all, and just expect that
 *			somebody will have put something on the stack.
 */
	if (Stack)
		longjmp (*Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}




# if __STDC__  /* Use ANSI variable args */

void
sys_error (int status, char *fmt, ...)


/*
 * Deal with a system error.  Like the normal "error" except that the
 * system error text is output first.
 */

{

        va_list args;
	char buf[200];

        va_start (args, fmt);

# else   /* non-ANSI variable args */

void
sys_error ( va_alist )
va_dcl

{

        va_list args;
        int status;
        char *fmt;
	char buf[200];

        va_start (args);
        status = va_arg (args, int);
        fmt = va_arg (args, char *);

# endif /* __STDC__ */

/*
 * Put out the system error message.
 */
	printf ("System error status: %d\n", status);
/*
 * Now add the program text.
 */
	vsprintf (buf, fmt, args);
	ui_error (buf);
        va_end (args);


}



# if __STDC__    /* Use ANSI variable args */

void
warning (char *fmt, ...)

/*
 * Put out a warning message.
 */
{
	char buf[200];
        va_list args;

        va_start (args, fmt);

# else  /* non_ANSI */

void
warning ( va_alist )
va_dcl
{
        char buf[200];
        char *fmt;

        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* __STDC__ */

        vsprintf (buf, fmt, args);

	printf ("\t*** WARNING:\t%s\n", buf);

        va_end (args);
}



void
ui_eresignal ()
/*
 * Resignal a trapped error.  The effect of this routine is (1) to perform
 * a ui_epop(), then to longjmp to the next entry in the error stack.
 */
{
	ui_epop ();
	if (Stack)
		longjmp (*Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}


void
err_resignal ()
{
	ui_eresignal ();
}
