/* 11/84 jc */
/*
 * Error handling.
 */
# include "ui_param.h"

# ifdef __STDC__
# include <stdarg.h>
# else
# include <varargs.h>
# endif

# include <setjmp.h>
# include "ui_symbol.h"

# ifdef UNIX
# ifndef SYSV
# define longjmp _longjmp
# endif /* SYSV */
# endif /* UNIX */


static char *rcsid = "$Id: ui_error.c,v 1.9 1994-11-10 17:32:55 case Exp $";
/*
 * Stack stuff.
 */
static struct jstack
{
	struct jstack *js_next;	/* Next entry in the stack	*/
#ifdef __hp9000s800  /* certain hps have to be different */
        double *js_value;
#else
	int *js_value;		/* The value we pushed		*/
#endif  

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





# ifdef __STDC__  /* Use the ANSI variable args list */



ui_error (char *fmt, ...)


/*
 * Put out an error message.
 */
{
	char buf[200], buf1[200];
        va_list args;

        va_start (args, fmt);


# else  /* Use K&R version for non-ANSI compilers */


ui_error ( va_alist )
va_dcl

{
        char buf[200], buf1[200];
        char *fmt;
        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* ifdef __STDC__ */

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
		longjmp (Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");

/*
 * We're done with the args list.
 */
        va_end (args);

}



#ifdef __STDC__  /* Use ANSI variable args */



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


# endif /* ifdef __STDC__ */

  
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
			longjmp (Stack->js_value, TRUE);
		else
			c_panic ("No error handler specified");
	}
}



# ifdef __STDC__  /* ANSI variable args */


ui_ns_error (char *fmt, ...)

/*
 * Put out an error message, but don't actually signal it.
 */
{
	char buf[200], buf1[200];
        va_list args;
        va_start (args, fmt);

# else  /* non-ANSI */

ui_ns_error ( va_alist )
va_dcl
{

        char buf[200], buf1[200];
        char *fmt;
        va_list args;
        va_start (args);
        fmt = va_arg (args, char *);

# endif /* ifdef __STDC __ */

/*
 * Write out the message.
 */
	vsprintf (buf, fmt, args);
	sprintf (buf1, Errorbell ? "*** %s\007" : "*** %s", buf);

        va_end (args);

	ui_ErrOut (buf1);
}



# ifdef __STDC__  /* ANSI variable args */


ui_bailout ( char *fmt, ...)

/*
 * Give an informative message, then longjmp back to the last catch.
 */
{
	char buf[200], buf1[200];
        va_list args;

        va_start (args, fmt);

# else  /* non-ANSI */


ui_bailout ( va_alist )
va_dcl

{
        char buf[200], buf1[200];
        char *fmt;
        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* ifdef __STDC__ */

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
		longjmp (Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}



# ifdef __STDC__  /* ANSI variable args */

ui_warning (char *fmt, ...)

/*
 * Put out a warning message.
 */
{
	char buf[200], buf1[200];
        va_list args;

        va_start (args, fmt);

# else /* non-ANSI */


ui_warning ( va_alist )
va_dcl
{
        char buf[200], buf1[200];
        char *fmt;
        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* ifdef __STDC__ */   

	vsprintf (buf, fmt, args);
        va_end (args);
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



# ifdef __STDC__  /* Use ANSI variable args */

bailout ( char *fmt, ...)


/*
 * Give an informative message, then longjmp back to the last catch.
 */
{
	char buf[200];
        va_list args;

        va_start (args, fmt);

# else  /* Use non-ANSI variable args */


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

# endif /* ifdef __STDC__ */

        
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
		longjmp (Stack->js_value, TRUE);
	else
		c_panic ("No error handler specified");
}




# ifdef __STDC__  /* Use ANSI variable args */

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

sys_error ( va_alist )
va_dcl

{

        va_list args;
        int status;
        char *fmt;

        va_start (args);
        status = va_arg (args, int);
        fmt = va_arg (args, char *);

# endif /* ifdef __STDC__ */

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
	vsprintf (buf, fmt, args);
	ui_error (buf);
        va_end (args);


}



# ifdef __STDC__    /* Use ANSI variable args */


warning (char *fmt, ...)

/*
 * Put out a warning message.
 */
{
	char buf[200];
        va_list args;

        va_start (args, fmt);

# else  /* non_ANSI */


warning ( va_alist )
va_dcl
{
        char buf[200];
        char *fmt;

        va_list args;

        va_start (args);
        fmt = va_arg (args, char *);

# endif /* ifdef __STDC__ */

        vsprintf (buf, fmt, args);

	printf ("\t*** WARNING:\t%s\n", buf);

        va_end (args);
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




