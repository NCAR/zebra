/* 2/87 jc */
/*
 * Interrupt handling.
 */
# ifdef UNIX
# include <signal.h>
# endif
# include "ui_param.h"

/*
 * This array holds addresses of functions to be called in the event
 * of an interrupt.
 */
# define MAXHANDLERS 10
typedef void (*vfptr) ();
static vfptr Handlers[MAXHANDLERS];
# define EMPTYSLOT	(vfptr) 0





uii_init ()
/*
 * Initialize the interrupt handling.
 */
{
	int hndl, uii_cc_handler ();
# ifdef SIGTSTP
	int uii_tstp ();
# endif
/*
 * No interrupts in batch jobs.
 */
	if (! ut_interactive ())
		return;
/*
 * Clear out the handlers array.
 */
 	for (hndl = 0; hndl < MAXHANDLERS; hndl++)
		Handlers[hndl] = EMPTYSLOT;
/*
 * Now set up the real interrupt handler.
 */
# ifdef UNIX
	signal (SIGINT, uii_cc_handler);
# ifdef SIGTSTP
	signal (SIGTSTP, uii_tstp);
# endif

# else
 	tty_setint (uii_cc_handler);
# endif
}




uii_cc_handler ()
/*
 * This is the ^C AST routine.
 */
{
	int hndl;
/*
 * Immediately re-establish this handler.
 */
# ifdef UNIX
	signal (SIGINT, uii_cc_handler);
# else
 	tty_setint (uii_cc_handler);
# endif
/*
 * Pass through the array of handlers, and call each one.
 */
 	for (hndl = 0; hndl < MAXHANDLERS; hndl++)
		if (Handlers[hndl] != EMPTYSLOT)
			(*Handlers[hndl]) ();
}




# ifdef SIGTSTP

uii_tstp ()
/*
 * Deal with a STOP signal.
 */
{
	int oldmask;

	tty_return ();	/* Terminal back to normal state */
	signal (SIGTSTP, SIG_DFL);
	oldmask = sigsetmask (0);
	kill (getpid (), SIGTSTP);
	/* ... */
	sigsetmask (oldmask);
	signal (SIGTSTP, uii_tstp);
	tty_setup ();
	ut_reline (); ut_do_reline ();
}


# endif




uii_set_handler (handler, last)
void (*handler) ();
bool last;
/*
 * Establish a handler to be called for a ^C interrupt.  Put it toward the
 * end iff LAST is true.
 */
{
	int hndl;
	
	if (! last)
	{
		for (hndl = 0; hndl < MAXHANDLERS; hndl++)
			if (Handlers[hndl] == EMPTYSLOT)
			{
				Handlers[hndl] = handler;
				return;
			}
	}
	else
	{
		for (hndl = MAXHANDLERS - 1; hndl >= 0; hndl--)
			if (Handlers[hndl] == EMPTYSLOT)
			{
				Handlers[hndl] = handler;
				return;
			}
	}
	c_panic ("Too many interrupt handlers established.");
}



uii_clear_handler (handler)
vfptr handler;
/*
 * Clear this interrupt handler.
 */
{
	int hndl;
	
	for (hndl = 0; hndl < MAXHANDLERS; hndl++)
		if (Handlers[hndl] == handler)
			Handlers[hndl] = EMPTYSLOT;
}
