/* 2/87 jc */
static char *rcsid = "$Id: ui_interrupt.c,v 1.7 1996-01-02 02:48:21 granger Exp $";
/*
 * Interrupt handling.
 */
# include <sys/types.h>
# ifdef UNIX
# include <signal.h>
# endif
# include "ui_param.h"
# include "ui_globals.h"

# ifdef sgi
# define SIGNAL_RETURN void
# else
# define SIGNAL_RETURN int
# endif

/*
 * This array holds addresses of functions to be called in the event
 * of an interrupt.
 */
# define MAXHANDLERS 10
typedef void (*vfptr) ();
static vfptr Handlers[MAXHANDLERS];
# define EMPTYSLOT	(vfptr) 0




# ifdef UNIX

static SIGNAL_RETURN
uii_fault (sig, code, scp, addr)
int sig, code;
struct sigcontext *scp;
char *addr;
/*
 * Handle a seg/bus error.
 */
{
	printf ("\n\r%s: code %d addr 0x%x!\n\r",
		sig == SIGBUS ? "Bus error" : "Segmentation violation",
		code, addr);
	ui_finish ();	/* Restore tty	*/
	abort ();
}

# endif


uii_init ()
/*
 * Initialize the interrupt handling.
 */
{
	int hndl;
	SIGNAL_RETURN uii_cc_handler ();
# ifdef SIGTSTP
	SIGNAL_RETURN uii_tstp ();
# endif
# ifdef UNIX
	void (*oldh)();
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
 * (4/89 jc)	Also set up catches for seg fault and bus error signals,
 *		so that we can clean up before dying.
 */
# ifdef UNIX
	signal (SIGINT, uii_cc_handler);
	if ((oldh = signal (SIGBUS, uii_fault)) != SIG_DFL)
		signal (SIGBUS, oldh);
	if ((oldh = signal (SIGSEGV, uii_fault)) != SIG_DFL)
		signal (SIGSEGV, oldh);
# ifdef SIGTSTP
	signal (SIGTSTP, uii_tstp);
# endif

# else
 	tty_setint (uii_cc_handler);
# endif
}



SIGNAL_RETURN
uii_cc_handler ()
/*
 * This is the ^C AST routine.
 */
{
	int hndl;

# ifdef BSD 
/*
 * Under BSD, we should go ahead and unblock interrupts, in case one of
 * the handlers longjmps out from under us.
 */
	int mask = sigblock (0);
	mask &= ~(sigmask (SIGINT));
	sigsetmask (mask);
# endif

# ifdef SYSV

/* SYSV kindly resets the handler to SIG_DFL before it executes our handler
 * so we have to reset the signal to make sure it will work right the next 
 * time we want it to
 */   
        signal (SIGINT, uii_cc_handler);
# endif

# ifdef VMS
/*
 * VMS handlers need to be reestablished each time.
 */
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

SIGNAL_RETURN
uii_tstp ()
/*
 * Deal with a STOP signal.
 */
{
#ifndef SVR4 
	int oldmask;
#else
        sigset_t oldmask;
#endif 
	tty_kpoff ();
	tty_return ();	/* Terminal back to normal state */
	signal (SIGTSTP, SIG_DFL);

#ifndef SVR4 
	oldmask = sigsetmask (0);
#else
        sigprocmask (SIG_SETMASK, NULL, &oldmask);
#endif
       
	kill (getpid (), SIGTSTP);
	/* ... */

#ifndef SVR4 
	sigsetmask (oldmask);
#else
        sigprocmask (SIG_SETMASK, &oldmask, NULL);
#endif
        
	signal (SIGTSTP, uii_tstp);
	tty_setup ();
	if (Keypad_on)
		tty_kpon ();
	ut_reline (); ut_do_reline (0);
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



