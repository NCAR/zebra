/* $Id: ui_tty.c,v 1.14 1999-03-25 22:23:45 burghart Exp $ */
/*
 * Basic terminal handling.  This is an extremely VMS-dependant module.
 */

# include <stdlib.h>

# ifdef SYSV
# ifdef AIXV3 
# include <sys/select.h>
# endif

# include <fcntl.h>
# include <termio.h>
# ifdef linux
# include <sys/time.h>	/* for FD_ set macros */
# endif
# endif

# ifdef __osf__
# include <sys/ioctl.h>
# endif

# ifdef BSD
# include <sys/types.h>
# include <sys/fcntl.h>
# include <termios.h>
# endif

/*
 * Implementation of X watching...currently BSD only.
 */
/* # ifdef BSD */
# ifdef XSUPPORT
# define MAXFD 10
static int Nfd = 0;	/* Number of FD's to watch	*/
typedef void (*void_function_pointer) ();
static int Fds[MAXFD];
static void_function_pointer Fd_funcs [MAXFD];
# endif
/* # endif */

# include <ctype.h>
# include <errno.h>
# include "ui.h"
# include "ui_tty.h"

# define TC_BUF_SIZ 20
static char TC_cl_eol[TC_BUF_SIZ];	/* Clear to end of line string */
static char TC_cl_eos[TC_BUF_SIZ];	/* Clear to end of screen	*/
static char TC_cm[TC_BUF_SIZ];		/* Cursor motion string	*/
static char TC_so[TC_BUF_SIZ];		/* Enter standout mode string */
static char TC_se[TC_BUF_SIZ];		/* Exit standout mode */
static char TC_ul[TC_BUF_SIZ];		/* Start underlining	*/
static char TC_ue[TC_BUF_SIZ];		/* Stop underlining	*/
static char TC_dsc[TC_BUF_SIZ];		/* Down scroll	*/
static char TC_home[TC_BUF_SIZ];	/* Home position */
static char TC_cs[TC_BUF_SIZ];		/* Change scroll region */
static char TC_left_arrow[TC_BUF_SIZ];	/* Arrow keys		*/
static char TC_right_arrow[TC_BUF_SIZ];
static char TC_up_arrow[TC_BUF_SIZ];
static char TC_down_arrow[TC_BUF_SIZ];
static char TC_kp_appl[TC_BUF_SIZ];	/* Keypad application	*/
static char TC_kp_num[TC_BUF_SIZ];	/* Back to numeric mode.	*/
/*
 * Information relating to arrow and other special keys.
 */
# define N_SPECIAL_KEY 30
# ifdef VMS
struct spkey
{
	int	spk_smgcode;		/* The SMG code for this key	*/
	int	spk_keycode;		/* Key code we return		*/
	char	spk_name[12];		/* Special key name		*/
	char	spk_seq[TC_BUF_SIZ];	/* The key's escape sequence	*/
} Sp_keys[N_SPECIAL_KEY] =
{
	{ SMG$K_KEY_DOWN_ARROW,		DOWN_ARROW,	"Down arrow",	___ },
	{ SMG$K_KEY_UP_ARROW,		UP_ARROW,	"Up arrow",	___ },
	{ SMG$K_KEY_LEFT_ARROW,		LEFT_ARROW,	"Left arrow",	___ },
	{ SMG$K_KEY_RIGHT_ARROW,	RIGHT_ARROW,	"Right arrow",	___ },
	{ SMG$K_KEY_PF1,		K_PF1,		"pf1",		___ },
	{ SMG$K_KEY_PF2,		K_PF2,		"pf2",		___ },
	{ SMG$K_KEY_PF3,		K_PF3,		"pf3",		___ },
	{ SMG$K_KEY_PF4,		K_PF4,		"pf4",		___ },
	{ SMG$K_KEY_0,			K_0,		"0",		___ },
	{ SMG$K_KEY_1,			K_1,		"1",		___ },
	{ SMG$K_KEY_2,			K_2,		"2",		___ },
	{ SMG$K_KEY_3,			K_3,		"3",		___ },
	{ SMG$K_KEY_4,			K_4,		"4",		___ },
	{ SMG$K_KEY_5,			K_5,		"5",		___ },
	{ SMG$K_KEY_6,			K_6,		"6",		___ },
	{ SMG$K_KEY_7,			K_7,		"7",		___ },
	{ SMG$K_KEY_8,			K_8,		"8",		___ },
	{ SMG$K_KEY_9,			K_9,		"9",		___ },
	{ SMG$K_KEY_COMMA,		K_COMMA,	"comma",	___ },
	{ SMG$K_KEY_ENTER,		K_ENTER,	"enter",	___ },
	{ SMG$K_KEY_MINUS,		K_MINUS,	"minus",	___ },
	{ SMG$K_KEY_PERIOD,		K_PERIOD,	"period",	___ },
	{ ___,				___,		___,		___ },
};
# endif

/*
 * The unix variety.  Termcap doesn't really understand the vt100 keypad
 * structure, so a lot of those keys aren't represented here.
 */
# ifdef UNIX
struct spkey
{
	char 	*spk_tcapcode;		/* The termcap code for this key*/
	int	spk_keycode;		/* Key code we return		*/
	char	spk_name[12];		/* Special key name		*/
	char	spk_seq[TC_BUF_SIZ];	/* The key's escape sequence	*/
} Sp_keys[N_SPECIAL_KEY] =
{
	{ "kd",		DOWN_ARROW,	"Down arrow",	___ },
	{ "ku",		UP_ARROW,	"Up arrow",	___ },
	{ "kl",		LEFT_ARROW,	"Left arrow",	___ },
	{ "kr",		RIGHT_ARROW,	"Right arrow",	___ },
	{ "k1",		K_PF1,		"pf1",		___ },
	{ "k2",		K_PF2,		"pf2",		___ },
	{ "k3",		K_PF3,		"pf3",		___ },
	{ "k4",		K_PF4,		"pf4",		___ },
	{ ___,		___,		___,		___ },
};
# endif

/*
 * Input info.
 */
static short TT_chan;		/* Channel to the terminal */
static char I_buf[100];		/* Input buffer.	*/
static char *Ibp = 0;		/* Pointer into I_buf	*/
static int TT_ef = 0;		/* Event flag.		*/

/*
 * Terminal output buffer.
 */
# define TT_BUF_SIZE 1024
static char TT_obuf[TT_BUF_SIZE];
static unsigned int TT_nch = 0;

# ifdef VMS
/*
 * The type number for the terminal.
 */
static int TT_type = 0;
static int TT_addr;

/*
 * SS macros.
 */
# define ___ 0
# define error(s) (! (s & 0x1))

/*
 * Exit handler control block description.
 */
static struct exhblk
{
	struct exhblk *eb_flink;	/* Forward link		*/
	int (*eb_handler) ();		/* Address of exit handler */
	int eb_argc;			/* Arg count		*/
	int *eb_cond;			/* Where to write the condition */
} Exh;
# endif

static bool Has_keypad = FALSE;

/*
 * The number of seconds we allow for arrow key sequences to arrive.
 * VMS seems to interpret a one second timeout as 0 to 1 seconds, with
 * the result that even at 9600 baud, the three characters of a vt100
 * arrow key do not arrive within a one second timeout period.  Thus,
 * we must use 2...
 */
# ifdef VMS
# define TIMEOUT 2
# else
# define TIMEOUT 1
# endif

/*
 * In unix land, we need to be careful to restore the terminal to its
 * original state.  So, we keep one of these structures around.
 */
# ifdef SYSV
static struct termio Saved_params;
static struct termio Ui_params;
# endif

# ifdef BSD
static struct termios Saved_params;
static struct termios Ui_params;
# endif

static bool Has_arrows;
static int N_lines = 24;	/* How many lines on the screen.	*/

/*
 * Prototypes 
 */
void tty_set (void);
void tty_setup (void);
void tty_flush (void);
void tty_putpad (char *str);
void tty_standout (void);
void tty_standin (void);
void tty_ul (void);
void tty_ulend (void);




void
tty_set ()
/*
 * Set up the terminal capabilities.
 */
{
	char tt_buf[1024], *tptr;
	char *cp, *tgetstr();
	int status, ttchar[3], len, key;
# ifdef VMS
/*
 * Assign a channel to the terminal.
 */
	status = sys$assign (descr ("TT"), &TT_chan, ___, ___);
	if (error (status))
	{
		printf ("Unable to get terminal channel");
		exit (status);
	}
	lib$get_ef (&TT_ef);
/*
 * See if we understand tabs, and get the terminal type.
 */
	status = sys$qiow (___, TT_chan, IO$_SENSEMODE, ___, ___, ___,
			   ttchar, 12, ___, ___, ___, ___);
	/* Has_tabs = (ttchar[1] & TT$M_MECHTAB) != 0; */
/*
 * Get the terminal type.
 */
	cp = (char *) ttchar;
	TT_type = cp[1];
/*
 * Get the SMG terminal table ready.
 */
	status = smg$init_term_table_by_type (&TT_type, &TT_addr);
	if (error (status))
	{
		printf ("Unable to get termtable info\n");
		exit (status);
	}
# else
/*
 * Open a terminal channel.
 */
	if ((TT_chan = open ("/dev/tty", O_RDWR)) < 0)
	{
		perror ("/dev/tty");
		c_panic ("Unable to open terminal channel");
	}
/*
 * Lookup our terminal.
 */
	if (tgetent (tt_buf, getenv ("TERM")) < 1)
		ui_error ("Unable to get termcap for term type '%s'", 
			getenv ("TERM"));
# endif
/*
 * Pull out the needed capabilities.
 */
# ifdef notdef
	status = smg$get_term_data (&TT_addr, &SMG$K_ERASE_TO_END_LINE,
			&TC_BUF_SIZ, &len, TC_cl_eol);
	if (error (status))
		syserr (status, "No ce");
	TC_cl_eol[len] = 0;
	/* Bum_clear = ! TC_cl_eol[0]; */
# endif
# ifdef VMS
/*
 * Clear to end of screen.
 */
	status = smg$get_term_data (&TT_addr, &SMG$K_ERASE_WHOLE_DISPLAY,
			&TC_BUF_SIZ, &len, TC_cl_eos);
	if (error (status))
		syserr (status, "No cl");
	TC_cl_eos[len] = 0;
/*
 * Home cursor.
 */
	status = smg$get_term_data (&TT_addr, &SMG$K_HOME, &TC_BUF_SIZ, &len,
			TC_home);
	if (error (status))
		syserr (status, "No ho");
	TC_home[len] = 0;
# endif
# ifdef UNIX
	tptr = TC_home;
	if (cp = tgetstr ("ho", &tptr))
		strcpy (TC_home, cp);
	tptr = TC_cl_eos;
	if (cp = tgetstr ("cl", &tptr))
		strcpy (TC_cl_eos, cp);
	tptr = TC_cm;
	if (cp = tgetstr ("cm", &tptr))
		strcpy (TC_cm, cp);
	N_lines = tgetnum ("li");
# endif
/*
 * Standout and standin.
 */
# ifdef VMS
	status = smg$get_term_data (&TT_addr, &SMG$K_BEGIN_REVERSE, &TC_BUF_SIZ,
			&len, TC_so);
	if (error (status))
		syserr (status, "No so");
	TC_so[len] = 0;
# else
/*
 * It appears that tgetstr under terminfo doesn't really stuff the string
 * into the buffer, but instead only returns the pointer.  So, we have to
 * copy ourselves.
 */
	tptr = TC_so;
	if (cp = tgetstr ("so", &tptr))
		strcpy (TC_so, cp);
# endif
# ifdef VMS
	status = smg$get_term_data (&TT_addr, &SMG$K_END_REVERSE, &TC_BUF_SIZ,
			&len, TC_se);
	if (error (status))
		syserr (status, "No se");
	TC_se[len] = 0;
# else
	tptr = TC_se;
	if (cp = tgetstr ("se", &tptr))
		strcpy (TC_se, cp);
# endif
/*
 * Underlining.
 */
# ifdef VMS
	status = smg$get_term_data (&TT_addr, &SMG$K_BEGIN_UNDERSCORE,
			&TC_BUF_SIZ, &len, TC_ul);
	TC_ul[len] = '\0';
	status = smg$get_term_data (&TT_addr, &SMG$K_END_UNDERSCORE, 
			&TC_BUF_SIZ, &len, TC_ue);
	TC_ue[len] = '\0';
# else
	tptr = TC_ul;
	if (cp = tgetstr ("us", &tptr))
		strcpy (TC_ul, cp);
	tptr = TC_ue;
	if (cp = tgetstr ("ue", &tptr))
		strcpy (TC_ue, cp);
# endif
/*
 * Check into application keypad mode.
 */
# ifdef VMS
 	status = smg$get_term_data (&TT_addr, &SMG$K_SET_APPLICATION_KEYPAD,
		&TC_BUF_SIZ, &len, TC_kp_appl);
	if (len > 0)	/* We actually have a keypad... */
	{
		TC_kp_appl[len] = '\0';
		Has_keypad = TRUE;
	 	status = smg$get_term_data(&TT_addr, &SMG$K_SET_NUMERIC_KEYPAD,
			&TC_BUF_SIZ, &len, TC_kp_num);
		tty_set_exh ();
	}
# endif
# ifdef UNIX
	tptr = TC_kp_appl;
	if (cp = tgetstr ("ks", &tptr))
	{
		strcpy (TC_kp_appl, cp);
		if (cp = tgetstr ("ke", &tptr))
			strcpy (TC_kp_num, cp);
		Has_keypad = TRUE;
	}
# endif
/*
 * Pull the special keys, if we can.
 */
	Has_arrows = FALSE;
 	for (key = 0; Sp_keys[key].spk_keycode; key++)
	{
# ifdef VMS
		status = smg$get_term_data (&TT_addr,&Sp_keys[key].spk_smgcode,
			 &TC_BUF_SIZ, &len, Sp_keys[key].spk_seq);
		if (error (status))
			break;
# else
		tptr = Sp_keys[key].spk_seq;
		if (! (cp = tgetstr (Sp_keys[key].spk_tcapcode, &tptr)))
			break;
		strcpy (Sp_keys[key].spk_seq, cp);
# endif
		Has_arrows = TRUE;
	}
# ifdef UNIX
/*
 * Set up our special params.
 */
	tty_setup ();
# endif
}


void
tty_setup ()
/*
 * Initialize the terminal the way we like it.
 */
{
# ifdef SYSV
	ioctl (TT_chan, TCGETA, &Ui_params);
	Saved_params = Ui_params;
	Ui_params.c_iflag &= ~(ICRNL | INLCR);
	Ui_params.c_oflag &= ~OPOST;
	Ui_params.c_cc[VMIN] = 1;
	Ui_params.c_cc[VTIME] = 0;
	Ui_params.c_lflag &= ~(ICANON | ECHO);
	ioctl (TT_chan, TCSETA, &Ui_params);
# endif

# ifdef BSD
	ioctl (TT_chan, TCGETS, &Ui_params);
	Saved_params = Ui_params;
	Ui_params.c_iflag &= ~(ICRNL | INLCR);
	Ui_params.c_oflag &= ~OPOST;
	Ui_params.c_cc[VMIN] = 1;
	Ui_params.c_cc[VTIME] = 0;
	Ui_params.c_lflag &= ~(ICANON | ECHO);
	ioctl (TT_chan, TCSETS, &Ui_params);
# endif
}


void
tty_return ()
/*
 * Return the terminal to its original state.
 */
{
# ifdef SYSV
	ioctl (TT_chan, TCSETA, &Saved_params);
# endif

# ifdef BSD
	ioctl (TT_chan, TCSETS, &Saved_params);
# endif
}



void
tty_move (x, y)
int x, y;
/*
 * Move to location x, y.  The origin is assumed to be at 1,1
 */
{
# ifdef VMS
	char buf[80];
	int status, len, argvec[3];
/*
 * Generate the motion string.
 */
	argvec[0] = 2;
	argvec[1] = x; argvec[2] = y;
	status = smg$get_term_data (&TT_addr, &SMG$K_SET_CURSOR_ABS,
			&80, &len, buf, argvec);
	if (error (status))
		syserr (status, "No cm");
	tty_out (buf, len);
# endif
# ifdef UNIX
	char *tgoto ();
	tty_putpad (tgoto (TC_cm, y-1, x-1));
# endif
}


# ifdef notdef

tty_cs (top, bottom)
int top, bottom;
/*
 * Change the scrolling region to be from top to bottom, inclusive.  One
 * should be sure that the terminal supports this before trying it.
 */
{
	char buf[80];
	int status, len, argvec[3];
/*
 * Generate the motion string.
 */
	argvec[0] = 2;
	argvec[1] = top; argvec[2] = bottom;
	status = smg$get_term_data (&TT_addr, &SMG$K_SET_SCROLL_REGION,
			&80, &len, buf, argvec);
	if (error (status))
		syserr (status, "No cs");
/*
 * Put it out.
 */
	tty_out (buf, len);
}

# endif





void
tty_out (cp, len)
char *cp;
int len;
/*
 * Output "len" chars that start at "cp".
 */
{
	char *bp;
/*
 * Check for buffer overflow.
 */
	if (TT_nch + len > TT_BUF_SIZE)
		tty_flush ();
/*
 * Now copy the stuff.
 */
	for (bp = cp; bp < cp + len; bp++)
		TT_obuf[TT_nch++] = *bp & 0x7f;
}



void
tty_sout (cp)
char *cp;
/*
 * String output.  This routine puts the given data out to the screen,
 * with reverse video and underline processing.
 */
{
	char *bp, rv = 0, ul = 0;
	int len;
/*
 * Check for buffer overflow.
 */
	if (TT_nch + (len = strlen (cp)) > TT_BUF_SIZE)
		tty_flush ();
/*
 * Now copy the stuff.
 */
	for (bp = cp; bp < cp + len; bp++)
	{
		switch (*bp)
		{
		   case '`':
			if (rv)
				tty_standin ();
			else
				tty_standout ();
			rv = ! rv;
			break;
		   case '~':
			if (ul)
				tty_ulend ();
			else
				tty_ul ();
			ul = ! ul;
			break;
		   default:
			TT_obuf[TT_nch++] = *bp & 0x7f;
		}
	}
}


void
tty_flush ()
/*
 * Flush the output buffer.
 */
{
	short iosb[4];
	int status;
/*
 * See if there is truly data to write.
 */
	if (TT_nch)
	{
# ifdef VMS
	/*
	 * Do the write.
	 */
		status = sys$qiow (TT_ef, TT_chan,
			IO$_WRITEVBLK | IO$M_NOFORMAT,
			iosb, ___, ___, TT_obuf, TT_nch, ___, ___, ___, ___);
	/*
	 * Make sure that it worked.
	 */
		if (error (status))
		{
			printf ("Terminal write error");
			errmes (&status);
			exit (1);
		}
	/*
	 * Gotta check IOSB too.
	 */
		status = iosb[0];
		if (error (status) && status)
		{
			printf ("Iosb error");
			errmes (&status);
			exit (1);
		}
# else
		write (TT_chan, TT_obuf, TT_nch);
# endif
	/*
	 * Reset the count.
	 */
		TT_nch = 0;
	}
}




void
tty_drain ()
/*
 * Throw away the terminal output buffer.
 */
{
	TT_nch = 0;
}


void
tty_putpad (str)
char *str;
/*
 * Put out the string with padding.  For now we just trim the
 * padding and hope for the best.
 */
{
	while (isdigit (*str))
		str++;
	tty_out (str, strlen (str));
}



void
tty_home ()
/*
 * Position the cursor to 1,1.
 */
{
	tty_putpad (TC_home);
}


void
tty_down_scroll ()
/*
 * Open up one line at the top of the screen.
 */
{
	tty_home ();
	tty_putpad (TC_dsc);
}


void
tty_standout ()
/*
 * Enter standout mode.
 */
{
	tty_putpad (TC_so);
}


void
tty_standin ()
/*
 * Leave standout mode.
 */
{
	tty_putpad (TC_se);
}

void
tty_ul ()
/*
 * Start underlining.
 */
{
	tty_putpad (TC_ul);
}

void
tty_ulend ()
/*
 * Stop underlining.
 */
{
	tty_putpad (TC_ue);
}



void
tty_clear ()
/*
 * Clear the screen.
 */
{
	tty_home ();
	tty_putpad (TC_cl_eos);
	/* L_bottom = 0; */
}


void
tty_clear_l ()
/*
 * Clear the current line.
 */
{
	tty_putpad (TC_cl_eol);
}


# ifdef VMS

tty_setint (handler)
int (*handler) ();
/*
 * Enable the ^C interrupt.
 */
{
	int status;
	short iosb[4];
	extern int int_handler ();

	status = sys$qiow (TT_ef, TT_chan, IO$_SETMODE | IO$M_CTRLCAST,
		iosb, ___, ___, handler, ___, 3, ___, ___, ___);
	if (error (status))
	{
		printf ("Unable to enable ^C AST");
		errmes (&status);
		exit (1);
	}
	status = iosb[0];
	if (error (status))
	{
		printf ("^C iosb error");
		errmes (&status);
		exit (1);
	}
}

# endif



char
tty_do_read (time)
int time;
/*
 * Read and return a character from the terminal.  If TIME is nonzero, a
 * read-with-timeout will be done.  Zero is returned for timeout errors.
 */
{
	int status, iofunc;
	short iosb[4];
	char c;
/*
 * Look for buffered info.
 */
 	if (Ibp)
	{
		c = *Ibp++;
		if (*Ibp == '\0')
			Ibp = 0;
		return (c);
	}
/*
 * Nope, gotta read something.
 */
# ifdef VMS
	iofunc = time ?
		IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR | IO$M_TIMED :
		IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR;
	status = sys$qiow (TT_ef, TT_chan, iofunc, iosb, ___, ___,
			   &c, 1, time, ___, ___, ___);
/*
 * Check the return status.
 */
	if (error (status))
	{
		printf ("Read error");
		errmes (&status);
		exit (1);
	}
/*
 * Also check the I/O status block.
 */
	if (iosb[0] == SS$_TIMEOUT)
		return (0);
	status = iosb[0];
	if (error (status))
	{
		if (status == 0)
			return ('\r');
		printf ("Read iosb error");
		errmes (&status);
		exit (1);
	}
	if (status == SS$_CONTROLC || status == SS$_CONTROLY)
		return ('\025');	/* Line kill */
# else
/*
 * If we have a timeout, we need to store it in the driver.
 */
	if (time)
	{
		Ui_params.c_cc[VTIME] = time*10;
# ifdef SYSV
		ioctl (TT_chan, TCSETA, &Ui_params);
# else
		ioctl (TT_chan, TCSETS, &Ui_params);
# endif
	}
/*
 * Read until we're successful or we get an error other than EINTR
 */
	while ((status = read (TT_chan, &c, 1)) < 0 && errno == EINTR)
		/* nothing */;

	if (time) /* Restore immediately */
	{
		Ui_params.c_cc[VTIME] = 0;
# ifdef SYSV
		ioctl (TT_chan, TCSETA, &Ui_params);
# else
		ioctl (TT_chan, TCSETS, &Ui_params);
# endif
	}
	if (status <= 0)
		return (0);
# endif
	return (c);
}




unsigned char
tty_readch ()
/*
 * Perform a one-char terminal read, with arrow key recognition.
 */
{
	char c;
	int i, nset;
	
# ifdef XSUPPORT
	if (Nfd > 0)
	{
		fd_set fset, ret;
		int width;
		int tt_sel_ch = TT_chan;

		FD_ZERO (&ret);
		while (Nfd > 0 && ! FD_ISSET (tt_sel_ch, &ret))
		{
		/*
		 * Set up the file descriptor list.
		 */
			FD_ZERO (&fset);
			FD_SET (tt_sel_ch, &fset);
			width = tt_sel_ch;
			for (i = 0; i < Nfd; i++)
			{
				FD_SET (Fds[i], &fset);
				if (Fds[i] > width)
					width = Fds[i];
			}
			width++;
			ret = fset;
		/*
		 * Wait until we get a successful select or an error other
		 * than EINTR.
		 */
			while ((nset = select (width, &ret, 0, 0, 0)) < 0 &&
				errno == EINTR)
				/* nothing */;
		/*
		 * Dispatch any incoming stuff.
		 */
		 	for (i = 0; i < Nfd && nset > 0; i++)
				if (FD_ISSET (Fds[i], &ret))
				{
					(*Fd_funcs[i]) (Fds[i]);
					nset--;
				}
		}
	}
# endif
	c = tty_do_read (0);
	
	return (Has_arrows ? tty_map (c) : c);
}





int
tty_map (c)
int c;
/*
 * Attempt to map the character "c" onto an arrow key
 * sequence.
 */
{
	char sequence[10], *sp;
	int map, tty_partmap ();
/*
 * See if this first character maps onto a partial sequence.
 */
	sequence[0] = c; sequence[1] = 0; sp = sequence + 1;
	if ((map = tty_partmap (sequence)) == 0)
		return (c);	/* nope. */
/*
 * Fill out the sequence.
 */
	while (map == -1)
	{
	/*
	 * Get the next character.  If it does not arrive quickly,
	 * give up on the sequence and return the first char.
	 */
		if ((*sp = tty_do_read (TIMEOUT)) == 0)
		{
			if (sequence[1])
			{
				strcpy (I_buf, sequence + 1);
				Ibp = I_buf;
			}
			return (c);
		}
	/*
	 * See if we have a full sequence now.
	 */
		*++sp = 0;
		map = tty_partmap (sequence);
	}
/*
 * Well, we dropped out of the loop.  See what happened.
 */
	if (map == 0)
	{
		if (sequence[1])
		{
			strcpy (I_buf, sequence + 1);
			Ibp = I_buf;
		}
		return (c);
	}
	else
		return (Sp_keys[map - 1].spk_keycode);
}


void
tty_bell ()
{
	static char bell = '\007';
	tty_out (&bell, 1);
}



int
tty_partmap (string)
char *string;
/* 
 * Attempt to map the given string onto an arrow key string.
 */
{
	int len = strlen (string), key;
/*
 * This is a nasty brute force algorithm, but it works.
 */
	for (key = 0; Sp_keys[key].spk_keycode; key++)
	{
		if (! strncmp (string, Sp_keys[key].spk_seq, len))
			return (strcmp (string, Sp_keys[key].spk_seq) ?
						-1 : key + 1);
	}
	return (0);
}



void
tty_zap_ta ()
/*
 * Clear out the typeahead buffer.
 */
{
	int status, kludge;
	short iosb[4];
# ifdef VMS
	status = sys$qiow (TT_ef, TT_chan,
		IO$_READVBLK | IO$M_PURGE | IO$M_TIMED, iosb, ___, ___,
		&kludge, 0, 0, ___, ___, ___);
# endif
# ifdef UNIX
	ioctl (TT_chan, TCFLSH, 0);
# endif
}



void
tty_dump_keys ()
/*
 * Dump out the special key database.
 */
{
	int key;
	char *cp;

	for (key = 0; Sp_keys[key].spk_keycode; key++)
	{
		ui_nf_printf ("%d, key %x, name '%s', seq: '", key,
			Sp_keys[key].spk_keycode, Sp_keys[key].spk_name);
		for (cp = Sp_keys[key].spk_seq; *cp; cp++)
			if (*cp < ' ')
				ui_nf_printf ("^%c", *cp | 0x40);
			else
				ui_nf_printf ("%c", *cp);
		ui_printf ("'\n");
	}
}




char *
tty_get_key_name (key)
int key;
/*
 * Return the name for this key code.
 */
{
	int k;
	
	for (k = 0; Sp_keys[k].spk_keycode; k++)
		if (Sp_keys[k].spk_keycode == key)
			return (Sp_keys[k].spk_name);
	return ("Bogus key");
}



int
tty_get_key_code (name)
char *name;
/*
 * Return the code for this key.
 */
{
	int k;
	
	for (k = 0; Sp_keys[k].spk_keycode; k++)
		if (! strcmp (name, Sp_keys[k].spk_name))
			return (Sp_keys[k].spk_keycode);
	return (0);
}


# ifdef VMS

tty_set_exh ()
/*
 * Set up our exit handler.
 */
{
	int tty_exit_handler (), status;
	static int Cond;	/* waste */

	Exh.eb_handler = tty_exit_handler;
	Exh.eb_argc = 0;
	Exh.eb_cond = &Cond;
	status = sys$dclexh (&Exh);
	if (error (status))
		syserr (status, "TTY set exit handler");
}


int
tty_exit_handler ()
/*
 * The actual exit handler.
 */
{
	tty_out (TC_kp_num, strlen (TC_kp_num));
	tty_flush ();
}

# endif



void
tty_string (string)
char *string;
/*
 * Output this string.
 */
{
	tty_out (string, strlen (string));
}


void
tty_kpon ()
/*
 * Turn the keypad over to application mode.
 */
{
	if (Has_keypad)
	{
		tty_out (TC_kp_appl, strlen (TC_kp_appl));
		tty_flush ();
	}
}


void
tty_kpoff ()
/*
 * Back to numeric mode.
 */
{
	if (Has_keypad)
	{
		tty_string (TC_kp_num);
		tty_flush ();
	}
}



int
tty_nlines ()
/*
 * Return the number of lines on the terminal screen.
 */
{
	return (N_lines ? N_lines : 24);
}




# ifdef XSUPPORT

void
tty_watch (fd, func)
int fd;
void_function_pointer func;
/*
 * Watch this FD for events.
 */
{
/*
 * Make sure we have room.
 */
	if (Nfd >= MAXFD)
		ui_error ("(BUG) FD table overflow");
/* 
 * Add this function.
 */
	Fds[Nfd] = fd;
	Fd_funcs[Nfd++] = func;
}


void
tty_nowatch (fd)
int fd;
/*
 * Stop watching.
 */
{
	int i;
/*
 * Find this file descriptor.
 */
	for (i = 0; i < Nfd; i++)
		if (Fds[i] == fd)
			break;
	if (i >= Nfd)
		c_panic ("Nowatch on unwatched FD %d\n", fd);
/*
 * Remove it.
 */
	for (; i < Nfd - 1; i++)
	{
		Fds[i] = Fds[i + 1];
		Fd_funcs[i] = Fd_funcs[i + 1];
	}
	Nfd--;
}

# endif

