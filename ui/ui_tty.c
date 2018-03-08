/* $Id: ui_tty.c,v 1.15 1999-06-25 19:21:02 burghart Exp $ */
/*
 * Basic terminal handling.
 */

# include <unistd.h>
# include <stdlib.h>
# include <fcntl.h>
# include <termios.h>
# include <sys/ioctl.h>
# include <sys/time.h>
# include <sys/types.h>

# ifdef AIXV3 
# include <sys/select.h>
# endif

# ifdef __osf__
# include <sys/ioctl.h>
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

/*
 * special keys
 */
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


static bool Has_keypad = FALSE;

/*
 * The number of seconds we allow for arrow key sequences to arrive.
 */
# define TIMEOUT 1

/*
 * We need to be careful to restore the terminal to its
 * original state.  So, we keep one of these structures around.
 */
static struct termios Saved_params;
static struct termios Ui_params;

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
/*
 * Pull out the needed capabilities.
 */
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
/*
 * Standout and standin.
 */
/*
 * It appears that tgetstr under terminfo doesn't really stuff the string
 * into the buffer, but instead only returns the pointer.  So, we have to
 * copy ourselves.
 */
	tptr = TC_so;
	if (cp = tgetstr ("so", &tptr))
		strcpy (TC_so, cp);
	tptr = TC_se;
	if (cp = tgetstr ("se", &tptr))
		strcpy (TC_se, cp);
/*
 * Underlining.
 */
	tptr = TC_ul;
	if (cp = tgetstr ("us", &tptr))
		strcpy (TC_ul, cp);
	tptr = TC_ue;
	if (cp = tgetstr ("ue", &tptr))
		strcpy (TC_ue, cp);
/*
 * Check into application keypad mode.
 */
	tptr = TC_kp_appl;
	if (cp = tgetstr ("ks", &tptr))
	{
		strcpy (TC_kp_appl, cp);
		if (cp = tgetstr ("ke", &tptr))
			strcpy (TC_kp_num, cp);
		Has_keypad = TRUE;
	}
/*
 * Pull the special keys, if we can.
 */
	Has_arrows = FALSE;
 	for (key = 0; Sp_keys[key].spk_keycode; key++)
	{
		tptr = Sp_keys[key].spk_seq;
		if (! (cp = tgetstr (Sp_keys[key].spk_tcapcode, &tptr)))
			break;
		strcpy (Sp_keys[key].spk_seq, cp);
		Has_arrows = TRUE;
	}
/*
 * Set up our special params.
 */
	tty_setup ();
}


void
tty_setup ()
/*
 * Initialize the terminal the way we like it.
 */
{
	ioctl (TT_chan, TCGETS, &Ui_params);
	Saved_params = Ui_params;
	Ui_params.c_iflag &= ~(ICRNL | INLCR);
	Ui_params.c_oflag &= ~OPOST;
	Ui_params.c_cc[VMIN] = 1;
	Ui_params.c_cc[VTIME] = 0;
	Ui_params.c_lflag &= ~(ICANON | ECHO);
	ioctl (TT_chan, TCSETS, &Ui_params);
}


void
tty_return ()
/*
 * Return the terminal to its original state.
 */
{
	ioctl (TT_chan, TCSETS, &Saved_params);
}



void
tty_move (x, y)
int x, y;
/*
 * Move to location x, y.  The origin is assumed to be at 1,1
 */
{
	char *tgoto ();
	tty_putpad (tgoto (TC_cm, y-1, x-1));
}



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
		write (TT_chan, TT_obuf, TT_nch);
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

/*
 * If we have a timeout, we need to store it in the driver.
 */
	if (time)
	{
		Ui_params.c_cc[VTIME] = time*10;
		ioctl (TT_chan, TCSETS, &Ui_params);
	}
/*
 * Read until we're successful or we get an error other than EINTR
 */
	while ((status = read (TT_chan, &c, 1)) < 0 && errno == EINTR)
		/* nothing */;

	if (time) /* Restore immediately */
	{
		Ui_params.c_cc[VTIME] = 0;
		ioctl (TT_chan, TCSETS, &Ui_params);
	}
	if (status <= 0)
		return (0);

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
	ioctl (TT_chan, TCFLSH, 0);
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

