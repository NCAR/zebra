/*
 * tty_ routines for the CRAY.  All of the fancy I/O has been changed
 * into simple stuff.  Many of the routines are just dummies.
 */

# define TRUE	-1
# define FALSE	0

/*
 * Terminal output buffer.
 */
# define TT_BUF_SIZE 1024
static char TT_obuf[TT_BUF_SIZE];
static int TT_nch = 0;




tty_set ()
/*
 * Dummy set-up.
 */
{
	return (TRUE);
}




tty_move (x, y)
int x, y;
/*
 * Dummy move.
 */
{
	return (TRUE);
}


tty_cs (top, bottom)
int top, bottom;
/*
 * Scrolling region on the CRAY?  Get real.
 */
{
	return (TRUE);
}




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



tty_flush ()
/*
 * Flush the output buffer.
 */
{
/*
 * See if there is truly data to write.
 */
	if (TT_nch)
	{
	/*
	 * Do the write.
	 */
		printf (TT_obuf);
	/*
	 * Reset the count.
	 */
		TT_nch = 0;
	}
}





tty_drain ()
/*
 * Throw away the terminal output buffer.
 */
{
	TT_nch = 0;
}



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




tty_home ()
/*
 * Dummy
 */
{
	return (TRUE);
}



tty_down_scroll ()
/*
 * Dummy
 */
{
	return (TRUE);
}



tty_stand ()
/*
 * Dummy (handles both tty_standin and tty_standout)
 */
{
	return (TRUE);
}




tty_clear ()
/*
 * Dummy (handles both tty_clear and tty_clear_l
 */
{
	return (TRUE);
}



tty_setint (handler)
int handler;
/*
 * Dummy
 */
{
	return (TRUE);
}



char
tty_do_read (time)
int time;
/*
 * Dummy
 */
{
	ui_error ("Attempted tty_do_read on CRAY");
}




char
tty_readch ()
/*
 * Dummy
 */
{
	ui_error ("Attempted tty_readch on CRAY");
}



tty_bell ()
{
	return (TRUE);
}




tty_zap_ta ()
/*
 * Dummy
 */
{
	return (TRUE);
}




tty_dump_keys ()
/*
 * Dummy
 */
{
	return (TRUE);
}




int
tty_get_key (key)
int key;
/*
 * Dummy (handles tty_get_key_name and tty_get_key_code)
 */
{
	return (TRUE);
}




tty_set_exh ()
/*
 * Dummy
 */
{
	return (TRUE);
}




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
		   case '~':
			break;
		   default:
			TT_obuf[TT_nch++] = *bp & 0x7f;
		}
	}
}




/*
 * Other stubs.
 */
tty_kpoff () {}
tty_kpon () {}
tty_return () {}
tty_string (string)
char *string;
{
	tty_out (string, strlen (string));
}
