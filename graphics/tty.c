/* 5/87 jc */
/*
 * Perform basic terminal functions for graphical input/output.
 */
# include "param.h"
# ifdef VMS
# include <iodef.h>
# endif
# ifdef unix
# ifdef SVR4
# include <fcntl.h>
# else
# include <sys/file.h>
# endif
# endif
# include "graphics.h"
# include "tty.h"

# ifdef VMS
# define s_error(s)  (((s) & 0x1) == 0)
# endif

/*
 * Forwards
 */
void gtty_flush();



int
gtty_open (device, tag)
char *device;
struct tty_tag **tag;
/*
 * Attempt to open a terminal device.
 */
{
	int status;
	struct tty_tag *t = (struct tty_tag *) getvm (sizeof (struct tty_tag));
/*
 * Try to get a channel to this device.
 */
# ifdef VMS
	t->tty_channel = 0;
 	status = sys$assign (descr (device), &t->tty_channel, ___, ___);
	if (s_error (status))
		return (GE_BAD_DEVICE);
# endif
# ifdef unix
	t->tty_channel = open (device, O_RDWR);
	if (t->tty_channel < 0)
		return (GE_BAD_DEVICE);
# endif

# ifdef VMS
/*
 * Get us an event flag as well.
 */
	t->tty_ef = 0;
 	lib$get_ef (&t->tty_ef);
# endif
/*
 * Reset the buffer pointer and return.
 */
 	t->tty_bufp = t->tty_buf;
	*tag = t;
	return (GE_OK);
}




void
gtty_out (tag, string)
struct tty_tag *tag;
char *string;
/*
 * Output a character string to this device.
 */
{
	int len = strlen (string);
/*
 * Make sure that there is room in the buffer, and perform a flush if
 * necessary.
 */
	if ((TTYBUFLEN - (tag->tty_bufp - tag->tty_buf)) < len)
		gtty_flush (tag);
/*
 * Now just copy the data into the buffer.
 */
 	strcpy (tag->tty_bufp, string);
	tag->tty_bufp += len;
}




void
gtty_flush (tag)
struct tty_tag *tag;
/*
 * Flush out any data in this terminal buffer.
 */
{
	int status;
# ifdef VMS
	short iosb[4];
# endif
/*
 * If there is no data, blow this call off.
 */
	if (tag->tty_bufp <= tag->tty_buf)
		return;
/*
 * Do the write.
 */
# ifdef VMS
	status = sys$qiow (tag->tty_ef, tag->tty_channel,
		IO$_WRITEVBLK | IO$M_NOFORMAT, iosb, ___, ___,
		tag->tty_buf, tag->tty_bufp - tag->tty_buf,
		___, ___, ___, ___);
	if (s_error (status))
		ui_error ("Write QIO status error %d", status);
	else if (s_error (iosb[0]))
		ui_error ("IOSB QIO error %d", iosb[0]);
# endif

# ifdef unix
	if (write (tag->tty_channel, tag->tty_buf,
		tag->tty_bufp - tag->tty_buf) < 0)
		perror ("Graphics tty write error");
# endif
/*
 * Reset the buffer.
 */
	tag->tty_bufp = tag->tty_buf;
}



void
gtty_readprompt (tag, prompt, reply, rlen)
struct	tty_tag *tag;
char	*prompt, *reply;
int	rlen;
/*
 * Prompt the device for information and get the reply
 * INPUT:
 *	TAG	tag of device to be prompted
 *	PROMPT	character string to be sent to the device
 *	REPLY	character buffer to hold the reply
 *	RLEN	usable length of the REPLY buffer
 * OUTPUT:
 *	REPLY	holds the (null-terminated) device reply to the prompt
 */
{
# ifdef VMS
	int	status, endchars[2];
	short	iosb[4];
/*
 * Only terminate the read on <CR>
 */
	endchars[0] = 0;
	endchars[1] = 0x2000;
/*
 * Do the qio
 */
	status = sys$qiow (tag->tty_ef, tag->tty_channel,
		IO$_READPROMPT, iosb, ___, ___,
		reply, rlen, ___, endchars, prompt, strlen (prompt));
/*
 * Check the return status
 */
	if (s_error (status))
		ui_error ("READPROMPT QIO status error %d", status);
	else if (s_error (iosb[0]))
		ui_printf ("READPROMPT iosb error %d", iosb[0]);
/*
 * Null terminate the reply
 */
	reply[iosb[1]] = 0;
# else
	printf ("BOZO!  I don't have readprompt working yet!\n");
# endif
}




void
gtty_close (tag)
struct tty_tag *tag;
/*
 * Close down this terminal.
 */
{
# ifdef VMS
	lib$free_ef (&tag->tty_ef);
	sys$close (tag->tty_channel);
# endif
# ifdef unix
	close (tag->tty_channel);
# endif
	relvm (tag);
}
