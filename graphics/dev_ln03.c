/* 9/86 jc */
/*
 * DEC LN03 driver, scarfed from the other tek-type drivers jc.
 *
 * This driver was snarfed from the ROBOT driver.
 *
 * Currently, this is a highly VMS-specific driver.
 */
# include "h_lib:ilist.h"
# include "graphics.h"
# include "device.h"
# include <iodef.h>
# include <stdio.h>
# include <sjcdef.h>

/*
 * Defines.
 */
# define ___	0
# define s_error(s) (((s) & 0x1) == 0)
# define HBIT 0x40	/* Bit to set for high integers	*/
# define NEGBIT 0x10	/* Sign bit	*/
# define LBIT 0x20	/* Low order integer bit	*/
# define CHFUDGE 30	/* To adjust for char positioning at bottom of char */
# define TEMPFILE "sys$scratch:robot_ln03.plt"

/*
 * Character codes of interest.
 */
# define GS	29	/* Enter vector mode */
# define CAN	24	/* Enter transparent (no graphics) mode */

# define VECT  "\35"		/* Vector mode string GS */
# define ALPHA "\35\37"		/* Alpha mode string GS US */
# define CLEAR "\35\33\14"	/* Clear screen string GS ESC FF   */
				/* (leaves terminal in alpha mode) */

/*
 * Our tag structure.
 */
# define DBUFLEN 1024
struct ln_tag
{
	int	lt_channel;	/* Our channel to the device	*/
	int	lt_ef;		/* Our event flag		*/
	char	lt_queue[80];	/* Device queue name.		*/
	char	lt_buf[DBUFLEN];/* The device output buffer	*/
	char	*lt_bufp;	/* Current position in lt_buf	*/
	int	lt_clear;	/* Is dev clear?		*/
};

/*
 * All of the LN03 line pattern commands are two-character escape
 * sequences.  This table holds the second character for each pattern.
 */
static char Lp_chars[] =
{
	'e',	/* GPLT_SOLID	*/
	'c',	/* GPLT_DASH	*/
	'a',	/* GPLT_DOT	*/
	'b',	/* GPLT_DASH_DOT */
	0
};



ln_open (device, type, tag, dev)
char *device, *type, **tag;
struct device *dev;
{
	int status;
	struct ln_tag *ltp = (struct ln_tag *) getvm (sizeof (struct ln_tag));
/*
 * Get connected to the device.
 */
	strcpy (ltp->lt_queue, device);
	if ((ltp->lt_channel = dcreate_nra (TEMPFILE)) <= 0)
		ui_error ("Unable to open spool file '%s'", TEMPFILE);
/*
 * Reset the buffer to the beginning.
 */
	ltp->lt_bufp = ltp->lt_buf;
	ltp->lt_clear = TRUE;
/*
 * Enter TEK emulation mode.
 */
 	ln_out_s (ltp, "\033[?38h");
/*
 * Clear the text and graphics areas
 */
	ln_clear (ltp);
	ln_out_s (ltp, ALPHA);
	ln_out_s (ltp, "\033;");	/* Small character size */
/*
 * Return the info.
 */
	*tag = (char *) ltp;
	return (GE_OK);
}






ln_flush (ctag)
char *ctag;
/*
 * Flush out the command buffer.
 */
{
	int status;
	short iosb[4];
	struct ln_tag *ltp = (struct ln_tag *) ctag;
/*
 * If there is no data, blow this call off.
 */
	if (ltp->lt_bufp <= ltp->lt_buf)
		return;
/*
 * Do the write.
 */
	dput (ltp->lt_channel, ltp->lt_buf, ltp->lt_bufp - ltp->lt_buf);
/*
 * Reset the buffer.
 */
	ltp->lt_bufp = ltp->lt_buf;
}




ln_out_s (ltp, str)
struct ln_tag *ltp;
char *str;
/*
 * Output a C string to the terminal.
 */
{
	ln_chk_buf (ltp);
	while (*str)
		*ltp->lt_bufp++ = *str++;
}





ln_clear (ctag)
char *ctag;
/*
 * Clear the screen and leave in transparent mode.
 */
{
	struct ln_tag *ltp = (struct ln_tag *) ctag;

	if (ltp->lt_clear)
		return;
	ln_chk_buf (ltp);		/* flush buffer if needed */
	ln_out_s (ltp, CLEAR);
	ltp->lt_clear = TRUE;
}








ln_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * Draw a polyline with the given number of points.
 */
{
	struct ln_tag *ltp = (struct ln_tag *) ctag;
	int point;
	static char vpat[3] = { '\033', 0, 0 };
/*
 * Select our vector pattern, and go into polyline mode.
 */
	vpat[1] = Lp_chars[ltype];
	ln_out_s (ltp, vpat);
	ln_out_s (ltp, VECT);
/*
 * Send out the coords for each point.
 */
	for (point = 0; point < npt; point++)
	{
		ln_chk_buf (ltp);
		ln_xy (ltp, data[0], data[1]);
		data += 2;
	}
	ltp->lt_clear = FALSE;
}







ln_xy (ltp, x, y)
struct ln_tag *ltp;
int x, y;
/*
 * Send the given numbers out as XY coords.  We can't use the generic
 * tek routines here, since they assume that the output is going to
 * a terminal device.
 */
{
	static int last_hx = 1, last_hy = 1;	/* The '1' is an illegal high
						 * value, insuring that we will
						 * send them the first time 
						 */
	int b;
/*
 * Put together the Hi-Y byte, and send it if necessary.
 */
	b = ((y & 0xF80) >> 7) | LBIT;
/*  	if (b != last_hy) */
		last_hy = *ltp->lt_bufp++ = b;
/*
 * Send the "extra" byte.
 */
	*ltp->lt_bufp++ = HBIT | LBIT | ((y & 0x3) << 2) | (x & 0x3);
/*
 * Now send the Lo-Y byte.
 */
	*ltp->lt_bufp++ = HBIT | LBIT | ((y & 0x7C) >> 2);
/*
 * Put together and send the Hi-X byte, if necessary.
 */
	b = ((x & 0xF80) >> 7) | LBIT;
 /*	if (b != last_hx) */
		last_hx = *ltp->lt_bufp++ = b;
/*
 * Send the Lo-X byte.
 */
	*ltp->lt_bufp++ = HBIT | ((x & 0x7C) >> 2);
}








ln_chk_buf (ltp)
struct ln_tag *ltp;
/*
 * Check the output buffer, to see if we need to do a flush.
 */
{
	if (ltp->lt_bufp - ltp->lt_buf > DBUFLEN - 100)
		ln_flush (ltp);
}










ln_close (ctag)
char *ctag;
/*
 * Shut down the terminal.
 */
{
	struct ln_tag *ltp = (struct ln_tag *) ctag;
/*
 * Put out a clear sequence, which will cause the last plot to be printed.
 */
	if (! ltp->lt_clear)
		ln_out_s (ltp, "\033\f");
/*
 * Put the printer back into normal mode.
 */
	ln_out_s (ltp, "\033[!p");
	ln_flush (ltp);
/*
 * If we are going queued, we need to close the file, and queue it to
 * be printed.
 */
	dclose (ltp->lt_channel);
	ln_queue_file (ltp->lt_queue, TEMPFILE, "ROBOT_LN03");
}


/*
 * Item list for device queuing.
 */
static struct i_list Queue_list[] =
{
	{ 0,	SJC$_DELETE_FILE,	0,	0 },
	{ 0,	SJC$_QUEUE,		0,	0 },
# define IL_QUEUE 1
	{ 0,	SJC$_FILE_SPECIFICATION,0,	0 },
# define IL_FILE 2
	{ 0,	SJC$_FORM_NAME,		0,	0 },
# define IL_FORM 3
	{ 0, 	SJC$_NOTIFY,		0,	0 },
	{ 0,	SJC$_NO_PAGINATE,	0,	0 },
	{ 0,	SJC$_PASSALL,		0,	0 },
	{ ___,	___,			___,	___ }
};




ln_queue_file (queue, file, form)
char *queue, *file, *form;
/*
 * Queue up this file to be printed and deleted, with the given forms type.
 */
{
	int status, ef = 0;
	short iosb[4];
/*
 * Fill in our item list.
 */
 	Queue_list[IL_QUEUE].il_dest = queue;
	Queue_list[IL_QUEUE].il_length = strlen (queue);
	Queue_list[IL_FILE].il_dest = file;
	Queue_list[IL_FILE].il_length = strlen (file);
	Queue_list[IL_FORM].il_dest = form;
	Queue_list[IL_FORM].il_length = strlen (form);
/*
 * Do the call.
 */
 	lib$get_ef (&ef);
	status = sys$sndjbcw (ef, SJC$_ENTER_FILE, ___, Queue_list,
			iosb, ___, ___);
 	lib$free_ef (&ef);
	if (s_error (status))
		ui_error("Unable to put file '%s' in '%s' form '%s', error %d",
			file, queue, form, status);
	else if (s_error (iosb[0]))
		ui_error ("(IOSB) Unable to put file '%s' in '%s' form '%s', error %d",
			file, queue, form, status);
/*
 * Done.
 */
}




ln_cmap (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * No-op color map routine, since this is a mono device.
 */
{ }
