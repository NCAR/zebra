/* 3/85 jc */
/*
 * Tektronix 4107 terminal graphics driver.
 * Redone 5/87 jc for the new graphics package.
 */
# include "param.h"

# ifdef DEV_4107

# include "graphics.h"
# include "tty.h"
# include "device.h"
# ifdef VMS
# include "lib_include:lib_proto.h"
# endif


/*
 * Defines.
 */
# define CHFUDGE 50	/* To adjust for char positioning at bottom of char */
# define BUSY 0x2	/* Printer busy bit	*/
# define POLL_INTERVAL 10 /* How often to poll printer, in seconds. */
# define FIXUP
# define XRES 640
# define YRES 480

/*
 * Unpacking info
 */
# define LOW5BITS	0x1F
# define EXTRA_YBITS	0xC
# define EXTRA_XBITS	0x3

/*
 * Character codes of interest.
 */
# define US	31	/* Leave graphic mode	*/
# define GS	29	/* Enter graphic mode	*/

/*
 * The structure type of our "device tag"
 */
struct tek_tag
{
	struct tty_tag	*tek_tty;	/* The terminal buffer info	*/
	int	tek_segno;		/* Next available segment #	*/
	int	tek_pixmap;		/* Running in pixmap mode?	*/
};


/*
 * Map graphics line types onto Tek types.
 */
static int Tek_ltypes[] =
{
	0,	/* GPLT_SOLID	*/
	4,	/* GPLT_DASH	*/
	1,	/* GPLT_DOT	*/
	2	/* GPLT_DASH_DOT*/
};




tek_open (device, type, tag, dev)
char *device, *type, **tag;
struct device *dev;
/*
 * Open up a TEK 4107 terminal.
 */
{
	int status;
	struct tek_tag *t = (struct tek_tag *) getvm (sizeof (struct tek_tag));
/*
 * Get a channel to the device.
 */
	t->tek_segno = 1;
 	if ((status = gtty_open (device, &t->tek_tty)) != GE_OK)
		return (status);
/*
 * See which mode we are to run in.
 */
 	t->tek_pixmap = ! strncmp (type, "4107p", 5);
/*
 * Attempt to clear the dialog area by (1) insuring ANSI mode, then (2)
 * sending the ANSI clear sequence.
 */
	gtty_out (t->tek_tty, "\033%!1");	/* Select code ansi */
	gtty_out (t->tek_tty, "\033[;H\033[2J");	/* Home, then clear */
/*
 * Put out the initial escape sequences to get the terminal into the
 * right mode.
 */
	gtty_out (t->tek_tty, "\033%!0");	/* Select TEK mode	*/
	gtty_out (t->tek_tty, "\033TM111"); /* RGB color mode, opaque, color */
/*
 * Make sure we have a large input queue size, since otherwise we're likely
 * to lose some stuff at higher speeds.
 */
	gtty_out (t->tek_tty, "\033NQ");	/* SET QUEUE SIZE */
	tek_int (32767, t->tek_tty);
/*
 * Set the window to be from (0, 0) to (639, 479), i.e. the hardware resolution
 * of the screen.
 */
 	gtty_out (t->tek_tty, "\033RW");	/* SET WINDOW...	*/
	tek_xy (0, 0, t->tek_tty, TRUE);	/* Origin		*/
	tek_xy (639, 479, t->tek_tty, TRUE);	/* extent	*/
/*
 * Delete all segments.
 */
	gtty_out (t->tek_tty, "\033SK");
	tek_int (-1, t->tek_tty);
/*
 * Make the crosshair (segment 0) invisible
 */
	gtty_out (t->tek_tty, "\033SV00");
/*
 * Set the copy size to small, since it happens a lot faster that way.
 */
	gtty_out (t->tek_tty, "\033QA1");	/* Set up small copy size */
/*
 * Set the fixup level to two.
 */
# ifdef FIXUP
	if (! t->tek_pixmap)
		gtty_out (t->tek_tty, "\033RF2");
# endif
	*tag = (char *) t;
	return (GE_OK);
}






tek_clear (tag)
struct tek_tag *tag;
/*
 * Clear the screen.
 */
{
/*
 * In order:
 *	clear graphics
 *	set terminal to ansi mode
 * 	clear dialog screen
 *	move cursor to lower left corner
 *	set terminal to TEK mode
 */
	gtty_out (tag->tek_tty, "\033\f\035\037");
	gtty_out (tag->tek_tty, "\033%!1");
	gtty_out (tag->tek_tty, "\033[2J");
	gtty_out (tag->tek_tty, "\033[32;1H");
	gtty_out (tag->tek_tty, "\033%!0");
}




tek_flush (ctag)
char *ctag;
/*
 * Perform a device flush.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;

	tek_endplot (tag, TRUE);
	gtty_flush (tag->tek_tty);
/*
 * 	Make sure that the first thing we do on the next write
 * 	is put the terminal back into TEK mode.
 */
	gtty_out (tag->tek_tty, "\033%!0");
}




tek_flush_nr (ctag)
char *ctag;
/*
 * Perform a device flush with no screen renewal.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;

	tek_endplot (tag, FALSE);
	gtty_flush (tag->tek_tty);
/*
 * 	Make sure that the first thing we do on the next write
 * 	is put the terminal back into TEK mode.
 */
	gtty_out (tag->tek_tty, "\033%!0");
}




tek_cmap (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * Load up a color map.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
	int color;

	for (color = base; color < ncolor + base; color++)
	{
		gtty_out (tag->tek_tty, "\033TG");/* Set Surface Color Map */
		tek_int (1, tag->tek_tty);	/* Output the surface number */
		tek_int (4, tag->tek_tty);	/* Four parameters	*/
		tek_int (color, tag->tek_tty);
		tek_int ((int) (*r++ * 100.0), tag->tek_tty);
		tek_int ((int) (*g++ * 100.0), tag->tek_tty);
		tek_int ((int) (*b++ * 100.0), tag->tek_tty);
	}
	return (GE_OK);
}





tek_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * Draw a polyline with the given number of points.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
	int point;
/*
 * Switch over to the given color.
 */
	gtty_out (tag->tek_tty, "\033ML");	/* SET LINE INDEX	*/
	tek_int (color, tag->tek_tty);
/*
 * Get the line index right.
 */
	gtty_out (tag->tek_tty, "\033MV");	/* SET LINE STYLE...	*/
	tek_int	(Tek_ltypes[ltype], tag->tek_tty);
/*
 * Go into polyline mode.
 */
	gtty_flush (tag->tek_tty);
 	gtty_out (tag->tek_tty, "\035");	/* GS (ENTER VECTOR MODE) */
/*
 * Send out the coords for each point.
 */
	for (point = 0; point < npt; point++)
	{
		tek_xy (data[0], data[1], tag->tek_tty, TRUE);
		data += 2;
	}
/*
 * Back into normal mode.
 */
	gtty_out (tag->tek_tty, "\037");	/* US */
}



int
tek_s_init (ctag)
char *ctag;
/*
 * Initialize (allocate) a new segment
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
	int seg = tag->tek_segno++;
	
/*
 * (5/12/88 jc)	We need to initialize the segment now, so that visibility
 *		changes won't generate errors.
 */
 	gtty_out (tag->tek_tty, "\033SO");	/* begin segment */
	tek_int (seg, tag->tek_tty);
	gtty_out (tag->tek_tty, "\033SC");	/* End segment	*/
	return (seg);
}



tek_s_clear (ctag, seg)
char *ctag;
int seg;
/*
 * Perform a segment clear.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
/*
 * We do this by simply deleting the segment, since 4107 segments are
 * graven in stone and may never be changed...
 */
	gtty_out (tag->tek_tty, "\033SK");
	tek_int (seg, tag->tek_tty);
/*
 * Reinitialize the segment
 */
 	gtty_out (tag->tek_tty, "\033SO");	/* begin segment */
	tek_int (seg, tag->tek_tty);
	gtty_out (tag->tek_tty, "\033SC");	/* End segment	*/
}




tek_s_select (ctag, seg, priority)
char *ctag;
int seg, priority;
/*
 * Perform a segment select on this device.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
/*
 * Throw in a SET SEGMENT DISPLAY PRIORITY to get that right.
 */
 	gtty_out (tag->tek_tty, "\033SS");
	tek_int (-2, tag->tek_tty);
	tek_int (priority, tag->tek_tty);
/*
 * Rename the selected segment to a temporary location 
 * (just take the next unused segment) so we can copy it back
 */
	gtty_out (tag->tek_tty, "\033SR");
	tek_int (seg, tag->tek_tty);
	tek_int (tag->tek_segno, tag->tek_tty);
/*
 * Do a BEGIN SEGMENT to get the segment all set up.
 */
 	gtty_out (tag->tek_tty, "\033SO");
	tek_int (seg, tag->tek_tty);
/*
 * INCLUDE COPY OF SEGMENT command to put the old segment stuff back in
 */
	gtty_out (tag->tek_tty, "\033LK");
	tek_int (tag->tek_segno, tag->tek_tty);
/*
 * Do a DELETE SEGMENT on the temporary segment
 */
	gtty_out (tag->tek_tty, "\033SK");
	tek_int (tag->tek_segno, tag->tek_tty);
}




tek_s_end (ctag, seg)
char *ctag;
int seg;
/*
 * Finish an update on this particular segment.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
/*
 * This becomes a simple END SEGMENT command.
 */
 	gtty_out (tag->tek_tty, "\033SC");
}




tek_s_attr (ctag, seg, vis)
char *ctag;
int seg, vis;
/*
 * Tweak with the attributes of this segment.  So far, the only one we
 * handle is visibility.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
/*
 * Do a simple SET SEGMENT VISIBILITY command.
 */
 	gtty_out (tag->tek_tty, "\033SV");
	tek_int (seg, tag->tek_tty);
	tek_int (vis ? 1 : 0, tag->tek_tty);
}


# ifdef notdef



tek_print (big)
int big;
/*
 * Print the current contents of the screen.
 */
{
	int status, cm;
	short iosb[4];
	char devstat[80];

/*
 * Set the print size.
 */
 	if (! Rasterizer)
		tek_out_s (big ? "\033QA0" : "\033QA1");
/*
 * Send the print command.
 */
	tek_out_s ("\033KH0");
	tek_flush ();
	if (Rasterizer)
		return;
/*
 * Now we have to poll the device until it says we're done.
 */
	for (;;)
	{
	/*
	 * Wait for a while.
	 */
		sleep (POLL_INTERVAL);
	/*
	 * Do a readprompt, and see if we are done yet.
	 */
		status = sys$qiow (Ef, Channel, IO$_READPROMPT | IO$M_PURGE,
			iosb, ___, ___,
			devstat, 80, ___, ___, "\033JQ3HC:", 7);
		if (s_error (status))
			ui_error ("Readprompt status error %d in tek_print",
				status);
		else if (s_error (iosb[0]))
			ui_printf ("Readprompt iosb error %d in tek_print",
				iosb[0]);
	/*
	 * Check the return status.
	 */
		devstat[iosb[1]] = 0;
		if ((devstat[4] & BUSY) == 0)
			break;	/* Done! */
	}
}





# endif


tek_endplot (tag, renew)
struct tek_tag *tag;
int	renew;
/*
 * We're at the end of a plot, so let's get out of graphics mode and
 * into normal terminal mode.  Do a full screen renew if requested
 */
{
/*
 * 	Go into ANSI mode and move the cursor to the lower right corner.
 *	This assures that the prompt won't get in the way when the plot
 * 	is finished.
 */
	if (! tag->tek_pixmap && renew)
		gtty_out (tag->tek_tty, "\033KN0");	/* Renew */
	gtty_out (tag->tek_tty, "\033LZ");	/* Clear dialog area */
	gtty_out (tag->tek_tty, "\033%!2");
	gtty_out (tag->tek_tty, "\033[24;1H");
}



tek_close (ctag)
char *ctag;
/*
 * Close down this device.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
/*
 * Put the terminal back into ansi mode, after clearing all segments out.
 * The segment clear is done because otherwise, only dynamite will make
 * them go away.  In particular, the erase buttons on the keyboard are
 * impotent against segmented graphics...
 * (we also put the window back to the default, to avoid confusing older
 * graphics software).
 */
 	gtty_out (tag->tek_tty, "\033RW");	/* SET WINDOW...	*/
	tek_xy (0, 0, tag->tek_tty, TRUE);	/* Origin		*/
	tek_xy (4095, 3130, tag->tek_tty, TRUE);/* extent	*/
	if (! tag->tek_pixmap)
	{
		gtty_out (tag->tek_tty, "\033SK");	/* DELETE SEGMENT */
		tek_int (-1, tag->tek_tty);
		gtty_out (tag->tek_tty, "\033SV00");	/* Remove crosshair */
		gtty_out (tag->tek_tty, "\033\f\035\037"); /* Graphics clear */
	}
	gtty_out (tag->tek_tty, "\033%!1");	/* Select code ansi */
	gtty_out (tag->tek_tty, "\033[;H\033[2J");	/* Home, then clear */
	gtty_flush (tag->tek_tty);
/*
 * Close down our channel, and release storage.
 */
	gtty_close (tag->tek_tty);
	relvm (tag);
}




tek_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag;
int x, y, xs, ys;
char *data;
int size, org;
/*
 * The tektronix pixel fill routine.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
	int xp, yp;
	char *pa = getvm (xs + 1);
/*
 * Do a BEGIN PIXEL OPERATIONS to get things set up right.
 */
	gtty_out (tag->tek_tty, "\033RU");
	tek_int (0, tag->tek_tty);	/* All surfaces			*/
	tek_int (11, tag->tek_tty);	/* ALU mode 11 -- overwrite	*/
	tek_int (4, tag->tek_tty);	/* Four bits/pixel		*/
/*
 * All of the examples in the manual do a SET PIXEL VIEWPORT command.  I
 * don't know if it is necessary, but I don't feel like screwing around
 * with it.
 */
 	gtty_out (tag->tek_tty, "\033RS");
	tek_xy (x, y, tag->tek_tty, TRUE);	/* Lower left corner	*/
	tek_xy (x + xs - 1, y + ys - 1, tag->tek_tty, TRUE); /* upper rgt */
/*
 * Now go through each row.
 */
 	for (yp = 0; yp < ys; yp++)
	{
		int paind;
	/*
	 * Do a SET PIXEL BEAM POSITION to get to the right spot.  Coords
	 * here are relative to the pixel viewport.
	 */
	 	gtty_out (tag->tek_tty, "\033RH");
		tek_xy (0, /* y + */ yp, tag->tek_tty, TRUE);
		if ((yp % 8) == 0)
			gtty_flush (tag->tek_tty);
	/*
	 * Go into RASTER WRITE mode, and ship out the stuff.
	 */
	 	gtty_out (tag->tek_tty, "\033RP");
		tek_int (xs, tag->tek_tty); /* Once for pixel count */
		paind = 0;
		for (xp = 0; xp < xs; xp += 3)
		{
			if (xs - xp >= 3)
			{
				pa[paind++] = (data[0] << 2) +
					((data[1] & 0xC) >> 2) + 32;
				pa[paind++] = ((data[1] & 0x3) << 4) +
					data[2] + 32;
				data += 3;
			}
			else if (xs - xp == 2)
			{
				pa[paind++] = (data[0] << 2) +
					((data[1] & 0xC) >> 2) + 32;
				pa[paind++] = ((data[1] & 0x3) << 4) + 32;
				data += 2;
			}
			else
				pa[paind++] = (*data++ << 2) + 32;
		}
		pa[paind] = 0;
		tek_int (strlen (pa), tag->tek_tty); /* Char array len */
		gtty_out (tag->tek_tty, pa);
	}
	gtty_flush (tag->tek_tty);
	relvm (pa);
}
		



tek_target (ctag, x, y)
char *ctag;
int *x, *y;
/*
 * Get the target position
 */
{
	int hi_y, lo_y, extra_y, hi_x, lo_x, extra_x;
	struct tek_tag *tag = (struct tek_tag *) ctag;
	char reply[81], dummy[2];
/*
 * Flush now (or lose the buffer to bypass mode)
 */
	gtty_flush (tag->tek_tty);
/*
 * Do a readprompt with the commands:
 *	"Set mode TEK"
 *	"Report segment status, segment 0, location"
 */
	gtty_readprompt (tag->tek_tty, "\033%!0\033SQ01X", reply, 80);
	if (strlen (reply) != 9)
		printf ("(BUG) Unexpected 4107 reply length %d: '%s'", 
			strlen (reply), reply);

	hi_x = reply[7] & LOW5BITS;
	lo_x = reply[8] & LOW5BITS;
	extra_x = reply[5] & EXTRA_XBITS;
	hi_y = reply[4] & LOW5BITS;
	lo_y = reply[6] & LOW5BITS;
	extra_y = reply[5] & EXTRA_YBITS;

	*x = hi_x << 7 | lo_x << 2 | extra_x;
	*y = hi_y << 7 | lo_y << 2 | extra_y;
/*
 * Read the extraneous carriage return
 */
	gtty_readprompt (tag->tek_tty, "", dummy, 1);
	if (strlen (dummy))
		printf ("(BUG) Got more than a <CR> in 4107 dummy read");
}



tek_put_target (ctag, x, y)
char *ctag;
int x, y;
/*
 * Put the target at the given location
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
/*
 * Set segment position for segment 0 (crosshair cursor)
 * and make it visible
 */
 	gtty_out (tag->tek_tty, "\033SX0");
	tek_xy (x, y, tag->tek_tty, TRUE);
	gtty_out (tag->tek_tty, "\033SV01");
	gtty_flush (tag->tek_tty);
}




tek_vp (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * Perform a viewport adjustment.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;

/*
 * Do some sanity checking.
 */
 	if (x1 <= x0 || y1 <= y0)
		return (GE_BAD_COORDS);
	if (x1 < 0 || y1 < 0 || x0 >= XRES || y0 >= YRES)
		return (GE_OFFSCREEN);
/*
 * Send out the instructions.
 */
 	gtty_out (tag->tek_tty, "\033RW");	/* SET WINDOW...	*/
	tek_xy (x0, y0, tag->tek_tty, TRUE);	/* Origin		*/
	tek_xy (x1, y1, tag->tek_tty, TRUE);	/* extent	*/
}





# endif /* DEV_4107 */
