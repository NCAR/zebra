/* 	10/84 jc	*/
/* 
 * Functions dealing with the RAMTEK.
 */
# include "param.h"

# ifdef DEV_RAMTEK

# include "graphics.h"
# include "device.h"
# include "h_lib:ramtek.h"
# include <iodef.h>
# include <ssdef.h>

/*
 * Defines.
 */
# define ___ 0
# define s_error(s) (((s) & 0x1) == 0)
# define RESX	1280	/* X, Y resolution of whole screen	*/
# define RESY	1024
# define IRESX	640	/* Resolution of a single image		*/
# define IRESY	512
# define MAXTRIES 100	/* Maximum number of tries for a channel*/

/*
 * Origins for "image plane" device types.
 */
static	int	X_offs[] = { 0, 0, 640, 0, 640 };
static 	int	Y_offs[] = { 0, 512, 512, 0, 0 };
static	int	Xres[] = { 1280, 640, 640, 640, 640 };
static	int	Yres[] = { 1024, 512, 512, 512, 512 };

/*
 * Line type info.
 */
static unsigned short Vpats[GPLT_NTYPES] =
{
	0xffff,		/* Solid line		*/
	0xff00,		/* Dashed		*/
	0xcccc,		/* Dotted		*/
	0xff18		/* dot-dash		*/
};

/*
 * To keep life a little simpler, we just use a single EF for all devices
 * that may be open.
 */
static int Ef = 0;

/*
 * The tag value we pass back is a pointer to one of these...
 */
# define RM_BUFLEN 32768
# define CMAPLEN 256
struct rm_tag
{
	char	rm_dev[20];		/* Device name			*/
	int	rm_dtype;		/* Device type, 0=full, else image */
	int	rm_cursor;		/* Cursor number		*/
	int	rm_vlt;			/* Lookup table number		*/
	short	rm_buf[RM_BUFLEN];	/* The command buffer		*/
	short	*rm_bufp;		/* Current pos in rm_buf	*/
	short	*rm_breset;		/* Reset position in rm_buf	*/
	char	rm_clear;		/* Is device screen clear?	*/
	unsigned char rm_cmap[4*CMAPLEN]; /* Color map			*/
	short	cx0, cy0, cx1, cy1;	/* Current clip window		*/
};



rm_open (device, type, tag, dev)
char *device, *type, **tag;
struct device *dev;
/*
 * Open and initialize a RAMTEK device.
 */
{
	struct rm_tag *rmt = (struct rm_tag *) getvm (sizeof (struct rm_tag));
	int status, chan, len = 0;
	short iosb[4];
	char devname[20];
/*
 * Fill in our tag structure.
 */
 	rmt->rm_dtype = type[5] - '0';
	rmt->rm_bufp = rmt->rm_buf;
	rmt->rm_clear = FALSE;
	rmt->cx0 = 0;
	rmt->cy0 = 0;
	rmt->cx1 = Xres[rmt->rm_dtype] - 1;
	rmt->cy1 = Yres[rmt->rm_dtype] - 1;
/*
 * Get an event flag to use in RAMTEK transactions.  Since all RAMTEK ops
 * are done synchronously, we can get by with only one flag, even if there
 * are multiple devices open.
 */
	if (Ef == 0)
	{
		status = lib$get_ef (&Ef);
		if (s_error (status))
			sys_error (status, "No event flags");
	}
/*
 * Get a channel to the device.
 */
	strcpy (rmt->rm_dev, device);
	if ((chan = rm_channel (device)) == 0)
		return (GE_BAD_DEVICE);
/*
 * Get our cursor number.
 */
	rmt->rm_cursor = -1;	/* Indicate that we want next avaible cursor */
	status = sys$qiow (Ef, chan, IO$_ACCESS, iosb, ___, ___,
			5, &rmt->rm_cursor, ___, ___, ___, ___);
	if (s_error (status))
		sys_error (status, "Unable to get RAMTEK cursor number");
	else if (s_error (iosb[0]))
		sys_error(iosb[0],"Unable to get RAMTEK cursor number (iosb)");
/*
 * Figure out the VLT number.  This kludge snarfed from RMOPEN.FOR.
 */
 	lib_devnam (&chan, 0, descr_n (devname, 20), &len);
	rmt->rm_vlt = devname[len - 2] - '0';
/*
 * Initialize the command buffer
 */
	rm_init_buffer ((char *) rmt);
/*
 * Done with the device.
 */
	rm_done (chan);
/*
 * Return the info.
 */
	*tag = (char *) rmt;
	return (GE_OK);
}




rm_init_buffer (ctag)
char	*ctag;
/*
 * Stuff in the init code, at the beginning of the buffer.  Since we only
 * move back to B_reset, this code will be sent with each RAMTEK transfer,
 * which is needed if more than one person is using the same display.  Note
 * that this arrangement will royally screw things up if a flush is done in
 * the middle of an instruction.
 */
{
	struct rm_tag *rmt = (struct rm_tag *) ctag;

	rmt->rm_bufp = rmt->rm_buf;
/*
 * Select the video origin to be at the bottom.
 */
	*rmt->rm_bufp++ = RM_I_SELVO | 1;
/*
 * Set up the clipping window
 */
	*rmt->rm_bufp++ = RM_I_SET | RM_F_RP | RM_F_OF1 | RM_F_OF2;
	*rmt->rm_bufp++ = RM_F1_WMSK | RM_F1_WIN;
	*rmt->rm_bufp++ = RM_F2_WEW | RM_F2_WOF;
	*rmt->rm_bufp++ = 0xFF;	/* Write mask to write all image planes. */
	*rmt->rm_bufp++ = X_offs[rmt->rm_dtype] + rmt->cx0; /* Format window */
	*rmt->rm_bufp++ = Y_offs[rmt->rm_dtype] + rmt->cy0;
        *rmt->rm_bufp++ = X_offs[rmt->rm_dtype] + rmt->cx1;
        *rmt->rm_bufp++ = Y_offs[rmt->rm_dtype] + rmt->cy1;
	*rmt->rm_bufp++ = X_offs[rmt->rm_dtype] + rmt->cx0; /*Wrt enable wndo*/
	*rmt->rm_bufp++ = Y_offs[rmt->rm_dtype] + rmt->cy0;
	*rmt->rm_bufp++ = X_offs[rmt->rm_dtype] + rmt->cx1;
        *rmt->rm_bufp++ = Y_offs[rmt->rm_dtype] + rmt->cy1;
	*rmt->rm_bufp++ = X_offs[rmt->rm_dtype] + rmt->cx0; /* WEW offset */
	*rmt->rm_bufp++ = Y_offs[rmt->rm_dtype] + rmt->cy0;
	rmt->rm_breset = rmt->rm_bufp;
/*
 * Throw in the instruction to return to the standard font.
 */
	*rmt->rm_bufp++ = RM_I_ATTPF | 0xFF;
}




rm_close (ctag)
char *ctag;
/*
 * Close down this device.
 */
{
/*
 * Flush out any remaining data.
 */
 	rm_flush (ctag);
/*
 * Simply deallocate the memory and be done.
 */
 	relvm (ctag);
}





int
rm_channel (dev)
char *dev;
/*
 * Get a channel to the device.
 */
{
	int status, chan, tries;
	short iosb[4];
/*
 * Do the assign.
 */
	chan = 0;
	for (tries = 0; tries < MAXTRIES; tries++)
	{
		status = sys$assign (descr (dev), &chan, ___, ___);
		if (status == SS$_NORMAL)
			break;
		else if (status != SS$_DEVALLOC)
			return (0);
		sleep (2);
	}
	if (chan == 0)
		ui_error ("Somebody is hogging the RAMTEK");
/*
 * Do the create command, without which the driver
 * throws a temper tantrum.
 */
	status = sys$qiow (Ef, chan,
			IO$_CREATE | IO$M_MOUNT | IO$M_DMOUNT,
			iosb, ___, ___, ___, ___, ___, ___, ___, ___);
	if (s_error (status))
		sys_error (status, "Unable to do RAMTEK create");
	else if (s_error (iosb[0]))
		sys_error (iosb[0], "Unable to do RAMTEK create (iosb)");
/*
 * Return the channel number.
 */
	return (chan);
}




rm_done (chan)
int chan;
/*
 * Return the channel to the system.
 */
{
	sys$dassgn (chan);
}




rm_clear (ctag)
char *ctag;
/*
 * Clear the screen.
 */
{
	struct rm_tag *tag = (struct rm_tag *) ctag;

	if (! tag->rm_clear)
	{
	/*
	 * Just load up the command..
	 */
		*tag->rm_bufp++ = RM_I_ERS | RM_F_RP | RM_F_DF;
		*tag->rm_bufp++ =  0;		/* Data flag */
		tag->rm_clear = TRUE;
	}
}







rm_flush (ctag)
/*
 * Flush out the RAMTEK buffer.
 */
{
	struct rm_tag *tag = (struct rm_tag *) ctag;
	short iosb[4];
	int status, len, chan;
/*
 * Make sure we really have data to write.
 */
	if (tag->rm_bufp <= tag->rm_breset)
		return;
/*
 * Set the scan parameter back to where the editor expects it to be.  This
 * should eliminate the nifty "upside-down letter" problem experienced by
 * radar users.
 */
	*tag->rm_bufp++ = RM_I_SET | RM_F_OF1 | RM_F_RP;
	*tag->rm_bufp++ = RM_F1_SCN;	/* Set scan to ... */
	*tag->rm_bufp++ = 0;		/* 0 */
	len = (tag->rm_bufp - tag->rm_buf)*sizeof (short);
/*
 * Get a channel to the device.
 */
	chan = rm_channel (tag->rm_dev);
/*
 * Write out the info.
 */
	status = sys$qiow (Ef, chan,
		IO$_WRITEVBLK | IO$M_MOUNT | IO$M_DMOUNT,
		iosb, 0, 0, tag->rm_buf, len, 0, 0, 0, 0);
	if (! (status & 0x1))
		sys_error (status, "Unable to write %d bytes to RAMTEK", len);
	if (! (iosb[0] & 0x1))
		sys_error(iosb[0], "Unable to write %d bytes to RAMTEK (iosb)",
					len);
/*
 * Make sure we re-establish the expected clip window
 */
	rm_init_buffer (ctag);
/*
 * All done.
 */
	tag->rm_bufp = tag->rm_breset;
	rm_done (chan);
}






rm_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * Draw a polyline.
 */
{
	struct rm_tag *tag = (struct tag *) ctag;
	int point;

	rm_check (tag, 2 * (npt - 1) + 9);
	*tag->rm_bufp++ = RM_I_WVL | RM_F_RP | RM_F_DF | RM_F_OF1 | RM_F_OF2;
	*tag->rm_bufp++ = RM_F1_FGD | RM_F1_COP;
	*tag->rm_bufp++ = RM_F2_VTX;
	*tag->rm_bufp++ = color;		/* FGD */
	*tag->rm_bufp++ = *data++ + X_offs[tag->rm_dtype];	/* COP */
	*tag->rm_bufp++ = *data++ + Y_offs[tag->rm_dtype];
	*tag->rm_bufp++ = Vpats[ltype];		/* VTX */
	*tag->rm_bufp++ = 0x1000;	/* 16-bit pat, no pixel rep */
	*tag->rm_bufp++ = (npt - 1)*4;
	for (point = 1; point < npt; point++)
	{
		*tag->rm_bufp++ = *data++ + X_offs[tag->rm_dtype];
		*tag->rm_bufp++ = *data++ + Y_offs[tag->rm_dtype];
	}
	tag->rm_clear = FALSE;
}




rm_check (tag, nword)
struct rm_tag *tag;
int nword;
/*
 * Make sure that the device buffer can hold this many words of data,
 * flushing it out if necessary.
 */
{
/*
 * KLUGE: Use (nword + 10) here since rm_flush () puts a few commands
 * into the buffer.
 */
	if (nword + 10 > (RM_BUFLEN - (tag->rm_bufp - tag->rm_buf)))
		rm_flush (tag);
}



rm_color_map (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * Set the color map for the Ramtek.
 */
{
	struct rm_tag *tag = (struct rm_tag *) ctag;
	unsigned char *bbufp;
	short *orig;
	int cv;
/*
 * Get set up.
 */
 	rm_check (tag, CMAPLEN/2 + 8);
	orig = tag->rm_bufp;
	*tag->rm_bufp++ = RM_I_WAITVR + 1;	/* Avoid flicker	*/
	*tag->rm_bufp++ = RM_I_LAM + tag->rm_vlt;
	*tag->rm_bufp++ = 0;		/* Start address for first VLT	*/
	*tag->rm_bufp++ = CMAPLEN*4;	/* Data length.		*/
/*
 * Now pass through the color values.
 */
	bbufp = (unsigned char *) tag->rm_bufp;
 	for (cv = 0; cv < base*4; cv++)
		*bbufp++ = tag->rm_cmap[cv];

 	for (cv = base; cv < base + ncolor; cv++)
	{
		if (*r < 0.0 || *r > 1.0 || *g < 0.0 || *g > 1.0 ||
		    *b < 0.0 || *b > 1.0)
		{
			tag->rm_bufp = orig;
			return (GE_BAD_COLOR);
		}
		tag->rm_cmap[cv*4] = *bbufp++ = (unsigned char) (*b++ * 255.0);
		tag->rm_cmap[cv*4 + 1] = *bbufp++ =
				(unsigned char) (*g++ * 255.0);
		tag->rm_cmap[cv*4 + 2] = *bbufp++ =
				(unsigned char) (*r++ * 255.0);
		tag->rm_cmap[cv*4 + 3] = *bbufp++ = 0;
	}
 	for (cv = (base + ncolor)*4; cv < CMAPLEN*4; cv++)
		*bbufp++ = tag->rm_cmap[cv];
	tag->rm_bufp = (short *) bbufp;
/*
 * Reset the VLT pointer.
 */
# ifdef notdef
	*tag->rm_bufp++ = RM_I_WAITVR + 1;	/* Avoid flicker	*/
# endif
	*tag->rm_bufp++ = RM_I_LAM + tag->rm_vlt;
	*tag->rm_bufp++ = 0;		/* Start address for first VLT	*/
	*tag->rm_bufp++ = 0;		/* Data length.		*/
	return (GE_OK);
}






rm_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag;
int x, y, xs, ys;
char *data;
int size, org;
/*
 * This is the pixel fill routine.
 */
{
	struct rm_tag *tag = (struct rm_tag *) ctag;
/*
 * Put together the instruction.  For now, assume byte, rastor org.
 */
 	rm_check (tag, (xs*ys)/2 + 10);
	*tag->rm_bufp++ = RM_I_WI | RM_F_RP | RM_F_OF1 | RM_F_OF2 | RM_F_DF;
	*tag->rm_bufp++ = RM_F1_WIN | RM_F1_SCN;
	*tag->rm_bufp++ = RM_F2_IMG;
	*tag->rm_bufp++ = x + X_offs[tag->rm_dtype];	/* Window parameters*/
	*tag->rm_bufp++ = y + Y_offs[tag->rm_dtype];
	*tag->rm_bufp++ = x + xs - 1 + X_offs[tag->rm_dtype];
	*tag->rm_bufp++ = y + ys - 1 + Y_offs[tag->rm_dtype];
	*tag->rm_bufp++ = 0;		/* Scan mode 0		*/
	*tag->rm_bufp++ = 1;		/* Low byte mode	*/
	*tag->rm_bufp++ = xs * ys;
/*
 * Fill in the data.
 */
	memcpy (tag->rm_bufp, data, xs * ys);
	tag->rm_bufp += (xs * ys)/2;
}



rm_target (ctag, x, y)
char *ctag;
int *x, *y;
/*
 * Get the target position
 */
{
	struct rm_tag *tag = (struct rm_tag *) ctag;
	short coords[2], iosb[4], prompt;
	int status, chan;
/*
 * Get a channel to the device.
 */
	chan = rm_channel (tag->rm_dev);
/*
 * Read back the info.
 */
	prompt = RM_I_RCSP + tag->rm_cursor;
	status = sys$qiow (Ef, chan, IO$_READPROMPT | IO$M_MOUNT | IO$M_DMOUNT,
			iosb, ___, ___, coords, 4, &prompt, 2, ___, ___);
	if (s_error (status))
		ui_error ("RAMTEK cursor read error %d", status);
	if (s_error (iosb[0]))
		ui_error ("RAMTEK cursor read iosb error %d", iosb[0]);
	rm_done (chan);
/*
 * Pull out the data.
 */
	*x = coords[0] - X_offs[tag->rm_dtype];
	*y = (coords[1] & 0x3FF) - Y_offs[tag->rm_dtype];
}




rm_put_target (ctag, x, y)
char *ctag;
int x, y;
/*
 * Put the target at the given position (and make it visible)
 */
{
	struct rm_tag *tag = (struct rm_tag *) ctag;
	short iosb[4], command[3];
	int status, chan;
/*
 * Get a channel to the device.
 */
	chan = rm_channel (tag->rm_dev);
/*
 * Read back the info.
 */
	command[0] = RM_I_WCSP + tag->rm_cursor;
	command[1] = x;
	command[2] = y | 0x400;
	status = sys$qiow (Ef, chan, IO$_WRITEVBLK | IO$M_MOUNT | IO$M_DMOUNT,
		iosb, 0, 0, command, 6, 0, 0, 0, 0);
	if (s_error (status))
		ui_error ("RAMTEK cursor put error %d", status);
	rm_done (chan);
}




rm_vp (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * Set the "viewport" (zoom/origin) as close as possible to what they want.
 */
{
	struct rm_tag *tag = (struct rm_tag *) ctag;
	int xzoom, yzoom;
/*
 * Do some sanity checking.
 */
 	if (x1 <= x0 || y1 <= y0)
		return (GE_BAD_COORDS);
	if (x1 < 0 || y1 < 0 || x0 > Xres[tag->rm_dtype] ||
			y0 > Yres[tag->rm_dtype])
		return (GE_OFFSCREEN);
/*
 * Figure out our zoom factors.
 */
 	xzoom = Xres[tag->rm_dtype]/(x1 - x0);
	yzoom = Yres[tag->rm_dtype]/(y1 - y0);
	if (tag->rm_dtype != 0)	/* "Image plane" */
	{
		xzoom *= 2;
		yzoom *= 2;
	}
	if (xzoom <= 0)
		xzoom = tag->rm_dtype ? 2 : 1;
	else if (xzoom > 16)
		xzoom = 16;
	if (yzoom <= 0)
		yzoom = tag->rm_dtype ? 2 : 1;
	else if (yzoom > 16)
		yzoom = 16;
/*
 * Now put together the instructions.
 */
 	rm_check (tag, 6);
	*tag->rm_bufp++ = RM_I_ZOOM;
	*tag->rm_bufp++ = ((yzoom - 1) << 8) + xzoom - 1;
	*tag->rm_bufp++ = RM_I_SET + RM_F_OF1;
	*tag->rm_bufp++ = RM_F1_ORG;
	*tag->rm_bufp++ = x0 + X_offs[tag->rm_dtype];
	*tag->rm_bufp++ = y0 + Y_offs[tag->rm_dtype];
/*
 * All done.
 */
 	return (GE_OK);
}





# ifdef notdef


rm_fill (x1, y1, x2, y2, color)
int x1, y1, x2, y2, color;
/*
 * Fill in a box with the given color.
 */
{
	int len = (x2 - x1 + 1)*(y2 - y1 + 1);
/*
 * Make len even.
 */
	len &= ~0x1;
/*
 * Put together the instruction.
 */
	*Bp++ = RM_I_WI | RM_F_RP | RM_F_OF1 | RM_F_OF2 | RM_F_DF;
	*Bp++ = RM_F1_WIN | RM_F1_SCN;
	*Bp++ = RM_F2_IMG;
	*Bp++ = x1;		/* Window parameters	*/
	*Bp++ = y1;
	*Bp++ = x2;
	*Bp++ = y2;
	*Bp++ = 0;		/* Scan mode 0		*/
	*Bp++ = 1;		/* Low byte mode	*/
	*Bp++ = len;
/*
 * Fill in the data.
 */
	lib$movc5 (&0, &0, &color, &len, Bp);
	Bp += len/2;
}









rm_circ (x, y, rad, color)
int x, y, rad, color;
/*
 * Generate a circle with the given color.
 */
{
/*
 * Draw the outline of the circle.
 */
	*Bp++ = RM_I_CIRC | RM_F_DF | RM_F_RP | RM_F_OF1;
	*Bp++ = RM_F1_FGD;
	*Bp++ = color;		/* Set the foreground color. */
	*Bp++ = 6;		/* 6 bytes = 1 circle	*/
	*Bp++ = x;		/* X coordinate of center	*/
	*Bp++ = y;		/* Y coordinate of center	*/
	*Bp++ = rad;		/* Radius of circle, in X pixels */
/*
 * Now we fill the circle.  The manual promises that COP will be set
 * to the center of this circle, so I can just say "go".
 */
	*Bp++ = RM_I_FILL | RM_F_DF | RM_F_RP | RM_F_OF2;
	*Bp++ = RM_F2_RMSK;
	*Bp++ = 0xFF;		/* Set read mask to everything.	*/
	*Bp++ = 2;		/* 2 bytes of data		*/
	*Bp++ = 0;		/* FILL-UNTIL mode		*/
}






rm_point (x, y)
int x, y;
/*
 * Write the given point.
 */
{
	*Bp++ = RM_I_WPT | RM_F_RP | RM_F_DF;
	*Bp++ = 4; /* Data length */
	*Bp++ = x;
	*Bp++ = y;
}







# endif




rm_hcw (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * Set the hardware clip window for this device.
 */
{
	struct rm_tag *tag = (struct rm_tag *) ctag;

 	rm_check (tag, 13);
/*
 * Put together a SET instruction to do all this stuff.
 */
	*tag->rm_bufp++ = RM_I_SET | RM_F_RP | RM_F_OF1 | RM_F_OF2;
	*tag->rm_bufp++ = RM_F1_WIN;
	*tag->rm_bufp++ = RM_F2_WEW | RM_F2_WOF;
	*tag->rm_bufp++ = x0 + X_offs[tag->rm_dtype]; /* Format window.	*/
	*tag->rm_bufp++ = y0 + Y_offs[tag->rm_dtype];
        *tag->rm_bufp++ = X_offs[tag->rm_dtype] + x1;
        *tag->rm_bufp++ = Y_offs[tag->rm_dtype] + y1;
	*tag->rm_bufp++ = X_offs[tag->rm_dtype] + x0; /* Write enable window.*/
	*tag->rm_bufp++ = Y_offs[tag->rm_dtype] + y0;
	*tag->rm_bufp++ = X_offs[tag->rm_dtype] + x1;
        *tag->rm_bufp++ = Y_offs[tag->rm_dtype] + y1;
	*tag->rm_bufp++ = X_offs[tag->rm_dtype] + x0;	/* WEW offset */
	*tag->rm_bufp++ = Y_offs[tag->rm_dtype] + y0;
/*
 * Save the window coordinates
 */
	tag->cx0 = x0;	tag->cx1 = x1;
	tag->cy0 = y0;	tag->cy1 = y1;
}
# endif /* DEV_RAMTEK */
