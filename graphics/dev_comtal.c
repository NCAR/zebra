/* 7/87 jc */
/*
 * COMTAL graphics driver.
 */
# include "param.h"

# ifdef DEV_COMTAL

# include "graphics.h"
# include "device.h"
# include "lib_include:lib_proto.h"

# define ___ 0
# define s_error(s) (((s) & 0x1) == 0)
# define RDSS_TABLET

/*
 * COMTAL I/O functions.
 */
# define IOF_IOW	0x0030		/* I/O Write		*/
# define IOF_IOWP	0x0230		/* I/O Write "prime"	*/
# define IOF_IORP	0x0231		/* I/O Read "prime"	*/

/*
 * Our tag structure.
 */
struct zbtag
{
	int	zb_channel;		/* Our channel to the device	*/
	int	zb_ef;			/* Event flag			*/
	int	zb_image;		/* Image plane number		*/
	short	zb_cmap[3] [256];	/* Color map storage.		*/
};




zb_open (device, type, ctag)
char *device, *type, **ctag;
struct device *dev;
/*
 * Open up the COMTAL device.
 */
{
	struct zbtag *tag = (struct zbtag *) getvm (sizeof (struct zbtag));
	int status;
/*
 * Get a channel to the device.
 */
	status = sys$assign (descr (device), &tag->zb_channel, ___, ___);
	if (s_error (status))
		return (GE_BAD_DEVICE);
/*
 * Get an event flag.
 */
 	lib$get_ef (&tag->zb_ef);
/*
 * Figure out our image plane number.
 */
 	if (! strcmp (type, "comtal") || ! strcmp (type, "comtal1"))
		tag->zb_image = 0;
	else if (! strcmp (type, "comtal2"))
		tag->zb_image = 1;
	else if (! strcmp (type, "comtal3"))
		tag->zb_image = 2;
	else if (! strcmp (type, "comtal4"))
		tag->zb_image = 3;
	else
		c_panic ("Bad type (%s) for COMTAL open", type);

	*ctag = (char *) tag;
	return (GE_OK);
}




zb_close (ctag)
char *ctag;
/*
 * Close this device.
 */
{
	struct zbtag *tag = (struct zbtag *) ctag;

	sys$dassgn (tag->zb_channel);
	lib$free_ef (&tag->zb_ef);
	relvm (tag);
}





zb_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag;
int x, y, xs, ys;
char *data;
int size, org;
/*
 * Perform a pixel fill on this device.
 */
{
	struct zbtag *tag = (struct zbtag *) ctag;
	short coords[4], iosb[4], igp[6];
	int status, mode;
/*
 * Assume byte, rastor org, since that's all that will work.  First thing
 * we do is to set up our coords and ship them over.
 */
	coords[0] = x;
	coords[1] = 512 - (y + ys);
	coords[2] = x + xs - 1;
	coords[3] = 511 - y;
/*
 * Do the coord transfer.
 */
# ifdef RDSS_TABLET
 	zb_dioff ();
# endif
	mode = 0x6000 | (tag->zb_image << 11);
	status = sys$qiow (tag->zb_ef, tag->zb_channel, IOF_IOWP, iosb,
		___, ___, coords, 8 /* sizeof (coords) */, ___,
		5 /* Code 1/dma */, mode, ___);
	if (s_error (status))
		sys_error (status, "Coord trans status error");
	else if (s_error (iosb[0]))
		sys_error (iosb[0], "Coord trans iosb error");
/*
 * Next we get to put together an IGP block.
 */
 	igp[0] = igp[1] = tag->zb_image;
	igp[2] = xs * ys;
	igp[3] = xs/2;
	igp[4] = ys;
	igp[5] = 1;	/* Use those coords we sent! */
/*
 * Send the silly thing over.
 */
	mode = 0x3000 | tag->zb_image;
	status = sys$qiow (tag->zb_ef, tag->zb_channel, IOF_IOWP, iosb,
		___, ___, igp, 12 /* sizeof (igp) */, ___,
		7 /* Code 3/dma */, mode, ___);
	if (s_error (status))
		sys_error (status, "IGP trans status error");
	else if (s_error (iosb[0]))
		sys_error (iosb[0], "IGP trans iosb error");
/*
 * After that whole ringamarole, we can actually think about sending the
 * data block.
 */
 	mode = tag->zb_image << 11;
	status = sys$qiow (tag->zb_ef, tag->zb_channel, IOF_IOWP, iosb,
		___, ___, data, xs*ys, ___, 5 /* Code 1/dma */, mode, ___);
	if (s_error (status))
		sys_error (status, "data trans status error");
	else if (s_error (iosb[0]))
		sys_error (iosb[0], "data trans iosb error");
/*
 * Done!
 */
}





zb_flush (ctag)
char *ctag;
/*
 * This is the flush routine.  However, since we buffer no data, there
 * is nothing to flush.
 */
{
# ifdef RDSS_TABLET
	zb_dion ();
# endif
}





zb_color (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * Perform a color map change.
 */
{
	short carray[256], iosb[4];
	int i, status;
	struct zbtag *tag = (struct zbtag *) ctag;
	float *ic[3] = { g, r, b };
# ifdef RDSS_TABLET
	zb_dioff ();
# endif
/*
 * Now do the three colors.
 */
	for (i = 0; i < 3; i++)
	{
		int nc;
		float *color = ic[i];

		for (nc = 0; nc < ncolor; nc++)
			tag->zb_cmap[i][nc + base] = (int) (*color++ * 255.0);
		status = sys$qiow (tag->zb_ef, tag->zb_channel, IOF_IOWP,
			iosb, ___, ___, tag->zb_cmap[i], 512, ___, 6,
			0x2000 | (i << 8), ___);
		if (s_error (status))
			sys_error (status, "color trans status error");
		else if (s_error (iosb[0]))
			sys_error (iosb[0], "color trans iosb error");
	/*
	 * It apparently takes the COMTAL a while to digest each one of
	 * these...
	 */
		if (i != 2)
			sleep (1);
	}
# ifdef RDSS_TABLET
	zb_dion ();
# endif
	return (GE_OK);
}




zb_clear () {}



zb_target (ctag, x, y)
char *ctag;
int *x, *y;
/*
 * Get the target location.
 */
{
	struct zbtag *tag = (struct zbtag *) ctag;
	short xy[4], iosb[4];
	int status, mode;

# ifdef RDSS_TABLET
 	zb_dioff ();
# endif
	mode = 0x8000;
	status = sys$qiow (tag->zb_ef, tag->zb_channel, IOF_IORP, iosb,
		___, ___, xy, 4 /* sizeof (xy) */, ___,
		7 /* Code 3/dma */, mode, ___);
	if (s_error (status))
		sys_error (status, "Read target status error");
	else if (s_error (iosb[0]))
		sys_error (iosb[0], "Read target iosb error");
/*
 * Pull out the X and Y coords
 */
	*x = xy[0];
	*y = 512 - xy[1];

# ifdef RDSS_TABLET
	zb_dion ();
# endif

	return (GE_OK);
}

zb_put_target (ctag, x, y)
char *ctag;
int x, y;
/*
 * Put the target at the given location.
 */
{
	struct zbtag *tag = (struct zbtag *) ctag;
	short iosb[4];
	int status;

# ifdef RDSS_TABLET
	zb_dioff ();
# endif
/*
 * First send the X coordinate.
 */
	status = sys$qiow (tag->zb_ef, tag->zb_channel, IOF_IOWP,
		iosb, ___, ___, ___, 0, ___, 3 /* code 3 no DMA */,
		x, ___);
	if (s_error (status))
		sys_error (status, "Move target (x) status error");
	else if (s_error (iosb[0]))
		sys_error (iosb[0], "Move target (x) iosb error");
/*
 * Now send the Y coordinate.
 */
	status = sys$qiow (tag->zb_ef, tag->zb_channel, IOF_IOWP,
		iosb, ___, ___, ___, 0, ___, 3 /* code 3 no DMA */,
		(0x4000 | (512 - y)), ___);
	if (s_error (status))
		sys_error (status, "Move target (y) status error");
	else if (s_error (iosb[0]))
		sys_error (iosb[0], "Move target (y) iosb error");
# ifdef RDSS_TABLET
	zb_dion ();
# endif
	return (GE_OK);
}



# ifdef RDSS_TABLET
static int Di_off = FALSE;

zb_dioff ()
{
	if (! Di_off)
		zb_nodimode ();
	Di_off = TRUE;
}


zb_dion ()
{
	if (Di_off)
		zb_dimode ();
	Di_off = FALSE;
}
# endif

# endif /* DEV_COMTAL */
