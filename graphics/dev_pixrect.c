/* 10/88 jc */
/*
 * Pixrect driver.
 */

# include "param.h"

# ifdef DEV_PIXRECT

# include <pixrect/pixrect_hs.h>
# include "graphics.h"
# include "device.h"

/*
 * Our display size.
 */
# define DEV_HEIGHT  900
# define DEV_WIDTH 1152

/*
 * The device tag type.
 */
struct pix_tag
{
	Pixrect *pix_pr;		/* Main pixrect			*/
	Pixrect *pix_reg;	/* Regional pixrect		*/
	int	pix_locked;	/* Did we do a window lock?	*/
	int	pix_x, pix_y;	/* Dimensions of our window	*/
};






pix_open (device, type, ctag, dev)
char *device, *type, **ctag;
struct device *dev;
/*
 * Open up the window system.
 */
{
	struct pix_tag *tag = (struct pix_tag *) getvm (sizeof (struct pix_tag));
	unsigned char c, col[128];

	tag->pix_x = 1152;
	tag->pix_y = 900;
/*
 * Create our data structures.  Note that the device argument is 
 * ignored for now.
 */
	tag->pix_pr = pr_open ("/dev/fb");
/*	tag->pix_reg = pr_region (tag->pix_pr, 200, 200, 500, 500); */
	tag->pix_reg = tag->pix_pr;

/*
 * Return our tag.
 */
	*ctag = (char *) tag;
	return (GE_OK);
}





pix_close (ctag)
char *ctag;
/*
 * Close the window system device.
 */
{
	struct pix_tag *tag = (struct pix_tag *) ctag;

	pr_close (tag->pix_pr);
	relvm (tag);
}



pix_flush (ctag)
char *ctag;
/*
 * The flush routine.
 */
{
# ifdef notdef
	struct pix_tag *tag = (struct pix_tag *) ctag;

	if (tag->pix_locked)
	{
		tag->pix_locked = FALSE;
		pw_batch_off (tag->pix_pixwin);
	}
	pw_show (tag->pix_pixwin);
	/* notify_dispatch (); */
# endif
}




pix_clear (ctag)
char *ctag;
/*
 * Clear the window.
 */
{
# ifdef notdef
	struct pix_tag *tag = (struct pix_tag *) ctag;

	pw_writebackground (tag->pix_pixwin, 0, 0, tag->pix_x, tag->pix_y,
		PIX_SRC);
# endif
}




pix_color (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * The color map routine.
 */
{
	unsigned char red[256], blue[256], green[256];
	int col;
	struct pix_tag *tag = (struct pix_tag *) ctag;
/*
 * Convert the colors over to byte format.
 */
 	for (col = 0; col < ncolor; col++)
	{
		red[col] = (unsigned char) (r[col]*255.0);
		blue[col] = (unsigned char) (b[col]*255.0);
		green[col] = (unsigned char) (g[col]*255.0);
	}
/*
 * Now send them in.
 */
 	pr_putcolormap (tag->pix_pr, base, ncolor, red, green, blue);
}




pix_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * The polyline routine.
 */
{
	struct pr_pos *pos;
	int pt;
	struct pix_tag *tag = (struct pix_tag *) ctag;
	struct pr_brush brush;

/*
 * Convert the polyline points over to the sunview format.
 */
	pos = (struct pr_pos *) getvm (npt*sizeof (struct pr_pos));
	for (pt = 0; pt < npt; pt++)
	{
		pos[pt].x = *data++;
		pos[pt].y = tag->pix_y - (*data++ + 1);
	}
/*
 * Draw the line.  I am blowing off texture for the moment.
 */
	brush.width = 1;
	pr_polyline (tag->pix_reg, 0, 0, npt, pos, POLY_DONTCLOSE,
		&brush, (struct pix_texture *) 0, PIX_SRC | PIX_COLOR (color));
}





pix_lock (tag)
struct pix_tag *tag;
/*
 * Put a lock on the display.
 */
{
# ifdef notdef
	if (tag->pix_locked)
		return;
	pw_batch_on (tag->pix_pixwin);
	tag->pix_locked = TRUE;
# endif
}





pix_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag, *data;
int x, y, xs, ys, size, org;
/*
 * The pixel fill routine.
 */
{
	Pixrect *pr;
	struct pix_tag *tag = (struct pix_tag *) ctag;
/*
 * Create a memory pixrect from the data.  There is an implicit assumption
 * here that the incoming data is already padded to a 16-bit boundary.
 */
	/* printf ("Pixel, (%d, %d) size (%d, %d)\n", x, y, xs, ys); */
	pr = mem_point (xs, ys, 8, (short *) data);
/*
 * Now ROP it out to the display.
 */
 	pr_rop (tag->pix_reg, x, tag->pix_y - (y + ys), xs, ys, PIX_SRC,
		pr, 0, 0);
/*
 * We're done with the pixrect now.
 */
 	pr_close (pr);
}




# ifdef notdef
pix_readscreen (ctag, x, y, xs, ys, data)
char *ctag, *data;
int x, y, xs, ys;
/*
 * Read back a chunk of screen.
 */
{
	Pixrect *pr;
	struct pix_tag *tag = (struct pix_tag *) ctag;
/*
 * Create a memory pixrect from the data.  There is an implicit assumption
 * here that the incoming data is already padded to a 16-bit boundary.
 */
	/* printf ("Pixel, (%d, %d) size (%d, %d)\n", x, y, xs, ys); */
	pr = mem_point (xs, ys, 8, (short *) data);
/*
 * Read in the data.
 */
	pw_read (pr, 0, 0, xs, ys, PIX_SRC, tag->pix_reg, x, y);
/*
 * We're done with the pixrect now.
 */
 	pix_close (pr);
}
# endif


# endif /* DEV_PIXRECT */
