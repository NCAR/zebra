/* 10/88 jc */
/*
 * Sunview driver.
 */

# include "param.h"

# ifdef DEV_SUNVIEW

# include <suntool/sunview.h>
# include <suntool/canvas.h>
# include "graphics.h"

/*
 * Our display size.
 */
# define DEV_HEIGHT 500
# define DEV_WIDTH 500

# define NCOLOR 128
/*
 * The device tag type.
 */
struct sv_tag
{
	Frame	sv_frame;	/* The outer frame		*/
	Canvas	sv_canvas;	/* The canvas			*/
	Pixwin	*sv_pixwin;	/* The pixwin for the canvas	*/
	int	sv_locked;	/* Did we do a window lock?	*/
	int	sv_x, sv_y;	/* Dimensions of our window	*/
	char	*sv_image;	/* Our window image.		*/
	char	*sv_bigimage;	/* Our *big* window image	*/
	int	sv_xzoom;	/* Where we are zoomed to	*/
	int	sv_yzoom;	/* ""				*/
	char	sv_imcur;	/* Is our image data current?	*/
	char	sv_zoom;	/* Are we zoomed up (vp)	*/
	char	sv_zwant;	/* Should we be zoomed?		*/
};



struct sv_tag *Tag;	/* KLUDGE!!!!!!!!!!!!!!!!! */



sv_open (device, type, ctag)
char *device, *type, **ctag;
/*
 * Open up the window system.
 */
{
	struct sv_tag *tag = (struct sv_tag *) getvm (sizeof (struct sv_tag));
	unsigned char c, col[NCOLOR], nc = 128;
	void sv_event ();

	Tag = tag;
	if (! strcmp (type, "sunview500") || ! strcmp (type, "sun500"))
		tag->sv_x = tag->sv_y = 500;
	else if (! strcmp (type, "sun700") || ! strcmp (type, "sun"))
		tag->sv_x = tag->sv_y = 700;
# ifdef notdef
	else if (! strcmp (type, "sun256"))
	{
		tag->sv_x = tag->sv_y = 500;
		nc = 256;
	}
# endif
	else
		c_panic ("Unknown SUN type: '%s'", type);
/*
 * Create our data structures.  Note that the device argument is 
 * ignored for now.
 */
 	tag->sv_frame = window_create (NULL, FRAME, FRAME_LABEL,
		"RDSS graphics output", WIN_HEIGHT, tag->sv_y + 32,
		WIN_WIDTH, tag->sv_x, 0);
	tag->sv_canvas = window_create (tag->sv_frame, CANVAS,
		CANVAS_RETAINED, FALSE,
		CANVAS_HEIGHT, tag->sv_y, CANVAS_WIDTH, tag->sv_x, 0);
	tag->sv_pixwin = canvas_pixwin (tag->sv_canvas);
	tag->sv_image = tag->sv_bigimage = 0;
	tag->sv_zwant = tag->sv_zoom = tag->sv_imcur = FALSE;
/*
 * Create a name for our colormap.  This could become a problem if 
 * two different graphics-package based programs run on the same display
 * at the same time, but I won't worry about that for now.  The window system
 * requires that you create your color map immediately, so I will throw in
 * a simple grayscale for now.
 */
 	pw_setcmsname (tag->sv_pixwin, "fof-graphics");
	for (c = 0; c < nc; c++)
		col[c] = (c*255)/nc;
	pw_putcolormap (tag->sv_pixwin, 0, nc, col, col, col);
	window_set (tag->sv_canvas, CANVAS_RETAINED, TRUE, 0);
/*
 * Make the frame visible, and enable mouse events.
 */
 	window_set (tag->sv_frame, WIN_SHOW, TRUE, 0);
	window_set (tag->sv_frame, WIN_CONSUME_PICK_EVENTS, WIN_NO_EVENTS,
		WIN_MOUSE_BUTTONS, 0, 0);
	window_set (tag->sv_canvas, WIN_CONSUME_PICK_EVENTS, WIN_NO_EVENTS,
		WIN_MOUSE_BUTTONS, 0, 0);
	window_set (tag->sv_canvas, WIN_CONSUME_KBD_EVENTS, WIN_NO_EVENTS,
		WIN_TOP_KEYS, 0, 0);
	window_set (tag->sv_canvas, WIN_EVENT_PROC, sv_event, 0);
/*
 * Fire off the "implicit" dispatcher.
 */
 	notify_do_dispatch ();
/*
 * Return our tag.
 */
	*ctag = (char *) tag;
	return (GE_OK);
}




void
sv_event (window, event, arg)
Window window;
Event *event;
caddr_t arg;
/*
 * This is the event handler called from Sunview.
 */
{
	sv_deal_with_event (Tag, event);
}





sv_deal_with_event (tag, event)
struct sv_tag *tag;
Event *event;
/*
 * Deal with an event on this window.
 */
{
	switch (event_action (event))
	{
	    case KEY_TOP (1):
	    	sv_qzoom (tag, 0, 0);
		break;

	    case KEY_TOP (2):
	    	sv_qzoom (tag, tag->sv_x/2, 0);
		break;
	    
	    case KEY_TOP (3):
	    	sv_qzoom (tag, 0, tag->sv_y/2);
		break;

	    case KEY_TOP (4):
	    	sv_qzoom (tag, tag->sv_x/2, tag->sv_y/2);
		break;

	    case KEY_TOP (5):
	    	if (tag->sv_zoom && tag->sv_image)
		{
			tag->sv_zoom = FALSE;
			sv_pixel (tag, 0, 0, tag->sv_x, tag->sv_y,
				tag->sv_image, 0, 0);
			sv_flush (tag);
		}
		break;
	}
}




sv_close (ctag)
char *ctag;
/*
 * Close the window system device.
 */
{
	struct sv_tag *tag = (struct sv_tag *) ctag;

	window_set (tag->sv_frame, FRAME_NO_CONFIRM, TRUE, 0);
	window_destroy (tag->sv_canvas);
	window_destroy (tag->sv_frame);
	if (tag->sv_image)
	{
		relvm (tag->sv_image);
		relvm (tag->sv_bigimage);
	}
	relvm (tag);
}



sv_flush (ctag)
char *ctag;
/*
 * The flush routine.
 */
{
	struct sv_tag *tag = (struct sv_tag *) ctag;
/*
 * Get everything out to the screen.
 */
	if (tag->sv_locked)
	{
		tag->sv_locked = FALSE;
		pw_batch_off (tag->sv_pixwin);
	}
	pw_show (tag->sv_pixwin);
/*
 * If we are running in a zoomed mode, we now need to go back to
 * the zoomed state.
 */
 	if (tag->sv_zwant)
	{
		tag->sv_zwant = FALSE;
		sv_qzoom (tag, tag->sv_xzoom, tag->sv_yzoom);
	}
	/* notify_dispatch (); */
}




sv_clear (ctag)
char *ctag;
/*
 * Clear the window.
 */
{
	struct sv_tag *tag = (struct sv_tag *) ctag;

	pw_writebackground (tag->sv_pixwin, 0, 0, tag->sv_x, tag->sv_y,
		PIX_SRC);
	tag->sv_imcur = FALSE;
}




sv_color (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * The color map routine.
 */
{
	unsigned char red[256], blue[256], green[256];
	int col;
	struct sv_tag *tag = (struct sv_tag *) ctag;
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
	pw_putcolormap (tag->sv_pixwin, base, ncolor, red, green, blue);
}




sv_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * The polyline routine.
 */
{
	struct pr_pos *pos;
	int pt;
	struct sv_tag *tag = (struct sv_tag *) ctag;
	struct pr_brush brush;

	sv_lock (tag);
	sv_change (tag);
/*
 * Convert the polyline points over to the sunview format.
 */
	pos = (struct pr_pos *) getvm (npt*sizeof (struct pr_pos));
	for (pt = 0; pt < npt; pt++)
	{
		pos[pt].x = *data++;
		pos[pt].y = tag->sv_y - (*data++ + 1);
	}
/*
 * Draw the line.  I am blowing off texture for the moment.
 */
	brush.width = 1;
	pw_polyline (tag->sv_pixwin, 0, 0, npt, pos, POLY_DONTCLOSE,
		&brush, (struct pr_texture *) 0, PIX_SRC | PIX_COLOR (color));
}





sv_lock (tag)
struct sv_tag *tag;
/*
 * Put a lock on the display.
 */
{
	if (tag->sv_locked)
		return;
	pw_batch_on (tag->sv_pixwin);
	tag->sv_locked = TRUE;
}





sv_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag, *data;
int x, y, xs, ys, size, org;
/*
 * The pixel fill routine.
 */
{
	Pixrect *pr;
	struct sv_tag *tag = (struct sv_tag *) ctag;
/*
 * Create a memory pixrect from the data.  There is an implicit assumption
 * here that the incoming data is already padded to a 16-bit boundary.
 */
	/* printf ("Pixel, (%d, %d) size (%d, %d)\n", x, y, xs, ys); */
	sv_change (tag);
	pr = mem_point (xs, ys, 8, (short *) data);
/*
 * Now ROP it out to the display.
 */
 	pw_rop (tag->sv_pixwin, x, tag->sv_y - (y + ys), xs, ys, PIX_SRC,
		pr, 0, 0);
/*
 * We're done with the pixrect now.
 */
 	pr_close (pr);
}




sv_readscreen (ctag, x, y, xs, ys, data)
char *ctag, *data;
int x, y, xs, ys;
/*
 * Read back a chunk of screen.
 */
{
	Pixrect *pr;
	struct sv_tag *tag = (struct sv_tag *) ctag;
/*
 * Create a memory pixrect from the data.  There is an implicit assumption
 * here that the incoming data is already padded to a 16-bit boundary.
 */
	/* printf ("Pixel, (%d, %d) size (%d, %d)\n", x, y, xs, ys); */
	pr = mem_point (xs, ys, 8, (short *) data);
/*
 * Read in the data.
 */
	pw_read (pr, 0, 0, xs, ys, PIX_SRC, tag->sv_pixwin, x, y);
/*
 * We're done with the pixrect now.
 */
 	pr_close (pr);
}




sv_target (ctag, x, y)
char *ctag;
int *x, *y;
/*
 * Read the target.
 */
{
	struct sv_tag *tag = (struct sv_tag *) ctag;
	Event ev;
	int fd;

	fd = (int) window_get (tag->sv_canvas, WIN_FD);
/*
 * The system likes to tell us when the mouse leaves the window, even
 * though it has not been requested.  So, we hang out and wait until
 * we actually get a button event.
 */
	do
	{
		input_readevent (fd, &ev);
	} while (event_action (&ev) == KBD_DONE);

	*x = event_x (&ev);
	*y = tag->sv_y - event_y (&ev) - 1;
	if (tag->sv_zoom)
	{
		*x = tag->sv_xzoom + (*x)/2;
		*y = (tag->sv_yzoom ? 0 : tag->sv_y/2) + (*y)/2;
	}
}






sv_pick (ctag, button, x, y)
char *ctag;
int *button, *x, *y;
/*
 * Perform a pick on the screen.
 */
{
	struct sv_tag *tag = (struct sv_tag *) ctag;
	Event ev;
	int fd, got_button = FALSE;

	fd = (int) window_get (tag->sv_canvas, WIN_FD);
/*
 * The system likes to tell us when the mouse leaves the window, even
 * though it has not been requested.  So, we hang out and wait until
 * we actually get a button event.
 */
	while (! got_button)
	{
		input_readevent (fd, &ev);
		switch (event_action (&ev))
		{
		   case MS_LEFT:
		   case MS_MIDDLE:
		   case MS_RIGHT:
		   	got_button = TRUE;
		   	break;
		   case KEY_TOP (1):
		   case KEY_TOP (2):
		   case KEY_TOP (3):
		   case KEY_TOP (4):
		   case KEY_TOP (5):
		   	sv_deal_with_event (tag, &ev);
			break;
		}
	}

/*
 * OK, we got something.  Return the info.
 */
	*x = event_x (&ev);
	*y = tag->sv_y - event_y (&ev) - 1;
	switch (event_action (&ev))
	{
	   case MS_LEFT: 	*button = 0; break;
	   case MS_MIDDLE:	*button = 1; break;
	   case MS_RIGHT:	*button = 2; break;
	   default:
	   	printf ("Funky event return: %d\n", event_action (&ev));
		*button = 99;
	}
/*
 * If the screen is zoomed, take that into account.
 */
	if (tag->sv_zoom)
	{
		*x = tag->sv_xzoom + (*x)/2;
		*y = (tag->sv_yzoom ? 0 : tag->sv_y/2) + (*y)/2;
	}
	return (GE_OK);
}





sv_vp (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * This is a fake "viewport" routine, intended to allow for basic perusal
 * /editor session control.
 */
{
	struct sv_tag *tag = (struct sv_tag *) ctag;
	int whole;
/*
 * See what sort of zoom is wanted.
 */
	whole = (x1 - x0)*3 > tag->sv_x*2 && (y1 - y0)*3 > tag->sv_y*2;
/*
 * Whole zooms are easy.  Either we go nowhere, or we just pump out the
 * saved image.
 */
 	if (whole)
	{
		if (! tag->sv_zoom)
			return;
		if (! tag->sv_image)
			c_panic ("Dev_sunview: zoomed with no image!");
		tag->sv_zoom = FALSE;
		sv_pixel (ctag, 0, 0, tag->sv_x, tag->sv_y, tag->sv_image,
			0, 0);
		sv_flush (ctag);
	}
/*
 * Otherwise, things get a little harder.
 */
 	else
	{
		if (x0 > 0)
			x0 = tag->sv_x/2;
		y0 = (y0 == 0) ? tag->sv_y/2 : 0;
		sv_qzoom (tag, x0, y0);
	}
	return (GE_OK);
}





sv_qzoom (tag, x0, y0)
struct sv_tag *tag;
int x0, y0;
/*
 * Perform a quadrant zoom.
 */
{
	register int rast, col, lwidth = 2 * tag->sv_x;
	register char *src, *dest;
/*
 * Note this zoom.
 */
 	tag->sv_xzoom = x0;
	tag->sv_yzoom = y0;
/*
 * If we have our information in place, we simply shove it out.
 */
	if (tag->sv_image && tag->sv_imcur)
	{
		sv_display_zoom (tag, x0, y0);
		return;
	}
/*
 * Nope.  Allocate (lots of) memory if we have to.
 */
	if (! tag->sv_image)
	{
		tag->sv_image = getvm (tag->sv_x * tag->sv_y);
		tag->sv_bigimage = getvm (4 * tag->sv_x * tag->sv_y);
	}
/*
 * Pull back the current screen image.
 */
	sv_readscreen (tag, 0, 0, tag->sv_x, tag->sv_y, tag->sv_image);
/*
 * Expand it into our large array.
 */
	src = tag->sv_image;
	dest = tag->sv_bigimage;
 	for (rast = 0; rast < tag->sv_y; rast++)
	{
		for (col = 0; col < tag->sv_x; col++)
		{
			register char c = *src++;
			*dest++ = c;
			*dest++ = c;
		}
		memcpy (dest, dest - lwidth, lwidth);
		dest += lwidth;
	}
	tag->sv_imcur = TRUE;
/*
 * Display it.
 */
 	sv_display_zoom (tag, x0, y0);
}





sv_display_zoom (tag, x0, y0)
struct sv_tag *tag;
int x0, y0;
/*
 * Display a zoomed image.
 */
{
	Pixrect *pr;
/*
 * Create a memory pixrect from the data.  There is an implicit assumption
 * here that the incoming data is already padded to a 16-bit boundary.
 */
	pr = mem_point (2*tag->sv_x, 2*tag->sv_y, 8,
		(short *) tag->sv_bigimage);
/*
 * Now ROP it out to the display.
 */
 	pw_rop (tag->sv_pixwin, 0, 0, tag->sv_x, tag->sv_y, PIX_SRC, pr,
		2*x0, 2*y0);
/*
 * We're done with the pixrect now.
 */
 	pr_close (pr);
/*
 * Flush out the change now.
 */
 	sv_flush (tag);
	tag->sv_zoom = TRUE;
}





sv_change (tag)
struct sv_tag *tag;
/*
 * Deal with the changing display, especially as it has to do with zooming.
 */
{
/*
 * In any case, a saved image is not current.
 */
	tag->sv_imcur = FALSE;
/*
 * If we are in a zoomed mode, go back to full screen for the duration of
 * the changes.  (Send the changes out directly with pw_show -- sv_flush
 * would put the zoom back!)
 */
 	if (tag->sv_zoom)
	{
		if (! tag->sv_image)
			c_panic ("Dev_sunview: zoomed with no image!");
		tag->sv_zoom = FALSE;
		tag->sv_zwant = TRUE;
		sv_pixel (tag, 0, 0, tag->sv_x, tag->sv_y, tag->sv_image,
			0, 0);
		pw_show (tag->sv_pixwin);
	}
}


# endif /* DEV_SUNVIEW */
