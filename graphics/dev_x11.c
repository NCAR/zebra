/* 12/88 jc */
/* $Id: dev_x11.c,v 1.8 1989-09-28 11:28:39 burghart Exp $	*/
/*
 * Graphics driver for the X window system, version 11.3
 */
# include "config.h"

# ifdef DEV_X11

# include "graphics.h"
# include "device.h"
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/cursorfont.h>

# define TGT_SIZE	17

# define ABS(x)		((x) < 0 ? -(x) : x)

# define TRUE	1
# define FALSE	0

typedef struct {
	short x, y;
} Xpoint;

/*
 * The device structure.
 */
struct xtag
{
	Display	*x_display;	/* The display structure	*/
	Window 	x_window;	/* The window.			*/
	GC	x_gc;		/* Graphics context		*/
	Cursor	x_cursor;	/* The cursor for this window	*/
	Visual	*x_visual;	/* The visual			*/
	int	x_xres, x_yres;	/* The resolution of our window	*/
	int	x_mono;		/* Is this a mono screen?	*/
	long	x_fg, x_bg;	/* Foreground and background colors	*/
	int 	*x_dev_bg;	/* KLUGE: pointer to bg color in dev struct */
	int	x_xtgt, x_ytgt;	/* Target position			*/
	GC	x_tgt_gc;	/* Target GC (no clipping)		*/
	int	x_do_pxm;	/* Do we need to do the target pixmap?	*/
	Pixmap	x_tgt_pixmap;	/* Pixmap to save data covered by target*/
};


/*
 * Dash patterns.
 */
static char Dp_dash[2] = { 8, 8 };
static char Dp_dot[2] = { 2, 4 };
static char Dp_dash_dot[4] = { 8, 4, 2, 4 };

static struct 
{
	char	*d_pattern;
	int	d_ndash;
} D_table[4] =
{
	{ 0, 0 },
	{ Dp_dash,	2 },
	{ Dp_dot,	2 },
	{ Dp_dash_dot,	4 }
};




x11_open (device, type, ctag, dev)
char *device, *type, **ctag;
struct device *dev;
/*
 * Open up an X11 window device.
 */
{
	struct xtag *tag = (struct xtag *) getvm (sizeof (struct xtag));
	XSetWindowAttributes attr;
	XGCValues gcontext;
	XVisualInfo template, *vlist;
	int screen, pv[2], pm, nmatch, depth;
	XEvent ev;
/*
 * Figure out what our resolution will be.
 */
 	if (! strcmp (type, "X11") || ! strcmp (type, "X700") ||
		! strcmp (type, "x11") || ! strcmp (type, "x700"))
		tag->x_xres = tag->x_yres = 700;
	else
		tag->x_xres = tag->x_yres = 500;
/*
 * First, open up our display.  If the device is "screen", convert it to
 * unix:0; otherwise take the device as given.
 */
	tag->x_display = XOpenDisplay (device && strcmp (device, "screen") ? 
			device : "unix:0.0");
	if (! tag->x_display)
	{
		relvm (tag);
		return (GE_BAD_DEVICE);
	}
	screen = DefaultScreen (tag->x_display);
	tag->x_mono = DefaultDepth (tag->x_display, screen) == 1;

	tag->x_fg = 1;
	tag->x_bg = 0;
/*
 * No target for now
 */
	tag->x_xtgt = -1;
	tag->x_ytgt = -1;
/*
 * Try to find a pseudocolor visual.  If we succeed, we use it; otherwise
 * it's monochrome city.
 */
# ifdef notdef
	template.screen = screen;
	template.depth = 8;
	template.class = PseudoColor;
	vlist = XGetVisualInfo (tag->x_display,
		VisualScreenMask | VisualDepthMask | VisualClassMask, 
		&template, &nmatch);
	printf ("We have %d visual matches\n", nmatch);
	tag->x_mono = ! nmatch;
	if (nmatch)
	{
		tag->x_visual = vlist->visual;
		depth = 8;
	}
	else
	{
		tag->x_visual = DefaultVisual (tag->x_display, screen);
		depth = CopyFromParent;
	}
	printf ("Visual is 0x%x\n", tag->x_visual);
 	XFree (vlist);
# endif
	tag->x_visual = DefaultVisual (tag->x_display, screen);
	depth = CopyFromParent;
/*
 * Create the window to exist on that display.
 */
	attr.background_pixel = BlackPixel (tag->x_display, screen);
	attr.border_pixel = WhitePixel (tag->x_display, screen);
	attr.backing_store = WhenMapped;
	attr.event_mask = ButtonPressMask | ExposureMask;
	tag->x_window = XCreateWindow (tag->x_display,
		RootWindow (tag->x_display, screen), 10, 10, tag->x_xres, 
		tag->x_yres, 2, depth, InputOutput, tag->x_visual,
		CWBackPixel|CWBorderPixel|CWBackingStore|CWEventMask, &attr);
/*
 * Store some properties.
 */
	XStoreName (tag->x_display, tag->x_window, "RDSS Graphics");
	XSetIconName (tag->x_display, tag->x_window, "Graphics");
/*
 * Map the window.  (Sync it to get the wmgr mapping process underway.)
 */
 	XMapWindow (tag->x_display, tag->x_window);
/*
 * Get the pixmap to save the area covered by the target
 */
	tag->x_tgt_pixmap = XCreatePixmap (tag->x_display, tag->x_window, 
		TGT_SIZE + 2, TGT_SIZE + 2,
		DefaultDepth (tag->x_display, screen));
/*
 * Get a graphics context.
 */
 	gcontext.foreground = WhitePixel (tag->x_display, screen);
	gcontext.background = BlackPixel (tag->x_display, screen);
	tag->x_gc = XCreateGC (tag->x_display, tag->x_window, 
		GCForeground | GCBackground, &gcontext);
/*
 * Create a separate graphics context for the target, since we don't want
 * clipping to affect it
 */
	tag->x_tgt_gc = XCreateGC (tag->x_display, tag->x_window, 
		GCForeground | GCBackground, &gcontext);
/*
 * Get the cursor set up.
 */
 	tag->x_cursor = XCreateFontCursor (tag->x_display, XC_top_left_arrow);
	XDefineCursor (tag->x_display, tag->x_window, tag->x_cursor);
/*
 * Fill in the device structure.
 */
 	if (tag->x_mono)
		dev->gd_ncolor = 2;
	else
	{
		dev->gd_ncolor = DisplayCells (tag->x_display, screen);
		dev->gd_flags |= GDF_PIXEL;
	}
	dev->gd_xres = tag->x_xres;
	dev->gd_yres = tag->x_yres;
	dev->gd_background = tag->x_bg;
/*
 * KLUGE: Keep a pointer to the background color in the dev structure, since
 * we need to change it when the user first allocates some colors
 */
	tag->x_dev_bg = &(dev->gd_background);
/*
 * Clear the window.
 */
	XWindowEvent (tag->x_display, tag->x_window, ExposureMask, &ev);
	XClearWindow (tag->x_display, tag->x_window);
	XSync (tag->x_display, False);
/*
 * All done.
 */
 	*ctag = (char *) tag;
	return (GE_OK);
}





x11_close (ctag)
char *ctag;
/*
 * Finish up.
 */
{
	struct xtag *tag = (struct xtag *) ctag;

	XCloseDisplay (tag->x_display);
	relvm (tag);
}





x11_clear (ctag)
char *ctag;
/*
 * Clear the window.
 */
{
	struct xtag *tag = (struct xtag *) ctag;

	XClearWindow (tag->x_display, tag->x_window);
/*
 * Clear the target pixmap and remember we need to redo it
 */
	XSetForeground (tag->x_display, tag->x_tgt_gc, 
		BlackPixel (tag->x_display, 0));
	XFillRectangle (tag->x_display, tag->x_tgt_pixmap, tag->x_tgt_gc, 
		0, 0, TGT_SIZE + 2, TGT_SIZE + 2);
	XSetForeground (tag->x_display, tag->x_tgt_gc, 
		WhitePixel (tag->x_display, 0));

	tag->x_do_pxm = TRUE;
}




x11_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * The polyline routine.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	Xpoint *xp;
	int pt;
/*
 * For color displays, fill in the color value.
 */
 	if (! tag->x_mono)
		XSetForeground (tag->x_display, tag->x_gc, color);
/*
 * Set up our dash pattern.
 */
	if (ltype == GPLT_SOLID)
		XSetLineAttributes (tag->x_display, tag->x_gc, 0, LineSolid,
			CapButt, JoinMiter);
	else
	{
		XSetLineAttributes (tag->x_display, tag->x_gc, 0,
			LineOnOffDash, CapButt, JoinMiter);
		XSetDashes (tag->x_display, tag->x_gc, 1,
			D_table[ltype].d_pattern, D_table[ltype].d_ndash);
	}
/*
 * Allocate our xpoint array, and fill it in.
 */
	xp = (Xpoint *) getvm (npt * sizeof (Xpoint));
	for (pt = 0; pt < npt; pt++)
	{
		xp[pt].x = *data++;
		xp[pt].y = tag->x_yres - (*data++ + 1);
	}
/*
 * Do the drawing.
 */
 	XDrawLines (tag->x_display, tag->x_window, tag->x_gc, xp, npt,
			CoordModeOrigin);
	relvm (xp);
}






x11_flush (ctag)
char *ctag;
/*
 * Flush out any pending operations.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
/*
 * Draw in the target if it's been placed
 */
	if (tag->x_xtgt >= 0 && tag->x_ytgt >= 0)
	{
	/*
	 * Save the portion of the screen which will be overwritten by 
	 * the target, if necessary
	 */
		if (tag->x_do_pxm)
		{
			XCopyArea (tag->x_display, tag->x_window, 
				tag->x_tgt_pixmap, tag->x_tgt_gc, 
				tag->x_xtgt - TGT_SIZE/2 - 1, 
				tag->x_ytgt - TGT_SIZE/2 - 1, 
				TGT_SIZE + 2, TGT_SIZE + 2, 0, 0);
			tag->x_do_pxm = FALSE;
		}
	/*
	 * Draw the target
	 */
		XDrawArc (tag->x_display, tag->x_window, tag->x_tgt_gc, 
			tag->x_xtgt - TGT_SIZE/2, tag->x_ytgt - TGT_SIZE/2, 
			TGT_SIZE, TGT_SIZE, 0, 25000);
		XDrawLine (tag->x_display, tag->x_window, tag->x_tgt_gc, 
			tag->x_xtgt - TGT_SIZE/2, tag->x_ytgt, 
			tag->x_xtgt + TGT_SIZE/2, tag->x_ytgt);
		XDrawLine (tag->x_display, tag->x_window, tag->x_tgt_gc, 
			tag->x_xtgt, tag->x_ytgt - TGT_SIZE/2, 
			tag->x_xtgt, tag->x_ytgt + TGT_SIZE/2);
	}
/*
 * Flush everything out
 */
	XFlush (tag->x_display);
}





x11_noop ()
{
	return (GE_OK);
}





x11_pick (ctag, button, x, y)
char *ctag;
int *button, *x, *y;
/*
 * The target pick routine.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	XEvent ev;
/*
 * Move everything out the display, and clear the event queue, so that 
 * there is no grungy old stuff sitting around.
 */
 	XSync (tag->x_display, True);
/*
 * Now wait for an event.
 */
	XWindowEvent (tag->x_display, tag->x_window, ButtonPressMask, &ev);
	*button = ev.xbutton.button - 1;
	*x = ev.xbutton.x;
	*y = tag->x_yres - ev.xbutton.y - 1;
}




x11_target (ctag, x, y)
char *ctag;
int *x, *y;
/*
 * Report the target location
 */
{
	struct xtag *tag = (struct xtag *) ctag;

	*x = tag->x_xtgt;
	*y = tag->x_yres - tag->x_ytgt;
	return;
}



x11_put_target (ctag, x, y)
char	*ctag;
int	x, y;
/*
 * Put the target at the chosen location
 */
{
	struct xtag *tag = (struct xtag *) ctag;
/*
 * Restore the data under the old target location (if there is an old
 * target location)
 */
	if (tag->x_xtgt > 0 && tag->x_ytgt > 0)
		XCopyArea (tag->x_display, tag->x_tgt_pixmap, tag->x_window,
			tag->x_tgt_gc, 0, 0, TGT_SIZE + 2, TGT_SIZE + 2, 
			tag->x_xtgt - TGT_SIZE/2 - 1, 
			tag->x_ytgt - TGT_SIZE/2 - 1);
/*
 * Save the new target location
 */
	tag->x_xtgt = x;
	tag->x_ytgt = tag->x_yres - y;
/*
 * Make sure we redo the pixmap on the next flush
 */
	tag->x_do_pxm = TRUE;

	x11_flush (ctag);
	return;
}




x11_casn (ctag, ncolor, base)
char *ctag;
int ncolor, *base;
/*
 * Grab some colors for the application.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	int	ncontig, nget, offset, ncells, contig_start;
	int	start, i, status;
	long	*cells, pm;
/*
 * Simple for a mono device
 */
	if (tag->x_mono)
	{
		*base = 0;
		return (GE_OK);
	}
/*
 * Start with space for 256 color cells (this may increase below)
 */
	ncells = 256;
	cells = (long *) malloc (ncells * sizeof (long));
/*
 * Initialize
 */
	offset = 0;
	ncontig = 1;
	nget = 0;
	contig_start = 0;
/*
 * Allocate color cells until we have the requested number of contiguous
 * cells
 */
	while (ncontig != ncolor)
	{
	/*
	 * Update the offset and the number of cells to get
	 */
		offset += nget;
		nget = ncolor - ncontig;
	/*
	 * Get more cell space if necessary
	 */
		if (offset + nget > ncells)
		{
			ncells *= 2;
			cells = (long *) 
				realloc (cells, ncells * sizeof (long));
		}
	/*
	 * Get as many color cells as we need to complete a chunk of ncolor
	 * contiguous cells
	 */
		status = XAllocColorCells (tag->x_display, 
			DefaultColormap (tag->x_display, 0), False, &pm, 0,
			cells + offset, nget);
		if (! status)
			return (GE_DEVICE_UNABLE);
	/*
	 * Find out how many contiguous cells we have now
	 */
		start = (offset == 0) ? 1 : offset;

		for (i = start; i < nget + offset; i++)
		{
			if (ABS (cells[i] - cells[i-1]) == 1)
				ncontig++;
			else
			{
				contig_start = i;
				ncontig == 1;
			}
		}
	}
/*
 * Deallocate the cells before the contiguous chunk
 */
	XFreeColors (tag->x_display, DefaultColormap (tag->x_display, 0), 
		cells, contig_start, 0);
/*
 * Save the base of the contiguous block
 */
	*base = cells[contig_start];
/*
 * Set the background if it hasn't been done
 */
	if (! tag->x_bg)
	{
		long	bg = cells[contig_start];

		tag->x_bg = bg;
		*(tag->x_dev_bg) = bg;
		XSetWindowBackground (tag->x_display, tag->x_window, bg);
	}
/*
 * Free the memory allocated for cells
 */
	free (cells);

	return (GE_OK);
}





x11_color (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
{
	struct xtag *tag = (struct xtag *) ctag;
	XColor *xc = (XColor *) getvm (ncolor * sizeof (XColor));
	int col;
/*
 * Ignore this stuff for mono displays.
 */
 	if (tag->x_mono)
		return (GE_DEVICE_UNABLE);
/*
 * Reformat the colors for X.
 */
 	for (col = 0; col < ncolor; col++)
	{
		xc[col].pixel = col + base;
		xc[col].red = (unsigned short) (*r++ * 65535.0);
		xc[col].green = (unsigned short) (*g++ * 65535.0);
		xc[col].blue = (unsigned short) (*b++ * 65535.0);
		xc[col].flags = DoRed | DoGreen | DoBlue;
	}
/*
 * Now, store them.
 */
 	XStoreColors (tag->x_display, DefaultColormap (tag->x_display, 0), 
		xc, ncolor);
	relvm (xc);
		
	return (GE_OK);
}




# ifdef notdef /* (8/24/89 cb) */

x11_fixcolor (tag)
struct xtag *tag;
/* 
 * Fix up our color map so as to cause minimal interference with everybody
 * else.
 */
{
	XColor xc[2];
	int pv[2], pm;

	XAllocColorCells (tag->x_display, tag->x_cmap, True,
		&pm, 0, pv, 2);
	xc[0].pixel = 0;
	xc[0].red = xc[0].green = xc[0].blue = 65535;
	xc[0].flags = xc[1].flags = DoRed | DoGreen | DoBlue;
	xc[1].pixel = 1;
	xc[1].red = xc[1].green = xc[1].blue = 0;
	XStoreColors (tag->x_display, tag->x_cmap, xc, 2);
}

# endif



x11_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag, *data;
int x, y, xs, ys, size, org;
/*
 * The pixel fill routine.
 */
{
	XImage *xi;
	struct xtag *tag = (struct xtag *) ctag;
/*
 * Kludge to allow the restoration of images saved under Sunview.
 */
	if (xs > 300 || ys > 300)
		x11_sv_kludge (data, xs, ys, tag->x_bg);
/*
 * Create the XImage structure.
 */
 	xi = XCreateImage (tag->x_display, tag->x_visual, 8, ZPixmap, 0, data,
		xs, ys, 8, xs);
/*
 * Send it to the display.
 */
 	XPutImage (tag->x_display, tag->x_window, tag->x_gc, xi, 0, 0, 
		x, tag->x_yres - (y + ys), xs, ys);
/*
 * Release the image structure.
 */
 	xi->data = (char *) 0;
	XDestroyImage (xi);
}





x11_sv_kludge (data, xs, ys, base)
register unsigned char *data;
int xs, ys;
unsigned int base;
/*
 * Offset this data to the base needed for X, if necessary.
 */
{
	register int i, npix = xs*ys;
/*
 * Look at up to the first thousand pixels.  If any are below the nominal
 * base value, assume that we have to offset it.
 */
	for (i = 0; i < 1000; i++)
		if (data[i] < base)
			break;
	if (i >= 1000)
		return;
	if (getenv ("XBASE"))
		base = atoi (getenv ("XBASE"));
	printf ("Found %d at %d -- offsetting by %d\n", data[i], i, base);
/*
 * Do it.
 */
 	for (i = 0; i < npix; i++)
		data[i] = (data[i] == 127) ? 0 : data[i] + base;
}




x11_clip (ctag, x0, y0, x1, y1)
char	*ctag;
int	x0, y0, x1, y1;
/*
 * Set the "hardware" clip window
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	XRectangle	rect;

	rect.x = x0;
	rect.y = tag->x_yres - y1;
	rect.width = x1 - x0 + 1;
	rect.height = y1 - y0 + 1;	

	XSetClipRectangles (tag->x_display, tag->x_gc, 0, 0, &rect, 1, 
		Unsorted);
}

# endif /* DEV_X11 */
