/* 12/88 jc */
/* $Id: dev_x11.c,v 1.2 1989-04-11 16:11:58 corbet Exp $	*/
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
	Colormap x_cmap;	/* The color map		*/
	Visual	*x_visual;	/* The visual			*/
	int	x_xres, x_yres;	/* The resolution of our window	*/
	int	x_mono;		/* Is this a mono screen?	*/
	long	x_fg, x_bg;	/* Foreground and background colors */
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
	int screen, pv[2], pm;
/*
 * Figure out what our resolution will be.
 */
 	if (! strcmp (type, "X11") || ! strcmp (type, "X700"))
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
	tag->x_fg = WhitePixel (tag->x_display, screen);
	tag->x_bg = BlackPixel (tag->x_display, screen);
/*
 * Create the window to exist on that display.
 */
	attr.background_pixel = 1;
	attr.border_pixel = tag->x_fg;
	attr.backing_store = Always;
	attr.event_mask = ButtonPressMask;
	tag->x_window = XCreateWindow (tag->x_display,
		RootWindow (tag->x_display, screen), 10, 10, tag->x_xres, 
		tag->x_yres, 2, CopyFromParent, CopyFromParent,
		CopyFromParent,
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
	XSync (tag->x_display, False);
/*
 * Get a graphics context.
 */
 	gcontext.foreground = tag->x_fg;
	gcontext.background = tag->x_bg;
	tag->x_gc = XCreateGC (tag->x_display, tag->x_window, 
		GCForeground | GCBackground, &gcontext);
/*
 * If this is a color display, get a colormap.  We need our own color map
 * because X is not interested in giving us contiguous colors.  The prospect
 * of remapping an entire pixel array of colors to suit X is not especially
 * pleasing.
 */
	if (! tag->x_mono)
	{
		int pv[128], pm;
		tag->x_visual = DefaultVisual (tag->x_display, screen);
		tag->x_cmap = XCreateColormap (tag->x_display, tag->x_window,
			tag->x_visual, AllocNone);
		XSetWindowColormap (tag->x_display, tag->x_window,
			tag->x_cmap);
		XAllocColorCells (tag->x_display, tag->x_cmap, True, &pm,
			0, pv, 2);
		XInstallColormap (tag->x_display, tag->x_cmap);
		x11_fixcolor (tag);
	}
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
			CapProjecting, JoinMiter);
	else
	{
		XSetLineAttributes (tag->x_display, tag->x_gc, 0,
			LineOnOffDash, CapProjecting, JoinMiter);
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

	/* XSync (tag->x_display, True); */
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
 * The target routine.
 */
{
	int junk;

	x11_pick (ctag, &junk, x, y);
}






x11_casn (ctag, ncolor, base)
char *ctag;
int ncolor, *base;
/*
 * Grab some colors for the application.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	int pv[256], pm;

	if (! tag->x_mono)
	{
		XAllocColorCells (tag->x_display, tag->x_cmap, True,
			&pm, 0, pv, ncolor);
		*base = pv[0];
	}
	else
		*base = 0;
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
 	XStoreColors (tag->x_display, tag->x_cmap, xc, ncolor);
	relvm (xc);
	return (GE_OK);
}






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





# endif /* DEV_X11 */
