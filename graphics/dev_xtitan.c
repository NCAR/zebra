/* 12/88 jc */
/* $Id: dev_xtitan.c,v 1.2 1989-08-07 15:58:04 corbet Exp $ */
/*
 * Graphics driver for the X window system, version 11.3, with Titan 
 * enhancements.
 */
# include "config.h"

# ifdef DEV_XTITAN

# include <stdio.h>
# include "graphics.h"
# include "device.h"
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/cursorfont.h>
# include <X11/XB.h>
# include <X11/XTitan.h>
# include <X11/Xdirect.h>

# define PTR_SIZE	18
# define NCOLOR		256


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
	long	x_fg, x_bg;	/* Foreground and background colors */
	int 	x_base;		/* KLUDGE: color base		*/
	int	x_xptr, x_yptr;	/* Pointer position		*/
	Pixmap	x_ptr_pixmap;	/* Pixmap to save data covered by pointer */
	WindowHandle *x_handle;	/* Direct window handle.	*/
	XdColor x_cmap[NCOLOR];	/* The color map		*/
	unsigned char x_rmap[NCOLOR], x_bmap[NCOLOR], x_gmap[NCOLOR];
				/* Direct color maps		*/

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




xt_open (device, type, ctag, dev)
char *device, *type, **ctag;
struct device *dev;
/*
 * Open up an X11 window device.
 */
{
	struct xtag *tag = (struct xtag *) getvm (sizeof (struct xtag));
	XSetWindowAttributes attr;
	XGCValues gcontext;
	int screen, pv[2];
	XEvent ev;
/*
 * Figure out what our resolution will be.
 */
 	if (! strcmp (type, "titan") || ! strcmp (type, "titan700"))
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
	tag->x_base = 0;
/*
 * No pointer for now
 */
	tag->x_xptr = -1;
	tag->x_yptr = -1;
/*
 * Create the window to exist on that display.
 */
	attr.background_pixel = 1;
	attr.border_pixel = tag->x_fg;
	attr.backing_store = Always;
	attr.event_mask = ButtonPressMask | ExposureMask;
	tag->x_window = XCreateWindow (tag->x_display,
		RootWindow (tag->x_display, screen), 10, 10, tag->x_xres, 
		tag->x_yres, 2, CopyFromParent, InputOutput, CopyFromParent,
		CWBackPixel|CWBorderPixel|CWEventMask, &attr);
/*		CWBackPixel|CWBorderPixel|CWBackingStore|CWEventMask, &attr);*/
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
 * Get the pixmap to save the area covered by the pointer
 */
	tag->x_ptr_pixmap = XCreatePixmap (tag->x_display, tag->x_window, 
		PTR_SIZE + 2, PTR_SIZE + 2, 
		DefaultDepth (tag->x_display, screen));
/*
 * Get a graphics context.
 */
 	gcontext.foreground = tag->x_fg;
	gcontext.background = tag->x_bg;
	tag->x_gc = XCreateGC (tag->x_display, tag->x_window, 
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
/*
 * Clear the window.
 */
	printf ("Waiting for expose...\n");
	XWindowEvent (tag->x_display, tag->x_window, ExposureMask, &ev);
	printf ("Got it.\n");
	/* XClearWindow (tag->x_display, tag->x_window); */
	XSync (tag->x_display, False);
/*
 * Get the direct graphics handle.
 */
	tag->x_handle = XdCreateWindowHandle (tag->x_display, tag->x_window);
	XdSetZFunction (tag->x_handle, Z_FUNC_NONE);
/*
 * All done.
 */
 	*ctag = (char *) tag;
	return (GE_OK);
}





xt_close (ctag)
char *ctag;
/*
 * Finish up.
 */
{
	struct xtag *tag = (struct xtag *) ctag;

	XCloseDisplay (tag->x_display);
	relvm (tag);
}





xt_clear (ctag)
char *ctag;
/*
 * Clear the window.
 */
{
	struct xtag *tag = (struct xtag *) ctag;

	/* XClearWindow (tag->x_display, tag->x_window); */
/*
 * Clear the pointer, too
 */
	tag->x_xptr = -1;
	tag->x_yptr = -1;

	XSetFunction (tag->x_display, tag->x_gc, GXset);
	XFillRectangle (tag->x_display, tag->x_ptr_pixmap, tag->x_gc, 
		0, 0, PTR_SIZE + 2, PTR_SIZE + 2);
	XSetFunction (tag->x_display, tag->x_gc, GXcopy);
	xt_flush (ctag);
}




xt_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * The polyline routine.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	XdPoint *xp;
	int pt;
/*
 * For color displays, fill in the color value.
 */
 	if (! tag->x_mono)
		XdSetBaseColor (tag->x_handle, &tag->x_cmap[color]);
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
 * Allocate our xpoint array, and fill it in.  Note that we have a line *list*
 * here, not a polyline, so the internal vertices must all be duplicated.
 */
	xp = (XdPoint *) getvm (2*(npt - 1) * sizeof (XdPoint));
	for (pt = 0; pt < 2*(npt - 1); pt++)
	{
		if (((pt & 1) == 0) && (pt != 0))
			xp[pt] = xp[pt - 1];
		else
		{
			xp[pt].x = (double) *data++;
			xp[pt].y = (double) (tag->x_yres - (*data++ + 1));
			xp[pt].z = 0.0;
		}
	}
/*
 * Do the drawing.
 */
	XdDrawLineList (tag->x_handle, xp, npt-1);
	relvm (xp);
}






xt_flush (ctag)
char *ctag;
/*
 * Flush out any pending operations.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
/*
 * Draw in the pointer if it's been placed
 */
	if (tag->x_xptr >= 0 && tag->x_yptr >= 0)
	{
		XSetForeground (tag->x_display, tag->x_gc, 
			WhitePixel (tag->x_display, 0));
		XDrawArc (tag->x_display, tag->x_window, tag->x_gc, 
			tag->x_xptr - PTR_SIZE/2, tag->x_yptr - PTR_SIZE/2, 
			PTR_SIZE, PTR_SIZE, 0, 25000);
		XDrawLine (tag->x_display, tag->x_window, tag->x_gc, 
			tag->x_xptr - PTR_SIZE/2, tag->x_yptr, 
			tag->x_xptr + PTR_SIZE/2, tag->x_yptr);
		XDrawLine (tag->x_display, tag->x_window, tag->x_gc, 
			tag->x_xptr, tag->x_yptr - PTR_SIZE/2, 
			tag->x_xptr, tag->x_yptr + PTR_SIZE/2);
	}
/*
 * Flush everything out
 */
	XdFlush (tag->x_handle);
}





xt_noop ()
{
	return (GE_OK);
}





xt_pick (ctag, button, x, y)
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




xt_target (ctag, x, y)
char *ctag;
int *x, *y;
/*
 * Report the target location
 */
{
	struct xtag *tag = (struct xtag *) ctag;

	*x = tag->x_xptr;
	*y = tag->x_yres - tag->x_yptr;
	return;
}



xt_put_target (ctag, x, y)
char	*ctag;
int	x, y;
/*
 * Put the target at the chosen location
 */
{
	struct xtag *tag = (struct xtag *) ctag;
/*
 * Restore the data under the old pointer location (if there is an old
 * pointer location)
 */
	if (tag->x_xptr > 0 && tag->x_yptr > 0)
		XCopyArea (tag->x_display, tag->x_ptr_pixmap, tag->x_window,
			tag->x_gc, 0, 0, PTR_SIZE + 2, PTR_SIZE + 2, 
			tag->x_xptr - PTR_SIZE/2 - 1, 
			tag->x_yptr - PTR_SIZE/2 - 1);
/*
 * Save the new target location
 */
	tag->x_xptr = x;
	tag->x_yptr = tag->x_yres - y;
/*
 * Copy the portion of the screen which will be overwritten by the target
 */
	XCopyArea (tag->x_display, tag->x_window, tag->x_ptr_pixmap,
		tag->x_gc, tag->x_xptr - PTR_SIZE/2 - 1, 
		tag->x_yptr - PTR_SIZE/2 - 1, 
		PTR_SIZE + 2, PTR_SIZE + 2, 0, 0);

	xt_flush (ctag);
	return;
}







xt_color (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
{
	struct xtag *tag = (struct xtag *) ctag;
	int col;
/*
 * Ignore this stuff for mono displays.
 */
 	if (tag->x_mono)
		return (GE_DEVICE_UNABLE);
/*
 * Reformat the colors for Xd.
 */
 	for (col = 0; col < ncolor; col++)
	{
		tag->x_rmap[col + base] = (unsigned char) (*r * 255.);
		tag->x_cmap[col + base].r = *r++;
		tag->x_bmap[col + base] = (unsigned char) (*b * 255.);
		tag->x_cmap[col + base].b = *b++;
		tag->x_gmap[col + base] = (unsigned char) (*g * 255.);
		tag->x_cmap[col + base].g = *g++;
	}
	return (GE_OK);
}







xt_pixel (ctag, x, y, xs, ys, data, size, org)
unsigned char *ctag, *data;
int x, y, xs, ys, size, org;
/*
 * The pixel fill routine.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	unsigned char *copy, *dp = data, *cp;
	int i;
/*
 * Perform a flush first, since this stuff seems to go through a separate
 * channel....
 */
	xt_flush (ctag);
/*
 * Allocate enough space to copy the data over.
 */
	cp = copy = (unsigned char *) getvm (xs*ys*4);

# ifdef notdef
	for (i = 0; i < xs*ys; i++)
		*cp++ = (unsigned char) (tag->x_cmap[*dp++].r * 255);
	XdWriteRectangle (tag->x_handle, 0, 0, x, tag->x_yres - y - 1,
		xs, ys, ys, copy, RedBank);
	cp = copy;
	dp = data;
	for (i = 0; i < xs*ys; i++)
		*cp++ = (unsigned char) (tag->x_cmap[*dp++].g * 255);
	XdWriteRectangle (tag->x_handle, 0, 0, x, tag->x_yres - y - 1,
		xs, ys, ys, copy, GreenBank);
	cp = copy;
	dp = data;
	for (i = 0; i < xs*ys; i++)
		*cp++ = (unsigned char) (tag->x_cmap[*dp++].b * 255);
	XdWriteRectangle (tag->x_handle, 0, 0, x, tag->x_yres - y - 1,
		xs, ys, ys, copy, BlueBank);
# endif

/*
 * Now do it.
 */
	for (i = 0; i < xs*ys; i++)
	{
		*cp++ = tag->x_rmap[*dp];
		*cp++ = tag->x_gmap[*dp];
		*cp++ = tag->x_bmap[*dp++];
	}
/*
 * Ship out the image.
 */
	XdWriteRectangleRGB (tag->x_handle, 0, 0, x, tag->x_yres - (y + ys),
		xs, ys, xs, copy);
/*
 * Free up the storage.
 */
	relvm (copy);
}





# endif /* DEV_XTITAN */
