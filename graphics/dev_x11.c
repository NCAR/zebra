/* 12/88 jc */
/*
 * Graphics driver for the X window system, version 11.3
 */
static char *rcsid = "$Id: dev_x11.c,v 1.34 2002-07-11 22:59:23 burghart Exp $";

# include "graphics.h"
# include "device.h"
# include <stdio.h>
# include <stdlib.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/cursorfont.h>
# include <X11/keysym.h>

char *getvm (), *getenv ();

# define TGT_SIZE	17
# define MAXFONT	100	/* Max fonts we'll consider		*/
/*
 * The default font pattern.
 */
# define FONTPAT "-*-helvetica-medium-r-*-*-*-*-*-*-*-*-*-*";

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
	Pixmap  x_sw[5];	/* Quadrant subwindows		*/
	GC	x_gc;		/* Graphics context		*/
	Cursor	x_cursor;	/* The cursor for this window	*/
	Visual	*x_visual;	/* The visual			*/
        unsigned long	*x_pixels;	/* index -> pixel array	*/
	int	x_pseudocolor;	/* Did we get a PseudoColor visual?	*/
	int	x_xres, x_yres;	/* The resolution of our window	*/
	int	x_xtgt, x_ytgt;	/* Target position			*/
	GC	x_tgt_gc;	/* Target GC (no clipping)		*/
	int	x_do_pxm;	/* Do we need to do the target pixmap?	*/
	Pixmap	x_tgt_pixmap;	/* Pixmap to save data covered by target*/
	char	*x_zwork;	/* Zoom workspace			*/
	int	x_zx, x_zy;	/* Zoom origins				*/
	char	x_current;	/* Window is up to date			*/
	char	x_zoomok;	/* Zoom is up to date.			*/
	char	x_zquad;	/* Which quad we are zoomed on		*/
	char	x_qdone[5];	/* Is this quad done?			*/
	XFontStruct *x_fonts[MAXFONT];/* Already loaded fonts		*/
	int	x_fmap[MAXFONT];/* Pixsize -> font map			*/
	char 	**x_fnames;	/* Font names				*/
	XFontStruct *x_fst;	/* Font info				*/
	int	x_nfont;	/* Actual # of fonts			*/
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


/*
 * The table used for zooming.
 */
static unsigned short Ztable[256];

/*
 * "Certain" X servers seem to have problems with large reads.  Thus, this
 * parameter, which limits the amount of data we will try to read in a 
 * single operation.
 */
# define MAXREAD	8000	/* This is a guess */

/*
 * Forwards
 */
void x11_clear(), x11_clip(), x11_calc_zoom(), x11_key(), x11_splitread();




int
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
	XWMHints hints;
	int screen, pv[2], pm, nmatch, depth, i;
	XEvent ev;
	char* default_dev;
/*
 * Figure out what our resolution will be.
 */
 	if (! strcmp (type, "X11") || ! strcmp (type, "X700") ||
		! strcmp (type, "x11") || ! strcmp (type, "x700"))
	{
		tag->x_xres = tag->x_yres = 704;
		dev->gd_xb = dev->gd_yb = 176;
	}
	else if (! strcmp (type, "X11-huge") || ! strcmp (type, "x11-huge"))
	{
		tag->x_xres = 912;
		tag->x_yres = 800;
		dev->gd_xb = 152;
	}
	else if (! strcmp (type, "Sun-screen"))
	{
		tag->x_xres = 1148;
		tag->x_yres = 864;
		dev->gd_xb = 164;
		dev->gd_yb = 108;
	}
	else
	{
		tag->x_xres = tag->x_yres = 512;
		dev->gd_yb = dev->gd_xb = 128;
	}
/*
 * First, open up our display.  If the device is "screen", use the current
 * value of getenv("DISPLAY"); otherwise take the device as given.
 */
	default_dev = getenv("DISPLAY");
	if (! default_dev)
	    default_dev = ":0";
	
	tag->x_display = XOpenDisplay ((device && strcmp (device, "screen")) ? 
				       device : default_dev);
	if (! tag->x_display)
	{
		relvm (tag);
		return (GE_BAD_DEVICE);
	}
	screen = DefaultScreen (tag->x_display);

	tag->x_current = FALSE;
/*
 * No target for now
 */
	tag->x_xtgt = -1;
	tag->x_ytgt = -1;
/*
 * Look for a good visual.  Try for a TrueColor one first, and if that
 * fails then get a PseudoColor visual with depth >= 8.
 */
	tag->x_visual = 0;
	
	template.screen = screen;
	template.class = TrueColor;
	vlist = XGetVisualInfo (tag->x_display,
				VisualScreenMask | VisualClassMask, 
				&template, &nmatch);
	if (nmatch)
	{
	    /*
	     * Good, we got a TrueColor visual
	     */
	    tag->x_visual = vlist[0].visual;
	    depth = vlist[0].depth;
	    tag->x_pseudocolor = FALSE;
	}
	else
	{
	    /*
	     * Try to find a PseudoColor visual.
	     */
	    template.class = PseudoColor;
	    vlist = XGetVisualInfo (tag->x_display,
				    VisualScreenMask | VisualClassMask, 
				    &template, &nmatch);
	    if (nmatch)
	    {
		/*
		 * Take the first one with at least a depth of 8.
		 */ 
		for (i = 0; i < nmatch; i++)
                {
                    if (vlist[i].depth >= 8){
		        tag->x_visual = vlist[i].visual;
		        depth = vlist[i].depth;
			tag->x_pseudocolor = TRUE;
		        XFree ((char *) vlist);
                        break;
                    }
                }
	    }
	}
	
	if (!tag->x_visual)
	    return GE_DEVICE_UNABLE;
/*
 * Create the window to exist on that display.
 */
	attr.background_pixel = BlackPixel (tag->x_display, screen);
	attr.border_pixel = WhitePixel (tag->x_display, screen);
	attr.backing_store = WhenMapped;
	attr.event_mask = ButtonPressMask | ExposureMask | KeyPressMask;
	tag->x_window = XCreateWindow (tag->x_display,
		RootWindow (tag->x_display, screen), 10, 10, tag->x_xres, 
		tag->x_yres, 0, depth, InputOutput, tag->x_visual,
		CWBackPixel|CWBorderPixel|CWBackingStore|CWEventMask, &attr);
/*
 * Set the input flag for difficult window managers.
 */
	hints.flags = InputHint;
	hints.input = True;
	XSetWMHints (tag->x_display, tag->x_window, &hints);
/*
 * Create all of the subwindows.
 */
	for (i = 0; i < 5; i++)
	    tag->x_sw[i] = XCreatePixmap (tag->x_display,
					  tag->x_window, tag->x_xres, 
					  tag->x_yres, depth);
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
	dev->gd_ncolor = 256;
	dev->gd_flags |= GDF_PIXEL;
	dev->gd_xres = tag->x_xres;
	dev->gd_yres = tag->x_yres;
	dev->gd_background = 0;
/*
 * Allocate our color mapping array
 */
	tag->x_pixels = (unsigned long*) 
	    malloc (dev->gd_ncolor * sizeof(unsigned long));
/*
 * Clear the window.
 */
	XWindowEvent (tag->x_display, tag->x_window, ExposureMask, &ev);
	x11_clear (tag);
/*
 * Initialize zoom stuff.
 */
	tag->x_zwork = 0;
	tag->x_zoomok = tag->x_zquad = 0;
	for (i = 0; i < 256; i++)
		Ztable[i] = i | (i << 8);
/*
 * Initialize fonts.
 */
	for (i = 0; i < MAXFONT; i++)
	{
		tag->x_fmap[i] = -1;
		tag->x_fonts[i] = 0;
	}
	tag->x_fnames = 0;
/*
 * All done.
 */
 	*ctag = (char *) tag;
	G_setfd (XConnectionNumber (tag->x_display));
	return (GE_OK);
}




void
x11_close (ctag)
char *ctag;
/*
 * Finish up.
 */
{
	struct xtag *tag = (struct xtag *) ctag;

	XCloseDisplay (tag->x_display);
	if (tag->x_zwork)
		relvm (tag->x_zwork);
	relvm (tag);
}




void
x11_clear (ctag)
char *ctag;
/*
 * Clear the window.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
/*
 * Clear the main window.
 */
	XClearWindow (tag->x_display, tag->x_window);
	XSetForeground (tag->x_display, tag->x_tgt_gc, tag->x_pixels[0]);
	XFillRectangle (tag->x_display, tag->x_sw[0], tag->x_tgt_gc, 0, 0,
			tag->x_xres, tag->x_yres);
	tag->x_zoomok = FALSE;
	tag->x_current = TRUE;
/*
 * Clear the target pixmap and remember we need to redo it
 */
	XFillRectangle (tag->x_display, tag->x_tgt_pixmap, tag->x_tgt_gc, 
		0, 0, TGT_SIZE + 2, TGT_SIZE + 2);
	XSetForeground (tag->x_display, tag->x_tgt_gc, 
		WhitePixel (tag->x_display, 0));

	tag->x_do_pxm = TRUE;
}



void
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
	XSetForeground (tag->x_display, tag->x_gc, tag->x_pixels[color]);
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
 * Do the drawing.  If we are not zoomed, just dump it straight to the
 * screen; otherwise just draw into the pixmap.
 */
# ifdef notdef
/*
 	if (! tag->x_zquad)
	{
	 	XDrawLines (tag->x_display, tag->x_window, tag->x_gc, 
                                (XPoint *) xp, npt,
			CoordModeOrigin);
		tag->x_zoomok = FALSE;
		relvm (xp);
		return;
	}
*/
# endif
 	XDrawLines (tag->x_display, tag->x_sw[0], tag->x_gc, 
                               (XPoint *) xp, npt,
		CoordModeOrigin);
	tag->x_current = FALSE;
/*
 * If we are not operating in a zoomed mode, we're done now.
 */
	if (! tag->x_zoomok || ! tag->x_zquad)
	{
		tag->x_zoomok = FALSE;
		relvm (xp);
		return;
	}
/*
 * Go through and double each coordinate and draw into quad 1.
 */
	XSetLineAttributes (tag->x_display, tag->x_gc, 2, 
		(ltype == GPLT_SOLID) ? LineSolid : LineOnOffDash, CapButt,
		JoinMiter);
	for (pt = 0; pt < npt; pt++)
	{
		xp[pt].x *= 2;
		xp[pt].y *= 2;
	}
 	XDrawLines (tag->x_display, tag->x_sw[1], tag->x_gc, 
                (XPoint *) xp, npt,
		CoordModeOrigin);
/*
 * Offset into quadrant 2.
 */
 	for (pt = 0; pt < npt; pt++)
		xp[pt].x -= tag->x_xres;
 	XDrawLines (tag->x_display, tag->x_sw[2], tag->x_gc, 
                (XPoint *) xp, npt,
		CoordModeOrigin);
/*
 * Offset into quadrant 4.
 */
 	for (pt = 0; pt < npt; pt++)
		xp[pt].y -= tag->x_yres;
 	XDrawLines (tag->x_display, tag->x_sw[4], tag->x_gc, 
                (XPoint *) xp, npt,
		CoordModeOrigin);
/*
 * Offset into quadrant 3.
 */
	for (pt = 0; pt < npt; pt++)
		xp[pt].x += tag->x_xres;
 	XDrawLines (tag->x_display, tag->x_sw[3], tag->x_gc, 
                (XPoint *) xp, npt,
		CoordModeOrigin);
	relvm (xp);
}





void
x11_flush (ctag)
char *ctag;
/*
 * Flush out any pending operations.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
/*
 * Just dump our relevant quad onto the screen.
 */
	x11_clip (ctag, 0, 0, tag->x_xres, tag->x_yres);
	if (tag->x_zquad)
		x11_calc_zoom (tag, tag->x_zquad);
 	if (tag->x_current)
		return;
	XCopyArea (tag->x_display, tag->x_sw[tag->x_zquad], tag->x_window,
		tag->x_gc, 0, 0, tag->x_xres, tag->x_yres, 0, 0);
/*
 * Draw in the target if it's been placed
 */
	if (tag->x_xtgt >= 0 && tag->x_ytgt >= 0)
	{
# ifdef notdef
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
# endif
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
	tag->x_current = TRUE;
}




int
x11_noop ()
{
	return (GE_OK);
}




void
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
	for (;;)
	{
		XWindowEvent (tag->x_display, tag->x_window,
			ButtonPressMask | KeyPressMask, &ev);
		if (ev.type == KeyPress)
			x11_key (tag, &ev);
		else
			break;
	}		
	*button = ev.xbutton.button - 1;
	*x = ev.xbutton.x;
	*y = tag->x_yres - ev.xbutton.y - 1;
/*
 * If we are zoomed, we must adjust the coordinates accordingly.
 */
 	if (tag->x_zquad)
	{
		*x /= 2;
		*y /= 2;
	 	if (tag->x_zquad == 2 || tag->x_zquad == 4)
			*x += tag->x_xres/2;
		if (tag->x_zquad < 3)
			*y += tag->x_yres/2;
	}
}



void
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


void
x11_put_target (ctag, x, y)
char	*ctag;
int	x, y;
/*
 * Put the target at the chosen location
 */
{
	struct xtag *tag = (struct xtag *) ctag;
# ifdef notdef
/*
 * Restore the data under the old target location (if there is an old
 * target location)
 */
	if (tag->x_xtgt > 0 && tag->x_ytgt > 0)
		XCopyArea (tag->x_display, tag->x_tgt_pixmap, tag->x_window,
			tag->x_tgt_gc, 0, 0, TGT_SIZE + 2, TGT_SIZE + 2, 
			tag->x_xtgt - TGT_SIZE/2 - 1, 
			tag->x_ytgt - TGT_SIZE/2 - 1);
# endif
/*
 * Save the new target location
 */
	tag->x_xtgt = x;
	tag->x_ytgt = tag->x_yres - y;
/*
 * Make sure we redo the pixmap on the next flush
 */
	tag->x_do_pxm = TRUE;
	tag->x_current = FALSE;

	x11_flush (ctag);
	return;
}



void
x11_untarget (ctag)
char	*ctag;
/*
 * Remove the target
 */
{
	struct xtag *tag = (struct xtag *) ctag;
# ifdef notdef
/*
 * Restore the data under the old target location (if there is an old
 * target location)
 */
	if (tag->x_xtgt > 0 && tag->x_ytgt > 0)
		XCopyArea (tag->x_display, tag->x_tgt_pixmap, tag->x_window,
			tag->x_tgt_gc, 0, 0, TGT_SIZE + 2, TGT_SIZE + 2, 
			tag->x_xtgt - TGT_SIZE/2 - 1, 
			tag->x_ytgt - TGT_SIZE/2 - 1);
# endif
/*
 * Negative target location for no target
 */
	tag->x_xtgt = tag->x_ytgt = -1;

	tag->x_current = FALSE;
	x11_flush (ctag);
	return;
}



int
x11_casn (ctag, ncolor, base)
char *ctag;
int ncolor, *base;
/*
 * Grab some colors for the application.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	/*
	 * We only need to allocate colors if we have a PseudoColor visual.
	 */
	if (tag->x_pseudocolor)
	{
	    int	status;
	    unsigned long pm;
	    /*
	     * Allocate writable cells from the default Colormap, and store
	     * them in our local color mapping array.
	     */
	    status = XAllocColorCells (tag->x_display, 
				       DefaultColormap (tag->x_display, 0),
				       False, &pm, 0, tag->x_pixels, ncolor);
	    if (! status)
		return (GE_DEVICE_UNABLE);
	}
	/*
	 * Returned base is always zero
	 */
	*base = 0;
	return (GE_OK);
}




int
x11_color (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
{
	struct xtag *tag = (struct xtag *) ctag;
	XColor xc;
	int col;
	/*
	 * Loop through the given colors
	 */
 	for (col = 0; col < ncolor; col++)
	{
		xc.red = (unsigned short) (*r++ * 65535.0);
		xc.green = (unsigned short) (*g++ * 65535.0);
		xc.blue = (unsigned short) (*b++ * 65535.0);
		xc.flags = DoRed | DoGreen | DoBlue;
		/*
		 * For PseudoColor, we store to our allocated pixel
		 */
		if (tag->x_pseudocolor)
		{
		    xc.pixel = tag->x_pixels[col];
		    XStoreColor (tag->x_display, 
				 DefaultColormap (tag->x_display, 0), &xc);
		}
		/*
		 * For TrueColor, we tell the server the RGB we want, and
		 * let it tell us the necessary pixel value, which we
		 * put into our colormap array.
		 */
		else
		{
		    XAllocColor(tag->x_display, 
				DefaultColormap (tag->x_display, 0), &xc);
		    tag->x_pixels[col] = xc.pixel;
		}
	}
	/*
	 * Make sure the window background color continues to match our idea of
	 * the background color
	 */
	XSetWindowBackground (tag->x_display, tag->x_window, tag->x_pixels[0]);
		
	return (GE_OK);
}





void
x11_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag, *data;
int x, y, xs, ys, size, org;
/*
 * The pixel fill routine.
 */
{
	XImage *xi;
	struct xtag *tag = (struct xtag *) ctag;
	int ix, iy;
/*
 * Remap the given char data into an array of pixel values
 */
	unsigned long *pixdata = (unsigned long*) 
	    malloc (xs * ys * sizeof(unsigned long));
	for (ix = 0; ix < xs; ix++)
	{
	    for (iy = 0; iy < ys; iy++)
	    {
		int elem = ix * xs + iy;
		pixdata[elem] = tag->x_pixels[data[elem]];
	    }
	}
/*
 * Create the XImage structure.
 */
 	xi = XCreateImage (tag->x_display, tag->x_visual, 
			   tag->x_visual->bits_per_rgb, ZPixmap, 0, 
			   (char*)pixdata, xs, ys, sizeof(*pixdata), xs);
/*
 * Send it to the display.
 */
 	XPutImage (tag->x_display, tag->x_sw[0], tag->x_gc, xi, 0, 0, 
		   x, tag->x_yres - (y + ys), xs, ys);
/*
 * Release the allocated pixdata and the image structure.
 */
	XDestroyImage (xi);
	tag->x_zoomok = FALSE;
	tag->x_current = FALSE;
}





void
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


void
x11_zreset (tag)
struct xtag *tag;
/*
 * Reset our zoom.
 */
{
	if (tag->x_zquad)
	{
	 	XCopyArea (tag->x_display, tag->x_sw[0], tag->x_window,
			tag->x_tgt_gc, 0, 0, tag->x_xres, tag->x_yres, 0, 0);
		tag->x_zquad = 0;
	}
	tag->x_zoomok = FALSE;
	tag->x_current = FALSE;
}



int
x11_vp (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * The change viewport routine -- only set up to do editor/perusal quads
 * for now.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	int whole;
/*
 * See what sort of zoom is wanted.
 */
	whole = (x1 - x0)*3 > tag->x_xres*2 && (y1 - y0)*3 > tag->x_yres*2;
/*
 * Whole image.
 */
 	if (whole)
	{
		if (tag->x_zquad == 0)
			return (GE_OK);		/* Not zoomed	*/
	 	XCopyArea (tag->x_display, tag->x_sw[0], tag->x_window,
			tag->x_tgt_gc, 0, 0, tag->x_xres, tag->x_yres, 0, 0);
		tag->x_zquad = 0;
		tag->x_current = FALSE;
	}
/*
 * Quad.
 */
 	else
	{
		int oldquad = tag->x_zquad;
	/*
	 * Calculate the quad.
	 */
		tag->x_zx = x0 ? tag->x_xres/2 : 0;
		tag->x_zy = y0 ? 0 : tag->x_yres/2;
		tag->x_zquad = (tag->x_zx ? 1 : 0) + (tag->x_zy ? 2 : 0) + 1;
		if (tag->x_zquad == oldquad)
			return (GE_OK);
		tag->x_current = FALSE;
	/*
	 * Create the zoomed image, if necessary.
	 */
		x11_calc_zoom (tag, tag->x_zquad);
	}
/*
 * Sync things out.
 */
 	x11_flush (ctag);
	return (GE_OK);
}



void
x11_do_quad (tag, x, y, q, read, write)
struct xtag *tag;
int x, y, q;
XImage *read, *write;
/*
 * Calculate a single quad.
 */
{
	register unsigned char *rp = (unsigned char *)
		tag->x_zwork + ((3*tag->x_xres)/4)*tag->x_yres;
	register short *wps = (short *) tag->x_zwork;
	register int row, col;
# ifdef __STDC__
	const int xwidth = tag->x_xres/2, xfull = tag->x_xres;
# else
	register int xwidth = tag->x_xres/2, xfull = tag->x_xres;
# endif
/*
 * Read back this quad.
 */
# ifdef notdef
	/* This is probably worth trying again someday */
	XGetSubImage (tag->x_display, tag->x_sw[0], x, y, tag->x_xres/2, 
		tag->x_yres/2, ~0, ZPixmap, read, 0, 0);
	read = XGetImage (tag->x_display, tag->x_sw[0], x, y, tag->x_xres/2,
		tag->x_yres/2, AllPlanes, ZPixmap);
	rp = read->data;
# endif
	x11_splitread (tag->x_display, tag->x_sw[0], rp, x, y, tag->x_xres/2,
		tag->x_yres/2);
/*
 * Zoom it.
 */
	for (row = 0; row < tag->x_yres/2; row++)
	{
		for (col = 0; col < xwidth; col++)
			*wps++ = Ztable[*rp++];
		memcpy (wps, wps - xwidth, xfull);
		wps += xwidth;
	}
/*
 * Write it back.
 */
 	XPutImage (tag->x_display, tag->x_sw[q], tag->x_tgt_gc, write, 0, 0,
		0, 0, tag->x_xres, tag->x_yres);
/* 0XDestroyImage (read);  */
}




void
x11_calc_zoom (tag, q)
struct xtag *tag;
int q;
/*
 * Calculate the zoomed window for this quad, if necessary.
 */
{
	XImage *read, *write;
	int i;
/*
 * If a workspace has not been allocated, do so now.
 */
 	if (! tag->x_zwork)
		tag->x_zwork = getvm (tag->x_xres * tag->x_yres);
/*
 * Save the full screen before we start.
 */
	if (! tag->x_zoomok)
	{
# ifdef notdef
		XCopyArea (tag->x_display, tag->x_window, tag->x_sw[0],
			tag->x_tgt_gc, 0, 0, tag->x_xres, tag->x_yres, 0, 0);
# endif
		for (i = 0; i < 5; i++)
			tag->x_qdone[i] = FALSE;
		tag->x_zoomok = TRUE;
	}
/*
 * If our quad is already done, we can quit now.
 */
 	if (tag->x_qdone[q])
		return;
/*
 * Create two XImage structures.  The "read" structure points to the last
 * quarter of our work space, while "write" uses the whole thing.  This
 * scheme allows quadrants to be zoomed "in place" with only one chunk
 * of memory.
 */
	read = XCreateImage (tag->x_display, tag->x_visual, 8, ZPixmap, 0,
		tag->x_zwork + ((3*tag->x_xres)/4)*tag->x_yres, tag->x_xres/2,
		tag->x_yres/2, 8, tag->x_xres/2);
	write = XCreateImage (tag->x_display, tag->x_visual, 8, ZPixmap, 0,
		tag->x_zwork, tag->x_xres, tag->x_yres, 8, tag->x_xres);
/*
 * Now do the quad of interest.
 */
 	x11_do_quad (tag, (q == 1 || q == 3) ? 0 : tag->x_xres/2,
		(q < 3) ? 0 : tag->x_yres/2, q, read, write);
	tag->x_qdone[q] = TRUE;
/*
 * All done.  Return our Ximages now.
 */
	read->data = write->data = 0;
	XDestroyImage (read);
	XDestroyImage (write);
}




int
x11_event (ctag)
char *ctag;
/*
 * Deal with any events which may have occurred.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	XEvent ev;
	int ret = 0;

	while (XCheckWindowEvent (tag->x_display, tag->x_window, 
		ExposureMask | KeyPressMask, &ev))
	{
		if (ev.type == KeyPress)
			x11_key (tag, &ev);
		else if (ev.type == Expose)
		{
			/* printf ("Expose reset\n"); */
			ret = 1;
		}
	}
	return (ret);
}



void
x11_key (tag, ev)
struct xtag *tag;
XEvent *ev;
/*
 * Deal with a key event.
 */
{
	static char string[20] = { 0 };
	KeySym key;

	XLookupString ((XKeyEvent *) ev, string, 20, &key, 0);
	switch (key)
	{
	   case XK_F1:	
		x11_vp ((char *) tag, 0, 1, 2, 2);
		break;
	   case XK_F2:	
		x11_vp ((char *) tag, 1, 1, 2, 2);
		break;
	   case XK_F3:	
		x11_vp ((char *) tag, 0, 0, 2, 2);
		break;
	   case XK_F4:	
		x11_vp ((char *) tag, 1, 0, 2, 2);
		break;
	   case XK_F5:
		x11_vp ((char *) tag, 0, 0, tag->x_xres, tag->x_yres);
		break;
	}
}





int
x11_coff (ctag)
char *ctag;
/*
 * Return the color offset.
 */
{
	return (0);
}




int
x11_readscreen (ctag, x, y, xs, ys, data)
char *ctag, *data;
int x, y, xs, ys;
/*
 * Read back a piece of the screen.
 */
{
	struct xtag *tag = (struct xtag *) ctag;

	x11_splitread (tag->x_display, tag->x_window, data, x, y, xs, ys);
	return (GE_OK);
}




void
x11_splitread (disp, src, dest, x, y, xs, ys)
Display *disp;
Drawable src;
char *dest;
int x, y, xs, ys;
/*
 * Read back a piece of image, split into chunks.
 */
{
	XImage *im;
	int readlen, nrast, nread, i;
/*
 * Figure out what our read size will be for the loop portion of the process.
 */
	nrast = MAXREAD/xs;
	nread = ys/nrast;
	readlen = nrast*xs;
/*
 * Now go through and do each chunk.
 */
	for (i = 0; i < nread; i++)
	{
	/*
	 * Read back the image.
	 */
		im = XGetImage (disp, src, x, y + i*nrast, xs, nrast,
			AllPlanes, ZPixmap);
	/*
	 * Copy the data over, and zap the Ximage.
	 */
		memcpy (dest, im->data, readlen);
		dest += readlen;
		XDestroyImage (im);
	}
/*
 * If there is a last piece, we grab it now.
 */
	if (nread*nrast < ys)
	{
		im = XGetImage (disp, src, x, y + nrast*nread, xs,
			ys - nread*nrast, AllPlanes, ZPixmap);
		memcpy (dest, im->data, xs*(ys - nread*nrast));
		XDestroyImage (im);
	}
}





int
x11_qtext (ctag, pixsize, rot)
char *ctag;
int pixsize;
float rot;
/*
 * Ask if we can do text of this size.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	int i, closest, cdiff = 9999;
/*
 * Don't try anything outlandish.
 */
	if (pixsize < 0 || pixsize >= MAXFONT || rot != 0.0)
		return (FALSE);
/*
 * If we haven't looked up our fonts yet, do so now.
 */
	if (! tag->x_fnames)
	{
		char *pat = getenv ("XFONTPATTERN") ? getenv ("XFONTPATTERN") :
				FONTPAT;
		tag->x_fnames = XListFontsWithInfo (tag->x_display, pat,
			MAXFONT, &tag->x_nfont, &tag->x_fst);
	}
/*
 * Go through and find the closest one.
 */
	for (i = 0; i < tag->x_nfont; i++)
	{
		int diff = ABS (tag->x_fst[i].ascent - pixsize);
		if (diff == 0)
		{
			closest = i;
			cdiff = 0;
			break;
		}
		else if (diff < cdiff)
		{
			closest = i;
			cdiff = diff;
		}
	}
/*
 * If we aren't close, give up.
 */
	if (cdiff > 4)
		return (FALSE);
/*
 * Otherwise, go ahead and load the font, since we'll probably use it.
 */
	if (! tag->x_fonts[closest])
		tag->x_fonts[closest] = XLoadQueryFont (tag->x_display,
			tag->x_fnames[closest]);
	tag->x_fmap[pixsize] = closest;
	return (TRUE);
}




void
x11_tsize (ctag, pixsize, rot, text, width, height, desc)
char *ctag, *text;
int pixsize, *width, *height, *desc;
float rot;
/*
 * Return the bounding box for this text.
 */
{
	int dir;
	XCharStruct xc;
	XFontStruct *xfp;
	struct xtag *tag = (struct xtag *) ctag;
/*
 * Do the query.
 */
	if (tag->x_fmap[pixsize] < 0)
		c_panic ("No font for pixsize %d", pixsize);
	xfp = tag->x_fonts[tag->x_fmap[pixsize]];
	XTextExtents (xfp, text, strlen (text), &dir, height, desc, &xc);
/*
 * Now return our info.
 */
	*height += *desc;
	*width = xc.width;
}




void
x11_text (ctag, x, y, color, pixsize, rot, text)
char *ctag, *text;
int x, y, color, pixsize;
float rot;
/*
 * The hardware text routine.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	int f;
/*
 * They should always have asked us first.
 */
	if ((f = tag->x_fmap[pixsize]) < 0)
		c_panic ("No font for pixsize %d", pixsize);
/*
 * For color displays, fill in the color value.
 */
	XSetForeground (tag->x_display, tag->x_gc, tag->x_pixels[color]);
/*
 * Do it.  We throw in the descent, since graphics expects us to draw at
 * the real bottom, not at the baseline.
 */
	XSetFont (tag->x_display, tag->x_gc, tag->x_fonts[f]->fid);
	XDrawString (tag->x_display, tag->x_sw[0], tag->x_gc, x, 
		tag->x_yres - y + 1 - tag->x_fonts[f]->descent, text,
		strlen (text));
	tag->x_current = FALSE;
}
