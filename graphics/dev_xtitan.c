/* 12/88 jc */
/* $Id: dev_xtitan.c,v 1.5 1990-02-08 10:23:30 burghart Exp $ */
/*
 * Graphics driver for the X window system, version 11.3, with Titan 
 * enhancements.
 */
# include "config.h"

# ifdef DEV_XTITAN

# include <stdio.h>
# include "graphics.h"
# include "device.h"
# include "param.h"
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/cursorfont.h>
# include <X11/XB.h>
# include <X11/XTitan.h>
# include <X11/Xdirect.h>
# include <X11/keysym.h>

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
	long	x_fg, x_bg;	/* Foreground and background colors */
	int 	x_base;		/* KLUDGE: color base		*/
	int	x_xptr, x_yptr;	/* Pointer position		*/
	Pixmap	x_ptr_pixmap;	/* Pixmap to save data covered by pointer */
	WindowHandle *x_handle;	/* Direct window handle.	*/
	XdColor x_cmap[NCOLOR];	/* The color map		*/
	Colormap x_color;	/* The pseudocolor map		*/
	unsigned char x_rmap[NCOLOR], x_bmap[NCOLOR], x_gmap[NCOLOR];
				/* Direct color maps		*/
	unsigned char * x_rimage;/* The current image	(red)   */
	unsigned char * x_gimage;/* The current image	(green) */
	unsigned char * x_bimage;/* The current image	(blue)  */
	unsigned char *x_zimage; /* The zoomed image		*/
	int	x_zx, x_zy;	/* The zoomed origin		*/
	int	x_buf;		/* The current display buffer	*/
	int	x_quads[2];	/* What's in each buffer	*/
	char	x_zoomed;	/* True iff we are zoomed	*/
	char	x_zwant;	/* True iff we want to be zoomed*/
	char	x_current;	/* True if the zimage is current*/
	char 	x_pseudo;	/* Is this a pseudocolor window */
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
 * Translation table for zooming.
 */
static unsigned short Xtab[256];


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
	int screen, pv[2], nv, depth, amask;
	XEvent ev;
	unsigned int ic;
	unsigned char *cxp = (unsigned char *) Xtab;
	XVisualInfo template, *vinfo;
/*
 * Figure out what our resolution will be.
 */
 	if (! strcmp (type, "titan") || ! strcmp (type, "pctitan"))
		tag->x_xres = tag->x_yres = 800;
	else if (! strcmp (type, "titan500") || ! strcmp (type, "pctitan500"))
		tag->x_xres = tag->x_yres = 500;
/*
 * Pseudocolor?
 */
	tag->x_pseudo = (type[0] == 'p' && type[1] == 'c');
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
	G_setfd (XConnectionNumber (tag->x_display));
/*
 * Fill in our tag.
 */
	tag->x_fg = WhitePixel (tag->x_display, screen);
	tag->x_bg = 0 /* BlackPixel (tag->x_display, screen) */;
	tag->x_base = 0;
	tag->x_rimage = tag->x_zimage = 0;
	tag->x_current = tag->x_zoomed = tag->x_zwant = tag->x_buf = 0;
	tag->x_quads[0] = tag->x_quads[1] = -1;
/*
 * No pointer for now
 */
	tag->x_xptr = -1;
	tag->x_yptr = -1;
/*
 * If we are running pseudocolor, get a visual.
 */
	if (tag->x_pseudo)
	{
		template.screen = screen;
		template.class = PseudoColor;
		vinfo = XGetVisualInfo (tag->x_display,
			VisualScreenMask | VisualClassMask, &template, &nv);
		tag->x_visual = vinfo->visual;
		depth = vinfo->depth;
		printf ("Got visual 0x%x, depth %d\n", vinfo->depth);
		tag->x_color = XCreateColormap (tag->x_display,
			RootWindow (tag->x_display, screen), tag->x_visual,
			AllocAll);
	}
	else
	{
		tag->x_visual = DefaultVisual (tag->x_display, screen);
		depth = CopyFromParent;
	}
/*
 * Figure out our window attributes.
 */
	attr.background_pixel = 1;
	attr.border_pixel = tag->x_fg;
	attr.backing_store = NotUseful;
	attr.event_mask = KeyPressMask | ButtonPressMask | ExposureMask;
	amask = CWBackPixel | CWBorderPixel | CWBackingStore | CWEventMask;
	if (tag->x_pseudo)
	{
		attr.colormap = tag->x_color;
		amask |= CWColormap;
	}
/*
 * Create the window to exist on that display.
 */
	tag->x_window = XBCreateWindow (tag->x_display,
		RootWindow (tag->x_display, screen), 10, 10, tag->x_xres, 
		tag->x_yres, 2, depth, InputOutput, tag->x_visual, 2, True,
		amask, &attr);
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
	dev->gd_ncolor = DisplayCells (tag->x_display, screen);
	dev->gd_flags |= GDF_PIXEL;
	dev->gd_xres = tag->x_xres;
	dev->gd_yres = tag->x_yres;
/*
 * Clear the window.
 */
	printf ("Waiting for expose...");
	XWindowEvent (tag->x_display, tag->x_window, ExposureMask, &ev);
	printf ("got it.\r\n");
	/* XClearWindow (tag->x_display, tag->x_window); */
	XSync (tag->x_display, False);
/*
 * Get the direct graphics handle.
 */
	tag->x_handle = XdCreateWindowHandle (tag->x_display, tag->x_window);
	XdReset ();
	XdSetZFunction (tag->x_handle, Z_FUNC_NONE);
# ifdef notdef
/*
 * Fill in our translation table.
 */
	for (ic = 0; ic <= 255; ic++)
	{
		*cxp++ = ic;
		*cxp++ = ic;
	}
# endif
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

	xt_change (tag);
/*
 * Fill the window with the background color
 */
	XdFillRectangle (tag->x_handle, 0, 0, tag->x_xres, tag->x_yres, 
		0, 0, FillColor);
/*
 * Clear the pointer, too
 */
	tag->x_xptr = -1;
	tag->x_yptr = -1;

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
	XColor xc;
	XdColor xdc;
	int pt;
/*
 * Fill in the color value.
 */
	xt_change (tag);
	if (tag->x_pseudo)
	{
		xc.pixel = color;
		XQueryColor (tag->x_display, tag->x_color, &xc);
		xdc.r = ((float) xc.red)/255.0;
		xdc.g = ((float) xc.green)/255.0;
		xdc.b = ((float) xc.blue)/255.0;
	}
	else
		xdc = tag->x_cmap[color];
	XdSetBaseColor (tag->x_handle, &xdc);
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
 	XSync (tag->x_display, False);
	XdFlush (tag->x_handle);
/*
 * IF they want a zoom, arrange it now.
 */
 	if (tag->x_zwant)
	{
		tag->x_zwant = False;
		xt_vp (ctag, tag->x_zx, tag->x_zy, tag->x_xres/2,
			tag->x_yres/2);
	}
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
	for (;;)
	{
	/*
	 * Get an event
	 */
		XWindowEvent (tag->x_display, tag->x_window,
			KeyPressMask | ButtonPressMask, &ev);
	/*
	 * If it's a keystroke event, deal with it elsewhere.
	 */
		if (ev.type == KeyPress)
		{
			xt_key (tag, &ev);
			continue;
		}
	/*
	 * Pull out the relevant info.
	 */
		*button = ev.xbutton.button - 1;
		*x = ev.xbutton.x;
		*y = tag->x_yres - ev.xbutton.y - 1;
	/*
	 * If the display is zoomed, we must account for that.
	 */
		if (tag->x_zoomed)
		{
			*x = tag->x_zx + (*x)/2;
			*y = (tag->x_zy ? 0 : tag->x_yres/2) + (*y)/2;
		}
		return;
	}
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
 * Reformat the colors for Xd.
 */
	if (tag->x_pseudo)
	{
		XColor *xc = (XColor *) getvm (ncolor * sizeof (XColor));
	 	for (col = 0; col < ncolor; col++)
		{
			xc[col].pixel = col + base;
			xc[col].red = (unsigned short) (*r++ * 65535.0);
			xc[col].green = (unsigned short) (*g++ * 65535.0);
			xc[col].blue = (unsigned short) (*b++ * 65535.0);
			xc[col].flags = DoRed | DoGreen | DoBlue;
		}
	 	XStoreColors (tag->x_display, tag->x_color, xc, ncolor);
		relvm (xc);
	}
	else
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
char *ctag;
unsigned char *data;
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
	xt_change (tag);
	/* xt_flush (ctag); */
/*
 * Pseudocolor is easy.
 */
 	if (tag->x_pseudo)
		XdWriteRectangle (tag->x_handle, 0, 0, x,
			tag->x_yres - (y + ys), xs, ys, xs, data, RedBank);
/*
 * Otherwise we have to fake our own pseudocolor.
 */
 	else
	{
	/*
	 * Allocate enough space to copy the data over.
	 */
		cp = copy = (unsigned char *) getvm (xs*ys*4);
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
		XdWriteRectangleRGB (tag->x_handle, 0, 0, x,
			tag->x_yres - (y + ys), xs, ys, xs, copy);
	/*
	 * Free up the storage.
	 */
		relvm (copy);
	}
}





xt_vp (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * The viewport routine for the titan; we only deal in quadrants.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	int whole, quad;
/*
 * See what sort of zoom is wanted.
 */
	whole = (x1 - x0)*3 > tag->x_xres*2 && (y1 - y0)*3 > tag->x_yres*2;
/*
 * If they want the whole image, give it to them.
 */
	if (whole)
	{
		if (! tag->x_zoomed)
			return;
		if (! tag->x_rimage)
			c_panic ("Zoomed with no image!");
		tag->x_buf ^= 0x1;
		XBSetDrawBuffer (tag->x_display, tag->x_window, tag->x_buf, 0);
		XdSetDrawBuffer (tag->x_handle, tag->x_buf, 0);
		if (tag->x_quads[tag->x_buf])
		{
			XdWriteRectangle (tag->x_handle, 0, 0, 0, 0,
					tag->x_xres, tag->x_yres,
					tag->x_xres, tag->x_rimage, RedBank);
			if (! tag->x_pseudo)
			{
			    XdWriteRectangle (tag->x_handle, 0, 0, 0, 0,
					tag->x_xres, tag->x_yres,
					tag->x_xres, tag->x_bimage, BlueBank);
			    XdWriteRectangle (tag->x_handle, 0, 0, 0, 0,
					tag->x_xres, tag->x_yres, tag->x_xres,
					tag->x_gimage, GreenBank);
			}
		}
		XBSetDisplayBuffer (tag->x_display, tag->x_window, tag->x_buf);
		tag->x_zoomed = 0;
		tag->x_quads[tag->x_buf] = 0;
	}
/*
 * Otherwise we have to pick out a quadrant.
 */
 	else
	{
	/*
	 * Calculate the new image if necessary.
	 */
		if (! tag->x_zimage || ! tag->x_current)
			xt_calc_zoom (tag);
	/*
	 * Housekeeping.
	 */
		tag->x_zx = x0 ? tag->x_xres/2 : 0;
		tag->x_zy = y0 ? 0 : tag->x_yres/2;
		tag->x_zoomed = TRUE;
		quad = (tag->x_zx ? 1 : 0) + (tag->x_zy ? 2 : 0) + 1;
	/*
	 * Switch buffers.
	 */
		tag->x_buf ^= 0x1;
		XBSetDrawBuffer (tag->x_display, tag->x_window, tag->x_buf, 0);
		XdSetDrawBuffer (tag->x_handle, tag->x_buf, 0);
	/*
	 * Now ship out the data.
	 */
		if (tag->x_quads[tag->x_buf] != quad)
		{
			if (tag->x_pseudo)
			    XdWriteRectangle (tag->x_handle, 2*tag->x_zx,
				   2*tag->x_zy, 0, 0, tag->x_xres, tag->x_yres,
				   2*tag->x_xres, tag->x_zimage, RedBank);
			else
			    XdWriteRectangleRGB (tag->x_handle, 2*tag->x_zx,
				   2*tag->x_zy, 0, 0, tag->x_xres, tag->x_yres,
				   2*tag->x_xres, tag->x_zimage);
		}		
		XBSetDisplayBuffer (tag->x_display, tag->x_window, tag->x_buf);
		tag->x_quads[tag->x_buf] = quad;
	}
/*
 * Flush it out, and we're done.
 */
	XdFlush (tag->x_handle);
}




xt_calc_zoom (tag)
struct xtag *tag;
/*
 * Calculate the zoomed image for this window.
 */
{
	int rast, col, offset, zwidth;
	unsigned char *dcp, *scp;
/*
 * If there is no memory allocated yet, do so now.
 */
	if (! tag->x_rimage)
	{
		tag->x_rimage = (unsigned char *)
			getvm (tag->x_xres*tag->x_yres);
		if (! tag->x_pseudo)
		{
			tag->x_gimage = (unsigned char *)
				getvm (tag->x_xres*tag->x_yres);
			tag->x_bimage = (unsigned char *)
				getvm (tag->x_xres*tag->x_yres);
		}
		tag->x_zimage = (unsigned char *)
			getvm (3*4*tag->x_xres*tag->x_yres);
	}
/*
 * Pull back the image from the screen.
 */
	XdReadRectangle (tag->x_handle, 0, 0, 0, 0, tag->x_xres, tag->x_yres,
			tag->x_xres, tag->x_rimage, RedBank);
	if (! tag->x_pseudo)
	{
		XdReadRectangle (tag->x_handle, 0, 0, 0, 0, tag->x_xres,
			tag->x_yres, tag->x_xres, tag->x_gimage, GreenBank);
		XdReadRectangle (tag->x_handle, 0, 0, 0, 0, tag->x_xres,
			tag->x_yres, tag->x_xres, tag->x_bimage, BlueBank);
	}
/*
 * Now replicate it.
 */
	scp = tag->x_rimage;
	dcp = tag->x_zimage;
	offset = 0;
	zwidth = (tag->x_pseudo ? 2 : 6)*tag->x_xres;
	for (rast = 0; rast < tag->x_yres; rast++)
	{
	/*
	 * Copy red.
	 */
	 	if (tag->x_pseudo)
		{
			xt_zap_prl (tag->x_rimage + offset, dcp, tag->x_xres);
			memcpy (dcp + zwidth, dcp, zwidth);
		}
		else
		{
			xt_zap_rl(tag->x_rimage + offset, dcp, tag->x_xres);
			xt_zap_rl(tag->x_gimage + offset, dcp + 1,tag->x_xres);
			xt_zap_rl(tag->x_bimage + offset, dcp + 2,tag->x_xres);
			memcpy (dcp + zwidth, dcp, zwidth);
		}
		dcp += 2*zwidth;
		offset += tag->x_xres;
	}
/*
 * Done.
 */
 	tag->x_current = TRUE;
}



xt_zap_rl (src, dst, width)
unsigned char *src, *dst;
int width;
/*
 * Zap over one raster line.
 */
{
	int i;

	for (i = 0; i < width; i++)
	{
		dst[0] = *src;
		dst[3] = *src++;
		dst += 6;
	}
}


xt_zap_prl (src, dst, width)
unsigned char *src, *dst;
int width;
/*
 * Zap over one pseudocolor raster line.
 */
{
	int i;

	for (i = 0; i < width; i++)
	{
		dst[0] = *src;
		dst[1] = *src++;
		dst += 2;
	}
}




xt_key (tag, ev)
struct xtag *tag;
XEvent *ev;
/*
 * Deal with a key event.
 */
{
	static char string[20] = { 0 };
	KeySym key;

	XLookupString (ev, string, 20, &key, 0);
	switch (key)
	{
	   case XK_F1:	
		xt_vp ((char *) tag, 0, 1, 2, 2);
		break;
	   case XK_F2:	
		xt_vp ((char *) tag, 1, 1, 2, 2);
		break;
	   case XK_F3:	
		xt_vp ((char *) tag, 0, 0, 2, 2);
		break;
	   case XK_F4:	
		xt_vp ((char *) tag, 1, 0, 2, 2);
		break;
	   case XK_F5:
		xt_vp ((char *) tag, 0, 0, tag->x_xres, tag->x_yres);
		break;
	}
}



xt_event (ctag)
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
			xt_key (tag, &ev);
		else if (ev.type == Expose)
		{
			printf ("Expose reset\n");
			XdReset ();
			ret = 1;
		}
	}
	return (ret);
}




xt_change (tag)
struct xtag *tag;
/*
 * Note that the display has changed.
 */
{
	tag->x_current = 0;
	tag->x_quads[0] = tag->x_quads[1] = 0;
	if (tag->x_zoomed)
	{
		tag->x_zwant = True;
		xt_vp ((char *) tag, 0, 0, tag->x_xres, tag->x_yres);	
	}
}





xt_readscreen (ctag, x, y, xs, ys, data)
char *ctag, *data;
int x, y, xs, ys;
/*
 * Read back a chunk of the screen.  This code is cloned from the sv
 * version; I expect it may not be right for reads anywhere except at
 * the origin.
 */
{
	struct xtag *tag = (struct xtag *) ctag;
	
	XdReadRectangle (tag->x_handle, x, y, 0, 0, xs, ys, xs, data,
			RedBank);
	return (GE_OK);
}


# endif /* DEV_XTITAN */
