/* 8/87 jc */
/*
 * X Window System "device" driver.
 */

# include "param.h"

# ifdef DEV_XWIN

# include "graphics.h"
# include <X/Xlib.h>

# define XRES 700
# define YRES 700


/*
 * Our device tag type.
 */
struct x_tag
{
	Display	*xt_disp;	/* The Display identifier	*/
	Window	xt_win;		/* The window identifier	*/
};

/*
 * The number of colors we pretend to have.
 */
# define NCOLOR 150

/*
 * Line patterns.
 */
static int Lpats[4] = {
	SolidLine,
	DashedLine,
	DottedLine,
	DotDashLine
};


/*
 * Bitmap for a basic cursor.
 */
static short Cursmap[10] =
{
	0x001f,
	0x000f,
	0x0007,
	0x000b,
	0x0011,
	0x0020,
	0x0040,
	0x0080,
	0x0100,
	0x0200
};






gx_open (device, type, ctag)
char *device, *type, **ctag;
/*
 * Perform a device open.
 */
{
	struct x_tag *tag = (struct x_tag *) getvm (sizeof (struct x_tag));
	OpaqueFrame bframe;
	Cursor c;
/*
 * Open up the display.
 */
	if (! strcmp (device, "screen"))
		device = (char *) NULL;
	if ((tag->xt_disp = XOpenDisplay (device)) == (Display *) NULL)
	{
		relvm (tag);
		return (GE_BAD_DEVICE);
	}
/*
 * Create a window on the display.
 */
	bframe.border = WhitePixmap;
	bframe.bdrwidth = 2;
	bframe.background = BlackPixmap;
	if ((tag->xt_win = XCreate ("Graphics", "Graphics", "=700x700",
		"=700x700", &bframe, 700, 700)) == NULL)
		error ("Unable to create X window!");
	XMapWindow (tag->xt_win);
/*
 * Set up for event input.  We are interested in expose events, which mean
 * we have to repaint, as well as button events, for target reading.
 */
 	XSelectInput (tag->xt_win, ButtonPressed | ExposeWindow);
/*
 * Get our cursor set up.
 */
	c = XCreateCursor (10, 10, Cursmap, (short *) 0, 0, 0, 1, 0, GXcopy);
	XDefineCursor (tag->xt_win, c);
/*
 * It looks like we are done.
 */
	*ctag = (char *) tag;
	return (GE_OK);
}







gx_clear (ctag)
char *ctag;
/*
 * Perform a device clear.
 */
{
	struct x_tag *tag = (struct x_tag *) ctag;

	XClear (tag->xt_win);
}




gx_close (ctag)
char *ctag;
/*
 * Close down this device.
 */
{
	struct x_tag *tag = (struct x_tag *) ctag;

	XCloseDisplay (tag->xt_disp);
	relvm (ctag);
}





gx_flush (ctag)
char *ctag;
/*
 * Flush out all changes.
 */
{
	XSync (0);
}






gx_color (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
{
	struct x_tag *tag = (struct x_tag *) ctag;
	Color cdefs[NCOLOR];
	int col;
/*
 * Convert all of the color values to the X range.
 */
	for (col = 0; col < ncolor; col++)
	{
		cdefs[col].pixel = col + base;
		cdefs[col].red   = (unsigned short) (*r++ * 65535.0);
		cdefs[col].green = (unsigned short) (*g++ * 65535.0);
		cdefs[col].blue  = (unsigned short) (*b++ * 65535.0);
	}
/*
 * Now store the new colors.
 */
	XStoreColors (ncolor, cdefs);

	return (GE_OK);
}





gx_pl (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * This is the polyline draw routine.
 */
{
	struct x_tag *tag = (struct x_tag *) ctag;
	Vertex *vlist = (Vertex *) getvm (npt * sizeof (Vertex));
	int pt;
/*
 * Convert the vertex representation over to the X format.
 */
	for (pt = 0; pt < npt; pt++)
	{
		vlist[pt].flags = 0;
		vlist[pt].x = *data++;
		vlist[pt].y = YRES - *data++ - 1;
	}
/*
 * Perform the line draw.
 */
	XDrawDashed (tag->xt_win, vlist, npt, 1, 1, color,
		Lpats[ltype], GXcopy, AllPlanes);
/*
 * Clean up and quit.
 */
	relvm (vlist);
	return (GE_OK);
}



gx_casn (ctag, ncolor, base)
char *ctag;
int ncolor, *base;
/* 
 * Perform color assignment.
 */
{
	int planes, pixels[NCOLOR];

	if (XGetColorCells (1, NCOLOR, 0, &planes, pixels) == 0)
		return (GE_NCOLOR);
	*base = pixels[0];
	return (GE_OK);
}




gx_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag;
int x, y, xs, ys;
unsigned char *data;
int size, org;
/*
 * The pixel fill routine.
 */
{
	struct x_tag *tag = (struct x_tag *) ctag;
	int i;

	XPixmapBitsPutZ (tag->xt_win, x, YRES - (y + ys) - 1, xs, ys, data, 0,
		GXcopy, AllPlanes);
}




gx_target (ctag, x, y)
char *ctag;
int *x, *y;
/*
 * Return the target location.
 */
{
	XEvent ev;
	XButtonEvent *bev = (XButtonEvent *) &ev;
	struct x_tag *tag = (struct x_tag *) ctag;

	XWindowEvent (tag->xt_win, ButtonPressed, &ev);
	printf ("Button event, coords (%d, %d)\n", bev->x, bev->y);
	*x = bev->x;
	*y = YRES - bev->y - 1;
}





gx_check (ctag)
char *ctag;
/*
 * Check for exposure events.
 */
{
	struct x_tag *tag = (struct x_tag *) ctag;
	XEvent ev;
	XExposeEvent *eev = (XExposeEvent *) &ev;
	int ret = 0;
/*
 * Simply pull all exposure events off the queue.  If there are any at
 * all, we will return an exposure status.
 */
	XSync (0);
 	while (XCheckWindowEvent (tag->xt_win, ExposeRegion|ExposeWindow, &ev))
		ret++;
	return (ret);
}

# endif
