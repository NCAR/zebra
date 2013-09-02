/*
 * Low-level image access, hopefully hiding the details of how it's done.
 */
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>

# include <zebra.h>
# include <message.h>
# include <GraphicsW.h>
# include <byteorder.h>

# include "GraphProc.h"
# include "RasterImage.h"

# ifdef SHM
# 	include <sys/ipc.h>
# 	include <sys/shm.h>
# 	include <X11/extensions/XShm.h>
# endif


RCSID ("$Id: RasterImage.c,v 2.6 1999-11-01 21:27:13 granger Exp $");

/*
 * Forwards.
 */
static int ri_ShmWorks (DestImage *);
static void ri_XImageSetup (DestImage *);


static inline int
ri_OurOrder ()
/*
 * Return our byte ordering, using the X11 symbols.
 */
{
	return (BigEndian () ? MSBFirst : LSBFirst);
}





# ifndef SHM
static inline int
ri_ShmWorks (DestImage *i)
/*
 * If shared memory isn't supported at all (should we even bother with this
 * case??), just bail quickly.
 */
{
	return (FALSE);
}
# else /* SHM */

static int
ri_ShmWorks (DestImage *i)
/*
 * Attempt to arrange for this image to work in the shared memory pixmap
 * mode.
 */
{
/*
 * If we're not actually operating in the shared memory mode (over the
 * net or some such) bail now.
 */
	if (! GWFrameShared (Graphics, i->di_frame))
		return (FALSE);
/*
 * OK, we're gonna make a go for this.  Start by forcing a sync with the
 * server; otherwise queued operations could go in after we have
 * scribbled in the shared pixmap.
 */
	eq_sync ();
/*
 * Now just fill in the info.
 */
	i->di_type = RI_ShmImage;
	i->di_image = GWGetFrameAddr (Graphics, i->di_frame);
	i->di_ximage = 0;
	i->di_bpl = GWGetBPL (Graphics, i->di_frame);
	i->di_ioffset = i->di_y*i->di_bpl + i->di_x*i->di_bdepth;
	i->di_needswap = (i->di_bdepth > 1) && (ri_OurOrder () !=
			GWGetByteOrder (Graphics, i->di_frame));
	return (TRUE);
}

# endif /* SHM */


static void
ri_ClearBackground (DestImage *i)
/*
 * Fill the image with the background pixel
 */
{
    int ix, iy;
    XColor xc_bg;
    Pixel bg;
    unsigned char *dp;

    XtVaGetValues (Graphics, XtNbackground, &xc_bg, NULL);
    bg = xc_bg.pixel;
    dp = i->di_image;

    for (iy = 0; iy < i->di_h; iy++)
    {
	for (ix = 0; ix < i->di_w; ix++)
	{
	    unsigned short *sdp = (unsigned short*) dp;
	    unsigned int *ldp = (unsigned int*) dp;
		
	    switch (i->di_bdepth)
	    {
	    case 1:
		*dp = bg;
		break;
	    case 2:
		*sdp = bg;
		break;
	    case 4:
		*ldp = bg;
		break;
	    }

	    dp += i->di_bdepth;
	}
    }
}



static void
ri_XImageSetup (DestImage *i)
/*
 * Make this one work just using a normal XImage.
 */
{
	Display *display = XtDisplay (Graphics);

	i->di_type = RI_XImage;
	i->di_image = malloc (i->di_w * i->di_h * i->di_bdepth);
    /*
     * Fill the image with the background pixel
     */
	if (! i->di_transparent)
		ri_ClearBackground (i);
    /*
     * Make the XImage and finish up
     */
	i->di_ximage = XCreateImage (display,
			DefaultVisual (display,
				XScreenNumberOfScreen (XtScreen (Graphics))),
			GWDepth (Graphics), ZPixmap, 0, i->di_image,
			i->di_w, i->di_h, i->di_bdepth*8, 0);
	i->di_bpl = i->di_ximage->bytes_per_line;
	i->di_ioffset = 0;
	i->di_needswap = (i->di_bdepth > 1) &&
		(ri_OurOrder () != i->di_ximage->byte_order);
	/*
	 * If attempting transparency, try to read the current background
	 * into this image.
	 */
	if (i->di_transparent)
	{
		   if (! XGetSubImage (display, GWFrame (Graphics), 
				       i->di_x, i->di_y, i->di_w, i->di_h, 
				       AllPlanes, ZPixmap, i->di_ximage,
				       0, 0))
		   {
			   msg_ELog (EF_PROBLEM, "getsubimage failed %s",
				     "for raster transparency");
		   }
		   else
		   {
			   msg_ELog (EF_DEBUG, "raster transparency: %s",
				     "background image copied");
		   }
	}
}
			
				     




DestImage *
ri_MakeMemImage (int w, int h, void *image, int bdepth, int bpl)
/*
 * Make a memory-resident image, using the given pointer and geometry.
 * We do not allocate the image memory, or make any assumptions about
 * whether it's freeable.
 */
{
	DestImage *ret = (DestImage *) malloc (sizeof (DestImage));

	ret->di_type = RI_MemImage;
	ret->di_x = ret->di_y = 0;
	ret->di_ioffset = 0;
	ret->di_w = w;
	ret->di_h = h;
	ret->di_image = image;
	ret->di_bdepth = bdepth;
	ret->di_bpl = bpl;
	ret->di_ximage = 0;
	ret->di_needswap = 0;
	return (ret);
}





DestImage *
ri_CreateImage (int frame, int xdest, int ydest, int w, int h, int transparent)
/*
 * Get a destination image, using the best approach we can arrange.  xdest
 * and ydest are where the image will end up in the display once we're
 * done; w and h are its dimensions, which should fit within the display
 * window.
 *
 * Return value is a DestImage which may be messed with.
 */
{
	DestImage *ret = (DestImage *) malloc (sizeof (DestImage));
/*
 * Fill in the common part of the destimage structure.
 */
	ret->di_transparent = transparent;
	ret->di_x = xdest;
	ret->di_y = ydest;
	ret->di_w = w;
	ret->di_h = h;
	ret->di_frame = frame;
	ret->di_bdepth = GWBDepth (Graphics);
/*
 * Attempt to arrange this with a shared memory pixmap, in which case
 * we will already hold the current background image (I think?) so
 * transparency needs no special handling.
 */
	if (ri_ShmWorks (ret))
	{
		msg_ELog (EF_DEBUG, "raster transparency: using shared image");
		return (ret);
	}
/*
 * Otherwise it's ximage time.
 */
	ri_XImageSetup (ret);
	return (ret);
}



DestImage *
ri_GetDestImage (int frame, int xdest, int ydest, int w, int h)
{
    return ri_CreateImage (frame, xdest, ydest, w, h, 0);
}



DestImage *
ri_GetTransparentImage (int frame, int xdest, int ydest, int w, int h)
{
    return ri_CreateImage (frame, xdest, ydest, w, h, 1);
}



void
ri_ShipImage (DestImage *i)
/*
 * Get this image onto the display, and clean it up.
 */
{
/*
 * If it's an ximage, we have to actually ship it.  Otherwise just clean up.
 */
	if (i->di_type == RI_XImage)
	{
		XPutImage (Disp, GWFrame (Graphics), Gcontext, i->di_ximage,
				0, 0, i->di_x, i->di_y, i->di_w, i->di_h);
		XDestroyImage (i->di_ximage);
	}
	free (i);
}
