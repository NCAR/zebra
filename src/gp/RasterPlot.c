/*
 * Raster display a rectangular array
 */
static char *rcsid = "$Id: RasterPlot.c,v 2.0 1991-07-18 23:00:21 corbet Exp $";

# include <errno.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/message.h"
# include "GraphicsW.h"

# ifdef TIMING
# include <sys/time.h>
# include <sys/resource.h>
# endif

/*
 * A macro to reference the data array two dimensionally
 */
# define DATA(i,j)	array[(i) * ydim + (j)]

/*
 * Color stuff
 */
static int	Ncolor;
static XColor	*Colors, Color_outrange;

/*
 * Data range for the color scale
 */
static float	Datamin, Datamax, Datarange;

/*
 * Clipping rectangle
 */
static XRectangle	Clip;

/*
 * Forwards.
 */
/* static */ void RP_FPRasterize (), RP_IRasterize ();
# ifdef __STDC__
	static XImage *RP_GetXImage (Widget, int, int);
	static void RP_ImageRasterize (unsigned char *, int, int,
		unsigned char *, unsigned char *, double, double, double,
		double, int, int);
# else
	static XImage *RP_GetXImage ();
	static void RP_ImageRasterize ();
# endif

/*
 * Shared memory ximages, if we can.
 */
# ifdef SHM
# include <sys/ipc.h>
# include <sys/shm.h>
# include <X11/extensions/XShm.h>

static bool RP_ShmPossible ();
static XImage *RP_GetSharedXImage ();
# endif




RasterPlot (w, d, array, xdim, ydim, xlo, ylo, xhi, yhi)
Widget	w;
Drawable 	d;
float	*array;
int	xlo, ylo, xhi, yhi;
int	xdim, ydim;
/*
 * Draw contours of the rectangular (xdim x ydim) array into widget w.
 * The coordinates (xlo,ylo) and (xhi,yhi) specify the spatial extent of
 * the array in pixel coordinates.
 */
{
	int		dummy, r_color, xpos, ypos, i, j;
	unsigned int	dwidth, dheight, udummy;
	Window		win;
	float		y0, yinc, fypos, cscale;
	int		eheight, ewidth;
	GC		gcontext;
	XGCValues	gcvals;

# ifdef TIMING
	int msec;
	struct rusage	ru;

	getrusage (RUSAGE_SELF, &ru);
	msec = - ((ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
			(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000);
# endif
/*
 * Find the size of the drawable
 */
	XGetGeometry (XtDisplay (w), d, &win, &dummy, &dummy, &dwidth, 
		&dheight, &udummy, &udummy);
/*
 * Get a graphics context
 */
	gcontext = XCreateGC (XtDisplay (w), XtWindow (w), 0, &gcvals);
	XSetClipRectangles (XtDisplay (w), gcontext, 0, 0, &Clip, 1, 
		Unsorted);
/*
 * Find out the height and width for one element
 */
	ewidth = (int)(fabs ((float)(xhi - xlo) / (float)(xdim - 1)) + 1.0);
	eheight = (int)(fabs ((float)(yhi - ylo) / (float)(ydim - 1)) + 1.0);
/*
 * Figure some quantities to take stuff out of the loop.
 */
	y0 = ylo + 0.5 / (float)(ydim - 1) * (yhi - ylo) + 0.5;
	yinc = (yhi - ylo) / (float)(ydim - 1);
	cscale = Ncolor/Datarange;
/*
 * Loop through the array points
 */
	for (i = 0; i < xdim; i++)
	{
		xpos = (int)(xlo + (i - 0.5) / (float)(xdim - 1) * 
			(xhi - xlo) + 0.5);

		fypos = y0 - yinc;
		for (j = 0; j < ydim; j++)
		{
			ypos = (int) (fypos += yinc);
		/*
		 * Find the correct color and get a graphics context
		 * with the color in the foreground
		 */
			r_color = (int) (cscale * (DATA (j, i) - Datamin));

			if (r_color >= 0 && r_color < Ncolor)
				XSetForeground (XtDisplay (w), gcontext, 
					Colors[r_color].pixel);
			else
				XSetForeground (XtDisplay (w), gcontext, 
					Color_outrange.pixel);
		/*
		 * Draw a rectangle for this point
		 */
			XFillRectangle (XtDisplay (w), d, gcontext, xpos,
				ypos, ewidth, eheight);
		}
	}
/*
 * Free the GC
 */
	XFreeGC (XtDisplay (w), gcontext);

# ifdef TIMING
	getrusage (RUSAGE_SELF, &ru);
	msec += (ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
		(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000;
	msg_ELog (EF_INFO, "Rect rastor time = %.3f sec", (float) msec/1000.0);
# endif
}




void
RP_Init (colors, count, c_outrange, clip, dmin, dmax)
int	count;
XColor	*colors, c_outrange;
XRectangle	clip;
float	dmin, dmax;
/*
 * Initialize colors and data flagging
 *
 * CENTER	center color index
 * COUNT	number of colors in range to use for contouring
 * C_OUTRANGE	color index for contours which fall outside of the specified
 *		range of colors
 * CLIP		XRectangle to use for clipping
 * DMIN		minimum data value for the color scale
 * DMAX		maximum data value for the color scale
 */
{
	Ncolor = count;
	Colors = colors;
	Color_outrange = c_outrange;
	Clip = clip;

	Datamin = dmin;
	Datamax = dmax;
	Datarange = Datamax - Datamin;
}





RasterXIPlot (w, d, array, xdim, ydim, xlo, ylo, xhi, yhi, fast)
Widget	w;
Drawable 	d;
float	*array;
int	xlo, ylo, xhi, yhi;
int	xdim, ydim;
bool fast;
/*
 * Draw contours of the rectangular (xdim x ydim) array into widget w.
 * The coordinates (xlo,ylo) and (xhi,yhi) specify the spatial extent of
 * the array in pixel coordinates.
 *
 * (6/20/90 jc) Do the same thing using the XImage interface.
 */
{
	int		r_color, i, j, width, height;
	unsigned int	*colgrid, *cgp;
	float		cscale, *gp;
	float		row, col, rowinc, colinc, icol, irow;
	int		gridelem;
	int		outrange = Color_outrange.pixel;
	unsigned char	*xim, *ximp;
	XImage		*image;
	GC		gcontext;
	XGCValues	gcvals;
	Display		*disp = XtDisplay (w);

# ifdef TIMING
	int msec;
	struct rusage	ru;

	getrusage (RUSAGE_SELF, &ru);
	msec = - ((ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
			(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000);
# endif
/*
 * Get a graphics context
 */
	gcontext = XCreateGC (disp, XtWindow (w), 0, &gcvals);
	XSetClipRectangles (disp, gcontext, 0, 0, &Clip, 1, Unsorted);
/*
 * Find the size of the grid, and allocate temporary space to store the
 * computed colors.
 */
	gridelem = xdim*ydim;
	colgrid = (unsigned int *) malloc (gridelem * sizeof (int));
/*
 * Go through and color code everything now.
 */
	cscale = Ncolor/Datarange;
	for (i = 0, gp = array, cgp = colgrid; i < gridelem; i++)
	{
		r_color = (int) (cscale * (*gp++ - Datamin));
		*cgp++ = (r_color >= 0 && r_color < Ncolor) ? 
			Colors[r_color].pixel : outrange;
	}
	width = xhi - xlo + 1;
	height = ylo - yhi + 1;
/*
 * Figure our offsets into the color array.
 */
	row = ydim - 0.5;
	colinc = ((float) xdim - 1)/((float) width);
	rowinc = -((float) ydim)/((float) height);
/*
 * Clip to the window, if appropriate.
 */
	if (xlo < Clip.x)
	{
		icol = (Clip.x - xlo - 1)*colinc;
		width -= Clip.x - xlo - 1;
		xlo = Clip.x;
	}
	else
		icol = 0.5;
	if (width > Clip.width)
		width = Clip.width;
	if (ylo > (Clip.y + Clip.height))
		height -= ylo - (Clip.y + Clip.height);
	if (height > Clip.height)
	{
		row += (height - Clip.height)*rowinc;
		height = Clip.height;
		yhi = Clip.y;
	}
/*
 * Get our ximage and do the rasterization.
 */
# ifdef SHM
	if (RP_ShmPossible (disp))
		image = RP_GetSharedXImage (w, width, height);
	else
# endif
		image = RP_GetXImage (w, width, height);

	ximp = (unsigned char *) image->data;
	if (fast)
		RP_IRasterize (ximp, width, height, colgrid, row, icol,
			rowinc, colinc, xdim, image->bytes_per_line - width);
	else
		RP_FPRasterize (ximp, width, height, colgrid, row, icol,
			rowinc, colinc, xdim, image->bytes_per_line - width);
/*
 * Now we ship over the image, and deallocate everything.
 */
# ifdef SHM
	if (RP_ShmPossible (disp))
		XShmPutImage (disp, d, gcontext, image, 0, 0,
			xlo, yhi, width, height, False);
	else
# endif
	XPutImage (disp, d, gcontext, image, 0, 0, xlo, yhi, width, height);

	XFreeGC (disp, gcontext);
	free (colgrid);

# ifdef TIMING
	getrusage (RUSAGE_SELF, &ru);
	msec += (ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
		(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000;
	msg_ELog (EF_INFO, "XI raster time = %.3f sec", (float) msec/1000.0);
# endif
}





# ifdef SHM
static bool
RP_ShmPossible (disp)
Display *disp;
/*
 * Return TRUE iff we can do shared memory.
 */
{
	static bool known = FALSE, possible;
	int maj, min, sp;

	if (known)
		return (possible);
	known = True;
	possible = XShmQueryVersion (disp, &maj, &min, &sp);
	msg_ELog (EF_DEBUG, "Shared memory: %s", possible ? "True" : "False");
}
# endif



static void
RP_FPRasterize (ximp, width, height, colgrid, row, icol, rowinc, colinc,
	xdim, pad)
unsigned char *ximp;
int width, height, xdim, pad;
unsigned int *colgrid;
float row, icol, rowinc, colinc;
/*
 * Do rasterization using the old floating point (Ardent vectorizable) 
 * method.
 */
{
	int i, j;
	float col;

	for (i = 0; i < height; i++)
	{
		unsigned int *cp = colgrid + ((int) row)*xdim;

		col = icol;
		for (j = 0; j < width; j++)
		{
			*ximp++ = cp[(int) col];
			col += colinc;
		}
		row += rowinc;
		ximp += pad;	/* End of raster line padding.	*/
	}
}






# ifdef SHM

/*
 * This is our Ximage in shared memory.
 */
static int XIwidth = -1, XIheight = -1;
static XImage *image = 0;
static XShmSegmentInfo shminfo;



static XImage *
RP_GetSharedXImage (w, width, height)
Widget w;
int width, height;
/*
 * Return a shared-memory XImage with this geometry.
 */
{
	Display *disp = XtDisplay (w);
/*
 * If the geometry matches, we can just return what we got last time.  This
 * will be the case most of the time -- only when the window changes will
 * this change.
 */
	if (width == XIwidth && height == XIheight)
		return (image);
/*
 * If there is an existing image, get rid of it.
 */
	if (image)
	{
		XShmDetach (disp, &shminfo);
		XDestroyImage (image);
		shmdt (shminfo.shmaddr);
		shmctl (shminfo.shmid, IPC_RMID, 0);
	}
/*
 * Now get a new one.
 */
	image = XShmCreateImage (disp, 0, 8, ZPixmap, 0, &shminfo, width,
			height);
/*
 * All of the shmucking around.
 */
	shminfo.shmid = shmget (IPC_PRIVATE, image->bytes_per_line*height,
		IPC_CREAT | 0777);
	if (shminfo.shmid < 0)
		msg_ELog (EF_EMERGENCY, "SHM get failure (%d)!", errno);
	shminfo.shmaddr = (char *) shmat (shminfo.shmid, 0, 0);
	if (shminfo.shmaddr == (char *) -1)
		msg_ELog (EF_EMERGENCY, "SHM attach failure (%d)!", errno);
/*
 * Hook everything together.
 */
	image->data = shminfo.shmaddr;
	shminfo.readOnly = False;
	XShmAttach (disp, &shminfo);
/*
 * Save info and return the image.
 */
	XIwidth = width;
	XIheight = height;
	return (image);
}






void
RP_ZapSHMImage (w)
Widget w;
/*
 * If there is a shared memory image, get rid of it.
 */
{
	Display *disp = XtDisplay (w);

	if (image)
	{
		XShmDetach (disp, &shminfo);
		XDestroyImage (image);
		shmdt (shminfo.shmaddr);
		shmctl (shminfo.shmid, IPC_RMID, 0);
		image = 0;
	}
}

# endif




static XImage *
RP_GetXImage (w, width, height)
Widget w;
int width, height;
/*
 * Return an XImage with this geometry.
 */
{
	static int XIwidth = -1, XIheight = -1;
	static XImage *image = 0;
	char *xim;
/*
 * If the geometry matches, we can just return what we got last time.  This
 * will be the case most of the time -- only when the window changes will
 * this change.
 */
	if (width == XIwidth && height == XIheight)
		return (image);
/*
 * If there is an existing image, get rid of it.
 */
	if (image)
		XDestroyImage (image);
/*
 * Now get a new one.
 */
	xim = malloc (width*height);
	image = XCreateImage (XtDisplay (w),
		DefaultVisual (XtDisplay (w),
			XScreenNumberOfScreen (XtScreen (w))),
		8, ZPixmap, 0, xim, width, height, 8, width);
/*
 * Save info and return the image.
 */
	XIwidth = width;
	XIheight = height;
	return (image);
}



/* static */ void
RP_IRasterize (ximp, width, height, colgrid, row, icol, rowinc, colinc,
	xdim, pad)
unsigned char *ximp;
int width, height, xdim, pad;
unsigned int *colgrid;
float row, icol, rowinc, colinc;
/*
 * Do rasterization using the new integer-based (Sun-fast) method.
 */
{
	int i, j, icolinc;
# ifdef __STDC__
	static int col;
	static const short *s_col = (short *) &col;
# else
	int col;
	short *s_col = (short *) &col;
# endif
/*
 * Set up our integer values, which are simply the FP values scaled by 
 * 64K, so that the integer part is in the upper two bytes.  We then kludge
 * our way in with the short pointer to pull out the int part directly.
 */
	icolinc = (int) (colinc * 65536);
/*
 * Step through the data.
 */
	for (i = 0; i < height; i++)
	{
		unsigned int *cp = colgrid + ((int) row)*xdim;

		col = (int) (icol*65536);
		for (j = 0; j < width; j++)
		{
			*ximp++ = cp[*s_col];
			col += icolinc;
		}
		row += rowinc;
		ximp += pad;	/* End of raster line padding.	*/
	}
}





# ifdef SHM



void
RasterImagePlot (w, frame, grid, xd, yd, xlo, ylo, xhi, yhi, scale, bias)
Widget w;
int frame;
unsigned char *grid;
int xd, yd, xlo, ylo, xhi, yhi;
float scale, bias;
/*
 * Do a raster plot of a raster image.
 */
{
	unsigned char *destimg, cmap[256];
	int c, rcolor, width, height;
	float cscale = Ncolor/Datarange;
	float row, col, rowinc, colinc, icol, irow;
/*
 * Go through and make the color map.
 */
	for (c = 0; c <= 254; c++)
	{
		rcolor = (int) (cscale*(c*scale + bias - Datamin));
		cmap[c] = (rcolor >= 0 && rcolor < Ncolor) ? 
				Colors[rcolor].pixel : Color_outrange.pixel;
	}
	cmap[255] = Color_outrange.pixel;
/*
 * Figure our offsets into the color array.  Width and height are screen
 * parameters.  Colinc and rowinc are the number of raster image points to
 * move for every screen pixel we move.
 */
	width = xhi - xlo + 1;
	height = ylo - yhi + 1;
	row = 0.5;
	colinc = ((float) xd - 1)/((float) width);
	rowinc = ((float) yd)/((float) height);
/*
 * Clip to the window, if appropriate.
 */
	if (xlo < Clip.x)
	{
		icol = (Clip.x - xlo - 1)*colinc;
		width -= Clip.x - xlo - 1;
		xlo = Clip.x;
	}
	else
		icol = 0.5;
	if (width + (xlo - Clip.x) > Clip.width)
		width = Clip.width - (xlo - Clip.x);
	if (yhi < Clip.y)
	{
		row += (Clip.y - yhi)*rowinc;
		height -= (Clip.y - yhi);
		yhi = Clip.y;
	}
	if (height + (yhi - Clip.y) > Clip.height)
		height = Clip.height - (yhi - Clip.y);
/*
 * Find the image space and start rasterizing.  Also force a sync with 
 * the server; things could happen in the wrong order otherwise.
 */
	eq_sync ();
	destimg = (unsigned char *) GWGetFrameAddr (w, frame);
	destimg += yhi*GWWidth(w) + xlo;
	RP_ImageRasterize (destimg, width, height, grid, cmap, row, icol,
		rowinc, colinc, xd, GWWidth (w) - width);
/*
 * Done!
 */
}




static void
RP_ImageRasterize (ximp, width, height, grid, cmap, row, icol, rowinc, colinc,
	xdim, pad)
unsigned char *ximp, *cmap;
int width, height, xdim, pad;
unsigned char *grid;
float row, icol, rowinc, colinc;
/*
 * Do rasterization using the new integer-based (Sun-fast) method.
 */
{
	int i, j, icolinc;
# ifdef __STDC__
	static int col;
	static const short *s_col = (short *) &col;
# else
	int col;
	short *s_col = (short *) &col;
# endif
/*
 * Set up our integer values, which are simply the FP values scaled by 
 * 64K, so that the integer part is in the upper two bytes.  We then kludge
 * our way in with the short pointer to pull out the int part directly.
 */
	icolinc = (int) (colinc * 65536);
/*
 * Step through the data.
 */
	for (i = 0; i < height; i++)
	{
		unsigned char *cp = grid + ((int) row)*xdim;

		col = (int) (icol*65536);
		for (j = 0; j < width; j++)
		{
			*ximp++ = cmap[cp[*s_col]];
			col += icolinc;
		}
		row += rowinc;
		ximp += pad;	/* End of raster line padding.	*/
	}
}


# endif
