/*
 * Raster display a rectangular array
 */
# include <errno.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/message.h"

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
static void RP_FPRasterize (), RP_IRasterize ();
# ifdef __STDC__
	static XImage *RP_GetXImage (Widget, int, int);
# else
	static XImage *RP_GetXImage ();
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
	gcontext = XCreateGC (XtDisplay (w), XtWindow (w), 0, &gcvals);
	XSetClipRectangles (XtDisplay (w), gcontext, 0, 0, &Clip, 1, 
		Unsorted);
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
	colinc = ((float) xdim)/((float) width);
	rowinc = -((float) ydim)/((float) height);
/*
 * Clip to the window, if appropriate.
 */
	if (xlo < Clip.x)
	{
		icol = (Clip.x - xlo - 1)*colinc;
		xlo = Clip.x;
		width -= Clip.x - xlo - 1;
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
	image = RP_GetXImage (w, width, height);
	ximp = image->data;
	if (fast)
		RP_IRasterize (ximp, width, height, colgrid, row, icol,
			rowinc, colinc, xdim);
	else
		RP_FPRasterize (ximp, width, height, colgrid, row, icol,
			rowinc, colinc, xdim);
/*
 * Now we ship over the image, and deallocate everything.
 */
	XPutImage (XtDisplay (w), d, gcontext, image, 0, 0, xlo, yhi,
		width, height);
	XFreeGC (XtDisplay (w), gcontext);
	free (colgrid);

# ifdef TIMING
	getrusage (RUSAGE_SELF, &ru);
	msec += (ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
		(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000;
	msg_ELog (EF_INFO, "XI raster time = %.3f sec", (float) msec/1000.0);
# endif
}





static void
RP_FPRasterize (ximp, width, height, colgrid, row, icol, rowinc, colinc, xdim)
unsigned char *ximp;
int width, height, xdim;
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
	}
}






static void
RP_IRasterize (ximp, width, height, colgrid, row, icol, rowinc, colinc, xdim)
unsigned char *ximp;
int width, height, xdim;
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
	}
}






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
