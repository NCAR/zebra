/*
 * Raster display a rectangular array
 */
# include <errno.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/message.h"

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
# ifdef notdef
			ypos = (int)(ylo + (j + 0.5) / (float)(ydim - 1) *
				(yhi - ylo) + 0.5);
# endif
		/*
		 * Find the correct color and get a graphics context
		 * with the color in the foreground
		 */
# ifdef notdef
			r_color = (int) floor (Ncolor * 
				(DATA (i, j) - Datamin) / Datarange);
# endif
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





RasterXIPlot (w, d, array, xdim, ydim, xlo, ylo, xhi, yhi)
Widget	w;
Drawable 	d;
float	*array;
int	xlo, ylo, xhi, yhi;
int	xdim, ydim;
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
 * Allocate space for the ximage.
 */
	xim = (unsigned char *) malloc (width*height);
	ximp = xim;
	image = XCreateImage (XtDisplay (w),
		DefaultVisual (XtDisplay (w),
			XScreenNumberOfScreen (XtScreen (w))),
		8, ZPixmap, 0, (char *) xim, width, height, 8, width);
/*
 * Loop through the array points
 */
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
/*
 * Now we ship over the image, and deallocate everything.
 */
	XPutImage (XtDisplay (w), d, gcontext, image, 0, 0, xlo, yhi,
		width, height);
	XDestroyImage (image);	/* Zaps xim too */
	free (colgrid);
}


