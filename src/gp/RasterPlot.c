/*
 * Raster display a rectangular array
 */
static char *rcsid = "$Id: RasterPlot.c,v 2.17 1995-06-16 16:19:43 burghart Exp $";
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include <errno.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include "config.h"		/* dependent on changes in SHM def */
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include "GraphProc.h"

# ifdef TIMING
# include <sys/time.h>
# include <sys/resource.h>
# endif
# include <byteorder.h>
/*
 * Shared memory ximages, if we can.
 */
# ifdef SHM
# 	include <sys/ipc.h>
# 	include <sys/shm.h>
# 	include <X11/extensions/XShm.h>
# endif

# ifdef SHM
static char shmopt[] = "$XShm: Compiled $";
# else
static char shmopt[] = "$XShm: NOT Compiled $";
# endif /* SHM */

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
 * Highlight color stuff.
 */
static Boolean	Highlight;
static XColor	HColor;
static float	HValue, HRange;

/*
 * Data range for the color scale
 */
static float	Datamin, Datamax, Datarange;

/*
 * Clipping rectangle
 */
static XRectangle	Clip;

/*
 * Kludgery of sorts: some of the raster code uses "fake floats" -- 32-bit
 * ints where the least-significant two bytes contain the fractional part
 * of a floating point number.  The point of all this is to allow floating
 * point precision (sort of) precision while making the integer part really
 * fast to get at.
 *
 * Here we give the offset (in shorts) to the integer part of such a number.
 */
# ifdef LITTLE_ENDIAN
# define FF_OFFSET 1
# else
# define FF_OFFSET 0
# endif


/*--------------------------------------------------
 * Public prototypes
 */



/*--------------------------------------------------
 * Private forwards
 */
static void RP_FPRasterize FP((
	unsigned char *ximp,
	int width, int height, 
	unsigned int *colgrid, 
	double row, double icol, 
	double rowinc, double colinc,
	int xdim, 
	int pad));

static void RP_IRasterize FP((
	unsigned char *ximp,
	int width, int height, 
	unsigned int *colgrid, 
	double row, double icol, 
	double rowinc, double colinc,
	int xdim, 
	int pad));

static XImage *RP_GetXImage FP((Widget, int, int));

static void RP_ImageRasterize FP ((unsigned char *ximp,
				   int width, int height,
				   unsigned char *grid,
				   unsigned char *cmap,
				   double row, double icol, 
				   double rowinc, double colinc,
				   int xdim, int pad));

/* static bool RP_ShmPossible FP ((Display *disp)); */
# ifdef SHM
static XImage *RP_GetSharedXImage FP ((Widget w, int width, int height));
# endif

/*--------------------------------------------------end prototypes--*/



void
RasterPlot (w, d, array, xdim, ydim, xlo, ylo, xhi, yhi)
Widget	w;
Drawable d;
float	*array;
int	xdim, ydim;
int	xlo, ylo, xhi, yhi;
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
			if (Highlight 
			   && (DATA (j, i) <= (HValue + HRange/2.0))
			   && (DATA (j, i) >= (HValue - HRange/2.0)))
				XSetForeground (XtDisplay (w), gcontext, 
					HColor.pixel);
			else
			{
				r_color = (int) (cscale * (DATA (j, i) - 
					Datamin));

				if (r_color >= 0 && r_color < Ncolor)
					XSetForeground (XtDisplay (w), 
					   gcontext, Colors[r_color].pixel);
				else
					XSetForeground (XtDisplay (w), 
					   gcontext, Color_outrange.pixel);
			}
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
RP_Init (colors, count, c_outrange, clip, dmin, dmax, highlight, hvalue, 
	hcolor, hrange)
XColor	*colors;
int	count;
XColor	c_outrange; 
XRectangle clip;
float	dmin, dmax;
Boolean	highlight;
float	hvalue;
XColor	hcolor;
float	hrange;
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

	Highlight = highlight;
	if (highlight)
	{
		HValue = hvalue;
		HColor = hcolor;
		HRange = hrange;
	}
}




void
RasterXIPlot (w, d, array, xdim, ydim, xlo, ylo, xhi, yhi, fast)
Widget	w;
Drawable d;
float	*array;
int	xdim, ydim;
int	xlo, ylo, xhi, yhi;
bool	fast;
/*
 * Draw a raster image of rectangular (xdim x ydim) array into widget w.
 * The coordinates (xlo,ylo) and (xhi,yhi) specify the centers of the
 * corner elements, in pixel space.  Since they are the centers, we will 
 * actually plot half a grid width outside of these bounds on each side 
 * (neglecting clipping, which may reduce this).
 */
{
	int		r_color, i, width, height;
	unsigned int	*colgrid, *cgp;
	float		cscale, *gp;
	float		leftcol, toprow, rowinc, colinc;
	int		gridelem;
	int		outrange = Color_outrange.pixel;
	unsigned char	*ximp;
	XImage		*image;
#ifdef SHM
	bool		using_shared;
#endif
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
		if (Highlight 
		   && (*gp <= (HValue + HRange/2.0)) 
		   && (*gp >= (HValue - HRange/2.0)))
		{
			*gp++;
			*cgp++ = HColor.pixel;
		}
		else
		{
			r_color = (int) (cscale * (*gp++ - Datamin));
			*cgp++ = (r_color >= 0 && r_color < Ncolor) ? 
				Colors[r_color].pixel : outrange;
		}
	}
/*
 * Columns per pixel and rows per pixel.  We use (xdim - 1) and (ydim - 1)
 * here since our pixel limits refer to the *centers* of the edge grid 
 * elements.
 */
	colinc = ((float) xdim - 1) / ((float) (xhi - xlo + 1));
	rowinc = -((float) ydim - 1) / ((float) (ylo - yhi + 1));
/*
 * Add half a grid width to our dimensions, so that we plot full squares
 * on the edges.  Again, this is because our current limits refer to the
 * centers of the edge grid elements.
 */
	xlo -= 0.5 / colinc;
	xhi += 0.5 / colinc;
	leftcol = -0.5;	/* column (floating) corresponding to xlo */

	ylo -= 0.5 / rowinc;
	yhi += 0.5 / rowinc;
/*	toprow = (ydim - 1) + 0.5; /* row (floating) corresponding to yhi */
	toprow = ydim + rowinc;
/*
 * Clip to the window, if appropriate.
 */
	if (xlo < Clip.x)
	{
		leftcol += (Clip.x - xlo + 1) * colinc;
		xlo = Clip.x;
	}

	if (xhi > (Clip.x + Clip.width))
		xhi = Clip.x + Clip.width;

	if (yhi < Clip.y)
	{
		toprow += (Clip.y - yhi + 1) * rowinc;
		yhi = Clip.y;
	}
	
	if (ylo > (Clip.y + Clip.height))
		ylo = Clip.y + Clip.height;
/*
 * Now we can figure the width and height
 */
	width = xhi - xlo + 1;
	height = ylo - yhi + 1;
/*
 * If width or height is < 0, then this picture's outside the window
 */
	if (width < 0 || height < 0)
	{		
		XFreeGC (disp, gcontext);
		free (colgrid);
		msg_ELog (EF_DEBUG, "Raster outside the window not plotted");
		return;
	}
/*
 * Get our ximage and do the rasterization.
 */
# ifdef SHM
	using_shared = FALSE;
	image = NULL;
	if (GWShmPossible (Graphics))
		image = RP_GetSharedXImage (w, width, height);
	if (image)
		using_shared = TRUE;
	else
# endif
		image = RP_GetXImage (w, width, height);

	ximp = (unsigned char *) image->data;
	if (fast)
		RP_IRasterize (ximp, width, height, colgrid, toprow, leftcol,
			rowinc, colinc, xdim, image->bytes_per_line - width);
	else
		RP_FPRasterize (ximp, width, height, colgrid, toprow, leftcol,
			rowinc, colinc, xdim, image->bytes_per_line - width);
/*
 * Now we ship over the image, and deallocate everything.
 */
# ifdef SHM
	/* if (GWShmPossible (Graphics)) */
	if (using_shared)
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




static void
RP_FPRasterize (ximp, width, height, colgrid, row, icol, rowinc, colinc,
		xdim, pad)
unsigned char	*ximp;
int 		width, height;
unsigned int 	*colgrid;
float 		row, icol, rowinc, colinc;
int 		xdim, pad;
/*
 * Do rasterization using the old floating point (Ardent vectorizable) 
 * method.
 */
{
	int i, j;
	float col;

	for (i = 0; i < height; i++)
	{
		unsigned int *cp = colgrid + ((int) row) * xdim;

		col = icol + 0.5; /* Add 0.5 to get closest int below */
		for (j = 0; j < width; j++)
		{
			*ximp++ = cp[(int)col];
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
static bool shm_failed = FALSE;	/* If we fail once, don't try it again */


static XImage *
RP_GetSharedXImage (w, width, height)
Widget w;
int width, height;
/*
 * Return a shared-memory XImage with this geometry.
 */
{
	Display *disp = XtDisplay (w);

	if (shm_failed)
		return (NULL);
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
/*
 * Check for failures.  More than likely the failures will be from lack of
 * shared memory facilities or high enough shared memory limits in the kernel.
 * Therefore, once we fail once, it behooves us not to try again.
 */
	if (shminfo.shmid < 0)
	{
		shm_failed = TRUE;
		msg_ELog (EF_EMERGENCY, "rp SHM get failure (%d)!", errno);
	}
	else if ((shminfo.shmaddr = (char *) shmat (shminfo.shmid, 0, 0))
		 == (char *) -1)
	{
		shm_failed = TRUE;
		msg_ELog (EF_EMERGENCY, "rp SHM attach failure (%d)!", 
			  errno);
	}
/*
 * Deal with our failures
 */
	if (shm_failed)
	{
		XDestroyImage (image);
		image = NULL;
		msg_ELog (EF_INFO, "Raster: Abandoning XShmExtension attempt");
		return (NULL);
	}
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

# endif /* SHM */




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



static void
RP_IRasterize (ximp, width, height, colgrid, row, icol, rowinc, colinc,
	xdim, pad)
unsigned char 	*ximp;
int 		width, height;
unsigned int 	*colgrid;
float 		row, icol, rowinc, colinc;
int		xdim, pad;
/*
 * Do rasterization using the new integer-based (Sun-fast) method.
 */
{
	int i, j, icolinc;
# ifdef __STDC__
	static int col;
	static const short *s_col = FF_OFFSET + (short *) &col;
# else
	int col;
	short *s_col = FF_OFFSET + (short *) &col;
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
		unsigned int *cp = colgrid + ((int) row) * xdim;

		col = (int) ((icol + 0.5) * 65536);
		for (j = 0; j < width; j++)
		{
			*ximp++ = cp[*s_col];
			col += icolinc;
		}
		row += rowinc;
		ximp += pad;	/* End of raster line padding.	*/
	}
}




/*-----------------------------------------------------------------
 * The following routines do raster plots of data that already
 * have an Image organization.  If shared memory is supported and
 * possible, it is used.
 */


void
RasterImagePlot (w, frame, grid, xd, yd, xlo, ylo, xhi, yhi, scale, bias)
Widget		w;
int 		frame;
unsigned char 	*grid;
int 		xd, yd, xlo, ylo, xhi, yhi;
float 		scale, bias;
/*
 * Plot the  (xd x yd) raster image into widget w.
 * The coordinates (xlo,ylo) and (xhi,yhi) specify the centers of the
 * corner elements, in pixel space.  Since they are the centers, we will 
 * actually plot half a grid width outside of these bounds on each side 
 * (neglecting clipping, which may reduce this).
 */
{
	unsigned char *destimg, cmap[256];
	int c, rcolor, width, height;
	float cscale = Ncolor/Datarange;
	float toprow, leftcol, rowinc, colinc;
	GC gcontext;
	XGCValues gcvals;
	Display *disp = XtDisplay(w);
	XImage *image;
/*
 * Go through and make the color map.
 */
	for (c = 0; c <= 254; c++)
	{
		if (Highlight 
		   && ((c*scale + bias) <= (HValue + HRange/2.0)) 
		   && ((c*scale + bias) >= (HValue - HRange/2.0)))
			cmap[c] = HColor.pixel;
		else
		{
			rcolor = (int) (cscale*(c*scale + bias - Datamin));
			cmap[c] = (rcolor >= 0 && rcolor < Ncolor) ? 
				Colors[rcolor].pixel : Color_outrange.pixel;
		}
	}
	cmap[255] = Color_outrange.pixel;
/*
 * Columns per pixel and rows per pixel.  We use (xd - 1) and (yd - 1)
 * here since our pixel limits refer to the *centers* of the edge grid 
 * elements.
 */
	colinc = ((float) xd - 1) / ((float) (xhi - xlo + 1));
	rowinc = ((float) yd - 1) / ((float) (ylo - yhi + 1));
/*
 * Add half a grid width to our dimensions, so that we plot full squares
 * on the edges.  Again, this is because our current limits refer to the
 * centers of the edge grid elements.
 */
	xlo -= 0.5 / colinc;
	xhi += 0.5 / colinc;
	leftcol = -0.5;	/* column (floating) corresponding to xlo */

	ylo += 0.5 / rowinc;
	yhi -= 0.5 / rowinc;
	toprow = -0.5; /* row (floating) corresponding to yhi */
/*
 * Clip to the window, if appropriate.
 */
	if (xlo < Clip.x)
	{
		leftcol += (Clip.x - xlo + 1) * colinc;
		xlo = Clip.x;
	}

	if (xhi > (Clip.x + Clip.width))
		xhi = Clip.x + Clip.width;

	if (yhi < Clip.y)
	{
		toprow += (Clip.y - yhi + 1) * rowinc;
		yhi = Clip.y;
	}
	
	if (ylo > (Clip.y + Clip.height))
		ylo = Clip.y + Clip.height;
/*
 * Now we can figure the width and height
 */
	width = xhi - xlo + 1;
	height = ylo - yhi + 1;
/*
 * Get a graphics context
 */
	gcontext = XCreateGC( disp, XtWindow(w), 0, &gcvals);
	XSetClipRectangles( disp, gcontext, 0, 0, &Clip, 1, Unsorted);
/*
 * Find the image space and start rasterizing.  Also force a sync with 
 * the server; things could happen in the wrong order otherwise.
 */
	eq_sync ();

# ifdef SHM
	/*
	 * The raster image will be written directly to the 
	 * server's memory through the Graphics Widget's shared memory,
	 * unless shared memory is not possible... in which case we
	 * must create a local image, process our data, and then
	 * send the image to the server.
	 */
	if (GWFrameShared (Graphics, frame))
	{
		destimg = (unsigned char *) GWGetFrameAddr (w, frame);
		destimg += yhi * GWGetBPL(w, frame) + xlo;
		RP_ImageRasterize (destimg, width, height, grid, cmap, toprow, 
				   leftcol, rowinc, colinc, xd, 
				   GWGetBPL (w, frame) - width);
	}
	else
# endif
	{
		image = RP_GetXImage(w, width, height);
		RP_ImageRasterize ((unsigned char *)(image->data), 
				   width, height, grid, cmap, toprow, leftcol,
				   rowinc, colinc, xd, 
				   image->bytes_per_line - width);
		/*
		 * Now send our local XImage to the server
	 	 */
		XPutImage(disp, GWFrame(w), gcontext, image, 0, 0,
			  xlo, yhi, width, height);
	}
	XFreeGC(disp, gcontext);
/*
 * Done!
 */
}




static void
RP_ImageRasterize (ximp, width, height, grid, cmap, row, icol, rowinc, colinc,
		   xdim, pad)
unsigned char 	*ximp;
int 		width, height;
unsigned char 	*grid;
unsigned char	*cmap;
float 		row, icol, rowinc, colinc;
int		xdim, pad;
/*
 * Do rasterization using the new integer-based (Sun-fast) method.
 */
{
	int i, j, icolinc;

# ifdef __STDC__
	static int col;
	static const short *s_col = FF_OFFSET + (short *) &col;
# else
	int col;
	short *s_col = FF_OFFSET + (short *) &col;
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
		unsigned char *cp = grid + (int)(row + 0.5) * xdim;

		col = (int) ((icol + 0.5) * 65536);
		for (j = 0; j < width; j++)
		{
			*ximp++ = cmap[cp[*s_col]];
			col += icolinc;
		}
		row += rowinc;
		ximp += pad;	/* End of raster line padding.	*/
	}
}

