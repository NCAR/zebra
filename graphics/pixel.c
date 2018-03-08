/* 7/87 jc */
/* $Id: pixel.c,v 1.5 2002-07-11 23:14:34 burghart Exp $ */
/*
 * Operations relating to pixel maps are performed here.
 */
# include "param.h"
# include "graphics.h"
# include "workstation.h"
# include "device.h"
# include "pixel.h"
# include "overlay.h"

/*
 * Bit patterns used to create the various line types in a pixel map.
 */
static unsigned short Vpats[GPLT_NTYPES] =
{
	0xffff,		/* Solid line		*/
	0xff00,		/* Dashed		*/
	0xcccc,		/* Dotted		*/
	0xff18		/* dot-dash		*/
};

/*
 * Absolute value.
 */
# define ABS(v) ((v) < 0 ? - (v) : (v))


/*
 * BIG_INSERT determines the number of pixels we consider to be a "large"
 * chunk in gp_insert.
 */
# define BIG_INSERT	512

/*
 * Forwards
 */
void gp_clear();



struct pixmap *
gp_make_pmap (dev)
struct device *dev;
/*
 * Create a new (full screen) pixel map for this device.  
 */
{
	struct pixmap *map = (struct pixmap *) getvm (sizeof (struct pixmap));
	int i;
/*
 * Just fill in the various fields of the structure.
 */
 	map->pm_data = (unsigned char *) getvm (dev->gd_xres * dev->gd_yres);
	map->pm_xres = dev->gd_xres;
	map->pm_yres = dev->gd_yres;
	map->pm_xb = dev->gd_xb;
	map->pm_yb = dev->gd_yb;
	map->pm_bsize = (map->pm_xb)*(map->pm_yb);
	map->pm_nxb = dev->gd_xres/dev->gd_xb;  /* Assumes even division! */
	map->pm_mod = getvm (map->pm_nxb * (dev->gd_yres/dev->gd_yb));
	map->pm_flags = 0;
	map->pm_xmap = (int *) getvm (dev->gd_xres * sizeof (int));
	map->pm_ymap = (int *) getvm (dev->gd_yres * sizeof (int));
	map->pm_bg = dev->gd_background;
/*
 * Create the quick lookup X and Y maps.
 */
 	if (dev->gd_flags & GDF_TOP)
	{
		int stuff = map->pm_xb*(map->pm_yb - 1);
		map->pm_flags |= PF_TOP;
		for (i = 0; i < map->pm_xres; i++)
		{
			map->pm_xmap[i] = (i/map->pm_xb);
			map->pm_xmap[i] = map->pm_xmap[i]*
				map->pm_xb*map->pm_yb + 
				  (i % map->pm_xb) + stuff;
		}
		for (i = 0; i < map->pm_yres; i++)
		{
			map->pm_ymap[i] = i/map->pm_yb;
			map->pm_ymap[i] = map->pm_ymap[i]*
				map->pm_xb*map->pm_nxb*map->pm_yb
				  - map->pm_xb*(i % map->pm_yb);
		}
	}
	else
	{
		for (i = 0; i < map->pm_xres; i++)
		{
			map->pm_xmap[i] = (i/map->pm_xb);
			map->pm_xmap[i] = map->pm_xmap[i]*map->pm_xb*
					map->pm_yb + (i % map->pm_xb);
		}
		for (i = 0; i < map->pm_yres; i++)
		{
			map->pm_ymap[i] = i/map->pm_yb;
			map->pm_ymap[i] = map->pm_ymap[i]*map->pm_xb*
				map->pm_nxb*map->pm_yb
				  + map->pm_xb*(i % map->pm_yb);
		}
	}
/*
 * Zero out the entire thing.
 */
	gp_clear (map);
/*
 * Finally, return a pointer to this whole mess.
 */
 	return (map);
}




void 
gp_pl (ov, color, ltype, npt, ndata)
struct overlay *ov;
int color, ltype, npt, *ndata;
/*
 * Draw this polyline into this pixel map.
 * Entry:
 *	OV	is the overlay of interest, which is a pixmap overlay.
 *	COLOR	is the color of the line to be drawn.
 *	LTYPE	is the line type.
 *	NPT	is the number of points in the polyline.
 *	NDATA	is the actual line data.
 * Exit:
 *	The line has been drawn into the map, with clip windows taken
 *	into account.
 */
{
	int bit = 0, bcol, px, py;
	float incx, incy, len, x, y;

	while (--npt > 0)
	{
	/*
	 * Figure out what our longer dimension is.
	 */
		len = ABS(ndata[2] - ndata[0]);
		if ( ABS(ndata[3] - ndata[1]) > len)
			len = ABS(ndata[3] - ndata[1]);
	/*
	 * Now calculate our increments in each direction.
	 */
		if (len != 0)
		{
			incx = ((float) (ndata[2] - ndata[0])) / len;
			incy = ((float) (ndata[3] - ndata[1])) / len;
		}
		x = 0.5 + (float) ndata[0];
		y = 0.5 + (float) ndata[1];
	/*
	 * Draw the line.
	 */
		for (; len >= 0; len--)
		{
		/*
		 * See which color to use in drawing this line.
		 */
		 	bcol = ((1 << bit) & Vpats[ltype]) ? color :
					ov->ov_pmap->pm_bg;
			if (++bit > 15)
				bit = 0;
		/*
		 * If we are within the clip window, draw the pixel.
		 */
			px = (int) x;
			py = (int) y;
			if (px >= ov->ov_cx0 && px <= ov->ov_cx1 && 
			    py >= ov->ov_cy0 && py <= ov->ov_cy1)
				STORE_PXL (ov->ov_pmap, px, py, bcol);
		/*
		 * Move on to the next pixel.
		 */
		 	x += incx; y += incy;
		}
	/*
	 * Move on to the next segment.
	 */
	 	ndata += 2;
	}
}



void
gp_update (wstn, pm, unconditional)
struct workstation *wstn;
struct pixmap *pm;
int unconditional;
/*
 * Physically update this pixmap onto the display.
 */
{
	struct device *dev = wstn->ws_dev;
	int blk, nblk;
/*
 * If this device does hardware clipping, we better disable it now, or
 * our stuff might not get out.
 */
	if (dev->gd_flags & GDF_HCW)
		(*dev->gd_set_hcw) (wstn->ws_tag, 0, 0, dev->gd_xres-1,
			dev->gd_yres - 1);
/*
 * Step through each block, and see which ones are modified.
 */
 	nblk = pm->pm_nxb * (pm->pm_yres/pm->pm_yb);
	for (blk = 0; blk < nblk; blk++)
	{
		int xb, yb;

		if ((! pm->pm_mod[blk]) && (pm->pm_flags & PF_ENTIRE) == 0 &&
				! unconditional)
			continue;
		xb = (blk % pm->pm_nxb) * pm->pm_xb;
		yb = (blk / pm->pm_nxb) * pm->pm_yb;
		(*dev->gd_pixel) (wstn->ws_tag, xb, yb, pm->pm_xb,
			pm->pm_yb, pm->pm_data + blk*pm->pm_xb*pm->pm_yb,
			0 /* size */, 0 /* org */ );
		pm->pm_mod[blk] = FALSE;
	}
	pm->pm_flags &= ~PF_ENTIRE;
}



void
gp_copy_pmap (source, dest, mflags)
struct pixmap *source, *dest;
int mflags;
/*
 * Copy one pixmap to another.
 * Entry:
 *	SOURCE	is the source pixmap.
 *	DEST	is the destination pixmap
 *	Note that SOURCE and DEST really ought to be for the same type
 *	of display, or ugly things will happen.
 *	MFLAGS	is TRUE iff modified flags are to be diddled.  When this
 *		is requested, mod flags will be cleared in SOURCE, and
 *		set in DEST (for non-zero blocks).
 * Exit:
 *	The copy has been performed.
 */
{
	int blk, npb, nblk, b;
	unsigned char *sp = source->pm_data, *dp = dest->pm_data;
/*
 * Do some checking.
 */
 	if (source->pm_xres != dest->pm_xres ||
	    source->pm_yres != dest->pm_yres || source->pm_xb != dest->pm_xb ||
	    source->pm_yb != dest->pm_yb)
	    	c_panic ("Mismatched pixmaps");
/*
 * Initial info.
 */
 	nblk = source->pm_nxb * (source->pm_yres/source->pm_yb);
	npb = source->pm_xb * source->pm_yb;
/*
 * Now copy one block at a time.
 */
	for (blk = 0; blk < nblk; blk++)
	{
	/*
	 * If we are doing mflags, we have to copy the slow way, so that
	 * we know what we have copied.
	 */
		if (mflags)
		{
			char nz = FALSE;
			for (b = 0; b < npb; b++)
				if (*dp++ = *sp++)
					nz = TRUE;
			source->pm_mod[blk] = FALSE;
			dest->pm_mod[blk] = nz;
		}
	/*
	 * Otherwise we might as well do it the fast way.
	 */
		else
		{
			memcpy (dp, sp, npb);
			dp += npb;
			sp += npb;
		}
	}
}



void
gp_mrg_pmaps (source, dest)
struct pixmap *source, *dest;
/*
 * Merge the source map into the destination.  This is essentially a copy,
 * with the exception that background-color pixels in SOURCE do not
 * overwrite the destination.
 *
 * This routine will deal with modified flags.
 */
{
	unsigned char *sp = source->pm_data, *dp = dest->pm_data;
	int b, npb, nblk, blk;
/*
 * Do some checking.
 */
 	if (source->pm_xres != dest->pm_xres ||
	    source->pm_yres != dest->pm_yres || source->pm_xb != dest->pm_xb ||
	    source->pm_yb != dest->pm_yb)
	    	c_panic ("Mismatched pixmaps");
/*
 * Initial info.
 */
 	nblk = source->pm_nxb * (source->pm_yres/source->pm_yb);
	npb = source->pm_xb * source->pm_yb;
/*
 * Now copy one block at a time.
 */
	for (blk = 0; blk < nblk; blk++)
	{
		register char mod = FALSE;
		for (b = 0; b < npb; b++)
		{
			register char c = *sp++;
			if (c)
			{
				*dp++ = c;
				mod = TRUE;
			}
			else
				dp++;
		}
		dest->pm_mod[blk] |= mod;
	}
}




void
gp_insert (src, size, xs, ys, pm, x0, y0)
char *src;
int size, xs, ys, x0, y0;
struct pixmap *pm;
/*
 * Overwrite a pixel array into a pmap.  It is assumed that this array is
 * appropriately clipped.
 * Entry:
 *	SRC	is the source pixel array.
 *	SIZE	is the data size in this array.
 *	XS	is the X dimension of this array.
 *	YS	is the Y dimension of this array.
 *	PM	is the destination pixmap.
 *	x0, y0	is the origin point for the lower left corner.
 * Exit:
 *	The data has been overwritten into the pmap.
 */
{
	int row, col;
/*
	printf ("GP_insert (%d, %d), %d/%d pm (%d, %d) bl %d %d nxb %d\n",
		x0, y0, xs, ys, pm->pm_xres, pm->pm_yres, pm->pm_xb,
		pm->pm_yb, pm->pm_nxb);
*/
/*
 * This is a somewhat slow scheme, not suitable for large arrays.
 */
 	if (xs*ys < BIG_INSERT)
	 	for (row = 0; row < ys; row++)
			for (col = 0; col < xs; col++)
			{
				register char c = *src++;
				STORE_PXL (pm, x0 + col, y0 + row, c);
			}
	else	/* Large array. */
	{
		int blk, b0, bn, bbeg[50], bsize[50], nblk = 1;
	/*
	 * Figure out block offsets and sizes.
	 */
		b0 = x0/pm->pm_xb;
		bn = (x0 + xs - 1)/pm->pm_xb;
		bbeg[0] = x0;
		bsize[0] = (b0 + 1)*pm->pm_xb - x0;
		for (blk = b0 + 1; blk < bn; blk++)
		{
			bbeg[nblk] = blk*pm->pm_xb;
			bsize[nblk++] = pm->pm_xb;
		}
		bbeg[nblk] = bn*pm->pm_xb;
		bsize[nblk] = (x0 + xs) - bbeg[nblk];
		nblk++;
	/*
	 * Step through each rastor line.
	 */
		for (row = 0; row < ys; row++)
		{
			int ymap = pm->pm_ymap[y0 + row], soff = 0;
			for (blk = 0; blk < nblk; blk++)
			{
				int idx = pm->pm_xmap[bbeg[blk]] + ymap;
				memcpy (pm->pm_data + idx, src + soff,
					bsize[blk]);
				pm->pm_mod[idx/pm->pm_bsize] = TRUE;
				soff += bsize[blk];
			}
			src += xs;
		}
	}
}





char *
gp_carve_window (win, x0, y0, x1, y1, cx0, cy0, cx1, cy1, nx0, ny0, nx1, ny1)
char *win;
int x0, y0, x1, y1, cx0, cy0, cx1, cy1, *nx0, *ny0, *nx1, *ny1;
/*
 * Carve out a chunk of this (byte, rastor) data window.
 * Entry:
 *	WIN	is an array containing the old data.
 *	Xn, Yn	describe the bounds of that window.
 *	CXn, CYn describe the bounds of the clip window to be applied.
 * Exit:
 *	A pointer is returned to the (dynamically-allocated) new window.
 *	NXn, NYn are the coordinates of the new window.
 */
{
	int x, y;
	char *new, *np;
/*
 * Come up with the new coords.
 */
 	*nx0 = (x0 < cx0) ? cx0 : x0;
	*nx1 = (x1 > cx1) ? cx1 : x1;
	*ny0 = (y0 < cy0) ? cy0 : y0;
	*ny1 = (y1 > cy1) ? cy1 : y1;
/*
 * Allocate the new memory, and fill it in.
 */
 	np = new = getvm ((*nx1 - *nx0 + 1)*(*ny1 - *ny0 + 1));
	for (y = *ny0; y <= *ny1; y++)
	{
		char *sp = win + (y - y0)*(x1 - x0 + 1) + *nx0 - x0;
		for (x = *nx0; x <= *nx1; x++)
			*np++ = *sp++;
	}
/*
 * Return the result.
 */
 	return (new);
}



void
gp_clear (pm)
struct pixmap *pm;
/*
 * Clear this pixel map to the zero state.
 */
{
 	memset (pm->pm_data, pm->pm_bg, pm->pm_xres * pm->pm_yres);
	memset (pm->pm_mod, pm->pm_bg, pm->pm_nxb * (pm->pm_yres/pm->pm_yb));
}




void
gp_remove (map)
struct pixmap *map;
/*
 * Cause this pixel map to cease to exist.
 */
{
/*
 * Mostly, we just release a whole bunch of memory.
 */
 	relvm (map->pm_data);
	relvm (map->pm_mod);
	relvm (map->pm_xmap);
	relvm (map->pm_ymap);
	memset (map, 0, sizeof (struct pixmap)); /* catch screwups */
	relvm (map);
}




void
gp_overlay (src, size, xs, ys, pm, x0, y0)
char *src;
int size, xs, ys, x0, y0;
struct pixmap *pm;
/*
 * Overlay a pixel array into a pmap.  It is assumed that this array is
 * appropriately clipped.
 * Entry:
 *	SRC	is the source pixel array.
 *	SIZE	is the data size in this array.
 *	XS	is the X dimension of this array.
 *	YS	is the Y dimension of this array.
 *	PM	is the destination pixmap.
 *	x0, y0	is the origin point for the lower left corner.
 * Exit:
 *	The data has been overlayed into the pmap.
 */
{
	int row, col;
/*
 * This is a somewhat slow scheme, not suitable for large arrays.
 */
 	for (row = 0; row < ys; row++)
		for (col = 0; col < xs; col++)
		{
			register char c = *src++;
			if (c)
				STORE_PXL (pm, x0 + col, y0 + row, c);
		}
}





void
gp_invert (data, xd, yd)
char *data;
int xd, yd;
/*
 * Invert the data in this array, top-to-bottom.
 */
{
	int row, nrow = yd/2, col;
	char *top, *bottom, c;

	for (row = 0; row < nrow; row++)
	{
		top = (yd - row - 1)*xd + data;
		bottom = row*xd + data;
		for (col = 0; col < xd; col++)
		{
			c = *top;
			*top++ = *bottom;
			*bottom++ = c;
		}
	}
}
