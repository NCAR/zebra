/* 6/87 jc */
/*
 * Handle the clipping of graphical objects.
 */
# include "param.h"
# include "workstation.h"
# include "device.h"
# include "graphics.h"
# include "overlay.h"
# include "oplist.h"


/*
 * A lot of the clipping algorithm used here comes from "Principles of
 * Interactive Computer Graphics," by William Newman and Robert Sproull.
 *
 * The first derivation from that work is this macro, which creates a four
 * bit pattern which describes the relationship of a given point to the
 * clip window.  It divides the world up like this:
 *
 *			|        |
 *		  1001	|  1000  |  1010
 *			|        |
 *		--------+--------+--------
 *			|        |
 *		  0001	|  0000  |  0010
 *			|        |
 *		--------+--------+--------
 *			|        |
 *		  0101	|  0100  |  0110
 *			|        |
 *
 * where the center box is the clip window.
 *
 * The nifty features of this encoding are (1) lines that do not need to be
 * clipped have codes of zero for both endpoints, and (2) lines for which the
 * intersection of the two codes is NOT zero are completely outside the 
 * window, and can be ignored.
 */
# define B_LEFT	0x01
# define B_RIGHT 0x02
# define B_BELOW 0x04
# define B_ABOVE 0x08

# define ENCODE(x,y,x0,y0,x1,y1) \
		((x < x0 ? B_LEFT : 0) | (x > x1 ? B_RIGHT : 0) | \
		 (y < y0 ? B_BELOW : 0) | (y > y1 ? B_ABOVE : 0))
			

void
gc_pl_clip (ov, color, ltype, npt, data, dev)
struct overlay *ov;
int color, ltype, npt, *data, dev;
/*
 * Clip an array of polyline data.
 * Entry:
 *	OV	is the overlay into which operations are to be performed.
 *	COLOR	is the color of the line.
 *	LTYPE	is the line type.
 *	NPT	is the number of data points.
 *	DATA	is the (interleaved) array of polyline data.
 *	DEV	is true iff the polyline data are to be sent straight to
 *		the device (as opposed to being stashed away in an oplist).
 * Exit:
 *	The clipping has been performed.
 */
{
	int sx, sy, px0, px1, py0, py1, c1, c2, *begin;
	int *enddata = data + 2*npt - 2;
/*
 * Convert the clip window boundaries to device coords.
 */
	px0 = ov->ov_cx0;	/* Left over for hysterical reasons */
	py0 = ov->ov_cy0;
	px1 = ov->ov_cx1;
	py1 = ov->ov_cy1;
/*
 * Now pass through all the data.
 */
	while (data < enddata)
	{
	/*
	 * Encode the endpoints of the first segment.
	 */
	 	c1 = ENCODE (data[0], data[1], px0, py0, px1, py1);
	 	c2 = ENCODE (data[2], data[3], px0, py0, px1, py1);
	/*
	 * As long as we are completely outside of our clip window, we
	 * do nothing.
	 */
		if ((c1 & c2) != 0)
		{
			data += 2;
			continue;
		}
	/*
	 * We now have a segment that shows itself within our window somewhere.
	 * Fix the first point to be within the window.  Also keep our eyes
	 * out for the occasional "corner case" -- a segment that turns out
	 * to be outside the window completely, despite our earlier tests.
	 */
	 	begin = data;
	 	if (c1)
		{
			if (! gc_clip_segment (c1, data, data + 1, data[2],
				data[3], px0, py0, px1, py1))
			{
				data += 2;
				continue;
			}
		}	
		data += 2;
	/*
	 * We now scan forward for as long as our endpoints are within
	 * the clip window.
	 */
		while (data < enddata && c2 == 0)
		{
			data += 2; npt--;
		 	c2 = ENCODE (data[0], data[1], px0, py0, px1, py1);
		}
	/*
	 * OK, we have come to an end of sorts.  Clip the endpoint if
	 * necessary, after carefully saving the old values.  The save
	 * is necessary to insure that we correctly calculate the next
	 * segment if it reenters the clip window.
	 */
	 	sx = data[0];
		sy = data[1];
		if (c2)
			gc_clip_segment (c2, data, data + 1, data[-2],
				data[-1], px0, py0, px1, py1);
	/*
	 * Flush out the command to the ops list, and restore the old values.
	 */
		if (dev)
			(*ov->ov_ws->ws_dev->gd_polyline) (ov->ov_ws->ws_tag,
				color, ltype, (data - begin)/2 + 1, begin);
		else
			gop_add_op (GOP_POLYLINE, color, &ltype, 1, ov,
				((data + 2) - begin)*sizeof (int), begin,
				FALSE);
		data[0] = sx;
		data[1] = sy;
	}
}





gc_clip_segment (code, x0, y0, x1, y1, cx0, cy0, cx1, cy1)
int code, *x0, *y0, x1, y1, cx0, cy0, cx1, cy1;
/*
 * Clip one end of a line segment.
 * Entry:
 *	CODE	is the code describing the position of (x0, y0)
 *	X0, Y0	is the point to be clipped.
 *	X1, Y1	is the other endpoint of the line.
 *	Can	are the corners of the clip window.
 * Exit:
 *	X0, Y0	have been adjusted to lie within the clip window.
 */
{
	int niter = 0;
/*
printf ("clseg: (%3d, %3d) -> (%3d, %3d), code %x cw (%3d, %3d) to (%3d, %3d)\n", *x0, *y0, x1, y1, code, cx0, cy0, cx1, cy1);
*/
	while (code)
	{
		int xd = x1 - *x0, yd = y1 - *y0;
	/*
	 * If we have been around more than four times, we have a segment
	 * that is not really within the clip window.
	 */
	 	if (++niter > 4)
			return (0);
	/*
	 * Mathematics here is done with quite a bit of care, so as to get
	 * the new endpoints on *exactly* the right pixel.  In particular,
	 * care is taken to avoid integer truncation of negative numbers,
	 * since it doesn't always go the way one wants.
	 */
		if (code & B_LEFT)
		{
			if (xd == 0)
				return (0);	/* kludge */
			if (yd > 0)
				*y0 += (yd*(cx0 - *x0) + xd/2 - 1)/xd;
			else
				*y0 -= ((-yd)*(cx0 - *x0) + xd/2 - 1)/xd;
			*x0 = cx0;
		}
		else if (code & B_RIGHT)
		{
			if (xd == 0)
				return (0);	/* kludge */
			if (yd > 0)
				*y0 += (yd*(*x0 - cx1) - xd/2 - 1)/(-xd);
			else
				*y0 -= ((-yd)*(*x0 - cx1) - xd/2 - 1)/(-xd);
			*x0 = cx1;
		}
		else if (code & B_BELOW)
		{
			if (yd == 0)
				return (0);	/* kludge */
			if (xd > 0)
				*x0 += (xd*(cy0 - *y0) + yd/2 - 1)/yd;
			else
				*x0 -= ((-xd)*(cy0 - *y0) + yd/2 - 1)/yd;
			*y0 = cy0;
		}
		else if (code & B_ABOVE)
		{
			if (yd == 0)
				return (0);	/* kludge */
			if (xd > 0)
				*x0 += (xd*(*y0 - cy1) - yd/2 - 1)/(-yd);
			else
				*x0 -= ((-xd)*(*y0 - cy1) - yd/2 - 1)/(-yd);
			*y0 = cy1;
		}
	 	code = ENCODE (*x0, *y0, cx0, cy0, cx1, cy1);
	}
	return (1);
}
