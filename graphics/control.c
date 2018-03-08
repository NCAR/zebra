/* 5/87 jc */

/*
 * The upper level, control routines for the graphics package.
 */

# include <math.h>
# include "param.h"
# include "graphics.h"
# include "device.h"
# include "overlay.h"
# include "oplist.h"
# include "workstation.h"
# include "pixel.h"

static char *rcsid = "$Id: control.c,v 1.18 2002-07-11 23:13:28 burghart Exp $";
static int Trace = 0;

/*
 * Macro to return b or c, whichever is closer to a
 * (floating point arguments)
 */
# define CLOSER(a,b,c)	((fabs (a-b) < fabs (a-c)) ? b : c)

/*
 * Major kludge: Keep a file ID around for code (radar) which might want to
 * do a select on it...
 */
static int Fdesc = -1;

/*
 * Forwards
 */
void G_easy_update(), G_pmap_update();



int
G_open (device, type, wstn, flags)
char *device, *type;
ws *wstn;
int flags;
/*
 * Open a workstation.
 * Entry:
 *	DEVICE	is the device name of the workstation to be opened.
 *	TYPE	is the type of that device.
 *	FLAGS	is a set of flags controlling the behavior of G_open.
 * Exit:
 *	If the device is successfully opened then
 *		The return value is zero.
 *		The workstation ID is returned in WS.
 *	else
 *		The appropriate error code is returned.
 */
{
	struct workstation *stn = (struct workstation *)
				getvm (sizeof (struct workstation));
	int code;
	struct device *dev;
/*
 * Find the device structure corresponding to the given type.
 */
	if ((dev = gd_get_dev (type)) == (struct device *) NULL)
	{
		relvm (stn);
		return (GE_BAD_TYPE);
	}
/*
 * Make our own copy of the device structure, so the open routine can
 * modify it.
 */
	stn->ws_dev = (struct device *) getvm (sizeof (struct device));
	*stn->ws_dev = *dev;
/*
 * Try to open up the device.
 */
 	if (code = (*stn->ws_dev->gd_open) (device, type, &stn->ws_tag, 
		stn->ws_dev))
	{
		relvm (stn->ws_dev);
		relvm (stn);
		return (code);
	}
/*
 * Now we are in business.  Order up a full device clear, then return the
 * workstation ID.
 */
	if ((flags & GF_NOCLEAR) == 0)
	 	(*stn->ws_dev->gd_clear) (stn->ws_tag);
	stn->ws_overlay = 0;
	stn->ws_pmap = (struct pixmap *) 0;
	gc_init (stn, stn->ws_dev->gd_ncolor);
 	*wstn = (ws) stn;
	return (0);
}




void
G_update (csta)
ws csta;
/*
 * Perform an update on this station.
 */
{
	struct workstation *wstn = (struct workstation *) csta;
	struct overlay *ov;
	int mod = 0, npmap = 0, lastadd = 0, pmbot = 0, cleared = 0;
/*
 * Pass through the overlay list, and scope out just what sort of job
 * we have to deal with here.
 */
 	for (ov = wstn->ws_overlay; ov; ov = ov->ov_next)
	{
		if (ov->ov_flags & OVF_MODIFIED)
			mod++;
		if (ov->ov_flags & OVF_EMPTY)
			continue;
		if (ov->ov_flags & OVF_PIXMAP)
			npmap++;
		if ((ov->ov_flags & OVF_ADDITIVE) &&
			  (ov->ov_flags & OVF_MODIFIED) && ov->ov_next == NULL)
			lastadd = TRUE;
	}
	if (mod == 1 && npmap == 1 && wstn->ws_overlay->ov_flags & OVF_MODIFIED
			&& wstn->ws_overlay->ov_flags & OVF_PIXMAP)
		pmbot = TRUE;
	if (Trace)
		printf ("UPDATE, mod %d, pm %d, lastadd %d pmbot %d\n", mod,
				npmap, lastadd, pmbot);
/*
 * If nothing has been changed, we quit now.  However, we still force a
 * flush of the output buffer, in case anything is hanging around waiting
 * to be sent out to the device.
 */
 	if (! mod)
	{
	 	(*wstn->ws_dev->gd_flush) (wstn->ws_tag);
		return;
	}
/*
 * If we do not have a device which supports segmentation, we are going to
 * have to redraw the whole thing.  That means we should clear the entire
 * screen now.
 *
 * Modified to handle additive updates.  The reasoning is this: if only
 * "additive" changes have been made to the highest priority overlay, we
 * can simply draw those changes without redrawing everything else.
 */
	if ((wstn->ws_dev->gd_flags & GDF_SEGMENT) == 0 &&
			(mod != 1 || ! lastadd) && ! pmbot)
	{
		(*wstn->ws_dev->gd_clear) (wstn->ws_tag);
		cleared++;
	}
/*
 * If we have a vector-capable device, and one of the following is true:
 *	- There are no pixmap overlays, or
 *	- There is exactly one pixmap overlay, and it is the lowest
 *	  priority overlay, ("FOF radar case")
 * then we can perform a simple oplist-driven update.
 */
 	if ((wstn->ws_dev->gd_flags & GDF_VECTOR) && (npmap == 0 ||
		(npmap == 1 && wstn->ws_overlay->ov_flags & OVF_PIXMAP)))
		G_easy_update (wstn, (mod == 1) && lastadd, cleared);
	else if (wstn->ws_overlay->ov_next == 0)
		G_easy_update (wstn, (mod == 1) && lastadd, cleared);
/*
 * Otherwise, life is a little more difficult.
 */
 	else
		G_pmap_update (wstn);
/*
 * Force a device update.  For segmented devices where we only changed
 * the highest priority segment, we don't need a screen renewal
 *
 * 8/3/88 jc	Hardcopy devices use the CLEAR function to force a
 *		page out.  Do that now.  For the moment, this patch rules
 *		out hardcopy devices with segments, but I don't think this
 *		is likely to be a problem.
 */
# ifdef notdef
	if (wstn->ws_dev->gd_flags & GDF_HARDCOPY)
		(*wstn->ws_dev->gd_clear) (wstn->ws_tag);
# endif
	if ((wstn->ws_dev->gd_flags & GDF_SEGMENT) &&
			(mod == 1) && lastadd)
	 	(*wstn->ws_dev->gd_flush_nr) (wstn->ws_tag);
	else
		(*wstn->ws_dev->gd_flush) (wstn->ws_tag);
}



void
G_easy_update (wstn, add, cleared)
struct workstation *wstn;
int add, cleared;
/*
 * Perform an easy, oplist-driven update.
 */
{
	struct overlay *ov;
/*
 * Now we pass through the list of overlays, forcing out those which need
 * to be updated.
 */
	if (Trace)
		printf ("Easy update...\n");
 	for (ov = wstn->ws_overlay; ov; ov = ov->ov_next)
	{
	/*
	 * If this is a segmented device, we only need to update this one if
	 * it has been changed.
	 */
		if (wstn->ws_dev->gd_flags & GDF_SEGMENT)
		{
			if ((ov->ov_flags & OVF_MODIFIED) == 0)
				continue;
			if (ov->ov_flags & OVF_MODIFIED)
			{
				(*wstn->ws_dev->gd_s_select) (wstn->ws_tag,
					ov->ov_number, ov->ov_priority);
				if (ov->ov_flags & OVF_PIXMAP)
					gp_update (wstn, ov, cleared);
				else
					gop_update (wstn, ov,
						ov->ov_flags & OVF_ADDITIVE);
				(*wstn->ws_dev->gd_s_end) (wstn->ws_tag,
						ov->ov_number);
			}
		}
	/*
	 * Every overlay needs to be redrawn for a non-segmented device.
	 * Eventual plans call for a little more intelligence here by 
	 * respecting the ADDITIVE flag, and only redrawing some segments.
	 */
	 	else
		{
			if ((ov->ov_flags & OVF_VISIBLE) &&
				((ov->ov_flags & OVF_EMPTY) == 0) &&
				(ov->ov_flags & OVF_MODIFIED || ! add))
			{
				if (ov->ov_flags & OVF_PIXMAP)
					gp_update (wstn, ov->ov_pmap, cleared);
				else
					gop_update (wstn, ov, add);
			}
		}
		ov->ov_flags &= ~OVF_MODIFIED;
		if ((wstn->ws_dev->gd_flags & GDF_HARDCOPY) == 0)
			ov->ov_flags |= OVF_ADDITIVE;
	}
}




void
G_pmap_update (wstn)
struct workstation *wstn;
/*
 * Perform a full, gory, pixmap update of this workstation.
 */
{
	struct overlay *ov, *highest = 0;

	if (Trace)
		printf ("pmap update...\n");
/*
 * Make sure that this workstation has a master pixmap.
 */
 	if (! wstn->ws_pmap)
		wstn->ws_pmap = gp_make_pmap (wstn->ws_dev);
/*
 * If this is a vector-capable device, we will perform any oplists with
 * priority greater than that of the highest pixmap as oplists.  Let's find
 * the highest pixmap now, then convert any oplists below that level to
 * pixmaps.
 *
 * (For non-vector devices, there should be *no* oplist overlays)
 */
 	if (wstn->ws_dev->gd_flags & GDF_VECTOR)
	{
		for (ov = wstn->ws_overlay; ov; ov = ov->ov_next)
			if (ov->ov_flags & OVF_PIXMAP)
				highest = ov;
		if (! highest)
			c_panic ("pmap_update with no pmaps", 0);
		for (ov = wstn->ws_overlay; ov != highest; ov = ov->ov_next)
			if ((ov->ov_flags & OVF_PIXMAP) == 0)
				gop_cvt_pmap (wstn, ov);
	}
/*
 * Make absolutely sure that there are no oplist overlays.  Also get
 * our pointer to the highest priority overlay, just to make life easy.
 */
	else
		for (ov = wstn->ws_overlay; ov; ov = ov->ov_next)
		{
			if ((ov->ov_flags & OVF_PIXMAP) == 0)
				c_panic ("Oplist overlay w/non-vector dev");
			highest = ov;
		}
/*
 * Go through the overlay list, and merge each one into the master map.
 * Note that the first overlay is not tested against highest.  For the
 * moment, I can get away with that, since G_easy_update will have been
 * called for that particular case.
 */
	ov = wstn->ws_overlay;
	gp_copy_pmap (ov->ov_pmap, wstn->ws_pmap, TRUE); /* Initialize map */
	ov->ov_flags &= ~OVF_MODIFIED;
	for (ov = ov->ov_next; (ov && ov != highest); ov = ov->ov_next)
	{
		if ((ov->ov_flags & OVF_VISIBLE) == 0)
			continue;
		gp_mrg_pmaps (ov->ov_pmap, wstn->ws_pmap);
		ov->ov_flags &= ~OVF_MODIFIED;
	}
/*
 * Let's not forget to do "highest", which will have caused us to drop out
 * of the loop.
 */
	if (ov)	/* (should always be the case) */
	{
	 	gp_mrg_pmaps (ov->ov_pmap, wstn->ws_pmap);
		ov->ov_flags &= ~OVF_MODIFIED;
		ov = ov->ov_next;
	}
/*
 * Now the master pmap can be forced out.
 */
 	gp_update (wstn, wstn->ws_pmap, FALSE);
/*
 * Finally, if there are any oplist overlays left, we should update
 * them now.
 */
 	for (; ov; ov = ov->ov_next)
	{
	/*
	 * If this is a segmented device, write into the appropriate segment.
	 */
		if (wstn->ws_dev->gd_flags & GDF_SEGMENT)
		{
			(*wstn->ws_dev->gd_s_select) (wstn->ws_tag,
				ov->ov_number, ov->ov_priority);
			gop_update (wstn, ov, ov->ov_flags & OVF_ADDITIVE);
			(*wstn->ws_dev->gd_s_end) (wstn->ws_tag,
					ov->ov_number);
		}
	/*
	 * Every overlay needs to be redrawn for a non-segmented device.
	 * Eventual plans call for a little more intelligence here by 
	 * respecting the ADDITIVE flag, and only redrawing some segments.
	 */
	 	else
		{
			if (ov->ov_flags & OVF_VISIBLE)
				gop_update (wstn, ov, 
                                    ov->ov_flags & OVF_ADDITIVE);
		}
		ov->ov_flags &= ~OVF_MODIFIED;
	}
}






overlay
G_new_overlay (cstn, priority)
char *cstn;
int priority;
/*
 * Create a new overlay.
 * Entry:
 *	CSTN	is a workstation ID returned from G_open().
 *	PRIORITY is the priority to assign to the new overlay.
 * Exit:
 *	The return value is the overlay ID for the newly created overlay.
 *	This overlay will be blank, and have coordinates that go from
 *	0.0 to 1.0 in both directions.
 */
{
	struct overlay *ov, *gov_new_overlay (), *prev, *check;
	struct workstation *wstn = (struct workstation *) cstn;
/*
 * Create a raw new overlay.
 */
 	ov = (struct overlay *) gov_new_overlay (priority);
/*
 * Now hook it into the list.
 */
 	if (wstn->ws_overlay == NULL ||
		wstn->ws_overlay->ov_priority > priority)
	{
		ov->ov_next = wstn->ws_overlay;
		wstn->ws_overlay = ov;
	}
	else
	{
		prev = check = wstn->ws_overlay;
		while (check && check->ov_priority <= priority)
		{
			prev = check;
			check = check->ov_next;
		}
		ov->ov_next = prev->ov_next;
		prev->ov_next = ov;
	}
/*
 * Put in the backpointer to the device.
 */
 	ov->ov_ws = wstn;
/*
 * If this is a segmented device, we also need to create a new hardware
 * segment.
 */
 	if (wstn->ws_dev->gd_flags & GDF_SEGMENT)
		ov->ov_number = (*wstn->ws_dev->gd_s_init) (wstn->ws_tag);
/*
 * Decide what data format we will use for this overlay.  For now, if the
 * device will do vector operations, we will be optimistic and start with
 * an oplist organization.  Otherwise we need to allocate a pixel map.
 */
	if ((wstn->ws_dev->gd_flags & GDF_VECTOR) == 0)
	{
		ov->ov_flags |= OVF_PIXMAP;
		ov->ov_pmap = gp_make_pmap (wstn->ws_dev);
	}
/*
 * Initialize the clip window to the whole display.
 */
	G_set_coords ((overlay) ov, 0.0, 0.0, 1.0, 1.0);
/*
 * Finally, pass back the overlay pointer.
 */
	return ((overlay) ov);
}



ws
G_ov_to_ws (cov)
overlay cov;
/*
 * Return the tag of the workstation associated with this overlay.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	
	return ((ws) ov->ov_ws);
}



int
G_set_coords (cov, x0, y0, x1, y1)
overlay cov;
float x0, y0, x1, y1;
/*
 * Reset the world coordinates for this overlay.
 * Entry:
 *	COV	is an overlay ID as returned from G_new_overlay ();
 *	Xn, Yn	are the new coordinates.
 * Exit:
 *	The new coordinates for the overlay have been established.  If there
 *	is a clip window for this overlay, it has been reset, and should be
 *	re-established if needed.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;

	if (x0 == x1 || y0 == y1)
		return (GE_BAD_COORDS);

	ov->ov_x0 = x0;
	ov->ov_y0 = y0;
	ov->ov_x1 = x1;
	ov->ov_y1 = y1;
	ov->ov_cx0 = ov->ov_cy0 = 0;
	ov->ov_cx1 = dev->gd_xres - 1;
	ov->ov_cy1 = dev->gd_yres - 1;
/*
 * For devices with hardware clip windows and oplist storage, we throw
 * in a SETHCW op now so that the clip window is always right.
 */
 	if ((dev->gd_flags & GDF_HCW) && !(ov->ov_flags & OVF_PIXMAP))
	{
		int	coords[4];

		coords[GOP_W_X0] = 0;
		coords[GOP_W_Y0] = 0;
		coords[GOP_W_X1] = dev->gd_xres - 1;
		coords[GOP_W_Y1] = dev->gd_yres - 1;
		gop_add_op (GOP_SETHCW, 0, coords, 4, ov, 0, 
			(float *) 0, FALSE);
	}

	return (GE_OK);
}



int
G_polyline (cov, ltype, color, npt, x, y)
char *cov;
int ltype, color, npt;
float *x, *y;
/*
 * Draw a polyline in the given overlay.
 * Entry:
 *	COV	is an overlay ID obtained from G_new_overlay ().
 *	LTYPE	is the line type to use, as defined in graphics.h
 *	COLOR	is the color to use.
 *	NPT	is the number of points to connect.
 *	X, Y	are the coordinate arrays.
 * Exit:
 *	The line has been drawn into the overlay.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	int *ndata, *ndp;
	int pt;
/*
 * Obtain storage, then convert the point data into normalized coords.
 */
 	ndp = ndata = (int *) getvm (2 * npt * sizeof (int));
	for (pt = 0; pt < npt; pt++)
	{
		*ndp++ = W_TO_DC (*x++, ov->ov_x0, ov->ov_x1,
				ov->ov_ws->ws_dev->gd_xres);
		*ndp++ = W_TO_DC (*y++, ov->ov_y0, ov->ov_y1,
				ov->ov_ws->ws_dev->gd_yres);
	}
/*
 * If we are dealing with a pixmap overlay, encode the line straight
 * into the map.
 */
	if (ov->ov_flags & OVF_PIXMAP)
		gp_pl (ov, color, ltype, npt, ndata);
/*
 * If we have a device that does its own clipping, we can just store up
 * one big pl operation.
 */
 	else if (ov->ov_ws->ws_dev->gd_flags & GDF_HCW)
	 	gop_add_op (GOP_POLYLINE, color, &ltype, 1,
			ov, 2*npt*sizeof (int), ndata, TRUE);
/*
 * Otherwise we have to clip the data first.  gc_pl_clip will put out the
 * individual segments itself, so our memory will be superfluous, and is
 * released after the clipping.
 */
 	else
	{
		gc_pl_clip (ov, color, ltype, npt, ndata, FALSE);
		relvm (ndata);
	}
	ov->ov_flags |= OVF_MODIFIED;
	ov->ov_flags &= ~OVF_EMPTY;
	return (GE_OK);
}



int
G_text (cov, color, font, height, hjust, vjust, x, y, text)
overlay cov;
int color, font, hjust, vjust;
float height, x, y;
char *text;
/*
 * Do a G_write with a rotation of zero
 */
{
	return (G_write (cov, color, font, height, hjust, vjust, x, y, 
			 0.0, text));
}



int
G_write (cov, color, font, height, hjust, vjust, x, y, rot, text)
overlay cov;
int color, font, hjust, vjust;
float height, x, y, rot;
char *text;
/*
 * Write some text into an overlay.
 * Entry:
 *	COV	is an overlay ID.
 *	COLOR	is the color to use.
 *	FONT	is the ID of the font to use.
 *	HEIGHT	Is the height of the text, in world coords.
 *	HJUST	is the horizontal justification, being LEFT, RIGHT, or CENTER.
 *	VJUST	is the vertical justification, being TOP, BOTTOM, or CENTER.
 *	X, Y	is where to position the text.
 *	ROT	is the rotation in degrees counterclockwise from horizontal
 *	TEXT	is the actual text to write.
 * Exit:
 *	The text has been written into the overlay.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;
	int dx, dy, extra[5], pixheight, ex, ey, ftype = gt_font_type (font);
	float scale, aspect;
/*
 * Make sure our device can handle this operation.
 */
 	if (ftype == GFT_PIXEL &&
		(ov->ov_ws->ws_dev->gd_flags & GDF_PIXEL) == 0)
		return (GE_DEVICE_UNABLE);

	if (ftype == GFT_PIXEL && rot != 0.0)
		return (GE_DEVICE_UNABLE);
/*
 * Come up with the scale factor to be applied to the font.
 */
 	pixheight = W_TO_DC (height, 0.0, ov->ov_y1 - ov->ov_y0,
		ov->ov_ws->ws_dev->gd_yres);
	scale = ((float) pixheight) / ((float) gt_f_height (font));
/*
 * If they want device text, and we can't do it for whatever reason, switch
 * them to stroke.
 */
	if (ftype == GFT_DEV &&
		((ov->ov_flags & OVF_PIXMAP) || ! (dev->gd_flags & GDF_TEXT) ||
		! (*dev->gd_qtext) (ov->ov_ws->ws_tag, pixheight, rot)))
	{
		font = (pixheight > 30) ? GTF_STROKE : GTF_MINSTROKE;
		ftype = GFT_STROKE;
		scale = ((float) pixheight) / ((float) gt_f_height (font));
	}
/*
 * Figure out the start position.
 */
 	dx = W_TO_DC (x, ov->ov_x0, ov->ov_x1, ov->ov_ws->ws_dev->gd_xres);
 	dy = W_TO_DC (y, ov->ov_y0, ov->ov_y1, ov->ov_ws->ws_dev->gd_yres);
	aspect = ov->ov_ws->ws_dev->gd_aspect;
	gt_get_start (dx, dy, font, scale, hjust, vjust, rot, aspect, text,
		extra + GOP_T_X, extra + GOP_T_Y, &ex, &ey, ov->ov_ws->ws_dev,
		ov->ov_ws->ws_tag);
/*
 * If this is a pixmap overlay, we simply write it into the map now.
 */
 	if (ov->ov_flags & OVF_PIXMAP)
		gt_do_text (ov->ov_ws, ov, color, extra[GOP_T_X], 
			extra[GOP_T_Y], font, (int) (100 * scale), rot, 
			text, FALSE);
/*
 * We try to avoid software clipping at all cost, since a text string of
 * moderate size can easily turn into hundreds of polyline operations.  So,
 * we check the enclosing box against the clip window, and defer the operation
 * if possible.
 *
 * [Check this clip test again -- should I not also check the origin points?]
 */
	else if (ftype == GFT_PIXEL||(ov->ov_ws->ws_dev->gd_flags & GDF_HCW) ||
		 ftype == GFT_DEV || 
		(ex >= ov->ov_cx0 && ex <= ov->ov_cx1 && ey >= ov->ov_cy0 &&
		 ey <= ov->ov_cy1))
	{
	 	extra[GOP_T_FONT] = font;
		extra[GOP_T_SCALE] = (int) (100 * scale);
		extra[GOP_T_ROT] = (int) (100 * rot);
		gop_add_op (GOP_TEXT, color, extra, 5, ov, strlen (text) + 1,
			text, FALSE);
	}
	else
		gt_do_text (ov->ov_ws, ov, color, extra[GOP_T_X], 
			extra[GOP_T_Y], font, (int) (100 * scale), rot, 
			text, FALSE);
	ov->ov_flags |= OVF_MODIFIED;
	ov->ov_flags &= ~OVF_EMPTY;
	return (GE_OK);
}


int
G_w_inquire (csta, item, value)
ws csta;
int item;
int *value;
/*
 * Inquire into some attribute of this workstation.
 * Entry:
 *	CSTA	is a workstation ID.
 *	ITEM	is the code for the item of interest.
 * Exit:
 *	VALUE	will contain the appropriate value.
 */
{
	struct workstation *wp = (struct workstation *) csta;
	
	switch (item)
	{
	   case GIW_NCOLOR:
	   	*value = wp->ws_dev->gd_ncolor;
		return (GE_OK);

	   case GIW_XRES:
	   	*value = wp->ws_dev->gd_xres;
		return (GE_OK);

	   case GIW_YRES:
	   	*value = wp->ws_dev->gd_yres;
		return (GE_OK);

	   case GIW_PIXEL:
	   	*value = (wp->ws_dev->gd_flags & GDF_PIXEL) != 0;
		return (GE_OK);

	   case GIW_PREC:
	   	*value = ((wp->ws_dev->gd_flags & GDF_PIXEL) != 0) &&
			  ((wp->ws_dev->gd_flags & GDF_AVOID_PIX) == 0);
		return (GE_OK);

	   case GIW_VECTOR:
	   	*value = (wp->ws_dev->gd_flags & GDF_VECTOR) != 0;
		return (GE_OK);

	   case GIW_SEGMENT:
	   	*value = (wp->ws_dev->gd_flags & GDF_SEGMENT) != 0;
		return (GE_OK);

	   case GIW_HARDCOPY:
	   	*value = wp->ws_dev->gd_flags & GDF_HARDCOPY;
		return (GE_OK);

	   case GIW_NBUTTON:
	   	*value = wp->ws_dev->gd_nbutton;
		return (GE_OK);

	   default:
	   	return (GE_BAD_ITEM);
	}
}



int
G_set_color_map (csta, base, ncolor, red, green, blue)
ws csta;
int base, ncolor;
float *red, *green, *blue;
/*
 * Set the color map for this device.
 * Entry:
 *	CSTA	is the ID of the workstation to be changed.
 *	BASE	is the first color number to change.
 *	NCOLOR	is the number of color map entries to change.
 *	RED	is the array of red gun values, in the range 0 <= r < 1
 *	GREEN	is the array of green values.
 *	BLUE	is the array of blue values.
 * Exit:
 *	The color map has been set.
 */
{
	struct workstation *wp = (struct workstation *) csta;
/*
 * Sanity check.
 */
 	if ((base + ncolor) > wp->ws_dev->gd_ncolor || ncolor < 1)
		return (GE_NCOLOR);
/*
 * Now, dump the color map into the device.
 */
 	return ((*wp->ws_dev->gd_set_color) (wp->ws_tag, base, ncolor, red,
			green, blue));
}




int
G_visible (cov, state)
overlay cov;
int state;
/*
 * Set the visibility state for this overlay.
 * Entry:
 *	COV	is an overlay ID
 *	STATE	is TRUE iff this overlay is to be visible.
 * Exit:
 *	The visibility state has been set to the desired value.
 *	The old state is the return value of the function.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct workstation *wstn = ov->ov_ws;
	int oldvis = ov->ov_flags & OVF_VISIBLE;
/*
 * Tweak the internal state flag.
 */
	if (state)
		ov->ov_flags |= OVF_VISIBLE;
	else
		ov->ov_flags &= ~OVF_VISIBLE;
/*
 * If this is a segmented device, we need to tweak the hardware attribute.
 * Someday we may need more smarts, to deal with segmented devices that don't
 * handle visible/invisible.  Until then, this works.
 */
 	if (wstn->ws_dev->gd_flags & GDF_SEGMENT)
		(*wstn->ws_dev->gd_s_attr) (wstn->ws_tag, ov->ov_number,state);
/*
 * Otherwise mark the overlay as being modified, so that it will be cleared
 * out in the next update.
 */
	else
	{
		ov->ov_flags |= OVF_MODIFIED;
		ov->ov_flags &= ~OVF_ADDITIVE;
	}
	return (oldvis);
}




int
G_clear (cov)
char *cov;
/*
 * Clear this overlay.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct workstation *wstn = ov->ov_ws;
	struct device *dev = ov->ov_ws->ws_dev;
	int coords[4];
/*
 * If this overlay is already empty, don't worry about it.
 */
 	if (ov->ov_flags & OVF_EMPTY)
		return (GE_OK);
/*
 * Clean out the oplist for this overlay.
 */
	if (ov->ov_flags & OVF_PIXMAP)
		gp_clear (ov->ov_pmap);
	else
	{
	 	gop_clear (ov);
	/*
	 * If we were using hardware clipping, put it back in the oplist
	 */
	 	if (dev->gd_flags & GDF_HCW)
		{
			coords[GOP_W_X0] = ov->ov_cx0;
			coords[GOP_W_Y0] = ov->ov_cy0;
			coords[GOP_W_X1] = ov->ov_cx1;
			coords[GOP_W_Y1] = ov->ov_cy1;
			gop_add_op (GOP_SETHCW, 0, coords, 4, cov, 0, 
				(float *) 0, FALSE);
		}
	}
/*
 * Clear the hardware segment (if any)
 */
 	if (dev->gd_flags & GDF_SEGMENT)
		(*dev->gd_s_clear) (wstn->ws_tag, ov->ov_number);
/*
 * Now mark the overlay as modified, and non-additively at that.
 */
 	ov->ov_flags |= OVF_MODIFIED | OVF_EMPTY;
	ov->ov_flags &= ~OVF_ADDITIVE;
	return (GE_OK);
}



int
G_ws_clear (cws)
ws cws;
/*
 * Clear the entire workstation.
 */
{
	struct workstation *wsta = (struct workstation *) cws;
	struct overlay *ov;
/*
 * Just individually clear each overlay.
 */
 	for (ov = wsta->ws_overlay; ov; ov = ov->ov_next)
		G_clear ((char *) ov);
	return (GE_OK);
}



int
G_close (csta)
char *csta;
/*
 * Close out this workstation.
 */
{
	struct workstation *wsta = (struct workstation *) csta;
/*
 * Call the device close routine.
 */
	(*wsta->ws_dev->gd_close) (wsta->ws_tag);
/*
 * Free up all the memory.
 */
 	while (wsta->ws_overlay)
		G_zap_overlay ((overlay)(wsta->ws_overlay));
	if (wsta->ws_pmap)
		gp_remove (wsta->ws_pmap);
	gc_close (wsta);
	relvm (wsta->ws_dev);
	relvm (wsta);
/*
 * All done.
 */
 	return (GE_OK);
}


int
G_clip_window (cov, x0, y0, x1, y1)
overlay cov;
float x0, y0, x1, y1;
/*
 * Set the clip window for this overlay.
 * Entry:
 *	COV	is the overlay to be modified.
 *	Xn, Yn	are the coordinates of the clip window corners.
 * Exit:
 *	The new clip window is in effect.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	int coords[4];
	int	x0_outside, x1_outside, y0_outside, y1_outside;
	struct device *dev = ov->ov_ws->ws_dev;
/*
 * Do some basic sanity checks.  While a clip window need not lie entirely
 * within the overlay, there should be some overlap, or this overlay will
 * become simply disabled.
 */
	x0_outside = ((x0 - ov->ov_x0) * (x0 - ov->ov_x1)) > 0.0;
	x1_outside = ((x1 - ov->ov_x0) * (x1 - ov->ov_x1)) > 0.0;
	y0_outside = ((y0 - ov->ov_y0) * (y0 - ov->ov_y1)) > 0.0;
	y1_outside = ((y1 - ov->ov_y0) * (y1 - ov->ov_y1)) > 0.0;

	if ((x0_outside || y0_outside) && (x1_outside || y1_outside))
		return (GE_BAD_COORDS);
/*
 * Make sure that the clip window lies within the overlay.  In other words,
 * clip the clip window.
 */
	if (x0_outside) x0 = CLOSER (x0, ov->ov_x0, ov->ov_x1);
	if (y0_outside) y0 = CLOSER (y0, ov->ov_y0, ov->ov_y1);
	if (x1_outside) x1 = CLOSER (x1, ov->ov_x0, ov->ov_x1);
	if (y1_outside) y1 = CLOSER (y1, ov->ov_y0, ov->ov_y1);
/*
 * Now store away the clip window in pixel coords.
 */
	ov->ov_cx0 = W_TO_DC (x0, ov->ov_x0, ov->ov_x1, dev->gd_xres);
	ov->ov_cy0 = W_TO_DC (y0, ov->ov_y0, ov->ov_y1, dev->gd_yres);
	ov->ov_cx1 = W_TO_DC (x1, ov->ov_x0, ov->ov_x1, dev->gd_xres);
	ov->ov_cy1 = W_TO_DC (y1, ov->ov_y0, ov->ov_y1, dev->gd_yres);
/*
 * Unless we have a hardware clip window, we are done.
 */
 	if ((dev->gd_flags & GDF_HCW) == 0 || (ov->ov_flags & OVF_PIXMAP) != 0)
		return (GE_OK);
/*
 * Store up our coords, and stash the operation away into the current
 * oplist.
 */
	coords[GOP_W_X0] = ov->ov_cx0;
	coords[GOP_W_Y0] = ov->ov_cy0;
	coords[GOP_W_X1] = ov->ov_cx1;
	coords[GOP_W_Y1] = ov->ov_cy1;
	gop_add_op (GOP_SETHCW, 0, coords, 4, cov, 0, (float *) 0, FALSE);
	return (GE_OK);
}



int
G_tex_font (font)
char *font;
/*
 * Load a TeX pixel font.  The return value is the font descriptor.
 */
{
	return (gt_load_font (font));
}


int
G_pixel_fill (cov, data, x, y, xs, ys, size, mode)
overlay cov;
char *data;
int x, y, xs, ys, size, mode;
/*
 * Perform a pixel fill operation on this overlay.
 * Entry:
 *	COV	is the overlay to use.
 *	DATA	is the actual pixel data.
 *	X, Y	is the lower, left corner to fill.
 *	XS, YS	is the size of the pixel array.
 *	SIZE	is the data-element size for the incoming array.
 *	MODE	is the fill mode.
 * Exit:
 *	The operation has been performed.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	int new = FALSE;
/*
 * If this device can't do pixel operations, we certainly can't do this.
 */
 	if ((ov->ov_ws->ws_dev->gd_flags & GDF_PIXEL) == 0)
		return (GE_DEVICE_UNABLE);
/*
 * If we are completely outside of the clip window, quit now and save 
 * a lot of work.
 */
 	if (x > ov->ov_cx1 || (x + xs) < ov->ov_cx0 || y > ov->ov_cy1 ||
			(y + ys) < ov->ov_cy0)
		return (GE_OK);
/*
 * If the overlay is not a pixel-map overlay, we will take steps to
 * change that.
 */
 	if ((ov->ov_flags & OVF_PIXMAP) == 0)
	{
		gop_cvt_pmap (ov->ov_ws, ov);
		ov->ov_flags &= ~OVF_ADDITIVE;
	}
/*
 * If necessary, clip the array first.
 */
 	if (x < ov->ov_cx0 || (x + xs - 1) > ov->ov_cx1 || y < ov->ov_cy0 ||
			(y + ys - 1) > ov->ov_cy1)
	{
		int x1, y1;
		new = TRUE;
		data = gp_carve_window (data, x, y, x + xs - 1, y + ys - 1,
			ov->ov_cx0, ov->ov_cy0, ov->ov_cx1, ov->ov_cy1,
			&x, &y, &x1, &y1);
		xs = x1 - x + 1;
		ys = y1 - y + 1;
	}
/*
 * Now perform the operation.
 */
	switch (mode)
	{
	   case GPM_OVERWRITE:
	   	gp_insert (data, size, xs, ys, ov->ov_pmap, x, y);
		break;

	   case GPM_OVERLAY:
	   	gp_overlay (data, size, xs, ys, ov->ov_pmap, x, y);
		break;

	   default:
		return (GE_UNK_FILL_MODE);
	}
/*
 * Finally, if we clipped the data, we have a dynamic array to get rid of.
 */
	ov->ov_flags |= OVF_MODIFIED;
	ov->ov_flags &= ~OVF_EMPTY;
 	if (new)
		relvm (data);
	return (GE_OK);
}




int
G_wc_to_px (cov, wx, wy, px, py)
overlay cov;
float wx, wy;
int *px, *py;
/*
 * Convert a set of world coordinates to pixel coordinates.
 * Entry:
 *	COV	is the overlay to be used.
 *	WX, WY	is the point of interest in world coordinates.
 * Exit:
 *	If the given point in on-screen then:
 *		The return value is GE_OK
 *		The associated pixel coordinates are to be found in PX, PY
 *	else
 *		The return value is GE_OFFSCREEN
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;
	int	outside_wx, outside_wy;

	outside_wx = ((wx - ov->ov_x0) * (wx - ov->ov_x1)) > 0.0;
	outside_wy = ((wy - ov->ov_y0) * (wx - ov->ov_y1)) > 0.0;

	if (outside_wx || outside_wy)
		return (GE_OFFSCREEN);
	*px = W_TO_DC (wx, ov->ov_x0, ov->ov_x1, dev->gd_xres);
	*py = W_TO_DC (wy, ov->ov_y0, ov->ov_y1, dev->gd_yres);
	return (GE_OK);
}




int
G_target (cov, x, y)
overlay cov;
float *x, *y;
/*
 * Read the workstation target.
 * Entry:
 *	COV	is the overlay of interest.
 * Exit:
 *	X, Y	are the target location, in the world coordinates of
 *		the given overlay.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;
	int px, py;

	(*dev->gd_target) (ov->ov_ws->ws_tag, &px, &py);
	*x = (((float) px) / ((float) dev->gd_xres)) * (ov->ov_x1 - ov->ov_x0)
			- ov->ov_x0;
	*y = (((float) py) / ((float) dev->gd_yres)) * (ov->ov_y1 - ov->ov_y0)
			- ov->ov_y0;
	return (GE_OK);
}




int
G_pick (cov, button, x, y)
overlay cov;
int *button;
float *x, *y;
/*
 * Perform a pick on the screen.
 * Entry:
 *	COV	is an overlay.
 * Exit:
 *	BUTTON	is the number of the button pressed by the user.
 *	X, Y	is the target location, in the overlay's coordinates.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;
	int px, py;
/*
 * Make sure we have a device which can do this.
 */
	if (dev->gd_nbutton <= 0 || ! dev->gd_pick)
		return (GE_DEVICE_UNABLE);
/*
 * Go for it.
 */
	(*dev->gd_pick) (ov->ov_ws->ws_tag, button, &px, &py);
	*x = (((float) px) / ((float) dev->gd_xres)) * (ov->ov_x1 - ov->ov_x0)
			- ov->ov_x0;
	*y = (((float) py) / ((float) dev->gd_yres)) * (ov->ov_y1 - ov->ov_y0)
			- ov->ov_y0;
	return (GE_OK);
}







int
G_put_target (cov, x, y)
overlay	cov;
float	x, y;
/*
 * Put the workstation target somewhere (an overlay is passed in instead
 * of just a workstation, since we need to know what coordinate system
 * to use)
 *
 * Entry:
 *	COV	is the overlay of interest.
 * Exit:
 *	X, Y	are the target location, in the world coordinates of
 *		the given overlay.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;
	int px, py;

	if (! dev->gd_put_target)
		return (GE_DEVICE_UNABLE);

	px = W_TO_DC (x, ov->ov_x0, ov->ov_x1, dev->gd_xres);
	py = W_TO_DC (y, ov->ov_y0, ov->ov_y1, dev->gd_yres);

	(*dev->gd_put_target) (ov->ov_ws->ws_tag, px, py);

	return (GE_OK);
}




int
G_untarget (cov)
overlay	cov;
/*
 * Undisplay the target on the chosen device
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;

	if (! dev->gd_untarget)
		return (GE_DEVICE_UNABLE);

	(*dev->gd_untarget) (ov->ov_ws->ws_tag);
	return (GE_OK);
}



int
G_zap_overlay (cov)
overlay cov;
/*
 * Make this overlay cease to exist forevermore.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct workstation *wstn = ov->ov_ws;
/*
 * Get rid of all image data for this overlay.
 */
 	if (ov->ov_flags & OVF_PIXMAP)
		gp_remove (ov->ov_pmap);
	else
		gop_remove (ov);
/*
 * Remove the overlay from the workstation chain.
 */
 	if (wstn->ws_overlay == ov)
		wstn->ws_overlay = ov->ov_next;
	else
	{
		struct overlay *prev;
		for (prev = wstn->ws_overlay; prev; prev = prev->ov_next)
			if (prev->ov_next == ov)
				break;
		if (! prev)
			c_panic ("Overlay not in device list!");
		prev->ov_next = ov->ov_next;
	}
/*
 * Release the overlay structure.
 */
	memset ((char *) ov, 0, sizeof (struct overlay));
 	relvm ((char *) ov);
	return (GE_OK);
}





struct pixmap *
G_get_pmap (cov)
overlay cov;
/*
 * Return a pointer to the pixel map for this overlay.
 */
{
	struct overlay *ov = (struct overlay *) cov;
/*
 * If this is not a pixel device, it is pretty foolish to be talking about
 * getting pixel maps.
 */
	if ((ov->ov_ws->ws_dev->gd_flags & GDF_PIXEL) == 0)
		return ((struct pixmap *) 0);
/*
 * If this overlay does not have a pixel map, we should create one.
 */
 	if ((ov->ov_flags & OVF_PIXMAP) == 0)
	{
		gop_cvt_pmap (ov->ov_ws, ov);
		ov->ov_flags &= ~OVF_ADDITIVE;
	}
/*
 * Finally, return the pmap pointer.
 */
	return (ov->ov_pmap);
}




int
G_redraw (cstn)
ws cstn;
/*
 * Force a complete redraw on this workstation.
 */
{
	struct workstation *wstn = (struct workstation *) cstn;
	struct overlay *ov;
/*
 * Mark each overlay as modified.
 */
	for (ov = wstn->ws_overlay; ov; ov = ov->ov_next)
	{
		ov->ov_flags |= OVF_MODIFIED;
		ov->ov_flags &= ~OVF_ADDITIVE;
	}
/*
 * Now force an update.
 */
 	G_update (cstn);
	return (GE_OK);
}



int
G_tx_box (cov, font, height, hjust, vjust, x, y, text, x0, y0, x1, y1)
overlay cov;
int font, hjust, vjust;
float height, x, y;
char *text;
float *x0, *y0, *x1, *y1;
/*
 * Just call G_wr_box with a rotation of 0.0
 */
{
	return (G_wr_box (cov, font, height, hjust, vjust, x, y, 0.0, text, 
			  x0, y0, x1, y1));
}



int
G_wr_box (cov, font, height, hjust, vjust, x, y, rot, text, x0, y0, x1, y1)
overlay cov;
int font, hjust, vjust;
float height, x, y, rot;
char *text;
float *x0, *y0, *x1, *y1;
/*
 * Return the bounds of a box containing this graphical text.
 * Entry:
 *	COV	is an overlay ID.
 *	FONT	is the ID of the font to use.
 *	HEIGHT	Is the height of the text, in world coords.
 *	HJUST	is the horizontal justification, being LEFT, RIGHT, or CENTER.
 *	VJUST	is the vertical justification, being TOP, BOTTOM, or CENTER.
 *	X, Y	is where to position the text.
 *	TEXT	is the actual text to write.
 * Exit:
 *	The dimensions of the text have been written into Xn, Yn.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;
	int dx, dy, pixheight, ex, ey, ftype = gt_font_type (font), sx, sy;
	float scale, aspect;
/*
 * Make sure our device can handle this operation.  We do this even though
 * the text is not really being written, since returning a dimension makes
 * no sense.
 */
 	if (ftype == GFT_PIXEL &&
		(dev->gd_flags & GDF_PIXEL) == 0)
		return (GE_DEVICE_UNABLE);
/*
 * Come up with the scale factor to be applied to the font.
 */
 	pixheight = W_TO_DC (height, 0.0, ov->ov_y1 - ov->ov_y0, dev->gd_yres);
	scale = ((float) pixheight) / ((float) gt_f_height (font));
/*
 * If they want device text, and we can't do it for whatever reason, switch
 * them to stroke.
 */
	if (ftype == GFT_DEV &&
		((ov->ov_flags & OVF_PIXMAP) || ! (dev->gd_flags & GDF_TEXT) ||
		! (*dev->gd_qtext) (ov->ov_ws->ws_tag, pixheight, rot)))
	{
		font = (pixheight > 30) ? GTF_STROKE : GTF_MINSTROKE;
		ftype = GFT_STROKE;
		scale = ((float) pixheight) / ((float) gt_f_height (font));
	}
/*
 * Figure out the start position.
 */
 	dx = W_TO_DC (x, ov->ov_x0, ov->ov_x1, dev->gd_xres);
 	dy = W_TO_DC (y, ov->ov_y0, ov->ov_y1, dev->gd_yres);
	aspect = ov->ov_ws->ws_dev->gd_aspect;
	gt_get_start (dx, dy, font, scale, hjust, vjust, rot, aspect, text, 
		&sx, &sy, &ex, &ey, dev, ov->ov_ws->ws_tag);
/*
 * Finally, convert our return values back to overlay coords.  Should a clip
 * window be applied?
 */
 	*x1 = (((float) ex) * (ov->ov_x1 - ov->ov_x0))/((float) dev->gd_xres)
			+ ov->ov_x0;
 	*y1 = (((float) ey) * (ov->ov_y1 - ov->ov_y0))/((float) dev->gd_yres)
			+ ov->ov_y0;
 	*x0 = (((float) sx) * (ov->ov_x1 - ov->ov_x0))/((float) dev->gd_xres)
			+ ov->ov_x0;
 	*y0 = (((float) sy) * (ov->ov_y1 - ov->ov_y0))/((float) dev->gd_yres)
			+ ov->ov_y0;
	return (GE_OK);
}






int
G_get_color (cstn, ncolor, base)
ws cstn;
int ncolor, *base;
/*
 * Obtain use of a range of color values.
 * Entry:
 *	CSTN	is the workstation of interest.
 *	NCOLOR	is the number of colors needed.
 * Exit:
 *	If the requested number of colors are available then
 *		BASE contains the number of the lowest of these colors.
 *		GE_OK is returned.
 *	else
 *		The return value is GE_NCOLOR.
 */
{
	struct workstation *wstn = (struct workstation *) cstn;

	if (wstn->ws_dev->gd_flags & GDF_DEV_COLOR)
		return ((*wstn->ws_dev->gd_casn) (wstn->ws_tag, ncolor, base));
	else
		return (gc_assign (wstn, ncolor, base));
}





void
G_check (cstn)
ws cstn;
/*
 * Check the device for any asnychronous changes that need to be dealt
 * with.
 */
{
	struct workstation *wstn = (struct workstation *) cstn;
	struct device *dev = wstn->ws_dev;
	struct overlay *ov;
/*
 * If there is no CHECK routine for this device, we assume that nothing
 * can go wrong.
 */
	if (! dev->gd_check)
		return;
/*
 * Check up on the device.  If a positive indication is returned, we will
 * simply redraw the whole damn thing.  Someday we could maybe do this 
 * better by looking at the actual region that was trashed, but for now
 * I don't want to deal with that.
 */
	if ((*dev->gd_check) (wstn->ws_tag))
	{
		for (ov = wstn->ws_overlay; ov; ov = ov->ov_next)
		{
			ov->ov_flags |= OVF_MODIFIED;
			ov->ov_flags &= ~OVF_ADDITIVE;
		}
		G_update (cstn);
	}
}




void
G_mark (cov)
overlay cov;
/*
 * Mark this overlay as modified.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	
	ov->ov_flags |= OVF_MODIFIED;
	ov->ov_flags &= ~OVF_ADDITIVE;
}




int
G_viewport (cov, x0, y0, x1, y1)
overlay cov;
float x0, y0, x1, y1;
/*
 * Adjust the viewport on this device to the given window, or at least as
 * close as the device will allow.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct device *dev = ov->ov_ws->ws_dev;
	int dx0 = W_TO_DC (x0, ov->ov_x0, ov->ov_x1, dev->gd_xres);
	int dy0 = W_TO_DC (y0, ov->ov_y0, ov->ov_y1, dev->gd_yres);
	int dx1 = W_TO_DC (x1, ov->ov_x0, ov->ov_x1, dev->gd_xres);
	int dy1 = W_TO_DC (y1, ov->ov_y0, ov->ov_y1, dev->gd_yres);

	if ((dev->gd_flags & GDF_VP) == 0)
		return (GE_DEVICE_UNABLE);
	return ((*dev->gd_viewport) (ov->ov_ws->ws_tag, dx0, dy0, dx1, dy1));
}




void
G_print (cstn)
ws cstn;
/*
 * Hardware print screen
 */
{
	struct workstation *wstn = (struct workstation *) cstn;
	struct device *dev = wstn->ws_dev;

	if (dev->gd_print)
		(*dev->gd_print) (wstn->ws_tag);
}




void
G_trace (t)
int t;
/*
 * Set the trace mode.
 */
{
	Trace = t;
}



int
G_name_to_rgb (cname, r, g, b)
char	*cname;
double	*r, *g, *b;
/*
 * Convert a color name to rgb values (dump this to the routine in color.c)
 */
{
	return (gc_name_to_rgb (cname, r, g, b));
}




void
G_setfd (fd)
int fd;
/* 
 * Set the kludge fd.
 */
{
	Fdesc = fd;
}


int
G_getfd ()
/*
 * Return the Fd.
 */
{
	return (Fdesc);
}
