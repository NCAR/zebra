/* 5/87 jc */
/*
 * Routines for handling overlays.
 */
# include "graphics.h"
# include "param.h"
# include "oplist.h"
# include "overlay.h"





overlay
gov_new_overlay (priority)
int priority;
/*
 * Create a new overlay, at the given priority.
 */
{
	struct overlay *ov = (struct overlay *) getvm (sizeof (struct overlay));
/*
 * Fill in the new overlay structure.
 */
 	ov->ov_ws = NULL;
	ov->ov_priority = priority;
	ov->ov_flags = OVF_VISIBLE | OVF_ADDITIVE | OVF_EMPTY;
	ov->ov_number = ov->ov_naop = 0;
	ov->ov_pmap = NULL;
	ov->ov_ops = ov->ov_aop = NULL;
	ov->ov_x0 = ov->ov_y0 = ov->ov_cx0 = ov->ov_cy0 = 0.0;
	ov->ov_x1 = ov->ov_y1 = ov->ov_cx1 = ov->ov_cy1 = 1.0;
	ov->ov_next = NULL;
	
	return ((overlay) ov);
}
