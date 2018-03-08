/* 5/87 jc */
/* $Id: oplist.c,v 1.5 2002-07-11 23:13:28 burghart Exp $ */
/*
 * Handle operations lists.
 */
# include "param.h"
# include "graphics.h"
# include "oplist.h"
# include "overlay.h"
# include "workstation.h"
# include "device.h"
# include "pixel.h"

/*
 * Forwards
 */
void gop_perform(), gop_pm_perform();



void
gop_add_op (opcode, color, extra, nextra, cov, npt, data, keep)
int opcode, color;
int *extra, nextra;
overlay cov;
int npt, keep;
char *data;
/*
 * Add an operation to this overlay's oplist.
 * Entry:
 *	OPCODE	is the code for the operation to be performed.
 *	COLOR	is the color to use for this operation.
 *	EXTRA	is data for the extra-data array.
 *	NEXTRA	is the length of this data.
 *	OV	is the overlay to be drawn into.
 *	NPT	is the number of data points to go with this command.
 *	DATA	is the list of those points.
 *	KEEP	is true of DATA is a dynamically-allocated array that can
 *		be kept with the list.  If KEEP is FALSE, a new array will
 *		be created and the data copied.
 * Exit:
 *	The operation has been added to the list.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct oplist *op, *gop_new_list ();
	struct operation *where;
	int nx;
/*
 * Sanity check.
 */
 	if (ov->ov_flags & OVF_PIXMAP)
		c_panic ("gop_add_op () called for a pixmap overlay");
/*
 * Find out where this operation is to go, adding a new list if necessary.
 */
 	if (ov->ov_ops == NULL)
		ov->ov_aop = ov->ov_ops = op = gop_new_list ();
	else
	{
		for (op = ov->ov_ops; op->opc_chain; op = op->opc_chain)
			;
		if (op->opc_nops >= N_OP_STRUCT)
		{
			op->opc_chain = gop_new_list ();
			op = op->opc_chain;
		}
	}
	where = op->opc_data + op->opc_nops++;
/*
 * Now stuff away the info.
 */
	where->op_opcode = opcode;
	where->op_color = color;
	where->op_npt = npt;
	if (npt > 0)
	{
		if (keep)
			where->op_data = data;
		else
		{
			where->op_data = getvm (npt);
			COPY (where->op_data, data, npt);
		}
	}
	else
		where->op_data = (char *) 0;
	for (nx = 0; nx < nextra; nx++)
		where->op_extra[nx] = extra[nx];
}



static char *C_opcodes[] = { "POLYLINE", "TEXT", "SETHCW", 0};

void
gop_dump_oplist (cov)
overlay cov;
/*
 * Dump out this oplist.
 */
{
	struct overlay *ov = (struct overlay *) cov;
	struct oplist *op = ov->ov_ops;

	printf ("Oplist dump....\n");
	while (op)
	{
		int nop;
		printf ("  List, %d ops\n", op->opc_nops);
		for (nop = 0; nop < op->opc_nops; nop++)
			printf ("\top %s, color %d, npt %d\n",
				C_opcodes[op->opc_data[nop].op_opcode],
				op->opc_data[nop].op_color,
				op->opc_data[nop].op_npt);
		op = op->opc_chain;
	}
}



struct oplist *
gop_new_list ()
/*
 * Create a new, empty oplist, and return a pointer to it.
 */
{
	struct oplist *list = (struct oplist *) getvm (sizeof (struct oplist));

	list->opc_chain = (struct oplist *) NULL;
	list->opc_nops = 0;
	list->opc_data = (struct operation *)
			getvm (N_OP_STRUCT * sizeof (struct operation));
	return (list);
}




void
gop_update (wstn, ov, add)
struct workstation *wstn;
struct overlay *ov;
int add;
/*
 * Update this overlay on this workstation.
 */
{
	struct oplist *op = ov->ov_ops, *lastop;
	int nop, opbegin = 0;
/*
 * If this overlay has an empty oplist, there is nothing for us to do.
 */
 	if (! op)
		return;
/*
 * If this is an additive update, get positioned correctly.
 */
 	if (add)
	{
		op = ov->ov_aop;
		opbegin = ov->ov_naop;
	}
/*
 * Pass through all of the oplists, and perform each individual operation.
 */
	while (op)
	{
		for (nop = opbegin; nop < op->opc_nops; nop++)
			gop_perform (wstn, op->opc_data + nop, ov);
		lastop = op;
		op = op->opc_chain;
		opbegin = 0;
	}
/*
 * Mark this point for future additive updates.
 */
 	ov->ov_aop = lastop;
	ov->ov_naop = lastop->opc_nops;
	ov->ov_flags |= OVF_ADDITIVE;
}




void
gop_perform (wstn, data, ov)
struct workstation *wstn;
struct operation *data;
struct overlay *ov;
/*
 * Perform an individual operation.
 */
{
	switch (data->op_opcode)
	{
	   /*
	    * Polyline operation.
	    */
	   case GOP_POLYLINE:
	   	(*wstn->ws_dev->gd_polyline) (wstn->ws_tag, data->op_color,
			data->op_extra[GOP_PL_LTYPE],
			data->op_npt/(sizeof (int) * 2), data->op_data);
		break;
	   /*
	    * Text.
	    */
	   case GOP_TEXT:
	   	gt_do_text (wstn, ov, data->op_color, data->op_extra[GOP_T_X],
			data->op_extra[GOP_T_Y], data->op_extra[GOP_T_FONT],
			data->op_extra[GOP_T_SCALE], 
			data->op_extra[GOP_T_ROT] / 100.0, 
			data->op_data, TRUE);
		break;
	   /*
	    * Set hardware clip window.
	    *
	    * 10/89 jc:	Stash the clip information into the overlay itself,
	    *		since some of the other operations use it.  We should
	    *		end up back with the original clip info by the time
	    *		we get to the end of the oplist.
	    */
	   case GOP_SETHCW:
	   	(*wstn->ws_dev->gd_set_hcw) (wstn->ws_tag,
			data->op_extra[GOP_W_X0], data->op_extra[GOP_W_Y0],
			data->op_extra[GOP_W_X1], data->op_extra[GOP_W_Y1]);
	   	ov->ov_cx0 = data->op_extra[GOP_W_X0];
	   	ov->ov_cy0 = data->op_extra[GOP_W_Y0];
	   	ov->ov_cx1 = data->op_extra[GOP_W_X1];
	   	ov->ov_cy1 = data->op_extra[GOP_W_Y1];
		break;

	   default:
	   	c_panic ("Bogus opcode: %d\n", data->op_opcode);
	}
}



void
gop_clear (ov)
struct overlay *ov;
/*
 * Clear out this overlay.
 */
{
	int nop;
	struct oplist *opl;
/*
 * If there is no oplist at all, no work needs to be done.
 */
	if (! ov->ov_ops)
		return;
/*
 * Step through all of the oplist structures hanging off this overlay, and
 * free up all memory associated with each one.
 */
 	for (opl = ov->ov_ops; opl; opl = opl->opc_chain)
	{
		for (nop = 0; nop < opl->opc_nops; nop++)
			if (opl->opc_data[nop].op_data)
			{
				relvm (opl->opc_data[nop].op_data);
				opl->opc_data[nop].op_data = NULL;
			}
		opl->opc_nops = 0;
	}
/*
 * Now we need to do away with the oplist structures themselves.  However,
 * since the first thing we will probably do is add operations to this
 * overlay, we will preserve the last structure.
 */
 	while (ov->ov_ops->opc_chain)
	{
		opl = ov->ov_ops;
		ov->ov_ops = ov->ov_ops->opc_chain;
		relvm (opl);
	}
	ov->ov_aop = ov->ov_ops;
	ov->ov_naop = 0;
}



void
gop_remove (ov)
struct overlay *ov;
/*
 * Completely remove the oplist from this overlay.
 */
{
	gop_clear (ov);
	if (ov->ov_ops)
	{
		relvm (ov->ov_ops);
		ov->ov_ops = ov->ov_aop = NULL;
	}
}





void
gop_cvt_pmap (wstn, ov)
struct workstation *wstn;
struct overlay *ov;
/*
 * Convert an oplist-based overlay into a pixel-mapped overlay.
 */
{
	struct oplist *op = ov->ov_ops;
	struct overlay fake_ov;
	int nop;
	
	if (ov->ov_flags & OVF_PIXMAP)
		c_panic ("This overlay is already pixmapped!");
/*
 * Create the new pixel map.  Also clone off a copy of the overlay structure,
 * which we use while performing ops so that we can tweak things (like the
 * clip window) around.
 */
 	ov->ov_pmap = gp_make_pmap (wstn->ws_dev);
	fake_ov = *ov;
	fake_ov.ov_flags |= OVF_PIXMAP;
/*
 * Now perform each oplist into the pixmap.
 */
	while (op)
	{
		for (nop = 0; nop < op->opc_nops && nop < N_OP_STRUCT; nop++)
			gop_pm_perform (wstn, op->opc_data + nop, &fake_ov);
		op = op->opc_chain;
	}
/*
 * Clear out the oplist, and change the classification of this overlay.
 */
 	gop_clear (ov);
	if (ov->ov_ops)
		relvm (ov->ov_ops);	/* Get the last one too */
	ov->ov_aop = ov->ov_ops = 0;
	ov->ov_flags |= OVF_PIXMAP;
}



void
gop_pm_perform (wstn, data, ov)
struct workstation *wstn;
struct operation *data;
struct overlay *ov;
/*
 * Perform an individual operation into this overlay's pixmap.
 */
{
	switch (data->op_opcode)
	{
	   /*
	    * Polyline operation.
	    */
	   case GOP_POLYLINE:
	   	gp_pl (ov, data->op_color, data->op_extra[GOP_PL_LTYPE],
			data->op_npt/(sizeof (int) * 2), (int*)data->op_data);
		break;
	   /*
	    * Text.
	    */
	   case GOP_TEXT:
	   	gt_do_text (wstn, ov, data->op_color, data->op_extra[GOP_T_X],
			data->op_extra[GOP_T_Y], data->op_extra[GOP_T_FONT],
			data->op_extra[GOP_T_SCALE], 
			data->op_extra[GOP_T_ROT] / 100.0,
			data->op_data, FALSE);
		break;
	   /*
	    * SHCW: we handle this by just stuffing the new clip window
	    * numbers into the overlay, so that succeeding instructions will
	    * see them.
	    */
	   case GOP_SETHCW:
	   	ov->ov_cx0 = data->op_extra[GOP_W_X0];
	   	ov->ov_cy0 = data->op_extra[GOP_W_Y0];
	   	ov->ov_cx1 = data->op_extra[GOP_W_X1];
	   	ov->ov_cy1 = data->op_extra[GOP_W_Y1];
		break;

	   default:
	   	c_panic ("Bogus opcode: %d\n", data->op_opcode);
	}
}


