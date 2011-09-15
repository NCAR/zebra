/*
 * Color table routines.
 */
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

# include <string.h>
# include <X11/Xlib.h>
# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <dm.h>
# include <dm_ctable.h>
# include "dm_cmds.h"
# include "dm_vars.h"

RCSID("$Id: dm_color.c,v 2.10 1998-12-17 17:17:41 burghart Exp $")

/*
 * The symbol table used to hold color tables.
 */
static stbl Ct_table = 0;

static char CTablePath[CFG_SEARCHPATH_LEN];/* Where are the color tables? */

/*
 * The internal format of a color table.
 */
# define MAXCT 256		/* Max number of colors			*/
typedef struct color_table
{
	char	ct_name[40];		/* The name of this table	*/
	int	ct_ncolor;		/* The number of colors		*/
	XColor	*ct_colors;		/* The actual color entries	*/
} CTable;


/*
 * Local stuff.
 */
static CTable *dc_LookupTable FP ((char *));
static int dc_InTable FP ((CTable *, struct ui_command *));
static void dm_TRNak FP ((struct dm_ctr *, char *));
static int dc_RemoveTable FP ((char *name, int type, union usy_value *v, int));
static int NthColor FP ((int, SValue *, int *, SValue *, int *));




static inline unsigned short
dc_ColorValue (v)
float v;
/*
 * Turn a floating point color value into something that X can deal with.
 */
{
	if (v < 0.0 || v > 1.0)
		ui_error ("Color value %.2f out of range 0 to 1", v);
	return ((unsigned short) (v * 65535.0));
}



void
dc_Init ()
/*
 * Initialize the color table module.
 */
{
	int type[4];
	stbl vtable = usy_g_stbl ("ui$variable_table");

	Ct_table = usy_c_stbl ("color_tables");
	sprintf (CTablePath, "%s/colortables", GetLibDir ());
	usy_c_indirect (vtable, "ctablepath", CTablePath, SYMT_STRING, 
			CFG_SEARCHPATH_LEN);
	type[0] = SYMT_STRING;
	type[1] = SYMT_INT;
	uf_def_function ("nthcolor", 2, type, NthColor);
}



void
dc_Define (name)
char *name;
/*
 * Define a color table by this name.
 */
{
	CTable *ct, *old;
	union usy_value v;
	int type;
/*
 * Get a new color table structure, and begin to fill it in.  Allocate,
 * for now, the maximum possible number of xcolor structures.
 */
	ct = ALLOC (CTable);
	strcpy (ct->ct_name, name);
	ct->ct_ncolor = 0;
	ct->ct_colors = (XColor *) malloc (MAXCT * sizeof (XColor));
/*
 * Now parse the internals of the table.
 */
	ERRORCATCH
		ui_subcommand ("dm-ct-initial", "CTable>", dc_InTable,
			       (long) ct);
	ON_ERROR
		free (ct->ct_colors);
		free (ct);
		RESIGNAL;
	ENDCATCH
/*
 * Look up this table and see if it already exists.  If so, delete the
 * old version.  Eventually we may want to send out some sort of notification
 * that this table has changed, so that graphics processes can update.
 */
	if (usy_g_symbol (Ct_table, name, &type, &v))
	{
		old = (CTable *) v.us_v_ptr;
		free (old->ct_colors);
		free (old);
	}
/*
 * Define the new one.
 */
	v.us_v_ptr = (char *) ct;
	usy_s_symbol (Ct_table, name, SYMT_POINTER, &v);
/*
 * Trim the memory back to just what we need.
 */
	ct->ct_colors = (XColor *) realloc (ct->ct_colors, 
			ct->ct_ncolor*sizeof (XColor));
}






static int
dc_InTable (ct, cmds)
CTable *ct;
struct ui_command *cmds;
/*
 * Deal with the internals of a color table.
 */
{
	XColor *xc = ct->ct_colors + ct->ct_ncolor;
/*
 * Maybe we're done.
 */
	if (cmds->uc_ctype == UTT_KW && UKEY (*cmds) == DMC_ENDTABLE)
		return (FALSE);
/*
 * If this is an RGB color value, just fill it in.
 */
	if (cmds->uc_ctype == UTT_VALUE)
	{
		xc->red   = dc_ColorValue (UFLOAT (cmds[0]));
		xc->green = dc_ColorValue (UFLOAT (cmds[1]));
		xc->blue  = dc_ColorValue (UFLOAT (cmds[2]));
	}
/*
 * Otherwise we look it up.
 */
	else
	{
		XColor exact;
		if (! XLookupColor (Dm_Display, DefaultColormap(Dm_Display, 0),
				UPTR (cmds[1]), &exact, xc))
			ui_error ("Unknown color name '%s' in table %s", 
				UPTR (cmds[1]), ct->ct_name);
	}
	ct->ct_ncolor++;
	return (TRUE);
}






static CTable *
dc_LookupTable (name)
char *name;
/*
 * Try to find this table.
 */
{
	int type;
	union usy_value v;
	char cmd[120];
/*
 * See if we already know about this table.
 */
	if (usy_g_symbol (Ct_table, name, &type, &v))
		return ((CTable *) v.us_v_ptr);
/*
 * Nope, try to find a file to load up.
 */
	strcpy (cmd, "read ");
	if (! FindFile (name, CTablePath, cmd + 5))
		return (NULL);
	ui_perform (cmd);
/*
 * If it's still not there we give up.
 */
	if (! usy_g_symbol (Ct_table, name, &type, &v))
	{
		msg_ELog (EF_PROBLEM, "CTable %s absent after load", name);
		return (NULL);
	}
	return ((CTable *) v.us_v_ptr);
}





static int
NthColor (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * The NthColor clf.
 *
 *	set color NthColor(table, n)
 */
{
	char ret[32];
	int n = argv[1].us_v_int;
	CTable *ct = dc_LookupTable (argv->us_v_ptr);
	XColor *xc;
/*
 * No table, no color.
 */
	*rett = SYMT_STRING;
	if (! ct)
	{
		retv->us_v_ptr = usy_string ("white");
		return (0);
	}
/*
 * Pull it out.
 */
	if (n >= 0 && n < ct->ct_ncolor)
	{
		xc = ct->ct_colors + n;
		sprintf (ret, "rgb:%02x/%02x/%02x", xc->red, xc->green,
			 xc->blue);
		retv->us_v_ptr = usy_string (ret);
	}
	else
		retv->us_v_ptr = usy_string ("hot pink");	/* WTH */
	return (0);
}





void
dc_TableRequest (ctr, proc)
struct dm_ctr *ctr;
char *proc;	/* name of the process (not the window) making the request */
/*
 * Deal with a color table request.
 */
{
	CTable *ct = dc_LookupTable (ctr->dmm_table);
	struct dm_ctable *repl;
	int length;
/*
 * Make sure that they have asked for an existing table.
 */
	if (! ct)
	{
		dm_TRNak (ctr, proc);
		return;
	}
/*
 * Allocate an appropriately sized response.
 */
	length = sizeof (struct dm_ctable) + ct->ct_ncolor*sizeof (XColor);
	repl = (struct dm_ctable *) malloc (length);
/*
 * Put together the response.
 */
	repl->dmm_type = DM_TABLE;
	repl->dmm_ncolor = ct->ct_ncolor;
	strcpy (repl->dmm_table, ctr->dmm_table);
	memcpy (repl->dmm_cols, ct->ct_colors, ct->ct_ncolor*sizeof (XColor));
/*
 * Send it.
 */
	msg_ELog (EF_DEBUG, "Color table %s -> %s", ctr->dmm_table, proc);
	dmsg_SendProcess (proc, repl, length);
	free (repl);
}




static void
dm_TRNak (ctr, proc)
struct dm_ctr *ctr;
char *proc;
/*
 * Deal with a failed color table request.
 */
{
	struct dm_ctable repl;

	msg_ELog (EF_PROBLEM, "Failed request for table %s from %s",
		ctr->dmm_table, proc);
	repl.dmm_type = DM_NOTABLE;
	strcpy (repl.dmm_table, ctr->dmm_table);
	dmsg_SendProcess (proc, &repl, sizeof (repl));
}



static int
dc_RemoveTable (name, type, v, param)
char *name;
int type;
union usy_value *v;
int param;
{
	CTable *ct = (CTable *) v->us_v_ptr;

	if (ct->ct_colors)
		free (ct->ct_colors);
	free (ct);
	usy_z_symbol (Ct_table, name);
	return (TRUE);
}



void
dc_FreeTables ()
/*
 * Free all of our color tables and the symbol table itself
 */
{
	usy_traverse (Ct_table, dc_RemoveTable, 0, FALSE);
	usy_z_stbl (Ct_table);
	Ct_table = 0;
}



