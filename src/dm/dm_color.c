/*
 * Color table routines.
 */
static char *rcsid = "$Id: dm_color.c,v 2.1 1991-09-12 01:30:31 corbet Exp $";
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

# include <X11/Xlib.h>
# include <ui.h>
# include <ui_error.h>
# include "../include/defs.h"
# include "dm_cmds.h"
# include "dm_vars.h"


/*
 * The symbol table used to hold color tables.
 */
static stbl Ct_table = 0;

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
# ifdef __STDC__
	static CTable *dc_LookupTable (char *);
	static int dc_InTable (CTable *, struct ui_command *);
	static unsigned short dc_ColorValue (double);
	static void dm_TRNak (struct dm_ctr *, char *);
# else
	static CTable *dc_LookupTable ();
	static int dc_InTable ();
	static unsigned short dc_ColorValue ();
	static void dm_TRNak ();
# endif




dc_Init ()
/*
 * Initialize the color table module.
 */
{
	Ct_table = usy_c_stbl ("color_tables");
}




dc_Define (name)
char *name;
/*
 * Define a color table by this name.
 */
{
	CTable *ct, *old;
	union usy_value v;
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
		ui_subcommand ("dm-ct-initial", "CTable>", dc_InTable, ct);
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
	if (old = dc_LookupTable (name))
	{
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





static CTable *
dc_LookupTable (name)
char *name;
/*
 * Try to find this table.
 */
{
	int type;
	union usy_value v;

	if (! usy_g_symbol (Ct_table, name, &type, &v))
		return (NULL);
	return ((CTable *) v.us_v_ptr);
}





void
dc_TableRequest (ctr, proc)
struct dm_ctr *ctr;
char *proc;
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
	msg_send (proc, MT_DISPLAYMGR, FALSE, repl, length);
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
	msg_send (proc, MT_DISPLAYMGR, FALSE, &repl, sizeof (repl));
}
