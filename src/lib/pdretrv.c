/*
 * Extracted from pdlib.c and put in a separate file, otherwise utilities
 * which don't need it (like pdcheck) are forced to link with X libraries
 * due to references in ui_expr.o
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

# include <stdio.h>
# include <string.h>
# include <ctype.h>

# include <ui.h>
# include <ui_symbol.h>
# include <ui_error.h>
# include <ui_date.h>
# include <ui_expr.h>		/* XXX */

# include <config.h>
# include "defs.h"
# include "message.h"
# include "pd.h"

RCSID ("$Id: pdretrv.c,v 2.3 1998-10-28 21:22:44 corbet Exp $")

zbool
pd_Retrieve (pd, comp, param, target, type)
plot_description pd;
char *comp, *param, *target;
int type;
/*
 * Try to retrieve a parameter from a plot description.
 * Entry:
 *	PD	is the internal plot description.
 *	COMP	is the component in which to look.
 *	PARAM	is the name of the parameter to search for.
 *	TARGET	is the destination of the parameter
 *	TYPE	is the expected data type of the parameter.
 * Exit:
 *	If the parameter is found then:
 *		It is converted to TYPE and stored in TARGET
 *		the return value is TRUE
 *	else
 *		The return value is FALSE.
 */
{
	union usy_value v;
	int t;
	struct parse_tree *pt;
/*
 * First, find this component.
 */
 	if (! usy_g_symbol (pd, comp, &t, &v))
		return (FALSE);
/*
 * Now look for the parameter.
 */
	if (! usy_g_symbol ((stbl) v.us_v_ptr, param, &t, &v))
		return (FALSE);
/*
 * If they want a string, give it to them.
 */
	if (type == SYMT_STRING)
	{
		strcpy (target, v.us_v_ptr);
		return (TRUE);
	}
/*
 * Evaluate it.
 */
	if (! (pt = ue_parse (v.us_v_ptr, 0, FALSE)))
	{
		msg_ELog (EF_PROBLEM, "Unparsable %s/%s = %s", comp, param,
			v.us_v_ptr);
		return (FALSE);
	}
	ue_eval (pt, &v, &t);
	ue_rel_tree (pt);
/*
 * If necessary, do the coercion.
 */
	ERRORCATCH
		if (type != t)
			uit_coerce (&v, t, type);
	ON_ERROR
		return (FALSE);
	ENDCATCH
/*
 * Store the result.
 */
	switch (type)
	{
	   case SYMT_INT:
		* (int *) target = v.us_v_int;
		break;
	   case SYMT_BOOL:
		* (zbool *) target = (zbool) v.us_v_int;
		break;
	   case SYMT_DATE:
	   	/* * (date *) target = v.us_v_date; */
		TC_UIToZt (&v.us_v_date, (ZebTime *) target);
		break;
	   case SYMT_FLOAT:
	   	* (float *) target = v.us_v_float;
		break;
	}
	return (TRUE);
}



