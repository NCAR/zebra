/* $Id: pdaux.c,v 1.8 1993-04-16 17:11:14 burghart Exp $ */
/*
 * Auxilliary library routines for plot descriptions.
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
# include <ui_symbol.h>
# include <defs.h>
# include "message.h"
# include "pd.h"


/*
 * The symbol table used to store plot descriptions.
 */
static stbl Pd_table = 0;







plot_description
pda_GetPD (name)
char *name;
/*
 * Look up this plot description by name.  If such a PD exists, it is returned;
 * otherwise the return value is 0.
 */
{
	union usy_value v;
	int type;
/*
 * Here we check to see that the table has been created yet.
 */
	if (! Pd_table)
	{
		Pd_table = usy_c_stbl ("pd");
		return ((plot_description) 0);
	}
/*
 * Look it up.
 */
	if (! usy_g_symbol (Pd_table, name, &type, &v))
		return ((plot_description) 0);
	return ((plot_description) v.us_v_ptr);
}






void
pda_StorePD (pd, name)
plot_description pd;
char *name;
/*
 * Store this plot description as a named entity.
 * Entry:
 *	PD	is a plot description in internal form.
 *	NAME	is the name to give to this PD.
 * Exit:
 *	The plot description has been stored under this name.
 *	If another PD already existed with this name, it has been destroyed.
 */
{
	plot_description oldpd = pda_GetPD (name);
	union usy_value v;
/*
 * If this one already exists, zap it.
 */
	if (oldpd)
		pd_Release (oldpd);
/*
 * Store the new one.
 */
	v.us_v_ptr = (char *) pd;
	usy_s_symbol (Pd_table, name, SYMT_SYMBOL /* XXX */, &v);
}





bool
pda_Search (pd, comp, param, qual, dest, type)
plot_description pd;
char *comp, *param, *qual, *dest;
int type;
/*
 * Search a parameter from the plot description.
 */
{
	plot_description defpd;
	char qstatic[80], *qparam = NULL;
	int qlen;
	bool found = FALSE;
/*
 * Try the given place first.
 */
	found = pd_Retrieve (pd, comp, param, dest, type);
/*
 * Build the qualified parameter
 */
	if (qual)
	{
		qlen = strlen (qual) + strlen (param) + 2;
		qparam = (qlen > sizeof(qstatic)) ? 
			(char *)malloc(qlen) : qstatic;
		sprintf (qparam, "%s-%s", qual, param);
	}
	else
		qparam = param;
/*
 * Try the global component if necessary.
 */
	if (!found)
		found = pd_Retrieve (pd, "global", qparam, dest, type);
/*
 * If there is a defaults table, try it if necessary.
 */
	if (!found && (defpd = pda_GetPD ("defaults")))
		found = pd_Retrieve (defpd, "defaults", qparam, dest, type);
/*
 * Free any malloc'ed memory and vamoose with our answer.
 */
	if (qparam && (qparam != qstatic) && (qparam != param))
		free (qparam);

	return (found);
}




bool
pda_ReqSearch (pd, comp, param, qual, dest, type)
plot_description	pd;
char	*comp, *param, *qual, *dest;
int	type;
/*
 * Search for a required parameter from the plot description.  If it
 * doesn't exist, log a message.
 */
{
	bool	success;

	if (! (success = pda_Search (pd, comp, param, qual, dest, type)))
		msg_ELog (EF_PROBLEM,
			"Required parameter '%s/%s%s%s' missing!", 
			comp, (qual ? qual : ""), (qual ? "-" : ""), param);

	return (success);
}
