/* $Id: pdaux.c,v 1.2 1990-07-08 13:00:54 corbet Exp $ */
/*
 * Auxilliary library routines for plot descriptions.
 */
# include <ui_symbol.h>
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
	char qparam[200];
/*
 * Try the given place first.
 */
	if (pd_Retrieve (pd, comp, param, dest, type))
		return (TRUE);
/*
 * Try the global component.
 */
	if (qual)
	{
		sprintf (qparam, "%s-%s", qual, param);
		param = qparam;
	}
	if (pd_Retrieve (pd, "global", param, dest, type))
		return (TRUE);
/*
 * If there is a defaults table, try it too.
 */
	if ((defpd = pda_GetPD ("defaults")) &&
			pd_Retrieve (defpd, "defaults", param, dest, type))
		return (TRUE);
/*
 * Nope.
 */
	return (FALSE);
}
