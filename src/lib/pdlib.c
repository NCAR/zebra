/*
 * The plot description library. 
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
# include <ui_error.h>
# include <ui_date.h>
# include <ui_expr.h>		/* XXX */
# include <message.h>
# include <defs.h>
# include "pd.h"
MAKE_RCSID ("$Id: pdlib.c,v 1.10 1992-01-29 15:54:06 barrett Exp $")

/*
 * A counter used to generate unique symbol table names.
 */
static int Count = 0;


/*
 * Forwards.
 */
static char *pd_CompileComp FP((char *pdname, stbl pd, char *data, char *end));
static void pd_CarveString FP ((char *dest, char *begin, char *end));
static void pd_Complain FP ((char *msg, char *data));
static int pd_ForEachComponent FP ((plot_description pd, int (*func)(),
				int param));
static stbl pd_NewPD FP ((char *name));
static stbl pd_NewComponent FP ((stbl pd, char *pdname, char *compname));

/*
 * Size of temp buffer used for writing raw PD's.
 */
# define RAWTEMP	20480


/*
 * Maximum number of components we expect.
 */
# define MAXCOMP 50




plot_description
pd_Load (raw)
raw_plot_description *raw;
/*
 * Compile a raw plot description into internal format.
 * Entry:
 *	RAW	is the raw plot description.
 * Exit:
 *	The return value is the compiled plot description.  This is a
 * 	dynamically-allocated data structure, which should be returned
 *	when no longer needed.
 */
{
	char *position, *end = raw->rp_data + raw->rp_len - 1;
	char table[80];
	stbl pd = pd_NewPD (table);
/*
 * Now compile each component into the PD.
 */
	for (position = raw->rp_data; position < end;)
		position = pd_CompileComp (table, pd, position, end);
/*
 * Return the result.
 */
	return ((plot_description) pd);
}




static stbl
pd_NewPD (name)
char *name;
/*
 * Create a new, empty plot description.
 */
{
	char table[80];
	stbl pd;
	union usy_value v;
	char **comps, *malloc ();
	int i;
/*
 * Create a new name and create the symbol table for this PD.
 */
	sprintf (table, "_pd%03d_", Count++);
	pd = usy_c_stbl (table);
/*
 * Create an empty components list.
 */
	comps = (char **) malloc (MAXCOMP*sizeof (char *));
	for (i = 0; i < MAXCOMP; i++)
		comps[i] = 0;
	v.us_v_ptr = (char *) comps;
	usy_s_symbol (pd, "$components", SYMT_POINTER, &v);
/*
 * If they want the name, give it to them.
 */
 	if (name)
		strcpy (name, table);
	return (pd);
}






static char *
pd_CompileComp (pdname, pd, data, end)
stbl pd;
char *pdname, *data, *end;
/*
 * Compile one component of this pd.
 */
{
	char *nl, *colon, *strchr (), param[100], value[200], clist[200];
	char line[200];
	stbl comp;
	union usy_value v;
	int type;
/*
 * First, get the component name out.
 */
	if (! (nl = strchr (data, '\n')))
	{
		pd_Complain ("Missing newline in comp name", data);
		return (end);
	}
	pd_CarveString (param, data, nl);
/*
 * If this is a comment line, we just pretend we're done with the component
 * and start over.
 */
	if (param[0] == '!')
		return (nl + 1);
/*
 * Add the component to the PD.
 */
 	comp = pd_NewComponent (pd, pdname, param);
/*
 * Carve out each parameter.
 */
	for (data = nl + 1; data < end && 
		(*data == '\t' || *data == '!'); data = nl + 1)
	{
	/*
	 * Find the separators.
	 */
		if (! (nl = strchr (data, '\n')))
		{
			pd_Complain ("Missing newline", data);
			return (end);
		}
		pd_CarveString (line, data, nl);
	/*
	 * Maybe this is a comment line.
	 */
		if (line[0] == '!')
			continue;
	/*
	 * Nope, pull out the various fields.
	 */
		if (! (colon = strchr (line, ':')))
		{
			pd_Complain ("Missing colon", line);
			return (end);
		}
		pd_CarveString (value, colon + 1, colon + strlen (colon));
		pd_CarveString (param, line, colon);
	/*
	 * Now just store the value.
	 */
		v.us_v_ptr = value;
		usy_s_symbol (comp, param, SYMT_STRING, &v);
	}
	return (data);
}






static void
pd_Complain (msg, data)
char *msg, *data;
/*
 * Gripe about this PD.
 */
{
	char tmpbuf[40], *nl, *strchr ();

	strncpy (tmpbuf, data, 40);
	tmpbuf[39] = '\0';
	if (nl = strchr (tmpbuf, '\n'))
		*nl = '\0';
	msg_ELog (EF_PROBLEM, "PD error (%s) at '%s...'", msg, tmpbuf);
}





static void
pd_CarveString (dest, begin, end)
char *dest, *begin, *end;
/*
 * Copy out all information from BEGIN up to, but not including, END.
 */
{
/*
 * First, trim out leading and trailing white space.
 */
	for (; begin < end && (*begin == ' ' || *begin == '\t'); begin++)
		;
	for (end--; end > begin && (*end == ' ' || *end == '\t'); end--)
		;
	if (end >= begin)
	{
		memcpy (dest, begin, end - begin + 1);
		dest[end - begin + 1] = '\0';
	}
	else
		dest[0] = '\0';
}




void
pd_Release (pd)
plot_description pd;
/*
 * Return a plot description to the system.
 */
{
	static int pd_RelComp ();
	int type;
	union usy_value v;

	usy_traverse ((stbl) pd, pd_RelComp, 0, FALSE);
	if (usy_g_symbol ((stbl) pd, "$components", &type, &v))
		free (v.us_v_ptr);
	usy_z_stbl ((stbl) pd);
}




static int
pd_RelComp (name, type, v, junk)
char *name;
int type, junk;
union usy_value *v;
/*
 * Release this component.
 */
{
	if (type == SYMT_SYMBOL)
		usy_z_stbl ((stbl) v->us_v_ptr);
	return (TRUE);
}





raw_plot_description *
pd_Unload (pd)
plot_description pd;
/*
 * Create a raw plot description from the internal representation.
 */
{
	raw_plot_description *rpd = NEW (raw_plot_description);
	char *malloc (), *realloc ();
	static int pd_UnloadComp ();
/*
 * Allocate a humungo amount of memory that can hold the largest conceivable
 * pd; we'll recycle it later.
 */
 	rpd->rp_data = malloc (RAWTEMP);
	rpd->rp_data[0] = '\0';
/*
 * Now just go through each component.
 */
	pd_ForEachComponent (pd, pd_UnloadComp, (int) rpd);
	rpd->rp_len = strlen (rpd->rp_data);
	rpd->rp_data = realloc (rpd->rp_data, rpd->rp_len);
	return (rpd);
}





static int
pd_UnloadComp (pd, comp, name, rpd)
plot_description pd;
stbl comp;
char *name;
raw_plot_description *rpd;
/*
 * Encode a single component into this rpd.
 */
{
	static int pd_UnloadParam ();

	strcat (rpd->rp_data, name);
	strcat (rpd->rp_data, "\n");
	usy_traverse (comp, pd_UnloadParam, (long) rpd, TRUE);
	return (0);
}






pd_UnloadParam (name, type, v, rpd)
char *name;
int type;
union usy_value *v;
raw_plot_description *rpd;
/*
 * Dump out this component of the pd.
 */
{
	char outbuf[300];

	sprintf (outbuf, "\t%s:\t%s\n", name, v->us_v_ptr);
	strcat (rpd->rp_data, outbuf);
	return (TRUE);
}






static int pd_ForEachComponent (pd, func, param)
plot_description pd;
int (*func) ();
int param;
/*
 * Execute the given FUNC for each component within the pd.  FUNC is called:
 *
 * 	(*func) (pd, comp, compname, param)
 *	plot_description pd;
 *	stbl comp;
 *	char *compname;
 *	int param;
 */
{
	char **comps;
	int type, retv, i;
	union usy_value v;
/*
 * Get the components list.
 */
 	usy_g_symbol ((stbl) pd, "$components", &type, &v);
	comps = (char **) v.us_v_ptr;
/*
 * Now go through it.
 */
	for (i = 0; i < MAXCOMP && comps[i]; i++)
	{
	/*
	 * Find the symbol table.
	 */
		if (! usy_g_symbol ((stbl) pd, comps[i], &type, &v))
		{
			msg_ELog (EF_PROBLEM, "BUG Missing comp '%s' in pd",
				comps[i]);
			continue;
		}
	/*
	 * Call the function.
	 */
		if (retv = (*func) (pd, (stbl) v.us_v_ptr, comps[i], param))
			return (retv);
	}
	return (0);
}



void
pd_RPDRelease (rpd)
raw_plot_description *rpd;
/*
 * Release a raw plot description that was obtained from pd_Unload.
 */
{
	free (rpd->rp_data);
	free (rpd);
}





plot_description
pd_ReadComponent (pd, comp, newname)
plot_description pd;
char *comp, *newname;
/*
 * Pull out this component as a separate plot description.
 */
{
	stbl new, comppd, newc;
	char compl[40], name[80];
	union usy_value v;
	int type;
/*
 * Find this component.
 */
	if (! usy_g_symbol (pd, comp, &type, &v))
	{
		msg_ELog (EF_PROBLEM, "pd_ReadComponent on bad comp '%s'\n",
			comp);
		return (0);
	}
	comppd = (stbl) v.us_v_ptr;
/*
 * Create the new pd and add the component table.
 */
	new = pd_NewPD (name);
	newc = pd_NewComponent (new, name, newname);
/*
 * Copy over the stuff.
 */
	pd_CopyComp (newc, comppd);
/*
 * Return the new pd.
 */
	return ((plot_description) new);
}

	




static stbl
pd_NewComponent (pd, pdname, compname)
stbl pd;
char *pdname, *compname;
/*
 * Add a new (empty) component to this PD.
 */
{
	char string[200], **comps, *zapcase ();
	stbl comp;
	union usy_value v;
	int type, i;
/*
 * Create a new stbl to hold the parameters in this component.  The symbol
 * table name is qualified by the plot description name to avoid potential
 * conflicts in the master table, but is also stored unmunged within that
 * table.
 */
	sprintf (string, "%s$%s", pdname, compname);
	comp = usy_c_stbl (string);
	v.us_v_ptr = (char *) comp;
	usy_s_symbol (pd, compname, SYMT_SYMBOL, &v);
/*
 * Add the component to the components list.
 */
	usy_g_symbol (pd, "$components", &type, &v);
	comps = (char **) v.us_v_ptr;
	for (i = 0; comps[i]; i++)
		;
	comps[i] = zapcase (usy_string (compname));
/*
 * Return the new table.
 */
 	return (comp);
}





pd_CopyComp (dest, src)
stbl dest, src;
/*
 * Copy all parameters from SRC to DEST.
 */
{
	static int pd_CopyParam ();

	usy_traverse (src, pd_CopyParam, (long) dest, FALSE);
}




static int
pd_CopyParam (name, type, v, dest)
char *name;
int type;
union usy_value *v;
stbl dest;
/*
 * Copy this param into dest.
 */
{
	usy_s_symbol (dest, name, type, v);
	return (TRUE);
}





void
pd_Merge (pd, new)
plot_description pd, new;
/*
 * Add all components of "new" DESTRUCTIVELY onto "pd".
 */
{
	static int pd_MergeComp ();

	pd_ForEachComponent (new, pd_MergeComp, (int) pd);
	usy_z_stbl (new);
}




static int
pd_MergeComp (pd, comp, compname, dest)
stbl pd, comp, dest;
char *compname;
/*
 * Merge this component into the destination PD.
 */
{
	int type, i;
	union usy_value v;
	char string[80], **comps, *zapcase ();
/*
 * If this component exists in the table now, get rid of it.
 */
	pd_RemoveComp (dest, compname);
/*
 * Add a new component with this name, but with the old table.
 */
	usy_g_symbol ((stbl) dest, "$components", &type, &v);
	comps = (char **) v.us_v_ptr;
	for (i = 0; comps[i]; i++)
		;
	comps[i] = zapcase (usy_string (compname));
	v.us_v_ptr = (char *) comp;
	usy_s_symbol (dest, compname, SYMT_SYMBOL, &v);

	return (0);
}




void
pd_AddComponent (pd, new, position)
plot_description pd, new;
int position;
/*
 * Add a new component to PD at the given position.
 */
{
	int ndest, type, i;
	SValue v;
	char **srccomps, **dstcomps;
	extern char *zapcase ();
/*
 * Get both component lists now.
 */
	usy_g_symbol ((stbl) new, "$components", &type, &v);
	srccomps = (char **) v.us_v_ptr;
	usy_g_symbol ((stbl) pd, "$components", &type, &v);
	dstcomps = (char **) v.us_v_ptr;
/*
 * If this component exists in the table now, get rid of it.  Then count
 * the destination components.
 */
	pd_RemoveComp (pd, srccomps[0]);
	for (ndest = 0; dstcomps[ndest]; ndest++)
		; /* yawn */
/*
 * If the position is positive, it is an absolute position, after the 
 * global comp.  Figure the real position accordingly.
 */
	if (position > 0)
	{
		if (position > ndest)
			position = ndest;
	}
/*
 * A negative position means count back from the end.
 */
	else
	{
		if ((position = position + ndest) < 1)
			position = 1;
	}
/*
 * Move any intervening components upward and store this one.
 */
	for (i = ndest; i > position; i--)
		dstcomps[i] = dstcomps[i - 1];
	dstcomps[position] = zapcase (usy_string (srccomps[0]));
	dstcomps[ndest + 1] = 0;
/*
 * Add the actual entry into the destination table.
 */
	usy_g_symbol (new, srccomps[0], &type, &v);
	usy_s_symbol ((stbl) pd, srccomps[0], SYMT_SYMBOL, &v);
}






int
pd_RemoveComp (pd, name)
plot_description pd;
char *name;
/*
 * Remove this component from this plot description.
 */
{
	stbl comp;
	int type, nlen = strlen (name), i;
	union usy_value v;
	char clist[200], *cp, *strchr (), *blank, **comps;
/*
 * Try to find the component first.
 */
	if (! usy_g_symbol (pd, name, &type, &v))
		return (FALSE);
/*
 * Clear the symbol table.
 */
 	usy_z_stbl ((stbl) v.us_v_ptr);
	usy_z_symbol (pd, name);
/*
 * Remove the name from the components list.
 */
	usy_g_symbol (pd, "$components", &type, &v);
	comps = (char **) v.us_v_ptr;
	for (i = 0; comps[i] && strcmp (comps[i], name); i++)
		;
	usy_rel_string (comps[i]);
	for (; comps[i]; i++)
		comps[i] = comps[i + 1];
	return (TRUE);
}






bool
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
	   case SYMT_BOOL:
		* (int *) target = v.us_v_int;
		break;
	   case SYMT_DATE:
	   	* (date *) target = v.us_v_date;
		break;
	   case SYMT_FLOAT:
	   	* (float *) target = v.us_v_float;
		break;
	}
	return (TRUE);
}





char **
pd_CompList (pd)
plot_description pd;
/*
 * Retrieve the list of components for this plot description.
 */
{
	int type;
	union usy_value v;

	usy_g_symbol ((stbl) pd, "$components", &type, &v);
	return ((char **) v.us_v_ptr);
}





plot_description
pd_CopyPD (pd)
plot_description pd;
/*
 * Return a new copy of this PD.
 */
{
	char name[80], **comps;
	plot_description new = pd_NewPD (name);
	union usy_value v;
	int type, i;
/*
 * Move the comment field if there is one.
 */
	if (usy_g_symbol ((stbl) pd, "$comment", &type, &v))
		usy_s_symbol ((stbl) new, "$comment", type, &v);
/*
 * Now get the component list.
 */
	usy_g_symbol ((stbl) pd, "$components", &type, &v);
	comps = (char **) v.us_v_ptr;
/*
 * Copy over each component.
 */
	for (i = 0; comps[i]; i++)
	{
		stbl newcomp = pd_NewComponent (new, name, comps[i]);
		usy_g_symbol (pd, comps[i], &type, &v);
		pd_CopyComp (newcomp, (stbl) v.us_v_ptr);
	}
/*
 * Return the finished product.
 */
	return (new);
}




void
pd_Store (pd, comp, param, value, type)
plot_description pd;
char *comp, *param, *value;
int type;
/*
 * Store an attribute into this PD.
 * Entry:
 *	PD	is the plot description
 *	COMP	is the name of the component to store into.
 *	PARAM	is the name of the parameter to store.
 *	VALUE	is the value to be stored
 *	TYPE	is the type of that value.
 * Exit:
 *	The value has been stored.
 */
{
	stbl comppd;
	int t;
	union usy_value v;
	char kludge[200];
/*
 * Find the component.
 */
	if (! usy_g_symbol (pd, comp, &t, &v))
	{
		msg_ELog (EF_PROBLEM, "Store attempted on missing comp '%s'",
			comp);
		return;
	}
	comppd = (stbl) v.us_v_ptr;
/*
 * Now simply store the value.
 */
	if (type == SYMT_STRING)
		v.us_v_ptr = value;
	else
	{
		v.us_v_ptr = kludge;
		switch (type)
		{
		   case SYMT_INT:
		   	sprintf (kludge, "%d", * (int *) value);
			break;
		   case SYMT_BOOL:
		   	strcpy (kludge, (* (int *) value) ? "True" : "False");
			break;
		   case SYMT_FLOAT:
		   	sprintf (kludge, "%.4f", * (float *) value);
			break;
		   case SYMT_DATE:
		   	ud_format_date (kludge, (date *) value, UDF_FULL);
			break;
		}
	}
	usy_s_symbol (comppd, param, SYMT_STRING, &v);
}


void
pd_RemoveParam (pd, comp, param)
plot_description pd;
char *comp, *param;
/*
 * Remove a parameter from this PD.
 * Entry:
 *	PD	is the plot description
 *	COMP	is the name of the component to remove from.
 *	PARAM	is the name of the parameter to remove.
 * Exit:
 *	The value has been remove.
 */
{
	stbl comppd;
	int t;
	union usy_value v;
/*
 * Find the component.
 */
	if (! usy_g_symbol (pd, comp, &t, &v))
	{
		msg_ELog (EF_PROBLEM, "Remove attempted on missing comp '%s'",
			comp);
		return;
	}
	comppd = (stbl) v.us_v_ptr;
/*
 * Now simply remove the value.
 */
	usy_z_symbol (comppd, param);
}




void
pd_MoveComponent (pd, comp, newpos)
plot_description pd;
char *comp;
int newpos;
/*
 * Move this component to the given new position.
 */
{
	char **comps, *scomp;
	SValue v;
	int type, oldpos, ncomp, i;
/*
 * Get the component list for this PD.
 */
	usy_g_symbol ((stbl) pd, "$components", &type, &v);
	comps = (char **) v.us_v_ptr;
/*
 * Count the components, and also find the one we want to move.
 */
	oldpos = -1;
	for (ncomp = 0; comps[ncomp]; ncomp++)
		if (! strcmp (comps[ncomp], comp))
			oldpos = ncomp;
	if (oldpos < 0)
	{
		msg_ELog (EF_PROBLEM, "Attempt to move bogus comp %s", comp);
		return;
	}
/*
 * Adjust newpos to a true absolute position.
 */
	if (newpos <= 0)
		newpos += ncomp - 1;
	if (newpos <= 0 || newpos >= ncomp)
		newpos = ncomp - 1;
/*
 * Now shuffle things around.
 */
	scomp = comps[oldpos];   /* Save our own pointer to comp */
	if (newpos < oldpos)
		for (i = oldpos; i > newpos; i--)
			comps[i] = comps[i - 1];
	else
		for (i = oldpos; i < newpos; i++)
			comps[i] = comps[i + 1];
	comps[newpos] = scomp;
}





bool
pd_CompExists (pd, comp)
plot_description pd;
char *comp;
/*
 * Return TRUE if this component exists here.
 */
{
	return (usy_defined ((stbl) pd, comp));
}
