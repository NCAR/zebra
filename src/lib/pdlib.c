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
# include <stdio.h>
# include <string.h>
# include <ctype.h>
#ifdef SVR4
# include <unistd.h>
#endif
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/param.h>
# include <sys/file.h>
# include <fcntl.h>

# include <config.h>
# include "zl_symbol.h"
# include "defs.h"
# include "message.h"
# include "pd.h"

RCSID ("$Id: pdlib.c,v 1.29 2003-04-09 23:53:38 burghart Exp $")

struct traverse {
	int (*func)();		/* Function to call for traverse */
	void *arg;		/* Argument to pass		 */
};


typedef enum { 
	PD_NOTOKEN, PD_NEWLINE, PD_END, PD_NAME,
	PD_COMPONENT, PD_PARAMETER, PD_VALUE, PD_COLON
} pd_token;


/*
 * Counters used to generate unique symbol table names.
 */
static int Count = 0;
static int CompCount = 0;

/*
 * Externals
 */
extern char *zapcase FP ((char *));

/*
 * Forwards.
 */
static int pd_CompilePD FP ((stbl pd, const char *source, const char *data, 
			     const char *end));
static void pd_Complain FP ((const char *msg, const char *source, int line, 
			     const char *data, const char *end));
static void pd_Warn FP ((const char *msg, const char *source, int line, 
			 const char *data, const char *end));
static void pd_Log FP ((int mask, const char *msg, const char *source, 
			int line, const char *data, const char *end));
static const char *pd_NextLine FP ((const char *data, const char *end));
static const char *pd_FindNewline FP ((const char *data, const char *end));
static const char *pd_GetToken FP ((const char *data, const char *end, 
				    char *name, pd_token *token));
static const char *pd_GetValue FP ((const char *data, const char *end, 
				    char *value));
static void pd_CarveString FP ((char *dest, const char *begin, 
				const char *end));
static int pd_ForEachComponent FP ((plot_description pd, int (*func)(),
				    long param));
static stbl pd_NewPD FP ((char *name));
static stbl pd_NewComponent FP ((stbl pd, const char *compname));
static int pd_ParamFunc FP((const char *name, int type, union usy_value *v,
			    struct traverse *t));
static void pd_CopyComp FP ((stbl dest, stbl src));
static int pd_UnloadParam FP ((const char *name, int type, union usy_value *v,
			       raw_plot_description *rpd));

static int pd_RelComp FP ((const char *name, int type, union usy_value *v, 
			   int));
static int pd_UnloadComp FP ((plot_description pd, stbl comp, const char *name,
			      raw_plot_description *rpd));
static int pd_CopyParam FP ((const char *name, int type, union usy_value *v, 
			     stbl));
static int pd_OverrideComp FP ((stbl pd, stbl comp, const char *name, 
				stbl dest));

/*
 * Size of temp buffer used for writing raw PD's, eg 40960
 */
# define RAWTEMP	CFG_PD_RAWTEMP


/*
 * Maximum number of components we expect, eg 50
 */
# define MAXCOMP 	CFG_PD_MAXCOMP




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
	char table[80];
	stbl pd = pd_NewPD (table);
	char *end = raw->rp_data + raw->rp_len - 1;
/*
 * Now compile each component into the PD.
 */
	pd_CompilePD (pd, table, raw->rp_data, end);
/*
 * Return the result.
 */
	return ((plot_description) pd);
}



plot_description
pd_Read (file)
const char *file;
/*
 * Load in a single plot description from a file.  Return the pd if
 * successful, 0 otherwise.
 */
{
	int fd;
	raw_plot_description rpd;
	stbl pd;
/*
 * Open the file, and find out how long it is.
 */
	if ((fd = open (file, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "could not open '%s'", file);
		return (0);
	}
#if defined(SVR4)
	rpd.rp_len = lseek (fd, 0, SEEK_END);
	(void) lseek (fd, 0, SEEK_SET);
#else
	rpd.rp_len = lseek (fd, 0, L_XTND);
	(void) lseek (fd, 0, L_SET);
#endif
/*
 * Just pull it in.
 */
	rpd.rp_data = malloc (rpd.rp_len + 1);
	rpd.rp_data[rpd.rp_len] = '\0';
	if (read (fd, rpd.rp_data, rpd.rp_len) < rpd.rp_len)
	{
		msg_ELog (EF_PROBLEM, "Reading '%s' incomplete...", file);
	}
	close (fd);
/*
 * Compile and return it.
 */
	pd = pd_NewPD (NULL);
	pd_CompilePD (pd, file, rpd.rp_data, rpd.rp_data + rpd.rp_len - 1);
	free (rpd.rp_data);
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
	char **comps;
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




static int
pd_CompilePD (pd, source, data, end)
stbl pd;
const char *source;
const char *data;
const char *end;
/*
 * Compile this pd.  Return non-zero on success, zero on failure.
 */
{
	pd_token token, expect;
	const char *last;	/* points to the line being parsed */
	int line;
	char name[256];
	char param[256];
	int nparam = 0;	/* count parameters added to a component */
	int ncomp;
	char value[1024];
	stbl comp;
	union usy_value v;
	int err;

	line = 1;
	last = data;
	comp = NULL;
	ncomp = 0;
	expect = PD_NOTOKEN;
	do {
		err = 0;
		data = pd_GetToken (data, end, name, &token);

		if (expect == PD_COLON && expect != token)
		{
			pd_Complain ("colon expected following parameter",
				     source, line, last, end);
			++err;
		}
		else if (token == PD_COLON && expect != PD_COLON)
		{
			pd_Complain ("colon not preceded by parameter",
				     source, line, last, end);
			++err;
		}
		else if (token == PD_COLON)
		{
			/*
			 * The rest of the line should be a value
			 */
			data = pd_GetValue (data, end, value);
			if (! value[0])
			{
				pd_Warn ("no parameter value", source,
					 line, last, end);
			}
			v.us_v_ptr = value;
			usy_s_symbol (comp, param, SYMT_STRING, &v);
			++nparam;
			expect = PD_NOTOKEN;
		}
		else if (expect == PD_NEWLINE && token != expect)
		{
			/* unexpected garbage following component name */
			pd_Complain ("unexpected text following component",
				     source, line, last, end);
			++err;
		}
		else if (token == PD_NEWLINE)
		{
			++line;
			last = data;
			expect = PD_NOTOKEN;
		}
		else if (token == PD_COMPONENT)
		{
			/*
			 * Check for empty components
			 */
			if (comp && nparam == 0)
				pd_Complain ("empty component", source, 
					     line, last, end);
			/*
			 * Add the component name to the PD.  The next token
			 * should be a new line.
			 */
			comp = pd_NewComponent (pd, name);
			++ncomp;
			nparam = 0;
			expect = PD_NEWLINE;
		}
		else if (token == PD_PARAMETER)
		{
			/* can't take parameters without a component first */
			if (! comp)
			{
				pd_Complain (
				     "component must precede parameter",
				     source, line, last, end);
				++err;
			}
			else
			{
				/* store parameter name and expect a colon */
				strcpy (param, name);
				expect = PD_COLON;
			}
		}
		else if (token != PD_END) /* something we weren't expecting */
		{
			pd_Complain ("unexpected token", source, line, 
				     last, end);
			++err;
		}
		if (err)
		{
			expect = PD_NOTOKEN;
			data = pd_NextLine (data, end);
			last = data;
			++line;
		}
	}
	while (token != PD_END);
	if (comp && nparam == 0)
		pd_Complain ("empty component", source, line, last, end);
	if (ncomp == 0)
		pd_Complain ("empty plot description, no components",
			     source, line, last, end);
	return (TRUE);
}




static void
pd_Complain (msg, source, line, data, end)
const char *msg;
const char *source;
int line;
const char *data;
const char *end;
{
	pd_Log (EF_PROBLEM, msg, source, line, data, end);
}



static void
pd_Warn (msg, source, line, data, end)
const char *msg;
const char *source;
int line;
const char *data;
const char *end;
{
	pd_Log (EF_DEBUG, msg, source, line, data, end);
}



static void
pd_Log (mask, msg, source, line, data, end)
int mask;
const char *msg;
const char *source;
int line;
const char *data;
const char *end;
/*
 * Gripe about this PD.  Note we can't just strcpy the offending text into
 * the message buf since it may not be null-terminated.  Argh.
 */
{
#	define BUFLEN 60
	char tmpbuf[BUFLEN];
	char *nl;

	if (data + BUFLEN - 1 <= end)
		end = data + BUFLEN - 1;
	pd_CarveString (tmpbuf, data, end);
	if ((nl = strchr (tmpbuf, '\n')))
		*nl = 0;
	msg_ELog (mask, "warning: %s at '%s...' in %s:%i ", 
		  msg, tmpbuf, source, line);
}




static const char *
pd_NextLine (data, end)
const char *data;
const char *end;
{
	pd_token token;
	char name[256];

	do {
		data = pd_GetToken (data, end, name, &token);
	}
	while ((token != PD_END) && (token != PD_NEWLINE));
	return (data);
}




static const char *
pd_GetToken (data, end, name, token)
const char *data;
const char *end;
char *name;
pd_token *token;
/*
 * Extract a line and parse it for comments, emptiness, component
 * name (no colon), or a parameter:value pair.  If a component header,
 * the component name is copied into name.  If a parameter:value pair,
 * the parameter is copied into name and the value into 'value'.
 * On return 'next' indexes the beginning of the next line to read.
 */
{
#	define NEWLINE(c) ((c)=='\n'||(c)=='\r')
#	define WHITE(c) (!(NEWLINE(c))&&isspace(c))
	const char *c;
	char *n;
/*
 * Skip whitespace until we reach a token or the end of the data.
 */
	c = data;
	while ((c <= end) && (*c) && WHITE(*c))
		++c;
	if ((c > end) || (*c == '\0'))
	{
		*token = PD_END;
		return (c);
	}
	switch (*c)
	{
	   case ':':
		*token = PD_COLON; 
		return (c+1);
	   case '\n':
	   case '\r':
		*token = PD_NEWLINE;
		return (c+1);
	   case '!':
		/* skip the rest of the line, including any newline */
		c = pd_FindNewline (c, end);
		if ((c <= end))
		{
			*token = PD_NEWLINE;
			++c;
		}
		else
			*token = PD_END;
		return (c);
	   default:
		/* it's a string which we copy below */
		break;
	}

	n = name;
	while ((c <= end) && (*c) && !isspace(*c) && 
	       (*c != '!') && (*c != ':'))
	{
		*n++ = *c++;
	}
	*n = '\0';
	/*
	 * Look ahead for the next non-whitespace or newline character, 
	 * since it indicates what kind of token we have
	 */
	while ((c <= end) && *c && WHITE(*c))
		++c;
	if ((c > end) || !(*c) || NEWLINE(*c))
		*token = PD_COMPONENT;
	else if (*c == ':')
		*token = PD_PARAMETER;
	else
		*token = PD_NAME;
	return (c);
}



static const char *
pd_FindNewline (data, end)
const char *data;
const char *end;
{
	const char *c = data;

	while ((c <= end) && *c)
	{
		if (NEWLINE(*c))
			return (c);
		++c;
	}
	return (c);
}



static const char *
pd_GetValue (data, end, value)
const char *data;
const char *end;
char *value;
/*
 * Values begin at first non-white space and end at the newline, 
 * end of string, or exclamation point.
 */
{
	const char *c;

	c = data;
	while ((c <= end) && (*c) && ! NEWLINE(*c) && (*c != '!'))
		++c;
	pd_CarveString (value, data, c);
	/*
	 * If value ended at a comment, we need to skip the comment
	 */
	while ((c <= end) && (*c) && ! NEWLINE(*c))
		++c;
	return (c);
}



static void
pd_CarveString (dest, begin, end)
char *dest;
const char *begin, *end;
/*
 * Copy out all information from BEGIN up to, but not including, END.
 */
{
/*
 * First, trim out leading and trailing white space.
 */
	for (; begin < end && (isspace(*begin)); begin++)
		;
	for (end--; end > begin && (isspace(*end)); end--)
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
	int type;
	union usy_value v;

	usy_traverse ((stbl) pd, pd_RelComp, 0, FALSE);
	if (usy_g_symbol ((stbl) pd, "$components", &type, &v))
		free (v.us_v_ptr);
	usy_z_stbl ((stbl) pd);
}




static int
pd_RelComp (name, type, v, junk)
const char *name;
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
/*
 * Allocate a humungo amount of memory that can hold the largest conceivable
 * pd; we'll recycle it later.
 */
 	rpd->rp_data = (char *) malloc (RAWTEMP);
	rpd->rp_data[0] = '\0';
/*
 * Now just go through each component.
 */
	pd_ForEachComponent (pd, pd_UnloadComp, (long) rpd);
	rpd->rp_len = strlen (rpd->rp_data);
/*
 * Sanity check
 */
	if (rpd->rp_len + 1 > RAWTEMP)
	{
		msg_ELog (EF_EMERGENCY, 
			"PD too big (%d)! Increase RAWTEMP in pdlib.c!",
			rpd->rp_len);
		exit (1);
	}
/*
 * Be sure to include the null terminator when realloc'ing.  This lets
 * rp_data be treated as a string, but note that '\0' will be stripped
 * when sent as message data because it is not included in rp_len
 */
	rpd->rp_data = (char *) realloc (rpd->rp_data, rpd->rp_len + 1);
	return (rpd);
}





static int
pd_UnloadComp (pd, comp, name, rpd)
plot_description pd;
stbl comp;
const char *name;
raw_plot_description *rpd;
/*
 * Encode a single component into this rpd.
 */
{
	strcat (rpd->rp_data, name);
	strcat (rpd->rp_data, "\n");
	usy_traverse (comp, pd_UnloadParam, (long) rpd, TRUE);
	return (0);
}





static int
pd_UnloadParam (name, type, v, rpd)
const char *name;
int type;
union usy_value *v;
raw_plot_description *rpd;
/*
 * Dump out this component of the pd.
 */
{
	char outbuf[1024];

	if (sizeof(outbuf) < (strlen(name) + strlen(v->us_v_ptr) + 4))
	{
	    msg_ELog (EF_EMERGENCY, 
		      "pd_UnloadParam: param '%s' too long (name+value > %d)!",
		      name, sizeof(outbuf));
	    exit (1);
	}
	    
	sprintf (outbuf, "\t%s:\t%s\n", name, v->us_v_ptr);
	strcat (rpd->rp_data, outbuf);
	return (TRUE);
}






static int
pd_ForEachComponent (pd, func, param)
plot_description pd;
int (*func) ();
long param;
/*
 * Execute the given FUNC for each component within the pd.  FUNC is called:
 *
 * 	(*func) (pd, comp, compname, param)
 *	plot_description pd;
 *	stbl comp;
 *	const char *compname;
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
		if ((retv = (*func) (pd, (stbl) v.us_v_ptr, comps[i], param)))
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
const char *comp, *newname;
/*
 * Pull out this component as a separate plot description.
 */
{
	stbl new, comppd, newc;
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
	new = pd_NewPD (NULL);
	newc = pd_NewComponent (new, newname);
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
pd_NewComponent (pd, compname)
stbl pd;
const char *compname;
/*
 * Add a new (empty) component to this PD.
 */
{
	char string[200], **comps;
	stbl comp;
	union usy_value v;
	int type, i;
/*
 * Create a new stbl to hold the parameters in this component.  The symbol
 * table name is qualified by a unique component number to avoid potential
 * conflicts in the master table, but is also stored unmunged within that
 * table.  It USED to be qualified by the plot description name, but that
 * is not necessary and made it difficult to add components when the pd name
 * was not known.
 */
	sprintf (string, "comp%i$%s", CompCount++, compname);
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
	comps[i] = zapcase (zl_string (compname));
/*
 * Return the new table.
 */
 	return (comp);
}




static void
pd_CopyComp (dest, src)
stbl dest, src;
/*
 * Copy all parameters from SRC to DEST.
 */
{
	usy_traverse (src, pd_CopyParam, (long) dest, FALSE);
}




static int
pd_CopyParam (name, type, v, dest)
const char *name;
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
	pd_ForEachComponent (new, pd_OverrideComp, (long) pd);
	usy_z_stbl (new);
}




static int
pd_OverrideComp (pd, comp, compname, dest)
stbl pd, comp, dest;
const char *compname;
/*
 * Merge this component into the destination PD, in place of any
 * existing component with the given name.
 */
{
	int type, i;
	union usy_value v;
	char **comps;
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
	comps[i] = zapcase (zl_string (compname));
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
	dstcomps[position] = zapcase (zl_string (srccomps[0]));
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
const char *name;
/*
 * Remove this component from this plot description.
 */
{
	int type, i;
	union usy_value v;
	char **comps;
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



void
pd_TraverseParameters (pd, compname, func, arg)
plot_description pd;
const char *compname;
int (*func)();
void *arg;
/*
 * Call the function for each parameter in the component
 */
{
	stbl comp;
	int type;
	union usy_value v;
	struct traverse t;

	/*
	 * Find the component symbol table.
	 */
	if (! usy_g_symbol ((stbl) pd, compname, &type, &v))
	{
		msg_ELog (EF_PROBLEM, "No comp '%s' in pd",
			  compname);
		return;
	}
	/*
	 * Traverse the parameters calling the designated function.
	 */
	comp = (stbl) v.us_v_ptr;
	t.func = func;
	t.arg = arg;
	usy_traverse (comp, pd_ParamFunc, (long) &t, TRUE);
}



static int
pd_ParamFunc (name, type, v, t)
const char *name;
int type;
union usy_value *v;
struct traverse *t;
{
	return ((*t->func)(name, v->us_v_ptr, t->arg));
}



plot_description
pd_CopyPD (pd)
plot_description pd;
/*
 * Return a new copy of this PD.
 */
{
	char **comps;
	plot_description new = pd_NewPD (NULL);
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
		stbl newcomp = pd_NewComponent (new, comps[i]);
		usy_g_symbol (pd, comps[i], &type, &v);
		pd_CopyComp (newcomp, (stbl) v.us_v_ptr);
	}
/*
 * Return the finished product.
 */
	return (new);
}



void
pd_MergeComp (dest, destname, src, srcname)
plot_description dest;
const char *destname;
plot_description src;
const char *srcname;
/*
 * Copy the parameters from comp1 of the src pd into comp2 of the
 * dest pd.  Existing parameters in the dest component are overridden.
 */
{
	stbl srccomp, destcomp;
	union usy_value v;
	int type;

	if (! usy_g_symbol ((stbl) src, srcname, &type, &v))
	{
		msg_ELog (EF_PROBLEM, "Src comp '%s' for merge is missing",
			  srcname);
		return;
	}
	srccomp = (stbl) v.us_v_ptr;
	if (! usy_g_symbol ((stbl) dest, destname, &type, &v))
		destcomp = pd_NewComponent (dest, destname);
	else
		destcomp = (stbl) v.us_v_ptr;
	pd_CopyComp (destcomp, srccomp);
}



void
pd_Store (pd, comp, param, value, type)
plot_description pd;
const char *comp, *param;
char *value;
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
		   	strcpy (kludge, (*(zbool *) value) ? "True" : "False");
			break;
		   case SYMT_FLOAT:
		   	sprintf (kludge, "%.4f", * (float *) value);
			break;
		   case SYMT_DATE:
		   	/* ud_format_date (kludge, (date *) value, UDF_FULL);*/
			TC_EncodeTime ((ZebTime *) value, TC_Full, kludge);
			break;
		}
	}
	usy_s_symbol (comppd, param, SYMT_STRING, &v);
}


void
pd_RemoveParam (pd, comp, param)
plot_description pd;
const char *comp, *param;
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
const char *comp;
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





zbool
pd_CompExists (pd, comp)
plot_description pd;
const char *comp;
/*
 * Return TRUE if this component exists here.
 */
{
	return (usy_defined ((stbl) pd, comp));
}
