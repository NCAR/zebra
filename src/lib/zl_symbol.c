/*
 * This is the Zebra library version of the RDSS UI symbol table module.
 */


#include <ctype.h>
#include <string.h>

#include "defs.h"
#include "zl_symbol.h"
#include "zl_regex.h"

RCSID("$Id: zl_symbol.c,v 2.4 2004-10-25 22:01:46 burghart Exp $")

/*
 * This is the format of a single symbol table entry.
 */
struct ste
{
	char	*ste_sym;		/* The actual symbol.		*/
	byte	ste_type;		/* The type of this symbol	*/
	byte 	ste_indirect;		/* Is this an indir. symbol?	*/
	short	ste_length;		/* ...for indirect strings	*/
	union usy_value ste_v;		/* Symbol value			*/
	struct ste *ste_next;		/* Next entry in hash chain	*/
	struct sdaemon *ste_daemons;	/* Watcher daemons		*/
};

/*
 * Actual symbol table entries are kept in a hashed scheme, to reduce the
 * access time.
 */
# define HASH_MOD 113
/* # define HASH_MASK 0x3F */
# define HASH(s) usy_hash (s)

/*
 * A structure describing a symbol daemon.
 */
struct sdaemon
{
	int (*sd_func) ();		/* The function to call		*/
	char * sd_arg;			/* Arg to pass in		*/
	struct sdaemon *sd_next;	/* Next daemon			*/
	int sd_ops;			/* Operations of interest	*/
};

/* 
 * This is the format of an actual symbol table.
 */
struct symbol_table
{
	char	*st_name;	/* The name given to this table.	*/
	int 	st_nsym;	/* Number of syms in the table		*/
	struct ste *st_ste[HASH_MOD];	/* Chains of symbol defs	*/
};

/*
 * We have one symbol table that is allocated at initialization time.  The
 * main purpose of this table is to keep track of the other tables that
 * have been created.
 */
static struct symbol_table *M_table = NULL;
#ifdef STAT_TABLE
static stbl Stat_table;
#endif

/*
 * Routines that are used.
 */
static struct ste *usy_find_symbol ();
static struct symbol_table *usy_extr_table ();


/*
 * Statistics stuff.
 */
static int S_nlookup = 0;		/* Number of symbol lookups	*/
static int S_ncoll = 0;			/* Number of hash collisions	*/
static int S_nset = 0;			/* Number of symbol sets	*/



static int
usy_hash (s)
const char *s;
/*
 * Return a hash number for this string.
 */
{
	int sum = 0;
	
	while (*s)
		sum += *s++;
	return (sum % HASH_MOD);
}




void
usy_init ()
/*
 * Initialize the symbol table module.
 */
{
	if (M_table)
		return;
/*
 * Create the master table.
 */
	M_table = (struct symbol_table *) getvm (sizeof (struct symbol_table));
	zfill ((char *) M_table, sizeof (struct symbol_table));
	M_table->st_name = usy_pstring ("usym$master_table");
#ifdef STAT_TABLE
/*
 * Create our symbol table to hold externally (user) accessible stuff.
 */
	Stat_table = usy_c_stbl ("stats");
/*
 * Be the first on the block to get indirect variables into the table.
 * (2/89 jc) The statistics are now stored in a separate table.
 */
 	usy_c_indirect (Stat_table, "s_nlookup", &S_nlookup,
		SYMT_INT, 0);
 	usy_c_indirect (Stat_table, "s_ncoll", &S_ncoll, SYMT_INT,0);
 	usy_c_indirect (Stat_table, "s_nset", &S_nset, SYMT_INT,0);
/*
 * Initialize dynamic strings.
 */
 	zl_st_init (Stat_table);
#else
 	zl_st_init (NULL);
#endif
}




stbl
usy_c_stbl (name)
const char *name;
/*
 * Create a symbol table by this name.
 */
{
	struct ste *sp;
	struct symbol_table *table;
	union usy_value v;
	char xname[20];

	if (! M_table)
		usy_init ();
/*
 * See if there is already a table by this name.  If so, zap it.
 */
	if (name && (sp = usy_find_symbol (M_table, name)) &&
			sp->ste_type == SYMT_SYMBOL)
		usy_z_stbl (sp->ste_v.us_v_ptr);
/*
 * Get the memory for a table.
 */
 	table = (struct symbol_table *) getvm (sizeof (struct symbol_table));
	if (! name)
	{
		sprintf (xname, "%lx", (long) table);
		name = xname;
	}
	zfill ((char *) table, sizeof (struct symbol_table));
	table->st_name = zl_zapcase (usy_pstring (name));
	table->st_nsym = 0;
/*
 * Put an entry into the master table for this new table.
 */
	v.us_v_ptr = (char *) table;
 	usy_s_symbol ((stbl) M_table, name, SYMT_SYMBOL, &v);
	return ((stbl) table);
}



void
usy_z_stbl (stable)
stbl stable;
/*
 * Delete this symbol table.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	int slot;

	if (! M_table)
		return;
/*
 * Do a sanity check.
 */
 	if (! usy_find_symbol (M_table, table->st_name))
		/*c_panic ("Zap on bad stbl '%s'", table->st_name);*/
		return;
/*
 * Delete the reference in the master table.
 */
 	usy_z_symbol ((stbl) M_table, table->st_name);
/*
 * Now clear out all symbols within the table.
 */
 	for (slot = 0; slot < HASH_MOD; slot++)
		while (table->st_ste[slot])
		{
			struct ste *sp = table->st_ste[slot];
			table->st_ste[slot] = sp->ste_next;
			if (sp->ste_type == SYMT_STRING && ! sp->ste_indirect)
				usy_rel_string (sp->ste_v.us_v_ptr);
			usy_rel_string (sp->ste_sym);
			relvm (sp);
		}
/*
 * Finally, release the table itself.
 */
	relvm (table->st_name);
 	relvm (table);
}



void
usy_z_symbol (stable, symbol)
stbl stable;
const char *symbol;
/*
 * Remove this symbol from the table.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	int slot;
	struct ste *sym, *prev;
	struct sdaemon *sdp, *old;

	if (! M_table)
		return;
/*
 * Look up this symbol.  If it does not exist, we will silently quit.
 */
 	if ((sym = usy_find_symbol (table, symbol)) == 0)
		return;
/*
 * Release any daemon structures.  Eventually we will want to signal
 * DESTROY operations too.
 */
	for (sdp = sym->ste_daemons; sdp;)
	{
		old = sdp;
		sdp = sdp->sd_next;
		relvm (old);
	}

/*
 * We have to unlink this symbol from the chain.  If it is the first one,
 * we have it easy.
 */
	symbol = zl_zapcase (usy_string ((char *) symbol));
 	slot = HASH (symbol);
 	if (table->st_ste[slot] == sym)
		table->st_ste[slot] = sym->ste_next;
/*
 * Nope, we have to search for it.
 */
 	else
	{
		for (prev = table->st_ste[slot]; prev; prev = prev->ste_next)
			if (prev->ste_next == sym)
				break;
		if (! prev)
			/*c_panic("Unable to find prev ptr for '%s'",symbol);*/
			return;
		prev->ste_next = sym->ste_next;
	}
/*
 * Now that we have the symbol by itself, we can release everything.
 */
	table->st_nsym--;
 	if (sym->ste_type == SYMT_STRING && ! sym->ste_indirect)
		usy_rel_string (sym->ste_v.us_v_ptr);
	usy_rel_string (sym->ste_sym);
	relvm (sym);
}
 
 
 
 


static struct ste *
usy_find_symbol (table, symbol)
struct symbol_table *table;
char *symbol;
/*
 * Find a symbol table entry.
 * Entry:
 *	TABLE	is a pointer to a symbol table.
 *	SYMBOL	is the symbol of interest.
 * Exit:
 *	If the symbol is defined in this table, return a pointer to the
 *		ste.
 *	else
 *		null is returned.
 */
{
	char lsym[500];
	int i;
	struct ste *sp;

	if (! table)
		return (NULL);
/*
 * Copy over the symbol, putting it into lower case while we're at it.
 */
	for (i = 0; symbol[i]; i++)
		lsym[i] = isupper(symbol[i]) ? tolower(symbol[i]) : symbol[i];
	lsym[i] = '\0';
/*
 * Find the table.
 */
	S_nlookup++;
	table = usy_extr_table (table, lsym);
	if (! table)
		return (NULL);
/*
 * Index into the hash structure, and find the chain to search.
 */
 	sp = table->st_ste[HASH (lsym)];
/*
 * Now search the chain.  We do the string compare in place, since this
 * will happen often, and we would like to avoid the strcmp overhead.
 */
 	while (sp)
	{
		char *sym = sp->ste_sym, *test = lsym;
		for (; *sym == *test; sym++, test++)
			if (*sym == '\0')
				return (sp);
		S_ncoll++;
		sp = sp->ste_next;
	}
	return (0);
}




static void
usy_g_indirect (sym, v)
struct ste *sym;
union usy_value *v;
/*
 * Return a value from an indirected symbol.
 */
{
	char *ip = sym->ste_v.us_v_ptr;
	
	switch (sym->ste_type)
	{
	   case SYMT_FLOAT:
		v->us_v_float = * (float *) ip;
		break;
	   case SYMT_INT:
	   	v->us_v_int = * (int *) ip;
		break;
 	   case SYMT_STRING:
	   case SYMT_SYMBOL:
	   case SYMT_POINTER:
		v->us_v_ptr = ip;
		break;
	   case SYMT_DATE:
	   	v->us_v_date = * (date *) ip;
		break;
	   case SYMT_BOOL:
	   	v->us_v_int = * (zbool *) ip;
		break;
	}
}




int
usy_g_symbol (stable, symbol, type, value)
const stbl stable;
const char *symbol;
union usy_value *value;
int *type;
/*
 * Get the value of a symbol.
 * Entry:
 *	TABLE	is a symbol table.
 *	SYMBOL	is the symbol of interest.
 * Exit:
 *	If the symbol is defined, then
 *		VALUE	contains the value given to the symbol.
 *		TYPE	is the data type of the symbol.
 *		The return value is TRUE
 *	else
 *		The return value is FALSE.
 *		TYPE is SYMT_UNDEFINED.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	struct ste *sp;

	if (! M_table)
		usy_init ();
/*
 * If this symbol is undefined, just quit.
 */
	sp = usy_find_symbol (table, symbol);
	if (! sp)
	{
	 	*type = SYMT_UNDEFINED;
		return (FALSE);
	}
/*
 * OK, we've found the symbol.
 */
 	*type = sp->ste_type;
	if (sp->ste_indirect)
		usy_g_indirect (sp, value);
	else
		*value = sp->ste_v;
	return (TRUE);
}







zbool
usy_defined (stable, symbol)
const stbl stable;
const char *symbol;
/*
 * See if this symbol is defined.
 * Entry:
 *	TABLE	is a symbol table.
 *	SYMBOL	is a symbol of interest.
 * Exit:
 *	TRUE is returned iff the symbol is defined in the given table.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	if (! M_table)
		usy_init ();
	return (usy_find_symbol (table, symbol) != 0);
}





#ifdef notdef
static void
usy_dcall (sym, op, type, value)
struct ste *sym;
int op, type;
union usy_value *value;
/*
 * Call the daemons associated with this symbol.
 * Entry:
 *	SYM	is the symbol of interest.
 *	OP	is the operation being performed.
 *	TYPE	is the type of any new value.
 *	VALUE	is a new value for SOP_WRITE.
 * Exit:
 *	The daemons have been called.
 */
{
	struct sdaemon *sdp;

	for (sdp = sym->ste_daemons; sdp; sdp = sdp->sd_next)
		(*sdp->sd_func) (sym->ste_sym, sdp->sd_arg, op, sym->ste_type,
			&sym->ste_v, type, value);
}
#endif





static struct ste *
usy_new_entry (table, symbol)
struct symbol_table *table;
char *symbol;
/*
 * Construct a new symbol table entry for this symbol.
 * Entry:
 *	TABLE 	is the table into which to place the symbol.
 *	SYMBOL	is the actual symbol.
 * Exit:
 *	A pointer to the new ste is returned.
 */
{
	struct ste *sp = (struct ste *) getvm (sizeof (struct ste));
	int slot;

	zfill (sp, sizeof (struct ste));
	sp->ste_sym = zl_zapcase (usy_pstring (symbol));
	slot = HASH (sp->ste_sym);
	sp->ste_next = table->st_ste[slot];
	table->st_ste[slot] = sp;
	table->st_nsym++;
	return (sp);
}





static void
usy_s_indirect (sym, v)
struct ste *sym;
const union usy_value *v;
/*
 * Set this indirect symbol's value from v.
 */
{
	char *ip = sym->ste_v.us_v_ptr;
        int i;
	
	switch (sym->ste_type)
	{
	   case SYMT_FLOAT:
		* (float *) ip = v->us_v_float;
		break;
	   case SYMT_INT:
	   	* (int *) ip = v->us_v_int;
		break;
	   case SYMT_STRING:
	   	strncpy (ip, v->us_v_ptr, sym->ste_length);
		if ((i = strlen (v->us_v_ptr)) > sym->ste_length)
			ip[sym->ste_length - 1] = '\0';
		break;
	   case SYMT_DATE:
	   	* (date *) ip = v->us_v_date;
		break;
	   case SYMT_BOOL:
	   	* (zbool *) ip = v->us_v_int;
		break;
	   case SYMT_SYMBOL:
	   case SYMT_POINTER:
	   	* (char **) ip = v->us_v_ptr;
		break;
	}
}





void
usy_s_symbol (stable, symbol, type, value)
stbl stable;
const char *symbol;
int type;
const union usy_value *value;
/*
 * Define a symbol.
 * Entry:
 *	TABLE	is an existing symbol table.
 *	SYMBOL	is the symbol to be defined.
 *	TYPE	is the type to be given to the symbol.
 *	VALUE	is the value to give to the symbol.
 * Exit:
 *	The symbol has been defined.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	struct ste *sp;

	if (! M_table)
		usy_init ();
	S_nset++;
/*
 * First, see if this symbol is already defined.
 * If this symbol has a string value, free up the memory occupied by the
 * old value.
 */
	table = usy_extr_table (table, symbol);
	if (! table)
		return;
 	if ((sp = usy_find_symbol (table, symbol)))
	{
#ifdef notdef
		if (sp->ste_daemons)
			usy_dcall (sp, SOP_WRITE, type, value);
#endif
		if (sp->ste_indirect && sp->ste_type != type)
			/* uit_coerce (value, type, sp->ste_type); */
			return;
		if (sp->ste_type == SYMT_STRING && ! sp->ste_indirect)
			usy_rel_string (sp->ste_v.us_v_ptr);
	}
/*
 * Otherwise we need to fix up a new entry.
 */
 	else
		sp = usy_new_entry (table, symbol);
/*
 * Put in the new entry.
 */
	if (sp->ste_indirect)
		usy_s_indirect (sp, value);
	else
	{
		sp->ste_type = type;
		sp->ste_v = *value;
		if (type == SYMT_STRING)
			sp->ste_v.us_v_ptr = usy_string (value->us_v_ptr);
	}
}





stbl
usy_g_stbl (name)
const char *name;
/*
 * Look up the tag of a symbol table.
 * Entry:
 *	NAME	is the name of a symbol table.
 * Exit:
 *	If the table exists, its tag is returned.  Otherwise NULL is returned.
 */
{
	struct ste *sp;

	if (! M_table)
		usy_init ();
/*
 * The master table does not recursively refer to itself, so we need to
 * check for it explicitly here.
 */
 	if (! strcmp (name, "usym$master_table"))
		return ((stbl) M_table);
/*
 * Make sure that if we found the name, it really is a symbol table.
 */
	sp = usy_find_symbol (M_table, name);
 	if (! sp || sp->ste_type != SYMT_SYMBOL)
		return ((stbl) NULL);
/*
 * OK, we have it.
 */
 	return ((stbl) sp->ste_v.us_v_ptr);
}




void
usy_c_indirect (stable, symbol, target, type, length)
stbl stable;
const char *symbol;
const void *target;
int type, length;
/*
 * Create an indirect symbol.
 * Entry:
 *	TABLE	is the table into which the symbol is to go.
 *	SYMBOL	is the symbol of interest.
 *	TARGET	is the actual storage for this symbol.
 *	TYPE	is the data type of the symbol.
 *	LENGTH	is the storage length (string only).
 * Exit:
 *	The indirect symbol has been set up.  If this symbol already existed,
 *	the old value is destroyed.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	struct ste *sp;

	if (! M_table)
		usy_init ();
	if (! table)
		return;
/*
 * Clear out a definition of this symbol, if it exists.
 */
 	if (usy_defined (stable, symbol))
		usy_z_symbol (stable, symbol);
/*
 * Create a new entry, and fill it in.
 */
 	sp = usy_new_entry (table, symbol);
	sp->ste_type = type;
	sp->ste_v.us_v_ptr = (char *) target;
	sp->ste_length = length;
	sp->ste_indirect = TRUE;
}





static void
usy_sort (list, nste)
struct ste **list;
int nste;
/*
 * Sort this list of symbol table entry pointers.
 */
{
	int ns, npass;
/*
 * For now, a slow, n^2 bubble sort.
 */
 	for (npass = 0; npass < nste; npass++)
		for (ns = 0; ns < nste - npass - 1; ns++)
			if (strcmp (list[ns]->ste_sym, list[ns + 1]->ste_sym)
					> 0)
			{
				struct ste *t = list[ns];
				list[ns] = list[ns + 1];
				list[ns + 1] = t;
			}
}





int
usy_traverse (stable, func, arg, sort)
const stbl stable;
int (*func) ();
long arg;
zbool sort;
/*
 * The old interface to usy_search.
 */
{
	return (usy_search (stable, func, arg, sort, (char *) 0));
}




int
usy_search (stable, func, arg, sort, re)
const stbl stable;
int (*func) ();
long arg;
zbool sort;
char *re;
/*
 * Perform a traversal of this table, possibly controlled by a regular
 * expression.
 * Entry:
 *	TABLE	is a symbol table.
 *	FUNC	is an integer function, that expects to be called as:
 *
 *			(*func) (symbol, type, value, arg)
 *			char *symbol;
 *			int type;
 *			union usy_value *value;
 *			long arg;
 *
 *	SORT	is true iff the list is to be sorted.
 *	RE	is a regular expression to be applied to the names of the
 *		symbols in the table.  If it is non-NULL, only symbols 
 *		which match the expression will be passed through.
 *
 * Action:
 *	The given function will be called once for every entry in the
 *	symbol table until (1) the table has been exhausted, or (2) the
 *	function returns a value of FALSE.  The final return value of
 *	FUNC will be the return value here.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	int slot = 0, ret, nsy = 0, i;
	struct ste *sp;
	struct ste **list =
		(struct ste **) getvm (table->st_nsym * sizeof (struct ste *));
	union usy_value v;
/*
 * If the table is empty, why bother?
 */
	if (table->st_nsym == 0)
		return (0);
/*
 * If there is an RE, compile it.
 */
	if (re)
	{
		char *stat = zl_re_comp (re);
                if (stat)
                        re = NULL;
	}
/*
 * Go through, and build up a list of all entries in this table.
 */
 	for (; slot < HASH_MOD; slot++)
		for (sp = table->st_ste[slot]; sp; )
		{
                        if (! re || zl_re_exec (sp->ste_sym))
				list[nsy++] = sp;
			sp = sp->ste_next;
		}
/*
 * Sort the list, if necessary.
 */
 	if (sort)
		usy_sort (list, nsy);
/*
 * Now we go through and execute the handler for each one.  While this
 * separation eliminates the problems that result when the handler deletes
 * the symbol of interest, we are still vulnerable to other changes, in
 * particular, the deletion of a symbol that has not yet been passed to
 * the handler.
 */
	ret = 0;
	for (i = 0; i < nsy; i++)
	{
		sp = list[i];
		if (sp->ste_indirect)
			usy_g_indirect (sp, &v);
		if ((ret = (*func) (sp->ste_sym, sp->ste_type,
			sp->ste_indirect ? &v : &sp->ste_v, arg)) == 0)
		{
			relvm (list);
			return (0);
		}
	}
/*
 * Clean up and quit.
 */
	relvm (list);
	return (ret);
}





static struct symbol_table *
usy_extr_table (table, symbol)
struct symbol_table *table;
char *symbol;
/*
 * Find out which symbol table is *really* wanted here, by looking for
 * symbols with the form table:symbol.
 */
{
	char *colon;
/*
 * If there is no colon in this symbol name, then just return the table
 * that was passed to us.
 */
	if ((colon = strchr (symbol, ':')) == NULL)
		return (table);
/*
 * Separate out the table name, and look it up.  This causes a recursive
 * call, but, since we have simplified things, it should work.
 */
	*colon = '\0';
	if ((table = (struct symbol_table *) usy_g_stbl (symbol)) == NULL)
		/* ui_error ("Unknown symbol table: '%s'", symbol); */
		return (NULL);
/*
 * Fix up the symbol name to be without the table part, and return.
 */
	strcpy (symbol, colon + 1);
	return (table);
}




#ifdef notdef		
/*
 * Character names of symbol table types.  This is dependant on the
 * symbols defined in ui_symbol.h.
 */
static char *St_types[] =
{
	"Float",	/* SYMT_FLOAT	*/
	"Integer",	/* SYMT_INT	*/
	"String",	/* SYMT_STRING	*/
	"Date",		/* SYMT_DATE	*/
	"Boolean",	/* SYMT_BOOL	*/
	"Stbl",		/* SYMT_SYMBOL	*/
	"Pointer"	/* SYMT_POINTER */
};
# define MAX_SYMT 6


static int
usy_list_symbol (sym, type, value, arg)
char *sym;
int type, arg;
union usy_value *value;
/*
 * List off the contents of this symbol table entry.
 */
{
	struct symbol_table *tp;
	ZebTime zt;
/*
 * List the symbol name.
 */
	printf ("\t%-20s\t", sym);
/*
 * List off its type.
 */
	printf ("%-10s ", (type <= MAX_SYMT) ?
		St_types[type] : "????");
/*
 * Do the printing.
 */
 	switch (type)
	{
	   case SYMT_FLOAT:
	   	printf ("%.2f", value->us_v_float);
		break;
	   case SYMT_INT:
	   	printf ("%d", value->us_v_int);
		break;
	   case SYMT_STRING:
	   	printf ("\"%s\"", value->us_v_ptr);
		break;
	   case SYMT_DATE:
		TC_UIToZt (&value->us_v_date, &zt);
		printf ("%s", TC_AscTime (&zt, TC_Full));
		break;
	   case SYMT_BOOL:
	   	printf (value->us_v_int ? "True" : "False");
		break;
	   case SYMT_SYMBOL:
		tp = (struct symbol_table *) value->us_v_ptr;
		printf ("(%s)", tp->st_name);
		break;
	   case SYMT_POINTER:
	   	printf ("0x%X", value->us_v_int);
		break;
	   default:
	   	printf ("????????");
	}
	printf ("\n");
	return (TRUE);
}





void
usy_dump_table (stable)
const stbl stable;
/*
 * Print out a listing of this table.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	
	printf ("\nListing of table %s (%d symbols)\n", table->st_name,
		table->st_nsym);
	usy_traverse (stable, usy_list_symbol, (long) 0, TRUE);
}
#endif




#ifdef notdef
int
usy_daemon (stable, symbol, ops, func, arg)
stbl stable;
const char *symbol;
int ops;
int (*func) ();
char * arg;
/*
 * Define a daemon for this symbol.
 * Entry:
 *	TABLE	is the table containing the symbol.
 *	SYMBOL	is the name of the actual symbol.
 *	OPS	is a mask of operations of interest:
 *		SOP_READ	Any symbol reference.
 *		SOP_WRITE	Whenever the symbol is changed.
 *		SOP_DESTROY	When the symbol is destroyed.
 *	FUNC	is the function to be called.
 *	ARG	is an argument to be passed to that function.
 * Exit:
 *	If SYMBOL exists then
 *		the daemon is established.
 *		the return value is TRUE
 *	else
 *		no daemon is established.
 *		the return value is FALSE.
 *
 * The daemon function is called as:
 *
 *	(*func) (symbol, arg, op, oldtype, oldvalue, newtype, newvalue)
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	struct ste *sym = usy_find_symbol (table, symbol);
	struct sdaemon *sdp;
/*
 * No symbol ==> do nothing.
 */
	if (! sym)
		return (FALSE);
/*
 * Get a new daemon structure and link it in.
 */
	sdp = NEW (struct sdaemon);
	sdp->sd_func = func;
	sdp->sd_arg = arg;
	sdp->sd_next = sym->ste_daemons;
	sym->ste_daemons = sdp;
	return (TRUE);
}




int
usy_z_daemon (stable, symbol, ops, func, arg)
stbl stable;
const char *symbol;
int ops;
int (*func) ();
char * arg;
/*
 * Remove all symbol daemons that match these args.
 * Entry:
 *	TABLE	is the symbol table containing the symbol.
 *	SYMBOL	Is the name of the actual symbol to be affected.
 *	OPS	is the list of operations handled by this daemon.
 *	FUNC	is the daemon function.
 *	ARG	is the argument to be passed to this daemon.
 * Exit:
 *	All daemons matching these arguments have been removed.  The return
 *	value is the number of daemons which were removed.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	struct ste *sym = usy_find_symbol (table, symbol);
	struct sdaemon *sdp, *next, *last;
	int nzap = 0;
/*
 * Make sure that the symbol actually exists.
 */
	if (! sym)
		return (0);
/*
 * Now step through the structures, removing the ones that match.
 */
	last = next = sym->ste_daemons;
	for (sdp = next; sdp; sdp = next)
	{
		next = sdp->sd_next;
		if (sdp->sd_func == func && sdp->sd_arg == arg)
		{
			nzap++;
			if (sdp == sym->ste_daemons)
				sym->ste_daemons = next;
			else
				last->sd_next = next;
			relvm (sdp);
		}
		else
			last = sdp;
	}
/*
 * Done.
 */
 	return (nzap);
}
#endif


