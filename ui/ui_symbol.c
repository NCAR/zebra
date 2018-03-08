/*
 * This is the symbol table module.
 */
static char *Rcsid = "$Id: ui_symbol.c,v 1.18 1999-06-25 19:16:30 burghart Exp $";

# include <string.h>

# if defined(SVR4) || defined(AIXV3)
# include <libgen.h>
# endif
# if defined(hpux) || defined(__osf__)
# include <regex.h>
# endif

# include <ctype.h>
# include "ui_symbol.h"
# include "ui_param.h"
# include "ui_error.h"
# include "ui_date.h"
# include "ui_globals.h"

extern void ui_nf_printf (char *fmt, ...);


/*
 * Make sure we won't get into trouble with "const".
 */
# ifndef __STDC__
# ifndef const
# define const
# endif
# endif

extern char *usy_pstring (), *usy_string ();

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
static struct symbol_table *M_table;
static stbl Stat_table;


/*
 * Routines that are used.
 */
char *getvm (), *zapcase ();
struct ste *usy_find_symbol ();
struct symbol_table *usy_extr_table ();

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


/*
 * Statistics stuff.
 */
static int S_nlookup = 0;		/* Number of symbol lookups	*/
static int S_ncoll = 0;			/* Number of hash collisions	*/
static int S_nset = 0;			/* Number of symbol sets	*/


/*
 * Prototypes
 */
int usy_t_daemon (char *symbol, char *arg, int op, int ot, 
		   union usy_value *ov, int nt, union usy_value *nv);
void usy_g_indirect (struct ste *sym, union usy_value *v);
void usy_dcall (struct ste *sym, int op, int type, 
		const union usy_value *value);
void usy_s_indirect (struct ste *sym, const union usy_value *v);
void usy_sort (struct ste **list, int nste);




void
usy_init ()
/*
 * Initialize the symbol table module.
 */
{
	union usy_value v;
/*
 * Create the master table.
 */
	M_table = (struct symbol_table *) getvm (sizeof (struct symbol_table));
	zfill ((char *) M_table, sizeof (struct symbol_table));
	M_table->st_name = usy_pstring ("usym$master_table");
/*
 * Create our symbol table to hold externally (user) accessible stuff.
 */
 	Ui_variable_table = usy_c_stbl ("ui$variable_table");
 	Arg_table = usy_c_stbl ("ui$argument_table");
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
 	usy_st_init (Stat_table);
/*
 * TEST feature: create a daemon for a test symbol.
 */
	v.us_v_int = 0;
	usy_s_symbol (Ui_variable_table, "daemon", SYMT_INT, &v);
	if (! usy_daemon (Ui_variable_table, "daemon", SOP_WRITE,
		usy_t_daemon, 0))
		ui_warning ("Daemon create failed");
	return;
}


int
usy_t_daemon (symbol, arg, op, ot, ov, nt, nv)
char *symbol, *arg;
int op, ot, nt;
union usy_value *ov, *nv;
{
	ui_nf_printf ("SYM CHANGE on '%s': from\n", symbol);
	usy_list_symbol (symbol, ot, ov, 0);
	ui_nf_printf ("    -- to --\n");
	usy_list_symbol (symbol, nt, nv, 0);
	return (1);
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
	table->st_name = zapcase (usy_pstring (name));
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
/*
 * Do a sanity check.
 */
 	if (! usy_find_symbol (M_table, table->st_name))
		c_panic ("Zap on bad stbl '%s'", table->st_name);
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
	symbol = zapcase (usy_string ((char *) symbol));
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
			c_panic ("Unable to find prev ptr for '%s'", symbol);
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
 
 
 
 


struct ste *
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
# ifdef notdef
	char *lsym = zapcase (usy_string (symbol));
# endif
	char lsym[500];
	int i;
	struct ste *sp;
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
	struct ste *sp = usy_find_symbol (table, symbol);
/*
 * If this symbol is undefined, just quit.
 */
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





void
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
	   	v->us_v_int = * (bool *) ip;
		break;
	}
}






bool
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
	return (usy_find_symbol (table, symbol) != 0);
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
	struct ste *sp, *usy_new_entry ();

	S_nset++;
/*
 * First, see if this symbol is already defined.
 * If this symbol has a string value, free up the memory occupied by the
 * old value.
 */
	table = usy_extr_table (table, symbol);
 	if (sp = usy_find_symbol (table, symbol))
	{
		if (sp->ste_daemons)
			usy_dcall (sp, SOP_WRITE, type, value);
		if (sp->ste_indirect && sp->ste_type != type)
			uit_coerce (value, type, sp->ste_type);
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




void
usy_dcall (sym, op, type, value)
struct ste *sym;
int op, type;
const union usy_value *value;
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





void
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
	   	* (bool *) ip = v->us_v_int;
		break;
	   case SYMT_SYMBOL:
	   case SYMT_POINTER:
	   	* (char **) ip = v->us_v_ptr;
		break;
	}
}



		

struct ste *
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
	sp->ste_sym = zapcase (usy_pstring (symbol));
	slot = HASH (sp->ste_sym);
	sp->ste_next = table->st_ste[slot];
	table->st_ste[slot] = sp;
	table->st_nsym++;
	return (sp);
}





void
usy_dump_table (stable)
const stbl stable;
/*
 * Print out a listing of this table.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	int usy_list_symbol ();
	
	ui_printf ("\nListing of table %s (%d symbols)\n", table->st_name,
			table->st_nsym);
	usy_traverse (stable, usy_list_symbol, (long) 0, TRUE);
}



int
usy_list_symbol (sym, type, value, arg)
char *sym;
int type, arg;
union usy_value *value;
/*
 * List off the contents of this symbol table entry.
 */
{
	struct symbol_table *tp;
	char d_buf[40];
/*
 * List the symbol name.
 */
	ui_nf_printf ("\t%-20s\t", sym);
/*
 * List off its type.
 */
# ifdef notdef
 	if (sp->ste_indirect)
	 	ui_printf ("->%-8s ", (sp->ste_type <= MAX_SYMT) ?
				St_types[sp->ste_type] : "????");
	else
# endif
	 	ui_printf ("%-10s ", (type <= MAX_SYMT) ?
				St_types[type] : "????");
# ifdef notdef
/*
 * Now, somehow cope with the value.
 */
 	if (sp->ste_indirect)
		usy_g_indirect (sp, &v);
	else
		v = sp->ste_v;
# endif
/*
 * Do the printing.
 */
 	switch (type)
	{
	   case SYMT_FLOAT:
	   	ui_nf_printf ("%.2f", value->us_v_float);
		break;
	   case SYMT_INT:
	   	ui_nf_printf ("%d", value->us_v_int);
		break;
	   case SYMT_STRING:
	   	ui_nf_printf ("\"%s\"", value->us_v_ptr);
		break;
	   case SYMT_DATE:
		ui_nf_printf ("%s", ud_format_date (d_buf, &value->us_v_date,
					UDF_FULL));
		break;
	   case SYMT_BOOL:
	   	ui_nf_printf (value->us_v_int ? "True" : "False");
		break;
	   case SYMT_SYMBOL:
		tp = (struct symbol_table *) value->us_v_ptr;
		ui_nf_printf ("(%s)", tp->st_name);
		break;
	   case SYMT_POINTER:
	   	ui_nf_printf ("0x%X", value->us_v_int);
		break;
	   default:
	   	ui_nf_printf ("????????");
	}
	ui_printf ("\n");
	return (TRUE);
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
	struct ste *sp = usy_find_symbol (M_table, name);
/*
 * The master table does not recursively refer to itself, so we need to
 * check for it explicitly here.
 */
 	if (! strcmp (name, "usym$master_table"))
		return ((stbl) M_table);
/*
 * Make sure that if we found the name, it really is a symbol table.
 */
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






int
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




int
usy_traverse (stable, func, arg, sort)
const stbl stable;
int (*func) ();
long arg;
bool sort;
/*
 * The old interface to usy_search.
 */
{
	struct symbol_table *table = (struct symbol_table *) stable;
	return (usy_search (stable, func, arg, sort, (char *) 0));
}




int
usy_search (stable, func, arg, sort, re)
const stbl stable;
int (*func) ();
long arg;
bool sort;
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
#if defined (SVR4) || defined (AIXV3)
        /* nothing special for Solaris yet */
#else
#if defined(hpux) || defined(__osf__)
        regex_t *preg;
#else 
	char *re_comp (), *re_exec ();
#endif
#endif /* ifdefs system checks */
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
#if defined (SVR4) || defined (AIXV3)
                char *stat = regcmp(re, (char*)0);
                if (!stat)
                        ui_error ("(BUG) RE comp error: %s", stat);
#else
#if defined(hpux) || defined(__osf__)
                int stat = regcomp ( preg, re, REG_EXTENDED );
                if (stat)
                        ui_error ("(BUG) RE comp error: %d", stat);
#else 
		char *stat = re_comp (re);
                if (stat)
                        ui_error ("(BUG) RE comp error: %s", stat);
#endif
#endif /* ifdefs system checks */
	}
/*
 * Go through, and build up a list of all entries in this table.
 */
	ERRORCATCH
 	for (; slot < HASH_MOD; slot++)
		for (sp = table->st_ste[slot]; sp; )
		{
#if defined (SVR4) || defined (AIXV3)
                        if (! re ||  ! regex (re, sp->ste_sym))
#else
#if defined(hpux) || defined(__osf__)
                        if (! re ||  ! regexec (preg, sp->ste_sym,
                                        (size_t) 0, NULL, 0))
#else
                        if (! re || re_exec (sp->ste_sym))
#endif
#endif /* ifdefs system checks */

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
	ON_ERROR
		relvm (list);
		err_resignal ();
	ENDCATCH
	relvm (list);
	return (ret);
}




void
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





struct symbol_table *
usy_extr_table (table, symbol)
struct symbol_table *table;
char *symbol;
/*
 * Find out which symbol table is *really* wanted here, by looking for
 * symbols with the form table:symbol.
 */
{
	char *colon;
	stbl usy_g_stbl ();
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
		ui_error ("Unknown symbol table: '%s'", symbol);
/*
 * Fix up the symbol name to be without the table part, and return.
 */
	strcpy (symbol, colon + 1);
	return (table);
}






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
