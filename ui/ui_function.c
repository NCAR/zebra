/* 10/88 jc */
/*
 * Function call handling.
 */
# include <math.h>
# include "ui.h"
# include "ui_expr.h"
# include "ui_error.h"
# include "ui_globals.h"


/*
 * These structures represent functions.
 */
# define MAXARG 5	/* Max number of arguments to a function	*/
# define MAXFNAME 40	/* Maximum function name length.		*/

struct func
{
	char f_name[MAXFNAME];	/* The name of this function		*/
	int	f_narg;		/* Number of arguments expected		*/
	int	f_types[MAXARG]; /* The types of each expected argument */
	int	f_flags;	/* Flags				*/
	int	(*f_func) ();	/* The actual function to be called	*/
};

/*
 * Flag field stuff.
 */
# define FF_HARD	0x01	/* Hard-coded function 		*/


/*
 * This table holds all of the functions known to us at compile-time.
 */
int uf_sqrt (), uf_exp (), uf_defined (), uf_stbl (), uf_concat ();
int uf_quote ();
int uf_cos (), uf_sin (), uf_tan (), uf_contains (), uf_substring ();

static struct func
Func_tbl[] =
{
  { "concat",	2,	{ SYMT_STRING, SYMT_STRING },	FF_HARD, uf_concat },
  { "contains",	2,	{ SYMT_STRING, SYMT_STRING },	FF_HARD, uf_contains},
  { "cos",	1,	{ SYMT_FLOAT },			FF_HARD, uf_cos },
  { "defined",	1,	{ SYMT_STRING },		FF_HARD, uf_defined },
  { "exp",	1,	{ SYMT_FLOAT },			FF_HARD, uf_exp },
  { "quote",	1,	{ SYMT_STRING },		FF_HARD, uf_quote },
  { "sin",	1,	{ SYMT_FLOAT },			FF_HARD, uf_sin },
  { "sqrt",	1,	{ SYMT_FLOAT },			FF_HARD, uf_sqrt },
  { "substring", 2,	{ SYMT_STRING, SYMT_STRING },	FF_HARD, uf_substring},
  { "symbol_table", 1,	{ SYMT_STRING },		FF_HARD, uf_stbl },
  { "tan",	1,	{ SYMT_FLOAT },			FF_HARD, uf_tan },
  { ___, 	___, 	___, 				___,	 ___	     }
};


/*
 * The function table.
 */
static stbl F_table = NULL;




uf_init ()
/*
 * Initialize the function table.
 */
{
	union usy_value v;
	struct func *fp;
/*
 * First, create our table.
 */
 	F_table = usy_c_stbl ("ui$function_table");
/*
 * Now add all of the hardcoded functions.
 */
 	for (fp = Func_tbl; fp->f_func; fp++)
	{
		v.us_v_ptr = (char *) fp;
		usy_s_symbol (F_table, fp->f_name, SYMT_POINTER, &v);
	}
}







uf_eval (name, arglist, v, type)
char *name;
struct parse_tree *arglist;
union usy_value *v;
int *type;
/*
 * Evaluate a function call.
 */
{
	int ftype, argt[MAXARG], narg;
	union usy_value fv, argv[MAXARG];
	struct func *fp;
/*
 * Look up the function.
 */
	if (! usy_g_symbol (F_table, name, &ftype, &fv))
		ui_error ("Function '%s' does not exist", name);
	fp = (struct func *) fv.us_v_ptr;
/*
 * Put together and check out the argument list.
 */
	narg = uf_get_args (fp, arglist, argv, argt);
/*
 * Perform the invocation.
 */
	(*fp->f_func) (narg, argv, argt, v, type);
}




int
uf_get_args (fp, arglist, argv, argt)
struct func *fp;
struct parse_tree *arglist;
union usy_value *argv;
int *argt;
/*
 * Deal with the argument list for this function call.
 */
{
	int arg;
/*
 * The case of zero args is easy to deal with; let's do it now.
 */
 	if (fp->f_narg == 0)
	{
		if (arglist)
			ui_error ("Function '%s' takes no arguments",
				fp->f_name);
		return;
	}
/*
 * OK, let's work through the args.
 */
	for (arg = 0; arg < fp->f_narg; arg++)
	{
		if (! arglist)
			ui_error ("Not enough args for '%s' -- need %d",
				fp->f_name, fp->f_narg);
		ue_eval (arglist->pt_left, argv, argt);
		if (*argt != fp->f_types[arg])
			uit_coerce (argv, *argt, fp->f_types[arg]);
		argv++;
		argt++;
		arglist = arglist->pt_right;
	}
/*
 * Make sure we used them all up.
 */
 	if (arglist)
		ui_error ("Excess args for '%s' -- need %d\n", fp->f_name,
			fp->f_narg);
	return (arg);
}




uf_def_function (name, narg, argt, func)
char *name;
int narg, *argt, (*func) ();
/*
 * Define a function.
 * Entry:
 *	NAME is the name of the function to be defined.
 *	NARG is the number of arguments taken by that function.
 *	ARGT is an array of types, one for each argument.
 *	FUNC is a pointer to the actual function to be called.
 * Exit:
 *	The function has been defined.
 */
{
	struct func *fp = (struct func *) getvm (sizeof (struct func));
	int arg;
	union usy_value v;
/*
 * Fill in the func structure.
 */
	strcpy (fp->f_name, name);
	fp->f_narg = narg;
	fp->f_flags = 0;
	fp->f_func = func;
	for (arg = 0; arg < narg; arg++)
		fp->f_types[arg] = argt[arg];
/*
 * Now define the function.
 */
 	v.us_v_ptr = (char *) fp;
	usy_s_symbol (F_table, name, SYMT_POINTER, &v);
}
 	




/*
 * Here are functions that we like to provide.  Mostly math functions here.
 */

uf_sqrt (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * Perform square roots.
 */
{
	*rett = SYMT_FLOAT;
	retv->us_v_float = sqrt (argv->us_v_float);
}



uf_exp (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * Perform exponentials.
 */
{
	*rett = SYMT_FLOAT;
	retv->us_v_float = exp (argv->us_v_float);
}




uf_defined (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * Return TRUE iff a symbol is defined.
 */
{
	*rett = SYMT_BOOL;
	retv->us_v_int = usy_defined (Ui_variable_table, argv->us_v_ptr);
}



uf_stbl (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * Return TRUE iff a symbol table exists.
 */
{
	*rett = SYMT_BOOL;
	retv->us_v_int = usy_g_stbl (argv->us_v_ptr) != NULL;
}




uf_concat (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * Concatenate two strings.
 */
{
	char *result = getvm (strlen (argv[0].us_v_ptr) +
		strlen (argv[1].us_v_ptr) + 1);

	*rett = SYMT_STRING;
	strcpy (result, argv[0].us_v_ptr);
	strcat (result, argv[1].us_v_ptr);
	retv->us_v_ptr = usy_string (result);
	relvm (result);
}




uf_sin (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * return sin(x)
 */
{
	*rett = SYMT_FLOAT;
	retv->us_v_float = sin (argv->us_v_float);
}





uf_cos (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * return cos(x)
 */
{
	*rett = SYMT_FLOAT;
	retv->us_v_float = cos (argv->us_v_float);
}





uf_tan (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * return tan(x)
 */
{
	*rett = SYMT_FLOAT;
	retv->us_v_float = tan (argv->us_v_float);
}






uf_substring (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * True if the second parameter is a substring of the first.
 */
{
	int len;
	char *check;
/*
 * Inefficient, but so it goes.
 */
	*rett = SYMT_BOOL;
	retv->us_v_int = TRUE;
	strcpyUC (argv[0].us_v_ptr, argv[0].us_v_ptr);
	strcpyUC (argv[1].us_v_ptr, argv[1].us_v_ptr);
 	len = strlen (argv[1].us_v_ptr);
	for (check = argv[0].us_v_ptr; *check; check++)
		if (! strncmp (check, argv[1].us_v_ptr, len))
			return;
	retv->us_v_int = FALSE;
}
	




uf_contains (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * The function interface to strchr.
 */
{
	*rett = SYMT_BOOL;
	retv->us_v_int = strchr (argv[0].us_v_ptr, *argv[1].us_v_ptr);
}





uf_quote (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * Quote a value as a single parameter.
 */
{
	*rett = SYMT_STRING;
	retv->us_v_ptr = usy_string (argv->us_v_ptr);
}
