/* 1/87 jc */
/* $Id: ui_expr.c,v 1.12 1998-10-28 21:23:03 corbet Exp $ */
/*
 * Expression handling.
 */
# include <ctype.h>
# include <math.h>

# include "ui.h"
# include "ui_expr.h"
# include "ui_error.h"
# include "ui_globals.h"
# include "ui_date.h"
# include "ui_cstack.h"


/*
 * The expression handler uses a stack of these structures to handle the
 * input stream.  Pushback is done by allocating (and pushing) another
 * one of these.
 */
# define CHLENGTH 132
# define TLEN 200
struct iss
{
	char *iss_sp;			/* Where we are in the stuff.	*/
	struct iss *iss_next;		/* Next structure in the stack	*/
	char iss_stuff[CHLENGTH];	/* The actual stuff to parse.	*/
	int iss_col;			/* Text column number.		*/
};
static struct iss *Iss_lal = 0;		/* Look aside list		*/


# define GET_ISS(ip) { if (Iss_lal) \
	{ (ip) = Iss_lal; Iss_lal = Iss_lal->iss_next; } \
	else (ip) = (struct iss *) getvm (sizeof (struct iss));}
	
# define REL_ISS(ip) { (ip)->iss_next = Iss_lal; Iss_lal = ip; };
/*
 * This is the actual input stack.
 */
static struct iss *Istack = 0;

/*
 * Debug flag.
 */
static bool Edebug = FALSE;

char *getvm ();


/*
 * Forward declarations for the various recursive descent parsing routines.
 */
struct parse_tree *ue_get_node (), *ue_fcall (), *ue_arglist ();
struct parse_tree * ue_expr (), *ue_conj (), *ue_rexpr (), *ue_mexpr ();
struct parse_tree *ue_term (), *ue_efactor (), *ue_factor (), *ue_item ();

# ifdef __STDC__
	void ue_free_result (int, SValue *);
# else
	void ue_free_result ();
# endif

/*
 * The relational operator conversion table.
 */
static struct roptab
{
	char *rt_char;		/* Character representation */
	int  rt_op;		/* Operator number.	*/
} Roptbl[] =
{
	{ ">",	OP_GT	},
	{ "<",	OP_LT	},
	{ "=",	OP_EQ	},
	{ "<>",	OP_NEQ	},
	{ "<=",	OP_LE	},
	{ ">=",	OP_GE	},
	{ ___,	___	}
};

/*
 * The lookaside list for parse tree nodes.
 */
static struct parse_tree *Pt_lal = (struct parse_tree *) 0;


/*
 * Prototypes
 */
void ue_qstring_snarf (char quote, char *string);
void ue_clear (void);
void ue_print_val (union usy_value *v, int type);
int ue_ieval (struct parse_tree *tree, union usy_value *v, int *type);
void ue_arith_op (struct parse_tree *tree, int *type, union usy_value *v);
void ue_do_add (int lt, union usy_value *lv, union usy_value *rv, int *type, 
		union usy_value *v);
void ue_do_sub (int lt, union usy_value *lv, union usy_value *rv, int *type, 
		union usy_value *v);
void ue_do_times (int lt, union usy_value *lv, union usy_value *rv, int *type, 
		  union usy_value *v);
void ue_do_div (int lt, union usy_value *lv, union usy_value *rv, int *type, 
		union usy_value *v);
void ue_do_exp (int lt, union usy_value *lv, union usy_value *rv, int *type, 
		union usy_value *v);
void ue_type_coerce (int *t1, union usy_value *v1, int *t2, 
		     union usy_value *v2);
void ue_log_op (struct parse_tree *tree, int *type, union usy_value *v);
void ue_rel_op (struct parse_tree *tree, int *type, union usy_value *v);
void ue_do_gt (int type, union usy_value *v1, union usy_value *v2, 
	       union usy_value *result);
void ue_do_ge (int type, union usy_value *v1, union usy_value *v2, 
	       union usy_value *result);
void ue_do_lt (int type, union usy_value *v1, union usy_value *v2, 
	       union usy_value *result);
void ue_do_le (int type, union usy_value *v1, union usy_value *v2, 
	       union usy_value *result);
void ue_do_eq (int type, union usy_value *v1, union usy_value *v2, 
	       union usy_value *result);
void ue_do_neq (int type, union usy_value *v1, union usy_value *v2, 
		union usy_value *result);





void
ue_init ()
/*
 * Initialize the expression parser.
 */
{
	usy_c_indirect (Ui_variable_table, "ui$edebug", &Edebug,
			SYMT_BOOL, 0);
}




void
ue_push (string, col)
char *string;
int col;
/*
 * Push back this string to be parsed.
 */
{
	struct iss *ip;
	char *zapcase ();

	if (Edebug)
		ui_printf ("Pushback (%s, %d)\n", string, col);
/*
 * Just allocate a structure and fill it in.
 */
/* 	ip = (struct iss *) getvm (sizeof (struct iss));*/
	GET_ISS(ip);
	ip->iss_stuff[0] = ' ';
	strcpy (ip->iss_stuff + 1, string);
	strcat (ip->iss_stuff, " ");
	/* (void) zapcase (ip->iss_stuff); */
	ip->iss_col = col - 1;
	ip->iss_sp = ip->iss_stuff;
	ip->iss_next = Istack;
	Istack = ip;
}



char
ue_get_char (col)
int *col;
/*
 * Return a single character from the input stream, or 0 if none exists.
 */
{
	struct iss *ip;

	if (! Istack)
		return (0);
	if (*Istack->iss_sp)
	{
		*col = Istack->iss_col + (Istack->iss_sp - Istack->iss_stuff);
		return (*Istack->iss_sp++);
	}
/*
 * This chunk is done; go on to the next one.
 */
 	ip = Istack;
	Istack = Istack->iss_next;
/*	relvm (ip); */
	REL_ISS(ip);
	return (ue_get_char (col));
}




char 
ue_peek ()
/*
 * Look ahead at what the next character will be without actually fetching it.
 */
{
	return (Istack->iss_sp ? *Istack->iss_sp : ' ');
}




void
ue_lookahead (string)
char *string;
/*
 * Pull out the entire pushback stash.
 */
{
	if (Istack->iss_sp)
		strcpy (string, Istack->iss_sp);
	else
		*string = '\0';
}






int
ue_get_token (string, col)
char *string;
int *col;
/*
 * Grab another token, putting it into STRING.  Returns true iff another
 * token was there to grab.
 */
{
	char ch;
	int icol;

	if (Edebug)
		ui_printf ("Get_token... ");
/*
 * Skip any initial white space.
 */
 	while ((ch = ue_get_char (col)) == ' ' || ch == '\t')
		;
	if (ch == 0)
	{
		if (Edebug)
			ui_printf ("EOS\n");
		return (FALSE);
	}
/*
 * Now we need to look at what we got, and snarf up what else is relevant.
 */
 	switch (ch)
	{
	/*
	 * Single character operators, that we know we can return right now.
	 */
	   case '+':
	   case '-':
	   case '*':
	   case '/':
	   case '=':
	   case '(':
	   case ')':
	   case '^':
	   case ',':
	   	string[0] = ch;
		string[1] = '\0';
		if (Edebug)
			ui_printf ("SC op '%c' %d\n", ch, *col);
		return (TRUE);
	/*
	 * Some characters may be single or double character operators.  Check
	 * for that now.
	 */
	   case '>':
	   case '<':
	   	string[0] = ch;
	   	if (ue_peek () == '=' || (ch == '<' && ue_peek () == '>'))
		{
			string[1] = ue_get_char (&icol); string[2] = '\0';
			if (Edebug)
				ui_printf ("DC op '%s'\n", string);
		}
		else
		{
			if (Edebug)
				ui_printf ("SC op '%c'\n", ch);
			string[1] = '\0';
		}
		return (TRUE);
	/*
	 * It could be a quoted string.
	 */
	   case '\"':
	   case '\'':
		string[0] = ch;
	   	ue_qstring_snarf (string[0], string + 1);
		return (TRUE);
	/*
	 * Check for a symbol reference.
	 */
	   case '#':
	   	string[0] = ch;
	   	ue_alnum_snarf (string);
		if (Edebug)
			ui_printf ("Symref '%s'\n", string);
		return (TRUE);
	}
/*
 * Well, none of the special characters appeared.  If this is a digit, we
 * need to snarf up a number.
 *
 * 5/11/87 jc	Allow floating point numbers like ".5".
 */
 	if (isdigit (ch) || ch == '.')
	{
		string[0] = ch;
		if (Edebug)
			ui_printf ("Digit string '%c...'\n", ch);
		return (ue_num_snarf (string + 1));
	}
/*
 * Maybe this is an alphanumeric identifier.
 */
	else if (isalpha (ch) || ch == '_' || ch == '$')
	{
		if (Edebug)
			ui_printf ("Ident string '%c...'\n", ch);
		string[0] = ch;
		return (ue_alnum_snarf (string));
	}
/*
 * Otherwise it is something we don't know how to deal with.
 */
 	else
		ui_cl_error (TRUE, *col, 
			"Bad character in expression: '%c'", ch);
	/* NOTREACHED */
}



int
ue_alnum_snarf (string)
char *string;
/*
 * Grab up an alphanumeric identifier.
 */
{
	char *orig = string;
	int junk;

	string++;
	while ((*string = ue_peek ()) != 0 &&
		(isalnum (*string) || *string == '_' || *string == '$' ||
		 *string == ':'))
	{
		string++;
		ue_get_char (&junk);
	}
	*string = '\0';
/*
 * Date kludge.  Iff (1) We have "today" or "yesterday", and (2) the next
 * character is a comma, we need to continue.
 */
 	if ((! strncmp (orig, "today", 4) || ! strncmp (orig, "yester", 4)) &&
		ue_peek () == ',')
		ue_time_snarf (string);
	return (TRUE);
}



void
ue_qstring_snarf (char quote, char *string)
/*
 * Grab up a quoted string.
 */
{
	char ch, *ostring = string;
	int col;
/*
 * Simply grab up the string.
 */
	while ((ch = ue_get_char (&col)) != quote && ch)
		*string++ = ch;
	if (! ch)
		ui_cl_error (TRUE, col, "Missing close quote");
	*string++ = ch; /* Don't forget the close quote */
	*string = '\0';
	if (Edebug)
		ui_printf ("Qstring: %s\n", ostring - 1);
}





int
ue_time_snarf (string)
char *string;
/*
 * Pull a time spec into string.
 */
{
	int junk;
	char c;
/*
 * Look for a comma here.
 */
 	if ((*string++ = ue_get_char (&junk)) != ',')
		c_panic ("I wasn't given a time spec!");
/*
 * Now work through this.
 */
 	for (;;)
	{
		if ((c = ue_peek ()) == 0)
			break;
		if (isdigit (c) || c == ':')
			*string++ = ue_get_char (&junk);
		else
			break;
	}
	*string = '\0';
	return (TRUE);
}



int
ue_num_snarf (string)
char *string;
/*
 * Soak up a digit string.
 */
{
	bool dot = FALSE;
	int junk;

	for (;;)
	{
		char ch = ue_peek ();
		if (isdigit (ch))
			*string++ = ch;
		else if (ch == '.' && ! dot)
		{
			*string++ = ch;
			dot = TRUE;
		}
		else
			break;
		ue_get_char (&junk);
	}
/*
 * Date kludge: Our number "nn" could be part of nn-mon-yr,hh:mm:ss, or
 * some such.  Let's deal with that.
 */
	*string = '\0';
	if (! dot)	/* No sense for reals */
	 	ue_check_date (string);
	return (TRUE);
}




static int
mcmp (test, month)
char *test, *month;
/*
 * Compare the test string against the month name, in a case-insensitive way.
 */
{
	return (tolower(test[0]) == month[0] &&
		tolower(test[1]) == month[1] &&
		tolower(test[2]) == month[2]);
}




int
ue_check_date (string)
char *string;
/*
 * See if an integer just brought in is the lead part of a date.
 */
{
	static char *months[] = { "jan", "feb", "mar", "apr", "may", "jun",
				  "jul", "aug", "sep", "oct", "nov", "dec"};
	char c, *ostr = string;
	int i, junk;
/*
 * In order for this to work, the next character must be a minus, followed
 * by a month.
 */
 	if (ue_peek () == ',')
		return (ue_time_snarf (string));
 	if (ue_peek () != '-')
		return (FALSE);
/*
 * We do have a minus.  Snarf it and the next three chars, and see what
 * comes through.
 */
	ue_lookahead (string);
	string[4] = '\0';
	for (i = 0; i < 12; i++)
		if (mcmp (ostr + 1, months[i]))
			break;
	if (i >= 12)
	{
		*ostr = '\0';
		return (FALSE);
	}
/*
 * Actually pull the stuff from the input stream.
 */
 	for (i = 0; i < 4; i++)
		*string++ = ue_get_char (&junk);
/*
 * It looks like this is going to work.  Let's see if there is more.
 */
	if ((c = ue_peek ()) == ',')
	{
		ue_time_snarf (string);
		return (FALSE);
	}
	if (c != '-')
		return (FALSE);
/*
 * There is another minus.  Snarf up a number.
 */
 	*string++ = ue_get_char (&junk); /* get the '-' */
	while (isdigit (ue_peek ()))
		*string++ = ue_get_char (&junk);
	*string = '\0';
/*
 * All that is left now is the optional time.
 */
 	if (ue_peek () == ',')
		ue_time_snarf (string);
	return (TRUE);
}






struct parse_tree *
ue_parse (string, col, signal)
char *string;
bool signal;
/*
 * Parse a string into a parse tree.
 * Entry:
 *	STRING	is the actual string of interest.
 *	COL	is the text column number of the string.
 *	SIGNAL	is true iff errors are to be signalled to the user.
 * Exit:
 *	If the string parses successfully then
 *		The return value is a pointer to the parse tree for this
 *		expression.
 *	else
 *		If SIGNAL is true, return will be via the error return
 *			mechanism.
 *		Otherwise, the return value is zero.
 */
{
	char str[132];
	struct parse_tree *pt, *ue_expr ();
/*
 * Store this string away in the input stack.
 */
	ue_clear ();
	ue_push (string, col);
/*
 * Now do the parse.
 */
	pt = ue_expr (signal);
	if (Edebug)
	{
		ue_dump_tree (pt);
		ui_printf ("\n");
	}
/*
 * If something is left in the input stream, it means that it failed to
 * parse.  Thus, we should complain.
 */
	if (ue_get_token (str, &col) && signal && pt)
		ui_cl_error (TRUE, col, 
			"Error parsing expression at '%s'", str);
	return (pt);
}




struct parse_tree *
ue_get_node ()
/*
 * Return a pointer to a newly-allocated tree node.
 */
{
	struct parse_tree *ret;
/*
 * Allocate a node off the lookaside list if possible; otherwise create
 * a new one.
 */
 	if (ret = Pt_lal)
		Pt_lal = Pt_lal->pt_left;
	else
		ret = (struct parse_tree *) getvm (sizeof (struct parse_tree));
/*
 * Clear it out and pass it back.
 */
	ret->pt_ntype = ret->pt_dtype = -1;
	ret->pt_left = ret->pt_right = 0;
	return (ret);
}





void
ue_clear ()
/*
 * Clear out the pushback stack.
 */
{
	struct iss *ip;
	
	while (Istack)
	{
		ip = Istack;
		Istack = Istack->iss_next;
		relvm (ip);
	}
}



/*
 * Recursive descent routines start here.
 */

struct parse_tree *
ue_expr (signal)
bool signal;
/*
 * This is the top level of a parse hierarchy.  It differs from the rest
 * in this particular respect: if "ret" is passed in nonzero, it is taken
 * as a pointer to a tree node to use.  Otherwise one is allocated.
 *
 *	<expr> :== <conj> | <conj> '|' <conj>
 */
{
	struct parse_tree *node, *left;
	char token[TLEN];
	int col;
/*
 * In any case, we need to be able to parse a conjunction.
 */
 	if ((left = ue_conj (signal)) == 0)
		return (0);
/*
 * Now, see if another token exists.  If it does, we can try for a logical
 * OR operator.
 */
 	if (ue_get_token (token, &col) == 0)
		return (left);
	if (strcmp (token, "|") && strcmp (token, "or"))
	{
		ue_push (token, col);
		return (left);
	}
/*
 * OK, we do in fact have a disjunction here.  Allocate a node structure,
 * fill it in, and parse the rest of the expression.
 */
 	node = ue_get_node ();
	node->pt_ntype = NT_OPER;
	node->pt_dtype = OP_OR;
	node->pt_left = left;
	if ((node->pt_right = ue_expr (signal)) == 0)
	{
		ue_rel_tree (node);
		return (0);
	}
	return (node);
}






struct parse_tree *
ue_conj (signal)
bool signal;
/*
 *	<conj> ::= <rexpr> | <rexpr> '&' <conj>
 */
{
	struct parse_tree *node, *left;
	char token[TLEN];
	int col;
/*
 * In any case, we need to be able to parse a relational expression
 */
 	if ((left = ue_rexpr (signal)) == 0)
		return (0);
/*
 * Now, see if another token exists.  If it does, we can try for a logical
 * AND operator.
 */
 	if (ue_get_token (token, &col) == 0)
		return (left);
	if (strcmp (token, "&") && strcmp (token, "and"))
	{
		ue_push (token, col);
		return (left);
	}
/*
 * OK, we do in fact have a conjunction here.  Allocate a node structure,
 * fill it in, and parse the rest of the expression.
 */
 	node = ue_get_node ();
	node->pt_ntype = NT_OPER;
	node->pt_dtype = OP_AND;
	node->pt_left = left;
	if ((node->pt_right = ue_conj (signal)) == 0)
	{
		ue_rel_tree (node);
		return (0);
	}
	return (node);
}




struct parse_tree *
ue_rexpr (signal)
bool signal;
/*
 * Parse a relational expression.
 *
 *	<rexpr> ::= <mexpr> | <mexpr> <relop> <mexpr>
 */
{
	struct parse_tree *node, *left;
	struct roptab *rp = Roptbl;
	char token[TLEN];
	int col;
/*
 * Grab up a mathematical expression.
 */
 	if ((left = ue_mexpr (signal)) == 0)
		return (0);
/*
 * Grab up a token, and try to find it in the table.
 */
	if (! ue_get_token (token, &col))
		return (left);
	for (; rp->rt_char; rp++)
		if (! strcmp (token, rp->rt_char))
			break;
	if (! rp->rt_char)
	{
		ue_push (token, col);
		return (left);
	}
/*
 * OK, we found a relop.  Allocate a new tree node and fill it in.
 */
 	node = ue_get_node ();
	node->pt_ntype = NT_OPER;
	node->pt_dtype = rp->rt_op;
	node->pt_left = left;
/*
 * Now try to parse another math expr for the rhs.
 */
 	if ((node->pt_right = ue_mexpr (signal)) == 0)
	{
		ue_rel_tree (node);
		return (0);
	}
	return (node);
}




struct parse_tree *
ue_mexpr (signal)
bool signal;
/*
 *	<mexpr> ::= <term> | <term> <addop> <mexpr>
 */
{
	struct parse_tree *node, *left;
	char token[TLEN];
	int col;
/*
 * In any case, we need to be able to parse a term.
 */
 	if ((left = ue_term (signal)) == 0)
		return (0);
/*
 * Now, see if another token exists.  If it does, we can try for an addop.
 */
 	if (ue_get_token (token, &col) == 0)
		return (left);
	if (strcmp (token, "+") && strcmp (token, "-"))
	{
		ue_push (token, col);
		return (left);
	}
/*
 * OK, we do in fact have a math expression here.  Allocate a node structure,
 * fill it in, and parse the rest of the expression.
 */
 	node = ue_get_node ();
	node->pt_ntype = NT_OPER;
	node->pt_dtype = (token[0] == '+') ? OP_PLUS : OP_MINUS;
	node->pt_left = left;
	if ((node->pt_right = ue_mexpr (signal)) == 0)
	{
		ue_rel_tree (node);
		return (0);
	}
	return (node);
}






struct parse_tree *
ue_term (signal)
bool signal;
/*
 *	<term> ::= <efactor> | <efactor> <mulop> <term>
 */
{
	struct parse_tree *node, *left;
	char token[TLEN];
	int col;
/*
 * In any case, we need to be able to parse an efactor.
 */
 	if ((left = ue_efactor (signal)) == 0)
		return (0);
/*
 * Now, see if another token exists.  If it does, we can try for a mulop.
 */
 	if (ue_get_token (token, &col) == 0)
		return (left);
	if (strcmp (token, "*") && strcmp (token, "/"))
	{
		ue_push (token, col);
		return (left);
	}
/*
 * OK, we do in fact have a term here.  Allocate a node structure,
 * fill it in, and parse the rest of the expression.
 */
 	node = ue_get_node ();
	node->pt_ntype = NT_OPER;
	node->pt_dtype = (token[0] == '*') ? OP_TIMES : OP_DIVIDE;
	node->pt_left = left;
	if ((node->pt_right = ue_term (signal)) == 0)
	{
		ue_rel_tree (node);
		return (0);
	}
	return (node);
}






struct parse_tree *
ue_efactor (signal)
bool signal;
/*
 *	<efactor> ::= <factor> | <factor> '^' <factor>
 */
{
	struct parse_tree *node, *left;
	char token[TLEN];
	int col;
/*
 * In any case, we need to be able to parse a factor.
 */
 	if ((left = ue_factor (signal)) == 0)
		return (0);
/*
 * Now, see if another token exists.  If it does, we can try for an '^'.
 */
 	if (ue_get_token (token, &col) == 0)
		return (left);
	if (strcmp (token, "^"))
	{
		ue_push (token, col);
		return (left);
	}
/*
 * OK, we do in fact have an efactor here.  Allocate a node structure,
 * fill it in, and parse the rest of the expression.
 */
 	node = ue_get_node ();
	node->pt_ntype = NT_OPER;
	node->pt_dtype = OP_EXP;
	node->pt_left = left;
	if ((node->pt_right = ue_factor (signal)) == 0)
	{
		ue_rel_tree (node);
		return (0);
	}
	return (node);
}





struct parse_tree *
ue_factor (signal)
bool signal;
/*
 *	<factor> ::= <item> | '(' <expr> ')'
 */
{
	struct parse_tree *left;
	char token[TLEN];
	int col;
/*
 * Grab a token and see if it is a left parenthesis.
 */
 	if (! ue_get_token (token, &col))
	{
		if (signal)
		{
			ERRORCATCH
				ui_error ("Unexpected end of expression");
			ENDCATCH
		}
		return (0);
	}
	if (! strcmp (token, "("))
	{
	/*
	 * Grab up an expression, then verify that the close paren is there.
	 */
	 	if ((left = ue_expr (signal)) == 0)
			return (0);
		if (! ue_get_token (token, &col))
		{
			if (signal)
				ui_cl_error (FALSE, col, "Missing ')'");
			return (0);
		}
		if (strcmp (token, ")"))
		{
			if (signal)
				ui_cl_error (FALSE, col,
					"Got '%s' when expecting ')'", token);
			ue_rel_tree (left);
			return (0);
		}
		return (left);
	}
/*
 * OK, the token was not a parenthesis.  Push it back, and try for an item.
 */
 	ue_push (token, col);
	return (ue_item (signal));
}




struct parse_tree *
ue_item (signal)
bool signal;
/*
 * Here we look for an actual leaf item in the tree.
 *
 *	<item> ::= <constant> | <symref> | fcall
 */
{
	struct parse_tree *node;
	char token[TLEN], token2[TLEN];
	int col, col2;
/*
 * Let's go ahead and try to allocate a node structure.  Then we can run
 * the next token through the interpreter and see what comes out.
 *
 * Also: Make a special case for '-', which is interpreted as a unary
 * minus sign.
 */
	if (! ue_get_token (token, &col))
		return (0);
	node = ue_get_node ();
	if (! strcmp (token, "-") || ! strcmp (token, "not"))
	{
		node->pt_ntype = NT_OPER;
		node->pt_dtype = OP_NEGATE;
		if ((node->pt_left = ue_factor (signal)) == 0)
		{
			relvm (node);
			return (0);
		}
		return (node);
	}
/*
 * Look and see if the next token is an open paren.  If so, the only
 * legal interpretation is that this is a function call.
 */
	if (ue_get_token (token2, &col2))
	{
		if (! strcmp (token2, "("))
			return (ue_fcall (node, token, signal));
		ue_push (token2, col2);
	}
/*
 * Not a function call.  Run it through the interpreter, and see what we
 * have got.
 */
	uit_interp (token, &node->pt_dtype, &(node->pt_v));
/*
 * If we have a non-string value, all is done.
 */
 	if (node->pt_dtype != SYMT_STRING)
		node->pt_ntype = NT_CONST;
/*
 * A string might be a constant too, if it is quoted.
 */
	else if (token[0] == '\"' || token[0] == '\'')
	{
		uip_dequote (token);
		node->pt_ntype = NT_CONST;
		node->pt_dtype = SYMT_STRING;
		node->pt_v.us_v_ptr = usy_string (token);
	}
/*
 * String values are returned as symbol references.  However, make sure we
 * really have a reasonable variable value.
 */
 	else if (isalpha(token[0]) || token[0] == '_' || token[0] == '$')
	{
		node->pt_ntype = NT_SYM;
		node->pt_v.us_v_ptr = usy_string (token);
	}
	else
	{
		relvm (node);
		if (signal)
			ui_cl_error (FALSE, col,
			"Found '%s' when expecting a constant or identifier",
					token);
		return (0);
	}
	return (node);
}




struct parse_tree *
ue_fcall (node, name, signal)
struct parse_tree *node;
char *name;
bool signal;
/*
 * Parse a function call.
 *
 * <fcall> :== <name> () | ( <arglist> )
 */
{
	int col;
	char token[TLEN];
/*
 * Start by putting this node entry together.
 */
 	node->pt_ntype = NT_FCALL;
	node->pt_v.us_v_ptr = usy_string (name);
/*
 * If the following token is a close paren, we are essentially done.
 */
	if (! ue_get_token (token, &col))
	{
		if (signal)
			ui_cl_error (FALSE, col, "Missing ) in function call");
		ue_rel_tree (node);
		return (NULL);
	}
	if (! strcmp (token, ")"))
	{
		node->pt_left = node->pt_right = NULL;
		return (node);
	}
	ue_push (token, col);
/*
 * No close paren.  Time to start parsing the arglist.
 */
	node->pt_left = NULL;
 	if ((node->pt_right = ue_arglist (signal)) == NULL)
	{
		ue_rel_tree (node);
		return (NULL);
	}	
/*
 * Now there really ought to be a close paren.  If not, we complain.
 */
	if (! ue_get_token (token, &col))
	{
		if (signal)
			ui_cl_error (FALSE, col,
				"Missing close paren in function call");
		ue_rel_tree (node);
		return (NULL);
	}	
	if (strcmp (token, ")"))
	{
		if (signal)
			ui_cl_error (FALSE, col,
	  "Found '%s' when expecting close paren in function call", token);
	  	ue_rel_tree (node);
		return (NULL);
	}
	return (node);
}	




struct parse_tree *
ue_arglist (signal)
bool signal;
/*
 * Deal with function call argument lists.
 *
 * <arglist> :== <expr> | <expr> , <arglist>
 */
{
	char token[TLEN];
	int col;
	struct parse_tree *node = ue_get_node ();
/*
 * We expect to see an expression first, so let's parse that.
 */
	node->pt_ntype = NT_FCARG;
 	if ((node->pt_left = ue_expr (signal)) == 0)
	{
		ue_rel_tree (node);
		return (0);
	}
	node->pt_right = NULL;
/*
 * If the next character is a comma, the arglist continues.
 */
	if (! ue_get_token (token, &col))
		return (node);
	if (strcmp (token, ","))
	{
		ue_push (token, col);
		return (node);
	}
	if ((node->pt_right = ue_arglist (signal)) == 0)
	{
		ue_rel_tree (node);
		return (0);
	}
	return (node);
}




void
ue_rel_tree (tree)
struct parse_tree *tree;
/*
 * Deallocate this entire tree.
 */
{
	if (tree->pt_left)
		ue_rel_tree (tree->pt_left);
	if (tree->pt_right)
		ue_rel_tree (tree->pt_right);
	if (tree->pt_ntype == NT_SYM || tree->pt_ntype == NT_FCALL ||
		(tree->pt_ntype == NT_CONST && tree->pt_dtype == SYMT_STRING))
		usy_rel_string (tree->pt_v.us_v_ptr);
	tree->pt_left = Pt_lal;
	Pt_lal = tree;
}
		



void
ue_dump_tree (tree)
struct parse_tree *tree;
/*
 * Try to put out a written description of this tree.
 */
{
	static char *ops[] = { "+", "-", "*", "/", "^", "U-", "and", "or",
				"<=", "<", ">=", ">", "=", "<>" };

	if (! tree)
		return;
	if (tree->pt_ntype == NT_CONST)
	{
		ue_print_val (&tree->pt_v, tree->pt_dtype);
		ui_nf_printf (" ");
	}
	else if (tree->pt_ntype == NT_SYM)
		ui_nf_printf ("[%s] ", tree->pt_v.us_v_ptr);
	else if (tree->pt_ntype == NT_FCALL)
	{
		ui_nf_printf ("FC[%s]( ", tree->pt_v.us_v_ptr);
		ue_dump_tree (tree->pt_right);
		ui_nf_printf (") ");
	}
	else if (tree->pt_ntype == NT_FCARG)
	{
		ue_dump_tree (tree->pt_left);
		if (tree->pt_right)
		{
			ui_nf_printf (", ");
			ue_dump_tree (tree->pt_right);
		}
	}
	else
	{
		if (tree->pt_left)
			ue_dump_tree (tree->pt_left);
		else
			ui_nf_printf ("[--] ");
		if (tree->pt_right)
			ue_dump_tree (tree->pt_right);
		else
			ui_nf_printf ("[--] ");
		ui_nf_printf ("%s ", ops[tree->pt_dtype]);
	}
}



void
ue_print_val (v, type)
union usy_value *v;
int type;
/*
 * Print out this value.
 */
{
	char d_buf[TLEN];

 	switch (type)
	{
	   case SYMT_FLOAT:
	   	ui_nf_printf ("%.2f", v->us_v_float);
		break;
	   case SYMT_INT:
	   	ui_nf_printf ("%d", v->us_v_int);
		break;
	   case SYMT_STRING:
	   	ui_nf_printf ("\"%s\"", v->us_v_ptr);
		break;
	   case SYMT_DATE:
		ui_nf_printf ("%s",
			ud_format_date (d_buf, &v->us_v_date, UDF_FULL));
		break;
	   case SYMT_BOOL:
	   	ui_nf_printf (v->us_v_int ? "True" : "False");
		break;
	   default:
	   	ui_nf_printf ("????????");
	}
}




void
ue_enc_val (v, type, dest)
union usy_value *v;
int type;
char *dest;
/*
 * Encode this value into the destination string.
 */
{
	char d_buf[TLEN];

 	switch (type)
	{
	   case SYMT_FLOAT:
	   	sprintf (dest, "%.2f", v->us_v_float);
		break;
	   case SYMT_INT:
	   	sprintf (dest, "%d", v->us_v_int);
		break;
	   case SYMT_STRING:
	   	sprintf (dest, "\"%s\"", v->us_v_ptr);
		break;
	   case SYMT_DATE:
		sprintf (dest, "%s",
			ud_format_date (d_buf, &v->us_v_date, UDF_FULL));
		break;
	   case SYMT_BOOL:
	   	sprintf (dest, v->us_v_int ? "True" : "False");
		break;
	   default:
	   	strcpy (dest, "????????");
	}
}



void
ue_eval (tree, v, type)
struct parse_tree *tree;
union usy_value *v;
int *type;
/*
 * Attempt to evaluate the given parse tree.
 * Entry:
 *	TREE	is a parse tree.
 * Exit:
 *	If the evaluation is successful then
 *		The return value is TRUE
 *		v contains the result, and TYPE is the type of the result.
 * 	else
 *		An error is signalled.
 */
{
/*
 * Simply call the internal evaluator.  If a string value comes back, we
 * then have to reallocate the string before passing it back.
 */
 	ue_ieval (tree, v, type);
# ifdef notdef
	if (*type == SYMT_STRING)
		v->us_v_ptr = usy_string (v->us_v_ptr);
# endif
}






int
ue_ieval (tree, v, type)
struct parse_tree *tree;
union usy_value *v;
int *type;
/*
 * The internal version of the evaluator, which does the real work.
 */
{
	switch (tree->pt_ntype)
	{
	/*
	 * If this is a constant node, just return the value now.
	 */
	   case NT_CONST:
		*v = tree->pt_v;
		*type = tree->pt_dtype;
		if (*type == SYMT_STRING)
			v->us_v_ptr = usy_string (v->us_v_ptr);
		return (TRUE);
	/*
	 * If it is a symbol node, we look up the symbol and return that value.
	 */
	   case NT_SYM:
		return (ue_lookup_sym (tree, type, v));
	/*
	 * Function calls get deferred to the function evaluator.
	 */
	   case NT_FCALL:
		return (uf_eval(tree->pt_v.us_v_ptr, tree->pt_right, v, type));
	/*
	 * Operator nodes require further evaluation.
	 */
	   case NT_OPER:
		return (ue_operate (tree, type, v));

	   default:
	   	c_panic ("Unknown parse tree node type: %d", tree->pt_ntype);
	}
	return (0);
}



int
ue_lookup_sym (tree, type, v)
struct parse_tree *tree;
int *type;
union usy_value *v;
/*
 * Perform a symbol lookup on this tree node.
 */
{
	char *string;
/*
 * OK, let's look it up.
 * 5/14/87	Kludge: (but possibly a feature...) default undefined variables
 *		to an integer zero.  The need for this is found in the loop
 *		handling code, which may generate undefined variables on the
 *		first passthrough of a set of loop commands.
 *
 * 1/4/88	Search arg table first, so that proc args override any
 *		variables in the normal variable table.
 *
 * 7/15/88	Arg tables are now procedure-invocation-specific, and
 *		live on the cstack.
 */
	if ((!Cs || Cs->cs_arg_table == 0 || ! usy_g_symbol (Cs->cs_arg_table,
					tree->pt_v.us_v_ptr, type, v)) &&
	 	! usy_g_symbol(Ui_variable_table, tree->pt_v.us_v_ptr,type, v))
	{
		*type = SYMT_INT;
		v->us_v_int = 0;
	}
/*
 * Reallocate a string, since it will get zapped later.
 */
 	if (*type == SYMT_STRING)
		v->us_v_ptr = usy_string (v->us_v_ptr);
	return (TRUE);
}



int
ue_operate (tree, type, v)
struct parse_tree *tree;
int *type;
union usy_value *v;
/*
 * Perform the operator specified by this tree node.
 */
{
/*
 * About all we do here is to split the operators into groups, and call
 * the appropriate group handler.
 */
 	switch (tree->pt_dtype)
	{
	   /*
	    * Arithmetic operators.
	    */
	   case OP_PLUS:
	   case OP_MINUS:
	   case OP_TIMES:
	   case OP_DIVIDE:
	   case OP_EXP:
	   	ue_arith_op (tree, type, v);
		break;
	   /*
	    * Negation.
	    */
	   case OP_NEGATE:
	   	ue_negate_op (tree, type, v);
		break;
	   /*
	    * Relational operators.
	    */
	   case OP_LE:
	   case OP_LT:
	   case OP_GE:
	   case OP_GT:
	   case OP_EQ:
	   case OP_NEQ:
	   	ue_rel_op (tree, type, v);
		break;
	   /*
	    * Logical operators.
	    */
	   case OP_AND:
	   case OP_OR:
		ue_log_op (tree, type, v);
		break;
	   /*
	    * Unknown operators.
	    */
	   default:
	   	c_panic ("Bizarre operator %d in parse tree", tree->pt_dtype);
	}
	return (0);
}




void
ue_arith_op (tree, type, v)
struct parse_tree *tree;
int *type;
union usy_value *v;
/*
 * Deal with an arithmetic operator.
 */
{
	int leftt, rightt, op = tree->pt_dtype;
	union usy_value leftv, rightv;
/*
 * Evaluate the left and right branches of the tree.
 */
 	ue_ieval (tree->pt_left, &leftv, &leftt);
	ue_ieval (tree->pt_right, &rightv, &rightt);
/*
 * Now we need to worry about type compatibility.
 */
	ERRORCATCH
 	ue_type_coerce (&leftt, &leftv, &rightt, &rightv);
/*
 * Figure out what to do.
 */
	switch (op)
	{
	   case OP_PLUS:
	   	ue_do_add (leftt, &leftv, &rightv, type, v);
		break;
	   case OP_MINUS:
	   	ue_do_sub (leftt, &leftv, &rightv, type, v);
		break;
	   case OP_TIMES:
	   	ue_do_times (leftt, &leftv, &rightv, type, v);
		break;
	   case OP_DIVIDE:
	   	ue_do_div (leftt, &leftv, &rightv, type, v);
		break;
	   case OP_EXP:
	   	ue_do_exp (leftt, &leftv, &rightv, type, v);
		break;
	}
	ENDCATCH
	ue_free_result (leftt, &leftv);
	ue_free_result (rightt, &rightv);
}


void
ue_do_add (lt, lv, rv, type, v)
int lt, *type;
union usy_value *lv, *rv, *v;
/*
 * Handle the + operator.
 */
{
	*type = lt;
	switch (lt)
	{
	   case SYMT_FLOAT:
	   	v->us_v_float = lv->us_v_float + rv->us_v_float;
		break;
	   case SYMT_INT:
	   	v->us_v_int = lv->us_v_int + rv->us_v_int;
		break;
	   case SYMT_DATE:
	   	if (D_OFFSET (lv->us_v_date) || D_OFFSET (rv->us_v_date))
			ud_add_date (&lv->us_v_date, &rv->us_v_date,
						&v->us_v_date);
		else
			ui_error ("Two absolute dates can not be added");
	}
}





void
ue_do_sub (lt, lv, rv, type, v)
int lt, *type;
union usy_value *lv, *rv, *v;
/*
 * Handle the - operator.
 */
{
	*type = lt;
	switch (lt)
	{
	   case SYMT_FLOAT:
	   	v->us_v_float = lv->us_v_float - rv->us_v_float;
		break;
	   case SYMT_INT:
	   	v->us_v_int = lv->us_v_int - rv->us_v_int;
		break;
	   case SYMT_DATE:
		ud_sub_date (&lv->us_v_date, &rv->us_v_date, &v->us_v_date);
		break;
	}
}





void
ue_do_times (lt, lv, rv, type, v)
int lt, *type;
union usy_value *lv, *rv, *v;
/*
 * Handle the * operator.
 */
{
	*type = lt;
	switch (lt)
	{
	   case SYMT_FLOAT:
	   	v->us_v_float = lv->us_v_float * rv->us_v_float;
		break;
	   case SYMT_INT:
	   	v->us_v_int = lv->us_v_int * rv->us_v_int;
		break;
	   case SYMT_DATE:
		ui_error ("Date values can not be multiplied.");
		break;
	}
}




void
ue_do_div (lt, lv, rv, type, v)
int lt, *type;
union usy_value *lv, *rv, *v;
/*
 * Handle the / operator.
 */
{
	*type = lt;
	switch (lt)
	{
	   case SYMT_FLOAT:
	   	if (rv->us_v_float == 0)
			ui_error ("Division by zero");
	   	v->us_v_float = lv->us_v_float / rv->us_v_float;
		break;
	   case SYMT_INT:
	   	if (rv->us_v_int == 0)
			ui_error ("Division by zero");
	   	v->us_v_int = lv->us_v_int / rv->us_v_int;
		break;
	   case SYMT_DATE:
		ui_error ("Date values can not be divided.");
		break;
	}
}





void
ue_do_exp (lt, lv, rv, type, v)
int lt, *type;
union usy_value *lv, *rv, *v;
/*
 * Handle the ^ operator.
 *
 * This is currently a poor implementation.
 */
{
	*type = lt;
	switch (lt)
	{
	   case SYMT_FLOAT:
	   	v->us_v_float = pow ((double) lv->us_v_float,
				     (double) rv->us_v_float);
		break;
	   case SYMT_INT:
	   	v->us_v_int = (int) pow ((double) lv->us_v_int,
				     (double) rv->us_v_int);
		break;
	   case SYMT_DATE:
		ui_error ("Date values can not be exponentiated.");
		break;
	}
}




void
ue_type_coerce (t1, v1, t2, v2)
int *t1, *t2;
union usy_value *v1, *v2;
/*
 * Perform type coercion for arithmetic operations.
 */
{
	int goal;
/*
 * 10/88 jc:	If either operand has a STRING type, then both must.  We
 *		do no implicit conversion here.
 */
 	if (*t1 == SYMT_STRING || *t2 == SYMT_STRING)
	{
		if (*t1 != *t2)
			ui_error ("Invalid STRING operand");
		return;
	}
/*
 * First, figure out what our goal type will be.  If either value is a
 * date, both will become date.
 */
 	if (*t1 == SYMT_DATE || *t2 == SYMT_DATE)
		goal = SYMT_DATE;
/*
 * If anything here is real, we do real arithmetic.
 */
 	else if (*t1 == SYMT_FLOAT || *t2 == SYMT_FLOAT)
		goal = SYMT_FLOAT;
/*
 * Otherwise we will shoot for integer arithmetic.
 */
 	else
		goal = SYMT_INT;
/*
 * Now coerce each operand.
 */
 	if (*t1 != goal)
	{
		uit_coerce (v1, *t1, goal);
		*t1 = goal;
	}
 	if (*t2 != goal)
	{
		uit_coerce (v2, *t2, goal);
		*t2 = goal;
	}
}




int
ue_negate_op (tree, type, v)
struct parse_tree *tree;
int *type;
union usy_value *v;
/*
 * Handle a unary minus.
 */
{
	int leftt;
	union usy_value leftv;
/*
 * Evaluate the item to be negated.
 */
 	ue_ieval (tree->pt_left, &leftv, &leftt);
/*
 * Now, branch out and do the negation.
 */
	*type = leftt;
	switch (leftt)
	{
	   case SYMT_INT:
	   	v->us_v_int = -leftv.us_v_int;
		break;
	   case SYMT_FLOAT:
	   	v->us_v_float = -leftv.us_v_float;
		break;
	   case SYMT_BOOL:
	   	v->us_v_int = ! leftv.us_v_int;
		break;
	   case SYMT_DATE:
	   	ui_error ("Negation of a date makes very little sense");
	}
	return (TRUE);
}





void
ue_log_op (tree, type, v)
struct parse_tree *tree;
int *type;
union usy_value *v;
/*
 * Deal with a logical operator.
 */
{
	int leftt, rightt, op = tree->pt_dtype;
	union usy_value leftv, rightv;
/*
 * Evaluate the left and right branches of the tree.
 */
 	ue_ieval (tree->pt_left, &leftv, &leftt);
	ue_ieval (tree->pt_right, &rightv, &rightt);
/*
 * Now we need to worry about type compatibility.  Fortunately, logical 
 * operators take nothing other than boolean values, so the coercion
 * decision is easy.
 */
	if (leftt == SYMT_DATE)
		ui_error ("Unable to perform logical operations on dates");
	else if (leftt != SYMT_BOOL)
		uit_coerce (&leftv, leftt, SYMT_BOOL);
	if (rightt == SYMT_DATE)
		ui_error ("Unable to perform logical operations on dates");
	else if (rightt != SYMT_BOOL)
		uit_coerce (&rightv, rightt, SYMT_BOOL);
/*
 * Figure out what to do.
 */
 	*type = SYMT_BOOL;
	switch (op)
	{
	   case OP_AND:
	   	v->us_v_int = leftv.us_v_int && rightv.us_v_int;
		break;
	   case OP_OR:
	   	v->us_v_int = leftv.us_v_int || rightv.us_v_int;
		break;
	}
}





void
ue_rel_op (tree, type, v)
struct parse_tree *tree;
int *type;
union usy_value *v;
/*
 * Deal with a relational operator.
 */
{
	int leftt, rightt, op = tree->pt_dtype;
	union usy_value leftv, rightv;
/*
 * Evaluate the left and right branches of the tree.
 */
 	ue_ieval (tree->pt_left, &leftv, &leftt);
	ue_ieval (tree->pt_right, &rightv, &rightt);
/*
 * Deal with type compatibility.  Here, we do essentially the same thing
 * as for the arithmetic operators.
 */
	ERRORCATCH
 	ue_type_coerce (&leftt, &leftv, &rightt, &rightv);
/*
 * Figure out what to do.
 */
	*type = SYMT_BOOL;
	switch (op)
	{
	   case OP_GT:
	   	ue_do_gt (leftt, &leftv, &rightv, v);
		break;
	   case OP_LT:
	   	ue_do_lt (leftt, &leftv, &rightv, v);
		break;
	   case OP_GE:
	   	ue_do_ge (leftt, &leftv, &rightv, v);
		break;
	   case OP_LE:
	   	ue_do_le (leftt, &leftv, &rightv, v);
		break;
	   case OP_EQ:
	   	ue_do_eq (leftt, &leftv, &rightv, v);
		break;
	   case OP_NEQ:
	   	ue_do_neq (leftt, &leftv, &rightv, v);
		break;
	}
	ENDCATCH
	ue_free_result (leftt, &leftv);
	ue_free_result (rightt, &rightv);
}





void
ue_do_gt (type, v1, v2, result)
int type;
union usy_value *v1, *v2, *result;
/*
 * Calculate (v1 > v2) and put the answer into RESULT as a boolean.
 */
{
	switch (type)
	{
	   case SYMT_BOOL:
	   case SYMT_INT:
	   	result->us_v_int = v1->us_v_int > v2->us_v_int;
		break;
	   case SYMT_FLOAT:
	   	result->us_v_int = v1->us_v_float > v2->us_v_float;
		break;
	   case SYMT_DATE:
	        ud_y2k_date (&v1->us_v_date);
		ud_y2k_date (&v2->us_v_date);
		result->us_v_int =
			(v1->us_v_date.ds_yymmdd > v2->us_v_date.ds_yymmdd) ||
			((v1->us_v_date.ds_yymmdd == v2->us_v_date.ds_yymmdd)&&
			 (v1->us_v_date.ds_hhmmss > v2->us_v_date.ds_hhmmss));
		break;
	   case SYMT_STRING:
	   	result->us_v_int = strcmp (v1->us_v_ptr, v2->us_v_ptr) > 0;
		break;
	}
}




void
ue_do_ge (type, v1, v2, result)
int type;
union usy_value *v1, *v2, *result;
/*
 * Calculate (v1 >= v2) and put the answer into RESULT as a boolean.
 */
{
	switch (type)
	{
	   case SYMT_BOOL:
	   case SYMT_INT:
	   	result->us_v_int = v1->us_v_int >= v2->us_v_int;
		break;
	   case SYMT_FLOAT:
	   	result->us_v_int = v1->us_v_float >= v2->us_v_float;
		break;
	   case SYMT_DATE:
	        ud_y2k_date (&v1->us_v_date);
		ud_y2k_date (&v2->us_v_date);
		result->us_v_int =
			(v1->us_v_date.ds_yymmdd >= v2->us_v_date.ds_yymmdd) ||
			((v1->us_v_date.ds_yymmdd == v2->us_v_date.ds_yymmdd)&&
			 (v1->us_v_date.ds_hhmmss >= v2->us_v_date.ds_hhmmss));
		break;
	   case SYMT_STRING:
	   	result->us_v_int = strcmp (v1->us_v_ptr, v2->us_v_ptr) >= 0;
		break;
	}
}




void
ue_do_lt (type, v1, v2, result)
int type;
union usy_value *v1, *v2, *result;
/*
 * Calculate (v1 < v2) and put the answer into RESULT as a boolean.
 */
{
	switch (type)
	{
	   case SYMT_BOOL:
	   case SYMT_INT:
	   	result->us_v_int = v1->us_v_int < v2->us_v_int;
		break;
	   case SYMT_FLOAT:
	   	result->us_v_int = v1->us_v_float < v2->us_v_float;
		break;
	   case SYMT_DATE:
	        ud_y2k_date (&v1->us_v_date);
		ud_y2k_date (&v2->us_v_date);
		result->us_v_int =
			(v1->us_v_date.ds_yymmdd < v2->us_v_date.ds_yymmdd) ||
			((v1->us_v_date.ds_yymmdd == v2->us_v_date.ds_yymmdd)&&
			 (v1->us_v_date.ds_hhmmss < v2->us_v_date.ds_hhmmss));
		break;
	   case SYMT_STRING:
	   	result->us_v_int = strcmp (v1->us_v_ptr, v2->us_v_ptr) < 0;
		break;
	}
}




void
ue_do_le (type, v1, v2, result)
int type;
union usy_value *v1, *v2, *result;
/*
 * Calculate (v1 <= v2) and put the answer into RESULT as a boolean.
 */
{
	switch (type)
	{
	   case SYMT_BOOL:
	   case SYMT_INT:
	   	result->us_v_int = v1->us_v_int <= v2->us_v_int;
		break;
	   case SYMT_FLOAT:
	   	result->us_v_int = v1->us_v_float <= v2->us_v_float;
		break;
	   case SYMT_DATE:
	        ud_y2k_date (&v1->us_v_date);
		ud_y2k_date (&v2->us_v_date);
		result->us_v_int =
			(v1->us_v_date.ds_yymmdd < v2->us_v_date.ds_yymmdd) ||
			((v1->us_v_date.ds_yymmdd == v2->us_v_date.ds_yymmdd)&&
			 (v1->us_v_date.ds_hhmmss <= v2->us_v_date.ds_hhmmss));
		break;
	   case SYMT_STRING:
	   	result->us_v_int = strcmp (v1->us_v_ptr, v2->us_v_ptr) <= 0;
		break;
	}
}




void
ue_do_eq (type, v1, v2, result)
int type;
union usy_value *v1, *v2, *result;
/*
 * Calculate (v1 = v2) and put the answer into RESULT as a boolean.
 */
{
	switch (type)
	{
	   case SYMT_BOOL:
	   	result->us_v_int = (v1->us_v_int == 0) == (v2->us_v_int == 0);
		break;
	   case SYMT_INT:
	   	result->us_v_int = v1->us_v_int == v2->us_v_int;
		break;
	   case SYMT_FLOAT:
	   	result->us_v_int = v1->us_v_float == v2->us_v_float;
		break;
	   case SYMT_DATE:
	        ud_y2k_date (&v1->us_v_date);
		ud_y2k_date (&v2->us_v_date);
		result->us_v_int =
			(v1->us_v_date.ds_yymmdd == v2->us_v_date.ds_yymmdd) &&
			(v1->us_v_date.ds_hhmmss == v2->us_v_date.ds_hhmmss);
		break;
	   case SYMT_STRING:
	   	result->us_v_int = strcmp (v1->us_v_ptr, v2->us_v_ptr) == 0;
		break;
	}
}



void
ue_do_neq (type, v1, v2, result)
int type;
union usy_value *v1, *v2, *result;
/*
 * Calculate (v1 != v2) and put the answer into RESULT as a boolean.
 */
{
	switch (type)
	{
	   case SYMT_BOOL:
	   	result->us_v_int = (v1->us_v_int == 0) != (v2->us_v_int == 0);
		break;
	   case SYMT_INT:
	   	result->us_v_int = v1->us_v_int != v2->us_v_int;
		break;
	   case SYMT_FLOAT:
	   	result->us_v_int = v1->us_v_float != v2->us_v_float;
		break;
	   case SYMT_DATE:
	        ud_y2k_date (&v1->us_v_date);
		ud_y2k_date (&v2->us_v_date);
		result->us_v_int =
			(v1->us_v_date.ds_yymmdd != v2->us_v_date.ds_yymmdd) ||
			(v1->us_v_date.ds_hhmmss != v2->us_v_date.ds_hhmmss);
		break;
	   case SYMT_STRING:
	   	result->us_v_int = strcmp (v1->us_v_ptr, v2->us_v_ptr) != 0;
		break;
	}
}





void
ue_free_result (type, v)
int type;
SValue *v;
/*
 * Free the result of a tree evaluation.
 */
{
	if (type == SYMT_STRING)
		usy_rel_string (v->us_v_ptr);
}
