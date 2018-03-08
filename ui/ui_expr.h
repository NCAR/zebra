/* $Id: ui_expr.h,v 1.3 1998-02-26 21:18:34 burghart Exp $ */
/*
 * Global info for the use of the recursive descent parser.
 */
# ifndef UI_EXPR_SYMBOLS
# define UI_EXPR_SYMBOLS

# include "ui_symbol.h"

/*
 * This structure is a node in a parse tree.
 */
struct parse_tree
{
	int pt_ntype;		/* The type of this node (see below)	*/
	int pt_dtype;		/* The data or op type of this node.   	*/
	struct parse_tree *pt_left;	/* Left operand			*/
	struct parse_tree *pt_right;	/* Right operand		*/
	union usy_value pt_v;	/* Actual value for leaf nodes.		*/
};


/*
 * Node types.
 */
# define NT_CONST	0	/* Constant value			*/
# define NT_SYM		1	/* Symbol reference			*/
# define NT_OPER	2	/* Some sort of operator		*/
# define NT_FCALL	3	/* Function call			*/
# define NT_FCARG	4	/* Function call argument list		*/

/*
 * Operator types.
 */
# define OP_PLUS	0
# define OP_MINUS	1
# define OP_TIMES	2
# define OP_DIVIDE	3
# define OP_EXP		4
# define OP_NEGATE	5	/* Unary - */
# define OP_AND		6
# define OP_OR		7
# define OP_LE		8
# define OP_LT		9
# define OP_GE		10
# define OP_GT		11
# define OP_EQ		12
# define OP_NEQ		13

# ifdef __STDC__
	struct parse_tree *ue_parse (char *, int, int);
	void ue_rel_tree (struct parse_tree *);
	void ue_dump_tree (struct parse_tree *);
	void ue_eval (struct parse_tree *, union usy_value *, int *);
# else
	struct parse_tree *ue_parse ();
	void ue_rel_tree ();
	void ue_dump_tree ();
	void ue_eval ();
# endif

# endif
