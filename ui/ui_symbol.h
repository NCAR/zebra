/* $Id: ui_symbol.h,v 1.3 1989-07-12 09:43:07 corbet Exp $ */
/*
 * Global declarations for the symbol table module.
 */
# ifndef SYMBOL_TABLE_SYMBOLS
# define SYMBOL_TABLE_SYMBOLS

# include <ui_param.h>
/*
 * Symbol types.
 */
# define SYMT_FLOAT	0	/* Floating point number	*/
# define SYMT_INT	1	/* Integer			*/
# define SYMT_STRING	2	/* C string type		*/
# define SYMT_DATE	3	/* ROBOT/MDA date format	*/
# define SYMT_BOOL	4	/* Boolean value		*/
# define SYMT_SYMBOL	5	/* Symbol table			*/
# define SYMT_POINTER	6	/* General pointer to somewhere	*/
# define SYMT_UNDEFINED 99	/* Undefined symbol		*/

/*
 * this union type is used to pass symbol values around.
 */
union usy_value
{
	int us_v_int;		/* Integer symbol value		*/
	float	us_v_float;	/* Floating point value		*/
	date	us_v_date;	/* Date value			*/
	char	*us_v_ptr;	/* Everything else		*/
};

/*
 * Symbol operations.
 */
# define SOP_READ	0x1	/* Read operation		*/
# define SOP_WRITE	0x2	/* Write operation		*/
# define SOP_DESTROY	0x4	/* Destroy			*/

/*
 * The (external) type for a symbol table.
 */
typedef char *stbl;

/*
 * Symbol table routines.
 */
int usy_g_symbol ();
bool usy_defined ();
char *usy_string (), *usy_pstring ();
stbl usy_g_stbl ();
stbl usy_c_stbl ();

# endif
