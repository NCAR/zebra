/* $Id: ui_symbol.h,v 1.8 1998-12-17 17:18:17 burghart Exp $ */
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
typedef union usy_value
{
	long us_v_int;		/* Integer symbol value		*/
	float	us_v_float;	/* Floating point value		*/
	date	us_v_date;	/* Date value			*/
	char	*us_v_ptr;	/* Everything else		*/
} SValue;

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
# ifdef __cplusplus
	int usy_traverse (const stbl, 
	  int (*)(const char *, int, const SValue *, long), long, int);
	int usy_search (const stbl, 
	  int (*)(const char *, int, const SValue *, long), long, int, char *);
# else
# if __STDC__
	int usy_traverse (const stbl, int (*)(), long, int);
	int usy_search (const stbl, int (*)(), long, int, char *);
# endif
# endif /* __cplusplus */

void usy_init (void);
stbl usy_c_stbl (const char *);
void usy_z_stbl (stbl);
void usy_z_symbol (stbl, const char *);
int usy_g_symbol (const stbl, const char *, int *, SValue *);
bool usy_defined (const stbl, const char *);
void usy_s_symbol (stbl, const char *, int, const SValue *);
void usy_dump_table (const stbl);
stbl usy_g_stbl (const char *);
void usy_c_indirect (stbl, const char *, const void *, int, int);
int usy_daemon (stbl, const char *, int, int (*)(), char *);
int usy_z_daemon (stbl, const char *, int, int (*)(), char *);
void uit_coerce (union usy_value *value, int from, int to);

# endif
