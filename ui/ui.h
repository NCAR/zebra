/* 10/86 jc */
/* $Id: ui.h,v 1.3 1990-05-23 09:11:09 burghart Exp $ */

# ifndef UI_H_SYMBOLS
# define UI_H_SYMBOLS
/*
 * Declarations for use outside (as well as inside) the user interface.
 */
# include <ui_symbol.h>
/*
 * This is the structure filled in by the command parser.  One of these
 * structures will appear for each non-ignored token in the input
 * stream.
 */
struct ui_command
{
	int	uc_ctype;	/* The token type -- see below.	*/
	int	uc_vptype;	/* Value parameter type		*/
	union usy_value uc_v;	/* Value parameter value.	*/
	int	uc_col;		/* Column number of the token	*/
	char 	*uc_text;	/* The actual token text	*/
};
/*
 * Token types.
 */
# define UTT_END	0	/* End of the token list	*/
# define UTT_VALUE	1	/* Value parameter		*/
# define UTT_OTHER	2	/* Something else.		*/
# define UTT_KW		3	/* Keyword token type.		*/
# define UTT_PARTIAL	4	/* End of a partial command	*/
# define UTT_SYM	5	/* Non-subst sym		*/

/*
 * Macros to ease dealing with cmds stuff.
 */
# define UINT(cmd) 	(cmd).uc_v.us_v_int
# define UKEY(cmd) 	(cmd).uc_v.us_v_int
# define UBOOL(cmd)	(cmd).uc_v.us_v_int
# define UPTR(cmd)	(cmd).uc_v.us_v_ptr
# define UFLOAT(cmd)	(cmd).uc_v.us_v_float
# define UDATE(cmd)	(cmd).uc_v.us_v_date
# define UCOL(cmd)	(cmd).uc_col

/*
 * Forward definitions of useful routines.
 */
int ui_int_prompt ();
double ui_float_prompt ();

/*
 * Let the application know if X support is present.
 */
# ifdef XSUPPORT
# define UI_X_SUPPORT
# endif

# endif
