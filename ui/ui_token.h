/* 10/86 jc */
/* $Header: /code/cvs/rdss/rdsslibs/ui/ui_token.h,v 1.1 1989-02-08 13:28:22 corbet Exp $ */
/*
 * Included info for the tokenizer.
 */
# ifndef TOKEN_SYMBOLS
# define TOKEN_SYMBOLS

/*
 * This is the structure passed to and from the tokenizer, which describes
 * the next token.
 */
# define TK_MAXSTR	256	/* Maximum length of one token	*/
struct token
{
	char	tk_string[TK_MAXSTR];	/* The actual token text	*/
	byte	tk_flags;		/* Token flags			*/
	byte	tk_tgroup;		/* Token group number		*/
	short	tk_type;		/* Token type.			*/
	int	tk_col;			/* Column number of this token	*/
};


/*
 * Token types.
 */
# define TT_EOS		0	/* End of string.		*/
# define TT_EOF		1	/* End of input source.		*/
# define TT_BACKUP	2	/* Int user backed into prev tok*/
# define TT_LKILL	3	/* Some sort of line-killing act */
# define TT_NORM	4	/* Normal token.		*/
# define TT_HELP	5	/* Help character		*/
# define TT_SYM		6	/* Non-substituted symbol	*/
# define TT_NOINPUT	7	/* No input data		*/

/*
 * Flags.
 */
# define TF_NOECHO	0x01	/* This token should not be echoed.	*/
# define TF_REPL	0x02	/* Replacement input -- sym/cmd subst	*/
# define TF_INCR	0x04	/* Increment token group -- kludge	*/

# endif
