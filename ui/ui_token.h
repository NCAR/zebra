/* 10/86 jc */
/* $Id: ui_token.h,v 1.6 1998-02-26 21:48:58 burghart Exp $ */
/*
 * Included info for the tokenizer.
 */
# ifndef TOKEN_SYMBOLS
# define TOKEN_SYMBOLS

/*
 * This is the structure passed to and from the tokenizer, which describes
 * the next token.
 */
# define TK_MAXSTR	768	/* Maximum length of one token	*/
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

/*
 * The token context type.
 */
/*
 * This information describes a context for a particular tokenization.
 */
# define MAXTGRP	100	/* Max number of token groups	*/
struct token_context
{
	char tc_iline[MAXLINE];	/* The actual input line */
	char tc_rline[MAXLINE];	/* The real line	*/
	int tc_iindex, tc_rindex; /* Indices into both lines */
	unsigned short tc_iflags[MAXLINE];	/* Flags for Iline	*/
	char tc_symbuf[MAXLINE];	/* Symbol buffer.	*/
	int tc_sindex;
	int tc_rstart;		/* Start of current token in Rline	*/
	int tc_iprompt;		/* Where the prompt ends.	*/
	bool tc_rdiff;		/* TRUE if rline is different from iline */
	bool tc_dumped;		/* TRUE if lines have been dumped.	*/
	bool tc_do_sub;		/* Do we perform substitutions?	*/
	int tc_target;		/* Where input is going.	*/
	int tc_saved_state;	/* Previous state			*/
	int tc_t_state;		/* The actual tokenizer state 		*/
	/*
	 * Token group accounting.
	 * A "token group" is a concept used to relate one or more "real line"
	 * tokens to a single input line token.
	 */
	int tc_tgroup;			/* Current token group number	*/
	int tc_rgroups[MAXTGRP];	/* Group pointers into Rline	*/
	bool tc_in_line;		/* Are we in a line?		*/
};
typedef struct token_context *Tcontext;

/*
 * Function declarations.
 */
# ifdef __STDC__
	void ut_zap_token ();
	void ut_finish_line (int);
	void ut_begin (char *, int);
	void ut_tok_repl (char *);
	void ut_put_msg (char *, int);
	void ut_reset ();
	void ut_get_token (struct token *);
	void ut_continue ();
	void ut_complete (char *);
	void ut_int_string (char *, struct token *);
# else
	void ut_zap_token ();
	void ut_finish_line ();
	void ut_begin ();
	void ut_tok_repl ();
	void ut_put_msg ();
	void ut_reset ();
	void ut_get_token ();
	void ut_continue ();
	void ut_complete ();
	void ut_int_string ();
# endif

# endif
