/* 12/86 jc */
/*
 * Tokenization.
 */
# include <stdio.h>
# include "ui.h"
# include "ui_symbol.h"
# include "ui_cstack.h"
# include "ui_globals.h"
# include "ui_token.h"
# include "ui_tty.h"
# include "ui_mode.h"

static char *Rcsid = "$Id: ui_token.c,v 1.18 2002-07-11 20:38:32 burghart Exp $";

/*
 * For input analysis, all characters are classified into one of the
 * following groups:
 */
# define CG_NORMAL	 0	/* Normal, printing character		*/
# define CG_SPACE	 1	/* Space bar				*/
# define CG_TAB		 2	/* Tab					*/
# define CG_RETURN	 3	/* A line terminator character		*/
# define CG_EOF		 4	/* End of file character		*/
# define CG_HELP	 5	/* Help character			*/
# define CG_OPAREN	 6	/* Open parenthesis			*/
# define CG_CPAREN	 7	/* Close parenthesis			*/
# define CG_QUOTE	 8	/* Some sort of quote character		*/
# define CG_UP_ARROW	 9	/* Up arrow				*/
# define CG_DOWN_ARROW  10	/* down arrow				*/
# define CG_ERROR	11	/* Something we can't properly deal with*/
# define CG_ERASE	12	/* Backspace character			*/
# define CG_LKILL	13	/* Line kill character			*/
# define CG_LITERAL	14	/* Literal next character		*/
# define CG_WKILL	15	/* Word kill				*/
# define CG_VAR		16	/* Variable subst character		*/
# define CG_MULTI	17	/* Multi-command separator (;)		*/
# define CG_AT		18	/* @					*/
# define CG_FUNCTION	19	/* Function key				*/
# define CG_COMMENT	20	/* Comment delimiter (!)		*/
# define CG_REDRAW	21	/* Redraw the line			*/
# define NCTYPE		22	/* Number of character types		*/

/*
 * The other dimension in the input analysis table is the current tokenizer
 * state.  These are those states.
 */
# define TS_BOT		0	/* Beginning of token			*/
# define TS_QUOTE	1	/* Within a quoted string		*/
# define TS_PAREN	2	/* Within a parenthesized expression	*/
# define TS_VAR		3	/* Variable substitution		*/
# define TS_VARE	4	/* Enclosed var  i.e. #{var}		*/
# define TS_TOKEN	5	/* Normal, within token state		*/
# define TS_AT		6	/* @ received.				*/
# define TS_PROMPT	7	/* Doing interactive prompt		*/
# define TS_COMMENT	8	/* Within a comment			*/
# define TS_DONE	99	/* Time to quit				*/
# define NSTATES	9	/* How many states we have.		*/

static char Quote;	/* Quotation information		*/
int Pcount;

/*
 * Forward declarations for the state response table.
 */
void ut_norm (), ut_bot (), ut_wtab (), ut_tab (), ut_err (), ut_endt ();
void ut_reos (), ut_eos (), ut_lkill (), ut_back (), ut_dbg ();
void ut_wkill (), ut_at (), ut_quote (), ut_endq (), ut_help ();
void ut_up (), ut_down (), ut_open (), ut_close (), ut_npar ();
void ut_var (), ut_evar (), ut_sret (), ut_atsp (), ut_func ();
void ut_peos (), ut_perr (), ut_qerr (), ut_bcom (), ut_scom (), ut_rdrw ();

/*
 * This is the actual table.
 */
typedef void (*void_func_ptr) ();
static void_func_ptr Tst_table [NCTYPE] [NSTATES] =
{
/* Ctype...	  BOT	     	Quote   	Paren     	Var
		VarE     	Token		At		Prompt 
		Comment							*/
/* Normal   */	{ ut_bot,	ut_norm,	ut_norm,	ut_norm,
		  ut_norm,	ut_norm,	ut_atsp,	ut_norm,
		  ut_norm },
/* Space    */	{ ut_norm,	ut_norm,	ut_norm,	ut_evar,
		  ut_norm,	ut_endt,	ut_endt,	ut_norm,
		  ut_norm },
/* Tab	    */	{ ut_tab,	ut_tab,		___,		ut_evar,
		  ut_tab,	ut_endt,	ut_tab,		ut_tab,
		  ut_norm },
/* Return   */	{ ut_reos,	ut_qerr,	ut_perr,	ut_evar,
		  ut_sret,	ut_eos,		___,		ut_peos,
		  ut_reos },
/* EOF      */	{ ut_reos,	ut_qerr,	ut_perr,	ut_evar,
		  ut_sret,	ut_eos,		___,		___ ,
		  ut_reos },
/* Help     */	{ ut_help,	ut_help, 	ut_help,	ut_help,
		  ut_help,	ut_help,	ut_help,	ut_help,
		  ut_norm },
/* Oparen   */	{ ut_open,	ut_norm,	ut_npar,	___,
		  ut_norm,	ut_open,	ut_atsp,	ut_norm,
		  ut_norm },
/* Cparen   */	{ ___,		ut_norm,	ut_close,	___,
		  ut_norm,	___,		ut_atsp,	ut_norm,
		  ut_norm },
/* Quote    */	{ ut_quote,	ut_endq,	ut_norm,	___,
		  ut_norm,	ut_quote,	ut_atsp,	ut_norm,
		  ut_norm },
/* Up arrow */	{ ut_up,	ut_up,		ut_up,		ut_up,
		  ut_up,	ut_up,		ut_up,		___,
		  ut_up },
/* Dn arrow */	{ ut_down,	ut_down,	ut_down,	ut_down,
		  ut_down,	ut_down,	ut_down,	___,
		  ut_down },
/* Error    */	{ ___,		___,		___,		___,
		  ___,		___,		___,		___,
		  ___ },
/* Backspace*/	{ ut_back,	ut_back,	ut_back,	ut_back,
		  ut_back,	ut_back,	ut_back,	ut_back,
		  ut_back },
/* Lkill    */	{ ut_lkill,	ut_lkill,	ut_lkill,	ut_lkill,
		  ut_lkill,	ut_lkill,	ut_lkill,	ut_wkill,
		  ut_lkill },
/* Debug  */	{ ut_dbg,	ut_dbg,		ut_dbg,		ut_dbg,
		  ut_dbg,	ut_dbg,		ut_dbg,		ut_dbg,
		  ut_dbg },
/* Wkill    */  { ut_wkill,	ut_wkill,	ut_wkill,	ut_wkill,
		  ut_wkill,	ut_wkill,	ut_wkill,	ut_wkill,
		  ut_wkill },
/* Var      */	{ ut_var,	ut_norm,	___,		___,
		  ut_norm,	___,		ut_atsp,	___,
		  ut_norm },
/* multi    */	{ ut_reos,	ut_norm,	___,		ut_evar,
		  ut_sret,	ut_eos,		___,		___,
		  ut_norm },
/* At       */	{ ut_at,	ut_norm,	___,		___,
		  ut_norm,	ut_norm,	___,		___,
		  ut_norm },
/* Function */	{ ut_func,	ut_func,	ut_func,	ut_func,
		  ut_func,	ut_func,	ut_func,	___,
		  ut_func },
/* Comment   */	{ ut_bcom,	ut_norm,	ut_norm,	ut_scom,
		  ut_scom,	ut_scom,	ut_scom,	ut_norm,
		  ut_norm },
/* Redraw    */ { ut_rdrw,	ut_rdrw,	ut_rdrw,	ut_rdrw,
		  ut_rdrw,	ut_rdrw,	ut_rdrw,	ut_rdrw,
		  ut_rdrw }
};

/*
 * This table is used to map characters onto their classifications.
 */
static byte Class_table[128] =
{
	/* NUL	*/	CG_ERROR,	/* ^A	*/	CG_ERROR,
	/* ^B	*/	CG_UP_ARROW,	/* ^C	*/	CG_ERROR,
	/* ^D	*/	CG_EOF,		/* ^E	*/	CG_ERROR,
	/* ^F	*/	CG_ERROR,	/* ^G	*/	CG_ERROR,
	/* ^H	*/	CG_ERASE,	/* ^I	*/	CG_TAB,
	/* ^J	*/	CG_RETURN,	/* ^K	*/	CG_UP_ARROW,
	/* ^L	*/	CG_ERROR,	/* ^M	*/	CG_RETURN,
	/* ^N	*/	CG_ERROR,	/* ^O	*/	CG_ERROR,
	/* ^P	*/	CG_UP_ARROW,	/* ^Q	*/	CG_ERROR,
	/* ^R	*/	CG_REDRAW,	/* ^S	*/	CG_ERROR,
	/* ^T	*/	CG_ERROR,	/* ^U	*/	CG_LKILL,
	/* ^V	*/	CG_LITERAL,	/* ^W	*/	CG_WKILL,
	/* ^X	*/	CG_LKILL,	/* ^Y	*/	CG_ERROR,
	/* ^Z	*/	CG_EOF,		/* ESC	*/	CG_ERROR,
	/* FS ^\*/	CG_ERROR,	/* GS ^]*/	CG_ERROR,
	/* RD ^^*/	CG_ERROR,	/* US ^_*/	CG_ERROR,
	/* SPC	*/	CG_SPACE,	/* !	*/	CG_COMMENT,
	/* "	*/	CG_QUOTE,	/* #	*/	CG_VAR,
	/* $	*/	CG_NORMAL,	/* %	*/	CG_NORMAL,
	/* &	*/	CG_VAR,		/* '	*/	CG_QUOTE,
	/* (	*/	CG_OPAREN,	/* )	*/	CG_CPAREN,
	/* *	*/	CG_NORMAL,	/* +	*/	CG_NORMAL,
	/* ,	*/	CG_NORMAL,	/* -	*/	CG_NORMAL,
	/* .	*/	CG_NORMAL,	/* /	*/	CG_NORMAL,
	/* 0	*/	CG_NORMAL,	/* 1	*/	CG_NORMAL,
	/* 2	*/	CG_NORMAL,	/* 3	*/	CG_NORMAL,
	/* 4	*/	CG_NORMAL,	/* 5	*/	CG_NORMAL,
	/* 6	*/	CG_NORMAL,	/* 7	*/	CG_NORMAL,
	/* 8	*/	CG_NORMAL,	/* 9	*/	CG_NORMAL,
	/* :	*/	CG_NORMAL,	/* ;	*/	CG_MULTI,
	/* <	*/	CG_NORMAL,	/* =	*/	CG_NORMAL,
	/* >	*/	CG_NORMAL,	/* ?	*/	CG_HELP,
	/* @	*/	CG_AT,		/* A	*/	CG_NORMAL,
	/* B	*/	CG_NORMAL,	/* C	*/	CG_NORMAL,
	/* D	*/	CG_NORMAL,	/* E	*/	CG_NORMAL,
	/* F	*/	CG_NORMAL,	/* G	*/	CG_NORMAL,
	/* H	*/	CG_NORMAL,	/* I	*/	CG_NORMAL,
	/* J	*/	CG_NORMAL,	/* K	*/	CG_NORMAL,
	/* L	*/	CG_NORMAL,	/* M	*/	CG_NORMAL,
	/* N	*/	CG_NORMAL,	/* O	*/	CG_NORMAL,
	/* P	*/	CG_NORMAL,	/* Q	*/	CG_NORMAL,
	/* R	*/	CG_NORMAL,
	/* S	*/	CG_NORMAL,	/* T	*/	CG_NORMAL,
	/* U	*/	CG_NORMAL,	/* V	*/	CG_NORMAL,
	/* W	*/	CG_NORMAL,	/* X	*/	CG_NORMAL,
	/* Y	*/	CG_NORMAL,	/* Z	*/	CG_NORMAL,
	/* [	*/	CG_NORMAL,	/* \	*/	CG_NORMAL,
	/* ]	*/	CG_NORMAL,	/* ^	*/	CG_NORMAL,
	/* _	*/	CG_NORMAL,	/* `	*/	CG_NORMAL,
	/* a	*/	CG_NORMAL,	/* b	*/	CG_NORMAL,
	/* c	*/	CG_NORMAL,	/* d	*/	CG_NORMAL,
	/* e	*/	CG_NORMAL,	/* f	*/	CG_NORMAL,
	/* g	*/	CG_NORMAL,	/* h	*/	CG_NORMAL,
	/* i	*/	CG_NORMAL,	/* j	*/	CG_NORMAL,
	/* k	*/	CG_NORMAL,	/* l	*/	CG_NORMAL,
	/* m	*/	CG_NORMAL,	/* n	*/	CG_NORMAL,
	/* o	*/	CG_NORMAL,	/* p	*/	CG_NORMAL,
	/* q	*/	CG_NORMAL,	/* r	*/	CG_NORMAL,
	/* s	*/	CG_NORMAL,	/* t	*/	CG_NORMAL,
	/* u	*/	CG_NORMAL,	/* v	*/	CG_NORMAL,
	/* x	*/	CG_NORMAL,	/* x	*/	CG_NORMAL,
	/* y	*/	CG_NORMAL,	/* z	*/	CG_NORMAL,
	/* {	*/	CG_NORMAL,	/* |	*/	CG_NORMAL,
	/* }	*/	CG_NORMAL,	/* ~	*/	CG_NORMAL,
	/* DEL	*/	CG_ERASE
};

# define SET_IFLAGS(ctx,fl) { if ((ctx)->tc_target & T_ILINE) \
				(ctx)->tc_iflags[(ctx)->tc_iindex] = (fl); }
/*
 * There are enough different places to store input that we have a separate
 * variable to keep track.
 */
# define T_ILINE	0x01
# define T_RLINE	0x02
# define T_SYMBUF	0x04

/*
 * Character flags.  The main use for these is in backspace processing.
 */
# define CF_BOT		0x0001	/* Beginning of token marker	*/
# define CF_OPAREN	0x0002	/* Open paren			*/
# define CF_CPAREN	0x0004	/* Close paren			*/
# define CF_REPL	0x0008	/* End of replaced string	*/
# define CF_TAB		0x0010	/* Tab character		*/
# define CF_QUOTE	0x0020	/* Quote character		*/
# define CF_PROMPT	0x0040	/* Part of the prompt string	*/
# define CF_ENDT	0x0080	/* This char ends a token	*/
# define CF_TSKIP	0x0100	/* Tab skip			*/





static bool F_echo = FALSE;	/* Are we doing echoed output? */

/*
 * Despite the multitude of input streams we may have, there is one output
 * stream that lasts forever.  This is it.
 */
static LUN Out_lun = 0;
static bool Out_tty;
static bool In_tty;		/* Is our base src a terminal	*/

/*
 * Information concerning the current position of the cursor.
 */
static bool Redo = FALSE;	/* Do we need to redraw the line? */

/*
 * The recall buffer.
 */
# define NRECALL 30		/* Number of lines to remember		*/
static char *Recall[NRECALL];	/* Recall info				*/
static int Rptr = 0;		/* Next line to recall			*/
static int Wptr = 0;		/* Next line to store			*/

/*
 * We are running in "true" interactive mode iff our top input source is
 * a terminal.
 */
# define INTERACTIVE	(Cs->cs_input && Cs->cs_input->s_type == IST_TTY)

static int Initialized = FALSE;

# ifdef LOGGING
static int Logging = FALSE;
FILE *Log_fp;
# endif

/*
 * Prototypes
 */
void ut_fix_pb (void);
void ut_crlf (void);
void ut_do_reline (void);
void ut_drain_ta (void);
void ut_redo (char *line);
void ut_new_state (struct token_context *ctx, int state);
void ut_clr_back (struct token_context *ctx);






void
ut_init (interact, nokeypad)
bool interact, nokeypad;
/*
 * Initialize the tokenizer.
 */
{
	struct input_stack *inp;
/*
 * Set up indirect variables.
 */
 	usy_c_indirect (Ui_variable_table, "ui$echo", &F_echo,
				SYMT_BOOL, 0);
 	usy_c_indirect (Ui_variable_table, "echo", &F_echo, SYMT_BOOL, 0);
/*
 * Clear out the recall buffer.
 */
 	for (Wptr = 0; Wptr < NRECALL; Wptr++)
		Recall[Wptr] = 0;
	Wptr = 0;
	Bol = Initialized = TRUE;
/*
 * If the INTERACT flag is false, we open no streams now.
 */
	In_tty = FALSE;
 	if (! interact)
		return;
/*
 * Set up an initial source stack.
 */
 	inp = ucs_input ();
	inp->s_xp = 0;
	inp->s_pb = 0;
	inp->s_snarf = FALSE;
/*
 * Now set up our actual sources.
 */
 	if (! batch ())
	{
		tty_set ();
		if (! nokeypad)
			tty_kpon ();
		Out_tty = TRUE;
	/*
	 * There are two possible cases here.  We could be a truly interactive
	 * job, or we could be within a command file, and expected to read
	 * our input from there.
	 */
	 	if (isatty (0))
		{
			strcpy (inp->s_name, "(Terminal)");
			In_tty = TRUE;
			inp->s_type = IST_TTY;
		}
		else
		{
			strcpy (inp->s_name, "(standard input)");
			inp->s_lun = stdin;
			inp->s_type = IST_FILE;
		}
	}
/*
 * Otherwise we do file I/O for both input and output.
 */
	else
	{
		strcpy (inp->s_name, IN_STD);
		Out_tty = FALSE;
		Out_lun = stdout;
		inp->s_lun = stdin;
		inp->s_type = IST_FILE;
	}
	uio_init (Out_tty, Out_lun);
# ifdef LOGGING
/*
 * Set up command logging.
 */
 	ut_linit ();	
# endif
}



void
ut_begin (prompt, subst)
char *prompt;
int subst;
/*
 * Start the process of reading a command line from the current input
 * source.
 */
{
	int i;
	struct token_context *ctx = Cs->cs_tctx;
/*
 * Basic sanity check.
 */
# ifdef notdef
	if (ctx->tc_in_line)
		ut_finish_line (TRUE);
# endif
	/* zfill (ctx, sizeof (struct token_context)); */
	memset (ctx, 0, sizeof (struct token_context));
	if (! Cs->cs_input)
		ui_error ("There is no input source!!!!!!!!");
	ctx->tc_do_sub = subst;
/*
 * Get things going.
 */
	Redo = FALSE;
 	ctx->tc_in_line = TRUE;
	Rptr = (Wptr == 0) ? NRECALL - 1 : Wptr - 1;
	ctx->tc_target = T_ILINE | T_RLINE;
	ctx->tc_t_state = TS_BOT;
	ctx->tc_tgroup = -1;
	ctx->tc_rdiff = ctx->tc_dumped = FALSE;
/*
 * If we are running off pushback, replacement text, fix it up now.
 */
 	ut_fix_pb ();
/*
 * Insert the prompt into the token buffer.
 */
	strcpy (ctx->tc_iline, prompt);
	strcpy (ctx->tc_rline, prompt);
	for (i = 0; ctx->tc_iline[i]; i++)
		ctx->tc_iflags[i] = CF_PROMPT;
	ctx->tc_iline[i] = ' ';
	ctx->tc_iflags[i] = CF_PROMPT;
	ctx->tc_rline[i] = ' ';
	ctx->tc_iprompt = ctx->tc_iindex = ctx->tc_rindex = i + 1;
	ctx->tc_sindex = 0;
	Quote = Pcount = 0;
/*
 * If we are taking screen input, put out the prompt.
 */
	if (INTERACTIVE)
	{
	/*
	 * Get the prompt onto the beginning of the screen.
	 */
	 	if (! Bol)
			ut_crlf ();
		tty_out (ctx->tc_iline, ctx->tc_iindex);
		tty_flush ();
		Bol = FALSE;
		Nlines = 0;
	}
	return;
}



void
ut_fix_pb ()
/*
 * Fix up the pushback stack to deal a little more gracefully with 
 * multi-command user-defined symbols.
 */
{
	struct input_stack *inp = Cs->cs_input;
top:
/*
 * If no replacement text, nothing to do.
 */
	if (inp->s_pb && *inp->s_pb->pb_tptr == '\0')
	{
		struct pb *pbp = inp->s_pb;
		inp->s_pb = inp->s_pb->pb_next;
		usy_rel_string (pbp->pb_text);
		relvm (pbp);
		goto top;
	}
	if (inp->s_pb == 0 || (inp->s_pb->pb_flags & TF_REPL) == 0)
		return;
/*
 * Clear out any leading blanks.
 */
 	while (*inp->s_pb->pb_tptr == ' ')
		inp->s_pb->pb_tptr++;
/*
 * Finally, clear the REPL flag, so this will be deal with as normal text.
 */
	inp->s_pb->pb_flags &= ~TF_REPL;
}




int
ut_src_int ()
/*
 * Return TRUE iff the current input source is an interactive source.
 */
{
	return (INTERACTIVE);
}



bool
ut_interactive ()
/*
 * Return TRUE iff we are running as an interactive job.
 */
{
	return (Out_tty);   /* (In_tty); */
}




void
ut_get_token (tok)
struct token *tok;
/*
 * Obtain the next token from the input stream.
 */
{
	Tcontext ctx = Cs->cs_tctx;
	unsigned char ch, ut_getch ();
	int class, flags;
	void (*action) ();
	bool setrg = FALSE;
/*
 * Get into the proper tokenizer state.  Since this state may have already
 * been set elsewhere (notably ut_continue ()), we only tweak it if it
 * is TS_DONE.
 */
 	if (ctx->tc_t_state == TS_DONE)
		ctx->tc_t_state = TS_BOT;
/*
 * Unless we are reading replacement input, increment the token group count.
 */
 	if (! ut_are_repl ())
	{
		ctx->tc_tgroup++;
		setrg = TRUE;
	}
	tok->tk_tgroup = ctx->tc_tgroup;
/*
 * Re-present our input line if needed.
 */
 	if (Redo)
		ut_do_reline ();
/*
 * now grab up a token.
 */
	repeat
	{
	/*
	 * Obtain a character from the input, and classify it.
	 */
		if ((ch = ut_getch (&flags)) == K_NOINPUT)
		{
			tok->tk_type = TT_NOINPUT;
			return;
		}
		class = ut_classify (ch);
	/*
	 * Now do something with it.
	 */
	 	if (! (action = Tst_table [class] [ctx->tc_t_state]))
			action = ut_err;
		(*action) (ch, flags, tok, class, ctx);
	} until (ctx->tc_t_state == TS_DONE);
/*
 * Set Rgroup if called for.
 */
 	if (setrg && tok->tk_type != TT_BACKUP && ctx->tc_tgroup >= 0)
		ctx->tc_rgroups[ctx->tc_tgroup] = ctx->tc_rstart;
	tok->tk_col = ctx->tc_rstart;
}


void
ut_pushback (string, flags)
char *string;
int flags;
/*
 * Push this string back into the current input stream, to be read again.
 */
{
	char *temp, *usy_string ();
	struct pb *pbp;
/*
 * Get a pushback struct and fill it in.
 */
 	pbp = (struct pb *) getvm (sizeof (struct pb));
	pbp->pb_text = usy_string (string);
	pbp->pb_tptr = pbp->pb_text;
	pbp->pb_flags = flags;
/*
 * Tack it onto the current source block.
 */
	if (! Cs->cs_input)
		c_panic ("Pushback with an empty input stack");
	pbp->pb_next = Cs->cs_input->s_pb;
	Cs->cs_input->s_pb = pbp;
}


int
ut_are_repl ()
/*
 * Return TRUE iff we are reading from replacement text.
 */
{
	struct input_stack *inp = Cs->cs_input;

	if (inp->s_pb == (struct pb *) 0)
		return (FALSE);
	else if (*inp->s_pb->pb_tptr == 0)
		return (inp->s_pb->pb_next != (struct pb *) 0 &&
			inp->s_pb->pb_next->pb_flags & TF_REPL);
	else
		return (inp->s_pb->pb_flags & TF_REPL);
}






void
ut_finish_line (history)
int history;
/*
 * Finish out this input line.
 * HISTORY is TRUE iff the line is to be saved in the recall list.
 */
{
	int last = (Wptr == 0) ? NRECALL - 1 : Wptr - 1;
	struct token_context *ctx = Cs->cs_tctx;
/*
 * Save this line, if it differs from the last one.
 */
	ctx->tc_iline[ctx->tc_iindex] = '\0';
	if (ctx->tc_iline[ctx->tc_iindex-1] == ' ')
		ctx->tc_iline[--ctx->tc_iindex] = '\0';
 	if (history && INTERACTIVE &&
		(Recall[last] == 0 || strcmp (Recall[last],
			ctx->tc_iline + ctx->tc_iprompt)))
	{
	 	if (Recall[Wptr])
			usy_rel_string (Recall[Wptr]);
		Recall[Wptr++] = usy_string (ctx->tc_iline + ctx->tc_iprompt);
		if (Wptr >= NRECALL)
			Wptr = 0;
	}
# ifdef LOGGING
	ut_log (Iline);
# endif
/*
 * If we are doing non-interactive output, the whole line gets put out now.
 */
 	if (! INTERACTIVE && F_echo)
		ui_printf ("%s\n", ctx->tc_iline);
/*
 * Do some cleanup.
 */
	if (! Bol)
		ut_crlf ();
	ctx->tc_in_line = FALSE;
}





unsigned char
ut_getch (flags)
int *flags;
{
	struct token_context *ctx = Cs->cs_tctx;
	unsigned char tty_readch (), *line;
	struct input_stack *inp;
top:
/*
 * See where our input is coming from.
 */
	inp = Cs->cs_input;
	if (! inp || (Cs->cs_mode != M_COMMAND && Cs->cs_mode != M_WINDOW))
		return (K_NOINPUT);
/*
 * Pull this character from the pushback text, if any exists.
 */
	if (inp->s_pb)
	{
	/*
	 * Make sure this particular entry is not done for.
	 */
		if (*inp->s_pb->pb_tptr == '\0')
		{
			struct pb *pbp = inp->s_pb;
		/*
		 * Kludgery to make user-defined commands work.
		 */
			if (inp->s_pb->pb_flags & TF_REPL &&
				(inp->s_pb->pb_next == 0 ||
			   (inp->s_pb->pb_next->pb_flags & TF_REPL) == 0))
				ctx->tc_target |= T_ILINE;
# ifdef notdef
			if (S_stack->s_pb->pb_flags & TF_INCR)
				Tgroup++;
# endif
		/*
		 * Now get rid of this entry and try again.
		 */
			inp->s_pb = pbp->pb_next;
			usy_rel_string (pbp->pb_text);
			relvm (pbp);
			if (INTERACTIVE)
				tty_flush ();
			goto top;
		}
	/*
	 * OK, get the next character.
	 */
	 	*flags = inp->s_pb->pb_flags;
		return (*inp->s_pb->pb_tptr++);
	}
/*
 * Check out the "extra" space.
 */
	*flags = 0;
	if (inp->s_xp)
	{
		char c = *inp->s_xp++;
		if (! *inp->s_xp)
			inp->s_xp = 0;
		return (c);
	}
/*
 * If this is a csave source, pull out the next line.
 */
	else if (inp->s_type == IST_CSAVE)
	{
		char *csline = ucs_g_csline ();

		if (csline)
			ut_pushback (csline, TF_NOECHO);
		goto top;
	}
/*
 * If we have a non-interactive source, we need to get in another line.
 */
	else if (inp->s_type == IST_FILE)
		return (ut_file_line (flags));
/*
 * Otherwise read a terminal character.
 */
	else
		return (tty_readch ());
}


int
ut_file_line (flags)
int *flags;
/*
 * Read in a line from the file, and return the first character therefrom.
 */
{
	int len, maxline = MAXLINE;
	struct input_stack *inp = Cs->cs_input;
/*
 * Read in a line.  Be very careful if we hit end of file.  If this file
 * is our only input in the initial command mode, we want to keep it
 * around for the time being.
 */
 	if ((len = dget (inp->s_lun, inp->s_xline, maxline)) < 0)
	{
		if (Cs->cs_next == 0 && Cs->cs_input->s_next == 0)
			return ('\032');
		ucs_z_input (TRUE);
		return (K_NOINPUT);
	}
/*
 * We have an input line.  Carefully, in order, remove ! comments, and
 * check for continuation lines.
 *
 * (7/88 jc)	Leave the comment lines in, now that the state machine
 *		can deal with them.  For the time being, this breaks
 *		the continuation mechanism when used with comments, but
 *		that is a minor problem...
 */
	for (;;)
	{
		inp->s_xline[len] = '\0';
		if (len == 0)
			return (ut_file_line (flags));
		if (inp->s_xline[len - 1] != '\\')
			break;
		len += dget (inp->s_lun, inp->s_xline + len - 1, maxline) - 1;
	}
/*
 * Put it into the input line.
 */
	inp->s_xline[len] = '\r';
	inp->s_xline[len + 1] = '\0';
	inp->s_xp = inp->s_xline + 1;
	return (inp->s_xline[0]);
}



void
ut_crlf ()
/*
 * Put out a carriage return/line feed.
 */
{
	if (Out_tty)
	{
		tty_out ("\r\n", 2);
		tty_flush ();
		Bol = TRUE;
	}
}



void
ut_reline ()
/*
 * Signal that a line redrawing is needed.
 */
{
	Redo = TRUE;
	Rptr = (Wptr == 0) ? NRECALL - 1 : Wptr - 1;
}



void
ut_do_reline ()
/*
 * Repaint the current line.
 */
{
	struct token_context *ctx = Cs->cs_tctx;
	if (INTERACTIVE)
	{
		if (! Bol)
			tty_out ("\r\n", 2);
		tty_out (ctx->tc_iline, ctx->tc_iindex);
		tty_flush ();
		Redo = Bol = FALSE;
	}
}




void
ut_backup ()
/*
 * Erase the previous character on the display.
 */
{
	if (INTERACTIVE)
	{
		tty_out ("\b \b", 3);
		tty_flush ();
	}
}



void
ut_put_msg (line, refresh)
char *line;
int refresh;
/*
 * Put this line to the output, with all due regard to incoming text.
 * Also -- highlight things.
 */
{
# ifdef LOGGING
	ut_log (line);
# endif

	if (! Initialized)
		printf ("%s\n", line);
	else if (! Out_tty)
	{
	/*
	 * For non-terminal output, tack on a newline and ship it out
	 */
		char	outline[200];

		strcpy (outline, line);
		strcat (outline, "\n");
		ui_line_out (outline);
	}
	else
	{
	/*
	 * If we are not currently at the beginning of a line, do a CRLF.
	 */
		if (! Bol)
			ut_crlf ();
	/*
	 * Put out the text of this line.
	 */
		tty_standout ();
		tty_out (line, strlen (line));
		tty_standin ();
		ut_crlf ();
	/*
	 * If a prompt is going on, refresh it.
	 */
		if (refresh)
			ut_reline ();
		else
			Nlines++;
		tty_flush ();
	}
}




void
ut_zap_token ()
/*
 * Get rid of the current token in the token buffer.
 */
{
	int pos;
	struct pb *pbp;
	struct token_context *ctx = Cs->cs_tctx;
/*
 * Do some sanity checking.
 */
	if (! ctx->tc_in_line)
		c_panic ("Zap_tok called with no prompt active");
	if (! INTERACTIVE)
		c_panic ("You tried to zap_tok on a non-int source!");
/*
 * Now delete the token.
 */
	for (pos = ctx->tc_iindex - 1; (ctx->tc_iflags[pos] & CF_BOT) == 0 &&
			pos >= ctx->tc_iprompt; pos--)
		;
	ctx->tc_iindex = pos;
	ctx->tc_rindex = ctx->tc_rgroups[ctx->tc_tgroup];
	ctx->tc_tgroup--;
	/* KLUDGE!! WRONG!! */ ctx->tc_rindex = pos;
/*
 * Finally, drain any pushed back text from the current source.  I hope
 * this is always a good thing to do.
 */
	ut_drain_ta ();
}



void
ut_complete (string)
char *string;
/*
 * Complete a token already in the token buffer.
 */
{
	int i;
	struct token_context *ctx = Cs->cs_tctx;
/*
 * Make the display look right.
 */
	if (INTERACTIVE && (ctx->tc_iflags[ctx->tc_iindex-1] & CF_REPL) == 0)
	{
		tty_out ("\b", 1);
		tty_out (string, strlen (string));
		tty_out (" ", 1);
		tty_flush ();
	}
/*
 * Now, make the buffer look right too.
 */
 	if (ctx->tc_target & T_ILINE && (ctx->tc_iflags[ctx->tc_iindex-1] &
			CF_REPL) == 0)
	{
		ctx->tc_iindex--;
		for (i = 0; string[i]; i++)
		{
			ctx->tc_iline[ctx->tc_iindex] = string[i];
			ctx->tc_iflags[ctx->tc_iindex++] = 0;
		}
		ctx->tc_iline[ctx->tc_iindex] = ' ';
		ctx->tc_iflags[ctx->tc_iindex++] = CF_ENDT;
	}
 	if (ctx->tc_target & T_RLINE)
	{
		strcpy (ctx->tc_rline + ctx->tc_rindex - 1, string);
		strcat (ctx->tc_rline + ctx->tc_rindex - 1, " ");
		ctx->tc_rindex += strlen (string);
	}
}
	



void
ut_continue ()
/*
 * Cause the parser to continue reading on the last token.
 */
{
	struct token_context *ctx = Cs->cs_tctx;

	ctx->tc_iindex--;
	ctx->tc_dumped = FALSE;
	if (ctx->tc_target & T_RLINE)
		ctx->tc_rindex--;
	ut_backup ();
	ctx->tc_t_state = ctx->tc_saved_state;
}



void
ut_drain_ta ()
/*
 * Delete any pending stuff, due to an error.  This routine is still
 * under development.
 */
{
	struct pb *pbp;
/*
 * Make sure the input stack has not disappeared altogether -- a possibility
 * in batch mode.
 */
 	if (! Initialized || ! Cs || ! Cs->cs_input)
		return;
/*
 * Clear out any pushed-back input
 */
	Cs->cs_input->s_xp = 0;
	for (pbp = Cs->cs_input->s_pb; pbp;)
	{
		struct pb *temp = pbp;
		usy_rel_string (pbp->pb_text);
		pbp = pbp->pb_next;
		relvm (temp);
	}
	Cs->cs_input->s_pb = 0;
/*
 * Zap any typeahead stored by the system, if we are running interactively.
 */
 	if (Out_tty)
		tty_zap_ta ();
}




void
ut_open_file (file, fatal)
char *file;
bool fatal;
/*
 * Open up a file and read the data therefrom.
 * Entry:
 *	FILE	is the name of the file to open.
 *	FATAL 	is true iff an error should be generated for open failures.
 */
{
	struct input_stack *src;
/*
 * Get a new source structure, and try to open the file.
 */
 	src = ucs_input ();
 	src->s_pb = 0;
	src->s_xp = 0;
	src->s_type = IST_FILE; /* Do this now, in case it fails */
	if ((src->s_lun = (LUN) dview (file)) == 0)
	{
		ucs_z_input (FALSE);
		if (fatal)
			ui_error ("Unable to open file '%s'", file);
		return;
	}
/*
 * Fill in the rest of the info.
 */
	strcpy (src->s_name, file);
	src->s_snarf = FALSE;
}




void
ut_breakout ()
/*
 * Break out of any existing file sources.  This is primarily an error-
 * recovery routine.
 */
{
	if (Bail)
		while (Cs->cs_input && Cs->cs_input->s_type != IST_TTY)
			ucs_z_input (TRUE);
	ut_drain_ta ();
}








void
ut_clr_line (howmany)
int howmany;
/*
 * Clear this many positions in the line.
 */
{
	if (INTERACTIVE)
	{
		for (; howmany > 0; howmany--)
			tty_out ("\b \b", 3);
		tty_flush ();
	}
	else
		ut_reline ();
}




void ut_perr (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle an EOS when there are missing parens.
 */
{
	if (INTERACTIVE)
		ut_put_msg ("Missing close paren(s)", TRUE);
	else
		ui_error ("Missing close paren(s)");
	ut_do_reline ();
}





void ut_qerr (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle an EOS when there are missing quotes.
 */
{
	if (INTERACTIVE)
		ut_put_msg ("Missing close quote", TRUE);
	else
		ui_error ("Missing close quote");
	ut_do_reline ();
}





void ut_up (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle the recall command.
 */
{
/*
 * Clear the line.
 */
 	ut_lkill (ch, flags, tok, class, ctx);
/*
 * Check for having passed through the entire list.
 */
 	if (Rptr == Wptr || Recall[Rptr] == (char *) 0)
		return;
/*
 * OK, find our new line, and deal with it.
 */
 	ut_redo (Recall [Rptr]);
	ut_pushback (Recall[Rptr], TF_NOECHO);
	if (--Rptr < 0)
		Rptr = NRECALL - 1;
}





void ut_down (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle the down arrow command.
 */
{
	int last = (Wptr == 0) ? NRECALL - 1 : Wptr - 1;
/*
 * Clear the line.
 */
 	ut_lkill (ch, flags, tok, class, ctx);
/*
 * Check for having passed through the entire list.
 */
	if (Rptr == last)
		return;
/*
 * OK, find our new line, and deal with it.
 */
 	if (++Rptr >= NRECALL)
		Rptr = 0;
 	ut_redo (Recall[Rptr]);
	ut_pushback (Recall[Rptr], TF_NOECHO);
}





void ut_func (ch, flags, tok, class, ctx)
unsigned char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle a function key.
 */
{
	char *name, *def, *uk_get_definition ();
/*
 * Find the name of this key, then the definition.
 */
 	name = tty_get_key_name (ch);
	if ((def = uk_get_definition (name)) == (char *) 0)
	{
		ut_err (ch, flags, tok, class, ctx);
		return;
	}
/*
 * Clear the line.
 */
 	ut_lkill (ch, flags, tok, class, ctx);
/*
 * Push back the new text.
 */
 	ut_redo (def);
	ut_pushback (def, flags | TF_NOECHO);
}





void
ut_redo (line)
char *line;
/*
 * Re-present the command line as the new line.
 */
{
/*
 * Fix up the screen.
 */
	tty_out (line, strlen (line));
	tty_flush ();
}



void
ut_list_recall ()
/*
 * Dump out the recall buffer.
 */
{
	int done = (Wptr == 0) ? NRECALL - 1 : Wptr - 1, line = Wptr;
	
	while (line != done)
	{
		if (Recall[line])
			ui_printf ("%2d %s\n", line, Recall[line]);
		if (++line >= NRECALL)
			line = 0;
	}
}









void
ut_err (ch)
char ch;
/*
 * Beep the terminal in response to some unwanted character.
 */
{
	if (INTERACTIVE)
	{
		tty_out ("\007", 1);
		tty_flush ();
	}
	else
		ui_error ("Bad char: 0x%X", ch);
}




void ut_tab (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
{
	/* ut_norm (' ', flags, tok, class); */
	int nspace = 8 - (ctx->tc_iindex % 8), i;
	char sparray[9];
	
	for (i = 0; i < nspace; i++)
		sparray[i] = ' ';
	sparray[i] = '\0';
	ut_pushback (sparray, flags);
}



void ut_wtab (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle tabs.
 */
{
	ut_norm (' ', flags, tok, class, ctx);
}



int
ut_classify (c)
unsigned char c;
/* 
 * Return the character class for this char.
 */
{
	switch (c)
	{
	   case UP_ARROW:
	   	return (CG_UP_ARROW);
	   case DOWN_ARROW:
	   	return (CG_DOWN_ARROW);
	   case LEFT_ARROW:
	   case RIGHT_ARROW:
	   	return (CG_ERROR);
	   default:
	   	return((c >= MIN_FUNCTION_KEY) ? CG_FUNCTION : Class_table[c]);
	}
}




/*
 * State table stuff below here.
 */

void
ut_echo (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Store and echo a character.
 */
{
/*
 * Stuff this character into whatever lines are appropriate.
 */
 	if (ctx->tc_target & T_ILINE)
		ctx->tc_iline[ctx->tc_iindex++] = ch;
	if (ctx->tc_target & T_RLINE)
		ctx->tc_rline[ctx->tc_rindex++] = ch;
	if (ctx->tc_target & T_SYMBUF)
		ctx->tc_symbuf[ctx->tc_sindex++] = ch;
/*
 * Echo the character, if appropriate.
 */
 	if ((flags & TF_NOECHO) == 0 && (ctx->tc_target & T_ILINE)
		&& INTERACTIVE)
	{
		tty_out (&ch, 1);
		if (! Cs->cs_input->s_pb)
			tty_flush ();
	}
}





void ut_norm (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle a normal character.
 */
{
	if (ctx->tc_target & T_ILINE)
		ctx->tc_iflags[ctx->tc_iindex] = 0;
	ut_echo (ch, flags, tok, class, ctx);
}





void ut_bot (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle a normal character that begins a token.
 */
{
	ctx->tc_rstart = ctx->tc_rindex;
	ut_new_state (ctx, TS_TOKEN);
	if (ctx->tc_target & T_ILINE)
		ctx->tc_iflags[ctx->tc_iindex] = CF_BOT;
	ut_echo (ch, flags, tok, class, ctx);
}



void ut_bcom (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Deal with the comment delimiter.
 */
{
	ctx->tc_rstart = ctx->tc_rindex;
	ut_new_state (ctx, TS_COMMENT);
	if (ctx->tc_target & T_ILINE)
		ctx->tc_iflags[ctx->tc_iindex] = CF_BOT;
	ut_echo (ch, flags, tok, class, ctx);
}



void ut_scom (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Deal with a comment right on the heels of an existing token by finishing
 * out the current token, and rereading the comment delimiter.
 */
{
	char pb[2];

	pb[0] = ch;
	pb[1] = '\0';
	ut_pushback (pb, 0);
	ut_endt (' ', flags, tok, class, ctx);
}



void ut_endt (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * A white-space character has just come in which ends this token.
 */
{
	int i;
/*
 * Fill in "tok".
 */
 	tok->tk_type = TT_NORM;
	for (i = 0; i < ctx->tc_rindex - ctx->tc_rstart; i++)
		tok->tk_string[i] = ctx->tc_rline[ctx->tc_rstart + i];
	tok->tk_string[i] = '\0';
/*
 * Get the character stashed away and echoed.
 */
	SET_IFLAGS (ctx, CF_ENDT);
	ut_echo (ch, flags, tok, class, ctx);
/*
 * Our new state is DONE.
 */
	ut_new_state (ctx, TS_DONE);
}




void ut_reos (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Return an immediate EOS or EOF.
 */
{
/*
 * Then, just finish things up.
 */
	tok->tk_type = (ch == '\r' || ch == ';') ? TT_EOS : TT_EOF;
	ut_new_state (ctx, TS_DONE);
}





void ut_sret (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle the end of a non-substituted symbol (eos).
 */
{
	tok->tk_type = TT_SYM;
	ctx->tc_rline[ctx->tc_rindex] = '\0';
	strcpy (tok->tk_string, ctx->tc_rline + ctx->tc_rstart);
	ut_new_state (ctx, TS_DONE);
}






void ut_eos (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle an EOS/EOF immediately after the token, by pushing back the EOS,
 * returning the current token.
 */
{
	ut_pushback (ch == '\r' ? "\r" : "\032", 0);
	ut_endt (' ', flags, tok, class, ctx);
}




void ut_peos (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle an EOS while in the prompt state.
 */
{
	ut_endt (' ', flags, tok, class, ctx);
}




void ut_lkill (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Deal with a line kill character.
 */
{
	int plen;
/*
 * Clean up.
 */
	tok->tk_type = TT_LKILL;
	ut_new_state (ctx, TS_DONE);
/*
 * Clean up the screen, so that only the prompt remains.
 */
 	for (plen = 0; ctx->tc_iflags[plen] & CF_PROMPT; plen++)
		;
	if (INTERACTIVE)
		ut_clr_line (ctx->tc_iindex - plen);
	Bol = FALSE;
	ctx->tc_iindex = ctx->tc_rindex = plen;
	ctx->tc_tgroup = -1;
}



void
ut_new_state (ctx, state)
int state;
struct token_context *ctx;
/*
 * Shift over to this new state.
 */
{
	ctx->tc_saved_state = ctx->tc_t_state;
	ctx->tc_t_state = state;
}




void ut_back (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * This routine handles backspaces when within a token.
 */
{
	int ip;

	if (ctx->tc_iflags[ctx->tc_iindex - 1] & CF_PROMPT)
		return;
/*
 * Go ahead and clear out the data.
 */
 	ut_clr_back (ctx);
/*
 * If we are at the first character of a token, switch to BOT state.
 */
 	if (ctx->tc_iflags[ctx->tc_iindex] & CF_BOT)
		ut_new_state (ctx, TS_BOT);
	else if (ctx->tc_iflags[ctx->tc_iindex] & CF_ENDT) /* Backup case */
	{
	/*
	 * Perform the state transistions to return out of the tokenizer
	 * with a BACKUP status.
	 */
		tok->tk_type = TT_BACKUP;
		ctx->tc_tgroup -= 2;
		ut_new_state (ctx, TS_DONE);
	/*
	 * Find the beginning of this token, and push the text back.
	 */
		for (ip = ctx->tc_iindex - 1;
				(ctx->tc_iflags[ip] & CF_BOT) == 0; ip--)
			;
		ctx->tc_iline[ctx->tc_iindex] = '\0';
		ut_pushback (ctx->tc_iline + ip, TF_NOECHO);
		ctx->tc_iindex = ip;
		ctx->tc_rindex = ctx->tc_rgroups[ctx->tc_tgroup + 1];
	}
	else if (ctx->tc_iflags[ctx->tc_iindex] & CF_QUOTE)
		ut_new_state (ctx,
			ctx->tc_t_state == TS_QUOTE ? TS_TOKEN : TS_QUOTE);
	else if (ctx->tc_iflags[ctx->tc_iindex] & CF_CPAREN)
	{
		if (ctx->tc_t_state == TS_PAREN)
			Pcount++;
		else
		{
			Pcount = 1;
			ut_new_state (ctx, TS_PAREN);
		}
	}
	else if (ctx->tc_iflags[ctx->tc_iindex] & CF_OPAREN && --Pcount <= 0)
		ut_new_state (ctx, TS_TOKEN);
}



void
ut_clr_back (ctx)
struct token_context *ctx;
/*
 * Back up one character.
 */
{
	if (ctx->tc_target & T_ILINE)
	{
		ctx->tc_iindex--;
		if (INTERACTIVE)
			ut_backup ();
	}
	if (ctx->tc_target & T_RLINE)
		ctx->tc_rindex--;
	if (ctx->tc_target & T_SYMBUF)
		ctx->tc_sindex--;
}



void ut_dbg (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Debug function.
 */
{
	int c, grp;
/*
 * Put out state info.
 */
	ut_crlf ();
	ui_nf_printf (
"State = %d, Tgrp %d, target 0x%x Rstart %d Iindex: %d Rindex %d Sindex %d\n",
		ctx->tc_t_state, ctx->tc_tgroup, ctx->tc_target,
		ctx->tc_rstart, ctx->tc_iindex, ctx->tc_rindex,ctx->tc_sindex);
	ctx->tc_iline[ctx->tc_iindex] = 0;
/*
 * Now dump out Iline, with the flag field underneath it.
 */
	ui_printf ("I: %s|\n   ", ctx->tc_iline);
	for (c = 0; c < ctx->tc_iindex; c++)
		ui_nf_printf ("%X", (ctx->tc_iflags[c] >> 4) & 0xF);
	ui_printf ("\n   ");
	for (c = 0; c < ctx->tc_iindex; c++)
		ui_nf_printf ("%X", ctx->tc_iflags[c] & 0xF);
	ui_printf ("\n");
/*
 * Put out Rline.
 */
	ctx->tc_rline[ctx->tc_rindex] = 0;
	ui_nf_printf ("R: %s|\n", ctx->tc_rline);
/*
 * Also indicate token groups under Rline.
 */
	c = 0;
	ui_nf_printf ("   ");
	for (grp = 0; grp < ctx->tc_tgroup; grp++)
	{
		for (; c < ctx->tc_rgroups[grp]; c++)
			ui_nf_printf (" ");
		ui_nf_printf ("^");
		c++;
	}
	ui_printf ("\n");
/*
 * Symbuf, if necessary.
 */
 	if (ctx->tc_target & T_SYMBUF)
	{
		ctx->tc_symbuf[ctx->tc_sindex] = '\0';
		ui_printf ("S: %s\n", ctx->tc_symbuf);
	}
/*
 * Finally, get the command line back out.
 */
	ut_reline ();
	ut_do_reline ();
}




void ut_wkill (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle the "Word kill" character.
 */
{
	int c;
/*
 * Don't back into the prompt.
 */
 	if (ctx->tc_iflags[ctx->tc_iindex - 1] & CF_PROMPT)
		return;
/*
 * If we are in the BOT state, we will be backing into the previous 
 * token.  Let's set everything up now.
 */
 	if (ctx->tc_t_state == TS_BOT)
	{
		tok->tk_type = TT_BACKUP;
		ut_new_state (ctx, TS_DONE);
		ctx->tc_tgroup -= 2;
		ctx->tc_rindex = ctx->tc_rgroups[ctx->tc_tgroup + 1];
	}
	else
	{
		ctx->tc_rindex = ctx->tc_rstart;
		if (ctx->tc_t_state != TS_PROMPT)	/* 5/87 jc */
			ut_new_state (ctx, TS_BOT);
	}
/*
 * Now search backward for the next token begin.
 */
 	for (c = ctx->tc_iindex - 1; ; c--)
		if (ctx->tc_iflags[c] & CF_PROMPT || ctx->tc_iflags[c] &CF_BOT)
			break;
	if (ctx->tc_iflags[c] & CF_PROMPT)
		c++;
/*
 * Clear up the space.
 */
 	ut_clr_line (ctx->tc_iindex - c);
	ctx->tc_iindex = c;
}



void ut_at (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle an @ at the beginning of a token.
 */
{
/*
 * Treat it as a normal character, except that we go into a special state
 * to insure that this @ gets isolated.
 */
 	ut_bot (ch, flags, tok, class, ctx);
	ut_new_state (ctx, TS_AT);
}




void
ut_atsp (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle a non-space character after an @.
 */
{
	char pbs[2];
/*
 * Put this character back in the input stream.
 */
	pbs[0] = ch; pbs[1] = '\0';
	ut_pushback (pbs, flags);
/*
 * End the @ token.
 */
 	ut_endt (' ', flags, tok, class, ctx);
}








void ut_quote (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Deal with an opening quote.
 */
{
/*
 * Stash away this quote mark.
 */
 	Quote = ch;
/*
 * Get our flag info right, and signal the state transition.
 */
	if (ctx->tc_t_state == TS_BOT)
	{
		ctx->tc_rstart = ctx->tc_rindex;
		SET_IFLAGS (ctx, CF_QUOTE | CF_BOT);
	}
	else
		SET_IFLAGS (ctx, CF_QUOTE);
	ut_new_state (ctx, TS_QUOTE);
/*
 * Echo the quote character.
 */
 	ut_echo (ch, flags, tok, class, ctx);
}




void ut_endq (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Deal with a quote from within a quoted string.
 */
{
/*
 * Make sure this is the same kind of quote that started the string, and
 * just treat it as a normal character if it is different.
 */
	if (ch != Quote)
	{
		ut_norm (ch, flags, tok, class, ctx);
		return;
	}
/*
 * OK, mark this quote, echo, and switch back to TOKEN state.
 */
		SET_IFLAGS (ctx, CF_QUOTE);
	ut_echo (ch, flags, tok, class, ctx);
	ut_new_state (ctx, TS_TOKEN);
}




void ut_help (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Deal with a plea for help.
 */
{
	int ip;
/*
 * Check for a misplaced question mark in non-interactive mode.
 */
 	if (! INTERACTIVE)
		ut_norm (ch, flags, tok, class, ctx);
/*
 * OK, get set up for a help display.
 */
 	else
	{
	/*
	 * Get something on the screen immediately so they know that the
	 * help request has been heard.
	 */
		tty_standout ();
		tty_out (" *HELP* \r\n", 10);
		tty_standin ();
		tty_flush ();
	/*
	 * Signal a reline for the next token read.
	 */
		Bol = TRUE;
		if (ctx->tc_t_state != TS_PROMPT)
			ut_reline ();
	/*
	 * Find the beginning of this token, and push the text back.
	 * KLUDGE added for now (5/87 jc) -- Since the TS_PROMPT state
	 * explicitly avoids all pushed-back input, it will not work for
	 * now to push back the current text.  We will just toss it and hope
	 * that they don't get too confused...
	 */
		if ((ctx->tc_iflags[ctx->tc_iindex - 1] & CF_PROMPT) == 0
				&& ctx->tc_t_state != TS_BOT
				&& ctx->tc_t_state != TS_PROMPT)
		{
			for (ip = ctx->tc_iindex - 1;
				(ctx->tc_iflags[ip] & CF_BOT) == 0; ip--)
				;
			ctx->tc_iline[ctx->tc_iindex] = '\0';
			ut_pushback (ctx->tc_iline + ip, 0);
			ctx->tc_iindex = ip;
			ctx->tc_rindex = ctx->tc_rstart;
		}
	/*
	 * Now mark things as "DONE" and return.
	 */
	 	tok->tk_type = TT_HELP;
		ut_new_state (ctx, TS_DONE);
	}
}




void ut_open (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Deal with an initial open parenthesis.
 */
{
/*
 * Get our flag info right, and signal the state transition.
 */
	if (ctx->tc_t_state == TS_BOT)
	{
		ctx->tc_rstart = ctx->tc_rindex;
		SET_IFLAGS (ctx, CF_OPAREN | CF_BOT);
	}
	else
		SET_IFLAGS (ctx, CF_OPAREN);
	ut_new_state (ctx, TS_PAREN);
	Pcount = 1;
/*
 * Echo the character.
 */
 	ut_echo (ch, flags, tok, class, ctx);
}



void ut_npar (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle an open paren while within a parenthesized expression.
 */
{
	Pcount++;
	ut_norm (ch, flags, tok, class, ctx);
}




void ut_close (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle a close paren within an expression.
 */
{
/*
 * Go ahead and echo this character.
 */
	if (ctx->tc_target & T_ILINE)
	 	ctx->tc_iflags[ctx->tc_iindex] = CF_CPAREN;
	ut_echo (ch, flags, tok, class, ctx);
/*
 * Decrement the paren count, and see if we are done.
 */
 	if (--Pcount <= 0)
		ut_new_state (ctx, TS_TOKEN);

}




void
ut_tok_repl (string)
char *string;
/*
 * Replace the last token with this string.
 */
{
	struct token_context *ctx = Cs->cs_tctx;
/*
 * Push it back into the input.
 */
	ut_pushback (" ", TF_NOECHO | TF_REPL | TF_INCR);
	ut_pushback (string, TF_REPL);
/*
 * Get rid of the old token in Rline.
 */
	ctx->tc_rindex = ctx->tc_rgroups[ctx->tc_tgroup];
	ctx->tc_target = T_RLINE;
	ctx->tc_rdiff = TRUE;
	ctx->tc_iflags[ctx->tc_iindex - 1] |= CF_REPL;
}




void
ut_reset ()
/*
 * Reset to the beginning of a line.
 */
{
	struct token_context *ctx = Cs->cs_tctx;
/*
 * This hack makes sure that full line comments get printed when running
 * non-interactively.
 */
 	if (! INTERACTIVE && F_echo)
	{
		ctx->tc_iline[ctx->tc_iindex] = '\0';
		ui_printf ("%s\n", ctx->tc_iline);
	}
/*
 * Now simply go back, throwing out any stored input.
 */
	ctx->tc_t_state = TS_BOT;
	ctx->tc_tgroup = -1;
	ctx->tc_iindex = ctx->tc_rindex = ctx->tc_iprompt;
}



void
ut_out_lines ()
/*
 * Dump out the input lines, in anticipation of an error message.
 */
{
	struct token_context *ctx = Cs->cs_tctx;
/*
 * Kludge to only get them out once.  This variable is reset at the begin
 * of each token.
 */
 	if (ctx->tc_dumped || ! Initialized)
		return;
	ctx->tc_dumped = TRUE;
/*
 * Get to the beginning of the line, and dump out the input line.
 */
	if (! Bol)
		ut_crlf ();
	ctx->tc_iline[ctx->tc_iindex] = '\0';
	ui_printf ("Input: %s\n", ctx->tc_iline);
# ifdef LOGGING
	ut_log (ctx->tc_iline);
# endif
/*
 * If the real line is different, dump that too.
 */
	if (ctx->tc_rdiff)
	{
		ctx->tc_rline[ctx->tc_rindex] = '\0';
# ifdef LOGGING
		ut_log (ctx->tc_rline);
# endif
		if (Out_tty)
		{
			tty_standout ();
			tty_out ("  --->", 6);
			tty_standin ();
			ui_printf (" %s\n", ctx->tc_rline);
		}
		else
			ui_printf ("  ----> %s\n",
					ctx->tc_rline + ctx->tc_iprompt);
	}
}



void ut_var (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle the string variable substitution character at the beginning of
 * a token.
 */
{
/*
 * Go ahead and echo the char.
 */
	ctx->tc_rstart = ctx->tc_rindex;
	if (ctx->tc_target & T_ILINE)
		ctx->tc_iflags[ctx->tc_iindex] = CF_BOT;
 	ut_echo (ch, flags, tok, class, ctx);
/*
 * Switch our states over.
 */
	if (ctx->tc_do_sub)
	{
	 	ctx->tc_target |= T_SYMBUF;
		ut_new_state (ctx, TS_VAR);
		ctx->tc_sindex = 0;
	}
	else
		ut_new_state (ctx, TS_VARE);
}




void ut_evar (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Handle a character which ends a string variable name.
 */
{
	union usy_value v;
	int type;
	char pbbuf[2];
/*
 * Push back this character, for now.
 */
	pbbuf[0] = ch; pbbuf[1] = '\0';
 	ut_pushback (pbbuf, flags | TF_INCR);
	/* ut_pushback (" ", TF_NOECHO | TF_REPL | TF_INCR); */
/*
 * Lookup this symbol.
 */
	ctx->tc_symbuf[ctx->tc_sindex] = 0;
 	if ((Cs->cs_arg_table == 0 || ! usy_g_symbol (Cs->cs_arg_table,
			ctx->tc_symbuf, &type, &v)) &&
	 	! usy_g_symbol (Ui_variable_table, ctx->tc_symbuf, &type, &v))
		ui_error ("Unknown symbol: %s", ctx->tc_symbuf);
	if (type != SYMT_STRING)	/* for now */
		ui_error ("Symbol '%s' is not a string type", ctx->tc_symbuf);
	ut_pushback (v.us_v_ptr, TF_REPL);
/*
 * Get rid of the old token in Rline.
 */
	ctx->tc_rindex = ctx->tc_rstart;
	ctx->tc_target = T_RLINE;
	ctx->tc_rdiff = TRUE;
	ctx->tc_iflags[ctx->tc_iindex - 1] |= CF_REPL;
	ut_new_state (ctx, TS_BOT);
}





void ut_rdrw (ch, flags, tok, class, ctx)
char ch;
int flags, class;
struct token *tok;
struct token_context *ctx;
/*
 * Redraw the current line.
 */
{
	ut_reline ();
	ut_do_reline ();
}







void
ut_int_string (prompt, tok)
char *prompt;
struct token *tok;
/*
 * Read a line of interactive input.  The given prompt will be put to
 * the screen, and a line of data will be read directly from the screen,
 * bypassing any other sort of input.
 */
{
	struct token_context *ctx = Cs->cs_tctx;
/*
 * Perform the usual sort of BOT setup.
 */
	ut_begin (prompt, FALSE);
	ctx->tc_t_state = TS_PROMPT;
	ctx->tc_rstart = ctx->tc_rindex;
/*
 * Grab the input data.
 */
	ut_get_token (tok);
	ut_finish_line (FALSE);
}




void
ut_new_ctx ()
/*
 * Throw a new tokenizer context onto the control stack.
 */
{
	Cs->cs_tctx = NEW (struct token_context);
}



void
ut_rel_ctx (ctx)
struct token_context *ctx;
/*
 * Remove the current token context from the stack.
 */
{
	relvm (ctx);
}




void
ut_done ()
/*
 * Kludge finishing routine for now.  Just resets terminal parameters,
 * so I can get some work done.  Should eventually close files too.
 */
{
	if (Out_tty)
		tty_return ();
}



# ifdef LOGGING

ut_linit ()
/*
 * Initialize the logging process.
 */
{
	union usy_value v;
	int type;
	char fn[80];

	if (! usy_g_symbol (Ui_variable_table, "user", &type, &v))
		return;
	sprintf (fn, "/rdss/ui/log/%s", v.us_v_ptr);
	if ((Log_fp = fopen (fn, "a")) == (FILE *) NULL)
		return;
	fprintf (Log_fp, "(startup)\n");
	Logging = TRUE;
}


ut_log (line)
char *line;
/*
 * Log a command line.
 */
{
	if (Logging)
		fprintf (Log_fp, "%s\n", line);
}

# endif

