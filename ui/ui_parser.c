/* 10/86 jc */
/*
 * The actual command line parser.
 */
# include <string.h>
# include "ui.h"
# include "ui_cstack.h"
# include "ui_globals.h"
# include "ui_state.h"
# include "ui_token.h"
# include "ui_commands.h"
# include "ui_expr.h"
# include "ui_error.h"

static char *Rcsid = "$Id: ui_parser.c,v 1.14 2002-07-11 22:50:44 burghart Exp $";

char *zapcase ();


/*
 * The parse loop can return any one of these statuses (stati?) from
 * any step.
 */
# define PS_OK		1	/* Everything cool, valid cmd returned	*/
# define PS_BACKUP	2	/* (int) the user has backed over this tok */
# define PS_LKILL	3	/* (int) Line kill character hit	*/
# define PS_NOINPUT	4	/* No input data			*/


/*
 * Maxcmd is the maximum length of a command list.  This variable is entered
 * into the ui variable table as ui$maxcmd.
 */
static int Maxcmd = 100;
static bool Stdebug = FALSE;


/*
 * This structure is used to communicate through symbol table traversals.
 */
struct ctnames
{
	char *test;
	char n1[40];
	char n2[40];
};


/* 
 * Prototypes
 */
void uip_reject (void);
void uip_dequote (char *cp);
void uip_release (struct ui_command *cmds);
void uip_fix_params (struct ui_command *cmds);



void
uip_init ()
/*
 * Initialize the parser module.
 */
{
/* 
 * The only thing we need to do here is to declare the local variables
 * that we dare to release to the world.
 */
 	usy_c_indirect (Ui_variable_table, "ui$maxcmd", &Maxcmd, 
			SYMT_INT, 0);
 	usy_c_indirect (Ui_variable_table, "ui$stdebug", &Stdebug, 
			SYMT_BOOL, 0);
}





struct ui_command *
uip_parse (initial, prompt, subst, retryblank)
char *initial, *prompt;
bool subst, retryblank;
/*
 * Perform a command line parse, given this initial state.
 * Entry:
 *	INITIAL	is an initial state to use.
 *	PROMPT	is the prompt.
 *	SUBST	is TRUE if string variables are to be substituted.
 *	RETRYBLANK is TRUE iff the blank lines are to be discarded.
 * Exit:
 *	If possible, a completed command has been returned.  For EOF
 *	situations, zero is returned.
 */
{
	struct state_table_entry *state;
	struct ui_command *cmds;
	int retsts;
	bool nobegin = FALSE;
/*
 * Pull the initial state pointer.
 */
	if ((state = ust_lookup_state (initial)) == 0)
		ui_error ("(BUG) ui_parse called with bogus initial state %s",
			initial);
top:
/*
 * Obtain some storage for this command.
 */
	cmds = (struct ui_command *)
			getvm (Maxcmd * sizeof (struct ui_command));
	cmds[0].uc_ctype = UTT_END;
/*
 * Now enter the parse loop.
 */
	if (! nobegin)
		ut_begin (prompt, subst);
	nobegin = FALSE;
	
	ERRORCATCH
		retsts = uip_loop_parse (state, cmds, 0, -1);
	ON_ERROR
		relvm (cmds);	/* Should we uip_release () ??? */
		RESIGNAL
	ENDCATCH
/*
 * Figure out what to do with it.
 */
	if (retsts == PS_OK)
	{
	/*
	 * Kludge to make "@" work right.
	 */
		if (cmds->uc_ctype == UTT_KW &&
				cmds->uc_v.us_v_int == UIC_READ)
		{
			ut_open_file (cmds[1].uc_v.us_v_ptr, TRUE);
			uip_fix_params (cmds + 1);
			uip_release (cmds);
			goto top;
		}
		return (cmds);
	}
/*
 * This mysterious code gets executed for abnormal returns from the parser
 * loop.  The most common such return is PS_LKILL, which happens for ^U
 * (an actual line kill) as well as with function keys, history, etc.
 */
	uip_release (cmds);
	if (retsts == PS_NOINPUT || ! retryblank)
		return ((struct ui_command *) 0);
	nobegin = TRUE;
	goto top;
}








int
uip_loop_parse (state, cmds, depth, prevgrp)
struct state_table_entry *state;
int depth, prevgrp;
struct ui_command *cmds;
/*
 * Perform one stage in the parser loop.
 * Entry:
 *	STATE	is the current parser state.
 *	DEPTH	is the parse depth.
 *	PREVGRP	is the token group of the previous token.
 * Exit:
 *	The return value is the status of the parse.
 *	The Command structure has been filled in if appropriate.
 */
{
	struct token tok;
	int kwnum, status, depthinc = 1;
	bool ambig, jump;
	char rest[40];
	struct state_action *ap = 0, udcact;
	struct ui_command *nxtcmd;
	struct state_table_entry *next_state;
/*
 * Our error reporting routine.  The idea is to easily deal with errors that
 * are fatal when dealing with file input, but not for interactive.
 */
	void (*errfun) (char *fmt, ...);

	if (ut_src_int ())
		errfun = ui_ns_error;
	else
		errfun = ui_error;

	jump = ! ut_src_int ();
/*
 * Check for command overflows.
 */
 	if (depth >= Maxcmd)
		(*errfun) ("Maximum command length (%d) exceeded", Maxcmd);
/*
 * Get the token we need.
 */
again: /* (sigh) */
	if (Stdebug)
		ut_put_msg (state->sta_name, TRUE);
	ut_get_token (&tok);
	cmds->uc_col = tok.tk_col;
/*
 * The first thing we check for is EOF.  If something special has been
 * decreed as the EOF text, we will push that back, and read it again
 * through the tokenizer.  Otherwise we turn the EOF into EOS.
 */
 	if (tok.tk_type == TT_EOF)
	{
		if (state->sta_flags & STF_EOFTXT)
		{
			ut_pushback ("\r", 0);
			ut_pushback (state->sta_eoftext, 0);
			goto again;
		}
		tok.tk_type = TT_EOS;
	}
/*
 * Maybe there is nothing at all.  The solution here is the easy one,
 * but it throws away memory used with uc_text pointers, etc.  Someday
 * this should be done right.
 */
	else if (tok.tk_type == TT_NOINPUT)
		return (PS_NOINPUT);
/*
 * Check for an EOS.  We have to be careful about how these are dealt with --
 * they are somewhat special.  There are three possibilities:
 *	- Nothing specified, in which case it is seen as unwelcome and is 
 *	  rejected.
 *	- DONE is specified, or
 *	- REJECT is specified.
 */
	if (tok.tk_type == TT_EOS)
	{
	/*
	 * Handle the "nothing specified" case.
	 */
		if (! (state->sta_flags & STF_EOS))
		{
			if (depth == 0) /* Be silent for blank line */
			{
				ut_reset ();
				ut_reline ();
				goto again;
			}
			else
			{
				ui_cl_error (jump, tok.tk_col,
					"Premature end of command");
				ut_reline ();
				goto again;
			}
		}
	/*
	 * There was some EOS info given.  Assume REJECT unless DONE is given.
	 */
		else
		{
			ap = state->sta_eosact;
			if (!(ap->act_flags & STAF_DONE))
			{
				if (ap->act_flags & STAF_MSG)
					ut_put_msg (ap->act_mtext, TRUE);
				goto again;
			}
			cmds->uc_ctype = UTT_END;
		}
	}
 /*
  * Another possibility is that the user is erasing into the previous
  * token.  If so, we need to drop back a level.
  */
	else if (tok.tk_type == TT_BACKUP)
		return (PS_BACKUP);
	else if (tok.tk_type == TT_LKILL)
	{
		cmds->uc_ctype = UTT_END;
		cmds->uc_text = 0;
		return (PS_LKILL);
	}
/*
 * Perhaps this is a non-substituted symbol, which takes in the rest
 * of the line.  (This is not currently used, but may be again, someday)
 */
 	else if (tok.tk_type == TT_SYM)
	{
		cmds->uc_ctype = UTT_SYM;
		cmds->uc_text = usy_string (tok.tk_string);
		cmds[1].uc_ctype = UTT_END;
		cmds[1].uc_text = (char *) 0;
		return (PS_OK);
	}
/*
 * Maybe this is a help request.
 */
	else if (tok.tk_type == TT_HELP)
	{
		if (state->sta_flags & STF_HELP)
			ui_help (state->sta_helpfile);
		else
			ui_printf ("Sorry, there is no help for this place\n");
		goto again;
	}
/*
 * OK, it can be reasonably safely assumed that we have a normal sort
 * of token here.  Let's see if we can match up a defined command to it.
 */
 	else if ((state->sta_flags & STF_CTABLE) &&
		uip_ct_match (state, &tok, &ambig))
	{
		if (ambig)
		{
			if (! ut_src_int ())
				ui_error ("Ambiguous command bailout");
			ut_continue ();
			goto again;
		}
		goto again;	/* Now read the real stuff. */
	}
/*
 * Look for a keyword.
 */
 	else if (uip_kw_match (state, &tok, &ambig, &ap, &kwnum, TRUE))
	{
	/*
	 * Deal with people who don't have enough to say.
	 */
		if (ambig)
		{
			if (! ut_src_int ())
				ui_error ("Ambiguous keyword bailout");
			ut_continue ();
			goto again;
		}
	/*
	 * Fill in the stuff in the command structure.
	 */
	 	cmds->uc_ctype = UTT_KW;
		cmds->uc_v.us_v_int = kwnum;
	}
/*
 * No such luck.  Let's try for a value param.
 */
 	else if (uip_vp_match (state, &tok, &ap, &cmds->uc_v,&cmds->uc_vptype))
		cmds->uc_ctype = UTT_VALUE;
/*
 * If no value param either, the only option left is "other."  Let's see
 * if such an option has been provided to us.
 */
 	else if (state->sta_flags & STF_OTHER)
	{
		cmds->uc_ctype = UTT_OTHER;
		ap = state->sta_otheract;
	}
/*
 * Hmm...here we have something that just can't be dealt with.  Let's give
 * the user another chance if we can.
 */
 	else
	{
		if (! strcmp (tok.tk_string, "@"))
		{
			ui_cl_error (jump, tok.tk_col,
				"Indirect files ('@') not allowed here");
			ut_reline ();
			ut_zap_token ();
		}
		else
		{
			ui_cl_error (jump, tok.tk_col,
				"Unknown input value '%s' -- hit ? for help",
					tok.tk_string);
			ut_reline ();
			ut_continue ();
		}
		goto again;
	}
/*
 * OK, we have presumably come through with a real action pointer here.
 * If there is a message to be passed out, let's do it here.
 */
 	if (ap->act_flags & STAF_MSG)
		ui_cl_error (FALSE, tok.tk_col, ap->act_mtext);
/*
 * If a reject is called for, do it now.
 */
 	if (ap->act_flags & STAF_REJECT)
	{
		if (! ut_src_int ())
			ui_error ("State table mandated REJECT");
		uip_reject ();
		cmds->uc_ctype = 0;
		goto again;
	}
/*
 * If this token is marked as being terminal, finish out the line and
 * be done.
 */
	if (ap->act_flags & STAF_DONE)
	{
		ut_finish_line (TRUE);
		return (PS_OK);
	}
/*
 * Unless we are instructed to ignore this token, we should now increment
 * the depth counter.
 */
 	if (! (ap->act_flags & STAF_IGNORE))
	{
		depth += depthinc;
		cmds->uc_text = usy_string (tok.tk_string);
		cmds[depthinc].uc_ctype = 0;
		nxtcmd = cmds + depthinc;
	}
	else
	{
		cmds[0].uc_ctype = 0;
		nxtcmd = cmds;
	}
/*
 * Time to parse up the rest of the line.
 */
	if ((next_state = ust_lookup_state (ap->act_next)) == 0)
		ui_error ("(BUG) Nonexistent next state: '%s' (cur '%s')",
				ap->act_next, state->sta_name);
	status = uip_loop_parse (next_state, nxtcmd, depth,
		tok.tk_tgroup);
/*
 * Now cope with what came back.
 */
 	if (status != PS_BACKUP)
		return (status);
/*
 * Some people just can't make up their minds.  This person just backed
 * up into the token that we just spent so much effort parsing.  I guess
 * we just have to do it again.
 */
 	if (! (ap->act_flags & STAF_IGNORE))
	{
		depth -= depthinc;
	 	if (cmds->uc_text)
			usy_rel_string (cmds->uc_text);
	}
	if (cmds->uc_ctype == UTT_VALUE && cmds->uc_vptype == SYMT_STRING)
		usy_rel_string (UPTR (*cmds));
	cmds->uc_ctype = UTT_END;
 	if (tok.tk_tgroup == prevgrp)
		return (PS_BACKUP);
	depthinc = 1;
	goto again;
}




void
uip_reject ()
/*
 * Do a token reject.
 */
{
	ut_zap_token ();
	ut_reline ();
}

	
	
	
	
int
uip_kw_match (state, tok, ambig, action, kwnum, complete)
struct state_table_entry *state;
struct token *tok;
bool *ambig, complete;
struct state_action **action;
int *kwnum;
/*
 * Attempt a keyword recognition.
 * Entry:
 *	STATE	is the current parser state.
 *	TOK	is the token to be interpreted.
 *	COMPLETE is TRUE iff the keyword is to be completed on the screen.
 * Exit:
 *	If the token matches one or more keywords, then:
 *		TRUE is returned.
 *		if the token matches more than one then
 *			AMBIG is returned TRUE.
 *		else
 *			AMBIG is returned false
 *			ACTION points to the action struct for this kw
 *			KWNUM is the keyword number.
 *	else
 *		FALSE is returned.
 */
{
	struct state_keyword *kwa, *match = 0;
	int nkw, len = strlen (tok->tk_string), left;
	char *string = zapcase (usy_string (tok->tk_string));
/*
 * Pass through the entire keyword array.
 */
 	kwa = state->sta_kw;
	for (nkw = 0; nkw < state->sta_nkw; nkw++)
	{
	/*
	 * Check for an exact match, in which case no ambiguity checks
	 * are done.
	 */
	 	if (! strcmp (kwa[nkw].stk_keyword, string))
		{
			match = kwa + nkw;
			break;
		}
	/*
	 * Nope, check for a partial match.
	 */
	 	if (! strncmp (kwa[nkw].stk_keyword, string, len))
		{
			if (match)
			{
				ERRORCATCH
				ui_error ("Ambiguous: matches both '%s' and '%s'",
					kwa[nkw].stk_keyword,
					match->stk_keyword);
				ENDCATCH
				ut_reline ();
				*ambig = TRUE;
				usy_rel_string (string);
				return (TRUE);
			}
			match = kwa + nkw;
		}
	}
	usy_rel_string (string);
/*
 * Now, see if anything matched.
 */
 	if (! match)
		return (FALSE);
/*
 * Return the needed info.
 */
 	*ambig = FALSE;
	*action = &match->stk_action;
	*kwnum =  match->stk_kwnum;
/*
 * Fill out the rest of the keyword.
 */
 	left = strlen (match->stk_keyword) - len;
	if (complete && left > 0)
	{
		ut_complete (match->stk_keyword + len);
		strcat (tok->tk_string, match->stk_keyword + len);
	}
	return (TRUE);
}







int
uip_vp_match (state, tok, action, v, type)
struct state_table_entry *state;
struct token *tok;
struct state_action **action;
union usy_value *v;
int *type;
/*
 * Perform a value-parameter match.
 * Entry:
 *	STATE	is the current parser state.
 *	TOK	is the token input.
 * Exit:
 *	If a match is possible:
 *		The return value is TRUE
 *		ACTION is the action to be performed.
 *		V contains the interpreted value.
 *		TYPE contains the type of that value.
 *	else
 *		The return value is FALSE
 */
{
	struct parse_tree *tree;
/*
 * If no value parameter is expected, then certainly none can be matched.
 */
 	if (state->sta_vptype == ST_NO_VP)
		return (FALSE);
/*
 * A special case is made for STRING types.
 */
 	if (state->sta_vptype == SYMT_STRING)
	{
		v->us_v_ptr = usy_string (tok->tk_string);
		uip_dequote (v->us_v_ptr);
		if (state->sta_vpact.act_flags & STAF_LOWERCASE)
			zapcase (v->us_v_ptr);
		*action = &state->sta_vpact;
		*type = SYMT_STRING;
		return (TRUE);	/* String match can't fail */
	}
/*
 * Otherwise we run through the recursive descent parser, and see what happens.
 */
 	if ((tree = ue_parse (tok->tk_string, tok->tk_col, TRUE)) == 0)
		return (FALSE);
/*
 * If this command will never be executed, do not actually evaluate the
 * parse tree -- chances are the variables are not bound right anyway, and
 * we should not signal errors.  (We do parse the expression, though, to
 * catch syntax errors.)
 *
 * (2/91 jc) Sigh.  Of course, every now and then one needs to evaluate
 *		    in any case, since the result could be the changing
 *		    of the eval flag.  I wonder how long elseif was
 *		    broken this way?
 */
	if (! Cs->cs_exec && ! (Cs->cs_next && Cs->cs_next->cs_exec &&
			(state->sta_vpact.act_flags & STAF_EVAL)))
	{
		*type = SYMT_INT;
		v->us_v_int = 0;
		*action = &state->sta_vpact;
		ue_rel_tree (tree);
		return (TRUE);
	}
/*
 * Evaluate the parse tree we got, and see what we can make of it.
 */
	ERRORCATCH
	 	ue_eval (tree, v, type);
	ON_ERROR
		ue_rel_tree (tree);
		err_resignal ();
	ENDCATCH
/*
 * OK, now clean up, and get our answer into the form needed by the 
 * state table entry.
 */
 	ue_rel_tree (tree);
	if (state->sta_vptype != SYMT_UNDEFINED && *type != state->sta_vptype)
	{
		uit_coerce (v, *type, state->sta_vptype);
		*type = state->sta_vptype;
	}
	*action = &state->sta_vpact;
	return (TRUE);
}




void
uip_dequote (cp)
char *cp;
/*
 * If this string is enclosed in quotes, remedy the situation now.
 */
{
	if (*cp == '\'' || *cp == '\"')
	{
		char *quote = (char *)strchr (cp + 1, *cp);
		if (! quote)
			ui_error ("Missing close quote");
		if (quote[1] == '\0')
			*quote = '\0';
		else
			strcpy (quote, quote + 1);
		strcpy (cp, cp + 1);
	}
}



void
uip_release (cmds)
struct ui_command *cmds;
/*
 * Release this command structure.
 */
{
	struct ui_command *cp = cmds;
/*
 * We need to pass through the list, and release any dynamic strings.
 */
	while (cp->uc_ctype != UTT_END)
	{
		if (cp->uc_ctype == UTT_VALUE && cp->uc_vptype == SYMT_STRING)
			usy_rel_string (cp->uc_v.us_v_ptr);
		if (cp->uc_text)
			usy_rel_string (cp->uc_text);
		cp++;
	}
	relvm (cmds);
}




struct ui_command *
uip_clone_clist (cmds)
struct ui_command *cmds;
/*
 * Reproduce this command list.
 */
{
	struct ui_command *cp, *new;
	int ncmd;
/*
 * First, go through and count the commands.
 */
	ncmd = 1;
 	for (cp = cmds; cp->uc_ctype != UTT_END; cp++)
		ncmd++;
/*
 * Now allocate the storage for the new list.
 */
 	new = (struct ui_command *) getvm (ncmd * sizeof (struct ui_command));
	cp = new;
	do
	{
		*cp = *cmds;
		if (cmds->uc_ctype != UTT_END && cmds->uc_text)
			cp->uc_text = usy_string (cmds->uc_text);
		if (cmds->uc_ctype == UTT_VALUE &&
					cmds->uc_vptype == SYMT_STRING)
			cp->uc_v.us_v_ptr = usy_string (cmds->uc_v.us_v_ptr);
		cp++; cmds++;
	} while (cmds[-1].uc_ctype != UTT_END);

	return (new);
}



int
uip_ct_match (ste, tok, ambig)
struct state_table_entry *ste;
struct token *tok;
bool *ambig;
/*
 * Try to match a user defined command.
 * Entry:
 *	STE	is the current state table entry.
 *	TOK	is the current input token, of type TT_NORM.
 * Exit:
 *	If a command was matched, then:
 *		The return value is TRUE
 *		If the symbol was ambiguous,
 *			AMBIG is TRUE
 *		else
 *			The tokenizer is primed to read the real value
 *			of this command.
 *	else
 *		The return value is FALSE.
 */
{
	struct ctnames names;
	int uip_check_cmd (), len = strlen (tok->tk_string), left;
	char *clist, *uct_lookup_command ();
/*
 * Get set up, then search through the table.
 */
	names.test = zapcase (usy_string (tok->tk_string));
 	names.n1[0] = names.n2[0] = '\0';
	usy_traverse (Ui_command_table, uip_check_cmd, (long) &names, FALSE);
	usy_rel_string (names.test);
/*
 * Check for the multiple and no match situations.
 */
 	if (names.n2[0])
	{
		*ambig = TRUE;
		ERRORCATCH
			ui_error ("Ambiguous: matches both '%s' and '%s'",
				names.n1, names.n2);
		ENDCATCH
		ut_reline ();
		return (TRUE);
	}
	else
		*ambig = FALSE;
	if (! names.n1[0])
		return (FALSE);
/*
 * We have a single match.  Get the associated command list.
 */
	if ((clist = uct_lookup_command (names.n1)) == 0)
		c_panic ("Command '%s' disappeared!", names.n1);
/*
 * Complete the command on the display.
 */
 	left = strlen (names.n1) - len;
	if (left > 0)
		ut_complete (names.n1 + len);
/*
 * Dump the real value of the command back into the parser.
 */
 	ut_tok_repl (clist);
	return (TRUE);
}



int
uip_check_cmd (sym, type, v, np)
char *sym;
int type;
union usy_value *v;
struct ctnames *np;
/*
 * Do a symbol-to-token compare.  This routine is called out of
 * usy_traverse.
 */
{
/*
 * Make sure nobody has been putting funky things in the command table.
 */
	if (type != SYMT_STRING)
		ui_error ("Weird symbol '%s' has type %d in command table",
				sym, type);
/*
 * Check for an exact match.
 */
	if (! strcmp (np->test, sym))
	{
		strcpy (np->n1, sym);
		np->n2[0] = '\0';
		return (FALSE);
	}
/*
 * Check for an abbreviated match.
 */
	if (! strncmp (np->test, sym, strlen (np->test)))
	{
		if (! np->n1[0])
			strcpy (np->n1, sym);
		else if (! np->n2[0])
			strcpy (np->n2, sym);
		else
			return (FALSE);
	}
	return (TRUE);
}





void
uip_fix_params (cmds)
struct ui_command *cmds;
/*
 * Assign positional parameters for this command file.
 */
{
	int pnum;
	char pstr[20];
	union usy_value v;
/*
 * Just loop through them.
 */
 	for (pnum = 0; cmds->uc_ctype != UTT_END; cmds++)
	{
		sprintf (pstr, "%d", pnum++);
		usy_s_symbol (Ui_variable_table, pstr, SYMT_STRING,
			&cmds->uc_v);
	}
	v.us_v_int = pnum;
	usy_s_symbol (Ui_variable_table, "nparam", SYMT_INT, &v);
}
