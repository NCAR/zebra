/* 5/87 jc */
/*
 * Routines for handling interactive prompting.
 */
# include "ui.h"
# include "ui_error.h"
# include "ui_token.h"
# include "ui_date.h"
# include "ui_state.h"
# include "ui_globals.h"
# include "ui_commands.h"
# include "ui_expr.h"
# include "ui_cstack.h"

static char *Rcsid = "$Id: ui_prompt.c,v 1.8 1999-06-25 19:21:01 burghart Exp $";

/*
 * Prototypes
 */
void ui_pr_cc (void);	/* Keyboard interrupt handler.	*/
void ui_cp_date (struct ui_command *cmds);
void ui_cp_string (struct ui_command *cmds);
void ui_cp_yn (struct ui_command *cmds);

extern void ui_ns_error (char *fmt, ...);





int
ui_int_prompt (prompt, helpfile, lower, upper, def)
char *prompt, *helpfile;
int lower, upper, def;
/*
 * Perform an interactive prompt for an integer value.
 * Entry:
 *	PROMPT	is the prompt to display to the terminal
 *	HELPFILE is the name of a file containing help information.  This
 *		argument may be NULL, in which case the user will be 
 *		disappointed if they ask for help...
 *	LOWER	is the lowest acceptible integer input value.
 *	UPPER	is the highest acceptible value.
 *	DEF	is the value to be used if a simple carriage return
 *		is typed.
 * Exit:
 *	The return value is the number typed in by the user.
 *
 * Note that interactive prompting is very difficult to do in batch mode.
 * An attempt to do so will cause an error to be signalled.
 */
{
	struct token tok;
	struct parse_tree *tree;
	char rprompt[80];
	union usy_value v;
	int type;
/*
 * Sanity check.
 */
 	if (! ut_interactive ())
		ui_error ("(BUG): Attempt to do prompting in batch mode");
/*
 * Actual input is done within a loop in case they screw it up the
 * first time.
 */
	for (;;)
	{
	/*
	 * Put together the real prompt, and get something from the
	 * tokenizer.
	 */
	 	sprintf (rprompt, "%s [Default %d]: ", prompt, def);
	 	uii_set_handler (ui_pr_cc, TRUE);
		ucs_tty_cmode ();
	 	ut_int_string (rprompt, &tok);
		ucs_pop_cstack ();
		uii_clear_handler (ui_pr_cc);
	/*
	 * Look for a help request.
	 */
	 	if (tok.tk_type == TT_HELP)
		{
			if (helpfile)
				ui_help (helpfile);
			else
				ui_printf ("Sorry, no help available here.\n");
			continue;
		}
	/*
	 * Now attempt to interpret what we got.  We start with the
	 * case of a simple <RETURN>, which is interpreted as the default
	 * value.
	 */
	 	if (tok.tk_string[0] == '\0')
		{
			type = SYMT_INT;
			v.us_v_int = def;
		}
	/*
	 * Otherwise we need to go through the whole parser routine.
	 */
	 	else
		{
			tree = (struct parse_tree *) NULL;
			ERRORCATCH
				tree = ue_parse (tok.tk_string,
					strlen (rprompt) + 1, TRUE);
				ue_eval (tree, &v, &type);
			ON_ERROR
				if (tree)
					ue_rel_tree (tree);
				ui_epop ();
				continue;
			ENDCATCH
			ue_rel_tree (tree);
		}
	/*
	 * Try to insure that we have a value of the right type.
	 */
		ERRORCATCH
		 	if (type != SYMT_INT)
				uit_coerce (&v, type, SYMT_INT);
		ON_ERROR
			ui_epop ();
			continue;
		ENDCATCH;
	/*
	 * Make sure we are within the allotted limits.
	 */
	 	if (lower != upper &&
			(v.us_v_int < lower || v.us_v_int > upper))
		{
			ui_ns_error ("Input value must be between %d and %d",
				lower, upper);
			continue;
		}
	/*
	 * If we actually got through all that, we can return the given
	 * value.
	 */
	 	return (v.us_v_int);
	}
}






double
ui_float_prompt (prompt, helpfile, lower, upper, def)
char *prompt, *helpfile;
float lower, upper, def;
/*
 * Perform an interactive prompt for a floating-point value.
 * Entry:
 *	PROMPT	is the prompt to display to the terminal
 *	HELPFILE is the name of a file containing help information.  This
 *		argument may be NULL, in which case the user will be 
 *		disappointed if they ask for help...
 *	LOWER	is the lowest acceptible real input value.
 *	UPPER	is the highest acceptible value.
 *	DEF	is the value to be used if a simple carriage return
 *		is typed.
 * Exit:
 *	The return value is the number typed in by the user.
 *
 * Note that interactive prompting is very difficult to do in batch mode.
 * An attempt to do so will cause an error to be signalled.
 */
{
	struct token tok;
	struct parse_tree *tree;
	char rprompt[80];
	union usy_value v;
	int type;
/*
 * Sanity check.
 */
 	if (! ut_interactive ())
		ui_error ("(BUG): Attempt to do prompting in batch mode");
/*
 * Actual input is done within a loop in case they screw it up the
 * first time.
 */
	for (;;)
	{
	/*
	 * Put together the real prompt, and get something from the
	 * tokenizer.
	 */
	 	sprintf (rprompt, "%s [Default %.2f]: ", prompt, def);
		uii_set_handler (ui_pr_cc, TRUE);
		ucs_tty_cmode ();
	 	ut_int_string (rprompt, &tok);
		ucs_pop_cstack ();
		uii_clear_handler (ui_pr_cc);
	/*
	 * Look for a help request.
	 */
	 	if (tok.tk_type == TT_HELP)
		{
			if (helpfile)
				ui_help (helpfile);
			else
				ui_printf ("Sorry, no help available here.\n");
			continue;
		}
	/*
	 * Now attempt to interpret what we got.  We start with the
	 * case of a simple <RETURN>, which is interpreted as the default
	 * value.
	 */
	 	if (tok.tk_string[0] == '\0')
		{
			type = SYMT_FLOAT;
			v.us_v_float = def;
		}
	/*
	 * Otherwise we need to go through the whole parser routine.
	 */
	 	else
		{
			tree = (struct parse_tree *) NULL;
			ERRORCATCH
				tree = ue_parse (tok.tk_string,
					strlen (rprompt) + 1, TRUE);
				ue_eval (tree, &v, &type);
			ON_ERROR
				if (tree)
					ue_rel_tree (tree);
				ui_epop ();
				continue;
			ENDCATCH
			ue_rel_tree (tree);
		}
	/*
	 * Try to insure that we have a value of the right type.
	 */
		ERRORCATCH
		 	if (type != SYMT_FLOAT)
				uit_coerce (&v, type, SYMT_FLOAT);
		ON_ERROR
			ui_epop ();
			continue;
		ENDCATCH;
	/*
	 * Make sure we are within the allotted limits.
	 */
	 	if (lower != upper &&
			(v.us_v_float < lower || v.us_v_float > upper))
		{
			ui_ns_error (
				"Input value must be between %.2f and %.2f",
				lower, upper);
			continue;
		}
	/*
	 * If we actually got through all that, we can return the given
	 * value.
	 */
	 	return ((double) v.us_v_float);
	}
}




void
ui_string_prompt (prompt, helpfile, dest, def)
char *prompt, *helpfile, *dest, *def;
/*
 * Perform an interactive prompt for a string value.
 * Entry:
 *	PROMPT	is the prompt to display to the terminal
 *	HELPFILE is the name of a file containing help information.  This
 *		argument may be NULL, in which case the user will be 
 *		disappointed if they ask for help...
 *	DEST	is the place to put the destination string.
 *	DEF	is the value to be used if a simple carriage return
 *		is typed.  If DEF is NULL, there is no default.
 * Exit:
 *	The return value is the string typed in by the user.
 *
 * Note that interactive prompting is very difficult to do in batch mode.
 * An attempt to do so will cause an error to be signalled.
 */
{
	struct token tok;
	char rprompt[80];
/*
 * Sanity check.
 */
 	if (! ut_interactive ())
		ui_error ("(BUG): Attempt to do prompting in batch mode");
/*
 * Put together the real prompt, and get something from the
 * tokenizer.
 */
 	for (;;)
	{
	/*
	 * Get the info.
	 */
		sprintf (rprompt, def ? "%s [Default '%s']:" : "%s:", prompt,
				def);
		uii_set_handler (ui_pr_cc, TRUE);
		ucs_tty_cmode ();
		ut_int_string (rprompt, &tok);
		ucs_pop_cstack ();
		uii_clear_handler (ui_pr_cc);
		if (tok.tk_type != TT_HELP)
			break;
	/*
	 * Oops, they want help.
	 */
	 	if (helpfile)
			ui_help (helpfile);
		else
			ui_printf ("Sorry, no help available here.  You're on your own.\n");
	}
/*
 * Now attempt to interpret what we got.  We start with the
 * case of a simple <RETURN>, which is interpreted as the default
 * value.
 */
	if (tok.tk_string[0] == '\0' && def)
		strcpy (dest, def);
	else
		strcpy (dest, tok.tk_string);
}





void
ui_date_prompt (prompt, helpfile, val, def)
char *prompt, *helpfile;
date *val, *def;
/*
 * Perform an interactive prompt for a date value.
 * Entry:
 *	PROMPT	is the prompt to display to the terminal
 *	HELPFILE is the name of a file containing help information.  This
 *		argument may be NULL, in which case the user will be 
 *		disappointed if they ask for help...
 *	VAL	is the returned date value.
 *	DEF	is the value to be used if a simple carriage return
 *		is typed.
 * Exit:
 *	The return value is the number typed in by the user.
 *
 * Note that interactive prompting is very difficult to do in batch mode.
 * An attempt to do so will cause an error to be signalled.
 */
{
	struct token tok;
	struct parse_tree *tree;
	char rprompt[80], dbuf[30];
	union usy_value v;
	int type;
/*
 * Sanity check.
 */
 	if (! ut_interactive ())
		ui_error ("(BUG): Attempt to do prompting in batch mode");
/*
 * Actual input is done within a loop in case they screw it up the
 * first time.
 */
	for (;;)
	{
	/*
	 * Put together the real prompt, and get something from the
	 * tokenizer.
	 */
		if (def)
		{
		 	ud_format_date (dbuf, def, UDF_FULL);
		 	sprintf (rprompt, "%s [Default %s]: ", prompt, dbuf);
		}
		else
			sprintf (rprompt, "%s: ", prompt);

		uii_set_handler (ui_pr_cc, TRUE);
		ucs_tty_cmode ();
	 	ut_int_string (rprompt, &tok);
		ucs_pop_cstack ();
		uii_clear_handler (ui_pr_cc);
	/*
	 * Look for a help request.
	 */
	 	if (tok.tk_type == TT_HELP)
		{
			if (helpfile)
				ui_help (helpfile);
			else
				ui_printf ("Sorry, no help available here.\n");
			continue;
		}
	/*
	 * Now attempt to interpret what we got.  We start with the
	 * case of a simple <RETURN>, which is interpreted as the default
	 * value.
	 */
	 	if (tok.tk_string[0] == '\0')
		{
			type = SYMT_DATE;
			v.us_v_date = *def;
		}
	/*
	 * Otherwise we need to go through the whole parser routine.
	 */
	 	else
		{
			tree = (struct parse_tree *) NULL;
			ERRORCATCH
				tree = ue_parse (tok.tk_string,
					strlen (rprompt) + 1, TRUE);
				ue_eval (tree, &v, &type);
			ON_ERROR
				if (tree)
					ue_rel_tree (tree);
				ui_epop ();
				continue;
			ENDCATCH
			ue_rel_tree (tree);
		}
	/*
	 * Try to insure that we have a value of the right type.
	 */
		ERRORCATCH
		 	if (type != SYMT_DATE)
				uit_coerce (&v, type, SYMT_DATE);
		ON_ERROR
			ui_epop ();
			continue;
		ENDCATCH;
	/*
	 * If we actually got through all that, we can return the given
	 * value.
	 */
		*val = v.us_v_date;
	 	return;
	}
}




int
ui_kw_prompt (prompt, helpfile, state, def)
char *prompt, *helpfile, *state, *def;
/*
 * Prompt for an input keyword, using the kw table associated with a
 * given parser state.
 * Entry:
 *	PROMPT	is the prompt to put on the screen.
 *	HELPFILE is a file of help information.
 *	STATE	is the name of a parser state to use for keyword matches.
 *	DEF	is the default keyword.
 * Exit:
 *	The return value is the number associated with the keyword chosen
 *	by the user.
 */
{
	char rprompt[80];
	struct state_table_entry *ste;
	struct state_action *action;
	struct token tok;
	bool ambig;
	int kwnum;
/*
 * Sanity check.
 */
 	if (! ut_interactive ())
		ui_error ("(BUG): Attempt to do prompting in batch mode");
/*
 * Perform a state lookup to insure that the state exists, and that it
 * has a keyword array.
 */
	if ((ste = ust_lookup_state (state)) ==
			(struct state_table_entry *) NULL)
		ui_error ("(BUG) Bogus prompt state '%s'", state);
	if (ste->sta_nkw == 0)
		ui_error ("(BUG) State '%s' has no keywords!", state);
/*
 * Actual input is done within a loop in case they screw it up the
 * first time.
 */
	for (;;)
	{
	/*
	 * Put together the real prompt, and get something from the
	 * tokenizer.
	 */
	 	sprintf (rprompt, "%s [Default '%s']: ", prompt, def);
		uii_set_handler (ui_pr_cc, TRUE);
		ucs_tty_cmode ();
	 	ut_int_string (rprompt, &tok);
		ucs_pop_cstack ();
		uii_clear_handler (ui_pr_cc);
		if (tok.tk_string[0] == '\0')
			strcpy (tok.tk_string, def);
	/*
	 * Look for a help request.
	 */
	 	if (tok.tk_type == TT_HELP)
		{
			if (helpfile)
				ui_help (helpfile);
			else
				ui_printf ("Sorry, no help available here.\n");
			continue;
		}
	/*
	 * See if we can match a keyword.
	 */
	 	if (! uip_kw_match (ste, &tok, &ambig, &action, &kwnum, FALSE))
		{
			ui_ns_error ("Unknown input '%s' -- hit '?' for help",
				tok.tk_string);
			continue;
		}
		else if (ambig)
			continue;
	/*
	 * We got one.  Nifty.
	 */
		return (kwnum);
	}
}




void
ui_pr_cc (void)
/*
 * The ^C handler used during short periods of time to properly
 * catch interrupts.
 */
{
	ut_finish_line (FALSE);
	ucs_pop_cstack ();
	ui_error ("Prompt aborted by ^C");
}



void
ui_cprompt (cmds)
struct ui_command *cmds;
/*
 * Handle the user-level PROMPT command.
 */
{
	int i;
	float def = 0.0;
	float lower = -999999.9, upper = 999999.9;
	union usy_value v;
	char *helpfile = (char *) NULL;
	stbl dest = Ui_variable_table;
/*
 * Check for exotic types, which are handled separately.
 */
	switch (UKEY (cmds[1]))
	{
	   case UIC_DATE:
		ui_cp_date (cmds);
		return;
	   case UIC_STRING:
		ui_cp_string (cmds);
		return;
	   case UIC_Y_OR_N:
	   	ui_cp_yn (cmds);
		return;
	}
/*
 * Pass through the optional parts of the command.
 */
 	for (i = 3; cmds[i].uc_ctype != UTT_END; i++)
	{
		switch (UKEY (cmds[i]))
		{
		   case UIC_LOWER:
		   	lower = UFLOAT (cmds[++i]);
			break;
		   case UIC_UPPER:
		   	upper = UFLOAT (cmds[++i]);
			break;
		   case UIC_DEFAULT:
		   	def = UFLOAT (cmds[++i]);
			break;
		   case UIC_HELPFILE:
		   	helpfile = UPTR (cmds[++i]);
			break;
		}
	}
/*
 * Do the actual prompting.
 */
 	if (UKEY (cmds[1]) == UIC_INTEGER)
		v.us_v_int = ui_int_prompt (UPTR (cmds[2]), helpfile,
			(int) lower, (int) upper, (int) def);
	else
		v.us_v_float = ui_float_prompt (UPTR (cmds[2]), helpfile,
			lower, upper, def);
/*
 * Finally, define a symbol with the result.
 */
	if (Cs->cs_arg_table && usy_defined (Cs->cs_arg_table, UPTR (*cmds)))
		dest = Cs->cs_arg_table;
	usy_s_symbol (dest, UPTR (cmds[0]),
		UKEY (cmds[1]) == UIC_REAL ? SYMT_FLOAT : SYMT_INT, &v);
}



void
ui_cp_date (cmds)
struct ui_command *cmds;
/*
 * Handle user-level date prompts.
 */
{
	int i;
	date def;
	union usy_value v;
	stbl dest = Ui_variable_table;
	char *helpfile = (char *) NULL;
	
	pmu_g_now (&def.ds_yymmdd, &def.ds_hhmmss); /* get default default */
/*
 * Pass through the optional parts of the command.
 */
 	for (i = 3; cmds[i].uc_ctype != UTT_END; i++)
	{
		switch (UKEY (cmds[i]))
		{
		   case UIC_DEFAULT:
		   	def = UDATE (cmds[++i]);
			break;
		   case UIC_HELPFILE:
		   	helpfile = UPTR (cmds[++i]);
			break;
		}
	}
/*
 * Do the actual prompting.
 */
	ui_date_prompt (UPTR (cmds[2]), helpfile, &v.us_v_date, &def);
/*
 * Finally, define a symbol with the result.
 */
	if (Cs->cs_arg_table && usy_defined (Cs->cs_arg_table, UPTR (*cmds)))
		dest = Cs->cs_arg_table;
	usy_s_symbol (dest, UPTR (cmds[0]), SYMT_DATE, &v);
}




void
ui_cp_string (cmds)
struct ui_command *cmds;
/*
 * Handle user-level string prompts.
 */
{
	int i;
	char *def = (char *) NULL, answer[200];
	union usy_value v;
	char *helpfile = (char *) NULL;
	stbl dest = Ui_variable_table;
/*
 * Pass through the optional parts of the command.
 */
 	for (i = 3; cmds[i].uc_ctype != UTT_END; i++)
	{
		switch (UKEY (cmds[i]))
		{
		   case UIC_DEFAULT:
		   	i++;
			if (cmds[i].uc_vptype != SYMT_STRING)
				ui_cl_error (TRUE, cmds[i].uc_col,
				  	"Default must be a STRING type");
		   	def = UPTR (cmds[i]);
			uip_dequote (def);
			break;
		   case UIC_HELPFILE:
		   	helpfile = UPTR (cmds[++i]);
			break;
		}
	}
/*
 * Do the actual prompting.
 */
	ui_string_prompt (UPTR (cmds[2]), helpfile, answer, def);
/*
 * Finally, define a symbol with the result.
 */
	v.us_v_ptr = answer;
	if (Cs->cs_arg_table && usy_defined (Cs->cs_arg_table, UPTR (*cmds)))
		dest = Cs->cs_arg_table;
	usy_s_symbol (dest, UPTR (cmds[0]), SYMT_STRING, &v);
}




void
ui_cp_yn (cmds)
struct ui_command *cmds;
/*
 * Handle user-level yes/no prompts.
 */
{
	int i, ans;
	bool def = FALSE;
	union usy_value v;
	char *helpfile = (char *) NULL;
	stbl dest = Ui_variable_table;
/*
 * Pass through the optional parts of the command.
 */
 	for (i = 3; cmds[i].uc_ctype != UTT_END; i++)
	{
		switch (UKEY (cmds[i]))
		{
		   case UIC_DEFAULT:
		   	def = (UKEY (cmds[++i]) == UIC_YES);
			break;
		   case UIC_HELPFILE:
		   	helpfile = UPTR (cmds[++i]);
			break;
		}
	}
/*
 * Do the actual prompting.
 */
	ans = ui_kw_prompt (UPTR (cmds[2]), helpfile, "ust$pr-yn-def",
		def ? "yes" : "no");
/*
 * Finally, define a symbol with the result.
 */
	v.us_v_int = (ans == UIC_YES);
	if (Cs->cs_arg_table && usy_defined (Cs->cs_arg_table, UPTR (*cmds)))
		dest = Cs->cs_arg_table;
	usy_s_symbol (dest, UPTR (cmds[0]), SYMT_BOOL, &v);
}
