/* 10/86 jc */
/*
 * This is the top level user interface stuff.  Control structure commands
 * are found herein, as well as the top-level parsing commands.  Most other
 * commands are in ui_cmds.c
 */

static char *Rcsid = "$Id: ui.c,v 1.24 2002-07-11 19:51:25 burghart Exp $";
# include "ui_param.h"
# include "ui.h"
# include "ui_error.h"
# include "ui_date.h"
# include "ui_symbol.h"
# include "ui_cstack.h"
# include "ui_globals.h"
# include "ui_commands.h"
# include "ui_loadfile.h"
# include "ui_expr.h"
# include "ui_mode.h"

# define SYSTEM_TYPE "unix"

/*
 * Jump is TRUE iff a longjmp is to be performed out of a ^C interrupt.
 * This will be the case only when we are entirely within the interface.
 */
static bool Jump = FALSE;

/*
 * This is where we save the initial state.
 */
static char Initial_state[100];
static char Prompt[100];
static int (*Initial_handler) ();
static long Initial_arg;

/*
 * The symbol table used for procedures.
 */
# define PROC_TABLE_NAME "ui$procedure_table"
static stbl Proc_table = 0;

/*
 * Within the symbol table are stored structures of this type.
 */
# define MAXPNAME 40	/* Longest procedure name */
struct procedure
{
	char p_name[MAXPNAME];		/* The actual procedure name	*/
	int p_narg;			/* Number of arguments		*/
	struct procarg *p_args;		/* Argument description		*/
	int p_ntext;			/* Number of text lines		*/
	char **p_text;			/* Actual text lines		*/
	bool p_init;			/* Loaded during initial start	*/
};

struct procarg
{
	char pa_name[MAXPNAME];		/* Name of this argument	*/
	int pa_type;			/* Argument type		*/
};


/*
 * prototypes
 */
void ui_un_init (void);
void ui_snarf_cmd (struct ui_command *cmds);
void ui_endwhile (void);
void ui_reset (void);
void ui_else (struct ui_command *cmds);
void ui_endfor (void);
void ui_finish (void);
void ui_defproc (struct ui_command *cmds);
void ui_endproc (void);
void ui_pcall (struct ui_command *cmds);
void ui_return (void);
void ui_edt (char* file);
void ui_zapproc (char *name);
void ui_endmode (int mode);
void ui_pushcmd (char *command);
void ui_cs_cmd (struct ui_command *cmds);





void
ui_init (loadfile, interact, nokeypad)
char *loadfile;
bool interact, nokeypad;
/*
 * Initialize the user interface module.  This routine must be called
 * before any other user interface functions are performed.
 * 
 * Loadfile is the file containing the state definitions for this program.
 * Interact is true iff real-time user interaction is to be performed.
 * NOKEYPAD is true iff the terminal keypad is to be left in numeric mode.
 */
{
	union usy_value v;
	char sdfname[200];
/*
 * Create an initial control stack, which puts us into command mode.  Since
 * new Cstack entries are cloned from existing ones, it behooves us to
 * be very careful about setting the first one up right.
 */
	ucs_new_entry ();
	Cs->cs_type = CST_MODE;
	Cs->cs_mode = M_COMMAND;
	Cs->cs_exec = TRUE;
	Cs->cs_done = FALSE;
	Cs->cs_input = (struct input_stack *) NULL;
	Cs->cs_csave = (struct csave *) NULL;
	Cs->cs_test = Cs->cs_tstr = Cs->cs_term = Cs->cs_cvar = (char *) NULL;
	Cs->cs_arg_table = (stbl) NULL;
	ut_new_ctx ();
/*
 * Initialize the symbol table, since not much else will work without it.
 */
 	usy_init ();
	ui_un_init ();		/* Username		*/
/*
 * Initialize the other modules of interest.
 */
	ui_errinit ();		/* Error handling 	*/
 	ust_init ();		/* state table		*/
	uip_init ();		/* Parser		*/
	uct_init ();		/* Command table	*/
	ut_init (interact, nokeypad);	/* Tokenizer	*/
	Keypad_on = (nokeypad == 0);
	ue_init ();		/* Expression parser	*/
	uii_init ();		/* Interrupt handling	*/
	uk_init ();		/* Special keys		*/
	um_init ();		/* Menus		*/
	uf_init ();		/* Functions		*/
# ifdef XSUPPORT
	uw_init ();		/* Windowing		*/
# endif
/*
 * Create the procedure table.
 */
	Proc_table = usy_c_stbl (PROC_TABLE_NAME);
/*
 * Now, pull in the designated loadfile.
 */
	ERRORCATCH
	 	if (loadfile)
			ui_load (loadfile, TRUE);
	ON_ERROR
		ui_finish ();
		RESIGNAL;
	ENDCATCH
/*
 * Set up the indirect variables.
 */
 	usy_c_indirect (Ui_variable_table, "ui$keypad", &Keypad_on,
			SYMT_BOOL, 0);
	usy_c_indirect (Ui_variable_table, "ui$initial_state", Initial_state,
			SYMT_STRING, 100);
	usy_c_indirect (Ui_variable_table, "ui$prompt", Prompt,
			SYMT_STRING, 100);
	usy_c_indirect (Ui_variable_table, "ui$bailout", &Bail, SYMT_BOOL,
			0);
	v.us_v_ptr = SYSTEM_TYPE;
	usy_s_symbol   (Ui_variable_table, "ui$system_type", SYMT_STRING, &v);
	Bail = TRUE;
/*
 * Also a variable to say where to get the UI state defs.
 */
	strcpy (sdfname, UIDIR);
	v.us_v_ptr = sdfname;
	usy_s_symbol (Ui_variable_table, "ui$rdss_root", SYMT_STRING, &v);
	strcat (sdfname, "/ui_commands");
	usy_s_symbol (Ui_variable_table, "ui$defs_file", SYMT_STRING, &v); 
/*
 * Initialize other stuff.
 */
	strcpy (Appl_name, "UI");
	Argv = 0; Argc = 0;
	Resources = 0;
/*
 * Finally, if an initialization procedure exists, execute it.
 */
	if (usy_defined (Proc_table, "ui$init"))
	{
		struct ui_command kludge[2];
		
		kludge[0].uc_ctype = UTT_VALUE;
		kludge[0].uc_v.us_v_ptr = "ui$init";
		kludge[1].uc_ctype = UTT_END;
		ui_pcall (kludge);
	}
}




void
ui_setup (name, argc, argv, resources)
char *name, **argv, *resources;
int *argc;
/*
 * Perform application-specific setup.
 */
{
	strcpy (Appl_name, name);
	Argc = argc;
	Argv = argv;
	Resources = resources;
}



void
ui_un_init ()
/*
 * Initialize the USERNAME variable.
 */
{
	char *name, *getenv();
	union usy_value v;

	if (! (name = getenv ("USER")))
		return;
	v.us_v_ptr = name;
	usy_s_symbol (Ui_variable_table, "user", SYMT_STRING, &v);
}



void
ui_get_command (initial, prompt, handler, arg)
char *initial, *prompt;
int (*handler) ();
long arg;
/*
 * This is the main user-level entry point for the user interface parsing
 * mechanism.
 * Entry:
 *	INITIAL	is the initial parser  state to use.
 *	PROMPT	is the prompt to use.
 *	HANDLER	is the routine to handle the parsed commands.
 *	ARG	is an argument to pass to the handler.
 */
{
	struct ui_command *cmds;
	void ui_cc ();
/*
 * Save this info.
 */
	strcpy (Initial_state, initial);
	strcpy (Prompt, prompt);
	Initial_handler = handler;
	Initial_arg = arg;
/*
 * Now do the actual work.
 */
	Jump = TRUE;
	uii_set_handler (ui_cc, TRUE);
	while (Cs && Cs->cs_input)
	{
		cmds = 0;
		switch (Cs->cs_mode)
		{
		/*
		 * Menu mode.
		 */
		   case M_MENU:
		   	um_do_menu (&cmds, Initial_state, Prompt);
			if (cmds && ! ui_exec_clist (cmds, TRUE))
			{
				uip_release (cmds);
				uii_clear_handler (ui_cc);
				return;
			}
			if (cmds)
				uip_release (cmds);
			break;
		/*
		 * Plain old command mode.
		 */
# ifdef XSUPPORT
		   case M_WINDOW:
# endif
		   case M_COMMAND:
		   	if (! ui_do_cmode ())
			{
				uii_clear_handler (ui_cc);
				return;
			}
			break;
		/*
		 * We should not see these here, but if we do, just return
		 * and hope somebody else deals with it.
		 */
		   case M_NONE:
		   	ui_printf ("Hmm...There is a M_NONE on the stack\n");
			return;

		   default:
		   	ui_error ("(BUG): Unknown mode: %d", Cs->cs_mode);
		}
	}
	uii_clear_handler (ui_cc);
}






ui_do_cmode ()
/*
 * Implement the "command" major mode.
 */
{
	struct ui_command *cmds, *uip_parse ();
	int ret;
	bool exec;
	void ui_cc ();
/*
 * We just keep doing this until the handler says not to.
 */
	do
	{
	/*
	 * Get set up, and make sure that we have input.  Also make sure that
	 * our current mode is COMMAND; otherwise we bail out.
	 */
		cmds = 0;
		if (! Cs || ! Cs->cs_input)
			return (FALSE);
		if (Cs->cs_mode != M_COMMAND && Cs->cs_mode != M_WINDOW)
			return (TRUE);
	/*
	 * Get a command string.
	 */
		if (! cmds)
		{
			exec = Cs->cs_exec;
			ERRORCATCH
				cmds = uip_parse (Initial_state, Prompt, exec,
							TRUE);
				if (! cmds)
				{
					ui_epop ();
					continue;
				}
			ON_ERROR
				ui_reset ();
				if (Bail && ! ut_interactive ())
				{
					uii_clear_handler (ui_cc);
					ui_epop ();
					ui_error ("Total error bailout");
				}
				continue;
			ENDCATCH
		}
	/*
	 * If we are snarfing commands, grab this one.
	 */
		if (Cs->cs_csave)
			ui_snarf_cmd (cmds);
	/*
	 * Now execute this command.
	 */
	 	ret = ui_exec_clist (cmds, exec);
	/*
	 * Finally, return the command storage.
	 */
		uip_release (cmds);
	} while (ret);

	return (FALSE);
}






ui_exec_clist (cmds, exec)
struct ui_command *cmds;
bool exec;
/*
 * Execute this command list.
 */
{
	void ui_cc ();
	int ret;
/*
 * If (1) the first element on the list is a keyword, and (2) it
 * has a negative number, then it is an internal command.  Let's
 * just handle it behind the user's back.  Otherwise pass it on to
 * the application command handler.
 */
	ERRORCATCH
		if ((cmds->uc_ctype == UTT_VALUE) ||
		   (cmds->uc_ctype == UTT_KW && cmds->uc_v.us_v_int<0))
			ret = ui_int_cmd (cmds, exec);
		else
		{
			Jump = FALSE;
			ret = exec ? (*Initial_handler)
				(Initial_arg, cmds) : TRUE;
		}
/*
 * If an error is signalled, reset the control stack, and, in non-interactive
 * cases, give up entirely (unless told not to).
 */
	ON_ERROR
		ui_reset ();
		if (Bail && ! ut_interactive ())
		{
			uii_clear_handler (ui_cc);
			ui_epop ();
			uip_release (cmds);
			ui_error ("Total error bailout");
		}
		ret = 1; /* Don't drop out of cmode */
	ENDCATCH
	Jump = TRUE;
	return (ret);
}






int 
ui_int_cmd (cmds, exec)
struct ui_command *cmds;
bool exec;
/*
 * Handle an internal command.
 * Entry:
 *	CMDS	is a parsed command sequence for an internal command.
 *	EXEC	is true iff we are currently executing non-cs comands.
 * Exit:
 *	The command has been handled.
 */
{
/*
 * If the first parameter is a value parameter, this is the '=' syntax.
 * Implement it first.
 */
 	if (cmds->uc_ctype == UTT_VALUE)
	{
	  	ui_vset (cmds, TRUE);
		return (TRUE);
	}
/*
 * Trap the control structure commands right now.
 */
	if (cmds->uc_v.us_v_int <= UIC_WHILE)
	{
		ui_cs_cmd (cmds);
		return (TRUE);
	}
/*
 * If we are currently executing, just return now.  This check is done
 * after the above, since control structure commands are always relevant.
 */
	else if (! exec)
		return (TRUE);
/*
 * Switch out, depending on the command.
 */
 	switch (cmds->uc_v.us_v_int)
	{
	/*
	 * Define state|menu|command|key|whatever...
	 */
	  case UIC_DEFINE:
		ui_define (cmds + 1);
		return (TRUE);
	/*
	 * Bailout time.  Just return FALSE, which will cause ui_get_command
	 * 		  to return back to its caller.
	 */
	  case UIC_EXIT:
		return (FALSE);
	/*
	 * Save up a load file.
	 */
	  case UIC_SAVE:
	  	ui_save (UPTR (cmds[1]), cmds[2].uc_ctype != UTT_END);
		return (TRUE);
	/*
	 * Invoke the EDT editor.
	 */
	  case UIC_EDT:
	  	ui_edt (UPTR (cmds[1]));
		return (TRUE);
	/*
	 * Pull in a load file.
	 */
	  case UIC_LOAD:
	  	ui_load (cmds[1].uc_v.us_v_ptr, FALSE);
		return (TRUE);
	/*
	 * Display something from inside the interface.
	 */
	  case UIC_DUMP:
	  	ui_dump (cmds + 1);
		return (TRUE);
	/*
	 * Read in a text command file.
	 */
	  case UIC_READ:
	  	ut_open_file (cmds[1].uc_v.us_v_ptr, TRUE);
		return (TRUE);
	/*
	 * Set something.
	 */
	  case UIC_SET:
	  case UIC_LOCAL:
	  	ui_set (cmds + 1, UKEY (cmds[0]) == UIC_LOCAL);
		return (TRUE);
	/*
	 * Testing hook.
	 */
	  case UIC_TEST:
		dump_str (UINT (cmds[1]));
		/* ui_test (cmds + 1); */
		return (TRUE);
	/*
	 * Evaluate an expression.
	 */
	  case UIC_EVAL:
	  	ui_eval (cmds[1].uc_v.us_v_ptr, cmds[1].uc_col);
		return (TRUE);
	/*
	 * List off the recall buffer.
	 */
	  case UIC_RECALL:
	  	ut_list_recall ();
		return (TRUE);
	/*
	 * Output a message to the output stream.
	 */
	  case UIC_MESSAGE:
	  	ui_message (cmds + 1);
		return (TRUE);
	/*
	 * Set a non-string variable.
	 */
	  case UIC_VSET:
	  	ui_vset (cmds + 1, TRUE);
		return (TRUE);
	/*
	 * Set a string variable.
	 */
	  case UIC_SSET:
	  	ui_vset (cmds + 1, FALSE);
		return (TRUE);
	/*
	 * Prompt for some information.
	 */
	  case UIC_PROMPT:
	  	ui_cprompt (cmds + 1);
		return (TRUE);
	/*
	 * Type out a file to the output.
	 */
	  case UIC_TYPE:
	  	ui_type_file (UPTR (cmds[1]));
		return (TRUE);
	/*
	 * Procedure calls and returns.
	 */
	   case UIC_PCALL:
		ui_pcall (cmds + 1);
		return (TRUE);
	   case UIC_RETURN:
		ui_return ();
		return (TRUE);
	/*
	 * Mode switching.
	 */
	    case UIC_MODE:
	    	ui_mode (cmds + 1);
		return (TRUE);
	    case UIC_ENDMODE:
	    	ui_endmode (0);
		return (TRUE);
	/*
	 * Keypad.
	 */
	   case UIC_KEYPAD:
	   	ui_keypad (UBOOL (cmds[1]));
		return (TRUE);
	/*
	 * Delete.
	 */
	   case UIC_DELETE:
	   	ui_delete (cmds + 1);
		return (TRUE);
# ifdef XSUPPORT
	/*
	 * Throw a widget on the screen.
	 */
	   case UIC_POPUP:
		if (cmds[2].uc_ctype != UTT_END)
		   	uw_GeomPopup (UPTR (cmds[1]), UPTR (cmds[2]));
		else
			uw_GeomPopup (UPTR (cmds[1]), NULL);

		return (TRUE);
	/*
	 * Take it off again.
	 */
	   case UIC_POPDOWN:
	   	uw_popdown (UPTR (cmds[1]));
		return (TRUE);
	/*
	 * Forms.
	 */
	   case UIC_FORMTEXT:
	   	uw_FormText (UPTR (cmds[1]), UPTR (cmds[2]), UPTR (cmds[3]));
		return (TRUE);
	   case UIC_FORMMENU:
	   	uw_FormMenu (UPTR (cmds[1]), UPTR (cmds[2]), UPTR (cmds[3]));
		return (TRUE);
# endif
	/*
	 * Otherwise complain.
	 */
	  default:
	  	ui_printf ("Huh?  kwnum = %d\n", cmds->uc_v.us_v_int);
		return (TRUE);
	}
}





void
ui_cs_cmd (cmds)
struct ui_command *cmds;
/*
 * This routine will attempt to handle the control structure commands.
 */
{
	switch (cmds->uc_v.us_v_int)
	{
	/*
	 * While . . . endwhile
	 */
	   case UIC_WHILE:
	   	ui_while (cmds);
		return;
	   case UIC_ENDWHILE:
	   	ui_endwhile ();
		return;
	/*
	 * Foreach
	 */
	   case UIC_FOREACH:
	   	ui_foreach (cmds + 1);
		return;
	   case UIC_ENDFOR:
	   	ui_endfor ();
		return;
	/*
	 * If () then ... elseif ... else ... endif
	 */
	  case UIC_IF:
	  	ui_if (cmds + 1);
		return;
	  case UIC_ENDIF:
	  	ui_endif ();
		return;
	  case UIC_ELSE:
	  	ui_else (cmds + 1);
		return;
	  case UIC_ELSEIF:
	  	ui_elseif (cmds + 1);
		return;
	/*
	 * Procedure definition.
	 */
	   case UIC_PROCEDURE:
		ui_defproc (cmds + 1);
		return;
	   case UIC_ENDPROCEDURE:
		ui_endproc ();
		return;

	  default:
	  	ui_printf ("Huh?  CSkwnum = %d\n", cmds->uc_v.us_v_int);
		return;
	}
}





void
ui_subcommand (initial, prompt, handler, arg)
char *initial, *prompt;
int (*handler) ();
long arg;
/*
 * This is an internal version of ui_get_command -- it always invokes the
 * specific handler.
 * Entry:
 *	INITIAL	is the initial parser  state to use.
 *	PROMPT	is the prompt to use.
 *	HANDLER	is the routine to handle the parsed commands.
 *	ARG	is an argument to pass to the handler.
 */
{
	struct ui_command *cmds, *uip_parse ();
	int ret;
/*
 * We just keep doing this until the handler says not to.
 */
	do
	{
	 	cmds = uip_parse (initial, prompt, TRUE, TRUE);
		if (! cmds)
			continue;	/* assume still cmode for now */
		ERRORCATCH
			ret = (*handler) (arg, cmds);
		ON_ERROR
			uip_release (cmds);
			RESIGNAL;
		ENDCATCH
		uip_release (cmds);
	} while (ret);
}




void
ui_snarf_cmd (cmds)
struct ui_command *cmds;
/*
 * Snarf up this command into the Csave structure.
 */
{
	char tmp[300];
/*
 * Glom together the command into a single string.
 */
	tmp[0] = '\0';
	while (cmds->uc_ctype != UTT_END)
	{
		if (cmds->uc_text)
		{
			strcat (tmp, cmds->uc_text);
			strcat (tmp, " ");
		}
		cmds++;
	}
	strcat (tmp, "\r");
/*
 * Now save it.
 */
	if (! Cs->cs_csave)
		c_panic ("Snarf with no stack");
	Cs->cs_csave->c_save[Cs->cs_csave->c_where++] = usy_string (tmp);
	Cs->cs_csave->c_save[Cs->cs_csave->c_where] = (char *) 0;
}




ui_while (cmds)
struct ui_command *cmds;
/*
 * Handle the beginning of a WHILE loop.
 */
{
/*
 * Get a new csinfo structure.
 */
 	ucs_new_entry ();
	Cs->cs_type = CST_WHILE;
/*
 * See if we need to begin the command snarf process.
 */
 	if (Cs->cs_input->s_type != IST_CSAVE && ! Cs->cs_csave)
	{
		Cs->cs_csave = (struct csave *) getvm (sizeof (struct csave));
		Cs->cs_csave->c_backptr = Cs;
		Cs->cs_csave->c_where = 0;
		Cs->cs_csave->c_read = -1;
		Cs->cs_csave->c_release = TRUE;
		Cs->cs_csave->c_save =
			(char **) getvm (NSAVE*sizeof (char *));
		Cs->cs_exec = FALSE;
	}
	else
		Cs->cs_exec = Cs->cs_exec && UINT (cmds[1]);
/*
 * Finish filling in the structure.
 */
	Cs->cs_test = usy_string (cmds[1].uc_text);
	Cs->cs_text = Cs->cs_csave ? 0 : Cs->cs_input->s_csave->c_read;
	Cs->cs_term = "endwhile";
	Cs->cs_mode = M_COMMAND;	/* Necessary??? */

	return (TRUE);
}




void
ui_endwhile ()
/*
 * Handle an endwhile command.
 */
{
	struct parse_tree *pt;
	int type;
	union usy_value v;
	bool exec, csave;
	struct cs_entry *csp;
/*
 * If our top structure is not a CST_WHILE, there is a nesting error.
 */
	if (Cs->cs_type != CST_WHILE)
	{
		if (Cs->cs_term)
			ui_error ("Received 'endwhile' when expecting '%s'",
				Cs->cs_term);
		else
			ui_error ("Unmatched 'endwhile'");
	}
	csp = Cs->cs_next;
/*
 * If we are snarfing, and this is the loop that started everything, turn
 * the snarfing off.
 */
	if (Cs->cs_csave && Cs->cs_csave->c_backptr == Cs)
	{
		csave = TRUE;
		ucs_csave_source (Cs->cs_csave, TRUE, TRUE);
		Cs->cs_csave = (struct csave *) 0;
	}
/*
 * OK, this is it.  Evaluate the test condition.
 */
	pt = ue_parse (Cs->cs_test, 0, TRUE);
	ue_eval (pt, &v, &type);
	ue_rel_tree (pt);
	if (type != SYMT_BOOL)
		uit_coerce (&v, type, SYMT_BOOL);
	exec = v.us_v_int && (Cs->cs_csave == 0) && 
		(csp == (struct cs_entry *) 0 || csp->cs_exec);
/*
 * If the condition evaluated false, or we are still snarfing up the
 * input data, we clean up and return.
 */
	if (! exec)
	{
		ucs_pop_cstack ();
		return;
	}
/*
 * The condition was true, meaning that we have to execute the loop again.
 */
	if (Cs->cs_input->s_type != IST_CSAVE)
		c_panic ("Endwhile: istack not csave");
	Cs->cs_input->s_csave->c_read = Cs->cs_text;
	Cs->cs_exec = TRUE;
}





void
ui_reset ()
/*
 * Reset the Control stack back to the last major mode.
 */
{
	ut_breakout ();
	while (Bail && Cs && Cs->cs_type != CST_MODE)
		ucs_pop_cstack ();
}



void
ui_cc ()
/*
 * This is the ui control/c handler.
 */
{
	ui_reset ();
	ut_reline ();
	if (Jump)
		ui_bailout ((char *) 0);
}



int
ui_if (cmds)
struct ui_command *cmds;
/*
 * Handle the IF command.
 */
{
/*
 * Simpl create a new entry on the Cstack, and fill it in.
 */
	ucs_new_entry ();
	Cs->cs_type = CST_IF;
	Cs->cs_done = Cs->cs_exec = UBOOL (*cmds) && Cs->cs_exec;
	Cs->cs_test = (char *) 0;
	Cs->cs_term = "else, elseif, or endif";
	Cs->cs_mode = M_COMMAND;
	return (TRUE);
}



int
ui_endif ()
/*
 * Handle the ENDIF command.
 */
{
/*
 * If our top structure is not a CST_IF, there is a nesting error.
 */
	if (Cs->cs_type != CST_IF)
	{
		if (Cs->cs_term)
			ui_error ("Received 'endif' when expecting '%s'",
				Cs->cs_term);
		else
			ui_error ("Unmatched 'endif'");
	}
/*
 * Just clean up the stack.
 */
	ucs_pop_cstack ();
	return (TRUE);
}



void
ui_else (cmds)
struct ui_command *cmds;
/*
 * Handle the ELSE command.
 */
{
	struct cs_entry *csp;
/*
 * If our top structure is not a CST_IF, there is a nesting error.
 */
	if (Cs->cs_type != CST_IF)
	{
		if (Cs->cs_term)
			ui_error ("Received 'else' when expecting '%s'",
				Cs->cs_term);
		else
			ui_error ("Unmatched 'else'");
	}
	csp = Cs->cs_next;
/*
 * Look at our CMDS array.  If it does not have an immediate end, then
 * what we really have here is "else if".
 */
 	if (cmds->uc_ctype != UTT_END)
	{
		ui_elseif (cmds);
		return;
	}
/*
 * Now, it is simply time to (maybe) tweak the EXEC flag.
 */
 	Cs->cs_exec = csp->cs_exec && ! Cs->cs_done;
	Cs->cs_done = TRUE;
}




ui_elseif (cmds)
struct ui_command *cmds;
/*
 * Handle the ELSEIF command.
 */
{
/*
 * If our top structure is not a CST_IF, there is a nesting error.
 */
	if (Cs->cs_type != CST_IF)
	{
		if (Cs->cs_term)
			ui_error ("Received 'elseif' when expecting '%s'",
				Cs->cs_term);
		else
			ui_error ("Unmatched 'elseif'");
	}
/*
 * If the DONE flag is set, there's nothing to do.
 */
 	if (Cs->cs_done)
	{
		Cs->cs_exec = FALSE;
		return (TRUE);
	}
/*
 * Check the new test condition.
 */
	if ((Cs->cs_done = UBOOL (*cmds)) == FALSE)
		return (TRUE);
/*
 * The test condition evaluates TRUE, and no other branch of the IF has
 * been done.  Thus, we turn execution iff not disabled by an outer loop.
 */
 	Cs->cs_exec = Cs->cs_next->cs_exec;
	return (TRUE);
}





ui_foreach (cmds)
struct ui_command *cmds;
/*
 * Handle the beginning of a FOREACH loop.
 */
{
	struct cs_entry *csp;
	char *ui_make_fortest (), *ui_parse_ftest ();
	union usy_value v;
/*
 * Put together a csinfo structure.
 */
	ucs_new_entry ();
	Cs->cs_type = CST_FOREACH;
/*
 * See if we need to begin the command snarf process.
 */
 	if (Cs->cs_input->s_type != IST_CSAVE && ! Cs->cs_csave)
	{
		Cs->cs_csave = (struct csave *) getvm (sizeof (struct csave));
		Cs->cs_csave->c_backptr = Cs;
		Cs->cs_csave->c_where = 0;
		Cs->cs_csave->c_read = -1;
		Cs->cs_csave->c_release = TRUE;
		Cs->cs_csave->c_save =
			(char **) getvm (NSAVE*sizeof (char *));
		Cs->cs_exec = FALSE;
	}
	/* else -- retain cs_exec from the stack */
/*
 * Finish filling things in.
 */
	Cs->cs_tstr = Cs->cs_test = ui_make_fortest (cmds + 1);
	Cs->cs_cvar = usy_string (cmds->uc_text);
	Cs->cs_text = Cs->cs_csave ? 0 : Cs->cs_input->s_csave->c_read;
	Cs->cs_term = "endfor";
	Cs->cs_mode = M_COMMAND;
/*
 * If we are running immediately, we need to set up the control variable.
 */
	if (Cs->cs_exec)
	{
		Cs->cs_tstr = ui_parse_ftest (Cs->cs_tstr, &v);
		if (v.us_v_ptr == (char *) 0)
			Cs->cs_exec = FALSE;
		else
			usy_s_symbol (Ui_variable_table, Cs->cs_cvar,
					SYMT_STRING, &v);
	}
	return (TRUE);
}



void
ui_endfor ()
/*
 * Handle an endfor command.
 */
{
	union usy_value v;
	bool exec;
	char *ui_parse_ftest ();
/*
 * If our top structure is not a CST_FOREACH, there is a nesting error.
 */
	if (Cs->cs_type != CST_FOREACH)
	{
		if (Cs->cs_term)
			ui_error ("Received 'endfor' when expecting '%s'",
				Cs->cs_term);
		else
			ui_error ("Unmatched 'endfor'");
	}
/*
 * If we are snarfing, and this is the loop that started everything, turn
 * the snarfing off.
 */
	if (Cs->cs_csave && Cs->cs_csave->c_backptr == Cs)
	{
		ucs_csave_source (Cs->cs_csave, TRUE, TRUE);
		Cs->cs_csave = (struct csave *) 0;
	}
/*
 * OK, this is it.  Evaluate the test condition.
 */
 	if ((! Cs->cs_tstr) || Cs->cs_csave || Cs->cs_next->cs_exec == 0)
		exec = FALSE;
	else
	{
		Cs->cs_tstr = ui_parse_ftest (Cs->cs_tstr, &v);
		usy_s_symbol (Ui_variable_table, Cs->cs_cvar,
				SYMT_STRING, &v);
		exec = TRUE;
	}
/*
 * If the condition evaluated false, or we are still snarfing up the
 * input data, we clean up and return.
 */
	if (! exec)
	{
	/*
	 * Release our resources.
	 */
		ucs_pop_cstack ();
		return;
	}
/*
 * The condition was true, meaning that we have to execute the loop again.
 */
	Cs->cs_input->s_csave->c_read = Cs->cs_text;
	Cs->cs_exec = TRUE;
}




char *
ui_make_fortest (cmds)
struct ui_command *cmds;
/*
 * Take this command string, and turn it into something that is easily
 * deal with in foreach loops.
 */
{
	int len = 0, param;
	char *line, *lp, *getvm ();
/*
 * Pass through the list once, and get a byte count.
 */
	for (param = 0; cmds[param].uc_ctype != UTT_END; param++)
		len += strlen (cmds[param].uc_text) + 1;
	lp = line = getvm (len + 2);
/*
 * Now, do it again, and copy out the stuff.
 */
 	for (param = 0; cmds[param].uc_ctype != UTT_END; param++)
	{
		strcpy (lp, cmds[param].uc_text);
		lp += strlen (cmds[param].uc_text) + 1;
	}
	*lp = '\0';
	return (line);
}




char *
ui_parse_ftest (string, v)
char *string;
union usy_value *v;
/*
 * Pull out the next entry in the foreach series.
 */
{
/*
 * Make sure there is something left.
 */
	if (! *string)
		return (v->us_v_ptr = (char *) 0);
/*
 * Return the current param, and go on to the next.
 */
 	v->us_v_ptr = string;
	string += strlen (string) + 1;
	return (*string ? string : (char *) 0);
}




void
ui_finish ()
/*
 * Perform whatever closing work may need to be done.
 */
{
	ut_done ();
}




void
ui_defproc (cmds)
struct ui_command *cmds;
/*
 * Enter the procedure definition state.
 */
{
	struct procedure *proc;
	union usy_value v;
	int narg;
/*
 * Put together a csinfo structure.
 */
 	ucs_new_entry ();
	if (Cs->cs_csave)
		ui_error ("Procedures may not be defined within loops");
	Cs->cs_type = CST_DEFPROC;
/*
 * Always turn execution off for procedure definitions.  We save the
 * proc name in the test field, so that endproc can easily find it.
 */
	Cs->cs_exec = FALSE;
	Cs->cs_test = usy_string (cmds->uc_text);
	Cs->cs_text = 0;
	Cs->cs_term = "endprocedure";
	Cs->cs_mode = M_COMMAND;
/*
 * If somehow execution has been turned off, we can quit now.
 */
	if (! Cs->cs_next->cs_exec)
		return;
/*
 * Fire up the command snarf process.
 */
	Cs->cs_csave = (struct csave *) getvm (sizeof (struct csave));
	Cs->cs_csave->c_backptr = Cs;
	Cs->cs_csave->c_where = 0;
	Cs->cs_csave->c_read = -1;
	Cs->cs_csave->c_release = TRUE;
	Cs->cs_csave->c_save =
		(char **) getvm (NSAVE*sizeof (char *));
	Cs->cs_exec = FALSE;
/*
 * Allocate a procedure structure, and begin to fill it in.
 */
	proc = (struct procedure *) getvm (sizeof (struct procedure));
	strcpy (proc->p_name, UPTR (*cmds));
	proc->p_init = proc->p_ntext = 0;
	cmds++;
/*
 * Grab the args, if any.
 */
	proc->p_narg = 0;
	if (cmds->uc_ctype != UTT_END)
	{
		for (narg = 0; cmds[narg].uc_ctype != UTT_END; narg += 2)
			proc->p_narg++; /* Count args */
		proc->p_args = (struct procarg *)
			getvm (proc->p_narg * sizeof (struct procarg));
		for (narg = 0; narg < proc->p_narg; narg++)
		{
			strcpy (proc->p_args[narg].pa_name, UPTR (*cmds));
			proc->p_args[narg].pa_type = UINT (cmds[1]);
			cmds += 2;
		}
	}
/*
 * Define the procedure in the table.
 */
	v.us_v_ptr = (char *) proc;
	ui_zapproc (proc->p_name);
	usy_s_symbol (Proc_table, proc->p_name, SYMT_POINTER, &v);
}



void
ui_endproc ()
/*
 * Handle the end of a procedure definition.
 */
{
	struct procedure *proc;
	union usy_value v;
	int type, i;
	char pcall[80];
	struct csave *csv;
/*
 * Do some checking.
 */
	if (Cs->cs_type != CST_DEFPROC)
	{
		if (Cs->cs_term)
			ui_error("Received 'endprocedure' when expecting '%s'",
				Cs->cs_term);
		else
			ui_error ("Unmatched 'endprocedure'");
	}
/*
 * If there is a previous layer that turned off execution, all we need to
 * do is to remove this entry and quit.
 */
	if (Cs->cs_next->cs_exec == FALSE)
	{
		ucs_pop_cstack ();
		return;
	}
/*
 * No such luck, we actually have to fix up this procedure.  Start by looking
 * up the proc structure we created above.
 */
	if (! usy_g_symbol (Proc_table, Cs->cs_test, &type, &v))
		c_panic ("Struct for proc '%s' disappeared!", Cs->cs_test);
	proc = (struct procedure *) v.us_v_ptr;
/*
 * The main thing we need to do is to snarf over the saved text.  Since
 * the array of pointers that we allocated initially is fairly large, it
 * is worthwhile to reallocate a smaller one.
 *
 * (Also, replace the last line, which is "endprocedure", with "return").
 */
 	csv = Cs->cs_csave;
	usy_rel_string (csv->c_save[csv->c_where - 1]);
	csv->c_save[csv->c_where - 1] = usy_string ("return\r");
	proc->p_ntext = csv->c_where;
	proc->p_text = (char **) getvm (proc->p_ntext * sizeof (char **));
	memcpy (proc->p_text, csv->c_save, proc->p_ntext*sizeof (char *));
/*
 * Define a command mapping the procedure name onto a pcall command
 * with that same name, so that users don't have to explicitly type
 * "pcall".
 */
 	sprintf (pcall, "pcall %s", proc->p_name);
	uct_def_command (proc->p_name, pcall);
/*
 * Now release our resources and return.
 */
	relvm (csv->c_save);
	relvm (csv);
	Cs->cs_csave = 0;
	ucs_pop_cstack ();
}



void
ui_pcall (cmds)
struct ui_command *cmds;
/*
 * Handle a procedure call.
 */
{
	struct procedure *proc;
	struct parse_tree *pt;
	struct csave *csv;
	union usy_value v;
	int type, arg;
	stbl argtable;
/*
 * Look up this procedure.
 */
	if (! usy_g_symbol (Proc_table, UPTR (*cmds), &type, &v))
		ui_error ("Undefined procedure: '%s'", UPTR (*cmds));
	proc = (struct procedure *) v.us_v_ptr;
/*
 * Fill in our info structure.  Things are done a little differently here 
 * than for most of these, since (1) we know that EXEC will be true, or we
 * wouldn't have been called, and (2) we have our own CSAVE.
 */
	ucs_new_entry ();
	csv = (struct csave *) getvm (sizeof (struct csave));
	csv->c_save = proc->p_text;
	csv->c_release = FALSE;
	csv->c_where = -1;
	Cs->cs_exec = TRUE;
	Cs->cs_type = CST_PROC;
	Cs->cs_text = csv->c_read = 0;
	Cs->cs_term = "return";
	Cs->cs_test = usy_string (proc->p_name);
	Cs->cs_mode = M_COMMAND;
/*	argtable = (proc->p_narg > 0) ? usy_c_stbl ((char *) 0) : 0; */
	argtable = usy_c_stbl ((char *) 0);
	ucs_csave_source (csv, TRUE);
/*
 * Bind all of the arguments.
 */
	cmds++;
	for (arg = 0; arg < proc->p_narg; arg++)
	{
	/*
	 * Make sure we haven't run out.
	 */
		if (cmds->uc_ctype == UTT_END)
		{
			Cs->cs_arg_table = 0;	/* not ours -- dont zap */
			ucs_pop_cstack ();
			ui_error ("Insufficient arguments for '%s' -- %d expected",
				proc->p_name, proc->p_narg);
		}
	/*
	 * Strings are easy.
	 */
		if (proc->p_args[arg].pa_type == SYMT_STRING)
		{
			v.us_v_ptr = UPTR (*cmds);
			usy_s_symbol (argtable,
				proc->p_args[arg].pa_name, SYMT_STRING, &v);
			cmds++;
			continue;
		}
	/*
	 * Otherwise things are a little trickier.
	 */
		if ((pt = ue_parse (UPTR (*cmds), cmds->uc_col, FALSE)) == 0)
		{
			Cs->cs_arg_table = 0;
			ucs_pop_cstack ();
			ui_cl_error (TRUE, cmds->uc_col,
				"Unable to parse '%s'", UPTR (*cmds));
		}
		ERRORCATCH
			ue_eval (pt, &v, &type);
			if (type != proc->p_args[arg].pa_type)
				uit_coerce (&v, type,
						proc->p_args[arg].pa_type);
		ON_ERROR
			Cs->cs_arg_table = 0;
			ucs_pop_cstack ();
			ue_rel_tree (pt);
			RESIGNAL;
		ENDCATCH
		ue_rel_tree (pt);
	/*
	 * Whew!  It actually worked.  Define this symbol and go on.
	 */
		usy_s_symbol (argtable, proc->p_args[arg].pa_name,
			proc->p_args[arg].pa_type, &v);
		cmds++;
	}
	Cs->cs_arg_table = argtable;
/*
 * Check for excess junk.
 */
	if (cmds->uc_ctype != UTT_END)
		ui_warning ("Excess arguments to '%s' -- %d expected",
			proc->p_name, proc->p_narg);
}



void
ui_return ()
/*
 * Handle a procedure return.
 */
{
	struct cs_entry *csp, *last = 0;
/*
 * RETURN's are a little strange in that they are not necessarily referring
 * to the top item on the stack.  So, we have to dig for our entry.
 */
	for (csp = Cs; csp; csp = csp->cs_next)
		if (csp->cs_type == CST_PROC)
		{
			last = csp;
			break;
		}
	if (! last)
		ui_error ("'Return' with no procedure active");
/*
 * Remove stack entries, starting with our last proc invocation.
 */
	while (Cs != last)
		ucs_pop_cstack ();
/*
 * We need to remove this entry from the stack as well.
 */
	if (Cs->cs_test)
		ucs_pop_cstack ();
}



void
ui_edt (file)
char *file;
/*
 * Try to invoke EDT on this file.
 */
{
	ui_error ("EDT is only available on VMS machines");
}



static int Save_all;

void
ui_psave (lun, all)
int lun, all;
/*
 * Save our procedure definitions.
 */
{
	int ui_sv_proc ();
	char marker = LF_PROC;
/*
 * Dump the marker out to the file.
 */
 	bfput (lun, &marker, 1);
/*
 * Now write out each procedure.
 */
 	Save_all = all;
	usy_traverse (Proc_table, ui_sv_proc, lun, TRUE);
/*
 * Put the end-of-section marker in, and quit.
 */
 	bfput (lun, &lun, 0);
}




ui_sv_proc (name, type, v, lun)
char *name;
int type, lun;
union usy_value *v;
/*
 * Save an individual procedure.
 */
{
	struct procedure *proc = (struct procedure *) v->us_v_ptr;
	int i;
/*
 * See if this one should be saved.
 */
 	if (Save_all == 0 && proc->p_init)
		return (TRUE);
/*
 * Write out, in order, the proc structure, the arg list, then the
 * actual text.
 */
	if (usy_defined (Ui_variable_table, "ui$save_babble"))
	 	ui_printf ("Saving procedure '%s'\n", name);
	bfput (lun, (char *) proc, sizeof (struct procedure));
	if (proc->p_narg > 0)
		bfput (lun, (char *) proc->p_args,
				proc->p_narg*sizeof (struct procarg));
	for (i = 0; i < proc->p_ntext; i++)
		bfput (lun, proc->p_text[i], strlen (proc->p_text[i]));
	return (TRUE);
}



void
ui_pload (lun, init)
int lun, init;
/*
 * Load procedures from this file.
 */
{
	struct procedure *proc;
	union usy_value v;
	int len, line;
/*
 * We loop until we run out of stuff.
 */
 	for (;;)
	{
	/*
	 * Pull in a proc structure.
	 */
	 	proc = (struct procedure *) getvm (sizeof (struct procedure));
		if ((len = bfget (lun, (char *) proc,
				sizeof (struct procedure))) !=
				sizeof (struct procedure))
		{
			if (len != 0)
				ui_printf ("Proc size read error!\n");
			relvm (proc);
			return;
		}
	/*
	 * If there are arguments, pull them in too.
	 */
	 	if (proc->p_narg > 0)
		{
			proc->p_args = (struct procarg *)
				getvm (proc->p_narg*sizeof (struct procarg));
			bfget (lun, (char *) proc->p_args,
				proc->p_narg*sizeof (struct procarg));
		}
	/*
	 * Now snarf up the text.
	 */
	 	proc->p_text = (char **) getvm(proc->p_ntext*sizeof (char **));
		for (line = 0; line < proc->p_ntext; line++)
		{
			char tline[300];
			len = bfget (lun, tline, 300);
			tline[len] = '\0';
			proc->p_text[line] = usy_string (tline);
		}
	/*
	 * Define the procedure.
	 */
	 	ui_zapproc (proc->p_name);
		proc->p_init = init;
		v.us_v_ptr = (char *) proc;
		usy_s_symbol (Proc_table, proc->p_name, SYMT_POINTER, &v);
	}
}



void
ui_zapproc (name)
char *name;
/*
 * Delete the definition of this procedure.  Let's only hope that it is
 * not actively invoked now, or unpleasant things could happen.
 */
{
	struct procedure *proc;
	int type, i;
	union usy_value v;
/*
 * See if a definition of this procedure exists.
 */
 	if (! usy_g_symbol (Proc_table, name, &type, &v))
		return;
	proc = (struct procedure *) v.us_v_ptr;
/*
 * Go through and release all resources.
 */
 	if (proc->p_narg)
		relvm ((char *) proc->p_args);
	for (i = 0; i < proc->p_ntext; i++)
		usy_rel_string (proc->p_text[i]);
	relvm ((char *) proc->p_text);
	relvm (proc);
	usy_z_symbol (Proc_table, name);
}







void
ui_push_mode (mode)
int mode;
/*
 * Push a new major mode onto the control stack.
 */
{
	struct input_stack *inp;
/*
 * Get and initialize a new csinfo structure.
 */
	ucs_new_entry ();
/*
 * Set up the rest of it the way we need.
 */
	Cs->cs_type = CST_MODE;
	Cs->cs_mode = mode;
	Cs->cs_exec = TRUE;
	Cs->cs_term = "endmode";
	Cs->cs_test = 0;
/*
 * If this is a command mode, create a token context for it.
 */
	if (mode == M_COMMAND)
		ut_new_ctx ();
/*
 * KLUDGE: put a tty source on it.
 */
	inp = ucs_input ();
	inp->s_type = IST_TTY;
	inp->s_snarf = FALSE;
	inp->s_pb = 0;
	inp->s_xp = 0;
}



void
ui_endmode (mode)
int mode;
/*
 * End the current mode.  If MODE is nonzero, it is expected to match
 * the current mode.
 */
{
	if (Cs->cs_type != CST_MODE)
		ui_error ("Stack top is not a MODE");
	else if (mode && mode != Cs->cs_mode)
		ui_error ("(BUG): endmode mismatch");
# ifdef XSUPPORT
/*
 * If we are ending window mode, shut things down.
 */
 	if (Cs->cs_mode == M_WINDOW)
		uw_endmode ();
# endif
	ucs_pop_cstack ();
}




void
ui_perform (command)
char *command;
/*
 * Cause this command to be executed; not returning until it is done.
 */
{
	char *fixedcmd = getvm (strlen (command) + 2);
/*
 * Throw the M_NONE entry onto the list.  This mode essentially acts like
 * break, to insure that ui_do_cmode will quit when the input runs out.
 */
	ucs_new_entry ();
	Cs->cs_exec = TRUE;
	Cs->cs_type = CST_MODE;
	Cs->cs_text = 0;
	Cs->cs_term = "(end-of-csave)";
	Cs->cs_test = 0;
	Cs->cs_mode = M_NONE;
/*
 * Fix the command, so that ends with a trailing return.  Yes, a kludge,
 * but so it goes.
 */
 	strcpy (fixedcmd, command);
	strcat (fixedcmd, "\r");
/*
 * Now push the command itself.
 */
 	ui_pushcmd (fixedcmd);
	relvm (fixedcmd);	/* ui_pushcmd does a usy_string */
/*
 * Invoke the command mode handler.
 */
 	ui_do_cmode ();
/*
 * Remove the M_BLOCK structure.
 */
	while (Cs && Cs->cs_mode != M_NONE)
		ucs_pop_cstack ();
	if (Cs && Cs->cs_mode == M_NONE)
		ucs_pop_cstack ();
}





void
ui_pushcmd (command)
char *command;
/*
 * Push a single-line command onto the control stack.
 */
{
	char **save;
	struct csave *csv;
/*
 * Allocate and fill in a cs_info structure.
 */
	ucs_new_entry ();
	Cs->cs_exec = TRUE;
	Cs->cs_type = CST_MODE;
	Cs->cs_text = 0;
	Cs->cs_term = "(end-of-csave)";
	Cs->cs_test = 0;
	Cs->cs_mode = M_COMMAND;
/*
 * This mode needs a new token context.
 */
 	ut_new_ctx ();
/*
 * Fill in the save info.
 */
	csv = (struct csave *) getvm (sizeof (struct csave));
	csv->c_save = (char **) getvm (2*sizeof (char **));
 	csv->c_save[0] = usy_string (command);
	csv->c_save[1] = (char *) 0;
	csv->c_read = 0;
	csv->c_where = -1;
	csv->c_release = TRUE;
/*
 * Finally, push it on the stack, and enable the fake source if it is
 * not already running.
 */
	ucs_csave_source (csv, TRUE);
}
