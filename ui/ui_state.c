/*
 * State table manipulation.
 */
# include "ui.h"
# include "ui_globals.h"
# include "ui_state.h"
# include "ui_commands.h"
# include "ui_loadfile.h"
# include "ui_error.h"


char *getvm ();

/*
 * This is our symbol table.
 */
# define STATE_TABLE_NAME	"ui$state_table"
static stbl State_table = 0;

/*
 * Internal structure used for info passing through the parser.
 */
struct stinfo
{
	struct state_table_entry *s_sp;
	struct state_action *s_ap;
};


/*
 * Prototypes
 */
void ust_input (struct state_table_entry *ste, struct ui_command *cmds);
void ust_act_dump (struct state_action *ap);
void ust_do_load (int lun, int init);




void
ust_init ()
/*
 * Initialize the state table.
 */
{
/*
 * Create our symbol table.
 */
 	if (State_table)
		usy_z_stbl (State_table);
	State_table = usy_c_stbl (STATE_TABLE_NAME);
/*
 * Load up the initial stuff.
 */
 	/* ust_bootstrap (State_table); */
}





struct state_table_entry *
ust_lookup_state (state)
char *state;
/*
 * Return a pointer to the state table entry that corresponds to this
 * state table name.
 */
{
	union usy_value v;
	int type;
/*
 * Lookup the symbol in the state table.
 */
 	if (! usy_g_symbol (State_table, state, &type, &v))
		return (0);
/*
 * "This should never happen", but, if somebody has been mucking around
 * in our table, it could...
 */
 	if (type != SYMT_POINTER)
		c_panic ("State '%s' is type %d, not pointer!!!", state, type);
	return ((struct state_table_entry *) v.us_v_ptr);
}



void
ust_def_state (cmds)
struct ui_command *cmds;
/*
 * Handle the DEFINE STATE command.
 */
{
	struct state_table_entry *ste, *old;
	int ust_int_state ();
	union usy_value v;
/*
 * Get a state table entry to describe this particular state.
 */
 	ste = (struct state_table_entry *)
				getvm (sizeof (struct state_table_entry));
	zfill ((char *) ste, sizeof (struct state_table_entry));
	strcpy (ste->sta_name, cmds->uc_v.us_v_ptr);
	ste->sta_vptype = ST_NO_VP;
	ste->sta_eosact = ste->sta_otheract = 0;
/*
 * Now we re-enter the command-line parser, in our special, restricted
 * state.
 */
	ERRORCATCH
		ui_subcommand ("ust$input_initial", "State>", ust_int_state,
					(long) ste);
	ON_ERROR
		if (ste->sta_nkw)
			relvm (ste->sta_kw);
		relvm (ste);
		ui_epop ();
		ui_error ("Definition of state '%s' abandoned due to error.",
			ste->sta_name);
	ENDCATCH
/*
 * Finally, define the state.
 */
 	if ((old = ust_lookup_state (ste->sta_name)) &&
				! old->sta_flags & STF_BOOT)
	{
		if (old->sta_nkw)
			relvm (old->sta_kw);
		if (old->sta_eosact && (old->sta_flags & STF_EOS))
			relvm (old->sta_eosact);
		if (old->sta_otheract && (old->sta_flags & STF_OTHER))
			relvm (old->sta_otheract);
		relvm (old);
	}
	v.us_v_ptr = (char *) ste;
	usy_s_symbol (State_table, ste->sta_name, SYMT_POINTER, &v);
}



void
ust_delete (name, col)
char *name;
int col;
/*
 * Handle the DELETE STATE call.
 */
{
	struct state_table_entry *state;
/*
 * See if this state really exists.
 */
 	if ((state = ust_lookup_state (name)) == 0)
		ui_cl_error (TRUE, col, "State '%s' does not exist", name);
/*
 * Clear the state out, but don't do anything untoward with boot states.
 */
	usy_z_symbol (State_table, name);
	if ((state->sta_flags & STF_BOOT) == 0)
	{
		if (state->sta_nkw)
			relvm (state->sta_kw);
		if (state->sta_eosact && (state->sta_flags & STF_EOS))
			relvm (state->sta_eosact);
		if (state->sta_otheract && (state->sta_flags & STF_OTHER))
			relvm (state->sta_otheract);
		relvm (state);
	}
}




int
ust_int_state (ste, cmds)
struct state_table_entry *ste;
struct ui_command *cmds;
/*
 * Handle stuff internal to the DEFINE STATE command.
 */
{
/*
 * Figure out which keyword we have here.
 */
 	if (cmds->uc_v.us_v_int == UIC_ENDDEF)
		return (FALSE);
/*
 * Check for EOFTEXT.
 */
	else if (cmds->uc_v.us_v_int == UIC_EOFTEXT)
	{
		ste->sta_flags |= STF_EOFTXT;
		strcpy (ste->sta_eoftext, cmds[1].uc_v.us_v_ptr);
	}
/*
 * Perhaps it's a helpfile.
 */
 	else if (cmds->uc_v.us_v_int == UIC_HELPFILE)
	{
		ste->sta_flags |= STF_HELP;
		strcpy (ste->sta_helpfile, cmds[1].uc_v.us_v_ptr);
	}
/*
 * Maybe this is a command table.
 */
 	else if (cmds->uc_v.us_v_int == UIC_CTABLE)
	{
		ste->sta_flags |= STF_CTABLE;
		strcpy (ste->sta_ctable, cmds[1].uc_v.us_v_ptr);
	}
/*
 * The only alternative is INPUT.
 */
	else if (cmds->uc_v.us_v_int == UIC_INPUT)
		ust_input (ste, cmds + 1);
	else
		ui_error ("Bizarre kw = %d, in ust_int_state",
				cmds->uc_v.us_v_int);
	return (TRUE);
}


void
ust_input (ste, cmds)
struct state_table_entry *ste;
struct ui_command *cmds;
/*
 * Now, let's deal with the various input possibilities.
 */
{
	struct stinfo s;
	int ust_within_input ();
	
	s.s_sp = ste;
/*
 * If this is a VALUE parameter, it is a keyword with an associated 
 * number.
 */
 	if (cmds->uc_ctype == UTT_VALUE)
	{
		struct state_keyword *tkp;
		int len;
	/*
	 * If this is the first keyword, we need to create a keyword array.
	 */
	 	if (ste->sta_nkw == 0)
			ste->sta_kw = (struct state_keyword *)
					getvm (sizeof (struct state_keyword));
		else
		{
			tkp = (struct state_keyword *)
					getvm ((ste->sta_nkw + 1) *
						sizeof (struct state_keyword));
			len = ste->sta_nkw * sizeof (struct state_keyword);
			COPY (ste->sta_kw, tkp, len);
			relvm (ste->sta_kw);
			ste->sta_kw = tkp;
		}
		strcpy (ste->sta_kw[ste->sta_nkw].stk_keyword,
				cmds->uc_v.us_v_ptr);
		ste->sta_kw[ste->sta_nkw].stk_kwnum = cmds[1].uc_v.us_v_int;
		s.s_ap = &ste->sta_kw[ste->sta_nkw].stk_action;
		s.s_ap->act_flags = 0;
		ste->sta_nkw++;
	}
/*
 * Otherwise it really ought to be one of the known keywords, indicating a
 * value parameter type.
 */
 	else
	{
		s.s_ap = &ste->sta_vpact;
		switch (cmds[0].uc_v.us_v_int)
		{
		   case UIC_STRING:	ste->sta_vptype = SYMT_STRING; break;
		   case UIC_INTEGER:	ste->sta_vptype = SYMT_INT; break;
		   case UIC_REAL:	ste->sta_vptype = SYMT_FLOAT; break;
		   case UIC_DATE:	ste->sta_vptype = SYMT_DATE; break;
		   case UIC_BOOLEAN:	ste->sta_vptype = SYMT_BOOL; break;
		   case UIC_EXPRESSION: ste->sta_vptype = SYMT_UNDEFINED;
		   			break;

		   case UIC_EOS:
			ste->sta_flags |= STF_EOS;
			if (ste->sta_eosact)
			{
	warning ("Overwriting existing EOS action for state '%s'.",
				ste->sta_name);
				relvm (ste->sta_eosact);
			}
			ste->sta_eosact = (struct state_action *)
				getvm (sizeof (struct state_action));
			zfill (ste->sta_eosact, sizeof (struct state_action));
		   	s.s_ap = ste->sta_eosact;
			break;
		   case UIC_OTHER:
			ste->sta_flags |= STF_OTHER;
			if (ste->sta_otheract)
			{
	warning ("Overwriting existing OTHER action for state '%s'.",
				ste->sta_name);
				relvm (ste->sta_otheract);
			}
			ste->sta_otheract = (struct state_action *)
				getvm (sizeof (struct state_action));
			zfill (ste->sta_otheract, sizeof (struct state_action));
		   	s.s_ap = ste->sta_otheract;
		   	break;
		   default:
		   	ui_error ("(BUG): kw number %d", cmds->uc_v.us_v_int);
		}
	}
/*
 * OK, somewhere in that mess above, we have come up with an action pointer.
 * Now we enter yet another subcommand to fill that structure in.
 */
 	ui_subcommand ("ust$within_input", "Input>", ust_within_input,
			(long) &s);
}




int
ust_within_input (s, cmds)
struct stinfo *s;
struct ui_command *cmds;
/*
 * Handle all of the junk that applies within an INPUT command.
 */
{
	switch (cmds->uc_v.us_v_int)
	{
	   case UIC_REJECT:
	   	s->s_ap->act_flags |= STAF_REJECT;
		break;
 	   case UIC_MESSAGE:
	   	s->s_ap->act_flags |= STAF_MSG;
		strcpy (s->s_ap->act_mtext, cmds[1].uc_v.us_v_ptr);
		break;
	   case UIC_IGNORE:
	   	s->s_ap->act_flags |= STAF_IGNORE;
		break;
	   case UIC_LOWERCASE:
	   	s->s_ap->act_flags |= STAF_LOWERCASE;
		break;
	   case UIC_FORCEEVAL:
	   	s->s_ap->act_flags |= STAF_EVAL;
		break;
	   case UIC_NEXT:
	   	strcpy (s->s_ap->act_next, cmds[1].uc_v.us_v_ptr);
		break;
	   case UIC_DONE:
		s->s_ap->act_flags |= STAF_DONE;
		break;
	   case UIC_PARTIAL:
	   	s->s_ap->act_flags |= STAF_PARTIAL;
		break;
	   case UIC_PUSHBACK:
	   	break;
	   case UIC_ENDINPUT:
	   	return (0);
	}
	return (1);
}




void
ust_dump (state)
char *state;
/*
 * Dump out the definition of this state.
 */
{
	int kw;
	struct state_table_entry *ste;
/*
 * See if this state exists.
 */
 	if ((ste = ust_lookup_state (state)) == 0)
		ui_error ("Unknown state: '%s'", state);
/*
 * Write it out.
 */
 	ui_printf ("State name = '%s', vptype = %d\n", ste->sta_name,
			ste->sta_vptype);
	ui_printf ("  Flags = %X, %d keywords\n", ste->sta_flags,
			ste->sta_nkw);
	if (ste->sta_flags & STF_CTABLE)
		ui_printf ("  Command table: '%s'\n", ste->sta_ctable);
	if (ste->sta_flags & STF_EOFTXT)
		ui_printf ("  EOF text is '%s'\n", ste->sta_eoftext);
	for (kw = 0; kw < ste->sta_nkw; kw++)
	{
		ui_printf ("  Keyword, name = '%s' number %d.\n",
					ste->sta_kw[kw].stk_keyword,
					ste->sta_kw[kw].stk_kwnum);
		ust_act_dump (&ste->sta_kw[kw].stk_action);
	}
	if (ste->sta_vptype != ST_NO_VP)
	{
		ui_printf ("  VP Action: \n");
		ust_act_dump (&ste->sta_vpact);
	}
	if (ste->sta_flags & STF_EOS)
	{
		ui_printf ("  EOS Action: \n");
		ust_act_dump (ste->sta_eosact);
	}
	if (ste->sta_flags & STF_OTHER)
	{
		ui_printf ("  Other action: \n");
		ust_act_dump (ste->sta_otheract);
	}
}


void
ust_act_dump (ap)
struct state_action *ap;
/*
 * Dump out an action pointer.
 */
{
	ui_printf ("    Action flags: 0x%X\n", ap->act_flags);
	ui_printf ("    Next state: '%s'\n", ap->act_next);
	if (ap->act_flags & STAF_MSG)
		ui_printf ("    Msg text: '%s'\n", ap->act_mtext);
}




/*
 * Kludge for preserving the ALL flag through usy_traverse.
 */
static int Save_all;

void
ust_save (lun, all)
int lun, all;
/*
 * Save the current state info to this file.
 */
{
	int ust_state_save ();
	char marker;
/*
 * Put our marker in.
 */
 	marker = LF_STATE;
	bfput (lun, &marker, 1);
/*
 * Now run through the symbol table.
 */
	Save_all = all;
 	usy_traverse (State_table, ust_state_save, lun, TRUE);
/*
 * All done.  Put the marker in and quit.
 */
 	bfput (lun, &lun, 0);
}



int
ust_state_save (symbol, type, v, lun)
char *symbol;
int type, lun;
union usy_value *v;
/*
 * Perform the save of a single state.
 */
{
	struct state_table_entry *ste =(struct state_table_entry *)v->us_v_ptr;
	bool babble = usy_defined (Ui_variable_table, "ui$save_babble");
/*
 * See if we really want to save this one.
 */
 	if (Save_all == 0 && (ste->sta_flags & STF_INITIAL))
		return (TRUE);
/*
 * Write out the structure.  If there is an associated keyword array, write
 * that too.
 */
	if (babble)
	 	ui_printf ("Saving state '%s'", ste->sta_name);
	bfput (lun, ste, sizeof (struct state_table_entry));
	if (ste->sta_nkw > 0)
	{
		if (babble)
			ui_printf (" (+ %d keywords)", ste->sta_nkw);
		bfput (lun, ste->sta_kw,
			ste->sta_nkw * sizeof (struct state_keyword));
	}
	if (ste->sta_flags & STF_EOS)
		bfput (lun, ste->sta_eosact, sizeof (struct state_action));
	if (ste->sta_flags & STF_OTHER)
		bfput (lun, ste->sta_otheract, sizeof (struct state_action));
	if (babble)
		ui_printf ("\n");
	return (TRUE);
}




void
ust_load (lun)
int lun;
/*
 * Perform a state load from this file.
 */
{
	ust_do_load (lun, 1);
}


void
ust_do_load (lun, init)
int lun;
int init;
/*
 * Read in the info from this (open) file.
 */
{
	struct state_table_entry *ste, *old;
	union usy_value v;
	int len;

	for (;;)
	{
	/*
	 * Obtain storage, then read in a state entry.
	 */
	 	ste = (struct state_table_entry *)
				getvm (sizeof (struct state_table_entry));
		if ((len = bfget (lun, ste, sizeof (struct state_table_entry)))
				!= sizeof (struct state_table_entry))
		{
			if (len != 0)
				ui_printf ("Read size error!\n");
			relvm (ste);
			return;
		}
	/*
	 * If there are keywords, get them too.
	 */
	 	if (ste->sta_nkw > 0)
		{
			int size = ste->sta_nkw * sizeof(struct state_keyword);

			ste->sta_kw = (struct state_keyword *) getvm (size);
			if (bfget (lun, ste->sta_kw, size) != size)
			ui_printf ("Kw read size error\n");
		}
	/*
	 * If there are EOS and OTHER action blocks, grab them.
	 */
		if (ste->sta_flags & STF_EOS)
		{
			ste->sta_eosact = (struct state_action *) 
				getvm (sizeof (struct state_action));
			bfget (lun, ste->sta_eosact,
				sizeof (struct state_action));
		}
		if (ste->sta_flags & STF_OTHER)
		{
			ste->sta_otheract = (struct state_action *) 
				getvm (sizeof (struct state_action));
			bfget (lun, ste->sta_otheract,
				sizeof (struct state_action));
		}
	/*
	 * Define the state.  Clear the boot flag, just in case it was set
	 * when this state was saved.
	 */
	 	ste->sta_flags &= ~(STF_BOOT | STF_INITIAL);
		if (init)
			ste->sta_flags |= STF_INITIAL;
	 	if ((old = ust_lookup_state (ste->sta_name)) &&
					! old->sta_flags & STF_BOOT)
		{
			if (old->sta_nkw)
				relvm (old->sta_kw);
			relvm (old);
		}
		v.us_v_ptr = (char *) ste;
		usy_s_symbol (State_table, ste->sta_name, SYMT_POINTER, &v);
	}
}
