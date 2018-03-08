/* 7/88 jc */

/*
 * Routines concerned with the manipulation of the control stack.
 */
# include "ui_param.h"
# include "ui_symbol.h"
# include "ui_cstack.h"
# include "ui_globals.h"
# include "ui_error.h"
# include "ui_mode.h"

static char *Rcsid = "$Id: ui_cstack.c,v 1.7 1998-02-26 21:18:23 burghart Exp $";

/*
 * The list of free cs_entry structures.  Whenever we need a new one,
 * we look here first.
 */
static struct cs_entry *Free_list = 0;


static char *Cs_types[] =
{
	"If", "While", "Foreach", "Procedure call", "Procedure definition",
	"Major mode", "Pushcmd", 0
};


static char *Modes[] =
{
	"???", "Menu", "Command", "NONE", "Window", 0
};

/*
 * Prototypes
 */
void ucs_in_dump (struct input_stack *inp);
void ucs_z_input (int cleanup);
void ucs_z_middle (struct cs_entry *backptr);




struct cs_entry *
ucs_new_entry ()
/*
 * Put a new entry onto the Cstack, returning a pointer to it.  The new
 * entry is initialized to look like the existing one, if any.
 */
{
	struct cs_entry *new;
/*
 * Get the new structure.
 */
 	if (Free_list)
	{
		new = Free_list;
		Free_list = Free_list->cs_next;
	}
	else
	{
		new = (struct cs_entry *) getvm (sizeof (struct cs_entry));
		memset ((char *) new, 0, sizeof (struct cs_entry));
	}
/*
 * Initialize it and put it onto the stack.
 */
 	if (Cs)
		*new = *Cs;
	new->cs_next = Cs;
	new->cs_test = 0;
	Cs = new;
/*
 * Return the new structure.
 */
 	return (Cs);
}



void
ucs_dump_cstack ()
/*
 * Dump out the cstack.
 */
{
	struct cs_entry *csp;
	struct input_stack *inp;

	ui_printf ("\n--Cs dump--\n");
	for (csp = Cs; csp; csp = csp->cs_next)
	{
		ui_nf_printf (
			"Entry, type '%s', term '%s', test '%s' mode %s\n",
			Cs_types[csp->cs_type],
			csp->cs_term ? csp->cs_term : "(none)",
			csp->cs_test ? csp->cs_test : "(none)",
			Modes[csp->cs_mode]);
		ui_nf_printf ("  Exec %d done %d text %d\n",
			csp->cs_exec, csp->cs_done, csp->cs_text);
		for (inp = csp->cs_input; inp; inp = inp->s_next)
			ucs_in_dump (inp);
	}
	ui_printf ("--End of dump--\n");
}



void
ucs_in_dump (inp)
struct input_stack *inp;
/*
 * Dump out an input entry.
 */
{
	switch (inp->s_type)
	{
	   case IST_TTY:
	   	ui_printf ("\tTTY input, snarf %s\n",
			inp->s_snarf ? "TRUE" : "FALSE");
		break;

	   case IST_FILE:
	   	ui_printf ("\tFile input, name '%s', snarf %s\n", inp->s_name,
			inp->s_snarf ? "TRUE" : "FALSE");
		break;

	   case IST_CSAVE:
	   	ui_printf ("\tCsave input, read %d, write %d, rel %s\n",
			inp->s_csave->c_read, inp->s_csave->c_where,
			inp->s_csave->c_release ? "TRUE" : "FALSE");
		break;

	   default:
	   	ui_printf ("\tUNKNOWN INPUT TYPE %d\n", inp->s_type);
		break;
	}
}


void
ucs_csave_source (csp, replace)
struct csave *csp;
bool replace;
/*
 * Add a CSAVE input source to the current input source stack.
 * Entry:
 *	CSP	is the csave info.
 *	REPLACE	is true iff this source is to replace the current
 *		input stack.
 * Exit:
 *	This csave has been established as the top source on the input stack.
 */
{
	struct input_stack *ip = (struct input_stack *)
		getvm (sizeof (struct input_stack));
/*
 * Fill the structure in.
 */
	ip->s_type = IST_CSAVE;
	ip->s_pb = 0;
	ip->s_xp = 0;
	ip->s_snarf = FALSE;
	ip->s_csave = csp;
	ip->s_backptr = Cs;
/*
 * Put it onto the stack.
 */
	ip->s_next = replace ? 0 : Cs->cs_input;
	Cs->cs_input = ip;
}



void
ucs_pop_cstack ()
/*
 * Remove the top entry from the Cstack.
 */
{
	struct cs_entry *old = Cs;
/*
 * Clean up the input stack.
 */
	while (old->cs_input && old->cs_input->s_backptr == old)
		ucs_z_input (FALSE);
/*
 * Remove the entry from the stack, but refuse to remove the initial mode.
 */
# ifdef notdef
	if (! (Cs = Cs->cs_next))
		c_panic ("Empty control stack");
# endif
	if (! Cs->cs_next)
		return;
	Cs = Cs->cs_next;
	old->cs_next = 0;
/*
 * Clear out any dynamic memory.
 */
	if (old->cs_type == CST_PROC && old->cs_arg_table)
	{
		usy_z_stbl (old->cs_arg_table);
		old->cs_arg_table = (stbl) 0;
	}
	if (old->cs_test)
		usy_rel_string (old->cs_test);
	if (old->cs_type == CST_FOREACH)
		usy_rel_string (old->cs_cvar);
	if (old->cs_type == CST_MODE && old->cs_mode == M_COMMAND)
		ut_rel_ctx (old->cs_tctx);
/*
 * Put the old entry onto the free list.
 */
 	old->cs_next = Free_list;
	Free_list = old;
}




void
ucs_rel_csave (csp)
struct csave *csp;
/*
 * Release all memory used by this csave structure.
 */
{
	int line;

	if (csp->c_release)
	{
		for (line = 0; csp->c_save[line]; line++)
			usy_rel_string (csp->c_save[line]);
		relvm (csp->c_save);
	}
	relvm (csp);
}










struct input_stack *
ucs_input ()
/*
 * Put a new input structure onto the Cstack.
 */
{
	struct input_stack *new = (struct input_stack *)
			getvm (sizeof (struct input_stack));

	new->s_next = Cs->cs_input;
	new->s_backptr = Cs;
	Cs->cs_input = new;
	return (new);
}



void
ucs_z_input (cleanup)
bool cleanup;
/*
 * Clear out the top input source.  If CLEANUP is TRUE, cleaning up will
 * be done on the Cstack as well.
 */
{
	struct input_stack *old;
	struct pb *pbp;
	bool err = FALSE;

	if (! Cs->cs_input)
		c_panic ("ucs_z_input() with no input");
 	old = Cs->cs_input;
/*
 * If the current Cstack entry is not the one that originated this input,
 * we need to clear out the intervening stuff.  Catch errors, so that we
 * can resignal them after the cleanup is done.
 */
	if (old->s_backptr != Cs)
	{
		if (! cleanup)
			c_panic ("Non-cleanup input zap with intervening entries");
		ERRORCATCH
			ucs_z_middle (old->s_backptr);
		ON_ERROR
			err = TRUE;
		ENDCATCH
	}
/*
 * If this is a file source, we need to close the file.
 */
 	if (old->s_type == IST_FILE && old->s_lun)
		dclose (old->s_lun);
/*
 * Csave sources need to be released.
 */
 	else if (old->s_type == IST_CSAVE)
		ucs_rel_csave (old->s_csave);
/*
 * Clear out any remaining pushback structures.
 */
	for (pbp = old->s_pb; pbp;)
	{
		register struct pb *zap = pbp;
		pbp = pbp->pb_next;
		usy_rel_string (zap->pb_text);
		relvm (zap);
	}
/*
 * Unlink the structure from the cstack and release it.
 */
 	Cs->cs_input = old->s_next;
	old->s_next = 0;
	relvm (old);
/*
 * If this Cstack entry has no input, we need to clear it too.
 */
 	if (cleanup && ! Cs->cs_input)
		ucs_pop_cstack ();
/*
 * Finally, if we are in an error situation, resignal it.
 */
 	if (err)
		ui_bailout ((char *) 0);
}




char *
ucs_g_csline ()
/*
 * Return a line from the current input source, which is expected to be
 * a csave source.
 */
{
	struct csave *csv = Cs->cs_input->s_csave;

	if (Cs->cs_input->s_type != IST_CSAVE)
		c_panic ("Top source is not a CSAVE");
/*
 * If no data exists to be read in this csave, clear it and return
 * back to the previous level.
 */
 	if (csv->c_save[csv->c_read] == 0)
	{
		ucs_z_input (TRUE);
		return ((char *) 0);
	}
/*
 * OK, we have something.  return it.
 */
 	return (csv->c_save[csv->c_read++]);
}



void
ucs_z_middle (backptr)
struct cs_entry *backptr;
/*
 * Zap all Cs entries between the top of the stack and the entry that
 * is pointed to by backptr.
 */
{
	bool err = FALSE;

	while (Cs && backptr != Cs)
	{
		if (Cs->cs_term)
		{
			ui_ns_error ("Missing '%s'", Cs->cs_term);
			err = TRUE;
		}
		ucs_pop_cstack ();
	}
	if (err)
		ui_error ("Terminating due to incomplete control structures");
}




void
ucs_tty_cmode ()
/*
 * Throw a command mode structure onto the control stack, with the terminal
 * as the only source.  One ought not to call this thing non-interactively.
 */
{
	struct input_stack *inp;
/*
 * Add the new stack entry.
 */
	ucs_new_entry ();
	Cs->cs_input = 0;
	Cs->cs_type = CST_MODE;
	Cs->cs_mode = M_COMMAND;
	Cs->cs_exec = TRUE;
	Cs->cs_test = 0;
	ut_new_ctx ();
/*
 * Put together an input source for it.
 */
	inp = ucs_input ();
	inp->s_type = IST_TTY;
	inp->s_snarf = FALSE;
	inp->s_pb = 0;
	inp->s_xp = 0;
}
