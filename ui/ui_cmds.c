/* 1/88 jc */
/*
 * This file holds routines for implementing various UI commands, which
 * are not control-structure related.
 */
# include "ui.h"
# include "ui_error.h"
# include "ui_expr.h"
# include "ui_loadfile.h"
# include "ui_commands.h"
# include "ui_globals.h"
# include "ui_mode.h"
# include "ui_cstack.h"

static char *Rcsid = "$Id: ui_cmds.c,v 1.15 1999-06-25 19:21:00 burghart Exp $";

# define HELPDIR "/rdss/help"

/*
 * Prototypes
 */
void ui_enc_sym (char *dest, char *sym);



void
ui_set (cmds, local)
struct ui_command *cmds;
bool local;
/*
 * Handle the SET command.
 */
{
	union usy_value v;
	stbl dest = Ui_variable_table;
/*
 * If this is a LOCAL variable, find the table for it.
 */
	if (local && Cs->cs_arg_table)
		dest = Cs->cs_arg_table;
/*
 * Set something to a boolean value.  That value is TRUE unless the user
 * has given us something else.
 */
	if (cmds[1].uc_ctype == UTT_END)
	{
		v.us_v_int = TRUE;
		usy_s_symbol (dest, cmds->uc_v.us_v_ptr, SYMT_BOOL, &v);
	}
	else
	{
# ifdef notdef
		if (cmds[1].uc_vptype == SYMT_STRING)
			uip_dequote (UPTR (cmds[1]));
# endif
		usy_s_symbol (dest, cmds->uc_v.us_v_ptr,
				cmds[1].uc_vptype, &cmds[1].uc_v);
	}
}




void
ui_define (cmds)
struct ui_command *cmds;
/*
 * Handle the DEFINE command.
 */
{
	if (cmds->uc_ctype != UTT_KW)
		c_panic ("Bad ctype: %d", cmds->uc_ctype);
	switch (cmds->uc_v.us_v_int)
	{
	/*
	 * Define state.
	 */
	   case UIC_STATE:
	   	ust_def_state (cmds + 1);
	   	break;
	/*
	 * Define command.
	 */
	   case UIC_COMMAND:
	   	uct_def_command (cmds[1].uc_v.us_v_ptr, cmds[2].uc_v.us_v_ptr);
		break;
	/*
	 * Define key.
	 */
	   case UIC_KEY:
	   	uk_define_key (cmds + 1);
		break;
	/*
	 * Menu definition time.
	 */
	   case UIC_MENU:
	   	um_define (cmds[1].uc_v.us_v_ptr);
		break;
# ifdef XSUPPORT
	/*
	 * Widgets.
	 */
	   case UIC_WIDGET:
	   	uw_define (cmds + 1);
		break;
# endif
	/*
	 * Sigh.
	 */
	   default:
	   	ui_error ("(BUG) bizarre kw number for DEFINE: %d",
				cmds->uc_v.us_v_int);
	}
}




int
ui_dump (cmds)
struct ui_command *cmds;
/*
 * Symbol table dump hook.
 */
{
	stbl t;
/*
 * See what is to be dumped.
 */
 	switch (cmds->uc_v.us_v_int)
	{
	   case UIC_STATE:
	  	ust_dump (cmds[1].uc_v.us_v_ptr);
		break;
	   case UIC_TABLE:
	   	t = usy_g_stbl (cmds[1].uc_v.us_v_ptr);
		if (! t)
			ui_cl_error (TRUE, cmds[1].uc_col,
					"Unknown symbol table: '%s'",
					cmds[1].uc_v.us_v_ptr);
		usy_dump_table (t);
		break;
	   case UIC_COMMAND:
	   	uct_dump_command (cmds[1].uc_v.us_v_ptr);
		break;
	   case UIC_CSAVE:
	   	ucs_dump_cstack ();
		break;
	   case UIC_KEY:
	   	tty_dump_keys ();
		break;
	   case UIC_MENU:
	   	um_dump (UPTR (cmds[1]));
		break;

	   default:
	   	ui_error ("(BUG): Dump what? (%d)", cmds->uc_v.us_v_int);
	}
	return (TRUE);
}



void
ui_test (cmds)
struct ui_command *cmds;
{
	ui_printf ("Test, vptype %d\n", cmds->uc_vptype);
	ui_nf_printf ("Value: ");
	ue_print_val (&cmds->uc_v, cmds->uc_vptype);
	ui_printf ("\n");
	if (cmds->uc_vptype == SYMT_STRING)
	{
		uip_dequote (UPTR (*cmds));
		ui_perform (UPTR (*cmds));
		ui_printf ("Perform done\n");
	}
}





void
ui_eval (string, col)
char *string;
int col;
/*
 * The EVAL command.
 */
{
	struct parse_tree *pt;
	int type;
	union usy_value v;
/*
 * First we generate a parse tree.
 */
	if ((pt = ue_parse (string, col, TRUE)) == 0)
		ui_error ("Unable to parse '%s'", string);
/*
 * Now print out the tree, evaluate it, and print the solution.
 */
	ERRORCATCH
		ue_dump_tree (pt);
		ui_nf_printf ("\n");
		ue_eval (pt, &v, &type);
		ui_nf_printf ("Solution: ");
		ue_print_val (&v, type);
		ui_printf ("\n");
	ON_ERROR
		ue_rel_tree (pt);
		RESIGNAL;
	ENDCATCH
	ue_rel_tree (pt);
	ue_free_result (type, &v);
}





void
ui_message (cmds)
struct ui_command *cmds;
/*
 * Handle the "message" command.
 */
{
	char mbuf[200], *csp = cmds->uc_v.us_v_ptr, *mbp;
	char *orig = cmds->uc_v.us_v_ptr;
	int type, col = cmds->uc_col;
	struct parse_tree *pt;
	union usy_value v;
/*
 * Pass through the control string, and put together the message.
 */
	mbp = mbuf;
	cmds++;
 	for (; *csp; csp++)
	{
	/*
	 * '#' is the parameter substitution character.
	 */
		if (*csp == '#')
		{
		/*
		 * '##', however, is just a #.
		 */
			if (csp[1] == '#')
			{
				*mbp++ = '#';
				csp++;
			}
			else if (cmds->uc_ctype == UTT_END)
				ui_cl_error (TRUE, col + (csp - orig) + 1,
					"Missing parameters for MESSAGE");
			else
			{
			/*
			 * Flush out any buffered text.
			 */
				*mbp = '\0';
				if (mbp > mbuf)
					ui_nf_printf (mbuf);
				mbp = mbuf;
			/*
			 * Now parse and print the parameter.
			 */
				if ((pt = ue_parse (cmds->uc_v.us_v_ptr,
						cmds->uc_col, FALSE)) == 0)
					ui_nf_printf (cmds->uc_v.us_v_ptr);
				else
				{
					ERRORCATCH
						ue_eval (pt, &v, &type);
						ue_print_val (&v, type);
					ON_ERROR
						ue_rel_tree (pt);
						RESIGNAL;
					ENDCATCH
					ue_rel_tree (pt);
					ue_free_result (type, &v);
				}
				cmds++;
			}
		}
	/*
	 * '&' implies straight string substitution.
	 */
		else if (*csp == '&')
		{
		/*
		 * '&&', however, is just a &.
		 */
			if (csp[1] == '&')
			{
				*mbp++ = '&';
				csp++;
			}
			else if (cmds->uc_ctype == UTT_END)
				ui_cl_error (TRUE, col + (csp - orig) + 1,
					"Missing parameters for MESSAGE");
			else
			{
			/*
			 * Flush out any buffered text, then print the given
			 * parameter.
			 */
# ifdef notdef
				*mbp = '\0';
				if (mbp > mbuf)
					ui_nf_printf (mbuf);
				mbp = mbuf;
				ui_nf_printf (cmds->uc_v.us_v_ptr);
# endif
				ui_enc_sym (mbp, UPTR (*cmds));
				mbp += strlen (mbp);
				cmds++;
			}
		}
		else
			*mbp++ = *csp;
	}
/*
 * Flush out any remaining output.
 */
	*mbp = '\0';
 	if (mbp > mbuf)
		ui_printf ("%s\n", mbuf);
	else
		ui_printf ("\n");
}



void
ui_enc_sym (dest, sym)
char *dest, *sym;
/*
 * Encode this symbol into this buffer.
 */
{
	int type;
	union usy_value v;
/*
 * Look up the symbol.  If it is not defined, we simply copy in the name
 * and leave it at that.
 */
 	if ((Cs->cs_arg_table == 0 || ! usy_g_symbol (Cs->cs_arg_table,
			sym, &type, &v)) &&
			! usy_g_symbol (Ui_variable_table, sym, &type, &v))
		strcpy (dest, sym);
/*
 * If it is a string variable, we can just copy over the value, and we're
 * done.  Otherwise, encode the value we got back.
 */
 	else if (type == SYMT_STRING)
		strcpy (dest, v.us_v_ptr);
	else
		ue_enc_val (&v, type, dest);
}


int
ui_vset (cmds, eval)
struct ui_command *cmds;
bool eval;
/*
 * Handle the VSET command.
 */
{
	struct parse_tree *pt;
	union usy_value v;
	int type;
/*
 * Come up with a value for this variable.
 */
 	if (eval)
	{
	 	pt = ue_parse (cmds[1].uc_v.us_v_ptr, cmds[1].uc_col, TRUE);
		ERRORCATCH
			ue_eval (pt, &v, &type);
		ON_ERROR
			ue_rel_tree (pt);
			RESIGNAL
		ENDCATCH
		ue_rel_tree (pt);
	}
	else
	{
		v.us_v_ptr = usy_string (cmds[1].uc_v.us_v_ptr);
		type = SYMT_STRING;
	}
/*
 * Store this symbol away.  If it is currently defined in the arg
 * table, we'll put it there.  Otherwise it goes into the global variable
 * table.
 */
	usy_s_symbol (usy_defined (Arg_table, UPTR (*cmds)) ? Arg_table :
		Ui_variable_table, cmds->uc_v.us_v_ptr, type, &v);
	ue_free_result (type, &v);
	return (TRUE);
}




void
ui_type_file (file)
char *file;
/*
 * Output this file to the screen.
 */
{
	int length;
	LUN lun;
	char line[200];

	if ((lun = (LUN) dview (file)) == 0)
		ui_error ("Unable to open file '%s'", file);
	while ((length = dget (lun, line, 200)) >= 0)
	{
		line[length] = '\0';
		ui_printf ("%s\n", line);
	}
	dclose (lun);
}



void
ui_load (file, init)
char *file;
int init;
/*
 * Bring in the contents of this load file.
 */
{
	int lun;
	char marker;
/*
 * Open the file.
 */
	if ((lun = bfview (file)) == 0)
		ui_error ("Unable to open load file '%s'", file);
/*
 * Get the LF header and make sure that we have a real file here.
 */
 	if ((bfget (lun, &marker, 1) != 1) || marker != LF_HDR)
	{
		bfclose (lun);
		ui_error ("'%s' is not a current UI load file", file);
	}
/*
 * Now go through the various segments of the file.
 */
 	ERRORCATCH
	 	while (bfget (lun, &marker, 1) == 1)
		{
			switch (marker)
			{
			   case LF_STATE:
				ust_do_load (lun, init);
				break;
			   case LF_PROC:
			   	ui_pload (lun, init);
				break;
			   case LF_COMMAND:
			   	uct_load (lun);
				break;
			   case LF_MENU:
				um_load (lun, init);
				break;
			   case LF_KEYS:
			   	uk_load (lun);
				break;
			   case LF_WIDGET:
				/* if no X support, ignore this marker */
# ifdef XSUPPORT
			   	uw_load (lun, init);
# endif
				break;
			   default:
			   	ui_printf ("Funky marker: %d\n", marker);
				break;
			}
		}
	ENDCATCH
	bfclose (lun);
}




void
ui_save (file, all)
char *file;
int all;
/*
 * Save the user interface state into this file.
 */
{
	char marker;
	int lun;
/*
 * Try to open up the file.
 */
 	if ((lun = bfcreate (file)) == 0)
		ui_error ("Unable to create file '%s'", file);
/*
 * Put the loadfile marker in.
 */
	marker = LF_HDR;
	bfput (lun, &marker, 1);
/*
 * Now go through and save everything.
 */
 	ust_save (lun, all);	/* State transition info	*/
	ui_psave (lun, all);	/* Procedure definitions	*/
	uct_save (lun);		/* Defined commands		*/
	um_save (lun, all);	/* Menus			*/
	uk_save (lun);		/* Defined keys			*/
# ifdef XSUPPORT
	uw_save (lun, all);	/* Widgets			*/
# endif
/*
 * All done.
 */
	bfclose (lun);
}



void
ui_mode (cmds)
struct ui_command *cmds;
/*
 * Deal with a MODE command.
 */
{
	switch (UINT (*cmds))
	{
	   case M_MENU:
	   	um_mode (cmds + 1);
		return;
	   case M_WINDOW:
# ifdef XSUPPORT
	   	uw_mode (cmds + 1);
# else
		ui_error ("X support not present in this version");
# endif
		return;
	   default:
	   	ui_error ("(BUG) Unknown mode: %s", cmds->uc_text);
	}
}




void
ui_help (file)
char *file;
/*
 * Present the info given in this helpfile.
 */
{
	int lun = 0, len;
	char line[128], fname[200];
/*
 * Try to open up the file.
 */
	if (usy_defined (Ui_variable_table, "ui$helpdir"))
	{
		union usy_value v;
		int type;

		usy_g_symbol (Ui_variable_table, "ui$helpdir", &type, &v);
		sprintf (fname, "%s%s", v.us_v_ptr, file);
	 	lun = dview (fname);
	}
	if (lun == 0)
	{
		sprintf (fname, "%s%s", HELPDIR, file);
		lun = dview (fname);
	}
	if (lun == 0)
	{
		ui_printf ("Unable to open help file '%s'\n", file);
		return;
	}
/*
 * Now dump it out.
 */
	ui_printf ("\n");
 	while ((len = dget (lun, line, 128)) >= 0)
	{
		line[len] = '\0';
		ui_printf ("%s\n", line);
	}
	ui_printf ("\n");
/*
 * All done.
 */
 	dclose (lun);
	return;
}



void
ui_keypad (state)
bool state;
/*
 * Turn the keypad on or off.
 */
{
	if (Keypad_on = state)
		tty_kpon ();
	else
		tty_kpoff ();
}



void
ui_delete (cmds)
struct ui_command *cmds;
/*
 * Handle the DELETE command.
 */
{
/*
 * See what is to be deleted, then order it done.
 */
 	switch (UKEY (cmds[0]))
	{
	/*
	 * Defined keys.
	 */
	   case UIC_KEY:
	   	uk_delete (UPTR (cmds[1]), cmds[1].uc_col);
		return;

	/*
	 * Menu.
	 */
	   case UIC_MENU:
	   	um_delete (UPTR (cmds[1]), cmds[1].uc_col);
		return;
	/*
	 * States.
	 */
	   case UIC_STATE:
	   	ust_delete (UPTR (cmds[1]), cmds[1].uc_col);
		return;

	/*
	 * Commands.
	 */
	   case UIC_COMMAND:
	   	uct_delete (UPTR (cmds[1]), cmds[1].uc_col);
		return;

	   default:
	   	ui_error ("Bizarre kw in delete: %d", UKEY (cmds[0]));
	}
}
