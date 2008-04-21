/* 12/87 jc */
/* $Id: ui_menu.c,v 1.6 1998-02-26 21:18:38 burghart Exp $ */
/*
 * Menuing functions are handled here.
 */
# include "ui_error.h"
# include "ui.h"
# include "ui_menu.h"
# include "ui_commands.h"
# include "ui_loadfile.h"
# include "ui_mode.h"
# include "ui_globals.h"
# include "ui_tty.h"
# include "ui_expr.h"


/*
 * The tag for our special symbol table.
 */
# define MENU_TABLE_NAME	"ui$menu_table"
static stbl Menu_table = 0;


/*
 * The menu invocation stack.
 */
struct mstack
{
	struct mstack *m_next;		/* Next entry in the stack 	*/
	struct menu *m_menu;		/* The actual menu		*/
};
static struct mstack *M_stack = 0;
static struct mstack *M_free = 0;	/* Free list of mstack structs	*/

/*
 * This flag controls whether the user wants to see menu numbers or not.
 */
static bool M_numbers = TRUE;
static bool M_instant = FALSE;	/* Instant selection on numbers */
static bool M_tcm_babble = TRUE; /* Babble in temp command mode */

/*
 * Info string for temp command mode.
 */
static char *Cmodestr =
"~TEMPORARY COMMAND MODE:~ enter your command; you will be returned to\r\n\
menu mode after the command completes.  If you entered this mode by\r\n\
mistake, type ~^U~, and you will be returned to menu mode immediately.\r\n\r\n";
 
/*
 * Prototypes
 */
void um_glom_mstr (struct ui_command *cmds, struct mchoice *cp);
void um_push (struct menu *mp);
void um_cstack (void);
void um_present (struct menu *mp);
void um_enc_choice (int selected, char *dest, char *chstr, int number);
void um_indicate (struct menu *mp, int choice, int line);
void um_represent (struct menu *mp, int old, int new, int line);
void um_stall (void);


 
void
um_init ()
/*
 * Initialize the menu module.
 */
{
/*
 * All we need to do is to get going with a clean symbol table, and a few
 * indirect variables.
 */
 	if (Menu_table)
		usy_z_stbl (Menu_table);
	Menu_table = usy_c_stbl (MENU_TABLE_NAME);
	M_stack = (struct mstack *) 0;
	usy_c_indirect (Ui_variable_table, "ui$menu_numbers", &M_numbers,
			SYMT_BOOL, 0);
	usy_c_indirect (Ui_variable_table, "ui$instant_select", &M_instant,
			SYMT_BOOL, 0);
	usy_c_indirect (Ui_variable_table, "ui$tcm_babble", &M_tcm_babble,
			SYMT_BOOL, 0);
}





struct menu *
um_lookup (name)
char *name;
/*
 * Pull up the structure for this menu.
 */
{
	union usy_value v;
	int type;
/*
 * Find this menu in our symbol table.
 */
 	if (! usy_g_symbol (Menu_table, name, &type, &v))
		return ((struct menu *) 0);
/*
 * Sanity check.  This will only happen in case of severe corruption, or
 * (more likely) somebody mucking around in the menu table.
 */
 	if (type != SYMT_POINTER)
		c_panic ("Menu '%s' does not have pointer type", name);
	return ((struct menu *) v.us_v_ptr);
}



void
um_delete (menu, col)
char *menu;
int col;
/*
 * Handle the DELETE MENU.
 */
{
	struct menu *mp = um_lookup (menu);
/*
 * If there is no such menu, we will not try to delete it.
 */
 	if (! mp)
		ui_cl_error (TRUE, col, "Menu '%s' does not exist", menu);
/*
 * OK, let's get rid of it.
 */
 	usy_z_symbol (Menu_table, menu);
	if (mp->m_nchoice > 0)
		relvm (mp->m_choices);
	relvm (mp);
}




int
um_define (name)
char *name;
/*
 * Handle the DEFINE MENU command.
 */
{
	struct menu *mp, *old;
	int um_within_menu ();
	union usy_value v;
/*
 * Get a menu structure, and fill it in.
 */
 	mp = (struct menu *) getvm (sizeof (struct menu));
	strncpy (mp->m_name, name, MNAMELEN);
	if (strlen (name) >= MNAMELEN)
	{
		ui_warning ("Menu name '%s' truncated to %d characters",
			name, MNAMELEN);
		mp->m_name[MNAMELEN - 1] = '\0';
	}
	mp->m_title[0] = '\0';
	mp->m_default = mp->m_nchoice = mp->m_flags = 0;
/*
 * Allocate a maximal list of menu choices.  This is a bit wasteful, but,
 * as a general rule, menus will come from loadfiles, and the excess will
 * be trimmed off.
 */
 	mp->m_choices = (struct mchoice *)
			getvm (MAXCHOICE * sizeof (struct mchoice));
/*
 * Now we enter the "within menu" restricted parser state, and fill in the
 * menu stuff.
 */
 	ERRORCATCH
		ui_subcommand ("ust$within_menu", "Menu>", um_within_menu,
			(long) mp);
	ON_ERROR
		relvm (mp->m_choices);
		relvm (mp);
		ui_epop ();
		ui_error ("Definition of menu '%s' abandoned due to error",
			name);
	ENDCATCH
/*
 * The user thinks the definition is complete.  Let's see what we think.
 */
 	if (mp->m_nchoice == 0)
	{
		relvm (mp->m_choices);
		relvm (mp);
		ui_error ("Menu '%s' was defined with no choices!", name);
	}
/*
 * Actually define this menu, taking care to clean out the old version,
 * if one exists.
 */
 	if ((old = um_lookup (mp->m_name)))
	{
		relvm (old->m_choices);
		relvm (old);
	}
	v.us_v_ptr = (char *) mp;
	usy_s_symbol (Menu_table, mp->m_name, SYMT_POINTER, &v);
	return (TRUE);
}





int
um_within_menu (mp, cmds)
struct menu *mp;
struct ui_command *cmds;
/*
 * Handle stuff within the DEFINE MENU command.
 */
{
	struct mchoice *chp;
	int um_choice ();
/*
 * Handle all of the easy cases.
 */
	switch (cmds->uc_v.us_v_int)
	{
	   case UIC_TITLE:
	   	strcpy (mp->m_title, cmds[1].uc_v.us_v_ptr);
		return (TRUE);

	   case UIC_ENDDEF:
	   	return (FALSE);

	   case UIC_CHOICE:
	   case UIC_DEFAULT:
	   	break;

	   default:
	   	ui_error ("Bizarre kw in DEFINE MENU: %d",cmds->uc_v.us_v_int);
	}
/*
 * If we get here, we have a menu choice to deal with.  The first thing we
 * do is to gather together the message string and all its parameters,
 * checking on the way.
 */
	chp = mp->m_choices + mp->m_nchoice;
	um_glom_mstr (cmds + 1, chp);
	if (UKEY (*cmds) == UIC_DEFAULT)
		mp->m_default = mp->m_nchoice;
	mp->m_nchoice++;	/* Done AFTER glomming in case of error */
/*
 * Initialize the rest of this choice structure.
 */
	chp->mc_command[0] = chp->mc_next[0] = chp->mc_helpfile[0] = '\0';
	chp->mc_newdef = -1;
/*
 * Time to enter an even further restricted parser state to grab the rest
 * of the choice info.
 */
	ui_subcommand ("ust$menu_choice", "Choice> ", um_choice, (long) chp);
	return (TRUE);
}





void
um_glom_mstr (cmds, cp)
struct ui_command *cmds;
struct mchoice *cp;
/*
 * Gather together the message string into one long, null-separated
 * string, and make sure that there are sufficient params for the msg.
 */
{
	char *str = cp->mc_display, *src = cmds->uc_v.us_v_ptr;
	int nparam = 0;
/*
 * Copy over the format string.
 */
	while (*src)
	{
		if (*src == '#' || *src == '&')
		{
			if (src[1] == *src)
				*str++ = *src++;
			else
				nparam++;
		}
		*str++ = *src++;
	}
	*str++ = '\0';
	cmds++;
/*
 * Now copy any other parameters.
 */
	while (cmds->uc_ctype != UTT_END)
	{
		strcpy (str, UPTR (*cmds));
		str += strlen (str) + 1;
		cmds++;
		nparam--;
	}
/*
 * If things don't match up, bitch.
 */
 	if (nparam > 0)
		ui_error ("Menu string '%s' lacks %d parameters", 
			cp->mc_display, nparam);
	else if (nparam < 0)
		ui_error ("Menu string '%s' has %d excess parameters", 
			cp->mc_display, -nparam);
}



int
um_choice (chp, cmds)
struct mchoice *chp;
struct ui_command *cmds;
/*
 * Handle options that appear within a menu choice.
 */
{
	switch (UKEY (*cmds))
	{
	   case UIC_ENDCHOICE:
	   	return (FALSE);

	   case UIC_COMMAND:
	   	strcpy (chp->mc_command, UPTR (cmds[1]));
		strcat (chp->mc_command, " \r");
		return (TRUE);

	   case UIC_HELPFILE:
	   	strcpy (chp->mc_helpfile, UPTR (cmds[1]));
		return (TRUE);

	   case UIC_MRETURN:
	   	strcpy (chp->mc_next, "PARENT");
		return (TRUE);

	   case UIC_PUSH:
	   	strcpy (chp->mc_next, UPTR (cmds[1]));
		return (TRUE);

	   case UIC_REPLACE:
	   	chp->mc_next[0] = '>';
		strcpy (chp->mc_next + 1, UPTR (cmds[1]));
		return (TRUE);

	   case UIC_NEWDEFAULT:
	   	chp->mc_newdef = UINT (cmds[1]);
		return (TRUE);

	   default:
	   	ui_error ("(BUG) Bizarre kw in um_choice: %d", UKEY (*cmds));
	}
	return (TRUE); /* lint food */
}




int
um_dump (menu)
char *menu;
/*
 * Dump out a description of this menu.
 */
{
	struct menu *mp = um_lookup (menu);
	int ch;
/*
 * Look up our menu.
 */
	if (! mp)
		ui_error ("No such menu: '%s'", menu);
/*
 * Start printing.
 */
 	ui_nf_printf ("Menu name %s\n\tTitle '%s'\n", mp->m_name, mp->m_title);
	ui_printf ("\t%d Choices (def %d):\n", mp->m_nchoice, mp->m_default);
	for (ch = 0; ch < mp->m_nchoice; ch++)
	{
		struct mchoice *chp = mp->m_choices + ch;
		
		ui_nf_printf ("\tDisp: '%s'\n\t\tHelp: '%s'\n",
			chp->mc_display, chp->mc_helpfile);
		ui_nf_printf ("\t\tCmd:  '%s'\n", chp->mc_command);
		ui_printf ("\t\tNext: '%s'\n", chp->mc_next);
	}
	return (TRUE);
}




static int Save_all = 0;

void
um_save (lun, all)
int lun, all;
/*
 * Save our defined menus.
 */
{
	char marker = LF_MENU;
	int um_m_save ();

	bfput (lun, &marker, 1);
	Save_all = all;
	usy_traverse (Menu_table, um_m_save, lun, FALSE);
	bfput (lun, &lun, 0);
}



int
um_m_save (menu, type, v, lun)
char *menu;
int type, lun;
union usy_value *v;
/*
 * Save a single menu.
 */
{
	struct menu *mp = (struct menu *) v->us_v_ptr;
/*
 * See if we really want to save this one.
 */
	if (Save_all == 0 && (mp->m_flags & MF_INIT))
		return (TRUE);
/*
 * Write out the menu structure.
 */
	if (usy_defined (Ui_variable_table, "ui$save_babble"))
		ui_printf ("Saving menu '%s'\n", mp->m_name);
	bfput (lun, mp, sizeof (struct menu));
	bfput (lun, mp->m_choices, mp->m_nchoice*sizeof (struct mchoice));
	return (TRUE);
}



void
um_load (lun, init)
int lun, init;
/*
 * Reload the menu structures from this file.
 */
{
	struct menu *mp;
	int len, type, clen;
	union usy_value v;

	for (;;)
	{
	/*
	 * Read in the next menu structure.
	 */
		mp = (struct menu *) getvm (sizeof (struct menu));
		if ((len = bfget (lun, mp, sizeof (struct menu))) <
				sizeof (struct menu))
		{
			relvm (mp);
			if (len == 0)
				return;
			ui_error ("Bizarre read len %d in loadfile", len);
		}
	/*
	 * Read in the choice info.
	 */
		len = mp->m_nchoice * sizeof (struct mchoice);
		mp->m_choices = (struct mchoice *) getvm (len);
		if (bfget (lun, (char *) mp->m_choices, len) != len)
			ui_warning ("Bad read length on menu '%s'", mp->m_name);
	/*
	 * Define this menu.
	 */
		mp->m_flags = init ? MF_INIT : 0;
		v.us_v_ptr = (char *) mp;
		usy_s_symbol (Menu_table, mp->m_name, SYMT_POINTER, &v);
	}
}





int
um_mode (cmds)
struct ui_command *cmds;
/*
 * Enter menu mode.
 */
{
	struct menu *mp;
	char *newmenu = 0;
	int type;
	union usy_value v;
/*
 * Menu mode when running in batch makes absolutely no sense whatsoever.
 */
 	if (! ut_interactive ())
		ui_error ("Menus in batch mode are ridiculous!");
/*
 * Look into what menu we want to start with.  We insist that (1) such a
 * menu be provided, or (2) there be something on the stack, or (finally)
 * the symbol ui$default_menu be defined to something meaningful.
 */
	if (cmds->uc_ctype != UTT_END)
	{
		newmenu = UPTR (*cmds);
		um_cstack ();
	}
	else if (! M_stack)
	{
		if (! usy_g_symbol (Ui_variable_table, "ui$default_menu",
				&type, &v))
			ui_error ("You must provide an initial menu name.");
		if (type != SYMT_STRING)
			ui_error ("Non-string value for ui$default_menu");
		newmenu = v.us_v_ptr;
	}
/*
 * If we have a new menu to deal with, look it up and put onto the stack.
 */
 	if (newmenu)
	{
		if ((mp = um_lookup (newmenu)) == 0)
			ui_error ("Unknown menu: '%s'", newmenu);
		um_push (mp);
	}
/*
 * The menu stack is now in good shape.  Let's push in our major mode,
 * and then we're done.
 */
 	ui_push_mode (M_MENU);
	return (TRUE);
}



void
um_push (mp)
struct menu *mp;
/*
 * Push an invocation of this menu onto the stack.
 */
{
	struct mstack *ent, *um_get_stack ();

	ent = um_get_stack ();
	ent->m_menu = mp;
	ent->m_next = M_stack;
	M_stack = ent;
}




struct mstack *
um_get_stack ()
/*
 * Return a free stack entry.
 */
{
	struct mstack *ent;
/*
 * If there is one sitting on the free list, use it.
 */
	if (M_free)
	{
		ent = M_free;
		M_free = M_free->m_next;
		return (ent);
	}
/*
 * Nope.  Gotta allocate a new one.
 */
 	return ((struct mstack *) getvm (sizeof (struct mstack)));
}


void
um_mpop ()
/*
 * Remove the top entry from the menu stack.
 */
{
	struct mstack *ent;
	
	if (! M_stack)
		c_panic ("Attempt to pop an empty menu stack.");
	ent = M_stack;
	M_stack = M_stack->m_next;
	ent->m_next = M_free;
	M_free = ent;
}



void
um_cstack ()
/*
 * Entirely clear the menu stack.
 */
{
	while (M_stack)
		um_mpop ();
}


void
um_do_menu (cmds, initial, prompt)
struct ui_command **cmds;
char *initial, *prompt;
/*
 * The top level menu-mode driver.
 */
{
	int choice, line;
	struct menu *mp;
	unsigned char tty_readch (), c;
	struct ui_command *uip_parse ();
	char pbs[2];

	if (Nlines > 0)
		um_stall ();
/*
 * Present our current menu.
 */
top:
	if (! M_stack)
		c_panic ("Menu mode with no menu stack");
 	um_present (mp = M_stack->m_menu);
	choice = mp->m_default;
	line = mp->m_nchoice + 6;
/*
 * Do something.
 */
	for (;;)
	{
		int oldc = choice;

		switch (c = tty_readch ())
		{
		/*
		 * Arrows just move up or down.
		 */
		   case UP_ARROW:
		   case '\013':
			choice = (choice == 0) ? mp->m_nchoice - 1 : choice -1;
		   	um_represent (mp, oldc, choice, line);
			continue;

		   case DOWN_ARROW:
		   case '\n':
		   	choice = (choice >= mp->m_nchoice-1) ? 0 : choice + 1;
		   	um_represent (mp, oldc, choice, line);
			continue;
		/*
		 * The single digits can be used to go directly to a
		 * specific menu entry.
		 */
		   case '0': case '1': case '2': case '3': case '4':
		   case '5': case '6': case '7': case '8': case '9':
			if ((choice = c - '0') > mp->m_nchoice - 1)
			{
				choice = oldc;
				tty_bell ();
				tty_flush ();
			}
			else
			{
				um_represent (mp, oldc, choice, line);
				if (M_instant)
				{
					c = '\r';
					break;
				}
			}
			continue;
		/*
		 * Help!
		 */
		   case '?':
			if (mp->m_choices[choice].mc_helpfile[0])
				ui_help (mp->m_choices[choice].mc_helpfile);
			else
	ui_printf ("Sorry, no help available.  Go harass the menu designer.\n");
			um_stall ();
			mp->m_default = choice;
			um_present (mp);
			continue;
		/*
		 * Redraw.
		 */
		   case '\014':
		   	mp->m_default = choice;
			um_present (mp);
			continue;
		/*
		 * Anything else we interpret as command mode text.
		 */
		   default:
		   	break;
		}
		break;
	}
/*
 * Now that we have dropped out of the loop, we need to decide what to do
 * next.  A carriage return implies that we should execute this particular
 * choice.
 */
 	if (c == '\r')
	{
		struct mchoice *mcp = mp->m_choices + choice;
	/*
	 * Immediately highlight this line, so that the user knows that
	 * we heard.
	 */
	 	um_indicate (mp, choice, line);
	/*
	 * Figure out which menu we are doing next.
	 */
		Nlines = 0;
		if (! um_do_next (mcp->mc_next))
		{
			ui_endmode (M_MENU);
			return;	/* Ran out of menus */
		}
		mp->m_default = (mcp->mc_newdef >= 0) ? mcp->mc_newdef :choice;
	/*
	 * If we have a command to execute, push it onto the control stack,
	 * and return to the command interpreter.  Control will end up back
	 * here after things have been executed, unless we leave menu mode
	 * in the meantime.
	 */
		if (mcp->mc_command[0])
		{
			ui_pushcmd (mcp->mc_command);
			return;
		}
		goto top;
	}
/*
 * Go into temporary command mode, and get the command currently being
 * typed.
 */
	if (M_tcm_babble)
	 	tty_sout (Cmodestr);
	ucs_tty_cmode ();
	pbs[0] = c; pbs[1] = '\0';
	ut_pushback (pbs, 0);
	ERRORCATCH
		*cmds = uip_parse (initial, prompt, TRUE, FALSE);
	ON_ERROR
		cmds = 0;
		ucs_pop_cstack ();
		RESIGNAL;
	ENDCATCH
	ucs_pop_cstack ();	/* zorch cmode */
	if (*cmds == 0)
		goto top;
	return;
}




int
um_do_next (next)
char *next;
/*
 * See what our next menu is.
 */
{
	struct menu *mp;
/*
 * If the menu designer said nothing, we just keep the current menu.
 */
 	if (next[0] == '\0')
		return (TRUE);
/*
 * if the next menu is "PARENT", we pop the stack.
 */
 	if (! strcmp (next, "PARENT"))
	{
		um_mpop ();
		return (M_stack != 0);
	}
/*
 * A new menu preceeded by ">" means that we replace the current menu
 * with this one.
 */
 	if (next[0] == '>')
	{
		um_mpop ();
		next++;
	}
/*
 * Otherwise we lookup and push the new menu.
 */
 	if (! (mp = um_lookup (next)))
		ui_error ("(MENU BUG): No such menu: '%s'", next);
	um_push (mp);
	return (TRUE);
}






void
um_present (mp)
struct menu *mp;
/*
 * Put this menu nicely upon the screen.
 */
{
	char string[200];
	int choice;
/*
 * Clear the screen, and put the title on the top line.
 */
 	tty_clear ();
	sprintf (string, "(%s) %s\r\n\n", mp->m_name, mp->m_title);
	tty_sout (string);
/*
 * Now go through and put out each choice.
 */
 	string[0] = ' ';
 	for (choice = 0; choice < mp->m_nchoice; choice++)
	{
		bool def = (mp->m_default == choice);
		um_enc_choice (def, string, mp->m_choices[choice].mc_display,
				choice);
		tty_sout (string);
		tty_string ("\r\n");
	}
/*
 * Put out the prompt, and we're done.
 */
 	tty_string (
	  "\nUse arrow keys to move, <RETURN> to select, ? for help\r\n\n");
	tty_flush ();
}



void
um_enc_choice (selected, dest, chstr, number)
bool selected;
char *dest, *chstr;
int number;
/*
 * Encode this choice, which is expressed as a message string.
 */
{
	char *param;
	struct parse_tree *pt;
	int type;
	union usy_value v;

	param = chstr + strlen (chstr) + 1;
/*
 * Throw in the choice number, if requested.
 */
 	if (M_numbers)
		sprintf (dest, selected ? "    --> `(%d)`%s " :
			"        (%d)%s ", number, number > 9 ? "" : " ");
	else
		strcpy (dest, selected ? "    `--> " : "        ");
	while (*dest)
		dest++;
/*
 * Pass through the control string, and put together the message.
 */
 	for (; *chstr; chstr++)
	{
	/*
	 * '#' is the parameter substitution character.
	 */
		if (*chstr == '#')
		{
		/*
		 * '##', however, is just a #.
		 */
			if (chstr[1] == '#')
			{
				*dest++ = '#';
				chstr++;
			}
			else
			{
			/*
			 * Now parse and print the parameter.
			 */
				if ((pt = ue_parse (param, 0, FALSE)) == 0)
					strcpy (dest, param);
				else
				{
					ERRORCATCH
						ue_eval (pt, &v, &type);
						ue_enc_val (&v, type, dest);
					ENDCATCH
					ue_rel_tree (pt);
				}
				dest += strlen (dest);
				param += strlen (param) + 1;
			}
		}
	/*
	 * '&' implies straight string substitution.
	 */
		else if (*chstr == '&')
		{
		/*
		 * '&&', however, is just a &.
		 */
			if (chstr[1] == '&')
			{
				*dest++ = '&';
				chstr++;
			}
			else
			{
				ui_enc_sym (dest, param);
				/* strcpy (dest, param); */
				dest += strlen (dest);
				param += strlen (param) + 1;
			}
		}
		else
			*dest++ = *chstr;
	}
	if (selected && ! M_numbers)
		*dest++ = '`';
	*dest = '\0';
}




void
um_indicate (mp, choice, line)
struct menu *mp;
int choice, line;
/*
 * Indicate that this choice has been selected.
 */
{
	char string[120];

 	um_enc_choice (FALSE, string, mp->m_choices[choice].mc_display,choice);
	strcat (string, "    "); /* it may have gotten shorter */
	strncpy (string, "   ***", 6);
	tty_standout ();
	tty_move (choice + 3, 1);
	tty_sout (string);
	tty_standin ();
	tty_move (line, 1);
	tty_flush ();
}



void
um_represent (mp, old, new, line)
struct menu *mp;
int old, new, line;
/*
 * Redraw the menu to reflect the fact that the selection has changed from
 * OLD to NEW.  LINE is the screen line on which to leave the cursor.
 */
{
	char string[120];
/*
 * Redraw the OLD choice.
 */
 	um_enc_choice (FALSE, string, mp->m_choices[old].mc_display, old);
	strcat (string, "    "); /* it may have gotten shorter */
	tty_move (old + 3, 1);
	tty_sout (string);
/*
 * Now redraw the NEW one.
 */
 	um_enc_choice (TRUE, string, mp->m_choices[new].mc_display, new);
	strcat (string, "   ");
	tty_move (new + 3, 1);
	tty_sout (string);
/*
 * Done.
 */
	tty_move (line, 1);
	tty_flush ();
}



void
um_stall ()
/*
 * Force a wait before drawing a new menu.
 */
{
	unsigned char tty_readch ();
/*
 * Put out the stall message.
 */
	if (! Bol)
		tty_sout ("\r\n");
	tty_sout ("`  Hit any key to continue  `");
	tty_flush ();
/*
 * Now wait for (and discard) the first character that comes in.
 */
	(void) tty_readch ();
	Nlines = 0;
	tty_sout ("\r\n");
	Bol = TRUE;
}
