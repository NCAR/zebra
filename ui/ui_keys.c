/* 4/87 jc */
/*
 * Special key handling.
 */
# include <string.h>

# include "ui_tty.h"
# include "ui.h"
# include "ui_loadfile.h"


/*
 * The tag for our symbol table.
 */
static stbl Key_table = 0;

/*
 * Prototypes 
 */
int uk_sv_key (char *key, int type, union usy_value *v, int lun);




void
uk_init ()
/*
 * Initialize the keyboard module.
 */
{
	if (Key_table)
		usy_z_stbl (Key_table);
	Key_table = usy_c_stbl ("ui$key_table");
}




void
uk_define_key (cmds)
struct ui_command *cmds;
/*
 * Handle the DEFINE KEY command.
 */
{
	union usy_value v;
	char def[300];
/*
 * Make sure that the key is known.
 */
 	if (tty_get_key_code (UPTR (*cmds)) == 0)
		ui_cl_error (TRUE, cmds->uc_col,
			"Unknown function key name: '%s'", UPTR (*cmds));
/*
 * Just perform the definition.
 */
	strcpy (def, UPTR (cmds[1]));
	strcat (def, "\r");
 	v.us_v_ptr = def;
 	usy_s_symbol (Key_table, UPTR (*cmds), SYMT_STRING, &v);
}




char *
uk_get_definition (key)
char *key;
/*
 * Return the definition for this key, if there is one.  If not, NULL is
 * returned.
 */
{
	union usy_value v;
	int type;
/*
 * Perform a lookup on this symbol.
 */
 	if (usy_g_symbol (Key_table, key, &type, &v) == 0)
		return ((char *) 0);
	if (type != SYMT_STRING)
		ui_error ("Somebody munged definition for sym '%s' in key tbl",
			key);
	return (v.us_v_ptr);
}



void
uk_delete (key, col)
char *key;
int col;
/*
 * Zap the definition of this key.
 */
{
/*
 * Make sure that the key is known.
 */
 	if (tty_get_key_code (key) == 0)
		ui_cl_error (TRUE, col, "Unknown function key name: '%s'",key);
/*
 * Just zap it out of the table.
 */
 	usy_z_symbol (Key_table, key);
}




void
uk_save (lun)
int lun;
/*
 * Save all defined keys to our load file.
 */
{
	char marker = LF_KEYS;
/*
 * Output the marker, romp through the table, then put the end marker in.
 */
 	bfput (lun, &marker, 1);
	usy_traverse (Key_table, uk_sv_key, lun, FALSE);
	bfput (lun, &lun, 0);
}



int
uk_sv_key (key, type, v, lun)
char *key;
int type, lun;
union usy_value *v;
/*
 * Save a single command.
 */
{
	char tbuf[400];
	int len;
/*
 * Append the sub string to the key symbol, marking the juncture
 * between the two with the magic symbol !.
 */
 	strcpy (tbuf, key);
	strcat (tbuf, "!");
	strcat (tbuf, v->us_v_ptr);
/*
 * Write it out.
 */
 	bfput (lun, tbuf, strlen (tbuf) + 1);
	return (TRUE);
}


void
uk_load (lun)
int lun;
/*
 * Bring in command definitions from the file.
 */
{
	char tbuf[400], *bang;
	union usy_value v;

	for (;;)
	{
		if (bfget (lun, tbuf, 400) <= 0)
			return;
		if ((bang = strchr (tbuf, '!')) == 0)
			ui_error ("Funky key: '%s'", tbuf);
		*bang++ = '\0';
		v.us_v_ptr = bang;
		usy_s_symbol (Key_table, tbuf, SYMT_STRING, &v);
	}
}

