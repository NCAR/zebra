/* 1/87 jc */
/*
 * Command table handling
 */
# include <string.h>
# include "ui.h"
# include "ui_globals.h"
# include "ui_loadfile.h"


void
uct_init ()
/*
 * Initialize the command table.
 */
{
	Ui_command_table = usy_c_stbl ("ui$command_table");
}




char *
uct_lookup_command (name)
char *name;
/*
 * Attempt to look up a command.
 * Entry:
 *	NAME	is the name of the command.
 * Exit:
 *	If the command has been defined, the associated string is returned.
 */
{
	union usy_value v;
	int type;
	
	if (! usy_g_symbol (Ui_command_table, name, &type, &v))
		return (0);
	if (type != SYMT_STRING)
		ui_error ("(BUG) Somebody put sym '%s' type %d into the command table!", name, type);
	return (v.us_v_ptr);
}





void
uct_def_command (name, string)
char *name, *string;
/*
 * Handle the DEFINE COMMAND command.
 */
{
	union usy_value v;
/*
 * define the command.
 */
 	v.us_v_ptr = string;
	usy_s_symbol (Ui_command_table, name, SYMT_STRING, &v);
}




void
uct_dump_command (cmd)
char *cmd;
/*
 * Dump out the definition of this command.
 */
{
	char *clist;
/*
 * Lookup this command.
 */
 	if ((clist = uct_lookup_command (cmd)) == 0)
		ui_error ("No definition for command '%s'", cmd);
/*
 * Now put out the info.
 */
 	ui_printf ("%s = '%s'\n", cmd, clist);
}



void
uct_save (lun)
int lun;
/*
 * Save all defined commands to our load file.
 */
{
	char marker = LF_COMMAND;
	int uct_sv_command ();
/*
 * Output the marker, romp through the table, then put the end marker in.
 */
 	bfput (lun, &marker, 1);
	usy_traverse (Ui_command_table, uct_sv_command, lun, FALSE);
	bfput (lun, &lun, 0);
}


int
uct_sv_command (command, type, v, lun)
char *command;
int type, lun;
union usy_value *v;
/*
 * Save a single command.
 */
{
	char tbuf[400];
	int len;
/*
 * Append the sub string to the command symbol, marking the juncture
 * between the two with the magic symbol !.
 */
 	strcpy (tbuf, command);
	strcat (tbuf, "!");
	strcat (tbuf, v->us_v_ptr);
/*
 * Write it out.
 */
 	bfput (lun, tbuf, strlen (tbuf) + 1);
	return (TRUE);
}


void
uct_load (lun)
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
			ui_error ("Funky command: '%s'", tbuf);
		*bang++ = '\0';
		v.us_v_ptr = bang;
		usy_s_symbol (Ui_command_table, tbuf, SYMT_STRING, &v);
	}
}



void
uct_delete (command, col)
char *command;
int col;
/*
 * Delete a defined command.
 */
{
	if (! usy_defined (Ui_command_table, command))
		ui_cl_error (TRUE, col, "Command '%s' does not exist",command);
	usy_z_symbol (Ui_command_table, command);
}
