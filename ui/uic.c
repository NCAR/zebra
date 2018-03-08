# include <stdlib.h>
# include "ui_error.h"
# include "ui_globals.h"
# include "ui.h"


/*
 * This string holds the initial state used by uic.  By changing it
 * with a SSET, it is possible to switch to a different set of states,
 * for debugging stuff.
 */
static char Istate[80];

static void dump_cmd (struct ui_command *cmd);


main (argc, argv)
int argc;
char **argv;
{
	int c_handler ();
	char *loadfile = getenv ("LOADFILE");
/*
 * Initialize.
 */
	strcpy (Istate, "ust$boot_initial");
	ERRORCATCH
	ui_init (loadfile ? loadfile : LF, TRUE, FALSE);
	ON_ERROR
		ui_printf ("No loadfile!\n");
		exit (1);
	ENDCATCH
	ui_setup ("uic", &argc, argv, 0);
/*
 * Now go for it.
 */
	usy_c_indirect (Ui_variable_table, "uic$istate", Istate,
		SYMT_STRING, 80);
	ERRORCATCH
		ui_get_command (Istate, "Ui>", c_handler, 0);
	ON_ERROR
		printf ("Error caught\n");
	ENDCATCH
	ui_finish ();
	exit (0);
}




/* ARGSUSED */
c_handler (arg, cmds)
int arg;
struct ui_command *cmds;
/*
 * Dump out the returned cmd structure.
 */
{
 	ui_printf ("***CMD dump\n");
	for (;;)
	{
		dump_cmd (cmds);
		if (cmds->uc_ctype == UTT_END)
			return (TRUE);
		cmds++;
	}
}



static void
dump_cmd (cmd)
struct ui_command *cmd;
{
	switch (cmd->uc_ctype)
	{
	   case UTT_END:
	   	ui_printf ("End of token list\n");
		return;
	   case UTT_VALUE:
		if (cmd->uc_vptype == SYMT_STRING)
		   	ui_printf ("Value param: '%s'\n", cmd->uc_v.us_v_ptr);
		else
			ui_printf ("Value param: %d\n", cmd->uc_v.us_v_int);
		return;
	   case UTT_OTHER:
	   	ui_printf ("Something weird...\n");
		return;
	   case UTT_KW:
	   	ui_printf ("Keyword number %d\n", cmd->uc_v.us_v_int);
		return;
	   default:
	   	ui_printf ("Something REALLY weird -- type = %d", cmd->uc_ctype);
	}
}
