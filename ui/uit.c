# include <setjmp.h>
# include "robot_include:error.h"
# include "ui_include:ui_state.h"
# include "ui_include:ui_token.h"
# include "ui_include:ui_globals.h"
# include "ui_include:ui.h"




main ()
{
	struct state_table_entry *state;
	struct state_action *action;
	struct token tk;
	struct ui_command *uip_parse (), *cmds;
	char sname[80];
	int ambig = 0, kwnum = 0, c_handler ();
/*
 * Initialize.
 */
	ui_init ("robot.slf", TRUE);
	ERRORCATCH
		ui_get_command ("robot-initial", "Robot>", c_handler, 0);
	ON_ERROR
		printf ("Error caught\n");
	ENDCATCH
}




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
	return (TRUE);
}




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
