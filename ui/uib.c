# include "ui.h"


main ()
{
	ui_init ((char *) 0, 1, 0);
	ust_bootstrap (usy_g_stbl ("ui$state_table"));
	ui_get_command ("ust$boot_initial", "Boot>");
	ui_finish ();
}
