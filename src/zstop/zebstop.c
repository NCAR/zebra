/*
 * Message server zapper.
 */
# include "message.h"

main ()
{
	int handler ();
	struct mh_template tm;

	if (! msg_connect (handler, "Grim reaper"))
		exit (0);
	tm.mh_type = MH_DIE;
	msg_send (MSG_MGR_NAME, MT_MESSAGE, 0, &tm, sizeof (tm));
	exit (0);
}



handler ()
{
	return (0);
}
