/*
 * Message server zapper.
 */
# include "message.h"

main ()
{
	int handler ();
	struct mh_template tm;

	msg_connect (handler, "Grim reaper");
	tm.mh_type = MH_DIE;
	msg_send (MSG_MGR_NAME, MT_MESSAGE, 0, &tm, sizeof (tm));
	msg_await ();
}



handler ()
{
	printf ("Huh?  Handler called\n");
	return (0);
}
