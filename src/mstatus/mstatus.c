/*
 * Message server status grabber.
 */
# include "message.h"

main ()
{
	int handler ();
	struct mh_template tm;

	if (! msg_connect (handler, "Status reporter"))
		exit (1);
	tm.mh_type = MH_STATS;
	msg_send (MSG_MGR_NAME, MT_MESSAGE, 0, &tm, sizeof (tm));
	msg_await ();
}



handler (msg)
struct message *msg;
{
	if (msg->m_proto != MT_MESSAGE)
		return;
	if (msg->m_len == 0)
		return (1);
	printf ("%s\n", msg->m_data);
	return (0);
}
