/*
 * Change the current time.
 */
# include <signal.h>
# include "../include/defs.h"
# include "../include/message.h"
# include "timer.h"


int msg_handler (), intr (), Slot;
void alrm ();

main (argc, argv)
int argc;
char **argv;
{
	struct tm_prt prt;
	time t;
	char *getenv ();

	if (argc != 3)
	{
		printf ("Usage: settime YYMMDD HHMMSS\n");
		exit (1);
	}
	msg_connect (msg_handler, getenv ("USER"));
/*
 * Put together and send off the request.
 */
	prt.tr_type = TR_PRT;
	prt.tr_time.ds_yymmdd = atoi (argv[1]);
	prt.tr_time.ds_hhmmss = atoi (argv[2]);
	prt.tr_scale = 1;
	msg_send ("Timer", MT_TIMER, FALSE, &prt, sizeof (prt));
}



msg_handler (msg)
struct message *msg;
{
	struct tm_time *t = (struct tm_time *) msg->m_data;

	if (msg->m_proto == MT_TIMER && t->tm_type == TRR_TIME)
		printf ("Time is %d %06d\n", t->tm_time.ds_yymmdd,
			t->tm_time.ds_hhmmss);
	else if (msg->m_proto == MT_TIMER && t->tm_type == TRR_CANCELACK)
	{
		printf ("Cancel ack\n");
		exit (0);
	}
	else if (msg->m_proto == MT_TIMER && t->tm_type == TRR_ALARM)
		tl_DispatchEvent (t);
	else
		printf ("Funky msg proto %d\n", msg->m_proto);
	return (0);
}




intr ()
{
	struct tm_req tr;

	printf ("OUCH! (%d)\n", Slot);
	tl_Cancel (Slot);
}



void
alrm (now, param)
time *now;
char *param;
{
	printf ("Alarm at %d %d, param '%s'\n", now->ds_yymmdd,
		now->ds_hhmmss, param);
}
