
#include "defs.h"
#include "message.h"

RCSID("$Id: msg_test.c,v 2.1 1995-04-25 17:40:47 granger Exp $")

int
main ()
{
	msg_log ("Message number one");
	msg_log ("Message %d", 2);
	msg_log ("%s %d %s %f", "integer", 3, "float", 4.5);
	msg_ELog (EF_INFO, "Message number one");
	msg_ELog (EF_PROBLEM, "Message %d", 2);
	msg_connect (0, "message test");
	msg_ELog (EF_EMERGENCY, "%s %d %s %f", "integer", 3, "float", 4.5);
	return (0);
}

