/*
 * Simple time iterpretation.
 */
static char *rcsid = "$Id: TInterp.c,v 2.0 1991-07-18 23:07:02 corbet Exp $";

# include <ctype.h>



int InterpDTime (trigger)
char *trigger;
/*
 * Try to interpret this string as a delta time.
 */
{
	int seconds = 0;
/*
 * Interpret as much as possible as a number.
 */
	while (isdigit (*trigger))
		seconds = seconds*10 + *trigger++ - '0';
/*
 * Insist on, at most, a following "m" or "s".
 */
	if (! *trigger || (*trigger == 's' && trigger[1] == '\0'))
		return (seconds);
	else if (trigger[0] == 'm' && trigger[1] == '\0')
		return (seconds*60);
	else if (trigger[0] == 'h' && trigger[1] == '\0')
		return (seconds*3600);
	else if (trigger[0] == 'd' && trigger[1] == '\0')
		return (seconds*24*3600);
	else
		return (0);
}

