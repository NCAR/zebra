/*
 * Simple time iterpretation.
 */
static char *rcsid = "$Id: TInterp.c,v 2.1 1991-09-13 15:01:58 corbet Exp $";
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

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

