/*
 * Network reading.
 */
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

# include <errno.h>
# include "defs.h"

RCSID("$Id: netread.c,v 2.2 1995-04-15 00:30:54 granger Exp $")


int
msg_netread (fd, dest, len)
int fd, len;
char *dest;
/*
 * Read from this file descriptor until (1) an error occurs, or (2) the full
 * length has been read.
 */
{
	int nread = 0, status;

	while (nread < len)
	{
		if ((status = read (fd, dest + nread, len - nread)) <= 0)
			return (errno == EWOULDBLOCK ? nread : status);
		nread += status;
	}
	return (nread);
}



int
msg_XX_netread (fd, dest, len)
int fd, len;
char *dest;
/*
 * Read from this file descriptor until (1) an error occurs, or (2) the full
 * length has been read.  Brute force it with ewouldblock errors.
 */
{
	int nread = 0, status;

	while (nread < len)
	{
		if ((status = read (fd, dest + nread, len - nread)) <= 0)
		{
			if (errno == EWOULDBLOCK && nread > 0)
				continue;
			return (status);
		}
		nread += status;
	}
	return (nread);
}
