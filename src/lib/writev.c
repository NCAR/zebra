/*
 * Emulation for (sadly) missing readv and writev routines.
 */
# include <sys/unistd.h>
# include <sys/types.h>
# include <sys/uio.h>
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


int
fcc_writev (fd, iov, niov)
int fd, niov;
struct iovec *iov;
/*
 * Vectored write.
 */
{
	int i, nwrote, total = 0;

	for (i = 0; i < niov; i++)
	{
		if ((nwrote = write (fd, iov[i].iov_base, iov[i].iov_len))
				< iov[i].iov_len)
			return (nwrote < 0 ? nwrote : total + nwrote);
		total += nwrote;
	}
	return (total);
}




int
fcc_readv (fd, iov, niov)
int fd, niov;
struct iovec *iov;
/*
 * Vectored read.
 */
{
	int i, nread, total = 0;

	for (i = 0; i < niov; i++)
	{
		if ((nread = read (fd, iov[i].iov_base, iov[i].iov_len))
				< iov[i].iov_len)
			return (nread < 0 ? nread : total + nread);
		total += nread;
	}
	return (total);
}

