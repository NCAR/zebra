//
// Implementation of the tape class.
//
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

# include <unistd.h>
# include <stream.h>
# include <errno.h>
# include "Tape.h"

static char *rcsid = "$Id: Tape.cc,v 1.1 1992-09-10 22:26:51 corbet Exp $";

Tape::Tape (const char *drive)
//
// Open up a tape.
//
{
//
// Get the drive open, and we are done.
//
	if ((t_fd = open (drive, O_RDONLY, 0)) < 0)
		cerr << "Unable to open drive " << drive << ".\n";
	else
		rewind ();
	t_status = Normal;
}




Tape::~Tape ()
//
// Destructor.
//
{
	close (t_fd);
}




void
Tape::rewind ()
//
// Rewind the tape.
//
{
	struct mtop mt_cmd;

	mt_cmd.mt_op = MTREW;
	mt_cmd.mt_count = 1;	// Once will do.
	if (ioctl (t_fd, MTIOCTOP, &mt_cmd) < 0)
		cerr << "Error rewinding tape\n";
	t_fileno = 0;
	t_status = Normal;
}




void
Tape::skip (int fno)
//
// Skip to this file.
//
{
	struct mtop mt_cmd;

	mt_cmd.mt_op = MTFSF;
	mt_cmd.mt_count = fno;
	if (ioctl (t_fd, MTIOCTOP, &mt_cmd) < 0)
		cerr << "Error skipping tape\n";
	t_fileno += fno;
	t_status = Normal;
}



int
Tape::getblock (char *buf, int bufsize)
//
// Read in a tape block.
//
{
	int nread;
//
// Don't let them read once we've hit EOT.
//
	if (t_status == Eot)
		return (TS_EOT);
//
// Do a read.
//
	if ((nread = read (t_fd, buf, bufsize)) > 0)
	{
		t_status = Normal;
		return (nread);
	}
//
// See what went wrong.  If we got zero back, we hit an EOF; we need
// to see if it is the second one.
//
	if (nread == 0)
	{
		if (t_status == Eof)
		{
			t_status = Eot;
			return (TS_EOT);
		}
		t_fileno++;
		t_status = Eof;
		return (getblock (buf, bufsize));
	}
//
// Hmm....
//
	cerr << "Error " << errno << " reading tape\n";
	return (TS_ERROR);
}


