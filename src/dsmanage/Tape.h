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

extern "C"
{
#	include <fcntl.h>
#	include <sys/types.h>
#	include <sys/ioctl.h>
#	include "zl_mtio.h"
}

//
// A class for dealing with tapes.
//
// This class is not set up to be robust with respect to copy and 
// initialization operations.
//
const int TS_EOF = 0, TS_EOT = -2, TS_ERROR = -3;

class Tape
{
	int	t_fd;		// Our file descriptor
	int	t_fileno;	// What tape file we are on.
	enum { Normal, Eof, Eot } t_status;
public:
	Tape (const char *, int = 0);
	~Tape ();
	void rewind ();
	int getblock (char *, int);
	int putblock (const char *, int);
	void skip (int);
	int filenum () const { return t_fileno; }
	int OK () const { return (t_fd >= 0); };
	void WriteEof ();
};


