//
// Manage info on data directories.
//
/*		Copyright (C) 1998 by UCAR
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

# if defined(SVR4) || defined(sgi) || defined(__osf__)
#   define USE_STATVFS
# endif

# ifdef USE_STATVFS
#   ifdef __osf__
      extern "C" 
      {
#       include <sys/statvfs.h>
      }
#   else
#     include <sys/statvfs.h>
#   endif
# else
#   ifdef AIXV3
#     include <sys/statfs.h>
#   else
#     include <sys/vfs.h>
#   endif  /* AIX */
    extern "C" int statfs (const char *, struct statfs *);
# endif  /* STATVFS */


static char *rcsid = "$Id: FreeSpace.cc,v 1.2 1999-03-01 16:48:52 burghart Exp $";


int
FreeSpace (const char *dirname)
//
// How much space is free here?
//
{
# ifdef USE_STATVFS
	struct statvfs stbuf;
	if (statvfs (dirname, &stbuf) < 0)
		return (-1);
	return (stbuf.f_frsize*stbuf.f_bavail);
# else
	struct statfs stbuf;
	if (statfs (dirname, &stbuf) < 0)
		return (-1);
	return (stbuf.f_bsize*stbuf.f_bavail);
# endif
}
