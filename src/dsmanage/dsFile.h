//
// The dsFile class
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
# ifndef _dsFile_h_
# define _dsFile_h_

# include <defs.h>
# include <DataStore.h>

//
// Data files are described by this class.  We'll have a bunch of these, so
// we try to keep them small...
//
class dsFile
{
    int srcid;
    PlatformId pid;
    ZebraTime begintime;
public:
    dsFile (const DataFile* df);
    dsFile (const dsFile& dsf);
//
// File size in bytes
//
    unsigned int size () const;
//
// begin and end times of the file
//
// The pointers returned by begin() and end() are good until the next call
// to each of the methods.
// 
    const ZebraTime *begin (void) const { return &begintime; }
    const ZebraTime *end (void) const;
//
// The name returned below is good until the next call to dsFile::name()
//
    const char *name () const;
private:
    const DataFile *GetFile (void) const;
};

# endif // _dsFile_h_
