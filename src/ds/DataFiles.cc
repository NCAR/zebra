//
// Methods for DataFileCore and DataFile classes
//
//		Copyright (C) 1998 by UCAR
//	University Corporation for Atmospheric Research
//		   All rights reserved
//
// No part of this work covered by the copyrights herein may be reproduced
// or used in any form or by any means -- graphic, electronic, or mechanical,
// including photocopying, recording, taping, or information storage and
// retrieval systems -- without permission of the copyright owner.
// 
// This software and any accompanying written materials are provided "as is"
// without warranty of any kind.  UCAR expressly disclaims all warranties of
// any kind, either express or implied, including but not limited to the
// implied warranties of merchantibility and fitness for a particular purpose.
// UCAR does not indemnify any infringement of copyright, patent, or trademark
// through use or modification of this software.  UCAR does not provide 
// maintenance or updates for its software.
//
# include "DataFiles.h"

RCSID ("$Id")

void
DataFileCore::translate( SerialStream& ss )
//
// Translate ourselves to/from the given SerialStream (depending on the
// stream type).
//
{
//
// Name first...
//
    ss.cstring( dfc_name, sizeof( dfc_name ) );
//
// ...then the rest
//
// We represent our inodes externally using 64 bits, since some OSs (e.g.,
// Irix 6.x) have 64-bit ino_t's.  We break the inode up before and
// reconstruct after, since we don't know whether this is an input or
// output operation.
//
    unsigned long x_inode[2] = {0, 0};
    char *xi = (char*) &x_inode;
    memcpy ((void*)(xi + 8 - sizeof (ino_t)), (void*)&dfc_inode, 
	    sizeof (ino_t));
	
    ss << dfc_begin << dfc_end << dfc_rev << x_inode[0] << x_inode[1] << 
	(int&) dfc_ftype << dfc_nsample;

    memcpy ((void*)&dfc_inode, (void*)(xi + 8 - sizeof (ino_t)), 
	    sizeof (ino_t));
}



ostream& 
DataFileCore::PutTo( ostream& s ) const
{
    ZTime begin( dfc_begin ), end( dfc_end );
	
    s << dfc_name << endl;
    s << "from " << begin << " to " << end << endl;
    s << "revision: " << dfc_rev << ", inode: " << dfc_inode << 
	", type: " << dfc_ftype << ", samples: " << dfc_nsample << endl;
    return s;
}


    
ostream& 
DataFile::PutTo( ostream& s ) const
{
    s << df_fullname << " (source " << df_srcid << ", plat " << df_pid <<
	")\n";
    s << "======== core part ========\n";
    s << df_core;
    return s;
}
