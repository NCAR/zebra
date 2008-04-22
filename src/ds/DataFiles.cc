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
// Name first...  There's no operator to translate the FileType enum
// directly, so we translate it back and forth between an int.
//
    ss.cstring( dfc_name, sizeof( dfc_name ) );
    int ft = dfc_ftype;
    ss << dfc_begin << dfc_end << dfc_rev << dfc_inode << ft << dfc_nsample;
    dfc_ftype = (FileType)ft;
}



std::ostream& 
DataFileCore::PutTo( std::ostream& s ) const
{
    using std::endl;
    ZTime begin( dfc_begin ), end( dfc_end );
	
    s << dfc_name << endl;
    s << "from " << begin << " to " << end << endl;
    s << "revision: " << dfc_rev << ", inode: " << dfc_inode << 
	", type: " << dfc_ftype << ", samples: " << dfc_nsample << endl;
    return s;
}


    
std::ostream& 
DataFile::PutTo( std::ostream& s ) const
{
    s << df_fullname << " (source " << df_srcid << ", plat " << df_pid <<
	")\n";
    s << "======== core part ========\n";
    s << df_core;
    return s;
}
