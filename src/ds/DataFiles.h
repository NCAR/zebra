/*
 * $Id: DataFiles.h,v 3.3 2002-09-17 20:00:18 granger Exp $
 *
 * Application interface to DataStore DataFile abstraction, for both
 * clients and daemon. 
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

# ifndef _DataFiles_h_
# define _DataFiles_h_

# include <config.h>		/* CFG_ symbol definitions */
# include <sys/types.h>		/* inode type for DataFile */

# include "DataTypes.h"

# if __cplusplus	/* extra headers for C++ */
#   include <iostream>
#   include <SerialStream.hh>
#   include <SerialZTime.hh>	/* ZTime with SerialStream methods */
# else
    typedef ZebraTime ZTime;	/* still need a ZTime type under C */
# endif	/* C++ extra headers */

/*
 * The core information for a data file, as stored in the persistent
 * data file cache.  The name here is pathless; the path must be derived
 * based on the platform and source.  This is used by itself only on the
 * daemon side.  Clients use the full DataFile (defined below).
 */
struct ds_DataFileCore
{
    char	dfc_name[CFG_DATAFILE_LEN];	/* The name of the file	*/
    ZTime	dfc_begin;		/* When the data begins		*/
    ZTime	dfc_end;		/* When it ends			*/
    long	dfc_rev;		/* Revision count		*/
    ino_t	dfc_inode;		/* Inode number			*/
    FileType	dfc_ftype;		/* Type of this file		*/
    unsigned int dfc_nsample;		/* Sample count for this file	*/
};

/*
 * C: DataFileCore is just a typedef for ds_DataFileCore
 *
 * C++: declare the DataFileCore class which subclasses the ds_DataFileCore
 * structure and adds functions for storage and retrieval and such.
 */

# if !__cplusplus
typedef struct ds_DataFileCore DataFileCore;
# else
class DataFileCore : public ds_DataFileCore
{
public:
    void translate( SerialStream& ss );
    std::ostream& PutTo( std::ostream& s ) const;
};

//
// We need the SerialStream << and >> operators for DataFileCore, so that
// we can store them as values in a BTree.  The operators defined by the
// SERIAL_STREAMABLE macro use the DataFileCore::translate() method.
//
// WARNING: Don't try to implement the "Translatable" interface with 
// DataFileCore in order to get these operators!  Since DataFileCore is
// a struct on the C side and an object on the C++ side, we can run
// into problems with the virtual table required when using "Translatable".
//
SERIAL_STREAMABLE( DataFileCore );

//
// Output operator for a regular ostream
//
inline std::ostream& 
operator <<(std::ostream& s, const DataFileCore& dfc)
{
    return dfc.PutTo( s );
}

# endif /* DataFileCore C/C++ choice */


/*
 * The full data file structure, with the core from above, and including 
 * full path file name, PlatformId, and SourceId.
 */
struct ds_DataFile
{
    DataFileCore	df_core;
    char	df_fullname[CFG_FILEPATH_LEN];
    int		df_pid;
    int		df_srcid;
};

    
/*
 * Under C the DataFile type is simply the structure.  C++ uses a class.
 */
# if !__cplusplus
typedef struct ds_DataFile DataFile;
# else
class DataFile : public ds_DataFile
{
public:
    std::ostream& PutTo( std::ostream& s ) const;
};

inline std::ostream& 
operator <<(std::ostream& s, const DataFile& df)
{
    return df.PutTo( s );
}

# endif /* DataFile C/C++ choice */

# endif /* _DataFiles_h_ */
