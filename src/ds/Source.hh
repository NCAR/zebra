//
// Source: a class holding a large list of files; each file can be retrieved
// by platform and time.  
// $Id: Source.hh,v 3.3 2001-08-24 22:23:11 granger Exp $
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
# ifndef _zebra_Source_hh_
# define _zebra_Source_hh_

# include <string>
# include <map>

# include <BlockFile.hh>
# include <ZTime.hh>

# include "DataFiles.h"
# include "Platforms.h"

template <class K, class T> class BTreeFile;

//
// PlatFileList: BTree of DataFileCores for a given platform, keyed by 
// begin time
//
typedef BTreeFile<ZTime,DataFileCore> PlatFileList;

//
// OffsetTree: a BTree mapping platform names to the block offsets for each
// platform's own BTree.  We keep the BTree at the "application block" (i.e.,
// the bootstrap block) of our block file.
//
typedef BTreeFile<std::string,BlkOffset> OffsetTree;


//
// The Source class
//
class Source
{
public:
    typedef std::string string;
//
// Source: Reconstruct the Source from file 'fname' if it exists, otherwise
// create 'fname'.  Either way, maintain the Source's state in 'fname'.
//
// AddPlatform: add a new platform
//
// RemovePlatform: delete an existing platform
//
// AddFile: add a new file to the list
//
// UpdateFile: update the given file, or add it to the list if new
//
// RemoveFile: delete an existing file from the list
//
// The methods Find, FindBefore, FindAfter, FindExact, First, Last, Prev,
// and Next all make use of the notion of the "current" position, leaving
// the current position referring to the DataFileCore being returned.  The
// Prev and Next methods explicitly work with respect to the current
// position.  A different current position is maintained for each platform.
// Also note that the AddFile, UpdateFile, and RemoveFile methods will alter
// the current position.
//
// FindBefore: If we have a file starting at or before 'zt', fill in 'dfc'
// with the latest such file and return non-zero.  Otherwise return zero
// and the contents returned in 'dfc' are undefined.  
//
// FindAfter: If we have a file beginning at or after 'zt', fill in 'dfc' with
// the earliest such file and return non-zero.  Otherwise return zero and
// the contents returned in 'dfc' are undefined.
//
// FindExact: If we have a file starting exactly at 'zt', fill in 'dfc' with
// the file, and return non-zero.  Otherwise return zero and the contents 
// returned in 'dfc' are undefined.
//
// First: return the earliest file for the given platform
//
// Last: return the latest file for the given platform
//
// Prev: return the DataFileCore previous to the current one.
//
// Next: return the DataFileCore after the current one.
//
// Name: return the source name
//
// RootDir: return the base directory string
//
// DataDir: return the data directory for a given platform
//
// ConfirmDataDir: return true iff the data directory for the given platform
// already exists or we succeed in creating it.
//
// SetPlatDir: set a directory overriding the default for the given platform.
// This directory may be a relative or an absolute path.
//
// NFiles: return the number of files we have for the given platform
//
// SetReadOnly: set the value of the read-only flag
//
// IsReadOnly: read only source?
//
// SetDirConst: set the value of the DirConst flag
//
// IsDirConst: return true iff directory and its files do not change
//
// SetFileConst: set the value of the FileConst flag
//
// IsFileConst: return true iff files may come and go, but the contents of
// the files remain constant
//
// SetRememberAll: set the value of the RememberAll flag
//
// RemembersAll: return true iff files should remain in the cache even if
// they disappear from the data directory
//
// SetForceDirs: set the value of the ForceDir flag
//
// DirsAreForced: return true iff platform directories should be created
// at scan time if not found
//
    Source( const char* in_srcname, const char* in_dir, 
	    const char* cachename );
    ~Source( void );
    bool AddPlatform( const Platform *p );
    bool RemovePlatform( const Platform *p );
    bool AddFile( const Platform *p, const DataFileCore& dfc );
    bool UpdateFile( const Platform *p, const DataFileCore& dfc );
    bool RemoveFile( const Platform *p, const DataFileCore& dfc );
    bool FindBefore( const Platform *p, const ZTime& zt, 
		    DataFileCore* dfc = 0 );
    bool FindAfter( const Platform *p, const ZTime& zt, 
		    DataFileCore* dfc = 0 );
    bool FindExact( const Platform *p, const ZTime& zt, 
		    DataFileCore* dfc = 0 );
    bool First( const Platform *p, DataFileCore* dfc = 0 );
    bool Last( const Platform *p, DataFileCore* dfc = 0 );
    bool Prev( const Platform *p, DataFileCore* dfc = 0 );
    bool Next( const Platform *p, DataFileCore* dfc = 0 );
    bool Current( const Platform *p, DataFileCore *dfc = 0, ZTime *t = 0 );
    const string& Name( void ) const;
    const string& RootDir( void ) const;
    const string& DataDir( const Platform *p );
    int ConfirmDataDir( const Platform *p );
    void SetPlatDir( const string& platname, const string& dir );
    int NFiles( const Platform *p );
    void SetReadOnly( bool readonly );
    bool IsReadOnly( void ) const;
    void SetDirConst( bool dconst );
    bool IsDirConst( void ) const;
    void SetFileConst( bool fconst );
    bool IsFileConst( void ) const;
    void SetRememberAll( bool rall );
    bool RemembersAll( void ) const;
    void SetForceDirs( bool force );
    bool DirsAreForced( void ) const;

private:
//
// Instance variables
//
// bfile: the BlockFile that contains our external representation, including 
// all of our platforms' individual file lists
//
// poffsets: a BTree mapping platform names to the block offsets for each
// platform's own BTree.  We keep the BTree at the "application block" (i.e.,
// the bootstrap block) of our block file.
//
// pflists: once we instantiate a platform's list of files, we keep a
// pointer to it in a local map, keyed by PlatformId.  We use a map here
// rather than a BTree, since the pflists info need not (and should not) be
// kept in our external representation.
//
// optplatdirs: directories for those platforms that don't use the default
//
// srcname: the user-assigned name for this source
//
// dir: the base directory under which all the data files live; the files
// may be in subdirectories, depending on platform specifics
//
// flags: source flags
//
    typedef std::map<PlatformId, PlatFileList*> PlatFilesMap;
    typedef std::map<string, string> PlatDirMap;
    
    BlockFile bfile;
    OffsetTree *poffsets;
    PlatFilesMap pflists;
    PlatDirMap optplatdirs;
    string srcname;
    string rootdir;
    int flags;
//
// Private methods
//
// GetPlatFileList: return a pointer to the PlatFileList for the given
// platform.  The list is created if it doesn't currently exist.
//
// SetFlag: set the state of one of the flag bits
//
    PlatFileList* GetPlatFileList( const Platform *p );
    void SetFlag( unsigned int flagbit, bool state );
//
// Values for the flags field:
//	DS_DirConst - true iff the source directory is to be considered 
//		immutable, i.e., we never need to verify the cache at startup
//	DS_FileConst - true iff files may come and go from the directory, but 
//		are assumed to remain unchanged if there, i.e., at startup
//		we may need to check for their existence, but not their
//		contents
//	DS_ReadOnly - true iff we should enforce read-only access
//	DS_RememberAll - true iff we do not automatically remove files from 
//		the cache if they disappear from the data directory, since 
//		they may be back at some point.  They can, of course, still 
//		be removed by more explicit means like the dsdelete program.
//	DS_ForceDirs - true iff we should force directory creation for all 
//		platforms when scanning this source's data directory
//
    static const unsigned int DS_DirConst =	0x01;
    static const unsigned int DS_FileConst =	0x02;
    static const unsigned int DS_ReadOnly =	0x04;
    static const unsigned int DS_RememberAll =	0x08;
    static const unsigned int DS_ForceDirs =	0x10;
};


inline const Source::string&
Source::Name( void ) const
{
    return srcname;
}


inline const Source::string&
Source::RootDir( void ) const
{
    return rootdir;
}


inline void
Source::SetReadOnly( bool readonly )
{
    SetFlag( DS_ReadOnly, readonly );
}


inline bool
Source::IsReadOnly( void ) const
{
    return( flags & DS_ReadOnly );
}

    
inline void
Source::SetDirConst( bool dconst )
{
    SetFlag( DS_DirConst, dconst );
}


inline bool
Source::IsDirConst( void ) const
{
    return( flags & DS_DirConst );
}

    
inline void
Source::SetFileConst( bool fconst )
{
    SetFlag( DS_FileConst, fconst );
}


inline bool
Source::IsFileConst( void ) const
{
    return( flags & DS_FileConst );
}
    

inline void
Source::SetRememberAll( bool fconst )
{
    SetFlag( DS_RememberAll, fconst );
}


inline bool
Source::RemembersAll( void ) const
{
    return( flags & DS_RememberAll );
}
    

inline void
Source::SetForceDirs( bool force )
{
    SetFlag( DS_ForceDirs, force );
}


inline bool
Source::DirsAreForced( void ) const
{
    return( flags & DS_ForceDirs );
}
    

inline void
Source::SetFlag( unsigned int flag, bool state )
//
// Set the state of one of the flag bits
//
{
    if (state)
	flags |= flag;
    else
	flags &= ~flag;
}


# endif // ndef _zebra_Source_hh_
