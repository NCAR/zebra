/*
 * $Id: BlockFile.hh,v 1.1 1997-11-24 10:43:20 granger Exp $
 *
 * Definition of the BlockFile class, for storing opaque blocks of bytes
 * into a file through a block interface.  The overhead information in the
 * file is machine-independent.
 */

#ifndef _BlockFile_hh_
#define _BlockFile_hh_

#include <stdlib.h>
#include <iostream.h>		// to add stream operator to dump header

#include "BlockStore.hh"
#include "Block.h"

class Logger;
class FreeList;
class BlockFileHeader;

// The machine-independent block file interface
//
class BlockFile : public BlockStore
{
public:
	// File operation flags
	enum BF_Flags
	{
		BF_NONE = 0,
		BF_CREATE = 1,		// New file, clobber any existing
		BF_READONLY = 2,	// Just what it says, else r/w
		BF_EXCLUSIVE = 4	// Non-shared open, else shared
	};

	// "Status symbols"
	enum BF_Result
	{
		OK = 0,
		NOT_OPEN = 1,
		COULD_NOT_OPEN = 2,
		NO_SUCH_BLOCK = 3,
		BLOCK_TOO_BIG = 4,
		OUT_OF_SYNC = 5,
		WRITE_FAILED = 6,
		READ_FAILED = 7,
		WRONG_BLOCK_MAGIC = 8,
		WRONG_APP_MAGIC = 9,
	};
	
	// Constructors

	BlockFile ();
	BlockFile (const char *path, int app_magic = 0, int flags = 0);

	// Destructors

	~BlockFile ();

	// The BlockStore interface which we inherit and implement

	BlkOffset Alloc (BlkSize size);

	// BlkOffset Realloc (BlkOffset addr, BlkSize size);
	void Free (BlkOffset addr);

	void *Read (void *buf, BlkOffset block, BlkSize len);
	int Write (BlkOffset block, void *buf, BlkSize len);

	// BlockFile extensions to BlockStore interface

	BlkOffset Alloc (BlkSize size, BlkSize *actual);

	// BlockFile-specific administrative interface

	int Status ();
	int Errno ();

	int Open (const char *path, int app_magic = 0, int flags = 0);
	int Create (const char *path, int app_magic = 0, int flags = 0);
	int Close ();

	BlkVersion AppVersion ();
	void SetAppVersion (BlkVersion version, const char *description);
	ostream& DumpHeader (ostream& out) const;

	// File sharing facilities

	void Lock ();		// Lock entire file
	void Unlock ();		// Unlock entire file

	void ReadSync ();	// Read sync from file
	void WriteSync ();	// Write sync to file

private:

	// In-memory structures

	BlockFileHeader *header;
	FreeList *freelist;

	int errno;	// Error result from last operation (or construction)
	int status;	// Current status number
	FILE *fp;	// File pointer of open file (NULL if not open)
	char *path;	// Path name of current file
	Logger *log;

	/* statistics and debugging */
	struct
	{
		int nread;
		int bread;
		int nwrite;
		int bwrite;
		int nrequest;
		int brequest;
		int nfreed;
		int bfreed;
	} stats;

	/*
	 * Private methods
	 */
	void seek (BlkOffset offset);
	long seek_end (void);
	int write (void *, long size);
	int read (void *, long size);

	void init ();

	BlkOffset AppendBlock (BlkSize size, BlkSize *actual = NULL);
};


inline int
BlockFile::Status ()
{ 
	return (status);
}


inline int
BlockFile::Errno ()
{ 
	return (this->errno);
}


inline BlkOffset
BlockFile::Alloc (BlkSize size)
{
	return (Alloc (size, NULL));
}



inline ostream&
operator<< (ostream& out, const BlockFile& bf)
{
	return (bf.DumpHeader(out));
}


#ifdef notdef
	void WriteHeader ();
	void ReadHeader ();
#endif

#ifdef notdef
	// Bit-field flags for regions of file kept in memory which need
	// to be sync'ed to disk

	enum BF_Region
	{
		HEADER = 1,
		FREELIST = 2,
	};

	void mark (enum BF_Region r)		// Mark region as changed
	{
		std |= (int)r;
	}
	int changed (enum BF_Region r)		// Check if region needs write
	{
		return (std & (int)r);
	}
#endif

#ifdef notdef
	// Stream positioning

	XDRStream *encodeStream (BlkOffset addr, BlkSize len = 0)
	{
		encode->setpos (addr);
		return (encode);
	}

	XDRStream *decodeStream (BlkOffset addr, BlkSize len = 0)
	{
		decode->setpos (addr);
		return (decode);
	}

	/* stdio XDR streams */
	WriteXDR *encode;
	ReadXDR *decode;

#endif


#endif /* _BlockFile_hh_ */

