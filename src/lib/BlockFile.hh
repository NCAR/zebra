/*
 * $Id: BlockFile.hh,v 1.3 1997-12-09 09:29:20 granger Exp $
 *
 * Definition of the BlockFile class, for storing opaque blocks of bytes
 * into a file through a block interface.  The overhead information in the
 * file is machine-independent.
 */

#ifndef _BlockFile_hh_
#define _BlockFile_hh_

#include <stdlib.h>
#include <iostream.h>		// to add stream operator to dump header

/* The types of our auxillary blocks, 
 * also an index into revision numbers in BlockFile objects. */

enum BlockInfo
{ 
	FREELIST = 0,
	JOURNAL = 1, 
	USERLIST = 2, 
	REGISTRY = 3
};

typedef unsigned long BlkOffset;
typedef unsigned long BlkSize;
typedef unsigned long BlkVersion;

#define BadBlockAddr (0)
#define BAD_BLOCK_ADDR BadBlockAddr
#define BadBlock BadBlockAddr

class Logger;
class FreeList;
class Journal;
class BlockFileHeader;
class AuxBlock;
class SerialBuffer;

// The machine-independent block file interface
//
class BlockFile
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

	// Destructor

	~BlockFile ();

	// ----- Common public interface -----

	BlkOffset Alloc (BlkSize size, BlkSize *actual);
	void Free (BlkOffset addr, BlkSize len);

	void *Read (void *buf, BlkOffset block, BlkSize len);
	int Write (BlkOffset block, void *buf, BlkSize len);

	// Return non-zero if this region has changed since rev
	int Changed (BlkVersion rev, BlkOffset block, BlkSize len);

	// Return the current revision of the file
	BlkVersion Revision ();

	int Status ();
	int Errno ();

	int Open (const char *path, unsigned long app_magic = 0, 
		  int flags = 0);
	int Create (const char *path, unsigned long app_magic = 0, 
		    int flags = 0);
	int Close ();

	// ----- Buffers -----

	// Reads the block into the serial buffer and returns the buffer
	SerialBuffer *readBuffer (BlkOffset addr, BlkSize length);

	// Returns a serial buffer with enough space for the given length
	SerialBuffer *writeBuffer (BlkSize length = 0);

	// Write the current contents of the serial buffer to an offset
	int Write (BlkOffset addr, SerialBuffer *sbuf);

	// ----- File sharing -----

	void Lock ();			// Lock entire file
	void Unlock ();			// Unlock entire file

	void ReadSync ();		// Read sync from file
	void WriteSync (int force = 0);	// Write sync to file

	// Debugging

	ostream& DumpHeader (ostream& out);

private:

	Logger *log;

	// In-memory structures

	BlockFileHeader *header;
	FreeList *freelist;
	Journal *journal;
	SerialBuffer *sbuf;

	friend FreeList;
	friend Journal;
	friend AuxBlock;
	friend BlockFileHeader;

	int errno;	// Error result from last operation (or construction)
	int status;	// Current status number
	FILE *fp;	// File pointer of open file (NULL if not open)
	char *path;	// Path name of current file
	int lock;	// Lock count

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
	int read (void *buf, BlkOffset block, BlkSize len);
	int write (BlkOffset block, void *buf, BlkSize len);
	int write (BlkOffset addr, SerialBuffer *sbuf);
	BlkOffset append (BlkSize size);
	void recover (BlkOffset addr);
	BlkOffset alloc (BlkSize size, BlkSize *actual);
	void free (BlkOffset addr, BlkSize len);

	void init ();
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



inline ostream&
operator<< (ostream& out, BlockFile& bf)
{
	return (bf.DumpHeader(out));
}


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

