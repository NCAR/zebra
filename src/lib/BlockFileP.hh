/*
 * $Id: BlockFileP.hh,v 1.8 2002-09-17 20:00:19 granger Exp $
 *
 * Private classes and declarations for BlockFile implementation,
 * such as auxiliary block classes.
 */


#ifndef _BlockFileP_hh_
#define _BlockFileP_hh_

#include <iostream>			// For << operators

#include "BlockFile.hh"
#include "BlockObject.hh"
#include "Serialize.hh"

const unsigned long BLOCK_FILE_MAGIC = 0xb10cf11e;	/* blocfile */
const unsigned long BLOCK_FILE_VERSION = 0x00010000;	/* major/minor 1.0 */
const unsigned long HEADER_SIZE = 256;

/* =============================
 * Our header structure, the root of the block file administration
 * overhead.  It's a syncable translatable block which will always
 * force a read on readSync(), and it always reads and writes itself
 * from the beginning of the block file (offset 0).
 *
 * We get our encode, decode, and size implementations for SerialBlock
 * from Translatable.
 */
struct BlockFileHeader : virtual public TranslateBlock
{
	/* Basic information */

	unsigned long bf_magic;	/* Block file magic number */
	unsigned long app_magic;/* Application's magic number */
	int header_size;	/* Size of block file header */
	long bf_version;	/* BlockFile format's version number */
	long revision;		/* Revision number of this file */
	BlkOffset bf_length;	/* length of file (aka, pointer to eof) */
	Block app_header;	/* application's header block */

	/* Auxiliary blocks */

	Block freelist;		/* Free list block */
	Block journal;		/* Journal entries */

	// ---------------- Methods ---------------- //

	void translate (SerialStream &ss)
	{
		ss << bf_magic << app_magic << header_size;
		ss << bf_version << revision;
		ss << bf_length;
		ss << app_header;
		ss << freelist;
		ss << journal;
	}

	// ---------------- Constructor  ----------------

	BlockFileHeader (BlockFile &_bf, int magic = 0) : 
		// Fix our block at the beginning
		SyncBlock (_bf, Block (0, HEADER_SIZE)),
		// Initialize members
		bf_magic (BLOCK_FILE_MAGIC),
		app_magic (magic),
		header_size (HEADER_SIZE),
		bf_version (BLOCK_FILE_VERSION),
		revision (0),
		bf_length (HEADER_SIZE),
		app_header(),
		freelist(),
		journal()
	{ }

	// ---------------- Override SyncBlock methods ----------------

	// Never need allocation, and we'll never grow
	virtual void allocate (BlkSize) { }

	// Always read the header when asked
	int needsRead ()
	{
		return (1);
	}

	void updateRev ()
	{
		// Take note of a new revision in our block
		block.revision = revision;
	}

	void mark (int _marked = 1)
	{
		// The first time we are marked, advance our revision
		if (clean() && _marked)
			++revision;

		SyncBlock::mark (_marked);
	}

	/*
	 * Override write() in SerialBlock to use the BlockFile internal
	 * write, which avoids recording each header write in the journal.
	 */
	virtual void write ()
	{
		// Encode ourself onto a serial buffer from the block file.
		// Be careful to size and allocate before encoding, in
		// case the object changes (e.g., FreeList) when allocated.
		SerialBuffer *sbuf = bf->writeBuffer (block.length);
		unsigned long growth = encodedSize (*sbuf);
		sbuf->Need (growth);

		// Now make sure we have space, then write into it.
		allocate (growth);
		encode (*sbuf);
		bf->write (block.offset, sbuf);
	}		

};



#ifdef notdef
/*
 * A file "user", which might include info on both the particular 
 * application and the user running the application.  The mode might
 * be useful to know if some other user is only reading the file, so
 * that an exclusive writer knows it does not need to read sync the file,
 * but that it should write sync after every operation.
 */
class User // : public Translatable
{
public:
	int id;			/* Simple id for this user application */
	int mode;		/* Flag for things like read-only mode */
	char *user;		/* Name of this user */
	char *app;		/* Description or name of application */
	BF_Time begin;		/* When this user opened file */

	User (int aid, const char *name, BF_Time &when) :
		id(aid), mode(0), app(0)
	{
		this->user = strdup (name);
		this->begin = when;
	}

	// Resurrect ourself from a serial buffer
	User (SerialBuffer &sbuf)
	{
		user = NULL;
		app = NULL;
		translate (*sbuf.decodeStream ());
	}

	~User()
	{
		if (user) free (user);
		if (app) free (app);
	}
		
	void translate (SerialStream &ss)
	{
		ss << id << mode << begin << user << app;
	}
};

SERIAL_STREAMABLE(User);
#endif


#ifdef notdef
/*
 * Preface information for describing an allocated block.
 */
class BlockPrefix : public Translatable
{
public:
	int block_magic;	/* Mark this as a known block */
	int magic;		/* Simple indicator of type of block */
	BlkSize alloc;		/* Size allocated */

	void translate (SerialStream &ss)
	{
		ss << block_magic << magic << alloc;
	}
};
#endif


#ifdef notdef
/*
 * A Block registration entry, describing the blocks with the given magic id.
 */
struct BlockEntry // : public Translatable
{
	int magic;		/* Block magic type id */
	char *description;	/* Contents of this kind of block */

	void translate (SerialStream &ss)
	{
		ss << magic << description;
	}
};

SERIAL_STREAMABLE(BlockEntry);
#endif


#ifdef notdef
/* ===================
 * Block-level locking.
 */
enum BF_LockType
{
	LOCK_READ,
	LOCK_WRITE
};


struct BF_Lock
{
	int id;			/* ID of owner of lock */
	Block block;		/* Block which is locked */
	BF_LockType lock;
};
#endif


#endif /* _BlockFileP_hh_ */

