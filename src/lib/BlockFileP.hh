/*
 * $Id: BlockFileP.hh,v 1.2 1997-11-24 18:15:10 granger Exp $
 *
 * Private classes and declarations for BlockFile implementation,
 * such as auxillary block classes.
 */


#ifndef _BlockFileP_hh_
#define _BlockFileP_hh_

#include <iostream.h>			// For << operators

#include "Block.h"
#include "Serialize.hh"
#include "ZTime.hh"

const unsigned long BLOCK_FILE_MAGIC = 0xb10cf11e;	/* blocfile */
const unsigned long BLOCK_FILE_VERSION = 0x00010000;	/* major/minor 1.0 */

typedef ZebraTime BF_Time;

SERIAL_XDR_OPERATOR(BF_Time, xdr_ZebraTime);


/* ================
 * A block.  The header uses this structure to point to blocks of 
 * auxiliary information.  The revision indicates whether the block
 * needs to be re-read from disk.
 */
class Block // : public Translatable
{
public:
	Block (BlkOffset bo = 0, BlkSize bs = 0, 
	       BlkVersion rev = 0) :
		offset(bo), length(bs), revision(rev)
	{ }

	BlkOffset offset;	/* Location of block */
	BlkSize length;		/* Length of block (bytes) */
	BlkVersion revision;	/* Revision block last changed */

	inline void translate (SerialStream &ss)
	{
		ss << offset << length << revision;
	}

};

SERIAL_STREAMABLE(Block);

inline ostream &
operator<< (ostream &o, const Block &b)
{
	o << "(" << b.offset << ", " << b.length << ", "
	  << ", " << b.revision << ")" << endl;
	return o;
}


/* =============================
 * Our header structure, the root of the block file structure
 */
struct BlockFileHeader : public Translatable
{
	/* Basic information */

	unsigned long bf_magic;	/* Block file magic number */
	unsigned long app_magic;/* Application's magic number */
	int header_size;	/* Size of block file header */
	// char description[256];	/* application's file description */
	BlkVersion app_version;	/* application's format version */
	BlkVersion bf_version;	/* BlockFile format's version number */
	BlkVersion revision;	/* Revision number of this file */
	BlkOffset bf_length;	/* length of file (aka, pointer to eof) */
	Block app_header;	/* application's header block */

	/* Auxiliary blocks */

	Block freelist;		/* Free list block */
#ifdef notdef
	BF_Block userlist;	/* List of concurrent users of this file */
	BF_Block blocklist;	/* offset to allocated block list (if used) */
	BF_Block locks;		/* offset to block-level lock block (:-) */
	BF_Block journal;	/* offset to journal block */
	BF_Block toc;		/* "table of contents" for block magic id's */
#endif

#ifdef notdef
	/* Statistics and debugging */
	int requested;		/* Bytes granted - bytes requested = unused */

	int granted;
	int spacefree;		/* Space held in free blocks */
#endif

	void translate (SerialStream &ss)
	{
		ss << bf_magic << app_magic << header_size;
		// ss.opaque (description, sizeof(description));
		ss << app_version << bf_version << revision;
		ss << bf_length;
		ss << app_header;

		ss << freelist;
	}
};



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



/*
 * Free blocks are just length and offset, without a revision.
 */
class FreeBlock // : public Translatable
{
public:
	FreeBlock (BlkOffset addr, BlkSize size) :
		offset(addr), length(size)
	{ }

	BlkOffset offset;	/* Location of block */
	BlkSize length;		/* Length of block */

	void translate (SerialStream &ss)
	{
		ss << offset << length;
	}
};

SERIAL_STREAMABLE(FreeBlock);


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


/* =================
 * Journal structure
 *
 * Describes and locates a change to the file and the new revision number
 * it resulted in.  Applications can use the journal to determine if an
 * in-memory copy of a block has changed and needs to be re-read.
 */

class Journal
{
	enum ChangeType
	{
		BeginTransaction,
		BlockRemoved,
		BlockAdded,
		BlockChanged,
		EndTransaction
	};
};


struct JournalEntry : public Translatable
{
	int change;		/* Type of change */
	BF_Time when;		/* Time of change */
	int id;			/* ID of application making change */
	int flags;		/* Other flags */
	Block block;		/* Region changed -- contains the revision */

	void translate (SerialStream &ss)
	{
		ss << change << when << id << flags << block;
	}
};


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




#endif /* _BlockFileP_hh_ */

