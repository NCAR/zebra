/*
 * $Id: BlockFileP.hh,v 1.3 1997-12-09 09:29:21 granger Exp $
 *
 * Private classes and declarations for BlockFile implementation,
 * such as auxiliary block classes.
 */


#ifndef _BlockFileP_hh_
#define _BlockFileP_hh_

#include <iostream.h>			// For << operators

#include "BlockFile.hh"
#include "Serialize.hh"

const unsigned long BLOCK_FILE_MAGIC = 0xb10cf11e;	/* blocfile */
const unsigned long BLOCK_FILE_VERSION = 0x00010000;	/* major/minor 1.0 */

#ifdef notdef
#include "ZTime.hh"
typedef ZebraTime BF_Time;
SERIAL_XDR_OPERATOR(BF_Time, xdr_ZebraTime);
#endif


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

	void init ()
	{
		offset = 0;
		length = 0;
		revision = 0;
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
	long bf_version;	/* BlockFile format's version number */
	long revision;		/* Revision number of this file */
	BlkOffset bf_length;	/* length of file (aka, pointer to eof) */
	Block app_header;	/* application's header block */

	/* Auxiliary blocks */

	Block freelist;		/* Free list block */
	Block journal;		/* Journal entries */

	int dirty;		/* Dirty status */
	BlockFile &bf;		/* Our block file */

	BlockFileHeader (BlockFile &_bf) : bf(_bf)
	{ }

	void mark () 
	{
		dirty = 1;
	}

	int clean ()
	{
		return (! dirty);
	}

	long Revision ()
	{
		return (revision);
	}

	void translate (SerialStream &ss)
	{
		ss << bf_magic << app_magic << header_size;
		ss << bf_version << revision;
		ss << bf_length;
		ss << app_header;
		ss << freelist;
		ss << journal;
	}

	void init (int magic = 0)
	{
		bf_magic = BLOCK_FILE_MAGIC;
		app_magic = magic;
		header_size = 256;	// Room for future header versions
		bf_version = BLOCK_FILE_VERSION;
		revision = 0;
		bf_length = header_size;
		app_header.init();
		freelist.init();
		journal.init();

		mark ();
	}

	void writeSync (int force = 0)
	{
		if (dirty || force)
		{
			++revision;
			SerialBuffer *sbuf = bf.writeBuffer ();
			*sbuf << *this;
			bf.write (0, sbuf);
			dirty = 0;
		}
	}

	// Always read the header; there is no check for revision sync
	void readSync ()
	{
		SerialBuffer *sbuf = bf.readBuffer (0, header_size);
		*sbuf >> *this;
		dirty = 0;
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

