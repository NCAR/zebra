/*
 * The auxillary block base class from which BlockFile helper classes
 * can derive common functionality for serialization and syncing.
 *
 * $Id: AuxBlock.hh,v 1.2 1997-12-09 09:29:16 granger Exp $
 */

#ifndef _AuxBlock_hh_
#define _AuxBlock_hh_

#include "BlockFileP.hh"
#include "Serialize.hh"

/* On block behavior:

   When read from the file, it is clean (un-marked) until it changes its
   state, at which time it mark()s itself.  The next write sync will write
   the block to the file, update the revision, and clear the mark.

   On read syncs, if the revision in memory differs from that in the file,
   the block is read and the mark is cleared.

   When created but not yet allocated or written to the file, it starts
   out marked, and possibly without a block address or length.  A write sync
   may first allocate space in the file for the block before writing it.

   */

///
/** A base class which holds the location and size of a block and its
    revision info, simplifying the synchronization of objects 
    to and from disk.

    Auxillary blocks must be serializable, and thus subclasses must
    implement the necessary interface methods.
    */
class AuxBlock : public Serializable
{
public:
	/* ---
	 * This is the interface we need all auxillary blocks to support
	 * to simplify the BlockFile class.
	 */

	/// Associate a memory object with a block in the block file
	AuxBlock (BlockFile &bf, Block &block);

	/// 
	/** Write this block to disk if its changed or if 'force' non-zero
	    */
	void writeSync (int force = 0);

	/// 
	/** Read this block from the file if its revision has changed
	    on the disk. */
	void readSync ();

	/* ---
	 * This is the interface to the implementation which can be
	 * shared among auxillary block subclasses.
	 */

	/// 
	/** Set the mark on this block.  Use mark() to mark it as changed
	    (dirty), else mark(0) to reset the mark (clean).
	    */
	void mark (int marked = 1);

	/// Return non-zero if this block is marked.
	int dirty ();

	/// Return non-zero if this block is clean (not marked).
	int clean ();

	/// The virtual destructor
	virtual ~AuxBlock ();

	//---- Subclasses must provide the serialization methods, and
	//---- they may override the allocate() method to change their
	//---- growth behavior.

	// How much space to allocate for this block in the block file
	virtual BlkSize allocate (BlkSize need);

#ifdef notdef
	///
	/** Translate ourself onto a serial stream.  Subclasses override
	    this, but call this before doing their translation.
	    */
	virtual void translate (SerialStream &ss);
#endif

protected:

	BlockFile &bf;		// The block file we're associated with
	Block &block;		// The block we're associated with
	BlkVersion revision;	// Revision we last sync'ed with
	int marked;		// Whether we're dirty or not
};


inline void AuxBlock::mark (int _marked = 1)
{
	this->marked = _marked;
}


inline int AuxBlock::dirty ()
{
	return marked;
}


inline int AuxBlock::clean ()
{
	return (! marked);
}


/*
 * Free blocks are just length and offset, without a revision.
 */
class FreeBlock // : public Translatable
{
public:
	FreeBlock (BlkOffset addr = 0, BlkSize size = 0) :
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


//
// Package free list memory management into a convenient structure
//
class FreeList : public virtual AuxBlock
{
public:
	FreeList (BlockFile &bf, Block &b);
	~FreeList ();

	/*
	 * Add a freed block to the list.
	 */
	void Free (BlkOffset offset, BlkSize length);

	/*
	 * Find a block for this request and take it off the list.
	 * Return non-zero on success, zero otherwise.
	 */
	BlkOffset Request (BlkSize length, BlkSize *ret_length);

	// Serialization interface
	int encode (SerialBuffer &sbuf);
	int decode (SerialBuffer &sbuf);
	long size (SerialBuffer &sbuf);

	// Inherit the growth allocation method

private:
	int ncache;	/* Number of free blocks allocated space in array */
	int n;		/* Actual number of free blocks in use in array */
	FreeBlock *blocks; /* The actual array of free blocks */

	void Remove (int);
	void Add (BlkSize length, BlkSize length);
	void growCache (int num);

};



/* =================
 * Journal structure
 *
 * Describes and locates a change to the file and the new revision number
 * it resulted in.  Applications can use the journal to determine if an
 * in-memory cache of a block has changed and needs to be re-read.
 */


class JournalEntry;

class Journal : public virtual AuxBlock
{
public:
	static const MaxEntries = 256;

	typedef int ChangeType;

	static const ChangeType BeginTransaction = 0;
	static const ChangeType BlockRemoved = 1;
	static const ChangeType BlockAdded = 2;
	static const ChangeType BlockChanged = 3;
	static const ChangeType EndTransaction = 4;

	Journal (BlockFile &bf, Block &b);
	~Journal ();

	// Functionality
	int Changed (BlkVersion, BlkOffset, BlkSize);
	void Record (ChangeType, BlkOffset, BlkSize);

	// Serialization interface
	int encode (SerialBuffer &sbuf);
	int decode (SerialBuffer &sbuf);
	long size (SerialBuffer &sbuf);

	// We'll never actually grow, so allocate only what we need when asked
	virtual BlkSize allocate (BlkSize need)
	{
		return (need);
	}

private:
	int serialSize;		// Fixed, so calculated once
	int max;		// Size of queue
	int first;		// Beginning of ciruclar queue
	int last;		// End of circular queue
	JournalEntry *entries;
};


struct JournalEntry // : public Translatable
{
	Journal::ChangeType change;
	Block block;			/* Region changed */

	void translate (SerialStream &ss)
	{
		ss << change << block;
	}

	JournalEntry () : change(0), block()
	{ }
};

SERIAL_STREAMABLE(JournalEntry);



#endif /* _AuxBlock_hh_ */


