/* $Id: BlockObject.hh,v 1.11 2002-09-17 20:00:19 granger Exp $
 *
 * A set of classes to facilitate object persistence with a BlockFile.
 */

#ifndef _BlockObject_hh_
#define _BlockObject_hh_

#include <iostream>

#include "BlockFile.hh"
#include "Serialize.hh"

/* ================
 * A convenient structure for describing a block.
 */
class Block
{
public:
	// Default constructor
	Block (BlkOffset bo = 0, BlkSize bs = 0, 
	       BlkVersion rev = 0) :
		offset(bo), length(bs), revision(rev)
	{ }

	// Copy constructor
	Block (const Block &block) :
		offset(block.offset), 
		length(block.length), 
		revision(block.revision)
	{ }

	// Assignment operator
	Block &operator= (const Block &block)
	{
		// self-assignment ok
		offset = block.offset;
		length = block.length;
		revision = block.revision;
		return *this;
	}

	inline void translate (SerialStream &ss)
	{
		ss << offset << length << revision;
	}

	BlkOffset offset;	/* Location of block */
	BlkSize length;		/* Length of block (bytes) */
	BlkVersion revision;	/* Revision block last changed */
};


SERIAL_STREAMABLE(Block);

inline std::ostream &
operator<< (std::ostream &o, const Block &b)
{
	o << "(" << b.offset << ", " << b.length << ", "
	  << ", " << b.revision << ")" << std::endl;
	return o;
}


// A basic sync-able block, meaning the object keeps track of its block
// address, length, and revision in the block file, and the last revision
// to which it was sync'ed.

class SyncBlock
{
public:
	/// Associate an object with a block file, either with or without
	/// an existing block.
	SyncBlock (BlockFile &_bf);
	SyncBlock (BlockFile &_bf, const Block &exist);

	///
	/** Attach a new block to this object */
	virtual void attach (const Block &exist);

	void attach (const Block &exist, BlockFile &_bf);

	/// 
	/** Write this block to disk if its changed or if 'force' non-zero
	    */
	virtual void writeSync (int force = 0);

	/// 
	/** Read this block from the file if its revision has changed
	    on the disk. */
	virtual void readSync ();

	virtual int needsRead ();

	virtual int needsWrite (int force);

	virtual void updateRev ();

	// Indicate the block size, location, or revision have changed
	virtual void blockChanged () { }

	// Write ourself to our block
	virtual void write () = 0;

	// Read ourself to our block
	virtual void read () = 0;

	// How much space to allocate for this block in the block file
	virtual void allocate (BlkSize need);

	// Free this block's space in the block file
	virtual void free ();

	virtual BlkSize grow (BlkSize needed);

	// Read lock this block, which also syncs it
	virtual void readLock ();

	// Write lock this block, which syncs it and
	// causes a write sync when unlocked
	virtual void writeLock ();

	// Unlock this block
	virtual void unlock ();

	/// 
	/** Set the mark on this block.  Use mark() to mark it as changed
	    (dirty), else mark(0) to reset the mark (clean).
	    */
	virtual void mark (int marked = 1);

	/// Return non-zero if this block is marked.
	inline int dirty ();

	/// Return non-zero if this block is clean (not marked).
	inline int clean ();

	/// The virtual destructor
	virtual ~SyncBlock ();

protected:

	BlockFile *bf;		// The block file we're associated with
	Block block;		// Our block in the block file
	int marked;		// Whether we're dirty or not
	int changed;		// Changed in blockfile (needs read)
	int lock;		// Lock count
	int writelock;		// writeSync pending

	SyncBlock () : bf(0), block(), marked(0), changed(0)
	{ }

private:

	SyncBlock (const SyncBlock &);
	SyncBlock &operator= (const SyncBlock &);
};



inline int SyncBlock::dirty ()
{
	return marked;
}


inline int SyncBlock::clean ()
{
	return (! marked);
}



/*
 * A RefBlock uses a block reference somewhere else to determine whether
 * it needs to be read.  Changes to the block reference (i.e. the block's
 * revision, offset, or length) are indicated to the referring block by
 * calling its mark() method.  This class assumes the parent block is
 * in read sync before accessing this object, since the object relies on
 * the revision in the block reference to know if it has changed on disk.
 */

class RefBlock : virtual public SyncBlock
{
public:
	RefBlock (Block &_ref, SyncBlock *_parent = 0) : 
		ref(&_ref), parent(_parent)
	{
		// cout << "RefBlock constructor" << endl;
	}

	// Override methods to use our own revision count, and to detect
	// changes between our reference revision and our current revision.

	/*
	 * Increment our revision number the first time we are marked.
	 */
	virtual void mark (int _marked = 1)
	{
		if (_marked && clean())
		{
			++block.revision;
			blockChanged ();
		}
		SyncBlock::mark (_marked);
	}

	/*
	 * We need to re-read our block if the reference to us has
	 * a greater revision, and we also have to update our own
	 * block information with the changes in the reference.
	 */
	virtual int needsRead ()
	{
		changed |= (block.revision < ref->revision);
		// Copy the current reference before reading 
		// in case the location or length on disk has changed
		if (changed)
			block = *ref;
		return (changed);
	}

	/*
	 * Update our revision with the one in our reference.
	 */
	virtual void updateRev ()
	{
		// block.revision = ref->revision;
	}

	/*
	 * Lastly, when we've changed we need to pass on the change to
	 * the parent reference.
	 */
	virtual void blockChanged ()
	{
		// Our revision, and possibly location,
		// have changed.  Update the reference.
		*ref = block;
		if (parent)
			parent->mark();
	}

	virtual ~RefBlock () 
	{ }

protected:

	Block *ref;
	SyncBlock *parent;

	RefBlock () : ref(0), parent(0)
	{ }

private:
	// Not implemented //
	RefBlock (const RefBlock &);
	RefBlock &operator= (const RefBlock &);
};



/*
 * A SerialBlock is a serializable block.  The subclass provides 
 * the implementation of the serialization interface, and this class
 * provides read() and write() methods which use that interface.
 */

class SerialBlock : virtual public SyncBlock, virtual public Serializable
{
public:
	SerialBlock ()
	{
		// cout << "SerialBlock constructor" << endl;
	}

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
		bf->Write (block.offset, sbuf);
	}		

	virtual void read ()
	{
		// Read a serial buffer from the block file for our block,
		// then decode ourselves from it.
		SerialBuffer *sbuf;
		sbuf = bf->readBuffer (block.offset, block.length);

		// As a special case, if we only know our offset because the
		// length is zero, use our encodedSize() to gauge our buffer
		// size.  Hence fixed-length objects can avoid needing to
		// explicitly setting their length.
		if (block.length == 0)
		{
			block.length = encodedSize (*sbuf);
			sbuf = bf->readBuffer (block.offset, block.length);
		}
		decode (*sbuf);
	}

	virtual ~SerialBlock ()
	{ }

private:
	// Not implemented //
	SerialBlock (const SerialBlock &);
	SerialBlock &operator= (const SerialBlock &);
};



/*
 * Implement the serializable interface with a single translate method.
 */
class TranslateBlock : virtual public SerialBlock
{
public:
	TranslateBlock ()
	{
		// cout << "TranslateBlock constructor" << endl;
	}

	int encode (SerialBuffer &buf);
	int decode (SerialBuffer &buf);
	long encodedSize (SerialBuffer &buf);

	// Subclasses should override this method, but we can be translated
	// to a SerialStream also, in which case we keep permanent track of
	// our allocated length so the subclass does not need to.  Just
	// call this method from the subclass translate() implementation.
	//
	virtual void translate (SerialStream &ss)
	{
		ss << block.length;
	}

	virtual ~TranslateBlock ()
	{ }

private:
	// Not implemented //
	TranslateBlock (const TranslateBlock &);
	TranslateBlock &operator= (const TranslateBlock &);
};



/*
 * Provide an object which simply keeps a reference to a Serializable
 * object.  The "client" object calls the readLock(), writeLock(), 
 * and unlock() methods, and then the Bridge calls the client's 
 * encode and decode as needed.  This is essentially an implementation
 * of object inheritance, for cases where actually subclassing SerialBlock
 * is just too complicated or otherwise undesirable.
 */
class SerialBlockBridge : virtual public SerialBlock
{
public:
	SerialBlockBridge (Serializable &_target, BlockFile &_bf)
		: SyncBlock (_bf), target (_target)
	{ }
	SerialBlockBridge (Serializable &_target, BlockFile &_bf,
			   const Block &exist)
		: SyncBlock (_bf, exist), target (_target)
	{ }

	~SerialBlockBridge () { }

	virtual int encode (SerialBuffer &buf)
	{
		return target.encode (buf);
	}

	virtual int decode (SerialBuffer &buf)
	{
		return target.decode (buf);
	}

	virtual long encodedSize (SerialBuffer &buf)
	{
		return target.encodedSize (buf);
	}

protected:
	Serializable &target;
};


#endif /* _BlockObject_hh_ */

