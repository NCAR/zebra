/* $Id: BlockObject.hh,v 1.1 1997-12-13 00:24:30 granger Exp $
 *
 * A set of classes to facilitate object persistence with a BlockFile.
 */

#ifndef _BlockObject_hh_
#define _BlockObject_hh_

#include <iostream.h>

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

	inline void translate (SerialStream &ss)
	{
		ss << offset << length << revision;
	}

	BlkOffset offset;	/* Location of block */
	BlkSize length;		/* Length of block (bytes) */
	BlkVersion revision;	/* Revision block last changed */
};


SERIAL_STREAMABLE(Block);

inline ostream &
operator<< (ostream &o, const Block &b)
{
	o << "(" << b.offset << ", " << b.length << ", "
	  << ", " << b.revision << ")" << endl;
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

	// Write ourself to our block
	virtual void write () = 0;

	// Read ourself to our block
	virtual void read () = 0;

	// How much space to allocate for this block in the block file
	virtual void allocate (BlkSize need);

	virtual BlkSize grow (BlkSize needed);

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

	BlockFile &bf;		// The block file we're associated with
	Block block;		// Our block in the block file
	int marked;		// Whether we're dirty or not
	int changed;		// Changed in blockfile (needs read)

private:
	SyncBlock () {}

	// Not implemented //
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
		ref(_ref), parent(_parent)
	{
		cout << "RefBlock constructor" << endl;
	}

	// Override the needsRead and updateRev methods

	/*
	 * We need to re-read our block if the reference to us has
	 * a greater revision, and we also have to update our own
	 * block information with the changes in the reference.
	 */
	virtual int needsRead ()
	{
		bool sync = (block.revision < ref.revision);
		if (sync)
			block = ref;
		return (sync);
	}

	/*
	 * After reading the block from disk, our block info 
	 * should have already been synced with our reference.  
	 */
	virtual void updateRev ()
	{ }

	/*
	 * Lastly, when we've changed we need to pass on the change to
	 * the parent reference.
	 */
	virtual void writeSync (int force = 0)
	{
		if (needsWrite (force))
		{
			write ();
			ref = block;
			if (parent)
				parent->mark();
		}
		mark (0);
	}

	virtual ~RefBlock () 
	{ }

protected:
	Block &ref;
	SyncBlock *parent;

private:
	RefBlock () {}

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
		cout << "SerialBlock constructor" << endl;
	}

	virtual void write ()
	{
		// Encode ourself onto a serial buffer from the block file
		SerialBuffer *sbuf = bf.writeBuffer (block.length);
		encode (*sbuf);
		unsigned long growth = sbuf->Position();

		// Now make sure we have space, then write into it
		allocate (growth);
		bf.Write (block.offset, sbuf);
	}		

	virtual void read ()
	{
		// Read a serial buffer from the block file for our block,
		// then decode ourselves from it
		SerialBuffer *sbuf;
		sbuf = bf.readBuffer (block.offset, block.length);
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
		cout << "TranslateBlock constructor" << endl;
	}

	int encode (SerialBuffer &buf);
	int decode (SerialBuffer &buf);
	long size (SerialBuffer &buf);

	virtual void translate (SerialStream &ss) = 0;

	virtual ~TranslateBlock ()
	{ }

private:
	// Not implemented //
	TranslateBlock (const TranslateBlock &);
	TranslateBlock &operator= (const TranslateBlock &);
};


#endif /* _BlockObject_hh_ */

