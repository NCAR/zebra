/*
 * The auxillary block base class from which BlockFile helper classes
 * can derive common functionality for serialization and syncing.
 *
 * $Id: AuxBlock.hh,v 1.1 1997-11-24 10:43:17 granger Exp $
 */

#ifndef _AuxBlock_hh_
#define _AuxBlock_hh_

#include "Block.h"
#include "Serialize.hh"

class BlockFile;

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

	/// Construct an object from a block file
	/** The constructor calls our decode() method, which the subclass
	    has provided to deserialize itself.
	    */
	AuxBlock (BlockFile &bf, BlkOffset where, BlkSize howbig);

	///
	/** Create an auxillary block from nothing: just a block file
	    without any allocation in the block file. */
	AuxBlock (BlockFile &bf);

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
	int marked ();
	int dirty ();

	/// Return non-zero if this block is clean (not marked).
	int clean ();

	// Allocate space for this block in the block file
	void allocate ();

	/// Return length to grow to given a needed amount of space
	/** Growth function for allocating growing room over and
	    above the stored size of the block, to limit re-allocations
	    in the block file. */
	virtual BlkSize grow (BlkSize needed);

	/// The virtual destructor
	~AuxBlock ();

	///
	/** Translate ourself onto a serial stream.  Subclasses override
	    this, but call this before doing their translation.
	    */
	virtual void translate (SerialStream &ss);

protected:
	// --- Stuff only the class and its subclasses need to know ---

	BlockFile *bf;		// The block file we're associated with

	BlkOffset block;	// Address of the block, zero if none yet
	BlkSize length;		// Length of block, zero if no block

	long revision;		// Revision of our block
	long magic;		// The magic identifer, set by subclass

	int id;			// "type" of this auxillary block info

	int _marked;

private:
	// Private initializer
	auxBlock ();
};


inline void AuxBlock::mark (int marked = 1)
{
	this->_marked = marked;
}


inline int AuxBlock::marked ()
{
	return _marked;
}


inline int AuxBlock::dirty ()
{
	return marked();
}


inline int AuxBlock::clean ()
{
	return (! _marked);
}


#endif /* _AuxBlock_hh_ */


