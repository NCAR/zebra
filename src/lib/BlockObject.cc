/*
 * Implementation of the auxillary block methods.
 */

//#include <std.h>
//#include <defs.h>
//#undef bool
//
//RCSID("$Id: BlockObject.cc,v 1.8 1998-06-05 19:35:24 granger Exp $")

#include "BlockFile.hh"
#include "BlockObject.hh"
#include "Format.hh"

/*
 * Construction and attachment puts us in one of our two initial states:
 * a new, unallocated block (block.offset == 0), or an allocated block
 * which has not yet been read into our object (block.offset > 0 && changed).
 */

SyncBlock::SyncBlock (BlockFile &_bf, const Block &exist) :
	bf(&_bf)
{
	// cout << "SyncBlock constructor" << endl;
	attach (exist);
#ifdef notdef
	block.revision = 0;	// we have no revision in memory yet
	marked = 0;
	changed = 0;
	lock = 0;
	writelock = 0;
	// If the given block has not been allocated, we're dirty
	// because we don't yet exist in the file, and we don't need to
	// be read-synced so our revision is set to the file rev.
	if (block.offset == 0 && block.length == 0)
	{
		mark ();
		block.revision = bf->Revision();
	}
#endif
}


SyncBlock::SyncBlock (BlockFile &_bf) :
	bf(&_bf)
{
	// cout << "SyncBlock constructor" << endl;
	attach (Block());
}


/*
 * This method puts us in one of our initial states, either from
 * a constructor or by an explicit call with a new block.
 */
void
SyncBlock::attach (const Block &exist)
{
	marked = 0;
	changed = 0;
	lock = 0;
	writelock = 0;

	// "exist" may be a null, unallocated block.
	block = exist;

	// Base class method does nothing, as when called from constructor.
	// Only calls virtual subclass implementations when calling
	// attach() on an existing SyncBlock object.
	blockChanged ();

	// Existing blocks need to be read on next readSync
	if (block.offset > 0)
		changed = 1;
}




void
SyncBlock::attach (const Block &exist, BlockFile &_bf)
{
	bf = &_bf;
	attach (exist);
}



void
SyncBlock::mark (int _marked)
{
#ifdef notdef
	// Increment our revision number on the first change
	if (clean() && _marked)
		++block.revision;
#endif
	this->marked = _marked;
}




/*
 * When we write our changed object, the block file's revision advances,
 * then we bring our block revision up to date with the file revision. 
 */
void 
SyncBlock::writeSync (int force)
{
	if (needsWrite (force))
	{
		// cout << Format("SyncBlock: writing block (%u,%u,%u)\n") %
		//	block.offset % block.length % block.revision;
		write ();
		updateRev ();
		changed = 0;
		mark (0);
	}
}



void
SyncBlock::readSync ()
{
	if (clean() && needsRead ())
	{
		// cout << Format("SyncBlock: reading block (%u,%u,%u)\n") %
		//	block.offset % block.length % block.revision;
		read ();
		changed = 0;
		mark (0);
	}
	updateRev ();
}



int
SyncBlock::needsRead ()
{
	// We do not need to be read from disk when
	//  1) We have no location, meaning we are a new and unallocated block
	//  2) Our block revision equals the current file revision
	//  3) We are marked as dirty, meaning we are pending a write sync

	// Otherwise, we need to be read when
	//  1) We have a location and a zero revision, meaning we have
	//     been newly attached to an existing block and not read yet.
	//  2) We have a block and a revision, but we're out of rev.

	if (block.offset > 0 && block.revision < bf->Revision())
	{
		changed = changed ||
		   bf->Changed (block.revision, block.offset, block.length);
	}
	return (changed);
}



int
SyncBlock::needsWrite (int force)
{
	return (! clean() || force);
}



void 
SyncBlock::updateRev ()
{
	block.revision = bf->Revision ();
}



void
SyncBlock::allocate (BlkSize need)
{
	if (need > block.length)
	{
		if (block.offset)
			bf->Free (block.offset, block.length);
		need = grow (need);
		//
		// If the object does not want its block to grow any larger,
		// then don't reallocate the block.
		//
		if (need > block.length)
		{
			block.offset = bf->Alloc (need, &block.length);

			// After allocating, we will never want to trigger
			// a readsync.  Likewise we need to be written to
			// the new block, so get dirty.
			changed = 0;
			mark ();
			blockChanged ();
		}
	}
}



void
SyncBlock::free ()
{
	if (block.offset)
	{
		bf->Free (block.offset, block.length);
	}
	attach (Block());
}



BlkSize
SyncBlock::grow (BlkSize needed)
{
	// Default growth algorithm, round to the nearest 256 bytes, then
	// add on half as many 256-byte blocks.
	long npages = (needed >> 8) + 1;
	npages += (npages >> 1);
	return (npages << 8);
}



void
SyncBlock::readLock ()
{
	if (lock++ == 0)
	{
		bf->ReadLock ();
		readSync ();
	}
}



void
SyncBlock::writeLock ()
{
	if (lock++ == 0)
	{
		bf->WriteLock ();
		readSync ();
	}
	writelock = 1;
}



void
SyncBlock::unlock ()
{
	if (lock == 1)
	{
		if (writelock)
			writeSync ();
		writelock = 0;
		bf->Unlock ();
	}
	--lock;
}




SyncBlock::~SyncBlock ()
{ }




int
TranslateBlock::encode (SerialBuffer &buf)
{
	buf.Need (this->encodedSize (buf));
	this->translate (*(buf.encodeStream ()));
	return (0);
}



int
TranslateBlock::decode (SerialBuffer &buf)
{
	this->translate (*(buf.decodeStream ()));
	return (0);
}



long
TranslateBlock::encodedSize (SerialBuffer &buf)
{
	SerialCountStream *cs = buf.countStream ();
	this->translate (*cs);
	return (cs->Count ());
}


