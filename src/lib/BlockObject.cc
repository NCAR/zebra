/*
 * Implementation of the auxillary block methods.
 */

//#include <std.h>
//#include <defs.h>
//#undef bool
//
//RCSID("$Id: BlockObject.cc,v 1.5 1998-05-15 19:36:47 granger Exp $")

#include "BlockFile.hh"
#include "BlockObject.hh"


SyncBlock::SyncBlock (BlockFile &_bf, const Block &exist) :
	bf(&_bf), block(exist)
{
	// cout << "SyncBlock constructor" << endl;
	block.revision = 0;	// we have no revision in memory yet
	marked = 0;
	changed = 0;
	lock = 0;
	writelock = 0;

	// If the given block has not been allocated, we're dirty
	// because we don't yet exist in the file
	if (block.offset == 0 && block.length == 0)
		mark ();
}


SyncBlock::SyncBlock (BlockFile &_bf) :
	bf(&_bf), block()
{
	// cout << "SyncBlock constructor" << endl;
	marked = 0;
	changed = 0;
	lock = 0;
	writelock = 0;
}



void
SyncBlock::attach (const Block &exist)
{
	block = exist;
	newRev ();
	block.revision = 0;	// still need to read the new block
}



void
SyncBlock::mark (int _marked)
{
	// Increment our revision number on the first change
	if (clean() && _marked)
		++block.revision;
	this->marked = _marked;
}



void 
SyncBlock::writeSync (int force)
{
	if (needsWrite (force))
	{
		write ();
	}
	newRev ();
	mark (0);
}



void
SyncBlock::readSync ()
{
	if (needsRead ())
	{
		read ();
		changed = 0;
	}
	updateRev ();
	mark (0);
}



int
SyncBlock::needsRead ()
{
	changed = (changed || bf->Changed (block.revision, block.offset,
					   block.length));
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
			block.offset = bf->Alloc (need, &block.length);
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


