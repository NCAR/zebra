/*
 * Implementation of the auxillary block methods.
 */

#include <std.h>
#include <defs.h>
#undef bool

RCSID("$Id: BlockObject.cc,v 1.2 1997-12-14 23:50:11 granger Exp $")

#include "BlockFile.hh"
#include "BlockObject.hh"


SyncBlock::SyncBlock (BlockFile &_bf, const Block &exist) :
	bf(&_bf), block(exist)
{
	cout << "SyncBlock constructor" << endl;
	marked = 0;
	changed = 0;
}


SyncBlock::SyncBlock (BlockFile &_bf) :
	bf(&_bf), block()
{
	cout << "SyncBlock constructor" << endl;
	marked = 0;
	changed = 0;
}



void
SyncBlock::mark (int _marked)
{
	// Increment our revision number on the first change
	if (clean() && _marked)
		++block.revision;
	this->marked = _marked;
	this->changed = _marked;
}



void 
SyncBlock::writeSync (int force)
{
	if (needsWrite (force))
	{
		write ();
	}
	mark (0);
}



void
SyncBlock::readSync ()
{
	if (needsRead ())
	{
		read ();
		updateRev ();
		mark (0);
	}
}



int
SyncBlock::needsRead ()
{
	if (! changed)
		changed = bf->Changed (block.revision, block.offset,
				       block.length);
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
#ifdef notdef		
		// Don't grow on the first allocation, in case we
		// are an object which will never grow.
		if (block.length > 0)
#endif
			need = grow (need);
		block.offset = bf->Alloc (need, &block.length);
	}
}



BlkSize
SyncBlock::grow (BlkSize needed)
{
	// Default growth algorithm, round to the nearest 256
	// bytes, then add on half as many 256-byte blocks.
	long npages = (needed >> 8) + 1;
	npages += (npages >> 1);
	return (npages << 8);
}



SyncBlock::~SyncBlock ()
{ }




int
TranslateBlock::encode (SerialBuffer &buf)
{
	buf.Need (this->size (buf));
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
TranslateBlock::size (SerialBuffer &buf)
{
	SerialCountStream *cs = buf.countStream ();
	this->translate (*cs);
	return (cs->Count ());
}


