/*
 * Implementation of the auxillary block methods.
 */

#include <std.h>
#include <defs.h>
#undef bool

RCSID("$Id: AuxBlock.cc,v 1.3 1997-12-13 00:24:21 granger Exp $")

#include "AuxBlock.hh"
#include "BlockFile.hh"


#ifdef notdef
/* 
 * Initialize ourself to an existing block reference, which may be empty.
 * It will remain empty until we change and we're told to write sync.  If
 * the block exists, then its revision is > 0, so we'll read in the block
 * on the first read sync.
 */
AuxBlock::AuxBlock (BlockFile &_bf, Block &b) :
	bf(_bf), block(b)
{
	revision = 0;
	marked = 0;
}



// The base class destructor does nothing
AuxBlock::~AuxBlock ()
{ }



void
AuxBlock::writeSync (int force)
{
	// We don't bother if we haven't changed and we're not being forced
	if (clean() && ! force)
		return;

	// Encode ourself onto a serial buffer from the block file
	SerialBuffer *sbuf = bf.writeBuffer (block.length);
	encode (*sbuf);
	unsigned long growth = sbuf->Position();

	// If we've grown beyond the space of our block, we need to reallocate
	if (growth > block.length)
	{
		if (block.offset)
			bf.free (block.offset, block.length);
		growth = allocate (growth);
		block.offset = bf.alloc (growth, &block.length);
	}

	bf.write (block.offset, sbuf);

	// All clean now, update our block reference and our revision
	++block.revision;
	bf.header->mark();
	revision = block.revision;
	mark (0);
}



void 
AuxBlock::readSync ()
{
	// If we're not out of date, there's no point in re-reading
	if (revision < block.revision)
	{
		// Read a serial buffer from the block file for the given range
		SerialBuffer *sbuf;
		sbuf = bf.readBuffer (block.offset, block.length);
		decode (*sbuf);
		revision = block.revision;
		mark (0);
	}
}



BlkSize
AuxBlock::grow (BlkSize needed)
{
	// As a default growth algorithm, round to the nearest 256 bytes,
	// then add on half as many of those 256-byte blocks.

	long npages = (needed >> 8) + 1;
	npages += (npages >> 1);
	return (npages << 8);
}

#endif



