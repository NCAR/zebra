/*
 * Implementation of the auxillary block methods.
 */

#include <std.h>
#include <defs.h>

RCSID("$Id: AuxBlock.cc,v 1.1 1997-11-24 10:43:16 granger Exp $")

#include "AuxBlock.hh"
#include "BlockFile.hh"

inline
AuxBlock::auxBlock()
{
	block = 0;
	length = 0;
	_marked = 0;
	revision = 0;
	magic = 0;
}



AuxBlock::AuxBlock (BlockFile &bf, BlkOffset where, BlkSize howbig)
{
	auxBlock();

	block = where;
	length = howbig;
	this->bf = &bf;
}



AuxBlock::AuxBlock (BlockFile &bf)
{
	auxBlock ();

	// Since we don't even exist in the file yet, we're dirty
	mark ();
}



// The base class destructor does nothing
AuxBlock::~AuxBlock ()
{ }



void
AuxBlock::writeSync (int force = 0)
{
	// We don't bother if we haven't changed and we're not being forced
	if (clean() && ! force)
		return;

	// Carry on with the write.  If no block yet, allocate one.
	if (! block)
		allocate ();

	// Get a serial buffer from the block file to write into
	SerialBuffer *sbuf = bf->tempBuffer (length);
	encode (*sbuf);
	bf->writeBuffer (sbuf, sbuf->Position());

	// All clean now
	mark (0);
}



void 
AuxBlock::readSync ()
{
	// Read a serial buffer from the block file for the given range
	SerialBuffer *sbuf = bf->readBuffer (block, length);
	decode (*sbuf);
	mark (0);
}



BlkSize
AuxBlock::grow (BlkSize needed)
{
	// As a default growth algorithm, round to the nearest 256 bytes,
	// then add on half as many of those 256-byte blocks.

	int npages = (needed >> 8) + 1;
	npages += (npages >> 1);
	return (npages << 8);
}



void
AuxBlock::translate (SerialStream &ss)
{
	ss << block << length << revision << magic << id;
}



