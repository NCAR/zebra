/*
 * The "BlockFile" interface to random-access, block-structured files.
 */

#include <stdio.h>
#include <errno.h>
#include <iostream.h>
#include <iomanip.h>

//#include <defs.h>
//RCSID ("$Id: FreeList.cc,v 1.9 1998-05-28 21:42:12 granger Exp $");

#include "BlockFile.hh"		// Our interface definition
#include "BlockFileP.hh"	// For the private header structure and stuff
#include "AuxBlock.hh"
#include "Logger.hh"
#include "Format.hh"

// Constructor

FreeList::FreeList (BlockFile &bf, Block &b, SyncBlock *parent) : 
	SyncBlock (bf, b), RefBlock (b, parent),
	ncache(0),
	n(0),
	blocks(0),
	stats()
{
	cout << "Constructing Freelist" << endl;
}


// Destructor

FreeList::~FreeList (void)
{
	if (blocks)
		::free (blocks);
}



void
FreeList::Free (BlkOffset offset, BlkSize _length)
/*
 * Add a freed block to the list.  If the block can just be appended and/or
 * prepended to an existing block, do so.
 */ 
{
	BlkSize length = _length;
	stats.bytesfreed += _length;
	mark ();

	int remove = -1;
	int i;
	for (i = 0; i < n; ++i)
	{
		if (offset + length == blocks[i].offset)
		{
			blocks[i].offset = offset;		/* prepend */
			blocks[i].length += length;
		}
		else if (offset == blocks[i].offset + blocks[i].length)
		{
			blocks[i].length += length;		/* append */
		}
		else
			continue;

		// Now we're looking to free a larger, combined block
		offset = blocks[i].offset;
		length = blocks[i].length;
		if (remove < 0)		// first contiguity found
		{
			// Freed space absorbed by an existing block
			stats.bytesfree += _length;
			remove = i;
		}
		else			// 2nd: freed block is in the middle
		{
			break;
		}
	}

	// At this point we have a contiguous, non-adjoininng free block
	// at (offset, length).  If i < n, the block is at i, else if
	// remove >= 0, the block is at index remove, else the block
	// has not been inserted yet.

	if (remove >= 0 && i < n)
	{
		// 3 blocks became 1, remove 1 of them
		Remove (remove);
	}

	// Now see if we can recover this free space from the end of the file
	if (offset + length >= bf->header->bf_length)
	{
		bf->recover (offset);

		// We may have absorbed the freed space into this larger
		// block, but now this whole block can be forgotten.
		// Otherwise, we just ignore the block and the free list
		// does not change.
		if (remove >= 0)
		{
			stats.bytesfree -= length;

			// If we merged with an existing block, remove it
			if (i <= n)
			{
				// moved back by first remove	
				if (i > remove)
					--i;
				// block @ remove already gone
				Remove (i);
			}
			else
			{
				Remove (remove);
			}
		}
	}
	else if (remove < 0)
	{
		// No choice but to add the freed block by itself
		Add (offset, length);
	}
	else
	{
		// The freed space was absorbed into an existing block
	}
}



/*
 * Find a block for this request and take it off the list.
 */
BlkOffset
FreeList::Request (BlkSize length, BlkSize *ret_length)
{
	FreeBlock ret;

	++stats.nrequest;
	stats.bytesrequest += length;
	mark ();

	// Calculate the block size to allocate for the request
	// Allocate in 64-byte increments
	BlkSize size = (((length-1) >> 6) + 1) << 6;
	ret.length = size;

	/* Search the free list for the closest match to this request.
	   If found, decide whether to return the whole block or split it. */
	int closest = -1;
	for (int i = 0 ; i < n ; ++i)
	{
		if (blocks[i].length == size)
		{
			closest = i;
			break;
		}
		if (blocks[i].length >= size &&
		    (closest == -1 || 
		     blocks[closest].length < blocks[i].length))
		{
			closest = i;
		}
	}

	if (closest < 0)
	{
		ret.offset = bf->append (size);
	}
	else
	{
		mark();
		// Just use the whole block if within 1/8 of needed length
		if (ret.length + (ret.length >> 3) >= blocks[closest].length)
			ret.length = blocks[closest].length;
		ret.offset = blocks[closest].offset + blocks[closest].length;
		ret.offset -= ret.length;
		if (ret.offset == blocks[closest].offset)
		{
			Remove (closest);
		}
		else
		{
			blocks[closest].length -= ret.length;
		}
		stats.bytesfree -= ret.length;
	}
	if (ret_length)
		*ret_length = ret.length;

	stats.bytesalloc += ret.length;
	return (ret.offset);
}




void
FreeList::Add (BlkOffset offset, BlkSize length)
{
	stats.nfree++;
	stats.bytesfree += length;
	if (!blocks || n >= ncache)
	{
		growCache (n+1);
	}
	blocks[n].offset = offset;
	blocks[n].length = length;
	n++;
}



void
FreeList::Remove (int x)
{
	stats.nfree--;
	n--;
	for (int i = x ; i < n ; ++i)
		blocks[i] = blocks[i+1];
}



void
FreeList::Show (ostream &out)
{
	for (int i = 0; i < n; ++i)
	{
		out << Printf("   %d bytes @ %d\n", 
			      blocks[i].length, blocks[i].offset);
	}

	FreeStats fs = Stats();
	out << "           nfree: " << fs.nfree << endl;
	out << "      bytes free: " << fs.bytesfree << endl;
	out << "        nrequest: " << fs.nrequest << endl;
	out << "   bytes request: " << fs.bytesrequest << endl;
	out << "     bytes alloc: " << fs.bytesalloc << endl;
	out << "     bytes freed: " << fs.bytesfreed << endl;
	long slop = fs.bytesalloc - fs.bytesrequest;
	out << "            slop: " << slop << " bytes";
	if (fs.bytesalloc > 0)
	{
		float sloppct = (float)slop/fs.bytesalloc*100.0;
		out << ", " << sloppct << "%";
	}
	out << endl;
	if (fs.nrequest > 0)
	{
		out << "     avg request: " 
		    << (float)fs.bytesrequest/fs.nrequest 
		    << " bytes" << endl;
	}
	out << "        pct free: "
	    << (float)fs.bytesfree/bf->header->bf_length*100.0 
	    << "%" << endl;
	if (fs.nfree > 0)
	{
		out << "  avg free block: "
		    << (float)fs.bytesfree/fs.nfree
		    << " bytes" << endl;
	}
}




// ---------------- Serialization ----------------


long
FreeList::encodedSize (SerialBuffer &sbuf)
{
	long s = serialCount (sbuf, n);
	s += serialCount (sbuf, stats);
	if (n > 0)
	{
		long b = serialCount (sbuf, blocks[0]);
		s += n*b;
	}
	return (s);
}



int
FreeList::encode (SerialBuffer &sbuf)
{
	sbuf << n;
	sbuf << stats;

	bf->log->Debug ("Writing free list blocks:");
	for (int i = 0; i < n; ++i)
	{
		bf->log->Debug (Printf("   %d bytes @ %d",
				      blocks[i].length, blocks[i].offset));
		sbuf << blocks[i];
	}
	return (0);
}



int
FreeList::decode (SerialBuffer &sbuf)
{
	sbuf >> n;
	sbuf >> stats;

	/*
	 * Make sure we have room in our array.
	 */
	growCache (n);

	/*
	 * Now read in the array.
	 */
	for (int i = 0; i < n; ++i)
	{
		sbuf >> blocks[i];
	}
	return (0);
}




void
FreeList::growCache (int num)
{
	int mincache = num + (num/2) + 1;
	int len = mincache * sizeof (FreeBlock);
	if (!blocks)
	{
		blocks = (FreeBlock *) malloc (len);
		ncache = mincache;
	}
	else if (mincache > ncache)
	{
		// Increase our cache
		blocks = (FreeBlock *) realloc (blocks, len);
		ncache = mincache;
	}
	else if (mincache < ncache / 2)
	{
		// trim off some excess cache space
		blocks = (FreeBlock *) realloc (blocks, len);
		ncache = mincache;
	}		
}




