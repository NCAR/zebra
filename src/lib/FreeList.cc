/*
 * The "BlockFile" interface to random-access, block-structured files.
 */

#include <stdio.h>
#include <errno.h>
#include <iostream>
#include <iomanip>

//#include <defs.h>
//RCSID ("$Id: FreeList.cc,v 1.14 2002-09-17 20:00:19 granger Exp $");

#include "BlockFile.hh"		// Our interface definition
#include "BlockFileP.hh"	// For the private header structure and stuff
#include "AuxBlock.hh"
#include "Logger.hh"
#include "Format.hh"

// Constructor

FreeList::FreeList (BlockFile &bf_, Block &b, SyncBlock *parent_) : 
	SyncBlock (bf_, b), RefBlock (b, parent_),
	ncache(0),
	nfree(0),
	blocks(0),
	stats()
{
	// cout << "Constructing Freelist" << endl;
}


// Destructor

FreeList::~FreeList (void)
{
	if (blocks)
		::free (blocks);
}



void
FreeList::Free (BlkOffset offset, BlkSize length)
/*
 * Add a freed block to the list.  If the block can just be appended and/or
 * prepended to an existing block, do so with a recursive free.
 */ 
{
	readSync ();
	stats.bytesfreed += length;
	mark ();

	int i;
	for (i = 0; i < nfree; ++i)
	{
		if (offset + length == blocks[i].offset)
		{
			/* prepend */
		}
		else if (offset == blocks[i].offset + blocks[i].length)
		{
			/* append */
			offset = blocks[i].offset;
		}
		else
			continue;

		// Now we're looking to free a larger, combined block
		length += blocks[i].length;
		Remove (i);
		stats.bytesfreed -= length;
		Free (offset, length);
		return;
	}

	// At this point we have a contiguous, non-adjoining free block
	// at (offset, length).

	// See if we can recover this free space from the end of the file
	if (offset + length >= bf->header->bf_length)
	{
		bf->recover (offset);
	}
	else
	{
		// No choice but to add the freed block by itself
		Add (offset, length);
	}
}




#ifdef notdef
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
#endif


/*
 * Find a block for this request and take it off the list.
 */
BlkOffset
FreeList::Request (BlkSize length, BlkSize *ret_length)
{
	FreeBlock ret;

	readSync ();
	++stats.nrequest;
	stats.bytesrequest += length;
	mark ();

	// Calculate the block size to allocate for the request.
	// Allocate in 64-byte increments.
	BlkSize size = (((length-1) >> 6) + 1) << 6;
	ret.length = size;

	/* Search the free list for the closest match to this request.
	   If found, decide whether to return the whole block or split it. */
	int closest = -1;
	for (int i = 0 ; i < nfree ; ++i)
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
			stats.bytesfree -= ret.length;
		}
	}
	if (ret_length)
		*ret_length = ret.length;

	stats.bytesalloc += ret.length;
	return (ret.offset);
}



FreeStats
FreeList::Stats ()
{
	readSync();
	stats.nfree = this->nfree;
	return (stats);
}



void
FreeList::Show (std::ostream &out)
{
        using std::endl;
	readSync ();
	for (int i = 0; i < nfree; ++i)
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
	out << " alloc - request: " << slop << " bytes";
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



// ---------------- Private methods ----------------


void
FreeList::Add (BlkOffset offset, BlkSize length)
{
	mark ();
	stats.bytesfree += length;
	if (!blocks || nfree >= ncache)
	{
		growCache (nfree+1);
	}
	blocks[nfree].offset = offset;
	blocks[nfree].length = length;
	nfree++;
}



void
FreeList::Remove (int x)
{
	nfree--;
	stats.bytesfree -= blocks[x].length;
	for (int i = x ; i < nfree ; ++i)
		blocks[i] = blocks[i+1];
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





// ---------------- Serialization ----------------


long
FreeList::encodedSize (SerialBuffer &sbuf)
{
	long s = serialCount (sbuf, nfree);
	s += serialCount (sbuf, stats);
	if (nfree > 0)
	{
		long b = serialCount (sbuf, blocks[0]);
		s += nfree*b;
	}
	return (s);
}



int
FreeList::encode (SerialBuffer &sbuf)
{
	sbuf << nfree;
	stats.nfree = nfree;
	sbuf << (const FreeStats)stats;

	bf->log.Debug ("Writing free list blocks:");
	for (int i = 0; i < nfree; ++i)
	{
		bf->log.Debug (Printf("   %d bytes @ %d",
				      blocks[i].length, blocks[i].offset));
		sbuf << (const FreeBlock)blocks[i];
	}
	return (0);
}



int
FreeList::decode (SerialBuffer &sbuf)
{
	sbuf >> nfree;
	sbuf >> stats;
	stats.nfree = nfree;

	/*
	 * Make sure we have room in our array.
	 */
	growCache (nfree);

	/*
	 * Now read in the array.
	 */
	for (int i = 0; i < nfree; ++i)
	{
		sbuf >> blocks[i];
	}
	return (0);
}


