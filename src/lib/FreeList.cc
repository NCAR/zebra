/*
 * The "BlockFile" interface to random-access, block-structured files.
 */

#include <stdio.h>
#include <errno.h>
#include <iostream.h>
#include <iomanip.h>

#include <defs.h>

RCSID ("$Id: FreeList.cc,v 1.4 1997-12-13 00:24:31 granger Exp $");

#include "BlockFile.hh"		// Our interface definition
#include "BlockFileP.hh"	// For the private header structure and stuff
#include "AuxBlock.hh"
#include "Logger.hh"
#include "Format.hh"

// Constructor

FreeList::FreeList (BlockFile &bf, Block &b, SyncBlock *parent) : 
	SyncBlock (bf, b), RefBlock (b, parent)
{
	cout << "Constructing Freelist" << endl;
	ncache = 0;
	n = 0;
	blocks = 0;
}


// Destructor

FreeList::~FreeList (void)
{
	if (blocks)
		free (blocks);
}



void
FreeList::Free (BlkOffset offset, BlkSize length)
/*
 * Add a freed block to the list.  If the block can just be appended and/or
 * prepended to an existing block, do so.
 *
 * Still need to add logic to recover free space at the end of the file.
 */ 
{
	int remove = -1;
	int i;
	for (i = 0; i < n; ++i)
	{
		if (offset + length == blocks[i].offset)	/* prepend */
		{
			blocks[i].offset = offset;
			blocks[i].length += length;
		}
		else if (offset == blocks[i].offset + blocks[i].length)
		{
			blocks[i].length += length;		/* append */
		}
		else
			continue;

		// Now we're looking to free a larger block
		offset = blocks[i].offset;
		length = blocks[i].length;
		if (remove < 0)		// first contiguity found
			remove = i;
		else			// 2nd: freed block is in the middle
			break;
	}
	if (remove >= 0 && i < n)	// 3 blocks became 1, remove 1 of them
	{
		Remove (remove);
	}

	// Now see if we can recover the freed block from the end of the file
	if (offset + length >= bf.header->bf_length)
	{
		bf.recover (offset);
		// If we merged with an existing block, remove that block,
		// otherwise we just don't add the freed block
		if (remove >= 0 && i <= n)
		{
			if (i > remove)
				--i;		// moved back by first remove
			Remove (i);		// block @ remove already gone
		}
		else if (remove >= 0)
			Remove (remove);
	}
	else if (remove < 0)
	{
		// No choice but to add the freed block by itself
		Add (offset, length);
	}
	mark ();
}



/*
 * Find a block for this request and take it off the list.
 */
BlkOffset
FreeList::Request (BlkSize length, BlkSize *ret_length)
{
	FreeBlock ret;

	/* Search the free list for the closest match to this request.
	   If found, decide whether to return the whole block or split it. */
	int closest = -1;
	for (int i = 0 ; i < n ; ++i)
	{
		if (blocks[i].length >= length &&
		    (closest == -1 || 
		     blocks[closest].length < blocks[i].length))
		{
			closest = i;
		}
	}

	// Calculate the block size to allocate for the request
	// Allocate in 64-byte increments
	int size = (((length-1) >> 6) + 1) << 6;
	ret.length = size;

	if (closest < 0)
	{
		ret.offset = bf.append (size);
	}
	else
	{
		mark();
		ret.offset = blocks[closest].offset + blocks[closest].length;
		ret.offset -= ret.length;
		if (ret.offset == blocks[closest].offset)
			Remove (closest);
		else
			blocks[closest].length -= ret.length;
	}
	if (ret_length)
		*ret_length = ret.length;

	return (ret.offset);
}




void
FreeList::Add (BlkOffset offset, BlkSize length)
{
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
	n--;
	for (int i = x ; i < n ; ++i)
		blocks[i] = blocks[i+1];
}


// ---------------- Serialization ----------------


long
FreeList::size (SerialBuffer &sbuf)
{
	long s = serialCount (sbuf, n);
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
	bf.log->Debug ("Writing free list blocks:");
	for (int i = 0; i < n; ++i)
	{
		bf.log->Debug (Printf("   %d bytes @ %d",
				      blocks[i].length, blocks[i].offset));
		sbuf << blocks[i];
	}
	return (0);
}



int
FreeList::decode (SerialBuffer &sbuf)
{
	sbuf >> n;

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




