/*
 * The "BlockFile" interface to random-access, block-structured files.
 */

#include <stdio.h>
#include <errno.h>
#include <iostream.h>
#include <iomanip.h>

#include <defs.h>

RCSID ("$Id: FreeList.cc,v 1.1 1997-11-24 10:29:41 granger Exp $");

#include "BlockFile.hh"		// Our interface definition
#include "blockfile.h"		// For the private header structure and stuff

// XDR_ADDTYPE(BF_FreeBlock)

//
// Package free list memory management into a convenient structure
//
class FreeList : public AuxBlock, public Translatable
{
public:
	FreeList (BlockFile &bf);
	~FreeList (void);

	/*
	 * Add a freed block to the free block list.
	 */
	void FreeBlock (BlkOffset offset, BlkSize length);

	/*
	 * Find a block for this request and take it off the list.
	 * Return non-zero on success, zero otherwise.
	 */
	int RequestBlock (BlkSize length, BF_FreeBlock &ret);
	BlkOffset RequestBlock (BlkSize length, BlkSize *ret_length);

	void translate (SerialStream &ss)
	{
		ss << ncache << n;
		for (int i = 0; i < n; ++i)
		{
			ss << blocks[i];
		}
	}

private:
	int ncache;	/* Number of free blocks allocated space in array */
	int n;		/* Actual number of free blocks in use in array */
	FreeBlock *blocks; /* The actual array of free blocks */

	void Remove (int);
	void Add (BlkSize length, BlkSize length);

};




// Constructor

FreeList::FreeList (BlockFile &bf) : AuxBlock (bf)
{
	ncache = 0;
	n = 0;
	blocks = NULL;
}


// Destructor

FreeList::~FreeList (void)
{
	if (blocks)
		free (blocks);
}



void
FreeList::FreeBlock (BlkOffset offset, BlkSize length)
/*
 * Add a freed block to the list.  If the block can just be appended and/or
 * prepended to an existing block, do so.
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
		if (remove < 0)		/* first contiguity found */
		{
			remove = i;
			offset = blocks[i].offset;
			length = blocks[i].length;
		}
		else			// second found (freed block in middle)
		{
			break;
		}
	}
	if (remove >= 0 && i < n)	// 3 blocks became 1, remove 1 of them
	{
		Remove (remove);
		return;
	}
	else if (remove >= 0)
	{
		return;			// freed block added to existing block
	}

	/*
	 * Looks like we have to actually add this block to the list.
	 */
	Add (offset, length);
}



BlkOffset
FreeList::RequestBlock (BlkSize length, BlkSize *ret_length)
{
	BF_FreeBlock fb;
	if (RequestBlock (length, fb))
	{
		*ret_length = fb.length;
		return (fb.offset);
	}
	return (BadBlock);
}



/*
 * Find a block for this request and take it off the list.
 */
int
FreeList::RequestBlock (BlkSize length, BF_FreeBlock &ret)
{
	/* Search the free list for the closest match to this request.
	   If found, decide whether to return the whole block or split it. */

	if (n == 0)
		return (0);

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

	if (closest < 0)
		return (0);
	ret = blocks[closest];
	Remove (closest);
	return (1);
}



void
FreeList::Add (BlkOffset offset, BlkSize length)
{
	if (n >= ncache)
	{
		ncache = (ncache > 0) ? ncache * 2 : 16;
		int len = ncache * sizeof (BF_FreeBlock);
		if (blocks)
			blocks = (BF_FreeBlock *) realloc (blocks, len);
		else
			blocks = (BF_FreeBlock *) malloc (len);
	}
	blocks[n].offset = offset;
	blocks[n].length = length;
	rev++;
	n++;
}



void
FreeList::Remove (int x)
{
	n--;
	for (int i = x ; i < n ; ++i)
		blocks[i] = blocks[i+1];
	++rev;
}



void
FreeList::WriteSync ()
/*
 * Write the contents of our free list in memory to the file on disk.
 */
{
	BF_Block *fl = &(bf->header->freelist);

	if (rev == fl->revision)	// haven't changed since last write
		return;

	/*
	 * Make sure there is room enough on disk for the list we will
	 * write.  The increment assures us that we'll have room for any
	 * extra blocks in the list if the list changes underneath us.
	 */
	int len = XDRStream::Length (xdr_BF_FreeBlock, blocks) * (n + 5);

	if (len > fl->length)
	{
		if (fl->length > 0)
			bf->Free (fl->offset);
		fl->offset = bf->Alloc (len, &fl->length);
	}
		
	fl->count = n;
	fl->revision = rev;
	bf->mark (BlockFile::HEADER);
	XDRStream *xdrs = bf->encodeStream (fl->offset);
	bf->log->Debug ("Writing free list blocks:");
	for (int i = 0; i < n; ++i)
	{
		bf->log->Debug ("   %d bytes @ %d", 
				blocks[i].length, blocks[i].offset);
		xdrs << blocks[i];
	}
}



void
FreeList::ReadSync ()
{
	BF_Block *fl = &(bf->header->freelist);

	/*
	 * Compare revisions.  The header is assumed to be in sync!
	 */
	if (fl->revision == rev)
		return;		/* a-ok */
	if (fl->count == 0)
	{
		// The free list is empty: no need to read; leave the cache.
		n = 0;
		return;
	}

	/*
	 * Make sure we have room in our array.
	 */
	int len = fl->count * sizeof (BF_FreeBlock);
	n = fl->count;
	if (!blocks)
	{
		blocks = (BF_FreeBlock *) malloc (len);
		ncache = fl->count;
	}
	else if (n > ncache)
	{
		// Increase our cache
		blocks = (BF_FreeBlock *) realloc (blocks, len);
		ncache = fl->count;
	}
	/*
	 * Now we can finally read the array.
	 */
	XDRStream *xdrs = bf->decodeStream (fl->offset);
	for (int i = 0; i < n; ++i)
	{
		xdrs >> blocks[i];
	}
	rev = fl->revision;
}


