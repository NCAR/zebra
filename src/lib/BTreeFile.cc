/*
 * Implementation of BTreeFile methods.
 */
#ifndef BTREEFILE_IMPLEMENTATION
#define BTREEFILE_IMPLEMENTATION

#include <stddef.h>
#include <stdlib.h>
#include <iostream>
#include <assert.h>

//#include <defs.h>
//#undef bool
//extern "C" {
//#include <message.h>
//}

// RCSID ("$Id: BTreeFile.cc,v 1.17 2002-09-17 20:00:18 granger Exp $")

#include "Logger.hh"
#include "Format.hh"
#include "BTreeFile.hh"

/*
 * Need the BTree implementation.
 */
#include "BTree.cc"

/*
 * Inherit the BTreeNode and SyncBlock functionality for our BlockFile node.
 */
template <class K, class T>
class BlockNode : virtual public BTreeNode<K,T>, virtual public TranslateBlock
{
public:
	BlockNode (BlockFile &bf_, BTreeFile<K,T> &t, int depth_) :
		SyncBlock (bf_), 
		BTreeNode<K,T> (t, depth_),
		overflow (),
		filetree (t)
	{ }

	virtual ~BlockNode ();

	virtual void sync (int force = 0);

	// Pass btree node virtual methods on to the block file
	virtual void mark ()
	{
		SyncBlock::mark ();
	}

	// We implement our own read and write methods for SyncBlock.
	virtual void read ();

	virtual void write ();

	// Add to the base class free() functionality to free our overflow.
	virtual void free ();

	// Extend the allocate() method to keep some stats.
	virtual void allocate (BlkSize need);

	// As well as the translate() method for serialization.
	virtual void translate (SerialStream &ss);

	long nodeSize (SerialBuffer &wb);
	long leafSize (SerialBuffer &wb);
	long baseSize (SerialBuffer &wb);

	// No sense in adding growth since our size is fixed by the btree.
	virtual BlkSize grow (BlkSize needed)
	{
		return needed;
	}

protected:

	friend class BTreeFile<K,T>;

	// If there's an overflow block, we keep info about it here.
	Block overflow;

	// To keep a typesafe reference to our tree, which is actually
	// a subclass of BTree.
	BTreeFile<K,T>& filetree;
	unsigned long lru;

	BlockNode<K,T> *findLRU (BlockNode<K,T> *lrunode, 
				 BTreeP::Node *parent, 
				 BTreeP::Node **lruparent, int *n);

	virtual void destroy ();
};



template <class K, class T>
const unsigned long BTreeFile<K,T>::MAGIC = 0xbeeef11e;


#ifdef notdef
/*
 * This gets us an un-numbered ICE!  Woo-hoo!
 */
template <class K, class T>
template class BTreeFile<K,T>::Stats;
#endif


template <class K, class T>
BTreeFile<K,T>::BTreeFile (BlockFile &_bf, int order_, long sz, int fix) :
	BTree<K,T>(0, 0, 0),
	SyncBlock (_bf),
	bf (&_bf),
	our_bf(0)
{
	Init (order_, sz, fix);
	OpenHeader ();
}



template <class K, class T>
BTreeFile<K,T>::BTreeFile (int order_, long sz, int fix) :
	BTree<K,T>(0, 0, 0),
	SyncBlock (*(new BlockFile ("btree.bf", MAGIC, 
				    BlockFile::BF_EXCLUSIVE))),
	bf (SyncBlock::bf),
	our_bf(1)
{
	Init (order_, sz, fix);
	OpenHeader ();
}




template <class K, class T>
BTreeFile<K,T>::BTreeFile (const char *fname, int order_, long sz, int fix) :
	BTree<K,T>(0, 0, 0),
	SyncBlock (*(new BlockFile (fname, MAGIC,
				    BlockFile::BF_EXCLUSIVE))),
	bf (SyncBlock::bf),
	our_bf(1)
{
	Init (order_, sz, fix);
	OpenHeader ();
}



template <class K, class T>
BTreeFile<K,T>::BTreeFile (BlkOffset addr, BlockFile &_bf, 
			   int order_, long sz, int fix) : 
	BTree<K,T>(0, 0, 0),
	SyncBlock (_bf),
	bf (&_bf),
	our_bf(0)
{
	Init (order_, sz, fix);
	if (addr)
	{
		// Open a tree on an existing block.
		Open (addr);
	}
	else
	{
		// Else create a new one.
		Create ();
	}
}



/*
 * Return the header block to which we're currently attached.
 */
template <class K, class T>
BlkOffset BTreeFile<K,T>::Address ()
{
	return block.offset;
}



template <class K, class T>
void
BTreeFile<K,T>::Reopen ()
{
	BlkOffset addr = Address();
	std::string path(bf->Path());

	// Release the resources we have, but re-open the blockfile
	// only if it was actually ours to close 
	release ();
	if (our_bf)
	{
		bf = new BlockFile (path.c_str(), MAGIC, 
				    BlockFile::BF_EXCLUSIVE);
	}
	attach (Block(), *bf);
	Init (order, elementSize(), elementFixed());
	Open (addr);
}



template <class K, class T>
void
BTreeFile<K,T>::Init (int order_, long sz, int fix)
{
	Setup (order_, sz, fix);	// Do our superclass setup
	key_size = 0;
	key_size_fixed = 0;
	node_size = 0;
	leaf_size = 0;
	lru = 0;
	maxcache = 20;
	ncache = 0;
	fstats.reset();
	log.Declare ("BTreeFile");
	log.Extend (bf->Path());
}



template <class K, class T>
void
BTreeFile<K,T>::OpenHeader ()
{
	if (bf->Status() != BlockFile::OK)
	{
		++err;
		return;
	}

	// We have been given the blockfile to store into, so look for the
	// correct app magic number with an existing app header, else
	// we're creating a whole new tree.

	Block header;
	unsigned long magic;
	bf->ReadLock ();
	bf->getHeader (&header, &magic);
	if (header.offset)
	{
		if (magic != MAGIC)
		{
			log.Error ("BlockFile has wrong magic number.");
			++err;
			return;
		}
		log.Info (Format("Reading BTreeFile header at (%u,%u)") %
			  header.offset % header.length);
		Open (header.offset);
	}
	else
	{
		// We're just creating a new tree from scratch, but we still
		// need to set the BlockFile application header.
		bf->WriteLock ();
		Create ();
		bf->setHeader (block, MAGIC);
		bf->Unlock ();
	}
	bf->Unlock ();
}



/*
 * Create a new b-tree by allocating a new header block.
 */
template <class K, class T>
void
BTreeFile<K,T>::Create ()
{
	if (bf->Status() != BlockFile::OK)
	{
		++err;
		return;
	}
	// We're just creating a new tree from scratch.
	bf->WriteLock ();
	writeSync (1);
	log.Info (Format("BTreeFile header allocated at (%u,%u)") %
		  block.offset % block.length);
	bf->Unlock ();
}



/*
 * Open the b-tree at the given header block.
 */
template <class K, class T>
void
BTreeFile<K,T>::Open (BlkOffset addr)
{
	if (bf->Status() != BlockFile::OK)
	{
		++err;
		return;
	}
	bf->ReadLock ();
	attach (Block(addr));
	readSync ();

	// If we have a root node, it must always be in memory.
	if (depth != -1)
		root = get (rootNode, depth);
	bf->Unlock ();
}



/*
 * Obliterate all memory of this tree.  This means first erasing
 * it, and finally freeing the header block and object.
 */
template <class K, class T>
void
BTreeFile<K,T>::Destroy ()
{
	Erase ();
	assert (Empty());
	free ();
	BTree<K,T>::Destroy();
}



template <class K, class T>
BTreeFile<K,T>::~BTreeFile ()
{
	release ();
}



template <class K, class T>
void
BTreeFile<K,T>::release ()
{
	// Delete any nodes in memory, writing them to disk first if
	// necessary, leaving the tree empty when our superclass destructor
	// is called.
	if (root)
	{
		root->sync (1);
		delete root;
	}
	writeSync (1);
	root = 0;
	if (our_bf)
	{
		bf->Close ();
		delete bf;
		bf = 0;
	}
	ncache = 0;
	maxcache = 20;
}




/*
 * Recursive depth-first full traversal of nodes in memory to find
 * the least-recently-used dangling node.
 */
template <class K, class T>
BlockNode<K,T> * 
BlockNode<K,T>::findLRU (BlockNode<K,T> *lrunode, BTreeP::Node *parent, 
			 BTreeP::Node **lruparent, int *n)
{
	// Add ourself to the count of nodes in the memory cache.
	++(*n);
	BlockNode<K,T> *child = 0;
	BTreeP::Node *down = 0;
	for (int i = 0; (depth > 0) && (i < nkeys); ++i)
	{
		child = (BlockNode<K,T> *)children[i].local;
		if (child)
		{
			IFD(cout << string(depth*3,' ');
			    cout << " - Child " << (void*)child;
			    cout << " in memory, lru:" << child->lru;
			    cout << endl);
			/*
			 * Let this child check its subtree.
			 */
			down = &(children[i]);
			lrunode = child->findLRU (lrunode, down, lruparent, n);
		}
	}
	/*
	 * We are a leaf or have no children in memory, so we're dangling.
	 * See if we're the lru so far.
	 */
	if (depth == 0 || down == 0)
	{
		if (! lrunode || (this->lru < lrunode->lru))
		{
			lrunode = this;
			*lruparent = parent;
		}
	}
	return (lrunode);
}



/*
 * If we currently have more nodes cached than we should, release
 * the extra ones.
 */
template <class K, class T>
void
BTreeFile<K,T>::trimCache ()
{
	BlockNode<K,T> *that;

	/*
	 * If for some reason the tree grows very deep, we need to keep
	 * at least enough nodes in memory for a depth-first traversal,
	 * and some extra for recombinations and such.
	 */
	if (3*depth >= maxcache)
	{
		maxcache = 3*depth;
	}

	/*
	 * Loop through the LRU nodes and release them until under the
	 * maxcache threshold.
	 */
	while (ncache > maxcache)
	{
		// Find an old one in memory.  Note that findLRU() could
		// return the root node back to us if it had no children,
		// which would be bad, but that cannot happen else there
		// would be more open memory cache slots and we wouldn't
		// be here.
		//
	        BTreeP::Node *parent;
		that = (BlockNode<K,T> *)rootNode.local;
		if (that)
		{	
			IFD(cout << "Current context: ";
			    current->show (cout);
			    cout << "Looking for least LRU in root:" << endl);
			int n = 0;
			that = that->findLRU (0, 0, &parent, &n);
			IFD(cout << "Found node " << (void*)that 
			     << " depth:" << that->depth
			     << " lru:" << that->lru
			     << "; nodes searched: " << n 
			     << "; ncache: " << ncache << endl);
		}
		if (that)
		{
			// Force it to disk if dirty and release it
			current->invalidate (that);
			parent->local = 0;
			if (that->dirty())
				that->writeSync (1);
			delete that;
		}
	}
}



template <class K, class T>
BlockNode<K,T> * 
BTreeFile<K,T>::lookupCache (int node_depth)
{
	BlockNode<K,T> *that;

	++ncache;
	that = new BlockNode<K,T> (*bf, *this, node_depth);
	return that;
}



template <class K, class T>
BTreeNode<K,T> * 
BTreeFile<K,T>::get (BTreeP::Node &node, int node_depth)
{
	// The simple case is when this node is still in memory
	BlockNode<K,T> *that = (BlockNode<K,T> *)node.local;

	if (! that)
	{
		that = lookupCache (node_depth);
		node.local = that;
		that->block.offset = node.addr;
		assert (that->block.offset > 0);
		// Initially set this to the minimum we need to read, but
		// it will be rewritten with the actual allocated size
		// when the node is translated.
		that->block.length = nodeSize (that);
		that->thisNode.addr = that->block.offset;
		that->thisNode.local = that;
		log.Debug (Format("Recreating node from block (%u,%u)") %
			    that->block.offset % that->block.length);
	}
	that->readSync ();
	that->lru = lru++;
	return (that);
}



template <class K, class T>
BTreeNode<K,T> * 
BTreeFile<K,T>::make (int node_depth)
{
	log.Info (Format(" + creating node, depth: %i") % node_depth);
	BlockNode<K,T> *made;
	made = lookupCache (node_depth);
	made->thisNode.local = made;
	made->lru = lru++;
	// Force allocation to get an address
	made->allocate (nodeSize (made));
	assert (made->block.offset > 0);
	made->thisNode.addr = made->block.offset;
	log.Info (Format(" + node created (%u,%u), depth: %i")
		  % made->block.offset % made->block.length % node_depth);
	return (made);
}



template <class K, class T>
void
BTreeFile<K,T>::enterWrite ()
{
	bf->WriteLock ();
	writeLock ();		// So that we writeSync when we unlock()
}


template <class K, class T>
void
BTreeFile<K,T>::enterRead ()
{
	bf->ReadLock ();
	readLock ();
}


template <class K, class T>
void
BTreeFile<K,T>::leave ()
{
	if (/*bf->WriteLockPending() &&*/ lock == 1 && root)
	{
		// Reduce the node cache to its threshold
		trimCache();

		// Need to tell all nodes in memory to writeSync(), which
		// right now is done by a recursive sync() method.  This
		// is unnecessary when we have exclusive access.
		if (! bf->Exclusive())
			root->sync();
	}
	unlock ();
	bf->Unlock ();
}


template <class K, class T>
void
BTreeFile<K,T>::mark ()
{
	TranslateBlock::mark ();
}



void
BTreeFileP::FileStatsPart::translate (SerialStream &ss)
{
	// Only the overflow and allocated bytes are persistent.  Read and
	// write counts last only as long as our block file object.
	ss << overflowBytes << allocBytes;
}


template <class K, class T>
std::ostream &
BTreeFile<K,T>::reportStats (std::ostream &out, 
			     const BTreeFileP::FileStats &s) const
{
        using std::endl;
	BTree<K,T>::reportStats (out, s);
	out << "Nodes read: " << s.file.nodesRead 
	    << "; written: " << s.file.nodesWritten
	    << "; Bytes alloc: " << s.file.allocBytes
	    << "; overflow: " << s.file.overflowBytes << endl;
	out << "Leaf blk size: " << leafBlockSize()
	    << "; branch: " << nodeBlockSize();
	long slop = s.file.allocBytes;
	slop -= (s.nLeaves * leafBlockSize());
	slop -= ((s.nNodes - s.nLeaves) * nodeBlockSize());
	out << "; slop: " << slop;
	out << "; Nodes cached: " << ncache 
	    << "; max cache: " << maxcache;
	out << endl;
	return out;
}



template <class K, class T>
void
BTreeFile<K,T>::translate (SerialStream &ss)
{
	// Translate our superclass state:
	TranslateBlock::translate (ss);
	BTree<K,T>::translate (ss);

	// Translate our own persistent state
	ss << key_size << key_size_fixed;
	ss << node_size << leaf_size;
	this->fstats.translate (ss);
}



template <class K, class T>
void
BTreeFile<K,T>::setKeySize (int ks, int fixed)
{
	enterWrite ();
	if (rootNode.addr == 0)
	{
		key_size = ks;
		key_size_fixed = fixed;
		leaf_size = 0;
		node_size = 0;
		mark ();
	}
	leave ();
}




template <class K, class T>
int
BTreeFile<K,T>::keySize (int *fixed)
{
	enterRead ();
	if (fixed)
		*fixed = key_size_fixed;
	leave ();
	return (key_size);
}



template <class K, class T>
int
BTreeFile<K,T>::setElementSize (int vs, int fixed)
{
	enterWrite ();
	int done = BTree<K,T>::setElementSize (vs, fixed);
	if (done)
	{
		leaf_size = 0;
		node_size = 0;
	}
	leave ();
	return done;
}




template <class K, class T>
long 
BTreeFile<K,T>::nodeSize (BlockNode<K,T> *node)
{
	if (node->Depth() > 0)
	{
		if (! node_size)
		{
			enterWrite ();
			node_size = node->nodeSize (*bf->writeBuffer(256));
			node_size = (((node_size-1) >> 7) + 1) << 7;
			leave();
		}
		return (node_size);
	}
	if (! leaf_size)
	{
		enterWrite ();
		leaf_size = node->leafSize (*bf->writeBuffer(256));
		leaf_size = (((leaf_size-1) >> 7) + 1) << 7;
		leave();
	}
	return (leaf_size);
}



/* ================================================================ */
/* BlockNode methods */



template <class K, class T>
BlockNode<K,T>::~BlockNode ()
{
	// One less node in the memory cache
	--filetree.ncache;

	// Recursively delete our children, but only those currently in memory
	if (depth > 0)
	{
		// Traverse and delete the children residing in memory.
		for (int i = 0; i < nkeys; ++i)
		{
			BlockNode<K,T> *that = 
				(BlockNode<K,T> *)children[i].local;
			if (that)
			{
				delete that;
			}
		}
		nkeys = 0;
	}
	// Superclass frees the rest of this node's memory.
}



template <class K, class T>
void
BlockNode<K,T>::destroy ()
{
	free();
	BTreeNode<K,T>::destroy ();	// Deletes us from memory
}



template <class K, class T>
void
BlockNode<K,T>::allocate (BlkSize need)
{
	BlkSize len = block.length;
	TranslateBlock::allocate (need);
	if (block.length != len)
	{
		filetree.fstats.allocBytes += (block.length - len);
		filetree.mark();
	}
}



template <class K, class T>
void
BlockNode<K,T>::free ()
{
	if (overflow.offset)
	{
		filetree.fstats.overflowBytes -= overflow.length;
		bf->Free (overflow.offset, overflow.length);
	}
	filetree.fstats.allocBytes -= block.length;
	filetree.mark();
	TranslateBlock::free ();
}



/*
 * Non-zero 'force' means force the write sync iff the node is dirty,
 * which is necessary when we have exclusive blockfile access but
 * the node is being released from memory.
 */
template <class K, class T>
void
BlockNode<K,T>::sync (int force)
{
	// Sync our children first, then ourself
	if (depth > 0)
	{
		for (int i = 0; i < nkeys; ++i)
		{
			BlockNode<K,T> *child = 
				(BlockNode<K,T> *)children[i].local;
			assert (children[i].addr > 0);
			if (child)
			{
				child->sync (force);
			}
		}
	}
	assert (thisNode.addr > 0);
	assert (block.offset > 0 && block.length > 0);
	if (dirty())
		writeSync (force);
}


/*
 * Write our node to the block file, possibly using an overflow block.
 * When this method is called, our superclass syncblock methods have
 * already determined and allocated our block size.  All that remains is
 * allocating an overflow block if necessary, encoding into a sufficiently
 * large buffer, and writing that buffer to the file.  We only write into
 * as much space as the btree has fixed upon for our node, since that is
 * all that will be read in when we are recreated.  The actual block
 * size allocated in the file, though, is still in SyncBlock::block.
 */
template <class K, class T>
void
BlockNode<K,T>::write ()
{
	assert (block.offset > 0 && block.length > 0);

	// Update stats
	++filetree.fstats.nodesWritten;
	filetree.mark ();

	// Figure out how much space we need and get a buffer to encode into.
	unsigned long nspace = filetree.nodeSize (this);
	SerialBuffer *wb = bf->writeBuffer (nspace + overflow.length);
	unsigned long len = encodedSize (*wb);

	// Get enough space to translate the whole node.
	wb->Need (len);

	// Determine whether we need overflow space, and if so allocate or
	// reallocate our overflow block as necessary.
	if (len > nspace && (len - nspace > overflow.length))
	{
		filetree.fstats.overflowBytes -= overflow.length;
		if (overflow.offset)
		{
			// Reallocate
			bf->Free (overflow.offset, overflow.length);
			overflow.offset = overflow.length = 0;
		}

		// Allocate
		overflow.offset = bf->Alloc (len - nspace, &overflow.length);
		filetree.fstats.overflowBytes += overflow.length;
		filetree.mark ();
	}
	else if (len <= nspace)
	{
		// At some point, this would be where we release an
		// overflow block which is no longer needed.
	}

	// Only do the encoding once the overflow state has been set
	translate (*(wb->encodeStream ()));

	// Write our buffer, and overflow if any
	bf->Write (block.offset, wb->Peek(0, 0), 
		   (len > nspace) ? nspace : len);
	if (len > nspace)
	{
		bf->Write (overflow.offset, wb->Peek(nspace, 0), len - nspace);
	}
}


/*
 * Read our node from the block file.
 */
template <class K, class T>
void
BlockNode<K,T>::read ()
{
	// Update stats
	++filetree.fstats.nodesRead;
	filetree.mark ();

	// Read the regular block size into a buffer and get our overflow.
	SerialBuffer *rb = bf->readBuffer (block.offset, block.length);
	SerialDecodeStream &ds = *rb->decodeStream ();
	ds >> overflow;

	if (overflow.offset)
	{
		rb->Seek (block.length);
		rb->Need (overflow.length);
		bf->Read (rb->Peek (), overflow.offset, overflow.length);
	}
	rb->Reset ();
	translate (ds);
}



template <class K, class T>
void
BlockNode<K,T>::translate (SerialStream &ss)
{
	// Overflow must be the first object in the stream.  It is
	// expected to be there by read(). 
	ss << overflow;

	// Need to keep our originally allocated block length in our
	// persistent state so we can free it all when done with it.
	TranslateBlock::translate (ss);

	ss << nkeys;
	for (int i = 0; i < nkeys; ++i)
	{
		ss << keys[i];
		if (depth > 0)
		{
			ss << children[i];
		}
		else
		{
			ss << table[i];
		}
	}
	if (depth == 0)
	{
		// Translate the contents of our serial buffer
		ss << table[nkeys];
		sbuf->Reset ();
		sbuf->Need (table[nkeys].offset);
		ss.opaque (sbuf->Peek (), table[nkeys].offset);
	}
}



template <class K, class T>
long
BlockNode<K,T>::baseSize (SerialBuffer &wb)
{

	/* Given the layout of our node, calculate some estimate of the
	   encoded size of a branch node. */

	// At the minimum we need space for the number of keys,
	// overflow information, and element offset table.
	// The last element offset is the length of our value buffer;
	// a separate count gives us the length of the key buffer.
	SerialCountStream& cs = *wb.countStream();
	cs << overflow;
	TranslateBlock::translate (cs);
	cs << nkeys;
	long s = cs.Count();

	int maxkeys = maxKeys();

	// Make a best guess on the key space we'll need.  If the set
	// size is zero, get the size from our first key. If not fixed,
	// add 50% slush space.
	int fixed;
	long keylen = filetree.keySize (&fixed);
	if (!keylen)
	{
		//keylen = sizeof (K);
		keylen = serialCount (wb, K());
	}
	keylen *= maxkeys;
	if (! fixed)
	{
		keylen += keylen / 2;
	}
	s += keylen;
	return (s);
}



template <class K, class T>
long
BlockNode<K,T>::leafSize (SerialBuffer &wb)
{
	long s = baseSize (wb);

	// Element offset table
	s += (maxKeys() + 1) * serialCount (wb, table[0]);

	// Values
	int fixed = 0;
	long vlen = filetree.elementSize (&fixed);
	if (!vlen)
		vlen = sizeof (T);
	vlen *= maxKeys();
	if (! fixed)
		vlen += vlen / 2;
	s += vlen;
	return (s);
}




template <class K, class T>
long
BlockNode<K,T>::nodeSize (SerialBuffer &wb)
{
	long s = baseSize (wb);

	// Children array
	s += tree.Order() * serialCount (wb, children[0]);
	return (s);
}


#endif /*BTREEFILE_IMPLEMENTATION*/

