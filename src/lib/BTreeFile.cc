/*
 * Implementation of BTreeFile methods.
 */

#include <stddef.h>
#include <stdlib.h>
#include <iostream.h>
#include <assert.h>

//#include <defs.h>
//#undef bool
//extern "C" {
//#include <message.h>
//}

// RCSID ("$Id: BTreeFile.cc,v 1.3 1998-05-28 22:00:45 granger Exp $")

#include "Logger.hh"
#include "Format.hh"
#include "BTreeFile.hh"
#include "BlockFactory.hh"

#include "Serialize.hh"

SERIAL_STREAMABLE (Node);


template <class K, class T>
const unsigned long BTreeFile<K,T>::MAGIC = 0xbeeef11e;


template <class K, class T>
BTreeFile<K,T>::BTreeFile (BlockFile &_bf, int order, long sz, int fix) :
	BTree(0, 0, 0),
	SyncBlock (_bf),
	bf (&_bf),
	our_bf(0)
{
	Init (order, sz, fix);
}



template <class K, class T>
BTreeFile<K,T>::BTreeFile (int order, long sz, int fix) :
	BTree(0, 0, 0),
	SyncBlock (*(new BlockFile ("btree.bf", MAGIC
				    /*BlockFile::BF_CREATE*/))),
	bf (SyncBlock::bf),
	our_bf(1)
{
	Init (order, sz, fix);
}



template <class K, class T>
void
BTreeFile<K,T>::Init (int order, long sz, int fix)
{
	Setup (order, sz, fix);
	key_size = 0;
	value_size = sz;
	key_size_fixed = 0;
	value_size_fixed = fix;
	node_size = 0;
	leaf_size = 0;
	log = Logger::For("BTreeFile");

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
			log->Error ("BlockFile has wrong magic number.");
			++err;
			return;
		}
		log->Info (Format("Reading BTreeFile header at (%u,%u)") %
			   header.offset % header.length);
		attach (header);
		readSync ();

		// If we have a root node, it must always be in memory.
		if (depth != -1)
			root = get (rootNode, depth);
	}
	else
	{
		// We're just creating a new tree from scratch.
		bf->WriteLock ();
		writeSync (1);
		log->Info (Format("BTreeFile header allocated at (%u,%u)") %
			   block.offset % block.length);
		bf->setHeader (block, MAGIC);
		bf->Unlock ();
	}
	bf->Unlock ();
}



#ifdef notdef
	key_size(0),
	value_size(sz),
	key_size_fixed(0),
	value_size_fixed(fix),
	node_size(0),
	leaf_size(0),
	log(Logger::For("BTreeFile"))
{
	log->Info (" - Constructing BtreeFile - ");
	//BlockFactory<K,T> *f = new BlockFactory<K,T> (*bf, *this);
	Setup (order, sz, fix/*, new BlockFactory<K,T> (*bf, *this)*/);
	bf->WriteLock ();
	writeSync (1);
	bf->setHeader (this->block, MAGIC);
	bf->Unlock ();
}
#endif


#ifdef notdef
template <class K, class T>
BTree<K,T>::BTree (BlockFile &bf, int _order) :
	factory (new BlockFactory (bf, *this)),
	// persistent
	depth(-1),
	order (_order), 
	root(), 
	// transient
	check(0),
	err(0),
	current (new Shortcut<K,T>)
{ }



template <class K, class T>
BTree<K,T>::BTree (BlockFile &bf, BlkOffset offset) :
	factory (new BlockFactory (bf, offset, *this)),
	// persistent
	depth(-1),
	order (_order), 
	root(), 
	// transient
	check(0),
	err(0),
	current (new Shortcut<K,T>)
{
	if (rootNode.addr)
	{
		root = factory->get (*this, rootNode, depth);
	}
}
#endif


template <class K, class T>
BTreeFile<K,T>::~BTreeFile ()
{
	// Delete any nodes in memory, leaving the tree empty when our
	// superclass destructor is called.
	if (root)
		delete root;
	root = 0;
	bf->Close ();
	if (our_bf)
		delete bf;
	delete log;
}



template <class K, class T>
BTreeNode<K,T> * 
BTreeFile<K,T>::get (Node &node, int depth)
{
	// The simple case is when this node is still in memory
	BlockNode<K,T> *that = (BlockNode<K,T> *)node.local;

	if (! that)
	{
		that = new BlockNode<K,T> (*bf, *this, depth);
		node.local = that;
		that->block.offset = node.addr;
		// Initially set this to the minimum we need to read, but
		// it will be rewritten with the actual allocated size
		// when the node is translated.
		that->block.length = nodeSize (that);
		log->Debug (Format("Recreating node from block (%u,%u)") %
			    that->block.offset % that->block.length);
	}
	that->readSync ();
	return (that);
}



template <class K, class T>
BTreeNode<K,T> * 
BTreeFile<K,T>::make (int depth)
{
	log->Info (Format(" + creating node, depth: %i") % depth);
	BlockNode<K,T> *made = new BlockNode<K,T> (*bf, *this, depth);
	made->thisNode.local = made;
	// Force allocation to get an address
	made->allocate (nodeSize (made));
	assert (made->block.offset > 0);
	made->thisNode.addr = made->block.offset;
	log->Info (Format(" + node created (%u,%u), depth: %i")
		   % made->block.offset % made->block.length % depth);
	return (made);
}


#ifdef notdef
/*
 * When releasing a btree from a file, we want to leave the block 
 * nodes in the file but delete all the nodes in memory.
 */
template <class K, class T>
void
BTreeFile<K,T>::release ()
{
	// Delete all nodes in memory, which we do by deleting the root
	if (root)
		delete root;
	bf->Close ();
	if (our_bf)
		delete bf;
}
#endif


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
	if (bf->WriteLockPending() && lock == 1 && root)
	{
		// Need to tell all nodes in memory to writeSync(), which
		// right now is done by a recursive sync() method.
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



template <class K, class T>
void
BTreeFile<K,T>::translate (SerialStream &ss)
{
	// Translate our persistent state
	ss << depth << order << rootNode;
	ss << key_size << value_size << key_size_fixed << value_size_fixed;
	ss << node_size << leaf_size;
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
void
BTreeFile<K,T>::setValueSize (int vs, int fixed)
{
	enterWrite ();
	if (rootNode.addr == 0)
	{
		value_size = vs;
		value_size_fixed = fixed;
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
BTreeFile<K,T>::valueSize (int *fixed)
{
	enterRead ();
	if (fixed)
		*fixed = value_size_fixed;
	leave ();
	return (value_size);
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
			leave();
		}
		return (node_size);
	}
	if (! leaf_size)
	{
		enterWrite ();
		leaf_size = node->leafSize (*bf->writeBuffer(256));
		leave();
	}
	return (leaf_size);
}



/* ================================================================ */
/* BlockNode methods */



template <class K, class T>
BlockNode<K,T>::~BlockNode ()
{
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
BlockNode<K,T>::prune ()
{
	// Free ourselves from the block file, then call the 
	// superclass method for the rest.
	if (overflow.offset)
	{
		bf->Free (overflow.offset, overflow.length);
	}
	free();
	BTreeNode<K,T>::prune();
}


template <class K, class T>
void
BlockNode<K,T>::sync ()
{
	// Sync our children first, then ourself
	if (depth > 0)
	{
		for (int i = 0; i < nkeys; ++i)
		{
			follow(children[i])->sync ();
		}
	}
	filetree.log->Debug (Printf("Write sync node (%u,%u,%u)", 
			    block.offset, block.length, block.revision));
	writeSync ();
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
	// Figure out how much space we need and get a buffer to encode into.
	unsigned long nspace = filetree.nodeSize (this);
	SerialBuffer *wb = bf->writeBuffer (nspace + overflow.length);
	unsigned long len = encodedSize (*wb);

	// Get enough space to translate the whole node.
	wb->Need (len);

	// If we have not been allocated yet, we need to get our fixed
	// length and allocate at least that much.
	if (! block.length)
	{
		allocate (nspace);
	}

	// Determine whether we need overflow space, and if so allocate or
	// reallocate our overflow block as necessary.
	if (len > nspace)
	{
		if (overflow.offset && (len - nspace > overflow.length))
		{
			// Reallocate
			bf->Free (overflow.offset, overflow.length);
			overflow.offset = overflow.length = 0;
		}

		// Allocate
		overflow.offset = bf->Alloc (len - nspace, &overflow.length);
	}
	else
	{
		// At some point, this would be where we release an
		// overflow block which is no longer needed.
	}

	// Only do the encoding once the overflow state has been set
	translate (*(wb->encodeStream ()));

	// Write our buffer
	bf->Write (block.offset, wb->Peek(0, 0), 
		   (len > nspace) ? nspace : len);
	if (overflow.offset)
	{
		bf->Write (overflow.offset, wb->Peek(nspace, 0),
			   len - nspace);
	}
}


/*
 * Read our node from the block file.
 */
template <class K, class T>
void
BlockNode<K,T>::read ()
{
	// Read the regular block size into a buffer and get our overflow.
	SerialBuffer *rb = bf->readBuffer (block.offset, block.length);
	SerialDecodeStream &ds = *rb->decodeStream ();
	ds >> overflow;

	if (overflow.offset)
	{
		rb->seekEnd ();
		rb->Need (overflow.length);
		bf->Read (sbuf->Peek (), overflow.offset, overflow.length);
	}

	rb->Reset ();
	translate (ds);
}



template <class K, class T>
void
BlockNode<K,T>::translate (SerialStream &ss)
{
	// Need to keep our originally allocated block length in our
	// persistent state so we can free it all when done with it.
	ss << overflow;
	ss << block.length;
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
		ss.opaque (sbuf->getBuffer (), table[nkeys].offset);
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
	cs << block.length;
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
	long vlen = filetree.valueSize (&fixed);
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


