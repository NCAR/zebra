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

// RCSID ("$Id: BTreeFile.cc,v 1.1 1998-05-15 19:36:38 granger Exp $")

#include "Logger.hh"
#include "Format.hh"
#include "BTreeFile.hh"
#include "BlockFactory.hh"

#include "Serialize.hh"

SERIAL_STREAMABLE (Node);


template <class K, class T>
BTreeFile<K,T>::BTreeFile (BlockFile &_bf, int order, long sz, int fix) :
	BTree(0, 0, 0),
	SyncBlock (_bf),
	bf (&_bf),
	key_size(0),
	value_size(sz),
	key_size_fixed(0),
	value_size_fixed(fix),
	our_bf(0),
	node_size(0),
	leaf_size(0),
	log(new Logger("BTreeFile"))
{
	//BlockFactory<K,T> *f = new BlockFactory<K,T> (_bf, *this);
	Setup (order, sz, fix/*, new BlockFactory<K,T> (_bf, *this)*/);

	// Now that we are initialized, we must allocate ourself on disk
	// and note our location in the header.
	writeSync (1);
	bf->setHeader (this->block);
}


template <class K, class T>
const unsigned long BTreeFile<K,T>::BTREEFILE_MAGIC = 0x12481632;


template <class K, class T>
BTreeFile<K,T>::BTreeFile (int order, long sz, int fix) :
	BTree(0, 0, 0),
	SyncBlock (*(new BlockFile ("btree.bf", BTREEFILE_MAGIC,
				    BlockFile::BF_CREATE))),
	bf (SyncBlock::bf),
	key_size(0),
	value_size(sz),
	key_size_fixed(0),
	value_size_fixed(fix),
	our_bf(1),
	node_size(0),
	leaf_size(0),
	log(new Logger("BTreeFile"))
{
	log->Info (" - Constructing BtreeFile - ");
	//BlockFactory<K,T> *f = new BlockFactory<K,T> (*bf, *this);
	Setup (order, sz, fix/*, new BlockFactory<K,T> (*bf, *this)*/);
	bf->WriteLock ();
	writeSync (1);
	bf->setHeader (this->block);
	bf->Unlock ();
}



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
	// Do nothing here and instead act when release() is called.
}



template <class K, class T>
BTreeNode<K,T> * 
BTreeFile<K,T>::get (Node &node, int depth)
{
	// The simple case is when this node is still in memory
	assert (node.local);
	BlockNode<K,T> *that = (BlockNode<K,T> *)node.local;
	return (that);

	if (! that)
	{
		that = new BlockNode<K,T> (*bf, *this, depth);
		node.local = that;
	}
	that->block.offset = node.addr;
	that->block.length = nodeSize (that);
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
	made->thisNode.addr = block.offset;
	log->Info (Format(" + node created, depth: %i") % depth);
	return (made);
}


/*
 * When releasing a btree from a file, we want to leave the block 
 * nodes in the file but delete all the nodes in memory.
 */
template <class K, class T>
void
BTreeFile<K,T>::release ()
{
	// For now just erase the tree
	Erase ();
	bf->Close ();
	if (our_bf)
		delete bf;
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
	if (bf->WriteLockPending() && root)
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
	writeSync ();
}


/*
 * Write our node to the block file, possibly using an overflow block.
 * When this method is called, our superclass syncblock methods have
 * already determined and allocated our block size.  All that remains is
 * allocating an overflow block if necessary, encoding into a sufficiently
 * large buffer, and writing that buffer to the file.
 */
template <class K, class T>
void
BlockNode<K,T>::write ()
{
	// Figure out how much space we need and get a buffer to encode into.
	SerialBuffer *wb = bf->writeBuffer (block.length);
	unsigned long len = encodedSize (*wb);

	// Get enough space to translate the whole node.
	wb->Need (len);

	// If we have not been allocated yet, we need to get our fixed
	// length and allocate that much.
	if (! block.length)
	{
		allocate (filetree.nodeSize (this));
	}

	// Determine whether we need overflow space, and if so allocate or
	// reallocate our overflow block as necessary.
	if (len > block.length)
	{
		if (overflow.offset && (len - block.length > overflow.length))
		{
			// Reallocate
			bf->Free (overflow.offset, overflow.length);
			overflow.offset = overflow.length = 0;
		}

		// Allocate
		overflow.offset = bf->Alloc (len - block.length, 
					     &overflow.length);
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
		   (len > block.length) ? block.length : len);
	if (overflow.offset)
	{
		bf->Write (overflow.offset, wb->Peek(block.length, 0),
			   len - block.length);
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
	ss << overflow;
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


