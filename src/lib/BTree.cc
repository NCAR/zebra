/*
 * B-Tree base class implementation.
 */

#include <stddef.h>
#include <stdlib.h>
#include <iostream.h>

#include <defs.h>
extern "C" {
#include <message.h>
}

RCSID ("$Id: BTree.cc,v 1.8 1997-12-29 07:15:26 granger Exp $")

#include "Logger.hh"
#include "BTreeP.hh"
#include "HeapFactory.hh"

// XDR_ADDTYPE (tree_base);



/*================================================================
 * BTree implementation
 *================================================================*/

// const int BTree::DEFAULT_ORDER = 128;

template <class K, class T>
BTree<K,T>::BTree (int _order, long sz = 0, int fix = 0) :
	factory(new HeapFactory<K,T>()), 
	// persistent
	depth(-1),
	order (_order), 
	rootNode (),
	fixed(fix),
	sizes(sz),
	// transient
	root(0), 
	check(0),
	err(0),
	current (new Shortcut<K,T>)
{
	if (order < 3)
		order = DEFAULT_ORDER;
	//factory = new HeapFactory<K,T>();

	// Set our best guess for initial serial buffer size
	bufSize = sizes * order;
	if (! bufSize)
	{
		bufSize = order << 2;
	}
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
{ }
#endif



template <class K, class T>
BTree<K,T>::~BTree ()
{
	factory->release (*this);
	delete factory;
	if (current)
		delete current;
}



template <class K, class T>
void
BTree<K,T>::Erase ()
{
	factory->writeLock ();
	current->clear ();
	if (! Empty())
		root->destroy ();
	setRoot (0);
	factory->unlock ();
}



template <class K, class T>
int 
BTree<K,T>::Insert (const K &key, const T &value)
{
	factory->writeLock ();
	current->clear ();
	// Bootstrap the root node if necessary.  Every node must
	// have at least one key in it.
	if (Empty())
	{
		root = factory->make (*this, /*depth*/ 0);
		setRoot (root);
		current->set (root, key, 0);
		if (! current->insert (value))
		{
			factory->destroy (root);
			setRoot (0);
			current->clear();
			return 0;
		}
		return 1;
	}

	// Find the location and insert
	root->find (key, current);
	int r = current->insert (value);
	if (check) err += Check ();
	factory->unlock ();
	return r;
}



template <class K, class T>
int BTree<K,T>::Find (const K &key, T *value)
{
	factory->readLock ();
	current->clear();
	int found = 0;
	if (! Empty() && root->find (key, current))
		found = current->value (value);
	factory->unlock ();
	return found;
}



template <class K, class T>
int BTree<K,T>::Value (T *value)
{
	factory->readLock ();
	int found = current->value (value);
	factory->unlock ();
	return found;
}



/*
 * Removals are handled by first finding the key, which makes it the current
 * key, then removing that key through the shortcut.
 */
template <class K, class T>
int BTree<K,T>::Remove (const K &key)
{
	factory->writeLock ();
	int done = 0;
	if (Find (key))
		done = Remove();
	factory->unlock ();
	return done;
}



template <class K, class T>
int BTree<K,T>::Remove ()
{
	factory->writeLock ();
	int r = current->remove();
	if (check) err += Check ();
	factory->unlock ();
	return (r);
}




template <class K, class T>
BTreeStats
BTree<K,T>::Statistics ()
{
	BTreeStats collect;
	factory->readLock ();
	if (! Empty())
	{
		root->stats (collect);
	}
	factory->unlock ();
	return (collect);
}



template <class K, class T>
int
BTree<K,T>::Check ()
{
	factory->readLock ();
	int e = 0;
	if (! Empty())
	{
		//cout << "Checking....";
		e = root->check ();
		if (e)
		{
			cout << "***** TREE CHECK FAILED; " 
			     << e << " errors:" << endl;
			root->print (cout);
		}
		//cout << endl;
	}
	factory->unlock ();
	return (e);
}



template <class K, class T>
int BTree<K,T>::Next (int n, K *key /* = 0*/, T *value /* = 0*/)
{
	factory->readLock ();
	// If we have a valid current shortcut, use it to find the next key
	// the specified steps away.
	if (current->valid() && n != 0)
	{
		(current->leaf)->step (current, n);
	}
	int done = current->value (value, key);
	factory->unlock ();
	return done;
}



template <class K, class T>
int BTree<K,T>::Prev (int n, K *key /* = 0*/, T *value /* = 0*/)
{
	return (Next (-n, key, value));
}



template <class K, class T>
int BTree<K,T>::Current (K *key /* = 0*/, T *value /* = 0*/)
{
	return (Next (0, key, value));
}



template <class K, class T>
int BTree<K,T>::First (K *key /* = 0*/, T *value /* = 0*/)
{
	factory->readLock ();
	current->clear ();
	int done = 0;
	if (! Empty() && root->findLeft (current))
		done = current->value (value, key);
	factory->unlock ();
	return (done);
}



template <class K, class T>
int BTree<K,T>::Last (K *key /* = 0*/, T *value /* = 0*/)
{
	factory->readLock ();
	current->clear ();
	int done = 0;
	if (! Empty() && root->findRight (current))
		done = current->value (value, key);
	factory->unlock ();
	return (done);
}



/* ---------------- Protected methods ---------------- */

template <class K, class T>
void
BTree<K,T>::setRoot (BTreeNode<K,T> *node)
{
	root = node;
	if (! node)
	{
		rootNode.addr = 0;
		depth = -1;	// Empty again
	}
	else
	{
		rootNode = node->thisNode;
		depth = node->Depth();
	}
}


template <class K, class T>
BTreeNode<K,T> *
BTree<K,T>::get (Node &node, int depth)
{
	return factory->get (*this, node, depth);
}


template <class K, class T>
BTreeNode<K,T> *
BTree<K,T>::make (int depth)
{
	return factory->make (*this, depth);
}


template <class K, class T>
void
BTree<K,T>::destroy (BTreeNode<K,T> *node)
{
	factory->destroy (node);
}


/*================================================================
 * BTreeNode implementation
 *================================================================*/

template <class K, class T>
BTreeNode<K,T>::BTreeNode (BTree<K,T> &t, int d) : tree(t), depth(d)
{
	int maxkeys = tree.Order();
	children = 0;
	table = 0;
	sbuf = 0;
	keys = new K[maxkeys];
	nkeys = 0;
	if (depth > 0)	// we're an internal node which only has children
	{
		children = new Node[maxkeys];
	}
	else	// we're a leaf which needs an element offset table
	{
		table = new Element[maxkeys+1];
		table[0].offset = 0; // Offset for first element insertion
		sbuf = new SerialBuffer (tree.bufSize, 0);
	}
}



template <class K, class T>
void
BTreeNode<K,T>::translate (SerialStream &ss)
{
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
		sbuf->Need (table[nkeys].offset);
		ss->opaque (sbuf->getBuffer (), table[nkeys].offset);
	}
}



template <class K, class T>
long
BTreeNode<K,T>::blockSize (SerialBuffer &sbuf)
{
	int keysize = serialCount (sbuf, keys[0]);
	long s = serialCount (sbuf, nkeys);	// nkeys
	int maxkeys = tree.MaxKeys();
	s += maxkeys * keysize;			// keys
	if (depth == 0)
	{
		// Element table
		s += maxkeys * serialCount (table[0]);
	}
	else
	{
		// Children array
		s += tree.Order() * serialCount (children[0]);
	}
	return (s);
}



template <class K, class T>
BTreeNode<K,T>::~BTreeNode ()
{
	if (keys)
		delete[] keys;
	if (children)
		delete[] children;
	if (table)
		delete[] table;
	if (sbuf)
		delete sbuf;
}



template <class K, class T>
void
BTreeNode<K,T>::destroy ()
{
	if (depth == 0)
	{
		tree.factory->destroy (this);
		return;
	}

	for (int i = 0; i <= nkeys; ++i)
	{
		down(children[i])->destroy ();
	}

	tree.factory->destroy (this);
}



#ifdef notdef
template <class K, class T>
int
BTree<K,T>::Order ()
{
	return (order);
}
#endif


#ifdef notdef
template <class K, class T>
int
BTree<K,T>::Depth ()
{ 
	return (root ? root->Depth() : 0);
}
#endif


template <class K, class T>
ostream &
BTree<K,T>::Print (ostream &out)
{
	if (! Empty())
		root->print (out);
	return (out);
}



template <class K, class T>
void
BTree<K,T>::translate (SerialStream &ss)
{
	// Translate our persistent state
	ss << depth << order << rootNode;
}



