/*
 * B-Tree base class implementation.
 */

#include <stddef.h>
#include <stdlib.h>
#include <iostream.h>

//#include <defs.h>
//extern "C" {
//#include <message.h>
//}

// RCSID ("$Id: BTree.cc,v 1.12 1998-05-15 19:36:36 granger Exp $")

#include "Logger.hh"
#include "BTreeP.hh"
//#include "HeapFactory.hh"

// XDR_ADDTYPE (tree_base);



/*================================================================
 * BTree implementation
 *================================================================*/

// const int BTree::DEFAULT_ORDER = 128;



template <class K, class T>
BTree<K,T>::BTree (int _order, long sz, int fix)
{
	Setup (_order, sz, fix /*,new HeapFactory<K,T>()*/);
}


#ifdef notdef
/*
 * For subclasses:
 */
template <class K, class T>
BTree<K,T>::BTree (int _order, long sz, int fix/*, NodeFactory<K,T> *f*/)
{
	Setup (_order, sz, fix/*, f*/);
}
#endif


template <class K, class T>
void
BTree<K,T>::Setup (int _order, long sz, int fix/*, NodeFactory<K,T> *f*/)
{
	depth = -1;
	root = 0;
	check = 0;
	err = 0;
	current = new Shortcut<K,T>;

	order = _order;
	fixed = fix;
	sizes = sz;
	//factory = f;
	if (order < 3)
		order = DEFAULT_ORDER;
}



template <class K, class T>
BTree<K,T>::~BTree ()
{
	release ();
	//delete factory;
	if (current)
		delete current;
}



template <class K, class T>
void
BTree<K,T>::Erase ()
{
	enterWrite ();
	current->clear ();
	if (! Empty())
		root->erase ();
	setRoot (0);
	leave ();
}



template <class K, class T>
int 
BTree<K,T>::Insert (const K &key, const T &value)
{
	enterWrite ();
	current->clear ();
	// Bootstrap the root node if necessary.
	int bootstrap = Empty();
	if (bootstrap)
	{
		root = make (/*depth*/ 0);
		setRoot (root);
	}
	// Find the location and insert
	root->find (key, current);
	int r = current->insert (value);
	if (bootstrap && !r)
	{
		// Enforce legal root of at least one key
		root->erase ();
		setRoot (0);
		current->clear();
	}
	if (current->RootChanged ()) 
		setRoot (current->getRoot());
	if (check) err += Check ();
	leave ();
	return r;
}



template <class K, class T>
int BTree<K,T>::Find (const K &key, T *value)
{
	enterRead ();
	current->clear();
	int found = 0;
	if (! Empty() && root->find (key, current))
		found = current->value (value);
	leave ();
	return found;
}



template <class K, class T>
int BTree<K,T>::Value (T *value)
{
	enterRead ();
	int found = current->value (value);
	leave ();
	return found;
}



/*
 * Removals are handled by first finding the key, which makes it the current
 * key, then removing that key through the shortcut.
 */
template <class K, class T>
int BTree<K,T>::Remove (const K &key)
{
	enterWrite ();
	int done = 0;
	if (Find (key))
		done = Remove();
	leave ();
	return done;
}



template <class K, class T>
int BTree<K,T>::Remove ()
{
	enterWrite ();
	int r = current->remove();
	if (current->RootChanged ()) 
		setRoot (current->getRoot());
	if (check) err += Check ();
	leave ();
	return (r);
}




template <class K, class T>
void
BTree<K,T>::Statistics (BTreeStats &collect)
{
	enterRead ();
	if (! Empty())
	{
		root->stats (collect);
	}
	leave ();
}



template <class K, class T>
int
BTree<K,T>::Check ()
{
	enterRead ();
	int e = 0;
	if (! Empty())
	{
		//cout << "Checking....";
		e = root->check ();
		if (e)
		{
			cout << "***** TREE CHECK FAILED; " 
			     << e << " errors:" << endl;
			Print (cout);
		}
		//cout << endl;
	}
	leave ();
	return (e);
}



template <class K, class T>
int BTree<K,T>::Next (int n, K *key /* = 0*/, T *value /* = 0*/)
{
	enterRead ();
	// If we have a valid current shortcut, use it to find the next key
	// the specified steps away.
	if (current->valid() && n != 0)
	{
		(current->leaf)->step (current, n);
	}
	int done = current->value (value, key);
	leave ();
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
	enterRead ();
	current->clear ();
	int done = 0;
	if (! Empty() && root->findLeft (current))
		done = current->value (value, key);
	leave ();
	return (done);
}



template <class K, class T>
int BTree<K,T>::Last (K *key /* = 0*/, T *value /* = 0*/)
{
	enterRead ();
	current->clear ();
	int done = 0;
	if (! Empty() && root->findRight (current))
		done = current->value (value, key);
	leave ();
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
		rootNode = node->Address();
		depth = node->Depth();
	}
	mark ();
}


/*
 * The default implementation of the btree "factory" interface for nodes.
 * Just pass the call on to the factory with this tree.
 */
template <class K, class T>
BTreeNode<K,T> *
BTree<K,T>::get (Node &node, int /*depth*/)
{
	return ((BTreeNode<K,T> *)node.local);
}


template <class K, class T>
BTreeNode<K,T> *
BTree<K,T>::make (int depth)
{
	return new BTreeNode<K,T> (*this, depth);
}


template <class K, class T>
void
BTree<K,T>::release ()
{
	// For a heap tree we just erase ourself to give up our memory.
	Erase ();
}


#ifdef notdef
/*
 * The default implementation of the btree "factory" interface for nodes.
 * Just pass the call on to the factory with this tree.
 */
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


#ifdef notdef
template <class K, class T>
void
BTree<K,T>::destroy (BTreeNode<K,T> *node)
{
	factory->destroy (node);
}
#endif


template <class K, class T>
void
BTree<K,T>::release ()
{
	factory->release (*this);
}
#endif


/*================================================================
 * BTreeNode implementation
 *================================================================*/

template <class K, class T>
BTreeNode<K,T>::BTreeNode (BTree<K,T> &t, int d) : 
	tree(t), depth(d)
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
		// Set offset for first element insertion
		table[0].offset = 0;
		// Create the element buffer with our best guess for size
		long bufSize = tree.elementSize() * tree.Order();
		if (! bufSize)
		{
			bufSize = tree.Order() << 2;
		}
		else if (! tree.elementFixed())
		{
			// Add a splash factor
			bufSize += bufSize / 2;
		}
		sbuf = new SerialBuffer (bufSize, 0);
	}
	thisNode.local = this;
	thisNode.addr = 0;
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



/*
 * Recursively prune all of this node's children and finally the
 * node itself.
 */
template <class K, class T>
void
BTreeNode<K,T>::erase ()
{
	if (depth > 0)
	{
		for (int i = 0; i < nkeys; ++i)
		{
			follow(children[i])->erase ();
		}
	}
	prune ();
}



template <class K, class T>
ostream &
BTree<K,T>::Print (ostream &out)
{
	if (! Empty())
		root->print (out);
	return (out);
}



