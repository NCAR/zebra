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

RCSID ("$Id: BTree.cc,v 1.11 1998-03-16 20:56:39 granger Exp $")

#include "Logger.hh"
#include "BTreeP.hh"
#include "HeapFactory.hh"

// XDR_ADDTYPE (tree_base);



/*================================================================
 * BTree implementation
 *================================================================*/

// const int BTree::DEFAULT_ORDER = 128;

template <class K, class T>
BTree<K,T>::BTree (int _order, long sz, int fix) :
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
	current (new Shortcut<K,T>),
	factory(new HeapFactory<K,T>())
{
	if (order < 3)
		order = DEFAULT_ORDER;
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
	enterWrite ();
	current->clear ();
	if (! Empty())
		root->destroy ();
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
		root = factory->make (*this, /*depth*/ 0);
		setRoot (root);
	}
	// Find the location and insert
	root->find (key, current);
	int r = current->insert (value);
	if (bootstrap && !r)
	{
		// Enforce legal root of at least one key
		root->destroy ();
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
			root->print (cout);
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



#ifdef notdef
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
#endif


/*================================================================
 * BTreeNode implementation
 *================================================================*/

template <class K, class T>
BTreeNode<K,T>::BTreeNode (NodeFactory<K,T> &f, BTree<K,T> &t, int d) : 
	factory(f), tree(t), depth(d)
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



template <class K, class T>
void
BTreeNode<K,T>::destroy ()
{
	if (depth == 0)
	{
		factory.destroy (this);
		return;
	}

	for (int i = 0; i < nkeys; ++i)
	{
		follow(children[i])->destroy ();
	}

	factory.destroy (this);
}



template <class K, class T>
ostream &
BTree<K,T>::Print (ostream &out)
{
	if (! Empty())
		root->print (out);
	return (out);
}



