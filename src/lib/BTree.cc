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

RCSID ("$Id: BTree.cc,v 1.1 1997-11-24 10:24:20 granger Exp $")

#include "Logger.hh"
#include "BTree.hh"
#include "BTreeP.hh"

// XDR_ADDTYPE (tree_base);


#ifdef notdef
/*
 * Interface to objects which will create, store, and manage our nodes
 * for us.
 */
template <class BT, 
class BTreeFactory
{
	typedef BTreeNode *address_type;

	/// Retrieve an existing node

	BTreeNode *
	get (address_type &addr, int depth, BTreeBranch *parent = 0);

	/// Create a new node

	BTreeNode *
	make (address_type &addr, int depth, BTreeBranch *parent = 0);

	/// Delete a node.  Can also be done with delete operator.

	void delete (address_type &addr);

	/// Destructor for the factory, which deletes nodes in memory
	
	virtual ~BTreeFactory ();

	BTreeFactory (BTree &t)
	{
		tree = t;
	}

protected:
	BTree &tree;		// The tree we serve

}


inline BTreeNode *
BTreeFactory::get (address_type &addr, int depth, BTreeBranch *parent)
{
	addr = new BTreeNode (&tree, depth, parent);
	return (addr);
}


inline BTreeNode *
BTreeFactory::make (address_type &addr, int depth, BTreeBranch *parent)
{
	addr = new BTreeNode (&tree, depth, parent);

	return (node);


}	

inline void
BTreeFactory::delete (address_type &addr)
{

}

	/// Destructor for the factory, which deletes nodes in memory
	
	virtual ~BTreeFactory ();
#endif



/*================================================================
 * BTree implementation
 *================================================================*/

// const int BTree::DEFAULT_ORDER = 128;

template <class K, class T>
BTree<K,T>::BTree (int _order)
{
	order = _order;
	if (order < 3)
		order = DEFAULT_ORDER;
	root = 0;
	check = 0;
	current = new Shortcut<K,T>;
}



template <class K, class T>
BTree<K,T>::~BTree ()
{
	Erase ();
	if (current)
		delete current;
}



template <class K, class T>
void
BTree<K,T>::Erase ()
{
	if (root)
		root->destroy ();
	root = 0;
}



template <class K, class T>
int 
BTree<K,T>::Insert (const K &key, const T &value)
{
	current->clear ();
	// Bootstrap the root node if necessary.  Every node must
	// have at least one key in it.
	if (! root)
	{
		root = new BTreeNode<K,T> (*this, /*depth*/ 0);
		current->set (root, key, 0);
		if (! current->insert (value))
		{
			delete root;
			root = 0;
			current->clear();
			return 0;
		}
		return 1;
	}

	// Find the location and insert
	root->find (key, current);
	int r = current->insert (value);
	Check ();
	return r;
}



template <class K, class T>
int BTree<K,T>::Find (const K &key, T *value)
{
	if (! root)
		return (0);

	current->clear();
	if (root->find (key, current))
		return (current->value (value));
	else
		return (0);
}



template <class K, class T>
int BTree<K,T>::Value (T *value)
{
	return (current->value (value));
}



/*
 * Removals are handled by first finding the key, which makes it the current
 * key, then removing that key through the shortcut.
 */
template <class K, class T>
int BTree<K,T>::Remove (const K &key)
{
	if (! Find (key))
		return (0);
	return (Remove ());
}



template <class K, class T>
int BTree<K,T>::Remove ()
{
	int r = current->remove();
	Check ();
	return (r);
}



template <class K, class T>
void
BTree<K,T>::setRoot (BTreeNode<K,T> *node)
{
	root = node;
}



template <class K, class T>
int
BTree<K,T>::Check ()
{
	int e = 0;
	if (root)
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
	check += e;
	return (e);
}



template <class K, class T>
int BTree<K,T>::Next (K *key /* = 0*/, T *value /* = 0*/)
{
	// If we have a valid current shortcut, use it to
	// find the next key by traversing to its right.
	if (current->valid())
	{
		(current->leaf)->right (current, current->key);
	}
	return (current->value (value, key));
}



template <class K, class T>
int BTree<K,T>::Prev (K *key /* = 0*/, T *value /* = 0*/)
{
	// If we have a valid current shortcut, use it to
	// find the previous key by traversing to its left.
	if (current->valid())
	{
		(current->leaf)->left (current, current->key);
	}
	return (current->value (value, key));
}



template <class K, class T>
int BTree<K,T>::Current (K *key /* = 0*/, T *value /* = 0*/)
{
	return (current->value (value, key));
}



template <class K, class T>
int BTree<K,T>::First (K *key /* = 0*/, T *value /* = 0*/)
{
	if (! root)
		return 0;
	current->clear ();
	if (root->findLeft (current))
		return (current->value (value, key));
	return (0);
}



template <class K, class T>
int BTree<K,T>::Last (K *key /* = 0*/, T *value /* = 0*/)
{
	if (! root)
		return 0;
	current->clear ();
	if (root->findRight (current))
		return (current->value (value, key));
	return (0);
}



/*================================================================
 * BTreeNode implementation
 *================================================================*/

template <class K, class T>
BTreeNode<K,T>::BTreeNode (BTree<K,T> &t, int d) : tree(t), depth(d)
{
	// Parent info, if any, gets set by setParent() when we
	// get inserted into a parent node.
	// parent = 0;

	children = 0;
	table = 0;
	sbuf = 0;
	keys = new K[tree.MaxKeys()];
	nkeys = 0;
	if (depth > 0)	// we're an internal node which only has children
	{
		children = new (BTreeNode<K,T> *)[tree.Order()];
	}
	else	// we're a leaf which needs an element offset table
	{
		table = new Element[tree.MaxKeys()+1];
		table[nkeys].offset = 0; // Offset for first element insertion
		sbuf = new SerialBuffer;
	}
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
		delete this;
		return;
	}

	for (int i = 0; i <= nkeys; ++i)
	{
		children[i]->destroy ();
	}

	delete this;
}



#ifdef notdef
/*
 * Create a new b-tree on the given BlockStore
 */
BTree::BTree (BlockStore &buf, int key_size, KeyType key_type = KEY_UNKNOWN,
	      int order = DEFAULT_ORDER)
{
	BTree();
	this->key_size = key_size;
	base->key_type = key_type;
	base->order = order;
	store = &buf;
	root = new BTreeLeaf (this);
	base->root = root->Block();
}



/* 
 * Open an existing b-tree whose base structure is at offset 'base'
 */
BTree::BTree (BlockStore &buf, BlkOffset addr)
{
	/* initialize everything and allocate a base structure */
	BTree();

	/* read the base */
	XDRSream *xdr = buf.decodeStream (addr);
	xdr >> base;

	/* restore the root */
	root = getNode (base->root, base->depth);
}


/* ----------------
 * Simple access which hides the tree_base structure from public view.
 */

KeyType
BTree::keyType ()
{ 
	return (base->key_type);
}

// Return estimated element size in bytes
int
BTree::ElementSize ()
{ 
	return (base->elem_size);
}

/* Set info.  The storage object and tree order cannot be changed.
 * The element size is used as a default length when inserting
 * a key and value, and it is used to estimate the space required
 * for a block of N elements for a leaf.
 */
void
BTree::SetElementSize (int size)
{ 
	base->elem_size = size;
}
#endif

template <class K, class T>
int
BTree<K,T>::Order ()
{
	return (order);
}



template <class K, class T>
int
BTree<K,T>::Depth ()
{ 
	return (root ? root->Depth() : 0);
}



template <class K, class T>
ostream &
BTree<K,T>::Print (ostream &out)
{
	if (root)
		root->print (out);
	return (out);
}

#ifdef notdef
/* ---------------- 
 * Retrieve nodes from the block store.  These are BTree instance methods
 * because the request for a new node may be filled either by allocating
 * a new node or by replacing an existing node on the cache linked list.
 */

/*
 * Restore an existing btree node from the block store.
 */
BTreeNode *
BTree::getNode (BlkOffset addr, int depth, BTreeBranch *parent = 0)
{
	BTreeNode *node;

	/* Check whether we've reached our limit... */

	node = new BTreeNode (this, addr, depth, parent);

	return (node);
}



/*
 * Actually create a whole new node for the b-tree, allocating it
 * space in the block store.
 */
BTreeNode *
BTree::newNode (int depth, BTreeBranch *parent = 0)
{
	BTreeNode *node = new BTreeNode (this, depth, parent);

	return (node);
}



/* ================================================================ */


int
BTree::Find (const Key *key, void *value = NULL, int *length = 0)
{
	// Recursively search the tree for this key, beginning with the root 

	int i = root->find (key, value, length);

	// Store the current node and key shortcut here, somehow

	return (i >= 0);
}




int
BTree::Insert (const Key *key, void *value = NULL, int length = 0)
{
	// Call insert on the root node and let it take it from there

	int i = root->insert (key, value, length);

	// Store shortcut to current node here

	return (i >= 0);
}
#endif

/* ================================================================ */

#ifdef notdef
InsertKey (TreeNode *parent, TreeNode *node, Key *key, recptr loc)
/*
 * Insert the given key with the given location into the node.
 * First look for the leaf node which should contain this key,
 * then insert using overflow, possibly resulting in the split
 * of the root node.  If parent is NULL, the given node is a root
 * node.
 */
{
	TreeNode *leaf;

	leaf = FindLeaf (node, key);
	/*
	 * Simple case: node is not full, so insert the new key
	 */
	if (leaf->nkeys < TREE_ORDER

}


NodeInsert (TreeNode *node, int slot, Key *key, recptr loc)
/*
 * Given that there is room in this node for another key, insert
 * the key and its associated pointer in the slot.
 */
{
	int i;

	/*
	 * Open up a hole for the key and pointer.
	 */
	for (i = node->nkeys+1; i > slot ; --i)
	{
		node->ptr[i] = node->ptr[i-1];
		memcpy (KeyN (node, i), KeyN (node, i-1), node->key_size);
	}
	node->ptr[slot] = loc;
	memcpy (KeyN(node, slot), key);
}



/*
 * Keys are "numbered" from 1 to Nkeys, which is actually array
 * indices 0..(Nkeys-1)
 */
inline Key *
KeyN (TreeNode *node, int i)
{
	return ((Key *)((char *)node->keys + ((i-1)*(node->key_size))));
}



#endif
