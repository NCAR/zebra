/*
 * $Id: BTree.hh,v 1.2 1997-11-24 18:13:32 granger Exp $
 *
 * Public BTree class interface.
 */

#ifndef _BTree_hh_
#define _BTree_hh_

// #include <stddef.h>
// #include <defs.h>

// #include "BlockStore.hh"
// #include "btree.h"

/*
 * Opaque forward references.
 */
template <class K, class T> class BTreeNode;
template <class K, class T> struct Shortcut;

/// BTree database access class

/* Template based on Key type and element value type. */

template <class K, class T> /* , class Factory<K,T> */
class BTree
{
public:
	// Type names

	typedef K key_type;
	typedef T value_type;

#ifdef notdef
	// Types which are dependent on the factory being used
	// The factory itself must be parameterized for Key and value Type
	typedef F<Key,T> Factory;
	typedef Factory::node_type Node;
	typedef Factory::leaf_type Leaf;
	typedef Factory::Reference Reference;
#endif

public:
	static const int DEFAULT_ORDER = 128;

	/// Insert key and value into the tree.

	int Insert (const K &key, const T &value);

	/** Find a key in the tree.  Returns zero if not found, and the
	 current key remains unchanged.  If found, the value will be
	 assigned to 'value', and the current key becomes the found key. */

	int Find (const K &key, T *value);

	/** Just check for existence of a given key, or attempt to set 
	    the current key to this, but don't return the value. */

	int Find (const K &key)
	{
		return (Find (key, 0));
	}

	/** Get the value of the current key.  Return zero if current
	    key invalid or there is no value. */

	int Value (T *value);

	/** Remove the given key from the tree, returning zero if the
	    key cannot be found. */

	int Remove (const K &key);

	/** Remove the current key from the tree, returning zero if there's
	    a problem. */

	int Remove ();

	/** 
	 Return the key preceding or succeeding the given key.  If key is
	 null, use the key most recently accessed by an insert, find, or
	 remove (the "current key").  Return 0 if no next or prev key,
	 or if key is 0 and no current key has been set. 
	 */
	int Next (K *key = 0, T *value = 0)
	{
		return (Next (1, key, value));
	}
	int Prev (K *key = 0, T *value = 0)
	{
		return (Prev (1, key, value));
	}
	int Current (K *key = 0, T *value = 0);

	/// Find the next key N steps from the current key
	int Next (int n, K *key = 0, T *value = 0);

	/// Find the previous key N steps from the current key
	int Prev (int n, K *key = 0, T *value = 0);

	/// Find the first (least, leftmost) key in the tree
	int First (K *key = 0, T *value = 0);

	/// Find the last (greatest, rightmost) key in the tree
	int Last (K *key = 0, T *value = 0);

	/// Eliminate all keys from this tree
	void Erase ();

	/* 
	 * Return info about this tree.
	 */

	int Empty ()
	{
		return (root == 0);
	}

	int Order ();

	int Depth ();

	int MaxKeys ()
	{ 
		return (Order() - 1);
	};

	/// Print all the nodes of the tree to 'out', indented by depth
	ostream &Print (ostream &out);

	/// Check the tree for consistency; return number of errors
	int Check ();

	/// Enable and disable internal checking
	void Check (int on)
	{
		check = (on != 0);
	}

	int Error ()
	{
		int _err = err;
		err = 0;
		return _err;
	}

	/* ----- Public constructors and destructors ----- */

	/// Create a simple empty BTree

	BTree (int order = DEFAULT_ORDER);

	~BTree ();

protected:

	int order;
	int check;
	int err;

	BTreeNode<K,T> *root;
	Shortcut<K,T> *current;	// Reference to current key
	// BTreeFactory *factory;	// Reference to our node factory

	friend BTreeNode<K,T>;

	// Change the root node when growing up or down (could be zero)
	void setRoot (BTreeNode<K,T> *node);

};


#ifdef notdef
	/// Create a new btree from scratch on the given BlockStore
	BTree (BlockStore &buf, int key_size, 
	       KeyType key_type = KEY_UNKNOWN, int order = DEFAULT_ORDER);

	/**
	  Open an existing b-tree whose base structure is at offset 'base'
	  of the given block store. */
	BTree (BlockStore &buf, BlkOffset base);
#endif


#ifdef notdef

	// Node caching info

	int lru_count;		// LRU count for node access
	int ncache;		// Number of nodes currently in memory
	BTreeNode *cache;	// Linked list of nodes in memory

	int lru () { return (lru_count++); }


	BlkOffset root_node;
	BlockStore *store;

	/// Read a node from storage
	BTreeNode *getNode (BlkOffset, int depth, BTreeBranch *parent = 0);

	/// Create a new node
	BTreeNode *newNode (int depth, BTreeBranch *parent = 0);

#endif

#ifdef notdef

// More intuitive initialization methods

	/// Create a new b-tree
	Create (BlockStore &, KeyType, int order = DEFAULT_ORDER);

	/// Open an existing and verify the key type is as expected
	Open (BlockStore &, KeyType, BlkOffset base);

#endif

#ifdef notdef
// These are the virtual routines which must be provided by the subclass
// to handle a specific key type.

	/// Given list of n keys, return index <= key target
	virtual int Search (const Key *list, int n, const Key *target,
			    int *i = 0) = 0;

	/// The base class implementation relies on key_size
	virtual void CopyKey (Key *dest, int n, Key *source)
	{
		memcpy ((char *)dest + (n*key_size), source, key_size);
	}
#endif

#ifdef notdef
	/// Return estimated element size in bytes
	int ElementSize ();

	/* Set info.  The storage object and tree order cannot be changed.
	 * The element size is used as a default length when inserting
	 * a key and value, and it is used to estimate the space required
	 * for a block of N elements for a leaf.
	 */
	void SetElementSize (int size);

	int keySize (void)
	{
		return (key_size);
	};
#endif

#ifdef notdef

	/// Return the BlockStore on which the btree nodes are stored.

	BlockStore *Storage ()
	{ 
		return (store);
	};

#endif

#endif /* _BTree_hh_ */
