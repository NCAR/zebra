/*
 * $Id: BTree.hh,v 1.12 1998-08-27 22:51:45 granger Exp $
 *
 * Public BTree class interface.
 */

#ifndef _BTree_hh_
#define _BTree_hh_

#include <iostream.h>
// #include "Factory.hh"		// Need the Node class

// Reference to a node by some remote address and size, else by a pointer
// to a cached copy in local memory.  In an entire tree, there is only one
// reference to each node, in its parent.

class SerialStream;

struct Node
{
	void *local;		// Memory cache
	unsigned long addr;	// Persistent address

	Node () : local(0), addr(0) { }

	inline void translate (SerialStream &ss);
};


/*
 * Opaque forward references.
 */
template <class K, class T> class BTreeNode;
template <class K, class T> struct Shortcut;
class BTreeStats;

/// BTree database access class

/*
 * The BTree template based on a key type K and element type T.
 *
 * The KEY type must be comparable and serializable with
 * copy semantics.  The tree expects to keep a private
 * copy of each inserted key.  The value type needs
 * to support the serial-streamable << operator.  It is
 * encoded and/or decoded within the tree into a private
 * buffer.
 */

template <class K, class T> /* , class Factory<K,T> */
class BTree
{
public:
	// Type names

	typedef K key_type;
	typedef T value_type;
	typedef BTreeNode<K,T> node_type;

public:
	static const int DEFAULT_ORDER = 128;

	/// Insert key and value into the tree.

	int Insert (const K &key, const T &value);

	/** Find a key in the tree.  Returns zero if not found, and
	    non-zero otherwise.  Regardless of the result, the search key
	    becomes the current key.  If found and 'value' is non-zero,
	    the key's value will be assigned to 'value'. */

	int Find (const K &key, T *value);

	/** Just check for the existence of a given key and make it
	    the current key.  Return non-zero if the key is found. */

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

	/** Return the key preceding or succeeding the current key.  If key
	    is null, advance the current key but do not return that key.  If
	    value is non-zero, return the value of the new key.  Return 0 if
	    no next or prev key, or if no current key has been set.
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
	inline int Empty ()
	{
		return (depth == -1);
	}

	inline int Order ()
	{
		return (order);
	}

	inline int Depth ()
	{
		return depth;
	}

	inline int elementFixed ()
	{
		return elem_size_fixed;
	};

	inline long elementSize ()
	{
		return elem_size;
	};

	inline long elementSize (int *fixed)
	{
		if (fixed) *fixed = elem_size_fixed;
		return elem_size;
	};

	// Attempt to set the element size and whether that size is constant.
	// This only succeeds when the tree is empty (ie, no root node).
	// Return non-zero when it succeeds.
	//
	virtual int setElementSize (int vs, int fixed = 0);

	void Statistics (BTreeStats &);

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

	// Allow our persistent state to be translated, usually by 
	// a persistent subclass implementation, without actually being
	// Translatable.
	//
	void serial (SerialStream &ss);

	/* ----- Public constructors and destructors ----- */

	/// Create a simple, empty BTree
	BTree (int order = DEFAULT_ORDER, long sz = sizeof(T), int fix = 0);

	virtual void Reopen () { }

	// Explicitly release this tree and all its keys and resources,
	// wherever they may be.
	//
	virtual void Destroy () 
	{
		delete this;
	}

	virtual ~BTree ();

protected:

	// Delayed initialization must be completed with this call.
	void Setup (int order, long sz, int fix);

	// Persistent state
	int depth;			// Depth of the root node (-1 if none)
	int order;
	Node rootNode;			// Root factory address
	int elem_size;			// sizes of elements, or a guess
	int elem_size_fixed;		// non-zero for fixed element sizes

	// Transient state
	BTreeNode<K,T> *root;
	int check;
	int err;

	Shortcut<K,T> *current;		// Reference to current key

	// Change the root node when growing up or down (could be zero)
	void setRoot (BTreeNode<K,T> *node);

	/* ---------------- The pseudo factory interface ---------------- */
	/*
	 * Pseudo because we do not use a separate factory object, so I
	 * suppose these follow the Factory Method pattern.  However,
	 * the methods isolate not just the kind of nodes we need to create,
	 * but also the kind of behavioural extensions different 
	 * factory implementations may need.  I.e., a persistent factory
	 * needs some notification of read and write sync needs, and changes
	 * in the root node.
	 */

	//NodeFactory<K,T> *factory;	// Reference to our node factory

	virtual void enterWrite () {}
	virtual void enterRead () {}
	virtual void leave () {}
	virtual void mark () {}

	/* ---------------- Nodes call into the tree here ---------------- */

	friend BTreeNode<K,T>;

	/// Resurrect a reference to a node
	virtual node_type *get (Node &, int depth);

	/// Create a new node
	virtual node_type *make (int depth);

};


#endif /* _BTree_hh_ */
