/*
 * $Id: BTree.hh,v 1.20 2002-09-17 20:00:18 granger Exp $
 *
 * Public BTree class interface.
 */

#ifndef _BTree_hh_
#define _BTree_hh_

#include <iostream>

// Reference to a node by some remote address and size, else by a pointer
// to a cached copy in local memory.  In an entire tree, there is only one
// reference to each node, in its parent.

class SerialStream;

/*
 * Opaque forward references.  */
template <class K, class T> class BTree;
template <class K, class T> class BTreeNode;
template <class K, class T> struct Shortcut;
class BTreeStats;


/*
 * Effectively a namespace to hide types which the class definitions
 * need but do not need to be public.  This gets over problems in 
 * Sun's CC with nested classes.
 */
class BTreeP
{
public:
	class Node
	{
	public:
		//BTreeNode<K,T> *local;	// Memory cache
		void *local;		// Memory cache
		unsigned long addr;	// Persistent address

		Node () : local(0), addr(0) { }
		inline void translate (SerialStream &ss);
	};

	/*
	 * Statistics we care about.
	 */
	class Stats
	{
	public:
		unsigned long nNodes;
		unsigned long nKeys;
		unsigned long nLeaves;
		void translate (SerialStream &ss);
		void reset () { nNodes = nKeys = nLeaves = 0; }
	        std::ostream &report (std::ostream &out) const;
		Stats() { reset(); }
	};
};


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

template <class K, class T>
class BTree
{
public:
	// Type names

	typedef K key_type;
	typedef T value_type;
	typedef BTreeNode<K,T> node_type;

	typedef BTreeP::Stats Stats;

public:
	enum { DEFAULT_ORDER = 128 };

	/// Insert key and value into the tree.

	int Insert (const K &key, const T &value);

	/** Find a key in the tree.  Returns zero if not found, and
	    non-zero otherwise.  Regardless of the result, the search key
	    becomes the current key.  If found and 'value' is non-zero,
	    the key's value will be assigned to 'value'. */

	int Find (const K &key, T *value);

	/** Just check for the existence of a given key and make it
	    the current key.  Return non-zero if the key is found. 
	    See Current(). */

	int Find (const K &key)
	{
		return (Find (key, 0));
	}

	/** Get the value of the current key.  Return zero if current
	    key invalid or there is no value.  See Current(). */

	int Value (T *value);

	/** Remove the given key from the tree, returning zero if the
	    key cannot be found. */

	int Remove (const K &key);

	/** Remove the current key from the tree, returning zero if there's
	    a problem.  See Current(). */

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

        /** The cursor, or "current key", is valid after every call to
	    Find(), even if the Find() fails to find the search key.  The
	    search key becomes the "current key".  When the cursor is
	    valid, Current() returns nonzero and Current (K* key) returns
	    the current key.  If the current key is not actually in the
	    BTree, then any call to Current() or Value() to get the value
	    of the current key will fail and return 0.  Once the current
	    key has been set, the current key can be advanced with the Next
	    and Prev methods.  The "current key" is like a database row
	    cursor. */

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
	inline int Empty () const
	{
		return (depth == -1);
	}

	inline int Order () const
	{
		return (order);
	}

	inline int Depth () const
	{
		return depth;
	}

	inline unsigned long numKeys () const
	{
		return stats.nKeys;
	}

	inline int elementFixed () const
	{
		return elem_size_fixed;
	};

	inline long elementSize () const
	{
		return elem_size;
	};

	inline long elementSize (int *fixed) const
	{
		if (fixed) *fixed = elem_size_fixed;
		return elem_size;
	};

	// Attempt to set the element size and whether that size is constant.
	// This only succeeds when the tree is empty (ie, no root node).
	// Return non-zero when it succeeds.
	//
	virtual int setElementSize (int vs, int fixed = 0);

	// Actually traverses the tree collecting all kinds of statistics,
	// so the time this method takes depends on tree size.
	virtual void collectStats (BTreeStats &);

	// Return the known and immediately available tree statistics.
	void currentStats (BTreeP::Stats &s) const
	{
		s = stats;
	}

        std::ostream &reportStats (std::ostream &out, 
				   const BTreeP::Stats &s) const;

	virtual std::ostream &reportStats (std::ostream &out) const
	{
		return reportStats (out, stats);
	}

	/// Print all the nodes of the tree to 'out', indented by depth
	std::ostream &Print (std::ostream &out);

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

	/// Create a simple, empty BTree
	BTree (int order = DEFAULT_ORDER, long sz = sizeof(T), int fix = 0);

	virtual void Reopen () { }

	// Explicitly release all keys and resources of this tree,
	// wherever they may be.  The BTree object itself may still
	// need to be deleted.
	//
	virtual void Destroy ();

	virtual ~BTree ();

protected:

	typedef BTreeP::Node Node;

	// Allow our persistent state to be translated, usually by 
	// a persistent subclass implementation, without actually being
	// Translatable.
	//
	void translate (SerialStream &ss);

	// Delayed initialization must be completed with this call.
	void Setup (int order, long sz, int fix);

	// Persistent state
	int depth;			// Depth of the root node (-1 if none)
	int order;
	Node rootNode;			// Root factory address
	int elem_size;			// sizes of elements, or a guess
	int elem_size_fixed;		// non-zero for fixed element sizes
	Stats stats;

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

	virtual void enterWrite () {}
	virtual void enterRead () {}
	virtual void leave () {}
	virtual void mark () {}

	/* ---------------- Nodes call into the tree here ---------------- */

	friend class BTreeNode<K,T>;

	/// Resurrect a reference to a node
	virtual node_type *get (Node &, int depth);

	/// Create a new node
	virtual node_type *make (int depth);

};


#endif /* _BTree_hh_ */
