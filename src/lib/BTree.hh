/*
 * $Id: BTree.hh,v 1.7 1998-03-04 17:08:01 granger Exp $
 *
 * Public BTree class interface.
 */

#ifndef _BTree_hh_
#define _BTree_hh_

#ifndef _BTreeStats_
#define _BTreeStats_
/*
 * Class to accumulate btree statistics for all of its nodes.
 */
struct BTreeStats
{
	BTreeStats () :
		nkeys(0),
		keysopen(0),
		nodeused(0),
		nodealloc(0),
		elemused(0),
		elemalloc(0),
		nbranch(0),
		nleaves(0),
		nelements(0),
		minkeys(0),
		maxkeys(0)
	{ }

	long nkeys;		// Number of keys in use
	long keysopen;		// Number of key slots unused
	long nodeused;		// Bytes used by nodes
	long nodealloc;		// Bytes allocated by nodes
	long elemused;		// Bytes used in element buffers
	long elemalloc;		// Bytes allocated for element buffers
	long nbranch;		// Number of branch nodes
	long nleaves;		// Number of leaves
	long nelements;		// Number of elements stored in leaves
	long minkeys;		// Minimum keys in any node besides root
	long maxkeys;		// Maximum keys in any node

	// Public methods to derive common statistics

	inline long memoryAlloc ()		// total space allocated
	{
		return elemalloc + nodealloc;
	}
	inline long memoryUsed ()		// total space used
	{
		return elemused + nodeused;
	}
	inline long numNodes ()
	{
		return nbranch + nleaves;
	}
	inline long numLeaves ()
	{
		return nleaves;
	}
	inline long totalSlots ()		// total number of key slots
	{
		return nkeys + keysopen;
	}
	inline float averageKeys ()
	{
		return (float)nkeys / numNodes();
	}
	inline long maxKeys ()
	{
		return maxkeys;
	}
	inline long minKeys ()
	{
		return minkeys;
	}
	inline long keysUnused ()
	{
		return keysopen;
	}
	inline long numKeys ()
	{
		return nkeys;
	}
	inline long elementsFree ()
	{
		return elemalloc - elemused;
	}
	inline long numElements ()
	{
		return nelements;
	}
	// memory usage as a percent of all allocated memory
	inline float percentMemory ()
	{
		return (float)memoryUsed() / (float)memoryAlloc() * 100.0;
	}
	// node memory usage as a percent of memory allocated by nodes
	inline float percentNodeMemory ()
	{
		return (float)nodeused / (float)nodealloc * 100.0;
	}
	// element buffer usage as a percent of all element buffer space
	inline float percentElementMemory ()
	{
		return (float)elemused / (float)elemalloc * 100.0;
	}
	inline float percentSlots ()
	{
		return (float)numKeys() / totalSlots() * 100.0;
	}
};
#endif /* _BTreeStats_ */


#include "Factory.hh"

/*
 * Opaque forward references.
 */
template <class K, class T> class BTreeNode;
template <class K, class T> struct Shortcut;

/// BTree database access class

/*
 * The BTree template based on a key type K and element type T.
 *
 * The KEY type must be comparable and serializable with
 * copy semantics.  The tree expects to keep a private
 * copy of each inserted key.  The value type needs
 * to support the serial-streamable << operator.  It is
 * encoded and/or decoded within the tree into private
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
		return (fixed);
	};

	inline long elementSize ()
	{ 
		return sizes;
	};

	BTreeStats Statistics ();

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
	BTree (int order = DEFAULT_ORDER, long sz = 0, int fix = 0);

	/// Create an empty, persistent BTree on the given BlockFile
	//BTree (BlockFile &bf, int order = DEFAULT_ORDER);

	/// Restore a BTree from a BlockFile at the given address
	//BTree (BlockFile &bf, BlkOffset offset);

	virtual ~BTree ();

	void translate (SerialStream &ss);

	// Return the fixed sizes of our nodes
	//long leafSize ();
	//long nodeSize ();

protected:

	NodeFactory<K,T> *factory;	// Reference to our node factory

	// Persistent state
	int depth;
	int order;
	Node rootNode;
	int fixed;			// non-zero for fixed element sizes
	long sizes;			// sizes of elements, zero if unknown
	long bufSize;			// Set at construction

	// Transient state
	BTreeNode<K,T> *root;
	int check;
	int err;

	Shortcut<K,T> *current;		// Reference to current key

	/* ---------------- Nodes call into the tree here ---------------- */

	friend BTreeNode<K,T>;

	// Change the root node when growing up or down (could be zero)
	virtual void setRoot (BTreeNode<K,T> *node);

	// Get a reference to a node
	virtual node_type *get (Node &, int depth);

	/// Create a new node
	virtual node_type *make (int depth);

	/// Delete a node.
	virtual void destroy (node_type *node);

};


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

#endif /* _BTree_hh_ */
