/*
 * B-Tree base class implementation.
 */
#ifndef BTREE_IMPLEMENTATION
#define BTREE_IMPLEMENTATION

#include <stddef.h>
#include <stdlib.h>
#include <iostream.h>
#include <vector>
#include <algorithm>
#include <assert.h>

#include "BTree.hh"
#include "BTreeStats.hh"
#include "Serialize.hh"

#ifndef IFD
#define IFD(x)
#endif

/*
 * For internal nodes, the pointer Pi (1 <= i <= M) points to the sub-tree
 * whose keys K satisfy (Ki < K < Ki+1).  All keys (K : K < K1) are pointed
 * to by P0, keys (K : K >= Km) are pointed to by Pm.  In other words:

    [K0]   K1 	  K2 	 K3 	  Km
       \  /   \  /   \  /  \	 /  \
	P0     P1     P2     ...     Pm

	[P0] < K1 <= [P1] < K2 <= [P2] ...

   Kn is the leftmost key of the child node [Pn] and all its children.
   M is the number of keys minus one, with a maximum of one less than
   the order of the tree.

   The K0 key is kept around even though it is not used to find a
   child given a particular key.  It is updated when the leafmost child
   passes up a new leftmost key, and it then passes the new leftmost
   key to its parent, and so on.  Then when nodes are recombined the
   correct key for a child is known without traversing into it.

   Leaves use the element table to point to a key's value in a serial
   buffer.  Each leaf has Order() keys and elements.

 */


/*
 * Declare this here so it can be inlined.
 */
void BTreeP::Node::translate (SerialStream &ss)
{
	ss << addr;
}

inline
SerialStream & operator<< (SerialStream &ss, BTreeP::Node &object)
{
	object.translate (ss);
	return (ss);
}

inline
SerialStream & operator>> (SerialStream &ss, BTreeP::Node &object)
{
	object.translate (ss);
	return (ss);
}


template <class K, class T> class BTreeNode;


/* =================================================================
 * The Shortcut class serves as a context holder, so that we can
 * record parent hierarchy and traversal information as we descend
 * into the tree, as well as a current leaf and key index.  This
 * frees us from needing to store parent pointers in child nodes,
 * and allows simple in-order stepping without multiple depth
 * traversals.
 */
template <class K, class T>
struct Shortcut
{
	BTreeNode<K,T> *leaf;	// Pointer to leaf containing key
	BTreeNode<K,T> *root;	// Most recently set root
	int newroot;		// Root value has changed
	int depth;		// Count of current tree depth
	K key;			// Copy of key
	int i;			// Index of key in leaf
	std::vector<BTreeNode<K,T> *> parents;

	/*
	 * If leaf is zero we are not a valid shortcut.
	 */
	Shortcut() : leaf(0), root(0), newroot(0), depth(0)
	{
		parents.resize (5, 0);
	}

	void clear ()		// Invalidate this shortcut
	{
		leaf = 0;
		root = 0;
		newroot = 0;
		depth = 0;
		parents[0] = 0;
	}

	void set (BTreeNode<K,T> *l, const K &k, int ix)
	{
		leaf = l;
		key = k;
		i = ix;
	}

	void growRoot (BTreeNode<K,T> *r)
	{
		root = r;
		++newroot;
		stack (depth, r);
	}

	void chopRoot (BTreeNode<K,T> *r)
	{
		root = r;
		++newroot;
		--depth;
		if (depth >= 0)
			parents[depth] = 0;
	}

	inline int Depth ()
	{
		return depth;
	}

	int RootChanged ()
	{
		return newroot;
	}

	BTreeNode<K,T> *getRoot ()
	{
		newroot = 0;
		return root;
	}

	// Set the parent at the given depth. parents[0] is the
	// parent of the leaf, while parents[tree.Depth()] == 0;

	void stack (int d, BTreeNode<K,T> *node)
	{
		// First descent from the root sets the depth of the tree
		if (d+1 > depth)
		{
			depth = d + 1;
			parents.clear ();
			//if ((unsigned int)depth >= parents.capacity ())
			//parents.reserve (2 * depth);
			parents.resize (depth+2, 0);
			parents[depth] = 0;
		}
		parents[d] = node;
	}

	inline bool valid () { return (leaf != 0); }

	// This really should never happen...
	void invalidate (BTreeNode<K,T> *node)
	{
		for (int j = 0; valid() && j < depth; ++j)
		{
			if (parents[j] && (parents[j] == node))
				leaf = 0;
		}
	}

	// Return this shortcut's value, if any, and possibly the key
	int value (T *value, K *k = 0);

	// Remove this shortcut's key, if any
	int remove ();

	// Insert a value at the current key and placeholder
	int insert (const T &value);

	void show (ostream &out)
	{
		if (valid())
		{
			out << "Depth: " << depth << " : ";
			for (int i = depth; i >= 0; --i)
			{
				out << (void*)parents[i] << ", ";
			}
		}
		else
		{
			out << "Context invalid.";
		}
		out << endl;
	}
};



// A simple structure for the offset to an element value
struct Element
{
	int offset;

	Element() : offset(0)
	{ }

	inline void translate (SerialStream &ss)
	{
		ss << offset;
	}
};

SERIAL_STREAMABLE (Element);


/*
 * Given 'nslots' elements of an array T, open 'n' slots at index 'slot'
 * with a backward copy of [slot,nslots) to [slot+n,nslots+n).  We don't use
 * the copy_backward STL algorithm because it requires the destination be
 * outside the source range, which may not be true here.
 */
template <class T>
inline void open_slot (T array[], int slot, int nslots, int n = 1)
{
	if (n > 0)
	{
		int src = nslots-1;
		int dest = nslots+n-1;
		while (slot <= src)
		{
			array[dest--] = array[src--];
		}
	}
}


/*
 * Like open_slot, but we use a forward copy to close 'n' slots.
 */
template <class T>
inline void close_slot (T array[], int slot, int nslots, int n = 1)
{
	if (n > 0)
	{
		int src = slot + n;
		int dest = slot;
		while (src < nslots)
		{
			array[dest++] = array[src++];
		}
	}
}


/* ================================================================
 * This is the class which actually holds node information, including
 * all keys, and either child pointers or values depending upon
 * whether the node is a leaf or a branch.
 */

template <class K, class T>
class BTreeNode
{
public:
	/* ----------------------------------------------------------------
	 * EXPOSED INTERFACE TO BTREE NODES
	 *
	 * Most calls will be made through a Shortcut, but a BTree does
	 * make some calls directly into a node.  Protected methods
	 * exist for extending the behavior for factories and persistence.
	 * All other methods are private.
	 *
	 * The BTree caller must check the Shortcut for a new root after
	 * every call into the tree which may change the tree.
	 * ----------------------------------------------------------------
	 */

	// typenames
	typedef BTree<K,T> tree_type;
	typedef BTree<K,T>::value_type value_type;
	typedef BTree<K,T>::key_type key_type;
	typedef BTreeNode<K,T> node_type;
	typedef BTreeNode<K,T> *node_type_ptr;
	typedef Shortcut<K,T> shortcut;
	typedef BTree<K,T>::Node Node;

	/* ---------------- Constructors and destructors ---------------- */

	// The lone constructor creates an empty node of the given depth.
	BTreeNode (BTree<K,T> &t, int d);

	virtual ~BTreeNode ();

	// Erase all of this node's children and the node itself from the tree.
	void erase ();

	const BTree<K,T>::Node &Address ()
	{
		return thisNode;
	}

	inline int Depth ()
	{
		return (depth);
	}

	// ---------------- Traversing nodes ----------------

	int find (const K &key, shortcut *s)
	{
		if (depth == 0)
		{
			int i;
			int found = findKey (key, &i);
			s->set (this, key, i);
			return (found);
		}
		// Keep descending...
		assert (nkeys);
		int n;
		findChild (key, &n);
		descend (s);
		return (follow(children[n])->find (key, s));
	}

	// Collect statistics on ourself and all our children
	int stats (BTreeStats &s)
	{
		s.nkeys += nkeys;
		s.keysopen += (maxKeys() - nkeys);
		// For counting max and min keys, ignore the root since
		// its an exception, unless its also the lone leaf;
		if (depth < tree.Depth() || depth == 0)
		{
			if (s.minkeys <= 0 || nkeys < s.minkeys)
				s.minkeys = nkeys;
			if (nkeys > s.maxkeys)
				s.maxkeys = nkeys;
		}
		long memalloc = 0;
		long memused = 0;
		memalloc = memused = sizeof (*this);
		memalloc += maxKeys() * sizeof(keys[0]);
		memused += nkeys * sizeof(keys[0]);
		if (depth == 0)
		{
			s.nleaves++;
			s.nelements += nkeys;
			s.elemused += table[nkeys].offset;
			s.elemalloc += sbuf->Length();
			memalloc += maxKeys() * sizeof(table[0]);
			memused += nkeys * sizeof(table[0]);
		}
		else
		{
			s.nbranch++;
			memalloc += tree.Order() * sizeof(children[0]);
			memused += nkeys * sizeof(children[0]);
		}
		s.nodealloc += memalloc;
		s.nodeused += memused;

		if (depth > 0)
		{
			// Now collect from all of the children
			for (int i = 0; i < nkeys; ++i)
			{
				follow(children[i])->stats(s);
			}
		}
		return 0;
	}


	// Check the integrity of our tree and return the number of errors.

	int check ()
	{
		int err = 0;
		err += (keys == 0);
		// Verify keys are sorted
		for (int i = 0; i < nkeys - 1; ++i)
		{
			err += ! (keys[i] < keys[i+1]);
		}
		if (depth == 0)
		{
			// Leaves need at least one key, no kids
			err += (nkeys < 1);
			err += (children != 0);
			err += (sbuf == 0);
			err += (table == 0);
			err += (! (table[nkeys].offset > 0));
		}
		else
		{
			// Make sure our kids' tags and keys are correct.
			node_type *child;
			for (int i = 0; i < nkeys; ++i)
			{
				child = follow(children[i]);
				err += (keys[i] > child->keys[0]);
				if (i > 0)
				{
					err += (children[i].addr > 0 && 
				  (children[i].addr == children[i-1].addr));
					err += (children[i].local && 
				  (children[i].local == children[i-1].local));
				}
			}
			child = follow(children[0]);
			err += (child->keys[child->nkeys-1] >= keys[1]);
		}

		if (err)
		{
			cout << "CHECK ERROR in " 
			     << (void*)this << ", depth: " << depth
			     << ", nkeys: " << nkeys << endl;
		}

		if (depth > 0)
		{
			// Now check all of the children
			for (int i = 0; i < nkeys; ++i)
			{
				err += follow(children[i])->check();
			}
		}
		return (err);
	}
		

	ostream &print (ostream &out)
	{
		// Depth first dump of this node
		int indent = (tree.Depth() - depth) * 5;
		std::string s(indent, ' ');
		out << s;
		out << "[" << (void *)this << " " << thisNode.addr
		    << ": depth " << depth;
		cout << ", nkeys " << nkeys << "]" << endl;
		for (int i = 0; i < nkeys; ++i)
		{
			out << s << keys[i];
			if (depth == 0)
			{
				T v;
				value (i, &v);
				out << ":     " << v << endl;
			}
			else
			{
				follow(children[i])->print (out);
			}
		}
		out << endl;
		return (out);
	}

	// For horizontal traverses

	/* The semantics are this: find the key strictly greater than (right)
	   or strictly less than (left) the given key.  So if we're traversing
	   from an existing key, we'll find the next or previous one,
	   respectively.  The search always starts in a leaf.
	   */

	int step (shortcut *s, int n = 1)
	{
		// For now the shortcut is required to be valid, since
		// it holds our parent stack.
		assert (s->valid());
		assert (depth == 0);	// entry point for leaves only

		if (n == 0)
			return 1;
		int k = s->i;
		// If the target is not equal to the current key,
		// we first need to advance to the next existing key.
		// The shortcut at least points to where the target key	
		// would have been inserted, i.e. s->key <= keys[k].
		if (n > 0 && s->key < keys[k])
		{
			// this *is* the next key greater than the target
			--n;
		}
		else if (n < 0 && s->key < keys[k])
		{
			// the key less than this one is behind us
			--k;
			++n;
		}
		s->set (this, keys[k], k);

		// Now try to consume the remaining number of steps
		k += n;
		n = 0;
		if (k >= nkeys)
		{
			n = k - (nkeys - 1);
			k = nkeys - 1;
		}
		else if (k < 0)
		{
			n = k;
			k = 0;
		}
		s->set (this, keys[k], k);
		if (n == 0)
		{
			return (1);
		}

		if (parent(s) && (parent(s)->upStep (s, this, n)))
			return (1);
		s->clear();
		return 0;
	}

	int findLeft (shortcut *s)
	{
		if (depth == 0)
		{
			assert (nkeys);
			s->set (this, keys[0], 0);
			return (s->valid());
		}
		descend (s);
		return (follow(children[0])->findLeft (s));
	}

	int findRight (shortcut *s)
	{
		if (depth == 0)
		{
			assert (nkeys);
			s->set (this, keys[nkeys-1], nkeys-1);
			return (s->valid());
		}
		descend (s);
		return (follow(children[nkeys-1])->findRight (s));
	}

	// ---------------- Shortcut direct access ----------------

	int insert (shortcut *s, const K &key, const T &value)
	{
		mark ();
		if (s->i < nkeys && key == keys[s->i])
		{
			replace (s->i, value);
		}
		else if (nkeys == maxKeys())
		{
			// uh-oh, time to recombine this node with siblings
			recombine (s, &key, &value);
		}
		else
		{
			insert (s->i, key, value);
			if (s->i == 0 && parent(s))
			{
				// update leftmost key
				parent(s)->replaceChild (s, keys[1], keys[0]);
			}
		}
		return (1);
	}

	int value (shortcut *s, T *v)
	{
		if (v)
		{
			value (s->i, v);
		}
		return (1);
	}

	int remove (shortcut *s)
	{
		mark ();
		assert (s->key == keys[s->i]);
		// Reclaim buffer space and the key slot
		reclaim (s->i);
		close_slot (keys, s->i, nkeys);
		close_slot (table, s->i, nkeys+1);
		--nkeys;

		if (s->i == 0 && parent(s))
		{
			// update leftmost key
			parent(s)->replaceChild (s, keys[0], keys[0]);
		}

		// Check if recombination necessary
		recombine (s);
		return (1);
	}

	// ---------- "Factory" methods -----------

	virtual void sync (int force = 0) { }
	virtual void mark ()
	{
		// cout << "mark() in base class called!" << endl;
	}

protected:

	inline node_type * follow (Node &a)
	{
		return (tree.get (a, depth-1));
	}

	inline node_type * make (int depth_)
	{
		++tree.stats.nNodes;
		if (depth_ == 0)
			++tree.stats.nLeaves;
		tree.mark ();
		return (tree.make (depth_));
	}		

	inline BTreeNode<K,T> *parent (shortcut *s)
	{
		assert (s->valid ());
		assert (this->depth <= s->Depth());
		BTreeNode<K,T> *p = s->parents[this->depth];
		if (p)
		{
			assert (p->thisNode.local);
			p = tree.get (p->thisNode, p->depth);
		}
		return (p);
	}

	// Cut this node from the tree and destroy it.  
	inline void prune ()
	{
		--tree.stats.nNodes;
		if (this->depth == 0)
			--tree.stats.nLeaves;
		tree.mark ();
		this->destroy();
	}

	// Subclasses override this method if there is any specific
	// destruction to be done, since the call from the prune()
	// method will only see the base node class interface.
	virtual void destroy ()
	{
		delete this;
	}

	inline void descend (shortcut *s)
	{
		assert (depth > 0);
		s->stack (depth - 1, this);
	}

	node_type *sibling (node_type *node, int i)
	{
		int n;
		findChild (node->keys[0], &n);
		n += i;
		if (n >= 0 && n < nkeys)
			return follow(children[n]);
		return 0;
	}

	inline node_type *leftSibling (node_type *node)
	{
		return (sibling (node, -1));
	}

	inline node_type *rightSibling (node_type *node)
	{
		return (sibling (node, 1));
	}

	inline int maxKeys ()
	{
		return tree.Order();
	}

	inline int Overflow ()
	{
		// Leave room for an insertion later on.
		int mk = maxKeys();
		return ((mk >= 16) ? mk-1 : mk);
	}

	// ---------------- Traversing nodes ----------------

	int findChild (const K &key, int *i = 0)
	{
		int n;
		int found;
		// Find greatest index n for which key <= keys[n], else
		// returns nkeys
		if (! (found = findKey (key, &n)) && n > 0)
		{
			// If key > keys[nkeys-1], child is P[n-1]
			// If key < keys[n], child is P[n-1]
			--n;
		}
		// Else key == keys[n] and child is P[n], or
		// key <= keys[0], so child is P[n] (n == 0)
		if (i) *i = n;
		return found;
	}

	int findKey (const K &key, int *i = 0)
	{
		// Look for this key, and set *i to the index at which this
		// key exists or would be inserted.  Return non-zero if
		// there's a match.
		//assert (nkeys); // Bootstrapped root leaves allowed to be mt
		K *which = std::lower_bound(keys, keys+nkeys, key);
		int found = 0;
		if (which != keys+nkeys)
			found = (*which == key);
		if (i) *i = which - keys;
		return (found);
	}

	int upStep (shortcut *s, node_type *child, int n)
	{
		assert (depth > 0);	// parents only

		// Try for a sibling, and if none keep going up
		int inc = (n > 0) ? 1 : -1;
		node_type *node = sibling (child, inc);
		if (node)
		{
			if (n < 0) // the greatest on this sibling is prev key
			{
				node->findRight (s);
				++n;
			}
			else	// the least on this sibling is the next key
			{
				node->findLeft (s);
				--n;
			}
			return ((s->leaf)->step (s, n));
		}

		if (parent(s))
			return (parent(s)->upStep (s, this, n));
		return 0;
	}


	inline int left (shortcut *s)
	{
		return (step (s, -1));
	}


	inline int right (shortcut *s)
	{
		return (step (s, 1));
	}

	/* ------------- Leaf operations for handling values ------------- */

	void adjust (int i, int sz)
	{
		mark ();
		// Expand or reduce space for element i to sz
		int to = table[i].offset + sz;
		int from = table[i+1].offset;
		int n = table[nkeys].offset - from;
		sbuf->Move (to, from, n);
		for (int j = i+1; j <= nkeys; ++j)
		{
			table[j].offset += to - from;
		}
	}

	// Reclaim space for a value by shifting the buffer and offset table

	void reclaim (int i)
	{
		// Empties the value at i
		adjust (i, 0);
	}

	// Insert the given value for key k into the value buffer

	void inject (int k, const T &value, long sz = 0)
	{
		mark ();
		if (! sz)
			sz = serialCount (*sbuf, value);
		adjust (k, sz);
		sbuf->Seek (table[k].offset);
		*sbuf << value;
		// check the offset for the next element
		assert (sbuf->Position() <= table[k+1].offset);
	}

	// Replace the value of an existing key

	void replace (int k, const T &value)
	{
		mark ();
		// Check if the new size still fits
		long sz = serialCount (*sbuf, value);
		long length = table[k+1].offset - table[k].offset;
		if (sz > length || sz < length/2)
		{
			// Inject the value at the new size
			inject (k, value, sz);
		}
		else
		{
			// Overwrite in place and leave length alone
			sbuf->Seek (table[k].offset);
			*sbuf << value;
		}
	}

	// Read a value

	void value (int k, T *value)
	{
		sbuf->Seek (table[k].offset);
		*sbuf >> *value;
	}

	// Insert a new key and value at the given index

	void insert (int k, const K &key, const T &value)
	{
		mark ();
		open_slot (table, k, nkeys+1);
		open_slot (keys, k, nkeys);
		++nkeys;
		keys[k] = key;
		// Initialize the new element slot to empty
		table[k].offset = table[k+1].offset;
		inject (k, value);
	}

	/* ---------------- Growth ---------------- */

	void addRight (shortcut *s, node_type *node, const K &key)
	{
		assert (node != this);
		IFD(cout << "This node: " << endl;
		    this->print (cout);
		    cout << "adding this node to the right: " << endl;
		    node->print (cout);)
		if (! parent(s))
		{
			growUp (s, node, key);
			assert (parent(s));
		}
		else
		{
			// We have a parent, and a sibling to insert
			parent(s)->recombine (s, &key, 0, node);
		}
	}

	// Grow upwards into a new root node

	void growUp (shortcut *s, BTreeNode<K,T> *right, const K &key)
	{
		BTreeNode<K,T> *root = make (depth+1);
		root->keys[0] = this->keys[0];
		root->keys[1] = key;
		root->nkeys = 2;
		root->children[0] = this->Address();
		root->children[1] = right->Address();
		//root->mark (); //redundant to make()
		s->growRoot (root);
	}

	// ---------------- Parent operations ----------------

	// Insert a new key and node given that there is already room for it

	void insert (const K &key, node_type *child)
	{
		int k;
		int found = findKey (key, &k);
		assert (! found);
		open_slot (keys, k, nkeys);
		keys[k] = key;
		open_slot (children, k, nkeys);
		children[k] = child->Address();
		++nkeys;
		mark ();
	}

	void removeChild (const K &key)
	{
		assert (depth && children);
		assert (nkeys);

		// A child is going away.  Shift our children and keys.
		int child;
		findChild (key, &child);
		close_slot (keys, child, nkeys);
		close_slot (children, child, nkeys);
		--nkeys;
		mark ();
	}

	void replaceChild (shortcut *s, const K &tag, const K &key)
	{
		// Change the key for the child which leads to this key
		int child;
		findChild (tag, &child);
		if (! (keys[child] == key))
		{
			keys[child] = key;
			mark ();
			// May need to tell our parent
			if (child == 0 && parent(s))
				parent(s)->replaceChild (s, tag, key);
		}
	}

	// ---------------- Node balancing methods ----------------

	// Move 'num' keys from the 'src' node at index 'sk' to index 'dk',
	// inserting in the dest node and removing from the source

	void shift (int dk, node_type &src, int sk, int num)
	{
		if (this == &src && sk == dk)
			return;

		this->mark();
		src.mark();

		// Make room for the keys in our node, then copy them
		open_slot (keys, dk, nkeys, num);
		std::copy (src.keys+sk, src.keys+sk+num, keys+dk);

		if (depth == 0)
		{
			shiftElements (dk, src, sk, num);
		}
		else
		{
			// Copy the children
			open_slot (children, dk, nkeys, num);
			std::copy (src.children+sk, src.children+sk+num, 
				   children+dk);
			// Remove the source children
			close_slot (src.children, sk, src.nkeys, num);
		}

		nkeys += num;
		close_slot (src.keys, sk, src.nkeys, num);
		src.nkeys -= num;
	}


	void shiftElements (int dk, node_type &src, int sk, int num)
	{
		// Make room for the new elements, then copy them and
		// their table entries
		int diff = src.table[sk].offset - table[dk].offset;
		int len = src.table[sk+num].offset - src.table[sk].offset;
		sbuf->Move (table[dk].offset + len, table[dk].offset,
			    table[nkeys].offset - table[dk].offset);
		void *from = src.sbuf->Peek (src.table[sk].offset, 0);
		sbuf->Seek (table[dk].offset);
		sbuf->Need (len);
		sbuf->Write (from, len);
		open_slot (table, dk, nkeys+1, num);
		std::copy (src.table+sk, src.table+sk+num, table+dk);

		// Adjust offsets of the new elements
		int j;
		for (j = dk; j < dk+num; ++j)
			table[j].offset -= diff;

		// Adjust offsets of the shifted elements
		for (j = dk+num; j <= nkeys + num; ++j)
			table[j].offset += len;

		// Now shift table and elements in the source
		int r = src.table[src.nkeys].offset - src.table[sk+num].offset;
		src.sbuf->Move (src.table[sk].offset, 
				src.table[sk+num].offset, r);
		close_slot (src.table, sk, src.nkeys+1, num);
		for (j = sk; j <= src.nkeys - num; ++j)
			src.table[j].offset -= len;
	}

	// Balance a left-right array of sibling nodes given a set of
	// keys to absorb from a left sibling.

	void balance (node_type *l, int take, node_type *r[], int n)
	{
		// n is the number of right siblings

		if (take + nkeys > maxKeys())
		{
			// We need our right sibling to take some on
			assert (n > 0);
			int give = nkeys + take - maxKeys();
			r[0]->balance (this, give, r+1, n-1);
		}
		// Now we can shift from the left sibling
		assert (nkeys + take <= maxKeys());
		shift (0, *l, l->nkeys - take, take);
		l->mark();
		mark();
	}

	// 'This' node needs to change, due to an insertion or a removal,
	// so try recombine it with its siblings and redistribute the keys,
	// which may require adding a new sibling or deleting an empty one.

	void recombine (shortcut *s, const K *ikey = 0, const T *ival = 0,
			BTreeNode<K,T> *inode = 0)
	{
		int in = (ikey != 0) ? 1 : 0;

		// Pass off root case.
		if (in == 0 && ! parent(s))
		{
			recombine_root (s);
			return;
		}			
				
		// All other nodes are ok with enough keys
		if (in == 0 && nkeys >= 2*maxKeys()/3)
			return;

		IFD(cout << "-*- Recombining node, tag: ";
		    if (depth == s->Depth()) cout << "Root";
		    else cout << keys[0];
		    cout << ", depth: " << depth << endl;)
		IFD(if (in) { cout << "Inserting " << *ikey << endl;
		    if (inode) inode->print(cout);
		    cout << "into this node: " << endl;
		    this->print (cout); })

		int width = 1;
		int node_alloc = (2*width+2) * 2;
		node_type_ptr *node;
		node = new node_type_ptr[ node_alloc ];
		K *tags = new K[ node_alloc ];

		// Collect our siblings into a left-right array of nodes,
		// unless we already have enough space for an insertion
		if (in > 0 && nkeys + in <= maxKeys())
			width = 0;
		int n = 0;
		int nk = in;
		int i;
		for (i = -width; i <= width; ++i)
		{
			node[n] = 0;
			if (i == 0)
				node[n] = this;
			else if (parent(s))
				node[n] = parent(s)->sibling (this, i);
			if (! node[n])
				continue;
			/*
			 * Don't redistribute adequately-filled left nodes
			 * when inserting keys.
			 */
			if ((n == 0) && (i < 0) && (in > 0) &&
			    node[n]->nkeys >= Overflow())
			{
				IFD(cout << "Skipping full-enough left node "
				    << i << ", nkeys: " << node[n]->nkeys
				    << endl;);
			}
			else
			{
				tags[n] = node[n]->keys[0];
				nk += node[n]->nkeys;
				IFD(cout << "Found node(" << n << ") at " << i
				     << ", nkeys: " << node[n]->nkeys << endl);
				++n;
			}

			/*
			 * If we now have enough key slots with these
			 * leftmost nodes, use them only.
			 */
			if (i >= 0 && nk <= n * maxKeys() && in > 0)
				break;
		}
		// Should have gotten at least ourself
		assert (n >= 1);
		assert (nk >= 1);

		// Find the minimum number of nodes we need to keep nodes at
		// least 2/3 full, without adding more than one node.
		// Force need to 1 if width is 0.

		int need = 1;
		int fill;
		if (width != 0)
		{
			// fill >= 2 since Order >= 3
			// fill = maxKeys() * 2 / 3;
			need = ((nk-1) / maxKeys()) + 1;
			// if (need * maxKeys() < nk)
			//	++need;
			// if (need > n + 1)
			//	need = n + 1;
			assert (nk/need >= 2 && need*maxKeys() >= nk);
		}
		IFD(cout << "Need: " << need << endl);
		assert (need > 0);
		assert (need <= n+1);

		// Create any needed nodes.
		int nexist = n;
		while (n < need)
		{
			IFD(cout << "Creating node " << n << endl);
			node[n++] = make (node[0]->depth);
		}

		BTreeNode<K,T> *up = node[0]->parent(s);
		int src = 1;		// first source node is right sibling
		int dest = 0;		// dest node
		int k = 0;		// Number of keys 'placed' so far
		
		// For each pass we try to fill the current dest node from
		// the current key source, whether the source be a right 
		// sibling or the key to be inserted.  The number of keys we
		// can fill on each pass is limited by the keys available
		// in the current source node, the number needing to be
		// filled, and the location of the insertions.

		int do_insert;
		int num;
		while (dest < need)
		{
			assert (src > dest);
			// How many keys the dest node needs
			fill = ((dest+1)*nk)/need - k;
			do_insert = 0;
			IFD(cout << "filling node " << dest
			    << ", needs " << fill << " keys" << endl;)

			// Find max we can take for what is needed
			num = (src < n) ? node[src]->nkeys : 0;
			num = std::min (num, fill - node[dest]->nkeys);

			// Determine if an insertion should happen here
			if (in > 0)
			{
				if (dest + 1 == need) /* last chance */
				{
					do_insert = 1;
					num = 0;
				}
				else
				if ((num>0 && *ikey < node[src]->keys[num-1])
				    ||
				    (num<=0 && *ikey<node[dest]->keys[fill-1]))
				{
					do_insert = 1;
					--num;
				}
			}

			if (num > 0)
			{
				IFD(cout << "shifting " << num <<
				    " keys from right node " << src << endl;)
				node[dest]->shift (node[dest]->nkeys,
						   *node[src], 0, num);
			}
			else if (num < 0)
			{
				IFD(cout << "shifting " << num <<
				    " excess keys to node " << dest+1 << endl;)
				assert (dest+1 < need);
				// dest has excess to pass on
				node[dest+1]->balance (node[dest], -num,
						       node+dest+2, n-dest-2);
			}

			// Now's the time for any pending insertion
			if (do_insert)
			{
				IFD(cout << "inserting in dest node " << dest
				    << endl;)
				if (depth == 0)
				{
					node[dest]->find(*ikey, s);
					node[dest]->insert(s->i, *ikey, *ival);
				}
				else
				{
					node[dest]->insert (*ikey, inode);
				}
				// insert is done:
				in = 0;	
				//node[dest]->mark();
			}

			// If the dest node is now full, move on
			if (node[dest]->nkeys >= fill)
			{
				IFD(cout << "node " << dest << " filled with "
				    << node[dest]->nkeys << " keys" << endl;)
				assert (node[dest]->nkeys == fill);
				node[dest]->mark();
				k += node[dest]->nkeys;
				++dest;
				src = dest + 1;
			}

			// Advance the source node if its empty
			if (src < n && node[src]->nkeys == 0)
			{
				IFD(cout << "inc src to " << src << endl;)
				++src;
			}
		}

		// Remove any extra nodes.
		int ndelete = 0;
		while (n > need)
		{
			assert (n <= nexist);
			--n;
			++ndelete;
			IFD(cout << "Removing node " << n
			    << " (" << (void *)node[n] << ") " << endl);
			up->removeChild (tags[n]);
			node[n]->prune();
		}
		// !!! 'this' may have been one of the nodes deleted !!!

		// Update the keys of the existing nodes in their parent
		for (i = 0; up && i < nexist && i < need; ++i)
		{
			up->replaceChild (s, tags[i], node[i]->keys[0]);
		}

		if (ndelete > 0)
		{
			IFD(cout << "Nodes removed; recombining in parent"
			    << endl);
			assert (up);
			up->recombine (s);
		}
		else if (n > nexist)
		{
			IFD(cout << "Adding node to the right" << endl);
			assert (n == nexist+1 && nexist > 0);
			node[nexist-1]->addRight (s, node[nexist], 
						  node[nexist]->keys[0]);
		}

		delete[] tags;
		delete[] node;
	}


	void recombine_root (shortcut *s)
	{
		// If we're root (and hence have no siblings), we don't
		// do anything unless we're an empty leaf or have only
		// one child.
		if (depth > 0 && nkeys < 2)
		{
			// Reduce depth to our remaining child
			assert (nkeys == 1);
			node_type *child0 = follow(children[0]);
			s->chopRoot (child0);
			IFD(cout << "Reducing to depth: " << depth-1
			    << ", key: " << child0->keys[0]
			    << endl);
			nkeys = 0;
		}
		else if (depth == 0 && nkeys == 0)
		{
			// We we're the last node in the tree
			s->chopRoot (0);
		}
		if (nkeys == 0)
		{
			IFD(cout << "Deleting root: " << (void *)this
			    << endl);
			prune ();
		}
	}


	/* ---------------- Protected ---------------- */
protected:

	Node thisNode;			// The reference by which we're known

	BTree<K,T> &tree;		// Reference to our tree base
	int depth;			// Depth of this node
	int nkeys;			// Number of keys in this node
	K *keys;			// Every node has an array of keys

	// Every node uses the above members.  The following depend on depth.

	Node *children;			// Only leaves don't have children

	Element *table;			// Only leaves have element offsets
	SerialBuffer *sbuf;		//  and a SerialBuffer
};




template <class K, class T>
inline int
Shortcut<K,T>::insert (const T &value)
{
	if (leaf)
	{
		return (leaf->insert (this, this->key, value));
	}
	return (0);
}



template <class K, class T>
inline int
Shortcut<K,T>::value (T *value, K *k)
{
	int ok = 0;
	if (valid())
	{
		ok = leaf->value (this, value); 
		if (ok && k)
			*k = key;
	}
	return ok;
}



template <class K, class T>
inline int
Shortcut<K,T>::remove ()
{
	if (leaf)
	{
		return (leaf->remove (this));
	}
	return (0);
}


/*================================================================
 * BTree implementation
 *================================================================*/


template <class K, class T>
BTree<K,T>::BTree (int _order, long sz, int fix) : current(0)
{
	Setup (_order, sz, fix);
}



template <class K, class T>
void
BTree<K,T>::Setup (int _order, long sz, int fix)
{
	depth = -1;
	root = 0;
	rootNode = Node();
	check = 0;
	err = 0;
	if (! current)
		current = new Shortcut<K,T>;

	order = _order;
	elem_size_fixed = fix;
	elem_size = sz;
	if (order < 3)
		order = DEFAULT_ORDER;
}



inline void
BTreeP::Stats::translate (SerialStream &ss)
{
	ss << nNodes << nKeys << nLeaves;
}


ostream &
BTreeP::Stats::report (ostream &out) const
{
	out << "Nodes: " << nNodes
	    << "; Keys: " << nKeys
	    << "; Leaves: " << nLeaves;
	out << endl;
	return out;
}


template <class K, class T>
ostream &BTree<K,T>::reportStats (ostream &out, const BTreeP::Stats &s) const
{
	s.report (out);
	int m = Order();
	out << "Depth: " << Depth() << "; Order: " << m << "; ";
	int minnodes = 0;
	if (numKeys() > 0)
		minnodes = ((numKeys() - 1) / (m - 1)) + 1;
	int pctnodes = 0;
	if (s.nNodes > 0)
		pctnodes = (int)(100.0*minnodes/(float)s.nNodes);
	out << "Min Nodes: " << minnodes << "; % nodes: " << pctnodes;
	out << endl;
	return out;
}



template <class K, class T>
void
BTree<K,T>::translate (SerialStream &ss)
{
	ss << depth << order << rootNode;
	ss << elem_size << elem_size_fixed;
	stats.translate (ss);
}



template <class K, class T>
BTree<K,T>::~BTree ()
{
	Destroy ();
}



template <class K, class T>
void
BTree<K,T>::Destroy ()
{
	// Root will only be non-zero if we are non-empty, and if the
	// subclass has not already taken care of freeing node memory
	// for us.
	if (root)
		delete root;
	if (current)
		delete current;
	root = 0;
	current = 0;
}



template <class K, class T>
void
BTree<K,T>::Erase ()
{
	enterWrite ();
	current->clear ();
	if (! Empty())
		root->erase ();
	stats.reset ();
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
		++stats.nNodes;
		++stats.nLeaves;
		mark();
		root = make (/*depth*/ 0);
		setRoot (root);
	}
	// Find the location and insert
	int found = root->find (key, current);
	int r = current->insert (value);
	if (bootstrap && !r)
	{
		// Enforce legal root of at least one key
		root->erase ();
		setRoot (0);
		current->clear();
		--stats.nNodes;
		--stats.nLeaves;
		mark();
	}
	if (!found && r)
	{
		++stats.nKeys;
		mark();
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
	if (r)
	{
		--stats.nKeys;
		mark ();
	}
	if (current->RootChanged ()) 
		setRoot (current->getRoot());
	if (check) err += Check ();
	leave ();
	return (r);
}




template <class K, class T>
void
BTree<K,T>::collectStats (BTreeStats &collect)
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
		e = root->check ();
		if (e)
		{
			cout << "***** TREE CHECK FAILED; " 
			     << e << " errors:" << endl;
			Print (cout);
		}
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



template <class K, class T>
ostream &
BTree<K,T>::Print (ostream &out)
{
	enterRead ();
	if (! Empty())
		root->print (out);
	leave ();
	return (out);
}



template <class K, class T>
int
BTree<K,T>::setElementSize (int vs, int fixed)
{
	int done = 0;
	enterWrite ();
	if (Empty())
	{
		elem_size = vs;
		elem_size_fixed = fixed;
		mark ();
		done = 1;
	}
	leave ();
	return done;
}



/* ---------------- Protected methods ---------------- */

template <class K, class T>
void
BTree<K,T>::setRoot (BTreeNode<K,T> *node)
{
	root = node;
	if (! node)
	{
		rootNode = Node();
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
BTree<K,T>::make (int depth_)
{
	return new BTreeNode<K,T> (*this, depth_);
}


/*================================================================
 * More BTreeNode implementation
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
	// If our children have not yet been deleted (i.e., by a subclass),
	// we need to do it
	if (children && nkeys)
	{
		for (int i = 0; i < nkeys; ++i)
			delete follow(children[i]);
		nkeys = 0;
	}
	if (children)
		delete[] children;
	if (keys)
		delete[] keys;
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
		nkeys = 0;   // Tells subclasses the children are gone
	}
	prune ();
}



#endif /*BTREE_IMPLEMENTATION*/
