/*
 * $Id: BTreeFile.hh,v 1.16 2004-10-22 22:44:49 burghart Exp $
 *
 * BTree subclass which implements persistence using a BlockFile.
 */

#ifndef _BTreeFile_hh_
#define _BTreeFile_hh_

#include <vector>

#include "BTree.hh"
#include "BlockObject.hh"
#include "Logger.hh"

template <class K, class T> class BlockNode;
template <class K, class T> class BTreeFile;


class BTreeFileP
{
public:
	/*
	 * Statistics we care about.
	 */
	class FileStatsPart
	{
	public:
		unsigned long nodesRead;
		unsigned long nodesWritten;
		unsigned long overflowBytes;	// persistent
		// Bytes allocated to us in file
		unsigned long allocBytes;
		void translate (SerialStream &ss);
		void reset ()
		{ 
			nodesRead = nodesWritten = 0; 
			overflowBytes = 0;
			allocBytes = 0;
		}
		FileStatsPart() { reset(); }
	};

	class FileStats : public BTreeP::Stats
	{
	public:
		FileStatsPart file;
	};
};


/*
 * We subclass the basic BTree interface, override the "factory methods",
 * and add some constructors specific to BlockFile usage.
 */

template <class K, class T>
class BTreeFile : virtual public BTree<K,T>, virtual public TranslateBlock
{
public:

	typedef BTreeFileP::FileStats Stats;
	typedef BTreeFileP::FileStats FileStats;
	typedef BTree<K,T> tree_type;
    

public:
	/* ----------------
	 * Note that the BTreeFile constructors accept element size and
	 * fixed-ness just as for BTree, but hints about key size and
	 * fixed-ness need to be made with a call to setKeySize().
	 */

	// Open a b-tree on an existing blockfile.  This constructor
	// expects the b-tree header to be the blockfile's application
	// header, and if not found creates a new b-tree at a new
	// application header.  When creating a b-tree object on an
	// existing b-tree, it is up to the application to use the correct
	// template types for the keys and values when opening the tree.
	//
	BTreeFile (BlockFile &bf, int order = tree_type::DEFAULT_ORDER, 
		   long sz = sizeof(T), int fix = 0);

	// Just like above except the default blockfile is created for the
	// given path.
	//
	BTreeFile (const char *fname, int order = tree_type::DEFAULT_ORDER, 
		   long sz = sizeof(T), int fix = 0);

	// Constructor for a new btree using a default file name
	//
	BTreeFile (int order = tree_type::DEFAULT_ORDER, 
		   long sz = sizeof(T), int fix = 0);

	// Open a b-tree at the given blockfile address of the given
	// blockfile.  If the offset is zero, allocate a new header in the
	// blockfile on which to create the new b-tree.  All other
	// parameters are as above, but are only used if the b-tree does
	// not already exist.
	//
	BTreeFile (BlkOffset addr, BlockFile &bf, 
		   int order = tree_type::DEFAULT_ORDER, 
		   long sz = sizeof(T), int fix = 0);

	// Retrieve the address for this b-tree in its blockfile.
	//
	BlkOffset Address ();

	// Close and re-open the blockfile associated with this b-tree,
	// and re-initialize this b-tree with the new blockfile.  This is
	// mostly useful for testing.
	//
	virtual void Reopen ();

	// We must override this method to erase all our keys and
	// also release our header block.
	//
	virtual void Destroy ();

	virtual ~BTreeFile ();

	/// Convenience methods for setting the block size parameters,
	/// which only work while there is no root node, since block size
	/// must remain consistent among all existing blocks.

	void setKeySize (int ks, int fixed = 0);
	int keySize (int *fixed = 0);

	// Override BTree method so we can detect changes and reflect them
	// in the leaf and node sizes.
	//
	virtual int setElementSize (int vs, int fixed = 0);

	// Return the current block sizes for branch nodes and leaves.
	// If the tree is empty, these may still be zero.
	//
	long leafBlockSize () const
	{
		return leaf_size;
	}

	long nodeBlockSize () const
	{
		return node_size;
	}

	void currentStats (FileStats &s) const
	{
		BTree<K,T>::currentStats (s);
		s.file = BTreeFile<K,T>::fstats;
	}

        std::ostream &reportStats (std::ostream &out, 
				   const FileStats &s) const;

	virtual std::ostream &reportStats (std::ostream &out) const
	{
		FileStats s;
		currentStats (s);
		return reportStats (out, s);
	}

	virtual void translate (SerialStream &ss);

	static const unsigned long MAGIC;

protected:

	void OpenHeader ();
	void Open (BlkOffset addr);
	void Create ();

	virtual void enterWrite ();
	virtual void enterRead ();
	virtual void leave ();
	virtual void mark ();

	friend class BlockNode<K,T>;

	/// Resurrect a reference to a node
	virtual BTreeNode<K,T> *get (BTreeP::Node &, int depth);

	/// Create a new node
	virtual BTreeNode<K,T> *make (int depth);

	void Init (int order, long sz, int fix);
	void release ();

	long nodeSize (BlockNode<K,T> *node);

	BlockFile *bf;
	int key_size;
	int key_size_fixed;
	int our_bf;

	typedef BlockNode<K,T> node_type;

	// Node cache 
	unsigned long lru;
	int maxcache;
	int ncache;
	node_type *lookupCache (int depth);
	void trimCache ();

	// The basic node size stays fixed while there are nodes in the
	// tree.  It gets set by the first node to be encoded.
	long node_size;
	long leaf_size;

	Sender log;

	BTreeFileP::FileStatsPart fstats;
};


#endif /* _BTreeFile_hh_ */

