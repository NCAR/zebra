/*
 * $Id: BTreeFile.hh,v 1.2 1998-05-28 22:00:46 granger Exp $
 *
 * BTree subclass which implements persistence using a BlockFile.
 */

#ifndef _BTreeFile_hh_
#define _BTreeFile_hh_

#include "BTree.hh"
#include "BlockObject.hh"
#include "BlockFactory.hh"
#include "Logger.hh"

/*
 * We subclass the basic BTree interface, override the "factory methods",
 * and add some constructors specific to BlockFile usage.
 */

template <class K, class T>
class BTreeFile : virtual public BTree<K,T>, virtual public TranslateBlock
{
public:
	BTreeFile (BlockFile &bf, int order = DEFAULT_ORDER, 
		   long sz = sizeof(T), int fix = 0);

	BTreeFile (const char *fname, int order = DEFAULT_ORDER, 
		   long sz = sizeof(T), int fix = 0);

	/// Constructor for a new btree using a default file name

	BTreeFile (int order = DEFAULT_ORDER, 
		   long sz = sizeof(T), int fix = 0);

	virtual ~BTreeFile ();

	/// Convenience methods for setting the block size parameters,
	/// which only work while there is no root node, since block size
	/// must remain consistent among all existing blocks.

	void setKeySize (int ks, int fixed = 0);
	void setValueSize (int vs, int fixed = 0);

	int keySize (int *fixed = 0);
	int valueSize (int *fixed = 0);

	long nodeSize (BlockNode<K,T> *node);

	virtual void translate (SerialStream &ss);

	static const unsigned long MAGIC;

protected:

	virtual void enterWrite ();
	virtual void enterRead ();
	virtual void leave ();
	virtual void mark ();

	friend BlockNode<K,T>;

	/// Resurrect a reference to a node
	virtual BTreeNode<K,T> *get (Node &, int depth);

	/// Create a new node
	virtual BTreeNode<K,T> *make (int depth);

	//virtual void release ();

private:
	BlockFile *bf;
	int key_size;
	int value_size;
	int key_size_fixed;
	int value_size_fixed;
	int our_bf;

	// The basic node size stays fixed while there are nodes in the
	// tree.  It gets set by the first node to be encoded.
	long node_size;
	long leaf_size;

	Logger *log;

	void Init (int order, long sz, int fix);

};



#endif /* _BTreeFile_hh_ */

