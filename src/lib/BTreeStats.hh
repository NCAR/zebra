// $Id: BTreeStats.hh,v 1.1 1998-03-16 20:56:45 granger Exp $

#ifndef _BTreeStats_hh_
#define _BTreeStats_hh_
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
#endif /* _BTreeStats_hh_ */

