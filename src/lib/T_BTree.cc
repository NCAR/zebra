/*
 * Test the BTree
 */

#include <iostream.h>
#include <assert.h>
#include <stdlib.h>
#include <vector>
#include <function.h>
#include <algorithm>

#include <MLCG.h>	// GNU c++ lib
#include <RndInt.h>

#include "BlockFileP.hh"
#include "ZTime.hh"
#include "BTree.hh"

#define ZTime long
typedef BTree<ZTime,long> TimeTree;

typedef long test_key;
typedef BTree<test_key,test_key> test_tree;

int TestTree (test_tree &tree, int N);

int main (int argc, char *argv[])
{
	int N = 10;
	int order = 3;

	if (argc > 1)
		N = atoi(argv[1]);
	if (argc > 2)
		order = atoi(argv[2]);

	int err = 0;
	if (argc > 1)
	{{
		test_tree tree(order);
		cout << "Testing tree of order " << tree.Order() 
		     << " with " << N << " members..." << endl;
		err += TestTree (tree, N);
	}
	exit (err);
	}

	// Perform the tests on trees of various orders, using a bigger N
	if (argc <= 1)
		N = 5000;
	for (int o = 3; o < 1000; o *= 3)
	{
		test_tree tree(o);
		
		cout << "Testing tree of order " << tree.Order() 
		     << " with " << N << " members..." << endl;
		err += TestTree (tree, N);
	}
	
#ifdef notdef
	BigTest (N, order);
#endif
	exit (err);
}



#ifdef notdef
int 
BigTest (int N = 10, int order = 3) 
{
	// Create an empty btree of longs keyed by zebra times
	ZTime when;
	long v;

	TimeTree tree(order);
	// when = time(0);
	// when += -3600;
	when = -2;
	ZTime start = when;
	for (int i = 0; i < N; ++i)
	{
		when += 2;
		long value = 2*i;
		cout << "Inserting " << when << ", value: " << value << endl;
		tree.Insert (when, value);
		tree.Print (cout);
	}


	cout << endl;
	when = start;
	for (int i = 0; i < 2*N; ++i)
	{
		when += 1;
		long value;
		cout << "Looking for " << when << ", ";
		if (tree.Find (when, &value))
			cout << "value: " << value << endl;
		else
			cout << "Not Found" << endl;
	}

	if (tree.First (&when, &v))
		cout << "First: " << when << ", " << v << endl;
	else
		cout << "First: None." << endl;

	if (tree.Last (&when, &v))
		cout << "Last: " << when << ", " << v << endl;
	else
		cout << "Last: None." << endl;

	cout << "Traversing the tree from first to last." << endl;
	tree.First ();
	while (tree.Current (&when, &v))
	{
		cout << when << ", " << v << endl;
		tree.Next ();
	}

	cout << "Traversing the tree from last to first." << endl;
	tree.Last ();
	while (tree.Current (&when, &v))
	{
		cout << when << ", " << v << endl;
		tree.Prev ();
	}

	// Remove from the beginning and in the middle
	cout << endl;
	when = start;
	for (int i = 0; i < N/4; ++i)
	{
		when += 2;
		cout << "Removing for " << when << ", ";
		if (tree.Remove (when))
			cout << "Done." << endl;
		else
			cout << "Not Found." << endl;
		tree.Print (cout);
		cout << "Removing for " << (when+N) << ", ";
		if (tree.Remove (when + N))
			cout << "Done." << endl;
		else
			cout << "Not Found." << endl;
		tree.Print (cout);
	}

	cout << endl;
	when = start;
	for (int i = 0; i < 2*N; ++i)
	{
		when += 1;
		long value;
		cout << "Looking for " << when << ", ";
		if (tree.Find (when, &value))
			cout << "value: " << value << endl;
		else
			cout << "Not Found" << endl;
	}

	if (tree.First (&when, &v))
		cout << "First: " << when << ", " << v << endl;
	else
		cout << "First: None." << endl;

	if (tree.Last (&when, &v))
		cout << "Last: " << when << ", " << v << endl;
	else
		cout << "Last: None." << endl;

	tree.Print (cout);

	cout << endl;
	when = start;
	for (int i = 0; i < N/2; ++i)
	{
		when += 2;
		cout << "Removing for " << when << ", ";
		if (tree.Remove (when))
			cout << "Done." << endl;
		else
			cout << "Not Found." << endl;
		cout << "Removing for " << (when+N) << ", ";
		if (tree.Remove (when + N))
			cout << "Done." << endl;
		else
			cout << "Not Found." << endl;
	}

	cout << endl;
	when = start;
	for (int i = 0; i < 2*N; ++i)
	{
		when += 1;
		long value;
		cout << "Looking for " << when << ", ";
		if (tree.Find (when, &value))
			cout << "value: " << value << endl;
		else
			cout << "Not Found" << endl;
	}

	if (tree.First (&when, &v))
		cout << "First: " << when << ", " << v << endl;
	else
		cout << "First: None." << endl;

	if (tree.Last (&when, &v))
		cout << "Last: " << when << ", " << v << endl;
	else
		cout << "Last: None." << endl;

	cout << "Traversing the tree from first to last." << endl;
	tree.First ();
	while (tree.Current (&when, &v))
	{
		cout << when << ", " << v << endl;
		tree.Next ();
	}
	assert (tree.Empty());
	return 0;
}
#endif


// To test a tree, use sequential and random generators of integers.  Try
// inserting in order, then in reverse, then randomly.  After each
// insertion sequence, verify that all the keys can be found and have the
// correct values.  Make sure traversing the tree forward and reverse
// returns the correct keys and values.  Reinsert all the keys with a new
// value and verify those values.  Then remove all the keys randomly, and
// verify the tree is empty after it all.

// Use internal checking and automatic aborting.


struct Counter
{
	typedef test_key result_type;

	Counter (result_type _n = result_type()) : n(_n) {}
	result_type operator()() { return n++; }

	result_type n;
};



struct tree_member // predicate
{
	typedef test_tree::key_type argument_type; 
	typedef bool result_type;

	bool operator() (const test_tree::key_type &key) const
	{
		return (tree.Find (key));
	}
	tree_member (test_tree &_tree) : tree(_tree)
	{ }

private:
	test_tree &tree;
};



int 
T_Members (test_tree &tree, vector<test_key> &keys)
{
	// Verify that all of the keys can be found in the tree
	int err = 0;
	vector<test_key>::iterator k;

	k = find_if (keys.begin(), keys.end(), 
		     compose1(logical_not<bool>(), tree_member(tree)));
	if (k != keys.end())
	{
		cout << "***** Member error: missing key: " << *k << endl;
		++err;
	}
	return err;
}



int
T_Compare (test_tree &tree, vector<test_key> &keys, vector<test_key> &values)
{
	// Given a tree, keys, and values, verify the tree contains all
	// the keys and has the correct values
	int err = 0;
	vector<test_key>::iterator k, v;
	test_tree::value_type value;

	for (k = keys.begin(), v = values.begin(); k != keys.end();
	     ++k, ++v)
	{
		if (! tree.Find (*k, &value))
		{
			++err;
			cout << "***** Compare error: missing key: "
			     << *k << endl;
		}
		else if (value != *v)
		{
			++err;
			cout << "***** Compare error: key: "
			     << *k << " has value " << value
			     << " instead of " << *v << endl;
		}
	}
	return err;
}



int 
T_Insert (test_tree &tree, vector<test_key> &keys, vector<test_key> &values)
{
	int err = 0;
	vector<test_key>::iterator k, v;

	// Do the insertions
	//cout << " ...inserting keys: ";
	for (k = keys.begin(), v = values.begin(); k != keys.end();
	     ++k, ++v)
	{
		//cout << "(" << *k << ", " << *v << ")";
		tree.Insert (*k, *v);
		if (tree.Error())
		{
			cout << "***** Insert: internal check failure, "
			     << "key " << *k << ", value " << *v << endl;
		}
	}
	//cout << endl;

	//cout << "----- Snapshot -----" << endl;
	//tree.Print (cout);

	// Verify membership
	cout << " ...testing membership" << endl;
	err += T_Members (tree, keys);

	// Compare values
	cout << " ...comparing values" << endl;
	err += T_Compare (tree, keys, values);

	return (err);
}



int
T_Removal (test_tree &tree, vector<test_tree::key_type> &keys)
{
	// As removing, make sure the removed key cannot be found
	//cout << " ...removing key: ";
	int err = 0;
	vector<test_key>::iterator k;
	for (k = keys.begin(); k != keys.end(); ++k)
	{
		//cout << *k << " ";
		if (! tree.Remove (*k))
		{
			cout << "***** Removal: key missing: " << *k << endl;
			tree.Print (cout);
			++err;
		}
		if (tree.Error())
		{
			cout << "***** Removal: internal check failure, "
			     << "key " << *k << endl;
			++err;
		}
		if (tree.Find (*k))
		{
			cout << "***** Removal: removed key still found: "
			     << *k << endl;
			++err;
		}
	}
	//cout << endl;
	if (! tree.Empty ())
	{
		cout << "***** Removal: tree not empty. " << endl;
		tree.Print (cout);
		++err;
	}
	return (err);
}



int
T_RandomRemoval (test_tree &tree)
{
	// Build a vector of keys by traversing the tree, then shuffle
	// and remove.  We should be empty when its over.
	vector<test_tree::key_type> keys;
	test_tree::key_type key;

	tree.First ();
	//cout << " ...removal finding keys: ";
	while (tree.Current (&key))
	{
		keys.push_back (key);
		//cout << key << " ";
		tree.Next ();
	}
	//cout << endl;

	MLCG rng(time(0));
	RandomInteger rnd(&rng);
	random_shuffle (keys.begin(), keys.end(), rnd);
	return (T_Removal (tree, keys));
}




int
T_ReverseRemoval (test_tree &tree)
{
	// Build a vector of keys by traversing the tree backwards.
	vector<test_tree::key_type> keys;
	test_tree::key_type key;

	tree.Last ();
	//cout << " ...removal finding keys: ";
	while (tree.Current (&key))
	{
		keys.push_back (key);
		//cout << key << " ";
		tree.Prev ();
	}
	//cout << endl;
	return (T_Removal (tree, keys));
}




int
TestTree (test_tree &tree, int N)
{
	// Put a tree through its paces

	int err = 0;
	vector<test_key> keys(N);
	MLCG rng(time(0));
	RandomInteger rnd(&rng);

	generate (keys.begin(), keys.end(), Counter(1));

	// Insert the sequential keys and values and test.
	cout << "Sequential insert... " 
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);

	// Ordered removal
	cout << "Forward removal... " << endl;
	err += T_Removal (tree, keys);

	// Insert the sequential keys and values and test.
	cout << "Sequential insert... " 
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);

	// Reverse removal
	cout << "Reverse removal... " << endl;
	err += T_ReverseRemoval (tree);

	// Insert the sequential keys and values and test.
	cout << "Sequential test and erase... " 
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);
	tree.Erase ();
	if (! tree.Empty())
	{
		++err;
		cout << "***** Tree not empty after Erase()" << endl;
	}
	// Make sure operations fail on an empty tree
	test_tree::key_type value;
	err += (tree.Find (keys[0]));
	err += (tree.Remove (keys[0]));
	err += (tree.First ());
	err += (tree.Next ());
	err += (tree.Value (&value));
	err += (tree.Last ());
	err += (tree.Prev ());

	// Do the reverse test
	reverse (keys.begin(), keys.end());
	cout << "Reverse insert... "
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);

	// Random overwrites, new values
	cout << "Random overwrites... " << endl;
	vector<test_key> values = keys;
	random_shuffle (keys.begin(), keys.end(), rnd);
	err += T_Insert (tree, keys, values);

	// Random removal
	cout << "Random removal... " << endl;
	err += T_RandomRemoval (tree);

	// Do the above tests a few times with randomly inserted keys
	for (int i = 0; i < 3; ++i)
	{
		srand (time(0));
		random_shuffle (keys.begin(), keys.end(), rnd);

		cout << "Random insertions... " << i+1 << endl;
		err += T_Insert (tree, keys, keys);
		cout << "Random removal...... " << i+1 << endl;
		err += T_RandomRemoval (tree);
	}

	return err;
}



