/*
 * Test the BTree
 */

#include <iostream.h>
#include <assert.h>
#include <stdlib.h>
#include <vector>
#include <function.h>
#include <algorithm>

#define RANDOM 0

#if RANDOM
#include <MLCG.h>	// GNU c++ lib
#include <RndInt.h>
#endif

#include <time.h>	// Need time() to seed srand()

#include "SerialZTime.hh"
#include "BTree.hh"
#include "BTreeStats.hh"
#include <string>
#include "Logger.hh"

#include "BTreeFile.hh"

//typedef BTreeFile<ZTime,ZTime> TimeTree;
//typedef BTreeFile<string,string> StringTree;

/*
 * Choose the test_key type for the test trees.
 */
//typedef long test_key;
//typedef string test_key;
typedef ZTime test_key;

typedef BTreeFile<test_key,test_key> test_tree;
//typedef BTree<test_key,test_key> test_tree;

static int Debug = 0;

#ifdef linux
extern "C" { int __getfpucw (); }

int __getfpucw ()
{
	return 0;
}
#endif



// Report statistics about the given tree
template <class K, class T>
Summarize (ostream &out, BTree<K,T> &t)
{
	BTreeStats s;
	t.Statistics (s);

	out << "             Depth: " << t.Depth() << endl;
	out << "Number of elements: " << s.numElements() << endl;
	out << "   Number of nodes: " << s.numNodes() << endl;
	out << "    Number of keys: " << s.numKeys() << endl;
	out << "   Total key slots: " << s.totalSlots() << endl;
	out << "  Key slots unused: " << s.keysUnused() << endl;
	out << " Elem slots unused: " 
	    << (t.Order()*s.numLeaves() - s.numElements()) << endl;
	out << " Avg keys per node: " << s.averageKeys() << endl;
	out << " Max keys per node: " << s.maxKeys() << endl;
	out << " Min keys per node: " << s.minKeys() << endl;
	out << "  Memory allocated: " << s.memoryAlloc() << " bytes." << endl;
	out << "       Memory used: " << s.memoryUsed() << " bytes." << endl;
	out << "   Pct memory used: " << s.percentMemory() << "%" << endl;
	out << "  Node memory used: " 
	    << s.percentNodeMemory() << "%" << endl;
	out << "Element buffer use: " 
	    << s.percentElementMemory() << "%" << endl;
	out << "Pct key slots used: " << s.percentSlots() << "%" << endl;
}



int TestTree (test_tree &tree, int N);
int T_Traversal (test_tree &tree, int print = 0);


int main (int argc, char *argv[])
{
	int N = 10;
	int order = 3;

	srand (1000);

	Logger::SetPrototype (NullLogger("logging disabled"));

	cout << "-----------------================----------------" << endl;
	if (argc > 1)
		N = atoi(argv[1]);
	if (argc > 2)
		order = atoi(argv[2]);

	int err = 0;
	if (argc > 1)
	{{
		test_tree tree(order, sizeof(test_key));
		tree.Check();
		tree.Print(cout);
		err += T_Traversal (tree, 1);
		tree.Check(1);
		tree.Erase ();	// Start fresh
		cout << "Testing tree of order " << tree.Order() 
		     << " with " << N << " members..." << endl;
		err += TestTree (tree, N);
	}
	if (err) cout << "***** ";
	cout << err << " errors." << endl;
	exit (err);
	}

	// Perform the tests on trees of various orders, using a bigger N
	if (argc <= 1)
		N = 5000;
	for (int o = 3; o < N / 10; o *= 3)
	{
		test_tree tree(o, sizeof(test_key));
		tree.Erase ();	// Start fresh
		
		cout << "Testing tree of order " << tree.Order() 
		     << " with " << N << " members..." << endl;
		err += TestTree (tree, N);
	}
	
	if (err) cout << "***** ";
	cout << err << " errors." << endl;
	exit (err);
}




// To test a tree, use sequential and random generators of integers.  Try
// inserting in order, then in reverse, then randomly.  After each
// insertion sequence, verify that all the keys can be found and have the
// correct values.  Make sure traversing the tree forward and reverse
// returns the correct keys and values.  Reinsert all the keys with a new
// value and verify those values.  Then remove all the keys randomly, and
// verify the tree is empty after it all.

// Use internal checking and automatic aborting.


/*
 * Simple generator for keys.
 */
template <class K>
struct Counter
{
	typedef K result_type;
	typedef long counter_type;

	Counter (counter_type _n = counter_type()) : n(_n) {}
	K operator()();

	counter_type n;
};


template <class K>
K Counter<K>::operator()()
{
	return K(n++);
}


template <>
string
Counter<string>::operator()()
{
	string s;
	cin >> s;
	return s;
}


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
T_Compare (test_tree &tree, vector<test_key> &keys, 
	   vector<test_tree::value_type> &values)
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
		if (Debug) tree.Print (cout);
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
T_Removal (test_tree &tree, 
	   vector<test_key>::iterator k, 
	   vector<test_key>::iterator last, int check_empty = 1)
{
	test_tree::value_type v0, v;	// Accept default initialization

	// As removing, make sure the removed key cannot be found
	//cout << " ...removing key: ";
	int err = 0;
	for ( ; k != last; ++k )
	{
		//cout << *k << " ";
		if (! tree.Remove (*k))
		{
			cout << "***** Removal: key missing: " << *k << endl;
			if (Debug) tree.Print (cout);
			++err;
		}
		if (tree.Error())
		{
			cout << "***** Removal: internal check failure, "
			     << "key " << *k << endl;
			++err;
		}
		if (tree.Find (*k, &v))
		{
			cout << "***** Removal: removed key still found: "
			     << *k << " value: " << v << endl;
			++err;
		}
		else if (v != v0)
		{
			cout << "***** Removal: key not found: "
			     << *k << " but value still set: " << v << endl;
			++err;
		}
		if (Debug) tree.Print (cout);
	}
	//cout << endl;
	if (check_empty && ! tree.Empty ())
	{
		cout << "***** Removal: tree not empty. " << endl;
		if (Debug) tree.Print (cout);
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

#if RANDOM
	MLCG rng(time(0));
	RandomInteger rnd(&rng);
	random_shuffle (keys.begin(), keys.end(), rnd);
#endif
	random_shuffle (keys.begin(), keys.end());
	return (T_Removal (tree, keys.begin(), keys.end()));
}




int
T_PartialRemoval (test_tree &tree, int n)
{
	int err = 0;
	// Build a vector of keys by traversing the tree, then shuffle
	// and remove n keys.  Report statistics, then re-insert those
	// n keys.
	//typedef pair<test_tree::key_type,test_tree::value_type> Pair; 
	//vector<Pair> entries;
	vector<test_tree::key_type> keys;
	vector<test_tree::value_type> values;
	test_tree::key_type key;
	test_tree::value_type val;
	vector<test_key>::iterator k;

	tree.First ();
	//cout << " ...removal finding keys: ";
	while (tree.Current (&key))
	{
		keys.push_back (key);
		//cout << key << " ";
		tree.Next ();
	}
	//cout << endl;

	random_shuffle (keys.begin(), keys.end());
	random_shuffle (keys.begin(), keys.end());
	for (k = keys.begin(); k != keys.end(); ++k)
	{
		err += (! tree.Find (*k, &val));
		values.push_back (val);
	}

	err += T_Removal (tree, keys.begin(), keys.begin()+n, 0);
	Summarize(cout, tree);

	// Now re-insert everything; some will be overwrites
	err += T_Insert (tree, keys, values);
	Summarize(cout, tree);

	return (err);
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
	return (T_Removal (tree, keys.begin(), keys.end()));
}





int
T_Traversal (test_tree &tree, int print = 0)
{
	// Build an ordered vector of keys by traversing the tree forwards.
	vector<test_tree::key_type> keys;
	vector<test_tree::key_type>::iterator iv;
	test_tree::key_type key;
	int err = 0;

	tree.First ();
	if (print)
		cout << "Traversal keys: ";
	while (tree.Current (&key))
	{
		if (print)
			cout << key << " ";
		keys.push_back (key);
		tree.Next ();
	}
	if (print) cout << endl;

	// Now do the same traversal backwards, and make sure we get the
	// same keys coming the other direction.
	tree.Last ();
	iv = keys.end();
	while (iv != keys.begin() && tree.Current (&key))
	{
		--iv;
		if (*iv != key)
		{
			cout << "***** Traversal backwards: expected key "
			     << *iv << ", got key " << key << endl;
			++err;
		}
		tree.Prev ();
	}
	if (iv != keys.begin() || tree.Current())
	{
		++err;
		cout << "***** Traversal backwards: did not exhaust tree"
		     << endl;
	}
	
	// Now traverse forwards in increments of 1/10 the size.
	iv = keys.begin();
	int step = keys.size() / 10 + 1;
	cout << " ...stepping forward by " << step << ": ";
	tree.First ();
	while (tree.Current (&key))
	{
		cout << key << " ";
		if (*iv != key)
		{
			cout << "***** Traversal error: expected key "
			     << *iv << ", got key " << key << endl;
			++err;
		}
		iv += step;
		tree.Next (step);
	}
	cout << endl;
	
	// Now traverse backwards in increments of 1/13 the size.
	iv = keys.end() - 1;
	step = keys.size() / 13 + 1;
	cout << " ...stepping backwards by " << step << ": ";
	tree.Last ();
	while (tree.Current (&key))
	{
		cout << key << " ";
		if (*iv != key)
		{
			cout << "***** Traversal error: expected key "
			     << *iv << ", got key " << key << endl;
			++err;
		}
		iv -= step;
		tree.Prev (step);
	}
	cout << endl;
	return err;
}



#ifdef notdef
int
Reopen (test_tree &tree)
{
	cout << " ### Re-opening tree ###" << endl;
	delete
}
#endif



int
TestTree (test_tree &tree, int N)
{
	// Put a tree through its paces.  Check() it after major operations.

	int err = 0;
	vector<test_key> keys(N);
#if RANDOM
	MLCG rng(time(0));
	RandomInteger rnd(&rng);
#endif
	generate (keys.begin(), keys.end(), Counter<test_key>(1));

	// Insert the sequential keys and values and test.
	cout << "Sequential insert... " 
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);
	err += tree.Check ();
	Summarize(cout, tree);
	err += T_Traversal (tree);

	//return (err);

	// Ordered removal
	cout << "Forward removal... " << endl;
	err += T_Removal (tree, keys.begin(), keys.end());
	err += tree.Check ();

	// Insert the sequential keys and values and test.
	cout << "Sequential insert... " 
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);
	err += tree.Check ();

	// Reverse removal
	cout << "Reverse removal... " << endl;
	err += T_ReverseRemoval (tree);
	err += tree.Check ();

	// Insert the sequential keys and values and test.
	cout << "Sequential test and erase... " 
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);
	err += tree.Check ();
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

	// Sequential insert and partial removal and re-insert
	cout << "Sequential insert and partial removal... " 
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);
	err += tree.Check ();
	Summarize(cout, tree);
	cout << "Removal of half of keys..." << endl;
	err += T_PartialRemoval (tree, N/2);
	cout << "Random removal..." << endl;
	err += T_RandomRemoval (tree);
	err += tree.Check ();

	// Do the reverse test
	reverse (keys.begin(), keys.end());
	cout << "Reverse insert... "
	     << *keys.begin() << " to " << keys.back() << endl;
	err += T_Insert (tree, keys, keys);
	err += tree.Check ();
	Summarize(cout, tree);

	// Test traversal
	err += T_Traversal (tree);

	// Random overwrites, new values
	cout << "Random overwrites... " << endl;
	vector<test_key> values = keys;
	random_shuffle (keys.begin(), keys.end());
	err += T_Insert (tree, keys, values);
	err += tree.Check ();
	Summarize(cout, tree);

	// Random removal
	cout << "Random removal... " << endl;
	err += T_RandomRemoval (tree);
	err += tree.Check ();

	// Do the above tests a few times with randomly inserted keys
	for (int i = 0; i < 3; ++i)
	{
		srand (time(0)+(i << (2*i)));
		random_shuffle (keys.begin(), keys.end());

		cout << "Random insertions... " << i+1 << endl;
		err += T_Insert (tree, keys, keys);
		err += tree.Check ();
		Summarize(cout, tree);
		cout << "Partial removal and re-insertion..." << endl;
		err += T_PartialRemoval (tree, N/2);
		err += tree.Check ();
		cout << "Random removal...... " << i+1 << endl;
		err += T_RandomRemoval (tree);
		err += tree.Check ();
	}

	return err;
}


