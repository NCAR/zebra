/*
 * Test the block interfaces.
 */

// Little sense running the test if asserts not on
#ifdef NDEBUG
#undef NDEBUG
#endif

#include <iostream>
#include <fstream>
#include <assert.h>

#include "BlockFile.hh"
#include "Logger.hh"
#include "Format.hh"

//RCSID ("$Id: T_Block.cc,v 1.9 2002-09-17 20:00:19 granger Exp $")

static int TestStore (char *name);

using std::ofstream;
using std::endl;
using std::cout;

int main (int, char *[])
{
    int errors = 0;
{
	ofstream lf("tblocks.log");
	StreamLogger log(lf);

	char *s = 0;
	cout << Format("null char *: %s") % s << endl;

	// Set the default logger.
	Logger::Prototype (log);

	cout << "-----------------------------------------------" << endl;

	// Create a block file
	BlockFile *bf = new BlockFile();

	log.Info ("Creating a new file:");
	assert (bf->Status() == BlockFile::NOT_OPEN);
	bf->Create ("test.bf");
	assert (bf->Status() == BlockFile::OK);
	cout << *bf << endl;
	delete bf;

	// Try opening the first
	log.Info ("Opening the first file:");
	bf = new BlockFile ("test.bf");
	assert (bf->Status() == BlockFile::OK);
	cout << *bf << endl;
	delete bf;

	// Try a second one over the first
	log.Info ("Overwriting first file:");
	BlockFile B("test.bf", 1234, BlockFile::BF_CREATE);
	assert (B.Status() == BlockFile::OK);
	cout << endl << B;

	assert (B.Close () == BlockFile::NOT_OPEN);
	assert (B.Status() == BlockFile::NOT_OPEN);

	log.Info ("Testing storage on second block file:");
	errors += TestStore ("test.bf");

}
 return errors;
}


int
TestStore (char *name)
{
    	int errors = 0;
	BlockFile *store = new BlockFile (name);

#	define N 50
	const int ndata = 500;
	unsigned long size = (128 * (N+1)) * sizeof(int);
	int *data = (int *) malloc (size);
	int i;
	size = ndata * sizeof(int);
	for (i = 0; i < ndata; ++i)
	{
		data[i] = ndata - i;
	}

	unsigned long len;
	BlkOffset buf = store->Alloc (size, &len);
	store->Write (buf, data, size);
	memset (data, 0, size);

	store->Read (data, buf, size);
	for (i = 0; i < ndata; ++i)
	{
		assert (data[i] == ndata - i);
	}

	store->Free (buf, len);

	BlkOffset blocks[N];
	BlkSize sizes[N];
	BlkVersion old;
	
	// Allocate several blocks
	for (i = 0; i < N; ++i)
	{
		blocks[i] = store->Alloc (128 * (i+1), &sizes[i]);
	}

	// Put data into each block
	for (i = 0; i < N; ++i)
	{
		for (unsigned j = 0; j < sizes[i]/sizeof(int); ++j)
		{
			data[j] = sizes[i];
		}
		old = store->Revision ();
		store->Write(blocks[i], data, sizes[i]);
		if (! store->Changed (old, blocks[i], sizes[i]))
		{
			cout << "Error: block " << i
			     << ", offset " << blocks[i]
			     << ", size " << sizes[i]
			     << "no change since revision " << old << endl;
			cout << "Current file revision: " 
			     << store->Revision() << endl;
			++errors;
		}
	}

	// Verify the data in each block
	for (i = 0; i < N; ++i)
	{
		old = store->Revision ();
		store->Read (data, blocks[i], sizes[i]);
		for (unsigned j = 0; j < sizes[i]/sizeof(int); ++j)
		{
			assert ((unsigned)data[j] == sizes[i]);
		}
		if (store->Changed (old, blocks[i], sizes[i]))
		{
			cout << "Error: block " << i
			     << ", offset " << blocks[i]
			     << ", size " << sizes[i]
			     << "changed since revision " << old << endl;
			cout << "Current file revision: " 
			     << store->Revision() << endl;
			++errors;
		}
	}

	store->Close ();

	cout << "After allocating all blocks and closing:" << endl;
	cout << *store;

	store->Open (name);

	cout << "After re-opening:" << endl;
	cout << *store;

	// Free half of them
	for (i = 0; i < N; i += 2)
	{
		store->Free (blocks[i], sizes[i]);
	}

	cout << "After freeing half of the blocks:" << endl;
	cout << *store;

	// Finally deallocate the rest of the blocks
	for (i = 1; i < N; i += 2)
	{
		store->Free (blocks[i], sizes[i]);
	}

	cout << "After freeing all of the blocks:" << endl;
	cout << *store;

	delete store;
	free (data);
	return errors;
}



