/*
 * Test the block interfaces.
 */

#include <iostream.h>
#include <assert.h>

#include <defs.h>
#include "BlockFile.hh"
#include "Logger.hh"

RCSID ("$Id: T_Block.cc,v 1.4 1997-12-14 23:50:16 granger Exp $")

static int TestStore (char *name);

int main (int, char *[])
{
	Logger log;

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
	TestStore ("test.bf");

	exit (0);
}


int
TestStore (char *name)
{
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
	return (0);
}





#ifdef notdef
static int
TestStore (BlockFile *store)
{
	const int ndata = 200;
	const int size = ndata * sizeof(int);
	int *data = (int *) malloc (size);
	for (int i = 0; i < 200; ++i)
	{
		data[i] = 200 - i;
	}

	BlkOffset buf = store->Alloc (size);
	store->Write (buf, data, size);
	memset (data, 0, size);

	int *more = (int *) store->Read (data, buf, size);
	assert (more == data);
	for (int i = 0; i < 200; ++i)
	{
		assert (data[i] == 200 - i);
	}

	store->Free (buf);


	BlkOffset blocks[10];
	// BlockAddr addr[10];
	
	// Allocate several blocks
	for (int i = 0; i < 10; ++i)
	{
		blocks[i] = store->Alloc (2 << (i+3));
		// addr[i] = blocks[i]->Address();	// Preserve the addr
		// assert (blocks[i]->Store() == store);
	}

	// Free half of them

	for (int i = 0; i < 10; i += 2)
	{
		store->Free (blocks[i]);
	}


	// Put data into each block's buffer
	for (int i = 0; i < 8; ++i)
	{
		int size;
		 *buf = blocks[i]->GetBuffer (&size);
		int *data = (int *)buf;
		assert (buf != NULL);
		assert (size == 2^(i+3));
		for (int j = 0; j < size/sizeof(int); ++j)
		{
			data[j] = size;
		}
		blocks[i]->Write();
	}

	// Delete the original blocks
	for (int i = 0; i < 8; ++i)
	{
		delete blocks[i];
	}

	// Try to read the blocks using the block addresses
	for (int i = 0; i < 8; ++i)
	{
		int size = 2^(i+3);
		blocks[i] = store->Read (addr[i], size);
		assert (blocks[i]->Store() == store);
	}

	// Verify the data in each block's buffer
	for (int i = 0; i < 8; ++i)
	{
		int size;
		Buffer *buf = blocks[i]->GetBuffer (&size);
		int *data = (int *)buf;
		assert (buf != NULL);
		assert (size == 2^(i+3));
		for (int j = 0; j < size/sizeof(int); ++j)
		{
			assert (data[j] == size);
		}
	}

	// Finally deallocate all the blocks
	for (int i = 0; i < 8; ++i)
	{
		assert (blocks[i]->Store() == store);
		blocks[i]->Free();
		delete blocks[i];
	}

	return (0);
}
#endif /* notdef */


