/*
 * Test the block interfaces.
 */

#include <iostream.h>
#include <assert.h>

#include <defs.h>
#include "BlockFile.hh"

RCSID ("$Id: T_Block.cc,v 1.1 1997-11-24 10:44:10 granger Exp $")

static int TestStore (BlockStore *store);

int main (int argc, char *argv[])
{
	Logger log;

	// Create a block file
	BlockFile *bf = new BlockFile();

	assert (bf->Status() == BlockFile::NOT_OPEN);
	bf->Create ("test.bf");
	assert (bf->Status() == BlockFile::OK);
	cout << endl << *bf << endl;
	log.Debug ("Setting version to 1.");
	bf->SetAppVersion (1);
	cout << endl << *bf << endl;
	delete bf;

	// Try opening the first
	bf = new BlockFile ("test.bf");
	assert (bf->Status() == BlockFile::OK);
	log.Info ("Checking that app version is correct in first file");
	cout << endl << *bf << endl;
	assert (bf->AppVersion() == 1);

	// Try a second one over the first
	log.Info ("Overwriting original file");
	BlockFile B("test.bf", 1234, BlockFile::BF_CREATE);
	assert (B.Status() == BlockFile::OK);
	assert (B.AppVersion() == 0);
	cout << endl << B;
	B.SetAppVersion (2);
	cout << endl << B;

	// Open a heap store
	BlockHeap *heap = new BlockHeap();
	log.Info ("Created BlockHeap()");

	log.Info ("Testing storage on heap:");
	TestStore (heap);
	log.Info ("Testing storage on second block file:");
	TestStore (&B);
	cout << B;

	assert (B.Close () == BlockFile::NOT_OPEN);
	assert (B.Status() == BlockFile::NOT_OPEN);
	delete heap;

	exit (0);
}




static int
TestStore (BlockStore *store)
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

#ifdef notdef

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

#endif /* notdef */

	return (0);
}


