/*
 * Dump block file info.
 */

#include <iostream.h>

#include "BlockFile.hh"

// RCSID ("$Id: dbf.cc,v 1.1 1998-05-28 21:45:06 granger Exp $")

int main (int argc, char *argv[])
{
	if (argc < 2)
	{
		cerr << "Usage: " << argv[0] << " blockfile [blockfile ...]"
		     << endl;
		exit (1);
	}

	// Create a block file
	BlockFile *bf = new BlockFile();
	for (int i = 1; i < argc; ++i)
	{
		cout << "-------------------------------------------" << endl;
		if (bf->Open (argv[i]) != BlockFile::OK)
		{
			cerr << "Error opening: " << argv[i] << endl;
		}
		else
		{
			cout << *bf << endl;
			bf->Close ();
		}
	}

	delete bf;
	exit (0);
}

