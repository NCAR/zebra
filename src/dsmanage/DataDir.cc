//
// Manage info on data directories.
//
# include <sys/types.h>
# include <sys/vfs.h>
# include <sys/stat.h>
# include <stream.h>

# include "container.h"
# include "DataDir.h"

extern "C" {
	int stat (const char *, struct stat *);
	int statfs (const char *, struct statfs *);
# ifndef __GNUC__
	int strlen (char *);
	char *strcpy (char *, char *);
# endif
};


//
// The data directory class.
//

DataDir::DataDir (char *dir)
//
// Initialize one of these babies.
//
{
	directory = new char[strlen (dir) + 1];

	strcpy (directory, dir);
}


DataDir::~DataDir ()
//
// Get rid of it.
//
{
	delete[] directory;
}




int DataDir::FreeSpace ()
//
// How much space is free here?
//
{
	struct statfs stbuf;

	if (statfs (directory, &stbuf) < 0)
		return (-1);
	return (stbuf.f_bsize*stbuf.f_bavail);
}



//
// Data file routines.
//

dsFile::dsFile (char *name, int findex)
//
// Set up this dsFile structure.
//
{
	struct stat buf;
//
// Allocate and store the name.
//
	fname = new char[strlen (name) + 1];
	strcpy (fname, name);
//
// Go ahead and look up the size.
//
	if (stat (name, &buf) < 0)
		cerr << "Unable to stat file " << name << "\n";
	else
		fsize = buf.st_size;
	index = findex;
}





dsFile::dsFile (const dsFile& old)
//
// The initialization case.
//
{
	fname = new char[strlen (old.fname) + 1];
	strcpy (fname, old.fname);
	fsize = old.fsize;
	index = old.index;
}




dsPlatform::dsPlatform (char *name, int pindex)
//
// Create a platform.
//
{
	pname = new char[strlen(name) + 1];
	strcpy (pname, name);
	index = pindex;
}



dsPlatform::dsPlatform (const dsPlatform& old)
//
// Initializer.
//
{
	pname = new char[strlen (old.pname) + 1];
	strcpy (pname, old.pname);
	index = old.index;
}



float
dsPlatform::space () const
//
// How much space?
//
{
	int df;
	float total = 0;

	for (df = 0; df < files.ncontained (); df++)
		total += files.nth(df).size ();
	return (total/1024000.0);
}
