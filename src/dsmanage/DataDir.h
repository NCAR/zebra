//
// The datadir class.
//

class DataDir
{
	char *directory;
public:
	DataDir (char *);
	~DataDir ();
	int FreeSpace ();
};



//
// Data files are described by the following class:
//
class dsFile
{
	char	*fname;		// Name of this file (w/o directory)
	int	fsize;		// How big it is.
public:
	int	index;		// SHM DF index
	dsFile (char *, int);
	dsFile (const dsFile &);
	inline ~dsFile () { delete[] fname; }
	inline int size () const { return fsize; }
	inline char *name () const { return fname; }
};

//
// Platforms.
//
class dsPlatform
{
	char	*pname;		// Name of this platform.
public:
	int	index;		// It's index
	IContainer<dsFile> files; // The files
	dsPlatform (char *, int);
	dsPlatform (const dsPlatform &);
	float space() const ;		// How much space it takes.
	int ndfile () const { return files.ncontained (); }
	char *name () const { return pname; }
};
