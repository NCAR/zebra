//
// Platforms.
//
class dsPlatform
{
	char	*pname;		// Name of this platform.
public:
	int	index;		// It's index
	fContainer files; // The files
	dsPlatform (char *, int);
	dsPlatform (const dsPlatform &);
	float space() const ;		// How much space it takes.
	int ndfile () const { return files.ncontained (); }
	char *name () const { return pname; }
};
