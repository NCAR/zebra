//
// dsPlatform class
//
# ifndef _dsPlatform_h_
# define _dsPlatform_h_

# include "fcontainer.h"

class dsPlatform
{
	char	*pname;		// Name of this platform.
public:
	int index;		// Its index
	fContainer files;	// The files
	dsPlatform (const char *, int);
	dsPlatform (const dsPlatform &);
	~dsPlatform () { delete [] pname; }
	float space() const ;	// How much space it takes.
	int ndfile () const { return files.ncontained (); }
	char *name () const { return pname; }
};

# endif // _dsPlatform_h_
