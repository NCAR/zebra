//
// un-templatized version of container for the dsFile type.
//
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# ifndef _fcontainer_h_
# define _fcontainer_h_

# include "dsFile.h"

class fContainer
{
	int nalloc;		// Number we have allocated;
	dsFile **stuff;	// The vector of things.
	int nelem;		// Number of containees;
public:
	fContainer (int n = 10);
	~fContainer ();
	void add (const dsFile&);	// Add an element to the container
	inline int ncontained() const { return nelem; }	// How many?
	dsFile& nth (const int which) const { return *(stuff[which]); }
	dsFile *index (const int) const;	// Get by index
	void zap (int);		// Zap the nth element
# ifdef notdef
	int pos (int index) const
		{
			int i;
			for (i = 0; i < nelem; i++)
				if (stuff[i]->index == index)
					return (i);
			return (-1);
		}
# endif
};



inline
fContainer::fContainer (int initial)
//
// Create an fContainer.
//
{
	nalloc = initial;
	nelem = 0;
	stuff = new dsFile * [nalloc];
}


inline
fContainer::~fContainer ()
//
// Bye bye
//
{
	int i;
//
// Free up the individual elements, so that their destructors get called.
//
	for (i = 0; i < nelem; i++)
		delete stuff[i];
	delete[] stuff;
}



inline
void fContainer::add (const dsFile& addee)
//
// Add a new guy to this container.
//
{
//
// Make sure our array is big enough.
//
	if ((nelem + 1) >= nalloc)
	{
		dsFile **newstuff = new dsFile*[nalloc*2];
		memcpy (newstuff, stuff, nalloc * sizeof (dsFile*));
		nalloc *= 2;
		delete[] stuff;
		stuff = newstuff;
	}
//
// Now just add the new thing.
//
	stuff[nelem++] = new dsFile (addee);
}




# ifdef notdef
template <class C> C& fContainer<C>::nth (const int which) const
//
// Return the nth element.
//
{
	if (which < 0 || which >= nelem)
	{
		cerr << "NTH value " << which << " out of range\n";
		which = 0;
	}
	return *(stuff[which]);
}
# endif


inline
void fContainer::zap (const int which)
//
// Delete an element.
//
{
	int i;
//
// Range checking.
//
	if (which < 0 || which >= nelem)
	{
		std::cerr << "NTH value " << which << " out of range\n";
		return;
	}
//
// Now get rid of it.
//
	delete stuff[which];
	for (i = which; i < nelem - 1; i++)
		stuff[i] = stuff[i + 1];
	nelem--;
}


# ifdef notdef
inline
dsFile *fContainer::index (const int ind) const
//
// Look up an entry by index.
//
{
	int i;

	for (i = 0; i < nelem; i++)
		if (stuff[i]->index == ind)
			return (stuff[i]);
	return (0);
}
# endif

# endif
