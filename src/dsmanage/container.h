//
// A simple, container for classes with an "index" member.
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


template<class C> class IContainer
{
	int nalloc;		// Number we have allocated;
	C **stuff;		// The vector of things.
	int nelem;		// Number of containees;
public:
	IContainer (int n = 10);
	~IContainer ();
	void add (const C&);		// Add an element to the container
	inline int ncontained() const { return nelem; }	// How many?
	C& nth (const int which) const { return *(stuff[which]); }
	C *index (const int) const;	// Get by index
	void zap (int);		// Zap the nth element
	int pos (int index) const
		{
			int i;
			for (i = 0; i < nelem; i++)
				if (stuff[i]->index == index)
					return (i);
			return (-1);
		}
};




template<class C> IContainer<C>::IContainer (int initial)
//
// Create an IContainer.
//
{
	nalloc = initial;
	nelem = 0;
	stuff = new C*[nalloc];
}



template <class C> IContainer<C>::~IContainer ()
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




template <class C> void IContainer<C>::add (const C& addee)
//
// Add a new guy to this container.
//
{
//
// Make sure our array is big enough.
//
	if ((nelem + 1) >= nalloc)
	{
		C **newstuff = new C*[nalloc*2];
		memcpy (newstuff, stuff, nalloc * sizeof (C*));
		nalloc *= 2;
		delete[] stuff;
		stuff = newstuff;
	}
//
// Now just add the new thing.
//
	stuff[nelem++] = new C (addee);
}




# ifdef notdef
template <class C> C& IContainer<C>::nth (const int which) const
//
// Return the nth element.
//
{
	if (which < 0 || which >= nelem)
	{
		std::cerr << "NTH value " << which << " out of range\n";
		which = 0;
	}
	return *(stuff[which]);
}
# endif



template <class C> void IContainer<C>::zap (const int which)
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



template <class C> C *IContainer<C>::index (const int ind) const
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
