//
// Implementation of the index class.
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

# ifdef hpux
# include <sys/sigevent.h>
# endif
extern "C"
{
#	include <defs.h>
};
# include <stream.h>
# include <stdio.h>
# include "STable.h"
# include "Index.h"
# include "ZTime.h"

MAKE_RCSID ("$Id: Index.cc,v 1.12 2001-08-27 20:00:16 granger Exp $");

//
// Internal structures
//
class DirList
{
public:
	inline DirList (void) { count = 0; files = NULL; }
	inline DirList (IndexFile *file)
	{ 
		count = 1;
		files = new IndexFile * [1];
		files[0] = file;
	}
	~DirList (void) 
	{ 
		if (files)
			delete[] files;
	}
	void add (IndexFile *file);
	inline IndexFile *nth (int n) { return files[n]; }
	inline int ndirs (void) { return count; }
private:
	int count;
	IndexFile **files;
};
	

//
// Forwards.
//
int ZapPlat (const char *name, int type, const SValue *v, long junk);
static int ZapDirList (const char *name, int type, const SValue *v, long junk);
static int WritePlat (const char *name, int type, const SValue *v, long lfp);

//
// Platform index symbol table number.
//
int PlatformIndex::TNum = 0;

void
DirList::add (IndexFile *file)
{
	IndexFile **newfiles = new IndexFile * [count + 1];
	if (count)
	{
		memcpy (newfiles, files, count * sizeof(IndexFile *));
		delete[] files;
	}
	files = newfiles;
	files[count++] = file;
}

//
// IndexFile routines.
//

IndexFile::IndexFile (const char *plat, const char *name, int size, int fileno,
		const ZebTime *begin, const ZebTime *end)
//
// Create an IndexFile.
//
{
	if_next = 0;			// No chain yet
	if_same = 0;
	if_name = new char[strlen (name) + 1];
	strcpy (if_name, name);
	if_plat = new char[strlen (plat) + 1];
	strcpy (if_plat, plat);
	if_size = size;
	if_fileno = fileno;
	if_begin = *begin;
	if_end = *end;
	if_marked = 0;
}





IndexFile::IndexFile (const IndexFile &old)
//
// Initialization constructor.
//
{
	*this = old;
	if_name = new char[strlen (old.if_name) + 1];
	strcpy (if_name, old.if_name);
	if_plat = new char[strlen (old.if_plat) + 1];
	strcpy (if_plat, old.if_plat);
	if_next = 0;		// Must find its own chain
	if_same = 0;
}






//
// Platform index stuff.
//
PlatformIndex::PlatformIndex ()
{
	char tname[30];
	
	sprintf (tname, "PlatformIndex-%d", TNum);
	table.init (tname);
	sprintf (tname, "PlatformIndexDirs-%d", TNum++);
	dirs.init (tname);
}



void
PlatformIndex::add (const char *plat)
//
// Make sure this platform is represented, with no files.
//
{
	PlatInfo *pi;
//
// See if there is a platform info structure for this platform.  If
// not, make one and store it.
//
	if (! (pi = findPlat (plat)))
	{
		pi = new PlatInfo;
		pi->pi_files = 0;
		pi->pi_marked = 0;
		table.set (plat, (void *) pi);
	}
}






void
PlatformIndex::add (const char *plat, IndexFile &file)
//
// Add a file to this platform.
//
{
	PlatInfo *pi;
	const ZebTime &fend = file.end ();
//
// See if there is a platform info structure for this platform.  If
// not, make one and store it.
//
	if (! (pi = findPlat (plat)))
	{
		pi = new PlatInfo;
		pi->pi_begin = file.begin ();
		pi->pi_end = file.end ();
		pi->pi_marked = 0;
		pi->pi_files = 0;
		table.set (plat, (void *) pi);
	}
//
// Before adding to the list, see if we have expanded the time bounds.
//
	if (file.begin () < pi->pi_begin)
		pi->pi_begin = file.begin ();
	if (file.end () > pi->pi_end)
		pi->pi_end = file.end ();
//
// See if the easy case is true, and we can prepend this file to the list.
//
	int placed = 0;
	if (! pi->pi_files || fend <= pi->pi_files->begin())
	{
		file.link (*pi->pi_files);
		pi->pi_files = &file;
		placed = 1;
	}
//
// We need to go through the chain if the file wasn't prepended above
//
	IndexFile *chain;
	for (chain = pi->pi_files; !placed && chain->next (); 
	     chain = chain->next ())
	{
		if (fend < chain->next ()->begin ())
		{
		//
		// Avoid overlapping files
		//
			if (chain->end() < file.begin())
			{
				file.link (*(chain->next ()));
				chain->link (file);
				placed = 1;
			}
			break;
		}
	}
//
// If we're at the end at it's not added yet, then try the end.
//
	if (!placed && !chain->next() && (chain->end() < file.begin()))
	{
		chain->link (file);
		placed = 1;
	}
//
// If it was added, check for other entries in the index for the same file.
//
	if (placed)
		FindDuplicates (file);
//
// If it still isn't placed, then it must overlap somewhere
//
#ifdef DEBUG
	else
	{
		printf ("%s overlaps %s\n", file.name(),
			chain->name());
	}
#endif
}




void
PlatformIndex::FindDuplicates (IndexFile &file)
//
// Check for other references to this same file in other platforms
//
{
	char *slash;
	char *dir;
	//
	// Extract the directory of this file and see if any other
	// files have been put in this directory.  If a platform 
	// other than the current one has used this directory,
	// check that platform's files for duplicates.
	//
	dir = new char[strlen(file.name()) + 1];
	strcpy (dir, file.name());
	if (slash = strrchr (dir, '/'))
		*slash = '\0';
	else
	{
		delete[] dir;
		return;
	}
	void *vptr;
	DirList *samedirs;
	if (! dirs.get (dir, vptr))
	{
		samedirs = new DirList(&file);
		dirs.set (dir, (void *)samedirs);
	}
	else
	{
		int i;
		int platfound = 0;
		samedirs = (DirList *) vptr;
		for (i = 0; i < samedirs->ndirs(); ++i)
		{
			const char *plat = samedirs->nth(i)->plat();
			//
			// Compare this file with each of the files in each
			// of the platforms represented in this list.  Returns
			// true when a duplicate is found and the search
			// can be ended.
			//
			if (!strcmp (plat, file.plat()))
				platfound = 1;
			else if (SameFile (plat, &file))
				break;
		}
		if (!platfound)
			samedirs->add (&file);
	}
	delete[] dir;
}




int
PlatformIndex::SameFile (const char *name, IndexFile *file)
//
// Check the platform for and files with the same path.  Return
// true if a duplicate is found.
//
{
	PlatInfo *pi = (PlatInfo *) findPlat (name);

	IndexFile *chain = pi->pi_files;
	while (chain)
	{
		if (chain != file && 
		    (strcmp (chain->name(), file->name()) == 0))
		{
#ifdef DEBUG
			printf("duplicate entry for %s between "
			       "platforms %s and %s\n", chain->name(),
			       chain->plat(), file->plat());
#endif
			break;
		}
		chain = chain->next();
	}
//
// If we didn't find a match, keep looking
//
	if (! chain)
		return (FALSE);
//
// Otherwise add this file to the duplicate chain
//
	IndexFile *next = chain->same();
	chain->dup (*file);
	if (next)
		file->dup (*next);
	else
		file->dup (*chain);
//
// The search ends here
//
	return (TRUE);
}






struct PlatInfo *
PlatformIndex::findPlat (const char *name) const
//
// Try to find a platform.
//
{
	void *vp;

	if (! table.get (name, vp))
		return (0);
	return ((struct PlatInfo *) vp);
}



PlatformIndex::~PlatformIndex ()
//
// Make this thing go away.
//
{
	table.traverse (ZapPlat, 0);
	dirs.traverse (ZapDirList, 0);
}



int
ZapPlat (const char *name, int type, const SValue *v, long junk)
//
// Get rid of this platform by deleting all of the dynamic structures.
//
{
	PlatInfo *pi = (PlatInfo *) v->us_v_ptr;
//
// Get rid of the files.
//
	while (pi->pi_files)
	{
		IndexFile *zap = pi->pi_files;
		pi->pi_files = (IndexFile *) pi->pi_files->next ();
		delete zap;
	}
//
// Now just the platinfo and we're done.
//
	delete (pi);
	return (TRUE);
}




static int
ZapDirList (const char *name, int type, const SValue *v, long junk)
//
// Get rid of a DirList
//
{
	DirList *dl = (DirList *) v->us_v_ptr;
	delete dl;
	return (TRUE);
}





int
PlatformIndex::save (const char *fname) const
//
// Save this index to a file.  Returns TRUE if it worked.
//
{
	FILE *fp;
//
// Make sure the file is open.
//
	if ((fp = fopen (fname, "w")) == NULL)
	{
		cerr << "Unable to open '" << fname << "'.\n";
		return (0);
	}
//
// Now the real work gets passed off.
//
	table.traverse (WritePlat, (long) fp);
	fclose (fp);
	return (1);
}





static int
WritePlat (const char *name, int type, const SValue *v, long lfp)
//
// Output a platform to this file.
//
{
	FILE *fp = (FILE *) lfp;
	PlatInfo *pi = (PlatInfo *) v->us_v_ptr;
	const IndexFile *indf = pi->pi_files;

	while (indf)
	{
		fprintf (fp, "%s %s %ld %ld %d %d\n", name, indf->name(),
			indf->begin().zt_Sec, indf->end().zt_Sec, indf->size(),
			indf->filenum ());
		indf = indf->next ();
	}
	return (TRUE);
}





PlatformIndex::PlatformIndex (const char *fname)
//
// Construct an index from a file.
//
{
	char plat[30], file[200];
	ZebTime begin, end;
	int size, filenum, nf = 0;
	IndexFile *indf;
	FILE *fp;
//
// Make our symbol table.
//
	table.init (fname);
	sprintf (file, "%s-dirs", fname);
	dirs.init (file);
//
// Try to open the file.
//
	if ((fp = fopen (fname, "r")) == NULL)
	{
		cerr << "Unable to open file '" << fname << "'.\n";
		return;
	}
//
// Now we plow through the entries.
//
	begin.zt_MicroSec = end.zt_MicroSec = 0;
	while (fscanf (fp, "%s %s %ld %ld %d %d\n", plat, file,
		&begin.zt_Sec, &end.zt_Sec, &size, &filenum) > 0)
	{
		indf = new IndexFile (plat, file, size, filenum, &begin, &end);
		add (plat, *indf);
		nf++;
	}
	cout << nf << " files loaded from index\n";
	fclose (fp);
}





int
PlatformIndex::coverage (const char *plat, ZebTime &begin, ZebTime &end) const
//
// Return the time period covered for this platform.
//
{
	PlatInfo *pi = findPlat (plat);

	if (! pi)
		return (0);
	begin = pi->pi_begin;
	end = pi->pi_end;
	return (1);
}




int &
PlatformIndex::isMarked (const char *plat)
//
// Return the mark variable.
//
{
	PlatInfo *pi = findPlat (plat);

	if (! pi)
	{
		static int MajorKludge = 0;
		return MajorKludge;
	}
	return pi->pi_marked;
}




IndexFile *
PlatformIndex::files (const char *plat) const
//
// Return the file chain for this platform.
//
{
	PlatInfo *pi = findPlat (plat);

	return (pi ? pi->pi_files : (IndexFile *) NULL );
}
