//
// Manage info on data directories.
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

# if defined(SVR4) || defined(sgi)
# define USE_STATVFS
# endif

# include <sys/types.h>
# ifdef USE_STATVFS
# include <sys/statvfs.h>
# else
# include <sys/vfs.h>
# endif
# include <sys/stat.h>
# include <stream.h>

# include "container.h"
# include "DataDir.h"

extern "C" {
//	int stat (const char *, struct stat *);
# ifndef USE_STATVFS
	int statfs (const char *, struct statfs *);
# endif
# ifndef __GNUC__
	int strlen (char *);
	char *strcpy (char *, char *);
# endif
};

static char *rcsid = "$Id: DataDir.cc,v 1.8 1996-01-23 22:36:27 corbet Exp $";

//
// The data directory class.
//

DataDir::DataDir (const char *dir)
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




int
DataDir::FreeSpace ()
//
// How much space is free here?
//
{
# ifdef USE_STATVFS
	struct statvfs stbuf;
	if (statvfs (directory, &stbuf) < 0)
		return (-1);
	return (stbuf.f_frsize*stbuf.f_bavail);
# else
	struct statfs stbuf;
	if (statfs (directory, &stbuf) < 0)
		return (-1);
	return (stbuf.f_bsize*stbuf.f_bavail);
# endif
}



//
// Data file routines.
//

dsFile::dsFile (const char *name, int findex)
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



dsFile::~dsFile ()
//
// Destroy one of these babies.
//
{
	delete [] this->fname;
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

