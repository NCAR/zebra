//
// The datadir class.
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
