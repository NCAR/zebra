//
// dsFile methods
//
/*		Copyright (C) 1998 by UCAR
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
# include <sys/stat.h>
# include "dsFile.h"

static char *rcsid = "$Id: dsFile.cc,v 1.3 2002-12-18 00:24:13 granger Exp $";


dsFile::dsFile (const DataFile *df)
//
// Set up this dsFile structure.
//
{
    srcid = df->df_srcid;
    pid = df->df_pid;
    begintime = df->df_core.dfc_begin;
}



dsFile::dsFile (const dsFile& old)
//
// The initialization case.
//
{
    srcid = old.srcid;
    pid = old.pid;
    begintime = old.begintime;
}


const char*
dsFile::name (void) const
//
// Return the name of this file, good until the next call to this function
//
{
    static char fname[CFG_FILEPATH_LEN];
    const DataFile *df = GetFile();

    if (! df)
	strcpy (fname, "(None)");
    else
	strcpy (fname, df->df_fullname);

    return fname;
}



unsigned int
dsFile::size (void) const
//
// Return the file's size
//
{
    struct stat buf;

    if (stat (name(), &buf) < 0)
    {
	std::cerr << "Unable to stat file " << name() << "\n";
	return 0;
    }
    else
	return buf.st_size;
}


const ZebraTime*
dsFile::end (void) const
//
// Return our end time, good until the next call here.
//
{
    static ZebraTime endtime;
    const DataFile *df = GetFile();
    endtime = df->df_core.dfc_end;
    return &endtime;
}

    


const DataFile*
dsFile::GetFile (void) const
//
// Get the full data file from the data store
//
{
    static DataFile curfile;
//
// If the last file we grabbed is the right one, just return that
//
    if (curfile.df_srcid == srcid && curfile.df_pid == pid && 
	TC_Eq (curfile.df_core.dfc_begin, begintime))
	return &curfile;
//
// Otherwise, talk to the data store
//
    const DataFile *df = ds_FindDFBefore (srcid, pid, &begintime);
    if (! df || ! TC_Eq (df->df_core.dfc_begin, begintime))
	return 0;

    curfile = *df;
    return &curfile;
}

