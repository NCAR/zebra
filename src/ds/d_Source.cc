//
// Daemon Source-related functions.  These are mostly C bindings to the
// methods of class Source.
//
//
//		Copyright (C) 1998 by UCAR
//	University Corporation for Atmospheric Research
//		   All rights reserved
//
// No part of this work covered by the copyrights herein may be reproduced
// or used in any form or by any means -- graphic, electronic, or mechanical,
// including photocopying, recording, taping, or information storage and
// retrieval systems -- without permission of the copyright owner.
// 
// This software and any accompanying written materials are provided "as is"
// without warranty of any kind.  UCAR expressly disclaims all warranties of
// any kind, either express or implied, including but not limited to the
// implied warranties of merchantibility and fitness for a particular purpose.
// UCAR does not indemnify any infringement of copyright, patent, or trademark
// through use or modification of this software.  UCAR does not provide 
// maintenance or updates for its software.
//

# include "d_Source.h"
# include "Source.hh"

RCSID ("$Id")

//
// All of our functions are defined with C binding
//
extern "C"
{

Source*
src_Open (const char *srcname, const char *rootdir, const char *fname)
{
    try
    {
	Source* s = new Source( srcname, rootdir, fname );
	return s;
    }
    catch (Source::Error)
    {
	return 0;
    }
}



void 
src_Close (Source *src)
{
    delete src;
}



zbool 
src_First (Source *src, const Platform *p, DataFileCore *dfc)
{
    return src->First( p, dfc );
}



zbool 
src_Next (Source *src, const Platform *p, DataFileCore *dfc)
{
    return src->Next( p, dfc );
}



zbool 
src_Prev (Source *src, const Platform *p, DataFileCore *dfc)
{
    return src->Prev( p, dfc );
}



zbool 
src_LastTime (Source *src, const Platform *p, ZebraTime *last)
//
// Return the latest time we have for the given platform.  In reality,
// we return the end time of the last file, which isn't guaranteed to be
// the latest time, but it should be OK.
//
{
    DataFileCore dfc;

    if (! src->Last( p, &dfc ))
	return 0;

    *last = dfc.dfc_end;
    return 1;
}



zbool 
src_FindBefore (Source *src, const Platform *p, const ZebraTime *t, 
		DataFileCore *dfc)
{
    return src->FindBefore( p, *t, dfc );
}



zbool 
src_FindAfter (Source *src, const Platform *p, const ZebraTime *t, 
	       DataFileCore *dfc)
{
    return src->FindAfter( p, *t, dfc );
}



zbool 
src_FindExact (Source *src, const Platform *p, const ZebraTime *t, 
	       DataFileCore *dfc)
{
    return src->FindExact( p, *t, dfc );
}



void 
src_UpdateFile (Source *src, const Platform *p, const DataFileCore *dfc)
{
    src->UpdateFile( p, *dfc );
}



void 
src_RemoveFile (Source *src, const Platform *p, const DataFileCore *dfc)
{
    src->RemoveFile( p, *dfc );
}


const char*
src_Name (const Source *src)
{
    return (src->Name().c_str());
}


const char*
src_RootDir (const Source *src)
{
    return (src->RootDir().c_str());
}


const char*
src_DataDir (Source *src, const Platform *p)
{
    return (src->DataDir( p ).c_str());
}


int
src_ConfirmDataDir (Source *src, const Platform *p)
{
    return src->ConfirmDataDir( p );
}


void
src_SetPlatDir (Source *src, const char *platname, const char *dir)
{
    Source::string splatname( platname ), sdir( dir );
    src->SetPlatDir( splatname, sdir );
}


int
src_NFiles (Source *src, const Platform *p)
{
    return (src->NFiles( p ));
}



void
src_SetReadOnly (Source *src, zbool readonly)
{
    src->SetReadOnly (readonly);
}



zbool
src_IsReadOnly (const Source *src)
{
    return src->IsReadOnly();
}



void
src_SetDirConst (Source *src, zbool dconst)
{
    src->SetDirConst (dconst);
}



zbool
src_IsDirConst (const Source *src)
{
    return src->IsDirConst();
}



void
src_SetFileConst (Source *src, zbool fconst)
{
    src->SetFileConst (fconst);
}



zbool
src_IsFileConst (const Source *src)
{
    return src->IsFileConst();
}


void
src_SetRememberAll (Source *src, zbool rall)
{
    src->SetRememberAll (rall);
}



zbool
src_RemembersAll (const Source *src)
{
    return src->RemembersAll();
}


void
src_SetForceDirs (Source *src, zbool force)
{
    src->SetForceDirs (force);
}



zbool
src_DirsAreForced (const Source *src)
{
    return src->DirsAreForced();
}


} // end of extern "C"
