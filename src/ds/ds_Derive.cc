
//
// Data store interface to field derivation.
//
//		Copyright (C) 1997 by UCAR
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

# include <errno.h>
# include <message.h>
# include "DerivTable.h"
# include "DataStore.h"
# include "Platforms.h"

# include <string>

MAKE_RCSID ("$Id")

//
// Default and project derivation tables
//
DerivTable	DefaultDerivs;
DerivTable	ProjectDerivs;

//
// A list of per-platform derivation tables.
//
class _PlatDerList
{
public:
    _PlatDerList( void );
    DerivTable* Table( PlatformId pid );
private:
    DerivTable **derivs;
    int len;
} PlatDerList;


//
// prototypes for local functions
//
static void LoadDerivs( DerivTable *dtable, const char *fname, int quiet );

//
// Symbols from the bison parser: FDParse.bison/FDParse.tab.cc
//
extern DerivTable *FDTable;
extern int FDparse( void );
extern void FD_scan_string ( const char *yy_str );

//
// C interface for the stuff we show externally
//
extern "C"
{

void
ds_DerivInit( void )
{
    char fname[128];
//
// Parse default derivation file <zebra_lib_dir>/Derivations, building
// derivation table DefaultDerivs.  Same for the optional project-specific 
// default derivations, building ProjectDerivs.
//
    sprintf( fname, "%s/Derivations", GetLibDir() );
    LoadDerivs( &DefaultDerivs, fname, 0 );

    sprintf( fname, "%s/derivs/defaults", GetProjDir() );
    LoadDerivs( &ProjectDerivs, fname, 1 );
}




int
ds_IsDerivable( PlatformId pid, FieldId wantid, FieldId *raw_ids, int nraw )
//
// Return true iff wantid can be obtained or derived from the 
// given list of raw fields.
//
{
    DerivMethod der;
    if ((der = ds_GetDerivation( pid, wantid, raw_ids, nraw )))
    {
	ds_DestroyDeriv ( der );
	return 1;
    }
    else
	return 0;
}



DerivMethod
ds_GetDerivation( PlatformId pid, FieldId wantid, FieldId raw_ids[], int nraw )
//
// Return a pointer to a new DerivMethod iff wantid can be derived from the
// given list of raw fields, else return 0.  The caller is responsible for
// calling ds_DestroyDeriv() to release the DerivMethod.  Return null if
// the wanted field cannot be derived.
//
// We use DerivMethod here as an opaque type that we can pass back to C
// modules.  It's just a cast pointer to a DerivNode.
//
{
    RawFldDNode	want( *(Field*)wantid );
    Field *raw_flds = new Field[nraw];
    DerivNode *deriv;
    const Platform *p = dt_FindPlatform (pid);
    PlatformId parent_id = pi_ParentId (p);
    const DerivTable *dtables[4];
    int ntables;

    for (int f = 0; f < nraw; f++)
	raw_flds[f] = *(Field*)(raw_ids[f]);
//
// Derivation table hierarchy in the order to be searched:
//
//	o platform derivation table
//	o platform parent derivation table (if platform is a subplatform)
//	o project derivation table
//	o default derivation table
//
    ntables = 0;
    
    dtables[ntables++] = PlatDerList.Table( pid );
    if (parent_id != BadPlatform)
	dtables[ntables++] = PlatDerList.Table( parent_id );
    dtables[ntables++] = &ProjectDerivs;
    dtables[ntables++] = &DefaultDerivs;
//
// Look for a viable derivation
//
    deriv = want.MetaEval( dtables, ntables, raw_flds, nraw );
    delete[] raw_flds;

    return ((DerivMethod)deriv);
}




int
ds_DerivIsAlias( DerivMethod der )
//
// Return true iff 'der' is an alias, i.e., if the resulting DerivNode
// is a RawFldDNode.
//
{
    DerivNode *deriv = (DerivNode*) der;
    return (! strcmp( deriv->ClassId(), "RawFldDNode" ));	// kluge...
}




void
ds_DoDerivation( DerivMethod der, FieldId fids[], int nflds, int ndata, 
		double* dptrs[], double results[], double badval )
//
// Perform the derivation using the given fields and associated data arrays
// of length ndata.  Derived values go into "results", and badval is the
// bad value flag used in the input arrays and to be used for the results as
// well.
//
{
//
// Cast the opaque DerivMethod into its real form (a DerivNode) and call
// its Eval method
//
    DerivNode *deriv = (DerivNode*) der;
    Field *raw_flds = new Field[nflds];

    for (int f = 0; f < nflds; f++)
	raw_flds[f] = *(Field*)(fids[f]);

    deriv->Eval( raw_flds, nflds, ndata, (const double**) dptrs, results, 
		 badval );

    delete[] raw_flds;
}



FieldId*
ds_DerivNeededFields( DerivMethod der, int *nneed )
//
// Return the list of fields required for the given derivation, and the
// number of those fields.  Each required field will be included in the list
// exactly once.  It is the responsibility of the caller to free the
// returned list.  An NULL pointer will be returned if the list is empty.
//
{
    FieldId *need = 0;
//
// Quick out for a void derivation
//
    if (! der)
	return( (FieldId*) 0 );
//
// We're really just a C wrapper around DerivNode::FieldList() here...
//
    DerivNode	*dnode = (DerivNode*) der;
    Field *flds = dnode->FieldList( nneed );
    if (! *nneed)
	return ( (FieldId*) 0 );

    need = (FieldId*) malloc ( *nneed * sizeof (FieldId) );
    for (int f = 0; f < *nneed; f++)
	need[f] = F_FieldId( flds[f] );
    
    delete[] flds;

    return (need);
}

    


void
ds_DestroyDeriv( DerivMethod der )
//
// Get rid of a DerivMethod returned by ds_GetDerivation()
//
{
    if (der)
	delete ((DerivNode*) der);
}

} // end of extern "C"


//
// Local functions can keep a C++ interface
//

static void
LoadDerivs( DerivTable *dtable, const char *fname, int quiet )
//
// Parse the derivations from file 'fname', putting them into the given
// 'dtable' Complain about a non-existent file unless 'quiet' is true.
//
{
    extern FILE	*FDin;

    if ((FDin = fopen (fname, "r")) != 0)
    {
	FDTable = dtable;
	FDparse();
	fclose (FDin);
	FDin = 0;
	msg_ELog (EF_DEBUG, "Loaded derivs file: %s", fname);
    }
    else if (! quiet)
    {
	msg_ELog (EF_PROBLEM, "Error %d opening derivations file '%s'",
		  errno, fname);
    }
    else
	msg_ELog (EF_DEBUG, "No derivs file: %s", fname);
}


//
// Our per-platform derivation list class methods
//

_PlatDerList::_PlatDerList( void )
{
    derivs = 0;
    len = 0;
}


DerivTable*
_PlatDerList::Table( PlatformId pid )
{
//
// See if we need more entries
//
    if (pid >= len)
    {
	DerivTable **oldderivs = derivs;
	int oldlen = len;
	int i;

	len = pid + 20;	// add a few extra while we're at it
	derivs = new DerivTable*[len];

	for (i = 0; i < oldlen; i++)
	    derivs[i] = oldderivs[i];

	for (; i < len; i++)
	    derivs[i] = (DerivTable*) 0;

	delete[] oldderivs; // delete[] since we allocated with new[]
    }
//
// Make an entry for this pid if it doesn't exist already
//
    if (! derivs[pid])
    {
	char fname[128];
	const char *pdir;
	const Platform *pi = dt_FindPlatform( pid );
	const PlatformClass* pc = pi_Class (pi);
	
	derivs[pid] = new DerivTable;
    //
    // Try to load platform-specific derivations from
    // <proj_dir>/derivs/<plat_dir> if the file exists.  Otherwise we just
    // have an empty DerivTable.  If the platform's suggested directory
    // is a relative path, use that, otherwise use the platform's name.
    //
	pdir = pi_SuggestedDir(pi);
	if (pdir[0] == '/')
	    pdir = ds_PlatformName( pid );

	sprintf( fname, "%s/derivs/%s", GetProjDir(), pdir );
	LoadDerivs( derivs[pid], fname, 1 );	

	// 
	// Now add the derivations from the platform class, if any.  I
	// think this means derivations in the per-platform file will take
	// precedence, which I think is what we want.
	//
	if (pc->dpc_derivations)
	{
	    msg_ELog (EF_DEBUG, "parsing class derivations for %s:",
		      pc_Name (pc));
	    msg_ELog (EF_DEVELOP, "%s", pc->dpc_derivations);
	    FDTable = derivs[pid];
	    FD_scan_string (pc->dpc_derivations);
	    FDparse();
	}
    }
    return derivs[pid];
}
